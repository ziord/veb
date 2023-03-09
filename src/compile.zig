const std = @import("std");
const value = @import("value.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const util = @import("util.zig");
const NovaAllocator = @import("allocator.zig");
const OpCode = @import("opcode.zig").OpCode;
const VM = @import("vm.zig").VM;

const Code = value.Code;
const Node = ast.AstNode;

const VRegister = struct {
  regs: [value.MAX_REGISTERS]Reg,
  reg: ?u8,

  const DummyReg = 0xfff;

  const Reg = struct {
    val: u8, 
    free: bool,

    pub inline fn setFree(self: *@This(), free: bool) void {
      self.free = free;
    }
  };

  const Self = @This();

  pub fn init() Self {
    var regs: [value.MAX_REGISTERS]Reg = undefined;
    for (0..value.MAX_REGISTERS) |i| {
      regs[i] = .{.val = @intCast(u8, i), .free = true};
    }
    return Self {.regs = regs, .reg = null};
  }

  pub fn getReg(self: *Self) error{RegistersExhausted}!u32 {
    for (&self.regs) |*reg| {
      if (reg.free) {
        reg.setFree(false);
        return @as(u32, reg.val);
      }
    }
    return error.RegistersExhausted;
  }

  pub fn releaseReg(self: *Self, reg: u32) void {
    if (reg == DummyReg) return;
    for (&self.regs) |*_reg| {
      if (_reg.val == reg) {
        std.debug.assert(!_reg.free);
        _reg.setFree(true);
        return;
      }
    }
  }
};

const GlobalVar = struct {
  name: []const u8,
  mempos: u32,
  index: u32, // index into compiler's `globals`
  initialized: bool = false,
  patched: bool = false,
};

/// like a GlobalVar but with more efficient read/write access
const GSymVar = struct {
  name: []const u8,
  initialized: bool = false,
  patched: bool = false,
};

const LocalVar = struct {
  scope: i32,
  name: []const u8,
  reg: u32,
  index: u32,
  initialized: bool = false,

  pub fn init(scope: i32, name: []const u8, reg: u32, index: u32) @This() {
    return @This() {.scope = scope, .name = name, .reg = reg, .index = index};
  }
};

pub const Compiler = struct {
  code: *Code,
  node: *Node,
  filename: []const u8,
  gsyms: [VM.MAX_GSYM_ITEMS]GSymVar,
  locals: [VM.MAX_LOCAL_ITEMS]LocalVar,
  globals: std.ArrayList(GlobalVar),
  vreg: VRegister,
  allocator: *NovaAllocator,
  scope: i32 = GLOBAL_SCOPE, // defaults to global scope
  locals_count: u32 = 0,
  gsyms_count: u32 = 0,
  vm: *VM,
  
  const Self = @This();

  const GLOBAL_SCOPE = 0;
  const GlobalVarInfo = struct {pos: u32, isGSym: bool, gvar: ?GlobalVar};

  pub fn init(node: *Node, filename: []const u8, vm: *VM, code: *Code, allocator: *NovaAllocator) Self {
    var self = Self {
      .code = code,
      .node = node, 
      .filename = filename,
      .allocator = allocator,
      .gsyms = undefined,
      .locals = undefined,
      .globals = std.ArrayList(GlobalVar).init(allocator.getArenaAllocator()),
      .vreg = VRegister.init(),
      .vm = vm,
    };
    return self;
  }

  fn compileError(self: *Self, comptime fmt: []const u8, msg: anytype) noreturn {
    _ = self;
    std.debug.print("CompileError: ", .{});
    std.debug.print(fmt ++ "\n", msg);
    std.os.exit(1);
  }

  fn lastLine(self: *Self) u32 {
    return self.code.lines.items[self.code.lines.items.len - 1];
  }

  fn getReg(self: *Self) u32 {
    return self.vreg.getReg() catch {
      self.compileError("registers exhausted", .{});
    };
  }

  /// return true if within RK limit else false
  fn withinRKLimit(self: *Self, num_operands: usize) bool {
    // check that we don't exceed the 9 bits of rk (+ num_operands for number of operands to be added)
    return ((self.code.values.items.len + value.MAX_REGISTERS + num_operands) <= value.Code._9bits);
  }

  /// return true if within BX limit else false
  fn withinBXLimit(self: *Self) bool {
    // check that we don't exceed the 18 bits of bx (+ 1 for the new addition)
    return ((self.code.values.items.len + 1) <= value.Code._18bits);
  }

  fn addLocal(self: *Self, node: *ast.VarNode) u32 {
    // add a local and return its register.
    if (self.locals_count >= self.locals.len) {
      self.compileError("Too many locals", .{});
    }
    var reg = self.getReg();
    self.locals[self.locals_count] = LocalVar.init(self.scope, node.token.value, reg, self.locals_count);
    self.locals_count += 1;
    return reg;
  }

  /// add gsym and return its index/pos
  fn addGSymVar(self: *Self, node: *ast.VarNode) struct {pos: u32, isGSym: bool} {
    // Deduplicate globals. If a spot is already allocated for a global 'a', 
    // a redefinition of 'a' should not allocate a new spot.
    if (self.findGlobal(node)) |info| {
      return .{.pos = info.pos, .isGSym = info.isGSym};
    }
    if (self.gsyms_count >= self.gsyms.len) {
      std.log.debug("gsyms exceeded..using globals list\n", .{});
      return .{.pos = self.addGlobalVar(node), .isGSym = false};
    }
    var idx = self.gsyms_count;
    self.gsyms_count += 1;
    self.gsyms[idx] = GSymVar {.name = node.token.value};
    return .{.pos = idx, .isGSym = true};
  }

  fn addGlobalVar(self: *Self, node: *ast.VarNode) u32 {
    var gvar = GlobalVar {
      .name = node.token.value, 
      .mempos = self.storeVar(node),
      .index = @intCast(u32, self.globals.items.len),
    };
    util.append(GlobalVar, &self.globals, gvar);
    return gvar.mempos;
  }

  fn findLocalVar(self: *Self, node: *ast.VarNode) ?LocalVar {
    if (self.locals_count == 0) return null;
    var lvar: LocalVar = undefined;
    var i = self.locals_count;
    while (i > 0): (i -= 1) {
      lvar = self.locals[i - 1];
      if (self.scope < lvar.scope) break;
      if (std.mem.eql(u8, lvar.name, node.token.value)) {
        return lvar;
      }
    }
    return null;
  }

  fn findGSymVar(self: *Self, node: *ast.VarNode) ?u32 {
    if (self.gsyms_count == 0) return null;
    var gsym: *GSymVar = undefined;
    var i: u32 = self.gsyms_count;
    while (i > 0): (i -= 1) {
      var idx = i - 1;
      gsym = &self.gsyms[idx];
      if (std.mem.eql(u8, gsym.name, node.token.value)) {
        return idx;
      }
    }
    return null;
  }

  fn findGlobalVar(self: *Self, node: *ast.VarNode) ?GlobalVar {
    if (self.globals.items.len == 0) return null;
    var gvar: GlobalVar = undefined;
    var i: usize = self.globals.items.len;
    while (i > 0): (i -= 1) {
      gvar = self.globals.items[i - 1];
      if (std.mem.eql(u8, gvar.name, node.token.value)) {
        return gvar;
      }
    }
    return null;
  }

  fn findGlobal(self: *Self, node: *ast.VarNode) ?GlobalVarInfo {
    if (self.findGSymVar(node)) |pos| {
      return .{.pos = pos, .isGSym = true, .gvar = null};
    } else if (self.findGlobalVar(node)) |gvar| {
      return .{.pos = gvar.mempos, .isGSym = false, .gvar = gvar};
    }
    return null;
  }

  fn popLocalVars(self: *Self) void {
    if (self.locals_count == 0) return;
    var lvar: LocalVar = undefined;
    var i: usize = self.locals_count;
    while (i > 0): (i -= 1) {
      lvar = self.locals[i - 1];
      if (lvar.scope > self.scope) {
        self.vreg.releaseReg(lvar.reg);
        self.locals_count -= 1;
      }
    }
  }

  /// preallocate all globals in `gsyms` or `globals`
  fn preallocateGlobals(self: *Self, toplevels: *ast.AstNodeList) void {
    // TODO: update
    if (self.scope > GLOBAL_SCOPE) return;
    for (toplevels.items) |decl| {
      switch (decl.*) {
        .AstVarDecl => |vd| {
          _ = self.addGSymVar(vd.ident);
        },
        else => {},
      }
    }
  }

  /// patch a preallocated global entry
  fn patchGlobal(self: *Self, node: *ast.VarNode) GlobalVarInfo {
    if (self.findGlobal(node)) |info| {
      if (info.isGSym) {
        // GSyms use `pos` as a direct index into the `gsyms` array
        // so we can reach this GSym directly
        self.gsyms[info.pos].patched = true;
      } else {
        self.globals.items[info.gvar.?.index].patched = true;
      }
      return info;
    } else {
      self.compileError("cannot patch variable '{s}'", .{node.token.value});
    }
  }

  fn validateNoUnpatchedGlobals(self: *Self) void {
    const fmt = "Use of unpached global '{s}'";
    for (0..self.gsyms_count) |i| {
      var gsym = &self.gsyms[i];
      if (!gsym.patched) {
        self.compileError(fmt, .{gsym.name});
      }
    }
    for (self.globals.items) |glob| {
      if (!glob.patched) {
        self.compileError(fmt, .{glob.name});
      }
    }
  }

  fn validateLocalVarUse(self: *Self, slot: u32, node: *ast.VarNode) void {
    if (!self.locals[slot].initialized) {
      self.compileError("cannot use variable '{s}' in its own initializer", .{node.token.value});
    }
  }

  fn validateGlobalVarUse(self: *Self, info: GlobalVarInfo, node: *ast.VarNode) void {
    if (info.isGSym) {
      var gsym = self.gsyms[info.pos];
      if (gsym.patched and !gsym.initialized) {
        self.compileError("cannot use variable '{s}' in its own initializer", .{node.token.value});
      }
    } else {
      var gvar = info.gvar.?;
      if (gvar.patched and !gvar.initialized) {
        self.compileError("cannot use variable '{s}' in its own initializer", .{node.token.value});
      }
    }
  }

  fn cConst(self: *Self, reg: u32, val: value.Value, line: usize) void {
    // load rx, memidx
    const memidx = self.code.writeValue(val, self.vm);
    self.code.write2ArgsInst(.Load, reg, memidx, @intCast(u32, line), self.vm);
  }

  fn storeVar(self: *Self, node: *ast.VarNode) u32 {
    var val = value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, false));
    return self.code.writeValue(val, self.vm);
  }

  fn cNumber(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    self.cConst(dst, value.numberVal(node.value), node.line);
    return dst;
  }

  fn cBool(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    self.cConst(dst, value.boolVal(node.token.is(.TkTrue)), node.line);
    return dst;
  }

  fn cString(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    self.cConst(
      dst,
      value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, node.token.is_alloc)),
      node.line
    );
    return dst;
  }

  fn cUnary(self: *Self, node: *ast.UnaryNode, reg: u32) u32 {
    const inst_op = node.op.toInstOp();
    const isConst = node.expr.isConst();
    const rx = reg;
    // check that we don't exceed the 18 bits of rk/bx (+ 1 for expr)
    if (isConst and self.withinBXLimit()) {
      // for now, only numbers and booleans are recognized as consts
      var rk: u32 = undefined;
      if (node.expr.isNum()) {
        rk = self.code.storeConst(value.numberVal(node.expr.AstNumber.value), self.vm);
      } else {
        rk = self.code.storeConst(value.boolVal(node.expr.AstBool.token.is(.TkTrue)), self.vm);
      }
      self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line), self.vm);
      return rx;
    }
    const rk = self.c(node.expr, reg);
    self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line), self.vm);
    return rx;
  }

  inline fn cCmp(self: *Self, node: *ast.BinaryNode) void {
    // <, >, <=, >=, ==, !=
    if (!node.op.isCmpOp()) return;
    self.code.writeNoArgInst(@intToEnum(OpCode, @enumToInt(node.op)), node.line, self.vm);
  }

  inline fn cLgc(self: *Self, node: *ast.BinaryNode, reg: u32) u32 {
    // and, or
    var rx = self.c(node.left, reg);
    const end_jmp = self.code.write2ArgsJmp(node.op.toInstOp(), rx, self.lastLine(), self.vm);
    rx = self.c(node.right, reg);
    self.code.patch2ArgsJmp(end_jmp);
    return rx;
  }

  fn cBinary(self: *Self, node: *ast.BinaryNode, dst: u32) u32 {
    // handle and | or
    if (node.op.isLgcOp()) return self.cLgc(node, dst);
    // handle other binary ops
    const lhsIsNum = node.left.isNum();
    const rhsIsNum = node.right.isNum();
    const inst_op = node.op.toInstOp();
    // check that we don't exceed the 9 bits of rk (+ 2 for lhs and rhs)
    if (lhsIsNum and rhsIsNum) {
      if (self.withinRKLimit(2)) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNumber.value), self.vm);
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNumber.value), self.vm);
        self.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(u32, node.line), self.vm);
        self.cCmp(node);
        return dst;
      }
    }
    // fallthrough
    var rk1: u32 = undefined;
    var rk2: u32 = undefined;
    if (lhsIsNum and self.withinRKLimit(1)) { // 2 * (3 * (4 * 5))
      rk1 = self.code.storeConst(value.numberVal(node.left.AstNumber.value), self.vm);
    } else {
      rk1 = self.c(node.left, dst);
    }
    if (rhsIsNum and self.withinRKLimit(1)) {
      rk2 = self.code.storeConst(value.numberVal(node.right.AstNumber.value), self.vm);
    } else {
      const dst2 = self.getReg();
      rk2 = self.c(node.right, dst2);
      // we own this register, so we can free
      self.vreg.releaseReg(dst2);
    }
    self.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(u32, node.line), self.vm);
    self.cCmp(node);
    return dst;
  }

  fn cList(self: *Self, node: *ast.ListNode, dst: u32) u32 {
    const size = @intCast(u32, node.elems.items.len);
    self.code.write2ArgsInst(.Nlst, dst, size, node.line, self.vm);
    var idx: u32 = undefined;
    for (node.elems.items, 0..) |elem, i| {
      var reg = self.getReg();
      var rk_val = self.c(elem, reg);
      var val = value.numberVal(@intToFloat(f64, i));
      if (self.withinRKLimit(1)) {
        idx = self.code.storeConst(val, self.vm);
      } else {
        idx = self.getReg();
        self.cConst(idx, val, node.line);
        self.vreg.releaseReg(idx);
      }
      self.code.write3ArgsInst(.Slst, dst, idx, rk_val, node.line, self.vm);
      self.vreg.releaseReg(reg);
    }
    return dst;
  }

  fn cMap(self: *Self, node: *ast.MapNode, dst: u32) u32 {
    const size = @intCast(u32, node.pairs.items.len);
    // TODO: specialize map with k-v types
    self.code.write2ArgsInst(.Nmap, dst, size, node.line, self.vm);
    for (node.pairs.items) |pair| {
      var reg1 = self.getReg();
      var reg2 = self.getReg();
      var rk_key = self.c(pair.key, reg1);
      var rk_val = self.c(pair.value, reg2);
      self.code.write3ArgsInst(.Smap, dst, rk_key, rk_val, node.line, self.vm);
      self.vreg.releaseReg(reg1);
      self.vreg.releaseReg(reg2);
    }
    return dst;
  }

  fn cExprStmt(self: *Self, node: *ast.ExprStmtNode, reg: u32) u32 {
    // sequence point
    const dst = self.getReg();
    _ = self.c(node.expr, dst);
    self.vreg.releaseReg(dst);
    return reg;
  }

  fn cVar(self: *Self, node: *ast.VarNode, dst: u32) u32 {
    if (self.findLocalVar(node)) |lvar| {
      self.validateLocalVarUse(lvar.index, node);
      return lvar.reg;
    } else if (self.findGlobal(node)) |info| {
      self.validateGlobalVarUse(info, node);
      const inst: OpCode = if(info.isGSym) .Ggsym else .Gglb;
      self.code.write2ArgsInst(inst, dst, info.pos, node.line, self.vm);
      return dst;
    } else {
      self.compileError("use of undefined variable '{s}'", .{node.token.value});
    }
  }

  fn cVarAssign(self: *Self, node: *ast.VarNode, expr: *Node, reg: u32) u32 {
    if (self.findLocalVar(node)) |lvar| {
      // use lvar's reg as dst for expr
      var ret = self.c(expr, lvar.reg);
      if (ret != lvar.reg) {
        // this implies the result of expr was stored in a different register.
        // Move it to lvar's register.
        self.code.write3ArgsInst(.Mov, lvar.reg, ret, 0, node.line, self.vm);
      }
      return lvar.reg;
    } else if (self.findGlobal(node)) |info| {
      // sgsym/sglb rx, bx -> GS[bx] = r(x) | G[K(bx)] = r(x)
      var rx = self.c(expr, reg);
      const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
      self.code.write2ArgsInst(inst, rx, info.pos, node.line, self.vm);
      return rx;
    } else {
      self.compileError("undefined variable '{s}'", .{node.token.value});
    }
  }

  fn cAssign(self: *Self, node: *ast.BinaryNode, reg: u32) u32 {
    return switch (node.left.*) {
      .AstVar => |*vr| {
        return self.cVarAssign(vr, node.right, reg);
      },
      // would be unreachable after type checking
      else => self.compileError("invalid assignment target", .{}),
    };
  }

  fn cVarDecl(self: *Self, node: *ast.VarDeclNode, reg: u32) u32 {
    // let var = expr
    // local
    if (self.scope > GLOBAL_SCOPE) {
      var dst = self.addLocal(node.ident);
      var lvar_pos = self.locals_count - 1;
      _ = self.c(node.value, dst);
      // initialize the local after its rhs has been compiled successfully
      self.locals[lvar_pos].initialized = true;
    }
    // global
    else {
      var info = self.patchGlobal(node.ident);
      // first, deinitialize the global just in case it was initialized by
      // some other code (since globals are deduplicated/shared)
      if (info.isGSym) {
        self.gsyms[info.pos].initialized = false;
      } else {
        self.globals.items[info.gvar.?.index].initialized = false;
      }
      var dst = self.getReg();
      var rx = self.c(node.value, dst);
      // now, initialize this global after its rhs has been compiled successfully
      const inst: OpCode = blk: {
        if(info.isGSym) {
          self.gsyms[info.pos].initialized = true;
          break :blk .Sgsym;
        } else {
          self.globals.items[info.gvar.?.index].initialized = true;
          break :blk .Sglb;
        }
      };
      // sgsym rx, bx -> GS[bx] = r(x)
      // sglb rx, bx -> G[K(bx)] = r(x)
      // info.pos is GSym pos or index into valuepool storing GlobalVar name
      self.code.write2ArgsInst(inst, rx, info.pos, node.line, self.vm);
      self.vreg.releaseReg(dst);
    }
    return reg;
  }

  fn cBlock(self: *Self, node: *ast.BlockNode, reg: u32) u32 {
    self.scope += 1;
    for (node.nodes.items) |nd| {
      _ = self.c(nd, reg);
    }
    self.scope -= 1;
    self.popLocalVars();
    return reg;
  }

  fn cProgram(self: *Self, node: *ast.ProgramNode, reg: u32) u32 {
    for (node.decls.items) |nd| {
      _ = self.c(nd, reg);
    }
    return reg;
  }

  fn c(self: *Self, node: *Node, reg: u32) u32 {
    return switch (node.*) {
      .AstNumber => |*nd| self.cNumber(nd, reg),
      .AstString => |*nd| self.cString(nd, reg),
      .AstBool => |*nd| self.cBool(nd, reg),
      .AstUnary => |*nd| self.cUnary(nd, reg),
      .AstBinary => |*nd| self.cBinary(nd, reg),
      .AstList => |*nd| self.cList(nd, reg),
      .AstMap => |*nd| self.cMap(nd, reg),
      .AstExprStmt => |*nd| self.cExprStmt(nd, reg),
      .AstVar => |*nd| self.cVar(nd, reg),
      .AstVarDecl => |*nd| self.cVarDecl(nd, reg),
      .AstAssign => |*nd| self.cAssign(nd, reg),
      .AstBlock => |*nd| self.cBlock(nd, reg),
      .AstNType => reg, // TODO
      .AstAlias => reg, // TODO
      .AstCast => |*nd| self.c(nd.expr, reg), // TODO
      .AstProgram => |*nd| self.cProgram(nd, reg),
    };
  }

  pub fn compile(self: *Self) void {
    self.preallocateGlobals(&self.node.AstProgram.decls);
    util.assert(self.c(self.node, VRegister.DummyReg) == VRegister.DummyReg, "should be a dummy register");
    const last_line = self.code.lines.items[self.code.lines.items.len - 1];
    self.code.writeNoArgInst(.Ret, last_line, self.vm);
    self.validateNoUnpatchedGlobals();
    // release memory associated with the arena, since we're done with it at this point.
    self.allocator.deinitArena();
  }

};
