const std = @import("std");
const value = @import("value.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");
const CnAllocator = @import("allocator.zig");
const OpCode = @import("opcode.zig").OpCode;
const VM = @import("vm.zig").VM;

const Code = value.Code;
const Node = ast.AstNode;
const Inst = value.Inst;
const TypeKind = parse.TypeKind;

const VRegister = struct {
  regs: std.MultiArrayList(Reg),

  const DummyReg = 0xfff;

  const Reg = struct {
    val: u8,
    free: bool,

    pub inline fn setFree(self: *@This(), free: bool) void {
      self.free = free;
    }
  };

  const Self = @This();

  pub fn init(al: std.mem.Allocator) Self {
    var regs = std.MultiArrayList(Reg){};
    regs.ensureTotalCapacity(al, value.MAX_REGISTERS) catch {};
    for (0..value.MAX_REGISTERS) |i| {
      regs.append(al, .{.val = @intCast(u8, i), .free = true}) catch {};
    }
    return Self {.regs = regs};
  }

  pub fn getReg(self: *Self) error{RegistersExhausted}!u32 {
    var slice = self.regs.slice();
    var frees = slice.items(.free);
    for (frees, 0..) |free, i| {
      if (free) {
        frees[i] = false;
        return @as(u32, slice.items(.val)[i]);
      }
    }
    return error.RegistersExhausted;
  }

  pub fn releaseReg(self: *Self, reg: u32) void {
    if (reg == DummyReg) return;
    var slice = self.regs.slice();
    var vals = slice.items(.val);
    for (vals, 0..) |val, i| {
      if (val == reg) {
        var frees = slice.items(.free);
        std.debug.assert(!frees[i]);
        frees[i] = true;
        return;
      }
    }
  }
};

const GlobalVar = struct {
  name: *[]const u8,
  mempos: u32,
  index: u32, // index into compiler's `globals`
  initialized: bool = false,
  patched: bool = false,
};

/// like a GlobalVar but with more efficient read/write access
const GSymVar = struct {
  name: *[]const u8,
  initialized: bool = false,
  patched: bool = false,
};

const LocalVar = struct {
  scope: i32,
  reg: u32,
  name: *[]const u8,
  index: u32,
  initialized: bool = false,

  pub fn init(scope: i32, name: *[]const u8, reg: u32, index: u32) @This() {
    return @This() {.scope = scope, .name = name, .reg = reg, .index = index};
  }
};

pub const Compiler = struct {
  code: *Code,
  gsyms: std.MultiArrayList(GSymVar),
  locals: std.MultiArrayList(LocalVar),
  globals: std.MultiArrayList(GlobalVar),
  loop_ctrls: ds.ArrayList(*ast.ControlNode),
  vreg: VRegister,
  allocator: *CnAllocator,
  vm: *VM,
  scope: i32 = GLOBAL_SCOPE, // defaults to global scope
  rk_bx: RkBxPair = RkBxPair{},
  
  const Self = @This();

  const GLOBAL_SCOPE = 0;
  const MAX_LOCALS = VM.MAX_LOCAL_ITEMS;
  const MAX_GSYMS = VM.MAX_GSYM_ITEMS;
  const GlobalVarInfo = struct {
    pos: u32,
    isGSym: bool,
    gvar: ?GlobalVar
  };
  const RkBxPair = struct {
    optimizeRK: u32 = 0, // can optimize for rk
    optimizeBX: u32 = 0, // can optimize for bx
    in_jmp: u32 = 0,
  };

  pub fn init(filename: []const u8, src: []const u8, vm: *VM, code: *Code, allocator: *CnAllocator) Self {
    _ = src;
    _ = filename;
    var al = allocator.getArenaAllocator();
    var gsyms = std.MultiArrayList(GSymVar){};
    var locals = std.MultiArrayList(LocalVar){};
    var globals = std.MultiArrayList(GlobalVar){};
    gsyms.ensureTotalCapacity(al, VM.MAX_GSYM_ITEMS) catch {};
    locals.ensureTotalCapacity(al, VM.MAX_LOCAL_ITEMS) catch {};
    return Self {
      .code = code,
      .allocator = allocator,
      .gsyms = gsyms,
      .locals = locals,
      .globals = globals,
      .loop_ctrls = ds.ArrayList(*ast.ControlNode).init(al),
      .vreg = VRegister.init(al),
      .vm = vm,
    };
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

  inline fn optimizeConstRK(self: *Self) void {
    self.rk_bx.optimizeRK += 1;
  }

  inline fn deoptimizeConstRK(self: *Self) void {
    self.rk_bx.optimizeRK -= 1;
  }

  inline fn optimizeConstBX(self: *Self) void {
    self.rk_bx.optimizeBX += 1;
  }

  inline fn deoptimizeConstBX(self: *Self) void {
    self.rk_bx.optimizeBX -= 1;
  }

  inline fn canOptimizeConstRK(self: *Self) bool {
    return self.rk_bx.optimizeRK > 0 and self.rk_bx.in_jmp == 0;
  }

  inline fn canOptimizeConstBX(self: *Self) bool {
    return self.rk_bx.optimizeBX > 0 and self.rk_bx.in_jmp == 0;
  }

  inline fn enterJmp(self: *Self) void {
    self.rk_bx.in_jmp += 1;
  }

  inline fn leaveJmp(self: *Self) void {
    self.rk_bx.in_jmp -= 1;
  }

  /// return true if within RK limit else false
  fn withinRKLimit(self: *Self, num_operands: usize) bool {
    // check that we don't exceed the 9 bits of rk (+ num_operands for number of operands to be added)
    return ((self.code.values.items.len + value.MAX_REGISTERS + num_operands) <= value.Code._9bits);
  }

  /// return true if within BX limit else false
  fn withinBXLimit(self: *Self) bool {
    // check that we don't exceed the 18 bits of bx (+ 1 for the new addition)
    return ((self.code.values.items.len + value.MAX_REGISTERS + 1) <= value.Code._18bits);
  }

  fn addLocal(self: *Self, node: *ast.VarNode) u32 {
    // add a local and return its register.
    if (self.locals.len >= MAX_LOCALS) {
      self.compileError("Too many locals", .{});
    }
    var reg = self.getReg();
    self.locals.append(
      self.allocator.getArenaAllocator(),
      LocalVar.init(self.scope, &node.token.value, reg, @intCast(u32, self.locals.len))
    ) catch {};
    return reg;
  }

  /// add gsym and return its index/pos
  fn addGSymVar(self: *Self, node: *ast.VarNode) struct {pos: u32, isGSym: bool} {
    // Deduplicate globals. If a spot is already allocated for a global 'a', 
    // a redefinition of 'a' should not allocate a new spot.
    if (self.findGlobal(node)) |info| {
      return .{.pos = info.pos, .isGSym = info.isGSym};
    }
    if (self.gsyms.len >= MAX_GSYMS) {
      std.log.debug("Gsyms exceeded..using globals list\n", .{});
      return .{.pos = self.addGlobalVar(node), .isGSym = false};
    }
    var idx = @intCast(u32, self.gsyms.len);
    self.gsyms.append(
      self.allocator.getArenaAllocator(),
      GSymVar {.name = &node.token.value}
    ) catch {};
    return .{.pos = idx, .isGSym = true};
  }

  fn addGlobalVar(self: *Self, node: *ast.VarNode) u32 {
    var gvar = GlobalVar {
      .name = &node.token.value, 
      .mempos = self.storeVar(node),
      .index = @intCast(u32, self.globals.len),
    };
    self.globals.append(self.allocator.getArenaAllocator(), gvar) catch {};
    return gvar.mempos;
  }

  fn findLocalVar(self: *Self, node: *ast.VarNode) ?LocalVar {
    if (self.locals.len == 0) return null;
    var i = self.locals.len;
    var slice = self.locals.slice(); 
    var scopes = slice.items(.scope);
    var names = slice.items(.name);
    while (i > 0): (i -= 1) {
      var idx = i - 1;
      if (self.scope < scopes[idx]) break;
      if (std.mem.eql(u8, names[idx].*, node.token.value)) {
        return self.locals.get(idx);
      }
    }
    return null;
  }

  fn findGSymVar(self: *Self, node: *ast.VarNode) ?u32 {
    if (self.gsyms.len == 0) return null;
    var i: usize = self.gsyms.len;
    var names = self.gsyms.items(.name);
    while (i > 0): (i -= 1) {
      if (std.mem.eql(u8, names[i - 1].*, node.token.value)) {
        return @intCast(u32, i - 1);
      }
    }
    return null;
  }

  fn findGlobalVar(self: *Self, node: *ast.VarNode) ?GlobalVar {
    if (self.globals.len == 0) return null;
    var i: usize = self.globals.len;
    var names = self.globals.items(.name);
    while (i > 0): (i -= 1) {
      if (std.mem.eql(u8, names[i - 1].*, node.token.value)) {
        return self.globals.get(i - 1);
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
    if (self.locals.len == 0) return;
    var i: usize = self.locals.len;
    var slice = self.locals.slice(); 
    var scopes = slice.items(.scope);
    var regs = slice.items(.reg);
    while (i > 0): (i -= 1) {
      var idx = i - 1;
      if (scopes[idx] > self.scope) {
        self.vreg.releaseReg(regs[idx]);
        self.locals.len -= 1;
      }
    }
  }

  /// preallocate all globals in `gsyms` or `globals`
  fn preallocateGlobals(self: *Self, toplevels: *ast.AstNodeList) void {
    // TODO: update
    if (self.scope > GLOBAL_SCOPE) return;
    for (toplevels.items()) |decl| {
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
        self.gsyms.items(.patched)[info.pos] = true;
      } else {
        self.globals.items(.patched)[info.gvar.?.index] = true;
      }
      return info;
    }
    self.compileError("cannot patch variable '{s}'", .{node.token.value});
  }

  fn validateNoUnpatchedGlobals(self: *Self) void {
    const fmt = "Use of unpached global '{s}'";
    {
      var slice = self.gsyms.slice();
      var patches = slice.items(.patched);
      for (patches, 0..) |patched, i| {
        if (!patched) {
          self.compileError(fmt, .{slice.items(.name)[i].*});
        }
      }
    }
    {
      var slice = self.globals.slice();
      var patches = slice.items(.patched);
      for (patches, 0..) |patched, i| {
        if (!patched) {
          self.compileError(fmt, .{slice.items(.name)[i]});
        }
      }
    }
  }

  fn validateLocalVarUse(self: *Self, slot: u32, node: *ast.VarNode) void {
    if (!self.locals.items(.initialized)[slot]) {
      self.compileError("cannot use variable '{s}' in its own initializer", .{node.token.value});
    }
  }

  fn validateGlobalVarUse(self: *Self, info: GlobalVarInfo, node: *ast.VarNode) void {
    if (info.isGSym) {
      var slice = self.gsyms.slice();
      if (slice.items(.patched)[info.pos] and !slice.items(.initialized)[info.pos]) {
        self.compileError("cannot use variable '{s}' in its own initializer", .{node.token.value});
      }
    } else {
      var gvar = info.gvar.?;
      if (gvar.patched and !gvar.initialized) {
        self.compileError("cannot use variable '{s}' in its own initializer", .{node.token.value});
      }
    }
  }

  fn cConst(self: *Self, reg: u32, val: value.Value, line: usize) u32 {
    // load rx, memidx
    const memidx = self.code.writeValue(val, self.vm);
    if (self.canOptimizeConstRK() and self.withinRKLimit(1)) {
      return memidx + value.MAX_REGISTERS;
    } else if (self.canOptimizeConstBX() and self.withinBXLimit()) {
      return memidx + value.MAX_REGISTERS;
    } else {
      self.code.write2ArgsInst(.Load, reg, memidx, @intCast(u32, line), self.vm);
      return reg;
    }
  }

  fn storeVar(self: *Self, node: *ast.VarNode) u32 {
    var val = value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, false));
    return self.code.writeValue(val, self.vm);
  }

  fn cNumber(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    return self.cConst(dst, value.numberVal(node.value), node.line());
  }

  fn cBool(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    return self.cConst(dst, value.boolVal(node.token.is(.TkTrue)), node.line());
  }

  fn cString(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    return self.cConst(
      dst,
      value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, node.token.isAlloc())),
      node.line()
    );
  }

  fn cNil(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    return self.cConst(dst, value.nilVal(), node.line());
  }

  fn cUnary(self: *Self, node: *ast.UnaryNode, reg: u32) u32 {
    self.optimizeConstBX();
    const inst_op = node.op.optype.toInstOp();
    const rk = self.c(node.expr, reg);
    self.code.write2ArgsInst(inst_op, reg, rk, @intCast(u32, node.line()), self.vm);
    self.deoptimizeConstBX();
    return reg;
  }

  inline fn cCmp(self: *Self, node: *ast.BinaryNode) void {
    // <, >, <=, >=, ==, !=
    if (!node.op.optype.isCmpOp()) return;
    self.code.writeNoArgInst(@intToEnum(OpCode, @enumToInt(node.op.optype)), node.line(), self.vm);
  }

  inline fn cLgc(self: *Self, node: *ast.BinaryNode, reg: u32) u32 {
    // and, or
    // for and/or we do not want any const optimizations as we need consts to be loaded to registers
    // since const optimizations can cause no instructions to be emitted for consts. For ex: `4 or 5`
    // turn off const optimizations
    self.enterJmp();
    var rx = self.c(node.left, reg);
    const end_jmp = self.code.write2ArgsJmp(node.op.optype.toInstOp(), rx, self.lastLine(), self.vm);
    rx = self.c(node.right, reg);
    self.code.patch2ArgsJmp(end_jmp);
    // restore const optimizations
    self.leaveJmp();
    return reg;
  }

  fn cBinary(self: *Self, node: *ast.BinaryNode, dst: u32) u32 {
    // handle and | or
    if (node.op.optype.isLgcOp()) return self.cLgc(node, dst);
    if (node.op.optype == .OpIs) return self.cIs(node, dst);
    self.optimizeConstRK();
    var rk1 = self.c(node.left, dst);
    const dst2 = self.getReg();
    var rk2 = self.c(node.right, dst2);
    // we own this register, so we can free
    self.vreg.releaseReg(dst2);
    const inst_op = node.op.optype.toInstOp();
    self.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(u32, node.line()), self.vm);
    self.cCmp(node);
    self.deoptimizeConstRK();
    return dst;
  }

  fn cIs(self: *Self, node: *ast.BinaryNode, dst: u32) u32 {
    self.optimizeConstRK();
    var rk1 = self.c(node.left, dst);
    const dst2 = self.getReg();
    var rk2 = blk: {
      var typ = node.right.getType().?;
      var tag = tb: {
        if (typ.isGeneric()) {
          break :tb (
            if (typ.isListTy()) @enumToInt(TypeKind.TyClass)
            else if (typ.isMapTy()) @enumToInt(TypeKind.TyClass) + 1
            else  @enumToInt(TypeKind.TyClass) + 2
          );
        } else {
          break :tb @enumToInt(typ.concrete().tkind);
        }
      };
      break :blk self.cConst(
        // - 1 to properly exclude TyType
        dst2, value.numberVal(@intToFloat(f64, tag - 1)),
        node.op.token.line
      );
    };
    self.vreg.releaseReg(dst2);
    self.code.write3ArgsInst(OpCode.Is, dst, rk1, rk2, @intCast(u32, node.line()), self.vm);
    self.deoptimizeConstRK();
    return dst;
  }

  inline fn cCollection(self: *Self, node: *ast.ListNode, dst: u32, new: OpCode, set: OpCode) u32 {
    const size = @intCast(u32, node.elems.len());
    self.code.write2ArgsInst(new, dst, size, node.line(), self.vm);
    var idx: u32 = undefined;
    for (node.elems.items(), 0..) |elem, i| {
      var reg = self.getReg();
      var rk_val = self.c(elem, reg);
      var val = value.numberVal(@intToFloat(f64, i));
      if (self.withinRKLimit(1)) {
        idx = self.code.storeConst(val, self.vm);
      } else {
        idx = self.getReg();
        _ = self.cConst(idx, val, node.line());
        self.vreg.releaseReg(idx);
      }
      self.code.write3ArgsInst(set, dst, idx, rk_val, node.line(), self.vm);
      self.vreg.releaseReg(reg);
    }
    return dst;
  }

  fn cList(self: *Self, node: *ast.ListNode, dst: u32) u32 {
    return self.cCollection(node, dst, .Nlst, .Slst);
  }

  fn cTuple(self: *Self, node: *ast.ListNode, dst: u32) u32 {
    return self.cCollection(node, dst, .Ntup, .Stup);
  }

  fn cMap(self: *Self, node: *ast.MapNode, dst: u32) u32 {
    const size = @intCast(u32, node.pairs.len());
    // TODO: specialize map with k-v types
    self.code.write2ArgsInst(.Nmap, dst, size, node.line(), self.vm);
    for (node.pairs.items()) |pair| {
      var reg1 = self.getReg();
      var reg2 = self.getReg();
      var rk_key = self.c(pair.key, reg1);
      var rk_val = self.c(pair.value, reg2);
      self.code.write3ArgsInst(.Smap, dst, rk_key, rk_val, node.line(), self.vm);
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
      self.code.write2ArgsInst(inst, dst, info.pos, node.line(), self.vm);
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
        self.code.write3ArgsInst(.Mov, lvar.reg, ret, 0, node.line(), self.vm);
      }
      return lvar.reg;
    } else if (self.findGlobal(node)) |info| {
      // sgsym rx, bx -> GS[bx] = r(x)
      // sglb rx, bx -> G[K(bx)] = r(x)
      var rx = self.c(expr, reg);
      const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
      self.code.write2ArgsInst(inst, rx, info.pos, node.line(), self.vm);
      return rx;
    } else {
      self.compileError("undefined variable '{s}'", .{node.token.value});
    }
  }

  fn cSubscriptAssign(self: *Self, node: *ast.SubscriptNode, rhs: *Node, dst: u32) u32 {
    self.optimizeConstRK();
    // lhs-expr
    var rx = self.c(node.expr, dst);
    // index
    var idx_reg = self.getReg();
    var rk_idx = self.c(node.index, idx_reg);
    // rhs-expr
    var val_reg = self.getReg();
    var rk_val = self.c(rhs, val_reg);
    var op: OpCode = if (node.expr.getType().?.isListTy()) .Slst else .Smap;
    self.code.write3ArgsInst(op, rx, rk_idx, rk_val, node.line(), self.vm);
    self.vreg.releaseReg(idx_reg);
    self.vreg.releaseReg(val_reg);
    self.deoptimizeConstRK();
    return dst;
  }

  fn cAssign(self: *Self, node: *ast.BinaryNode, reg: u32) u32 {
    return switch (node.left.*) {
      .AstVar => |*vr| {
        return self.cVarAssign(vr, node.right, reg);
      },
      .AstSubscript => |*sub| {
        return self.cSubscriptAssign(sub, node.right, reg);
      },
      // would be unreachable after type checking
      else => self.compileError("invalid assignment target", .{}),
    };
  }

  fn cCast(self: *Self, node: *ast.CastNode, dst: u32) u32 {
    const ty = node.expr.getType();
    // generate code for bool cast, if the expression is not already a bool type
    if (ty != null and !ty.?.isBoolTy()) {
      if (node.typn.typ.isSimple() and node.typn.typ.isBoolTy()) {
        self.optimizeConstRK();
        var rk = self.c(node.expr, dst);
        self.code.write2ArgsInst(.Bcst, dst, rk, node.line(), self.vm);
        self.deoptimizeConstRK();
        return dst;
      }
    }
    return self.c(node.expr, dst);
  }

  fn cVarDecl(self: *Self, node: *ast.VarDeclNode, reg: u32) u32 {
    // let var = expr
    var bin: ast.BinaryNode = .{
      .left = &.{.AstVar = node.ident.*},
      .right = node.value,
      .op = undefined, .typ = node.ident.typ
    };
    if (self.scope > GLOBAL_SCOPE) {
      // local
      var dst = self.addLocal(node.ident);
      var lvar_pos = self.locals.len - 1;
      _ = self.cAssign(&bin, dst);
      // initialize the local after its rhs has been compiled successfully
      self.locals.items(.initialized)[lvar_pos] = true;
    } else {
      // global
      var info = self.patchGlobal(node.ident);
      // first, deinitialize the global just in case it was initialized by
      // some other code (since globals are deduplicated/shared)
      if (info.isGSym) {
        self.gsyms.items(.initialized)[info.pos] = false;
      } else {
        self.globals.items(.initialized)[info.gvar.?.index] = false;
      }
      var dst = self.getReg();
      _ = self.cAssign(&bin, dst);
      // now, initialize this global after its rhs has been compiled successfully
      if (info.isGSym) {
        self.gsyms.items(.initialized)[info.pos] = true;
      } else {
        self.globals.items(.initialized)[info.gvar.?.index] = true;
      }
      self.vreg.releaseReg(dst);
    }
    return reg;
  }

  fn cSubscript(self: *Self, node: *ast.SubscriptNode, dst: u32) u32 {
    if (node.expr.getType()) |typ| {
      self.optimizeConstRK();
      if (typ.isListTy() or typ.isTupleTy()) {
        // list index
        var rk_val = self.c(node.expr, dst);
        var reg = self.getReg();
        var rk_idx = self.c(node.index, reg);
        var op: OpCode = if (typ.isListTy()) .Glst else .Gtup;
        self.code.write3ArgsInst(op, dst, rk_idx, rk_val, node.line(), self.vm);
        self.vreg.releaseReg(reg);
      } else {
        // map access
        std.debug.assert(typ.isMapTy());
        var rk_val = self.c(node.expr, dst);
        var reg = self.getReg();
        var rk_key = self.c(node.index, reg);
        self.code.write3ArgsInst(.Gmap, dst, rk_key, rk_val, node.line(), self.vm);
        self.vreg.releaseReg(reg);
      }
      self.deoptimizeConstRK();
      return dst;
    }
    unreachable;
  }

  fn cDeref(self: *Self, node: *ast.DerefNode, reg: u32) u32 {
    var rx = self.c(node.expr, reg);
    self.code.write3ArgsInst(.Asrt, rx, 0, 0, node.token.line, self.vm);
    return reg;
  }

  fn cBlock(self: *Self, node: *ast.BlockNode, reg: u32) u32 {
    self.scope += 1;
    for (node.nodes.items()) |nd| {
      _ = self.c(nd, reg);
    }
    self.scope -= 1;
    self.popLocalVars();
    return reg;
  }

  fn cNType(self: *Self, node: *ast.TypeNode, reg: u32) u32 {
    // only compile if not in annotation/alias context
    if (!node.from_alias_or_annotation) {
      // TODO: should we be using the typeid() instead of a singular value 
      // (@enumToInt(TypeKind.TyType)) each time?
      // this makes sense for now, since having num == str be true is kinda counterintuitive
      // but again, no one should even be doing this in the first place! 
      return self.cConst(
        reg, value.numberVal(@intToFloat(f64, node.typ.typeid())),
        node.token.line
      );
    }
    return reg;
  }

  const JmpPatch = struct{jmp_to_next: usize, jmp_to_end: usize};

  fn cIf(self: *Self, node: *ast.IfNode, dst: u32) u32 {
    // self.enterJmp();
    var reg = self.getReg();
    // if-
    var rx = self.c(node.cond, reg);
    // jmp to elif, if any
    var cond_to_elif_or_else = self.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = self.cBlock(&node.then.AstBlock, reg);
    var should_patch_if_then_to_end = true;
    var if_then_to_end = blk: {
      // as a simple optimization, if we only have an if-end statement, i.e. no elif & else,
      // we don't need to jump after the last statement in the if-then block, control
      // would naturally fallthrough to outside the if-end statement.
      if (node.elifs.len() == 0 and node.els.AstBlock.nodes.len() == 0) {
        should_patch_if_then_to_end = false;
        break :blk @as(usize, 0);
      }
      break :blk self.code.write2ArgsJmp(.Jmp, 2, self.lastLine(), self.vm);
    };
    var elifs_then_to_end: ds.ArrayList(usize) = undefined;
    // elifs-
    if (node.elifs.len() > 0) {
      // first, patch cond_to_elif_or_else here
      self.code.patch2ArgsJmp(cond_to_elif_or_else);
      elifs_then_to_end = ds.ArrayList(usize).init(self.allocator.getArenaAllocator());
      var last_patch: ?JmpPatch = null;
      for (node.elifs.items()) |elif| {
        if (last_patch) |pch| {
          // we want the last compiled elif's failing cond to jump here; 
          // - just before the next elif code
          self.code.patch2ArgsJmp(pch.jmp_to_next);
        }
        var patch: JmpPatch = self.cElif(&elif.AstElif, reg);
        elifs_then_to_end.append(patch.jmp_to_end);
        last_patch = patch;
      }
      // the last elif in `elifs` would not be patched considering we're 
      // patching the first only after we've seen the second, hence, we patch it here
      self.code.patch2ArgsJmp(last_patch.?.jmp_to_next);
    } else {
      self.code.patch2ArgsJmp(cond_to_elif_or_else);
    }
    // else-
    _ = self.cBlock(&node.els.AstBlock, reg);
    if (node.elifs.len() > 0) {
      // patch up elif_then_to_end 
      for (elifs_then_to_end.items()) |idx| {
        self.code.patch2ArgsJmp(idx);
      }
    }
    if (should_patch_if_then_to_end) {
      self.code.patch2ArgsJmp(if_then_to_end);
    }
    // self.leaveJmp();
    self.vreg.releaseReg(reg);
    return dst;
  }

  fn patchLoopJmps(self: *Self, loop_cond: usize) void {
    for (self.loop_ctrls.items()) |ctrl| {
      if (ctrl.isBreak()) {
        // this is a forward jmp, so patch the jmp offset
        self.code.patch2ArgsJmp(ctrl.patch_index);
      } else {
        // obtain offset to jmp forward to, then rewrite the jmp instruction
        var offset = @intCast(u32, ctrl.patch_index - loop_cond + 1);
        _ = self.code.write2ArgsInst(.Jmp, 0, offset, ctrl.token.line, self.vm);
        self.code.words.items[ctrl.patch_index] = self.code.words.pop();
      }
    }
    // clear for reuse
    self.loop_ctrls.clearRetainingCapacity();
  }

  fn cWhile(self: *Self, node: *ast.WhileNode, dst: u32) u32 {
    var reg = self.getReg();
    // cond-
    var to_cond = self.code.getInstLen();
    var rx = self.c(node.cond, reg);
    // jmp to exit
    var cond_to_exit = self.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = self.cBlock(&node.then.AstBlock, reg);
    // loop to cond
    // +1 to include the jmp inst itself, which we already processed
    var offset = @intCast(u32, self.code.getInstLen() - to_cond + 1);
    _ = self.code.write2ArgsInst(.Jmp, 0, offset, self.lastLine(), self.vm);
    self.code.patch2ArgsJmp(cond_to_exit);
    self.vreg.releaseReg(reg);
    // patch all loop controls
    self.patchLoopJmps(to_cond);
    return dst;
  }

  fn cControl(self: *Self, node: *ast.ControlNode, dst: u32) u32 {
    if (node.isBreak()) {
      // jmp fwd
      node.patch_index = self.code.write2ArgsJmp(.Jmp, 2, node.token.line, self.vm);
    } else {
      // jmp bck
      node.patch_index = self.code.write2ArgsJmp(.Jmp, 0, node.token.line, self.vm);
    }
    self.loop_ctrls.append(node);
    return dst;
  }

  /// returns jmp_to_next for patching the jmp to the next elif/else/,
  /// and jmp_to_end for patching the jmp to the end of the entire if-stmt
  fn cElif(self: *Self, node: *ast.ElifNode, reg: u32) JmpPatch {
    var rx = self.c(node.cond, reg);
    // jmp to else, if any
    var cond_jmp = self.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = self.cBlock(&node.then.AstBlock, reg);
    var then_jmp = self.code.write2ArgsJmp(.Jmp, 2, self.lastLine(), self.vm);
    return .{.jmp_to_next = cond_jmp, .jmp_to_end = then_jmp};
  }

  fn cProgram(self: *Self, node: *ast.ProgramNode, reg: u32) u32 {
    for (node.decls.items()) |nd| {
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
      .AstTuple => |*nd| self.cTuple(nd, reg),
      .AstMap => |*nd| self.cMap(nd, reg),
      .AstExprStmt => |*nd| self.cExprStmt(nd, reg),
      .AstVar => |*nd| self.cVar(nd, reg),
      .AstVarDecl => |*nd| self.cVarDecl(nd, reg),
      .AstAssign => |*nd| self.cAssign(nd, reg),
      .AstBlock => |*nd| self.cBlock(nd, reg),
      .AstNType => |*nd| self.cNType(nd, reg),
      .AstAlias => reg, // TODO
      .AstNil => |*nd| self.cNil(nd, reg),
      .AstCast => |*nd| self.cCast(nd, reg),
      .AstSubscript => |*nd| self.cSubscript(nd, reg),
      .AstDeref => |*nd| self.cDeref(nd, reg),
      .AstIf => |*nd| self.cIf(nd, reg),
      .AstWhile => |*nd| self.cWhile(nd, reg),
      .AstControl => |*nd| self.cControl(nd, reg),
      .AstProgram => |*nd| self.cProgram(nd, reg),
      .AstSimpleIf, .AstElif, .AstCondition, .AstEmpty => unreachable,
    };
  }

  pub fn compile(self: *Self, node: *Node) void {
    self.preallocateGlobals(&node.AstProgram.decls);
    util.assert(self.c(node, VRegister.DummyReg) == VRegister.DummyReg, "should be a dummy register");
    const last_line = if (self.code.lines.items.len > 0) self.code.lines.items[self.code.lines.items.len - 1] else 0;
    self.code.writeNoArgInst(.Ret, last_line, self.vm);
    self.validateNoUnpatchedGlobals();
    // release memory associated with the arena, since we're done with it at this point.
    self.allocator.deinitArena();
  }

};
