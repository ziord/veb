const std = @import("std");
const value = @import("value.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");
const CnAllocator = @import("allocator.zig");
const diagnostics = @import("diagnostics.zig");
const check = @import("check.zig");
const Disassembler = @import("debug.zig").Disassembler;
const OpCode = @import("opcode.zig").OpCode;
const VM = @import("vm.zig").VM;
const Token = ast.Token;

const Code = value.Code;
const Node = ast.AstNode;
const Inst = value.Inst;
const ObjFn = value.ObjFn;
const TypeKind = parse.TypeKind;
const Diagnostic = diagnostics.Diagnostic;
const FnInfo = check.TypeChecker.FnInfo;
const FnInfoMap = ds.ArrayHashMap(*Node, *ds.ArrayList(FnInfo));
const TypeList = check.TypeList;

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

  /// get a series of registers in increments of 1, up to n starting with a known free register. 
  /// Returns the register after `reg` in the series
  pub fn getRegWindow(self: *Self, reg: u32, n: usize) error{RegistersExhausted, RegistersNonSequenceable}!u32 {
    if (n == 0) return reg;
    var slice = self.regs.slice();
    var frees = slice.items(.free);
    var vals = slice.items(.val);
    var curr = reg;
    var j: usize = 0;
    var i: usize = reg + 1;
    for (frees[i..], vals[i..]) |free, val| {
      if (!free) continue;
      if ((val - curr) != 1) return error.RegistersNonSequenceable;
      frees[i] = false;
      curr = val;
      j += 1;
      i += 1;
      if (j == n) break;
    }
    std.debug.assert(j == n);
    return reg + 1;
  }

  /// like getRegWindow(), but can choose a register sequence (window) 
  /// from any register above `reg` that sequences.
  pub fn getAnyRegWindow(self: *Self, reg: u32, n: usize) u32 {
    if (n == 0) return reg;
    var slice = self.regs.slice();
    var frees = slice.items(.free);
    var vals = slice.items(.val);
    var curr = reg;
    var start = reg + 1;
    var j: usize = 0;
    var i: usize = undefined;
    main: while (true) {
      i = start;
      for (frees[start..], vals[start..]) |free, val| {
        if (!free) {
          continue;
        }
        if ((val - curr) != 1) {
          start = val;
          curr = val - 1;
          j = 0;
          break;
        }
        frees[i] = false;
        curr = val;
        j += 1;
        i += 1;
        if (j == n) break :main;
      }
    }
    std.debug.assert(j == n);
    return start;
  }

  /// release all registers in this register window, starting from the first
  pub fn releaseRegWindow(self: *Self, reg: u32, n: usize) void {
    if (n == 0) return;
    var j = n;
    var slice = self.regs.slice();
    var vals = slice.items(.val);
    var frees = slice.items(.free);
    var r = reg;
    for (vals, frees, 0..) |val, free, i| {
      if (j == 0) break;
      if (val == r) {
        std.debug.assert(!free);
        frees[i] = true;
        r += 1;
        j -= 1;
      }
    }
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
  captured: bool = false,

  pub fn init(scope: i32, name: *[]const u8, reg: u32, index: u32) @This() {
    return @This() {.scope = scope, .name = name, .reg = reg, .index = index};
  }
};

const Upvalue = struct {
  index: u32,
  is_local: bool,

  pub fn init(index: u32, is_local: bool) @This() {
    return @This() {.index = index, .is_local = is_local};
  }
};

pub const Compiler = struct {
  fun: *ObjFn,
  gsyms: std.MultiArrayList(GSymVar),
  locals: std.MultiArrayList(LocalVar),
  upvalues: std.MultiArrayList(Upvalue),
  globals: std.MultiArrayList(GlobalVar),
  loop_ctrls: ds.ArrayList(*ast.ControlNode),
  vreg: VRegister,
  allocator: *CnAllocator,
  enclosing: ?*Compiler = null,
  vm: *VM,
  scope: i32 = GLOBAL_SCOPE, // defaults to global scope
  rk_bx: RkBxPair = RkBxPair{},
  diag: *Diagnostic,
  generics: *FnInfoMap,
  
  const Self = @This();

  const GLOBAL_SCOPE = 0;
  const MAX_LOCALS = VM.MAX_LOCAL_ITEMS;
  const MAX_GSYMS = VM.MAX_GSYM_ITEMS;
  const GlobalVarInfo = struct {
    pos: u32,
    isGSym: bool,
    gvar: ?GlobalVar,
    /// whether this GlobalVarInfo from an enclosing compiler instead of the current compiler
    from_enclosing: bool = false,
  };
  const RkBxPair = struct {
    optimizeRK: u32 = 0, // can optimize for rk
    optimizeBX: u32 = 0, // can optimize for bx
    in_jmp: u32 = 0,
  };
  const CompileError = error{CompileError};

  pub fn init(diag: *Diagnostic, vm: *VM, fun: *ObjFn, info: *FnInfoMap, allocator: *CnAllocator) Self {
    var al = allocator.getArenaAllocator();
    var gsyms = std.MultiArrayList(GSymVar){};
    var locals = std.MultiArrayList(LocalVar){};
    var upvalues = std.MultiArrayList(Upvalue){};
    var globals = std.MultiArrayList(GlobalVar){};
    gsyms.ensureTotalCapacity(al, MAX_GSYMS) catch {};
    locals.ensureTotalCapacity(al, MAX_LOCALS) catch {};
    return Self {
      .fun = fun,
      .allocator = allocator,
      .gsyms = gsyms,
      .locals = locals,
      .upvalues = upvalues,
      .globals = globals,
      .loop_ctrls = ds.ArrayList(*ast.ControlNode).init(al),
      .vreg = VRegister.init(al),
      .vm = vm,
      .diag = diag,
      .generics = info,
    };
  }

  fn compileError(self: *Self, token: Token, comptime fmt: []const u8, msg: anytype) CompileError {
    self.diag.addDiagnostics(.DiagError, token, "CompileError: " ++ fmt ++ "\n", msg);
    return error.CompileError;
  }

  inline fn alloc(self: *Self) std.mem.Allocator {
    return self.allocator.getArenaAllocator();
  }

  fn lastLine(self: *Self) u32 {
    return (
      if (self.fun.code.lines.len > 0)
        self.fun.code.lines.items[self.fun.code.lines.len - 1]
      else 
        1
    );
  }

  inline fn incScope(self: *Self) void {
    self.scope += 1;
  }

  inline fn decScope(self: *Self) void {
    self.scope -= 1;
  }

  fn getReg(self: *Self, debug: Token) !u32 {
    return self.vreg.getReg() catch {
      return self.compileError(debug, "registers exhausted", .{});
    };
  }

  fn getRegWindow(self: *Self, reg: u32, n: usize, debug: Token) !u32 {
    return self.vreg.getRegWindow(reg, n) catch |e| {
      return self.compileError(debug, "Could not obtain register window: {}", .{e});
    };
  }

  inline fn inGlobalScope(self: *Self) bool {
    return self.scope == GLOBAL_SCOPE;
  }

  inline fn inLocalScope(self: *Self) bool {
    return self.scope > GLOBAL_SCOPE;
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
    return ((self.fun.code.values.len + value.MAX_REGISTERS + num_operands) <= value.Code._9bits);
  }

  /// return true if within BX limit else false
  fn withinBXLimit(self: *Self) bool {
    // check that we don't exceed the 18 bits of bx (+ 1 for the new addition)
    return ((self.fun.code.values.len + value.MAX_REGISTERS + 1) <= value.Code._18bits);
  }

  fn addLocal(self: *Self, node: *ast.VarNode) !u32 {
    // add a local and return its register.
    if (self.locals.len >= MAX_LOCALS) {
      return self.compileError(node.token, "Too many locals", .{});
    }
    var reg = try self.getReg(node.token);
    self.locals.append(
      self.alloc(),
      LocalVar.init(self.scope, &node.token.value, reg, @intCast(u32, self.locals.len))
    ) catch {};
    return reg;
  }

  fn initLocal(self: *Self, node: *ast.VarNode) !u32 {
    var dst = try self.addLocal(node);
    self.locals.items(.initialized)[self.locals.len - 1] = true;
    return dst;
  }

  fn addUpvalue(self: *Self, index: u32, is_local: bool, debug: Token) !u32 {
     if (self.locals.len >= MAX_LOCALS) {
      return self.compileError(debug, "Too many upvalues", .{});
    }
    var slice = self.upvalues.slice();
    var indexs = slice.items(.index);
    var is_locals = slice.items(.is_local);
    for (indexs, is_locals, 0..) |idx, loc, i| {
      if (index == idx and is_local == loc) {
        return @intCast(u32, i);
      }
    }
    var ret = @intCast(u32, self.upvalues.len);
    self.upvalues.append(self.alloc(), Upvalue.init(index, is_local)) catch |e| {
      std.debug.print("error: {}\n", .{e});
      return error.CompileError;
    };
    return ret;
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
      self.alloc(),
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
    self.globals.append(self.alloc(), gvar) catch {};
    return gvar.mempos;
  }

  inline fn addPatchInitGlobalVar(self: *Self, node: *ast.VarNode) void {
     _ = self.addGSymVar(node);
    var info = self.patchGlobal(node) catch return;
    self.initializeGlobal(info);
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

  fn findUpvalue(self: *Self, node: *ast.VarNode) ?u32 {
    if (self.enclosing == null) return null;
    var compiler = self.enclosing.?;
    var local = compiler.findLocalVar(node);
    if (local) |loc| {
      compiler.locals.items(.captured)[loc.index] = true;
      return self.addUpvalue(loc.index, true, node.token) catch null;
    }
    var index = compiler.findUpvalue(node);
    if (index) |idx| {
      return self.addUpvalue(idx, false, node.token) catch null;
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
    } else if (self.enclosing) |compiler| {
      if (compiler.findGlobal(node)) |info| {
        var ret = info;
        ret.from_enclosing = true;
        return ret;
      }
    }
    return null;
  }

  fn popLocalVars(self: *Self) void {
    if (self.locals.len == 0) return;
    var slice = self.locals.slice(); 
    var scopes = slice.items(.scope);
    var regs = slice.items(.reg);
    var captures = slice.items(.captured);
    var close: ?u32 = null;
    for (scopes, regs, captures) |scope, reg, captured| {
      if (scope > self.scope) {
        // only take the first local, close upwards from there
        if (captured and close == null) {
          close = reg;
        }
        self.vreg.releaseReg(reg);
        self.locals.len -= 1;
      }
    }
    if (close) |reg| {
      self.fun.code.write2ArgsInst(.Cupv, reg, 0, self.lastLine(), self.vm);
    }
  }

  /// preallocate all globals in `gsyms` or `globals`
  fn preallocateGlobals(self: *Self, toplevels: *ast.AstNodeList) void {
    // TODO: update
    if (self.inLocalScope()) return;
    for (toplevels.items()) |decl| {
      switch (decl.*) {
        .AstVarDecl => |*vd| {
          _ = self.addGSymVar(vd.ident);
        },
        .AstFun => |*fun| {
          if (fun.isGeneric()) {
            if (fun.name) |ident| self.addPatchInitGlobalVar(ident);
            if (self.generics.get(decl)) |list| {
              for (list.items()) |itm| {
                var nfun = &itm.instance.AstFun;
                nfun.name = itm.synth_name;
                _ = self.addGSymVar(itm.synth_name);
              }
            }
          } else {
            if (fun.name) |ident| {
              _ = self.addGSymVar(ident);
            }
          }
        },
        else => {},
      }
    }
  }

  /// patch a preallocated global entry
  fn patchGlobal(self: *Self, node: *ast.VarNode) !GlobalVarInfo {
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
    return self.compileError(node.token, "cannot patch variable '{s}'", .{node.token.value});
  }

  fn initializeGlobal(self: *Self, info: GlobalVarInfo) void {
    if (info.isGSym) {
      self.gsyms.items(.initialized)[info.pos] = true;
    } else {
      self.globals.items(.initialized)[info.gvar.?.index] = true;
    }
  }

  fn validateNoUnpatchedGlobals(self: *Self, token: Token) !void {
    const fmt = "Use of unpached global '{s}'";
    {
      var slice = self.gsyms.slice();
      var patches = slice.items(.patched);
      for (patches, 0..) |patched, i| {
        if (!patched) {
          return self.compileError(token, fmt, .{slice.items(.name)[i].*});
        }
      }
    }
    {
      var slice = self.globals.slice();
      var patches = slice.items(.patched);
      for (patches, 0..) |patched, i| {
        if (!patched) {
          return self.compileError(token, fmt, .{slice.items(.name)[i]});
        }
      }
    }
  }

  fn validateLocalVarUse(self: *Self, slot: u32, node: *ast.VarNode) !void {
    if (!self.locals.items(.initialized)[slot]) {
      return self.compileError(node.token, "cannot use variable '{s}' in its own initializer", .{node.token.value});
    }
  }

  fn validateGlobalVarUse(self: *Self, info: GlobalVarInfo, node: *ast.VarNode) !void {
    if (info.from_enclosing) {
      // this global var was obtained from an enclosing compiler's
      // global state, so it IS already validated. Just return.
      return;
    }
    if (info.isGSym) {
      var slice = self.gsyms.slice();
      if (slice.items(.patched)[info.pos] and !slice.items(.initialized)[info.pos]) {
        return self.compileError(node.token, "cannot use variable '{s}' in its own initializer", .{node.token.value});
      }
    } else {
      var gvar = info.gvar.?;
      if (gvar.patched and !gvar.initialized) {
        return self.compileError(node.token, "cannot use variable '{s}' in its own initializer", .{node.token.value});
      }
    }
  }

  fn cConst(self: *Self, reg: u32, val: value.Value, line: usize) u32 {
    // load rx, memidx
    const memidx = self.fun.code.writeValue(val, self.vm);
    if (self.canOptimizeConstRK() and self.withinRKLimit(1)) {
      return memidx + value.MAX_REGISTERS;
    } else if (self.canOptimizeConstBX() and self.withinBXLimit()) {
      return memidx + value.MAX_REGISTERS;
    } else {
      self.fun.code.write2ArgsInst(.Load, reg, memidx, @intCast(u32, line), self.vm);
      return reg;
    }
  }

  fn storeVar(self: *Self, node: *ast.VarNode) u32 {
    var val = value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, false));
    return self.fun.code.writeValue(val, self.vm);
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

  fn cUnary(self: *Self, node: *ast.UnaryNode, reg: u32) CompileError!u32 {
    self.optimizeConstBX();
    const inst_op = node.op.optype.toInstOp();
    const rk = try self.c(node.expr, reg);
    self.fun.code.write2ArgsInst(inst_op, reg, rk, @intCast(u32, node.line()), self.vm);
    self.deoptimizeConstBX();
    return reg;
  }

  inline fn cLgc(self: *Self, node: *ast.BinaryNode, reg: u32) CompileError!u32 {
    // and, or
    // for and/or we do not want any const optimizations as we need consts to be loaded to registers
    // since const optimizations can cause no instructions to be emitted for consts. For ex: `4 or 5`
    // turn off const optimizations
    self.enterJmp();
    var rx = try self.c(node.left, reg);
    const end_jmp = self.fun.code.write2ArgsJmp(node.op.optype.toInstOp(), rx, node.line(), self.vm);
    rx = try self.c(node.right, reg);
    self.fun.code.patch2ArgsJmp(end_jmp);
    // restore const optimizations
    self.leaveJmp();
    return reg;
  }

  fn cBinary(self: *Self, node: *ast.BinaryNode, dst: u32) CompileError!u32 {
    // handle and | or
    if (node.op.optype.isLgcOp()) return try self.cLgc(node, dst);
    if (node.op.optype == .OpIs) return try self.cIs(node, dst);
    self.optimizeConstRK();
    var rk1 = try self.c(node.left, dst);
    const dst2 = try self.getReg(node.left.getToken());
    var rk2 = try self.c(node.right, dst2);
    // we own this register, so we can free
    self.vreg.releaseReg(dst2);
    const inst_op = node.op.optype.toInstOp();
    self.fun.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(u32, node.line()), self.vm);
    self.deoptimizeConstRK();
    return dst;
  }

  fn cIs(self: *Self, node: *ast.BinaryNode, dst: u32) !u32 {
    self.optimizeConstRK();
    var rk1 = try self.c(node.left, dst);
    const dst2 = try self.getReg(node.left.getToken());
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
    self.fun.code.write3ArgsInst(OpCode.Is, dst, rk1, rk2, @intCast(u32, node.line()), self.vm);
    self.deoptimizeConstRK();
    return dst;
  }

  inline fn cCollection(self: *Self, node: *ast.ListNode, ast_node: *Node, dst: u32, new: OpCode, set: OpCode) !u32 {
    const size = @intCast(u32, node.elems.len());
    self.fun.code.write2ArgsInst(new, dst, size, self.lastLine(), self.vm);
    var token = ast_node.getToken();
    var idx: u32 = undefined;
    for (node.elems.items(), 0..) |elem, i| {
      var reg = try self.getReg(token);
      var rk_val = try self.c(elem, reg);
      var val = value.numberVal(@intToFloat(f64, i));
      if (self.withinRKLimit(1)) {
        idx = self.fun.code.storeConst(val, self.vm);
      } else {
        idx = try self.getReg(token);
        _ = self.cConst(idx, val, self.lastLine());
        self.vreg.releaseReg(idx);
      }
      self.fun.code.write3ArgsInst(set, dst, idx, rk_val, self.lastLine(), self.vm);
      self.vreg.releaseReg(reg);
    }
    return dst;
  }

  fn cList(self: *Self, node: *ast.ListNode, ast_node: *Node, dst: u32) !u32 {
    return try self.cCollection(node, ast_node, dst, .Nlst, .Slst);
  }

  fn cTuple(self: *Self, node: *ast.ListNode, ast_node: *Node, dst: u32) !u32 {
    return try self.cCollection(node, ast_node, dst, .Ntup, .Stup);
  }

  fn cMap(self: *Self, node: *ast.MapNode, dst: u32) !u32 {
    const size = @intCast(u32, node.pairs.len());
    // TODO: specialize map with k-v types
    self.fun.code.write2ArgsInst(.Nmap, dst, size, self.lastLine(), self.vm);
    var token = Token.getDefault();
    for (node.pairs.items()) |pair| {
      var reg1 = try self.getReg(token);
      var reg2 = try self.getReg(token);
      var rk_key = try self.c(pair.key, reg1);
      var rk_val = try self.c(pair.value, reg2);
      self.fun.code.write3ArgsInst(.Smap, dst, rk_key, rk_val, self.lastLine(), self.vm);
      self.vreg.releaseReg(reg1);
      self.vreg.releaseReg(reg2);
    }
    return dst;
  }

  fn cExprStmt(self: *Self, node: *ast.ExprStmtNode, reg: u32) !u32 {
    // sequence point
    const dst = try self.getReg(node.expr.getToken());
    _ = try self.c(node.expr, dst);
    self.vreg.releaseReg(dst);
    return reg;
  }

  fn cVar(self: *Self, node: *ast.VarNode, dst: u32) !u32 {
    // Sometimes, the lookup of generic and non-generic function calls may collide
    // based on how such calls were made.
    // To handle such an edge case, we *first* check if this node is a function type.
    // Since the type checker guarantees that the node embedded in the function type is the
    // function being called, we can safely use the function's name for lookup
    // which if generic, should already be synthesized.
    if (node.typ != null and node.typ.?.isFunction()) {
      var fun = &node.typ.?.function().node.AstFun;
      if (fun.isGeneric()) {
        return self.cConst(dst, value.numberVal(@intToFloat(f64, node.typ.?.typeid())), node.token.line);
      } else if (fun.name) |ident| {
        return try self.cVar(ident, dst);
      }
    }
    if (self.findLocalVar(node)) |lvar| {
      try self.validateLocalVarUse(lvar.index, node);
      return lvar.reg;
    } else if (self.findUpvalue(node)) |upv| {
      self.fun.code.write2ArgsInst(.Gupv, dst, upv, node.line(), self.vm);
      return dst;
    } else if (self.findGlobal(node)) |info| {
      try self.validateGlobalVarUse(info, node);
      const inst: OpCode = if(info.isGSym) .Ggsym else .Gglb;
      self.fun.code.write2ArgsInst(inst, dst, info.pos, node.line(), self.vm);
      return dst;
    } else {
      return self.compileError(node.token, "use of undefined variable '{s}'", .{node.token.value});
    }
  }

  fn cVarAssign(self: *Self, node: *ast.VarNode, expr: *Node, reg: u32) !u32 {
    if (self.findLocalVar(node)) |lvar| {
      // use lvar's reg as dst for expr
      var ret = try self.c(expr, lvar.reg);
      if (ret != lvar.reg) {
        // this implies the result of expr was stored in a different register.
        // Move it to lvar's register.
        self.fun.code.write3ArgsInst(.Mov, lvar.reg, ret, 0, node.line(), self.vm);
      }
      return lvar.reg;
    } else if (self.findUpvalue(node)) |upv| {
      var rx = try self.c(expr, reg);
      self.fun.code.write2ArgsInst(.Supv, rx, upv, node.line(), self.vm);
      return reg;
    } else if (self.findGlobal(node)) |info| {
      // sgsym rx, bx -> GS[bx] = r(x)
      // sglb rx, bx -> G[K(bx)] = r(x)
      var rx = try self.c(expr, reg);
      const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
      self.fun.code.write2ArgsInst(inst, rx, info.pos, node.line(), self.vm);
      return rx;
    } else {
      return self.compileError(node.token, "undefined variable '{s}'", .{node.token.value});
    }
  }

  fn cSubscriptAssign(self: *Self, node: *ast.SubscriptNode, rhs: *Node, dst: u32) !u32 {
    self.optimizeConstRK();
    // lhs-expr
    var rx = try self.c(node.expr, dst);
    // index
    var idx_reg = try self.getReg(node.expr.getToken());
    var rk_idx = try self.c(node.index, idx_reg);
    // rhs-expr
    var val_reg = try self.getReg(node.index.getToken());
    var rk_val = try self.c(rhs, val_reg);
    var op: OpCode = if (node.expr.getType().?.isListTy()) .Slst else .Smap;
    self.fun.code.write3ArgsInst(op, rx, rk_idx, rk_val, node.line(), self.vm);
    self.vreg.releaseReg(idx_reg);
    self.vreg.releaseReg(val_reg);
    self.deoptimizeConstRK();
    return dst;
  }

  fn cAssign(self: *Self, node: *ast.BinaryNode, reg: u32) !u32 {
    return switch (node.left.*) {
      .AstVar => |*vr| {
        return try self.cVarAssign(vr, node.right, reg);
      },
      .AstSubscript => |*sub| {
        return try self.cSubscriptAssign(sub, node.right, reg);
      },
      // would be unreachable after type checking
      else => return self.compileError(node.left.getToken(), "invalid assignment target", .{}),
    };
  }

  fn cCast(self: *Self, node: *ast.CastNode, dst: u32) !u32 {
    const ty = node.expr.getType();
    // generate code for bool cast, if the expression is not already a bool type
    if (ty != null and !ty.?.isBoolTy()) {
      if (node.typn.typ.isSimple() and node.typn.typ.isBoolTy()) {
        self.optimizeConstRK();
        var rk = try self.c(node.expr, dst);
        self.fun.code.write2ArgsInst(.Bcst, dst, rk, node.line(), self.vm);
        self.deoptimizeConstRK();
        return dst;
      }
    }
    return try self.c(node.expr, dst);
  }

  fn cVarDecl(self: *Self, node: *ast.VarDeclNode, reg: u32) !u32 {
    // let var = expr
    var bin: ast.BinaryNode = .{
      .left = @constCast(&@as(Node, .{.AstVar = node.ident.*})),
      .right = node.value,
      .op = undefined, .typ = node.ident.typ
    };
    if (self.inLocalScope()) {
      // local
      var dst = try self.addLocal(node.ident);
      var lvar_pos = self.locals.len - 1;
      _ = try self.cAssign(&bin, dst);
      // initialize the local after its rhs has been compiled successfully
      self.locals.items(.initialized)[lvar_pos] = true;
    } else {
      // global
      var info = try self.patchGlobal(node.ident);
      // first, deinitialize the global just in case it was initialized by
      // some other code (since globals are deduplicated/shared)
      if (info.isGSym) {
        self.gsyms.items(.initialized)[info.pos] = false;
      } else {
        self.globals.items(.initialized)[info.gvar.?.index] = false;
      }
      var dst = try self.getReg(node.ident.token);
      _ = try self.cAssign(&bin, dst);
      // now, initialize this global after its rhs has been compiled successfully
      self.initializeGlobal(info);
      self.vreg.releaseReg(dst);
    }
    return reg;
  }

  fn cSubscript(self: *Self, node: *ast.SubscriptNode, dst: u32) !u32 {
    if (node.expr.getType()) |typ| {
      self.optimizeConstRK();
      if (typ.isListTy() or typ.isTupleTy()) {
        // list index
        var rk_val = try self.c(node.expr, dst);
        var reg = try self.getReg(node.expr.getToken());
        var rk_idx = try self.c(node.index, reg);
        var op: OpCode = if (typ.isListTy()) .Glst else .Gtup;
        self.fun.code.write3ArgsInst(op, dst, rk_idx, rk_val, node.line(), self.vm);
        self.vreg.releaseReg(reg);
      } else {
        // map access
        std.debug.assert(typ.isMapTy());
        var rk_val = try self.c(node.expr, dst);
        var reg = try self.getReg(node.expr.getToken());
        var rk_key = try self.c(node.index, reg);
        self.fun.code.write3ArgsInst(.Gmap, dst, rk_key, rk_val, node.line(), self.vm);
        self.vreg.releaseReg(reg);
      }
      self.deoptimizeConstRK();
      return dst;
    }
    unreachable;
  }

  fn cDeref(self: *Self, node: *ast.DerefNode, reg: u32) !u32 {
    var rx = try self.c(node.expr, reg);
    self.fun.code.write3ArgsInst(.Asrt, rx, 0, 0, node.token.line, self.vm);
    return reg;
  }

  fn cBlockNoPops(self: *Self, node: *ast.BlockNode, reg: u32) !u32 {
    self.incScope();
    for (node.nodes.items()) |nd| {
      _ = try self.c(nd, reg);
    }
    self.decScope();
    return reg;
  }

  fn cBlock(self: *Self, node: *ast.BlockNode, reg: u32) !u32 {
    self.incScope();
    for (node.nodes.items()) |nd| {
      _ = try self.c(nd, reg);
    }
    self.decScope();
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

  fn cIf(self: *Self, node: *ast.IfNode, dst: u32) !u32 {
    // self.enterJmp();
    var reg = try self.getReg(node.cond.getToken());
    // if-
    var rx = try self.c(node.cond, reg);
    // jmp to elif, if any
    var cond_to_elif_or_else = self.fun.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = try self.cBlock(&node.then.AstBlock, reg);
    var should_patch_if_then_to_end = true;
    var if_then_to_end = blk: {
      // as a simple optimization, if we only have an if-end statement, i.e. no elif & else,
      // we don't need to jump after the last statement in the if-then block, control
      // would naturally fallthrough to outside the if-end statement.
      if (node.elifs.len() == 0 and node.els.AstBlock.nodes.len() == 0) {
        should_patch_if_then_to_end = false;
        break :blk @as(usize, 0);
      }
      break :blk self.fun.code.write2ArgsJmp(.Jmp, 2, self.lastLine(), self.vm);
    };
    var elifs_then_to_end: ds.ArrayList(usize) = undefined;
    // elifs-
    if (node.elifs.len() > 0) {
      // first, patch cond_to_elif_or_else here
      self.fun.code.patch2ArgsJmp(cond_to_elif_or_else);
      elifs_then_to_end = ds.ArrayList(usize).init(self.alloc());
      var last_patch: ?JmpPatch = null;
      for (node.elifs.items()) |elif| {
        if (last_patch) |pch| {
          // we want the last compiled elif's failing cond to jump here; 
          // - just before the next elif code
          self.fun.code.patch2ArgsJmp(pch.jmp_to_next);
        }
        var patch: JmpPatch = try self.cElif(&elif.AstElif, reg);
        elifs_then_to_end.append(patch.jmp_to_end);
        last_patch = patch;
      }
      // the last elif in `elifs` would not be patched considering we're 
      // patching the first only after we've seen the second, hence, we patch it here
      self.fun.code.patch2ArgsJmp(last_patch.?.jmp_to_next);
    } else {
      self.fun.code.patch2ArgsJmp(cond_to_elif_or_else);
    }
    // else-
    _ = try self.cBlock(&node.els.AstBlock, reg);
    if (node.elifs.len() > 0) {
      // patch up elif_then_to_end 
      for (elifs_then_to_end.items()) |idx| {
        self.fun.code.patch2ArgsJmp(idx);
      }
    }
    if (should_patch_if_then_to_end) {
      self.fun.code.patch2ArgsJmp(if_then_to_end);
    }
    // self.leaveJmp();
    self.vreg.releaseReg(reg);
    return dst;
  }

  fn patchLoopJmps(self: *Self, loop_cond: usize) void {
    for (self.loop_ctrls.items()) |ctrl| {
      if (ctrl.isBreak()) {
        // this is a forward jmp, so patch the jmp offset
        self.fun.code.patch2ArgsJmp(ctrl.patch_index);
      } else {
        // obtain offset to jmp forward to, then rewrite the jmp instruction
        var offset = @intCast(u32, ctrl.patch_index - loop_cond + 1);
        _ = self.fun.code.write2ArgsInst(.Jmp, 0, offset, ctrl.token.line, self.vm);
        self.fun.code.words.items[ctrl.patch_index] = self.fun.code.words.pop();
      }
    }
    // clear for reuse
    self.loop_ctrls.clearRetainingCapacity();
  }

  fn cWhile(self: *Self, node: *ast.WhileNode, dst: u32) !u32 {
    var reg = try self.getReg(node.cond.getToken());
    // cond-
    var to_cond = self.fun.code.getInstLen();
    var rx = try self.c(node.cond, reg);
    // jmp to exit
    var cond_to_exit = self.fun.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = try self.cBlock(&node.then.AstBlock, reg);
    // loop to cond
    // +1 to include the jmp inst itself, which we already processed
    var offset = @intCast(u32, self.fun.code.getInstLen() - to_cond + 1);
    _ = self.fun.code.write2ArgsInst(.Jmp, 0, offset, self.lastLine(), self.vm);
    self.fun.code.patch2ArgsJmp(cond_to_exit);
    self.vreg.releaseReg(reg);
    // patch all loop controls
    self.patchLoopJmps(to_cond);
    return dst;
  }

  fn cControl(self: *Self, node: *ast.ControlNode, dst: u32) u32 {
    if (node.isBreak()) {
      // jmp fwd
      node.patch_index = self.fun.code.write2ArgsJmp(.Jmp, 2, node.token.line, self.vm);
    } else {
      // jmp bck
      node.patch_index = self.fun.code.write2ArgsJmp(.Jmp, 0, node.token.line, self.vm);
    }
    self.loop_ctrls.append(node);
    return dst;
  }

  /// returns jmp_to_next for patching the jmp to the next elif/else/,
  /// and jmp_to_end for patching the jmp to the end of the entire if-stmt
  fn cElif(self: *Self, node: *ast.ElifNode, reg: u32) !JmpPatch {
    var rx = try self.c(node.cond, reg);
    // jmp to else, if any
    var cond_jmp = self.fun.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = try self.cBlock(&node.then.AstBlock, reg);
    var then_jmp = self.fun.code.write2ArgsJmp(.Jmp, 2, self.lastLine(), self.vm);
    return .{.jmp_to_next = cond_jmp, .jmp_to_end = then_jmp};
  }

  fn cFun(self: *Self, node: *ast.FunNode, ast_node: *Node, _reg: u32) !u32 {
    if (node.isGeneric()) return self.cFunGeneric(ast_node, _reg);
    var reg: u32 = undefined;
    var token = ast_node.getToken();
    var new_fn = value.createFn(self.vm, @intCast(u8, node.params.len()));
    var is_global = self.inGlobalScope();
    var is_lambda = node.name == null;
    if (is_global) {
      // global
      if (!is_lambda) {
        var ident = node.name.?;
        var info = try self.patchGlobal(ident);
        self.initializeGlobal(info);
        reg = try self.getReg(token);
        if (!info.isGSym) {
          new_fn.name = value.asString(self.fun.code.values.items[info.pos]);
        } else {
          new_fn.name = @constCast(value.createString(self.vm, &self.vm.strings, ident.token.value, false));
        }
      } else reg = _reg;
    } else if (node.name) |ident| {
      // local (named)
      reg = try self.initLocal(ident);
      new_fn.name = @constCast(value.createString(self.vm, &self.vm.strings, ident.token.value, false));
    } else {
      // local (lambda)
      std.debug.assert(_reg != VRegister.DummyReg);
      reg = _reg;
    }
    var compiler = Compiler.init(self.diag, self.vm, new_fn, self.generics, self.allocator);
    compiler.enclosing = self;
    // localize params
    compiler.incScope();
    for (node.params.items()) |param| {
      _ = try compiler.initLocal(param.ident);
    }
    // compile function body using cBlockNoPops() since no need to pop locals,
    // - return does this automatically
    _ = try compiler.cBlockNoPops(&node.body.AstBlock, _reg);
    // append return inst as last
    if (!node.body.AstBlock.nodes.getLast().isRet()) {
      compiler.fun.code.write2ArgsInst(.Ret, 0, 0, compiler.lastLine(), compiler.vm);
    }
    compiler.decScope();
    new_fn.envlen = compiler.upvalues.len;
    self.optimizeConstBX();
    var tmp = try self.getReg(token);
    var dst = self.cConst(tmp, value.objVal(new_fn), token.line);
    self.fun.code.write2ArgsInst(.Bclo, reg, dst, token.line, self.vm);
    self.vreg.releaseReg(tmp);
    self.deoptimizeConstBX();
    // set upvalues
    var slice = compiler.upvalues.slice();
    var indexs = slice.items(.index);
    var is_locals = slice.items(.is_local);
    for (indexs, is_locals) |idx, loc| {
      // use .Bclo as a placeholder
      self.fun.code.write2ArgsInst(.Bclo, @boolToInt(loc), idx, self.lastLine(), self.vm);
    }
    if (is_global) {
      if (node.name) |ident| {
        var info = self.findGlobal(ident).?;
        const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
        self.fun.code.write2ArgsInst(inst, reg, info.pos, self.lastLine(), self.vm);
      }
    }
    if (util.getMode() == .Debug) {
      std.debug.print("\n", .{});
      Disassembler.disCode(&new_fn.code, new_fn.getName());
    }
    if (is_lambda) return reg;
    if (is_global) self.vreg.releaseReg(reg);
    return _reg;
  }

  fn cFunGeneric(self: *Self, node: *Node, reg: u32) CompileError!u32 {
    if (self.generics.get(node)) |list| {
      for (list.items()) |itm| {
        var fun = &itm.instance.AstFun;
        // only attach names in the local scope since if we're in the global scope,
        // the name has already been attached in preallocateGlobals()
        if (self.inLocalScope()) {
          fun.name = itm.synth_name;
        }
        _ = try self.cFun(fun, itm.instance, reg);
      }
    }
    return reg;
  }

  fn cCall(self: *Self, node: *ast.CallNode, reg: u32) !u32 {
    // For calls, we need to:
    // - get a register window
    // - compile the call expr
    // - generate the instruction: call r(func), b(argc)
    var token = node.expr.getToken();
    var fun_dst = try self.c(node.expr, reg);
    var _dst = reg;
    var n = @intCast(u32, node.args.len());
    var do_move = false;
    var win = blk: {
      break :blk self.vreg.getRegWindow(reg, n) catch {
        do_move = true;
        break :blk self.vreg.getAnyRegWindow(reg, n + 1);
      };
    };
    if (do_move) {_dst = win; win += 1;}
    // we need the function to be in a specific position in the register window, along with its args
    if (fun_dst != _dst) {
      std.debug.assert(self.inLocalScope());
      self.fun.code.write3ArgsInst(.Mov, _dst, fun_dst, 0, token.line, self.vm);
    }
    // don't optimize this call's arguments
    self.enterJmp();
    for (node.args.items(), 0..) |arg, i| {
      var dst = win + @intCast(u32, i);
      var tmp = try self.c(arg, dst);
      // we need the args to be positioned in line with the function in the register window
      if (tmp != dst) {
        std.debug.assert(self.inLocalScope());
        self.fun.code.write3ArgsInst(.Mov, dst, tmp, 0, self.lastLine(), self.vm);
      }
    }
    self.leaveJmp();
    self.fun.code.write2ArgsInst(.Call, _dst, n, token.line, self.vm);
    if (do_move) {
      self.fun.code.write3ArgsInst(.Mov, reg, _dst, 0, token.line, self.vm);
      self.vreg.releaseRegWindow(_dst, n + 1);
    } else {
      self.vreg.releaseRegWindow(win, n);
    }
    return reg;
  }

  fn cRet(self: *Self, node: *ast.RetNode, reg: u32) !u32 {
    _ = reg;
    // TODO: return nil when expr is null?
    var dst: u32 = 0;
    if (node.expr) |expr| {
      var rg = try self.getReg(expr.getToken());
      dst = try self.c(expr, rg);
      self.vreg.releaseReg(rg);
    }
    self.fun.code.write2ArgsInst(.Ret, dst, 0, node.token.line, self.vm);
    return dst;
  }

  fn cProgram(self: *Self, node: *ast.ProgramNode, start: u32) !u32 {
    var reg = start;
    for (node.decls.items()) |nd| {
      reg = try self.c(nd, reg);
    }
    return reg;
  }

  fn c(self: *Self, node: *Node, reg: u32) CompileError!u32 {
    return switch (node.*) {
      .AstNumber => |*nd| self.cNumber(nd, reg),
      .AstString => |*nd| self.cString(nd, reg),
      .AstBool => |*nd| self.cBool(nd, reg),
      .AstUnary => |*nd| try self.cUnary(nd, reg),
      .AstBinary => |*nd| try self.cBinary(nd, reg),
      .AstList => |*nd| try self.cList(nd, node, reg),
      .AstTuple => |*nd| try self.cTuple(nd, node, reg),
      .AstMap => |*nd| try self.cMap(nd, reg),
      .AstExprStmt => |*nd| try self.cExprStmt(nd, reg),
      .AstVar => |*nd| try self.cVar(nd, reg),
      .AstVarDecl => |*nd| try self.cVarDecl(nd, reg),
      .AstAssign => |*nd| try self.cAssign(nd, reg),
      .AstBlock => |*nd| try self.cBlock(nd, reg),
      .AstNType => |*nd| self.cNType(nd, reg),
      .AstAlias => reg, // TODO
      .AstNil => |*nd| self.cNil(nd, reg),
      .AstCast => |*nd| try self.cCast(nd, reg),
      .AstSubscript => |*nd| try self.cSubscript(nd, reg),
      .AstDeref => |*nd| try self.cDeref(nd, reg),
      .AstIf => |*nd| try self.cIf(nd, reg),
      .AstWhile => |*nd| try self.cWhile(nd, reg),
      .AstControl => |*nd| self.cControl(nd, reg),
      .AstFun => |*nd| try self.cFun(nd, node, reg),
      .AstCall => |*nd| try self.cCall(nd, reg),
      .AstRet => |*nd| try self.cRet(nd, reg),
      .AstProgram, .AstSimpleIf, .AstElif, .AstCondition, .AstEmpty => unreachable,
    };
  }

  pub fn compile(self: *Self, node: *Node) !void {
    self.preallocateGlobals(&node.AstProgram.decls);
    var token = node.getToken();
    util.assert(
      (self.cProgram(&node.AstProgram, VRegister.DummyReg) catch VRegister.DummyReg) == VRegister.DummyReg,
      "should be a dummy register"
    );
    self.fun.code.write2ArgsInst(.Ret, 0, 0, self.lastLine(), self.vm);
    self.validateNoUnpatchedGlobals(token) catch {};
    defer self.allocator.deinitArena();
    if (self.diag.hasErrors()) {
      self.diag.display();
      self.fun.code.words.len = 0;
      return error.CompileError;
    }
  }

};
