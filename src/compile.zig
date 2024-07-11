const std = @import("std");
const value = @import("value.zig");
const tir = @import("tir.zig");
const ks = @import("constants.zig");
const parse = @import("parse.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");
const VebAllocator = @import("allocator.zig");
const diagnostics = @import("diagnostics.zig");
const check = @import("check.zig");
const Disassembler = @import("debug.zig").Disassembler;
const OpCode = @import("opcode.zig").OpCode;
const VM = @import("vm.zig").VM;

const Token = tir.Token;
const Code = value.Code;
const Node = tir.Node;
const Inst = value.Inst;
const ObjFn = value.ObjFn;
const TypeKind = parse.TypeKind;
const Diagnostic = diagnostics.Diagnostic;
const GenInfo = check.TypeChecker.GenInfo;
const GenInfoMap = ds.ArrayHashMap(*Node, *ds.ArrayList(GenInfo));
const TypeList = check.TypeList;
const Type = check.Type;
const U8Writer = util.U8Writer;
const Prelude = check.TypeChecker.Prelude;
const InitVar = ks.InitVar;
const assert = check.assert;
const logger = std.log.scoped(.compile);

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
      regs.appendAssumeCapacity(.{.val = @intCast(i), .free = true});
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
    assert(j == n);
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
    assert(j == n);
    return start;
  }

  /// release all registers in this register window, starting from the first
  pub fn releaseRegWindow(self: *Self, reg: u32, n: usize) void {
    if (n == 0) return;
    var j = n;
    var slice = self.regs.slice();
    const vals = slice.items(.val);
    var frees = slice.items(.free);
    var r = reg;
    for (vals, frees, 0..) |val, free, i| {
      if (j == 0) break;
      if (val == r) {
        assert(!free);
        frees[i] = true;
        r += 1;
        j -= 1;
      }
    }
  }

  pub fn releaseReg(self: *Self, reg: u32) void {
    if (reg == DummyReg) return;
    var slice = self.regs.slice();
    const vals = slice.items(.val);
    for (vals, 0..) |val, i| {
      if (val == reg) {
        var frees = slice.items(.free);
        assert(!frees[i]);
        frees[i] = true;
        return;
      }
    }
  }
};

const GlobalVar = struct {
  _name: [*]const u8,
  _name_len: u32,
  mempos: u32,
  index: u32, // index into compiler's `globals`
  initialized: bool = false,
  patched: bool = false,

  pub inline fn name(self: *const GlobalVar) []const u8 {
    return self._name[0..@intCast(self._name_len)];
  }
};

/// like a GlobalVar but with more efficient read/write access
const GSymVar = struct {
  _name: [*]const u8,
  _name_len: u32,
  initialized: bool = false,
  patched: bool = false,

  pub inline fn name(self: *const GSymVar) []const u8 {
    return self._name[0..@intCast(self._name_len)];
  }
};

const LocalVar = struct {
  _name: [*]const u8,
  _name_len: u8,
  reg: u32,
  scope: i32,
  index: u32,
  initialized: bool = false,
  captured: bool = false,

  pub fn init(scope: i32, _name: []const u8, reg: u32, index: u32) @This() {
    return .{.scope = scope, ._name = _name.ptr, ._name_len = @intCast(_name.len), .reg = reg, .index = index};
  }

  pub inline fn name(self: *const LocalVar) []const u8 {
    return self._name[0..@intCast(self._name_len)];
  }
};

const Upvalue = struct {
  index: u32,
  is_local: bool,

  pub fn init(index: u32, is_local: bool) @This() {
    return .{.index = index, .is_local = is_local};
  }
};

pub const Compiler = struct {
  gsyms: ds.ArrayListUnmanaged(GSymVar),
  locals: ds.ArrayListUnmanaged(LocalVar),
  upvalues: ds.ArrayListUnmanaged(Upvalue),
  globals: ds.ArrayListUnmanaged(GlobalVar),
  loop_ctrls: ds.ArrayListUnmanaged(LoopPatch),
  vreg: VRegister,
  rk_bx: RkBxPair = RkBxPair{},
  scope: i32 = GLOBAL_SCOPE, // defaults to global scope
  skip_gsyms: bool = false,
  u8w: U8Writer,
  allocator: *VebAllocator,
  enclosing: ?*Compiler = null,
  fun: *ObjFn,
  vm: *VM,
  diag: *Diagnostic,
  generics: *GenInfoMap,
  prelude: Prelude,
  entry_module: bool = false,
  module_dst: u32 = undefined,
  module_decl_idx: u32 = 0,
  
  const Self = @This();

  const GLOBAL_SCOPE = 0;
  const MAX_LOCALS = VM.MAX_LOCAL_ITEMS;
  const MAX_GSYMS = VM.MAX_GSYM_ITEMS;
  const Module = struct {
    compiled: bool = false,
    /// the module as a callable ObjClosure object
    closure: value.Value = 0,
    /// the actual module - an ObjFn object
    obj: *ObjFn,
    /// the compiler for this module
    compiler: *Compiler,
  };
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
    in_noopt: u32 = 0,
  };
  const LoopPatch = struct{
    ctrl: *tir.ControlNode,
    dummy: u32,
  };
  const CompileError = error{CompileError};
  var modules = ds.StringArrayHashMapUnmanaged(Module).init();

  pub fn init(
    diag: *Diagnostic, vm: *VM, fun: *ObjFn, generics: *GenInfoMap,
    allocator: *VebAllocator, prel: Prelude,
    prev: ?*@This(), mtd_token: ?Token,
  ) Self {
    const al = allocator.getArenaAllocator();
    var self = Self {
      .gsyms = ds.ArrayListUnmanaged(GSymVar).initCapacity(MAX_GSYMS, al),
      .locals = ds.ArrayListUnmanaged(LocalVar).initCapacity(MAX_LOCALS, al),
      .upvalues = ds.ArrayListUnmanaged(Upvalue).init(),
      .globals = ds.ArrayListUnmanaged(GlobalVar).init(),
      .loop_ctrls = ds.ArrayListUnmanaged(LoopPatch).init(),
      .vreg = VRegister.init(al),
      .u8w = U8Writer.init(al),
      .allocator = allocator,
      .fun = fun,
      .vm = vm,
      .diag = diag,
      .generics = generics,
      .prelude = prel,
    };
    if (prev) |p| {
      self.adoptPrelude(p);
    } else {
      self.loadPrelude();
    }
    if (mtd_token) |token| {
      self.appendSelfParam(token) catch unreachable;
    } else {
      _ = self.initLocal(Token.getDefaultToken()) catch unreachable;
    }
    return self;
  }

  fn softError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnosticsWithLevel(.DiagError, token, "CompileError: " ++ fmt ++ "\n", args);
  }

  fn compileError(self: *Self, token: Token, comptime fmt: []const u8, msg: anytype) CompileError {
    self.diag.addDiagnosticsWithLevel(.DiagError, token, "CompileError: " ++ fmt ++ "\n", msg);
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

  inline fn addModule(self: *Self, name: []const u8, module: *ObjFn, compiler: *Self) void {
    modules.set(name, .{.obj = module, .compiler = compiler}, self.alloc());
  }

  inline fn finishModule(self: *Self, name: []const u8, module: value.Value) void {
    _ = self;
    modules.getPtr(name).?.compiled = true;
    modules.getPtr(name).?.closure = module;
  }

  inline fn isSelfVar(self: *Self, node: *tir.TVarNode) bool {
    _ = self;
    return node.token.valueEql(ks.SelfVar);
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
    return self.rk_bx.optimizeRK > 0 and self.rk_bx.in_noopt == 0;
  }

  inline fn canOptimizeConstBX(self: *Self) bool {
    return self.rk_bx.optimizeBX > 0 and self.rk_bx.in_noopt == 0;
  }

  inline fn enterNoOpt(self: *Self) void {
    self.rk_bx.in_noopt += 1;
  }

  inline fn leaveNoOpt(self: *Self) void {
    self.rk_bx.in_noopt -= 1;
  }

  /// return true if within RK limit else false
  fn withinRKLimit(self: *Self, num_operands: usize) bool {
    // check that we don't exceed the 9 bits of rk (+ num_operands for number of operands to be added)
    return ((self.fun.code.values.len + value.MAX_REGISTERS + num_operands) <= value.Code._9Bits);
  }

  /// return true if within BX limit else false
  fn withinBXLimit(self: *Self) bool {
    // check that we don't exceed the 18 bits of bx (+ 1 for the new addition)
    return ((self.fun.code.values.len + value.MAX_REGISTERS + 1) <= value.Code._18Bits);
  }

  fn addLocal(self: *Self, token: Token) !u32 {
    // add a local and return its register.
    if (self.locals.len() >= MAX_LOCALS) {
      return self.compileError(token, "Too many locals", .{});
    }
    const reg = try self.getReg(token);
    self.locals.append(
      LocalVar.init(self.scope, token.lexeme(), reg, @intCast(self.locals.len())),
      self.alloc(),
    );
    return reg;
  }

  fn initLocal(self: *Self, token: Token) !u32 {
    const dst = try self.addLocal(token);
    self.locals.items()[self.locals.len() - 1].initialized = true;
    return dst;
  }

  fn addUpvalue(self: *Self, index: u32, is_local: bool, debug: Token) !u32 {
     if (self.locals.len() >= MAX_LOCALS) {
      return self.compileError(debug, "Too many upvalues", .{});
    }
    for (self.upvalues.items(), 0..) |upv, i| {
      if (index == upv.index and is_local == upv.is_local) {
        return @intCast(i);
      }
    }
    const ret: u32 = @intCast(self.upvalues.len());
    self.upvalues.append(Upvalue.init(index, is_local), self.alloc());
    return ret;
  }

  /// add gsym and return its index/pos
  fn addGSymVar(self: *Self, token: Token) struct {pos: u32, isGSym: bool} {
    // Deduplicate globals. If a spot is already allocated for a global 'a', 
    // a redefinition of 'a' should not allocate a new spot.
    if (self.findGlobal(token)) |info| {
      return .{.pos = info.pos, .isGSym = info.isGSym};
    }
    if (self.skip_gsyms) {
      logger.debug("Gsyms {s}..using globals list", .{if (self.skip_gsyms) "elided" else "exceeded"});
      return .{.pos = self.addGlobalVar(token), .isGSym = false};
    }
    const idx: u32 = @intCast(self.gsyms.len());
    self.gsyms.append(GSymVar{._name = token.val, ._name_len = token.len}, self.alloc());
    return .{.pos = idx, .isGSym = true};
  }

  fn addGlobalVar(self: *Self, token: Token) u32 {
    const gvar = GlobalVar {
      ._name = token.val,
      ._name_len = token.len,
      .mempos = self.storeVar(token),
      .index = @intCast(self.globals.len()),
    };
    self.globals.append(gvar, self.alloc());
    return gvar.mempos;
  }

  inline fn addPatchInitGlobalVar(self: *Self, token: Token) void {
    _ = self.addGSymVar(token);
    const info = self.patchGlobal(token) catch return;
    self.initializeGlobal(info);
  }

  fn findLocalVar(self: *Self, node: *tir.TVarNode) ?LocalVar {
    var i = self.locals.len() -| 1;
    for (0..self.locals.len()) |_| {
      if (self.scope < self.locals.itemAt(i).scope) break;
      if (node.token.valueEql(self.locals.itemAt(i).name())) {
        return self.locals.itemAt(i);
      }
      i -|= 1;
    }
    return null;
  }

  fn findUpvalue(self: *Self, node: *tir.TVarNode) ?u32 {
    if (self.enclosing == null) return null;
    var compiler = self.enclosing.?;
    if (compiler.findLocalVar(node)) |loc| {
      compiler.locals.items()[loc.index].captured = true;
      return self.addUpvalue(loc.reg, true, node.token) catch null;
    }
    if (compiler.findUpvalue(node)) |reg| {
      return self.addUpvalue(reg, false, node.token) catch null;
    }
    return null;
  }

  fn findGSymVar(self: *Self, token: Token) ?u32 {
    var i = self.gsyms.len() -| 1;
    for (0..self.gsyms.len()) |_| {
      if (token.valueEql(self.gsyms.itemAt(i).name())) {
        return @intCast(i);
      }
      i -|= 1;
    }
    return null;
  }

  fn findGlobalVar(self: *Self, token: Token) ?GlobalVar {
    var i = self.globals.len() -| 1;
    for (0..self.globals.len()) |_| {
      if (token.valueEql(self.globals.itemAt(i).name())) {
        return self.globals.itemAt(i);
      }
      i -|= 1;
    }
    return null;
  }

  fn findGlobal(self: *Self, token: Token) ?GlobalVarInfo {
    if (self.findGSymVar(token)) |pos| {
      return .{.pos = pos, .isGSym = true, .gvar = null};
    } else if (self.findGlobalVar(token)) |gvar| {
      return .{.pos = gvar.mempos, .isGSym = false, .gvar = gvar};
    } else if (self.enclosing) |compiler| {
      if (compiler.findGlobal(token)) |info| {
        var ret = info;
        ret.from_enclosing = true;
        return ret;
      }
    }
    return null;
  }

  fn appendSelfParam(self: *Self, token: Token) !void {
    const self_tok = token.tkFrom(ks.SelfVar, token.ty);
    _ = try self.initLocal(self_tok);
  }

  fn popLocalVars(self: *Self) void {
    var close: ?u32 = null;
    for (self.locals.items()) |lcl| {
      if (lcl.scope > self.scope) {
        // only take the first local, close upwards from there
        if (lcl.captured and close == null) {
          close = lcl.reg;
        }
        self.vreg.releaseReg(lcl.reg);
        self.locals.list.items.len -= 1;
      }
    }
    if (close) |reg| {
      self.fun.code.write2ArgsInst(.Cupv, reg, 0, self.lastLine(), self.vm);
    }
  }

  /// load prelude
  fn loadPrelude(self: *Self) void {
    util.assert(self.globals.len() == 0, "globals len should be zero at startup");
    self.skip_gsyms = true;
    self.preallocatePrelude();
    self.skip_gsyms = false;
    // patch and initialize all builtin globals:
    for (0..self.globals.len()) |i| {
      self.globals.items()[i].patched = true;
      self.globals.items()[i].initialized = true;
    }
  }

  /// adopt prelude
  fn adoptPrelude(self: *Self, other: *Self) void {
    const al = self.alloc();
    for (0..other.globals.len()) |i| {
      // store the global refs
      self.globals.append(other.globals.itemAt(i), al);
      // store the actual global values
      _ = self.fun.code.writeValue(other.fun.code.values.items[i], self.vm);
    }
  }

  fn scrampleGenericMethods(self: *Self) void {
    for (self.generics.values()) |list| {
      for (list.items()) |itm| {
        if (itm.instance.isClass()) {
          var cls = &itm.instance.NdClass;
          if (cls.data.builtin) continue;
          if (cls.data.methods.isNotEmpty()) {
            for (cls.data.methods.items()) |mth| {
              if (mth.isGenericMtd()) {
                mth.getBasicFun().data.name.? = mth.getBasicFun().data.name.?.tkFrom("$$", .TkIdent);
              }
            }
          }
        }
      }
    }
  }

  /// preallocate all globals in `gsyms` or `globals`
  fn preallocateGlobals(self: *Self, toplevels: tir.NodeItems) void {
    assert(!self.inLocalScope());
    for (toplevels) |decl| {
      switch (decl.*) {
        .NdVarDecl, .NdConstVarDecl => |*vd| {
          _ = self.addGSymVar(vd.name.toToken());
        },
        .NdPubVarDecl => |*vd| {
          _ = self.addGSymVar(vd.getVarDecl().name.toToken());
        },
        .NdBasicFun => |*fun| {
          if (fun.data.name) |ident| {
            _ = self.addGSymVar(ident);
          }
        },
        .NdGenericFun => {
          if (self.generics.get(decl)) |list| {
            for (list.items()) |itm| {
              itm.instance.NdBasicFun.data.name.? = itm.synth_name;
              _ = self.addGSymVar(itm.synth_name);
            }
          }
        },
        .NdClass => |*cls| {
          if (cls.isParameterized()) {
            if (self.generics.get(decl)) |list| {
              for (list.items()) |itm| {
                itm.instance.NdClass.name = tir.IdentToken.init(itm.synth_name);
                _ = self.addGSymVar(itm.synth_name);
              }
            }
          } else {
            _ = self.addGSymVar(cls.name.toToken());
          }
        },
        .NdImport => |*nd| {
          self.addPatchInitGlobalVar(nd.getImportNameToken());
          if (nd.data.entities) |entities| {
            for (entities) |entity| {
              self.addPatchInitGlobalVar(entity.getName());
            }
          }
        },
        else => {},
      }
    }
  }

  /// preallocate all prelude globals in `gsyms` or `globals`
  fn preallocatePrelude(self: *Self) void {
    assert(!self.inLocalScope());
    for (self.prelude.core.NdProgram.decls) |decl| {
      switch (decl.*) {
        .NdVarDecl, .NdConstVarDecl => |*vd| {
          _ = self.addGSymVar(vd.name.toToken());
        },
        .NdPubVarDecl => |*vd| {
          _ = self.addGSymVar(vd.getVarDecl().name.toToken());
        },
        .NdBasicFun => |*fun| {
          if (fun.data.name) |ident| {
            _ = self.addGSymVar(ident);
          }
        },
        .NdGenericFun => |_| {
          if (self.generics.get(decl)) |list| {
            for (list.items()) |itm| {
              itm.instance.NdBasicFun.data.name.? = itm.synth_name;
              _ = self.addGSymVar(itm.synth_name);
            }
          }
        },
        .NdClass => |*cls| {
          if (cls.isParameterized()) {
            if (self.generics.get(decl)) |list| {
              for (list.items()) |itm| {
                itm.instance.NdClass.name = tir.IdentToken.init(itm.synth_name);
                _ = self.addGSymVar(itm.synth_name);
              }
            }
          } else {
            _ = self.addGSymVar(cls.name.toToken());
          }
        },
        .NdImport => |*nd| {
          self.addPatchInitGlobalVar(nd.getImportNameToken());
          if (nd.data.entities) |entities| {
            for (entities) |entity| {
              _ = self.addPatchInitGlobalVar(entity.getName());
            }
          }
        },
        else => {},
      }
    }
  }

  fn compilePrelude(self: *Self, reg: u32) !void {
    for (self.prelude.core.NdProgram.decls) |decl| {
      switch (decl.*) {
        .NdVarDecl, .NdConstVarDecl => |*vd| {
          _ = try self.cVarDecl(vd, reg);
        },
        .NdPubVarDecl => |*vd| {
          _ = try self.cVarDecl(vd.getVarDecl(), reg);
        },
        .NdBasicFun => |*fun| {
          if (fun.data.builtin) continue;
          _ = try self.cFun(decl, false, reg);
        },
        .NdGenericFun => |*fun| {
          if (fun.fun.getBasicFun().data.builtin) continue;
          _ = try self.cFunGeneric(decl, false, reg);
        },
        .NdClass => |*cls| {
          if (cls.data.builtin) continue;
          _ = try self.cClass(decl, cls, reg);
        },
        else => {},
      }
    }
  }

  /// patch a preallocated global entry
  fn patchGlobal(self: *Self, token: Token) !GlobalVarInfo {
    if (self.findGlobal(token)) |info| {
      if (info.isGSym) {
        // GSyms use `pos` as a direct index into the `gsyms` array
        // so we can reach this GSym directly
        self.gsyms.items()[info.pos].patched = true;
      } else {
        self.globals.items()[info.gvar.?.index].patched = true;
      }
      return info;
    }
    return self.compileError(token, "cannot patch variable '{s}'", .{token.lexeme()});
  }

  fn initializeGlobal(self: *Self, info: GlobalVarInfo) void {
    if (info.isGSym) {
      self.gsyms.items()[info.pos].initialized = true;
    } else {
      self.globals.items()[info.gvar.?.index].initialized = true;
    }
  }

  fn validateNoUnpatchedGlobals(self: *Self, token: Token) !void {
    const fmt = "Use of unpached global '{s}'";
    {
      for (self.gsyms.items()) |gsym| {
        if (!gsym.patched) {
          return self.compileError(token, fmt, .{gsym.name()});
        }
      }
    }
    {
      for (self.globals.items()) |gsym| {
        if (!gsym.patched) {
          return self.compileError(token, fmt, .{gsym.name()});
        }
      }
    }
  }

  fn validateLocalVarUse(self: *Self, slot: u32, node: *tir.TVarNode) !void {
    if (!self.locals.items()[slot].initialized) {
      return self.compileError(node.token, "cannot use variable '{s}' in its own initializer", .{node.token.lexeme()});
    }
  }

  fn validateGlobalVarUse(self: *Self, info: GlobalVarInfo, token: Token) !void {
    if (info.from_enclosing) {
      // this global var was obtained from an enclosing compiler's
      // global state, so it IS already validated. Just return.
      return;
    }
    if (info.isGSym) {
      if (self.gsyms.items()[info.pos].patched and !self.gsyms.items()[info.pos].initialized) {
        return self.compileError(token, "cannot use variable '{s}' in its own initializer", .{token.lexeme()});
      }
    } else {
      const gvar = info.gvar.?;
      if (gvar.patched and !gvar.initialized) {
        return self.compileError(token, "cannot use variable '{s}' in its own initializer", .{token.lexeme()});
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
      self.fun.code.write2ArgsInst(.Load, reg, memidx, @intCast(line), self.vm);
      return reg;
    }
  }

  fn cConstNoOpt(self: *Self, reg: u32, val: value.Value, line: usize) u32 {
    // load rx, memidx
    const memidx = self.fun.code.writeValue(val, self.vm);
    self.fun.code.write2ArgsInst(.Load, reg, memidx, @intCast(line), self.vm);
    return reg;
  }

  fn storeVar(self: *Self, token: Token) u32 {
    const val = value.objVal(value.createString(self.vm, &self.vm.strings, token.lexeme(), false));
    return self.fun.code.writeValue(val, self.vm);
  }

  fn cNumber(self: *Self, node: *tir.NumberNode, dst: u32) u32 {
    // load rx, memidx
    return self.cConst(dst, value.numberVal(node.value), node.token.line);
  }

  fn cBool(self: *Self, node: *tir.SymNode, dst: u32) u32 {
    // load rx, memidx
    return self.cConst(dst, value.boolVal(node.token.is(.TkTrue)), node.token.line);
  }

  fn cEscString(self: *Self, node: *tir.SymNode, dst: u32) u32 {
    const token = node.token;
    const val = token.lexeme();
    const escapes = std.mem.count(u8, val, "\\");
    var buf = util.allocSlice(u8, token.len - escapes, self.allocator.getAllocator());
    var i: u32 = 0;
    var idx: u32 = 0;
    while (idx < token.len) : (idx += 1) {
      const char = val[idx];
      if (char == '\\') {
        const next = val[idx + 1];
        switch (next) {
          'a' => buf[i] = 7,    // '\a'
          'b' => buf[i] = 8,    // '\b'
          'f' => buf[i] = 12,   // '\f'
          'n' => buf[i] = '\n', // '\n'
          'r' => buf[i] = '\r', // '\r'
          't' => buf[i] = '\t', // '\a'
          'v' => buf[i] = 11,   // '\v'
          else => |ch| buf[i] = ch,
        }
        idx += 1;
      } else {
        buf[i] = char;
      }
      i += 1;
    }
    return self.cConst(
      dst,
      value.objVal(value.createString(self.vm, &self.vm.strings, buf[0..i], true)),
      node.token.line,
    );
  }

  fn cString(self: *Self, node: *tir.SymNode, dst: u32) u32 {
    if (node.token.is(.TkString)) {
      return self.cConst(
        dst,
        value.objVal(value.createString(self.vm, &self.vm.strings, node.token.lexeme(), false)),
        node.token.line,
      );
    } else {
      return self.cEscString(node, dst);
    }
  }

  fn cUnary(self: *Self, node: *tir.UnaryNode, reg: u32) CompileError!u32 {
    if (node.op.optype() == .OpAdd) {
      return self.c(node.expr, reg);
    }
    self.optimizeConstBX();
    const inst_op = node.op.optype().toInstOp();
    const rk = try self.c(node.expr, reg);
    self.fun.code.write2ArgsInst(inst_op, reg, rk, node.op.line, self.vm);
    self.deoptimizeConstBX();
    return reg;
  }

  inline fn cLgc(self: *Self, node: *tir.BinaryNode, reg: u32) CompileError!u32 {
    // and, or
    // for and/or we do not want any const optimizations as we need consts to be loaded to registers
    // since const optimizations can cause no instructions to be emitted for consts. For ex: `4 or 5`
    // turn off const optimizations
    self.enterNoOpt();
    var rx = try self.c(node.left, reg);
    const end_jmp = self.fun.code.write2ArgsJmp(node.optype().toInstOp(), rx, node.line(), self.vm);
    rx = try self.c(node.right, reg);
    self.fun.code.patch2ArgsJmp(end_jmp);
    // restore const optimizations
    self.leaveNoOpt();
    return reg;
  }

  fn cBinary(self: *Self, node: *tir.BinaryNode, dst: u32) CompileError!u32 {
    // handle and | or
    if (node.optype().isLgcOp()) return try self.cLgc(node, dst);
    if (node.optype() == .OpIs) return try self.cIs(node, dst);
    self.optimizeConstRK();
    const rk1 = try self.c(node.left, dst);
    const dst2 = try self.getReg(node.left.getToken());
    const rk2 = try self.c(node.right, dst2);
    // we own this register, so we can free
    self.vreg.releaseReg(dst2);
    const inst_op = node.optype().toInstOp();
    self.fun.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(node.line()), self.vm);
    self.deoptimizeConstRK();
    return dst;
  }

  fn cIs(self: *Self, node: *tir.BinaryNode, dst: u32) !u32 {
    self.optimizeConstRK();
    var op = OpCode.Is;
    const rk1 = try self.c(node.left, dst);
    const rk2 = tb: {
      var typ = node.right.getType().?;
      typ = if (typ.isTop()) typ.top().child else typ;
      if (typ.isClass()) {
        break :tb (
          if (typ.isListTy()) TypeKind.TyClass
          else if (typ.isMapTy()) TypeKind.TyClass + 1
          else if (typ.isTupleTy()) TypeKind.TyClass + 2
          else if (typ.klass().isStringClass()) @intFromEnum(TypeKind.TyString)
          else blk: {
            op = OpCode.Iscls;
            const cls = typ.klass();
            const reg = try self.getReg(node.right.getToken());
            var name = tir.TVarNode.init(cls.getName());
            const rk2 = try self.cVar(&name, reg);
            self.vreg.releaseReg(reg);
            break :blk rk2;
          }
        );
      } else if (typ.isNoneTy() or (typ.isTagOrClass() and typ.toc().tktype.is(.TkNone))) {
        break :tb @intFromEnum(tir.TypeKind.TyNil);
      } else if (typ.isErrorTy()) {
        break :tb TypeKind.TyClass + 3;
      } else if (typ.isTag()) {
        // get name
        op = OpCode.Istag;
        const token = node.right.getToken();
        const name = value.createString(self.vm, &self.vm.strings, typ.tag().name, false);
        const dst2 =  try self.getReg(node.left.getToken());
        const rk2 = self.cConst(dst2, value.objVal(name), token.line);
        self.vreg.releaseReg(dst2);
        break :tb rk2;
      } else if (typ.isTagOrClass()) {
        op = OpCode.Istoc;
        const token = node.right.getToken();
        const dst2 =  try self.getReg(node.left.getToken());
        const name = value.createString(self.vm, &self.vm.strings, typ.toc().name, false);
        const rk2 = self.cConst(dst2, value.objVal(name), token.line);
        self.vreg.releaseReg(dst2);
        break :tb rk2;
      } else {
        break :tb @intFromEnum(typ.concrete().kind);
      }
    };
    self.fun.code.write3ArgsInst(op, dst, rk1, rk2, @intCast(node.line()), self.vm);
    self.deoptimizeConstRK();
    return dst;
  }

  inline fn cCollection(self: *Self, node: *tir.ListNode, ast_node: *Node, dst: u32, op: OpCode, set: OpCode) !u32 {
    const size: u32 = @intCast(node.elems.len);
    self.fun.code.write2ArgsInst(op, dst, size, self.lastLine(), self.vm);
    const token = ast_node.getToken();
    var idx: u32 = undefined;
    for (node.elems, 0..) |elem, i| {
      const reg = try self.getReg(token);
      const rk_val = try self.c(elem, reg);
      const val = value.numberVal(@floatFromInt(i));
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

  fn cList(self: *Self, node: *tir.ListNode, ast_node: *Node, dst: u32) !u32 {
    return try self.cCollection(node, ast_node, dst, .Nlst, .Slst);
  }

  fn cTuple(self: *Self, node: *tir.ListNode, ast_node: *Node, dst: u32) !u32 {
    return try self.cCollection(node, ast_node, dst, .Ntup, .Stup);
  }

  fn cMap(self: *Self, node: *tir.MapNode, dst: u32) !u32 {
    const size: u32 = @intCast(node.pairs.len);
    // TODO: specialize map with k-v types
    self.fun.code.write2ArgsInst(.Nmap, dst, size, self.lastLine(), self.vm);
    const token = Token.getDefaultToken();
    for (node.pairs) |pair| {
      const reg1 = try self.getReg(token);
      const reg2 = try self.getReg(token);
      const rk_key = try self.c(pair.key, reg1);
      const rk_val = try self.c(pair.value, reg2);
      self.fun.code.write3ArgsInst(.Smap, dst, rk_key, rk_val, self.lastLine(), self.vm);
      self.vreg.releaseReg(reg1);
      self.vreg.releaseReg(reg2);
    }
    return dst;
  }

  fn cExprStmt(self: *Self, node: *tir.ExprStmtNode, reg: u32) !u32 {
    // sequence point
    const dst = try self.getReg(node.expr.getToken());
    _ = try self.c(node.expr, dst);
    self.vreg.releaseReg(dst);
    return reg;
  }

  fn cVar(self: *Self, node: *tir.TVarNode, dst: u32) !u32 {
    // Sometimes, the lookup of generic and non-generic function calls may collide
    // based on how such calls were made.
    // To handle such an edge case, we *first* check if this node is a function type.
    // Since the type checker guarantees that the node embedded in the function type is the
    // function being called, we can safely use the function's name for lookup
    // which if generic, should already be synthesized.
    if (node.typ) |typ| {
      if (typ.isFunction()) {
        var fun = typ.function();
        if (fun.isParameterized()) {
          self.softError(node.token, "I cannot generate code for an uninstantiated generic type.", .{});
          return dst;
        } else if (fun.data.node != null and fun.data.node.?.NdBasicFun.data.name != null) {
          const last = self.diag.count();
          var id = tir.TVarNode.init(fun.getName());
          const reg = self.cVar(&id, dst) catch undefined;
          // we use this indirection because for some reason zig generates bad
          // code for the above `catch` when used for handling this error
          if (self.diag.count() == last) return reg;
          // clear last error generated
          self.diag.popUntil(last);
        }
      }
    }
    if (self.findLocalVar(node)) |lvar| {
      try self.validateLocalVarUse(lvar.index, node);
      return lvar.reg;
    } else if (self.findUpvalue(node)) |upv| {
      self.fun.code.write2ArgsInst(.Gupv, dst, upv, node.token.line, self.vm);
      return dst;
    } else if (self.findGlobal(node.token)) |info| {
      try self.validateGlobalVarUse(info, node.token);
      const inst: OpCode = if(info.isGSym) .Ggsym else .Gglb;
      self.fun.code.write2ArgsInst(inst, dst, info.pos, node.token.line, self.vm);
      return dst;
    } else if (node.typ) |typ| {
      if (typ.isClass() and !self.isSelfVar(node) and !typ.klass().builtin and typ.inferred) {
        var cls = typ.klass();
        if (cls.isParameterized()) {
          if (!cls.isInstantiatedGeneric()) {
            self.softError(node.token, "cannot generate code for an uninstantiated generic type", .{});
            return dst;
          } else if (cls.data.node) |nd| {
            // try to capture the monomorphized class type, we only do this for looking up the var
            // that actually defines the class's type and not a var annotated with a class type.
            const last = self.diag.count();
            var id = tir.TVarNode.init(nd.NdClass.name.toToken());
            const reg = self.cVar(&id, dst) catch undefined;
            if (self.diag.count() == last) return reg;
            self.diag.popUntil(last);
          }
        }
      }
    }
    if (node.typ) |ty| {
      if (ty.isTag()) {
        return self.cTagVar(node, ty, dst);
      }
    }
    return self.compileError(node.token, "use of undefined variable '{s}'", .{node.token.lexeme()});
  }

  fn cDotAccess(self: *Self, node: *tir.DotAccessNode, is_call: bool, dst: u32) !u32 {
    // is_call is an optimization hint that helps generate an inst that combines Gmtd + Call
    self.optimizeConstRK();
    defer self.deoptimizeConstRK();
    var tmp = node.lhs.getType().?;
    if (tmp.isTrait() or tmp.isUnion()) {
      return self.cTraitDotAccess(node, is_call, dst);
    }
    // qualified tag access using tag union, e.g T.Bar
    if (tmp.isTaggedUnion() or (tmp == node.typ.?)) {
      // the second condition holds if this is a fully qualified tag access like: type T = K ;; T.K
      return self.cTagVar(&node.rhs.NdTVar, node.typ.?, dst);
    }
    const rk_inst = try self.c(node.lhs, dst);
    // tag field access
    if (tmp.isTag()) {
      const prop_idx: u32 =
        if (node.rhs.isNumberLiteral()) @intFromFloat(node.rhs.NdNumber.value)
        else if (tmp.tag().getFieldWithId(node.rhs.NdTVar.value())) |pos| @intCast(pos)
        else unreachable; 
      self.fun.code.write3ArgsInst(.Gsfd, dst, rk_inst, prop_idx, node.lhs.getToken().line, self.vm);
      return dst;
    }
    const prop = node.rhs.getToken(); // should be a VarNode's token
    var prop_idx: u32 = 0;
    var op: OpCode = undefined;
    if (tmp.isClass() or tmp.isInstance()) {
      var ty = tmp.classOrInstanceClass().klass();
      if (ty.getFieldIndex(prop.lexeme())) |idx| {
        prop_idx = @intCast(idx);
        op = .Gfd;
      } else if (ty.getMethodIndex(prop.lexeme())) |idx| {
        prop_idx = @intCast(idx);
        op = if (is_call) .Jnextc else .Gmtd;
      } else {
        return self.compileError(prop, "No such field/method '{s}'", .{prop.lexeme()});
      }
      self.fun.code.write3ArgsInst(op, dst, rk_inst, prop_idx, prop.line, self.vm);
    } else if (tmp.isModule()) {
      const name = tmp.module().name();
      const m_info = modules.get(name).?;
      if (m_info.compiler.findGlobal(prop)) |info| {
        assert(info.isGSym);
        self.fun.code.write3ArgsInst(.Gmsym, dst, rk_inst, info.pos, prop.line, self.vm);
        return dst;
      } else if (node.typ) |ty| {
        if (ty.isFunction() or ty.isClass()) {
          const _prop = if (ty.isFunction()) ty.function().getName() else ty.klass().getName();
          if (m_info.compiler.findGlobal(_prop)) |info| {
            assert(info.isGSym);
            self.fun.code.write3ArgsInst(.Gmsym, dst, rk_inst, info.pos, prop.line, self.vm);
            return dst;
          }
        } else if (ty.isTag()) {
          return self.cTagVar(&node.rhs.NdTVar, ty, dst);
        }
      }
      return self.compileError(prop, "No such property '{s}'", .{prop.lexeme()});
    }
    return dst;
  }

  fn cTraitDotAccess(self: *Self, node: *tir.DotAccessNode, is_call: bool, dst: u32) !u32 {
    const prop = node.rhs.getToken(); // should be a VarNode's token
    const rk_inst = try self.c(node.lhs, dst);
    const prop_idx = self.storeVar(prop);
    self.fun.code.write3ArgsInst(if (is_call) .Jnextcs else .Gmtds, dst, rk_inst, prop_idx, prop.line, self.vm);
    return dst;
  }

  fn cVarAssign(self: *Self, node: *tir.TVarNode, expr: *Node, reg: u32) !u32 {
    if (self.findLocalVar(node)) |lvar| {
      // use lvar's reg as dst for expr
      const ret = try self.c(expr, lvar.reg);
      if (ret != lvar.reg) {
        // this implies the result of expr was stored in a different register.
        // Move it to lvar's register.
        self.fun.code.write3ArgsInst(.Mov, lvar.reg, ret, 0, node.token.line, self.vm);
      }
      return lvar.reg;
    } else if (self.findUpvalue(node)) |upv| {
      const rx = try self.c(expr, reg);
      self.fun.code.write2ArgsInst(.Supv, rx, upv, node.token.line, self.vm);
      return reg;
    } else if (self.findGlobal(node.token)) |info| {
      // sgsym rx, bx -> GS[bx] = r(x)
      // sglb rx, bx -> G[K(bx)] = r(x)
      const rx = try self.c(expr, reg);
      const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
      self.fun.code.write2ArgsInst(inst, rx, info.pos, node.token.line, self.vm);
      return rx;
    } else {
      return self.compileError(node.token, "undefined variable '{s}'", .{node.token.lexeme()});
    }
  }

  fn cSubscriptAssign(self: *Self, node: *tir.SubscriptNode, rhs: *Node, dst: u32) !u32 {
    self.optimizeConstRK();
    defer self.deoptimizeConstRK();
    // lhs-expr
    const rx = try self.c(node.expr, dst);
    // index
    const idx_reg = try self.getReg(node.expr.getToken());
    const rk_idx = try self.c(node.index, idx_reg);
    // rhs-expr
    const token = node.index.getToken();
    const val_reg = try self.getReg(token);
    const rk_val = try self.c(rhs, val_reg);
    const op: OpCode = if (node.expr.getType().?.isListTy()) .Slst else .Smap;
    self.fun.code.write3ArgsInst(op, rx, rk_idx, rk_val, token.line, self.vm);
    self.vreg.releaseReg(idx_reg);
    self.vreg.releaseReg(val_reg);
    return dst;
  }

  fn cDotAccessAssign(self: *Self, node: *tir.DotAccessNode, rhs: *Node, dst: u32) !u32 {
    self.optimizeConstRK();
    defer self.deoptimizeConstRK();
    // lhs-expr
    const rx = try self.c(node.lhs, dst);
    // rhs-expr
    const val_reg = try self.getReg(node.rhs.getToken());
    const rk_val = try self.c(rhs, val_reg);
    var typ = node.lhs.getType().?;
    const token = node.lhs.getToken();
    const prop = node.rhs.getToken();
    if (typ.isInstance() or typ.isClass()) {
      var ty = typ.classOrInstanceClass().klass();
      var prop_idx: u32 = 0;
      if (ty.getFieldIndex(prop.lexeme())) |idx| {
        prop_idx = @intCast(idx);
      } else {
        return self.compileError(prop, "No such field '{s}'", .{prop.lexeme()});
      }
      self.fun.code.write3ArgsInst(.Sfd, rx, prop_idx, rk_val, prop.line, self.vm);
    } else if (typ.isModule()) {
      // smsym rx(mod), idx, rk(value)
      const name = typ.module().name();
      var m_info = modules.get(name).?;
      if (m_info.compiler.findGlobal(prop)) |info| {
        assert(info.isGSym);
        self.fun.code.write3ArgsInst(.Smsym, rx, info.pos, rk_val, prop.line, self.vm);
        return dst;
      } else {
        return self.compileError(prop, "No such property '{s}'", .{prop.lexeme()});
      }
    } else if (typ.isJustTy()) {
      // TODO: is mutation of tag's content safe?
      self.fun.code.write3ArgsInst(.Ssfd, rx, 0, rk_val, token.line, self.vm);
    } else {
      return self.compileError(token, "unexpected type: '{s}'", .{typ.typename(&self.u8w)});
    }
    self.vreg.releaseReg(val_reg);
    return dst;
  }

  fn cAssign(self: *Self, node: *tir.BinaryNode, reg: u32) !u32 {
    // desugaring can make a noreturn type assigned to a var. for ex $p = panic(..)
    // don't compile the lhs of an assignment that falls into this scenario.
    if (node.right.getType()) |ty| {
      if (ty.isNoreturnTy()) {
        return self.c(node.right, reg);
      }
    }
    // desugaring may assign an Empty node to a var in a vardecl to reserve the slot,
    // if that is the case, just skip the assignment
    if (node.right.isEmpty()) {
      return reg;
    }
    return switch (node.left.*) {
      .NdTVar => |*vr| {
        return try self.cVarAssign(vr, node.right, reg);
      },
      .NdSubscript => |*sub| {
        return try self.cSubscriptAssign(sub, node.right, reg);
      },
      .NdDotAccess => |*da| {
        return try self.cDotAccessAssign(da, node.right, reg);
      },
      // would be unreachable after type checking
      else => return self.compileError(node.left.getToken(), "invalid assignment target", .{}),
    };
  }

  fn cCast(self: *Self, node: *tir.CastNode, dst: u32) !u32 {
    const ty = node.expr.getType();
    // generate code for bool cast, if the expression is not already a bool type
    if (ty != null and !ty.?.isBoolTy()) {
      if (node.typn.typ.isSimple() and node.typn.typ.isBoolTy()) {
        self.optimizeConstRK();
        const rk = try self.c(node.expr, dst);
        self.fun.code.write2ArgsInst(.Bcst, dst, rk, self.lastLine(), self.vm);
        self.deoptimizeConstRK();
        return dst;
      }
    }
    return try self.c(node.expr, dst);
  }

  fn cVarDecl(self: *Self, node: *tir.VarDeclNode, reg: u32) !u32 {
    // let var = expr
    const token = node.name.toToken();
    var bin: tir.BinaryNode = .{
      .left = @constCast(&@as(Node, .{.NdTVar = tir.TVarNode.init(token)})),
      .right = node.value,
      .op_origin = undefined,
      .op_tkty = undefined,
      .op_offset = undefined,
      .typ = node.typ
    };
    if (self.inLocalScope()) {
      // local
      const dst = try self.addLocal(token);
      const lvar_pos = self.locals.len() - 1;
      _ = try self.cAssign(&bin, dst);
      // initialize the local after its rhs has been compiled successfully
      self.locals.items()[lvar_pos].initialized = true;
    } else {
      // global
      const info = try self.patchGlobal(token);
      // first, deinitialize the global just in case it was initialized by
      // some other code (since globals are deduplicated/shared)
      if (info.isGSym) {
        self.gsyms.items()[info.pos].initialized = false;
      } else {
        self.globals.items()[info.gvar.?.index].initialized = false;
      }
      const dst = try self.getReg(token);
      _ = try self.cAssign(&bin, dst);
      // now, initialize this global after its rhs has been compiled successfully
      self.initializeGlobal(info);
      self.vreg.releaseReg(dst);
    }
    return reg;
  }

  fn cSubscript(self: *Self, node: *tir.SubscriptNode, dst: u32) !u32 {
    if (node.expr.getType()) |typ| {
      self.optimizeConstRK();
      defer self.deoptimizeConstRK();
      if (typ.isListTy() or typ.isTupleTy()) {
        // list index
        const rk_val = try self.c(node.expr, dst);
        const reg = try self.getReg(node.expr.getToken());
        const rk_idx = try self.c(node.index, reg);
        const op: OpCode = if (typ.isListTy()) .Glst else .Gtup;
        self.fun.code.write3ArgsInst(op, dst, rk_idx, rk_val, self.lastLine(), self.vm);
        self.vreg.releaseReg(reg);
      } else {
        // map access
        assert(typ.isMapTy());
        const rk_val = try self.c(node.expr, dst);
        const reg = try self.getReg(node.expr.getToken());
        const rk_key = try self.c(node.index, reg);
        self.fun.code.write3ArgsInst(.Gmap, dst, rk_key, rk_val, self.lastLine(), self.vm);
        self.vreg.releaseReg(reg);
      }
      return dst;
    }
    unreachable;
  }

  fn cDeref(self: *Self, node: *tir.DerefNode, reg: u32) !u32 {
    const rx = try self.c(node.expr, reg);
    self.fun.code.write3ArgsInst(.Asrt, rx, 0, 0, node.tkbit.line, self.vm);
    return rx;
  }

  fn cBlockNoPops(self: *Self, node: *tir.BlockNode, reg: u32) !u32 {
    self.incScope();
    for (node.nodes) |nd| {
      _ = try self.c(nd, reg);
    }
    self.decScope();
    return reg;
  }

  fn cBlock(self: *Self, node: *tir.BlockNode, reg: u32) !u32 {
    self.incScope();
    for (node.nodes) |nd| {
      _ = try self.c(nd, reg);
    }
    self.decScope();
    self.popLocalVars();
    return reg;
  }

  fn cNType(self: *Self, node: *tir.TypeNode, reg: u32) u32 {
    // only compile if not in annotation/alias context
    if (!node.from_alias_or_annotation) {
      // TODO: should we be using the typeid() instead of a singular value 
      // (@enumToInt(TypeKind.TyType)) each time?
      // this makes sense for now, since having num == str be true is kinda counterintuitive
      // but again, no one should even be doing this in the first place! 
      return self.cConst(
        reg, value.numberVal(@floatFromInt(node.typ.typeid())),
        node.tkbit.line
      );
    }
    return reg;
  }

  const JmpPatch = struct{jmp_to_next: usize, jmp_to_end: usize};

  fn cSimpleIf(self: *Self, node: *tir.SimpleIfNode, dst: u32) !u32 {
    const reg = try self.getReg(node.cond.getToken());
    // if-
    const rx = try self.c(node.cond, reg);
    // jmp to elif, if any
    const cond_to_else = self.fun.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = try self.c(node.then, reg);
    var should_patch_if_then_to_end = true;
    const if_then_to_end = blk: {
      // as a simple optimization, if we only have an if-end statement, i.e. no elif & else,
      // we don't need to jump after the last statement in the if-then block, control
      // would naturally fallthrough to outside of the if-end statement.
      if (node.els.isBlock() and node.els.block().canAssumeEmpty(Node.isEmpty)) {
        should_patch_if_then_to_end = false;
        break :blk @as(usize, 0);
      }
      break :blk self.fun.code.write2ArgsSJmp(.Jmp, 2, false, self.lastLine(), self.vm);
    };
    self.fun.code.patch2ArgsJmp(cond_to_else);
    // else-
    _ = try self.c(node.els, reg);
    if (should_patch_if_then_to_end) {
      self.fun.code.patch2ArgsSJmp(if_then_to_end);
    }
    self.vreg.releaseReg(reg);
    return dst;
  }

  fn patchLoopJmps(self: *Self, loop_cond: usize) void {
    for (self.loop_ctrls.items()) |_ctrl| {
      const ctrl = _ctrl.ctrl;
      assert(self.fun.code.words.items[ctrl.patch_index] == _ctrl.dummy);
      if (ctrl.isBreak()) {
        // this is a forward jmp, so patch the jmp offset
        self.fun.code.patch2ArgsSJmp(ctrl.patch_index);
      } else {
        // obtain offset to jmp forward to, then rewrite the jmp instruction
        const offset: i32 = @intCast(ctrl.patch_index - loop_cond + 1);
        _ = self.fun.code.write2ArgsSignedInst(.Jmp, 0, -offset, ctrl.token.line, self.vm);
        self.fun.code.words.items[ctrl.patch_index] = self.fun.code.words.pop();
      }
    }
    // clear for reuse
    self.loop_ctrls.clearRetainingCapacity();
  }

  fn cWhile(self: *Self, node: *tir.WhileNode, dst: u32) !u32 {
    const reg = try self.getReg(node.cond.getToken());
    // cond-
    const to_cond = self.fun.code.getInstLen();
    const rx = try self.c(node.cond, reg);
    // jmp to exit
    const cond_to_exit = self.fun.code.write2ArgsJmp(.Jf, rx, self.lastLine(), self.vm);
    _ = try self.cBlock(node.then.block(), reg);
    // loop to cond
    // +1 to include the jmp inst itself, which we already processed
    const offset: i32 = @intCast(self.fun.code.getInstLen() - to_cond + 1);
    _ = self.fun.code.write2ArgsSignedInst(.Jmp, 0, -offset, self.lastLine(), self.vm);
    self.fun.code.patch2ArgsJmp(cond_to_exit);
    self.vreg.releaseReg(reg);
    // patch all loop controls
    self.patchLoopJmps(to_cond);
    return dst;
  }

  fn cControl(self: *Self, node: *tir.ControlNode, dst: u32) u32 {
    if (node.isBreak()) {
      // jmp fwd
      node.patch_index = self.fun.code.write2ArgsSJmp(.Jmp, 2, false, node.token.line, self.vm);
    } else {
      // jmp bck
      node.patch_index = self.fun.code.write2ArgsSJmp(.Jmp, 0, true, node.token.line, self.vm);
    }
    const dummy = self.fun.code.words.items[node.patch_index];
    self.loop_ctrls.append(.{.ctrl = node, .dummy = dummy}, self.alloc());
    return dst;
  }

  fn cFun(self: *Self, ast_node: *Node, is_method: bool, _reg: u32) !u32 {
    // is_method tells us if we're currently trying to compile a class's method
    if (ast_node.isGenericFun()) return self.cFunGeneric(ast_node, is_method, _reg);
    var node = &ast_node.NdBasicFun;
    if (!node.data.body.block().checked) {
      if (self.inGlobalScope() and node.data.name != null) {
        _ = try self.patchGlobal(node.data.name.?);
      }
      logger.debug("skipping compilation of unchecked function: {}", .{node});
      return _reg;
    }
    var reg: u32 = undefined;
    const token = ast_node.getToken();
    var new_fn = value.createFn(self.vm, @intCast(node.params.len));
    new_fn.module = self.fun.module;
    const is_global = self.inGlobalScope();
    const is_lambda = node.data.name == null;
    if (is_lambda) new_fn.name = value.createString(self.vm, &self.vm.strings, ks.LambdaVar, false);
    if (is_global) {
      // global
      if (!is_lambda) {
        const ident = node.data.name.?;
        const info = try self.patchGlobal(ident);
        self.initializeGlobal(info);
        reg = try self.getReg(token);
        if (!info.isGSym) {
          new_fn.name = value.asString(self.fun.code.values.items[info.pos]);
        } else {
          new_fn.name = value.createString(self.vm, &self.vm.strings, classname(ident), false);
        }
      } else reg = _reg;
    } else if (node.data.name) |ident| {
      // local (named)
      reg = try self.initLocal(ident);
      new_fn.name = value.createString(self.vm, &self.vm.strings, classname(ident), false);
    } else {
      // local (lambda)
      assert(_reg != VRegister.DummyReg);
      reg = _reg;
    }
    var compiler = Compiler.init(
      self.diag, self.vm, new_fn, self.generics,
      self.allocator, self.prelude, self,
      if (is_method) token else null,
    );
    compiler.enclosing = self;
    // localize params
    compiler.incScope();
    for (node.params) |param| {
      _ = try compiler.initLocal(param.name);
    }
    // compile function body using cBlockNoPops() since no need to pop locals,
    // - return does this automatically
    _ = try compiler.cBlockNoPops(node.data.body.block(), _reg);
    // append return inst as last
    if (is_method and  node.data.name.?.valueEql(InitVar)) {
      // return `self`
      compiler.fun.code.write2ArgsInst(.Ret, 0, 0, token.line, self.vm);
    } else if (node.data.body.block().nodes.len == 0 or !node.data.body.block().getLast().isRet()) {
      _ = compiler.cVoidRet(compiler.lastLine());
    }
    compiler.decScope();
    new_fn.envlen = compiler.upvalues.len();
    self.optimizeConstBX();
    const tmp = try self.getReg(token);
    const dst = self.cConst(tmp, value.objVal(new_fn), token.line);
    self.fun.code.write2ArgsInst(.Bclo, reg, dst, token.line, self.vm);
    self.vreg.releaseReg(tmp);
    self.deoptimizeConstBX();
    // set upvalues
    for (compiler.upvalues.items()) |upv| {
      // use .Bclo as a placeholder
      self.fun.code.write2ArgsInst(.Bclo, @intFromBool(upv.is_local), upv.index, self.lastLine(), self.vm);
    }
    if (is_global) {
      if (node.data.name) |ident| {
        const info = self.findGlobal(ident).?;
        const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
        self.fun.code.write2ArgsInst(inst, reg, info.pos, self.lastLine(), self.vm);
      }
    }
    if (comptime util.inDebugMode()) {
      std.debug.print("\n", .{});
      Disassembler.disCode(&new_fn.code, new_fn.getName());
    }
    if (is_lambda) return reg;
    if (is_global) self.vreg.releaseReg(reg);
    // we return this reg, even though it's been released because
    // method compilation will use this before anything else does
    if (is_method) return reg;
    return _reg;
  }

  fn cFunGeneric(self: *Self, node: *Node, is_method: bool, reg: u32) CompileError!u32 {
    if (self.generics.get(node)) |list| {
      for (list.items()) |itm| {
        var fun = &itm.instance.NdBasicFun;
        if (fun.data.builtin) continue;
        // only attach names in the local scope since if we're in the global scope,
        // the name has already been attached in preallocateGlobals()
        if (self.inLocalScope()) {
          fun.data.name = itm.synth_name;
        }
        _ = try self.cFun(itm.instance, is_method, reg);
      }
    }
    return reg;
  }

  fn cCall(self: *Self, node: *tir.CallNode, reg: u32) !u32 {
    // For calls, we need to:
    // - get a register window
    // - compile the call expr
    // - generate the instruction: call r(func), b(argc)
    if (node.variadic) node.transformVariadicArgs(self.alloc());
    const token = node.expr.getToken();
    var ty = node.expr.getType().?;
    ty = if (ty.isTop()) ty.top().child else ty;
    if (ty.isTag()) {
      return self.cTagCall(node, ty, reg); // node.typ.?
    }
    if (node.expr.isDotAccess()) {
      const t = node.expr.NdDotAccess.lhs.getType().?;
      if (t.isInstance() or (ty.isFunction() and !t.isModule())) {
        return try self.cMethodCall(node, reg);
      }
    }
    const fun_dst = try self.c(node.expr, reg);
    var _dst = reg;
    const n: u32 = @intCast(node.args.len);
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
      self.fun.code.write3ArgsInst(.Mov, _dst, fun_dst, 0, token.line, self.vm);
    }
    // don't optimize this call's arguments
    self.enterNoOpt();
    for (node.args, 0..) |arg, i| {
      const dst = win + @as(u32, @intCast(i));
      const tmp = try self.c(arg, dst);
      // we need the args to be positioned in line with the function in the register window
      if (tmp != dst) {
        self.fun.code.write3ArgsInst(.Mov, dst, tmp, 0, self.lastLine(), self.vm);
      }
    }
    self.leaveNoOpt();
    if (!ty.isClass() or node.expr.isFun()) {
      self.fun.code.write2ArgsInst(.Call, _dst, n, token.line, self.vm);
    } else {
      const flen: u32 = @intCast(ty.klass().data.fields.len());
      // after this instruction is executed, `_dst` holds the instance
      self.fun.code.write3ArgsInst(.Callc, _dst, n, flen, token.line, self.vm);
      // compile default fields
      const clsnode = &ty.klass().data.node.?.NdClass;
      const tmp = try self.getReg(token);
      self.optimizeConstRK();
      for (clsnode.data.fields, 0..) |fd, idx| {
        if (fd.getFieldValue()) |val| {
          const _r = try self.c(val, tmp);
          self.fun.code.write3ArgsInst(.Sfd, _dst, @intCast(idx), _r, token.line, self.vm);
        }
      }
      self.deoptimizeConstRK();
      self.vreg.releaseReg(tmp);
      // call init to initialize the class's instance
      if (ty.klass().getMethod(InitVar)) |_| {
        self.fun.code.write2ArgsInst(.Fcls, _dst, 0, token.line, self.vm);
      }
    }
    if (do_move) {
      self.fun.code.write3ArgsInst(.Mov, reg, _dst, 0, token.line, self.vm);
      self.vreg.releaseRegWindow(_dst, n + 1);
    } else {
      self.vreg.releaseRegWindow(win, n);
    }
    return reg;
  }

  fn cTagVar(self: *Self, node: *tir.TVarNode, ty: *Type, reg: u32) !u32 {
    const tag = ty.tag();
    const token = node.token;
    assert(tag.fieldsLen() == 0);
    if (ty.isNoneTy()) {
      return self.cConstNoOpt(reg, value.noneVal(), token.line);
    }
    return self.cConstNoOpt(reg, value.createTag(self.vm, tag.name), token.line);
  }

  fn cTagCall(self: *Self, node: *tir.CallNode, ty: *Type, reg: u32) !u32 {
    if (ty.isErrorTy()) {
      var nd = tir.ErrorNode.init(node.args[0]);
      return self.cError(&nd, reg);
    }
    const tag = ty.tag();
    const token = node.expr.getToken();
    const strc = value.createStruct(self.vm, tag.fieldsLen());
    strc.name = value.createString(self.vm, &self.vm.strings, tag.name, false);
    const dst = self.cConstNoOpt(reg, value.objVal(strc), token.line);
    const _dst = try self.getReg(token);
    for (node.args, 0..) |arg, idx| {
      const tmp = try self.c(arg, _dst);
      self.fun.code.write3ArgsInst(.Ssfd, dst, @intCast(idx), tmp, token.line, self.vm);
    }
    self.vreg.releaseReg(_dst);
    return dst;
  }

  fn isTupleLenMethodCall(self: *Self, node: *tir.CallNode) bool {
    _ = self;
    if (node.expr.isDotAccess()) {
      var da = &node.expr.NdDotAccess;
      if (da.lhs.getType()) |ty| {
        return (
          ty.isTupleTy()
          and da.rhs.isTVariable()
          and da.rhs.NdTVar.token.valueEql(ks.LenVar)
        );
      }
    }
    return false;
  }

  fn cMethodCall(self: *Self, node: *tir.CallNode, reg: u32) !u32 {
    // For calls, we need to:
    // - get a register window
    // - compile the call expr
    // - generate the instruction: call r(func), b(argc)

    // optimize tuple len call since we know the result at compile time, i.e. tuple.len()
    if (self.isTupleLenMethodCall(node)) {
      const len = node.expr.NdDotAccess.lhs.getType().?.klass().tparamsLen();
      return self.cConstNoOpt(reg, value.numberVal(@floatFromInt(len)), node.expr.getToken().line);
    }
    if (node.variadic) node.transformVariadicArgs(self.alloc());
    const token = node.expr.getToken();
    var _dst = reg;
    const n: u32 = @intCast(node.args.len);
    const fun_dst = try self.cDotAccess(&node.expr.NdDotAccess, true, _dst);
    assert(fun_dst == _dst);
    const inst = self.fun.code.words.pop();
    const line = self.fun.code.lines.pop();
    var do_move = false;
    var win = blk: {
      break :blk self.vreg.getRegWindow(reg, n) catch {
        do_move = true;
        break :blk self.vreg.getAnyRegWindow(reg, n + 1);
      };
    };
    if (do_move) {_dst = win; win += 1;}
    // we need the function to be in a specific position in the register window, along with its args
    // don't optimize this call's arguments
    if (fun_dst != _dst) {
      self.fun.code.write3ArgsInst(.Mov, _dst, fun_dst, 0, token.line, self.vm);
    }
    self.enterNoOpt();
    for (node.args, 0..) |arg, i| {
      const dst = win + @as(u32, @intCast(i));
      const tmp = try self.c(arg, dst);
      // we need the args to be positioned in line with the function in the register window
      if (tmp != dst) {
        self.fun.code.write3ArgsInst(.Mov, dst, tmp, 0, self.lastLine(), self.vm);
      }
    }
    self.leaveNoOpt();
    self.fun.code.words.push(inst, self.vm);
    self.fun.code.lines.push(line, self.vm);
    self.fun.code.write2ArgsInst(.Call, _dst, n, token.line, self.vm);
    if (do_move) {
      self.fun.code.write3ArgsInst(.Mov, reg, _dst, 0, token.line, self.vm);
      self.vreg.releaseRegWindow(_dst, n + 1);
    } else {
      self.vreg.releaseRegWindow(win, n);
    }
    return reg;
  }

  fn cVoidRet(self: *Self, line: usize) u32 {
    const dst = 0;
    const memidx = self.fun.code.writeValue(value.VOID_VAL, self.vm);
    self.fun.code.write2ArgsInst(.Load, dst, memidx, @intCast(line), self.vm);
    self.fun.code.write2ArgsInst(.Ret, dst, 0, line, self.vm);
    return dst;
  }

  fn cRet(self: *Self, node: *tir.RetNode, reg: u32) !u32 {
    _ = reg;
    if (node.expr) |expr| {
      const rg = try self.getReg(expr.getToken());
      const dst = try self.c(expr, rg);
      self.vreg.releaseReg(rg);
      self.fun.code.write2ArgsInst(.Ret, dst, 0, node.tkbit.line, self.vm);
      return dst;
    }
    return self.cVoidRet(node.tkbit.line);
  }

  fn cError(self: *Self, node: *tir.ErrorNode, reg: u32) !u32 {
    const tmp = try self.getReg(node.expr.getToken());
    const dst = try self.c(node.expr, tmp);
    self.vreg.releaseReg(tmp);
    self.fun.code.write2ArgsInst(.Nerr, reg, dst, self.lastLine(), self.vm);
    return reg;
  }

  fn cMatch(self: *Self, node: *tir.MatchNode, reg: u32) !u32 {
    return self.c(node.lnode.?, reg);
  }

  inline fn cModuleStore(self: *Self, compiler: *Self, node: *tir.ImportNode, module: *value.ObjModule) !void {
    const id = node.getImportNameToken();
    assert(self.inGlobalScope());
    if (self.findGlobal(id)) |info| {
      if (info.isGSym) {
        self.fun.module.items[info.pos] = value.objVal(module);
      } else {
        self.fun.code.write2ArgsInst(.Sglb, compiler.module_dst, info.pos, self.lastLine(), self.vm);
      }
    }
  }

  fn cModuleExec(self: *Self, compiler: *Self, node: *tir.ImportNode, new_fn: *ObjFn, call: value.Value) !void {
    try self.cModuleStore(compiler, node, new_fn.module);
    const reg = try self.getReg(node.getToken());
    defer self.vreg.releaseReg(reg);
    const dst = self.cConstNoOpt(reg, call, self.lastLine());
    self.fun.code.write2ArgsInst(.Call, dst, 0, self.lastLine(), self.vm);
  }

  fn cImport(self: *Self, node: *tir.ImportNode, reg: u32) !u32 {
    // check if we're not already compiling this
    const path = node.program.NdProgram.filepath;
    if (node.typ) |typ| {
      if (!typ.module().resolved) {
        logger.debug("skipping compilation of unchecked module: '{s}'", .{path});
        return reg;
      }
    }
    if (modules.get(path)) |mod| {
      if (mod.compiled) {
        logger.debug("using cached module: '{s}'", .{path});
        try self.cModuleExec(mod.compiler, node, mod.obj, mod.closure);
      } else {
        try self.cModuleStore(mod.compiler, node, mod.obj.module);
      }
      return reg;
    }
    // first compile this node
    var new_fn = value.createFn(self.vm, 0);
    var compiler = util.box(Compiler, Compiler.init(
      self.diag, self.vm, new_fn, self.generics,
      self.allocator, self.prelude, self, null,
    ), self.alloc());
    self.addModule(path, new_fn, compiler);
    try compiler.compileModule(node.program);
    new_fn.name = new_fn.module.name;
    const call = value.objVal(value.createClosure(self.vm, new_fn));
    self.finishModule(path, call);
    try self.cModuleExec(compiler, node, new_fn, call);
    if (comptime util.inDebugMode()) {
      std.debug.print("\n", .{});
      Disassembler.disCode(&new_fn.code, new_fn.getName());
    }
    return reg;
  }

  fn cClassGeneric(self: *Self, node: *Node, reg: u32) CompileError!u32 {
    if (self.generics.get(node)) |list| {
      for (list.items()) |itm| {
        if (itm.instance.isClass()) {
          var cls = &itm.instance.NdClass;
          if (cls.data.builtin) continue;
          // only attach names in the local scope since if we're in the global scope,
          // the name has already been attached in preallocateGlobals()
          if (self.inLocalScope()) {
            cls.name = tir.IdentToken.init(itm.synth_name);
          }
          _ = try self.cClass(itm.instance, cls, reg);
        }
      }
    }
    return reg;
  }

  /// extract the actual class name
  inline fn classname(name: Token) []const u8 {
    return if (std.mem.indexOf(u8, name.lexeme(), ".")) |idx| name.lexeme()[0..idx] else name.lexeme();
  }

  fn cClass(self: *Self, ast_node: *Node, node: *tir.StructNode, _reg: u32) !u32 {
    if (node.isParameterized()) return self.cClassGeneric(ast_node, _reg);
    var reg: u32 = undefined;
    const token = ast_node.getToken();
    var cls = value.createClass(self.vm, node.data.methods.len());
    const is_global = self.inGlobalScope();
    const ident = node.name.toToken();
    if (is_global) {
      // global
      const info = try self.patchGlobal(ident);
      self.initializeGlobal(info);
      reg = try self.getReg(token);
      if (!info.isGSym) {
        cls.name = value.asString(self.fun.code.values.items[info.pos]);
      } else {
        cls.name = value.createString(self.vm, &self.vm.strings, classname(ident), false);
      }
    } else {
      // local
      reg = try self.initLocal(ident);
      cls.name = value.createString(self.vm, &self.vm.strings, classname(ident), false);
    }
    // emit class
    const dst = self.cConstNoOpt(reg, value.objVal(cls), token.line);
    self.incScope();
    for (node.data.methods.items(), 0..) |mth, i| {
      if (mth.isGenericMtd()) continue;
      const mreg = try self.cFun(mth, true, _reg);
      self.fun.code.write3ArgsInst(.Smtd, mreg, dst, @intCast(i), token.line, self.vm);
    }
    self.decScope();
    if (is_global) {
      const info = self.findGlobal(ident).?;
      const inst: OpCode = if (info.isGSym) .Sgsym else .Sglb;
      self.fun.code.write2ArgsInst(inst, dst, info.pos, self.lastLine(), self.vm);
      self.vreg.releaseReg(reg);
    }
    return _reg;
  }

  fn cProgram(self: *Self, node: *tir.ProgramNode, start: u32) !u32 {
    const dst = try self.getReg(Token.getDefaultToken());
    defer self.vreg.releaseReg(dst);
    self.module_dst = self.cConstNoOpt(dst, value.objVal(self.fun.module), 1);
    if (self.entry_module) {
      const reg = try self.getReg(Token.getDefaultToken());
      defer self.vreg.releaseReg(reg);
      try self.compilePrelude(reg);
    }
    var reg = start;
    for (node.decls) |nd| {
      reg = try self.c(nd, reg);
    }
    return reg;
  }

  fn c(self: *Self, node: *Node, reg: u32) CompileError!u32 {
    return switch (node.*) {
      .NdNumber => |*nd| self.cNumber(nd, reg),
      .NdString => |*nd| self.cString(nd, reg),
      .NdBool => |*nd| self.cBool(nd, reg),
      .NdUnary => |*nd| self.cUnary(nd, reg),
      .NdBinary => |*nd| self.cBinary(nd, reg),
      .NdList => |*nd| self.cList(nd, node, reg),
      .NdTuple => |*nd| self.cTuple(nd, node, reg),
      .NdMap => |*nd| self.cMap(nd, reg),
      .NdExprStmt => |*nd| self.cExprStmt(nd, reg),
      .NdTVar => |*nd| self.cVar(nd, reg),
      .NdVarDecl, .NdConstVarDecl => |*nd| self.cVarDecl(nd, reg),
      .NdPubVarDecl => |*nd| self.cVarDecl(nd.getVarDecl(), reg),
      .NdAssign => |*nd| self.cAssign(nd, reg),
      .NdBlock => |*nd| self.cBlock(nd, reg),
      .NdType => |*nd| self.cNType(nd, reg),
      .NdAlias, .NdScope, .NdFailMarker,
      .NdRedunMarker, .NdDiagStartMarker,
      .NdEmpty, .NdTrait => reg,
      .NdCast => |*nd| self.cCast(nd, reg),
      .NdSubscript => |*nd| self.cSubscript(nd, reg),
      .NdDeref => |*nd| self.cDeref(nd, reg),
      .NdSimpleIf => |*nd| self.cSimpleIf(nd, reg),
      .NdWhile => |*nd| self.cWhile(nd, reg),
      .NdControl => |*nd| self.cControl(nd, reg),
      .NdBasicFun, .NdGenericFun, => self.cFun(node, false, reg),
      .NdBasicCall, .NdGenericCall => self.cCall(@constCast(&node.toCallNode()), reg),
      .NdRet => |*nd| self.cRet(nd, reg),
      .NdError => |*nd| self.cError(nd, reg),
      .NdClass => |*nd| self.cClass(node, nd, reg),
      .NdDotAccess => |*nd| self.cDotAccess(nd, false, reg),
      .NdMatch => |*nd| self.cMatch(nd, reg),
      .NdMCondition => |*nd| self.c(nd.tst.NdCondition.cond, reg),
      .NdImport => |*nd| self.cImport(nd, reg),
      .NdParam, .NdField, .NdPubField,
      .NdProgram, .NdCondition, .NdLblArg,
      .NdOrElse, .NdPipeHolder, .NdFor,
      .NdForCounter, .NdGenericMtd => unreachable,
    };
  }

  fn compileModule(self: *Self, node: *Node) !void {
    self.preallocateGlobals(node.NdProgram.decls);
    self.fun.module = value.createModule(self.vm, self.gsyms.len(), node.NdProgram.filepath);
    const token = node.getToken();
    util.assert(
      (self.cProgram(&node.NdProgram, VRegister.DummyReg) catch VRegister.DummyReg) == VRegister.DummyReg,
      "should be a dummy register"
    );
    self.fun.code.write2ArgsInst(.Ret, 0, 0, self.lastLine(), self.vm);
    self.validateNoUnpatchedGlobals(token) catch {};
    if (self.diag.hasErrors()) {
      self.diag.display();
      self.fun.code.words.len = 0;
      return error.CompileError;
    }
  }

  fn setupEntryModule(self: *Self, node: *Node) void {
    // save this module for future import resolution, if any.
    // no need to call finishModule since this is the entry module.
    self.addModule(node.NdProgram.filepath, self.fun, self);
    self.entry_module = true;
  }

  pub fn compile(self: *Self, node: *Node) !void {
    self.scrampleGenericMethods();
    self.setupEntryModule(node);
    try self.compileModule(node);
    self.fun.name = self.fun.module.name;
    modules.clearAndFree(self.alloc());
  }
};
