pub const std = @import("std");
pub const ast = @import("ast.zig");
pub const link = @import("link.zig");
pub const util = @import("util.zig");
const diagnostics = @import("diagnostics.zig");
const analysis = @import("analysis.zig");
const flow = @import("flow.zig");
const ds = @import("ds.zig");
const parse = @import("parse.zig");
const prelude = @import("prelude.zig");
const VebAllocator = @import("allocator.zig");
const NodeList = ast.AstNodeList;
const TypeHashMap = std.StringHashMap(*Type);
const FlowNode = flow.FlowNode;
const FlowList = flow.FlowList;
const FlowMeta = flow.FlowMeta;
const CFG = flow.CFG;
const CFGBuilder = flow.CFGBuilder;
const types = link.types;
const Scope = link.Scope;
const Token = link.Token;
const Type = link.Type;
const TypeKind = link.TypeKind;
const TypeInfo = link.TypeInfo;
const Union = link.Union;
const TContext = link.TContext;
const Node = link.Node;
const TypeLinker = link.TypeLinker;
const TypeHashSet = types.TypeHashSet;
pub const TypeList = types.TypeList;
const Diagnostic = diagnostics.Diagnostic;
const Analysis = analysis.Analysis;

const TypeEnv = struct {
  global: Scope,
  narrowed: TypeHashMap,

  pub fn init(allocator: std.mem.Allocator, other: ?*TypeEnv) @This() {
    var self = @This() {
      .global = Scope.init(allocator),
      .narrowed = TypeHashMap.init(allocator)
    };
    if (other) |source| {
      self.copy(source);
    }
    return self;
  }

  pub fn display(self: *@This(), prompt: []const u8) void {
    std.debug.print("{s}\n", .{prompt});
    var glob = self.global.decls;
    std.debug.print("------------Globals------------\n", .{});
    var i: usize = self.global.len();
    while (i > 0): (i -= 1) {
      var itr = glob.items[i - 1].map.iterator();
      while (itr.next()) |entry| {
        std.debug.print("name: '{s}', ty: '{s}'\n", .{entry.key_ptr.*, entry.value_ptr.*.typename(self.global.allocator)});
      }
      std.debug.print("-------------------------------\n", .{});
    }
    std.debug.print("------------Narrowed------------\n", .{});
    var itr_n = self.narrowed.iterator();
    while (itr_n.next()) |entry| {
      var ty = entry.value_ptr.*;
      std.debug.print("name: '{s}', ty: '{s}'\n", .{entry.key_ptr.*, ty.typename(self.global.allocator)});
    }
    std.debug.print("-------------------------------\n", .{});
  }

  pub fn copy(self: *@This(), source: *@This()) void {
    // globals don't need to be copied.
    self.global = source.global;
    var itr = source.narrowed.iterator();
    while (itr.next()) |entry| {
      self.putNarrowed(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  pub fn clear(self: *@This()) void {
    self.global.clear();
    self.narrowed.clearRetainingCapacity();
  }

  pub fn promoteNarrowed(self: *@This()) void {
    var itr = self.narrowed.iterator();
    self.global.pushScope();
    while (itr.next()) |entry| {
      self.putGlobal(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  pub fn promoteAllGlobals(self: *@This(), cleanup: bool) void {
    if (self.global.len() < 2) return;
    var j: usize = 1;
    var fst = &self.global.decls.items[0].map;
    while (j < self.global.len()) {
      var sec = self.global.decls.items[j].map.iterator();
      while (sec.next()) |entry| {
        if (fst.get(entry.key_ptr.*) == null) {
          util.setStr(*Type, fst, entry.key_ptr.*, entry.value_ptr.*);
        }
      }
      j += 1;
    }
    if (cleanup) {
      while (self.global.len() > 1) {
        self.global.popScope();
      }
    }
  }

  pub inline fn putGlobal(self: *@This(), name: []const u8, ty: *Type) void {
    self.global.insert(name, ty);
  }

  pub inline fn getGlobal(self: *@This(), name: []const u8) ?*Type {
    return self.global.lookup(name);
  }

  pub inline fn putNarrowed(self: *@This(), name: []const u8, ty: *Type) void {
    util.setStr(*Type, &self.narrowed, name, ty);
  }

  pub inline fn getNarrowed(self: *@This(), name: []const u8) ?*Type {
    return self.narrowed.get(name);
  }

  /// combines the narrowed types in self with other into a union when possible
  pub fn or_(self: *@This(), other: *@This()) void {
    var allocator = self.global.allocator();
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      var ident = entry.key_ptr.*;
      var t1 = entry.value_ptr.*;
      if (other.narrowed.get(ident)) |t2| {
        entry.value_ptr.* = t1.unionify(t2, allocator);
      }
    }
    itr = other.narrowed.iterator();
    while (itr.next()) |entry| {
      if (self.narrowed.get(entry.key_ptr.*) == null) {
        var ty = other.getGlobal(entry.key_ptr.*).?;
        self.putNarrowed(entry.key_ptr.*, ty.unionify(entry.value_ptr.*, allocator));
      }
    }
  }

  /// computes the intersection of the narrowed types in self with other
  pub fn and_(self: *@This(), other: *@This()) void {
    var allocator = self.global.allocator();
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      var ident = entry.key_ptr.*;
      var t1 = entry.value_ptr.*;
      if (other.narrowed.get(ident)) |t2| {
        entry.value_ptr.* = (
          t1.intersect(t2, allocator) 
          orelse Type.newNever(allocator)
        );
      }
    }
    itr = other.narrowed.iterator();
    while (itr.next()) |entry| {
      if (self.narrowed.get(entry.key_ptr.*) == null) {
        self.putNarrowed(entry.key_ptr.*, entry.value_ptr.*);
      }
    }
  }

  /// transforms types by negation - negates the narrowed types using the global types
  pub fn not_(self: *@This(), name: []const u8, tc: *TypeChecker) !bool {
    _ = tc;
    var allocator = self.global.allocator();
    if (self.narrowed.get(name)) |t2| {
      var t1 = self.global.lookup(name).?;
      var neg = blk: {
        break :blk t1.negate(t2, allocator) catch {
          std.log.debug("negate with t1 exactly equal to t2 results in the 'never' type\n", .{});
          break :blk (
            if (t1.typeidEql(t2)) Type.newNever(allocator)
            else t1
          );
        };
      };
      self.narrowed.put(name, neg) catch |e| {
        std.debug.print("Error: {}", .{e});
        std.os.exit(1);
      };
      return neg != t1;
    }
    return false;
  }
};

pub const TypeChecker = struct {
  ctx: TContext,
  cfg: CFG = undefined,
  diag: *Diagnostic,
  generics: ds.ArrayHashMap(*Node, *ds.ArrayList(FnInfo)),
  linker: *TypeLinker = undefined,
  current_fn: ?*FlowMeta = null,
  void_ty: *Type,
  _prelude: *Node = undefined,
  cycles: NodeList,
  builder: CFGBuilder,
  analyzer: Analysis,

  const UnitTypes = struct {
    const num = types.Concrete.init(.TyNumber);
    const str = types.Concrete.init(.TyString);
    const bol = types.Concrete.init(.TyBool);
    const nil = types.Concrete.init(.TyNil);
    const tyty = types.Concrete.init(.TyType);

    var tyNumber: Type = Type.init(.{.Concrete = num});
    var tyString: Type = Type.init(.{.Concrete = str});
    var tyBool: Type = Type.init(.{.Concrete = bol});
    var tyNil: Type = Type.init(.{.Concrete = nil});
    var TyTy: Type = Type.init(.{.Concrete = tyty});
  };
  pub const FnInfo = struct {
    instance: *Node,
    synth_name: *ast.VarNode,
    typ: *Type,
  };
  const Self = @This();
  const MAX_STRING_SYNTH_LEN = 0xc;
  const TypeCheckError = error{CheckError, TypeLinkError, SynthFailure, SynthTooLarge, DeadCode};

  const _BuiltinsItf = prelude._BuiltinsItf;

  pub fn init(allocator: std.mem.Allocator, diag: *Diagnostic) @This() {
    return Self {
      .ctx = TContext.init(allocator),
      .diag = diag,
      .generics = ds.ArrayHashMap(*Node, *ds.ArrayList(FnInfo)).init(allocator),
      .builder = CFGBuilder.init(allocator),
      .cycles = NodeList.init(allocator),
      .analyzer = Analysis.init(diag),
      .void_ty = Type.newVoid().box(allocator),
    };
  }

  fn loadBuiltinsPrelude(self: *Self, linker: *TypeLinker, al: *VebAllocator) void {
    const filename = @as([]const u8, "$builtins$");
    var parser = parse.Parser.init(@constCast(&@as([]const u8, _BuiltinsItf)), &filename, al);
    const node = try parser.parse();
    linker.linkTypes(node) catch unreachable;
    self.buildProgramFlow(node) catch unreachable;
    self.flowInferEntry(self.cfg.program.entry) catch unreachable;
    for (node.AstProgram.decls.items()) |itm| {
      if (itm.isFun()) {
        itm.AstFun.body.AstBlock.checked = true;
        itm.AstFun.is_builtin = true;
      }
    }
    self._prelude = node;
  }

  fn error_(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) self.diag.addDiagnostics(.DiagError, token, "TypeError: " ++ fmt, args);
    return error.CheckError;
  }

  inline fn warn(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) void {
    if (emit) self.diag.addDiagnostics(.DiagWarn, token, "TypeWarning: " ++ fmt, args);
  }

  fn genName(self: *Self, l1: usize, l2: usize) []const u8 {
    var name = std.fmt.allocPrint(self.ctx.allocator(), "${}.{}", .{l1, l2}) catch "";
    return name;
  }

  fn createSynthName(self: *Self, fun: *ast.FunNode, args: *TypeList, targs: ?*NodeList) []const u8 {
    // FIXME: this is unhygienically inefficient.
    if (fun.is_builtin) {
      // don't monomorphize builtin generic func names
      return fun.name.?.token.value;
    }
    var start = if (fun.name) |nm| nm.token.value else self.genName(
      if (targs) |ta| ta.len() else 0,
      args.len()
    );
    var al = self.ctx.allocator();
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = writer.write(start) catch undefined;
    if (targs) |ta| {
      if (args.len() == 0) {
        for (ta.items()) |nd| {
          _ = writer.write(".") catch undefined;
          _ = writer.write(nd.getType().?.typenameNoAlias(al)) catch undefined;
        }
      }
    }
    for (args.items()) |ty| {
      _ = writer.write(".") catch undefined;
      _ = writer.write(ty.typenameNoAlias(al)) catch undefined;
    }
    return writer.context.items;
  }

  fn boxSynthName(self: *Self, token: Token, val: []const u8) *ast.VarNode {
    var name_token = token;
    name_token.ty = .TkAllocString;
    name_token.value = val;
    return ast.VarNode.init(name_token).box(self.ctx.allocator());
  }

  fn addFnInfo(self: *Self, origin: *Node, instance: *Node, synth_name: *ast.VarNode, typ: *Type) void {
    var data = @as(FnInfo, .{
      .instance = instance,
      .synth_name = synth_name,
      .typ = typ,
    });
    if (self.generics.get(origin)) |list| {
      list.append(data);
    } else {
      var list = util.box(
        ds.ArrayList(FnInfo),
        ds.ArrayList(FnInfo).init(self.ctx.allocator()),
        self.ctx.allocator()
      );
      list.append(data);
      self.generics.set(origin, list);
    }
  }

  fn findFnInfo(self: *Self, origin: *Node, synth_name: []const u8) ?FnInfo {
    if (self.generics.get(origin)) |list| {
      for (list.items()) |info| {
        if (std.mem.eql(u8, info.synth_name.token.value, synth_name)) {
          return info;
        }
      }
    }
    return null;
  }

  fn findType(self: *Self, name: []const u8) ?*Type {
    // TODO
    if (self.ctx.typScope.lookup(name)) |ty| {
      return switch (ty.kind) {
        .Concrete, .Variable => ty,
        else => self.ctx.copyType(ty),
      };
    }
    return null;
  }

  fn findName(self: *Self, name: []const u8) ?*Type {
    if (self.ctx.varScope.lookup(name)) |ty| {
      return switch (ty.kind) {
        .Concrete, .Variable, .Function => ty,
        else => self.ctx.copyType(ty),
      };
    }
    return self.findType(name);
  }

  fn lookupName(self: *Self, ident: *ast.VarNode, emit: bool) !*Type {
    if (self.findName(ident.token.value)) |found| {
      return found;
    } else {
      return self.error_(emit, ident.token, "Could not resolve type of ident: '{s}'", .{ident.token.value});
    }
  }

  fn validateGenericParamSize(self: *Self, size: usize, exp_size: usize, token: Token) !void {
    if (size != exp_size) {
      return self.error_(
        true, token, 
        "Generic type parameter(s) mismatch. Expected {} type parameter(s), but got {}", 
        .{exp_size, size}
      );
    }
  }

  fn insertType(self: *Self, var_typ: *Type, typ: *Type) void {
    var tvar = var_typ.variable();
    if (tvar.tokens.len() > 1) util.todo("multiple var tokens!");
    var name = tvar.tokens.itemAt(0).value;
    self.ctx.typScope.insert(name, typ);
  }

  pub inline fn insertVar(self: *Self, name: []const u8, ty: *Type) void {
    self.ctx.varScope.insert(name, ty);
  }

  fn getTypename(self: *Self, typ: *Type) []const u8 {
    return typ.typename(self.ctx.allocator());
  }

  fn newAstNode(self: *Self) *Node {
    return util.alloc(Node, self.ctx.allocator());
  }

  fn newUnaryNode(self: *Self, expr: *Node, op: Token) *Node {
    var node = self.newAstNode();
    node.* = .{.AstUnary = ast.UnaryNode.init(expr, op)};
    return node;
  }

  fn newStringNode(self: *Self, val: []const u8, token: Token) Node {
    _ = self;
    var tok = Token.tokenFrom(&token);
    tok.value = val;
    return .{.AstString = ast.LiteralNode.init(tok)};
  }

  /// copy env to varScope
  fn copyEnv(self: *Self, env: *TypeEnv) void {
    var itr = env.global.current().map.iterator();
    while (itr.next()) |entry| {
      self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
    }
    itr = env.narrowed.iterator();
    while (itr.next()) |entry| {
      self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  /// check if this flow node is unresolved
  inline fn isFlowNodeUnresolved(self: *Self, node: *FlowNode) bool {
    _ = self;
    return node.res.isUnresolved();
  }

  //***********************************************************//
  //***********  narrowing  ***********************************//
  //***********************************************************//

  fn synthesizeVar(self: *Self, vr: *ast.VarNode, other: ?*Node, env: *TypeEnv) TypeCheckError!*ast.VarNode {
    if (vr.typ == null) _ = try self.inferVar(vr, false);
    if (other == null) return vr;
    switch (other.?.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| {
        if (lit.token.value.len > MAX_STRING_SYNTH_LEN) {
          return error.SynthTooLarge;
        }
        var token = ast.Token.tokenFrom(&vr.token);
        token.value = std.fmt.allocPrint(
          self.ctx.allocator(), "{s}.{s}",
          .{vr.token.value, lit.token.value}
        ) catch return error.SynthFailure;
        var ret = ast.VarNode.init(token);
        var last = self.diag.count();
        _ = self.narrowVariable(&ret, env) catch {
          self.diag.popUntil(last);
        };
        return ret.box(self.ctx.allocator());
      },
      else => |els| {
        std.debug.print("unexpected ast type for synth {}", .{els});
        return error.SynthFailure;
      }
    }
  }

  fn synthesizeSubscript(self: *Self, node: *ast.SubscriptNode, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    // synthesize node.expr with node.index
    try self.narrow(node.expr, env, assume_true);
    // if this node has been narrowed before, we don't want to reset its type,
    // as narrowing now may be from taking a false path. Hence, we use `change`
    // to determine whether to update `node.narrowed` or not
    var change = (node.narrowed == null);
    if (change) {
      // since we're in the process of narrowing this subscript, we do not want `inferSubscript()`
      // to assume that this node is already narrowed, hence, we set a dummy value for `node.narrowed`
      // as a flag to `inferSubscript()` indicating that this node is being narrowed.
      node.narrowed = @constCast(&.{.token = undefined, .typ = null});
    }
    var ty = try self.inferSubscript(node);
    var narrowed = try self.synthesize(node.expr, env, assume_true);
    narrowed = try self.synthesizeVar(narrowed, node.index, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }
  
  fn synthesizeDeref(self: *Self, node: *ast.DerefNode, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    // synthesize node.expr
    try self.narrow(node.expr, env, assume_true);
    // similar to what inferSubscript() does here.
    var change = (node.narrowed == null);
    if (change) {
      // similar to what inferSubscript() does here.
      node.narrowed = @constCast(&.{.token = undefined, .typ = null});
    }
    var ty = try self.inferDeref(node);
    var narrowed = try self.synthesize(node.expr, env, assume_true);
    var nil = self.newStringNode("?", node.token);
    narrowed = try self.synthesizeVar(narrowed, &nil, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }

  fn synthesize(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    return switch (node.*) {
      .AstVar => |*vr| try self.synthesizeVar(vr, null, env),
      .AstSubscript => |*sub| try self.synthesizeSubscript(sub, env, assume_true),
      .AstDeref => |*der| try self.synthesizeDeref(der, env, assume_true),
      else => error.SynthFailure,
    };
  }

  inline fn canNarrowSubscript(self: *Self, node: *ast.SubscriptNode) bool {
    return self.canNarrow(node.expr) and node.index.isComptimeConst();
  }

  inline fn canNarrowDeref(self: *Self, node: *ast.DerefNode) bool {
    return self.canNarrow(node.expr);
  }

  fn canNarrow(self: *Self, node: *Node) bool {
    return switch (node.*) {
      .AstVar => true,
      .AstSubscript => |*sub| self.canNarrowSubscript(sub),
      .AstDeref => |*der| self.canNarrowDeref(der),
      else => false,
    };
  }

  fn narrowAtomic(self: *Self, node: *Node, env: *TypeEnv) !void {
    _ = env;
    _ = try self.infer(node);
  }

  fn narrowVariable(self: *Self, node: *ast.VarNode, env: *TypeEnv) !void {
    if (env.getNarrowed(node.token.value)) |ty| {
      self.insertVar(node.token.value, ty);
      node.typ = ty;
    } else if (env.getGlobal(node.token.value)) |ty| {
      self.insertVar(node.token.value, ty);
      node.typ = ty;
    } else {
      var ty = try self.lookupName(node, true);
      env.putGlobal(node.token.value, ty);
      node.typ = ty;
    }
  }

  fn narrowCast(self: *Self, node: *ast.CastNode, env: *TypeEnv, assume_true: bool) !void {
    try self.narrow(node.expr, env, assume_true);
    _ = try self.inferCast(node);
  }

  fn narrowSubscript(self: *Self, node: *ast.SubscriptNode, env: *TypeEnv, assume_true: bool) !void {
    if (self.canNarrowSubscript(node)) {
      var vr = try self.synthesizeSubscript(node, env, assume_true);
      if (self.findName(vr.token.value)) |_| {
        try self.narrowVariable(vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertVar(vr.token.value, vr.typ.?);
      }
    } else {
      try self.narrow(node.expr, env, assume_true);
      try self.narrow(node.index, env, assume_true);
      _ = try self.inferSubscript(node);
    }
  }

  fn narrowDeref(self: *Self, node: *ast.DerefNode, env: *TypeEnv, assume_true: bool) !void {
    if (self.canNarrowDeref(node)) {
      var vr = try self.synthesizeDeref(node, env, assume_true);
      if (self.findName(vr.token.value)) |_| {
        try self.narrowVariable(vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertVar(vr.token.value, vr.typ.?);
      }
    } else {
      try self.narrow(node.expr, env, assume_true);
      _ = try self.inferDeref(node);
    }
  }

  inline fn isNarrowableLiteral(self: *Self, node: *ast.BinaryNode) bool {
    return (
      (node.op.optype == .OpEqq or node.op.optype == .OpNeq)
      and self.canNarrow(node.left)
      and (node.right.isNilLiteral() or node.right.isConstLiteral())
    );
  }

  fn narrowBinary(self: *Self, node: *ast.BinaryNode, env: *TypeEnv, assume_true: bool) !void {
    if (node.op.optype == .OpIs) {
      if (node.left.isVariable()) {
        try self.narrowVariable(&node.left.AstVar, env);
        _ = try self.inferBinary(node);
        // if we get here, then right must be a TypeNode
        var ty = try self.lookupName(&node.left.AstVar, true);
        if (Type.is(ty, node.right.AstNType.typ, self.ctx.allocator())) |is_ty| {
          env.putNarrowed(node.left.AstVar.token.value, is_ty);
          if (!assume_true) {
            if (!try env.not_(node.left.AstVar.token.value, self)) {
              self.warn(
                true, node.op.token,
                "Cannot narrow type '{s}'",
                .{self.getTypename(node.left.getType().?)}
              );
            }
          }
          node.left.AstVar.typ = is_ty;
          return;
        }
      } else if (self.canNarrow(node.left)) {
        try self.narrow(node.left, env, assume_true);
        if (node.left.getNarrowed()) |vr| {
          var synth = node.*;
          synth.left = @constCast(&@as(Node, .{.AstVar = vr.*}));
          try self.narrowBinary(&synth, env, assume_true);
          node.typ = synth.typ;
          return;
        }
      }
      if (node.right.getType().?.isConstant()) {
        try self.narrow(node.left, env, assume_true);
        try self.narrow(node.right, env, assume_true);
        _ = try self.inferBinary(node);
        return;
      }
      var lty = node.left.getType();
      var rty = node.right.getType();
      if (lty != null and rty != null) {
        self.warn(
          true, node.op.token,
          "Cannot narrow type '{s}' to '{s}'",
          .{self.getTypename(lty.?), self.getTypename(rty.?)}
        );
      } else {
        self.warn(
          true, node.op.token,
          "Cannot narrow type at expression",
          .{}
        );
      }
    } else if (node.op.optype == .OpAnd) {
      env.global.pushScope();
      if (assume_true) {
        try self.narrow(node.left, env, true);
        var rhs_env = TypeEnv.init(self.ctx.allocator(), env);
        rhs_env.promoteNarrowed();
        try self.narrow(node.right, &rhs_env, true);
        env.and_(&rhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      } else {
        try self.narrow(node.left, env, false);
        var lhs_env = TypeEnv.init(self.ctx.allocator(), null);
        lhs_env.global = env.global;
        try self.narrow(node.left, &lhs_env, true);
        try self.narrow(node.right, &lhs_env, false);
        env.or_(&lhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      }
      return;
    } else if (node.op.optype == .OpOr) {
      if (assume_true) {
        try self.narrow(node.left, env, true);
        var lhs_env = TypeEnv.init(self.ctx.allocator(), null);
        lhs_env.global = env.global;
        try self.narrow(node.left, &lhs_env, false);
        try self.narrow(node.right, &lhs_env, true);
        env.or_(&lhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      } else {
        try self.narrow(node.left, env, false);
        var rhs_env = TypeEnv.init(self.ctx.allocator(), env);
        rhs_env.promoteNarrowed();
        try self.narrow(node.right, &rhs_env, false);
        env.and_(&rhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      }
      return;
    } else if (self.isNarrowableLiteral(node)) {
      var bin = b: {
        var bin = node.*;
        bin.op.optype = .OpIs;
        bin.right = node.right.toTypeNode(self.ctx.allocator());
        break :b bin;
      };
      try self.narrowBinary(&bin, env, if (node.op.optype == .OpEqq) assume_true else !assume_true);
      node.typ = bin.typ;
      return;
    }
    try self.narrow(node.left, env, assume_true);
    try self.narrow(node.right, env, assume_true);
    _ = try self.inferBinary(node);
  }

  fn narrowUnary(self: *Self, node: *ast.UnaryNode, env: *TypeEnv, assume_true: bool) !void {
    if (node.op.optype == .OpNot) {
      try self.narrow(node.expr, env, !assume_true);
      // at this point, node.expr typechecked successfully
      node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    } else {
      try self.narrow(node.expr, env, assume_true);
      _ = try self.inferUnary(node);
    }
  }

  fn narrow(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!void {
    return switch (node.*) {
      .AstVar => |*vr| try self.narrowVariable(vr, env),
      .AstBinary => |*bin| try self.narrowBinary(bin, env, assume_true),
      .AstUnary => |*una| try self.narrowUnary(una, env, assume_true),
      .AstSubscript => |*sub| try self.narrowSubscript(sub, env, assume_true),
      .AstCast => |*cst| try self.narrowCast(cst, env, assume_true),
      .AstDeref => |*der| try self.narrowDeref(der, env, assume_true),
      .AstNumber,
      .AstString,
      .AstBool,
      .AstList,
      .AstMap,
      .AstNType => try self.narrowAtomic(node, env),
      else => unreachable,
    };
  }

  //***********************************************************//
  //****************  flow  ***********************************//
  //***********************************************************//
  fn flowInferEntry(self: *Self, node: *FlowNode) !void {
    std.debug.assert(node.tag == .CfgEntry);
    self.ctx.enterScope();
    // automatically resolved on entry
    node.res = .Resolved;
    // `entry` node, so we don't care about the `node` & `prev` properties
    for (node.prev_next.items()) |item| {
      if (item.next) {
        try self.flowInfer(item.flo, true);
      }
    }
  }

  fn flowInferExit(self: *Self, node: *FlowNode) !void {
    std.debug.assert(node.tag == .CfgExit);
    node.res = .Resolved;
    self.ctx.leaveScope();
    // assert no more nodes after exit
    std.debug.assert(node.prev_next.count(FlowNode.isNext) == 0);
  }

  fn flowInferMeta(self: *Self, node: *FlowNode) !void {
    if (node.res.isResolved()) return;
    if (node.tag == .CfgEntry) {
      return try self.flowInferEntry(node);
    }
    if (node.tag == .CfgExit) {
      return try self.flowInferExit(node);
    }
    return self.flowInferNode(node);
  }

  fn flowInferNode(self: *Self, node: *FlowNode) !void {
    if (node.res.isResolved()) return;
    _ = try self.infer(node.node);
    node.res = .Resolved;
  }

  fn flowInferCondition(self: *Self, node: *FlowNode) !void {
    if (node.res.isResolved()) return;
    node.res = .Processing;
    // TODO: is there a need to explicitly merge types from incoming edges?
    self.ctx.varScope.pushScope();
    errdefer self.ctx.varScope.popScope();
    // This is a branch point and a meet point. 
    // As a meet point, all types on the incoming edges are merged.
    // As a branch point, types are narrowed along outgoing edges based 
    // on the condition expression.
    var env = TypeEnv.init(self.ctx.allocator(), null);
    env.global.pushScope();
    self.narrow(node.node.AstCondition.cond, &env, true) catch |e| return e;
    // TODO: rework token extraction for better error reporting
    try self.checkCondition(node.node.getType().?, node.node.AstCondition.cond.getToken());
    // get all nodes on the true edges & flowInfer with env
    var out_nodes = node.getOutgoingNodes(.ETrue, self.ctx.allocator());
    self.copyEnv(&env);
    for (out_nodes.items()) |nd| {
      try self.flowInfer(nd, false);
    }
    self.ctx.varScope.popScope();
    self.ctx.varScope.pushScope();
    // get all nodes on the false edges & flowInfer with not\env
    out_nodes = node.getOutgoingNodes(.EFalse, self.ctx.allocator());
    env.clear();
    env.global.pushScope();
    // TODO: finetune
    var last = self.diag.count();
    self.narrow(node.node.AstCondition.cond, &env, false) catch {
      self.diag.popUntil(last);
      env.narrowed.clearRetainingCapacity();
    };
    self.copyEnv(&env);
    for (out_nodes.items()) |nd| {
      try self.flowInfer(nd, false);
    }
    node.res = .Resolved;
    // when type checking conditions along the false path, check the first sequential node
    // after the nodes on the false path. If that node has only one incoming edge,
    // then don’t pop the type information gathered on the false path
    if (out_nodes.len() == 0) {
      var edges: usize = 0;
      for (node.prev_next.items()) |itm| {
        // get the first sequential node after the nodes on the false path
        if (itm.next and itm.flo.edge == .ESequential) {
          // track the number of incoming edges from this node
          for (itm.flo.prev_next.items()) |fd| {
            if (fd.prev) {
              edges += 1;
            }
          }
          break;
        }
      }
      if (edges == 1) {
        // set all type information gathered to the current varScope
        var scope = self.ctx.varScope.decls.pop();
        var itr = scope.map.iterator();
        while (itr.next()) |entry| {
          self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
        }
        return;
      }
    }
    self.ctx.varScope.popScope();
  }

  fn flowInfer(self: *Self, node: *FlowNode, inferNext: bool) TypeCheckError!void {
    for (node.prev_next.items()) |item| {
      // we can only proceed to resolve this node when all 
      // incoming edges have been resolved or when we're resolving 
      // all nodes on the true edges of a Condition.
      if (item.prev) {
        if (self.isFlowNodeUnresolved(item.flo)) {
          return;
        }
      }
    }
    switch (node.node.*) {
      .AstCondition => try self.flowInferCondition(node),
      .AstEmpty => try self.flowInferMeta(node),
      .AstControl => {},
      .AstFun => {
        if (node.isEntryNode()) {
          try self.flowInferEntry(node);
        } else {
          // Some functions may be mutually recursive, or may depend on some other
          // functions not yet known at the time of inference of this current one.
          // So only partially infer this function. Full inference is done by need.
          if (!node.node.AstFun.isAnonymous()) {
            _ = try self.inferFunPartial(node.node);
          } else {
            _ = try self.inferFun(node.node, null);
          }
          node.res = .Resolved;
        }
      },
      else => try self.flowInferNode(node),
    }
    if (!inferNext) return;
    for (node.prev_next.items()) |item| {
      if (item.next) {
        // FIXME: this is just a hack, don't know how to fix this yet.
        //        temporarily fixes cycles arising from while loops
        if (item.flo.node.isCondition() and item.flo.res.isResolved()) continue;
        try self.flowInfer(item.flo, inferNext);
      }
    }
  }

  //***********************************************************//
  //***********  inference  ***********************************//
  //***********************************************************//
  fn inferLiteral(self: *Self, node: *ast.LiteralNode, kind: types.Concrete) !*Type {
    if (node.typ) |typ| {
      return typ;
    } else {
      node.typ = kind.toType().box(self.ctx.allocator());
      node.typ.?.kind.Concrete.val = &node.token.value;
      return node.typ.?;
    }
  }

  fn inferNil(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.nil);
  }

  fn inferNumber(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.num);
  }

  fn inferString(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.str);
  }

  fn inferBool(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.bol);
  }

  inline fn inferCollection(self: *Self, node: *ast.ListNode, name: []const u8) !*Type {
    // create a new type
    var al = self.ctx.allocator();
    var base = Type.newConcrete(.TyClass, name).box(al);
    node.typ = Type.newGeneric(al, base).box(al);
    if (node.elems.len() == 0) {
      return node.typ.?;
    }
    // infer type of elements stored in the list
    var typeset = TypeHashSet.init(al);
    typeset.ensureTotalCapacity(node.elems.len());
    for (node.elems.items()) |elem| {
      var typ = try self.infer(elem);
      typeset.set(typ.typeid(), typ);
    }
    var gen = node.typ.?.generic();
    gen.append(Type.compressTypes(&typeset, null));
    return node.typ.?;
  }

  fn inferList(self: *Self, node: *ast.ListNode) !*Type {
    return self.inferCollection(node, "list");
  }

  fn inferTuple(self: *Self, node: *ast.ListNode) !*Type {
    return self.inferCollection(node, "tuple");
  }

  fn inferMap(self: *Self, node: *ast.MapNode) !*Type {
    // create a new type
    var al = self.ctx.allocator();
    var base = Type.newConcrete(.TyClass, "map").box(al);
    node.typ = Type.newGeneric(al, base).box(al);
    if (node.pairs.len() == 0) {
      return node.typ.?;
    }
    // infer type of items stored in the map
    var first_pair = node.pairs.itemAt(0);
    var key_typ = try self.infer(first_pair.key);
    var val_typ = try self.infer(first_pair.value);

    if (node.pairs.len() > 1) {
      for (node.pairs.items()[1..], 1..) |pair, i| {
        _ = i;
        var typ = try self.infer(pair.key);
        var debug = pair.key.getToken();
        _ = self.checkAssign(key_typ, typ, debug, false) catch {
          return self.error_(
            true, debug,
            "expected key type '{s}', but found '{s}'",
            .{self.getTypename(key_typ), self.getTypename(typ)}
          );
        };
        typ = try self.infer(pair.value);
        debug = pair.value.getToken();
        _ = self.checkAssign(val_typ, typ, debug, false) catch {
          return self.error_(
            true, debug,
            "expected value type '{s}', but found '{s}'",
            .{self.getTypename(val_typ), self.getTypename(typ)}
          );
        };
      }
    }
    if (key_typ.isUnion()) {
      // we don't want active types in a map's key union as this can 
      // confuse the type checker on permissible operations during casting
      key_typ.union_().active = null;
    }
    if (val_typ.isUnion()) {
      // ditto
      val_typ.union_().active = null;
    }
    var gen = node.typ.?.generic();
    gen.append(key_typ);
    gen.append(val_typ);
    return node.typ.?;
  }

  fn inferUnary(self: *Self, node: *ast.UnaryNode) !*Type {
    node.typ = try self.infer(node.expr);
    // unary op: ~, !, -, +
    if (node.op.optype == .OpBitInvert or node.op.optype == .OpAdd or node.op.optype == .OpSub) {
      try self.checkUnary(node, &UnitTypes.tyNumber);
    } else {
      std.debug.assert(node.op.optype == .OpNot);
      // `!` accepts any type and returns a boolean. 
      // It applies an implicit bool cast to such a type.
      node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    }
    return node.typ.?;
  }

  fn inferBinary(self: *Self, node: *ast.BinaryNode) !*Type {
    if (node.op.optype == .OpIs) {
      return self.inferIs(node);
    }
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    node.typ = lhsTy;
    try self.checkBinary(node, rhsTy, false);
    if (node.op.optype.isCmpOp()) {
      node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    }
    return node.typ.?;
  }

  fn inferIs(self: *Self, node: *ast.BinaryNode) !*Type {
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    // lhs must not be type Type, and rhs must be type Type
    if (lhsTy.isTypeTy()) {
      return self.error_(
        true, node.op.token,
        "Expected type instance in lhs of `is` operator but found '{s}'",
        .{self.getTypename(lhsTy)}
      );
    } else if (!rhsTy.isTypeTy()) {
      var help = (
        if (node.right.isConstLiteral() or rhsTy.isLikeConstant())
          "\n\tHelp: For constant types, consider using '==' or '!=' operator instead."
        else ""
      );
      return self.error_(
        true, node.op.token,
        "Expected type 'Type' in rhs of `is` operator but found type '{s}'{s}",
        .{self.getTypename(rhsTy), help}
      );
    }
    // at this point, rhs is a TypeNode
    var ty = node.right.AstNType.typ;
    if ((ty.isGeneric() and ty.generic().tparams_len() != 0) or ty.isUnion() or ty.isVariable() or ty.isFunction()) {
      return self.error_(
        true, node.op.token,
        "Expected a concrete or class type in the rhs of the `is` operator but found '{s}'",
        .{self.getTypename(ty)}
      );
    }
    // temporarily assign lhsTy for checking
    node.typ = lhsTy;
    // use the actual type on rhs for checks
    try self.checkBinary(node, ty, true);
    // `is` returns type bool, so reassign
    node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    return node.typ.?;
  }

  fn inferVar(self: *Self, node: *ast.VarNode, emit: bool) !*Type {
    // TODO: Do we need to always fetch the updated type?
    node.typ = try self.lookupName(node, emit);
    var typ = node.typ.?;
    // since we infer functions by need, we check if this is a reference
    // to a function and try to infer the function if it's not yet fully inferred
    if (typ.isFunction() and !typ.function().isGeneric()) {
      var fun_ty = typ.function();
      if (
        fun_ty.node != null
        and !fun_ty.node.?.AstFun.body.AstBlock.checked
        and !self.cycles.contains(fun_ty.node.?, Node.eql)
      ) {
        _ = try self.inferFun(fun_ty.node.?, typ);
      }
    }
    return typ;
  }

  fn inferCast(self: *Self, node: *ast.CastNode) !*Type {
    var typ = try self.infer(node.expr);
    var cast_typ = node.typn.typ;
    // use coercion rules
    return try self.checkCast(typ, cast_typ, node.typn.token, true);
  }

  fn inferAssign(self: *Self, node: *ast.BinaryNode) !*Type {
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    var typ = try self.checkAssign(lhsTy, rhsTy, node.op.token, true);
    // update type.
    switch (node.left.*) {
      // need to always update, because lookup copies.
      .AstVar => |ident| self.insertVar(ident.token.value, typ),
      .AstSubscript => |*sub| {
        if (sub.expr.getType()) |ty| {
          if (ty.isTupleTy()) {
            return self.error_(true, node.op.token,
              "Cannot modify immutable type '{s}'",
              .{self.getTypename(ty)}
            );
          }
        }
      },
      else => {}
    }
    return typ;
  }

  fn inferNType(self: *Self, node: *ast.TypeNode) !*Type {
    _ = self;
    // if this type node was found in an expression 
    // (i.e. not in an alias or annotation context), then return TyType
    if (!node.from_alias_or_annotation) {
      return &UnitTypes.TyTy;
    }
    return node.typ;
  }

  fn inferAlias(self: *Self, node: *ast.AliasNode) !*Type {
    _ = try self.inferNType(node.alias);
    _ = try self.inferNType(node.aliasee);
    return node.typ;
  }

  fn inferExprStmt(self: *Self, node: *ast.ExprStmtNode) !*Type {
    return try self.infer(node.expr);
  }

  fn inferVarDecl(self: *Self, node: *ast.VarDeclNode) !*Type {
    if (!node.is_param) {
      if (node.ident.typ) |typ| {
        var expr_ty = try self.infer(node.value);
        node.ident.typ = typ;
        _ = try self.checkAssign(typ, expr_ty, node.ident.token, true);
      } else {
        node.ident.typ = try self.infer(node.value);
      }
    }
    self.insertVar(node.ident.token.value, node.ident.typ.?);
    return node.ident.typ.?;
  }

  fn inferSubscript(self: *Self, node: *ast.SubscriptNode) TypeCheckError!*Type {
    if (node.typ) |ty| return ty;
    // if we're narrowing this subscript, it's narrowed type would not be
    // set yet, hence, we use `node.narrowed == null` to check that we're not narrowing,
    // hence, the narrowed type should already exist in varScope
    if (node.narrowed == null and self.canNarrowSubscript(node)) {
      var env = TypeEnv.init(self.ctx.allocator(), null);
      env.global.pushScope();
      node.narrowed = self.synthesizeSubscript(node, &env, true) catch |e| return e;
      if (node.narrowed) |narrowed| {
        node.typ = self.inferVar(narrowed, false) catch null;
        if (node.typ) |ty| {
          return ty;
        }
      }
    }
    var expr_ty = try self.infer(node.expr);
    // fail fast
    if (!expr_ty.isGeneric()) {
      return self.error_(
        true, node.index.getToken(),
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    var index_ty = try self.infer(node.index);
    try self.checkSubscript(node, expr_ty, index_ty);
    return node.typ.?;
  }

  fn inferDeref(self: *Self, node: *ast.DerefNode) !*Type {
    if (node.typ) |ty| return ty;
    // if this node is not being narrowed (`node.narrowed == null`),
    // try to see if we can obtain an inferred narrow type.
    if (node.narrowed == null and self.canNarrowDeref(node)) {
      var env = TypeEnv.init(self.ctx.allocator(), null);
      env.global.pushScope();
      node.narrowed = self.synthesizeDeref(node, &env, true) catch return error.CheckError;
      if (node.narrowed) |narrowed| {
        node.typ = self.inferVar(narrowed, false) catch null;
        if (node.typ) |ty| {
          return ty;
        }
      }
    }
    var expr_ty = try self.infer(node.expr);
    try self.checkDeref(node, expr_ty);
    return node.typ.?;
  }

  fn inferBlock(self: *Self, node: *ast.BlockNode) !*Type {
    self.ctx.enterScope();
    for (node.nodes.items()) |item| {
      _ = try self.infer(item);
    }
    self.ctx.leaveScope();
    node.checked = true;
    return self.void_ty;
  }

  fn inferWhile(self: *Self, node: *ast.WhileNode) !*Type {
    var ty = try self.infer(node.cond);
    try self.checkCondition(ty, node.cond.getToken());
    return try self.infer(node.then);
  }

  fn createFunType(self: *Self, ast_node: *Node, ret: ?*Type) !*Type {
    var ret_ty = blk: {
      if (ret) |typ| {
        break :blk typ;
      } else {
        var tmp = Type.newVariable(self.ctx.allocator());
        tmp.variable().append(Token.getDefault());
        break :blk tmp.box(self.ctx.allocator());
      }
    };
    var ty = Type.newFunction(self.ctx.allocator(), ret_ty);
    var fun = ty.function();
    var node = &ast_node.AstFun;
    fun.node = ast_node;
    fun.tparams = node.tparams;
    fun.params.ensureTotalCapacity(node.params.capacity());
    for (node.params.items()) |vd| {
      fun.params.append(vd.ident.typ.?);
    }
    return ty.box(self.ctx.allocator());
  }

  fn getExitPrevs(self: *Self, node: *FlowNode) FlowList {
    var nodes = FlowList.init(self.ctx.allocator());
    for (node.prev_next.items()) |nd| {
      if (FlowNode.isPrev(nd)) {
        nodes.append(nd.flo);
      }
    }
    return nodes;
  }

  fn inferFunPartial(self: *Self, node: *Node) !*Type {
    var fun = &node.AstFun;
    // we need to infer this first to handle recursive functions
    var ty = try self.createFunType(node,  node.getType());
    // set the function's name (if available) to it's full type
    if (fun.name) |ident| {
      self.insertVar(ident.token.value, ty);
    }
    return ty;
  }

  fn inferFun(self: *Self, node: *Node, typ: ?*Type) !*Type {
    var fun = &node.AstFun;
    var ty = (
      typ orelse if (fun.name) |ident| try self.lookupName(ident, true)
      else try self.createFunType(node, node.getType())
    );
    // generic is infer-by-need - performed on call/reference.
    if (!fun.isGeneric()) {
      var flo = blk: {
        if (self.cfg.lookup(node)) |fm| break :blk fm;
        std.debug.assert(!node.AstFun.isGeneric());
        try self.linker.linkFun(fun, true);
        try self.buildFunFlow(node);
        break :blk self.cfg.lookup(node).?;
      };
      try self.analyzer.analyzeDeadCode(flo.dead);
      self.cycles.append(node);
      var prev_fn = self.current_fn;
      self.current_fn = &flo;
      try self.flowInfer(flo.entry, true);
      self.current_fn = prev_fn;
      _ = self.cycles.pop();
      ty.function().ret = try self.inferFunReturnType(node, flo);
      fun.body.AstBlock.checked = true;
      try self.analyzer.analyzeDeadCodeWithTypes(flo.entry);
    }
    return ty;
  }

  fn inferFunReturnTypePartial(self: *Self, flo: FlowMeta) ?*Type {
    var prev_nodes = self.getExitPrevs(flo.exit);
    // unionify all return types at exit
    var uni = Union.init(self.ctx.allocator());
    for (prev_nodes.items()) |nd| {
      if (nd.node.isRet()) {
        if (nd.node.getType()) |ty| {
          uni.set(ty);
        }
      }
    }
    if (uni.variants.count() == 0) {
      return null;
    } else if (uni.variants.count() == 1) {
      return uni.variants.values()[0];
    }
    return uni.toType().box(self.ctx.allocator());
  }

  fn inferFunReturnType(self: *Self, node: *Node, flo: FlowMeta) !*Type {
    if (node.AstFun.is_builtin) {
      // builtin function types are fully well-typed
      return node.AstFun.ret.?.AstNType.typ;
    }
    // TODO: improve error message token - nd.node.getToken()
    var prev_nodes = self.getExitPrevs(flo.exit);
    var has_void_ty = false;
    var has_rec_ty = false;
    var has_non_void_ty = false;
    var has_noreturn_ty = false;
    var has_return_node = false;
    for (prev_nodes.items()) |nd| {
      // skip dead node, since it's most likely at exit
      if (nd.isDeadNode()) {
        continue;
      }
      if (nd.node.isRet()) {
        has_return_node = true;
        if (nd.node.getType()) |ty| {
          if (ty.isRecursive()) {
            has_rec_ty = true;
          } else if (ty.isVoidTy()) {
            has_void_ty = true;
          } else if (ty.isNoreturnTy()) {
            has_noreturn_ty = true;
          } else {
            has_non_void_ty = true;
          }
        } else {
          has_void_ty = true;
        }
      } else if (nd.node.getType()) |ty| {
        if (ty.isNoreturnTy()) {
          has_noreturn_ty = true;
        } else {
          has_void_ty = true;
        }
      } else {
        has_void_ty = true;
      }
    }
    // if we find the function has type noreturn, and there's no return node in the function,
    // simply turn off has_void_ty:
    if (has_noreturn_ty and !has_return_node) has_void_ty = false;
    // When inferring a function's return type:
    // - if we find a recursive type, and a non-void type, use the non-void type as the function's return type
    // - if we find a recursive type, and a void type or only a recursive type, then use the type 'never'
    var uni = Union.init(self.ctx.allocator());
    var void_ty: *Type = self.void_ty;
    var nvr_ty: *Type = if (has_rec_ty) Type.newNever(self.ctx.allocator()) else undefined;
    // unionify all return types at exit
    for (prev_nodes.items()) |nd| {
      // skip dead node, since it's most likely at exit
      if (nd.isDeadNode()) {
        continue;
      }
      var typ = if (!nd.node.isRet()) void_ty else nd.node.getType() orelse void_ty;
      if (!has_void_ty and typ.isVoidTy()) {
        continue;
      }
      if (has_rec_ty) {
        if (has_non_void_ty) {
          if (typ.isRecursive()) {
            continue;
          }
        } else if (has_void_ty or typ.isRecursive()) {
          typ = nvr_ty;
        }
      }
      uni.set(typ);
    }
    if (has_noreturn_ty) uni.set(Type.newConcrete(.TyNoReturn, null).box(self.ctx.allocator()));
    var inf_ret_ty = uni.toType();
    if (node.AstFun.ret) |ret| {
      var ret_ty = ret.AstNType.typ;
      for (prev_nodes.items()) |nd| {
        if (nd.node.isRet()) {
          if (nd.node.AstRet.typ) |typ| {
            if (typ.isRecursive()) {
              continue;
            }
            if (!ret_ty.isRelatedTo(typ, .RCAny, self.ctx.allocator())) {
              return self.error_(
                true, nd.node.getToken(),
                "Expected return type '{s}', but got '{s}'",
                .{self.getTypename(ret_ty), self.getTypename(typ)}
              );
            }
          }
          // TODO: else { what happens here? }
        } else if (!ret_ty.isLikeVoid() and !ret_ty.isNoreturnTy()) {
          // control reaches exit from this node (`nd`), although this node
          // doesn't return anything hence (void), but return type isn't void
          return self.error_(
            true, nd.node.getToken(),
            "Control flow reaches exit from this point without returning type '{s}'",
            .{self.getTypename(ret_ty)}
          );
        }
      }
      if (!ret_ty.isRelatedTo(&inf_ret_ty, .RCAny, self.ctx.allocator())) {
        if (!ret_ty.isNoreturnTy()) {
          return self.error_(
            true, node.getToken(),
            "Expected return type '{s}', but got '{s}'",
            .{self.getTypename(ret_ty), self.getTypename(&inf_ret_ty)}
          );
        } else {
          var token = (
            if (prev_nodes.len() > 0) prev_nodes.itemAt(0).node.getToken()
            else node.getToken()
          );
          return self.error_(
            true, token,
            "Control flow reaches exit; function declared type 'noreturn' returns",
            .{}
          );
        }
      }
      return ret_ty;
    }
    // prefer inferred type for function types, since function types need to have extra
    // meta-data (`node`) which would not be present if the type was created by a user
    return inf_ret_ty.box(self.ctx.allocator());
  }
  
  fn inferCall(self: *Self, node: *ast.CallNode) !*Type {
    var ty = try self.infer(node.expr);
    // check that we're actually calling a function
    if (!ty.isFunction()) {
      return self.error_(
        true, node.expr.getToken(),
        "Expected callable or function type but found '{s}'", .{self.getTypename(ty)}
      );
    }
    var fun_ty = ty.function();
    // perform signature checks
    if (fun_ty.params.len() != node.args.len()) {
       return self.error_(
        true, node.expr.getToken(),
        "Argument mismatch. Expected {} argument(s) but found {}", .{fun_ty.params.len(), node.args.len()}
      );
    }
    // check generic is properly called
    if (node.isGeneric() and !fun_ty.isGeneric()) {
      return self.error_(
        true, node.expr.getToken(),
        "Non-generic function called as generic", .{}
      );
    }
    if (!fun_ty.isGeneric()) {
      var is_cyclic = false;
      if (fun_ty.node) |fun_node| {
        is_cyclic = self.cycles.contains(fun_node, Node.eql);
        if (!fun_node.AstFun.body.AstBlock.checked and !is_cyclic) {
          _ = try self.inferFun(fun_node, ty);
        }
      }
      for (fun_ty.params.items(), node.args.items()) |p_ty, arg| {
        var arg_ty = try self.infer(arg);
        if (!p_ty.isRelatedTo(arg_ty, .RCAny, self.ctx.allocator())) {
          return self.error_(
            true, arg.getToken(),
            "Argument mismatch. Expected type '{s}' but found '{s}'",
            .{self.getTypename(p_ty), self.getTypename(arg_ty)}
          );
        }
        // set extra 'node' metadata for this arg type, which would be null if
        // arg type was created by a user
        if (p_ty.isFunction() and arg.isFun()) {
          p_ty.function().node = arg;
        }
      }
      // [N*]: check if we're currently resolving this type, i.e. if it's recursive
      if (is_cyclic) {
        if (self.inferFunReturnTypePartial(self.cfg.lookup(fun_ty.node.?).?)) |typ| {
          return typ;
        }
        return ty.newRecursive().box(self.ctx.allocator());
      }
      node.typ = fun_ty.ret;
      return fun_ty.ret;
    }
    if (node.targs) |targs| {
      if (targs.len() != fun_ty.tparams.?.len()) {
        return self.error_(
          true, node.expr.getToken(),
          "Generic function not instantiated correctly. Expected {} type parameter(s), but found {}",
          .{fun_ty.tparams.?.len(), targs.len()}
        );
      }
    }
    self.ctx.enterScope();
    errdefer self.ctx.leaveScope();
    // generic, so monomorphize.
    var args_inf = TypeList.init(self.ctx.allocator());
    for (node.args.items()) |arg| {
      args_inf.append(try self.infer(arg));
    }
    var old_fun_node = fun_ty.node.?;
    var fun = &old_fun_node.AstFun;
    // inferred type arguments must match the generic type params if specified
    // ex: foo{str, num}('a', 5)
    if (node.targs) |targs| {
      for (fun.tparams.?.items(), 0..) |tvar, tpos| {
        for (fun.params.items(), 0..) |param, ppos| {
          var typ = param.ident.typ.?;
          if (typ.typeidEql(tvar)) {
            if (!targs.itemAt(tpos).getType().?.typeidEql(args_inf.itemAt(ppos))) {
              return self.error_(
                true, node.args.itemAt(ppos).getToken(),
                "Type parameter mismatch. Expected type '{s}', but found '{s}'",
                .{self.getTypename(targs.itemAt(tpos).getType().?), self.getTypename(args_inf.itemAt(ppos))}
              );
            }
          }
        }
      }
      for (fun_ty.tparams.?.items(), targs.items()) |tvar, nd| {
        self.insertType(tvar, nd.getType().?);
      }
    } else {
      for (fun.tparams.?.items()) |tvar| {
        var resolved = false;
        for (fun.params.items(), 0..) |param, ppos| {
          var typ = param.ident.typ.?;
          if (typ.typeidEql(tvar)) {
            self.insertType(tvar, args_inf.itemAt(ppos));
            resolved = true;
            break;
          }
        }
        if (!resolved) {
          return self.error_(
            true, tvar.variable().tokens.itemAt(0),
            "Type variable '{s}' must be explicitly specified if not used with an argument",
            .{self.getTypename(tvar)}
          );
        }
      }
    }
    self.linker.ctx.typScope = self.ctx.typScope;
    // link the function
    // lookup node using inferred args.
    // - if found just return the ret type of the found node
    // - else, do the stuff below, and cache the node
    var synth_name = self.createSynthName(fun, &args_inf, node.targs);
    if (self.findFnInfo(old_fun_node, synth_name)) |info| {
      node.expr.setType(info.typ);
      self.ctx.leaveScope();
      return info.typ.function().ret;
    }
    var new_fun_node = fun.clone(self.ctx.allocator());
    var newfun = &new_fun_node.AstFun;
    // keep tparams to ban use as alias in types contained in fun, as much as possible
    try self.linker.linkFun(newfun, true);
    // for some reason, `self.ctx.typScope` sometimes becomes invalidated after linking fun, so reassign again 
    self.ctx.typScope = self.linker.ctx.typScope;
    // make fun non-generic to allow building of the cfg
    newfun.tparams = null;
    try self.buildFunFlow(new_fun_node);
    self.ctx.varScope.pushScope();
    errdefer self.ctx.varScope.popScope();
    var new_fun_ty = try self.inferFunPartial(new_fun_node);
    for (new_fun_ty.function().params.items(), 0..) |typ, ppos| {
      if (typ.isFunction() and node.args.itemAt(ppos).isFun()) {
        typ.function().node = node.args.itemAt(ppos);
      }
    }
    // validate monomorphized function params
    for (new_fun_ty.function().params.items(), node.args.items()) |p_ty, arg| {
      var arg_ty = try self.infer(arg);
      if (!p_ty.isRelatedTo(arg_ty, .RCAny, self.ctx.allocator())) {
        return self.error_(
          true, arg.getToken(),
          "Argument mismatch. Expected type '{s}' but found '{s}'",
          .{self.getTypename(p_ty), self.getTypename(arg_ty)}
        );
      }
    }
    var typ = try self.inferFun(new_fun_node, new_fun_ty);
    node.typ = typ.function().ret;
    node.expr.setType(new_fun_ty);
    self.ctx.varScope.popScope();
    self.ctx.leaveScope();
    self.addFnInfo(
      old_fun_node,
      new_fun_node,
      self.boxSynthName(old_fun_node.getToken(), synth_name),
      typ
    );
    return node.typ.?;
  }

  fn inferRet(self: *Self, node: *ast.RetNode) !*Type {
    if (node.expr) |expr| {
      node.typ = try self.infer(expr);
    } else {
      node.typ = self.void_ty;
    }
    return node.typ.?;
  }

  fn inferError(self: *Self, node: *ast.ErrorNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    var ty = try self.infer(node.expr);
    if (ty.isErrorTy()) {
      return self.error_(
        true, node.expr.getToken(),
        "nested error types are unsupported: '{s}'", .{self.getTypename(ty)}
      );
    }
    var al = self.ctx.allocator();
    var base = Type.newConcrete(.TyClass, "err").box(al);
    var typ = Type.newGeneric(al, base).box(al);
    typ.generic().append(ty);
    node.typ = typ;
    return typ;
  }

  fn inferOrElse(self: *Self, node: *Node) !*Type {
    // - build cfg of this node. 
    var exit = if (self.current_fn) |curr| curr.exit else self.cfg.program.exit;
    var builder = CFGBuilder.initWithExit(self.ctx.allocator(), exit);
    var flo = builder.buildOrElse(&self.cfg, node);
    var ok_ty = try self.infer(node.AstOrElse.ok);
    return try self.checkOrElse(node, ok_ty, flo);
  }

  fn inferProgram(self: *Self, node: *ast.ProgramNode) !*Type {
    self.ctx.enterScope();
    for (node.decls.items()) |item| {
      _ = self.infer(item) catch undefined;
    }
    self.ctx.leaveScope();
    // crash and burn
    return undefined;
  }

  fn checkCast(self: *Self, node_ty: *Type, cast_ty: *Type, debug: Token, emit: bool) TypeCheckError!*Type {
    var ty = node_ty.canBeCastTo(cast_ty, self.ctx.allocator()) catch |e| {
      if (e == error.UnionCastError) {
        var active = if (node_ty.isUnion()) self.getTypename(node_ty.union_().active.?) else "different";
        return self.error_(
          emit, debug,
          "Cannot cast from type '{s}' to type '{s}' because the active type is '{s}'",
          .{self.getTypename(node_ty), self.getTypename(cast_ty), active}
        );
      }
      return self.error_(
        emit, debug,
        "Cannot cast from type '{s}' to type '{s}'",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    };
    if (ty == node_ty) {
      self.warn(
        emit, debug,
        "Could not cast from type '{s}' to type '{s}' because the active type is unknown",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    } 
    return ty;
  }

  fn checkAssign(self: *Self, target: *Type, source: *Type, debug: Token, emit: bool) !*Type {
    var typ = target.canBeAssigned(source, self.ctx.allocator());
    if (typ == null) {
      return self.error_(
        emit, debug,
        "Cannot assign type '{s}' to type '{s}'",
        .{self.getTypename(source), self.getTypename(target)}
      );
    } 
    return typ.?;
  }

  fn checkNil(self: *Self, node: *ast.LiteralNode, typ: *Type) !void {
    _ = typ;
    return self.error_(true, node.token, "Should not be checking nil",  .{});
  }

  fn checkUnary(self: *Self, node: *ast.UnaryNode, expected: *Type) !void {
    if (node.typ.?.typeid() != expected.typeid()) {
      const op = node.op.token.value;
      return self.error_(
        true, node.op.token,
        "Expected type {s} '{s}', but got {s} '{s}'",
        .{op, self.getTypename(expected), op, self.getTypename(node.typ.?)}
      );
    }
  }

  fn checkBinary(self: *Self, node: *ast.BinaryNode, source: *Type, narrowed: bool) !void {
    // source is type of rhs
    // node.typ is type of lhs
    if (node.op.optype == .OpEqq or node.op.optype == .OpNeq or node.op.optype == .OpIs) {
      if (!node.typ.?.isEitherWayRelatedTo(source, .RCAny, self.ctx.allocator())) {
        return self.error_(
          true, node.op.token,
          "types must be related for comparison.{s}'{s}' is not related to '{s}'",
          .{
            if (!narrowed) " " else " Narrowed type ",
            self.getTypename(node.typ.?), self.getTypename(source),
          }
        );
      }
      return;
    }
    if (node.op.optype == .OpAnd or node.op.optype == .OpOr) {
      node.typ = node.typ.?.unionify(source, self.ctx.allocator());
      return;
    }
    var errTy: ?*Type = null;
    if (!node.typ.?.typeidEql(&UnitTypes.tyNumber)) {
      errTy = node.typ;
    } else if (!source.typeidEql(&UnitTypes.tyNumber)) {
      errTy = source;
    }
    if (errTy != null) {
      const name = self.getTypename(&UnitTypes.tyNumber);
      const op = node.op.token.value;
      return self.error_(
        true, node.op.token,
        "Expected type '{s}' {s} '{s}', but got '{s}' {s} '{s}'",
        .{
          name, op, name,
          self.getTypename(node.typ.?), op, self.getTypename(source)
        }
      );
    }
  }

  fn checkSubscript(self: *Self, node: *ast.SubscriptNode, expr_ty: *Type, index_ty: *Type) !void {
    var token = node.index.getToken();
    if (!expr_ty.isListTy() and !expr_ty.isMapTy() and !expr_ty.isTupleTy()) {
      return self.error_(
        true, token,
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.generic().tparams_len() == 0) {
      return self.error_(
        true, token,
        "Cannot index empty or non-specialized '{s}' type", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.isListTy() or expr_ty.isTupleTy()) {
      if (!index_ty.isNumTy()) {
        return self.error_(
          true, token,
          "Cannot index '{s}' type with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      }
      node.typ = expr_ty.generic().tparams.itemAt(0);
    } else if (expr_ty.isMapTy()) {
      // k-v. index with k, get v.
      var gen = expr_ty.generic();
      std.debug.assert(gen.tparams_len() == 2);
      var key_typ = gen.tparams.itemAt(0);
      var val_typ = gen.tparams.itemAt(1);
      _ = self.checkAssign(key_typ, index_ty, node.index.getToken(), false) catch {
        return self.error_(
          true, token,
          "Cannot index type '{s}' with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      };
      node.typ = val_typ;
    }
  }

  fn checkDeref(self: *Self, node: *ast.DerefNode, expr_ty: *Type) !void {
    if (!expr_ty.isNullable()) {
      return self.error_(
        true, node.token,
        "Cannot dereference non-nullable type: '{s}'",
        .{self.getTypename(expr_ty)}
      );
    }
    node.typ = expr_ty.subtype(self.ctx.allocator());
  }

  fn checkCondition(self: *Self, cond_ty: *Type, debug: Token) !void {
    if (!cond_ty.isBoolTy()) {
      return self.error_(
        true, debug,
        "Expected condition expression to be of type 'bool', but got '{s}'",
        .{self.getTypename(cond_ty)}
      );
    }
  }

  fn excludeError(self: *Self, typ: *Type, debug: Token) !*Type {
    var errors: usize = 0;
    var uni = Union.init(self.ctx.allocator());
    for (typ.union_().variants.values()) |ty| {
      if (ty.isErrorTy()) {
        errors += 1;
      } else {
        uni.set(ty);
      }
      if (errors > 1) {
        return self.error_(
          true, debug, "error unions with multiple error types are unsupported: '{s}'",
          .{self.getTypename(typ)}
        );
      }
    }
    return uni.toType().box(self.ctx.allocator());
  }

  fn checkOrElse(self: *Self, node: *Node, ok_ty: *Type, flo: FlowMeta) !*Type {
    // - check that try is used with an error union type
    // - check that the type on `ok` and `err` are related
    var oe = &node.AstOrElse;
    var debug = oe.ok.getToken();
    if (!ok_ty.isErrorUnion()) {
      var help = (
        if (ok_ty.isNullable())
          "\n\tHelp: nullable types take precedence over error types"
        else ""
      ); 
      return self.error_(
        true, debug,
        "Expected error union type in 'try' expression. Type '{s}' is not an error union{s}",
        .{self.getTypename(ok_ty), help}
      );
    }
    var typ = try self.excludeError(ok_ty, debug);
    // if excludeError() doesn't err, it's safe to say this type has only one error type
    self.ctx.varScope.pushScope();
    errdefer self.ctx.varScope.popScope();
    if (oe.evar) |evar| {
      for (ok_ty.union_().variants.values()) |ty| {
        if (ty.isErrorTy()) {
          evar.typ = ty;
          self.insertVar(evar.token.value, ty);
          break;
        }
      }
    }
    try self.flowInfer(flo.entry, true);
    var err_ty = if (oe.err.isBlock()) self.void_ty else oe.err.getType().?;
    if (oe.from_try and oe.err.isRet()) {
      // TODO
    } else if (!typ.isRelatedTo(err_ty, .RCAny, self.ctx.allocator())) {
      return self.error_(
        true, debug,
        "type on both sides of 'orelse' must be related.\n\t"
        ++ "'{s}' is not related to '{s}'",
        .{self.getTypename(typ), self.getTypename(err_ty)}
      );
    }
    self.ctx.varScope.popScope();
    oe.typ = typ;
    return typ;
  }

  fn infer(self: *Self, node: *Node) TypeCheckError!*Type {
    return switch (node.*) {
      .AstNumber => |*nd| try self.inferNumber(nd),
      .AstString => |*nd| try self.inferString(nd),
      .AstBool => |*nd| try self.inferBool(nd),
      .AstUnary => |*nd| try self.inferUnary(nd),
      .AstBinary => |*nd| try self.inferBinary(nd),
      .AstList => |*nd| try self.inferList(nd),
      .AstTuple => |*nd| try self.inferTuple(nd),
      .AstMap => |*nd| try self.inferMap(nd),
      .AstExprStmt => |*nd| try self.inferExprStmt(nd),
      .AstVar => |*nd| try self.inferVar(nd, true),
      .AstVarDecl => |*nd| try self.inferVarDecl(nd),
      .AstAssign => |*nd| try self.inferAssign(nd),
      .AstBlock => |*nd| try self.inferBlock(nd),
      .AstNType => |*nd| try self.inferNType(nd),
      .AstAlias => |*nd| try self.inferAlias(nd),
      .AstNil => |*nd| try self.inferNil(nd),
      .AstCast => |*nd| try self.inferCast(nd),
      .AstSubscript => |*nd| try self.inferSubscript(nd),
      .AstDeref => |*nd| try self.inferDeref(nd),
      .AstWhile => |*nd| try self.inferWhile(nd),
      .AstCall => |*nd| try self.inferCall(nd),
      .AstRet => |*nd| try self.inferRet(nd),
      .AstFun => try self.inferFun(node, null),
      .AstError => |*nd| try self.inferError(nd),
      .AstOrElse => try self.inferOrElse(node),
      .AstProgram => |*nd| try self.inferProgram(nd),
      .AstIf, .AstElif, .AstSimpleIf,
      .AstCondition, .AstEmpty, .AstControl => return undefined,
    };
  }

  pub fn buildFunFlow(self: *Self, node: *Node) !void {
    self.builder.buildFun(&self.cfg, node);
    var flo = self.cfg.lookup(node).?;
    try self.analyzer.analyzeDeadCode(flo.dead);
  }

  fn buildProgramFlow(self: *Self, root: *Node) !void {
    self.cfg = self.builder.build(root);
    self.analyzer.analyzeDeadCode(self.cfg.program.dead) catch |e| {
      self.diag.display();
      return e;
    };
  }

  pub fn typecheck(self: *Self, node: *Node, va: *VebAllocator) TypeCheckError!void {
    var linker = TypeLinker.init(self.ctx.allocator(), self.diag);
    self.loadBuiltinsPrelude(&linker, va);
    linker.linkTypes(node) catch |e| {
      return e;
    };
    self.linker = &linker;
    self.ctx.typScope = linker.ctx.typScope;
    try self.buildProgramFlow(node);
    self.flowInferEntry(self.cfg.program.entry) catch {
      // just stack up as much errors as we can from here.
      var nodes = self.cfg.program.entry.getOutgoingNodes(.ESequential, self.ctx.allocator());
      for (nodes.items()) |flo| {
        if (flo.res.isResolved()) continue;
        self.flowInfer(flo, false) catch {};
      }
    };
    self.analyzer.analyzeDeadCodeWithTypes(self.cfg.program.entry) catch {};
    if (self.diag.hasAny()) {
      self.diag.display();
      return error.CheckError;
    }
  }
};
