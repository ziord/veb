pub const std = @import("std");
pub const ast = @import("ast.zig");
pub const link = @import("link.zig");
pub const util = @import("util.zig");
const diagnostics = @import("diagnostics.zig");
const flow = @import("flow.zig");
const TypeHashMap = std.StringHashMap(*Type);
const FlowNode = flow.FlowNode;
const FlowList = flow.FlowList;
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
const TypeList = types.TypeList;
const Diagnostic = diagnostics.Diagnostic;

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
          orelse Type.newNever(allocator, t1.debug).box(allocator)
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
          // TODO: log this
          break :blk (
            if (t1.typeidEql(t2)) Type.newNever(allocator, t2.debug).box(allocator)
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
  cfg: *FlowNode = undefined,
  diag: Diagnostic,

  const UnitTypes = struct {
    const num = types.Concrete.init(.TyNumber);
    const str = types.Concrete.init(.TyString);
    const bol = types.Concrete.init(.TyBool);
    const nil = types.Concrete.init(.TyNil);
    const tyty = types.Concrete.init(.TyType);

    const tyNumber: Type = Type.init(.{.Concrete = num}, Token.getDefault());
    const tyString: Type = Type.init(.{.Concrete = str}, Token.getDefault());
    const tyBool: Type = Type.init(.{.Concrete = bol}, Token.getDefault());
    const tyNil: Type = Type.init(.{.Concrete = nil}, Token.getDefault());
    const TyTy: Type = Type.init(.{.Concrete = tyty}, Token.getDefault());
  };
  const Self = @This();
  const MAX_STRING_SYNTH_LEN = 0xc;
  pub const TypeCheckError = error{TypeCheckError};

  pub fn init(allocator: std.mem.Allocator, filename: *const[]const u8, src: *[]const u8) @This() {
    return Self {
      .ctx = TContext.init(allocator, filename, src),
      .diag = Diagnostic.init(allocator, filename, src),
    };
  }

  fn error_(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) self.diag.addDiagnostics(.DiagError, token, "TypeError: " ++ fmt, args);
    return error.TypeCheckError;
  }

  inline fn warn(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) void {
    if (emit) self.diag.addDiagnostics(.DiagWarn, token, "TypeWarning: " ++ fmt, args);
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
        .Concrete, .Variable => ty,
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

  pub inline fn insertType(self: *Self, name: []const u8, ty: *Type) void {
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
    var tok = Token.tokenFrom(@constCast(&token));
    tok.value = val;
    return .{.AstString = ast.LiteralNode.init(tok)};
  }

  /// copy env to varScope
  fn copyEnv(self: *Self, env: *TypeEnv) void {
    var itr = env.global.current().map.iterator();
    while (itr.next()) |entry| {
      self.insertType(entry.key_ptr.*, entry.value_ptr.*);
    }
    itr = env.narrowed.iterator();
    while (itr.next()) |entry| {
      self.insertType(entry.key_ptr.*, entry.value_ptr.*);
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
  const NarrowError = error{TypeCheckError, SynthFailure, SynthTooLarge};

  fn synthesizeVar(self: *Self, vr: *ast.VarNode, other: ?*Node, env: *TypeEnv) NarrowError!ast.VarNode {
    if (vr.typ == null) _ = try self.inferVar(vr, false);
    if (other == null) return vr.*;
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
        return ret;
      },
      else => |els| {
        std.debug.print("unexpected ast type for synth {}", .{els});
        return error.SynthFailure;
      }
    }
  }

  fn synthesizeSubscript(self: *Self, node: *ast.SubscriptNode, env: *TypeEnv, assume_true: bool) NarrowError!ast.VarNode {
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
      node.narrowed = .{.token = undefined, .typ = null};
    }
    var ty = try self.inferSubscript(node);
    var narrowed = try self.synthesize(node.expr, env, assume_true);
    narrowed = try self.synthesizeVar(&narrowed, node.index, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }
  
  fn synthesizeDeref(self: *Self, node: *ast.DerefNode, env: *TypeEnv, assume_true: bool) NarrowError!ast.VarNode {
    // synthesize node.expr
    try self.narrow(node.expr, env, assume_true);
    // similar to what inferSubscript() does here.
    var change = (node.narrowed == null);
    if (change) {
      // similar to what inferSubscript() does here.
      node.narrowed = .{.token = undefined, .typ = null};
    }
    var ty = try self.inferDeref(node);
    var narrowed = try self.synthesize(node.expr, env, assume_true);
    var nil = self.newStringNode("?", node.token);
    narrowed = try self.synthesizeVar(&narrowed, &nil, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }

  fn synthesize(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) NarrowError!ast.VarNode {
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
      self.insertType(node.token.value, ty);
      node.typ = ty;
    } else if (env.getGlobal(node.token.value)) |ty| {
      self.insertType(node.token.value, ty);
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
        try self.narrowVariable(&vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertType(vr.token.value, vr.typ.?);
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
        try self.narrowVariable(&vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertType(vr.token.value, vr.typ.?);
      }
    } else {
      try self.narrow(node.expr, env, assume_true);
      _ = try self.inferDeref(node);
    }
  }

  fn narrowBinary(self: *Self, node: *ast.BinaryNode, env: *TypeEnv, assume_true: bool) !void {
    if (node.op.optype == .OpIs) {
      if (node.left.isVariable()) {
        try self.narrowVariable(&node.left.AstVar, env);
        _ = try self.inferBinary(node);
        // if we get here, then right must be a TypeNode
        var ty = try self.lookupName(&node.left.AstVar, true);
        if (Type.is(ty, &node.right.AstNType.typ, self.ctx.allocator())) |is_ty| {
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
          synth.left = &.{.AstVar = vr};
          try self.narrowBinary(&synth, env, assume_true);
          node.typ = synth.typ;
          return;
        }
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
    } else if (self.canNarrow(node.left) and node.right.isNilLiteral()) {
      var token = node.right.AstNil.token;
      var tmp = self.newAstNode();
      tmp.* = .{.AstNType = ast.TypeNode.init(Type.newConcrete(.TyNil, null, token), token)};
      var bin = b: {
        if (node.op.optype == .OpEqq or node.op.optype == .OpNeq) {
          var bin = node.*;
          bin.op.optype = .OpIs;
          bin.right = tmp;
          break :b bin;
        }
        break :b undefined;
      };
      if (node.op.optype == .OpEqq) {
        try self.narrowBinary(&bin, env, assume_true);
        node.typ = bin.typ;
        return;
      } else if (node.op.optype == .OpNeq) {
        // rewrite to unary(binary(expr))
        var op = ast.Token.tokenFrom(&node.op.token);
        op.ty = .TkExMark;
        var una = ast.UnaryNode.init(&.{.AstBinary = bin}, op);
        try self.narrowUnary(&una, env, assume_true);
        node.typ = una.typ;
        return;
      }
    }
    try self.narrow(node.left, env, assume_true);
    try self.narrow(node.right, env, assume_true);
    _ = try self.inferBinary(node);
  }

  fn narrowUnary(self: *Self, node: *ast.UnaryNode, env: *TypeEnv, assume_true: bool) !void {
    if (node.op.optype == .OpNot) {
      try self.narrow(node.expr, env, !assume_true);
      // at this point, node.expr typechecked successfully
      node.typ = UnitTypes.bol.toType(node.op.token).box(self.ctx.allocator());
    } else {
      try self.narrow(node.expr, env, assume_true);
      _ = try self.inferUnary(node);
    }
  }

  fn narrow(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) NarrowError!void {
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
  fn flowInferEntry(self: *Self) !void {
    var node = self.cfg;
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

  fn flowInferExit(self: *Self, node: *FlowNode, inferNext: bool) !void {
    _ = inferNext;
    std.debug.assert(node.tag == .CfgExit);
    node.res = .Resolved;
    self.ctx.leaveScope();
    // assert no more nodes after exit
    std.debug.assert(node.prev_next.count(FlowNode.isNext) == 0);
  }

  fn flowInferNode(self: *Self, node: *FlowNode, inferNext: bool) !void {
    _ = inferNext;
    _ = try self.infer(node.node);
    node.res = .Resolved;
  }

  fn flowInferCondition(self: *Self, node: *FlowNode, inferNext: bool) !void {
    _ = inferNext;
    node.res = .Processing;
    // TODO: is there a need to explicitly merge types from incoming edges?
    self.ctx.varScope.pushScope();
    // This is a branch point and a meet point. 
    // As a meet point, all types on the incoming edges are merged.
    // As a branch point, types are narrowed along outgoing edges based 
    // on the condition expression.
    var env = TypeEnv.init(self.ctx.allocator(), null);
    env.global.pushScope();
    self.narrow(node.node.AstCondition.cond, &env, true) catch return error.TypeCheckError;
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
    self.ctx.varScope.popScope();
  }

  fn flowInfer(self: *Self, node: *FlowNode, inferNext: bool) TypeCheckError!void {
    if (node.res.isResolved()) return;
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
      .AstExprStmt => try self.flowInferNode(node, inferNext),
      .AstVarDecl => try self.flowInferNode(node, inferNext),
      .AstCondition => try self.flowInferCondition(node, inferNext),
      .AstEmpty => try self.flowInferExit(node, inferNext),
      .AstControl => {},
      else => unreachable,
    }
    if (!inferNext) return;
    for (node.prev_next.items()) |item| {
      if (item.next) {
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
      node.typ = kind.toType(node.token).box(self.ctx.allocator());
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
    var base = Type.newConcrete(.TyClass, name, node.token).box(al);
    node.typ = Type.newGeneric(al, base, node.token).box(al);
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
    gen.append(Type.compressTypes(&typeset, node.token, null));
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
    var base = Type.newConcrete(.TyClass, "map", node.token).box(al);
    node.typ = Type.newGeneric(al, base, node.token).box(al);
    if (node.pairs.len() == 0) {
      return node.typ.?;
    }
    // infer type of items stored in the map
    var first_pair = node.pairs.items()[0];
    var key_typ = try self.infer(first_pair.key);
    var val_typ = try self.infer(first_pair.value);

    if (node.pairs.len() > 1) {
      for (node.pairs.items()[1..], 1..) |pair, i| {
        _ = i;
        var typ = try self.infer(pair.key);
        _ = self.checkAssign(key_typ, typ, typ.debug, false) catch {
          return self.error_(
            true, typ.debug,
            "expected key type '{s}', but found '{s}'",
            .{self.getTypename(key_typ), self.getTypename(typ)}
          );
        };
        typ = try self.infer(pair.value);
        _ = self.checkAssign(val_typ, typ, typ.debug, false) catch {
          return self.error_(
            true, typ.debug,
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
      try self.checkUnary(node, @constCast(&UnitTypes.tyNumber));
    } else {
      std.debug.assert(node.op.optype == .OpNot);
      // `!` accepts any type and returns a boolean. 
      // It applies an implicit bool cast to such a type.
      node.typ = UnitTypes.bol.toType(node.typ.?.debug).box(self.ctx.allocator());
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
      node.typ = UnitTypes.bol.toType(lhsTy.debug).box(self.ctx.allocator());
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
        if (rhsTy.isLikeConstant())
          "\nHelp: For constant types, consider using '==' or '!=' operator instead."
        else ""
      );
      return self.error_(
        true, node.op.token,
        "Expected type 'Type' in rhs of `is` operator but found type '{s}'{s}",
        .{self.getTypename(rhsTy), help}
      );
    }
    // at this point, rhs is a TypeNode
    var ty = &node.right.AstNType.typ;
    if (!ty.isConcrete() and ty.isGeneric() and ty.generic().tparams_len() != 0) {
      return self.error_(
        true, node.op.token,
        "Expected a concrete/class type in rhs of `is` operator but found '{s}'",
        .{self.getTypename(ty)}
      );
    }
    // temporarily assign lhsTy for checking
    node.typ = lhsTy;
    // use the actual type on rhs for checks
    try self.checkBinary(node, ty, true);
    // `is` returns type bool, so reassign
    node.typ = UnitTypes.bol.toType(lhsTy.debug).box(self.ctx.allocator());
    return node.typ.?;
  }

  fn inferVar(self: *Self, node: *ast.VarNode, emit: bool) !*Type {
    // TODO: Do we need to always fetch the updated type?
    // if (node.typ) |typ| return typ;
    node.typ = try self.lookupName(node, emit);
    return node.typ.?;
  }

  fn inferCast(self: *Self, node: *ast.CastNode) !*Type {
    var typ = try self.infer(node.expr);
    var cast_typ = &node.typn.typ;
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
      .AstVar => |ident| self.insertType(ident.token.value, typ),
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
      return @constCast(&UnitTypes.TyTy);
    }
    return &node.typ;
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
    if (node.ident.typ) |typ| {
      var expr_ty = try self.infer(node.value);
      node.ident.typ = typ;
      _ = try self.checkAssign(typ, expr_ty, node.ident.token, true);
    } else {
      node.ident.typ = try self.infer(node.value);
    }
    self.insertType(node.ident.token.value, node.ident.typ.?);
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
      node.narrowed = self.synthesizeSubscript(node, &env, true) catch return error.TypeCheckError;
      if (node.narrowed) |*narrowed| {
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
      node.narrowed = self.synthesizeDeref(node, &env, true) catch return error.TypeCheckError;
      if (node.narrowed) |*narrowed| {
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
    // crash and burn
    return undefined;
  }

  fn inferWhile(self: *Self, node: *ast.WhileNode) !*Type {
    var ty = try self.infer(node.cond);
    try self.checkCondition(ty, node.cond.getToken());
    return try self.infer(node.then);
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

  fn checkAssign(self: *Self, target: *Type, source: *Type, debug: ?Token, emit: bool) !*Type {
    var typ = target.canBeAssigned(source, self.ctx.allocator());
    if (typ == null) {
      return self.error_(
        emit, if (debug) |deb| deb else source.debug,
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
    if (node.typ.?.typeid() != @constCast(&UnitTypes.tyNumber).typeid()) {
      errTy = node.typ;
    } else if (source.typeid() != @constCast(&UnitTypes.tyNumber).typeid()) {
      errTy = source;
    }
    if (errTy != null) {
      const name = self.getTypename(@constCast(&UnitTypes.tyNumber));
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
      node.typ = expr_ty.generic().tparams.items()[0];
    } else if (expr_ty.isMapTy()) {
      // k-v. index with k, get v.
      var gen = expr_ty.generic();
      std.debug.assert(gen.tparams_len() == 2);
      var key_typ = gen.tparams.items()[0];
      var val_typ = gen.tparams.items()[1];
      _ = self.checkAssign(key_typ, index_ty, index_ty.debug, false) catch {
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
      .AstProgram => |*nd| try self.inferProgram(nd),
      .AstIf, .AstElif, .AstSimpleIf,
      .AstCondition, .AstEmpty, .AstControl => return undefined,
    };
  }

  pub fn typecheck(self: *Self, node: *Node) TypeCheckError!void {
    var linker = link.TypeLinker.init(self.ctx.allocator(), self.ctx.filename, self.ctx.src);
    linker.linkTypes(node) catch {
      return error.TypeCheckError;
    };
    self.ctx.typScope = linker.ctx.typScope;
    var builder = CFGBuilder.init(node, self.ctx.allocator());
    self.cfg = builder.build();
    self.flowInferEntry() catch {
      // just stack up as much errors as we can from here.
      _ = self.infer(node) catch undefined;
    };
    if (self.diag.hasAny()) {
      self.diag.display();
      return error.TypeCheckError;
    }
  }
};
