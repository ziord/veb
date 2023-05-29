const std = @import("std");
const ast = @import("ast.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");
pub const types = @import("type.zig");
pub const Token = @import("lex.zig").Token;

pub const Type = types.Type;
pub const Union = types.Union;
pub const TypeKind = types.TypeKind;
pub const TypeInfo = types.TypeInfo;
pub const Generic = types.Generic;
pub const Variable = types.Variable;
pub const Recursive = types.Recursive;
pub const Node = ast.AstNode;

fn CreateMap(comptime K: type, comptime V: type) type {
  return struct {
    map: Map,

    const Map = std.StringHashMap(V);

    pub fn init(allocator: std.mem.Allocator) @This() {
      return @This() {.map = Map.init(allocator)};
    }

    pub fn put(self: *@This(), key: K, value: V) void {
      self.map.put(key, value) catch |e| {
        std.debug.print("Error: {}\n", .{e});
      };
    }

    pub fn get(self: *@This(), key: K) ?V {
      return self.map.get(key);
    }

    pub fn del(self: *@This(), key: K) bool {
      return self.map.remove(key);
    }
  };
}

pub fn GenScope(comptime K: type, comptime V: type) type {
  return struct {
    decls: ds.ArrayList(ScopeMap),
    allocator: std.mem.Allocator,
    const ScopeMap = CreateMap(K, V);

    pub fn init(allocator: std.mem.Allocator) @This(){
      return @This() {.decls = ds.ArrayList(ScopeMap).init(allocator), .allocator = allocator};
    }

    pub fn pushScope(self: *@This()) void {
      self.decls.append(ScopeMap.init(self.allocator));
    }

    pub fn popScope(self: *@This()) void {
      if (self.decls.len() == 0) {
        util.error_("pop from empty scope-list", .{});
      }
      _ = self.decls.pop();
    }

    pub fn popScopes(self: *@This(), count: usize) void {
      if (self.decls.len() == 0) {
        util.error_("pop from empty scope-list", .{});
      }
      for (0..count) |_| {
        if (self.len() > 0) {
          _ = self.decls.pop();
        }
      }
    }

    pub fn current(self: *@This()) ScopeMap {
      return self.decls.getLast();
    }

    pub inline fn len(self: *@This()) usize {
      return self.decls.len();
    }

    pub fn lookup(self: *@This(), name: K) ?V {
      if (self.len() == 0) return null;
      var i: usize = self.len();
      while (i > 0): (i -= 1) {
        var map = self.decls.items()[i - 1];
        if (map.get(name)) |ty| {
          return ty;
        }
      }
      return null;
    }

    pub fn remove(self: *@This(), name: K) void {
      if (self.len() == 0) return;
      var i: usize = self.len();
      while (i > 0): (i -= 1) {
        var map = self.decls.items()[i - 1];
        if (map.del(name)) {
          return;
        }
      }
    }

    pub fn insert(self: *@This(), name: K, ty: V) void {
      if (self.len() == 0) {
        util.error_("insert into empty scope-list", .{});
      }
      self.decls.items()[self.len() - 1].put(name, ty);
    }

    pub fn clear(self: *@This()) void {
      while (self.decls.len() > 0) {
        _ = self.decls.pop();
      }
    }
  };
}

fn CreateTContext(comptime TypScope: type, comptime VarScope: type) type {
  return struct {
    allocator: std.mem.Allocator,
    /// type scope
    typScope: TypScope,
    /// scope for other declarations, e.g. variables, functions, etc.
    varScope: VarScope,
    filename: []const u8,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, filename: []const u8) Self {
      return Self {
        .allocator = allocator, 
        .typScope = TypScope.init(allocator), 
        .varScope = VarScope.init(allocator),
        .filename = filename,
      };
    }

    pub inline fn newType(self: *Self, kind: TypeInfo, debug: Token) *Type {
      return util.box(Type, Type.init(kind, debug), self.allocator);
    }

    pub inline fn enterScope(self: *Self) void {
      self.typScope.pushScope();
      self.varScope.pushScope();
    }

    pub inline fn leaveScope(self: *Self) void {
      self.typScope.popScope();
      self.varScope.popScope();
    }

    pub fn copyType(self: *Self, typ: *Type) *Type {
      // we need to deepcopy typ
      var new = typ.clone(self.allocator);
      return new;
    }
  };
}

pub const Scope = GenScope([]const u8, *Type);
pub const TContext = CreateTContext(Scope, Scope);

pub const TypeLinker = struct {
  ctx: TContext,
  /// track each substitution/resolution steps
  sub_steps: usize = 0,
  /// track if a Variable type is from a generic parameter type substitution 
  using_tvar: usize = 0,
  /// scope for cycle detection
  cyc_scope: PairScope,
  /// resolving an infinite/recursive type?
  in_inf: bool = false,

  const MultiPair = struct {setter: *Type, key: *Type, value: *Type};
  const PairScope = GenScope([]const u8, MultiPair);
  const TypeLinkError = error{TypeLinkError};
  const MAX_DEPTH = 0x64;
  pub const MAX_SUB_STEPS = types.MAX_RECURSIVE_DEPTH;

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator, filename: []const u8) @This() {
    return Self { .ctx = TContext.init(allocator, filename), .cyc_scope = PairScope.init(allocator) };
  }

  fn error_(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) TypeLinkError {
    token.showError(self.ctx.filename, "TypeError: " ++ fmt, args);
    return error.TypeLinkError;
  }

  fn insertTVar(self: *Self, typ: *Type, data: MultiPair) void {
    var tvar = typ.variable();
    if (tvar.tokens.len() > 1) return;
    var name = tvar.tokens.items()[0].value;
    self.cyc_scope.insert(name, data);
  }

  fn checkTVar(self: *Self, typ: *Type, found: *Type) ?*Type {
    var tvar = typ.variable();
    if (tvar.tokens.len() > 1) return null;
    var name = tvar.tokens.items()[0].value;
    if (self.cyc_scope.lookup(name)) |pair| {
      // extra p.o.c;
      if (pair.key == found) {
        return pair.value;
      }
    }
    return null;
  }

  fn delTVar(self: *Self, typ: *Type) void {
    var tvar = typ.variable();
    if (tvar.tokens.len() > 1) return;
    var name = tvar.tokens.items()[0].value;
    if (self.cyc_scope.lookup(name)) |pair| {
      // only delete this pair if `typ` is its exact setter
      if (pair.setter == typ) {
        self.cyc_scope.remove(name);
      }
    }
  }

  inline fn lookupVarType(self: *Self, typ: *Type) ?*Type {
    var tokens = typ.variable().tokens.items();
    return if (tokens.len > 1) {
      // TODO: context type
      util.todo("multiple names impl with context type");
    } else blk: {
      var name = tokens[0];
      break :blk self.ctx.typScope.lookup(name.value);
    };
  }

  fn findType(self: *Self, typ: *Type, from_gen: bool) ?*Type {
    // TODO: augment to return failing name token
    if (self.lookupVarType(typ)) |ty| {
      // if this variable occurs in the pair stack before we push it,
      // then it's cyclic/recursive
      if (self.checkTVar(typ, ty)) |rec| {
        // if the type isn't from a generic lookup or if we're certain 
        // we're resolving an infinite type, then return a recursive type
        if (!from_gen or self.in_inf) {
          _ = rec.typeid();
          return Type.newRecursive(rec, rec.debug).box(self.ctx.allocator);
        }
      }
      var result = switch (ty.kind) {
        .Concrete, .Variable => ty,
        else => self.ctx.copyType(ty),
      };
      // cache the type.
      self.insertTVar(typ, MultiPair{.setter = typ, .key = ty, .value = result});
      return result;
    }
    return null;
  }

  fn lookupType(self: *Self, typ: *Type, from_gen: bool) !*Type {
    if (self.findType(typ, from_gen)) |found| {
      return found;
    } else {
      return self.error_(typ.debug, "Could not resolve type with name: '{s}'", .{typ.getName()});
    }
  }

  fn lookupTypeAbs(self: *Self, typ: *Type, from_gen: bool) ?*Type {
    return self.findType(typ, from_gen);
  }

  inline fn assertMaxSubStepsNotExceeded(self: *Self, typ: *Type, emit: bool) !void {
    errdefer {
      self.cyc_scope.decls.clearAndFree();
      self.cyc_scope = PairScope.init(self.ctx.allocator);
    }
    if (self.sub_steps >= MAX_DEPTH) {
      if (!emit) return error.TypeLinkError;
      return self.error_(
        typ.debugToken(),
        "Potentially infinite substitutions arising from probable self-referencing types", 
        .{}
      );
    }
  }

  inline fn assertGenericAliasSubMatches(self: *Self, alias: *Type, typ: *Type) !void {
    // check that a generic instantiation of a type alias matches the defined alias type
    if (!alias.isGeneric()) {
      return self.error_(
        typ.debug,
        "Type alias is not generic, but instantiated with {} parameters", 
        .{typ.generic().tparams_len()},
      );
    } else if (!typ.isGeneric()) {
      return self.error_(
        typ.debug,
        "Type alias is generic, but used without instantiation", 
        .{},
      );
    }
    var l_gen = alias.generic();
    var r_gen = typ.generic();
    if (l_gen.tparams_len() != r_gen.tparams_len()) {
      return self.error_(
        typ.debug,
        "Parameter mismatch in generic type instantiation. Expected {} generic argument(s), got {}", 
        .{l_gen.tparams_len(), r_gen.tparams_len()},
      );
    }
  }

  fn resolveTypeAbs(self: *Self, typ: *Type) TypeLinkError!bool {
    // resolve a type alias abstractly by walking the alias chain without substituting
    if (typ.isGeneric()) {
      var gen = typ.generic();
      if (gen.base.isVariable()) {
        var eqn = try self.lookupType(gen.base, false);
        if (eqn.isRecursive()) return true;
        if (eqn.isGeneric()) {
          var eqn_gen = eqn.generic();
          for (eqn_gen.getSlice()) |param| {
            if (try self.resolveTypeAbs(param)) {
              return true;
            }
          }
        }
        var ret = try self.resolveTypeAbs(eqn);
        self.delTVar(gen.base);
        return ret;
      }
      // gen.base is not a Variable, so nothing to lookup.
      // Instead, resolve the tparams which may be Variable or some interesting type
      for (gen.getSlice()) |param| {
        if (try self.resolveTypeAbs(param)) {
          return true;
        }
      }
      return false;
    }
    if (typ.isUnion()) {
      for (typ.union_().variants.values()) |variant| {
        if (try self.resolveTypeAbs(variant)) {
          return true;
        }
      }
      return false;
    }
    if (typ.isVariable()) {
      // if this variable occurs in the pair stack before we push it, then it's cyclic/recursive
      // when we begin resolving a variable - we push it on the pair stack
      var eqn = self.lookupTypeAbs(typ, false);
      // could be null from an unsubstituted aliasee of a type alias, e.g. type Foo{T} = list{T}
      // `T` here would be null since resolveTypeAbs() doesn't do substitutions
      if (eqn == null) return false;
      if (eqn.?.isRecursive()) {
        return true;
      }
      if (try self.resolveTypeAbs(eqn.?)) {
        return true;
      }
      // when we finish resolving the variable - we pop it off the pair stack
      self.delTVar(typ);
      return false;
    }
    return false;
  }

  fn resolveType(self: *Self, typ: *Type) TypeLinkError!*Type {
    try self.assertMaxSubStepsNotExceeded(typ, false);
    if (typ.isSimple() or typ.isRecursive()) {
      return typ;
    }
    if (typ.isGeneric()) {
      self.sub_steps += 1;
      var gen = typ.generic();
      if (gen.base.isVariable()) {
        var eqn = try self.lookupType(gen.base, true);
        // specially handle recursive generic types
        if (eqn.isRecursive()) {
          var rec = eqn.recursive();
          if (rec.base.alias_info) |alias_info| {
            if (!alias_info.lhs.isGeneric()) {
              return self.error_(typ.debug, "Non-generic type instantiated as generic", .{});
            }
            // check that the tparams of this generic type matches it's type alias tparams exactly.
            try self.assertGenericAliasSubMatches(alias_info.lhs, typ);
            // A generic recursive type's tparams will never be resolved and substituted for, so
            // just ensure that the tparam is actually valid.
            for (gen.tparams.items()) |tparam| {
              _ = try self.resolveType(tparam);
            }
            return eqn;
          }
          unreachable;
        }
        // only instantiate generic type variables when the alias type is guaranteed to be generic
        else if (eqn.alias_info == null or !eqn.alias_info.?.lhs.isGeneric()) {
          return self.error_(typ.debug, "Non-generic type instantiated as generic", .{});
        }
        var alias = eqn.alias_info.?.lhs;
        try self.assertGenericAliasSubMatches(alias, typ);
        self.ctx.typScope.pushScope();
        self.using_tvar += 1;
        var alias_gen = alias.generic();
        for (alias_gen.getSlice(), 0..) |tvar, i| {
          var tsub = gen.tparams.items()[i];
          // var r_tsub = try self.resolveType(tsub);
          std.debug.assert(tvar.variable().tokens.len() == 1);
          self.ctx.typScope.insert(tvar.variable().tokens.items()[0].value, tsub); // r_tsub
        }
        // `eqn` is the type alias' aliasee, and may not be generic, so we add an extra guard.
        // for ex: type Foo{T} = T  # <-- aliasee/eqn 'T' is not generic here.
        if (eqn.isGeneric()) {
          var eqn_gen = eqn.generic();
          for (eqn_gen.getSlice(), 0..) |param, i| {
            eqn_gen.tparams.items()[i] = try self.resolveType(param);
          }
        }
        // resolving eqn resolves typ
        var sol = try self.resolveType(eqn);
        self.using_tvar -= 1;
        self.ctx.typScope.popScope();
        self.delTVar(gen.base);
        return sol;
      } else {
        for (gen.getSlice(), 0..) |param, i| {
          gen.tparams.items()[i] = try self.resolveType(param);
        }
        return typ;
      }
    }
    if (typ.isUnion()) {
      // TODO: need to figure out how to do this in-place
      var uni = typ.union_();
      var old_variants = uni.variants;
      uni.variants = types.TypeHashSet.init(self.ctx.allocator);
      for (old_variants.values()) |variant| {
        uni.set(try self.resolveType(variant));
      }
      // no need to clear and free old variants since, arena
      return typ;
    }
    if (typ.isVariable()) {
      // if this variable occurs in the pair stack before we push it, then it's cyclic/recursive
      // when we begin resolving a variable - we push it on the pair stack
      var eqn = try self.lookupType(typ, false);
      var alias_info = if (eqn.isRecursive()) eqn.recursive().base.alias_info else eqn.alias_info;
      if (alias_info) |alias| {
        if (alias.lhs.isGeneric()) {
          // check if this is a regular generic type called without instantiation, 
          // or a recursive generic type called without instantiation
          if (self.using_tvar == 0 or (eqn.isRecursive())) {
            return self.error_(
              typ.debug,
              "Generic type '{s}' may not have been instantiated correctly",
              .{typ.typename(self.ctx.allocator)}
            );
          }
        }
      }
      var sol = try self.resolveType(eqn);
      // when we finish resolving the variable - we pop it off the pair stack
      self.delTVar(typ);
      return sol;
    }
    return typ;
  }

  fn tryResolveType(self: *Self, typ: *Type) TypeLinkError!*Type {
    self.sub_steps = 0;
    self.in_inf = false;
    if (typ.isLikeGeneric()) {
      self.cyc_scope.pushScope();
      self.in_inf = try self.resolveTypeAbs(typ);
      self.cyc_scope.popScope();
    }
    self.cyc_scope.pushScope();
    var sol = try self.resolveType(typ);
    if (sol.isUnion()) {
      sol = Type.compressTypes(&sol.union_().variants, sol.debug, sol);
    }
    self.cyc_scope.popScope();
    return sol;
  }

  fn resolve(self: *Self, typ: *Type) !*Type {
    const ty = try self.tryResolveType(typ);
    // set alias info for debugging
    if (typ.alias_info == null) {
      // TODO: does this need to be set only for variable types?
      ty.alias_info = types.AliasInfo.init(typ, ty);
    }
    return ty;
  }

  fn linkNumber(self: *Self, node: *ast.LiteralNode) !void {
    _ = self;
    _ = node;
  }

  fn linkString(self: *Self, node: *ast.LiteralNode) !void {
    _ = self;
    _ = node;
  }

  fn linkBool(self: *Self, node: *ast.LiteralNode) !void {
    _ = self;
    _ = node;
  }

  fn linkUnary(self: *Self, node: *ast.UnaryNode) !void {
    try self.link(node.expr);
  }

  fn linkBinary(self: *Self, node: *ast.BinaryNode) !void {
    try self.link(node.left);
    try self.link(node.right);
  }

  fn linkList(self: *Self, node: *ast.ListNode) !void {
    for (node.elems.items()) |elem| {
      try self.link(elem);
    }
  }

  fn linkMap(self: *Self, node: *ast.MapNode) !void {
    for (node.pairs.items()) |pair| {
      try self.link(pair.key);
      try self.link(pair.value);
    }
  }

  fn linkNil(self: *Self, node: *ast.LiteralNode) !void {
    _ = self;
    _ = node;
  }

  fn linkExprStmt(self: *Self, node: *ast.ExprStmtNode) !void {
    try self.link(node.expr);
  }

  fn linkVar(self: *Self, node: *ast.VarNode) !void {
    var typ = self.ctx.varScope.lookup(node.token.value);
    if (typ != null) {
      node.typ = typ;
    }
  }

  fn linkAssign(self: *Self, node: *ast.BinaryNode) !void {
    try self.linkBinary(node);
  }
  
  fn linkBlock(self: *Self, node: *ast.BlockNode) !void {
    self.ctx.enterScope();
    for (node.nodes.items()) |item| {
      try self.link(item);
    }
    self.ctx.leaveScope();
  }

  fn linkNType(self: *Self, node: *ast.TypeNode) !void {
    // TODO: using an indirection here because somehow using the `node.typ` 
    //      itself leads to aliasing issues in Zig.
    var typ = node.typ.box(self.ctx.allocator);
    var tmp = try self.resolve(typ);
    node.typ = tmp.*;
  }

  fn linkCast(self: *Self, node: *ast.CastNode) !void {
    try self.link(node.expr);
    try self.linkNType(node.typn);
  }

  fn linkVarDecl(self: *Self, node: *ast.VarDeclNode) !void {
    if (node.ident.typ) |ty| {
      node.ident.typ = try self.resolve(ty);
      self.ctx.varScope.insert(node.ident.token.value, node.ident.typ.?);
    }
    try self.link(node.value);
  }

  fn linkAlias(self: *Self, node: *ast.AliasNode) !void {
    var typ = node.alias.typ;
    var tokens = if (typ.isGeneric()) typ.generic().base.variable().tokens else typ.variable().tokens;
    self.ctx.typScope.insert(tokens.items()[0].value, &node.aliasee.typ);
  }

  fn linkSubscript(self: *Self, node: *ast.SubscriptNode) !void {
    try self.link(node.expr);
    try self.link(node.index);
  }

  fn linkDeref(self: *Self, node: *ast.DerefNode) !void {
    try self.link(node.expr);
  }

  fn linkIf(self: *Self, node: *ast.IfNode) !void {
    try self.link(node.cond);
    try self.linkBlock(&node.then.AstBlock);
    for (node.elifs.items()) |elif| {
      try self.linkElif(&elif.AstElif);
    }
    try self.linkBlock(&node.els.AstBlock);
  }

  fn linkElif(self: *Self, node: *ast.ElifNode) !void {
    try self.link(node.cond);
    try self.linkBlock(&node.then.AstBlock);
  }

  fn linkWhile(self: *Self, node: *ast.WhileNode) !void {
    try self.link(node.cond);
    try self.linkBlock(&node.then.AstBlock);
  }

  fn linkProgram(self: *Self, node: *ast.ProgramNode) !void {
    self.ctx.enterScope();
    for (node.decls.items()) |item| {
      try self.link(item);
    }
    // only pop off varScope, since typScope needs to be 
    // reused by the type checker
    self.ctx.varScope.popScope();
  }

  fn link(self: *Self, node: *Node) TypeLinkError!void {
    switch (node.*) {
      .AstNumber => |*nd| try self.linkNumber(nd),
      .AstString => |*nd| try self.linkString(nd),
      .AstBool => |*nd| try self.linkBool(nd),
      .AstUnary => |*nd| try self.linkUnary(nd),
      .AstBinary => |*nd| try self.linkBinary(nd),
      .AstList, .AstTuple => |*nd| try self.linkList(nd),
      .AstMap => |*nd| try self.linkMap(nd),
      .AstExprStmt => |*nd| try self.linkExprStmt(nd),
      .AstVar => |*nd| try self.linkVar(nd),
      .AstNil => |*nd| try self.linkNil(nd),
      .AstVarDecl => |*nd| try self.linkVarDecl(nd),
      .AstAssign => |*nd| try self.linkAssign(nd),
      .AstBlock => |*nd| try self.linkBlock(nd),
      .AstNType => |*nd| try self.linkNType(nd),
      .AstAlias => |*nd| try self.linkAlias(nd),
      .AstCast => |*nd| try self.linkCast(nd),
      .AstSubscript => |*nd| try self.linkSubscript(nd),
      .AstDeref => |*nd| try self.linkDeref(nd),
      .AstIf => |*nd| try self.linkIf(nd),
      .AstElif => |*nd| try self.linkElif(nd),
      .AstWhile => |*nd| try self.linkWhile(nd),
      .AstControl => {},
      .AstProgram => |*nd| try self.linkProgram(nd),
      .AstSimpleIf, .AstCondition, .AstEmpty => unreachable,
    }
  }

  pub fn linkTypes(self: *Self, node: *Node) !void {
    try self.link(node);
  }
};
