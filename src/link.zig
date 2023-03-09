const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");
const util = @import("util.zig");

const Type = types.NType;
const TypeKind = types.NTypeKind;
const Node = ast.AstNode;

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
  };
}

const Scope = struct {
  decls: std.ArrayList(TypeMap),
  allocator: std.mem.Allocator,

  const K = []const u8;
  const V = *Type;
  const TypeMap = CreateMap(K, V);

  pub fn init(allocator: std.mem.Allocator) @This(){
    return @This() {.decls = std.ArrayList(TypeMap).init(allocator), .allocator = allocator};
  }

  pub fn pushScope(self: *@This()) void {
    util.append(TypeMap, &self.decls, TypeMap.init(self.allocator));
  }

  pub fn popScope(self: *@This()) void {
    if (self.decls.items.len == 0) {
      util.error_("pop from empty scope-list", .{});
    }
    _ = self.decls.pop();
  }

  fn current(self: *@This()) TypeMap {
    return self.decls.getLast();
  }

  inline fn len(self: *@This()) usize {
    return self.decls.items.len;
  }

  pub fn lookup(self: *@This(), name: K) ?V {
    if (self.len() == 0) return null;
    var i: usize = self.len();
    while (i > 0): (i -= 1) {
      var map = self.decls.items[i - 1];
      if (map.get(name)) |ty| {
        return ty;
      }
    }
    return null;
  }

  pub fn insert(self: *@This(), name: K, ty: V) void {
    if (self.len() == 0) {
      util.error_("insert into empty scope-list", .{});
    }
    self.decls.items[self.len() - 1].put(name, ty);
  }
};

pub const TypeLinker = struct {
  allocator: std.mem.Allocator,
  ctx: TContext,

  const TContext = struct {
    /// type scope
    scope: Scope,
    /// scope for other declarations, e.g. variables, functions, etc.
    varScope: Scope,
    sub_steps: usize = 0,

    const MAX_SUB_STEPS = 0x3e8;
  };

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {
      .allocator = allocator, 
      .ctx = TContext{
        .scope = Scope.init(allocator), 
        .varScope = Scope.init(allocator)
      }
    };
  }

  fn error_(self: *Self, comptime fmt: []const u8, args: anytype) noreturn {
    _ = self;
    util.error_("TypeError: " ++ fmt, args);
  }

  inline fn newType(self: *Self, kind: TypeKind) *Type {
    return util.box(Type, Type.init(kind, null), self.allocator);
  }

  inline fn enterScope(self: *Self) void {
    self.ctx.scope.pushScope();
    self.ctx.varScope.pushScope();
  }

  inline fn leaveScope(self: *Self) void {
    self.ctx.scope.popScope();
    self.ctx.varScope.popScope();
  }

  fn copyType(self: *Self, typ: *Type) *Type {
    var new = self.newType(typ.kind);
    new.* = typ.*;
    return new;
  }

  fn findName(self: *Self, typ: *Type) ?*Type {
    // TODO: augment to return failing name token
    var tokens = typ.name.?.tokens.items;
    var found = if (tokens.len > 1) {
      // TODO: context type
      util.todo("multiple names impl with context type");
    } else blk: {
      var name = tokens[0];
      break :blk self.ctx.scope.lookup(name.value);
    };
    if (found) |ty| {
      return self.copyType(ty);
    }
    return null;
  }

  fn lookupName(self: *Self, typ: *Type) *Type {
    if (self.findName(typ)) |found| {
      return found;
    } else {
      self.error_("Could not resolve type with name: '{s}'", .{typ.getName()});
    }
  }

  inline fn assertMaxSubStepsNotExceeded(self: *Self) void {
    if (self.ctx.sub_steps >= TContext.MAX_SUB_STEPS) {
      self.error_(
        "Potentially infinite substitutions arising from probable self-referencing types", 
        .{}
      );
    }
  }

  inline fn assertNonNullableSubtype(self: *Self, subtype: *Type) void {
    // check that this subtype to be assigned to a nullable type is not itself nullable
    if (subtype.kind == .TyNullable) {
      self.error_("Nullable type cannot have a nullable subtype: {}", .{subtype});
    }
  }

  /// substitute abstract generic type parameters with more concrete ones
  fn substitute(self: *Self, sub: *Type, eqn: *Type) *Type {
    // type A -> str
    // type B{K} -> list{K}
    // x: B{A} -> list{K} -> list{A} -> list{str}
    self.ctx.sub_steps += 1;
    self.assertMaxSubStepsNotExceeded();
    if (sub.isGeneric()) {
      for (0..sub.tparams.len) |i| {
        var sub_param = sub.tparams.params[i];
        for (0..eqn.tparams.len) |j| {
          var eqn_param = eqn.tparams.params[j];
          // try to substitute if it's not a simple type
          if (eqn_param.isCompound()) {
            eqn.tparams.params[j] = self.substitute(sub_param, eqn_param);
          }
        }
      }
    }
    // resolve names in eqn if any
    if (eqn.isGeneric()) {
      if (eqn.tparams.len > 0) {
        for (0..eqn.tparams.len) |i| {
          var param = eqn.tparams.params[i];
          if (param.hasNameType(TContext.MAX_SUB_STEPS)) {
            eqn.tparams.params[i] = self.resolveType(param);
          }
        }
      } else {
        // generic but not instantiated. copy the types
        for (0..sub.tparams.len) |i| {
          eqn.tparams.params[i] = self.resolveType(sub.tparams.params[i]);
          eqn.tparams.len += 1;
        }
      }
    }
    if (eqn.kind == .TyNullable) {
      var ty = self.substitute(sub, eqn.nsubtype.?);
      return Type.newNullable(self.newType(.TyNullable), ty);
    }
    if (eqn.kind == .TyUnion) {
      for (eqn.union_.?.types.items, 0..) |uni, i| {
        eqn.union_.?.types.items[i] = self.substitute(sub, uni);
      }
    }
    if (eqn.kind == .TyName) {
      if (self.findName(eqn)) |ty| {
        // solve ty using eqn as sub, i.e. substitue eqn into ty
        return self.substitute(eqn, ty);
      } else {
        // at this point, sub is the solution to eqn
        return sub;
      }
    }
    // eqn is the final solution
    return eqn;
  }

  fn resolveType(self: *Self, typ: *Type) *Type {
    if (typ.isSimple()) {
      return typ;
    }
    if (typ.isGeneric()) {
      for (0..typ.tparams.len) |i| {
        var param = typ.tparams.params[i];
        typ.tparams.params[i] = self.resolveType(param);
      }
    }
    if (typ.kind == .TyNullable) {
      var tmp = self.resolveType(typ.nsubtype.?);
      self.assertNonNullableSubtype(tmp);
      return Type.newNullable(self.newType(.TyNullable), tmp);
    }
    if (typ.kind == .TyUnion) {
      for (typ.union_.?.types.items, 0..) |uni, i| {
        typ.union_.?.types.items[i] = self.resolveType(uni);
      }
    }
    if (typ.kind == .TyName) {
      var eqn = self.lookupName(typ);
      // only instantiate generic type variables when the calling type is instantiated
      if (eqn.alias != null and typ.isGeneric()) {
        var alias = eqn.alias.?;
        if (!alias.isGeneric()) {
          self.error_(
            "Type alias is not generic, but instantiated with {} parameters", 
            .{typ.tparams.len}
          );
        }
        if (alias.tparams.len != typ.tparams.len) {
          self.error_(
            "Parameter mismatch in generic type instantiion. Expected {} generic arguments, got {}", 
            .{alias.tparams.len, typ.tparams.len}
          );
        }
        self.ctx.scope.pushScope();
        for (0..alias.tparams.len) |i| {
          var tvar = alias.tparams.params[i];
          var tsub = typ.tparams.params[i];
          self.ctx.scope.insert(tvar.name.?.tokens.items[0].value, tsub);
        }
        typ.* = self.substitute(typ, eqn).*;
        self.ctx.scope.popScope();
      } else {
        typ.* = self.substitute(typ, eqn).*;
      }
    }
    return typ;
  }

  fn resolve(self: *Self, typ: *Type) *Type {
    self.ctx.sub_steps = 0;
    return self.resolveType(typ);
  }

  fn linkNumber(self: *Self, node: *ast.LiteralNode) void {
    _ = self;
    _ = node;
  }

  fn linkString(self: *Self, node: *ast.LiteralNode) void {
    _ = self;
    _ = node;
  }

  fn linkBool(self: *Self, node: *ast.LiteralNode) void {
    _ = self;
    _ = node;
  }

  fn linkUnary(self: *Self, node: *ast.UnaryNode) void {
    self.link(node.expr);
  }

  fn linkBinary(self: *Self, node: *ast.BinaryNode) void {
    self.link(node.left);
    self.link(node.right);
  }

  fn linkList(self: *Self, node: *ast.ListNode) void {
    _ = self;
    _ = node;
  }

  fn linkMap(self: *Self, node: *ast.MapNode) void {
    _ = self;
    _ = node;
  }

  fn linkExprStmt(self: *Self, node: *ast.ExprStmtNode) void {
    self.link(node.expr);
  }

  fn linkVar(self: *Self, node: *ast.VarNode) void {
    var typ = self.ctx.varScope.lookup(node.token.value);
    if (typ != null) {
      node.typ = typ;
    }
  }

  fn linkAssign(self: *Self, node: *ast.BinaryNode) void {
    self.linkBinary(node);
  }
  
  fn linkBlock(self: *Self, node: *ast.BlockNode) void {
    self.enterScope();
    for (node.nodes.items) |item| {
      self.link(item);
    }
    self.leaveScope();
  }

  fn linkNType(self: *Self, node: *ast.TypeNode) void {
    _ = self;
    _ = node;
  }

  fn linkCast(self: *Self, node: *ast.CastNode) void {
    _ = self;
    _ = node;
  }

  fn linkVarDecl(self: *Self, node: *ast.VarDeclNode) void {
    if (node.ident.typ) |ty| {
      node.ident.typ = self.resolve(ty);
      self.ctx.varScope.insert(node.ident.token.value, node.ident.typ.?);
    }
  }

  fn linkAlias(self: *Self, node: *ast.AliasNode) void {
    var name = node.alias.typ.name.?;
    self.ctx.scope.insert(name.tokens.items[0].value, &node.aliasee.typ);
  }

  fn linkProgram(self: *Self, node: *ast.ProgramNode) void {
    self.enterScope();
    for (node.decls.items) |item| {
      self.link(item);
    }
    self.leaveScope();
  }

  fn link(self: *Self, node: *Node) void {
    switch (node.*) {
      .AstNumber => |*nd| self.linkNumber(nd),
      .AstString => |*nd| self.linkString(nd),
      .AstBool => |*nd| self.linkBool(nd),
      .AstUnary => |*nd| self.linkUnary(nd),
      .AstBinary => |*nd| self.linkBinary(nd),
      .AstList => |*nd| self.linkList(nd),
      .AstMap => |*nd| self.linkMap(nd),
      .AstExprStmt => |*nd| self.linkExprStmt(nd),
      .AstVar => |*nd| self.linkVar(nd),
      .AstVarDecl => |*nd| self.linkVarDecl(nd),
      .AstAssign => |*nd| self.linkAssign(nd),
      .AstBlock => |*nd| self.linkBlock(nd),
      .AstNType => |*nd| self.linkNType(nd),
      .AstAlias => |*nd| self.linkAlias(nd),
      .AstCast => |*nd| self.linkCast(nd),
      .AstProgram => |*nd| self.linkProgram(nd),
    }
  }

  pub fn linkTypes(self: *Self, node: *Node) void {
    self.link(node);
  }

};
