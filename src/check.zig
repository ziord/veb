const std = @import("std");
const ast = @import("ast.zig");
const link = @import("link.zig");
const util = @import("util.zig");

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

pub const TypeChecker = struct {
  allocator: std.mem.Allocator,
  ctx: TContext,

  const UnitTypes = struct {
    const num = types.Concrete.init(.TyNumber);
    const str = types.Concrete.init(.TyString);
    const bol = types.Concrete.init(.TyBool);
    const nil = types.Concrete.init(.TyNil);

    const tyNumber: Type = Type.init(.{.Concrete = num}, Token.getDefault());
    const tyString: Type = Type.init(.{.Concrete = str}, Token.getDefault());
    const tyBool: Type = Type.init(.{.Concrete = bol}, Token.getDefault());
    const tyNil: Type = Type.init(.{.Concrete = nil}, Token.getDefault());
  };
  const Self = @This();
  pub const TypeCheckError = error{TypeCheckError};

  pub fn init(allocator: std.mem.Allocator, filename: []const u8) @This() {
    return Self {
      .allocator = allocator, 
      .ctx = TContext.init(allocator, filename)
    };
  }

  fn error_(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    token.showError(self.ctx.filename, "TypeError: " ++ fmt, args);
    return error.TypeCheckError;
  }

  fn errorOrEmit(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) {
      return self.error_(token, fmt, args);
    }
    return error.TypeCheckError;
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
    return null;
  }

  fn lookupName(self: *Self, ident: *ast.VarNode) !*Type {
    if (self.findName(ident.token.value)) |found| {
      return found;
    } else {
      return self.error_(ident.token, "Could not resolve type of ident: '{s}'", .{ident.token.value});
    }
  }

  fn validateGenericParamSize(self: *Self, size: usize, exp_size: usize, token: Token) !void {
    if (size != exp_size) {
      return self.error_(
        token, 
        "Generic type parameter(s) mismatch. Expected {} type parameter(s), but got {}", 
        .{exp_size, size}
      );
    }
  }

  fn getTypename(self: *Self, typ: *Type) []const u8 {
    return typ.typename(self.allocator);
  }

  fn inferLiteral(self: *Self, node: *ast.LiteralNode, kind: types.Concrete) !*Type {
    if (node.typ) |typ| {
      return typ;
    } else {
      node.typ = kind.toType(node.token).box(self.allocator);
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

  fn inferList(self: *Self, node: *ast.ListNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    // create a new type
    var base = Type.newConcrete(.TyClass, "list", node.token).box(self.allocator);
    node.typ = Type.newGeneric(self.allocator, base, node.token).box(self.allocator);
    if (node.elems.items.len == 0) {
      return node.typ.?;
    }
    // infer type of elements stored in the list
    var typeset = TypeHashSet.init(self.allocator);
    typeset.ensureTotalCapacity(node.elems.items.len) catch {};
    for (node.elems.items) |elem| {
      var typ = try self.infer(elem);
      typeset.put(typ.typeid(), typ) catch break;
    }
    var gen = node.typ.?.generic();
    if (typeset.count() > 1) {
      // convert types to a single union type
      var unionTy = Type.newUnion(self.allocator, node.token).box(self.allocator);
      unionTy.union_().addAll(&typeset);
      gen.append(unionTy);
    } else {
      gen.append(typeset.values()[0]);
    }
    return node.typ.?;
  }

  fn inferMap(self: *Self, node: *ast.MapNode) !*Type {
     if (node.typ) |typ| {
      return typ;
    }
    // create a new type
    var base = Type.newConcrete(.TyClass, "map", node.token).box(self.allocator);
    node.typ = Type.newGeneric(self.allocator, base, node.token).box(self.allocator);
    if (node.pairs.items.len == 0) {
      return node.typ.?;
    }
    // infer type of items stored in the map
    var first_pair = node.pairs.items[0];
    var key_typ = try self.infer(first_pair.key);
    var val_typ = try self.infer(first_pair.value);

    if (node.pairs.items.len > 1) {
      for (node.pairs.items[1..], 1..) |pair, i| {
        _ = i;
        var typ = try self.infer(pair.key);
        _ = self.checkAssign(key_typ, typ, typ.debug, false) catch {
          return self.error_(
            node.token,
            "expected key type '{s}', but found '{s}'",
            .{self.getTypename(key_typ), self.getTypename(typ)}
          );
        };
        typ = try self.infer(pair.value);
        _ = self.checkAssign(val_typ, typ, typ.debug, false) catch {
          return self.error_(
            node.token,
            "expected value type '{s}', but found '{s}'",
            .{self.getTypename(val_typ), self.getTypename(typ)}
          );
        };
      }
    }
    var gen = node.typ.?.generic();
    gen.append(key_typ);
    gen.append(val_typ);
    return node.typ.?;
  }

  fn inferUnary(self: *Self, node: *ast.UnaryNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    node.typ = try self.infer(node.expr);
    // unary op: ~, !, -, +
    if (node.op.optype == .OpBitInvert or node.op.optype == .OpAdd or node.op.optype == .OpSub) {
      try self.checkUnary(node, @constCast(&UnitTypes.tyNumber));
    } else {
      std.debug.assert(node.op.optype == .OpNot);
      // ! accepts any type and returns a boolean. 
      // It applies an implicit bool cast to such a type.
    }
    return node.typ.?;
  }

  fn inferBinary(self: *Self, node: *ast.BinaryNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    node.typ = lhsTy;
    try self.checkBinary(node, rhsTy);
    if (node.op.optype.isCmpOp()) {
      node.typ = UnitTypes.bol.toType(lhsTy.debug).box(self.allocator);
    }
    return node.typ.?;
  }

  fn inferVar(self: *Self, node: *ast.VarNode) !*Type {
    node.typ = try self.lookupName(node);
    return node.typ.?;
  }

  fn inferCast(self: *Self, node: *ast.CastNode) !*Type {
    var typ = try self.infer(node.expr);
    var cast_typ = &node.typn.typ;
    // use coercion rules
    return try self.checkCast(typ, cast_typ, node.token, true);
  }

  fn inferAssign(self: *Self, node: *ast.BinaryNode) !*Type {
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    var typ = try self.checkAssign(lhsTy, rhsTy, node.op.token, true);
    // update type.
    switch (node.left.*) {
      .AstVar => |ident| self.ctx.varScope.insert(ident.token.value, typ),
      else => {}
    }
    return typ;
  }

  fn inferNType(self: *Self, node: *ast.TypeNode) !*Type {
    _ = self;
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
    self.ctx.varScope.insert(node.ident.token.value, node.ident.typ.?);
    return node.ident.typ.?;
  }

  fn inferBlock(self: *Self, node: *ast.BlockNode) !*Type {
    self.ctx.enterScope();
    for (node.nodes.items) |item| {
      _ = try self.infer(item);
    }
    self.ctx.leaveScope();
    // crash and burn
    return undefined;
  }

  fn inferProgram(self: *Self, node: *ast.ProgramNode) !*Type {
    self.ctx.enterScope();
    for (node.decls.items) |item| {
      _ = try self.infer(item);
    }
    self.ctx.leaveScope();
    // crash and burn
    return undefined;
  }

  fn checkCast(self: *Self, node_ty: *Type, cast_ty: *Type, debug: Token, emit: bool) TypeCheckError!*Type {
    _ = node_ty.canBeCastTo(cast_ty, self.allocator) catch {
      return self.errorOrEmit(
        emit, debug,
        "Cannot cast from type '{s}' to type '{s}'",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    };
    return cast_ty;
  }

  fn checkAssign(self: *Self, target: *Type, source: *Type, debug: ?Token, emit: bool) !*Type {
    var typ = target.canBeAssigned(source, self.allocator);
    if (typ == null) {
      return self.errorOrEmit(
        emit,
        if (debug) |deb| deb else source.debug,
        "Cannot assign type '{s}' to type '{s}'",
        .{self.getTypename(source), self.getTypename(target)}
      );
    }
    return typ.?;
  }

  fn checkNil(self: *Self, node: *ast.LiteralNode, typ: *Type) !void {
    _ = typ;
    return self.error_(node.token, "Should not be checking nil",  .{});
  }

  fn checkUnary(self: *Self, node: *ast.UnaryNode, expected: *Type) !void {
    if (node.typ.?.typeid() != expected.typeid()) {
      const op = node.op.token.value;
      return self.error_(
        node.op.token,
        "Expected type {s} '{s}', but got {s} '{s}'",
        .{op, self.getTypename(expected), op, self.getTypename(node.typ.?)}
      );
    }
  }

  fn checkBinary(self: *Self, node: *ast.BinaryNode, source: *Type) !void {
    // source is type of rhs
    // node.typ is type of lhs
    if (node.op.optype == .OpEqq or node.op.optype == .OpNeq) {
      _ = self.checkCast(node.typ.?, source, node.op.token, false) catch {
        return self.error_(
          node.op.token,
          "types must be assignable for equality comparison",
          .{}
        );
      };
      return;
    }
    const op = node.op.token.value;
    if (node.op.optype == .OpAnd or node.op.optype == .OpOr) {
      // And/Or returns the type of their operands, or the union, if the types are different/disjoint
      if (node.typ.?.typeid() != source.typeid()) {
        var uni = Type.newUnion(self.allocator, node.typ.?.debug).box(self.allocator);
        uni.union_().set(node.typ.?);
        uni.union_().set(source);
        node.typ = uni;
      }
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
      return self.error_(
        node.op.token,
        "Expected type '{s}' {s} '{s}', but got '{s}' {s} '{s}'",
        .{
          name, op, name,
          self.getTypename(node.typ.?), op, self.getTypename(source)
        }
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
      .AstMap => |*nd| try self.inferMap(nd),
      .AstExprStmt => |*nd| try self.inferExprStmt(nd),
      .AstVar => |*nd| try self.inferVar(nd),
      .AstVarDecl => |*nd| try self.inferVarDecl(nd),
      .AstAssign => |*nd| try self.inferAssign(nd),
      .AstBlock => |*nd| try self.inferBlock(nd),
      .AstNType => |*nd| try self.inferNType(nd),
      .AstAlias => |*nd| try self.inferAlias(nd),
      .AstNil => |*nd| try self.inferNil(nd),
      .AstCast => |*nd| try self.inferCast(nd),
      .AstEmpty => unreachable,
      .AstProgram => |*nd| try self.inferProgram(nd),
    };
  }

  pub fn typecheck(self: *Self, node: *Node) TypeCheckError!void {
    var linker = link.TypeLinker.init(self.allocator, self.ctx.filename);
    linker.linkTypes(node) catch {
      return error.TypeCheckError;
    };
    _ = try self.infer(node);
  }
};
