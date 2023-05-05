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
const TypeList = types.TypeList;

pub const TypeChecker = struct {
  allocator: std.mem.Allocator,
  ctx: TContext,

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
  pub const TypeCheckError = error{TypeCheckError};

  pub fn init(allocator: std.mem.Allocator, filename: []const u8) @This() {
    return Self {
      .allocator = allocator, 
      .ctx = TContext.init(allocator, filename)
    };
  }

  fn error_(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) token.showError(self.ctx.filename, "TypeError: " ++ fmt, args);
    return error.TypeCheckError;
  }

  inline fn warn(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) void {
    if (emit) token.showError(self.ctx.filename, "TypeWarning: " ++ fmt, args);
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
      return self.error_(true, ident.token, "Could not resolve type of ident: '{s}'", .{ident.token.value});
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

  fn compressTypes(self: *Self, typeset: *TypeHashSet, debug: Token) *Type {
    // combine types in typeset as much as possible
    if (typeset.count() > 1) {
      var final = TypeList.init(self.allocator);
      var last_ty: ?*Type = null;
      var has_nil = false;
      for (typeset.values()) |typ| {
        // skip related types & nil types
        if (typ.isNilTy()) {
          has_nil = true;
          continue;
        }
        if (last_ty) |ty| {
          // TODO: would this be good for classes?
          if (ty.isRelatedTo(typ, .RCAny, self.allocator)) {
            continue;
          }
        }
        last_ty = typ;
        final.append(typ) catch break;
      }
      // convert types to a single union type
      return blk: {
        var tmp: *Type = undefined;
        if (final.items.len > 1) {
          tmp = Type.newUnion(self.allocator, debug).box(self.allocator);
          tmp.union_().addAll(&final);
        } else {
          tmp = final.items[0];
        }
        if (has_nil) {
          tmp = Type.newNullable(tmp, debug).box(self.allocator);
        }
        break :blk tmp;
      };
    }
    return typeset.values()[0];
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
    gen.append(self.compressTypes(&typeset, node.token));
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
    if (node.typ) |typ| {
      return typ;
    }
    node.typ = try self.infer(node.expr);
    // unary op: ~, !, -, +
    if (node.op.optype == .OpBitInvert or node.op.optype == .OpAdd or node.op.optype == .OpSub) {
      try self.checkUnary(node, @constCast(&UnitTypes.tyNumber));
    } else {
      std.debug.assert(node.op.optype == .OpNot);
      // `!` accepts any type and returns a boolean. 
      // It applies an implicit bool cast to such a type.
      node.typ = UnitTypes.bol.toType(node.typ.?.debug).box(self.allocator);
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
    // TODO: Do we need to always fetch the updated type?
    // if (node.typ) |typ| return typ;
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
      // need to always update, because lookup copies.
      .AstVar => |ident| self.ctx.varScope.insert(ident.token.value, typ),
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
    self.ctx.varScope.insert(node.ident.token.value, node.ident.typ.?);
    return node.ident.typ.?;
  }

  fn inferSubscript(self: *Self, node: *ast.SubscriptNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    var expr_ty = try self.infer(node.expr);
    // fail fast
    if (!expr_ty.isGeneric()) {
      return self.error_(
        true, node.token,
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    var index_ty = try self.infer(node.index);
    try self.checkSubscript(node, expr_ty, index_ty);
    return node.typ.?;
  }

  fn inferDeref(self: *Self, node: *ast.DerefNode) !*Type {
    var expr_ty = try self.infer(node.expr);
    try self.checkDeref(node, expr_ty);
    return node.typ.?;
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

  fn inferIf(self: *Self, node: *ast.IfNode) !*Type {
    var ty = try self.infer(node.cond);
    _ = try self.inferBlock(&node.then);
    for (node.elifs.items) |*elif| {
      _ = try self.inferElif(elif);
    }
    _ = try self.inferBlock(&node.els);    
    try self.checkIfCond(ty, node.token);
    return undefined;
  }

  fn inferElif(self: *Self, node: *ast.ElifNode) !*Type {
    var ty = try self.infer(node.cond);
    _ = try self.inferBlock(&node.then);
    try self.checkIfCond(ty, node.token);
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
    var ty = node_ty.canBeCastTo(cast_ty, self.allocator) catch |e| {
      if (e == error.UnionCastError) {
        var active = if (node_ty.isUnion()) self.getTypename(node_ty.union_().active.?) else "different";
        return self.error_(
          emit, debug,
          "Cannot cast from type '{s}' to type '{s}' because the active type is {s}",
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
    var typ = target.canBeAssigned(source, self.allocator);
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

  fn checkBinary(self: *Self, node: *ast.BinaryNode, source: *Type) !void {
    // source is type of rhs
    // node.typ is type of lhs
    if (node.op.optype == .OpEqq or node.op.optype == .OpNeq) {
      if (!node.typ.?.isEitherWayRelatedTo(source, .RCAny, self.allocator)) {
        return self.error_(
          true, node.op.token,
          "types must be related for equality comparison",
          .{}
        );
      }
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
    if (!expr_ty.isListTy() and !expr_ty.isMapTy()) {
      return self.error_(
        true, node.token,
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.generic().tparams_len() == 0) {
      return self.error_(
        true, node.token,
        "Cannot index empty or non-specialized '{s}' type", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.isListTy()) {
      if (!index_ty.isNumTy()) {
        return self.error_(
          true, node.token,
          "Cannot index '{s}' type with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      }
      node.typ = expr_ty.generic().tparams.items[0];
    } else if (expr_ty.isMapTy()) {
      // k-v. index with k, get v.
      var gen = expr_ty.generic();
      std.debug.assert(gen.tparams_len() == 2);
      var key_typ = gen.tparams.items[0];
      var val_typ = gen.tparams.items[1];
      _ = self.checkAssign(key_typ, index_ty, index_ty.debug, false) catch {
        return self.error_(
          true, node.token,
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
    node.typ = expr_ty.nullable().subtype;
  }

  fn checkIfCond(self: *Self, cond_ty: *Type, debug: Token) !void {
    if (!cond_ty.isBoolTy()) {
      return self.error_(
        true, debug, 
        "Expected type 'bool' in condition, but got '{s}'", 
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
      .AstSubscript => |*nd| try self.inferSubscript(nd),
      .AstDeref => |*nd| try self.inferDeref(nd),
      .AstIf => |*nd| try self.inferIf(nd),
      .AstElif => |*nd| try self.inferElif(nd),
      .AstProgram => |*nd| try self.inferProgram(nd),
      .AstEmpty => unreachable,
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
