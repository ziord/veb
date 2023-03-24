const std = @import("std");
const ast = @import("ast.zig");
const link = @import("link.zig");
const util = @import("util.zig");

const Scope = link.Scope;
const Token = link.Token;
const Type = link.Type;
const TypeKind = link.TypeKind;
const TUnion = link.TUnion;
const TContext = link.TContext;
const Node = link.Node;
const TypeLinker = link.TypeLinker;

const TypeHashMap = std.AutoArrayHashMap(u32, *Type);

pub const TypeChecker = struct {
  allocator: std.mem.Allocator,
  ctx: TContext,

  const UnitTypes = struct {
    var tyNumber: Type = Type.init(.TyNumber, null, Token.getDefault());
    var tyString: Type = Type.init(.TyString, null, Token.getDefault());
    var tyBool: Type = Type.init(.TyBool, null, Token.getDefault());
    var tyNil: Type = Type.init(.TyNil, null, Token.getDefault());
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

  pub fn findType(self: *Self, name: []const u8) ?*Type {
    // TODO
    if (self.ctx.typScope.lookup(name)) |ty| {
      return self.ctx.copyType(ty);
    }
    return null;
  }

  pub fn findName(self: *Self, name: []const u8) ?*Type {
    if (self.ctx.varScope.lookup(name)) |ty| {
      return ty; // self.ctx.copyType(ty);
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

  fn validateGenericParamSize(self: *Self, size: usize, max_size: usize, exact: bool, type_name: []const u8, token: Token) !void {
    var not_okay = if (exact) size < max_size else size > max_size;
    if (not_okay) {
      return self.error_(
        token, 
        "'{s}' type accepts {} generic type parameter(s), but got {}", 
        .{type_name, max_size, size}
      );
    }
  }

  fn getTypename(self: *Self, typ: *Type) []const u8 {
    return typ.typename(self.allocator);
  }

  fn inferLiteral(self: *Self, node: *ast.LiteralNode, kind: TypeKind) !*Type {
    if (node.typ) |typ| {
      return typ;
    } else {
      node.typ = self.ctx.newType(kind, node.token);
      return node.typ.?;
    }
  }

  fn inferNil(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, .TyNil);
  }

  fn inferNumber(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, .TyNumber);
  }

  fn inferString(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, .TyString);
  }

  fn inferBool(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, .TyBool);
  }

  fn inferList(self: *Self, node: *ast.ListNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    // create a new type
    node.typ = self.ctx.newType(.TyList, node.token);
    if (node.elems.items.len == 0) {
      return node.typ.?;
    }
    // infer type of elements stored in the list
    var types = TypeHashMap.init(self.allocator);
    types.ensureTotalCapacity(node.elems.items.len) catch {};
    for (node.elems.items) |elem| {
      var typ = try self.infer(elem);
      types.put(typ.typeid(), typ) catch break;
    }
    if (types.count() > 1) {
      // convert types to a single union type
      var tparam = self.ctx.newType(.TyUnion, node.token);
      tparam.union_ = TUnion.init(self.allocator);
      for (types.values()) |typ| {
        util.append(*Type, &tparam.union_.?.types, typ);
      }
      node.typ.?.tparams.params[0] = tparam;
      node.typ.?.tparams.len += 1;
    } else {
      node.typ.?.tparams.params[0] = types.values()[0];
      node.typ.?.tparams.len += 1;
    }
    return node.typ.?;
  }

  fn inferMap(self: *Self, node: *ast.MapNode) !*Type {
     if (node.typ) |typ| {
      return typ;
    }
    // create a new type
    node.typ = self.ctx.newType(.TyMap, node.token);
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
    node.typ.?.tparams.params[0] = key_typ;
    node.typ.?.tparams.params[1] = val_typ;
    node.typ.?.tparams.len += 2;
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
      node.typ = self.ctx.newType(.TyBool, lhsTy.debug);
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
    var typ = try self.inferBinary(node);
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

  /// check cast follows coercion rules
  fn _checkCast(
      self: *Self, node_ty: *Type,
      cast_ty: *Type, debug: Token,
      emit: bool, report_node_ty: *Type, report_cast_ty: *Type
  ) TypeCheckError!*Type {
    if (cast_ty.kind == .TyNil) {
      return self.errorOrEmit(emit, debug, "Illegal cast to nil", .{});
    }
    // any type may be cast to bool
    if (cast_ty.kind == .TyBool) {
      return cast_ty;
    }
    // nil may cast to nullable
    if (node_ty.kind == .TyNil) {
      if (cast_ty.kind == .TyNullable) {
        return cast_ty;
      } else {
        return self.errorOrEmit(emit, debug, "Illegal cast from 'nil' to '{s}'", .{self.getTypename(report_cast_ty)});
      }
    }
    // a nullable type may be cast to bool or an assignable-nullable type
    if (node_ty.kind == .TyNullable) {
      if (cast_ty.kind == .TyNullable) {
        _ = try self._checkCast(node_ty.nsubtype.?, cast_ty.nsubtype.?, debug, emit, report_node_ty, report_cast_ty);
        return cast_ty;
      }
      return self.errorOrEmit(emit, debug, "Nullable type may only cast to bool or equi-nullable type", .{});
    }
    // any type may be cast to nullable of that type, i.e. type -> type?
    if (cast_ty.kind == .TyNullable) {
      _ = try self._checkCast(node_ty, cast_ty.nsubtype.?, debug, emit, report_node_ty, report_cast_ty);
      return cast_ty;
    }
    if (node_ty.kind == .TyList and cast_ty.kind == .TyList) {
      try self.validateGenericParamSize(cast_ty.tparams.len, 1, true, "list", debug);
      // empty generic to specialized generic
      if (node_ty.tparams.len == 0) return cast_ty;
      // non-empty to another type
      try self.validateGenericParamSize(node_ty.tparams.len, 1, true, "list", debug);
      _ = try self._checkCast(node_ty.tparams.params[0], cast_ty.tparams.params[0], debug, emit, report_node_ty, report_cast_ty);
      return cast_ty;
    }
    if (node_ty.kind == .TyMap and cast_ty.kind == .TyMap) {
      try self.validateGenericParamSize(cast_ty.tparams.len, 2, true, "map", debug);
      // empty generic to specialized generic
      if (node_ty.tparams.len == 0) return cast_ty;
      // non-empty to another type
      try self.validateGenericParamSize(node_ty.tparams.len, 2, true, "map", debug);
      _ = try self._checkCast(node_ty.tparams.params[0], cast_ty.tparams.params[0], debug, emit, report_node_ty, report_cast_ty);
      _ = try self._checkCast(node_ty.tparams.params[1], cast_ty.tparams.params[1], debug, emit, report_node_ty, report_cast_ty);
      return cast_ty;
    }
    // upcasting/widening
    if (cast_ty.containsType(node_ty)) {
      std.debug.assert(cast_ty.kind == .TyUnion);
      cast_ty.union_.?.active = node_ty;
      return cast_ty;
    }
    // downcasting
    if (node_ty.containsType(cast_ty)) {
      std.debug.assert(node_ty.kind == .TyUnion);
      if (node_ty.union_.?.active) |active| {
        // check if the active type is assignable to the cast type
        _ = self.checkAssign(cast_ty, active, debug, false) catch {
          return self.errorOrEmit(
            emit, debug,
            "Cannot cast from type '{s}' to type '{s}', because the active type is '{s}'",
            .{self.getTypename(report_node_ty), self.getTypename(report_cast_ty), self.getTypename(active)}
          );
        };
        return cast_ty;
      } else {
        // TODO:
        // we need to keep track of a cast like this. For example:
        // let p: str | num = 5
        // (p as num) + 2  # ok
        // (p as str).concat(...) # not okay  <-- to avoid this, we need to somehow keep 
        // track of casts performed without an active type being known (in runtime cases)
        // just return the union in this case, for now.
        return node_ty;
      }
    }
    // Due to the nature of typeid, T | S is equal to S | T.
    // Hence, we allow generics and other checks take precedence before equality comparison,
    // that way, things like: x: map{num, str} ; x as map{str, num} would be properly caught.
    if (node_ty.typeid() == cast_ty.typeid()) {
      return cast_ty;
    }
    return self.errorOrEmit(
      emit, debug,
      "Cannot cast from type '{s}' to type '{s}'",
      .{self.getTypename(report_node_ty), self.getTypename(report_cast_ty)}
    );
  }

  /// check assignment follows assignment rules
  fn _checkAssign(self: *Self, target: *Type, source: *Type) ?*Type {
    if (target == source) return target;
    switch (target.kind) {
      .TyUnion => {
        if (target.typeid() == source.typeid()) {
          if (source.union_.?.active) |active| {
            target.union_.?.active = active;
          }
          return target;
        }
        if (target.containsType(source)) {
          target.union_.?.active = source;
          return target;
        }
      },
      .TyNullable => {
        // NOTE: nullable type does not distribute over it's subtype.
        // For example: (str | num)? !== str? | num?
        // The subtype of a nullable type is its **own concrete** type.
        if (source.kind == .TyNil) return target;
        if (self._checkAssign(target.nsubtype.?, source) != null) {
          return target;
        }
      },
      .TyList, .TyMap => |kind| {
        if (source.kind == kind) {
          if (target.tparams.len == source.tparams.len) {
            var found: ?*Type = null;
            for (target.tparams.getSlice(), 0..) |param, i| {
              found = self._checkAssign(param, source.tparams.params[i]);
              if (found != null) return target;
            }
            return null;
          }
        }
      },
      else => {},
    }
    // lowest precedence
    if (target.typeid() == source.typeid()) {
      return target;
    }
    return null;
  }

  fn checkCast(self: *Self, node_ty: *Type, cast_ty: *Type, debug: Token, emit: bool) TypeCheckError!*Type {
    return self._checkCast(node_ty, cast_ty, debug, emit, node_ty, cast_ty);
  }

  fn checkAssign(self: *Self, target: *Type, source: *Type, debug: ?Token, emit: bool) !*Type {
    var typ = self._checkAssign(target, source);
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
    if (node.op.optype == .OpAssign) {
      _ = try self.checkAssign(node.typ.?, source, node.op.token, true);
      return;
    }
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
        var uni = self.ctx.newType(.TyUnion, node.typ.?.debug);
        uni.union_ = TUnion.init(self.allocator);
        util.append(*Type, &uni.union_.?.types, node.typ.?);
        util.append(*Type, &uni.union_.?.types, source);
        node.typ = uni;
      }
      return;
    }
    var errTy: ?*Type = null;
    if (node.typ.?.typeid() != (&UnitTypes.tyNumber).typeid()) {
      errTy = node.typ;
    } else if (source.typeid() != (&UnitTypes.tyNumber).typeid()) {
      errTy = source;
    }
    if (errTy != null) {
      const name = self.getTypename(&UnitTypes.tyNumber);
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
