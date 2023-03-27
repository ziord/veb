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

  pub fn findType(self: *Self, name: []const u8) ?*Type {
    // TODO
    if (self.ctx.typScope.lookup(name)) |ty| {
      return switch (ty.kind) {
        .Concrete, .Variable => ty,
        else => self.ctx.copyType(ty),
      };
    }
    return null;
  }

  pub fn findName(self: *Self, name: []const u8) ?*Type {
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

  /// check cast follows coercion rules
  fn _checkCast(
      self: *Self, node_ty: *Type,
      cast_ty: *Type, debug: Token,
      emit: bool, report_node_ty: *Type, report_cast_ty: *Type
  ) TypeCheckError!*Type {
    if (cast_ty.isNilTy()) {
      return self.errorOrEmit(emit, debug, "Illegal cast to nil", .{});
    }
    // any type may be cast to bool
    if (cast_ty.isBoolTy()) {
      return cast_ty;
    }
    // nil may cast to nullable
    if (node_ty.isNilTy()) {
      if (cast_ty.isNullable()) {
        return cast_ty;
      } else {
        return self.errorOrEmit(emit, debug, "Illegal cast from 'nil' to '{s}'", .{self.getTypename(report_cast_ty)});
      }
    }
    // a nullable type may be cast to bool or an assignable-nullable type
    if (node_ty.isNullable()) {
      if (cast_ty.isNullable()) {
        _ = try self._checkCast(node_ty.nullable().subtype, cast_ty.nullable().subtype, debug, emit, report_node_ty, report_cast_ty);
        return cast_ty;
      }
      return self.errorOrEmit(emit, debug, "Nullable type may only cast to bool or equi-nullable type", .{});
    }
    // any type may be cast to nullable of that type, i.e. type -> type?
    if (cast_ty.isNullable()) {
      _ = try self._checkCast(node_ty, cast_ty.nullable().subtype, debug, emit, report_node_ty, report_cast_ty);
      return cast_ty;
    }
    if (node_ty.isGeneric() and cast_ty.isGeneric()) {
      var node_gen = node_ty.generic();
      var cast_gen = cast_ty.generic();
      // check if the base types are assignable
      _ = self.checkAssign(cast_gen.base, node_gen.base, debug, false) catch {
        return self.errorOrEmit(
          emit, debug,
          "Cannot cast from type '{s}' to type '{s}', because neither types sufficiently overlaps",
          .{self.getTypename(report_node_ty), self.getTypename(report_cast_ty)}
        );
      };
      // empty generic to specialized generic
      if (node_gen.tparams_len() == 0) return cast_ty;
      // non-empty to another type
      try self.validateGenericParamSize(node_gen.tparams_len(), cast_gen.tparams_len(), debug);
      for (node_gen.tparams.items, 0..) |param, i| {
        _ = try self._checkCast(param, cast_gen.tparams.items[i], debug, emit, report_node_ty, report_cast_ty);
      }
      return cast_ty;
    }
    // upcasting/widening
    if (cast_ty.containsType(node_ty)) {
      std.debug.assert(cast_ty.isUnion());
      cast_ty.union_().active = node_ty;
      return cast_ty;
    }
    // downcasting
    if (node_ty.containsType(cast_ty)) {
      std.debug.assert(node_ty.isUnion());
      if (node_ty.union_().active) |active| {
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
      .Concrete => |conc| {
        if (!source.isConcrete()) return null;
        switch (conc.tkind) {
          .TyClass => {
            // resolved name must match for target and source
            // TODO: this would need to be updated when class inheritance is implemented
            if (conc.name != null and source.concrete().name != null) {
              if (std.mem.eql(u8, conc.name.?, source.concrete().name.?)) {
                return target;
              }
            }
          },
          else => {}
        }
      },
      .Union => |*union_| {
        if (source.isUnion() and target.typeid() == source.typeid()) {
          if (source.union_().active) |active| {
            union_.active = active;
          }
          return target;
        }
        if (target.containsType(source)) {
          union_.active = source;
          return target;
        }
      },
      .Nullable => |nul| {
        // NOTE: nullable type does not distribute over it's subtype.
        // For example: (str | num)? !== str? | num?
        // The subtype of a nullable type is its **own concrete** type.
        if (source.isNilTy()) return target;
        if (self._checkAssign(nul.subtype, source) != null) {
          return target;
        }
      },
      .Generic => |*gen| {
        if (source.isGeneric()) {
          var s_gen = source.generic();
          if (self._checkAssign(gen.base, s_gen.base) == null) return null;
          // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
          if (gen.tparams_len() == 0) return source;
          self.validateGenericParamSize(gen.tparams_len(), s_gen.tparams_len(), source.debug) catch return null;
          for (gen.tparams.items, 0..) |param, i| {
            var res = self._checkAssign(param, s_gen.tparams.items[i]);
            if (res == null) return res;
          }
          return target;
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
