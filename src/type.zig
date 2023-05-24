const std = @import("std");
const util = @import("util.zig");
const VarNode = @import("ast.zig").VarNode;
const Token = @import("lex.zig").Token;

const ID_HASH = 0x12;
pub const MAX_STEPS = MAX_RECURSIVE_DEPTH / 2;
pub const MAX_TPARAMS = 0xA;
pub const MAX_RECURSIVE_DEPTH = 0x3e8;
pub const TypeHashSet = std.AutoArrayHashMap(u32, *Type);
pub const TypeList = std.ArrayList(*Type);

pub const TypeKind = enum (u8) {
  /// 'type' type:
  /// type of all types
  TyType,
  /// boolean type:
  ///  bool
  TyBool,
  /// number type:
  ///  num
  TyNumber,
  /// string type:
  ///  str
  TyString,
  /// nil type:
  ///  nil
  TyNil,
  /// class type:
  ///  ex. list, map, etc.
  TyClass,
};

fn allocate(comptime T: type, alloc: std.mem.Allocator) *T {
  return alloc.create(T) catch |e| {
    std.debug.print("{}", .{e});
    std.os.exit(1);
  };
}

pub const Concrete = struct {
  /// kind of this Concrete type
  tkind: TypeKind,
  /// resolved name of this Concrete type
  name: ?[]const u8 = null,
  /// Variable from which and for which this Concrete type was created
  variable: ?Variable = null,

  pub fn init(tkind: TypeKind) @This() {
    return @This() {.tkind = tkind};
  }

  pub fn toType(self: Concrete, debug: Token) Type {
    return Type.init(.{.Concrete = self}, debug);
  }

  pub fn isRelatedTo(this: *Concrete, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    _ = A;
    _ = ctx;
    switch (other.kind) {
      // Concrete & Concrete 
      .Concrete => |*conc| {
        if (conc.tkind != .TyClass) return conc.tkind == this.tkind;
        // resolved name must match for target and source
        // TODO: this would need to be updated with subtype rules when class inheritance is implemented
        if (this.name != null and conc.name != null) {
          if (std.mem.eql(u8, this.name.?, conc.name.?)) {
            return true;
          }
        }
      },
      .Union, .Generic, .Variable, .Recursive => return false,
    }
    return false;
  }
};

pub const Union = struct {
  /// each discriminant of this union
  variants: TypeHashSet,
  /// the active type in the union
  active: ?*Type = null,
  is_nullable: bool = false,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return Union{.variants = TypeHashSet.init(allocator)};
  }

  pub fn toType(self: Union, debug: Token) Type {
    return Type.init(.{.Union = self}, debug);
  }

  pub fn set(self: *@This(), typ: *Type) void {
    if (!typ.isUnion()) {
      util.set(u32, *Type, &self.variants, typ.typeid(), typ);
    } else {
      var uni = typ.union_();
      for (uni.variants.values()) |vr| {
        util.set(u32, *Type, &self.variants, vr.typeid(), vr);
      }
    }
    if (typ.isNullable() or typ.isNilTy()) {
      self.is_nullable = true;
    }
  }

  pub fn addAll(self: *@This(), types: *TypeList) void {
    for (types.items) |ty| {
      self.set(ty);
    }
  }

  pub fn isRelatedTo(this: *Union, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    // this -> T1, other -> T2
    switch (other.kind) {
      .Union => {
        if (ctx == .RCTypeParams) {
          // .RCTypeParams constrains to 'exactness'
          if (this.variants.count() != other.union_().variants.count()) {
            return false;
          }
        }
        // related if each & every variants of T2 are related to any of the variants of T1 with given context
        for (other.union_().variants.values()) |variant| {
          if (!this.isRelatedTo(variant, .RCAny, A)) {
            return false;
          }
        }
        return true;
      },
      .Concrete, .Generic, .Variable, .Recursive => {
        if (ctx == .RCTypeParams) return false;
        // related if there exists a variant of T1 that is related to T2
        for (this.variants.values()) |variant| {
          if (variant.isRelatedTo(other, ctx, A)) {
            return true;
          }
        }
      }
    }
    return false;
  }
};

pub const Generic = struct {
  base: *Type,
  tparams: TypeList,

  pub fn init(allocator: std.mem.Allocator, base: *Type) @This() {
    return Generic {.tparams = TypeList.init(allocator), .base = base};
  }

  pub fn toType(self: Generic, debug: Token) Type {
    return Type.init(.{.Generic = self}, debug);
  }

  pub fn getSlice(self: *@This()) []*Type {
    return self.tparams.items[0..self.tparams.items.len];
  }

  pub fn append(self: *@This(), typ: *Type) void {
    util.append(*Type, &self.tparams, typ);
  }

  pub inline fn tparams_len(self: *@This()) usize {
    return self.tparams.items.len;
  }

  pub fn isRelatedTo(this: *Generic, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    switch (other.kind) {
      .Generic => |*gen| {
        if (!this.base.isRelatedTo(gen.base, ctx, A)) return false;
        // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
        if (gen.tparams_len() == 0) return true;
        if (this.tparams_len() != gen.tparams_len()) return false;
        for (this.tparams.items, 0..) |tparam, i| {
          var param = gen.tparams.items[i];
          if (!tparam.isRelatedTo(param, .RCTypeParams, A)) {
            return false;
          }
        }
        return true;
      },
      .Concrete, .Union, .Variable, .Recursive => return false,
    }
    return false;
  }
};

pub const Variable = struct {
  tokens: std.ArrayList(Token),

  pub fn init(allocator: std.mem.Allocator) @This() {
    return Variable {.tokens = std.ArrayList(Token).init(allocator)};
  }

  pub fn append(self: *@This(), name: Token) void {
    util.append(Token, &self.tokens, name);
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    if (other.tokens.items.len != self.tokens.items.len) return false;
    for (other.tokens.items, self.tokens.items) |a, b| {
      if (!std.mem.eql(u8, a.value, b.value)) {
        return false;
      }
    }
    return true;
  }

  pub fn isRelatedTo(this: *Variable, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    _ = A;
    _ = ctx;
    return switch (other.kind) {
      .Variable => |*vr| this.eql(vr),
      .Concrete, .Union, .Recursive, .Generic => false,
    };
  }
};

pub const Recursive = struct {
  base: *Type,

  pub fn init(base: *Type) @This() {
    return @This() {.base = base};
  }

  pub fn isRelatedTo(this: *Recursive, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    // if (ctx == .RCTypeParams) return false;
    var flat_this = this.base.unfold(A);
    var flat_other = other.unfold(A);
    start:
    for (flat_other.items) |ty| {
      for (flat_this.items) |rty| {
        if (rty.isRelatedTo(ty, ctx, A)) {
          continue :start;
        }
      }
      return false;
    }
    return true;
  }
};

pub const AliasInfo = struct {
  lhs: *Type, // alias 
  rhs: *Type, // aliasee

  pub fn init(lhs: *Type, rhs: *Type) @This() {
    return @This() {.lhs = lhs, .rhs = rhs};
  }
};

pub const TypeInfo = union(enum) {
  Concrete: Concrete,
  Variable: Variable,
  Union: Union,
  Generic: Generic,
  Recursive: Recursive,
};

/// context for inspecting relation rules 
const RelationContext = enum(u8) {
  /// any context
  RCAny,
  /// in generic type parameter context
  RCTypeParams,
};

pub const Type = struct {
  tid: u32 = 0,
  alias_info: ?AliasInfo = null,
  ident: ?*VarNode = null,
  debug: Token,
  kind: TypeInfo,

  const Self = @This();

  pub fn init(data: TypeInfo, debug: Token) Self {
    return Self {.kind = data, .debug = debug};
  }

  pub fn box(self: Self, allocator: std.mem.Allocator) *Self {
    var al = allocate(Self, allocator);
    al.* = self;
    return al;
  }

  fn setRestFields(ty1: *Self, ty2: *Self) void {
    ty1.tid = ty2.tid;
    ty1.alias_info = ty2.alias_info;
    ty1.ident = ty2.ident;
    // ty1.debug = ty2.debug;
  }

  pub fn clone(self: *Self, A: std.mem.Allocator) *Self {
    switch (self.kind) {
      .Concrete, .Variable, .Recursive => return self,
      .Generic => |*gen| {
        var new = Generic.init(A, gen.base.clone(A));
        new.tparams.ensureTotalCapacity(gen.tparams.capacity) catch {};
        for (gen.tparams.items) |ty| {
          new.append(ty.clone(A));
        }
        var ret = Type.init(.{.Generic = new}, self.debug).box(A);
        ret.setRestFields(self);
        return ret;
      },
      .Union => |*uni| {
        var new = Union.init(A);
        new.active = uni.active;
        new.variants.ensureTotalCapacity(uni.variants.capacity()) catch {};
        for (uni.variants.values()) |ty| {
          new.set(ty.clone(A));
        }
        var ret = Type.init(.{.Union = new}, self.debug).box(A);
        ret.setRestFields(self);
        return ret;
      },
    }
  }

  pub fn newConcrete(kind: TypeKind, name: ?[]const u8, debug: Token) Self {
    var conc = Concrete.init(kind);
    conc.name = name;
    return Self.init(.{.Concrete = conc}, debug);
  }

  pub fn newNullable(ty: *Self, debug: Token, al: std.mem.Allocator) *Self {
    if (ty.isUnion()) {
      var uni = ty.union_();
      if (uni.is_nullable) return ty;
      uni.set(Type.newConcrete(.TyNil, null, debug).box(al));
      uni.is_nullable = true;
      return ty;
    } else {
      var typ = Type.newUnion(al, debug);
      var uni = typ.union_();
      uni.set(ty);
      uni.set(Type.newConcrete(.TyNil, null, debug).box(al));
      uni.is_nullable = true;
      return typ.box(al);
    }
  }

  pub fn newVariable(allocator: std.mem.Allocator, debug: Token) Self {
    var vr = Variable.init(allocator);
    return Self.init(.{.Variable = vr}, debug);
  }

  pub fn newUnion(allocator: std.mem.Allocator, debug: Token) Self {
    var uni = Union.init(allocator);
    return Self.init(.{.Union = uni}, debug);
  }

  pub fn newGeneric(allocator: std.mem.Allocator, base: *Self, debug: Token) Self {
    var gen = Generic.init(allocator, base);
    return Self.init(.{.Generic = gen}, debug);
  }

  pub fn newRecursive(base: *Self, debug: Token) Self {
    var rec = Recursive.init(base);
    return Self.init(.{.Recursive = rec}, debug);
  }

  pub fn newNever(allocator: std.mem.Allocator, debug: Token) Self {
    var ty = newVariable(allocator, debug);
    var nvr = Token.tokenFrom(@constCast(&debug));
    nvr.value = "never";
    ty.variable().append(nvr);
    return ty;
  }

  pub fn subtype(self: *Self, al: std.mem.Allocator) *Type {
    std.debug.assert(self.isNullable());
    var sub = TypeHashSet.init(al);
    for (self.nullable().variants.values()) |ty| {
      if (!ty.isNilTy()) {
        util.set(u32, *Type, &sub, ty.typeid(), ty);
      }
    }
    return compressTypes(&sub, self.debug, null);
  }
 
  /// simple/concrete 'unit' type
  pub inline fn isSimple(self: *Self) bool {
    return switch (self.kind) {
      .Concrete => true,
      else => false,
    };
  }

  /// simple/concrete 'unit' type
  pub inline fn isConcrete(self: *Self) bool {
    return self.isSimple();
  }

  /// a compound type that may also be generic
  pub inline fn isCompound(self: *Self) bool {
    return !self.isSimple();
  }

  /// a type that may require some form of substitution
  pub inline fn isGeneric(self: *Self) bool {
    return switch (self.kind) {
      .Generic => true,
      else => false,
    };
  }

  /// a nullable type
  pub inline fn isNullable(self: *Self) bool {
    return switch (self.kind) {
      .Union => |*uni| uni.is_nullable,
      else => false,
    };
  }

  /// a union type
  pub inline fn isUnion(self: *Self) bool {
    return switch (self.kind) {
      .Union => true,
      else => false,
    };
  }

  /// a name/variable type
  pub inline fn isVariable(self: *Self) bool {
    return switch (self.kind) {
      .Variable => true,
      else => false,
    };
  }

  /// a recursive type
  pub inline fn isRecursive(self: *Self) bool {
    return switch (self.kind) {
      .Recursive => true,
      else => false,
    };
  }

  /// a type that may be generic (from annotation usage)
  pub inline fn isLikeGeneric(self: *Self) bool {
    if (self.isGeneric()) return true;
    if (self.isUnion()) {
      for (self.union_().variants.values()) |vr| {
        if (vr.isGeneric()) {
          return true;
        }
      }
    }
    return false;
  }

  inline fn isConcreteTypeEq(self: *Self, kind: TypeKind) bool {
    return switch (self.kind) {
      .Concrete => |conc| conc.tkind == kind,
      else => false,
    };
  }

  inline fn isXClassTy(self: *Self, name: []const u8) bool {
    if (self.isGeneric()) {
      var gen = self.generic();
      if (gen.base.isClassTy()) {
        return std.mem.eql(u8, gen.base.concrete().name.?, name);
      }
    }
    return false;
  }

  /// more qol helper methods
  pub inline fn isBoolTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyBool);
  }

  pub inline fn isStrTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyString);
  }

  pub inline fn isNumTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyNumber);
  }

  pub inline fn isNilTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyNil);
  }

  pub inline fn isClassTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyClass);
  }

  pub inline fn isTypeTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyType);
  }

  pub fn isListTy(self: *Self) bool {
    return self.isXClassTy("list");
  }

  pub fn isMapTy(self: *Self) bool {
    return self.isXClassTy("map");
  }

  /// extract the appropriate typeinfo of this type
  pub inline fn nullable(self: *Self) *Union {
    return &self.kind.Union;
  }

  pub inline fn concrete(self: *Self) Concrete {
    return self.kind.Concrete;
  }

  pub inline fn generic(self: *Self) *Generic {
    return &self.kind.Generic;
  }

  pub inline fn variable(self: *Self) *Variable {
    return &self.kind.Variable;
  }

  pub inline fn union_(self: *Self) *Union {
    return &self.kind.Union;
  }

  pub inline fn recursive(self: *Self) Recursive {
    return self.kind.Recursive;
  }

  pub fn debugToken(self: *Self) Token {
    if (self.ident) |ident| {
      return ident.token;
    }
    return self.debug;
  }

  pub fn getName(self: *Self) []const u8 {
    return switch (self.kind) {
      .Variable => |name| name.tokens.getLast().value,
      else => "",
    };
  }

  pub fn typeid(self: *Self) u32 {
    if (self.tid != 0) return self.tid;
    switch (self.kind) {
      .Concrete => |conc| {
        switch (conc.tkind) {
          .TyBool =>   self.tid = 1 << ID_HASH,
          .TyNumber => self.tid = 2 << ID_HASH,
          .TyString => self.tid = 3 << ID_HASH,
          .TyNil =>    self.tid = 4 << ID_HASH,
          .TyClass =>  self.tid = 5 << ID_HASH,
          .TyType =>   self.tid = 12 << ID_HASH,
        }
      },
      .Generic => |*gen| {
        self.tid = gen.base.typeid();
        for (gen.getSlice()) |typ| {
          self.tid += typ.typeid();
        }
      },
      .Union => |*uni| {
        self.tid = 6 << ID_HASH;
        for (uni.variants.values()) |ty| {
          self.tid += ty.typeid();
          self.tid <<= 1; // mix
        }
      },
      .Variable => |vr| {
        for (vr.tokens.items) |tok| {
          self.tid += @as(u32, @enumToInt(tok.ty)) << ID_HASH;
          // TODO: more efficient approach
          for (tok.value) |ch| {
            self.tid += @as(u8, ch);
          }
        }
      },
      .Recursive => |rec| {
        self.tid = rec.base.typeid();
      }
    }
    std.debug.assert(self.tid != 0);
    return self.tid;
  }

  pub inline fn typeidEql(self: *Self, other: *Self) bool {
    return self.typeid() == other.typeid();
  }

  fn _unfoldRecursive(typ: *Self, step: usize, list: *TypeList, visited: *TypeHashSet) void {
    if (step > 0 and typ.isRecursive() and visited.get(typ.typeid()) != null) {
      return;
    }
    switch (typ.kind) {
      .Concrete => util.append(*Type, list, typ),
      .Variable => util.append(*Type, list, typ),
      .Generic => |*gen| {
        gen.base._unfoldRecursive(step + 1, list, visited);
        for (gen.tparams.items) |param| {
          param._unfoldRecursive(step + 1, list, visited);
        }
      },
      .Union => |uni| {
        for (uni.variants.values()) |ty| {
          ty._unfoldRecursive(step + 1, list, visited);
        }
      },
      .Recursive => |rec| {
        util.set(u32, *Type, visited, typ.typeid(), typ);
        rec.base._unfoldRecursive(step + 1, list, visited);
      }
    }
  }

  fn _unfold(self: *Self, list: *TypeList) void {
    switch (self.kind) {
      .Concrete => util.append(*Type, list, self),
      .Variable => util.append(*Type, list, self),
      .Generic => |*gen| {
        gen.base._unfold(list);
        for (gen.tparams.items) |param| {
          param._unfold(list);
        }
      },
      .Union => |uni| {
        for (uni.variants.values()) |ty| {
          ty._unfold(list);
        }
      }, 
      .Recursive => |rec| {
        var visited = TypeHashSet.init(list.allocator);
        util.set(u32, *Type, &visited, self.typeid(), self);
        rec.base._unfoldRecursive(0, list, &visited);
        visited.clearAndFree();
      }
    }
  }

  fn unfold(self: *Self, allocator: std.mem.Allocator) TypeList {
    // TODO: use TypeHashSet for unfold?
    var list = TypeList.init(allocator);
    self._unfold(&list);
    return list;
  }

  fn recContainsType(rec_list: *TypeList, o_list: *TypeList) bool {
    start: 
    for (o_list.items) |ty| {
      for (rec_list.items) |rty| {
        if (ty.typeidEql(rty)) {
          continue :start;
        }
      }
      return false;
    }
    return true;
  }

  pub fn isRelatedTo(this: *Self, other: *Self, ctx: RelationContext, A: std.mem.Allocator) bool {
    // We use target & source as in assignment/cast target and assignment/cast source respectively. 
    // Context ctx is the context in which such relation is being performed.
    // Target.          Source    Context
    // this.     &.      other.     ctx
    if (this == other) return true;
    switch (this.kind) {
      .Concrete => |*conc| {
        return conc.isRelatedTo(other, ctx, A);
      },
      .Union => |*uni| {
        return uni.isRelatedTo(other, ctx, A);
      },
      .Generic => |*gen| {
        return gen.isRelatedTo(other, ctx, A);
      },
      .Variable => |*vr| {
        return vr.isRelatedTo(other, ctx, A);
      },
      .Recursive => |*rec| {
        return rec.isRelatedTo(other, ctx, A);
      },
    }
    unreachable;
  }

  pub inline fn isEitherWayRelatedTo(this: *Self, other: *Self, ctx: RelationContext, A: std.mem.Allocator) bool {
    return this.isRelatedTo(other, ctx, A) or other.isRelatedTo(this, ctx, A);
  }

  pub fn canBeCastTo(node_ty: *Type, cast_ty: *Type, A: std.mem.Allocator) error{CastError, UnionCastError}!*Type {
    switch (cast_ty.kind) {
      .Concrete => |conc| {
        switch (conc.tkind) {
          // any type may be cast to bool
          .TyBool => return cast_ty,
          .TyNil => return error.CastError,
          else => {
            if (cast_ty.isRelatedTo(node_ty, .RCAny, A)) {
              return cast_ty;
            }
          },
        }
      },
      .Union => |*uni| {
        // upcasting/widening
        if (cast_ty.isRelatedTo(node_ty, .RCAny, A)) {
          // keep track of the active
          uni.active = node_ty;
          return cast_ty;
        }
      },
      else => {
        if (cast_ty.isRelatedTo(node_ty, .RCAny, A)) {
          return cast_ty;
        }
      }
    }
    // downcasting
    if (node_ty.castContainsType(cast_ty, A)) {
      if (node_ty.union_().active) |active| {
        // check if the active type can be cast to the cast type
        _ = active.canBeCastTo(cast_ty, A) catch return error.UnionCastError;
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
    if (node_ty.typeidEql(cast_ty)) {
      return cast_ty;
    }
    return error.CastError;
  }
  
  pub fn canBeAssigned(target: *Self, source: *Self, A: std.mem.Allocator) ?*Type {
    switch (target.kind) {
      .Union => |*uni| {
        if (target.isRelatedTo(source, .RCAny, A)) {
          uni.active = if (source.isUnion()) source.union_().active else source;
          return target;
        }
      },
      else => {
        if (target.isRelatedTo(source, .RCAny, A)) {
          return target;
        }
      },
    }
    // lowest precedence
    if (target.typeidEql(source)) {
      return target;
    }
    return null;
  }

  /// check if `source` is contained in `target`. This is false if `target` is not a union.
  pub fn assignContainsType(target: *Self, source: *Self, A: std.mem.Allocator) bool {
    switch (target.kind) {
      .Union => |*uni_a| {
        switch (source.kind) {
          .Union => |*uni_b| {
            for (uni_b.variants.values()) |ty| {
              if (!target.assignContainsType(ty, A)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.values()) |ty| {
              if (ty.typeidEql(source)) {
                return true;
              } else if (ty.canBeAssigned(source, A)) |_| {
                return true;
              }
            }
          }
        }
      },
      else => {}
    }
    return false;
  }

  /// check if `cast_ty` is contained in `node_ty`. This is false if `node_ty` is not a union.
  pub fn castContainsType(node_ty: *Self, cast_ty: *Self, A: std.mem.Allocator) bool {
    switch (node_ty.kind) {
      .Union => |*uni_a| {
        switch (cast_ty.kind) {
          .Union => |*uni_b| {
            for (uni_b.variants.values()) |ty| {
              if (!node_ty.castContainsType(ty, A)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.values()) |ty| {
              if (ty.typeidEql(cast_ty)) {
                return true;
              } else if (cast_ty.canBeCastTo(ty, A) catch null) |_| {
                return true;
              }
            }
          }
        }
      },
      else => {}
    }
    return false;
  }

  fn writeName(allocator: std.mem.Allocator, tokens: *std.ArrayList(Token)) ![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
    for (tokens.items, 0..) |tok, i| {
      _ = try writer.write(tok.value);
      if (i != tokens.items.len - 1) {
        // compound names are separated via '.'
        _ = try writer.write(".");
      }
    }
    return writer.context.items;
  }

  fn _typename(self: *Self, allocator: std.mem.Allocator, depth: *usize) ![]const u8 {
    depth.* = depth.* + 1;
    if (depth.* > MAX_STEPS) return "...";
    return switch (self.kind) {
      .Concrete => |conc| switch (conc.tkind) {
        .TyBool =>   "bool",
        .TyNumber => "num",
        .TyString => "str",
        .TyNil =>    "nil",
        .TyType => "Type",
        .TyClass => conc.name.?,
      },
      .Generic => |*gen| {
        const name = try gen.base._typename(allocator, depth);
        if (gen.tparams_len() == 0) {
          return name;
        } else {
          var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
          _ = try writer.write(name);
          _ = try writer.write("{");
          for (gen.getSlice(), 0..) |param, i| {
            _ = try writer.write(try param._typename(allocator, depth));
            if (i != gen.tparams.items.len - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
          return writer.context.items;
        }
      },
      .Union => |*uni| {
        var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
        var values = uni.variants.values();
        for (values, 0..) |typ, i| {
          _ = try writer.write(try typ._typename(allocator, depth));
          if (i != values.len - 1) {
            _ = try writer.write(" | ");
          }
        }
        return writer.context.items;
      },
      .Variable => |*vr| try writeName(allocator, &vr.tokens),
      .Recursive => "{...}"
    };
  }

  pub fn typename(self: *Self, allocator: std.mem.Allocator) []const u8 {
    var depth: usize = 0;
    if (self.alias_info) |info| return info.lhs._typename(allocator, &depth) catch "";
    return self._typename(allocator, &depth) catch "";
  }

  /// combine types in typeset as much as possible
  pub fn compressTypes(typeset: *TypeHashSet, debug: Token, uni: ?*Type) *Type {
    if (typeset.count() > 1) {
      var final = TypeList.init(typeset.allocator);
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
          if (ty.isRelatedTo(typ, .RCAny, typeset.allocator)) {
            continue;
          }
        }
        last_ty = typ;
        final.append(typ) catch break;
      }
      // convert types to a single union type
      return blk: {
        var tmp: *Type = undefined;
        if (final.items.len == typeset.count()) {
          if (uni) |ty| {
            return ty;
          }
        }
        if (final.items.len > 1) {
          tmp = Type.newUnion(typeset.allocator, debug).box(typeset.allocator);
          tmp.union_().addAll(&final);
        } else {
          tmp = final.items[0];
        }
        if (has_nil) {
          tmp = Type.newNullable(tmp, debug, typeset.allocator);
        }
        break :blk tmp;
      };
    }
    return typeset.values()[0];
  }

  /// combine t1 and t2 into a union type if possible
  pub fn unionify(t1: *Type, t2: *Type, allocator: std.mem.Allocator) *Type {
    if (t1.typeid() == t2.typeid()) {
      return t1;
    }
    if (t1.isNilTy() and t2.isNilTy()) {
      // TODO: should we err?
      return t1;
    }
    if (t1.isNilTy()) {
      if (t2.isNullable()) return t2;
      return t2.newNullable(t2.debug, allocator);
    } else if (t2.isNilTy()) {
      if (t1.isNullable()) return t1;
      return t1.newNullable(t1.debug, allocator);
    } else {
      if (t1.isRelatedTo(t2, .RCAny, allocator)) {
        return t1;
      }
      if (t2.isUnion()) {
        var variants = t2.union_().variants.clone() catch |e| {
          std.debug.print("Error: {}", .{e});
          std.os.exit(1);
        };
        util.set(u32, *Type, &variants, t1.typeid(), t1);
        return compressTypes(&variants, t2.debug, null);
      }
      if (t2.isRelatedTo(t1, .RCAny, allocator)) {
        return t2;
      }
      if (t1.isUnion()) {
        var variants = t1.union_().variants.clone() catch |e| {
          std.debug.print("Error: {}", .{e});
          std.os.exit(1);
        };
        util.set(u32, *Type, &variants, t2.typeid(), t2);
        return compressTypes(&variants, t1.debug, null);
      }
      var tmp = Type.newUnion(allocator, t1.debug).box(allocator);
      tmp.union_().set(t1);
      tmp.union_().set(t2);
      return tmp;
    }
  }

  /// find the intersection of t1 and t2
  pub fn intersect(t1: *Type, t2: *Type, allocator: std.mem.Allocator) ?*Type {
    if (t1.typeid() == t2.typeid()) {
      return t1;
    }
    if (t1.isRelatedTo(t2, .RCAny, allocator)) {
      switch (t1.kind) {
        .Union => |*uni| {
          var uni2 = TypeHashSet.init(allocator);
          for (uni.variants.values()) |typ| {
            if (typ.intersect(t2, allocator)) |ty| {
              util.set(u32, *Type, &uni2, ty.typeid(), ty);
            }
          }
          return compressTypes(&uni2, t1.debug, null);
        },
        .Recursive => {
          var typs = t1.unfold(allocator);
          for (typs.items) |typ| {
            if (typ.intersect(t2, allocator)) |ty| {
              return ty;
            }
          }
        },
        else => return t1,
      }
    } else if (t2.isRelatedTo(t1, .RCAny, allocator)) {
      switch (t2.kind) {
        .Union => |*uni| {
          var uni2 = TypeHashSet.init(allocator);
          for (uni.variants.values()) |typ| {
            if (typ.intersect(t1, allocator)) |ty| {
              util.set(u32, *Type, &uni2, ty.typeid(), ty);
            }
          }
          return compressTypes(&uni2, t1.debug, null);
        },
        .Recursive => {
          var typs = t2.unfold(allocator);
          for (typs.items) |typ| {
            if (typ.intersect(t1, allocator)) |ty| {
              return ty;
            }
          }
        },
        else => return t2,
      }
    }
    return null;
  }

  /// negate t2 from t1 -> t1\t2
  pub fn negate(t1: *Type, t2: *Type, allocator: std.mem.Allocator) error{Negation}!*Type {
    switch (t1.kind) {
      .Union => |*uni| {
        var new_uni = TypeHashSet.init(allocator);
        for (uni.variants.values()) |ty| {
          if (!ty.isEitherWayRelatedTo(t2, .RCAny, allocator)) {
            util.set(u32, *Type, &new_uni, ty.typeid(), ty);
          }
        }
        if (new_uni.count() == 0) {
          return error.Negation;
        }
        return Self.compressTypes(&new_uni, t1.debug, null);
      },
      .Concrete, .Generic, .Variable, => return error.Negation,
      // TODO: c'est fini?
      .Recursive => return t1,
    }
  }

  /// t1 is t2 -> select t2 from t1
  pub fn is(t1: *Type, t2: *Type, al: std.mem.Allocator) ?*Type {
    // ex: is(str | list{num}, list) -> list{num}
    switch (t1.kind) {
      .Concrete => |conc| {
        if (!t2.isConcrete()) return null;
        if (conc.tkind != .TyClass) {
          if (conc.tkind == t2.concrete().tkind) {
            return t1;
          }
        } else {
          if (!t2.isClassTy()) return null;
          // TODO: class relations
          if (std.mem.eql(u8, conc.name.?, t2.concrete().name.?)) {
            return t1;
          }
        }
      },
      .Generic => |*t1_gen| {
        if (!t2.isGeneric()) return null;
        var t2_gen = t2.generic();
        if (is(t1_gen.base, t2_gen.base, al) == null) return null;
        std.debug.assert(t2_gen.tparams_len() == 0);
        return t1;
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          if (is(ty, t2, al)) |typ| {
            return typ;
          }
        }
      },
      .Variable => |*vr| {
        if (!t2.isVariable()) return null;
        if (vr.eql(t2.variable())) {
          return t1;
        }
      },
      // TODO:
      .Recursive => return t1,
    }
    return null;
  }
};
