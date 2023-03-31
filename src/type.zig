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
};

pub const Union = struct {
  /// each discriminant of this union
  variants: TypeHashSet,
  /// the active type in the union
  active: ?*Type = null,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return Union{.variants = TypeHashSet.init(allocator)};
  }

  pub fn toType(self: Union, debug: Token) Type {
    return Type.init(.{.Union = self}, debug);
  }

  pub fn set(self: *@This(), typ: *Type) void {
    self.variants.put(typ.typeid(), typ) catch |e| {
      std.debug.print("error: {}", .{e});
      std.os.exit(1);
    };
  }

  pub fn addAll(self: *@This(), typeset: *TypeHashSet) void {
    for (typeset.values()) |ty| {
      self.set(ty);
    }
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

  pub fn tparams_len(self: *@This()) usize {
    return self.tparams.items.len;
  }
};

pub const Nullable = struct {
  subtype: *Type,

  pub fn init(subtype: *Type) @This() {
    return @This() {.subtype = subtype};
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
};

pub const Recursive = struct {
  base: *Type,

  pub fn init(base: *Type) @This() {
    return @This() {.base = base};
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
  Nullable: Nullable,
  Variable: Variable,
  Union: Union,
  Generic: Generic,
  Recursive: Recursive,
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
      .Concrete, .Variable, .Nullable, .Recursive => return self,
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

  pub fn newNullable(subtype: *Self, debug: Token) Self {
    var nul = Nullable.init(subtype);
    return Self.init(.{.Nullable = nul}, debug);
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
      .Nullable => true,
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

  fn isConcreteTypeEq(self: *Self, kind: TypeKind) bool {
    return switch (self.kind) {
      .Concrete => |conc| conc.tkind == kind,
      else => false,
    };
  }

  /// more qol helper methods
  pub fn isBoolTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyBool);
  }

  pub fn isStrTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyString);
  }

  pub fn isNumTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyNum);
  }

  pub fn isNilTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyNil);
  }

  pub fn isClassTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyClass);
  }

  /// extract the appropriate typeinfo of this type
  pub inline fn nullable(self: *Self) Nullable {
    return self.kind.Nullable;
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

  fn checkVariable(self: *Self, startStep: usize, maxSteps: usize) !bool {
    if (startStep >= maxSteps) {
      return error.PotentiallyInfiniteSteps;
    }
    return switch (self.kind) {
      .Variable => true,
      .Nullable => |nul| try nul.subtype.checkVariable(startStep + 1, maxSteps),
      .Generic => |*gen| if (gen.base.isVariable()) true else false,
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          if (try ty.checkVariable(startStep + 1, maxSteps)) return true;
        }
        return false;
      },
      else => false,
    };
  }

  pub fn hasVariable(self: *Self, maxSteps: usize) !bool {
    return try self.checkVariable(0, maxSteps);
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
        }
      },
      .Nullable => |nul| {
        self.tid = 7 << ID_HASH;
        self.tid += nul.subtype.typeid();
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

  fn getNullVariableType(self: *Self, allocator: std.mem.Allocator, debug: Token) *Type {
    _ = self;
    var nul = Self.newVariable(allocator, debug);
    var token = Token.getDefault();
    (&token).* = debug;
    token.value = "$$null$$";
    token.ty = .TkIdent;
    nul.variable().append(token);
    return nul.box(allocator);
  }

  fn _unfoldRecursive(typ: *Self, step: usize, list: *TypeList, visited: *TypeHashSet) void {
    if (step > 0 and typ.isRecursive() and visited.get(typ.typeid()) != null) {
      return;
    }
    switch (typ.kind) {
      .Concrete => util.append(*Type, list, typ),
      .Variable => util.append(*Type, list, typ),
      .Nullable => |nul| {
        util.append(*Type, list, typ.getNullVariableType(list.allocator, typ.debug));
        nul.subtype._unfoldRecursive(step + 1, list, visited);
      },
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
      .Nullable => |nul| {
        util.append(*Type, list, self.getNullVariableType(list.allocator, self.debug));
        nul.subtype._unfold(list);
      },
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
        if (ty.typeid() == rty.typeid()) {
          continue :start;
        }
      }
      return false;
    }
    return true;
  }

  pub fn canBeCastTo(node_ty: *Type, cast_ty: *Type, A: std.mem.Allocator) error{CastError}!*Type {
    if (cast_ty.isNilTy()) {
      return error.CastError;
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
        return error.CastError;
      }
    }
    // a nullable type may be cast to bool or an assignable-nullable type
    if (node_ty.isNullable()) {
      if (cast_ty.isNullable()) {
        _ = try node_ty.nullable().subtype.canBeCastTo(cast_ty.nullable().subtype, A);
        return cast_ty;
      }
      return error.CastError;
    }
    // any type may be cast to nullable of that type, i.e. type -> type?
    if (cast_ty.isNullable()) {
      _ = try node_ty.canBeCastTo(cast_ty.nullable().subtype, A);
      return cast_ty;
    }
    if (node_ty.isGeneric() and cast_ty.isGeneric()) {
      var node_gen = node_ty.generic();
      var cast_gen = cast_ty.generic();
      // check if the base types are assignable
      if (cast_gen.base.canBeAssigned(node_gen.base, A) == null) return error.CastError;
      // empty generic to specialized generic
      if (node_gen.tparams_len() == 0) return cast_ty;
      // non-empty to another type
      if (node_gen.tparams_len() != cast_gen.tparams_len()) return error.CastError;
      for (node_gen.tparams.items, 0..) |param, i| {
        _ = try param.canBeCastTo(cast_gen.tparams.items[i], A);
      }
      return cast_ty;
    }
    // upcasting/widening
    if (cast_ty.containsType(node_ty, A)) {
      std.debug.assert(cast_ty.isUnion());
      cast_ty.union_().active = node_ty;
      return cast_ty;
    }
    // downcasting
    if (node_ty.containsType(cast_ty, A)) {
      std.debug.assert(node_ty.isUnion());
      if (node_ty.union_().active) |active| {
        // check if the active type is assignable to the cast type
        if (cast_ty.canBeAssigned(active, A) == null) return error.CastError;
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
    return error.CastError;
  }

  pub fn canBeAssigned(target: *Self, source: *Self, A: std.mem.Allocator) ?*Type {
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
      .Union => |*uni| {
        if (source.isUnion() and target.typeid() == source.typeid()) {
          if (source.union_().active) |active| {
            uni.active = active;
          }
          return target;
        }
        if (target.containsType(source, A)) {
          uni.active = source;
          return target;
        }
      },
      .Nullable => |nul| {
        // NOTE: nullable type does not distribute over it's subtype.
        // For example: (str | num)? !== str? | num?
        // The subtype of a nullable type is its **own concrete** type.
        if (source.isNilTy()) return target;
        var src = if (source.isNullable()) source.nullable().subtype else source;
        if (nul.subtype.canBeAssigned(src, A) != null) {
          return target;
        }
      },
      .Generic => |*gen| {
        if (source.isGeneric()) {
          var s_gen = source.generic();
          if (gen.base.canBeAssigned(s_gen.base, A) == null) return null;
          // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
          if (gen.tparams_len() == 0) return source;
          // specific to less specific, fox ex: let x: list{str} = ['fox']; x = []
          if (s_gen.tparams_len() == 0) return target;
          // len must match
          if (s_gen.tparams_len() != gen.tparams_len()) return null;
          for (gen.tparams.items, 0..) |param, i| {
            var res = param.canBeAssigned(s_gen.tparams.items[i], A);
            if (res == null) return res;
          }
          return target;
        }
      },
      .Recursive => {
        var flat_rec = target.unfold(A);
        var flat_source = source.unfold(A);
        if (Self.recContainsType(&flat_rec, &flat_source)) {
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

  /// check if typ is contained in self. This is false if self is not a union.
  pub fn containsType(self: *Self, typ: *Self, A: std.mem.Allocator) bool {
    switch (self.kind) {
      .Union => |*uni_a| {
        switch (typ.kind) {
          .Union => |*uni_b| {
            for (uni_b.variants.values()) |ty| {
              if (!self.containsType(ty, A)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.values()) |ty| {
              if (ty.typeid() == typ.typeid()) {
                return true;
              } else if (ty.canBeAssigned(typ, A)) |_| {
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
      .Nullable => |nul| {
        var sub = try nul.subtype._typename(allocator, depth);
        if (std.mem.containsAtLeast(u8, sub, 1, "|")) {
          return std.fmt.allocPrint(allocator, "({s})?", .{sub}) catch "";
        } else {
          return std.fmt.allocPrint(allocator, "{s}?", .{sub}) catch "";
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
};
