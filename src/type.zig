const std = @import("std");
const util = @import("util.zig");
const VarNode = @import("ast.zig").VarNode;
const Token = @import("lex.zig").Token;

const ID_HASH = 0x12;
const MAX_STEPS = MAX_RECURSIVE_DEPTH / 2;
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
  /// recursive flag
  recursive: bool = false,

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

  pub fn clone(self: *@This()) @This() {
    return Union{
      .variants = self.variants.clone() catch std.os.exit(12),
      .active = self.active,
      .recursive = self.recursive,
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

  pub fn clone(self: *@This()) @This() {
    return Generic{
      .base = self.base,
      .tparams = self.tparams.clone() catch std.os.exit(12)
    };
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

  pub fn clone(self: *TypeInfo) TypeInfo {
    return switch (self.*) {
      .Union => |*t| .{.Union = t.clone()},
      .Generic => |*t| .{.Generic = t.clone()},
      else => self.*,
    };
  }
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
  pub fn isGeneric(self: *Self) bool {
    return switch (self.kind) {
      .Generic => true,
      else => false,
    };
  }

  /// a nullable type
  pub fn isNullable(self: *Self) bool {
    return switch (self.kind) {
      .Nullable => true,
      else => false,
    };
  }

  /// a union type
  pub fn isUnion(self: *Self) bool {
    return switch (self.kind) {
      .Union => true,
      else => false,
    };
  }

  /// a name/variable type
  pub fn isVariable(self: *Self) bool {
    return switch (self.kind) {
      .Variable => true,
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
        for (uni.variants.values()) |ty| {
          self.tid += ty.typeid();
        }
      },
      .Nullable => |nul| {
        self.tid = 6 << ID_HASH;
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
    }
    std.debug.assert(self.tid != 0);
    return self.tid;
  }

  pub fn recContainsType(self: *Self, typ: *Self) bool {
    _ = typ;
    _ = self;
  }

  /// check if typ is contained in self. This is false if self is not a union.
  pub fn containsType(self: *Self, typ: *Self) bool {
    switch (self.kind) {
      .Union => |*uni_a| {
        switch (typ.kind) {
          .Union => |*uni_b| {
            for (uni_b.variants.values()) |ty| {
              if (!self.containsType(ty)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.values()) |ty| {
              if (ty.typeid() == typ.typeid()) {
                return true;
              }
            }
          }
        }
      },
      else => return false
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
          if (uni.recursive and typ.isUnion()) {
            _ = try writer.write("{...}");
          } else {
            _ = try writer.write(try typ._typename(allocator, depth));
            if (i != values.len - 1) {
              _ = try writer.write(" | ");
            }
          }
        }
        return writer.context.items;
      },
      .Variable => |*vr| try writeName(allocator, &vr.tokens),
    };
  }

  pub fn typename(self: *Self, allocator: std.mem.Allocator) []const u8 {
    var depth: usize = 0;
    return self._typename(allocator, &depth) catch "";
  }
};
