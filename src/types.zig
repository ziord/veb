const std = @import("std");
const ast = @import("ast.zig");
const util = @import("util.zig");
const Token = @import("lex.zig").Token;

pub const MAX_TPARAMS = 0xA;
const ID_HASH = 0x12;

pub const NTypeKind = enum (u8) {
  /// boolean type: 
  ///  bool
  TyBool,
  /// number type: 
  ///  num
  TyNumber,
  /// string type: 
  ///  str
  TyString,
  /// nil type
  ///  nil
  TyNil,
  /// list type: 
  ///  list{T}
  TyList,
  /// map type: 
  ///  map{K, V}
  TyMap,
  /// type associated with names, e.g. 
  ///  Foo, A,B,C in A.B.C, T in Foo{T},
  TyName,
  /// union type: 
  ///  A | B
  TyUnion,
  /// nullable type:
  ///  type?
  TyNullable
  // TODO: func, method, class, instance
};

pub const TName = struct {
  tokens: std.ArrayList(Token),

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.tokens = std.ArrayList(Token).init(allocator)};
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

pub const TUnion = struct {
  types: std.ArrayList(*NType),
  /// the active type in the union
  active: ?*NType = null,
  recursive: bool = false,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.types = std.ArrayList(*NType).init(allocator)};
  }
};

pub const TParam = struct {
  params: [MAX_TPARAMS]*NType = undefined,
  len: usize = 0,

  pub fn getSlice(self: *@This()) []*NType {
    return self.params[0..self.len];
  }
};

pub const MAX_RECURSIVE_DEPTH = 0x3e8;
const MAX_STEPS = MAX_RECURSIVE_DEPTH / 2;

/// Nova's type representation
pub const NType = struct {
  /// the type's 'kind', which may subsume the need for other properties
  kind: NTypeKind,
  /// token for error tracing and debugging
  debug: Token,
  /// name of this type
  name: ?TName,
  /// identifier the type was declared with e.g:
  ///  let x: bool = true  ident -> 'x'
  ident: ?*ast.VarNode = null,
  /// union type
  union_: ? TUnion = null,
  /// the type to which _this main_ type serves as an alias e.g.
  ///  type X = str. `aliasee` here is str
  aliasee: ?*NType = null,
  /// the type alias of _this main_ type e.g.
  ///  type X = str. `alias` here is X
  alias: ?*NType = null,
  /// the type pointed to by a nullable type
  nsubtype: ?*NType = null,
  /// (generic) type parameters
  tparams: TParam = .{},
  /// unique id of this type
  id: u32 = 0,
  // TODO: func, class, etc.

  const Self = @This();

  pub fn init(kind: NTypeKind, name: ?TName, debug: Token) Self {
    return Self {.kind = kind, .name = name, .debug = debug};
  }

  pub fn getName(self: *Self) []const u8 {
    if (self.name) |name| {
      return name.tokens.getLast().value;
    } else {
      return "";
    }
  }

  /// non-generic type that requires no substitution
  pub inline fn isSimple(self: *Self) bool {
    return switch (self.kind) {
      .TyNumber, .TyBool, .TyString, .TyNil => true,
      else => false,
    };
  }

  /// a compound type that may also be generic
  pub inline fn isCompound(self: *Self) bool {
    return !self.isSimple();
  }

  /// a type that may require some form of substitution
  pub fn isGeneric(self: *Self) bool {
    if (self.tparams.len > 0) return true;
    return switch (self.kind) {
      .TyList, .TyMap => true,
      .TyNullable => self.nsubtype.?.isGeneric(),
      else => false,
    };
  }

  /// a generic type that isn't instantiated
  pub fn isGenericButUninstantiated(self: *Self, tparam_len: usize) bool {
    return (self.tparams.len != tparam_len);
  }

  pub fn getBuiltinGenericParamsLen(self: *Self) usize {
    return switch (self.kind) {
      .TyList => 1,
      .TyMap => 2,
      else => 0
    };
  }

  /// a built-in generic type
  pub fn isBuiltinGeneric(self: *Self) bool {
    return switch (self.kind) {
      .TyList, .TyMap => true,
      else => false,
    };
  }

  fn checkNameType(self: *Self, startStep: usize, maxSteps: usize) !bool {
    if (startStep >= maxSteps) {
      return error.PotentiallyInfiniteSteps;
    }
    return switch (self.kind) {
      .TyName => true,
      .TyNullable => try self.nsubtype.?.checkNameType(startStep + 1, maxSteps),
      .TyUnion => blk: {
        for (self.union_.?.types.items) |typ| {
          if (try typ.checkNameType(startStep + 1, maxSteps)) {
            break :blk true;
          }
        }
        break :blk false;
      },
      else => false, // don't inspect generics
    };
  }

  pub inline fn hasNameType(self: *Self, maxSteps: usize) !bool {
    return try self.checkNameType(0, maxSteps);
  }

  pub fn typeid(self: *Self) u32 {
    if (self.id != 0) return self.id;
    switch (self.kind) {
      .TyBool =>    self.id = 1 << ID_HASH,
      .TyNumber =>  self.id = 2 << ID_HASH,
      .TyString =>  self.id = 3 << ID_HASH,
      .TyNil =>     self.id = 4 << ID_HASH,
      .TyList, .TyMap => |kind| {
        self.id = (if (kind == .TyList) @as(u32, 5) else @as(u32, 6)) << ID_HASH;
        for (0..self.tparams.len) |i| {
          self.id += self.tparams.params[i].typeid();
        }
      },
      .TyUnion => {
        self.id = 7 << ID_HASH;
        for (self.union_.?.types.items) |ty| {
          self.id += ty.typeid();
        }
      },
      .TyNullable => {
        self.id = 8 << ID_HASH;
        self.id += self.nsubtype.?.typeid();
      },
      else => |kind| {
        std.debug.print("Bad type kind: {}\n", .{kind});
        self.id = 1;
      },
    }
    return self.id;
  }

  /// check if typ is contained in self. This is false if self is not a union.
  pub fn containsType(self: *Self, typ: *Self) bool {
    if (self.kind == .TyUnion) {
      if (typ.kind != .TyUnion) {
        for (self.union_.?.types.items) |ty| {
          if (ty.typeid() == typ.typeid()) {
            return true;
          }
        }
      } else {
        for (typ.union_.?.types.items) |ty| {
          if (!self.containsType(ty)) {
            return false;
          }
        }
        return true;
      }
    }
    return false;
  }

  fn _typename(self: *Self, allocator: std.mem.Allocator, depth: *usize) ![]const u8 {
    depth.* = depth.* + 1;
    if (depth.* > MAX_STEPS) return "...";
    return switch (self.kind) {
      .TyBool => "bool",
      .TyNumber => "num",
      .TyString => "str",
      .TyNil => "nil",
      .TyMap, .TyList => |kind| blk: {
        const name = if (kind == .TyMap) "map" else "list";
        if (self.tparams.len == 0) {
          break :blk name;
        } else {
          var list = std.ArrayList(u8).init(allocator);
          var writer = list.writer();
          _ = try writer.write(name);
          _ = try writer.write("{");
          for (self.tparams.getSlice(), 0..) |param, i| {
            _ = try writer.write(try param._typename(allocator, depth));
            if (i != self.tparams.len - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
          return list.items;
        }
      },
      .TyNullable => {
        var sub = try self.nsubtype.?._typename(allocator, depth);
        if (std.mem.containsAtLeast(u8, sub, 1, "|")) {
          return std.fmt.allocPrint(allocator, "({s})?", .{sub}) catch "";
        } else {
          return std.fmt.allocPrint(allocator, "{s}?", .{sub}) catch "";
        }
      },
      .TyUnion => {
        var list = std.ArrayList(u8).init(allocator);
        var writer = list.writer();
        var len: usize = self.union_.?.types.items.len;
        for (self.union_.?.types.items, 0..) |typ, i| {
          _ = try writer.write(try typ._typename(allocator, depth));
          if (i != len - 1) {
            _ = try writer.write(" | ");
          }
        }
        return list.items;
      },
      else => error.BadTypeKind,
    };
  }

  pub fn typename(self: *Self, allocator: std.mem.Allocator) []const u8 {
    var depth: usize = 0;
    return self._typename(allocator, &depth) catch "";
  }

  pub fn newNullable(newnode: *Self, nsubtype: *Self) *NType {
    newnode.kind = .TyNullable;
    newnode.nsubtype = nsubtype;
    return newnode;
  }
};
