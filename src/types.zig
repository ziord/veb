const std = @import("std");
const ast = @import("ast.zig");
const util = @import("util.zig");
const Token = @import("lex.zig").Token;

pub const MAX_TPARAMS = 0xA;

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

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.types = std.ArrayList(*NType).init(allocator)};
  }
};

pub const TParam = struct {
  params: [MAX_TPARAMS]*NType = undefined,
  len: usize = 0,
};


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
  var_name: ?[]const u8 = null,
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
      .TyNumber, .TyBool, .TyString => true,
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

  pub fn newNullable(newnode: *Self, nsubtype: *Self) *NType {
    newnode.kind = .TyNullable;
    newnode.nsubtype = nsubtype;
    return newnode;
  }
};
