const std = @import("std");
const ast = @import("ast.zig");
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
  // TODO: func, class, etc.

  const Self = @This();

  pub fn init(kind: NTypeKind, name: ?TName) @This() {
    return Self {.kind = kind, .name = name};
  }
};
