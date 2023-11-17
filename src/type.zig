const std = @import("std");
const ds = @import("ds.zig");
const util = @import("util.zig");
const ks = @import("constants.zig");
const VarNode = @import("ast.zig").VarNode;
const lex = @import("lex.zig");

const ID_HASH = 0x12;
const U8Writer = util.U8Writer;
const Token = lex.Token;
const TokenType = lex.TokenType;
pub const MAX_STEPS = MAX_RECURSIVE_DEPTH / 2;
pub const MAX_TPARAMS = 0xA;
pub const MAX_RECURSIVE_DEPTH = 0x3e8;
pub const TypeHashSet = ds.ArrayHashMap(u32, *Type);
pub const TypeList = ds.ArrayList(*Type);
const TypeHashMap = ds.ArrayHashMap(*Type, *Type);

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
  /// void type:
  ///  void
  TyVoid,
  /// noreturn type:
  ///  noreturn
  TyNoReturn,
  /// any type:
  ///  any
  TyAny,

  pub const TyClass: u8 = 7;
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
  /// the token value of this Concrete type
  val: ?*[]const u8 = null,

  pub fn init(tkind: TypeKind) @This() {
    return @This() {.tkind = tkind};
  }

  pub fn toType(self: Concrete) Type {
    return Type.init(.{.Concrete = self});
  }

  pub fn isRelatedTo(this: *Concrete, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    _ = A;
    _ = ctx;
    if (this.tkind == .TyAny) return true;
    switch (other.kind) {
      // Concrete & Concrete 
      .Concrete => |*conc| {
        return conc.tkind == this.tkind;
      },
      .Constant => |*cons| return cons.kind == this.tkind,
      .Union => |*uni| {
        if (this.tkind == .TyBool or this.tkind == .TyString or this.tkind == .TyNumber) {
          for (uni.variants.values()) |ty| {
            if (!(ty.isConstant() and ty.constant().kind == this.tkind)) {
              return false;
            }
          }
          return true;
        }
      },
      .Class => |*cls| {
        if (this.tkind == .TyString) {
          return cls.isStringClass();
        }
        return false;
      },
      .Generic, .Variable, .Recursive, .Function,
      .Method, .Top, .Instance, .Tag, .TaggedUnion => return false,
    }
    return false;
  }
};

pub const Constant = struct {
  kind: TypeKind,
  val: []const u8,

  pub fn init(kind: TypeKind, val: []const u8) @This() {
    return @This() {.kind = kind, .val = val};
  }

  pub inline fn isTrue(self: *Constant) bool {
    return std.mem.eql(u8, self.val, ks.TrueVar);
  }

  pub fn toType(self: Constant) Type {
    return Type.init(.{.Constant = self});
  }

  pub fn isRelatedTo(this: *Constant, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    _ = A;
    _ = ctx;
    switch (other.kind) {
      // Constant & Concrete 
      .Constant => |*cons| {
        return this.kind == cons.kind and std.mem.eql(u8, this.val, cons.val);
      },
      .Concrete => |conc| {
        if (conc.tkind == this.kind) {
          if (conc.val) |val| {
            return std.mem.eql(u8, this.val, val.*);
          }
        }
        return false;
      },
      .Class => |*cls| {
        if (this.kind == .TyString) {
          return cls.isStringClass();
        }
        return false;
      },
      .Union, .Generic, .Variable, .Recursive, .Function,
      .Method, .Top, .Instance, .Tag, .TaggedUnion => return false,
    }
    return false;
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

  pub fn toType(self: Union) Type {
    return Type.compressTypes(@constCast(&self.variants), null).*;
  }

  pub inline fn isBoolUnionTy(self: *Union) bool {
    return (
      self.variants.count() == 2 and
      self.variants.get(Type.getConstantTrueHash()) != null and
      self.variants.get(Type.getConstantFalseHash()) != null
    );
  }

  pub fn set(self: *@This(), typ: *Type) void {
    if (!typ.isUnion()) {
      self.variants.set(typ.typeid(), typ);
    } else {
      var uni = typ.union_();
      for (uni.variants.values()) |vr| {
        self.variants.set(vr.typeid(), vr);
      }
    }
  }

  pub inline fn isNullable(self: *@This()) bool {
    for (self.variants.values()) |ty| {
      if (ty.isNilTy()) return true;
    }
    return false;
  }

  pub fn addAll(self: *@This(), types: *TypeList) void {
    for (types.items()) |ty| {
      self.set(ty);
    }
  }

  pub fn addSlice(self: *@This(), types: []const *Type) void {
    for (types) |ty| {
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
      .Constant, .Concrete, .Generic, .Variable, .Recursive, .Function, .Method, .Class, .Top, .Instance => {
        if (this.isBoolUnionTy() and other.isBoolTy()) return true;
        if (ctx == .RCTypeParams) return false;
        // related if there exists a variant of T1 that is related to T2
        for (this.variants.values()) |variant| {
          if (variant.isRelatedTo(other, ctx, A)) {
            return true;
          }
        }
      },
      .Tag, .TaggedUnion => return false,
    }
    return false;
  }
};

pub const TaggedUnion = struct {
  variants: TypeList,
  active: i32 = -1,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return TaggedUnion{.variants = TypeList.init(allocator)};
  }

  pub fn toType(self: TaggedUnion) Type {
    return Type.compressTaggedTypes(@constCast(&self.variants), null).*;
  }

  pub fn set(self: *@This(), typ: *Type) void {
    std.debug.assert(typ.isTag());
    self.variants.append(typ);
  }

  pub fn append(self: *@This(), typ: *Type) void {
    self.variants.append(typ);
  }

  pub fn addAll(self: *@This(), types: *TypeList) void {
    self.variants.extend(types);
  }

  pub fn addSlice(self: *@This(), types: []const *Type) void {
    for (types) |ty| {
      self.set(ty);
    }
  }

  pub fn activeTy(self: *@This()) ?*Type {
    if (self.active >= 0) {
      return self.variants.itemAt(@intCast(self.active));
    } else {
      return null;
    }
  }

  pub inline fn isNullable(self: *@This()) bool {
    for (self.variants.items()) |ty| {
      if (ty.tag().nameEql(ks.NoneVar)) {
        return true;
      }
    }
    return false;
  }

  pub inline fn isErrorUnion(self: *@This()) bool {
    if (self.isNullable()) return false;
    for (self.variants.items()) |ty| {
      if (ty.isErrorTy()) {
        return true;
      }
    }
    return false;
  }

  pub inline fn hasTag(self: *@This(), name: []const u8) ?*Type {
    for (self.variants.items()) |tg| {
      if (tg.tag().nameEql(name)) {
        return tg;
      }
    }
    return null;
  }

  pub fn _isRelatedTo(this: *TaggedUnion, other: *Type, ctx: RelationContext, A: std.mem.Allocator) ?usize {
    // this -> T1, other -> T2
    switch (other.kind) {
      .TaggedUnion => {
        if (ctx == .RCTypeParams) {
          // .RCTypeParams constrains to 'exactness'
          if (this.variants.len() != other.taggedUnion().variants.len()) {
            return null;
          }
        }
        // related if each & every variants of T2 are related to any of the variants of T1 with given context
        for (other.taggedUnion().variants.items()) |variant| {
          if (!this.isRelatedTo(variant, .RCAny, A)) {
            return null;
          }
        }
        return 0;
      },
      .Recursive => {
        if (this._isRelatedTo(other.recursive().base, ctx, A)) |i| {
          return i;
        }
      },
      .Constant, .Concrete, .Generic, .Variable, .Function, .Method, .Class, .Top, .Instance, .Tag => {
        if (ctx == .RCTypeParams) return null;
        // related if there exists a variant of T1 that is related to T2
        for (this.variants.items(), 0..) |variant, i| {
          if (variant.isRelatedTo(other, ctx, A)) {
            return i;
          }
        }
      },
      .Union => return null,
    }
    return null;
  }

  pub inline fn isRelatedTo(this: *TaggedUnion, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    // this -> T1, other -> T2
    return (this._isRelatedTo(other, ctx, A) != null);
  }
};

pub const Generic = struct {
  base: *Type,
  tparams: TypeList,
  /// whether this container-like generic has no items
  empty: bool = false,

  pub inline fn init(allocator: std.mem.Allocator, base: *Type) @This() {
    return Generic {.tparams = TypeList.init(allocator), .base = base};
  }

  pub inline fn toType(self: Generic) Type {
    return Type.init(.{.Generic = self});
  }

  pub inline fn getSlice(self: *@This()) []*Type {
    return self.tparams.items();
  }

  pub inline fn append(self: *@This(), typ: *Type) void {
    self.tparams.append(typ);
  }

  pub inline fn tparamsLen(self: *@This()) usize {
    return self.tparams.len();
  }

  pub fn isRelatedTo(this: *Generic, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    switch (other.kind) {
      .Generic => |*gen| {
        // TODO: update this
        if (!this.base.isRelatedTo(gen.base, ctx, A)) return false;
        // expr is Type
        if (gen.tparamsLen() == 0 and ctx == .RCIs) return true;
        // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
        if (gen.empty and ctx == .RCConst) return true;
        if (this.tparamsLen() != gen.tparamsLen()) return false;
        for (this.tparams.items(), 0..) |tparam, i| {
          var param = gen.tparams.itemAt(i);
          if (!tparam.isRelatedTo(param, .RCTypeParams, A)) {
            return false;
          }
        }
        return true;
      },
      .Concrete, .Constant, .Union, .Variable, .Recursive,
      .Function, .Method, .Class, .Top, .Instance, .Tag, .TaggedUnion => return false,
    }
    return false;
  }
};

pub const Variable = struct {
  tokens: ds.ArrayList(Token),

  pub fn init(allocator: std.mem.Allocator) @This() {
    return Variable {.tokens = ds.ArrayList(Token).init(allocator)};
  }

  pub fn append(self: *@This(), name: Token) void {
    self.tokens.append(name);
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    if (other.tokens.len() != self.tokens.len()) return false;
    for (other.tokens.items(), self.tokens.items()) |a, b| {
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
      .Constant, .Concrete, .Union, .Recursive, .Function,
      .Method, .Generic, .Class, .Top, .Instance, .Tag, .TaggedUnion => false,
    };
  }
};

const Node = @import("ast.zig").AstNode;
const NodeList = ds.ArrayList(*Node);

pub const Function = struct {
  params: TypeList,
  ret: *Type,
  tparams: ?*TypeList,
  node: ?*Node,

  pub fn init(allocator: std.mem.Allocator, ret: *Type, tparams: ?*TypeList, node: ?*Node) @This() {
    return Function {.params = TypeList.init(allocator), .ret = ret, .tparams = tparams, .node = node};
  }

  pub fn isGeneric(self: *@This()) bool {
    return self.tparams != null;
  }

  pub fn toType(self: @This()) Type {
    return Type.init(.{.Function = self});
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    // TODO: is typeidEql() sufficient?
    if (self.tparams) |tp1| {
      if (other.tparams) |tp2| {
        if (tp1.len() != tp2.len()) {
          return false;
        }
      } else return false;
    } else if (self.tparams != other.tparams) {
      return false;
    }
    if (self.params.len() != other.params.len()) return false;
    if (!self.ret.typeidEql(other.ret)) return false;
    for (self.params.items(), other.params.items()) |a, b| {
      if (!a.typeidEql(b)) return false;
    }
    return true;
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    _ = A;
    _ = ctx;
    return switch (other.kind) {
      .Function => |*fun| this.eql(fun),
      .Variable, .Constant, .Concrete, .Union, .Recursive,
      .Generic, .Method, .Class, .Top, .Instance, .Tag, .TaggedUnion => false,
    };
  }
};

pub const Method = struct {
  func: *Type,
  cls: *Type,

  pub fn init(func: *Type, cls: *Type) @This() {
    return Method {.func = func, .cls = cls};
  }

  pub fn toType(self: @This()) Type {
    return Type.init(.{.Method = self});
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    return switch (other.kind) {
      .Method => |*oth| this.func.isRelatedTo(oth.func, ctx, A) and this.cls.isRelatedTo(oth.cls, ctx, A),
      .Function, .Variable, .Constant, .Concrete, .Union,
      .Recursive, .Generic, .Class, .Top, .Instance, .Tag, .TaggedUnion => false,
    };
  }
};

pub const Top = struct {
  child: *Type,

  pub fn init(child: *Type) @This() {
    return Top {.child = child};
  }

  pub fn toType(self: @This()) Type {
    return Type.init(.{.Top = self});
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    return switch (other.kind) {
      .Top => |*tp| this.child.isRelatedTo(tp.child, ctx, A),
      .Instance, .Class => if (this.child.isClass()) this.child.isRelatedTo(other, ctx, A) else false,
      else => false,
    };
  }
};

pub const Class = struct {
  name: []const u8,
  fields: *NodeList,
  methods: *TypeList,
  tparams: ?*TypeList,
  node: ?*Node,
  empty: bool,
  builtin: bool,
  immutable: bool,
  resolved: bool = false,

  pub fn init(
    name: []const u8, fields: *NodeList, methods: *TypeList,
    tparams: ?*TypeList, node: ?*Node, empty: bool, builtin: bool
  ) @This() {
    return Class {
      .name = name, .fields = fields, .methods = methods,
      .tparams = tparams, .node = node, .empty = empty,
      .builtin = builtin,
      .immutable = false,
    };
  }

  pub fn initWithDefault(name: []const u8, al: std.mem.Allocator) @This() {
    var fields = NodeList.init(al).box();
    var methods = TypeList.init(al).box();
    return Class.init(name, fields, methods, null, null, false, false);
  }

  pub inline fn initTParams(self: *@This(), al: std.mem.Allocator) void {
    if (self.tparams == null) {
      self.tparams = TypeList.init(al).box();
    }
  }

  pub inline fn isGeneric(self: *@This()) bool {
    return self.tparams != null;
  }

  pub inline fn isInstantiatedGeneric(self: *@This()) bool {
    if (self.tparams) |tparams| {
      return (tparams.isNotEmpty() and !tparams.itemAt(0).isVariable());
    }
    return false;
  }

  pub inline fn tparamsLen(self: *@This()) usize {
    return if (self.tparams) |tp| tp.len() else 0;
  }

  pub inline fn getSlice(self: *@This()) []*Type {
    return if (self.tparams) |tp| tp.items() else (&[_]*Type{});
  }

  pub fn appendTParam(self: *@This(), typ: *Type) void {
    self.tparams.?.append(typ);
  }

  pub fn appendTParamSlice(self: *@This(), typs: []const *Type) void {
    for (typs) |ty| {
      self.tparams.?.append(ty);
    }
  }

  pub fn toType(self: @This()) Type {
    return Type.init(.{.Class = self});
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    return std.mem.eql(u8, self.name, other.name);
  }

  pub fn isStringClass(self: *@This()) bool {
    return self.builtin and std.mem.eql(u8, self.name, ks.StrVar);
  }

  pub fn setAsResolved(self: *@This()) void {
    self.resolved = true;
  }

  pub fn getField(self: *@This(), name: []const u8) ?*Node {
    for (self.fields.items()) |field| {
      if (field.AstVarDecl.ident.token.valueEql(name)) {
        return field;
      }
    }
    return null;
  }

  pub fn getFieldIndex(self: *@This(), name: []const u8) ?usize {
    for (self.fields.items(), 0..) |field, i| {
      if (field.AstVarDecl.ident.token.valueEql(name)) {
        return i;
      }
    }
    return null;
  }

  pub fn getMethod(self: *@This(), name: []const u8) ?*Node {
    if (self.node) |node| {
      for (node.AstClass.methods.items()) |mth| {
        if (mth.AstFun.name.?.token.valueEql(name)) {
          return mth;
        }
      }
    }
    return null;
  }

  pub fn getMethodIndex(self: *@This(), name: []const u8) ?usize {
    if (self.node) |node| {
      for (node.AstClass.methods.items(), 0..) |mth, i| {
        if (mth.AstFun.name.?.token.valueEql(name)) {
          return i;
        }
      }
    }
    return null;
  }

  pub fn getMethodTy(self: *@This(), name: []const u8) ?*Type {
    for (self.methods.items()) |mth| {
      if (mth.function().node.?.AstFun.name.?.token.valueEql(name)) {
        return mth;
      }
    }
    return null;
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    return switch (other.kind) {
      .Class => |*oth| {
        if (!std.mem.eql(u8, this.name, oth.name)) return false;
        // expr is Type
        if (oth.tparamsLen() == 0 and ctx == .RCIs) return true;
        // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
        if (oth.empty and ctx == .RCConst) return true;
        if (this.tparams) |tparams1| {
          if (oth.tparams) |tparams2| {
            const _ctx = if (ctx != .RCIs) .RCTypeParams else ctx;
            for (tparams1.items(), tparams2.items()) |tp1, tp2| {
              if (!tp1.isRelatedTo(tp2, _ctx, A)) {
                return false;
              }
            }
          } else {
            return false;
          }
        } else if (oth.tparams != null) {
          return false;
        }
        if (this.builtin) {
          return oth.builtin;
        }
        return true;
      },
      .Instance => this.isRelatedTo(other.instance().cls, ctx, A),
      .Constant => |*cons| {
        return this.isStringClass() and cons.kind == .TyString;
      },
      .Concrete => |*conc| {
        return this.isStringClass() and conc.tkind == .TyString;
      },
      .Variable, .Union, .Recursive, .Function,
      .Method, .Generic, .Top, .Tag, .TaggedUnion, => false,
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
    for (flat_other.items()) |ty| {
      for (flat_this.items()) |rty| {
        if (rty.isRelatedTo(ty, ctx, A)) {
          continue :start;
        }
      }
      return false;
    }
    return true;
  }
};

pub const Instance = struct {
  cls: *Type,

  pub fn init(cls: *Type) @This() {
    return Instance {.cls = cls};
  }

  pub fn toType(self: @This()) Type {
    return Type.init(.{.Instance = self});
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    return switch (other.kind) {
      .Instance => |*oth| this.cls.isRelatedTo(oth.cls, ctx, A),
      .Class => if (ctx == .RCIs) this.cls.isRelatedTo(other, ctx, A) else false,
      .Function, .Variable, .Constant, .Concrete, .Union,
      .Recursive, .Generic, .Method, .Top, .Tag, .TaggedUnion => false,
    };
  }
};

pub const Tag = struct {
  name: []const u8,
  params: ?*TagParamList,
  ty: TokenType,

  pub const TagParamList = ds.ArrayList(TagParam);

  pub const TagParam = struct {
    name: ?Token,
    typ: *Type,

    pub fn clone(self: @This(), al: std.mem.Allocator, map: *TypeHashMap) @This() {
      return @This(){.name = self.name, .typ = self.typ._clone(al, map)};
    }
  };

  pub fn init(name: []const u8, ty: TokenType) @This() {
    return @This(){.name = name, .params = null, .ty = ty};
  }

  pub fn initParams(self: *@This(), al: std.mem.Allocator) void {
    self.params = TagParamList.init(al).box();
  }

  pub fn addParamTypes(self: *@This(), types: []const *Type) void {
    for (types) |ty| {
      self.appendParam(.{.name = null, .typ = ty});
    }
  }

  pub fn addParams(self: *@This(), params: []const TagParam) void {
    for (params) |prm| {
      self.appendParam(prm);
    }
  }

  pub inline fn nameEql(self: *@This(), name: []const u8) bool {
    return std.mem.eql(u8, self.name, name);
  }

  pub fn appendParam(self: *@This(), param: TagParam) void {
    self.params.?.append(param);
  }

  pub fn getParam(self: *@This(), idx: usize) ?TagParam {
    if (self.params) |params| {
      if (idx < params.len()) {
        return params.itemAt(idx);
      }
    }
    return null;
  }

  pub fn getParamWithId(self: *@This(), id: []const u8) ?usize {
    if (self.params) |params| {
      for (params.items(), 0..) |prm, i| {
        if (prm.name) |name| {
          if (name.valueEql(id)) {
            return i;
          }
        }
      }
    }
    return null;
  }

  pub inline fn paramsLen(self: *@This()) usize {
    return if (self.params) |tp| tp.len() else 0;
  }

  pub inline fn paramSlice(self: *@This()) []TagParam {
    return if (self.params) |tp| tp.items() else &[_]TagParam{};
  }

  pub fn paramList(self: *@This(), al: std.mem.Allocator) TypeList {
    var list = TypeList.init(al);
    if (self.params) |params| {
      for (params.items()) |itm| {
        list.append(itm.typ);
      }
    }
    return list;
  }

  pub fn isRelatedTo(this: *Tag, other: *Type, ctx: RelationContext, A: std.mem.Allocator) bool {
    switch (other.kind) {
      .Tag => |*tg| {
        if (!this.nameEql(tg.name)) return false;
        var slice1 = this.paramSlice();
        var slice2 = tg.paramSlice();
        if (slice1.len != slice2.len) return false;
        // nested & if patterns do not require the tag's params to be fully typed
        // (i.e. no need for full expansion if they belong to other tagged unions)
        const _ctx = if (ctx != .RCIs) .RCTypeParams else ctx;
        for (slice1, slice2) |t1, t2| {
          if (!t1.typ.isRelatedTo(t2.typ, _ctx, A)) {
            return false;
          }
        }
        return true;
      },
      else => return false,
    }
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
  Constant: Constant,
  Variable: Variable,
  Union: Union,
  Tag: Tag,
  TaggedUnion: TaggedUnion,
  Generic: Generic,
  Function: Function,
  Method: Method,
  Class: Class,
  Instance: Instance,
  Recursive: Recursive,
  Top: Top,
};

/// context for inspecting relation rules 
pub const RelationContext = enum(u8) {
  /// in any/general context
  RCAny,
  /// in generic type parameter context
  RCTypeParams,
  /// in a constant context
  RCConst,
  /// in an `is` context
  RCIs,
};

pub const Type = struct {
  tid: u32 = 0,
  alias: ?*Type = null,
  kind: TypeInfo,
  /// only applies to function types; whether this function type is variadic
  variadic: bool = false,
  /// whether this type was inferred automatically i.e not an annotation
  inferred: bool = true,

  const Self = @This();

  pub fn init(data: TypeInfo) Self {
    return Self {.kind = data};
  }

  pub fn box(self: Self, allocator: std.mem.Allocator) *Self {
    var al = allocate(Self, allocator);
    al.* = self;
    return al;
  }

  fn setRestFields(ty1: *Self, ty2: *Self) void {
    ty1.tid = ty2.tid;
    ty1.alias = ty2.alias;
    ty1.variadic = ty2.variadic;
    ty1.inferred = ty2.inferred;
  }

  fn _clone(self: *Self, al: std.mem.Allocator, map: *TypeHashMap) *Self {
    switch (self.kind) {
      .Constant, .Concrete, .Variable, .Recursive => return self,
      .Function => |*fun| {
        if (map.get(self)) |ty| return ty;
        var new = Type.init(.{.Function = Function.init(al, fun.ret._clone(al, map), null, fun.node)}).box(al);
        map.set(self, new);
        var _fun = new.function();
        _fun.params.ensureTotalCapacity(fun.params.capacity());
        if (fun.tparams) |tparams| {
          var new_tparams = TypeList.init(al).boxEnsureCapacity(tparams.capacity());
          for (tparams.items()) |ty| {
            new_tparams.append(ty._clone(al, map));
          }
          _fun.tparams = new_tparams;
        }
        for (fun.params.items()) |ty| {
          _fun.params.append(ty._clone(al, map));
        }
        new.setRestFields(self);
        return new;
      },
      .Class => |*cls| {
        if (map.get(self)) |ty| return ty;
        var fields =  NodeList.init(al).boxEnsureCapacity(cls.fields.capacity());
        var methods = TypeList.init(al).boxEnsureCapacity(cls.methods.capacity());
        var ret = Type.init(.{.Class = Class.init(cls.name, fields, methods, null, cls.node, cls.empty, cls.builtin)}).box(al);
        map.set(self, ret);
        for (cls.fields.items()) |itm| {
          fields.append(itm.AstVarDecl.clone(al));
        }
        for (cls.methods.items()) |itm| {
          methods.append(itm._clone(al, map));
        }
        var tparams: ?*TypeList = null;
        if (cls.tparams) |tp| {
          var new_tparams = TypeList.init(al).boxEnsureCapacity(tp.capacity());
          for (tp.items()) |ty| {
            new_tparams.append(ty._clone(al, map));
          }
          tparams = new_tparams;
        }
        ret.klass().tparams = tparams;
        ret.klass().immutable = cls.immutable;
        ret.setRestFields(self);
        return ret;
      },
      .Generic => |*gen| {
        var new = Generic.init(al, gen.base._clone(al, map));
        new.empty = gen.empty;
        new.tparams.ensureTotalCapacity(gen.tparams.capacity());
        for (gen.tparams.items()) |ty| {
          new.append(ty._clone(al, map));
        }
        var ret = Type.init(.{.Generic = new}).box(al);
        ret.setRestFields(self);
        return ret;
      },
      .Union => |*uni| {
        var new = Union.init(al);
        new.active = uni.active;
        new.variants.ensureTotalCapacity(uni.variants.capacity());
        for (uni.variants.values()) |ty| {
          new.set(ty._clone(al, map));
        }
        var ret = Type.init(.{.Union = new}).box(al);
        ret.setRestFields(self);
        return ret;
      },
      .Tag => |*tg| {
        if (map.get(self)) |ty| return ty;
        var ret = Type.init(.{.Tag = Tag.init(tg.name, tg.ty)}).box(al);
        map.set(self, ret);
        var params: ?*Tag.TagParamList = null;
        if (tg.params) |prms| {
          var new_params = Tag.TagParamList.init(al).boxEnsureCapacity(prms.capacity());
          for (prms.items()) |prm| {
            new_params.append(prm.clone(al, map));
          }
          params = new_params;
        }
        ret.tag().params = params;
        ret.setRestFields(self);
        return ret;
      },
      .TaggedUnion => |*uni| {
        var new = TaggedUnion.init(al);
        new.active = uni.active;
        new.variants.ensureTotalCapacity(uni.variants.capacity());
        for (uni.variants.items()) |ty| {
          new.set(ty._clone(al, map));
        }
        var ret = Type.init(.{.TaggedUnion = new}).box(al);
        ret.setRestFields(self);
        return ret;
      },
      .Instance => |*inst| {
        // TODO: clone class?
        var new = Instance.init(inst.cls).toType().box(al);
        new.setRestFields(self);
        return new;
      },
      .Top => |*tp| {
        var new = Type.newTop(tp.child.clone(al)).box(al);
        new.setRestFields(self);
        return new;
      },
      else => unreachable,
    }
  }

  pub fn clone(self: *Self, A: std.mem.Allocator) *Self {
    var map = TypeHashMap.init(A);
    return self._clone(A, &map);
  }

  pub fn newConcrete(kind: TypeKind) Self {
    var conc = Concrete.init(kind);
    return Self.init(.{.Concrete = conc});
  }

  pub fn newBoolUnion(al: std.mem.Allocator) *Self {
    var uni = Union.init(al);
    const t1 = Type.newConstant(.TyBool, ks.TrueVar).box(al);
    const t2 = Type.newConstant(.TyBool, ks.FalseVar).box(al);
    uni.addSlice(&[_]*Type{t1, t2});
    return Self.init(.{.Union = uni}).box(al);
  }

  pub fn newConstant(kind: TypeKind, val: []const u8) Self {
    return Self.init(.{.Constant = Constant.init(kind, val)});
  }

  pub fn newNullable(ty: *Self, al: std.mem.Allocator, nil_ty: ?*Type) *Self {
    if (ty.isNullable()) {
      return ty;
    }
    var nil = if (nil_ty) |nil| nil else Type.newConcrete(.TyNil).box(al);
    if (ty.isUnion()) {
      ty.union_().set(nil);
      return compressTypes(&ty.union_().variants, ty);
    } else {
      var hs = TypeHashSet.init(al);
      hs.set(ty.typeid(), ty);
      hs.set(nil.typeid(), nil);
      return compressTypes(&hs, null);
    }
  }

  pub fn newTaggedNullable(ty: *Self, al: std.mem.Allocator, none_ty: ?*Type) *Self {
    const none = if (none_ty) |none| none else newNoneTag(al);
    const just = newJustTag(al, ty);
    var nl = newTaggedUnion(al).box(al);
    nl.taggedUnion().addSlice(&[_]*Type{just, none});
    return nl;
  }

  pub fn newVariable(allocator: std.mem.Allocator) Self {
    return Self.init(.{.Variable = Variable.init(allocator)});
  }

  pub fn newUnion(allocator: std.mem.Allocator) Self {
    return Self.init(.{.Union = Union.init(allocator)});
  }

  pub fn newTaggedUnion(allocator: std.mem.Allocator) Self {
    return Self.init(.{.TaggedUnion = TaggedUnion.init(allocator)});
  }

  pub fn newGeneric(allocator: std.mem.Allocator, base: *Self) Self {
    return Self.init(.{.Generic = Generic.init(allocator, base)});
  }

  pub fn newFunction(allocator: std.mem.Allocator, ret: *Self) Self {
    return Self.init(.{.Function = Function.init(allocator, ret, null, null)});
  }

  pub fn newMethod(func: *Type, cls: *Type) Self {
    return Self.init(.{.Method = Method.init(func, cls)});
  }

  pub fn newTop(child: *Type) Self {
    return Self.init(.{.Top = Top.init(child)});
  }

  pub fn newInstance(cls: *Type) Self {
    return Self.init(.{.Instance = Instance.init(cls)});
  }

  pub fn newClass(name: []const u8, allocator: std.mem.Allocator) Self {
    return Self.init(.{.Class = Class.initWithDefault(name, allocator)});
  }

  pub fn newTag(name: []const u8, ty: TokenType) Self {
    return Self.init(.{.Tag = Tag.init(name, ty)});
  }

  pub fn newRecursive(base: *Self) Self {
    return Self.init(.{.Recursive = Recursive.init(base)});
  }

  pub fn newNever(allocator: std.mem.Allocator) *Self {
    var ty = newVariable(allocator);
    var nvr = Token.getDefault();
    nvr.value = ks.NeverVar;
    ty.variable().append(nvr);
    return ty.box(allocator);
  }

  pub fn newVoid() Self {
    return Self.init(.{.Concrete = Concrete.init(.TyVoid)});
  }

  pub fn newBuiltinGenericClass(name: []const u8, al: std.mem.Allocator) *Type {
    var base = Type.newClass(name, al).box(al);
    base.klass().initTParams(al);
    base.klass().builtin = true;
    return base;
  }

  pub fn newJustTag(al: std.mem.Allocator, _ty: ?*Type) *Self {
    var tg = Self.init(.{.Tag = Tag.init(ks.JustVar, .TkJust)}).box(al);
    tg.tag().initParams(al);
    if (_ty) |ty| tg.tag().appendParam(.{.name = null, .typ = ty});
    return tg;
  }

  pub fn newNoneTag(al: std.mem.Allocator) *Self {
    return Self.init(.{.Tag = Tag.init(ks.NoneVar, .TkNone)}).box(al);
  }

  pub fn newUnboxedNoneTag() Self {
    return Self.init(.{.Tag = Tag.init(ks.NoneVar, .TkNone)});
  }

  pub fn newTagWithParams(name: []const u8, ty: TokenType, params: []const Tag.TagParam, al: std.mem.Allocator) *Self {
    var typ =  Self.init(.{.Tag = Tag.init(name, ty)}).box(al);
    typ.tag().initParams(al);
    typ.tag().addParams(params);
    return typ;
  }

  pub fn newTagWithParamTypes(name: []const u8, ty: TokenType, params: []const *Type, al: std.mem.Allocator) *Self {
    var typ =  Self.init(.{.Tag = Tag.init(name, ty)}).box(al);
    typ.tag().initParams(al);
    typ.tag().addParamTypes(params);
    return typ;
  }

  pub fn subtype(self: *Self, al: std.mem.Allocator) *Type {
    std.debug.assert(self.isTaggedNullable());
    var sub = TypeList.init(al);
    for (self.taggedUnion().variants.items()) |ty| {
      if (!ty.isNoneTy()) {
        sub.append(ty);
      }
    }
    return compressTaggedTypes(&sub, null);
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

  /// a type that may require some form of substitution
  pub inline fn isClsGeneric(self: *Self) bool {
    return switch (self.kind) {
      .Class => |*cls| cls.isGeneric(),
      else => false,
    };
  }

  /// a compile-time constant type
  pub inline fn isConstant(self: *Self) bool {
    return switch (self.kind) {
      .Constant => true,
      else => false,
    };
  }

  /// a nullable type
  pub inline fn isNullable(self: *Self) bool {
    return switch (self.kind) {
      .Union => |*uni| uni.isNullable(),
      else => false,
    };
  }

  /// a tagged nullable type
  pub inline fn isTaggedNullable(self: *Self) bool {
    return switch (self.kind) {
      .TaggedUnion => |*uni| uni.isNullable(),
      else => false,
    };
  }

  /// an error union type
  pub inline fn isErrorTaggedUnion(self: *Self) bool {
    return switch (self.kind) {
      .TaggedUnion => |*uni| uni.isErrorUnion(),
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

  /// a tagged union type
  pub inline fn isTaggedUnion(self: *Self) bool {
    return switch (self.kind) {
      .TaggedUnion => true,
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

  /// a function type
  pub inline fn isFunction(self: *Self) bool {
    return switch (self.kind) {
      .Function => true,
      else => false,
    };
  }

  /// a method type
  pub inline fn isMethod(self: *Self) bool {
    return switch (self.kind) {
      .Method => true,
      else => false,
    };
  }

  /// an instance type
  pub inline fn isInstance(self: *Self) bool {
    return switch (self.kind) {
      .Instance => true,
      else => false,
    };
  }

  /// a class type
  pub inline fn isClass(self: *Self) bool {
    return switch (self.kind) {
      .Class => true,
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

  /// a tag type
  pub inline fn isTag(self: *Self) bool {
    return switch (self.kind) {
      .Tag => true,
      else => false,
    };
  }

  /// a top type
  pub inline fn isTop(self: *Self) bool {
    return switch (self.kind) {
      .Top => true,
      else => false,
    };
  }

  /// a top type with a 'child' class type
  pub inline fn isClassFromTop(self: *Self) bool {
    return switch (self.kind) {
      .Top => |*tp| tp.child.isClass(),
      else => false,
    };
  }

  /// a type that may be generic (from annotation usage)
  pub inline fn isLikeXTy(self: *Self, comptime check: fn(s: *Self) callconv(.Inline) bool) bool {
    if (check(self)) return true;
    if (self.isUnion()) {
      for (self.union_().variants.values()) |vr| {
        if (check(vr)) {
          return true;
        }
      }
    }
    return false;
  }

  /// a type that may be generic (from annotation usage)
  pub inline fn isLikeGeneric(self: *Self) bool {
    return self.isLikeXTy(isGeneric) or self.isLikeXTy(isClsGeneric);
  }

  /// a type that may be constant
  pub inline fn isLikeConstant(self: *Self) bool {
    return self.isLikeXTy(isConstant);
  }

  /// a type that may be void
  pub inline fn isLikeVoid(self: *Self) bool {
    return self.isLikeXTy(isVoidTy);
  }

  /// a type that may be noreturn
  pub inline fn isLikeNoreturn(self: *Self) bool {
    return self.isLikeXTy(isNoreturnTy);
  }

  inline fn isConcreteTypeEq(self: *Self, kind: TypeKind) bool {
    return switch (self.kind) {
      .Concrete => |conc| conc.tkind == kind,
      else => false,
    };
  }

  inline fn isXClassTy(self: *Self, name: []const u8) bool {
    if (self.isClass()) {
      return std.mem.eql(u8, self.klass().name, name);
    }
    return false;
  }

  pub inline fn isBoolUnionTy(self: *Self) bool {
    return self.union_().isBoolUnionTy();
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

  pub inline fn isVoidTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyVoid);
  }

  pub inline fn isNoreturnTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyNoReturn);
  }

  pub inline fn isAnyTy(self: *Self) bool {
    return self.isConcreteTypeEq(.TyAny);
  }

  pub inline fn isNeverTy(self: *Self) bool {
    return (
      self.isVariable() and
      self.variable().tokens.len() == 1 and
      self.variable().tokens.getLast().valueEql(ks.NeverVar)
    );
  }

  pub fn isListTy(self: *Self) bool {
    return self.isXClassTy(ks.ListVar);
  }

  pub fn isMapTy(self: *Self) bool {
    return self.isXClassTy(ks.MapVar);
  }

  pub fn isTupleTy(self: *Self) bool {
    return self.isXClassTy(ks.TupleVar);
  }

  pub fn isJustTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkJust;
  }

  pub fn isNoneTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkNone;
  }

  /// Tag error type
  pub fn isErrorTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkError;
  }

  /// extract the appropriate typeinfo of this type
  pub inline fn nullable(self: *Self) *Union {
    return &self.kind.Union;
  }

  pub inline fn concrete(self: *Self) Concrete {
    return self.kind.Concrete;
  }

  pub inline fn constant(self: *Self) *Constant {
    return &self.kind.Constant;
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

  pub inline fn taggedUnion(self: *Self) *TaggedUnion {
    return &self.kind.TaggedUnion;
  }

  pub inline fn function(self: *Self) *Function {
    return &self.kind.Function;
  }

  pub inline fn method(self: *Self) *Method {
    return &self.kind.Method;
  }

  pub inline fn instance(self: *Self) *Instance {
    return &self.kind.Instance;
  }

  pub inline fn tag(self: *Self) *Tag {
    return &self.kind.Tag;
  }

  pub inline fn top(self: *Self) *Top {
    return &self.kind.Top;
  }

  pub inline fn classfromtop(self: *Self) *Class {
    return self.kind.Top.child.klass();
  }

  pub inline fn klass(self: *Self) *Class {
    return &self.kind.Class;
  }

  pub inline fn recursive(self: *Self) Recursive {
    return self.kind.Recursive;
  }

  pub inline fn classOrInstanceClass(self: *Self) *Type {
    return if (self.isInstance()) self.instance().cls else self;
  }

  /// check if a class type contains a recursive type (param)
  pub fn hasRecursive(self: *Self) bool {
    for (self.klass().getSlice()) |ty| {
      if (ty.isRecursive() or ty.isLikeXTy(isRecursive)) {
        return true;
      }
      if (ty.isClass() and ty.hasRecursive()) {
        return true;
      }
    }
    return false;
  }

  /// check if a type contains a variable type (param)
  pub fn hasVariable(self: *Self) bool {
    return switch (self.kind) {
      .Concrete, .Constant, .Instance => false,
      .Variable => true,
      .Class => |*cls| {
        if (cls.tparams) |tparams| {
          for (tparams.items()) |ty| {
            if (ty.hasVariable()) {
              return true;
            }
          }
        }
        for (cls.getSlice()) |ty| {
          if (ty.hasVariable()) {
            return true;
          }
        }
        return false;
      },
      .Tag => |*tg| {
        for (tg.paramSlice()) |prm| {
          if (prm.typ.hasVariable()) {
            return true;
          }
        }
        return false;
      },
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
          if (ty.hasVariable()) {
            return true;
          }
        }
        return false;
      },
      .Function => |*fun| {
        if (fun.tparams) |tparams| {
          for (tparams.items()) |ty| {
            if (ty.hasVariable()) {
              return true;
            }
          }
        }
        for (fun.params.items()) |ty| {
          if (ty.hasVariable()) {
            return true;
          }
        }
        return false;
      },
      .Top => |*tp| tp.child.hasVariable(),
      .Generic => |*gen| {
        if (gen.base.hasVariable()) {
          return true;
        }
        for (gen.getSlice()) |ty| {
          if (ty.hasVariable()) {
            return true;
          }
        }
        return false;
      },
      .Recursive => true, 
      else => false,
    };
  }

  pub fn getName(self: *Self) []const u8 {
    return switch (self.kind) {
      .Variable => |*name| name.tokens.getLast().value,
      else => "",
    };
  }

  pub fn typeid(self: *Self) u32 {
    if (self.tid != 0) return self.tid;
    switch (self.kind) {
      .Concrete => |conc| {
        switch (conc.tkind) {
          .TyBool     => self.tid = 1 << ID_HASH,
          .TyNumber   => self.tid = 2 << ID_HASH,
          .TyString   => self.tid = 3 << ID_HASH,
          .TyNil      => self.tid = 4 << ID_HASH,
          .TyVoid     => self.tid = 9 << ID_HASH,
          .TyNoReturn => self.tid = 10 << ID_HASH,
          .TyAny      => self.tid = 11 << ID_HASH,
        }
      },
      .Generic => |*gen| {
        self.tid = 13 << ID_HASH;
        self.tid += gen.base.typeid();
        for (gen.getSlice()) |typ| {
          self.tid += typ.typeid();
        }
      },
      .Class => |*cls| {
        self.tid = 5 << ID_HASH;
        for (cls.name) |ch| {
          self.tid += @as(u8, ch);
        }
        for (cls.getSlice()) |typ| {
          self.tid += typ.typeid();
        }
        for (cls.fields.items()) |nd| {
          if (nd.getType()) |typ| {
            self.tid += typ.typeid();
          } else {
            for (nd.AstVarDecl.ident.token.value) |ch| {
              self.tid += @as(u8, ch);
            }
          }
        }
      },
      .Union => |*uni| {
        self.tid = 6 << ID_HASH;
        for (uni.variants.values()) |ty| {
          self.tid += ty.typeid();
        }
      },
      .Constant => |*cons| {
        self.tid = 7 << ID_HASH;
        // TODO: more efficient approach
        for (cons.val) |ch| {
          self.tid += @as(u8, ch);
        }
      },
      .Function => |*fun| {
        self.tid = 8 << ID_HASH;
        if (fun.tparams) |tparams| {
          self.tid += @intCast(tparams.len());
        }
        for (fun.params.items()) |ty| {
          self.tid += ty.typeid();
        }
        self.tid += fun.ret.typeid();
      },
      .Method => |*mth| {
        self.tid = 14 << ID_HASH;
        self.tid += mth.func.typeid();
        self.tid += mth.cls.typeid();
      },
      .Instance => |*inst| {
        self.tid = 15 << ID_HASH;
        self.tid += inst.cls.typeid();
      },
      .Top => |*tp| {
        self.tid = 12 << ID_HASH;
        self.tid += tp.child.typeid();
      },
      .Variable => |*vr| {
        for (vr.tokens.items()) |tok| {
          self.tid += @as(u32, @intFromEnum(tok.ty)) << ID_HASH;
          // TODO: more efficient approach
          for (tok.value) |ch| {
            self.tid += @as(u8, ch);
          }
        }
      },
      .Recursive => |*rec| {
        self.tid = rec.base.typeid();
      },
      .Tag => |*tg| {
        self.tid = 16 << ID_HASH;
        for (tg.name) |ch| {
          self.tid += @as(u8, ch);
        }
        if (tg.params) |params| {
          self.tid += @intCast(params.len());
          for (params.items()) |param| {
            self.tid += param.typ.typeid();
          }
        }
      },
      .TaggedUnion => |*uni| {
        self.tid = 17 << ID_HASH;
        for (uni.variants.items()) |ty| {
          self.tid += ty.typeid();
        }
      },
    }
    std.debug.assert(self.tid != 0);
    return self.tid;
  }

  inline fn getConstantTrueHash() u32 {
    return 1835456;
  }

  inline fn getConstantFalseHash() u32 {
    return 1835531;
  }

  pub inline fn typeidEql(self: *Self, other: *Self) bool {
    return self.typeid() == other.typeid();
  }

  pub inline fn ptrEql(t1: *Type, t2: *Type) bool {
    return t1 == t2;
  }

  fn _unfoldRecursive(typ: *Self, step: usize, list: *TypeList, visited: *TypeHashSet) void {
    if (step > 0 and typ.isRecursive() and visited.get(typ.typeid()) != null) {
      return;
    }
    switch (typ.kind) {
      // TODO: unfold function & method like generic
      .Concrete, .Variable, .Constant, .Top, .Function, .Method, .Instance => list.append(typ),
      .Generic => |*gen| {
        gen.base._unfoldRecursive(step + 1, list, visited);
        for (gen.tparams.items()) |param| {
          param._unfoldRecursive(step + 1, list, visited);
        }
      },
      .Class => |*cls| {
        // name
        var name_ty = Type.newVariable(list.allocator()).box(list.allocator());
        var token = Token.getDefault();
        token.value = cls.name;
        name_ty.variable().append(token);
        list.append(name_ty);
        // tparams
        for (cls.getSlice()) |param| {
          param._unfoldRecursive(step + 1, list, visited);
        }
        // fields
        for (cls.fields.items()) |field| {
          if (field.getType()) |ty| {
            ty._unfoldRecursive(step + 1, list, visited);
          }
        }
        // methods
        for (cls.methods.items()) |mth| {
          mth._unfoldRecursive(step + 1, list, visited);
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          ty._unfoldRecursive(step + 1, list, visited);
        }
      },
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
          ty._unfoldRecursive(step + 1, list, visited);
        }
      },
      .Tag => |*tg| {
        var name_ty = Type.newVariable(list.allocator()).box(list.allocator());
        var token = Token.getDefault();
        token.value = tg.name;
        name_ty.variable().append(token);
        list.append(name_ty);
        // params
        for (tg.paramSlice()) |param| {
          param.typ._unfoldRecursive(step + 1, list, visited);
        }
      },
      .Recursive => |*rec| {
        visited.set(typ.typeid(), typ);
        rec.base._unfoldRecursive(step + 1, list, visited);
      },
    }
  }

  fn _unfold(self: *Self, list: *TypeList) void {
    switch (self.kind) {
      // TODO: unfold function & method like generic
      .Concrete, .Constant, .Variable, .Top, .Function, .Method, .Instance => list.append(self),
      .Generic => |*gen| {
        gen.base._unfold(list);
        for (gen.tparams.items()) |param| {
          param._unfold(list);
        }
      },
      .Class => |*cls| {
        // name
        var name_ty = Type.newVariable(list.allocator()).box(list.allocator());
        var token = Token.getDefault();
        token.value = cls.name;
        name_ty.variable().append(token);
        list.append(name_ty);
        // tparams
        for (cls.getSlice()) |param| {
          param._unfold(list);
        }
        // fields
        for (cls.fields.items()) |field| {
          if (field.getType()) |ty| {
            ty._unfold(list);
          }
        }
        // methods
        for (cls.methods.items()) |mth| {
          mth._unfold(list);
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          ty._unfold(list);
        }
      },
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
          ty._unfold(list);
        }
      },
      .Tag => |*tg| {
        var name_ty = Type.newVariable(list.allocator()).box(list.allocator());
        var token = Token.getDefault();
        token.value = tg.name;
        name_ty.variable().append(token);
        list.append(name_ty);
        // params
        for (tg.paramSlice()) |param| {
          param.typ._unfold(list);
        }
      },
      .Recursive => |*rec| {
        var visited = TypeHashSet.init(list.allocator());
        visited.set(self.typeid(), self);
        rec.base._unfoldRecursive(0, list, &visited);
        visited.clearAndFree();
      },
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
    for (o_list.items()) |ty| {
      for (rec_list.items()) |rty| {
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
      inline else => |*tyk| return tyk.isRelatedTo(other, ctx, A),
    }
    unreachable;
  }

  pub inline fn isEitherWayRelatedTo(this: *Self, other: *Self, ctx: RelationContext, A: std.mem.Allocator) bool {
    return this.isRelatedTo(other, ctx, A) or other.isRelatedTo(this, ctx, A);
  }

  pub fn canBeCastTo(node_ty: *Type, cast_ty: *Type, ctx: RelationContext, A: std.mem.Allocator) error{CastError, UnionCastError}!*Type {
    switch (cast_ty.kind) {
      .Concrete => |conc| {
        switch (conc.tkind) {
          // any type may be cast to bool
          .TyBool => return cast_ty,
          .TyNil => return error.CastError,
          else => {
            if (cast_ty.isRelatedTo(node_ty, ctx, A)) {
              return cast_ty;
            }
          },
        }
      },
      .Union => |*uni| {
        // upcasting/widening
        if (cast_ty.isRelatedTo(node_ty, ctx, A)) {
          // keep track of the active type
          uni.active = node_ty;
          return cast_ty;
        }
      },
      .TaggedUnion => |*uni| {
        // upcasting/widening
        if (uni._isRelatedTo(node_ty, ctx, A)) |active| {
          // keep track of the active type
          uni.active = @intCast(active);
          return cast_ty;
        }
      },
      else => {
        if (cast_ty.isRelatedTo(node_ty, ctx, A)) {
          return cast_ty;
        }
      }
    }
    // downcasting
    if (node_ty.castContainsType(cast_ty, ctx, A)) {
      if (node_ty.isTaggedUnion()) {
        if (node_ty.taggedUnion().activeTy()) |active| {
          // check if the active type can be cast to the cast type
          _ = active.canBeCastTo(cast_ty, ctx, A) catch return error.UnionCastError;
          return cast_ty;
        } else {
          // TODO: runtime active type tracking
          return node_ty;
        }
      } else {
        if (node_ty.union_().active) |active| {
          // check if the active type can be cast to the cast type
          _ = active.canBeCastTo(cast_ty, ctx, A) catch return error.UnionCastError;
          return cast_ty;
        } else {
          return node_ty;
        }
      }
    }
    return error.CastError;
  }
  
  pub fn canBeAssigned(target: *Self, source: *Self, ctx: RelationContext, A: std.mem.Allocator) ?*Type {
    switch (target.kind) {
      .Union => |*uni| {
        if (target.isRelatedTo(source, ctx, A)) {
          if (uni.isBoolUnionTy()) return target;
          var active = if (source.isUnion()) source.union_().active else source;
          // if this is a Constant type assignment, we want to use the constant
          // type as the active type (if the active type is not itself a constant type)
          // instead of the general inferred type
          if (active != null and active.?.isConcrete() and target.isLikeConstant()) {
            var active_ = active.?;
            for (uni.variants.values()) |ty| {
              if (ty.isConstant() and ty.constant().kind == active_.concrete().tkind) {
                if (active_.concrete().val) |v| {
                  if (std.mem.eql(u8, v.*, ty.constant().val)) {
                    uni.active = ty;
                    return target;
                  }
                }
              }
            }
          }
          uni.active = active;
          return target;
        }
      },
      .TaggedUnion => |*uni| {
        if (uni._isRelatedTo(source, ctx, A)) |active| {
          uni.active = @intCast(active);
          return target;
        }
      },
      else => {
        if (target.isRelatedTo(source, ctx, A)) {
          return target;
        }
      },
    }
    return null;
  }

  /// check if `cast_ty` is contained in `node_ty`. This is false if `node_ty` is not a union.
  pub fn castContainsType(node_ty: *Self, cast_ty: *Self, ctx: RelationContext, A: std.mem.Allocator) bool {
    switch (node_ty.kind) {
      .Union => |*uni_a| {
        switch (cast_ty.kind) {
          .Union => |*uni_b| {
            for (uni_b.variants.values()) |ty| {
              if (!node_ty.castContainsType(ty, ctx, A)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.values()) |ty| {
              if (ty.typeidEql(cast_ty)) {
                return true;
              } else if (cast_ty.canBeCastTo(ty, ctx, A) catch null) |_| {
                return true;
              }
            }
          }
        }
      },
      .TaggedUnion => |*uni_a| {
        switch (cast_ty.kind) {
          .TaggedUnion => |*uni_b| {
            for (uni_b.variants.items()) |ty| {
              if (!node_ty.castContainsType(ty, ctx, A)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.items()) |ty| {
              if (ty.typeidEql(cast_ty)) {
                return true;
              } else if (cast_ty.canBeCastTo(ty, ctx, A) catch null) |_| {
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

  pub fn toInstance(self: *Self, al: std.mem.Allocator) *Self {
    switch (self.kind) {
      .Class => {
        return Self.newInstance(self).box(al);
      },
      else => unreachable,
    }
  }

  pub fn toTaggedUnion(self: *Self) *Self {
    const al = self.union_().variants.allocator();
    var tu = TaggedUnion.init(al);
    tu.addSlice(self.union_().variants.values());
    return tu.toType().box(al);
  }

  fn writeName(tokens: *ds.ArrayList(Token), u8w: *U8Writer) !void {
    var writer = u8w.writer();
    for (tokens.items(), 0..) |tok, i| {
      // variables starting with $ are generated and internal, so use this symbol instead
      _ = if (tok.value[0] != ks.GeneratedVarMarker) try writer.write(tok.value) else try writer.write(ks.GeneratedTypeVar);
      if (i != tokens.len() - 1) {
        // compound names are separated via '.'
        _ = try writer.write(".");
      }
    }
  }

  fn _typename(self: *Self, depth: *usize, u8w: *U8Writer) !void {
    depth.* = depth.* + 1;
    defer depth.* = depth.* - 1;
    if (depth.* > MAX_STEPS) {
      _ = try u8w.writer().write("...");
      return;
    }
    switch (self.kind) {
      .Concrete => |conc| switch (conc.tkind) {
        .TyBool     => _ = try u8w.writer().write(ks.BoolVar),
        .TyNumber   => _ = try u8w.writer().write(ks.NumVar),
        .TyString   => _ = try u8w.writer().write(ks.StrVar),
        .TyNil      => _ = try u8w.writer().write(ks.NilVar),
        .TyVoid     => _ = try u8w.writer().write(ks.VoidVar),
        .TyNoReturn => _ = try u8w.writer().write(ks.NoReturnVar),
        .TyAny      => _ = try u8w.writer().write(ks.AnyVar),
      },
      .Constant => |*cons| {
        _ = try u8w.writer().write(cons.val);
      },
      .Generic => |*gen| {
        try gen.base._typename(depth, u8w);
        if (gen.tparamsLen() != 0) {
          var writer = u8w.writer();
          _ = try writer.write("{");
          for (gen.getSlice(), 0..) |param, i| {
            try param._typename(depth, u8w);
            if (i != gen.tparams.len() - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
        }
      },
      .Class => |*cls| {
        const name = cls.name;
        if (cls.tparams == null) {
          _ = try u8w.writer().write(name);
        } else {
          var writer = u8w.writer();
          _ = try writer.write(name);
          _ = try writer.write("{");
          var tparams = cls.tparams.?;
          for (tparams.items(), 0..) |param, i| {
            try param._typename(depth, u8w);
            if (i != tparams.len() - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
        }
      },
      .Union => |*uni| {
        if (uni.isBoolUnionTy()) {
          _ = try u8w.writer().write(ks.BoolVar);
          return;
        }
        var writer = u8w.writer();
        var values = uni.variants.values();
        for (values, 0..) |typ, i| {
          if (!typ.isFunction()) {
            try typ._typename(depth, u8w);
          } else {
            _ = try writer.write("(");
            try typ._typename(depth, u8w);
            _ = try writer.write(")");
          }
          if (i != values.len - 1) {
            _ = try writer.write(" && ");
          }
        }
      },
      .Function => |*fun| {
        // fn (params): ret
        var writer = u8w.writer();
        _ = try writer.write("fn ");
        if (fun.tparams) |tparams| {
          _ = try writer.write("{");
          for (tparams.items(), 0..) |ty, i| {
            try ty._typename(depth, u8w);
            if (i < tparams.len() - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
        }
        _ = try writer.write("(");
        for (fun.params.items(), 0..) |ty, i| {
          try ty._typename(depth, u8w);
          if (i < fun.params.len() - 1) {
            _ = try writer.write(", ");
          }
        }
        _ = try writer.write(")");
        if (!fun.ret.isVariable() or fun.ret.variable().tokens.itemAt(0).ty != .TkEof) {
          _ = try writer.write(": ");
          try fun.ret._typename(depth, u8w);
        }
      },
      .Method => |*mth| {
        _ = try mth.func._typename(depth, u8w);
      },
      .Instance => |*inst| {
        var writer = u8w.writer();
        _ = try inst.cls._typename(depth, u8w);
        _ = try writer.write(" instance");
      },
      .Top => {
        _ = try u8w.writer().write("Type");
      },
      .Tag => |*tg| {
        _ = try u8w.writer().write(tg.name);
        if (tg.params) |params| {
          _ = try u8w.writer().write("(");
          const last = params.len() - 1;
          for (params.items(), 0..) |tp, i| {
            try tp.typ._typename(depth, u8w);
            if (i < last) {
              _ = try u8w.writer().write(", ");
            }
          }
          _ = try u8w.writer().write(")");
        }
      },
      .TaggedUnion => |*uni| {
        var writer = u8w.writer();
        const last = uni.variants.len() -| 1;
        for (uni.variants.items(), 0..) |typ, i| {
          try typ._typename(depth, u8w);
          if (i != last) {
            _ = try writer.write(" | ");
          }
        }
      },
      .Variable => |*vr| try writeName(&vr.tokens, u8w),
      .Recursive => _ = try u8w.writer().write("{...}"),
    }
  }

  pub fn typenameInPlace(self: *Self, u8w: *U8Writer) void {
    var depth: usize = 0;
    (if (self.alias) |lhs| lhs._typename(&depth, u8w) else self._typename(&depth, u8w)) catch {};
  }

  pub fn typename(self: *Self, u8w: *U8Writer) []const u8 {
    @call(.always_inline, Self.typenameInPlace, .{self, u8w});
    return u8w.items();
  }

  /// combine types in typeset as much as possible
  pub fn compressTypes(typeset: *TypeHashSet, uni: ?*Type) *Type {
    var allocator = typeset.allocator();
    if (typeset.count() > 1) {
      var final = TypeList.init(allocator);
      var last_ty: ?*Type = null;
      var nil_ty: ?*Type = null;
      var true_ty: ?*Type = null;
      var false_ty: ?*Type = null;
      for (typeset.values()) |typ| {
        // skip related types & nil types
        if (typ.isAnyTy()) {
          // any supercedes all other types
          return typ;
        } else if (typ.isNilTy()) {
          nil_ty = typ;
          continue;
        } else if (typ.typeid() == Type.getConstantTrueHash()) {
          true_ty = typ;
          continue;
        } else if (typ.typeid() == Type.getConstantFalseHash()) {
          false_ty = typ;
          continue;
        }
        if (last_ty) |ty| {
          // TODO: would this be good for classes?
          if (ty.isRelatedTo(typ, .RCAny, allocator)) {
            continue;
          }
        }
        last_ty = typ;
        final.append(typ);
      }
      // convert types to a single union type
      var typ: *Type = undefined;
      if (final.len() == typeset.count()) {
        if (uni) |ty| {
          return ty;
        }
      }
      // add constant true & false types if available
      if (true_ty) |tru| {
        if (false_ty) |_| {
          final.append(Type.newConcrete(.TyBool).box(allocator));
        } else {
          final.append(tru);
        }
      } else if (false_ty) |fal| {
        final.append(fal);
      }
      if (final.len() > 1) {
        typ = Type.newUnion(allocator).box(allocator);
        typ.union_().addAll(&final);
      } else {
        typ = final.itemAt(0);
      }
      if (nil_ty) |nil| {
        if (typ.isUnion()) {
          typ.union_().set(nil);
        } else {
          var tmp = Type.newUnion(allocator).box(allocator);
          tmp.union_().addSlice(&[_]*Type{typ, nil});
          typ = tmp;
        }
      }
      return typ;
    }
    return typeset.values()[0];
  }

  /// combine types in typelist as much as possible
  pub fn compressTaggedTypes(typelist: *TypeList, uni: ?*Type) *Type {
    const al = typelist.allocator();
    if (typelist.len() > 1) {
      var final = TypeList.init(al);
      var last_ty: ?*Type = null;
      var none_ty: ?*Type = null;
      for (typelist.items()) |typ| {
        // any supercedes all other types
        if (typ.isAnyTy()) {
          return typ;
        }
        if (typ.isNoneTy()) {
          none_ty = typ;
          continue;
        }
        if (last_ty) |ty| {
          if (ty.isRelatedTo(typ, .RCAny, al)) {
            continue;
          }
        }
        last_ty = typ;
        final.append(typ);
      }
      // convert types to a single tagged union type
      var typ: *Type = undefined;
      if (final.len() == typelist.len()) {
        if (uni) |ty| {
          return ty;
        }
      }
      if (final.len() > 1) {
        typ = Type.newTaggedUnion(al).box(al);
        typ.taggedUnion().addAll(&final);
      } else {
        typ = final.itemAt(0);
      }
      if (none_ty) |none| {
        if (typ.isTaggedUnion()) {
          typ.taggedUnion().append(none);
        } else {
          var tmp = Type.newTaggedUnion(al).box(al);
          tmp.taggedUnion().addSlice(&[_]*Type{typ, none});
          typ = tmp;
        }
      }
      return typ;
    }
    return typelist.itemAt(0);
  }

  /// combine t1 and t2 into a union type if possible
  inline fn _unionify(t1: *Type, t2: *Type, allocator: std.mem.Allocator) *Type {
    if (t1.typeid() == t2.typeid()) {
      return t1;
    }
    if (t1.isNilTy() and t2.isNilTy()) {
      // TODO: should we err?
      return t1;
    }
    if (t1.isNilTy()) {
      if (t2.isNullable()) return t2;
      return t2.newNullable(allocator, t1);
    } else if (t2.isNilTy()) {
      if (t1.isNullable()) return t1;
      return t1.newNullable(allocator, t2);
    } else {
      if (t1.isRelatedTo(t2, .RCAny, allocator)) {
        return t1;
      }
      if (t2.isUnion()) {
        var variants = t2.union_().variants.copy();
        variants.set(t1.typeid(), t1);
        return compressTypes(&variants, null);
      }
      if (t2.isTaggedUnion()) {
        var variants = t2.taggedUnion().variants.copy();
        variants.append(t1);
        return compressTaggedTypes(&variants, null);
      }
      if (t2.isRelatedTo(t1, .RCAny, allocator)) {
        return t2;
      }
      if (t1.isUnion()) {
        var variants = t1.union_().variants.copy();
        variants.set(t2.typeid(), t2);
        return compressTypes(&variants, null);
      }
      if (t1.isTaggedUnion()) {
        var variants = t1.taggedUnion().variants.copy();
        variants.append(t2);
        return compressTaggedTypes(&variants, null);
      }
      var tmp = Type.newTaggedUnion(allocator).box(allocator);
      tmp.taggedUnion().addSlice(&[_]*Type{t1, t2});
      return tmp;
    }
  }

  /// combine t1 and t2 into a union type if possible
  pub fn unionify(t1: *Type, t2: *Type, allocator: std.mem.Allocator) *Type {
    // The idea is that, if we're unionifying a 'never' type and a non-never type
    // then this must be from a type negation. Essentially, it means the never type 
    // does not hold if we still have the possibility of a non-never type.
    if (t1.isNeverTy()) {
      return t2;
    }
    if (t2.isNeverTy()) {
      return t1;
    }
    return _unionify(t1, t2, allocator);
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
              uni2.set(ty.typeid(), ty);
            }
          }
          return compressTypes(&uni2, null);
        },
        .TaggedUnion => |*uni| {
          var uni2 = TypeList.init(allocator);
          for (uni.variants.items()) |typ| {
            if (typ.intersect(t2, allocator)) |ty| {
              uni2.append(ty);
            }
          }
          return compressTaggedTypes(&uni2, null);
        },
        .Recursive => {
          var typs = t1.unfold(allocator);
          for (typs.items()) |typ| {
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
              uni2.set(ty.typeid(), ty);
            }
          }
          return compressTypes(&uni2, null);
        },
        .TaggedUnion => |*uni| {
          var uni2 = TypeList.init(allocator);
          for (uni.variants.items()) |typ| {
            if (typ.intersect(t1, allocator)) |ty| {
              uni2.append(ty);
            }
          }
          return compressTaggedTypes(&uni2, null);
        },
        .Recursive => {
          var typs = t2.unfold(allocator);
          for (typs.items()) |typ| {
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
            new_uni.set(ty.typeid(), ty);
          }
        }
        if (new_uni.count() == 0) {
          return error.Negation;
        }
        return Self.compressTypes(&new_uni, null);
      },
      .TaggedUnion => |*uni| {
        var new_uni = TypeList.init(allocator);
        for (uni.variants.items()) |ty| {
          if (ty.is(t2, allocator) == null) {
            new_uni.append(ty);
          }
        }
        if (new_uni.isEmpty()) {
          return error.Negation;
        }
        return Self.compressTaggedTypes(&new_uni, null);
      },
      .Constant, .Concrete, .Generic, .Variable, .Top,
      .Function, .Method, .Class, .Instance, .Tag => return error.Negation,
      .Recursive => return t1,
    }
  }

  /// t1 is t2 -> select t2 from t1
  pub fn is(t1: *Type, t2: *Type, al: std.mem.Allocator) ?*Type {
    // ex: is(str | list{num}, list) -> list{num}
    switch (t1.kind) {
      .Concrete => |conc| {
        if (t2.isConcrete()) {
          if (conc.tkind == t2.concrete().tkind) {
            return t1;
          }
        }
      },
      .Constant => {
        if (t2.isConstant() and t1.typeidEql(t2)) {
          return t1;
        }
      },
      .Generic => |*t1_gen| {
        if (t2.isGeneric()) {
          var t2_gen = t2.generic();
          if (is(t1_gen.base, t2_gen.base, al)) |_| {
            return t1;
          }
        } else {
          return null;
        }
      },
      .Class => |*cls1| {
        if (t2.isClass() or t2.isClassFromTop()) {
          const ty = if (t2.isClass()) t2 else t2.top().child;
          if (std.mem.eql(u8, cls1.name, ty.klass().name)) {
            return t1;
          }
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          if (is(ty, t2, al)) |typ| {
            return typ;
          }
        }
      },
      .TaggedUnion => |*uni| {
        var list = TypeList.init(uni.variants.allocator());
        for (uni.variants.items()) |ty| {
          if (is(ty, t2, al)) |typ| {
            list.append(typ);
          }
        }
        if (list.isNotEmpty()) {
          return compressTaggedTypes(&list, t1);
        }
      },
      .Variable => |*vr| {
        if (t2.isVariable()) {
          if (vr.eql(t2.variable())) {
            return t1;
          }
        }
      },
      .Function => |*fun| {
        if (t2.isFunction()) {
          if (fun.eql(t2.function())) {
            return t1;
          }
        }
      },
      .Method => |*mth| {
        if (t2.isMethod()) {
          if (
            is(mth.func, t2.method().func, al) != null and 
            is(mth.cls, t2.method().cls, al) != null
          ) {
            return t1;
          }
        }
      },
      .Instance => |*inst| {
        if (t2.isClass()) {
          if (is(inst.cls, t2, al) != null) {
            return t1;
          }
        } else if (t2.isClassFromTop()) {
          if (is(inst.cls, t2.top().child, al) != null) {
            return t1;
          }
        }
      },
      .Tag => |*tg| {
        if (t2.isTag()) {
          if (tg.nameEql(t2.tag().name)) {
            return t1;
          }
        }
      },
      .Top => |*tp| {
        return tp.child.is(t2, al);
      },
      .Recursive => return t1,
    }
    return null;
  }
};
