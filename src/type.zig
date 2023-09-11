const std = @import("std");
const ds = @import("ds.zig");
const util = @import("util.zig");
const VarNode = @import("ast.zig").VarNode;
const Token = @import("lex.zig").Token;

const ID_HASH = 0x12;
pub const MAX_STEPS = MAX_RECURSIVE_DEPTH / 2;
pub const MAX_TPARAMS = 0xA;
pub const MAX_RECURSIVE_DEPTH = 0x3e8;
pub const TypeHashSet = ds.ArrayHashMap(u32, *Type);
pub const TypeList = ds.ArrayList(*Type);

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
      .Generic, .Variable, .Recursive, .Function, .Method, .Class, .Top, .Instance => return false,
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
        if (conc.tkind != this.kind) return false;
        if (conc.val) |val| {
          return std.mem.eql(u8, this.val, val.*);
        }
        return false;
      },
      .Union, .Generic, .Variable, .Recursive, .Function, .Method, .Class, .Top, .Instance => return false,
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

  pub inline fn isErrorUnion(self: *@This()) bool {
    // TODO: figure out if this is a good idea.
    if (self.isNullable()) return false;
    for (self.variants.values()) |ty| {
      if (ty.isErrorTy()) return true;
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
  /// whether this container-like generic has no items
  empty: bool = false,

  pub fn init(allocator: std.mem.Allocator, base: *Type) @This() {
    return Generic {.tparams = TypeList.init(allocator), .base = base};
  }

  pub fn toType(self: Generic) Type {
    return Type.init(.{.Generic = self});
  }

  pub fn getSlice(self: *@This()) []*Type {
    return self.tparams.items()[0..self.tparams.len()];
  }

  pub fn append(self: *@This(), typ: *Type) void {
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
      .Concrete, .Constant, .Union, .Variable, .Recursive, .Function, .Method, .Class, .Top, .Instance => return false,
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
      .Constant, .Concrete, .Union, .Recursive, .Function, .Method, .Generic, .Class, .Top, .Instance => false,
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
      .Variable, .Constant, .Concrete, .Union, .Recursive, .Generic, .Method, .Class, .Top, .Instance => false,
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
      .Function, .Variable, .Constant, .Concrete, .Union, .Recursive, .Generic, .Class, .Top, .Instance => false,
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
    _ = A;
    _ = ctx;
    _ = this;
    return switch (other.kind) {
      .Top => true,
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
  resolved: bool = false,

  pub fn init(
    name: []const u8, fields: *NodeList, methods: *TypeList,
    tparams: ?*TypeList, node: ?*Node, empty: bool, builtin: bool
  ) @This() {
    return Class {
      .name = name, .fields = fields, .methods = methods,
      .tparams = tparams, .node = node, .empty = empty,
      .builtin = builtin,
    };
  }

  pub fn initWithDefault(name: []const u8, al: std.mem.Allocator) @This() {
    var fields = util.box(NodeList, NodeList.init(al), al);
    var methods = util.box(TypeList, TypeList.init(al), al);
    return Class.init(name, fields, methods, null, null, false, false);
  }

  pub inline fn initTParams(self: *@This(), al: std.mem.Allocator) void {
    if (self.tparams == null) {
      self.tparams = util.box(TypeList, TypeList.init(al), al);
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
    return if (self.tparams) |tp| tp.items()[0..tp.len()] else (&[_]*Type{})[0..];
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

  pub fn setAsResolved(self: *@This()) void {
    self.resolved = true;
  }

  pub fn getField(self: *@This(), name: []const u8) ?*Node {
    for (self.fields.items()) |field| {
      if (std.mem.eql(u8, field.AstVarDecl.ident.token.value, name)) {
        return field;
      }
    }
    return null;
  }

  pub fn getFieldIndex(self: *@This(), name: []const u8) ?usize {
    for (self.fields.items(), 0..) |field, i| {
      if (std.mem.eql(u8, field.AstVarDecl.ident.token.value, name)) {
        return i;
      }
    }
    return null;
  }

  pub fn getMethod(self: *@This(), name: []const u8) ?*Node {
    if (self.node) |node| {
      for (node.AstClass.methods.items()) |mth| {
        if (std.mem.eql(u8, mth.AstFun.name.?.token.value, name)) {
          return mth;
        }
      }
    }
    return null;
  }

  pub fn getMethodIndex(self: *@This(), name: []const u8) ?usize {
    if (self.node) |node| {
      for (node.AstClass.methods.items(), 0..) |mth, i| {
        if (std.mem.eql(u8, mth.AstFun.name.?.token.value, name)) {
          return i;
        }
      }
    }
    return null;
  }

  pub fn getMethodTy(self: *@This(), name: []const u8) ?*Type {
    for (self.methods.items()) |mth| {
      if (std.mem.eql(u8, mth.function().node.?.AstFun.name.?.token.value, name)) {
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
            if (tparams1.len() != tparams2.len()) return false;
            for (tparams1.items(), tparams2.items()) |tp1, tp2| {
              if (!tp1.isRelatedTo(tp2, .RCTypeParams, A)) {
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
      .Variable, .Constant, .Concrete, .Union, .Recursive, .Function, .Method, .Generic, .Top => false,
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
      .Function, .Variable, .Constant, .Concrete, .Union, .Recursive, .Generic, .Method, .Top => false,
    };
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
  }

  pub fn clone(self: *Self, A: std.mem.Allocator) *Self {
    switch (self.kind) {
      .Constant, .Concrete, .Variable, .Recursive => return self,
      .Function => |*fun| {
        var new = Function.init(A, fun.ret.clone(A), null, fun.node);
        new.params.ensureTotalCapacity(fun.params.capacity());
        if (fun.tparams) |tparams| {
          var new_tparams = util.boxEnsureCapacity(TypeList, TypeList.init(A), A, tparams.capacity());
          for (tparams.items()) |ty| {
            new_tparams.append(ty.clone(A));
          }
          new.tparams = new_tparams;
        }
        for (fun.params.items()) |ty| {
          new.params.append(ty.clone(A));
        }
        var ret = Type.init(.{.Function = new}).box(A);
        ret.setRestFields(self);
        return ret;
      },
      .Method => |*mth| {
        var new = Method.init(mth.func.clone(A), mth.cls.clone(A)).toType().box(A);
        new.setRestFields(self);
        return new;
      },
      .Top => |*tp| {
        var new = Top.init(tp.child).toType().box(A);
        new.setRestFields(self);
        return new;
      },
      .Generic => |*gen| {
        var new = Generic.init(A, gen.base.clone(A));
        new.empty = gen.empty;
        new.tparams.ensureTotalCapacity(gen.tparams.capacity());
        for (gen.tparams.items()) |ty| {
          new.append(ty.clone(A));
        }
        var ret = Type.init(.{.Generic = new}).box(A);
        ret.setRestFields(self);
        return ret;
      },
      .Union => |*uni| {
        var new = Union.init(A);
        new.active = uni.active;
        new.variants.ensureTotalCapacity(uni.variants.capacity());
        for (uni.variants.values()) |ty| {
          new.set(ty.clone(A));
        }
        var ret = Type.init(.{.Union = new}).box(A);
        ret.setRestFields(self);
        return ret;
      },
      .Instance => |*inst| {
        // TODO: clone class?
        var new = Instance.init(inst.cls).toType().box(A);
        new.setRestFields(self);
        return new;
      },
      .Class => |*cls| {
        var fields = util.boxEnsureCapacity(NodeList, NodeList.init(A), A, cls.fields.capacity());
        var methods = util.boxEnsureCapacity(TypeList, TypeList.init(A), A, cls.methods.capacity());
        for (cls.fields.items()) |itm| {
          fields.append(itm.AstVarDecl.clone(A));
        }
        for (cls.methods.items()) |itm| {
          methods.append(itm.clone(A));
        }
        var tparams: ?*TypeList = null;
        if (cls.tparams) |tp| {
          var new_tparams = util.boxEnsureCapacity(TypeList, TypeList.init(A), A, tp.capacity());
          for (tp.items()) |ty| {
            new_tparams.append(ty.clone(A));
          }
          tparams = new_tparams;
        }
        var new = Class.init(cls.name, fields, methods, tparams, cls.node, cls.empty, cls.builtin);
        var ret = Type.init(.{.Class = new}).box(A);
        ret.setRestFields(self);
        return ret;
      },
    }
  }

  pub fn newConcrete(kind: TypeKind) Self {
    var conc = Concrete.init(kind);
    return Self.init(.{.Concrete = conc});
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

  pub fn newVariable(allocator: std.mem.Allocator) Self {
    return Self.init(.{.Variable = Variable.init(allocator)});
  }

  pub fn newUnion(allocator: std.mem.Allocator) Self {
    return Self.init(.{.Union = Union.init(allocator)});
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

  pub fn newRecursive(base: *Self) Self {
    return Self.init(.{.Recursive = Recursive.init(base)});
  }

  pub fn newNever(allocator: std.mem.Allocator) *Self {
    var ty = newVariable(allocator);
    var nvr = Token.getDefault();
    nvr.value = "never";
    ty.variable().append(nvr);
    return ty.box(allocator);
  }

  pub fn newVoid() Self {
    return Self.init(.{.Concrete = Concrete.init(.TyVoid)});
  }

  pub fn subtype(self: *Self, al: std.mem.Allocator) *Type {
    std.debug.assert(self.isNullable());
    var sub = TypeHashSet.init(al);
    for (self.nullable().variants.values()) |ty| {
      if (!ty.isNilTy()) {
        sub.set(ty.typeid(), ty);
      }
    }
    return compressTypes(&sub, null);
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

  /// an error union type
  pub inline fn isErrorUnion(self: *Self) bool {
    return switch (self.kind) {
      .Union => |*uni| uni.isErrorUnion(),
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


  pub fn isListTy(self: *Self) bool {
    return self.isXClassTy("list");
  }

  pub fn isMapTy(self: *Self) bool {
    return self.isXClassTy("map");
  }

  pub fn isTupleTy(self: *Self) bool {
    return self.isXClassTy("tuple");
  }

  pub fn isErrorTy(self: *Self) bool {
    return self.isXClassTy("err");
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

  pub inline fn function(self: *Self) *Function {
    return &self.kind.Function;
  }

  pub inline fn method(self: *Self) *Method {
    return &self.kind.Method;
  }

  pub inline fn instance(self: *Self) *Instance {
    return &self.kind.Instance;
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
        self.tid += @boolToInt(gen.empty);
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
          self.tid += nd.getType().?.typeid();
        }
        // TODO: method type may not be available at the
        //       time this typeid is being computed
        // for (cls.methods.items()) |nd| {
        //   self.tid += nd.getType().?.typeid();
        // }
        self.tid += @intCast(u32, cls.methods.len());
        self.tid += @boolToInt(cls.empty);
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
          self.tid += @intCast(u32, tparams.len());
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
      .Top => {
        self.tid = 12 << ID_HASH;
      },
      .Variable => |*vr| {
        for (vr.tokens.items()) |tok| {
          self.tid += @as(u32, @enumToInt(tok.ty)) << ID_HASH;
          // TODO: more efficient approach
          for (tok.value) |ch| {
            self.tid += @as(u8, ch);
          }
        }
      },
      .Recursive => |*rec| {
        self.tid = rec.base.typeid();
      }
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
      .Recursive => |*rec| {
        visited.set(typ.typeid(), typ);
        rec.base._unfoldRecursive(step + 1, list, visited);
      }
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
      .Recursive => |*rec| {
        var visited = TypeHashSet.init(list.allocator());
        visited.set(self.typeid(), self);
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
      .Concrete => |*conc| {
        return conc.isRelatedTo(other, ctx, A);
      },
      .Constant => |*cons| {
        return cons.isRelatedTo(other, ctx, A);
      },
      .Union => |*uni| {
        return uni.isRelatedTo(other, ctx, A);
      },
      .Generic => |*gen| {
        return gen.isRelatedTo(other, ctx, A);
      },
      .Class => |*cls| {
        return cls.isRelatedTo(other, ctx, A);
      },
      .Variable => |*vr| {
        return vr.isRelatedTo(other, ctx, A);
      },
      .Function => |*fun| {
        return fun.isRelatedTo(other, ctx, A);
      },
      .Method => |*mth| {
        return mth.isRelatedTo(other, ctx, A);
      },
      .Instance => |*inst| {
        return inst.isRelatedTo(other, ctx, A);
      },
      .Top => |*tp| {
        return tp.isRelatedTo(other, ctx, A);
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
          // keep track of the active
          uni.active = node_ty;
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
      if (node_ty.union_().active) |active| {
        // check if the active type can be cast to the cast type
        _ = active.canBeCastTo(cast_ty, ctx, A) catch return error.UnionCastError;
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
  
  pub fn canBeAssigned(target: *Self, source: *Self, ctx: RelationContext, A: std.mem.Allocator) ?*Type {
    switch (target.kind) {
      .Union => |*uni| {
        if (target.isRelatedTo(source, ctx, A)) {
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
      else => {
        if (target.isRelatedTo(source, ctx, A)) {
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

  fn writeName(allocator: std.mem.Allocator, tokens: *ds.ArrayList(Token)) ![]const u8 {
    var writer = @constCast(&ds.ArrayList(u8).init(allocator)).writer();
    for (tokens.items(), 0..) |tok, i| {
      _ = try writer.write(tok.value);
      if (i != tokens.len() - 1) {
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
        .TyBool     => "bool",
        .TyNumber   => "num",
        .TyString   => "str",
        .TyNil      => "nil",
        .TyVoid     => "void",
        .TyNoReturn => "noreturn",
        .TyAny      => "any",
      },
      .Constant => |*cons| {
        return cons.val;
      },
      .Generic => |*gen| {
        const name = try gen.base._typename(allocator, depth);
        if (gen.tparamsLen() == 0) {
          return name;
        } else {
          var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
          _ = try writer.write(name);
          _ = try writer.write("{");
          for (gen.getSlice(), 0..) |param, i| {
            _ = try writer.write(try param._typename(allocator, depth));
            if (i != gen.tparams.len() - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
          return writer.context.items;
        }
      },
      .Class => |*cls| {
        const name = cls.name;
        if (cls.tparams == null) {
          return name;
        } else {
          var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
          _ = try writer.write(name);
          _ = try writer.write("{");
          var tparams = cls.tparams.?;
          for (tparams.items(), 0..) |param, i| {
            _ = try writer.write(try param._typename(allocator, depth));
            if (i != tparams.len() - 1) {
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
          if (!typ.isFunction()) {
            _ = try writer.write(try typ._typename(allocator, depth));
          } else {
            _ = try writer.write("(");
            _ = try writer.write(try typ._typename(allocator, depth));
            _ = try writer.write(")");
          }
          if (i != values.len - 1) {
            _ = try writer.write(" | ");
          }
        }
        return writer.context.items;
      },
      .Function => |*fun| {
        // fn (params): ret
        var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
        _ = try writer.write("fn ");
        if (fun.tparams) |tparams| {
          _ = try writer.write("{");
          for (tparams.items(), 0..) |ty, i| {
            _ = try writer.write(try ty._typename(allocator, depth));
            if (i < tparams.len() - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
        }
        _ = try writer.write("(");
        for (fun.params.items(), 0..) |ty, i| {
          _ = try writer.write(try ty._typename(allocator, depth));
          if (i < fun.params.len() - 1) {
            _ = try writer.write(", ");
          }
        }
        _ = try writer.write(")");
        if (!fun.ret.isVariable() or fun.ret.variable().tokens.itemAt(0).ty != .TkEof) {
          _ = try writer.write(": ");
          _ = try writer.write(try fun.ret._typename(allocator, depth));
        }
        return writer.context.items;
      },
      .Method => |*mth| {
        return try mth.func._typename(allocator, depth);
      },
      .Instance => |*inst| {
        var writer = @constCast(&std.ArrayList(u8).init(allocator)).writer();
        _ = try writer.write(try inst.cls._typename(allocator, depth));
        _ = try writer.write(" instance");
        return writer.context.items;
      },
      .Top => {
        return "Type";
      },
      .Variable => |*vr| try writeName(allocator, &vr.tokens),
      .Recursive => "{...}"
    };
  }

  pub fn typename(self: *Self, allocator: std.mem.Allocator) []const u8 {
    var depth: usize = 0;
    if (self.alias) |lhs| return lhs._typename(allocator, &depth) catch "";
    return self._typename(allocator, &depth) catch "";
  }

  pub fn typenameNoAlias(self: *Self, allocator: std.mem.Allocator) []const u8 {
    var depth: usize = 0;
    return self._typename(allocator, &depth) catch "";
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
      return t2.newNullable(allocator, t1);
    } else if (t2.isNilTy()) {
      if (t1.isNullable()) return t1;
      return t1.newNullable(allocator, t2);
    } else {
      if (t1.isRelatedTo(t2, .RCAny, allocator)) {
        return t1;
      }
      if (t2.isUnion()) {
        var variants = t2.union_().variants.clone();
        variants.set(t1.typeid(), t1);
        return compressTypes(&variants, null);
      }
      if (t2.isRelatedTo(t1, .RCAny, allocator)) {
        return t2;
      }
      if (t1.isUnion()) {
        var variants = t1.union_().variants.clone();
        variants.set(t2.typeid(), t2);
        return compressTypes(&variants, null);
      }
      var tmp = Type.newUnion(allocator).box(allocator);
      tmp.union_().addSlice(([_]*Type{t1, t2})[0..]);
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
              uni2.set(ty.typeid(), ty);
            }
          }
          return compressTypes(&uni2, null);
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
      .Constant, .Concrete, .Generic, .Variable, .Top, .Function, .Method, .Class, .Instance => return error.Negation,
      // TODO: c'est fini?
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
        if (t2.isClass()) {
          // TODO: class relations
          if (std.mem.eql(u8, cls1.name, t2.klass().name)) {
            return t1;
          }
        } else if (t2.isClassFromTop()) {
          if (std.mem.eql(u8, cls1.name, t2.classfromtop().name)) {
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
      .Top => {},
      // TODO:
      .Recursive => return t1,
    }
    return null;
  }
};
