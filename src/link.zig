const std = @import("std");
const util = @import("util.zig");
const diagnostics = @import("diagnostics.zig");
const ds = @import("ds.zig");
const FlowNode = @import("fir.zig").FlowNode;
const TypeChecker = @import("check.zig").TypeChecker;
pub const tir = @import("tir.zig");

const Allocator = tir.Allocator;
const Token = tir.lex.Token;
const Type = tir.Type;
const TypeList = tir.TypeList;
const Union = tir.Union;
const TypeKind = tir.TypeKind;
const TypeInfo = tir.TypeInfo;
const Generic = tir.Generic;
const Variable = tir.Variable;
const Recursive = tir.Recursive;
const Node = tir.Node;
const Diagnostic = diagnostics.Diagnostic;
const U8Writer = util.U8Writer;
const assert = std.debug.assert;
const TypeCheckError = TypeChecker.TypeCheckError;

fn CreateMap(comptime K: type, comptime V: type) type {
  return struct {
    map: Map,

    const Map = if (K == []const u8) std.StringHashMap(V) else std.AutoHashMap(K, V);

    pub fn init(allocator: Allocator) @This() {
      return .{.map = Map.init(allocator)};
    }

    pub fn put(self: *@This(), key: K, value: V) void {
      self.map.put(key, value) catch |e| {
        std.debug.print("Error: {}\n", .{e});
      };
    }

    pub fn get(self: *@This(), key: K) ?V {
      return self.map.get(key);
    }

    pub fn del(self: *@This(), key: K) bool {
      return self.map.remove(key);
    }

    pub fn copy(self: *const @This()) @This() {
      return .{.map = self.map.clone() catch unreachable};
    }
  };
}

pub const TypeData = struct {
  typ: *Type,
  aspec: tir.AccessSpecifier,
};

pub fn GenScope(comptime K: type, comptime V: type) type {
  return struct {
    decls: ds.ArrayList(ScopeMap),
    pub const ScopeMap = CreateMap(K, V);

    pub fn init(al: Allocator) @This() {
      return .{.decls = ds.ArrayList(ScopeMap).init(al)};
    }

    pub inline fn allocator(self: *@This()) Allocator {
      return self.decls.allocator();
    }

    pub fn pushScope(self: *@This()) void {
      self.decls.append(ScopeMap.init(self.decls.allocator()));
    }

    pub fn popScope(self: *@This()) void {
      if (self.decls.len() == 0) {
        util.error_("pop from empty scope-list", .{});
      }
      _ = self.decls.pop();
    }

    /// push a new scope and return the last known count before the new scope
    pub fn pushScopeSafe(self: *@This()) usize {
      self.decls.append(ScopeMap.init(self.decls.allocator()));
      return self.decls.len() - 1;
    }

    /// safely restore the scope list to a last known count `last`
    pub fn popScopeSafe(self: *@This(), last: usize) void {
      // pop n (now - last) items to return to `last`
      self.popScopes(self.decls.len() - last);
    }

    pub fn popScopes(self: *@This(), count: usize) void {
      if (self.decls.len() == 0) {
        util.error_("pop from empty scope-list", .{});
      }
      for (0..count) |_| {
        if (self.len() > 0) {
          _ = self.decls.pop();
        }
      }
    }

    pub fn current(self: *@This()) ScopeMap {
      return self.decls.getLast();
    }

    pub inline fn len(self: *@This()) usize {
      return self.decls.len();
    }

    pub fn lookup(self: *@This(), name: K) ?V {
      if (self.len() == 0) return null;
      var i: usize = self.len();
      while (i > 0): (i -= 1) {
        var map = self.decls.itemAt(i - 1);
        if (map.get(name)) |ty| {
          return ty;
        }
      }
      return null;
    }

    pub fn update(self: *@This(), name: K, new: V) ?V {
      if (self.len() == 0) return null;
      var i: usize = self.len();
      while (i > 0): (i -= 1) {
        var map = &self.decls.items()[i - 1];
        if (map.get(name)) |ty| {
          map.put(name, new);
          return ty;
        }
      }
      return null;
    }

    pub fn remove(self: *@This(), name: K) void {
      if (self.len() == 0) return;
      var i: usize = self.len();
      while (i > 0): (i -= 1) {
        var map = self.decls.itemAt(i - 1);
        if (map.del(name)) {
          return;
        }
      }
    }

    pub fn insert(self: *@This(), name: K, val: V) void {
      if (self.len() == 0) {
        util.error_("insert into empty scope-list", .{});
      }
      self.decls.items()[self.len() - 1].put(name, val);
    }

    pub fn clear(self: *@This()) void {
      self.decls.clearRetainingCapacity();
    }

    pub fn copy(self: *@This()) @This() {
      var decls = ds.ArrayList(ScopeMap).initCapacity(
        self.decls.len(),
        self.decls.allocator(),
      );
      for (self.decls.items()) |decl| {
        decls.appendAssumeCapacity(decl.copy());
      }
      return .{.decls = decls};
    }
  };
}

fn CreateTContext(comptime CoreTypScope: type, comptime TypScope: type, comptime VarScope: type) type {
  return struct {
    /// core type scope
    core_typ_scope: CoreTypScope,
    /// type scope
    typ_scope: TypScope,
    /// scope for other declarations, e.g. variables, functions, etc.
    var_scope: VarScope,
    /// modules directly/indirectly linked to this context
    modules: ds.ArrayListUnmanaged(*tir.Module),
    /// data
    data: ContextData,

    const Self = @This();

    pub fn init(al: Allocator) Self {
      return Self {
        .core_typ_scope = CoreTypScope.init(al),
        .typ_scope = TypScope.init(al),
        .var_scope = VarScope.init(al),
        .modules = ds.ArrayListUnmanaged(*tir.Module).init(),
        .data = .{},
      };
    }

    pub inline fn allocator(self: *Self) Allocator {
      return @call(.always_inline, TypScope.allocator, .{&self.typ_scope});
    }

    pub inline fn enterScope(self: *Self) void {
      self.core_typ_scope.pushScope();
      self.typ_scope.pushScope();
      self.var_scope.pushScope();
    }

    pub inline fn leaveScope(self: *Self) void {
      self.core_typ_scope.popScope();
      self.typ_scope.popScope();
      self.var_scope.popScope();
    }

    pub inline fn enterTVScope(self: *Self) void {
      self.typ_scope.pushScope();
      self.var_scope.pushScope();
    }

    pub inline fn leaveTVScope(self: *Self) void {
      self.typ_scope.popScope();
      self.var_scope.popScope();
    }

    pub inline fn addModule(self: *Self, ty: *Type, al: Allocator) void {
      self.modules.append(ty.module(), al);
    }

    pub inline fn copyType(self: *Self, typ: *Type) *Type {
      // we need to deepcopy typ
      return typ.clone(self.typ_scope.allocator());
    }

    pub fn lookupVar(self: *Self, name: []const u8) ?TypeData {
      if (self.var_scope.lookup(name)) |info| {
        return info;
      }
      var i = self.modules.len() -| 1;
      for (0..self.modules.len()) |_| {
        if (self.modules.itemAt(i).env.ctx.var_scope.lookup(name)) |info| {
          // extra core module check ensures only core module item selection 
          if (info.aspec.isPublic() and self.modules.itemAt(i).env.alias.ptr == tir.ks.CoreModuleVar.ptr) {
            return info;
          } else {
            break;
          }
        }
        i -|= 1;
      }
      return null;
    }

    pub fn lookupTyp(self: *Self, name: []const u8) ?TypeData {
      if (self.typ_scope.lookup(name)) |info| {
        return info;
      }
      var i = self.modules.len() -| 1;
      for (0..self.modules.len()) |_| {
        if (self.modules.itemAt(i).env.ctx.typ_scope.lookup(name)) |info| {
          return info;
        }
        i -|= 1;
      }
      return null;
    }

    pub inline fn lookupInVarScope(self: *Self, name: []const u8) ?*Type {
      if (self.lookupVar(name)) |info| {
        return info.typ;
      }
      return null;
    }

    pub inline fn lookupInTypScope(self: *Self, name: []const u8) ?*Type {
      if (self.lookupTyp(name)) |info| {
        return info.typ;
      }
      return null;
    }

    pub fn lookupInCoreTypScope(self: *Self, name: []const u8) ?*Type {
      if (self.core_typ_scope.lookup(name)) |typ| {
        return typ;
      }
      var i = self.modules.len() -| 1;
      for (0..self.modules.len()) |_| {
        if (self.modules.itemAt(i).env.ctx.core_typ_scope.lookup(name)) |typ| {
          return typ;
        }
        i -|= 1;
      }
      return null;
    }
  };
}

pub const Scope = GenScope([]const u8, *Type);
pub const ScopeT = GenScope([]const u8, TypeData);
pub const TContext = CreateTContext(Scope, ScopeT, ScopeT);
pub const ContextData = struct {
  parent: ?*Node = null,
  flo_node: ?FlowNode =  null,
};

pub const TypeLinker = struct {
  ctx: *TContext,
  /// track each substitution/resolution steps
  sub_steps: usize = 0,
  /// track if a Variable type is from a generic parameter type substitution 
  using_tvar: usize = 0,
  /// scope for cycle detection
  cyc_scope: PairScope,
  /// resolving an infinite/recursive type?
  in_inf: bool = false,
  /// types that shouldn't be used as aliases
  ban_alias: ?tir.TypeItems = null,
  /// resolved type variables
  resolved_tvars: TypeMap,
  diag: *Diagnostic,
  u8w: *U8Writer,
  tc: *TypeChecker = undefined,

  const TypeMap = CreateMap([]const u8, *Type);
  const MultiPair = struct {setter: *Type, key: *Type, value: *Type};
  const PairScope = GenScope([]const u8, MultiPair);
  const MAX_DEPTH = 0x64;
  pub const TypeLinkError = error{TypeLinkError};
  pub const MAX_SUB_STEPS = tir.MAX_RECURSIVE_DEPTH;

  const Self = @This();

  pub fn init(ctx: *TContext, diag: *Diagnostic, u8w: *U8Writer, al: Allocator) @This() {
    return .{
      .ctx = ctx,
      .cyc_scope = PairScope.init(al),
      .resolved_tvars = TypeMap.init(al),
      .diag = diag,
      .u8w = u8w,
    };
  }

  fn error_(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    self.diag.addDiagnosticsWithLevel(.DiagError, token, "Error: " ++ fmt, args);
    return error.TypeLinkError;
  }

  fn softError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnosticsWithLevel(.DiagError, token, "Error: " ++ fmt, args);
  }

  fn getConcatName(self: *Self, tvar: *tir.Variable) []const u8 {
    const end = tvar.tokens.len() - 1;
    for (tvar.tokens.items(), 0..) |tk, i| {
      _ = self.u8w.writer().write(tk.lexeme()) catch 0;
      if (i < end) {
        _ = self.u8w.writer().write(".") catch 0;
      }
    }
    return self.u8w.items();
  }

  fn insertTVar(self: *Self, typ: *Type, data: MultiPair) void {
    self.cyc_scope.insert(typ.variable().lexeme(), data);
  }

  fn checkTVar(self: *Self, typ: *Type, found: *Type) ?*Type {
    if (self.cyc_scope.lookup(typ.variable().lexeme())) |pair| {
      // extra p.o.c;
      if (pair.key == found) {
        return pair.value;
      }
    }
    return null;
  }

  fn delTVar(self: *Self, typ: *Type) void {
    const name = typ.variable().lexeme();
    if (self.cyc_scope.lookup(name)) |pair| {
      // only delete this pair if `typ` is its exact setter
      if (pair.setter == typ) {
        self.cyc_scope.remove(name);
      }
    }
  }

  inline fn insert(self: *Self, name: []const u8, typ: *Type) void {
    self.ctx.typ_scope.insert(name, .{.typ = typ, .aspec = typ.aspec});
  }

  pub inline fn lookupTypeVariable(self: *Self, typ: *Type) ?*Type {
    return self.ctx.lookupInTypScope(typ.variable().lexeme());
  }

  fn findType(self: *Self, typ: *Type, from_gen: bool) ?*Type {
    if (self.lookupTypeVariable(typ)) |ty| {
      // if this variable occurs in the pair stack before we push it,
      // then it's cyclic/recursive
      if (self.checkTVar(typ, ty)) |rec| {
        // if the type isn't from a generic lookup or if we're certain 
        // we're resolving an infinite type, then return a recursive type
        if (!from_gen or self.in_inf) {
          _ = rec.typeid();
          return Type.newRecursive(rec).box(self.ctx.allocator());
        }
      }
      const result = switch (ty.info) {
        .Concrete, .Variable, .Constant => ty,
        .Class, .Trait => if (!ty.isParameterized()) ty else self.ctx.copyType(ty),
        else => self.ctx.copyType(ty),
      };
      // cache the type.
      self.insertTVar(typ, MultiPair{.setter = typ, .key = ty, .value = result});
      return result;
    }
    return null;
  }

  fn lookupType(self: *Self, typ: *Type, from_gen: bool, debug: Token) !*Type {
    if (self.findType(typ, from_gen)) |found| {
      return found;
    } else {
      return self.error_(debug, "could not resolve type with name: '{s}'", .{typ.getName()});
    }
  }

  fn lookupTypeAbs(self: *Self, typ: *Type, from_gen: bool) ?*Type {
    return self.findType(typ, from_gen);
  }

  inline fn assertMaxSubStepsNotExceeded(self: *Self, emit: bool, debug: Token) !void {
    if (self.sub_steps >= MAX_DEPTH) {
      if (!emit) return error.TypeLinkError;
      return self.error_(
        debug,
        "potentially infinite substitutions arising from probable self-referencing types", 
        .{}
      );
    }
  }

  /// check that a generic instantiation of a type alias matches the defined alias type
  inline fn assertGenericAliasSubMatches(self: *Self, alias: *Type, typ: *Type, debug: Token) !void {
    if (!alias.isGeneric()) {
      return self.error_(
        debug,
        "type alias is not generic, but instantiated with {} parameters", 
        .{typ.generic().tparamsLen()},
      );
    } else if (!typ.isGeneric()) {
      return self.error_(
        debug,
        "type alias is generic, but used without instantiation", 
        .{},
      );
    }
    var l_gen = alias.generic();
    var r_gen = typ.generic();
    if (l_gen.tparamsLen() != r_gen.tparamsLen()) {
      return self.error_(
        debug,
        "parameter mismatch in generic type instantiation. Expected {} generic argument(s), got {}", 
        .{l_gen.tparamsLen(), r_gen.tparamsLen()},
      );
    }
  }

  fn resolveDotType(self: *Self, typ: *Type, debug: Token, al: Allocator) !*Type {
    var dot = typ.dot();
    var lhs = try self.resolveType(dot.lhs, debug, al);
    if (lhs.isModule()) {
      if (!lhs.module().resolved) {
        try self.tc.resolveModuleType(lhs);
      }
      if (dot.rhs.isVariable()) {
        const token = dot.rhs.variable().value;
        const info = try self.tc.checkModuleAccess(lhs, token, token.lexeme());
        return info.@"0";
      } else if (dot.rhs.isGeneric()) {
        var gen = dot.rhs.generic();
        assert(gen.base.isVariable());
        const token = gen.base.variable().value;
        const info = try self.tc.checkModuleAccess(lhs, token, token.lexeme());
        var ty = info.@"0".clone(al);
        if (info.@"1" and !ty.isClassOrTrait()) {
          self.tc.typeReferenceError(token, ty);
          return error.TypeLinkError;
        }
        if (ty.isTaggedUnion()) {
          if (ty.alias) |alias| {
            if (alias.isGeneric() and alias.generic().tparamsLen() == gen.tparamsLen()) {
              self.ctx.enterScope();
              defer self.ctx.leaveScope();
              if (alias.generic().base.isVariable()) {
                self.insert(alias.generic().base.getName(), ty);
              }
              for (alias.generic().getSlice(), gen.getSlice()) |tvar, t| {
                const _t = try self.resolveType(t, debug, al);
                self.insert(tvar.getName(), _t);
              }
              if (!self.in_inf and alias.isLikeParameterized()) {
                self.cyc_scope.pushScope();
                defer self.cyc_scope.popScope();
                self.in_inf = try self.resolveTypeAbs(alias, debug);
              }
              return self.resolveType(ty, debug, al);
            }
          }
        } else if (ty.isGeneric() and ty.generic().tparamsLen() == gen.tparamsLen()) {
          var slice = ty.generic().getSlice();
          for (gen.getSlice(), 0..) |t, i| {
            slice[i] = try self.resolveType(t, debug, al);
          }
          return self.resolveType(ty, debug, al);
        } else if (ty.isClassOrTrait() and ty.klassOrTrait().tparamsLen() == gen.tparamsLen()) {
          self.ctx.enterScope();
          defer self.ctx.leaveScope();
          for (ty.klassOrTrait().getSlice(), gen.getSlice()) |tvar, t| {
            self.insert(tvar.getName(), try self.resolveType(t, debug, al));
          }
          return self.resolveType(ty, debug, al);
        }
        return self.error_(
          debug, "bad generic type reference: {s}",
          .{self.tc.getTypename(ty)}
        );
      } else {
        return self.error_(
          debug, "expected variable or generic type in rhs: {s}",
          .{dot.rhs.typename(self.u8w)}
        );
      }
    } else if (lhs.isTaggedUnion()) {
      if (dot.rhs.isVariable()) {
        if (lhs.taggedUnion().getTag(dot.rhs.variable().lexeme())) |ty| {
          return ty;
        } else {
          return self.error_(
            debug, "type '{s}' has no tag '{s}'",
            .{self.tc.getTypename(lhs), self.tc.getTypename(dot.rhs)}
          );
        }
      } else {
        return self.error_(
          debug, "expected variable type in rhs: {s}",
          .{self.tc.getTypename(dot.rhs)}
        );
      }
    } else {
      return self.error_(
        debug, "expected module or tagged union type: {s}",
        .{self.tc.getTypename(typ)}
      );
    }
  }

  /// resolve a type alias abstractly by walking the alias chain without substituting
  fn resolveTypeAbs(self: *Self, typ: *Type, debug: Token) !bool {
    if (typ.isRecursive()) return true;
    if (typ.isGeneric()) {
      var gen = typ.generic();
      util.assert(gen.base.isVariable(), "gen.base should be a Variable()");
      var eqn = try self.lookupType(gen.base, false, debug);
      if (eqn.isRecursive()) return true;
      if (eqn.isGeneric()) {
        var eqn_gen = eqn.generic();
        for (eqn_gen.getSlice()) |param| {
          if (try self.resolveTypeAbs(param, debug)) {
            return true;
          }
        }
      }
      const ret = try self.resolveTypeAbs(eqn, debug);
      self.delTVar(gen.base);
      return ret;
    }
    if (typ.isClassOrTrait()) {
      // Resolve the tparams which may be Variable or some interesting type
      var kot = typ.klassOrTrait();
      for (kot.getSlice()) |param| {
        if (try self.resolveTypeAbs(param, debug)) {
          return true;
        }
      }
      if (kot.data.trait) |ty| {
        if (try self.resolveTypeAbs(ty, debug)) {
          return true;
        }
      }
      return false;
    }
    if (typ.isUnion()) {
      for (typ.union_().variants.values()) |variant| {
        if (try self.resolveTypeAbs(variant, debug)) {
          return true;
        }
      }
      return false;
    }
    if (typ.isTaggedUnion()) {
      for (typ.taggedUnion().variants.items()) |variant| {
        if (try self.resolveTypeAbs(variant, debug)) {
          return true;
        }
      }
      return false;
    }
    if (typ.isTag()) {
      if (typ.tag().fields) |params| {
        for (params.items()) |param| {
          if (try self.resolveTypeAbs(param.typ, debug)) {
            return true;
          }
        }
      }
      return false;
    }
    if (typ.isDot()) {
      if (try self.resolveTypeAbs(typ.dot().lhs, debug)) {
        return true;
      }
      return self.resolveTypeAbs(typ.dot().rhs, debug);
    }
    if (typ.isVariable()) {
      // if this variable occurs in the pair stack before we push it, then it's cyclic/recursive
      // when we begin resolving a variable - we push it on the pair stack
      var eqn = self.lookupTypeAbs(typ, false);
      // could be null from an unsubstituted aliasee of a type alias, e.g. type Foo{T} = list{T}
      // `T` here would be null since resolveTypeAbs() doesn't do substitutions
      if (eqn == null) return false;
      if (eqn.?.isRecursive()) {
        return true;
      }
      if (try self.resolveTypeAbs(eqn.?, debug)) {
        return true;
      }
      // when we finish resolving the variable - we pop it off the pair stack
      self.delTVar(typ);
      return false;
    }
    return false;
  }

  fn resolveType(self: *Self, typ: *Type, debug: Token, al: Allocator) TypeCheckError!*Type {
    try self.assertMaxSubStepsNotExceeded(false, debug);
    if (typ.isSimple() or typ.isConstant() or typ.isRecursive()) {
      return typ;
    }
    if (typ.isVariable()) {
      // if this variable occurs in the pair stack before we push it, then it's cyclic/recursive
      // when we begin resolving a variable - we push it on the pair stack
      var eqn = try self.lookupType(typ, false, debug);
      const alias_info = if (eqn.isRecursive()) eqn.recursive().base.alias else eqn.alias;
      if (alias_info) |alias| {
        if (alias.isGeneric()) {
          // check if this is a regular generic type called without instantiation, 
          // or a recursive generic type called without instantiation
          if ((self.using_tvar == 0 or eqn.isRecursive()) and !eqn.isTaggedUnion()) {
            if (!eqn.isTrait()) {
              return self.error_(
                debug,
                "generic type '{s}' may not have been instantiated correctly",
                .{typ.typename(self.u8w)}
              );
            }
          }
        }
      }
      const sol = try self.resolveType(eqn, debug, al);
      // when we finish resolving the variable - we pop it off the pair stack
      self.delTVar(typ);
      return sol;
    }
    if (typ.isGeneric()) {
      self.sub_steps += 1;
      var gen = typ.generic();
      util.assert(gen.base.isVariable(), "gen.base should be a Variable()");
      var eqn = try self.lookupType(gen.base, true, debug);
      // specially handle recursive generic types
      if (eqn.isRecursive()) {
        const rec = eqn.recursive();
        var lhs = rec.base.alias.?;
        if (!lhs.isGeneric()) {
          return self.error_(debug, "non-generic type instantiated as generic", .{});
        }
        // check that the tparams of this generic type matches it's type alias tparams exactly.
        try self.assertGenericAliasSubMatches(lhs, typ, debug);
        // A generic recursive type's tparams will never be resolved and substituted for, so
        // just ensure that the tparam is actually valid.
        for (gen.tparams.items()) |tparam| {
          _ = try self.resolveType(tparam, debug, al);
        }
        // complete the ouroboros
        typ.* = eqn.*;
        return eqn;
      } else if (eqn.isClassOrTrait()) {
        // create a temp generic type and validate if this generic substitution matches
        var tparams = TypeList.init();
        tparams.appendSlice(eqn.klassOrTrait().tparams orelse &[_]*Type{}, al);
        var tmp = Type.init(.{.Generic = .{.tparams = tparams, .base = eqn}});
        try self.assertGenericAliasSubMatches(&tmp, typ, debug);
        var alias = eqn.alias orelse &tmp;
        try self.assertGenericAliasSubMatches(alias, typ, debug);
        self.ctx.typ_scope.pushScope();
        self.using_tvar += 1;
        var alias_gen = alias.generic();
        var resolved_params = TypeList.init();
        for (alias_gen.getSlice(), 0..) |tvar, i| {
          const tsub = gen.tparams.itemAt(i);
          const r_tsub = try self.resolveType(tsub, debug, al);
          resolved_params.append(r_tsub, al);
          if (self.resolved_tvars.get(tvar.variable().lexeme()) == null) {
            self.resolved_tvars.put(tvar.variable().lexeme(), r_tsub);
          }
        }
        for (alias_gen.getSlice(), resolved_params.items()) |tvar, tsub| {
          self.insert(tvar.variable().lexeme(), tsub);
        }
        var slice = eqn.klassOrTrait().getSlice();
        for (0..slice.len) |i| {
          slice[i] = try self.resolveType(tparams.itemAt(i), debug, al);
        }
        if (eqn.isTrait()) {
          if (eqn.trait().data.trait) |trt| {
            eqn.trait().data.trait = try self.resolveType(trt, debug, al);
          } 
        }
        self.using_tvar -= 1;
        self.ctx.typ_scope.popScope();
        return eqn;
      } else if (eqn.alias == null or !eqn.alias.?.isGeneric()) {
        // only instantiate generic type variables when the alias type is guaranteed to be generic
        return self.error_(debug, "Non-generic type instantiated as generic", .{});
      }
      var alias = eqn.alias.?;
      try self.assertGenericAliasSubMatches(alias, typ, debug);
      self.ctx.typ_scope.pushScope();
      self.using_tvar += 1;
      var alias_gen = alias.generic();
      var resolved_params = TypeList.init();
      for (alias_gen.getSlice(), 0..) |tvar, i| {
        const tsub = gen.tparams.itemAt(i);
        const r_tsub = try self.resolveType(tsub, debug, al);
        resolved_params.append(r_tsub, al);
        if (self.resolved_tvars.get(tvar.variable().lexeme()) == null) {
          self.resolved_tvars.put(tvar.variable().lexeme(), r_tsub);
        }
      }
      for (alias_gen.getSlice(), resolved_params.items()) |tvar, tsub| {
        self.insert(tvar.variable().lexeme(), tsub);
      }
      // `eqn` is the type alias' aliasee, and may not be generic, so we add an extra guard.
      // for ex: type Foo{T} = T  # <-- aliasee/eqn 'T' is not generic here.
      if (eqn.isGeneric()) {
        var eqn_gen = eqn.generic();
        for (eqn_gen.getSlice(), 0..) |param, i| {
          eqn_gen.tparams.items()[i] = try self.resolveType(param, debug, al);
        }
      }
      // resolving eqn resolves typ
      const sol = try self.resolveType(eqn, debug, al);
      self.using_tvar -= 1;
      self.ctx.typ_scope.popScope();
      self.delTVar(gen.base);
      return sol;
    }
    if (typ.isClassOrTrait()) {
      var slice = typ.klassOrTrait().getSlice();
      for (slice, 0..) |param, i| {
        slice[i] = try self.resolveType(param, debug, al);
      }
      return typ;
    }
    if (typ.isFunction() and !typ.function().isParameterized()) {
      var fun = typ.function();
      for (fun.data.params, 0..) |param, i| {
        fun.data.params[i] = try self.resolveType(param, debug, al);
      }
      const count = self.diag.count();
      fun.data.ret = self.resolveType(fun.data.ret, debug, al) catch {
        self.diag.popUntil(count);
        return typ;
      };
    }
    if (typ.isUnion()) {
      var variants = tir.TypeHashSet.init();
      for (typ.union_().variants.values()) |variant| {
        tir.Union.setDirect(&variants, try self.resolveType(variant, debug, al), al);
      }
      return Type.compressTypes(&variants, typ, al);
    }
    if (typ.isTaggedUnion()) {
      var variants = &typ.taggedUnion().variants;
      for (variants.items(), 0..) |variant, i| {
        variants.items()[i] = try self.resolveType(variant, debug, al);
      }
      return typ;
    }
    if (typ.isTag()) {
      if (typ.tag().fields) |params| {
        for (params.items(), 0..) |prm, i| {
          params.items()[i].typ = try self.resolveType(prm.typ, debug, al);
        }
      }
      self.insert(typ.tag().name, typ);
      return typ;
    }
    if (typ.isDot()) {
      return self.resolveDotType(typ, debug, al);
    }
    return typ;
  }

  fn tryResolveType(self: *Self, typ: *Type, debug: Token) !*Type {
    self.sub_steps = 0;
    self.in_inf = false;
    if (typ.isLikeParameterized()) {
      self.cyc_scope.pushScope();
      defer self.cyc_scope.popScope();
      self.in_inf = try self.resolveTypeAbs(typ, debug);
    }
    self.cyc_scope.pushScope();
    defer self.cyc_scope.popScope();
    var sol = try self.resolveType(typ, debug, self.ctx.allocator());
    if (sol.isTaggedUnion()) {
      sol = Type.compressTaggedTypes(&sol.taggedUnion().variants, sol, self.ctx.allocator());
      if (sol.isTaggedUnion()) {
        for (sol.taggedUnion().variants.items()) |ty| {
          self.insert(ty.tag().name, ty);
        }
      }
    } else if (sol.isTag()) {
      self.insert(sol.tag().name, sol);
    }
    return sol;
  }

  pub fn resolve(self: *Self, typ: *Type, debug: Token) !*Type {
    self.ctx.enterTVScope();
    defer self.ctx.leaveTVScope();
    self.resolved_tvars.map.clearRetainingCapacity();
    const ty = try self.tryResolveType(typ, debug);
    // set alias info for debugging
    if (typ.alias == null and ty.alias == null) {
      const alias = self.ctx.copyType(typ);
      if (self.ban_alias) |typs| {
        for (typs) |_ty| {
          if (!_ty.typeidEql(typ)) {
            ty.alias = alias;
            break;
          }
        }
      } else {
        ty.alias = alias;
      }
    }
    // see if we can substitute any 
    if (ty.alias) |alias| {
      if (alias.isGeneric() and alias.hasVariable()) {
        var new_alias = self.ctx.copyType(alias);
        var slice = new_alias.generic().getSlice();
        for (alias.generic().getSlice(), 0..) |_ty, i| {
          if (_ty.isVariable()) {
            if (self.ctx.lookupInTypScope(_ty.variable().lexeme())) |_typ| {
              slice[i] = self.ctx.copyType(_typ);
            } else if (self.resolved_tvars.get(_ty.variable().lexeme())) |_typ| {
              slice[i] = self.ctx.copyType(_typ);
            }
          }
        }
        ty.alias = new_alias;
      }
    }
    return ty;
  }

  inline fn linkList(self: *Self, node: *tir.ListNode) !void {
    for (node.elems) |elem| {
      try self.link(elem);
    }
  }

  inline fn linkMap(self: *Self, node: *tir.MapNode) !void {
    for (node.pairs) |pair| {
      try self.link(pair.key);
      try self.link(pair.value);
    }
  }
  
  fn linkBlock(self: *Self, node: *tir.BlockNode) !void {
    self.ctx.enterScope();
    for (node.nodes) |item| {
      try self.link(item);
    }
    self.ctx.leaveScope();
  }

  pub inline fn linkType(self: *Self, node: *tir.TypeNode) !void {
    node.typ = try self.resolve(node.typ, node.tkbit.toToken());
  }

  pub fn _linkType(self: *Self, typ: *Type, token: Token) !*Type {
    return try self.resolve(typ, token);
  }

  pub fn linkCast(self: *Self, node: *tir.CastNode) !void {
    try self.link(node.expr);
    try self.linkType(node.typn);
  }

  pub inline fn linkVarDecl(self: *Self, node: *tir.VarDeclNode) !void {
    try self.link(node.value);
    if (node.typ) |typ| {
      node.typ = try self.resolve(typ, node.name.toToken());
    }
  }


  pub fn linkAlias(self: *Self, node: *tir.AliasNode) !void {
    var aliasee = node.aliasee.typ;
    var typ = node.alias.typ;
    const lexeme = (
      if (typ.isGeneric()) typ.generic().base.variable().lexeme()
      else typ.variable().lexeme()
    );
    self.insert(lexeme, aliasee);
    // set tags
    typ = aliasee;
    if (typ.isTaggedUnion()) {
      for (typ.taggedUnion().variants.items()) |ty| {
        self.insert(ty.tag().name, ty);
        if (node.alias.typ.isGeneric()) {
          ty.tag().alias_is_parameterized = true;
          if (ty.tag().fieldsLen() > 0) {
            ty.tag().fields.?.items()[0].tdecl = node.alias.typ;
          }
        }
      }
    } else if (typ.isTag()) {
      self.insert(typ.tag().name, typ);
      if (node.alias.typ.isGeneric()) {
        typ.tag().alias_is_parameterized = true;
        if (typ.tag().fieldsLen() > 0) {
          typ.tag().fields.?.items()[0].tdecl = node.alias.typ;
        }
      }
    } else if (!node.alias.typ.isGeneric() and !aliasee.isNeverTy()) {
      self.diag.skipEntry();
      defer self.diag.resumeEntry();
      const ty = self.resolve(aliasee, node.aliasee.tkbit.toToken()) catch {
        self.insert(lexeme, node.aliasee.typ);
        return;
      };
      node.aliasee.typ = ty;
      self.insert(lexeme, node.aliasee.typ);
    }
  }

  pub inline fn linkParam(self: *Self, node: *tir.ParamNode) !void {
    node.typ = try self.resolve(node.typ, node.name);
  }

  pub inline fn linkField(self: *Self, node: *tir.FieldNode) !void {
    if (node.value) |val| try self.link(val);
    if (node.typ) |typ| {
      node.typ = try self.resolve(typ, node.name.toToken());
    }
  }

  pub inline fn linkPubField(self: *Self, node: *tir.PubFieldNode) !void {
    if (node.value) |val| try self.link(val);
    if (node.typ) |typ| {
      node.typ = try self.resolve(typ, node.name.toToken());
    }
  }

  fn linkWhile(self: *Self, node: *tir.WhileNode) !void {
    try self.link(node.cond);
    try self.linkBlock(node.then.block());
  }

  fn linkBasicCall(self: *Self, node: *tir.BasicCallNode) !void {
    try self.link(node.expr);
    for (node.args()) |arg| {
      try self.link(arg);
    }
  }

  pub fn linkGenericCall(self: *Self, node: *tir.GenericCallNode) !void {
    for (node.targs) |typ| {
      typ.* = (try self._linkType(typ, node.call.getToken())).*;
    }
    try self.link(node.call);
  }

  pub fn linkBasicFun(self: *Self, node: *tir.BasicFunNode, tparams: ?tir.TypeItems) !void {
    if (tparams) |tp| self.ban_alias = tp;
    for (node.params) |nd| {
      try self.linkParam(nd);
    }
    if (node.data.ret) |ret| {
      const token = if (node.data.name) |name| name else node.data.body.getToken();
      node.data.ret = try self._linkType(ret, token);
    }
    try self.link(node.data.body);
  }

  pub fn linkGenericFun(self: *Self, node: *tir.GenericFunNode, allow_generic: bool) !void {
    if (!allow_generic) return;
    self.ban_alias = node.params;
    try self.link(node.fun);
  }

  pub fn linkClass(self: *Self, node: *tir.StructNode, token: Token, allow_generic: bool) !void {
    if (node.isParameterized()) {
      if (!allow_generic) return;
      self.ban_alias = node.data.params;
    }
    if (node.trait) |trait| {
      node.trait = try self.resolve(trait, token);
    }
    for (node.data.fields) |field| {
      if (field.isField()) {
        try self.linkField(&field.NdField);
      } else {
        try self.linkPubField(&field.NdPubField);
      }
    }
    for (node.data.methods.items()) |method| {
      if (method.isBasicFun()) {
        try self.linkBasicFun(&method.NdBasicFun, null);
      }
    }
  }

  pub fn linkTrait(self: *Self, node: *tir.StructNode, allow_generic: bool) !void {
    assert(node.data.fields.len == 0);
    if (node.isParameterized()) {
      if (!allow_generic) return;
      self.ban_alias = node.data.params;
    }
    for (node.data.methods.items()) |method| {
      if (method.isBasicFun()) {
        try self.linkBasicFun(&method.NdBasicFun, null);
      }
    }
  }

  inline fn linkRet(self: *Self, node: *tir.RetNode) !void {
    if (node.expr) |expr| {
      try self.link(expr); 
    }
  }

  pub fn link(self: *Self, node: *tir.Node) TypeCheckError!void {
    return switch (node.*) {
      .NdUnary => |*nd| self.link(nd.expr),
      .NdBinary, .NdAssign => |*nd| {
        try self.link(nd.left);
        try self.link(nd.right);
      },
      .NdTVar => |*nd| {
        const typ = self.ctx.var_scope.lookup(nd.value());
        if (nd.typ == null) {
          if (typ) |t| {
            nd.typ = t.typ;
          }
        }
      },
      .NdSubscript => |*nd| {
        try self.link(nd.expr);
        try self.link(nd.index);
      },
      .NdDotAccess => |*nd| {
        try self.link(nd.lhs);
        try self.link(nd.rhs);
      },
      .NdOrElse => |*nd| {
        try self.link(nd.ok);
        try self.link(nd.err);
      },
      .NdSimpleIf => |*nd| {
        try self.link(nd.cond);
        try self.link(nd.then);
        try self.link(nd.els);
      },
      .NdParam => |*nd| self.linkParam(nd),
      .NdField => |*nd| self.linkField(nd),
      .NdPubField => |*nd| self.linkPubField(nd),
      .NdDeref => |*nd| self.link(nd.expr),
      .NdExprStmt => |*nd| self.link(nd.expr),
      .NdBasicCall => |*nd| self.linkBasicCall(nd),
      .NdGenericCall => |*nd| self.linkGenericCall(nd),
      .NdError => |*nd| self.link(nd.expr),
      .NdLblArg => |*nd| self.link(nd.value),
      .NdList, .NdTuple => |*nd| self.linkList(nd),
      .NdMap => |*nd| self.linkMap(nd),
      .NdVarDecl, .NdConstVarDecl  => |*nd| self.linkVarDecl(nd),
      .NdPubVarDecl => |*nd| self.linkVarDecl(nd.getVarDecl()),
      .NdBlock => |*nd| self.linkBlock(nd),
      .NdType => |*nd| self.linkType(nd),
      .NdAlias => |*nd| self.linkAlias(nd),
      .NdCast => |*nd| self.linkCast(nd),
      .NdWhile => |*nd| self.linkWhile(nd),
      .NdBasicFun => |*nd| self.linkBasicFun(nd, null),
      .NdGenericFun => |*nd| self.linkGenericFun(nd, false),
      .NdClass => |*nd| self.linkClass(nd, node.getToken(), false),
      .NdTrait => |*nd| self.linkTrait(nd, false),
      .NdRet => |*nd| self.linkRet(nd),
      .NdImport, .NdNumber, .NdString, .NdBool, .NdControl,
      .NdScope, .NdCondition, .NdMCondition, .NdEmpty, .NdMatch,
      .NdFailMarker, .NdRedunMarker, .NdDiagStartMarker,
      .NdPipeHolder, .NdFor, .NdForCounter, .NdGenericMtd, .NdProgram => {},
    };
  }
};
