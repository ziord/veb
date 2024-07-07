const diagnostics = @import("diagnostics.zig");
const analysis = @import("analysis.zig");
const ds = @import("ds.zig");
const parse = @import("parse.zig");
const ptn = @import("pattern.zig");
const VebAllocator = @import("allocator.zig");
const ks = @import("constants.zig");
const desugar = @import("desugar.zig");
pub const std = @import("std");
pub const fir = @import("fir.zig");
pub const link = @import("link.zig");
pub const util = @import("util.zig");

pub const tir = link.tir;
const prelude = parse.prelude;
const Desugar = desugar.Desugar;
const NodeList = tir.NodeList;
const TypeHashMap = ds.StringHashMap(*Type);
const FlowNode = fir.FlowNode;
const FlowData = fir.FlowData;
const FlowList = fir.FlowList;
const FlowGraph = fir.FlowGraph;
const NeighbourList = fir.NeighbourList;
const CompUnit = fir.CompUnit;
const CFGBuilder = fir.CFGBuilder;
const Scope = link.Scope;
const Token = tir.Token;
const RelationContext = tir.RelationContext;
const AccessSpecifier = tir.AccessSpecifier;
const TypeKind = tir.TypeKind;
const TypeInfo = tir.TypeInfo;
const Union = tir.Union;
const Node = tir.Node;
const TVarNode = tir.TVarNode;
const NodeItems = tir.NodeItems;
const TypeItems = tir.TypeItems;
const TContext = link.TContext;
const TypeLinker = link.TypeLinker;
const TypeHashSet = tir.TypeHashSet;
pub const Type = tir.Type;
pub const Class = tir.Class;
pub const TypeList = tir.TypeList;
const Pattern = ptn.Pattern;
const MatchCompiler = ptn.MatchCompiler;
const Diagnostic = diagnostics.Diagnostic;
const Analysis = analysis.Analysis;
const U8Writer = util.U8Writer;
const keywords = tir.lex.Keywords;
const Allocator = tir.Allocator;
pub const assert = std.debug.assert;
pub const logger = std.log.scoped(.check);

const TypeEnv = struct {
  global: Scope,
  narrowed: TypeHashMap,
  never_ty: *Type,

  pub fn init(never_ty: *Type, allocator: Allocator, other: ?*TypeEnv) @This() {
    var self = @This() {
      .global = Scope.init(allocator),
      .narrowed = TypeHashMap.init(allocator),
      .never_ty = never_ty,
    };
    if (other) |source| {
      self.copy(source);
    }
    return self;
  }

  pub fn display(self: *@This(), prompt: []const u8, u8w: *U8Writer) void {
    std.debug.print("{s}\n", .{prompt});
    var glob = self.global.decls;
    std.debug.print("------------Globals------------\n", .{});
    var i: usize = self.global.len();
    while (i > 0): (i -= 1) {
      var itr = glob.itemAt(i - 1).map.iterator();
      while (itr.next()) |entry| {
        std.debug.print("name: '{s}', ty: '{s}'\n", .{entry.key_ptr.*, entry.value_ptr.*.typename(u8w)});
      }
      std.debug.print("-------------------------------\n", .{});
    }
    std.debug.print("------------Narrowed------------\n", .{});
    var itr_n = self.narrowed.iterator();
    while (itr_n.next()) |entry| {
      var ty = entry.value_ptr.*;
      std.debug.print("name: '{s}', ty: '{s}'\n", .{entry.key_ptr.*, ty.typename(u8w)});
    }
    std.debug.print("-------------------------------\n", .{});
  }

  pub fn copy(self: *@This(), source: *@This()) void {
    // globals don't need to be copied.
    self.global = source.global;
    var itr = source.narrowed.iterator();
    while (itr.next()) |entry| {
      self.putNarrowed(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  pub fn clear(self: *@This()) void {
    self.global.clear();
    self.narrowed.clearRetainingCapacity();
  }

  pub fn promoteNarrowed(self: *@This()) void {
    var itr = self.narrowed.iterator();
    self.global.pushScope();
    while (itr.next()) |entry| {
      self.putGlobal(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  pub inline fn putGlobal(self: *@This(), name: []const u8, ty: *Type) void {
    self.global.insert(name, ty);
  }

  pub inline fn getGlobal(self: *@This(), name: []const u8) ?*Type {
    return self.global.lookup(name);
  }

  pub inline fn putNarrowed(self: *@This(), name: []const u8, ty: *Type) void {
    self.narrowed.set(name, ty);
  }

  pub inline fn getNarrowed(self: *@This(), name: []const u8) ?*Type {
    return self.narrowed.get(name);
  }

  pub fn hasNeverTy(self: *@This()) bool {
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      if (entry.value_ptr.*.isNeverTy()) {
        return true;
      }
    }
    return false;
  }

  /// combines the narrowed types in self with other into a union when possible
  pub fn or_(self: *@This(), other: *@This()) void {
    const al = self.global.allocator();
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      const ident = entry.key_ptr.*;
      var t1 = entry.value_ptr.*;
      if (other.narrowed.get(ident)) |t2| {
        entry.value_ptr.* = t1.unionify(t2, al);
      }
    }
    itr = other.narrowed.iterator();
    while (itr.next()) |entry| {
      if (self.narrowed.get(entry.key_ptr.*) == null) {
        var ty = other.getGlobal(entry.key_ptr.*).?;
        self.putNarrowed(entry.key_ptr.*, ty.unionify(entry.value_ptr.*, al));
      }
    }
  }

  /// computes the intersection of the narrowed types in self with other
  pub fn and_(self: *@This(), other: *@This()) void {
    const allocator = self.global.allocator();
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      const ident = entry.key_ptr.*;
      var t1 = entry.value_ptr.*;
      if (other.narrowed.get(ident)) |t2| {
        entry.value_ptr.* = (
          t1.intersect(t2, allocator)
          orelse self.never_ty
        );
      }
    }
    itr = other.narrowed.iterator();
    while (itr.next()) |entry| {
      if (self.narrowed.get(entry.key_ptr.*) == null) {
        self.putNarrowed(entry.key_ptr.*, entry.value_ptr.*);
      }
    }
  }

  /// transforms types by negation - negates the narrowed types using the global types
  pub fn not_(self: *@This(), name: []const u8) !bool {
    const allocator = self.global.allocator();
    if (self.narrowed.get(name)) |t2| {
      var t1 = self.global.lookup(name).?;
      const neg = blk: {
        break :blk t1.negate(t2, allocator) catch {
          logger.debug("negate with t1 exactly equal to t2 results in the 'never' type", .{});
          break :blk (
            if (t1.typeidEql(t2)) self.never_ty
            else t1
          );
        };
      };
      self.narrowed.set(name, neg);
      return neg != t1;
    }
    return false;
  }
};


pub const TypeChecker = struct {
  al: Allocator,
  linker: TypeLinker = undefined,
  namegen: util.NameGen,
  builder: CFGBuilder,
  analyzer: Analysis,
  u8w: U8Writer,
  diag: *Diagnostic,
  des: Desugar,
  cunit: *CompUnit = undefined,
  /// track narrowing
  narrowing: u32 = 0,
  /// type context
  ctx: TContext,
  /// generic function/class info
  generics: ds.ArrayHashMap(*Node, *ds.ArrayList(GenInfo)),
  /// inferred class methods
  methods: ds.StringArrayHashMap(*std.AutoHashMap(u32, InferredFunction)),
  /// inferred modules
  modules: ds.StringHashMapUnmanaged(InferredModule),
  /// function nodes with cyclic references
  cycles: tir.NodeListU,
  /// class types being resolved at hand
  resolving: TypeList,
  /// builtin types
  builtins: TypeList,
  /// prelude entries
  prelude: Prelude,
  /// type environment for narrowing
  tenv: ?*TypeEnv = null,
  /// meta for match nodes
  matchmeta: MatchMeta,
  /// current function/method being type checked
  current_fn: ?*Node = null,
  /// manage missing methods arising from trait impls
  missing_tmtds: ds.ArrayList(TraitInfo),
  /// cached types
  void_ty: *Type,
  str_ty: *Type,
  never_ty: *Type,
  tvar_ty: *Type,
  mvar_ty: *Type,

  const Self = @This();
  const ConsList = ds.ArrayList(*ptn.Constructor);

  const MAX_STRING_SYNTH_LEN = 0xc;

  pub const TypeCheckError = (
    MatchCompiler.MatchError ||
    TypeLinker.TypeLinkError ||
    error{
      SynthFailure, SynthTooLarge, DeadCode,
      DotAccessIntercept, CheckError, DesugarError
    }
  );

  /// cached 'unit' types
  const UnitTypes = struct {
    const num = tir.Concrete.init(.TyNumber);
    const str = tir.Concrete.init(.TyString);
    const bol = tir.Concrete.init(.TyBool);
    const nil = tir.Concrete.init(.TyNil);
    const tyty = tir.Top.init(&tyAny);

    var tyNumber: Type = Type.init(.{.Concrete = num});
    var tyString: Type = Type.init(.{.Concrete = str});
    var tyBool: Type = Type.init(.{.Concrete = bol});
    var tyNil: Type = Type.init(.{.Concrete = nil});
    var tyAny: Type = Type.newConcrete(.TyAny);
    var tyVoid: Type = Type.newConcrete(.TyVoid);
    var tyNoReturn: Type = Type.newConcrete(.TyNoReturn);
    var TyTy: Type = Type.init(.{.Top = tyty});
  };

  /// manage 'match' related data
  const MatchMeta = struct {
    /// list of conditions for which we may infer inexhaustiveness
    tests: TestList,
    /// list of missing patterns
    missing: ds.StringArrayHashMap(Token),
    /// manage redundancy warnings arising from match cases
    warnings: RedundancyMap,

    /// PtnTest -> BinaryNode, FlowNode, ConditionNode
    const PtnTest = struct {*Node, FlowNode, *Node};
    const TestList = ds.ArrayList(PtnTest);
    const RedundancyMap = ds.ArrayHashMap(Token, u32);

    pub fn init(allocator: Allocator) MatchMeta {
      return .{
        .tests = TestList.init(allocator),
        .missing = ds.StringArrayHashMap(Token).init(allocator),
        .warnings = RedundancyMap.init(allocator),
      };
    }
  };

  /// generic information
  pub const GenInfo = struct {
    /// monomorphized generic class/function node
    instance: *Node,
    /// actual type
    typ: *Type,
    /// synthesized name of the generic type
    synth_name: Token,
  };

  /// inferred method/function information
  const InferredFunction = struct {
    node: *Node,
    partial_ty: *Type,
    full_ty: *Type,
  };

  /// inferred module with its compilation unit
  const InferredModule = struct {
    cunit: *CompUnit,
    typ: *Type,
  };

  /// prelude nodes
  pub const Prelude = struct {core: *Node};

  /// name, typename, token
  pub const TraitInfo = struct{[]const u8, []const u8, Token};

  pub fn init(allocator: Allocator, diag: *Diagnostic, namegen: util.NameGen) Self {
    return .{
      .al = allocator,
      .ctx = undefined,
      .diag = diag,
      .linker = TypeLinker.init(undefined, diag, undefined, allocator),
      .modules = ds.StringHashMapUnmanaged(InferredModule).init(),
      .namegen = util.NameGen.init(allocator),
      .generics = ds.ArrayHashMap(*Node, *ds.ArrayList(GenInfo)).init(allocator),
      .methods = ds.StringArrayHashMap(*std.AutoHashMap(u32, InferredFunction)).init(allocator),
      .builder = CFGBuilder.init(allocator),
      .cycles = tir.NodeListU.init(),
      .resolving = TypeList.init(),
      .analyzer = Analysis.init(diag),
      .builtins = TypeList.init(),
      .u8w = U8Writer.init(allocator),
      .prelude = undefined,
      .des = Desugar.init(namegen, diag),
      .matchmeta = MatchMeta.init(allocator),
      .missing_tmtds = ds.ArrayList(TraitInfo).init(allocator),
      .str_ty = undefined,
      .void_ty = Type.newVoid().box(allocator),
      .tvar_ty = getTVarType(ks.GeneratedTypeVar, allocator),
      .mvar_ty = getTVarType(ks.UnderscoreVar, allocator),
      .never_ty = Type.newNever(allocator),
    };
  }

  fn loadPrelude(self: *Self) void {
    const root = prelude.CoreNode;
    self.buildProgramFlow(root, true) catch unreachable;
    self.ctx.enterPreludeModule(Type.newModule(root, "core", self.al));
    self.flowInferEntry(self.cunit.program.entry()) catch unreachable;
    for (root.NdProgram.decls) |node| {
      if (node.isFun()) {
        const fun = node.getBasicFun();
        if (fun.data.builtin) {
          fun.data.body.block().checked = true;
        }
      } else if (node.isClass()) {
        if (!node.NdClass.data.builtin) continue;
        node.NdClass.data.checked = true;
        for (node.NdClass.data.methods.items()) |method| {
          method.getBasicFun().data.builtin = true;
        }
        if (node.NdClass.data.tktype != .TkStr) {
          self.builtins.append(self.inferClassPartial(node) catch unreachable, self.al);
        } else {
          const ty = self.inferClassPartial(node) catch unreachable;
          self.str_ty = self.inferClass(node, ty, &[_]*Type{}, &[_]*Type{}) catch unreachable;
          self.builtins.append(self.str_ty, self.al);
        }
      }
    }
    self.prelude = .{.core = root};
  }

  inline fn inCoreModule(self: *Self) bool {
    return (
      self.ctx.modules.len() == 1 and
      self.ctx.cm().node.NdProgram.filepath.ptr == ks.PreludeFilename.ptr
    );
  }

  inline fn getTVarType(txt: []const u8, al: Allocator) *Type {
    return Type.newVariableAToken(Token.getDefaultToken().tkFrom(txt, .TkIdent), al);
  }

  pub inline fn getNarrowed(self: *Self, node: *Node) ?*Node {
    _ = self;
    if (node.isTVariable()) return node;
    return node.getNarrowed();
  }

  pub inline fn getType(self: *Self, node: *Node) ?*Type {
    _ = self;
    return node.getType();
  }

  pub inline fn getTypeNodeType(node: *Node) *Type {
    return node.NdType.typ;
  }

  pub fn error_(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) self.diag.addDiagnostics(token, "TypeError: " ++ fmt, args);
    return error.CheckError;
  }

  pub fn errorFrom(self: *Self, emit: bool, node: *Node, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) self.diag.addDiagnostics(node.getToken(), "TypeError: " ++ fmt, args);
    return error.CheckError;
  }

  pub fn softError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnostics(token, "TypeError: " ++ fmt, args);
  }

  pub fn softErrorFrom(self: *Self, node: *Node, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnostics(node.getToken(), "TypeError: " ++ fmt, args);
  }

  pub fn softErrorFmt(self: *Self, token: Token, diag_depth: u32, comptime spaces: u32, comptime fmt: []const u8, args: anytype) void {
    comptime var space = @as([]const u8, "");
    inline for(0..spaces) |_| {
      space = space ++ @as([]const u8, " ");
    }
    self.diag.addDiagnosticsWithDepth(token, diag_depth, space ++ fmt, args);
  }

  pub fn errorFmt(self: *Self, token: Token, diag_depth: u32, comptime spaces: u32, comptime fmt: []const u8, args: anytype) TypeCheckError {
    self.softErrorFmt(token, diag_depth, spaces, fmt, args);
    return error.CheckError;
  }

  inline fn warn(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) void {
    if (emit) self.diag.addDiagnosticsWithLevel(.DiagWarn, token, "TypeWarning: " ++ fmt, args);
  }

  inline fn genName(al: Allocator, l1: usize, l2: usize) []const u8 {
    return std.fmt.allocPrint(al, "${}.{}", .{l1, l2}) catch @panic("could not gen name");
  }

  fn makeSynthName(self: *Self, name: ?[]const u8, is_builtin: bool, args: tir.TypeItems, targs: ?TypeItems, cls_ty: ?*Type) []const u8 {
    _ = is_builtin;
    // FIXME: this is unhygienically inefficient.
    var start = name orelse genName(
      self.al,
      if (targs) |ta| ta.len else 0,
      args.len
    );
    if (cls_ty) |ty| {
      start = std.fmt.allocPrint(
        self.al, "${s}.{s}.{}", .{start, ty.klassOrTrait().data.name, ty.typeid()}
      ) catch @panic("could not gen cls name");
    }
    var list = std.ArrayList(u8).init(self.al);
    var writer = list.writer();
    _ = writer.write(start) catch undefined;
    var buf: [100]u8 = undefined;
    if (targs) |ta| {
      if (args.len == 0) {
        for (ta) |ty| {
          _ = writer.write(".") catch undefined;
          const id_str = std.fmt.bufPrint(&buf, "{}", .{ty.typeid()}) catch unreachable;
          _ = writer.write(id_str) catch undefined;
        }
      }
    }
    for (args) |ty| {
      _ = writer.write(".") catch undefined;
      const id_str = std.fmt.bufPrint(&buf, "{}", .{ty.typeid()}) catch unreachable;
      _ = writer.write(id_str) catch undefined;
    }
    return writer.context.items;
  }

  fn createFunSynthName(self: *Self, fun: *tir.BasicFunNode, args: *TypeList, targs: ?TypeItems, cls_ty: ?*Type) []const u8 {
    if (fun.data.builtin) return fun.data.name.?.lexeme();
    return self.makeSynthName(if (fun.data.name) |nm| nm.lexeme() else null, fun.data.builtin, args.items(), targs, cls_ty);
  }

  inline fn boxSynthName(token: Token, val: []const u8) Token {
    return token.tkFrom(val, .TkIdent);
  }

  fn addGenInfo(self: *Self, origin: *Node, instance: *Node, synth_name: Token, typ: *Type) void {
    const data = @as(GenInfo, .{
      .instance = instance,
      .synth_name = synth_name,
      .typ = typ,
    });
    if (self.generics.get(origin)) |list| {
      list.append(data);
    } else {
      self.generics.set(origin, ds.ArrayList(GenInfo).initWith(self.al, data).box());
    }
  }

  fn findGenInfo(self: *Self, origin: *Node, synth_name: []const u8) ?GenInfo {
    if (self.generics.get(origin)) |list| {
      for (list.items()) |info| {
        if (info.synth_name.valueEql(synth_name)) {
          return info;
        }
      }
    }
    return null;
  }

  fn findBuiltinType(self: *Self, name: []const u8) ?*Type {
    for (self.builtins.items()) |itm| {
      if (std.mem.eql(u8, itm.klass().data.name, name)) {
        return itm;
      }
    }
    return null;
  }

  inline fn findCoreType(self: *Self, name: []const u8) ?*Type {
    return self.ctx.core_typ_scope.lookup(name);
  }

  inline fn findType(self: *Self, name: []const u8) ?*Type {
    return self.ctx.lookupInTypScope(name);
  }

  inline fn findName(self: *Self, name: []const u8) ?*Type {
    return self.ctx.var_scope.lookup(name);
  }

  fn lookupName(self: *Self, ident: anytype, emit: bool) !*Type {
    if (@TypeOf(ident) == *tir.TVarNode) {
      if (self.findName(ident.value())) |found| {
        return found;
      } else if (self.findType(ident.value())) |found| {
        return found;
      } else {
        return self.error_(
          emit, ident.token, "Could not resolve type of ident: '{s}'",
          .{ident.value()}
        );
      }
    } else {
      if (self.findName(ident.lexeme())) |found| {
        return found;
      } else if (self.findType(ident.lexeme())) |found| {
        return found;
      } else {
        return self.error_(
          emit, ident, "Could not resolve type of ident: '{s}'",
          .{ident.lexeme()}
        );
      }
    }
  }

  fn lookupNameWithInfo(self: *Self, ident: anytype, emit: bool) !struct{typ: *Type, ident: bool} {
    if (@TypeOf(ident) == *tir.TVarNode) {
      if (self.findName(ident.value())) |found| {
        return .{.typ = found, .ident = true};
      } else if (self.findType(ident.value())) |found| {
        return .{.typ = found, .ident = false};
      } else {
        return self.error_(emit, ident.token, "Could not resolve type of ident: '{s}'", .{ident.value()});
      }
    } else {
      if (self.findName(ident.lexeme())) |found| {
        return .{.typ = found, .ident = true};
      } else if (self.findType(ident.lexeme())) |found| {
        return .{.typ = found, .ident = false};
      } else {
        return self.error_(emit, ident, "Could not resolve type of ident: '{s}'", .{ident.lexeme()});
      }
    }
  }

  fn insertType(self: *Self, var_typ: *Type, typ: *Type) void {
    self.ctx.typ_scope.insert(var_typ.variable().lexeme(), typ);
  }

  inline fn insertCoreType(self: *Self, name: []const u8, typ: *Type) void {
    if (self.findCoreType(name) == null) {
      self.ctx.core_typ_scope.insert(name, typ);
    }
    self.ctx.typ_scope.insert(name, typ);
  }

  pub inline fn insertVar(self: *Self, name: []const u8, ty: *Type) void {
    self.ctx.var_scope.insert(name, ty);
  }

  pub inline fn insertTy(self: *Self, name: []const u8, ty: *Type) void {
    self.ctx.typ_scope.insert(name, ty);
  }

  pub inline fn updateVar(self: *Self, name: []const u8, ty: *Type) void {
    _ = self.ctx.var_scope.update(name, ty);
  }

  pub inline fn deleteVar(self: *Self, name: []const u8) void {
    self.ctx.var_scope.remove(name);
  }

  pub fn getTypename(self: *Self, typ: *Type) []const u8 {
    return typ.typename(&self.u8w);
  }

  fn newIdentNode(self: *Self, val: []const u8, token: Token) *Node {
    return Node.new(.{.NdTVar = TVarNode.init(token.tkFrom(val, .TkIdent))}, self.al);
  }

  /// copy env to varScope
  fn copyEnv(self: *Self, env: *TypeEnv) void {
    var itr = env.global.current().map.iterator();
    while (itr.next()) |entry| {
      self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
    }
    itr = env.narrowed.iterator();
    while (itr.next()) |entry| {
      self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  //***********************************************************//
  //***********  narrowing  ***********************************//
  //***********************************************************//

  fn _synth(self: *Self,  vr: Token, value: []const u8, env: *TypeEnv) TypeCheckError!*TVarNode{
    const val = std.fmt.allocPrint(self.al, "{s}.{s}", .{vr.lexeme(), value})
      catch return error.SynthFailure;
    var node = TVarNode.init(vr.tkFrom(val, .TkIdent)).box(self.al);
    self.diag.skipEntry();
    defer self.diag.resumeEntry();
    _ = self.narrowVariable(node, env) catch {};
    return node;
  }

  fn synthesizeVar(self: *Self, node: *TVarNode, other: ?*Node, env: *TypeEnv) TypeCheckError!*TVarNode {
    if (node.typ == null) {
      node.typ = try self.inferVar(node, false);
    }
    if (other) |oth| {
      const value: []const u8 = switch (oth.*) {
        .NdNumber => |*nd| nd.lexeme(self.al),
        .NdString => |*nd| nd.token.lexeme(),
        .NdBool => |*nd| nd.token.lexeme(),
        .NdTVar => |*id| id.token.lexeme(),
        else => |els| {
          std.debug.print("unexpected tir type for synth {}", .{els});
          return error.SynthFailure;
        }
      };
      if (value.len > MAX_STRING_SYNTH_LEN) {
        return error.SynthTooLarge;
      }
      return try self._synth(node.token, value, env);
    } else {
      return node;
    }
  }

  fn synthesizeSubscript(self: *Self, node: *tir.SubscriptNode, env: *TypeEnv, assume_true: bool) TypeCheckError!*TVarNode {
    var narrowed = (try self.narrow(node.expr, env, assume_true)).?;
    const ty = try self.inferSubscript(node, true);
    narrowed = try self.synthesizeVar(narrowed, node.index, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    return narrowed;
  }

  fn synthesizeDotAccess(self: *Self, node: *tir.DotAccessNode, ast_n: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!*TVarNode  {
    var narrowed = (try self.narrow(node.lhs, env, assume_true)).?;
    const ty = try self.inferDotAccess(node, ast_n, true);
    narrowed = try self.synthesizeVar(narrowed, node.rhs, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    return narrowed;
  }

  fn synthesizeDeref(self: *Self, node: *tir.DerefNode, env: *TypeEnv, assume_true: bool) TypeCheckError!*TVarNode {
    var narrowed = (try self.narrow(node.expr, env, assume_true)).?;
    const ty = try self.inferDeref(node, true);
    narrowed = try self.synthesizeVar(narrowed, self.newIdentNode("?", node.tkbit.toToken()), env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    return narrowed;
  }

  fn synthesize(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) !*TVarNode {
    return switch (node.*) {
      .NdTVar => try self.synthesizeVar(node, null, env),
      .NdSubscript => |*sub| try self.synthesizeSubscript(sub, env, assume_true),
      .NdDotAccess => |*da| try self.synthesizeDotAccess(da, node, env, assume_true),
      .NdDeref => |*der| try self.synthesizeDeref(der, env, assume_true),
      else => error.SynthFailure,
    };
  }

  inline fn isNarrowableLiteral(self: *Self, node: *tir.BinaryNode) bool {
    return (
      (node.optype() == .OpEqq or node.optype() == .OpNeq)
      and self.canNarrow(node.left)
      and (
        node.right.isNoneLiteral()
        or node.right.isConstLiteral()
        or (
          node.right.isTVariable()
          and node.right.NdTVar.valueEql(ks.NoneVar)
        )
      )
    );
  }

  inline fn canNarrowSubscript(self: *Self, node: *tir.SubscriptNode) bool {
    return self.canNarrow(node.expr) and node.index.isComptimeConst();
  }

  inline fn canNarrowDeref(self: *Self, node: *tir.DerefNode) bool {
    return self.canNarrow(node.expr);
  }

  inline fn canNarrowDotAccess(self: *Self, node: *tir.DotAccessNode) bool {
    return self.canNarrow(node.lhs);
  }

  fn canNarrow(self: *Self, node: *Node) bool {
    if (self.narrowing == 0) return false;
    return switch (node.*) {
      .NdTVar => true,
      .NdSubscript => |*sub| self.canNarrowSubscript(sub),
      .NdDeref => |*der| self.canNarrowDeref(der),
      .NdDotAccess => |*da| self.canNarrowDotAccess(da),
      else => false,
    };
  }

  fn narrowAtomic(self: *Self, node: *Node, env: *TypeEnv) !?*TVarNode {
    _ = env;
    _ = try self.infer(node);
    return null;
  }

  fn narrowVariable(self: *Self, node: *TVarNode, env: *TypeEnv) !?*TVarNode {
    const value = node.token.lexeme();
    if (env.getNarrowed(value)) |ty| {
      self.insertVar(value, ty);
      node.typ = ty;
    } else if (env.getGlobal(value)) |ty| {
      self.insertVar(value, ty);
      node.typ = ty;
    } else {
      var ty = try self.lookupName(node, true);
      if (!ty.isBoolTy()) {
        env.putGlobal(value, ty);
      } else {
        // translate `bool` into `true | false`
        const typ = Type.newBoolUnion(self.al);
        env.putGlobal(value, typ);
        self.insertVar(value, typ);
      }
      node.typ = ty;
    }
    return node;
  }

  fn narrowCast(self: *Self, node: *tir.CastNode, ast_n: *Node, env: *TypeEnv, assume_true: bool) !?*TVarNode {
    const ret = try self.narrow(node.expr, env, assume_true);
    const ty = try self.inferCast(node, ast_n);
    if (ret) |nrw| nrw.typ = ty;
    return ret;
  }

  fn narrowSubscript(self: *Self, node: *tir.SubscriptNode, env: *TypeEnv, assume_true: bool) !?*TVarNode {
    if (self.canNarrowSubscript(node)) {
      const synth = try self.synthesizeSubscript(node, env, assume_true);
      if (self.findName(synth.value())) |_| {
        _ = try self.narrowVariable(synth, env);
        node.typ = env.getGlobal(synth.value());
      } else {
        self.insertVar(synth.value(), synth.typ.?);
      }
      return synth;
    } else {
      _ = try self.narrow(node.expr, env, assume_true);
      _ = try self.narrow(node.index, env, assume_true);
      _ = try self.inferSubscript(node, true);
      return null;
    }
  }

  fn narrowDeref(self: *Self, node: *tir.DerefNode, env: *TypeEnv, assume_true: bool) !?*TVarNode {
    if (self.canNarrowDeref(node)) {
      const synth = try self.synthesizeDeref(node, env, assume_true);
      if (self.findName(synth.value())) |_| {
        _ = try self.narrowVariable(synth, env);
        node.typ = env.getGlobal(synth.value());
      } else {
        self.insertVar(synth.value(), synth.typ.?);
      }
      return synth;
    } else {
      _ = try self.narrow(node.expr, env, assume_true);
      _ = try self.inferDeref(node, true);
      return null;
    }
  }

  fn narrowDotAccess(self: *Self, node: *tir.DotAccessNode, ast_n: *Node, env: *TypeEnv, assume_true: bool) !?*TVarNode {
    if (self.canNarrowDotAccess(node)) {
      const synth = try self.synthesizeDotAccess(node, ast_n, env, assume_true);
      if (self.findName(synth.value())) |_| {
        _ = try self.narrowVariable(synth, env);
        node.typ = env.getGlobal(synth.value());
      } else {
        self.insertVar(synth.value(), synth.typ.?);
      }
      return synth;
    } else {
      _ = try self.narrow(node.lhs, env, assume_true);
      _ = try self.inferDotAccess(node, ast_n, true);
      return null;
    }
  }

  fn narrowUnary(self: *Self, node: *tir.UnaryNode, env: *TypeEnv, assume_true: bool) !?*TVarNode {
    if (node.op.optype() == .OpNot) {
      _ = try self.narrow(node.expr, env, !assume_true);
      // at this point, node.expr type-checked successfully
      node.typ = UnitTypes.bol.toType().box(self.al);
    } else {
      _ = try self.narrow(node.expr, env, assume_true);
      _ = try self.inferUnary(node);
    }
    return null;
  }

  fn narrowBinary(self: *Self, node: *tir.BinaryNode, env: *TypeEnv, assume_true: bool) !?*TVarNode {
    if (node.optype() == .OpIs) {
      var lnode = node.left;
      const token = Token.fromBinaryNode(node);
      if (lnode.isTVariable()) {
        _ = try self.narrowVariable(&lnode.NdTVar, env);
        _ = try self.inferBinary(node);
        const ty = try self.lookupName(&lnode.NdTVar, true);
        if (Type.is(ty, getTypeNodeType(node.right), self.al)) |is_ty| {
          env.putNarrowed(lnode.NdTVar.token.lexeme(), is_ty);
          if (!assume_true) {
            if (!try env.not_(lnode.NdTVar.value())) {
              self.warn(
                true, token,
                "Cannot narrow type '{s}'",
                .{self.getTypename(node.left.getType().?)}
              );
            }
          }
          lnode.NdTVar.typ = is_ty;
          return null;
        }
      } else if (self.canNarrow(node.left)) {
        if (try self.narrow(node.left, env, assume_true)) |left| {
          var synth = node.*;
          synth.left = @constCast(&@as(Node, .{.NdTVar = left.*}));
          _ = try self.narrowBinary(&synth, env, assume_true);
          node.typ = synth.typ;
          return null;
        }
      }
      if (getTypeNodeType(node.right).isConstant()) {
        _ = try self.narrow(node.left, env, assume_true);
        _ = try self.narrow(node.right, env, assume_true);
        _ = try self.inferBinary(node);
        return null;
      }
      const lty = node.left.getType();
      const rty = node.right.getType();
      if (lty != null and rty != null) {
        self.warn(
          true, token,
          "Cannot narrow type '{s}' to '{s}'",
          .{self.getTypename(lty.?), self.getTypename(rty.?)}
        );
      } else {
        self.warn(
          true, token,
          "Cannot narrow type at expression",
          .{}
        );
      }
    } else if (node.optype() == .OpAnd) {
      env.global.pushScope();
      if (assume_true) {
        _ = try self.narrow(node.left, env, true);
        var rhs_env = TypeEnv.init(self.never_ty, self.al, env);
        rhs_env.promoteNarrowed();
        _ = try self.narrow(node.right, &rhs_env, true);
        env.and_(&rhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.al);
      } else {
        _ = try self.narrow(node.left, env, false);
        var lhs_env = TypeEnv.init(self.never_ty, self.al, null);
        lhs_env.global = env.global;
        _ = try self.narrow(node.left, &lhs_env, true);
        _ = try self.narrow(node.right, &lhs_env, false);
        env.or_(&lhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.al);
      }
      return null;
    } else if (node.optype() == .OpOr) {
      if (assume_true) {
        _ = try self.narrow(node.left, env, true);
        var lhs_env = TypeEnv.init(self.never_ty, self.al, null);
        lhs_env.global = env.global;
        _ = try self.narrow(node.left, &lhs_env, false);
        _ = try self.narrow(node.right, &lhs_env, true);
        env.or_(&lhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.al);
      } else {
        _ = try self.narrow(node.left, env, false);
        var rhs_env = TypeEnv.init(self.never_ty, self.al, env);
        rhs_env.promoteNarrowed();
        _ = try self.narrow(node.right, &rhs_env, false);
        env.and_(&rhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.al);
      }
      return null;
    } else if (self.isNarrowableLiteral(node)) {
      const saved = node.*;
      const optype = node.op_tkty.optype();
      defer {
        const typ = node.typ;
        node.* = saved;
        node.typ = typ;
      }
      node.op_tkty = .TkIs;
      node.allow_consts = true;
      node.right = node.right.toTypeNode(node.right, self.al);
      _ = try self.narrowBinary(node, env, if (optype == .OpEqq) assume_true else !assume_true);
      return null;
    }
    _ = try self.narrow(node.left, env, assume_true);
    _ = try self.narrow(node.right, env, assume_true);
    _ = try self.inferBinary(node);
    return null;
  }

  fn narrow(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!?*TVarNode {
    self.narrowing += 1;
    defer self.narrowing -= 1;
    return switch (node.*) {
      .NdTVar => |*vr| self.narrowVariable(vr, env),
      .NdBinary => |*bin| self.narrowBinary(bin, env, assume_true),
      .NdUnary => |*una| self.narrowUnary(una, env, assume_true),
      .NdCast => |*cst| self.narrowCast(cst, node, env, assume_true),
      .NdSubscript => |*sub| self.narrowSubscript(sub, env, assume_true),
      .NdDeref => |*der| self.narrowDeref(der, env, assume_true),
      .NdDotAccess => |*da| self.narrowDotAccess(da, node, env, assume_true),
      .NdNumber,
      .NdString,
      .NdBool,
      .NdList,
      .NdMap,
      .NdType => self.narrowAtomic(node, env),
      else => |*d| {
        logger.debug("attempt to narrow node: {}", .{d});
        return null;
      },
    };
  }
  //***********************************************************//
  //****************  flow  ***********************************//
  //***********************************************************//

  /// recursively find the `is` test expression. Entry node is a ConditionNode.
  fn getTestFromCondition(self: *Self, tst: *Node) *Node {
    if (tst.isTVariable()) {
      return tst;
    } else if (tst.isCondition()) {
      return self.getTestFromCondition(tst.NdCondition.cond);
    } else if (tst.isBinary()) {
      const bin = &tst.NdBinary;
      if (bin.optype() == .OpIs or bin.left.isCall()) {
        return tst;
      }
      var node = self.getTestFromCondition(bin.left);
      if (node.isTVariable()) {
        return tst;
      }
      return node;
    } else {
      return tst;
    }
  }

  fn getIdent(self: *Self, node: *tir.BinaryNode) *TVarNode {
    _ = self;
    if (!node.left.isCall()) {
      return &node.left.NdTVar;
    } else {
      const call = &node.left.NdBasicCall;
      assert(call.expr.isDotAccess());
      assert(call.expr.NdDotAccess.lhs.isTVariable());
      return &call.expr.NdDotAccess.lhs.NdTVar;
    }
  }

  fn isLengthCheck(self: *Self, n: *Node) bool {
    _ = self;
    if (n.isBinary()) {
      var node = &n.NdBinary;
      if (node.left.isCall()) {
        const call = &node.left.NdBasicCall;
        if (call.expr.isDotAccess() and call.expr.NdDotAccess.rhs.isTVariable()) {
          return call.expr.NdDotAccess.rhs.NdTVar.valueEql(ks.LenVar);
        }
      }
    }
    return false;
  }

  inline fn isEmptyOutgoingNodes(out_nodes: *FlowList) bool {
    if (out_nodes.isEmpty()) return true;
    if (out_nodes.len() == 1) {
      var nodes = out_nodes.itemAt(0).get().bb.items();
      for (nodes) |node| {
        if (!node.isMarker() and !node.isExitScope() and !node.isEmpty()) { 
          return false;
        }
      }
      return true;
    }
    var resolved: usize = 0;
    for (out_nodes.items()) |nd| {
      if (nd.get().res.isResolved()) {
        resolved += 1;
      }
    }
    return resolved == 1;
  }

  fn getTagOrClassFieldLen(self: *Self, typ: ?*Type) usize {
    _ = self;
    return if (typ) |ty| (
      if (ty.isClass()) (
        if (ty.klass().builtin) ty.klass().tparamsLen()
        else ty.klass().fieldsLen()
      ) else ty.tag().fieldsLen()
    ) else 0;
  }

  fn getTagOrClassNameInfo(self: *Self, typ: *Type) struct{[]const u8, tir.TokenType} {
    _ = self;
    return (
      if (typ.isTagOrClass()) .{typ.toc().name, typ.toc().tktype}
      else .{typ.klass().data.name, typ.klass().tktype}
    );
  }

  /// build missing types for map patterns
  fn buildMapPatternTypes(
    self: *Self, ident: *TVarNode, is_direct_tst: bool,
    default: *Type, nghbs: *fir.NeighbourList, al: Allocator,
  ) *Type {
    for (nghbs.items()) |ngh| {
      for (ngh.node.get().bb.nodes.items()) |node| {
        if (node.isVarDecl() and (!is_direct_tst or ident.valueEql(node.NdVarDecl.name.lexeme()))) {
          var val = node.NdVarDecl.value;
          if (val.isSubscript()) {
            var lexeme = val.NdSubscript.expr.NdTVar.value();
            const is_key = lexeme[lexeme.len - 2] == 'y';
            var key_typ: *Type = undefined;
            var val_typ: *Type = undefined;
            if (is_key) {
              key_typ = val.getType().?;
              val_typ = self.mvar_ty;
            } else {
              key_typ = self.mvar_ty;
              val_typ = val.getType().?;
            }
            var ret = Type.newBuiltinGenericClass(ks.MapVar, .TkMap, al);
            ret.klass().initTParamSlice(@constCast(&[_]*Type{key_typ, val_typ}), al);
            return ret;
          }
        }
      }
    }
    return default;
  }

  fn buildListPatternTypes(default: *Type, al: Allocator) *Type {
    var _ty = Type.newBuiltinGenericClass(ks.ListVar, .TkMap, al);
    _ty.klass().appendTParam(default, al);
    return _ty;
  }

  /// compute missing pattern types
  fn buildMissingPatternTypes(self: *Self, default: *Type, ident: *TVarNode, is_direct_tst: bool) *Type {
    // works by using the fail type `default`, and picking the constructor
    // from which the failure escalated. When the constructor is found,
    // the field/param corresponding to the test from which failure was
    // triggered is calculated by locating the position in the constructor
    // where the field was accessed.
    const al = self.al;
    var j = self.matchmeta.tests.len() - 1;
    var last_tst: MatchMeta.PtnTest = undefined;
    for (0..self.matchmeta.tests.len()) |i| {
      var tst = self.matchmeta.tests.itemAt(j - i);
      if (tst.@"0".isBinary()) {
        var right = tst.@"0".NdBinary.right;
        if (right.isType()) {
          const name_info = self.getTagOrClassNameInfo(right.NdType.typ);
          const name = name_info.@"0";
          const tktype = name_info.@"1";
          var typ = self.findType(name);
          // use the conditions flow node to get next neighbours
          var nghbs = tst.@"1".getNextNeighbours();
          if (typ) |t| {
            if (t.isMapTy()) {
              nghbs = last_tst.@"1".getNextNeighbours();
              return self.buildMapPatternTypes(ident, is_direct_tst, default, nghbs, al);
            } else if (t.isListTy()) {
              nghbs = last_tst.@"1".getNextNeighbours();
            }
          }
          for (nghbs.items()) |ngh| {
            var bb = ngh.node.get().bb.nodes.items();
            for (bb) |node| {
              if (node.isVarDecl() and ident.valueEql(node.NdVarDecl.name.lexeme())) {
                var val = node.NdVarDecl.value;
                var idx = blk: {
                  if (val.isDotAccess()) {
                    break :blk val.NdDotAccess.rhs;
                  } else if (val.isSubscript()) {
                    typ = val.NdSubscript.expr.getType();
                    break :blk val.NdSubscript.index;
                  } else {
                    assert(val.isTVariable());
                    break :blk val;
                  }
                };
                var typ_idx: ?usize = null;
                var fields_len = self.getTagOrClassFieldLen(typ);
                if (idx.isNumberLiteral()) {
                  typ_idx = @intFromFloat(idx.NdNumber.value);
                } else if (typ) |ty| {
                  typ_idx = (
                    if (ty.isClass()) ty.klass().getFieldIndex(idx.NdTVar.value()).?
                    else ty.tag().getFieldWithId(idx.NdTVar.value()) orelse 0 // FIXME
                  );
                } else if (idx.isTVariable()) {
                  // FIXME: figure out the best index for this
                  typ_idx = 0;
                }
                if (typ_idx) |index| {
                  if (typ.?.isListTy()) {
                    // attaching the list building here helps deduplicate the built type
                    return buildListPatternTypes(default, al);
                  }
                  var _ty = Type.newTagWithParamTypes(name, tktype, @constCast(&[_]*Type{}), al);
                  for (0..fields_len) |k| {
                    if (k != index) {
                      _ty.tag().appendFieldType(self.mvar_ty, al);
                    } else {
                      _ty.tag().appendFieldType(default, al);
                    }
                  }
                  return _ty;
                }
              }
            }
          }
        }
      }
      last_tst = tst;
    }
    return default;
  }

  fn flowInferProgram(self: *Self, cunit: *CompUnit, typ: *Type) void {
    // TODO: should cunit be updated here?
    // const curr_cunit = self.cunit;
    // defer self.cunit = curr_cunit;
    // self.cunit = cunit;
    self.ctx.enterModule(typ);
    defer self.ctx.leaveModule();
    self.flowInferEntry(cunit.program.entry()) catch {};
    self.analyzer.analyzeDeadCodeWithTypes(cunit.program.entry()) catch {};
    typ.module().setAsResolved();
  }

  inline fn flowInferNextNeighbours(self: *Self, flo: FlowNode, infer_next: bool) !void {
    for (flo.getNextNeighbours().items()) |item| {
      try self.flowInfer(item.node, infer_next);
    }
  }

  fn flowInferEntry(self: *Self, node: FlowNode) !void {
    assert(node.get().tag == .CfgEntry);
    // automatically resolved on entry
    node.get().res = .Resolved;
    // `entry` node, so we don't care about the `node` & `prev` properties
    return self.flowInferNextNeighbours(node, true);
  }

  fn flowInferExit(self: *Self, node: FlowNode) !void {
    _ = self;
    assert(node.get().tag == .CfgExit);
    node.get().res = .Resolved;
  }

  fn flowInferMeta(self: *Self, flo_node: FlowNode, node: *Node) !void {
    if (flo_node.get().res.isResolved()) return;
    if (flo_node.get().tag == .CfgEntry) {
      return try self.flowInferEntry(flo_node);
    }
    if (flo_node.get().tag == .CfgExit) {
      return try self.flowInferExit(flo_node);
    }
    return self.flowInferNode(flo_node, node);
  }

  fn flowInferNode(self: *Self, flo: FlowNode, node: *Node) !void {
    const curr_flo = self.ctx.data.flo_node;
    defer self.ctx.data.flo_node = curr_flo;
    self.ctx.data.flo_node = flo;
    _ = try self.infer(node);
  }

  fn flowInferCondition(self: *Self, flo: FlowNode, node: *Node) !void {
    flo.get().res = .Processing;
    self.ctx.var_scope.pushScope();
    errdefer self.ctx.var_scope.popScope();
    // This is a branch point and a meet point.
    // As a meet point, all types on the incoming edges are merged.
    // As a branch point, types are narrowed along outgoing edges based
    // on the condition expression.
    const curr_tenv = self.tenv;
    defer self.tenv = curr_tenv;
    const cond = node.NdCondition.cond;
    const al = self.al;
    var env = TypeEnv.init(self.never_ty, al, null);
    self.tenv = &env;
    env.global.pushScope();
    _ = self.narrow(cond, &env, true) catch |e| return e;
    if (cond.getType()) |ty| {
      try self.checkCondition(ty, cond);
    } else {
      try self.checkCondition(try self.infer(cond), cond);
    }
    // get all nodes on the true edges & flowInfer with env
    var out_t_nodes = flo.getOutgoingNodes(.ETrue, al);
    self.copyEnv(&env);
    for (out_t_nodes.items()) |itm| {
      try self.flowInfer(itm, false);
    }
    self.ctx.var_scope.popScope();
    self.ctx.var_scope.pushScope();
    // get all nodes on the false edges & flowInfer with not\env
    var out_f_nodes = flo.getOutgoingNodes(.EFalse, al);
    env.clear();
    env.global.pushScope();
    self.diag.skipEntry();
    _ = self.narrow(cond, &env, false) catch {
      env.narrowed.clearRetainingCapacity();
    };
    self.diag.resumeEntry();
    // set this attribute to aid function return type inference
    node.NdCondition.has_never_typ_in_false_path = env.hasNeverTy();
    self.copyEnv(&env);
    for (out_f_nodes.items()) |itm| {
      try self.flowInfer(itm, false);
    }
    flo.get().res = .Resolved;
    // when type checking conditions along the false path, check the first sequential node
    // after the nodes on the false path. If that node has only one incoming edge,
    // then don’t pop the type information gathered on the false path
    if (isEmptyOutgoingNodes(&out_f_nodes)) {
      var edges: usize = 0;
      for (flo.getNextNeighbours().items()) |itm| {
        // get the first sequential node after the nodes on the false path
        if (itm.edge == .ESequential) {
          // track the number of incoming edges from this node
          edges += itm.node.getPrevNeighbours().len();
          break;
        }
      }
      var exits = @as(usize, 0);
      if (edges == 0) {
        // check that the nodes on the true path exits
        for (out_t_nodes.items()) |itm| {
          if (itm.get().bb.getNonScopeLast()) |lst| {
            if (lst.isRet()) {
              exits += 1;
            } else if (lst.getType()) |ty| {
              if (ty.isNeverTy()) {
                exits += 1;
              }
            }
          }
        }
      }
      if (edges == 1 or exits >= 1) {
        // set all type information gathered to the current varScope
        const scope = self.ctx.var_scope.decls.pop();
        var itr = scope.map.iterator();
        while (itr.next()) |entry| {
          self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
        }
        return;
      }
    }
    self.ctx.var_scope.popScope();
  }

  fn flowInferMCondition(self: *Self, flo_node: FlowNode, node: *Node) !void {
    const tst = node.NdMCondition.tst;
    self.matchmeta.tests.append(.{self.getTestFromCondition(tst), flo_node, tst});
    defer _ = self.matchmeta.tests.pop();
    try self.flowInferCondition(flo_node, tst);
  }

  fn inferFail(self: *Self, node: *tir.MarkerNode) !*Type {
    // save and restore diag level on exit
    self.diag.pushLevel(.DiagError);
    defer _ = self.diag.popLevel();
    if (self.matchmeta.tests.isEmpty()) return error.CheckError;
    const nd = self.matchmeta.tests.getLast().@"0";
    var allow_rested = false;
    var ident: *TVarNode = undefined;
    var idx = self.matchmeta.tests.len() - 1;
    // controls how the missing pattern types would be built when using the ident name to discover those types 
    var is_direct_tst = true;
    if (nd.isBinary()) {
      allow_rested = nd.NdBinary.allow_rested;
      ident = self.getIdent(&nd.NdBinary);
    } else if (nd.isTVariable()) {
      ident = &nd.NdTVar;
    } else {
      // get the nearest (enclosing) binary/var test
      is_direct_tst = false;
      var i = idx;
      var id: ?*TVarNode = null;
      while (i > 0): (i -= 1) {
        const tst = self.matchmeta.tests.itemAt(i - 1).@"0";
        if (tst.isBinary()) {
          // don't set allow_rested, because this isn't the direct condition being tested.
          id = self.getIdent(&tst.NdBinary);
          if (!tst.NdBinary.allow_rested) {
            idx = i - 1;
          }
          break;
        } else if (tst.isTVariable()) {
          id = &tst.NdTVar;
          idx = i - 1;
          break;
        }
      }
      if (id) |_id| {
        ident = _id;
      } else {
        self.matchmeta.missing.set("", node.token);
        return self.void_ty;
      }
    }
    var ty = try self.lookupName(ident, false);
    if (!ty.isNeverTy() and !allow_rested) {
      // It is possible that this Fail node is from an enclosing `rested` constructor, because the
      // match compiler tries to produce an optimal decision tree without repeated constructor tests.
      // Hence, we look up the test 'stack', if we find any test with an allow_rested property,
      // then the test is exhaustive, but the match arms were (originally) ordered poorly
      // Get the nearest enclosing condition
      const tst = self.matchmeta.tests.itemAt(idx).@"0";
      allow_rested = tst.isBinary() and tst.NdBinary.allow_rested;
      if (!allow_rested) {
        // Finally, before generating errors, check if this branch is unreachable, if so, just return void
        if (idx > 0 and self.matchmeta.tests.len() > 1) {
          const tst_cond = self.matchmeta.tests.itemAt(idx - 1).@"2";
          if (tst_cond.NdCondition.has_never_typ_in_false_path) {
            const outer_tst = self.matchmeta.tests.itemAt(idx - 1).@"0";
            if (outer_tst.isBinary() and outer_tst.NdBinary.left.isTVariable() and outer_tst.NdBinary.op_tkty.is(.TkIs)) {
              return self.void_ty;
            }
          }
        }
        self.matchmeta.missing.set(self.getTypename(self.buildMissingPatternTypes(ty, ident, is_direct_tst)), ident.token);
        return self.void_ty;
      }
    }
    return self.void_ty;
  }

  fn inferRedundant(self: *Self, node: *tir.MarkerNode) !*Type {
    if (self.diag.hasErrors()) {
      return self.void_ty;
    }
    if (self.tenv) |env| {
      self.checkTestRedundancy(env, node);
    }
    return self.void_ty;
  }

  //*** match type checks ***//

  inline fn compressTypes(self: *@This(), typeset: *TypeHashSet) !*Type {
    return if (typeset.count() < 1) error.CheckError else Type.compressTypes(typeset, null, self.al);
  }

  inline fn compressTaggedTypes(self: *@This(), list: *TypeList) !*Type {
    return if (list.len() < 1) error.CheckError else Type.compressTaggedTypes(list, null, self.al);
  }

  inline fn copyType(self: *Self, ty: *Type) *Type {
    var nty = self.ctx.copyType(ty);
    nty.tid = 0;
    return nty;
  }

  fn selectType(self: *Self, haystack: *Type, needle: *Type, token: Token) !*Type {
    if (haystack.is(needle, self.al)) |ty| {
      return ty;
    } else {
      return self.error_(
        true, token, "type '{s}' is not related to type '{s}'", .{
          self.getTypename(haystack), self.getTypename(needle)
        }
      );
    }
  }

  inline fn getClassType(self: *Self, haystack: *Type, needle: *Type, token: Token) !*Type {
    return (try self.selectType(haystack, needle, token)).classOrInstanceClass();
  }

  // infer the type of this pattern
  fn inferPatternx(self: *Self, pat: *Pattern, conses: *ConsList, expr_ty: *Type, al: Allocator) !*Type {
    switch (pat.variant.*) {
      .cons => |*cons| {
        switch (cons.tag) {
          .List => {
            var tmp = Type.newBuiltinGenericClass(cons.name, .TkList, al);
            var ty = try self.getClassType(expr_ty, tmp, pat.token);
            if (cons.args.len > 0) {
              var param_ty = ty.klass().getSlice()[0];
              for (cons.args) |arg| {
                _ = try self.inferPatternx(arg, conses, param_ty, al);
              }
            }
            if (cons.rested and cons.node != null) {
              self.insertVar(cons.node.?.NdTVar.value(), ty);
            }
            return ty;
          },
          .Tuple => {
            var tmp = Type.newBuiltinGenericClass(cons.name, .TkTuple, al);
            var ty = try self.getClassType(expr_ty, tmp, pat.token);
            if (cons.args.len > 0) {
              if (ty.klass().tparamsLen() == cons.args.len or cons.rested and ty.klass().tparamsLen() >= cons.args.len) {
                var typs = ty.klass().getSlice()[0..cons.args.len];
                for (cons.args, typs) |arg, _ty| {
                  _ = try self.inferPatternx(arg, conses, _ty, al);
                }
              } else {
                return self.error_(true, pat.token, "Expected type '{s}'", .{self.getTypename(ty)});
              }
            }
            return ty;
          },
          .Map => {
            var tmp = Type.newBuiltinGenericClass(ks.MapVar, .TkMap, al);
            var ty = try self.getClassType(expr_ty, tmp, pat.token);
            var list = &cons.args[0].variant.cons;
            const typs = ty.klass().getSlice();
            var key_ty = typs[0];
            var val_ty = typs[1];
            if (list.args.len > 0) {
              var i = @as(usize, 0);
              while (i < list.args.len): (i += 2) {
                _ = try self.inferPatternx(list.args[i], conses, key_ty, al);
                _ = try self.inferPatternx(list.args[i + 1], conses, val_ty, al);
              }
            }
            var uni = Type.newUnion().box(al);
            uni.union_().addSlice(typs, al);
            var list_ty = Type.newBuiltinGenericClass(ks.ListVar, .TkList, al);
            list_ty.klass().appendTParam(uni, al);
            return ty;
          },
          .Literal => {
            var ty = try self.infer(cons.node.?);
            if (!expr_ty.isRelatedTo(ty, .RCAny, al)) {
              return self.error_(
                true, pat.token, "Expected type '{s}' but found '{s}'",
                .{self.getTypename(expr_ty), self.getTypename(ty)}
              );
            }
            return ty;
          },
          .Or => {
            // Or is a synthetic constructor
            var typeset = TypeHashSet.init();
            typeset.ensureTotalCapacity(cons.args.len, al);
            for (cons.args) |arg| {
              var typ = try self.inferPatternx(arg, conses, expr_ty, al);
              typeset.setAssumeCapacity(typ.typeid(), typ);
            }
            return try self.compressTypes(&typeset);
          },
          .Other => {
            // Other represents a user defined constructor
            var typ = try self.lookupName(&cons.node.?.NdTVar, true);
            if (typ.isClass()) {
              const token = cons.node.?.getToken();
              var ty = self.copyType(try self.getClassType(expr_ty, typ, token));
              try self.resolveType(ty, token, al);
              const tyname = self.getTypename(ty);
              const info = if (cons.rested) " or more" else "";
              const len = cons.args.len;
              const cls = ty.klass();
              if (len > cls.data.fields.len()) {
                return self.error_(
                  true, token, "type '{s}' has {} field(s), but pattern test assumes {}{s}",
                  .{tyname, cls.data.fields.len(), len, info}
                );
              }
              if (cons.rested) {
                const wc_token = token.tkFrom(ks.UnderscoreVar, .TkIdent);
                var fields_left = cls.data.fields.len() - len;
                if (fields_left == 1) {
                  cons.append(
                    Pattern.init(ptn.Wildcard.init(wc_token, true).toVariant(al), token, .{}).box(al),
                    al
                  );
                } else {
                  var args = ptn.Patterns.initCapacity(cons.args.len + fields_left, al);
                  args.appendSliceAssumeCapacity(cons.args);
                  while (fields_left > 0): (fields_left -= 1) {
                    args.appendAssumeCapacity(
                      Pattern.init(ptn.Wildcard.init(wc_token, true).toVariant(al), token, .{}).box(al)
                    );
                  }
                  cons.args = args.items();
                }
              }
              if (!cons.rested) {
                try self.checkArity(cons.args.len, cls.data.fields.len(), token,
                  "type '{s}' has {} field(s), but pattern test assumes {}{s}",
                  .{tyname, cls.data.fields.len(), len, info}
                );
              }
              if (cons.hasField()) {
                for (cons.args) |arg| {
                  var checked = false;
                  for (cls.data.fields.items()) |field| {
                    if (arg.alat.hasField() and arg.alat.field.?.NdTVar.valueEql(field.getFieldLexeme())) {
                      checked = true;
                      break;
                    }
                  }
                  if (!checked and arg.alat.hasField()) {
                    const tk = arg.alat.field.?.getToken();
                    self.softError(tk, "type '{s}' has no field '{s}'", .{tyname, tk.lexeme()});
                  }
                }
                // try to sort the patterns
                var sorted: ?Token = null;
                for (cls.data.fields.items(), 0..) |field, i| {
                  for (cons.args, 0..) |arg, j| {
                    if (arg.alat.hasField()) {
                      if (arg.alat.field.?.NdTVar.valueEql(field.getFieldLexeme())) {
                        if (i != j) {
                          cons.args[j] = cons.args[i];
                          cons.args[i] = arg;
                          sorted = arg.alat.field.?.getToken();
                        }
                      }
                    }
                  }
                }
                if (sorted) |dbg| {
                  self.softError(
                    dbg, "The field '{s}' was found in a wrong position." ++
                    "\n    Help: Consider rearranging the order of arguments to the type constructor.",
                    .{dbg.lexeme()}
                  );
                }
              }
              for (cons.args, cls.data.fields.items()) |arg, field| {
                var fty = try self.inferClsField(field);
                _ = try self.inferPatternx(arg, conses, fty, al);
              }
              return ty;
            } else if (typ.isTag()) {
              const token = cons.node.?.getToken();
              const tyname = self.getTypename(typ);
              const info = if (cons.rested) " or more" else "";
              const len = cons.args.len;
              const tag = typ.tag();
              if (len > tag.fieldsLen()) {
                return self.error_(
                  true, token, "type '{s}' has {} field(s), but pattern test assumes {}{s}",
                  .{tyname, tag.fieldsLen(), len, info}
                );
              }
              if (cons.rested) {
                const wc_token = token.tkFrom(ks.UnderscoreVar, .TkIdent);
                var fields_left = tag.fieldsLen() - len;
                var args = ptn.Patterns.initCapacity(cons.args.len + fields_left, al);
                args.appendSliceAssumeCapacity(cons.args);
                while (fields_left > 0): (fields_left -= 1) {
                  args.appendAssumeCapacity(
                    Pattern.init(ptn.Wildcard.init(wc_token, true).toVariant(al), token, .{}).box(al)
                  );
                }
                cons.args = args.items();
              }
              if (!cons.rested) {
                try self.checkArity(cons.args.len, tag.fieldsLen(), token,
                  "type '{s}' has {} field(s), but pattern test assumes {}{s}",
                  .{tyname, tag.fieldsLen(), len, info}
                );
              }
              if (cons.hasField()) {
                var sorted: ?Token = null;
                for (cons.args) |arg| {
                  var checked = false;
                  for (tag.fieldSlice()) |prm| {
                    if (arg.alat.hasField() and arg.alat.field.?.NdTVar.valueEql(prm.name.?)) {
                      checked = true;
                      break;
                    }
                  }
                  if (!checked and arg.alat.hasField()) {
                    const tk = arg.alat.field.?.getToken();
                    self.softError(tk, "type '{s}' has no field '{s}'", .{tyname, tk.lexeme()});
                  }
                }
                // try to sort the patterns
                for (tag.fieldSlice(), 0..) |prm, i| {
                  for (cons.args, 0..) |arg, j| {
                    if (arg.alat.hasField()) {
                      if (prm.name) |name| {
                        if (arg.alat.field.?.NdTVar.valueEql(name)) {
                          if (i != j) {
                            cons.args[j] = cons.args[i];
                            cons.args[i] = arg;
                            sorted = arg.alat.field.?.getToken();
                          }
                        }
                      }
                    }
                  }
                }
                if (sorted) |dbg| {
                  self.softError(
                    dbg, "The tag field '{s}' was found in a wrong position." ++
                    "\n    Help: Consider rearranging the order of arguments to the type constructor.",
                    .{dbg.lexeme()}
                  );
                }
              }
              var ty = try self.selectType(expr_ty, typ, token);
              for (cons.args, ty.tag().fieldSlice()) |arg, prm| {
                _ = try self.inferPatternx(arg, conses, prm.typ, al);
              }
              return ty;
            } else {
              self.softErrorFrom(
                cons.node.?,
                "bad/ambiguous pattern constructor: '{s}'",
                .{self.getTypename(typ)}
              );
              return typ;
            }
          },
        }
      },
      .vari => |*vari| {
        self.insertTy(vari.ident.NdTVar.value(), expr_ty);
        return expr_ty;
      },
      .wildc => |*wildc| {
        wildc.typ = expr_ty;
        return expr_ty;
      },
      else => {},
    }
    unreachable;
  }

  fn checkMatchDiagStart(self: *Self, md: *tir.MarkerNode) !*Type {
    const al = self.al;
    {
      defer {
        // if there are errors after type checking the original match node, then
        // the desugared match node will generate a lot of errors, some of which
        // are not useful to a user.
        if (self.diag.hasErrors()) {
          // set diag level to `internal error`, as we do not want to display internal errors
          // on the lowered transformation due to faulty match nodes
          self.diag.pushLevel(.DiagIError);
        }
      }
      var node = &md.payload.?.NdMatch;
      var m_expr_ty = try self.infer(node.expr);
      var expr_ty = m_expr_ty.classOrInstanceClass();
      var conses = ConsList.init(al);
      for (node.cases) |case| {
        // use a new scope for resolving each pattern, so that each variable type
        // doesn't conflict with existing types in ctx.
        self.ctx.enterScope();
        var ty = try self.inferPatternx(case.pattern, &conses, expr_ty, al);
        if (case.pattern.isConstructor()) {
          if (expr_ty.canBeAssigned(ty, .RCAny, al) == null and !case.pattern.isOrConstructor()) {
            self.softError(
              case.pattern.token, "Expected type '{s}' but found '{s}'",
              .{self.getTypename(m_expr_ty), self.getTypename(ty)},
            );
          }
        }
        // pop scope off
        self.ctx.leaveScope();
      }
      if (self.diag.hasErrors()) {
        return error.CheckError;
      }
    }
    return self.void_ty;
  }

  fn checkMatchDiagEnd(self: *Self) !*Type {
    _ = self.diag.popLevel();
    if (self.matchmeta.missing.count() > 0) {
      // some tokens are generated ($) and won't be displayed, so we make a fresh token 
      const token = self.matchmeta.missing.values()[0];
      self.softError(token, "inexhaustive pattern match.", .{});
      var prompt = false;
      for (self.matchmeta.missing.keys()) |msg| {
        if (msg.len == 0) {
          continue;
        }
        if (!prompt) {
          self.diag.addDiagnosticsDirect(token, "  Remaining pattern type(s):\n");
          prompt = true;
        }
        self.diag.addDiagnosticsSliceDirect(token, &[_][]const u8{"    ", msg, "\n"});
      }
      self.diag.addDiagnosticsDirect(token, "\n");
      self.matchmeta.missing.clearRetainingCapacity();
    }
    if (self.matchmeta.warnings.isNotEmpty()) {
      var last_0xfe: ?Token = null;
      var _0xfes = @as(u32, 0);
      var itr = self.matchmeta.warnings.iterator();
      while (itr.next()) |entry| {
        if (entry.value_ptr.* != 0xff) {
          if (entry.value_ptr.* != 0xfe) {
            self.warn(true, entry.key_ptr.*, "possible redundant case", .{});
          } else {
            entry.value_ptr.* = 0xff;
            last_0xfe = entry.key_ptr.*;
            _0xfes += 1;
          }
        }
      }
      // this arises from using 'rested patterns'. only warn when there are more than
      // one 0xfe flag set, this ensures that there is indeed a redundant test case,
      // FIXME: this also produces false +ves for nested match nodes
      if (_0xfes > 1) {
        self.warn(true, last_0xfe.?, "possible redundant case", .{});
      }
    }
    return self.void_ty;
  }

  /// check if a particular test/condition is redundant, given the variable of interest.
  fn checkTestRedundancy(self: *Self, env: *TypeEnv, marker: *tir.MarkerNode) void {
    var ident: *tir.TVarNode = undefined;
    var is_call = false;
    var tst = self.matchmeta.tests.getLast().@"0";
    var allow_rested = false;
    if (tst.isBinary()) {
      ident = self.getIdent(&tst.NdBinary);
      is_call = tst.NdBinary.left.isCall();
      allow_rested = tst.NdBinary.allow_rested;
    } else if (tst.isTVariable()) {
      ident = &tst.NdTVar;
    } else {
      self.matchmeta.warnings.set(marker.token, 0xff);
      return;
    }
    const typ = (
      if (!is_call) (env.getNarrowed(ident.value()) orelse env.getGlobal(ident.value())).?
      else self.findName(ident.value()).?
    );
    if (typ.isNeverTy()) {
      if (self.matchmeta.warnings.get(marker.token) != null) {
        return;
      }
      self.matchmeta.warnings.set(marker.token, 1);
    } else if (is_call and allow_rested and self.isLengthCheck(tst)) {
      // This is a rested test for list types. Set a different flag for this case
      if (tst.NdBinary.left.NdBasicCall.expr.NdDotAccess.lhs.getType()) |ty| {
        assert(ty.isListTy());
        if (tst.NdBinary.right.isNumberLiteral() and tst.NdBinary.right.NdNumber.value == 0) {
          self.matchmeta.warnings.set(marker.token, 0xfe);
        }
      }
    } else {
      self.matchmeta.warnings.set(marker.token, 0xff);
    }
  }

  inline fn resolveNode(self: *Self, flo: FlowNode, node: *Node) !void {
    switch (node.*) {
      .NdCondition => try self.flowInferCondition(flo, node),
      .NdMCondition => try self.flowInferMCondition(flo, node),
      .NdEmpty => try self.flowInferMeta(flo, node),
      .NdControl => {},
      .NdBasicFun => |*fun| {
        // the entry flow-node sometimes stores a FunNode in its basic block (for future ref).
        // so we check if this flow-node is actually an entry node, or just a regular flow-node
        if (flo.isEntryNode()) {
          try self.flowInferEntry(flo);
        } else if (flo.get().res.isUnresolved()) {
          // Some functions may be mutually recursive, or may depend on some other
          // functions not yet known at the time of inference of this current one.
          // So only partially infer this function. Full inference is done by need.
          if (!fun.isAnonymous()) {
            _ = try self.inferFunPartial(node);
          } else {
            _ = try self.inferFun(node, null);
          }
        }
      },
      .NdGenericFun => {
        if (flo.isEntryNode()) {
          try self.flowInferEntry(flo);
        } else if (flo.get().res.isUnresolved()) {
          _ = try self.inferFunPartial(node);
        }
      },
      .NdClass => {
        if (flo.get().res.isUnresolved()) {
          _ = try self.inferClassPartial(node);
        }
      },
      .NdTrait => {
        if (flo.get().res.isUnresolved()) {
          _ = try self.inferTraitPartial(node);
        }
      },
      .NdImport => |*nd| {
        // create a module type, save the type with its alias or last file name.
        var mod_ty = try self.buildImpProgramFlow(nd, true);
        self.ctx.cm().addModule(mod_ty, self.al);
        nd.typ = mod_ty;
        // handle entities
        if (nd.data.entities) |entities| {
          try self.resolveModuleType(mod_ty);
          for (entities) |entity| {
            const id = entity.getName().lexeme();
            const info = try self.checkModuleAccess(mod_ty, entity.name, id);
            const new_ty = info.@"0";
            const aspec = AccessSpecifier.getASpec(entity.public);
            // this is an id property
            if (info.@"1") {
              self.insertVar(id, new_ty);
              self.ctx.cm().setIdTy(id, new_ty, aspec, self.al);
            } else {
              self.insertTy(id, new_ty);
              self.ctx.cm().setTy(id, new_ty, aspec, self.al);
            }
          }
        } else {
          const id = nd.getImportName();
          mod_ty.module().env.alias = id;
          self.insertVar(id, mod_ty);
          self.insertTy(id, mod_ty);
          self.ctx.cm().setIdTy(id, mod_ty, AccessSpecifier.getASpec(nd.data.public), self.al);
        }
      },
      else => try self.flowInferNode(flo, node),
    }
  }

  inline fn resolveBB(self: *Self, flo: FlowNode) void {
    if (flo.get().res.isResolved()) return;
    var bb = &flo.get().bb;
    if (bb.et_scope) {
      self.ctx.enterScope();
    }
    for (bb.items()) |ast_node| {
      self.resolveNode(flo, ast_node) catch {};
    }
    if (bb.ex_mdiag) {
      _ = self.checkMatchDiagEnd() catch undefined;
    }
    if (bb.ex_scope) {
      self.ctx.leaveScope();
    }
    flo.get().res = .Resolved;
  }

  fn flowInfer(self: *Self, flo: FlowNode, infer_next: bool) TypeCheckError!void {
    for (flo.getPrevNeighbours().items()) |item| {
      // we can only proceed to resolve this node when all
      // incoming edges have been resolved or when we're resolving
      // all nodes on the true edges of a Condition.
      if (item.node.get().res.isUnresolved()) {
        return;
      }
    }
    self.resolveBB(flo);
    if (!infer_next) return;
    return self.flowInferNextNeighbours(flo, infer_next);
  }

  const PassesFn = fn (itm: *const fir.Neighbour) callconv(.Inline) bool;

  inline fn getNextFlowNodes(self: *Self, node: FlowNode) FlowList {
    const items = node.getNextNeighbours().items();
    var nodes = FlowList.initCapacity(items.len, self.al);
    for (items) |item| {
      nodes.appendAssumeCapacity(item.node);
    }
    return nodes;
  }

  //***********************************************************//
  //***********  inference  ***********************************//
  //***********************************************************//

  fn isCurrentlyResolving(self: *Self, typ: *Type) bool {
    m:
    for (self.resolving.items()) |ty| {
      if (ty == typ) {
        return true;
      }
      if ((ty.isClass() and typ.isClass()) or (ty.isTrait() and typ.isTrait())) {
        var cls1 = ty.klassOrTrait();
        var cls2 = typ.klassOrTrait();
        if (cls1.eql(cls2)) {
          const tparams1 = cls1.getSlice();
          const tparams2 = cls2.getSlice();
          if (tparams1.len == tparams2.len) {
            for (tparams1, tparams2) |t1, t2| {
              if (!t1.isRelatedTo(t2, .RCAny, self.al)) {
                if (typ.isTrait()) {
                  return true;
                }
                continue :m;
              }
            }
            typ.* = ty.*;
            return true;
          }
        }
      }
    }
    return false;
  }

  fn resolveBuiltinClassType(self: *Self, typ: *Type, debug: Token) TypeCheckError!void {
    if (self.findBuiltinType(typ.klass().data.name)) |ty| {
      typ.* = (try self.synthInferClsType(typ, ty, debug)).*;
      typ.klass().setAsResolved();
    }
  }

  fn resolveUserClassType(self: *Self, typ: *Type, debug: Token) TypeCheckError!void {
    if (self.findCoreType(typ.klass().data.name)) |_ty| {
      assert(!_ty.isTop());
      typ.* = (try self.synthInferClsType(typ, _ty, debug)).*;
      typ.classOrInstanceClass().klass().setAsResolved();
    }
  }

  fn resolveClassType(self: *Self, typ: *Type, debug: Token) TypeCheckError!void {
    if (typ.isClass()) {
      if (typ.klass().resolved or self.isCurrentlyResolving(typ)) {
        return;
      } else if (typ.klass().builtin) {
        return try self.resolveBuiltinClassType(typ, debug);
      } else {
        return try self.resolveUserClassType(typ, debug);
      }
    }
  }

  fn resolveTrait(self: *Self, trait: *Type, debug: Token) !*Type {
    if (trait.isUnion()) {
      var new = Type.newUnion();
      for (trait.union_().variants.values()) |_ty| {
        new.union_().set(try self.resolveTrait(_ty, debug), self.al);
      }
      return new.union_().toTypeBoxed(self.al);
    }
    if (trait.isVariable()) {
      return try self.resolveTrait(try self.linker.resolve(trait, trait.variable().token()), debug);
    }
    if (trait.isGeneric() or trait.isDot()) {
      return try self.linker.resolve(trait, debug);
    }
    if (trait.isTrait()) {
      if (trait.trait().resolved) {
        return trait;
      }
      if (trait.trait().data.trait) |trt| {
        trait.trait().data.trait = try self.resolveTrait(trt, debug);
      }
    }
    return trait;
  }

  fn resolveUserTraitType(self: *Self, typ: *Type, debug: Token) TypeCheckError!void {
    if (self.findCoreType(typ.trait().data.name)) |_ty| {
      assert(!_ty.isTop());
      typ.* = (try self.synthInferTraitType(typ, _ty, debug)).*;
      typ.trait().setAsResolved();
    }
  }

  fn resolveTraitType(self: *Self, typ: *Type, debug: Token, al: Allocator) !*Type {
    if (typ.isTrait()) {
      if (typ.trait().resolved or self.isCurrentlyResolving(typ)) {
        return typ;
      }
    }
    var ty = try self.resolveTrait(typ, debug);
    if (ty.isTrait()) {
      if (!ty.trait().resolved and !self.isCurrentlyResolving(ty)) {
        try self.resolveUserTraitType(ty, debug);
      }
    } else if (ty.isUnion()) {
      try self.resolveType(ty, debug, al);
    }
    return ty;
  }

  pub fn resolveModuleType(self: *Self, mod_ty: *Type) !void {
    if (!mod_ty.module().resolved) {
      if (!self.resolving.contains(mod_ty, Type.ptrEql)) {
        self.ctx.enterTVScope();
        defer self.ctx.leaveTVScope();
        const info = self.modules.get(mod_ty.module().name()).?;
        self.resolving.append(mod_ty, self.al);
        defer _ = self.resolving.pop();
        self.flowInferProgram(info.cunit, mod_ty);
      }
    }
  }

  fn resolveType(self: *Self, typ: *Type, debug: Token, al: Allocator) TypeCheckError!void {
    return switch (typ.info) {
      .Class => self.resolveClassType(typ, debug),
      .Trait => _ = try self.resolveTraitType(typ, debug, al),
      .Union => |*uni| {
        var tset = TypeHashSet.init();
        for (uni.variants.values()) |ty| {
          try self.resolveType(ty, debug, al);
          tset.set(ty.typeid(), ty, al);
        }
        typ.* = Type.compressTypes(&tset, typ, al).*;
      },
      .Constant, .Concrete, .Variable, .Recursive,
      .Function, .Top, .Tag, .TaggedUnion,
      .Instance, .TagOrClass, .Module, .Dot => return,
      else => unreachable,
    };
  }

  fn synthInferClsType(self: *Self, user_ty: *Type, core_ty: *Type, debug: Token) !*Type {
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    var core_cls = core_ty.klass();
    // generic
    if (core_cls.isParameterized()) {
      // FIXME: this check is a hack since resolving a CastNode's type after linking
      // (sometimes) breaks in the presence of recursive types.
      if (user_ty.hasRecursive()) return user_ty;
      var user_cls = user_ty.klass();
      if (!user_cls.isParameterized()) {
        return self.error_(
          true, debug, "type '{s}' is generic but used without type parameters",
          .{self.getTypename(user_ty)}
        );
      }
      // if any type params in core_cls is already substituted, just return user_ty because
      // it means user_ty is already undergoing monomorphization from an earlier call
      if (!core_cls.getSlice()[0].isVariable()) {
        return user_ty;
      }
      const ctparams = core_cls.tparams.?;
      const tparams = user_cls.tparams.?;
      const synth_name = self.makeSynthName(core_cls.data.name, core_cls.builtin, tparams, null, null);
      const old_cls = core_cls.data.node.?;
      if (self.findGenInfo(old_cls, synth_name)) |info| {
        return info.typ;
      }
      if (!core_ty.isTupleTy()) {
        // typelinker & parser ensures that user_cls' tparams and core_cls' tparams are equal in size
        for (ctparams, tparams) |tvar, ty| {
          self.insertType(tvar, ty);
        }
      } else {
        // use union type to represent the multiple types of the tuple
        var uni = Type.newUnion().box(self.al);
        uni.union_().addSlice(tparams, self.al);
        self.insertType(ctparams[0], uni);
      }
      var new_node = old_cls.clone(self.al);
      self.ctx.cm().setIdTy(new_node.NdClass.name.lexeme(), user_ty, user_ty.aspec, self.al);
      // we only link when we're sure core_cls is an unsubstituted generic type
      try self.linker.linkClass(&new_node.NdClass, debug, true);
      // make class non-generic to allow inference
      new_node.NdClass.data.params = null;
      user_cls.data.node = new_node;
      self.addGenInfo(
        old_cls,
        new_node,
        boxSynthName(old_cls.getToken(), synth_name),
        user_ty,
      );
      return try self.inferClass(new_node, user_ty, ctparams, tparams);
    }
    // non-generic
    if (core_cls.data.node) |cls_node| {
      if (!self.cycles.contains(cls_node, Node.eql) and !cls_node.NdClass.data.checked) {
        return try self.inferClass(cls_node, core_ty, &[_]*Type{}, &[_]*Type{});
      }
    }
    return core_ty;
  }

  fn synthInferTraitType(self: *Self, user_ty: *Type, core_ty: *Type, debug: Token) !*Type {
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    var core_trt = core_ty.trait();
    // generic
    if (core_trt.isParameterized()) {
      // FIXME: this is a hack. see synthInferClsType()
      if (user_ty.hasRecursive()) return user_ty;
      var user_trt = user_ty.trait();
      if (!user_trt.isParameterized()) {
        return self.error_(
          true, debug, "type '{s}' is generic but used without type parameters",
          .{self.getTypename(user_ty)}
        );
      }
      // if any type params in core_trt is already substituted, just return user_ty because
      // it means user_ty is already undergoing monomorphization from an earlier call
      if (!core_trt.getSlice()[0].isVariable()) {
        return user_ty;
      }
      const ctparams = core_trt.tparams.?;
      const tparams = user_trt.tparams.?;
      // resolve the provided type params (user_ty's)
      var call: Node = .{.NdGenericCall = tir.GenericCallNode.init(undefined, tparams)};
      try self.linkAndCheckCallTypeParams(&call, debug);
      const synth_name = self.makeSynthName(core_trt.data.name, core_trt.builtin, tparams, null, null);
      const old_trt = core_trt.data.node.?;
      if (self.findGenInfo(old_trt, synth_name)) |info| {
        return info.typ;
      }
      // insert tvar substitutions into scope before resolving for traits.
      for (ctparams, tparams) |tvar, t| {
        self.insertType(tvar, t);
      }
      for (ctparams, tparams) |tvar, t| {
        if (try self.checkForTraitBounds(tvar, t, debug)) |_bty| {
          self.insertType(tvar, _bty);
        }
      }
      var new_node = old_trt.clone(self.al);
      // we only link when we're sure core_trt is an unsubstituted generic type
      try self.linker.linkTrait(&new_node.NdTrait, true);
      // make trait non-generic to allow inference
      new_node.NdTrait.data.params = null;
      self.addGenInfo(
        old_trt,
        new_node,
        boxSynthName(old_trt.getToken(), synth_name),
        user_ty,
      );
      return try self.inferTrait(new_node, user_ty, ctparams, tparams);
    }
    // non-generic
    if (core_trt.data.node) |trt_node| {
      if (!self.cycles.contains(trt_node, Node.eql) and !trt_node.NdTrait.data.checked) {
        return try self.inferTrait(trt_node, core_ty, &[_]*Type{}, &[_]*Type{});
      } else if (user_ty.trait().data.methods.len() != core_ty.trait().data.methods.len()) {
        logger.debug("unequal user_ty and core_ty method sizes", .{});
      }
    }
    return core_ty;
  }

  /// Given a type-variable, a field/parameter type, and an inferred expr type
  /// infer/resolve the type of the type-variable
  fn inferTypex(self: *Self, tk: Token, tvar: *Type, fparam: *Type, texpr: *Type) !*Type {
    if (texpr.isRecursive()) {
      return fparam;
    }
    switch (fparam.info) {
      .Class => |*cls| {
        var ty = (try self.getClassType(texpr, fparam, tk));
        var cls2 = ty.klass();
        try self.checkArity(cls.tparamsLen(), cls2.tparamsLen(), tk,
          "type '{s}' has {} field(s), but inferred type assumes {}",
          .{self.getTypename(fparam), cls.tparamsLen(), cls2.tparamsLen()}
        );
        for (cls.getSlice(), cls2.getSlice()) |tp1, tp2| {
          _ = try self.inferTypex(tk, tvar, tp1, tp2);
        }
        return ty;
      },
      .Trait => |*trt| {
        var ty = (try self.getClassType(texpr, fparam, tk));
        var trt2 = ty.trait();
        try self.checkArity(trt.tparamsLen(), trt2.tparamsLen(), tk,
          "type '{s}' has {} field(s), but inferred type assumes {}",
          .{self.getTypename(fparam), trt.tparamsLen(), trt.tparamsLen()}
        );
        for (trt.getSlice(), trt2.getSlice()) |tp1, tp2| {
          _ = try self.inferTypex(tk, tvar, tp1, tp2);
        }
        return ty;
      },
      .Tag => |*tg| {
        var ty = try self.selectType(texpr, fparam, tk);
        var tg2 = ty.tag();
        try self.checkArity(tg.fieldsLen(), tg2.fieldsLen(), tk,
          "type '{s}' has {} field(s), but inferred type assumes {}",
          .{self.getTypename(fparam), tg.fieldsLen(), tg2.fieldsLen()}
        );
        for (tg.fieldSlice(), tg2.fieldSlice()) |fd1, fd2| {
          _ = try self.inferTypex(tk, tvar, fd1.typ, fd2.typ);
        }
        return ty;
      },
      .Generic => |*gen| {
        const name = gen.base.variable().token();
        var typ = self.copyType(try self.lookupName(name, true));
        if (!typ.isParameterized()) {
          return self.inferTypex(tk, tvar, typ, texpr);
        }
        if (typ.isClass()) {
          var cls = typ.klass();
          try self.checkArity(gen.tparamsLen(), cls.tparamsLen(), tk,
            "type '{s}' has {} field(s), but inferred type assumes {}",
            .{self.getTypename(fparam), gen.tparamsLen(), cls.tparamsLen()}
          );
          const ty = (try self.getClassType(texpr, typ, tk));
          var slice = cls.getSlice();
          for (gen.getSlice(), 0..) |tv, i| {
            slice[i] = tv;
          }
          return try self.inferTypex(tk, tvar, typ, ty);
        } else if (typ.isTag()) {
          var tg = typ.tag();
          try self.checkArity(gen.tparamsLen(), tg.fieldsLen(), tk,
            "type '{s}' has {} field(s), but inferred type assumes {}",
            .{self.getTypename(fparam), gen.tparamsLen(), tg.fieldsLen()}
          );
          const ty = (try self.selectType(texpr, typ, tk));
          var slice = tg.fieldSlice();
          for (gen.getSlice(), 0..) |tv, i| {
            slice[i].typ = tv;
          }
          return try self.inferTypex(tk, tvar, typ, ty);
        } else if (typ.isTrait()) {
          var trt = typ.trait();
          try self.checkArity(gen.tparamsLen(), trt.tparamsLen(), tk,
            "type '{s}' has {} field(s), but inferred type assumes {}",
            .{self.getTypename(fparam), gen.tparamsLen(), trt.tparamsLen()}
          );
          self.diag.skipEntry();
          const ty = self.getClassType(texpr, typ, tk) catch blk: {
            self.diag.resumeEntry();
            try self.resolveType(texpr, tk, self.al);
            break :blk try self.getClassType(texpr, typ, tk);
          };
          self.diag.resumeEntry(); // not a mistake, if the catch branch isn't taken, this is set.
          var slice = trt.getSlice();
          for (gen.getSlice(), 0..) |tv, i| {
            slice[i] = tv;
          }
          return try self.inferTypex(tk, tvar, typ, ty);
        }
      },
      .TaggedUnion => |*uni| {
        var tps = TypeList.init();
        for (uni.variants.items()) |typ| {
          tps.append(try self.inferTypex(tk, tvar, typ, texpr), self.al);
        }
        return self.compressTaggedTypes(&tps);
      },
      .Function => |*fun| {
        // fn{T}(T):T
        if (texpr.isFunction()) {
          var fun2 = texpr.function();
          try self.checkArity(fun.tparamsLen(), fun2.tparamsLen(), tk,
            "type '{s}' has {} type parameter(s), but inferred type assumes {}",
            .{self.getTypename(fparam), fun.tparamsLen(), fun2.tparamsLen()}
          );
          if (fun.tparams != null) {
            for (fun.tparams.?, fun2.tparams.?) |tp1, tp2| {
              _ = try self.inferTypex(tk, tvar, tp1, tp2);
            }
          }
          try self.checkArity(fun.data.params.len, fun2.data.params.len, tk,
            "type '{s}' has {} parameter(s), but inferred type assumes {}",
            .{self.getTypename(fparam), fun.tparamsLen(), fun2.tparamsLen()}
          );
          for (fun.data.params, fun2.data.params) |a, b| {
            _ = try self.inferTypex(tk, tvar, a,b);
          }
          _ = try self.inferTypex(tk, tvar, fun.data.ret, fun2.data.ret);
          return fparam;
        }
      },
      .Variable => {
          self.insertType(tvar, texpr);
          return texpr;
      },
      else => { // Concrete, Constant, Union
        if (fparam.typeidEql(texpr)) {
          return texpr;
        }
      }
    }
    return self.error_(
      true, tk, "type '{s}' is not compatible with type '{s}'",
      .{self.getTypename(fparam), self.getTypename(texpr)}
    );
  }

  inline fn isCheckedFun(self: *Self, node: *Node) bool {
    _ = self;
    return node.NdBasicFun.data.body.block().checked;
  }

  inline fn isCheckedClass(self: *Self, node: *Node) bool {
    _ = self;
    return node.NdClass.data.checked;
  }

  inline fn isCheckedTrait(self: *Self, node: *Node) bool {
    _ = self;
    return node.NdTrait.data.checked;
  }

  fn inferVar(self: *Self, node: anytype, emit: bool) !*Type {
    var res = try self.lookupNameWithInfo(node, emit);
    if (!res.ident) {
      // TODO: need to cover more cases
      if (res.typ.isVariable()) {
        res.typ = try self.linker.resolve(res.typ, node.token);
      }
    }
    if (self.ctx.data.parent) |parent| {
      // type vars are only accessible to call & dot access nodes
      if (!parent.isCall() and !parent.isDotAccess()) {
        if (!res.ident) {
          return self.error_(
            true, node.token, "Could not resolve type of ident: '{s}'", .{node.token.lexeme()}
          );
        }
      }
    } else if (!res.ident) {
      if (!res.typ.isTag() or res.typ.tag().fieldsLen() != 0) {
        self.typeReferenceError(node.token, res.typ);
      }
    }
    var typ = res.typ;
    // since we infer functions by need, we check if this is a reference
    // to a function and try to infer the function if it's not yet fully inferred
    if (typ.isFunction() and !typ.function().isParameterized()) {
      if (typ.function().data.node) |nd| {
        if (!self.isCheckedFun(nd) and !self.cycles.contains(nd, Node.eql)) {
          _ = try self.inferFun(nd, typ);
        }
      }
    } else if (typ.isClass() and !typ.klass().isParameterized()) {
      if (typ.klass().data.node) |nd| {
        if (!self.isCheckedClass(nd) and !self.cycles.contains(nd, Node.eql)) {
          _ = try self.inferClass(nd, typ, &[_]*Type{}, &[_]*Type{});
        }
      }
    } else if (typ.isTrait() and !typ.trait().isParameterized()) {
      if (typ.trait().data.node) |nd| {
        if (!self.isCheckedTrait(nd) and !self.cycles.contains(nd, Node.eql)) {
          _ = try self.inferTrait(nd, typ, &[_]*Type{}, &[_]*Type{});
        }
      }
    }
    node.typ = typ;
    return typ;
  }

  fn inferLiteral(self: *Self, kind: tir.Concrete, val: ?[]const u8, node: anytype) *Type {
    const typ = kind.toType().box(self.al);
    typ.info.Concrete.val = val;
    node.typ = typ;
    return typ;
  }

  fn inferUnary(self: *Self, node: *tir.UnaryNode) !*Type {
    var typ = try self.infer(node.expr);
    // unary op: ~, !, -, +
    if (node.op.optype() == .OpBitInvert or node.op.optype() == .OpAdd or node.op.optype() == .OpSub) {
      try self.checkUnary(node, &UnitTypes.tyNumber, typ);
    } else {
      assert(node.op.optype() == .OpNot);
      // `!` accepts any type and returns a boolean.
      // It applies an implicit bool cast to such a type.
      typ = UnitTypes.bol.toType().box(self.al);
    }
    node.typ = typ;
    return typ;
  }

  fn inferBinary(self: *Self, node: *tir.BinaryNode) !*Type {
    if (node.optype() == .OpIs) {
      return self.inferIs(node);
    }
    const lhsTy = try self.infer(node.left);
    const rhsTy = try self.infer(node.right);
    try self.checkBinary(node, lhsTy, rhsTy, false);
    const ty = if (node.optype().isCmpOp()) UnitTypes.bol.toType().box(self.al) else lhsTy;
    node.typ = ty;
    return ty;
  }

  fn inferIs(self: *Self, node: *tir.BinaryNode) !*Type {
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    var ty = getTypeNodeType(node.right);
    const err_token = Token.fromBinaryNode(node);
    // lhs must not be type Type, and rhs must be type Type
    if (lhsTy.isTop()) {
      return self.error_(
        true, err_token,
        "Expected type instance in lhs of `is` operator but found '{s}'",
        .{self.getTypename(lhsTy)}
      );
    } else if (!rhsTy.isTop() and !rhsTy.isClass() or (ty.isLikeConstant() and !node.allow_consts)) {
      const help = (
        if (ty.isLikeConstant())
          "\n    Help: For constant types, consider using '==' or '!=' operator instead."
        else ""
      );
      return self.error_(
        true, err_token,
        "Expected type 'Type' in rhs of `is` operator but found type '{s}'{s}",
        .{self.getTypename(ty), help}
      );
    }
    // at this point, rhs is a TypeNode
    if (ty.isUnion() or ty.isVariable() or ty.isFunction()) {
      return self.error_(
        true, err_token,
        "Expected a concrete or class type in the rhs of the `is` operator but found '{s}'",
        .{self.getTypename(ty)}
      );
    }
    // use the actual type on rhs for checks
    try self.checkBinary(node, lhsTy, ty, true);
    if (!node.right.NdType.skip_type_resolution) {
      try self.resolveType(ty, node.right.getToken(), self.al);
    }
    // `is` returns type bool, so reassign
    const typ = UnitTypes.bol.toType().box(self.al);
    node.typ = typ;
    return typ;
  }

  fn inferExprStmt(self: *Self, node: *tir.ExprStmtNode) !*Type {
    return self.infer(node.expr);
  }

  fn inferParam(self: *Self, node: *tir.ParamNode, ast_n: *Node) !*Type {
    _ = ast_n;
    try self.linker.linkParam(node);
    self.insertVar(node.name.lexeme(), node.typ);
    return node.typ;
  }

  fn inferField(self: *Self, node: *tir.FieldNode) !*Type {
    try self.linker.linkField(node);
    var ret: *Type = undefined;
    if (node.value) |val| {
      if (node.typ) |typ| {
        ret = typ;
        _ = try self.checkInitAssign(typ, try self.infer(val), node.name.toToken(), true);
      } else {
        ret = try self.infer(val);
        node.typ = ret;
      }
    } else {
      ret = node.typ.?;
    }
    ret.aspec = .SpecPrivate;
    return ret;
  }

  fn inferPubField(self: *Self, node: *tir.PubFieldNode) !*Type {
    try self.linker.linkPubField(node);
    var ret: *Type = undefined;
    if (node.value) |val| {
      if (node.typ) |typ| {
        ret = typ;
        _ = try self.checkInitAssign(typ, try self.infer(val), node.name.toToken(), true);
      } else {
        ret = try self.infer(val);
        node.typ = ret;
      }
    } else {
      ret = node.typ.?;
    }
    ret.aspec = .SpecPublic;
    return ret;
  }

  inline fn inferClsField(self: *Self, node: *Node) !*Type {
    return if (node.isField()) self.inferField(&node.NdField) else self.inferPubField(&node.NdPubField);
  }

  fn inferVarDecl(self: *Self, node: *tir.VarDeclNode) !*Type {
    if (!node.value.isEmpty()) {
      // type check as expected
      // set narrowing for this decl when narrowed types are available
      const should_narrow = if (self.tenv) |env| env.narrowed.count() > 0 else false;
      if (should_narrow) self.narrowing += 1;
      defer { if (should_narrow) self.narrowing -= 1; }
      // get expr type
      try self.linker.linkVarDecl(node);
      var ret: *Type = undefined;
      if (node.typ) |typ| {
        ret = try self.checkInitAssign(typ, try self.infer(node.value), node.name.toToken(), true);
      } else {
        ret = try self.infer(node.value);
        node.typ = ret;
      }
      self.insertVar(node.name.lexeme(), ret);
      return ret;
    } else {
      // we only get an Empty node from a desugared OrElse/Match expr.
      // if from a match expr, insert type `<>` for var
      if (node.value.NdEmpty.token.lexeme().len == 0) {
        self.insertVar(node.name.lexeme(), self.tvar_ty);
        return self.tvar_ty;
      }
      // if from an OrElse expr, lookup the potential Result type and select Ok.
      const ty = try self.lookupName(node.value.NdEmpty.token, true);
      const debug = node.value.NdEmpty.token;
      if (!ty.isErrorTaggedUnion()) {
        const help = (
          if (ty.isTaggedNullable())
            "\n    Help: nullable types take precedence over error types"
          else ""
        ); 
        return self.error_(
          true, debug,
          "Expected error union type in 'try/orelse' expression. Type '{s}' is not an error union{s}",
          .{self.getTypename(ty), help}
        );
      } else if (ty.isTaggedUnion()) {
        if (ty.taggedUnion().getTag(ks.OkVar)) |tag| {
          const typ = tag.tag().getField(0).?.typ;
          self.insertVar(node.name.lexeme(), typ);
          return typ;
        }
      }
      return self.error_(true, debug, "Bad initialization on <empty>", .{});
    }
  }

  fn inferAssign(self: *Self, node: *tir.BinaryNode) !*Type {
    const token = Token.fromBinaryNode(node);
    var lhsTy = try self.infer(node.left);
    const rhsTy = try self.infer(node.right);
    // Here we use the op_tkty TkNoReturn for disambiguation btwn generated assignments
    // and user defined assignments.
    if (node.op_tkty == .TkNoReturn) {
      if (rhsTy.isNoreturnTy()) {
        // if we are assigning Noreturn to an ident from a desugared OrElse expr,
        // where type must be Result or noreturn, just return the ident's type.
        return lhsTy;
      } else if (lhsTy.isVariable()) {
        // if we are assigning to a Variable type from a desugared Match expr,
        // update the type of var to the value's type, return the value's type
        self.updateVar(node.left.getToken().lexeme(), rhsTy);
        return rhsTy;
      }
    }
    if (!lhsTy.mutable) {
      return self.mutationError("constant", token, lhsTy);
    }
    // continue to type check as expected
    const typ = try self.checkAssign(lhsTy, rhsTy, token, true);
    // update type.
    switch (node.left.*) {
      // need to always update, because lookup copies.
      .NdTVar => |*ident| self.insertVar(ident.token.lexeme(), typ),
      .NdSubscript => |*sub| {
        if (sub.expr.getType()) |ty| {
          if (ty.isModule() or (ty.isClass() and ty.klass().immutable)) {
            return self.mutationError(null, token, ty);
          }
        }
      },
      .NdDotAccess => |*da| {
        if (lhsTy.isFunction()) {
          if (da.lhs.getType()) |_ty| {
            if (!_ty.isModule()) {
              var ty = _ty.classOrInstanceClass();
              if (ty.klass().getMethodTy(da.rhs.NdTVar.value()).? == lhsTy) {
                return self.mutationError(null, token, lhsTy);
              }
            } else {
              return self.mutationError(null, token, _ty);
            }
          }
        }
      },
      else => {}
    }
    return typ;
  }

  fn inferList(self: *Self, node: *tir.ListNode) !*Type {
    var base = Type.newBuiltinGenericClass(ks.ListVar, .TkList, self.al);
    node.typ = base;
    if (node.elems.len == 0) {
      base.klass().appendTParam(&UnitTypes.tyAny, self.al);
      base.klass().empty = true;
      return base;
    }
    // infer type of elements stored in the list
    const typ = try self.infer(node.elems[0]);
    if (node.elems.len > 1) {
      for (node.elems[1..]) |elem| {
        const ty = try self.infer(elem);
        _ = self.checkAssign(typ, ty, undefined, false) catch {
          return self.errorFrom(
            true, elem,
            "Expected type '{s}' but found '{s}'",
            .{self.getTypename(typ), self.getTypename(ty)}
          );
        };
      }
    }
    base.klass().appendTParam(typ, self.al);
    return base;
  }

  fn inferTuple(self: *Self, node: *tir.ListNode) !*Type {
    var base = Type.newBuiltinGenericClass(ks.TupleVar, .TkTuple, self.al);
    node.typ = base;
    if (node.elems.len == 0) {
      base.klass().empty = true;
      return base;
    }
    // infer type of elements stored in the tuple
    var list = TypeList.initCapacity(node.elems.len, self.al);
    for (node.elems) |elem| {
      list.appendAssumeCapacity(try self.infer(elem));
    }
    base.klass().initTParamSlice(list.items(), null);
    return base;
  }

  fn inferMap(self: *Self, node: *tir.MapNode) !*Type {
    var base = Type.newBuiltinGenericClass(ks.MapVar, .TkMap, self.al);
    node.typ = base;
    if (node.pairs.len == 0) {
      const any = &UnitTypes.tyAny;
      base.klass().initTParamSlice(@constCast(&[_]*Type{any, any}), self.al);
      base.klass().empty = true;
      return base;
    }
    // infer type of items stored in the map
    const first_pair = node.pairs[0];
    var key_typ = try self.infer(first_pair.key);
    var val_typ = try self.infer(first_pair.value);
    if (node.pairs.len > 1) {
      for (node.pairs[1..]) |pair| {
        var typ = try self.infer(pair.key);
        _ = self.checkAssign(key_typ, typ, undefined, false) catch {
          return self.errorFrom(
            true, pair.key,
            "expected key type '{s}' but found '{s}'",
            .{self.getTypename(key_typ), self.getTypename(typ)}
          );
        };
        typ = try self.infer(pair.value);
        _ = self.checkAssign(val_typ, typ, undefined, false) catch {
          return self.errorFrom(
            true, pair.value,
            "expected value type '{s}' but found '{s}'",
            .{self.getTypename(val_typ), self.getTypename(typ)}
          );
        };
      }
    }
    assert(!key_typ.isUnion() and !val_typ.isUnion());
    base.klass().initTParamSlice(@constCast(&[_]*Type{key_typ, val_typ}), self.al);
    return base;
  }

  fn inferType(self: *Self, node: *tir.TypeNode) !*Type {
    try self.linker.linkType(node);
    // if this type node was found in an expression
    // (i.e. not in an alias or annotation context), then return TyType
    if (!node.from_alias_or_annotation) {
      return &UnitTypes.TyTy;
    }
    return node.typ;
  }

  fn inferAliasForCore(self: *Self, node: *tir.AliasNode) !*Type {
    try self.linker.linkAlias(node);
    return node.typ;
  }
  
  fn inferAlias(self: *Self, node: *tir.AliasNode) !*Type {
    if (self.inCoreModule()) {
      return self.inferAliasForCore(node);
    }
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    try self.linker.linkAlias(node);
    const typs = self.ctx.typ_scope.decls.getLast();
    self.ctx.cm().env.addTypes(typs, self.al);
    return node.typ;
  }

  fn inferCast(self: *Self, node: *tir.CastNode, ast_n: *Node) !*Type {
    try self.linker.linkCast(node);
    const typ = try self.infer(node.expr);
    try self.resolveType(node.typn.typ, ast_n.getToken(), self.al);
    var true_ctx: RelationContext = .RCAny;
    switch (node.expr.*) {
      .NdList, .NdTuple => |*col| {
        if (col.elems.len == 0) {
          true_ctx = .RCConst;
        }
      },
      .NdMap => |*map| {
        if (map.pairs.len == 0) {
          true_ctx = .RCConst;
        }
      },
      else => {}
    }
    // use coercion rules
    return try self.checkCast(typ, node.typn.typ, true_ctx, ast_n, true);
  }

  fn inferSubscript(self: *Self, node: *tir.SubscriptNode, narrowing: bool) TypeCheckError!*Type {
    if (node.typ) |ty| return ty;
    // if we're narrowing this subscript, it's narrowed type would not be
    // set yet, hence, we use `node.narrowed == null` to check that we're not narrowing,
    // hence, the narrowed type should already exist in varScope
    if (!narrowing and self.canNarrowSubscript(node)) {
      var env = TypeEnv.init(self.never_ty, self.al, null);
      env.global.pushScope();
      var narrowed = self.synthesizeSubscript(node, &env, true) catch |e| return e;
      if (self.inferVar(narrowed, false) catch null) |ty| {
        node.typ = ty;
        return ty;
      }
    }
    var expr_ty = try self.infer(node.expr);
    // fail fast
    if (!expr_ty.isClsParameterized()) {
      return self.errorFrom(
        true, node.index,
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    const index_ty = try self.infer(node.index);
    const ty = try self.checkSubscript(node, expr_ty, index_ty);
    node.typ = ty;
    return ty;
  }

  fn inferDeref(self: *Self, node: *tir.DerefNode, narrowing: bool) !*Type {
    if (node.typ) |ty| return ty;
    // if this node is not being narrowed (`node.narrowed == null`),
    // try to see if we can obtain an inferred narrow type.
    if (!narrowing and self.canNarrowDeref(node)) {
      var env = TypeEnv.init(self.never_ty, self.al, self.tenv);
      env.global.pushScope();
      var narrowed = self.synthesizeDeref(node, &env, true) catch return error.CheckError;
      if (self.inferVar(narrowed, false) catch null) |ty| {
        node.typ = ty;
        return ty;
      }
    }
    const expr_ty = try self.infer(node.expr);
    const ty = try self.checkDeref(node, expr_ty);
    node.typ = ty;
    return ty;
  }

  /// error if this tag is parameterized but is being accessed like a non-parameterized tag, for ex:
  /// type Tag = Foo(str) | Bar(num) ; Tag.Foo !error!
  /// type Tag{A, B} = Foo(A) | Bar(B) ; Tag.Foo !error!
  inline fn errorIfParameterizedTagAccessedBadly(
    self: *Self, ty: *Type, parent: ?*Node, token: Token, da: *tir.DotAccessNode,
  ) !void {
    if (ty.tag().fieldsLen() > 0 and (parent == null or !parent.?.isCall())) {
      return self.error_(
        true, token, "bad access of parameterized tag type '{s}'",
        .{self.getTypename(ty)}
      );
    }
    if (ty.tag().alias_is_parameterized) {
      return self.error_(
        true, da.lhs.getToken(), "bad access of parameterized type", .{}
      );
    }
  }

  fn canInterceptDotAccess(self: *Self, narrowing: bool, lhs_ty: *Type, node: *tir.DotAccessNode, ast_n: *Node) !bool {
    if (lhs_ty.isListTy() and node.rhs.isTVariable()) {
      const token = node.rhs.NdTVar.token;
      if (token.valueEql(@as([]const u8, ks.IterMVar))) {
        if (narrowing) return error.DotAccessIntercept;
        node.rhs.NdTVar.token = token.tkFrom(ks.ListIteratorVar, .TkIdent);
        const call = self.des.newCallNode(node.rhs, &[_]*Node{node.lhs});
        const rhs = self.des.newTVarNode(token);
        ast_n.* = .{.NdDotAccess = tir.DotAccessNode.init(call, rhs)};
        return true;
      }
    }
    return false;
  }

  fn inferDotAccess(self: *Self, node: *tir.DotAccessNode, ast_n: *Node, narrowing: bool) !*Type {
    if (node.typ) |ty| return ty;
    // set parent for later type disambiguation
    const parent = self.ctx.data.parent;
    errdefer self.ctx.data.parent = parent;

    // if this node is not being narrowed, try to see if we can obtain an inferred narrow type.
    if (!narrowing and self.canNarrowDotAccess(node)) {
      var env = TypeEnv.init(self.never_ty, self.al, self.tenv);
      env.global.pushScope();
      var erred = false;
      const narrowed = self.synthesizeDotAccess(node, ast_n, &env, true) catch |e| b: {
        if (e != error.DotAccessIntercept) return e;
        erred = true;
        break :b undefined;
      };
      if (!erred) {
        if (self.inferVar(narrowed, false) catch null) |ty| {
          node.typ = ty;
          return ty;
        }
      }
    }
    self.ctx.data.parent = ast_n;
    var lhs_ty = try self.infer(node.lhs);
    if (try self.canInterceptDotAccess(narrowing, lhs_ty, node, ast_n)) {
      return self.infer(ast_n);
    }
    self.ctx.data.parent = parent;
    const rhs_tok = node.rhs.getToken();
    if (lhs_ty.isStrTy()) {
      node.lhs.forceSetType(self.str_ty);
      lhs_ty = self.str_ty;
    } else if (lhs_ty.isTag()) {
      if (node.rhs.isNumberLiteral()) {
        // if this is a Just tag or this node has permission to use a number literal access:
        if (lhs_ty.isJustTy() or node.allow_tag_access) {
          return self.checkTagDotAccess(node, rhs_tok.lexeme(), lhs_ty, node.rhs.toIntNumber(usize));
        } else {
          return self.errorFrom(
            true, node.lhs, "expected 'Maybe' type, found '{s}'",
            .{self.getTypename(lhs_ty)}
          );
        }
      } else if (lhs_ty.tag().nameEql(rhs_tok.lexeme())) { // check if this tag type has this tag
        try self.errorIfParameterizedTagAccessedBadly(lhs_ty, self.ctx.data.parent, rhs_tok, node);
        node.typ = lhs_ty;
        return lhs_ty;
      } else if (node.is_desugared) {
        // sometimes in a match pattern, tag field access gets desugared to a dotaccess expr, for ex:
        // Foo(a=5, b='ha') -> $t1 = $tag.a ; $t2 = $tag.b, because desugaring has no knowledge of types.
        // If this happens, we rewrite the node to use the field's index/position instead
        if (lhs_ty.tag().getFieldWithId(rhs_tok.lexeme())) |idx| {
          node.rhs = Node.new(.{.NdNumber = tir.NumberNode.init(rhs_tok, @floatFromInt(idx))}, self.al);
          node.allow_tag_access = true;
          return self.inferDotAccess(node, ast_n, narrowing);
        }
      }
    } else if (lhs_ty.isTaggedUnion()) {
      // check if this tagged union has a tag with same name as rhs of the dot access expr
      if (lhs_ty.taggedUnion().getTag(rhs_tok.lexeme())) |ty| {
        try self.errorIfParameterizedTagAccessedBadly(ty, self.ctx.data.parent, rhs_tok, node);
        node.typ = ty;
        return if (parent) |p| (if (!p.isCall()) lhs_ty else ty) else lhs_ty;
      } else {
        return self.errorFrom(
          true, node.lhs, "type '{s}' has no tag '{s}'",
          .{self.getTypename(lhs_ty), rhs_tok.lexeme()}
        );
      }
    } else if (lhs_ty.isUnion()) {
      // handle Union Traits
      for (lhs_ty.union_().variants.values()) |_ty| {
        var is_ok = true;
        const ret = self.checkDotAccess(node, _ty, rhs_tok, rhs_tok.lexeme(), false) catch b: {
          is_ok = false;
          break :b undefined;
        };
        if (is_ok) return ret;
      }
    } else if (lhs_ty.isModule()) {
      const info = try self.checkModuleAccess(lhs_ty, rhs_tok, rhs_tok.lexeme());
      const ty = info.@"0";
      node.typ = ty;
      if (ty.isTaggedUnion() and (parent == null or !parent.?.isDotAccess())) {
        self.typeReferenceError(rhs_tok, ty);
      }
      return ty;
    }
    if (!lhs_ty.isClass() and !lhs_ty.isInstance() and !lhs_ty.isTrait()) {
      return self.errorFrom(
        true, node.lhs, "type '{s}' has no property '{s}'",
        .{self.getTypename(lhs_ty), rhs_tok.lexeme()}
      );
    }
    var cls_ty = lhs_ty.classOrInstanceClass();
    try self.resolveType(cls_ty, node.lhs.getToken(), self.al);
    return self.checkDotAccess(node, cls_ty, rhs_tok, rhs_tok.lexeme(), true);
  }

  fn inferRet(self: *Self, node: *tir.RetNode) !*Type {
    if (node.expr) |expr| {
      node.typ = try self.infer(expr);
    } else {
      node.typ = self.void_ty;
    }
    @setRuntimeSafety(false);
    return node.typ.?;
  }

  fn inferError(self: *Self, node: *tir.ErrorNode, typ: ?*Type) !*Type {
    if (typ) |ty| return ty;
    var ty = try self.infer(node.expr);
    if (ty.isErrorTy()) {
      return self.errorFrom(
        true, node.expr,
        "Nested error types are unsupported: '{s}'", .{self.getTypename(ty)}
      );
    }
    return Type.newTagWithParamTypes(ks.ErrorVar, .TkError, &[_]*Type{ty}, self.al);
  }

  fn inferScope(self: *Self, node: *tir.ScopeNode) !*Type {
    if (node.enter) {
      self.ctx.enterScope();
    } else if (node.leave) {
      self.ctx.leaveScope();
    } else {
      return try self.checkMatchDiagEnd();
    }
    return self.void_ty;
  }

  fn validateCallArgCount(self: *Self, fun_ty: *tir.Function, node: *tir.CallNode) !void {
    if (fun_ty.data.params.len != node.args.len) {
      if (node.variadic) {
        // argc -> == n - 1 or > n
        if (!(node.args.len == fun_ty.data.params.len - 1 or node.args.len > fun_ty.data.params.len)) {
          return self.errorFrom(
            true, node.expr,
            "Argument arity mismatch. Expected {} argument(s) but found {}", .{fun_ty.data.params.len - 1, node.args.len}
          );
        }
      } else {
        return self.errorFrom(
          true, node.expr,
          "Argument arity mismatch. Expected {} argument(s) but found {}", .{fun_ty.data.params.len, node.args.len}
        );
      }
    }
  }

  fn validateLabeledCallArguments(self: *Self, fun_ty: *tir.Function, node: *tir.CallNode) !void {
    if (node.labeled) {
      const fun = &fun_ty.data.node.?.NdBasicFun;
      var map = ds.StringHashMap(u32).init(self.al);
      // find duplicate labels
      for (node.args) |arg| {
        if (arg.isLblArg()) {
          if (map.get(arg.NdLblArg.label)) |_| {
            if (node.variadic) {
              // skip if this is an arg to the variadic param
              if (std.mem.eql(u8, fun.params[fun.params.len - 1].name.lexeme(), arg.NdLblArg.label)) {
                continue;
              }
            }
            self.softErrorFrom(arg.NdLblArg.ident, "duplicate labeled argument found", .{});
          } else {
            map.set(arg.NdLblArg.label, 1);
          }
        }
      }
      // simply sort the labeled arguments
      for (fun.params, 0..) |_param, i| {
        for (node.args, 0..) |arg, j| {
          if (arg.isLblArg()) {
            if (std.mem.eql(u8, _param.name.lexeme(), arg.NdLblArg.label)) {
              if (i >= node.args.len) {
                self.softErrorFrom(arg.NdLblArg.ident, "missing required argument(s)", .{});
                continue;
              }
              node.args[j] = node.args[i];
              node.args[i] = arg.NdLblArg.value;
            }
          }
        }
      }
      if (self.diag.hasErrors()) return error.CheckError;
      // report errors for unsorted/untransformed labeled args if any
      for (node.args) |arg| {
        if (arg.isLblArg()) {
          return self.errorFrom(
            true, arg.NdLblArg.ident, "illegal or invalid label: '{s}'",
            .{arg.NdLblArg.label}
          );
        }
      }
    }
  }

  fn validateCallArguments(self: *Self, fun_ty: *tir.Function, node: *tir.CallNode) !void {
    try self.validateLabeledCallArguments(fun_ty, node);
    if (!node.variadic) {
      for (fun_ty.data.params, node.args) |p_ty, arg| {
        const arg_ty = try self.infer(arg);
        _ = try self.checkArgumentAssign(
          p_ty, arg_ty, arg.getToken(),
          "Argument type mismatch. Expected type"
        );
        // set extra 'node' metadata for this arg type, which would be null if
        // arg type was created by a user
        if (p_ty.isFunction() and arg.isFun()) {
          p_ty.function().data.node = arg;
        }
      }
    } else {
      const non_varargs_len = fun_ty.data.params.len - 1;
      node.va_start = non_varargs_len;
      if (node.args.len == 0) return;
      // validate regular args
      for (fun_ty.data.params[0..non_varargs_len], node.args[0..non_varargs_len]) |p_ty, arg| {
        const arg_ty = try self.infer(arg);
        _ = try self.checkArgumentAssign(
          p_ty, arg_ty, arg.getToken(),
          "Argument type mismatch. Expected type"
        );
        // set extra 'node' metadata for this arg type, which would be null if
        // arg type was created by a user
        if (p_ty.isFunction() and arg.isFun()) {
          p_ty.function().data.node = arg;
        }
      }
      // validate varargs
      // we expect the variadic type to be a tuple, so perform relations check with it tparam:
      var va_ty = fun_ty.data.params[fun_ty.data.params.len - 1].klass().tparams.?[0];
      for (node.args[non_varargs_len..]) |arg| {
        const arg_ty = try self.infer(arg);
        _ = try self.checkArgumentAssign(
          va_ty, arg_ty, arg.getToken(),
          "Argument type mismatch. Expected type"
        );
      }
    }
  }

  inline fn checkForTraitBounds(self: *Self, tvar: *Type, on_typ: *Type, token: Token) !?*Type {
    if (tvar.variable().bounds) |bounds| {
      assert(!bounds.isTrait());
      const trait = try self.resolveTraitType(bounds, tvar.variable().token(), self.al);
      try self.checkTrait(on_typ, trait, token);
      return trait;
    }
    return null;
  }

  /// resolve type params on a call node
  fn linkAndCheckCallTypeParams(self: *Self, node: *Node, debug: Token) !void {
    const parent = self.ctx.data.parent;
    defer self.ctx.data.parent = parent;
    self.ctx.data.parent = node;
    for (node.NdGenericCall.targs, 0..) |ty, i| {
      if (ty.isVariable()) {
        var ident = tir.TVarNode.init(ty.variable().value);
        node.NdGenericCall.targs[i] = try self.inferVar(&ident, true);
      } else {
        try self.resolveType(ty, debug, self.al);
      }
    }
  }

  fn canInterceptCall(self: *Self, typ: *tir.Function, node: *tir.CallNode) bool {
    _ = self;
    if (node.args.len == 1) {
      if (node.args[0].getType()) |ty| {
        if (ty.isListTy()) {
          var name = if (typ.data.node) |nd| nd.getBasicFun().data.name orelse return false else return false;
          if (name.lexeme()[0] == '@' and std.mem.eql(u8, name.lexeme(), ks.IterVar)) {
            node.expr.NdTVar.token = node.expr.NdTVar.token.tkFrom(ks.ListIteratorVar, .TkIdent);
            return true;
          }
        }
      }
    }
    return false;
  }

  fn setSynthNameOnDotAccessLHS(self: *Self, node: *Node, synth_name: []const u8) void {
    _ = self;
    if (node.isDotAccess()) {
      const token = node.NdDotAccess.rhs.getToken();
      node.NdDotAccess.rhs.* = .{.NdTVar = tir.TVarNode.init(token.tkFrom(synth_name, .TkIdent))};
    }
  }

  fn inferCall(self: *Self, node: *tir.CallNode, ast_n: *Node) !*Type {
    // set parent for later type disambiguation
    const parent = self.ctx.data.parent;
    errdefer self.ctx.data.parent = parent;
    self.ctx.data.parent = ast_n;
    var ty = try self.infer(node.expr);
    self.ctx.data.parent = parent;
    if (ty.isClass()) {
      return try self.inferClsCall(node, ty, ast_n);
    } else if (ty.isTag()) {
      return try self.inferTagCall(node, ty);
    }
    // check that we're actually calling a function
    if (!ty.isFunction()) {
      return self.errorFrom(
        true, node.expr,
        "Expected callable or function type but found '{s}'", .{self.getTypename(ty)}
      );
    }
    var fun_ty = ty.function();
    node.variadic = ty.variadic;
    // perform signature checks
    try self.validateCallArgCount(fun_ty, node);
    // check generic is properly called
    if (node.isGeneric() and !fun_ty.isParameterized()) {
      return self.errorFrom(
        true, node.expr, "Non-generic function called as generic", .{}
      );
    }
    const debug = ast_n.getToken();
    if (!fun_ty.isParameterized()) {
      for (fun_ty.data.params, 0..) |typ, ppos| {
        try self.resolveType(typ, debug, self.al);
        if (typ.isFunction() and node.args[ppos].isFun()) {
          typ.function().data.node = node.args[ppos];
        }
      }
      var is_cyclic = false;
      if (fun_ty.data.node) |fun_idx| {
        is_cyclic = self.cycles.contains(fun_idx, Node.eql);
        if (!is_cyclic and !self.isCheckedFun(fun_idx)) {
          _ = try self.inferFun(fun_idx, ty);
        }
      }
      try self.validateCallArguments(fun_ty, node);
      // [N*]: check if we're currently resolving this type, i.e. if it's recursive
      if (is_cyclic) {
        if (try self.inferFunReturnTypePartial(self.cunit.lookupFunc(fun_ty.data.node.?).?)) |typ| {
          return typ;
        }
        return ty.newRecursive().box(self.al);
      }
      return fun_ty.data.ret;
    }
    if (node.targs) |targs| {
      if (targs.len != fun_ty.tparams.?.len) {
        return self.errorFrom(
          true, node.expr,
          "Generic function not instantiated correctly. Expected {} type parameter(s) but found {}",
          .{fun_ty.tparams.?.len, targs.len}
        );
      }
    }
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    // generic, so monomorphize.
    var args_inf = TypeList.initCapacity(node.args.len, self.al);
    for (node.args) |arg| {
      args_inf.appendAssumeCapacity(try self.infer(arg));
    }
    if (self.canInterceptCall(fun_ty, node)) {
      return self.inferCall(node, ast_n);
    }
    const old_fun_id = fun_ty.data.node.?;
    var is_generic_mtd = old_fun_id.isGenericMtd();
    var cls_ty: ?*Type = null;
    if (is_generic_mtd) {
      var params = old_fun_id.NdGenericMtd.params;
      for (params.items()) |data| {
        if (data.tvar) |tvar| {
          self.insertType(tvar, data.typ.?);
        }
      }
      // Insert self into current scope. Order matters here.
      const tmp = params.getLast().from;
      self.insertVar(ks.SelfVar, tmp);
      cls_ty = tmp;
    }
    defer { if (is_generic_mtd) self.deleteVar(ks.SelfVar); }
    const fun = old_fun_id.getGenericFun();
    const bfun = &fun.fun.NdBasicFun;
    // store the actual generic type arguments received by this class
    var _targs = TypeList.init();
    // inferred type arguments must match the generic type params if specified
    // ex: foo{str, num}('a', 5)
    if (node.targs) |targs| {
      try self.linkAndCheckCallTypeParams(ast_n, debug);
      for (fun.params, 0..) |tvar, tpos| {
        for (bfun.params, 0..) |param, ppos| {
          if (param.typ.typeidEql(tvar)) {
            if (!targs[tpos].typeidEql(args_inf.itemAt(ppos).classOrInstanceClass())) {
              return self.errorFrom(
                true, node.args[ppos],
                "Type parameter mismatch. Expected type '{s}' but found '{s}'",
                .{self.getTypename(targs[tpos]), self.getTypename(args_inf.itemAt(ppos))}
              );
            }
          }
        }
      }
      const tparams = fun_ty.tparams.?;
      for (tparams, targs) |tvar, _ty| {
        self.insertType(tvar, _ty);
        _targs.append(_ty, self.al);
      }
      for (tparams, targs, 0..) |tvar, _ty, i| {
        if (try self.checkForTraitBounds(tvar, _ty, debug)) |_bty| {
          self.insertType(tvar, _bty);
          _targs.items()[i] = _bty;
        }
      }
    } else {
      for (fun.params) |tvar| {
        var resolved = false;
        for (bfun.params, 0..) |param, ppos| {
          if (param.typ.typeidEql(tvar)) {
            // NOTE: It's important to use `tvar` instead of `param.typ` because the bounds is literally on `tvar`
            const t = args_inf.itemAt(ppos);
            if (try self.checkForTraitBounds(tvar, t, debug)) |_bty| {
              self.insertType(tvar, _bty);
              _targs.append(_bty, self.al);
            } else {
              self.insertType(tvar, t);
              _targs.append(t, self.al);
            }
            resolved = true;
            break;
          } else if (param.typ.hasThisVariable(tvar)) {
            const inf_ty = blk: {
              var t = args_inf.itemAt(ppos).classOrInstanceClass();
              if (!bfun.data.variadic) {
                break :blk t;
              }
              var typ = Type.newBuiltinGenericClass(ks.ListVar, .TkList, self.al);
              typ.klass().appendTParam(t, self.al);
              break :blk typ;
            };
            _ = try self.inferTypex(debug, tvar, param.typ, inf_ty);
            const t = try self.lookupName(tvar.variable().value, true);
            if (try self.checkForTraitBounds(tvar, t, debug)) |_bty| {
              self.insertType(tvar, _bty);
              _targs.append(_bty, self.al);
            } else {
              _targs.append(t, self.al);
            }
            resolved = true;
            break;
          }
        }
        if (!resolved) {
          self.softError(
            tvar.variable().value,
            "Could not resolve the generic type variable '{s}'. Consider explicitly specifying the type parameter.",
            .{self.getTypename(tvar)}
          );
          return self.errorFmt(debug, 4, 2, "Called from here:", .{});
        }
      }
    }
    // link the function
    // lookup node using inferred args.
    // - if found just return the ret type of the found node
    // - else, do the stuff below, and cache the node
    const synth_name = self.createFunSynthName(bfun, &args_inf, _targs.items(), cls_ty);
    if (self.findGenInfo(old_fun_id, synth_name)) |info| {
      node.expr.setType(info.typ);
      self.setSynthNameOnDotAccessLHS(node.expr, synth_name);
      return info.typ.function().data.ret;
    }
    const new_fun_id = bfun.clone(self.al);
    _ = try self.buildFunFlow(new_fun_id);
    self.ctx.var_scope.pushScope();
    defer self.ctx.var_scope.popScope();
    var new_fun_ty = try self.inferFunPartial(new_fun_id);
    for (new_fun_ty.function().data.params, 0..) |typ, ppos| {
      try self.resolveType(typ, debug, self.al);
      if (typ.isFunction() and node.args[ppos].isFun()) {
        typ.function().data.node = node.args[ppos];
      }
    }
    // validate monomorphized function params
    try self.validateCallArguments(new_fun_ty.function(), node);
    var typ = try self.inferFun(new_fun_id, new_fun_ty);
    const ret = typ.function().data.ret;
    node.expr.setType(typ);
    self.addGenInfo(
      old_fun_id,
      new_fun_id,
      boxSynthName(old_fun_id.getToken(), synth_name),
      typ
    );
    if (cls_ty) |_ty| {
      // handle generic method calls:
      // look for the class with this method, and add it to the node.
      _ty.klass().appendMethodTyAndNode(typ, self.al);
      self.setSynthNameOnDotAccessLHS(node.expr, synth_name);
      @setRuntimeSafety(false);
      new_fun_id.NdBasicFun.data.name.? = new_fun_id.NdBasicFun.data.name.?.tkFrom(synth_name, .TkIdent);
    } else {
      self.setSynthNameOnDotAccessLHS(node.expr, synth_name);
    }
    return ret;
  }

  fn validateClsCallArguments(self: *Self, node: *tir.CallNode, fun_ty: *Type, token: Token) !void {
    if (!fun_ty.function().data.ret.isVoidTy()) {
      return self.error_(
        true, token, "Expected 'void' return type in `init` method but found '{s}'",
        .{self.getTypename(fun_ty.function().data.ret)}
      );
    }
    try self.validateCallArguments(fun_ty.function(), node);
  }

  fn validateFieldInitializations(self: *Self, init_mtd: ?*Node, cls_ty: *tir.Class, node: *tir.CallNode) !void {
    if (cls_ty.data.fields.len() == 0) return;
    if (init_mtd == null) {
      for (cls_ty.data.fields.items()) |field| {
        if (!field.fieldHasDefault()) {
          self.softErrorFrom(
            (cls_ty.data.node orelse node.expr),
            "a class having field(s) without defaults "
            ++ "must define an `init` method that initializes such field(s)",
            .{}
          );
          return self.errorFrom(
            true, field,
            "field '{s}' is declared but uninitialized",
            .{field.getFieldLexeme()}
          );
        }
      }
    }
    if (init_mtd) |_mtd| {
      // This only verifies definite assignments from direct statements on a seq edge for now
      const stmts = _mtd.NdBasicFun.data.body.block().nodes;
      var errors: usize = 0;
      for (cls_ty.data.fields.items()) |field| {
        if (field.fieldHasDefault()) continue;
        var initialized = false;
        for (stmts) |stmt| {
          if (stmt.isExprStmt()) {
            if (stmt.NdExprStmt.isSelfDotAccessAssignment()) |da| {
              if (da.rhs.NdTVar.valueEql(field.getFieldLexeme())) {
                // check if this field is used in its own initialization
                if (stmt.NdExprStmt.expr.NdAssign.right.containsField(field.getFieldName())) |tk| {
                  self.softError(tk, "This field appears to be used in its own initialization.", .{});
                } else {
                  initialized = true;
                }
                break;
              }
            }
          } else if (stmt.containsField(field.getFieldName())) |tk| {
            self.softError(tk, "This field appears to be used in its own initialization.", .{});
            break;
          }
        }
        if (!initialized) {
          errors += 1;
          self.softErrorFrom(
            field,
            "I am unable to deduce that the field '{s}' is definitely initialized.",
            .{field.getFieldLexeme()}
          );
        }
      }
      if (errors > 0) return error.CheckError;
    } else {
      var inits: usize = 0;
      for (cls_ty.data.fields.items()) |field| {
        if (field.fieldHasDefault()) {
          inits += 1;
        }
      }
      if (inits != cls_ty.data.fields.len()) {
        return self.errorFrom(
          true, node.expr,
          "class '{s}' have fields that are uninitialized",
          .{cls_ty.data.name}
        );
      }
    }
  }

  fn inferClsCall(self: *Self, node: *tir.CallNode, ty: *Type, ast_n: *Node) TypeCheckError!*Type {
    const al = self.al;
    // infer
    var cls_ty = ty.klass();
    var init_mtd = cls_ty.getMethodTy(ks.InitVar);
    const init_mtd_id = cls_ty.getMethod(ks.InitVar);
    if (init_mtd_id) |_nd| {
      node.variadic = _nd.NdBasicFun.data.variadic;
      var fun_ty = try self.createFunType(_nd, null);
      try self.validateCallArgCount(fun_ty.function(), node);
    } else if (node.args.len > 0) {
      // if the class has no init method, it shouldn't be called with arguments
      return self.errorFrom(
        true, node.expr,
        "Too many arguments to class call. Expected none but found {}.", .{node.args.len}
      );
    }
    // check fields
    try self.validateFieldInitializations(init_mtd_id, cls_ty, node);
    // check generic is properly called
    if (node.isGeneric() and !cls_ty.isParameterized()) {
      return self.errorFrom(
        true, node.expr,
        "Non-generic class called as generic", .{}
      );
    }
    if (!cls_ty.isParameterized()) {
      if (cls_ty.data.node) |cls_node| {
        if (!self.cycles.contains(cls_node, Node.eql) and !cls_node.NdClass.data.checked) {
          _ = try self.inferClass(cls_node, ty, &[_]*Type{}, &[_]*Type{});
          // try to obtain the inferred init since this class just got checked
          init_mtd = cls_ty.getMethodTy(ks.InitVar);
        }
      }
      if (init_mtd_id) |_nd| {
        try self.validateClsCallArguments(
          node,
          init_mtd orelse try self.createFunType(_nd, null),
          _nd.getToken(),
        );
      }
      const ret = ty.toInstance(al);
      node.typ = ret;
      return ret;
    }
    if (node.targs) |targs| {
      if (targs.len != cls_ty.tparams.?.len) {
        return self.errorFrom(
          true, node.expr,
          "Generic class instantiated incorrectly. Expected {} type parameter(s) but found {}",
          .{cls_ty.tparams.?.len, targs.len}
        );
      }
    }
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    // generic, so monomorphize.
    var args_inf = TypeList.initCapacity(node.args.len, al);
    for (node.args) |arg| {
      args_inf.appendAssumeCapacity(try self.infer(arg));
    }
    // Insert self into current scope. Order matters here.
    self.insertVar(ks.SelfVar, ty);
    defer self.deleteVar(ks.SelfVar);
    const old_cls_id = cls_ty.data.node.?;
    const cls = &old_cls_id.NdClass;
    const tk = ast_n.getToken();
    // store the actual generic type arguments received by this class
    var _targs = TypeList.init();
    // inferred type arguments must match the generic type params if specified
    // ex: Fmt{str, num}('a', 5)
    if (node.targs) |targs| {
      try self.linkAndCheckCallTypeParams(ast_n, tk);
      if (init_mtd_id) |_nd| {
        const cls_params = _nd.NdBasicFun.params;
        for (cls.data.params.?, 0..) |tvar, tpos| {
          for (cls_params, 0..) |param, ppos| {
            if (param.typ.typeidEql(tvar)) {
              if (!targs[tpos].typeidEql(args_inf.itemAt(ppos).classOrInstanceClass())) {
                return self.errorFrom(
                  true, node.args[ppos],
                  "Type parameter mismatch. Expected type '{s}' but found '{s}'",
                  .{self.getTypename(targs[tpos]), self.getTypename(args_inf.itemAt(ppos))}
                );
              }
            }
          }
        }
      }
      const tparams = cls_ty.tparams.?;
      for (tparams, targs) |tvar, t| {
        self.insertType(tvar, t);
        _targs.append(t, al);
      }
      for (tparams, targs, 0..) |tvar, t, i| {
        if (try self.checkForTraitBounds(tvar, t, tk)) |_bty| {
          self.insertType(tvar, _bty);
          _targs.items()[i] = _bty;
        }
      }
    } else {
      var cls_params: ?tir.ParamItems = if (init_mtd_id) |_nd| _nd.NdBasicFun.params else null;
      const token = node.expr.getToken();
      for (cls.data.params.?) |tvar| {
        var resolved = false;
        if (cls_params) |params| {
          for (params, 0..) |param, ppos| {
            if (param.typ.typeidEql(tvar)) {
              const t = args_inf.itemAt(ppos);
              if (try self.checkForTraitBounds(tvar, t, tk)) |_bty| {
                self.insertType(tvar, _bty);
                _targs.append(_bty, al);
              } else {
                self.insertType(tvar, t);
                _targs.append(t, al);
              }
              resolved = true;
              break;
            } else if (param.typ.hasThisVariable(tvar)) {
              const inf_ty = blk: {
                var t = args_inf.itemAt(ppos).classOrInstanceClass();
                if (!init_mtd_id.?.NdBasicFun.data.variadic) {
                  break :blk t;
                }
                var typ = Type.newBuiltinGenericClass(ks.ListVar, .TkList, al);
                typ.klass().appendTParam(t, al);
                break :blk typ;
              };
              _ = try self.inferTypex(token, tvar, param.typ, inf_ty);
              const t = try self.lookupName(tvar.variable().value, true);
              if (try self.checkForTraitBounds(tvar, t, tk)) |_bty| {
                self.insertType(tvar, _bty);
                _targs.append(_bty, al);
              } else {
                _targs.append(t, al);
              }
              resolved = true;
              break;
            }
          }
        }
        if (!resolved) {
          self.softError(
            tvar.variable().value,
            "Could not resolve the generic type variable '{s}'. Consider explicitly specifying the type parameter.",
            .{self.getTypename(tvar)}
          );
          return self.errorFmt(token, 4, 2, "Called from here:", .{});
        }
      }
    }
    const cls_node = cls_ty.data.node;
    cls_ty.data.node = null;
    var new_cls_ty = ty.clone(al);
    new_cls_ty.klass().tparams = _targs.items();
    cls_ty.data.node = cls_node;
    try self.resolveType(new_cls_ty, tk, al);
    const typ = new_cls_ty.toInstance(al);
    node.expr.setType(new_cls_ty);
    node.typ = typ;
    return typ;
  }

  fn validateTagArgCount(self: *Self, tag: *tir.Tag, node: *tir.CallNode) !void {
    if (tag.fieldsLen() != node.args.len) {
      return self.errorFrom(
        true, node.expr,
        "Argument arity mismatch. Expected {} argument(s) but found {}", .{tag.fieldsLen(), node.args.len}
      );
    }
  }

  fn validateLabeledTagArguments(self: *Self, tag_ty: *tir.Tag, node: *tir.CallNode) !void {
    if (node.labeled) {
      var map = ds.StringHashMap(u32).init(self.al);
      // find duplicate labels
      for (node.args) |arg| {
        if (arg.isLblArg()) {
          if (map.get(arg.NdLblArg.label)) |_| {
            self.softErrorFrom(arg.NdLblArg.ident, "duplicate labeled argument found", .{});
          } else {
            map.set(arg.NdLblArg.label, 1);
          }
        }
      }
      // simply sort the labeled arguments
      for (tag_ty.fieldSlice(), 0..) |param, i| {
        for (node.args, 0..) |arg, j| {
          if (arg.isLblArg()) {
            if (param.name) |name| {
              if (name.valueEql(arg.NdLblArg.label)) {
                node.args[j] = node.args[i];
                node.args[i] = arg.NdLblArg.value;
              }
            }
          }
        }
      }
      if (self.diag.hasErrors()) return error.CheckError;
      // report errors for unsorted/untransformed labeled args if any
      for (node.args) |arg| {
        if (arg.isLblArg()) {
          return self.errorFrom(
            true, arg.NdLblArg.ident, "illegal or invalid label: '{s}'",
            .{arg.NdLblArg.label}
          );
        }
      }
    }
  }

  fn inferTagCall(self: *Self, node: *tir.CallNode, ty: *Type) !*Type {
    const cast = blk: {
      if (node.expr.isDotAccess()) {
        if (node.expr.NdDotAccess.lhs.getType()) |t| {
          if (t.isTaggedUnion()) {
            break :blk t;
          }
        }
      }
      break :blk null;
    };
    var tag = ty.tag();
    const token = node.expr.getToken();
    if (tag.instantiated) {
      return self.error_(
        true, token, "Cannot call already instantiated tag type '{s}'",
        .{self.getTypename(ty)}
      );
    }
    try self.validateTagArgCount(tag, node);
    try self.validateLabeledTagArguments(tag, node);
    if (!tag.isParameterized()) {
      const ret = ty.clone(self.al);
      ret.tag().instantiated = true;
      node.typ = ret;
      return cast orelse ret;
    }
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    var fields = tag.fields.?;
    var type_params: ?*TypeList = (
      if (fields.itemAt(0).tdecl != null and fields.itemAt(0).tdecl.?.isGeneric())
        &fields.items()[0].tdecl.?.generic().tparams
      else null
    );
    var inf = TypeList.initCapacity(node.args.len, self.al);
    var args = tir.Tag.TagFieldList.initCapacity(node.args.len, self.al);
    var checks = @as(usize, 0);
    for (node.args, 0..) |arg, i| {
      var inf_ty = try self.infer(arg);
      var param_ty = fields.itemAt(i).typ;
      inf.appendAssumeCapacity(inf_ty);
      if (type_params == null) {
        var pty = blk: {
          if (!param_ty.hasVariable()) break :blk param_ty;
          const t =
            if (param_ty.isVariable()) try self.lookupName(param_ty.variable().value, true)
            else break :blk try self.linker.resolve(param_ty, token);
          break :blk try self.linker.resolve(t, token);
        };
        const t = try self.checkArgumentAssign(pty, inf_ty, arg.getToken(), "Expected type");
        args.appendAssumeCapacity(.{.name = fields.itemAt(i).name, .typ = t});
        checks += 1;
      } else {
        args.appendAssumeCapacity(.{.name = fields.itemAt(i).name, .typ = self.void_ty});
      }
    }
    if (checks == node.args.len) {
      var new = Type.newTagWithParams(tag.name, tag.ty, args.items(), self.al);
      new.tag().instantiated = true;
      node.typ = new;
      return cast orelse new;
    }
    if (type_params) |tparams| {
      for (tparams.items()) |tparam| {
        for (fields.items(), 0..) |fd, i| {
          if (tparam.typeidEql(fd.typ)) {
            self.insertType(tparam, inf.itemAt(i));
            break;
          } else if (fd.typ.hasThisVariable(tparam)) {
            var t = inf.itemAt(i).classOrInstanceClass();
            const _sol = try self.inferTypex(token, tparam, fd.typ, t);
            if (self.findType(tparam.variable().lexeme()) == null) {
              self.insertType(tparam, _sol);
            }
            break;
          }
        }
      }
      for (0..node.args.len) |i| {
        var tk = node.args[i].getToken();
        var inf_ty = inf.itemAt(i);
        var param_ty = blk: {
          // FIXME: first check if the field type is a variable, if so, just use the resolved type.
          // This fixes the issue with resolving a variable directly when the resolved
          // type's alias is generic it confuses the linker, mistaking the resolution
          // for the use of a generic type without parameters.
          var fd_ty = fields.itemAt(i).typ;
          if (fd_ty.isVariable()) {
            if (self.linker.lookupTypeVariable(fd_ty)) |_ty| {
              break :blk _ty;
            }
          }
          break :blk self.linker.resolve(fd_ty, tk) catch fd_ty;
        };
        const t = try self.checkArgumentAssign(param_ty, inf_ty, tk, "Expected type");
        args.items()[i] = (.{.name = fields.itemAt(i).name, .typ = t});
      }
    }
    var new = Type.newTagWithParams(tag.name, tag.ty, args.items(), self.al);
    new = try self.linker.resolve(new, token);
    new.tag().instantiated = true;
    node.typ = new;
    return cast orelse new;
  }

  fn looksLikeNeverType(nghs: *NeighbourList, fun_ty: *Type) bool {
    // Conservatively check if a function calls itself infinitely
    if (nghs.len() == 1) {
      if (nghs.itemAt(0).node.get().bb.len() == 1) {
        const node = nghs.itemAt(0).node.get().bb.getLast().?;
        if (node.isExprStmt() and node.NdExprStmt.expr.isCall()) {
          if (node.NdExprStmt.expr.getBasicCall().expr.getType()) |typ| {
            return typ == fun_ty;
          }
        }
      }
    }
    return false;
  }

  fn createFunType(self: *Self, ast_node: *Node, ret: ?*Type) !*Type {
    const ret_ty = blk: {
      if (ret) |typ| {
        break :blk typ;
      } else {
        break :blk Type.newVariableAToken(Token.getDefaultToken(), self.al);
      }
    };
    const mod_ty = if (!ast_node.getBasicFun().data.builtin) self.ctx.cmt() else null;
    var ty = Type.newFunction(ret_ty, mod_ty, self.al);
    var fun = ty.function();
    fun.data.node = ast_node;
    const _node = ast_node.getBasicFun();
    if (ast_node.isGenericFun()) {
      fun.tparams = ast_node.NdGenericFun.params;
    } else if (ast_node.isGenericMtd()) {
      fun.tparams = ast_node.getGenericFun().params;
    }
    ty.aspec = AccessSpecifier.getASpec(_node.data.public);
    var params = TypeList.initCapacity(_node.params.len, self.al);
    for (_node.params) |prm| {
      params.appendAssumeCapacity(prm.typ);
    }
    fun.data.params = params.items();
    ty.variadic = _node.data.variadic;
    return ty.box(self.al);
  }

  fn inferFunPartial(self: *Self, node: *Node) !*Type {
    if (node.isBasicFun()) {
      try self.linker.linkBasicFun(&node.NdBasicFun, null);
    }
    // we need to infer this first to handle recursive functions
    const ty = try self.createFunType(node, node.getType());
    // set the function's name (if available) to it's full type
    if (node.getBasicFun().data.name) |ident| {
      self.insertVar(ident.lexeme(), ty);
      self.ctx.cm().setIdTy(ident.lexeme(), ty, ty.aspec, self.al);
    }
    return ty;
  }

  fn excludeNoreturn(typ: *Type, al: Allocator) *Type {
    var uni = Union.init();
    for (typ.union_().variants.values()) |ty| {
      if (ty.isNoreturnTy()) {
        continue;
      }
      uni.set(ty, al);
    }
    if (uni.variants.count() == typ.union_().variants.count()) return typ;
    return uni.toTypeBoxed(al);
  }

  fn returnType(self: *Self, typ: *Type, debug: *Node) *Type {
    if (typ.isUnion()) {
      var ty = excludeNoreturn(typ, self.al);
      if (ty.isUnion()) {
        var all_tags = true;
        for (ty.union_().variants.values()) |_ty| {
          if (!_ty.isTag()) {
            all_tags = false;
            break;
          }
        }
        if (!all_tags) {
          self.softErrorFrom(debug, "cannot return multiple types: '{s}'", .{self.getTypename(typ)});
        } else {
          return ty.toTaggedUnion(self.al);
        }
      }
    }
    return typ;
  }

  fn inferFunReturnTypePartial(self: *Self, graph: *FlowGraph) !?*Type {
    // we're only here because this function is recursive
    var prev_nodes = graph.exit().getPrevNeighbours().items();
    // unionify all return types at exit
    var uni = Union.init();
    for (prev_nodes) |ngh| {
      if (graph.get(ngh.node.pos).bb.getNonScopeLast()) |last| {
        if (last.isRet()) {
          if (last.getType()) |ty| {
            uni.set(ty, self.al);
          } else if (last.NdRet.expr) |expr| {
            if (!self.cycles.contains(expr, Node.eql)) {
              self.cycles.append(expr, self.al);
              defer _ = self.cycles.pop();
              return try self.infer(expr);
            }
          }
        }
      }
    }
    if (uni.variants.count() == 0) {
      return null;
    } else if (uni.variants.count() == 1) {
      return uni.variants.values()[0];
    }
    return uni.toTypeBoxed(self.al);
  }

  fn inferFunReturnType(self: *Self, node: *Node, graph: *FlowGraph, fun_ty: *Type) !*Type {
    const al = self.al;
    var prev_flo_nodes = graph.exit().getPrevNeighbours().filter(FlowNode.FilterPredicate.isNotDeadNode, al);
    var has_void_ty = false;
    var has_rec_ty = false;
    var has_non_void_ty = false;
    var has_noreturn_ty = false;
    var has_return_node = false;
    var i = @as(usize, 0);
    m: while (i < prev_flo_nodes.len()) : (i += 1) {
      var ngh = prev_flo_nodes.itemAt(i);
      if (ngh.node.get().bb.isOrphanSymbol()) {
        const prevs = ngh.node.getPrevNeighbours();
        prev_flo_nodes.extend(prevs, al);
        continue;
      }
      if (ngh.node.get().bb.getLast()) |last| {
        if (last.isRet()) {
          has_return_node = true;
          if (last.getType()) |ty| {
            if (ty.isRecursive()) {
              has_rec_ty = true;
            } else if (ty.isVoidTy()) {
              has_void_ty = true;
            } else if (ty.isNoreturnTy()) {
              has_noreturn_ty = true;
            } else {
              has_non_void_ty = true;
            }
            continue;
          }
        } else if (last.getTypeE()) |ty| {
          if (ty.isNoreturnTy()) {
            has_noreturn_ty = true;
          } else if (last.isCondition() or last.isMCondition()) {
            has_void_ty = !(
              if (last.isCondition()) last.NdCondition.has_never_typ_in_false_path
              else last.NdMCondition.tst.NdCondition.has_never_typ_in_false_path
            );
            continue;
          }
        } else if (last.isEmpty()) {
          const prevs = ngh.node.getPrevNeighbours();
          for (prevs.items()) |ng| {
            if (ng.node.get().bb.getLast()) |lst| {
              if (lst.isCondition() or lst.isMCondition()) {
                 has_void_ty = !(
                  if (lst.isCondition()) lst.NdCondition.has_never_typ_in_false_path
                  else lst.NdMCondition.tst.NdCondition.has_never_typ_in_false_path
                );
                continue :m;
              }
            }
          }
        } else if (last.isFailMarker()) {
          continue;
        }
      }
      has_void_ty = true;
    }
    var add_never_ty = false;
    // if it looks like a never type function, set rec & never
    if (looksLikeNeverType(&prev_flo_nodes, fun_ty)) {
      add_never_ty = true;
      has_rec_ty = true;
      has_void_ty = false;
    }
    // if we find the function has type noreturn, and there's no return node in the function,
    // simply turn off has_void_ty:
    if (has_noreturn_ty and !has_return_node) {
      has_void_ty = false;
    }
    // When inferring a function's return type:
    // - if we find a recursive type, and a non-void type, use the non-void type as the function's return type
    // - if we find a recursive type, and a void type or only a recursive type, then use the type 'never'
    var uni = Union.init();
    const void_ty: *Type = self.void_ty;
    const nvr_ty: *Type = self.never_ty;
    // unionify all return types at exit
    for (prev_flo_nodes.items()) |ngh| {
      if (ngh.node.get().bb.getLast()) |last| {
        if (!last.isRet()) continue;
        var typ = last.getType() orelse void_ty;
        if (!has_void_ty and typ.isVoidTy()) {
          continue;
        }
        if (has_rec_ty) {
          if (has_non_void_ty) {
            if (typ.isRecursive()) {
              continue;
            }
          } else if (has_void_ty or typ.isRecursive()) {
            typ = nvr_ty;
          }
        }
        uni.set(typ, al);
      }
    }
    if (add_never_ty) uni.set(nvr_ty, al);
    if (has_noreturn_ty) uni.set(&UnitTypes.tyNoReturn, al);
    if (has_void_ty) uni.set(void_ty, al);
    var inf_ret_ty = if (uni.variants.isNotEmpty()) uni.toType(al) else void_ty.*;
    if (node.NdBasicFun.data.ret) |ret_ty| {
      for (prev_flo_nodes.items()) |ngh| {
        if (ngh.node.get().bb.getNonScopeDiagLast()) |last| {
          if (last.isRet()) {
            if (last.getType()) |typ| {
              if (typ.isRecursive()) {
                continue;
              }
              if (!node.NdBasicFun.data.builtin and !ret_ty.isRelatedTo(typ, .RCAny, al)) {
                return self.errorFrom(
                  true, last,
                  "Expected return type '{s}' but found '{s}'",
                  .{self.getTypename(ret_ty), self.getTypename(typ)}
                );
              }
            }
          } else if (!node.NdBasicFun.data.builtin and !ret_ty.isLikeVoid() and !ret_ty.isLikeNoreturn()) {
            if (!(ret_ty.isRecursive() and ret_ty.recursive().base.isNeverTy())) {
              // control reaches exit from this node (`nd`), although this node
              // doesn't return anything hence (void), but return type isn't void
              return self.errorFrom(
                true, last,
                "Control flow reaches exit from this point without returning type '{s}'",
                .{self.getTypename(ret_ty)}
              );
            }
          }
        }
      }
      const ty = self.returnType(&inf_ret_ty, node);
      if (!node.NdBasicFun.data.builtin and !ret_ty.isRelatedTo(ty, .RCAny, al)) {
        if (!ret_ty.isNoreturnTy()) {
          return self.errorFrom(
            true, node,
            "Expected return type '{s}' but found '{s}'",
            .{self.getTypename(ret_ty), self.getTypename(&inf_ret_ty)}
          );
        } else {
          const debug = (
            if (prev_flo_nodes.isNotEmpty()) prev_flo_nodes.itemAt(0).node.get().bb.nodes.itemAt(0)
            else node
          );
          return self.errorFrom(
            true, debug,
            "Control flow reaches exit; function declared type '" ++ ks.NoReturnVar ++ "' returns",
            .{}
          );
        }
      }
      return self.returnType(ret_ty, node);
    }
    const ty = self.returnType(inf_ret_ty.box(al), node);
    node.NdBasicFun.data.ret = ty;
    return ty;
  }

  fn inferFun(self: *Self, node: *Node, typ: ?*Type) !*Type {
    var _fun = node.getBasicFun();
    var ty = (
      typ orelse if (_fun.data.name) |ident| try self.lookupName(ident, true)
      else try self.createFunType(node, node.getType())
    );
    // generic is infer-by-need - performed on call/reference.
    if (node.isBasicFun()) {
      // > 1 implies not in core module
      var alias: ?[]const u8 = null;
      if (self.ctx.modules.len() > 1) {
        // insert the current module alias (import name) into scope so that future
        // references within this function resolves to its type 
        self.insertTy(self.ctx.cm().env.alias, self.ctx.cmt());
        alias = self.ctx.cm().env.alias;
      }
      defer if (alias) |name| {
        self.ctx.typ_scope.remove(name);
      };
      // save this module for stuff
      self.ctx.modules.append(ty.function().data.module orelse self.ctx.cmt(), self.al);
      defer _ = self.ctx.modules.pop();
      var graph = blk: {
        if (self.cunit.lookupFunc(node)) |fg| break :blk fg;
        try self.linker.linkBasicFun(_fun, null);
        break :blk try self.buildFunFlow(node);
      };
      self.cycles.append(node, self.al);
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      const prev_fn = self.current_fn;
      defer self.current_fn = prev_fn;
      if (!_fun.data.allow_all_aspec) self.current_fn = node;
      try self.flowInferEntry(graph.entry());
      _ = self.cycles.pop();
      try self.resolveType(ty.function().data.ret, node.getToken(), self.al);
      if (!_fun.data.trait_fun or !_fun.data.empty_trait_fun) {
        ty.function().data.ret = try self.inferFunReturnType(node, graph, ty);
      }
      _fun.data.body.block().checked = true;
      try self.analyzer.analyzeDeadCode(graph);
      try self.analyzer.analyzeDeadCodeWithTypes(graph.entry());
    }
    return ty;
  }

  fn copyClassFields(self: *Self, fields: NodeItems) ds.ArrayListUnmanaged(*Node) {
    var nodes = ds.ArrayListUnmanaged(*Node).initCapacity(fields.len, self.al);
    nodes.appendSliceAssumeCapacity(fields);
    return nodes;
  }

  fn createClsType(self: *Self, cls: *tir.StructNode, node: *Node) *Type {
    const methods = TypeList.initCapacity(cls.data.methods.len(), self.al);
    return Class.init(
      cls.name.lexeme(), cls.data.tktype, self.copyClassFields(cls.data.fields), methods,
      cls.data.params, node, false, cls.data.builtin, cls.data.public, cls.trait, self.al
    ).toType().box(self.al);
  }

  fn createTraitType(self: *Self, trt: *tir.StructNode, node: *Node) *Type {
    const methods = TypeList.initCapacity(trt.data.methods.len(), self.al);
    return Class.init(
      trt.name.lexeme(), trt.data.tktype, self.copyClassFields(trt.data.fields), methods,
      trt.data.params, node, false, trt.data.builtin, trt.data.public, trt.trait, self.al
    ).toTraitType().box(self.al);
  }

  fn forbidReturnInInit(self: *Self, node: *Node) void {
    // we do not want any return statements in an init method, to aid
    // auto-returning of the self parameter after init() is called
    var list = self.cunit.lookupFunc(node).?.exit().getPrevNeighbours().items();
    for (list) |ngh| {
      for (ngh.node.get().bb.items()) |nd| {
        if (nd.isRet()) {
          self.softErrorFrom(nd,
            "illegal return statement in `init` method",
            .{}
          );
          break;
        }
      }
    }
  }

  fn inferClassPartial(self: *Self, node: *Node) !*Type {
    // we need to infer this first to handle recursive classes
    var cls = &node.NdClass;
    var ty = self.createClsType(cls, node);
    if (!cls.isParameterized() and !cls.data.builtin) {
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      if (cls.data.methods.len() > 0) {
        self.insertVar(ks.SelfVar, ty);
        defer self.deleteVar(ks.SelfVar);
        self.insertCoreType(cls.name.lexeme(), ty);
        for (cls.data.methods.items()) |itm| {
          ty.klass().appendMethodTy(try self.inferFunPartial(itm), self.al);
        }
      }
    }
    // set the class's name to it's full type
    self.insertCoreType(cls.name.lexeme(), ty);
    self.ctx.cm().setIdTy(cls.name.lexeme(), ty, ty.aspec, self.al);
    if (cls.data.builtin) {
      if (self.methods.get(cls.name.lexeme())) |map| {
        // see if we can reuse an existing map
        map.clearRetainingCapacity();
      } else {
        const map = util.box(
          std.AutoHashMap(u32, InferredFunction),
          std.AutoHashMap(u32, InferredFunction).init(self.al),
          self.al,
        );
        self.methods.set(cls.name.lexeme(), map);
      }
    }
    return ty;
  }

  fn inferMethod(self: *Self, cls_ty: *Type, node: *Node, typ: *Type) !*Type {
    var map = self.methods.get(cls_ty.klass().data.name).?;
    const tid = typ.typeid();
    // if this method is in cache, skip inference
    if (map.get(tid)) |info| {
      const fun_name = node.getBasicFun().data.name.?;
      if (info.node.getBasicFun().data.name.?.valueEql(fun_name)) {
        node.* = info.node.*;
        return info.full_ty;
      }
    }
    typ.tid = 0;
    const full_ty = try self.inferFun(node, typ);
    map.put(tid, .{.node = node, .partial_ty = typ, .full_ty = full_ty}) catch {};
    return full_ty;
  }

  fn inferClass(self: *Self, node: *Node, typ: ?*Type, ctparams: []*Type, tparams: []*Type) !*Type {
    var cls = &node.NdClass;
    var ty = typ orelse try self.lookupName(cls.name.toToken(), true);
    // generic is infer-by-need - performed on call/reference.
    if (!cls.isParameterized()) {
      const al = self.al;
      self.cycles.append(node, al);
      self.resolving.append(ty, al);
      defer _ = self.cycles.pop();
      defer _ = self.resolving.pop();
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      const token = node.getToken();
      if (!cls.data.builtin) {
        for (cls.data.fields) |itm| {
          _ = try self.inferClsField(itm);
        }
        ty.klass().data.fields = self.copyClassFields(cls.data.fields);
        if (cls.data.methods.len() > 0) {
          self.insertVar(ks.SelfVar, ty);
          defer self.deleteVar(ks.SelfVar);
          var fun_ty: *Type = undefined;
          for (cls.data.methods.items(), 0..) |itm, i| {
            if (ty.klass().data.methods.len() > i) {
              // use the partially inferred method type if available
              fun_ty = ty.klass().data.methods.itemAt(i);
            } else {
              // otherwise, create one
              fun_ty = try self.inferFunPartial(itm);
              ty.klass().appendMethodTy(fun_ty, al);
            }
            if (!itm.isGenericMtd()) {
              for (fun_ty.function().data.params) |pty| {
                try self.resolveType(pty, token, al);
              }
              ty.klass().data.methods.items()[i] = try self.inferFun(itm, fun_ty);
            } else {
              for (ctparams, tparams) |_tvar, _typ| {
                itm.NdGenericMtd.params.append(.{.tvar = _tvar, .typ = _typ, .from = ty}, al);
              }
              if (ctparams.len == 0) itm.NdGenericMtd.params.append(.{.from = ty}, al);
              ty.klass().data.methods.items()[i] = fun_ty;
            }
          }
          if (ty.klass().getMethod(ks.InitVar)) |mtd| {
            self.forbidReturnInInit(mtd);
          }
        }
        if (cls.trait) |trait| {
          const trt = try self.resolveTraitType(trait, token, al);
          ty.klass().data.trait = trt;
          try self.checkTrait(ty, trt, token);
        }
      } else {
        for (cls.data.fields) |itm| {
          _ = try self.inferClsField(itm);
        }
        ty.klass().data.fields = self.copyClassFields(cls.data.fields);
        for (cls.data.methods.items(), 0..) |itm, i| {
          const fun_ty = try self.inferFunPartial(itm);
          for (fun_ty.function().data.params) |pty| {
            try self.resolveType(pty, token, al);
          }
          ty.klass().appendMethodTy(fun_ty, al);
          ty.klass().data.methods.items()[i] = try self.inferMethod(ty, itm, fun_ty);
        }
        if (cls.trait) |trait| {
          const trt = try self.resolveTraitType(trait, token, al);
          ty.klass().data.trait = trt;
          try self.checkTrait(ty, trt, token);
        }
      }
      cls.data.checked = true;
      ty.klass().resolved = true;
    }
    return ty;
  }

  fn inferTraitPartial(self: *Self, node: *Node) !*Type {
    // we need to infer this first to handle recursive classes
    var trt = &node.NdTrait;
    var ty = self.createTraitType(trt, node);
    if (!trt.isParameterized() and !trt.data.builtin) {
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      if (trt.data.methods.len() > 0) {
        self.insertVar(ks.SelfVar, ty);
        defer self.deleteVar(ks.SelfVar);
        self.insertCoreType(trt.name.lexeme(), ty);
        for (trt.data.methods.items()) |itm| {
          ty.trait().appendMethodTy(try self.inferFunPartial(itm), self.al);
        }
      }
    }
    // set the class's name to it's full type
    self.insertCoreType(trt.name.lexeme(), ty);
    self.ctx.cm().setIdTy(trt.name.lexeme(), ty, ty.aspec, self.al);
    return ty;
  }

  fn inferTrait(self: *Self, node: *Node, typ: ?*Type, ctparams: []*Type, tparams: []*Type) !*Type {
    var trt = &node.NdTrait;
    var ty = typ orelse try self.lookupName(trt.name.toToken(), true);
    // generic is infer-by-need - performed on call/reference.
    if (!trt.isParameterized()) {
      self.cycles.append(node, self.al);
      self.resolving.append(ty, self.al);
      defer _ = self.cycles.pop();
      defer _ = self.resolving.pop();
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      ty.trait().data.fields = self.copyClassFields(trt.data.fields);
      if (trt.data.methods.len() > 0) {
        self.insertVar(ks.SelfVar, ty);
        defer self.deleteVar(ks.SelfVar);
        var fun_ty: *Type = undefined;
        for (trt.data.methods.items(), 0..) |itm, i| {
          if (ty.trait().data.methods.len() > i) {
            // use the partially inferred method type if available
            fun_ty = ty.trait().data.methods.itemAt(i);
          } else {
            // otherwise, create one
            fun_ty = try self.inferFunPartial(itm);
            ty.trait().appendMethodTy(fun_ty, self.al);
          }
          if (!itm.isGenericMtd()) {
            // TODO: resolve param types?
            ty.trait().data.methods.items()[i] = try self.inferFun(itm, fun_ty);
          } else {
            for (ctparams, tparams) |_tvar, _typ| {
              itm.NdGenericMtd.params.append(.{.tvar = _tvar, .typ = _typ, .from = ty}, self.al);
            }
            if (ctparams.len == 0) itm.NdGenericMtd.params.append(.{.from = ty}, self.al);
            ty.trait().data.methods.items()[i] = fun_ty;
          }
        }
        if (ty.trait().getMethod(ks.InitVar) != null) {
          self.softErrorFrom(node,
            "traits cannot have an `init` method",
            .{}
          );
        }
        assert(trt.data.fields.len == 0 and trt.trait == null);
      }
      trt.data.checked = true;
      ty.trait().resolved = true;
    }
    return ty;
  }

  //***** Errors ******//
  inline fn mutationError(self: *Self, m: ?[]const u8, token: Token, ty: *Type) TypeCheckError {
    return self.error_(
      true, token, "Cannot mutate {s} type '{s}'",
      .{m orelse "immutable", self.getTypename(ty)}
    );
  }

  inline fn propertyAccessError(self: *Self, token: Token, typ: *Type, prop: []const u8, emit: bool) TypeCheckError {
    return self.error_(emit, token, "type '{s}' has no property '{s}'", .{self.getTypename(typ), prop});
  }

  pub inline fn typeReferenceError(self: *Self, token: Token, typ: *Type) void {
    self.softError(
      token, "Invalid reference of type '{s}'.", .{self.getTypename(typ)}
    );
  }

  fn checkUnary(self: *Self, node: *tir.UnaryNode, expected: *Type, got: *Type) !void {
    if (got.typeid() != expected.typeid()) {
      const op = node.op.ty.str();
      return self.error_(
        true, node.op.token(),
        "Expected type {s} '{s}' but found {s} '{s}'",
        .{op, self.getTypename(expected), op, self.getTypename(got)}
      );
    }
  }

  fn checkBinary(self: *Self, node: *tir.BinaryNode, node_ty: *Type, source: *Type, narrowed: bool) !void {
    // source is type of rhs
    // node.typ is type of lhs
    const err_token = Token.fromBinaryNode(node);
    if (node.optype() == .OpEqq or node.optype() == .OpNeq or node.optype() == .OpIs) {
      const rc: RelationContext = if (node.optype() == .OpIs) .RCIs else .RCAny;
      if (!node_ty.isEitherWayRelatedTo(source, rc, self.al)) {
        return self.error_(
          true, err_token,
          "Types must be related for this operation.{s}'{s}' is not related to '{s}'",
          .{
            if (!narrowed) " " else " Narrowed type ",
            self.getTypename(node_ty), self.getTypename(source),
          }
        );
      }
      return;
    }
    if (node.optype() == .OpAnd or node.optype() == .OpOr) {
      if (!node_ty.isEitherWayRelatedTo(source, .RCAny, self.al)) {
        return self.error_(
          true, err_token,
          "Types on lhs and rhs of binary operator '{s}' must be related for this operation."
          ++ "{s}'{s}' is not related to '{s}'",
          .{
            node.op_tkty.str(),
            if (!narrowed) " " else " Narrowed type ",
            self.getTypename(node_ty), self.getTypename(source),
          }
        );
      }
      return;
    }
    if (node.op_tkty == .TkPipeGthan) {
      return self.error_(true, err_token, "Invalid pipe application", .{});
    }
    var errTy: ?*Type = null;
    if (!node_ty.typeidEql(&UnitTypes.tyNumber)) {
      errTy = node_ty;
    } else if (!source.typeidEql(&UnitTypes.tyNumber)) {
      errTy = source;
    }
    if (errTy != null) {
      const name = self.getTypename(&UnitTypes.tyNumber);
      const op = node.op_tkty.str();
      return self.error_(
        true, err_token,
        "Expected type '{s}' {s} '{s}' but found '{s}' {s} '{s}'",
        .{
          name, op, name,
          self.getTypename(node_ty), op, self.getTypename(source)
        }
      );
    }
  }

  fn checkCondition(self: *Self, cond_ty: *Type, debug: *Node) !void {
    if (!cond_ty.isBoolTy() and !(cond_ty.isUnion() and cond_ty.isBoolUnionTy())) {
      return self.errorFrom(
        true, debug,
        "Expected condition expression to be of type 'bool' but found '{s}'",
        .{self.getTypename(cond_ty)}
      );
    }
  }

  /// check if an assignment is valid.
  /// debug can be invalid, that is, not a token, as long as emit is false.
  /// Otherwise debug must be a valid Token.
  fn checkAssign(self: *Self, target: *Type, source: *Type, debug: Token, emit: bool) !*Type {
    const typ = target.canBeAssigned(source, .RCAny, self.al);
    return if (typ) |ty| ty else self.error_(emit, debug,
        "Cannot assign type '{s}' to type '{s}'",
        .{self.getTypename(source), self.getTypename(target)}
      );
  }

  fn checkArgumentAssign(self: *Self, target: *Type, source: *Type, debug: Token, comptime msg: [] const u8) !*Type {
    const typ = target.canBeAssigned(source, .RCAny, self.al);
    return if (typ) |ty| ty else self.error_(true, debug,
        msg ++ " '{s}' but found '{s}'",
        .{self.getTypename(target), self.getTypename(source)}
      );
  }

  fn checkInitAssign(self: *Self, target: *Type, source: *Type, debug: Token, emit: bool) !*Type {
    const typ = target.canBeAssigned(source, .RCConst, self.al);
    return if (typ) |ty| ty else self.error_(emit, debug,
      "Cannot initialize type '{s}' with type '{s}'",
      .{self.getTypename(target), self.getTypename(source)}
    );
  }

  inline fn checkArity(self: *Self, found: usize, expected: usize, debug: Token, comptime fmt: [] const u8, args: anytype) !void {
    if (found != expected) {
      return self.error_(true, debug, fmt, args);
    }
  }

  fn checkSubscript(self: *Self, node: *tir.SubscriptNode, expr_ty: *Type, index_ty: *Type) !*Type {
    if (!expr_ty.isListTy() and !expr_ty.isMapTy() and !expr_ty.isTupleTy()) {
      return self.errorFrom(
        true, node.index,
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.klass().tparamsLen() == 0) {
      return self.errorFrom(
        true, node.index,
        "Cannot index empty or non-specialized '{s}' type", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.isListTy() or expr_ty.isTupleTy()) {
      if (!index_ty.isNumTy()) {
        return self.errorFrom(
          true, node.index,
          "Cannot index '{s}' type with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      }
      if (expr_ty.isListTy()) {
        return expr_ty.klass().tparams.?[0];
      } else if (!node.index.isNumberLiteral()) {
        return self.errorFrom(
          true, node.index,
          "tuple index must be a compile-time number literal", .{}
        );
      } else {
        var val = node.index.NdNumber.value;
        if (val < 0) val += @floatFromInt(expr_ty.klass().tparamsLen());
        if (val < 0) return self.errorFrom(true, node.index, "tuple index out of range", .{});
        const index: usize = @intFromFloat(val);
        if (index >= expr_ty.klass().tparamsLen()) {
          return self.errorFrom(true, node.index, "tuple index out of range", .{});
        }
        return expr_ty.klass().tparams.?[index];
      }
    } else if (expr_ty.isMapTy()) {
      // k-v. index with k, get v.
      const cls = expr_ty.klass();
      const key_typ = cls.tparams.?[0];
      const val_typ = cls.tparams.?[1];
      _ = self.checkAssign(key_typ, index_ty, undefined, false) catch {
        return self.errorFrom(
          true, node.index,
          "Cannot index type '{s}' with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      };
      return val_typ;
    }
    unreachable;
  }

  fn checkDeref(self: *Self, node: *tir.DerefNode, expr_ty: *Type) !*Type {
    if (!expr_ty.isTaggedNullable()) {
      return self.errorFrom(
        true, node.expr,
        "Cannot dereference non-nullable type: '{s}'",
        .{self.getTypename(expr_ty)}
      );
    }
    return expr_ty.subtype(self.al);
  }

  fn checkAccessSpecifier(
    self: *Self, typ: *Type, aspec: AccessSpecifier, token: Token,
    prop: []const u8, is_field: bool, is_module: bool,
  ) !void {
    // class method check:
    //  if this method is being accessed outside the class('s methods),
    //  check that the method is defined as public
    // class field check:
    //  if this field is being accessed outside the class('s methods),
    //  check that the field is defined as public
    // error if none of the cases above hold.
    if (aspec.isPrivate()) {
      if (!is_module) {
        if (self.current_fn) |fun| {
          if (fun.getBasicFun().data.name) |name| {
            if (typ.klassOrTrait().getMethod(name.lexeme())) |mth| {
              if (mth == fun) {
                return;
              }
            }
          }
        }
      }
    } else return;
    const thing = if (is_field) "field" else if (is_module) "module property" else "method";
    const container = if (!is_module) "class" else "module";
    return self.softError(
      token,
      "Access of private {s} '{s}' outside its defining {s}.\n    "
      ++ "Consider making this {s} public.",
      .{thing, prop, container, thing}
    );
  }


  fn checkDotAccess(self: *Self, node: *tir.DotAccessNode, cls_ty: *Type, token: Token, prop: []const u8, emit: bool) !*Type {
    var cls = cls_ty.klassOrTrait();
    if (!node.rhs.isNumberLiteral()) {
      if (cls.getField(prop)) |field| {
        const typ = field.getFieldType().?;
        try self.checkAccessSpecifier(cls_ty, AccessSpecifier.getASpec(field.isPubField()), token, prop, true, false);
        node.typ = typ;
        return typ;
      } else if (cls.getMethodTy(prop)) |mth_ty| {
        try self.checkAccessSpecifier(cls_ty, mth_ty.aspec, token, prop, false, false);
        node.typ = mth_ty;
        return mth_ty;
      } else if (cls_ty.isTrait() and cls.data.trait != null) {
        // try to look up the property in the trait inheritance chain
        return self.checkDotAccess(node, cls.data.trait.?, token, prop, false) catch
          self.propertyAccessError(token, cls_ty, prop, emit);
      } else {
        return self.propertyAccessError(token, cls_ty, prop, emit);
      }
    } else {
      const num: usize = @intFromFloat(node.rhs.NdNumber.value);
      if (node.allow_tag_access and num >= 0 and num < cls_ty.klass().data.fields.len()) {
        // transform rhs to the actual property
        if (cls.getFieldAtIndex(num)) |field| {
          const typ = field.getFieldType().?;
          const name = field.getFieldLexeme();
          try self.checkAccessSpecifier(cls_ty, typ.aspec, token, name, true, false);
          node.rhs = self.newIdentNode(name, token);
          node.typ = typ;
          return typ;
        }
      }
      return self.propertyAccessError(token, cls_ty, token.lexeme(), emit);
    }
  }
  
  /// check the validity of this property access, return the resolved type and
  /// whether this is a type or id property
  pub fn checkModuleAccess(self: *Self, mod_ty: *Type, token: Token, prop: []const u8) !struct{*Type, bool} {
    try self.resolveModuleType(mod_ty);
    const mod = mod_ty.module();
    if (mod.getIdTy(prop)) |td| {
      try self.checkAccessSpecifier(mod_ty, td.aspec, token, prop, false, true);
      return .{td.typ, true};
    } else if (mod.getTy(prop)) |td| {
      try self.checkAccessSpecifier(mod_ty, td.aspec, token, prop, false, true);
      return .{td.typ, false};
    } else {
      return self.propertyAccessError(token, mod_ty, prop, true);
    }
  }

  fn checkTagDotAccess(self: *Self, node: *tir.DotAccessNode, tk_val: []const u8, ty: *Type, idx: usize) !*Type {
    var tag = ty.tag();
    if (tag.getField(idx)) |prm| {
      node.typ = prm.typ;
      return prm.typ;
    } else {
      const msg = (
        if (std.mem.eql(u8, tk_val, ks.GeneratedTypeVar))
          "invalid property access"
        else
          "invalid dereference"
      );
      return self.errorFrom(true, node.rhs, "{s}", .{msg});
    }
  }

  fn checkCast(self: *Self, node_ty: *Type, cast_ty: *Type, rc: RelationContext, debug: *Node, emit: bool) TypeCheckError!*Type {
    const ty = node_ty.canBeCastTo(cast_ty, rc, self.al) catch |e| {
      if (e == error.UnionCastError) {
        const active = if (node_ty.isTaggedUnion()) self.getTypename(node_ty.taggedUnion().activeTy().?) else "different";
        return self.errorFrom(
          emit, debug,
          "Cannot cast from type '{s}' to type '{s}' because the active type is '{s}'",
          .{self.getTypename(node_ty), self.getTypename(cast_ty), active}
        );
      }
      return self.errorFrom(
        emit, debug,
        "Cannot cast from type '{s}' to type '{s}'",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    };
    if (ty == node_ty) {
      self.warn(
        emit, debug.getToken(),
        "Could not cast from type '{s}' to type '{s}' because the active type is unknown",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    }
    return ty;
  }

  fn checkDuplicateTraitMethods(self: *Self, trt1: *tir.Trait, trt2: *tir.Trait) !void {
    for (trt1.data.methods.items()) |tm| {
      const name = tm.function().getName();
      if (trt2.getMethod(name.lexeme())) |mth| {
        self.softError(name, "duplicate trait method '{s}'", .{name.lexeme()});
        self.softErrorFmt(mth.getToken(), 4, 2, "Method already defined here:", .{});
      }
    }
  }

  fn _checkTrait(self: *Self, ty: *Type, trait: *Type, debug: Token) TypeCheckError!void {
    // `ty` may be a Class or Trait
    if (trait.isUnion()) {
      var disamb = ds.StringHashMap(Token).init(self.al);
      for (trait.union_().variants.values()) |trt| {
        for (trt.trait().data.methods.items()) |mth| {
          const token = mth.function().data.node.?.getToken();
          if (disamb.get(token.lexeme())) |tk| {
            self.softError(token, "duplicate trait method '{s}'", .{token.lexeme()});
            self.softErrorFmt(tk, 4, 2, "Method already defined here:", .{});
          } else {
            disamb.set(token.lexeme(), token);
          }
        }
        try self._checkTrait(ty, trt, debug);
      }
      return;
    }
    if (!trait.isTrait()) {
      return self.error_(
        true, debug, "Expected trait type but found '{s}'",
        .{self.getTypename(trait)}
      );
    }
    if (!trait.trait().isParameterized()) {
      if (trait.trait().data.node) |nd| {
        if (!self.isCheckedTrait(nd) and !self.cycles.contains(nd, Node.eql)) {
          _ = try self.inferTrait(nd, trait, &[_]*Type{}, &[_]*Type{});
        }
      }
    }
    if (ty.isClass()) {
      var cls = ty.klass();
      var trt = trait.trait();
      if (cls.data.node == null) {
        // this issue arises from generic classes with complicated traits spec
        self.softError(debug,
          "I cannot resolve the trait type '{s}' for class '{s}'",
          .{self.getTypename(trait), self.getTypename(ty)}
        );
        return self.errorFmt(debug, 4, 2,
          "This trait may have been specified in a way that is "
          ++ "too complicated for my resolution process.", .{}
        );
      }
      const cls_token = cls.data.node.?.getToken();
      for (trt.data.methods.items()) |tm| {
        var _node = tm.function().data.node.?;
        var fun = _node.getBasicFun();
        const name = fun.data.name.?.lexeme();
        var err = false;
        if (cls.data.trait) |t| {
          if (t.hasVariableSafe(self.al)) {
            // this type is still being resolved
            logger.debug("resolving type t in t, where t={s}", .{self.getTypename(t)});
          } else if (!trait.isRelatedTo(t, .RCAny, self.al)) {
            err = true;
          }
        } else {
          err = true;
        }
        if (err) {
          self.softError(cls_token,
              "type '{s}' does not implement the trait '{s}'.",
              .{self.getTypename(ty), self.getTypename(trait)}
            );
          self.softErrorFmt(debug, 4, 2, "This error was triggered from here:", .{});
        } else if (cls.getMethodTy(name)) |cm| {
          const token = cm.function().data.node.?.getToken();
          const revert = self.diag.count();
          _ = self.checkArgumentAssign(tm, cm, token, "Expected method") catch {
            if (_node.isGenericMtd()) {
              self.diag.popUntil(revert);
              self.softError(debug,
                "Generic trait methods are experimental and may not be overriden.",
                .{}
              );
              continue;
            }
            return self.errorFmt(debug, 4, 2, "This error was triggered from here:", .{});
          };
          if (tm.aspec != cm.aspec) {
            self.softError(token,
              "method '{s}' has a visibility different from its trait specification.",
              .{name}
            );
            self.softErrorFmt(fun.data.name.?, 4, 2, "Trait method specified here:", .{});
          }
        } else if (!fun.data.empty_trait_fun) {
          var mth_ty = tm.clone(self.al);
          if (!_node.isGenericMtd()) {
            cls.appendMethodTyAndNode(mth_ty, self.al);
          } else {
            // set `from` for this generic method to the class type
            var gmtd = &mth_ty.function().data.node.?.NdGenericMtd;
            for (0..gmtd.params.len()) |i| {
              gmtd.params.items()[i].from = ty;
            }
            cls.appendMethodTyAndNode(mth_ty, self.al);
          }
        } else {
          self.missing_tmtds.append(.{name, self.getTypename(tm), cls_token});
        }
      }
      assert(trt.data.trait == null);
    } else if (ty.isTrait()) {
      try self.checkDuplicateTraitMethods(ty.trait(), trait.trait());
    } else {
      return self.error_(
        true, debug, "Expected trait or class type but found '{s}'",
        .{self.getTypename(ty)}
      );
    }
  }

  fn checkTrait(self: *Self, ty: *Type, trait: *Type, debug: Token) TypeCheckError!void {
    self.missing_tmtds.clearRetainingCapacity();
    var err = false;
    self._checkTrait(ty.classOrInstanceClass(), trait, debug) catch {
      err = true;
    };
    if (self.missing_tmtds.isEmpty()) {
      if (err) return error.CheckError;
      return;
    }
    const items = self.missing_tmtds.items();
    const token = items[0].@"2";
    const len = self.diag.count();
    self.softError(token,
      "type '{s}' does not satisfy the trait constraint(s) of '{s}':",
      .{self.getTypename(ty), self.getTypename(trait)}
    );
    // diag deduplicates error msgs so if this error was encountered before
    // nothing would be added so just return
    if (self.diag.count() == len) return;
    self.diag.addDiagnosticsDirect(token, "  The following method(s) are not implemented:\n");
    for (items) |item| {
      self.diag.addDiagnosticsSliceDirect(token, &[_][]const u8{"    ", item.@"0", " : ", item.@"1", "\n"});
    }
    self.diag.addDiagnosticsDirect(debug, "\n");
    self.softErrorFmt(debug, 4, 2, "This error was triggered from here:", .{});
  }

  fn infer(self: *Self, node: *Node) TypeCheckError!*Type {
    return switch (node.*) {
      .NdPubVarDecl => |*nd| {
        var ty = try self.infer(nd.decl);
        ty.aspec = .SpecPublic;
        self.ctx.cm().setIdTy(nd.getVarDecl().name.lexeme(), ty, .SpecPublic, self.al);
        return ty;
      },
      .NdConstVarDecl => |*nd| {
        var ty = try self.inferVarDecl(nd);
        ty.mutable = false;
        return ty;
      },
      .NdBasicCall, .NdGenericCall, => {
        var call = node.toCallNode();
        const ty = try self.inferCall(&call, node);
        node.getBasicCall().copyCall(&call, ty);
        return ty;
      },
      .NdExprStmt => |*nd| self.inferExprStmt(nd),
      .NdVarDecl => |*nd| self.inferVarDecl(nd),
      .NdNumber => |*nd| self.inferLiteral(UnitTypes.num, nd.lexeme(self.al), nd),
      .NdBool => |*nd| self.inferLiteral(UnitTypes.bol, nd.token.lexeme(), nd),
      .NdString => |*nd| self.inferLiteral(UnitTypes.str, nd.token.lexeme(), nd),
      .NdParam => |*nd| self.inferParam(nd, node),
      .NdAssign => |*nd| self.inferAssign(nd),
      .NdTVar => |*nd| self.inferVar(nd, true),
      .NdList => |*nd| self.inferList(nd),
      .NdTuple => |*nd| self.inferTuple(nd),
      .NdMap => |*nd| self.inferMap(nd),
      .NdType => |*nd| self.inferType(nd),
      .NdAlias => |*nd| self.inferAlias(nd),
      .NdRet => |*nd| self.inferRet(nd),
      .NdScope => |*nd| self.inferScope(nd),
      .NdError => |*nd| self.inferError(nd, null),
      .NdFailMarker => |*nd| self.inferFail(nd),
      .NdRedunMarker => |*nd| self.inferRedundant(nd),
      .NdDiagStartMarker => |*nd| self.checkMatchDiagStart(nd),
      .NdUnary => |*nd| self.inferUnary(nd),
      .NdBinary => |*nd| self.inferBinary(nd),
      .NdCast => |*nd| self.inferCast(nd, node),
      .NdSubscript => |*nd| self.inferSubscript(nd, false),
      .NdDotAccess => |*nd| self.inferDotAccess(nd, node, false),
      .NdDeref => |*nd| self.inferDeref(nd, false),
      .NdBasicFun => self.inferFun(node, null),
      .NdCondition, .NdMCondition, .NdEmpty, .NdControl => undefined,
      .NdSimpleIf, .NdLblArg, .NdBlock, .NdMatch,
      .NdWhile, .NdProgram, .NdClass, .NdGenericFun => unreachable,
      else => self.void_ty,
    };
  }

  fn buildFunFlow(self: *Self, node: *Node) !*FlowGraph {
    try self.des.desugarFun(node);
    var graph = self.builder.buildFun(node);
    self.cunit.putFunc(graph);
    try self.analyzer.analyzeDeadCode(graph);
    return graph;
  }

  fn buildImpProgramFlow(self: *Self, node: *tir.ImportNode, display_diag: bool) !*Type {
    const path = node.program.NdProgram.filepath;
    if (self.modules.get(path)) |info| {
      return info.typ;
    }
    const root = node.program;
    _ = try self.des.desugar(root);
    const program = self.builder.buildProgram(root);
    self.analyzer.analyzeDeadCode(program) catch |e| {
      if (display_diag) self.diag.display();
      return e;
    };
    var cunit = CompUnit.init(self.al);
    cunit.program = program;
    const typ = Type.newModule(root, node.getImportName(), self.al);
    self.modules.set(path, .{.cunit = cunit, .typ = typ}, self.al);
    return typ;
  }

  fn buildProgramFlow(self: *Self, root: *Node, display_diag: bool) !void {
    _ = try self.des.desugar(root);
    self.cunit = CompUnit.init(self.al);
    self.cunit.program = self.builder.buildProgram(root);
    self.analyzer.analyzeDeadCode(self.cunit.program) catch |e| {
      if (display_diag) self.diag.display();
      return e;
    };
  }

  fn setupEntryModule(self: *Self, root: *Node, display_diag: bool) !*Type {
    try self.buildProgramFlow(root, display_diag);
    const typ = Type.newModule(root, root.NdProgram.getModuleName(), self.al);
    self.modules.set(root.NdProgram.filepath, .{.cunit = self.cunit, .typ = typ}, self.al);
    return typ;
  }

  pub fn typecheck(self: *Self, root: *Node, display_diag: bool) TypeCheckError!void {
    defer {
      if (self.diag.hasAny()) {
        if (display_diag) self.diag.display();
      }
    }
    self.ctx = TContext.init(self.al);
    self.ctx.enterScope();
    self.linker.ctx = &self.ctx;
    self.linker.u8w = &self.u8w;
    self.linker.tc = self;
    self.loadPrelude();
    const typ = try self.setupEntryModule(root, display_diag);
    self.flowInferProgram(self.cunit, typ);
    if (self.diag.hasErrors()) {
      return error.CheckError;
    }
  }
};
