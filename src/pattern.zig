const std = @import("std");
const tir = @import("tir.zig");
const ks = @import("constants.zig");
const ds = @import("ds.zig");
const util = @import("util.zig");
const check = @import("check.zig");
const diagnostics = @import("diagnostics.zig");

const Allocator = tir.Allocator;
const Type = tir.Type;
const TokenType = tir.TokenType;
const Token = tir.Token;
const Node = tir.Node;
const TypeChecker = check.TypeChecker;
const Diagnostic = diagnostics.Diagnostic;
const U8Writer = util.U8Writer;
const addDepth = util.addDepth;
const logger = check.logger;
const PSlice = []*Pattern;
pub const Patterns = ds.ArrayList(*Pattern);

/// C(p1...pn)
pub const Constructor = struct {
  /// is a builtin constructor
  builtin: bool,
  /// has rest pattern
  rested: bool,
  /// from map pattern
  from_map: bool = false,
  /// token type corresponding to `name`
  tktype: TokenType,
  tag: ConsTag,
  args: PSlice,
  name: []const u8,
  node: ?*Node = null,

  pub fn init(name: []const u8, tktype: TokenType, builtin: bool, tag: ConsTag, rested: bool) @This() {
    return .{.name = name, .builtin = builtin, .tag = tag, .rested = rested, .args = &[_]*Pattern{}, .tktype = tktype};
  }

  pub fn append(self: *Constructor, pattern: *Pattern, al: Allocator) void {
    const items = util.allocSlice(*Pattern, self.args.len + 1, al);
    @memcpy(items[0..self.args.len], self.args);
    items[self.args.len] = pattern;
    self.args = items;
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    return std.mem.eql(u8, self.cname(), other.cname());
  }

  pub fn hasField(self: *@This()) bool {
    for (self.args) |arg| {
      if (arg.alat.hasField()) {
        return true;
      }
    }
    return false;
  }

  pub fn newOrCons() @This() {
    return @This().init("$OR", .TkIdent, true, .Or, false);
  }

  pub fn newListCons() @This() {
    return @This().init(ks.ListVar, .TkList, true, .List, false);
  }

  pub fn newTupleCons() @This() {
    return @This().init(ks.TupleVar, .TkTuple, true, .Tuple, false);
  }

  pub fn newMapCons() @This() {
    return @This().init(ks.MapVar, .TkMap, true, .Map, false);
  }
  
  pub fn newClassCons(name: []const u8, tktype: TokenType, node: *Node) @This() {
    var this = @This().init(name, tktype, false, .Other, false);
    this.node = node;
    return this;
  }

  pub fn newLiteralCons(node: *Node, al: Allocator) @This() {
    var name = switch (node.*) {
      .NdBool => |*lit| lit.token.lexeme(),
      .NdString => |*lit| lit.token.lexeme(),
      .NdNumber => |*lit| lit.lexeme(al),
      else => unreachable
    };
    var this = @This().init(name, .TkIdent, true, .Literal, false);
    this.node = node;
    return this;
  }

  pub inline fn cname(self: *@This()) []const u8 {
    return self.name;
  }

  pub fn toVariant(self: @This(), al: Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.cons = self}, al);
  }

  pub fn clone(self: *@This(), al: Allocator) @This() {
    var new = @This().init(self.name, self.tktype, self.builtin, self.tag, self.rested);
    var args = Patterns.initCapacity(self.args.len, al);
    for (self.args) |arg| {
      args.appendAssumeCapacity(arg.clone(al));
    }
    new.from_map = self.from_map;
    new.args = args.items();
    if (self.node) |nd| {
      new.node = nd.clone(al);
    }
    return new;
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    if (self.tag == .Literal and !std.mem.startsWith(u8, self.name, "lit")) {
      _ = try writer.write("lit.");
    }
    _ = try writer.write(self.name);
    if (self.args.len == 0) {
      _ = try writer.write("()");
      return;
    }
    _ = try writer.write("(\n");
    const len = self.args.len -| 1;
    for (self.args, 0..) |itm, i| {
      try addDepth(&writer, depth + 2);
      try itm.render(depth + 2, u8w);
      if (i < len) _ = try writer.write(",\n");
    }
    _ = try writer.write("\n");
    try addDepth(&writer, depth + 4);
    _ = try writer.write(")");
  }
};

/// x => E
pub const Variable = struct {
  ident: *Node,

  pub fn init(ident: *Node) @This() {
    return .{.ident = ident};
  }

  pub fn toVariant(self: @This(), al: Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.vari = self}, al);
  }

  pub fn clone(self: @This(), al: Allocator) @This() {
    return .{.ident = self.ident.clone(al)};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Variable(");
    _ = try writer.write(self.ident.NdTVar.value());
    _ = try writer.write(")");
  }
};

/// _ => E
pub const Wildcard = struct {
  /// for error reporting
  token: Token,
  generated: bool,
  typ: ?*Type = null,

  pub fn init(token: Token, generated: bool) @This() {
    return .{.token = token, .generated = generated};
  }

  pub fn toVariant(self: @This(), al: Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.wildc = self}, al);
  }

  pub fn clone(self: @This(), al: Allocator) @This() {
    _ = al;
    return self;
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = self;
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Wildcard(_)");
  }
};

/// idi is Ci
pub const Relation = struct {
  ident: *Node, // TVarNode
  pattern: *Pattern,

  pub fn varRelationToVarDecl(self: *const @This(), al: Allocator) *Node {
    // a is y -> let y = a
    return Node.new(.{
      .NdVarDecl = tir.VarDeclNode.init(self.pattern.variant.vari.ident.NdTVar.token, self.ident, null)
    }, al);
  }

  pub fn clone(self: @This(), al: Allocator) @This() {
    return .{.ident = self.ident, .pattern = self.pattern.clone(al)};
  }

  pub fn toVariant(self: @This(), al: Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.rel = self}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    _ = u8w;
    _ = self;
    _ = depth;
    unreachable;
  }
};

/// id1 is C1, ..., idn is Cn
pub const MultiRelation = struct {
  relations: RelationList,

  pub const RelationList = ds.ArrayListUnmanaged(Relation);

  pub fn init() @This() {
    return .{.relations = RelationList.init()};
  }

  pub fn initCapacity(cap: usize, al: Allocator) @This() {
    return .{.relations = RelationList.initCapacity(cap, al)};
  }

  pub fn toVariant(self: @This(), al: Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.mrel = self}, al);
  }

  pub fn clone(self: *@This(), al: Allocator) @This() {
    return .{.relations = self.relations.clone(al)};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    _ = u8w;
    _ = self;
    _ = depth;
    unreachable;
  }
};

/// a pattern in a pattern clause of P -> A
pub const Pattern = struct {
  variant: *MatchVariant,
  alat: AliasOrAttr,
  token: Token,

  /// attribute or alias
  pub const AliasOrAttr = struct {
    alias: ?*Node = null,
    field: ?*Node = null,

    pub fn init(alias: ?*Node, field: ?*Node) @This() {
      return .{.alias = alias, .field = field};
    }

    pub fn hasAlias(self: *const @This()) bool {
      return self.alias != null;
    }

    pub fn hasField(self: *const @This()) bool {
      return self.field != null;
    }

    pub fn clone(self: @This(), al: Allocator) @This() {
      const field = if (self.field) |field| field.clone(al) else self.field;
      const alias = if (self.alias) |alias| alias.clone(al) else self.alias;
      return .{.alias = alias, .field = field};
    }
  };

  pub fn init(variant: *MatchVariant, token: Token, capture: AliasOrAttr) @This() {
    return .{.variant = variant, .token = token, .alat = capture};
  }

  pub fn isVariable(self: *@This()) bool {
    return switch (self.variant.*) {
      .vari => true,
      else => false,
    };
  }

  pub fn isWildcard(self: *@This()) bool {
    return switch (self.variant.*) {
      .wildc => true,
      else => false,
    };
  }

  pub fn isRelation(self: *@This()) bool {
    return switch (self.variant.*) {
      .rel => true,
      else => false,
    };
  }

  pub fn isMultiRelation(self: *@This()) bool {
    return switch (self.variant.*) {
      .mrel => true,
      else => false,
    };
  }

  pub fn isConstructor(self: *@This()) bool {
    return switch (self.variant.*) {
      .cons => true,
      else => false,
    };
  }

  pub fn isOrConstructor(self: *@This()) bool {
    return switch (self.variant.*) {
      .cons => |cons| cons.tag == .Or,
      else => false,
    };
  }

  pub inline fn box(self: @This(), al: Allocator) *Pattern {
    return util.box(Pattern, self, al);
  }

  pub fn clone(self: *@This(), al: Allocator) *@This() {
    return (@This(){
        .variant = self.variant.clone(al),
        .alat = self.alat.clone(al),
        .token = self.token
      }).box(al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    try self.variant.render(depth, u8w);
    var writer = u8w.writer();
    if (self.alat.hasField()) {
      _ = try writer.write(" as-field ");
      _ = try writer.write(self.alat.field.?.NdTVar.value());
    }
    if (self.alat.hasAlias()) {
      _ = try writer.write(" as ");
      _ = try writer.write(self.alat.alias.?.NdTVar.value());
    }
  }
};

/// a body of actions
pub const Body = struct {
  /// the main body - in reverse order if `reversed`
  node: *Node,
  decls: tir.NodeListU,

  pub fn init(node: *Node, rev: bool, al: Allocator) @This() {
    _ = rev;
    var block: *Node = undefined;
    if (node.isBlock()) {
      block = node;
    } else {
      block = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{node}), al);
    }
    return .{.node = block, .decls = tir.NodeListU.init()};
  }

  pub fn addNode(self: *@This(), node: *Node, al: Allocator) void {
    return self.decls.append(node, al);
  }

  pub fn clone(self: *@This(), al: Allocator) @This() {
    return .{.node = self.node.clone(al), .decls = self.decls.clone(al)};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var block = self.node.block();
    const len = block.nodes.len;
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    if (len > 0) {
      _ = try writer.write("body(\n");
      for (0..len) |i| {
        const node = block.nodes[len - i - 1];
        try addDepth(&writer, (depth + 2) * 2 + 1);
        if (node.isVarDecl()) {
          const vd = node.NdVarDecl;
          const source = if (vd.value.isTVariable()) vd.value.NdTVar.value() else "$expr";
          const str = std.fmt.allocPrint(u8w.allocator(), "let {s} = {s}\n", .{vd.name.lexeme(), source}) catch unreachable;
          _ = try writer.write(str);
        } else {
          _ = try writer.write("[node]\n");
        }
      }
      try addDepth(&writer, depth + 1);
      _ = try writer.write(")");
    } else {
      _ = try writer.write("body()");
    }
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    _ = parent;
    _ = skip_cons_test;
    return t.tLeaf(self, false);
  }
};

/// from Maranget's (with gaurds added from Yorick's)
const Result = union(enum) {
  leaf: Leaf,
  fail: Fail,
  swch: Switch,
  gard: Guard,

  pub fn isSwitch(self: *@This()) bool {
    return switch (self.*) {
      .swch => true,
      else => false,
    };
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    switch (self.*) {
      inline else => |*this| try this.render(depth, u8w),
    }
  }

  pub fn clone(self: *@This(), al: Allocator) *@This() {
    return switch (self.*) {
      .leaf => |*nd| util.box(Result, .{.leaf = nd.clone(al)}, al),
      inline else => |*nd| nd.clone(al),
    };
  }
};

/// Leaf(k) -> success(k)
const Leaf = Body;

/// Failure
const Fail = struct {

  fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = self;
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Fail");
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    _ = parent;
    _ = skip_cons_test;
    return t.tFail(self);
  }

  fn clone(self: *@This(), al: Allocator) *Result {
    return util.box(Result, .{.fail = self.*}, al);
  }
};

/// Guard -> if cond => ..
pub const Guard = struct {
  cond: *Node,
  body: Leaf,
  fallback: *Result,

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Guard(cond,\n");
    try addDepth(&writer, (depth + 1) * 2);
    _ = try writer.write("body = (\n");
    try addDepth(&writer, (depth + 1) * 2);
    try self.body.render(depth + 3, u8w);
    try addDepth(&writer, depth + 2);
    _ = try writer.write("),\n");
    try addDepth(&writer, (depth + 1) * 2);
    _ = try writer.write("fallback = (\n");
    try addDepth(&writer, (depth + 1) * 2);
    try self.fallback.render(depth + 3, u8w);
     try addDepth(&writer, depth + 2);
    _ = try writer.write(")\n");
    try addDepth(&writer, depth + 1);
    _ = try writer.write(")\n");
  }

  fn clone(self: *@This(), al: Allocator) *Result {
    const cln: Guard = .{
      .cond = self.cond.clone(al),
      .body = self.body.clone(al),
      .fallback = self.fallback.clone(al)
    };
    return util.box(Result, .{.gard = cln}, al);
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    return t.tGuard(self, parent, skip_cons_test);
  }
};

/// Switch o (L) -> multiway test, o is an occurrence
const Switch = struct {
  /// occurrence
  occ: *Node,
  /// branch
  branches: [2]Branch,
  /// error location token
  token: Token,

  pub fn init(expr: *Node, token: Token) @This() {
    return .{.occ = expr, .branches = undefined, .token = token};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Switch (");
    _ = try writer.write(self.occ.NdTVar.value());
    _ = try writer.write(",\n");
    for (self.branches[0..]) |*branch| {
      try addDepth(&writer, depth + 2);
      try branch.render(depth + 2, u8w);
      _ = try writer.write("\n");
    }
    try addDepth(&writer, depth + 1);
    _ = try writer.write(")\n");
  }

  fn clone(self: *@This(), al: Allocator) *Result {
    var swch = @This().init(self.occ.clone(al), self.token);
    swch.branches[0] = self.branches[0].clone(al);
    swch.branches[1] = self.branches[1].clone(al);
    return util.box(Result, .{.swch = swch}, al);
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    return t.tSwitch(self, parent, skip_cons_test);
  }
};

/// Cons | Wildcard
const Test = union (enum) {
  cons: Constructor,
  wildc: Wildcard,

  fn isConstructor(self: *const @This()) bool {
    return switch (self.*) {
      .cons => true,
      else => false,
    };
  }

  fn isWildcard(self: *const @This()) bool {
    return switch (self.*) {
      .wildc => true,
      else => false,
    };
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    switch (self.*) {
      .cons => |*cons| try cons.render(depth, u8w),
      .wildc => |*wc| try wc.render(depth, u8w),
    }
  }
};

/// Branch
///  -> {Cons | Wildcard : Switch | Guard | Leaf | Fail}
const Branch = struct {
  /// Cons | Wildcard
  lhs: Test,
  /// Switch | Guard | Leaf | Fail
  rhs: *Result,

  fn clone(self: *@This(), al: Allocator) @This() {
    const lhs: Test = (
      if (self.lhs.isConstructor()) .{.cons = self.lhs.cons.clone(al)}
      else .{.wildc = self.lhs.wildc.clone(al)}
    );
    return .{.lhs = lhs, .rhs = self.rhs.clone(al)};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("test(\n");
    try addDepth(&writer, depth + 1);
    try Test.render(&self.lhs, depth + 2, u8w);
    _ = try writer.write(",\n");
    try addDepth(&writer, depth + 2);
    try self.rhs.render(depth + 2, u8w);
    try addDepth(&writer, depth + 1);
    _ = try writer.write(")");
  }
};

pub const DecisionTree = Result;

/// each matchable variant
pub const MatchVariant = union (enum) {
  cons: Constructor,
  rel: Relation,
  mrel: MultiRelation,
  vari: Variable,
  wildc: Wildcard,

  pub fn clone(self: *@This(), al: Allocator) *@This() {
    const variant: MatchVariant = switch (self.*) {
      .cons => |*c| .{.cons = c.clone(al)},
      .mrel => |*m| .{.mrel = m.clone(al)},
      .rel => |r| .{.rel = r.clone(al)},
      .vari => |v| .{.vari = v.clone(al)},
      .wildc => |w| .{.wildc = w.clone(al)},
    };
    return util.box(MatchVariant, variant, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    switch (self.*) {
      inline else => |*this| try this.render(depth, u8w),
    }
  }
};

/// Builtin Constructors
pub const ConsTag = enum (u8) {
  Or,
  List,
  Tuple,
  Map,
  Literal,
  Other,
};

/// case P1...Pn [guard] => E
pub const Case = struct {
  pattern: *Pattern,
  guard: ?*Node,
  body: Body,

  pub const CaseList = ds.ArrayList(*Case);

  pub fn init(pattern: *Pattern, guard: ?*Node, body: *Node, rev: bool, al: Allocator) @This() {
    return .{.pattern = pattern, .guard = guard, .body = Body.init(body, rev, al)};
  }

  pub fn new(pattern: *Pattern, guard: ?*Node, body: *Node, rev: bool, al: Allocator) *@This() {
    const case = @This().init(pattern, guard, body, rev, al);
    return util.box(Case, case, al);
  }

  pub fn from(self: *@This(), pat: *Pattern, al: Allocator) *@This() {
    const guard = if (self.guard) |guard| guard.clone(al) else self.guard;
    return util.box(Case, .{.pattern = pat, .guard = guard, .body = self.body.clone(al)}, al);
  }

  pub fn clone(self: *@This(), al: Allocator) *@This() {
    const guard = if (self.guard) |guard| guard.clone(al) else self.guard;
    const case = @This(){.pattern = self.pattern.clone(al), .guard = guard, .body = self.body.clone(al)};
    return util.box(Case, case, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var writer = u8w.writer();
    try self.pattern.render(depth, u8w);
    if (self.guard) |_| {
      _ = try writer.write(" If [guard]");
    }
    _ = try writer.write(" => ");
    try self.body.render(depth, u8w);
    _ = try writer.write("\n");
  }
};

pub const MatchCompiler = struct {
  allocator: Allocator,
  diag: *Diagnostic,
  namegen: util.NameGen,
  u8w: U8Writer,
  /// store (body) nodes that may/may not have been included in the decision ir
  failure: ds.ArrayHashMapUnmanaged(*Node, FailStatus),
  /// select a constructor to test based on some heuristic
  heuristic: SelectionHeuristic,
  /// transform a decision ir into lowered form
  dtt: DecisionTreeTransformer,

  const Self = @This();
  pub const CaseList = Case.CaseList;
  pub const MatchError = error{MatchError};

  /// determine if the body of this case was included in the decision ir or not.
  /// every Body included in the decision ir / result ends up being removed from
  /// `self.failure`, that is, `removed` here is set to false. 
  const FailStatus = struct {
    removed: bool,
    case: *Case,

    pub fn init(removed: bool, case: *Case) @This() {
      return .{.removed = removed, .case = case};
    }
  };

  /// heuristics for selecting a constructor in the pattern matrix to match on
  const SelectionHeuristic = struct {
    row: RowMap,
    col: ColumnMap,
    disamb: std.StringHashMap(u32),
    /// store a constructor's row and it's total frequency in the pattern matrix
    const Row = struct {pos: usize, count: usize};
    /// store a constructor's name and its column position
    const Column = struct {name: []const u8, column: usize};

    const RowMap = std.StringHashMap(Row);
    const ColumnMap = std.HashMap(Column, usize, ColumnContext, std.hash_map.default_max_load_percentage);

    const ColumnContext = struct {
      pub fn hash(this: @This(), s: Column) u64 {
        _ = this;
        var buf: [256]u8 = undefined;
        var str = std.fmt.bufPrint(&buf, "{s}{}", .{s.name, s.column}) catch unreachable;
        return std.hash_map.hashString(str);
      }
      pub fn eql(this: @This(), a: Column, b: Column) bool {
        _ = this;
        return a.column == b.column and std.mem.eql(u8, a.name, b.name);
      }
    };

    fn init(al: Allocator) @This() {
      return .{
        .row = RowMap.init(al),
        .col = ColumnMap.init(al),
        .disamb = std.StringHashMap(u32).init(al)
      };
    }

    /// save this constructor, its position (row) in the pattern matrix, and its frequency
    /// of occurrence in the matrix
    fn addRow(self: *@This(), cons: *Constructor, i: usize) void {
      const name = cons.cname();
      if (self.row.get(name)) |row| {
        if (row.pos != i) {
          self.row.put(name, .{.pos = row.pos, .count = row.count + 1}) catch {};
        }
      } else {
        self.row.put(name, .{.pos = i, .count = 1}) catch {};
      }
    }

    /// save this constructor's column in the pattern matrix, and its frequency of occurrence
    /// accross the column, in the matrix
    fn addColumn(self: *@This(), cons: *Constructor, i: usize) void {
      if (self.col.getEntry(.{.name = cons.cname(), .column = i})) |entry| {
        self.col.put(entry.key_ptr.*, entry.value_ptr.* + 1) catch {};
      } else {
        self.col.put(.{.name = cons.cname(), .column = i}, 1) catch {};
      }
    }

    /// check along a given row, if there are multiple constructors that share the same
    /// name with the current best constructor - `best`
    fn isATiedConstructor(self: *@This(), mrel: *MultiRelation, best: []const u8) bool {
      for (mrel.relations.items()) |rel| {
        if (rel.pattern.isConstructor()) {
          const cons_name = rel.pattern.variant.cons.cname();
          if (self.disamb.get(cons_name)) |val| {
            self.disamb.put(cons_name, val + 1) catch {};
          } else {
            self.disamb.put(cons_name, 1) catch {};
          }
          if (self.disamb.get(best)) |val| {
            if (val > 1) {
              return true;
            }
          }
        }
      }
      return false;
    }

    fn selectTest(self: *@This(), cases: *CaseList) usize {
      // select a test in the first clause, that is present in the maximum number of other clauses
      // if the first case has a pattern that isn't a multirelation, there's no choice
      if (!cases.itemAt(0).pattern.isMultiRelation()) return 0;
      defer self.row.clearRetainingCapacity();
      for (cases.items(), 0..) |case, i| {
        var pat = case.pattern;
        if (pat.isMultiRelation()) {
          for (pat.variant.mrel.relations.items()) |rel| {
            if (rel.pattern.isConstructor()) {
              self.addRow(&rel.pattern.variant.cons, i);
            }
          }
        } else if (pat.isRelation()) {
          if (pat.variant.rel.pattern.isConstructor()) {
            self.addRow(&pat.variant.rel.pattern.variant.cons, i);
          }
        } else if (pat.isConstructor() and pat.variant.cons.tag != .Or) {
          self.addRow(&pat.variant.cons, i);
        }
      }
      var maxc: usize = 0;
      var idx: usize = 0;
      var case = cases.itemAt(0);
      var mrel = &case.pattern.variant.mrel;
      var best: []const u8 = "";
      // try to find the best constructor in the first case clause:
      // select a constructor from the first clause that has the highest occurrence in other clauses
      for (mrel.relations.items(), 0..) |rel, i| {
        if (rel.pattern.isConstructor()) {
          const cons_name = rel.pattern.variant.cons.cname();
          if (self.row.get(cons_name)) |row| {
            if (row.count > maxc) {
              maxc = row.count;
              best = cons_name;
              idx = i;
            }
          }
        }
      }
      if (self.isATiedConstructor(mrel, best)) {
        // there are multiple constructors that share the same name with the best constructor.
        // We need to find a tie breaker by picking a constructor in the best column.
        defer self.col.clearRetainingCapacity();
        for (cases.items()) |_case| {
          var pat = _case.pattern;
          if (pat.isMultiRelation()) {
            for (pat.variant.mrel.relations.items(), 0..) |rel, j| {
              if (rel.pattern.isConstructor()) {
                self.addColumn(&rel.pattern.variant.cons, j);
              }
            }
          } else if (pat.isRelation()) {
            if (pat.variant.rel.pattern.isConstructor()) {
              self.addColumn(&pat.variant.rel.pattern.variant.cons, 0);
            }
          } else if (pat.isConstructor() and pat.variant.cons.tag != .Or) {
            self.addColumn(&pat.variant.cons, 0);
          }
        }
        var nidx: usize = 0;
        var nbest: []const u8 = "";
        var nmaxc: usize = 0;
        for (mrel.relations.items(), 0..) |rel, i| {
          if (rel.pattern.isConstructor()) {
            const cons_name = rel.pattern.variant.cons.cname();
            if (self.col.get(.{.name = cons_name, .column = i})) |count| {
              if (count > nmaxc) {
                nmaxc = count;
                nidx = i;
                nbest = cons_name;
              }
            }
          }
        }
        if (std.mem.eql(u8, best, nbest)) {
          return nidx;
        }
      }
      return idx;
    }
  };

  pub fn init(diag: *Diagnostic, al: Allocator) @This() {
    return .{
      .allocator = al,
      .diag = diag,
      .failure = ds.ArrayHashMapUnmanaged(*Node, FailStatus).init(),
      .heuristic = SelectionHeuristic.init(al),
      .namegen = util.NameGen.init(al),
      .dtt = DecisionTreeTransformer.init(al),
      .u8w = U8Writer.init(al),
    };
  }

  fn softError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
   self.diag.addDiagnosticsWithLevel(.DiagError, token, "Error: " ++ fmt, args);
  }

  fn hardError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) MatchError {
   self.diag.addDiagnosticsWithLevel(.DiagError, token, "Error: " ++ fmt, args);
   return error.MatchError;
  }

  inline fn resetIdCount(self: *Self) void {
    self.namegen.resetTo(1);
  }

  inline fn boxPattern(self: *Self, pat: Pattern) *Pattern {
    return pat.box(self.allocator);
  }

  fn varEql(self: *Self, v1: *Node, v2: *Node) bool {
    _ = self;
    return v1.NdTVar.valueEql(v2);
  }

  inline fn isExpandedPattern(self: *Self, variant: *MatchVariant) bool {
    _ = self;
    return switch (variant.*) {
      .rel, .mrel => true,
      .wildc => |*wc| wc.generated,
      else => false,
    };
  }

  fn reportRedundantCases(self: *Self, node: *tir.MatchNode) void {
    // TODO: Refine this. For cases where the redundancy occurs from a pattern expansion,
    //  we may need to report the redundant pattern from the expansion.
    // NOTE: we do not handle redundancy checks for Ranges!
    _ = node;
    // If there are previous errors, then this error may be a false positive arising
    // from those errors. So don't cascade errors.
    if (self.diag.hasErrors()) return;
    var itr = self.failure.iterator();
    var msg: []const u8 = undefined;
    while (itr.next()) |entry| {
      if (!entry.value_ptr.*.removed) {
        const case = entry.value_ptr.*.case;
        if (self.isExpandedPattern(case.pattern.variant)) {
          msg = "possible redundant case.\n"
              ++ "  This case may already be covered in a prior case or contains patterns that does.\n"
              ++ "  Consider rewriting the pattern or rearranging the case clause(s)."
              ;
        } else {
          msg = "redundant case";
        }
        self.softError(case.pattern.token, "{s}", .{msg});
      }
    }
  }

  inline fn genWildcardToken(tok: ?Token) Token {
    return if (tok) |token| token.tkFrom("_", .TkIdent) else Token.getDefaultToken();
  }

  fn genFreshVarToken(self: *Self, tok: ?Token, start: []const u8) Token {
    std.debug.assert(start.len != 0);
    var slice = if (start[0] == ks.GeneratedVarMarker) start[1..] else start;
    var id = self.namegen.generate("${s}", .{slice});
    return if (tok) |token| token.tkFrom(id, .TkIdent) else Token.getDefaultToken();
  }

  inline fn genFreshVarNode(self: *Self, token: ?Token, start: []const u8) *Node {
    return Node.new(
      .{.NdTVar = tir.TVarNode.init(self.genFreshVarToken(token, start))},
      self.allocator,
    );
  }

  /// save this case as a default fail. If its body gets included in the
  /// decision ir, this is updated to a 'removed' state indicating success.
  inline fn addFailure(self: *Self, case: *Case) void {
    if (self.failure.get(case.body.node) == null) {
      self.failure.set(case.body.node, FailStatus.init(false, case), self.allocator);
    }
  }

  inline fn removeFailure(self: *Self, case: *Case) void {
    self.failure.set(case.body.node, FailStatus.init(true, case), self.allocator);
  }

  inline fn hasFailure(self: *Self, case: *Case) bool {
    return self.failure.get(case.body.node) != null;
  }

  inline fn hasRedunMarker(case: *Case) bool {
    if (case.body.node.isBlock()) {
      var block = case.body.node.block();
      for (block.nodes) |node| {
        if (node.isRedunMarker()) {
          return true;
        }
      }
    }
    return false;
  }

  /// convert the pattern in each case to relation patterns
  fn convertPtnToRelationPtn(self: *Self, m_expr: *Node, cases: []*Case) void {
    for (cases) |case| {
      if (!case.pattern.isRelation() and !case.pattern.isMultiRelation()) {
        self.capturePatternIfAliased(case, case.pattern, m_expr);
        const pattern = self.boxPattern(
          Pattern.init(
            self.caseToRelation(case, m_expr).toVariant(self.allocator),
            case.pattern.token,
            case.pattern.alat,
          )
        );
        case.pattern = pattern;
      }
    }
  }

  /// flatten or patterns; converting its subpatterns to cases 
  fn convertOrConsToCases(self: *Self, m_expr: *Node, cons: *Constructor, case: *Case) CaseList {
    // remove this case, since it's being expanded and will not be referenced after here
    self.removeFailure(case);
    var cases = CaseList.initCapacity(cons.args.len, self.allocator);
    for (cons.args) |pat| {
      cases.appendAssumeCapacity(Case.from(case, pat, self.allocator));
    }
    self.convertPtnToRelationPtn(m_expr, cases.items());
    return self.filterCases(cases.items());
  }

  /// flatten or patterns; converting its subpatterns to cases 
  fn convertOrConsToCasesFromMultiRelation(self: *Self, m_expr: *Node, case: *Case) ?CaseList {
    var mrel = case.pattern.variant.mrel;
    var _rel: ?Relation = null;
    for (mrel.relations.items()) |rel| {
      if (rel.pattern.isOrConstructor()) {
        if (self.varEql(rel.ident, m_expr)) {
          _rel = rel;
          break;
        }
      }
    }
    if (_rel == null) return null;
    // remove this case, since it's being expanded and will not be referenced after here
    self.removeFailure(case);
    var rel = _rel.?;
    var cons = &rel.pattern.variant.cons;
    var cases = CaseList.initCapacity(cons.args.len, self.allocator);
    for (cons.args) |ptn| {
      var new_mrel = MultiRelation.initCapacity(cons.args.len, self.allocator);
      for (mrel.relations.items()) |rl| {
        if (rl.pattern != rel.pattern) {
          new_mrel.relations.appendAssumeCapacity(rl.clone(self.allocator));
        } else {
          new_mrel.relations.appendAssumeCapacity(.{.ident = rel.ident, .pattern = ptn});
        }
      }
      const pat = self.boxPattern(Pattern.init(new_mrel.toVariant(self.allocator), ptn.token, ptn.alat));
      cases.appendAssumeCapacity(Case.from(case, pat, self.allocator));
    }
    return self.filterCases(cases.items());
  }

  /// convert a capture to a variable declaration
  fn convertCaptureToVarDecl(self: *const @This(), expr: *Node, capture: *Node) *Node {
    // ptn as y -> let y = ptn(a)
    return Node.new(
      .{.NdVarDecl = tir.VarDeclNode.init(capture.NdTVar.token, expr, null)},
      self.allocator,
    );
  }

  /// convert a case's pattern to a relation pattern
  fn caseToRelation(self: *Self, case: *Case, ident: *Node) Relation {
    _ = self;
    std.debug.assert(!case.pattern.isRelation() and !case.pattern.isMultiRelation());
    return .{.ident = ident, .pattern = case.pattern};
  }

  /// capture a pattern with an alias (alias) for ex. ptn as foo 
  inline fn capturePatternIfAliased(self: *Self, case: *Case, pattern: *Pattern, expr: *Node) void {
    // convert to a var decl and save to the case's body
    if (pattern.alat.hasAlias()) {
      if (pattern.isVariable() and pattern.variant.vari.ident.NdTVar.isGeneratedVar()) {
        // capture the pattern's variable if it's generated
        case.body.addNode(self.convertCaptureToVarDecl(pattern.variant.vari.ident, pattern.alat.alias.?), self.allocator);
        pattern.alat.alias = null;
      } else {
        std.debug.assert(expr.isTVariable());
        case.body.addNode(self.convertCaptureToVarDecl(expr, pattern.alat.alias.?), self.allocator);
        pattern.alat.alias = null;
      }
    }
  }

  fn selectBestTest(self: *Self, cases: *CaseList) ?Relation {
    // 2. Select one of the tests a is C(P1, . . . , Pn) in the first clause using some heuristic
    const case = cases.itemAt(0);
    switch (case.pattern.variant.*) {
      .rel => |rel| {
        if (rel.pattern.isConstructor()) {
          return rel;
        }
      },
      .mrel => |*mrel| {
        const idx = self.heuristic.selectTest(cases);
        var best = mrel.relations.itemAt(idx);
        if (best.pattern.isConstructor()) {
          return best;
        }
      },
      .cons, .vari, .wildc => {},
    }
    return null;
  }

  /// transform a constructor to a new one with args replaced with Variables()
  fn transformConstructor(self: *Self, cons: *Constructor, ident: *tir.TVarNode) Constructor {
    // TODO: move `node` to init
    var new = Constructor.init(cons.cname(), cons.tktype, cons.builtin, cons.tag, cons.rested);
    new.node = cons.node;
    new.from_map = cons.from_map;
    self.resetIdCount();
    var args = Patterns.init(self.allocator);
    if (cons.tag != .Literal) {
      for (cons.args) |arg| {
        var id = self.genFreshVarNode(ident.token, ident.token.lexeme());
        args.append(self.boxPattern(Pattern.init(Variable.init(id).toVariant(self.allocator), arg.token, arg.alat)));
      }
    }
    new.args = args.items();
    return new;
  }

  fn equalizeConstructorSizes(self: *Self, tcons: *Constructor, cons: *Constructor, case: *Case) !void {
    var big: *Constructor = undefined;
    var small: *Constructor = undefined;
    if (tcons.args.len > cons.args.len) {
      big = tcons;
      small = cons;
    } else {
      big = cons;
      small = tcons;
    }
    if (!cons.rested and !tcons.rested) {
      std.debug.assert(cons.tag == .Tuple or cons.tag == .Other);
      self.softError(
        case.pattern.token,
        "pattern of unequal number of argument(s): {} and {}",
        .{small.args.len, big.args.len}
      );
    } else if (!small.rested) {
      return self.hardError(
        case.pattern.token,
        "pattern of unequal number of argument(s): {} and {}",
        .{small.args.len, big.args.len}
      );
    }
    if (cons.tag == .Tuple or cons.tag == .Other) {
      const id = self.genFreshVarNode(genWildcardToken(case.pattern.token), "_");
      var ptns = Patterns.initCapacity(big.args.len, self.allocator);
      ptns.appendSliceAssumeCapacity(small.args);
      const wc = self.boxPattern(Pattern.init(
        Variable.init(id).toVariant(self.allocator),
        case.pattern.token,
        .{},
      ));
      for (small.args.len..big.args.len) |_| {
        ptns.appendAssumeCapacity(wc);
      }
      small.args = ptns.items();
    }
  }

  const SubProblem = struct {cases_a: []*Case, cases_b: []*Case};

  fn generateSubProblems(self: *Self, occ: *Node, tcons: *Constructor, cases: *CaseList) !SubProblem {
    // 4. Create the two sub problems [A] and [B] as follows by iterating over all the clauses.
    // .   One of three cases can happen:
    var cases_a = CaseList.init(self.allocator);
    var cases_b = CaseList.init(self.allocator);
    for (cases.items()) |case| {
      // (a). The clause contains a test a is C(P1, ... , Pn), ... REST ... for a.
      if (case.pattern.isRelation() or case.pattern.isMultiRelation()) {
        // (note that each clause can only have one test for a)
        var _rel: ?Relation = null;
        var _mrel: ?*MultiRelation = null;
        if (case.pattern.isRelation()) {
          _rel = case.pattern.variant.rel;
        } else {
          var mrel = &case.pattern.variant.mrel;
          _mrel = mrel;
          for (mrel.relations.items()) |rl| {
            if (self.varEql(occ, rl.ident)) {
              _rel = rl;
              break;
            }
          }
        }
        if (_rel != null and _rel.?.pattern.isConstructor()) {
          var rel = _rel.?;
          if (self.varEql(occ, rel.ident)) {
            // constructors are equal
            if (tcons.eql(&rel.pattern.variant.cons)) {
              // Add the expanded clause a1 is P1, .. , an is Pn, .. REST .. to A.
              var cons = rel.pattern.variant.cons;
              if (tcons.args.len != cons.args.len) {
                logger.debug(
                  "unequal constructor sizes: tcons {}-{} and cons {}-{}",
                  .{tcons.tag, tcons.args.len, cons.tag, cons.args.len}
                );
                if (cons.tag == .List) {
                  if (!tcons.rested or tcons.args.len != 0) {
                    cases_a.append(case);
                    cases_b.append(case);
                  }
                  continue;
                }
                try self.equalizeConstructorSizes(tcons, &cons, case);
              } else if (cons.tag == .List and tcons.rested != cons.rested and cons.rested) {
                cases_b.append(case);
              }
              var mrel = MultiRelation.init();
              for (tcons.args, cons.args[0..tcons.args.len]) |vr, pat| {
                self.capturePatternIfAliased(case, pat, vr.variant.vari.ident);
                mrel.relations.append(.{.ident = vr.variant.vari.ident, .pattern = pat}, self.allocator);
              }
              var newp: ?Pattern = null;
              // if we're in a multirelation, select all relations /= ai is Pi since this is a multirelation
              if (_mrel) |mr| {
                for (mr.relations.items()) |rl| {
                  if (!self.varEql(rl.ident, rel.ident)) {
                    mrel.relations.append(rl, self.allocator);
                  }
                }
              }
              if (mrel.relations.isEmpty()) {
                // if we can't find any relation, simply generate a wildcard pattern
                newp = Pattern.init(
                  Wildcard.init(occ.getToken(), true).toVariant(self.allocator),
                  rel.pattern.token,
                  .{}
                );
              }
              if (newp == null) {
                newp = (
                  if (mrel.relations.len() > 1) Pattern.init(
                    mrel.toVariant(self.allocator),
                    rel.pattern.token,
                    .{},
                  )
                  else Pattern.init(
                    mrel.relations.itemAt(0).toVariant(self.allocator),
                    mrel.relations.itemAt(0).pattern.token,
                    mrel.relations.itemAt(0).pattern.alat,
                  )
                );
              }
              cases_a.append(Case.from(case, self.boxPattern(newp.?), self.allocator));
              self.removeFailure(case);
            } else {
              // (b). The clause contains a test a is D(P1, .. , Pn), .. REST .. where D  ̸= C
              // Add this clause to B unchanged.
              cases_b.append(case);
            }
            continue;
          }
        }
      }
      // (c). The clause contains no test for a. Add this clause to both A and B.
      cases_a.append(case);
      cases_b.append(case);
    }
    return .{.cases_a = cases_a.items(), .cases_b = cases_b.items()};
  }

  fn filterCases(self: *Self, cases: []*Case) CaseList {
    // 1. Push tests against bare variables a is y into the right hand side using let y = a, so that all the remaining tests are against constructors.
    // `-> convert cases to relations, then promote variable relations to rhs of body
    var ncases = CaseList.init(self.allocator);
    start: for (0..cases.len) |idx| {
      var case = cases[idx];
      self.addFailure(case);
      if (case.pattern.isRelation()) {
        var rel = case.pattern.variant.rel;
        if (rel.pattern.isVariable()) {
          case.body.addNode(rel.varRelationToVarDecl(self.allocator), self.allocator);
          const pattern = self.boxPattern(
            Pattern.init(
              Wildcard.init(genWildcardToken(case.pattern.token), true).toVariant(self.allocator),
              case.pattern.token,
              case.pattern.alat
            )
          );
          case.pattern = pattern;
        } else if (rel.pattern.isOrConstructor()) {
          // flatten or patterns
          const flattened = self.convertOrConsToCases(rel.ident, &rel.pattern.variant.cons, case);
          ncases.extend(&flattened);
          continue;
        }
      } else if (case.pattern.isMultiRelation()) {
        var mrel = case.pattern.variant.mrel;
        var rels = ds.ArrayListUnmanaged(Relation).init();
        // a is Some(..), b is y,
        // exclude all tests m_expr is var (bare variables)
        for (mrel.relations.items()) |rel| {
          if (rel.pattern.isVariable()) {
            case.body.addNode(rel.varRelationToVarDecl(self.allocator), self.allocator);
          } else if (rel.pattern.isOrConstructor()) {
            // flatten or patterns
            if (self.convertOrConsToCasesFromMultiRelation(rel.ident, case)) |*flattened| {
              ncases.extend(flattened);
              continue :start;
            }
            rels.append(rel, self.allocator);
          } else {
            rels.append(rel, self.allocator);
          }
        }
        if (mrel.relations.len() != rels.len()) {
          if (rels.isNotEmpty()) {
            // still a multirelation if rels has more than one relation item
            if (rels.len() > 1) {
              case.pattern.variant.mrel.relations = rels;
            } else {
              case.pattern = self.boxPattern(
                Pattern.init(
                  rels.itemAt(0).toVariant(self.allocator),
                  rels.itemAt(0).pattern.token,
                  rels.itemAt(0).pattern.alat,
                )
              );
            }
          } else {
            // this means all relations in mrel were bare variable tests
            // so generate a wildcard for the entire multi-relation pattern
            const pattern = self.boxPattern(
              Pattern.init(Wildcard.init(
                genWildcardToken(case.pattern.token), true).toVariant(self.allocator),
                case.pattern.token,
                case.pattern.alat
              )
            );
            case.pattern = pattern;
          }
        } else {}
      }
      ncases.append(case);
    }
    return ncases;
  }

  fn compileCase(self: *Self, _m_expr: *Node, _cases: []*Case) !*Result {
    // 1. Push tests against bare variables a is y into the right hand sides using let y = a,
    // .  so that all the remaining tests are against constructors.
    var cases = self.filterCases(_cases);
    if (cases.isEmpty()) {
      return util.box(Result, .{.fail = .{}}, self.allocator);
    }
    // 2. Select one of the tests a is C(P1, . . . , Pn) in the first clause using some heuristic
    var bst = self.selectBestTest(&cases);
    if (bst == null) {
      var case = cases.itemAt(0);
      self.removeFailure(case);
      const body = case.body.clone(self.allocator);
      if (case.guard) |cond| {
        _ = cases.list.orderedRemove(0);
        const guard: Guard = .{
          .cond = cond, .body = body,
          .fallback = try self.compileCase(_m_expr, cases.items())
        };
        return util.box(Result, .{.gard = guard}, self.allocator);
      } else {
        return util.box(Result, .{.leaf = body}, self.allocator);
      }
    }
    // a is C(P1, . . . , Pn)
    var rel = bst.?;
    // 3. Generate this pattern match
    // .   match# a with
    // .   | C(a1, . . . , an) => [A]
    // .   | _                 => [B]
    var m_expr = rel.ident;
    var swch = Switch.init(m_expr, rel.pattern.token);
    var tcons = self.transformConstructor(&rel.pattern.variant.cons, &m_expr.NdTVar);
    const wildc = Wildcard.init(genWildcardToken(m_expr.getToken()), true);
    // 4. Create the two sub problems [A] and [B] as follows by iterating over all the clauses.
    const subp = try self.generateSubProblems(m_expr, &tcons, &cases);
    // 5. Recursively generate code for [A] and [B].
    const res_a = try self.compileCase(m_expr, subp.cases_a);
    const res_b = try self.compileCase(m_expr, subp.cases_b);
    swch.branches[0] = (.{.lhs = .{.cons = tcons}, .rhs = res_a});
    swch.branches[1] = (.{.lhs = .{.wildc = wildc}, .rhs = res_b});
    return util.box(Result, .{.swch = swch}, self.allocator);
  }

  /// This implements pattern matching as described by Jules Jacobs, with modifications & extensions.
  /// Some inspiration from Maranget is also utilized.
  pub fn compile(self: *Self, node: *tir.MatchNode) !*DecisionTree {
    // we match on m_expr
    if (util.getMode() == .Debug) {
      node.render(0, &self.u8w) catch {};
      logger.debug("match ast dump:\n{s}\n", .{self.u8w.items()});
    }
    var m_expr = node.expr;
    self.convertPtnToRelationPtn(m_expr, node.cases);
    const tree = try self.compileCase(m_expr, node.cases);
    if (util.getMode() == .Debug) {
      tree.render(0, &self.u8w) catch {};
      logger.debug("decision tree dump:\n{s}\n", .{self.u8w.items()});
    }
    self.reportRedundantCases(node);
    return tree;
  }

  pub fn lowerDecisionTree(self: *Self, tree: *DecisionTree, fail_token: Token) !*Node {
    const node = try self.dtt.transform(tree, fail_token);
    if (!node.isBlock()) {
      return tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{node}), self.allocator);
    }
    return node;
  }
};

/// transform/lower a decision ir to if-else statements
pub const DecisionTreeTransformer = struct {
  allocator: Allocator,
  fail_token: Token = undefined,
  cached_pairs: ds.ArrayList(MapPair),

  const Self = @This();
  const MapPair = struct {key: *Node, val: *Node, parent: ?*Constructor};

  pub fn init(allocator: Allocator) Self {
    return Self {
      .allocator = allocator,
      .cached_pairs = ds.ArrayList(MapPair).init(allocator)
    };
  }

  inline fn andToken(token: Token) Token {
    return token.dupTk(tir.TokenType.TkAnd);
  }

  inline fn isToken(token: Token) Token {
    return token.dupTk(tir.TokenType.TkIs);
  }

  inline fn eqeqToken(token: Token) Token {
    return token.dupTk(tir.TokenType.Tk2Eq);
  }

  inline fn geqToken(token: Token) Token {
    return token.dupTk(tir.TokenType.TkGeq);
  }

  fn newNumberNode(self: *Self, token: Token, val: f64) *Node {
    return Node.new(.{.NdNumber = tir.NumberNode.init(token.dupTk(.TkNumber), val)}, self.allocator);
  }

  fn newTypeNode(self: *Self, ty: *Type, token: Token) *Node {
    return Node.new(.{.NdType = tir.TypeNode.init(ty, token)}, self.allocator);
  }

  fn newTypeNodeWithSkip(self: *Self, ty: *Type, token: Token) *Node {
    var node = Node.new(.{.NdType = tir.TypeNode.init(ty, token)}, self.allocator);
    // don't resolve this type because it is incomplete 
    node.NdType.skip_type_resolution = true;
    return node;
  }

  fn newIfElseNode(self: *Self, cond: *Node, then: *Node, els: *Node) *Node {
    return Node.new(.{.NdSimpleIf = tir.SimpleIfNode.init(cond, then, els)}, self.allocator);
  }

  /// ident -> ident[index]
  fn newSubscript(self: *Self, ident: *Node, idx: usize) *Node {
    // create index
    const token = ident.NdTVar.token.tkFrom("<num>", .TkNumber);
    const index = self.newNumberNode(token, @floatFromInt(idx));
    // create ident[index]
    return Node.new(.{.NdSubscript = tir.SubscriptNode.init(ident, index)}, self.allocator);
  }

  /// ident -> ident.field
  fn newFieldAccess(self: *Self, ident: *Node, field_name: []const u8) *Node {
    // create field
    const token = ident.NdTVar.token.tkFrom(field_name, .TkIdent);
    const rhs = Node.new(.{.NdTVar = tir.TVarNode.init(token)}, self.allocator);
    // create ident.field
    return Node.new(.{.NdDotAccess = tir.DotAccessNode.init(ident, rhs)}, self.allocator);
  }

  /// ident -> ident.method_name(...args)
  fn newMethodCallWithArgs(self: *Self, ident: *Node, method_name: []const u8, args: tir.NodeItems) *Node {
    const dot_expr = self.newFieldAccess(ident, method_name);
    return Node.new(.{.NdBasicCall = tir.BasicCallNode.init(dot_expr, args)}, self.allocator);
  }

  /// ident -> ident.method_name()
  inline fn newMethodCall(self: *Self, ident: *Node, method_name: []const u8) *Node {
    return self.newMethodCallWithArgs(ident, method_name, &[_]*Node{});
  }

  /// creates a regular block with an 'entry' scope node
  inline fn newBlock(self: *Self) *Node {
    return Node.new(.{.NdBlock = tir.BlockNode.init(&[_]*Node{})}, self.allocator);
  }

  /// transform constructor `C(var)` into `var = expr()`
  fn constructorMethodToVarDecl(self: *Self, cons: *Constructor, ident: *Node, method_name: []const u8) *Node {
    std.debug.assert(cons.args.len == 1);
    const call = self.newMethodCall(ident, method_name);
    return Node.new(
      .{.NdVarDecl = tir.VarDeclNode.init(cons.args[0].variant.vari.ident.NdTVar.token, call, null)},
      self.allocator,
    );
  }

  /// transform a map constructor `C(var)` into `var = keys() ; var = values()`
  fn mapConstructorMethodToVarDecl(self: *Self, cons: *Constructor, ident: *Node, parent: ?*Constructor) struct{*Node, *Node} {
    std.debug.assert(cons.args.len == 1);
    const call_1 = self.newMethodCall(ident, "keys");
    const call_2 = self.newMethodCall(ident, "values");
    const cons_ident = cons.args[0].variant.vari.ident.NdTVar.token;
    const id = cons_ident.lexeme();
    var value = std.fmt.allocPrint(self.allocator, "{s}_keys", .{id}) catch unreachable;
    const keys_id = Node.new(.{.NdTVar = tir.TVarNode.init(cons_ident.tkFrom(value, .TkIdent))}, self.allocator);
    value = std.fmt.allocPrint(self.allocator, "{s}_vals", .{id}) catch unreachable;
    const vals_id =  Node.new(.{.NdTVar = tir.TVarNode.init(cons_ident.tkFrom(value, .TkIdent))}, self.allocator);
    // decls
    const decl_1 = Node.new(.{.NdVarDecl = tir.VarDeclNode.init(keys_id.NdTVar.token, call_1, null)}, self.allocator);
    const decl_2 = Node.new(.{.NdVarDecl = tir.VarDeclNode.init(vals_id.NdTVar.token, call_2, null)}, self.allocator);
    self.cached_pairs.append(.{.key = keys_id, .val = vals_id, .parent = parent});
    return .{decl_1, decl_2};
  }

  /// transform constructor `C(var)` into `var = id[expr]`
  fn constructorSubscriptToVarDecl(self: *Self, arg: *Pattern, ident: *Node, index: usize) *Node {
    const subsc = self.newSubscript(ident, index);
    return Node.new(.{.NdVarDecl = tir.VarDeclNode.init(arg.variant.vari.ident.NdTVar.token, subsc, null)}, self.allocator);
  }

  /// transform field access in a constructor to a var decl, for ex:
  /// Cons(id) => id = occ.field
  fn fieldAccessToVarDecl(self: *Self, source: *Node, field: []const u8, target: *Node) *Node {
    const access = self.newFieldAccess(source, field);
    access.NdDotAccess.is_desugared = true;
    return Node.new(.{.NdVarDecl = tir.VarDeclNode.init(target.NdTVar.token, access, null)}, self.allocator);
  }

  /// transform field access in a tag constructor to a var decl, for ex:
  /// Cons(id) => id = occ.field_index
  fn tagFieldAccessToVarDecl(self: *Self, source: *Node, field: usize, target: *Node) *Node {
    // create field
    const token = source.NdTVar.token.tkFrom(ks.GeneratedTypeVar, .TkNumber);
    const rhs = self.newNumberNode(token, @floatFromInt(field));
    // create ident.field
    const dot_expr = Node.new(.{.NdDotAccess = tir.DotAccessNode.init(source, rhs)}, self.allocator);
    dot_expr.NdDotAccess.allow_tag_access = true;
    // create var decl
    return Node.new(.{.NdVarDecl = tir.VarDeclNode.init(target.NdTVar.token, dot_expr, null)}, self.allocator);
  }

  fn blockOrToBlock(self: *Self, node: *Node) *Node {
    if (!node.isBlock()) {
      return tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{node}), self.allocator);
    }
    return node;
  }

  fn tListOfSwitch(self: *Self, swch: *Switch, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    // transpile if condition
    var if_branch = swch.branches[0];
    var els_branch = swch.branches[1];
    const skip = els_branch.rhs.isSwitch() and els_branch.rhs.swch.branches[0].lhs.cons.tag == .List;
    const id = swch.occ.clone(self.allocator);
    var cons = &if_branch.lhs.cons;
    const from_map = cons.from_map;
    var nodes = tir.NodeListU.init();
    if (!from_map) {
      const then = self.newBlock();
      const len = self.newNumberNode(swch.token, @floatFromInt(cons.args.len));
      const op = if (cons.rested) geqToken(swch.token) else eqeqToken(swch.token);
      const cond = Node.new(.{.NdBinary = tir.BinaryNode.init(self.newMethodCall(id, ks.LenVar), len, op)}, self.allocator);
      cond.NdBinary.allow_rested = cons.rested;
      nodes.ensureTotalCapacity(cons.args.len, self.allocator);
      for (cons.args, 0..) |arg, index| {
        // don't capture wildcard patterns
        if (arg.isWildcard()) continue;
        // transform vars to var decl
        nodes.appendAssumeCapacity(self.constructorSubscriptToVarDecl(arg, id, index));
      }
      if (cons.rested and cons.node != null) {
        // tail = id.slice(cons.args.len, id.len())
        var args = tir.NodeList.initCapacity(2, self.allocator);
        args.appendSliceAssumeCapacity(@constCast(&[_]*Node{len, self.newMethodCall(id, ks.LenVar)}));
        const slice = self.newMethodCallWithArgs(id, "slice", args.items());
        const decl = Node.new(.{.NdVarDecl = tir.VarDeclNode.init(cons.node.?.NdTVar.token, slice, null)}, self.allocator);
        nodes.append(decl, self.allocator);
      }
      const _then = try self.tDecision(if_branch.rhs, parent, false);
      if (_then.isBlock()) {
        nodes.appendSlice(_then.block().nodes, self.allocator);
      } else {
        nodes.append(_then, self.allocator);
      }
      then.block().nodes = nodes.items();
      std.debug.assert(els_branch.lhs.isWildcard());
      const ife = self.newIfElseNode(
        Node.toMatchCondition(cond, self.allocator),
        then,
        self.blockOrToBlock(try self.tDecision(els_branch.rhs, parent, skip))
      );
      if (skip_cons_test) return ife;
      const tyn = self.newTypeNodeWithSkip(Type.newBuiltinGenericClass(ks.ListVar, .TkList, self.allocator), swch.token);
      const t_cond = Node.new(.{.NdBinary = tir.BinaryNode.init(id, tyn, isToken(swch.token))}, self.allocator);
      const t_then = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{ife}), self.allocator);
      return self.newIfElseNode(Node.toMatchCondition(t_cond, self.allocator), t_then, self.newBlock());
    } else {
      const then = self.newBlock();
      const pair = self.cached_pairs.getLast();
      // len becomes cons.args.len / 2 because we're using the .keys() (or .values()) list here instead of the .listItems()
      const len = self.newNumberNode(swch.token, @floatFromInt(cons.args.len / 2));
      const op = if (cons.rested) geqToken(swch.token) else eqeqToken(swch.token);
      const cond = Node.new(.{.NdBinary = tir.BinaryNode.initRested(self.newMethodCall(pair.key, ks.LenVar), len, op)}, self.allocator);
      nodes.ensureTotalCapacity(cons.args.len, self.allocator);
      for (cons.args, 0..) |arg, i| {
        // don't capture wildcard patterns
        if (arg.isWildcard()) continue;
        // transform vars to var decl
        if ((i + 1) & 1 == 1) {
          // key. Position/slot is at i / 2 in .keys()
          nodes.appendAssumeCapacity(self.constructorSubscriptToVarDecl(arg, pair.key, (i / 2)));
        } else {
          // value. Position/slot is at i / 2 in .values()
          nodes.appendAssumeCapacity(self.constructorSubscriptToVarDecl(arg, pair.val, (i / 2)));
        }
      }
      const _then = try self.tDecision(if_branch.rhs, parent, false);
      if (_then.isBlock()) {
        nodes.appendSlice(_then.block().nodes, self.allocator);
      } else {
        nodes.append(_then, self.allocator);
      }
      then.block().nodes = nodes.items();
      std.debug.assert(els_branch.lhs.isWildcard());
      return self.newIfElseNode(
        Node.toMatchCondition(cond, self.allocator),
        then,
        self.blockOrToBlock(try self.tDecision(els_branch.rhs, parent, skip))
      );
    }
  }

  fn tTupleOfSwitch(self: *Self, swch: *Switch, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    // transpile if condition
    var if_branch = swch.branches[0];
    var els_branch = swch.branches[1];
    const skip = els_branch.rhs.isSwitch() and els_branch.rhs.swch.branches[0].lhs.cons.tag == .Tuple;
    const id = swch.occ.clone(self.allocator);
    var cons = &if_branch.lhs.cons;
    var nodes = tir.NodeListU.init();
    const then = self.newBlock();
    const len = self.newNumberNode(swch.token, @floatFromInt(cons.args.len));
    const op = if (cons.rested) geqToken(swch.token) else eqeqToken(swch.token);
    const cond = Node.new(.{.NdBinary = tir.BinaryNode.init(self.newMethodCall(id, ks.LenVar), len, op)}, self.allocator);
    cond.NdBinary.allow_rested = cons.rested;
    nodes.ensureTotalCapacity(cons.args.len, self.allocator);
    for (cons.args, 0..) |arg, index| {
      // don't capture wildcard patterns
      if (arg.isWildcard()) continue;
      // transform vars to var decl
      nodes.appendAssumeCapacity(self.constructorSubscriptToVarDecl(arg, id, index));
    }
    const _then = try self.tDecision(if_branch.rhs, parent, false);
    if (_then.isBlock()) {
      nodes.appendSlice(_then.block().nodes, self.allocator);
    } else {
      nodes.append(_then, self.allocator);
    }
    then.block().nodes = nodes.items();
    std.debug.assert(els_branch.lhs.isWildcard());
    const ife = self.newIfElseNode(
      Node.toMatchCondition(cond, self.allocator),
      then,
      self.blockOrToBlock(try self.tDecision(els_branch.rhs, parent, skip))
    );
    if (skip_cons_test) return ife;
    const tyn = self.newTypeNodeWithSkip(Type.newBuiltinGenericClass(ks.TupleVar, .TkTuple, self.allocator), swch.token);
    const t_cond = Node.new(.{.NdBinary = tir.BinaryNode.init(id, tyn, isToken(swch.token))}, self.allocator);
    const t_then = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{ife}), self.allocator);
    return self.newIfElseNode(Node.toMatchCondition(t_cond, self.allocator), t_then, self.newBlock());
  }

  /// Switch (occ) test (..., body(..)) test (Wildcard(_) body(..)) ->
  /// if (occ op test(..)) body(..) else ...
  fn tSwitch(self: *Self, swch: *Switch, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    // transpile if condition
    var if_branch = swch.branches[0];
    var els_branch = swch.branches[1];
    if (if_branch.lhs.cons.tag == .List) {
      return self.tListOfSwitch(swch, parent, skip_cons_test);
    } else if (if_branch.lhs.cons.tag == .Tuple) {
      return self.tTupleOfSwitch(swch, parent, skip_cons_test);
    }
    // make occ is test(..)
    const node = self.newBlock();
    var if_cond: *Node = undefined;
    const if_body = self.newBlock();
    const id = swch.occ.clone(self.allocator);
    var cons = &if_branch.lhs.cons;
    var _parent: ?*Constructor = null;
    var _if_body = if_body.block();
    switch (cons.tag) {
      .Map, .Other => {
        var nodes = tir.NodeListU.init();
        if (cons.tag == .Map) {
          // transform constructor `C(var)` into `var = keys() ; var = values()`
          _parent = cons;
          const pair = self.mapConstructorMethodToVarDecl(cons, id, _parent);
          _if_body.appendSlice(@constCast(&[_]*Node{pair.@"0", pair.@"1"}), self.allocator);
          const tyn = self.newTypeNodeWithSkip(Type.newBuiltinGenericClass(ks.MapVar, .TkMap, self.allocator), swch.token);
          if_cond = Node.new(.{.NdBinary = tir.BinaryNode.init(id, tyn, isToken(swch.token))}, self.allocator);
        } else {
          // tag or class type
          if (cons.args.len > 0) {
            nodes.ensureTotalCapacity(cons.args.len, self.allocator);
            for (cons.args, 0..) |arg, idx| {
              // don't capture wildcard patterns
              if (arg.isWildcard()) {
                continue;
              }
              if (arg.alat.hasField()) {
                // transform field access to a var decl
                nodes.appendAssumeCapacity(self.fieldAccessToVarDecl(id, arg.alat.field.?.NdTVar.token.lexeme(), arg.variant.vari.ident));
              } else {
                // transform field access - by position, to a var decl
                nodes.appendAssumeCapacity(self.tagFieldAccessToVarDecl(id, idx, arg.variant.vari.ident));
              }
            }
            _if_body.nodes = nodes.items();
          }
          const tyn = self.newTypeNode(Type.newTagOrClass(cons.cname(), cons.tktype, self.allocator), swch.token);
          if_cond = Node.new(.{.NdBinary = tir.BinaryNode.init(id, tyn, isToken(swch.token))}, self.allocator);
        }
      },
      .Literal => {
        if_cond = Node.new(.{
          .NdBinary = tir.BinaryNode.init(id, cons.node.?, eqeqToken(swch.token))
        }, self.allocator);
      },
      else => unreachable,
    }
    // make if body
    const body = try self.tDecision(if_branch.rhs, _parent orelse parent, skip_cons_test);
    if (body.isBlock()) {
      _if_body.appendSlice(body.block().nodes, self.allocator);
    } else {
      _if_body.append(body, self.allocator);
    }
    // make else:
    // for else, test is always a wildcard, so transpile the rhs directly
    std.debug.assert(els_branch.lhs.isWildcard());
    const ife = self.newIfElseNode(
      Node.toMatchCondition(if_cond, self.allocator),
      if_body,
      self.blockOrToBlock(try self.tDecision(els_branch.rhs, _parent orelse parent, skip_cons_test))
    );
    if (self.cached_pairs.isNotEmpty()) {
      const last = self.cached_pairs.getLast();
      // only pop when we're back at the call (here) which set `parent` on the cached pair
      if (last.parent == _parent) {
        _ = self.cached_pairs.pop();
      }
    }
    var block = node.block();
    block.append(ife, self.allocator);
    return if (block.nodes.len == 1) block.nodes[0] else node;
  }

  fn tLeaf(self: *Self, leaf: *Leaf, exclude_decls: bool) !*Node {
    leaf.decls.reverse();
    if (!exclude_decls and leaf.decls.isNotEmpty()) {
      if (leaf.node.isBlock()) {
        leaf.node.block().prependSlice(leaf.decls.items(), self.allocator);
      } else {
        var node = self.newBlock();
        var stmts = tir.NodeListU.initCapacity(leaf.decls.len() + 1, self.allocator);
        stmts.appendSliceAssumeCapacity(leaf.decls.items());
        stmts.appendAssumeCapacity(leaf.node);
        node.block().nodes = stmts.items();
        return node;
      }
    }
    return leaf.node;
  }

  fn tFail(self: *Self, fail: *Fail) !*Node {
    _ = fail;
    return Node.new(.{.NdFailMarker = tir.MarkerNode.init(self.fail_token)}, self.allocator);
  }

  fn tGuard(self: *Self, gard: *Guard, parent: ?*Constructor, skip_cons_test: bool) !*Node {
    const then = self.blockOrToBlock(try self.tLeaf(&gard.body, true));
    var _nodes = tir.NodeListU.initCapacity(gard.body.decls.len() + 1, self.allocator);
    _nodes.appendSliceAssumeCapacity(gard.body.decls.items());
    _nodes.appendAssumeCapacity(self.newIfElseNode(
        Node.toMatchCondition(gard.cond, self.allocator),
        then,
        self.blockOrToBlock(try self.tDecision(gard.fallback, parent, skip_cons_test))
      )
    );
    return tir.BlockNode.newBlockWithOwnedNodes(_nodes.items(), self.allocator);
  }

  fn tDecision(self: *Self, dt: *DecisionTree, parent: ?*Constructor, skip_cons_test: bool) anyerror!*Node {
    return switch (dt.*) {
      inline else => |*node| @call(.always_inline, @TypeOf(node.*).transform, .{node, @This(), self, parent, skip_cons_test}),
    };
  }

  pub fn transform(self: *Self, tree: *DecisionTree, fail_token: Token) !*Node {
    self.fail_token = fail_token;
    return @call(.always_inline, Self.tDecision, .{self, tree, null, false});
  }
};
