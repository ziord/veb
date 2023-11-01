const std = @import("std");
const types = @import("type.zig");
const ast = @import("ast.zig");
const ks = @import("constants.zig");
const ds = @import("ds.zig");
const util = @import("util.zig");
const diagnostics = @import("diagnostics.zig");

const Type = types.Type;
const Token = @import("lex.zig").Token;
const TypeChecker = @import("check.zig").TypeChecker;
const Diagnostic = diagnostics.Diagnostic;
const addDepth = util.addDepth;

/// C(p1...pn)
pub const Constructor = struct {
  /// is a builtin constructor
  builtin: bool,
  /// has rest pattern
  rested: bool,
  tag: ConsTag,
  args: ds.ArrayList(*Pattern),
  name: []const u8,
  synth_name: ?[]const u8 = null,
  targs: ?*ast.AstList = null,
  node: ?*ast.AstNode = null,
  typ: ?*Type = null,

  pub fn init(name: []const u8, builtin: bool, tag: ConsTag, rested: bool, al: std.mem.Allocator) @This() {
    return @This(){.name = name, .builtin = builtin, .tag = tag, .rested = rested, .args = ds.ArrayList(*Pattern).init(al)};
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    return std.mem.eql(u8, self.cname(), other.cname());
  }

  pub fn newOrCons(al: std.mem.Allocator) @This() {
    return @This().init("$OR", true, .Or, false, al);
  }

  pub fn newListCons(al: std.mem.Allocator) @This() {
    return @This().init(ks.ListVar, true, .List, false, al);
  }

  pub fn newTupleCons(al: std.mem.Allocator) @This() {
    return @This().init(ks.TupleVar, true, .Tuple, false, al);
  }

  pub fn newErrCons(al: std.mem.Allocator) @This() {
    return @This().init(ks.ErrVar, true, .Err, false, al);
  }

  pub fn newMapCons(al: std.mem.Allocator) @This() {
    return @This().init(ks.MapVar, true, .Map, false, al);
  }
  
  pub fn newClassCons(name: []const u8, node: *ast.AstNode, al: std.mem.Allocator) @This() {
    var this = @This().init(name, false, .Other, false, al);
    this.node = node;
    return this;
  }

  pub fn newLiteralCons(node: *ast.AstNode, al: std.mem.Allocator) @This() {
    var name = switch (node.*) {
      .AstNil, .AstBool, .AstString => |*lit| lit.token.value,
      .AstNumber => |*lit| blk: {
        if (lit.value > 0) break :blk lit.token.value;
        break :blk std.fmt.allocPrint(al, "lit.{d}", .{lit.value}) catch @panic("could not generate lit.cons.");
      },
      else => unreachable
    };
    var this = @This().init(name, true, .Literal, false, al);
    this.node = node;
    return this;
  }

  pub fn cname(self: *@This()) []const u8 {
    if (self.synth_name) |name| {
      return name;
    }
    if (self.typ) |typ| {
      if (!typ.isClsGeneric()) {
        self.synth_name = self.name;
        return self.name;
      } else if (self.synth_name) |name| {
        return name;
      } else {
        const name = TypeChecker.makeSynthName(
          self.args.allocator(),
          self.name,
          typ.klass().builtin,
          typ.klass().tparams.?,
          null
        );
        self.synth_name = name;
        return name;
      }
    }
    return self.name;
  }

  pub fn toVariant(self: @This(), al: std.mem.Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.cons = self}, al);
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) @This() {
    var new = @This().init(self.name, self.builtin, self.tag, self.rested, al);
    for (self.args.items()) |arg| {
      new.args.append(arg.clone(al));
    }
    if (self.node) |node| {
      new.node = node.clone(al);
    }
    if (self.targs) |targs| {
      new.targs = ast.AstList.clone(targs, al).box();
    }
    if (self.typ) |typ| {
      new.typ = typ.clone(al);
    }
    new.synth_name = self.synth_name;
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    if (self.tag == .Literal and !std.mem.startsWith(u8, self.name, "lit")) {
      _ = try writer.write("lit.");
    }
    _ = try writer.write(self.name);
    if (self.targs) |targs| {
      _ = try writer.write("{");
      var len = targs.len() - 1;
      for (targs.items(), 0..) |node, i| {
        if (i < len) _ = try writer.write(", ");
         _ = try writer.write(node.AstNType.typ.typename(al));
      }
      _ = try writer.write("}");
    }
    if (self.args.isEmpty()) {
      _ = try writer.write("()");
      return writer.context.items;
    }
    _ = try writer.write("(\n");
    const len = self.args.len() -| 1;
    for (self.args.items(), 0..) |itm, i| {
      try addDepth(&writer, depth + 2);
      _ = try writer.write(try itm.render(depth + 2, al));
      if (i < len) _ = try writer.write(",\n");
    }
    _ = try writer.write("\n");
    try addDepth(&writer, depth + 4);
    _ = try writer.write(")");
    return writer.context.items;
  }
};

/// x => E
pub const Variable = struct {
  ident: *ast.AstNode,

  pub fn init(ident: *ast.AstNode) @This() {
    return @This(){.ident = ident};
  }

  pub fn toVariant(self: @This(), al: std.mem.Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.vari = self}, al);
  }

  pub fn clone(self: @This(), al: std.mem.Allocator) @This() {
    return @This(){.ident = self.ident.clone(al)};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Variable(");
    _ = try writer.write(self.ident.AstVar.token.value);
    _ = try writer.write(")");
    return writer.context.items;
  }
};

/// _ => E
pub const Wildcard = struct {
  /// for error reporting
  token: Token,
  generated: bool,
  typ: ?*Type = null,

  pub fn init(token: Token, generated: bool) @This() {
    return @This(){.token = token, .generated = generated};
  }

  pub fn toVariant(self: @This(), al: std.mem.Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.wildc = self}, al);
  }

  pub fn clone(self: @This(), al: std.mem.Allocator) @This() {
    _ = al;
    return self;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Wildcard(_)");
    return writer.context.items;
  }
};

/// idi is Ci
pub const Relation = struct {
  ident: *ast.AstNode,
  pattern: *Pattern,

  pub fn varRelationToVarDecl(self: *const @This(), al: std.mem.Allocator) *ast.AstNode {
    // a is y -> let y = a
    std.debug.assert(self.pattern.isVariable());
    var node = ast.AstNode.create(al);
    // TODO: clone var?
    const vr = self.pattern.variant.vari.ident.clone(al);
    node.* = .{.AstVarDecl = ast.VarDeclNode.init(&vr.AstVar, self.ident.clone(al), false)};
    return node;
  }

  pub fn clone(self: @This(), al: std.mem.Allocator) @This() {
    return @This(){.ident = self.ident.clone(al), .pattern = self.pattern.clone(al)};
  }

  pub fn toVariant(self: @This(), al: std.mem.Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.rel = self}, al);
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    _ = al;
    _ = self;
    _ = depth;
    unreachable;
  }
};

/// id1 is C1, ..., idn is Cn
pub const MultiRelation = struct {
  relations: RelationList,

  pub const RelationList = ds.ArrayList(Relation);

  pub fn init(al: std.mem.Allocator) @This() {
    return @This(){.relations = RelationList.init(al)};
  }

  pub fn toVariant(self: @This(), al: std.mem.Allocator) *MatchVariant {
    return util.box(MatchVariant, .{.mrel = self}, al);
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) @This() {
    return @This(){.relations = RelationList.clone(&self.relations, al)};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
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
    alias: ?*ast.AstNode = null,
    field: ?*ast.AstNode = null,

    pub fn init(alias: ?*ast.AstNode, field: ?*ast.AstNode) @This() {
      return .{.alias = alias, .field = field};
    }

    pub fn hasAlias(self: *const @This()) bool {
      return self.alias != null;
    }

    pub fn hasField(self: *const @This()) bool {
      return self.field != null;
    }

    pub fn clone(self: @This(), al: std.mem.Allocator) @This() {
      const field = if (self.field) |node| node.clone(al) else self.field;
      const alias = if (self.alias) |node| node.clone(al) else self.alias;
      return .{.alias = alias, .field = field};
    }
  };

  pub fn init(variant: *MatchVariant, token: Token, capture: AliasOrAttr) @This() {
    return @This() {.variant = variant, .token = token, .alat = capture};
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

  pub fn setType(self: *@This(), ty: *Type) void {
    switch (self.variant.*) {
      .cons => |*cons| {
        cons.typ = ty;
      },
      .vari => |*vari| {
        vari.ident.AstVar.typ = ty;
      },
      .wildc => {},
      else => unreachable,
    }
  }

  pub inline fn box(self: @This(), al: std.mem.Allocator) *Pattern {
    return util.box(Pattern, self, al);
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *@This() {
    const capture = self.alat.clone(al);
    const pat = @This(){.variant = self.variant.clone(al), .alat = capture, .token = self.token};
    return pat.box(al);
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(try self.variant.render(depth, al));
    if (self.alat.hasField()) {
      _ = try writer.write(" as-field ");
      _ = try writer.write(self.alat.field.?.AstVar.token.value);
    }
    if (self.alat.hasAlias()) {
      _ = try writer.write(" as ");
      _ = try writer.write(self.alat.alias.?.AstVar.token.value);
    }
    return writer.context.items;
  }
};

/// a body of actions
pub const Body = struct {
  /// the main body - in reverse order if `reversed`
  node: *ast.AstNode,
  reversed: bool,

  pub fn init(node: *ast.AstNode, rev: bool, al: std.mem.Allocator) @This() {
    var block: *ast.AstNode = undefined;
    var reversed = rev;
    if (node.isBlock()) {
      block = node;
      if (!rev) {
        block.block().nodes.reverse();
        reversed = true;
      }
    } else {
      block = ast.BlockNode.newEmptyBlock(al);
      block.block().nodes.append(node);
      reversed = true;
    }
    return @This(){.node = block, .reversed = reversed};
  }

  pub fn addNode(self: *@This(), node: *ast.AstNode) void {
    return self.node.block().nodes.append(node);
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) @This() {
    return @This(){.node = self.node.clone(al), .reversed = self.reversed};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    const len = self.node.block().nodes.len();
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    if (len > 0) {
      _ = try writer.write("body(\n");
      for (0..len) |i| {
        const node = self.node.block().nodes.itemAt(len - i - 1);
        try addDepth(&writer, (depth + 2) * 2 + 1);
        if (node.isVarDecl()) {
          const vd = node.AstVarDecl;
          const source = if (vd.value.isVariable()) vd.value.AstVar.token.value else "$expr";
          const str = std.fmt.allocPrint(al, "let {s} = {s}\n", .{vd.ident.token.value, source}) catch unreachable;
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
    return writer.context.items;
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer) !*ast.AstNode {
    return t.tLeaf(self);
  }
};

/// from Maranget's decision tree
const Result = union(enum) {
  leaf: Leaf,
  fail: Fail,
  swch: Switch,
  gard: Guard,

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    return switch (self.*) {
      inline else => |*this| try this.render(depth, al),
    };
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *@This() {
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

  fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Fail");
    return writer.context.items;
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer) !*ast.AstNode {
    return t.tFail(self);
  }

  fn clone(self: *@This(), al: std.mem.Allocator) *Result {
    return util.box(Result, .{.fail = self.*}, al);
  }
};

/// Guard -> if cond => ..
pub const Guard = struct {
  cond: *ast.AstNode,
  body: Leaf,
  fallback: *Result,

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Guard(cond,\n");
    try addDepth(&writer, (depth + 1) * 2);
    _ = try writer.write("body = (\n");
    try addDepth(&writer, (depth + 1) * 2);
    _ = try writer.write(try self.body.render(depth + 3, al));
    try addDepth(&writer, depth + 2);
    _ = try writer.write("),\n");
    try addDepth(&writer, (depth + 1) * 2);
    _ = try writer.write("fallback = (\n");
    try addDepth(&writer, (depth + 1) * 2);
    _ = try writer.write(try self.fallback.render(depth + 3, al));
     try addDepth(&writer, depth + 2);
    _ = try writer.write(")\n");
    try addDepth(&writer, depth + 1);
    _ = try writer.write(")\n");
    return writer.context.items;
  }

  fn clone(self: *@This(), al: std.mem.Allocator) *Result {
    const cln: Guard = .{.cond = self.cond.clone(al), .body = self.body.clone(al), .fallback = self.fallback.clone(al)};
    return util.box(Result, .{.gard = cln}, al);
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer) !*ast.AstNode {
    return t.tGuard(self);
  }
};

/// Switch o (L) -> multiway test, o is an occurrence
const Switch = struct {
  /// occurrence
  occ: *ast.AstNode,
  /// branch
  branches: [2]Branch,
  /// error location token
  token: Token,

  pub fn init(expr: *ast.AstNode, token: Token) @This() {
    return @This(){.occ = expr, .branches = undefined, .token = token};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("Switch (");
    _ = try writer.write(self.occ.AstVar.token.value);
    _ = try writer.write(",\n");
    for (self.branches[0..]) |*branch| {
      try addDepth(&writer, depth + 2);
      _ = try writer.write(try branch.render(depth + 2, al));
      _ = try writer.write("\n");
    }
    try addDepth(&writer, depth + 1);
    _ = try writer.write(")\n");
    return writer.context.items;
  }
    
  fn clone(self: *@This(), al: std.mem.Allocator) *Result {
    var swch = @This().init(self.occ.clone(al), self.token);
    swch.branches[0] = self.branches[0].clone(al);
    swch.branches[1] = self.branches[1].clone(al);
    return util.box(Result, .{.swch = swch}, al);
  }

  inline fn transform(self: *@This(), comptime Transformer: type, t: *Transformer) !*ast.AstNode {
    return t.tSwitch(self);
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

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    return switch (self.*) {
      .cons => |*cons| try cons.render(depth, al),
      .wildc => |*wc| try wc.render(depth, al),
    };
  }
};

/// Branch
///  -> {Cons | Wildcard : Switch | Guard | Leaf | Fail}
const Branch = struct {
  /// Cons | Wildcard
  lhs: Test,
  /// Switch | Guard | Leaf | Fail
  rhs: *Result,

  fn clone(self: *@This(), al: std.mem.Allocator) @This() {
    const lhs: Test = if (self.lhs.isConstructor()) .{.cons = self.lhs.cons.clone(al)} else .{.wildc = self.lhs.wildc.clone(al)};
    const rhs = self.rhs.clone(al);
    return .{.lhs = lhs, .rhs = rhs};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try addDepth(&writer, depth + 1);
    _ = try writer.write("test(\n");
    try addDepth(&writer, depth + 1);
    _ = try writer.write(try Test.render(&self.lhs, depth + 2, al));
    _ = try writer.write(",\n");
    try addDepth(&writer, depth + 2);
    _ = try writer.write(try self.rhs.render(depth + 2, al));
    try addDepth(&writer, depth + 1);
    _ = try writer.write(")");
    return writer.context.items;
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

  pub fn clone(self: *@This(), al: std.mem.Allocator) *@This() {
    const variant: MatchVariant = switch (self.*) {
      .cons => |*c| .{.cons = @constCast(c).clone(al)},
      .mrel => |*m| .{.mrel = @constCast(m).clone(al)},
      .rel => |r| .{.rel = r.clone(al)},
      .vari => |v| .{.vari = v.clone(al)},
      .wildc => |w| .{.wildc = w.clone(al)},
    };
    return util.box(MatchVariant, variant, al);
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    return switch (self.*) {
      inline else => |*this| try this.render(depth, al),
    };
  }
};

/// Builtin Constructors
pub const ConsTag = enum (u8) {
  Or,
  List,
  Tuple,
  Map,
  Err,
  Literal,
  Other,
};

/// case P1...Pn [guard] => E
pub const Case = struct {
  pattern: *Pattern,
  guard: ?*ast.AstNode,
  body: Body,

  pub fn init(pattern: *Pattern, guard: ?*ast.AstNode, body: *ast.AstNode, rev: bool, al: std.mem.Allocator) @This() {
    return @This(){.pattern = pattern, .guard = guard, .body = Body.init(body, rev, al)};
  }

  pub fn  new(pattern: *Pattern, guard: ?*ast.AstNode, body: *ast.AstNode, rev: bool, al: std.mem.Allocator) *@This() {
    const case = @This().init(pattern, guard, body, rev, al);
    return util.box(Case, case, al);
  }

  pub fn from(self: *@This(), pat: *Pattern, al: std.mem.Allocator) *@This() {
    const guard = if (self.guard) |guard| guard.clone(al) else self.guard;
    const case = @This(){.pattern = pat, .guard = guard, .body = self.body.clone(al)};
    return util.box(Case, case, al);
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *@This() {
    const guard = if (self.guard) |guard| guard.clone(al) else self.guard;
    const case = @This(){.pattern = self.pattern.clone(al), .guard = guard, .body = self.body.clone(al)};
    return util.box(Case, case, al);
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(try self.pattern.render(depth, al));
    if (self.guard) |_| {
      _ = try writer.write(" If [guard]");
    }
    _ = try writer.write(" => ");
    _ = try writer.write(try self.body.render(depth, al));
    _ = try writer.write("\n");
    return writer.context.items;
  }
};

pub const MatchCompiler = struct {
  allocator: std.mem.Allocator,
  diag: *Diagnostic,
  namegen: util.NameGen,
  /// store (body) nodes that may/may not have been included in the decision tree
  failure: ds.ArrayHashMap(*Node, FailStatus),
  /// select a constructor to test based on some heuristic
  heuristic: SelectionHeuristic,
  dtt: DecisionTreeTransformer,

  const Self = @This();
  const Node = ast.AstNode;
  const CaseList = ds.ArrayList(*Case);
  pub const MatchError = error{MatchError};

  /// determine if the body of this case was included in the decision tree or not.
  /// every body included in the decision tree / result ends up being removed from
  /// `self.failure`, that is, `removed` here is set to false. 
  const FailStatus = struct {
    removed: bool,
    case: *Case,

    pub fn init(removed: bool, case: *Case) @This() {
      return @This(){.removed = removed, .case = case};
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

    fn init(al: std.mem.Allocator) @This() {
      return @This(){
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

    fn isATiedConstructor(self: *@This(), mrel: *MultiRelation, name: []const u8) bool {
      defer self.disamb.clearRetainingCapacity();
      for (mrel.relations.items()) |rel| {
        if (rel.pattern.isConstructor()) {
          const cons_name = rel.pattern.variant.cons.cname();
          if (self.disamb.get(cons_name)) |val| {
            self.disamb.put(cons_name, val + 1) catch {};
          } else {
            self.disamb.put(cons_name, 1) catch {};
          }
          if (self.disamb.get(name)) |val| {
            if (val > 1) {
              return true;
            }
          }
        }
      }
      return false;
    }

    fn selectTest(self: *@This(), cases: *CaseList) usize {
      // TODO: Need to separate generic Constructors from non-generic ones
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

  pub fn init(diag: *Diagnostic, tc: *TypeChecker, al: std.mem.Allocator) @This() {
    return @This() {
      .allocator = al,
      .diag = diag,
      .failure = ds.ArrayHashMap(*Node, FailStatus).init(al),
      .heuristic = SelectionHeuristic.init(al),
      .namegen = util.NameGen.init(al),
      .dtt = DecisionTreeTransformer.init(al, tc),
    };
  }

  inline fn newNode(self: *Self) *Node {
    return Node.create(self.allocator);
  }

  fn softError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
   self.diag.addDiagnostics(.DiagError, token, "Error: " ++ fmt, args);
  }

  fn hardError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) MatchError {
   self.diag.addDiagnostics(.DiagError, token, "Error: " ++ fmt, args);
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
    return std.mem.eql(u8, v1.AstVar.token.value, v2.AstVar.token.value);
  }

  inline fn isExpandedPattern(self: *Self, variant: *MatchVariant) bool {
    _ = self;
    return switch (variant.*) {
      .rel, .mrel => true,
      .wildc => |*wc| wc.generated,
      else => false,
    };
  }

  fn reportRedundantCases(self: *Self, node: *ast.MatchNode) void {
    // TODO: Refine this. For cases where the redundancy occurs from a pattern expansion,
    //  we may need to report the redundant pattern from the expansion.
    // NOTE: we do not handle redundancy checks for Ranges!
    _ = node;
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

  fn genWildcardToken(self: *Self, tok: ?Token) Token {
    _ = self;
    var token = if (tok) |token| Token.from(&token) else Token.getDefault();
    token.value = "_";
    token.ty = .TkIdent;
    return token;
  }

  fn genFreshVarToken(self: *Self, tok: ?Token, start: []const u8) Token {
    std.debug.assert(start.len != 0);
    var slice = if (start[0] == '$') start[1..] else start;
    var id = self.namegen.generate("${s}", .{slice});
    var token = if (tok) |token| Token.from(&token) else Token.getDefault();
    token.value = id;
    token.ty = .TkIdent;
    return token;
  }

  fn genFreshVarNode(self: *Self, token: ?Token, start: []const u8) *Node {
    var node = Node.create(self.allocator);
    node.* = .{.AstVar = ast.VarNode.init(self.genFreshVarToken(token, start))};
    return node;
  }

  /// save this case as a default fail. If its body gets included in the
  /// decision tree, this is updated to a 'removed' state indicating success.
  inline fn addFailure(self: *Self, case: *Case) void {
    if (self.failure.get(case.body.node) == null) {
      self.failure.set(case.body.node, FailStatus.init(false, case));
    }
  }

  inline fn removeFailure(self: *Self, case: *Case) void {
    self.failure.set(case.body.node, FailStatus.init(true, case));
  }

  /// convert the pattern in each case to relation patterns
  fn convertPtnToRelationPtn(self: *Self, m_expr: *Node, cases: *CaseList, save_guard: bool) void {
    _ = save_guard;
    for (cases.items()) |case| {
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
    var cases = CaseList.init(self.allocator);
    for (cons.args.items()) |pat| {
      cases.append(Case.from(case, pat, self.allocator));
    }
    self.convertPtnToRelationPtn(m_expr, &cases, false);
    return self.filterCases(m_expr, &cases);
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
    var cases = CaseList.init(self.allocator);
    var rel = _rel.?;
    var cons = &rel.pattern.variant.cons;
    for (cons.args.items()) |ptn| {
      var new_mrel = MultiRelation.init(self.allocator);
      for (mrel.relations.items()) |rl| {
        if (rl.pattern != rel.pattern) {
          new_mrel.relations.append(rl.clone(self.allocator));
        } else {
          new_mrel.relations.append(.{.ident = rel.ident, .pattern = ptn});
        }
      }
      var pat = self.boxPattern(Pattern.init(new_mrel.toVariant(self.allocator), ptn.token, ptn.alat));
      cases.append(Case.from(case, pat, self.allocator));
    }
    return self.filterCases(m_expr, &cases);
  }

  /// convert a capture to a variable declaration
  fn convertCaptureToVarDecl(self: *const @This(), expr: *Node, capture: *Node) *ast.AstNode {
    // ptn as y -> let y = ptn(a)
    var node = ast.AstNode.create(self.allocator);
    node.* = .{.AstVarDecl = ast.VarDeclNode.init(&capture.AstVar, expr, false)};
    return node;
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
      std.debug.assert(expr.isVariable());
      case.body.addNode(self.convertCaptureToVarDecl(expr, pattern.alat.alias.?));
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
  fn transformConstructor(self: *Self, cons: *Constructor, ident: *ast.VarNode) Constructor {
    // TODO: move `node` & `typ` to init
    var new = Constructor.init(cons.cname(), cons.builtin, cons.tag, cons.rested, self.allocator);
    new.node = cons.node;
    new.typ = cons.typ;
    new.synth_name = cons.synth_name;
    if (cons.targs) |targs| {
      new.targs = targs.clone(self.allocator).box();
    }
    self.resetIdCount();
    if (cons.tag != .Literal) {
      for (cons.args.items()) |arg| {
        var id = self.genFreshVarNode(ident.token, ident.token.value);
        new.args.append(self.boxPattern(Pattern.init(Variable.init(id).toVariant(self.allocator), arg.token, arg.alat)));
      }
    } else {
      std.debug.assert(cons.args.len() == 0);
    }
    return new;
  }

  const SubProblem = struct {cases_a: CaseList, cases_b: CaseList};

  fn generateSubProblems(self: *Self, occ: *Node, tcons: *Constructor, cases: *CaseList) !SubProblem {
    // 4. Create the two sub problems [A] and [B] as follows by iterating over all the clauses.
    // .   One of three cases can happen:
    var cases_a = CaseList.init(cases.allocator());
    var cases_b = CaseList.init(cases.allocator());
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
              // If tcons.args > cons.args, treat `cons` as a different constructor.
              // This leads to a bad effect of repeating test on constructor types, but is the best we can manage for now.
              if (tcons.args.len() != cons.args.len()) {
                std.log.debug("unequal constructor sizes: tcons {} and cons {}", .{tcons.args.len(), cons.args.len()});
                if (tcons.args.len() > cons.args.len()) {
                  cases_a.append(case);
                  cases_b.append(case);
                  continue;
                }
              }
              // FIXME: this is a hack. Needs a proper fix.
              else if (cons.tag != .Other and tcons.rested != cons.rested and cons.rested) {
                cases_b.append(case);
              }
              var mrel = MultiRelation.init(self.allocator);
              for (tcons.args.items(), cons.args.items()[0..tcons.args.len()]) |vr, pat| {
                self.capturePatternIfAliased(case, pat, vr.variant.vari.ident);
                mrel.relations.append(.{.ident = vr.variant.vari.ident, .pattern = pat});
              }
              var newp: ?Pattern = null;
              // if we're in a multirelation, select all relations /= ai is Pi since this is a multirelation
              if (_mrel) |mr| {
                for (mr.relations.items()) |rl| {
                  if (!self.varEql(rl.ident, rel.ident)) {
                    mrel.relations.append(rl);
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
              // (b). The clause contains a test a is D(P1, .. , Pn), .. REST .. where D ̸= C
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
    return .{.cases_a = cases_a, .cases_b = cases_b};
  }

  fn filterCases(self: *Self, m_expr: *Node, cases: *CaseList) CaseList {
    _ = m_expr;
    // 1. Push tests against bare variables a is y into the right hand sides using let y = a, so that all the remaining tests are against constructors.
    // `-> convert cases to relations, then promote variable relations to rhs of body
    var ncases = CaseList.init(self.allocator);
    start: for (0..cases.len()) |idx| {
      var case = cases.itemAt(idx);
      self.addFailure(case);
      if (case.pattern.isRelation()) {
        var rel = case.pattern.variant.rel;
        if (rel.pattern.isVariable()) {
          case.body.addNode(rel.varRelationToVarDecl(self.allocator));
          const pattern = self.boxPattern(
            Pattern.init(
              Wildcard.init(self.genWildcardToken(case.pattern.token), true).toVariant(self.allocator),
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
        var rels = ds.ArrayList(Relation).init(self.allocator);
        // a is Some(..), b is y,
        // exclude all tests m_expr is var (bare variables)
        for (mrel.relations.items()) |rel| {
          if (rel.pattern.isVariable()) {
            case.body.addNode(rel.varRelationToVarDecl(self.allocator));
          } else if (rel.pattern.isOrConstructor()) {
            // flatten or patterns
            if (self.convertOrConsToCasesFromMultiRelation(rel.ident, case)) |*flattened| {
              ncases.extend(flattened);
              continue :start;
            }
            rels.append(rel);
          } else {
            rels.append(rel);
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
                self.genWildcardToken(case.pattern.token), true).toVariant(self.allocator),
                case.pattern.token,
                case.pattern.alat
              )
            );
            case.pattern = pattern;
          }
        } else {
          // rels.clearAndFree();
        }
      }
      ncases.append(case);
    }
    return ncases;
  }

  fn compileCase(self: *Self, _m_expr: *Node, _cases: *CaseList) !*Result {
    std.log.debug("compiling.. _m_expr is {s}", .{_m_expr.AstVar.token.value});
    // 1. Push tests against bare variables a is y into the right hand sides using let y = a,
    // .  so that all the remaining tests are against constructors.
    var cases = self.filterCases(_m_expr, _cases);
    if (cases.isEmpty()) {
      return util.box(Result, .{.fail = .{}}, self.allocator);
    }
    // 2. Select one of the tests a is C(P1, . . . , Pn) in the first clause using some heuristic
    var bst = self.selectBestTest(&cases);
    if (bst == null) {
      var case = cases.itemAt(0);
      self.removeFailure(case);
      if (case.guard) |cond| {
        _ = cases.list.orderedRemove(0);
        var guard: Guard = .{.cond = cond, .body = case.body, .fallback = try self.compileCase(_m_expr, &cases)};
        return util.box(Result, .{.gard = guard}, self.allocator);
      } else {
        return util.box(Result, .{.leaf = case.body.clone(self.allocator)}, self.allocator);
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
    var tcons = self.transformConstructor(&rel.pattern.variant.cons, &m_expr.AstVar);
    const wildc = Wildcard.init(self.genWildcardToken(m_expr.getToken()), true);
    // 4. Create the two sub problems [A] and [B] as follows by iterating over all the clauses.
    var subp = try self.generateSubProblems(m_expr, &tcons, &cases);
    // 5. Recursively generate code for [A] and [B].
    var res_a = try self.compileCase(m_expr, &subp.cases_a);
    var res_b = try self.compileCase(m_expr, &subp.cases_b);
    swch.branches[0] = (.{.lhs = .{.cons = tcons}, .rhs = res_a});
    swch.branches[1] = (.{.lhs = .{.wildc = wildc}, .rhs = res_b});
    return util.box(Result, .{.swch = swch}, self.allocator);
  }

  /// This implements pattern matching as described by Jules Jacobs, with modifications & extensions.
  /// Some inspiration from Maranget is also utilized.
  pub fn compile(self: *Self, node: *ast.MatchNode) !*DecisionTree {
    // we match on m_expr
    std.log.debug("match ast dump:\n{s}\n", .{node.render(0, self.allocator) catch ""});
    var m_expr = node.expr;
    self.convertPtnToRelationPtn(m_expr, &node.cases, true);
    const tree = try self.compileCase(m_expr, &node.cases);
    std.log.debug("decision tree dump:\n{s}\n", .{tree.render(0, self.allocator) catch ""});
    self.reportRedundantCases(node);
    return tree;
  }

  pub fn lowerDecisionTree(self: *Self, tree: *DecisionTree, fail_token: Token) !*Node {
    const node = try self.dtt.transform(tree, fail_token);
    if (!node.isBlock()) {
      var tmp = ast.BlockNode.newEmptyBlock(self.allocator);
      tmp.block().nodes.append(node);
      return tmp;
    }
    return node;
  }
};

/// transform/lower a decision tree to if-else statements
pub const DecisionTreeTransformer = struct {
  allocator: std.mem.Allocator,
  fail_token: Token = undefined,
  tc: *TypeChecker,

  const Self = @This();
  const Node = ast.AstNode;
  const NodeList = ds.ArrayList(*Node);

  pub fn init(allocator: std.mem.Allocator, tc: *TypeChecker) Self {
    return Self {.allocator = allocator, .tc = tc};
  }

  inline fn andToken(self: *Self, token: Token) Token {
    _ = self;
    return Token.fromWithValue(&token, "and", .TkAnd);
  }

  inline fn isToken(self: *Self, token: Token) Token {
    _ = self;
    return Token.fromWithValue(&token, "is", .TkIs);
  }

  inline fn eqeqToken(self: *Self, token: Token) Token {
    _ = self;
    return Token.fromWithValue(&token, "==", .Tk2Eq);
  }

  inline fn leqToken(self: *Self, token: Token) Token {
    _ = self;
    return Token.fromWithValue(&token, "<=", .TkLeq);
  }

  inline fn geqToken(self: *Self, token: Token) Token {
    _ = self;
    return Token.fromWithValue(&token, ">=", .TkGeq);
  }

  fn newNumberNode(self: *Self, token: Token, val: f64) *Node {
    var node = Node.create(self.allocator);
    var ntoken = token;
    ntoken.ty = .TkNumber;
    node.* = .{.AstNumber = ast.LiteralNode.init(ntoken)};
    node.AstNumber.value = val;
    return node;
  }

  fn newTypeNode(self: *Self, ty: *Type, token: Token) *Node {
    var node = Node.create(self.allocator);
    node.* = .{.AstNType = ast.TypeNode.init(ty, token)};
    return node;
  }

  fn newIfElseNode(self: *Self, cond: *Node, then: *Node, els: *Node) *Node {
    var node = Node.create(self.allocator);
    node.* = .{.AstSimpleIf = ast.SimpleIfNode.init(cond, then, els)};
    return node;
  }

  /// ident -> ident[index]
  fn newSubscript(self: *Self, ident: *Node, idx: usize) *Node {
    // create index
    const tok = Token.fromWithValue(&ident.AstVar.token, "<num>", .TkNumber);
    const index = self.newNumberNode(tok, @floatFromInt(idx));
    // create ident[index]
    var sub_expr = Node.create(self.allocator);
    sub_expr.* = .{.AstSubscript = ast.SubscriptNode.init(ident.clone(self.allocator), index)};
    return sub_expr;
  }

  /// ident -> ident.field
  fn newFieldAccess(self: *Self, ident: *Node, field_name: []const u8) *Node {
    // create field
    var rhs = Node.create(self.allocator);
    const tok = Token.fromWithValue(&ident.AstVar.token, field_name, .TkIdent);
    rhs.* = .{.AstVar = ast.VarNode.init(tok)};
    // create ident.field
    var dot_expr = Node.create(self.allocator);
    dot_expr.* = .{.AstDotAccess = ast.DotAccessNode.init(ident.clone(self.allocator), rhs)};
    return dot_expr;
  }

  /// ident -> ident.method_name()
  fn newMethodCall(self: *Self, ident: *Node, method_name: []const u8) *Node {
    const dot_expr = self.newFieldAccess(ident, method_name);
    var call = Node.create(self.allocator);
    call.* = .{.AstCall = ast.CallNode.init(dot_expr, NodeList.init(self.allocator), null, 0, false, false)};
    return call;
  }

  /// creates a regular block with an 'entry' scope node
  inline fn newBlock(self: *Self) *Node {
    return ast.BlockNode.newEmptyBlock(self.allocator);
  }

  /// add an 'exit' scope node to this block
  fn addExitScopeToBlock(self: *Self, node: *Node) void {
    var scope = Node.create(self.allocator);
    scope.* = .{.AstScope = ast.ScopeNode.init(false, true)};
    node.block().nodes.append(scope);
  }

  /// transform constructor `C(var)` into `var = expr()`
  fn constructorMethodToVarDecl(self: *Self, cons: *Constructor, ident: *Node, method_name: []const u8) *Node {
    var decl = Node.create(self.allocator);
    std.debug.assert(cons.args.len() == 1);
    const call = self.newMethodCall(ident, method_name);
    decl.* = .{.AstVarDecl = ast.VarDeclNode.init(&cons.args.itemAt(0).variant.vari.ident.AstVar, call, false)};
    return decl;
  }

  /// transform constructor `C(var)` into `var = id[expr]`
  fn constructorSubscriptToVarDecl(self: *Self, arg: *Pattern, ident: *Node, index: usize) *Node {
    var decl = Node.create(self.allocator);
    const subsc = self.newSubscript(ident.clone(self.allocator), index);
    decl.* = .{.AstVarDecl = ast.VarDeclNode.init(&arg.variant.vari.ident.AstVar, subsc, false)};
    return decl;
  }

  /// transform field access in a constructor to a var decl, for ex:
  /// Cons(id) => id = occ.field
  fn fieldAccessToVarDecl(self: *Self, source: *Node, field: *ast.VarNode, target: *Node) *Node {
    var decl = Node.create(self.allocator);
    const access = self.newFieldAccess(source, field.token.value);
    decl.* = .{.AstVarDecl = ast.VarDeclNode.init(&target.AstVar, access, false)};
    return decl;
  }

  fn elsOrToBlock(self: *Self, els: *Node) *Node {
    if (!els.isBlock()) {
      const els_body = self.newBlock();
      els_body.block().nodes.append(els);
      return els_body;
    }
    return els;
  }

  /// Switch (occ) test (..., body(..)) test (Wildcard(_) body(..)) ->
  /// if (occ op test(..)) body(..) else ...
  fn tSwitch(self: *Self, swch: *Switch) !*Node {
    // transpile if condition
    var if_branch = swch.branches[0];
    var els_branch = swch.branches[1];
    // make occ is test(..)
    std.debug.assert(if_branch.lhs.isConstructor());
    const ty = if_branch.lhs.cons.typ.?.classOrInstanceClass();
    if (ty.isUnion()) {
      return self.tc.error_(
        true, swch.token,
        "cannot match on constructor with ambiguous multiple types.\n\t" ++
        "Consider narrowing one of this type: '{s}'",
        .{ty.typename(self.allocator)}
      );
    }
    var node = self.newBlock();
    var if_cond: *Node = undefined;
    var if_body = self.newBlock();
    var id = swch.occ.clone(self.allocator);
    var cons = &if_branch.lhs.cons;
    switch (cons.tag) {
      .List, .Tuple, .Map, .Err, .Other => {
        var lhs = Node.create(self.allocator);
        const tyn = self.newTypeNode(ty, swch.token);
        lhs.* = .{.AstBinary = ast.BinaryNode.init(id, tyn, self.isToken(swch.token))};
        // list/tuple
        if (cons.tag == .List or cons.tag == .Tuple) {
          // length check & rested: id.len() (>= || ==) cons.args.len
          var rhs = Node.create(self.allocator);
          const len = self.newNumberNode(swch.token, @floatFromInt(cons.args.len()));
          const op = if (cons.rested) self.geqToken(swch.token) else self.eqeqToken(swch.token);
          rhs.* = .{.AstBinary = ast.BinaryNode.init(self.newMethodCall(id, "len"), len, op)};
          if_cond = Node.create(self.allocator);
          if_cond.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, self.andToken(swch.token))};
          lhs.AstBinary.allow_rested = cons.rested;
          for (cons.args.items(), 0..) |arg, index| {
            // don't capture wildcard patterns
            if (arg.isWildcard()) continue;
            // transform vars to var decl
            if_body.block().nodes.append(self.constructorSubscriptToVarDecl(arg, id, index));
          }
        } else if (cons.tag == .Map) {
          // transform constructor `map(var)` into `var = id.listItems()`
          if_body.block().nodes.append(self.constructorMethodToVarDecl(cons, id, "listItems"));
          if_cond = lhs;
        } else if (cons.tag == .Err) {
          // transform constructor `err(var)` into `var = id.value()`
          if_body.block().nodes.append(self.constructorMethodToVarDecl(cons, id, "value"));
          if_cond = lhs;
        } else {
          // .Other -> validate fields
          // C(a, b, ..)
          var cls = ty.klass();
          if (cons.args.isNotEmpty()) {
            // verify class fields.
            const tyname = ty.typename(self.allocator);
            for (cons.args.items(), cls.fields.items()) |arg, field| {
              // don't capture wildcard patterns
              if (arg.isWildcard()) continue;
              if (arg.alat.hasField()) {
                var fd = &arg.alat.field.?.AstVar;
                if (cls.getField(fd.token.value) == null) {
                  self.tc.softError(fd.token, "type '{s}' has no field '{s}'", .{tyname, fd.token.value});
                  continue;
                } else {
                  // transform field access to a var decl
                  if_body.block().nodes.append(self.fieldAccessToVarDecl(id, fd, arg.variant.vari.ident));
                  continue;
                }
              }
              // transform field access - by position, to a var decl
              if_body.block().nodes.append(self.fieldAccessToVarDecl(id, field.AstVarDecl.ident, arg.variant.vari.ident));
            }
            if (self.tc.diag.hasErrors()) {
              return error.CheckError;
            }
          }
          if_cond = lhs;
        }
      },
      .Literal => {
        if_cond = Node.create(self.allocator);
        if_cond.* = .{.AstBinary = ast.BinaryNode.init(id, cons.node.?, self.eqeqToken(swch.token))};
      },
      else => unreachable,
    }
    // make if body
    var body = try self.tDecision(if_branch.rhs);
    if (body.isBlock()) {
      if_body.block().nodes.extend(&body.block().nodes);
    } else {
      if_body.block().nodes.append(body);
    }
    // make else:
    // for else, test is always a wildcard, so transpile the rhs directly
    std.debug.assert(els_branch.lhs.isWildcard());
    const ife = self.newIfElseNode(
      if_cond.toMatchCondition(self.allocator),
      if_body,
      self.elsOrToBlock(try self.tDecision(els_branch.rhs))
    );
    node.block().nodes.append(ife);
    if (node.block().nodes.len() == 1) {
      return node.block().nodes.getLast();
    } else {
      return node;
    }
  }

  fn tLeaf(self: *Self, leaf: *Leaf) !*Node {
    _ = self;
    if (leaf.reversed) {
      leaf.node.block().nodes.reverse();
      leaf.reversed = false;
    }
    return leaf.node;
  }

  fn tFail(self: *Self, fail: *Fail) !*Node {
    _ = fail;
    var node = Node.create(self.allocator);
    node.* = .{.AstFailMarker = ast.MarkerNode.init(self.fail_token)};
    return node;
  }

  fn tGuard(self: *Self, gard: *Guard) !*Node {
    // create a block node to house the entire guard clause
    var block = ast.BlockNode.newEmptyBlock(self.allocator);
    // lift all declarations prior to liftmarker node into the new block
    var nodes = &(try self.tLeaf(&gard.body)).block().nodes;
    if (nodes.isNotEmpty()) {
      var delto: usize = 0;
      for (nodes.items(), 0..) |node, i| {
        delto = i;
        if (!node.isLiftMarker()) {
          block.block().nodes.append(node);
        } else {
          break;
        }
      }
      // remove all declarations up to liftmarker
      nodes.replaceRange(0, delto + 1, &[_]*Node{});
    }
    block.block().nodes.append(
      self.newIfElseNode(
        gard.cond.toMatchCondition(self.allocator),
        gard.body.node,
        self.elsOrToBlock(try self.tDecision(gard.fallback))
      )
    );
    return block;
  }

  fn tDecision(self: *Self, dt: *DecisionTree) anyerror!*Node {
    return switch (dt.*) {
      inline else => |*node| @call(.always_inline, @TypeOf(node.*).transform, .{node, @This(), self}),
    };
  }

  pub fn transform(self: *Self, tree: *DecisionTree, fail_token: Token) !*Node {
    self.fail_token = fail_token;
    return @call(.always_inline, Self.tDecision, .{self, tree});
  }
};
