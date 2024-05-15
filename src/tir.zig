pub const std = @import("std");
pub const util = @import("util.zig");
pub const ptn = @import("pattern.zig");
pub const lex = @import("lex.zig");
pub const ds = @import("ds.zig");
const VebAllocator = @import("allocator.zig");

pub const ks = lex.ks;
pub const Token = lex.Token;
pub const IdentToken = lex.IdentToken;
pub const TokenBit = lex.TokenBit;
pub const TokenType = lex.TokenType;
pub const NodeListU = ds.ArrayListUnmanaged(*Node);
pub const NodeList = ds.ArrayList(*Node);
pub const Allocator = std.mem.Allocator;
pub const NodeItems = Items(*Node);
pub const TypeItems = Items(*Type);
pub const ParamItems = Items(*ParamNode);
pub const TypeHashSet = ds.ArrayHashMapUnmanaged(u32, *Type);
pub const TypeList = ds.ArrayListUnmanaged(*Type);

pub fn Items(comptime T: type) type {
  return []T;
}

// tir node types
pub const NodeType = enum {
  NdNumber,
  NdString,
  NdBool,
  NdBinary,
  NdAssign,
  NdSubscript,
  NdUnary,
  NdList,
  NdTuple,
  NdMap,
  NdExprStmt,
  NdParam,
  NdField,
  NdPubField,
  NdVarDecl,
  NdBlock,
  NdType,
  NdAlias,
  NdDeref,
  NdCondition,
  NdMCondition,
  NdEmpty,
  NdCast,
  NdSimpleIf,
  NdWhile,
  NdControl,
  NdBasicCall,
  NdGenericCall,
  NdError,
  NdOrElse,
  NdBasicFun,
  NdGenericFun,
  NdDotAccess,
  NdClass,
  NdLblArg,
  NdMatch,
  NdFailMarker,
  NdRedunMarker,
  NdDiagStartMarker,
  NdRet,
  NdScope,
  NdTVar,
  NdProgram,
};

fn cloneNodeItems(items: NodeItems, al: Allocator) NodeItems {
  const elems = util.allocSlice(*Node, items.len, al);
  for (items, 0..) |itm, i| {
    @setRuntimeSafety(false);
    elems[i] = itm.clone(al);
  }
  return elems;
}

fn allocate(comptime T: type, alloc: Allocator) *T {
  return alloc.create(T) catch |e| {
    std.debug.print("{}", .{e});
    std.os.exit(1);
  };
}

pub fn reverseItems(_items: *NodeItems) void {
  if (_items.*.len == 0) return;
  var items = _items.*;
  var j = items.len - 1;
  for (items, 0..) |itm, i| {
    if (i >= j) break;
    items[i] = items[j];
    items[j] = itm;
    j -= 1;
  }
  _items.* = items;
}

/// NdNumber
pub const NumberNode = struct {
  token: IdentToken,
  value: f64,
  typ: ?*Type = null,

  pub inline fn init(token: Token, value: f64) @This() {
    return @This(){.token = IdentToken.init(token), .value = value};
  }

  pub fn lexeme(self: *@This(), al: Allocator) []const u8 {
    if (self.value >= 0) return self.token.lexeme();
    return std.fmt.allocPrint(al, "{}", .{self.value}) catch self.token.lexeme();
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = try u8w.writer().write(
      std.fmt.allocPrint(u8w.allocator(), "{d}", .{self.value})
      catch self.token.lexeme()
    );
  }
};

/// NdString
pub const StringNode = struct {
  token: Token,
  typ: ?*Type = null,

  pub inline fn init(token: Token) @This() {
    return @This(){.token = token};
  }

  pub inline fn isAlloc(self: *@This()) bool {
    return self.token.ty == .TkEscString;
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = try u8w.writer().write(self.token.lexeme());
  }
};

/// NdBool
pub const BoolNode = struct {
  token: Token,
  typ: ?*Type = null,

  pub inline fn init(token: Token) @This() {
    return @This(){.token = token};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = try u8w.writer().write(self.token.lexeme());
  }
};

/// NdBinary
pub const BinaryNode = struct {
  // narrowing meta
  allow_rested: bool = false,
  allow_consts: bool = false,
  // op meta
  op_origin: lex.SrcKind,
  op_tkty: TokenType,
  op_offset: u32,
  // nodes
  left: *Node,
  right: *Node,
  typ: ?*Type = null,

  pub inline fn init(left: *Node, right: *Node, op_token: Token) @This() {
    return @This(){
      .left = left,
      .right = right,
      .op_tkty = op_token.ty,
      .op_offset = op_token.offset,
      .op_origin = op_token.src_kind,
    };
  }

  pub inline fn initRested(left: *Node, right: *Node, op_token: Token) @This() {
    return @This(){
      .left = left,
      .right = right,
      .op_tkty = op_token.ty,
      .op_offset = op_token.offset,
      .op_origin = op_token.src_kind,
      .allow_rested = true
    };
  }

  pub fn clone(self: *@This(), node: *Node, al: Allocator) *Node {
    return switch (node.*) {
      .NdBinary => Node.new(.{.NdBinary = .{
        .left = self.left.clone(al),
        .right = self.right.clone(al),
        .op_tkty = self.op_tkty,
        .op_offset = self.op_offset,
        .op_origin = self.op_origin,
        .allow_rested = self.allow_rested,
        .allow_consts = self.allow_consts,
      }}, al),
      .NdAssign => Node.new(.{.NdAssign = .{
        .left = self.left.clone(al),
        .right = self.right.clone(al),
        .op_tkty = self.op_tkty,
        .op_offset = self.op_offset,
        .op_origin = self.op_origin,
        .allow_rested = self.allow_rested,
        .allow_consts = self.allow_consts,
      }}, al),
      else => unreachable,
    };
  }

  pub fn line(self: *BinaryNode) u32 {
    return self.left.getToken().line;
  }

  pub inline fn optype(self: *BinaryNode) OpType {
    return self.op_tkty.optype();
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try Node.render(self.left, depth, u8w);
    _ = try writer.write(" ");
    _ = try writer.write(self.op_tkty.str());
    _ = try writer.write(" ");
    try Node.render(self.right, depth, u8w);
    if (self.allow_rested) {
      _ = try writer.write(" [rested] ");
    }
  }
};

/// NdUnary
pub const UnaryNode = struct {
  expr: *Node,
  op: lex.Optr,
  typ: ?*Type = null,

  pub inline fn init(expr: *Node, op_token: Token) @This() {
    return @This(){.expr = expr, .op = lex.Optr.init(op_token)};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdUnary = .{.expr = self.expr.clone(al), .op = self.op}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    _ = try writer.write(self.op.str());
    try Node.render(self.expr, depth, u8w);
  }
};

/// NdList, NdTuple
pub const ListNode = struct {
  elems: NodeItems,
  typ: ?*Type = null,

  pub inline fn init(elems: NodeItems) @This() {
    return @This() {.elems = elems};
  }

  pub fn clone(self: *@This(), node: *Node, al: Allocator) *Node {
    const elems = cloneNodeItems(self.elems, al);
    return switch (node.*) {
      .NdList => Node.new(.{.NdList = .{.elems = elems}}, al),
      .NdTuple => Node.new(.{.NdTuple = .{.elems = elems}}, al),
      else => unreachable,
    };
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var writer = u8w.writer();
    if (self.elems.len == 0) {
      _ = try writer.write(" []");
      return;
    }
    _ = try writer.write(" [");
    const len = self.elems.len - 1;
    for (self.elems, 0..) |itm, i| {
      try Node.render(itm, depth, u8w);
      if (i < len) _ = try writer.write(",\n");
    }
    _ = try writer.write("]");
  }
};

/// NdMap
pub const MapNode = struct {
  pairs: Items(Pair),
  typ: ?*Type = null,

  pub const Pair = struct {key: *Node, value: *Node};

  pub inline fn init(pairs: []Pair) @This() {
    return @This() {.pairs = pairs};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    const pairs = util.allocSlice(Pair, self.pairs.len, al);
    for (self.pairs, 0..) |pair, i| {
      pairs[i] = .{.key = pair.key.clone(al), .value = pair.value.clone(al)};
      @setRuntimeSafety(false);
    }
    return Node.new(.{.NdMap = .{.pairs = pairs}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = self;
    _ = depth;
    _ = try u8w.writer().write("Map(args..)");
  }
};

/// NdTVar
pub const TVarNode = struct {
  token: Token,
  typ: ?*Type = null,

  pub inline fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub inline fn initType(token: Token, typ: ?*Type) @This() {
    return @This() {.token = token, .typ = typ};
  }

  pub inline fn box(self: *const @This(), al: Allocator) *@This() {
    const new = util.alloc(@This(), al);
    new.* = self.*;
    return new;
  }

  pub inline fn isGeneratedVar(self: *@This()) bool {
    return self.token.lexeme()[0] == ks.GeneratedVarMarker;
  }

  pub fn valueEql(self: *const @This(), other: anytype) bool {
    const T = @TypeOf(other);
    if (T == *Node) {
      return std.mem.eql(u8, self.token.lexeme(), other.NdTVar.token.lexeme());
    } else if (T == []const u8) {
      return std.mem.eql(u8, self.token.lexeme(), other);
    } else if (T == Token) {
      return std.mem.eql(u8, self.token.lexeme(), other.lexeme());
    } else {
      @panic("invalid value type");
    }
  }

  pub fn dryClone(self: *@This(), al: Allocator) *@This() {
    const typ = if (self.typ) |ty| ty.clone(al) else null;
    return @as(@This(), .{.token = self.token, .typ = typ}).box(al);
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    const typ = if (self.typ) |ty| ty.clone(al) else null;
    return Node.new(.{.NdTVar = .{.token = self.token, .typ = typ}}, al);
  }

  pub inline fn value(self: *const @This()) []const u8 {
    return self.token.lexeme();
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = try u8w.writer().write(self.token.lexeme());
  }
};

/// NdExprStmt
pub const ExprStmtNode = struct {
  expr: *Node,
  has_sugar: bool = false,

  pub inline fn init(expr: *Node) @This() {
    return @This() {.expr = expr};
  }

  pub inline fn initAll(expr: *Node, has_sugar: bool) @This() {
    return @This() {.expr = expr, .has_sugar = has_sugar};
  }

  pub fn isSelfDotAccessAssignment(self: *@This()) ?*DotAccessNode {
    const expr = self.expr;
    if (expr.isAssign() and expr.NdAssign.optype() == .OpAssign) {
      if (expr.NdAssign.left.isDotAccess()) {
        if (expr.NdAssign.left.NdDotAccess.lhs.isTVariable()) {
          if (std.mem.eql(u8, ks.SelfVar, expr.NdAssign.left.NdDotAccess.lhs.NdTVar.value())) {
            return &expr.NdAssign.left.NdDotAccess;
          }
        }
      }
    }
    return null;
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdExprStmt = .{
      .expr = self.expr.clone(al), 
      .has_sugar = self.has_sugar,
      }}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    try self.expr.render(depth, u8w);
    _ = try u8w.writer().write("\n");
  }
};

/// NdParam
pub const ParamNode = struct {
  name: Token,
  typ: *Type,

  pub inline fn init(name: Token, typ: *Type) @This() {
    return @This() {.name = name, .typ = typ};
  }

  pub inline fn new(name: Token, typ: *Type, al: Allocator) *@This() {
    const obj = util.alloc(ParamNode, al);
    obj.* = @This(){.name = name, .typ = typ};
    return obj;
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdParam = .{
        .name = self.name,
        .typ = self.typ.clone(al),
      }}, al);
  }

  pub fn dryClone(self: *@This(), al: Allocator) *ParamNode {
    return ParamNode.new(self.name, self.typ.clone(al), al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    var decl = std.fmt.allocPrint(u8w.allocator(), "{s}: typ", .{self.name.lexeme()}) catch unreachable;
    _ = try writer.write(decl);
  }
};

/// NdField
pub const FieldNode = struct {
  name: IdentToken,
  value: ?*Node,
  typ: ?*Type,

  pub inline fn init(name: Token, value: ?*Node, typ: ?*Type) @This() {
    return @This() {.name = IdentToken.init(name), .value = value, .typ = typ};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdField = .{
        .name = self.name,
        .value = if (self.value) |val| val.clone(al) else null,
        .typ = if (self.typ) |typ| typ.clone(al) else null,
      }}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    var decl = std.fmt.allocPrint(u8w.allocator(), "{s}: typ", .{self.name.lexeme()}) catch unreachable;
    _ = try writer.write(decl);
  }
};

/// NdPubField
pub const PubFieldNode = struct {
  name: IdentToken,
  value: ?*Node,
  typ: ?*Type,

  pub inline fn init(name: Token, value: ?*Node, typ: ?*Type) @This() {
    return @This() {.name = IdentToken.init(name), .value = value, .typ = typ};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdPubField = .{
        .name = self.name,
        .value = if (self.value) |val| val.clone(al) else null,
        .typ = if (self.typ) |typ| typ.clone(al) else null,
      }}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    var decl = std.fmt.allocPrint(u8w.allocator(), "{s}: typ", .{self.name.lexeme()}) catch unreachable;
    _ = try writer.write(decl);
  }
};

/// NdVarDecl
pub const VarDeclNode = struct {
  name: IdentToken,
  value: *Node,
  typ: ?*Type,

  pub inline fn init(name: Token, value: *Node, typ: ?*Type) @This() {
    return @This() {.name = IdentToken.init(name), .value = value, .typ = typ};
  }

  pub inline fn box(self: *const @This(), al: Allocator) *@This() {
    const new = util.alloc(VarDeclNode, al);
    new.* = self.*;
    return new;
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdVarDecl = .{
        .name = self.name,
        .value = self.value.clone(al),
        .typ = if (self.typ) |typ| typ.clone(al) else null,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    var decl = std.fmt.allocPrint(u8w.allocator(), "let {s} = ", .{self.name.lexeme()}) catch unreachable;
    _ = try writer.write(decl);
    try Node.render(self.value, depth, u8w);
    _ = try writer.write("\n");
  }
};

/// NdBlock
pub const BlockNode = struct {
  nodes: NodeItems,
  checked: bool = false,
  scoped: bool = false,

  pub inline fn init(nodes: NodeItems) @This() {
    return @This() {.nodes = nodes};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdBlock = .{
      .nodes = cloneNodeItems(self.nodes, al),
      .scoped = self.scoped}
    }, al);
  }

  pub inline fn getLast(self: *@This()) *Node {
    return self.nodes[self.nodes.len - 1];
  }

  pub fn newBlockWithNodes(slice: NodeItems, alloc: Allocator) *Node {
    const items = util.allocSlice(*Node, slice.len, alloc);
    @memcpy(items, slice);
    return Node.new(.{.NdBlock = BlockNode.init(items)}, alloc);
  }

  pub inline fn newBlockWithOwnedNodes(slice: NodeItems, al: Allocator) *Node {
    return Node.new(.{.NdBlock = BlockNode.init(slice)}, al);
  }

  pub inline fn reverse(self: *@This()) void {
    reverseItems(&self.nodes);
  }

  pub fn prepend(self: *@This(), node: *Node, al: Allocator) void {
    const items = util.allocSlice(*Node, self.nodes.len + 1, al);
    items[0] = node;
    @memcpy(items[1..], self.nodes);
    self.nodes = items;
  }

  pub fn append(self: *@This(), node: *Node, al: Allocator) void {
    const items = util.allocSlice(*Node, self.nodes.len + 1, al);
    @memcpy(items[0..self.nodes.len], self.nodes);
    items[self.nodes.len] = node;
    self.nodes = items;
  }

  pub fn prependSlice(self: *@This(), nodes: NodeItems, al: Allocator) void {
    const items = util.allocSlice(*Node, self.nodes.len + nodes.len, al);
    @memcpy(items[0..nodes.len], nodes);
    @memcpy(items[nodes.len..], self.nodes);
    self.nodes = items;
  }

  pub fn appendSlice(self: *@This(), nodes: NodeItems, al: Allocator) void {
    const items = util.allocSlice(*Node, self.nodes.len + nodes.len, al);
    @memcpy(items[0..self.nodes.len], self.nodes);
    @memcpy(items[self.nodes.len..], nodes);
    self.nodes = items;
  }

  pub fn canAssumeEmpty(self: *@This(), comptime assume: fn (*Node) callconv(.Inline) bool) bool {
    for (self.nodes) |itm| {
      if (!assume(itm)) {
        return false;
      }
    }
    return true;
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    _ = try writer.write("<block>\n");
    for (self.nodes) |node| {
      try Node.render(node, depth + 1, u8w);
    }
    try util.addDepth(&writer, depth);
    _ = try writer.write("</block>\n");
  }
};

/// NdType
pub const TypeNode = struct {
  /// track whether this type was created in an alias or annotation context
  from_alias_or_annotation: bool = false,
  /// whether type resolution should be skipped or not during type checking
  skip_type_resolution: bool = false,
  tkbit: TokenBit,
  typ: *Type,

  pub inline fn init(typ: *Type, token: Token) @This() {
    return @This() {.tkbit = TokenBit.init(token), .typ = typ};
  }

  pub fn dryClone(self: *@This(), al: Allocator) *TypeNode {
    return util.box(TypeNode, .{
        .tkbit = self.tkbit,
        .typ = self.typ.clone(al),
        .from_alias_or_annotation = self.from_alias_or_annotation
      }, al);
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdType = .{
        .tkbit = self.tkbit,
        .typ = self.typ.clone(al),
        .from_alias_or_annotation = self.from_alias_or_annotation,
        .skip_type_resolution = self.skip_type_resolution,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    self.typ.typenameInPlace(u8w);
  }
};

/// NdAlias
pub const AliasNode = struct {
  alias: *TypeNode,
  aliasee: *TypeNode,
  typ: *Type, // alias and aliasee is set in `typ`

  pub inline fn init(alias: *TypeNode, aliasee: *TypeNode) @This() {
    aliasee.typ.alias = alias.typ;
    alias.from_alias_or_annotation = true;
    aliasee.from_alias_or_annotation = true;
    return @This() {.alias = alias, .aliasee = aliasee, .typ = alias.typ};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<type alias>");
  }
};

/// NdDeref - null dereference: expr.?
pub const DerefNode = struct {
  tkbit: TokenBit,
  assertion: bool = false,
  expr: *Node,
  typ: ?*Type = null,

  pub inline fn init(expr: *Node, token: Token) @This() {
    return @This() {.expr = expr, .tkbit = TokenBit.init(token)};
  }

  pub inline fn initAssertion(expr: *Node, token: Token) @This() {
    return @This() {.expr = expr, .tkbit = TokenBit.init(token), .assertion = true};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdDeref = .{
        .expr = self.expr.clone(al),
        .tkbit = self.tkbit,
        .typ = if (self.typ) |typ| typ.clone(al) else null,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = try u8w.writer().write("(");
    try self.expr.render(depth, u8w);
    _ = try u8w.writer().write(").?");
  }
};

/// NdDotAccess
pub const DotAccessNode = struct {
  lhs: *Node,
  rhs: *Node,
  allow_tag_access: bool = false,
  is_desugared: bool = false,
  typ: ?*Type = null,

  pub inline fn init(lhs: *Node, rhs: *Node) @This() {
    return @This() { .lhs = lhs, .rhs = rhs};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdDotAccess = .{
        .lhs = self.lhs.clone(al),
        .rhs = self.rhs.clone(al),
        .typ = if (self.typ) |typ| typ.clone(al) else null,
        .allow_tag_access = self.allow_tag_access,
        .is_desugared = self.is_desugared,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try Node.render(self.lhs, depth, u8w);
    _ = try writer.write(".");
    try Node.render(self.rhs, depth, u8w);
  }
};

/// NdSubscript
pub const SubscriptNode = struct {
  expr: *Node,
  index: *Node,
  typ: ?*Type = null,

  pub inline fn init(expr: *Node, index: *Node) @This() {
    return @This() { .expr = expr, .index = index};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdSubscript = .{
        .expr = self.expr.clone(al),
        .index = self.index.clone(al),
        .typ = if (self.typ) |typ| typ.clone(al) else null
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try Node.render(self.expr, depth, u8w);
    _ = try writer.write("[");
    try Node.render(self.index, depth, u8w);
    _ = try writer.write("]");
  }
};

/// NdCondition
pub const ConditionNode = struct {
  cond: *Node,
  has_never_typ_in_false_path: bool = false,
  is_from_loop: bool = false,

  pub inline fn init(cond: *Node) @This() {
    return @This() {.cond = cond};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdCondition = .{
        .cond = self.cond.clone(al),
        .has_never_typ_in_false_path = self.has_never_typ_in_false_path,
        .is_from_loop = self.is_from_loop,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    return Node.render(self.cond, depth, u8w);
  }
};

/// NdMCondition - wrapper for a condition/test generated from a match pattern
pub const MatchConditionNode = struct {
  tst: *Node,

  pub inline fn init(cond: *Node) @This() {
    return @This() {.tst = cond};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdMCondition = .{.tst = self.tst.clone(al)}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    return Node.render(self.tst, depth, u8w);
  }
};

/// NdEmpty
pub const EmptyNode = struct {
  token: Token,

  pub inline fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<empty>\n");
  }
};

/// NdCast
pub const CastNode = struct {
  expr: *Node,
  typn: *TypeNode,

  pub inline fn init(expr: *Node, typn: *TypeNode) @This() {
    return @This() {.expr = expr, .typn = typn};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdCast = .{
        .expr = self.expr.clone(al),
        .typn = self.typn.dryClone(al)
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<cast>");
  }
};

/// NdSimpleIf
pub const SimpleIfNode = struct {
  cond: *Node,
  then: *Node,
  els: *Node,

  pub inline fn init(cond: *Node, then: *Node, els: *Node) @This() {
    return @This() {.cond = cond, .then = then, .els = els};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdSimpleIf = .{
        .cond = self.cond.clone(al),
        .then = self.then.clone(al),
        .els = self.els.clone(al)
      }}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    _ = try writer.write("if (");
    try Node.render(self.cond, depth, u8w);
    _ = try writer.write(")\n");
    try util.addDepth(&writer, depth);
    try Node.render(self.then, depth, u8w);
    try util.addDepth(&writer, depth);
    _ = try writer.write("else\n");
    try util.addDepth(&writer, depth);
    try Node.render(self.els, depth, u8w);
    try util.addDepth(&writer, depth);
    _ = try writer.write("end\n");
  }
};

/// NdWhile
pub const WhileNode = struct {
  cond: *Node,
  then: *Node,

  pub inline fn init(cond: *Node, then: *Node) @This() {
    return @This() {.cond = cond, .then = then};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdWhile = .{.cond = self.cond.clone(al), .then = self.then.clone(al)}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<while>\n");
  }
};

/// NdControl
pub const ControlNode = struct {
  token: Token,
  /// the slot in which this node is compiled to an instruction
  patch_index: usize = 0,

  pub inline fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub fn isBreak(self: ControlNode) bool {
    return self.token.ty == .TkBreak;
  }

  pub fn isContinue(self: ControlNode) bool {
    return self.token.ty == .TkContinue;
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<control>\n");
  }
};

/// NdBasicCall
pub const BasicCallNode = struct {
  _args: [*]*Node,
  _len: u16,
  _va_start: u32 = 0,
  variadic: bool = false,
  labeled: bool = false,
  expr: *Node,
  typ: ?*Type = null,

  pub inline fn init(expr: *Node, _args: NodeItems) @This() {
    return @This() {.expr = expr, ._args = _args.ptr, ._len = @intCast(_args.len)};
  }

  pub inline fn args(self: *@This()) NodeItems {
    return self._args[0..self._len];
  }

  pub inline fn isVariadic(self: *@This()) bool {
    return self.variadic;
  }

  pub inline fn isLabeled(self: *@This()) bool {
    return self.labeled;
  }

  pub inline fn copyCall(self: *@This(), call: *CallNode, typ: *Type) void {
    self._va_start = @intCast(call.va_start);
    self.variadic = call.variadic;
    self.typ = typ;
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdBasicCall = .{
        .expr = self.expr.clone(al),
        ._args = cloneNodeItems(self.args(), al).ptr,
        ._len = self._len,
        ._va_start = self._va_start,
        .labeled = self.labeled,
        .variadic = self.variadic,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    try Node.render(self.expr, depth, u8w);
    if (self._len == 0) {
      _ = try writer.write("()");
      return;
    }
    _ = try writer.write("(");
    const len = self._len - 1;
    for (self.args(), 0..) |itm, i| {
      try Node.render(itm, depth, u8w);
      if (i < len) _ = try writer.write(",\n");
    }
    _ = try writer.write(")");
  }
};


/// NdGenericCall
pub const GenericCallNode = struct {
  targs: TypeItems,
  /// NdBasicCall
  call: *Node,

  pub inline fn init(call: *Node, targs: TypeItems) @This() {
    return @This() {.call = call, .targs = targs};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdGenericCall = .{.call = self.call.clone(al), .targs = self.targs}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    _ = try writer.write("{");
    const len = self.targs.len - 1;
    for (self.targs, 0..) |itm, i| {
      itm.typenameInPlace(u8w);
      if (i < len) _ = try writer.write(", ");
    }
    _ = try writer.write("}");
    try Node.render(self.call, depth, u8w);
  }
};

pub const CallNode = struct {
  variadic: bool = false,
  labeled: bool = false,
  va_start: usize = 0,
  expr: *Node,
  targs: ?TypeItems = null,
  args: NodeItems,
  typ: ?*Type = null,

  pub inline fn init(
    expr: *Node, args: NodeItems, targs: ?TypeItems,
    va_start: usize, variadic: bool, labeled: bool,
    typ: ?*Type,
  ) @This() {
    return @This() {
      .expr = expr, .args = args, .targs = targs,
      .va_start = va_start, .variadic = variadic,
      .labeled = labeled, .typ = typ,
    };
  }

  pub inline fn isGeneric(self: *const @This()) bool {
    return self.targs != null;
  }

  pub fn transformVariadicArgs(self: *@This(), al: Allocator) void {
    var args = NodeListU.init();
    args.appendSlice(self.args[self.va_start..], al);
    var node = Node.new(.{.NdList = .{.elems = args.items()}}, al);
    if (self.args.len > 0) {
      self.args[self.va_start] = node;
      self.args = self.args[0..self.va_start + 1];
    } else {
      self.args = util.allocSlice(*Node, 1, al);
      self.args[0] = node;
    }
  }
};

/// NdError
pub const ErrorNode = struct {
  expr: *Node,
  typ: ?*Type = null,

  pub inline fn init(expr: *Node) @This() {
    return @This() {.expr = expr};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdError = .{.expr = self.expr.clone(al)}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) anyerror!void {
    var writer = u8w.writer();
    _ = try writer.write("(");
    try self.expr.render(depth, u8w);
    _ = try writer.write(")!");
  }
};

/// NdOrElse
pub const OrElseNode = struct {
  ok: *Node,
  err: *Node,
  evar: ?*TVarNode = null,
  typ: ?*Type = null,

  pub inline fn init(ok: *Node, err: *Node, evar: ?*TVarNode) @This() {
    return @This(){.ok = ok, .err = err, .evar = evar};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{.NdOrElse = .{.ok = self.ok.clone(al), .err = self.err.clone(al), .evar = self.evar}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    try self.ok.render(depth, u8w);
    _ = try u8w.writer().write(" orelse ");
    try self.err.render(depth, u8w);
  }
};

pub const FunData = struct {
  name: ?Token,
  body: *Node,
  ret: ?*Type,
  /// whether this function is builtin
  builtin: bool,
  /// whether this function is variadic
  variadic: bool,
  /// whether this function is public i.e. aspec = SpecPublic
  public: bool = false,
  /// whether all attributes (aspec public/private) can
  /// be accessed within this function without errors
  allow_all_aspec: bool = false,

  pub inline fn init(name: ?Token, body: *Node, ret: ?*Type, is_builtin: bool, variadic: bool, publ: bool) @This() {
    return @This() {
      .name = name, .body = body, .ret = ret,
      .builtin = is_builtin, .variadic = variadic,
      .public = publ
    };
  }

  pub fn clone(self: *FunData, al: Allocator) *FunData {
    return util.box(FunData, @as(FunData, .{
      .name = self.name,
      .body = self.body.clone(al),
      .ret = if (self.ret) |ret| ret.clone(al) else null,
      .builtin = self.builtin,
      .variadic = self.variadic,
      .public = self.public,
      .allow_all_aspec = self.allow_all_aspec,
    }), al);
  }
};

/// NdBasicFun
pub const BasicFunNode = struct {
  params: ParamItems,
  data: *FunData,

  pub inline fn init(
    params: ParamItems, name: ?Token, body: *Node, ret: ?*Type,
    is_builtin: bool, variadic: bool, publ: bool, al: Allocator
  ) @This() {
    return @This(){
      .params = params,
      .data = util.box(FunData, FunData.init(name, body, ret, is_builtin, variadic, publ), al)
    };
  }

  pub fn update(
    self: *@This(), params: ParamItems, name: ?Token, body: *Node,
    ret: ?*Type, is_builtin: bool, variadic: bool, publ: bool
  ) void {
    self.params = params;
    self.data.* = .{
      .name = name, .body = body, .ret = ret, .builtin = is_builtin,
      .variadic = variadic, .public = publ
    }; 
  }

  pub inline fn isAnonymous(self: @This()) bool {
    return self.data.name == null;
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    const params = util.allocSlice(*ParamNode, self.params.len, al);
    for (self.params, 0..) |param, i| {
      params[i] = param.dryClone(al);
    }
    return Node.new(.{.NdBasicFun = .{.params = params, .data = self.data.clone(al)}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var name: []const u8 = undefined;
    if (self.data.name) |n| {
      name = n.lexeme();
    } else {
      name = "<>";
    }
    _ = try u8w.writer().write("<def ");
    _ = try u8w.writer().write(name);
    _ = try u8w.writer().write(">\n");
    try self.data.body.render(depth + 1, u8w);
     _ = try u8w.writer().write("<end ");
    _ = try u8w.writer().write(name);
    _ = try u8w.writer().write(">\n");
  }
};

/// NdGenericFun
pub const GenericFunNode = struct {
  params: TypeItems,
  /// NdBasicFun
  fun: *Node,

  pub inline fn init(params: TypeItems, fun: *Node) @This() {
    return @This(){.params = params, .fun = fun};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<g.function>\n");
  }
};

/// NdClass
pub const ClassNode = struct {
  name: IdentToken,
  data: *ClassData,
  protos: ?*Type,

  pub const ClassData = struct {
    fields: NodeItems,
    methods: NodeItems,
    params: ?TypeItems,
    builtin: bool,
    checked: bool,
    tktype: TokenType,

    pub inline fn init(
      fields: NodeItems, methods: NodeItems, params: ?TypeItems,
      checked: bool, builtin: bool, tktype: TokenType
    ) @This() {
      return .{
        .fields = fields, .methods = methods,
        .params = params, .checked = checked,
        .builtin = builtin, .tktype = tktype
      };
    }

    pub fn clone(self: *ClassData, al: Allocator) *ClassData {
      var params: ?TypeItems = null;
      if (self.params) |_params| {
        params = util.allocSlice(*Type, _params.len, al);
        for (_params, 0..) |ty, i| {
          params.?[i] = ty.clone(al);
        }
      }
      return util.box(ClassData, @as(ClassData, .{
        .fields = cloneNodeItems(self.fields, al),
        .methods = cloneNodeItems(self.methods, al),
        .params = params,
        .builtin = self.builtin,
        .checked = self.checked,
        .tktype = self.tktype,
      }), al);
    }
  };

  pub inline fn init(
    name: Token, protos: ?*Type, fields: NodeItems, methods: NodeItems,
    params: ?TypeItems, checked: bool, builtin: bool, al: Allocator
  ) @This() {
    return @This() {
      .name = IdentToken.init(name),
      .protos = protos, 
      .data = util.box(
        ClassData, ClassData.init(
          fields, methods, params, checked, builtin, name.ty
        ), al
      )
    };
  }

  pub inline fn isParameterized(self: @This()) bool {
    return self.data.params != null;
  }

  pub const isGeneric = isParameterized;

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdClass = .{
        .name = self.name,
        .data = self.data.clone(al),
        .protos = if (self.protos) |protos| protos.clone(al) else null,
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<class>\n");
  }
};

/// NdLblArg
pub const LblArgNode = struct {
  label: []const u8,
  ident: *Node,
  value: *Node,

  pub inline fn init(label: Token, value: *Node, ident: *Node) @This() {
    return @This(){.label = label.lexeme(), .value = value, .ident = ident};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    return Node.new(.{
      .NdLblArg = .{
        .label = self.label,
        .ident = self.ident, // don't clone ident
        .value = self.value.clone(al),
      }
    }, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = depth;
    _ = self;
    _ = try u8w.writer().write("<lbl arg>");
  }
};

/// NdMatch
pub const MatchNode = struct {
  /// var decl if expr was converted to one
  /// match expr
  expr: *Node,
  /// case nodes
  cases: CaseItems,
  /// compiled lowered form
  lnode: ?*Node = null,

  pub const CaseItems = Items(*ptn.Case);

  pub inline fn init(expr: *Node, cases: CaseItems) @This() {
    return @This() {.expr = expr, .cases = cases};
  }

  pub inline fn getVariableOfInterest(self: *@This()) Token {
    return self.expr.NdTVar.token;
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    const cases = util.allocSlice(*ptn.Case, self.cases.len, al);
    for (self.cases, 0..) |case, i| {
      cases[i] = case.clone(al);
    }
    return Node.new(.{.NdMatch = .{.expr = self.expr.clone(al), .cases = cases}}, al);
  }

  pub fn cloneNode(self: *@This(), al: Allocator) Node {
    const cases = util.allocSlice(*ptn.Case, self.cases.len, al);
    for (self.cases, 0..) |case, i| {
      cases[i] = case.clone(al);
    }
    return .{.NdMatch = .{.expr = self.expr.clone(al), .cases = cases}};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    var writer = u8w.writer();
    if (self.lnode) |lnode| {
      try lnode.render(depth, u8w);
    } else {
      _ = try writer.write("match [expr]\n");
      for (self.cases) |case| {
        try case.render(0, u8w);
      }
      _ = try writer.write("\n");
    }
  }
};

/// NdFailMarker, NdRedunMarker, NdDiagStartMarker
pub const MarkerNode = struct {
  token: Token,
  payload: ?*Node = null,

  pub inline fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer, comptime str: []const u8) !void {
    _ = self;
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    _ = try writer.write(str ++ "\n");
  }
};

/// NdRet
pub const RetNode = struct {
  tkbit: TokenBit,
  expr: ?*Node,
  typ: ?*Type = null,

  pub inline fn init(expr: ?*Node, token: Token) @This() {
    return @This() {.expr = expr, .tkbit = TokenBit.init(token)};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    const expr = if (self.expr) |expr| expr.clone(al) else null;
    return Node.new(.{.NdRet = .{.expr = expr, .tkbit = self.tkbit}}, al);
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = try u8w.writer().write("return ");
    if (self.expr) |expr| {
      try expr.render(depth, u8w);
    }
    _ = try u8w.writer().write("\n");
  }
};

/// NdScope
pub const ScopeNode = struct {
  enter: bool,
  leave: bool,

  pub inline fn init(enter: bool, leave: bool) @This() {
    return @This() {.enter = enter, .leave = leave};
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = self;
    var writer = u8w.writer();
    try util.addDepth(&writer, depth);
    _ = try writer.write("<scope>\n");
  }
};

/// NdProgram
pub const ProgramNode = struct {
  decls: NodeItems,

  pub inline fn init(decls: NodeItems) @This() {
    return @This() {.decls = decls,};
  }

  pub fn clone(self: *@This(), al: Allocator) *Node {
    _ = self;
    _ = al;
    unreachable;
  }

  pub fn render(self: *@This(), depth: usize, u8w: *U8Writer) !void {
    _ = try u8w.writer().write("<program>.B\n");
    for (self.decls) |itm| {
      try itm.render(depth, u8w);
    }
    _ = try u8w.writer().write("<program>.E\n");
  }
};

pub const Node = union(NodeType)  {
  NdNumber: NumberNode,
  NdString: StringNode,
  NdBool: BoolNode,
  NdBinary: BinaryNode,
  NdAssign: BinaryNode,
  NdSubscript: SubscriptNode,
  NdUnary: UnaryNode,
  NdList: ListNode,
  NdTuple: ListNode,
  NdMap: MapNode,
  NdExprStmt: ExprStmtNode,
  NdParam: ParamNode,
  NdField: FieldNode,
  NdPubField: PubFieldNode,
  NdVarDecl: VarDeclNode,
  NdBlock: BlockNode,
  NdType: TypeNode,
  NdAlias: AliasNode,
  NdDeref: DerefNode,
  NdCondition: ConditionNode,
  NdMCondition: MatchConditionNode,
  NdEmpty: EmptyNode,
  NdCast: CastNode,
  NdSimpleIf: SimpleIfNode,
  NdWhile: WhileNode,
  NdControl: ControlNode,
  NdBasicCall: BasicCallNode,
  NdGenericCall: GenericCallNode,
  NdError: ErrorNode,
  NdOrElse: OrElseNode,
  NdBasicFun: BasicFunNode,
  NdGenericFun: GenericFunNode,
  NdDotAccess: DotAccessNode,
  NdClass: ClassNode,
  NdLblArg: LblArgNode,
  NdMatch: MatchNode,
  NdFailMarker: MarkerNode,
  NdRedunMarker: MarkerNode,
  NdDiagStartMarker: MarkerNode,
  NdRet: RetNode,
  NdScope: ScopeNode,
  NdTVar: TVarNode,
  NdProgram: ProgramNode,

  pub inline fn create(al: Allocator) *Node {
    return util.alloc(Node, al);
  }

  pub inline fn new(data: anytype, al: Allocator) *Node {
    const n = util.alloc(Node, al);
    n.* = data;
    return n;
  }

  pub inline fn isDeref(self: *const @This()) bool {
    return switch (self.*) {
      .NdDeref => true,
      else => false,
    };
  }
  
  pub inline fn isOrElse(self: *const @This()) bool {
    return switch (self.*) {
      .NdOrElse => true,
      else => false,
    };
  }

  pub inline fn isVarDecl(self: *const @This()) bool {
    return switch (self.*) {
      .NdVarDecl => true,
      else => false,
    };
  }

  pub inline fn isTVariable(self: *const @This()) bool {
    return switch (self.*) {
      .NdTVar => true,
      else => false,
    };
  }

  pub inline fn isNoneLiteral(self: *const @This()) bool {
    return switch (self.*) {
      .NdType => |*nt| nt.typ.isNoneTy(),
      else => false,
    };
  }

  pub inline fn isConstLiteral(self: *const @This()) bool {
    return switch (self.*) {
      .NdBool, .NdString, .NdNumber => true,
      else => false,
    };
  }

  pub inline fn isBasicFun(self: *const @This()) bool {
    return switch (self.*) {
      .NdBasicFun => true,
      else => false,
    };
  }

  pub inline fn isGenericFun(self: *const @This()) bool {
    return switch (self.*) {
      .NdGenericFun => true,
      else => false,
    };
  }

  pub inline fn isFun(self: *const @This()) bool {
    return switch (self.*) {
      .NdGenericFun, .NdBasicFun => true,
      else => false,
    };
  }

  pub inline fn isClass(self: *const @This()) bool {
    return switch (self.*) {
      .NdClass => true,
      else => false,
    };
  }

  pub inline fn isRet(self: *const @This()) bool {
    return switch (self.*) {
      .NdRet => true,
      else => false,
    };
  }

  pub inline fn isBinary(self: *const @This()) bool {
    return switch (self.*) {
      .NdBinary => true,
      else => false,
    };
  }

  pub inline fn isUnary(self: *const @This()) bool {
    return switch (self.*) {
      .NdUnary => true,
      else => false,
    };
  }

  pub inline fn isType(self: *const @This()) bool {
    return switch (self.*) {
      .NdType => true,
      else => false,
    };
  }

  pub inline fn isCondition(self: *const @This()) bool {
    return switch (self.*) {
      .NdCondition => true,
      else => false,
    };
  }

  pub inline fn isMCondition(self: *const @This()) bool {
    return switch (self.*) {
      .NdMCondition => true,
      else => false,
    };
  }

  pub inline fn isEmpty(self: *const @This()) bool {
    return switch (self.*) {
      .NdEmpty => true,
      else => false,
    };
  }

  pub inline fn isControl(self: *const @This()) bool {
    return switch (self.*) {
      .NdControl => true,
      else => false,
    };
  }

  pub inline fn isBlock(self: *const @This()) bool {
    return switch (self.*) {
      .NdBlock => true,
      else => false,
    };
  }

  pub inline fn isScope(self: *const @This()) bool {
    return switch (self.*) {
      .NdScope => true,
      else => false,
    };
  }

  pub inline fn isAssign(self: *const @This()) bool {
    return switch (self.*) {
      .NdAssign => true,
      else => false,
    };
  }

  pub inline fn isExitScope(self: *const @This()) bool {
    return switch (self.*) {
      .NdScope => |sc| sc.leave,
      else => false,
    };
  }

  pub inline fn isSimpleIf(self: *const @This()) bool {
    return switch (self.*) {
      .NdSimpleIf => true,
      else => false,
    };
  }

  pub inline fn isDotAccess(self: *const @This()) bool {
    return switch (self.*) {
      .NdDotAccess => true,
      else => false,
    };
  }

  pub inline fn isSubscript(self: *const @This()) bool {
    return switch (self.*) {
      .NdSubscript => true,
      else => false,
    };
  }

  pub inline fn isField(self: *const @This()) bool {
    return switch (self.*) {
      .NdField => true,
      else => false,
    };
  }

  pub inline fn isPubField(self: *const @This()) bool {
    return switch (self.*) {
      .NdPubField => true,
      else => false,
    };
  }

  pub inline fn isBasicCall(self: *const @This()) bool {
    return switch (self.*) {
      .NdBasicCall => true,
      else => false,
    };
  }

  pub inline fn isGenericCall(self: *const @This()) bool {
    return switch (self.*) {
      .NdGenericCall => true,
      else => false,
    };
  }

  pub inline fn isCall(self: *const @This()) bool {
    return switch (self.*) {
      .NdBasicCall, .NdGenericCall => true,
      else => false,
    };
  }

  pub inline fn isNumberLiteral(self: *const @This()) bool {
    return switch (self.*) {
      .NdNumber => true,
      else => false,
    };
  }

  pub inline fn isStringLiteral(self: *const @This()) bool {
    return switch (self.*) {
      .NdString => true,
      else => false,
    };
  }

  pub inline fn isExprStmt(self: *const @This()) bool {
    return switch (self.*) {
      .NdExprStmt => true,
      else => false,
    };
  }

  pub inline fn isLblArg(self: *const @This()) bool {
    return switch (self.*) {
      .NdLblArg => true,
      else => false,
    };
  }

  pub inline fn isMatch(self: *const @This()) bool {
    return switch (self.*) {
      .NdMatch => true,
      else => false,
    };
  }

  pub inline fn isLiftMarker(self: *const @This()) bool {
    return switch (self.*) {
      .NdLiftMarker => true,
      else => false,
    };
  }

  pub inline fn isFailMarker(self: *const @This()) bool {
    return switch (self.*) {
      .NdFailMarker => true,
      else => false,
    };
  }

  pub inline fn isRedunMarker(self: *const @This()) bool {
    return switch (self.*) {
      .NdRedunMarker => true,
      else => false,
    };
  }

  pub inline fn isDiagStartMarker(self: *const @This()) bool {
    return switch (self.*) {
      .NdDiagStartMarker => true,
      else => false,
    };
  }

  pub inline fn isMarker(self: *const @This()) bool {
    return switch (self.*) {
      .NdRedunMarker, .NdFailMarker,
      .NdDiagStartMarker => true,
      else => false,
    };
  }

  pub inline fn isProgram(self: *const @This()) bool {
    return switch (self.*) {
      .NdProgram => true,
      else => false,
    };
  }

  pub fn toMatchCondition(nd: *Node, al: Allocator) *Node {
    return Node.new(.{.NdMCondition = MatchConditionNode.init(nd)}, al);
  }

  pub inline fn isComptimeConst(self: *const @This()) bool {
    return switch (self.*) {
      .NdNumber, .NdString, .NdBool => true,
      else => false,
    };
  }

  pub fn getNarrowed(self: *@This()) ?*TVarNode {
    const narrowed = switch (self.*) {
      .NdSubscript => |*sub| sub.narrowed,
      .NdDeref => |*der| der.narrowed,
      .NdDotAccess => |*da| da.narrowed,
      else => null,
    };
    return if (narrowed) |nrw| (if (nrw.token != Token.getDefaultToken()) nrw else null) else null;
  }

  pub inline fn eql(self: *@This(), other: *@This()) bool {
    return self == other;
  }

  pub inline fn block(self: *@This()) *BlockNode {
    return &self.NdBlock;
  }

  pub fn box(self: @This(), al: Allocator) *@This() {
    return util.box(Node, self, al);
  }

  pub inline fn toIntNumber(self: *@This(), comptime T: type) T {
    return @intFromFloat(self.NdNumber.value);
  }

  pub fn getBasicFun(self: *@This()) *BasicFunNode {
    if (self.isBasicFun()) return &self.NdBasicFun;
    return &self.NdGenericFun.fun.NdBasicFun;
  }

  pub fn getBasicCall(self: *@This()) *BasicCallNode {
    if (self.isBasicCall()) return &self.NdBasicCall;
    return getBasicCall(self.NdGenericCall.call);
  }

  pub fn toCallNode(self: *@This()) CallNode {
    const node = self.getBasicCall();
    const targs = if (self.isGenericCall()) self.NdGenericCall.targs else null;
    return CallNode.init(node.expr, node.args(), targs, node._va_start, node.isVariadic(), node.isLabeled(), node.typ);
  }

  pub fn toTypeNode(self: *@This(), nd: *Node, al: Allocator) *Node {
    var tyn: TypeNode = undefined;
    switch (self.*) {
      .NdNumber => |*num| {
        tyn = TypeNode.init(Type.newConstant(.TyNumber, num.lexeme(al)).box(al), num.token.toToken());
      },
      .NdString => |*str| {
        tyn = TypeNode.init(Type.newConstant(.TyString, str.token.lexeme()).box(al), str.token);
      },
      .NdBool => |*bol| {
        tyn = TypeNode.init(Type.newConstant(.TyBool, bol.token.lexeme()).box(al), bol.token);
      },
      .NdType => return nd,
      .NdTVar => |*vr| {
        std.debug.assert(vr.token.valueEql(ks.NoneVar));
        tyn = TypeNode.init(Type.newNoneTag(al), vr.token);
        vr.typ = tyn.typ;
      },
      else => unreachable,
    }
    return Node.new(.{.NdType = tyn}, al);
  }

  pub fn clone(node: *Node, al: Allocator) *Node {
    return switch (node.*) {
      .NdNumber, .NdString, .NdBool, .NdAlias,
      .NdEmpty, .NdControl, .NdFailMarker, .NdGenericFun,
      .NdRedunMarker, .NdScope, .NdDiagStartMarker => node,
      .NdBinary, .NdAssign => |*nd| nd.clone(node, al),
      .NdList, .NdTuple => |*nd| nd.clone(node, al),
      inline else => |*nd| nd.clone(al),
    };
  }

  pub fn render(node: *Node, depth: usize, u8w: *U8Writer) !void {
    return switch (node.*) {
      .NdFailMarker => |*nd| nd.render(depth, u8w, "Fail"),
      .NdRedunMarker => |*nd| nd.render(depth, u8w, "Redundant"),
      .NdDiagStartMarker => |*nd| nd.render(depth, u8w, "MDiagStart"),
      inline else => |*nd| nd.render(depth, u8w),
    };
  }

  pub fn getToken(self: *const @This()) Token {
    return switch (self.*) {
      .NdNumber => |*nd| nd.token.toToken(),
      .NdString => |*nd| nd.token,
      .NdBool => |*nd| nd.token,
      .NdBinary, .NdAssign => |*nd| Token.fromBinaryNode(nd),
      .NdUnary => |*nd| nd.op.token(),
      .NdExprStmt => |*nd| nd.expr.getToken(),
      .NdVarDecl => |*nd| nd.name.toToken(),
      .NdType => |*nd| nd.tkbit.toToken(),
      .NdAlias => |*nd| nd.alias.tkbit.toToken(),
      .NdCast => |*nd| nd.typn.tkbit.toToken(),
      .NdSubscript => |*nd| nd.index.getToken(),
      .NdDeref => |*nd| nd.tkbit.toToken(),
      .NdCondition => |*nd| nd.cond.getToken(),
      .NdMCondition => |*nd| nd.tst.getToken(),
      .NdControl => |ct| ct.token,
      .NdWhile => |*nd| nd.cond.getToken(),
      .NdRet => |*nd| nd.tkbit.toToken(),
      .NdBasicCall => |*nd| nd.expr.getToken(),
      .NdGenericCall => |*nd| nd.call.getToken(),
      .NdError => |*nd| nd.expr.getToken(),
      .NdOrElse => |*nd| nd.ok.getToken(),
      .NdEmpty => |nd| nd.token,
      .NdDotAccess => |*nd| nd.lhs.getToken(),
      .NdLblArg => |*nd| nd.ident.getToken(),
      .NdMatch => |*nd| nd.expr.getToken(),
      .NdFailMarker, .NdRedunMarker, => |*nd| nd.token,
      .NdParam => |*nd| nd.name,
      .NdField => |*nd| nd.name.toToken(),
      .NdPubField => |*nd| nd.name.toToken(),
      .NdTVar => |*nd| nd.token,
      .NdClass => |*nd| nd.name.toToken(),
      .NdBasicFun => |*nd| {
        if (nd.data.name) |name| {
          return name;
        }
        if (nd.params.len > 0) {
          return nd.params[0].name;
        }
        if (nd.data.body.block().nodes.len > 0) {
          return nd.data.body.block().nodes[0].getToken();
        }
        util.logger.debug("Could not obtain token from node: {}. Using default", .{self});
        return Token.getDefaultToken();
      },
      .NdGenericFun => |*nd| nd.fun.getToken(),
      else => {
        switch (self.*) {
          .NdList, .NdTuple => |*nd| {
            if (nd.elems.len > 0) {
              return nd.elems[0].getToken();
            }
          },
          .NdMap => |*nd| {
            if (nd.pairs.len > 0) {
              return nd.pairs[0].key.getToken();
            }
          },
          .NdBlock => |*nd| {
            if (nd.nodes.len > 0) {
              return nd.nodes[0].getToken();
            }
          },
          .NdProgram => |*nd| {
            if (nd.decls.len > 0) {
              return nd.decls[0].getToken();
            }
          },
          else => {}
        }
        util.logger.debug("Could not obtain token from node: {}. Using default", .{self});
        return Token.getDefaultToken();
      },
    };
  }

  pub fn getFieldType(self: *@This()) ?*Type {
    return switch (self.*) {
      .NdField => |*nd| nd.typ,
      .NdPubField => |*nd| nd.typ,
      else => unreachable,
    };
  }

  pub fn getFieldLexeme(self: *@This()) []const u8 {
    return switch (self.*) {
      .NdField => |*nd| nd.name.lexeme(),
      .NdPubField => |*nd| nd.name.lexeme(),
      else => unreachable,
    };
  }

  pub fn getFieldValue(self: *@This()) ?*Node {
    return switch (self.*) {
      .NdField => |*nd| nd.value,
      .NdPubField => |*nd| nd.value,
      else => unreachable,
    };
  }

  pub fn setFieldValue(self: *@This(), value: *Node) void {
    return switch (self.*) {
      .NdField => |*nd| nd.value = value,
      .NdPubField => |*nd| nd.value = value,
      else => unreachable,
    };
  }

  pub fn fieldHasDefault(self: *@This()) bool {
    return switch (self.*) {
      .NdField => |*nd| nd.value != null,
      .NdPubField => |*nd| nd.value != null,
      else => unreachable,
    };
  }

  pub inline fn getDiagEndPayload(self: *@This()) *Node {
    return switch (self.*) {
      .NdDiagEndMarker => |*d| d.payload.?,
      else => @panic("Not a DiagEndMarker")
    };
  }

  pub fn getType(self: *@This()) ?*Type {
    return switch (self.*) {
      .NdExprStmt => &Type.void_ty,
      .NdCast => |*nd| nd.typn.typ,
      .NdBasicFun => |*fun| if (fun.data.ret) |ret| ret else null,
      .NdCondition => |*nd| nd.cond.getType(),
      .NdMCondition => |*nd| nd.tst.getType(),
      .NdGenericFun, .NdBlock, .NdSimpleIf,
      .NdWhile, .NdControl, .NdScope, .NdLblArg,
      .NdFailMarker, .NdRedunMarker,
      .NdEmpty, .NdDiagStartMarker,
      .NdProgram, .NdClass, .NdMatch => null,
      .NdBasicCall => |*nd| nd.typ,
      .NdGenericCall => |*nd| nd.call.getType(),
      inline else => |*nd| nd.typ,
    };
  }

  pub fn getTypeE(self: *@This()) ?*Type {
    return if (self.isExprStmt()) self.NdExprStmt.expr.getType() else self.getType();
  }

  pub fn setType(self: *@This(), typ: *Type) void {
    switch (self.*) {
      .NdUnary => |*una| una.typ = typ,
      .NdBinary, .NdAssign => |*nd| nd.typ = typ,
      .NdTVar => |*id| id.typ = typ,
      .NdCast => |*cst| cst.typn.typ = typ,
      .NdDotAccess => |*nd| nd.typ = typ,
      .NdSubscript => |*nd| nd.typ = typ,
      .NdDeref => |*nd| nd.typ = typ,
      .NdString => |*nd| nd.typ = typ,
      .NdBool => |*nd| nd.typ = typ,
      .NdScope => {},
      else => {
        util.logger.debug("Attempt to set type on node: {}", .{self});
      },
    }
  }

  pub fn forceSetType(self: *@This(), typ: *Type) void {
    switch (self.*) {
      .NdString => |*nd| nd.typ = typ,
      .NdNumber => |*nd| nd.typ = typ,
      .NdBool => |*nd| nd.typ = typ,
      .NdBinary, .NdAssign => |*nd| nd.typ = typ,
      .NdUnary => |*nd| nd.typ = typ,
      .NdDotAccess => |*nd| nd.typ = typ,
      .NdSubscript => |*nd| nd.typ = typ,
      .NdDeref => |*nd| nd.typ = typ,
      .NdTVar => |*nd| nd.typ = typ,
      .NdCast => |*cst| cst.typn.typ = typ,
      .NdOrElse => |*nd| nd.typ = typ,
      else => unreachable,
    }
  }

  pub fn hasSugar(self: *@This()) bool {
    return switch (self.*) {
      .NdNumber, .NdString, .NdBool, .NdParam,
      .NdField, .NdPubField, .NdType, .NdAlias,
      .NdCondition, .NdMCondition, .NdEmpty,
      .NdControl, .NdFailMarker,
      .NdRedunMarker, .NdScope, .NdTVar,
      .NdDiagStartMarker => false,
      .NdOrElse, .NdMatch, .NdDeref => true,
      .NdBinary => |*nd| nd.left.hasSugar() or nd.right.hasSugar(),
      .NdAssign => |*nd| nd.right.hasSugar() or nd.left.hasSugar(),
      .NdSubscript => |*nd| nd.expr.hasSugar() or nd.index.hasSugar(),
      .NdUnary => |*nd| nd.expr.hasSugar(),
      .NdExprStmt => |*nd| nd.has_sugar,
      .NdVarDecl => |*nd| nd.value.hasSugar(),
      .NdCast => |*nd| nd.expr.hasSugar(),
      .NdSimpleIf => |*nd| nd.cond.hasSugar() or nd.then.hasSugar() or nd.els.hasSugar(),
      .NdWhile => |*nd| nd.cond.hasSugar() or nd.then.hasSugar(),
      .NdError => |*nd| nd.expr.hasSugar(),
      .NdDotAccess => |*nd| nd.lhs.hasSugar() or nd.rhs.hasSugar(),
      .NdRet => |*nd| if (nd.expr) |expr| expr.hasSugar() else false,
      .NdLblArg => |*nd| nd.value.hasSugar(),
      .NdBasicFun => |*nd| nd.data.body.hasSugar(),
      .NdGenericFun => |*nd| nd.fun.hasSugar(),
      .NdGenericCall => |*nd| nd.call.hasSugar(),
      .NdBasicCall => |*nd| nd.expr.hasSugar() or blk: {
        for (nd.args()) |arg| {
          if (arg.hasSugar()) {
            return true;
          }
        }
        break :blk false;
      },
      .NdList, .NdTuple  => |*nd| {
        for (nd.elems) |itm| {
          if (itm.hasSugar()) {
            return true;
          }
        }
        return false;
      },
      .NdMap => |*nd| {
        for (nd.pairs) |pair| {
          if (pair.value.hasSugar() or pair.key.hasSugar()) {
            return true;
          }
        }
        return false;
      },
      .NdBlock => |*nd| {
        for (nd.nodes) |itm| {
          if (itm.hasSugar()) {
            return true;
          }
        }
        return false;
      },
      .NdClass => |*nd| {
        for (nd.data.methods) |itm| {
          if (itm.hasSugar()) {
            return true;
          }
        }
        return false;
      },
      .NdProgram => |*nd| {
        for (nd.decls) |itm| {
          if (itm.hasSugar()) {
            return true;
          }
        }
        return false;
      },
    };
  }
};


const OpType = lex.OpType;
const U8Writer = util.U8Writer;
const TypeHashMap = ds.ArrayHashMapUnmanaged(*Type, *Type);

const ID_SEED = 0x12;
pub const MAX_STEPS = MAX_RECURSIVE_DEPTH / 2;
pub const MAX_TPARAMS = 0xA;
pub const MAX_RECURSIVE_DEPTH = 0x3e8;

pub const TypeKind = enum (u4) {
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

  pub const TyClass: u4 = 7;
};

pub const AliasInfo = struct {
  lhs: *Type, // alias 
  rhs: *Type, // aliasee

  pub inline fn init(lhs: *Type, rhs: *Type) @This() {
    return @This() {.lhs = lhs, .rhs = rhs};
  }
};

pub const AccessSpecifier = enum(u4) {
  SpecPublic,
  SpecPrivate,

  pub fn getASpec(is_public: bool) @This() {
    return if (is_public) .SpecPublic else .SpecPrivate;
  }

  pub fn isPublic(self: @This()) bool {
    return self == .SpecPublic;
  }

  pub fn isPrivate(self: @This()) bool {
    return self == .SpecPrivate;
  }
};

pub const TypeInfo = union(enum) {
  Concrete: Concrete,
  Constant: Constant,
  Variable: Variable,
  Union: Union,
  TaggedUnion: TaggedUnion,
  Tag: Tag,
  Generic: Generic,
  Function: Function,
  Class: Class,
  Instance: Instance,
  Recursive: Recursive,
  TagOrClass: TagOrClass,
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
  info: TypeInfo,
  /// how this type can be accessed
  aspec: AccessSpecifier = .SpecPublic,
  /// only applies to function types; whether this function type is variadic
  variadic: bool = false,
  /// whether this type was inferred automatically i.e not an annotation
  inferred: bool = true,

  const Self = @This();

  var void_ty = Type.newVoid();

  pub inline fn init(data: TypeInfo) Self {
    return Self {.info = data};
  }

  /// combine types in typeset as much as possible
  pub fn compressTypes(typeset: *TypeHashSet, uni: ?*Type, allocator: Allocator) *Type {
    if (typeset.count() > 1) {
      var final = TypeList.init();
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
          if (ty.isRelatedTo(typ, .RCAny, allocator)) {
            continue;
          }
        }
        last_ty = typ;
        final.append(typ, allocator);
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
          final.append(Type.newConcrete(.TyBool).box(allocator), allocator);
        } else {
          final.append(tru, allocator);
        }
      } else if (false_ty) |fal| {
        final.append(fal, allocator);
      }
      if (final.len() > 1) {
        typ = Type.newUnion().box(allocator);
        typ.union_().addAll(&final, allocator);
      } else {
        typ = final.itemAt(0);
      }
      if (nil_ty) |nil| {
        if (typ.isUnion()) {
          typ.union_().set(nil, allocator);
        } else {
          var tmp = Type.newUnion().box(allocator);
          tmp.union_().addSlice(&[_]*Type{typ, nil}, allocator);
          typ = tmp;
        }
      }
      return typ;
    }
    return typeset.values()[0];
  }

  /// combine types in typelist as much as possible
  pub fn compressTaggedTypes(typelist: *TypeList, uni: ?*Type, al: Allocator) *Type {
    if (typelist.len() > 1) {
      var final = TypeList.init();
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
        final.append(typ, al);
      }
      // convert types to a single tagged union type
      var typ: *Type = undefined;
      if (final.len() == typelist.len()) {
        if (uni) |ty| {
          return ty;
        }
      }
      if (final.len() > 1) {
        typ = Type.newTaggedUnion().box(al);
        typ.taggedUnion().addAll(&final, al);
      } else {
        typ = final.itemAt(0);
      }
      if (none_ty) |none| {
        if (typ.isTaggedUnion()) {
          typ.taggedUnion().append(none, al);
        } else {
          var tmp = Type.newTaggedUnion().box(al);
          tmp.taggedUnion().addSlice(&[_]*Type{typ, none}, al);
          typ = tmp;
        }
      }
      return typ;
    }
    return typelist.itemAt(0);
  }

  fn setRestFields(ty1: *Self, ty2: *Self) void {
    ty1.tid = ty2.tid;
    ty1.alias = ty2.alias;
    ty1.variadic = ty2.variadic;
    ty1.inferred = ty2.inferred;
    ty1.aspec = ty2.aspec;
  }

  fn _clone(self: *Self, map: *TypeHashMap, al: Allocator) *Self {
    switch (self.info) {
      .Constant, .Concrete, .Variable, .Recursive, .TagOrClass => return self,
      .Function => |*fun| {
        if (map.get(self)) |ty| return ty;
        var new = Type.init(.{.Function = Function.init(fun.data.ret._clone(map, al), null, fun.data.node, al)}).box(al);
        map.set(self, new, al);
        var _fun = new.function();
        if (fun.tparams) |tparams| {
          var new_tparams = util.allocSlice(*Type, tparams.len, al);
          for (tparams, 0..) |ty, i| {
            new_tparams[i] = ty._clone(map, al);
          }
          _fun.tparams = new_tparams;
        }
        var params = util.allocSlice(*Type, fun.data.params.len, al);
        for (fun.data.params, 0..) |ty, i| {
          params[i] = ty._clone(map, al);
        }
        _fun.data.params = params;
        new.setRestFields(self);
        return new;
      },
      .Class => |*cls| {
        if (map.get(self)) |ty| return ty;
        var fields =  ds.ArrayListUnmanaged(*Node).initCapacity(cls.data.fields.capacity(), al);
        var methods = TypeList.initCapacity(cls.data.methods.capacity(), al);
        var ret = Type.init(.{.Class = Class.init(cls.data.name, cls.tktype, fields, methods, null, cls.data.node, cls.empty, cls.builtin, al)}).box(al);
        map.set(self, ret, al);
        for (cls.data.fields.items()) |itm| {
          fields.appendAssumeCapacity(itm.clone(al));
        }
        for (cls.data.methods.items()) |itm| {
          methods.appendAssumeCapacity(itm._clone(map, al));
        }
        if (cls.tparams) |tp| {
          var new_tparams = TypeList.initCapacity(tp.len, al);
          for (tp) |ty| {
            new_tparams.appendAssumeCapacity(ty._clone(map, al));
          }
          ret.klass().tparams = new_tparams.items();
        }
        ret.klass().immutable = cls.immutable;
        ret.setRestFields(self);
        return ret;
      },
      .Generic => |*gen| {
        var new = Generic.init(gen.base._clone(map, al));
        new.tparams.ensureTotalCapacity(gen.tparams.capacity(), al);
        for (gen.tparams.items()) |ty| {
          new.tparams.appendAssumeCapacity(ty._clone(map, al));
        }
        var ret = Type.init(.{.Generic = new}).box(al);
        ret.setRestFields(self);
        return ret;
      },
      .Union => |*uni| {
        var new = Union.init();
        new.variants.ensureTotalCapacity(uni.variants.capacity(), al);
        for (uni.variants.values()) |ty| {
          new.set(ty._clone(map, al), al);
        }
        var ret = Type.init(.{.Union = new}).box(al);
        ret.setRestFields(self);
        return ret;
      },
      .Tag => |*tg| {
        if (map.get(self)) |ty| return ty;
        var ret = Type.init(.{.Tag = Tag.init(tg.name, tg.ty)}).box(al);
        map.set(self, ret, al);
        var params: ?*Tag.TagFieldList = null;
        if (tg.fields) |prms| {
          var new_params = Tag.TagFieldList.initCapacity(prms.capacity(), al).box(al);
          for (prms.items()) |prm| {
            new_params.appendAssumeCapacity(prm.clone(al, map));
          }
          params = new_params;
        }
        var t = ret.tag();
        t.fields = params;
        t.instantiated = tg.instantiated;
        t.alias_is_parameterized = tg.alias_is_parameterized;
        ret.setRestFields(self);
        return ret;
      },
      .TaggedUnion => |*uni| {
        var new = TaggedUnion.init();
        new.active = uni.active;
        new.variants.ensureTotalCapacity(uni.variants.capacity(), al);
        for (uni.variants.items()) |ty| {
          new.variants.appendAssumeCapacity(ty._clone(map, al));
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
    }
  }

  pub fn clone(self: *Self, al: Allocator) *Self {
    var map = TypeHashMap.init();
    return self._clone(&map, al);
  }

  pub fn box(self: Self, allocator: Allocator) *Self {
    const al = allocate(Self, allocator);
    al.* = self;
    return al;
  }

  pub inline fn newConcrete(kind: TypeKind) Self {
    return Self.init(.{.Concrete = Concrete.init(kind)});
  }

  pub fn newBoolUnion(al: Allocator) *Self {
    var uni = Union.init();
    const t1 = Type.newConstant(.TyBool, ks.TrueVar).box(al);
    const t2 = Type.newConstant(.TyBool, ks.FalseVar).box(al);
    uni.addSlice(&[_]*Type{t1, t2}, al);
    return Self.init(.{.Union = uni}).box(al);
  }

  pub inline fn newConstant(kind: TypeKind, val: []const u8) Self {
    return Self.init(.{.Constant = Constant.init(kind, val)});
  }

  pub fn newNullable(ty: *Self, al: Allocator, nil_ty: ?*Type) *Self {
    if (ty.isNullable()) {
      return ty;
    }
    var nil = if (nil_ty) |nil| nil else Type.newConcrete(.TyNil).box(al);
    if (ty.isUnion()) {
      ty.union_().set(nil, al);
      return compressTypes(&ty.union_().variants, ty, al);
    } else {
      var hs = TypeHashSet.init();
      hs.set(ty.typeid(), ty, al);
      hs.set(nil.typeid(), nil, al);
      return compressTypes(&hs, null, al);
    }
  }

  pub fn newTaggedNullable(ty: *Self, al: Allocator, none_ty: ?*Type) *Self {
    const none = if (none_ty) |none| none else newNoneTag(al);
    const just = newJustTag(al, ty);
    var nl = newTaggedUnion().box(al);
    nl.taggedUnion().addSlice(&[_]*Type{just, none}, al);
    return nl;
  }

  pub inline fn newVariable() Self {
    return Self.init(.{.Variable = Variable.init()});
  }

  pub inline fn newVariableWToken(token: Token, al: Allocator) Self {
    var tvar = Self.init(.{.Variable = Variable.init()});
    tvar.variable().appendTok(token, al);
    return tvar;
  }

  pub inline fn newVariableAToken(token: Token, al: Allocator) *Self {
    var tvar = Self.init(.{.Variable = Variable.init()}).box(al);
    tvar.variable().appendTok(token, al);
    return tvar;
  }

  pub inline fn newUnion() Self {
    return Self.init(.{.Union = Union.init()});
  }

  pub inline fn newTaggedUnion() Self {
    return Self.init(.{.TaggedUnion = TaggedUnion.init()});
  }

  pub inline fn newGeneric(base: *Self) Self {
    return Self.init(.{.Generic = Generic.init(base)});
  }

  pub inline fn newFunction(ret: *Self, al: Allocator) Self {
    return Self.init(.{.Function = Function.init(ret, null, null, al)});
  }

  pub inline fn newTop(child: *Type) Self {
    return Self.init(.{.Top = Top.init(child)});
  }

  pub inline fn newInstance(cls: *Type) Self {
    return Self.init(.{.Instance = Instance.init(cls)});
  }

  pub inline fn newClass(name: []const u8, tktype: TokenType, allocator: Allocator) Self {
    return Self.init(.{.Class = Class.initWithDefault(name, tktype, allocator)});
  }

  pub inline fn newTag(name: []const u8, ty: TokenType) Self {
    return Self.init(.{.Tag = Tag.init(name, ty)});
  }

  pub inline fn newTagOrClass(name: []const u8, tktype: TokenType, allocator: Allocator) *Self {
    return Self.init(.{.TagOrClass = TagOrClass.init(name, tktype)}).box(allocator);
  }

  pub inline fn newRecursive(base: *Self) Self {
    return Self.init(.{.Recursive = Recursive.init(base)});
  }

  pub inline fn deriveToken(val: []const u8) Token {
    return Token.getDefaultToken().tkFrom(val, .TkIdent);
  }

  pub fn newNever(allocator: Allocator) *Self {
    var ty = newVariable();
    ty.variable().append(deriveToken(ks.NeverVar), allocator);
    return ty.box(allocator);
  }

  pub inline fn newVoid() Self {
    return Self.init(.{.Concrete = Concrete.init(.TyVoid)});
  }

  pub fn newBuiltinGenericClass(name: []const u8, tktype: TokenType, al: Allocator) *Type {
    var base = Type.newClass(name, tktype, al).box(al);
    base.klass().initTParams();
    return base;
  }

  pub fn newJustTag(al: Allocator, _ty: ?*Type) *Self {
    var tg = Self.init(.{.Tag = Tag.init(ks.JustVar, .TkJust)}).box(al);
    tg.tag().initFields(al);
    if (_ty) |ty| tg.tag().appendFieldType(ty, al);
    return tg;
  }

  pub inline fn newNoneTag(al: Allocator) *Self {
    return Self.init(.{.Tag = Tag.init(ks.NoneVar, .TkNone)}).box(al);
  }

  pub inline fn newUnboxedNoneTag() Self {
    return Self.init(.{.Tag = Tag.init(ks.NoneVar, .TkNone)});
  }

  pub fn newTagWithParams(name: []const u8, ty: TokenType, params: []const Tag.TagField, al: Allocator) *Self {
    var typ =  Self.init(.{.Tag = Tag.init(name, ty)}).box(al);
    typ.tag().initFields(al);
    typ.tag().addFields(params, al);
    return typ;
  }

  pub fn newTagWithParamTypes(name: []const u8, ty: TokenType, params: []const *Type, al: Allocator) *Self {
    var typ =  Self.init(.{.Tag = Tag.init(name, ty)}).box(al);
    typ.tag().initFieldTypes(params, al);
    return typ;
  }

  pub fn subtype(self: *Self, al: Allocator) *Type {
    std.debug.assert(self.isTaggedNullable());
    var sub = TypeList.init();
    for (self.taggedUnion().variants.items()) |ty| {
      if (!ty.isNoneTy()) {
        sub.append(ty, al);
      }
    }
    return compressTaggedTypes(&sub, null, al);
  }

  /// simple/concrete 'unit' type
  pub inline fn isSimple(self: *Self) bool {
    return switch (self.info) {
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
    return switch (self.info) {
      .Generic => true,
      else => false,
    };
  }

  /// a type that may require some form of substitution
  pub inline fn isClsParameterized(self: *Self) bool {
    return switch (self.info) {
      .Class => |*cls| cls.isParameterized(),
      else => false,
    };
  }

  /// a type that may require some form of substitution
  pub inline fn isTagParameterized(self: *Self) bool {
    return switch (self.info) {
      .Tag => |*tg| tg.isParameterized(),
      else => false,
    };
  }

  /// a compile-time constant type
  pub inline fn isConstant(self: *Self) bool {
    return switch (self.info) {
      .Constant => true,
      else => false,
    };
  }

  /// a nullable type
  pub inline fn isNullable(self: *Self) bool {
    return switch (self.info) {
      .Union => |*uni| uni.isNullable(),
      else => false,
    };
  }

  /// a tagged nullable type
  pub inline fn isTaggedNullable(self: *Self) bool {
    return switch (self.info) {
      .TaggedUnion => |*uni| uni.isNullable(),
      else => false,
    };
  }

  /// an error union type
  pub inline fn isErrorTaggedUnion(self: *Self) bool {
    return switch (self.info) {
      .TaggedUnion => |*uni| uni.isErrorUnion(),
      else => false,
    };
  }

  /// a union type
  pub inline fn isUnion(self: *Self) bool {
    return switch (self.info) {
      .Union => true,
      else => false,
    };
  }

  /// a tagged union type
  pub inline fn isTaggedUnion(self: *Self) bool {
    return switch (self.info) {
      .TaggedUnion => true,
      else => false,
    };
  }

  /// a name/variable type
  pub inline fn isVariable(self: *Self) bool {
    return switch (self.info) {
      .Variable => true,
      else => false,
    };
  }

  /// a function type
  pub inline fn isFunction(self: *Self) bool {
    return switch (self.info) {
      .Function => true,
      else => false,
    };
  }

  /// an instance type
  pub inline fn isInstance(self: *Self) bool {
    return switch (self.info) {
      .Instance => true,
      else => false,
    };
  }

  /// a class type
  pub inline fn isClass(self: *Self) bool {
    return switch (self.info) {
      .Class => true,
      else => false,
    };
  }

  /// a recursive type
  pub inline fn isRecursive(self: *Self) bool {
    return switch (self.info) {
      .Recursive => true,
      else => false,
    };
  }

  /// a tag type
  pub inline fn isTag(self: *Self) bool {
    return switch (self.info) {
      .Tag => true,
      else => false,
    };
  }

  /// a TagOrClass type
  pub inline fn isTagOrClass(self: *Self) bool {
    return switch (self.info) {
      .TagOrClass => true,
      else => false,
    };
  }

  /// a top type
  pub inline fn isTop(self: *Self) bool {
    return switch (self.info) {
      .Top => true,
      else => false,
    };
  }

  /// a top type with a 'child' class type
  pub inline fn isClassFromTop(self: *Self) bool {
    return switch (self.info) {
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

  /// a type that is parameterized
  pub inline fn isParameterized(self: *Self) bool {
    return (
      self.isLikeXTy(isGeneric) or
      self.isLikeXTy(isClsParameterized) or
      self.isLikeXTy(isTagParameterized) or 
      self.isGeneric() or
      self.isClsParameterized() or
      self.isTagParameterized()
    );
  }

  /// a type that may be parameterized/generic (from annotation usage)
  pub inline fn isLikeParameterized(self: *Self) bool {
    return self.isLikeXTy(isGeneric) or self.isLikeXTy(isClsParameterized) or self.isLikeXTy(isTagParameterized);
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
    return switch (self.info) {
      .Concrete => |conc| conc.kind == kind,
      else => false,
    };
  }

  inline fn isXClassTy(self: *Self, tktype: TokenType) bool {
    if (self.isClass()) {
      return self.klass().tktype == tktype;
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
      self.variable().len() == 1 and
      self.variable().tokens.getLast().valueEql(ks.NeverVar)
    );
  }

  pub fn isListTy(self: *Self) bool {
    return self.isXClassTy(.TkList);
  }

  pub fn isMapTy(self: *Self) bool {
    return self.isXClassTy(.TkMap);
  }

  pub fn isTupleTy(self: *Self) bool {
    return self.isXClassTy(.TkTuple);
  }

  pub fn isJustTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkJust;
  }

  pub fn isNoneTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkNone;
  }
  /// Tag ok type
  pub fn isOkTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkOk;
  }

  /// Tag error type
  pub fn isErrorTy(self: *Self) bool {
    return self.isTag() and self.tag().ty == .TkError;
  }

   /// extract the appropriate typeinfo of this type
  pub inline fn nullable(self: *Self) *Union {
    return &self.info.Union;
  }

  pub inline fn concrete(self: *Self) Concrete {
    return self.info.Concrete;
  }

  pub inline fn constant(self: *Self) *Constant {
    return &self.info.Constant;
  }

  pub inline fn generic(self: *Self) *Generic {
    return &self.info.Generic;
  }

  pub inline fn variable(self: *Self) *Variable {
    return &self.info.Variable;
  }

  pub inline fn union_(self: *Self) *Union {
    return &self.info.Union;
  }

  pub inline fn taggedUnion(self: *Self) *TaggedUnion {
    return &self.info.TaggedUnion;
  }

  pub inline fn function(self: *Self) *Function {
    return &self.info.Function;
  }

  pub inline fn instance(self: *Self) *Instance {
    return &self.info.Instance;
  }

  pub inline fn tag(self: *Self) *Tag {
    return &self.info.Tag;
  }

  pub inline fn toc(self: *Self) *TagOrClass {
    return &self.info.TagOrClass;
  }

  pub inline fn top(self: *Self) *Top {
    return &self.info.Top;
  }

  pub inline fn classfromtop(self: *Self) *Class {
    return self.info.Top.child.klass();
  }

  pub inline fn klass(self: *Self) *Class {
    return &self.info.Class;
  }

  pub inline fn recursive(self: *Self) Recursive {
    return self.info.Recursive;
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
    return switch (self.info) {
      .Concrete, .Constant, .Instance => false,
      .Variable => true,
      .Class => |*cls| {
        if (cls.tparams) |tparams| {
          for (tparams) |ty| {
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
        for (tg.fieldSlice()) |prm| {
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
          for (tparams) |ty| {
            if (ty.hasVariable()) {
              return true;
            }
          }
        }
        for (fun.data.params) |ty| {
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

  /// check if a type contains a variable type (param)
  pub fn hasThisVariable(self: *Self, typ: *Type) bool {
    return switch (self.info) {
      .Concrete, .Constant, .Instance => false,
      .Variable => |*vr| vr.eql(typ.variable()),
      .Class => |*cls| {
        if (cls.tparams) |tparams| {
          for (tparams) |ty| {
            if (ty.hasThisVariable(typ)) {
              return true;
            }
          }
        }
        for (cls.getSlice()) |ty| {
          if (ty.hasThisVariable(typ)) {
            return true;
          }
        }
        return false;
      },
      .Tag => |*tg| {
        for (tg.fieldSlice()) |prm| {
          if (prm.typ.hasThisVariable(typ)) {
            return true;
          }
        }
        return false;
      },
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
          if (ty.hasThisVariable(typ)) {
            return true;
          }
        }
        return false;
      },
      .Function => |*fun| {
        if (fun.tparams) |tparams| {
          for (tparams) |ty| {
            if (ty.hasThisVariable(typ)) {
              return true;
            }
          }
        }
        for (fun.data.params) |ty| {
          if (ty.hasThisVariable(typ)) {
            return true;
          }
        }
        return fun.data.ret.hasThisVariable(typ);
      },
      .Top => |*tp| tp.child.hasThisVariable(typ),
      .Generic => |*gen| {
        if (gen.base.hasThisVariable(typ)) {
          return true;
        }
        for (gen.getSlice()) |ty| {
          if (ty.hasThisVariable(typ)) {
            return true;
          }
        }
        return false;
      },
      .Recursive => false, 
      else => false,
    };
  }

  pub fn getName(self: *Self) []const u8 {
    return switch (self.info) {
      .Variable => |*name| name.tokens.getLast().lexeme(),
      else => "",
    };
  }

  pub fn typeid(self: *Self) u32 {
    if (self.tid != 0) return self.tid;
    switch (self.info) {
      .Concrete => |conc| {
        switch (conc.kind) {
          .TyBool     => self.tid = 1 << ID_SEED,
          .TyNumber   => self.tid = 2 << ID_SEED,
          .TyString   => self.tid = 3 << ID_SEED,
          .TyNil      => self.tid = 4 << ID_SEED,
          .TyVoid     => self.tid = 9 << ID_SEED,
          .TyNoReturn => self.tid = 10 << ID_SEED,
          .TyAny      => self.tid = 11 << ID_SEED,
        }
      },
      .Generic => |*gen| {
        self.tid = 13 << ID_SEED;
        self.tid += gen.base.typeid();
        for (gen.getSlice()) |typ| {
          self.tid += typ.typeid();
        }
      },
      .Class => |*cls| {
        self.tid = 5 << ID_SEED;
        for (cls.data.name) |ch| {
          self.tid += @as(u8, ch);
        }
        for (cls.getSlice()) |typ| {
          self.tid += typ.typeid();
        }
        for (cls.data.fields.items()) |nd| {
          if (nd.getFieldType()) |typ| {
            self.tid += typ.typeid();
          } else {
            const lxm = nd.getFieldLexeme();
            for (lxm) |ch| {
              self.tid += @as(u8, ch);
            }
          }
        }
      },
      .Union => |*uni| {
        self.tid = 6 << ID_SEED;
        for (uni.variants.values()) |ty| {
          self.tid += ty.typeid();
        }
      },
      .Constant => |*cons| {
        self.tid = 7 << ID_SEED;
        // TODO: more efficient approach
        for (cons.val) |ch| {
          self.tid += @as(u8, ch);
        }
      },
      .Function => |*fun| {
        self.tid = 8 << ID_SEED;
        if (fun.tparams) |tparams| {
          self.tid += @intCast(tparams.len);
        }
        for (fun.data.params) |ty| {
          self.tid += ty.typeid();
        }
        self.tid += fun.data.ret.typeid();
      },
      .Instance => |*inst| {
        self.tid = 14 << ID_SEED;
        self.tid += inst.cls.typeid();
      },
      .Top => |*tp| {
        self.tid = 12 << ID_SEED;
        self.tid += tp.child.typeid();
      },
      .Variable => |*vr| {
        self.tid = 17 << ID_SEED;
        for (vr.tokens.items()) |tok| {
          self.tid += @as(u32, @intFromEnum(TokenType.TkIdent));
          for (tok.lexeme()) |ch| {
            self.tid += @as(u8, ch);
          }
        }
      },
      .Recursive => |*rec| {
        self.tid = rec.base.typeid();
      },
      .Tag => |*tg| {
        self.tid = 15 << ID_SEED;
        for (tg.name) |ch| {
          self.tid += @as(u8, ch);
        }
        if (tg.fields) |params| {
          self.tid += @intCast(params.len());
          for (params.items()) |param| {
            self.tid += param.typ.typeid();
          }
        }
      },
      .TaggedUnion => |*uni| {
        self.tid = 16 << ID_SEED;
        for (uni.variants.items()) |ty| {
          self.tid += ty.typeid();
        }
      },
      .TagOrClass => |*tc| {
        self.tid = 18 << ID_SEED;
        for (tc.name) |ch| {
          self.tid += @as(u8, ch);
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

  fn _unfoldRecursive(typ: *Self, step: usize, list: *TypeList, al: Allocator, visited: *TypeHashSet) void {
    if (step > 0 and typ.isRecursive() and visited.get(typ.typeid()) != null) {
      return;
    }
    switch (typ.info) {
      // TODO: unfold function & method like generic
      .Concrete, .Variable, .Constant, .Top,
      .Function, .Instance, .TagOrClass => list.append(typ, al),
      .Generic => |*gen| {
        gen.base._unfoldRecursive(step + 1, list, al, visited);
        for (gen.tparams.items()) |param| {
          param._unfoldRecursive(step + 1, list, al, visited);
        }
      },
      .Class => |*cls| {
        // name
        list.append(Type.newVariableAToken(deriveToken(cls.data.name), al), al);
        // tparams
        for (cls.getSlice()) |param| {
          param._unfoldRecursive(step + 1, list, al, visited);
        }
        // fields
        for (cls.data.fields.items()) |field| {
          if (field.getFieldType()) |ty| {
            ty._unfoldRecursive(step + 1, list, al, visited);
          }
        }
        // methods
        for (cls.data.methods.items()) |mth| {
          mth._unfoldRecursive(step + 1, list, al, visited);
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          ty._unfoldRecursive(step + 1, list, al, visited);
        }
      },
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
          ty._unfoldRecursive(step + 1, list, al, visited);
        }
      },
      .Tag => |*tg| {
        list.append(Type.newVariableAToken(deriveToken(tg.name), al), al);
        // params
        for (tg.fieldSlice()) |param| {
          param.typ._unfoldRecursive(step + 1, list, al, visited);
        }
      },
      .Recursive => |*rec| {
        visited.set(typ.typeid(), typ, al);
        rec.base._unfoldRecursive(step + 1, list, al, visited);
      },
    }
  }

  fn _unfold(self: *Self, list: *TypeList, al: Allocator) void {
    switch (self.info) {
      // TODO: unfold function & method like generic
      .Concrete, .Constant, .Variable, .Top, .Function,
      .Instance, .TagOrClass => list.append(self, al),
      .Generic => |*gen| {
        gen.base._unfold(list, al);
        for (gen.tparams.items()) |param| {
          param._unfold(list, al);
        }
      },
      .Class => |*cls| {
        // name
        list.append(Type.newVariableAToken(deriveToken(cls.data.name), al), al);
        // tparams
        for (cls.getSlice()) |param| {
          param._unfold(list, al);
        }
        // fields
        for (cls.data.fields.items()) |field| {
          if (field.getFieldType()) |ty| {
            ty._unfold(list, al);
          }
        }
        // methods
        for (cls.data.methods.items()) |mth| {
          mth._unfold(list, al);
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          ty._unfold(list, al);
        }
      },
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
          ty._unfold(list, al);
        }
      },
      .Tag => |*tg| {
        list.append(Type.newVariableAToken(deriveToken(tg.name), al), al);
        // params
        for (tg.fieldSlice()) |param| {
          param.typ._unfold(list, al);
        }
      },
      .Recursive => |*rec| {
        var visited = TypeHashSet.init();
        visited.set(self.typeid(), self, al);
        rec.base._unfoldRecursive(0, list, al, &visited);
      },
    }
  }

  fn unfold(self: *Self, al: Allocator) TypeList {
    var list = TypeList.init();
    self._unfold(&list, al);
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

  pub fn isRelatedTo(this: *Self, other: *Self, ctx: RelationContext, al: Allocator) bool {
    // We use target & source as in assignment/cast target and assignment/cast source respectively. 
    // Context ctx is the context in which such relation is being performed.
    // Target.          Source    Context
    // this.     &.      other.     ctx
    if (this == other) return true;
    switch (this.info) {
      inline else => |*tyk| return tyk.isRelatedTo(other, ctx, al),
    }
    unreachable;
  }

  pub inline fn isEitherWayRelatedTo(this: *Self, other: *Self, ctx: RelationContext, al: Allocator) bool {
    return this.isRelatedTo(other, ctx, al) or other.isRelatedTo(this, ctx, al);
  }

  pub fn canBeCastTo(node_ty: *Type, cast_ty: *Type, ctx: RelationContext, al: Allocator) error{CastError, UnionCastError}!*Type {
    switch (cast_ty.info) {
      .Concrete => |conc| {
        switch (conc.kind) {
          // any type may be cast to bool
          .TyBool => return cast_ty,
          .TyNil => return error.CastError,
          else => {
            if (cast_ty.isRelatedTo(node_ty, ctx, al)) {
              return cast_ty;
            }
          },
        }
      },
      .Union => {
        // upcasting/widening
        if (cast_ty.isRelatedTo(node_ty, ctx, al)) {
          // keep track of the active type
          // uni.active = node_ty;
          return cast_ty;
        }
      },
      .TaggedUnion => |*uni| {
        // upcasting/widening
        if (uni._isRelatedTo(node_ty, ctx, al)) |active| {
          // keep track of the active type
          uni.active = @intCast(active);
          return cast_ty;
        }
      },
      else => {
        if (cast_ty.isRelatedTo(node_ty, ctx, al)) {
          return cast_ty;
        }
      }
    }
    // downcasting
    if (node_ty.castContainsType(cast_ty, ctx, al)) {
      if (node_ty.isTaggedUnion()) {
        if (node_ty.taggedUnion().activeTy()) |active| {
          // check if the active type can be cast to the cast type
          _ = active.canBeCastTo(cast_ty, ctx, al) catch return error.UnionCastError;
          return cast_ty;
        } else {
          return node_ty;
        }
      } else {
        return node_ty;
      }
    }
    return error.CastError;
  }
  
  pub fn canBeAssigned(target: *Self, source: *Self, ctx: RelationContext, al: Allocator) ?*Type {
    switch (target.info) {
      .Union => {
        if (target.isRelatedTo(source, ctx, al)) {
          return target;
        }
      },
      .TaggedUnion => |*uni| {
        if (uni._isRelatedTo(source, ctx, al)) |active| {
          uni.active = @intCast(active);
          return target;
        }
      },
      else => {
        if (target.isRelatedTo(source, ctx, al)) {
          return target;
        }
      },
    }
    return null;
  }

  /// check if `cast_ty` is contained in `node_ty`. This is false if `node_ty` is not a union.
  pub fn castContainsType(node_ty: *Self, cast_ty: *Self, ctx: RelationContext, al: Allocator) bool {
    switch (node_ty.info) {
      .Union => |*uni_a| {
        switch (cast_ty.info) {
          .Union => |*uni_b| {
            for (uni_b.variants.values()) |ty| {
              if (!node_ty.castContainsType(ty, ctx, al)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.values()) |ty| {
              if (ty.typeidEql(cast_ty)) {
                return true;
              } else if (cast_ty.canBeCastTo(ty, ctx, al) catch null) |_| {
                return true;
              }
            }
          }
        }
      },
      .TaggedUnion => |*uni_a| {
        switch (cast_ty.info) {
          .TaggedUnion => |*uni_b| {
            for (uni_b.variants.items()) |ty| {
              if (!node_ty.castContainsType(ty, ctx, al)) {
                return false;
              }
            }
            return true;
          },
          else => {
            for (uni_a.variants.items()) |ty| {
              if (ty.typeidEql(cast_ty)) {
                return true;
              } else if (cast_ty.canBeCastTo(ty, ctx, al) catch null) |_| {
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

  pub fn toInstance(self: *Self, al: Allocator) *Self {
    switch (self.info) {
      .Class => {
        return Self.newInstance(self).box(al);
      },
      else => unreachable,
    }
  }

  pub fn toTaggedUnion(self: *Self, al: Allocator) *Self {
    var tu = TaggedUnion.init();
    tu.addSlice(self.union_().variants.values(), al);
    return tu.toType(al).box(al);
  }

  fn writeName(tokens: *Variable.TokenList, u8w: *U8Writer) !void {
    var writer = u8w.writer();
    for (tokens.items(), 0..) |tok, i| {
      // variables starting with $ are generated and internal, so use this symbol instead
      const value = tok.lexeme();
      _ = if (value.len > 0 and value[0] != ks.GeneratedVarMarker)
            try writer.write(value)
          else try writer.write(ks.GeneratedTypeVar);
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
    switch (self.info) {
      .Concrete => |conc| switch (conc.kind) {
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
        const name = cls.data.name;
        if (cls.tparams == null) {
          _ = try u8w.writer().write(name);
        } else {
          var writer = u8w.writer();
          _ = try writer.write(name);
          _ = try writer.write("{");
          const tparams = cls.tparams.?;
          for (tparams, 0..) |param, i| {
            try param._typename(depth, u8w);
            if (i != tparams.len - 1) {
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
        const values = uni.variants.values();
        for (values, 0..) |typ, i| {
          if (!typ.isFunction()) {
            try typ._typename(depth, u8w);
          } else {
            _ = try writer.write("(");
            try typ._typename(depth, u8w);
            _ = try writer.write(")");
          }
          if (i != values.len - 1) {
            _ = try writer.write(" & ");
          }
        }
      },
      .Function => |*fun| {
        // fn (params): ret
        var writer = u8w.writer();
        _ = try writer.write("fn ");
        if (fun.tparams) |tparams| {
          _ = try writer.write("{");
          for (tparams, 0..) |ty, i| {
            try ty._typename(depth, u8w);
            if (i < tparams.len - 1) {
              _ = try writer.write(", ");
            }
          }
          _ = try writer.write("}");
        }
        _ = try writer.write("(");
        for (fun.data.params, 0..) |ty, i| {
          try ty._typename(depth, u8w);
          if (i < fun.data.params.len - 1) {
            _ = try writer.write(", ");
          }
        }
        _ = try writer.write(")");
        if (!fun.data.ret.isVariable() or fun.data.ret.variable().getFirst().ty != .TkEof) {
          _ = try writer.write(": ");
          try fun.data.ret._typename(depth, u8w);
        }
      },
      .Instance => |*inst| {
        var writer = u8w.writer();
        _ = try inst.cls._typename(depth, u8w);
        _ = try writer.write(" instance");
      },
      .Top => {
        _ = try u8w.writer().write("Type");
      },
      .TagOrClass => |*tc| {
        _ = try u8w.writer().write(tc.name);
      },
      .Tag => |*tg| {
        _ = try u8w.writer().write(tg.name);
        if (tg.fields) |params| {
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

  /// combine t1 and t2 into a union type if possible
  inline fn _unionify(t1: *Type, t2: *Type, al: Allocator) *Type {
    if (t1.typeid() == t2.typeid()) {
      return t1;
    }
    if (t1.isNilTy() and t2.isNilTy()) {
      return t1;
    }
    if (t1.isNilTy()) {
      if (t2.isNullable()) return t2;
      return t2.newNullable(al, t1);
    } else if (t2.isNilTy()) {
      if (t1.isNullable()) return t1;
      return t1.newNullable(al, t2);
    } else {
      if (t1.isRelatedTo(t2, .RCAny, al)) {
        return t1;
      }
      if (t2.isUnion()) {
        var variants = t2.union_().variants.copy(al);
        variants.set(t1.typeid(), t1, al);
        return compressTypes(&variants, null, al);
      }
      if (t2.isTaggedUnion()) {
        var variants = t2.taggedUnion().variants.copy(al);
        variants.append(t1, al);
        return compressTaggedTypes(&variants, null, al);
      }
      if (t2.isRelatedTo(t1, .RCAny, al)) {
        return t2;
      }
      if (t1.isUnion()) {
        var variants = t1.union_().variants.copy(al);
        variants.set(t2.typeid(), t2, al);
        return compressTypes(&variants, null, al);
      }
      if (t1.isTaggedUnion()) {
        var variants = t1.taggedUnion().variants.copy(al);
        variants.append(t2, al);
        return compressTaggedTypes(&variants, null, al);
      }
      var tmp = Type.newTaggedUnion().box(al);
      tmp.taggedUnion().addSlice(&[_]*Type{t1, t2}, al);
      return tmp;
    }
  }

  /// combine t1 and t2 into a union type if possible
  pub fn unionify(t1: *Type, t2: *Type, al: Allocator) *Type {
    // The idea is that, if we're unionifying a 'never' type and a non-never type
    // then this must be from a type negation. Essentially, it means the never type 
    // does not hold if we still have the possibility of a non-never type.
    if (t1.isNeverTy()) {
      return t2;
    }
    if (t2.isNeverTy()) {
      return t1;
    }
    return _unionify(t1, t2, al);
  }

  /// find the intersection of t1 and t2
  pub fn intersect(t1: *Type, t2: *Type, allocator: Allocator) ?*Type {
    if (t1.typeid() == t2.typeid()) {
      return t1;
    }
    if (t1.isRelatedTo(t2, .RCAny, allocator)) {
      switch (t1.info) {
        .Union => |*uni| {
          var uni2 = TypeHashSet.init();
          for (uni.variants.values()) |typ| {
            if (typ.intersect(t2, allocator)) |ty| {
              uni2.set(ty.typeid(), ty, allocator);
            }
          }
          return compressTypes(&uni2, null, allocator);
        },
        .TaggedUnion => |*uni| {
          var uni2 = TypeList.init();
          for (uni.variants.items()) |typ| {
            if (typ.intersect(t2, allocator)) |ty| {
              uni2.append(ty, allocator);
            }
          }
          return compressTaggedTypes(&uni2, null, allocator);
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
      switch (t2.info) {
        .Union => |*uni| {
          var uni2 = TypeHashSet.init();
          for (uni.variants.values()) |typ| {
            if (typ.intersect(t1, allocator)) |ty| {
              uni2.set(ty.typeid(), ty, allocator);
            }
          }
          return compressTypes(&uni2, null, allocator);
        },
        .TaggedUnion => |*uni| {
          var uni2 = TypeList.init();
          for (uni.variants.items()) |typ| {
            if (typ.intersect(t1, allocator)) |ty| {
              uni2.append(ty, allocator);
            }
          }
          return compressTaggedTypes(&uni2, null, allocator);
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
  pub fn negate(t1: *Type, t2: *Type, allocator: Allocator) error{Negation}!*Type {
    switch (t1.info) {
      .Union => |*uni| {
        var new_uni = TypeHashSet.init();
        for (uni.variants.values()) |ty| {
          if (!ty.isEitherWayRelatedTo(t2, .RCAny, allocator)) {
            new_uni.set(ty.typeid(), ty, allocator);
          }
        }
        if (new_uni.count() == 0) {
          return error.Negation;
        }
        return Self.compressTypes(&new_uni, null, allocator);
      },
      .TaggedUnion => |*uni| {
        var new_uni = TypeList.init();
        for (uni.variants.items()) |ty| {
          if (ty.is(t2, allocator) == null) {
            new_uni.append(ty, allocator);
          }
        }
        if (new_uni.isEmpty()) {
          return error.Negation;
        }
        return Self.compressTaggedTypes(&new_uni, null, allocator);
      },
      .Constant, .Concrete, .Generic, .Variable, .Top,
      .Function, .Class, .Instance, .Tag, .TagOrClass => return error.Negation,
      .Recursive => return t1,
    }
  }

  /// t1 is t2 -> select t2 from t1
  pub fn is(t1: *Type, t2: *Type, al: Allocator) ?*Type {
    // ex: is(str | list{num}, list) -> list{num}
    switch (t1.info) {
      .Concrete => |conc| {
        if (t2.isConcrete()) {
          if (conc.kind == t2.concrete().kind) {
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
          const t2_gen = t2.generic();
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
          if (std.mem.eql(u8, cls1.data.name, ty.klass().data.name)) {
            return t1;
          }
        } else if (t2.isTagOrClass()) {
          if (std.mem.eql(u8, cls1.data.name, t2.toc().name)) {
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
        var list = TypeList.init();
        for (uni.variants.items()) |ty| {
          if (is(ty, t2, al)) |typ| {
            list.append(typ, al);
          }
        }
        if (list.isNotEmpty()) {
          return compressTaggedTypes(&list, t1, al);
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
      .Instance => |*inst| {
        if (t2.isClass() or t2.isTagOrClass()) {
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
        } else if (t2.isTagOrClass()) {
          if (tg.nameEql(t2.toc().name)) {
            return t1;
          }
        }
      },
      .TagOrClass => {},
      .Top => |*tp| {
        return tp.child.is(t2, al);
      },
      .Recursive => return t1,
    }
    return null;
  }
};

pub const Concrete = struct {
  /// kind of this Concrete type
  kind: TypeKind,
  /// the token value of this Concrete type
  val: ?[]const u8 = null,

  pub inline fn init(kind: TypeKind) @This() {
    return @This() {.kind = kind};
  }

  pub inline fn toType(self: Concrete) Type {
    return Type.init(.{.Concrete = self});
  }

  pub fn isRelatedTo(this: *Concrete, other: *Type, ctx: RelationContext, al: Allocator) bool {
    _ = ctx;
    _ = al;
    if (this.kind == .TyAny) return true;
    switch (other.info) {
      // Concrete & Concrete 
      .Concrete => |*conc| {
        return conc.kind == this.kind;
      },
      .Constant => |*cons| return cons.kind == this.kind,
      .Union => |*uni| {
        if (this.kind == .TyBool or this.kind == .TyString or this.kind == .TyNumber) {
          for (uni.variants.values()) |ty| {
            if (!(ty.isConstant() and ty.constant().kind == this.kind)) {
              return false;
            }
          }
          return true;
        }
      },
      .Class => |*cls| {
        if (this.kind == .TyString) {
          return cls.isStringClass();
        }
        return false;
      },
      .Generic, .Variable, .Recursive, .Function,
      .Top, .Instance, .Tag, .TaggedUnion, .TagOrClass => return false,
    }
    return false;
  }
};

pub const Constant = struct {
  kind: TypeKind,
  val: []const u8,

  pub inline fn init(kind: TypeKind, val: []const u8) @This() {
    return @This() {.kind = kind, .val = val};
  }

  pub inline fn isTrue(self: *Constant) bool {
    return std.mem.eql(u8, self.val, ks.TrueVar);
  }

  pub inline fn toType(self: Constant) Type {
    return Type.init(.{.Constant = self});
  }

  pub fn isRelatedTo(this: *Constant, other: *Type, ctx: RelationContext, al: Allocator) bool {
    _ = al;
    _ = ctx;
    switch (other.info) {
      // Constant & Concrete 
      .Constant => |*cons| {
        return this.kind == cons.kind and std.mem.eql(u8, this.val, cons.val);
      },
      .Concrete => |*conc| {
        if (conc.kind == this.kind) {
          if (conc.val) |val| {
            return std.mem.eql(u8, this.val, val);
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
      .Top, .Instance, .Tag, .TaggedUnion, .TagOrClass => return false,
    }
    return false;
  }
};

pub const Union = struct {
  /// each discriminant of this union
  variants: TypeHashSet,

  pub inline fn init() @This() {
    return Union{.variants = TypeHashSet.init()};
  }

  pub inline fn toType(self: Union, al: Allocator) Type {
    return Type.compressTypes(@constCast(&self.variants), null, al).*;
  }

  pub inline fn isBoolUnionTy(self: *Union) bool {
    return (
      self.variants.count() == 2 and
      self.variants.get(Type.getConstantTrueHash()) != null and
      self.variants.get(Type.getConstantFalseHash()) != null
    );
  }

  pub fn set(self: *@This(), typ: *Type, al: Allocator) void {
    if (!typ.isUnion()) {
      self.variants.set(typ.typeid(), typ, al);
    } else {
      var uni = typ.union_();
      for (uni.variants.values()) |vr| {
        self.variants.set(vr.typeid(), vr, al);
      }
    }
  }

  pub inline fn isNullable(self: *@This()) bool {
    for (self.variants.values()) |ty| {
      if (ty.isNilTy()) return true;
    }
    return false;
  }

  pub fn addAll(self: *@This(), types: *TypeList, al: Allocator) void {
    for (types.items()) |ty| {
      self.set(ty, al);
    }
  }

  pub fn addSlice(self: *@This(), types: []const *Type, al: Allocator) void {
    for (types) |ty| {
      self.set(ty, al);
    }
  }

  pub fn isRelatedTo(this: *Union, other: *Type, ctx: RelationContext, al: Allocator) bool {
    // this -> T1, other -> T2
    switch (other.info) {
      .Union => {
        if (ctx == .RCTypeParams) {
          // .RCTypeParams constrains to 'exactness'
          if (this.variants.count() != other.union_().variants.count()) {
            return false;
          }
        }
        // related if each & every variants of T2 are related to any of the variants of T1 with given context
        for (other.union_().variants.values()) |variant| {
          if (!this.isRelatedTo(variant, .RCAny, al)) {
            return false;
          }
        }
        return true;
      },
      .Constant, .Concrete, .Generic, .Variable, .Recursive, .Function, .Class, .Top, .Instance, .TagOrClass => {
        if (this.isBoolUnionTy() and other.isBoolTy()) return true;
        if (ctx == .RCTypeParams) return false;
        // related if there exists a variant of T1 that is related to T2
        for (this.variants.values()) |variant| {
          if (variant.isRelatedTo(other, ctx, al)) {
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

  pub inline fn init() @This() {
    return TaggedUnion{.variants = TypeList.init()};
  }

  pub inline fn toType(self: TaggedUnion, al: Allocator) Type {
    return Type.compressTaggedTypes(@constCast(&self.variants), null, al).*;
  }

  pub inline fn set(self: *@This(), typ: *Type, al: Allocator) void {
    std.debug.assert(typ.isTag());
    self.variants.append(typ, al);
  }

  pub inline fn append(self: *@This(), typ: *Type, al: Allocator) void {
    self.variants.append(typ, al);
  }

  pub inline fn addAll(self: *@This(), types: *TypeList, al: Allocator) void {
    self.variants.extend(types, al);
  }

  pub fn addSlice(self: *@This(), types: []const *Type, al: Allocator) void {
    for (types) |ty| {
      self.set(ty, al);
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

  pub inline fn getTag(self: *@This(), name: []const u8) ?*Type {
    for (self.variants.items()) |tg| {
      if (tg.tag().nameEql(name)) {
        return tg;
      }
    }
    return null;
  }

  pub fn isResultTy(self: *@This()) bool {
    if (self.variants.len() != 2) return false;
    var has_error = false;
    var has_ok = false;
    for (self.variants.items()) |ty| {
      if (ty.isErrorTy()) {
        has_error = true;
      } else if (ty.isOkTy()) {
        has_ok = true;
      }
    }
    return has_error and has_ok;
  }

  pub fn _isRelatedTo(this: *TaggedUnion, other: *Type, ctx: RelationContext, al: Allocator) ?usize {
    // this -> T1, other -> T2
    switch (other.info) {
      .TaggedUnion => {
        if (ctx == .RCTypeParams) {
          // .RCTypeParams constrains to 'exactness'
          if (this.variants.len() != other.taggedUnion().variants.len()) {
            return null;
          }
        }
        // related if each & every variants of T2 are related to any of the variants of T1 with given context
        for (other.taggedUnion().variants.items()) |variant| {
          if (!this.isRelatedTo(variant, .RCAny, al)) {
            return null;
          }
        }
        return 0;
      },
      .Recursive => {
        if (this._isRelatedTo(other.recursive().base, ctx, al)) |i| {
          return i;
        }
      },
      .Constant, .Concrete, .Generic, .Variable, .Function, .Class, .Top, .Instance, .Tag, .TagOrClass => {
        if (ctx == .RCTypeParams) return null;
        // related if there exists a variant of T1 that is related to T2
        for (this.variants.items(), 0..) |variant, i| {
          if (variant.isRelatedTo(other, ctx, al)) {
            return i;
          }
        }
      },
      .Union => return null,
    }
    return null;
  }

  pub inline fn isRelatedTo(this: *TaggedUnion, other: *Type, ctx: RelationContext, al: Allocator) bool {
    // this -> T1, other -> T2
    return (this._isRelatedTo(other, ctx, al) != null);
  }
};

pub const Generic = struct {
  base: *Type,
  tparams: TypeList,

  pub inline fn init(base: *Type) @This() {
    return Generic {.tparams = TypeList.init(), .base = base};
  }

  pub inline fn toType(self: Generic) Type {
    return Type.init(.{.Generic = self});
  }

  pub inline fn getSlice(self: *@This()) []*Type {
    return self.tparams.items();
  }

  pub inline fn append(self: *@This(), typ: *Type, al: Allocator) void {
    self.tparams.append(typ, al);
  }

  pub inline fn tparamsLen(self: *@This()) usize {
    return self.tparams.len();
  }

  pub fn isRelatedTo(this: *Generic, other: *Type, ctx: RelationContext, al: Allocator) bool {
    switch (other.info) {
      .Generic => |*gen| {
        if (!this.base.isRelatedTo(gen.base, ctx, al)) return false;
        // expr is Type
        if (gen.tparamsLen() == 0 and ctx == .RCIs) return true;
        // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
        if (this.tparamsLen() != gen.tparamsLen()) return false;
        for (this.tparams.items(), 0..) |tparam, i| {
          const param = gen.tparams.itemAt(i);
          if (!tparam.isRelatedTo(param, .RCTypeParams, al)) {
            return false;
          }
        }
        return true;
      },
      .Concrete, .Constant, .Union, .Variable,
      .Recursive, .Function, .Class, .Top,
      .Instance, .Tag, .TaggedUnion, .TagOrClass => return false,
    }
    return false;
  }
};

pub const Variable = struct {
  tokens: TokenList,

  pub const TokenList = ds.ArrayListUnmanaged(Token);


  pub inline fn init() @This() {
    return Variable {.tokens = TokenList.init()};
  }

  pub inline fn appendTok(self: *@This(), name: Token, al: Allocator) void {
    self.tokens.append(name, al);
  }

  pub const append = appendTok;

  pub fn eql(self: *@This(), other: *@This()) bool {
    if (other.tokens.len() != self.tokens.len()) return false;
    for (other.tokens.items(), self.tokens.items()) |a, b| {
      if (!std.mem.eql(u8, a.lexeme(), b.lexeme())) {
        return false;
      }
    }
    return true;
  }

  pub fn len(self: *@This()) usize {
    return self.tokens.len();
  }

  pub fn getTokens(self: *@This()) *TokenList {
    return &self.tokens;
  }

  pub fn getFirst(self: *@This()) Token {
    return self.tokens.items()[0];
  }

  pub fn getFirstLexeme(self: *@This()) []const u8 {
    return self.tokens.items()[0].lexeme();
  }

  pub fn isRelatedTo(this: *Variable, other: *Type, ctx: RelationContext, al: Allocator) bool {
    _ = al;
    _ = ctx;
    return switch (other.info) {
      .Variable => |*vr| this.eql(vr),
      .Constant, .Concrete, .Union, .Recursive,
      .Function, .Generic, .Class, .Top,
      .Instance, .Tag, .TaggedUnion, .TagOrClass => false,
    };
  }
};

pub const Function = struct {
  tparams: ?[]*Type,
  data: *FunctionData,

  pub const FunctionData = struct {
    params: []*Type,
    ret: *Type,
    node: ?*Node,
  };

  pub inline fn init(ret: *Type, tparams: ?[]*Type, node: ?*Node, al: Allocator) @This() {
    return Function {
      .tparams = tparams,
      .data = util.box(
        FunctionData, .{.params = &[_]*Type{}, .ret = ret, .node = node}, al
      )
    };
  }

  pub inline fn isParameterized(self: *@This()) bool {
    return self.tparams != null;
  }

  pub inline fn toType(self: @This()) Type {
    return Type.init(.{.Function = self});
  }

  pub inline fn tparamsLen(self: *@This()) usize {
    return if (self.tparams) |tp| tp.len else 0;
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    if (self.tparams) |tp1| {
      if (other.tparams) |tp2| {
        if (tp1.len != tp2.len) {
          return false;
        }
      } else return false;
    } else if (other.tparams != null) {
      return false;
    }
    if (self.data.params.len != other.data.params.len) return false;
    if (!self.data.ret.typeidEql(other.data.ret)) return false;
    for (self.data.params, other.data.params) |a, b| {
      if (!a.typeidEql(b)) return false;
    }
    return true;
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, al: Allocator) bool {
    _ = al;
    _ = ctx;
    return switch (other.info) {
      .Function => |*fun| this.eql(fun),
      .Variable, .Constant, .Concrete, .Union,
      .Recursive, .Generic, .Class, .Top,
      .Instance, .Tag, .TaggedUnion, .TagOrClass => false,
    };
  }
};

pub const Class = struct {
  empty: bool,
  builtin: bool,
  immutable: bool = false,
  resolved: bool = false,
  tktype: TokenType,
  tparams: ?[]*Type,
  data: *ClassData,

  pub const ClassData = struct {
    name: []const u8,
    fields: ds.ArrayListUnmanaged(*Node),
    methods: TypeList,
    node: ?*Node,
  };

  pub inline fn init(
    name: []const u8, tktype: TokenType, fields: ds.ArrayListUnmanaged(*Node), methods: TypeList,
    tparams: ?[]*Type, node: ?*Node, empty: bool, builtin: bool, al: Allocator
  ) @This() {
    return Class {
      .empty = empty,
      .builtin = builtin,
      .tparams = tparams,
      .tktype = tktype,
      .data = util.box(ClassData, .{
        .name = name,
        .fields = fields,
        .methods = methods,
        .node = node,
      }, al), 
    };
  }

  fn isBuiltin(tkty: TokenType) bool {
    return switch (tkty) {
      .TkList, .TkTuple, .TkMap, .TkStr => true,
      else => false, 
    };
  }

  pub inline fn initWithDefault(name: []const u8, tktype: TokenType, al: Allocator) @This() {
    var cls = Class.init(
      name, tktype,
      ds.ArrayListUnmanaged(*Node).init(),
      TypeList.init(),
      null, null, false, false, al
    );
    cls.immutable = tktype.is(.TkTuple);
    cls.builtin = isBuiltin(tktype);
    return cls;
  }

  pub inline fn initTParams(self: *@This()) void {
    if (self.tparams == null) {
      self.tparams = &[_]*Type{};
    }
  }

  pub inline fn isParameterized(self: *@This()) bool {
    return self.tparams != null;
  }

  pub inline fn isInstantiatedGeneric(self: *@This()) bool {
    if (self.tparams) |tparams| {
      return (tparams.len > 0 and !tparams[0].isVariable());
    }
    return false;
  }

  pub inline fn tparamsLen(self: *@This()) usize {
    return if (self.tparams) |tp| tp.len else 0;
  }

  pub inline fn fieldsLen(self: *@This()) usize {
    return self.data.fields.len();
  }

  pub inline fn getSlice(self: *@This()) []*Type {
    return if (self.tparams) |tp| tp else &[_]*Type{};
  }

  pub fn appendTParam(self: *@This(), typ: *Type, al: Allocator) void {
    const len = self.tparamsLen();
    const slice = util.allocSlice(*Type, len + 1, al);
    @memcpy(slice[0..len], self.getSlice());
    slice[len] = typ.classOrInstanceClass();
    self.tparams = slice;
  }

  pub fn initTParamSlice(self: *@This(), typs: []*Type, _al: ?Allocator) void {
    if (_al) |al| {
      self.tparams = util.allocSlice(*Type, typs.len, al);
      @memcpy(self.tparams.?, typs);
    } else {
      self.tparams = typs;
    }
  }

  pub inline fn toType(self: @This()) Type {
    return Type.init(.{.Class = self});
  }

  pub fn eql(self: *@This(), other: *@This()) bool {
    return std.mem.eql(u8, self.data.name, other.data.name);
  }

  pub fn isStringClass(self: *@This()) bool {
    return self.builtin and self.tktype == .TkStr;
  }

  pub fn setAsResolved(self: *@This()) void {
    self.resolved = true;
  }

  pub fn getField(self: *@This(), name: []const u8) ?*Node {
    for (self.data.fields.items()) |fd| {
      if (std.mem.eql(u8, fd.getFieldLexeme(), name)) {
        return fd;
      }
    }
    return null;
  }

  pub fn getFieldIndex(self: *@This(), name: []const u8) ?usize {
    for (self.data.fields.items(), 0..) |fd, i| {
      if (std.mem.eql(u8, fd.getFieldLexeme(), name)) {
        return i;
      }
    }
    return null;
  }

  pub fn getFieldAtIndex(self: *@This(), index: usize) ?*Node {
    for (self.data.fields.items(), 0..) |fd, i| {
      if (i == index) {
        return fd;
      }
    }
    return null;
  }

  pub fn getMethod(self: *@This(), name: []const u8) ?*Node {
    if (self.data.node) |_node| {
      for (_node.NdClass.data.methods) |mth| {
        if (mth.NdBasicFun.data.name.?.valueEql(name)) {
          return mth;
        }
      }
    }
    return null;
  }

  pub fn getMethodIndex(self: *@This(), name: []const u8) ?usize {
    if (self.data.node) |_node| {
      for (_node.NdClass.data.methods, 0..) |mth, i| {
        if (mth.NdBasicFun.data.name.?.valueEql(name)) {
          return i;
        }
      }
    }
    return null;
  }

  pub fn getMethodTy(self: *@This(), name: []const u8) ?*Type {
    for (self.data.methods.items()) |mth| {
      if (mth.function().data.node.?.NdBasicFun.data.name.?.valueEql(name)) {
        return mth;
      }
    }
    return null;
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, al: Allocator) bool {
    return switch (other.info) {
      .Class => |*oth| {
        if (!std.mem.eql(u8, this.data.name, oth.data.name)) return false;
        // less specific to specific, for ex: lex x = []; x = [1, 2, 3]
        if (oth.empty and ctx == .RCConst) return true;
        // expr is Type
        if (this.tparamsLen() != oth.tparamsLen()) {
          return ctx == .RCIs;
        }
        if (this.tparams) |tparams1| {
          if (oth.tparams) |tparams2| {
            const _ctx = if (ctx != .RCIs) .RCTypeParams else ctx;
            for (tparams1, tparams2) |tp1, tp2| {
              if (!tp1.isRelatedTo(tp2, _ctx, al)) {
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
      .TagOrClass => |*tg| {
        if (ctx == .RCIs) return tg.nameEql(this.data.name);
        return false;
      },
      .Instance => this.isRelatedTo(other.instance().cls, ctx, al),
      .Constant => |*cons| {
        return this.isStringClass() and cons.kind == .TyString;
      },
      .Concrete => |*conc| {
        return this.isStringClass() and conc.kind == .TyString;
      },
      .Variable, .Union, .Recursive, .Function,
      .Generic, .Top, .Tag, .TaggedUnion, => false,
    };
  }
};

pub const Tag = struct {
  name: []const u8,
  fields: ?*TagFieldList,
  ty: TokenType,
  instantiated: bool = false,
  alias_is_parameterized: bool = false,


  pub const TagFieldList = ds.ArrayListUnmanaged(TagField);

  pub const TagField = struct {
    name: ?Token,
    typ: *Type,
    tdecl: ?*Type = null,

    pub fn clone(self: @This(), al: Allocator, map: *TypeHashMap) @This() {
      return @This(){
        .name = self.name,
        .typ = self.typ._clone(map, al),
        .tdecl = self.tdecl,
      };
    }
  };

  pub inline fn init(name: []const u8, ty: TokenType) @This() {
    return @This(){.name = name, .fields = null, .ty = ty};
  }

  pub inline fn isParameterized(self: *@This()) bool {
    return self.fields != null;
  }

  pub inline fn isInstantiated(self: *@This()) bool {
    return self.instantiated;
  }

  pub inline fn initFields(self: *@This(), al: Allocator) void {
    self.fields = TagFieldList.init().box(al);
  }

  pub fn initFieldTypes(self: *@This(), types: []const *Type, al: Allocator) void {
    self.initFields(al);
    for (types) |ty| {
      self.appendFieldType(ty, al);
    }
  }

  pub fn addFieldTypes(self: *@This(), types: []const *Type, al: Allocator) void {
    for (types) |ty| {
      self.appendFieldType(ty, al);
    }
  }

  pub fn addFields(self: *@This(), params: []const TagField, al: Allocator) void {
    for (params) |prm| {
      self.appendField(prm, al);
    }
  }

  pub inline fn nameEql(self: *@This(), name: []const u8) bool {
    return std.mem.eql(u8, self.name, name);
  }

  pub fn appendField(self: *@This(), param: TagField, al: Allocator) void {
    self.fields.?.append(param, al);
  }

  pub fn appendFieldType(self: *@This(), typ: *Type, al: Allocator) void {
    self.fields.?.append(.{.name = null, .typ = typ}, al);
  }

  pub fn getField(self: *@This(), idx: usize) ?TagField {
    if (self.fields) |params| {
      if (idx < params.len()) {
        return params.itemAt(idx);
      }
    }
    return null;
  }

  pub fn getFieldWithId(self: *@This(), id: []const u8) ?usize {
    if (self.fields) |params| {
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

  pub inline fn fieldsLen(self: *@This()) usize {
    return if (self.fields) |tp| tp.len() else 0;
  }

  pub inline fn fieldSlice(self: *@This()) []TagField {
    return if (self.fields) |tp| tp.items() else &[_]TagField{};
  }

  pub fn isRelatedTo(this: *Tag, other: *Type, ctx: RelationContext, al: Allocator) bool {
    switch (other.info) {
      .Tag => |*tg| {
        if (!this.nameEql(tg.name)) return false;
        const slice1 = this.fieldSlice();
        const slice2 = tg.fieldSlice();
        if (slice1.len != slice2.len) {
          return ctx == .RCIs;
        }
        // nested & if patterns do not require the tag's params to be fully typed
        // (i.e. no need for full expansion if they belong to other tagged unions)
        const _ctx = if (ctx != .RCIs) .RCTypeParams else ctx;
        for (slice1, slice2) |t1, t2| {
          if (!t1.typ.isRelatedTo(t2.typ, _ctx, al)) {
            return false;
          }
        }
        return true;
      },
      .TagOrClass => |*tc| {
        if (ctx == .RCIs) return this.nameEql(tc.name);
        return false;
      },
      else => return false,
    }
  }
};

pub const TagOrClass = struct {
  name: []const u8,
  tktype: TokenType,

  pub inline fn init(name: []const u8, tktype: TokenType) @This() {
    return @This(){.name = name, .tktype = tktype};
  }

  pub inline fn nameEql(self: *@This(), name: []const u8) bool {
    return std.mem.eql(u8, self.name, name);
  }

  pub inline fn toClass(self: *@This(), al: Allocator) *Type {
    return Type.newClass(self.name, self.tktype, al).box(al);
  } 

  pub inline fn toTag(self: *@This(), al: Allocator) *Type {
    return Type.newTag(self.name, self.tktype).box(al);
  }

  pub fn isRelatedTo(this: *TagOrClass, other: *Type, ctx: RelationContext, al: Allocator) bool {
    _ = al;
    _ = this;
    _ = ctx;
    _ = other;
    return false;
  }
};

pub const Instance = struct {
  cls: *Type,

  pub inline fn init(cls: *Type) @This() {
    return Instance {.cls = cls};
  }

  pub fn toType(self: @This()) Type {
    return Type.init(.{.Instance = self});
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, al: Allocator) bool {
    return switch (other.info) {
      .Instance => |*oth| this.cls.isRelatedTo(oth.cls, ctx, al),
      .Class => if (ctx == .RCIs) this.cls.isRelatedTo(other, ctx, al) else false,
      .TagOrClass => if (ctx == .RCIs) this.cls.isRelatedTo(other, ctx, al) else false,
      .Function, .Variable, .Constant, .Concrete, .Union,
      .Recursive, .Generic, .Top, .Tag, .TaggedUnion, => false,
    };
  }
};

pub const Recursive = struct {
  base: *Type,

  pub inline fn init(base: *Type) @This() {
    return @This() {.base = base};
  }

  pub fn isRelatedTo(this: *Recursive, other: *Type, ctx: RelationContext, al: Allocator) bool {
    var flat_this = this.base.unfold(al);
    var flat_other = other.unfold(al);
    start:
    for (flat_other.items()) |ty| {
      for (flat_this.items()) |rty| {
        if (rty.isRelatedTo(ty, ctx, al)) {
          continue :start;
        }
      }
      return false;
    }
    return true;
  }
};

pub const Top = struct {
  child: *Type,

  pub inline fn init(child: *Type) @This() {
    return Top {.child = child};
  }

  pub inline fn toType(self: @This()) Type {
    return Type.init(.{.Top = self});
  }

  pub fn isRelatedTo(this: *@This(), other: *Type, ctx: RelationContext, al: Allocator) bool {
    return switch (other.info) {
      .Top => |*tp| this.child.isRelatedTo(tp.child, ctx, al),
      .Instance, .Class => if (this.child.isClass()) this.child.isRelatedTo(other, ctx, al) else false,
      else => false,
    };
  }
};


comptime {
  std.debug.assert(@sizeOf(Node) == 40);
  std.debug.assert(@sizeOf(Type) == 56);
}
