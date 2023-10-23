const std = @import("std");
const lex = @import("lex.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const ptn = @import("pattern.zig");
pub const ds = @import("ds.zig");

const Type = types.Type;
const OpType = lex.OpType;
pub const Token = lex.Token;
pub const AstList = ds.ArrayList(*AstNode);
pub const VarDeclList = ds.ArrayList(VarDeclNode);

// ast node types
pub const AstType = enum {
  AstNumber,
  AstString,
  AstBool,
  AstBinary,
  AstUnary,
  AstList,
  AstTuple,
  AstMap,
  AstExprStmt,
  AstVarDecl,
  AstVar,
  AstAssign,
  AstBlock,
  AstNType,
  AstAlias,
  AstCast,
  AstSubscript,
  AstNil,
  AstEmpty,
  AstDeref,
  AstIf,
  AstCondition,
  AstMCondition,
  AstElif,
  AstSimpleIf,
  AstWhile,
  AstControl,
  AstFun,
  AstRet,
  AstCall,
  AstError,
  AstOrElse,
  AstClass,
  AstDotAccess,
  AstScope,
  AstLblArg,
  AstMatch,
  AstFail,
  AstProgram,
};

// ast nodes
pub const LiteralNode = struct {
  token: Token,
  value: f64,
  typ: ?*Type = null,

  pub fn init(token: Token) @This() {
    return @This() {.token = token, .value = undefined};
  }

  pub inline fn line(self: *@This()) usize {
    return self.token.line;
  }

  pub fn clone(self: *@This(), node: *AstNode, al: std.mem.Allocator) *AstNode {
    var lit = LiteralNode.init(self.token);
    var new = util.alloc(AstNode, al);
    switch (node.*) {
      .AstNumber => {
        lit.value = self.value;
        new.* = .{.AstNumber = lit};
      },
      .AstString => new.* = .{.AstString = lit},
      .AstBool => new.* = .{.AstBool = lit},
      .AstNil => new.* = .{.AstNil = lit},
      else => unreachable,
    }
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = depth;
    if (self.token.ty == .TkNumber) {
      return std.fmt.allocPrint(al, "{d}", .{self.value}) catch self.token.value;
    }
    return self.token.value;
  }
};

pub const BinaryNode = struct {
  left: *AstNode,
  right: *AstNode,
  op: lex.Optr,
  /// allow const narrowing in `is` op expressions
  allow_consts: bool = false,
  /// allow 'rested' tests in `is` op expressions
  allow_rested: bool = false, 
  typ: ?*Type = null,

  pub fn init(left: *AstNode, right: *AstNode, op: Token) @This() {
    return @This() {
      .left = left,
      .right = right,
      .op = lex.Optr.init(op),
    };
  }

  pub inline fn line(self: *@This()) usize {
    return self.op.token.line;
  }

  pub fn clone(self: *@This(), node: *AstNode, al: std.mem.Allocator) *AstNode {
    var bin = @This() {
      .left = self.left.clone(al),
      .right = self.right.clone(al),
      .op = self.op,
    };
    bin.allow_consts = self.allow_consts;
    bin.allow_rested = self.allow_rested;
    var new = util.alloc(AstNode, al);
    switch (node.*) {
      .AstBinary => new.* = .{.AstBinary = bin},
      .AstAssign => new.* = .{.AstAssign = bin},
      else => unreachable,
    }
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(try self.left.render(depth, al));
    _ = try writer.write(" ");
    _ = try writer.write(self.op.token.value);
    _ = try writer.write(" ");
    _ = try writer.write(try self.right.render(depth, al));
    if (self.allow_rested) {
      _ = try writer.write(" [rested] ");
    }
    return writer.context.items;
  }
};

pub const SubscriptNode = struct {
  expr: *AstNode,
  index: *AstNode,
  narrowed: ?*VarNode = null,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, index: *AstNode) @This() {
    return @This() { .expr = expr, .index = index};
  }

  pub inline fn line(self: *@This()) usize {
    return self.index.getToken().line;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var sub = SubscriptNode.init(self.expr.clone(al), self.index.clone(al));
    if (self.narrowed) |narrowed| {
      sub.narrowed = &narrowed.clone(al).AstVar;
    }
    var new = util.alloc(AstNode, al);
    new.* = .{.AstSubscript = sub};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(try self.expr.render(depth, al));
    _ = try writer.write("[");
    _ = try writer.write(try self.index.render(depth, al));
    _ = try writer.write("]");
    return writer.context.items;
  }
};

pub const UnaryNode = struct {
  expr: *AstNode,
  op: lex.Optr,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, op: Token) @This() {
    return @This() {.expr = expr, .op = lex.Optr.init(op)};
  }

   pub inline fn line(self: *@This()) usize {
    return self.op.token.line;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var una =  @This() {.expr = self.expr.clone(al), .op = self.op};
    var new = util.alloc(AstNode, al);
    new.* = .{.AstUnary = una};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(self.op.token.value);
    _ = try writer.write(try self.expr.render(depth, al));
    return writer.context.items;
  }
};

pub const ListNode = struct {
  elems: AstList,
  typ: ?*Type = null,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.elems = AstList.init(allocator)};
  }

  pub fn clone(self: *@This(), node: *AstNode, al: std.mem.Allocator) *AstNode {
    var list = @This() {.elems = AstList.clone(&self.elems, al)};
    var new = util.alloc(AstNode, al);
    switch (node.*) {
      .AstList => new.* = .{.AstList = list},
      .AstTuple => new.* = .{.AstTuple = list},
      else => unreachable,
    }
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    _ = al;
    _ = depth;
    return "List(args..)";
  }
};

pub const MapNode = struct {
  pairs: PairList,
  typ: ?*Type = null,

  pub const Pair = struct {key: *AstNode, value: *AstNode};
  pub const PairList = ds.ArrayList(Pair);

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.pairs = PairList.init(allocator)};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var map = MapNode.init(al);
    map.pairs.ensureTotalCapacity(self.pairs.capacity());
    for (self.pairs.items()) |itm| {
      map.pairs.append(@as(Pair, .{.key = itm.key.clone(al), .value = itm.value.clone(al)}));
    }
    var new = util.alloc(AstNode, al);
    new.* = .{.AstMap = map};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    _ = al;
    _ = depth;
    return "Map(args..)";
  }
};

pub const VarNode = struct {
  token: lex.Token,
  typ: ?*Type,

  pub fn init(token: Token) @This() {
    return @This() {.token = token, .typ = null};
  }

  pub inline fn line(self: *@This()) usize {
    return self.token.line;
  }

  pub inline fn box(self: *const @This(), al: std.mem.Allocator) *@This() {
    var new = util.alloc(VarNode, al);
    new.* = self.*;
    return new;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var typ: ?*Type = null;
    if (self.typ) |ty| {
      typ = ty.clone(al);
    }
    var new = util.alloc(AstNode, al);
    new.* = .{.AstVar = @This() {.token = self.token, .typ = typ}};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    return self.token.value;
  }
};

pub const ExprStmtNode = struct {
  expr: *AstNode,

  pub fn init(expr: *AstNode) @This() {
    return @This() {.expr = expr};
  }

  pub fn isSelfDotAccessAssignment(self: *@This()) ?*DotAccessNode {
    if (self.expr.isAssign() and self.expr.AstAssign.op.optype == .OpAssign) {
      if (self.expr.AstAssign.left.isDotAccess()) {
        if (self.expr.AstAssign.left.AstDotAccess.lhs.isVariable()) {
          var value = self.expr.AstAssign.left.AstDotAccess.lhs.AstVar.token.value;
          if (std.mem.eql(u8, "self", value)) {
            return &self.expr.AstAssign.left.AstDotAccess;
          }
        }
      }
    }
    return null;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var es = ExprStmtNode.init(self.expr.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstExprStmt = es};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    _ = al;
    _ = depth;
    return "ExprStmt(expr)";
  }
};

pub const VarDeclNode = struct {
  ident: *VarNode,
  value: *AstNode,
  /// whether this node has a type annotation
  has_annotation: bool = false,
  /// whether this node is a function/method parameter
  is_param: bool = false,
  /// whether this node is a class field
  is_field: bool = false,
  /// whether this node being a class field has a default value
  has_default: bool = false,

  pub fn init(ident: *VarNode, value: *AstNode, is_param: bool) @This() {
    return @This() {.ident = ident, .value = value, .is_param = is_param};
  }

  pub inline fn line(self: *@This()) usize {
    return self.ident.line();
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var value: *AstNode = (
      if (self.is_param or (self.is_field and !self.has_default)) self.value
      else self.value.clone(al)
    );
    var vn = VarDeclNode.init(&self.ident.clone(al).AstVar, value, self.is_param);
    vn.is_field = self.is_field;
    vn.has_default = self.has_default;
    vn.has_annotation = self.has_annotation;
    var new = util.alloc(AstNode, al);
    new.* = .{.AstVarDecl = vn};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try util.addDepth(&writer, depth);
    var decl = std.fmt.allocPrint(
      al, "let {s} = {s}",
      .{self.ident.token.value, try self.value.render(depth, al)}
    ) catch unreachable;
    _ = try writer.write(decl);
    return writer.context.items;
  }
};

pub const BlockNode = struct {
  nodes: AstList,
  /// whether this block has been successfully typechecked
  checked: bool = false,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.nodes = AstList.init(allocator)};
  }

  pub fn newEmptyBlock(alloc: std.mem.Allocator) *AstNode {
    var block = util.alloc(AstNode, alloc);
    block.* = .{.AstBlock = BlockNode.init(alloc)};
    return block;
  }

  pub fn getLast(self: *BlockNode) ?*AstNode {
    if (self.nodes.isNotEmpty()) return self.nodes.getLast();
    return null;
  }

  pub fn getSecondLast(self: *BlockNode) ?*AstNode {
    if (self.nodes.len() > 1) return self.nodes.itemAt(self.nodes.len() - 2);
    return null;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var bl = @This() {.nodes = AstList.clone(&self.nodes, al)};
    var block = util.alloc(AstNode, al);
    block.* = .{.AstBlock = bl};
    return block;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write("<block>\n");
    for (self.nodes.items()) |node| {
      _ = try writer.write(try node.render(depth + 1, al));
      _ = try writer.write("\n");
    }
    try util.addDepth(&writer, depth);
    _ = try writer.write("</block>\n");
    return writer.context.items;
  }
};

pub const TypeNode = struct {
  typ: *Type,
  token: Token,
  /// track whether this type was created in an alias or annotation context
  from_alias_or_annotation: bool = false,

  pub fn init(typ: *Type, token: Token) @This() {
    return @This() {.typ = typ, .token = token};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var node = util.alloc(AstNode, al);
    node.* = .{.AstNType = @This() {
      .typ = self.typ.clone(al),
      .token = self.token,
      .from_alias_or_annotation = self.from_alias_or_annotation
    }};
    return node;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = depth;
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(self.typ.typename(al));
    return writer.context.items;
  }
};

pub const AliasNode = struct {
  alias: *TypeNode,
  aliasee: *TypeNode,
  typ: *Type, // alias and aliasee is set in `typ`

  pub fn init(alias: *TypeNode, aliasee: *TypeNode) @This() {
    aliasee.typ.alias = alias.typ;
    alias.from_alias_or_annotation = true;
    aliasee.from_alias_or_annotation = true;
    return @This() {.alias = alias, .aliasee = aliasee, .typ = alias.typ};
  }

  pub fn clone(self: *@This(), node: *AstNode, al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    return node;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<type alias>";
  }
};

/// null dereference: expr.?
pub const DerefNode = struct {
  token: Token,
  expr: *AstNode,
  narrowed: ?*VarNode = null,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, token: Token) @This() {
    return @This() {.expr = expr, .token = token};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var drf = DerefNode.init(self.expr.clone(al), self.token);
    if (self.narrowed) |narrowed| {
      drf.narrowed = &narrowed.clone(al).AstVar;
    }
    var new = util.alloc(AstNode, al);
    new.* = .{.AstDeref = drf};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<deref>";
  }
};

pub const ConditionNode = struct {
  cond: *AstNode,

  pub fn init(cond: *AstNode) @This() {
    return @This() {.cond = cond};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    unreachable;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    return self.cond.render(depth, al);
  }
};

/// wrapper for a condition/test generated from a match pattern
pub const MatchConditionNode = struct {
  tst: *AstNode,

  pub fn init(cond: *AstNode) @This() {
    return @This() {.tst = cond};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    unreachable;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    return self.tst.render(depth, al);
  }
};

pub const EmptyNode = struct {
  token: Token,

  pub fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub fn clone(self: *@This(), node: *AstNode, al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    return node;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<empty>";
  }
};

pub const CastNode = struct {
  expr: *AstNode,
  typn: *TypeNode,

  pub fn init(expr: *AstNode, typn: *TypeNode) @This() {
    return @This() {.expr = expr, .typn = typn};
  }

  pub inline fn line(self: *@This()) usize {
    return self.typn.token.line;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var cn = CastNode.init(self.expr.clone(al), &self.typn.clone(al).AstNType);
    var new = util.alloc(AstNode, al);
    new.* = .{.AstCast = cn};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<cast>";
  }
};

pub const ElifNode = struct {
  cond: *AstNode,
  then: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode) @This() {
    return @This() {.cond = cond, .then = then};
  }

  pub fn toIf(self: *ElifNode, alloc: std.mem.Allocator) IfNode {
    return IfNode.init(
      self.cond, self.then, AstList.init(alloc),
      BlockNode.newEmptyBlock(alloc),
    );
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var el = ElifNode.init(self.cond.clone(al), self.then.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstElif = el};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<elif>";
  }
};

pub const IfNode = struct {
  cond: *AstNode,
  then: *AstNode,
  elifs: AstList,
  els: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode, elifs: AstList, els: *AstNode) @This() {
    return @This() {.cond = cond, .then = then, .elifs = elifs, .els = els};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var el = IfNode.init(
      self.cond.clone(al),
      self.then.clone(al),
      AstList.clone(&self.elifs, al),
      self.els.clone(al)
    );
    var new = util.alloc(AstNode, al);
    new.* = .{.AstIf = el};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<if>";
  }
};

pub const SimpleIfNode = struct {
  cond: *AstNode,
  then: *AstNode,
  els: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode, els: *AstNode) @This() {
    return @This() {.cond = cond, .then = then, .els = els};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    unreachable;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try util.addDepth(&writer, depth);
    _ = try writer.write("if (");
    _ = try writer.write(try self.cond.render(depth, al));
    _ = try writer.write(")\n");
    try util.addDepth(&writer, depth);
    _ = try writer.write(try self.then.render(depth, al));
    try util.addDepth(&writer, depth);
    _ = try writer.write("else\n");
    try util.addDepth(&writer, depth);
    _ = try writer.write(try self.els.render(depth, al));
    try util.addDepth(&writer, depth);
    _ = try writer.write("end");
    return writer.context.items;
  }
};

pub const WhileNode = struct {
  cond: *AstNode,
  then: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode) @This() {
    return @This() {.cond = cond, .then = then};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var wh = WhileNode.init(self.cond.clone(al), self.then.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstWhile = wh};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<while>";
  }
};

pub const ControlNode = struct {
  token: Token,
  /// the slot in which this node is compiled to an instruction
  patch_index: usize = 0,

  pub fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub fn isBreak(self: ControlNode) bool {
    return self.token.ty == .TkBreak;
  }

  pub fn isContinue(self: ControlNode) bool {
    return self.token.ty == .TkContinue;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var cn = ControlNode.init(self.token);
    var new = util.alloc(AstNode, al);
    new.* = .{.AstControl = cn};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<control>";
  }
};

pub const CallNode = struct {
  variadic: bool = false,
  labeled: bool = false,
  va_start: usize = 0,
  expr: *AstNode,
  targs: ?*AstList = null,
  args: AstList,
  typ: ?*Type = null,

  pub fn init(
    expr: *AstNode, args: AstList, targs: ?*AstList,
    va_start: usize, variadic: bool, labeled: bool
  ) @This() {
    return @This() {
      .expr = expr, .args = args, .targs = targs,
      .va_start = va_start, .variadic = variadic, .labeled = labeled
    };
  }

  pub inline fn isGeneric(self: *@This()) bool {
    return self.targs != null;
  }

  pub fn transformVariadicArgs(self: *@This()) void {
    var al = self.args.allocator();
    var tuple = AstList.init(al);
    tuple.appendSlice(self.args.items()[self.va_start..]);
    var node = util.alloc(AstNode, al);
    node.* = .{.AstTuple = .{.elems = tuple}};
    if (self.args.isNotEmpty()) {
      self.args.items()[self.va_start] = node;
    } else {
      self.args.append(node);
    }
    self.args.list.items.len = self.va_start + 1;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var targs: ?*AstList = null;
    if (self.targs) |list| {
      targs = AstList.clone(list, al).box();
    }
    var call = CallNode.init(
      self.expr.clone(al), AstList.clone(&self.args, al), targs,
      self.va_start, self.variadic, self.labeled
    );
    var new = util.alloc(AstNode, al);
    new.* = .{.AstCall = call};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(try self.expr.render(depth, al));
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
    _ = try writer.write("(");
    const len = self.args.len() -| 1;
    for (self.args.items(), 0..) |itm, i| {
      _ = try writer.write(try itm.render(depth, al));
      if (i < len) _ = try writer.write(",\n");
    }
    _ = try writer.write(")");
    return writer.context.items;
  }
};

pub const ErrorNode = struct {
  expr: *AstNode,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode) @This() {
    return @This() {.expr = expr};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var er = ErrorNode.init(self.expr.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstError = er};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write("(");
    _ = try writer.write(try self.expr.render(depth, al));
    _ = try writer.write(")!");
    return writer.context.items;
  }
};

pub const OrElseNode = struct {
  ok: *AstNode,
  err: *AstNode,
  evar: ?*VarNode,
  from_try: bool = false,
  typ: ?*Type = null,

  pub fn init(ok: *AstNode, err: *AstNode, evar: ?*VarNode) @This() {
    return @This() {.ok = ok, .err = err, .evar = evar};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var evar: ?*VarNode = null;
    if (self.evar) |ev| evar = &ev.clone(al).AstVar;
    var oe = OrElseNode.init(self.ok.clone(al), self.err.clone(al), evar);
    oe.from_try = self.from_try;
    var new = util.alloc(AstNode, al);
    new.* = .{.AstOrElse = oe};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<orelse>";
  }
};

pub const FunNode = struct {
  params: VarDeclList,
  tparams: ?*types.TypeList = null,
  body: *AstNode,
  name: ?*VarNode,
  ret: ?*AstNode = null,
  is_builtin: bool,
  variadic: bool,

  pub fn init(
    params: VarDeclList, body: *AstNode, name: ?*VarNode, ret: ?*AstNode,
    tparams: ?*types.TypeList, is_builtin: bool, variadic: bool
  ) @This() {
    return @This() {
      .params = params, .body = body, .name = name, .ret = ret,
      .tparams = tparams, .is_builtin = is_builtin, .variadic = variadic
    };
  }

  pub inline fn isGeneric(self: @This()) bool {
    return self.tparams != null;
  }

  pub inline fn isAnonymous(self: @This()) bool {
    return self.name == null;
  }

  pub inline fn isChecked(self: @This()) bool {
    return self.body.AstBlock.checked;
  }

  pub inline fn getName(self: *@This()) ?[]const u8 {
    return if (self.name) |name| name.token.value else null;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var params = VarDeclList.init(al);
    for (self.params.items()) |*param| {
      params.append(
        @as(VarDeclNode, .{
          .ident = &param.ident.clone(al).AstVar,
          .value = if (!param.is_param) param.value.clone(al) else param.value,
          .is_param = param.is_param
        })
      );
    }
    var ret: ?*AstNode = null;
    if (self.ret) |expr| {
      ret = expr.clone(al);
    }
    // don't clone tparams, they're always substituted.
    // don't clone name, it'll be updated.
    var fun = FunNode.init(
      params, self.body.clone(al), self.name, ret,
      self.tparams, self.is_builtin, self.variadic
    );
    var new = util.alloc(AstNode, al);
    new.* = .{.AstFun = fun};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<function>";
  }
};

pub const DotAccessNode = struct {
  lhs: *AstNode,
  rhs: *AstNode,
  narrowed: ?*VarNode = null,
  typ: ?*Type = null,

  pub fn init(lhs: *AstNode, rhs: *AstNode) @This() {
    return @This() { .lhs = lhs, .rhs = rhs};
  }

  pub inline fn line(self: *@This()) usize {
    return self.rhs.getToken().line;
  }

  pub inline fn isSelfExpr(self: *@This()) bool {
    return self.lhs.isVariable() and self.lhs.AstVar.token.ty == .TkSelf;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var da = DotAccessNode.init(self.lhs.clone(al), self.rhs.clone(al));
    if (self.narrowed) |narrowed| {
      da.narrowed = &narrowed.clone(al).AstVar;
    }
    var new = util.alloc(AstNode, al);
    new.* = .{.AstDotAccess = da};
    return new;
  }
  
  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) anyerror![]const u8 {
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write(try self.lhs.render(depth, al));
    _ = try writer.write(".");
    _ = try writer.write(try self.rhs.render(depth, al));
    return writer.context.items;
  }
};

pub const ClassNode = struct {
  name: *VarNode,
  trait: ?*Type = null,
  tparams: ?*types.TypeList = null,
  fields: *AstList,
  methods: *AstList,
  typ: ?*Type = null,
  is_builtin: bool,
  checked: bool,

  pub fn init(
    name: *VarNode, tparams: ?*types.TypeList, trait: ?*Type, fields: *AstList,
    methods: *AstList, is_builtin: bool, checked: bool
  ) @This() {
    return @This() {
      .name = name, .tparams = tparams, .trait = trait, .fields = fields,
      .methods = methods, .is_builtin = is_builtin, .checked = checked
    };
  }

  pub inline fn isGeneric(self: @This()) bool {
    return self.tparams != null;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var name = &self.name.clone(al).AstVar;
    var trait: ?*Type = if (self.trait) |trait| trait.clone(al) else self.trait;
    var fields: *AstList = AstList.init(al).boxEnsureCapacity(self.fields.capacity());
    for (self.fields.items()) |itm| {
      fields.append(itm.AstVarDecl.clone(al));
    }
    var methods: *AstList = AstList.init(al).boxEnsureCapacity(self.methods.capacity());
    for (self.methods.items()) |itm| {
      methods.append(itm.AstFun.clone(al));
    }
    var typ: ?*Type = if (self.typ) |typ| typ.clone(al) else self.typ;
    // don't clone tparams, they're always substituted.
    var cls = ClassNode.init(name, self.tparams, trait, fields, methods, self.is_builtin, false);
    cls.typ = typ;
    var new = util.alloc(AstNode, al);
    new.* = .{.AstClass = cls};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<class>";
  }
};

pub const LblArgNode = struct {
  label: Token,
  value: *AstNode,
  ident: *AstNode,

  pub fn init(label: Token, value: *AstNode, ident: *AstNode) @This() {
    return @This() {.label = label, .value = value, .ident = ident};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var arg = @This().init(self.label, self.value.clone(al), self.ident.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstLblArg = arg};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<lbl arg>";
  }
};

pub const MatchNode = struct {
  /// debug token
  token: Token,
  /// var decl if expr was converted to one
  decl: ?*AstNode = null,
  /// match expr
  expr: *AstNode,
  /// case nodes
  cases: CaseList,
  /// compiled lowered form
  lnode: *AstNode = undefined,

  pub const CaseList = ds.ArrayList(*ptn.Case);

  pub fn init(token: Token, expr: *AstNode, cases: CaseList) @This() {
    return @This() {.token = token, .expr = expr, .cases = cases};
  }

  pub inline fn getVariableOfInterest(self: *@This()) Token {
    return self.expr.AstVar.token;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var match = @This().init(self.token, self.expr.clone(al), ds.ArrayList(*ptn.Case).clone(&self.cases, al));
    match.decl = if (self.decl) |decl| decl.clone(al) else self.decl;
    var new = util.alloc(AstNode, al);
    new.* = .{.AstMatch = match};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![] const u8 {
    _ = depth;
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = try writer.write("match [expr]\n");
    for (self.cases.items()) |case| {
      _ = try writer.write(try case.render(0, al));
    }
    return writer.context.items;
  }
};

pub const FailNode = struct {
  token: Token,

  pub fn init(token: Token) @This() {
    return @This() {.token = token};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try util.addDepth(&writer, depth);
    _ = try writer.write("Fail\n");
    return writer.context.items;
  }
};

pub const RetNode = struct {
  token: Token,
  expr: ?*AstNode,
  typ: ?*Type = null,

  pub fn init(expr: ?*AstNode, token: Token) @This() {
    return @This() {.expr = expr, .token = token};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var expr: ?*AstNode = null;
    if (self.expr) |exp| {
      expr = exp.clone(al);
    }
    var ret = RetNode.init(expr, self.token);
    var new = util.alloc(AstNode, al);
    new.* = .{.AstRet = ret};
    return new;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<return>";
  }
};

pub const ScopeNode = struct {
  enter: bool,
  leave: bool,

  pub fn init(enter: bool, leave: bool) @This() {
    return @This() {.enter = enter, .leave = leave};
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = self;
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    try util.addDepth(&writer, depth);
    _ = try writer.write("<scope>");
    return writer.context.items;
  }
};

// TODO: refactor to BlockNode if no other useful info needs to be added.
pub const ProgramNode = struct {
  decls: AstList,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.decls = AstList.init(allocator)};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    unreachable;
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    _ = al;
    _ = depth;
    _ = self;
    return "<program>";
  }
};

pub const AstNode = union(AstType) {
  AstNumber: LiteralNode,
  AstString: LiteralNode,
  AstBool: LiteralNode,
  AstNil: LiteralNode,
  AstBinary: BinaryNode,
  AstUnary: UnaryNode,
  AstList: ListNode,
  AstTuple: ListNode,
  AstMap: MapNode,
  AstExprStmt: ExprStmtNode,
  AstVarDecl: VarDeclNode,
  AstVar: VarNode,
  AstAssign: BinaryNode,
  AstBlock: BlockNode,
  AstNType: TypeNode,
  AstAlias: AliasNode,
  AstCast: CastNode,
  AstSubscript: SubscriptNode,
  AstEmpty: EmptyNode,
  AstDeref: DerefNode,
  AstIf: IfNode,
  AstElif: ElifNode,
  AstSimpleIf: SimpleIfNode,
  AstCondition: ConditionNode,
  AstMCondition: MatchConditionNode,
  AstWhile: WhileNode,
  AstControl: ControlNode,
  AstFun: FunNode,
  AstRet: RetNode,
  AstCall: CallNode,
  AstError: ErrorNode,
  AstOrElse: OrElseNode,
  AstClass: ClassNode,
  AstDotAccess: DotAccessNode,
  AstScope: ScopeNode,
  AstLblArg: LblArgNode,
  AstMatch: MatchNode,
  AstFail: FailNode,
  AstProgram: ProgramNode,

  pub inline fn isComptimeConst(self: *@This()) bool {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => true,
      else => false,
    };
  }

  pub inline fn isVariable(self: *@This()) bool {
    return switch (self.*) {
      .AstVar => true,
      else => false,
    };
  }

  pub inline fn isSubscript(self: *@This()) bool {
    return switch (self.*) {
      .AstSubscript => true,
      else => false,
    };
  }

  pub inline fn isUnary(self: *@This()) bool {
    return switch (self.*) {
      .AstUnary => true,
      else => false,
    };
  }

  pub inline fn isBinary(self: *@This()) bool {
    return switch (self.*) {
      .AstBinary => true,
      else => false,
    };
  }

  pub inline fn isAssign(self: *@This()) bool {
    return switch (self.*) {
      .AstAssign => true,
      else => false,
    };
  }

  pub inline fn isBlock(self: *@This()) bool {
    return switch (self.*) {
      .AstBlock => true,
      else => false,
    };
  }

  pub inline fn isEmpty(self: *@This()) bool {
    return switch (self.*) {
      .AstEmpty => true,
      else => false,
    };
  }

  pub inline fn isOrElse(self: *@This()) bool {
    return switch (self.*) {
      .AstOrElse => true,
      else => false,
    };
  }

  pub inline fn isCondition(self: *@This()) bool {
    return switch (self.*) {
      .AstCondition => true,
      else => false,
    };
  }

  pub inline fn isMCondition(self: *@This()) bool {
    return switch (self.*) {
      .AstMCondition => true,
      else => false,
    };
  }

  pub inline fn isBool(self: *@This()) bool {
    return switch (self.*) {
      .AstBool => true,
      else => false,
    };
  }

  pub inline fn isDeref(self: *@This()) bool {
    return switch (self.*) {
      .AstDeref => true,
      else => false,
    };
  }

  pub inline fn isNilLiteral(self: *@This()) bool {
    return switch (self.*) {
      .AstNil => true,
      else => false,
    };
  }

  pub inline fn isConstLiteral(self: *@This()) bool {
    return switch (self.*) {
      .AstBool, .AstString, .AstNumber => true,
      else => false,
    };
  }

  pub inline fn isCast(self: *@This()) bool {
    return switch (self.*) {
      .AstCast => true,
      else => false,
    };
  }

  pub inline fn isTypeN(self: *@This()) bool {
    return switch (self.*) {
      .AstNType => true,
      else => false,
    };
  }

  pub inline fn isTypeAlias(self: *@This()) bool {
    return switch (self.*) {
      .AstAlias => true,
      else => false,
    };
  }

  pub inline fn isSimpleIf(self: *@This()) bool {
    return switch (self.*) {
      .AstSimpleIf => true,
      else => false,
    };
  }

  pub inline fn isWhile(self: *@This()) bool {
    return switch (self.*) {
      .AstWhile => true,
      else => false,
    };
  }

  pub inline fn isControl(self: *@This()) bool {
    return switch (self.*) {
      .AstControl => true,
      else => false,
    };
  }

  pub inline fn isScope(self: *@This()) bool {
    return switch (self.*) {
      .AstScope => true,
      else => false,
    };
  }

  pub inline fn isExitScope(self: *@This()) bool {
    return switch (self.*) {
      .AstScope => |sc| sc.leave,
      else => false,
    };
  }

  pub inline fn isFun(self: *@This()) bool {
    return switch (self.*) {
      .AstFun => true,
      else => false,
    };
  }

  pub inline fn isFail(self: *@This()) bool {
    return switch (self.*) {
      .AstFail => true,
      else => false,
    };
  }

  pub inline fn isClass(self: *@This()) bool {
    return switch (self.*) {
      .AstClass => true,
      else => false,
    };
  }

  pub inline fn isRet(self: *@This()) bool {
    return switch (self.*) {
      .AstRet => true,
      else => false,
    };
  }

  pub inline fn isExprStmt(self: *@This()) bool {
    return switch (self.*) {
      .AstExprStmt => true,
      else => false,
    };
  }

  pub inline fn isDotAccess(self: *@This()) bool {
    return switch (self.*) {
      .AstDotAccess => true,
      else => false,
    };
  }

  pub inline fn isCall(self: *@This()) bool {
    return switch (self.*) {
      .AstCall => true,
      else => false,
    };
  }

  pub inline fn isVarDecl(self: *@This()) bool {
    return switch (self.*) {
      .AstVarDecl => true,
      else => false,
    };
  }

  pub inline fn isLblArg(self: *@This()) bool {
    return switch (self.*) {
      .AstLblArg => true,
      else => false,
    };
  }

  pub fn getNarrowed(self: *@This()) ?*VarNode {
    return switch (self.*) {
      .AstVar => |*vr| vr,
      .AstSubscript => |*sub| sub.narrowed,
      .AstDeref => |*der| der.narrowed,
      .AstDotAccess => |*da| da.narrowed,
      else => null,
    };
  }

  pub inline fn block(self: *@This()) *BlockNode {
    return &self.AstBlock;
  }

  pub inline fn eql(self: *@This(), other: *@This()) bool {
    return self == other;
  }

  pub fn getType(self: *@This()) ?*Type {
    return switch (self.*) {
      .AstExprStmt => |*stmt| stmt.expr.getType(),
      .AstVarDecl => |*decl| decl.ident.typ,
      .AstCast => |*cst| cst.typn.typ,
      .AstSubscript => |sub| if (sub.narrowed) |nrw| nrw.typ else sub.typ,
      .AstDeref => |der| if (der.narrowed) |nrw| nrw.typ else der.typ,
      .AstCondition => |*cnd| cnd.cond.getType(),
      .AstMCondition => |*nd| nd.tst.getType(),
      .AstFun => |*fun| if (fun.ret) |ret| ret.AstNType.typ else null,
      .AstBlock, .AstIf, .AstElif,
      .AstWhile, .AstControl, .AstScope,
      .AstLblArg, .AstMatch, .AstFail,
      .AstEmpty, .AstSimpleIf, .AstProgram => null,
      inline else => |*nd| nd.typ,
    };
  }

  pub fn setType(self: *@This(), typ: *Type) void {
    switch (self.*) {
      .AstUnary => |*una| una.typ = typ,
      .AstVar => |*id| id.typ = typ,
      .AstCast => |*cst| cst.typn.typ = typ,
      .AstCall => |*call| call.typ = typ,
      .AstDotAccess => |*dot| {
        if (dot.narrowed) |nrw| nrw.typ = typ
        else dot.typ = typ;
      },
      .AstSubscript => |*sub| {
        if (sub.narrowed) |nrw| nrw.typ = typ
        else sub.typ = typ;
      },
      .AstDeref => |*der| {
        if (der.narrowed) |nrw| nrw.typ = typ
        else der.typ = typ;
      },
      .AstScope => {},
      .AstString => |*lit| {
        lit.typ = typ;
      },
      else => {
        std.log.debug("Attempt to set type on node: {}", .{self});
      },
    }
  }

  pub fn forceSetType(self: *@This(), typ: *Type) void {
    switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |*lit| lit.typ = typ,
      .AstBinary, .AstAssign => |*bin| bin.typ = typ,
      .AstUnary => |*una| una.typ = typ,
      .AstDotAccess => |*dot| {
        if (dot.narrowed) |nrw| nrw.typ = typ
        else dot.typ = typ;
      },
      .AstSubscript => |*sub| {
        if (sub.narrowed) |nrw| nrw.typ = typ
        else sub.typ = typ;
      },
      .AstDeref => |*der| {
        if (der.narrowed) |nrw| nrw.typ = typ
        else der.typ = typ;
      },
      .AstVar => |*vr| vr.typ = typ,
      .AstCast => |*cst| cst.typn.typ = typ,
      .AstCall => |*call| call.typ = typ,
      .AstOrElse => |*oe| oe.typ = typ,
      else => unreachable,
    }
  }

  pub fn toTypeNode(self: *@This(), al: std.mem.Allocator) *@This() {
    var tyn: TypeNode = undefined;
    switch (self.*) {
      .AstNumber => |num| {
        tyn = TypeNode.init(Type.newConstant(.TyNumber, num.token.value).box(al), num.token);
      },
      .AstString => |str| {
        tyn = TypeNode.init(Type.newConstant(.TyString, str.token.value).box(al), str.token);
      },
      .AstBool => |bol| {
        tyn = TypeNode.init(Type.newConstant(.TyBool, bol.token.value).box(al), bol.token);
      },
      .AstNil => |nil| {
        tyn = TypeNode.init(Type.newConcrete(.TyNil).box(al), nil.token);
      },
      else => unreachable,
    }
    var node = util.alloc(AstNode, al);
    node.* = .{.AstNType = tyn};
    return node;
  }

  pub fn toMatchCondition(self: *@This(), al: std.mem.Allocator) *@This() {
    var node = AstNode.create(al);
    node.* = .{.AstMCondition = MatchConditionNode.init(self)};
    return node;
  }

  pub fn getToken(self: *@This()) Token {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| lit.token,
      .AstBinary, .AstAssign => |bin| bin.op.token,
      .AstUnary => |una| una.op.token,
      .AstExprStmt => |stmt| stmt.expr.getToken(),
      .AstVarDecl => |decl| decl.ident.token,
      .AstVar => |id| id.token,
      .AstNType => |typn| typn.token,
      .AstAlias => |ali| ali.alias.token,
      .AstCast => |cst| cst.typn.token,
      .AstSubscript => |sub| sub.index.getToken(),
      .AstDeref => |der| der.token,
      .AstCondition => |cnd| cnd.cond.getToken(),
      .AstMCondition => |nd| nd.tst.getToken(),
      .AstControl => |ct| ct.token,
      .AstIf => |ifn| ifn.cond.getToken(),
      .AstElif => |elif| elif.cond.getToken(),
      .AstWhile => |whi| whi.cond.getToken(),
      .AstRet => |ret| ret.token,
      .AstCall => |call| call.expr.getToken(),
      .AstError => |er| er.expr.getToken(),
      .AstOrElse => |oe| oe.ok.getToken(),
      .AstEmpty => |emp| emp.token,
      .AstDotAccess => |*dot| dot.lhs.getToken(),
      .AstClass => |*cls| cls.name.token,
      .AstLblArg => |*arg| arg.label,
      .AstMatch => |*match| match.expr.getToken(),
      .AstFun => |*fun| {
        if (fun.name) |name| {
          return name.token;
        }
        if (fun.params.isNotEmpty()) {
          return fun.params.itemAt(0).ident.token;
        }
        if (fun.body.block().nodes.isNotEmpty()) {
          return fun.body.block().nodes.itemAt(0).getToken();
        }
        // std.debug.print("Could not obtain token from node: {}", .{self});
        return Token.getDefault();
      },
      else => {
        switch (self.*) {
          .AstList, .AstTuple => |*col| {
            if (col.elems.isNotEmpty()) {
              return col.elems.itemAt(0).getToken();
            }
          },
          .AstMap => |*map| {
            if (map.pairs.isNotEmpty()) {
              return map.pairs.itemAt(0).key.getToken();
            }
          },
          .AstBlock => |*blk| {
            if (blk.nodes.isNotEmpty()) {
              return blk.nodes.itemAt(0).getToken();
            }
          },
          .AstProgram => |*prog| {
            if (prog.decls.isNotEmpty()) {
              return prog.decls.itemAt(0).getToken();
            }
          },
          else => {}
        }
        std.log.debug("Could not obtain token from node: {}.\nUsing default", .{self});
        return Token.getDefault();
      },
    };
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *@This() {
    return switch (self.*) {
      .AstAlias, .AstFail, .AstEmpty, .AstScope => self,
      .AstCondition, .AstMCondition, .AstSimpleIf, .AstProgram => unreachable,
      .AstBinary, .AstAssign => |*bin| bin.clone(self, al),
      .AstList, .AstTuple => |*lst| lst.clone(self, al),
      .AstNumber, .AstString, .AstBool, .AstNil => |*lit| lit.clone(self, al),
      inline else => |*nd| nd.clone(al),
    };
  }

  pub fn render(self: *@This(), depth: usize, al: std.mem.Allocator) ![]const u8 {
    return switch (self.*) {
      inline else => |*nd| try nd.render(depth, al),
    };
  }

  pub inline fn create(al: std.mem.Allocator) *AstNode {
    return util.alloc(AstNode, al);
  }

  pub fn box(self: @This(), al: std.mem.Allocator) *@This() {
    return util.box(AstNode, self, al);
  }
};
