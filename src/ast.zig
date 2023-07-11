const std = @import("std");
const lex = @import("lex.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");
const Type = types.Type;

const OpType = lex.OpType;
pub const Token = lex.Token;
pub const AstNodeList = ds.ArrayList(*AstNode);
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
  AstElif,
  AstSimpleIf,
  AstWhile,
  AstControl,
  AstFun,
  AstRet,
  AstCall,
  AstError,
  AstOrElse,
  AstProgram,
};

fn cloneNodeList(ori: *AstNodeList, al: std.mem.Allocator) AstNodeList {
  var new = AstNodeList.init(al);
  new.ensureTotalCapacity(ori.capacity());
  for (ori.items()) |itm| {
    new.append(itm.clone(al));
  }
  return new;
}

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
};

pub const BinaryNode = struct {
  left: *AstNode,
  right: *AstNode,
  op: lex.Optr,
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
    var new = util.alloc(AstNode, al);
    switch (node.*) {
      .AstBinary => new.* = .{.AstBinary = bin},
      .AstAssign => new.* = .{.AstAssign = bin},
      else => unreachable,
    }
    return new;
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
};

pub const ListNode = struct {
  elems: AstNodeList,
  typ: ?*Type = null,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.elems = AstNodeList.init(allocator)};
  }

  pub fn clone(self: *@This(), node: *AstNode, al: std.mem.Allocator) *AstNode {
    var list = @This() {.elems = cloneNodeList(&self.elems, al)};
    var new = util.alloc(AstNode, al);
    switch (node.*) {
      .AstList => new.* = .{.AstList = list},
      .AstTuple => new.* = .{.AstTuple = list},
      else => unreachable,
    }
    return new;
  }
};

pub const MapNode = struct {
  pairs: ds.ArrayList(Pair),
  typ: ?*Type = null,

  pub const Pair = struct {key: *AstNode, value: *AstNode};

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.pairs = ds.ArrayList(Pair).init(allocator)};
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
};

pub const ExprStmtNode = struct {
  expr: *AstNode,

  pub fn init(expr: *AstNode) @This() {
    return @This() {.expr = expr};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var es = ExprStmtNode.init(self.expr.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstExprStmt = es};
    return new;
  }
};

pub const VarDeclNode = struct {
  ident: *VarNode,
  value: *AstNode,
  is_param: bool = false,

  pub fn init(ident: *VarNode, value: *AstNode, is_param: bool) @This() {
    return @This() {.ident = ident, .value = value, .is_param = is_param};
  }

  pub inline fn line(self: *@This()) usize {
    return self.ident.line();
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var value: *AstNode = if (self.is_param) self.value else self.value.clone(al);
    var vn = VarDeclNode.init(&self.ident.clone(al).AstVar, value, self.is_param);
    var new = util.alloc(AstNode, al);
    new.* = .{.AstVarDecl = vn};
    return new;
  }
};

pub const BlockNode = struct {
  nodes: AstNodeList,
  /// whether this block is from a branching entry
  cond: ?*AstNode,
  /// whether this block has been successfully typechecked
  checked: bool = false,

  pub fn init(allocator: std.mem.Allocator, cond: ?*AstNode) @This() {
    return @This() {.nodes = AstNodeList.init(allocator), .cond = cond};
  }

  pub fn newEmptyBlock(alloc: std.mem.Allocator, cond: ?*AstNode) *AstNode {
    var block = util.alloc(AstNode, alloc);
    block.* = .{.AstBlock = BlockNode.init(alloc, cond)};
    return block;
  }

  pub fn getLast(self: *BlockNode) ?*AstNode {
    if (self.nodes.len() > 0) return self.nodes.getLast();
    return null;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var bl = @This() {.nodes = cloneNodeList(&self.nodes, al), .cond = self.cond};
    var block = util.alloc(AstNode, al);
    block.* = .{.AstBlock = bl};
    return block;
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
};

pub const ElifNode = struct {
  cond: *AstNode,
  then: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode) @This() {
    return @This() {.cond = cond, .then = then};
  }

  pub fn toIf(self: *ElifNode, alloc: std.mem.Allocator) IfNode {
    var list = AstNodeList.init(alloc);
    return IfNode.init(
      self.cond, self.then, list,
      BlockNode.newEmptyBlock(alloc, self.cond),
    );
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var el = ElifNode.init(self.cond.clone(al), self.then.clone(al));
    var new = util.alloc(AstNode, al);
    new.* = .{.AstElif = el};
    return new;
  }
};

pub const IfNode = struct {
  cond: *AstNode,
  then: *AstNode,
  elifs: AstNodeList,
  els: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode, elifs: AstNodeList, els: *AstNode) @This() {
    return @This() {.cond = cond, .then = then, .elifs = elifs, .els = els};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var el = IfNode.init(
      self.cond.clone(al),
      self.then.clone(al),
      cloneNodeList(&self.elifs, al),
      self.els.clone(al)
    );
    var new = util.alloc(AstNode, al);
    new.* = .{.AstIf = el};
    return new;
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
};

pub const CallNode = struct {
  expr: *AstNode,
  targs: ?*AstNodeList = null,
  args: AstNodeList,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, args: AstNodeList, targs: ?*AstNodeList) @This() {
    return @This() {.expr = expr, .args = args, .targs = targs};
  }

  pub inline fn isGeneric(self: *@This()) bool {
    return self.targs != null;
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    var targs: ?*AstNodeList = null;
    if (self.targs) |list| {
      targs = util.box(AstNodeList, cloneNodeList(list, al), al);
    }
    var call = CallNode.init(self.expr.clone(al), cloneNodeList(&self.args, al), targs);
    var new = util.alloc(AstNode, al);
    new.* = .{.AstCall = call};
    return new;
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
};

pub const FunNode = struct {
  params: VarDeclList,
  tparams: ?*types.TypeList = null,
  body: *AstNode,
  name: ?*VarNode,
  ret: ?*AstNode = null,
  is_builtin: bool = false,

  pub fn init(params: VarDeclList, body: *AstNode, name: ?*VarNode, ret: ?*AstNode, tparams: ?*types.TypeList) @This() {
    return @This() {.params = params, .body = body, .name = name, .ret = ret, .tparams = tparams};
  }

  pub inline fn isGeneric(self: @This()) bool {
    return self.tparams != null;
  }

  pub inline fn isAnonymous(self: @This()) bool {
    return self.name == null;
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
    var fun = FunNode.init(params, self.body.clone(al), self.name, ret, self.tparams);
    fun.is_builtin = self.is_builtin;
    var new = util.alloc(AstNode, al);
    new.* = .{.AstFun = fun};
    return new;
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
};

// TODO: refactor to BlockNode if no other useful info needs to be added.
pub const ProgramNode = struct {
  decls: AstNodeList,

  pub fn init(allocator: std.mem.Allocator) @This() {
    return @This() {.decls = AstNodeList.init(allocator)};
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *AstNode {
    _ = al;
    _ = self;
    unreachable;
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
  AstWhile: WhileNode,
  AstControl: ControlNode,
  AstFun: FunNode,
  AstRet: RetNode,
  AstCall: CallNode,
  AstError: ErrorNode,
  AstOrElse: OrElseNode,
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

  pub inline fn isTypeAlias(self: *@This()) bool {
    return switch (self.*) {
      .AstAlias => true,
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

  pub inline fn isFun(self: *@This()) bool {
    return switch (self.*) {
      .AstFun => true,
      else => false,
    };
  }

  pub inline fn isRet(self: *@This()) bool {
    return switch (self.*) {
      .AstRet => true,
      else => false,
    };
  }

  pub fn getNarrowed(self: *@This()) ?*VarNode {
    return switch (self.*) {
      .AstVar => |*vr| vr,
      .AstSubscript => |*sub| sub.narrowed,
      .AstDeref => |*der| der.narrowed,
      else => null,
    };
  }

  pub inline fn eql(self: *@This(), other: *@This()) bool {
    return self == other;
  }

  pub fn getType(self: *@This()) ?*Type {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| lit.typ,
      .AstBinary, .AstAssign => |bin| bin.typ,
      .AstUnary => |una| una.typ,
      .AstList, .AstTuple => |col| col.typ,
      .AstMap => |map| map.typ,
      .AstExprStmt => |stmt| stmt.expr.getType(),
      .AstVarDecl => |decl| decl.ident.typ,
      .AstVar => |id| id.typ,
      .AstNType => |*typn| typn.typ,
      .AstAlias => |ali| ali.typ,
      .AstCast => |*cst| cst.typn.typ,
      .AstSubscript => |sub| if (sub.narrowed) |nrw| nrw.typ else sub.typ,
      .AstDeref => |der| if (der.narrowed) |nrw| nrw.typ else der.typ,
      .AstCondition => |cnd| cnd.cond.getType(),
      .AstFun => |fun| if (fun.ret) |ret| ret.AstNType.typ else null,
      .AstRet => |ret| ret.typ,
      .AstCall => |call| call.typ,
      .AstError => |er| er.typ,
      .AstOrElse => |oe| oe.typ,
      .AstBlock, .AstIf, .AstElif,
      .AstWhile, .AstControl => null,
      else => unreachable,
    };
  }

  pub fn setType(self: *@This(), typ: *Type) void {
    switch (self.*) {
      .AstUnary => |*una| una.typ = typ,
      .AstVar => |*id| id.typ = typ,
      .AstCast => |*cst| cst.typn.typ = typ,
      .AstSubscript => |*sub| {
        if (sub.narrowed) |nrw| nrw.typ = typ
        else sub.typ = typ;
      },
      .AstDeref => |*der| {
        if (der.narrowed) |nrw| nrw.typ = typ
        else der.typ = typ;
      },
      .AstCall => |*call| call.typ = typ,
      else => {
        std.log.debug("Attempt to set type on node: {}\n", .{self});
      },
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
        tyn = TypeNode.init(Type.newConcrete(.TyNil, nil.token.value).box(al), nil.token);
      },
      else => unreachable,
    }
    var node = util.alloc(AstNode, al);
    node.* = .{.AstNType = tyn};
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
      .AstControl => |ct| ct.token,
      .AstIf => |ifn| ifn.cond.getToken(),
      .AstElif => |elif| elif.cond.getToken(),
      .AstWhile => |whi| whi.cond.getToken(),
      .AstRet => |ret| ret.token,
      .AstCall => |call| call.expr.getToken(),
      .AstError => |er| er.expr.getToken(),
      .AstOrElse => |oe| oe.ok.getToken(),
      .AstEmpty => |emp| emp.token,
      .AstFun => |*fun| {
        if (fun.name) |name| {
          return name.token;
        }
        if (fun.params.len() > 0) {
          return fun.params.itemAt(0).ident.token;
        }
        if (fun.body.AstBlock.nodes.len() > 0) {
          return fun.body.AstBlock.nodes.itemAt(0).getToken();
        }
        // std.debug.print("Could not obtain token from node: {}", .{self});
        return Token.getDefault();
      },
      else => {
        switch (self.*) {
          .AstList, .AstTuple => |*col| {
            if (col.elems.len() > 0) {
              return col.elems.itemAt(0).getToken();
            }
          },
          .AstMap => |*map| {
            if (map.pairs.len() > 0) {
              return map.pairs.itemAt(0).key.getToken();
            }
          },
          .AstBlock => |*blk| {
            if (blk.nodes.len() > 0) {
              return blk.nodes.itemAt(0).getToken();
            }
          },
          .AstProgram => |*prog| {
            if (prog.decls.len() > 0) {
              return prog.decls.itemAt(0).getToken();
            }
          },
          else => {}
        }
        // std.debug.print("Could not obtain token from node: {}", .{self});
        return Token.getDefault();
      },
    };
  }

  pub fn clone(self: *@This(), al: std.mem.Allocator) *@This() {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |*lit| lit.clone(self, al),
      .AstBinary, .AstAssign => |*bin| bin.clone(self, al),
      .AstUnary => |*una| una.clone(al),
      .AstSubscript => |*sub| sub.clone(al),
      .AstList, .AstTuple => |*lst| lst.clone(self, al),
      .AstVar => |*vr| vr.clone(al),
      .AstBlock => |*bl| bl.clone(al),
      .AstNType => |*tn| tn.clone(al),
      .AstAlias => self,
      .AstCast => |*cst| cst.clone(al),
      .AstDeref => |*der| der.clone(al),
      .AstCondition => unreachable,
      .AstWhile => |*wh| wh.clone(al),
      .AstMap => |*nd| nd.clone(al),
      .AstExprStmt => |*nd| nd.clone(al),
      .AstVarDecl => |*nd| nd.clone(al),
      .AstControl => |*ctr| ctr.clone(al),
      .AstEmpty => self,
      .AstIf => |*if_| if_.clone(al),
      .AstElif => |*elif| elif.clone(al),
      .AstSimpleIf => unreachable,
      .AstRet => |*ret| ret.clone(al),
      .AstCall => |*call| call.clone(al),
      .AstError => |*er| er.clone(al),
      .AstOrElse => |*oe| oe.clone(al),
      .AstFun => |*fun| fun.clone(al),
      .AstProgram => unreachable,
    };
  }
};
