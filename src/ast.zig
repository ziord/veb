const std = @import("std");
const lex = @import("lex.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");
const Type = types.Type;

const OpType = lex.OpType;
pub const Token = lex.Token;
pub const AstNodeList = ds.ArrayList(*AstNode);

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
};

pub const SubscriptNode = struct {
  expr: *AstNode,
  index: *AstNode,
  narrowed: ?VarNode = null,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, index: *AstNode) @This() {
    return @This() { .expr = expr, .index = index};
  }

  pub inline fn line(self: *@This()) usize {
    return self.index.getToken().line;
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
};

pub const ListNode = struct {
  elems: AstNodeList,
  token: Token,
  typ: ?*Type = null,

  pub fn init(allocator: std.mem.Allocator, token: Token) @This() {
    return @This() {
      .elems = AstNodeList.init(allocator),
      .token = token
    };
  }

  pub inline fn line(self: *@This()) usize {
    return self.token.line;
  }
};

pub const MapNode = struct {
  pairs: ds.ArrayList(Pair),
  token: Token,
  typ: ?*Type = null,

  pub const Pair = struct {key: *AstNode, value: *AstNode};

  pub fn init(allocator: std.mem.Allocator, token: Token) @This() {
    return @This() {
      .pairs = ds.ArrayList(Pair).init(allocator),
      .token = token
    };
  }

  pub inline fn line(self: *@This()) usize {
    return self.token.line;
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
};

pub const ExprStmtNode = struct {
  expr: *AstNode,
  line: usize,

  pub fn init(expr: *AstNode, line: usize) @This() {
    return @This() {.expr = expr, .line = line};
  }
};

pub const VarDeclNode = struct {
  ident: *VarNode,
  value: *AstNode,

  pub fn init(ident: *VarNode, value: *AstNode) @This() {
    return @This() {.ident = ident, .value = value};
  }

  pub inline fn line(self: *@This()) usize {
    return self.ident.line();
  }
};

pub const BlockNode = struct {
  nodes: AstNodeList,
  line: usize,

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.nodes = AstNodeList.init(allocator), .line = line};
  }

  pub fn newEmptyBlock(line: usize, alloc: std.mem.Allocator) *AstNode {
    var block = util.alloc(AstNode, alloc);
    block.* = .{.AstBlock = BlockNode.init(alloc, line)};
    return block;
  }

  pub fn getLast(self: *BlockNode) ?*AstNode {
    if (self.nodes.len() > 0) return self.nodes.getLast();
    return null;
  }
};

pub const TypeNode = struct {
  typ: Type,
  token: Token,
  /// track whether this type was created in an alias or annotation context
  from_alias_or_annotation: bool = false,

  pub fn init(typ: Type, token: Token) @This() {
    return @This() {.typ = typ, .token = token};
  }
};

pub const AliasNode = struct {
  token: Token, // token for 'type'
  alias: *TypeNode,
  aliasee: *TypeNode,
  typ: *Type, // alias and aliasee is set in `typ`

  pub fn init(typ_token: Token, alias: *TypeNode, aliasee: *TypeNode) @This() {
    aliasee.typ.alias = &alias.typ;
    alias.from_alias_or_annotation = true;
    aliasee.from_alias_or_annotation = true;
    return @This() {.alias = alias, .aliasee = aliasee, .token = typ_token, .typ = &alias.typ};
  }
};

/// null dereference: expr.?
pub const DerefNode = struct {
  token: Token,
  expr: *AstNode,
  narrowed: ?VarNode = null,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, token: Token) @This() {
    return @This() {.expr = expr, .token = token};
  }
};

pub const ConditionNode = struct {
  cond: *AstNode,

  pub fn init(cond: *AstNode) @This() {
    return @This() {.cond = cond};
  }
};

pub const EmptyNode = struct {
  token: Token,

  pub fn init(token: Token) @This() {
    return @This() {.token = token};
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
      BlockNode.newEmptyBlock(self.cond.getToken().line, alloc),
    );
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
};

pub const SimpleIfNode = struct {
  cond: *AstNode,
  then: *AstNode,
  els: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode, els: *AstNode) @This() {
    return @This() {.cond = cond, .then = then, .els = els};
  }
};

pub const WhileNode = struct {
  cond: *AstNode,
  then: *AstNode,

  pub fn init(cond: *AstNode, then: *AstNode) @This() {
    return @This() {.cond = cond, .then = then};
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
};

// TODO: refactor to BlockNode if no other useful info needs to be added.
pub const ProgramNode = struct {
  decls: AstNodeList,
  line: usize,

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.decls = AstNodeList.init(allocator), .line = line};
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

  pub fn getNarrowed(self: *@This()) ?VarNode {
    return switch (self.*) {
      .AstVar => |vr| vr,
      .AstSubscript => |*sub| sub.narrowed,
      .AstDeref => |*der| der.narrowed,
      else => null,
    };
  }

  pub fn getType(self: *@This()) ?*Type {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| lit.typ,
      .AstBinary => |bin| bin.typ,
      .AstUnary => |una| una.typ,
      .AstList, .AstTuple => |col| col.typ,
      .AstMap => |map| map.typ,
      .AstExprStmt => |stmt| stmt.expr.getType(),
      .AstVarDecl => |decl| decl.ident.typ,
      .AstVar => |id| id.typ,
      .AstAssign => |asi| asi.typ,
      .AstNType => |*typn| &typn.typ,
      .AstAlias => |ali| ali.typ,
      .AstCast => |*cst| &cst.typn.typ,
      .AstSubscript => |sub| if (sub.narrowed) |nrw| nrw.typ else sub.typ,
      .AstDeref => |der| if (der.narrowed) |nrw| nrw.typ else der.typ,
      .AstCondition => |cnd| cnd.cond.getType(),
      .AstBlock, .AstIf, .AstElif,
      .AstWhile, .AstControl => null,
      else => unreachable,
    };
  }

  pub fn getToken(self: *@This()) Token {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| lit.token,
      .AstBinary, .AstAssign => |bin| bin.op.token,
      .AstUnary => |una| una.op.token,
      .AstList, .AstTuple => |col| col.token,
      .AstMap => |map| map.token,
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
      .AstBlock => |*blk| {
        if (blk.nodes.len() > 0) {
          return blk.nodes.items()[0].getToken();
        } else {
          util.error_("Could not obtain token from node: {}", .{self});
        }
      },
      else => unreachable,
    };
  }
};
