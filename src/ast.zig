const std = @import("std");
const lex = @import("lex.zig");
const types = @import("type.zig");
const Type = types.Type;

const Token = lex.Token;
const OpType = lex.OpType;
pub const AstNodeList = std.ArrayList(*AstNode);

// ast node types
pub const AstType = enum {
  AstNumber,
  AstString,
  AstBool,
  AstBinary,
  AstUnary,
  AstList,
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
  token: Token,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, index: *AstNode, token: Token) @This() {
    return @This() {
      .expr = expr,
      .index = index,
      .token = token,
    };
  }

  pub inline fn line(self: *@This()) usize {
    return self.token.line;
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
  pairs: std.ArrayList(Pair),
  token: Token,
  typ: ?*Type = null,

  pub const Pair = struct {key: *AstNode, value: *AstNode};

  pub fn init(allocator: std.mem.Allocator, token: Token) @This() {
    return @This() {
      .pairs = std.ArrayList(Pair).init(allocator),
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
};

pub const TypeNode = struct {
  typ: Type,
  token: Token,

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
    const info = types.AliasInfo.init(&alias.typ, &aliasee.typ);
    alias.typ.alias_info = info;
    aliasee.typ.alias_info = info;
    return @This() {.alias = alias, .aliasee = aliasee, .token = typ_token, .typ = &alias.typ};
  }
};

/// null dereference: expr.?
pub const DerefNode = struct {
  token: Token,
  expr: *AstNode,
  typ: ?*Type = null,

  pub fn init(expr: *AstNode, token: Token) @This() {
    return @This() {.expr = expr, .token = token};
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
  token: Token,

  pub fn init(expr: *AstNode, typn: *TypeNode, token: Token) @This() {
    return @This() {
      .expr = expr,
      .typn = typn,
      .token = token
    };
  }

  pub inline fn line(self: *@This()) usize {
    return self.token.line;
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
  AstProgram: ProgramNode,

  pub fn isComptimeConst(self: *@This()) bool {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => true,
      else => false,
    };
  }

  pub fn getType(self: *@This()) ?*Type {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| lit.typ,
      .AstBinary => |bin| bin.typ,
      .AstUnary => |una| una.typ,
      .AstList => |lst| lst.typ,
      .AstMap => |map| map.typ,
      .AstExprStmt => |stmt| stmt.expr.getType(),
      .AstVarDecl => |decl| decl.ident.typ,
      .AstVar => |id| id.typ,
      .AstAssign => |asi| asi.typ,
      .AstNType => |typn| @constCast(&typn.typ),
      .AstAlias => |ali| ali.typ,
      .AstCast => |cst| @constCast(&cst.typn.typ),
      .AstSubscript => |sub| sub.typ,
      .AstDeref => |der| der.typ,
      else => unreachable,
    };
  }
};
