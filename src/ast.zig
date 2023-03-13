const std = @import("std");
const lex = @import("lex.zig");
const NType = @import("types.zig").NType;

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
  AstNil,
  AstEmpty,
  AstProgram,
};

// ast nodes
pub const LiteralNode = struct {
  token: Token,
  value: f64,
  line: usize,
  typ: ?*NType = null,

  pub fn init(token: Token) @This() {
    return @This() {
      .token = token,
      .value = undefined,
      .line = token.line,
    };
  }
};

pub const BinaryNode = struct {
  left: *AstNode,
  right: *AstNode,
  op: lex.Optr,
  line: usize,
  typ: ?*NType = null,

  pub fn init(left: *AstNode, right: *AstNode, op: Token) @This() {
    return @This() {
      .left = left,
      .right = right,
      .op = lex.Optr.init(op),
      .line = op.line,
    };
  }
};

pub const UnaryNode = struct {
  expr: *AstNode,
  op: lex.Optr,
  line: usize,
  typ: ?*NType = null,

  pub fn init(expr: *AstNode, op: Token, line: usize) @This() {
    return @This() {
      .expr = expr,
      .op = lex.Optr.init(op),
      .line = line,
    };
  }
};

pub const ListNode = struct {
  elems: AstNodeList,
  line: usize,
  token: Token,
  typ: ?*NType = null,

  pub fn init(allocator: std.mem.Allocator, token: Token) @This() {
    return @This() {
      .elems = AstNodeList.init(allocator),
      .line = token.line,
      .token = token
    };
  }
};

pub const MapNode = struct {
  pairs: std.ArrayList(Pair),
  line: usize,
  token: Token,
  typ: ?*NType = null,

  pub const Pair = struct {key: *AstNode, value: *AstNode};

  pub fn init(allocator: std.mem.Allocator, token: Token) @This() {
    return @This() {
      .pairs = std.ArrayList(Pair).init(allocator),
      .line = token.line,
      .token = token
    };
  }
};

pub const VarNode = struct {
  token: lex.Token,
  typ: ?*NType,
  line: usize,

  pub fn init(token: Token) @This() {
    return @This() {
      .token = token,
      .line = token.line,
      .typ = null,
    };
  }
};

pub const ExprStmtNode = struct {
  expr: *AstNode,
  line: usize,

  pub fn init(expr: *AstNode, line: usize) @This() {
    return @This() {.expr = expr, .line = line,};
  }
};

pub const VarDeclNode = struct {
  ident: *VarNode,
  value: *AstNode,
  line: usize,

  pub fn init(ident: *VarNode, value: *AstNode) @This() {
    return @This() {
      .ident = ident,
      .value = value,
      .line = ident.line,
    };
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
  typ: NType,
  token: Token,

  pub fn init(typ: NType, token: Token) @This() {
    return @This() {.typ = typ, .token = token};
  }
};

pub const AliasNode = struct {
  token: Token, // token for 'type'
  alias: *TypeNode,
  aliasee: *TypeNode,
  typ: *NType, // alias and aliasee is set in `typ`

  pub fn init(typ_token: Token, alias: *TypeNode, aliasee: *TypeNode) @This() {
    var typ = &alias.typ;
    typ.aliasee = &aliasee.typ;
    aliasee.typ.alias = typ;
    return @This() {.alias = alias, .aliasee = aliasee, .token = typ_token, .typ = typ};
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
  line: usize,

  pub fn init(expr: *AstNode, typn: *TypeNode, token: Token) @This() {
    return @This() {
      .expr = expr,
      .typn = typn,
      .line = token.line,
      .token = token
    };
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
  AstEmpty: EmptyNode,
  AstProgram: ProgramNode,

  pub fn line(self: *@This()) usize {
    return switch (self.*) {
      .AstNumber, .AstString, .AstBool, .AstNil => |lit| lit.line,
      .AstBinary => |bin| bin.line,
      .AstUnary => |una| una.line,
      .AstList => |lst| lst.line,
      .AstMap => |map| map.line,
      .AstExprStmt => |expr| expr.line,
      .AstVarDecl => |decl| decl.line,
      .AstVar => |id| id.line,
      .AstAssign => |asi| asi.line,
      .AstBlock => |blk| blk.line,
      .AstNType => |typ| typ.token.line,
      .AstAlias => |ali| ali.token.line,
      .AstCast => |cst| cst.line,
      .AstEmpty => |emp| emp.token.line,
      .AstProgram => |prog| prog.line,
    };
  }

  pub fn isNum(self: *@This()) bool {
    return switch (self.*) {
      .AstNumber => true,
      else => false,
    };
  }

  pub fn isEmptyNode(self: *@This()) bool {
    return switch (self.*) {
      .AstEmpty => true,
      else => false,
    };
  }

  pub fn isNumOrBoolNode(self: *@This()) bool {
    return switch (self.*) {
      .AstNumber, .AstBool => true,
      else => false,
    };
  }

  pub fn getType(self: *@This()) ?*NType {
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
      else => unreachable,
    };
  }
};
