const std = @import("std");
const lex = @import("lex.zig");
const NType = @import("types.zig").NType;

const Token = lex.Token;
const OpType = lex.OpType;
pub const AstNodeList = std.ArrayList(*AstNode);

// ast node types
pub const AstType = enum {
  AstNum,
  AstStr,
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
  AstProgram,
};

// ast nodes
pub const LiteralNode = struct {
  token: Token,
  value: f64,
  line: usize,

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
  op: OpType,
  line: usize,

  pub fn init(left: *AstNode, right: *AstNode, op: OpType, line: usize) @This() {
    return @This() {
      .left = left,
      .right = right,
      .op = op,
      .line = line,
    };
  }
};

pub const UnaryNode = struct {
  expr: *AstNode,
  op: OpType,
  line: usize,

  pub fn init(expr: *AstNode, op: OpType, line: usize) @This() {
    return @This() {.expr = expr, .op = op, .line = line,};
  }
};

pub const ListNode = struct {
  elems: AstNodeList,
  line: usize,

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.elems = AstNodeList.init(allocator), .line = line,};
  }
};

pub const MapNode = struct {
  pairs: std.ArrayList(Pair),
  line: usize,

  pub const Pair = struct {key: *AstNode, value: *AstNode};

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.pairs = std.ArrayList(Pair).init(allocator), .line = line};
  }
};

pub const VarNode = struct {
  token: lex.Token,
  typn: ?*TypeNode,
  line: usize,

  pub fn init(token: Token) @This() {
    return @This() {
      .token = token,
      .line = token.line,
      .typn = null,
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

// TODO: refactor to BlockNode if no other useful info needs to be added.
pub const ProgramNode = struct {
  decls: AstNodeList,
  line: usize,

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.decls = AstNodeList.init(allocator), .line = line};
  }
};

pub const AstNode = union(AstType) {
  AstNum: LiteralNode,
  AstStr: LiteralNode,
  AstBool: LiteralNode,
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
  AstProgram: ProgramNode,

  pub fn line(self: *@This()) usize {
    return switch (self.*) {
      .AstNum, .AstStr, .AstBool => |lit| lit.line,
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
      .AstProgram => |prog| prog.line,
    };
  }

  pub fn isNum(self: *@This()) bool {
    return switch (self.*) {
      .AstNum => true,
      else => false,
    };
  }

  pub fn isConst(self: *@This()) bool {
    // for now, only numbers and booleans are recognized as consts
    return switch (self.*) {
      .AstNum, .AstBool, => true,
      else => false,
    };
  }
};
