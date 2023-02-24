const std = @import("std");
const lex = @import("lex.zig");

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
};

// ast nodes
pub const LiteralNode = struct {
  token: Token,
  value: f64,
  line: usize,
  reg: u32,

  pub fn init(token: Token) @This() {
    return @This() {
      .token = token,
      .value = undefined,
      .reg = undefined,
      .line = token.line,
    };
  }
};

pub const BinaryNode = struct {
  left: *AstNode,
  right: *AstNode,
  op: OpType,
  line: usize,
  reg: u32,

  pub fn init(left: *AstNode, right: *AstNode, op: OpType, line: usize) @This() {
    return @This() {
      .left = left,
      .right = right,
      .op = op,
      .line = line,
      .reg = undefined,
    };
  }
};

pub const UnaryNode = struct {
  expr: *AstNode,
  op: OpType,
  line: usize,
  reg: u32,

  pub fn init(expr: *AstNode, op: OpType, line: usize) @This() {
    return @This() {.expr = expr, .op = op, .line = line, .reg = undefined};
  }
};

pub const ListNode = struct {
  elems: AstNodeList,
  line: usize,
  reg: u32,

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.elems = AstNodeList.init(allocator), .line = line, .reg = undefined};
  }
};

pub const MapNode = struct {
  pairs: std.ArrayList(Pair),
  line: usize,
  reg: u32,

  pub const Pair = struct {key: *AstNode, value: *AstNode};

  pub fn init(allocator: std.mem.Allocator, line: usize) @This() {
    return @This() {.pairs = std.ArrayList(Pair).init(allocator), .line = line, .reg = undefined};
  }
};

pub const VarNode = struct {
  token: lex.Token,
  line: usize,
  type: IType,
  reg: u32,

  pub fn init(token: Token) @This() {
    return @This() {
      .token = token,
      .line = token.line,
      .type = undefined,
      .reg = undefined,
    };
  }
};

pub const ExprStmtNode = struct {
  expr: *AstNode,
  line: usize,
  reg: u32,

  pub fn init(expr: *AstNode, line: usize) @This() {
    return @This() {.expr = expr, .line = line, .reg = undefined,};
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

  pub fn line(self: *@This()) usize {
    return switch (self.*) {
      .AstNum, .AstStr, .AstBool => |lit| lit.line,
      .AstBinary => |bin| bin.line,
      .AstUnary => |una| una.line,
      .AstList => |lst| lst.line,
      .AstMap => |map| map.line,
      .AstExprStmt => |expr| expr.line,
    };
  }

  pub fn reg(self: *@This()) u32 {
    return switch (self.*) {
      .AstNum, .AstStr, .AstBool => |lit| lit.reg,
      .AstBinary => |bin| bin.reg,
      .AstUnary => |una| una.reg,
      .AstList => |lst| lst.reg,
      .AstMap => |map| map.reg,
      .AstExprStmt => |expr| expr.reg,
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

pub const ITypeKind = enum (u8) {
  TyBool,
  TyNumber,
  TyString,
  TyList,
  TyMap,
};

pub const IType = struct {
  kind: ITypeKind,
  list: ? struct {
    len: usize,
    base: IType,
  },

  pub fn init() @This() {
    // TODO
  }
};
