const std = @import("std");
const lex = @import("lex.zig");

const Token = lex.Token;
const OpType = lex.OpType;
pub const AstNodeList = std.ArrayList(*AstNode);

// ast node types
pub const AstType = enum {
  AstNum,
  AstStr,
  AstBinary,
  AstUnary,
  AstExprStmt,
};

// ast nodes
pub const NumberNode = struct {
  token: Token,
  value: f64,
  line: usize,
  reg: u32,

  pub fn init(value: f64, token: Token) @This() {
    return @This() {
      .value = value, 
      .token = token, 
      .line = token.line,
      .reg = undefined,
    };
  }
};

pub const StringNode = struct {
  token: Token,
  reg: u32,

  pub fn init(token: Token) @This() {
    return @This() {
      .token = token,
      .reg = undefined,
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
    return @This() {.expr = expr, .op = op, .line = line};
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
  AstNum: NumberNode,
  AstStr: StringNode,
  AstBinary: BinaryNode,
  AstUnary: UnaryNode,
  AstExprStmt: ExprStmtNode,

  pub fn line(self: *@This()) usize {
    return switch (self.*) {
      .AstNum => |num| num.line,
      .AstStr => |str| str.token.line,
      .AstBinary => |bin| bin.line,
      .AstUnary => |una| una.line,
      .AstExprStmt => |expr| expr.line,
    };
  }

  pub fn reg(self: *@This()) u32 {
    return switch (self.*) {
      .AstNum => |num| num.reg,
      .AstStr => |str| str.reg,
      .AstBinary => |bin| bin.reg,
      .AstUnary => |una| una.reg,
      .AstExprStmt => |expr| expr.reg,
    };
  }

  pub fn isNum(self: *@This()) bool {
    return switch (self.*) {
      .AstNum => true,
      else => false,
    };
  }
};
