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
  AstExprStmt: ExprStmtNode,

  pub fn line(self: *@This()) usize {
    return switch (self.*) {
      .AstNum, .AstStr, .AstBool => |lit| lit.line,
      .AstBinary => |bin| bin.line,
      .AstUnary => |una| una.line,
      .AstExprStmt => |expr| expr.line,
    };
  }

  pub fn reg(self: *@This()) u32 {
    return switch (self.*) {
      .AstNum, .AstStr, .AstBool => |lit| lit.reg,
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

  pub fn isConst(self: *@This()) bool {
    // for now, only numbers and booleans are recognized as consts
    return switch (self.*) {
      .AstNum, .AstBool, => true,
      else => false,
    };
  }
};
