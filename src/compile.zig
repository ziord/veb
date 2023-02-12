const std = @import("std");
const value = @import("value.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const util = @import("util.zig");
const OpCode = @import("opcode.zig").Opcode;

const Code = value.Code;
const Node = ast.AstNode;

pub const Compiler = struct {
  code: Code,
  node: *Node,
  filename: []const u8,
  registers: std.ArrayList(u32),
  
  const Self = @This();

  fn initRegisters(allocator: std.mem.Allocator) std.ArrayList(u32) {
    var list = std.ArrayList(u32).init(allocator);
    var i: u32 = value.MAX_REGISTERS - 1;
    while (i > 0): (i -= 1) util.append(u32, &list, i);
    util.append(u32, &list, 0);
    return list;
  }

  pub fn init(node: *Node, filename: []const u8, allocator: std.mem.Allocator) Self {
    var self = Self {
      .code = Code.init(allocator), 
      .node = node, 
      .filename = filename,
      .registers = initRegisters(allocator),
    };
    return self;
  }

  fn compileError(self: *Self, msg: []const u8) noreturn {
    _ = self;
    std.debug.print("CompileError: {s}\n", .{msg});
    std.os.exit(1);
  }

  fn getScratchReg(self: *Self) u32 {
    if (self.registers.items.len > 0) {
      return self.registers.pop();
    }
    self.compileError("registers exhausted");
  }

  fn freeScratchReg(self: *Self, reg: u32) void {
    util.append(u32, &self.registers, reg);
  }

  fn cNum(self: *Self, node: *ast.NumberNode) void {
    // load rx, memidx
    node.reg = self.getScratchReg();
    const memidx = self.code.writeValue(value.numberVal(node.value));
    self.code.write2ArgsInst(.Load, node.reg, memidx, @intCast(u32, node.line));
  }

  fn cStr(self: *Self, node: *ast.StringNode) void {
    _ = self;
    _ = node;
  }

  fn cUnary(self: *Self, node: *ast.UnaryNode) void {
    _ = self;
    _ = node;
  }

  fn cBinary(self: *Self, node: *ast.BinaryNode) void {
    const lhsIsNum = node.left.isNum();
    const rhsIsNum = node.right.isNum();
    const inst_op = node.op.toInstOp();
    const rx = self.getScratchReg();
    // check that we don't exceed the 9 bits of rk (+ 2 for lhs and rhs)
    if ((self.code.values.items.len + value.MAX_REGISTERS + 2) < value.Code._9bits) {
      if (lhsIsNum and rhsIsNum) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value));
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value));
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line));
        node.reg = rx;
        return;
      } else if (lhsIsNum) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value));
        self.c(node.right);
        const rk2 = node.right.reg();
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line));
        self.freeScratchReg(rk2);
        node.reg = rx;
        return;
      } else if (rhsIsNum) {
        self.c(node.left);
        const rk1 = node.left.reg();
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value));
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line));
        self.freeScratchReg(rk1);
        node.reg = rx;
        return;
      }
      // fallthrough
    }
    self.c(node.left);
    const rk1 = node.left.reg();
    self.c(node.right);
    const rk2 = node.right.reg();
    self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line));
    self.freeScratchReg(rk1);
    self.freeScratchReg(rk2);
    node.reg = rx;
  }

  fn cExprStmt(self: *Self, node: *ast.ExprStmtNode) void {
    self.c(node.expr);
  }

  fn c(self: *Self, node: *Node) void {
    switch (node.*) {
      .AstNum => |*nd| self.cNum(nd),
      .AstStr => |*nd| self.cStr(nd),
      .AstUnary => |*nd| self.cUnary(nd),
      .AstBinary => |*nd| self.cBinary(nd),
      .AstExprStmt => |*nd| self.cExprStmt(nd),
    }
  }

  pub fn compile(self: *Self) void {
    self.c(self.node);
    const last_line = self.code.lines.items[self.code.lines.items.len - 1];
    self.code.writeNoArgInst(.Ret, last_line);
  }

};
