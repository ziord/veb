const std = @import("std");
const value = @import("value.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const util = @import("util.zig");
const NovaAllocator = @import("allocator.zig");
const OpCode = @import("opcode.zig").OpCode;
const VM = @import("vm.zig").VM;

const Code = value.Code;
const Node = ast.AstNode;

pub const Compiler = struct {
  code: *Code,
  node: *Node,
  filename: []const u8,
  registers: std.ArrayList(u32),
  allocator: *NovaAllocator,
  vm: *VM,
  
  const Self = @This();

  fn initRegisters(allocator: std.mem.Allocator) std.ArrayList(u32) {
    var list = std.ArrayList(u32).init(allocator);
    var i: u32 = value.MAX_REGISTERS - 1;
    while (i > 0): (i -= 1) util.append(u32, &list, i);
    util.append(u32, &list, 0);
    return list;
  }

  pub fn init(node: *Node, filename: []const u8, vm: *VM, code: *Code, allocator: *NovaAllocator) Self {
    var self = Self {
      .code = code,
      .node = node, 
      .filename = filename,
      .allocator = allocator,
      .registers = initRegisters(allocator.getArenaAllocator()),
      .vm = vm,
    };
    return self;
  }

  fn compileError(self: *Self, msg: []const u8) noreturn {
    _ = self;
    std.debug.print("CompileError: {s}\n", .{msg});
    std.os.exit(1);
  }

  fn getScratchReg(self: *Self) u32 {
    // todo: augment
    if (self.registers.items.len > 0) {
      return self.registers.pop();
    }
    self.compileError("registers exhausted");
  }

  fn freeScratchReg(self: *Self, reg: u32) void {
    util.append(u32, &self.registers, reg);
  }

  fn lastLine(self: *Self) u32 {
    return self.code.lines.items[self.code.lines.items.len - 1];
  }

  fn cConst(self: *Self, reg: u32, val: value.Value, line: usize) void {
    // load rx, memidx
    const memidx = self.code.writeValue(val);
    self.code.write2ArgsInst(.Load, reg, memidx, @intCast(u32, line));
  }

  fn cNum(self: *Self, node: *ast.LiteralNode) void {
    // load rx, memidx
    node.reg = self.getScratchReg();
    self.cConst(node.reg, value.numberVal(node.value), node.line);
  }

  fn cBool(self: *Self, node: *ast.LiteralNode) void {
    // load rx, memidx
    node.reg = self.getScratchReg();
    self.cConst(node.reg, value.boolVal(node.token.is(.TkTrue)), node.line);
  }

  fn cStr(self: *Self, node: *ast.LiteralNode) void {
    node.reg = self.getScratchReg();
    self.cConst(
      node.reg,
      value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, node.token.is_alloc)),
      node.line
    );
  }

  fn cUnary(self: *Self, node: *ast.UnaryNode) void {
    const inst_op = node.op.toInstOp();
    const isConst = node.expr.isConst();
    const rx = self.getScratchReg();
    // check that we don't exceed the 18 bits of rk/bx (+ 1 for expr)
    if ((self.code.values.items.len + value.MAX_REGISTERS + 1) < value.Code._18bits and isConst) {
      // for now, only numbers and booleans are recognized as consts
      var rk: u32 = undefined;
      if (node.expr.isNum()) {
        rk = self.code.storeConst(value.numberVal(node.expr.AstNum.value));
      } else {
        rk = self.code.storeConst(value.boolVal(node.expr.AstBool.token.is(.TkTrue)));
      }
      self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line));
      node.reg = rx;
      return;
    }
    self.c(node.expr);
    const rk = node.expr.reg();
    self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line));
    self.freeScratchReg(rk);
    node.reg = rx;
  }

  inline fn cCmp(self: *Self, node: *ast.BinaryNode) void {
    if (!node.op.isCmpOp()) return;
    self.code.writeNoArgInst(@intToEnum(OpCode, @enumToInt(node.op)), node.line);
  }

  inline fn cLgc(self: *Self, node: *ast.BinaryNode) void {
    self.c(node.left);
    const rx = node.left.reg();
    const end_jmp = self.code.write2ArgsJmp(node.op.toInstOp(), rx, self.lastLine());
    self.freeScratchReg(rx);
    self.c(node.right);
    self.code.patch2ArgsJmp(end_jmp);
    node.reg = node.right.reg();
  }

  fn cBinary(self: *Self, node: *ast.BinaryNode) void {
    // handle and | or
    if (node.op.isLgcOp()) return self.cLgc(node);
    // handle other binary ops
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
        self.cCmp(node);
        node.reg = rx;
        return;
      } else if (lhsIsNum) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value));
        self.c(node.right);
        const rk2 = node.right.reg();
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line));
        self.freeScratchReg(rk2);
        self.cCmp(node);
        node.reg = rx;
        return;
      } else if (rhsIsNum) {
        self.c(node.left);
        const rk1 = node.left.reg();
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value));
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line));
        self.freeScratchReg(rk1);
        self.cCmp(node);
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
    self.cCmp(node);
    node.reg = rx;
  }

  fn cExprStmt(self: *Self, node: *ast.ExprStmtNode) void {
    self.c(node.expr);
  }

  fn c(self: *Self, node: *Node) void {
    switch (node.*) {
      .AstNum => |*nd| self.cNum(nd),
      .AstStr => |*nd| self.cStr(nd),
      .AstBool => |*nd| self.cBool(nd),
      .AstUnary => |*nd| self.cUnary(nd),
      .AstBinary => |*nd| self.cBinary(nd),
      .AstExprStmt => |*nd| self.cExprStmt(nd),
    }
  }

  pub fn compile(self: *Self) void {
    self.c(self.node);
    const last_line = self.code.lines.items[self.code.lines.items.len - 1];
    self.code.writeNoArgInst(.Ret, last_line);
    // release memory associated with the arena, since we're done with it at this point.
    self.allocator.deinitArena();
  }

};
