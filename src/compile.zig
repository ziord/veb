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

const VRegister = struct {
  regs: [value.MAX_REGISTERS]Reg,
  reg: ?u8,

  const Reg = struct {
    val: u8, 
    free: bool,

    pub inline fn setFree(self: *@This(), free: bool) void {
      self.free = free;
    }
  };

  const Self = @This();

  pub fn init() Self {
    var regs: [value.MAX_REGISTERS]Reg = undefined;
    var i: u8 = 0;
    while (i < value.MAX_REGISTERS): (i += 1) {
      regs[i] = .{.val = i, .free = true};
    }
    return Self {.regs = regs, .reg = null};
  }

  pub fn setReg(self: *Self, reg: u32) void {
    self.reg = @intCast(u8, reg);
  }

  pub fn getReg(self: *Self) error{RegistersExhausted}!u32 {
    if (self.reg) |reg| {
      return reg;
    }
    for (&self.regs) |*reg| {
      if (reg.free) {
        reg.setFree(false);
        return @as(u32, reg.val);
      }
    }
    return error.RegistersExhausted;
  }

  pub fn releaseReg(self: *Self, reg: u32) void {
    for (&self.regs) |*_reg| {
      if (_reg.val == reg) {
        std.debug.assert(!_reg.free);
        _reg.setFree(true);
        return;
      }
    }
  }
};

pub const Compiler = struct {
  code: *Code,
  node: *Node,
  filename: []const u8,
  registers: VRegister,
  allocator: *NovaAllocator,
  vm: *VM,
  
  const Self = @This();

  pub fn init(node: *Node, filename: []const u8, vm: *VM, code: *Code, allocator: *NovaAllocator) Self {
    var self = Self {
      .code = code,
      .node = node, 
      .filename = filename,
      .allocator = allocator,
      .registers = VRegister.init(),
      .vm = vm,
    };
    return self;
  }

  fn compileError(self: *Self, msg: []const u8) noreturn {
    _ = self;
    std.debug.print("CompileError: {s}\n", .{msg});
    std.os.exit(1);
  }

  fn lastLine(self: *Self) u32 {
    return self.code.lines.items[self.code.lines.items.len - 1];
  }

  fn getReg(self: *Self) u32 {
    return self.registers.getReg() catch {
      self.compileError("registers exhausted");
    };
  }

  fn cConst(self: *Self, reg: u32, val: value.Value, line: usize) void {
    // load rx, memidx
    const memidx = self.code.writeValue(val, self.vm);
    self.code.write2ArgsInst(.Load, reg, memidx, @intCast(u32, line), self.vm);
  }

  fn cNum(self: *Self, node: *ast.LiteralNode) void {
    // load rx, memidx
    node.reg = self.getReg();
    self.cConst(node.reg, value.numberVal(node.value), node.line);
  }

  fn cBool(self: *Self, node: *ast.LiteralNode) void {
    // load rx, memidx
    node.reg = self.getReg();
    self.cConst(node.reg, value.boolVal(node.token.is(.TkTrue)), node.line);
  }

  fn cStr(self: *Self, node: *ast.LiteralNode) void {
    node.reg = self.getReg();
    self.cConst(
      node.reg,
      value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, node.token.is_alloc)),
      node.line
    );
  }

  fn cUnary(self: *Self, node: *ast.UnaryNode) void {
    const inst_op = node.op.toInstOp();
    const isConst = node.expr.isConst();
    const rx = self.getReg();
    // check that we don't exceed the 18 bits of rk/bx (+ 1 for expr)
    if ((self.code.values.items.len + value.MAX_REGISTERS + 1) < value.Code._18bits and isConst) {
      // for now, only numbers and booleans are recognized as consts
      var rk: u32 = undefined;
      if (node.expr.isNum()) {
        rk = self.code.storeConst(value.numberVal(node.expr.AstNum.value), self.vm);
      } else {
        rk = self.code.storeConst(value.boolVal(node.expr.AstBool.token.is(.TkTrue)), self.vm);
      }
      self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line), self.vm);
      node.reg = rx;
      return;
    }
    self.c(node.expr);
    const rk = node.expr.reg();
    self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line), self.vm);
    self.registers.releaseReg(rk);
    node.reg = rx;
  }

  inline fn cCmp(self: *Self, node: *ast.BinaryNode) void {
    if (!node.op.isCmpOp()) return;
    self.code.writeNoArgInst(@intToEnum(OpCode, @enumToInt(node.op)), node.line, self.vm);
  }

  inline fn cLgc(self: *Self, node: *ast.BinaryNode) void {
    self.c(node.left);
    const rx = node.left.reg();
    const end_jmp = self.code.write2ArgsJmp(node.op.toInstOp(), rx, self.lastLine(), self.vm);
    self.registers.releaseReg(rx);
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
    const rx = self.getReg();
    // check that we don't exceed the 9 bits of rk (+ 2 for lhs and rhs)
    if ((self.code.values.items.len + value.MAX_REGISTERS + 2) < value.Code._9bits) {
      if (lhsIsNum and rhsIsNum) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value), self.vm);
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value), self.vm);
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line), self.vm);
        self.cCmp(node);
        node.reg = rx;
        return;
      } else if (lhsIsNum) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value), self.vm);
        self.c(node.right);
        const rk2 = node.right.reg();
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line), self.vm);
        self.registers.releaseReg(rk2);
        self.cCmp(node);
        node.reg = rx;
        return;
      } else if (rhsIsNum) {
        self.c(node.left);
        const rk1 = node.left.reg();
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value), self.vm);
        self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line), self.vm);
        self.registers.releaseReg(rk1);
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
    self.code.write3ArgsInst(inst_op, rx, rk1, rk2, @intCast(u32, node.line), self.vm);
    self.registers.releaseReg(rk1);
    self.registers.releaseReg(rk2);
    self.cCmp(node);
    node.reg = rx;
  }

  fn cList(self: *Self, node: *ast.ListNode) void {
    var reg = self.getReg();
    const size = @intCast(u32, node.elems.items.len);
    if (size > 0) {
      // free to allow reuse by the first element
      self.registers.releaseReg(reg);
    }
    for (node.elems.items) |elem| {
      self.c(elem);
    }
    if (size > 1) {
      for (node.elems.items[1..]) |elem| {
        self.registers.releaseReg(elem.reg());
      }
    }
    node.reg = reg;
    // blst rx, elem-count
    self.code.write2ArgsInst(.Blst, reg, size, node.line, self.vm);
  }

  fn cMap(self: *Self, node: *ast.MapNode) void {
    var reg = self.getReg();
    const size = @intCast(u32, node.pairs.items.len);
    if (size > 0) {
      // free to allow reuse by the first element
      self.registers.releaseReg(reg);
    }
    for (node.pairs.items) |pair| {
      self.c(pair.key);
      self.c(pair.value);
    }
    if (size >= 1) {
      for (node.pairs.items[1..]) |elem| {
        self.registers.releaseReg(elem.key.reg());
        self.registers.releaseReg(elem.value.reg());
      }
      // also free the `value` reg of the first key-value pair, 
      // since we only need the `key`'s reg for the map.
      self.registers.releaseReg(node.pairs.items[0].value.reg());
    }
    node.reg = reg;
    // bmap rx, elem-count
    self.code.write2ArgsInst(.Bmap, reg, size, node.line, self.vm);
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
      .AstList => |*nd| self.cList(nd),
      .AstMap => |*nd| self.cMap(nd),
      .AstExprStmt => |*nd| self.cExprStmt(nd),
    }
  }

  pub fn compile(self: *Self) void {
    self.c(self.node);
    const last_line = self.code.lines.items[self.code.lines.items.len - 1];
    self.code.writeNoArgInst(.Ret, last_line, self.vm);
    // release memory associated with the arena, since we're done with it at this point.
    self.allocator.deinitArena();
  }

};
