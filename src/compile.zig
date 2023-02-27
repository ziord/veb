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

  const DummyReg = 0xfff;

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

  pub fn getReg(self: *Self) error{RegistersExhausted}!u32 {
    for (&self.regs) |*reg| {
      if (reg.free) {
        reg.setFree(false);
        return @as(u32, reg.val);
      }
    }
    return error.RegistersExhausted;
  }

  pub fn releaseReg(self: *Self, reg: u32) void {
    if (reg == DummyReg) return;
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
  vreg: VRegister,
  allocator: *NovaAllocator,
  vm: *VM,
  
  const Self = @This();

  pub fn init(node: *Node, filename: []const u8, vm: *VM, code: *Code, allocator: *NovaAllocator) Self {
    var self = Self {
      .code = code,
      .node = node, 
      .filename = filename,
      .allocator = allocator,
      .vreg = VRegister.init(),
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
    return self.vreg.getReg() catch {
      self.compileError("registers exhausted");
    };
  }

  /// return true if within RK limit else false
  fn withinRKLimit(self: *Self, num_operands: usize) bool {
    // check that we don't exceed the 9 bits of rk (+ num_operands for number of operands to be added)
    return ((self.code.values.items.len + value.MAX_REGISTERS + num_operands) <= value.Code._9bits);
  }

  /// return true if within BX limit else false
  fn withinBXLimit(self: *Self) bool {
    // check that we don't exceed the 18 bits of bx (+ 1 for the new addition)
    return ((self.code.values.items.len + 1) <= value.Code._18bits);
  }

  fn cConst(self: *Self, reg: u32, val: value.Value, line: usize) void {
    // load rx, memidx
    const memidx = self.code.writeValue(val, self.vm);
    self.code.write2ArgsInst(.Load, reg, memidx, @intCast(u32, line), self.vm);
  }

  fn cNum(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    self.cConst(dst, value.numberVal(node.value), node.line);
    return dst;
  }

  fn cBool(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    // load rx, memidx
    self.cConst(dst, value.boolVal(node.token.is(.TkTrue)), node.line);
    return dst;
  }

  fn cStr(self: *Self, node: *ast.LiteralNode, dst: u32) u32 {
    self.cConst(
      dst,
      value.objVal(value.createString(self.vm, &self.vm.strings, node.token.value, node.token.is_alloc)),
      node.line
    );
    return dst;
  }

  fn cUnary(self: *Self, node: *ast.UnaryNode, reg: u32) u32 {
    const inst_op = node.op.toInstOp();
    const isConst = node.expr.isConst();
    const rx = reg;
    // check that we don't exceed the 18 bits of rk/bx (+ 1 for expr)
    if (isConst and self.withinBXLimit()) {
      // for now, only numbers and booleans are recognized as consts
      var rk: u32 = undefined;
      if (node.expr.isNum()) {
        rk = self.code.storeConst(value.numberVal(node.expr.AstNum.value), self.vm);
      } else {
        rk = self.code.storeConst(value.boolVal(node.expr.AstBool.token.is(.TkTrue)), self.vm);
      }
      self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line), self.vm);
      return rx;
    }
    const rk = self.c(node.expr, reg);
    self.code.write2ArgsInst(inst_op, rx, rk, @intCast(u32, node.line), self.vm);
    return rx;
  }

  inline fn cCmp(self: *Self, node: *ast.BinaryNode) void {
    // <, >, <=, >=, ==, !=
    if (!node.op.isCmpOp()) return;
    self.code.writeNoArgInst(@intToEnum(OpCode, @enumToInt(node.op)), node.line, self.vm);
  }

  inline fn cLgc(self: *Self, node: *ast.BinaryNode, reg: u32) u32 {
    // and, or
    var rx = self.c(node.left, reg);
    const end_jmp = self.code.write2ArgsJmp(node.op.toInstOp(), rx, self.lastLine(), self.vm);
    rx = self.c(node.right, reg);
    self.code.patch2ArgsJmp(end_jmp);
    return rx;
  }

  fn cBinary(self: *Self, node: *ast.BinaryNode, dst: u32) u32 {
    // handle and | or
    if (node.op.isLgcOp()) return self.cLgc(node, dst);
    // handle other binary ops
    const lhsIsNum = node.left.isNum();
    const rhsIsNum = node.right.isNum();
    const inst_op = node.op.toInstOp();
    // check that we don't exceed the 9 bits of rk (+ 2 for lhs and rhs)
    if (lhsIsNum and rhsIsNum) {
      if (self.withinRKLimit(2)) {
        const rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value), self.vm);
        const rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value), self.vm);
        self.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(u32, node.line), self.vm);
        self.cCmp(node);
        return dst;
      }
    }
    // fallthrough
    var rk1: u32 = undefined;
    var rk2: u32 = undefined;
    if (lhsIsNum and self.withinRKLimit(1)) { // 2 * (3 * (4 * 5))
      rk1 = self.code.storeConst(value.numberVal(node.left.AstNum.value), self.vm);
    } else {
      rk1 = self.c(node.left, dst);
    }
    if (rhsIsNum and self.withinRKLimit(1)) {
      rk2 = self.code.storeConst(value.numberVal(node.right.AstNum.value), self.vm);
    } else {
      const dst2 = self.getReg();
      rk2 = self.c(node.right, dst2);
      // we own this register, so we can free
      self.vreg.releaseReg(dst2);
    }
    self.code.write3ArgsInst(inst_op, dst, rk1, rk2, @intCast(u32, node.line), self.vm);
    self.cCmp(node);
    return dst;
  }

  fn cList(self: *Self, node: *ast.ListNode, dst: u32) u32 {
    const size = @intCast(u32, node.elems.items.len);
    self.code.write2ArgsInst(.Nlst, dst, size, node.line, self.vm);
    for (node.elems.items, 0..) |elem, i| {
      var reg = self.getReg();
      _ = self.c(elem, reg);
      var val = value.numberVal(@intToFloat(f64, i));
      var idx: u32 = undefined;
      if (self.withinRKLimit(1)) {
        idx = self.code.storeConst(val, self.vm);
      } else {
        idx = self.getReg();
        self.cConst(idx, val, node.line);
        self.vreg.releaseReg(idx);
      }
      self.code.write3ArgsInst(.Slst, dst, idx, reg, node.line, self.vm);
      self.vreg.releaseReg(reg);
    }
    return dst;
  }

  fn cMap(self: *Self, node: *ast.MapNode, dst: u32) u32 {
    const size = @intCast(u32, node.pairs.items.len);
    self.code.write2ArgsInst(.Nmap, dst, size, node.line, self.vm);
    for (node.pairs.items) |pair| {
      var reg1 = self.getReg();
      var reg2 = self.getReg();
      _ = self.c(pair.key, reg1);
      _ = self.c(pair.value, reg2);
      self.code.write3ArgsInst(.Smap, dst, reg1, reg2, node.line, self.vm);
      self.vreg.releaseReg(reg1);
      self.vreg.releaseReg(reg2);
    }
    return dst;
  }

  fn cExprStmt(self: *Self, node: *ast.ExprStmtNode, reg: u32) u32 {
    // sequence point
    const dst = self.getReg();
    _ = self.c(node.expr, dst);
    self.vreg.releaseReg(dst);
    return reg;
  }

  fn c(self: *Self, node: *Node, reg: u32) u32 {
    return switch (node.*) {
      .AstNum => |*nd| self.cNum(nd, reg),
      .AstStr => |*nd| self.cStr(nd, reg),
      .AstBool => |*nd| self.cBool(nd, reg),
      .AstUnary => |*nd| self.cUnary(nd, reg),
      .AstBinary => |*nd| self.cBinary(nd, reg),
      .AstList => |*nd| self.cList(nd, reg),
      .AstMap => |*nd| self.cMap(nd, reg),
      .AstExprStmt => |*nd| self.cExprStmt(nd, reg),
    };
  }

  pub fn compile(self: *Self) void {
    util.assert(self.c(self.node, VRegister.DummyReg) == VRegister.DummyReg, "should be a dummy register");
    const last_line = self.code.lines.items[self.code.lines.items.len - 1];
    self.code.writeNoArgInst(.Ret, last_line, self.vm);
    // release memory associated with the arena, since we're done with it at this point.
    self.allocator.deinitArena();
  }

};
