const std = @import("std");
const vl = @import("value.zig");
const OpCode = @import("opcode.zig").OpCode;

const Value = vl.Value;
const Code = vl.Code;

pub const VM = struct {
  sp: usize,
  ip: usize,
  stack: [STACK_MAX]Value,
  code: Code,
  allocator: std.mem.Allocator,

  const Self = @This();
  const STACK_MAX = 0x15;
  const RuntimeError = error{RuntimeError};

  pub fn init(allocator: std.mem.Allocator, code: Code) Self {
    return Self {
      .sp = 0, 
      .ip = 0, 
      .stack = undefined, 
      .allocator = allocator, 
      .code = code
    };
  }

  inline fn readWord(self: *Self) u32 {
    self.ip += 1;
    return self.code.words.items[self.ip - 1];
  }

  inline fn readInstOp(self: *Self, word: u32) OpCode {
    _ = self;
    const op = (word >> 26) & Code._6bits;
    return @intToEnum(OpCode, op);
  }

  inline fn read3Args(self: *Self, word: u32, a1: *u32, a2: *u32, a3: *u32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = (word >> 9) & Code._9bits;
    a3.* = word & Code._9bits;
  }

  inline fn read2Args(self: *Self, word: u32, a1: *u32, a2: *u32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = word & Code._18bits;
  }

  inline fn read1Arg(self: *Self, word: u32, a: *u32) void {
    _ = self;
    a.* = word & Code._26bits;
  }

  inline fn RK(self: *Self, x: u32) Value {
    // rk(x) = r(x) if x < MAX_REGISTERS else k(x - MAX_REGISTERS)
    return if (x < vl.MAX_REGISTERS) self.stack[x] else self.code.values.items[x - vl.MAX_REGISTERS];
  }

  inline fn assert(self: *Self, cond: bool) void {
    _ = self;
    std.debug.assert(cond);
  }

  fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) RuntimeError {
    _ = self;
    std.debug.print(fmt, args);
    return error.RuntimeError;
  }

  inline fn printStack(self: *Self) void {
    for (self.stack) |val| {
      std.debug.print("[ ", .{});
      vl.printValue(val);
      std.debug.print(" ]", .{});
    }
    std.debug.print("\n", .{});
  }

  pub fn run(self: *Self) RuntimeError!void {
    while (true) {
      const inst = @call(.always_inline, self.readWord, .{});
      switch (@call(.always_inline, self.readInstOp, .{inst})) {
        .Add => {
          // add rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) + vl.asNumber(b));
          continue;
        },
        .Sub => {
          // sub rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) - vl.asNumber(b));
          continue;
        },
        .Mul => {
          // mul rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) * vl.asNumber(b));
          continue;
        },
        .Div => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) / vl.asNumber(b));
          continue;
        },
        .Mod => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          if (@call(.always_inline, vl.asNumber, .{b}) == 0) {
            return self.runtimeError("Modulo by zero\n", .{});
          }
          self.stack[rx] = vl.numberVal(@rem(vl.asNumber(a), vl.asNumber(b)));
          continue;
        },
        .Load => {
          // load rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          self.stack[rx] = self.code.values.items[bx];
          continue;
        },
        .Ret => {
          std.debug.print("stack: \n", .{});
          self.printStack();
          break;
        },
      }
    }
  }

};
