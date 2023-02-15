const std = @import("std");
const util = @import("util.zig");
const OpCode = @import("opcode.zig").OpCode;
pub const OpType = @import("lex.zig").OpType;

pub const MAX_REGISTERS = 250;

pub const Value = u64;
const QNAN = @as(u64, 0x7ffc000000000000);
const SIGN_BIT = @as(u64, 0x8000000000000000);
// TAGS
const TAG_NIL = 0x1;      // 01
const TAG_FALSE = 0x2;    // 10
const TAG_TRUE = 0x3;     // 11
const TAG_NOTHING = 0x4;  // 100
// Values
pub const NIL_VAL = @as(Value, (QNAN | TAG_NIL));
pub const FALSE_VAL = @as(Value, (QNAN | TAG_FALSE));
pub const TRUE_VAL = @as(Value, (QNAN | TAG_TRUE));
pub const NOTHING_VAL = @as(Value, (QNAN | TAG_NOTHING));


pub inline fn numberVal(num: f64) Value {
  return @ptrCast(*const Value, &num).*;
}

pub inline fn asNumber(val: Value) f64 {
  return @ptrCast(*const f64, &val).*;
}

pub inline fn isNumber(val: Value) bool {
  return (val & QNAN) != QNAN;
}

pub inline fn boolVal(bl: bool) Value {
  return if (bl) TRUE_VAL else FALSE_VAL;
}

pub inline fn asBool(val: Value) bool {
  return val == TRUE_VAL;
}

pub inline fn isBool(val: Value) bool {
  return (val | 1) == TRUE_VAL;
}

pub inline fn isNil(val: Value) bool {
  return val == NIL_VAL;
}

pub inline fn valueEqual(a: Value, b: Value) bool {
  if (isNumber(a) and isNumber(b)) return asNumber(a) == asNumber(b);
  return a == b;
}

pub inline fn valueFalsy(val: Value) bool {
  return (isBool(val) and !asBool(val)) or isNil(val) or (isNumber(val) and asNumber(val) == 0);
}

pub fn printValue(val: Value) void {
  if (isNumber(val)) {
    std.debug.print("{d}", .{asNumber(val)});
  } else if (isBool(val)) {
    std.debug.print("{}", .{asBool(val)});
  } else if (isNil(val)) {
    std.debug.print("nil", .{});
  }
}

pub const Code = struct {
  words: std.ArrayList(u32),
  values: std.ArrayList(Value),
  lines: std.ArrayList(u32),

  const Self = @This();
  pub const _6bits: u32 = 0x3f;
  pub const _8bits: u32 = 0xff;
  pub const _9bits: u32 = 0x1ff;
  pub const _18bits: u32 = 0x3ffff;
  pub const _26bits: u32 = 0x3ffffff;
  pub const _32bits: u32 = 0xffffffff;

  pub fn init(allocator: std.mem.Allocator) Self {
    return Self {
      .words = std.ArrayList(u32).init(allocator),
      .values = std.ArrayList(Value).init(allocator),
      .lines = std.ArrayList(u32).init(allocator)
    };
  }

  fn checkOffset(offset: usize, max: u32, err_msg: []const u8) void {
    if (offset > max) {
      std.debug.print("{s}", .{err_msg});
      std.os.exit(1);
    }
  }

  pub inline fn readInstOp(word: u32) OpCode {
    // [op] rx rk1 rk2
    //  6   8   9   9
    const op = (word >> 26) & _6bits;
    return @intToEnum(OpCode, op);
  }


  pub inline fn readInstOpNoConv(word: u32) u32 {
    return (word >> 26) & Code._6bits;
  }

  pub inline fn readRX(word: u32) u32 {
    // op [rx] rk1 rk2
    // 6   8    9   9
    return (word >> 18) & _8bits;
  }

  pub inline fn readRK1(word: u32) u32 {
    // op rx [rk1] rk2
    // 6  8    9    9
    return (word >> 9) & _9bits;
  }

  pub inline fn readRK2(word: u32) u32 {
    // op rx rk1 [rk2]
    // 6  8   9    9
    return word & _9bits;
  }

  pub inline fn readBX(word: u32) u32 {
    // op rx [bx] 
    // 6  8   18
    return word & _18bits;
  }

  pub fn writeValue(self: *Self, value: Value) u32 {
    util.append(Value, &self.values, value);
    return @intCast(u32, self.values.items.len - 1);
  }

  pub fn writeByte(self: *Self, byte: u8, line: usize) void {
    util.append(u32, &self.words, (byte & _32bits) << 24);
    util.append(u32, &self.lines, @intCast(u32, line));
  }

  pub fn write3ArgsInst(self: *Self, op: OpCode,  arg1: u32, arg2: u32, arg3: u32, line: usize) void {
    // [op 6bits][reg 8bits][reg 9bits][reg 9bits]
    const inst = ((@enumToInt(op) & _6bits) << 26) | ((arg1 & _8bits) << 18) | ((arg2 & _9bits) << 9) | ((arg3 & _9bits));
    util.append(u32, &self.words, inst);
    util.append(u32, &self.lines, @intCast(u32, line));
  }

  pub fn write2ArgsInst(self: *Self, op: OpCode,  arg1: u32, arg2: u32, line: usize) void {
    // [op 6bits][reg 8bits][reg 18bits]
    const inst = ((@enumToInt(op) & _6bits) << 26) | ((arg1 & _8bits) << 18) | ((arg2 & _18bits));
    util.append(u32, &self.words, inst);
    util.append(u32, &self.lines, @intCast(u32, line));
  }

  pub fn write1ArgInst(self: *Self, op: OpCode,  arg: u32, line: usize) void {
    // [op 6bits][reg 26bits]
    const inst = ((@enumToInt(op) & _6bits) << 26) | ((arg & _26bits));
    util.append(u32, &self.words, inst);
    util.append(u32, &self.lines, @intCast(u32, line));
  }

  pub fn writeNoArgInst(self: *Self, op: OpCode, line: usize) void {
    // [op 6bits]
    const inst = ((@enumToInt(op) & _6bits) << 26);
    util.append(u32, &self.words, inst);
    util.append(u32, &self.lines, @intCast(u32, line));
  }

  pub fn write2ArgsJmp(self: *Self, op: OpCode, arg1: u32, line: usize) usize {
    // jmp_inst, arg1, dummy_offset
    //   [6]      [8]     [18]
    const offset = 0x40000;
    self.write2ArgsInst(op, arg1, offset, line);
    // return instruction offset
    return self.words.items.len - 1;
  }

  pub fn patch2ArgsJmp(self: *Self, index: usize) void {
    const inst = self.words.items[index];
    // get jmp_inst, arg1
    const first = (inst >> 26) & _6bits;
    const second = (inst >> 18) & _8bits;
    const real_offset = self.words.items.len - index - 1;
    checkOffset(real_offset, _18bits, "max jump offset exceeded");
    const new = (first << 26) | (second << 18) | real_offset;
    self.words.items[index] = @intCast(u32, new);
  }

  pub fn storeConst(self: *Self, value: Value) u32 {
    const idx = self.writeValue(value);
    return idx + MAX_REGISTERS;
  }
};