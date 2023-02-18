const std = @import("std");
const util = @import("util.zig");
const Mem = @import("mem.zig");
const Vec = @import("vec.zig").Vec;
const OpCode = @import("opcode.zig").OpCode;
pub const OpType = @import("lex.zig").OpType;


pub const Code = struct {
  words: Vec(u32),
  values: Vec(Value),
  lines: Vec(u32),

  const Self = @This();
  pub const _6bits: u32 = 0x3f;
  pub const _8bits: u32 = 0xff;
  pub const _9bits: u32 = 0x1ff;
  pub const _18bits: u32 = 0x3ffff;
  pub const _26bits: u32 = 0x3ffffff;
  pub const _32bits: u32 = 0xffffffff;

  pub fn init() Self {
    return Self {
      .words = Vec(u32).init(),
      .values = Vec(Value).init(),
      .lines = Vec(u32).init()
    };
  }

  pub fn deinit(self: *Self, vm: *VM) void {
    self.words.clearAndFree(vm);
    self.values.clearAndFree(vm);
    self.lines.clearAndFree(vm);
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

  pub fn writeValue(self: *Self, value: Value, vm: *VM) u32 {
    self.values.push(value, vm);
    return @intCast(u32, self.values.items.len - 1);
  }

  pub fn write3ArgsInst(self: *Self, op: OpCode,  arg1: u32, arg2: u32, arg3: u32, line: usize, vm: *VM) void {
    // [op 6bits][reg 8bits][reg 9bits][reg 9bits]
    const inst = ((@enumToInt(op) & _6bits) << 26) | ((arg1 & _8bits) << 18) | ((arg2 & _9bits) << 9) | ((arg3 & _9bits));
    self.words.push(inst, vm);
    self.lines.push(@intCast(u32, line), vm);
  }

  pub fn write2ArgsInst(self: *Self, op: OpCode,  arg1: u32, arg2: u32, line: usize, vm: *VM) void {
    // [op 6bits][reg 8bits][reg 18bits]
    const inst = ((@enumToInt(op) & _6bits) << 26) | ((arg1 & _8bits) << 18) | ((arg2 & _18bits));
    self.words.push(inst, vm);
    self.lines.push(@intCast(u32, line), vm);
  }

  pub fn write1ArgInst(self: *Self, op: OpCode,  arg: u32, line: usize, vm: *VM) void {
    // [op 6bits][reg 26bits]
    const inst = ((@enumToInt(op) & _6bits) << 26) | ((arg & _26bits));
    self.words.push(inst, vm);
    self.lines.push(@intCast(u32, line), vm);
  }

  pub fn writeNoArgInst(self: *Self, op: OpCode, line: usize, vm: *VM) void {
    // [op 6bits]
    const inst = ((@enumToInt(op) & _6bits) << 26);
    self.words.push(inst, vm);
    self.lines.push(@intCast(u32, line), vm);
  }

  pub fn write2ArgsJmp(self: *Self, op: OpCode, arg1: u32, line: usize, vm: *VM) usize {
    // jmp_inst, arg1, dummy_offset
    //   [6]      [8]     [18]
    const offset = 0x40000;
    self.write2ArgsInst(op, arg1, offset, line, vm);
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

  pub fn storeConst(self: *Self, value: Value, vm: *VM) u32 {
    const idx = self.writeValue(value, vm);
    return idx + MAX_REGISTERS;
  }
};

pub const MAX_REGISTERS = 250;
pub const LOAD_FACTOR = 80;
pub const MAX_STR_HASHING_LEN = 0xff;

const QNAN = @as(u64, 0x7ffc000000000000);
const SIGN_BIT = @as(u64, 0x8000000000000000);
// TAGS
const TAG_NIL = 0x1;      // 01
const TAG_FALSE = 0x2;    // 10
const TAG_TRUE = 0x3;     // 11
const TAG_NOTHING = 0x4;  // 100
const TAG_OBJECT = (SIGN_BIT | QNAN);
// Values
pub const Value = u64;
pub const NIL_VAL = @as(Value, (QNAN | TAG_NIL));
pub const FALSE_VAL = @as(Value, (QNAN | TAG_FALSE));
pub const TRUE_VAL = @as(Value, (QNAN | TAG_TRUE));
pub const NOTHING_VAL = @as(Value, (QNAN | TAG_NOTHING));

pub const ObjTy = enum(u8) {
  ObjStr,
};

pub const Obj = packed struct {
  ty: ObjTy,
  next: ?*Obj,
};

pub const ObjString = struct {
  obj: Obj,
  hash: u64,
  str: []const u8,
};


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

pub inline fn objVal(ptr: anytype) Value {
  return @intCast(Value, @intCast(u64, @ptrToInt(ptr)) | TAG_OBJECT);
}

pub inline fn asObj(val: Value) *Obj {
  return @intToPtr(*Obj, @intCast(usize, (val & ~TAG_OBJECT)));
}

pub inline fn isObj(val: Value) bool {
  return (val & TAG_OBJECT) == TAG_OBJECT;
}

pub inline fn isObjType(val: Value, ty: ObjTy) bool {
  return asObj(val).ty == ty;
}

/// convert a `Value` type to a specified Object type
pub inline fn valToSpecObject(comptime T: type, val: Value) *T {
  return @ptrCast(*T, asObj(val));
}

/// convert an `Obj` type to a specified Object type
pub inline fn objToSpecObject(comptime T: type, obj: *Obj) *T {
  return @ptrCast(*T, obj);
}

pub inline fn isString(val: Value) bool {
  return isObjType(val, .ObjStr);
}

pub inline fn asString(val: Value) *ObjString {
  return @ptrCast(*ObjString, asObj(val));
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
    util.print("{d}", .{asNumber(val)});
  } else if (isBool(val)) {
    util.print("{}", .{asBool(val)});
  } else if (isNil(val)) {
    util.print("nil", .{});
  } else if (isObj(val)) {
    printObject(val);
  }
}

pub fn printObject(val: Value) void {
  const obj = asObj(val);
  switch (obj.ty) {
    .ObjStr => {
      util.print("{s}", .{asString(val).str});
    }
  }
}

//**StringHashMap**//
pub const StringContext = struct {
  pub fn hash(self: @This(), k: *ObjString) u64 {
    _ = self;
    return k.hash;
  }

  pub fn eql(self: @This(), k1: *ObjString, k2: *ObjString) bool {
    _ = self;
    return k1 == k2;
  }
};

pub const StringHashMap = std.HashMap(*ObjString, Value, StringContext, LOAD_FACTOR);

pub fn strmapPut(map: *StringHashMap, key: *ObjString, val: Value) void {
  map.put(key, val) catch |e| {
    util.error_("Map put error: {}", .{e});
  };
}

fn strmapFindInterned(map: *StringHashMap, str: []const u8, hash: u64) ?*ObjString {
  var it = map.keyIterator();
  while (it.next()) |k| {
    if (k.*.hash == hash and std.mem.eql(u8, k.*.str, str)) {
      return k.*;
    }
  }
  return null;
}

pub fn hashString(str: []const u8) u64 {
  // FNV-1a hashing algorithm
  @setRuntimeSafety(false);
  var hash: u128 = 2166136261;
  const fnv_prime: u32 = 16777619;
  var i: usize = 0;
  const len = if (str.len > MAX_STR_HASHING_LEN) MAX_STR_HASHING_LEN else str.len;
  while (i < len): (i += 1) {
    hash = hash ^ @as(u8, str[i]);
    hash = hash * fnv_prime;
  }
  return @truncate(u64, hash);
}
//**End StringHashMap**//


pub const VM = @import("vm.zig").VM;

pub fn createObject(vm: *VM, ty: ObjTy, comptime T: type) *T {
  var mem = vm.gc.mem.alloc(T, vm);
  mem.obj.ty = ty;
  mem.obj.next = vm.objects;
  vm.objects = &mem.obj;
  return mem;
}

pub fn createString(vm: *VM, map: *StringHashMap, str: []const u8, is_alloc: bool) *ObjString {
  const hash = hashString(str);
  var string = strmapFindInterned(map, str, hash);
  if (string == null) {
    var tmp = @call(.always_inline, createObject, .{vm, .ObjStr, ObjString});
    tmp.str = str;
    tmp.hash = hash;
    if (!is_alloc) {
      var s = vm.gc.mem.allocBuf(u8, vm, str.len);
      std.mem.copy(u8, s, str);
      tmp.str = s;
    } else {
      vm.gc.bytes_allocated += str.len;
    }
    strmapPut(map, tmp, FALSE_VAL);
    string = tmp;
    return tmp;
  }
  return string.?;
}
