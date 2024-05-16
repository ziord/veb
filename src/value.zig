const std = @import("std");
const util = @import("util.zig");
const Mem = @import("mem.zig");
const Vec = @import("vec.zig").Vec;
const Map = @import("map.zig").Map;
const NativeFns = @import("native.zig").NativeFns;
const OpCode = @import("opcode.zig").OpCode;
pub const ks = @import("constants.zig");
pub const OpType = @import("lex.zig").OpType;

/// a register-encoded instruction
pub const Inst = u32;

pub const Code = extern struct {
  words: Vec(Inst),
  values: Vec(Value),
  lines: Vec(u32),

  const Self = @This();
  pub const _6Bits: u32 = 0x3f;
  pub const _8Bits: u32 = 0xff;
  pub const _9Bits: u32 = 0x1ff;
  pub const _17Bits: u32 = 0x1ffff;
  pub const _18Bits: u32 = 0x3ffff;
  pub const _26Bits: u32 = 0x3ffffff;
  pub const _32Bits: u32 = 0xffffffff;

  pub fn init() Self {
    return Self {
      .words = Vec(Inst).init(),
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
    // rk2 rk1 rx  [op] 
    //  9   9   8   6 
    return @enumFromInt(word & _6Bits);
  }


  pub inline fn readInstOpNoConv(word: u32) u32 {
    return word & _6Bits;
  }

  pub inline fn readRX(word: u32) u32 {
    // rk2 rk1 [rx] op 
    //  9   9   8   6       
    return (word >> 6) & _8Bits;
  }

  pub inline fn readRK1(word: u32) u32 {
    // rk2 [rk1] rx op 
    //  9    9   8   6   
    return (word >> 14) & _9Bits;
  }

  pub inline fn readRK2(word: u32) u32 {
    // [rk2] rk1 rx op 
    //  9    9   8   6  
    return (word >> 23) & _9Bits;
  }

  pub inline fn readBX(word: u32) u32 {
    // [bx] rx op 
    //  18  8  6
    return (word >> 14) & _18Bits;
  }

  pub inline fn readSBX(word: u32) i32 {
    // [sbx] rx op 
    //  18  8  6
    const _word = word & ((1 << 31) - 1);
    const mag: i32 = @intCast((_word >> 14) & _17Bits);
    return mag - @as(i32, @intCast((((word >> 31) & 1)) << 17));
  }

  pub fn resetBy(self: *Self, n: u32) void {
    for (0..n) |_| {
      _ = self.words.pop();
      _ = self.lines.pop();
    }
  }

  pub fn writeValue(self: *Self, value: Value, vm: *VM) u32 {
    self.values.push(value, vm);
    return @intCast(self.values.len - 1);
  }

  pub fn write3ArgsInst(self: *Self, op: OpCode,  arg1: u32, arg2: u32, arg3: u32, line: usize, vm: *VM) void {
    // [reg 9bits][reg 9bits][reg 8bits][op 6bits]
    const inst = ((arg3 & _9Bits) << 23) | ((arg2 & _9Bits) << 14) | ((arg1 & _8Bits) << 6) | (@intFromEnum(op) & _6Bits);
    self.words.push(inst, vm);
    self.lines.push(@intCast(line), vm);
  }

  pub fn write2ArgsInst(self: *Self, op: OpCode,  arg1: u32, arg2: u32, line: usize, vm: *VM) void {
    // [reg 18bits][reg 8bits][op 6bits]
    const inst = ((arg2 & _18Bits) << 14) | ((arg1 & _8Bits) << 6) | (@intFromEnum(op) & _6Bits);
    self.words.push(inst, vm);
    self.lines.push(@intCast(line), vm);
  }

  pub fn write2ArgsSignedInst(self: *Self, op: OpCode,  arg1: u32, arg2: i32, line: usize, vm: *VM) void {
    // [reg 1bit][reg 17bits][reg 8bits][op 6bits]
    @setRuntimeSafety(false);
    var sign_bit: u32 = 0;
    var magnitude: u32 = 0;
    if (arg2 >= 0) {
      magnitude = @intCast(arg2);
    } else {
      // set sign_bit to 1 and add 2^bit_length to get the positive counterpart - two's complement form
      sign_bit = 1;
      magnitude = @intCast((1 << 17) + arg2);
    }
    const inst = (sign_bit << 31) | ((magnitude & _17Bits) << 14) | ((arg1 & _8Bits) << 6) | (@intFromEnum(op) & _6Bits);
    self.words.push(inst, vm);
    self.lines.push(@intCast(line), vm);
  }

  pub fn write1ArgInst(self: *Self, op: OpCode,  arg: u32, line: usize, vm: *VM) void {
    // [reg 26bits][op 6bits]
    const inst = ((arg & _26Bits) << 6) | (@intFromEnum(op) & _6Bits);
    self.words.push(inst, vm);
    self.lines.push(@intCast(line), vm);
  }

  pub fn writeNoArgInst(self: *Self, op: OpCode, line: usize, vm: *VM) void {
    // [op 6bits]
    const inst = (@intFromEnum(op) & _6Bits);
    self.words.push(inst, vm);
    self.lines.push(@intCast(line), vm);
  }

  pub fn write2ArgsJmp(self: *Self, op: OpCode, arg1: u32, line: usize, vm: *VM) usize {
    // jmp_inst, arg1, dummy_offset
    //   [6]      [8]     [18]
    self.write2ArgsInst(op, arg1, 0x40000, line, vm);
    // return instruction offset
    return self.words.len - 1;
  }

  pub fn patch2ArgsJmp(self: *Self, index: usize) void {
    const inst = self.words.items[index];
    // get jmp_inst, arg1
    const first = inst & _6Bits;
    const second = (inst >> 6) & _8Bits;
    const real_offset = self.words.len - index - 1;
    checkOffset(real_offset, _18Bits, "max jump offset exceeded");
    const new = real_offset << 14 | (second << 6) | first;
    self.words.items[index] = @intCast(new);
  }

  pub fn write2ArgsSJmp(self: *Self, op: OpCode, arg1: u32, signed: bool, line: usize, vm: *VM) usize {
    // jmp_inst, arg1, dummy_offset
    //   [6]      [8]     [18]
    self.write2ArgsSignedInst(op, arg1, if (signed) -0x40000 else 0x40000, line, vm);
    // return instruction offset
    return self.words.len - 1;
  }

  pub fn patch2ArgsSJmp(self: *Self, index: usize) void {
    const inst = self.words.items[index];
    // get jmp_inst, arg1
    const sign_bit = (inst >> 31) & 1;
    const first = inst & _6Bits;
    const second = (inst >> 6) & _8Bits;
    const real_offset = self.words.len - index - 1;
    checkOffset(real_offset, _17Bits, "max jump offset exceeded");
    var _offset: u32 = 0;
    if (sign_bit == 0) {
      _offset = @intCast(real_offset);
    } else {
      _offset = @intCast((1 << 17) - real_offset);
    }
    const new = (sign_bit << 31) | (_offset << 14) | (second << 6) | first;
    self.words.items[index] = @intCast(new);
  }

  pub inline fn getInstLen(self: *Self) usize {
    return self.words.size();
  }

  pub fn storeConst(self: *Self, value: Value, vm: *VM) u32 {
    const idx = self.writeValue(value, vm);
    return idx + MAX_REGISTERS;
  }
};

pub const MAX_REGISTERS = 250;
pub const LOAD_FACTOR = 80;
pub const MAX_STR_HASHING_LEN = 0x7ff;
pub const MAX_STR_LEN = ks.MAX_STR_LEN;

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

pub const NativeFn = *const fn (*VM, argc: u32, args: u32) Value;
pub const StringHashMap = Map(*const ObjString, Value);
pub const ValueHashMap = Map(Value, Value);

pub const ObjId = enum(u8) {
  objstring,
  objlist,
  objvalmap,
  objtuple,
  objfn,
  objclosure,
  objupvalue,
  objnativefn,
  objfiber,
  objerror,
  objclass,
  objinstance,
  objmethod,
  objstruct,
  objtag,
};

pub const Obj = extern struct {
  id: ObjId,
  cls: ?*ObjClass = null,
  next: ?*Obj,
};

pub const ObjString = extern struct {
  obj: Obj,
  hash: u64,
  len: usize,
  str: [*]const u8,

  pub inline fn string(this: *const @This()) []const u8 {
    return this.str[0..this.len];
  }

  pub inline fn concat(this: *const @This(), other: *const @This(), al: std.mem.Allocator) []const u8 {
    var new = util.allocSlice(u8, this.len + other.len, al);
    @memcpy(new.ptr, this.string());
    @memcpy(new.ptr + this.len, other.string());
    return new;
  }
};

pub const ObjList = extern struct {
  obj: Obj,
  len: usize,
  capacity: usize,
  items: [*]Value,

  pub fn append(self: *@This(), vm: *VM, item: Value) void {
    if (self.len >= self.capacity) {
      const new_capacity = Mem.growCapacity(self.capacity);
      self.items = vm.mem.resizeBuf(Value, vm, self.items, self.capacity, new_capacity).ptr;
      self.capacity = new_capacity;
    }
    self.items[self.len] = item;
    self.len += 1;
  }
};

pub const ObjTuple = extern struct {
  obj: Obj,
  len: usize,
  items: [*]Value,
};

pub const ObjMap = extern struct {
  obj: Obj,
  meta: ValueHashMap,
};

pub const ObjSMap = extern struct {
  obj: Obj,
  meta: StringHashMap,
};

pub const ObjFn = extern struct {
  obj: Obj,
  arity: u8,
  envlen: usize,
  code: Code,
  name: *const ObjString,

  pub inline fn getName(self: *const ObjFn) []const u8 {
    return self.name.string();
  }
};

pub const MethodType = enum(u8) {
  BoundNativeMethod,
  BoundUserMethod,
};

pub const ObjMethod = extern struct {
  obj: Obj,
  typ: MethodType,
  as: extern union {
    native: BoundNativeFn,
    user: BoundUserFn,
  },

  pub const BoundUserFn = extern struct {
    instance: Value,
    closure: *ObjClosure,
  };

  pub const BoundNativeFn = extern struct {
    instance: Value,
    fun: *ObjNativeFn,
  };

  pub inline fn isBoundUserMethod(self: *@This()) bool {
    return self.typ == MethodType.BoundUserMethod;
  }

  pub inline fn isBoundNativeMethod(self: *@This()) bool {
    return self.typ == MethodType.BoundNativeMethod;
  }
};

pub const ObjNativeFn = extern struct {
  obj: Obj,
  arity: u32,
  fun: NativeFn,
  name: usize,

  pub fn getName(self: *const ObjNativeFn) []const u8 {
    return NativeFns[self.name];
  }
};

pub const ObjClosure = extern struct {
  obj: Obj,
  fun: *ObjFn,
  env: [*]*ObjUpvalue,
};

pub const ObjUpvalue = extern struct {
  obj: Obj,
  value: Value = NOTHING_VAL,
  loc: *Value,
  next: ?*ObjUpvalue,
};

pub const ObjError = extern struct {
  obj: Obj,
  val: Value,
};

pub const ObjClass = extern struct {
  obj: Obj,
  mlen: usize,
  name: *const ObjString,
  methods: [*]Value,

  pub inline fn getInitMethod(self: *@This()) ?Value {
    for (self.methods[0..self.mlen]) |mtd| {
      if (std.mem.eql(u8, asClosure(mtd).fun.getName(), "init")) {
        return mtd;
      }
    }
    return null;
  }

  pub inline fn nameStr(self: *const @This()) []const u8 {
    return self.name.string();
  }
};

pub const ObjTag = extern struct {
  obj: Obj,
  name: *const ObjString,

  pub inline fn nameStr(self: *@This()) []const u8 {
    return self.name.string();
  }

  pub inline fn eql(self: *@This(), other: *@This()) bool {
    return self.name == other.name;
  }
};

/// structs are just classes with fields for methods
pub const ObjStruct = ObjClass;

pub const ObjInstance = extern struct {
  obj: Obj,
  flen: usize,
  fields: [*]Value,
};

pub const ObjFiber = extern struct {
  obj: Obj,
  errval: Value = NOTHING_VAL,
  origin: FiberOrigin,
  stack_cap: usize,
  frame_cap: usize,
  frame_len: usize,
  fp: *CallFrame,
  stack: [*]Value,
  frames: [*]CallFrame,
  open_upvalues: ?*ObjUpvalue,
  caller: ?*ObjFiber,

  pub fn appendFrame(self: *ObjFiber, clos: *ObjClosure, stack: [*]Value) void {
    std.debug.assert(self.frame_len < self.frame_cap);
    self.frames[self.frame_len] = CallFrame {.ip = 0, .closure = clos, .stack = stack};
    self.fp = &self.frames[self.frame_len];
    self.frame_len += 1;
  }

  pub fn popFrame(self: *ObjFiber) CallFrame {
    self.frame_len -= 1;
    const frame = self.frames[self.frame_len];
    if (self.frame_len > 0) {
      self.fp = &self.frames[self.frame_len - 1];
    }
    return frame;
  }
};

pub const CallFrame = struct {
  ip: usize,
  closure: *ObjClosure,
  stack: [*]Value,
};

pub const FiberOrigin = enum (u8) {
  Root,
  Other,
};

pub const FiberStatus = enum {
  Created,
  Running,
  Suspended,
  Completed,
};

pub const ValueStringWriter = struct {
  backing: Vec(u8),

  pub const Writer = Vec(u8).Writer;

  pub fn init() @This() {
    return .{.backing = Vec(u8).init()};
  }

  pub fn writer(self: *@This(), vm: *VM) Writer {
    return self.backing.writer(vm);
  }

  pub inline fn allocator(self: *@This()) std.mem.Allocator {
    return self.backing.allocator;
  }

  pub inline fn capacity(self: *@This()) usize {
    return self.backing.capacity;
  }

  pub inline fn len(self: *@This()) usize {
    return self.backing.items.len;
  }

  pub inline fn clear(self: *@This()) void {
    self.backing.clearRetainingCapacity();
  }

  pub fn items(self: *@This()) []const u8 {
    const chars = self.backing.getItems();
    self.backing.len = 0;
    return chars;
  }

  pub fn deinit(self: *@This(), vm: *VM) void {
    self.backing.clearAndFree(vm);
  }
};

pub inline fn numberVal(num: f64) Value {
  return @as(*const Value, @ptrCast(&num)).*;
}

pub inline fn asNumber(val: Value) f64 {
  return @bitCast(val);
}

pub inline fn asIntNumber(comptime T: type, val: Value) T {
  @setRuntimeSafety(false);
  return @intFromFloat(asNumber(val));
}

pub inline fn isNumber(val: Value) bool {
  return (val & QNAN) != QNAN;
}

pub fn isNumberNoInline(val: Value) bool {
  return isNumber(val);
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

pub fn isBoolNoInline(val: Value) bool {
  return isBool(val);
}

pub inline fn nilVal() Value {
  return NIL_VAL;
}

pub inline fn isNil(val: Value) bool {
  return val == NIL_VAL;
}

pub fn isNilNoInline(val: Value) bool {
  return isNil(val);
}

pub fn isVoidNoInline(val: Value) bool {
  return isNothing(val);
}

pub fn isNoreturnNoInline(val: Value) bool {
  _ = val;
  return false;
}

pub fn isAnyNoInline(val: Value) bool {
  _ = val;
  return true;
}

pub inline fn isNothing(val: Value) bool {
  return val == NOTHING_VAL;
}

pub inline fn objVal(ptr: anytype) Value {
  return (@as(u64, @intFromPtr(ptr)) | TAG_OBJECT);
}

pub inline fn asObj(val: Value) *Obj {
  return @ptrFromInt(val & ~TAG_OBJECT);
}

pub inline fn isObj(val: Value) bool {
  return (val & TAG_OBJECT) == TAG_OBJECT;
}

pub inline fn isObjType(val: Value, id: ObjId) bool {
  return isObj(val) and asObj(val).id == id;
}

pub inline fn isString(val: Value) bool {
  return isObjType(val, .objstring);
}

pub inline fn isList(val: Value) bool {
  return isObjType(val, .objlist);
}

pub inline fn isTuple(val: Value) bool {
  return isObjType(val, .objtuple);
}

pub inline fn isMap(val: Value) bool {
  return isObjType(val, .objvalmap);
}

pub inline fn isFn(val: Value) bool {
  return isObjType(val, .objfn);
}

pub inline fn isClosure(val: Value) bool {
  return isObjType(val, .objclosure);
}

pub inline fn isUpvalue(val: Value) bool {
  return isObjType(val, .objupvalue);
}

pub inline fn isNativeFn(val: Value) bool {
  return isObjType(val, .objnativefn);
}

pub inline fn isFiber(val: Value) bool {
  return isObjType(val, .objfiber);
}

pub inline fn isError(val: Value) bool {
  return isObjType(val, .objerror);
}

pub inline fn isClass(val: Value) bool {
  return isObjType(val, .objclass);
}

pub inline fn isStruct(val: Value) bool {
  return isObjType(val, .objstruct);
}

pub inline fn isTag(val: Value) bool {
  return isObjType(val, .objtag);
}

pub inline fn isMethod(val: Value) bool {
  return isObjType(val, .objmethod);
}

pub inline fn isInstance(val: Value) bool {
  return isObjType(val, .objinstance);
}

pub fn isStringNoInline(val: Value) bool {
  return isString(val);
}

pub fn isListNoInline(val: Value) bool {
  return isList(val);
}

pub fn isMapNoInline(val: Value) bool {
  return isMap(val);
}

pub fn isTupleNoInline(val: Value) bool {
  return isTuple(val);
}

pub fn isFnNoInline(val: Value) bool {
  return isFn(val);
}

pub fn isClosureNoInline(val: Value) bool {
  return isClosure(val);
}

pub fn isUpvalueNoInline(val: Value) bool {
  return isUpvalue(val);
}

pub fn isNativeFnNoInline(val: Value) bool {
  return isNativeFn(val);
}

pub fn isFiberNoInline(val: Value) bool {
  return isFiber(val);
}

pub fn isErrorNoInline(val: Value) bool {
  return isError(val);
}

pub inline fn asString(val: Value) *ObjString {
  return @ptrCast(asObj(val));
}

pub inline fn asList(val: Value) *ObjList {
  return @ptrCast(asObj(val));
}

pub inline fn asMap(val: Value) *ObjMap {
  return @ptrCast(asObj(val));
}

pub inline fn asTuple(val: Value) *ObjTuple {
  return @ptrCast(asObj(val));
}

pub inline fn asFn(val: Value) *ObjFn {
  return @ptrCast(asObj(val));
}

pub inline fn asNativeFn(val: Value) *ObjNativeFn {
  return @ptrCast(asObj(val));
}

pub inline fn asClosure(val: Value) *ObjClosure {
  return @ptrCast(asObj(val));
}

pub inline fn asUpvalue(val: Value) *ObjUpvalue {
  return @ptrCast(asObj(val));
}

pub inline fn asFiber(val: Value) *ObjFiber {
  return @ptrCast(asObj(val));
}

pub inline fn asError(val: Value) *ObjError {
  return @ptrCast(asObj(val));
}

pub inline fn asClass(val: Value) *ObjClass {
  return @ptrCast(asObj(val));
}

pub inline fn asStruct(val: Value) *ObjStruct {
  return @ptrCast(asObj(val));
}

pub inline fn asTag(val: Value) *ObjTag {
  return @ptrCast(asObj(val));
}

pub inline fn asInstance(val: Value) *ObjInstance {
  return @ptrCast(asObj(val));
}

pub inline fn asMethod(val: Value) *ObjMethod {
  return @ptrCast(asObj(val));
}

pub inline fn valueEqual(a: Value, b: Value) bool {
  if (a == b) {
    return true;
  } else if (isString(a) and isString(b)) {
    return std.mem.eql(u8, asString(a).string(), asString(b).string());
  } else {
    return false;
  }
}

pub inline fn valueFalsy(val: Value) bool {
  return (
    (isBool(val) and !asBool(val))
    or isNil(val)
    or (isNumber(val) and asNumber(val) == 0)
    or (isString(val) and asString(val).len == 0)
    or (isList(val) and asList(val).len == 0)
    or (isMap(val) and asMap(val).meta.len == 0)
    or (isTuple(val) and asTuple(val).len == 0)
  );
}

pub fn display(val: Value) void {
  if (isString(val)) {
    var tmp = asString(val);
    util.print("\"{s}\"", .{tmp.string()});
  } else {
    printValue(val);
  }
}

pub fn printValue(val: Value) void {
  if (isNumber(val)) {
    util.print("{d}", .{asNumber(val)});
  } else if (isBool(val)) {
    util.print("{}", .{asBool(val)});
  } else if (isNil(val)) {
    util.print(ks.NoneVar, .{});
  } else if (isObj(val)) {
    printObject(val);
  }
}

pub fn printObject(val: Value) void {
  const obj = asObj(val);
  switch (obj.id) {
    .objstring => {
      util.print("{s}", .{asString(val).string()});
    },
    .objlist => {
      var list = asList(val);
      const add_comma = list.len > 1;
      const comma_end = if (add_comma) list.len - 1 else 0;
      util.print("[", .{});
      for (list.items[0..list.len], 0..) |item, i| {
        @call(.always_inline, display, .{item});
        if (add_comma and i < comma_end) {
          util.print(", ", .{});
        }
      }
      util.print("]", .{});
    },
    .objtuple => {
      var tuple = asTuple(val);
      const add_comma = tuple.len > 1;
      const comma_end = if (add_comma) tuple.len - 1 else 0;
      util.print("(", .{});
      for (tuple.items[0..tuple.len], 0..) |item, i| {
        @call(.always_inline, display, .{item});
        if (add_comma and i < comma_end) {
          util.print(", ", .{});
        }
      }
      if (tuple.len == 1) {
        util.print(",)", .{});
      } else {
        util.print(")", .{});
      }
    },
    .objerror => {
      const err = asError(val);
      util.print(ks.ErrorVar ++ "(", .{});
      @call(.always_inline, display, .{err.val});
      util.print(")", .{});
    },
    .objvalmap => {
      asMap(val).meta.display();
    },
    .objclass => {
      util.print("{s}", .{asClass(val).nameStr()});
    },
    .objstruct => {
      const st = asStruct(val);
      util.print("{s}", .{st.nameStr()});
      if (st.mlen > 0) {
        util.print("(", .{});
        const stop = st.mlen -| 1;
        for (st.methods[0..st.mlen], 0..) |vl, i| {
          @call(.always_inline, display, .{vl});
          if (i < stop) {
            util.print(", ", .{});
          }
        }
        util.print(")", .{});
      }
    },
    .objtag => {
      util.print("{s}", .{asTag(val).nameStr()});
    },
    .objinstance => {
      util.print("{{{s} instance}}", .{asObj(val).cls.?.nameStr()});
    },
    .objmethod => {
      var mtd = asMethod(val);
      if (mtd.isBoundUserMethod()) {
        util.print("{{bound-method {s}}}", .{mtd.as.user.closure.fun.getName()});
      } else {
        util.print("{{bound-b.method {s}}}", .{mtd.as.native.fun.getName()});
      }
    },
    .objclosure => {
      util.print("{{fn {s}}}", .{asClosure(val).fun.getName()});
    },
    .objupvalue => {
      util.print("{{upvalue}}", .{});
    },
    .objnativefn => {
      util.print("{{b.fn {s}}}", .{asNativeFn(val).getName()});
    },
    .objfiber => {
      util.print("{{fiber}}", .{});
    },
    .objfn => {
      util.print("{{fn {s}}}", .{asFn(val).getName()});
    },
  }
}

inline fn writeValues(items: []Value, last: usize, vm: *VM, uw: *ValueStringWriter.Writer) void {
  for (items, 0..) |itm, i| {
    if (isString(itm)) {
      uw.write("'");
    }
    _valToString(itm, vm, uw);
    if (isString(itm)) {
      uw.write("'");
    }
    if (i < last) {
      uw.write(", ");
    }
  }
}

fn _objToString(val: Value, vm: *VM, uw: *ValueStringWriter.Writer) void {
  switch (asObj(val).id) {
    .objstring => {
      uw.write(asString(val).string());
    },
    .objvalmap => {
      uw.write("{");
      var map = asMap(val);
      const last = map.meta.len -| 1;
      for (map.meta.items[0..map.meta.len], 0..) |itm, i| {
        _valToString(itm.key, vm, uw);
        uw.write(": ");
        _valToString(itm.value, vm, uw);
        if (i < last) {
          uw.write(", ");
        }
      }
      uw.write("}");
    },
    .objlist => {
      uw.write("[");
      var list = asList(val);
      @call(.always_inline, writeValues, .{list.items[0..list.len], list.len -| 1, vm, uw});
      uw.write("]");
    },
    .objtuple => {
      uw.write("(");
      var tuple = asTuple(val);
      @call(.always_inline, writeValues, .{tuple.items[0..tuple.len], tuple.len -| 1, vm, uw});
      uw.write(")");
    },
    .objerror => {
      uw.write("Error(");
      _valToString(asError(val).val, vm, uw);
      uw.write(")");
    },
    .objclosure => {
      uw.write("{fn ");
      uw.write(asFn(val).getName());
      uw.write("}");
    },
    .objnativefn => {
      uw.write("{builtin_fn ");
      uw.write(asNativeFn(val).getName());
      uw.write("}");
    },
    .objfiber => {
      uw.write("<fiber>");
    },
    .objinstance => {
      uw.write("{");
      uw.write(asObj(val).cls.?.name.string());
      uw.write(" instance}");
    },
    .objmethod => {
      uw.write("{bound-method ");
      var mtd = asMethod(val);
      if (mtd.isBoundUserMethod()) {
        uw.write(mtd.as.user.closure.fun.getName());
      } else {
        uw.write(mtd.as.native.fun.getName());
      }
      uw.write("}");
    },
    .objclass => {
      uw.write("{class ");
      uw.write(asClass(val).nameStr());
      uw.write("}");
    },
    .objstruct => {
      var stk = asStruct(val);
      uw.write(stk.nameStr());
      uw.write("(");
      @call(.always_inline, writeValues, .{stk.methods[0..stk.mlen], stk.mlen -| 1, vm, uw});
      uw.write(")");
    },
    .objtag => {
      uw.write(asTag(val).name.string());
    },
    .objupvalue, .objfn => unreachable,
  }
}

fn _valToString(val: Value, vm: *VM, uw: *ValueStringWriter.Writer) void {
  if (isObj(val)) {
    _objToString(val, vm, uw);
  } else if (isNumber(val)) {
    const num = asNumber(val);
    if (std.math.isNan(num)) {
      uw.write("nan");
    } else if (std.math.isInf(num)) {
      uw.write(if (num > 0) "inf" else "-inf");
    } else {
      var buff: [25]u8 = undefined;
      const fmt = std.fmt.bufPrint(&buff, "{d}", .{num}) catch unreachable;
      uw.write(fmt);
    }
  } else if (isBool(val)) {
    uw.write(if (asBool(val)) ks.TrueVar else ks.FalseVar);
  } else if (isNil(val)) {
    uw.write(ks.NoneVar);
  }
}

/// stringify an object value
pub fn objectToString(val: Value, vm: *VM) Value {
  // no allocations if we're already dealing with a string
  if (isString(val)) return val;
  var writer = vm.vsw.writer(vm);
  _objToString(val, vm, &writer);
  defer {
    if (vm.vsw.capacity() > Mem.MAX_VALUE_STRING_CAP) {
      vm.vsw.deinit(vm);
    }
  }
  return objVal(createString(vm, &vm.strings, vm.vsw.items(), false));
}

/// stringify any value
pub fn valueToString(val: Value, vm: *VM) Value {
  // no allocations if we're already dealing with a string
  if (isString(val)) return val;
  var writer = vm.vsw.writer(vm);
  _valToString(val, vm, &writer);
  defer {
    if (vm.vsw.capacity() > Mem.MAX_VALUE_STRING_CAP) {
      vm.vsw.deinit(vm);
    }
  }
  return objVal(createString(vm, &vm.strings, vm.vsw.items(), false));
}

pub fn hashString(str: []const u8) u64 {
  // FNV-1a hashing algorithm
  @setRuntimeSafety(false);
  var hash: u128 = 2166136261;
  const fnv_prime: u32 = 16777619;
  const len = if (str.len > MAX_STR_HASHING_LEN) MAX_STR_HASHING_LEN else str.len;
  for (0..len) |i| {
    hash = hash ^ @as(u8, str[i]);
    hash = hash * fnv_prime;
  }
  return @truncate(hash);
}

pub fn hashBits(val: Value) u64 {
  // from Wren,
  // adapted from http://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm
  @setRuntimeSafety(false);
  var hash = val;
  hash = ~hash + (hash << 18);
  hash = hash ^ (hash >> 31);
  hash = hash * 21;
  hash = hash ^ (hash >> 11);
  hash = hash + (hash << 6);
  hash = hash ^ (hash >> 22);
  return @truncate((hash & 0x3fffffff));
}

pub fn hashObject(val: Value, vm: *VM) u64 {
  return switch (asObj(val).id) {
    .objstring => asString(val).hash,
    .objclass, .objstruct => {
      const it = asClass(val);
      return it.name.hash ^ hashBits(@intCast(it.mlen));
    },
    .objinstance => {
      const inst = asInstance(val);
      return inst.obj.cls.?.name.hash ^ hashBits(@intCast(inst.flen));
    },
    .objtag => asTag(val).name.hash,
    .objtuple => hashBits(val) ^ hashBits(asTuple(val).len),
    .objclosure => {
      const clo = asClosure(val);
      return clo.fun.name.hash ^ hashBits(clo.fun.arity) ^ hashBits(clo.fun.code.words.len);
    },
    .objnativefn => {
      const fun = asNativeFn(val);
      return hashString(fun.getName()) ^ hashBits(fun.arity);
    },
    .objmethod => {
      const fun = asMethod(val);
      return (
        if (fun.isBoundNativeMethod())
          hashObject(objVal(fun.as.native.fun), vm) ^ hashObject(fun.as.native.instance, vm)
        else 
          hashObject(objVal(fun.as.user.closure), vm) ^ hashObject(fun.as.user.instance, vm)
      );
    },
    else => {
      vm.panicUnwindError("unhashable type: '{s}'", .{asString(objectToString(val, vm)).string()});
      return 0;
    }
  };
}

pub fn hashValue(val: Value, vm: *VM) u64 {
  if (isObj(val)) return hashObject(val, vm);
  return hashBits(val);
}

pub const VM = @import("vm.zig").VM;

pub fn createObject(vm: *VM, id: ObjId, cls: ?*ObjClass, comptime T: type) *T {
  var mem = vm.mem.alloc(T, vm);
  mem.obj.id = id;
  mem.obj.cls = cls;
  mem.obj.next = vm.objects;
  vm.objects = &mem.obj;
  return mem;
}

pub fn createString(vm: *VM, map: *StringHashMap, str: []const u8, is_alloc: bool) *const ObjString {
  const hash = hashString(str);
  const string = map.findInterned(str, hash);
  if (string == null) {
    var tmp = @call(.always_inline, createObject, .{vm, .objstring, vm.classes.string, ObjString});
    tmp.len = str.len;
    tmp.hash = hash;
    if (!is_alloc) {
      tmp.str = vm.mem.dupeStr(vm, str).ptr;
    } else {
      tmp.str = str.ptr;
      vm.gc.bytes_allocated += str.len;
    }
    _ = map.set(tmp, FALSE_VAL, vm);
    return tmp;
  } else if (is_alloc) {
    vm.gc.allocator.getAllocator().free(str);
  }
  return string.?.key;
}

pub fn createList(vm: *VM, len: usize) *ObjList {
  const cap = Mem.alignTo(Mem.growCapacity(len), Mem.BUFFER_INIT_SIZE);
  var list = @call(.always_inline, createObject, .{vm, .objlist, vm.classes.list, ObjList});
  list.items = @ptrCast(vm.mem.allocBuf(Value, cap, vm));
  list.capacity = cap;
  list.len = len;
  return list;
}

pub fn createMap(vm: *VM, len: usize) *ObjMap {
  var map = @call(.always_inline, createObject, .{vm, .objvalmap, vm.classes.map, ObjMap});
  map.meta = ValueHashMap.init();
  map.meta.ensureCapacity(vm, len);
  return map;
}

pub fn createTuple(vm: *VM, len: usize) *ObjTuple {
  var tuple = @call(.always_inline, createObject, .{vm, .objtuple, vm.classes.tuple, ObjTuple});
  tuple.items = @ptrCast(vm.mem.allocBuf(Value, len, vm));
  tuple.len = len;
  return tuple;
}

pub inline fn createStringV(vm: *VM, map: *StringHashMap, str: []const u8, is_alloc: bool) Value {
  return objVal(createString(vm, map, str, is_alloc));
}

pub fn createFn(vm: *VM, arity: u8) *ObjFn {
  var fun = @call(.always_inline, createObject, .{vm, .objfn, null, ObjFn});
  fun.code = Code.init();
  fun.name = undefined;
  fun.arity = arity;
  fun.envlen = 0;
  return fun;
}

pub fn createScriptFn(vm: *VM, arity: u8) *ObjFn {
  const fun = createFn(vm, arity);
  fun.name = createString(vm, &vm.strings, ks.ScriptVar, false);
  return fun;
}

pub fn createNativeFn(vm: *VM, zfun: NativeFn, arity: u32, name: usize) *ObjNativeFn {
  var fun = @call(.always_inline, createObject, .{vm, .objnativefn, null, ObjNativeFn});
  fun.fun = zfun;
  fun.arity = arity;
  fun.name = name;
  return fun;
}

pub fn createClosure(vm: *VM, fun: *ObjFn) *ObjClosure {
  const env = vm.mem.allocBuf(*ObjUpvalue, fun.envlen, vm);
  var clos = @call(.always_inline, createObject, .{vm, .objclosure, null, ObjClosure});
  clos.env = env.ptr;
  clos.fun = fun;
  return clos;
}

pub fn createUpvalue(vm: *VM, loc: *Value) *ObjUpvalue {
  var upv = @call(.always_inline, createObject, .{vm, .objupvalue, null, ObjUpvalue});
  upv.next = null;
  upv.loc = loc;
  return upv;
}

pub fn createError(vm: *VM, val: Value) *ObjError {
  var err = @call(.always_inline, createObject, .{vm, .objerror, vm.classes.err, ObjError});
  err.val = val;
  return err;
}

pub fn createClass(vm: *VM, mlen: usize) *ObjClass {
  const methods = vm.mem.allocBuf(Value, mlen, vm);
  var cls = @call(.always_inline, createObject, .{vm, .objclass, null, ObjClass});
  cls.mlen = mlen;
  cls.methods = methods.ptr;
  cls.name = undefined;
  return cls;
}

pub fn createStruct(vm: *VM, flen: usize) *ObjStruct {
  var strukt = createClass(vm, flen);
  strukt.obj.id = .objstruct;
  return strukt;
}

pub fn createTag(vm: *VM, name: []const u8) Value {
  const str = createString(vm, &vm.strings, name, false);
  if (vm.tags.findInterned(str.string(), str.hash)) |itm| {
    return itm.value;
  }
  const tag = @call(.always_inline, createObject, .{vm, .objtag, null, ObjTag});
  tag.name = str;
  _ = vm.tags.set(str, objVal(tag), vm);
  return objVal(tag);
}

pub fn createInstance(vm: *VM, cls: *ObjClass, flen: usize) *ObjInstance {
  const fields = vm.mem.allocBuf(Value, flen, vm);
  var inst = @call(.always_inline, createObject, .{vm, .objinstance, cls, ObjInstance});
  inst.fields = fields.ptr;
  inst.flen = flen;
  return inst;
}

pub fn createBoundNativeMethod(vm: *VM, instance: Value, func: *ObjNativeFn) *ObjMethod {
  var meth = @call(.always_inline, createObject, .{vm, .objmethod, null, ObjMethod});
  meth.as.native = @as(ObjMethod.BoundNativeFn, .{.instance = instance, .fun = func});
  meth.typ = .BoundNativeMethod;
  return meth;
}

pub fn createBoundUserMethod(vm: *VM, instance: Value, closure: *ObjClosure) *ObjMethod {
  var meth = @call(.always_inline, createObject, .{vm, .objmethod, null, ObjMethod});
  meth.as.user = @as(ObjMethod.BoundUserFn, .{.instance = instance, .closure = closure});
  meth.typ = .BoundUserMethod;
  return meth;
}

pub fn createFiber(vm: *VM, clo: ?*ObjClosure, origin: FiberOrigin, caller: ?*ObjFiber) *ObjFiber {
  const arity = if (clo != null) clo.?.fun.arity else 1;  // heuristic
  const cap = Mem.alignTo(arity + 0xff, Mem.BUFFER_INIT_SIZE);
  const frames = vm.mem.allocBuf(CallFrame, Mem.BUFFER_INIT_SIZE, vm);
  var stack = vm.mem.allocBuf(Value, cap, vm);
  var fiber = @call(.always_inline, createObject, .{vm, .objfiber, null, ObjFiber});
  fiber.errval = NOTHING_VAL;
  fiber.origin = origin;
  fiber.caller = caller;
  fiber.frame_cap = Mem.BUFFER_INIT_SIZE;
  fiber.stack_cap = cap;
  fiber.frame_len = 0;
  fiber.frames = frames.ptr;
  fiber.stack = stack.ptr;
  fiber.open_upvalues = null;
  fiber.fp = undefined;
  if (clo) |closure| {
    stack[0] = objVal(closure);
    fiber.appendFrame(closure, stack.ptr);
  }
  return fiber;
}

pub fn structVal(vm: *VM, val: Value, name: []const u8) Value {
  var just = createStruct(vm, 1);
  just.name = createString(vm, &vm.strings, name, false);
  just.methods[0] = val;
  return objVal(just);
}

pub fn justVal(vm: *VM, val: Value) Value {
  return @call(.always_inline, structVal, .{vm, val, ks.JustVar});
}

pub inline fn noneVal() Value {
  return NIL_VAL;
}
