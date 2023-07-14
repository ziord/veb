const std = @import("std");
const util = @import("util.zig");
const Mem = @import("mem.zig");
const Vec = @import("vec.zig").Vec;
const Map = @import("map.zig").Map;
const ZFnNames = @import("native.zig").ZFnNames;
const OpCode = @import("opcode.zig").OpCode;
pub const OpType = @import("lex.zig").OpType;

/// a register-encoded instruction
pub const Inst = u32;

pub const Code = extern struct {
  words: Vec(Inst),
  values: Vec(Value),
  lines: Vec(u32),

  const Self = @This();
  pub const _6bits: u32 = 0x3f;
  pub const _8bits: u32 = 0xff;
  pub const _9bits: u32 = 0x1ff;
  pub const _18bits: u32 = 0x3ffff;
  pub const _26bits: u32 = 0x3ffffff;
  pub const _32bits: u32 = 0xffffffff;
  pub const _sign: u32 = 0x20000;

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

  pub inline fn readSBX(word: u32) i32 {
    // op rx [sbx] 
    // 6  8   18
    const sbx = @intCast(i32, word & _18bits);
    return if (sbx & _sign == _sign) sbx - (1 << 18) else sbx;
  }

  pub fn writeValue(self: *Self, value: Value, vm: *VM) u32 {
    self.values.push(value, vm);
    return @intCast(u32, self.values.len - 1);
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
    return self.words.len - 1;
  }

  pub fn patch2ArgsJmp(self: *Self, index: usize) void {
    const inst = self.words.items[index];
    // get jmp_inst, arg1
    const first = (inst >> 26) & _6bits;
    const second = (inst >> 18) & _8bits;
    const real_offset = self.words.len - index - 1;
    checkOffset(real_offset, _18bits, "max jump offset exceeded");
    const new = (first << 26) | (second << 18) | real_offset;
    self.words.items[index] = @intCast(u32, new);
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

pub const ZFn = *const fn (*VM, argc: u32, args: u32) Value;
pub const StringNullKey = ObjString {.obj = .{.id = .objstring, .next = null}, .hash = 0, .str = "", .len = 0};
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
  objzfn,
  objfiber,
  objerror,
};

pub const Obj = extern struct {
  id: ObjId,
  next: ?*Obj,
};

pub const ObjString = extern struct {
  obj: Obj,
  hash: u64,
  len: usize,
  str: [*]const u8,
};

pub const ObjList = extern struct {
  obj: Obj,
  len: usize,
  capacity: usize,
  items: [*]Value,
};

pub const ObjTuple = extern struct {
  obj: Obj,
  len: usize,
  items: [*]Value,
};

pub const ObjMap = extern struct {
  obj: Obj,
  meta: ValueHashMap
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
  name: ?*ObjString,

  pub fn getName(self: *ObjFn) []const u8 {
    if (self.name) |name| {
      return name.str[0..name.len];
    }
    return "<lambda>";
  }
};

pub const ObjZFn = extern struct {
  obj: Obj,
  arity: u32,
  fun: ZFn,
  name: usize,

  pub fn getName(self: *ObjZFn) []const u8 {
    return ZFnNames[self.name];
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

pub const ObjFiber = extern struct {
  obj: Obj,
  errval: Value = NOTHING_VAL, // TODO: remove?
  origin: FiberOrigin,
  stack_cap: usize,
  frame_cap: usize,
  frame_len: usize,
  // sp: *Value,
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
    var frame = self.frames[self.frame_len];
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


pub inline fn numberVal(num: f64) Value {
  return @ptrCast(*const Value, &num).*;
}

pub inline fn asNumber(val: Value) f64 {
  return @bitCast(f64, val);
}

pub inline fn asIntNumber(comptime T: type, val: Value) T {
  @setRuntimeSafety(false);
  return @floatToInt(T, asNumber(val));
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
  return @intCast(Value, @intCast(u64, @ptrToInt(ptr)) | TAG_OBJECT);
}

pub inline fn asObj(val: Value) *Obj {
  return @intToPtr(*Obj, @intCast(u64, (val & ~TAG_OBJECT)));
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

pub inline fn isZFn(val: Value) bool {
  return isObjType(val, .objzfn);
}

pub inline fn isFiber(val: Value) bool {
  return isObjType(val, .objfiber);
}

pub inline fn isError(val: Value) bool {
  return isObjType(val, .objerror);
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

pub fn isZFnNoInline(val: Value) bool {
  return isZFn(val);
}

pub fn isFiberNoInline(val: Value) bool {
  return isFiber(val);
}

pub fn isErrorNoInline(val: Value) bool {
  return isError(val);
}

pub inline fn asString(val: Value) *ObjString {
  return @ptrCast(*ObjString, asObj(val));
}

pub inline fn asList(val: Value) *ObjList {
  return @ptrCast(*ObjList, asObj(val));
}

pub inline fn asMap(val: Value) *ObjMap {
  return @ptrCast(*ObjMap, asObj(val));
}

pub inline fn asTuple(val: Value) *ObjTuple {
  return @ptrCast(*ObjTuple, asObj(val));
}

pub inline fn asFn(val: Value) *ObjFn {
  return @ptrCast(*ObjFn, asObj(val));
}

pub inline fn asZFn(val: Value) *ObjZFn {
  return @ptrCast(*ObjZFn, asObj(val));
}

pub inline fn asClosure(val: Value) *ObjClosure {
  return @ptrCast(*ObjClosure, asObj(val));
}

pub inline fn asUpvalue(val: Value) *ObjUpvalue {
  return @ptrCast(*ObjUpvalue, asObj(val));
}

pub inline fn asFiber(val: Value) *ObjFiber {
  return @ptrCast(*ObjFiber, asObj(val));
}

pub inline fn asError(val: Value) *ObjError {
  return @ptrCast(*ObjError, asObj(val));
}

pub inline fn valueEqual(a: Value, b: Value) bool {
  if (isNumber(a) and isNumber(b)) return asNumber(a) == asNumber(b);
  return a == b;
}

pub inline fn valueFalsy(val: Value) bool {
  return (
    (isBool(val) and !asBool(val)) or isNil(val) or 
    (isNumber(val) and asNumber(val) == 0) or 
    (isString(val) and asString(val).len == 0) or
    (isList(val) and asList(val).len == 0) or
    (isMap(val) and asMap(val).meta.len == 0) or
    (isTuple(val) and asTuple(val).len == 0)
  );
}

pub fn display(val: Value) void {
  if (isString(val)) {
    var tmp = asString(val);
    util.print("\"{s}\"", .{tmp.str[0..tmp.len]});
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
    util.print("nil", .{});
  } else if (isObj(val)) {
    printObject(val);
  }
}

pub fn printObject(val: Value) void {
  const obj = asObj(val);
  switch (obj.id) {
    .objstring => {
      var tmp = asString(val);
      util.print("{s}", .{tmp.str[0..tmp.len]});
    },
    .objlist => {
      var list = asList(val);
      var add_comma = list.len > 1;
      var comma_end = if (add_comma) list.len - 1 else 0;
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
      var add_comma = tuple.len > 1;
      var comma_end = if (add_comma) tuple.len - 1 else 0;
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
      var err = asError(val);
      util.print("(", .{});
      @call(.always_inline, display, .{err.val});
      util.print(")!", .{});
    },
    .objvalmap => {
      asMap(val).meta.display();
    },
    .objclosure => {
      util.print("{{fn {s}}}", .{asClosure(val).fun.getName()});
    },
    .objupvalue => {
      util.print("{{upvalue}}", .{});
    },
    .objzfn => {
      util.print("{{b.fn {s}}}", .{asZFn(val).getName()});
    },
    .objfiber => {
      util.print("{{fiber}}", .{});
    },
    .objfn => {
      util.print("{{fn {s}}}", .{asFn(val).getName()});
    },
  }
}

pub fn objectToString(val: Value, vm: *VM) Value {
  // TODO: handle size overflow
  switch (asObj(val).id) {
    .objstring => return val,
    .objvalmap => {
      var buff: [20]u8 = undefined;
      var fmt = std.fmt.bufPrint(&buff, "@map[{}]", .{asMap(val).meta.len}) catch "";
      return createStringV(vm, &vm.strings, fmt, false);
    },
    .objlist => {
      var buff: [20]u8 = undefined;
      var fmt = std.fmt.bufPrint(&buff, "@list[{}]", .{asList(val).len}) catch "";
      return createStringV(vm, &vm.strings, fmt, false);
    },
    .objtuple => {
      var buff: [20]u8 = undefined;
      var fmt = std.fmt.bufPrint(&buff, "@tuple[{}]", .{asTuple(val).len}) catch "";
      return createStringV(vm, &vm.strings, fmt, false);
    },
    .objerror => {
      // TODO: val
      return createStringV(vm, &vm.strings, "@error[]", false);
    },
    .objclosure => {
      var buff: [30]u8 = undefined;
      var fmt = std.fmt.bufPrint(&buff, "@fn[{s}]", .{asFn(val).getName()}) catch "fn()";
      return createStringV(vm, &vm.strings, fmt, false);
    },
    .objzfn => {
      var buff: [30]u8 = undefined;
      var fmt = std.fmt.bufPrint(&buff, "@builtin_fn[{s}]", .{asZFn(val).getName()}) catch "builtin_fn()";
      return createStringV(vm, &vm.strings, fmt, false);
    },
    .objfiber => {
      return createStringV(vm, &vm.strings, "<fiber>", false);
    },
    .objupvalue, .objfn => unreachable,
  }
  unreachable;
}

pub fn valueToString(val: Value, vm: *VM) Value {
  if (isObj(val)) return objectToString(val, vm);
  if (isNumber(val)) {
    var num = asNumber(val);
    if (std.math.isNan(num)) {
      return createStringV(vm, &vm.strings, "nan", false);
    } else if (std.math.isInf(num)) {
      return createStringV(vm, &vm.strings, if (num > 0) "inf" else "-inf", false);
    } else {
      // TODO
      var buff: [25]u8 = undefined;
      var fmt = std.fmt.bufPrint(&buff, "{d}", .{num}) catch "";
      return createStringV(vm, &vm.strings, fmt, false);
    }
  } else if (isBool(val)) {
    return createStringV(vm, &vm.strings, if (asBool(val)) "true" else "false", false);
  } else if (isNil(val)) {
    return createStringV(vm, &vm.strings, "nil", false);
  }
  unreachable;
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
  return @truncate(u64, hash);
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
  return @truncate(u64, (hash & 0x3fffffff));
}

pub fn hashObject(val: Value) u64 {
  return switch (asObj(val).id) {
    .objstring => asString(val).hash,
    else => |id| std.debug.panic("unhashable type: '{}'", .{id})
  };
}

pub fn hashValue(val: Value) u64 {
  if (isObj(val)) return hashObject(val);
  return hashBits(val);
}

pub const VM = @import("vm.zig").VM;

pub fn createObject(vm: *VM, id: ObjId, comptime T: type) *T {
  var mem = vm.mem.alloc(T, vm);
  mem.obj.id = id;
  mem.obj.next = vm.objects;
  vm.objects = &mem.obj;
  return mem;
}

pub fn createString(vm: *VM, map: *StringHashMap, str: []const u8, is_alloc: bool) *const ObjString {
  const hash = hashString(str);
  var string = map.findInterned(str, hash);
  if (string == null) {
    var tmp = @call(.always_inline, createObject, .{vm, .objstring, ObjString});
    tmp.str = @ptrCast([*]const u8, str);
    tmp.hash = hash;
    tmp.len = str.len;
    if (!is_alloc) {
      var s = vm.mem.allocBuf(u8, str.len, vm);
      std.mem.copy(u8, s, str);
      tmp.str = @ptrCast([*]const u8, s);
    } else {
      vm.gc.bytes_allocated += str.len;
    }
    _ = map.put(tmp, FALSE_VAL, vm);
    string = tmp;
    return tmp;
  }
  return string.?;
}

pub fn createList(vm: *VM, len: usize) *ObjList {
  const cap = Mem.alignTo(Mem.growCapacity(len), Mem.BUFFER_INIT_SIZE);
  var list = @call(.always_inline, createObject, .{vm, .objlist, ObjList});
  list.items = @ptrCast([*]Value, vm.mem.allocBuf(Value, cap, vm));
  list.capacity = cap;
  list.len = len;
  return list;
}

pub fn createMap(vm: *VM, len: usize) *ObjMap {
  var map = @call(.always_inline, createObject, .{vm, .objvalmap, ObjMap});
  map.meta = ValueHashMap.init();
  map.meta.ensureCapacity(vm, len);
  return map;
}

pub fn createTuple(vm: *VM, len: usize) *ObjTuple {
  var tuple = @call(.always_inline, createObject, .{vm, .objtuple, ObjTuple});
  tuple.items = @ptrCast([*]Value, vm.mem.allocBuf(Value, len, vm));
  tuple.len = len;
  return tuple;
}

pub inline fn createStringV(vm: *VM, map: *StringHashMap, str: []const u8, is_alloc: bool) Value {
  return objVal(createString(vm, map, str, is_alloc));
}

pub fn createFn(vm: *VM, arity: u8) *ObjFn {
  var fun = @call(.always_inline, createObject, .{vm, .objfn, ObjFn});
  fun.code = Code.init();
  fun.name = null;
  fun.arity = arity;
  fun.envlen = 0;
  return fun;
}

pub fn createZFn(vm: *VM, zfun: ZFn, arity: u32, name: usize) *ObjZFn {
  var fun = @call(.always_inline, createObject, .{vm, .objzfn, ObjZFn});
  fun.fun = zfun;
  fun.arity = arity;
  fun.name = name;
  return fun;
}

pub fn createClosure(vm: *VM, fun: *ObjFn) *ObjClosure {
  var env = vm.mem.allocBuf(*ObjUpvalue, fun.envlen, vm);
  var clos = @call(.always_inline, createObject, .{vm, .objclosure, ObjClosure});
  clos.env = env.ptr;
  clos.fun = fun;
  return clos;
}

pub fn createUpvalue(vm: *VM, loc: *Value) *ObjUpvalue {
  var upv = @call(.always_inline, createObject, .{vm, .objupvalue, ObjUpvalue});
  upv.next = null;
  upv.loc = loc;
  return upv;
}

pub fn createError(vm: *VM, val: Value) *ObjError {
  var err = @call(.always_inline, createObject, .{vm, .objerror, ObjError});
  err.val = val;
  return err;
}

pub fn createFiber(vm: *VM, clo: ?*ObjClosure, origin: FiberOrigin, caller: ?*ObjFiber) *ObjFiber {
  var arity = if (clo != null) clo.?.fun.arity else 1;  // heuristic
  const cap = Mem.alignTo(arity + 0xff, Mem.BUFFER_INIT_SIZE);
  var frames = vm.mem.allocBuf(CallFrame, Mem.BUFFER_INIT_SIZE, vm);
  var stack = vm.mem.allocBuf(Value, cap, vm);
  var fiber = @call(.always_inline, createObject, .{vm, .objfiber, ObjFiber});
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
    // + 1 for closure
    fiber.appendFrame(closure, stack.ptr + 1);
  }
  return fiber;
}
