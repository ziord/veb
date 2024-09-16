const std = @import("std");
const ks = @import("constants.zig");
const vl = @import("value.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;

const Value = vl.Value;
const NativeFn = vl.NativeFn;
const VOID_VAL = vl.VOID_VAL;
const NONE_VAL = vl.NIL_VAL;
const TRUE_VAL = vl.TRUE_VAL;
const FALSE_VAL = vl.FALSE_VAL;
const VarArgC = 256;


/// ********************
/// > helpers
/// ********************

inline fn addNativeFn(vm: *VM, name: []const u8, arity: u32, func: NativeFn) void {
  _ = vm.globals.set(
    vl.createString(vm, &vm.strings, name, false),
    vl.objVal(vl.createNativeFn(vm, func, arity, name)),
    vm
  );
}

inline fn addExternFn(vm: *VM, name: []const u8, arity: u32, func: NativeFn) void {
  _ = vm.externs.push(
    vl.objVal(vl.createNativeFn(vm, func, arity, name)),
    vm
  );
}

inline fn addNativeClass(vm: *VM, cls: *vl.ObjClass) void {
  _ = vm.globals.set(cls.name, vl.objVal(cls), vm);
}

inline fn newString(vm: *VM, chars: []const u8) *vl.ObjString {
  return vl.createString(vm, &vm.strings, chars, false);
}

inline fn newStringV(vm: *VM, chars: []const u8) Value {
  return vl.createStringV(vm, &vm.strings, chars, false);
}

inline fn getArg(vm: *VM, pos: u32) Value {
  return vm.fiber.fp.stack[pos];
}

/// ******************************
/// > barebones builtin functions
/// ******************************

/// assert(cond: bool, msg: str): void | noreturn
pub fn fnAssert(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  if (vl.valueFalsy(getArg(vm, args))) {
    var msg = vl.asString(vl.valueToString(getArg(vm, args + 1), vm));
    vm.panicUnwindError("AssertionError: '{s}'", .{msg.str[0..msg.len]});
    return FALSE_VAL;
  }
  return VOID_VAL;
}

/// exit(code: num): noreturn
pub fn fnExit(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const code = vl.asIntNumber(u8, getArg(vm, args));
  vm.deinit();
  std.posix.exit(code);
}

/// panic{T}(msg: T): noreturn
pub fn fnPanic(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var msg = vl.asString(vl.valueToString(getArg(vm, args), vm));
  vm.panicUnwindError("panicked at: '{s}'", .{msg.str[0..msg.len]});
  vm.deinit();
  std.posix.exit(1);
}

/// print(args*: any): void
pub fn fnPrint(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var tup = vl.asList(getArg(vm, args));
  for (tup.items[0..tup.len], 1..) |val, i| {
    vl.printValue(val);
    if (i < tup.len) {
      util.print(" ", .{});
    }
  }
  return VOID_VAL;
}

/// @string(val: any): str
pub fn fnString(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.valueToString(getArg(vm, args), vm);
}

/// println(args*: any): void
pub fn fnPrintln(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var tup = vl.asList(getArg(vm, args));
  for (tup.items[0..tup.len], 1..) |val, i| {
    vl.printValue(val);
    if (i < tup.len) {
      util.print(" ", .{});
    }
  }
  util.print("\n", .{});
  return VOID_VAL;
}

/// ****************************
/// > barebones builtin classes
/// ****************************

//******** str ********//

// len(): num
fn stringLen(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberIntVal(vl.asString(getArg(vm, args)).len);
}

// concat(other: str): str
fn stringConcat(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args));
  const conc = str.concat(vl.asString(getArg(vm, args + 1)), vm.gc.allocator);
  return vl.createStringV(vm, &vm.strings, conc, true);
}

// slice(begin: num, end: num): str
fn stringSlice(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args));
  var begin = vl.asIntNumber(isize, getArg(vm, args + 1));
  var end = vl.asIntNumber(isize, getArg(vm, args + 2));
  if (begin < 0) begin += @intCast(str.len);
  if (end < 0) end += @intCast(str.len);
  if (begin < 0 or end < 0 or end < begin or begin >= str.len) {
    return newStringV(vm, "");
  }
  return newStringV(vm, str.slice(@intCast(begin), @intCast(end)));
}

// at(index: num): str
fn stringAt(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args));
  var index = vl.asIntNumber(isize, getArg(vm, args + 1));
  if (index < 0) index += @intCast(str.len);
  if (index < 0 or index >= str.len) {
    return newStringV(vm, "");
  }
  return newStringV(vm, &[_]u8{str.string()[@intCast(index)]});
}

// starts_with(substr: str): bool
fn stringStartsWith(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  return vl.boolVal(std.mem.startsWith(u8, str, sub));
}

// ends_with(substr: str): bool
fn stringEndsWith(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  return vl.boolVal(std.mem.endsWith(u8, str, sub));
}

// find(substr: str): Maybe{num}
fn stringFind(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  if (std.mem.indexOf(u8, str, sub)) |pos| {
    return vl.justVal(vm, vl.numberIntVal(pos));
  }
  return NONE_VAL;
}

// split(sep: str): List{str}
fn stringSplit(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  if (sub.len != 0) {
    var itr = std.mem.splitSequence(u8, str, sub);
    const root_len = vm.temp_roots.size();
    defer vl.delTempRoots(vm, root_len);
    while (itr.next()) |slice| {
      vm.temp_roots.push(newStringV(vm, slice), vm);
    }
    var list = vl.createList(vm, vm.temp_roots.size() - root_len);
    for (root_len .. root_len + list.len, 0..) |i, j| {
      list.items[j] = vm.temp_roots.items[i];
    }
    return vl.objVal(list);
  } else {
    const list = vl.createList(vm, str.len);
    const root_len = vl.saveTempRoot(vm, vl.objVal(list));
    defer vl.delTempRoots(vm, root_len);
    for (str, 0..) |ch, i| {
      list.items[i] = newStringV(vm, &[_]u8{ch});
    }
    return vl.objVal(list);
  }
}

// strip(substr: str): str
fn stringStrip(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  if (sub.len > str.len) {
    return getArg(vm, args);
  }
  while (std.mem.startsWith(u8, str, sub)) {
    str = str[sub.len..];
  }
  while (std.mem.endsWith(u8, str, sub)) {
    str = str[0..str.len - sub.len];
  }
  return newStringV(vm, str);
}

// lstrip(substr: str): str
fn stringLstrip(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  if (sub.len > str.len) {
    return getArg(vm, args);
  }
  while (std.mem.startsWith(u8, str, sub)) {
    str = str[sub.len..];
  }
  return newStringV(vm, str);
}

// rstrip(substr: str): str
fn stringRstrip(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  if (sub.len > str.len) {
    return getArg(vm, args);
  }
  while (std.mem.endsWith(u8, str, sub)) {
    str = str[0..str.len - sub.len];
  }
  return newStringV(vm, str);
}

// is_alpha(): bool
fn stringIsAlpha(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  for (str) |ch| {
    if (!std.ascii.isAlphabetic(ch)) {
      return FALSE_VAL;
    }
  }
  return TRUE_VAL;
}

// is_digit(): bool
fn stringIsDigit(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  for (str) |ch| {
    if (!std.ascii.isDigit(ch)) {
      return FALSE_VAL;
    }
  }
  return TRUE_VAL;
}

// is_alnum(): bool
fn stringIsAlnum(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  for (str) |ch| {
    if (!std.ascii.isAlphanumeric(ch)) {
      return FALSE_VAL;
    }
  }
  return TRUE_VAL;
}

// is_ascii(): bool
fn stringIsAscii(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  for (str) |ch| {
    if (!std.ascii.isASCII(ch)) {
      return FALSE_VAL;
    }
  }
  return TRUE_VAL;
}

// is_lower(): bool
fn stringIsLower(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  for (str) |ch| {
    if (!std.ascii.isLower(ch)) {
      return FALSE_VAL;
    }
  }
  return TRUE_VAL;
}

// is_upper(): bool
fn stringIsUpper(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  for (str) |ch| {
    if (!std.ascii.isUpper(ch)) {
      return FALSE_VAL;
    }
  }
  return TRUE_VAL;
}

// lower(): str
fn stringLower(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const buf = vm.mem.allocBuf(u8, str.len, vm);
  const ret = std.ascii.lowerString(buf, str);
  return vl.createStringV(vm, &vm.strings, ret, true);
}

// upper(): str
fn stringUpper(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const buf = vm.mem.allocBuf(u8, str.len, vm);
  const ret = std.ascii.upperString(buf, str);
  return vl.createStringV(vm, &vm.strings, ret, true);
}

// count(substr: str): num
fn stringCount(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  return vl.numberIntVal(std.mem.count(u8, str, sub));
}

// contains(substr: str): bool
fn stringContains(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args)).string();
  const sub = vl.asString(getArg(vm, args + 1)).string();
  return vl.boolVal(std.mem.containsAtLeast(u8, str, 1, sub));
}

fn createStringClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {
    stringLen, stringConcat, stringSlice, stringAt, stringStartsWith, stringEndsWith,
    stringFind, stringSplit, stringStrip, stringLstrip, stringRstrip, stringIsAlpha,
    stringIsDigit, stringIsAlnum, stringIsAscii, stringIsLower, stringIsUpper,
    stringLower, stringUpper, stringCount, stringContains,
  };
  //*** arity of each method ***//
  const arities = [_]u32 {
    0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
  };
  const names = [_][]const u8{
    "len", "concat", "slice", "at", "starts_with", "ends_with",
    "find", "split", "strip", "lstrip", "rstrip", "is_alpha",
    "is_digit", "is_alnum", "is_ascii", "is_lower", "is_upper",
    "lower", "upper", "count", "contains",
  };
  // important to do this first
  var cls = vl.createClass(vm, methods.len);
  vm.classes.string = cls;
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.items[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.StrVar);
  return cls;
}

//******** list ********//

// append(item: T): void
fn listAppend(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  vl.asList(getArg(vm, args)).append(vm, getArg(vm, args + 1));
  return VOID_VAL;
}

// len(): num
fn listLen(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberIntVal((vl.asList(getArg(vm, args)).len));
}

// pop(): Maybe{T}
fn listPop(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var list = vl.asList(getArg(vm, args));
  if (list.len == 0) {
    return NONE_VAL;
  }
  const val = list.items[list.len - 1];
  list.len -= 1;
  return vl.justVal(vm, val);
}

// get(index: num): Maybe{T}
fn listGet(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  if (list.len == 0) return NONE_VAL;
  var idx = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (idx < 0) idx += @intCast(list.len);
  if (idx >= list.len or idx < 0) return NONE_VAL;
  return vl.justVal(vm, list.items[@intCast(idx)]);
}

// slice(start: num, end: num): List{T}
fn listSlice(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  if (list.len == 0) return NONE_VAL;
  var s_idx = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (s_idx < 0) s_idx += @intCast(list.len);
  var e_idx = vl.asIntNumber(i64, getArg(vm, args + 2));
  if (e_idx < 0) e_idx += @intCast(list.len);
  if (e_idx <= s_idx or s_idx < 0 or e_idx < 0) {
    return vl.objVal(vl.createList(vm, 0));
  }
  const len: usize = @intCast(e_idx - s_idx);
  var res = vl.createList(vm, len);
  @memcpy(res.items[0..len], list.items[@intCast(s_idx)..@intCast(e_idx)]);
  return vl.objVal(res);
}

// clear(): void
fn listClear(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  vl.asList(getArg(vm, args)).clear(vm);
  return VOID_VAL;
}

// copy(): List{T}
fn listCopy(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.asList(getArg(vm, args)).copy(vm);
}

// extend(list: List{T}): void
fn listExtend(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  vl.asList(getArg(vm, args)).extend(vl.asList(getArg(vm, args+1)), vm);
  return VOID_VAL;
}

// remove(index: num): bool
fn listRemove(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  if (list.len == 0) return FALSE_VAL;
  var idx = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (idx < 0) idx += @intCast(list.len);
  if (idx < 0 or idx >= list.len) return FALSE_VAL;
  return list.remove(@intCast(idx));
}

// reverse(): void
fn listReverse(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  std.mem.reverse(Value, list.items[0..list.len]);
  return VOID_VAL;
}


fn createListClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  // NOTE: Methods are set according to the order in prelude
  const methods = [_]NativeFn {
    listAppend, listLen, listPop, listGet, listSlice, listClear, listCopy,
    listExtend, listRemove, listReverse
  };
  //*** arity of each method ***//
  const arities = [_]u32 {1, 0, 0, 1, 2, 0, 0, 1, 1, 0};
  const names = [_][]const u8{
    "append", "len", "pop", "get", "slice", "clear", "copy", "extend",
    "remove", "reverse",
  };
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.items[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.ListVar);
  vm.classes.list = cls;
  return cls;
}

//******** tuple ********//

// len(): num
fn tupleLen(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberIntVal(vl.asTuple(getArg(vm, args)).len);
}

fn createTupleClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {tupleLen};
  //*** arity of each method ***//
  const arities = [_]u32 {VarArgC};
  const names = [_][]const u8{"len"};
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.items[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.TupleVar);
  vm.classes.tuple = cls;
  return cls;
}

//******** map ********//

// len(): num
fn mapLen(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberIntVal(vl.asMap(getArg(vm, args)).meta.len);
}

// set(key: K, value: V): bool
fn mapSet(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  const res = map.meta.set(getArg(vm, args + 1), getArg(vm, args + 2), vm);
  return vl.boolVal(res);
}

// get(key: K): Maybe{V}
fn mapGet(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  if (vl.asMap(getArg(vm, args)).meta.get(getArg(vm, args + 1), vm)) |res| {
    return vl.justVal(vm, res);
  }
  return NONE_VAL;
}

// delete(key: K): bool
fn mapDelete(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const val = vl.asMap(getArg(vm, args)).meta.delete(getArg(vm, args + 1), vm);
  return vl.boolVal(val);
}

// remove(key: K): bool
fn mapRemove(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const val = vl.asMap(getArg(vm, args)).meta.remove(getArg(vm, args + 1), vm);
  return vl.boolVal(val);
}

// keys(): List{K}
fn mapKeys(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.keys(vm);
}

// values(): List{V}
fn mapValues(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.values(vm);
}

// items(): List{Tuple{K, V}}
fn mapItems(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var list = vl.createList(vm, map.meta.len);
  const root_len = vl.saveTempRoot(vm, vl.objVal(list));
  defer vl.delTempRoots(vm, root_len);
  var valc: usize = 0;
  for (map.meta.items[0..map.meta.len]) |item| {
    var tuple = vl.createTuple(vm, 2);
    tuple.items[0] = item.key;
    tuple.items[1] = item.value;
    list.items[valc] = vl.objVal(tuple);
    valc += 1;
  }
  return vl.objVal(list);
}

// entries(): List{MapEntry{K, V}} -> List{Key(K) | Value(V)}
fn mapEntries(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var list = vl.createList(vm, map.meta.len << 1);
  const root_len = vl.saveTempRoot(vm, vl.objVal(list));
  defer vl.delTempRoots(vm, root_len);
  var kvc: usize = 0;
  for (map.meta.items[0..map.meta.len]) |item| {
    list.items[kvc] = vl.structVal(vm, item.key, ks.KeyVar);
    list.items[kvc + 1] = vl.structVal(vm, item.value, ks.ValueVar);
    kvc += 2;
  }
  return vl.objVal(list);
}

// clear(): void
fn mapClear(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  vl.asMap(getArg(vm, args)).meta.clear(vm);
  return VOID_VAL;
}

// copy(): Map{K, V}
fn mapCopy(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.copy(vm);
}

// pop(key: K): Maybe{V}
fn mapPop(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.pop(getArg(vm, args + 1), vm);
}

fn createMapClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {
    mapSet, mapGet, mapDelete, mapRemove, mapKeys, mapValues, mapItems,
    mapEntries, mapLen, mapClear, mapCopy, mapPop,
  };
  //*** arity of each method ***//
  const arities = [_]u32 {2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1};
  const names = [_][]const u8{
    "set", "get", "delete", "remove", "keys", "values",
    "items", "entries", "len", "clear", "copy", "pop",
  };
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.items[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.MapVar);
  vm.classes.map = cls;
  return cls;
}

//******** err ********//

fn createErrorClass(vm: *VM) *vl.ObjClass {
  var cls = vl.createClass(vm, 0);
  cls.name = newString(vm, ks.ErrorVar);
  vm.classes.err = cls;
  return cls;
}

//*********Externs**********//

//******** [math] *********//
// ceil(n: num): num
fn mathCeil(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.ceil(vl.asNumber(getArg(vm, args))));
}

// floor(n: num): num
fn mathFloor(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.floor(vl.asNumber(getArg(vm, args))));
}

// cos(n: num): num
fn mathCos(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.cos(vl.asNumber(getArg(vm, args))));
}

// sin(n: num): num
fn mathSin(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.sin(vl.asNumber(getArg(vm, args))));
}

// tan(n: num): num
fn mathTan(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.tan(vl.asNumber(getArg(vm, args))));
}

// acos(n: num): num
fn mathAcos(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.acos(vl.asNumber(getArg(vm, args))));
}

// asin(n: num): num
fn mathAsin(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.asin(vl.asNumber(getArg(vm, args))));
}

// atan(n: num): num
fn mathAtan(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.atan(vl.asNumber(getArg(vm, args))));
}

// cosh(n: num): num
fn mathCosh(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.cosh(vl.asNumber(getArg(vm, args))));
}

// sinh(n: num): num
fn mathSinh(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.sinh(vl.asNumber(getArg(vm, args))));
}

// tanh(n: num): num
fn mathTanh(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.tanh(vl.asNumber(getArg(vm, args))));
}

// acosh(n: num): num
fn mathAcosh(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.acosh(vl.asNumber(getArg(vm, args))));
}

// asinh(n: num): num
fn mathAsinh(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.asinh(vl.asNumber(getArg(vm, args))));
}

// atanh(n: num): num
fn mathAtanh(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.atanh(vl.asNumber(getArg(vm, args))));
}

// sqrt(n: num): num
fn mathSqrt(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.sqrt(vl.asNumber(getArg(vm, args))));
}

// deg_to_radians(n: num): num
fn mathDegtoradians(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.degreesToRadians(vl.asNumber(getArg(vm, args))));
}

// rad_to_degrees(n: num): num
fn mathRadtodegrees(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.radiansToDegrees(vl.asNumber(getArg(vm, args))));
}

// pow(x: num, y: num): num
fn mathPow(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.pow(f64, vl.asNumber(getArg(vm, args)), vl.asNumber(getArg(vm, args + 1))));
}

// log(x: num, base: num): num
fn mathLog(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberVal(std.math.log(f64, vl.asNumber(getArg(vm, args)), vl.asNumber(getArg(vm, args + 1))));
}

// min(x: num, y: num): num
fn mathMin(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const x = vl.asNumber(getArg(vm, args));
  const y = vl.asNumber(getArg(vm, args + 1));
  return vl.numberVal(if (x < y) x else y);
}

// max(x: num, y: num): num
fn mathMax(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const x = vl.asNumber(getArg(vm, args));
  const y = vl.asNumber(getArg(vm, args + 1));
  return vl.numberVal(if (x > y) x else y);
}

fn roundNumber(num: f64, digits: i64) f64 {
  if (digits == 0) return @round(num);
  const multiplier: f64 = @floatFromInt(std.math.pow(i64, 10, digits));
  var tmp = num * multiplier;
  if (tmp >= 0) {
    tmp = @floor(tmp + 0.5);
  } else {
    tmp = @ceil(tmp - 0.5);
  }
  return tmp / multiplier;
}

// round(n: num, digits: num)
fn mathRound(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const digit = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (digit < 0) {
    vm.reportRuntimeError("round called with negative digit", .{});
    return 0;
  }
  return vl.numberVal(roundNumber(vl.asNumber(getArg(vm, args)), digit));
}

fn addMathExterns(vm: *VM) void {
  addExternFn(vm, "ceil", 1, mathCeil);
  addExternFn(vm, "floor", 1, mathFloor);
  addExternFn(vm, "cos", 1, mathCos);
  addExternFn(vm, "sin", 1, mathSin);
  addExternFn(vm, "tan", 1, mathTan);
  addExternFn(vm, "acos", 1, mathAcos);
  addExternFn(vm, "asin", 1, mathAsin);
  addExternFn(vm, "atan", 1, mathAtan);
  addExternFn(vm, "cosh", 1, mathCosh);
  addExternFn(vm, "sinh", 1, mathSinh);
  addExternFn(vm, "tanh", 1, mathTanh);
  addExternFn(vm, "acosh", 1, mathAcosh);
  addExternFn(vm, "asinh", 1, mathAsinh);
  addExternFn(vm, "atanh", 1, mathAtanh);
  addExternFn(vm, "sqrt", 1, mathSqrt);
  addExternFn(vm, "deg_to_radians", 1, mathDegtoradians);
  addExternFn(vm, "rad_to_degrees", 1, mathRadtodegrees);
  addExternFn(vm, "pow", 2, mathPow);
  addExternFn(vm, "log", 2, mathLog);
  addExternFn(vm, "min", 2, mathMin);
  addExternFn(vm, "max", 2, mathMax);
  addExternFn(vm, "round", 2, mathRound);
}

//******** [fs.file] *********//
fn getOpenMode(tag: *vl.ObjTag) std.fs.File.OpenMode {
  if (tag.nameStr()[0] == 'R') { // Read
    return .read_only;
  } else if (tag.nameStr()[0] == 'W') { // Write
    return .write_only;
  } else { // Plus
    return .read_write;
  }
}

// returns Error(OpenError) -> Error(ObjTag)
fn getOpenError(vm: *VM, e: anyerror) Value {
  const tag: Value = switch (e) {
    error.FileNotFound,
    error.PathAlreadyExists,
    error.SharingViolation,
    error.BadPathName,
    error.NotDir,
    error.NoSpaceLeft,
    error.FileTooBig,
    error.AccessDenied,
    error.NameTooLong,
    error.PipeBusy,
    error.SymLinkLoop,
    error.InvalidUtf8,
    error.ReadOnlyFileSystem => vl.createTag(vm, @errorName(e)),
    else => vl.createTag(vm, "Unexpected"),
  };
  const root_len = vl.saveTempRoot(vm, tag);
  defer vl.delTempRoots(vm, root_len);
  return vl.createError(vm, tag);
}

// returns Error(ReadError) -> Error(ObjTag)
fn getReadError(vm: *VM, e: anyerror) Value {
  const tag: Value = switch (e) {
    error.InputOutput,
    error.SystemResources,
    error.IsDir,
    error.OperationAborted,
    error.BrokenPipe,
    error.ConnectionResetByPeer,
    error.ConnectionTimedOut,
    error.NotOpenForReading,
    error.SocketNotConnected,
    error.AccessDenied,
    error.EndOfFile => vl.createTag(vm, @errorName(e)),
    else => vl.createTag(vm, "Unexpected"),
  };
  const root_len = vl.saveTempRoot(vm, tag);
  defer vl.delTempRoots(vm, root_len);
  return vl.createError(vm, tag);
}

// returns Error(WriteError) -> Error(ObjTag)
fn getWriteError(vm: *VM, e: std.fs.File.WriteError) Value {
  const tag: Value = switch (e) {
    error.DiskQuota,
    error.FileTooBig,
    error.InputOutput,
    error.NoSpaceLeft,
    error.DeviceBusy,
    error.InvalidArgument,
    error.AccessDenied,
    error.BrokenPipe,
    error.SystemResources,
    error.OperationAborted,
    error.NotOpenForWriting => vl.createTag(vm, @errorName(e)),
    else => vl.createTag(vm, "Unexpected"),
  };
  const root_len = vl.saveTempRoot(vm, tag);
  defer vl.delTempRoots(vm, root_len);
  return vl.createError(vm, tag);
}

// returns Error(DeleteError) -> Error(ObjTag)
fn getDeleteError(vm: *VM, e: anyerror) Value {
  const tag: Value = switch (e) {
    error.DirNotEmpty,
    error.FileNotFound,
    error.BadPathName,
    error.NotDir,
    error.IsDir,
    error.AccessDenied,
    error.NameTooLong,
    error.FileBusy,
    error.SymLinkLoop,
    error.InvalidUtf8,
    error.ReadOnlyFileSystem => vl.createTag(vm, @errorName(e)),
    else => vl.createTag(vm, "Unexpected"),
  };
  const root_len = vl.saveTempRoot(vm, tag);
  defer vl.delTempRoots(vm, root_len);
  return vl.createError(vm, tag);
}

const stdin = std.io.getStdIn();
const MAX_STDIN_BYTES = 0xffff;

fn readFileHandle(file: std.fs.File, al: std.mem.Allocator) ![]const u8 {
  if (file.handle != stdin.handle) {
    const pos = try file.getPos();
    const size = try file.getEndPos();
    if (pos == size) {
      return error.EndOfFile;
    }
    const content = try al.allocSentinel(u8, size, 0);
    const got = try file.readAll(content);
    if (got != size) {
      al.free(content);
      return error.Unexpected;
    }
    return content;
  } else {
    var data = try file.readToEndAlloc(al, MAX_STDIN_BYTES);
    data[data.len - 1] = 0;
    return data[0..data.len - 1: 0];
  }
}

// open_file(path: str, mode: Mode): Result{Fd, OpenError}
fn fsFileOpen(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const path = vl.asString(getArg(vm, args)).string();
  const mode = getOpenMode(vl.asTag(getArg(vm, args + 1)));
  const fd = blk: {
    break :blk std.fs.cwd().openFile(path, .{.mode = mode}) catch |e| {
      if (mode == .write_only) {
        break :blk std.fs.cwd().createFile(path, .{}) catch |e2| {
          return getOpenError(vm, e2);
        };
      }
      return getOpenError(vm, e);
    };
  };
  return vl.okVal(vm, vl.numberIntVal(fd.handle));
}

// read_file(fd: Fd): Result{num, ReadError}
fn fsFileRead(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const fd = vl.asIntNumber(std.fs.File.Handle, getArg(vm, args));
  const content = readFileHandle(.{.handle = fd}, vm.gc.allocator) catch |e| {
    return getReadError(vm, e);
  };
  vm.gc.bytes_allocated += 1; // for sentinel
  const str = vl.createString(vm, &vm.strings, content, true);
  str.len += 1;
  const root_len = vl.saveTempRoot(vm, vl.objVal(str));
  defer vl.delTempRoots(vm, root_len);
  return vl.okVal(vm, vl.objVal(str));
}

// write_file(fd: Fd, s: str): Result{num, WriteError}
fn fsFileWrite(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var file: std.fs.File = .{.handle = vl.asIntNumber(std.fs.File.Handle, getArg(vm, args))};
  const size = file.write(vl.asString(getArg(vm, args + 1)).string()) catch |e| {
    return getWriteError(vm, e);
  };
  return vl.okVal(vm, vl.numberIntVal(size));
}

// close_file(fd: Fd): void
fn fsFileClose(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  std.fs.File.close(.{.handle = vl.asIntNumber(std.fs.File.Handle, getArg(vm, args))});
  return VOID_VAL;
}

// mkdir(path: str): Result{void, OpenError}
fn fsFileMakeDir(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const path = vl.asString(getArg(vm, args)).string();
  std.fs.cwd().makeDir(path) catch |e| {
    return getOpenError(vm, e);
  };
  return vl.okVal(vm, VOID_VAL);
}

// rmdir(path: str): Result{void, DeleteError}
fn fsFileRemoveDir(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const path = vl.asString(getArg(vm, args)).string();
  std.fs.cwd().deleteDir(path) catch |e| {
    return getDeleteError(vm, e);
  };
  return vl.okVal(vm, VOID_VAL);
}

// rmfile(path: str): Result{void, DeleteError}
fn fsFileRemoveFile(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const path = vl.asString(getArg(vm, args)).string();
  std.fs.cwd().deleteFile(path) catch |e| {
    return getDeleteError(vm, e);
  };
  return vl.okVal(vm, VOID_VAL);
}

// rename(curr: str, new: str): Result{void, RenameError}
fn fsFileRename(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const old = vl.asString(getArg(vm, args)).string();
  const new = vl.asString(getArg(vm, args + 1)).string();
  std.fs.cwd().rename(old, new) catch |e| {
    return getOpenError(vm, e);
  };
  return vl.okVal(vm, VOID_VAL);
}

// get_stderr(): Fd
fn fsFileGetStdErr(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = vm;
  _ = args;
  return vl.numberIntVal(std.io.getStdErr().handle);
}

// get_stdout(): Fd
fn fsFileGetStdOut(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = vm;
  _ = args;
  return vl.numberIntVal(std.io.getStdOut().handle);
}

// get_stdin(): Fd
fn fsFileGetStdIn(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = vm;
  _ = args;
  return vl.numberIntVal(stdin.handle);
}

// list_dir(path: str): Result{List{str}, OpenError}
fn fsFileListDir(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const path = vl.asString(getArg(vm, args)).string();
  var dir = std.fs.cwd().openDir(path, .{}) catch |e| {
    return getOpenError(vm, e);
  };
  defer dir.close();
  var itr = dir.iterate();
  const root_len = vm.temp_roots.size();
  defer vl.delTempRoots(vm, root_len);
  while (true) {
    if (itr.next()) |val| {
      if (val) |v| {
        vm.temp_roots.push(newStringV(vm, v.name), vm);
        continue;
      }
    } else |_| {}
    break;
  }
  var list = vl.createList(vm, vm.temp_roots.size() - root_len);
  if (list.len > 0) @memcpy(list.items[0..list.len], vm.temp_roots.items[root_len..]);
  return vl.okVal(vm, vl.objVal(list));
}

fn addFsFileExterns(vm: *VM) void {
  addExternFn(vm, "open_file", 2, fsFileOpen);
  addExternFn(vm, "read_file", 1, fsFileRead);
  addExternFn(vm, "write_file", 2, fsFileWrite);
  addExternFn(vm, "close_file", 1, fsFileClose);
  addExternFn(vm, "mkdir", 1, fsFileMakeDir);
  addExternFn(vm, "rmdir", 1, fsFileRemoveDir);
  addExternFn(vm, "rmfile", 1, fsFileRemoveFile);
  addExternFn(vm, "ls", 1, fsFileListDir);
  addExternFn(vm, "rename", 2, fsFileRename);
  addExternFn(vm, "get_stderr", 0, fsFileGetStdErr);
  addExternFn(vm, "get_stdout", 0, fsFileGetStdOut);
  addExternFn(vm, "get_stdin", 0, fsFileGetStdIn);
}

//******** [time] *********//
// sleep(secs: num): void
fn timeSleep(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  std.time.sleep(@intFromFloat(vl.asNumber(getArg(vm, args)) * std.time.ns_per_s));
  return VOID_VAL;
}

// millis(): num
fn timeMillis(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = vm;
  _ = args;
  return vl.numberIntVal(std.time.milliTimestamp());
}

// nanos(): num
fn timeNanos(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = vm;
  _ = args;
  return vl.numberIntVal(std.time.nanoTimestamp());
}

// now(): num
fn timeNow(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = vm;
  _ = args;
  return vl.numberIntVal(std.time.timestamp());
}

fn addTimeExterns(vm: *VM) void {
  addExternFn(vm, "sleep", 1, timeSleep);
  addExternFn(vm, "millis", 0, timeMillis);
  addExternFn(vm, "nanos", 0, timeNanos);
  addExternFn(vm, "now", 0, timeNow);
}

//******** [fs.path] *********//

// get_sep(): str
fn pathGetSep(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = args;
  return newStringV(vm, &[_]u8{std.fs.path.sep});
}

// basename(p: str): str
fn pathBasename(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return newStringV(vm, std.fs.path.basename(vl.asString(getArg(vm, args)).string()));
}

// dirname(p: str): str
fn pathDirname(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return newStringV(
    vm,
    std.fs.path.dirname(vl.asString(getArg(vm, args)).string()) orelse "",
  );
}

// extension(p: str): str
fn pathExtension(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return newStringV(vm, std.fs.path.extension(vl.asString(getArg(vm, args)).string()));
}

// exists(p: str): bool
fn pathExists(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const p = vl.asString(getArg(vm, args)).string();
  (
    if (std.fs.path.isAbsolute(p)) std.fs.accessAbsolute(p, .{})
    else std.fs.cwd().access(p, .{})
  ) catch return FALSE_VAL;
  return TRUE_VAL;
}

// realpath(p: str): str
fn pathRealpath(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const p = std.fs.realpathAlloc(vm.mem.allocator, vl.asString(getArg(vm, args)).string())
    catch return getArg(vm, args);
  return vl.createStringV(vm, &vm.strings, p, true);
}

// is_dir(p: str): bool
fn pathIsDir(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  var dir = std.fs.cwd().openDir(vl.asString(getArg(vm, args)).string(), .{})
    catch return FALSE_VAL;
  dir.close();
  return TRUE_VAL;
}

// is_file(p: str): bool
fn pathIsFile(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  const file = std.fs.cwd().openFile(vl.asString(getArg(vm, args)).string(), .{})
    catch return FALSE_VAL;
  file.close();
  return TRUE_VAL;
}

fn addPathExterns(vm: *VM) void {
  addExternFn(vm, "get_sep", 0, pathGetSep);
  addExternFn(vm, "basename", 1, pathBasename);
  addExternFn(vm, "dirname", 1, pathDirname);
  addExternFn(vm, "extension", 1, pathExtension);
  addExternFn(vm, "exists", 1, pathExists);
  addExternFn(vm, "realpath", 1, pathRealpath);
  addExternFn(vm, "is_dir", 1, pathIsDir);
  addExternFn(vm, "is_file", 1, pathIsFile);
}

//******** [os] *********//

// getcwd(): Result{str, str}
fn osGetcwd(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  _ = args;
  var buf: [std.fs.MAX_NAME_BYTES]u8 = undefined;
  const cwd = std.posix.getcwd(&buf) catch |e| {
    const reas = newStringV(vm, @errorName(e));
    const root_len = vl.saveTempRoot(vm, reas);
    defer vl.delTempRoots(vm, root_len);
    return vl.createError(vm, reas);
  };
  const val = newStringV(vm, cwd);
  const root_len = vl.saveTempRoot(vm, val);
  defer vl.delTempRoots(vm, root_len);
  return vl.okVal(vm, val);
}

// getenv(key: str): Maybe{str}
fn osGetenv(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  if (std.posix.getenv(vl.asString(getArg(vm, args)).string())) |val| {
    const v = newStringV(vm, val);
    const root_len = vl.saveTempRoot(vm, v);
    defer vl.delTempRoots(vm, root_len);
    return vl.justVal(vm, v);
  }
  return NONE_VAL;
}

// chdir(path: str): Result{void, str}
fn osChdir(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  std.posix.chdir(vl.asString(getArg(vm, args)).string()) catch |e| {
    const reas = newStringV(vm, @errorName(e));
    const root_len = vl.saveTempRoot(vm, reas);
    defer vl.delTempRoots(vm, root_len);
    return vl.createError(vm, reas);
  };
  return vl.okVal(vm, VOID_VAL);
}

fn addOsExterns(vm: *VM) void {
  addExternFn(vm, "getcwd", 0, osGetcwd);
  addExternFn(vm, "getenv", 1, osGetenv);
  addExternFn(vm, "chdir", 1, osChdir);
}

//******** [string] *********//
// hash_string(s: str): num
fn stringHashString(vm: *VM, argc: u32, args: u32) callconv(.C) Value {
  _ = argc;
  return vl.numberIntVal(vl.asString(getArg(vm, args)).hash);
}

fn addStringExterns(vm: *VM) void {
  addExternFn(vm, "hash_string", 1, stringHashString);
}

pub const ExternPos = struct {
  start: usize,
  end: usize,
};

pub const ExternMapping = std.StaticStringMap(ExternPos).initComptime(.{
  .{"math.veb", .{.start = 0, .end = 22}},
  .{"file.veb", .{.start = 22, .end = 34}},
  .{"time.veb", .{.start = 34, .end = 38}},
  .{"path.veb", .{.start = 38, .end = 46}},
  .{"os.veb", .{.start = 46, .end = 49}},
  .{"string.veb", .{.start = 49, .end = 50}},
});

pub fn addBuiltins(vm: *VM) void {
  // Add builtin classes
  // NOTE: string class should always be created first, as other things
  //       like addNativeFn, and other classes depend on it!
  addNativeClass(vm, createStringClass(vm));
  addNativeClass(vm, createListClass(vm));
  addNativeClass(vm, createTupleClass(vm));
  addNativeClass(vm, createMapClass(vm));
  addNativeClass(vm, createErrorClass(vm));
  // Add builtin functions
  //** VM, fn-name, fn-name-index, arity, fn-exec **//
  addNativeFn(vm, "assert", 2, fnAssert);
  addNativeFn(vm, "@exit", 1, fnExit);
  addNativeFn(vm, "@panic", 1, fnPanic);
  addNativeFn(vm, "print", VarArgC, fnPrint);
  addNativeFn(vm, "println", VarArgC, fnPrintln);
  addNativeFn(vm, "@string", 1, fnString);
}

pub fn addExterns(vm: *VM) void {
  // Order matters here
  addMathExterns(vm);
  addFsFileExterns(vm);
  addTimeExterns(vm);
  addPathExterns(vm);
  addOsExterns(vm);
  addStringExterns(vm);
}

pub fn addNames(vm: *VM) void {
  vm.names.init = newString(vm, ks.InitVar);
  vm.names.ok = newString(vm, ks.OkVar);
  vm.names.err = newString(vm, ks.ErrorVar);
  vm.names.just = newString(vm, ks.JustVar);
}

pub fn addAll(vm: *VM) void {
  addBuiltins(vm);
  addNames(vm);
  addExterns(vm);
}
