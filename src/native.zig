const std = @import("std");
const ks = @import("constants.zig");
const vl = @import("value.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;

const Value = vl.Value;
const NativeFn = vl.NativeFn;
const NOTHING_VAL = vl.NOTHING_VAL;
const VarArgC = 256;

pub const NativeFns = [_][]const u8 {
  "assert",
  "@exit",
  "@panic",
  "print",
  "append",
  "len",
  "value",
  "init",
  "pop",
  "set",
  "get",
  "keys",
  "values",
  "items",
  "entries",
  "delete",
  "remove",
  "println",
  "slice",
  "concat",
  "@string",
  "next",
  "iter",
};


/// ********************
/// > helpers
/// ********************

pub inline fn getNativeFnName(name: usize) []const u8 {
  return NativeFns[name];
}

fn addNativeFn(vm: *VM, name: []const u8, name_idx: usize, arity: u32, func: NativeFn) void {
  _ = vm.globals.set(
    vl.createString(vm, &vm.strings, name, false),
    vl.objVal(vl.createNativeFn(vm, func, arity, name_idx)),
    vm
  );
}

inline fn addNativeClass(vm: *VM, cls: *vl.ObjClass) void {
  _ = vm.globals.set(cls.name, vl.objVal(cls), vm);
}

inline fn newString(vm: *VM, chars: []const u8) *const vl.ObjString {
  return vl.createString(vm, &vm.strings, chars, false);
}

inline fn getArg(vm: *VM, pos: u32) Value {
  return vm.fiber.fp.stack[pos];
}

/// ******************************
/// > barebones builtin functions
/// ******************************

/// assert(cond: bool, msg: str): void | noreturn
pub fn fnAssert(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  if (vl.valueFalsy(getArg(vm, args))) {
    var msg = vl.asString(vl.valueToString(getArg(vm, args + 1), vm));
    vm.panicUnwindError("AssertionError: '{s}'", .{msg.str[0..msg.len]});
    return vl.FALSE_VAL;
  }
  return NOTHING_VAL;
}

/// exit(code: num): noreturn
pub fn fnExit(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const code = vl.asIntNumber(u8, getArg(vm, args));
  vm.deinit();
  std.os.exit(code);
}

/// panic{T}(msg: T): noreturn
pub fn fnPanic(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var msg = vl.asString(vl.valueToString(getArg(vm, args), vm));
  vm.panicUnwindError("panicked at: '{s}'", .{msg.str[0..msg.len]});
  vm.deinit();
  std.os.exit(1);
}

/// print(args*: any): void
pub fn fnPrint(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var tup = vl.asList(getArg(vm, args));
  for (tup.items[0..tup.len], 1..) |val, i| {
    vl.printValue(val);
    if (i < tup.len) {
      util.print(" ", .{});
    }
  }
  return NOTHING_VAL;
}

/// @string(val: any): str
pub fn fnString(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  return vl.valueToString(getArg(vm, args), vm);
}

/// println(args*: any): void
pub fn fnPrintln(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var tup = vl.asList(getArg(vm, args));
  for (tup.items[0..tup.len], 1..) |val, i| {
    vl.printValue(val);
    if (i < tup.len) {
      util.print(" ", .{});
    }
  }
  util.print("\n", .{});
  return NOTHING_VAL;
}

/// ****************************
/// > barebones builtin classes
/// ****************************

//******** str ********//

// len(): num
fn stringLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args));
  return vl.numberVal(@floatFromInt(str.len));
}

// concat(other: str): str
fn stringConcat(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const str = vl.asString(getArg(vm, args));
  const conc = str.concat(vl.asString(getArg(vm, args + 1)), vm.gc.allocator.getAllocator());
  return vl.objVal(vl.createString(vm, &vm.strings, conc, true));
}

fn createStringClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {stringLen, stringConcat};
  //*** arity of each method ***//
  const arities = [_]u32 {0, 1};
  //*** index into NativeFns array ***//
  const names = [_]usize{5, 19};
  // important to do this first
  var cls = vl.createClass(vm, methods.len);
  vm.classes.string = cls;
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.StrVar);
  return cls;
}

//******** list ********//

// append(item: T): void
fn listAppend(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  vl.asList(getArg(vm, args)).append(vm, getArg(vm, args + 1));
  return NOTHING_VAL;
}

// len(): num
fn listLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  return vl.numberVal(@floatFromInt(list.len));
}

// pop(): Maybe{T}
fn listPop(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var list = vl.asList(getArg(vm, args));
  if (list.len == 0) {
    return vl.noneVal();
  }
  const val = list.items[list.len - 1];
  list.len -= 1;
  return vl.justVal(vm, val);
}

// get(index: num): Maybe{T}
fn listGet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  if (list.len == 0) return vl.noneVal();
  var idx = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (idx < 0) idx += @intCast(list.len);
  if (idx >= list.len or idx < 0) return vl.noneVal();
  return vl.justVal(vm, list.items[@intCast(idx)]);
}

// slice(start: num, end: num): List{T}
fn listSlice(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const list = vl.asList(getArg(vm, args));
  if (list.len == 0) return vl.noneVal();
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

fn createListClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  // NOTE: Methods are set according to the order in prelude
  const methods = [_]NativeFn {listAppend, listLen, listPop, listGet, listSlice};
  //*** arity of each method ***//
  const arities = [_]u32 {1, 0, 0, 1, 2};
  //*** index into NativeFns array ***//
  const names = [_]usize{4, 5, 8, 10, 18};
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.ListVar);
  vm.classes.list = cls;
  return cls;
}

//******** tuple ********//

// len(): num
fn tupleLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const tuple = vl.asTuple(getArg(vm, args));
  return vl.numberVal(@floatFromInt(tuple.len));
}

fn createTupleClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {tupleLen};
  //*** arity of each method ***//
  const arities = [_]u32 {VarArgC};
  //*** index into NativeFns array ***//
  const names = [_]usize{5};
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.TupleVar);
  vm.classes.tuple = cls;
  return cls;
}

//******** map ********//

// len(): num
fn mapLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const map = vl.asMap(getArg(vm, args));
  return vl.numberVal(@floatFromInt(map.meta.len));
}

// set(key: K, value: V): bool
fn mapSet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  const res = map.meta.set(getArg(vm, args + 1), getArg(vm, args + 2), vm);
  return vl.boolVal(res);
}

// get(key: K): Maybe{V}
fn mapGet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  if (vl.asMap(getArg(vm, args)).meta.get(getArg(vm, args + 1), vm)) |res| {
    return vl.justVal(vm, res);
  }
  return vl.noneVal();
}

// delete(key: K): bool
fn mapDelete(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const val = vl.asMap(getArg(vm, args)).meta.delete(getArg(vm, args + 1), vm);
  return vl.boolVal(val);
}

// remove(key: K): bool
fn mapRemove(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  const val = vl.asMap(getArg(vm, args)).meta.remove(getArg(vm, args + 1), vm);
  return vl.boolVal(val);
}

// keys(): List{K}
fn mapKeys(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.keys(vm);
}

// values(): List{V}
fn mapValues(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.values(vm);
}

// items(): List{Tuple{K, V}}
fn mapItems(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var list = vl.createList(vm, map.meta.len);
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
fn mapEntries(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var list = vl.createList(vm, map.meta.len << 1);
  var kvc: usize = 0;
  for (map.meta.items[0..map.meta.len]) |item| {
    list.items[kvc] = vl.structVal(vm, item.key, ks.KeyVar);
    list.items[kvc + 1] = vl.structVal(vm, item.value, ks.ValueVar);
    kvc += 2;
  }
  return vl.objVal(list);
}

fn createMapClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {
    mapSet,
    mapGet,
    mapDelete,
    mapRemove,
    mapKeys,
    mapValues,
    mapItems,
    mapEntries,
    mapLen
  };
  //*** arity of each method ***//
  const arities = [_]u32 {2, 1, 1, 1, 0, 0, 0, 0, 0};
  //*** index into NativeFns array ***//
  const names = [_]usize{5, 9, 15, 16, 10, 11, 12, 14, 5};
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
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
  addNativeFn(vm, "assert", 0, 2, fnAssert);
  addNativeFn(vm, "@exit", 1, 1, fnExit);
  addNativeFn(vm, "@panic", 2, 1, fnPanic);
  addNativeFn(vm, "print", 3, VarArgC, fnPrint);
  addNativeFn(vm, "println", 17, VarArgC, fnPrintln);
  addNativeFn(vm, "@string", 20, 1, fnString);
}

pub fn addNames(vm: *VM) void {
  vm.names.init = vl.createString(vm, &vm.strings, ks.InitVar, false);
}
