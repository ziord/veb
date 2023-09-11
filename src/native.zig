const std = @import("std");
const vl = @import("value.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;

const Value = vl.Value;
const NativeFn = vl.NativeFn;
const NOTHING_VAL = vl.NOTHING_VAL;
const VarArgC = 256;

pub const NativeFns = [_][]const u8 {
  "assert",
  "exit",
  "panic",
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

inline fn newString(vm: *VM, chars: []const u8) *vl.ObjString {
  return @constCast(vl.createString(vm, &vm.strings, chars, false));
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
    vm.panickUnwindError("AssertionError: '{s}'", .{msg.str[0..msg.len]});
    return vl.FALSE_VAL;
  }
  return NOTHING_VAL;
}

/// exit(code: num): noreturn
pub fn fnExit(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var code = vl.asIntNumber(u8, getArg(vm, args));
  vm.deinit();
  std.os.exit(code);
}

/// panic{T}(msg: T): noreturn
pub fn fnPanic(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var msg = vl.asString(vl.valueToString(getArg(vm, args), vm));
  vm.panickUnwindError("Error: '{s}'", .{msg.str[0..msg.len]});
  vm.deinit();
  std.os.exit(1);
}

/// print(args*: any): void
pub fn fnPrint(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var tup = vl.asTuple(getArg(vm, args));
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
  var str = vl.asString(getArg(vm, args));
  return vl.numberVal(@intToFloat(f64, str.len));
}

fn createStringClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {stringLen};
  //*** arity of each method ***//
  const arities = [_]u32 {0};
  //*** index into NativeFns array ***//
  const names = [_]usize{5};
  var cls = vl.createClass(vm, (@sizeOf(@TypeOf(methods)) / methods.len));
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, "str");
  vm.classes.string = cls;
  return cls;
}

//******** list ********//

// init(args*: T): void
fn listInit(vm: *VM, argc: u32, args: u32) Value {
  _ = args;
  _ = vm;
  _ = argc;
  return NOTHING_VAL;
}

// append(item: T): void
fn listAppend(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  vl.asList(getArg(vm, args)).append(vm, getArg(vm, args + 1));
  return NOTHING_VAL;
}

// len(): num
fn listLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var list = vl.asList(getArg(vm, args));
  return vl.numberVal(@intToFloat(f64, list.len));
}

// pop(): T
fn listPop(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var list = vl.asList(getArg(vm, args));
  if (list.len == 0) {
    vm.panickUnwindError("Error: pop from empty list", .{});
    return NOTHING_VAL;
  }
  var val = list.items[list.len - 1];
  list.len -= 1;
  return val;
}

fn createListClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  // NOTE: Methods are set according to the order in prelude
  const methods = [_]NativeFn {listInit, listAppend, listLen, listPop};
  //*** arity of each method ***//
  const arities = [_]u32 {VarArgC, 1, 0, 0};
  //*** index into NativeFns array ***//
  const names = [_]usize{7, 4, 5, 8};
  var cls = vl.createClass(vm, (@sizeOf(@TypeOf(methods)) / methods.len));
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, "list");
  vm.classes.list = cls;
  return cls;
}

//******** tuple ********//

// init(args*: T): void
fn tupleInit(vm: *VM, argc: u32, args: u32) Value {
  _ = args;
  _ = vm;
  _ = argc;
  return NOTHING_VAL;
}

// len(): num
fn tupleLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var tuple = vl.asTuple(getArg(vm, args));
  return vl.numberVal(@intToFloat(f64, tuple.len));
}

fn createTupleClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {tupleInit, tupleLen};
  //*** arity of each method ***//
  const arities = [_]u32 {0, VarArgC};
  //*** index into NativeFns array ***//
  const names = [_]usize{7, 5};
  var cls = vl.createClass(vm, (@sizeOf(@TypeOf(methods)) / methods.len));
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, "tuple");
  vm.classes.tuple = cls;
  return cls;
}

//******** map ********//

// len(): num
fn mapLen(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  return vl.numberVal(@intToFloat(f64, map.meta.len));
}

// set(key: K, value: V): void
fn mapSet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  _ = map.meta.set(getArg(vm, args + 1), getArg(vm, args + 2), vm);
  return NOTHING_VAL;
}

// get(key: K): V?
fn mapGet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  if (vl.asMap(getArg(vm, args)).meta.get(getArg(vm, args + 1))) |res| {
    return res;
  }
  return vl.NIL_VAL;
}

// keys(): list{K}
fn mapKeys(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.keys(vm);
}

// values(): list{V}
fn mapValues(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  return vl.asMap(getArg(vm, args)).meta.values(vm);
}

// items(): list{tuple{K | V}}
fn mapItems(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var list = vl.createList(vm, map.meta.len);
  var idx: usize = 0;
  var valc: usize = 0;
  while (valc < map.meta.len) {
    var entry = &map.meta.entries[idx];
    if (!map.meta.isNullKey(entry.key)) {
      var tuple = vl.createTuple(vm, 2);
      tuple.items[0] = entry.key;
      tuple.items[1] = entry.value;
      list.items[valc] = vl.objVal(tuple);
      valc += 1;
    }
    idx += 1;
  }
  return vl.objVal(list);
}

fn createMapClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {
    mapSet,
    mapGet,
    mapKeys,
    mapValues,
    mapItems,
    mapLen
  };
  //*** arity of each method ***//
  const arities = [_]u32 {2, 1, 0, 0, 0, 0};
  //*** index into NativeFns array ***//
  const names = [_]usize{5, 9, 10, 11, 12, 5};
  var cls = vl.createClass(vm, (@sizeOf(@TypeOf(methods)) / methods.len));
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, "map");
  vm.classes.map = cls;
  return cls;
}

//******** err ********//

// init(val: T): void
fn errInit(vm: *VM, argc: u32, args: u32) Value {
  _ = args;
  _ = vm;
  _ = argc;
  return NOTHING_VAL;
}

// value(): T
fn errValue(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  return vl.asError(getArg(vm, args)).val;
}

fn createErrClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {errInit, errValue};
  //*** arity of each method ***//
  const arities = [_]u32 {1, 0};
  //*** index into NativeFns array ***//
  const names = [_]usize{7, 6};
  var cls = vl.createClass(vm, (@sizeOf(@TypeOf(methods)) / methods.len));
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, "err");
  vm.classes.err = cls;
  return cls;
}

pub fn addBuiltins(vm: *VM) void {
  // Add builtin functions
  //** VM, fn-name, fn-name-index, arity, fn-exec **//
  addNativeFn(vm, "assert", 0, 2, fnAssert);
  addNativeFn(vm, "exit", 1, 1, fnExit);
  addNativeFn(vm, "panic", 2, 1, fnPanic);
  addNativeFn(vm, "print", 3, 1, fnPrint);

  // Add builtin classes
  addNativeClass(vm, createStringClass(vm));
  addNativeClass(vm, createListClass(vm));
  addNativeClass(vm, createTupleClass(vm));
  addNativeClass(vm, createMapClass(vm));
  addNativeClass(vm, createErrClass(vm));
}
