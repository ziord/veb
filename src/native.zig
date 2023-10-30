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
  "listItems",
  "delete",
  "remove",
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
    vm.panicUnwindError("AssertionError: '{s}'", .{msg.str[0..msg.len]});
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
  vm.panicUnwindError("Error: '{s}'", .{msg.str[0..msg.len]});
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
  return vl.numberVal(@floatFromInt(str.len));
}

fn createStringClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {stringLen};
  //*** arity of each method ***//
  const arities = [_]u32 {0};
  //*** index into NativeFns array ***//
  const names = [_]usize{5};
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
  return vl.numberVal(@floatFromInt(list.len));
}

// pop(): T
fn listPop(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var list = vl.asList(getArg(vm, args));
  if (list.len == 0) {
    vm.panicUnwindError("Error: pop from empty list", .{});
    return NOTHING_VAL;
  }
  var val = list.items[list.len - 1];
  list.len -= 1;
  return val;
}

// get(index: num): T?
fn listGet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var list = vl.asList(getArg(vm, args));
  if (list.len == 0) return vl.NIL_VAL;
  var idx = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (idx < 0) idx += @intCast(list.len);
  if (idx >= list.len) return vl.NIL_VAL;
  return list.items[@intCast(idx)];
}

fn createListClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  // NOTE: Methods are set according to the order in prelude
  const methods = [_]NativeFn {listInit, listAppend, listLen, listPop, listGet};
  //*** arity of each method ***//
  const arities = [_]u32 {VarArgC, 1, 0, 0, 1};
  //*** index into NativeFns array ***//
  const names = [_]usize{7, 4, 5, 8, 10};
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.ListVar);
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
  return vl.numberVal(@floatFromInt(tuple.len));
}

// get(index: num): T?
fn tupleGet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var tuple = vl.asTuple(getArg(vm, args));
  if (tuple.len == 0) return vl.NIL_VAL;
  var idx = vl.asIntNumber(i64, getArg(vm, args + 1));
  if (idx < 0) idx += @intCast(tuple.len);
  if (idx >= tuple.len) return vl.NIL_VAL;
  return tuple.items[@intCast(idx)];
}

fn createTupleClass(vm: *VM) *vl.ObjClass {
  //*** method executable ***//
  const methods = [_]NativeFn {tupleInit, tupleLen, tupleGet};
  //*** arity of each method ***//
  const arities = [_]u32 {0, VarArgC, 1};
  //*** index into NativeFns array ***//
  const names = [_]usize{7, 5, 10};
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
  var map = vl.asMap(getArg(vm, args));
  return vl.numberVal(@floatFromInt(map.meta.len));
}

// set(key: K, value: V): bool
fn mapSet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var res = map.meta.set(getArg(vm, args + 1), getArg(vm, args + 2), vm);
  return vl.boolVal(res);
}

// get(key: K): V?
fn mapGet(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  if (vl.asMap(getArg(vm, args)).meta.get(getArg(vm, args + 1), vm)) |res| {
    return res;
  }
  return vl.NIL_VAL;
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

// listItems(): list{K | V}
fn mapListItems(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var map = vl.asMap(getArg(vm, args));
  var list = vl.createList(vm, map.meta.len << 1);
  var kvc: usize = 0;
  for (map.meta.items[0..map.meta.len]) |item| {
    list.items[kvc] = item.key;
    list.items[kvc + 1] = item.value;
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
    mapListItems,
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
  var cls = vl.createClass(vm, methods.len);
  for (methods, arities, names, 0..) |mtd, arity, name, i| {
    cls.methods[i] = vl.objVal(vl.createNativeFn(vm, mtd, arity, name));
  }
  cls.name = newString(vm, ks.ErrVar);
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
  addNativeClass(vm, createErrClass(vm));
  // Add builtin functions
  //** VM, fn-name, fn-name-index, arity, fn-exec **//
  addNativeFn(vm, "assert", 0, 2, fnAssert);
  addNativeFn(vm, "exit", 1, 1, fnExit);
  addNativeFn(vm, "panic", 2, 1, fnPanic);
  addNativeFn(vm, "print", 3, 1, fnPrint);
}
