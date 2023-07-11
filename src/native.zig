const std = @import("std");
const vl = @import("value.zig");
const VM = @import("vm.zig").VM;

const Value = vl.Value;
const ZFn = vl.ZFn;

pub const ZFnNames = [_][]const u8 {
  "assert",
  "exit",
  "panic",
};

/// ********************
/// > barebones builtins
/// ********************

/// assert(cond: bool, msg: str): void | noreturn
pub fn fnAssert(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  if (vl.valueFalsy(vm.fiber.fp.stack[args])) {
    var msg = vl.asString(vl.valueToString(vm.fiber.fp.stack[args + 1], vm));
    vm.panickUnwindError("AssertionError: '{s}'", .{msg.str[0..msg.len]});
    return vl.FALSE_VAL;
  }
  return vl.NIL_VAL;
}

/// exit(code: num): noreturn
pub fn fnExit(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var code = vl.asIntNumber(u8, vm.fiber.fp.stack[args]);
  vm.deinit();
  std.os.exit(code);
}

/// panic{T}(msg: T): noreturn
pub fn fnPanic(vm: *VM, argc: u32, args: u32) Value {
  _ = argc;
  var msg = vl.asString(vl.valueToString(vm.fiber.fp.stack[args], vm));
  vm.panickUnwindError("Error: '{s}'", .{msg.str[0..msg.len]});
  vm.deinit();
  std.os.exit(1);
}

/// ********************
/// > helpers
/// ********************

pub fn getZFnName(name: usize) []const u8 {
  return ZFnNames[name];
}

fn addZFn(vm: *VM, name: []const u8, name_idx: usize, arity: u32, func: ZFn) void {
  _ = vm.globals.put(
    vl.createString(vm, &vm.strings, name, false),
    vl.objVal(vl.createZFn(vm, func, arity, name_idx)),
    vm
  );
}

pub fn addBuiltins(vm: *VM) void {
  //** VM, fn-name, fn-name-index, arity, fn-exec **//
  addZFn(vm, "assert", 0, 2, fnAssert);
  addZFn(vm, "exit", 1, 1, fnExit);
  addZFn(vm, "panic", 2, 1, fnPanic);
}
