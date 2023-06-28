const std = @import("std");
const value = @import("value.zig");
const VM = @import("vm.zig").VM;
const NovaAllocator = @import("allocator.zig");

const Obj = value.Obj;

bytes_allocated: usize,
next_collection: usize,
gray_stack: std.ArrayList(*Obj),
allocator: *NovaAllocator,

const Self = @This();

pub fn init(allocator: *NovaAllocator) Self {
  return Self {
    .gray_stack = std.ArrayList(*Obj).init(allocator.getArenaAllocator()),
    .bytes_allocated = 0,
    .next_collection = 0,
    .allocator = allocator,
  };
}

pub fn deinit(self: *Self) void {
  self.gray_stack.deinit();
}

pub fn collect(self: *Self, vm: *VM) void {
  _ = self;
  _ = vm;
}

pub inline fn tryCollect(self: *Self, vm: *VM) void {
  if (self.bytes_allocated > self.next_collection) {
    self.collect(vm);
  }
}

pub inline fn forceCollect(self: *Self, vm: *VM, likely: bool) void {
  if (likely) {
    self.collect(vm);
  }
}

pub fn freeObject(self: *Self, obj: *Obj, vm: *VM) void {
  _ = self;
  switch (obj.id) {
    .objstring => {
      const T = value.ObjString;
      var string = @ptrCast(*T, obj);
      vm.mem.freeBuf(u8, vm, string.str[0..string.len]);
      vm.mem.free(T, vm, string);
    },
    .objlist => {
      const T = value.ObjList;
      var list = @ptrCast(*T, obj);
      vm.mem.freeBuf(value.Value, vm, list.items[0..list.capacity]);
      vm.mem.free(T, vm, list);
    },
    .objtuple => {
      const T = value.ObjTuple;
      var tuple = @ptrCast(*T, obj);
      vm.mem.freeBuf(value.Value, vm, tuple.items[0..tuple.len]);
      vm.mem.free(T, vm, tuple);
    },
    .objvalmap => {
      const T = value.ObjMap;
      var map = @ptrCast(*T, obj);
      map.meta.free(vm);
      vm.mem.free(T, vm, map);
    },
    .objfn => {
      const T = value.ObjFn;
      var fun = @ptrCast(*T, obj);
      fun.code.deinit(vm);
      vm.mem.free(T, vm, fun);
    },
    .objzfn => {
      vm.mem.free(value.ObjZFn, vm, @ptrCast(*value.ObjZFn, obj));
    },
    .objclosure => {
      const T = value.ObjClosure;
      var clo = @ptrCast(*T, obj);
      vm.mem.freeBuf(*value.ObjUpvalue, vm, clo.env[0..clo.fun.envlen]);
      vm.mem.free(T, vm, clo);
    },
    .objerror => {
      vm.mem.free(value.ObjError, vm, @ptrCast(*value.ObjError, obj));
    },
    .objupvalue => {
      vm.mem.free(value.ObjUpvalue, vm, @ptrCast(*value.ObjUpvalue, obj));
    },
    .objfiber => {
      const T = value.ObjFiber;
      var fiber = @ptrCast(*T, obj);
      vm.mem.freeBuf(value.CallFrame, vm, fiber.frames[0..fiber.frame_cap]);
      vm.mem.freeBuf(value.Value, vm, fiber.stack[0..fiber.stack_cap]);
      vm.mem.free(T, vm, fiber);
    },
  }
}
