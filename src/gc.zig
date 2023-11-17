const std = @import("std");
const value = @import("value.zig");
const VM = @import("vm.zig").VM;
const VebAllocator = @import("allocator.zig");

const Obj = value.Obj;

bytes_allocated: usize,
next_collection: usize,
gray_stack: std.ArrayList(*Obj),
allocator: *VebAllocator,

const Self = @This();

pub fn init(allocator: *VebAllocator) Self {
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
      var string: *T = @ptrCast(obj);
      vm.mem.freeBuf(u8, vm, string.str[0..string.len]);
      vm.mem.free(T, vm, string);
    },
    .objlist => {
      const T = value.ObjList;
      var list: *T = @ptrCast(obj);
      vm.mem.freeBuf(value.Value, vm, list.items[0..list.capacity]);
      vm.mem.free(T, vm, list);
    },
    .objtuple => {
      const T = value.ObjTuple;
      var tuple: *T = @ptrCast(obj);
      vm.mem.freeBuf(value.Value, vm, tuple.items[0..tuple.len]);
      vm.mem.free(T, vm, tuple);
    },
    .objvalmap => {
      const T = value.ObjMap;
      var map: *T = @ptrCast(obj);
      map.meta.free(vm);
      vm.mem.free(T, vm, map);
    },
    .objfn => {
      const T = value.ObjFn;
      var fun: *T = @ptrCast(obj);
      fun.code.deinit(vm);
      vm.mem.free(T, vm, fun);
    },
    .objnativefn => {
      vm.mem.free(value.ObjNativeFn, vm, @as(*value.ObjNativeFn, @ptrCast(obj)));
    },
    .objclosure => {
      const T = value.ObjClosure;
      var clo: *T = @ptrCast(obj);
      vm.mem.freeBuf(*value.ObjUpvalue, vm, clo.env[0..clo.fun.envlen]);
      vm.mem.free(T, vm, clo);
    },
    .objerror => {
      vm.mem.free(value.ObjError, vm, @as(*value.ObjError, @ptrCast(obj)));
    },
    .objupvalue => {
      vm.mem.free(value.ObjUpvalue, vm, @as(*value.ObjUpvalue, @ptrCast(obj)));
    },
    .objfiber => {
      const T = value.ObjFiber;
      var fiber: *T = @ptrCast(obj);
      vm.mem.freeBuf(value.CallFrame, vm, fiber.frames[0..fiber.frame_cap]);
      vm.mem.freeBuf(value.Value, vm, fiber.stack[0..fiber.stack_cap]);
      vm.mem.free(T, vm, fiber);
    },
    .objclass, .objstruct => {
      const T = value.ObjClass;
      var cls: *T = @ptrCast(obj);
      vm.mem.freeBuf(value.Value, vm, cls.methods[0..cls.mlen]);
      vm.mem.free(T, vm, cls);
    },
    .objtag => {
      const T = value.ObjTag;
      vm.mem.free(T, vm, @as(*T, @ptrCast(obj)));
    },
    .objinstance => {
      const T = value.ObjInstance;
      var inst: *T = @ptrCast(obj);
      vm.mem.freeBuf(value.Value, vm, inst.fields[0..inst.flen]);
      vm.mem.free(T, vm, inst);
    },
    .objmethod => {
      vm.mem.free(value.ObjMethod, vm, @as(*value.ObjMethod, @ptrCast(obj)));
    },
  }
}
