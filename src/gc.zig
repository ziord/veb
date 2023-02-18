const std = @import("std");
const Mem = @import("mem.zig");
const value = @import("value.zig");
const VM = @import("vm.zig").VM;
const NovaAllocator = @import("allocator.zig");

const Obj = value.Obj;

bytes_allocated: usize,
next_collection: usize,
gray_stack: std.ArrayList(*Obj),
mem: Mem,
allocator: *NovaAllocator,

const Self = @This();

pub fn init(allocator: *NovaAllocator) Self {
  return Self {
    .gray_stack = std.ArrayList(*Obj).init(allocator.getArenaAllocator()),
    .bytes_allocated = 0,
    .next_collection = 0,
    .mem = Mem.init(allocator.getAllocator()),
    .allocator = allocator,
  };
}

pub fn deinit(self: *Self) void {
  self.gray_stack.deinit();
  self.mem.deinit();
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
  switch (obj.ty) {
    .ObjStr => {
      const T = value.ObjString;
      var string = @ptrCast(*value.ObjString, obj);
      self.mem.freeBuf(u8, vm, string.str);
      self.mem.free(T, vm, value.objToSpecObject(T, obj));
    }
  }
}
