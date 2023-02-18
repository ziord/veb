const std = @import("std");
const Mem = @import("mem.zig");
const VM = @import("vm.zig").VM;

pub fn Vec(comptime T: type) type {
  return struct {
    capacity: usize,
    items: Slice,

    const Self = @This();
    pub const Slice = []T;

    pub fn init() Self {
      return Self {.capacity = 0, .items = &[_]T{}};
    }

    inline fn allocatedSlice(self: Self) Slice {
      return self.items.ptr[0..self.capacity];
    }

    pub fn push(self: *Self, item: T, vm: *VM) void {
      if (self.items.len >= self.capacity) {
        const new_capacity = Mem.growCapacity(self.capacity);
        self.items.ptr = vm.gc.mem.resizeBuf(T, vm, self.allocatedSlice(),  self.capacity, new_capacity).ptr;
        self.capacity = new_capacity;
      }
      self.items.len += 1;
      self.items[self.items.len - 1] = item;
    }

    pub fn pop(self: *Self) T {
      const val = self.items[self.items.len - 1];
      self.items.len -= 1;
      return val;
    }

    pub inline fn size(self: *Self) usize {
      return self.items.len;
    }

    pub fn clearAndFree(self: *Self, vm: *VM) void {
      // expand len (if not already at max cap) for freeing
      self.items.len = self.capacity;
      vm.gc.mem.freeBuf(T, vm, self.items);
      self.items.len = 0;
      self.capacity = 0;
    }
  };
}
