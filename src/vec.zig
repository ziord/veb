const std = @import("std");
const Mem = @import("mem.zig");
const VM = @import("vm.zig").VM;

pub fn Vec(comptime T: type) type {
  return extern struct {
    capacity: usize,
    len: usize,
    items: [*]T,

    const Self = @This();
    pub const Slice = []T;

    pub fn init() Self {
      return Self {.capacity = 0, .len = 0, .items = &[_]T{}};
    }

    inline fn allocatedSlice(self: Self) Slice {
      return self.items[0..self.capacity];
    }

    pub fn push(self: *Self, item: T, vm: *VM) void {
      if (self.len >= self.capacity) {
        const new_capacity = Mem.growCapacity(self.capacity);
        self.items = vm.mem.resizeBuf(T, vm, self.allocatedSlice(),  self.capacity, new_capacity).ptr;
        self.capacity = new_capacity;
      }
      self.items[self.len] = item;
      self.len += 1;
    }

    pub fn pop(self: *Self) T {
      self.len -= 1;
      return self.items[self.len];
    }

    pub inline fn size(self: *Self) usize {
      return self.len;
    }

    pub fn clearAndFree(self: *Self, vm: *VM) void {
      // expand len (if not already at max cap) for freeing
      vm.mem.freeBuf(T, vm, self.items[0..self.capacity]);
      self.len = 0;
      self.capacity = 0;
    }
  };
}
