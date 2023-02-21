const std = @import("std");
const util = @import("util.zig");
const value = @import("value.zig");
const VM = @import("vm.zig").VM;
const Obj = value.Obj;

pub const BUFFER_INIT_SIZE = 8;

pub inline fn growCapacity(cap: usize) usize {
  return if (cap < BUFFER_INIT_SIZE) BUFFER_INIT_SIZE else cap << 2;
}

pub inline fn alignTo(n: usize, align_: usize) usize {
  return (n + align_ - 1) / align_ * align_;
}

// Mem-struct related thingies:

allocator: std.mem.Allocator,

const Self = @This();

pub fn init(allocator: std.mem.Allocator) Self {
  return Self {.allocator = allocator};
}

pub inline fn deinit(self: *Self) void {
  _ = self;
  // TODO
}

pub fn alloc(self: *Self, comptime T: type, vm: *VM) *T {
  const size = @sizeOf(T);
  vm.gc.bytes_allocated += size;
  vm.gc.forceCollect(vm, (util.getMode() == .Debug));
  vm.gc.tryCollect(vm);
  return self.allocator.create(T) catch |e| {
    util.error_("AllocationError: {}", .{e});
  };
}

pub fn allocBuf(self: *Self, comptime T: type, len: usize, vm: *VM) []T {
  vm.gc.bytes_allocated += @sizeOf(T) * len;
  // TODO: refactor this.
  vm.gc.forceCollect(vm, (util.getMode() == .Debug));
  vm.gc.tryCollect(vm);
  return self.allocator.alloc(T, len) catch |e| {
    util.error_("AllocationError: {}", .{e});
  };
}

pub fn resizeBuf(self: *Self, comptime T: type, vm: *VM, ptr: []T, old_len: usize, new_len: usize) []T {
  const old_size = @sizeOf(T) * old_len;
  const new_size = @sizeOf(T) * new_len;
  vm.gc.bytes_allocated += (new_size - old_size);
  // TODO: refactor this.
  if (new_size > old_size) {
    vm.gc.forceCollect(vm, (util.getMode() == .Debug));
    vm.gc.tryCollect(vm);
  }
  return self.allocator.realloc(ptr, new_len) catch |e| {
    util.error_("AllocationError: {}", .{e});
  };
}

pub fn free(self: *Self, comptime T: type, vm: *VM, ptr: *T) void {
  vm.gc.bytes_allocated -= @sizeOf(T);
  self.allocator.destroy(ptr);
}

pub fn freeBuf(self: *Self, comptime T: type, vm: *VM, ptr: []const T) void {
  vm.gc.bytes_allocated -= @sizeOf(T) * ptr.len;
  self.allocator.free(ptr);
}
