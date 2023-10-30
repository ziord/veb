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
  return self.allocator.create(T) catch |e| {
    util.error_("AllocationError: {}", .{e});
  };
}

pub fn allocBuf(self: *Self, comptime T: type, len: usize, vm: *VM) []T {
  vm.gc.bytes_allocated += @sizeOf(T) * len;
  return self.allocator.alloc(T, len) catch |e| {
    util.error_("AllocationError: {}", .{e});
  };
}

pub fn dupeStr(self: *Self, vm: *VM, str: []const u8) []u8 {
  vm.gc.bytes_allocated += @sizeOf(u8) * str.len;
  return self.allocator.dupe(u8, str) catch |e| {
    util.error_("AllocationError: {}", .{e});
  };
}

pub fn resizeBuf(self: *Self, comptime T: type, vm: *VM, buf: [*]T, old_len: usize, new_len: usize) []T {
  const old_size = @sizeOf(T) * old_len;
  const new_size = @sizeOf(T) * new_len;
  vm.gc.bytes_allocated += (new_size - old_size);
  if (self.allocator.resize(buf[0..old_len], new_len)) {
    return buf[0..new_len];
  }
  return self.allocator.realloc(buf[0..old_len], new_len) catch |e| {
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
