const std = @import("std");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;

pub const Mem = struct {
  allocator: std.mem.Allocator,

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator) Self {
    return Self {.allocator = allocator};
  }

  pub fn vmAlloc(self: *Self, comptime T: type, vm: *VM, ptr: ?[]T, curr_size: usize, new_size: usize) ?[]T {
    _ = curr_size;
    _ = vm;
    if (new_size == 0) {
      self.allocator.free(ptr.?);
      return null;
    }
    return @call(.always_inline, self.alloc, .{T, ptr, new_size});
  }

  pub inline fn alloc(self: *Self, comptime T: type, ptr: ?[]T, new_size: usize) []T {
    var tmp: []T = undefined;
    if (ptr) |c_ptr| {
      tmp = self.allocator.realloc(c_ptr, new_size) catch |e| {
        util.error_("AllocationError: {}", .{e});
      };
    } else {
      tmp = self.allocator.alloc(T, new_size) catch |e| {
        util.error_("AllocationError: {}", .{e});
      };
    }
    return tmp;
  }
};

