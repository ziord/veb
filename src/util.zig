const std = @import("std");
const builtin = @import("builtin");

pub fn append(comptime T: type, list: *std.ArrayList(T), val: T) void {
  list.append(val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.os.exit(1);
  };
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
  var out_w = std.io.getStdOut().writer();
  out_w.print(fmt, args) catch return;
}

pub fn error_(comptime fmt: []const u8, args: anytype) noreturn {
  std.debug.print(fmt, args);
  std.os.exit(1);
}

pub inline fn getMode() std.builtin.Mode {
  return builtin.mode;
}

pub inline fn assert(check: bool, msg: []const u8) void {
  if (!check) {
    @panic(msg);
  }
}

pub inline fn box(comptime T: type, val: T, allocator: std.mem.Allocator) *T {
  var item = allocator.create(T) catch {
    std.debug.print("Allocation failed\n", .{});
    std.os.exit(1);
  };
  item.* = val;
  return item;
}
