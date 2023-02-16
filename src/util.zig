const std = @import("std");

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
