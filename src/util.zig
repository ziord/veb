const std = @import("std");

pub fn append(comptime T: type, list: *std.ArrayList(T), val: T) void {
  list.append(val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.os.exit(1);
  };
}
