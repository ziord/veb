const std = @import("std");
const util = @import("util.zig");

pub const std_options = struct {
  pub const log_level = if (util.getMode() == .Debug) .debug else .info;
};

pub fn main() !void {
  std.debug.print("hello veb!\n", .{});
}
