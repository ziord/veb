const std = @import("std");
const util = @import("util.zig");
const Token = @import("lex.zig").Token;

pub const DiagLevel = enum (u8) {
  DiagInfo,
  DiagWarn,
  DiagError,
};

pub const DiagData = struct {
  level: DiagLevel,
  msg: []const u8,
};

pub const Diagnostic = struct {
  data: std.ArrayList(DiagData),

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator) Self {
    return Self {.data = std.ArrayList(DiagData).init(allocator)};
  }

  fn pushData(self: *Self, level: DiagLevel, token: Token, filename: []const u8, comptime fmt: []const u8, args: anytype) !void {
    var allocator = self.data.allocator;
    try self.data.append(.{.level = level, .msg = try std.fmt.allocPrint(allocator, fmt ++ "\n", args)});
    try self.data.append(
      .{
        .level = level,
        .msg = try std.fmt.allocPrint(
          allocator, "{s}.{}:{}:\n\t{s}\n",
          .{filename, token.line, token.column, token.getLine()}
        )
      }
    );
    try self.data.append(.{.level = level, .msg = "\t"});
    var i = if (token.column >= token.value.len) token.column - token.value.len else token.value.len - token.column;
    while (i > 0) {
      try self.data.append(.{.level = level, .msg = " "});
      i -= 1;
    }
    try self.printSquig(level, token.value.len);
    try self.data.append(.{.level = level, .msg = "\n\n"});
  }

  fn printSquig(self: *Self, level: DiagLevel, i: usize) !void {
    var y = i;
    while (y > 0) {
      try self.data.append(
        .{.level = level,.msg = try std.fmt.allocPrint(self.data.allocator, "{s:^}", .{"^"})}
      );
      y -= 1;
    }
  }

  inline fn has(self: *Self, level: DiagLevel) bool {
    for (self.data.items) |item| {
      if (item.level == level) return true;
    }
    return false;
  }

  pub fn hasErrors(self: *Self) bool {
    return self.has(.DiagError);
  }

  pub inline fn count(self: *Self) usize {
    return self.data.items.len;
  }

  pub fn popUntil(self: *Self, size: usize) void {
    while (self.data.items.len > size) {
      _ = self.data.pop();
    }
  }

  pub fn addDiagnostics(self: *Self, level: DiagLevel, token: Token, filename: []const u8, comptime fmt: []const u8, args: anytype) void {
    self.pushData(level, token, filename, fmt, args) catch |e| {
      std.debug.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn display(self: *Self) void {
    // TODO: level-based display
    for (self.data.items) |data| {
      std.debug.print("{s}", .{data.msg});
    }
  }
};
