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
  token: Token,
};

pub const Diagnostic = struct {
  data: std.ArrayList(DiagData),
  filename: []const u8,
  src: []const u8,

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator, filename: []const u8, src: []const u8) Self {
    return Self {.data = std.ArrayList(DiagData).init(allocator), .filename = filename, .src = src};
  }

  fn alreadyIn(self: *Self, token: Token, msg: []const u8) bool {
    for (self.data.items) |data| {
      // we also check the msg because tokens are shared among nodes
      if (data.token.eql(token) and std.mem.eql(u8, data.msg, msg)) {
        return true;
      }
    }
    return false;
  }

  fn pushData(self: *Self, level: DiagLevel, token: Token, comptime fmt: []const u8, args: anytype)
  !void {
    var allocator = self.data.allocator;
    var col = token.column(self.src);
    var msg = try std.fmt.allocPrint(allocator, fmt ++ "\n", args);
    if (self.alreadyIn(token, msg)) return;
    try self.data.append(.{.level = level, .msg = msg, .token = token});
    try self.data.append(
      .{
        .level = level,
        .token = token,
        .msg = try std.fmt.allocPrint(
          allocator, "{s}.{}:{}:\n\t{s}\n",
          .{self.filename, token.line,col, token.getLine(self.src)}
        )
      }
    );
    try self.data.append(.{.level = level, .msg = "\t", .token = token});
    var i = if (col >= token.value.len) col - token.value.len else token.value.len - col;
    while (i > 0) {
      try self.data.append(.{.level = level, .msg = " ", .token = token});
      i -= 1;
    }
    try self.printSquig(level, if (token.value.len != 0) token.value.len else 1, token);
    try self.data.append(.{.level = level, .msg = "\n\n", .token = token});
  }

  fn printSquig(self: *Self, level: DiagLevel, i: usize, token: Token) !void {
    var y = i;
    while (y > 0) {
      try self.data.append(
        .{
          .level = level,
          .token = token,
          .msg = try std.fmt.allocPrint(self.data.allocator, "{s:^}", .{"^"}),
        }
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

  pub fn hasAny(self: *Self) bool {
    return self.data.items.len > 0;
  }

  pub inline fn count(self: *Self) usize {
    return self.data.items.len;
  }

  pub fn popUntil(self: *Self, size: usize) void {
    while (self.data.items.len > size) {
      _ = self.data.pop();
    }
  }

  pub fn addDiagnostics(self: *Self, level: DiagLevel, token: Token, comptime fmt: []const u8, args: anytype)
  void {
    self.pushData(level, token, fmt, args) catch |e| {
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
