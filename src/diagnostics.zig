const std = @import("std");
const util = @import("util.zig");
const lex = @import("lex.zig");

const Token = lex.Token;

pub const DiagLevel = enum (u8) {
  DiagInfo,
  DiagWarn,
  DiagIError, // Internal Error
  DiagError,

  pub fn pretext(self: @This()) []const u8 {
    return switch (self) {
      .DiagError => "Error",
      .DiagWarn => "Warning",
      .DiagInfo => "Info",
      .DiagIError => "InternalError"
    };
  }
};

pub const DiagData = struct {
  level: DiagLevel,
  msg: []const u8,
  token: Token,
};

pub const Diagnostic = struct {
  data: std.ArrayList(DiagData),
  filename: *const[]const u8,
  src: *[]const u8,
  levels: std.ArrayList(DiagLevel),

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator, filename: *const[]const u8, src: *[]const u8) Self {
    return Self {
      .data = std.ArrayList(DiagData).init(allocator),
      .filename = filename,
      .src = src,
      .levels = std.ArrayList(DiagLevel).init(allocator),
    };
  }

  fn alreadyIn(self: *Self, token: Token, msg: []const u8) bool {
    for (self.data.items) |data| {
      // we also check the msg because tokens are shared among nodes
      if (token.equal(data.token) and std.mem.eql(u8, data.msg, msg)) {
        return true;
      }
    }
    return false;
  }

  fn pushData(self: *Self, level: DiagLevel, token: Token, comptime fmt: []const u8, args: anytype)
  !void {
    const allocator = self.data.allocator;
    const col = token.column(self.src.*);
    const msg = try std.fmt.allocPrint(allocator, fmt ++ "\n", args);
    if (self.alreadyIn(token, msg)) return;
    try self.data.append(.{.level = level, .msg = msg, .token = token});
    try self.data.append(
      .{
        .level = level,
        .token = token,
        .msg = try std.fmt.allocPrint(
          allocator, "{s}.{}:{}:\n\t{s}\n",
          .{self.filename.*, token.line, col, token.getLine(self.src.*)}
        )
      }
    );
    try self.data.append(.{.level = level, .msg = "\t", .token = token});
    const lexeme = token.lexeme();
    var i = if (col >= lexeme.len) col - lexeme.len else lexeme.len - col;
    while (i > 0) {
      try self.data.append(.{.level = level, .msg = " ", .token = token});
      i -= 1;
    }
    try self.printSquig(level, if (lexeme.len != 0) lexeme.len else 1, token);
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
    return self.has(.DiagError) or self.has(.DiagIError);
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

  pub fn pushLevel(self: *Self, level: DiagLevel) void {
    self.levels.append(level) catch {};
  }

  pub fn popLevel(self: *Self) DiagLevel {
    return if (self.levels.items.len > 0) self.levels.pop() else .DiagError;
  }
  
  pub inline fn getLevel(self: *Self) DiagLevel {
    if (self.levels.items.len > 0) {
      return self.levels.items[self.levels.items.len - 1];
    }
    return .DiagError;
  }

  pub fn addDiagnostics(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.pushData(self.getLevel(), token, fmt, args) catch |e| {
      std.debug.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn addDiagnosticsDirect(self: *Self, token: Token, msg: []const u8) void {
    self.data.append(.{.level = self.getLevel(), .msg = msg, .token = token}) catch |e| {
      std.debug.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn addDiagnosticsWithLevel(self: *Self, level: DiagLevel, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.pushData(level, token, fmt, args) catch |e| {
      std.debug.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn display(self: *Self) void {
    for (self.data.items) |data| {
      // we default to display warnings and user specified levels.
      if (data.level == .DiagError or data.level == .DiagWarn) {
        std.debug.print("{s}", .{data.msg});
      }
    }
    self.data.clearRetainingCapacity();
  }
};
