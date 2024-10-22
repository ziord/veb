const std = @import("std");
const ds = @import("ds.zig");
const util = @import("util.zig");
const lex = @import("lex.zig");
const ks = @import("constants.zig");

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
  data: ds.ArrayList(DiagData),
  levels: ds.ArrayList(DiagLevel),
  srcs: ds.StringArrayHashMap([]const u8),
  skip_entry: bool = false,
  /// entry file name
  filename: *const[]const u8,
  /// entry src
  src: *[]const u8,

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator, filename: *const[]const u8, src: *[]const u8) Self {
    return Self {
      .data = ds.ArrayList(DiagData).init(allocator),
      .levels = ds.ArrayList(DiagLevel).init(allocator),
      .srcs = ds.StringArrayHashMap([]const u8).init(allocator),
      .filename = filename,
      .src = src,
    };
  }

  fn alreadyIn(self: *Self, token: Token, msg: []const u8) bool {
    for (self.data.items()) |data| {
      // we also check the msg because tokens are shared among nodes
      if (token.equal(data.token) and std.mem.eql(u8, data.msg, msg)) {
        return true;
      }
    }
    return false;
  }

  inline fn tokenFilename(self: *Self, token: Token) []const u8 {
    return self.srcs.keys()[@intCast(token.file)];
  }

  pub inline fn getFilename(self: *Self) []const u8 {
    return self.filename.*;
  }

  pub inline fn getFile(self: *const Self) usize {
    return self.srcs.count() - 1;
  }

  pub inline fn getFileLen(self: *Self) u16 {
    return self.srcs.count();
  }

  pub inline fn getSrc(self: *Self) []const u8 {
    return self.src.*;
  }

  fn pushData(self: *Self, level: DiagLevel, token: Token, depth: u32, comptime fmt: []const u8, args: anytype) !void {
    if (self.skip_entry) return;
    const allocator = self.data.allocator();
    const filename = self.tokenFilename(token);
    const src = self.srcs.get(filename).?;
    const col = token.column(src);
    const msg = try std.fmt.allocPrint(allocator, fmt ++ "\n\n", args);
    if (self.alreadyIn(token, msg)) return;
    self.data.append(.{.level = level, .msg = msg, .token = token});
    for (0..depth) |_| {
      self.data.append(.{.level = level, .msg = " ", .token = token});
    }
    self.data.append(
      .{
        .level = level,
        .token = token,
        .msg = try std.fmt.allocPrint(allocator, "{s}.{}:{}:\n", .{filename, token.line, col})
      }
    );
    for (0..depth + 2) |_| {
      self.data.append(.{.level = level, .msg = " ", .token = token});
    }
    self.data.append(.{.level = level, .msg = token.getLine(src), .token = token});
    self.data.append(.{.level = level, .msg = "\n", .token = token});
    const lexeme = token.lexeme();
    var i = (depth + 2) + if (col >= lexeme.len) col - lexeme.len else lexeme.len - col;
    while (i > 0) {
      self.data.append(.{.level = level, .msg = " ", .token = token});
      i -= 1;
    }
    try self.printSquig(level, if (lexeme.len != 0) lexeme.len else 1, token);
    self.data.append(.{.level = level, .msg = "\n", .token = token});
  }

  fn printSquig(self: *Self, level: DiagLevel, i: usize, token: Token) !void {
    var y = i;
    while (y > 0) {
      self.data.append(
        .{
          .level = level,
          .token = token,
          .msg = try std.fmt.allocPrint(self.data.allocator(), "{s:^}", .{"^"}),
        }
      );
      y -= 1;
    }
  }

  inline fn has(self: *Self, level: DiagLevel) bool {
    for (self.data.items()) |item| {
      if (item.level == level) return true;
    }
    return false;
  }

  pub inline fn skipEntry(self: *Self) void {
    self.skip_entry = true;
  }

  pub inline fn resumeEntry(self: *Self) void {
    self.skip_entry = false;
  }

  pub fn hasErrors(self: *Self) bool {
    return self.has(.DiagError) or self.has(.DiagIError);
  }

  pub fn hasAny(self: *Self) bool {
    return self.data.isNotEmpty();
  }

  pub inline fn count(self: *Self) usize {
    return self.data.len();
  }

  pub fn popUntil(self: *Self, size: usize) void {
    while (self.data.len() > size) {
      _ = self.data.pop();
    }
  }

  pub fn pushLevel(self: *Self, level: DiagLevel) void {
    self.levels.append(level);
  }

  pub fn popLevel(self: *Self) DiagLevel {
    return if (self.levels.isNotEmpty()) self.levels.pop() else .DiagError;
  }
  
  pub inline fn getLevel(self: *Self) DiagLevel {
    if (self.levels.isNotEmpty()) {
      return self.levels.items()[self.levels.len() - 1];
    }
    return .DiagError;
  }

  pub inline fn addSrcFile(self: *Self, filename: []const u8, src: []const u8) void {
    self.srcs.set(filename, src);
  }

  pub fn addDiagnostics(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.pushData(self.getLevel(), token, 2, fmt, args) catch |e| {
      util.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn addDiagnosticsWithDepth(self: *Self, token: Token, depth: u32, comptime fmt: []const u8, args: anytype) void {
    self.pushData(self.getLevel(), token, depth, fmt, args) catch |e| {
      util.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn addDiagnosticsDirect(self: *Self, token: Token, msg: []const u8) void {
    self.data.append(.{.level = self.getLevel(), .msg = msg, .token = token});
  }

  pub fn addDiagnosticsSliceDirect(self: *Self, token: Token, msgs: []const []const u8) void {
    for (msgs) |msg| {
      self.data.append(.{.level = self.getLevel(), .msg = msg, .token = token});
    }
  }

  pub fn addDiagnosticsWithLevel(self: *Self, level: DiagLevel, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.pushData(level, token, 2, fmt, args) catch |e| {
      util.print("Could not add diagnostic: {}", .{e});
    };
  }

  pub fn display(self: *Self) void {
    for (self.data.items()) |data| {
      // we default to display warnings and user specified levels.
      if (data.level == .DiagError or data.level == .DiagWarn) {
        util.print("{s}", .{data.msg});
      }
    }
    self.data.clearRetainingCapacity();
  }
};
