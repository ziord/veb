const std = @import("std");
const builtin = @import("builtin");

pub inline fn append(comptime T: type, list: *std.ArrayList(T), val: T) void {
  list.append(val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.os.exit(1);
  };
}

pub inline fn extend(comptime T: type, target: *std.ArrayList(T), src: *std.ArrayList(T)) void {
  target.appendSlice(src.items[0..src.items.len]) catch |e| {
    std.debug.print("error: {}", .{e});
    std.os.exit(1);
  };
}

pub inline fn set(comptime K: type, comptime V: type, map: *std.AutoArrayHashMap(K, V), key: K, val: V) void {
  map.put(key, val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.os.exit(1);
  };
}

pub inline fn setStr(comptime V: type, map: *std.StringHashMap(V), key: []const u8, val: V) void {
  map.put(key, val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.os.exit(1);
  };
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
  var out_w = std.io.getStdOut().writer();
  out_w.print(fmt, args) catch return;
}

pub fn error_(comptime fmt: []const u8, args: anytype) noreturn {
  std.debug.print(fmt ++ "\n", args);
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

pub inline fn alloc(comptime T: type, allocator: std.mem.Allocator) *T {
  return allocator.create(T) catch |e| {
    std.debug.print("AllocationError {}", .{e});
    std.os.exit(1);
  };
}

pub inline fn box(comptime T: type, val: T, allocator: std.mem.Allocator) *T {
  var item = allocator.create(T) catch {
    std.debug.print("Allocation failed\n", .{});
    std.os.exit(1);
  };
  item.* = val;
  return item;
}

pub fn boxEnsureCapacity(comptime T: type, val: T, al: std.mem.Allocator, cap: usize) *T {
  var item = @call(.always_inline, box, .{T, val, al});
  item.ensureTotalCapacity(cap);
  return item;
}

pub inline fn todo(msg: []const u8) noreturn {
  @panic("Todo! " ++ msg ++ "\n");
}
