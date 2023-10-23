const std = @import("std");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;

pub fn ArrayList(comptime T: type) type {
  return struct {
    list: std.ArrayList(T),

    pub inline fn init(al: Allocator) @This() {
      return @This() {.list = std.ArrayList(T).init(al)};
    }

    pub inline fn initWith(al: Allocator, item: T) @This() {
      var list = @This() {.list = std.ArrayList(T).init(al)};
      list.append(item);
      return list;
    }

    pub inline fn items(self: *@This()) []T {
      return self.list.items;
    }

    pub inline fn itemAt(self: *@This(), pos: usize) T {
      return self.list.items[pos];
    }

    pub inline fn isEmpty(self: *@This()) bool {
      return self.list.items.len == 0;
    }

    pub inline fn isNotEmpty(self: *@This()) bool {
      return self.list.items.len > 0;
    }

    pub inline fn append(self: *@This(), item: T) void {
      self.list.append(item) catch |e| {
        std.debug.print("error: {}", .{e});
        std.os.exit(1);
      };
    }

    pub inline fn prepend(self: *@This(), item: T) void {
      self.list.insert(0, item) catch |e| {
        std.debug.print("error: {}", .{e});
        std.os.exit(1);
      };
    }

    pub inline fn replaceRange(self: *@This(), start: usize, length: usize, new: []const T) void {
      self.list.replaceRange(start, length, new) catch |e| {
        std.debug.print("error: {}", .{e});
        std.os.exit(1);
      };
    }

    pub inline fn appendSlice(self: *@This(), item: []const T) void {
      self.list.appendSlice(item) catch |e| {
        std.debug.print("error: {}", .{e});
        std.os.exit(1);
      };
    }

    pub fn count(self: *@This(), comptime func: fn(_: T) bool) usize {
      var i = @as(usize, 0);
      for (self.list.items) |itm| {
        if (func(itm)) i += 1;
      }
      return i;
    }

    pub inline fn len(self: *@This()) usize {
      return self.list.items.len;
    }

    pub fn extend(self: *@This(), src: *const @This()) void {
      self.list.appendSlice(src.list.items[0..src.list.items.len]) catch |e| {
        std.debug.print("error: {}", .{e});
        std.os.exit(1);
      };
    }

    pub fn reverse(self: *@This()) void {
      if (self.list.items.len == 0) return;
      var j = self.list.items.len - 1;
      for (self.list.items, 0..) |itm, i| {
        if (i >= j) break;
        self.list.items[i] = self.list.items[j];
        self.list.items[j] = itm;
        j -= 1;
      }
    }

    pub inline fn pop(self: *@This()) T {
      return self.list.pop();
    }

    const CmpFn = fn (a: T, b: T) callconv(.Inline) bool;
    pub fn contains(self: *@This(), item: T, comptime cmp_fn: CmpFn) bool {
      for (self.list.items) |itm| {
        if (cmp_fn(itm, item)) return true;
      }
      return false;
    }

    pub inline fn clearAndFree(self: *@This()) void {
      self.list.clearAndFree();
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize) void {
      self.list.ensureTotalCapacity(new_cap) catch {};
    }

    pub inline fn allocator(self: *@This()) Allocator {
      return self.list.allocator;
    }

    pub inline fn capacity(self: *@This()) usize {
      return self.list.capacity;
    }

    pub inline fn getLast(self: *@This()) T {
      return self.list.getLast();
    }

    pub fn clone(ori: *ArrayList(T), al: std.mem.Allocator) ArrayList(T) {
      var new = ArrayList(T).init(al);
      new.ensureTotalCapacity(ori.capacity());
      for (ori.items()) |itm| {
        new.append(itm.clone(al));
      }
      return new;
    }

    pub inline fn box(self: ArrayList(T)) *ArrayList(T) {
      return util.box(ArrayList(T), self, self.list.allocator);
    }

    pub inline fn boxEnsureCapacity(self: ArrayList(T), cap: usize) *ArrayList(T) {
      return util.boxEnsureCapacity(ArrayList(T), self, self.list.allocator, cap);
    }

    pub inline fn writer(self: *@This()) std.ArrayList(T).Writer {
      return self.list.writer();
    }

    pub inline fn clearRetainingCapacity(self: *@This()) void {
      self.list.clearRetainingCapacity();
    }
  };
}

pub fn ArrayHashMap(comptime K: type, comptime V: type) type {
  return struct {
    map: std.AutoArrayHashMap(K, V),

    pub inline fn init(al: Allocator) @This() {
      return @This() {.map = std.AutoArrayHashMap(K, V).init(al)};
    }

    pub inline fn iterator(self: *@This()) std.AutoArrayHashMap(K, V).Iterator {
      return self.map.iterator();
    }

    pub fn set(self: *@This(), key: K, val: V) void {
      self.map.put(key, val) catch |e| {
        std.debug.print("error: {}", .{e});
        std.os.exit(1);
      };
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize) void {
      self.map.ensureTotalCapacity(new_cap) catch {};
    }

    pub inline fn values(self: *@This()) []V {
      return self.map.values();
    }

    pub inline fn count(self: *@This()) usize {
      return self.map.count();
    }

    pub inline fn capacity(self: *@This()) usize {
      return self.map.capacity();
    }

    pub inline fn allocator(self: *@This()) Allocator {
      return self.map.allocator;
    }

    pub inline fn isEmpty(self: *@This()) bool {
      return self.map.count() == 0;
    }

    pub inline fn isNotEmpty(self: *@This()) bool {
      return self.map.count() > 0;
    }

    pub fn clone(self: *@This()) @This() {
      var map = self.map.clone() catch {
        var new = @This() {.map = std.AutoArrayHashMap(K, V).init(self.map.allocator)};
        var itr = self.map.iterator();
        while (itr.next()) |entry| {
          new.set(entry.key_ptr.*, entry.value_ptr.*);
        }
        return new;
      };
      return @This() {.map = map};
    }

    pub inline fn clearAndFree(self: *@This()) void {
      self.map.clearAndFree();
    }

    pub inline fn get(self: *@This(), k: K) ?V {
      return self.map.get(k);
    }
  };
}
