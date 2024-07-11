const std = @import("std");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;

pub fn ArrayList(comptime T: type) type {
  const CmpFn = fn (a: T, b: T) callconv(.Inline) bool;
  const FilterFn = fn (a: T) callconv(.Inline) bool;
  return struct {
    list: std.ArrayList(T),

    pub inline fn init(al: Allocator) @This() {
      return .{.list = std.ArrayList(T).init(al)};
    }

    pub inline fn initCapacity(cap: usize, al: Allocator) @This() {
      return .{.list = std.ArrayList(T).initCapacity(al, cap) catch std.ArrayList(T).init(al)};
    }

    pub inline fn initWith(al: Allocator, item: T) @This() {
      var list = @This() {.list = std.ArrayList(T).initCapacity(al, 1) catch @panic("ArrayList.initWith")};
      list.appendAssumeCapacity(item);
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
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn appendAssumeCapacity(self: *@This(), item: T) void {
      self.list.appendAssumeCapacity(item);
    }

    pub inline fn prepend(self: *@This(), item: T) void {
      self.list.insert(0, item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn insert(self: *@This(), n: usize, item: T) void {
      self.list.insert(n, item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn replaceRange(self: *@This(), start: usize, length: usize, new: []const T) void {
      self.list.replaceRange(start, length, new) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn appendSlice(self: *@This(), item: []const T) void {
      self.list.appendSlice(item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn appendSliceAssumeCapacity(self: *@This(), item: []const T) void {
      self.list.appendSliceAssumeCapacity(item);
    }

    pub fn count(self: *@This(), comptime func: fn(_: T) bool) usize {
      var i = @as(usize, 0);
      for (self.list.items) |itm| {
        if (func(itm)) i += 1;
      }
      return i;
    }

    pub fn indexOf(self: *@This(), item: T, comptime cmp_fn: CmpFn) i32 {
      for (self.list.items, 0..) |itm, i| {
        if (cmp_fn(itm, item)) return @intCast(i);
      }
      return -1;
    }
  
    pub inline fn len(self: *const @This()) usize {
      return self.list.items.len;
    }

    pub fn extend(self: *@This(), src: *const @This()) void {
      self.list.appendSlice(src.list.items[0..src.list.items.len]) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
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

    pub fn contains(self: *@This(), item: T, comptime cmp_fn: CmpFn) bool {
      for (self.list.items) |itm| {
        if (cmp_fn(itm, item)) return true;
      }
      return false;
    }

    pub fn filter(self: *const @This(), comptime predicate: FilterFn) @This() {
      var new = @This().init(self.allocator());
      for (self.list.items) |itm| {
        if (predicate(itm)) {
          new.append(itm);
        }
      }
      return new;
    }

    pub inline fn clearAndFree(self: *@This()) void {
      self.list.clearAndFree();
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize) void {
      self.list.ensureTotalCapacity(new_cap) catch {};
    }

    pub inline fn allocator(self: *const @This()) Allocator {
      return self.list.allocator;
    }

    pub inline fn capacity(self: *@This()) usize {
      return self.list.capacity;
    }

    pub inline fn getLast(self: *@This()) T {
      return self.list.getLast();
    }

    pub fn clone(ori: *ArrayList(T)) ArrayList(T) {
      var new = ArrayList(T).init(ori.list.allocator);
      new.ensureTotalCapacity(ori.len());
      for (ori.items()) |itm| {
        new.appendAssumeCapacity(itm.clone(ori.list.allocator));
      }
      return new;
    }

    pub fn copy(ori: *ArrayList(T)) ArrayList(T) {
      var new = ArrayList(T).init(ori.allocator());
      new.ensureTotalCapacity(ori.len());
      new.appendSliceAssumeCapacity(ori.items());
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

    pub fn deinit(self: *@This()) void {
      self.list.deinit();
    }

    pub inline fn clearRetainingCapacity(self: *@This()) void {
      self.list.clearRetainingCapacity();
    }
  };
}

pub fn ArrayListUnmanaged(comptime T: type) type {
  const CmpFn = fn (a: T, b: T) callconv(.Inline) bool;
  const FilterFn = fn (a: T) callconv(.Inline) bool;
  const FilterWithFn = fn (a: T, args: anytype) callconv(.Inline) bool;
  return struct {
    list: std.ArrayListUnmanaged(T),

    pub inline fn init() @This() {
      return .{.list = std.ArrayListUnmanaged(T){}};
    }

    pub inline fn initCapacity(cap: usize, al: Allocator) @This() {
      return .{.list = std.ArrayListUnmanaged(T).initCapacity(al, cap) catch std.ArrayListUnmanaged(T){}};
    }

    pub inline fn initWith(al: Allocator, item: T) @This() {
      var list = @This() {.list = std.ArrayListUnmanaged(T).initCapacity(al, 1) catch undefined};
      list.appendAssumeCapacity(item);
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

    inline fn growCapacity(cap: usize) usize {
      return if (cap < 8) 8 else cap << 1;
    }

    pub inline fn append(self: *@This(), item: T, al: Allocator) void {
      self.list.append(al, item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn appendAssumeCapacity(self: *@This(), item: T) void {
      self.list.appendAssumeCapacity(item);
    }

    pub inline fn prepend(self: *@This(), item: T, al: Allocator) void {
      self.list.insert(al, 0, item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn insert(self: *@This(), n: usize, item: T, al: Allocator) void {
      self.list.insert(al, n, item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn replaceRange(self: *@This(), start: usize, length: usize, new: []const T, al: Allocator) void {
      self.list.replaceRange(al, start, length, new) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn appendSlice(self: *@This(), item: []const T, al: Allocator) void {
      self.list.appendSlice(al, item) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub inline fn appendSliceAssumeCapacity(self: *@This(), item: []const T) void {
      self.list.appendSliceAssumeCapacity(item);
    }

    pub fn count(self: *@This(), comptime func: fn(_: T) bool) usize {
      var i = @as(usize, 0);
      for (self.list.items) |itm| {
        if (func(itm)) i += 1;
      }
      return i;
    }

    pub fn indexOf(self: *@This(), item: T, comptime cmp_fn: CmpFn) i32 {
      for (self.list.items, 0..) |itm, i| {
        if (cmp_fn(itm, item)) return @intCast(i);
      }
      return -1;
    }
  
    pub inline fn len(self: *const @This()) usize {
      return self.list.items.len;
    }

    pub fn extend(self: *@This(), src: *const @This(), al: Allocator) void {
      self.list.appendSlice(al, src.list.items[0..src.list.items.len]) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
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

    pub fn contains(self: *@This(), item: T, comptime cmp_fn: CmpFn) bool {
      for (self.list.items) |itm| {
        if (cmp_fn(itm, item)) return true;
      }
      return false;
    }

    pub fn filter(self: *const @This(), comptime predicate: FilterFn, al: Allocator) @This() {
      var new = @This().init();
      for (self.list.items) |itm| {
        if (predicate(itm)) {
          new.append(itm, al);
        }
      }
      return new;
    }

    pub fn filterWith(self: *const @This(), comptime predicate: FilterWithFn, al: Allocator, args: anytype) @This() {
      var new = @This().init();
      for (self.list.items) |itm| {
        if (predicate(itm, args)) {
          new.append(itm, al);
        }
      }
      return new;
    }
  
    pub inline fn clearAndFree(self: *@This(), al: Allocator) void {
      self.list.clearAndFree(al);
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize, al: Allocator) void {
      self.list.ensureTotalCapacity(al, new_cap) catch {};
    }

    pub inline fn ensureTotalCapacityAndLen(self: *@This(), new_cap: usize, al: Allocator, val: T) void {
      self.list.ensureTotalCapacity(al, new_cap) catch {};
      self.list.items.len = self.list.capacity;
      @memset(self.list.items, val);
    }

    pub inline fn capacity(self: *@This()) usize {
      return self.list.capacity;
    }

    pub inline fn getLast(self: *@This()) T {
      return self.list.getLast();
    }

    pub fn clone(ori: *ArrayListUnmanaged(T), al: Allocator) ArrayListUnmanaged(T) {
      var new = ArrayListUnmanaged(T).init();
      new.ensureTotalCapacity(ori.len(), al);
      for (ori.items()) |itm| {
        new.appendAssumeCapacity(itm.clone(al));
      }
      return new;
    }

    pub fn copy(ori: *ArrayListUnmanaged(T), al: Allocator) ArrayListUnmanaged(T) {
      var new = ArrayListUnmanaged(T).init();
      new.ensureTotalCapacity(ori.len(), al);
      new.appendSliceAssumeCapacity(ori.items());
      return new;
    }
  
    pub inline fn box(self: ArrayListUnmanaged(T), al: Allocator) *ArrayListUnmanaged(T) {
      return util.box(ArrayListUnmanaged(T), self, al);
    }

    pub inline fn boxEnsureCapacity(cap: usize, al: Allocator) *ArrayListUnmanaged(T) {
      return util.box(ArrayListUnmanaged(T), ArrayListUnmanaged(T).initCapacity(cap, al), al);
    }

    pub inline fn writer(self: *@This()) std.ArrayList(T).Writer {
      return self.list.writer();
    }

    pub fn deinit(self: *@This(), al: Allocator) void {
      self.list.deinit(al);
    }

    pub inline fn clearRetainingCapacity(self: *@This()) void {
      self.list.clearRetainingCapacity();
    }
  };
}

pub fn ArrayHashMap(comptime K: type, comptime V: type) type {
  return struct {
    map: T,

    const T = if (K != []const u8) std.AutoArrayHashMap(K, V) else std.StringArrayHashMap(V);

    pub inline fn init(al: Allocator) @This() {
      return .{.map = T.init(al)};
    }

    pub inline fn iterator(self: *@This()) T.Iterator {
      return self.map.iterator();
    }

    pub fn set(self: *@This(), key: K, val: V) void {
      self.map.put(key, val) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub fn setAssumeCapacity(self: *@This(), key: K, val: V,) void {
      self.map.putAssumeCapacity(key, val);
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize) void {
      self.map.ensureTotalCapacity(new_cap) catch {};
    }

    pub inline fn keys(self: *@This()) []K {
      return self.map.keys();
    }

    pub inline fn values(self: *@This()) []V {
      return self.map.values();
    }

    pub inline fn count(self: *const @This()) usize {
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

    pub fn copy(self: *@This()) @This() {
      const map = self.map.clone() catch {
        var new = @This() {.map = T.init(self.map.allocator)};
        var itr = self.map.iterator();
        while (itr.next()) |entry| {
          new.set(entry.key_ptr.*, entry.value_ptr.*);
        }
        return new;
      };
      return .{.map = map};
    }

    pub inline fn clearAndFree(self: *@This()) void {
      self.map.clearAndFree();
    }

    pub inline fn clearRetainingCapacity(self: *@This()) void {
      self.map.clearRetainingCapacity();
    }

    pub inline fn get(self: *@This(), k: K) ?V {
      return self.map.get(k);
    }
  };
}

pub fn HashMap(comptime K: type, comptime V: type, comptime Context: type, comptime max_load_perc: u64) type {
  return struct {
    map: T,

    const T = std.HashMap(K, V, Context, max_load_perc);

    pub inline fn init(al: Allocator) @This() {
      return .{.map = T.init(al)};
    }

    pub inline fn iterator(self: *@This()) T.Iterator {
      return self.map.iterator();
    }

    pub fn set(self: *@This(), key: K, val: V) void {
      self.map.put(key, val) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub fn setAssumeCapacity(self: *@This(), key: K, val: V,) void {
      self.map.putAssumeCapacity(key, val);
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize) void {
      self.map.ensureTotalCapacity(new_cap) catch {};
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

    pub fn copy(self: *@This()) @This() {
      const map = self.map.clone() catch {
        var new = @This() {.map = T.init(self.map.allocator)};
        var itr = self.map.iterator();
        while (itr.next()) |entry| {
          new.set(entry.key_ptr.*, entry.value_ptr.*);
        }
        return new;
      };
      return .{.map = map};
    }

    pub inline fn clearAndFree(self: *@This()) void {
      self.map.clearAndFree();
    }

    pub inline fn clearRetainingCapacity(self: *@This()) void {
      self.map.clearRetainingCapacity();
    }

    pub inline fn get(self: *@This(), k: K) ?V {
      return self.map.get(k);
    }

    pub inline fn getPtr(self: *@This(), k: K) ?*V {
      return self.map.getPtr(k);
    }
  };
}

pub fn HashMapUnmanaged(comptime K: type, comptime V: type, comptime Context: type, comptime max_load_perc: u64) type {
  return struct {
    map: T,

    const T = std.HashMapUnmanaged(K, V, Context, max_load_perc);

    pub inline fn init() @This() {
      return .{.map = T{}};
    }

    pub inline fn iterator(self: *@This()) T.Iterator {
      return self.map.iterator();
    }

    pub fn set(self: *@This(), key: K, val: V, al: Allocator) void {
      self.map.put(al, key, val) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub fn setAssumeCapacity(self: *@This(), key: K, val: V) void {
      self.map.putAssumeCapacity(key, val);
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize, al: Allocator) void {
      self.map.ensureTotalCapacity(al, new_cap) catch {};
    }

    pub inline fn count(self: *const @This()) usize {
      return self.map.count();
    }

    pub inline fn capacity(self: *@This()) usize {
      return self.map.capacity();
    }

    pub inline fn isEmpty(self: *@This()) bool {
      return self.map.count() == 0;
    }

    pub inline fn isNotEmpty(self: *@This()) bool {
      return self.map.count() > 0;
    }

    pub fn copy(self: *@This(), al: Allocator) @This() {
      const map = self.map.clone(al) catch {
        var new = @This() {.map = T{}};
        var itr = self.map.iterator();
        while (itr.next()) |entry| {
          new.set(entry.key_ptr.*, entry.value_ptr.*, al);
        }
        return new;
      };
      return .{.map = map};
    }

    pub inline fn clearAndFree(self: *@This(), al: Allocator) void {
      self.map.clearAndFree(al);
    }

    pub inline fn clearRetainingCapacity(self: *@This()) void {
      self.map.clearRetainingCapacity();
    }

    pub inline fn get(self: *@This(), k: K) ?V {
      return self.map.get(k);
    }
  };
}

pub fn ArrayHashMapUnmanaged(comptime K: type, comptime V: type) type {
  return struct {
    map: Map,

    const Map = if (K != []const u8) std.AutoArrayHashMapUnmanaged(K, V) else std.StringArrayHashMapUnmanaged(V);

    pub inline fn init() @This() {
      return .{.map = Map{}};
    }

    pub inline fn iterator(self: *@This()) Map.Iterator {
      return self.map.iterator();
    }

    pub fn set(self: *@This(), key: K, val: V, al: Allocator) void {
      self.map.put(al, key, val) catch |e| {
        util.logger.debug("error: {}", .{e});
        std.posix.exit(1);
      };
    }

    pub fn setAssumeCapacity(self: *@This(), key: K, val: V,) void {
      self.map.putAssumeCapacity(key, val);
    }

    pub inline fn ensureTotalCapacity(self: *@This(), new_cap: usize, al: Allocator) void {
      self.map.ensureTotalCapacity(al, new_cap) catch {};
    }

    pub inline fn values(self: *const @This()) []V {
      return self.map.values();
    }

    pub inline fn count(self: *@This()) usize {
      return self.map.count();
    }

    pub inline fn capacity(self: *@This()) usize {
      return self.map.capacity();
    }

    pub inline fn isEmpty(self: *@This()) bool {
      return self.map.count() == 0;
    }

    pub inline fn isNotEmpty(self: *@This()) bool {
      return self.map.count() > 0;
    }

    pub fn copy(self: *@This(), al: Allocator) @This() {
      const map = self.map.clone(al) catch {
        var new = @This() {.map = undefined};
        var itr = self.map.iterator();
        while (itr.next()) |entry| {
          new.set(entry.key_ptr.*, entry.value_ptr.*, al);
        }
        return new;
      };
      return .{.map = map};
    }

    pub inline fn clearAndFree(self: *@This(), al: Allocator) void {
      self.map.clearAndFree(al);
    }

    pub inline fn get(self: *@This(), k: K) ?V {
      return self.map.get(k);
    }

    pub inline fn getPtr(self: *@This(), k: K) ?*V {
      return self.map.getPtr(k);
    }
  };
}

pub fn StringHashMap(comptime V: type) type {
  return HashMap([]const u8, V, std.hash_map.StringContext, std.hash_map.default_max_load_percentage);
}

pub fn StringHashMapUnmanaged(comptime V: type) type {
  return HashMapUnmanaged([]const u8, V, std.hash_map.StringContext, std.hash_map.default_max_load_percentage);
}

pub fn StringArrayHashMap(comptime V: type) type {
  return ArrayHashMap([]const u8, V);
}

pub fn StringArrayHashMapUnmanaged(comptime V: type) type {
  return ArrayHashMapUnmanaged([]const u8, V);
}
