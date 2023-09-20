const std = @import("std");
const Mem = @import("mem.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;
const vl = @import("value.zig");

const StringType = *const vl.ObjString;
const LOAD_FACTOR: f64 = @as(f64, vl.LOAD_FACTOR) / @as(f64, 100);

pub fn Map(comptime K: type, comptime V: type) type {
  const KVEntry = struct {key: K, value: V};
  return extern struct {
    entries: [*]KVEntry,
    capacity: usize,
    len: usize,
    
    const Self = @This();

    const ctx = if (K == StringType) StringContext{} else ValueContext {};
    const NullKey = if (K == StringType) &vl.StringNullKey else vl.NOTHING_VAL;
    const TombVal = @as(vl.Value, vl.TRUE_VAL | vl.NOTHING_VAL);

    pub fn init() Self {
      return Self {.entries = &[_]KVEntry{}, .capacity = 0, .len = 0};
    }

    pub inline fn isNullKey(self: *Self, key: K) bool {
      _ = self;
      return key == NullKey;
    }

    inline fn isNullVal(self: *Self, value: V) bool {
      _ = self;
      return value == vl.NOTHING_VAL;
    }

    pub const StringContext = struct {
      const CtxK = *const vl.ObjString;
      pub fn hash(self: @This(), k: CtxK) u64 {
        _ = self;
        return k.hash;
      }

      pub fn eql(self: @This(), k1: CtxK, k2: CtxK) bool {
        _ = self;
        return k1 == k2;
      }

      pub inline fn cmpInterned(self: @This(), key: K, str: []const u8, str_hash: u64) bool {
        _ = self;
        return (key.hash == str_hash and std.mem.eql(u8, key.str[0..key.len], str));
      }
    };

    pub const ValueContext = struct {
      const CtxK = vl.Value;
      pub fn hash(self: @This(), k: CtxK) u64 {
        _ = self;
        return vl.hashValue(k);
      }

      pub fn eql(self: @This(), k1: CtxK, k2: CtxK) bool {
        _ = self;
        return vl.valueEqual(k1, k2);
      }
    };

    fn resizeMap(self: *Self, vm: *VM, ensure: usize) void {
      const new_capacity = Mem.growCapacity(self.capacity + ensure);
      var tmp = vm.mem.allocBuf(KVEntry, new_capacity, vm);
      std.debug.assert(new_capacity == tmp.len);
      for (tmp) |*entry| {
        entry.key = NullKey;
        entry.value = vl.NOTHING_VAL;
      }
      var new_entries: [*]KVEntry = @ptrCast(tmp);
      self.len = 0; // reset len for resizing.
      for (self.entries[0..self.capacity]) |*entry| {
        if (!self.isNullKey(entry.key)) {
          var slot = self.findEntry(new_entries, new_capacity, entry.key);
          slot.* = entry.*;
          self.len += 1;
        }
      }
      vm.mem.freeBuf(KVEntry, vm, self.entries[0..self.capacity]);
      self.entries = new_entries;
      self.capacity = new_capacity;
    }

    fn findEntry(self: *Self, entries: [*]KVEntry, capacity: usize, key: K) *KVEntry {
      const mask = capacity - 1;
      var index: u64 = ctx.hash(key) & mask;
      var start_index = index;
      var deleted: ?*KVEntry = null;
      var entry: *KVEntry = undefined;
      while (true) {
        entry = &entries[index];
        if (self.isNullKey(entry.key)) {
          // means fresh entry
          if (self.isNullVal(entry.value)) {
            return if (deleted) |delt| delt else entry;
          }
          // means deleted entry
          else if (deleted == null) {
            // save first deleted entry for reuse when possible
            deleted = entry;
          }
        } else if (ctx.eql(entry.key, key)) {
          return entry;
        }
        index = (index + 1) & mask;
        // TODO: how to lift this out of the loop body, since zig doesn't support do-while loops
        if (index == start_index) break;
      }
      util.assert(deleted != null, "map must have at least one deleted slot");
      return deleted.?;
    }

    pub fn ensureCapacity(self: *Self, vm: *VM, len: usize) void {
      if (self.capacity < len) {
        self.resizeMap(vm, len);
      }
    }

    pub fn set(self: *Self, key: K, value: V, vm: *VM) bool {
      if (self.len >= @as(usize, @intFromFloat((@as(f64, @floatFromInt(self.capacity)) * LOAD_FACTOR)))) {
        self.resizeMap(vm, 0);
      }
      var entry = self.findEntry(self.entries, self.capacity, key);
      var is_new_key = self.isNullKey(entry.key);
      if (is_new_key) self.len += 1;
      entry.key = key;
      entry.value = value;
      return is_new_key;
    }

    pub fn get(self: *Self, key: K) ?V {
      if (self.capacity == 0) {
        return null;
      }
      var entry = self.findEntry(self.entries, self.capacity, key);
      return if (self.isNullKey(entry.key)) null else entry.value;
    }

    pub fn del(self: *Self, key: K) bool {
      if (self.capacity == 0) {
        return false;
      }
      var entry = self.findEntry(self.entries, self.capacity, key);
      if (self.isNullKey(entry.key)) {
        return false;
      } else {
        self.len -= 1;
        entry.key = NullKey;
        entry.value = TombVal;
        return true;
      }
    }

    pub fn display(self: *Self) void {
      util.print("{s}", .{"{"});
      var i: u32 = 0;
      var last = @as(i64, @intCast(self.len)) - 1;
      for (self.entries[0..self.capacity]) |entry| {
        if (!self.isNullKey(entry.key)) {
          @call(.always_inline, vl.display, .{entry.key});
          util.print(": ", .{});
          @call(.always_inline, vl.display, .{entry.value});
          if (i < last) {
            util.print(", ", .{});
          }
          i += 1;
        }
      }
      util.print("{s}", .{"}"});
    }

    pub fn keys(self: *Self, vm: *VM) vl.Value {
      var list = vl.createList(vm, self.len);
      var entry: *KVEntry = undefined;
      var idx: usize = 0;
      var keyc: usize = 0;
      while (keyc < self.len) {
        entry = &self.entries[idx];
        if (!self.isNullKey(entry.key)) {
          list.items[keyc] = entry.key;
          keyc += 1;
        }
        idx += 1;
      }
      return vl.objVal(list);
    }

    pub fn values(self: *Self, vm: *VM) vl.Value {
      var list = vl.createList(vm, self.len);
      var entry: *KVEntry = undefined;
      var idx: usize = 0;
      var valc: usize = 0;
      while (valc < self.len) {
        entry = &self.entries[idx];
        if (!self.isNullKey(entry.key)) {
          list.items[valc] = entry.value;
          valc += 1;
        }
        idx += 1;
      }
      return vl.objVal(list);
    }

    /// Map has to be a StringMap to use this method!
    pub fn findInterned(self: *Self, str: []const u8, hash: u64) ?K {
      comptime {
        if (K != StringType) {
          @compileError("findInterned() called with non-stringmap\n");
        }
      }
      if (self.capacity == 0) {
        return null;
      }
      const mask = self.capacity - 1;
      var index = hash & mask;
      var entry: *KVEntry = undefined;
      while (true) {
        entry = &self.entries[index];
        if (self.isNullKey(entry.key)) {
          if (self.isNullVal(entry.value)) {
            return null;
          }
        } else if (ctx.cmpInterned(entry.key, str, hash)) {
          return entry.key;
        }
        index = (index + 1) & mask;
      }
      unreachable;
    }

    pub fn free(self: *Self, vm: *VM) void {
      vm.mem.freeBuf(KVEntry, vm, self.entries[0..self.capacity]);
    }

    pub fn clearAndFree(self: *Self, vm: *VM) void {
      vm.mem.freeBuf(KVEntry, vm, self.entries);
      self.capacity = 0;
      self.len = 0;
    }

    pub fn keyIterator(self: *Self) KeyIterator {
      return KeyIterator.init(self);
    }

    const KeyIterator = struct {
      curr: usize,
      map: *Self,

      pub fn init(self: *Self) @This() {
        return @This() {.curr = 0, .map = self};
      }

      pub fn next(self: *@This()) ?K {
        var entry: *KVEntry = undefined;
        while (self.curr < self.map.capacity) {
          entry = &self.map.entries[self.curr];
          self.curr += 1;
          if (!self.map.isNullKey(entry.key)) {
            return entry.key;
          }
        }
        return null;
      }
    };

  };
}
