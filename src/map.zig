const std = @import("std");
const Mem = @import("mem.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;
const vl = @import("value.zig");

const StringType = *const vl.ObjString;
const LOAD_FACTOR: f64 = @as(f64, vl.LOAD_FACTOR) / @as(f64, 100);

pub fn Map(comptime K: type, comptime V: type) type {
  const MapItem = struct {hash: u64, key: K, value: V};
  return extern struct {
    /// actual entries, but with indices
    entries: [*]i32,
    /// compact entries
    items: [*]MapItem,
    /// helpers
    entries_cap: usize,
    items_cap: usize,
    len: usize,
    
    const Self = @This();

    const ctx = if (K == StringType) StringContext{} else ValueContext {};
    const FreeEntry = -1;
    const TombEntry = -2;

    pub fn init() Self {
      return Self {
        .entries = &[_]i32{},
        .items = &[_]MapItem{},
        .entries_cap = 0,
        .items_cap = 0,
        .len = 0
      };
    }

    pub inline fn isFreeEntry(self: *Self, entry: i32) bool {
      _ = self;
      return entry == FreeEntry;
    }

    pub inline fn isTombEntry(self: *Self, entry: i32) bool {
      _ = self;
      return entry == TombEntry;
    }

    pub inline fn isUsedEntry(self: *Self, entry: i32) bool {
      return !self.isFreeEntry(entry) and !self.isTombEntry(entry);
    }

    pub const StringContext = struct {
      const CtxK = *const vl.ObjString;
      pub fn hash(self: @This(), k: CtxK, vm: *VM) u64 {
        _ = vm;
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
      pub fn hash(self: @This(), k: CtxK, vm: *VM) u64 {
        _ = self;
        return vl.hashValue(k, vm);
      }

      pub fn eql(self: @This(), k1: CtxK, k2: CtxK) bool {
        _ = self;
        return vl.valueEqual(k1, k2);
      }
    };

    inline fn shouldResizeEntries(self: *Self) bool {
      return (self.len >= @as(usize, @intFromFloat((@as(f64, @floatFromInt(self.entries_cap)) * LOAD_FACTOR))));
    }

    inline fn shouldResizeItems(self: *Self) bool {
      return (self.len >= self.items_cap);
    }

    fn resizeItemsIfNecessary(self: *Self, vm: *VM, ensure: usize, likely: bool) void {
      if (likely) {
        const new_capacity = Mem.growCapacity(self.items_cap + ensure);
        self.items = Mem.resizeBuf(&vm.mem, MapItem, vm, self.items, self.items_cap, new_capacity).ptr;
        self.items_cap = new_capacity;
      }
    }

    inline fn usize_(val: i32) usize {
      return @intCast(val);
    }

    inline fn i32_(val: usize) i32 {
      return @intCast(val);
    }

    fn resizeMap(self: *Self, vm: *VM, ensure: usize) void {
      self.resizeItemsIfNecessary(vm, ensure, self.shouldResizeItems());
      const new_capacity = Mem.growCapacity(self.entries_cap + ensure);
      const tmp = vm.mem.allocBuf(i32, new_capacity, vm);
      std.debug.assert(new_capacity == tmp.len);
      @memset(tmp, FreeEntry);
      var new_entries: [*]i32 = tmp.ptr;
      self.len = 0; // reset len for resizing.
      for (self.entries[0..self.entries_cap]) |entry| {
        if (self.isUsedEntry(entry)) {
          const item = self.items[usize_(entry)];
          const slot = self.findEntry(new_entries, new_capacity, item.key, item.hash);
          new_entries[slot] = entry;
          self.len += 1;
        }
      }
      vm.mem.freeBuf(i32, vm, self.entries[0..self.entries_cap]);
      self.entries = new_entries;
      self.entries_cap = new_capacity;
    }

    fn findEntry(self: *Self, entries: [*]i32, capacity: usize, key: K, hash: u64) usize {
      const mask = capacity - 1;
      var i = hash & mask;
      const start = i;
      var deleted: ?usize = null;
      var entry: i32 = undefined;
      while (true) {
        entry = entries[i];
        if (self.isFreeEntry(entry)) {
          // if we've encountered a tomb entry, return that, otherwise this fresh entry
          return if (deleted) |delt| delt else i;
        } else if (self.isTombEntry(entry)) {
          // save first deleted entry for reuse when possible
          deleted = i;
        } else if (ctx.eql(self.items[usize_(entry)].key, key)) {
          return i;
        }
        i = (i + 1) & mask;
        if (i == start) break;
      }
      util.assert(deleted != null, "map must have at least one deleted slot");
      return deleted.?;
    }

    pub fn ensureCapacity(self: *Self, vm: *VM, capacity: usize) void {
      if (self.entries_cap < capacity) {
        self.resizeMap(vm, capacity);
      }
    }

    pub fn set(self: *Self, key: K, value: V, vm: *VM) bool {
      if (self.shouldResizeEntries()) {
        self.resizeMap(vm, 0);
      } else if (self.shouldResizeItems()) {
        self.resizeItemsIfNecessary(vm, 0, true);
      }
      const hash = ctx.hash(key, vm);
      const slot = self.findEntry(self.entries, self.entries_cap, key, hash);
      const is_new_key = self.isFreeEntry(self.entries[slot]);
      if (is_new_key) {
        self.items[self.len] = .{.hash = hash, .key = key, .value = value};
        self.entries[slot] = i32_(self.len);
        self.len += 1;
      } else {
        self.items[usize_(self.entries[slot])].value = value;
      }
      return is_new_key;
    }

    pub fn get(self: *Self, key: K, vm: *VM) ?V {
      if (self.entries_cap == 0) {
        return null;
      }
      const slot = self.findEntry(self.entries, self.entries_cap, key, ctx.hash(key, vm));
      return if (self.isUsedEntry(self.entries[slot])) self.items[usize_(self.entries[slot])].value else null;
    }

    /// ordered delete
    pub fn delete(self: *Self, key: K, vm: *VM) bool {
      if (self.entries_cap == 0) {
        return false;
      }
      const slot = self.findEntry(self.entries, self.entries_cap, key, ctx.hash(key, vm));
      if (self.isUsedEntry(self.entries[slot])) {
        // move all items after `slot` forward
        const len = self.len - 1;
        const deleted = self.entries[slot];
        const start = usize_(deleted);
        for (start..len) |i| {
          self.items[i] = self.items[i + 1];
          if (i == len) break;
        }
        self.len = len;
        // set tomb entry
        self.entries[slot] = TombEntry;
        for (self.entries[0..self.entries_cap], 0..) |entry, i| {
          if (self.isUsedEntry(entry)) {
            if (entry >= deleted) {
              self.entries[i] -= 1;
            }
          }
        }
        return true;
      }
      return false;
    }

    /// swap delete
    pub fn remove(self: *Self, key: K, vm: *VM) bool {
      if (self.entries_cap == 0) {
        return false;
      }
      const slot = self.findEntry(self.entries, self.entries_cap, key, ctx.hash(key, vm));
      if (self.isUsedEntry(self.entries[slot])) {
        // swap item at slot with item at last
        const last_item = self.items[self.len - 1];
        self.items[usize_(self.entries[slot])] = last_item;
        const last_item_slot = self.findEntry(self.entries, self.entries_cap, last_item.key, last_item.hash);
        self.entries[last_item_slot] = self.entries[slot];
        // set tomb entry
        self.entries[slot] = TombEntry;
        self.len -= 1;
        return true;
      }
      return false;
    }

    pub fn display(self: *Self) void {
      util.print("{s}", .{"{"});
      const last = self.len -| 1;
      for (self.items[0..self.len], 0..) |itm, i| {
        @call(.always_inline, vl.display, .{itm.key});
        util.print(": ", .{});
        @call(.always_inline, vl.display, .{itm.value});
        if (i < last) {
          util.print(", ", .{});
        }
      }
      util.print("{s}", .{"}"});
    }

    pub fn keys(self: *Self, vm: *VM) vl.Value {
      var list = vl.createList(vm, self.len);
      var keyc: usize = 0;
      for (self.items[0..self.len]) |itm| {
        list.items[keyc] = itm.key;
        keyc += 1;
      }
      return vl.objVal(list);
    }

    pub fn values(self: *Self, vm: *VM) vl.Value {
      var list = vl.createList(vm, self.len);
      var keyc: usize = 0;
      for (self.items[0..self.len]) |itm| {
        list.items[keyc] = itm.value;
        keyc += 1;
      }
      return vl.objVal(list);
    }

    /// Map has to be a StringMap to use this method!
    pub fn findInterned(self: *Self, str: []const u8, hash: u64) ?MapItem {
      comptime {
        if (K != StringType) {
          @compileError("findInterned() called with non-stringmap\n");
        }
      }
      if (self.entries_cap == 0) {
        return null;
      }
      const mask = self.entries_cap - 1;
      var i = hash & mask;
      var entry: i32 = undefined;
      while (true) {
        entry = self.entries[i];
        if (self.isFreeEntry(entry)) {
          // if we've encountered a tomb entry, return that otherwise this fresh entry
          return null;
        } else if (self.isTombEntry(entry)) {
          // noop
        } else if (ctx.cmpInterned(self.items[usize_(entry)].key, str, hash)) {
          return self.items[usize_(entry)];
        }
        i = (i + 1) & mask;
      }
      unreachable;
    }

    pub fn free(self: *Self, vm: *VM) void {
      vm.mem.freeBuf(i32, vm, self.entries[0..self.entries_cap]);
      vm.mem.freeBuf(MapItem, vm, self.items[0..self.items_cap]);
    }

    pub fn clearAndFree(self: *Self, vm: *VM) void {
      vm.mem.freeBuf(i32, vm, self.entries[0..self.entries_cap]);
      vm.mem.freeBuf(MapItem, vm, self.items[0..self.items_cap]);
      self.entries_cap = 0;
      self.items_cap = 0;
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
        var entry: *MapItem = undefined;
        while (self.curr < self.map.capacity) {
          entry = &self.map.items[self.curr];
          self.curr += 1;
          return entry.key;
        }
        return null;
      }
    };
  };
}
