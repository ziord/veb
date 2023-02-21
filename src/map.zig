const std = @import("std");
const Mem = @import("mem.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;
const vl = @import("value.zig");

const LOAD_FACTOR = vl.LOAD_FACTOR / 100;
const TOMB_VAL = @as(vl.Value, vl.TRUE_VAL | vl.NOTHING_VAL);
const StringType = *const vl.ObjString;

/// A generic map implementable for map & stringmap types in Nova.
/// We expect Context to have *4* methods: 
///  eql(key1: K, key2: K) bool
///  hash(key: K) u64
///  isNullKey(key: K) bool
///  isNullVal(value: V) bool
/// Note:
/// We could also require a tombVal() for assigning tombvals for deleted entries, 
/// but that isn't necessary since this isn't a library 💀
pub fn Map(
  comptime K: type,
  comptime V: type,
  comptime Context: type,
  comptime null_key: anytype,
) type {
  const KVEntry = struct {key: K, value: V};
  return struct {
    entries: [*]KVEntry,
    capacity: usize,
    len: usize,
    ctx: Context,
    
    const Self = @This();

    // for now, only verify eql() & hash() methods of Context.
    // TODO: need to verify isNullKey() & isNullVal() at some point but it's too much work for now. 
    //       See [Note] above.
    comptime {
      std.hash_map.verifyContext(Context, K, K, u64, false);
    }

  pub fn init() Self {
    return Self {.entries = &[_]KVEntry{}, .capacity = 0, .len = 0, .ctx = undefined};
  }

  fn resizeMap(self: *Self, vm: *VM) void {
    const new_capacity = Mem.growCapacity(self.capacity);
    var tmp = vm.gc.mem.allocBuf(KVEntry, vm, new_capacity);
    std.debug.assert(new_capacity == tmp.len);
    for (tmp) |*entry| {
      entry.key = null_key;
      entry.value = vl.NOTHING_VAL;
    }
    var new_entries = @ptrCast([*]KVEntry, tmp);
    self.len = 0; // reset len for resizing.
    for (self.entries[0..self.capacity]) |*entry| {
      if (!self.ctx.isNullKey(entry.key)) {
        var slot = self.findEntry(new_entries, new_capacity, entry.key);
        slot.* = entry.*;
        self.len += 1;
      }
    }
    vm.gc.mem.freeBuf(KVEntry, vm, self.entries[0..self.capacity]);
    self.entries = new_entries;
    self.capacity = new_capacity;
  }

  fn findEntry(self: *Self, entries: [*]KVEntry, capacity: usize, key: K) *KVEntry {
    const mask = capacity - 1;
    var index: u64 = self.ctx.hash(key) & mask;
    var start_index = index;
    var deleted: ?*KVEntry = null;
    var entry: *KVEntry = undefined;
    while (true) {
      entry = &entries[index];
      if (self.ctx.isNullKey(entry.key)) {
        // means fresh entry
        if (vl.isNothing(entry.value)) {
          return if (deleted) |delt| delt else entry;
        }
        // means deleted entry
        else if (deleted == null) {
          // save first deleted entry for reuse when possible
          deleted = entry;
        }
      } else if (self.ctx.eql(entry.key, key)) {
        return entry;
      }
      index = (index + 1) & mask;
      comptime {
        if (K != StringType) {
          // only check if not string type
          if (index == start_index) break;
        }
      }
    }
    util.assert(deleted != null, "map must have at least one deleted slot");
    return deleted.?;
  }

  pub fn put(self: *Self, key: K, value: V, vm: *VM) bool {
    if (self.len >= (self.capacity * LOAD_FACTOR)) {
      self.resizeMap(vm);
    }
    var entry = self.findEntry(self.entries, self.capacity, key);
    var is_new_key = self.ctx.isNullKey(entry.key);
    if (is_new_key and self.ctx.isNullVal(entry.value)) {
      // only increment len if not re-using a deleted/tombstone entry
      self.len += 1;
    }
    entry.key = key;
    entry.value = value;
    return is_new_key;
  }

  pub fn get(self: *Self, key: K) ?V {
    if (self.capacity == 0) {
      return null;
    }
    var entry = self.findEntry(self.entries, self.capacity, key);
    return if (self.ctx.isNullKey(entry.value)) null else entry.value;
  }

  pub fn del(self: *Self, key: K) bool {
    if (self.capacity == 0) {
      return false;
    }
    var entry = self.findEntry(self.entries, self.capacity, key);
    if (self.ctx.isNullKey(entry.key)) {
      return false;
    } else {
      entry.key = null_key;
      entry.value = TOMB_VAL; // or self.ctx.tombVal() but unnecessary.
      return true;
    }
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
      if (self.ctx.isNullKey(entry.key)) {
        if (self.ctx.isNullVal(entry.value)) {
          return null;
        }
      } else if (self.ctx.cmpInterned(entry.key, str, hash)) {
        return entry.key;
      }
      index = (index + 1) & mask;
    }
    unreachable;
  }

  pub fn free(self: *Self, vm: *VM) void {
    vm.gc.mem.freeBuf(KVEntry, vm, self.entries[0..self.capacity]);
  }

  pub fn clearAndFree(self: *Self, vm: *VM) void {
    vm.gc.mem.freeBuf(KVEntry, vm, self.entries);
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
        if (!self.map.ctx.isNullKey(entry.key)) {
          return entry.key;
        }
      }
      return null;
    }
  };
  
  };
}
