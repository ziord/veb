const std = @import("std");
const Mem = @import("mem.zig");
const VM = @import("vm.zig").VM;
const Allocator = std.mem.Allocator;

pub fn Vec(comptime T: type) type {
  return extern struct {
    capacity: usize,
    len: usize,
    items: [*]T,

    const Self = @This();
    pub const Slice = []T;
    pub const Writer = _Writer(*Self, error{OutOfMemory}, appendWrite);

    pub fn init() Self {
      return Self {.capacity = 0, .len = 0, .items = &[_]T{}};
    }

    pub inline fn allocatedSlice(self: Self) Slice {
      return self.items[0..self.capacity];
    }

    inline fn ensureCapacity(self: *Self, total: usize, vm: *VM) void {
      if (total >= self.capacity) {
        const new_capacity = Mem.growCapacity(self.capacity + (total - self.capacity));
        self.items = vm.mem.resizeBuf(T, vm, self.items, self.capacity, new_capacity).ptr;
        self.capacity = new_capacity;
      }
    }

    pub fn push(self: *Self, item: T, vm: *VM) void {
      self.ensureCapacity(self.len, vm);
      self.items[self.len] = item;
      self.len += 1;
    }

    fn pushSlice(self: *Self, items: []const T, vm: *VM) void {
      self.ensureCapacity(self.len + items.len, vm);
      @memcpy(self.items[self.len..][0..items.len], items);
      self.len += items.len;
    }

    pub fn pop(self: *Self) T {
      self.len -= 1;
      return self.items[self.len];
    }

    pub fn appendWrite(self: *Self, m: []const T, vm: *VM) void {
      self.pushSlice(m, vm);
    }

    pub fn writer(self: *Self, vm: *VM) Writer {
      return .{ .context = self, .vm = vm };
    }

    pub inline fn size(self: *Self) usize {
      return self.len;
    }

    pub inline fn getItems(self: *Self) []T {
      return self.items[0..self.len];
    }

    pub fn clearAndFree(self: *Self, vm: *VM) void {
      // expand len (if not already at max cap) for freeing
      vm.mem.freeBuf(T, vm, self.items[0..self.capacity]);
      self.len = 0;
      self.capacity = 0;
    }

    pub const deinit = clearAndFree;
  };
}

pub fn _Writer(
    comptime Context: type,
    comptime WriteError: type,
    comptime writeFn: fn (context: Context, bytes: []const u8, vm: *VM) void,
) type {
  return struct {
    context: Context,
    vm: *VM,

    const Self = @This();
    pub const Error = WriteError;

    pub fn write(self: Self, bytes: []const u8) void {
      return writeFn(self.context, bytes, self.vm);
    }

    pub fn writeAll(self: Self, bytes: []const u8) void {
      var index: usize = 0;
      while (index != bytes.len) {
        index += self.write(bytes[index..], self.vm);
      }
    }

    pub fn print(self: Self, comptime format: []const u8, args: anytype) void {
      return std.fmt.format(self, format, args);
    }

    pub fn writeByte(self: Self, byte: u8) void {
      const array = [1]u8{byte};
      return self.writeAll(&array, self.vm);
    }

    pub fn writeByteNTimes(self: Self, byte: u8, n: usize) void {
      var bytes: [256]u8 = undefined;
      @memset(bytes[0..], byte);

      var remaining: usize = n;
      while (remaining > 0) {
        const to_write = @min(remaining, bytes.len);
        self.writeAll(bytes[0..to_write], self.vm);
        remaining -= to_write;
      }
    }
  };
}
