const std = @import("std");
const builtin = @import("builtin");

pub const logger = std.log.scoped(.veb);

pub const NameGen = struct {
  al: std.mem.Allocator,
  name_id: usize = 0,

  pub inline fn init(al: std.mem.Allocator) @This() {
    return @This(){.al = al};
  }

  pub fn generate(self: *@This(), comptime fmt: []const u8, args: anytype) [] const u8 {
    var name = std.fmt.allocPrint(self.al, fmt ++ ".{}", args ++ .{self.name_id}) catch @panic("could not generate name");
    self.name_id += 1;
    return name;
  }

  pub inline fn reset(self: *@This()) void {
    self.name_id = 0;
  }

  pub inline fn resetTo(self: *@This(), idc: usize) void {
    self.name_id = idc;
  }

  pub inline fn getCurrentId(self: *@This()) usize {
    return self.name_id;
  }
};

pub const U8Writer = struct {
  backing: std.ArrayList(u8),

  pub const Writer = std.ArrayList(u8).Writer;

  pub fn init(al: std.mem.Allocator) @This() {
    return @This(){.backing = std.ArrayList(u8).init(al)};
  }

  pub fn writer(self: *@This()) Writer {
    return self.backing.writer();
  }

  pub inline fn allocator(self: *@This()) std.mem.Allocator {
    return self.backing.allocator;
  }

  pub inline fn clear(self: *@This()) void {
    self.backing.clearRetainingCapacity();
  }

  pub fn items(self: *@This()) []u8 {
    const chars = self.backing.items;
    self.backing = std.ArrayList(u8).init(self.allocator());
    return chars;
  }
};

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

/// handy function for adding spaces to a writer context
pub fn addDepth(writer: *std.ArrayList(u8).Writer, depth: usize) !void {
  for (0..depth) |_| {
    _ = try writer.write(" ");
  }
}
