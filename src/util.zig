const std = @import("std");
const builtin = @import("builtin");


const Allocator = std.mem.Allocator;
pub const logger = std.log.scoped(.veb);

pub const NameGen = struct {
  al: Allocator,
  name_id: usize = 0,

  pub inline fn init(al: Allocator) @This() {
    return .{.al = al};
  }

  pub fn generate(self: *@This(), comptime fmt: []const u8, args: anytype) [] const u8 {
    const name = std.fmt.allocPrint(self.al, fmt ++ ".{}", args ++ .{self.name_id}) catch @panic("could not generate name");
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

pub const U8Writer = TWriter(u8);
pub const StringWriter = TWriter([]const u8);

pub fn TWriter(comptime T: type) type {
  return struct {
    backing: std.ArrayList(T),

    pub const Writer = std.ArrayList(T).Writer;

    pub fn init(al: Allocator) @This() {
      return .{.backing = std.ArrayList(T).init(al)};
    }

    pub fn writer(self: *@This()) Writer {
      return self.backing.writer();
    }

    pub fn view(self: *@This()) []const T {
      return self.backing.items;
    }

    pub fn len(self: *@This()) usize {
      return self.backing.items.len;
    }
  
    pub inline fn allocator(self: *@This()) Allocator {
      return self.backing.allocator;
    }

    pub inline fn clear(self: *@This()) void {
      self.backing.clearRetainingCapacity();
    }

    pub fn items(self: *@This()) []T {
      const chars = self.backing.items;
      self.backing = std.ArrayList(T).init(self.allocator());
      return chars;
    }
  };
}

pub inline fn append(comptime T: type, list: *std.ArrayList(T), val: T) void {
  list.append(val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.posix.exit(1);
  };
}

pub inline fn extend(comptime T: type, target: *std.ArrayList(T), src: *std.ArrayList(T)) void {
  target.appendSlice(src.items[0..src.items.len]) catch |e| {
    std.debug.print("error: {}", .{e});
    std.posix.exit(1);
  };
}

pub inline fn set(comptime K: type, comptime V: type, map: *std.AutoArrayHashMap(K, V), key: K, val: V) void {
  map.put(key, val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.posix.exit(1);
  };
}

pub inline fn setStr(comptime V: type, map: *std.StringHashMap(V), key: []const u8, val: V) void {
  map.put(key, val) catch |e| {
    std.debug.print("error: {}", .{e});
    std.posix.exit(1);
  };
}

pub const print = if (inDebugMode) stderr_print else stdout_print;

pub fn stdout_print(comptime fmt: []const u8, args: anytype) void {
  std.io.getStdOut().writer().print(fmt, args) catch return;
}

pub fn stderr_print(comptime fmt: []const u8, args: anytype) void {
  std.io.getStdErr().writer().print(fmt, args) catch return;
}

pub fn error_(comptime fmt: []const u8, args: anytype) noreturn {
  std.debug.print(fmt ++ "\n", args);
  std.posix.exit(1);
}

pub inline fn getMode() std.builtin.Mode {
  return builtin.mode;
}

pub const inDebugMode = (builtin.mode == .Debug);

pub inline fn getVersion() []const u8 {
  return "0.1.0";
}

pub inline fn assert(check: bool, msg: []const u8) void {
  if (!check) {
    @panic(msg);
  }
}

pub inline fn alloc(comptime T: type, allocator: Allocator) *T {
  return allocator.create(T) catch |e| {
    std.debug.print("AllocationError {}", .{e});
    std.posix.exit(1);
  };
}

pub inline fn allocSlice(comptime T: type, n: usize, allocator: Allocator) []T {
  return allocator.alloc(T, n) catch |e| {
    std.debug.print("AllocationError {}", .{e});
    std.posix.exit(1);
  };
}

pub inline fn box(comptime T: type, val: T, allocator: Allocator) *T {
  const item = allocator.create(T) catch {
    std.debug.print("Allocation failed\n", .{});
    std.posix.exit(1);
  };
  item.* = val;
  return item;
}

pub fn boxEnsureCapacity(comptime T: type, val: T, al: Allocator, cap: usize) *T {
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

pub fn readFile(filename: []const u8, al: Allocator) ![]const u8 {
  errdefer print("Error opening file: {s}\n", .{filename});
  var path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
  const abs_path = try std.fs.realpath(filename, &path_buffer);
  const file = try std.fs.openFileAbsolute(abs_path, .{});
  defer file.close();
  const size = try file.getEndPos();
  const content = try al.allocSentinel(u8, size, 0);
  const got = try file.readAll(content);
  assert(got == size, "file size should match");
  return content;
}

pub fn readFileHandle(file: std.fs.File, al: Allocator) ![]const u8 {
  defer file.close();
  const size = try file.getEndPos();
  const content = try al.allocSentinel(u8, size, 0);
  const got = try file.readAll(content);
  assert(got == size, "file size should match");
  return content;
}
