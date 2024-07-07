const std = @import("std");
const logger = std.log.scoped(.allocator);

arena: std.heap.ArenaAllocator,
gpa: std.heap.GeneralPurposeAllocator(.{}),
is_arena_released: bool = false,

const Self = @This();

pub fn init(arena: std.heap.ArenaAllocator) Self {
  return Self{.arena = arena, .gpa = std.heap.GeneralPurposeAllocator(.{}){}};
}

pub fn deinit(self: *Self) void {
  defer self.deinitArena();
  const check = self.gpa.deinit();
  if (check == .leak) logger.debug("Memory leak detected.", .{});
}

pub fn deinitArena(self: *Self) void {
  if (!self.is_arena_released) {
    self.arena.deinit();
    self.is_arena_released = true;
  }
}

pub fn getArenaAllocator(self: *Self) std.mem.Allocator {
  if (self.is_arena_released) {
    // reset the arena allocator if already released
    self.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    self.is_arena_released = false;
  }
  return self.arena.allocator();
}

pub fn getAllocator(self: *Self) std.mem.Allocator {
  return self.gpa.allocator();
}
