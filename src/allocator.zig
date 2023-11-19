const std = @import("std");
const logger = std.log.scoped(.allocator);

arena: std.heap.ArenaAllocator,
gpa: std.heap.GeneralPurposeAllocator(.{}),
isArenaReleased: bool = false,

const Self = @This();

pub fn init(arena: std.heap.ArenaAllocator) Self {
  return Self {.arena = arena, .gpa = std.heap.GeneralPurposeAllocator(.{}){}};
}

pub fn deinit(self: *Self) void {
  defer self.deinitArena();
  const check = self.gpa.deinit();
  if (check == .leak) logger.debug("Memory leak detected.", .{});
}

pub fn deinitArena(self: *Self) void {
  if (!self.isArenaReleased) {
    self.arena.deinit();
    self.isArenaReleased = true;
  }
}

pub fn getArenaAllocator(self: *Self) std.mem.Allocator {
  if (self.isArenaReleased) {
    // reset the arena allocator if already released
    self.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    self.isArenaReleased = false;
  }
  return self.arena.allocator();
}

pub fn getAllocator(self: *Self) std.mem.Allocator {
  return self.gpa.allocator();
}
