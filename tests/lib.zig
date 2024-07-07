pub const std = @import("std");
pub const parse = @import("../src/parse.zig");
pub const compile = @import("../src/compile.zig");
pub const vm = @import("../src/vm.zig");
pub const debug = @import("../src/debug.zig");
pub const value = @import("../src/value.zig");
pub const link = @import("../src/link.zig");
pub const check = @import("../src/check.zig");
pub const diagnostics = @import("../src/diagnostics.zig");
pub const VebAllocator = @import("../src/allocator.zig");
pub const cli = @import("../src/cli.zig");

pub fn doRuntimeTest(src: []const u8) !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.heap.page_allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = parse.Parser.init(@constCast(&src), &filename, "", .User, cna.getArenaAllocator());
  const node = try parser.parse(true);
  var tych = check.TypeChecker.init(al, &parser.diag, parser.namegen);
  try tych.typecheck(node, true);
  var cpu = vm.VM.init(&cna);
  defer cpu.shutdown();
  var fun = value.createScriptFn(&cpu, 0);
  var compiler = compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, &cna, tych.prelude, null, null);
  try compiler.compile(node);
  debug.Disassembler.disCode(&fun.code, "test");
  var start = std.time.milliTimestamp();
  cpu.boot(fun);
  try cpu.run();
  var end = std.time.milliTimestamp();
  std.debug.print("took: {}ms\n", .{end - start});
  value.printValue(cpu.fiber.fp.stack[0]);
  std.debug.print("\n", .{});
}

pub fn doParsingTest(src: []const u8) !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = parse.Parser.init(@constCast(&src), &filename, "", .User, al);
  _ = try parser.parse(true);
}

pub fn doStaticTest(src: []const u8) !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = parse.Parser.init(@constCast(&src), &filename, "", .User, al);
  const node = try parser.parse(true);
  var tych = check.TypeChecker.init(al, &parser.diag, parser.namegen);
  try tych.typecheck(node, true);
  var cpu = vm.VM.init(&cna);
  defer cpu.deinit();
  var fun = value.createScriptFn(&cpu, 0);
  var compiler = compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, &cna, tych.prelude, null, null);
  try compiler.compile(node);
  debug.Disassembler.disCode(&fun.code, "test");
}

fn checkEql(diag: *diagnostics.Diagnostic, comptime size: comptime_int, exp_slice: [size][]const u8) !void {
  var got = @as(usize, 0);
  var i = @as(usize, 0);
  for (exp_slice[0..]) |exp| {
    for (diag.data.items()[i..]) |itm| {
      i += 1;
      if (std.mem.containsAtLeast(u8, itm.msg, 1, exp)) {
        got += 1;
        break;
      }
    }
  }
  try std.testing.expect(got == size);
}

pub fn doErrorTest(src: []const u8, comptime size: comptime_int, exp_slice: [size][]const u8) !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = parse.Parser.init(@constCast(&src), &filename, "", .User, al);
  const node = parser.parse(false) catch {
    try checkEql(&parser.diag, size, exp_slice);
    return;
  };
  var tych = check.TypeChecker.init(al, &parser.diag, parser.namegen);
  tych.typecheck(node, false) catch {
    try checkEql(tych.diag, size, exp_slice);
    return;
  };
  return error.NoError;
}
