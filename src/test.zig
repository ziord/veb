const std = @import("std");
const cli = @import("cli.zig");

fn checkEql(diag: *cli.diagnostics.Diagnostic, comptime size: comptime_int, exp_slice: [size][]const u8) !void {
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

pub fn doRuntimeTest(src: []const u8) !void {
  var cna = cli.VebAllocator.init(std.heap.ArenaAllocator.init(std.heap.page_allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = cli.parse.Parser.init(@constCast(&src), &filename, "", .User, cna.getArenaAllocator());
  const node = try parser.parse(true);
  var tych = cli.check.TypeChecker.init(al, &parser.diag, parser.namegen);
  try tych.typecheck(node, true);
  var cpu = cli.vm.VM.init(&cna, &[_][]const u8{});
  defer cpu.shutdown();
  cpu.initGC();
  var fun = cli.value.createScriptFn(&cpu, 0);
  var compiler = cli.compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, &cna, tych.prelude, null, null);
  try compiler.compile(node);
  cli.debug.Disassembler.disCode(&fun.code, "test");
  const start = std.time.milliTimestamp();
  cpu.boot(fun);
  try cpu.run();
  const end = std.time.milliTimestamp();
  std.debug.print("took: {}ms\n", .{end - start});
  cli.value.printValue(cpu.fiber.fp.stack[0]);
  std.debug.print("\n", .{});
}

pub fn doParsingTest(src: []const u8) !void {
  var cna = cli.VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = cli.parse.Parser.init(@constCast(&src), &filename, "", .User, al);
  _ = try parser.parse(true);
}

pub fn doStaticTest(src: []const u8) !void {
  var cna = cli.VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = cli.parse.Parser.init(@constCast(&src), &filename, "", .User, al);
  const node = try parser.parse(true);
  var tych = cli.check.TypeChecker.init(al, &parser.diag, parser.namegen);
  try tych.typecheck(node, true);
  var cpu = cli.vm.VM.init(&cna, &[_][]const u8{});
  defer cpu.deinit();
  cpu.initGC();
  var fun = cli.value.createScriptFn(&cpu, 0);
  var compiler = cli.compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, &cna, tych.prelude, null, null);
  try compiler.compile(node);
  cli.debug.Disassembler.disCode(&fun.code, "test");
}

pub fn doErrorTest(src: []const u8, comptime size: comptime_int, exp_slice: [size][]const u8) !void {
  var cna = cli.VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  const al = cna.getArenaAllocator();
  try cli.loadCore(try cli.findLibPath(al), al);
  var parser = cli.parse.Parser.init(@constCast(&src), &filename, "", .User, al);
  const node = parser.parse(false) catch {
    try checkEql(&parser.diag, size, exp_slice);
    return;
  };
  var tych = cli.check.TypeChecker.init(al, &parser.diag, parser.namegen);
  tych.typecheck(node, false) catch {
    try checkEql(tych.diag, size, exp_slice);
    return;
  };
  return error.NoError;
}
