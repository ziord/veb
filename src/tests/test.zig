pub const std = @import("std");
pub const parse = @import("../parse.zig");
pub const compile = @import("../compile.zig");
pub const vm = @import("../vm.zig");
pub const debug = @import("../debug.zig");
pub const value = @import("../value.zig");
pub const link = @import("../link.zig");
pub const check = @import("../check.zig");
pub const flow = @import("../flow.zig");
pub const ast = @import("../ast.zig");
pub const diagnostics = @import("../diagnostics.zig");
pub const VebAllocator = @import("../allocator.zig");

pub fn doRuntimeTest(src: []const u8) !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.heap.page_allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  var al = cna.getArenaAllocator();
  var parser = parse.Parser.init(@constCast(&src), &filename, &cna);
  const node = try parser.parse(true);
  var tych = check.TypeChecker.init(al, &parser.diag);
  try tych.typecheck(node, &cna, true);
  var cpu = vm.VM.init(&cna);
  defer cpu.deinit();
  var fun = value.createFn(&cpu, 0);
  var compiler = compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, &cna, tych._prelude, null, null);
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

pub fn doStaticTest(src: []const u8) !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.testing.allocator));
  defer cna.deinit();
  const filename = @as([]const u8, "test.veb");
  var al = cna.getArenaAllocator();
  var parser = parse.Parser.init(@constCast(&src), &filename, &cna);
  const node = try parser.parse(true);
  var tych = check.TypeChecker.init(al, &parser.diag);
  try tych.typecheck(node, &cna, true);
  var cpu = vm.VM.init(&cna);
  defer cpu.deinit();
  var fun = value.createFn(&cpu, 0);
  var compiler = compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, &cna, tych._prelude, null, null);
  try compiler.compile(node);
  debug.Disassembler.disCode(&fun.code, "test");
}

fn checkEql(diag: *diagnostics.Diagnostic, comptime size: comptime_int, exp_slice: [size][]const u8) !void {
  var got = @as(usize, 0);
  var i = @as(usize, 0);
  for (exp_slice[0..]) |exp| {
    for (diag.data.items[i..]) |itm| {
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
  var al = cna.getArenaAllocator();
  var parser = parse.Parser.init(@constCast(&src), &filename, &cna);
  const node = parser.parse(false) catch {
    try checkEql(&parser.diag, size, exp_slice);
    return;
  };
  var tych = check.TypeChecker.init(al, &parser.diag);
  tych.typecheck(node, &cna, false) catch {
    try checkEql(tych.diag, size, exp_slice);
    return;
  };
  return error.NoError;
}
