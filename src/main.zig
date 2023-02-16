const std = @import("std");
const parse = @import("parse.zig");
const compile = @import("compile.zig");
const vm = @import("vm.zig");
const debug = @import("debug.zig");
const value = @import("value.zig");

pub fn main() !void {
  std.debug.print("hello nova!\n", .{});
}

fn doTest(src: []const u8, allocator: std.mem.Allocator) !value.Value {
  const filename = "test.nova";
  var parser = parse.Parser.init(src, filename, allocator);
  const node = parser.parse();
  std.debug.print("node: {}\n", .{node});
  var code = value.Code.init(allocator);
  var cpu = vm.VM.init(allocator, &code);
  var compiler = compile.Compiler.init(node, filename, &cpu, &code, allocator);
  compiler.compile();
  debug.Disassembler.disCode(code, "test");
  try cpu.run();
  return cpu.stack[0];
}

test "arithmetic ops" {
  var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
  defer arena.deinit();
  const allocator = arena.allocator();
  const srcs = [_][]const u8{
    "(0x2 * 45 / 2 * 5 - 1 + 6 / 3 - 0x5 + 6 * (0b1 - 0o2) / 0o1_5) + 234_56.e-2 - 2 % (5-4) - 6",
    "2 ^ 3 ^ (6 | 0 | 1 | 5)",
    "2 | 3 ^ 1 & 0xff",
    "300 >> 8 & 0xff",
    "0xf << 6 | 2",
    "~0x123 + --2",
    "~0x123 ++ --2",
  };
  const exp = [_]f64{449.09846153846155, 6, 2, 1, 962, -290, -290};
  for (srcs) |src, i| {
    const got = try doTest(src, allocator);
    try std.testing.expect(value.asNumber(got) == exp[i]);
  }
}

test "comparison ops" {
  var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
  defer arena.deinit();
  const allocator = arena.allocator();
  const srcs = [_][]const u8{
      "0x123 < 4",
      "123.45 > 12_40",
      "0b111_000 <= 0o12_12",
      "123.e-2 >= 0x12_34_5",
      "123.e-2 != 0x12_34_5",
      "0xdeadbeef == 0o33653337357",
  };
  const exp = [_]bool{false, false, true, false, true, true};
  for (srcs) |src, i| {
    const got = try doTest(src, allocator);
    try std.testing.expect(value.asBool(got) == exp[i]);
  }
}

test "booleans" {
  var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
  defer arena.deinit();
  const allocator = arena.allocator();
  const srcs = [_][]const u8{
      "0x123 < 4 and 1 < 5",
      "123.45 > 12_40 or 2 == 2",
      "0b111_000 <= 0o12_12 or 1 > 0.5",
      "123.e-2 >= 0x12_34_5 and 6 and 7 > 2",
      "123.e-2 != 0x12_34_5 and 0 or 6 > 2",
      "(1 or 2) == 1",
      "(1 and 2) == 2",
      "(0b00 and 2) == 0o0",
      "(0x0 or 2) == 2",
      "true or false",
      "false or true",
      "false or false",
      "true or true",
      "true and false",
      "false and true",
      "false and false",
      "!false",
      "!true",
      "!0x0_0",
      "!!1",
      "!1",
      "'foxes and pirates' == 'foxes and pirates'",
      "'foxes and pirates' != 'fishes and pirates'",
  };
  const exp = [_]bool{
    false, true, true, false, true, 
    true, true, true, true, true, 
    true, false, true, false, false, false,
    true, false, true, true, false, 
    true, true
  };
  for (srcs) |src, i| {
    const got = try doTest(src, allocator);
    try std.testing.expect(value.asBool(got) == exp[i]);
  }
}

test "strings" {
  var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
  defer arena.deinit();
  const allocator = arena.allocator();
  const srcs = [_][]const u8{
      "'foxes'",
      \\"the quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog\n
       ++
      \\the quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog"
  };
  const exp = [_][]const u8{
    "foxes", 
    "the quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog\n" ++
    "the quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog,\nthe quick brown fox runs over the lazy dog"
  };
  for (srcs) |src, i| {
    const got = try doTest(src, allocator);
    try std.testing.expect(std.mem.eql(u8, value.asString(got).str, exp[i]));
  }
}
