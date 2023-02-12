const std = @import("std");
const parse = @import("parse.zig");
const compile = @import("compile.zig");
const vm = @import("vm.zig");
const debug = @import("debug.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const src = "(0x2 * 45 / 2 * 5 - 1 + 6 / 3 - 0x5 + 6 * (0b1 - 0o2) / 0o1_5) + 234_56.e-2 - 2 % (5-4) - 6";
    var parser = parse.Parser.init(src, "", allocator);
    const node = parser.parse();
    std.debug.print("node: {}\n", .{node});
    var compiler = compile.Compiler.init(node, "test.nova", allocator);
    compiler.compile();
    var code = compiler.code;
    var cpu = vm.VM.init(allocator, code);
    try cpu.run();
    debug.Disassembler.disCode(code, "test");
}
