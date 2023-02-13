const std = @import("std");
const parse = @import("parse.zig");
const compile = @import("compile.zig");
const vm = @import("vm.zig");
const debug = @import("debug.zig");
const value = @import("value.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
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
        var parser = parse.Parser.init(src, "", allocator);
        const node = parser.parse();
        std.debug.print("node: {}\n", .{node});
        var compiler = compile.Compiler.init(node, "test.nova", allocator);
        compiler.compile();
        var code = compiler.code;
        var cpu = vm.VM.init(allocator, code);
        try cpu.run();
        debug.Disassembler.disCode(code, "test");
        try std.testing.expect(value.asNumber(cpu.stack[0]) == exp[i]);
    }
}
