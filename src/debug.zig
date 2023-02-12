const std = @import("std");
const value = @import("value.zig");

const Code = value.Code;

pub const Disassembler = struct {
  const Self = @This();

  pub fn _3ArgsInst(name: []const u8, word: u32) void {
    const a1 = Code.readRX(word);
    const a2 = Code.readRK1(word);
    const a3 = Code.readRK2(word);
    std.debug.print("{s} {}, {}, {}\n", .{name, a1, a2, a3});
  }

  pub fn _2ArgsInst(name: []const u8, word: u32) void {
    const a1 = Code.readRX(word);
    const a2 = Code.readBX(word);
    std.debug.print("{s} {}, {}\n", .{name, a1, a2});
  }

  pub fn _1ArgInst(name: []const u8, word: u32) void {
    const a1 = word & Code._26bits;
    std.debug.print("{s} {}\n", .{name, a1});
  }

  pub fn plainInst(name: []const u8) void {
    std.debug.print("{s}\n", .{name});
  }

  pub fn disInstruction(code: Code, word: u32, idx: usize) void {
    if (idx > 0 and code.lines.items[idx] == code.lines.items[idx - 1]) {
      std.debug.print("   |\t{d:0>4}\t", .{idx});
    } else {
      std.debug.print("{d:>4}\t{d:0>4}\t", .{code.lines.items[idx], idx});
    }
    const op = Code.readInstOp(word);
    switch (op) {
      .Add => _3ArgsInst("add", word),
      .Sub => _3ArgsInst("sub", word),
      .Mul => _3ArgsInst("mul", word),
      .Div => _3ArgsInst("div", word),
      .Mod => _3ArgsInst("mod", word),
      .Load => _2ArgsInst("load", word),
      .Ret => plainInst("ret"),
    }
  }

  pub fn disCode(code: Code, name: []const u8) void {
    std.debug.print(">>Disassembly of {s}<<\n", .{name});
    for (code.words.items) |word, i| {
      disInstruction(code, word, i);
    }
  }
};
