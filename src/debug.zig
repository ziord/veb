const std = @import("std");
const value = @import("value.zig");

const Code = value.Code;

pub const Disassembler = struct {
  const Self = @This();


  pub fn __3ArgsInst(name: []const u8, word: u32) void {
    const a1 = Code.readRX(word);
    const a2 = Code.readRK1(word);
    const a3 = Code.readRK2(word);
    std.debug.print("{s} {}, {}, {}", .{name, a1, a2, a3});
  }

  pub fn _3ArgsInst(name: []const u8, word: u32) void {
    __3ArgsInst(name, word);
    std.debug.print("\n", .{});
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

  pub fn disInstruction(code: Code, word: u32, index: *usize) void {
    const idx = index.*;
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
      .Xor => _3ArgsInst("xor", word),
      .Or => _3ArgsInst("or", word),
      .And => _3ArgsInst("and", word),
      .Shl => _3ArgsInst("shl", word),
      .Shr => _3ArgsInst("shr", word),
      .Inv => _2ArgsInst("inv", word),
      .Load => _2ArgsInst("load", word),
      .Ret => plainInst("ret"),
      .Cmp => {
        __3ArgsInst("cmp", word);
        index.* = index.* + 1; // cmp_op
        const cmp_op = Code.readInstOpNoConv(code.words.items[index.*]);
        std.debug.print(" ({})\n", .{@intToEnum(value.OpType, cmp_op)});
      },
    }
  }

  pub fn disCode(code: Code, name: []const u8) void {
    std.debug.print(">>Disassembly of {s}<<\n", .{name});
    var i: usize = 0;
    while (i < code.words.items.len): (i += 1) {
      disInstruction(code, code.words.items[i], &i);
    }
  }
};
