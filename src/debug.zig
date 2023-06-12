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

  pub fn __2ArgsInst(name: []const u8, word: u32) void {
    const a1 = Code.readRX(word);
    const a2 = Code.readBX(word);
    std.debug.print("{s} {}, {}", .{name, a1, a2});
  }

  pub fn _2ArgsInst(name: []const u8, word: u32) void {
    __2ArgsInst(name, word);
    std.debug.print("\n", .{});
  }

  pub fn _1ArgInst(name: []const u8, word: u32) void {
    const a1 = word & Code._26bits;
    std.debug.print("{s} {}\n", .{name, a1});
  }

  pub fn plainInst(name: []const u8) void {
    std.debug.print("{s}\n", .{name});
  }

  pub fn disInstruction(code: *Code, word: u32, index: *usize) void {
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
      .Is => _3ArgsInst("is", word),
      .Shl => _3ArgsInst("shl", word),
      .Shr => _3ArgsInst("shr", word),
      .Not => _3ArgsInst("not", word),
      .Slst => _3ArgsInst("slst", word),
      .Glst => _3ArgsInst("glst", word),
      .Stup => _3ArgsInst("stup", word),
      .Gtup => _3ArgsInst("gtup", word),
      .Smap => _3ArgsInst("smap", word),
      .Gmap => _3ArgsInst("gmap", word),
      .Inv => _2ArgsInst("inv", word),
      .Jt => _2ArgsInst("jt", word),
      .Jf => _2ArgsInst("jf", word),
      .Nlst => _2ArgsInst("nlst", word),
      .Ntup => _2ArgsInst("ntup", word),
      .Nmap => _2ArgsInst("nmap", word),
      .Gglb => _2ArgsInst("gglb", word),
      .Sglb => _2ArgsInst("sglb", word),
      .Ggsym => _2ArgsInst("ggsym", word),
      .Sgsym => _2ArgsInst("sgsym", word),
      .Bcst => _2ArgsInst("bcst", word),
      .Bclo => _2ArgsInst("bclo", word),
      .Call => _2ArgsInst("call", word),
      .Ret => _2ArgsInst("ret", word),
      .Mov => {
        // mov is a 2-arg inst using a 3-arg format.
        const a1 = Code.readRX(word);
        const a2 = Code.readRK1(word);
        std.debug.assert(Code.readRK2(word) == 0);
        std.debug.print("mov {}, {}\n", .{a1, a2});
      },
      .Asrt => {
        // asrt is a 1-arg inst using a 3-arg format.
        const a1 = Code.readRX(word);
        std.debug.assert(Code.readRK1(word) == 0);
        std.debug.assert(Code.readRK2(word) == 0);
        std.debug.print("asrt {}\n", .{a1});
      },
      .Jmp => {
        // jmp is a 1-arg inst using a 2-arg format.
        var rx = Code.readRX(word);
        std.debug.assert(rx == 0 or rx == 2);
        const n = Code.readBX(word);
        std.debug.print("jmp {s}{}\n", .{if (rx == 0) "-" else "", n});
      },
      .Load => {
        __2ArgsInst("load", word);
        var bx = Code.readBX(word);
        std.debug.print("   ( ", .{});
        value.printValue(code.values.items[bx]);
        std.debug.print(" )\n", .{});
      },
      .Cmp => {
        __3ArgsInst("cmp", word);
        index.* = index.* + 1; // cmp_op
        const cmp_op = Code.readInstOpNoConv(code.words.items[index.*]);
        std.debug.print(" ({})\n", .{@intToEnum(value.OpType, cmp_op)});
      },
    }
  }

  pub fn disCode(code: *Code, name: []const u8) void {
    std.debug.print(">>Disassembly of {s}<<\n", .{name});
    var i: usize = 0;
    while (i < code.words.len): (i += 1) {
      disInstruction(code, code.words.items[i], &i);
    }
    std.debug.print("\n", .{});
  }
};
