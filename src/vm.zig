const std = @import("std");
const vl = @import("value.zig");
const OpCode = @import("opcode.zig").OpCode;
const Mem = @import("mem.zig");
const GC = @import("gc.zig");
const NovaAllocator = @import("allocator.zig");

const Value = vl.Value;
const Code = vl.Code;
const StringHashMap = vl.StringHashMap;

pub const VM = struct {
  ip: usize,
  stack: [STACK_MAX]Value,
  code: *Code,
  strings: StringHashMap,
  gsyms: [MAX_GSYM_ITEMS]Value,
  globals: StringHashMap,
  objects: ?*vl.Obj,
  mem: Mem,
  gc: GC,

  const Self = @This();
  const STACK_MAX = 0xffff;
  pub const MAX_GSYM_ITEMS = vl.MAX_REGISTERS << 1;
  pub const MAX_LOCAL_ITEMS = vl.MAX_REGISTERS << 1;
  const RuntimeError = error{RuntimeError};

  pub fn init(allocator: *NovaAllocator, code: *Code) Self {
    return Self {
      .ip = 0,
      .stack = undefined,
      .mem = Mem.init(allocator.getAllocator()),
      .gc = GC.init(allocator),
      .strings = StringHashMap.init(),
      .gsyms = undefined,
      .globals = StringHashMap.init(),
      .objects = null,
      .code = code
    };
  }

  pub fn deinit(self: *Self) void {
    self.strings.free(self);
    self.globals.free(self);
    var curr = self.objects;
    while (curr) |cur| {
      var next = cur.next;
      self.gc.freeObject(cur, self);
      curr = next;
    }
    // TODO: move this when functions are implemented
    self.code.deinit(self);
    self.gc.deinit();
    self.mem.deinit();
  }

  inline fn readWord(self: *Self) u32 {
    self.ip += 1;
    return self.code.words.items[self.ip - 1];
  }

  inline fn read3Args(self: *Self, word: u32, a1: *u32, a2: *u32, a3: *u32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = (word >> 9) & Code._9bits;
    a3.* = word & Code._9bits;
  }

  inline fn read2Args(self: *Self, word: u32, a1: *u32, a2: *u32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = word & Code._18bits;
  }

  inline fn read2ArgsSigned(self: *Self, word: u32, a1: *u32, a2: *i32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = @intCast(i32, word & Code._18bits);
  }

  inline fn read1Arg(self: *Self, word: u32, a: *u32) void {
    _ = self;
    a.* = word & Code._26bits;
  }

  inline fn readConst(self: *Self, pos: u32) Value {
    return self.code.values.items[pos];
  }

  inline fn RK(self: *Self, x: u32) Value {
    // rk(x) = r(x) if x < MAX_REGISTERS else k(x - MAX_REGISTERS)
    return if (x < vl.MAX_REGISTERS) self.stack[x] else self.code.values.items[x - vl.MAX_REGISTERS];
  }

  inline fn assert(self: *Self, cond: bool) void {
    _ = self;
    std.debug.assert(cond);
  }

  fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) RuntimeError {
    _ = self;
    std.debug.print(fmt ++ "\n", args);
    return error.RuntimeError;
  }

  inline fn printStack(self: *Self) void {
    for (self.stack) |val| {
      std.debug.print("[ ", .{});
      vl.printValue(val);
      std.debug.print(" ]", .{});
    }
    std.debug.print("\n", .{});
  }

  pub fn run(self: *Self) RuntimeError!void {
    while (true) {
      const inst = @call(.always_inline, self.readWord, .{});
      switch (@call(.always_inline, Code.readInstOp, .{inst})) {
        .Gglb => {
          // gglb rx, bx -> r(x) = G[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          var glb = vl.asString(self.readConst(bx));
          var val = self.globals.get(glb);
          self.assert(val != null);
          self.stack[rx] = val.?;
          continue;
        },
        .Sglb => {
          // sglb rx, bx -> G[K(bx)] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          var glb = vl.asString(self.readConst(bx));
          _ = self.globals.put(glb, self.stack[rx], self);
          continue;
        },
        .Ggsym => {
          // ggsym rx, bx -> r(x) = GS[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          self.stack[rx] = self.gsyms[bx];
          continue;
        },
        .Sgsym => {
          // sgsym rx, bx -> GS[bx] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          self.gsyms[bx] = self.stack[rx];
          continue;
        },
        .Mov => {
          // mov rx, ry
          var rx: u32 = undefined;
          var ry: u32 = undefined;
          var dum: u32 = undefined;
          self.read3Args(inst, &rx, &ry, &dum);
          self.stack[rx] = self.stack[ry];
          continue;
        },
        .Asrt => {
          // asrt rx
          var rx: u32 = undefined;
          var dum1: u32 = undefined;
          var dum2: u32 = undefined;
          self.read3Args(inst, &rx, &dum1, &dum2);
          if (vl.isNil(self.stack[rx])) {
            return self.runtimeError("Attempt to use value 'nil'", .{});
          }
          continue;
        },
        .Add => {
          // add rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) + vl.asNumber(b));
          continue;
        },
        .Sub => {
          // sub rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) - vl.asNumber(b));
          continue;
        },
        .Mul => {
          // mul rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) * vl.asNumber(b));
          continue;
        },
        .Div => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(vl.asNumber(a) / vl.asNumber(b));
          continue;
        },
        .Mod => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          if (@call(.always_inline, vl.asNumber, .{b}) == 0) {
            return self.runtimeError("Modulo by zero\n", .{});
          }
          self.stack[rx] = vl.numberVal(@rem(vl.asNumber(a), vl.asNumber(b)));
          continue;
        },
        .Cmp => {
          // cmp rx, rk(x), rk(x) | cmp_op 
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var next_inst = @call(.always_inline, self.readWord, .{});
          const cmp_op = @call(.always_inline, Code.readInstOpNoConv, .{next_inst});
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          switch (@intToEnum(vl.OpType, cmp_op)) {
            .OpLess => self.stack[rx] = vl.boolVal(vl.asNumber(a) < vl.asNumber(b)),
            .OpGrt => self.stack[rx] = vl.boolVal(vl.asNumber(a) > vl.asNumber(b)),
            .OpLeq => self.stack[rx] = vl.boolVal(vl.asNumber(a) <= vl.asNumber(b)),
            .OpGeq => self.stack[rx] = vl.boolVal(vl.asNumber(a) >= vl.asNumber(b)),
            // todo: type handling?
            .OpEqq => self.stack[rx] = vl.boolVal(@call(.always_inline, vl.valueEqual, .{a, b})),
            .OpNeq => self.stack[rx] = vl.boolVal(!@call(.always_inline, vl.valueEqual, .{a, b})),
            else => unreachable,
          }
          continue;
        },
        .Xor => {
          // xor rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(@intToFloat(f64, (vl.asIntNumber(i64, a) ^ vl.asIntNumber(i64, b))));
          continue;
        },
        .Or => {
          // or rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(@intToFloat(f64, (vl.asIntNumber(i64, a) | vl.asIntNumber(i64, b))));
          continue;
        },
        .And => {
          // and rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(@intToFloat(f64, (vl.asIntNumber(i64, a) & vl.asIntNumber(i64, b))));
          continue;
        },
        .Shl => {
          // shl rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(@intToFloat(f64, std.math.shl(i64, vl.asIntNumber(i64, a), vl.asIntNumber(i64, b))));
          continue;
        },
        .Shr => {
          // shr rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1);
          const b = self.RK(rk2);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          self.stack[rx] = vl.numberVal(@intToFloat(f64, std.math.shr(i64, vl.asIntNumber(i64, a), vl.asIntNumber(i64, b))));
          continue;
        },
        .Inv => {
          // inv rx, rk(x): rk(x) == bx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(inst, &rx, &rk);
          const a = self.RK(rk);
          self.assert(vl.isNumber(a));
          self.stack[rx] = vl.numberVal(@intToFloat(f64, ~vl.asIntNumber(i64, a)));
          continue;
        },
        .Jt => {
          // jt rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          if (!vl.valueFalsy(self.stack[rx])) {
            self.ip += bx;
          }
          continue;
        },
        .Jf => {
          // jf rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          if (vl.valueFalsy(self.stack[rx])) {
            self.ip += bx;
          }
          continue;
        },
        .Jmp => {
          // jmp rx, bx
          var dum: u32 = undefined;
          var bx: u32 = undefined; // TODO: signed bx
          self.read2Args(inst, &dum, &bx);
          self.ip += bx;
          continue;
        },
        .Not => {
          // not rx, rk(x)
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(inst, &rx, &rk);
          self.stack[rx] = vl.boolVal(vl.valueFalsy(self.RK(rk)));
          continue;
        },
        .Load => {
          // load rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          self.stack[rx] = self.code.values.items[bx];
          continue;
        },
        .Nlst => {
          // nlst rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(inst, &rx, &count);
          self.stack[rx] = vl.objVal(vl.createList(self, @as(usize, count)));
          continue;
        },
        .Slst => {
          // slst rx, rk(idx), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var list = vl.asList(self.stack[rx]);
          var idx = vl.asIntNumber(i64, self.RK(rk1));
          if (idx < 0) idx += @intCast(i64, list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          list.items[@intCast(usize, idx)] = self.RK(rk2);
          continue;
        },
        .Glst => {
          // glst rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var list = vl.asList(self.RK(rk2));
          var idx = vl.asIntNumber(i64, self.RK(rk1));
          if (idx < 0) idx += @intCast(i64, list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          self.stack[rx] = list.items[@intCast(usize, idx)];
          continue;
        },
        .Nmap => {
          // TODO: map specialization
          // nmap rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(inst, &rx, &count);
          self.stack[rx] = vl.objVal(vl.createMap(self, @as(usize, count)));
          continue;
        },
        .Smap => {
          // smap rx, rk(key), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var map = vl.asMap(self.stack[rx]);
          _ = map.meta.put(self.RK(rk1), self.RK(rk2), self);
          continue;
        },
        .Gmap => {
          // gmap rx, rk(key), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var map = vl.asMap(self.RK(rk2));
          const key = self.RK(rk1);
          if (map.meta.get(key)) |v| {
            self.stack[rx] = v;
            continue;
          }
          var kstr = vl.asString(vl.valueToString(key, self));
          return self.runtimeError("KeyError: map has no key: '{s}'", .{kstr.str[0..kstr.len]});
        },
        .Bcst => {
          // bcst rx, rk(x): rk(x) == bx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(inst, &rx, &rk);
          self.stack[rx] = vl.boolVal(!vl.valueFalsy(self.RK(rk)));
          continue;
        },
        .Ret => {
          break;
        },
      }
    }
  }

};
