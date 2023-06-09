const std = @import("std");
const vl = @import("value.zig");
const OpCode = @import("opcode.zig").OpCode;
const Mem = @import("mem.zig");
const GC = @import("gc.zig");
const NovaAllocator = @import("allocator.zig");

const Value = vl.Value;
const Code = vl.Code;
const Inst = vl.Inst;
const ObjFiber = vl.ObjFiber;
const ObjFn = vl.ObjFn;
const ObjClosure = vl.ObjClosure;
const CallFrame = vl.CallFrame;
const StringHashMap = vl.StringHashMap;

pub const VM = struct {
  fiber: *ObjFiber,
  strings: StringHashMap,
  gsyms: [MAX_GSYM_ITEMS]Value,
  globals: StringHashMap,
  objects: ?*vl.Obj,
  mem: Mem,
  gc: GC,

  const Self = @This();
  const STACK_MAX = 0xfff;
  pub const MAX_GSYM_ITEMS = vl.MAX_REGISTERS << 1;
  pub const MAX_LOCAL_ITEMS = vl.MAX_REGISTERS << 1;
  const RuntimeError = error{RuntimeError};
  const TypeTag2CheckFunc = [_]*const fn(Value) bool {
    vl.isBoolNoInline, vl.isNumberNoInline, vl.isStringNoInline,
    vl.isNilNoInline, vl.isVoidNoInline, vl.isListNoInline, vl.isMapNoInline,
    vl.isTupleNoInline, vl.isFnNoInline, vl.isClosureNoInline, vl.isUpvalueNoInline,
    vl.isZFnNoInline, vl.isFiberNoInline
  };
  pub const ZFnNames = [_][]const u8 {
    "",
  };

  pub fn init(allocator: *NovaAllocator) Self {
    return Self {
      .fiber = undefined,
      .mem = Mem.init(allocator.getAllocator()),
      .gc = GC.init(allocator),
      .strings = StringHashMap.init(),
      .gsyms = undefined,
      .globals = StringHashMap.init(),
      .objects = null,
    };
  }

  pub fn boot(self: *Self, fun: *ObjFn) void {
    var clo = vl.createClosure(self, fun);
    // TODO: init builtins
    self.fiber = vl.createFiber(self, clo, .Root, null);
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
    self.gc.deinit();
    self.mem.deinit();
  }

  inline fn readWord(self: *Self, fp: *CallFrame) Inst {
    _ = self;
    fp.ip += 1;
    @setRuntimeSafety(false);
    return fp.closure.fun.code.words.items[fp.ip - 1];
  }

  inline fn read3Args(self: *Self, word: Inst, a1: *u32, a2: *u32, a3: *u32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = (word >> 9) & Code._9bits;
    a3.* = word & Code._9bits;
  }

  inline fn read2Args(self: *Self, word: Inst, a1: *u32, a2: *u32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = word & Code._18bits;
  }

  inline fn read2ArgsSigned(self: *Self, word: Inst, a1: *u32, a2: *i32) void {
    _ = self;
    a1.* = (word >> 18) & Code._8bits;
    a2.* = vl.Code.readSBX(word);
  }

  inline fn read1Arg(self: *Self, word: Inst, a: *u32) void {
    _ = self;
    a.* = word & Code._26bits;
  }

  inline fn readConst(self: *Self, pos: u32, fp: *vl.CallFrame) Value {
    _ = self;
    return fp.closure.fun.code.values.items[pos];
  }

  inline fn RK(self: *Self, x: u32, fp: *CallFrame) Value {
    // rk(x) = r(x) if x < MAX_REGISTERS else k(x - MAX_REGISTERS)
    _ = self;
    return if (x < vl.MAX_REGISTERS) fp.stack[x] else fp.closure.fun.code.values.items[x - vl.MAX_REGISTERS];
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
    for (self.fiber.fp.stack) |val| {
      std.debug.print("[ ", .{});
      vl.printValue(val);
      std.debug.print(" ]", .{});
    }
    std.debug.print("\n", .{});
  }

  pub fn run(self: *Self) RuntimeError!void {
    var fiber = self.fiber;
    var fp = fiber.fp;
    while (true) {
      const inst = @call(.always_inline, Self.readWord, .{self, fp});
      switch (@call(.always_inline, Code.readInstOp, .{inst})) {
        .Gglb => {
          // gglb rx, bx -> r(x) = G[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          var glb = vl.asString(self.readConst(bx, fp));
          var val = self.globals.get(glb);
          self.assert(val != null);
          fp.stack[rx] = val.?;
          continue;
        },
        .Sglb => {
          // sglb rx, bx -> G[K(bx)] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          var glb = vl.asString(self.readConst(bx, fp));
          _ = self.globals.put(glb, fp.stack[rx], self);
          continue;
        },
        .Ggsym => {
          // ggsym rx, bx -> r(x) = GS[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          fp.stack[rx] = self.gsyms[bx];
          continue;
        },
        .Sgsym => {
          // sgsym rx, bx -> GS[bx] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          self.gsyms[bx] = fp.stack[rx];
          continue;
        },
        .Mov => {
          // mov rx, ry
          var rx: u32 = undefined;
          var ry: u32 = undefined;
          var dum: u32 = undefined;
          self.read3Args(inst, &rx, &ry, &dum);
          fp.stack[rx] = fp.stack[ry];
          continue;
        },
        .Asrt => {
          // asrt rx
          if (vl.isNil(fp.stack[vl.Code.readRX(inst)])) {
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
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(vl.asNumber(a) + vl.asNumber(b));
          continue;
        },
        .Sub => {
          // sub rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(vl.asNumber(a) - vl.asNumber(b));
          continue;
        },
        .Mul => {
          // mul rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(vl.asNumber(a) * vl.asNumber(b));
          continue;
        },
        .Div => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(vl.asNumber(a) / vl.asNumber(b));
          continue;
        },
        .Mod => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          if (@call(.always_inline, vl.asNumber, .{b}) == 0) {
            return self.runtimeError("Modulo by zero\n", .{});
          }
          fp.stack[rx] = vl.numberVal(@rem(vl.asNumber(a), vl.asNumber(b)));
          continue;
        },
        .Cmp => {
          // cmp rx, rk(x), rk(x) | cmp_op 
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var next_inst = @call(.always_inline, Self.readWord, .{self, fp});
          const cmp_op = @call(.always_inline, Code.readInstOpNoConv, .{next_inst});
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          switch (@intToEnum(vl.OpType, cmp_op)) {
            .OpLess => fp.stack[rx] = vl.boolVal(vl.asNumber(a) < vl.asNumber(b)),
            .OpGrt => fp.stack[rx] = vl.boolVal(vl.asNumber(a) > vl.asNumber(b)),
            .OpLeq => fp.stack[rx] = vl.boolVal(vl.asNumber(a) <= vl.asNumber(b)),
            .OpGeq => fp.stack[rx] = vl.boolVal(vl.asNumber(a) >= vl.asNumber(b)),
            // todo: type handling?
            .OpEqq => fp.stack[rx] = vl.boolVal(@call(.always_inline, vl.valueEqual, .{a, b})),
            .OpNeq => fp.stack[rx] = vl.boolVal(!@call(.always_inline, vl.valueEqual, .{a, b})),
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
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, (vl.asIntNumber(i64, a) ^ vl.asIntNumber(i64, b))));
          continue;
        },
        .Or => {
          // or rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, (vl.asIntNumber(i64, a) | vl.asIntNumber(i64, b))));
          continue;
        },
        .And => {
          // and rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, (vl.asIntNumber(i64, a) & vl.asIntNumber(i64, b))));
          continue;
        },
        .Is => {
          // is rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const val = self.RK(rk1, fp);
          const ty_tag = vl.asIntNumber(usize, self.RK(rk2, fp));
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(TypeTag2CheckFunc[ty_tag](val));
          continue;
        },
        .Shl => {
          // shl rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, std.math.shl(i64, vl.asIntNumber(i64, a), vl.asIntNumber(i64, b))));
          continue;
        },
        .Shr => {
          // shr rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          const a = self.RK(rk1, fp);
          const b = self.RK(rk2, fp);
          self.assert(vl.isNumber(a));
          self.assert(vl.isNumber(b));
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, std.math.shr(i64, vl.asIntNumber(i64, a), vl.asIntNumber(i64, b))));
          continue;
        },
        .Inv => {
          // inv rx, rk(x): rk(x) == bx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(inst, &rx, &rk);
          const a = self.RK(rk, fp);
          self.assert(vl.isNumber(a));
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, ~vl.asIntNumber(i64, a)));
          continue;
        },
        .Jt => {
          // jt rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          if (!vl.valueFalsy(fp.stack[rx])) {
            fp.ip += bx;
          }
          continue;
        },
        .Jf => {
          // jf rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          if (vl.valueFalsy(fp.stack[rx])) {
            fp.ip += bx;
          }
          continue;
        },
        .Jmp => {
          // jmp rx, bx
          var direction: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &direction, &bx);
          // 2 -> jmp fwd, 0 -> jmp bck
          @setRuntimeSafety(false);
          fp.ip = @intCast(usize, (@intCast(i64, fp.ip) + (@intCast(i64, direction) - 1) * @intCast(i64, bx)));
          continue;
        },
        .Not => {
          // not rx, rk(x)
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(inst, &rx, &rk);
          fp.stack[rx] = vl.boolVal(vl.valueFalsy(self.RK(rk, fp)));
          continue;
        },
        .Load => {
          // load rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(inst, &rx, &bx);
          fp.stack[rx] = fp.closure.fun.code.values.items[bx];
          continue;
        },
        .Nlst => {
          // nlst rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(inst, &rx, &count);
          fp.stack[rx] = vl.objVal(vl.createList(self, @as(usize, count)));
          continue;
        },
        .Slst => {
          // slst rx, rk(idx), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var list = vl.asList(fp.stack[rx]);
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(i64, list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          list.items[@intCast(usize, idx)] = self.RK(rk2, fp);
          continue;
        },
        .Glst => {
          // glst rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var list = vl.asList(self.RK(rk2, fp));
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(i64, list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          fp.stack[rx] = list.items[@intCast(usize, idx)];
          continue;
        },
        .Ntup => {
          // ntup rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(inst, &rx, &count);
          fp.stack[rx] = vl.objVal(vl.createTuple(self, @as(usize, count)));
          continue;
        },
        .Stup => {
          // stup rx, rk(idx), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var tuple = vl.asTuple(fp.stack[rx]);
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          // assumed safe
          tuple.items[@intCast(usize, idx)] = self.RK(rk2, fp);
          continue;
        },
        .Gtup => {
          // gtup rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var tuple = vl.asTuple(self.RK(rk2, fp));
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(i64, tuple.len);
          if (idx >= tuple.len) {
            return self.runtimeError("IndexError: tuple index out of range: {}", .{idx});
          }
          fp.stack[rx] = tuple.items[@intCast(usize, idx)];
          continue;
        },
        .Nmap => {
          // TODO: map specialization
          // nmap rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(inst, &rx, &count);
          fp.stack[rx] = vl.objVal(vl.createMap(self, @as(usize, count)));
          continue;
        },
        .Smap => {
          // smap rx, rk(key), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var map = vl.asMap(fp.stack[rx]);
          _ = map.meta.put(self.RK(rk1, fp), self.RK(rk2, fp), self);
          continue;
        },
        .Gmap => {
          // gmap rx, rk(key), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(inst, &rx, &rk1, &rk2);
          var map = vl.asMap(self.RK(rk2, fp));
          const key = self.RK(rk1, fp);
          if (map.meta.get(key)) |v| {
            fp.stack[rx] = v;
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
          fp.stack[rx] = vl.boolVal(!vl.valueFalsy(self.RK(rk, fp)));
          continue;
        },
        .Ret => {
          break;
        },
      }
    }
  }
};
