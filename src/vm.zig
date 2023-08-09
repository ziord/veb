const std = @import("std");
const vl = @import("value.zig");
const OpCode = @import("opcode.zig").OpCode;
const Mem = @import("mem.zig");
const GC = @import("gc.zig");
const VebAllocator = @import("allocator.zig");
const native = @import("native.zig");

const Value = vl.Value;
const Code = vl.Code;
const Inst = vl.Inst;
const ObjFiber = vl.ObjFiber;
const ObjFn = vl.ObjFn;
const ObjClosure = vl.ObjClosure;
const ObjUpvalue = vl.ObjUpvalue;
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
  has_error: bool = false,

  const Self = @This();
  const STACK_MAX = 0xfff;
  pub const MAX_GSYM_ITEMS = vl.MAX_REGISTERS << 1;
  pub const MAX_LOCAL_ITEMS = vl.MAX_REGISTERS;
  pub const MAX_LOCAL_ITEMS_BYTES = MAX_LOCAL_ITEMS * @sizeOf(Value);
  pub const MAX_FRAMES = 0x63ff;
  pub const MAX_STACK_ITEMS = 0xffff;
  const RuntimeError = error{RuntimeError};
  const TypeTag2CheckFunc = [_]*const fn(Value) bool {
    vl.isBoolNoInline, vl.isNumberNoInline, vl.isStringNoInline,
    vl.isNilNoInline, vl.isVoidNoInline, vl.isNoreturnNoInline,
    vl.isAnyNoInline,
    vl.isListNoInline, vl.isMapNoInline, vl.isTupleNoInline,
    vl.isErrorNoInline, vl.isFnNoInline, vl.isClosureNoInline,
    vl.isUpvalueNoInline, vl.isZFnNoInline, vl.isFiberNoInline
  };

  pub fn init(allocator: *VebAllocator) Self {
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
    native.addBuiltins(self);
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
    @setRuntimeSafety(false);
    return fp.closure.fun.code.values.items[pos];
  }

  inline fn RK(self: *Self, x: u32, fp: *CallFrame) Value {
    // rk(x) = r(x) if x < MAX_REGISTERS else k(x - MAX_REGISTERS)
    _ = self;
    @setRuntimeSafety(false);
    return if (x < vl.MAX_REGISTERS) fp.stack[x] else fp.closure.fun.code.values.items[x - vl.MAX_REGISTERS];
  }

  fn ensureStackCapacity(self: *Self) !void {
    if (
      (@divExact(
        @ptrToInt(self.fiber.stack + self.fiber.stack_cap)
        - @ptrToInt(self.fiber.fp.stack), @sizeOf(Value)
      ) > MAX_LOCAL_ITEMS)
    ) return;
    if (self.fiber.stack_cap >= MAX_STACK_ITEMS) {
      // TODO: unwind
      return self.runtimeError("Stack overflow: too many stack items", .{});
    }
    var old_stack = self.fiber.stack;
    const cap = Mem.alignTo(Mem.growCapacity(self.fiber.stack_cap), MAX_LOCAL_ITEMS);
    self.fiber.stack = self.mem.resizeBuf(
      Value, self,
      self.fiber.stack[0..self.fiber.stack_cap],
      self.fiber.stack_cap,
      cap
    ).ptr;
    self.fiber.stack_cap = cap;
    // reset the stack pointer for each call frame.
    if (self.fiber.stack != old_stack) {
      var i: usize = 0;
      while (i < self.fiber.frame_len) : (i += 1) {
        var frame = &self.fiber.frames[i];
        frame.stack = (
          self.fiber.stack
          + @divExact(@ptrToInt(frame.stack) - @ptrToInt(old_stack), @sizeOf(Value))
        );
      }
      // reset the stack pointer for each open upvalue.
      var upvalues = self.fiber.open_upvalues;
      while (upvalues) |upvalue| {
        upvalue.loc = (
          &(self.fiber.stack
          + (@divExact(@ptrToInt(upvalue.loc) - @ptrToInt(old_stack), @sizeOf(Value))))[0]
        );
        upvalues = upvalue.next;
      }
    }
  }

  fn ensureFrameCapacity(self: *Self, fp: **CallFrame, fiber: **ObjFiber) !void {
    try self.ensureStackCapacity();
    if (self.fiber.frame_len + 1 < self.fiber.frame_cap) return;
    if (self.fiber.frame_cap >= MAX_FRAMES) {
      // TODO: unwind
      return self.runtimeError("Stack overflow: too many call frames", .{});
    }
    var cap = Mem.alignTo(Mem.growCapacity(self.fiber.frame_cap), Mem.BUFFER_INIT_SIZE);
    self.fiber.frames = self.mem.resizeBuf(
      CallFrame, self,
      self.fiber.frames[0..self.fiber.frame_cap],
      self.fiber.frame_cap,
      cap
    ).ptr;
    self.fiber.frame_cap = cap;
    self.fiber.fp = &self.fiber.frames[self.fiber.frame_len - 1];
    fp.* = self.fiber.fp;
    fiber.* = self.fiber;
  }

  fn captureUpvalue(self: *Self, fiber: *ObjFiber, local: *Value) *ObjUpvalue {
    var current = fiber.open_upvalues;
    var previous: ?*ObjUpvalue = null;
    // try to find a position where `local` fits
    while (current != null and @ptrToInt(current.?.loc) > @ptrToInt(local)) {
      previous = current;
      current = current.?.next;
    }
    // if we already captured this local, just return current
    if (current != null and current.?.loc == local) {
      return current.?;
    }
    var upv = vl.createUpvalue(self, local);
    upv.next = current;
    if (previous) |prev| {
      prev.next = upv;
    } else {
      // open_upvalues is null
      fiber.open_upvalues = upv;
    }
    return upv;
  }

  fn closeUpvalues(fiber: *ObjFiber, slot: *Value) void {
    while (fiber.open_upvalues != null and @ptrToInt(fiber.open_upvalues.?.loc) >= @ptrToInt(slot)) {
      var tmp = fiber.open_upvalues.?;
      tmp.value = tmp.loc.*;
      tmp.loc = &tmp.value;
      fiber.open_upvalues = tmp.next;
    }
  }

  inline fn assert(self: *Self, cond: bool) void {
    _ = self;
    std.debug.assert(cond);
  }

  fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) RuntimeError {
    self.has_error = true;
    std.debug.print(fmt ++ "\n", args);
    return error.RuntimeError;
  }

  pub fn panickUnwindError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    // TODO: unwind the stack
    self.has_error = true;
    std.debug.print(fmt ++ "\n", args);
  }

  pub inline fn printStack(self: *Self) void {
    for (self.fiber.stack[0..self.fiber.stack_cap]) |val| {
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
      const code = @call(.always_inline, Self.readWord, .{self, fp});
      switch (@call(.always_inline, Code.readInstOp, .{code})) {
        .Load => {
          // load rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          fp.stack[rx] = fp.closure.fun.code.values.items[bx];
          continue;
        },
        .Gglb => {
          // gglb rx, bx -> r(x) = G[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          var glb = vl.asString(self.readConst(bx, fp));
          var val = self.globals.get(glb);
          self.assert(val != null);
          @setRuntimeSafety(false);
          fp.stack[rx] = val.?;
          continue;
        },
        .Sglb => {
          // sglb rx, bx -> G[K(bx)] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          var glb = vl.asString(self.readConst(bx, fp));
          _ = self.globals.put(glb, fp.stack[rx], self);
          continue;
        },
        .Ggsym => {
          // ggsym rx, bx -> r(x) = GS[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          fp.stack[rx] = self.gsyms[bx];
          continue;
        },
        .Sgsym => {
          // sgsym rx, bx -> GS[bx] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          self.gsyms[bx] = fp.stack[rx];
          continue;
        },
        .Mov => {
          // mov rx, ry
          var rx: u32 = undefined;
          var ry: u32 = undefined;
          var dum: u32 = undefined;
          self.read3Args(code, &rx, &ry, &dum);
          @setRuntimeSafety(false);
          fp.stack[rx] = fp.stack[ry];
          continue;
        },
        .Add => {
          // add rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(vl.asNumber(self.RK(rk1, fp)) + vl.asNumber(self.RK(rk2, fp)));
          continue;
        },
        .Sub => {
          // sub rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(vl.asNumber(self.RK(rk1, fp)) - vl.asNumber(self.RK(rk2, fp)));
          continue;
        },
        .Mul => {
          // mul rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(vl.asNumber(self.RK(rk1, fp)) * vl.asNumber(self.RK(rk2, fp)));
          continue;
        },
        .Div => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(vl.asNumber(self.RK(rk1, fp)) / vl.asNumber(self.RK(rk2, fp)));
          continue;
        },
        .Cles => {
          // cles rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(self.RK(rk1, fp) < self.RK(rk2, fp));
          continue;
        },
        .Cgrt => {
          // cgrt rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(self.RK(rk1, fp) > self.RK(rk2, fp));
          continue;
        },
        .Cleq => {
          // cleq rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(self.RK(rk1, fp) <= self.RK(rk2, fp));
          continue;
        },
        .Cgeq => {
          // cgeq rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(self.RK(rk1, fp) >= self.RK(rk2, fp));
          continue;
        },
        .Ceqq => {
          // ceqq rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(@call(.always_inline, vl.valueEqual, .{self.RK(rk1, fp), self.RK(rk2, fp)}));
          continue;
        },
        .Cneq => {
          // cneq rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(!@call(.always_inline, vl.valueEqual, .{self.RK(rk1, fp), self.RK(rk2, fp)}));
          continue;
        },
        .Bclo => {
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          var clo = vl.createClosure(self, vl.asFn(self.RK(bx, fp)));
          fp.stack[rx] = vl.objVal(clo);
          var i: usize = 0;
          while (i < clo.fun.envlen) : (i += 1) {
            var next_inst = @call(.always_inline, Self.readWord, .{self, fp});
            var is_local = Code.readRX(next_inst);
            var slot = Code.readBX(next_inst);
            if (is_local == 1) {
              clo.env[i] = self.captureUpvalue(fiber, &fp.stack[slot]);
            } else {
              clo.env[i] = fp.closure.env[slot];
            }
          }
          continue;
        },
        .Call => {
          var rx: u32 = undefined;
          var n: u32 = undefined;
          self.read2Args(code, &rx, &n);
          try self.ensureFrameCapacity(&fp, &fiber);
          var val = fp.stack[rx];
          if (vl.isClosure(val)) {
            fiber.appendFrame(vl.asClosure(val), fp.stack + rx + 1);
            fp = fiber.fp;
            continue;
          } else if (vl.isMethod(val)) {
            var mtd = vl.asMethod(val);
            fiber.appendFrame(mtd.closure, fp.stack + rx + 1);
            (fp.stack + rx + 1)[0] = mtd.instance;
            fp = fiber.fp;
            continue;
          } else {
            var func = vl.asZFn(val);
            var res = func.fun(self, n, rx + 1);
            if (self.has_error) return error.RuntimeError;
            fp.stack[rx] = res;
            continue;
          }
        },
        .Jmtd => {
          // super instruction for get mtd & call mtd
          // jmtd rx, rk(inst), rk(prop.idx) | call rx, bx
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          var inst = self.RK(rk1, fp);
          var closure = vl.asClosure(vl.asInstance(inst).cls.methods[idx]);
          var next = @call(.always_inline, Self.readWord, .{self, fp});
          self.read2Args(next, &rx, &rk1);
          fiber.appendFrame(closure, fp.stack + rx + 1);
          (fp.stack + rx + 1)[0] = inst;
          fp = fiber.fp;
          continue;
        },
        .Callc => {
          // callc rx, argc, flen
          var rx: u32 = undefined;
          var n: u32 = undefined;
          var flen: u32 = undefined;
          self.read3Args(code, &rx, &n, &flen);
          try self.ensureFrameCapacity(&fp, &fiber);
          var cls = vl.asClass(fp.stack[rx]);
          var inst = vl.createInstance(self, cls, flen);
          fp.stack[rx] = vl.objVal(inst);
          continue;
        },
        .Ret => {
          var rx: u32 = Code.readRX(code);
          var res = fp.stack[rx];
          var frame = fiber.popFrame();
          closeUpvalues(fiber, &frame.stack[0]);
          fp = fiber.fp;
          if (fiber.frame_len == 0) {
            if (fiber.caller == null) {
              // TODO: handle result
              return;
            }
            // TODO: handle resuming fibers
          } else {
            // place result where future instructions expect it; at the function's slot
            (frame.stack - 1)[0] = res;
          }
          continue;
        },
        .Asrt => {
          // asrt rx
          if (vl.isNil(fp.stack[vl.Code.readRX(code)])) {
            return self.runtimeError("Attempt to use value 'nil'", .{});
          }
          continue;
        },
        .Or => {
          // or rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          fp.stack[rx] = vl.numberVal(
            @intToFloat(f64, (vl.asIntNumber(i64, self.RK(rk1, fp)) | vl.asIntNumber(i64, self.RK(rk2, fp))))
          );
          continue;
        },
        .And => {
          // and rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(
            @intToFloat(f64, (vl.asIntNumber(i64, self.RK(rk1, fp)) & vl.asIntNumber(i64, self.RK(rk2, fp))))
          );
          continue;
        },
        .Gupv => {
          // gupv rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          fp.stack[rx] = fp.closure.env[bx].loc.*;
          continue;
        },
        .Supv => {
          // supv rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          fp.closure.env[bx].loc.* = fp.stack[rx];
          continue;
        },
        .Cupv => {
          closeUpvalues(fiber, &fp.stack[Code.readRX(code)]);
          continue;
        },
        .Is => {
          // is rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var ty_tag: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &ty_tag);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(TypeTag2CheckFunc[ty_tag](self.RK(rk1, fp)));
          continue;
        },
        .Iscls => {
          // iscls rx, rk(x), rk(y)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          var inst = vl.asInstance(self.RK(rk1, fp));
          var cls = vl.asClass(self.RK(rk2, fp));
          fp.stack[rx] = vl.boolVal((inst.cls == cls));
          continue;
        },
        .Xor => {
          // xor rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(
            @intToFloat(f64, (vl.asIntNumber(i64, self.RK(rk1, fp)) ^ vl.asIntNumber(i64, self.RK(rk2, fp))))
          );
          continue;
        },
        .Shl => {
          // shl rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(
            @intToFloat(f64, std.math.shl(i64, vl.asIntNumber(i64, self.RK(rk1, fp)), vl.asIntNumber(i64, self.RK(rk2, fp))))
          );
          continue;
        },
        .Shr => {
          // shr rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          fp.stack[rx] = vl.numberVal(
            @intToFloat(f64, std.math.shr(i64, vl.asIntNumber(i64, self.RK(rk1, fp)), vl.asIntNumber(i64, self.RK(rk2, fp))))
          );
          continue;
        },
        .Inv => {
          // inv rx, rk(x): rk(x) == bx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(code, &rx, &rk);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(@intToFloat(f64, ~vl.asIntNumber(i64, self.RK(rk, fp))));
          continue;
        },
         .Mod => {
          // div rx, rk(x), rk(x)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          const b = self.RK(rk2, fp);
          if (@call(.always_inline, vl.asNumber, .{b}) == 0) {
            return self.runtimeError("Modulo by zero\n", .{});
          }
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(@rem(vl.asNumber(self.RK(rk1, fp)), vl.asNumber(b)));
          continue;
        },
        .Jt => {
          // jt rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          if (!vl.valueFalsy(fp.stack[rx])) {
            fp.ip += bx;
          }
          continue;
        },
        .Jf => {
          // jf rx, bx
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          if (vl.valueFalsy(fp.stack[rx])) {
            fp.ip += bx;
          }
          continue;
        },
        .Jmp => {
          // jmp rx, bx
          var direction: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &direction, &bx);
          // 2 -> jmp fwd, 0 -> jmp bck
          @setRuntimeSafety(false);
          fp.ip = @intCast(usize, (@intCast(i64, fp.ip) + (@intCast(i64, direction) - 1) * @intCast(i64, bx)));
          continue;
        },
        .Not => {
          // not rx, rk(x)
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(code, &rx, &rk);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(vl.valueFalsy(self.RK(rk, fp)));
          continue;
        },
        .Finc => {
          // finc rx (1-arg using 2-arg fmt)
          var rx: u32 = undefined;
          var tmp: u32 = undefined;
          self.read2Args(code, &rx, &tmp);
          try self.ensureFrameCapacity(&fp, &fiber);
          var inst = fp.stack[rx];
          var init_mtd = vl.asInstance(inst).cls.getInitMethod().?;
          fiber.appendFrame(vl.asClosure(init_mtd), fp.stack + rx + 1);
          (fp.stack + rx + 1)[0] = inst;
          fp = fiber.fp;
          continue;
        },
        .Smtd => {
          // smtd rx(mth), rk(cls), idx
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          vl.asClass(self.RK(rk1, fp)).methods[idx] = fp.stack[rx];
          continue;
        },
        .Gmtd => {
          // gmtd rx, rk(inst), rk(prop.idx)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          var inst = self.RK(rk1, fp);
          var mtd = vl.createBMethod(self, inst, vl.asClosure(vl.asInstance(inst).cls.methods[idx]));
          fp.stack[rx] = vl.objVal(mtd);
          continue;
        },
        .Sfd => {
          // sfd rx(inst), prop.idx, rk(value)
          var rx: u32 = undefined;
          var idx: u32 = undefined;
          var rk: u32 = undefined;
          self.read3Args(code, &rx, &idx, &rk);
          vl.asInstance(fp.stack[rx]).fields[idx] = self.RK(rk, fp);
          continue;
        },
        .Gfd => {
          // gfd rx, rk(inst), prop.idx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk, &idx);
          fp.stack[rx] = vl.asInstance(self.RK(rk, fp)).fields[idx];
          continue;
        },
        .Nerr => {
          // nerr rx, val
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(code, &rx, &rk);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(vl.createError(self, self.RK(rk, fp)));
          continue;
        },
        .Nlst => {
          // nlst rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(code, &rx, &count);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(vl.createList(self, @as(usize, count)));
          continue;
        },
        .Slst => {
          // slst rx, rk(idx), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          var list = vl.asList(fp.stack[rx]);
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(i64, list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          @setRuntimeSafety(false);
          list.items[@intCast(usize, idx)] = self.RK(rk2, fp);
          continue;
        },
        .Glst => {
          // glst rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          var list = vl.asList(self.RK(rk2, fp));
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(i64, list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          @setRuntimeSafety(false);
          fp.stack[rx] = list.items[@intCast(usize, idx)];
          continue;
        },
        .Ntup => {
          // ntup rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(code, &rx, &count);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(vl.createTuple(self, @as(usize, count)));
          continue;
        },
        .Stup => {
          // stup rx, rk(idx), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          var tuple = vl.asTuple(fp.stack[rx]);
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          // assumed safe
          @setRuntimeSafety(false);
          tuple.items[@intCast(usize, idx)] = self.RK(rk2, fp);
          continue;
        },
        .Gtup => {
          // gtup rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          var tuple = vl.asTuple(self.RK(rk2, fp));
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(i64, tuple.len);
          if (idx >= tuple.len) {
            return self.runtimeError("IndexError: tuple index out of range: {}", .{idx});
          }
          @setRuntimeSafety(false);
          fp.stack[rx] = tuple.items[@intCast(usize, idx)];
          continue;
        },
        .Nmap => {
          // TODO: map specialization
          // nmap rx, count
          var rx: u32 = undefined;
          var count: u32 = undefined;
          self.read2Args(code, &rx, &count);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(vl.createMap(self, @as(usize, count)));
          continue;
        },
        .Smap => {
          // smap rx, rk(key), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          var map = vl.asMap(fp.stack[rx]);
          _ = map.meta.put(self.RK(rk1, fp), self.RK(rk2, fp), self);
          continue;
        },
        .Gmap => {
          // gmap rx, rk(key), rk(val)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
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
          self.read2Args(code, &rx, &rk);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.boolVal(!vl.valueFalsy(self.RK(rk, fp)));
          continue;
        },
      }
    }
  }
};
