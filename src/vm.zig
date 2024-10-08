const std = @import("std");
const vl = @import("value.zig");
const OpCode = @import("opcode.zig").OpCode;
const Mem = @import("mem.zig");
const GC = @import("gc.zig");
const VebAllocator = @import("allocator.zig");
const native = @import("native.zig");
const debug = @import("debug.zig");

const Value = vl.Value;
const Code = vl.Code;
const Inst = vl.Inst;
const ObjFiber = vl.ObjFiber;
const ObjFn = vl.ObjFn;
const ObjClosure = vl.ObjClosure;
const ObjUpvalue = vl.ObjUpvalue;
const ObjClass = vl.ObjClass;
const CallFrame = vl.CallFrame;
const StringHashMap = vl.StringHashMap;
const Dis = debug.Disassembler;
const ValueStringWriter = vl.ValueStringWriter;
const ks = vl.ks;

pub const VM = struct {
  fiber: *ObjFiber,
  strings: StringHashMap,
  tags: StringHashMap,
  globals: StringHashMap,
  externs: vl.Vec(Value),
  objects: ?*vl.Obj,
  classes: BuiltinCls,
  names: CachedNames,
  mem: Mem,
  gc: GC,
  vsw: ValueStringWriter,
  temp_roots: vl.Vec(Value),
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
    vl.isUpvalueNoInline, vl.isNativeFnNoInline, vl.isFiberNoInline
  };

  const BuiltinCls = struct {
    string: *ObjClass,
    list: *ObjClass,
    map: *ObjClass,
    tuple: *ObjClass,
    err: *ObjClass,

    pub fn init() @This() {
      return .{
        .string = undefined,
        .list = undefined,
        .map = undefined,
        .tuple = undefined,
        .err = undefined,
      };
    }
  };

  const CachedNames = struct {
    init: *vl.ObjString = undefined,
    ok: *vl.ObjString = undefined,
    err: *vl.ObjString = undefined,
    just: *vl.ObjString = undefined,

    pub fn new() @This() {
      return .{};
    }
  };

  pub fn init(allocator: *VebAllocator) Self {
    const al = allocator.getAllocator();
    var vm = Self {
      .fiber = undefined,
      .mem = Mem.init(al),
      .gc = GC.init(allocator, undefined),
      .strings = StringHashMap.init(),
      .tags = StringHashMap.init(),
      .globals = StringHashMap.init(),
      .externs = vl.Vec(Value).init(),
      .objects = null,
      .classes = BuiltinCls.init(),
      .names = CachedNames.new(),
      .temp_roots = vl.Vec(Value).init(),
      .vsw = ValueStringWriter.init(),
    };
    vm.gc.skip_collection = true;
    native.addAll(&vm);
    return vm;
  }

  pub inline fn initGC(self: *Self) void {
    self.gc.vm = self;
  }

  pub fn boot(self: *Self, fun: *ObjFn) void {
    self.fiber = vl.createFiber(self, vl.createClosure(self, fun), .Root, null);
    self.gc.skip_collection = false;
  }

  pub fn shutdown(self: *Self) void {
    self.deinit();
  }

  pub fn deinit(self: *Self) void {
    var curr = self.objects;
    while (curr) |cur| {
      const next = cur.next;
      self.gc.freeObject(cur, self);
      curr = next;
    }
    self.strings.free(self);
    self.tags.free(self);
    self.globals.free(self);
    self.externs.deinit(self);
    self.temp_roots.deinit(self);
    self.vsw.deinit(self);
    self.gc.deinit();
    self.mem.deinit();
  }

  pub fn getExtern(self: *Self, module: []const u8, name: *vl.ObjString) ?Value {
    if (native.ExternMapping.get(module)) |pos| {
      const externs = self.externs.getItems()[pos.start..pos.end];
      for (externs) |ext| {
        if (name == vl.asNativeFn(ext).name) {  
          return ext;
        }
      }
    }
    return null;
  }

  inline fn readWord(self: *Self, fp: *CallFrame) Inst {
    _ = self;
    fp.ip += 1;
    @setRuntimeSafety(false);
    return fp.closure.fun.code.words.items[fp.ip - 1];
  }

  inline fn read3Args(self: *Self, word: Inst, a1: *u32, a2: *u32, a3: *u32) void {
    _ = self;
    a1.* = (word >> 6) & Code._8Bits;
    a2.* = (word >> 14) & Code._9Bits;
    a3.* = (word >> 23) & Code._9Bits;
  }

  inline fn read2Args(self: *Self, word: Inst, a1: *u32, a2: *u32) void {
    _ = self;
    a1.* = (word >> 6) & Code._8Bits;
    a2.* = (word >> 14) & Code._18Bits;
  }

  inline fn readSigned2Args(self: *Self, word: Inst, sbx: *i32) void {
    _ = self;
    const mag: i32 = @intCast(((word & ((1 << 31) - 1)) >> 14) & Code._17Bits);
    sbx.* = mag - @as(i32, @intCast(((word >> 31) & 1) << 17));
  }

  inline fn read1Arg(self: *Self, word: Inst, a: *u32) void {
    _ = self;
    a.* = (word >> 6);
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
        @intFromPtr(self.fiber.stack + self.fiber.stack_cap)
        - @intFromPtr(self.fiber.fp.stack), @sizeOf(Value)
      ) > MAX_LOCAL_ITEMS)
    ) return;
    if (self.fiber.stack_cap >= MAX_STACK_ITEMS) {
      // TODO: unwind
      return self.runtimeError("Stack overflow: too many stack items", .{});
    }
    const old_stack = self.fiber.stack;
    const cap = Mem.alignTo(Mem.growCapacity(self.fiber.stack_cap), MAX_LOCAL_ITEMS);
    self.fiber.stack = self.mem.resizeBuf(
      Value, self,
      self.fiber.stack,
      self.fiber.stack_cap,
      cap
    ).ptr;
    @memset(self.fiber.stack[self.fiber.stack_cap..cap], vl.NOTHING_VAL);
    self.fiber.stack_cap = cap;
    // reset the stack pointer for each call frame.
    if (self.fiber.stack != old_stack) {
      var i: usize = 0;
      while (i < self.fiber.frame_len) : (i += 1) {
        var frame = &self.fiber.frames[i];
        frame.stack = (
          self.fiber.stack
          + @divExact(@intFromPtr(frame.stack) - @intFromPtr(old_stack), @sizeOf(Value))
        );
      }
      // reset the stack pointer for each open upvalue.
      var upvalues = self.fiber.open_upvalues;
      while (upvalues) |upvalue| {
        upvalue.loc = (
          &(self.fiber.stack
          + (@divExact(@intFromPtr(upvalue.loc) - @intFromPtr(old_stack), @sizeOf(Value))))[0]
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
    const cap = Mem.alignTo(Mem.growCapacity(self.fiber.frame_cap), Mem.BUFFER_INIT_SIZE);
    self.fiber.frames = self.mem.resizeBuf(
      CallFrame, self,
      self.fiber.frames,
      self.fiber.frame_cap,
      cap
    ).ptr;
    self.fiber.frame_cap = cap;
    self.fiber.fp = &self.fiber.frames[self.fiber.frame_len - 1];
    fp.* = self.fiber.fp;
    fiber.* = self.fiber;
  }

  fn captureUpvalue(self: *Self, fiber: *ObjFiber, local: *Value) *ObjUpvalue {
    @setRuntimeSafety(false);
    var current = fiber.open_upvalues;
    var previous: ?*ObjUpvalue = null;
    // try to find a position where `local` fits
    while (current != null and @intFromPtr(current.?.loc) > @intFromPtr(local)) {
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
    @setRuntimeSafety(false);
    while (fiber.open_upvalues != null and @intFromPtr(fiber.open_upvalues.?.loc) >= @intFromPtr(slot)) {
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

  pub fn reportRuntimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    self.has_error = true;
    std.debug.print("RuntimeError: " ++ fmt ++ "\n", args);
  }

  fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) RuntimeError {
    self.reportRuntimeError(fmt, args);
    return error.RuntimeError;
  }

  pub fn panicUnwindError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    // TODO: unwind the stack
    self.has_error = true;
    std.debug.print(fmt ++ "\n", args);
  }

  pub inline fn printStack(self: *Self, max: ?usize) void {
    const len = max orelse self.fiber.stack_cap;
    for (self.fiber.stack[0..len]) |val| {
      std.debug.print("[ ", .{});
      vl.printValue(val);
      std.debug.print(" ]", .{});
    }
    std.debug.print("\n", .{});
  }

  fn printValue(self: *Self, comptime str: []const u8, val: Value) void {
    _ = self;
    std.debug.print(str, .{});
    vl.printValue(val);
    std.debug.print("\n", .{});
  }

  pub fn run(self: *Self) RuntimeError!void {
    var fiber = self.fiber;
    var fp = fiber.fp;
    var gsyms = fp.closure.fun.module.items;
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
        .Ltrue => {
          // ltrue rx, _bx
          var rx: u32 = undefined;
          var _bx: u32 = undefined;
          self.read2Args(code, &rx, &_bx);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.TRUE_VAL;
          continue;
        },
        .Lfalse => {
          // lfalse rx, _bx
          var rx: u32 = undefined;
          var _bx: u32 = undefined;
          self.read2Args(code, &rx, &_bx);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.FALSE_VAL;
          continue;
        },
        .Gglb => {
          // gglb rx, bx -> r(x) = G[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          const glb = vl.asString(self.readConst(bx, fp));
          const val = self.globals.get(glb, self);
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
          const glb = vl.asString(self.readConst(bx, fp));
          _ = self.globals.set(glb, fp.stack[rx], self);
          continue;
        },
        .Ggsym => {
          // ggsym rx, bx -> r(x) = GS[K(bx)]
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          fp.stack[rx] = gsyms[bx];
          continue;
        },
        .Sgsym => {
          // sgsym rx, bx -> GS[bx] = r(x)
          var rx: u32 = undefined;
          var bx: u32 = undefined;
          self.read2Args(code, &rx, &bx);
          @setRuntimeSafety(false);
          gsyms[bx] = fp.stack[rx];
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
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(clo);
          var i: usize = 0;
          while (i < clo.fun.envlen) : (i += 1) {
            const next_inst = @call(.always_inline, Self.readWord, .{self, fp});
            const is_local = Code.readRX(next_inst);
            const slot = Code.readBX(next_inst);
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
          const val = fp.stack[rx];
          if (vl.isClosure(val)) {
            fiber.appendFrame(vl.asClosure(val), fp.stack + rx);
            fp = fiber.fp;
            gsyms = fp.closure.fun.module.items;
          } else if (vl.isMethod(val)) {
            var mtd = vl.asMethod(val);
            if (mtd.isBoundUserMethod()) {
              fiber.appendFrame(mtd.as.user.closure, fp.stack + rx);
              @setRuntimeSafety(false);
              fp.stack[rx] = mtd.as.user.instance;
              fp = fiber.fp;
              gsyms = fp.closure.fun.module.items;
            } else {
              fp.stack[rx] = mtd.as.native.instance;
              const res = mtd.as.native.fun.fun(self, n, rx);
              if (self.has_error) return error.RuntimeError;
              fp.stack[rx] = res;
            }
          } else {
            var func = vl.asNativeFn(val);
            const res = func.fun(self, n, rx + 1);
            if (self.has_error) return error.RuntimeError;
            fp.stack[rx] = res;
          }
          continue;
        },
        .Jnextc => {
          // super instruction for get mtd & call mtd
          // Jnextc rx, rk(inst), rk(prop.idx) | call rx, bx
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          const inst = self.RK(rk1, fp);
          if (vl.isInstance(inst)) {
            const closure = vl.asClosure(vl.asObj(inst).cls.?.items[idx]);
            const next = @call(.always_inline, Self.readWord, .{self, fp});
            self.read2Args(next, &rx, &rk1);
            fiber.appendFrame(closure, fp.stack + rx);
            @setRuntimeSafety(false);
            fp.stack[rx] = inst;
            fp = fiber.fp;
            gsyms = fp.closure.fun.module.items;
          } else {
            var func = vl.asNativeFn(vl.asObj(inst).cls.?.items[idx]);
            const next = @call(.always_inline, Self.readWord, .{self, fp});
            self.read2Args(next, &rx, &rk1);
            fp.stack[rx] = inst;
            const res = func.fun(self, rk1, rx);
            if (self.has_error) return error.RuntimeError;
            @setRuntimeSafety(false);
            fp.stack[rx] = res;
            fp = fiber.fp;
            gsyms = fp.closure.fun.module.items;
          }
          continue;
        },
        .Callc => {
          // callc rx, argc, flen
          var rx: u32 = undefined;
          var n: u32 = undefined;
          var flen: u32 = undefined;
          self.read3Args(code, &rx, &n, &flen);
          try self.ensureFrameCapacity(&fp, &fiber);
          const cls = vl.asClass(fp.stack[rx]);
          const inst = vl.createInstance(self, cls, flen);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(inst);
          continue;
        },
        .Jnextcs => {
          // super instruction for get mtd slow & call mtd
          // Jnextcs rx, rk(inst), rk(prop.idx) | call rx, bx  {Jnextc slow}
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          const inst = self.RK(rk1, fp);
          const prop = self.readConst(idx, fp);
          if (vl.isInstance(inst)) {
            const closure = vl.asClosure(vl.asObj(inst).cls.?.getMethodWithName(vl.asString(prop)).?);
            const next = @call(.always_inline, Self.readWord, .{self, fp});
            self.read2Args(next, &rx, &rk1);
            fiber.appendFrame(closure, fp.stack + rx);
            @setRuntimeSafety(false);
            fp.stack[rx] = inst;
            fp = fiber.fp;
            gsyms = fp.closure.fun.module.items;
          } else {
            var func = vl.asNativeFn(vl.asObj(inst).cls.?.getNativeMethodWithName(vl.asString(prop)).?);
            const next = @call(.always_inline, Self.readWord, .{self, fp});
            self.read2Args(next, &rx, &rk1);
            fp.stack[rx] = inst;
            const res = func.fun(self, rk1, rx);
            if (self.has_error) return error.RuntimeError;
            @setRuntimeSafety(false);
            fp.stack[rx] = res;
            fp = fiber.fp;
            gsyms = fp.closure.fun.module.items;
          }
          continue;
        },
        .Ret => {
          const rx: u32 = Code.readRX(code);
          const res = fp.stack[rx];
          var frame = fiber.popFrame();
          closeUpvalues(fiber, &frame.stack[0]);
          fp = fiber.fp;
          gsyms = fp.closure.fun.module.items;
          if (fiber.frame_len == 0) {
            if (fiber.caller == null) {
              // TODO: handle result
              return;
            }
            // TODO: handle resuming fibers
          } else {
            // place result where future instructions expect it; at the function's slot
            frame.stack[0] = res;
          }
          continue;
        },
        .Asrt => {
          // asrt rx
          if (vl.isNil(fp.stack[vl.Code.readRX(code)])) {
            return self.runtimeError("Attempt to use value '" ++ ks.NoneVar ++ "'", .{});
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
            @floatFromInt((vl.asIntNumber(i64, self.RK(rk1, fp)) | vl.asIntNumber(i64, self.RK(rk2, fp))))
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
            @floatFromInt((vl.asIntNumber(i64, self.RK(rk1, fp)) & vl.asIntNumber(i64, self.RK(rk2, fp))))
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
          const obj = vl.asObj(self.RK(rk1, fp));
          const cls = vl.asClass(self.RK(rk2, fp));
          fp.stack[rx] = vl.boolVal((obj.cls.? == cls));
          continue;
        },
        .Istag => {
          // istag rx, rk(x), rk(y)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          const val = self.RK(rk1, fp);
          if (!vl.isNil(val)) {
            const name = if (vl.isStruct(val)) vl.asStruct(val).name else vl.asTag(val).name;
            fp.stack[rx] = vl.boolVal(name == vl.asString(self.RK(rk2, fp)));
          } else {
            fp.stack[rx] = vl.FALSE_VAL;
          }
          continue;
        },
        .Istoc => {
          // istoc rx, rk(x), rk(y)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          @setRuntimeSafety(false);
          const val = self.RK(rk1, fp);
          const name = vl.asString(self.RK(rk2, fp));
          if (vl.isNil(val)) {
            fp.stack[rx] = vl.FALSE_VAL;
          } else if (vl.asObj(val).cls) |cls| { // cls/instance
            fp.stack[rx] = vl.boolVal(cls.name == name);
          } else if (vl.isStruct(val)) { // parameterized tags
            fp.stack[rx] = vl.boolVal(vl.asStruct(val).name == name);
          } else if (vl.isTag(val)) { // non-parameterized tags
            fp.stack[rx] = vl.boolVal(vl.asTag(val).name == name);
          } else {
            fp.stack[rx] = vl.FALSE_VAL;
          }
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
            @floatFromInt((vl.asIntNumber(i64, self.RK(rk1, fp)) ^ vl.asIntNumber(i64, self.RK(rk2, fp))))
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
            @floatFromInt(std.math.shl(i64, vl.asIntNumber(i64, self.RK(rk1, fp)), vl.asIntNumber(i64, self.RK(rk2, fp))))
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
            @floatFromInt(std.math.shr(i64, vl.asIntNumber(i64, self.RK(rk1, fp)), vl.asIntNumber(i64, self.RK(rk2, fp))))
          );
          continue;
        },
        .Inv => {
          // inv rx, rk(x): rk(x) == bx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(code, &rx, &rk);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.numberVal(@floatFromInt(~vl.asIntNumber(i64, self.RK(rk, fp))));
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
          // jmp rx, sbx
          var bx: i32 = undefined;
          self.readSigned2Args(code, &bx);
          @setRuntimeSafety(false);
          fp.ip = @intCast((@as(i64, @intCast(fp.ip))) + @as(i64, @intCast(bx)));
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
        .Fcls => {
          // fcls rx (1-arg using 2-arg fmt)
          var rx: u32 = undefined;
          var tmp: u32 = undefined;
          self.read2Args(code, &rx, &tmp);
          try self.ensureFrameCapacity(&fp, &fiber);
          const init_mtd = vl.asObj(fp.stack[rx]).cls.?.getInitMethod(self).?;
          fiber.appendFrame(vl.asClosure(init_mtd), fp.stack + rx);
          fp = fiber.fp;
          gsyms = fp.closure.fun.module.items;
          continue;
        },
        .Smtd => {
          // smtd rx(mth), rk(cls), idx
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          @setRuntimeSafety(false);
          vl.asClass(self.RK(rk1, fp)).items[idx] = fp.stack[rx];
          continue;
        },
        .Gmtd => {
          // gmtd rx, rk(inst), rk(prop.idx)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          const inst = self.RK(rk1, fp);
          const mtd = (
            if (vl.isInstance(inst)) vl.createBoundUserMethod(self, inst, vl.asClosure(vl.asObj(inst).cls.?.items[idx]))
            else vl.createBoundNativeMethod(self, inst, vl.asNativeFn(vl.asObj(inst).cls.?.items[idx]))
          );
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(mtd);
          continue;
        },
        .Sfd => {
          // sfd rx(inst), prop.idx, rk(value)
          var rx: u32 = undefined;
          var idx: u32 = undefined;
          var rk: u32 = undefined;
          self.read3Args(code, &rx, &idx, &rk);
          @setRuntimeSafety(false);
          vl.asInstance(fp.stack[rx]).fields[idx] = self.RK(rk, fp);
          continue;
        },
        .Ssfd => {
          // ssfd rx(inst), prop.idx, rk(value)
          var rx: u32 = undefined;
          var idx: u32 = undefined;
          var rk: u32 = undefined;
          self.read3Args(code, &rx, &idx, &rk);
          @setRuntimeSafety(false);
          vl.asStruct(fp.stack[rx]).items[idx] = self.RK(rk, fp);
          continue;
        },
        .Gfd => {
          // gfd rx, rk(inst), prop.idx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk, &idx);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.asInstance(self.RK(rk, fp)).fields[idx];
          continue;
        },
        .Gsfd => {
          // gsfd rx, rk(inst), prop.idx
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk, &idx);
          @setRuntimeSafety(false);
          const val = self.RK(rk, fp);
          fp.stack[rx] = if (vl.isStruct(val)) vl.asStruct(self.RK(rk, fp)).items[idx] else vl.asError(val).val;
          continue;
        },
        .Gmtds => {
          // gmtds rx, rk(inst), rk(prop.idx) {gmtd slow}
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          const prop = self.readConst(idx, fp);
          const inst = self.RK(rk1, fp);
          const mtd = (
            if (vl.isInstance(inst)) 
              vl.createBoundUserMethod(self, inst, vl.asClosure(vl.asObj(inst).cls.?.getMethodWithName(vl.asString(prop)).?))
            else 
              vl.createBoundNativeMethod(self, inst, vl.asNativeFn(vl.asObj(inst).cls.?.getMethodWithName(vl.asString(prop)).?))
          );
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.objVal(mtd);
          continue;
        },
        .Gmsym => {
          // gmsym rx, rk(mod), idx
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var idx: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &idx);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.asModule(self.RK(rk1, fp)).items[idx];
          continue;
        },
        .Smsym => {
          // smsym rx(mod), idx, rk(value)
          var rx: u32 = undefined;
          var idx: u32 = undefined;
          var rk: u32 = undefined;
          self.read3Args(code, &rx, &idx, &rk);
          @setRuntimeSafety(false);
          vl.asModule(fp.stack[rx]).items[idx] = self.RK(rk, fp);
          continue;
        },
        .Nerr => {
          // nerr rx, val
          var rx: u32 = undefined;
          var rk: u32 = undefined;
          self.read2Args(code, &rx, &rk);
          @setRuntimeSafety(false);
          fp.stack[rx] = vl.createError(self, self.RK(rk, fp));
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
          if (idx < 0) idx += @intCast(list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          @setRuntimeSafety(false);
          list.items[@intCast(idx)] = self.RK(rk2, fp);
          continue;
        },
        .Glst => {
          // glst rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          const list = vl.asList(self.RK(rk2, fp));
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(list.len);
          if (idx >= list.len) {
            return self.runtimeError("IndexError: list index out of range: {}", .{idx});
          }
          @setRuntimeSafety(false);
          fp.stack[rx] = list.items[@intCast(idx)];
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
          const idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          // assumed safe
          @setRuntimeSafety(false);
          tuple.items[@intCast(idx)] = self.RK(rk2, fp);
          continue;
        },
        .Gtup => {
          // gtup rx, rk(idx), rk(list)
          var rx: u32 = undefined;
          var rk1: u32 = undefined;
          var rk2: u32 = undefined;
          self.read3Args(code, &rx, &rk1, &rk2);
          const tuple = vl.asTuple(self.RK(rk2, fp));
          var idx = vl.asIntNumber(i64, self.RK(rk1, fp));
          if (idx < 0) idx += @intCast(tuple.len);
          if (idx >= tuple.len) {
            return self.runtimeError("IndexError: tuple index out of range: {}", .{idx});
          }
          @setRuntimeSafety(false);
          fp.stack[rx] = tuple.items[@intCast(idx)];
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
          @setRuntimeSafety(false);
          var map = vl.asMap(fp.stack[rx]);
          _ = map.meta.set(self.RK(rk1, fp), self.RK(rk2, fp), self);
          if (self.has_error) return error.RuntimeError;
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
          if (map.meta.get(key, self)) |v| {
            fp.stack[rx] = v;
            continue;
          }
          var kstr = vl.asString(vl.valueToString(key, self));
          return self.runtimeError("KeyError: map has no key: '{s}'", .{kstr.string()});
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
