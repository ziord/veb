const std = @import("std");
const ds = @import("ds.zig");
const v = @import("value.zig");
const util = @import("util.zig");
const VM = @import("vm.zig").VM;
const VebAllocator = @import("allocator.zig");
const logger = std.log.scoped(.gc);

const Allocator = std.mem.Allocator;
const Obj = v.Obj;

vm: *VM,
skip_collection: bool = false,
bytes_allocated: usize,
heap_threshold: usize,
gray_stack: ds.ArrayListUnmanaged(*Obj),
allocator: Allocator,

const HEAP_GROWTH_FACTOR = 0x1;
const GC_INIT_THRESHOLD = 0x400;
const Self = @This();

pub fn init(allocator: *VebAllocator, vm: *VM) Self {
  return Self {
    .vm = vm,
    .gray_stack = ds.ArrayListUnmanaged(*Obj).init(),
    .bytes_allocated = 0,
    .heap_threshold = GC_INIT_THRESHOLD,
    .allocator = allocator.getAllocator(),
  };
}

pub fn deinit(self: *Self) void {
  self.gray_stack.deinit(self.allocator);
}

pub fn freeObject(self: *Self, obj: *Obj, vm: *VM) void {
  _ = self;
  logger.debug("freeing object type {}", .{obj.id});
  switch (obj.id) {
    .objstring => {
      const T = v.ObjString;
      var string: *T = @ptrCast(obj);
      vm.mem.freeBuf(u8, vm, string.str[0..string.len]);
      vm.mem.free(T, vm, string);
    },
    .objlist => {
      const T = v.ObjList;
      var list: *T = @ptrCast(obj);
      vm.mem.freeBuf(v.Value, vm, list.items[0..list.capacity]);
      vm.mem.free(T, vm, list);
    },
    .objtuple => {
      const T = v.ObjTuple;
      var tuple: *T = @ptrCast(obj);
      vm.mem.freeBuf(v.Value, vm, tuple.items[0..tuple.len]);
      vm.mem.free(T, vm, tuple);
    },
    .objvalmap => {
      const T = v.ObjMap;
      var map: *T = @ptrCast(obj);
      map.meta.free(vm);
      vm.mem.free(T, vm, map);
    },
    .objfn => {
      const T = v.ObjFn;
      var fun: *T = @ptrCast(obj);
      fun.code.deinit(vm);
      vm.mem.free(T, vm, fun);
    },
    .objnativefn => {
      vm.mem.free(v.ObjNativeFn, vm, @as(*v.ObjNativeFn, @ptrCast(obj)));
    },
    .objclosure => {
      const T = v.ObjClosure;
      var clo: *T = @ptrCast(obj);
      vm.mem.freeBuf(*v.ObjUpvalue, vm, clo.env[0..clo.fun.envlen]);
      vm.mem.free(T, vm, clo);
    },
    .objerror => {
      vm.mem.free(v.ObjError, vm, @as(*v.ObjError, @ptrCast(obj)));
    },
    .objupvalue => {
      vm.mem.free(v.ObjUpvalue, vm, @as(*v.ObjUpvalue, @ptrCast(obj)));
    },
    .objfiber => {
      const T = v.ObjFiber;
      var fiber: *T = @ptrCast(obj);
      vm.mem.freeBuf(v.CallFrame, vm, fiber.frames[0..fiber.frame_cap]);
      vm.mem.freeBuf(v.Value, vm, fiber.stack[0..fiber.stack_cap]);
      vm.mem.free(T, vm, fiber);
    },
    .objclass, .objstruct, .objmodule => {
      const T = v.ObjClass;
      var cls: *T = @ptrCast(obj);
      vm.mem.freeBuf(v.Value, vm, cls.items[0..cls.len]);
      vm.mem.free(T, vm, cls);
    },
    .objtag => {
      const T = v.ObjTag;
      vm.mem.free(T, vm, @as(*T, @ptrCast(obj)));
    },
    .objinstance => {
      const T = v.ObjInstance;
      var inst: *T = @ptrCast(obj);
      vm.mem.freeBuf(v.Value, vm, inst.fields[0..inst.flen]);
      vm.mem.free(T, vm, inst);
    },
    .objmethod => {
      vm.mem.free(v.ObjMethod, vm, @as(*v.ObjMethod, @ptrCast(obj)));
    },
  }
}

fn markObject(self: *Self, obj: *Obj) void {
  if (obj.marked) return;
  obj.marked = true;
  self.gray_stack.append(obj, self.allocator);
  if (util.inDebugMode) {
    logger.debug(" [*] mark object {*} type {}", .{obj, obj.id});
    v.printValue(v.objVal(obj));
    std.debug.print("\n", .{});
  }
}

fn markValue(self: *Self, value: v.Value) void {
  if (v.isObj(value)) {
    self.markObject(v.asObj(value));
  }
}

fn markFiber(self: *Self, fiber: *v.ObjFiber) void {
  // (call frame, stack, next, upvalue etc.)
  self.markObject(&fiber.obj);
  self.markValue(fiber.errval);
  for (fiber.frames[0..fiber.frame_len]) |frame| {
    for (frame.stack[0..v.MAX_REGISTERS]) |value| {
      if (!v.isNothing(value)) {
        self.markValue(value);
      } else {
        break;
      }
    }
    self.markObject(&frame.closure.obj);
  }
  var upvalue: ?*v.ObjUpvalue = fiber.open_upvalues;
  while (upvalue) |upv| {
    self.markObject(&upv.obj);
    upvalue = upv.next;
  }
  if (fiber.caller) |fb| {
    self.markFiber(fb);
  }
}

fn markStringMap(self: *Self, map: *v.StringHashMap) void {
  for (map.items[0..map.len]) |itm| {
    self.markObject(&itm.key.obj);
    self.markValue(itm.value);
  }
}

fn markValueMap(self: *Self, map: *v.ValueHashMap) void {
  for (map.items[0..map.len]) |itm| {
    self.markValue(itm.key);
    self.markValue(itm.value);
  }
}

fn markCachedNames(self: *Self) void {
  self.markObject(&self.vm.names.init.obj);
  self.markObject(&self.vm.names.ok.obj);
  self.markObject(&self.vm.names.err.obj);
  self.markObject(&self.vm.names.just.obj);
}

fn markRoots(self: *Self) void {
  // mark temporary roots
  for (self.vm.temp_roots.getItems()) |val| {
    self.markObject(v.asObj(val));
  }
  // mark externs
  for (self.vm.externs.getItems()) |val| {
    self.markValue(val);
  }
  // mark fiber roots
  self.markFiber(self.vm.fiber);
  // mark tag roots
  self.markStringMap(&self.vm.tags);
  // mark globals (usually builtins)
  self.markStringMap(&self.vm.globals);
  // mark cached name roots
  self.markCachedNames();
  // mark argv
  self.markValue(self.vm.argv);
}

fn blackenObject(self: *Self, obj: *Obj) void {
  switch (obj.id) {
    .objlist => {
      const list: *v.ObjList = @ptrCast(obj);
      for (list.items[0..list.len]) |val| {
        self.markValue(val);
      }
    },
    .objtuple => {
      const tuple: *v.ObjTuple = @ptrCast(obj);
      for (tuple.items[0..tuple.len]) |val| {
        self.markValue(val);
      }
    },
    .objclass, .objstruct, .objmodule => {
      const cls: *v.ObjClass = @ptrCast(obj);
      for (cls.items[0..cls.len]) |val| {
        self.markValue(val);
      }
      self.markObject(&cls.name.obj);
    },
    .objinstance => {
      const inst: *v.ObjInstance = @ptrCast(obj);
      for (inst.fields[0..inst.flen]) |val| {
        self.markValue(val);
      }
    },
    .objvalmap => {
      self.markValueMap(&@as(*v.ObjMap, @ptrCast(obj)).meta);
    },
    .objfn => {
      const fun: *v.ObjFn = @ptrCast(obj);
      self.markObject(&fun.name.obj);
      self.markObject(&fun.module.obj);
      for (fun.code.values.getItems()) |val| {
        self.markValue(val);
      }
    },
    .objclosure => {
      const clos: *v.ObjClosure = @ptrCast(obj);
      self.markObject(&clos.fun.obj);
      for (clos.env[0..clos.fun.envlen]) |upv| {
        self.markObject(&upv.obj);
      }
    },
    .objupvalue => {
      self.markValue(@as(*v.ObjUpvalue, @ptrCast(obj)).value);
    },
    .objtag => {
      self.markObject(&@as(*v.ObjTag, @ptrCast(obj)).name.obj);
    },
    .objnativefn => {
      self.markObject(&@as(*v.ObjNativeFn, @ptrCast(obj)).name.obj);
    },
    .objfiber => {
      self.markFiber(@ptrCast(obj));
    },
    .objerror => {
      self.markValue(@as(*v.ObjError, @ptrCast(obj)).val);
    },
    .objmethod => {
      const mth: *v.ObjMethod = @ptrCast(obj);
      if (mth.isBoundUserMethod()) {
        self.markValue(mth.as.user.instance);
        self.markObject(&mth.as.user.closure.obj);
      } else {
        self.markValue(mth.as.native.instance);
        self.markObject(&mth.as.native.fun.obj);
      }
    },
    .objstring => {},
  }
}

fn removeWhites(self: *Self) void {
  var map = &self.vm.strings;
  for (map.items[0..map.len]) |itm| {
    if (!itm.key.obj.marked) {
      std.debug.assert(map.remove(itm.key, self.vm));
      if (util.inDebugMode) {
        const obj = itm.key.obj;
        logger.debug(" [*] removing map weakref {*} type {} val ({s})", .{&obj, obj.id, itm.key.string()});
      }
    }
  }
}

fn traceReferences(self: *Self) void {
  while (self.gray_stack.isNotEmpty()) {
    self.blackenObject(self.gray_stack.pop());
  }
}

fn sweep(self: *Self) void {
  logger.debug(" [*] begin sweep", .{});
  var prev: ?*Obj = null; 
  var curr = self.vm.objects;
  while (curr) |c| {
    if (c.marked) {
      c.marked = false;
      prev = curr;
      curr = c.next;
    } else {
      const garbage = c;
      curr = c.next;
      if (prev) |p| {
        p.next = curr;
      } else {
        self.vm.objects = curr;
      }
      self.freeObject(garbage, self.vm);
    }
  }
  logger.debug(" [*] end sweep", .{});
}

pub inline fn canCollect(self: *Self) bool {
  return self.bytes_allocated > self.heap_threshold;
}

pub fn collect(self: *Self) void {
  if (self.skip_collection) return;
  const bytes_alloc = self.bytes_allocated;
  self.markRoots();
  self.traceReferences();
  self.removeWhites();
  self.sweep();
  const bytes_freed = bytes_alloc - self.bytes_allocated;
  self.heap_threshold = self.bytes_allocated << HEAP_GROWTH_FACTOR;
  logger.debug(
    " [*] collected {} bytes (from {} to {}), next collection at {}",
    .{bytes_freed, bytes_alloc, self.bytes_allocated, self.heap_threshold}
  );
}

pub inline fn forceCollect(self: *Self, likely: bool) void {
  if (likely) {
    self.collect(self.vm);
  }
}
