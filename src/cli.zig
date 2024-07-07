pub const std = @import("std");
pub const parse = @import("parse.zig");
pub const check = @import("check.zig");
pub const util = @import("util.zig");
pub const ks = @import("constants.zig");
pub const prelude = @import("prelude.zig");
pub const value = @import("value.zig");
pub const compile = @import("compile.zig");
pub const debug = @import("debug.zig");
pub const vm = @import("vm.zig");
pub const diagnostics = @import("diagnostics.zig");
pub const VebAllocator = @import("allocator.zig");

const Allocator = std.mem.Allocator;
const print = util.print;
const sep_str = std.fs.path.sep_str;

pub fn fatalError(comptime fmt: []const u8, args: anytype) noreturn {
  print(fmt ++ "\n", args);
  std.posix.exit(1);
}

pub fn getCWD(al: Allocator) []const u8 {
  return std.process.getCwdAlloc(al) catch |e| {
    fatalError("could not obtain the current working directory: {}", .{e});
  };
}

pub fn dirExistsOrError(path: []const u8) void {
  _ = std.fs.accessAbsolute(path, .{}) 
    catch fatalError("Directory '{s}' does not exist", .{path});
}

pub fn findLibPath(al: Allocator) ![]const u8 {
  // find veb/lib
  var temp: ?[]const u8 = std.fs.path.dirname(try std.fs.selfExeDirPathAlloc(al));
  while (temp) |tmp| : (temp = std.fs.path.dirname(tmp)) {
    if (std.mem.endsWith(u8, tmp, "veb")) {
      const path = try std.fs.path.join(al, &[_][]const u8{tmp, ks.LibDir});
      dirExistsOrError(path);
      parse.Parser.lib_path = path;
      return path;
    }
  }
  fatalError("Unable to locate veb lib path.", .{});
}

pub fn findMainPath(al: Allocator, cwd: []const u8) ![]const u8 {
  // find main: cwd/src/main.veb
  errdefer print("Unable to locate main.veb", .{});
  const path = try std.fs.path.join(al, &[_][]const u8{cwd, ks.SrcDir, "main.veb"});
  try std.fs.accessAbsolute(path, .{});
  return path;
}

pub fn createNewProject(name: []const u8, al: Allocator) !void {
  // name
  //  |-src
  //  |--main.veb
  //  |-test
  //  |--test.veb
  var src_dir = try std.fs.cwd().makeOpenPath(
    try std.fs.path.join(al, &[_][]const u8{name, "src"}),
    .{}
  );
  defer src_dir.close();
  var main_file = try src_dir.createFile("main.veb", .{});
  defer main_file.close();
  try main_file.writeAll("println('hello world!')");

  var tst_dir = try std.fs.cwd().makeOpenPath(
    try std.fs.path.join(al, &[_][]const u8{name, "test"}),
    .{}
  );
  defer tst_dir.close();
  var tst_file = try tst_dir.createFile(
    try std.mem.concat(al, u8, &[_][]const u8{"test_", name, ".veb"}),
    .{}
  );
  defer tst_file.close();
  try tst_file.writeAll("assert(0xf00 == 3_840, 'should be the same')");
}

pub fn loadCore(path: []const u8, al: Allocator) !void {
  var dir = try std.fs.openDirAbsolute(path, .{});
  defer dir.close();
  var core_dir = try dir.openDir("core" ++ sep_str ++ ks.SrcDir, .{.iterate = true});
  var itr = core_dir.iterate();
  var size = @as(usize, 0);
  while (try itr.next()) |file| {
    const f = try core_dir.openFile(file.name, .{});
    size += try f.getEndPos();
  }
  var buf = util.allocSlice(u8, size + 1, al);
  var st = @as(usize, 0);
  itr = core_dir.iterate();
  while (try itr.next()) |file| {
    const content = try core_dir.readFile(file.name, buf[st..]);
    st += content.len;
  }
  buf[st] = 0;
  buf = buf[0..st:0];
  util.assert(st == size, "content of core should be equal to computed size");
  prelude.CoreSrc = buf;
  var parser = parse.Parser.initForCore(&buf, &ks.PreludeFilename, "", .Builtin, al);
  prelude.CoreNode = try parser.parse(true);
}

pub fn typecheck(filename: []const u8, lib_path: []const u8, cwd: ?[]const u8, cna: *VebAllocator) !void {
  const al = cna.getArenaAllocator();
  try loadCore(lib_path, al);
  var src = try util.readFile(filename, al);
  var ps = parse.Parser.init(&src, &filename, cwd orelse getCWD(al), .User, al);
  const ir = try ps.parse(true);
  var tych = check.TypeChecker.init(al, &ps.diag, ps.namegen);
  try tych.typecheck(ir, true);
}

pub fn run(filename: []const u8, lib_path: []const u8, cwd: ?[]const u8, cna: *VebAllocator) !void {
  const al = cna.getArenaAllocator();
  try loadCore(lib_path, al);
  var src = try util.readFile(filename, al);
  var ps = parse.Parser.init(&src, &filename, cwd orelse getCWD(al), .User, al);
  const ir = try ps.parse(true);
  var tych = check.TypeChecker.init(al, &ps.diag, ps.namegen);
  try tych.typecheck(ir, true);
  var cpu = vm.VM.init(cna);
  defer cpu.shutdown();
  var fun = value.createScriptFn(&cpu, 0);
  var compiler = compile.Compiler.init(tych.diag, &cpu, fun, &tych.generics, cna, tych.prelude, null, null);
  try compiler.compile(ir);
  if (util.inDebugMode()) debug.Disassembler.disCode(&fun.code, filename);
  cna.deinitArena();
  cpu.boot(fun);
  try cpu.run();
}
