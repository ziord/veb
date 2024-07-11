const std = @import("std");
const util = @import("util.zig");
const cli = @import("cli.zig");
const parse = @import("parse.zig");
const VebAllocator = @import("../src/allocator.zig");

const Allocator = std.mem.Allocator;
const print = util.print;

pub const std_options = .{.log_level = if (util.getMode() == .Debug) .debug else .info};

const menu =
" veb v" ++ util.getVersion() ++ "\n" ++
\\ 
\\ Usage: veb [command] [options]
\\
\\ Commands:
\\   new       Create a new project
\\   build     Compile and build a project
\\   run       Run a project or execute a veb file
\\   test      Run project tests
\\   repl      Launch a REPL environment
\\   check     Typecheck a project or veb file
\\ 
\\ Options:
\\   -h, --help        Display help message
\\   -v, --version     Display veb's version
\\
;

inline fn displayUsage() void {
  print(menu, .{});
}

inline fn displayVersion() void {
  print(util.getVersion() ++ "\n", .{});
}

fn displayErrorAndUsage(cmd: ?[]const u8) void {
  if (cmd) |_cmd| {
    print("Invalid command: '{s}'\n", .{_cmd});
  } else {
    print("Invalid command.\n", .{});
  }
  displayUsage();
}

fn cmdNew(name: []const u8, al: Allocator) void {
  cli.createNewProject(name, al) catch {
    cli.fatalError("Unable to create project '{s}'.", .{name});
  };
  print("Created project {s}.\n", .{name});
}

fn cmdBuild() void {
  // TODO
}

fn cmdCheck(file: ?[]const u8, al: Allocator, cna: *VebAllocator) !void {
  const lib_path = try cli.findLibPath(al);
  if (file) |filename| {
    cli.typecheck(filename, lib_path, null, cna) catch return;
  } else {
    const cwd = cli.getCWD(al);
    const filename = try cli.findMainPath(al, cwd);
    cli.typecheck(filename, lib_path, cwd, cna) catch return;
  }
  print("All Ok.\n", .{});
}

fn cmdRun(file: ?[]const u8, al: Allocator, cna: *VebAllocator) !void {
  const lib_path = try cli.findLibPath(al);
  if (file) |filename| {
    cli.run(filename, lib_path, null, cna) catch {};
  } else {
    const cwd = cli.getCWD(al);
    const filename = try cli.findMainPath(al, cwd);
    cli.run(filename, lib_path, cwd, cna) catch {};
  }
}

fn cmdTest(al: Allocator) void {
  // _ = lib_path;
  _ = al;
}

fn cmdRepl(al: Allocator) void {
  // _ = lib_path;
  _ = al;
}

pub fn main() !void {
  var cna = VebAllocator.init(std.heap.ArenaAllocator.init(std.heap.page_allocator));
  defer cna.deinit();
  const al = cna.getArenaAllocator();
  const args = try std.process.argsAlloc(al);
  if (args.len == 1) {
    displayUsage();
    return;
  }
  const arg = args[1];
  // handle options
  if (args.len == 2) {
    if (arg[0] == '-') {
      if (arg.len == 2) {
        if (arg[1] == 'h') {
          displayUsage();
          return;
        } else if (arg[1] == 'v') {
          displayVersion();
          return;
        }
      } else if (std.mem.eql(u8, "--help", arg)) {
        displayUsage();
        return;
      } else if (std.mem.eql(u8, "--version", arg)) {
        displayVersion();
        return;
      }
      cli.fatalError("invalid option: '{s}'", .{arg});
    }
  }
  if (args.len == 2) {
    if (std.mem.eql(u8, "test", arg)) {
      cmdTest(al);
      return;
    } else if (std.mem.eql(u8, "repl", arg)) {
      cmdRepl(al);
      return;
    } else if (std.mem.eql(u8, "run", arg)) {
      try cmdRun(null, al, &cna);
      return;
    } else if (std.mem.eql(u8, "check", arg)) {
      try cmdCheck(null, al, &cna);
      return;
    }
  } else if (args.len == 3) {
    if (std.mem.eql(u8, "new", arg)) {
      cmdNew(args[2], al);
      return;
    } else if (std.mem.eql(u8, "run", arg)) {
      try cmdRun(args[2], al, &cna);
      return;
    } else if (std.mem.eql(u8, "check", arg)) {
      try cmdCheck(args[2], al, &cna);
      return;
    }
  }
  displayErrorAndUsage(if (args.len > 3) args[3] else if (args.len > 2) args[2] else args[1]);
}
