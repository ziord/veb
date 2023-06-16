const std = @import("std");
const flow = @import("flow.zig");
const diagnostics = @import("diagnostics.zig");

const FlowNode = flow.FlowNode;
const FlowList = flow.FlowList;
const FlowMeta = flow.FlowMeta;
const CFG = flow.CFG;
const Diagnostic = diagnostics.Diagnostic;

pub const Analysis = struct {
  diag: *Diagnostic,

  const Self = @This();

  pub fn init(diag: *Diagnostic) Self {
    return Self {.diag = diag};
  }

  /// node is .CfgDead
  pub fn analyzeDeadCode(self: *Self, node: *FlowNode) !void {
    std.debug.assert(node.isDeadNode());
    var start = self.diag.count();
    for (node.prev_next.items()) |itm| {
      if (itm.next and !itm.flo.isExitNode()) {
        self.diag.addDiagnostics(
          .DiagError,
          itm.flo.node.getToken(),
          "Dead code: control flow never reaches this code", .{}
        );
      }
    }
    if (self.diag.count() > start) {
      return error.DeadCode;
    }
  }
};
