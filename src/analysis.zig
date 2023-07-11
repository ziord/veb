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

  fn hasAtLeastOneIncomingEdgeWithTypeNotNoreturn(self: *Self, flo: *FlowNode) bool {
    // check if this flow node has at least one incoming/prev edge with type that isn't noreturn
    _ = self;
    for (flo.prev_next.items()) |fd| {
      if (fd.prev) {
        if (fd.flo.node.getType()) |typ| {
          if (!typ.isNoreturnTy()) return true;
        } else {
          return true;
        }
      }
    }
    return false;
  }

  fn checkDeadCodeWithTypes(self: *Self, nodes: *FlowList, start: usize) !void {
    for (nodes.items()) |itm| {
      if (itm.node.isEmpty()) continue;
      if (itm.node.getType()) |typ| {
        if (typ.isNoreturnTy() and !itm.node.isFun()) { // skip func decls
          for (itm.prev_next.items()) |fd| {
            if (fd.next and !fd.flo.isExitNode() and !self.hasAtLeastOneIncomingEdgeWithTypeNotNoreturn(fd.flo)) {
              self.diag.addDiagnostics(
                .DiagError,
                fd.flo.node.getToken(),
                "Dead code: control flow never reaches this code", .{}
              );
              break;
            }
          }
        }
      }
      if (itm.node.isCondition()) {
        var tmp = itm.getOutgoingNodes(.ETrue, self.diag.data.allocator);
        try self.checkDeadCodeWithTypes(&tmp, start);
        tmp = itm.getOutgoingNodes(.EFalse, self.diag.data.allocator);
        try self.checkDeadCodeWithTypes(&tmp, start);
      }
    }
    if (self.diag.count() > start) {
      return error.DeadCode;
    }
  }

  pub fn analyzeDeadCodeWithTypes(self: *Self, node: *FlowNode) !void {
    var nodes = node.getOutgoingNodes(.ESequential, self.diag.data.allocator);
    return try self.checkDeadCodeWithTypes(&nodes, self.diag.count());
  }
};
