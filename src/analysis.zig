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
        // last item may be an exit Scope node, which doesn't count as dead code
        if (itm.flo.bb.len() == 1) {
          if (itm.flo.bb.getLast()) |lst| {
            if (lst.isExitScope()) continue;
          }
        }
        self.diag.addDiagnosticsWithLevel(
          .DiagError,
          itm.flo.bb.nodes.itemAt(0).getToken(),
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
        if (fd.flo.bb.getLast()) |node| {
          if (node.getType()) |typ| {
            if (!typ.isNoreturnTy()) return true;
          } else {
            return true;
          }
        }
      }
    }
    return false;
  }

  fn checkDeadCodeWithTypes(self: *Self, flo_nodes: *FlowList, start: usize) !void {
    for (flo_nodes.items()) |flo_node| {
      for (flo_node.bb.items(), 0..) |node, i| {
        if (node.isEmpty()) continue;
        if (node.getType()) |typ| {
          if (typ.isNoreturnTy() and !node.isFun()) { // skip func decls
            if (node != flo_node.bb.getLast().?) {
              var next = flo_node.bb.nodes.itemAt(i + 1);
              self.diag.addDiagnosticsWithLevel(
                .DiagError,
                next.getToken(),
                "Dead code: control flow never reaches this code", .{}
              );
            } else {
              for (flo_node.prev_next.items()) |fln| {
                // hasAtLeastOneIncomingEdgeWithTypeNotNoreturn() <- ensure the code is not reachable from other parts
                if (fln.next and !fln.flo.isExitNode() and !self.hasAtLeastOneIncomingEdgeWithTypeNotNoreturn(fln.flo)) {
                  self.diag.addDiagnosticsWithLevel(
                    .DiagError,
                    fln.flo.bb.nodes.itemAt(0).getToken(),
                    "Dead code: control flow never reaches this code", .{}
                  );
                  break;
                }
              }
            }
          }
        }
        if (node.isCondition()) {
          var tmp = flo_node.getOutgoingNodes(.ETrue, self.diag.data.allocator);
          try self.checkDeadCodeWithTypes(&tmp, start);
          tmp = flo_node.getOutgoingNodes(.EFalse, self.diag.data.allocator);
          try self.checkDeadCodeWithTypes(&tmp, start);
        }
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
