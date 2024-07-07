const std = @import("std");
const fir = @import("fir.zig");
const diagnostics = @import("diagnostics.zig");

const FlowNode = fir.FlowNode;
const FlowList = fir.FlowList;
const FlowGraph = fir.FlowGraph;
const CompUnit = fir.CompUnit;
const Diagnostic = diagnostics.Diagnostic;

pub const Analysis = struct {
  diag: *Diagnostic,

  const Self = @This();

  pub fn init(diag: *Diagnostic) Self {
    return Self {.diag = diag};
  }

  pub fn analyzeDeadCode(self: *Self, graph: *FlowGraph) !void {
    const start = self.diag.count();
    for (graph.dead().getNextNeighbours().items()) |itm| {
      if (!itm.node.isExitNode()) {
        // last item may be an exit Scope node, which doesn't count as dead code
        var bb = &itm.node.get().bb;
        if (bb.getLast()) |lst| {
          if (lst.isScope()) {
            continue;
          }
        }
        self.diag.addDiagnosticsWithLevel(
          .DiagError,
          bb.nodes.itemAt(0).getToken(),
          "Dead code: control flow never reaches this code", .{}
        );
      }
    }
    if (self.diag.count() > start) {
      return error.DeadCode;
    }
  }

  fn hasAtLeastOneIncomingEdgeWithTypeNotNoreturn(self: *Self, flo: FlowNode) bool {
    _ = self;
    // check if this flow node has at least one incoming/prev edge with type that isn't noreturn
    for (flo.getPrevNeighbours().items()) |ngh| {
      if (ngh.node.get().bb.getLast()) |lst| {
        if (lst.getTypeE()) |typ| {
          if (!typ.isNoreturnTy()) return true;
        } else {
          return true;
        }
      }
    }
    return false;
  }

  fn checkDeadCodeWithTypes(self: *Self, flo_nodes: *FlowList, start: usize) !void {
    for (flo_nodes.items()) |flo_node| {
      var dt = flo_node.get();
      for (dt.bb.items(), 0..) |node, i| {
        if (node.isEmpty()) continue;
        if (node.getTypeE()) |typ| {
          if (typ.isNoreturnTy() and !node.isFun()) { // skip func decls
            if (node != dt.bb.getLast().?) {
              var next = dt.bb.nodes.itemAt(i + 1);
              self.diag.addDiagnosticsWithLevel(
                .DiagError,
                next.getToken(),
                "Dead code: control flow never reaches this code", .{}
              );
            } else {
              for (flo_node.getNextNeighbours().items()) |fln| {
                // hasAtLeastOneIncomingEdgeWithTypeNotNoreturn() <- ensure the code is not reachable from other parts
                if (!fln.node.isExitNode() and !self.hasAtLeastOneIncomingEdgeWithTypeNotNoreturn(fln.node)) {
                  self.diag.addDiagnosticsWithLevel(
                    .DiagError,
                    fln.node.get().bb.nodes.itemAt(0).getToken(),
                    "Dead code: control flow never reaches this code", .{}
                  );
                  break;
                }
              }
            }
          }
        }
        if (node.isCondition()) {
          var tmp = flo_node.getOutgoingNodes(.ETrue, self.diag.data.allocator());
          try self.checkDeadCodeWithTypes(&tmp, start);
          tmp = flo_node.getOutgoingNodes(.EFalse, self.diag.data.allocator());
          try self.checkDeadCodeWithTypes(&tmp, start);
        }
      }
    }
    if (self.diag.count() > start) {
      return error.DeadCode;
    }
  }

  pub fn analyzeDeadCodeWithTypes(self: *Self, node: FlowNode) !void {
    var nodes = node.getOutgoingNodes(.ESequential, self.diag.data.allocator());
    return try self.checkDeadCodeWithTypes(&nodes, self.diag.count());
  }
};
