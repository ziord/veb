const std = @import("std");
const types = @import("type.zig");
const ast = @import("ast.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");

const Node = ast.AstNode;
const Type = types.Type;
pub const FlowList = ds.ArrayList(*FlowNode);
const FlowDataList = ds.ArrayList(FlowData);

pub const FlowData = struct {
  prev: bool,
  next: bool,
  flo: *FlowNode,
};

pub const FlowTag = enum {
  CfgEntry,
  CfgExit,
  CfgOther,
  CfgDead,
};

pub const FlowEdge = enum {
  ETrue,
  EFalse,
  ESequential,
};

pub const ResolutionState = enum {
  Resolved,
  Processing,
  Unresolved,

  pub inline fn isResolved(res: @This()) bool {
    return res == .Resolved;
  }

  pub inline fn isUnresolved(res: @This()) bool {
    return res == .Unresolved;
  }
};

pub const FlowNode = struct {
  /// Tag associated with this node
  tag: FlowTag,
  /// node in the graph node/vertex
  node: *Node,
  /// the edge type along which this node was found
  edge: FlowEdge = .ESequential,
  /// whether this node has been type checked successfully
  res: ResolutionState = .Unresolved,
  /// predecessors (incoming edges to this node) and successors (outgoing edges from this node)
  prev_next: FlowDataList,

  pub fn init(tag: FlowTag, node: *Node, allocator: std.mem.Allocator) *@This() {
    var self = util.alloc(FlowNode, allocator);
    self.* = @This() {
      .tag = tag, .node = node,
      .prev_next = FlowDataList.init(allocator),
    };
    return self;
  }

  pub inline fn isEntryNode(self: *@This()) bool {
    return self.tag == .CfgEntry;
  }

  pub inline fn isExitNode(self: *@This()) bool {
    return self.tag == .CfgExit;
  }

  pub inline fn isDeadNode(self: *@This()) bool {
    return self.tag == .CfgDead;
  }

  pub fn toList(self: *@This(), allocator: std.mem.Allocator) FlowList {
    var list = FlowList.init(allocator);
    list.append(self);
    return list;
  }

  fn outgoingNodes(node: *@This(), visited: *std.AutoHashMap(*FlowNode, u32), edge: FlowEdge, list: *FlowList) void {
    if (visited.get(node)) |_| return;
    for (node.prev_next.items()) |nd| {
      if (nd.next and nd.flo.edge == edge) {
        list.append(nd.flo);
        visited.put(nd.flo, 0) catch {};
        outgoingNodes(nd.flo, visited, edge, list);
      }
    }
  }

  pub fn getOutgoingNodes(node: *@This(), edge: FlowEdge, allocator: std.mem.Allocator) FlowList {
    var nodes = FlowList.init(allocator);
    var visited = std.AutoHashMap(*FlowNode, u32).init(allocator);
    outgoingNodes(node, &visited, edge, &nodes);
    visited.clearAndFree();
    return nodes;
  }

  pub fn isNext(itm: FlowData) bool {
    return itm.next;
  }

  pub fn isPrev(itm: FlowData) bool {
    return itm.prev;
  }
};

pub const CFGBuilder = struct {
  allocator: std.mem.Allocator,
  nodes: std.AutoHashMap(*Node, *FlowNode),
  root: *Node,
  entry: *FlowNode,
  exit: *FlowNode,
  dead: *FlowNode,
  /// the condition node of the current while loop
  curr_while_cond: ?*FlowNode = null,
  /// the node after the current while loop
  after_while: ?*Node = null,

  const Self = @This();

  pub fn init(root: *Node, allocator: std.mem.Allocator) Self {
    var empty = createEmptyNode(allocator);
    return Self {
      .root = root,
      .allocator = allocator,
      .entry = FlowNode.init(.CfgEntry, empty, allocator),
      .exit = FlowNode.init(.CfgExit, empty, allocator),
      .dead = FlowNode.init(.CfgDead, empty, allocator),
      .nodes = std.AutoHashMap(*Node, *FlowNode).init(allocator),
    };
  }

  fn createEmptyNode(allocator: std.mem.Allocator) *Node {
    var node = util.alloc(Node, allocator);
    node.* = .{.AstEmpty = ast.EmptyNode.init(ast.Token.getDefault())};
    return node;
  }

  fn createConditionNode(self: *Self, cond: *Node) *Node {
    var node = util.alloc(Node, self.allocator);
    node.* = .{.AstCondition = ast.ConditionNode.init(cond)};
    return node;
  }

  fn getFlowNode(self: *Self, node: *Node, tag: FlowTag) *FlowNode {
    var flow = FlowNode.init(tag, node, self.allocator);
    self.nodes.put(node, flow) catch |e| {
      std.debug.print("Error - {}", .{e});
      std.debug.assert(false);
    };
    return flow;
  }

  fn connectVertices(self: *Self, prev: FlowList, next: *FlowNode) void {
    _ = self;
    for (prev.list.items) |p_node| {
      p_node.prev_next.append(.{.prev = false, .next = true, .flo = next});
      next.prev_next.append(.{.prev = true, .next = false, .flo = p_node});
    }
  }

  fn connectVerticesWithEdgeInfo(self: *Self, prev: FlowList, next: *FlowNode, edge: FlowEdge) void {
    self.connectVertices(prev, next);
    next.edge = edge;
  }

  fn simplifyIfNode(self: *Self, node: *ast.IfNode) ast.SimpleIfNode {
    // turn if .. elif .. else .. into
    // if .. else (.. if .. else)
    var ifn = ast.SimpleIfNode.init(
      node.cond, node.then,
      ast.BlockNode.newEmptyBlock(self.allocator)
    );
    var els: ?*Node = null;
    if (node.elifs.len() > 0) {
      for (node.elifs.items(), 0..) |elif, i| {
        var if_ = elif.AstElif.toIf(self.allocator);
        var nd = util.alloc(Node, self.allocator);
        nd.* = .{.AstSimpleIf = self.simplifyIfNode(&if_)};
        if (els) |_els| _els.AstSimpleIf.els = nd;
        if (i == 0) ifn.els = nd;
        els = nd;
      }
      // connect the last elif's else (elif is now simple-if) to this node's original else
      els.?.AstSimpleIf.els = node.els;
    } else {
      ifn.els = node.els;
    }
    return ifn;
  }

  fn linkExprStmt(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var flow = self.getFlowNode(node, .CfgOther);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    return flow.toList(self.allocator);
  }

  fn linkNodeList(self: *Self, nodes: *ast.AstNodeList, prev: FlowList, edge: FlowEdge) FlowList {
    var _prev = prev;
    for (nodes.items(), 0..) |item, i| {
      if (item.isTypeAlias()) {
        continue;
      } else if (item.isControl()) {
        _prev = self.linkControl(item, _prev, edge, i + 1 == nodes.len());
        continue;
      } else if (item.isWhile()) {
        if (i + 1 < nodes.len()) {
          self.after_while = nodes.items()[i + 1];
        }
      }
      _prev = self.link(item, _prev, edge);
    }
    return _prev;
  }
  
  fn linkBlock(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var block = &node.AstBlock;
    return self.linkNodeList(&block.nodes, prev, edge);
  }

  fn linkVarDecl(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var curr = self.getFlowNode(node, .CfgOther);
    self.connectVerticesWithEdgeInfo(prev, curr, edge);
    return curr.toList(self.allocator);
  }

  fn linkSimpleIf(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var node = &ast_node.AstSimpleIf;
    var cond = self.createConditionNode(node.cond);
    var flow = self.getFlowNode(cond, .CfgOther);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    var flow_list = flow.toList(self.allocator);
    var ret = FlowList.init(self.allocator);
    // cond->then.body, and to if-exit 
    var if_then = self.linkBlock(node.then, flow_list, .ETrue);
    ret.extend(&if_then);
    // cond->else, and to if-exit
    var els = self.link(node.els, flow_list, .EFalse);
    ret.extend(&els);
    return ret;
  }

  fn linkIf(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var tmp = self.simplifyIfNode(&ast_node.AstIf);
    var simple_if = util.alloc(Node, self.allocator);
    simple_if.* = .{.AstSimpleIf = tmp};
    return self.linkSimpleIf(simple_if, prev, edge);
  }

  fn linkWhile(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    // prev -> cond, cond -> then.body, last.then.body -> cond, cond -> while-exit
    // prev
    // while cond
    //   ... (then.body)
    // end
    // next (while-exit)
    var node = &ast_node.AstWhile;
    var curr_while_cond = self.curr_while_cond;
    var cond = self.createConditionNode(node.cond);
    var flow = self.getFlowNode(cond, .CfgOther);
    self.curr_while_cond = flow;
    // prev -> cond
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    var flow_list = flow.toList(self.allocator);
    // cond -> then.body
    var then = self.linkBlock(node.then, flow_list, .ETrue);
    // last.then.body -> cond
    if (node.then.AstBlock.getLast()) |last| {
      for (then.items()) |item| {
        if (item.node == last) {
          self.connectVerticesWithEdgeInfo(item.toList(self.allocator), flow, .ESequential);
          item.res = .Processing;
        }
      }
    }
    // naturally handles cond -> while-exit
    flow_list.extend(&then);
    self.curr_while_cond = curr_while_cond;
    return flow_list;
  }

  fn linkControl(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge, is_last: bool) FlowList {
    // continue -> loop-cond, break -> loop-exit
    var flow = self.getFlowNode(node, .CfgOther);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    var flow_list = flow.toList(self.allocator);
    std.debug.assert(self.curr_while_cond != null);
    var ctrl = &node.AstControl;
    if (ctrl.isContinue()) {
      self.connectVerticesWithEdgeInfo(flow_list, self.curr_while_cond.?, edge);
      if (!is_last) {
        return self.dead.toList(self.allocator);
      } else {
        return FlowList.init(self.allocator);
      }
    } else { // break
      if (self.after_while) |after| {
        _ = self.link(after, flow_list, edge);
      }
      // if the last node in the loop body is a break statement, we want the outgoing
      // cfg node (from this node) to not be dead - since a break statement at the end
      // of a loop body is essentially normal control flow. - similar to continue (see if part)
      if (!is_last) {
        return self.dead.toList(self.allocator);
      } else {
        return flow_list;
      }
    }
  }

  fn linkProgram(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var node = &ast_node.AstProgram;
    var _prev = self.linkNodeList(&node.decls, prev, edge);
    self.connectVertices(_prev, self.exit);
    return undefined;
  }

  fn link(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    return switch (node.*) {
      .AstExprStmt => self.linkExprStmt(node, prev, edge),
      .AstVarDecl => self.linkVarDecl(node, prev, edge),
      .AstBlock => self.linkBlock(node, prev, edge),
      .AstIf => self.linkIf(node, prev, edge),
      .AstSimpleIf => self.linkSimpleIf(node, prev, edge),
      .AstWhile => self.linkWhile(node, prev, edge),
      .AstControl => self.linkControl(node, prev, edge, false),
      .AstProgram => self.linkProgram(node, prev, edge),
      else => unreachable,
    };
  }

  pub fn build(self: *Self) *FlowNode {
    _ = self.linkProgram(self.root, self.entry.toList(self.allocator), .ESequential);
    return self.entry;
  }
};
