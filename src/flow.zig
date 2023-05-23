const std = @import("std");
const types = @import("type.zig");
const ast = @import("ast.zig");
const util = @import("util.zig");

const Node = ast.AstNode;
const Type = types.Type;
pub const FlowList = std.ArrayList(*FlowNode);

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

pub const FlowNode = struct {
  /// Tag associated with this node
  tag: FlowTag,
  /// node in the graph node/vertex
  node: *Node,
  /// the edge type along which this node was found
  edge: FlowEdge = .ESequential,
  /// type on the graph edge
  typ: ?*Type = null,
  /// whether this node has been type checked successfully
  resolved: bool = false,
  /// predecessors or incoming edges to this node
  prev: FlowList,
  /// successors or outgoing edges from this node
  next: FlowList,

  pub fn init(tag: FlowTag, node: *Node, allocator: std.mem.Allocator) *@This() {
    var prev = FlowList.init(allocator);
    var next = FlowList.init(allocator);
    var self = util.alloc(FlowNode, allocator);
    self.* = @This() {
      .tag = tag, .node = node,
      .prev = prev, .next = next,
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
    util.append(*FlowNode, &list, self);
    return list;
  }

  fn outgoingNodes(node: *@This(), edge: FlowEdge, list: *FlowList) void {
    for (node.next.items) |nd| {
      if (nd.edge == edge) {
        util.append(*FlowNode, list, nd);
        outgoingNodes(nd, edge, list);
      }
    }
  }

  pub fn getOutgoingNodes(node: *@This(), edge: FlowEdge, allocator: std.mem.Allocator) FlowList {
    var nodes = FlowList.init(allocator);
    outgoingNodes(node, edge, &nodes);
    return nodes;
  }
};

pub const CFGBuilder = struct {
  root: *Node,
  entry: *FlowNode,
  exit: *FlowNode,
  dead: *FlowNode,
  allocator: std.mem.Allocator,
  nodes: std.AutoHashMap(*Node, *FlowNode),

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

  fn createConditionNode(self: *Self, cond: *Node, token: ast.Token) *Node {
    var node = util.alloc(Node, self.allocator);
    node.* = .{.AstCondition = ast.ConditionNode.init(cond, token)};
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
    for (prev.items) |p_node| {
      util.append(*FlowNode, &p_node.next, next);
      util.append(*FlowNode, &next.prev, p_node);
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
      ast.BlockNode.newEmptyBlock(node.token.line, self.allocator),
      node.token
    );
    var els: ?*Node = null;
    if (node.elifs.items.len > 0) {
      for (node.elifs.items, 0..) |elif, i| {
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
  
  fn linkBlock(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var block = &node.AstBlock;
    var prv = prev;
    for (block.nodes.items) |item| {
      prv = self.link(item, prv, edge);
    }
    return prv;
  }

  fn linkVarDecl(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var curr = self.getFlowNode(node, .CfgOther);
    self.connectVerticesWithEdgeInfo(prev, curr, edge);
    return curr.toList(self.allocator);
  }

  fn linkSimpleIf(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var node = &ast_node.AstSimpleIf;
    var cond = self.createConditionNode(node.cond, node.token);
    var flow = self.getFlowNode(cond, .CfgOther);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    var flow_list = flow.toList(self.allocator);
    var ret = FlowList.init(self.allocator);
    // cond->then.body, and to if-exit 
    var if_then = self.linkBlock(node.then, flow_list, .ETrue);
    util.extend(*FlowNode, &ret, &if_then);
    // cond->else, and to if-exit
    var els = self.link(node.els, flow_list, .EFalse);
    util.extend(*FlowNode, &ret, &els);
    return ret;
  }

  fn linkIf(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var tmp = self.simplifyIfNode(&ast_node.AstIf);
    var simple_if = util.alloc(Node, self.allocator);
    simple_if.* = .{.AstSimpleIf = tmp};
    return self.linkSimpleIf(simple_if, prev, edge);
  }

  fn linkProgram(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var node = &ast_node.AstProgram;
    var _prev = prev;
    for (node.decls.items) |item| {
      if (item.isTypeAlias()) continue;
      _prev = self.link(item, _prev, edge);
    }
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
      .AstProgram => self.linkProgram(node, prev, edge),
      else => unreachable,
    };
  }

  pub fn build(self: *Self) *FlowNode {
    _ = self.linkProgram(self.root, self.entry.toList(self.allocator), .ESequential);
    return self.entry;
  }
};
