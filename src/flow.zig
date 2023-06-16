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

pub const FlowMeta = struct {
  entry: *FlowNode,
  exit: *FlowNode,
  dead: *FlowNode,

  pub fn init(entry: *FlowNode, exit: *FlowNode, dead: *FlowNode) @This() {
    return @This() {.entry = entry, .exit = exit, .dead = dead};
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

  fn outgoingNodes(node: *@This(), visited: *std.AutoArrayHashMap(*FlowNode, u32), edge: FlowEdge) void {
    if (visited.get(node)) |_| return;
    for (node.prev_next.items()) |nd| {
      if (nd.next and nd.flo.edge == edge) {
        visited.put(nd.flo, 0) catch {};
        outgoingNodes(nd.flo, visited, edge);
      }
    }
  }

  pub fn getOutgoingNodes(node: *@This(), edge: FlowEdge, allocator: std.mem.Allocator) FlowList {
    var nodes = FlowList.init(allocator);
    var visited = std.AutoArrayHashMap(*FlowNode, u32).init(allocator);
    outgoingNodes(node, &visited, edge);
    nodes.list.items = visited.keys();
    return nodes;
  }

  pub fn isNext(itm: FlowData) bool {
    return itm.next;
  }

  pub fn isPrev(itm: FlowData) bool {
    return itm.prev;
  }
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

pub const CFG = struct {
  /// named functions
  funcs: ds.ArrayList(FlowMeta),
  /// whole program
  program: FlowMeta,

  pub fn init(al: std.mem.Allocator) @This() {
    return @This() {
      .funcs = ds.ArrayList(FlowMeta).init(al),
      .program = undefined,
    };
  }

  pub fn putFunc(self: *@This(), info: FlowMeta) void {
    self.funcs.append(info);
  }

  pub fn lookup(self: *@This(), node: *Node) FlowMeta {
    for (self.funcs.items()) |itm| {
      if (itm.entry.node == node) {
        return itm;
      }
    }
    unreachable;
  }
};

pub const CFGBuilder = struct {
  nodes: std.AutoHashMap(*Node, *FlowNode),
  entry: *FlowNode,
  exit: *FlowNode,
  dead: *FlowNode,
  cfg: *CFG = undefined,
  /// the condition node of the current while loop
  curr_while_cond: ?*FlowNode = null,
  /// the node after the current while loop
  after_while: ?*Node = null,

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator) Self {
    var empty = createEmptyNode(allocator);
    return Self {
      .entry = FlowNode.init(.CfgEntry, empty, allocator),
      .exit = FlowNode.init(.CfgExit, empty, allocator),
      .dead = FlowNode.init(.CfgDead, empty, allocator),
      .nodes = std.AutoHashMap(*Node, *FlowNode).init(allocator),
    };
  }

  inline fn alloc(self: *Self) std.mem.Allocator {
    return self.nodes.allocator;
  }

  fn createEmptyNode(allocator: std.mem.Allocator) *Node {
    var node = util.alloc(Node, allocator);
    node.* = .{.AstEmpty = ast.EmptyNode.init(ast.Token.getDefault())};
    return node;
  }

  fn createConditionNode(self: *Self, cond: *Node) *Node {
    var node = util.alloc(Node, self.alloc());
    node.* = .{.AstCondition = ast.ConditionNode.init(cond)};
    return node;
  }

  fn getFlowNode(self: *Self, node: *Node, tag: FlowTag) *FlowNode {
    var flow = FlowNode.init(tag, node, self.alloc());
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
      ast.BlockNode.newEmptyBlock(self.alloc(), node.cond)
    );
    var els: ?*Node = null;
    if (node.elifs.len() > 0) {
      for (node.elifs.items(), 0..) |elif, i| {
        var if_ = elif.AstElif.toIf(self.alloc());
        var nd = util.alloc(Node, self.alloc());
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

  fn hasReturnNode(self: *Self, nodes: *FlowList) bool {
    _ = self;
    for (nodes.items()) |itm| {
      if (itm.node.isRet()) {
        return true;
      }
    }
    return false;
  }

  fn linkAtomic(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge, tag: FlowTag) FlowList {
    var flow = self.getFlowNode(node, tag);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    return flow.toList(self.alloc());
  }

  fn linkNodeList(self: *Self, nodes: *ast.AstNodeList, cond: ?*Node, prev: FlowList, edge: FlowEdge) FlowList {
    // cond indicates whether this block is from a branching entry or a normal do..end block
    var _prev = prev;
    for (nodes.items(), 0..) |item, i| {
      if (item.isTypeAlias()) {
        continue;
      } else if (item.isControl()) {
        var is_last = if (cond != null) i + 1 == nodes.len() else false;
        _prev = self.linkControl(item, _prev, edge, is_last);
        continue;
      } else if (item.isRet()) {
        var is_last = if (cond != null) i + 1 == nodes.len() else false;
        _prev = self.linkRet(item, _prev, edge, is_last);
        continue;
      } else if (item.isWhile()) {
        if (i + 1 < nodes.len()) {
          self.after_while = nodes.itemAt(i + 1);
        }
      }
      _prev = self.link(item, _prev, edge);
    }
    return _prev;
  }
  
  fn linkBlock(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var block = &node.AstBlock;
    return self.linkNodeList(&block.nodes, block.cond, prev, edge);
  }

  fn linkSimpleIf(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var node = &ast_node.AstSimpleIf;
    var cond = self.createConditionNode(node.cond);
    var flow_list = self.linkAtomic(cond, prev, edge, .CfgOther);
    var ret = FlowList.init(self.alloc());
    // cond->then.body, and to if-exit 
    var if_then = self.linkBlock(node.then, flow_list, .ETrue);
    ret.extend(&if_then);
    // cond->else, and to if-exit
    var els = self.link(node.els, flow_list, .EFalse);
    ret.extend(&els);
    // if we return from both branches of the if condition, then whatever follows the if is dead
    if (self.hasReturnNode(&if_then) and self.hasReturnNode(&els)) {
      ret.append(self.dead);
    }
    return ret;
  }

  fn linkIf(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var tmp = self.simplifyIfNode(&ast_node.AstIf);
    var simple_if = util.alloc(Node, self.alloc());
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
    var flow_list = flow.toList(self.alloc());
    // cond -> then.body
    var then = self.linkBlock(node.then, flow_list, .ETrue);
    // last.then.body -> cond
    if (node.then.AstBlock.getLast()) |last| {
      // return node should not link back to condition
      if (!last.isRet()) {
        for (then.items()) |item| {
          if (item.node == last) {
            self.connectVerticesWithEdgeInfo(item.toList(self.alloc()), flow, .ESequential);
            item.res = .Processing;
          }
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
    var flow_list = self.linkAtomic(node, prev, edge, .CfgOther);
    std.debug.assert(self.curr_while_cond != null);
    var ctrl = &node.AstControl;
    if (ctrl.isContinue()) {
      self.connectVerticesWithEdgeInfo(flow_list, self.curr_while_cond.?, edge);
      if (!is_last) {
        return self.dead.toList(self.alloc());
      } else {
        return FlowList.init(self.alloc());
      }
    } else { // break
      if (self.after_while) |after| {
        _ = self.link(after, flow_list, edge);
      }
      // if the last node in the loop body is a break statement, we want the outgoing
      // cfg node (from this node) to not be dead - since a break statement at the end
      // of a loop body is essentially normal control flow. - similar to continue (see if part)
      if (!is_last) {
        flow_list.append(self.dead);
      }
      return flow_list;
    }
  }

  fn linkRet(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge, is_last: bool) FlowList {
    var flow_list = self.linkAtomic(node, prev, edge, .CfgOther);
    self.connectVerticesWithEdgeInfo(flow_list, self.exit, edge);
    if (!is_last) {
      return self.dead.toList(self.alloc());
    }
    return FlowList.init(self.alloc());
  }

  fn linkFun(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    // this is just a placeholder node to complete the link
    // between functions and non-functions in the flow graph
    var flow_list = self.linkAtomic(node, prev, edge, .CfgOther);
    // skip generic functions
    if (node.AstFun.isGeneric()) return flow_list;
    // build the cfg of fun
    var fun = &node.AstFun;
    // TODO: cache builder
    var builder = CFGBuilder.init(self.alloc());
    var synth = ast.BlockNode.init(self.alloc(), fun.body.AstBlock.cond);
    synth.nodes.ensureTotalCapacity(fun.params.len() + fun.body.AstBlock.nodes.len());
    for (fun.params.items()) |param| {
      var tmp = util.alloc(Node, self.alloc());
      tmp.* = @as(Node, .{.AstVarDecl = param});
      synth.nodes.append(tmp);
    }
    synth.nodes.extend(&fun.body.AstBlock.nodes);
    var body = @as(Node, .{.AstBlock = synth});
    var flo = builder.buildBlock(self.cfg, &body);
    // save node for future lookup()s
    flo.entry.node = node;
    self.cfg.putFunc(flo);
    return flow_list;
  }

  fn linkProgram(self: *Self, ast_node: *Node, prev: FlowList, edge: FlowEdge) void {
    var node = &ast_node.AstProgram;
    var _prev = self.linkNodeList(&node.decls, null, prev, edge);
    self.connectVertices(_prev, self.exit);
    self.cfg.program = FlowMeta.init(self.entry, self.exit, self.dead);
  }

  fn link(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    return switch (node.*) {
      .AstExprStmt => self.linkAtomic(node, prev, edge, .CfgOther),
      .AstVarDecl => self.linkAtomic(node, prev, edge, .CfgOther),
      .AstCall => self.linkAtomic(node, prev, edge, .CfgOther),
      .AstBlock => self.linkBlock(node, prev, edge),
      .AstIf => self.linkIf(node, prev, edge),
      .AstSimpleIf => self.linkSimpleIf(node, prev, edge),
      .AstWhile => self.linkWhile(node, prev, edge),
      .AstControl => self.linkControl(node, prev, edge, false),
      .AstFun => self.linkFun(node, prev, edge),
      else => unreachable,
    };
  }

  pub fn buildFun(self: *Self, cfg: *CFG, node: *Node) void {
    self.cfg = cfg;
    _ = self.linkFun(node, self.entry.toList(self.alloc()), .ESequential);
  }

  pub fn buildBlock(self: *Self, cfg: *CFG, ast_node: *Node) FlowMeta {
    self.cfg = cfg;
    var node = &ast_node.AstBlock;
    var _prev = self.linkNodeList(&node.nodes, node.cond, self.entry.toList(self.alloc()), .ESequential);
    self.connectVertices(_prev, self.exit);
    return FlowMeta.init(self.entry, self.exit, self.dead);
  }

  pub fn build(self: *Self, root: *Node) CFG {
    var cfg = CFG.init(self.alloc());
    self.cfg = &cfg;
    _ = self.linkProgram(root, self.entry.toList(self.alloc()), .ESequential);
    self.cfg = undefined;
    return cfg;
  }
};
