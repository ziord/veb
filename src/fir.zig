const std = @import("std");
const util = @import("util.zig");
const ds = @import("ds.zig");
pub const tir = @import("tir.zig");

const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Node = tir.Node;
const NodeListU = tir.NodeListU;
pub const VertexList = ds.ArrayListUnmanaged(FlowData);
pub const FlowList = ds.ArrayListUnmanaged(FlowNode);
pub const NeighbourList = ds.ArrayListUnmanaged(Neighbour);
pub const FlowDataList = ds.ArrayListUnmanaged(FlowData);

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
  Compiled,
  Resolved,
  Processing,
  Unresolved,

  pub inline fn isCompiled(res: @This()) bool {
    return res == .Compiled;
  }

  pub inline fn isNotCompiled(res: @This()) bool {
    return res != .Compiled and res != .Processing;
  }

  pub inline fn isResolved(res: @This()) bool {
    return res == .Resolved;
  }

  pub inline fn isUnresolved(res: @This()) bool {
    return res == .Unresolved;
  }
};

pub const Neighbour = struct {
  /// the edge type along which this node was found
  edge: FlowEdge = .ESequential,
  /// the node itself
  node: FlowNode,
};

pub const FlowData = struct {
  /// Tag associated with this node
  tag: FlowTag,
  /// whether this node has been type checked successfully
  res: ResolutionState = .Unresolved,
  /// basic block of nodes
  bb: BasicBlock,

  pub inline fn init(tag: FlowTag, bb: BasicBlock) FlowData {
    return FlowData{.tag = tag, .bb = bb};
  }

  pub inline fn isEntryNode(self: *const @This()) bool {
    return self.tag == .CfgEntry;
  }

  pub inline fn isExitNode(self: *const @This()) bool {
    return self.tag == .CfgExit;
  }

  pub inline fn isDeadNode(self: *const @This()) bool {
    return self.tag == .CfgDead;
  }
};

pub const BasicBlock = struct {
  nodes: NodeListU,
  et_scope: bool = false,
  ex_scope: bool = false,
  ex_mdiag: bool = false,

  pub fn init(nodes: NodeListU) @This() {
    return .{.nodes = nodes};
  }

  pub inline fn len(self: *@This()) usize {
    return self.nodes.len();
  }

  pub inline fn isEmpty(self: *@This()) bool {
    return self.nodes.isEmpty();
  }

  pub inline fn isNotEmpty(self: *@This()) bool {
    return self.nodes.isNotEmpty();
  }

  pub inline fn appendNode(self: *@This(), node: *Node, al: Allocator) void {
    self.nodes.append(node, al);
  }

  pub inline fn items(self: *@This()) []*Node {
    return self.nodes.items();
  }

  pub inline fn getLast(self: *@This()) ?*Node {
    return if (self.nodes.isNotEmpty()) self.nodes.getLast() else null;
  }

  pub inline fn getSecondLast(self: *@This()) ?*Node {
    return if (self.nodes.len() >= 2) self.nodes.itemAt(self.nodes.len() - 2) else null;
  }

  /// get the last node. If last is a Scope (exit), get the second last,
  /// if second last is a Scope (entry) -- indicating an empty block, simply return null
  pub inline fn getNonScopeLast(self: *@This()) ?*Node {
    const last = if (self.getLast()) |lst| (if (lst.isScope()) self.getSecondLast() else lst) else null;
    return if (last) |lst| (if (!lst.isScope()) lst else null) else null;
  }

  /// get the last node. If last is a Scope (exit) or DiagEndMarker, get the second last,
  /// if second last is a Scope (entry) -- indicating an empty block, simply return null
  pub inline fn getNonScopeDiagLast(self: *@This()) ?*Node {
    const last = if (self.getLast()) |lst| (if (lst.isScope()) self.getSecondLast() else lst) else null;
    return if (last) |lst| (if (!lst.isScope()) lst else null) else null;
  }

  pub inline fn isOrphanSymbol(self: *@This()) bool {
    return self.len() == 1 and self.nodes.getLast().isScope();
  }
};

pub const FlowNode = struct {
  /// the position in the graph identifying this node
  pos: usize,
  /// the graph to which this node belongs
  graph: *FlowGraph,

  pub inline fn get(self: *const @This()) *FlowData {
    return self.graph.get(self.pos);
  }

  pub inline fn isEntryNode(self: *const @This()) bool {
    return self.get().isEntryNode();
  }

  pub inline fn isExitNode(self: *const @This()) bool {
    return self.get().isExitNode();
  }

  pub inline fn isDeadNode(self: *const @This()) bool {
    return self.get().isDeadNode();
  }

  inline fn getVisited(self: *const @This(), al: Allocator) []bool {
    return self.graph.getVisited(al);
  }

  pub inline fn getNextNeighbours(self: @This()) *NeighbourList {
    return self.graph.getNextNeighbours(self);
  }

  pub inline fn getNextNeighboursWithEdge(self: @This(), edge: FlowEdge, al: Allocator) NeighbourList {
    var neighbours = self.graph.getNextNeighbours(self);
    var result = NeighbourList.init();
    for (neighbours.items()) |ngh| {
      if (ngh.edge == edge) {
        result.append(ngh, al);
      }
    }
    return result;
  }

  pub inline fn getPrevNeighbours(self: @This()) *NeighbourList {
    return self.graph.getPrevNeighbours(self);
  }

  pub fn getOutgoingNodes(start: FlowNode, edge: FlowEdge, al: Allocator) FlowList {
    var nodes = FlowList.init();
    var visited = start.graph.getVisited(al);
    var stack: ds.ArrayListUnmanaged(Neighbour) = undefined;
    {
      // skip the node for which the outgoing nodes is being computed,
      // if on the same edge as its outgoing nodes
      const neighbours = start.getNextNeighbours().items();
      stack = ds.ArrayListUnmanaged(Neighbour).initCapacity(neighbours.len, al);
      stack.appendSliceAssumeCapacity(neighbours);
    }
    while (stack.isNotEmpty()) {
      const ngh = stack.pop();
      if (!visited[ngh.node.pos]) {
        visited[ngh.node.pos]= true;
        if (ngh.edge == edge) {
          nodes.append(ngh.node, al);
        }
        for (ngh.node.getNextNeighbours().items()) |_ngh| {
          if (!visited[_ngh.node.pos]) {
            stack.append(_ngh, al);
          }
        }
      }
    }
    return nodes;
  }

  pub fn isExitScopeOrDiagEndFlowNode(self: FlowNode) bool {
    var bb = self.get().bb;
    if (bb.getLast()) |last| {
      return (last.isExitScope() or last.isDiagEndMarker());
    }
    return false;
  }
  
  pub const FilterPredicate = struct {
    pub inline fn isNotDeadNode(ngh: Neighbour) bool {
      return !ngh.node.get().isDeadNode();
    }

    pub inline fn isRetFlowNode(node: FlowNode) bool {
      return if (node.get().bb.getNonScopeLast()) |lst| lst.isRet() else false;
    }

    pub inline fn isNonRetFlowNode(node: FlowNode) bool {
      return if (node.get().bb.getNonScopeLast()) |lst| !lst.isRet() else true;
    }
  };
};

pub const FlowGraph = struct {
  data: FlowDataList,
  preds: ds.ArrayListUnmanaged(NeighbourList),
  succs: ds.ArrayListUnmanaged(NeighbourList),
  visited: ?[*]bool = null,

  pub fn init(al: Allocator) *@This() {
    const _entry = FlowData.init(.CfgEntry, BasicBlock.init(NodeListU.init()));
    const _exit = FlowData.init(.CfgExit, BasicBlock.init(NodeListU.init()));
    const _dead = FlowData.init(.CfgDead, BasicBlock.init(NodeListU.init()));
    var data = FlowDataList.initCapacity(3, al);
    data.appendSliceAssumeCapacity(&[_]FlowData{_entry, _exit, _dead});
    var preds = ds.ArrayListUnmanaged(NeighbourList).initCapacity(3, al);
    var succs = ds.ArrayListUnmanaged(NeighbourList).initCapacity(3, al);
    preds.appendSliceAssumeCapacity(&[_]NeighbourList{NeighbourList.init(), NeighbourList.init(), NeighbourList.init()});
    succs.appendSliceAssumeCapacity(&[_]NeighbourList{NeighbourList.init(), NeighbourList.init(), NeighbourList.init()});
    return util.box(FlowGraph, .{ .data = data, .preds = preds, .succs = succs}, al);
  }

  pub inline fn getEntry(self: *@This()) *FlowData {
    return self.get(0);
  }

  pub inline fn getExit(self: *@This()) *FlowData {
    return self.get(1);
  }

  pub inline fn getDead(self: *@This()) *FlowData {
    return self.get(2);
  }

  pub inline fn getNextNeighbours(self: *@This(), node: FlowNode) *NeighbourList {
    return &self.succs.items()[node.pos];
  }

  pub inline fn getPrevNeighbours(self: *@This(), node: FlowNode) *NeighbourList {
    return &self.preds.items()[node.pos];
  }

  pub inline fn entry(self: *@This()) FlowNode {
    return .{.pos = 0, .graph = self};
  }

  pub inline fn exit(self: *@This()) FlowNode {
    return .{.pos = 1, .graph = self};
  }

  pub inline fn dead(self: *@This()) FlowNode {
    return .{.pos = 2, .graph = self};
  }

  pub inline fn get(self: *@This(), pos: usize) *FlowData {
    @setRuntimeSafety(false);
    return &self.data.items()[pos];
  }

  pub fn addVertex(self: *@This(), data: FlowData, al: Allocator) FlowNode {
    const pos = self.data.len();
    assert(pos == self.preds.len() and pos == self.succs.len());
    self.data.append(data, al);
    self.preds.append(NeighbourList.init(), al);
    self.succs.append(NeighbourList.init(), al);
    return .{.pos = pos, .graph = self};
  }

  pub inline fn getVertex(self: *@This(), pos: usize) FlowData {
    return self.data.itemAt(pos);
  }

  fn getVisited(self: *@This(), al: Allocator) []bool {
    if (self.visited) |v| {
      var slice = v[0..self.data.len()];
      @memset(slice, false);
      return slice;
    }
    var visited = util.allocSlice(bool, self.data.len(), al);
    @memset(visited, false);
    self.visited = visited.ptr;
    return visited;
  }
};

pub const CompUnit = struct {
  /// named functions
  funcs: ds.ArrayList(*FlowGraph),
  /// classes
  classes: ds.ArrayList(*FlowGraph),
  /// whole program
  program: *FlowGraph,

  pub fn init(al: Allocator) @This() {
    return .{
      .funcs = ds.ArrayList(*FlowGraph).init(al),
      .classes = ds.ArrayList(*FlowGraph).init(al),
      .program = undefined,
    };
  }

  pub fn putFunc(self: *@This(), info: *FlowGraph) void {
    self.funcs.append(info);
  }

  pub fn putClass(self: *@This(), info: *FlowGraph) void {
    self.classes.append(info);
  }

  pub fn lookupFunc(self: *@This(), node: *Node) ?*FlowGraph {
    for (self.funcs.items()) |itm| {
      for (itm.getEntry().bb.items()) |nd| {
        if (nd == node) {
          return itm;
        }
      }
    }
    return null;
  }

  pub fn lookupClass(self: *@This(), node: *Node) ?*FlowGraph {
    for (self.classes.items()) |itm| {
      for (itm.getEntry().bb.items()) |nd| {
        if (nd == node) {
          return itm;
        }
      }
    }
    return null;
  }
};

pub const CFGBuilder = struct {
  graph: *FlowGraph,
  alloc: Allocator,
  /// track entry scopes
  et_scope: u32 = 0,
  /// track exit scopes 
  ex_scope: u32 = 0,
  /// exit match diagnostics marker
  ex_mdiag: u32 = 0,

  const Self = @This();

  pub fn init(allocator: Allocator) Self {
    return Self {.graph = undefined, .alloc = allocator};
  }

  inline fn newGraph(self: *Self) void {
    self.graph = FlowGraph.init(self.alloc);
  }

  fn createConditionNode(self: *Self, cond: *Node) *Node {
    if (cond.isMCondition()) {
      // rewrite cond so that the ConditionNode becomes its tst
      cond.NdMCondition.tst = Node.new(.{.NdCondition = tir.ConditionNode.init(cond.NdMCondition.tst)}, self.alloc);
      return cond;
    } else {
      return Node.new(.{.NdCondition = tir.ConditionNode.init(cond)}, self.alloc);
    }
  }

  fn connectVertices(self: *Self, prev: FlowList, next: FlowNode) void {
    for (prev.list.items) |p_node| {
      assert(p_node.graph == next.graph and self.graph == next.graph);
      self.graph.succs.items()[p_node.pos].append(.{.node = next}, self.alloc);
      self.graph.preds.items()[next.pos].append(.{.node = p_node}, self.alloc);
    }
  }

  pub fn connectVerticesWithEdgeInfo(self: *Self, prev: FlowList, next: FlowNode, edge: FlowEdge) void {
    for (prev.list.items) |p_node| {
      assert(p_node.graph == next.graph and self.graph == next.graph);
      self.graph.succs.items()[p_node.pos].append(.{.node = next, .edge = edge}, self.alloc);
      self.graph.preds.items()[next.pos].append(.{.node = p_node, .edge = edge}, self.alloc);
    }
  }

  inline fn toList(self: *Self, node: FlowNode) FlowList {
    return FlowList.initWith(self.alloc, node);
  }

  fn hasReturnNode(self: *Self, nodes: *FlowList, node: *Node) bool {
    _ = self;
    // AstBlock (`node`) has a return node if there's at least 1 node in the block
    // and `nodes` is empty (since RetNode produces an empty flowlist)
    // This is the same as doing:
        // for (node.NdBlock.nodes) |itm| {
        //   if (itm.isRet()) return true;
        // }
        // return false;
    // but we manage to eliminate the need for iteration
    return nodes.len() == 0 and node.block().nodes.len > 0;
  }

  fn linkAtomic(self: *Self, nodes: NodeListU, prev: FlowList, edge: FlowEdge, tag: FlowTag) FlowList {
    var flow = self.graph.addVertex(FlowData.init(tag, BasicBlock.init(nodes)), self.alloc);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    if (self.et_scope > 0) {
      flow.get().bb.et_scope = true;
      self.et_scope -= 1;
    }
    if (self.ex_scope > 0) {
      flow.get().bb.ex_scope = true;
      self.ex_scope -= 1;
    }
    if (self.ex_mdiag > 0) {
      flow.get().bb.ex_mdiag = true;
      self.ex_mdiag -= 1;
    }
    return self.toList(flow);
  }

  fn linkNodeList(self: *Self, nodes: tir.NodeItems, prev: FlowList, edge: FlowEdge, bb: ?NodeListU) FlowList {
    @setRuntimeSafety(false);
    var _prev = prev;
    var i: usize = 0;
    var bb_nodes: NodeListU = undefined;
    var ext_bb = bb != null;
    main: while (i < nodes.len) {
      bb_nodes = NodeListU.init();
      if (ext_bb) {
        bb_nodes.extend(&bb.?, self.alloc);
        ext_bb = false;
      }
      var atomic = true;
      const slice = nodes[i..];
      for (slice, 0..) |item, j| {
        switch (item.*) {
          .NdSimpleIf => {
            // We can only reach this point when comming from a 
            // match statement that's been compiled down to an if-else node
            _prev = self.linkSimpleIf(item, &bb_nodes, _prev, edge);
            atomic = false;
          },
          .NdWhile => |*nd| {
            // link the current bb to this while's condition
            //* prev -> cond, cond -> then.body, last.then.body -> cond, cond -> while-exit
            //* prev
            //* while cond
            //*   ... (then.body)
            //* end
            //* next (while-exit)

            //* prev -> cond
            var cond = self.createConditionNode(nd.cond);
            cond.NdCondition.is_from_loop = true;
            bb_nodes.append(cond, self.alloc);
            var frm_cnd = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);

            //* cond -> then.body
            var frm_then = self.linkBlock(nd.then, frm_cnd, .ETrue, null);
            // TODO:
            //* last.then.body -> cond

            //* cond -> else
            // the nodes after this while has both the while's condition and it's body (ETrue path)
            // as their predecessors, so we include both for `_prev`
            _prev = frm_then;
            // when linking nd.then, if the block is empty, _prev (frm_cnd) would be returned,
            // in such case, extending _prev (frm_then) here implies extending the list with itself
            // which is UB because extend invalidates pointers.
            if (_prev.list.items.ptr != frm_cnd.list.items.ptr) {
              _prev.extend(&frm_cnd, self.alloc);
            }
            atomic = false;
          },
          .NdControl => {
            // continue -> loop-cond, break -> loop-exit
            bb_nodes.append(item, self.alloc);
            const is_last = i == nodes.len - 1;
            if (is_last) {
              _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
            } else if (!nodes[i + 1].isExitScope()) {
               _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
              _prev.append(self.graph.dead(), self.alloc);
            } else {
              assert(i + 1 == nodes.len - 1);
              // this is an exit ScopeNode. We need to add that to the basic block
              self.ex_scope += 1;
              _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
              i += 1;
            }
            atomic = false;
          },
          .NdRet => {
            bb_nodes.append(item, self.alloc);
            const is_last = i == nodes.len - 1;
            if (is_last) {
              _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
              self.connectVerticesWithEdgeInfo(_prev, self.graph.exit(), edge);
              _prev = FlowList.init();
            } else if (!nodes[i + 1].isExitScope()) {
              _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
              self.connectVerticesWithEdgeInfo(_prev, self.graph.exit(), edge);
              _prev = self.toList(self.graph.dead());
            } else {
              assert(i + 1 == nodes.len - 1);
              // this is an exit ScopeNode. We need to add that to the basic block
              self.ex_scope += 1;
              _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
              self.connectVerticesWithEdgeInfo(_prev, self.graph.exit(), edge);
              _prev = FlowList.init();
              i += 1;
            }
            atomic = false;
          },
          .NdBlock => {
            _prev = self.linkBlock(item, _prev, edge, bb_nodes);
            atomic = false;
          },
          .NdMatch => {
            var lnode = item.NdMatch.lnode.?;
            assert(lnode.isBlock());
            const token = item.NdMatch.expr.getToken();
            const start_diag = Node.new(.{.NdDiagStartMarker = .{.token = token}}, self.alloc);
            start_diag.NdDiagStartMarker.payload = item;
            lnode.block().prepend(start_diag, self.alloc);
            lnode.block().append(Node.new(.{.NdScope = tir.ScopeNode.init(false, false)}, self.alloc), self.alloc);
            _prev = self.linkBlock(lnode, _prev, edge, bb_nodes);
            atomic = false;
          },
          .NdScope => |*nd| {
            if (nd.enter) {
              if (_prev.isNotEmpty() and !_prev.getLast().isDeadNode()) {
                bb_nodes.append(item, self.alloc);
              }
            } else if (nd.leave) {
              if (_prev.isNotEmpty() and !_prev.getLast().isDeadNode()) {
                bb_nodes.append(item, self.alloc);
              }
            } else {
              if (j + 1 != slice.len) {
                self.ex_mdiag += 1;
              } else if (_prev.isNotEmpty() and !_prev.getLast().isDeadNode()) {
                bb_nodes.append(item, self.alloc);
              }
            }
          },
          else => {
            bb_nodes.append(item, self.alloc);
          },
        }
        i += 1;
        if (!atomic) continue :main;
      }
      if (bb_nodes.isNotEmpty()) {
        _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
      }
    }
    return _prev;
  }
  
  fn linkBlock(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge, bb: ?NodeListU) FlowList {
    if (edge == .EFalse and node.block().nodes.len == 0) {
      node.block().append(Node.new(.{.NdEmpty = tir.SymNode.init(node.getToken())}, self.alloc), self.alloc);
    }
    return self.linkNodeList(node.block().nodes, prev, edge, bb);
  }

  fn linkSimpleIf(self: *Self, ast_node: *Node, bb_nodes: *NodeListU, prev: FlowList, edge: FlowEdge) FlowList {
    // link the current bb to this if's condition
    var node = &ast_node.NdSimpleIf;
    bb_nodes.append(self.createConditionNode(node.cond), self.alloc);
    const _prev = self.linkAtomic(bb_nodes.*, prev, edge, .CfgOther);
    // link this if's then & else branches
    var ret = FlowList.init();
    // cond->then.body, and to if-exit
    var if_then = self.linkBlock(node.then, _prev, .ETrue, null);
    ret.extend(&if_then, self.alloc);
    // cond->else, and to if-exit
    const els_node = node.els;
    var els = (
      if (els_node.isSimpleIf()) self.linkSimpleIf(els_node, @constCast(&NodeListU.init()), _prev, .EFalse)
      else self.linkBlock(els_node, _prev, .EFalse, null)
    );
    ret.extend(&els, self.alloc);
    // if we return from both branches of the if condition, then whatever follows the if is dead
    if (self.hasReturnNode(&if_then, node.then) and self.hasReturnNode(&els, els_node)) {
      ret.append(self.graph.dead(), self.alloc);
    }
    return ret;
  }

  pub fn buildFun(self: *Self, node: *Node) *FlowGraph {
    // skip generic functions
    assert(node.isBasicFun());
    self.newGraph();
    // build the cfg of fun
    const fun = &node.NdBasicFun;
    var nodes = NodeListU.init();
    nodes.ensureTotalCapacity(fun.params.len + fun.data.body.block().nodes.len, self.alloc);
    for (fun.params) |param| {
      nodes.appendAssumeCapacity(Node.new(.{.NdParam = param.*}, self.alloc));
    }
    nodes.appendSliceAssumeCapacity(fun.data.body.block().nodes);
    const _prev = self.linkNodeList(nodes.items(), self.toList(self.graph.entry()), .ESequential, null);
    self.connectVertices(_prev, self.graph.exit());
    // save node for future lookup()s
    self.graph.getEntry().bb.appendNode(node, self.alloc);
    return self.graph;
  }

  pub fn buildProgram(self: *Self, ast_node: *Node) *FlowGraph {
    self.newGraph();
    var node = &ast_node.NdProgram;
    var _prev = self.linkNodeList(node.decls, self.toList(self.graph.entry()), .ESequential, null);
    self.connectVertices(_prev, self.graph.exit());
    return self.graph;
  }
};
