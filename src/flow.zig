const std = @import("std");
const types = @import("type.zig");
const ast = @import("ast.zig");
const util = @import("util.zig");
const ds = @import("ds.zig");

const Node = ast.AstNode;
const Type = types.Type;
pub const FlowList = ds.ArrayList(*FlowNode);
const FlowDataList = ds.ArrayList(FlowData);
const NodeList = ds.ArrayList(*Node);

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

pub const BasicBlock = struct {
  nodes: NodeList,

  pub fn init(nodes: NodeList) @This() {
    return @This(){.nodes = nodes};
  }

  pub inline fn appendNode(self: *@This(), node: *Node) void {
    self.nodes.append(node);
  }

  pub inline fn items(self: *@This()) []*Node {
    return self.nodes.items();
  }

  pub inline fn getLast(self: *@This()) ?*Node {
    return if (self.nodes.len() > 0) self.nodes.getLast() else null;
  }
};

pub const FlowNode = struct {
  /// Tag associated with this node
  tag: FlowTag,
  /// basic block of nodes
  bb: BasicBlock,
  /// the edge type along which this node was found
  edge: FlowEdge = .ESequential,
  /// whether this node has been type checked successfully
  res: ResolutionState = .Unresolved,
  /// predecessors (incoming edges to this node) and successors (outgoing edges from this node)
  prev_next: FlowDataList,

  pub fn init(tag: FlowTag, nodes: NodeList, allocator: std.mem.Allocator) *@This() {
    var self = util.alloc(FlowNode, allocator);
    self.* = @This() {
      .tag = tag, .bb = BasicBlock.init(nodes),
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

  fn outgoingNodes(node: *@This(), visited: *std.AutoArrayHashMap(*FlowNode, u32), nodes: *FlowList, edge: FlowEdge) void {
    // skip the node for which the outgoing nodes is being computed,
    // if on the same edge as its outgoing nodes
    if (node.edge == edge and visited.count() > 0) {
      nodes.append(node);
    }
    visited.put(node, 0) catch {};
    for (node.prev_next.items()) |nd| {
      if (nd.next and visited.get(nd.flo) == null) {
        outgoingNodes(nd.flo, visited, nodes, edge);
      }
    }
  }

  pub fn getOutgoingNodes(node: *@This(), edge: FlowEdge, allocator: std.mem.Allocator) FlowList {
    var nodes = FlowList.init(allocator);
    var visited = std.AutoArrayHashMap(*FlowNode, u32).init(allocator);
    outgoingNodes(node, &visited, &nodes, edge);
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
  /// classes
  classes: ds.ArrayList(FlowMeta),
  /// whole program
  program: FlowMeta,

  pub fn init(al: std.mem.Allocator) @This() {
    return @This() {
      .funcs = ds.ArrayList(FlowMeta).init(al),
      .classes = ds.ArrayList(FlowMeta).init(al),
      .program = undefined,
    };
  }

  pub fn putFunc(self: *@This(), info: FlowMeta) void {
    self.funcs.append(info);
  }

  pub fn putClass(self: *@This(), info: FlowMeta) void {
    self.classes.append(info);
  }

  pub fn lookupFunc(self: *@This(), node: *Node) ?FlowMeta {
    for (self.funcs.items()) |itm| {
      for (itm.entry.bb.items()) |nd| {
        if (nd == node) {
          return itm;
        }
      }
    }
    return null;
  }

  pub fn lookupClass(self: *@This(), node: *Node) ?FlowMeta {
    for (self.classes.items()) |itm| {
      for (itm.entry.bb.items()) |nd| {
        if (nd == node) {
          return itm;
        }
      }
    }
    return null;
  }
};

pub const CFGBuilder = struct {
  entry: *FlowNode,
  exit: *FlowNode,
  dead: *FlowNode,
  cfg: *CFG = undefined,
  alloc: std.mem.Allocator,

  const Self = @This();

  pub fn init(allocator: std.mem.Allocator) Self {
    var empty = createEmptyNode(allocator);
    return Self {
      .entry = FlowNode.init(.CfgEntry, empty, allocator),
      .exit = FlowNode.init(.CfgExit, empty, allocator),
      .dead = FlowNode.init(.CfgDead, empty, allocator),
      .alloc = allocator,
    };
  }

  pub fn initWithExit(allocator: std.mem.Allocator, exit: *FlowNode) Self {
    var empty = createEmptyNode(allocator);
    return Self {
      .entry = FlowNode.init(.CfgEntry, empty, allocator),
      .exit = exit,
      .dead = FlowNode.init(.CfgDead, empty, allocator),
      .alloc = allocator,
    };
  }


  fn createEmptyNode(allocator: std.mem.Allocator) NodeList {
    var node = util.alloc(Node, allocator);
    node.* = .{.AstEmpty = ast.EmptyNode.init(ast.Token.getDefault())};
    return NodeList.initWith(allocator, node);
  }

  fn createConditionNode(self: *Self, cond: *Node) *Node {
    var node = util.alloc(Node, self.alloc);
    node.* = .{.AstCondition = ast.ConditionNode.init(cond)};
    return node;
  }

  fn getFlowNode(self: *Self, nodes: NodeList, tag: FlowTag) *FlowNode {
    return FlowNode.init(tag, nodes, self.alloc);
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
      ast.BlockNode.newEmptyBlock(self.alloc, node.cond)
    );
    var els: ?*Node = null;
    if (node.elifs.len() > 0) {
      for (node.elifs.items(), 0..) |elif, i| {
        var if_ = elif.AstElif.toIf(self.alloc);
        var nd = util.alloc(Node, self.alloc);
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

  fn hasReturnNode(self: *Self, nodes: *FlowList, node: *Node) bool {
    _ = self;
    // AstBlock (`node`) has a return node if there's at least 1 node in the block
    // and `nodes` is empty (since RetNode produces an empty flowlist)
    // This is the same as doing:
    //     for (node.AstBlock.nodes.items()) |itm| {
    //       if (itm.isRet()) return true;
    //     }
    //     return false;
    // but we manage to eliminate the need for iteration
    return nodes.len() == 0 and node.AstBlock.nodes.len() > 0;
  }

  fn linkAtomic(self: *Self, nodes: NodeList, prev: FlowList, edge: FlowEdge, tag: FlowTag) FlowList {
    var flow = self.getFlowNode(nodes, tag);
    self.connectVerticesWithEdgeInfo(prev, flow, edge);
    return flow.toList(self.alloc);
  }

  fn linkNodeList(self: *Self, nodes: *NodeList, cond: ?*Node, prev: FlowList, edge: FlowEdge) FlowList {
    // cond indicates whether this block is from a branching entry or a normal do..end block
    var _prev = prev;
    var i: usize = 0;
    @setRuntimeSafety(false);
    while (i < nodes.len()) {
      var bb_nodes = NodeList.init(self.alloc);
      var atomic = true;
      var slice = nodes.items()[i..];
      for (slice) |item| {
        switch (item.*) {
          .AstAlias => {},
          .AstIf => |*nd| {
            // link the current bb to this if's condition
            var tmp = self.simplifyIfNode(nd);
            var simple_if = util.alloc(Node, self.alloc);
            simple_if.* = .{.AstSimpleIf = tmp};
            _prev = self.linkSimpleIf(simple_if, &bb_nodes, _prev, edge);
            atomic = false;
          },
          .AstWhile => |*nd| {
            // link the current bb to this while's condition
            //* prev -> cond, cond -> then.body, last.then.body -> cond, cond -> while-exit
            //* prev
            //* while cond
            //*   ... (then.body)
            //* end
            //* next (while-exit)

            //* prev -> cond
            bb_nodes.append(self.createConditionNode(nd.cond));
            var frm_cnd = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);

            //* cond -> then.body
            var frm_then = self.linkBlock(nd.then, frm_cnd, .ETrue);

            // TODO:
            //* last.then.body -> cond

            //* cond -> else
            // the nodes after this while has both the while's condition and it's body (ETrue path)
            // as their predecessors, so we include both for `_prev`
            _prev = frm_then;
            _prev.extend(&frm_cnd);
            atomic = false;
          },
          .AstControl => {
            // continue -> loop-cond, break -> loop-exit
            var is_last = if (cond != null) i + 1 == nodes.len() else false;
            bb_nodes.append(item);
            _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
            if (!is_last) {
              // if the next thing after this ControlNode thing isn't a ScopeNode, then it's def dead
              if (!nodes.itemAt(i + 1).isScope()) {
                _prev.append(self.dead);
              }
            }
            atomic = false;
          },
          .AstRet => {
            bb_nodes.append(item);
            _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
            self.connectVerticesWithEdgeInfo(_prev, self.exit, edge);
            var is_last = if (cond != null) i + 1 == nodes.len() else false;
            if (!is_last) {
              _prev = self.dead.toList(self.alloc);
            } else {
              _prev = FlowList.init(self.alloc);
            }
            atomic = false;
          },
          .AstBlock => {
            if (bb_nodes.isNotEmpty()) {
              _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
            }
            _prev = self.linkBlock(item, _prev, edge);
            atomic = false;
          },
          else => {
            bb_nodes.append(item);
          },
        }
        i += 1;
        if (!atomic) {
          bb_nodes = NodeList.init(self.alloc);
          break;
        }
      }
      if (bb_nodes.len() > 0) {
        _prev = self.linkAtomic(bb_nodes, _prev, edge, .CfgOther);
      }
    }
    return _prev;
  }
  
  fn linkBlock(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var block = &node.AstBlock;
    return self.linkNodeList(&block.nodes, block.cond, prev, edge);
  }

  fn linkSimpleIf(self: *Self, ast_node: *Node, bb_nodes: *NodeList, prev: FlowList, edge: FlowEdge) FlowList {
    // link the current bb to this if's condition
    var node = &ast_node.AstSimpleIf;
    var cond = self.createConditionNode(node.cond);
    bb_nodes.append(cond);
    var _prev = self.linkAtomic(bb_nodes.*, prev, edge, .CfgOther);
    // link this if's then & else branches
    var ret = FlowList.init(self.alloc);
    // cond->then.body, and to if-exit 
    var if_then = self.linkBlock(node.then, _prev, .ETrue);
    ret.extend(&if_then);
    // cond->else, and to if-exit
    var els = (
      if (node.els.isSimpleIf()) self.linkSimpleIf(node.els, @constCast(&NodeList.init(self.alloc)), _prev, .EFalse)
      else self.linkBlock(node.els, _prev, .EFalse)
    );
    ret.extend(&els);
    // if we return from both branches of the if condition, then whatever follows the if is dead
    if (self.hasReturnNode(&if_then, node.then) and self.hasReturnNode(&els, node.els)) {
      ret.append(self.dead);
    }
    return ret;
  }

  fn linkFun(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    // this is just a placeholder node to complete the link
    // between functions and non-functions in the flow graph
    var flow_list = self.linkAtomic(NodeList.initWith(self.alloc, node), prev, edge, .CfgOther);
    // skip generic functions
    if (node.AstFun.isGeneric()) return flow_list;
    // build the cfg of fun
    var fun = &node.AstFun;
    // TODO: cache builder
    var builder = CFGBuilder.init(self.alloc);
    var synth = ast.BlockNode.init(self.alloc, fun.body.AstBlock.cond);
    synth.nodes.ensureTotalCapacity(fun.params.len() + fun.body.AstBlock.nodes.len());
    for (fun.params.items()) |param| {
      var tmp = util.alloc(Node, self.alloc);
      tmp.* = @as(Node, .{.AstVarDecl = param});
      synth.nodes.append(tmp);
    }
    synth.nodes.extend(&fun.body.AstBlock.nodes);
    var body = @as(Node, .{.AstBlock = synth});
    var flo_meta = builder.buildBlock(self.cfg, &body);
    // save node for future lookup()s
    flo_meta.entry.bb.appendNode(node);
    self.cfg.putFunc(flo_meta);
    return flow_list;
  }

  fn linkClass(self: *Self, node: *Node, prev: FlowList, edge: FlowEdge) FlowList {
    var flow_list = self.linkAtomic(NodeList.initWith(self.alloc, node), prev, edge, .CfgOther);
    // skip generic classes
    if (node.AstClass.isGeneric()) return flow_list;
    // build the cfg of fun
    var cls = &node.AstClass;
    // TODO: cache builder
    var builder = CFGBuilder.init(self.alloc);
    var synth = ast.BlockNode.init(self.alloc, null);
    synth.nodes.ensureTotalCapacity(cls.fields.len() + cls.methods.len());
    synth.nodes.extend(cls.fields);
    synth.nodes.extend(cls.methods);
    var body = @as(Node, .{.AstBlock = synth});
    var flo_meta = builder.buildBlock(self.cfg, &body);
    // save node for future lookup()s
    flo_meta.entry.bb.appendNode(node);
    self.cfg.putClass(flo_meta);
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
      .AstBlock => self.linkBlock(node, prev, edge),
      .AstFun => self.linkFun(node, prev, edge),
      .AstClass => self.linkClass(node, prev, edge),
      else => |nd| {
        std.log.debug("trying to link node: {}\n", .{nd});
        return self.linkAtomic(NodeList.initWith(self.alloc, node), prev, edge, .CfgOther);
      },
    };
  }

  pub fn buildOrElse(self: *Self, cfg: *CFG, node: *Node) FlowMeta {
    self.cfg = cfg;
    var err = node.AstOrElse.err;
    if (!err.isBlock()) {
      err = util.alloc(Node, self.alloc);
      var block = ast.BlockNode.init(self.alloc, node.AstOrElse.ok);
      block.nodes.append(node.AstOrElse.err);
      err.* = .{.AstBlock = block};
    }
    _ = self.link(err, self.entry.toList(self.alloc), .ESequential);
    return FlowMeta.init(self.entry, self.exit, self.dead);
  }

  pub fn buildFun(self: *Self, cfg: *CFG, node: *Node) void {
    self.cfg = cfg;
    _ = self.linkFun(node, self.entry.toList(self.alloc), .ESequential);
  }

  pub fn buildCls(self: *Self, cfg: *CFG, node: *Node) void {
    self.cfg = cfg;
    _ = self.linkClass(node, self.entry.toList(self.alloc), .ESequential);
  }

  pub fn buildBlock(self: *Self, cfg: *CFG, ast_node: *Node) FlowMeta {
    self.cfg = cfg;
    var node = &ast_node.AstBlock;
    var _prev = self.linkNodeList(&node.nodes, node.cond, self.entry.toList(self.alloc), .ESequential);
    self.connectVertices(_prev, self.exit);
    return FlowMeta.init(self.entry, self.exit, self.dead);
  }

  pub fn build(self: *Self, root: *Node) CFG {
    var cfg = CFG.init(self.alloc);
    self.cfg = &cfg;
    _ = self.linkProgram(root, self.entry.toList(self.alloc), .ESequential);
    self.cfg = undefined;
    return cfg;
  }
};
