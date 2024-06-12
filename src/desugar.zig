const std = @import("std");
const ks = @import("constants.zig");
const tir = @import("tir.zig");
const util = @import("util.zig");
const ptn = @import("pattern.zig");
const ds = @import("ds.zig");
const diagnostics = @import("diagnostics.zig");

const Node = tir.Node;
const Token = tir.Token;
const Allocator = std.mem.Allocator;
const NodeItems = tir.NodeItems;
const Type = tir.Type;
const NodeList = tir.NodeList;
const MatchCompiler = ptn.MatchCompiler;
const Diagnostic = diagnostics.Diagnostic;
pub const logger = std.log.scoped(.desugar);

pub const Desugar = struct {
  namegen: util.NameGen,
  al: Allocator,
  block: NodeList,
  mc: MatchCompiler,
  u8w: util.U8Writer,
  pipesubs: ds.ArrayHashMapUnmanaged(*Node, ds.ArrayListUnmanaged(*Node)),

  pub fn init(namegen: util.NameGen, diag: *Diagnostic) Desugar {
    return .{
      .namegen = namegen,
      .al = namegen.al,
      .block = undefined,
      .mc = MatchCompiler.init(diag, namegen.al),
      .u8w = util.U8Writer.init(namegen.al),
      .pipesubs = ds.ArrayHashMapUnmanaged(*Node, ds.ArrayListUnmanaged(*Node)).init(),
    };
  }

  inline fn newNode(self: *Desugar, data: anytype) *Node {
    return Node.box(data, self.al);
  }

  pub inline fn newTVarNode(self: *Desugar, token: Token) *Node {
    return self.newNode(.{.NdTVar = tir.TVarNode.init(token)});
  }

  pub inline fn newScopeNode(self: *Desugar, enter: bool, exit: bool) *Node {
    return self.newNode(.{.NdScope = tir.ScopeNode.init(enter, exit)});
  }

  inline fn newRetNode(self: *Desugar, expr: ?*Node, token: Token) *Node {
    return self.newNode(.{.NdRet = tir.RetNode.init(expr, token)});
  }

  inline fn newNumberNode(self: *Desugar, token: Token, num: f64) *Node {
    return self.newNode(.{.NdNumber = tir.NumberNode.init(token, num)});
  }

  inline fn newEmptyNode(self: *Desugar, token: Token) *Node {
    return self.newNode(.{.NdEmpty = tir.SymNode.init(token)});
  }

  inline fn newBoolNode(self: *Desugar, token: Token) *Node {
    return self.newNode(.{.NdBool = tir.SymNode.init(token)});
  }

  inline fn newSimpleIfNode(self: *Desugar, cond: *Node, then: *Node, els: *Node) *Node {
    return self.newNode(.{.NdSimpleIf = tir.SimpleIfNode.init(cond, then, els)});
  }

  inline fn newTypeNode(self: *Desugar, typ: *Type, token: Token) *Node {
    return self.newNode(.{.NdType = tir.TypeNode.init(typ.box(self.al), token)});
  }
  
  inline fn newControlNode(self: *Desugar, token: Token) *Node {
    return self.newNode(.{.NdControl = tir.ControlNode.init(token)});
  }

  inline fn newVarDeclNode(self: *Desugar, token: Token, value: *Node) *Node {
    return self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(token, value, null)});
  }

  inline fn newDotAccessNode(self: *Desugar, lhs: *Node, rhs: *Node, allow_tag_access: bool) *Node {
    return self.newNode(.{.NdDotAccess = tir.DotAccessNode.initAll(lhs, rhs, allow_tag_access, null)});
  }

  inline fn newBinaryNode(self: *Desugar, left: *Node, right: *Node, op: Token) *Node {
    return self.newNode(.{.NdBinary = tir.BinaryNode.init(left, right, op)});
  }

  inline fn newAssignNode(self: *Desugar, left: *Node, right: *Node, op: Token) *Node {
    return self.newNode(.{.NdAssign = tir.BinaryNode.init(left, right, op)});
  }

  pub inline fn newCallNode(self: *Desugar, expr: *Node, cargs: []const *Node) *Node {
    var args = util.allocSlice(*Node, cargs.len, self.al);
    @memcpy(args, cargs);
    return self.newNode(.{.NdBasicCall = tir.BasicCallNode.init(expr, args)});
  }

  inline fn newExprStmtNode(self: *Desugar, expr: *Node, has_sugar: bool) *Node {
    return self.newNode(.{.NdExprStmt = tir.ExprStmtNode.initAll(expr, has_sugar)});
  }

  inline fn getNodeList(self: *Desugar, cap: usize) NodeList {
    return NodeList.initCapacity(cap, self.al);
  }

  inline fn _desugarBlock(self: *Desugar, items: NodeItems) NodeItems {
    const curr = self.block;
    defer self.block = curr;
    self.block = self.getNodeList(items.len);
    for (items) |itm| {
      self.block.append(self.des(itm));
    }
    const nodes = self.block.items();
    return nodes;
  }

  inline fn _desugarBlockExpr(self: *Desugar, items: NodeItems) *Node {
    // currently, a block expr is only produced for a match node with the expr
    // being matched transformed into a VarDecl by the parser.
    @setRuntimeSafety(false);
    std.debug.assert(items.len == 2 and items[0].isVarDecl() and items[1].isMatch());
    // keep the VarDecl, return the match desugar
    self.block.append(self.des(items[0]));
    return self.desMatchExpr(items[1]);
  }

  inline fn _desugarItems(self: *Desugar, items: NodeItems) NodeItems {
    var block = self.getNodeList(items.len);
    for (items) |itm| {
      block.append(self.desExpr(itm));
    }
    return block.items();
  }

  fn desMatchExpr(self: *Desugar, node: *Node) *Node {
    var nd = &node.NdMatch;
    const token = nd.expr.getToken().tkFrom(self.namegen.generate("$d", .{}), .TkIdent);
    const val = self.newEmptyNode(token.tkFrom("", .TkIdent));
    const decl = self.newVarDeclNode(token, val);
    const ident = self.newTVarNode(token);
    for (nd.cases) |case| {
      if (case.body.node.isBlock()) {
        const index: usize = if (case.body.node.NdBlock.nodes.len > 1) 1 else 0;
        const src = case.body.node.NdBlock.nodes[index];
        const assign = self.newAssignNode(ident.clone(self.al), src, token.tkFrom("=", .TkNoReturn));
        case.body.node.NdBlock.nodes[index] = self.newExprStmtNode(assign, assign.hasSugar());
      } else {
        const assign = self.newAssignNode(ident.clone(self.al), case.body.node, token.tkFrom("=", .TkNoReturn));
        case.body.node = self.newExprStmtNode(assign, assign.hasSugar());
      }
    }
    self.block.append(decl);
    self.desMatchStmt(nd);
    self.block.append(node); // append the transformed match node as a stmt
    return ident;
  }

  fn desMatchStmt(self: *Desugar, node: *tir.MatchNode) void {
    var nd = node.cloneNode(self.al);
    var tree = self.mc.compile(&nd.NdMatch) catch return;
    const lnode = self.mc.lowerDecisionTree(tree, node.expr.getToken()) catch return;
    if (util.getMode() == .Debug) {
      Node.render(lnode, 0, &self.u8w) catch {};
      logger.debug("tree:\n{s}\n", .{self.u8w.items()});
    }
    node.lnode = self.des(lnode);
  }

  fn desOrElse(self: *Desugar, node: *tir.OrElseNode) *Node {
    // foo() orelse duh
    // decl: let $p = foo()
    // decl: let $r = $p  # result
    // tmp id
    const token = node.ok.getToken().tkFrom(self.namegen.generate("$d", .{}), .TkIdent);
    const decl = self.newVarDeclNode(token, self.desExpr(node.ok));
    const ident = self.newTVarNode(token);
    // result id
    const token2 = node.ok.getToken().tkFrom(self.namegen.generate("$r", .{}), .TkIdent);
    const decl2 = self.newVarDeclNode(token2, self.newEmptyNode(token));
    const ident2 = self.newTVarNode(token2);
    
    // cond: $p is Error
    const cond = self.newBinaryNode(
      ident,
      self.newTypeNode(Type.newTag(ks.ErrorVar, .TkError).box(self.al), token),
      token.tkFrom("is", .TkIs),
    );
    // then:
    var then: *Node = undefined;
    {
      var evar_decl: ?*Node = null;
      if (node.evar) |evar| {
        // let evar = $gen
        evar_decl = self.newVarDeclNode(evar.token, ident.clone(self.al));
      }
      var stmts: []*Node = @constCast(&[_]*Node{});
      var stmt: *Node = undefined;
      // assign err to generated ident
      if (!node.err.isRet()) {
        const assign = self.newAssignNode(ident2.clone(self.al), node.err, token.tkFrom("=", .TkNoReturn));
        stmt = self.newExprStmtNode(assign, assign.hasSugar());
      } else {
        stmt = node.err;
      }
      if (evar_decl) |evd| {
        stmts = @constCast(&[_]*Node{evd, stmt});
      } else {
        stmts = @constCast(&[_]*Node{stmt});
      }
      then = self.des(tir.BlockNode.newBlockWithNodes(stmts, self.al));
    }
    // else:
    var els: *Node = undefined;
    {
      const zero = self.newNumberNode(token.tkFrom("0", .TkNumber), 0);
      const access = self.newDotAccessNode(ident.clone(self.al), zero, true);
      const assign = self.newAssignNode(ident2.clone(self.al), access, token.tkFrom("=", .TkEqual));
      const stmt = self.newExprStmtNode(assign, assign.hasSugar());
      els = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{stmt}), self.al);
    }
    // if $p is Error then:
    const ifs = self.newSimpleIfNode(cond, then, els);
    self.block.appendSlice(@constCast(&[_]*Node{decl, decl2, ifs}));
    if (util.getMode() == .Debug) {
      Node.render(decl, 0, &self.u8w) catch {};
      Node.render(decl2, 0, &self.u8w) catch {};
      Node.render(ifs, 0, &self.u8w) catch {};
      logger.debug("orelse:\n{s}\n", .{self.u8w.items()});
    }
    return ident2;
  }

  fn desDeref(self: *Desugar, node: *tir.DerefNode) *Node {
    const token = node.tkbit.toToken();
    const vartk1 = token.tkFrom(self.namegen.generate("$t", .{}), .TkIdent);
    const vartk2 = token.tkFrom(self.namegen.generate("$r", .{}), .TkIdent);
    // assign
    const decl1 = self.newVarDeclNode(vartk1, self.desExpr(node.expr));
    // if $t is None
    var ident = self.newTVarNode(vartk1);
    const none = self.newTypeNode(Type.newTag(ks.NoneVar, .TkNone).box(self.al), token);
    const cond = self.newBinaryNode(ident, none, token.tkFrom("is", .TkIs));
    // return $t
    const ret = self.newRetNode(ident.clone(self.al), vartk1);
    const then = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{ret}), self.al);
    const els = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{}), self.al);
    const ifs = self.newSimpleIfNode(cond, then, els);
    // unwrap
    const zero = self.newNumberNode(token.tkFrom("0", .TkNumber), 0);
    const decl2 = self.newVarDeclNode(vartk2, self.newDotAccessNode(ident.clone(self.al), zero, false));
    self.block.appendSlice(@constCast(&[_]*Node{decl1, ifs, decl2}));
    if (util.getMode() == .Debug) {
      Node.render(decl1, 0, &self.u8w) catch {};
      Node.render(ifs, 0, &self.u8w) catch {};
      Node.render(decl2, 0, &self.u8w) catch {};
      logger.debug("deref:\n{s}\n", .{self.u8w.items()});
    }
    return self.newTVarNode(vartk2);
  }

  fn desForLoop(self: *Desugar, node: *tir.ForNode, counter: ?Token) *Node {
    // for (i,)? f in foo ... end
    const ident = node.ident.toToken();
    // let itr = @iter(foo)
    const itr = ident.tkFrom(self.namegen.generate("$itr", .{}), .TkIdent);
    const tmp = ident.tkFrom(self.namegen.generate("$tmp", .{}), .TkIdent);
    const tmp_ident = self.newTVarNode(tmp);
    const iter_func_name = self.newTVarNode(ident.tkFrom(ks.IterVar, .TkIdent));
    const decl1 = self.newVarDeclNode(itr, self.newCallNode(iter_func_name, &[_]*Node{node.itrbl}));
    // let i = -1
    var count_decl: ?*Node = null;
    var count_incr: ?*Node = null;
    if (counter) |c| {
      const minus_one = self.newNumberNode(c.tkFrom("-1", .TkNumber), -1);
      count_decl = self.newVarDeclNode(c, minus_one);
      // i = i + 1
      const one = self.newNumberNode(c.tkFrom("1", .TkNumber), 1);
      var cvar = self.newTVarNode(c);
      const add = self.newBinaryNode(cvar.clone(self.al), one, c.tkFrom("+", .TkPlus));
      count_incr = self.newExprStmtNode(self.newAssignNode(cvar, add, c.tkFrom("=", .TkEqual)), false);
    }
    // while true
    const while_cond = self.newBoolNode(ident.tkFrom("true", .TkTrue));
    // let f = itr.next()
    const next_da = self.newDotAccessNode(self.newTVarNode(itr), self.newTVarNode(ident.tkFrom("next", .TkIdent)), false);
    const decl2 = self.newVarDeclNode(tmp, self.newCallNode(next_da, &[_]*Node{}));
    // if f == None
    const none = self.newTypeNode(Type.newTag(ks.NoneVar, .TkNone).box(self.al), ident);
    const if_cond = self.newBinaryNode(tmp_ident.clone(self.al), none, ident.tkFrom("is", .TkIs));
    // break
    const brk = self.newControlNode(ident.tkFrom("break", .TkBreak));
    const if_then = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{brk}), self.al);
    // else: let f = $tmp.0
    const zero = self.newNumberNode(ident.tkFrom("0", .TkNumber), 0);
    const deref = self.newDotAccessNode(tmp_ident, zero, false);
    const decl3 = self.newVarDeclNode(ident, deref);
    node.then.block().prepend(decl3, self.al);
    const if_els = node.then;
    const ifs = self.newSimpleIfNode(if_cond, if_then, if_els);
    const while_then = tir.BlockNode.newBlockWithNodes(@constCast(
      if (count_incr) |inc| &[_]*Node{inc, decl2, ifs} else &[_]*Node{decl2, ifs}
    ), self.al);
    const while_node = self.newNode(.{.NdWhile = tir.WhileNode.init(while_cond, while_then)});
    // we need scope nodes since this is a 'bare' do..end block with a bunch of declarations
    const scope_entry = self.newScopeNode(true, false);
    const scope_exit = self.newScopeNode(false, true);
    const res = tir.BlockNode.newBlockWithNodes(@constCast(
      if (count_decl) |cdecl| &[_]*Node{scope_entry, cdecl, decl1, while_node, scope_exit}
      else &[_]*Node{scope_entry, decl1, while_node, scope_exit}
    ), self.al);
    return self.des(res);
  }

  inline fn subPipeHolderItems(self: *Desugar, sub: *Node, nodes: NodeItems) bool {
    var is_sub = false;
    for (nodes) |elem| {
      if (self.subPipeHolder(sub, elem)) {
        is_sub = true;
      }
    }
    return is_sub;
  }

  /// substitute `sub` into `expr` - if expr contains a pipe placeholder
  fn subPipeHolder(self: *Desugar, sub: *Node, expr: *Node) bool {
    switch (expr.*) {
      .NdPipeHolder => {
        expr.* = sub.clone(self.al).*;
        self.addSub(sub, expr);
        return true;
      },
      .NdUnary => |*nd| {
        return self.subPipeHolder(sub, nd.expr);
      },
      .NdBinary => |*nd| {
        if (nd.op_tkty == .TkPipeGthan) {
          expr.* = self.desPipe(nd).*;
          return false;
        }
        const is_sub = self.subPipeHolder(sub, nd.left);
        return self.subPipeHolder(sub, nd.right) or is_sub;
      },
      .NdSubscript => |*nd| {
        const is_sub = self.subPipeHolder(sub, nd.expr);
        return self.subPipeHolder(sub, nd.index) or is_sub;
      },
      .NdList, .NdTuple => |*nd| {
        return self.subPipeHolderItems(sub, nd.elems);
      },
      .NdMap => |*nd| {
        var is_sub = false;
        for (nd.pairs) |itm| {
          if (self.subPipeHolder(sub, itm.key)) is_sub = true;
          if (self.subPipeHolder(sub, itm.value)) is_sub = true;
        }
        return is_sub;
      },
      .NdDotAccess => |*nd| {
        const is_sub = self.subPipeHolder(sub, nd.lhs);
        return self.subPipeHolder(sub, nd.rhs) or is_sub;
      },
      .NdDeref => |*nd| {
        return self.subPipeHolder(sub, nd.expr);
      },
      .NdCast => |*nd| {
        return self.subPipeHolder(sub, nd.expr);
      },
      .NdBasicCall => |*nd| {
        const is_sub = self.subPipeHolder(sub, nd.expr);
        return self.subPipeHolderItems(sub, nd.args()) or is_sub;
      },
      .NdGenericCall => |*nd| {
        return self.subPipeHolder(sub, nd.call);
      },
      .NdError => |*nd| {
        return self.subPipeHolder(sub, nd.expr);
      },
      .NdLblArg => |*nd| {
        return self.subPipeHolder(sub, nd.value);
      },
      .NdOrElse => |*nd| {
        const is_sub = self.subPipeHolder(sub, nd.ok);
        return self.subPipeHolder(sub, nd.err) or is_sub;
      },
      .NdBasicFun => |*nd| {
        return self.subPipeHolder(sub, nd.data.body);
      },
      .NdMatch => |*nd| {
        var is_sub = self.subPipeHolder(sub, nd.expr);
        for (nd.cases) |case| {
          if (case.guard) |gd| {
            if (self.subPipeHolder(sub, gd)) {
              is_sub = true;
            }
          }
          if (self.subPipeHolder(sub, case.body.node)) {
            is_sub = true;
          }
          if (case.body.decls.len() > 0) {
            logger.debug(
              "[subPipeHolder] case.body.decls > 0: {}",
              .{case.body.decls.len()}
            );
          }
        }
        return is_sub;
      },
      // statements producible from a transformed expr
      .NdVarDecl => |*nd| {
        return self.subPipeHolder(sub, nd.value);
      },
      .NdBlock => |*nd| {
        return self.subPipeHolderItems(sub, nd.nodes);
      },
      .NdExprStmt => |*nd| {
        return self.subPipeHolder(sub, nd.expr);
      },
      .NdWhile => |*nd| {
        const is_sub = self.subPipeHolder(sub, nd.cond);
        return self.subPipeHolder(sub, nd.then) or is_sub;
      },
      .NdSimpleIf => |*nd| {
        const is_sub1 = self.subPipeHolder(sub, nd.cond);
        const is_sub2 = self.subPipeHolder(sub, nd.then);
        return self.subPipeHolder(sub, nd.els) or is_sub1 or is_sub2;
      },
      .NdRet => |*nd| {
        return if (nd.expr) |_expr| self.subPipeHolder(sub, _expr) else false;
      },
      else => {
        logger.debug("[subPipeHolder] node: {}", .{expr});
        return false;
      }
    }
  }

  fn addSub(self: *Desugar, sub: *Node, expr: *Node) void {
    if (self.pipesubs.getPtr(sub)) |lst| {
      lst.append(expr, self.al);
    } else {
      var lst = ds.ArrayListUnmanaged(*Node).initCapacity(1, self.al);
      lst.appendAssumeCapacity(expr);
      self.pipesubs.set(sub, lst, self.al);
    }
  }

  fn deduplicateSubs(self: *Desugar, sub: *Node) void {
    if (self.pipesubs.getPtr(sub)) |lst| {
      if (lst.len() > 1) {
        // lift sub -> expr to vardecl
        const token = sub.getToken().tkFrom(self.namegen.generate("$d", .{}), .TkIdent);
        const ident = self.newTVarNode(token);
        self.block.append(self.newVarDeclNode(token, lst.itemAt(0).forceClone(self.al)));
        for (lst.items()) |expr| {
          expr.* = ident.*;
        }
      }
    }
  }

  fn desPipe(self: *Desugar, node: *tir.BinaryNode) *Node {
    // substitue lhs into rhs - if pipe placeholder is found
    // if not, turn rhs into call, and pass lhs as its sole argument
    if (self.subPipeHolder(node.left, node.right)) {
      self.deduplicateSubs(node.left);
      if (util.getMode() == .Debug) {
        node.right.render(0, &self.u8w) catch {};
        logger.debug("pipeline:\n{s}", .{self.u8w.items()});
      }
      return node.right;
    }
    var res = self.newCallNode(node.right, &[_]*Node{node.left});
    if (util.getMode() == .Debug) {
      res.render(0, &self.u8w) catch {};
      logger.debug("pipeline:\n{s}", .{self.u8w.items()});
    }
    return res;
  }

  fn desConcat(self: *Desugar, node: *tir.BinaryNode) *Node {
    // foo <> bar -> foo.concat(bar)
    const token = node.right.getToken();
    const lhs = self.desExpr(node.left);
    const rhs = self.desExpr(node.right);
    const access = self.newDotAccessNode(lhs, self.newTVarNode(token.tkFrom("concat", .TkIdent)), false);
    return self.newCallNode(access, &[_]*Node{rhs});
  }

  fn desExpr(self: *Desugar, node: *Node) *Node {
    switch (node.*) {
      .NdBinary => |*nd| {
        if (nd.op_tkty == .TkPipeGthan) {
          return self.desExpr(self.desPipe(nd));
        }
        if (nd.op_tkty == .TkGthanLthan) {
          return self.desConcat(nd);
        }
        nd.left = self.desExpr(nd.left);
        nd.right = self.desExpr(nd.right);
      },
      .NdAssign => |*nd| {
        nd.left = self.desExpr(nd.left);
        nd.right = self.desExpr(nd.right);
      },
      .NdSubscript => |*nd| {
        nd.expr = self.desExpr(nd.expr);
        nd.index = self.desExpr(nd.index);
      },
      .NdUnary => |*nd| {
        nd.expr = self.desExpr(nd.expr);
      },
      .NdList, .NdTuple => |*nd| {
        nd.elems = self._desugarItems(nd.elems);
      },
      .NdMap => |*nd| {
        var pairs = tir.ds.ArrayList(tir.MapNode.Pair).initCapacity(nd.pairs.len, self.al);
        for (nd.pairs) |itm| {
          pairs.append(.{.key = self.desExpr(itm.key), .value = self.desExpr(itm.value)});
        }
        nd.pairs = pairs.items();
      },
      .NdDotAccess => |*nd| {
        nd.lhs = self.desExpr(nd.lhs);
        nd.rhs = self.desExpr(nd.rhs);
      },
      .NdDeref => |*nd| {
        if (!nd.assertion) {
          return self.desDeref(nd);
        } else {
          nd.expr = self.desExpr(nd.expr);
        }
      },
      .NdCast => |*nd| {
        nd.expr = self.desExpr(nd.expr);
      },
      .NdBasicCall => |*nd| {
        nd.expr = self.desExpr(nd.expr);
        nd._args = self._desugarItems(nd.args()).ptr;
      },
      .NdGenericCall => |*nd| {
        nd.call = self.desExpr(nd.call);
      },
      .NdError => |*nd| {
        nd.expr = self.desExpr(nd.expr);
      },
      .NdLblArg => |*nd| {
        nd.value = self.desExpr(nd.value);
      },
      .NdOrElse => |*nd| return self.desOrElse(nd),
      .NdBlock => |*nd| return self._desugarBlockExpr(nd.nodes),
      .NdMatch => return self.desMatchExpr(node),
      .NdNumber, .NdString, .NdBool, .NdTVar, .NdType, .NdMCondition => {},
      .NdBasicFun, .NdPipeHolder => return node,
      else => {
        logger.debug("[desExpr] node: {}", .{node});
        unreachable;
      },
    }
    return node;
  }

  fn des(self: *Desugar, node: *Node) *Node {
    if (!node.hasSugar()) return node;
    switch (node.*) {
      .NdVarDecl => |*nd| {
        nd.value = self.desExpr(nd.value);
      },
      .NdExprStmt => |*nd| {
        nd.expr = self.desExpr(nd.expr);
      },
      .NdBlock => |*nd| {
        nd.nodes = self._desugarBlock(nd.nodes);
      },
      .NdWhile => |*nd| {
        nd.cond = self.desExpr(nd.cond);
        nd.then = self.des(nd.then);
      },
      .NdFor => |*nd| {
        return self.desForLoop(nd, null);
      },
      .NdForCounter => |*nd| {
        return self.desForLoop(&nd.forl.NdFor, nd.counter.toToken());
      },
      .NdSimpleIf => |*nd| {
        nd.cond = self.desExpr(nd.cond);
        nd.then = self.des(nd.then);
        nd.els = self.des(nd.els);
      },
      .NdBasicFun, .NdGenericFun => {},
      .NdClass, .NdTrait => |*nd| {
        for (nd.data.methods.items()) |mth| {
          self.desugarFun(mth) catch {};
        }
      },
      .NdMatch => |*nd| {
        self.desMatchStmt(nd);
      },
      .NdRet => |*nd| {
        if (nd.expr) |expr| {
          nd.expr = self.desExpr(expr);
        }
      },
      .NdProgram => |*nd| {
        nd.decls = self._desugarBlock(nd.decls);
      },
      else => {
        return self.desExpr(node);
      },
    }
    return node;
  }

  pub fn desugarFun(self: *Desugar, node: *Node) !void {
    const errors = self.mc.diag.count();
    if (node.isBasicFun()) {
      var nd = &node.NdBasicFun;
      nd.data.body.NdBlock.nodes = self._desugarBlock(nd.data.body.NdBlock.nodes);
    }
    if (self.mc.diag.count() != errors and self.mc.diag.hasErrors()) {
      return error.DesugarError;
    }
  }

  pub fn desugar(self: *Desugar, node: *Node) !*Node {
    const ret = self.des(node);
    if (self.mc.diag.hasErrors()) {
      return error.DesugarError;
    }
    return ret;
  }
};
