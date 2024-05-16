const std = @import("std");
const ks = @import("constants.zig");
const tir = @import("tir.zig");
const util = @import("util.zig");
const ptn = @import("pattern.zig");
const diagnostics = @import("diagnostics.zig");

const Node = tir.Node;
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

  pub fn init(namegen: util.NameGen, diag: *Diagnostic) Desugar {
    return .{
      .namegen = namegen,
      .al = namegen.al,
      .block = undefined,
      .mc = MatchCompiler.init(diag, namegen.al),
      .u8w = util.U8Writer.init(namegen.al),
    };
  }

  inline fn newNode(self: *Desugar, data: anytype) *Node {
    return Node.box(data, self.al);
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
    const token2 = token.tkFrom("", .TkIdent);
    const val = self.newNode(.{.NdEmpty = tir.SymNode.init(token2)});
    const decl = self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(token, val, null)});
    const ident = self.newNode(.{.NdTVar = tir.TVarNode.init(token)});
    for (nd.cases) |case| {
      if (case.body.node.isBlock()) {
        const index: usize = if (case.body.node.NdBlock.nodes.len > 1) 1 else 0;
        const src = case.body.node.NdBlock.nodes[index];
        const assign = self.newNode(.{.NdAssign = tir.BinaryNode.init(ident.clone(self.al), src, token.tkFrom("=", .TkNoReturn))});
        var stmt = self.newNode(.{.NdExprStmt = tir.ExprStmtNode.init(assign)});
        stmt.NdExprStmt.has_sugar = assign.hasSugar();
        case.body.node.NdBlock.nodes[index] = stmt;
      } else {
        const assign = self.newNode(.{.NdAssign = tir.BinaryNode.init(ident.clone(self.al), case.body.node, token.tkFrom("=", .TkNoReturn))});
        var stmt = self.newNode(.{.NdExprStmt = tir.ExprStmtNode.init(assign)});
        stmt.NdExprStmt.has_sugar = assign.hasSugar();
        case.body.node = stmt;
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
    const decl = self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(token, self.desExpr(node.ok), null)});
    const ident = self.newNode(.{.NdTVar = tir.TVarNode.init(token)});
    // result id
    const token2 = node.ok.getToken().tkFrom(self.namegen.generate("$r", .{}), .TkIdent);
    const val = self.newNode(.{.NdEmpty = tir.SymNode.init(token)});
    const decl2 = self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(token2, val, null)});
    const ident2 = self.newNode(.{.NdTVar = tir.TVarNode.init(token2)});
    
    // cond: $p is Error
    const is_token = token.tkFrom("is", .TkIs);
    var typ = Type.newTag(ks.ErrorVar, .TkError);
    const err = self.newNode(.{.NdType = tir.TypeNode.init(typ.box(self.al), token)});
    const cond = self.newNode(.{.NdBinary = tir.BinaryNode.init(ident, err, is_token)});
    // then:
    var then: *Node = undefined;
    const eq_token = token.tkFrom("=", .TkEqual);
    {
      var evar_decl: ?*Node = null;
      if (node.evar) |evar| {
        // let evar = $gen
        evar_decl = self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(evar.token, ident.clone(self.al), null)});
      }
      var stmts: []*Node = @constCast(&[_]*Node{});
      var stmt: *Node = undefined;
      // assign err to generated ident
      if (!node.err.isRet()) {
        const assign = self.newNode(.{.NdAssign = tir.BinaryNode.init(ident2.clone(self.al), node.err, token.tkFrom("=", .TkNoReturn))});
        stmt = self.newNode(.{.NdExprStmt = tir.ExprStmtNode.init(assign)});
        stmt.NdExprStmt.has_sugar = assign.hasSugar();
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
      const zero = self.newNode(.{.NdNumber = tir.NumberNode.init(token.tkFrom("0", .TkNumber), 0)});
      const access = self.newNode(.{.NdDotAccess = tir.DotAccessNode.init(ident.clone(self.al), zero)});
      access.NdDotAccess.allow_tag_access = true;
      const assign = self.newNode(.{.NdAssign = tir.BinaryNode.init(ident2.clone(self.al), access, eq_token)});
      const stmt = self.newNode(.{.NdExprStmt = tir.ExprStmtNode.init(assign)});
      stmt.NdExprStmt.has_sugar = assign.hasSugar();
      els = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{stmt}), self.al);
    }
    // if $p is Error then:
    const tmp = tir.SimpleIfNode.init(cond, then, els);
    const ifs = self.newNode(.{.NdSimpleIf = tmp});
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
    const decl1 = self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(vartk1, self.desExpr(node.expr), null)});
    // if $t is None
    var ident = self.newNode(.{.NdTVar = tir.TVarNode.init(vartk1)});
    const is_token = token.tkFrom("is", .TkIs);
    var typ = Type.newTag(ks.NoneVar, .TkNone);
    const none = self.newNode(.{.NdType = tir.TypeNode.init(typ.box(self.al), token)});
    const cond = self.newNode(.{.NdBinary = tir.BinaryNode.init(ident, none, is_token)});
    // return $t
    const ret = self.newNode(.{.NdRet = tir.RetNode.init(ident, vartk1)});
    const then = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{ret}), self.al);
    const els = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{}), self.al);
    const ifs = self.newNode(.{.NdSimpleIf = tir.SimpleIfNode.init(cond, then, els)});
    // unwrap
    const zero = self.newNode(.{.NdNumber = tir.NumberNode.init(token.tkFrom("0", .TkNumber), 0)});
    const val = self.newNode(.{.NdDotAccess = tir.DotAccessNode.init(ident.clone(self.al), zero)});
    const decl2 = self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(vartk2, val, null)});
    self.block.appendSlice(@constCast(&[_]*Node{decl1, ifs, decl2}));
    if (util.getMode() == .Debug) {
      Node.render(decl1, 0, &self.u8w) catch {};
      Node.render(ifs, 0, &self.u8w) catch {};
      Node.render(decl2, 0, &self.u8w) catch {};
      logger.debug("deref:\n{s}\n", .{self.u8w.items()});
    }
    return self.newNode(.{.NdTVar = tir.TVarNode.init(vartk2)});
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

  fn desPipe(self: *Desugar, node: *tir.BinaryNode) *Node {
    // substitue lhs into rhs - if pipe placeholder is found
    // if not, turn rhs into call, and pass lhs as its sole argument
    if (self.subPipeHolder(node.left, node.right)) {
      if (util.getMode() == .Debug) {
        node.right.render(0, &self.u8w) catch {};
        logger.debug("pipeline:\n{s}", .{self.u8w.items()});
      }
      return node.right;
    }
    var args = util.allocSlice(*Node, 1, self.al);
    args[0] = node.left;
    var res = self.newNode(.{.NdBasicCall = tir.BasicCallNode.init(node.right, args)});
    if (util.getMode() == .Debug) {
      res.render(0, &self.u8w) catch {};
      logger.debug("pipeline:\n{s}", .{self.u8w.items()});
    }
    return res;
  }

  fn desExpr(self: *Desugar, node: *Node) *Node {
    switch (node.*) {
      .NdBinary => |*nd| {
        if (nd.op_tkty == .TkPipeGthan) {
          return self.desExpr(self.desPipe(nd));
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
      .NdSimpleIf => |*nd| {
        nd.cond = self.desExpr(nd.cond);
        nd.then = self.des(nd.then);
        nd.els = self.des(nd.els);
      },
      .NdBasicFun, .NdGenericFun => {},
      .NdClass => |*nd| {
        for (nd.data.methods) |mth| {
          self.desugarFun(mth) catch {};
        }
      },
      .NdMatch => |*nd| self.desMatchStmt(nd),
      .NdRet => |*nd| {
        if (nd.expr) |expr| {
          nd.expr = self.desExpr(expr);
        }
      },
      .NdProgram => |*nd| {
        nd.decls = self._desugarBlock(nd.decls);
      },
      else => return self.desExpr(node),
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