const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const diagnostics = @import("diagnostics.zig");
const VebAllocator = @import("allocator.zig");

const Node = ast.AstNode;
const exit = std.os.exit;
pub const TypeKind = types.TypeKind;
const Type = types.Type;
const Generic = types.Generic;
const Union = types.Union;
const Variable = types.Variable;
const Concrete = types.Concrete;
const Function = types.Function;
const Class = types.Class;
const TypeList = types.TypeList;
const NodeList = ast.AstNodeList;
const Diagnostic = diagnostics.Diagnostic;

/// maximum number of elements of a list literal 
const MAX_LISTING_ELEMS = 0xff;
/// maximum number of callable parameters
const MAX_PARAMS = 0xff;
/// maximum number of class fields
const MAX_FIELDS = 0xc8;
/// maximum number of class methods
const MAX_METHODS = 0xc8;

pub const Parser = struct {
  current_tok: lex.Token,
  previous_tok: lex.Token,
  lexer: lex.Lexer,
  allocator: std.mem.Allocator,
  diag: Diagnostic,
  allow_nl: usize = 0,
  func: ?*Node = null,
  class: ?*Node = null,
  in_cast: u32 = 0,
  loops: u32 = 0,

  const Self = @This();
  var name_id: u32 = 0;

  const BindingPower = enum (u32) {
    None,        // other
    Assignment,  //
    Or,          // or
    And,         // and
    BitOr,       // |
    BitXor,      // ^
    BitAnd,      // &
    Equality,    // !=, ==, is
    Comparison,  // >, >=, <, <=
    Shift,       // >>, <<
    Term,        // +, -
    Factor,      // /, *, %
    Power,       // **
    Unary,       // !, -, +, ~
    Call,        // ()
    Access,      // [], ., as
  };
  const ParseError = error{ParseError};
  const PrefixFn = *const fn (*Self, bool) anyerror!*Node;
  const InfixFn = *const fn (*Self, *Node, bool) anyerror!*Node;
  const ExprParseTable = struct {
    bp: BindingPower,
    prefix: ?PrefixFn,
    infix: ?InfixFn
  };
  const ptable = [_]ExprParseTable{
    .{.bp = .Term, .prefix = Self.unary, .infix = Self.binary},         // TkPlus
    .{.bp = .Term, .prefix = Self.unary, .infix = Self.binary},         // TkMinus
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},             // TkSlash
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},             // TkStar
    .{.bp = .Call, .prefix = Self.grouping, .infix = Self.callExpr},    // TkLBracket
    .{.bp = .None, .prefix = null, .infix = null},                      // TkRBracket
    .{.bp = .Access, .prefix = Self.listing, .infix = Self.indexing},   // TkLSqrBracket
    .{.bp = .None, .prefix = null, .infix = null},                      // TkRSqrBracket
    .{.bp = .None, .prefix = null, .infix = null},                      // TkSemic
    .{.bp = .None, .prefix = null, .infix = null},                      // TkColon
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkLthan
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkGthan
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEqual
    .{.bp = .Call, .prefix = Self.mapping, .infix = Self.callExpr},     // TkLCurly
    .{.bp = .None, .prefix = null, .infix = null},                      // TkRCurly
    .{.bp = .BitAnd, .prefix = null, .infix = Self.binary},             // TkAmp
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},             // TkPerc
    .{.bp = .None, .prefix = null, .infix = null},                      // TkComma
    .{.bp = .Unary, .prefix = Self.unary, .infix = null},               // TkExMark
    .{.bp = .BitXor, .prefix = null, .infix = Self.binary},             // TkCaret
    .{.bp = .BitOr, .prefix = null, .infix = Self.binary},              // TkPipe
    .{.bp = .Unary, .prefix = Self.unary, .infix = null},               // TkTilde
    .{.bp = .Access, .prefix = null, .infix = Self.dotderef},           // TkDot
    .{.bp = .None, .prefix = null, .infix = null},                      // TkQMark
    .{.bp = .None, .prefix = null, .infix = null},                      // TkNewline
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEqGrt
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkLeq
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkGeq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},           // Tk2Eq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},           // TkNeq
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},              // Tk2Lthan
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},              // Tk2Rthan
    .{.bp = .Access, .prefix = null, .infix = Self.casting},            // TkAs
    .{.bp = .None, .prefix = null, .infix = null},                      // TkDo
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkFn
    .{.bp = .Equality, .prefix = null, .infix = Self.binIs},            // TkIs
    .{.bp = .None, .prefix = null, .infix = null},                      // TkIf
    .{.bp = .Or, .prefix = null, .infix = Self.binary},                 // TkOr
    .{.bp = .None, .prefix = null, .infix = null},                      // TkFor
    .{.bp = .And, .prefix = null, .infix = Self.binary},                // TkAnd
    .{.bp = .None, .prefix = Self.funExpr, .infix = null},              // TkDef
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEnd
    .{.bp = .None, .prefix = null, .infix = null},                      // TkNot
    .{.bp = .None, .prefix = null, .infix = null},                      // TkLet
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkNum
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkMap
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkStr
    .{.bp = .None, .prefix = Self.nullable, .infix = null},             // TkNil
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkErr
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkAny
    .{.bp = .Unary, .prefix = Self.tryExpr, .infix = null},             // TkTry
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkBool
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkList
    .{.bp = .None, .prefix = null, .infix = null},                      // TkThen
    .{.bp = .None, .prefix = null, .infix = null},                      // TkType
    .{.bp = .None, .prefix = null, .infix = null},                      // TkElse
    .{.bp = .None, .prefix = null, .infix = null},                      // TkElif
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkTrue
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkVoid
    .{.bp = .None, .prefix = Self.selfExpr, .infix = null},             // TkSelf
    .{.bp = .None, .prefix = null, .infix = null},                      // TkClass
    .{.bp = .None, .prefix = null, .infix = null},                      // TkBreak
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkFalse
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkTuple
    .{.bp = .None, .prefix = null, .infix = null},                      // TkWhile
    .{.bp = .Term, .prefix = null, .infix = Self.orElseExpr},           // TkOrElse
    .{.bp = .None, .prefix = null, .infix = null},                      // TkReturn
    .{.bp = .None, .prefix = null, .infix = null},                      // TkContinue
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkNoReturn
    .{.bp = .None, .prefix = Self.number, .infix = null},               // TkNumber
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkString
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkAllocString
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkIdent
    .{.bp = .None, .prefix = null, .infix = null},                      // TkError
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEof
  };

  pub fn init(src: *[]const u8, filename: *const[]const u8, allocator: *VebAllocator) Self {
    var al = allocator.getArenaAllocator();
    return Self {
      .current_tok = undefined,
      .previous_tok = undefined,
      .lexer = lex.Lexer.init(src.*, allocator),
      .diag = Diagnostic.init(al, filename, src),
      // use the arena allocator for allocating general nodes.
      .allocator = al,
    };
  }

  inline fn _errWithArgs(self: *Self, token: lex.Token, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnostics(.DiagError, token, "Error: " ++ fmt, args);
  }

  fn softErrArgs(self: *Self, token: lex.Token, comptime fmt: []const u8, args: anytype) void {
   self._errWithArgs(token, fmt, args);
  }

  fn softErrMsg(self: *Self, token: lex.Token, msg: []const u8) void {
    self._errWithArgs(token, "{s}", .{msg});
  }

  fn errMsg(self: *Self, token: lex.Token, msg: []const u8) ParseError {
    self._errWithArgs(token, "{s}", .{msg});
    return error.ParseError;
  }

  fn errArgs(self: *Self, token: lex.Token, comptime fmt: []const u8, args: anytype) ParseError {
   self._errWithArgs(token, fmt, args);
   return error.ParseError;
  }

  fn advance(self: *Self) !void {
    self.previous_tok = self.current_tok;
    const tok = self.lexer.getToken();
    if (!tok.isErr()) {
      self.current_tok = tok;
    } else {
      return self.errMsg(tok, tok.value);
    }
  }

  fn consume(self: *Self, tty: lex.TokenType) !void {
    if (self.check(tty)) {
      try self.advance();
    } else {
      var buff: [1024]u8 = undefined;
      var msg = std.fmt.bufPrint(
        &buff,  "expected token '{s}', but found '{s}'", 
        .{tty.str(), self.current_tok.ty.str()}
      ) catch exit(1);
      return self.errMsg(self.current_tok, msg);
    }
  }

  inline fn newNode(self: *Self) *Node {
    return self.allocator.create(Node) catch {
      std.debug.print("Allocation failed\n", .{});
      exit(1);
    };
  }

  inline fn check(self: *Self, ty: lex.TokenType) bool {
    return self.current_tok.is(ty);
  }

  inline fn match(self: *Self, ty: lex.TokenType) bool {
    if (self.check(ty)) {
      self.advance() catch {};
      return true;
    }
    return false;
  }

  inline fn incNl(self: *Self) void {
    self.allow_nl += 1;
    self.lexer.allow_nl = self.allow_nl;
  }

  inline fn decNl(self: *Self) void {
    self.allow_nl -= 1;
    self.lexer.allow_nl = self.allow_nl;
  }

  inline fn incLoop(self: *Self) void {
    self.loops += 1;
  }

  inline fn decLoop(self: *Self) void {
    self.loops -= 1;
  }

  inline fn inLoop(self: *Self) bool {
    return self.loops > 0;
  }

  inline fn inFun(self: *Self) bool {
    return self.func != null;
  }

  fn genName(self: *Self, start: []const u8) []const u8 {
    var name = std.fmt.allocPrint(
      self.allocator, "$.{s}.{}", .{start, Self.name_id}
    ) catch return "";
    Self.name_id += 1;
    return name;
  }

  inline fn consumeNlOrEof(self: *Self) !void {
    // Try to consume Newline. 
    // If not, check that the current token is Eof, else error
    if (!self.match(.TkNewline)) {
      if (!self.check(.TkEof)) {
        // error, since we originally expected Newline
        try self.consume(.TkNewline);
      }
    }
  }

  inline fn assertMaxElements(self: *Self, len: usize, msg: []const u8) !void {
    if (len > MAX_LISTING_ELEMS) {
      return self.errMsg(self.current_tok, msg);
    }
  }

  fn _parse(self: *Self, bp: BindingPower) !*Node {
    const prefix = ptable[@enumToInt(self.current_tok.ty)].prefix;
    if (prefix == null) {
      return self.errMsg(self.current_tok, "token found at an invalid prefix position");
    }
    const bp_val = @enumToInt(bp);
    const assignable = bp_val <= @enumToInt(BindingPower.Assignment);
    var node = try prefix.?(self, assignable);
    while (bp_val < @enumToInt(ptable[@enumToInt(self.current_tok.ty)].bp)) {
      var infix = ptable[@enumToInt(self.current_tok.ty)].infix;
      if (infix == null) {
        return self.errMsg(self.current_tok, "token found at an invalid infix position");
      }
      node = try infix.?(self, node, assignable);
    }
    return node;
  }

  fn literal(self: *Self, kind: lex.TokenType) !ast.LiteralNode {
    try self.consume(kind);
    return  ast.LiteralNode.init(self.previous_tok);
  }

  fn number(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const node = self.newNode();
    node.* = .{.AstNumber = try self.literal(.TkNumber)};
    var token = node.AstNumber.token;
    node.AstNumber.value = token.parseNum() catch b: {
      self.softErrMsg(token, "invalid number token");
      break :b 0;
    };
    return node;
  }

  fn string(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const node = self.newNode();
    node.* = .{.AstString = try self.literal(if (self.check(.TkString)) .TkString else .TkAllocString)};
    return node;
  }

  fn boolean(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    var ty: lex.TokenType = if (self.check(.TkTrue)) .TkTrue else .TkFalse;
    const node = self.newNode();
    node.* = .{.AstBool = try self.literal(ty)};
    return node;
  }

  fn unary(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok;
    const line_tok = self.current_tok;
    try self.advance();
    const expr = try self._parse(bp);
    const node = self.newNode();
    // rewrite -expr to 0 - expr
    if (op.ty.optype() == .OpSub) {
      const num = self.newNode();
      var lit = ast.LiteralNode.init(line_tok);
      lit.value = 0;
      num.* = .{.AstNumber = lit};
      node.* = .{.AstBinary = ast.BinaryNode.init(num, expr, op)};
      return node;
    } else if (op.ty.optype() == .OpAdd) {
      // rewrite +expr to expr
      return expr;
    }
    node.* = .{.AstUnary = ast.UnaryNode.init(expr, op)};
    return node;
  }

  fn binary(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok;
    try self.advance();
    const rhs = try self._parse(bp);
    const node = self.newNode();
    node.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, op)};
    return node;
  }

  fn binIs(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok;
    try self.advance();
    var is_not = self.match(.TkNot);
    var not_token = self.previous_tok;
    var rhs = try self._parse(bp);
    const node = self.newNode();
    // wrap nil literal as nil TypeNode
    if (rhs.isNilLiteral()) {
      var nil = Type.newConcrete(.TyNil).box(self.allocator);
      rhs.* = .{.AstNType = ast.TypeNode.init(nil, self.previous_tok)};
    }
    node.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, op)};    
    if (is_not) {
      var neg = self.newNode();
      not_token.ty = .TkExMark;
      neg.* = .{.AstUnary = ast.UnaryNode.init(node, not_token)};
      return neg;
    }
    return node;
  }

  fn casting(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    try self.consume(.TkAs);
    self.in_cast += 1;
    const rhs = try self.typing(false);
    const node = self.newNode();
    node.* = .{.AstCast = ast.CastNode.init(lhs, &rhs.AstNType)};
    self.in_cast -= 1;
    return node;
  }

  fn grouping(self: *Self, assignable: bool) !*Node {
    self.incNl();
    try self.consume(.TkLBracket);
    if (self.check(.TkRBracket)) {
      return try self.tupling(assignable, null);
    }
    const node = try self.parseExpr();
    if (self.match(.TkComma)) {
      return try self.tupling(assignable, node);
    }
    self.decNl();
    try self.consume(.TkRBracket);
    if (self.match(.TkExMark)) {
      var er = self.newNode();
      er.* = .{.AstError = ast.ErrorNode.init(node)};
      return er;
    }
    return node;
  }

  fn tupling(self: *Self, assignable: bool, first: ?*Node) !*Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstTuple = ast.ListNode.init(self.allocator)};
    var tuple = &node.*.AstTuple.elems;
    if (first) |elem| {
      tuple.append(elem);
    }
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      tuple.append(try self.parseExpr());
      if (!self.check(.TkRBracket)) {
        try self.consume(.TkComma);
      }
      try self.assertMaxElements(
        tuple.len(),
        "maximum number of tuple elements exceeded"
      );
    }
    self.decNl();
    try self.consume(.TkRBracket);
    return node;
  }

  fn listing(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstList = ast.ListNode.init(self.allocator)};
    self.incNl();
    try self.consume(.TkLSqrBracket);
    var list = &node.*.AstList.elems;
    while (!self.check(.TkEof) and !self.check(.TkRSqrBracket)) {
      if (list.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRSqrBracket)) {
          break;
        }
      }
      try self.assertMaxElements(
        list.len(),
        "maximum number of list elements exceeded"
      );
      list.append(try self.parseExpr());
    }
    self.decNl();
    try self.consume(.TkRSqrBracket);
    return node;
  }

  fn mapping(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstMap = ast.MapNode.init(self.allocator)};
    self.incNl();
    try self.consume(.TkLCurly);
    var pairs = &node.*.AstMap.pairs;
    const max_items: usize = MAX_LISTING_ELEMS / 2;
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (pairs.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRCurly)) {
          break;
        }
      }
      try self.assertMaxElements(
        pairs.len(),
        "maximum number of map elements exceeded"
      );
      if (pairs.len() > max_items) {
        self.softErrMsg(self.current_tok, "maximum number of map items exceeded");
      }
      var key = try self.parseExpr();
      try self.consume(.TkColon);
      var val = try self.parseExpr();
      pairs.append(.{.key = key, .value = val});
    }
    self.decNl();
    try self.consume(.TkRCurly);
    return node;
  }

  fn indexing(self: *Self, left: *Node, assignable: bool) !*Node {
    self.incNl();
    try self.consume(.TkLSqrBracket);
    var index = try self.parseExpr();
    self.decNl();
    try self.consume(.TkRSqrBracket);
    var node = self.newNode();
    node.* = .{.AstSubscript = ast.SubscriptNode.init(left, index)};
    return try self.handleAugAssign(node, assignable);
  }

  fn dotderef(self: *Self, left: *Node, assignable: bool) !*Node {
    // expr.?
    var token = self.current_tok;
    try self.consume(.TkDot);
    if (self.match(.TkQMark)) {
      var node = self.newNode();
      node.* = .{.AstDeref = ast.DerefNode.init(left, token)};
      return try self.handleAugAssign(node, assignable);
    } else {
      return self.dotExpr(left, assignable);
    }
  }

  fn dotExpr(self: *Self, left: *Node, assignable: bool) !*Node {
    if (!self.check(.TkIdent)) {
      self.softErrMsg(self.current_tok, "expected identifier after '.'");
    }
    const bp = ptable[@enumToInt(self.previous_tok.ty)].bp;
    const right = try self._parse(bp);
    var node = self.newNode();
    node.* = .{.AstDotAccess = ast.DotAccessNode.init(left, right)};
    return try self.handleAugAssign(node, assignable);
  }

  fn selfExpr(self: *Self, assignable: bool) !*Node {
    if (self.class == null) {
      self.softErrMsg(self.current_tok, "Use of 'self' outside class statement");
    }
    if (self.func == null) {
      self.softErrMsg(self.current_tok, "Use of 'self' outside method definition");
    }
    try self.consume(.TkSelf);
    var node = self.newNode();
    node.* = .{.AstVar = ast.VarNode.init(self.previous_tok)};
    return try self.handleAugAssign(node, assignable);
  }

  fn callExpr(self: *Self, left: *Node, assignable: bool) !*Node {
    _ = assignable;
    // CallExpr    :=  Expr TypeParams? "(" Params? ")"
    var targs: ?*NodeList = null;
    if (self.match(.TkLCurly)) {
      var list = NodeList.init(self.allocator);
      while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
        if (list.isNotEmpty()) try self.consume(.TkComma);
        try self.assertMaxTParams(list.len());
        list.append(try self.typing(false));
      }
      try self.assertNonEmptyTParams(list.len());
      try self.consume(.TkRCurly);
      targs = util.box(NodeList, list, self.allocator);
    }
    try self.consume(.TkLBracket);
    var args = NodeList.init(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (args.isNotEmpty()) try self.consume(.TkComma);
      args.append(try self.parseExpr());
    }
    try self.consume(.TkRBracket);
    try self.assertMaxArgs(args.len(), "arguments");
    var node = self.newNode();
    node.* = .{.AstCall = ast.CallNode.init(left, args, targs)};
    return node;
  }

  fn blockExpr(self: *Self) !*Node {
     _ = self.match(.TkNewline);
    var block = self.newNode();
    block.* = .{.AstBlock = ast.BlockNode.init(self.allocator, null)};
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      try self.addStatement(&block.AstBlock.nodes);
    }
    try self.consume(.TkEnd);
    return block;
  }

  fn tryExpr(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    if (self.func == null) {
      self.softErrMsg(
        self.current_tok,
        "use of 'try' expression in top-level code. Consider using 'orelse' instead."
      );
    }
    try self.consume(.TkTry);
    var ok = try self._parse(ptable[@enumToInt(self.previous_tok.ty)].bp);
    var token = lex.Token.tokenFrom(&self.previous_tok);
    token.ty = .TkAllocString;
    token.value = self.genName("e");
    var evar = self.newNode();
    evar.* = .{.AstVar = ast.VarNode.init(token)};
    var err_ = self.newNode();
    err_.* = .{.AstRet = ast.RetNode.init(evar, token)};
    var node = self.newNode();
    node.* = .{.AstOrElse = ast.OrElseNode.init(ok, err_, &evar.AstVar)};
    node.AstOrElse.from_try = true;
    return node;
  }

  fn orElseExpr(self: *Self, left: *Node, assignable: bool) !*Node {
    // expr orelse |e|? expr | (do .. end)
    _ = assignable;
    if (left.isOrElse()) {
      self.softErrMsg(
        self.previous_tok,
        "try/orelse expression should not be used in an 'orelse' expression"
      );
    }
    try self.consume(.TkOrElse);
    var evar: ?*ast.VarNode = null;
    if (self.match(.TkPipe)) {
      evar = &(try self.variable(false)).AstVar;
      try self.consume(.TkPipe);
    }
    var err_: *Node = undefined;
    if (self.match(.TkDo)) {
      err_ = try self.blockExpr();
    } else {
      err_ = try self.parseExpr();
    }
    var node = self.newNode();
    node.* = .{.AstOrElse = ast.OrElseNode.init(left, err_, evar)};
    return node;
  }

  fn handleAugAssign(self: *Self, left: *Node, assignable: bool) !*Node {
    // assignments are not expressions, but statements
    if (!assignable) return left;
    var node = left;
    if (self.current_tok.ty.isAssignLikeOp() and self.lexer.currentChar() == '=') {
      // +=, -=, ...=
      // e.g. var += expr => var = var + expr;
      var op_sign = self.current_tok;
      try self.advance(); // skip assignlike op
      var op_eq = self.current_tok;
      try self.advance(); // skip '=' op
      var value = try self.parseExpr();
      var right = self.newNode();
      right.* = .{.AstBinary = ast.BinaryNode.init(left, value, op_sign)};
      node = self.newNode();
      node.* = .{.AstAssign = ast.BinaryNode.init(
        if (left.isDeref()) left.AstDeref.expr else left,
        right, op_eq
      )};
      // only ascertain that a newline or eof is present, if not, error
      if (!self.check(.TkNewline) and !self.check(.TkEof)) try self.consumeNlOrEof();
      // if present, exprStmt() would consume it for us.
      return node;
    } else if (self.match(.TkEqual)) {
      var token = self.previous_tok;
      var value = try self.parseExpr();
      node = self.newNode();
      node.* = .{.AstAssign = ast.BinaryNode.init(
        if (left.isDeref()) left.AstDeref.expr else left,
        value, token
      )};
      // only ascertain that a newline or eof is present, if not, error
      if (!self.check(.TkNewline) and !self.check(.TkEof)) try self.consumeNlOrEof();
      // if present, exprStmt() would consume it for us.
      return node;
    } else {
      return node;
    }
  }

  fn variable(self: *Self, assignable: bool) !*Node {
    try self.consume(.TkIdent);
    var ident = self.previous_tok;
    var node = self.newNode();
    node.* = .{.AstVar = ast.VarNode.init(ident)};
    return try self.handleAugAssign(node, assignable);
  }

  fn nullable(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstNil = try self.literal(.TkNil)};
    return node;
  }

  inline fn assertMaxTParams(self: *Self, len: usize) !void {
    if (len >= types.MAX_TPARAMS) {
      self.softErrMsg(self.current_tok, "maximum type parameters exceeded");
    }
  }

  inline fn assertMaxArgs(self: *Self, len: usize, comptime d: []const u8) !void {
    if (len >= MAX_PARAMS) {
      self.softErrMsg(self.current_tok, "maximum " ++ d ++ " exceeded");
    }
  }

  inline fn assertNonEmptyTParams(self: *Self, len: usize) !void {
    if (len == 0) {
      self.softErrMsg(self.previous_tok, "empty type parameters are not supported");
    }
  }

  inline fn assertBuiltinExpTParams(self: *Self, cls: *Class, typ: *Type, tok: lex.Token) !void {
    var exp: usize = (
      if (typ.isListTy()) 1
      else if (typ.isMapTy()) 2
      else if (typ.isTupleTy()) 1
      else if (typ.isErrorTy()) 1
      else return
    );
    if (cls.tparamsLen() != exp) {
      self.softErrArgs(
        tok, "generic type instantiated with wrong number of paramters. "
        ++ "Expected {} but got {}",
        .{exp, cls.tparamsLen()}
      );
    }
  }

  inline fn assertUniqueTParams(self: *Self, alias: *Generic, param: *Type) !void {
    for (alias.getSlice()) |typ| {
      // `param` and `typ` have Variable.tokens equal to size 1.
      var token = param.variable().tokens.getLast();
      if (std.mem.eql(u8, typ.variable().tokens.getLast().value, token.value)) {
        return self.softErrMsg(token, "redefinition of type parameter");
      }
    }
  }

  fn aliasParam(self: *Self) !Type {
    var debug = self.current_tok;
    try self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator);
    typ.variable().append(debug);
    if (self.check(.TkDot)) {
      return self.errMsg(self.current_tok, "expected single identifier, found multiple");
    }
    return typ;
  }

  fn abstractTypeParams(self: *Self, typ: *Type) !Type {
    try self.consume(.TkLCurly);
    var gen = Generic.init(self.allocator, typ);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (gen.tparams.isNotEmpty()) try self.consume(.TkComma);
      try self.assertMaxTParams(gen.tparamsLen());
      var param = try self.aliasParam();
      try self.assertUniqueTParams(&gen, &param);
      gen.append(param.box(self.allocator));
    }
    try self.assertNonEmptyTParams(gen.tparamsLen());
    try self.consume(.TkRCurly);
    return gen.toType();
  }

  fn abstractType(self: *Self) !Type {
    // AbstractType   :=  TypeName
    // TypeName       :=  Ident AbsTypeParams?
    // AbsTypeParams  :=  "{" Ident ( "," Ident )* "}"
    try self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator);
    typ.variable().append(self.previous_tok);
    return (
      if (!self.check(.TkLCurly)) typ
      else try self.abstractTypeParams(typ.box(self.allocator))
    );
  }

  fn constantType(self: *Self) !Type {
    // Constant  := StringLiteral | BooleanLiteral | NumberLiteral
    var kind: TypeKind = switch (self.current_tok.ty) {
      .TkTrue, .TkFalse => .TyBool, 
      .TkNumber => .TyNumber,
      .TkString, .TkAllocString => .TyString,
      else => {
        return self.errMsg(self.current_tok, "invalid type-start");
      }
    };
    // direct 'unit' types such as listed above do not need names
    var typ = Type.newConstant(kind, self.current_tok.value);
    try self.advance();
    return typ;
  }

  fn builtinType(self: *Self) !Type {
    // handle builtin list/map/tuple/err type
    try self.advance();
    var ty = Type.newClass(self.previous_tok.value, self.allocator);
    ty.klass().builtin = true;
    return ty;
  }

  fn refType(self: *Self) !Type {
    try self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator);
    typ.variable().append(self.previous_tok);
    while (self.match(.TkDot)) {
      try self.consume(.TkIdent);
      typ.variable().append(self.previous_tok);
    }
    return typ;
  }

  fn builtinOrRefType(self: *Self) !Type {
    if (self.check(.TkIdent)) {
      return try self.refType();
    } else {
      return switch (self.current_tok.ty) {
        .TkList,
        .TkMap,
        .TkTuple,
        .TkErr => try self.builtinType(),
        else => self.errMsg(self.current_tok, "invalid type")
      };
    }
  }

  fn funType(self: *Self) !Type {
    // Function    :=  "fn" AbsTypeParams "(" Params? ")" ReturnSig
    try self.consume(.TkFn);
    var tparams: ?*TypeList = null;
    // Turn off support for user defined generic fn types (for now):
      // if (self.check(.TkLCurly)) {
      //   var tmp = try self.abstractTypeParams(undefined);
      //   tparams = util.box(TypeList, tmp.generic().tparams, self.allocator);
      // }
    try self.consume(.TkLBracket);
    var fun = Function.init(self.allocator, undefined, tparams, null);
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (fun.params.isNotEmpty()) try self.consume(.TkComma);
      fun.params.append((try self.tExpr()).box(self.allocator));
    }
    try self.consume(.TkRBracket);
    fun.ret = (try self.returnSig()).AstNType.typ;
    return fun.toType();
  }

  fn refGeneric(self: *Self, typ: *Type) !Type {
    var ret: Type = undefined;
    var gen: *Generic = undefined;
    if (typ.isGeneric()) {
      ret = typ.*;
    } else {
      var tmp = Generic.init(self.allocator, typ.box(self.allocator));
      ret = tmp.toType();
    }
    gen = ret.generic();
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (gen.tparams.isNotEmpty()) try self.consume(.TkComma);
      try self.assertMaxTParams(gen.tparamsLen());
      var param = try self.tExpr();
      gen.append(param.box(self.allocator));
    }
    try self.consume(.TkRCurly);
    try self.assertNonEmptyTParams(gen.tparamsLen());
    return ret;
  }

  fn builtinGeneric(self: *Self, typ: *Type) !void {
    var cls = typ.klass();
    @call(.always_inline, Class.initTParams, .{cls, self.allocator});
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (cls.tparamsLen() > 0) try self.consume(.TkComma);
      try self.assertMaxTParams(cls.tparamsLen());
      var param = try self.tExpr();
      cls.appendTParam(param.box(self.allocator));
    }
    try self.consume(.TkRCurly);
    try self.assertNonEmptyTParams(cls.tparamsLen());
    // check that builtin generic types are properly instantiated
    try self.assertBuiltinExpTParams(cls, typ, self.previous_tok);
  }

  fn tGeneric(self: *Self) ParseError!Type {
    // Generic     :=  ( Builtin | Reference ) ("{" Expression ( "," Expression )* "}")?
    var typ = try self.builtinOrRefType();
    if (self.match(.TkLCurly)) {
      if (typ.isClass()) {
        try self.builtinGeneric(&typ);
      } else {
        return try self.refGeneric(&typ);
      }
    }
    return typ;
  }

  fn tPrimary(self: *Self) ParseError!Type {
    // Primary  := ( Generic | Constant | Concrete | Function | “(“ Expression “)” ) "?"?
    var typ: Type = undefined;
    switch (self.current_tok.ty) {
      // Generic
      .TkIdent, .TkList, .TkMap, .TkTuple, .TkErr => {
        typ = try self.tGeneric();
      },
      .TkFn => {
        // Function
        typ = try self.funType();
      },
      .TkLBracket => {
        // “(“ Expression “)”
        try self.advance();
        typ = try self.tExpr();
        try self.consume(.TkRBracket);
      },
      .TkBool, .TkNum, .TkStr, .TkVoid, .TkNoReturn, .TkAny => |ty| {
          var tkind: TypeKind = switch (ty) {
          .TkBool => .TyBool,
          .TkNum => .TyNumber,
          .TkStr => .TyString,
          .TkVoid => .TyVoid,
          .TkNoReturn => .TyNoReturn,
          .TkAny => .TyAny,
          else => unreachable,
        };
        // direct 'unit' types such as listed above do not need names
        typ = Type.newConcrete(tkind);
        try self.advance();
      },
      else => {
        typ = try self.constantType();
      },
    }
    if (self.match(.TkQMark)) {
      typ = Type.newNullable(
        typ.box(self.allocator),
        self.allocator, null
      ).*;
    }
    return typ;
  }

  fn tUnion(self: *Self) !Type {
    // Union := Primary ( “|” Primary )*
    var typ = try self.tPrimary();
    if (self.check(.TkPipe)) {
      var uni = Union.init(self.allocator);
      uni.set(typ.box(self.allocator));
      while (self.match(.TkPipe)) {
        typ = try self.tPrimary();
        uni.set(typ.box(self.allocator));
      }
      return uni.toType();
    }
    return typ;
  }

  fn tExpr(self: *Self) !Type {
    // Expression := Union
    return try self.tUnion();
  }

  fn typing(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    var token = self.current_tok;
    var typ = try self.tExpr();
    var node = self.newNode();
    node.* = .{.AstNType = ast.TypeNode.init(typ.box(self.allocator), token)};
    return node;
  }

  fn checkGenericTParam(self: *Self, tvar: *Type, rhs_ty: *Type) ?*lex.Token {
    // T{K} -> P{Q | K{V}}, here K is tvar
    switch (rhs_ty.kind) {
      .Generic => |*gen| {
        switch (gen.base.kind) {
          .Variable => |*vr| {
            if (vr.eql(&tvar.kind.Variable)) {
              return &vr.tokens.items()[0];
            }
          },
          else => {}
        }
        for (gen.tparams.items()) |item| {
          if (self.checkGenericTParam(tvar, item)) |tok| {
            return tok;
          }
        }
      },
      .Class => |*cls| {
        for (cls.getSlice()) |item| {
          if (self.checkGenericTParam(tvar, item)) |tok| {
            return tok;
          }
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          if (self.checkGenericTParam(tvar, ty)) |tok| {
            return tok;
          }
        }
      },
      else => {},
    }
    return null;
  }

  fn assertNoGenericParameterTypeVariable(self: *Self, abs_ty: *Type, rhs_ty: *Type) !void {
    // Check that no type variable in generic params of the type alias is used "generically" 
    // in the aliasee
    switch (abs_ty.kind) {
      .Generic => |*gen| {
        for (gen.getSlice()) |param| {
          if (self.checkGenericTParam(param, rhs_ty)) |tok| {
            return self.softErrMsg(tok.*, "type variable in generic parameter cannot be generic");
          }
        }
      },
      else => {}
    }
  }

  fn assertNoDirectRecursiveAlias(self: *Self, abs_ty: *Type, rhs_ty: *Type) !void {
    // Check that type alias name is not used directly in the aliasee. This is not an in-depth
    // check, as it's possible for the alias to be meaningfully hidden in the aliasee.
    var lhs_ty = if (abs_ty.isGeneric()) abs_ty.generic().base else abs_ty;
    if (self.checkGenericTParam(lhs_ty, rhs_ty)) |tok| {
      return self.softErrMsg(tok.*, "type alias cannot be used directly in the aliasee");
    }
  }

  fn typeAlias(self: *Self) !*Node {
    // TypeAlias   := "type" AbstractType "=" ConcreteType
    var alias_typ = (try self.abstractType()).box(self.allocator);
    var alias = self.newNode();
    alias.* = .{.AstNType = ast.TypeNode.init(alias_typ, self.current_tok)};
    try self.consume(.TkEqual);
    var aliasee = try self.typing(false);
    var node = self.newNode();
    // check that generic type variable parameters in `AbstractType` are not generic in `ConcreteType`
    try self.assertNoGenericParameterTypeVariable(alias_typ, aliasee.AstNType.typ);
    // TODO: should this be disallowed? It poses no issues at the moment.
    // self.assertNoDirectRecursiveAlias(&alias_typ, &aliasee.AstNType.typ);
    node.* = .{.AstAlias = ast.AliasNode.init(&alias.AstNType, &aliasee.AstNType)};
    try self.consumeNlOrEof();
    return node;
  }

  fn annotation(self: *Self, ident: *ast.VarNode) !void {
    var typ_node = &(try self.typing(false)).AstNType;
    typ_node.from_alias_or_annotation = true;
    ident.typ = typ_node.typ;
  }

  fn parseExpr(self: *Self) !*Node {
    return try self._parse(.Assignment);
  }

  fn blockStmt(self: *Self, skip_do: bool, skip_nl: bool, add_scope: bool) anyerror!*Node {
    if (!skip_do) try self.consume(.TkDo);
    try self.consume(.TkNewline);
    var node = self.newNode();
    node.* = .{.AstBlock = ast.BlockNode.init(self.allocator, null)};
    if (add_scope) {
      // enter scope
      var tmp = self.newNode();
      tmp.* = .{.AstScope = ast.ScopeNode.init(true, false)};
      node.AstBlock.nodes.append(tmp);
    }
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      try self.addStatement(&node.AstBlock.nodes);
    }
    if (add_scope) {
      // leave scope
      var tmp = self.newNode();
      tmp.* = .{.AstScope = ast.ScopeNode.init(false, true)};
      node.AstBlock.nodes.append(tmp);
    }
    try self.consume(.TkEnd);
    // eat newline if present
    if (!skip_nl) {
      _ = self.match(.TkNewline);
    }
    return node;
  }

  fn varDecl(self: *Self) !*Node {
    // let var (: type)? = expr
    try self.consume(.TkIdent);
    var name = self.previous_tok;
    var ident = self.newNode();
    ident.* = .{.AstVar = ast.VarNode.init(name)};
    if (self.match(.TkColon)) {
      try self.annotation(&ident.*.AstVar);
    }
    try self.consume(.TkEqual);
    var val = try self.parseExpr();
    var decl = self.newNode();
    decl.* = .{.AstVarDecl = ast.VarDeclNode.init(&ident.AstVar, val, false)};
    try self.consumeNlOrEof();
    return decl;
  }

  fn ifStmt(self: *Self) anyerror!*Node {
    // if expr then? nl body (elif expr then? nl body)* else nl body end
    const cond = try self.parseExpr();
    _ = self.match(.TkThen);
    try self.consume(.TkNewline);
    var then = ast.BlockNode.init(self.allocator, cond);
    while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
      try self.addStatement(&then.nodes);
    }
    var elifs = NodeList.init(self.allocator);
    while (self.match(.TkElif)) {
      var elif_cond = try self.parseExpr();
      _ = self.match(.TkThen);
      try self.consume(.TkNewline);
      var elif_then = ast.BlockNode.init(self.allocator, elif_cond);
      while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
        try self.addStatement(&elif_then.nodes);
      }
      var elif_then_node = self.newNode();
      var elif_node = self.newNode();
      elif_then_node.* = .{.AstBlock = elif_then};
      elif_node.* = .{.AstElif = ast.ElifNode.init(elif_cond, elif_then_node)};
      elifs.append(elif_node);
    }
    var els = ast.BlockNode.init(self.allocator, cond);
    if (self.match(.TkElse)) {
      try self.consume(.TkNewline);
      while (!self.check(.TkEof) and !self.check(.TkEnd)) {
        try self.addStatement(&els.nodes);
      }
    }
    try self.consume(.TkEnd);
    try self.consumeNlOrEof();
    var then_node = self.newNode();
    var els_node = self.newNode();
    var node = self.newNode();
    then_node.* = .{.AstBlock = then};
    els_node.* = .{.AstBlock = els};
    node.* = .{.AstIf = ast.IfNode.init(cond, then_node, elifs, els_node)};
    return node;
  }

  fn controlStmt(self: *Self) !*Node {
    if (!self.inLoop()) {
      self.softErrMsg(self.current_tok, "control statement used outside loop");
    }
    if (!self.match(.TkBreak)) {
      try self.consume(.TkContinue);
    }
    var node = self.newNode();
    node.* = .{.AstControl = ast.ControlNode.init(self.previous_tok)};
    try self.consumeNlOrEof();
    return node;
  }

  fn whileStmt(self: *Self) !*Node {
    // while cond do? ... end
    self.incLoop();
    var cond = try self.parseExpr();
    var then = try self.blockStmt(!self.check(.TkDo), false, true);
    self.decLoop();
    var node = self.newNode();
    node.* = .{.AstWhile = ast.WhileNode.init(cond, then)};
    then.AstBlock.cond = cond;
    return node;
  }

  fn funParams(self: *Self, variadic: *bool) !ast.VarDeclList {
    // Params      :=  "(" Ident ":" Type ("," Ident ":" Type)* ")"
    var params = ast.VarDeclList.init(self.allocator);
    var disamb = std.StringHashMap(u32).init(self.allocator);
    if (self.match(.TkLBracket)) {
      while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
        if (params.isNotEmpty()) {
          try self.consume(.TkComma);
        }
        try self.consume(.TkIdent);
        var ident = ast.VarNode.init(self.previous_tok);
        if (disamb.get(ident.token.value)) |_| {
          self.softErrMsg(
            ident.token,
            "duplicate parameter is illegal in parameter list"
          );
        }
        disamb.put(ident.token.value, 0) catch {};
        if (!self.match(.TkStar)) {
          try self.consume(.TkColon);
          try self.annotation(&ident);
          params.append(ast.VarDeclNode.init(ident.box(self.allocator), undefined, true));
        } else {
          variadic.* = true;
          var tuple = Type.newClass("tuple", self.allocator).box(self.allocator);
          tuple.klass().builtin = true;
          tuple.klass().initTParams(self.allocator);
          try self.consume(.TkColon);
          try self.annotation(&ident);
          tuple.klass().appendTParam(ident.typ.?);
          ident.typ = tuple;
          params.append(ast.VarDeclNode.init(ident.box(self.allocator), undefined, true));
          if (!self.check(.TkRBracket)) {
            self.softErrMsg(
              self.current_tok,
              "variadic parameter should be last in a parameter list"
            );
          }
          break;
        }
      }
      try self.consume(.TkRBracket);
    }
    try self.assertMaxArgs(params.len(), "parameters");
    return params;
  }

  fn returnSig(self: *Self) !*Node {
    // ReturnSig   :=  ":" Type
    try self.consume(.TkColon);
    return try self.typing(false);
  }

  fn funStmt(self: *Self, lambda: bool) !*Node {
    // FunDecl     :=  "def" TypeParams? Params? ReturnSig? NL Body End
    try self.consume(.TkDef);
    var prev_func = self.func;
    var func = self.newNode();
    self.func = func;
    var ident: ?*ast.VarNode = null;
    if (!lambda) {
      try self.consume(.TkIdent);
      ident = ast.VarNode.init(self.previous_tok).box(self.allocator);
    }
    func.* = .{.AstFun = ast.FunNode {
      .params = undefined, .body = undefined,
      .name = ident, .ret = undefined,
      .tparams = undefined, .is_builtin = false,
      .variadic = false,
    }};
    var tparams: ?*TypeList = null;
    if (self.check(.TkLCurly)) {
      if (lambda) self.softErrMsg(self.current_tok, "generic lambdas are unsupported");
      var tmp = try self.abstractTypeParams(undefined);
      tparams = util.box(TypeList, tmp.generic().tparams, self.allocator);
    }
    var variadic = false;
    var params = try self.funParams(&variadic);
    var ret: ?*Node = null;
    if (self.check(.TkColon)) {
      ret = try self.returnSig();
    }
    var body = blk: {
      if (!lambda) break :blk try self.blockStmt(true, lambda, false);
      if (self.match(.TkEqGrt)) {
        var expr = try self.parseExpr();
        var block = self.newNode();
        block.* = .{.AstBlock = ast.BlockNode.init(self.allocator, null)};
        var rexp = self.newNode();
        rexp.* = .{.AstRet = ast.RetNode.init(expr, self.previous_tok)};
        block.AstBlock.nodes.append(rexp);
        break :blk block;
      } else {
        break :blk try self.blockExpr();
      }
    };
    self.func = prev_func;
    body.AstBlock.cond = func;
    func.* = .{.AstFun = ast.FunNode.init(params, body, ident, ret, tparams, false, variadic)};
    return func;
  }

  fn funExpr(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    return try self.funStmt(true);
  }

  fn returnStmt(self: *Self) !*Node {
    if (!self.inFun()) {
      self.softErrMsg(self.previous_tok, "return statement used outside function");
    }
    var node = self.newNode();
    var expr: ?*Node = null;
    if (!self.check(.TkNewline)) {
      expr = try self.parseExpr();
    }
    node.* = .{.AstRet = ast.RetNode.init(expr, self.previous_tok)};
    if (!self.func.?.AstFun.isAnonymous()) {
      try self.consumeNlOrEof();
    } else if (self.check(.TkNewline) or self.check(.TkEof)) {
      try self.consumeNlOrEof();
    }
    return node;
  }

  fn classTypeAnnotation(self: *Self) !?*Type {
    if (self.match(.TkColon)) {
      try self.consume(.TkIdent);
      var ty = Type.newVariable(self.allocator).box(self.allocator);
      ty.variable().append(self.previous_tok);
      if (self.check(.TkPipe)) {
        var tmp = Union.init(self.allocator);
        tmp.set(ty);
        while (self.match(.TkPipe)) {
          try self.consume(.TkIdent);
          var t = Type.newVariable(self.allocator).box(self.allocator);
          t.variable().append(self.previous_tok);
          tmp.set(t);
        }
        ty = tmp.toType().box(self.allocator);
      }
      return ty;
    }
    return null;
  }

  fn classStmt(self: *Self) !*Node {
    // ClassDecl           :=  "class" Ident TypeParams TypeAnnotation? ClassBody "end"
    var prev_cls = self.class;
    var cls = self.newNode();
    self.class = cls;
    switch (self.current_tok.ty) {
      .TkList, .TkErr, .TkTuple, .TkMap => try self.advance(),
      else => try self.consume(.TkIdent)
    }
    var ident = ast.VarNode.init(self.previous_tok).box(self.allocator);
    var tparams: ?*TypeList = null;
    if (self.check(.TkLCurly)) {
      var tmp = try self.abstractTypeParams(undefined);
      tparams = util.box(TypeList, tmp.generic().tparams, self.allocator);
    }
    var trait = try self.classTypeAnnotation();
    try self.consume(.TkNewline);
    while (self.match(.TkNewline)) {}
    // ClassBody
    // ClassFields
    var disamb = std.StringHashMap(lex.Token).init(self.allocator);
    var fields: *NodeList = util.box(NodeList, NodeList.init(self.allocator), self.allocator);
    if (self.check(.TkIdent)) {
      while (self.match(.TkIdent)) {
        if (disamb.get(self.previous_tok.value)) |tok| {
          self.softErrMsg(self.previous_tok, "illegal duplicate field");
          self.softErrMsg(tok, "field also declared here");
        }
        if (fields.len() > MAX_FIELDS) {
          self.softErrMsg(self.previous_tok, "maximum number of field declarations exceeded");
        }
        var id = ast.VarNode.init(self.previous_tok).box(self.allocator);
        var val: *Node = undefined;
        var has_default = false;
        if (self.match(.TkColon)) {
          try self.annotation(id);
        }
        if (self.match(.TkEqual)) {
          val = try self.parseExpr();
          has_default = true;
        }
        var field = @as(Node, .{.AstVarDecl = ast.VarDeclNode.init(id, val, false)}).box(self.allocator);
        field.AstVarDecl.is_field = true;
        field.AstVarDecl.has_default = has_default;
        fields.append(field);
        disamb.put(id.token.value, id.token) catch {};
        while (self.match(.TkNewline)) {}
      }
    }
    // ClassMethods
    var mdisamb = std.StringHashMap(lex.Token).init(self.allocator);
    var methods: *NodeList = util.box(NodeList, NodeList.init(self.allocator), self.allocator);
    if (self.check(.TkDef)) {
      while (self.check(.TkDef)) {
        var method = try self.funStmt(false);
        if (disamb.get(method.AstFun.name.?.token.value)) |tok| {
          self.softErrArgs(method.AstFun.name.?.token, "method conflicts with field '{s}'", .{tok.value});
          self.softErrMsg(tok, "field declared here");
        }
        if (mdisamb.get(method.AstFun.name.?.token.value)) |tok| {
          self.softErrMsg(method.AstFun.name.?.token, "illegal duplicate method");
          self.softErrMsg(tok, "method also declared here");
        }
        if (method.AstFun.isGeneric()) {
          self.softErrMsg(method.AstFun.name.?.token, "generic methods are unsupported");
        }
        if (methods.len() > MAX_METHODS) {
          self.softErrMsg(method.getToken(), "maximum number of method declarations exceeded");
        }
        methods.append(method);
        mdisamb.put(method.AstFun.name.?.token.value, method.AstFun.name.?.token) catch {};
        while (self.match(.TkNewline)) {}
      }
    }
    while (self.match(.TkNewline)) {}
    try self.consume(.TkEnd);
    try self.consumeNlOrEof();
    self.class = prev_cls;
    cls.* = .{.AstClass = ast.ClassNode.init(ident, tparams, trait, fields, methods, false, false)};
    return cls;
  }

  fn exprStmt(self: *Self) !*Node {
    const expr = try self.parseExpr();
    const node = self.newNode();
    node.* = .{.AstExprStmt = ast.ExprStmtNode.init(expr)};
    try self.consumeNlOrEof();
    return node;
  }

  fn emptyStmt(self: *Self) *Node {
    return ast.BlockNode.newEmptyBlock(self.allocator, null);
  }

  fn recover(self: *Self) void {
    if (self.check(.TkNewline)) {
      self.advance() catch {};
      return;
    }
    while (!self.check(.TkEof) and !self.match(.TkNewline)) {
      self.advance() catch {};
    }
  }

  fn addStatement(self: *Self, list: *NodeList) anyerror!void {
    var stmt = self.statement() catch |e| {
      if (e != error.EmptyStatement) {
        return e;
      }
      return;
    };
    list.append(stmt);
  }

  fn statement(self: *Self) !*Node {
    if (self.match(.TkLet)) {
      return try self.varDecl();
    } else if (self.match(.TkType)) {
      return try self.typeAlias();
    } else if (self.check(.TkDo)) {
      return try self.blockStmt(false, false, true);
    } else if (self.match(.TkNewline)) {
      return error.EmptyStatement;
    } else if (self.match(.TkIf)) {
      return try self.ifStmt();
    } else if (self.match(.TkWhile)) {
      return try self.whileStmt();
    } else if (self.check(.TkBreak) or self.check(.TkContinue)) {
      return try self.controlStmt();
    } else if (self.check(.TkDef)) {
      return try self.funStmt(false);
    } else if (self.match(.TkReturn)) {
      return try self.returnStmt();
    } else if (self.match(.TkClass)) {
      return try self.classStmt();
    }
    return try self.exprStmt();
  }

  pub fn parse(self: *Self, display_diag: bool) !*Node {
    self.advance() catch {};
    var program = self.newNode();
    program.* = .{.AstProgram = ast.ProgramNode.init(self.allocator)};
    while (!self.match(.TkEof)) {
      self.addStatement(&program.AstProgram.decls) catch self.recover();
    }
    if (self.diag.hasAny()) {
      var has_error = self.diag.hasErrors();
      if (display_diag) self.diag.display();
      if (has_error) {
        program.AstProgram.decls.clearRetainingCapacity();
        return error.ParseError;
      }
    }
    return program;
  }
};
