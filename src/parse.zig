const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const ks = @import("constants.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const ptn = @import("pattern.zig");
const diagnostics = @import("diagnostics.zig");
const VebAllocator = @import("allocator.zig");

const ds = ast.ds;
const Token = lex.Token;
const Node = ast.AstNode;
const exit = std.os.exit;
const Type = types.Type;
const Generic = types.Generic;
const Union = types.Union;
const Variable = types.Variable;
const Concrete = types.Concrete;
const Function = types.Function;
const Class = types.Class;
const TypeList = types.TypeList;
const NodeList = ast.AstList;
const Diagnostic = diagnostics.Diagnostic;
const Pattern = ptn.Pattern;
pub const TypeKind = types.TypeKind;

/// maximum number of elements of a list literal 
const MAX_LISTING_ELEMS = 0xff;
/// maximum number of callable parameters
const MAX_PARAMS = 0xff;
/// maximum number of class fields
const MAX_FIELDS = 0xc8;
/// maximum number of class methods
const MAX_METHODS = 0xc8;

pub const Parser = struct {
  current_tok: Token,
  previous_tok: Token,
  lexer: lex.Lexer,
  allocator: std.mem.Allocator,
  diag: Diagnostic,
  meta: ParseMeta,
  namegen: util.NameGen,

  pub const ParseMode = enum {
    Builtin,
    User,
  };

  const ParseMeta = struct {
    casts: u32 = 0,
    loops: u32 = 0,
    allow_nl: usize = 0,
    func: ?*Node = null,
    class: ?*Node = null,
    m_literals: ds.ArrayList(NameTuple),
    mode: ParseMode,

    const NameTuple = struct{*Node, *Node, bool};

    fn init(al: std.mem.Allocator, mode: ParseMode) @This() {
      return @This(){.m_literals = ds.ArrayList(NameTuple).init(al), .mode = mode};
    }
  };

  const ExprParseTable = struct {
    bp: BindingPower,
    prefix: ?PrefixFn,
    infix: ?InfixFn
  };

  const Self = @This();
  const ParseError = error{ParseError};
  const PrefixFn = *const fn (*Self, bool) anyerror!*Node;
  const InfixFn = *const fn (*Self, *Node, bool) anyerror!*Node;

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
    .{.bp = .None, .prefix = null, .infix = null},                      // Tk2Dot
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
    .{.bp = .None, .prefix = null, .infix = null},                      // TkCase
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkTrue
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkVoid
    .{.bp = .None, .prefix = Self.selfExpr, .infix = null},             // TkSelf
    .{.bp = .None, .prefix = null, .infix = null},                      // TkWith
    .{.bp = .None, .prefix = null, .infix = null},                      // TkClass
    .{.bp = .None, .prefix = null, .infix = null},                      // TkBreak
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkFalse
    .{.bp = .None, .prefix = null, .infix = null},                      // TkMatch
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
      .namegen = util.NameGen.init(al),
      .meta = ParseMeta.init(al, .User),
      // use the arena allocator for allocating general nodes.
      .allocator = al,
    };
  }

  pub inline fn setParseMode(self: *Self, mode: ParseMode) void {
    self.meta.mode = mode;
  }

  inline fn inBuiltinMode(self: *Self) bool {
    return self.meta.mode == .Builtin;
  }

  inline fn _errWithArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnosticsWithLevel(.DiagError, token, "Error: " ++ fmt, args);
  }

  fn softErrArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
   self._errWithArgs(token, fmt, args);
  }

  fn softErrMsg(self: *Self, token: Token, msg: []const u8) void {
    self._errWithArgs(token, "{s}", .{msg});
  }

  fn errMsg(self: *Self, token: Token, msg: []const u8) ParseError {
    self._errWithArgs(token, "{s}", .{msg});
    return error.ParseError;
  }

  fn errArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) ParseError {
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

  inline fn skipNewlines(self: *Self) void {
    while (self.match(.TkNewline)) {}
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
    self.meta.allow_nl += 1;
    self.lexer.allow_nl = self.meta.allow_nl;
  }

  inline fn decNl(self: *Self) void {
    self.meta.allow_nl -= 1;
    self.lexer.allow_nl = self.meta.allow_nl;
  }

  inline fn incLoop(self: *Self) void {
    self.meta.loops += 1;
  }

  inline fn decLoop(self: *Self) void {
    self.meta.loops -= 1;
  }

  inline fn incCast(self: *Self) void {
    self.meta.casts += 1;
  }

  inline fn decCast(self: *Self) void {
    self.meta.casts -= 1;
  }

  inline fn inLoop(self: *Self) bool {
    return self.meta.loops > 0;
  }

  inline fn inFun(self: *Self) bool {
    return self.meta.func != null;
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

  inline fn createObj(self: *Self, comptime T: type) *T {
    return util.alloc(T, self.allocator);
  }

  inline fn newNode(self: *Self) *Node {
    return self.createObj(Node);
  }

  inline fn genName(self: *Self, start: []const u8) []const u8 {
    return self.namegen.generate("$.{s}", .{start});
  }

  fn _parse(self: *Self, bp: BindingPower) !*Node {
    const prefix = ptable[@intFromEnum(self.current_tok.ty)].prefix;
    if (prefix == null) {
      return self.errMsg(self.current_tok, "token found at an invalid prefix position");
    }
    const bp_val = @intFromEnum(bp);
    const assignable = bp_val <= @intFromEnum(BindingPower.Assignment);
    var node = try prefix.?(self, assignable);
    while (bp_val < @intFromEnum(ptable[@intFromEnum(self.current_tok.ty)].bp)) {
      var infix = ptable[@intFromEnum(self.current_tok.ty)].infix;
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
    const bp = ptable[@intFromEnum(self.current_tok.ty)].bp;
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
    const bp = ptable[@intFromEnum(self.current_tok.ty)].bp;
    const op = self.current_tok;
    try self.advance();
    const rhs = try self._parse(bp);
    const node = self.newNode();
    node.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, op)};
    return node;
  }

  fn binIs(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    const op = self.current_tok;
    try self.advance();
    var is_not = self.match(.TkNot);
    var not_token = self.previous_tok;
    var rhs = try self.typing(false);
    const node = self.newNode();
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
    self.incCast();
    defer self.decCast();
    const rhs = try self.typing(false);
    const node = self.newNode();
    node.* = .{.AstCast = ast.CastNode.init(lhs, &rhs.AstNType)};
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
        if (self.check(.TkRBracket)) break;
      }
    }
    self.decNl();
    self.assertMaxElements(tuple.len(), "tuple elements");
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
      list.append(try self.parseExpr());
    }
    self.decNl();
    self.assertMaxElements(list.len(), "list elements");
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
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (pairs.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRCurly)) {
          break;
        }
      }
      var key = try self.parseExpr();
      try self.consume(.TkColon);
      var val = try self.parseExpr();
      pairs.append(.{.key = key, .value = val});
    }
    self.decNl();
    self.assertMaxElements(pairs.len() << 1, "map elements");
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
    const bp = ptable[@intFromEnum(self.previous_tok.ty)].bp;
    const right = try self._parse(bp);
    var node = self.newNode();
    node.* = .{.AstDotAccess = ast.DotAccessNode.init(left, right)};
    return try self.handleAugAssign(node, assignable);
  }

  fn selfExpr(self: *Self, assignable: bool) !*Node {
    if (self.meta.class == null) {
      self.softErrMsg(self.current_tok, "Use of 'self' outside class statement");
    }
    if (self.meta.func == null) {
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
      targs = try self.typeParams();
    }
    try self.consume(.TkLBracket);
    var args = NodeList.init(self.allocator);
    var start: ast.Token = undefined;
    var labeled = false;
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (args.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRBracket)) break;
      }
      start = self.current_tok;
      var arg = try self.parseExpr(); 
      if (self.match(.TkColon)) {
        labeled = true;
        if (start.ty != .TkIdent) {
          self.softErrMsg(self.previous_tok, "invalid labeled argument");
        }
        var val = try self.parseExpr();
        var tmp = self.newNode();
        tmp.* = .{.AstLblArg = ast.LblArgNode.init(start, val, arg)};
        args.append(tmp);
      } else {
        args.append(arg);
      }
    }
    try self.consume(.TkRBracket);
    self.assertMaxArgs(args.len(), "arguments");
    var node = self.newNode();
    node.* = .{.AstCall = ast.CallNode.init(left, args, targs, 0, false, labeled)};
    return node;
  }

  fn blockExpr(self: *Self) !*Node {
     _ = self.match(.TkNewline);
    var block = self.newNode();
    block.* = .{.AstBlock = ast.BlockNode.init(self.allocator)};
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      try self.addStatement(&block.block().nodes);
    }
    try self.consume(.TkEnd);
    return block;
  }

  fn tryExpr(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    if (self.meta.func == null) {
      self.softErrMsg(
        self.current_tok,
        "use of 'try' expression in top-level code. Consider using 'orelse' instead."
      );
    }
    try self.consume(.TkTry);
    var ok = try self._parse(ptable[@intFromEnum(self.previous_tok.ty)].bp);
    const token = Token.fromWithValue(&self.previous_tok, self.genName("e"), .TkAllocString);
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

  inline fn assertMaxTParams(self: *Self, len: usize) void {
    if (len >= types.MAX_TPARAMS) {
      self.softErrMsg(self.current_tok, "maximum type parameters exceeded");
    }
  }

  inline fn assertMaxElements(self: *Self, len: usize, comptime msg: []const u8) void {
    if (len > MAX_LISTING_ELEMS) {
      self.softErrMsg(self.current_tok, "maximum number of " ++ msg ++ " exceeded");
    }
  }

  inline fn assertMaxArgs(self: *Self, len: usize, comptime d: []const u8) void {
    if (len >= MAX_PARAMS) {
      self.softErrMsg(self.current_tok, "maximum " ++ d ++ " exceeded");
    }
  }

  inline fn assertNonEmptyTParams(self: *Self, len: usize) void {
    if (len == 0) {
      self.softErrMsg(self.previous_tok, "empty type parameters are not supported");
    }
  }

  inline fn assertBuiltinExpTParams(self: *Self, cls: *Class, typ: *Type, tok: Token) void {
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

  inline fn assertUniqueTParams(self: *Self, alias: *Generic, param: *Type) void {
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

  fn typeParams(self: *Self) !*NodeList {
    // assumes previously consumed token is TkLCurly
    var list = NodeList.init(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (list.isNotEmpty()) try self.consume(.TkComma);
      list.append(try self.typing(false));
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(list.len());
    self.assertMaxTParams(list.len());
    return list.box();
  }

  fn abstractTypeParams(self: *Self, typ: *Type) !Type {
    try self.consume(.TkLCurly);
    var gen = Generic.init(self.allocator, typ);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (gen.tparams.isNotEmpty()) try self.consume(.TkComma);
      var param = try self.aliasParam();
      self.assertUniqueTParams(&gen, &param);
      gen.append(param.box(self.allocator));
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(gen.tparamsLen());
    self.assertMaxTParams(gen.tparamsLen());
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
    // handle builtin list/map/tuple/err/str type
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
      var param = try self.tExpr();
      gen.append(param.box(self.allocator));
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(gen.tparamsLen());
    self.assertMaxTParams(gen.tparamsLen());
    return ret;
  }

  fn builtinGeneric(self: *Self, typ: *Type) !void {
    var cls = typ.klass();
    @call(.always_inline, Class.initTParams, .{cls, self.allocator});
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (cls.tparamsLen() > 0) try self.consume(.TkComma);
      var param = try self.tExpr();
      cls.appendTParam(param.box(self.allocator));
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(cls.tparamsLen());
    self.assertMaxTParams(cls.tparamsLen());
    // check that builtin generic types are properly instantiated
    self.assertBuiltinExpTParams(cls, typ, self.previous_tok);
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
      // Function
      .TkFn => {
        typ = try self.funType();
      },
      // “(“ Expression “)”
      .TkLBracket => {
        try self.advance();
        typ = try self.tExpr();
        try self.consume(.TkRBracket);
      },
      // Concrete
      .TkBool, .TkNum, .TkStr, .TkVoid, .TkNoReturn, .TkAny, .TkNil => |ty| {
          var tkind: TypeKind = switch (ty) {
          .TkBool => .TyBool,
          .TkNum => .TyNumber,
          .TkStr => .TyString,
          .TkVoid => .TyVoid,
          .TkNoReturn => .TyNoReturn,
          .TkAny => .TyAny,
          .TkNil => .TyNil,
          else => unreachable,
        };
        // direct 'unit' types such as listed above do not need names
        typ = Type.newConcrete(tkind);
        try self.advance();
      },
      // Constant
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

  fn checkGenericTParam(self: *Self, tvar: *Type, rhs_ty: *Type) ?*Token {
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

  fn assertNoGenericParameterTypeVariable(self: *Self, abs_ty: *Type, rhs_ty: *Type) void {
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

  fn assertNoDirectRecursiveAlias(self: *Self, abs_ty: *Type, rhs_ty: *Type) void {
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
    self.assertNoGenericParameterTypeVariable(alias_typ, aliasee.AstNType.typ);
    // TODO: should this be disallowed? It poses no issues at the moment.
    // self.assertNoDirectRecursiveAlias(&alias_typ, &aliasee.AstNType.typ);
    node.* = .{.AstAlias = ast.AliasNode.init(&alias.AstNType, &aliasee.AstNType)};
    try self.consumeNlOrEof();
    return node;
  }

  fn annotation(self: *Self, ident: *ast.VarNode) !void {
    ident.typ = (try self.typing(false)).AstNType.typ;
  }

  fn parseExpr(self: *Self) !*Node {
    return try self._parse(.Assignment);
  }

  fn blockStmt(self: *Self, skip_do: bool, skip_nl: bool, add_scope: bool) anyerror!*Node {
    if (!skip_do) try self.consume(.TkDo);
    try self.consume(.TkNewline);
    var node = self.newNode();
    node.* = .{.AstBlock = ast.BlockNode.init(self.allocator)};
    if (add_scope) {
      // enter scope
      var tmp = self.newNode();
      tmp.* = .{.AstScope = ast.ScopeNode.init(true, false)};
      node.block().nodes.append(tmp);
    }
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      try self.addStatement(&node.block().nodes);
    }
    if (add_scope) {
      // leave scope
      var tmp = self.newNode();
      tmp.* = .{.AstScope = ast.ScopeNode.init(false, true)};
      node.block().nodes.append(tmp);
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
    if (ident.AstVar.typ) |_| {
      decl.AstVarDecl.has_annotation = true;
    }
    try self.consumeNlOrEof();
    return decl;
  }

  fn ifStmt(self: *Self) anyerror!*Node {
    // if expr then? nl body (elif expr then? nl body)* else nl body end
    const cond = try self.parseExpr();
    _ = self.match(.TkThen);
    try self.consume(.TkNewline);
    var then = ast.BlockNode.init(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
      try self.addStatement(&then.nodes);
    }
    var elifs = NodeList.init(self.allocator);
    while (self.match(.TkElif)) {
      var elif_cond = try self.parseExpr();
      _ = self.match(.TkThen);
      try self.consume(.TkNewline);
      var elif_then = ast.BlockNode.init(self.allocator);
      while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
        try self.addStatement(&elif_then.nodes);
      }
      var elif_then_node = self.newNode();
      var elif_node = self.newNode();
      elif_then_node.* = .{.AstBlock = elif_then};
      elif_node.* = .{.AstElif = ast.ElifNode.init(elif_cond, elif_then_node)};
      elifs.append(elif_node);
    }
    var els = ast.BlockNode.init(self.allocator);
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
    defer self.decLoop();
    const cond = try self.parseExpr();
    var then = try self.blockStmt(!self.check(.TkDo), false, false);
    var node = self.newNode();
    node.* = .{.AstWhile = ast.WhileNode.init(cond, then)};
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
          var tuple = Type.newClass(ks.TupleVar, self.allocator).box(self.allocator);
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
    self.assertMaxArgs(params.len(), "parameters");
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
    var prev_func = self.meta.func;
    var func = self.newNode();
    self.meta.func = func;
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
      tparams = tmp.generic().tparams.box();
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
        block.* = .{.AstBlock = ast.BlockNode.init(self.allocator)};
        var rexp = self.newNode();
        rexp.* = .{.AstRet = ast.RetNode.init(expr, self.previous_tok)};
        block.block().nodes.append(rexp);
        break :blk block;
      } else {
        break :blk try self.blockExpr();
      }
    };
    self.meta.func = prev_func;
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
    if (!self.meta.func.?.AstFun.isAnonymous()) {
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
    var prev_cls = self.meta.class;
    defer self.meta.class = prev_cls;
    var cls = self.newNode();
    self.meta.class = cls;
    switch (self.current_tok.ty) {
      .TkList, .TkErr, .TkTuple, .TkMap, .TkStr => if (self.inBuiltinMode()) try self.advance() else try self.consume(.TkIdent),
      else => try self.consume(.TkIdent)
    }
    var ident = ast.VarNode.init(self.previous_tok).box(self.allocator);
    var tparams: ?*TypeList = null;
    if (self.check(.TkLCurly)) {
      var tmp = try self.abstractTypeParams(undefined);
      tparams = tmp.generic().tparams.box();
    }
    var trait = try self.classTypeAnnotation();
    try self.consume(.TkNewline);
    self.skipNewlines();
    // ClassBody
    // ClassFields
    var disamb = std.StringHashMap(Token).init(self.allocator);
    var fields: *NodeList = NodeList.init(self.allocator).box();
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
        self.skipNewlines();
      }
    }
    // ClassMethods
    var mdisamb = std.StringHashMap(Token).init(self.allocator);
    var methods: *NodeList = NodeList.init(self.allocator).box();
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
        self.skipNewlines();
      }
    }
    self.skipNewlines();
    try self.consume(.TkEnd);
    try self.consumeNlOrEof();
    cls.* = .{.AstClass = ast.ClassNode.init(ident, tparams, trait, fields, methods, false, false)};
    return cls;
  }

  inline fn isWildcardPtn(self: *Self, chars: []const u8) bool {
    _ = self;
    return std.mem.eql(u8, chars, "_");
  }

  inline fn errIfWildcard(self: *Self, node: *Node, msg: []const u8) void {
    if (self.isWildcardPtn(node.AstVar.token.value)) {
      self.softErrMsg(node.AstVar.token, msg);
    }
  }

  inline fn literalCons(self: *Self, node: *Node, token: ast.Token) !*Pattern {
    // wrap up a literal pattern into a literal constructor pattern
    const cons = ptn.Constructor.newLiteralCons(node, self.allocator);
    return Pattern.init(cons.toVariant(self.allocator), token, .{}).box(self.allocator);
  }

  inline fn _finishPossibleRestPattern(self: *Self, cons: ptn.Constructor, token: Token) *Pattern {
    return Pattern.init(cons.toVariant(self.allocator), token, .{}).box(self.allocator);
  }

  fn _restPattern(self: *Self, cons: *ptn.Constructor) bool {
    // rest_pattern := '..'
    if (self.match(.Tk2Dot)) {
      cons.rested = true;
      _ = self.match(.TkComma);
      return true;
    }
    return false;
  }

  fn _numberPattern(self: *Self) !*Pattern {
    const sub = self.match(.TkMinus);
    const token = self.current_tok;
    var num = try self.number(false);
    num.AstNumber.value = if (!sub) num.AstNumber.value else -num.AstNumber.value;
    return self.literalCons(num, token);
  }

  fn _numberOrRangePattern(self: *Self) !*Pattern {
    const token = self.current_tok;
    const p1 = try self._numberPattern();
    if (self.match(.Tk2Dot)) {
      //  low..high -> a if a >= low and a <= high
      const lhs = p1.variant.cons.node.?;
      const p2 = try self._numberPattern();
      const rhs = p2.variant.cons.node.?;
      var id = self.newNode();
      id.* = .{.AstVar = ast.VarNode.init(Token.fromWithValue(&token, self.genName("id"), .TkIdent))};
      // create if a >= low and a <= high
      const op_leq = Token.fromWithValue(&token, "<=", .TkLeq);
      const op_geq = Token.fromWithValue(&token, ">=", .TkGeq);
      const op_and = Token.fromWithValue(&token, "and", .TkAnd);
      var bin_lhs = self.newNode();
      bin_lhs.* = .{.AstBinary = ast.BinaryNode.init(id, lhs, op_geq)};
      var bin_rhs = self.newNode();
      bin_rhs.* = .{.AstBinary = ast.BinaryNode.init(id, rhs, op_leq)};
      var bin = self.newNode();
      bin.* = .{.AstBinary = ast.BinaryNode.init(bin_lhs, bin_rhs, op_and)};
      self.meta.m_literals.append(.{id, bin, true});
      return Pattern.init(ptn.Variable.init(id).toVariant(self.allocator), token, .{}).box(self.allocator);
    }
    return p1;
  }

  fn _groupOrSeqPattern(self: *Self) !*Pattern {
    // group_pattern | sequence_pattern
    var token = self.current_tok;
    if (self.match(.TkLBracket)) {
      // sequence_pattern (empty)
      if (self.match(.TkRBracket)) {
        return (
          Pattern.init(
            ptn.Constructor.newTupleCons(self.allocator).toVariant(self.allocator),
            token,
            .{}
          ).box(self.allocator)
        );
      }
      // sequence_pattern (non-empty rest)
      if (self.check(.Tk2Dot)) {
        var cons = ptn.Constructor.newTupleCons(self.allocator);
        _ = self._restPattern(&cons);
        try self.consume(.TkRBracket);
        return self._finishPossibleRestPattern(cons, token);
      }
      // group_pattern
      var pat = try self._pattern();
      if (self.match(.TkRBracket)) {
        // error_pattern: (pat)!
        if (self.match(.TkExMark)) {
          var cons = ptn.Constructor.newErrCons(self.allocator);
          cons.args.append(pat);
          return Pattern.init(cons.toVariant(self.allocator), token, .{}).box(self.allocator);
        }
        return pat;
      }
      // sequence_pattern (non-empty)
      var cons = ptn.Constructor.newTupleCons(self.allocator);
      cons.args.append(pat);
      while (self.match(.TkComma)) {
        if (self._restPattern(&cons)) break;
        if (self.check(.TkRBracket)) break;
        cons.args.append(try self._pattern());
      }
      self.assertMaxArgs(cons.args.len(), "sequence patterns");
      try self.consume(.TkRBracket);
      return self._finishPossibleRestPattern(cons, token);
    } else {
      token = self.current_tok;
      try self.consume(.TkLSqrBracket);
      // sequence_pattern (empty)
      if (self.match(.TkRSqrBracket)) {
        return (
          Pattern.init(
            ptn.Constructor.newListCons(self.allocator).toVariant(self.allocator),
            token,
            .{}
          ).box(self.allocator)
        );
      }
      // sequence_pattern (non-empty)
      var cons = ptn.Constructor.newListCons(self.allocator);
      while (!self.check(.TkEof) and !self.check(.TkRSqrBracket)) {
        if (cons.args.isNotEmpty()) {
          try self.consume(.TkComma);
          if (self.check(.TkRSqrBracket)) break;
        }
        if (self._restPattern(&cons)) break;
        cons.args.append(try self._pattern());
      }
      self.assertMaxArgs(cons.args.len(), "sequence patterns");
      try self.consume(.TkRSqrBracket);
      return self._finishPossibleRestPattern(cons, token);
    }
  }

  fn _capturePattern(self: *Self) !*Node {
    // capture_pattern: !"_" NAME !('.' | '(' | '=') 
    var res = try self.variable(false);
    self.errIfWildcard(res, "cannot use wildcard in capture pattern");
    return res;
  }

  fn _mappingPattern(self: *Self) !*Pattern {
    // '{' has been skipped
    const token = self.previous_tok;
    if (self.match(.TkRCurly)) {
      var map = Pattern.init(
        ptn.Constructor.newMapCons(self.allocator).toVariant(self.allocator),
        token, .{}
      ).box(self.allocator);
      const list = ptn.Constructor.newListCons(self.allocator);
      map.variant.cons.args.append(Pattern.init(list.toVariant(self.allocator), token, .{}).box(self.allocator));
      return map;
    }
    var list = ptn.Constructor.newListCons(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (list.args.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRCurly)) break;
      }
      if (self._restPattern(&list)) break;
      // key_value_pattern: | (literal_pattern | constant_pattern) ':' pattern
      var tok = self.current_tok;
      var key: *Pattern = switch (tok.ty) {
        // literal_pattern
        .TkNumber, .TkNil, .TkTrue,
        .TkFalse, .TkString, .TkMinus, => try self._pattern(),
        // constant_pattern := attr := name_or_attr
        .TkIdent => (
            Pattern.init( // use capture pattern for now
              ptn.Variable.init(try self._capturePattern()).toVariant(self.allocator),
              tok,
              .{}
            ).box(self.allocator)
          ),
        else => blk: {
          self.softErrMsg(self.current_tok, "invalid mapping pattern");
          break :blk try self._pattern();
        }
      };
      try self.consume(.TkColon);
      list.args.appendSlice(&[_]*Pattern{key, try self._pattern()});
    }
    self.assertMaxArgs(list.args.len(), "mapping patterns");
    try self.consume(.TkRCurly);
    var map = ptn.Constructor.newMapCons(self.allocator);
    // {lit_ptn: ptn} ->
    // Map(List(Literal(...), Pattern))
    map.args.append(Pattern.init(list.toVariant(self.allocator), token, .{}).box(self.allocator));
    return Pattern.init(map.toVariant(self.allocator), token, .{}).box(self.allocator);
  }

  fn _classPattern(self: *Self) !*Pattern {
    // capture_pattern  | wildcard_pattern | class_pattern
    // class_pattern: name_or_attr ('{' Type Params '}')? '(' [pattern_arguments ','?] ')'
    if (self.isWildcardPtn(self.current_tok.value)) {
      try self.advance();
      return (
        Pattern.init(
          ptn.Wildcard.init(self.previous_tok, false).toVariant(self.allocator),
          self.previous_tok,
          .{}
        ).box(self.allocator)
      );
    }
    var id = try self.variable(false);
    if (!self.check(.TkLBracket) and !self.check(.TkLCurly)) {
      // at this point, this is a capture_pattern
      return (
        Pattern.init(
          ptn.Variable.init(id).toVariant(self.allocator),
          id.AstVar.token,
          .{}
        ).box(self.allocator)
      );
    }
    var targs: ?*NodeList = null;
    if (self.match(.TkLCurly)) {
      targs = try self.typeParams();
    }
    try self.consume(.TkLBracket);
    var cons = ptn.Constructor.newClassCons(id.AstVar.token.value, id, self.allocator);
    cons.targs = targs;
    var start: Token = undefined;
    var disamb = std.StringHashMap(u32).init(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (cons.args.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRBracket)) break;
      }
      if (self._restPattern(&cons)) break;
      start = self.current_tok;
      var arg = try self._pattern();
      if (self.match(.TkEqual)) {
        if (!arg.isVariable()) {
          self.softErrMsg(start, "invalid field pattern");
          _ = try self._pattern(); // skip the ptn after '='
          continue;
        }
        var val = try self._pattern();
        if (disamb.get(arg.variant.vari.ident.AstVar.token.value)) |_| {
          self.softErrMsg(start, "duplicate field pattern");
        } else {
          disamb.put(arg.variant.vari.ident.AstVar.token.value, 1) catch {};
        }
        val.alat.field = arg.variant.vari.ident;
        cons.args.append(val);
      } else {
        cons.args.append(arg);
      }
    }
    self.assertMaxArgs(cons.args.len(), "arguments");
    try self.consume(.TkRBracket);
    return self._finishPossibleRestPattern(cons, id.AstVar.token);
  }

  fn _closedPattern(self: *Self) !*Pattern {
    //    closed_pattern: | literal_pattern | capture_pattern | wildcard_pattern 
    //                    | value_pattern | group_pattern | sequence_pattern 
    //                    | mapping_pattern | class_pattern
    // literal_pattern
    var token = self.current_tok;
    switch (self.current_tok.ty) {
      .TkNil => return self.literalCons(try self.nullable(false), token),
      .TkTrue, .TkFalse => return self.literalCons(try self.boolean(false), token),
      .TkString => return self.literalCons(try self.string(false), token),
      .TkMinus, .TkNumber => return self._numberOrRangePattern(),
      else => {}
    }

    // capture_pattern  | wildcard_pattern | class_pattern
    if (self.check(.TkIdent)) {
      return try self._classPattern();
    }

    // group_pattern | sequence_pattern 
    if (self.check(.TkLBracket) or self.check(.TkLSqrBracket)) {
      return self._groupOrSeqPattern();
    }

    // mapping_pattern
    if (self.match(.TkLCurly)) {
      return self._mappingPattern();
    }
    return self.errMsg(self.current_tok, "unexpected token");
  }

  fn _orPattern(self: *Self) !*Pattern {
    // or_pattern: '|'.closed_pattern+
    var token = self.current_tok;
    var pat = try self._closedPattern();
    if (self.check(.TkPipe)) {
      var cons = ptn.Constructor.newOrCons(self.allocator);
      cons.args.append(pat);
      while (self.match(.TkPipe)) {
        cons.args.append(try self._closedPattern());
      }
      pat = Pattern.init(cons.toVariant(self.allocator), token, .{}).box(self.allocator);
    }
    return pat;
  }

  fn _asPattern(self: *Self) !*Pattern {
    // as_pattern: or_pattern 'as' capture_pattern
    var pat = try self._orPattern();
    if (self.match(.TkAs)) {
      pat.alat.alias = try self._capturePattern();
    }
    return pat;
  }

  fn _pattern(self: *Self) anyerror!*Pattern {
    // pattern: as_pattern | or_pattern
    //    as_pattern: or_pattern 'as' capture_pattern
    //    or_pattern: '|'.closed_pattern+
    return (try self._asPattern());
  }

  fn caseStmt(self: *Self) !*ptn.Case {
    // case_block: "case" patterns guard? '=>' (expr | block)
    // patterns: pattern
    self.meta.m_literals.clearRetainingCapacity();
    const token = self.current_tok;
    var pat = try self._pattern();
    var guard: ?*Node = null;
    if (self.match(.TkIf)) {
      guard = try self.parseExpr();
    }
    try self.consume(.TkEqGrt);
    var body = try self.statement();
    if (self.meta.m_literals.isNotEmpty()) {
      // take all {name, id} pairs, and generate conditions
      const op_and = Token.fromWithValue(&token, "and", .TkAnd);
      var last: ?*Node = null;
      for (self.meta.m_literals.items()) |itm| {
        std.debug.assert(itm.@"2");
        var bin: *Node = itm.@"1";
        if (last) |nd| {
          const new = self.newNode();
          new.* = .{.AstBinary = ast.BinaryNode.init(nd, bin, op_and)};
          last = new;
        } else {
          last = bin;
        }
      }
      if (guard) |gard| {
        var cond = self.newNode();
        cond.* = .{.AstBinary = ast.BinaryNode.init(gard, last.?, op_and)};
        last = cond;
      }
      guard = last;
    }
    // a case with a guard must have a block body with lift markers for
    // hoisting any added declarations in the case's body during ptn compilation
    if (guard != null) {
      const marker = @as(Node, .{.AstLiftMarker = ast.MarkerNode.init(token)}).box(self.allocator);
      if (!body.isBlock()) {
        body = ast.BlockNode.newBlockWithNodes(self.allocator, &[_]*Node{marker, body});
      } else {
        body.block().nodes.prepend(marker);
      }
    }
    var node = self.createObj(ptn.Case);
    node.* = ptn.Case.init(pat, guard, body, false, self.allocator);
    return node;
  }

  fn convertMatchExprToVar(self: *Self, m: *ast.MatchNode, token: Token) void {
    if (m.expr.isVariable()) return;
    // expr -> let $id = expr
    var id = self.newNode();
    var tok = Token.fromWithValue(&token, self.genName("m_expr"), .TkIdent);
    id.* = .{.AstVar = ast.VarNode.init(tok)};
    var node = self.newNode();
    node.* = .{.AstVarDecl = ast.VarDeclNode.init(&id.AstVar, m.expr, false)};
    m.decl = node;
    m.expr = id;
  }

  fn matchStmt(self: *Self) !*Node {
    // match_stmt: "match" match_expr 'with'? NEWLINE case_block+ "end"
    const tok = self.previous_tok;
    var expr = try self.parseExpr();
    _ = self.match(.TkWith);
    try self.consume(.TkNewline);
    self.skipNewlines();
    var cases = ast.MatchNode.CaseList.init(self.allocator);
    while (self.match(.TkCase)) {
      cases.append(try self.caseStmt());
      self.skipNewlines();
    }
    try self.consume(.TkEnd);
    if (cases.isEmpty()) {
      self.softErrMsg(tok, "match statement missing case arms");
    } else {
      const last = cases.getLast();
      var red = self.newNode();
      red.* = .{.AstRedundantMarker = ast.MarkerNode.init(last.pattern.token)};
      if (last.body.node.isBlock()) {
        last.body.node.block().nodes.append(red);
      } else {
        last.body.node = ast.BlockNode.newBlockWithNodes(self.allocator, &[_]*Node{red, last.body.node});
      }
    }
    var node = self.newNode();
    node.* = .{.AstMatch = ast.MatchNode.init(tok, expr, cases)};
    self.convertMatchExprToVar(&node.AstMatch, tok);
    return node;
  }

  fn exprStmt(self: *Self) !*Node {
    const expr = try self.parseExpr();
    const node = self.newNode();
    node.* = .{.AstExprStmt = ast.ExprStmtNode.init(expr)};
    try self.consumeNlOrEof();
    return node;
  }

  fn emptyStmt(self: *Self) *Node {
    return ast.BlockNode.newEmptyBlock(self.allocator);
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
      return self.varDecl();
    } else if (self.match(.TkType)) {
      return self.typeAlias();
    } else if (self.check(.TkDo)) {
      return self.blockStmt(false, false, true);
    } else if (self.match(.TkNewline)) {
      return error.EmptyStatement;
    } else if (self.match(.TkIf)) {
      return self.ifStmt();
    } else if (self.match(.TkWhile)) {
      return self.whileStmt();
    } else if (self.check(.TkBreak) or self.check(.TkContinue)) {
      return self.controlStmt();
    } else if (self.check(.TkDef)) {
      return self.funStmt(false);
    } else if (self.match(.TkReturn)) {
      return self.returnStmt();
    } else if (self.match(.TkClass)) {
      return self.classStmt();
    } else if (self.match(.TkMatch)) {
      return self.matchStmt();
    }
    return self.exprStmt();
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
