const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const diagnostics = @import("diagnostics.zig");
const CnAllocator = @import("allocator.zig");

const Node = ast.AstNode;
const exit = std.os.exit;
pub const TypeKind = types.TypeKind;
const Type = types.Type;
const Generic = types.Generic;
const Union = types.Union;
const Variable = types.Variable;
const Concrete = types.Concrete;
const Diagnostic = diagnostics.Diagnostic;

// maximum number of elements of a list literal 
const MAX_LISTING_ELEMS = 0xff;


pub const Parser = struct {
  current_tok: lex.Token,
  previous_tok: lex.Token,
  lexer: lex.Lexer,
  allocator: std.mem.Allocator,
  diag: Diagnostic,
  cna: *CnAllocator,
  allow_nl: usize = 0,
  using_is: u32 = 0,
  in_cast: u32 = 0,
  loops: u32 = 0,

  const Self = @This();

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
    .{.bp = .Term, .prefix = Self.grouping, .infix = null},             // TkLBracket
    .{.bp = .None, .prefix = null, .infix = null},                      // TkRBracket
    .{.bp = .Access, .prefix = Self.listing, .infix = Self.indexing},   // TkLSqrBracket
    .{.bp = .None, .prefix = null, .infix = null},                      // TkRSqrBracket
    .{.bp = .None, .prefix = null, .infix = null},                      // TkSemic
    .{.bp = .None, .prefix = null, .infix = null},                      // TkColon
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkLthan
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkGthan
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEqual
    .{.bp = .None, .prefix = Self.mapping, .infix = null},              // TkLCurly
    .{.bp = .None, .prefix = null, .infix = null},                      // TkRCurly
    .{.bp = .BitAnd, .prefix = null, .infix = Self.binary},             // TkAmp
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},             // TkPerc
    .{.bp = .None, .prefix = null, .infix = null},                      // TkComma
    .{.bp = .Unary, .prefix = Self.unary, .infix = null},               // TkExMark
    .{.bp = .BitXor, .prefix = null, .infix = Self.binary},             // TkCaret
    .{.bp = .BitOr, .prefix = null, .infix = Self.binary},              // TkPipe
    .{.bp = .Unary, .prefix = Self.unary, .infix = null},               // TkTilde
    .{.bp = .Access, .prefix = null, .infix = Self.dotderef},            // TkDot
    .{.bp = .None, .prefix = null, .infix = null},                      // TkQMark
    .{.bp = .None, .prefix = null, .infix = null},                      // TkNewline
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkLeq
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkGeq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},           // Tk2Eq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},           // TkNeq
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},              // Tk2Lthan
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},              // Tk2Rthan
    .{.bp = .Access, .prefix = null, .infix = Self.casting},            // TkAs
    .{.bp = .None, .prefix = null, .infix = null},                      // TkDo
    .{.bp = .Equality, .prefix = null, .infix = Self.binIs},            // TkIs
    .{.bp = .None, .prefix = null, .infix = null},                      // TkIf
    .{.bp = .Or, .prefix = null, .infix = Self.binary},                 // TkOr
    .{.bp = .None, .prefix = null, .infix = null},                      // TkFor
    .{.bp = .And, .prefix = null, .infix = Self.binary},                // TkAnd
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEnd
    .{.bp = .None, .prefix = null, .infix = null},                      // TkNot
    .{.bp = .None, .prefix = null, .infix = null},                      // TkLet
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkNum
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkMap
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkStr
    .{.bp = .None, .prefix = Self.nullable, .infix = null},             // TkNil
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkBool
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkList
    .{.bp = .None, .prefix = null, .infix = null},                      // TkThen
    .{.bp = .None, .prefix = null, .infix = null},                      // TkType
    .{.bp = .None, .prefix = null, .infix = null},                      // TkElse
    .{.bp = .None, .prefix = null, .infix = null},                      // TkElif
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkTrue
    .{.bp = .None, .prefix = null, .infix = null},                      // TkBreak
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkFalse
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkTuple
    .{.bp = .None, .prefix = null, .infix = null},                      // TkWhile
    .{.bp = .None, .prefix = null, .infix = null},                      // TkReturn
    .{.bp = .None, .prefix = null, .infix = null},                      // TkContinue
    .{.bp = .None, .prefix = Self.number, .infix = null},               // TkNumber
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkString
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkAllocString
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkIdent
    .{.bp = .None, .prefix = null, .infix = null},                      // TkErr
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEof
  };

  pub fn init(src: *[]const u8, filename: *const[]const u8, allocator: *CnAllocator) Self {
    var al = allocator.getArenaAllocator();
    return Self {
      .current_tok = undefined,
      .previous_tok = undefined,
      .lexer = lex.Lexer.init(src.*, allocator),
      .cna = allocator,
      .diag = Diagnostic.init(al, filename, src),
      // use the arena allocator for allocating general nodes.
      .allocator = al,
    };
  }

  fn err(self: *Self, token: lex.Token, msg: []const u8) ParseError {
    self.diag.addDiagnostics(.DiagError, token, "Error: {s}", .{msg});
    return error.ParseError;
  }

  fn advance(self: *Self) !void {
    self.previous_tok = self.current_tok;
    const tok = self.lexer.getToken();
    if (!tok.isErr()) {
      self.current_tok = tok;
    } else {
      return self.err(tok, tok.value);
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
      return self.err(self.current_tok, msg);
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

  inline fn incIs(self: *Self) void {
    self.using_is += 1;
  }

  inline fn decIs(self: *Self) void {
    self.using_is -= 1;
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

  inline fn parsingIs(self: *Self) bool {
    return self.using_is > 0 and self.in_cast == 0;
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
      return self.err(self.current_tok, msg);
    }
  }

  fn _parse(self: *Self, bp: BindingPower) !*Node {
    const prefix = ptable[@enumToInt(self.current_tok.ty)].prefix;
    if (prefix == null) {
      return self.err(self.current_tok, "token found at an invalid prefix position");
    }
    const bp_val = @enumToInt(bp);
    const assignable = bp_val <= @enumToInt(BindingPower.Assignment);
    var node = try prefix.?(self, assignable);
    while (bp_val < @enumToInt(ptable[@enumToInt(self.current_tok.ty)].bp)) {
      var infix = ptable[@enumToInt(self.current_tok.ty)].infix;
      if (infix == null) {
        return self.err(self.current_tok, "token found at an invalid infix position");
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
    node.AstNumber.value = token.parseNum() catch {
      return self.err(token, "invalid number token");
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
    self.incIs();
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok;
    try self.advance();
    var is_not = self.match(.TkNot);
    var not_token = self.previous_tok;
    var rhs = try self._parse(bp);
    const node = self.newNode();
    // wrap nil literal as nil TypeNode
    if (rhs.isNilLiteral()) {
      var nil = Type.newConcrete(.TyNil, null, self.previous_tok);
      rhs.* = .{.AstNType = ast.TypeNode.init(nil, self.previous_tok)};
    }
    node.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, op)};    
    self.decIs();
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
    return node;
  }

  fn tupling(self: *Self, assignable: bool, first: ?*Node) !*Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstTuple = ast.ListNode.init(self.allocator, self.current_tok)};
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
    node.* = .{.AstList = ast.ListNode.init(self.allocator, self.current_tok)};
    self.incNl();
    try self.consume(.TkLSqrBracket);
    var list = &node.*.AstList.elems;
    while (!self.check(.TkEof) and !self.check(.TkRSqrBracket)) {
      if (list.len() > 0) {
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
    node.* = .{.AstMap = ast.MapNode.init(self.allocator, self.current_tok)};
    self.incNl();
    try self.consume(.TkLCurly);
    var pairs = &node.*.AstMap.pairs;
    const max_items: usize = MAX_LISTING_ELEMS / 2;
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (pairs.len() > 0) {
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
        return self.err(self.current_tok, "maximum number of map items exceeded");
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
    try self.consume(.TkQMark);
    var node = self.newNode();
    node.* = .{.AstDeref = ast.DerefNode.init(left, token)};
    return try self.handleAugAssign(node, assignable);
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

  inline fn assertMaxTParams(self: *Self, typ: *Generic) !void {
    if (typ.tparams.len() >= types.MAX_TPARAMS) {
      return self.err(self.current_tok, "maximum type parameters exceeded");
    }
  }

  inline fn assertBuiltinExpTParams(self: *Self, typ: *Generic) !void {
    if (typ.base.isSimple()) {
      var conc = typ.base.kind.Concrete;
      if (conc.tkind == .TyClass) {
        var tok = conc.variable.?.tokens.getLast();
        // list or map
        var exp: usize = (
          if (tok.ty == .TkList) 1 
          else if (tok.ty == .TkMap) 2  
          else if (tok.ty == .TkTuple) 1 
          else return
        );
        if (typ.tparams.len() != exp) {
          return self.err(tok, "generic type instantiated with wrong number of paramters");
        }
      }
    }
    try self.assertNonEmptyTParams(typ);
  }

  inline fn assertNonEmptyTParams(self: *Self, typ: *Generic) !void {
    if (typ.tparams.len() == 0) {
      return self.err(self.previous_tok, "empty type parameters not allowed");
    }
  }

  inline fn assertUniqueTParams(self: *Self, alias: *Generic, param: *Type) !void {
    for (alias.getSlice()) |typ| {
      // `param` and `typ` have Variable.tokens equal to size 1.
      var token = param.variable().tokens.getLast();
      if (std.mem.eql(u8, typ.variable().tokens.getLast().value, token.value)) {
        return self.err(token, "redefinition of alias type parameter");
      }
    }
  }

  fn aliasParam(self: *Self) !Type {
    var debug = self.current_tok;
    try self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator, debug);
    typ.variable().append(debug);
    if (self.check(.TkDot)) {
      return self.err(self.current_tok, "expected single identifier, found multiple");
    }
    return typ;
  }

  fn abstractType(self: *Self) !Type {
    // AbstractType := TypeName
    // TypeName    := Ident TypeParams?
    // TypeParams  := "{" Ident ( "," Ident )* "}"
    try self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator, self.previous_tok);
    typ.variable().append(self.previous_tok);
    if (self.match(.TkLCurly)) {
      var gen = Generic.init(self.allocator, typ.box(self.allocator));
      while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
        if (gen.tparams.len() > 0) try self.consume(.TkComma);
        try self.assertMaxTParams(&gen);
        var param = try self.aliasParam();
        try self.assertUniqueTParams(&gen, &param);
        gen.append(param.box(self.allocator));
      }
      try self.assertNonEmptyTParams(&gen);
      try self.consume(.TkRCurly);
      return gen.toType(self.previous_tok);
    }
    return typ;
  }

  fn constantType(self: *Self) !Type {
    // Constant  := StringLiteral | BooleanLiteral | NumberLiteral
    var kind: TypeKind = switch (self.current_tok.ty) {
      .TkTrue, .TkFalse => .TyBool, 
      .TkNumber => .TyNumber,
      .TkString, .TkAllocString => .TyString,
      else => {
        return self.err(self.current_tok, "invalid type-start");
      }
    };
    // direct 'unit' types such as listed above do not need names
    var typ = Type.newConstant(kind, self.current_tok.value, self.current_tok);
    try self.advance();
    return typ;
  }

  fn builtinType(self: *Self) !Type {
    // handle builtin list/map/tuple type
    var debug = self.current_tok;
    try self.advance();
    var tvar = Variable.init(self.allocator);
    tvar.append(debug);
    var conc = Type.newConcrete(.TyClass, debug.value, debug);
    conc.kind.Concrete.name = debug.value;
    conc.kind.Concrete.variable = tvar;
    return Type.newGeneric(self.allocator, conc.box(self.allocator), debug);
  }

  fn refType(self: *Self) !Type {
    try self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator, self.previous_tok);
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
    } else if (self.check(.TkList) or self.check(.TkMap) or self.check(.TkTuple)) {
      return try self.builtinType();
    }
    var tkind: TypeKind = switch (self.current_tok.ty) {
      .TkBool => .TyBool,
      .TkNum => .TyNumber,
      .TkStr => .TyString,
      // TODO: func, method, class, instance
      else => return try self.constantType(),
    };

    // direct 'unit' types such as listed above do not need names
    var typ = Type.newConcrete(tkind, null, self.current_tok);
    try self.advance();
    return typ;
  }

  fn tPrimary(self: *Self) ParseError!Type {
    // Primary  :=  Builtin | Reference | Constant | “(“ Expression “)”
    var typ: Type = undefined;
    if (self.match(.TkLBracket)) {
      typ = try self.tExpr();
      try self.consume(.TkRBracket);
    } else {
      typ = try self.builtinOrRefType();
    }
    return typ;
  }

  fn tGeneric(self: *Self) ParseError!Type {
    // Generic := ( Primary | Primary "{" Expression ( "," Expression )* "}" ) "?"?
    var typ = try self.tPrimary();
    if (self.parsingIs()) {
      return typ;
    }
    if (self.match(.TkLCurly)) {
      if (typ.isSimple()) {
        return self.err(self.previous_tok, "cannot instantiate simple type as generic");
      }
      var gen: *Generic = undefined;
      var ret: Type = undefined;
      if (typ.isGeneric()) {
        ret = typ;
      } else {
        var tmp = Generic.init(self.allocator, typ.box(self.allocator));
        ret = tmp.toType(self.previous_tok);
      }
      gen = ret.generic();
      while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
        if (gen.tparams.len() > 0) try self.consume(.TkComma);
        try self.assertMaxTParams(gen);
        var param = try self.tExpr();
        gen.append(param.box(self.allocator));
      }
      try self.assertNonEmptyTParams(gen);
      // check that builtin generic types are properly instantiated
      try self.assertBuiltinExpTParams(gen);
      try self.consume(.TkRCurly);
      typ = ret;
    } else if (typ.isGeneric()) {
      try self.assertBuiltinExpTParams(typ.generic());
    }
    if (self.match(.TkQMark)) {
      typ = Type.newNullable(typ.box(self.allocator), self.previous_tok, self.allocator).*;
    }
    return typ;
  }

  fn tUnion(self: *Self) !Type {
    // Union := Generic ( “|” Generic )*
    var typ = try self.tGeneric();
    if (self.parsingIs()) {
      return typ;
    }
    if (self.check(.TkPipe)) {
      var token = self.current_tok;
      var uni = Union.init(self.allocator);
      uni.set(typ.box(self.allocator));
      while (self.match(.TkPipe)) {
        typ = try self.tGeneric();
        uni.set(typ.box(self.allocator));
      }
      return uni.toType(token);
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
    node.* = .{.AstNType = ast.TypeNode.init(typ, token)};
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
            return self.err(tok.*, "type variable in generic parameter cannot be generic");
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
      return self.err(tok.*, "type alias cannot be used directly in the aliasee");
    }
  }

  fn typeAlias(self: *Self) !*Node {
    // TypeAlias   := "type" AbstractType "=" ConcreteType
    var type_tok = self.previous_tok;
    var alias_typ = try self.abstractType();
    var alias = self.newNode();
    alias.* = .{.AstNType = ast.TypeNode.init(alias_typ, self.current_tok)};
    try self.consume(.TkEqual);
    var aliasee = try self.typing(false);
    var node = self.newNode();
    // check that generic type variable parameters in `AbstractType` are not generic in `ConcreteType`
    try self.assertNoGenericParameterTypeVariable(&alias_typ, &aliasee.AstNType.typ);
    // TODO: should this be disallowed? It poses no issues at the moment.
    // self.assertNoDirectRecursiveAlias(&alias_typ, &aliasee.AstNType.typ);
    node.* = .{.AstAlias = ast.AliasNode.init(type_tok, &alias.AstNType, &aliasee.AstNType)};
    try self.consumeNlOrEof();
    return node;
  }

  fn annotation(self: *Self, ident: *ast.VarNode) !void {
    if (self.match(.TkColon)) {
      var typ_node = &(try self.typing(false)).AstNType;
      typ_node.from_alias_or_annotation = true;
      typ_node.typ.ident = ident;
      ident.typ = &typ_node.typ;
    }
  }

  fn parseExpr(self: *Self) !*Node {
    return try self._parse(.Assignment);
  }

  fn blockStmt(self: *Self, skip_do: bool) !*Node {
    const line = self.current_tok.line;
    if (!skip_do) try self.consume(.TkDo);
    try self.consume(.TkNewline);
    var node = self.newNode();
    node.* = .{.AstBlock = ast.BlockNode.init(self.allocator, line)};
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      node.AstBlock.nodes.append(try self.statement());
    }
    try self.consume(.TkEnd);
    // eat newline if present
    _ = self.match(.TkNewline);
    return node;
  }

  fn varDecl(self: *Self) !*Node {
    // let var (: type)? = expr
    try self.consume(.TkIdent);
    var name = self.previous_tok;
    var ident = self.newNode();
    ident.* = .{.AstVar = ast.VarNode.init(name)};
    try self.annotation(&ident.*.AstVar);
    try self.consume(.TkEqual);
    var val = try self.parseExpr();
    var decl = self.newNode();
    decl.* = .{.AstVarDecl = ast.VarDeclNode.init(&ident.AstVar, val)};
    try self.consumeNlOrEof();
    return decl;
  }

  fn ifStmt(self: *Self) !*Node {
    // if expr then? nl body (elif expr then? nl body)* else nl body end
    const cond = try self.parseExpr();
    _ = self.match(.TkThen);
    try self.consume(.TkNewline);
    var then = ast.BlockNode.init(self.allocator, self.previous_tok.line);
    while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
      then.nodes.append(try self.statement());
    }
    var elifs = ast.AstNodeList.init(self.allocator);
    while (self.match(.TkElif)) {
      var elif_cond = try self.parseExpr();
      _ = self.match(.TkThen);
      try self.consume(.TkNewline);
      var elif_then = ast.BlockNode.init(self.allocator, self.previous_tok.line);
      while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
        elif_then.nodes.append(try self.statement());
      }
      var elif_then_node = self.newNode();
      var elif_node = self.newNode();
      elif_then_node.* = .{.AstBlock = elif_then};
      elif_node.* = .{.AstElif = ast.ElifNode.init(elif_cond, elif_then_node)};
      elifs.append(elif_node);
    }
    var els = ast.BlockNode.init(self.allocator, self.previous_tok.line);
    if (self.match(.TkElse)) {
      try self.consume(.TkNewline);
      while (!self.check(.TkEof) and !self.check(.TkEnd)) {
        els.nodes.append(try self.statement());
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
      return self.err(self.current_tok, "control statement used outside loop");
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
    var then = try self.blockStmt(!self.check(.TkDo));
    self.decLoop();
    var node = self.newNode();
    node.* = .{.AstWhile = ast.WhileNode.init(cond, then)};
    return node;
  }

  fn exprStmt(self: *Self) !*Node {
    const line = self.current_tok.line;
    const expr = try self.parseExpr();
    const node = self.newNode();
    node.* = .{.AstExprStmt = ast.ExprStmtNode.init(expr, line)};
    try self.consumeNlOrEof();
    return node;
  }

  fn emptyStmt(self: *Self) *Node {
    return ast.BlockNode.newEmptyBlock(self.previous_tok.line, self.allocator);
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

  fn statement(self: *Self) !*Node {
    if (self.match(.TkLet)) {
      return try self.varDecl();
    } else if (self.match(.TkType)) {
      return try self.typeAlias();
    } else if (self.check(.TkDo)) {
      return try self.blockStmt(false);
    } else if (self.match(.TkNewline)) {
      return self.emptyStmt();
    } else if (self.match(.TkIf)) {
      return try self.ifStmt();
    } else if (self.match(.TkWhile)) {
      return try self.whileStmt();
    } else if (self.check(.TkBreak) or self.check(.TkContinue)) {
      return try self.controlStmt();
    }
    return try self.exprStmt();
  }

  pub fn parse(self: *Self) !*Node {
    self.advance() catch {};
    var program = self.newNode();
    program.* = .{.AstProgram = ast.ProgramNode.init(self.allocator, self.current_tok.line)};
    while (!self.match(.TkEof)) {
      var stmt = self.statement() catch blk: {
        self.recover();
        break :blk self.emptyStmt();
      };
      program.AstProgram.decls.append(stmt);
    }
    if (self.diag.hasAny()) {
      self.diag.display();
      program.AstProgram.decls.clearRetainingCapacity();
    }
    return program;
  }
};
