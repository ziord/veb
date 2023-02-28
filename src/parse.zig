const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const util = @import("util.zig");
const NovaAllocator = @import("allocator.zig");

const Node = ast.AstNode;
const exit = std.os.exit;

// maximum number of elements of a list literal 
const MAX_LISTING_ELEMS = 0xff;


pub const Parser = struct {
  current_tok: lex.Token,
  previous_tok: lex.Token,
  lexer: lex.Lexer,
  allocator: std.mem.Allocator,
  nva: *NovaAllocator,
  filename: []const u8,
  allowNl: usize = 0,

  const Self = @This();

  const BindingPower = enum (u32) {
    None,        // other
    Assignment,  //
    Or,          // or
    And,         // and
    BitOr,       // |
    BitXor,      // ^
    BitAnd,      // &
    Equality,    // !=, ==
    Comparison,  // >, >=, <, <=
    Shift,       // >>, <<
    Term,        // +, -
    Factor,      // /, *, %
    Power,       // **
    Unary,       // !, -, +, ~
    Call,        // ()
    Access,      // [], .,
  };
  const PrefixFn = *const fn (*Self, bool) *Node;
  const InfixFn = *const fn (*Self, *Node, bool) *Node;
  const ExprParseTable = struct {
    bp: BindingPower,
    prefix: ?PrefixFn,
    infix: ?InfixFn
  };
  const ptable = [_]ExprParseTable{
    .{.bp = .Term, .prefix = Self.unary, .infix = Self.binary},   // TkPlus
    .{.bp = .Term, .prefix = Self.unary, .infix = Self.binary},   // TkMinus
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},       // TkSlash
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},       // TkStar
    .{.bp = .Term, .prefix = Self.grouping, .infix = null},       // TkLBracket
    .{.bp = .None, .prefix = null, .infix = null},                // TkRBracket
    .{.bp = .None, .prefix = Self.listing, .infix = null},        // TkLSqrBracket
    .{.bp = .None, .prefix = null, .infix = null},                // TkRSqrBracket
    .{.bp = .None, .prefix = null, .infix = null},                // TkSemic
    .{.bp = .None, .prefix = null, .infix = null},                // TkColon
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},   // TkLthan
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},   // TkGthan
    .{.bp = .None, .prefix = null, .infix = null},                // TkEqual
    .{.bp = .None, .prefix = Self.mapping, .infix = null},        // TkLCurly
    .{.bp = .None, .prefix = null, .infix = null},                // TkRCurly
    .{.bp = .BitAnd, .prefix = null, .infix = Self.binary},       // TkAmp
    .{.bp = .Factor, .prefix = null, .infix = Self.binary},       // TkPerc
    .{.bp = .None, .prefix = null, .infix = null},                // TkComma
    .{.bp = .Unary, .prefix = Self.unary, .infix = null},         // TkExMark
    .{.bp = .BitXor, .prefix = null, .infix = Self.binary},       // TkCaret
    .{.bp = .BitOr, .prefix = null, .infix = Self.binary},        // TkPipe
    .{.bp = .Unary, .prefix = Self.unary, .infix = null},         // TkTilde
    .{.bp = .None, .prefix = null, .infix = null},                // TkNewline
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},   // TkLeq
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},   // TkGeq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},     // Tk2Eq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},     // TkNeq
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},        // Tk2Lthan
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},        // Tk2Rthan
    .{.bp = .None, .prefix = null, .infix = null},                // TkIf
    .{.bp = .Or, .prefix = null, .infix = Self.binary},           // TkOr
    .{.bp = .None, .prefix = null, .infix = null},                // TkFor
    .{.bp = .And, .prefix = null, .infix = Self.binary},          // TkAnd
    .{.bp = .None, .prefix = null, .infix = null},                // TkLet
    .{.bp = .None, .prefix = null, .infix = null},                // TkElse
    .{.bp = .None, .prefix = Self.boolean, .infix = null},        // TkTrue
    .{.bp = .None, .prefix = Self.boolean, .infix = null},        // TkFalse
    .{.bp = .None, .prefix = null, .infix = null},                // TkWhile
    .{.bp = .None, .prefix = null, .infix = null},                // TkReturn
    .{.bp = .None, .prefix = Self.number, .infix = null},         // TkNum
    .{.bp = .None, .prefix = Self.string, .infix = null},         // TkStr
    .{.bp = .None, .prefix = Self.variable, .infix = null},       // TkIdent
    .{.bp = .None, .prefix = null, .infix = null},                // TkErr
    .{.bp = .None, .prefix = null, .infix = null},                // TkEof
  };

  pub fn init(src: []const u8, filename: []const u8, allocator: *NovaAllocator) Self {
    return Self {
      .current_tok = undefined,
      .previous_tok = undefined,
      .lexer = lex.Lexer.init(src, allocator),
      .nva = allocator,
      .filename = filename,
      // use the arena allocator for allocating general nodes.
      .allocator = allocator.getArenaAllocator(),
    };
  }

  fn printSquig(self: *Self, i: usize) void {
    _ = self;
    var y = i;
    while (y > 0) {
      std.debug.print("{s:^}", .{"^"});
      y -= 1;
    }
  }

  fn err(self: *Self, token: lex.Token) noreturn {
    var line = self.lexer.getLine(token.line);
    std.debug.print("ParseError: {s}\n", .{token.msg.?});
    std.debug.print("{s}.{}:{}:\n\t{s}\n", .{self.filename, token.line, token.column, line});
    if (std.mem.indexOf(u8, line, token.value) != null) {
      std.debug.print("\t", .{});
      var i = token.column - token.value.len;
      while (i > 0) {
        std.debug.print(" ", .{});
        i -= 1;
      }
      self.printSquig(token.value.len);
      std.debug.print("\n", .{});
    } else {
      std.debug.print("\t{s}\n", .{token.value});
      std.debug.print("\t", .{});
      self.printSquig(token.value.len);
      std.debug.print("\n", .{});
    }
    // free only arena for now.
    self.nva.deinitArena();
    exit(1);
  }

  fn advance(self: *Self) void {
    self.previous_tok = self.current_tok;
    const tok = self.lexer.getToken();
    if (!tok.isErr()) {
      self.current_tok = tok;
    } else {
      self.err(tok);
    }
  }

  fn consume(self: *Self, tty: lex.TokenType) void {
    if (self.check(tty)) {
      self.advance();
    } else {
      var buff: [1024]u8 = undefined;
      self.current_tok.msg = std.fmt.bufPrint(
        &buff,  "Expected token '{s}', but found '{s}'", 
        .{tty.str(), self.current_tok.ty.str()}
      ) catch exit(1);
      self.err(self.current_tok);
    }
  }

  inline fn newNode(self: *Self) *Node {
    return self.allocator.create(Node) catch {
      _ = std.io.getStdErr().write("Allocation failed\n") catch exit(1);
      exit(1);
    };
  }

  inline fn check(self: *Self, ty: lex.TokenType) bool {
    return self.current_tok.is(ty);
  }

  inline fn match(self: *Self, ty: lex.TokenType) bool {
    if (self.check(ty)) {
      self.advance();
      return true;
    }
    return false;
  }

  inline fn incNl(self: *Self) void {
    self.allowNl += 1;
    self.lexer.allowNl = self.allowNl;
  }

  inline fn decNl(self: *Self) void {
    self.allowNl -= 1;
    self.lexer.allowNl = self.allowNl;
  }

  inline fn consumeNlOrEof(self: *Self) void {
    // Try to consume Newline. 
    // If not, check that the current token is Eof, else error
    if (!self.match(.TkNewline)) {
      if (!self.check(.TkEof)) {
        self.consume(.TkNewline);
      }
    }
  }

  fn _parse(self: *Self, bp: BindingPower) *Node {
    const prefix = ptable[@enumToInt(self.current_tok.ty)].prefix;
    if (prefix == null) {
      self.current_tok.msg = "Invalid token for prefix";
      self.err(self.current_tok);
    }
    const bp_val = @enumToInt(bp);
    const assignable = bp_val <= @enumToInt(BindingPower.Assignment);
    var node = prefix.?(self, assignable);
    while (bp_val < @enumToInt(ptable[@enumToInt(self.current_tok.ty)].bp)) {
      const infix = ptable[@enumToInt(self.current_tok.ty)].infix.?;
      node = infix(self, node, assignable);
    }
    return node;
  }

  fn literal(self: *Self, kind: lex.TokenType) ast.LiteralNode {
    self.consume(kind);
    return  ast.LiteralNode.init(self.previous_tok);
  }

  fn number(self: *Self, assignable: bool) *Node {
    _ = assignable;
    const node = self.newNode();
    node.* = .{.AstNum = self.literal(.TkNum)};
    var token = node.AstNum.token;
    node.AstNum.value = token.parseNum() catch {
      token.msg = "Invalid number token";
      self.err(token);
    };
    return node;
  }

  fn string(self: *Self, assignable: bool) *Node {
    _ = assignable;
    const node = self.newNode();
    node.* = .{.AstStr = self.literal(.TkStr)};
    return node;
  }

  fn boolean(self: *Self, assignable: bool) *Node {
    _ = assignable;
    var ty: lex.TokenType = if (self.check(.TkTrue)) .TkTrue else .TkFalse;
    const node = self.newNode();
    node.* = .{.AstBool = self.literal(ty)};
    return node;
  }

  fn unary(self: *Self, assignable: bool) *Node {
    _ = assignable;
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok.ty.optype();
    const line_tok = self.current_tok;
    self.advance();
    const expr = self._parse(bp);
    const node = self.newNode();
    // rewrite -expr to 0 - expr
    if (op == .OpSub) {
      const num = self.newNode();
      var lit = ast.LiteralNode.init(line_tok);
      lit.value = 0;
      num.* = .{.AstNum = lit};
      node.* = .{.AstBinary = ast.BinaryNode.init(num, expr, op, line_tok.line)};
    } else if (op == .OpAdd) {
      // rewrite +expr to expr
      return expr;
    }
    node.* = .{.AstUnary = ast.UnaryNode.init(expr, op, line_tok.line)};
    return node;
  }

  fn binary(self: *Self, lhs: *Node, assignable: bool) *Node {
    _ = assignable;
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok.ty.optype();
    const line = self.current_tok.line;
    self.advance();
    const rhs = self._parse(bp);
    const node = self.newNode();
    node.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, op, line)};
    return node;
  }

  fn grouping(self: *Self, assignable: bool) *Node {
    _ = assignable;
    self.incNl();
    self.consume(.TkLBracket);
    const node = self.parseExpr();
    self.decNl();
    self.consume(.TkRBracket);
    return node;
  }

  fn listing(self: *Self, assignable: bool) *Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstList = ast.ListNode.init(self.allocator, self.current_tok.line)};
    self.incNl();
    self.consume(.TkLSqrBracket);
    var list = &node.*.AstList.elems;
    while (!self.check(.TkEof) and !self.check(.TkRSqrBracket)) {
      if (list.items.len > 0) {
        self.consume(.TkComma);
      }
      if (list.items.len > MAX_LISTING_ELEMS) {
        self.current_tok.msg = "Maximum number of list elements exceeded";
        self.err(self.current_tok);
      }
      util.append(*Node, list, self.parseExpr());
    }
    self.decNl();
    self.consume(.TkRSqrBracket);
    return node;
  }

  fn mapping(self: *Self, assignable: bool) *Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstMap = ast.MapNode.init(self.allocator, self.current_tok.line)};
    self.incNl();
    self.consume(.TkLCurly);
    var pairs = &node.*.AstMap.pairs;
    const max_items: usize = MAX_LISTING_ELEMS / 2;
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (pairs.items.len > 0) {
        self.consume(.TkComma);
      }
      if (pairs.items.len > max_items) {
        self.current_tok.msg = "Maximum number of map items exceeded";
        self.err(self.current_tok);
      }
      var key = self.parseExpr();
      self.consume(.TkColon);
      var val = self.parseExpr();
      util.append(ast.MapNode.Pair, pairs, .{.key = key, .value = val});
    }
    self.decNl();
    self.consume(.TkRCurly);
    return node;
  }

  fn handleAugAssign(self: *Self, left: *Node, assignable: bool) *Node {
    if (!assignable) return left;
    var node = left;
    if (self.current_tok.ty.isAssignLikeOp() and self.lexer.currentChar() == '=') {
      // +=, -=, ...=
      // e.g. var += expr => var = var + expr;
      var tok = self.current_tok;
      var op = self.current_tok.ty.optype();
      self.advance(); // skip assignlike op
      self.advance(); // skip '=' op
      var value = self.parseExpr();
      var right = self.newNode();
      right.* = .{.AstBinary = ast.BinaryNode.init(left, value, op, tok.line)};
      node = self.newNode();
      node.* = .{.AstAssign = ast.BinaryNode.init(left, right, .OpAssign, tok.line)};
    } else if (self.match(.TkEqual)) {
      var token = self.previous_tok;
      var value = self.parseExpr();
      node = self.newNode();
      node.* = .{.AstAssign = ast.BinaryNode.init(left, value, .OpAssign, token.line)};
    }
    return node;
  }

  fn variable(self: *Self, assignable: bool) *Node {
    self.consume(.TkIdent);
    var ident = self.previous_tok;
    var node = self.newNode();
    node.* = .{.AstVar = ast.VarNode.init(ident)};
    return self.handleAugAssign(node, assignable);
  }

  fn vardecl(self: *Self) *Node {
    // let var (: type)? = expr
    self.consume(.TkLet);
    self.consume(.TkIdent);
    var name = self.previous_tok;
    var ident = self.newNode();
    ident.* = .{.AstVar = ast.VarNode.init(name)};
    self.consume(.TkEqual);
    var val = self.parseExpr();
    var decl = self.newNode();
    decl.* = .{.AstVarDecl = ast.VarDeclNode.init(&ident.AstVar, val)};
    self.consumeNlOrEof();
    return decl;
  }

  fn parseExpr(self: *Self) *Node {
    return self._parse(.Assignment);
  }

  fn exprStmt(self: *Self) *Node {
    const line = self.current_tok.line;
    const expr = self.parseExpr();
    const node = self.newNode();
    node.* = .{.AstExprStmt = ast.ExprStmtNode.init(expr, line)};
    self.consumeNlOrEof();
    return node;
  }

  fn statement(self: *Self) *Node {
    if (self.check(.TkLet)) {
      return self.vardecl();
    }
    return self.exprStmt();
  }

  pub fn parse(self: *Self) *Node {
    self.advance();
    var program = self.newNode();
    program.* = .{.AstProgram = ast.ProgramNode.init(self.allocator, self.current_tok.line)};
    while (!self.match(.TkEof)) {
      util.append(*Node, &program.AstProgram.decls, self.statement());
    }
    return program;
  }
};
