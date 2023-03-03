const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const util = @import("util.zig");
const NovaAllocator = @import("allocator.zig");

const Node = ast.AstNode;
const exit = std.os.exit;
const NTypeKind = ast.NTypeKind;
const TName = ast.TName;
const TUnion = ast.TUnion;
const NType = ast.NType;

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
    .{.bp = .Access, .prefix = null, .infix = null},              // TkDot
    .{.bp = .None, .prefix = null, .infix = null},                // TkQMark
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
    .{.bp = .None, .prefix = Self.typing, .infix = null},         // TkNum
    .{.bp = .None, .prefix = Self.typing, .infix = null},         // TkMap
    .{.bp = .None, .prefix = Self.typing, .infix = null},         // TkStr
    .{.bp = .None, .prefix = Self.typing, .infix = null},         // TkBool
    .{.bp = .None, .prefix = Self.typing, .infix = null},         // TkList
    .{.bp = .None, .prefix = null, .infix = null},                // TkType
    .{.bp = .None, .prefix = null, .infix = null},                // TkElse
    .{.bp = .None, .prefix = Self.boolean, .infix = null},        // TkTrue
    .{.bp = .None, .prefix = Self.boolean, .infix = null},        // TkFalse
    .{.bp = .None, .prefix = null, .infix = null},                // TkWhile
    .{.bp = .None, .prefix = null, .infix = null},                // TkReturn
    .{.bp = .None, .prefix = Self.number, .infix = null},         // TkNumber
    .{.bp = .None, .prefix = Self.string, .infix = null},         // TkString
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
      std.debug.print("Allocation failed\n", .{});
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
    node.* = .{.AstNum = self.literal(.TkNumber)};
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
    node.* = .{.AstStr = self.literal(.TkString)};
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

  inline fn assertMaxTParams(self: *Self, typ: *NType) void {
    if (typ.tparams.len >= ast.MAX_TPARAMS) {
      self.current_tok.msg = "maximum type parameters exceeded";
      self.err(self.current_tok);
    }
  }

  inline fn assertNonEmptyTParams(self: *Self, typ: *NType) void {
    if (typ.tparams.len == 0) {
      self.previous_tok.msg = "Empty type parameters not allowed";
      self.err(self.previous_tok);
    }
  }

  fn refType(self: *Self, consume_dot: bool) NType {
    var name = TName.init(self.allocator);
    while (self.match(.TkIdent)) {
      util.append(lex.Token, &name.tokens, self.previous_tok);
      if (name.tokens.items.len > 0 and self.check(.TkDot)) {
        if (consume_dot) {
          self.advance();
        } else {
          self.current_tok.msg = "Expected single identifier, found multiple";
          self.err(self.current_tok);
        }
      }
    }
    return NType.init(NTypeKind.TyName, name);
  }

  fn builtinOrRefType(self: *Self) NType {
    if (self.check(.TkIdent)) {
      return self.refType(true);
    }
    var kind: NTypeKind = switch (self.current_tok.ty) {
      .TkBool => .TyBool,
      .TkNum => .TyNumber,
      .TkStr => .TyString,
      .TkList => .TyList,
      .TkMap => .TyMap,
      // TODO: func, method, class, instance
      else => {
        self.current_tok.msg = "Invalid type-start";
        self.err(self.current_tok);
      }
    };
    // direct types such as listed above do not need names
    var typ = NType.init(kind, null);
    self.advance();
    return typ;
  }

  fn abstractType(self: *Self) NType {
    // AbstractType := TypeName
    // TypeName    := Ident TypeParams?
    // TypeParams  := "{" Ident ( "," Ident )* "}"
    self.consume(.TkIdent);
    var name = TName.init(self.allocator);
    util.append(lex.Token, &name.tokens, self.previous_tok);
    var typ = NType.init(.TyName, name);
    if (self.match(.TkLCurly)) {
      while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
        if (typ.tparams.len > 0) self.consume(.TkComma);
        self.assertMaxTParams(&typ);
        var param = self.refType(false);
        typ.tparams.params[typ.tparams.len] = util.box(NType, param, self.allocator);
        typ.tparams.len += 1;
      }
      self.assertNonEmptyTParams(&typ);
      self.consume(.TkRCurly);
    }
    return typ;
  }

  fn tPrimary(self: *Self) NType {
    // Primary  :=  Builtin | Reference | “(“ Expression “)” | Primary “?”
    var typ: NType = undefined;
    if (self.match(.TkLBracket)) {
      typ = self.tExpr();
      self.consume(.TkRBracket);
    } else {
      typ = self.builtinOrRefType();
    }
    return typ;
  }

  fn tGeneric(self: *Self) NType {
    // Generic := ( Primary | Primary "{" Expression ( "," Expression )* "}" ) "?"?
    var typ = self.tPrimary();
    if (self.match(.TkLCurly)) {
      while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
        if (typ.tparams.len > 0) self.consume(.TkComma);
        self.assertMaxTParams(&typ);
        var param = self.tExpr();
        typ.tparams.params[typ.tparams.len] = util.box(NType, param, self.allocator);
        typ.tparams.len += 1;
      }
      self.assertNonEmptyTParams(&typ);
      self.consume(.TkRCurly);
    }
    if (self.match(.TkQMark)) {
      var nsubtype = typ;
      typ = NType.init(.TyNullable, null);
      typ.nsubtype = util.box(NType, nsubtype, self.allocator);
    }
    return typ;
  }

  fn tUnion(self: *Self) NType {
    // Union := Generic ( “|” Generic )*
    var typ = self.tGeneric();
    if (self.check(.TkPipe)) {
      typ.union_ = TUnion.init(self.allocator);
      while (self.match(.TkPipe)) {
        var gen = self.tGeneric();
        util.append(*NType, &typ.union_.?.types, util.box(NType, gen, self.allocator));
      }
    }
    return typ;
  }

  fn tExpr(self: *Self) NType {
    // Expression := Union
    return self.tUnion();
  }

  fn typing(self: *Self, assignable: bool) *Node {
    _ = assignable;
    var token = self.current_tok;
    var typ = self.tExpr();
    var node = self.newNode();
    node.* = .{.AstNType = ast.TypeNode.init(typ, token)};
    return node;
  }

  fn typeAlias(self: *Self) *Node {
    // TypeAlias   := "type" AbstractType "=" ConcreteType
    var type_tok = self.current_tok;
    self.consume(.TkType);
    var alias_typ = self.abstractType();
    var alias = self.newNode();
    alias.* = .{.AstNType = ast.TypeNode.init(alias_typ, self.current_tok)};
    self.consume(.TkEqual);
    var aliasee = self.typing(false);
    var node = self.newNode();
    node.* = .{.AstAlias = ast.AliasNode.init(type_tok, &alias.AstNType, &aliasee.AstNType)};
    self.consumeNlOrEof();
    return node;
  }

  fn annotation(self: *Self, ident: *ast.VarNode) void {
    if (self.match(.TkColon)) {
      var typ_node = &self.typing(false).AstNType;
      typ_node.typ.ident = ident;
      ident.typn = typ_node;
    }
  }

  fn varDecl(self: *Self) *Node {
    // let var (: type)? = expr
    self.consume(.TkLet);
    self.consume(.TkIdent);
    var name = self.previous_tok;
    var ident = self.newNode();
    ident.* = .{.AstVar = ast.VarNode.init(name)};
    self.annotation(&ident.*.AstVar);
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
      return self.varDecl();
    } else if (self.check(.TkType)) {
      return self.typeAlias();
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
