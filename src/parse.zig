const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const types = @import("type.zig");
const util = @import("util.zig");
const NovaAllocator = @import("allocator.zig");

const Node = ast.AstNode;
const exit = std.os.exit;
const TypeKind = types.TypeKind;
const Type = types.Type;
const Generic = types.Generic;
const Union = types.Union;
const Variable = types.Variable;
const Nullable = types.Nullable;
const Concrete = types.Concrete;

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
    Access,      // [], ., as
  };
  const PrefixFn = *const fn (*Self, bool) *Node;
  const InfixFn = *const fn (*Self, *Node, bool) *Node;
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
    .{.bp = .Access, .prefix = null, .infix = null},                    // TkDot
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
    .{.bp = .None, .prefix = null, .infix = null},                      // TkIf
    .{.bp = .Or, .prefix = null, .infix = Self.binary},                 // TkOr
    .{.bp = .None, .prefix = null, .infix = null},                      // TkFor
    .{.bp = .And, .prefix = null, .infix = Self.binary},                // TkAnd
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEnd
    .{.bp = .None, .prefix = null, .infix = null},                      // TkLet
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkNum
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkMap
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkStr
    .{.bp = .None, .prefix = Self.nullable, .infix = null},             // TkNil
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkBool
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkList
    .{.bp = .None, .prefix = null, .infix = null},                      // TkType
    .{.bp = .None, .prefix = null, .infix = null},                      // TkElse
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkTrue
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkFalse
    .{.bp = .None, .prefix = null, .infix = null},                      // TkWhile
    .{.bp = .None, .prefix = null, .infix = null},                      // TkReturn
    .{.bp = .None, .prefix = Self.number, .infix = null},               // TkNumber
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkString
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkIdent
    .{.bp = .None, .prefix = null, .infix = null},                      // TkErr
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEof
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

  fn err(self: *Self, token: lex.Token) noreturn {
    token.showError(self.filename, "ParseError: {s}", .{token.msg.?});
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
        // error, since we originally expected Newline
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
      var infix = ptable[@enumToInt(self.current_tok.ty)].infix;
      if (infix == null) {
        self.current_tok.msg = "Invalid token for infix";
        self.err(self.current_tok);
      }
      node = infix.?(self, node, assignable);
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
    node.* = .{.AstNumber = self.literal(.TkNumber)};
    var token = node.AstNumber.token;
    node.AstNumber.value = token.parseNum() catch {
      token.msg = "Invalid number token";
      self.err(token);
    };
    return node;
  }

  fn string(self: *Self, assignable: bool) *Node {
    _ = assignable;
    const node = self.newNode();
    node.* = .{.AstString = self.literal(.TkString)};
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
    const op = self.current_tok;
    const line_tok = self.current_tok;
    self.advance();
    const expr = self._parse(bp);
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
    node.* = .{.AstUnary = ast.UnaryNode.init(expr, op, line_tok.line)};
    return node;
  }

  fn binary(self: *Self, lhs: *Node, assignable: bool) *Node {
    _ = assignable;
    const bp = ptable[@enumToInt(self.current_tok.ty)].bp;
    const op = self.current_tok;
    self.advance();
    const rhs = self._parse(bp);
    const node = self.newNode();
    node.* = .{.AstBinary = ast.BinaryNode.init(lhs, rhs, op)};
    return node;
  }

  fn casting(self: *Self, lhs: *Node, assignable: bool) *Node {
    _ = assignable;
    self.consume(.TkAs);
    const token = self.previous_tok;
    const rhs = self.typing(false);
    const node = self.newNode();
    node.* = .{.AstCast = ast.CastNode.init(lhs, &rhs.AstNType, token)};
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
    node.* = .{.AstList = ast.ListNode.init(self.allocator, self.current_tok)};
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
    node.* = .{.AstMap = ast.MapNode.init(self.allocator, self.current_tok)};
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

  fn indexing(self: *Self, left: *Node, assignable: bool) *Node {
    self.incNl();
    self.consume(.TkLSqrBracket);
    var token = self.current_tok;
    var index = self.parseExpr();
    self.decNl();
    self.consume(.TkRSqrBracket);
    var node = self.newNode();
    node.* = .{.AstSubscript = ast.SubscriptNode.init(left, index, token)};
    return self.handleAugAssign(node, assignable);
  }

  fn handleAugAssign(self: *Self, left: *Node, assignable: bool) *Node {
    // assignments are not expressions, but statements
    if (!assignable) return left;
    var node = left;
    if (self.current_tok.ty.isAssignLikeOp() and self.lexer.currentChar() == '=') {
      // +=, -=, ...=
      // e.g. var += expr => var = var + expr;
      var op_sign = self.current_tok;
      self.advance(); // skip assignlike op
      var op_eq = self.current_tok;
      self.advance(); // skip '=' op
      var value = self.parseExpr();
      var right = self.newNode();
      right.* = .{.AstBinary = ast.BinaryNode.init(left, value, op_sign)};
      node = self.newNode();
      node.* = .{.AstAssign = ast.BinaryNode.init(left, right, op_eq)};
      // only ascertain that a newline or eof is present, if not, error
      if (!self.check(.TkNewline) and !self.check(.TkEof)) self.consumeNlOrEof();
      // if present, exprStmt() would consume it for us.
      return node;
    } else if (self.match(.TkEqual)) {
      var token = self.previous_tok;
      var value = self.parseExpr();
      node = self.newNode();
      node.* = .{.AstAssign = ast.BinaryNode.init(left, value, token)};
      // only ascertain that a newline or eof is present, if not, error
      if (!self.check(.TkNewline) and !self.check(.TkEof)) self.consumeNlOrEof();
      // if present, exprStmt() would consume it for us.
      return node;
    } else {
      return node;
    }
  }

  fn variable(self: *Self, assignable: bool) *Node {
    self.consume(.TkIdent);
    var ident = self.previous_tok;
    var node = self.newNode();
    node.* = .{.AstVar = ast.VarNode.init(ident)};
    return self.handleAugAssign(node, assignable);
  }

  fn nullable(self: *Self, assignable: bool) *Node {
    _ = assignable;
    var node = self.newNode();
    node.* = .{.AstNil = self.literal(.TkNil)};
    return node;
  }

  inline fn assertMaxTParams(self: *Self, typ: *Generic) void {
    if (typ.tparams.items.len >= types.MAX_TPARAMS) {
      self.current_tok.msg = "maximum type parameters exceeded";
      self.err(self.current_tok);
    }
  }

  inline fn assertBuiltinExpTParams(self: *Self, typ: *Generic) void {
    if (typ.base.isSimple()) {
      var conc = typ.base.kind.Concrete;
      if (conc.tkind == .TyClass) {
        var tok = conc.variable.?.tokens.getLast();
        // list or map
        var exp: usize = if (tok.ty == .TkList) 1 else if (tok.ty == .TkMap) 2 else return;
        if (typ.tparams.items.len != exp) {
          tok.msg = "Generic type instantiated with wrong number of paramters";
          self.err(tok);
        }
      }
    }
    self.assertNonEmptyTParams(typ);
  }

  inline fn assertNonEmptyTParams(self: *Self, typ: *Generic) void {
    if (typ.tparams.items.len == 0) {
      self.previous_tok.msg = "Empty type parameters not allowed";
      self.err(self.previous_tok);
    }
  }

  inline fn assertUniqueTParams(self: *Self, alias: *Generic, param: *Type) void {
    for (alias.getSlice()) |typ| {
      // `param` and `typ` have Variable.tokens equal to size 1.
      var token = param.variable().tokens.getLast();
      if (std.mem.eql(u8, typ.variable().tokens.getLast().value, token.value)) {
        token.msg = "Redefinition of alias type parameter";
        self.err(token);
      }
    }
  }

  fn aliasParam(self: *Self) Type {
    var debug = self.current_tok;
    self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator, debug);
    typ.variable().append(debug);
    if (self.check(.TkDot)) {
      self.current_tok.msg = "Expected single identifier, found multiple";
      self.err(self.current_tok);
    }
    return typ;
  }

  fn builtinType(self: *Self) Type {
    // handle builtin list/map type
    var debug = self.current_tok;
    self.advance();
    var tvar = Variable.init(self.allocator);
    tvar.append(debug);
    var conc = Type.newConcrete(.TyClass, debug.value, debug);
    conc.kind.Concrete.name = debug.value;
    conc.kind.Concrete.variable = tvar;
    return Type.newGeneric(self.allocator, conc.box(self.allocator), debug);
  }

  fn refType(self: *Self) Type {
    self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator, self.previous_tok);
    typ.variable().append(self.previous_tok);
    while (self.match(.TkDot)) {
      self.consume(.TkIdent);
      typ.variable().append(self.previous_tok);
    }
    return typ;
  }

  fn builtinOrRefType(self: *Self) Type {
    if (self.check(.TkIdent)) {
      return self.refType();
    } else if (self.check(.TkList) or self.check(.TkMap)) {
      return self.builtinType();
    }
    var tkind: TypeKind = switch (self.current_tok.ty) {
      .TkBool => .TyBool,
      .TkNum => .TyNumber,
      .TkStr => .TyString,
      // TODO: func, method, class, instance
      else => {
        self.current_tok.msg = "Invalid type-start";
        self.err(self.current_tok);
      }
    };

    // direct 'unit' types such as listed above do not need names
    var typ = Type.newConcrete(tkind, null, self.current_tok);
    self.advance();
    return typ;
  }

  fn abstractType(self: *Self) Type {
    // AbstractType := TypeName
    // TypeName    := Ident TypeParams?
    // TypeParams  := "{" Ident ( "," Ident )* "}"
    self.consume(.TkIdent);
    var typ = Type.newVariable(self.allocator, self.previous_tok);
    typ.variable().append(self.previous_tok);
    if (self.match(.TkLCurly)) {
      var gen = Generic.init(self.allocator, typ.box(self.allocator));
      while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
        if (gen.tparams.items.len > 0) self.consume(.TkComma);
        self.assertMaxTParams(&gen);
        var param = self.aliasParam();
        self.assertUniqueTParams(&gen, &param);
        gen.append(param.box(self.allocator));
      }
      self.assertNonEmptyTParams(&gen);
      self.consume(.TkRCurly);
      return gen.toType(self.previous_tok);
    }
    return typ;
  }

  fn tPrimary(self: *Self) Type {
    // Primary  :=  Builtin | Reference | “(“ Expression “)” | Primary “?”
    var typ: Type = undefined;
    if (self.match(.TkLBracket)) {
      typ = self.tExpr();
      self.consume(.TkRBracket);
    } else {
      typ = self.builtinOrRefType();
    }
    return typ;
  }

  fn tGeneric(self: *Self) Type {
    // Generic := ( Primary | Primary "{" Expression ( "," Expression )* "}" ) "?"?
    var typ = self.tPrimary();
    if (self.match(.TkLCurly)) {
      if (typ.isSimple()) {
        self.previous_tok.msg = "Cannot instantiate simple type as generic";
        self.err(self.previous_tok);
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
        if (gen.tparams.items.len > 0) self.consume(.TkComma);
        self.assertMaxTParams(gen);
        var param = self.tExpr();
        gen.append(param.box(self.allocator));
      }
      self.assertNonEmptyTParams(gen);
      // check that builtin generic types are properly instantiated
      self.assertBuiltinExpTParams(gen);
      self.consume(.TkRCurly);
      typ = ret;
    } else if (typ.isGeneric()) {
      self.assertBuiltinExpTParams(typ.generic());
    }
    if (self.match(.TkQMark)) {
      var subtype = typ;
      if (subtype.isNullable()) {
        self.previous_tok.msg = "Nullable type cannot be nullable";
        self.err(self.previous_tok);
      }
      typ = Type.newNullable(subtype.box(self.allocator), self.previous_tok);
    }
    return typ;
  }

  fn tUnion(self: *Self) Type {
    // Union := Generic ( “|” Generic )*
    var typ = self.tGeneric();
    if (self.check(.TkPipe)) {
      var token = self.current_tok;
      var uni = Union.init(self.allocator);
      uni.set(typ.box(self.allocator));
      while (self.match(.TkPipe)) {
        typ = self.tGeneric();
        uni.set(typ.box(self.allocator));
      }
      return uni.toType(token);
    }
    return typ;
  }

  fn tExpr(self: *Self) Type {
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

  fn checkGenericTParam(self: *Self, tvar: *Type, rhs_ty: *Type) ?*lex.Token {
    // T{K} -> P{Q | K{V}}, here K is tvar
    switch (rhs_ty.kind) {
      .Generic => |*gen| {
        switch (gen.base.kind) {
          .Variable => |*vr| {
            if (vr.eql(&tvar.kind.Variable)) {
              return &vr.tokens.items[0];
            }
          },
          else => {}
        }
      },
      .Union => |*uni| {
        for (uni.variants.values()) |ty| {
          if (self.checkGenericTParam(tvar, ty)) |tok| {
            return tok;
          }
        }
      },
      .Nullable => |nul| {
        return self.checkGenericTParam(tvar, nul.subtype);
      },
      // .Variable => |*vr| {
      //   if (vr.eql(&tvar.kind.Variable)) {
      //     return &vr.tokens.items[0];
      //   }
      // },
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
            tok.msg = "type variable in generic parameter cannot be generic";
            self.err(tok.*);
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
      tok.msg = "type alias cannot be used directly in the aliasee";
      self.err(tok.*);
    }
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
    // check that generic type variable parameters in `AbstractType` are not generic in `ConcreteType`
    self.assertNoGenericParameterTypeVariable(&alias_typ, &aliasee.AstNType.typ);
    // TODO: should this be disallowed? It poses no issues at the moment.
    // self.assertNoDirectRecursiveAlias(&alias_typ, &aliasee.AstNType.typ);
    node.* = .{.AstAlias = ast.AliasNode.init(type_tok, &alias.AstNType, &aliasee.AstNType)};
    self.consumeNlOrEof();
    return node;
  }

  fn annotation(self: *Self, ident: *ast.VarNode) void {
    if (self.match(.TkColon)) {
      var typ_node = &self.typing(false).AstNType;
      typ_node.typ.ident = ident;
      ident.typ = &typ_node.typ;
    }
  }

  fn parseExpr(self: *Self) *Node {
    return self._parse(.Assignment);
  }

  fn blockStmt(self: *Self) *Node {
    const line = self.current_tok.line;
    self.consume(.TkDo);
    self.consume(.TkNewline);
    var node = self.newNode();
    node.* = .{.AstBlock = ast.BlockNode.init(self.allocator, line)};
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      util.append(*Node, &node.AstBlock.nodes, self.statement());
    }
    self.consume(.TkEnd);
    // eat newline if present
    _ = self.match(.TkNewline);
    return node;
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
    } else if (self.check(.TkDo)) {
      return self.blockStmt();
    } else if (self.match(.TkNewline)) {
      return self.statement();
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
