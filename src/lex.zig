const std = @import("std");
const prelude = @import("prelude.zig");
const OpCode = @import("opcode.zig").OpCode;
const VebAllocator = @import("allocator.zig");
const util = @import("util.zig");
pub const ks = @import("constants.zig");

pub const Keywords = std.StaticStringMap(TokenType).initComptime(.{
  .{"if", .TkIf},
  .{"in", .TkIn},
  .{"else", .TkElse},
  .{"elif", .TkElif},
  .{"for", .TkFor},
  .{"where", .TkWhere},
  .{"while", .TkWhile},
  .{"and", .TkAnd},
  .{"let", .TkLet},
  .{"do", .TkDo},
  .{"fn", .TkFn},
  .{"is", .TkIs},
  .{"or", .TkOr},
  .{"as", .TkAs},
  .{"def", .TkDef},
  .{"end", .TkEnd},
  .{"not", .TkNot},
  .{"case", .TkCase},
  .{"from", .TkFrom},
  .{"match", .TkMatch},
  .{"try", .TkTry},
  .{"alias", .TkAlias},
  .{"pub", .TkPub},
  .{"const", .TkConst},
  .{"type", .TkType},
  .{"then", .TkThen},
  .{"break", .TkBreak},
  .{"with", .TkWith},
  .{"class", .TkClass},
  .{"trait", .TkTrait},
  .{"import", .TkImport},
  .{"orelse", .TkOrElse},
  .{"extern", .TkExtern},
  .{"return", .TkReturn},
  .{"builtin", .TkBuiltin},
  .{"continue", .TkContinue},
  .{ks.OkVar, .TkOk},
  .{ks.SelfVar, .TkSelf},
  .{ks.VoidVar, .TkVoid},
  .{ks.NeverVar, .TkNever},
  .{ks.TrueVar, .TkTrue},
  .{ks.FalseVar, .TkFalse},
  .{ks.NumVar, .TkNum},
  .{ks.MapVar, .TkMap},
  .{ks.StrVar, .TkStr},
  .{ks.BoolVar, .TkBool},
  .{ks.JustVar, .TkJust},
  .{ks.NoneVar, .TkNone},
  .{ks.MaybeVar, .TkMaybe},
  .{ks.ResultVar, .TkResult},
  .{ks.ListVar, .TkList},
  .{ks.ErrorVar, .TkError},
  .{ks.AnyVar, .TkAny},
  .{ks.TupleVar, .TkTuple},
});

pub const TokenType = enum (u8) {
  TkPlus = 0,       // +
  TkMinus,          // -
  TkSlash,          // /
  TkStar,           // *
  TkLBracket,       // (
  TkRBracket,       // )
  TkLSqrBracket,    // [
  TkRSqrBracket,    // ]
  TkSemic,          // ;
  TkColon,          // :
  TkLthan,          // <
  TkGthan,          // >
  TkEqual,          // =
  TkLCurly,         // {
  TkRCurly,         // }
  TkAmp,            // &
  TkPerc,           // %
  TkComma,          // ,
  TkExMark,         // !
  TkCaret,          // ^
  TkPipe,           // |
  TkTilde,          // ~
  TkDot,            // .
  TkQMark,          // ?
  Tk2QMark,         // ??
  TkPipeGthan,      // |>
  TkEqGrt,          // =>
  Tk2Dot,           // ..
  TkGthanLthan,     // <>
  TkLeq,            // <=
  TkGeq,            // >=
  Tk2Eq,            // ==
  TkNeq,            // !=
  Tk2Lthan,         // <<
  Tk2Gthan,         // >>
  TkAs,             // as
  TkDo,             // do
  TkFn,             // fn
  TkIs,             // is
  TkIf,             // if
  TkIn,             // in
  TkOk,             // ok
  TkOr,             // or
  TkFor,            // for
  TkAnd,            // and
  TkDef,            // def
  TkEnd,            // end
  TkNot,            // not
  TkLet,            // let
  TkNum,            // num
  TkMap,            // map
  TkPub,            // pub
  TkStr,            // str
  TkError,          // error
  TkAny,            // any
  TkTry,            // try
  TkAlias,          // alias
  TkBool,           // bool
  TkJust,           // just
  TkNone,           // none
  TkList,           // list
  TkThen,           // then
  TkType,           // type
  TkElse,           // else
  TkElif,           // elif
  TkCase,           // case
  TkFrom,           // from
  TkTrue,           // true
  TkVoid,           // void
  TkSelf,           // self
  TkWith,           // with
  TkClass,          // class
  TkConst,          // const
  TkTrait,          // trait
  TkBreak,          // break
  TkFalse,          // false
  TkMatch,          // match
  TkMaybe,          // maybe
  TkTuple,          // tuple
  TkWhere,          // where
  TkWhile,          // while
  TkResult,         // result
  TkImport,         // import
  TkOrElse,         // orelse
  TkExtern,         // extern
  TkReturn,         // return
  TkBuiltin,        // builtin
  TkContinue,       // continue
  TkNever,          // never
  TkNumber,         // <number>
  TkString,         // <string>
  TkEscString,      // <string>
  TkIdent,          // <identifier>
  TkLexError,       // <error>
  TkEof,            // <eof>

  pub inline fn is(self: @This(), ty: @This()) bool {
    return self == ty;
  }

  pub fn optype(self: @This()) OpType {
    return switch (self) {
      .TkPlus => .OpAdd,
      .TkMinus => .OpSub,
      .TkSlash => .OpDiv,
      .TkStar => .OpMul,
      .TkLthan => .OpLess,
      .TkGthan => .OpGrt,
      .TkLeq => .OpLeq,
      .TkGeq => .OpGeq,
      .Tk2Eq => .OpEqq,
      .TkNeq => .OpNeq,
      .TkEqual => .OpAssign,
      .TkExMark => .OpNot,
      .TkPerc => .OpMod,
      .TkCaret => .OpBitXor,
      .TkPipe => .OpBitOr,
      .TkAmp => .OpBitAnd,
      .Tk2Lthan => .OpBitLShift,
      .Tk2Gthan => .OpBitRShift,
      .TkTilde => .OpBitInvert,
      .TkAnd => .OpAnd,
      .TkOr => .OpOr,
      .TkIs => .OpIs,
      else => unreachable,
    };
  }

  pub fn isAssignLikeOp(self: @This()) bool {
    return switch (self) {
      .TkPlus,
      .TkMinus,
      .TkStar,
      .TkSlash,
      .TkPerc,
      .TkAmp,
      .TkCaret,
      .TkPipe,
      .Tk2Lthan,
      .Tk2Gthan => true,
      else => false,
    };
  }

  // TokenType to string
  pub fn str(self: @This()) []const u8 {
    return switch (self) {
      .TkPlus => "+",
      .TkMinus => "-",
      .TkSlash => "/",
      .TkStar => "*",
      .TkLBracket => "(",
      .TkRBracket => ")",
      .TkLSqrBracket => "[",
      .TkRSqrBracket => "]",
      .TkSemic => ";",
      .TkColon => ":",
      .TkLthan => "<",
      .TkGthan => ">",
      .TkEqual => "=",
      .TkLCurly => "{",
      .TkRCurly => "}",
      .TkAmp => "&",
      .TkPerc => "%",
      .TkComma => ",",
      .TkExMark => "!",
      .TkCaret => "^",
      .TkPipe => "|",
      .TkTilde => "~",
      .TkDot => ".",
      .TkQMark => "?",
      .Tk2QMark => "??",
      .TkPipeGthan => "|>",
      .TkEqGrt => "=>",
      .Tk2Dot => "..",
      .TkGthanLthan => "<>",
      .TkLeq => "<=",
      .TkGeq => ">=",
      .Tk2Eq => "==",
      .TkNeq => "!=",
      .Tk2Lthan => "<<",
      .Tk2Gthan => ">>",
      .TkAs => "as",
      .TkIf => "if",
      .TkIn => "in",
      .TkOr => "or",
      .TkDo => "do",
      .TkFn => "fn",
      .TkIs => "is",
      .TkFor => "for",
      .TkAnd => "and",
      .TkDef => "def",
      .TkEnd => "end",
      .TkNot => "not",
      .TkLet => "let",
      .TkPub => "pub",
      .TkTry => "try",
      .TkThen => "then",
      .TkAlias => "alias",
      .TkType => "type",
      .TkElse => "else",
      .TkElif => "elif",
      .TkCase => "case",
      .TkFrom => "from",
      .TkWith => "with",
      .TkClass => "class",
      .TkConst => "const",
      .TkTrait => "trait",
      .TkBreak => "break",
      .TkOk => ks.OkVar,
      .TkTrue => ks.TrueVar,
      .TkNum => ks.NumVar,
      .TkMap => ks.MapVar,
      .TkStr => ks.StrVar,
      .TkError => ks.ErrorVar,
      .TkAny => ks.AnyVar,
      .TkBool => ks.BoolVar,
      .TkJust => ks.JustVar,
      .TkNone => ks.NoneVar,
      .TkList => ks.ListVar,
      .TkSelf => ks.SelfVar,
      .TkVoid => ks.VoidVar,
      .TkFalse => ks.FalseVar,
      .TkTuple => ks.TupleVar,
      .TkNever => ks.NeverVar,
      .TkMaybe => ks.MaybeVar,
      .TkResult => ks.ResultVar,
      .TkMatch => "match",
      .TkWhere => "where",
      .TkWhile => "while",
      .TkImport => "import",
      .TkOrElse => "orelse",
      .TkExtern => "extern",
      .TkReturn => "return",
      .TkBuiltin => "builtin",
      .TkContinue => "continue",
      .TkNumber => "<num>",
      .TkString => "<str>",
      .TkEscString => "<esc-str>",
      .TkIdent => "<ident>",
      .TkLexError => "<error>",
      .TkEof => "<eof>",
    };
  }
};

pub const OpType = enum (u8) {
  OpAdd,
  OpSub,
  OpDiv,
  OpMul,
  OpMod,
  OpEqq,
  OpNeq,
  OpLeq,
  OpGeq,
  OpLess,
  OpGrt,
  OpAssign,
  OpNot,
  OpBitXor,
  OpBitOr,
  OpBitAnd,
  OpBitLShift,
  OpBitRShift,
  OpBitInvert,
  OpAnd,
  OpOr,
  OpIs,
  OpIsNot,

  pub fn toInstOp(self: @This()) OpCode {
    return switch (self) {
      .OpAdd => OpCode.Add,
      .OpSub => OpCode.Sub,
      .OpDiv => OpCode.Div,
      .OpMul => OpCode.Mul,
      .OpMod => OpCode.Mod,
      .OpBitXor => OpCode.Xor,
      .OpBitOr => OpCode.Or,
      .OpBitAnd => OpCode.And,
      .OpBitLShift => OpCode.Shl,
      .OpBitRShift => OpCode.Shr,
      .OpBitInvert => OpCode.Inv,
      .OpAnd => OpCode.Jf,
      .OpOr => OpCode.Jt,
      .OpIs => OpCode.Is,
      .OpNot => OpCode.Not,
      .OpLess => OpCode.Cles,
      .OpGrt => OpCode.Cgrt,
      .OpLeq => OpCode.Cleq,
      .OpGeq => OpCode.Cgeq,
      .OpEqq => OpCode.Ceqq,
      .OpNeq => OpCode.Cneq,
      else => unreachable,
    };
  }

  pub fn getTraitName(self: @This()) ?[]const u8 {
    return switch (self) {
      .OpAdd => "Add",
      .OpSub => "Sub",
      .OpDiv => "Div",
      .OpMul => "Mul",
      .OpEqq, .OpNeq => "Eq",
      .OpLess, .OpGrt, .OpLeq, .OpGeq => "Ord",
      else => null,
    };
  }

  pub inline fn isEqCmpOp(self: @This()) bool {
    return switch (self) {
      .OpEqq, .OpNeq => true,
      else => false,
    };
  }
  
  pub inline fn isOrdCmpOp(self: @This()) bool {
    return switch (self) {
      .OpLess, .OpGrt, .OpLeq, .OpGeq => true,
      else => false,
    };
  }

  pub inline fn isCmpOp(self: @This()) bool {
    return switch (self) {
      .OpLess, .OpGrt, .OpLeq, .OpGeq, .OpEqq, .OpNeq => true,
      else => false,
    };
  }

  pub inline fn isLgcOp(self: @This()) bool {
    return switch (self) {
      .OpAnd, .OpOr => true,
      else => false,
    };
  }
};

pub const Optr = struct {
  ty: TokenType,
  file: u16,
  pos: u32,
  line: u32,

  pub fn init(tok: Token) @This() {
    return .{.ty = tok.ty, .pos = tok.offset, .line = tok.line, .file = tok.file};
  }

  pub inline fn token(self: Optr) Token {
    return Token.init(self.ty, self.pos, self.file, self.ty.str(), self.line);
  }

  pub inline fn str(self: @This()) []const u8 {
    return self.ty.str();
  }

  pub inline fn optype(self: @This()) OpType {
    return self.ty.optype();
  }
};

const U8Writer = util.U8Writer;

pub const TokenBit = struct {
  pos: u32,
  ty: TokenType,
  line: u32,
  file: u16,

  pub inline fn init(token: Token) TokenBit {
    return TokenBit{
      .pos = token.offset, .ty = token.ty,
      .line = @intCast(token.line), .file = token.file,
    };
  }

  pub inline fn toToken(self: TokenBit) Token {
    return Token.init(self.ty, self.pos, self.file, self.ty.str(), self.line);
  }
};

pub const IdentToken = struct {
  val: [*]const u8,
  len: u8,
  file: u8,
  line: u16,
  offset: u32,

  pub inline fn init(token: Token) IdentToken {
    return IdentToken{
      .val = token.val, .len = @intCast(token.len),
      .line = @intCast(token.line), .offset = @intCast(token.offset),
      .file = @intCast(token.file),
    };
  }

  pub inline fn lexeme(self: *const IdentToken) []const u8 {
    @setRuntimeSafety(false);
    return self.val[0..@intCast(self.len)];
  }

  pub inline fn toToken(self: IdentToken) Token {
    @setRuntimeSafety(false);
    return Token.init(.TkIdent, self.offset, @intCast(self.file), self.val[0..@intCast(self.len)], @intCast(self.line));
  }
};

pub const Token = struct {
  val: [*]const u8,
  offset: u32,
  len: u32,
  line: u32,
  file: u16,
  ty: TokenType,

  const DefaultToken = Token.init(.TkEof, 0, 1, "", 1);

  pub inline fn init(ty: TokenType, offset: u32, file: usize, val: []const u8, line: u32) Token {
    return Token{.val = val.ptr, .offset = offset, .len = @intCast(val.len), .ty = ty, .file = @intCast(file), .line = line};
  }

  pub inline fn init2(ty: TokenType, offset: u32, file: u16, val: []const u8, line: u32) Token {
    return Token{.val = val.ptr, .offset = offset, .len = @intCast(val.len), .ty = ty, .file = file, .line = line};
  }

  pub inline fn is(self: *const Token, ty: TokenType) bool {
    return self.ty == ty;
  }

  pub inline fn equal(self: *const Token, other: Token) bool {
    return self.offset == other.offset and self.ty == other.ty;
  }

  pub inline fn lexeme(self: *const Token) []const u8 {
    @setRuntimeSafety(false);
    return self.val[0..self.len];
  }

  /// check if a token has the same value as val ([]const u8 | Token)
  pub inline fn valueEql(self: *const Token, val: anytype) bool {
    @setRuntimeSafety(false);
    if (@TypeOf(val) == []const u8) {
      return std.mem.eql(u8, self.val[0..self.len], val);
    } else {
      return std.mem.eql(u8, self.val[0..self.len], val.lexeme());
    }
  }

  pub inline fn getDefaultToken() Token {
    return DefaultToken;
  }

  pub inline fn dupTk(self: Token, ty: TokenType) Token {
    return Token.init2(ty, self.offset, self.file, self.lexeme(), self.line);
  }

  pub inline fn isTkErr(self: Token) bool {
    return self.ty == .TkLexError;
  }

  pub inline fn isTkEof(self: Token) bool {
    return self.ty == .TkEof;
  }

  pub inline fn fromBinaryNode(node: anytype) Token {
    // node really is a BinaryNode
    return Token.init(node.op_tkty, node.op_offset, node.op_origin, node.op_tkty.str(), node.left.getToken().line);
  }

  pub inline fn tkFrom(self: *const @This(), val: []const u8, ty: TokenType) Token {
    return Token.init2(ty, self.offset, self.file, val, self.line);
  }

  pub fn parseNum(self: *const @This()) !f64 {
    const value = self.lexeme();
    if (value.len > 1 and value[0] == '0') {
      const ty = value[1];
      if (ty == 'x' or ty == 'o' or ty == 'b') {
        return @floatFromInt(try std.fmt.parseInt(i64, value, 0));
      }
    }
    return try std.fmt.parseFloat(f64, value);
  }

  fn _column(self: *const @This(), src: []const u8) usize {
    const start = self.offset;
    const offset = if (start == src.len) start - 1 else start;
    return (
      if (std.mem.lastIndexOf(u8, src[0..offset], "\n")) |col|
        offset - @as(u32, @intCast(col)) - 1
      else
        offset
    ) + self.len;
  }

  fn _getLine(self: *const @This(), src: []const u8) []const u8 {
    const start = self.offset;
    const offset = if (self.ty == .TkEof and start > 0) start - 1 else start;
    // walk backwards
    var start_col: usize = offset;
    while (start_col > 0): (start_col -= 1) {
      if (src[start_col] == '\n') {
        start_col += 1;
        break;
      }
    }
    if (src[start_col] == '\n') start_col += 1;
    var end_col: usize = start_col;
    // walk forwards
    for (src[start_col..]) |ch| {
      if (ch == '\n') break;
      end_col += 1;
    }
    return src[start_col..end_col];
  }

  pub fn column(self: *const @This(), src: []const u8) usize {
    return self._column(src);
  }

  pub fn getLine(self: *const @This(), src: []const u8) []const u8 {
    return self._getLine(src);
  }
};

pub const ParseMode = enum(u1) {
  Builtin = 0,
  User = 1,
};

/// A snapshot of the lexer's state at any point in lexing
pub const LexSnapShot = struct {
  at_error: bool,
  line: usize,
  column: usize,
  start: usize,
  current: usize,
  token: Token = undefined,
};

pub const Lexer = struct {
  line: usize,
  column: usize,
  current: usize,
  start: usize,
  file: usize,
  at_error: bool,
  src: []const u8,
  allocator: std.mem.Allocator,

  const Self = @This();
  const LexError = error {
    none,
    illegal_token,    // "Illegal token"
    invalid_dec_lit,  // "Invalid decimal literal"
    invalid_bin_lit,  // "Invalid binary literal"
    invalid_oct_lit,  // "Invalid octal literal"
    invalid_hex_lit,  // "Invalid hex literal"
    invalid_num_lit,  // "Invalid number literal"
    unclosed_string,  // "Unclosed string"
    unknown_token,    // "Unknown token"
    str_too_long,     // "String too long"
    name_too_long,    // "Name too long"
  };

  pub fn getError(code: LexError) []const u8 {
    return switch (code) {
      error.none => "",
      error.illegal_token => "Illegal token",
      error.invalid_dec_lit => "Invalid decimal literal",
      error.invalid_bin_lit => "Invalid binary literal",
      error.invalid_oct_lit => "Invalid octal literal",
      error.invalid_hex_lit => "Invalid hex literal",
      error.invalid_num_lit => "Invalid number literal",
      error.unclosed_string => "Unclosed string",
      error.unknown_token => "Unknown token",
      error.str_too_long => "String too long",
      error.name_too_long => "Name too long",
    };
  }

  pub fn init(src: []const u8, file: usize, allocator: std.mem.Allocator) Self {
    return Self {
      .line = 1,
      .column = 1,
      .start = 0,
      .current = 0,
      .at_error = false,
      .src = src,
      .file = file,
      .allocator = allocator,
    };
  }

  pub inline fn getErrorToken(self: *Self) Token {
    return self.newToken(.TkLexError);
  }

  inline fn atEnd(self: *Self) bool {
    return self.current >= self.src.len;
  }

  inline fn peek(self: *Self) u8 {
    return if (self.atEnd()) 0 else self.src[self.current];
  }

  inline fn peekN(self: *Self, n: usize) u8 {
    return if (self.current + n >= self.src.len) 0 else self.src[self.current + n];
  }

  inline fn isAlpha(self: *Self, char: u8) bool {
    _ = self;
    return std.ascii.isAlphanumeric(char) or char == '_' or char == '@';
  }

  inline fn match(self: *Self, char: u8) bool {
    if (self.peek() == char) {
      _ = self.advance();
      return true;
    }
    return false;
  }

  pub fn currentChar(self: *Self) u8 {
    return self.peek();
  }

  fn eofToken(self: *Self) Token {
    return self.newToken(.TkEof);
  }

  inline fn slice(self: *Self) []const u8 {
    @setRuntimeSafety(false);
    return self.src[self.start..self.current];
  }

  fn newToken(self: *Self, ty: TokenType) Token {
    return Token.init(ty, @intCast(self.start), self.file, self.src[self.start..self.current], @intCast(self.line));
  }

  fn advance(self: *Self) u8 {
    if (self.src[self.current] == '\n') {
      self.line += 1;
      self.column = 1;
    } else {
      self.column += 1;
    }
    self.current += 1;
    return self.src[self.current - 1];
  }

  inline fn adv(self: *Self) void {
    _ = self.advance();
  }

  fn skipComment(self: *Self) void {
    self.adv(); // skip '#'
    while (!self.atEnd() and self.peek() != '\n') {
      self.adv();
    }
  }
    
  fn skipWhitespace(self: *Self) void {
    while (true) {
      const char = self.peek();
      switch(char) {
        ' ', '\r', '\t', '\n' => self.adv(),
        '#' => self.skipComment(),
        else => return,
      }
    }
  }

  fn lexNum(self: *Self, start: u8) !Token {
    // handle:
    // hex: 0x[a-fA-F0-9]*
    // oct: 0o[1-7]*
    // bin: 0b[0-1]*
    const is_zero = start == '0';
    const current = self.peek();
    var err_ = LexError.invalid_dec_lit;
    if (is_zero and current == 'b') {
      // "0b" bin_int
      // bin_int <- [01] '_'? bin_int
      self.adv(); // skip 'b'
      if (self.peek() == '0' or self.peek() == '1') {
          while (self.peek() == '0' or self.peek() == '1') {
          self.adv();
          if (self.peek() == '_') {
            self.adv();
            if (self.peek() != '0' and self.peek() != '1') {
              return error.invalid_bin_lit;
            }
          }
        }
        return self.newToken(.TkNumber);
      }
    } else if (is_zero and current == 'o') {
      // "0o" oct_int
      // oct_int <- [0-7] '_'? oct_int
      self.adv(); // skip 'o'
      if ('0' <= self.peek() and self.peek() <= '7') {
        while (switch (self.peek()) {
          '0'...'7' => true,
          else => false,
        }) {
          self.adv();
          if (self.peek() == '_') {
            self.adv();
            if (!('0' <= self.peek() and self.peek() <= '7')) {
              return error.invalid_oct_lit;
            }
          }
        }
        return self.newToken(.TkNumber);
      }
    } else if (is_zero and current == 'x') {
      // "0x" hex_int
      // hex_int -> [0-9a-fA-F] '_'? hex_int
      self.adv(); // skip 'x'
      err_ = error.invalid_hex_lit;
      if (std.ascii.isHex(self.peek())) {
        while (std.ascii.isHex(self.peek())) {
          self.adv();
          if (self.peek() == '_') {
            self.adv();
            if (!std.ascii.isHex(self.peek())) {
              return err_;
            }
          }
        }
        return self.newToken(.TkNumber);
      }
    } else {
      // dec_int
      // dec_int "." dec_int ([eE] [-+]? dec_int)?
      // dec_int [eE] [-+]? dec_int
      // we know that `start` is a digit. The next character may be an '_' or a digit.
      // for example: 1_00_000 (start = 1).
      if (self.peek() == '_') { // 
        self.adv(); 
        if (!std.ascii.isDigit(self.peek())) {
          return err_;
        }
      }
      while (std.ascii.isDigit(self.peek())) {
        self.adv();
        if (self.peek() == '_') {
          self.adv();
          if (!std.ascii.isDigit(self.peek())) {
            return err_;
          }
        }
      }
      // "." dec_int ([eE] [-+]? dec_int)?
      if (self.peek() == '.') {
        if (self.peekN(1) == '.') {
          return self.newToken(.TkNumber);
        }
        // "." dec_int
        self.adv(); // skip '.'
        if (std.ascii.isDigit(self.peek())) {
          while (std.ascii.isDigit(self.peek())) {
            self.adv();
            if (self.peek() == '_') {
              self.adv();
              if (!std.ascii.isDigit(self.peek())) {
                return err_;
              }
            }
          }
        } else {
          // hack for: 0.e1234 i.e. dec+ "." e dec+ since zig supports this
          if (std.ascii.toLower(self.peek()) != 'e') return err_;
        }
      }
      if (std.ascii.toLower(self.peek()) == 'e') {
        // ([eE] [-+]? dec_int)?
        self.adv(); // skip 'e'
        if (self.peek() == '+' or self.peek() == '-') {
          self.adv();
        }
        if (std.ascii.isDigit(self.peek())) {
          while (std.ascii.isDigit(self.peek())) {
            self.adv();
            if (self.peek() == '_') {
              self.adv();
              if (!std.ascii.isDigit(self.peek())) {
                return err_;
              }
            }
          }
        } else {
          return err_;
        }
      }
      return self.newToken(.TkNumber);
    }
    return error.invalid_num_lit;
  }

  fn lexIdent(self: *Self) !Token {
    while (!self.atEnd() and self.isAlpha(self.src[self.current])): (_ = self.advance()) {}
    if (Keywords.get(self.slice())) |ty| {
      return self.newToken(ty);
    }
    if ((self.current - self.start) > ks.MAX_IDENT_LEN) {
      return error.name_too_long;
    }
    return self.newToken(.TkIdent);
  }

  fn lexStr(self: *Self, start: u8) !Token {
    var escaped = false;
    while (!self.atEnd() and self.peek() != start) {
      if (self.peek() == '\\') {
        // skip - so next char is skipped.
        self.adv();
        escaped = true;
      }
      self.adv();
    }
    if (self.atEnd()) {
      return error.unclosed_string;
    }
    // skip opening quot `"`
    self.start += 1;
    if ((self.current - self.start) > ks.MAX_STR_LEN) {
      return error.str_too_long;
    }
    const token = self.newToken(if (!escaped) .TkString else .TkEscString);
    // skip closing quot `"`
    self.adv();
    return token;
  }

  pub fn snapshot(self: *Self) LexSnapShot {
    return .{
      .at_error = self.at_error,
      .line = self.line,
      .column = self.column,
      .current = self.current,
      .start = self.start
    };
  }

  pub fn rewind(self: *Self, ss: LexSnapShot) void {
    self.at_error = ss.at_error;
    self.line = ss.line;
    self.column = ss.column;
    self.current = ss.current;
    self.start = ss.start;
  }

  pub fn getTentativeToken(self: *Self) Token {
    const ss = self.snapshot();
    const tok = self.getToken() catch self.newToken(.TkError);
    self.rewind(ss);
    return tok;
  }

  pub fn getToken(self: *Self) !Token {
    self.skipWhitespace();
    self.start = self.current;
    if (self.atEnd()) {
      return self.eofToken();
    }
    const ch = self.advance();
    if (std.ascii.isDigit(ch)) {
      return self.lexNum(ch);
    }
    if (std.ascii.isAlphabetic(ch) or ch == '_' or ch == '@') {
      return self.lexIdent();
    }
    return switch(ch) {
      '+' => self.newToken(.TkPlus),
      '-' => self.newToken(.TkMinus),
      '(' => self.newToken(.TkLBracket),
      ')' => self.newToken(.TkRBracket),
      '[' => self.newToken(.TkLSqrBracket),
      ']' => self.newToken(.TkRSqrBracket),
      '/' => self.newToken(.TkSlash),
      '*' => self.newToken(.TkStar),
      '&' => self.newToken(.TkAmp),
      '%' => self.newToken(.TkPerc),
      ',' => self.newToken(.TkComma),
      ';' => self.newToken(.TkSemic),
      ':' => self.newToken(.TkColon),
      '{' => self.newToken(.TkLCurly),
      '}' => self.newToken(.TkRCurly),
      '^' => self.newToken(.TkCaret),
      '~' => self.newToken(.TkTilde),
      '"', '\'' => self.lexStr(ch),
      '|' => self.newToken(if (self.match('>')) .TkPipeGthan else .TkPipe),
      '?' => self.newToken(if (self.match('?')) .Tk2QMark else .TkQMark),
      '.' => self.newToken(if (self.match('.')) .Tk2Dot else .TkDot),
      '!' => self.newToken(if (self.match('=')) .TkNeq else .TkExMark),
      '=' => self.newToken(if (self.match('=')) .Tk2Eq else if (self.match('>')) .TkEqGrt else .TkEqual),
      '<' => self.newToken(if (self.match('=')) .TkLeq else if (self.match('>')) .TkGthanLthan else if (self.match('<')) .Tk2Lthan else .TkLthan),
      '>' => self.newToken(if (self.match('=')) .TkGeq else if (self.match('>')) .Tk2Gthan else .TkGthan),
      else => error.unknown_token,
    };
  }
};
