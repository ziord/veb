const std = @import("std");
const OpCode = @import("opcode.zig").OpCode;
const NovaAllocator = @import("allocator.zig");

const keywords = std.ComptimeStringMap(TokenType, .{
  .{"return", .TkReturn},
  .{"if", .TkIf},
  .{"else", .TkElse},
  .{"elif", .TkElif},
  .{"for", .TkFor},
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
  .{"true", .TkTrue},
  .{"false", .TkFalse},
  .{"num", .TkNum},
  .{"map", .TkMap},
  .{"str", .TkStr},
  .{"nil", .TkNil},
  .{"try", .TkTry},
  .{"bool", .TkBool},
  .{"list", .TkList},
  .{"err", .TkErr},
  .{"type", .TkType},
  .{"tuple", .TkTuple},
  .{"then", .TkThen},
  .{"break", .TkBreak},
  .{"void", .TkVoid},
  .{"orelse", .TkOrElse},
  .{"continue", .TkContinue},
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
  TkNewline,        // \n
  TkEqGrt,          // =>
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
  TkOr,             // or
  TkFor,            // for
  TkAnd,            // and
  TkDef,            // def
  TkEnd,            // end
  TkNot,            // not
  TkLet,            // let
  TkNum,            // num
  TkMap,            // map
  TkStr,            // str
  TkNil,            // nil
  TkErr,            // err
  TkTry,            // try
  TkBool,           // bool
  TkList,           // list
  TkThen,           // then
  TkType,           // type
  TkElse,           // else
  TkElif,           // elif
  TkTrue,           // true
  TkVoid,           // void
  TkBreak,          // break
  TkFalse,          // false
  TkTuple,          // tuple
  TkWhile,          // while
  TkOrElse,         // orelse
  TkReturn,         // return
  TkContinue,       // continue
  TkNumber,         // <number>
  TkString,         // <string>
  TkAllocString,    // <string>
  TkIdent,          // <identifier>
  TkError,          // <error>
  TkEof,            // <eof>


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
      .TkNewline => "<newline>",
      .TkEqGrt => "=>",
      .TkLeq => "<=",
      .TkGeq => ">=",
      .Tk2Eq => "==",
      .TkNeq => "!=",
      .Tk2Lthan => "<<",
      .Tk2Gthan => ">>",
      .TkAs => "as",
      .TkIf => "if",
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
      .TkNum => "num",
      .TkMap => "map",
      .TkStr => "str",
      .TkNil => "nil",
      .TkErr => "err",
      .TkTry => "try",
      .TkBool => "bool",
      .TkList => "list",
      .TkThen => "then",
      .TkType => "type",
      .TkElse => "else",
      .TkElif => "elif",
      .TkTrue => "true",
      .TkVoid => "void",
      .TkBreak => "break",
      .TkFalse => "false",
      .TkTuple => "tuple",
      .TkWhile => "while",
      .TkOrElse => "orelse",
      .TkReturn => "return",
      .TkContinue => "continue",
      .TkNumber => "<number>",
      .TkString, .TkAllocString => "<string>",
      .TkIdent => "<identifier>",
      .TkError => "<error>",
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
      else => unreachable, // todo
    };
  }

  pub fn isCmpOp(self: @This()) bool {
    return switch (self) {
      .OpLess, .OpGrt, .OpLeq, .OpGeq, .OpEqq, .OpNeq => true,
      else => false,
    };
  }

  pub fn isLgcOp(self: @This()) bool {
    return switch (self) {
      .OpAnd, .OpOr => true,
      else => false,
    };
  }
};

pub const Optr = struct {
  optype: OpType,
  token: Token,

  pub fn init(token: Token) @This() {
    return @This() {.optype = token.ty.optype(), .token = token};
  }
};

pub const Token = struct {
  ty: TokenType,
  line: usize,
  offset: usize,
  value: []const u8,

  pub inline fn is(self: @This(), ty: TokenType) bool {
    return self.ty == ty;
  }

  pub fn isEof(self: @This()) bool {
    return self.is(.TkEof);
  }

  pub fn eql(self: @This(), other: @This()) bool {
    return self.offset == other.offset and self.ty == other.ty;
  }

  pub fn isErr(self: @This()) bool {
    return self.is(.TkError);
  }

  pub fn parseNum(self: @This()) !f64 {
    if (self.value.len > 1 and self.value[0] == '0') {
      const ty = self.value[1];
      if (ty == 'x' or ty == 'o' or ty == 'b') {
        return @intToFloat(f64, try std.fmt.parseInt(i64, self.value, 0));
      }
    }
    return try std.fmt.parseFloat(f64, self.value);
  }

  pub fn column(self: @This(), src: []const u8) usize {
    return (
      if (std.mem.lastIndexOf(u8, src[0..self.offset], "\n")) |col|
        self.offset - col - 1
      else 
        self.offset
    ) + self.value.len 
      + @boolToInt((self.ty == .TkString or self.ty == .TkAllocString)); // quote
  }

  pub fn isAlloc(self: @This()) bool {
    return self.ty == .TkAllocString;
  }

  pub fn getLine(self: @This(), src: []const u8) []const u8 {
    var offset: usize = if (self.ty == .TkNewline or self.ty == .TkEof) self.offset - 1 else self.offset;
    // walk backwards
    var start_col: usize = offset;
    while (start_col > 0): (start_col -= 1) {
      if (src[start_col] == '\n') {
        start_col += 1;
        break;
      }
    }
    var end_col: usize = start_col;
    // walk forwards
    for (src[start_col..]) |ch| {
      if (ch == '\n') break;
      end_col += 1;
    }
    return src[start_col..end_col];
  }

  fn printSquig(self: @This(), i: usize) void {
    _ = self;
    var y = i;
    while (y > 0) {
      std.debug.print("{s:^}", .{"^"});
      y -= 1;
    }
  }

  pub fn getDefault() Token {
    return Token {
      .ty = TokenType.TkEof,
      .value = "",
      .line = 0,
      .offset = 1,
    };
  }

  pub fn tokenFrom(token: *const Token) Token {
    var new: Token = undefined;
    new = token.*;
    return new;
  }

  pub fn showError(self: @This(), filename: []const u8, src: []const u8, comptime fmt: []const u8, args: anytype) void {
    var loc = self.getLine(src);
    var col = self.column(src);
    std.debug.print(fmt ++ "\n", args);
    std.debug.print("{s}.{}:{}:\n\t{s}\n", .{filename, self.line, col, loc});
    std.debug.print("\t", .{});
    var i = if (col >= self.value.len) col - self.value.len else self.value.len - col;
    while (i > 0) {
      std.debug.print(" ", .{});
      i -= 1;
    }
    self.printSquig(if (self.value.len != 0) self.value.len else 1);
    std.debug.print("\n", .{});
  }
};

pub const Lexer = struct {
  line: usize,
  column: usize,
  current: usize,
  start: usize,
  at_error: bool,
  src: []const u8,
  allocator: std.mem.Allocator,
  allow_nl: usize = 0,

  const Self = @This();

  pub fn init(src: []const u8, allocator: *NovaAllocator) Self {
    return Self {
      .line = 1,
      .column = 1,
      .start = 0,
      .current = 0,
      .at_error = false,
      .src = src,
      .allocator = allocator.getAllocator(),
    };
  }

  inline fn curr(self: *Self) u8 {
    return if (self.atEnd()) 0 else self.src[self.current];
  }

  inline fn atEnd(self: *Self) bool {
    return self.current >= self.src.len;
  }

  inline fn peek(self: *Self) u8 {
    return if (self.atEnd()) 0 else self.src[self.current];
  }

  inline fn isAlpha(self: *Self, char: u8) bool {
    _ = self;
    return std.ascii.isAlphanumeric(char) or char == '_';
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

  fn newToken(self: *Self, ty: TokenType) Token {
    return Token {
      .ty = ty,
      .line = self.line,
      .value = self.src[self.start..self.current],
      .offset = self.start,
    };
  }

  fn eofToken(self: *Self) Token {
    return self.newToken(.TkEof);
  }

  fn errToken(self: *Self, cause: ?[]const u8) Token {
    var token = self.newToken(.TkError);
    token.value = cause orelse "Illegal token";
    return token;
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

  fn skipWhitespace(self: *Self) !void {
    while (true) {
      const char = self.peek();
      switch(char) {
        ' ', '\r', '\t' => self.adv(),
        '\n' => {
          if (self.allow_nl > 0) self.adv() else return;
        },
        '#' => {
         self.skipComment();
        },
        else => return,
      }
    }
  }

  fn lexNum(self: *Self, start: u8) Token {
    // handle:
    // hex: 0x[a-fA-F0-9]*
    // oct: 0o[1-7]*
    // bin: 0b[0-1]*
    var is_zero = start == '0';
    var current = self.peek();
    var err_token = self.errToken("Invalid decimal literal");
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
              return self.errToken("Invalid binary literal");
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
              return self.errToken("Invalid octal literal");
            }
          }
        }
        return self.newToken(.TkNumber);
      }
    } else if (is_zero and current == 'x') {
      // "0x" hex_int
      // hex_int -> [0-9a-fA-F] '_'? hex_int
      self.adv(); // skip 'x'
      err_token.value = "Invalid hex literal";
      if (std.ascii.isHex(self.peek())) {
        while (std.ascii.isHex(self.peek())) {
          self.adv();
          if (self.peek() == '_') {
            self.adv();
            if (!std.ascii.isHex(self.peek())) {
              return err_token;
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
          return err_token;
        }
      }
      while (std.ascii.isDigit(self.peek())) {
        self.adv();
        if (self.peek() == '_') {
          self.adv();
          if (!std.ascii.isDigit(self.peek())) {
            return err_token;
          }
        }
      }
      // "." dec_int ([eE] [-+]? dec_int)?
      if (self.peek() == '.') {
        // "." dec_int
        self.adv(); // skip '.'
        if (std.ascii.isDigit(self.peek())) {
          while (std.ascii.isDigit(self.peek())) {
            self.adv();
            if (self.peek() == '_') {
              self.adv();
              if (!std.ascii.isDigit(self.peek())) {
                return err_token;
              }
            }
          }
        } else {
          // hack for: 0.e1234 i.e. dec+ "." e dec+ since zig supports this
          if (std.ascii.toLower(self.peek()) != 'e') return err_token;
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
                return err_token;
              }
            }
          }
        } else {
          return err_token;
        }
      }
      return self.newToken(.TkNumber);
    }
    return self.errToken("Invalid number literal");
  }

  fn lexIdent(self: *Self) Token {
    while (!self.atEnd() and self.isAlpha(self.src[self.current])): (_ = self.advance()) {}
    var token = self.newToken(.TkIdent);
    if (keywords.get(token.value)) |ty| {
      token.ty = ty;
    }
    return token;
  }

  inline fn convHex(self: *Self, ch: u8) u8 {
    _ = self;
    if ('0' <= ch and ch <= '9') {
      return ch - '0';
    }
    if ('a' <= ch and ch <= 'f') {
      return ch - 'a' + 10;
    }
    return ch - 'A' + 10;
  }

  fn lexStr(self: *Self, start: u8) Token {
    var escapes: u32 = 0;
    while (!self.atEnd() and self.peek() != start) {
      if (self.peek() == '\\') {
        // skip - so next char is skipped.
        self.adv();
        escapes += 1;
      }
      self.adv();
    }
    if (self.atEnd()) {
      return self.errToken("Unclosed string");
    }
    var token = self.newToken(.TkString);
    token.value = token.value[1..]; // skip opening quot `"`
    self.adv(); // skip closing quot `"`
    // check for escape sequences
    if (std.mem.indexOf(u8, token.value, "\\") == null) return token;
    var buf = self.allocator.alloc(u8, token.value.len - escapes) catch {
      return self.errToken("could not allocate string with escape sequence");
    };
    @memset(buf, 0);
    var len = token.value.len;
    var i: u32 = 0;
    var idx: u32 = 0;
    while (idx < len) : (idx += 1) {
      var char = token.value[idx];
      if (char == '\\') {
        var next = token.value[idx + 1];
        switch (next) {
          'a' => buf[i] = 7,    // '\a'
          'b' => buf[i] = 8,    // '\b'
          'f' => buf[i] = 12,   // '\f'
          'n' => buf[i] = '\n', // '\n'
          'r' => buf[i] = '\r', // '\r'
          't' => buf[i] = '\t', // '\a'
          'v' => buf[i] = 11,   // '\v'
          else => |ch| buf[i] = ch,
        }
        idx += 1;
      } else {
        buf[i] = char;
      }
      i += 1;
    }
    token.value = buf;
    token.ty = .TkAllocString;
    return token;
  }

  pub fn getToken(self: *Self) Token {
    self.skipWhitespace() catch {
      return self.errToken("Unclosed comment");
    };
    self.start = self.current;
    if (self.atEnd()) {
      return self.eofToken();
    }
    var ch = self.advance();
    if (std.ascii.isDigit(ch)) {
      return self.lexNum(ch);
    }
    if (std.ascii.isAlphabetic(ch) or ch == '_') {
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
      '|' => self.newToken(.TkPipe),
      '~' => self.newToken(.TkTilde),
      '.' => self.newToken(.TkDot),
      '?' => self.newToken(.TkQMark),
      '\n' => self.newToken(.TkNewline),
      '"', '\'' => self.lexStr(ch),
      '!' => self.newToken(if (self.match('=')) .TkNeq else .TkExMark),
      '=' => self.newToken(if (self.match('=')) .Tk2Eq else if (self.match('>')) .TkEqGrt else .TkEqual),
      '<' => self.newToken(if (self.match('=')) .TkLeq else if (self.match('<')) .Tk2Lthan else .TkLthan),
      '>' => self.newToken(if (self.match('=')) .TkGeq else if (self.match('>')) .Tk2Gthan else .TkGthan),
      else => self.errToken("Unknown token"),
    };
  }
};
