const std = @import("std");
const OpCode = @import("opcode.zig").OpCode;

const keywords = std.ComptimeStringMap(TokenType, .{
  .{"return", .TkReturn},
  .{"if", .TkIf},
  .{"else", .TkElse},
  .{"for", .TkFor},
  .{"while", .TkWhile},
  .{"and", .TkAnd},
  .{"or", .TkOr},
  .{"true", .TkTrue},
  .{"false", .TkFalse},
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
  TkLeq,            // <=
  TkGeq,            // >=
  Tk2Eq,            // ==
  TkNeq,            // !=
  Tk2Lthan,         // <<
  Tk2Gthan,         // >>
  TkIf,             // if
  TkOr,             // or
  TkFor,            // for
  TkAnd,            // and
  TkElse,           // else
  TkTrue,           // true
  TkFalse,          // false
  TkWhile,          // while
  TkReturn,         // return
  TkNum,            // <number>
  TkStr,            // <string>
  TkIdent,          // <identifier>
  TkErr,            // <error>
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
      else => unreachable,
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
      .TkLeq => "<=",
      .TkGeq => ">=",
      .Tk2Eq => "==",
      .TkNeq => "!=",
      .Tk2Lthan => "<<",
      .Tk2Gthan => ">>",
      .TkIf => "if",
      .TkOr => "or",
      .TkFor => "for",
      .TkAnd => "and",
      .TkElse => "else",
      .TkTrue => "true",
      .TkFalse => "false",
      .TkWhile => "while",
      .TkReturn => "return",
      .TkNum => "<number>",
      .TkStr => "<string>",
      .TkIdent => "<identifier>",
      .TkErr => "<error>",
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
      .OpNot => OpCode.Not,
      .OpLess, .OpGrt, .OpLeq, .OpGeq, .OpEqq, .OpNeq => OpCode.Cmp,
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

pub const Token = struct {
  ty: TokenType,
  value: []const u8,
  msg: ?[]const u8,
  line: usize,
  column: usize,
  is_alloc: bool = false,

  pub inline fn is(self: @This(), ty: TokenType) bool {
    return self.ty == ty;
  }

  pub fn isEof(self: @This()) bool {
    return self.is(.TkEof);
  }

  pub fn isErr(self: @This()) bool {
    return self.is(.TkErr);
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
};

pub const Lexer = struct {
  line: usize,
  column: usize,
  current: usize,
  start: usize,
  at_error: bool,
  src: []const u8,
  allocator: std.mem.Allocator,

  const Self = @This();

  pub fn init(src: []const u8, allocator: std.mem.Allocator) Self {
    return Self {
      .line = 1,
      .column = 1,
      .start = 0,
      .current = 0,
      .at_error = false,
      .src = src,
      .allocator = allocator,
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

  fn newToken(self: *Self, ty: TokenType) Token {
    return Token {
      .ty = ty,
      .line = self.line,
      .column = self.column - 1,
      .value = self.src[self.start..self.current],
      .msg = null,
    };
  }

  fn eofToken(self: *Self) Token {
    return self.newToken(.TkEof);
  }

  fn errToken(self: *Self, cause: ?[]const u8) Token {
    var token = self.newToken(.TkErr);
    token.msg = cause orelse "Illegal token";
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
        ' ', '\n', '\r', '\t' => _ = self.advance(),
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
        return self.newToken(.TkNum);
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
        return self.newToken(.TkNum);
      }
    } else if (is_zero and current == 'x') {
      // "0x" hex_int
      // hex_int -> [0-9a-fA-F] '_'? hex_int
      self.adv(); // skip 'x'
      err_token.msg = "Invalid hex literal";
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
        return self.newToken(.TkNum);
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
      return self.newToken(.TkNum);
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

  fn lexStr(self: *Self) Token {
    while (!self.atEnd() and self.peek() != '"') {
      if (self.peek() == '\\') {
        // skip - so next char is skipped.
        self.adv();
      }
      self.adv();
    }
    if (self.atEnd()) {
      return self.errToken("Unclosed string");
    }
    var token = self.newToken(.TkStr);
    token.value = token.value[1..]; // skip opening quot `"`
    self.adv(); // skip closing quot `"`
    // check for escape sequences
    if (std.mem.indexOf(u8, token.value, "\\") == null) return token;
    var buf = self.allocator.alloc(u8, token.value.len) catch {
      return self.errToken("could not allocate string with escape sequence");
    };
    std.mem.set(u8, buf, 0);
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
    token.is_alloc = true;
    return token;
  }

  pub fn getLine(self: *Self, line: usize) []const u8 {
    var curr_line: usize = 0;
    var start_col: usize = 0;
    for (self.src) |ch, idx| {
      if (curr_line == line - 1) {
        start_col = idx;
        break; 
      }
      if (ch == '\n') curr_line += 1;
    }
    var end_col: usize = start_col;
    for (self.src[start_col..]) |ch| {
      if (ch == '\n') break;
      end_col += 1;
    }
    return self.src[start_col..end_col];
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
      '{' => self.newToken(.TkLCurly),
      '}' => self.newToken(.TkRCurly),
      '^' => self.newToken(.TkCaret),
      '|' => self.newToken(.TkPipe),
      '~' => self.newToken(.TkTilde),
      '"' => self.lexStr(),
      '!' => self.newToken(if (self.match('=')) .TkNeq else .TkExMark),
      '=' => self.newToken(if (self.match('=')) .Tk2Eq else .TkEqual),
      '<' => self.newToken(if (self.match('=')) .TkLeq else if (self.match('<')) .Tk2Lthan else .TkLthan),
      '>' => self.newToken(if (self.match('=')) .TkGeq else if (self.match('>')) .Tk2Gthan else .TkGthan),
      else => self.errToken("Unknown token"),
    };
  }
};
