const std = @import("std");
const lex = @import("lex.zig");
const ks = @import("constants.zig");
const tir = @import("tir.zig");
const util = @import("util.zig");
const ptn = @import("pattern.zig");
const diagnostics = @import("diagnostics.zig");
const VebAllocator = @import("allocator.zig");

const ds = tir.ds;
const Token = lex.Token;
const Node = tir.Node;
const Type = tir.Type;
const Generic = tir.Generic;
const Union = tir.Union;
const TaggedUnion = tir.TaggedUnion;
const Variable = tir.Variable;
const Concrete = tir.Concrete;
const Function = tir.Function;
const Class = tir.Class;
const TypeList = tir.TypeList;
const NodeItems = tir.NodeItems;
const TypeItems = tir.TypeItems;
const Diagnostic = diagnostics.Diagnostic;
const DiagLevel = diagnostics.DiagLevel;
const NodeList = tir.NodeList;
const Pattern = ptn.Pattern;
const ParseMode = lex.ParseMode;
pub const TypeKind = tir.TypeKind;

/// maximum number of elements of a list literal 
const MAX_LISTING_ELEMS = 0xff;
/// maximum number of callable parameters
const MAX_PARAMS = 0xff;
/// maximum number of class fields
const MAX_FIELDS = 0xff;
/// maximum number of class methods
const MAX_METHODS = 0xff;

pub const Parser = struct {
  current_tok: Token,
  previous_tok: Token,
  lexer: lex.Lexer,
  allocator: std.mem.Allocator,
  diag: Diagnostic,
  meta: ParseMeta,
  namegen: util.NameGen,

  const ParseMeta = struct {
    casts: u32 = 0,
    loops: u32 = 0,
    parens: u32 = 0,
    sugars: u32 = 0,
    pipes: u32 = 0,
    func: ?*Node = null,
    class: ?*Node = null,
    m_literals: ds.ArrayList(NameTuple),
    mode: ParseMode,
    in_type_decl: bool = false,
    in_labeled_call: bool = false,
    in_type_alias: bool = false,

    const NameTuple = struct{*Node, *Node, bool};

    fn init(al: std.mem.Allocator, mode: ParseMode) @This() {
      return .{.m_literals = ds.ArrayList(NameTuple).init(al), .mode = mode};
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
    Pipe,        // |>
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
    .{.bp = .Factor, .prefix = Self.placeholder, .infix = Self.binary}, // TkStar
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
    .{.bp = .None, .prefix = null, .infix = null},                      // Tk2QMark
    .{.bp = .Pipe, .prefix = null, .infix = Self.pipeline},             // TkPipeGthan
    .{.bp = .None, .prefix = null, .infix = null},                      // TkNewline
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEqGrt
    .{.bp = .None, .prefix = null, .infix = null},                      // Tk2Dot
    .{.bp = .Term, .prefix = null, .infix = Self.concat},               // TkGthanLthan
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkLeq
    .{.bp = .Comparison, .prefix = null, .infix = Self.binary},         // TkGeq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},           // Tk2Eq
    .{.bp = .Equality, .prefix = null, .infix = Self.binary},           // TkNeq
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},              // Tk2Lthan
    .{.bp = .Shift, .prefix = null, .infix = Self.binary},              // Tk2Rthan
    .{.bp = .Access, .prefix = null, .infix = Self.casting},            // TkAs
    .{.bp = .None, .prefix = null, .infix = null},                      // TkDo
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkFn
    .{.bp = .Equality, .prefix = null, .infix = Self.binaryIs},         // TkIs
    .{.bp = .None, .prefix = null, .infix = null},                      // TkIf
    .{.bp = .Or, .prefix = Self.variable, .infix = null},               // TkOk
    .{.bp = .Or, .prefix = null, .infix = Self.binary},                 // TkOr
    .{.bp = .None, .prefix = null, .infix = null},                      // TkFor
    .{.bp = .And, .prefix = null, .infix = Self.binary},                // TkAnd
    .{.bp = .None, .prefix = Self.funExpr, .infix = null},              // TkDef
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEnd
    .{.bp = .None, .prefix = null, .infix = null},                      // TkNot
    .{.bp = .None, .prefix = null, .infix = null},                      // TkLet
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkNum
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkMap
    .{.bp = .None, .prefix = null, .infix = null},                      // TkPub
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkStr
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkError
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkAny
    .{.bp = .Unary, .prefix = Self.tryExpr, .infix = null},             // TkTry
    .{.bp = .None, .prefix = null, .infix = null},                      // TkAlias
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkBool
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkJust
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkNone
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
    .{.bp = .None, .prefix = null, .infix = null},                      // TkTrait
    .{.bp = .None, .prefix = null, .infix = null},                      // TkBreak
    .{.bp = .None, .prefix = Self.boolean, .infix = null},              // TkFalse
    .{.bp = .None, .prefix = Self.matchExpr, .infix = null},            // TkMatch
    .{.bp = .None, .prefix = null, .infix = null},                      // TkMaybe
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkPanic
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkTuple
    .{.bp = .None, .prefix = null, .infix = null},                      // TkWhere
    .{.bp = .None, .prefix = null, .infix = null},                      // TkWhile
    .{.bp = .None, .prefix = null, .infix = null},                      // TkResult
    .{.bp = .Term, .prefix = null, .infix = Self.orElseExpr},           // TkOrElse
    .{.bp = .None, .prefix = null, .infix = null},                      // TkReturn
    .{.bp = .None, .prefix = null, .infix = null},                      // TkContinue
    .{.bp = .None, .prefix = Self.typing, .infix = null},               // TkNoReturn
    .{.bp = .None, .prefix = Self.number, .infix = null},               // TkNumber
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkString
    .{.bp = .None, .prefix = Self.string, .infix = null},               // TkEscString
    .{.bp = .None, .prefix = Self.variable, .infix = null},             // TkIdent
    .{.bp = .None, .prefix = null, .infix = null},                      // TkLexError
    .{.bp = .None, .prefix = null, .infix = null},                      // TkEof
  };

  pub fn init(src: *[]const u8, filename: *const[]const u8, mode: ParseMode, al: std.mem.Allocator) Self {
    return Self {
      .current_tok = undefined,
      .previous_tok = undefined,
      .lexer = lex.Lexer.init(src.*, mode, al),
      .diag = Diagnostic.init(al, filename, src),
      .namegen = util.NameGen.init(al),
      .meta = ParseMeta.init(al, mode),
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

  inline fn _errWithArgs(self: *Self, comptime level: DiagLevel, token: Token, comptime fmt: []const u8, args: anytype) void {
    const start = (comptime level.pretext()) ++ ": ";
    self.diag.addDiagnosticsWithLevel(level, token, start ++ fmt, args);
  }

  fn softErrArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
   self._errWithArgs(.DiagError, token, fmt, args);
  }

  fn softErrMsg(self: *Self, token: Token, msg: []const u8) void {
    self._errWithArgs(.DiagError, token, "{s}", .{msg});
  }

  pub fn softErrFmt(self: *Self, token: Token, diag_depth: u32, comptime spaces: u32, comptime fmt: []const u8, args: anytype) void {
    comptime var space = @as([]const u8, "");
    inline for(0..spaces) |_| {
      space = space ++ @as([]const u8, " ");
    }
    self.diag.addDiagnosticsWithDepth(token, diag_depth, space ++ fmt, args);
  }

  fn errMsg(self: *Self, token: Token, msg: []const u8) ParseError {
    self._errWithArgs(.DiagError, token, "{s}", .{msg});
    return error.ParseError;
  }

  fn errArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) ParseError {
   self._errWithArgs(.DiagError, token, fmt, args);
   return error.ParseError;
  }

  fn softWarnArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
   self._errWithArgs(.DiagWarn, token, fmt, args);
  }

  fn softWarnMsg(self: *Self, token: Token, msg: []const u8) void {
    self._errWithArgs(.DiagWarn, token, "{s}", .{msg});
  }

  fn warnMsg(self: *Self, token: Token, msg: []const u8) ParseError {
    self._errWithArgs(.DiagWarn, token, "{s}", .{msg});
    return error.ParseError;
  }

  fn warnArgs(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) ParseError {
   self._errWithArgs(.DiagWarn, token, fmt, args);
   return error.ParseError;
  }

  inline fn incParens(self: *Self) void {
    self.meta.parens += 1;
    self.lexer.allow_nl = self.meta.parens;
  }

  inline fn decParens(self: *Self) void {
    self.meta.parens -= 1;
    self.lexer.allow_nl = self.meta.parens;
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

  inline fn inParenthesizedExpr(self: *Self) bool {
    return self.meta.parens > 0;
  }

  inline fn skipNewlines(self: *Self) void {
    while (self.match(.TkNewline)) {}
  }

  inline fn check(self: *Self, ty: lex.TokenType) bool {
    return self.current_tok.ty == ty;
  }

  fn match(self: *Self, ty: lex.TokenType) bool {
    if (self.check(ty)) {
      self.advance() catch unreachable;
      return true;
    }
    return false;
  }

  inline fn snapshot(self: *Self) lex.LexSnapShot {
    var ss = self.lexer.snapshot();
    ss.token = self.current_tok;
    return ss;
  }

  inline fn rewind(self: *Self, ss: lex.LexSnapShot) void {
    self.lexer.rewind(ss);
    self.previous_tok = ss.token;
    self.current_tok = ss.token;
  }

  inline fn advance(self: *Self) !void {
    self.previous_tok = self.current_tok;
    self.current_tok = self.lexer.getToken() catch |e| {
      return self.errMsg(self.lexer.getErrorToken(), lex.Lexer.getError(e));
    };
  }

  fn consume(self: *Self, tty: lex.TokenType) !void {
    if (self.check(tty)) {
      try self.advance();
    } else {
      return self.errArgs(
        self.current_tok,  "expected token '{s}' but found '{s}'",
        .{tty.str(), self.current_tok.ty.str()}
      );
    }
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

  inline fn getDisambiguator(self: *Self, comptime T: type) std.StringHashMap(T) {
    return std.StringHashMap(T).init(self.allocator);
  }

  inline fn getNodeList(self: *Self) NodeList {
    return NodeList.init(self.allocator);
  }

  inline fn getTypeList(self: *Self) TypeList {
    _ = self;
    return TypeList.init();
  }

  inline fn initNodeListWith(self: *Self, item: *Node) NodeList {
    return NodeList.initWith(self.allocator, item);
  }

  fn toSlice(self: *Self, node: *Node) NodeItems {
    var slice = util.allocSlice(*Node, 1, self.allocator);
    slice[0] = node;
    return slice;
  }

  inline fn genName(self: *Self, start: []const u8) []const u8 {
    return self.namegen.generate("$.{s}", .{start});
  }

  inline fn createNode(self: *Self) *Node {
    return util.alloc(Node, self.allocator);
  }

  inline fn newObj(self: *Self, comptime T: type, data: anytype) *T {
    const n = util.alloc(T, self.allocator);
    n.* = data;
    return n;
  }

  inline fn newNode(self: *Self, data: anytype) *Node {
    return self.newObj(Node, data);
  }

  inline fn assertMaxTParams(self: *Self, len: usize) void {
    if (len >= tir.MAX_TPARAMS) {
      self.softErrMsg(self.current_tok, "maximum type parameters exceeded");
    }
  }

  inline fn assertMaxElements(self: *Self, len: usize, comptime msg: []const u8) void {
    if (len > MAX_LISTING_ELEMS) {
      self.softErrMsg(self.current_tok, "maximum number of " ++ msg ++ " exceeded");
    }
  }

  inline fn assertMaxArgs(self: *Self, len: usize, comptime d: []const u8) void {
    if (len > MAX_PARAMS) {
      self.softErrMsg(self.current_tok, "maximum " ++ d ++ " exceeded");
    }
  }

  inline fn assertNonEmptyTParams(self: *Self, len: usize) void {
    if (len == 0) {
      self.softErrMsg(self.previous_tok, "empty type parameters are not supported");
    }
  }

  inline fn assertBuiltinExpTParams(self: *Self, cls: *Class, typ: *Type, tok: Token) void {
    const exp: usize = (
      if (typ.isListTy()) 1
      else if (typ.isMapTy()) 2
      else return
    );
    if (cls.tparamsLen() != exp) {
      self.softErrArgs(
        tok, "generic type instantiated with wrong number of paramters. "
        ++ "Expected {} but found {}",
        .{exp, cls.tparamsLen()}
      );
    }
  }

  inline fn assertUniqueTParams(self: *Self, alias: *Generic, param: *Type) void {
    for (alias.getSlice()) |typ| {
      // `param` and `typ` have Variable.tokens equal to size 1.
      var token = param.variable().tokens.getLast();
      if (token.valueEql(typ.variable().tokens.getLast())) {
        return self.softErrMsg(token, "redefinition of type parameter");
      }
    }
  }

  fn checkGenericTParam(self: *Self, tvar: *Type, rhs_ty: *Type) ?Token {
    // T{K} -> P{Q | K{V}}, here K is tvar
    switch (rhs_ty.info) {
      .Generic => |*gen| {
        switch (gen.base.info) {
          .Variable => |*vr| {
            if (vr.eql(&tvar.info.Variable)) {
              return vr.getFirst();
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
      .Tag => |*tg| {
        for (tg.fieldSlice()) |tp| {
          if (self.checkGenericTParam(tvar, tp.typ)) |tok| {
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
      .TaggedUnion => |*uni| {
        for (uni.variants.items()) |ty| {
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
    switch (abs_ty.info) {
      .Generic => |*gen| {
        for (gen.getSlice()) |param| {
          if (self.checkGenericTParam(param, rhs_ty)) |tok| {
            return self.softErrMsg(tok, "type variable in generic parameter cannot be generic");
          }
        }
      },
      else => {}
    }
  }

  fn warnIfGenericParamsMatchesTagNames(self: *Self, abs_ty: *Type, rhs_ty: *Type) void {
    // warn if the type parameters in the abstract type is used as tags in the tagged union
    const warning = "type variable is used as a tag in its type definition.\n" 
      ++ "    If this is a mistake, consider renaming the type parameter.";
    switch (abs_ty.info) {
      .Generic => |*gen| {
        if (rhs_ty.isTaggedUnion()) {
          for (gen.getSlice()) |param| {
            const token = param.variable().tokens.getLast();
            for (rhs_ty.taggedUnion().variants.items()) |tag| {
              if (token.valueEql(tag.tag().name)) {
                return self.softWarnMsg(token, warning);
              }
            }
          }
        } else {
          for (gen.getSlice()) |param| {
            const token = param.variable().tokens.getLast();
            if (token.valueEql(rhs_ty.tag().name)) {
              return self.softWarnMsg(token, warning);
            }
          }
        }
      },
      else => {}
    }
  }

  inline fn validateMaybeTagCall(self: *Self, maybe: *Node, args: *NodeList) void {
    if (!maybe.isTVariable()) return;
    const ty = maybe.NdTVar.token.ty;
    if (ty == .TkJust or ty == .TkError or ty == .TkOk) {
      if (args.len() != 1) {
        self.softErrMsg(maybe.NdTVar.token, "expected tag parameter");
      }
    } else if (ty == .TkNone) {
      if (args.len() != 0) {
        self.softErrMsg(maybe.NdTVar.token, "unexpected tag parameter");
      }
    }
  }

  inline fn builtinTaggedUnionToIdent(self: *Self) void {
    if (self.check(.TkMaybe) or self.check(.TkResult)) {
      self.current_tok = self.current_tok.dupTk(.TkIdent);
    }
  }

  inline fn checkBuiltinTag(self: *Self) bool {
    return (
      self.check(.TkJust)
      or self.check(.TkNone)
      or self.check(.TkOk)
      or self.check(.TkError)
    );
  }

  inline fn matchBuiltinTag(self: *Self) bool {
    return (
      self.match(.TkJust)
      or self.match(.TkNone)
      or self.match(.TkOk)
      or self.match(.TkError)
    );
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
      const infix = ptable[@intFromEnum(self.current_tok.ty)].infix;
      if (infix == null) {
        return self.errMsg(self.current_tok, "token found at an invalid infix position");
      }
      node = try infix.?(self, node, assignable);
    }
    return node;
  }

   fn handleAugAssign(self: *Self, left: *Node, assignable: bool) !*Node {
    // assignments are not expressions, but statements
    if (!assignable or self.meta.in_labeled_call) return left;
    if (
      self.current_tok.ty.isAssignLikeOp() and
      self.lexer.currentChar() == '='
    ) {
      // +=, -=, ...=
      // e.g. var += expr => var = var + expr;
      const op_sign = self.current_tok;
      try self.advance(); // skip assignlike op
      const op_eq = self.current_tok;
      try self.advance(); // skip '=' op
      const value = try self.parseExpr();
      const right = self.newNode(.{.NdBinary = tir.BinaryNode.init(left, value, op_sign)});
      const node = self.newNode(.{.NdAssign = tir.BinaryNode.init(
        if (left.isDeref()) left.NdDeref.expr else left, right, op_eq
      )});
      // only ascertain that a newline or eof is present, if not, error
      if (!self.check(.TkNewline) and !self.check(.TkEof)) try self.consumeNlOrEof();
      // if present, exprStmt() would consume it for us.
      return node;
    } else if (self.match(.TkEqual)) {
      const token = self.previous_tok;
      const value = try self.parseExpr();
      const node = self.newNode(.{.NdAssign = tir.BinaryNode.init(
        if (left.isDeref()) left.NdDeref.expr else left, value, token
      )});
      // only ascertain that a newline or eof is present, if not, error
      if (!self.check(.TkNewline) and !self.check(.TkEof)) try self.consumeNlOrEof();
      // if present, exprStmt() would consume it for us.
      return node;
    } else {
      return left;
    }
  }

  fn variable(self: *Self, assignable: bool) !*Node {
    if (!self.matchBuiltinTag()) {
      if (!self.match(.TkIdent)) {
        try self.consume(.TkPanic);
      }
    }
    const ident = self.previous_tok;
    return try self.handleAugAssign(
      self.newNode(.{.NdTVar = tir.TVarNode.init(ident)}),
      assignable
    );
  }

  fn number(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    try self.consume(.TkNumber);
    const value = self.previous_tok.parseNum() catch b: {
      self.softErrMsg(self.previous_tok, "invalid number token");
      break :b 0;
    };
    return self.newNode(.{.NdNumber = tir.NumberNode.init(self.previous_tok, value)});
  }

  fn string(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    if (!self.match(.TkString)) try self.consume(.TkEscString);
    return self.newNode(.{.NdString = tir.SymNode.init(self.previous_tok)});
  }

  fn boolean(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const ty: lex.TokenType = if (self.check(.TkTrue)) .TkTrue else .TkFalse;
    try self.consume(ty);
    return self.newNode(.{.NdBool = tir.SymNode.init(self.previous_tok)});
  }

  fn placeholder(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    if (self.meta.pipes == 0) {
      self.softErrMsg(
        self.current_tok,
        "use of pipe placeholder outside a pipeline expression",
      );
    }
    try self.consume(.TkStar);
    return self.newNode(.{.NdPipeHolder = tir.SymNode.init(self.previous_tok)});
  }

  fn pipeline(self: *Self, lhs: *Node, assignable: bool) !*Node {
    self.meta.pipes += 1;
    defer self.meta.pipes -= 1;
    self.meta.sugars += 1;
    return self.binary(lhs, assignable);
  }

  fn concat(self: *Self, lhs: *Node, assignable: bool) !*Node {
    self.meta.sugars += 1;
    return self.binary(lhs, assignable);
  }

  fn unary(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const bp = ptable[@intFromEnum(self.current_tok.ty)].bp;
    const op = self.current_tok;
    const opty = op.ty.optype();
    try self.advance();
    const expr = try self._parse(bp);
    if (opty == .OpSub and expr.isNumberLiteral()) {
      expr.NdNumber.value = -expr.NdNumber.value;
      return expr;
    }
    // rewrite -expr to 0 - expr
    if (opty == .OpSub) {
      return self.newNode(.{
        .NdBinary = tir.BinaryNode.init(
          self.newNode(.{.NdNumber = tir.NumberNode.init(op, 0)}),
          expr, op,
        )});
    }
    return self.newNode(.{.NdUnary = tir.UnaryNode.init(expr, op)});
  }

  fn binary(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    const bp = ptable[@intFromEnum(self.current_tok.ty)].bp;
    const op = self.current_tok;
    try self.advance();
    return self.newNode(.{.NdBinary = tir.BinaryNode.init(lhs, try self._parse(bp), op)});
  }

  fn binaryIs(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    const op = self.current_tok;
    try self.advance();
    const is_not = self.match(.TkNot);
    const not_token = self.previous_tok;
    const node = self.newNode(.{.NdBinary = tir.BinaryNode.init(lhs, try self.typing(false), op)});
    if (!is_not) {
      return node;
    } else {
      return self.newNode(.{.NdUnary = tir.UnaryNode.init(node, not_token.dupTk(.TkExMark))});
    }
  }

  fn casting(self: *Self, lhs: *Node, assignable: bool) !*Node {
    _ = assignable;
    try self.consume(.TkAs);
    self.incCast();
    defer self.decCast();
    return self.newNode(.{.NdCast = tir.CastNode.init(lhs, try self.typingTypeNode())});
  }

  fn grouping(self: *Self, assignable: bool) !*Node {
    self.incParens();
    try self.consume(.TkLBracket);
    if (self.check(.TkRBracket)) {
      return try self.tupling(assignable, null);
    }
    const node = try self.parseExpr();
    if (self.match(.TkComma)) {
      return try self.tupling(assignable, node);
    }
    self.decParens();
    try self.consume(.TkRBracket);
    if (!self.match(.TkExMark)) {
      return node;
    } else {
      return self.newNode(.{.NdError = tir.ErrorNode.init(node)});
    }
  }

  fn tupling(self: *Self, assignable: bool, first: ?*Node) !*Node {
    _ = assignable;
    var items = self.getNodeList();
    if (first) |elem| {
      items.append(elem);
    }
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      items.append(try self.parseExpr());
      if (!self.check(.TkRBracket)) {
        try self.consume(.TkComma);
        if (self.check(.TkRBracket)) break;
      }
    }
    self.decParens();
    self.assertMaxElements(items.len(), "tuple elements");
    try self.consume(.TkRBracket);
    return self.newNode(.{.NdTuple = tir.ListNode.init(items.items())});
  }

  fn listing(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    var items = self.getNodeList();
    self.incParens();
    try self.consume(.TkLSqrBracket);
    while (!self.check(.TkEof) and !self.check(.TkRSqrBracket)) {
      if (items.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRSqrBracket)) {
          break;
        }
      }
      items.append(try self.parseExpr());
    }
    self.decParens();
    self.assertMaxElements(items.len(), "list elements");
    try self.consume(.TkRSqrBracket);
    return self.newNode(.{.NdList = tir.ListNode.init(items.items())});
  }

  fn mapping(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    self.incParens();
    try self.consume(.TkLCurly);
    var pairs = ds.ArrayList(tir.MapNode.Pair).init(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (pairs.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRCurly)) {
          break;
        }
      }
      const key = try self.parseExpr();
      try self.consume(.TkColon);
      const val = try self.parseExpr();
      pairs.append(.{.key = key, .value = val});
    }
    self.decParens();
    self.assertMaxElements(pairs.len() << 1, "map elements");
    try self.consume(.TkRCurly);
    return self.newNode(
      .{.NdMap = tir.MapNode.init(pairs.items())}
    );
  }

  fn indexing(self: *Self, left: *Node, assignable: bool) !*Node {
    self.incParens();
    try self.consume(.TkLSqrBracket);
    const index = try self.parseExpr();
    self.decParens();
    try self.consume(.TkRSqrBracket);
    return try self.handleAugAssign(
      self.newNode(.{.NdSubscript = tir.SubscriptNode.init(left, index)}),
      assignable
    );
  }

  fn dotderef(self: *Self, left: *Node, assignable: bool) !*Node {
    // expr.?
    const token = self.current_tok;
    try self.consume(.TkDot);
    if (self.match(.TkQMark)) {
      self.meta.sugars += 1;
      const node = self.newNode(.{.NdDeref = tir.DerefNode.init(left, token)});
      return try self.handleAugAssign(node, assignable);
    } else if (self.match(.Tk2QMark)) {
      const num = self.newNode(.{.NdNumber = tir.NumberNode.init(self.previous_tok, 0)});
      const deref = self.newNode(.{.NdDeref = tir.DerefNode.initAssertion(left, token)});
      const node = self.newNode(.{.NdDotAccess = tir.DotAccessNode.init(deref, num)});
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
    return try self.handleAugAssign(
      self.newNode(.{.NdDotAccess = tir.DotAccessNode.init(left, right)}),
      assignable
    );
  }

  fn selfExpr(self: *Self, assignable: bool) !*Node {
    if (self.meta.class == null) {
      self.softErrMsg(self.current_tok, "Use of 'self' outside class or trait statement");
    }
    if (self.meta.func == null) {
      self.softErrMsg(self.current_tok, "Use of 'self' outside method definition");
    }
    try self.consume(.TkSelf);
    return try self.handleAugAssign(
      self.newNode(.{.NdTVar = tir.TVarNode.init(self.previous_tok)}),
      assignable
    );
  }

  fn callExpr(self: *Self, left: *Node, assignable: bool) !*Node {
    _ = assignable;
    // CallExpr    :=  Expr TypeParams? "(" Params? ")"
    var targs: ?TypeItems = null;
    const in_labeled_call = self.meta.in_labeled_call;
    defer self.meta.in_labeled_call = in_labeled_call;
    if (self.match(.TkLCurly)) {
      targs = try self.typeParams();
    }
    self.incParens();
    try self.consume(.TkLBracket);
    var args = self.getNodeList();
    var start: Token = undefined;
    var labeled = false;
    self.meta.in_labeled_call = true;
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (args.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRBracket)) break;
      }
      start = self.current_tok;
      const arg = try self.parseExpr();
      if (self.match(.TkEqual)) {
        labeled = true;
        if (start.ty != .TkIdent) {
          self.softErrMsg(self.previous_tok, "invalid labeled argument");
        }
        const val = try self.parseExpr();
        args.append(self.newNode(.{.NdLblArg = tir.LblArgNode.init(start, val, arg)}));
      } else {
        args.append(arg);
      }
    }
    self.decParens();
    try self.consume(.TkRBracket);
    self.assertMaxArgs(args.len(), "arguments");
    self.validateMaybeTagCall(left, &args);
    // basic call
    var call = self.newNode(.{.NdBasicCall = tir.BasicCallNode.init(left, args.items())});
    call.NdBasicCall.labeled = labeled;
    if (targs) |_targs| {
      return self.newNode(.{.NdGenericCall = tir.GenericCallNode.init(call, _targs)});
    }
    return call;
  }

  fn blockExpr(self: *Self) !*Node {
    _ = self.match(.TkNewline);
    var nodes = self.getNodeList();
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      try self.addStatement(&nodes);
    }
    try self.consume(.TkEnd);
    return self.newNode(.{.NdBlock = tir.BlockNode.init(nodes.items())});
  }

  fn funExpr(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    return try self.funStmt(true, false, false, false);
  }

  fn tryExpr(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const _token = self.current_tok;
    self.meta.sugars += 1;
    try self.consume(.TkTry);
    const ok = try self._parse(ptable[@intFromEnum(_token.ty)].bp);
    const token = _token.tkFrom(self.genName("e"), .TkString);
    const evar = self.newNode(.{.NdTVar = tir.TVarNode.init(token)});
    var _err = @as(*Node, undefined);
    if (self.meta.func == null) {
      // panic in top-level code
      const panic = self.newNode(.{
        .NdTVar = tir.TVarNode.init(
          token.tkFrom("panic", .TkIdent)
        )});
      _err = self.newNode(.{.NdBasicCall = tir.BasicCallNode.init(panic, self.toSlice(evar))});
    } else {
      // return in function code
      _err = self.newNode(.{.NdRet = tir.RetNode.init(evar, token)});
    }
    // NdOrElse
    return self.newNode(.{.NdOrElse = tir.OrElseNode.init(ok, _err, &evar.NdTVar)});
  }

  fn matchExpr(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    try self.consume(.TkMatch);
    return self.matchStmt(false);
  }

  fn orElseExpr(self: *Self, left: *Node, assignable: bool) !*Node {
    // expr orelse |e|? expr | (do .. end)
    _ = assignable;
    self.meta.sugars += 1;
    if (left.isOrElse()) {
      self.softErrMsg(
        self.previous_tok,
        "try/orelse expression should not be used in an 'orelse' expression"
      );
    }
    try self.consume(.TkOrElse);
    var _evar: ?*tir.TVarNode = null;
    if (self.match(.TkPipe)) {
      try self.consume(.TkIdent);
      _evar = tir.TVarNode.init(self.previous_tok).box(self.allocator);
      try self.consume(.TkPipe);
    }
    return self.newNode(.{.NdOrElse = tir.OrElseNode.init(left, try self.parseExpr(), _evar)});
  }

  fn parseExpr(self: *Self) !*Node {
    return try self._parse(.Assignment);
  }

  fn aliasParam(self: *Self, can_have_bounds: bool) !Type {
    const debug = self.current_tok;
    try self.consume(.TkIdent);
    var typ = Type.newVariableWToken(debug, self.allocator);
    if (self.check(.TkDot)) {
      return self.errMsg(self.current_tok, "expected single identifier, found multiple");
    }
    if (can_have_bounds and self.match(.TkColon)) {
      var ty = try self.annotation();
      if (self.check(.TkPlus)) {
        var tmp = Union.init();
        tmp.set(ty, self.allocator);
        while (self.match(.TkPlus)) {
          tmp.set(try self.annotation(), self.allocator);
        }
        ty = tmp.toTypeBoxed(self.allocator);
      }
      typ.variable().bounds = ty;
    }
    return typ;
  }

  fn typeParams(self: *Self) !TypeItems {
    // assumes previously consumed token is TkLCurly
    var list = self.getTypeList();
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (list.isNotEmpty()) try self.consume(.TkComma);
      list.append(try self._typing(), self.allocator);
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(list.len());
    self.assertMaxTParams(list.len());
    return list.items();
  }

  fn abstractTypeParams(self: *Self, typ: *Type, can_have_bounds: bool) !Type {
    try self.consume(.TkLCurly);
    var gen = Generic.init(typ);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (gen.tparams.isNotEmpty()) try self.consume(.TkComma);
      var param = try self.aliasParam(can_have_bounds);
      self.assertUniqueTParams(&gen, &param);
      gen.append(param.box(self.allocator), self.allocator);
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(gen.tparamsLen());
    self.assertMaxTParams(gen.tparamsLen());
    return gen.toType();
  }

  fn abstractType(self: *Self, can_have_bounds: bool) !Type {
    // AbstractType   :=  TypeName
    // TypeName       :=  Ident AbsTypeParams?
    // AbsTypeParams  :=  "{" Ident ( "," Ident )* "}"
    try self.consume(.TkIdent);
    var typ = Type.newVariableWToken(self.previous_tok, self.allocator);
    return (
      if (!self.check(.TkLCurly)) typ
      else try self.abstractTypeParams(typ.box(self.allocator), can_have_bounds)
    );
  }

  fn constantType(self: *Self) !Type {
    // Constant  := StringLiteral | BooleanLiteral | NumberLiteral
    const kind: TypeKind = switch (self.current_tok.ty) {
      .TkTrue, .TkFalse => .TyBool, 
      .TkNumber => .TyNumber,
      .TkString, .TkEscString => .TyString,
      else => {
        return self.errMsg(self.current_tok, "invalid type-start");
      }
    };
    // direct 'unit' types such as listed above do not need names
    const typ = Type.newConstant(kind, self.current_tok.lexeme());
    try self.advance();
    return typ;
  }

  fn builtinType(self: *Self) !Type {
    // handle builtin list/map/tuple/err/str type
    try self.advance();
    var ty = Type.newClass(self.previous_tok.lexeme(), self.previous_tok.ty, self.allocator);
    ty.klass().builtin = true;
    return ty;
  }

  fn refType(self: *Self) !Type {
    try self.consume(.TkIdent);
    var typ = Type.newVariableWToken(self.previous_tok, self.allocator);
    while (self.match(.TkDot)) {
      try self.consume(.TkIdent);
      typ.variable().appendTok(self.previous_tok, self.allocator);
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
        .TkError => try self.builtinType(),
        else => self.errMsg(self.current_tok, "invalid type")
      };
    }
  }

  fn returnSig(self: *Self) !*Type {
    // ReturnSig   :=  ":" Type
    try self.consume(.TkColon);
    return self._typing();
  }

  fn funType(self: *Self) !Type {
    // Function    :=  "fn" AbsTypeParams "(" Params? ")" ReturnSig
    try self.consume(.TkFn);
    var params = TypeList.init();
    try self.consume(.TkLBracket);
    var fun = Function.init(undefined, null, null, self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (params.isNotEmpty()) try self.consume(.TkComma);
      params.append(try self._typing(), self.allocator);
    }
    try self.consume(.TkRBracket);
    fun.data.ret = (try self.returnSig());
    fun.data.params = params.items();
    return fun.toType();
  }

  fn refGeneric(self: *Self, typ: *Type) !Type {
    var ret: Type = undefined;
    var gen: *Generic = undefined;
    if (typ.isGeneric()) {
      ret = typ.*;
    } else {
      const tmp = Generic.init(typ.box(self.allocator));
      ret = tmp.toType();
    }
    gen = ret.generic();
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (gen.tparams.isNotEmpty()) try self.consume(.TkComma);
      var param = try self.tExpr();
      gen.append(param.box(self.allocator), self.allocator);
    }
    try self.consume(.TkRCurly);
    self.assertNonEmptyTParams(gen.tparamsLen());
    self.assertMaxTParams(gen.tparamsLen());
    return ret;
  }

  fn builtinGeneric(self: *Self, typ: *Type) !void {
    var cls = typ.klass();
    @call(.always_inline, Class.initTParams, .{cls});
    var list = TypeList.init();
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (list.isNotEmpty()) try self.consume(.TkComma);
      var param = try self.tExpr();
      list.append(param.box(self.allocator), self.allocator);
    }
    cls.initTParamSlice(list.items(), null);
    try self.consume(.TkRCurly);
    // immutable types can be empty
    if (!cls.immutable) self.assertNonEmptyTParams(cls.tparamsLen());
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
    self.builtinTaggedUnionToIdent();
    switch (self.current_tok.ty) {
      // Generic
      .TkIdent, .TkList, .TkMap, .TkTuple, .TkError => {
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
      .TkBool, .TkNum, .TkStr, .TkVoid, .TkNoReturn, .TkAny, => |ty| {
          const tkind: TypeKind = switch (ty) {
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
      // Constant
      else => {
        typ = try self.constantType();
      },
    }
    if (!self.meta.in_type_alias and self.match(.TkQMark)) {
      typ = Type.newTaggedNullable(
        typ.box(self.allocator),
        self.allocator, null
      ).*;
    }
    return typ;
  }

  fn tExpr(self: *Self) !Type {
    // Expression := Primary
    return self.tPrimary();
  }

  fn typing(self: *Self, assignable: bool) !*Node { 
    _ = assignable;
    const token = self.current_tok;
    const typ = try self.tExpr();
    return self.newNode(.{.NdType = tir.TypeNode.init(typ.box(self.allocator), token)});
  }

  fn typingTypeNode(self: *Self) !*tir.TypeNode {
    const token = self.current_tok;
    const typ = try self.tExpr();
    return util.box(
      tir.TypeNode,
      tir.TypeNode.init(typ.box(self.allocator), token),
      self.allocator
    );
  }

  inline fn _typing(self: *Self) !*Type { 
    return (try self.tExpr()).box(self.allocator);
  }

  const annotation = _typing;

  fn taggedTypeParam(self: *Self) !tir.Tag.TagField {
    // TaggedTypeParam := (ID ":")? TaggedType
    if (self.check(.TkIdent)) {
      const id = self.current_tok;
      const tok = self.lexer.getTentativeToken();
      if (tok.ty == .TkColon) {
        try self.advance(); // skip TkIdent
        try self.advance(); // skip TkColon
        return tir.Tag.TagField{.name = id, .typ = try self._typing()};
      }
    }
    return tir.Tag.TagField{.name = null, .typ = try self._typing()};
  }

  fn taggedPrimary(self: *Self) ParseError!Type {
    // ID (“(“TypeParam (“,” TypeParam)* “)”)? "?"?
    const id = self.current_tok;
    if (self.meta.in_type_decl) {
      // we only allow definition of Just & None in builtin-mode
      if (self.inBuiltinMode()) {
        if (!self.matchBuiltinTag()) {
          try self.consume(.TkIdent);
        }
      } else try self.consume(.TkIdent);
    } else try self.consume(.TkIdent);
    var typ = Type.newTag(id.lexeme(), id.ty);
    var disamb = self.getDisambiguator(u32);
    if (self.match(.TkLBracket)) {
      typ.tag().initFields(self.allocator);
      var labels = @as(usize, 0);
      var labeled = false;
      while (true) {
        if (typ.tag().fields.?.isNotEmpty()) {
          try self.consume(.TkComma);
          if (self.check(.TkRBracket)) break;
        }
        const param = try self.taggedTypeParam();
        if (param.name) |name| {
          labels += 1;
          labeled = true;
          const value = name.lexeme();
          if (disamb.get(value) != null) {
            self.softErrMsg(name, "duplicate param handle");
          } else {
            disamb.put(value, 1) catch {};
          }
        }
        typ.tag().appendField(param, self.allocator);
        if (self.check(.TkEof) or self.check(.TkRBracket)) break;
      }
      if (labeled and labels != typ.tag().fieldsLen()) {
        self.softErrMsg(id, "type with one or more labeled argument(s) must be completely labeled");
      }
      try self.consume(.TkRBracket);
    }
    return typ;
  }

  fn taggedUnion(self: *Self) !Type {
    // Union := TaggedPrimary ( “|” TaggedPrimary )*
    const stepped = self.match(.TkNewline);
    if (stepped) _ = self.match(.TkPipe);
    var typ = try self.taggedPrimary();
    if (stepped) _ = self.match(.TkNewline);
    if (self.check(.TkPipe)) {
      var disamb = self.getDisambiguator(u32);
      if (typ.isTag()) disamb.put(typ.tag().name, 1) catch {};
      var uni = TaggedUnion.init();
      uni.set(typ.box(self.allocator), self.allocator);
      var ss = @as(lex.LexSnapShot, undefined);
      while (self.match(.TkPipe)) {
        typ = try self.taggedPrimary();
        if (typ.isTag()) {
          if (disamb.get(typ.tag().name) != null) {
            self.softErrMsg(self.current_tok, "duplicate tag");
          } else {
            disamb.put(typ.tag().name, 1) catch {};
          }
        }
        uni.set(typ.box(self.allocator), self.allocator);
        ss = self.snapshot();
        if (stepped) _ = self.match(.TkNewline);
      }
      if (self.previous_tok.ty == .TkNewline) self.rewind(ss);
      return uni.toType(self.allocator);
    }
    return typ;
  }

  fn taggedExpr(self: *Self) !Type {
    // Expression := Union
    return try self.taggedUnion();
  }

  fn taggedTyping(self: *Self, assignable: bool) !*Node {
    _ = assignable;
    const token = self.current_tok;
    const typ = try self.taggedExpr();
    return self.newNode(.{.NdType = tir.TypeNode.init(typ.box(self.allocator), token)});
  }

  fn taggedTypingAnn(self: *Self) !*tir.TypeNode {
    const token = self.current_tok;
    const typ = try self.taggedExpr();
    return util.box(
      tir.TypeNode,
      tir.TypeNode.init(typ.box(self.allocator), token),
      self.allocator
    );
  }

  fn typeAlias(self: *Self) !*Node {
    // TypeAlias   := "alias" AbstractType "=" ConcreteType
    defer self.meta.in_type_alias = false;
    self.meta.in_type_alias = true;
    const alias_typ = (try self.abstractType(false)).box(self.allocator);
    const alias = util.box(
      tir.TypeNode,
      tir.TypeNode.init(alias_typ, self.current_tok),
      self.allocator
    );
    try self.consume(.TkEqual);
    const aliasee = try self.typingTypeNode();
    // check that generic type variable parameters in `AbstractType` are not generic in `ConcreteType`
    self.assertNoGenericParameterTypeVariable(alias_typ, aliasee.typ);
    try self.consumeNlOrEof();
    return self.newNode(.{.NdAlias = tir.AliasNode.init(alias, aliasee)});
  }

  fn typeDecl(self: *Self) !*Node {
    // “type” AbstractType = ID (“(“TypeParam (“,” TypeParam)* “)”)?  (“|”   ID (“(“TypeParam (“,” TypeParam)* “)”)?)*
    const name = self.current_tok;
    if (self.inBuiltinMode()) {
      self.builtinTaggedUnionToIdent();
    }
    const typ_name = (try self.abstractType(false)).box(self.allocator);
    const alias = util.box(tir.TypeNode, tir.TypeNode.init(typ_name, name), self.allocator);
    try self.consume(.TkEqual);
    self.meta.in_type_decl = true;
    defer self.meta.in_type_decl = false;
    const aliasee = try self.taggedTypingAnn();
    // check that generic type variable parameters in `AbstractType` are not generic in `ConcreteType`
    self.assertNoGenericParameterTypeVariable(typ_name, aliasee.typ);
    self.warnIfGenericParamsMatchesTagNames(typ_name, aliasee.typ);
    if (aliasee.typ.isTaggedUnion()) {
      // forbid a type whose name collides with its variants
      for (aliasee.typ.taggedUnion().variants.items()) |ty| {
        if (ty.tag().nameEql(name.lexeme())) {
          self.softErrArgs(name, "type with name '{s}' shadows one of its variants", .{name.lexeme()});
        }
      }
    }
    try self.consumeNlOrEof();
    return self.newNode(.{.NdAlias = tir.AliasNode.init(alias, aliasee)});
  }

  fn varDecl(self: *Self) !*Node {
    // let var (: type)? = expr
    try self.consume(.TkIdent);
    const ident = self.previous_tok;
    var anot_ty: ?*Type = null;
    if (self.match(.TkColon)) {
      anot_ty = try self.annotation();
      anot_ty.?.inferred = false;
    }
    try self.consume(.TkEqual);
    const val = try self.parseExpr();
    try self.consumeNlOrEof();
    return self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(ident, val, anot_ty)});
  }

  fn exprStmt(self: *Self) !*Node {
    self.meta.sugars = 0;
    const expr = try self.parseExpr();
    try self.consumeNlOrEof();
    return self.newNode(.{.NdExprStmt = tir.ExprStmtNode.initAll(expr, self.meta.sugars > 0)});
  }

  fn _ifStmt(self: *Self, parent_if: ?*tir.SimpleIfNode) !*Node {
    // if expr then? nl body (elif expr then? nl body)* else nl body end
    const cond = try self.parseExpr();
    _ = self.match(.TkThen);
    try self.consume(.TkNewline);
    var then_stmts = self.getNodeList();
    while (!self.check(.TkEof) and !self.check(.TkElif) and !self.check(.TkElse) and !self.check(.TkEnd)) {
      try self.addStatement(&then_stmts);
    }
    const then = self.newNode(.{.NdBlock = tir.BlockNode.init(then_stmts.items())});
    var ifnode = self.newNode(.{.NdSimpleIf = tir.SimpleIfNode.init(cond, then, undefined)});
    if (parent_if) |parent| {
      parent.els = ifnode;
    }
    if (self.check(.TkElif)) {
      while (self.match(.TkElif)) {
        _ = try self._ifStmt(&ifnode.NdSimpleIf);
      }
      return ifnode;
    }
    var els_stmts = self.getNodeList();
    if (self.match(.TkElse)) {
      try self.consume(.TkNewline);
      while (!self.check(.TkEof) and !self.check(.TkEnd)) {
        try self.addStatement(&els_stmts);
      }
    }
    try self.consume(.TkEnd);
    try self.consumeNlOrEof();
    ifnode.NdSimpleIf.els = self.newNode(.{.NdBlock = tir.BlockNode.init(els_stmts.items())});
    return ifnode;
  }

  fn ifStmt(self: *Self) !*Node {
    // if expr then? nl body (elif expr then? nl body)* else nl body end
    return self._ifStmt(null);
  }

  fn controlStmt(self: *Self) !*Node {
    if (!self.inLoop()) {
      self.softErrMsg(self.current_tok, "control statement used outside loop");
    }
    const token = self.current_tok;
    if (!self.match(.TkBreak)) {
      try self.consume(.TkContinue);
    }
    try self.consumeNlOrEof();
    return self.newNode(.{.NdControl = tir.ControlNode.init(token)});
  }

  fn whileStmt(self: *Self) !*Node {
    // while cond do? ... end
    self.incLoop();
    defer self.decLoop();
    const cond = try self.parseExpr();
    const then = try self.blockStmt(!self.check(.TkDo), false, false);
    return self.newNode(.{.NdWhile = tir.WhileNode.init(cond, then)});
  }

  fn funParams(self: *Self, variadic: *bool) !tir.ParamItems {
    // Params      :=  "(" Ident ":" Type ("," Ident ":" Type)* ")"
    var params = ds.ArrayListUnmanaged(*tir.ParamNode).init();
    var disamb = self.getDisambiguator(u32);
    if (self.match(.TkLBracket)) {
      var erred = false;
      while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
        if (params.isNotEmpty()) {
          try self.consume(.TkComma);
        }
        try self.consume(.TkIdent);
        const ident = self.previous_tok;
        if (disamb.get(ident.lexeme())) |_| {
          self.softErrMsg(
            ident,
            "duplicate parameter is illegal in parameter list"
          );
        } else {
          disamb.put(ident.lexeme(), 0) catch {};
        }
        if (!self.match(.TkStar)) {
          try self.consume(.TkColon);
          const ty = try self.annotation();
          params.append(tir.ParamNode.new(ident, ty, self.allocator), self.allocator);
        } else {
          variadic.* = true;
          try self.consume(.TkColon);
          const anot_ty = try self.annotation();
          var ty = Type.newBuiltinGenericClass(ks.ListVar, .TkList, self.allocator);
          ty.klass().appendTParam(anot_ty, self.allocator);
          ty.klass().immutable = true;
          params.append(tir.ParamNode.new(ident, ty, self.allocator), self.allocator);
          if (!self.check(.TkRBracket)) {
            if (!erred) {
              self.softErrMsg(
                self.current_tok,
                "variadic parameter should be last in a parameter list"
              );
              erred = true;
            }
          }
          break;
        }
      }
      try self.consume(.TkRBracket);
    }
    self.assertMaxArgs(params.len(), "parameters");
    return params.items();
  }

  fn whereClause(self: *Self, tparams: *TypeList) !void {
    // where T: Foo + Bar, U: Bar
    //  or
    // where
    //  T: Foo + Bar,
    //  U: Bar
    try self.consume(.TkWhere);
    self.skipNewlines();
    var params = TypeList.init();
    var disamb = self.getDisambiguator(u32);
    while (!self.check(.TkEof)) {
      if (params.isNotEmpty()) {
        if (self.check(.TkNewline)) {
          break;
        }
        try self.consume(.TkComma);
        const ss = self.snapshot();
        self.skipNewlines();
        if (!self.lexer.getTentativeToken().is(.TkColon)) {
          self.rewind(ss);
          break;
        }
      }
      var ty = try self.aliasParam(true);
      if (disamb.get(ty.variable().getFirstLexeme()) != null) {
        self.softErrMsg(ty.variable().getFirst(), "duplicate type parameter found");
      } else {
        disamb.put(ty.variable().getFirstLexeme(), 1) catch {};
      }
      params.append(ty.box(self.allocator), self.allocator);
    }
    self.assertMaxTParams(params.len());
    for (params.items()) |ty| {
      var resolved = false;
      for (tparams.items()) |_ty| {
        if (ty.variable().eql(_ty.variable())) {
          var typ = _ty.variable();
          var where_bounds = ty.variable().bounds.?;
          if (typ.bounds) |bounds| {
            if (bounds.isUnion()) {
              bounds.union_().set(where_bounds, self.allocator);
              typ.bounds = bounds.union_().toTypeBoxed(self.allocator);
            } else if (where_bounds.isUnion()) {
              where_bounds.union_().set(bounds, self.allocator);
              typ.bounds = where_bounds.union_().toTypeBoxed(self.allocator);
            } else {
              var uni = Type.newUnion();
              uni.union_().addSlice(&[_]*Type{bounds, where_bounds}, self.allocator);
              typ.bounds = uni.union_().toTypeBoxed(self.allocator);
            }
          } else {
            typ.bounds = where_bounds;
          }
          resolved = true;
          break;
        }
      }
      if (!resolved) {
        self.softErrMsg(ty.variable().getFirst(), "the type used here is not defined");
      }
    }
  }

  fn funStmt(self: *Self, lambda: bool, allow_pub: bool, is_method: bool, trait_fun: bool) !*Node {
    // FunDecl     :=  "def" TypeParams? Params? ReturnSig? NL Body End
    const is_pub = if (allow_pub) self.match(.TkPub) else if (trait_fun) true else false;
    try self.consume(.TkDef);
    const prev_func = self.meta.func;
    const name = if (!lambda) blk: {
      if (!self.inBuiltinMode() or !self.match(.TkPanic)) try self.consume(.TkIdent);
      break :blk self.previous_tok;
    } else null;
    var func = self.newNode(.{
      .NdBasicFun = tir.BasicFunNode.init(
        undefined, name, undefined, null, false, false, false, self.allocator
      )});
    self.meta.func = func;
    var tparams: ?TypeList = null;
    if (self.check(.TkLCurly)) {
      if (lambda) self.softErrMsg(self.current_tok, "generic lambdas are unsupported");
      if (is_method) self.softErrMsg(self.current_tok, "generic methods are unsupported");
      var tmp = try self.abstractTypeParams(undefined, !lambda and !is_method);
      tparams = tmp.generic().tparams;
    }
    var variadic = false;
    const params = try self.funParams(&variadic);
    const ret = (
      if (self.check(.TkColon)) try self.returnSig()
      else if (trait_fun and self.check(.TkSemic)) Type.newVoid().box(self.allocator)
      else null
    );
    const semic = self.current_tok;
    const body: *Node = blk: {
      if (trait_fun and self.match(.TkSemic)) {
        break :blk self.newNode(.{.NdBlock = tir.BlockNode.init(&[_]*Node{})});
      }
      if (!lambda) {
        if (!is_method and tparams != null) {
          const ss = self.snapshot();
          self.skipNewlines();
          if (self.check(.TkWhere)) {
            try self.whereClause(&tparams.?);
          } else {
            self.rewind(ss);
          }
        }
        break :blk try self.blockStmt(true, lambda, false);
      }
      if (self.match(.TkEqGrt)) {
        const rexp = self.newNode(.{.NdRet = tir.RetNode.init(try self.parseExpr(), self.previous_tok)});
        break :blk self.newNode(.{.NdBlock = tir.BlockNode.init(self.toSlice(rexp))});
      } else {
        break :blk try self.blockExpr();
      }
    };
    func.NdBasicFun.update(params, name, body, ret, false, variadic, is_pub);
    func.NdBasicFun.data.trait_fun = trait_fun;
    func.NdBasicFun.data.empty_trait_fun = semic.is(.TkSemic);
    self.meta.func = prev_func;
    if (tparams) |*tp| {
      return self.newNode(.{.NdGenericFun = tir.GenericFunNode.init(tp.items(), func)});
    } else {
      return func;
    }
  }

  fn returnStmt(self: *Self) !*Node {
    if (!self.inFun()) {
      self.softErrMsg(self.previous_tok, "return statement used outside function");
    }
    const token = self.previous_tok;
    var expr: ?*Node = null;
    if (!self.check(.TkNewline)) {
      expr = try self.parseExpr();
    }
    if (self.meta.func) |func| {
      const node = self.newNode(.{.NdRet = tir.RetNode.init(expr, token)});
      if (func.NdBasicFun.data.name != null) {
        try self.consumeNlOrEof();
      } else if (self.check(.TkNewline) or self.check(.TkEof)) {
        try self.advance();
      }
      return node;
    } else {
      return error.ParseError;
    }
  }

  inline fn isWildcardPtn(self: *Self, chars: []const u8) bool {
    _ = self;
    return std.mem.eql(u8, chars, "_");
  }

  inline fn errIfWildcard(self: *Self, node: *Node, msg: []const u8) void {
    if (self.isWildcardPtn(node.NdTVar.token.lexeme())) {
      self.softErrMsg(node.NdTVar.token, msg);
    }
  }

  inline fn literalCons(self: *Self, node: *Node, token: Token) !*Pattern {
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
      if (cons.tag == .List and self.match(.TkIdent)) {
        cons.node = self.newNode(.{.NdTVar = tir.TVarNode.init(self.previous_tok)});
      }
      _ = self.match(.TkComma);
      return true;
    }
    return false;
  }

  inline fn _numberPattern(self: *Self) !*Pattern {
    const token = self.current_tok;
    return self.literalCons(try self.number(false), token);
  }

  fn _numberOrRangePattern(self: *Self) !*Pattern {
    const token = self.current_tok;
    const p1 = try self._numberPattern();
    if (self.match(.Tk2Dot)) {
      //  low..high -> a if a >= low and a <= high
      const lhs = p1.variant.cons.node.?;
      const p2 = try self._numberPattern();
      const rhs = p2.variant.cons.node.?;
      const id = self.newNode(.{.NdTVar = tir.TVarNode.init(token.tkFrom(self.genName("id"), .TkIdent))});
      // create if a >= low and a <= high
      const op_leq = token.dupTk(.TkLeq);
      const op_geq = token.dupTk(.TkGeq);
      const op_and = token.dupTk(.TkAnd);
      const bin_lhs = self.newNode(.{.NdBinary = tir.BinaryNode.init(id, lhs, op_geq)});
      const bin_rhs = self.newNode(.{.NdBinary = tir.BinaryNode.init(id, rhs, op_leq)});
      const bin = self.newNode(.{.NdBinary = tir.BinaryNode.init(bin_lhs, bin_rhs, op_and)});
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
            ptn.Constructor.newTupleCons().toVariant(self.allocator),
            token,
            .{}
          ).box(self.allocator)
        );
      }
      // sequence_pattern (non-empty rest)
      if (self.check(.Tk2Dot)) {
        var cons = ptn.Constructor.newTupleCons();
        _ = self._restPattern(&cons);
        try self.consume(.TkRBracket);
        return self._finishPossibleRestPattern(cons, token);
      }
      // group_pattern
      const pat = try self._pattern();
      if (self.match(.TkRBracket)) {
        return pat;
      }
      // sequence_pattern (non-empty)
      var cons = ptn.Constructor.newTupleCons();
      var args = ptn.Patterns.initWith(self.allocator, pat);
      while (self.match(.TkComma)) {
        if (self._restPattern(&cons)) break;
        if (self.check(.TkRBracket)) break;
        args.append(try self._pattern());
      }
      cons.args = args.items();
      self.assertMaxArgs(cons.args.len, "sequence patterns");
      try self.consume(.TkRBracket);
      return self._finishPossibleRestPattern(cons, token);
    } else {
      token = self.current_tok;
      try self.consume(.TkLSqrBracket);
      // sequence_pattern (empty)
      if (self.match(.TkRSqrBracket)) {
        return (
          Pattern.init(
            ptn.Constructor.newListCons().toVariant(self.allocator),
            token,
            .{}
          ).box(self.allocator)
        );
      }
      // sequence_pattern (non-empty)
      var cons = ptn.Constructor.newListCons();
      var args = ptn.Patterns.init(self.allocator);
      while (!self.check(.TkEof) and !self.check(.TkRSqrBracket)) {
        if (args.isNotEmpty()) {
          try self.consume(.TkComma);
          if (self.check(.TkRSqrBracket)) break;
        }
        if (self._restPattern(&cons)) break;
        args.append(try self._pattern());
      }
      cons.args = args.items();
      self.assertMaxArgs(cons.args.len, "sequence patterns");
      try self.consume(.TkRSqrBracket);
      return self._finishPossibleRestPattern(cons, token);
    }
  }

  fn _capturePattern(self: *Self) !*Node {
    // capture_pattern: !"_" NAME !('.' | '(' | '=') 
    const res = try self.variable(false);
    self.errIfWildcard(res, "cannot use wildcard in capture pattern");
    return res;
  }

  fn _mappingPattern(self: *Self) !*Pattern {
    // '{' has been skipped
    const token = self.previous_tok;
    if (self.match(.TkRCurly)) {
      var map = Pattern.init(
        ptn.Constructor.newMapCons().toVariant(self.allocator),
        token, .{}
      ).box(self.allocator);
      map.variant.cons.append(
        Pattern.init(ptn.Constructor.newListCons().toVariant(self.allocator), token, .{}).box(self.allocator),
        self.allocator
      );
      return map;
    }
    var list = ptn.Constructor.newListCons();
    list.from_map = true;
    var args = ptn.Patterns.init(self.allocator);
    while (!self.check(.TkEof) and !self.check(.TkRCurly)) {
      if (args.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRCurly)) break;
      }
      if (self._restPattern(&list)) break;
      // key_value_pattern: | (literal_pattern | constant_pattern) ':' pattern
      var tok = self.current_tok;
      var key: *Pattern = switch (tok.ty) {
        // literal_pattern
        .TkNumber, .TkTrue, .TkFalse, .TkString, .TkMinus, => try self._pattern(),
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
      args.appendSlice(&[_]*Pattern{key, try self._pattern()});
    }
    list.args = args.items();
    self.assertMaxArgs(list.args.len, "mapping patterns");
    try self.consume(.TkRCurly);
    var map = ptn.Constructor.newMapCons();
    // {lit_ptn: ptn} ->
    // Map(List(Literal(...), Pattern))
    map.append(Pattern.init(list.toVariant(self.allocator), token, .{}).box(self.allocator), self.allocator);
    return Pattern.init(map.toVariant(self.allocator), token, .{}).box(self.allocator);
  }

  fn _classPattern(self: *Self) !*Pattern {
    // capture_pattern  | wildcard_pattern | class_pattern
    // class_pattern: name_or_attr ('{' Type Params '}')? '(' [pattern_arguments ','?] ')'
    if (self.isWildcardPtn(self.current_tok.lexeme())) {
      try self.advance();
      return (
        Pattern.init(
          ptn.Wildcard.init(self.previous_tok, false).toVariant(self.allocator),
          self.previous_tok,
          .{}
        ).box(self.allocator)
      );
    }
    const id = try self.variable(false);
    const token = id.NdTVar.token;
    if (!self.check(.TkLBracket) and !self.check(.TkLCurly)) {
      // check if this pattern starts with uppercase
      if (!std.ascii.isUpper(token.lexeme()[0])) {
        // at this point, this is a capture_pattern
        return (
          Pattern.init(
            ptn.Variable.init(id).toVariant(self.allocator),
            token,
            .{}
          ).box(self.allocator)
        );
      } else {
        // a possible tag type
        const cons = ptn.Constructor.newClassCons(token.lexeme(), token.ty, id);
        return Pattern.init(cons.toVariant(self.allocator), token, .{}).box(self.allocator);
      }
    }
    try self.consume(.TkLBracket);
    var cons = ptn.Constructor.newClassCons(token.lexeme(), token.ty, id);
    var start: Token = undefined;
    var lbl_token: Token = undefined;
    var disamb = self.getDisambiguator(u32);
    var args = ptn.Patterns.init(self.allocator);
    var labels: u32 = 0;
    while (!self.check(.TkEof) and !self.check(.TkRBracket)) {
      if (args.isNotEmpty()) {
        try self.consume(.TkComma);
        if (self.check(.TkRBracket)) break;
      }
      if (self._restPattern(&cons)) break;
      start = self.current_tok;
      const arg = try self._pattern();
      if (self.match(.TkEqual)) {
        lbl_token = start;
        if (!arg.isVariable()) {
          self.softErrMsg(start, "invalid field pattern");
          _ = try self._pattern(); // skip the ptn after '='
          continue;
        }
        labels += 1;
        const val = try self._pattern();
        const value = arg.variant.vari.ident.NdTVar.token.lexeme();
        if (disamb.get(value)) |_| {
          self.softErrMsg(start, "duplicate field pattern");
        } else {
          disamb.put(value, 1) catch {};
        }
        val.alat.field = arg.variant.vari.ident;
        args.append(val);
      } else {
        args.append(arg);
      }
    }
    cons.args = args.items();
    self.assertMaxArgs(cons.args.len, "arguments");
    try self.consume(.TkRBracket);
    if (labels > 0 and labels != args.len()) {
      self.softErrMsg(
        lbl_token,
        "label used inconsistently in pattern arguments"
      );
    }
    return self._finishPossibleRestPattern(cons, token);
  }

  fn _closedPattern(self: *Self) !*Pattern {
    //    closed_pattern: | literal_pattern | capture_pattern | wildcard_pattern 
    //                    | value_pattern | group_pattern | sequence_pattern 
    //                    | mapping_pattern | class_pattern
    // literal_pattern
    var token = self.current_tok;
    switch (self.current_tok.ty) {
      .TkTrue, .TkFalse => return self.literalCons(try self.boolean(false), token),
      .TkString => return self.literalCons(try self.string(false), token),
      .TkNumber => return self._numberOrRangePattern(),
      else => {}
    }

    // capture_pattern  | wildcard_pattern | class_pattern
    if (self.check(.TkIdent) or self.checkBuiltinTag()) {
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
    const token = self.current_tok;
    var pat = try self._closedPattern();
    if (self.check(.TkPipe)) {
      var cons = ptn.Constructor.newOrCons();
      var args = ptn.Patterns.init(self.allocator);
      args.append(pat);
      while (self.match(.TkPipe)) {
        args.append(try self._closedPattern());
      }
      cons.args = args.items();
      pat = Pattern.init(cons.toVariant(self.allocator), token, .{}).box(self.allocator);
    }
    return pat;
  }

  fn _asPattern(self: *Self) !*Pattern {
    // as_pattern: or_pattern 'as' capture_pattern
    const pat = try self._orPattern();
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

  fn caseStmt(self: *Self, is_stmt: bool) !*ptn.Case {
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
    var ret_token: Token = undefined;
    var body: *Node = undefined;
    if (is_stmt) {
      body = try self.statement();
    } else {
      ret_token = self.current_tok;
      body = try self._parse(.Or);
    }
    if (self.meta.m_literals.isNotEmpty()) {
      // take all {name, id} pairs, and generate conditions
      const op_and = token.dupTk(.TkAnd);
      var last: ?*Node = null;
      for (self.meta.m_literals.items()) |itm| {
        std.debug.assert(itm.@"2");
        const bin: *Node = itm.@"1";
        if (last) |nd| {
          last = self.newNode(.{.NdBinary = tir.BinaryNode.init(nd, bin, op_and)});
        } else {
          last = bin;
        }
      }
      if (guard) |gard| {
        last = self.newNode(.{.NdBinary = tir.BinaryNode.init(gard, last.?, op_and)});
      }
      guard = last;
    }
    if (guard != null and is_stmt) {
      if (!body.isBlock()) {
        body = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{body}), self.allocator);
      }
    }
    return self.newObj(ptn.Case, ptn.Case.init(pat, guard, body, false, self.allocator));
  }

  fn convertMatchExprToVar(self: *Self, m: *tir.MatchNode, token: Token) ?*Node {
    if (m.expr.isTVariable()) return null;
    // expr -> let $id = expr
    const tok = token.tkFrom(self.genName("m_expr"), .TkIdent);
    const m_expr = m.expr;
    m.expr = self.newNode(.{.NdTVar = tir.TVarNode.init(tok)});
    return self.newNode(.{.NdVarDecl = tir.VarDeclNode.init(tok, m_expr, null)});
  }

  fn matchStmt(self: *Self, is_stmt: bool) !*Node {
    // match_stmt: "match" match_expr 'with'? NEWLINE case_block+ "end"
    const tok = self.previous_tok;
    const expr = try self.parseExpr();
    _ = self.match(.TkWith);
    if (is_stmt or !self.inParenthesizedExpr()) {
      try self.consume(.TkNewline);
    } else {
      _ = self.match(.TkNewline);
    }
    if (is_stmt) self.skipNewlines();
    var cases = ptn.Case.CaseList.init(self.allocator);
    while (self.match(.TkCase)) {
      cases.append(try self.caseStmt(is_stmt));
      self.skipNewlines();
    }
    try self.consume(.TkEnd);
    if (cases.isEmpty()) {
      self.softErrMsg(tok, "match statement missing case arms");
    } else if (cases.len() > 1) {
      const last = cases.getLast();
      const red = self.newNode(.{.NdRedunMarker = tir.MarkerNode.init(last.pattern.token)});
      if (last.body.node.isBlock()) {
        last.body.node.block().prepend(red, self.allocator);
      } else {
        last.body.node = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{red, last.body.node}), self.allocator);
      }
    }
    var node = self.newNode(.{.NdMatch = tir.MatchNode.init(expr, cases.items())});
    if (self.convertMatchExprToVar(&node.NdMatch, tok)) |decl| {
      node = tir.BlockNode.newBlockWithNodes(@constCast(&[_]*Node{decl, node}), self.allocator);
    }
    return node;
  }

  fn blockStmt(self: *Self, skip_do: bool, skip_eat_nl: bool, add_scope: bool) anyerror!*Node {
    if (!skip_do) try self.consume(.TkDo);
    try self.consume(.TkNewline);
    var stmts = self.getNodeList();
    if (add_scope) {
      // enter scope
      stmts.append(self.newNode(.{.NdScope = tir.ScopeNode.init(true, false)}));
    }
    while (!self.check(.TkEof) and !self.check(.TkEnd)) {
      try self.addStatement(&stmts);
    }
    if (stmts.len() == 1 and add_scope) {
      // !Don't add scopes to blocks that are empty!
      _ = stmts.pop();
    } else if (add_scope) {
      // leave scope
      stmts.append(self.newNode(.{.NdScope = tir.ScopeNode.init(false, true)}));
    }
    try self.consume(.TkEnd);
    // eat newline if present
    if (!skip_eat_nl) {
      _ = self.match(.TkNewline);
    }
    var node = self.newNode(.{.NdBlock = tir.BlockNode.init(stmts.items())});
    return node;
  }

  fn classTypeAnnotation(self: *Self) !?*Type {
    if (self.match(.TkColon)) {
      var ty = try self.annotation();
      if (self.check(.TkPipe)) {
        var tmp = Union.init();
        tmp.set(ty, self.allocator);
        while (self.match(.TkPipe)) {
          tmp.set(try self.annotation(), self.allocator);
        }
        ty = tmp.toTypeBoxed(self.allocator);
      }
      return ty;
    }
    return null;
  }

  fn classStmt(self: *Self) !*Node {
    // ClassDecl           :=  "class" Ident TypeParams TypeAnnotation? ClassBody "end"
    const prev_cls = self.meta.class;
    defer self.meta.class = prev_cls;
    self.meta.class = @constCast(&@as(Node, .{.NdEmpty = tir.SymNode.init(self.previous_tok)}));
    switch (self.current_tok.ty) {
      .TkList, .TkError, .TkTuple, .TkMap, .TkStr => (
        if (self.inBuiltinMode()) try self.advance()
        else try self.consume(.TkIdent)
      ),
      else => try self.consume(.TkIdent)
    }
    const ident = self.previous_tok;
    var tparams: ?*TypeList = null;
    if (self.check(.TkLCurly)) {
      var tmp = try self.abstractTypeParams(undefined, true);
      tparams = tmp.generic().tparams.box(self.allocator);
    }
    const traits = try self.classTypeAnnotation();
    try self.consume(.TkNewline);
    self.skipNewlines();
    if (self.check(.TkWhere) and tparams != null) {
      try self.whereClause(tparams.?);
      self.skipNewlines();
    }
    // ClassBody
    // ClassFields
    var disamb = self.getDisambiguator(Token);
    var fields = self.getNodeList();
    while (self.check(.TkIdent) or self.check(.TkPub)) {
      const ss = self.snapshot();
      const is_pub = self.match(.TkPub);
      if (self.check(.TkDef)) {
        self.rewind(ss);
        break;
      }
      try self.consume(.TkIdent);
      const value = self.previous_tok.lexeme();
      if (disamb.get(value)) |tok| {
        self.softErrMsg(self.previous_tok, "illegal duplicate field");
        self.softErrFmt(tok, 4, 2, "Field also declared here:", .{});
      }
      if (fields.len() > MAX_FIELDS) {
        self.softErrMsg(self.previous_tok, "maximum number of field declarations exceeded");
      }
      const id = self.previous_tok;
      var val: ?*Node = null;
      const field_ty: ?*Type = if (self.match(.TkColon)) blk: {
        var ty = try self.annotation();
        ty.inferred = false;
        break :blk ty;
      } else null;
      if (self.match(.TkEqual)) {
        var tok = self.current_tok;
        val = try self.parseExpr();
        if (val.?.hasSugar()) {
          self.softErrMsg(tok, "field default initializer must be a compile-time known value");
        }
      }
      if (is_pub) {
        fields.append(self.newNode(.{.NdPubField = tir.PubFieldNode.init(id, val, field_ty)}));
      } else {
        fields.append(self.newNode(.{.NdField = tir.FieldNode.init(id, val, field_ty)}));
      }
      disamb.put(value, id) catch {};
      self.skipNewlines();
    }
    // ClassMethods
    var mdisamb = self.getDisambiguator(Token);
    var methods = self.getNodeList();
    while (self.check(.TkDef) or self.check(.TkPub)) {
      const method = try self.funStmt(false, true, true, false);
      const token = (
        if (method.isBasicFun()) method.NdBasicFun.data.name.?
        else method.NdGenericFun.fun.NdBasicFun.data.name.?
      );
      const value = token.lexeme();
      if (disamb.get(value)) |tok| {
        self.softErrArgs(token, "method conflicts with field '{s}'", .{value});
        self.softErrFmt(tok, 4, 2, "Field declared here:", .{});
      }
      if (mdisamb.get(value)) |tok| {
        self.softErrMsg(token, "illegal duplicate method");
        self.softErrFmt(tok, 4, 2, "Method also declared here:", .{});
      }
      if (method.isGenericFun()) {
        self.softErrMsg(token, "generic methods are unsupported");
      }
      if (methods.len() > MAX_METHODS) {
        self.softErrMsg(token, "maximum number of method declarations exceeded");
      }
      methods.append(method);
      mdisamb.put(value, token) catch {};
      self.skipNewlines();
    }
    self.skipNewlines();
    try self.consume(.TkEnd);
    try self.consumeNlOrEof();
    return self.newNode(.{.NdClass = tir.StructNode.init(
        ident, traits, fields.items(), methods.items(),
        if (tparams) |tp| tp.items() else null,
        false, false, self.allocator
      )});
  }

  fn traitStmt(self: *Self) !*Node {
    // TraitDecl           :=  "trait" Ident TypeParams TypeAnnotation? ";" | TraitBody "end"
    const prev_cls = self.meta.class;
    defer self.meta.class = prev_cls;
    self.meta.class = @constCast(&@as(Node, .{.NdEmpty = tir.SymNode.init(self.previous_tok)}));
    try self.consume(.TkIdent);
    const ident = self.previous_tok;
    var tparams: ?*TypeList = null;
    if (self.check(.TkLCurly)) {
      var tmp = try self.abstractTypeParams(undefined, true);
      tparams = tmp.generic().tparams.box(self.allocator);
    }
    // NOTE: Disable trait extension for now
    // const traits = try self.classTypeAnnotation();
    try self.consume(.TkNewline);
    // TraitMethods
    var mdisamb = self.getDisambiguator(Token);
    var methods = self.getNodeList();
    while (self.check(.TkDef) or self.check(.TkPub)) {
      const method = try self.funStmt(false, true, true, true);
      const token = (
        if (method.isBasicFun()) method.NdBasicFun.data.name.?
        else method.NdGenericFun.fun.NdBasicFun.data.name.?
      );
      const value = token.lexeme();
      if (mdisamb.get(value)) |tok| {
        self.softErrMsg(token, "illegal duplicate method");
        self.softErrFmt(tok, 4, 2, "Method also declared here:", .{});
      }
      if (methods.len() > MAX_METHODS) {
        self.softErrMsg(token, "maximum number of method declarations exceeded");
      }
      methods.append(method);
      mdisamb.put(value, token) catch {};
      self.skipNewlines();
    }
    self.skipNewlines();
    try self.consume(.TkEnd);
    try self.consumeNlOrEof();
    return self.newNode(.{.NdTrait = tir.StructNode.init(
        ident, null, &[_]*Node{}, methods.items(),
        if (tparams) |tp| tp.items() else null,
        false, false, self.allocator
      )});
  }

  fn statement(self: *Self) !*Node {
    if (self.match(.TkLet)) {
      return self.varDecl();
    } else if (self.match(.TkAlias)) {
      return self.typeAlias();
    } else if (self.match(.TkType)) {
      return self.typeDecl();
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
      return self.funStmt(false, false, false, false);
    } else if (self.match(.TkReturn)) {
      return self.returnStmt();
    } else if (self.match(.TkClass)) {
      return self.classStmt();
    } else if (self.match(.TkTrait)) {
      return self.traitStmt();
    } else if (self.match(.TkMatch)) {
      return self.matchStmt(true);
    }
    return self.exprStmt();
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
    const stmt = self.statement() catch |e| {
      if (e != error.EmptyStatement) {
        return e;
      }
      return;
    };
    list.append(stmt);
  }

  pub fn parse(self: *Self, display_diag: bool) !*Node {
    try self.advance();
    var entry = self.createNode();
    var list = self.getNodeList();
    while (!self.match(.TkEof)) {
      self.addStatement(&list) catch self.recover();
    }
    if (self.diag.hasAny()) {
      const has_error = self.diag.hasErrors();
      if (display_diag) self.diag.display();
      if (has_error) {
        return error.ParseError;
      }
    }
    entry.* = .{.NdProgram = tir.ProgramNode.init(list.items())};
    return entry;
  }
};
