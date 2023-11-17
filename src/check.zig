pub const std = @import("std");
pub const ast = @import("ast.zig");
pub const link = @import("link.zig");
pub const util = @import("util.zig");
pub const types = link.types;
const diagnostics = @import("diagnostics.zig");
const analysis = @import("analysis.zig");
const flow = @import("flow.zig");
const ds = @import("ds.zig");
const parse = @import("parse.zig");
const ptn = @import("pattern.zig");
const prelude = @import("prelude.zig");
const VebAllocator = @import("allocator.zig");
const ks = @import("constants.zig");
const NodeList = ast.AstList;
const TypeHashMap = std.StringHashMap(*Type);
const FlowNode = flow.FlowNode;
const FlowList = flow.FlowList;
const FlowMeta = flow.FlowMeta;
const CFG = flow.CFG;
const CFGBuilder = flow.CFGBuilder;
const Scope = link.Scope;
const Token = link.Token;
const RelationContext = types.RelationContext;
const TypeKind = link.TypeKind;
const TypeInfo = link.TypeInfo;
const Union = link.Union;
const Node = link.Node;
const TContext = link.TContext;
const TypeLinker = link.TypeLinker;
const TypeHashSet = types.TypeHashSet;
pub const Type = link.Type;
pub const Class = types.Class;
pub const TypeList = types.TypeList;
const Pattern = ptn.Pattern;
const MatchCompiler = ptn.MatchCompiler;
const Diagnostic = diagnostics.Diagnostic;
const Analysis = analysis.Analysis;
const U8Writer = util.U8Writer;
const keywords = ast.lex.Keywords;
pub const logger = std.log.scoped(.check);

const TypeEnv = struct {
  global: Scope,
  narrowed: TypeHashMap,

  pub fn init(allocator: std.mem.Allocator, other: ?*TypeEnv) @This() {
    var self = @This() {
      .global = Scope.init(allocator),
      .narrowed = TypeHashMap.init(allocator)
    };
    if (other) |source| {
      self.copy(source);
    }
    return self;
  }

  pub fn display(self: *@This(), prompt: []const u8, u8w: *U8Writer) void {
    std.debug.print("{s}\n", .{prompt});
    var glob = self.global.decls;
    std.debug.print("------------Globals------------\n", .{});
    var i: usize = self.global.len();
    while (i > 0): (i -= 1) {
      var itr = glob.itemAt(i - 1).map.iterator();
      while (itr.next()) |entry| {
        std.debug.print("name: '{s}', ty: '{s}'\n", .{entry.key_ptr.*, entry.value_ptr.*.typename(u8w)});
      }
      std.debug.print("-------------------------------\n", .{});
    }
    std.debug.print("------------Narrowed------------\n", .{});
    var itr_n = self.narrowed.iterator();
    while (itr_n.next()) |entry| {
      var ty = entry.value_ptr.*;
      std.debug.print("name: '{s}', ty: '{s}'\n", .{entry.key_ptr.*, ty.typename(u8w)});
    }
    std.debug.print("-------------------------------\n", .{});
  }

  pub fn copy(self: *@This(), source: *@This()) void {
    // globals don't need to be copied.
    self.global = source.global;
    var itr = source.narrowed.iterator();
    while (itr.next()) |entry| {
      self.putNarrowed(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  pub fn clear(self: *@This()) void {
    self.global.clear();
    self.narrowed.clearRetainingCapacity();
  }

  pub fn promoteNarrowed(self: *@This()) void {
    var itr = self.narrowed.iterator();
    self.global.pushScope();
    while (itr.next()) |entry| {
      self.putGlobal(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  pub inline fn putGlobal(self: *@This(), name: []const u8, ty: *Type) void {
    self.global.insert(name, ty);
  }

  pub inline fn getGlobal(self: *@This(), name: []const u8) ?*Type {
    return self.global.lookup(name);
  }

  pub inline fn putNarrowed(self: *@This(), name: []const u8, ty: *Type) void {
    util.setStr(*Type, &self.narrowed, name, ty);
  }

  pub inline fn getNarrowed(self: *@This(), name: []const u8) ?*Type {
    return self.narrowed.get(name);
  }

  pub fn hasNeverTy(self: *@This()) bool {
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      if (entry.value_ptr.*.isNeverTy()) {
        return true;
      }
    }
    return false;
  }

  /// combines the narrowed types in self with other into a union when possible
  pub fn or_(self: *@This(), other: *@This()) void {
    const allocator = self.global.allocator();
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      var ident = entry.key_ptr.*;
      var t1 = entry.value_ptr.*;
      if (other.narrowed.get(ident)) |t2| {
        entry.value_ptr.* = t1.unionify(t2, allocator);
      }
    }
    itr = other.narrowed.iterator();
    while (itr.next()) |entry| {
      if (self.narrowed.get(entry.key_ptr.*) == null) {
        var ty = other.getGlobal(entry.key_ptr.*).?;
        self.putNarrowed(entry.key_ptr.*, ty.unionify(entry.value_ptr.*, allocator));
      }
    }
  }

  /// computes the intersection of the narrowed types in self with other
  pub fn and_(self: *@This(), other: *@This()) void {
    const allocator = self.global.allocator();
    var itr = self.narrowed.iterator();
    while (itr.next()) |entry| {
      var ident = entry.key_ptr.*;
      var t1 = entry.value_ptr.*;
      if (other.narrowed.get(ident)) |t2| {
        entry.value_ptr.* = (
          t1.intersect(t2, allocator) 
          orelse Type.newNever(allocator)
        );
      }
    }
    itr = other.narrowed.iterator();
    while (itr.next()) |entry| {
      if (self.narrowed.get(entry.key_ptr.*) == null) {
        self.putNarrowed(entry.key_ptr.*, entry.value_ptr.*);
      }
    }
  }

  /// transforms types by negation - negates the narrowed types using the global types
  pub fn not_(self: *@This(), name: []const u8, tc: *TypeChecker) !bool {
    _ = tc;
    const allocator = self.global.allocator();
    if (self.narrowed.get(name)) |t2| {
      var t1 = self.global.lookup(name).?;
      var neg = blk: {
        break :blk t1.negate(t2, allocator) catch {
          logger.debug("negate with t1 exactly equal to t2 results in the 'never' type", .{});
          break :blk (
            if (t1.typeidEql(t2)) Type.newNever(allocator)
            else t1
          );
        };
      };
      self.narrowed.put(name, neg) catch |e| {
        std.debug.print("Error: {}", .{e});
        std.os.exit(1);
      };
      return neg != t1;
    }
    return false;
  }
};

pub const TypeChecker = struct {
  cfg: CFG = undefined,
  linker: TypeLinker = undefined,
  namegen: util.NameGen,
  builder: CFGBuilder,
  analyzer: Analysis,
  pc: PatternChecker,
  u8w: U8Writer,
  diag: *Diagnostic,
  /// type context
  ctx: TContext,
  /// generic function/class info
  generics: ds.ArrayHashMap(*Node, *ds.ArrayList(GenInfo)),
  /// function nodes with cyclic references
  cycles: NodeList,
  /// class types being resolved at hand
  resolving: TypeList,
  /// builtin classes/types
  builtins: TypeList,
  /// builtin prelude
  _prelude: *Node = undefined,
  /// type environment for narrowing
  tenv: ?*TypeEnv = null,
  /// current function being type checked
  current_fn: ?*FlowMeta = null,
  /// current match node being type checked
  match_node: ?*ast.MatchNode = null,
  /// cached types
  void_ty: *Type,
  str_ty: *Type,

  const Self = @This();
  const ConsList = ds.ArrayList(*ptn.Constructor);
  const MAX_STRING_SYNTH_LEN = 0xc;
  const BuiltinsSrc = prelude.BuiltinsSrc;
  const TypeCheckError = (
    MatchCompiler.MatchError ||
    TypeLinker.TypeLinkError ||
    error{SynthFailure, SynthTooLarge, DeadCode, CheckError}
  );

  /// cached 'unit' types
  const UnitTypes = struct {
    const num = types.Concrete.init(.TyNumber);
    const str = types.Concrete.init(.TyString);
    const bol = types.Concrete.init(.TyBool);
    const nil = types.Concrete.init(.TyNil);
    const tyty = types.Top.init(&tyAny);

    var tyNumber: Type = Type.init(.{.Concrete = num});
    var tyString: Type = Type.init(.{.Concrete = str});
    var tyBool: Type = Type.init(.{.Concrete = bol});
    var tyNil: Type = Type.init(.{.Concrete = nil});
    var tyAny: Type = Type.newConcrete(.TyAny);
    var tyVoid: Type = Type.newConcrete(.TyVoid);
    var TyTy: Type = Type.init(.{.Top = tyty});
  };

  /// generic information
  pub const GenInfo = struct {
    /// monomorphized generic class/function node
    instance: *Node,
    /// synthesized name of the generic type
    synth_name: *ast.VarNode,
    /// actual type
    typ: *Type,
  };

  /// type checker for match nodes
  const PatternChecker = struct {
    tc: *TypeChecker,
    mc: MatchCompiler,
    /// current redundant marker node
    redmarker: ?*ast.MarkerNode = null,
    /// list of conditions for which we may infer inexhaustiveness
    tests: NodeList,
    /// list of remaining patterns
    rempatterns: std.StringArrayHashMap(u8),

    fn init(tc: *TypeChecker) @This() {
      const al = tc.ctx.allocator();
      return @This(){
        .tc = tc,
        .mc = MatchCompiler.init(tc.diag, tc, al),
        .tests = NodeList.init(al),
        .rempatterns = std.StringArrayHashMap(u8).init(al),
      };
    }

    inline fn compressTypes(self: *@This(), typeset: *TypeHashSet) !*Type {
      _ = self;
      return if (typeset.count() < 1) error.CheckError else Type.compressTypes(typeset, null);
    }

    inline fn compressTaggedTypes(self: *@This(), list: *TypeList) !*Type {
      _ = self;
      return if (list.len() < 1) error.CheckError else Type.compressTaggedTypes(list, null);
    }
  
    /// generate a new expandable Variable type
    inline fn newVariableType(self: *@This(), token: Token, al: std.mem.Allocator) *Type {
      var vr = Type.newVariable(al).box(al);
      var name = self.tc.namegen.generate("$ptn", .{});
      vr.variable().append(Token.fromWithValue(&token, name, .TkIdent));
      return vr;
    }

    fn getType(self: *@This(), haystack: *Type, needle: *Type, token: Token, al: std.mem.Allocator) !*Type {
      if (haystack.is(needle, al)) |ty| {
        return ty;
      } else {
        return self.tc.error_(
          true, token, "type '{s}' is not related to type '{s}'", .{
            self.tc.getTypename(haystack), self.tc.getTypename(needle)
          }
        );
      }
    }

    inline fn getClassType(self: *@This(), haystack: *Type, needle: *Type, token: Token, al: std.mem.Allocator) !*Type {
      return (try self.getType(haystack, needle, token, al)).classOrInstanceClass();
    }

    /// infer the type of this pattern
    fn inferPatternx(self: *@This(), pat: *Pattern, conses: *ConsList, expr_ty: *Type, al: std.mem.Allocator) !*Type {
      switch (pat.variant.*) {
        .cons => |*cons| {
          switch (cons.tag) {
            .List => {
              var tmp = Type.newBuiltinGenericClass(cons.name, al);
              var ty = try self.getClassType(expr_ty, tmp, pat.token, al);
              if (cons.args.isNotEmpty()) {
                var param_ty = ty.klass().getSlice()[0];
                for (cons.args.items()) |arg| {
                  _ = try self.inferPatternx(arg, conses, param_ty, al);
                }
              }
              cons.typ = ty;
              return ty;
            },
            .Tuple => {
              var tmp = Type.newBuiltinGenericClass(cons.name, al);
              var ty = try self.getClassType(expr_ty, tmp, pat.token, al);
              if (cons.args.isNotEmpty()) {
                if (ty.klass().tparamsLen() >= cons.args.len()) {
                  var typs = ty.klass().getSlice()[0..cons.args.len()];
                  for (cons.args.items(), typs) |arg, _ty| {
                    _ = try self.inferPatternx(arg, conses, _ty, al);
                  }
                } else {
                  return self.tc.error_(true, pat.token, "Expected type '{s}'", .{self.tc.getTypename(ty)});
                }
              }
              cons.typ = ty;
              return ty;
            },
            .Map => {
              var tmp = Type.newBuiltinGenericClass(ks.MapVar, al);
              var ty = try self.getClassType(expr_ty, tmp, pat.token, al);
              var list = &cons.args.itemAt(0).variant.cons;
              const typs = ty.klass().getSlice();
              var key_ty = typs[0];
              var val_ty = typs[1];
              if (list.args.isNotEmpty()) {
                var i = @as(usize, 0);
                while (i < list.args.len()): (i += 2) {
                  _ = try self.inferPatternx(list.args.itemAt(i), conses, key_ty, al);
                  _ = try self.inferPatternx(list.args.itemAt(i + 1), conses, val_ty, al);
                }
              }
              var uni = Type.newUnion(al).box(al);
              uni.union_().addSlice(typs);
              var list_ty = Type.newBuiltinGenericClass(ks.ListVar, al);
              list_ty.klass().appendTParam(uni);
              list.typ = list_ty;
              cons.typ = ty;
              return ty;
            },
            .Literal => {
              var ty = try self.tc.infer(cons.node.?);
              if (!expr_ty.isRelatedTo(ty, .RCAny, al)) {
                return self.tc.error_(
                  true, pat.token, "expected type '{s}', but got '{s}'",
                  .{self.tc.getTypename(expr_ty), self.tc.getTypename(ty)}
                );
              }
              cons.typ = ty;
              return ty;
            },
            .Or => {
              // Or is a synthetic constructor
              var typeset = TypeHashSet.init(al);
              typeset.ensureTotalCapacity(cons.args.len());
              for (cons.args.items()) |arg| {
                var typ = try self.inferPatternx(arg, conses, expr_ty, al);
                typeset.set(typ.typeid(), typ);
              }
              return try self.compressTypes(&typeset);
            },
            .Other => {
              // Other represents a user defined constructor
              var typ = try self.tc.lookupName(&cons.node.?.AstVar, true);
              if (typ.isClass()) {
                const token = cons.node.?.getToken();
                var ty = self.copyType(try self.getClassType(expr_ty, typ, token, al));
                try self.tc.resolveType(ty, token);
                const tyname = ty.typename(&self.tc.u8w);
                const info = if (cons.rested) " or more" else "";
                const len = cons.args.len();
                const cls = ty.klass();
                if (len > cls.fields.len()) {
                  return self.tc.error_(
                    true, token, "type '{s}' has {} field(s), but pattern test assumes {}{s}", 
                    .{tyname, cls.fields.len(), len, info}
                  );
                }
                if (cons.rested) {
                  const wc_token = Token.fromWithValue(&token, "_", .TkIdent);
                  var fields_left = cls.fields.len() - len;
                  while (fields_left > 0): (fields_left -= 1) {
                    cons.args.append(
                      Pattern.init(ptn.Wildcard.init(wc_token, true).toVariant(al), token, .{}).box(al)
                    );
                  }
                }
                if (cons.args.len() != cls.fields.len()) {
                  return self.tc.error_(
                    true, token, "type '{s}' has {} field(s), but pattern test assumes {}{s}", 
                    .{tyname, cls.fields.len(), len, info}
                  );
                }
                if (cons.hasField()) {
                  // try to sort the patterns
                  for (cls.fields.items(), 0..) |field, i| {
                    for (cons.args.items(), 0..) |arg, j| {
                      if (arg.alat.hasField()) {
                        if (field.AstVarDecl.ident.token.valueEql(arg.alat.field.?.AstVar.token.value)) {
                          cons.args.items()[j] = cons.args.items()[i];
                          cons.args.items()[i] = arg;
                        }
                      }
                    }
                  }
                }
                for (cons.args.items(), cls.fields.items()) |arg, field| {
                  var fty = try self.tc.inferVarDecl(&field.AstVarDecl);
                  _ = try self.inferPatternx(arg, conses, fty, al);
                }
                cons.typ = ty;
                return ty;
              } else if (typ.isTag()) {
                const token = cons.node.?.getToken();
                const tyname = typ.typename(&self.tc.u8w);
                const info = if (cons.rested) " or more" else "";
                const len = cons.args.len();
                const tag = typ.tag();
                if (len > tag.paramsLen()) {
                  return self.tc.error_(
                    true, token, "type '{s}' has {} field(s), but pattern test assumes {}{s}", 
                    .{tyname, tag.paramsLen(), len, info}
                  );
                }
                if (cons.rested) {
                  const wc_token = Token.fromWithValue(&token, "_", .TkIdent);
                  var fields_left = tag.paramsLen() - len;
                  while (fields_left > 0): (fields_left -= 1) {
                    cons.args.append(
                      Pattern.init(ptn.Wildcard.init(wc_token, true).toVariant(al), token, .{}).box(al)
                    );
                  }
                }
                if (cons.args.len() != tag.paramsLen()) {
                  return self.tc.error_(
                    true, token, "type '{s}' has {} field(s), but pattern test assumes {}{s}", 
                    .{tyname, tag.paramsLen(), len, info}
                  );
                }
                if (cons.hasField()) {
                  // try to sort the patterns
                  for (tag.paramSlice(), 0..) |prm, i| {
                    for (cons.args.items(), 0..) |arg, j| {
                      if (arg.alat.hasField()) {
                        if (prm.name) |name| {
                          if (arg.alat.field.?.AstVar.token.valueEql(name.value)) {
                            cons.args.items()[j] = cons.args.items()[i];
                            cons.args.items()[i] = arg;
                          }
                        }
                      }
                    }
                  }
                }
                var ty = try self.getType(expr_ty, typ, token, al);
                for (cons.args.items(), ty.tag().paramSlice()) |arg, prm| {
                  _ = try self.inferPatternx(arg, conses, prm.typ, al);
                }
                cons.typ = ty;
                return ty;
              } else {
                self.tc.softError(
                  cons.node.?.AstVar.token,
                  "bad/ambiguous pattern constructor: '{s}'",
                  .{self.tc.getTypename(typ)}
                );
                cons.typ = typ;
                return typ;
              }
            },
          }
        },
        .vari => |*vari| {
          self.tc.ctx.typScope.insert(vari.ident.AstVar.token.value, expr_ty);
          return expr_ty;
        },
        .wildc => |*wildc| {
          wildc.typ = expr_ty;
          return expr_ty;
        },
        else => {},
      }
      unreachable;
    }

    inline fn copyType(self: *@This(), ty: *Type) *Type {
      var nty = self.tc.ctx.copyType(ty);
      nty.tid = 0;
      return nty;
    }

    /// recursively find the `is` test expression. Entry node is a ConditionNode.
    fn getTestFromCondition(self: *@This(), tst: *Node) *Node {
      if (tst.isVariable()) {
        return tst;
      } else if (tst.isCondition()) {
        return self.getTestFromCondition(tst.AstCondition.cond);
      } else if (tst.isBinary()) {
        const bin = &tst.AstBinary;
        if (bin.op.optype == .OpIs or bin.left.isCall()) {
          return tst;
        }
        var node = self.getTestFromCondition(bin.left);
        if (node.isVariable()) {
          return tst;
        }
        return node;
      } else {
        return tst;
      }
    }

    /// check if a boolean pattern match is exhaustive by walking up the tests 'stack'
    fn isBoolExhaustive(self: *@This()) bool {
      // if we are able to gather a `true | false` type along the tests stack,
      // then the boolean match must be exhaustive.
      const al = self.tc.ctx.allocator();
      var uni = Type.newUnion(al);
      var tty = Type.newConstant(.TyBool, ks.TrueVar);
      var fty = Type.newConstant(.TyBool, ks.FalseVar);
      var j = self.tests.len();
      while (j > 0): (j -= 1) {
        const nd = self.tests.itemAt(j - 1).AstBinary.right;
        const typ = if (nd.isBool()) nd.AstBool.token.value else break;
        if (std.mem.eql(u8, typ, ks.TrueVar)) {
          uni.union_().set(&tty);
        } else if (std.mem.eql(u8, typ, ks.FalseVar)) {
          uni.union_().set(&fty);
        } else {
          break;
        }
      }
      return (uni.union_().variants.count() == 2);
    }

    fn _inferMatch(self: *@This(), flo_node: *FlowNode, node: *ast.MatchNode) !*Type {
      // save current redundancy marker on the stack, restore on exit
      const redmarker = self.redmarker;
      defer self.redmarker = redmarker;
      // save current match node on the stack, restore on exit
      const curr_mn = self.tc.match_node;
      defer self.tc.match_node = curr_mn;
      // save current tests on the stack, restore on exit
      const curr_tests = self.tests;
      defer self.tests = curr_tests;
      // reset match node, tests & redmarker for a new match check
      self.tc.match_node = node;
      self.tests = NodeList.init(self.tc.ctx.allocator());
      self.redmarker = null;
      // infer all
      if (node.decl) |decl| {
        _ = try self.tc.infer(decl);
      }
      var m_expr_ty = try self.tc.infer(node.expr);
      var expr_ty = m_expr_ty.classOrInstanceClass();
      const al = self.tc.ctx.allocator();
      var conses = ConsList.init(al);
      for (node.cases.items()) |case| {
        // use a new scope for resolving each pattern, so that each variable type
        // doesn't conflict with existing types in ctx.
        self.tc.ctx.enterScope();
        var ty = try self.inferPatternx(case.pattern, &conses, expr_ty, al);
        logger.debug("Resolved type: {s}", .{self.tc.getTypename(ty)});
        if (case.pattern.isConstructor()) {
          if (expr_ty.canBeAssigned(ty, .RCAny, self.tc.ctx.allocator())) |_| {
            case.pattern.variant.cons.typ = ty;
          } else if (case.pattern.isOrConstructor()) { // Or cons still uses untagged unions
            case.pattern.variant.cons.typ = ty;
          } else {
            self.tc.softError(
              case.pattern.token, "Expected type '{s}' but found '{s}'",
              .{self.tc.getTypename(m_expr_ty), self.tc.getTypename(ty)},
            );
          }
        }
        // pop scope off
        self.tc.ctx.leaveScope();
      }
      if (self.tc.diag.hasErrors()) return error.CheckError;
      var tree = try self.mc.compile(node);
      if (self.tc.diag.hasErrors()) return error.CheckError;
      var lnode = try self.mc.lowerDecisionTree(tree, node.expr.getToken());
      if (node.decl) |decl| {
        lnode.block().nodes.prepend(decl);
      }
      if (util.getMode() == .Debug) {
        lnode.render(0, &self.tc.u8w) catch {};
        logger.debug("tree:\n{s}\n", .{self.tc.u8w.items()});
      }
      var builder = CFGBuilder.init(self.tc.ctx.allocator());
      var flo = builder.buildBlock(&self.tc.cfg, lnode);
      // save and restore diag level on exit
      const level = self.tc.diag.getLevel();
      defer self.tc.diag.setLevel(level);
      node.lnode = lnode;
      //@ patch current cfg with match's cfg.
      std.debug.assert(flo_node.bb.len() == 1);
      //> connect all predecessors of the match's flow node to all outgoing nodes from the new cfg entry.
      //>   this should disconnect the new cfg's entry flow node 
      var preds = self.tc.getPrevFlowNodes(flo_node);
      var succs = self.tc.getNextFlowNodes(flo.entry);
      for (succs.items()) |succ| {
        builder.connectVerticesWithEdgeInfo(preds, succ, .ESequential);
      }
      //> connect successors of the match's flow node to all incoming nodes of the new cfg exit
      //>   care should be taken to only connect non exit & non return nodes as the exit node is shared
      preds = self.tc.getPrevFlowNodes(flo.exit);
      var non_ret_preds = preds.filter(FlowNode.FilterPredicate.isNonRetFlowNode);
      succs = self.tc.getNextFlowNodes(flo_node);
      for (succs.items()) |succ| {
        if (!succ.isExitNode()) {
          // only connect non-return nodes
          builder.connectVerticesWithEdgeInfo(non_ret_preds, succ, .ESequential);
        } else {
          builder.connectVerticesWithEdgeInfo(preds, succ, .ESequential);
        }
      }
      //>   if this match node returns on all paths, then whatever succeeds the match node is dead
      const returns_on_all_paths = non_ret_preds.len() == 1 and non_ret_preds.itemAt(0).isDeadNode();
      if (returns_on_all_paths) {
        // we can only return in functions, so this is definitely a function
        const dead = self.tc.current_fn.?.dead;
        // connect preds to dead,
        builder.connectVerticesWithEdgeInfo(preds, dead, .ESequential);
        // and dead to succ
        const dl = FlowList.initWith(al, dead); // the new prev
        for (succs.items()) |succ| {
          builder.connectVerticesWithEdgeInfo(dl, succ, .ESequential);
        }
      }
      // set diag level to `internal error`, as we do not want to display internal errors
      // on the lowered transformation due to faulty match nodes
      self.tc.diag.setLevel(.DiagIError);
      _ = try self.tc.flowInferEntry(flo.entry);
      flo_node.bb.nodes.clearRetainingCapacity();
      node.typ = self.tc.void_ty;
      return node.typ.?;
    }

    fn inferMatch(self: *@This(), flo_node: *FlowNode, node: *ast.MatchNode) !*Type {
      const ty = try self._inferMatch(flo_node, node); 
      {
        if (self.rempatterns.count() > 0) {
          const token = node.expr.getToken();
          self.tc.softError(token, "inexhaustive pattern match.", .{});
          var prompt = false;
          for (self.rempatterns.keys()) |msg| {
            if (msg.len == 0) {
              continue;
            }
            if (!prompt) {
              self.tc.diag.addDiagnosticsDirect(token, "  Remaining pattern type(s):\n");
              prompt = true;
            }
            self.tc.diag.addDiagnosticsDirect(token, "\t");
            self.tc.diag.addDiagnosticsDirect(token, msg,);
            self.tc.diag.addDiagnosticsDirect(token, "\n");
          }
          self.tc.diag.addDiagnosticsDirect(token, "\n");
          self.rempatterns.clearRetainingCapacity();
        }
        return error.CheckError;
      }
      return ty;
    }

    fn flowInferMCondition(self: *@This(), flo_node: *FlowNode, ast_node: *Node) !void {
      const redmarker = self.redmarker;
      defer self.redmarker = redmarker;
      const tst = ast_node.AstMCondition.tst;
      self.tests.append(self.getTestFromCondition(tst));
      defer _ = self.tests.pop();
      try self.tc.flowInferCondition(flo_node, tst);
    }

    fn getIdent(self: *@This(), node: *ast.BinaryNode) *ast.VarNode {
      _ = self;
      if (!node.left.isCall()) {
        return &node.left.AstVar;
      } else {
        const call = &node.left.AstCall;
        std.debug.assert(call.expr.isDotAccess());
        std.debug.assert(call.expr.AstDotAccess.lhs.isVariable());
        return &call.expr.AstDotAccess.lhs.AstVar;
      }
    }

    fn inferFail(self: *@This(), node: *ast.MarkerNode) !*Type {
      _ = node;
      // save and restore diag level on exit
      const level = self.tc.diag.getLevel();
      defer self.tc.diag.setLevel(level);
      self.tc.diag.setLevel(.DiagError);
      const nd = self.tests.getLast();
      var allow_rested = false;
      var ident: *ast.VarNode = undefined;
      if (nd.isBinary()) {
        allow_rested = nd.AstBinary.allow_rested;
        ident = self.getIdent(&nd.AstBinary);
      } else if (nd.isVariable()) {
        ident = &nd.AstVar;
      } else {
        // get the nearest (enclosing) binary/var test
        var i = self.tests.len() - 1;
        var id: ?*ast.VarNode = null;
        while (i > 0): (i -= 1) {
          const tst = self.tests.itemAt(i - 1);
          if (tst.isBinary()) {
            // don't set allow_rested, because this isn't the direct condition being tested.
            id = self.getIdent(&tst.AstBinary);
            break;
          } else if (tst.isVariable()) {
            id = &tst.AstVar;
            break;
          }
        }
        if (id) |_id| {
          ident = _id;
        } else {
          self.rempatterns.put("", 0) catch {};
          return self.tc.void_ty;
        }
      }
      var ty = try self.tc.lookupName(ident, false);
      if (!ty.isNeverTy() and !allow_rested) {
        // It is possible that this Fail node is from an enclosing `rested` constructor, because the
        // match compiler tries to produce an optimal decision tree without repeated constructor tests.
        // Hence, we look up the test 'stack', if we find any test with an allow_rested property,
        // then the test is exhaustive, but the match arms were (originally) ordered poorly
        // Get the nearest enclosing condition
        const tst = self.tests.itemAt(self.tests.len() - 1);
        allow_rested = tst.isBinary() and tst.AstBinary.allow_rested;
        if (!allow_rested) {
          self.rempatterns.put(self.tc.getTypename(ty), 1) catch {};
          return self.tc.void_ty;
        }
      }
      return self.tc.void_ty;
    }

    fn inferRedundant(self: *@This(), node: *ast.MarkerNode) !*Type {
      if (self.tc.diag.hasErrors()) {
        return self.tc.void_ty;
      }
      if (self.tc.match_node) |nd| {
        if (self.tc.tenv) |env| {
          self.checkTestRedundancy(nd, env, node.token);
        }
      }
      return self.tc.void_ty;
    }

    /// check if a particular test/condition is redundant, given the variable of interest.
    fn checkTestRedundancy(self: *@This(), match_node: *ast.MatchNode, env: *TypeEnv, err_token: Token) void {
      // FIXME: eliminate false positives in redundancy warnings
      const token = match_node.getVariableOfInterest();
      const typ = env.getNarrowed(token.value) orelse env.getGlobal(token.value);
      if (typ) |nty| {
        if (nty.isNeverTy() and self.redmarker == null) {
          return self.tc.warn(true, err_token, "possible redundant case", .{});
        }
      }
    }
  };

  pub fn init(allocator: std.mem.Allocator, diag: *Diagnostic) Self {
    var self = Self {
      .ctx = TContext.init(allocator),
      .diag = diag,
      .namegen = util.NameGen.init(allocator),
      .generics = ds.ArrayHashMap(*Node, *ds.ArrayList(GenInfo)).init(allocator),
      .builder = CFGBuilder.init(allocator),
      .cycles = NodeList.init(allocator),
      .resolving = TypeList.init(allocator),
      .analyzer = Analysis.init(diag),
      .void_ty = Type.newVoid().box(allocator),
      .str_ty = undefined,
      .builtins = TypeList.init(allocator),
      .pc = undefined,
      .u8w = U8Writer.init(allocator),
    };
    self.linker = TypeLinker.init(&self.ctx, self.diag, undefined);
    return self;
  }

  fn loadBuiltinsPrelude(self: *Self, al: *VebAllocator) void {
    const filename = @as([]const u8, "$builtins$");
    var parser = parse.Parser.init(@constCast(&@as([]const u8, BuiltinsSrc)), &filename, al);
    parser.setParseMode(.Builtin);
    const node = parser.parse(true) catch unreachable;
    self.buildProgramFlow(node, true) catch unreachable;
    self.flowInferEntry(self.cfg.program.entry) catch unreachable;
    for (node.AstProgram.decls.items()) |itm| {
      if (itm.isFun()) {
        itm.AstFun.body.block().checked = true;
        itm.AstFun.is_builtin = true;
      } else if (itm.isClass()) {
        itm.AstClass.is_builtin = true;
        itm.AstClass.checked = true;
        for (itm.AstClass.methods.items()) |method| {
          method.AstFun.is_builtin = true;
        }
        if (!itm.AstClass.name.token.valueEql(ks.StrVar)) {
          self.builtins.append(self.inferClassPartial(itm) catch unreachable);
        } else {
          var ty = try self.inferClassPartial(itm);
          self.str_ty = self.inferClass(itm, ty) catch unreachable;
          self.builtins.append(self.str_ty);
        }
      }
    }
    self._prelude = node;
  }

  pub fn error_(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) TypeCheckError {
    if (emit) self.diag.addDiagnostics(token, "TypeError: " ++ fmt, args);
    return error.CheckError;
  }

  pub fn softError(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) void {
    self.diag.addDiagnostics(token, "TypeError: " ++ fmt, args);
  }

  inline fn warn(self: *Self, emit: bool, token: Token, comptime fmt: []const u8, args: anytype) void {
    if (emit) self.diag.addDiagnosticsWithLevel(.DiagWarn, token, "TypeWarning: " ++ fmt, args);
  }

  inline fn genName(al: std.mem.Allocator, l1: usize, l2: usize) []const u8 {
    return std.fmt.allocPrint(al, "${}.{}", .{l1, l2}) catch @panic("could not gen name");
  }

  pub fn makeSynthName(al: std.mem.Allocator, name: ?[]const u8, is_builtin: bool, args: *TypeList, targs: ?*NodeList) []const u8 {
    _ = is_builtin;
    // FIXME: this is unhygienically inefficient.
    var start = name orelse genName(
      al,
      if (targs) |ta| ta.len() else 0,
      args.len()
    );
    var writer = @constCast(&std.ArrayList(u8).init(al)).writer();
    _ = writer.write(start) catch undefined;
    var buf: [100]u8 = undefined;
    if (targs) |ta| {
      if (args.len() == 0) {
        for (ta.items()) |nd| {
          _ = writer.write(".") catch undefined;
          var id_str = std.fmt.bufPrint(&buf, "{}", .{nd.getType().?.typeid()}) catch unreachable;
          _ = writer.write(id_str) catch undefined;
        }
      }
    }
    for (args.items()) |ty| {
      _ = writer.write(".") catch undefined;
      var id_str = std.fmt.bufPrint(&buf, "{}", .{ty.typeid()}) catch unreachable;
      _ = writer.write(id_str) catch undefined;
    }
    return writer.context.items;
  }

  inline fn createSynthName(self: *Self, name: ?[]const u8, is_builtin: bool, args: *TypeList, targs: ?*NodeList) []const u8 {
    return makeSynthName(self.ctx.allocator(), name, is_builtin, args, targs);
  }

  fn createFunSynthName(self: *Self, fun: *ast.FunNode, args: *TypeList, targs: ?*NodeList) []const u8 {
    if (fun.is_builtin) return fun.name.?.token.value;
    return self.createSynthName(if (fun.name) |nm| nm.token.value else null, fun.is_builtin, args, targs);
  }

  fn boxSynthName(self: *Self, token: Token, val: []const u8) *ast.VarNode {
    var name_token = token;
    name_token.ty = .TkAllocString;
    name_token.value = val;
    return ast.VarNode.init(name_token).box(self.ctx.allocator());
  }

  fn addGenInfo(self: *Self, origin: *Node, instance: *Node, synth_name: *ast.VarNode, typ: *Type) void {
    var data = @as(GenInfo, .{
      .instance = instance,
      .synth_name = synth_name,
      .typ = typ,
    });
    if (self.generics.get(origin)) |list| {
      list.append(data);
    } else {
      var list = ds.ArrayList(GenInfo).init(self.ctx.allocator()).box();
      list.append(data);
      self.generics.set(origin, list);
    }
  }

  fn findGenInfo(self: *Self, origin: *Node, synth_name: []const u8) ?GenInfo {
    if (self.generics.get(origin)) |list| {
      for (list.items()) |info| {
        if (info.synth_name.token.valueEql(synth_name)) {
          return info;
        }
      }
    }
    return null;
  }

  fn findBuiltinType(self: *Self, name: []const u8) ?*Type {
    for (self.builtins.items()) |itm| {
      if (std.mem.eql(u8, itm.klass().name, name)) {
        return itm;
      }
    }
    return null;
  }

  inline fn findType(self: *Self, name: []const u8) ?*Type {
    return self.ctx.typScope.lookup(name);
  }

  inline fn findName(self: *Self, name: []const u8) ?*Type {
    return self.ctx.varScope.lookup(name);
  }

  fn lookupName(self: *Self, ident: *ast.VarNode, emit: bool) !*Type {
    if (self.findName(ident.token.value)) |found| {
      return found;
    } else if (self.findType(ident.token.value)) |found| {
      return found;
    } else {
      return self.error_(emit, ident.token, "Could not resolve type of ident: '{s}'", .{ident.token.value});
    }
  }

  fn lookupNameWithInfo(self: *Self, ident: *ast.VarNode, emit: bool) !struct{typ: *Type, ident: bool} {
    if (self.findName(ident.token.value)) |found| {
      return .{.typ = found, .ident = true};
    } else if (self.findType(ident.token.value)) |found| {
      return .{.typ = found, .ident = false};
    } else {
      return self.error_(emit, ident.token, "Could not resolve type of ident: '{s}'", .{ident.token.value});
    }
  }

  fn validateGenericParamSize(self: *Self, size: usize, exp_size: usize, token: Token) !void {
    if (size != exp_size) {
      return self.error_(
        true, token, 
        "Generic type parameter(s) mismatch. Expected {} type parameter(s), but got {}", 
        .{exp_size, size}
      );
    }
  }

  fn insertType(self: *Self, var_typ: *Type, typ: *Type) void {
    var tvar = var_typ.variable();
    if (tvar.tokens.len() > 1) util.todo("multiple var tokens!");
    var name = tvar.tokens.itemAt(0).value;
    self.ctx.typScope.insert(name, typ);
  }

  pub inline fn insertVar(self: *Self, name: []const u8, ty: *Type) void {
    self.ctx.varScope.insert(name, ty);
  }

  pub inline fn deleteVar(self: *Self, name: []const u8) void {
    self.ctx.varScope.remove(name);
  }

  fn getTypename(self: *Self, typ: *Type) []const u8 {
    return typ.typename(&self.u8w);
  }

  fn newUnaryNode(self: *Self, expr: *Node, op: Token) *Node {
    var node = Node.create(self.ctx.allocator());
    node.* = .{.AstUnary = ast.UnaryNode.init(expr, op)};
    return node;
  }

  fn newStringNode(self: *Self, val: []const u8, token: Token) Node {
    _ = self;
    return .{.AstString = ast.LiteralNode.init(Token.fromWithValue(&token, val, .TkString))};
  }

  /// copy env to varScope
  fn copyEnv(self: *Self, env: *TypeEnv) void {
    var itr = env.global.current().map.iterator();
    while (itr.next()) |entry| {
      self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
    }
    itr = env.narrowed.iterator();
    while (itr.next()) |entry| {
      self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
    }
  }

  /// check if this flow node is unresolved
  inline fn isFlowNodeUnresolved(self: *Self, node: *FlowNode) bool {
    _ = self;
    return node.res.isUnresolved();
  }

  //***********************************************************//
  //***********  narrowing  ***********************************//
  //***********************************************************//

  fn _synth(self: *Self,  vr: *ast.VarNode, value: []const u8, env: *TypeEnv) TypeCheckError!*ast.VarNode {
    var token = ast.Token.from(&vr.token);
    token.value = std.fmt.allocPrint(
      self.ctx.allocator(), "{s}.{s}",
      .{vr.token.value, value}
    ) catch return error.SynthFailure;
    var ret = ast.VarNode.init(token);
    var last = self.diag.count();
    _ = self.narrowVariable(&ret, env) catch {
      self.diag.popUntil(last);
    };
    return ret.box(self.ctx.allocator());
  }

  fn synthesizeVar(self: *Self, vr: *ast.VarNode, other: ?*Node, env: *TypeEnv) TypeCheckError!*ast.VarNode {
    if (vr.typ == null) _ = try self.inferVar(vr, false);
    if (other) |oth| {
      switch (oth.*) {
        .AstNumber, .AstString, .AstBool, .AstNil => |lit| {
          if (lit.token.value.len > MAX_STRING_SYNTH_LEN) {
            return error.SynthTooLarge;
          }
          return try self._synth(vr, lit.token.value, env);
        },
        .AstVar => |*id| {
          if (id.token.value.len > MAX_STRING_SYNTH_LEN) {
            return error.SynthTooLarge;
          }
          return try self._synth(vr, id.token.value, env);
        },
        else => |els| {
          std.debug.print("unexpected ast type for synth {}", .{els});
          return error.SynthFailure;
        }
      }
    } else {
      return vr;
    }
  }

  fn synthesizeSubscript(self: *Self, node: *ast.SubscriptNode, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    // synthesize node.expr with node.index
    try self.narrow(node.expr, env, assume_true);
    // if this node has been narrowed before, we don't want to reset its type,
    // as narrowing now may be from taking a false path. Hence, we use `change`
    // to determine whether to update `node.narrowed` or not
    var change = (node.narrowed == null);
    if (change) {
      // since we're in the process of narrowing this subscript, we do not want `inferSubscript()`
      // to assume that this node is already narrowed, hence, we set a dummy value for `node.narrowed`
      // as a flag to `inferSubscript()` indicating that this node is being narrowed.
      node.narrowed = @constCast(&.{.token = undefined, .typ = null});
    }
    var ty = try self.inferSubscript(node);
    var narrowed = try self.synthesize(node.expr, env, assume_true);
    narrowed = try self.synthesizeVar(narrowed, node.index, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }
  
  fn synthesizeDeref(self: *Self, node: *ast.DerefNode, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    // synthesize node.expr
    try self.narrow(node.expr, env, assume_true);
    // similar to what inferSubscript() does here.
    var change = (node.narrowed == null);
    if (change) {
      // similar to what inferSubscript() does here.
      node.narrowed = @constCast(&.{.token = undefined, .typ = null});
    }
    var ty = try self.inferDeref(node);
    var narrowed = try self.synthesize(node.expr, env, assume_true);
    var nil = self.newStringNode("?", node.token);
    narrowed = try self.synthesizeVar(narrowed, &nil, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }

  fn synthesizeDotAccess(self: *Self, node: *ast.DotAccessNode, ast_node: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    // synthesize node.expr
    try self.narrow(node.lhs, env, assume_true);
    // similar to what inferSubscript() does here.
    var change = (node.narrowed == null);
    if (change) {
      // similar to what inferSubscript() does here.
      node.narrowed = @constCast(&.{.token = undefined, .typ = null});
    }
    var ty = try self.inferDotAccess(node, ast_node);
    var narrowed = try self.synthesize(node.lhs, env, assume_true);
    narrowed = try self.synthesizeVar(narrowed, node.rhs, env);
    if (narrowed.typ == null) {
      narrowed.typ = ty;
    }
    if (change) {
      node.narrowed = narrowed;
    }
    return narrowed;
  }

  fn synthesize(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!*ast.VarNode {
    return switch (node.*) {
      .AstVar => |*vr| try self.synthesizeVar(vr, null, env),
      .AstSubscript => |*sub| try self.synthesizeSubscript(sub, env, assume_true),
      .AstDeref => |*der| try self.synthesizeDeref(der, env, assume_true),
      .AstDotAccess => |*da| try self.synthesizeDotAccess(da, node, env, assume_true),
      else => error.SynthFailure,
    };
  }

  inline fn canNarrowSubscript(self: *Self, node: *ast.SubscriptNode) bool {
    return self.canNarrow(node.expr) and node.index.isComptimeConst();
  }

  inline fn canNarrowDeref(self: *Self, node: *ast.DerefNode) bool {
    return self.canNarrow(node.expr);
  }

  inline fn canNarrowDotAccess(self: *Self, node: *ast.DotAccessNode) bool {
    return self.canNarrow(node.lhs);
  }

  fn canNarrow(self: *Self, node: *Node) bool {
    return switch (node.*) {
      .AstVar => true,
      .AstSubscript => |*sub| self.canNarrowSubscript(sub),
      .AstDeref => |*der| self.canNarrowDeref(der),
      .AstDotAccess => |*da| self.canNarrowDotAccess(da),
      else => false,
    };
  }

  fn narrowAtomic(self: *Self, node: *Node, env: *TypeEnv) !void {
    _ = env;
    _ = try self.infer(node);
  }

  fn narrowVariable(self: *Self, node: *ast.VarNode, env: *TypeEnv) !void {
    if (env.getNarrowed(node.token.value)) |ty| {
      self.insertVar(node.token.value, ty);
      node.typ = ty;
    } else if (env.getGlobal(node.token.value)) |ty| {
      self.insertVar(node.token.value, ty);
      node.typ = ty;
    } else {
      var ty = try self.lookupName(node, true);
      if (!ty.isBoolTy()) {
        env.putGlobal(node.token.value, ty);
      } else {
        // translate `bool` into `true | false`
        const typ = Type.newBoolUnion(self.ctx.allocator());
        env.putGlobal(node.token.value, typ);
        self.insertVar(node.token.value, typ);
      }
      node.typ = ty;
    }
  }

  fn narrowCast(self: *Self, node: *ast.CastNode, env: *TypeEnv, assume_true: bool) !void {
    try self.narrow(node.expr, env, assume_true);
    _ = try self.inferCast(node);
  }

  fn narrowSubscript(self: *Self, node: *ast.SubscriptNode, env: *TypeEnv, assume_true: bool) !void {
    if (self.canNarrowSubscript(node)) {
      var vr = try self.synthesizeSubscript(node, env, assume_true);
      if (self.findName(vr.token.value)) |_| {
        try self.narrowVariable(vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertVar(vr.token.value, vr.typ.?);
      }
    } else {
      try self.narrow(node.expr, env, assume_true);
      try self.narrow(node.index, env, assume_true);
      _ = try self.inferSubscript(node);
    }
  }

  fn narrowDeref(self: *Self, node: *ast.DerefNode, env: *TypeEnv, assume_true: bool) !void {
    if (self.canNarrowDeref(node)) {
      var vr = try self.synthesizeDeref(node, env, assume_true);
      if (self.findName(vr.token.value)) |_| {
        try self.narrowVariable(vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertVar(vr.token.value, vr.typ.?);
      }
    } else {
      try self.narrow(node.expr, env, assume_true);
      _ = try self.inferDeref(node);
    }
  }

  fn narrowDotAccess(self: *Self, node: *ast.DotAccessNode, ast_node: *Node, env: *TypeEnv, assume_true: bool) !void {
    if (self.canNarrowDotAccess(node)) {
      var vr = try self.synthesizeDotAccess(node, ast_node, env, assume_true);
      if (self.findName(vr.token.value)) |_| {
        try self.narrowVariable(vr, env);
        node.typ = env.getGlobal(vr.token.value);
      } else {
        self.insertVar(vr.token.value, vr.typ.?);
      }
    } else {
      try self.narrow(node.lhs, env, assume_true);
      _ = try self.inferDotAccess(node, ast_node);
    }
  }

  inline fn isNarrowableLiteral(self: *Self, node: *ast.BinaryNode) bool {
    return (
      (node.op.optype == .OpEqq or node.op.optype == .OpNeq)
      and self.canNarrow(node.left)
      and (
        (node.right.isVariable() and node.right.AstVar.token.valueEql(ks.NoneVar))
        or node.right.isNoneLiteral()
        or node.right.isConstLiteral()
      )
    );
  }

  fn narrowBinary(self: *Self, node: *ast.BinaryNode, env: *TypeEnv, assume_true: bool) !void {
    if (node.op.optype == .OpIs) {
      if (node.left.isVariable()) {
        try self.narrowVariable(&node.left.AstVar, env);
        _ = try self.inferBinary(node);
        var ty = try self.lookupName(&node.left.AstVar, true);
        if (Type.is(ty, node.right.getType().?, self.ctx.allocator())) |is_ty| {
          env.putNarrowed(node.left.AstVar.token.value, is_ty);
          if (!assume_true) {
            if (!try env.not_(node.left.AstVar.token.value, self)) {
              self.warn(
                true, node.op.token,
                "Cannot narrow type '{s}'",
                .{self.getTypename(node.left.getType().?)}
              );
            }
          }
          node.left.AstVar.typ = is_ty;
          return;
        }
      } else if (self.canNarrow(node.left)) {
        try self.narrow(node.left, env, assume_true);
        if (node.left.getNarrowed()) |vr| {
          var synth = node.*;
          synth.left = @constCast(&@as(Node, .{.AstVar = vr.*}));
          try self.narrowBinary(&synth, env, assume_true);
          node.typ = synth.typ;
          return;
        }
      }
      if (node.right.getType().?.isConstant()) {
        try self.narrow(node.left, env, assume_true);
        try self.narrow(node.right, env, assume_true);
        _ = try self.inferBinary(node);
        return;
      }
      var lty = node.left.getType();
      var rty = node.right.getType();
      if (lty != null and rty != null) {
        self.warn(
          true, node.op.token,
          "Cannot narrow type '{s}' to '{s}'",
          .{self.getTypename(lty.?), self.getTypename(rty.?)}
        );
      } else {
        self.warn(
          true, node.op.token,
          "Cannot narrow type at expression",
          .{}
        );
      }
    } else if (node.op.optype == .OpAnd) {
      env.global.pushScope();
      if (assume_true) {
        try self.narrow(node.left, env, true);
        var rhs_env = TypeEnv.init(self.ctx.allocator(), env);
        rhs_env.promoteNarrowed();
        try self.narrow(node.right, &rhs_env, true);
        env.and_(&rhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      } else {
        try self.narrow(node.left, env, false);
        var lhs_env = TypeEnv.init(self.ctx.allocator(), null);
        lhs_env.global = env.global;
        try self.narrow(node.left, &lhs_env, true);
        try self.narrow(node.right, &lhs_env, false);
        env.or_(&lhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      }
      return;
    } else if (node.op.optype == .OpOr) {
      if (assume_true) {
        try self.narrow(node.left, env, true);
        var lhs_env = TypeEnv.init(self.ctx.allocator(), null);
        lhs_env.global = env.global;
        try self.narrow(node.left, &lhs_env, false);
        try self.narrow(node.right, &lhs_env, true);
        env.or_(&lhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      } else {
        try self.narrow(node.left, env, false);
        var rhs_env = TypeEnv.init(self.ctx.allocator(), env);
        rhs_env.promoteNarrowed();
        try self.narrow(node.right, &rhs_env, false);
        env.and_(&rhs_env);
        node.typ = Type.unionify(node.left.getType().?, node.right.getType().?, self.ctx.allocator());
      }
      return;
    } else if (self.isNarrowableLiteral(node)) {
      var bin = b: {
        var bin = node.*;
        bin.op.optype = .OpIs;
        bin.right = node.right.toTypeNode(self.ctx.allocator());
        bin.allow_consts = true;
        break :b bin;
      };
      try self.narrowBinary(&bin, env, if (node.op.optype == .OpEqq) assume_true else !assume_true);
      node.typ = bin.typ;
      return;
    }
    try self.narrow(node.left, env, assume_true);
    try self.narrow(node.right, env, assume_true);
    _ = try self.inferBinary(node);
  }

  fn narrowUnary(self: *Self, node: *ast.UnaryNode, env: *TypeEnv, assume_true: bool) !void {
    if (node.op.optype == .OpNot) {
      try self.narrow(node.expr, env, !assume_true);
      // at this point, node.expr typechecked successfully
      node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    } else {
      try self.narrow(node.expr, env, assume_true);
      _ = try self.inferUnary(node);
    }
  }

  fn narrow(self: *Self, node: *Node, env: *TypeEnv, assume_true: bool) TypeCheckError!void {
    return switch (node.*) {
      .AstVar => |*vr| try self.narrowVariable(vr, env),
      .AstBinary => |*bin| try self.narrowBinary(bin, env, assume_true),
      .AstUnary => |*una| try self.narrowUnary(una, env, assume_true),
      .AstSubscript => |*sub| try self.narrowSubscript(sub, env, assume_true),
      .AstCast => |*cst| try self.narrowCast(cst, env, assume_true),
      .AstDeref => |*der| try self.narrowDeref(der, env, assume_true),
      .AstDotAccess => |*da| try self.narrowDotAccess(da, node, env, assume_true),
      .AstNumber,
      .AstString,
      .AstBool,
      .AstList,
      .AstMap,
      .AstNType => try self.narrowAtomic(node, env),
      else => |*d| {
        logger.debug("attempt to narrow node: {}", .{d});
      },
    };
  }

  //***********************************************************//
  //****************  flow  ***********************************//
  //***********************************************************//
  fn flowInferEntry(self: *Self, node: *FlowNode) !void {
    std.debug.assert(node.tag == .CfgEntry);
    // automatically resolved on entry
    node.res = .Resolved;
    // `entry` node, so we don't care about the `node` & `prev` properties
    for (node.prev_next.items()) |item| {
      if (item.next) {
        try self.flowInfer(item.flo, true);
      }
    }
  }

  fn flowInferExit(self: *Self, node: *FlowNode) !void {
    _ = self;
    std.debug.assert(node.tag == .CfgExit);
    node.res = .Resolved;
    // assert no more nodes after exit
    std.debug.assert(node.prev_next.count(FlowNode.isNext) == 0);
  }

  fn flowInferMeta(self: *Self, flo_node: *FlowNode, ast_node: *Node) !void {
    if (flo_node.res.isResolved()) return;
    if (flo_node.tag == .CfgEntry) {
      return try self.flowInferEntry(flo_node);
    }
    if (flo_node.tag == .CfgExit) {
      return try self.flowInferExit(flo_node);
    }
    return self.flowInferNode(flo_node, ast_node);
  }

  fn flowInferNode(self: *Self, flo_node: *FlowNode, ast_node: *Node) !void {
    if (flo_node.res.isResolved()) return;
    if (!ast_node.isMatch()) {
      _ = try self.infer(ast_node);
    } else {
      _ = try self.pc.inferMatch(flo_node, &ast_node.AstMatch);
    }
  }

  fn flowInferCondition(self: *Self, flo_node: *FlowNode, node: *Node) !void {
    if (flo_node.res.isResolved()) return;
    flo_node.res = .Processing;
    // TODO: is there a need to explicitly merge types from incoming edges?
    self.ctx.varScope.pushScope();
    errdefer self.ctx.varScope.popScope();
    // This is a branch point and a meet point. 
    // As a meet point, all types on the incoming edges are merged.
    // As a branch point, types are narrowed along outgoing edges based 
    // on the condition expression.
    const curr_tenv = self.tenv;
    defer self.tenv = curr_tenv;
    var cond = node.AstCondition.cond;
    var env = TypeEnv.init(self.ctx.allocator(), null);
    self.tenv = &env;
    env.global.pushScope();
    self.narrow(cond, &env, true) catch |e| return e;
    if (node.getType()) |ty| {
      try self.checkCondition(ty, cond.getToken());
    } else {
      return self.error_(true, node.getToken(), "expected 'bool' type in condition expression", .{});
    }
    // get all nodes on the true edges & flowInfer with env
    var out_nodes = flo_node.getOutgoingNodes(.ETrue, self.ctx.allocator());
    self.copyEnv(&env);
    for (out_nodes.items()) |nd| {
      try self.flowInfer(nd, false);
    }
    self.ctx.varScope.popScope();
    self.ctx.varScope.pushScope();
    // get all nodes on the false edges & flowInfer with not\env
    out_nodes = flo_node.getOutgoingNodes(.EFalse, self.ctx.allocator());
    env.clear();
    env.global.pushScope();
    // TODO: finetune
    var last = self.diag.count();
    self.narrow(cond, &env, false) catch {
      self.diag.popUntil(last);
      env.narrowed.clearRetainingCapacity();
    };
    // set this attribute to aid function return type inference
    node.AstCondition.has_never_typ_in_false_path = env.hasNeverTy();
    self.copyEnv(&env);
    for (out_nodes.items()) |nd| {
      try self.flowInfer(nd, false);
    }
    flo_node.res = .Resolved;
    // when type checking conditions along the false path, check the first sequential node
    // after the nodes on the false path. If that node has only one incoming edge,
    // then don’t pop the type information gathered on the false path
    if (out_nodes.len() == 0) {
      var edges: usize = 0;
      for (flo_node.prev_next.items()) |itm| {
        // get the first sequential node after the nodes on the false path
        if (itm.next and itm.flo.edge == .ESequential) {
          // track the number of incoming edges from this node
          for (itm.flo.prev_next.items()) |fd| {
            if (fd.prev) {
              edges += 1;
            }
          }
          break;
        }
      }
      if (edges == 1) {
        // set all type information gathered to the current varScope
        var scope = self.ctx.varScope.decls.pop();
        var itr = scope.map.iterator();
        while (itr.next()) |entry| {
          self.insertVar(entry.key_ptr.*, entry.value_ptr.*);
        }
        return;
      }
    }
    self.ctx.varScope.popScope();
  }

  inline fn resolveNode(self: *Self, flo_node: *FlowNode, ast_node: *Node) !void {
    switch (ast_node.*) {
      .AstCondition => try self.flowInferCondition(flo_node, ast_node),
      .AstMCondition => try self.pc.flowInferMCondition(flo_node, ast_node),
      .AstEmpty => try self.flowInferMeta(flo_node, ast_node),
      .AstControl => {},
      .AstFun => |*fun| {
        // the entry flow-node sometimes stores a FunNode in its basic block (for future ref).
        // so we check if this flow-node is actually an entry node, or just a regular flow-node
        if (flo_node.isEntryNode()) {
          try self.flowInferEntry(flo_node);
        } else {
          // Some functions may be mutually recursive, or may depend on some other
          // functions not yet known at the time of inference of this current one.
          // So only partially infer this function. Full inference is done by need.
          if (!fun.isAnonymous()) {
            _ = try self.inferFunPartial(ast_node);
          } else {
            _ = try self.inferFun(ast_node, null);
          }
        }
      },
      .AstClass => {
        if (flo_node.isEntryNode()) {
          try self.flowInferEntry(flo_node);
        } else {
          _ = try self.inferClassPartial(ast_node);
        }
      },
      else => try self.flowInferNode(flo_node, ast_node),
    }
  }

  inline fn resolveBB(self: *Self, flo_node: *FlowNode) void {
    for (flo_node.bb.items()) |ast_node| {
      self.resolveNode(flo_node, ast_node) catch continue;
    }
    flo_node.res = .Resolved;
  }

  fn flowInfer(self: *Self, flo_node: *FlowNode, inferNext: bool) TypeCheckError!void {
    for (flo_node.prev_next.items()) |item| {
      // we can only proceed to resolve this node when all 
      // incoming edges have been resolved or when we're resolving 
      // all nodes on the true edges of a Condition.
      if (item.prev) {
        if (self.isFlowNodeUnresolved(item.flo)) {
          return;
        }
      }
    }
    self.resolveBB(flo_node);
    if (!inferNext) return;
    for (flo_node.prev_next.items()) |item| {
      if (item.next) {
        try self.flowInfer(item.flo, inferNext);
      }
    }
  }

  //***********************************************************//
  //***********  inference  ***********************************//
  //***********************************************************//
  /// resolves a builtin class type
  fn resolveBuiltinClassType(self: *Self, typ: *Type, token: Token) TypeCheckError!void {
    if (self.findBuiltinType(typ.klass().name)) |ty| {
      self.resolving.append(typ);
      typ.* = (try self.synthInferClsType(typ, ty, token)).*;
      typ.klass().setAsResolved();
      _ = self.resolving.pop();
    }
  }

  fn resolveUserClassType(self: *Self, typ: *Type, token: Token) TypeCheckError!void {
    if (self.findType(typ.klass().name)) |_ty| {
      self.resolving.append(typ);
      var ty = if (_ty.isTop()) _ty.top().child else _ty;
      typ.* = (try self.synthInferClsType(typ, ty, token)).*;
      if (typ.isClass()) {
        typ.klass().setAsResolved();
      } else {
        typ.instance().cls.klass().setAsResolved();
      }
      _ = self.resolving.pop();
    }
  }

  fn resolveClassType(self: *Self, typ: *Type, token: Token) TypeCheckError!void {
    if (typ.isClass()) {
      if (typ.klass().resolved) {
        return;
      } else if (self.resolving.contains(typ, Type.ptrEql)) {
        return;
      } else if (typ.klass().builtin) {
        return try self.resolveBuiltinClassType(typ, token);
      } else {
        return try self.resolveUserClassType(typ, token);
      }
    }
  }

  fn resolveType(self: *Self, typ: *Type, token: Token) TypeCheckError!void {
    return switch (typ.kind) {
      .Class => self.resolveClassType(typ, token),
      .Union => |*uni| {
        var tset = TypeHashSet.init(self.ctx.allocator());
        for (uni.variants.values()) |ty| {
          try self.resolveType(ty, token); 
          tset.set(ty.typeid(), ty);
        }
        typ.* = Type.compressTypes(&tset, typ).*;
      },
      .Constant, .Concrete, .Variable, .Recursive, .Function, .Top, .Tag, .TaggedUnion, .Instance => return,
      else => unreachable,
    };
  }

  fn synthInferClsType(self: *Self, user_ty: *Type, core_ty: *Type, token: Token) !*Type {
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    var core_cls = core_ty.klass();
    var user_cls = user_ty.klass();
    // generic
    if (core_cls.isGeneric()) {
      // FIXME: this check is a hack since resolving a CastNode's type after linking
      // (sometimes) breaks in the presence of recursive types.
      if (user_ty.hasRecursive()) return user_ty;
      if (!user_cls.isGeneric()) {
        return self.error_(
          true, token, "type '{s}' is generic but used without generic parameters", 
          .{self.getTypename(user_ty)}
        );
      }
      // if any type params in core_cls is already substituted, just return user_ty because
      // it means user_ty is already undergoing monomorphization from an earlier call
      if (!core_cls.getSlice()[0].isVariable()) {
        return user_ty;
      }
      var tparams = user_cls.tparams.?.*;
      if (!core_ty.isTupleTy()) {
        // typelinker & parser ensures that user_cls' tparams and core_cls' tparams are equal in size
        for (core_cls.tparams.?.items(), tparams.items()) |tvar, ty| {
          self.insertType(tvar, ty);
        }
      } else {
        // use union type to represent the multiple types of the tuple
        var uni = Type.newUnion(self.ctx.allocator()).box(self.ctx.allocator());
        uni.union_().addAll(&tparams);
        self.insertType(core_cls.tparams.?.itemAt(0), uni);
      }
      var synth_name = self.createSynthName(core_cls.name, core_cls.builtin, &tparams, null);
      if (self.findGenInfo(core_cls.node.?, synth_name)) |info| {
        return info.typ;
      }
      var cloned = core_cls.node.?.AstClass.clone(self.ctx.allocator());
      // we only link when we're sure core_cls is an unsubstituted generic type
      try self.linker.linkClass(&cloned.AstClass, true);
      // make class non-generic to allow inference
      cloned.AstClass.tparams = null;
      user_cls.node = cloned;
      var typ = try self.inferClass(cloned, user_ty);
      self.addGenInfo(
        core_cls.node.?,
        cloned,
        self.boxSynthName(core_cls.node.?.getToken(), synth_name),
        typ
      );
      return typ;
    }
    // non-generic
    if (core_cls.node) |cls_node| {
      var is_cyclic = self.cycles.contains(cls_node, Node.eql);
      if (!cls_node.AstClass.checked and !is_cyclic) {
        return try self.inferClass(cls_node, core_ty);
      }
    }
    return core_ty;
  }

  fn inferLiteral(self: *Self, node: *ast.LiteralNode, kind: types.Concrete) !*Type {
    if (node.typ) |typ| {
      return typ;
    } else {
      node.typ = kind.toType().box(self.ctx.allocator());
      node.typ.?.kind.Concrete.val = &node.token.value;
      return node.typ.?;
    }
  }

  fn inferNil(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.nil);
  }

  fn inferNumber(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.num);
  }

  fn inferString(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.str);
  }

  fn inferBool(self: *Self, node: *ast.LiteralNode) !*Type {
    return try self.inferLiteral(node, UnitTypes.bol);
  }

  fn inferList(self: *Self, node: *ast.ListNode) !*Type {
    const al = self.ctx.allocator();
    var base = Type.newBuiltinGenericClass(ks.ListVar, al);
    node.typ = base;
    if (node.elems.isEmpty()) {
      base.klass().appendTParam(&UnitTypes.tyAny);
      base.klass().empty = true;
      return base;
    }
    // infer type of elements stored in the list
    var typ = try self.infer(node.elems.itemAt(0));
    if (node.elems.len() > 1) {
      for (node.elems.items()) |elem| {
        var ty = try self.infer(elem);
        const debug = elem.getToken();
        _ = self.checkAssign(typ, ty, debug, false) catch {
          return self.error_(
            true, debug,
            "expected type '{s}', but found '{s}'",
            .{self.getTypename(typ), self.getTypename(ty)}
          );
        };
      }
    }
    base.klass().appendTParam(typ);
    return base;
  }

  fn inferTuple(self: *Self, node: *ast.ListNode) !*Type {
    const al = self.ctx.allocator();
    var base = Type.newBuiltinGenericClass(ks.TupleVar, al);
    node.typ = base;
    if (node.elems.isEmpty()) {
      base.klass().empty = true;
      return base;
    }
    // infer type of elements stored in the tuple
    var list = TypeList.init(al);
    for (node.elems.items()) |elem| {
      list.append(try self.infer(elem));
    }
    base.klass().appendTParamSlice(list.items());
    return base;
  }

  fn inferMap(self: *Self, node: *ast.MapNode) !*Type {
    // create a new type
    const al = self.ctx.allocator();
    var base = Type.newBuiltinGenericClass(ks.MapVar, al);
    node.typ = base;
    if (node.pairs.isEmpty()) {
      var any = &UnitTypes.tyAny;
      base.klass().appendTParamSlice(&[_]*Type{any, any});
      base.klass().empty = true;
      return base;
    }
    // infer type of items stored in the map
    var first_pair = node.pairs.itemAt(0);
    var key_typ = try self.infer(first_pair.key);
    var val_typ = try self.infer(first_pair.value);
    if (node.pairs.len() > 1) {
      for (node.pairs.items()[1..]) |pair| {
        var typ = try self.infer(pair.key);
        var debug = pair.key.getToken();
        _ = self.checkAssign(key_typ, typ, debug, false) catch {
          return self.error_(
            true, debug,
            "expected key type '{s}', but found '{s}'",
            .{self.getTypename(key_typ), self.getTypename(typ)}
          );
        };
        typ = try self.infer(pair.value);
        debug = pair.value.getToken();
        _ = self.checkAssign(val_typ, typ, debug, false) catch {
          return self.error_(
            true, debug,
            "expected value type '{s}', but found '{s}'",
            .{self.getTypename(val_typ), self.getTypename(typ)}
          );
        };
      }
    }
    std.debug.assert(!key_typ.isUnion() and !val_typ.isUnion());
    base.klass().appendTParamSlice(&[_]*Type{key_typ, val_typ});
    return base;
  }

  fn inferUnary(self: *Self, node: *ast.UnaryNode) !*Type {
    node.typ = try self.infer(node.expr);
    // unary op: ~, !, -, +
    if (node.op.optype == .OpBitInvert or node.op.optype == .OpAdd or node.op.optype == .OpSub) {
      try self.checkUnary(node, &UnitTypes.tyNumber);
    } else {
      std.debug.assert(node.op.optype == .OpNot);
      // `!` accepts any type and returns a boolean. 
      // It applies an implicit bool cast to such a type.
      node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    }
    return node.typ.?;
  }

  fn inferBinary(self: *Self, node: *ast.BinaryNode) !*Type {
    if (node.op.optype == .OpIs) {
      return self.inferIs(node);
    }
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    node.typ = lhsTy;
    try self.checkBinary(node, rhsTy, false);
    if (node.op.optype.isCmpOp()) {
      node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    }
    return node.typ.?;
  }

  fn inferIs(self: *Self, node: *ast.BinaryNode) !*Type {
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    var ty = node.right.getType().?;
    // lhs must not be type Type, and rhs must be type Type
    if (lhsTy.isTop()) {
      return self.error_(
        true, node.op.token,
        "Expected type instance in lhs of `is` operator but found '{s}'",
        .{self.getTypename(lhsTy)}
      );
    } else if (!rhsTy.isTop() and !rhsTy.isClass() or (ty.isLikeConstant() and !node.allow_consts)) {
      var help = (
        if (ty.isLikeConstant())
          "\n\tHelp: For constant types, consider using '==' or '!=' operator instead."
        else ""
      );
      return self.error_(
        true, node.op.token,
        "Expected type 'Type' in rhs of `is` operator but found type '{s}'{s}",
        .{self.getTypename(ty), help}
      );
    }
    // at this point, rhs is a TypeNode
    if (ty.isUnion() or ty.isVariable() or ty.isFunction()) {
      return self.error_(
        true, node.op.token,
        "Expected a concrete or class type in the rhs of the `is` operator but found '{s}'",
        .{self.getTypename(ty)}
      );
    }
    // temporarily assign lhsTy for checking
    node.typ = lhsTy;
    // use the actual type on rhs for checks
    try self.checkBinary(node, ty, true);
    // `is` returns type bool, so reassign
    node.typ = UnitTypes.bol.toType().box(self.ctx.allocator());
    try self.resolveType(ty, node.right.getToken());
    return node.typ.?;
  }

  fn inferVar(self: *Self, node: *ast.VarNode, emit: bool) !*Type {
    // TODO: Do we need to always fetch the updated type?
    var res = try self.lookupNameWithInfo(node, emit);
    if (!res.ident) {
      // TODO: need to cover more cases
      if (res.typ.isVariable()) {
        res.typ = try self.linker.resolve(res.typ, node.token);
      }
    }
    if (self.ctx.data.parent) |parent| {
      // type vars are only accessible to call & dot access nodes
      if (!parent.isCall() and !parent.isDotAccess()) {
        if (!res.ident) {
          return self.error_(
            true, node.token, "Could not resolve type of ident: '{s}'", .{node.token.value}
          );
        }
      }
    } else if (res.typ.isTag()) {
      // this is safe to check and error from here because a tag type referenced with
      // args will _always_ be intercepted by inferCall->inferTag and will never get here.
      if (res.typ.tag().paramsLen() > 0 and res.typ.tag().nameEql(node.token.value)) {
        self.softError(
          node.token, "Tag '{s}' referenced without arguments", .{res.typ.tag().name}
        );
      }
    }
    var typ = res.typ;
    node.typ = typ;
    // since we infer functions by need, we check if this is a reference
    // to a function and try to infer the function if it's not yet fully inferred
    if (typ.isFunction() and !typ.function().isGeneric()) {
      var fun_ty = typ.function();
      if (
        fun_ty.node != null
        and !fun_ty.node.?.AstFun.body.block().checked
        and !self.cycles.contains(fun_ty.node.?, Node.eql)
      ) {
        _ = try self.inferFun(fun_ty.node.?, typ);
      }
    } else if (typ.isClass() and !typ.klass().isGeneric()) {
      var cls_ty = typ.klass();
      if (
        cls_ty.node != null
        and !cls_ty.node.?.AstClass.checked
        and !self.cycles.contains(cls_ty.node.?, Node.eql)
      ) {
        _ = try self.inferClass(cls_ty.node.?, typ);
      }
    }
    return typ;
  }

  fn inferCast(self: *Self, node: *ast.CastNode) !*Type {
    try self.linker.linkCast(node);
    var typ = try self.infer(node.expr);
    var cast_typ = node.typn.typ;
    try self.resolveType(cast_typ, node.typn.token);
    var true_ctx: RelationContext = undefined;
    switch (node.expr.*) {
      .AstList, .AstTuple => |*col| {
        if (col.elems.len() == 0) {
          true_ctx = .RCConst;
        }
      },
      .AstMap => |*map| {
        if (map.pairs.len() == 0) {
          true_ctx = .RCConst;
        }
      },
      else => {
        true_ctx = .RCAny;
      }
    }
    // use coercion rules
    return try self.checkCast(typ, cast_typ, true_ctx, node.typn.token, true);
  }

  fn inferAssign(self: *Self, node: *ast.BinaryNode) !*Type {
    var lhsTy = try self.infer(node.left);
    var rhsTy = try self.infer(node.right);
    var typ = try self.checkAssign(lhsTy, rhsTy, node.op.token, true);
    // update type.
    switch (node.left.*) {
      // need to always update, because lookup copies.
      .AstVar => |ident| self.insertVar(ident.token.value, typ),
      .AstSubscript => |*sub| {
        if (sub.expr.getType()) |ty| {
          if (ty.isTupleTy() or ty.isClass() and ty.klass().immutable) {
            return self.error_(true, node.op.token,
              "Cannot modify immutable type '{s}'",
              .{self.getTypename(ty)}
            );
          }
        }
      },
      .AstDotAccess => |*da| {
        if (lhsTy.isFunction()) {
          if (da.lhs.getType()) |_ty| {
            var ty = _ty.classOrInstanceClass();
            if (ty.klass().getMethodTy(da.rhs.AstVar.token.value).? == lhsTy) {
              return self.error_(true, node.op.token,
                "Cannot modify immutable type '{s}'",
                .{self.getTypename(lhsTy)}
              );
            }
          }
        }
      },
      else => {}
    }
    return typ;
  }

  fn inferNType(self: *Self, node: *ast.TypeNode) !*Type {
    try self.linker.linkNType(node);
    // if this type node was found in an expression 
    // (i.e. not in an alias or annotation context), then return TyType
    if (!node.from_alias_or_annotation) {
      return &UnitTypes.TyTy;
    }
    return node.typ;
  }

  fn inferAlias(self: *Self, node: *ast.AliasNode) !*Type {
    try self.linker.linkAlias(node);
    return node.typ;
  }

  fn inferExprStmt(self: *Self, node: *ast.ExprStmtNode) !*Type {
    return try self.infer(node.expr);
  }

  fn inferVarDecl(self: *Self, node: *ast.VarDeclNode) !*Type {
    try self.linker.linkVarDecl(node);
    if (!node.is_param and (!node.is_field or node.has_default)) {
      if (node.ident.typ) |typ| {
        var expr_ty = try self.infer(node.value);
        _ = try self.checkInitAssign(typ, expr_ty, node.ident.token, true);
      } else {
        node.ident.typ = try self.infer(node.value);
      }
    }
    if (node.is_param) try self.resolveType(node.ident.typ.?, node.ident.token);
    self.insertVar(node.ident.token.value, node.ident.typ.?);
    return node.ident.typ.?;
  }

  fn inferSubscript(self: *Self, node: *ast.SubscriptNode) TypeCheckError!*Type {
    if (node.typ) |ty| return ty;
    // if we're narrowing this subscript, it's narrowed type would not be
    // set yet, hence, we use `node.narrowed == null` to check that we're not narrowing,
    // hence, the narrowed type should already exist in varScope
    if (node.narrowed == null and self.canNarrowSubscript(node)) {
      var env = TypeEnv.init(self.ctx.allocator(), null);
      env.global.pushScope();
      node.narrowed = self.synthesizeSubscript(node, &env, true) catch |e| return e;
      if (node.narrowed) |narrowed| {
        node.typ = self.inferVar(narrowed, false) catch null;
        if (node.typ) |ty| {
          return ty;
        }
      }
    }
    var expr_ty = try self.infer(node.expr);
    // fail fast
    if (!expr_ty.isClsGeneric()) {
      return self.error_(
        true, node.index.getToken(),
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    var index_ty = try self.infer(node.index);
    try self.checkSubscript(node, expr_ty, index_ty);
    return node.typ.?;
  }

  fn inferDeref(self: *Self, node: *ast.DerefNode) !*Type {
    if (node.typ) |ty| return ty;
    // if this node is not being narrowed (`node.narrowed == null`),
    // try to see if we can obtain an inferred narrow type.
    if (node.narrowed == null and self.canNarrowDeref(node)) {
      var env = TypeEnv.init(self.ctx.allocator(), self.tenv);
      env.global.pushScope();
      node.narrowed = self.synthesizeDeref(node, &env, true) catch return error.CheckError;
      if (node.narrowed) |narrowed| {
        node.typ = self.inferVar(narrowed, false) catch null;
        if (node.typ) |ty| {
          return ty;
        }
      }
    }
    var expr_ty = try self.infer(node.expr);
    try self.checkDeref(node, expr_ty);
    return node.typ.?;
  }

  fn inferBlock(self: *Self, node: *ast.BlockNode) !*Type {
    self.ctx.enterScope();
    for (node.nodes.items()) |item| {
      _ = try self.infer(item);
    }
    self.ctx.leaveScope();
    node.checked = true;
    return self.void_ty;
  }

  fn inferScope(self: *Self, node: *ast.ScopeNode) !*Type {
    if (node.enter) {
      self.ctx.enterScope();
    } else {
      self.ctx.leaveScope();
    }
    return self.void_ty;
  }

  fn inferWhile(self: *Self, node: *ast.WhileNode) !*Type {
    var ty = try self.infer(node.cond);
    try self.checkCondition(ty, node.cond.getToken());
    return try self.infer(node.then);
  }

  fn createFunType(self: *Self, ast_node: *Node, ret: ?*Type) !*Type {
    var ret_ty = blk: {
      if (ret) |typ| {
        break :blk typ;
      } else {
        var tmp = Type.newVariable(self.ctx.allocator());
        tmp.variable().append(Token.getDefault());
        break :blk tmp.box(self.ctx.allocator());
      }
    };
    var ty = Type.newFunction(self.ctx.allocator(), ret_ty);
    var fun = ty.function();
    var node = &ast_node.AstFun;
    fun.node = ast_node;
    fun.tparams = node.tparams;
    fun.params.ensureTotalCapacity(node.params.capacity());
    for (node.params.items()) |vd| {
      fun.params.append(vd.ident.typ.?);
    }
    ty.variadic = ast_node.AstFun.variadic;
    return ty.box(self.ctx.allocator());
  }

  fn getFlowNextOrPrev(self: *Self, node: *FlowNode, comptime passes: fn(itm: flow.FlowData) bool) FlowList {
    var nodes = FlowList.init(self.ctx.allocator());
    for (node.prev_next.items()) |nd| {
      if (passes(nd)) {
        nodes.append(nd.flo);
      }
    }
    return nodes;
  }

  inline fn getPrevFlowNodes(self: *Self, node: *FlowNode) FlowList {
    return self.getFlowNextOrPrev(node, FlowNode.isPrev);
  }

  inline fn getNextFlowNodes(self: *Self, node: *FlowNode) FlowList {
    return self.getFlowNextOrPrev(node, FlowNode.isNext);
  }

  fn inferFunPartial(self: *Self, node: *Node) !*Type {
    var fun = &node.AstFun;
    try self.linker.linkFun(fun, false);
    // we need to infer this first to handle recursive functions
    var ty = try self.createFunType(node,  node.getType());
    // set the function's name (if available) to it's full type
    if (fun.name) |ident| {
      self.insertVar(ident.token.value, ty);
    }
    return ty;
  }

  fn inferFun(self: *Self, node: *Node, typ: ?*Type) !*Type {
    var fun = &node.AstFun;
    var ty = (
      typ orelse if (fun.name) |ident| try self.lookupName(ident, true)
      else try self.createFunType(node, node.getType())
    );
    // generic is infer-by-need - performed on call/reference.
    if (!fun.isGeneric()) {
      var flo = blk: {
        if (self.cfg.lookupFunc(node)) |fm| break :blk fm;
        std.debug.assert(!node.AstFun.isGeneric());
        try self.linker.linkFun(fun, true);
        try self.buildFunFlow(node);
        break :blk self.cfg.lookupFunc(node).?;
      };
      try self.analyzer.analyzeDeadCode(flo.dead);
      const _dead_size = flo.dead.prev_next.len();
      self.cycles.append(node);
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      var prev_fn = self.current_fn;
      self.current_fn = &flo;
      try self.flowInfer(flo.entry, true);
      self.current_fn = prev_fn;
      _ = self.cycles.pop();
      ty.function().ret = try self.inferFunReturnType(node, flo, ty);
      fun.body.block().checked = true;
      // check again since match statements can modify the function's cfg
      if (flo.dead.prev_next.len() != _dead_size) try self.analyzer.analyzeDeadCode(flo.dead);
      try self.analyzer.analyzeDeadCodeWithTypes(flo.entry);
    }
    return ty;
  }

  fn inferFunReturnTypePartial(self: *Self, flo: FlowMeta) ?*Type {
    var prev_nodes = self.getPrevFlowNodes(flo.exit);
    // unionify all return types at exit
    var uni = Union.init(self.ctx.allocator());
    for (prev_nodes.items()) |flo_nd| {
      if (flo_nd.bb.getNonScopeLast()) |nd| {
        if (nd.isRet()) {
          if (nd.getType()) |ty| {
            uni.set(ty);
          }
        }
      }
    }
    if (uni.variants.count() == 0) {
      return null;
    } else if (uni.variants.count() == 1) {
      return uni.variants.values()[0];
    }
    return uni.toType().box(self.ctx.allocator());
  }

  fn looksLikeNeverType(self: *Self, flo_nodes: *FlowList, fun_ty: *Type) bool {
    // Conservatively check if a function calls itself infinitely
    _ = self;
    if (flo_nodes.len() == 1) {
      if (flo_nodes.itemAt(0).bb.len() == 1) {
        const node = flo_nodes.itemAt(0).bb.getLast().?;
        if (node.isExprStmt() and node.AstExprStmt.expr.isCall()) {
          const call = &node.AstExprStmt.expr.AstCall;
          if (call.expr.getType()) |typ| {
            return typ == fun_ty;
          }
        }
      }
    }
    return false;
  }

  fn excludeNoreturn(self: *Self, typ: *Type) *Type {
    var uni = Union.init(self.ctx.allocator());
    for (typ.union_().variants.values()) |ty| {
      if (ty.isNoreturnTy()) {
        continue;
      }
      uni.set(ty);
    }
    if (uni.variants.count() == typ.union_().variants.count()) return typ;
    return uni.toType().box(self.ctx.allocator());
  }

  fn returnType(self: *Self, typ: *Type, token: Token) *Type {
    if (typ.isUnion()) {
      var ty = self.excludeNoreturn(typ);
      if (ty.isUnion()) {
        var all_tags = true;
        for (ty.union_().variants.values()) |ty_| {
          if (!ty_.isTag()) {
            all_tags = false;
            break;
          }
        }
        if (!all_tags) {
          self.softError(token, "cannot return multiple types: '{s}'", .{self.getTypename(typ)});
        } else {
          return ty.toTaggedUnion();
        }
      }
    }
    return typ;
  }

  fn inferFunReturnType(self: *Self, node: *Node, flo: FlowMeta, fun_ty: *Type) !*Type {
    var prev_flo_nodes = self.getPrevFlowNodes(flo.exit).filter(FlowNode.FilterPredicate.isNotDeadNode);
    var has_void_ty = false;
    var has_rec_ty = false;
    var has_non_void_ty = false;
    var has_noreturn_ty = false;
    var has_return_node = false;
    const al = self.ctx.allocator();
    for (prev_flo_nodes.items()) |flo_nd| {
      if (flo_nd.bb.getNonScopeLast()) |nd| {
        if (nd.isRet()) {
          has_return_node = true;
          if (nd.getType()) |ty| {
            if (ty.isRecursive()) {
              has_rec_ty = true;
            } else if (ty.isVoidTy()) {
              has_void_ty = true;
            } else if (ty.isNoreturnTy()) {
              has_noreturn_ty = true;
            } else {
              has_non_void_ty = true;
            }
          } else {
            has_void_ty = true;
          }
        } else if (nd.getType()) |ty| {
          if (ty.isNoreturnTy()) {
            has_noreturn_ty = true;
          } else if (nd.isMatch()) {
            continue;
          } else if (nd.isCondition() or nd.isMCondition()) {
            has_void_ty = !(
              if (nd.isCondition()) nd.AstCondition.has_never_typ_in_false_path 
              else nd.AstMCondition.tst.AstCondition.has_never_typ_in_false_path
            );
          } else {
            has_void_ty = true;
          }
        }
      }
    }
    var add_never_ty = false;
    // if it looks like a never type function, set rec & never
    if (self.looksLikeNeverType(&prev_flo_nodes, fun_ty)) {
      add_never_ty = true;
      has_rec_ty = true;
      has_void_ty = false;
    }
    // if we find the function has type noreturn, and there's no return node in the function,
    // simply turn off has_void_ty:
    if (has_noreturn_ty and !has_return_node) {
      has_void_ty = false;
    }
    // When inferring a function's return type:
    // - if we find a recursive type, and a non-void type, use the non-void type as the function's return type
    // - if we find a recursive type, and a void type or only a recursive type, then use the type 'never'
    var uni = Union.init(al);
    var void_ty: *Type = self.void_ty;
    var nvr_ty: *Type = if (has_rec_ty) Type.newNever(al) else undefined;
    // unionify all return types at exit
    for (prev_flo_nodes.items()) |flo_nd| {
      if (flo_nd.bb.getNonScopeLast()) |nd| {
        if (!nd.isRet()) continue;
        var typ = nd.getType() orelse void_ty;
        if (!has_void_ty and typ.isVoidTy()) {
          continue;
        }
        if (has_rec_ty) {
          if (has_non_void_ty) {
            if (typ.isRecursive()) {
              continue;
            }
          } else if (has_void_ty or typ.isRecursive()) {
            typ = nvr_ty;
          }
        }
        uni.set(typ);
      }
    }
    if (add_never_ty) uni.set(nvr_ty);
    if (has_noreturn_ty) uni.set(Type.newConcrete(.TyNoReturn).box(al));
    if (has_void_ty) uni.set(void_ty);
    var inf_ret_ty = if (uni.variants.isNotEmpty()) uni.toType() else self.void_ty.*;
    const token = node.getToken();
    if (node.AstFun.ret) |ret| {
      var ret_ty = ret.AstNType.typ;
      for (prev_flo_nodes.items()) |flo_nd| {
        if (flo_nd.bb.getNonScopeLast()) |nd| {
          if (nd.isRet()) {
            if (nd.AstRet.typ) |typ| {
              if (typ.isRecursive()) {
                continue;
              }
              if (!node.AstFun.is_builtin and !ret_ty.isRelatedTo(typ, .RCAny, al)) {
                return self.error_(
                  true, nd.getToken(),
                  "Expected return type '{s}', but got '{s}'",
                  .{self.getTypename(ret_ty), self.getTypename(typ)}
                );
              }
            }
          } else if (!node.AstFun.is_builtin and !ret_ty.isLikeVoid() and !ret_ty.isLikeNoreturn()) {
            if (!(ret_ty.isRecursive() and ret_ty.recursive().base.isNeverTy())) {
              // control reaches exit from this node (`nd`), although this node
              // doesn't return anything hence (void), but return type isn't void
              return self.error_(
                true, nd.getToken(),
                "Control flow reaches exit from this point without returning type '{s}'",
                .{self.getTypename(ret_ty)}
              );
            }
          }
        }
      }
      var ty = self.returnType(&inf_ret_ty, token);
      if (!node.AstFun.is_builtin and !ret_ty.isRelatedTo(ty, .RCAny, al)) {
        if (!ret_ty.isNoreturnTy()) {
          return self.error_(
            true, token,
            "Expected return type '{s}', but got '{s}'",
            .{self.getTypename(ret_ty), self.getTypename(&inf_ret_ty)}
          );
        } else {
          var _token = (
            if (prev_flo_nodes.isNotEmpty()) prev_flo_nodes.itemAt(0).bb.nodes.itemAt(0).getToken()
            else token
          );
          return self.error_(
            true, _token,
            "Control flow reaches exit; function declared type '" ++ ks.NoReturnVar ++ "' returns",
            .{}
          );
        }
      }
      return self.returnType(ret_ty, token);
    }
    const ty = self.returnType(inf_ret_ty.box(al), token);
    node.AstFun.ret = @as(Node, .{.AstNType = ast.TypeNode.init(ty, token)}).box(al);
    return ty;
  }

  fn createClsType(self: *Self, node: *Node) *Type {
    var cls = &node.AstClass;
    var methods = TypeList.init(self.ctx.allocator()).boxEnsureCapacity(cls.methods.capacity());
    return Class.init(
      cls.name.token.value, cls.fields, methods,
      cls.tparams, node, false, cls.is_builtin
    ).toType().box(self.ctx.allocator());
  }

  fn forbidReturnInInit(self: *Self, node: *Node) void {
    // we do not want any return statements in an init method, to aid
    // auto-returning of the self parameter after init() is called
    var list = self.getPrevFlowNodes(self.cfg.lookupFunc(node).?.exit);
    for (list.items()) |flo_nd| {
      for (flo_nd.bb.items()) |nd| {
        if (nd.isRet()) {
          self.softError(
            nd.getToken(),
            "illegal return statement in `init` method", .{}
          );
          break;
        }
      }
    }
  }

  fn inferClassPartial(self: *Self, node: *Node) !*Type {
    // we need to infer this first to handle recursive classes
    var cls = &node.AstClass;
    var ty = self.createClsType(node);
    // set the class's name to it's full type
    self.ctx.typScope.insert(cls.name.token.value, ty);
    return ty;
  }

  fn inferClass(self: *Self, node: *Node, typ: ?*Type) !*Type {
    var cls = &node.AstClass;
    var ty = typ orelse try self.lookupName(cls.name, true);
    // generic is infer-by-need - performed on call/reference.
    if (!cls.isGeneric()) {
      self.cycles.append(node);
      self.ctx.enterScope();
      defer self.ctx.leaveScope();
      if (!cls.is_builtin) {
        for (cls.fields.items()) |itm| {
          _ = try self.inferVarDecl(&itm.AstVarDecl);
        }
        ty.klass().fields = cls.fields;
        if (cls.methods.isNotEmpty()) {
          self.insertVar(ks.SelfVar, ty);
          defer self.deleteVar(ks.SelfVar);
          for (cls.methods.items(), 0..) |itm, i| {
            var fun_ty = try self.inferFunPartial(itm);
            ty.klass().methods.append(fun_ty);
            fun_ty = try self.inferFun(itm, fun_ty);
            ty.klass().methods.items()[i] = fun_ty;
          }
          if (ty.klass().getMethod(ks.InitVar)) |mtd| {
            self.forbidReturnInInit(mtd);
          }
        }
      } else {
        for (cls.fields.items()) |itm| {
          _ = try self.inferVarDecl(&itm.AstVarDecl);
        }
        ty.klass().fields = cls.fields;
        for (cls.methods.items(), 0..) |itm, i| {
          var fun_ty = try self.inferFunPartial(itm);
          ty.klass().methods.append(fun_ty);
          fun_ty = try self.inferFun(itm, fun_ty);
          ty.klass().methods.items()[i] = fun_ty;
        }
      }
      _ = self.cycles.pop();
      cls.checked = true;
      ty.klass().resolved = true;
    }
    cls.typ = ty;
    return ty;
  }

  fn validateCallArgCount(self: *Self, fun_ty: *types.Function, node: *ast.CallNode) !void {
    if (fun_ty.params.len() != node.args.len()) {
      if (node.variadic) {
        // argc -> == n - 1 or > n
        if (!(node.args.len() == fun_ty.params.len() - 1 or node.args.len() > fun_ty.params.len())) {
          return self.error_(
            true, node.expr.getToken(),
            "Argument mismatch. Expected {} argument(s) but found {}", .{fun_ty.params.len() - 1, node.args.len()}
          );
        }
      } else {
        return self.error_(
          true, node.expr.getToken(),
          "Argument mismatch. Expected {} argument(s) but found {}", .{fun_ty.params.len(), node.args.len()}
        );
      }
    }
  }

  fn validateLabeledCallArguments(self: *Self, fun_ty: *types.Function, node: *ast.CallNode) !void {
    if (node.labeled) {
      var fun = &fun_ty.node.?.AstFun;
      var map = std.StringHashMap([]const u8).init(self.ctx.allocator());
      defer map.clearAndFree();
      // find duplicate labels
      for (node.args.items()) |arg| {
        if (arg.isLblArg()) {
          var str = arg.AstLblArg.label.value;
          if (map.get(str)) |_| {
            if (node.variadic) {
              // skip if this is an arg to the variadic param
              if (fun.params.getLast().ident.token.valueEql(str)) {
                continue;
              }
            }
            self.softError(arg.AstLblArg.label, "duplicate labeled argument found", .{});
          } else {
            map.put(str, str) catch {};
          }
        }
      }
      // simply sort the labeled arguments
      for (fun.params.items(), 0..) |param, i| {
        for (node.args.items(), 0..) |arg, j| {
          if (arg.isLblArg()) {
            var lbl = arg.AstLblArg.label;
            if (param.ident.token.valueEql(lbl.value)) {
              if (i >= node.args.len()) {
                self.softError(lbl, "missing required argument(s)", .{});
                continue;
              }
              node.args.items()[j] = node.args.items()[i];
              node.args.items()[i] = arg.AstLblArg.value;
            }
          }
        }
      }
      if (self.diag.hasErrors()) return error.CheckError;
      // report errors for unsorted/untransformed labeled args if any
      for (node.args.items()) |arg| {
        if (arg.isLblArg()) {
          return self.error_(true, arg.AstLblArg.label, "illegal or invalid label", .{});
        }
      }
    }
  }

  fn validateCallArguments(self: *Self, fun_ty: *types.Function, node: *ast.CallNode) !void {
    try self.validateLabeledCallArguments(fun_ty, node);
    if (!node.variadic) {
      for (fun_ty.params.items(), node.args.items()) |p_ty, arg| {
        var arg_ty = try self.infer(arg);
        if (!p_ty.isRelatedTo(arg_ty, .RCAny, self.ctx.allocator())) {
          return self.error_(
            true, arg.getToken(),
            "Argument mismatch. Expected type '{s}' but found '{s}'",
            .{self.getTypename(p_ty), self.getTypename(arg_ty)}
          );
        }
        // set extra 'node' metadata for this arg type, which would be null if
        // arg type was created by a user
        if (p_ty.isFunction() and arg.isFun()) {
          p_ty.function().node = arg;
        }
      }
    } else {
      var non_varargs_len = fun_ty.params.len() - 1;
      node.va_start = non_varargs_len;
      if (node.args.len() == 0) return;
      // validate regular args
      for (fun_ty.params.items()[0..non_varargs_len], node.args.items()[0..non_varargs_len]) |p_ty, arg| {
        var arg_ty = try self.infer(arg);
        if (!p_ty.isRelatedTo(arg_ty, .RCAny, self.ctx.allocator())) {
          return self.error_(
            true, arg.getToken(),
            "Argument mismatch. Expected type '{s}' but found '{s}'",
            .{self.getTypename(p_ty), self.getTypename(arg_ty)}
          );
        }
        // set extra 'node' metadata for this arg type, which would be null if
        // arg type was created by a user
        if (p_ty.isFunction() and arg.isFun()) {
          p_ty.function().node = arg;
        }
      }
      // validate varargs
      // we expect the variadic type to be a tuple, so perform relations check with it tparam:
      var va_ty = fun_ty.params.getLast().klass().tparams.?.itemAt(0);
      for (node.args.items()[non_varargs_len..]) |arg| {
        var arg_ty = try self.infer(arg);
        if (!va_ty.isRelatedTo(arg_ty, .RCAny, self.ctx.allocator())) {
          return self.error_(
            true, arg.getToken(),
            "Argument mismatch. Expected type '{s}' but found '{s}'",
            .{self.getTypename(va_ty), self.getTypename(arg_ty)}
          );
        }
      }
    }
  }

  /// error if this tag is parameterized but is being accessed like a non-parameterized tag, for ex:
  /// type Tag = Foo(str) | Bar(num) ; Tag.Foo !error!
  inline fn errorIfParameterizedTagAccessedBadly(self: *Self, ty: *Type, parent: ?*Node, token: Token) !void {
    if (ty.tag().paramsLen() > 0 and parent != null and !parent.?.isCall()) {
      return self.error_(
        true, token, "bad access of parameterized tag type '{s}'",
        .{self.getTypename(ty)}
      );
    }
  }

  fn inferDotAccess(self: *Self, node: *ast.DotAccessNode, ast_node: *Node) !*Type {
    if (node.typ) |ty| return ty;
    // set parent for later type disambiguation
    var parent = self.ctx.data.parent;
    errdefer self.ctx.data.parent = parent;
    if (node.parent == null) {
      node.parent = parent;
    }
    self.ctx.data.parent = ast_node;
    // if this node is not being narrowed (`node.narrowed == null`),
    // try to see if we can obtain an inferred narrow type.
    if (node.narrowed == null and self.canNarrowDotAccess(node)) {
      var env = TypeEnv.init(self.ctx.allocator(), self.tenv);
      env.global.pushScope();
      node.narrowed = self.synthesizeDotAccess(node, ast_node, &env, true) catch return error.CheckError;
      if (node.narrowed) |narrowed| {
        node.typ = self.inferVar(narrowed, false) catch null;
        if (node.typ) |ty| {
          return ty;
        }
      }
    }
    var lhs_ty = try self.infer(node.lhs);
    self.ctx.data.parent = parent;
    const lhs_tok = node.lhs.getToken();
    const rhs_tok = node.rhs.getToken();
    if (lhs_ty.isStrTy()) {
      node.lhs.forceSetType(self.str_ty);
      lhs_ty = self.str_ty;
    } else if (lhs_ty.isTag()) {
      if (node.rhs.isNumberLiteral()) {
        // if this is a Just tag or this node has permission to use a number literal access:
        if (lhs_ty.isJustTy() or node.allow_tag) {
          return self.checkTagDotAccess(node, lhs_ty, node.rhs.toIntNumber(usize));
        } else {
          return self.error_(
            true, lhs_tok, "expected 'Maybe' type, found '{s}'",
            .{self.getTypename(lhs_ty)}
          );
        }
      } else if (lhs_ty.tag().nameEql(rhs_tok.value)) { // check if this tag type has this tag
        try self.errorIfParameterizedTagAccessedBadly(lhs_ty, node.parent, rhs_tok);
        node.typ = lhs_ty;
        return lhs_ty;
      }
    } else if (lhs_ty.isTaggedUnion()) {
      // check if this tagged union has a tag with same name as rhs of the dot access expr
      if (lhs_ty.taggedUnion().hasTag(rhs_tok.value)) |ty| {
        try self.errorIfParameterizedTagAccessedBadly(ty, node.parent, rhs_tok);
        node.typ = ty;
        return ty;
      } else {
        return self.error_(
          true, lhs_tok, "type '{s}' has no tag '{s}'",
          .{self.getTypename(lhs_ty), rhs_tok.value}
        );
      }
    }
    if (!lhs_ty.isClass() and !lhs_ty.isInstance()) {
      return self.error_(
        true, lhs_tok, "type '{s}' has no property '{s}'",
        .{self.getTypename(lhs_ty), rhs_tok.value}
      );
    }
    var ty = lhs_ty.classOrInstanceClass();
    try self.resolveType(ty, lhs_tok);
    try self.checkDotAccess(node, ty, &node.rhs.AstVar);
    return node.typ.?;
  }
  
  fn inferCall(self: *Self, node: *ast.CallNode, ast_node: *Node) !*Type {
    // set parent for later type disambiguation
    var parent = self.ctx.data.parent;
    errdefer self.ctx.data.parent = parent;
    self.ctx.data.parent = ast_node;
    var _ty = try self.infer(node.expr);
    self.ctx.data.parent = parent;
    if (_ty.isClass()) {
      return try self.inferClsCall(node, _ty);
    } else if (_ty.isTag()) {
      return try self.inferTagCall(node, _ty);
    }
    // check that we're actually calling a function
    if (!_ty.isFunction() and !_ty.isMethod()) {
      return self.error_(
        true, node.expr.getToken(),
        "Expected callable or function type but found '{s}'", .{self.getTypename(_ty)}
      );
    }
    var fun_ty: *types.Function = undefined;
    var ty: *Type = undefined;
    if (_ty.isFunction()) {
      fun_ty = _ty.function();
      ty = _ty;
    } else {
      fun_ty = _ty.method().func.function();
      self.insertVar(ks.SelfVar, _ty.method().cls);
      ty = _ty.method().func;
    }
    defer {
      if (_ty.isMethod()) {
        self.deleteVar(ks.SelfVar);
      }
    }
    node.variadic = ty.variadic;
    // perform signature checks
    try self.validateCallArgCount(fun_ty, node);
    // check generic is properly called
    if (node.isGeneric() and !fun_ty.isGeneric()) {
      return self.error_(
        true, node.expr.getToken(),
        "Non-generic function called as generic", .{}
      );
    }
    if (!fun_ty.isGeneric()) {
      var is_cyclic = false;
      if (fun_ty.node) |fun_node| {
        is_cyclic = self.cycles.contains(fun_node, Node.eql);
        if (!fun_node.AstFun.body.block().checked and !is_cyclic) {
          _ = try self.inferFun(fun_node, ty);
        }
      }
      try self.validateCallArguments(fun_ty, node);
      // [N*]: check if we're currently resolving this type, i.e. if it's recursive
      if (is_cyclic) {
        if (self.inferFunReturnTypePartial(self.cfg.lookupFunc(fun_ty.node.?).?)) |typ| {
          return typ;
        }
        return ty.newRecursive().box(self.ctx.allocator());
      }
      node.typ = fun_ty.ret;
      return fun_ty.ret;
    }
    if (node.targs) |targs| {
      if (targs.len() != fun_ty.tparams.?.len()) {
        return self.error_(
          true, node.expr.getToken(),
          "Generic function not instantiated correctly. Expected {} type parameter(s), but found {}",
          .{fun_ty.tparams.?.len(), targs.len()}
        );
      }
    }
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    // generic, so monomorphize.
    var args_inf = TypeList.init(self.ctx.allocator());
    for (node.args.items()) |arg| {
      args_inf.append(try self.infer(arg));
    }
    var old_fun_node = fun_ty.node.?;
    var fun = &old_fun_node.AstFun;
    // inferred type arguments must match the generic type params if specified
    // ex: foo{str, num}('a', 5)
    if (node.targs) |targs| {
      for (fun.tparams.?.items(), 0..) |tvar, tpos| {
        for (fun.params.items(), 0..) |param, ppos| {
          var typ = param.ident.typ.?;
          if (typ.typeidEql(tvar)) {
            if (!targs.itemAt(tpos).getType().?.typeidEql(args_inf.itemAt(ppos))) {
              return self.error_(
                true, node.args.itemAt(ppos).getToken(),
                "Type parameter mismatch. Expected type '{s}', but found '{s}'",
                .{self.getTypename(targs.itemAt(tpos).getType().?), self.getTypename(args_inf.itemAt(ppos))}
              );
            }
          }
        }
      }
      for (fun_ty.tparams.?.items(), targs.items()) |tvar, nd| {
        self.insertType(tvar, nd.getType().?);
      }
    } else {
      for (fun.tparams.?.items()) |tvar| {
        var resolved = false;
        for (fun.params.items(), 0..) |param, ppos| {
          var typ = param.ident.typ.?;
          if (typ.typeidEql(tvar)) {
            self.insertType(tvar, args_inf.itemAt(ppos));
            resolved = true;
            break;
          }
        }
        if (!resolved) {
          self.softError(
            tvar.variable().tokens.itemAt(0),
            "Could not resolve the generic type variable '{s}'. Consider explicitly specifying the type parameter",
            .{self.getTypename(tvar)}
          );
          return self.error_(true, node.expr.getToken(), "Called from here:", .{});
        }
      }
    }
    // link the function
    // lookup node using inferred args.
    // - if found just return the ret type of the found node
    // - else, do the stuff below, and cache the node
    var synth_name = self.createFunSynthName(fun, &args_inf, node.targs);
    if (self.findGenInfo(old_fun_node, synth_name)) |info| {
      node.expr.setType(info.typ);
      node.typ = info.typ.function().ret;
      return info.typ.function().ret;
    }
    var new_fun_node = fun.clone(self.ctx.allocator());
    var newfun = &new_fun_node.AstFun;
    // keep tparams to ban use as alias in types contained in fun, as much as possible
    try self.linker.linkFun(newfun, true);
    // make fun non-generic to allow building of the cfg
    newfun.tparams = null;
    try self.buildFunFlow(new_fun_node);
    self.ctx.varScope.pushScope();
    defer self.ctx.varScope.popScope();
    var new_fun_ty = try self.inferFunPartial(new_fun_node);
    for (new_fun_ty.function().params.items(), 0..) |typ, ppos| {
      if (typ.isFunction() and node.args.itemAt(ppos).isFun()) {
        typ.function().node = node.args.itemAt(ppos);
      }
    }
    // validate monomorphized function params
    try self.validateCallArguments(new_fun_ty.function(), node);
    var typ = try self.inferFun(new_fun_node, new_fun_ty);
    node.typ = typ.function().ret;
    node.expr.setType(new_fun_ty);
    self.addGenInfo(
      old_fun_node,
      new_fun_node,
      self.boxSynthName(old_fun_node.getToken(), synth_name),
      typ
    );
    return node.typ.?;
  }

  fn validateClsCallArguments(self: *Self, node: *ast.CallNode, fun_ty: *Type, token: Token) !void {
    if (!fun_ty.function().ret.isVoidTy()) {
      return self.error_(
        true, token, "Expected 'void' return type in `init` method, but found '{s}'", 
        .{self.getTypename(fun_ty.function().ret)}
      );
    }
    try self.validateCallArguments(fun_ty.function(), node);
  }

  fn validateFieldInitializations(self: *Self, init_mtd: ?*Node, cls_ty: *types.Class, node: *ast.CallNode) !void {
    if (cls_ty.fields.len() == 0) return;
    if (init_mtd == null) {
      for (cls_ty.fields.items()) |field| {
        if (!field.AstVarDecl.has_default) {
          self.softError(
            (cls_ty.node orelse node.expr).getToken(),
            "a class having field(s) without defaults " 
            ++ "must define an `init` method that initializes such field(s)",
            .{}
          );
          return self.error_(
            true, field.AstVarDecl.ident.token,
            "field '{s}' is declared but uninitialized",
            .{field.AstVarDecl.ident.token.value}
          );
        }
      }
    }
    if (init_mtd) |mtd| {
      // This only verifies definite assignments from direct statements on a seq edge for now
      var stmts = NodeList.init(self.ctx.allocator());
      for (mtd.AstFun.body.block().nodes.items()) |itm| {
        if (itm.isExprStmt()) {
          stmts.append(itm);
        }
      }
      var errors: usize = 0;
      for (cls_ty.fields.items()) |field| {
        if (field.AstVarDecl.has_default) continue;
        var initialized = false;
        for (stmts.items()) |stmt| {
          if (stmt.AstExprStmt.isSelfDotAccessAssignment()) |da| {
            if (field.AstVarDecl.ident.token.valueEql(da.rhs.AstVar.token.value)) {
              initialized = true;
              break;
            }
          }
        }
        if (!initialized) {
          errors += 1;
          self.softError(
            field.AstVarDecl.ident.token,
            "I am unable to deduce that the field '{s}' is definitely initialized",
            .{field.AstVarDecl.ident.token.value}
          );
        }
      }
      if (errors > 0) return error.CheckError;
    } else {
      var inits: usize = 0;
      for (cls_ty.fields.items()) |field| {
        if (field.AstVarDecl.has_default) {
          inits += 1;
        }
      }
      if (inits != cls_ty.fields.len()) {
        return self.error_(
          true, node.expr.getToken(),
          "class {s} have fields that are uninitialized",
          .{cls_ty.name}
        );
      }
    }
  }

  fn inferClsCall(self: *Self, node: *ast.CallNode, ty: *Type) !*Type {
    // insert self into current scope
    self.insertVar(ks.SelfVar, ty);
    defer self.deleteVar(ks.SelfVar);
    const al = self.ctx.allocator();
    // infer
    var cls_ty = ty.klass();
    var init_mtd = cls_ty.getMethodTy(ks.InitVar);
    var init_mtd_node = cls_ty.getMethod(ks.InitVar);
    if (init_mtd_node) |_nd| {
      node.variadic = _nd.AstFun.variadic;
      var fun_ty = try self.createFunType(_nd, null);
      try self.validateCallArgCount(fun_ty.function(), node);
    } else if (node.args.isNotEmpty()) {
      // if the class has no init method, it shouldn't be called with arguments
      return self.error_(
        true, node.expr.getToken(),
        "Too many arguments to class call. Expected none, but got {}.", .{node.args.len()}
      );
    }
    // check fields
    try self.validateFieldInitializations(init_mtd_node, cls_ty, node);
    // check generic is properly called
    if (node.isGeneric() and !cls_ty.isGeneric()) {
      return self.error_(
        true, node.expr.getToken(),
        "Non-generic class called as generic", .{}
      );
    }
    if (!cls_ty.isGeneric()) {
      if (cls_ty.node) |cls_node| {
        var is_cyclic = self.cycles.contains(cls_node, Node.eql);
        if (!cls_node.AstClass.checked and !is_cyclic) {
          _ = try self.inferClass(cls_node, ty);
          // try to obtain the inferred init since this class just got checked
          init_mtd = cls_ty.getMethodTy(ks.InitVar);
        }
      }
      if (init_mtd_node) |_nd| {
        var fun_ty = init_mtd orelse try self.createFunType(_nd, null);
        try self.validateClsCallArguments(node, fun_ty, _nd.getToken());
      }
      node.typ = ty.toInstance(al);
      return node.typ.?;
    }
    if (node.targs) |targs| {
      if (targs.len() != cls_ty.tparams.?.len()) {
        return self.error_(
          true, node.expr.getToken(),
          "Generic class instantiated incorrectly. Expected {} type parameter(s), but found {}",
          .{cls_ty.tparams.?.len(), targs.len()}
        );
      }
    }
    self.ctx.enterScope();
    defer self.ctx.leaveScope();
    // generic, so monomorphize.
    var args_inf = TypeList.init(al);
    for (node.args.items()) |arg| {
      args_inf.append(try self.infer(arg));
    }
    var old_cls_node = cls_ty.node.?;
    var cls = &old_cls_node.AstClass;
    // store the actual generic type arguments received by this class
    var _targs = TypeList.init(al);
    // inferred type arguments must match the generic type params if specified
    // ex: Fmt{str, num}('a', 5)
    if (node.targs) |targs| {
      if (init_mtd_node) |_nd| {
        var cls_params = _nd.AstFun.params;
        for (cls.tparams.?.items(), 0..) |tvar, tpos| {
          for (cls_params.items(), 0..) |param, ppos| {
            var typ = param.ident.typ.?;
            if (typ.typeidEql(tvar)) {
              if (!targs.itemAt(tpos).getType().?.typeidEql(args_inf.itemAt(ppos))) {
                return self.error_(
                  true, node.args.itemAt(ppos).getToken(),
                  "Type parameter mismatch. Expected type '{s}', but found '{s}'",
                  .{self.getTypename(targs.itemAt(tpos).getType().?), self.getTypename(args_inf.itemAt(ppos))}
                );
              }
            }
          }
        }
      }
      for (cls_ty.tparams.?.items(), targs.items()) |tvar, nd| {
        var t = nd.getType().?;
        self.insertType(tvar, t);
        _targs.append(t);
      }
    } else {
      var cls_params: ?ast.VarDeclList = if (init_mtd_node) |_nd| _nd.AstFun.params else null;
      for (cls.tparams.?.items()) |tvar| {
        var resolved = false;
        if (cls_params) |*params| {
          for (params.items(), 0..) |param, ppos| {
            var typ = param.ident.typ.?;
            if (typ.typeidEql(tvar)) {
              var t = args_inf.itemAt(ppos);
              self.insertType(tvar, t);
              _targs.append(t);
              resolved = true;
              break;
            }
          }
        }
        if (!resolved) {
          self.softError(
            tvar.variable().tokens.itemAt(0),
            "Could not resolve the generic type variable '{s}'. Consider explicitly specifying the type parameter",
            .{self.getTypename(tvar)}
          );
          return self.error_(true, node.expr.getToken(), "Called from here:", .{});
        }
      }
    }
    // link the class
    // lookup node using inferred args.
    // - if found just return the type found
    // - else, do the stuff below, and cache the node
    var synth_name = self.createSynthName(cls_ty.name, false, &_targs, null);
    if (self.findGenInfo(old_cls_node, synth_name)) |info| {
      node.expr.setType(info.typ);
      node.typ = info.typ.toInstance(al);
      return node.typ.?;
    }
    // TODO: do we need to not clone methods considering they'll never be generic?
    var new_cls_node = cls.clone(al);
    var newcls = &new_cls_node.AstClass;
    // keep tparams to ban use as alias in types contained in cls, as much as possible
    try self.linker.linkClass(newcls, true);
    // make class non-generic to allow building of the cfg
    newcls.tparams = null;
    self.ctx.varScope.pushScope();
    defer self.ctx.varScope.popScope();
    var new_cls_ty = try self.inferClassPartial(new_cls_node);
    new_cls_ty.klass().tparams = TypeList.init(al).boxEnsureCapacity(cls.tparams.?.capacity());
    for (cls.tparams.?.items()) |tvar| {
      new_cls_ty.klass().appendTParam(self.findType(tvar.variable().tokens.itemAt(0).value).?);
    }
    if (new_cls_ty.klass().getMethodTy(ks.InitVar)) |fun_ty| {
      for (fun_ty.function().params.items(), 0..) |typ, ppos| {
        if (typ.isFunction() and node.args.itemAt(ppos).isFun()) {
          typ.function().node = node.args.itemAt(ppos);
        }
      }
    }
    var typ = try self.inferClass(new_cls_node, new_cls_ty);
    if (typ.klass().getMethodTy(ks.InitVar)) |mth| {
      try self.validateClsCallArguments(
        node, mth, (init_mtd_node orelse new_cls_ty.klass().node.?).getToken()
      );
    }
    typ = typ.toInstance(al);
    node.typ = typ;
    node.expr.setType(new_cls_ty);
    self.addGenInfo(
      old_cls_node,
      new_cls_node,
      self.boxSynthName(old_cls_node.getToken(), synth_name),
      new_cls_ty
    );
    return typ;
  }

  fn validateTagArgCount(self: *Self, tag: *types.Tag, node: *ast.CallNode) !void {
    if (tag.paramsLen() != node.args.len()) {
      return self.error_(
        true, node.expr.getToken(),
        "Argument mismatch. Expected {} argument(s) but found {}", .{tag.paramsLen(), node.args.len()}
      );
    }
  }

  fn validateLabeledTagArguments(self: *Self, tag_ty: *types.Tag, node: *ast.CallNode) !void {
    if (node.labeled) {
      // var tag = &fun_ty.node.?.AstFun;
      var map = std.StringHashMap([]const u8).init(self.ctx.allocator());
      defer map.clearAndFree();
      // find duplicate labels
      for (node.args.items()) |arg| {
        if (arg.isLblArg()) {
          var str = arg.AstLblArg.label.value;
          if (map.get(str)) |_| {
            self.softError(arg.AstLblArg.label, "duplicate labeled argument found", .{});
          } else {
            map.put(str, str) catch {};
          }
        }
      }
      // simply sort the labeled arguments
      for (tag_ty.paramSlice(), 0..) |param, i| {
        for (node.args.items(), 0..) |arg, j| {
          if (arg.isLblArg()) {
            const lbl = arg.AstLblArg.label;
            if (param.name) |name| {
              if (name.valueEql(lbl.value)) {
                node.args.items()[j] = node.args.items()[i];
                node.args.items()[i] = arg.AstLblArg.value;
              }
            }
          }
        }
      }
      if (self.diag.hasErrors()) return error.CheckError;
      // report errors for unsorted/untransformed labeled args if any
      for (node.args.items()) |arg| {
        if (arg.isLblArg()) {
          return self.error_(
            true, arg.AstLblArg.label, "illegal or invalid label: '{s}'",
            .{arg.AstLblArg.label.value}
          );
        }
      }
    }
  }

  fn validateTagArguments(self: *Self, node: *ast.CallNode, ty: *Type, token: Token) !*Type {
    var tag = ty.tag();
    try self.validateLabeledTagArguments(tag, node);
    const al = self.ctx.allocator();
    var args_inf = ds.ArrayList(types.Tag.TagParam).init(al);
    if (tag.params) |params| {
      for (node.args.items(), 0..) |arg, i| {
        args_inf.append(.{.name = params.itemAt(i).name, .typ = try self.infer(arg)});
      }
    } else {
      for (node.args.items()) |arg| {
        args_inf.append(.{.name = null, .typ = try self.infer(arg)});
      }
    }
    var fnd = Type.newTagWithParams(tag.name, tag.ty, args_inf.items(), al);
    const generic = ty.hasVariable();
    const exp = if (generic) try self.linker.resolve(fnd, token) else fnd;
    if (!exp.isRelatedTo(fnd, .RCAny, al)) {
      self.softError(
        node.expr.getToken(),
        "Expected type '{s}' but got '{s}'",
        .{self.getTypename(ty), self.getTypename(fnd)}
      );
    }
    node.typ = exp;
    return exp;
  }

  fn inferTagCall(self: *Self, node: *ast.CallNode, ty: *Type) !*Type {
    var tag = ty.tag();
    const token = node.expr.getToken();
    try self.validateTagArgCount(tag, node);
    return try self.validateTagArguments(node, ty, token);
  }

  fn inferRet(self: *Self, node: *ast.RetNode) !*Type {
    if (node.expr) |expr| {
      node.typ = try self.infer(expr);
    } else {
      node.typ = self.void_ty;
    }
    return node.typ.?;
  }

  fn inferError(self: *Self, node: *ast.ErrorNode) !*Type {
    if (node.typ) |typ| {
      return typ;
    }
    var ty = try self.infer(node.expr);
    if (ty.isErrorTy()) {
      return self.error_(
        true, node.expr.getToken(),
        "Nested error types are unsupported: '{s}'", .{self.getTypename(ty)}
      );
    }
    const base = Type.newTagWithParamTypes(ks.ErrorVar, .TkError, &[_]*Type{ty}, self.ctx.allocator());
    node.typ = base;
    return base;
  }

  fn inferOrElse(self: *Self, node: *Node) !*Type {
    // - build cfg of this node. 
    var exit = if (self.current_fn) |curr| curr.exit else self.cfg.program.exit;
    var builder = CFGBuilder.initWithExit(self.ctx.allocator(), exit);
    var flo = builder.buildOrElse(&self.cfg, node);
    var ok_ty = try self.infer(node.AstOrElse.ok);
    return try self.checkOrElse(node, ok_ty, flo);
  }

  fn inferProgram(self: *Self, node: *ast.ProgramNode) !*Type {
    self.ctx.enterScope();
    for (node.decls.items()) |item| {
      _ = self.infer(item) catch undefined;
    }
    self.ctx.leaveScope();
    // crash and burn
    return undefined;
  }

  fn checkCast(self: *Self, node_ty: *Type, cast_ty: *Type, ctx: RelationContext, debug: Token, emit: bool) TypeCheckError!*Type {
    var ty = node_ty.canBeCastTo(cast_ty, ctx, self.ctx.allocator()) catch |e| {
      if (e == error.UnionCastError) {
        const active = if (node_ty.isTaggedUnion()) self.getTypename(node_ty.taggedUnion().activeTy().?) else "different";
        return self.error_(
          emit, debug,
          "Cannot cast from type '{s}' to type '{s}' because the active type is '{s}'",
          .{self.getTypename(node_ty), self.getTypename(cast_ty), active}
        );
      }
      return self.error_(
        emit, debug,
        "Cannot cast from type '{s}' to type '{s}'",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    };
    if (ty == node_ty) {
      self.warn(
        emit, debug,
        "Could not cast from type '{s}' to type '{s}' because the active type is unknown",
        .{self.getTypename(node_ty), self.getTypename(cast_ty)}
      );
    }
    return ty;
  }

  fn checkAssign(self: *Self, target: *Type, source: *Type, debug: Token, emit: bool) !*Type {
    var typ = target.canBeAssigned(source, .RCAny, self.ctx.allocator());
    if (typ == null) {
      return self.error_(
        emit, debug,
        "Cannot assign type '{s}' to type '{s}'",
        .{self.getTypename(source), self.getTypename(target)}
      );
    }
    return typ.?;
  }

  fn checkInitAssign(self: *Self, target: *Type, source: *Type, debug: Token, emit: bool) !*Type {
    var typ = target.canBeAssigned(source, .RCConst, self.ctx.allocator());
    if (typ == null) {
      return self.error_(
        emit, debug,
        "Cannot initialize type '{s}' with type '{s}'",
        .{self.getTypename(target), self.getTypename(source)}
      );
    }
    return typ.?;
  }

  fn checkNil(self: *Self, node: *ast.LiteralNode, typ: *Type) !void {
    _ = typ;
    return self.error_(true, node.token, "Should not be checking nil",  .{});
  }

  fn checkUnary(self: *Self, node: *ast.UnaryNode, expected: *Type) !void {
    if (node.typ.?.typeid() != expected.typeid()) {
      const op = node.op.token.value;
      return self.error_(
        true, node.op.token,
        "Expected type {s} '{s}', but got {s} '{s}'",
        .{op, self.getTypename(expected), op, self.getTypename(node.typ.?)}
      );
    }
  }

  fn checkBinary(self: *Self, node: *ast.BinaryNode, source: *Type, narrowed: bool) !void {
    // source is type of rhs
    // node.typ is type of lhs
    if (node.op.optype == .OpEqq or node.op.optype == .OpNeq or node.op.optype == .OpIs) {
      var ctx: RelationContext = if (node.op.optype == .OpIs) .RCIs else .RCAny;
      if (!node.typ.?.isEitherWayRelatedTo(source, ctx, self.ctx.allocator())) {
        return self.error_(
          true, node.op.token,
          "Types must be related for comparison.{s}'{s}' is not related to '{s}'",
          .{
            if (!narrowed) " " else " Narrowed type ",
            self.getTypename(node.typ.?), self.getTypename(source),
          }
        );
      }
      return;
    }
    if (node.op.optype == .OpAnd or node.op.optype == .OpOr) {
      if (!node.typ.?.isEitherWayRelatedTo(source, .RCAny, self.ctx.allocator())) {
        return self.error_(
          true, node.op.token,
          "Types on lhs and rhs of binary operator '{s}' must be related for comparison."
          ++ "{s}'{s}' is not related to '{s}'",
          .{
            node.op.token.ty.str(),
            if (!narrowed) " " else " Narrowed type ",
            self.getTypename(node.typ.?), self.getTypename(source),
          }
        );
      }
      return;
    }
    var errTy: ?*Type = null;
    if (!node.typ.?.typeidEql(&UnitTypes.tyNumber)) {
      errTy = node.typ;
    } else if (!source.typeidEql(&UnitTypes.tyNumber)) {
      errTy = source;
    }
    if (errTy != null) {
      const name = self.getTypename(&UnitTypes.tyNumber);
      const op = node.op.token.value;
      return self.error_(
        true, node.op.token,
        "Expected type '{s}' {s} '{s}', but got '{s}' {s} '{s}'",
        .{
          name, op, name,
          self.getTypename(node.typ.?), op, self.getTypename(source)
        }
      );
    }
  }

  fn checkSubscript(self: *Self, node: *ast.SubscriptNode, expr_ty: *Type, index_ty: *Type) !void {
    var token = node.index.getToken();
    if (!expr_ty.isListTy() and !expr_ty.isMapTy() and !expr_ty.isTupleTy()) {
      return self.error_(
        true, token,
        "Type '{s}' is not indexable", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.klass().tparamsLen() == 0) {
      return self.error_(
        true, token,
        "Cannot index empty or non-specialized '{s}' type", .{self.getTypename(expr_ty)}
      );
    }
    if (expr_ty.isListTy() or expr_ty.isTupleTy()) {
      if (!index_ty.isNumTy()) {
        return self.error_(
          true, token,
          "Cannot index '{s}' type with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      }
      if (expr_ty.isListTy()) {
        node.typ = expr_ty.klass().tparams.?.itemAt(0);
      } else if (!node.index.isNumberLiteral()) {
        return self.error_(
          true, token,
          "tuple index must be a compile-time number literal", .{}
        );
      } else {
        const index = node.index.toIntNumber(usize);
        if (index >= expr_ty.klass().tparamsLen()) {
          return self.error_(true, token, "tuple index out of range", .{});
        }
        node.typ = expr_ty.klass().tparams.?.itemAt(index);
      }
    } else if (expr_ty.isMapTy()) {
      // k-v. index with k, get v.
      var cls = expr_ty.klass();
      var key_typ = cls.tparams.?.itemAt(0);
      var val_typ = cls.tparams.?.itemAt(1);
      _ = self.checkAssign(key_typ, index_ty, node.index.getToken(), false) catch {
        return self.error_(
          true, token,
          "Cannot index type '{s}' with type '{s}'",
          .{self.getTypename(expr_ty), self.getTypename(index_ty)}
        );
      };
      node.typ = val_typ;
    }
  }

  fn checkDeref(self: *Self, node: *ast.DerefNode, expr_ty: *Type) !void {
    if (!expr_ty.isTaggedNullable()) {
      return self.error_(
        true, node.token,
        "Cannot dereference non-nullable type: '{s}'",
        .{self.getTypename(expr_ty)}
      );
    }
    node.typ = expr_ty.subtype(self.ctx.allocator());
  }

  fn checkDotAccess(self: *Self, node: *ast.DotAccessNode, ty: *Type, prop: *ast.VarNode) !void {
    var cls = if (ty.isClass()) ty.klass() else ty.instance().cls.klass();
    if (cls.getField(prop.token.value)) |field| {
      node.typ = field.AstVarDecl.ident.typ;
    } else if (cls.getMethodTy(prop.token.value)) |mth_ty| {
      node.typ = mth_ty; //Type.newMethod(mth_ty, cls_ty).box(self.ctx.allocator());
    } else {
      return self.error_(
        true, prop.token, "{s} has no property '{s}'",
        .{self.getTypename(ty), prop.token.value}
      );
    }
  }

  fn checkTagDotAccess(self: *Self, node: *ast.DotAccessNode, ty: *Type, idx: usize) !*Type {
    var tag = ty.tag();
    if (tag.getParam(idx)) |prm| {
      node.typ = prm.typ;
      return prm.typ;
    } else {
      const token = node.rhs.getToken();
      const msg = (
        if (token.valueEql(ks.GeneratedTypeVar))
          "invalid property access"
        else 
          "invalid dereference"
      );
      return self.error_(true, node.rhs.getToken(), "{s}", .{msg});
    }
  }

  fn checkCondition(self: *Self, cond_ty: *Type, debug: Token) !void {
    if (!cond_ty.isBoolTy() and !(cond_ty.isUnion() and cond_ty.isBoolUnionTy())) {
      return self.error_(
        true, debug,
        "Expected condition expression to be of type 'bool', but got '{s}'",
        .{self.getTypename(cond_ty)}
      );
    }
  }

  fn excludeError(self: *Self, typ: *Type, debug: Token) !*Type {
    var errors: usize = 0;
    var uni = types.TaggedUnion.init(self.ctx.allocator());
    for (typ.taggedUnion().variants.items()) |ty| {
      if (ty.isErrorTy()) {
        errors += 1;
      } else {
        uni.append(ty);
      }
    }
    if (errors > 1) {
      return self.error_(
        true, debug, "Error unions with multiple error types are unsupported: '{s}'",
        .{self.getTypename(typ)}
      );
    }
    return uni.toType().box(self.ctx.allocator());
  }

  fn checkOrElse(self: *Self, node: *Node, ok_ty: *Type, flo: FlowMeta) !*Type {
    // - check that try is used with an error union type
    // - check that the type on `ok` and `err` are related
    var oe = &node.AstOrElse;
    var debug = oe.ok.getToken();
    if (!ok_ty.isErrorTaggedUnion()) {
      var help = (
        if (ok_ty.isTaggedNullable())
          "\n\tHelp: nullable types take precedence over error types"
        else ""
      ); 
      return self.error_(
        true, debug,
        "Expected error union type in 'try/orelse' expression. Type '{s}' is not an error union{s}",
        .{self.getTypename(ok_ty), help}
      );
    }
    var typ = try self.excludeError(ok_ty, debug);
    // if excludeError() doesn't err, it's safe to say this type has only one error type
    var last = self.ctx.varScope.pushScopeSafe();
    defer self.ctx.varScope.popScopeSafe(last);
    if (oe.evar) |evar| {
      for (ok_ty.taggedUnion().variants.items()) |ty| {
        if (ty.isErrorTy()) {
          evar.typ = ty;
          self.insertVar(evar.token.value, ty);
          break;
        }
      }
    }
    try self.flowInferEntry(flo.entry);
    var err_ty = if (oe.err.isBlock()) self.void_ty else oe.err.getType().?;
    if (oe.from_try and oe.err.isRet()) {
      // TODO
    } else if (!typ.isRelatedTo(err_ty, .RCAny, self.ctx.allocator())) {
      if (!err_ty.isNoreturnTy()) {
        return self.error_(
          true, debug,
          "Type on both sides of 'orelse' must be related.\n\t"
          ++ "'{s}' is not related to '{s}'",
          .{self.getTypename(typ), self.getTypename(err_ty)}
        );
      }
    }
    oe.typ = typ;
    return typ;
  }

  fn infer(self: *Self, node: *Node) TypeCheckError!*Type {
    return switch (node.*) {
      .AstNumber => |*nd| try self.inferNumber(nd),
      .AstString => |*nd| try self.inferString(nd),
      .AstBool => |*nd| try self.inferBool(nd),
      .AstUnary => |*nd| try self.inferUnary(nd),
      .AstBinary => |*nd| try self.inferBinary(nd),
      .AstList => |*nd| try self.inferList(nd),
      .AstTuple => |*nd| try self.inferTuple(nd),
      .AstMap => |*nd| try self.inferMap(nd),
      .AstExprStmt => |*nd| try self.inferExprStmt(nd),
      .AstVar => |*nd| try self.inferVar(nd, true),
      .AstVarDecl => |*nd| try self.inferVarDecl(nd),
      .AstAssign => |*nd| try self.inferAssign(nd),
      .AstBlock => |*nd| try self.inferBlock(nd),
      .AstNType => |*nd| try self.inferNType(nd),
      .AstAlias => |*nd| try self.inferAlias(nd),
      .AstNil => |*nd| try self.inferNil(nd),
      .AstCast => |*nd| try self.inferCast(nd),
      .AstSubscript => |*nd| try self.inferSubscript(nd),
      .AstDeref => |*nd| try self.inferDeref(nd),
      .AstWhile => |*nd| try self.inferWhile(nd),
      .AstCall => |*nd| try self.inferCall(nd, node),
      .AstRet => |*nd| try self.inferRet(nd),
      .AstFun => try self.inferFun(node, null),
      .AstError => |*nd| try self.inferError(nd),
      .AstOrElse => try self.inferOrElse(node),
      .AstClass => try self.inferClass(node, null),
      .AstDotAccess => |*nd| try self.inferDotAccess(nd, node),
      .AstScope => |*nd| try self.inferScope(nd),
      .AstFailMarker => |*nd| try self.pc.inferFail(nd),
      .AstRedundantMarker => |*nd| try self.pc.inferRedundant(nd),
      .AstProgram => |*nd| try self.inferProgram(nd),
      .AstIf, .AstElif, .AstSimpleIf, .AstLblArg,
      .AstCondition, .AstMCondition, .AstEmpty, .AstControl => return undefined,
     .AstMatch,  .AstLiftMarker => unreachable,
    };
  }

  fn buildFunFlow(self: *Self, node: *Node) !void {
    self.builder.buildFun(&self.cfg, node);
    var flo = self.cfg.lookupFunc(node).?;
    try self.analyzer.analyzeDeadCode(flo.dead);
  }

  inline fn buildClsFlow(self: *Self, node: *Node) !void {
    self.builder.buildCls(&self.cfg, node);
  }

  fn buildProgramFlow(self: *Self, root: *Node, display_diag: bool) !void {
    self.cfg = self.builder.build(root);
    self.analyzer.analyzeDeadCode(self.cfg.program.dead) catch |e| {
      if (display_diag) self.diag.display();
      return e;
    };
  }

  pub fn typecheck(self: *Self, node: *Node, va: *VebAllocator, display_diag: bool) TypeCheckError!void {
    self.pc = PatternChecker.init(self);
    self.ctx.enterScope();
    self.linker.ctx = &self.ctx;
    self.linker.u8w = &self.u8w;
    self.loadBuiltinsPrelude(va);
    try self.buildProgramFlow(node, display_diag);
    self.flowInferEntry(self.cfg.program.entry) catch {};
    self.analyzer.analyzeDeadCodeWithTypes(self.cfg.program.entry) catch {};
    if (self.diag.hasAny()) {
      var has_error = self.diag.hasErrors();
      if (display_diag) self.diag.display();
      if (has_error) return error.CheckError;
    }
  }
};
