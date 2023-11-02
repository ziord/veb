const lib = @import("lib.zig");
const doErrorTest = lib.doErrorTest;

test "binary operators" {
  var src =
  \\ 'fox' + 5
  \\ false - 'foobar'
  \\ [] * ()
  \\ nil / {'x': 5}
  \\ ~false / {'x': 5}
  \\ nil ^ true
  \\ 'fin' > true
  \\ [('1', 2, 'a')] < ('oops')!
  \\ ([1, 2])! <= ('oops')!
  \\ false >= nil
  \\ [('1', 2, 'a')] == ('oops')!
  \\ 'mist' != 6
  \\ let q = err{num} + str
  ;
  try doErrorTest(src, 13, [_][]const u8{
    "Expected type 'num' + 'num', but got 'str' + 'num'",
    "Expected type 'num' - 'num', but got 'bool' - 'str'",
    "Expected type 'num' * 'num', but got 'list{any}' * 'tuple{any}'",
    "Expected type 'num' / 'num', but got 'nil' / 'map{str, num}'",
    "Expected type ~ 'num', but got ~ 'bool'",
    "Expected type 'num' ^ 'num', but got 'nil' ^ 'bool'",
    "Expected type 'num' > 'num', but got 'str' > 'bool'",
    "Expected type 'num' < 'num', but got 'list{tuple{str | num}}' < 'err{str}'",
    "Expected type 'num' <= 'num', but got 'err{list{num}}' <= 'err{str}'",
    "Expected type 'num' >= 'num', but got 'bool' >= 'nil'",
    "Types must be related for comparison. 'list{tuple{str | num}}' is not related to 'err{str}'",
    "Types must be related for comparison. 'str' is not related to 'num'",
    "Expected type 'num' + 'num', but got 'Type' + 'Type'",
  });
}

test "builtin properties" {
  var src =
  \\ let j = (1, 2)
  \\ j[0] += 5
  \\ let p = {'b': 5 as num | str}
  \\ p[5]
  \\ let p = []
  \\ p[1]
  \\ p = [1, 2]
  \\ p["fox"]
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Cannot modify immutable type 'tuple{num}'",
    "Cannot index type 'map{str, num | str}' with type 'num'",
    "Cannot index 'list{any}' type with type 'str'",
  });
}

test "builtin generics" {
  var src =
  \\ let j: list{} = []
  \\ let k: map{str} = {1: 1}
  \\ let i: tuple{num, bool} = (false,)
  \\ let x: err{num, list{num}} = (56)!
  \\ type T{K} = list{K{T}}
  \\ type X{K} = num | str | K{bool}
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "empty type parameters are not supported",
    "generic type instantiated with wrong number of paramters. Expected 2 but got 1",
    "generic type instantiated with wrong number of paramters. Expected 1 but got 2",
    "generic type instantiated with wrong number of paramters. Expected 1 but got 2",
    "type variable in generic parameter cannot be generic",
    "type variable in generic parameter cannot be generic",
  });
}

test "casting" {
  var src =
  \\ let j = (1, 2)
  \\ j as tuple{str | num}
  \\ let j = {'a': 5} as map{str, num | bool}
  \\ let t = []
  \\ t as list{str | num}
  \\ let j: any = 5
  \\ (j as num) + 6
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Cannot cast from type 'tuple{num}' to type 'tuple{str | num}'",
    "Cannot cast from type 'map{str, num}' to type 'map{str, num | bool}'",
    "Cannot cast from type 'list{any}' to type 'list{str | num}'",
    "Cannot cast from type 'any' to type 'num'",
  });
}

test "casting.<active types>" {
  var src =
  \\ let j: num | str = 5
  \\ let p = j as str
  \\ print(p)
  \\ let j: list{tuple{num}} | list{list{str}} = [['a'], ['b']]
  \\ let p = j as list{tuple{num}}
  \\ print(p)
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Cannot cast from type 'num | str' to type 'str' because the active type is 'num'",
    "Cannot cast from type 'list{tuple{num}} | list{list{str}}' to type 'list{tuple{num}}' because the active type is 'list{list{str}}'"
  });
}

test "noreturn" {
  var src =
  \\ def foo(): noreturn
  \\  print("oops")
  \\ end
  \\
  \\ foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit; function declared type 'noreturn' returns",
  });
}

test "never & noreturn" {
  var src =
  \\ def foo()
  \\  foo()
  \\ end
  \\
  \\ let j: noreturn = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'noreturn' with type 'never'",
  });
}

test "never & void .1" {
  var src =
  \\ def foo(): never
  \\  foo()
  \\ end
  \\ let j: void = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'void' with type 'never'",
  });
}

test "never & void .2" {
  var src =
  \\ def foo(): never
  \\  3
  \\ end
  \\ let j: void = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected return type 'never', but got 'void'",
  });
}

test "type linking" {
  var src =
  \\ type HashMap{K, V} = map{K, V}
  \\ type Q = Q | Q
  \\ type StringHashMap{V} = HashMap{str, J}
  \\ type NumList = list{X}
  \\ let a: NumList = [1, 2]
  \\ let b: HashMap{num, bool} = {0: false}
  \\ let c: StringHashMap{bool} = {'foo': false}
  \\ let x: str = 'over the garden wall!'
  \\ let y: Q = 'oops' as Pinky
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "could not resolve type with name: 'X'",
    "could not resolve type with name: 'J'",
    "could not resolve type with name: 'Pinky'",
  });

  var src2 =
  \\ type T{K} = str | S{str}
  \\ type S{K} = num | T{num}
  \\ type T = T
  \\ type S = S
  \\ type Q = T | S | any
  \\ let j: Q = 5
  \\ j += j
  \\ let t: list{(num? | str?)?} = [nil, 5 as num | str, nil]
  \\ t * t
  \\ t.?
  ;
  try doErrorTest(src2, 3, [_][]const u8{
    "Expected type 'num' + 'num', but got 'Q' + 'Q'",
    "Expected type 'num' * 'num', but got 'list{num | str | nil}' * 'list{num | str | nil}'",
    "Cannot dereference non-nullable type: 'list{num | str | nil}'",
  });
}

test "conditionals" {
  var src =
  \\ if 3 then
  \\ end
  \\ while [{},] do
  \\ end
  \\ str is num
  \\ 5 is false
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected condition expression to be of type 'bool', but got 'num'",
    "Expected condition expression to be of type 'bool', but got 'list{map{any, any}}'",
    "Expected type instance in lhs of `is` operator but found 'Type'",
    "Expected type 'Type' in rhs of `is` operator but found type 'false'\n\tHelp: For constant types, consider using '==' or '!=' operator instead.",
  });
}

test "narrowing-1" {
  var src =
  \\ let x: (str | num)? = 'foobar'
  \\ let p = 10
  \\ if x is not nil and !!!(x is num) and x > p
  \\ end
  \\ let x: list{num} | map{str, num} = [5]
  \\ let p = 10
  \\ if x is list{num}
  \\   x += 5
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'num' > 'num', but got 'str' > 'num'",
    "Expected type 'num' + 'num', but got 'list{num}' + 'num'",
  });
}

test "narrowing-2" {
  var src =
  \\ let x: list{num} | map{str, num} = [5]
  \\ if x is list{num}
  \\ elif x is map{str, num}
  \\ else
  \\  ~x 
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type ~ 'num', but got ~ 'never'",
  });
}

test "narrowing-3" {
  var src =
  \\ let x: num? = 9
  \\ if x is not nil and x is num
  \\   x + 5
  \\ else 
  \\   0+x
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'num' + 'nil'",
  });
}

test "narrowing-4" {
  var src =
  \\ let x: num? = 9
  \\ if x is not nil and x is num
  \\   x + 5
  \\ elif x is not num
  \\   0+x
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Types must be related for comparison. Narrowed type 'nil' is not related to 'num'",
    "elif x is not num",
  });
}

test "narrowing-5" {
  var src =
  \\ let x: list{num | list{num}} | num = [9 as num | list{num}]
  \\ let p = 0
  \\ if x is list{num | list{num}}
  \\    if x[0] is list{num}
  \\        p /= 5
  \\    end
  \\ elif x is num
  \\    p += x
  \\ else
  \\    x[0] # never
  \\ end
  \\ p
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Type 'never' is not indexable",
  });
}

test "narrowing-6" {
  var src =
  \\ let x: (list{num | str} | str)? = [5, 'a']
  \\ if x.? is list{num | str} and x.?[0] is num
  \\    x.?[1] += 5
  \\ else
  \\    x.?
  \\ end
  \\ x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'num | str' + 'num'",
  });
}

test "narrowing-7" {
  var src =
  \\ let x: str | num = 5
  \\ let y: str | num = 10
  \\ if x is num and y is num or y is num
  \\    x # num | str
  \\    y *= 5
  \\ else
  \\    x + y
  \\ end
  \\ y
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'str | num' + 'str'",
  });
}

test "narrowing-8" {
  var src =
  \\ let x: list{list{num | str}} | map{str, list{num | str}} = [[5 as num | str]]
  \\ let p = 10
  \\ if x is map{str, list{num | str}}
  \\   if x['a'][0] is num and x['a'][0] + 2 > 0xff
  \\      x['a'][0] + 5
  \\   else
  \\     x # list{...}
  \\   end
  \\ else
  \\   if x[0][0] is num
  \\      p -= x[0][1]
  \\   end
  \\ end
  \\ p
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'num' - 'num', but got 'num' - 'num | str'",
    "p -= x[0][1]",
  });
}

test "narrowing-9" {
  var src =
  \\ let x: (num | str?) = 'fox'
  \\ if x is not str
  \\    x += 10
  \\ end
  \\ x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'num | nil' + 'num'",
  });
}

test "narrowing-10" {
  var src =
  \\ def fun(n: num | str)
  \\  if n is str
  \\    return n
  \\  end
  \\  if n is num
  \\    return n + 5
  \\  end
  \\  n / n
  \\ end
  \\ fun('fancy')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' / 'num', but got 'never' / 'never'",
  });
}

test "narrowing-11" {
  var src =
  \\ def fish(p: "a" | "b" | 5)
  \\  if p == "a"
  \\    return 'nice'
  \\  elif p == "b"
  \\    return 'good'
  \\  elif p == 5
  \\    return 'okay'
  \\  else # never
  \\    p += 5 # never
  \\    return 'hmm'
  \\  end
  \\ end
  \\
  \\ let x = fish(5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'never' + 'num'",
  });
}

test "narrowing-12" {
    var src =
    \\ class Fox
    \\    x: num | str = 5
    \\    u = 12
    \\ end
    \\ class Foo
    \\    x = 'ok'
    \\    u = 13
    \\ end
    \\
    \\ let f: Fox | Foo = Foo()
    \\ if f is Foo
    \\  assert(f.u == 13, 'this should be Foo.u')
    \\ elif f is Fox
    \\  assert(f.u == 12, 'this should be Fox.u')
    \\ else
    \\  f += 5 # never
    \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'never' + 'num'",
  });
}

test "dca-0" {
  var src =
  \\ def foo()
  \\  return 5
  \\  type X = 7
  \\ end
  \\
  \\ foo()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "type X = 7",
  });
}

test "dca-1" {
  var src =
  \\ def fun(n: num | str)
  \\  if n is str
  \\    return n
  \\  else 
  \\    return n + 5
  \\  end
  \\  n + 5
  \\ end
  \\ fun(12)
  \\
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "n + 5",
  });
}

test "dca-2" {
  var src =
  \\ def bin
  \\  if 1 > 2
  \\    return 5
  \\  else
  \\    return 6
  \\  end
  \\  return 0
  \\ end
  \\ bin()
  \\
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "return 0",
  });
}

test "dca-3" {
  var src =
  \\ def bin
  \\  if 1 > 2
  \\    return 5
  \\  else
  \\    return 6
  \\  end
  \\  return 0
  \\ end
  \\ exit(5)
  \\ bin()
  \\
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "bin()",
  });
}

test "dca-4" {
  var src =
  \\ def bin
  \\  if 1 > 2
  \\    exit(5)
  \\  else
  \\    panic('oops')
  \\  end
  \\  return 0
  \\ end
  \\ bin()
  \\
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "return 0",
  });
}

test "dca-5" {
  var src =
  \\ while true do
  \\  continue
  \\  let j = 5
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "let j = 5",
  });
}

test "dca-6" {
  var src =
  \\ while true do
  \\  continue
  \\  let j = 5
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "let j = 5",
  });
}

test "dca-7" {
  var src =
  \\ while true do
  \\  break
  \\  let j = 5
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "let j = 5",
  });
}

test "dca-8" {
  var src =
  \\ def bin
  \\  let j = 12
  \\  return 0
  \\  return j
  \\ end
  \\ bin()
  \\
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "return j",
  });
}

test "constant types" {
  var src =
  \\ let x: 5 = 5
  \\ let y: 7 = 7
  \\ y * x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' * 'num', but got '7' * '5'",
  });
}

test "functions-1" {
  var src =
  \\ type T = num
  \\ def j(a: T): T
  \\  return (a * 2)
  \\ end
  \\ j('eeeek')
  \\ j{T}(5)
  \\ def funny
  \\    def foo{T}(a: T): T
  \\     return a
  \\    end
  \\    let j = foo{str}('5')
  \\    let k = foo(10)
  \\    let p = foo(56)
  \\    k += 5
  \\    return (j, k, p)
  \\ end
  \\ funny(12)
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Argument mismatch. Expected type 'T' but found 'str'",
    "Non-generic function called as generic",
    "Argument mismatch. Expected 0 argument(s) but found 1",
  });
}

test "functions-2" {
  var src =
  \\ def foo(a: num)
  \\  return foo(a)
  \\ end
  \\
  \\ foo(5) - foo(12)
  \\
  \\ def fancy{T}(x: T)
  \\  let j: T = x
  \\  return fancy(x)
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' - 'num', but got 'never' - 'never'",
  });
}

test "functions-3" {
  var src =
  \\ def fancy{T}(x: T)
  \\  let j: T = x
  \\  return fancy{T}(j)
  \\ end
  \\ fancy(false)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Non-generic function called as generic",
  });
}

test "functions-4" {
  var src =
  \\ def fancy{T}(x: T): fn{T}(num): str
  \\  let j: T = x
  \\  return fancy{T}(j)
  \\ end
  \\ fancy(false)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '(', but found '{'",
  });
}

test "functions-5" {
  var src =
  \\ def fancy{T}(x: T): str
  \\  let j: T = x
  \\  return def{T}(j: num) => j
  \\ end
  \\ fancy(false)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "generic lambdas are unsupported",
  });
}

test "functions-6" {
  var src =
  \\ def fun
  \\ end
  \\ def fancy(x: any)
  \\  return try fun()
  \\ end
  \\ fancy(false)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected error union type in 'try/orelse' expression. Type 'void' is not an error union",
  });
}

test "functions-7.<function arguments>" {
  var src =
  \\ def funny(t: list{str})
  \\  print(t)
  \\ end
  \\ funny([])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument mismatch. Expected type 'list{str}' but found 'list{any}'",
  });
}

test "error type" {
  var src =
  \\ let j = try fancy(5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "use of 'try' expression in top-level code. Consider using 'orelse' instead",
  });
  var src2 =
  \\ def fancy(x: num)
  \\  if x > 5
  \\    return ('bad')!
  \\  else 
  \\    return x * 4
  \\  end
  \\ end
  \\ let j = fancy(5)
  \\ j + 5
  ;
  try doErrorTest(src2, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'err{str} | num' + 'num'",
  });
}

test "simple-classes-1" {
  var src =
  \\ class Fox
  \\    x: num
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ 
  \\ let f = Fox()
  \\ f.y + f.y
  \\
  \\ class Foo
  \\  y: str
  \\ end
  \\
  \\ Foo()
  \\
  \\ class Bar
  \\  y: str
  \\  def init(t: str)
  \\    self.y = t
  \\  end
  \\ end
  \\
  \\ let q = Bar()
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Fox has no property 'y'",
    "a class having field(s) without defaults must define an `init` method that initializes such field(s)",
    "field 'y' is declared but uninitialized",
    "Argument mismatch. Expected 1 argument(s) but found 0",
  });
}

test "simple-classes-2" {
  var src =
  \\ class Fox
  \\    x: num
  \\    u = 12
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ class Racoon
  \\    x: num
  \\    u = 12
  \\    def init(): void
  \\      self.x = self.u
  \\      return
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ let f = Fox()
  \\ f.pulse = f.pulse
  \\ let r = Racoon()
  \\ r.x
  
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Cannot modify immutable type 'fn (): Fox'",
    "illegal return statement in `init` method"
  });
}

test "generic-classes-1" {
  var src =
  \\ let p = {'a': 5}
  \\ p.doesNotExist()
  \\ let q = [1, 2]
  \\ q.lens()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "map{str, num} has no property 'doesNotExist'",
    "list{num} has no property 'lens'",
  });
}

test "generic-classes-2" {
  var src =
  \\ class Fox{T}
  \\    x: tuple{T}
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\
  \\    def getGen()
  \\      def fun{T}(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{num}(6, 7, 8)
  \\ let t: Fox{num} = x
  \\ t.pulse().y
  \\ t.pulse().getGen()((7)!)
  \\ t.pulse().pulse().x.len()[0]
  \\ let q = {'a': 5, 'c': 12}
  \\ q.keys().len() + 5
  \\ q.items()[0].len() - '2'
  \\ x.pulse().getGen()(5)
  ;
    try doErrorTest(src, 5, [_][]const u8{
    "Fox{num} has no property 'y'",
    "Type 'err{num}' is not indexable",
    "Type 'num' is not indexable",
    "Expected type 'num' - 'num', but got 'num' - 'str'",
    "Type 'num' is not indexable"
  });
}

test "generic-classes-3" {
  var src =
  \\ class Fox{T}
  \\    x: tuple{T}
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\
  \\    def getGen()
  \\      def fun{T}(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let q = w.pulse
  \\ q().pulse().x += 5
  \\ j.pulse().x += 6 as 6
  \\ p().pulse().x + 5
  \\ w.pulse().x += 2
  \\ w.getGen()([12]) + 'g'
  \\ w.getGen()(['a', 'b']) + 'g'
  \\ let t = q().pulse()
  \\ t.init(12)
  \\ let s = t.pulse().getGen()
  \\ s(((0)!,))
  \\ 
  \\ type Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j: Poo{'miah'} = Fox{'mia'}('mia')
  ;
    try doErrorTest(src, 8, [_][]const u8{
    "Expected type 'num' + 'num', but got 'tuple{mia}' + 'num'",
    "Could not resolve type of ident: 'j'",
    "Could not resolve type of ident: 'p'",
    "Expected type 'num' + 'num', but got 'tuple{mia}' + 'num'",
    "Expected type 'num' + 'num', but got 'num' + 'str'",
    "Expected type 'num' + 'num', but got 'str' + 'str'",
    "Argument mismatch. Expected type 'mia' but found 'num'",
    "Cannot initialize type 'Poo{miah}' with type 'Fox{mia} instance'"
  });
}

test "loopy" {
  var src =
  \\ let x: num | str = 5
  \\ while x is num and x < 25 do
  \\  let j = 0
  \\  while j < x
  \\    j += '1'
  \\    continue
  \\  end
  \\  x += j
  \\  break
  \\ end
  \\ x + 5
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'num' + 'num', but got 'num' + 'str'",
    "Expected type 'num' + 'num', but got 'num | str' + 'num'",
  });
}

test "labeled argument" {
  var src =
  \\ def fun(x: str, y: num, a: list{num}, b: err{str})
  \\  print('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y: 'ops', a: 6, x: [6], ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument mismatch. Expected type 'str' but found 'list{num}'",
  });
}

test "labeled argument 2" {
  var src =
  \\ def fun(x: str, y: num, a: list{num}, b: err{str})
  \\  print('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y: 'ops', a: 6, x: 'ok', ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument mismatch. Expected type 'num' but found 'str'",
  });
}

test "labeled argument 3" {
  var src =
  \\ def fun(x: str, y: num, a: list{num}, b: err{str})
  \\  print('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y: 5, a: 6, x: 'ok', ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument mismatch. Expected type 'list{num}' but found 'num'"
  });
}

test "labeled argument 4" {
  var src =
  \\ def fun(x: str, y: num, a*: list{num})
  \\  print('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y: 5, a: [2, 3], y: 'oo', a: [1, 2], a: [5, 6, 7])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "duplicate labeled argument found",
  });
}

test "labeled argument 5" {
  var src =
  \\ def fun(x: str, y: num, a*: list{num})
  \\  print('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y: 5, a: [2, 3])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "missing required argument(s)",
  });
}

test "labeled argument 6" {
  var src =
  \\ def fun(x: str, y: num, a: list{num}, b: err{str})
  \\  print('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(6: 'a', 12: 9)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "invalid labeled argument",
  });
}

test "class init" {
  var src =
  \\ class Err{B}
  \\  val: B
  \\ end
  \\ Err('box')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Too many arguments to class call. Expected none, but got 1",
  });
}

test "patterns-1.<ordinary match>" {
  var src =
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a' as a, 'b' as b) as d => print('ok')
  \\  case ('q', 'k') => print('third')
  \\ end
  \\ 
  ;
 try doErrorTest(src, 2, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'str'",
    "inexhaustive pattern match.\n\tRemaining case type(s): 'tuple{str}'"
  });
}

test "patterns-2.<scopes>" {
  var src =
  \\ let o = 5
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a', 'b' as o) as d => o = 10
  \\  case ('q', 'k') => print('third')
  \\  case _ => print("last")
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot assign type 'num' to type 'str'",
  });
}

test "patterns-3.<nested match>" {
  var src =
  \\ let z = false
  \\ match (('a', 'b'), ('x', 'y'))
  \\
  \\  case (('x', 'y'), ..) => do
  \\    let p = z
  \\    print('first')
  \\  end
  \\  case (('a', ..), u) => do
  \\    let b = z
  \\    print('here!')
  \\  end
  \\  case (('a', t, ..) as d, ..) => do
  \\    let h = z
  \\    print('second')
  \\    z = true
  \\  end
  \\  case (('x', k), y) => do
  \\    let v = z
  \\    print('third')
  \\  end
  \\  case _ as n => let j = n
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case (('a', t, ..) as d, ..) => do",
  });
}

test "patterns-4.<match on unions>" {
  var src =
  \\ let j: 'a' | 'b' | 'c' = 'b'
  \\ match j
  \\  case 'a' => print('a')
  \\  case 'c' => print('hmm')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'b'"
  });
}

test "patterns-5.<match on classes (fields)>" {
  var src =
  \\ class Ant
  \\  a = 5
  \\ end
  \\ class Rat
  \\  x = 1
  \\  y = 8
  \\ end
  \\ let j = Rat() as Ant | Rat
  \\ match j
  \\  case Ant(a) if a > 2 => print(12)
  \\  case Ant(5) => print(13)
  \\  case Ant() => print(40)
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "'Ant' has 1 field(s), but pattern test assumes 0",
  });
}

test "patterns-6.<match on classes (fields)>" {
  var src =
  \\ class Ant
  \\  a = 5
  \\ end
  \\ class Rat
  \\  x = 1
  \\  y = 8
  \\ end
  \\ let j = Rat() as Ant | Rat
  \\ match j
  \\  case Ant(a) if a > 2 => print(12)
  \\  case Ant(5) => print(13)
  \\  case Rat(2) => print(40)
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "'Rat' has 2 field(s), but pattern test assumes 1",
  });
}

test "patterns-7.<tuple exhaustiveness>" {
  var src =
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a', 'b' as o) as d => print('second')
  \\  case ('q', 'k') => print('third')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'str'",
    "inexhaustive pattern match.\n\tRemaining case type(s): 'tuple{str}'"
  });
}

test "patterns-8.<list exhaustiveness>" {
  var src =
  \\ let z = false
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => print('first')
  \\  case [('a', 'b' as o)] as d => z = true
  \\  case [('q', 'k')] => print('third')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'str'",
    "inexhaustive pattern match.\n\tRemaining case type(s): 'tuple{str}'",
    "inexhaustive pattern match.\n\tRemaining case type(s): 'list{tuple{str}}'"
  });
}

test "patterns-9.<match redundancy>" {
  var src =
  \\ let z = false
  \\ class Cat
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case Dog() => print('good')
  \\  case Dog() => print('hmm')
  \\  case Cat() => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case Dog() => print('hmm')"
  });
}

test "patterns-10.<type checks>" {
  var src =
  \\ class Cat
  \\  x = 1
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case Dog() => print('hmm')
  \\  case Cat('fox') => print('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "illegal argument pattern type. Expected type 'num'"
  });
}

test "patterns-11.<type checks>" {
  var src =
  \\ class Cat
  \\  x = 1
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case [] => print('hmm')
  \\  case [Cat(5)] => print('nope')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "expected type 'Cat | Dog' but found 'list{<>}'",
    "expected type 'Cat | Dog' but found 'list{Cat | <>}'"
  });
}

test "patterns-12.<type checks>" {
  var src =
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case [] => print('hmm')
  \\  case [Cat(5)] => print('nope')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "expected type 'Cat | Dog' but found 'list{<>}'",
    "illegal argument pattern type. Expected type 'list{Dog instance}'"
  });
}

test "patterns-13.<type checks>" {
  var src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case Dog(x=5) => print('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Dog' has no field 'x'",
  });
}

test "patterns-14.<type checks (fields)>" {
  var src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case Dog(x=5, j=10) => print('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Dog' has 1 field(s), but pattern test assumes 2",
  });
}

test "patterns-14b.<type checks (fields)>" {
  var src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case Dog(y=5, x, ..) => print('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Dog' has 1 field(s), but pattern test assumes 2 or more",
  });
}

test "patterns-15.<match on bool (exhaustiveness)>" {
  var src =
  \\ match (1 < 2)
  \\  case false => print('nay')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'true'",
  });
}

test "patterns-16.<match on bool (exhaustiveness)>" {
  var src =
  \\ match (1 < 2)
  \\  case true => print('nay')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'false'",
  });
}

test "patterns-17.<ranges (exhaustiveness)>" {
  var src =
  \\  let n = 10 / 2
  \\  match n
  \\    case 0..2 => print('hey')
  \\    case 3..5 => print('hah')
  \\  end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'num'",
  });
}

test "patterns-18.<redundancy>" {
  var src =
  \\ match true
  \\  case false => print('nay')
  \\  case true => print('yay')
  \\  case _ => print('oops')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "redundant case",
    "case _ => print('oops')"
  });
}

test "patterns-19.<redundancy>" {
  var src =
  \\ type T = "a" | "b" | "c"
  \\ let j: T = "c"
  \\ match j
  \\   case "a" => print('a!')
  \\   case "b" => print('b!')
  \\   case "c" => print('c!')
  \\   case _ => print('other!')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "redundant case",
    "case _ => print('other!')"
  });
}

test "patterns-20.<redundancy>" {
  var src =
  \\ class Ant
  \\ end
  \\ class Rat
  \\ end
  \\ let j: Ant | Rat = Rat()
  \\ match j
  \\  case Ant() => 12
  \\  case Rat() => 15
  \\  case _ => 5
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "redundant case",
    "case _ => 5"
  });
}

test "patterns-21.<match on nil (exhaustiveness)>" {
  var src =
  \\ let j: ("a" | "b")? = "b"
  \\ match j
  \\   case "a" => print('a!')
  \\   case "b" => print('b!')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'nil'",
  });
}

test "patterns-22.<match on generics (exhaustiveness)>" {
  var src =
  \\ class Fox{T}
  \\  j: T
  \\  def init(j: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let z = false
  \\ let j: Fox{str} | Fox{num} = Fox{str}('pin')
  \\ match j
  \\  case Fox{str}('pin') as x => z = true
  \\  case Fox{num}(6) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'str'",
  });
}

test "patterns-23.<match on generics (exhaustiveness)>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant{T}
  \\ end
  \\ let j: Fox{str} | Fox{num} | Ant{str} = Ant{str}()
  \\ match j
  \\  case Fox{str}(..) as x => print('yes', x)
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'Ant{str}'"
  });
}

test "patterns-24.<match on generics (redundancy)>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let j: Fox{str} | Fox{num} = Fox{str}('pin', 'pan')
  \\ match j
  \\  case Fox{str}(..) as x => print('yes', x)
  \\  case Fox{str}(_) => print('caught ya str')
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case Fox{str}(_) => print('caught ya str')"
  });
}

test "patterns-25.<match on generics (redundancy)>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant{T}
  \\ end
  \\ let j: Fox{str} | Fox{num} | Ant{str} = Ant{str}()
  \\ match j
  \\  case Fox{str}(..) as x => print('yes', x)
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\  case Ant{str}() => print('no')
  \\  case _ => 'oops'
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "redundant case",
    "case _ => 'oops'"
  });
}

test "patterns-26.<redundancy>" {
  var src =
  \\ type Type = "a" | "b" | "c"
  \\ let j: Type = "a"
  \\ match j
  \\   case "a" as k => print(j, k)
  \\   case _ as t => match t
  \\      case "b" => print("B.1")
  \\      case "c" => print("C.1")
  \\      case _ => print('err.1')
  \\   end
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "redundant case",
    "case _ => print('err.1')"
  });
}

test "patterns-27.<redundancy>" {
  var src =
  \\ let j = true
  \\ match j
  \\   case false as k => print(j, k)
  \\   case _ as t => match t
  \\      case true => print("B.1")
  \\      case _ => print('err.2')
  \\   end
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "redundant case",
    "case _ => print('err.2')"
  });
}

test "patterns-28.<match on maps (redundancy)>" {
  var src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let foo = {"sound": Fox('fin.co') as Fox | str, "format": "txt"}
  \\ match foo
  \\  case {"sound": _, "format": _} => print(1)
  \\  case {"sound" as a: Fox(url) as b, "format": "ogg",} => print(12, url, a, b)
  \\  case {..} => print('default')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case {\"sound\" as a: Fox(url) as b, \"format\": \"ogg\",} => print(12, url, a, b)"
  });
}

test "patterns-29.<match on maps (exhaustiveness)>" {
  var src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let foo = {"sound": Fox('fin.co') as Fox | str, "format": "txt"}
  \\ match foo
  \\  case {"sound" as a: Fox(url) as b, "format": "ogg",} => print(12, url, a, b)
  \\  case {"sound": _, "format": _} => print(1)
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'str'",
    "inexhaustive pattern match.\n\tRemaining case type(s): 'list{str}'",
  });
}

test "patterns-30.<match on maps (redundancy)>" {
  var src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let foo = {"sound": Fox('fin.co') as Fox | str, "format": "txt"}
  \\ match foo
  \\  case {..} => print('default')
  \\  case {"sound" as a: Fox(url) as b, "format": "ogg",} => print(12, url, a, b)
  \\  case {"sound": _, "format": _} => print(1)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case {\"sound\" as a: Fox(url) as b, \"format\": \"ogg\",} => print(12, url, a, b)",
    "possible redundant case",
    "case {\"sound\": _, \"format\": _} => print(1)",
  });
}

test "patterns-31.<match in functions>" {
  var src =
  \\ def fib(n: num)
  \\  match n
  \\    case 0..1 => do
  \\      return n
  \\    end
  \\    case _ as t if t > 5 => do
  \\      return fib(n - 1) + fib(n - 2)
  \\    end
  \\  end
  \\ end
  \\ let j = fib(12)
  \\ j += 4
  \\ assert(j == 148, 'should be 148')
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'num'",
    "Expected type 'num' + 'num', but got 'void | num' + 'num'",
  });
}

test "patterns-32.<match in functions>" {
  var src =
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 11..50 as q => print('yep')
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let j = check(12)
  \\ j += 4
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'void | num' + 'num'",
  });
}

test "patterns-33.<match in functions>" {
  var src =
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 'a'
  \\   case 5..9 => return true
  \\    case 11..50 as q => print('yep')
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let j = check(12)
  \\ j += 4
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num', but got 'void | str | bool | num' + 'num'",
  });
}

test "patterns-34.<match in functions>" {
  var src =
  \\ def check(n: num): num
  \\  match n
  \\   case 1..4 => return 5
  \\   case 5..9 => print('jjj')
  \\    case 11..50 as q => return 5
  \\   case _ => return 3
  \\  end
  \\ end
  \\ check(13)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit from this point without returning type 'num'",
  });
}

test "patterns-35.<match in functions>" {
  var src =
  \\ def check(n: num): num
  \\  match n
  \\   case 1..4 => return 5
  \\   case 5..9 => return 'oops'
  \\    case 11..50 as q => return 5
  \\   case _ => return 3
  \\  end
  \\ end
  \\ check(13)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected return type 'num', but got 'str'",
  });
}

test "patterns-36.<inexhaustive rested patterns>" {
  var src =
  \\ let z = false
  \\ match ['a', false, 'b', true, 'c', false]
  \\  case ['x', _, 'c', _, ..] => print('has keys "x" and "c"')
  \\  case ['a', _, 'b', _ as p, ..] if z => print(z)
  \\  case [..] if !z => print('has something or none')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'list{str | bool}'",
  });
}

test "patterns-37.<inexhaustive rested patterns>" {
  var src =
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}
  \\  case {'x': _, 'c': _, ..} => print('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} if !z => print('has something or none')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'list{str}'",
  });
}

test "patterns-38.<inexhaustive patterns>" {
  var src =
  \\ def goodOrBad(n: num)
  \\   if n > 25
  \\    return ('oops')!
  \\   end
  \\   return n * 2
  \\ end
  \\
  \\ match goodOrBad(30)
  \\  case (error)! => print(error)
  \\  case 1..10 as x => print(x)
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'num'",
  });
}


test "patterns-39.<inexhaustive patterns>" {
  var src =
  \\ def goodOrBad(n: num)
  \\   if n > 25
  \\    return ('oops')!
  \\   end
  \\   return n * 2
  \\ end
  \\
  \\ match goodOrBad(30)
  \\  case ('ack' as j)! => print(j)
  \\  case 1..10 as x => print(x)
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "inexhaustive pattern match.\n\tRemaining case type(s): 'str'",
    "inexhaustive pattern match.\n\tRemaining case type(s): 'num'",
  });
}

test "parse-modes .1" {
  var src =
  \\ class str
  \\   x: num
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<identifier>', but found 'str'",
  });
}

test "parse-modes .2" {
  var src =
  \\ class list{X}
  \\   x: X
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<identifier>', but found 'list'",
  });
}
