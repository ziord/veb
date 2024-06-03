const lib = @import("lib.zig");
const doErrorTest = lib.doErrorTest;

test "binary operators" {
  const src =
  \\ 'fox' + 5
  \\ false - 'foobar'
  \\ [] * ()
  \\ None / {'x': 5}
  \\ ~false / {'x': 5}
  \\ None ^ true
  \\ 'fin' > true
  \\ [('1', 2, 'a')] < ('oops')!
  \\ ([1, 2])! <= ('oops')!
  \\ false >= None
  \\ [('1', 2, 'a')] == ('oops')!
  \\ 'mist' != 6
  \\ let q = List{num} + str
  ;
  try doErrorTest(src, 13, [_][]const u8{
    "Expected type 'num' + 'num' but found 'str' + 'num'",
    "Expected type 'num' - 'num' but found 'bool' - 'str'",
    "Expected type 'num' * 'num' but found 'List{any}' * 'Tuple{}'",
    "Expected type 'num' / 'num' but found 'None' / 'Map{str, num}'",
    "Expected type ~ 'num' but found ~ 'bool'",
    "Expected type 'num' ^ 'num' but found 'None' ^ 'bool'",
    "Expected type 'num' > 'num' but found 'str' > 'bool'",
    "Expected type 'num' < 'num' but found 'List{Tuple{str, num, str}}' < 'Error(str)'",
    "Expected type 'num' <= 'num' but found 'Error(List{num})' <= 'Error(str)'",
    "Expected type 'num' >= 'num' but found 'bool' >= 'None'",
    "Types must be related for this operation. 'List{Tuple{str, num, str}}' is not related to 'Error(str)'",
    "Types must be related for this operation. 'str' is not related to 'num'",
    "Expected type 'num' + 'num' but found 'Type' + 'Type'",
  });
}

test "builtin properties .1" {
  const src =
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
    "Cannot modify immutable type 'Tuple{num, num}'",
    "Expected type 'num' | 'num' but found 'num' | 'Type'",
    "Cannot index 'List{any}' type with type 'str'",
  });
}

test "builtin properties .2" {
  const src =
  \\ let j = (1, 2)
  \\ j[0] += 5
  \\ let p = {'b': 5}
  \\ p[5]
  \\ let p = []
  \\ p[1]
  \\ p = [1, 2]
  \\ p["fox"]
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Cannot modify immutable type 'Tuple{num, num}'",
    "Cannot index type 'Map{str, num}' with type 'num'",
    "Cannot index 'List{any}' type with type 'str'",
  });
}

test "builtin generics" {
  const src =
  \\ let j: List{} = []
  \\ let k: Map{str} = {1: 1}
  \\ let i: Tuple{num, bool} = (false,)
  \\ let x: Error{num, list{num}} = (56)!
  \\ alias T{K} = List{K{T}}
  \\ type T{K} = List{K{T}}
  \\ type X{K} = num | str | K{bool}
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "empty type parameters are not supported",
    "generic type instantiated with wrong number of paramters. Expected 1 but found 0",
    "generic type instantiated with wrong number of paramters. Expected 2 but found 1",
    "type variable in generic parameter cannot be generic",
    "expected token '<ident>' but found 'List'",
    "expected token '<ident>' but found 'num'",
  });
}

test "casting.<regular>" {
  const src =
  \\ let j = (1, 2)
  \\ j as Tuple{str, num}
  \\ let j = {'a': 5} as Map{str, Tuple{num, bool}}
  \\ let t = []
  \\ t as List{str}
  \\ let j: any = 5
  \\ (j as num) + 6
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Cannot cast from type 'Tuple{num, num}' to type 'Tuple{str, num}'",
    "Cannot cast from type 'Map{str, num}' to type 'Map{str, Tuple{num, bool}}'",
    "Cannot cast from type 'List{any}' to type 'List{str}'",
    "Cannot cast from type 'any' to type 'num'",
  });
}

test "casting.<active types>" {
  const src =
  \\ type H = A | B
  \\ type H20 = X(List{Tuple{num}}) | Y(List{List{str}})
  \\ let j: H = A
  \\ let p = j as B
  \\ println(p)
  \\ let j: H20 = Y([['a'], ['b']])
  \\ let p = j as H20.X
  \\ println(p)
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Cannot cast from type 'H' to type 'B' because the active type is 'A'",
    "Cannot cast from type 'H20' to type 'H20.X' because the active type is 'Y(List{List{str}})'"
  });
}

test "noreturn" {
  const src =
  \\ def foo(): noreturn
  \\  println("oops")
  \\ end
  \\
  \\ foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit; function declared type 'noreturn' returns",
  });
}

test "never & noreturn" {
  const src =
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

test "never & noreturn .2" {
  const src =
  \\ def noret: noreturn
  \\  noret()
  \\ end
  \\ noret()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit; function declared type 'noreturn' returns",
  });
}

test "never & void .1" {
  const src =
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
  const src =
  \\ def foo(): never
  \\  3
  \\ end
  \\ let j: void = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected return type 'never' but found 'void'",
  });
}

test "type linking" {
  const src =
  \\ alias HashMap{K, V} = Map{K, V}
  \\ alias Q = Q
  \\ alias StringHashMap{V} = HashMap{str, J}
  \\ alias NumList = List{X}
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

  const src2 =
  \\ alias T = T
  \\ alias S = S
  \\ type Q = A(T) | B(S) | C(any)
  \\ let j: Q = C(5)
  \\ j += j
  \\ let t: List{(num?)?} = [Just(Just(5) as num?) as (num?)?, None, None]
  \\ t * t
  \\ t.?
  ;
  try doErrorTest(src2, 3, [_][]const u8{
    "Expected type 'num' + 'num' but found 'Q' + 'Q'",
    "Expected type 'num' * 'num' but found 'List{Just(Just(num) | None) | None}' * 'List{Just(Just(num) | None) | None}'",
    "Types must be related for this operation. Narrowed type 'List{Just(Just(num) | None) | None}' is not related to 'None'",
  });
}

test "conditionals" {
  const src =
  \\ if 3 then
  \\ end
  \\ while [{},] do
  \\ end
  \\ str is num
  \\ 5 is false
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected condition expression to be of type 'bool' but found 'num'",
    "Expected condition expression to be of type 'bool' but found 'List{Map{any, any}}'",
    "Expected type instance in lhs of `is` operator but found 'Type'",
    "Expected type 'Type' in rhs of `is` operator but found type 'false'\n    Help: For constant types, consider using '==' or '!=' operator instead.",
  });
}

test "circularity" {
  const src =
  \\ type T{P} = A1(str) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{num})
  \\ let p: T{str} = A1('fox')
  \\ p = B1(A2(5) as S{str})
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot cast from type 'A2(num)' to type 'S{str}'",
  });
}

test "circularity-2" {
  const src =
  \\ type T{P} = A1(P) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{K})
  \\ let p: T{str} = A1('fox')
  \\ p = B1(A2(5) as S{num})
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot assign type 'B1(A2(num) | B2(A1(num) | B1({...})))' to type 'T{str}'",
  });
}

test "narrowing-1" {
  const src =
  \\ type StrNum = Str(str) | Num(num)
  \\ let x: StrNum? = Just(Str('foobar') as StrNum)
  \\ let p = 10
  \\ match x
  \\  case Just(Str(f)) => f > p
  \\  case Just(Num(q)) => 'yodo'
  \\  case None => do
  \\    type LM = L(List{num}) | M(Map{str, num})
  \\    let x: LM = L([5])
  \\    let p = 10
  \\    if x is LM.L
  \\      x += p
  \\    end
  \\  end
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'num' > 'num' but found 'str' > 'num'",
    "Expected type 'num' + 'num' but found 'L(List{num})' + 'num'",
  });
}

test "narrowing-2" {
  const src =
  \\ type T = L(List{num}) | M(Map{str, num})
  \\ let x: T = L([5])
  \\ if x is T.L
  \\ elif x is T.M
  \\ else
  \\  ~x 
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type ~ 'num' but found ~ 'never'",
  });
}

test "narrowing-3" {
  const src =
  \\ let x: num? = Just(9)
  \\ if x is not num and x is num
  \\   x + 5
  \\ else 
  \\   0+x
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'num' + 'num' but found 'Just(num) | None' + 'num'",
    "Expected type 'num' + 'num' but found 'num' + 'Just(num) | None'",
  });
}

test "narrowing-4" {
  const src =
  \\ let x: num? = Just(9)
  \\ if x is not num and x is num
  \\   x + 5
  \\ elif x is not num
  \\   0+x
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Types must be related for this operation. Narrowed type 'Just(num) | None' is not related to 'num'",
    "elif x is not num",
  });
}

test "narrowing-5" {
  const src =
  \\ let x: Tuple{List{Tuple{num, List{num}}}, num} = ([(9, [] as List{num})], 5)
  \\ let p = 0
  \\ if x[0] is List{Tuple{num, List{num}}}
  \\    if x[0][0] is Tuple{num, List{num}}
  \\      if x[0][0][1] is List{num}
  \\        p /= 5
  \\      end
  \\    end
  \\ elif x[1] is num
  \\    p += x
  \\ else
  \\    x[0]
  \\ end
  \\ p
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'num' + 'Tuple{List{Tuple{num, List{num}}}, num}'",
  });
}

test "narrowing-6.1" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ type ListStr = L(List{NumStr}) | S(str)
  \\ let x: (ListStr)? = Just(L([NumStr.N(5) as NumStr, NumStr.S('a')]) as ListStr)
  \\ if x.? is L and x.?[0] is num
  \\   # pass
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Type 'ListStr' is not indexable",
  });
}

test "narrowing-6.2" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ type ListStr = L(List{NumStr}) | S(str)
  \\ let x: (ListStr)? = Just(L([NumStr.N(5) as NumStr, NumStr.S('a')]) as ListStr)
  \\ if x.? is L
  \\   match x.?
  \\    case L([t, ..]) => t += 5
  \\    case L([..]) => assert(false, 'yay')
  \\   end
  \\ else
  \\    x.?
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'NumStr' + 'num'",
  });
}

test "narrowing-6.3" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ type ListStr = L(List{NumStr}) | S(str)
  \\ let x: (ListStr)? = Just(L([NumStr.N(5) as NumStr, ListStr.S('a')]) as ListStr)
  \\ if x.? is L and x.?[0] is num
  \\   # pass
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'NumStr' but found 'ListStr'",
  });
}

test "narrowing-7" {
  const src =
  \\ type AB = A | B
  \\ let x: AB = A
  \\ let y: AB = B
  \\ if x is A and y is A or y is B
  \\    x # num | str
  \\    y *= 5
  \\ else
  \\    x + y
  \\ end
  \\ y
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'num' * 'num' but found 'A | B' * 'num'",
    "Expected type 'num' + 'num' but found 'B | A' + 'A'",
  });
}

test "narrowing-8" {
  const src =
  \\ type NumStr = Num(num) | Str(str)
  \\ type T = L(List{List{NumStr}}) | M(Map{str, List{NumStr}})
  \\ let x: T =  L([[Num(5) as NumStr]])
  \\ let p = 10
  \\
  \\ match x
  \\  case M({'a': [Num(g), ..]} as m) => if g + 2 > 0xff
  \\    m['a'][0] + 5
  \\  end
  \\  case M(_) => assert(false, '')
  \\  case L(q) => p -= q[0][1]
  \\ end
  \\ assert(p == 5, 'should be 5')
  \\ p
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected type 'num' + 'num' but found 'NumStr' + 'num'",
    "m['a'][0] + 5",
    "Expected type 'num' - 'num' but found 'num' - 'NumStr'",
    "case L(q) => p -= q[0][1]",
  });
}

test "narrowing-9" {
  const src =
  \\ type T = A | Str(str)
  \\ let x: T = Str('fox')
  \\ if x is not Str
  \\    x += 10
  \\ end
  \\ x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'A' + 'num'",
  });
}

test "narrowing-10" {
  const src =
  \\ type NumStr =
  \\  | N(num) | S(str)
  \\ def fun(n: NumStr)
  \\  if n is S
  \\    return n
  \\  end
  \\  if n is N
  \\    return n
  \\  end
  \\  n / n
  \\ end
  \\ fun(S('fancy'))
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' / 'num' but found 'never' / 'never'",
  });
}

test "narrowing-11" {
  const src =
  \\ type Stuff = A("a") | B("b") | Five(5)
  \\ def fish(p: Stuff)
  \\  if p is A
  \\    return 'nice'
  \\  elif p is B
  \\    return 'good'
  \\  elif p is Five
  \\    return 'okay'
  \\  else
  \\    p += 5
  \\  end
  \\ end
  \\
  \\ let x = fish(Five(5))
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'never' + 'num'",
  });
}

test "narrowing-12" {
    const src =
    \\ class Fox
    \\    x: num = 5
    \\    u = 12
    \\ end
    \\ class Foo
    \\    x = 'ok'
    \\    u = 13
    \\ end
    \\ 
    \\ type FooFox = Fo(Foo) | Fx(Fox)
    \\ let f: FooFox = Fo(Foo())
    \\ if f is Fo
    \\  assert(f.u == 13, 'this should be Foo.u')
    \\ elif f is Fx
    \\  assert(f.u == 12, 'this should be Fox.u')
    \\ else
    \\  f += 5 # never
    \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'never' + 'num'",
  });
}

test "dca-0" {
  const src =
  \\ def foo()
  \\  return 5
  \\  alias X = 7
  \\ end
  \\
  \\ foo()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "alias X = 7",
  });
}

test "dca-1" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ def fun(n: NumStr)
  \\  if n is N
  \\    return n
  \\  else 
  \\    return n + 5
  \\  end
  \\  n + 5
  \\ end
  \\ fun(N(12))
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "n + 5",
  });
}

test "dca-2" {
  const src =
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
  const src =
  \\ def bin
  \\  if 1 > 2
  \\    return 5
  \\  else
  \\    return 6
  \\  end
  \\  return 0
  \\ end
  \\ @exit(5)
  \\ bin()
  \\
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "bin()",
  });
}

test "dca-4" {
  const src =
  \\ def bin
  \\  if 1 > 2
  \\    @exit(5)
  \\  else
  \\    @panic('oops')
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
  const src =
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
  const src =
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
  const src =
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
  const src =
  \\ def bin
  \\  let j = 12
  \\  return 0
  \\  return j
  \\ end
  \\ bin()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "return j",
  });
}

test "dca-9" {
  const src =
  \\ def fun(x: num)
  \\  match x
  \\    case 1..100 as x => return x
  \\    case 101..200 => return 12
  \\    case _ => return 0
  \\  end
  \\  return 7
  \\ end
  \\
  \\ println(fun(5))
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "return 7",
  });
}

test "constant types" {
  const src =
  \\ let x: 5 = 5
  \\ let y: 7 = 7
  \\ y * x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' * 'num' but found '7' * '5'",
  });
}

test "functions-1" {
  const src =
  \\ alias T = num
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
    "Argument type mismatch. Expected type 'T' but found 'str'",
    "Non-generic function called as generic",
    "Argument arity mismatch. Expected 0 argument(s) but found 1",
  });
}

test "functions-2" {
  const src =
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
    "Expected type 'num' - 'num' but found 'never' - 'never'",
  });
}

test "functions-3" {
  const src =
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
  const src =
  \\ def fancy{T}(x: T): fn{T}(num): str
  \\  let j: T = x
  \\  return fancy{T}(j)
  \\ end
  \\ fancy(false)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '(' but found '{'",
  });
}

test "functions-5" {
  const src =
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
  const src =
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
  const src =
  \\ def funny(t: List{str})
  \\  println(t)
  \\ end
  \\ funny([])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'List{str}' but found 'List{any}'",
  });
}

test "error type" {
  const src =
  \\ def fancy(x: num)
  \\  if x > 5
  \\    return ('bad')!
  \\  else 
  \\    return Ok(x * 4)
  \\  end
  \\ end
  \\ let j = fancy(5)
  \\ j + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'Error(str) | Ok(num)' + 'num'",
  });
}

test "simple-classes-1" {
  const src =
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
    "type 'Fox' has no property 'y'",
    "a class having field(s) without defaults must define an `init` method that initializes such field(s)",
    "field 'y' is declared but uninitialized",
    "Argument arity mismatch. Expected 1 argument(s) but found 0",
  });
}

test "simple-classes-2" {
  const src =
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

test "immutable method mod" {
  const src =
  \\ class Foo
  \\  pub def fish()
  \\    return 5
  \\  end
  \\ end
  \\
  \\ let j = Foo()
  \\ j.fish = def => 10
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Cannot modify immutable type 'fn (): num'",
    "j.fish = def => 10"
  });
}

test "field init" {
  const src =
  \\ class Foo
  \\  x: str
  \\  def init()
  \\  end
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "I am unable to deduce that the field 'x' is definitely initialized"
  });
}

test "generic-classes-1" {
  const src =
  \\ let p = {'a': 5}
  \\ p.doesNotExist()
  \\ let q = [1, 2]
  \\ q.lens()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "type 'Map{str, num}' has no property 'doesNotExist'",
    "type 'List{num}' has no property 'lens'",
  });
}

test "generic-classes-2" {
  const src =
  \\ class Fox{T}
  \\    x: List{T}
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
    "type 'Fox{num}' has no property 'y'",
    "Type 'Error(num)' is not indexable",
    "Type 'num' is not indexable",
    "Expected type 'num' - 'num' but found 'num' - 'str'",
    "Type 'num' is not indexable"
  });
}

test "generic-classes-3" {
  const src =
  \\ class Fox{T}
  \\    x: List{T}
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
  \\ alias Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j: Poo{'miah'} = Fox{'mia'}('mia')
  ;
    try doErrorTest(src, 8, [_][]const u8{
    "Expected type 'num' + 'num' but found 'List{mia}' + 'num'",
    "Could not resolve type of ident: 'j'",
    "Could not resolve type of ident: 'p'",
    "Expected type 'num' + 'num' but found 'List{mia}' + 'num'",
    "Expected type 'num' + 'num' but found 'num' + 'str'",
    "Expected type 'num' + 'num' but found 'str' + 'str'",
    "Argument type mismatch. Expected type 'mia' but found 'num'",
    "Cannot initialize type 'Poo{miah}' with type 'Fox{mia} instance'"
  });
}

test "loopy" {
  const src =
  \\ type AB = A | B
  \\ let x: AB = A
  \\ let y = 5
  \\ while x is A and y < 25 do
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
    "Expected type 'num' + 'num' but found 'num' + 'str'",
    "Expected type 'num' + 'num' but found 'AB' + 'num'",
  });
}

test "labeled argument" {
  const src =
  \\ def fun(x: str, y: num, a: List{num}, b: Result{any, str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y='ops', a=6, x=[6], ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'str' but found 'List{num}'",
  });
}

test "labeled argument 2" {
  const src =
  \\ def fun(x: str, y: num, a: List{num}, b: Result{any, str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y='ops', a=6, x='ok', ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'num' but found 'str'",
  });
}

test "labeled argument 3" {
  const src =
  \\ def fun(x: str, y: num, a: List{num}, b: Result{any, str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y=5, a=6, x='ok', ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'List{num}' but found 'num'"
  });
}

test "labeled argument 4" {
  const src =
  \\ def fun(x: str, y: num, a*: List{num})
  \\  println('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y=5, a=[2, 3], y='oo', a=[1, 2], a=[5, 6, 7])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "duplicate labeled argument found",
  });
}

test "labeled argument 5" {
  const src =
  \\ def fun(x: str, y: num, a*: List{num})
  \\  println('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y=5, a=[2, 3])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "missing required argument(s)",
  });
}

test "labeled argument 6" {
  const src =
  \\ def fun(x: str, y: num, a: List{num}, b: Result{any, str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(6='a', 12: 9)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "invalid labeled argument",
  });
}

test "class init" {
  const src =
  \\ class Err{B}
  \\  val: B
  \\ end
  \\ Err('box')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Too many arguments to class call. Expected none but found 1",
  });
}

test "patterns-1.<ordinary match>" {
  const src =
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a' as a, 'b' as b) as d => println('ok')
  \\  case ('q', 'k') => println('third')
  \\ end
  ;
 try doErrorTest(src, 4, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, str)",
    "Tuple(str, _)"
  });
}

test "patterns-2.<scopes>" {
  const src =
  \\ let o = 5
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a', 'b' as o) as d => o = 10
  \\  case ('q', 'k') => println('third')
  \\  case _ => println("last")
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot assign type 'num' to type 'str'",
  });
}

test "patterns-3.<nested match>" {
  const src =
  \\ let z = false
  \\ match (('a', 'b'), ('x', 'y'))
  \\
  \\  case (('x', 'y'), ..) => do
  \\    let p = z
  \\    println('first')
  \\  end
  \\  case (('a', ..), u) => do
  \\    let b = z
  \\    println('here!')
  \\  end
  \\  case (('a', t, ..) as d, ..) => do
  \\    let h = z
  \\    println('second')
  \\    z = true
  \\  end
  \\  case (('x', k), y) => do
  \\    let v = z
  \\    println('third')
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
  const src =
  \\ type abc = A | B | C
  \\ let j: abc = B
  \\ match j
  \\  case A => println('a')
  \\  case C => println('hmm')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "B"
  });
}

test "patterns-5.<match on classes (fields)>" {
  const src =
  \\ class Ant
  \\  a = 5
  \\ end
  \\ class Rat
  \\  x = 1
  \\  y = 8
  \\ end
  \\ type AntRat = A(Ant) | R(Rat)
  \\ let j = R(Rat()) as AntRat
  \\ match j
  \\  case A(Ant(a)) if a > 2 => println(12)
  \\  case A(Ant(5)) => println(13)
  \\  case A(Ant()) => println(40)
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "pattern of unequal number of argument(s): 0 and 1",
    "case A(Ant()) => println(40)",
  });
}

test "patterns-6.<match on classes (fields)>" {
  const src =
  \\ class Ant
  \\  a = 5
  \\ end
  \\ class Rat
  \\  x = 1
  \\  y = 8
  \\ end
  \\ type AntRat = A(Ant) | R(Rat)
  \\ let j = R(Rat()) as AntRat
  \\ match j
  \\  case A(Ant(a)) if a > 2 => println(12)
  \\  case A(Ant(5)) => println(13)
  \\  case R(Rat(2)) => println(40)
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "'Rat' has 2 field(s), but pattern test assumes 1",
  });
}

test "patterns-7.<tuple exhaustiveness>" {
  const src =
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a', 'b' as o) as d => println('second')
  \\  case ('q', 'k') => println('third')
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, str)",
    "Tuple(str, _)"
  });
}

test "patterns-8.<list exhaustiveness>" {
  const src =
  \\ let z = false
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => println('first')
  \\  case [('a', 'b' as o)] as d => z = true
  \\  case [('q', 'k')] => println('third')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, str)",
    "Tuple(str, _)",
    "List{Tuple{str, str}}"
  });
}

test "patterns-9.<match redundancy>" {
  const src =
  \\ let z = false
  \\ class Cat
  \\ end
  \\ class Dog
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match p
  \\  case D(Dog()) => println('good')
  \\  case D(Dog()) => println('hmm')
  \\  case C(Cat()) => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case D(Dog()) => println('hmm')"
  });
}

test "patterns-10.<type checks>" {
  const src =
  \\ class Cat
  \\  x = 1
  \\ end
  \\ class Dog
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match p
  \\  case D(Dog()) => println('hmm')
  \\  case C(Cat('fox')) => println('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' but found 'str'"
  });
}

test "patterns-11.<type checks>" {
  const src =
  \\ class Cat
  \\  x = 1
  \\ end
  \\ class Dog
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match p
  \\  case [] => println('hmm')
  \\  case [C(Cat(5))] => println('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Animal' is not related to type 'List{}'",
  });
  const src2 =
  \\ class Cat
  \\  x = 1
  \\ end
  \\ class Dog
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match p
  \\  case Cat(5) => println('nope')
  \\ end
  ;
  try doErrorTest(src2, 1, [_][]const u8{
    "type 'Animal' is not related to type 'Cat'",
  });
}

test "patterns-12.<type checks>" {
  const src =
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match [p]
  \\  case [] => println('hmm')
  \\  case [D(Cat(5))] => println('nope')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "type 'Dog' is not related to type 'Cat'",
    "case [D(Cat(5))] => println('nope')"
  });
}

test "patterns-13.<type checks>" {
  const src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match p
  \\  case D(Dog(x=5)) => println('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Dog' has no field 'x'",
  });
}

test "patterns-14.<type checks (fields)>" {
  const src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = D(Dog())
  \\ let z = false
  \\ match p
  \\  case D(Dog(x=5, j=10)) => println('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Dog' has 1 field(s), but pattern test assumes 2",
  });
}

test "patterns-14b.<label arguments>" {
  const src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = D(Dog())
  \\ let z = false
  \\ match p
  \\  case D(Dog(y=5, x, ..)) => println('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "label used inconsistently in pattern arguments",
  });
}

test "patterns-14b.<type checks (fields)>" {
  const src =
  \\ class Cat
  \\  x = 5
  \\ end
  \\ class Dog
  \\  y = 10
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = D(Dog())
  \\ let z = false
  \\ match p
  \\  case D(Dog(x, y, ..)) => println('nope')
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "type 'Dog' has 1 field(s), but pattern test assumes 2 or more",
    "inexhaustive pattern match",
    "Remaining pattern type(s)",
    "C(Cat)"
  });
}

test "patterns-15.<match on bool (exhaustiveness)>" {
  const src =
  \\ match (1 < 2)
  \\  case false => println('nay')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "true",
  });
}

test "patterns-16.<match on bool (exhaustiveness)>" {
  const src =
  \\ match (1 < 2)
  \\  case true => println('nay')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "false",
  });
}

test "patterns-17.<ranges (exhaustiveness)>" {
  const src =
  \\  let n = 10 / 2
  \\  match n
  \\    case 0..2 => println('hey')
  \\    case 3..5 => println('hah')
  \\  end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "num",
  });
}

test "patterns-18.<redundancy>" {
  const src =
  \\ match true
  \\  case false => println('nay')
  \\  case true => println('yay')
  \\  case _ => println('oops')
  \\ end
  \\ 1 / '' # necessary for error propagation
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => println('oops')"
  });
}

test "patterns-19.<redundancy>" {
  const src =
  \\ type T = A | B | C
  \\ let j: T = C
  \\ match j
  \\   case A => println('a!')
  \\   case B => println('b!')
  \\   case C => println('c!')
  \\   case _ => println('other!')
  \\ end
  \\ 1 / ''
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => println('other!')"
  });
}

test "patterns-20.<redundancy>" {
  const src =
  \\ class Ant
  \\ end
  \\ class Rat
  \\ end
  \\ type AntRat = A(Ant) | R(Rat)
  \\ let j = R(Rat()) as AntRat
  \\ match j
  \\  case A(Ant()) => 12
  \\  case R(Rat()) => 15
  \\  case _ => 5
  \\ end
  \\ 1 / ''
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => 5"
  });
}

test "patterns-21.<match on None (exhaustiveness)>" {
  const src =
  \\ type T = A | B
  \\ let j: T? = Just(B as T)
  \\ match j
  \\   case Just(A | B) => println('a!')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "None",
  });
}

test "patterns-22.<match on generics (exhaustiveness)>" {
  const src =
  \\ class Fox{T}
  \\  j: T
  \\  def init(j: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let z = false
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{str,num} = F1(Fox('pin'))
  \\ match j
  \\  case F2(Fox(6)) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\  case F1(Fox('pin')) as x => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Fox(str)",
  });
}

test "patterns-23.<match on generics (exhaustiveness)>" {
  const src =
  \\ class Fox{T}
  \\  j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant{T}
  \\ end
  \\ type Fx{T, V, K} = F1(Fox{T}) | F2(Fox{V}) | F3(Fox{K}) | F4(Ant{T})
  \\ let j: Fx{str, num, str} = F4(Ant{str}())
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F2(Fox([6])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "F3(Fox{str}) | F4(Ant{str})",
  });
}

test "patterns-24.<match on generics (redundancy)>" {
  const src =
  \\ class Fox{T}
  \\  j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{str, num} = F1(Fox{str}('pin', 'pan'))
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F1(Fox(_)) => println('caught ya str')
  \\  case F2(Fox([6])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case F1(Fox(_)) => println('caught ya str')"
  });
}

test "patterns-25.<match on generics (redundancy)>" {
  const src =
  \\ class Fox{T}
  \\  pub j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant{T}
  \\ end
  \\ type Fx{T, V, K} = F1(Fox{T}) | F2(Fox{V}) | F3(Ant{T})
  \\ let j: Fx{str, num, str} = F3(Ant{str}())
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F2(Fox([6])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\  case F3(Ant()) => println('caught ya num')
  \\  case _ => 'oops'
  \\ end
  \\ 1 / ''
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => 'oops'"
  });
}

test "patterns-26.<redundancy>" {
  const src =
  \\ type Type = A | B| C
  \\ let j: Type = A
  \\ match j
  \\   case A as k => println(j, k)
  \\   case _ as t => match t
  \\      case B => println("B.1")
  \\      case C => println("C.1")
  \\      case _ => println('err.1')
  \\   end
  \\ end
  \\ 1 / ''
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => println('err.1')"
  });
}

test "patterns-26.<redundancy-hard-error>" {
  const src =
  \\ match ('a', 'b', 'c')
  \\  case ('x', 'y', 'a') => println('first')
  \\  case ('a' as a, 'b' as b, _) as d => println('ok')
  \\  case ('q', 'k', 'q') => println('third')
  \\  case (_, _, _) => ''
  \\  case (..) => '^_^'
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Consider rewriting the pattern or rearranging the case clause(s).",
    "case (..) => '^_^'"
  });
}

test "patterns-27.<redundancy>" {
  const src =
  \\ let j = true
  \\ match j
  \\   case false as k => println(j, k)
  \\   case _ as t => match t
  \\      case true => println("B.1")
  \\      case _ => println('err.2')
  \\   end
  \\ end
  \\ 1 / ''
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => println('err.2')"
  });
}

test "patterns-28.<match on maps (redundancy)>" {
  const src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ match foo
  \\  case {"sound": _, "format": _} => println(1)
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Str("txt"),} => println(12, url, a, b)
  \\  case {..} => println('default')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case {\"sound\" as a: Class(Fox(url)) as b, \"format\": Str(\"txt\"),} => println(12, url, a, b)"
  });
}

test "patterns-29.<match on maps (exhaustiveness)>" {
  const src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ match foo
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Str("ogg"),} => println(12, url, a, b)
  \\  case {"sound": _, "format": _} => println(1)
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Map{str, _}",
  });
}

test "patterns-29.<match on maps (exhaustivenes 2)>" {
  const src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ match foo
  \\  case { a: Class(Fox('url')) as b} => println(12, url, a, b)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Fox(str)",
    "Map{_, Class(Fox) | Str(str)}",
  });
}

test "patterns-29.<match on maps (exhaustivenes 3)>" {
  const src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ match foo
  \\  case { a: Class(Fox('url')) as b, x: Str("ogg"),} => println(12, url, a, b)
  \\ end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Str(str)",
    "Map{_, Class(Fox) | Str(str)}",
    "Fox(str)",
  });
}

test "patterns-30.<match on maps (redundancy)>" {
  const src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ match foo
  \\  case {..} => println('default')
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Str("ogg"),} => println(12, url, a, b)
  \\  case {"sound": _, "format": _} => println(1)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case {\"sound\" as a: Class(Fox(url)) as b, \"format\": Str(\"ogg\"),} => println(12, url, a, b)",
    "possible redundant case",
    "case {\"sound\": _, \"format\": _} => println(1)",
  });
}


test "patterns-30b.<match on lists (redundancy)>" {
  const src =
  \\ match [1, 6]
  \\  case [..] => println('default')
  \\  case [8, 9] => println(12, url, a, b)
  \\  case [0, 8] => println(1)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case [8, 9] => println(12, url, a, b)",
    "possible redundant case",
    "case [0, 8] => println(1)",
  });
}

test "patterns-31.<match in functions>" {
  const src =
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
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "num",
  });
}

test "patterns-32.<match in functions>" {
  const src =
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 11..50 as q => println('yep')
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let j = check(12)
  \\ j += 4
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot return multiple types: 'num & void'",
    "Expected type 'num' + 'num' but found 'num & void' + 'num'"
  });
}

test "cfa.<void returns>" {
  const src =
  \\ def check(n: num)
  \\  if n > 5
  \\    do
  \\    end
  \\  else
  \\    return 5
  \\  end
  \\ end
  \\ check(3) + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'num & void' + 'num'",
  });
}

test "cfa-2.<void returns>" {
  const src =
  \\ def check(n: num)
  \\  if n > 5
  \\    return 5
  \\  else
  \\    do
  \\    end
  \\  end
  \\ end
  \\ check(3) + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'num & void' + 'num'",
  });
}

test "patterns-33.<match in functions>" {
  const src =
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 'a'
  \\   case 5..9 => return true
  \\    case 11..50 as q => println('yep')
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let j = check(12)
  \\ j += 4
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot return multiple types: 'str & bool & num & void'",
    "Expected type 'num' + 'num' but found 'str & bool & num & void' + 'num'"
  });
}

test "patterns-34.<match in functions>" {
  const src =
  \\ def check(n: num): num
  \\  match n
  \\   case 1..4 => return 5
  \\   case 5..9 => println('jjj')
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
  const src =
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
    "Expected return type 'num' but found 'str'",
  });
}

test "patterns-36.<inexhaustive rested patterns>" {
  const src =
  \\ let z = false
  \\ match ('a', false, 'b', true, 'c', false)
  \\  case ('x', _, 'c', _, ..) => println('has keys "x" and "c"')
  \\  case ('a', _, 'b', _ as p, ..) if z => println(z)
  \\  case (..) if !z => println('has something or none')
  \\ end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, _, str, _, _, _)",
    "bool",
    "Tuple(str, _, _, _, _, _)"
  });
}

test "patterns-37.<inexhaustive rested patterns>" {
  const src =
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} if !z => println('has something or none')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "List{str}",
  });
}

test "patterns-38.<inexhaustive patterns>" {
  const src =
  \\ def goodOrBad(n: num)
  \\   if n > 25
  \\    return ('oops')!
  \\   end
  \\   return Ok(n * 2)
  \\ end
  \\
  \\ match goodOrBad(30)
  \\  case Error(error) => println(error)
  \\  case Ok(1..10) as x => println(x)
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Ok(num)",
  });
}

test "patterns-39.<inexhaustive patterns>" {
  const src =
  \\ def goodOrBad(n: num)
  \\   if n > 25
  \\    return ('oops')!
  \\   end
  \\   return Ok(n * 2)
  \\ end
  \\
  \\ match goodOrBad(30)
  \\  case Error('ack' as j) => println(j)
  \\  case Ok(1..10) as x => println(x)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Error(str)",
    "Ok(num)",
  });
}

test "patterns-40.<annotated tags>" {
  const src =
  \\ type T = Tag(a: num, b: str, c: bool)
  \\  match Tag(a=5, b='oops', c=false)
  \\    case Tag(a=5, b=6, c=7) => println('yay')
  \\  end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'str' but found 'num'",
  });
}

test "patterns-41.<annotated tags>" {
  const src =
  \\ type T = Tag(a: num, b: str, c: bool)
  \\  match Tag(a=5, b='oops', c=false)
  \\    case Tag(a=5, b='oops', c=false) => println('yay')
  \\  end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tag(_, _, true)",
    "Tag(_, str, _)",
    "Tag(num, _, _)",
  });
}

test "patterns-42.<annotated tags>" {
  const src =
  \\ type T = Tag(a: num, b: str, c: bool)
  \\  match Tag(a=5, b='oops', c=false)
  \\    case Tag(a=7, b='nope', d=true) => println('nay')
  \\  end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'T' has no field 'd'",
    "inexhaustive pattern match",
    "Remaining pattern type(s):",
    "Tag(_, str, _)",
    "Tag(num, _, _)"
  });
}

test "patterns-43.<annotated tags exhaustiveness>" {
  const src =
  \\ type T = Tag(a: num, b: str, c: bool)
  \\  match Tag(a=5, b='oops', c=false)
  \\    case Tag(a=5, b='oopsy', c=false) => println('nay')
  \\    case Tag(a=_, b=_, c=true) => println('nay')
  \\  end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tag(_, _, false)",
  });
}

test "patterns-44.<constant types>" {
  const src =
  \\ type F = Foo(a: 5, b: 5)
  \\ match Foo(a=10, b=10)
  \\  case _ => "nah"
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type '5' but found 'num'",
  });
}

test "patterns-44.<constant types 2>" {
  const src =
  \\ type F = Foo(a: 5, b: 5)
  \\ match Foo(a=5, b=5)
  \\  case Foo(b=10, a=10) => "nah"
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type '5' but found 'num'",
  });
}

test "patterns-44.<wrong label position>" {
  const src =
  \\ type F = Foo(a: 5, b: 5)
  \\ match Foo(a=5, b=5)
  \\  case Foo(b=5, a=5) => "nah"
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "The tag field 'a' was found in a wrong position.",
  });
}

test "patterns-45.<redundancy in constant types>" {
  const src =
  \\ class Foo
  \\  pub a: 5 = 5
  \\ end
  \\ match Foo()
  \\  case Foo(a=5) => "nah"
  \\  case _ => "boh"
  \\ end
  \\ 1 / '1' # error propagation since we're testing a warning
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => \"boh\""
  });
}

test "patterns-46.<missing patterns>" {
  const src =
  \\ class Ant
  \\ end
  \\ class Rat
  \\ end
  \\ type AntRat = A(Ant) | R(Rat)
  \\ let j = R(Rat()) as AntRat
  \\ match j
  \\  case A(Ant()) => 12
  \\  case R(Rat()) => 15
  \\  case _ => 5
  \\ end
  \\ type Season = Spring | Summer | Autumn | Winter | Mag(str, num, str, num)
  \\ match Autumn as Season
  \\   case Spring => "Mild"
  \\   case Summer => "Hot"
  \\   case Autumn => "Windy"
  \\   case Winter => "Cold"
  \\   case Mag("a", 5, "b", 7) => "ah"
  \\ end
  ;
 try doErrorTest(src, 6, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Mag(_, _, _, num)",
    "Mag(_, _, str, _)",
    "Mag(_, num, _, _)",
    "Mag(str, _, _, _)",
  });
}

test "patterns-47.<missing patterns>" {
  const src =
  \\ class Panda
  \\  pub x: num
  \\  pub y: str
  \\  def init(a: num, b: str)
  \\    self.x = a
  \\    self.y = b
  \\  end
  \\ end
  \\ type Legion = Legion(str, num, Panda, bool)
  \\ let j = Legion('a', 5, Panda(5, 'boy'), true)
  \\ match j
  \\  case Legion('a', 5, Panda(5, 'boy'), true) => println('you rock!')
  \\ end
  ;
 try doErrorTest(src, 7, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Legion(_, _, _, false)",
    "Panda(_, str)",
    "Panda(num, _)",
    "Legion(_, num, _, _)",
    "Legion(str, _, _, _)"
  });
}

test "parse-modes .1" {
  const src =
  \\ class str
  \\   x: num
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<ident>' but found 'str'",
  });
}

test "parse-modes .2" {
  const src =
  \\ class List{X}
  \\   x: X
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<ident>' but found 'List'",
  });
}

test "parse-modes .3" {
  const src =
  \\ type Foo = None | Just
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<ident>' but found 'None'",
  });
}

test "typing.<untagged unions>" {
  const src =
  \\ let x: str | num = 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '=' but found '|'",
  });
}

test "typing.<nullable>" {
  const src =
  \\ let x: num? = None
  \\ let y: str? = x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'Just(str) | None' with type 'Just(num) | None'",
  });
}

test "typing.<tagged unions>" {
  const src =
  \\ def stup(x: num)
  \\  if x > 2
  \\    return None
  \\  else
  \\    return Error('oops')
  \\  end
  \\ end
  \\
  \\ stup(1).? + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected 'Maybe' type, found 'Error(str)'",
  });
}

test "typing.<tagged unions 2>" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ def fun(n: NumStr)
  \\  if n is S
  \\    return n
  \\  end
  \\ end
  \\ fun(S('fancy'))
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "cannot return multiple types: 'S(str) & void'",
  });
}

test "typing.<tagged unions 3>" {
  const src =
  \\ type Foo{T} = Lists(T{List{Num}}) | Maps(Map{Str, Num})
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type variable in generic parameter cannot be generic",
  });
}

test "typing.<tagged unions 3b>" {
  const src =
  \\ type Bat = A(List{Fox}, Tuple{Fox}) | B
  \\ type Cat = A(name: Str, booh: Num) | B
  \\ alias Fox = Foo?
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<newline>' but found '?'"
  });
}

test "typing.<tagged unions 4>" {
  const src =
  \\ type Bat = A(List{Fox}, Tuple{Fox}) | B
  \\ type Cat = A(name:Str, age:Num) | B
  \\ alias Fox = Foo
  \\ type BTree{T} = Node(T) | Branch(BTree{T}, BTree{T})
  \\ let tree: BTree{num} = Branch(Node(1), Node(2))
  \\ alias Foxy = str
  \\ type Bio = Fox(num) | Name(Foxy)
  \\ type Pairk{K, V} = Pair(K, V)
  \\ let p:Pairk{str, str} = Pair(0, 0)
  \\ type VOrC{V, C} = Vee(V) | Cee(C)
  \\ alias A{K, V} = Map{K, Map{K, VOrC{VOrC{K, V}, K}}}
  \\ 2 as A{str, num}
  \\ alias A{K, V} = Maybe{VOrC{K, V}}
  \\ 2 as A{bool, str}
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Cannot initialize type 'Pairk{str, str}' with type 'Pair(num, num)'",
    "Cannot cast from type 'num' to type 'A{str, num}'",
    "Cannot cast from type 'num' to type 'A{bool, str}'"
  });
}

test "typing.<immutable varargs>" {
  const src =
  \\ def fun(x*: num)
  \\  x[0] = 12
  \\ end
  \\
  \\ fun(2, 3, 4)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot modify immutable type 'List{num}'",
  });
}

test "typing.<type and tag collision>" {
  const src =
  \\ type J = J(str) | T(num)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type with name 'J' shadows one of its variants",
  });
}

test "typing.<type param and tag collision>" {
  const src =
  \\ type J{K, V} = K | V
  \\ 1 / ''
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type variable is used as a tag in its type definition.\n"
    ++ "    If this is a mistake, consider renaming the type parameter.",
  });
}

test "typing.<tag collision>" {
  const src =
  \\ type J = K | K
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "duplicate tag",
  });
}

test "tags.<params>" {
  const src =
  \\ type T = A(str)
  \\ let j = A()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument arity mismatch. Expected 1 argument(s) but found 0",
  });
}

test "tags.<duplicate>" {
  const src =
  \\ type T = A(x:str, y:num)
  \\ let j = A(x='a', x=5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "duplicate labeled argument found",
  });
}

test "tags.<label incomplete>" {
  const src =
  \\ type T = A(x:str, num)
  \\ let j = A(j='a', 5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type with one or more labeled argument(s) must be completely labeled",
  });
}

test "tags.<label>" {
  const src =
  \\ type T = A(x:str, y:num)
  \\ let j = A(j='a', 5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "illegal or invalid label: 'j'",
  });
}

test "aspec.<fields 1>" {
  const src =
  \\ class Foo
  \\  x: num = 19
  \\  y = 11
  \\ end
  \\ let j = Foo()
  \\ j.x + j.y
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "access of private field 'x' outside its defining class",
    "access of private field 'y' outside its defining class",
  });
}

test "aspec.<fields 2>" {
  const src =
  \\ class Fish
  \\  pub x: str
  \\  pub y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{num}
  \\  end
  \\
  \\  pub def fox()
  \\    let x = match Fox()
  \\      case Fox(x) => 2 + x
  \\    end
  \\    return x
  \\  end
  \\ end
  \\
  \\ class Fox
  \\  x = 12
  \\ end
  \\
  \\ Fish().fox()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "access of private field 'x' outside its defining class",
  });
}

test "aspec.<fields 3>" {
  const src =
  \\ class Fish
  \\  pub x: str
  \\  pub y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{num}
  \\  end
  \\
  \\  pub def fox()
  \\    let x = def () => (match Fish() case Fish(x, y, j) => 2 + y end)
  \\    return x()
  \\  end
  \\ end
  \\ Fish().fox()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "access of private field 'j' outside its defining class",
  });
}

test "aspec.<fields 4>" {
  const src =
  \\ class Fish
  \\  pub x: str
  \\  pub y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{num}
  \\  end
  \\
  \\  pub def fox()
  \\    let x = def () => (match self case Fish(x, y, j) => 2 + y end)
  \\    return x()
  \\  end
  \\ end
  \\ Fish().fox()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "access of private field 'j' outside its defining class",
  });
}

test "aspec.<methods 1>" {
  const src =
  \\ class Foo
  \\  def fun()
  \\    print(self.fun)
  \\  end
  \\ end
  \\ let j = Foo()
  \\ j.fun()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "access of private method 'fun' outside its defining class",
  });
}

test "aspec.<methods 2>" {
  const src =
  \\ class Fish
  \\  pub x: str
  \\  pub y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 0
  \\    self.j = [] as List{num}
  \\    print((def () => self.fox())())
  \\  end
  \\
  \\  def fox()
  \\    return 5
  \\  end
  \\ end
  \\
  \\ Fish()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "access of private method 'fox' outside its defining class",
  });
}

test "aspec.<methods 3>" {
  const src =
  \\ class Fish
  \\  x: str
  \\  y: num
  \\  j: List{num}
  \\
  \\  def pub fox()
  \\    return self.doStuff()
  \\  end
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<ident>' but found 'pub'",
  });
}

test "tag namespaces" {
  const src =
  \\ type Cat = A(str) | B(str)
  \\ type Dog = A(num) | B(str)
  \\ let x: Cat = Cat.A('fox')
  \\ let y: Dog = Dog.A(12)
  \\ println(x, y)
  \\ # no longer okay, because although Dog.B is exactly same as Cat.B, the namespace (Dog or Cat) now matters, they're not just aliases
  \\ x = Dog.B('foo')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot assign type 'Dog' to type 'Cat'",
  });
}

test "tag namespaces .2" {
  const src =
  \\ type T = A | B
  \\ let j = T.A
  \\ j + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' + 'num' but found 'T' + 'num'",
  });
}

test "tag namespaces .3" {
  const src =
  \\ type T = A | B
  \\ let j: A = T.A
  \\ j + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'A' with type 'T'",
  });
}

test "parameterized access" {
  const src =
  \\ type T{K, V} = K
  \\ let x: T{str, num} = T.K
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "bad access of parameterized type",
  });
}

test "unary add" {
  const src =
  \\ let x = "abc"
  \\ println(+x)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type + 'num' but found + 'str'",
  });
}

test "pipelines .1" {
  const src =
  \\ let x = *
  \\ println(x)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "use of pipe placeholder outside a pipeline expression",
  });
}

test "match <statement in expr>" {
  const src =
  \\ let j = 10
  \\ let k = j |> match *
  \\  case 1..6 as p => j = true
  \\  case _ as w => j = false
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token 'end' but found '='",
  });
}

test "traits <required methods .1>" {
  const src =
  \\ trait Display
  \\  pub def fmt(): str;
  \\ end
  \\
  \\ class Foo: Display
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display':",
    "The following method(s) are not implemented:",
    "fmt", " : ", "fn (): str",
  });
}

test "traits <required methods .2>" {
  const src =
  \\ trait Display
  \\  pub def fmt(): str;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return 5
  \\  end
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected method 'fn (): str' but found 'fn (): num'",
    "pub def fmt()",
  });
}

test "traits <required methods .3>" {
  const src =
  \\ trait Display
  \\  pub def fmt(): str
  \\    return "NotImplemented"
  \\  end
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return 5
  \\  end
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected method 'fn (): str' but found 'fn (): num'",
    "pub def fmt()",
  });
}

test "traits <required methods .3.5>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{num} | Clone{Stuff}
  \\    x = 12
  \\  pub def clone()
  \\    return 'Stuff()'
  \\  end
  \\  pub def shift(shr: num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  \\ println(s.shift(2))
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected method 'fn (): Stuff' but found 'fn (): str",
    "pub def clone()",
    "This error was triggered from here:",
    "class Stuff: Shifts{num} | Clone{Stuff}"
  });
}

test "traits <required methods .4>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ class Foo: Display | Clone
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 8, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display & Clone':",
    "The following method(s) are not implemented:",
    "fmt", " : ", "fn (): str",
    "clone", " : ", "fn (): Clone",
  });
}

test "traits <required methods .5>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\
  \\ class Foo: Display | Clone{Foo}
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 8, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display & Clone{Foo}':",
    "The following method(s) are not implemented:",
    "fmt", " : ", "fn (): str",
    "clone", " : ", "fn (): Foo",
  });
}

test "traits <required methods .6>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\
  \\ class Foo: Display | Clone{Foo}
  \\  pub def fmt()
  \\    return "Oops"
  \\  end
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display & Clone{Foo}':",
    "The following method(s) are not implemented:",
    "clone", " : ", "fn (): Foo",
  });
}

test "traits <required methods .7>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait DisplayExt{T: Display}
  \\  pub def show(): String;
  \\ end
  \\
  \\ class Foo: Display | DisplayExt{Foo}
  \\ end
  \\
  \\ let f1 = Foo()
  ;
  try doErrorTest(src, 8, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display & DisplayExt{Foo}':",
    "The following method(s) are not implemented:",
    "fmt", " : ", "fn (): str",
    "show", " : ", "fn (): str",
  });
}

test "traits <required methods .8>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait DisplayExt{T: Display}
  \\  pub def show(): String;
  \\ end
  \\
  \\ class Foo: Display | DisplayExt{Foo}
  \\  pub def show()
  \\    return "MyShow!"
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display & DisplayExt{Foo}':",
    "The following method(s) are not implemented:",
    "fmt", " : ", "fn (): str",
  });
}

test "traits <required methods .9>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait DisplayExt{T: Display}
  \\  pub def show(): String;
  \\ end
  \\
  \\ class Foo: Display | DisplayExt{Foo}
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display & DisplayExt{Foo}':",
    "The following method(s) are not implemented:",
    "show", " : ", "fn (): str",
  });
}

test "traits <required methods .10>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait DisplayExt{T: Display}
  \\  pub def show(): String;
  \\ end
  \\
  \\ class Bar
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\ end
  \\
  \\ class Foo: Display | DisplayExt{Bar}
  \\  pub def show()
  \\    return "MyShow!"
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type 'Bar' does not implement the trait 'Display'",
  });
}

test "traits <visibility .1>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\ assert(Foo().fmt() == "Foo()", 'should be same')
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "method 'fmt' has a visibility different from its trait specification",
    "Trait method specified here:",
    "pub def fmt(): String;",
  });
}

test "traits <visibility .2>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Clone{T}
  \\  def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{num} | Clone{Stuff}
  \\    x = 12
  \\  pub def clone()
  \\    return Stuff()
  \\  end
  \\  pub def shift(shr: num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "method 'clone' has a visibility different from its trait specification",
    "Trait method specified here:",
    "def clone(): T;",
  });
}

test "traits <contravariance .1>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ let f: Display = Foo()
  \\ let g: Foo = f # contravariance
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'Foo' with type 'Display'",
  });
}

test "traits <contravariance .2>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ let f: List{Display} = [Foo()]
  \\ let g: List{Foo} = f # contravariance
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'List{Foo}' with type 'List{Display}'",
  });
}

test "traits <duplicate methods spec>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ let f: List{Display} = [Foo()]
  \\ let g: List{Foo} = f # contravariance
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "illegal duplicate method",
    "pub def fmt(): String;",
    "Method also declared here",
    "pub def fmt(): String;"
  });
}

test "traits <duplicate methods trait-chain>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def fmt(): num;
  \\ end
  \\
  \\ class Foo: Clone | Display
  \\  pub def fmt()
  \\    return 5
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "duplicate trait method 'fmt'",
    "pub def fmt(): String;",
    "Method already defined here:",
    "pub def fmt(): num;"
  });
}

test "traits <function-bounds>" {
  const src =
  \\ alias String = str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ def format(t: Display, v: Display)
  \\  return t.fmt() <> " $ " <> v.fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ class Bar
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar())
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'Display' but found 'Bar instance'"
  });
}

test "traits <generic-function-bounds .1>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar(), Foo())
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "type 'Bar' does not implement the trait 'Display'",
    "class Bar: Clone",
    "This error was triggered from here:",
    "let r = format(Foo(), Bar(), Foo())",
  });
}

test "traits <generic-function-bounds .2>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Bar: Clone
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let b = Bar()
  \\ let r = format(b, b, b)
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "type 'Bar' does not implement the trait 'Display'",
    "class Bar: Clone",
    "This error was triggered from here:",
    "let r = format(b, b, b)",
  });
}

test "traits <generic-function-bounds .3>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar(), Foo())
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "type 'Bar' does not implement the trait 'Display'",
    "class Bar: Clone",
    "This error was triggered from here:",
    "let r = format(Foo(), Bar(), Foo())",
  });
}

test "traits <generic-function-bounds .4>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone | Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let f = Foo()
  \\ let r = format(f, f, f)
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "type 'Foo' does not implement the trait 'Clone'",
    "class Foo: Display",
    "This error was triggered from here:",
    "let r = format(f, f, f)",
  });
}

test "traits <generic-function-bounds .5>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar(), Foo())
  ;
  try doErrorTest(src, 7, [_][]const u8{
    "type 'Bar' does not implement the trait 'Display'",
    "class Bar: Clone",
    "This error was triggered from here:",
    "let r = format(Foo(), Bar(), Foo())",
    "type 'Foo' does not implement the trait 'Clone'",
    "class Foo: Display",
    "type 'Clone' has no property 'fmt'",
  });
}

test "traits <generic-function-bounds .6>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.clone().fmt() <> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone | Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar(), Bar())
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "type 'Display' has no property 'clone'",
    "return a.clone().fmt() <> \" $ \" <> b.fmt() <> \" $ \" <> c.clone().fmt()"
  });
}

test "traits <generic-function-bounds .7>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> (c.clone() as A).fmt()
  \\ end
  \\
  \\ class Foo: Display | Clone
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone | Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar(), Foo())
  \\ assert(r == "Foo() $ Bar() $ Foo()", 'should be same')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot cast from type 'Clone' to type 'Display'"
  });
}

test "traits <generic without type params .1>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Clone{K}
  \\  def clone(): K;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{num} | Clone
  \\    x = 12
  \\  pub def shift(shr: num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "could not resolve type with name: 'K'",
  });
}

test "traits <generic without type params .2>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Clone{T}
  \\  def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{num} | Clone
  \\    x = 12
  \\  pub def shift(shr: num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "could not resolve type with name: 'T'",
  });
}

test "traits <generic missing traits>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): str
  \\  where
  \\    B: Display + Clone{B},
  \\    C: Display + Clone{C},
  \\  return a.fmt() #<> " $ " <> b.fmt() <> " $ " <> c.clone().fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone{Bar} | Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format{Foo, Bar, Foo}(Foo(), Bar(), Foo())
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "type 'Foo' does not implement the trait 'Clone{C}'",
    "class Foo: Display",
    "This error was triggered from here:",
    "let r = format{Foo, Bar, Foo}(Foo(), Bar(), Foo())",
  });
}

test "traits <match patterns>" {
  const src =
  \\ alias String = str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ class Foo: Display | Clone
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ let f = Foo()
  \\ match f
  \\  case Display() => "oops"
  \\  case Clone() => "oops"
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "bad/ambiguous pattern constructor: 'Display'",
    "bad/ambiguous pattern constructor: 'Clone'",
  });
}

test "builtin @ <vardecl, match>" {
  const src =
  \\ let @foo = 5
  \\ let j = @string(5)
  \\ match 5 
  \\  case {@j: @q} => 5
  \\  case @Foo() => 10
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
  });
}

test "builtin @ <match, orelse>" {
  const src =
  \\ match 5 
  \\  case [@a, @b] => 5
  \\  case (@c, @d) => 10
  \\ end
  \\ let j = foo orelse |@j| pooh
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
  });
}

test "builtin @ <type, alias, function, class, trait>" {
  const src =
  \\ type @Foo = Bar | @Bad
  \\ type Foo = Bar(@fox)
  \\ alias @T = @Foo
  \\ def @oops()
  \\  return "oops"
  \\ end
  \\ class @Bad
  \\  pub def @nooo(): str
  \\    return "yep"
  \\  end
  \\ end
  \\ trait @NotGood
  \\  pub def @yeah(): str;
  \\ end
  ;
  try doErrorTest(src, 10, [_][]const u8{
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
    "cannot use an identifier marked with '@' in this context.",
  });
}

test "builtin @ <assignment>" {
  const src =
  \\ @box = 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<newline>' but found '='",
  });
}
