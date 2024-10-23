const lib = @import("test");
const doErrorTest = lib.doErrorTest;

test "binary operators" {
  const src =
  \\ _ = 'fox' + 5
  \\ _ = False - 'foobar'
  \\ _ = [] * ()
  \\ _ = None / {'x': 5}
  \\ _ = ~False / {'x': 5}
  \\ _ = None ^ True
  \\ _ = 'fin' > True
  \\ _ = [('1', 2, 'a')] < ('oops')!
  \\ _ = ([1, 2])! <= ('oops')!
  \\ _ = False >= None
  \\ _ = [('1', 2, 'a')] == ('oops')!
  \\ _ = 'mist' != 6
  \\ let q = List{Num} + Str
  ;
  try doErrorTest(src, 13, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'Str' + 'Num'",
    "Expected type 'Num' - 'Num' but found 'Bool' - 'Str'",
    "Expected type 'Num' * 'Num' but found 'List{Any}' * 'Tuple{}'",
    "Expected type 'Num' / 'Num' but found 'None' / 'Map{Str, Num}'",
    "Expected type ~ 'Num' but found ~ 'Bool'",
    "Expected type 'Num' ^ 'Num' but found 'None' ^ 'Bool'",
    "Expected type 'Num' > 'Num' but found 'Str' > 'Bool'",
    "Expected type 'Num' < 'Num' but found 'List{Tuple{Str, Num, Str}}' < 'Error(Str)'",
    "Expected type 'Num' <= 'Num' but found 'Error(List{Num})' <= 'Error(Str)'",
    "Expected type 'Num' >= 'Num' but found 'Bool' >= 'None'",
    "Types must be related for this operation. 'List{Tuple{Str, Num, Str}}' is not related to 'Error(Str)'",
    "Types must be related for this operation. 'Str' is not related to 'Num'",
    "Expected type 'Num' + 'Num' but found 'Type' + 'Type'",
  });
}

test "builtin properties .1" {
  const src =
  \\ let j = (1, 2)
  \\ j[0] += 5
  \\ let p = {'b': 5 as Num | Str}
  \\ p[5]
  \\ let p = []
  \\ p[1]
  \\ p = [1, 2]
  \\ p["fox"]
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Cannot mutate immutable type 'Tuple{Num, Num}'",
    "Expected type 'Num' | 'Num' but found 'Num' | 'Type'",
    "Cannot index 'List{Any}' type with type 'Str'",
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
    "Cannot mutate immutable type 'Tuple{Num, Num}'",
    "Cannot index type 'Map{Str, Num}' with type 'Num'",
    "Cannot index 'List{Any}' type with type 'Str'",
  });
}

test "builtin generics" {
  const src =
  \\ let j: List{} = []
  \\ let k: Map{Str} = {1: 1}
  \\ let i: Tuple{Num, Bool} = (False,)
  \\ let x: Error{Num, list{Num}} = (56)!
  \\ alias T{K} = List{K{T}}
  \\ type T{K} = List{K{T}}
  \\ type X{K} = Num | Str | K{Bool}
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "empty type parameters are not supported",
    "generic type instantiated with wrong number of paramters. Expected 1 but found 0",
    "generic type instantiated with wrong number of paramters. Expected 2 but found 1",
    "type variable in generic parameter cannot be generic",
    "expected token '<ident>' but found 'List'",
    "expected token '<ident>' but found 'Num'",
  });
}

test "casting.<regular>" {
  const src =
  \\ let j = (1, 2)
  \\ _ = j as Tuple{Str, Num}
  \\ let j = {'a': 5} as Map{Str, Tuple{Num, Bool}}
  \\ let t = []
  \\ _ = t as List{Str}
  \\ let j: Any = 5
  \\ _ = (j as Num) + 6
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Cannot cast from type 'Tuple{Num, Num}' to type 'Tuple{Str, Num}'",
    "Cannot cast from type 'Map{Str, Num}' to type 'Map{Str, Tuple{Num, Bool}}'",
    "Cannot cast from type 'List{Any}' to type 'List{Str}'",
    "Cannot cast from type 'Any' to type 'Num'",
  });
}

test "casting.<active types>" {
  const src =
  \\ type H = A | B
  \\ type H20 = X(List{Tuple{Num}}) | Y(List{List{Str}})
  \\ let j: H = A
  \\ let p = j as B
  \\ println(p)
  \\ let j: H20 = Y([['a'], ['b']])
  \\ let p = j as H20.X
  \\ println(p)
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Cannot cast from type 'H' to type 'B' because the active type is 'A'",
    "Cannot cast from type 'H20' to type 'H20.X' because the active type is 'Y(List{List{Str}})'"
  });
}

test "Never .1" {
  const src =
  \\ def foo(): Never
  \\  println("oops")
  \\ end
  \\
  \\ foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit; function declared type 'Never' returns",
  });
}

test "Never .2" {
  const src =
  \\ def foo()
  \\  foo()
  \\ end
  \\
  \\ let j: Never = foo()
  \\ j = 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot assign type 'Num' to type 'Never'",
  });
}

test "Never .3" {
  const src =
  \\ def noret: Never
  \\  5
  \\ end
  \\ noret()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit; function declared type 'Never' returns",
  });
}

test "Never & Unit .1" {
  const src =
  \\ def foo(): Never
  \\  foo()
  \\ end
  \\ let j: Unit = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'Unit' with type 'Never'",
  });
}

test "Never & Unit .2" {
  const src =
  \\ def foo(): Never
  \\  3
  \\ end
  \\ let j: Unit = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Control flow reaches exit; function declared type 'Never' returns",
  });
}

test "Never & Unit .3" {
  const src =
  \\ def foo(): Never
  \\  @exit(12)
  \\ end
  \\ let j: Unit = foo()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'Unit' with type 'Never'",
  });
}

test "type linking" {
  const src =
  \\ alias HashMap{K, V} = Map{K, V}
  \\ alias Q = Q
  \\ alias StringHashMap{V} = HashMap{Str, J}
  \\ alias NumList = List{X}
  \\ let a: NumList = [1, 2]
  \\ let b: HashMap{Num, Bool} = {0: False}
  \\ let c: StringHashMap{Bool} = {'foo': False}
  \\ let x: Str = 'over the garden wall!'
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
  \\ type Q = A(T) | B(S) | C(Any)
  \\ let j: Q = C(5)
  \\ j += j
  \\ let t: List{(Num?)?} = [Just(Just(5) as Num?) as (Num?)?, None, None]
  \\ t * t
  \\ t.?
  ;
  try doErrorTest(src2, 3, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'Q' + 'Q'",
    "Expected type 'Num' * 'Num' but found 'List{Just(Just(Num) | None) | None}' * 'List{Just(Just(Num) | None) | None}'",
    "Types must be related for this operation. Narrowed type 'List{Just(Just(Num) | None) | None}' is not related to 'None'",
  });
}

test "conditionals" {
  const src =
  \\ if 3 then
  \\ end
  \\ while [{},] do
  \\ end
  \\ Str is Num
  \\ 5 is False
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected condition expression to be of type 'Bool' but found 'Num'",
    "Expected condition expression to be of type 'Bool' but found 'List{Map{Any, Any}}'",
    "Expected type instance in lhs of `is` operator but found 'Type'",
    "Expected type 'Type' in rhs of `is` operator but found type 'False'\n    Help: For constant types, consider using '==' or '!=' operator instead.",
  });
}

test "circularity" {
  const src =
  \\ type T{P} = A1(Str) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{Num})
  \\ let p: T{Str} = A1('fox')
  \\ p = B1(A2(5) as S{Str})
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot cast from type 'A2(Num)' to type 'S{Str}'",
  });
}

test "circularity-2" {
  const src =
  \\ type T{P} = A1(P) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{K})
  \\ let p: T{Str} = A1('fox')
  \\ p = B1(A2(5) as S{Num})
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot assign type 'B1(A2(Num) | B2(A1(Num) | B1({...})))' to type 'T{Str}'",
  });
}

test "narrowing-1" {
  const src =
  \\ type StrNum = Ext(Str) | Zen(Num)
  \\ let x: StrNum? = Just(Ext('foobar') as StrNum)
  \\ let p = 10
  \\ match x
  \\  case Just(Ext(f)) => f > p
  \\  case Just(Zen(q)) => 'yodo'
  \\  case None => do
  \\    type LM = L(List{Num}) | M(Map{Ext, Zen})
  \\    let x: LM = L([5])
  \\    let p = 10
  \\    if x is LM.L
  \\      x += p
  \\    end
  \\  end
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'Num' > 'Num' but found 'Str' > 'Num'",
    "Expected type 'Num' + 'Num' but found 'L(List{Num})' + 'Num'",
  });
}

test "narrowing-2" {
  const src =
  \\ type T = L(List{Num}) | M(Map{Str, Num})
  \\ let x: T = L([5])
  \\ if x is T.L
  \\ elif x is T.M
  \\ else
  \\  ~x 
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type ~ 'Num' but found ~ 'Never'",
  });
}

test "narrowing-3" {
  const src =
  \\ let x: Num? = Just(9)
  \\ if x is not Num and x is Num
  \\   x + 5
  \\ else 
  \\   0+x
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'Just(Num) | None' + 'Num'",
    "Expected type 'Num' + 'Num' but found 'Num' + 'Just(Num) | None'",
  });
}

test "narrowing-4" {
  const src =
  \\ let x: Num? = Just(9)
  \\ if x is not Num and x is Num
  \\   x + 5
  \\ elif x is not Num
  \\   0+x
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Types must be related for this operation. Narrowed type 'Just(Num) | None' is not related to 'Num'",
    "elif x is not Num",
  });
}

test "narrowing-5" {
  const src =
  \\ let x: Tuple{List{Tuple{Num, List{Num}}}, Num} = ([(9, [] as List{Num})], 5)
  \\ let p = 0
  \\ if x[0] is List{Tuple{Num, List{Num}}}
  \\    if x[0][0] is Tuple{Num, List{Num}}
  \\      if x[0][0][1] is List{Num}
  \\        p /= 5
  \\      end
  \\    end
  \\ elif x[1] is Num
  \\    p += x
  \\ else
  \\    x[0]
  \\ end
  \\ p
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'Num' + 'Tuple{List{Tuple{Num, List{Num}}}, Num}'",
  });
}

test "narrowing-6.1" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ type ListStr = L(List{NumStr}) | S(Str)
  \\ let x: (ListStr)? = Just(L([NumStr.N(5) as NumStr, NumStr.S('a')]) as ListStr)
  \\ if x.? is L and x.?[0] is Num
  \\   # pass
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Type 'ListStr' is not indexable",
  });
}

test "narrowing-6.2" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ type ListStr = L(List{NumStr}) | S(Str)
  \\ let x: (ListStr)? = Just(L([NumStr.N(5) as NumStr, NumStr.S('a')]) as ListStr)
  \\ if x.? is L
  \\   match x.?
  \\    case L([t, ..]) => t += 5
  \\    case L([..]) => assert(False, 'yay')
  \\   end
  \\ else
  \\    x.?
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'NumStr' + 'Num'",
  });
}

test "narrowing-6.3" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ type ListStr = L(List{NumStr}) | S(Str)
  \\ let x: (ListStr)? = Just(L([NumStr.N(5) as NumStr, ListStr.S('a')]) as ListStr)
  \\ if x.? is L and x.?[0] is Num
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
  \\    x # Num | Str
  \\    y *= 5
  \\ else
  \\    x + y
  \\ end
  \\ y
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected type 'Num' * 'Num' but found 'A | B' * 'Num'",
    "Expected type 'Num' + 'Num' but found 'B | A' + 'A'",
  });
}

test "narrowing-8" {
  const src =
  \\ type NumStr = Zen(Num) | Ext(Str)
  \\ type T = L(List{List{NumStr}}) | M(Map{Str, List{NumStr}})
  \\ let x: T =  L([[Zen(5) as NumStr]])
  \\ let p = 10
  \\
  \\ match x
  \\  case M({'a': [Zen(g), ..]} as m) => if g + 2 > 0xff
  \\    m['a'][0] + 5
  \\  end
  \\  case M(_) => assert(False, '')
  \\  case L(q) => p -= q[0][1]
  \\ end
  \\ assert(p == 5, 'should be 5')
  \\ p
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'NumStr' + 'Num'",
    "m['a'][0] + 5",
    "Expected type 'Num' - 'Num' but found 'Num' - 'NumStr'",
    "case L(q) => p -= q[0][1]",
  });
}

test "narrowing-9" {
  const src =
  \\ type T = A | Ext(Str)
  \\ let x: T = Ext('fox')
  \\ if x is not Ext
  \\    x += 10
  \\ end
  \\ x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'A' + 'Num'",
  });
}

test "narrowing-10" {
  const src =
  \\ type NumStr =
  \\  | N(Num) | S(Str)
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
    "Expected type 'Num' / 'Num' but found 'Never' / 'Never'",
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
    "Expected type 'Num' + 'Num' but found 'Never' + 'Num'",
  });
}

test "narrowing-12" {
  const src =
  \\ class Fox
  \\    x: Num = 5
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
  \\  f += 5 # Never
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'Never' + 'Num'",
  });
}

test "narrowing-13" {
  const src =
  \\ class MyFoo
  \\ end
  \\ 
  \\ if MyFoo() is MyFoo
  \\   assert(True, 'yay')
  \\ else
  \\   assert(False, 'nay')
  \\ end
  \\ 1 / ''  # propagate warning 
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot narrow type at expression",
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
  \\ type NumStr = N(Num) | S(Str)
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
  \\ while True do
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
  \\ while True do
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
  \\ while True do
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
  \\ def fun(x: Num)
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

test "dca-10.<recursive>" {
  const src =
  \\ # mutually recursive
  \\ def mutA{U}(x: U)
  \\  return mutB(x)
  \\ end
  \\
  \\ def mutB{T}(y: T)
  \\  return mutA(y)
  \\ end
  \\ 
  \\ mutA(10)
  \\ mutB('b')
  \\
  \\ def mutA(x: Num)
  \\  if x > 2
  \\    return mutB(x)
  \\  else
  \\    return x
  \\  end
  \\ end
  \\
  \\ def mutB(y: Num)
  \\  if y > 2
  \\    return mutA(y)
  \\  else
  \\    return y
  \\  end
  \\ end
  \\ mutA(10) + mutB(7)
  \\
  \\ # recursive
  \\ def mutMe(x: Str)
  \\  return mutMe('5')
  \\ end
  \\ mutMe('fox')
  \\
  \\ alias J =  Tuple{Num, Str}
  \\ def fox{A, B}(x: A, y: B)
  \\  let p: J = (13, '4')
  \\  return fox(x, y)
  \\ end
  \\ fox('a', None)
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "mutB('b')",
  });
}

test "dca-11.<recursive>" {
  const src =
  \\ # mutually recursive
  \\ def mutA(x: Num)
  \\  if x > 2
  \\    return mutB(x)
  \\  else
  \\    return x
  \\  end
  \\ end
  \\
  \\ def mutB(y: Num)
  \\  if y > 2
  \\    return mutA(y)
  \\  else
  \\    return y
  \\  end
  \\ end
  \\ mutA(10) + mutB(7)
  \\
  \\ # recursive
  \\ def mutMe(x: Str)
  \\  return mutMe('5')
  \\ end
  \\ mutMe('fox')
  \\
  \\ alias J =  Tuple{Num, Str}
  \\ def fox{A, B}(x: A, y: B)
  \\  let p: J = (13, '4')
  \\  return fox(x, y)
  \\ end
  \\ fox('a', None)
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Dead code: control flow never reaches this code",
    "alias J =  Tuple{Num, Str}",
  });
}

test "constant types" {
  const src =
  \\ let x: 5 = 5
  \\ let y: 7 = 7
  \\ y * x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' * 'Num' but found '7' * '5'",
  });
}

test "functions-1" {
  const src =
  \\ alias T = Num
  \\ def j(a: T): T
  \\  return (a * 2)
  \\ end
  \\ j('eeeek')
  \\ j{T}(5)
  \\ def funny
  \\    def foo{T}(a: T): T
  \\     return a
  \\    end
  \\    let j = foo{Str}('5')
  \\    let k = foo(10)
  \\    let p = foo(56)
  \\    k += 5
  \\    return (j, k, p)
  \\ end
  \\ funny(12)
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Argument type mismatch. Expected type 'T' but found 'Str'",
    "Non-generic function called as generic",
    "Argument arity mismatch. Expected 0 argument(s) but found 1",
  });
}

test "functions-2" {
  const src =
  \\ def foo(a: Num)
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
    "Expected type 'Num' - 'Num' but found 'Never' - 'Never'",
  });
}

test "functions-3" {
  const src =
  \\ def fancy{T}(x: T)
  \\  let j: T = x
  \\  return fancy{T}(j)
  \\ end
  \\ fancy(False)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Non-generic function called as generic",
  });
}

test "functions-4" {
  const src =
  \\ def fancy{T}(x: T): fn{T}(Num): Str
  \\  let j: T = x
  \\  return fancy{T}(j)
  \\ end
  \\ fancy(False)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '(' but found '{'",
  });
}

test "functions-5" {
  const src =
  \\ def fancy{T}(x: T): Str
  \\  let j: T = x
  \\  return def{T}(j: Num) => j
  \\ end
  \\ fancy(False)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "generic lambdas are unsupported",
  });
}

test "functions-6" {
  const src =
  \\ def fun
  \\ end
  \\ def fancy(x: Any)
  \\  return try fun()
  \\ end
  \\ fancy(False)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected error union type in 'try/orelse' expression. Type 'Unit' is not an error union",
  });
}

test "functions-7.<function arguments>" {
  const src =
  \\ def funny(t: List{Str})
  \\  println(t)
  \\ end
  \\ funny([])
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'List{Str}' but found 'List{Any}'",
  });
}

test "error type" {
  const src =
  \\ def fancy(x: Num)
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
    "Expected type 'Num' + 'Num' but found 'Error(Str) | Ok(Num)' + 'Num'",
  });
}

test "simple-classes-1" {
  const src =
  \\ class Fox
  \\    x: Num
  \\    def init(): Unit
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
  \\  y: Str
  \\ end
  \\
  \\ Foo()
  \\
  \\ class Bar
  \\  y: Str
  \\  def init(t: Str)
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
  \\    x: Num
  \\    u = 12
  \\    def init(): Unit
  \\      self.x = 0
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ class Racoon
  \\    x: Num
  \\    u = 12
  \\    def init(): Unit
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
    "Cannot mutate immutable type 'fn (): Fox'",
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
    "Cannot mutate immutable type 'fn (): Num'",
    "j.fish = def => 10"
  });
}

test "field init" {
  const src =
  \\ class Foo
  \\  x: Str
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
    "type 'Map{Str, Num}' has no property 'doesNotExist'",
    "type 'List{Num}' has no property 'lens'",
  });
}

test "generic-classes-2" {
  const src =
  \\ class Fox{T}
  \\    x: List{T}
  \\    def init(x*: T): Unit
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
  \\ let x = Fox{Num}(6, 7, 8)
  \\ let t: Fox{Num} = x
  \\ t.pulse().y
  \\ t.pulse().getGen()((7)!)
  \\ t.pulse().pulse().x.len()[0]
  \\ let q = {'a': 5, 'c': 12}
  \\ q.keys().len() + 5
  \\ q.items()[0].len() - '2'
  \\ x.pulse().getGen()(5)
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Fox{Num}' has no property 'y'",
    "Type 'Error(Num)' is not indexable",
    "Type 'Num' is not indexable",
    "Expected type 'Num' - 'Num' but found 'Num' - 'Str'",
    "Type 'Num' is not indexable"
  });
}

test "generic-classes-3" {
  const src =
  \\ class Fox{T}
  \\    x: List{T}
  \\    def init(x*: T): Unit
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
    "Expected type 'Num' + 'Num' but found 'List{mia}' + 'Num'",
    "Could not resolve type of ident: 'j'",
    "Could not resolve type of ident: 'p'",
    "Expected type 'Num' + 'Num' but found 'List{mia}' + 'Num'",
    "Expected type 'Num' + 'Num' but found 'Num' + 'Str'",
    "Expected type 'Num' + 'Num' but found 'Str' + 'Str'",
    "Argument type mismatch. Expected type 'mia' but found 'Num'",
    "Cannot initialize type 'Poo{miah}' with type 'Fox{mia} instance'"
  });
}

test "generic-classes-4" {
  const src =
  \\ class Foo{T}
  \\   pub def see(x: T): Bar{T}
  \\    return Bar{T}()
  \\   end
  \\ end
  \\
  \\ class Bar{U}
  \\    pub def ees(y: U): Foo{U}
  \\      return Foo{U}()
  \\    end
  \\ end
  \\
  \\ let j = Foo{Num}()
  \\ let t = j.see((5))
  \\ t.ees('oops')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'Num' but found 'Str'"
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
    "Expected type 'Num' + 'Num' but found 'Num' + 'Str'",
    "Expected type 'Num' + 'Num' but found 'AB' + 'Num'",
  });
}

test "labeled argument" {
  const src =
  \\ def fun(x: Str, y: Num, a: List{Num}, b: Result{Any, Str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y='ops', a=6, x=[6], ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'Str' but found 'List{Num}'",
  });
}

test "labeled argument 2" {
  const src =
  \\ def fun(x: Str, y: Num, a: List{Num}, b: Result{Any, Str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y='ops', a=6, x='ok', ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'Num' but found 'Str'",
  });
}

test "labeled argument 3" {
  const src =
  \\ def fun(x: Str, y: Num, a: List{Num}, b: Result{Any, Str})
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y=5, a=6, x='ok', ('oops')!)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'List{Num}' but found 'Num'"
  });
}

test "labeled argument 4" {
  const src =
  \\ def fun(x: Str, y: Num, a*: List{Num})
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
  \\ def fun(x: Str, y: Num, a*: List{Num})
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
  \\ def fun(x: Str, y: Num, a: List{Num}, b: Result{Any, Str})
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

test "patterns-0.<empty match>" {
  const src =
  \\ match ('a', 'b')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "match statement missing case arms"
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
    "Tuple(_, Str)",
    "Tuple(Str, _)"
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
    "Cannot assign type 'Num' to type 'Str'",
  });
}

test "patterns-3.<nested match>" {
  const src =
  \\ let z = False
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
  \\    z = True
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

test "patterns-3.1.<exhaustiveness>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (None, None) => 0
  \\  case (None, _) => 1
  \\  case (_, None) => 2
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, Just(Num))",
  });
}

test "patterns-3.2.<redundancy>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (None, None) => assert(False, 'a')
  \\  case (Just(a), Just(b)) => assert(True, 'b')
  \\  case (_, Just(_)) => assert(False, 'red')
  \\  case (None, _) => assert(False, 'c')
  \\  case (_, None) => assert(False, 'd')
  \\ end
  \\ 1/''
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case (None, _) => assert(False, 'c')",
  });
}

test "patterns-3.3.<redundancy>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (None, None) => 0
  \\  case (None, _) => 1
  \\  case (_, None) => 2
  \\  case (None, Just(_)) => 3
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case (None, Just(_)) => 3",
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
    "Tuple(_, Str)",
    "Tuple(Str, _)"
  });
}

test "patterns-8.<list exhaustiveness>" {
  const src =
  \\ let z = False
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => println('first')
  \\  case [('a', 'b' as o)] as d => z = True
  \\  case [('q', 'k')] => println('third')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, Str)",
    "Tuple(Str, _)",
    "List{Tuple{Str, Str}}"
  });
}

test "patterns-9.<match redundancy>" {
  const src =
  \\ let z = False
  \\ class Cat
  \\ end
  \\ class Dog
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ let z = False
  \\ match p
  \\  case D(Dog()) => println('good')
  \\  case D(Dog()) => println('hmm')
  \\  case C(Cat()) => z = True
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
  \\ let z = False
  \\ match p
  \\  case D(Dog()) => println('hmm')
  \\  case C(Cat('fox')) => println('nope')
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' but found 'Str'"
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
  \\ let z = False
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
  \\ let z = False
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
  \\ let z = False
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
  \\ let z = False
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
  \\ let z = False
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
  \\ let z = False
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
  \\ let z = False
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

test "patterns-15.<match on Bool (exhaustiveness)>" {
  const src =
  \\ match (1 < 2)
  \\  case False => println('nay')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "True",
  });
}

test "patterns-16.<match on Bool (exhaustiveness)>" {
  const src =
  \\ match (1 < 2)
  \\  case True => println('nay')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "False",
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
    "Num",
  });
}

test "patterns-18.<redundancy>" {
  const src =
  \\ match True
  \\  case False => println('nay')
  \\  case True => println('yay')
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
  \\ let z = False
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{Str,Num} = F1(Fox('pin'))
  \\ match j
  \\  case F2(Fox(6)) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\  case F1(Fox('pin')) as x => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Fox(Str)",
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
  \\ let j: Fx{Str, Num, Str} = F4(Ant{Str}())
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F2(Fox([6])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "F3(Fox{Str}) | F4(Ant{Str})",
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
  \\ let j: Fx{Str, Num} = F1(Fox{Str}('pin', 'pan'))
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F1(Fox(_)) => println('caught ya Str')
  \\  case F2(Fox([6])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case F1(Fox(_)) => println('caught ya Str')"
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
  \\ let j: Fx{Str, Num, Str} = F3(Ant{Str}())
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
  \\ let j = True
  \\ match j
  \\   case False as k => println(j, k)
  \\   case _ as t => match t
  \\      case True => println("B.1")
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
  \\  url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Ext(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Ext("txt")}
  \\ match foo
  \\  case {"sound": _, "format": _} => println(1)
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Ext("txt"),} => println(12, url, a, b)
  \\  case {..} => println('default')
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case {\"sound\" as a: Class(Fox(url)) as b, \"format\": Ext(\"txt\"),} => println(12, url, a, b)"
  });
}

test "patterns-29.<match on maps (exhaustiveness)>" {
  const src =
  \\ class Fox
  \\  url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Ext(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Ext("txt")}
  \\ match foo
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Ext("ogg"),} => println(12, url, a, b)
  \\  case {"sound": _, "format": _} => println(1)
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Map{Str, _}",
  });
}

test "patterns-29.<match on maps (exhaustivenes 2)>" {
  const src =
  \\ class Fox
  \\  url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Ext(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Ext("txt")}
  \\ match foo
  \\  case { a: Class(Fox('url')) as b} => println(12, url, a, b)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Fox(Str)",
    "Map{_, Class(Fox) | Ext(Str)}",
  });
}

test "patterns-29.<match on maps (exhaustivenes 3)>" {
  const src =
  \\ class Fox
  \\  url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Ext(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Ext("txt")}
  \\ match foo
  \\  case { a: Class(Fox('url')) as b, x: Ext("ogg"),} => println(12, url, a, b)
  \\ end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Ext(Str)",
    "Map{_, Class(Fox) | Ext(Str)}",
    "Fox(Str)",
  });
}

test "patterns-30.<match on maps (redundancy)>" {
  const src =
  \\ class Fox
  \\  url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Ext(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Ext("txt")}
  \\ match foo
  \\  case {..} => println('default')
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Ext("ogg"),} => println(12, url, a, b)
  \\  case {"sound": _, "format": _} => println(1)
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case {\"sound\" as a: Class(Fox(url)) as b, \"format\": Ext(\"ogg\"),} => println(12, url, a, b)",
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
  \\ def fib(n: Num)
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
    "Num",
  });
}

test "patterns-32.<match in functions>" {
  const src =
  \\ def check(n: Num)
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
    "cannot return multiple types: 'Num & Unit'",
    "Expected type 'Num' + 'Num' but found 'Num & Unit' + 'Num'"
  });
}

test "cfa.<Unit returns>" {
  const src =
  \\ def check(n: Num)
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
    "Expected type 'Num' + 'Num' but found 'Num & Unit' + 'Num'",
  });
}

test "cfa-2.<Unit returns>" {
  const src =
  \\ def check(n: Num)
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
    "Expected type 'Num' + 'Num' but found 'Num & Unit' + 'Num'",
  });
}

test "patterns-33.<match in functions>" {
  const src =
  \\ def check(n: Num)
  \\  match n
  \\   case 1..4 => return 'a'
  \\   case 5..9 => return True
  \\    case 11..50 as q => println('yep')
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let j = check(12)
  \\ j += 4
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot return multiple types: 'Str & Bool & Num & Unit'",
    "Expected type 'Num' + 'Num' but found 'Str & Bool & Num & Unit' + 'Num'"
  });
}

test "patterns-34.<match in functions>" {
  const src =
  \\ def check(n: Num): Num
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
    "Control flow reaches exit from this point without returning type 'Num'",
  });
}

test "patterns-35.<match in functions>" {
  const src =
  \\ def check(n: Num): Num
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
    "Expected return type 'Num' but found 'Str'",
  });
}

test "patterns-36.<inexhaustive rested patterns>" {
  const src =
  \\ let z = False
  \\ match ('a', False, 'b', True, 'c', False)
  \\  case ('x', _, 'c', _, ..) => println('has keys "x" and "c"')
  \\  case ('a', _, 'b', _ as p, ..) if z => println(z)
  \\  case (..) if !z => println('has something or none')
  \\ end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tuple(_, _, Str, _, _, _)",
    "Bool",
    "Tuple(Str, _, _, _, _, _)"
  });
}

test "patterns-37.<inexhaustive rested patterns>" {
  const src =
  \\ let z = False
  \\ match {'a': False, 'b': True, 'c': False}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} if !z => println('has something or none')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Map{Str, _}",
  });
}

test "patterns-38.<inexhaustive patterns>" {
  const src =
  \\ def goodOrBad(n: Num)
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
    "Ok(Num)",
  });
}

test "patterns-39.<inexhaustive patterns>" {
  const src =
  \\ def goodOrBad(n: Num)
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
    "Error(Str)",
    "Ok(Num)",
  });
}

test "patterns-40.<annotated tags>" {
  const src =
  \\ type T = Tag(a: Num, b: Str, c: Bool)
  \\  match Tag(a=5, b='oops', c=False)
  \\    case Tag(a=5, b=6, c=7) => println('yay')
  \\  end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Str' but found 'Num'",
  });
}

test "patterns-41.<annotated tags>" {
  const src =
  \\ type T = Tag(a: Num, b: Str, c: Bool)
  \\  match Tag(a=5, b='oops', c=False)
  \\    case Tag(a=5, b='oops', c=False) => println('yay')
  \\  end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tag(_, _, True)",
    "Tag(_, Str, _)",
    "Tag(Num, _, _)",
  });
}

test "patterns-42.<annotated tags>" {
  const src =
  \\ type T = Tag(a: Num, b: Str, c: Bool)
  \\  match Tag(a=5, b='oops', c=False)
  \\    case Tag(a=7, b='nope', d=True) => println('nay')
  \\  end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'T' has no field 'd'",
    "inexhaustive pattern match",
    "Remaining pattern type(s):",
    "Tag(_, Str, _)",
    "Tag(Num, _, _)"
  });
}

test "patterns-43.<annotated tags exhaustiveness>" {
  const src =
  \\ type T = Tag(a: Num, b: Str, c: Bool)
  \\  match Tag(a=5, b='oops', c=False)
  \\    case Tag(a=5, b='oopsy', c=False) => println('nay')
  \\    case Tag(a=_, b=_, c=True) => println('nay')
  \\  end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Tag(_, _, False)",
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
    "Expected type '5' but found 'Num'",
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
    "Expected type '5' but found 'Num'",
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

test "patterns-45b.<redundancy in tuples>" {
  const src =
  \\ let tuple = ('a', 'b', 'c')
  \\ match tuple
  \\   case ('a', '1', 'c') => 'one' |> println
  \\   case (..) => 'two' |> println
  \\   case _ => 'three' |> println
  \\ end
  \\ 1 / '1' # error propagation since we're testing a warning
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => 'three' |> println"
  });
}

test "patterns-45c.<redundancy in tuples>" {
  const src =
  \\ let tuple = (1, 2, 3)
  \\ match tuple
  \\   case (1, ..) => 'one' |> println
  \\   case (..) as t => match t
  \\    case (..) => 'ea' |> println
  \\    case _ => 'rest' |> println
  \\   end
  \\   case _ => 'other' |> println
  \\ end
 \\ 1 / '1'
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case _ => 'rest' |> println",
    "possible redundant case",
    "case _ => 'other' |> println",
  });
}

test "patterns-45d.<redundancy in list>" {
  const src =
  \\ let list = [1, 2, 3]
  \\ match list
  \\   case [1,] => 'one' |> println
  \\   case [..t] => ('two', t) |> println
  \\   case _ => 'else' |> println
  \\ end
  \\ 1 / '1'
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case _ => 'else' |> println",
  });
}

test "patterns-45e.<redundancy in list>" {
  const src =
  \\ let list = [1, 2, 3]
  \\ match list
  \\   case [1,] => 'one' |> println
  \\   case [..t] => match t
  \\    case [..] => 'ea' |> println
  \\    case _ => 'rest' |> println
  \\   end
  \\   case _ => 'other' |> println
  \\ end
  \\ 1 / '1'
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case _ => 'rest' |> println",
    "possible redundant case",
    "case _ => 'other' |> println",
  });
}

test "patterns-45f.<redundancy in list (False +ve)>" {
  const src =
  \\ let list = [1, 2, 3]
  \\ match list
  \\   case [1,] => 'one' |> println
  \\   case [..t] => match t
  \\    case [..] => 'ea' |> println
  \\   end
  \\   case _ => 'other' |> println
  \\ end
  \\ 1 / '1'
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "possible redundant case",
    "case [..] => 'ea' |> println", // False +ve
    "possible redundant case",
    "case _ => 'other' |> println",
  });
}

test "patterns-45g.<redundancy in list (False +ve)>" {
  const src =
  \\ let list = [1, 2, 3]
  \\ match list
  \\   case [1,] => 'one' |> println
  \\   case [..t] => match t
  \\    case [..] => 'ea' |> println
  \\   end
  \\ end
  \\ 1 / '1'
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "possible redundant case",
    "case [..] => 'ea' |> println", // False +ve
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
  \\ type Season = Spring | Summer | Autumn | Winter | Mag(Str, Num, Str, Num)
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
    "Mag(_, _, _, Num)",
    "Mag(_, _, Str, _)",
    "Mag(_, Num, _, _)",
    "Mag(Str, _, _, _)",
  });
}

test "patterns-47.<missing patterns>" {
  const src =
  \\ class Panda
  \\  pub x: Num
  \\  pub y: Str
  \\  def init(a: Num, b: Str)
  \\    self.x = a
  \\    self.y = b
  \\  end
  \\ end
  \\ type Legion = Legion(Str, Num, Panda, Bool)
  \\ let j = Legion('a', 5, Panda(5, 'boy'), True)
  \\ match j
  \\  case Legion('a', 5, Panda(5, 'boy'), True) => println('you rock!')
  \\ end
  ;
 try doErrorTest(src, 7, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Legion(_, _, _, False)",
    "Panda(_, Str)",
    "Panda(Num, _)",
    "Legion(_, Num, _, _)",
    "Legion(Str, _, _, _)"
  });
}

test "patterns-47b.<missing patterns>" {
  const src =
  \\ class Panda
  \\  pub x: Num
  \\  pub y: Str
  \\  def init(a: Num, b: Str)
  \\    self.x = a
  \\    self.y = b
  \\  end
  \\ end
  \\ type Legion = Legion(Str, Num, Str, Bool)
  \\ let j = Legion('a', 5, 'boy', True)
  \\ match j
  \\  case Legion('a', 5, 'boy', True) => println('you rock!')
  \\ end
  ;
 try doErrorTest(src, 6, [_][]const u8{
    "inexhaustive pattern match.",
    "Remaining pattern type(s):",
    "Legion(_, _, _, False)",
    "Legion(_, _, Str, _)",
    "Legion(_, Num, _, _)",
    "Legion(Str, _, _, _)"
  });
}

test "patterns-48.<constant patterns>" {
  const src =
  \\ class F
  \\ end
  \\ 
  \\ type FooBar = A(F) | B(Num)
  \\ 
  \\ let a: FooBar = B(12)
  \\ 
  \\ match a
  \\   case A(f) => assert(False, 'nope')
  \\   case B(FOO) => assert(FOO + 5 == 17, 'should be 17')
  \\ end
  \\ 
  \\ const FOO_CONST = 124
  \\
  \\ match 124
  \\   case FOO_CONST => assert(FOO_CONST == 124, 'should be 124')
  \\   case ME_CONST => assert(False, 'oops')
  \\   case _ => assert(False, 'unreachable')
  \\ end
  ;
 try doErrorTest(src, 1, [_][]const u8{
    "Could not resolve type of ident: 'ME_CONST'"
  });
}

test "patterns-49.<constant patterns>" {
  const src =
  \\ const FOO_CONST = 124
  \\
  \\ match 124
  \\   case FOO_CONST => assert(FOO_CONST == 124, 'should be 124')
  \\   case _ME_CONST => assert(False, 'oops')  # not a constant, an identifier
  \\   case _ => assert(False, 'unreachable')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "this variable begins with an '_' which prevents it from being considered as a constant pattern",
    "possible redundant case",
    "case _ => assert(False, 'unreachable')",
  });
}

test "patterns-50.<dot patterns>" {
  const src =
  \\ type MyStuff = A | B | C 
  \\
  \\ match MyStuff.A
  \\   case MyStuff.A => println('a')
  \\   case MyStuff.B => println('b')
  \\ end
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "inexhaustive pattern match",
    "Remaining pattern type(s):",
    "C",
  });
}

test "parse-modes .1" {
  const src =
  \\ class Str
  \\   x: Num
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '<ident>' but found 'Str'",
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
  \\ let x: Str | Num = 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token '=' but found '|'",
  });
}

test "typing.<nullable>" {
  const src =
  \\ let x: Num? = None
  \\ let y: Str? = x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot initialize type 'Just(Str) | None' with type 'Just(Num) | None'",
  });
}

test "typing.<tagged unions>" {
  const src =
  \\ def stup(x: Num)
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
    "expected 'Maybe' type, found 'Error(Str)'",
  });
}

test "typing.<tagged unions 2>" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ def fun(n: NumStr)
  \\  if n is S
  \\    return n
  \\  end
  \\ end
  \\ fun(S('fancy'))
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "cannot return multiple types: 'S(Str) & Unit'",
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
    "token found at an invalid position"
  });
}

test "typing.<tagged unions 4>" {
  const src =
  \\ type Bat = A(List{Fox}, Tuple{Fox}) | B
  \\ type Cat = A(name:Str, age:Num) | B
  \\ alias Fox = Foo
  \\ type BTree{T} = Node(T) | Branch(BTree{T}, BTree{T})
  \\ let tree: BTree{Num} = Branch(Node(1), Node(2))
  \\ alias Foxy = Str
  \\ type Bio = Fox(Num) | Name(Foxy)
  \\ type Pairk{K, V} = Pair(K, V)
  \\ let p:Pairk{Str, Str} = Pair(0, 0)
  \\ type VOrC{V, C} = Vee(V) | Cee(C)
  \\ alias A{K, V} = Map{K, Map{K, VOrC{VOrC{K, V}, K}}}
  \\ 2 as A{Str, Num}
  \\ alias A{K, V} = Maybe{VOrC{K, V}}
  \\ 2 as A{Bool, Str}
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "Cannot initialize type 'Pairk{Str, Str}' with type 'Pair(Num, Num)'",
    "Cannot cast from type 'Num' to type 'A{Str, Num}'",
    "Cannot cast from type 'Num' to type 'A{Bool, Str}'"
  });
}

test "typing.<immutable varargs>" {
  const src =
  \\ def fun(x*: Num)
  \\  x[0] = 12
  \\ end
  \\
  \\ fun(2, 3, 4)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot mutate immutable type 'List{Num}'",
  });
}

test "typing.<type and tag collision>" {
  const src =
  \\ type J = J(Str) | T(Num)
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
  \\ type T = A(Str)
  \\ let j = A()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument arity mismatch. Expected 1 argument(s) but found 0",
  });
}

test "tags.<duplicate>" {
  const src =
  \\ type T = A(x:Str, y:Num)
  \\ let j = A(x='a', x=5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "duplicate labeled argument found",
  });
}

test "tags.<label incomplete>" {
  const src =
  \\ type T = A(x:Str, Num)
  \\ let j = A(j='a', 5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type with one or more labeled argument(s) must be completely labeled",
  });
}

test "tags.<label>" {
  const src =
  \\ type T = A(x:Str, y:Num)
  \\ let j = A(j='a', 5)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "illegal or invalid label: 'j'",
  });
}

test "aspec.<fields 1>" {
  const src =
  \\ class Foo
  \\  x: Num = 19
  \\  y = 11
  \\ end
  \\ let j = Foo()
  \\ j.x + j.y
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Access of private field 'x' outside its defining class",
    "Access of private field 'y' outside its defining class",
  });
}

test "aspec.<fields 2>" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{Num}
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
    "Access of private field 'x' outside its defining class",
  });
}

test "aspec.<fields 3>" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{Num}
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
    "Access of private field 'j' outside its defining class",
  });
}

test "aspec.<fields 4>" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{Num}
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
    "Access of private field 'j' outside its defining class",
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
    "Access of private method 'fun' outside its defining class",
  });
}

test "aspec.<methods 2>" {
  const src =
  \\ class Fish
  \\  x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 0
  \\    self.j = [] as List{Num}
  \\  end
  \\
  \\  def fox()
  \\    print((def () => self.x)())
  \\    return 5
  \\  end
  \\ end
  \\
  \\ Fish()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Access of private field 'x' outside its defining class",
    "print((def () => self.x)())",
  });
}

test "aspec.<methods 2.5>" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 0
  \\    self.j = [] as List{Num}
  \\  end
  \\
  \\  def doom()
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
  try doErrorTest(src, 2, [_][]const u8{
    "Access of private method 'fox' outside its defining class",
    "print((def () => self.fox())())",
  });
}

test "aspec.<methods 3>" {
  const src =
  \\ class Fish
  \\  x: Str
  \\  y: Num
  \\  j: List{Num}
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
  \\ type Cat = A(Str) | B(Str)
  \\ type Dog = A(Num) | B(Str)
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
    "Expected type 'Num' + 'Num' but found 'T' + 'Num'",
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

test "binary tree" {
  const src =
  \\ type Tree{a} =
  \\  | Node(val: a, lhs: Tree{a}, rhs: Tree{a})
  \\  | Empty
  \\ def print_tree{a}(tree: Tree{a})
  \\  match tree
  \\    case Empty => println("Empty")
  \\    case Node(val, lhs, rhs) => do
  \\      println("Node:", val)
  \\      print("Left: ")
  \\      print_tree(lhs)
  \\      print("Right: ")
  \\      print_tree(rhs)
  \\    end
  \\  end
  \\ end
  \\ let tree = Node(
  \\    1,
  \\    Node (
  \\      2,
  \\      Node (3, Empty, Empty),
  \\      Node (4, Empty, Empty)
  \\    ),
  \\    Node (
  \\      5,
  \\      Node (6, Empty, Empty),
  \\      Node (7, Empty, Empty)
  \\    )
  \\  )
  \\ print_tree(tree as Tree{Str})
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot cast from type 'Node(Num, Node(Num, {...}, {...}) | Empty, Node(Num, {...}, {...}) | Empty)' to type 'Tree{Str}'",
  });
}

test "parameterized access" {
  const src =
  \\ type T{K, V} = K
  \\ let x: T{Str, Num} = T.K
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
    "Expected type + 'Num' but found + 'Str'",
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
  \\  case 1..6 as p => j = True
  \\  case _ as w => j = False
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token 'end' but found '='",
  });
}

test "traits <required methods .1>" {
  const src =
  \\ trait Display
  \\  pub def fmt(): Str;
  \\ end
  \\
  \\ class Foo: Display
  \\ end
  \\ Foo()
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Display':",
    "The following method(s) are not implemented:",
    "fmt", " : ", "fn (): Str",
  });
}

test "traits <required methods .2>" {
  const src =
  \\ trait Display
  \\  pub def fmt(): Str;
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
    "Expected method 'fn (): Str' but found 'fn (): Num'",
    "pub def fmt()",
  });
}

test "traits <required methods .3>" {
  const src =
  \\ trait Display
  \\  pub def fmt(): Str
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
    "Expected method 'fn (): Str' but found 'fn (): Num'",
    "pub def fmt()",
  });
}

test "traits <required methods .3.5>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{Num} | Clone{Stuff}
  \\    x = 12
  \\  pub def clone()
  \\    return 'Stuff()'
  \\  end
  \\  pub def shift(shr: Num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  \\ println(s.shift(2))
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected method 'fn (): Stuff' but found 'fn (): Str",
    "pub def clone()",
    "This error was triggered from here:",
    "class Stuff: Shifts{Num} | Clone{Stuff}"
  });
}

test "traits <required methods .4>" {
  const src =
  \\ alias String = Str
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
    "fmt", " : ", "fn (): Str",
    "clone", " : ", "fn (): Clone",
  });
}

test "traits <required methods .5>" {
  const src =
  \\ alias String = Str
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
    "fmt", " : ", "fn (): Str",
    "clone", " : ", "fn (): Foo",
  });
}

test "traits <required methods .6>" {
  const src =
  \\ alias String = Str
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
  \\ alias String = Str
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
    "fmt", " : ", "fn (): Str",
    "show", " : ", "fn (): Str",
  });
}

test "traits <required methods .8>" {
  const src =
  \\ alias String = Str
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
    "fmt", " : ", "fn (): Str",
  });
}

test "traits <required methods .9>" {
  const src =
  \\ alias String = Str
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
    "show", " : ", "fn (): Str",
  });
}

test "traits <required methods .10>" {
  const src =
  \\ alias String = Str
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

test "traits <required methods .11>" {
  const src =
  \\ class Range: Iter{Num}
  \\    curr = 0
  \\    start: Num
  \\    stop: Maybe{Num}
  \\    step: Maybe{Num}
  \\
  \\    def init(start: Num, stop: Maybe{Num}, step: Maybe{Num})
  \\      self.start = start
  \\      self.stop = stop
  \\      self.step = step
  \\    end
  \\
  \\    def get_step()
  \\      return match self.step
  \\        case Just(s) => s
  \\        case None => 1
  \\      end
  \\    end
  \\
  \\    pub def iter()
  \\      return self
  \\    end
  \\ end
  \\
  \\ let range = Range(1, Just(12), Just(2))
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Expected method 'fn (): Iterator{Num}' but found 'fn (): Range'",
    "pub def iter()",
  });
}

test "traits <required methods .12>" {
  const src =
  \\ class Range: Iter{Num} | Iterator{Num}
  \\    curr = 0
  \\    start: Num
  \\    stop: Maybe{Num}
  \\    step: Maybe{Num}
  \\
  \\    def init(start: Num, stop: Maybe{Num}, step: Maybe{Num})
  \\      self.start = start
  \\      self.stop = stop
  \\      self.step = step
  \\    end
  \\
  \\    def get_step()
  \\      return match self.step
  \\        case Just(s) => s
  \\        case None => 1
  \\      end
  \\    end
  \\
  \\    pub def iter()
  \\      return self
  \\    end
  \\ end
  \\
  \\ let range = Range(1, Just(12), Just(2))
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "type 'Range' does not satisfy the trait constraint(s) of 'Iter{Num} & Iterator{Num}'",
    "class Range: Iter{Num} | Iterator{Num}",
    "The following method(s) are not implemented",
    "next", " : ", "fn (): Just(Num) | None",
  });
}

test "traits <visibility .1>" {
  const src =
  \\ alias String = Str
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
  \\ alias String = Str
  \\
  \\ trait Clone{T}
  \\  def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{Num} | Clone{Stuff}
  \\    x = 12
  \\  pub def clone()
  \\    return Stuff()
  \\  end
  \\  pub def shift(shr: Num)
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
  \\ alias String = Str
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
  \\ alias String = Str
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
  \\ alias String = Str
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

test "traits <duplicate methods trait-chain .1>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def fmt(): Num;
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
    "conflicting trait method 'fmt'",
    "pub def fmt(): Num;",
    "Method already specified here:",
    "pub def fmt(): String;"
  });
}

test "traits <function-bounds>" {
  const src =
  \\ alias String = Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
  \\ alias String = Str
  \\
  \\ trait Clone{K}
  \\  def clone(): K;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{Num} | Clone
  \\    x = 12
  \\  pub def shift(shr: Num)
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
  \\ alias String = Str
  \\
  \\ trait Clone{T}
  \\  def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{Num} | Clone
  \\    x = 12
  \\  pub def shift(shr: Num)
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
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
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
    "type 'Foo' does not implement the trait 'Clone{Foo}'",
    "class Foo: Display",
    "This error was triggered from here:",
    "let r = format{Foo, Bar, Foo}(Foo(), Bar(), Foo())",
  });
}

test "traits <match patterns>" {
  const src =
  \\ alias String = Str
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

test "traits <where bounds>" {
  const src =
  \\ trait Speaks
  \\  def speak(): Str;
  \\ end
  \\
  \\ trait Barks{T}
  \\   where
  \\      T: Speaks
  \\ end
  \\
  \\ class Foo: Barks{Foo}
  \\ end
  \\
  \\ Foo()
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "type 'Foo' does not satisfy the trait constraint(s) of 'Speaks'",
    "class Foo: Barks{Foo}",
    "The following method(s) are not implemented:",
    "speak", " : ", "fn (): Str",
  });
}

test "traits <where without generics>" {
  const src =
  \\ trait Barks
  \\   where
  \\      T: Speaks
  \\ end
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "expected token 'end' but found 'where'",
  });
}

test "traits <generics & resolution .1>" {
  const src =
  \\ trait Speaks
  \\  pub def speak(): Str;
  \\ end
  \\
  \\ trait Barks{T}
  \\   where
  \\      T: Speaks
  \\ end
  \\
  \\ class Foo{T}: Speaks | Barks{Foo{T}}

  \\ end
  \\
  \\ Foo{Num}()
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "type 'Foo{Num}' does not satisfy the trait constraint(s) of 'Speaks'",
    "class Foo{T}: Speaks | Barks{Foo{T}}",
    "The following method(s) are not implemented:",
    "speak", " : ", "fn (): Str",
  });
}

test "traits <generics & resolution .2>" {
  const src =
  \\ trait Speaks
  \\  pub def speak(): Str;
  \\ end
  \\
  \\ trait Barks{T}
  \\   where
  \\      T: Speaks
  \\  pub def bark(): Str;
  \\ end
  \\
  \\ class Foo{T}: Barks{Foo{T}} | Speaks
  \\  pub def speak()
  \\    return "Foo speaking here!"
  \\  end
  \\ end
  \\
  \\ Foo{Num}()
  ;
  try doErrorTest(src, 6, [_][]const u8{
    "type 'Foo{Num}' does not satisfy the trait constraint(s) of 'Barks{Foo{Num}} & Speaks'",
    "class Foo{T}: Barks{Foo{T}} | Speaks",
    "The following method(s) are not implemented:",
    "bark", " : ", "fn (): Str",
  });
}

test "traits <init>" {
  const src =
  \\ trait Oops
  \\  def init(): Str;
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "A trait may not define or specify the method 'init'",
    "def init(): Str;",
  });
}

test "classes <generic-init>" {
  const src =
  \\ class Oops
  \\  def init{T}() end
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Method 'init' cannot be generic.",
    "def init{T}() end",
  });
}

test "classes <generic-method>" {
  const src =
  \\ class Oops{T}
  \\  def bad{T, U}(): Str 5 end
  \\ end
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "Warning: generic methods are experimental and should not be used unless absolutely necessary.",
    "Duplicate generic type variable 'T'",
    "def bad{T, U}(): Str 5 end",
    "Type variable also specified here:",
    "class Oops{T}",
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
  try doErrorTest(src, 3, [_][]const u8{
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
  \\  pub def @nooo(): Str
  \\    return "yep"
  \\  end
  \\ end
  \\ trait @NotGood
  \\  pub def @yeah(): Str;
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
    "token found at an invalid position",
  });
}

test "discard vardecl" {
  const src =
  \\ let _ = j.append
  \\ j.append(5)
  \\ _(1)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "cannot use the identifier '_' in this context.",
  });
}

test "parser recovery" {
  const src =
  \\ trait Speaks
  \\  def speak(): Str;
  \\ end
  \\
  \\ trait Barks
  \\   where
  \\      T: Speaks
  \\ end
  \\ let _ = fox
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "expected token 'end' but found 'where'",
    "cannot use the identifier '_' in this context."
  });
}

test "parameter resolution" {
  const src =
  \\ def iter{U}(itr: Iterator{U})
  \\ end
  \\
  \\ class Foo: Iterator{Num} | Iter{Num}
  \\  state = 0
  \\  data: List{Num}
  \\
  \\  def init(d: List{Num})
  \\    self.data = d
  \\  end
  \\
  \\  pub def fun(x: Iterator{Str})
  \\    _ = x
  \\  end
  \\
  \\  pub def next()
  \\    if self.state >= self.data.len()
  \\      return None
  \\    end
  \\    self.state += 1
  \\    return Just(self.data[self.state - 1])
  \\  end
  \\
  \\  pub def iter()
  \\    return Foo(self.data)
  \\  end
  \\ end
  \\
  \\ let f = Foo([1, 2, 3, 4])
  \\ iter{Str}(f)
  \\ f.fun(f)
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Argument type mismatch. Expected type 'Iterator{Str}' but found 'Foo'",
    "Argument type mismatch. Expected type 'Iterator{Str}' but found 'Foo'",
  });
}

test "duplicate declaration .1" {
  const src =
  \\ class Foo
  \\ end
  \\
  \\ class Foo:Iter{Str}
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "illegal duplicate declaration (class)",
    "class Foo:Iter{Str}",
    "'Foo' is also declared here",
    "class Foo",
  });
}

test "duplicate declaration .2" {
  const src =
  \\ trait Foo
  \\ end
  \\
  \\ trait Foo
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "illegal duplicate declaration (trait)",
    "trait Foo",
    "'Foo' is also declared here",
    "trait Foo",
  });
}

test "duplicate declaration .3" {
  const src =
  \\ trait Foo
  \\ end
  \\
  \\ class Foo
  \\ end
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "illegal duplicate declaration (class)",
    "class Foo",
    "'Foo' is also declared here",
    "trait Foo",
  });
}

test "duplicate declaration .4" {
  const src =
  \\ trait Iterator{U}
  \\  pub def next(): Maybe{U};
  \\ end
  \\
  \\ trait Iter{T}
  \\  pub def iter(): Iterator{T};
  \\ end
  ;
  try doErrorTest(src, 8, [_][]const u8{
    "illegal duplicate declaration (trait)",
    "trait Iterator{U}",
    "'Iterator' is also declared here:",
    "trait Iterator{U}",
    "illegal duplicate declaration (trait)",
    "trait Iter{T}",
    "'Iter' is also declared here:",
    "trait Iter{T}",
  });
}

test "duplicate declaration .5 + error recovery" {
  const src =
  \\ def ListIterator{U}
  \\  pub def next(): Maybe{U};
  \\ end
  \\
  \\ trait Iter{T}
  \\  pub def iter(): Iterator{T};
  \\ end
  ;
  try doErrorTest(src, 8, [_][]const u8{
    "illegal duplicate declaration (function)",
    "def ListIterator{U}",
    "'ListIterator' is also declared here:",
    "class ListIterator{T}: Iterator{T}",
    "illegal duplicate declaration (trait)",
    "trait Iter{T}",
    "'Iter' is also declared here:",
    "trait Iter{T}",
  });
}

test "builtin-functions-override" {
  const src =
  \\ def exit(x: Num)
  \\  return x - 2
  \\ end
  \\
  \\ assert(exit(10) == 8, 'okay')
  \\
  \\ def panic(x: Num)
  \\  return x - 2
  \\ end
  \\
  \\ assert(panic(10) == 8, 'okay')
  \\
  \\ let check = assert
  \\ def assert{T}(t: T)
  \\  check(t, 'nice')
  \\ end
  \\ assert(!!check)
  \\
  \\ def print(x*: Any)
  \\  return x
  \\ end
  \\ check(print('fox', 'fry', 1, 2, 3)[0] == 'fox', 'should be "fox"')
  ;
  try doErrorTest(src, 8, [_][]const u8{
    "illegal duplicate declaration (function)",
    "def assert{T}(t: T)",
    "'assert' is also declared here:",
    "def assert(arg: Bool, msg: Str): Unit",
    "illegal duplicate declaration (function)",
    "def print(x*: Any)",
    "'print' is also declared here:",
    "def print(args*: Any)",
  });
}

test "definite field assignment .1" {
  const src =
  \\ class Foo
  \\  x: Num
  \\
  \\  def init()
  \\    self.x = self.x
  \\  end
  \\ end
  \\
  \\ Foo()
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "This field appears to be used in its own initialization",
    "self.x = self.x",
    "I am unable to deduce that the field 'x' is definitely initialized",
  });
}

test "definite field assignment .2" {
  const src =
  \\ class Foo
  \\  x: Num
  \\
  \\  def init()
  \\    self.x = match 5
  \\      case 1..12 => 10
  \\      case _ => self.x
  \\    end
  \\  end
  \\ end
  \\
  \\ Foo()
  ;
  try doErrorTest(src, 3, [_][]const u8{
    "This field appears to be used in its own initialization",
    "case _ => self.x",
    "I am unable to deduce that the field 'x' is definitely initialized",
  });
}

test "class in init method .1" {
  const src =
  \\ class Foo
  \\  x: Num
  \\
  \\  def init()
  \\    class Fooby
  \\    end
  \\  end
  \\ end
  \\
  \\ Foo()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot define a class in the init method of a class",
    "class Fooby",
  });
}

test "class in init method .2" {
  const src =
  \\ class Foo
  \\  def init
  \\    do
  \\      do
  \\        class Pox
  \\        end
  \\      end
  \\    end
  \\  end
  \\ end
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot define a class in the init method of a class",
    "class Pox",
  });
}

test "function in init method .1" {
  const src =
  \\ class Foo
  \\  x: Num
  \\
  \\  def init()
  \\    def Fooby
  \\    end
  \\  end
  \\ end
  \\
  \\ Foo()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot define a function in the init method of a class",
    "def Fooby",
  });
}

test "function in init method .2" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = (def () => self.x)()
  \\    self.y = 0
  \\    self.j = [] as List{Num}
  \\  end
  \\
  \\  def fox()
  \\    return 5
  \\  end
  \\ end
  \\
  \\ Fish()
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "cannot define a function in the init method of a class",
    "self.x = (def () => self.x)()",
  });
}

test "for loop <scopes>" {
  const src =
  \\ import std.iter 
  \\ let i = 'a'
  \\ let j = 'b'
  \\ let l = [] as List{fn(): Num}
  \\ for i, j in iter.range(1, Just(12), None) do
  \\  _ = l.append(def () => j)
  \\ end
  \\ let expected = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  \\ for idx, fun in l
  \\  _ = assert(fun() == expected[i], 'should be same')
  \\ end
  \\ assert(i == 'a' and j == 'b', 'should be same')
  ;
  try doErrorTest(src, 2, [_][]const u8{
    "Cannot index 'List{Num}' type with type 'Str'",
    "_ = assert(fun() == expected[i], 'should be same')",
  });
}

test "generic call parameter mismatch .1" {
  const src =
  \\ class Fox{T}
  \\   pub x: List{T}
  \\   def init(x*: T): Unit
  \\     self.x = x
  \\   end
  \\   pub def pulse()
  \\     return self
  \\   end
  \\   pub def getGen()
  \\     alias T = Tuple{Str}
  \\     def fun(p: T)
  \\       return p[0]
  \\     end
  \\     return fun
  \\   end
  \\ end
  \\ let j = Fox{'mia'}(9)
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'mia' but found 'Num'",
  });
}

test "generic call parameter mismatch .2" {
  const src =
  \\ class Fox{T}
  \\   pub x: List{T}
  \\   def init(x*: T): Unit
  \\     self.x = x
  \\   end
  \\   pub def pulse()
  \\     return self
  \\   end
  \\   pub def getGen()
  \\     alias T = Tuple{Str}
  \\     def fun(p: T)
  \\       return p[0]
  \\     end
  \\     return fun
  \\   end
  \\ end
  \\ let j = Fox{'mia'}('miah')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Argument type mismatch. Expected type 'mia' but found 'Str'",
  });
}

test "generic call parameter mismatch .3" {
  const src =
  \\ class Fox{T}
  \\    x: List{T}
  \\    def init(x*: T): Unit
  \\      self.x = x
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\    def getGen()
  \\      def fun{T}(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{Num}() # tests empty variadic args
  \\ x.getGen()([1, 2]) |> println
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Access of private method 'getGen' outside its defining class",
  });
}

test "const variables" {
  const src =
  \\ const i = 'a'
  \\ const j = 'b'
  \\ j = i
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Cannot mutate constant type 'Str'",
  });
}

test "module lib leaks" {
  const src =
  \\ import std
  \\ const fs = std.fs
  \\ fs.file.open('goooo.txt', fs.file.Mode.Read)
  \\ _ = fs.open()
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "type '{module lib.veb}' has no property 'open'",
  });
}

test "module privacy" {
  const src =
  \\ import std
  \\ const fs = std.fs
  \\ fs.file.open('goooo.txt', fs.file.Mode.Read)
  \\ fs.file.open_file('px')
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Access of private module property 'open_file' outside its defining module",
  });
}

test "trait operators <ordering .1>" {
  const src =
  \\ class Weight: Ord{Weight}
  \\   val: Num = 0
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\   
  \\   pub def eq(other: Weight)
  \\     return self.val == other.val
  \\   end
  \\ end
  \\
  \\ Weight(12)
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Weight' does not satisfy the trait constraint(s) of 'Ord{Weight}'",
    "The following method(s) are not implemented:",
    "cmp", " : ", "fn (Weight): Greater | Less | Equal",
  });
}

test "trait operators <ordering .1b>" {
  const src =
  \\ class Weight: Ord{Weight}
  \\   val: Num = 0
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\   
  \\   pub def cmp(other: Weight)
  \\     return Ordering.Less
  \\   end
  \\ end
  \\
  \\ Weight(12)
  ;
  try doErrorTest(src, 5, [_][]const u8{
    "type 'Weight' does not satisfy the trait constraint(s) of 'Ord{Weight}'",
    "The following method(s) are not implemented:",
    "eq", " : ", "fn (Weight): Bool",
  });
}

test "trait operators <ordering .2>" {
  const src =
  \\ class Weight
  \\   val: Num = 0
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\ end
  \\
  \\ const w1 = Weight(12)
  \\ const w2 = Weight(10)
  \\ w1 > w2
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' > 'Num' but found 'Weight' > 'Weight'",
  });
}

test "trait operators <ordering .3>" {
  const src =
  \\ class Weight
  \\   val: Num = 0
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\
  \\   pub def eq(other: Weight)
  \\    return self.val == other.val
  \\   end
  \\   
  \\   pub def cmp(other: Weight)
  \\     if self.val > other.val
  \\       return Ordering.Greater
  \\     elif self.val < other.val
  \\       return Ordering.Less
  \\     else
  \\       return Ordering.Equal
  \\     end
  \\   end
  \\ end
  \\ Weight(12) > Weight(10)
  \\ Weight(12) >= Weight(10)
  \\ Weight(12) < Weight(10)
  \\ Weight(12) <= Weight(10)
  ;
  try doErrorTest(src, 4, [_][]const u8{
    "Expected type 'Num' > 'Num' but found 'Weight instance' > 'Weight instance'",
    "Expected type 'Num' >= 'Num' but found 'Weight instance' >= 'Weight instance'",
    "Expected type 'Num' < 'Num' but found 'Weight instance' < 'Weight instance'",
    "Expected type 'Num' <= 'Num' but found 'Weight instance' <= 'Weight instance'",
  });
}

test "string iterator" {
  const src =
  \\ import std.string
  \\ @iter(string.String('foo')) + 5
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'Num' + 'Num' but found 'Iterator{Str}' + 'Num'",
  });
}
