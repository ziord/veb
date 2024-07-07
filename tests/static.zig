const lib = @import("lib.zig");
const doStaticTest = lib.doStaticTest;
const doParsingTest = lib.doParsingTest;

test "recursive types" {
  const src =
  \\ # simple recursive
  \\ type K = A(str) | B(bool) | C(num)
  \\ alias V = Map{K, V}
  \\ alias V2 = Map{V, V2}
  \\ let p = (1, (2,), 3, (1, 2, [3]))
  \\ let x = {
  \\   { A('abc') as K: { A('abc') as K: C(123) as K, B(true): C(0xff), A('obs'): A('fin') } } as V :
  \\   { C(0.123) as K: { A('abc') as K: C(123) as K, B(true): C(0xff), A('obs'): A('fin') } }
  \\ }
  \\ print(x)
  ;
  try doStaticTest(src);
}

test "functions.<recursive>" {
  var src =
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
  \\ def mutA(x: num)
  \\  if x > 2
  \\    return mutB(x)
  \\  else
  \\    return x
  \\  end
  \\ end
  \\
  \\ def mutB(y: num)
  \\  if y > 2
  \\    return mutA(y)
  \\  else
  \\    return y
  \\  end
  \\ end
  \\ mutA(10) + mutB(7)
  \\
  \\ # recursive
  \\ def mutMe(x: str)
  \\  return mutMe('5')
  \\ end
  \\ mutMe('fox')
  \\
  \\ alias J =  Tuple{num, str}
  \\ def fox{A, B}(x: A, y: B)
  \\  let p: J = (13, '4')
  \\  return fox(x, y)
  \\ end
  \\ fox('a', None)
  ;
  try doStaticTest(src);
}

test "functions.<never>" {
  var src =
  \\ def foo(): never
  \\  foo()
  \\ end
  \\ let j = foo()
  ;
  try doStaticTest(src);
}

test "functions.<noreturn>" {
  var src =
  \\ def foo()
  \\  @exit(1)
  \\ end
  \\ let j:noreturn = foo()
  ;
  try doStaticTest(src);
}

test "functions.<void>" {
  var src =
  \\ def foo()
  \\  print('yay')
  \\ end
  \\ let j:void = foo()
  ;
  try doStaticTest(src);
}

test "functions-12-user-defined-never" {
  var src =
  \\ alias never = never
  \\ def rec(): never
  \\  return rec()
  \\ end
  \\
  \\ rec()
  \\
  \\ def fun(x: num)
  \\  if x > 5
  \\    return S('ok')
  \\  else
  \\    return N(x - 3)
  \\  end
  \\ end
  \\ type NS = N(num) | S(str)
  \\ let j = fun(5)
  \\ let p: never = rec()
  \\ if j is S
  \\  _ = [j]
  \\ elif j is N
  \\  j = N(9)
  \\ else
  \\  p = j
  \\ end
  \\ p
  ;
  try doStaticTest(src);
}

test "simple-classes-1" {
  var src =
  \\ class Fox
  \\    pub x: num
  \\    pub u = 12
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\ end
  \\
  \\ let f = Fox()
  \\ let p = f.x + 5
  \\ assert(f.pulse() == f, "instances should be the same")
  \\ assert(f.pulse().x + 5 == p, "field should be equal")
  \\ let j: Fox = f
  \\ assert(j is Fox, "j should be type Fox")
  \\ assert(j.u == 12, 'field "u" should be 12')
  ;
  try doStaticTest(src);
}

test "generic-classes-1" {
  var src =
  \\ let j = [1, 2, 3]
  \\ let p = (j.pop().?) + 4
  \\ j.append(4)
  \\ let k = (j, p)
  \\ p += k.len()
  \\ 
  \\ let x = {'a': 5, 'b': 6}
  \\ x.keys().len()
  \\ x.values().len()
  \\ x.get('a').? + 12
  \\ let j = [1, 2, 3]
  \\ let p = (j.pop().?) + 4
  ;
  try doStaticTest(src);
}

test "generic-classes-2" {
  var src =
  \\ class Fox{T}
  \\    pub x: List{T}
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\
  \\    pub def getGen()
  \\      alias T = Tuple{str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox(6, 7, 8)
  \\ let t: Fox{num} = x
  \\ t.pulse().x[0] + 12
  \\ t.pulse().getGen()(('starters',))
  \\ t.pulse().pulse().x.len()
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j: Fox{'mia'} = w
  \\ j.pulse().x
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j = Fox{'mia'}('mia')
  \\ j = w
  \\
  \\ alias Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j:Poo{'mia'} = Fox{'mia'}('mia')
  ;
  try doStaticTest(src);
}

test "maximal munch parsing style" {
  const src =
  \\ let t: num? =
  \\  Just(5)
  \\ t.? |> println
  \\ type Foo = 
  \\  | A
  \\  | B
  \\  | C(num)
  \\ type Foo = | A | B | C(num)
  \\ do println('yep') end
  \\ def foo(a: num): str println(yeah) let j = 5 return j end
  \\ let p = def => 5
  \\ p()
  \\ match fox with
  \\  case 1 => do println('yeye') end
  \\ end
  \\ match fox with case 1 => do println('yeye') end end
  \\ if foo let k = 10 else let j = 12 end
  \\ while foo do j = 12 p = [1, 2, 4][j] let t = 12 end
  \\ class T: Foo def init(a: num): str let j = 12 end end
  \\ class U{X, Y}: Foo where X: P + K{V}, Y: P + Q, def init(a: num): str let j = 12 end end
  \\ trait V{X, Y} 
  \\    where 
  \\      X: P + K{V},
  \\      Y: P + Q,
  \\    def inits(a: num): str
  \\      let j = 12
  \\    end
  \\  end
  \\ def foo(a: num): str
  \\  return 12
  \\    |> foo
  \\    |> bar
  \\ end
  ;
  try doParsingTest(src);
}
