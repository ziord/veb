const doStaticTest = @import("lib.zig").doStaticTest;

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
  \\ type J =  list{num | str}
  \\ def fox{A, B}(x: A, y: B)
  \\  let p: J = [13 as num | str, 4]
  \\  return fox(x, y)
  \\ end
  \\ fox('a', nil)
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
  \\  exit(1)
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
  \\ type never = never
  \\ def rec(): never
  \\  return rec()
  \\ end
  \\
  \\ rec()
  \\
  \\ def fun(x: num)
  \\  if x > 5
  \\    return 'ok'
  \\  else
  \\    return x - 3
  \\  end
  \\ end
  \\
  \\ let j = fun(5)
  \\ let p: never = rec()
  \\ if j is str
  \\  [j]
  \\ elif j is num
  \\  j *= 9
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
  \\    x: num
  \\    u = 12
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    def pulse()
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
  \\ let p = (j.pop() orelse 0) + 4
  \\ j.append(4)
  \\ let k = (j, p)
  \\ p += k.len()
  \\ 
  \\ let x = {'a': 5, 'b': 6}
  \\ x.keys().len()
  \\ x.values().len()
  \\ x.get('a').? + 12
  \\ let j = [1, 2, 3]
  \\ let p = (j.pop() orelse 0) + 4
  ;
  try doStaticTest(src);
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
  \\      type T = tuple{str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{num}(6, 7, 8)
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
  \\ type Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j:Poo{'mia'} = Fox{'mia'}('mia')
  ;
  try doStaticTest(src);
}
