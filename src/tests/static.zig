const tests = @import("test.zig");

const doStaticTest = tests.doStaticTest;

test "functions-2" {
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
