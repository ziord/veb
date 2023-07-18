const tests = @import("test.zig");
const doErrorTest = tests.doErrorTest;
const doRuntimeTest = tests.doRuntimeTest;

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
    "Expected type 'Type' in rhs of `is` operator but found type 'bool'\n\tHelp: For constant types, consider using '==' or '!=' operator instead.",
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
  \\ if x is list
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
  \\ if x is list
  \\ elif x is map
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
  \\ if x is list
  \\    if x[0] is list
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
  \\ let x: (list{num} | str)? = [5]
  \\ if x.? is list and x.?[0] is num
  \\    x.?[0] += 5
  \\ else
  \\    x.?
  \\ end
  \\ x
  ;
  try doErrorTest(src, 1, [_][]const u8{
    "Expected type 'num' - 'num', but got 'str | list{num}' - 'num'",
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
  \\ if x is map 
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
    "Expected error union type in 'try' expression. Type 'void' is not an error union",
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