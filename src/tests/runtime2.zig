const std = @import("std");
const tests = @import("test.zig");

const value = tests.value;
const doRuntimeTest = tests.doRuntimeTest;

test "functions-1.<narrowing-return>" {
  var src =
  \\ def chee(n: num | str): num
  \\  if n is num
  \\    return n
  \\  end
  \\  return n.len()
  \\ end
  \\ assert(chee('oops') == 4, 'should be 4')
  \\ assert(chee(4) == 4, 'should be 4')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-1.<call & dot access mutation>" {
  var src =
  \\ class Foxy
  \\  x = [1]
  \\ end
  \\
  \\ def fun(f: Foxy)
  \\  return f
  \\ end
  \\
  \\ let k = Foxy()
  \\ fun(k).x = [1, 2, 3]
  \\ assert(k.x.len() == 3 and k.x.get(2).? == 3, 'should be')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-2.<instance list assignment>" {
  var src =
  \\ class Dod
  \\ end
  \\ let j = [Dod()]
  \\ let k: list{Dod} = j
  \\ assert(k.len() == j.len(), 'should be same')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-1" {
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
  \\ assert(t.pulse().x[0] == 6, 'first arg is 6')
  \\ assert(t.pulse().x[1] == 7, 'first arg is 7')
  \\ assert(t.pulse().x[2] == 8, 'first arg is 8')
  \\ # print(t.x)
  \\ assert(t.x[0] == 6, 'first arg is 6')
  \\ assert(t.x[1] == 7, 'second arg is 7')
  \\ assert(t.x[2] == 8, 'third arg is 8')
  \\ let z = Fox{'mia'}('mia')
  \\ assert(z.x[0] == 'mia', 'index 0 gives mia')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-2" {
  var src =
  \\ let j = [1, 2, 3]
  \\ let p = (j.pop() orelse 0) + 4
  \\ j.append(4)
  \\ let k = (j, p)
  \\ p += k.len()
  \\ assert(p == 9, 'p should be 9')
  \\ 
  \\ let x = {'a': 5, 'b': 6}
  \\ assert(x.keys().len() == 2, 'length of keys should be 2')
  \\ assert(x.values().len() == 2, 'length of values should be 2')
  \\ assert(x.items().len() == 2, 'length of items should be 2')
  \\ assert(x.get('a').? + 12 == 17, 'sum should be 17')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-3" {
  var src =
  \\ let j = []
  \\ let _ = j.append
  \\ _(5)
  \\ _(1)
  \\ assert(j.len() == 2, 'len should be 2')
  \\ let x = j.pop()
  \\ assert(x == 1, 'x should be 1')
  \\ assert(j.pop() == 5, 'should be 5')
  \\ assert(j.len() == 0, 'len should be 0 now')
  \\ let t = {'a': 1, 'b': 5, 'c': 12}
  \\ print(t, t.get('d'))
  \\ print(t.keys())
  \\ print(t.values())
  \\ print(t.items())
  \\ let g = (t)!
  \\ print(g, g.value())
  \\ let _ = g.value
  \\ print(_, _().len())
  \\ t.set('a', 0xff)
  \\ print(t)
  \\ let foo = t.get('p')
  \\ assert(foo is nil, 'foo must be nil')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-4" {
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
  \\ assert(t.pulse().pulse().x.len() == 3, 'should be 3')
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia', 'mia')
  \\ let j: Fox{'mia'} = w
  \\ assert(j.pulse().x.len() == 4, 'len should be 4')
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j = Fox{'mia'}('mia')
  \\ assert(j.x.len() == 1, 'len should be 1')
  \\ j = w
  \\ assert(j.pulse().x.len() == 3, 'len should be 3')
  \\
  \\ type Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j:Poo{'mia'} = Fox{'mia'}('mia', 'mia')
  \\ assert(j.x.len() == 2, 'len should be 2')
  \\ j = w
  \\ assert(j.x.len() == 3, 'len should be 3')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-5" {
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
  \\ let y = Fox{str}("a", "b")
  \\ let j: Fox{num} | Fox{str} = y
  \\
  \\ if j is Fox{num}
  \\  j.x = (j.x[0] + 5, j.x[1])
  \\  assert(j.x[0] == 11, 'should be 11')
  \\ end
  \\ if j is Fox{str}
  \\  j.x = ("hello world", j.x[1])
  \\ end
  \\ assert(j is Fox{str}, 'is Fox{str}')
  \\ assert((j as Fox{str}).x[0] == 'hello world', 'should be "hello world"')
  \\ j = x
  \\ assert(j is Fox{num}, 'is Fox{num}')
  \\ assert((j as Fox{num}).x[0] == 6, 'should be 6')
  ;
  try doRuntimeTest(src);
}

test "labeled-argument" {
  var src =
  \\ def fun(x: str, y: num, a: list{num}, b: err{str})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  print('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y: 5, a: [2, 3], x: 'oo', ('oops')!)
  \\ fun(y: 5, a: [2, 3], b: ('oops')!, x: 'oo')
  \\ 
  \\ def fun(x: str, y: num, a*: list{num})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  print('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y: 5, a: [2, 3], x: 'oo', a: [1, 2], a: [5, 6, 7])
  \\ fun(y: 5, a: [2, 3], x: 'oo', a: [1, 2], a: [5, 6, 7])
  \\ fun(y: 5, a: [2, 3], x: 'oo', [1, 2], [5, 6, 7])
  \\
  \\ def fun(x: str, y: num, a*: list{num})
  \\  assert(y == 5, 'y should not change')
  \\  print('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(a: [0x1, 0x2], y: 5, 'a', a: [2, 3])
  ;
  try doRuntimeTest(src);
}

test "labeled-argument-2" {
  var src =
  \\ class Fun
  \\  a: num
  \\  b: str
  \\  def init(a: num, b: str)
  \\    self.a = a
  \\    self.b = b
  \\  end
  \\  def send(data: list{any})
  \\    let i = 0
  \\    while i < data.len()
  \\      print('sending...', data[i])
  \\      i += 1
  \\    end
  \\  end
  \\ end
  \\ let f = Fun(b: 'oops', a: 12)
  \\ print(f.a, f.b)
  \\ f.send(data: ['a', 1, f])
  ;
  try doRuntimeTest(src);
}

test "patterns-1.<ordinary match>" {
  var src =
  \\ let e1 = ''
  \\ let e2 = ''
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a' as a, 'b' as b) as d => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case ('q', 'k') => print('third')
  \\  case _ => print("last")
  \\ end
  \\ assert(e1 == 'a', 'should be a')
  \\ assert(e2 == 'b', 'should be b')
  \\ 
  ;
  try doRuntimeTest(src);
}

test "patterns-2.<scopes>" {
  var src =
  \\ let o = '--'
  \\ let z = false
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a', 'b' as o) as d => z = true
  \\  case ('q', 'k') => print('third')
  \\  case _ => print("last")
  \\ end
  \\ assert(z, 'should match')
  \\ assert(o == '--', 'o should not change')
  ;
  try doRuntimeTest(src);
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
  \\  case (('a', 'p', ..), u) => do
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
  \\  case (..) as n => let j = n
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-4.<match on unions>" {
  var src =
  \\ let z = false
  \\ let j: 'a' | 'b' | 'c' = 'b'
  \\ match j
  \\  case 'a' => print('a')
  \\  case 'b' => do
  \\    print('ok')
  \\    z = true
  \\  end
  \\  case 'c' => print('hmm')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-5.<match on classes>" {
  var src =
  \\ let z = false
  \\ class Ant
  \\  a = 5
  \\ end
  \\ class Rat
  \\  x = 'yes'
  \\  y = 'no'
  \\  z = 'ok'
  \\ end
  \\ let j = Rat() as Ant | Rat
  \\ match j
  \\  case Ant(a) if a > 2 => print(12)
  \\  case Ant(5) => print(13)
  \\  case Ant(_) => print(40)
  \\  case Rat(x, z='ok.', ..) if x == 'yes' => print(9)
  \\  case Rat(x='ok', z='12', ..) => print(15)
  \\  case Rat(x='yes' as a, y='no' as p, ..) as t => do
  \\    print(19, t, a, p)
  \\    z = true
  \\  end
  \\  case Rat(..) => print(30)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-6.<tuple exhaustiveness>" {
  var src =
  \\ let z = false
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a', 'b' as o) as d => z = true
  \\  case ('q', 'k') => print('third')
  \\  case (..) => print("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-7.<list exhaustiveness>" {
  var src =
  \\ let z = false
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => print('first')
  \\  case [('a', 'b' as o)] as d => z = true
  \\  case [('q', 'k')] => print('third')
  \\  case [..] => print("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-8.<match exhaustiveness>" {
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
  \\  case Cat() => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-9.<nested match on classes>" {
  var src =
  \\ let j = [] as list{num}
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\  x = Pooh()
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ match p
  \\  case Cat([Dog(Pooh(a))]) as x => j = a
  \\  case _ => assert(false, 'bad match')
  \\ end
  \\ assert(j[0] == 1, 'should be 1')
  \\ assert(j[1] == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-10.<nested match on classes>" {
  var src =
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  y = [Dog()]
  \\ end
  \\ class Dog
  \\  z = [Pooh()]
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case Cat([Dog([Pooh([a, b])])]) as x => do
  \\    print('x:', x, 'a:', a, 'b:', b)
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case _ => assert(false, 'bad match')
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-11.<nested match on classes>" {
  var src =
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\  x = [Pooh()]
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case Cat([Dog([Pooh([a, b])])]) as x => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case _ => assert(false, 'bad match')
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-12.<nested match on classes>" {
  var src =
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\  x = [Pooh()]
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case Cat(x=[Dog(x=[Pooh(x=[a, b])])]) as x => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case _ => assert(false, 'bad match')
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-13.<match on bool>" {
  var src =
  \\ let z = false
  \\ let j = [false]
  \\ match j
  \\  case [a, ..] => match a
  \\    case true => print('yay')
  \\    case false as w => z = !w
  \\  end
  \\  case [..] => print('bad')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-14.<match on bool>" {
  var src =
  \\ let z = false
  \\ match (1 < 2)
  \\  case false => print('nay')
  \\  case true as t => z = !!t
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-15.<ranges>" {
  var src =
  \\ let z = false
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => print('hey')
  \\    case 3..5 => z = true
  \\    case _ => print('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-16.<ranges>" {
  var src =
  \\ let z = false
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => print('hey')
  \\    case 3..5 => z = true
  \\    case _ => print('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-17.<nullable types>" {
  var src =
  \\ let z = false
  \\ let j: ("a" | "b")? = "b"
  \\ match j
  \\   case "a" => print('a!')
  \\   case "b" => z = true
  \\   case nil => print('nah!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-18.<nullable types>" {
  var src =
  \\ let z = false
  \\ let j: ("a" | "b")? = nil
  \\ match j
  \\   case "a" => print('a!')
  \\   case "b" => print('nah!')
  \\   case nil => z = !z
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}
