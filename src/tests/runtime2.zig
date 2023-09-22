const std = @import("std");
const tests = @import("test.zig");

const value = tests.value;
const doRuntimeTest = tests.doRuntimeTest;

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

test "labeled argument" {
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

test "labeled argument 2" {
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
