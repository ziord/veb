### veb

A delightful, statically typed programming language for writing reliable software. `veb` features algebraic data types, pattern matching, generics, classes (without inheritance), traits, flow-sensitive typing with type narrowing, trait-driven operator overloading and modules. Ergonomic features such as pipe and concat operators are also supported. Currently, the language runtime executes on a custom register-based virtual machine written in Zig.

> [!WARNING] 
> This project is highly experimental and a work in progress. Expect bugs.

### Syntax and Examples
#### Functions
```ruby
# function to compute the nth fibonacci number
def fib(n: num)
  return match n
    case 0 | 1 => n
    case _ => fib(n - 1) + fib(n - 2)
  end
end
fib(35) |> println
```

#### Pattern Matching
```ruby
# create and display a simple binary tree
type Tree{a} =
  | Node(val:a, left:Tree{a}, right:Tree{a})
  | Empty
 
def display{a}(tree: Tree{a})
  match tree
    case Node(val, lhs, rhs) => do
      println("Node:", val)
      print("Left: ")
      display(lhs)
      print("Right: ")
      display(rhs)
    end
    case Empty => println("Empty")
  end
end

let tree: Tree{num} = Node(
   1,
   Node (
     2,
     Node (3, Empty, Empty),
     Node (4, Empty, Empty)
   ),
   Node (
     5,
     Node (6, Empty, Empty),
     Node (7, Empty, Empty)
   )
)
display(tree)
```
**Exhaustiveness**
```ruby
# create and match on a tag
type Legion = Legion(str, num, str, bool)
let j = Legion('a', 5, 'boy', true)
match j
 case Legion('a', 1, 'first', true) => println('you rock!')
end
```
Results in:
```
TypeError: inexhaustive pattern match.

  .lang/test.veb.3:7:
    match j
          ^
  Remaining pattern type(s):
    Legion(_, _, _, false)
    Legion(_, _, str, _)
    Legion(_, num, _, _)
    Legion(str, _, _, _)
```

#### Higher-order Functions
```ruby
# functional reduce
def reduce(l: List{num}, func: fn(num, num):num, init: num)
  return match l
    case [] => init
    case [h, ..t] => func(reduce(t, func, init), h)
  end
end
const x = [1, 2, 3, 4, 5, 6, 7]
const add = def (a: num, b: num) => a + b
const result = reduce(x, add, 0) 
result |> println
result == 28 |> assert(*, 'should be 28')
```

> [!TIP]
> Check out the [tests](https://github.com/ziord/veb/tree/main/tests) directory for more examples.

### Building
You'll need to have the [Zig compiler](https://github.com/ziglang/zig/releases) (v0.13.0) installed. \
Build the project: `zig build -Doptimize=ReleaseSafe` \
Build the tests: `zig build test`
