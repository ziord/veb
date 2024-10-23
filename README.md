### veb

A delightful, statically typed programming language for writing reliable software. `veb` features algebraic data types, pattern matching, generics, classes (without inheritance), traits, flow-sensitive typing with type narrowing, trait-driven operator overloading and modules. Ergonomic features such as pipe and concat operators are also supported. Currently, the language runtime executes on a custom register-based virtual machine written in Zig.

> [!WARNING] 
> This project is highly experimental and a work in progress. Expect bugs.

### Syntax and Examples
#### Functions
```ruby
# function to compute the nth fibonacci number
def fib(n: Num)
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

let tree = Node(
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
) as Tree{Num}
display(tree)
```
**Exhaustiveness**
```ruby
# create and match on a tag
type Legion = Legion(Str, Num, Str, Bool)
let j = Legion('a', 1, 'first', True)
match j
 case Legion('a', 1, 'first', True) => println('you rock!')
end
```
Results in:
```
TypeError: inexhaustive pattern match.

  .lang/test.veb.4:7:
    match j
          ^
  Remaining pattern type(s):
    Legion(_, _, _, False)
    Legion(_, _, Str, _)
    Legion(_, Num, _, _)
    Legion(Str, _, _, _)
```

#### Higher-order Functions
```ruby
# functional reduce
def reduce(l: List{Num}, func: fn(Num, Num):Num, init: Num)
  return match l
    case [] => init
    case [h, ..t] => func(reduce(t, func, init), h)
  end
end
const x = [1, 2, 3, 4, 5, 6, 7]
const add = def (a: Num, b: Num) => a + b
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
