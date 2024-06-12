pub const NoExecsSrc =
\\ type Maybe{T} = Just(T) | None
\\
\\ type Result{T, E} = Ok(T) | Error(E)
\\
\\ alias never = never
\\
\\ def @exit(code: num): noreturn
\\    #[[internal]]
\\ end
\\
\\ def @panic{T}(msg: T): noreturn
\\   #[[internal]]
\\ end
\\
\\ def @string(val: any): str
\\   #[[internal]]
\\ end
\\
\\ def assert(arg: bool, msg: str): void
\\    #[[internal]]
\\ end
\\
\\ def print(args*: any): void
\\   #[[internal]]
\\ end
\\
\\ def println(args*: any): void
\\   #[[internal]]
\\ end
\\
\\ trait Iterator{U}
\\   pub def next(): Maybe{U};
\\
\\   pub def count(): num
\\     let len = 0
\\     while self.next() != None
\\       len += 1
\\     end
\\     return len
\\   end
\\
\\   pub def zip(itr: Iterator{U}): List{Tuple{U, U}}
\\     let res = [] as List{Tuple{U, U}}
\\     while true
\\       match (self.next(), itr.next())
\\         case (Just(a), Just(b)) => res.append((a, b))
\\         case _ => break
\\       end
\\     end
\\     return res
\\   end
\\ end
\\
\\ trait Iter{T}
\\   pub def iter(): Iterator{T};
\\ end
\\
\\ class List{T}: Iter{T}
\\   pub def append(item: T): void
\\     #[[internal]]
\\   end
\\
\\   pub def len(): num
\\     #[[internal]]
\\   end
\\
\\   pub def pop(): Maybe{T}
\\     #[[internal]]
\\   end
\\
\\   pub def get(index: num): Maybe{T}
\\     #[[internal]]
\\   end
\\
\\   pub def slice(start_index: num, end_index: num): List{T}
\\     #[[internal]]
\\   end
\\
\\   pub def iter(): Iterator{T}
\\     #[[internal]]
\\   end
\\ end
\\
\\ class Tuple{T}
\\   pub def len(): num
\\     #[[internal]]
\\   end
\\ end
\\
\\ type MapEntry{K, V} = Key(K) | Value(V)
\\
\\ class Map{K, V}
\\   pub def set(key: K, value: V): bool
\\     #[[internal]]
\\   end
\\
\\   pub def get(key: K): Maybe{V}
\\     #[[internal]]
\\   end
\\
\\   pub def delete(key: K): bool
\\     #[[internal]]
\\   end
\\
\\   pub def remove(key: K): bool
\\     #[[internal]]
\\   end
\\
\\   pub def keys(): List{K}
\\     #[[internal]]
\\   end
\\
\\   pub def values(): List{V}
\\     #[[internal]]
\\   end
\\  
\\   pub def items(): List{Tuple{K, V}}
\\     #[[internal]]
\\   end
\\
\\   pub def entries(): List{MapEntry{K, V}}
\\     #[[internal]]
\\   end
\\
\\   pub def len(): num
\\     #[[internal]]
\\   end
\\ end
\\
\\ class str
\\   pub def len(): num
\\     #[[internal]]
\\   end
\\
\\   pub def concat(other: str): str
\\     #[[internal]]
\\   end
\\ end
;

pub const ExecsSrc =
\\ def @iter{T}(itr: Iter{T}): Iterator{T}
\\    return itr.iter()
\\ end
\\
\\ def zip{T, U}(x: Iterator{T}, y: Iterator{U}): List{Tuple{T, U}}
\\   let res = [] as List{Tuple{T, U}}
\\   while true
\\     match (x.next(), y.next())
\\       case (Just(a), Just(b)) => res.append((a, b))
\\       case _ => break
\\     end
\\   end
\\   return res
\\ end
\\
\\ class ListIterator{T}: Iterator{T}
\\   cursor = 0
\\   list: List{T}
\\   def init(list: List{T})
\\     self.list = list
\\   end
\\  
\\   pub def next(): Maybe{T}
\\     let ret = self.list.get(self.cursor)
\\     self.cursor += 1
\\     return ret
\\   end
\\
\\   pub def iter()
\\     return self
\\   end
\\ end
\\
\\ class range: Iter{num} | Iterator{num}
\\   curr: num
\\   start: num
\\   step: num
\\   stop: Maybe{num}
\\ 
\\   def init(start: num, stop: Maybe{num}, step: Maybe{num})
\\     self.start = start
\\     self.curr = start
\\     self.stop = stop
\\     self.step = match step
\\       case Just(s) => s
\\       case None => 1
\\     end
\\   end
\\
\\   pub def next()
\\     match self.stop
\\       case Just(stop) => do
\\         if self.curr >= stop then
\\           return None
\\         end
\\         let res = Just(self.curr)
\\         self.curr += self.step
\\         return res
\\       end
\\       case None => do
\\         let res = Just(self.curr)
\\         self.curr += self.step
\\         return res
\\       end
\\     end
\\   end
\\
\\   pub def iter()
\\     return self
\\   end
\\ end
;

pub const ExecsDecls = @as(usize, 4);
pub const BuiltinsSrc = @as([]const u8, NoExecsSrc ++ ExecsSrc);
