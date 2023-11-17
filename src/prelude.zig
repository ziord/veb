pub const BuiltinsSrc =
\\ type Maybe{T} = Just(T) | None
\\
\\ type Result{T, E} = Ok(T) | Error(E)
\\
\\ alias never = never
\\
\\ def assert(arg: bool, msg: str): void
\\    #[[internal]]
\\ end
\\
\\ def exit(code: num): noreturn
\\    #[[internal]]
\\ end
\\
\\ def panic{T}(msg: T): noreturn
\\   #[[internal]]
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
\\ class List{T}
\\  def append(item: T): void
\\    #[[internal]]
\\  end
\\
\\  def len(): num
\\    #[[internal]]
\\  end
\\
\\  def pop(): Maybe{T}
\\    #[[internal]]
\\  end
\\
\\  def get(index: num): Maybe{T}
\\    #[[internal]]
\\  end
\\ end
\\
\\ class Tuple{T}
\\  def len(): num
\\    #[[internal]]
\\  end
\\ end
\\
\\
\\ type MapEntry{K, V} = Key(K) | Value(V)
\\
\\ class Map{K, V}
\\  def set(key: K, value: V): bool
\\    #[[internal]]
\\  end
\\
\\  def get(key: K): Maybe{V}
\\    #[[internal]]
\\  end
\\
\\  def delete(key: K): bool
\\    #[[internal]]
\\  end
\\
\\  def remove(key: K): bool
\\    #[[internal]]
\\  end
\\
\\  def keys(): List{K}
\\    #[[internal]]
\\  end
\\
\\  def values(): List{V}
\\    #[[internal]]
\\  end
\\  
\\  def items(): List{Tuple{K, V}}
\\    #[[internal]]
\\  end
\\
\\  def listItems(): List{MapEntry{K, V}}
\\    #[[internal]]
\\  end
\\
\\  def len(): num
\\    #[[internal]]
\\  end
\\ end
\\
\\ class str
\\  def len(): num
\\    #[[internal]]
\\  end
\\ end
;
