pub const BuiltinsSrc =
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
\\ class List{T}
\\  pub def append(item: T): void
\\    #[[internal]]
\\  end
\\
\\  pub def len(): num
\\    #[[internal]]
\\  end
\\
\\  pub def pop(): Maybe{T}
\\    #[[internal]]
\\  end
\\
\\  pub def get(index: num): Maybe{T}
\\    #[[internal]]
\\  end
\\
\\  pub def slice(start_index: num, end_index: num): List{T}
\\    #[[internal]]
\\  end
\\ end
\\
\\ class Tuple{T}
\\  pub def len(): num
\\    #[[internal]]
\\  end
\\ end
\\
\\ type MapEntry{K, V} = Key(K) | Value(V)
\\
\\ class Map{K, V}
\\  pub def set(key: K, value: V): bool
\\    #[[internal]]
\\  end
\\
\\  pub def get(key: K): Maybe{V}
\\    #[[internal]]
\\  end
\\
\\  pub def delete(key: K): bool
\\    #[[internal]]
\\  end
\\
\\  pub def remove(key: K): bool
\\    #[[internal]]
\\  end
\\
\\  pub def keys(): List{K}
\\    #[[internal]]
\\  end
\\
\\  pub def values(): List{V}
\\    #[[internal]]
\\  end
\\  
\\  pub def items(): List{Tuple{K, V}}
\\    #[[internal]]
\\  end
\\
\\  pub def listItems(): List{MapEntry{K, V}}
\\    #[[internal]]
\\  end
\\
\\  pub def len(): num
\\    #[[internal]]
\\  end
\\ end
\\
\\ class str
\\  pub def len(): num
\\    #[[internal]]
\\  end
\\
\\  pub def concat(other: str): str
\\    #[[internal]]
\\  end
\\ end
;
