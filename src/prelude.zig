pub const BuiltinsSrc =
\\ type never = never
\\
\\ def assert(arg: bool, msg: str): void | noreturn
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
\\ class list{T}
\\  def init(args*: T): void
\\     #[[internal]]
\\  end
\\
\\  def append(item: T): void
\\    #[[internal]]
\\  end
\\
\\  def len(): num
\\    #[[internal]]
\\  end
\\
\\  def pop(): T | err{str}
\\    #[[internal]]
\\  end
\\
\\  def get(index: num): T?
\\    #[[internal]]
\\  end
\\ end
\\
\\ class tuple{T}
\\  def init(args*: T): void
\\     #[[internal]]
\\  end
\\
\\  def len(): num
\\    #[[internal]]
\\  end
\\
\\  def get(index: num): T?
\\    #[[internal]]
\\  end
\\ end
\\
\\ class map{K, V}
\\  def set(key: K, value: V): void
\\    #[[internal]]
\\  end
\\
\\  def get(key: K): V?
\\    #[[internal]]
\\  end
\\
\\  def keys(): list{K}
\\    #[[internal]]
\\  end
\\
\\  def values(): list{V}
\\    #[[internal]]
\\  end
\\  
\\  def items(): list{tuple{K | V}}
\\    #[[internal]]
\\  end
\\
\\  def len(): num
\\    #[[internal]]
\\  end
\\ end
\\
\\ class err{T}
\\  def init(val: T): void
\\     #[[internal]]
\\  end
\\
\\  def value(): T
\\     #[[internal]]
\\  end
\\ end
\\
\\ class str
\\  def len(): num
\\    #[[internal]]
\\  end
\\ end
;
