pub const _BuiltinsItf =
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
;
