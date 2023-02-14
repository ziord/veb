// rx = reg(x)
// rk(x) = r(x) if x < max_regs else k(x - max_regs)

pub const OpCode = enum (u8) {
  // add rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Add,

  // sub rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Sub,

  // div rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Div,

  // mul rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Mul,

  // mod rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Mod,

  // xor rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Xor,

  // or rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Or,

  // and rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  And,

  // shr rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Shr,

  // shl rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Shl,

  // cmp rx, rk(x), rk(x) | cmp_op 
  // [6] [8]  [9]    [9]     [32]
  Cmp,

  // inv rx, rk(x)
  // [6] [8]  [18]
  Inv,

  // load rx, bx
  //  [6] [8]  [18]
  Load,

  // ret
  // [6]
  Ret   
};