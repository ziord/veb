// rx = reg(x)
// rk(x) = r(x) if x < max_regs else k(x - max_regs)

pub const OpCode = enum (u8) {
  // add rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Add,

  // sub rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Sub,

  // div rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Div,

  // mul rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Mul,

  // mod rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Mod,

  // xor rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Xor,

  // or rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Or,

  // and rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  And,

  // is rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Is,

  // shr rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Shr,

  // shl rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Shl,

  // cles rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cles,

  // cgrt rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cgrt,

  // cleq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cleq,

  // cgeq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cgeq,

  // ceqq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Ceqq,

  // cneq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cneq,

  // inv rx, rk(x)
  // [6] [8]  [18]
  Inv,

  // bcst rx, rk(x)
  // [6] [8]  [18]
  Bcst,

  // jt rx, bx
  // [6] [8]  [18]
  Jt,

  // jf rx, bx
  // [6] [8]  [18]
  Jf,

  // jmp d, bx as jmp rx, bx
  //  `---------> [6] [8]  [18]
  Jmp,

  // not rx, bx
  // [6] [8] [18]
  Not,

  // nerr rx, val
  // [6] [8] [18]
  Nerr,

  // nlst rx, size
  // [6] [8] [18]
  Nlst,

  // slst rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Slst,

  // glst rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Glst,

  // ntup rx, size
  // [6] [8] [18]
  Ntup,

  // stup rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Stup,

  // gtup rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Gtup,

  // nmap rx, size
  // [6] [8] [18]
  Nmap,

  // smap rx, rk(key), rk(val)
  // [6]  [8]   [9]      [9]
  Smap,

  // gmap rx, rk(key), rk(val)
  // [6]  [8]   [9]      [9]
  Gmap,

  // gglb rx, bx -> r(x) = G[K(bx)]
  // [6] [8] [18]
  Gglb,

  // sglb rx, bx -> G[K(bx)] = r(x)
  // [6] [8] [18]
  Sglb,

  // ggsym rx, bx -> r(x) = GS[bx]
  // [6] [8] [18]
  Ggsym,

  // sgsym rx, bx -> GS[bx] = r(x)
  // [6] [8] [18]
  Sgsym,

  // mov rx, ry
  // [6] [8]  [9]
  Mov,

  // asrt rx (1-arg using 2-arg fmt)
  // [6] [8]
  Asrt,

  // load rx, bx
  //  [6] [8]  [18]
  Load,

  // bclo rx, bx
  // [6] [8] [18]
  Bclo,

  // call rx, bx
  // [6] [8] [18]
  Call,

  // gupv rx, bx
  // [6] [8] [18]
  Gupv,

  // supv rx, bx
  // [6] [8] [18]
  Supv,

  // cupv rx (1-arg using 2-arg fmt)
  // [6] [8]
  Cupv,

  // ret rx  (1-arg using 2-arg fmt)
  // [6] [8]
  Ret   
};
