// rx = reg(x)
// rk(x) = r(x) if x < max_regs else k(x - max_regs)

pub const OpCode = enum (u8) {
  // add rx, rk(x), rk(x)
  // [6] [8]  [9]    [9]
  Add,

  // sub rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Sub, // subtract

  // div rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Div, // divide

  // mul rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Mul, // multiply

  // mod rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Mod, // modulus

  // xor rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Xor, // bitwise xor

  // or rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Or, // bitwise or

  // and rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  And, // bitwise and

  // is rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Is, // type check

  // iscls rx, rk(x), rk(y)
  // [6]  [8]  [9]    [9]
  Iscls, // type check

  // istag rx, rk(x), rk(y)
  // [6]  [8]  [9]    [9]
  Istag, // type check

  // istoc rx, rk(x), rk(y)
  // [6]  [8]  [9]    [9]
  Istoc, // type check

  // shr rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Shr, // bitwise right-shift

  // shl rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Shl, // bitwise left-shift

  // cles rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cles, // cmp less

  // cgrt rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cgrt, // cmp greater

  // cleq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cleq, // cmp less-or-equal

  // cgeq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cgeq, // cmp greater-or-equal

  // ceqq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Ceqq, // cmp equal-to

  // cneq rx, rk(x), rk(y)
  // [6] [8]  [9]    [9]
  Cneq, // cmp not-equal-to

  // inv rx, rk(x)
  // [6] [8]  [18]
  Inv, // bitwise invert

  // bcst rx, rk(x)
  // [6] [8]  [18]
  Bcst, // bool cast

  // jt rx, bx
  // [6] [8]  [18]
  Jt, // jump if true

  // jf rx, bx
  // [6] [8]  [18]
  Jf, // jump if false

  // jmp d, sbx as jmp _, sbx
  //  `---------> [6] [8]  [18]
  Jmp, // jump

  // not rx, bx
  // [6] [8] [18]
  Not, // logic not

  // nerr rx, val
  // [6] [8] [18]
  Nerr, // new error

  // nlst rx, size
  // [6] [8] [18]
  Nlst, // new list

  // slst rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Slst, // set list

  // glst rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Glst, // get list

  // ntup rx, size
  // [6] [8] [18]
  Ntup, // new tuple

  // stup rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Stup, // set tuple

  // gtup rx, rk(idx), rk(val)
  // [6]  [8]   [9]      [9]
  Gtup, // get tuple

  // nmap rx, size
  // [6] [8] [18]
  Nmap, // new map

  // smap rx, rk(key), rk(val)
  // [6]  [8]   [9]      [9]
  Smap, // set map

  // gmap rx, rk(key), rk(val)
  // [6]  [8]   [9]      [9]
  Gmap, // get map

  // gglb rx, bx -> r(x) = G[K(bx)]
  // [6] [8] [18]
  Gglb, // get global

  // sglb rx, bx -> G[K(bx)] = r(x)
  // [6] [8] [18]
  Sglb, // set global

  // ggsym rx, bx -> r(x) = GS[bx]
  // [6] [8] [18]
  Ggsym, // get global sym

  // sgsym rx, bx -> GS[bx] = r(x)
  // [6] [8] [18]
  Sgsym, // set global sym

  // gmsym rx, rk(mod), rk(idx)
  // [6]   [8]    [9]         [9]
  Gmsym, // get module sym

  // smsym rx(mod), idx, rk(value)
  // [6]   [8]    [9]         [9]
  Smsym, // set module sym

  // mov rx, ry
  // [6] [8]  [9]
  Mov, // move

  // asrt rx (1-arg using 2-arg fmt)
  // [6] [8]
  Asrt,  // assert

  // load rx, bx
  //  [6] [8]  [18]
  Load,

  // bclo rx, bx
  // [6] [8] [18]
  Bclo,  // build closure

  // call rx, bx
  // [6] [8] [18]
  Call,

  // gupv rx, bx
  // [6] [8] [18]
  Gupv, // get upvalue

  // supv rx, bx
  // [6] [8] [18]
  Supv, // set upvalue

  // cupv rx (1-arg using 2-arg fmt)
  // [6] [8]
  Cupv, // close upvalue

  // callc rx, argc, flen
  // [6]  [8] [9]    [9]
  Callc, // call class

  // fcls rx (1-arg using 2-arg fmt)
  // [6]  [8]
  Fcls, // finish class

  // smtd rx(mth), rk(cls), idx
  // [6]    [8]     [9]    [9]
  Smtd, // set method

  // gmtd rx, rk(inst), rk(prop.idx)
  // [6]   [8]    [9]         [9]
  Gmtd, // get method

  // Jnextc rx, rk(inst), rk(prop.idx) | call rx, bx
  // [6]   [8]    [9]         [9]
  Jnextc, // super instruction for get mtd & call mtd

  // gmtd rx, rk(inst), rk(prop.idx)
  // [6]   [8]    [9]         [9]
  Gmtds, // get method slow

  // Jnextcs rx, rk(inst), rk(prop.idx) | call rx, bx
  // [6]   [8]    [9]         [9]
  Jnextcs, // super instruction for get mtd slow & call mtd

  // sfd rx(inst), prop.idx, rk(value)
  // [6]   [8]    [9]         [9]
  Sfd, // set field

  // ssfd rx(inst), prop.idx, rk(value)
  // [6]   [8]    [9]         [9]
  Ssfd, // set struct field

  // gfd rx, rk(inst), prop.idx
  // [6]   [8]    [9]         [9]
  Gfd, // get field

  // gsfd rx, rk(inst), prop.idx
  // [6]   [8]    [9]         [9]
  Gsfd, // get struct field

  // ret rx  (1-arg using 2-arg fmt)
  // [6] [8]
  Ret   
};
