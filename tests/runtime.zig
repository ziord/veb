const std = @import("std");
const lib = @import("test");

const doRuntimeTest = lib.doRuntimeTest;

test "arithmetic ops" {
  const srcs =
    \\ let j = (0x2 * 45 / 2 * 5 - 1 + 6 / 3 - 0x5 + 6 * (0b1 - 0o2) / 0o1_5) + 234_56.e-2 - 2 % (5-4) - 6
    \\ assert(j == 449.09846153846155, 'j should be 449.09846153846155')
    \\ assert((2 ^ 3 ^ (6 | 0 | 1 | 5)) == 6, 'expr should be 6')
    \\ assert((2 | 3 ^ 1 & 0xff) == 2, 'expr should be 2')
    \\ assert((300 >> 8 & 0xff) == 1, 'expr should be 1')
    \\ assert((0xf << 6 | 2) == 962, 'expr should be 962')
    \\ assert((~0x123 + --2) == -290, 'expr should be -290')
    \\ assert((~0x123 ++ --2) == -290, 'expr should be -290')
    \\ assert((0 ++ --2 + ~0x123) == -290, 'expr should be -290')
    \\ assert((0 ++ --2 + ~0x123 - ~(3 * 4 - (6 + 2 ) * 5)) == -317, 'expr should be -317')
    \\ assert(-5 == -5, 'expr should be -5')
  ;
  try doRuntimeTest(srcs);
}

test "comparison ops" {
  const srcs =
    \\ assert((0x123 < 4) == False, '0x123 < 4')
    \\ assert((123.45 > 12_40) == False, '123.45 > 12_40')
    \\ assert(0b111_000 <= 0o12_12, 'should be lte')
    \\ assert((123.e-2 >= 0x12_34_5) == False, 'should be gte')
    \\ assert(123.e-2 != 0x12_34_5, 'should be unequal')
    \\ assert(0xdeadbeef == 0o33653337357, 'should be equal')
  ;
  try doRuntimeTest(srcs);
}

test "booleans" {
  const srcs = 
  \\ assert((0x123 < 4 and 1 < 5) == False, '0x123 < 4 and 1 < 5')
  \\ assert((123.45 > 12_40 or 2 > 1), '123.45 > 12_40 or 2 > 1')
  \\ assert((0b111_000 <= 0o12_12 or 1 > 0.5), '0b111_000 <= 0o12_12 or 1 > 0.5')
  \\ assert(!(123.e-2 >= 0x12_34_5 and 7 > 2), '!(123.e-2 >= 0x12_34_5 and 7 > 2)')
  \\ assert(!!(123.e-2 != 0x12_34_5 or 6 > 2), '!!(123.e-2 != 0x12_34_5 or 6 > 2)')
  \\ assert((1 or 2) == 1, '(1 or 2) == 1')
  \\ assert((1 and 2) == 2, '(1 and 2) == 2')
  \\ assert((0b00 and 2) == 0o0, '(0b00 and 2) == 0o0')
  \\ assert((0x0 or 2) == 2, '(0x0 or 2) == 2')
  \\ assert(True or False, 'True or False')
  \\ assert(False or True, 'False or True')
  \\ assert((False or False) == False, 'False or False')
  \\ assert(True or True, 'True or True')
  \\ assert((True and False) == False, 'True and False')
  \\ assert((False and True) == False, 'False and True')
  \\ assert((False and False) == False, 'False and False')
  \\ assert(!False, '!False')
  \\ assert(!True == False, '!True')
  \\ assert(!(0x0_0), '!(0x0_0)')
  \\ assert(!!(1), '!!(1)')
  \\ assert(!(1) == False, '!(1)')
  \\ assert('foxes and pirates' == 'foxes and pirates', 'foxes & pirates eql')
  \\ assert('foxes and pirates' != 'fishes and pirates', 'foxes & pirates & fishes neql')
  \\ assert(('fox' or '') == 'fox', 'should be fox')
  \\ assert(('fox' and '') == '', 'should be empty Str')
  ;
  try doRuntimeTest(srcs);
}

test "strings" {
  const src =
    \\assert("'foxes' the quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog
    \\the quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog" ==
    ++
    "\"'foxes' the quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog\n" ++
    "the quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog,\nthe quick brown fox jumps over the lazy dog\"" ++
    ", 'should be eql')"
  ;
  try doRuntimeTest(src);
}

test "lists & tuples" {
  const src = 
    \\ _ = [1, 2, 3, 4]
    \\ _ = [Ok(1) as Result{Num, Num}, Error(2), Ok(3), Error(4)]
    \\ _ = (1, 'fox', 3, 'cat')
    \\ _ = [1]
    \\ _ = []
    \\ _ = (1, 'fox', 3, 'cat', (1, 'fox', 3, 'cat'))
    \\ _ = (1, 2, {'a': 'set'})
    \\ _ = (1, 2, {'a': 'set'},)
    \\ _ = (1,)
    \\ _ = ({'abc': 123}, {True: 0xff}, {'obs': 0b101})
    \\ _ = ()
    \\ _ = ([1, 2, 3], [5, 5, 5])
    \\ _ = ('abc',) as Tuple{'abc'}
    \\ _ = ('x', 'y',)
    \\ let x: List{Result{Num, Num}} = [Ok(1) as Result{Num, Num}, Error(2), Ok(3), Error(4)]
    \\ println(x, x.len())
  ;
  try doRuntimeTest(src);
}

test "maps" {
  const srcs =
    \\ _ = []
    \\ _ = {'abc': 123}
    \\ type Key = Str_(Str) | Bool_(Bool)
    \\ _ = {Str_('abc') as Key: 123, Bool_(True): 0xff, Str_('obs'): 0b101}
    \\ _ = {}
    \\ _ = {24: [1, 2, 3]}
    \\ _ = {24: [1, 2, 3],}
  ;
  try doRuntimeTest(srcs);
}

test "regs" {
  const src = \\ {(1 * 2) * 3 - (3 * 5) : [1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\, 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
  \\]}
;
try doRuntimeTest(src);
}

test "vars" {
  const src = 
  \\ let x = 5 * 0xff - 2
  \\ let k = 10
  \\ k = 5
  \\ let p = k
  \\ _ = [p, k]
  \\ let y = (
  \\     x - 5
  \\ )
  \\ let z = [
  \\     {x: y},
  \\ {
  \\     x: y
  \\ }
  \\ ]
  \\ _ = {123: "foxlike"}
  \\ #let y = 10
  \\ #[x, y]
  \\ _ = z
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ let x = 5
  \\ x += 10
  \\ x -= 3
  \\ x /= 2
  \\ x *= 2
  \\ x &= 1
  \\ x ^= 3
  \\ x |= 4
  \\ assert(x == 7, 'x should be 7')
  ;
  try doRuntimeTest(src2);
}

test "types" {
  const src = 
  \\ type VOrC{V, C} = Vee(V) | Cee(C)
  \\ alias P{K, V} = VOrC{VOrC{K, V}, V}
  \\ alias A{K, V} = Map{K, Map{K, P{K, V}}}
  \\ let x: A{Str, Num} = (
  \\  {
  \\    'a': (
  \\        {
  \\          'x': Vee(
  \\                  (Cee(5) as VOrC{Str, Num})
  \\                ) as P{Str, Num}
  \\        }
  \\      ) as Map{Str, P{Str, Num}}
  \\  }
  \\ )
  \\ println(x)
  \\ let x = {Just(False) as Bool?: {True: Just('ok') as Str?} as Map{Bool, Str?}}
  \\ let y = 15 as Num
  \\ let j = (15 as Num) as Num
  \\ let z = {15: ['foxy']} as Map{Num, List{Str}}
  \\ let z1 = {15: ['foxy']} as (Map{Num, List{Str}})
  \\ let z2 = Just({12: ['foxy']}) as (Map{Num, List{Str}})?
  \\ type T{K, V} = K | V
  \\ let p: T{Num, Str} = K
  \\ p = V
  \\ let x = None
  \\ let y: Str? = x
  \\ let z: (Num)? = Just(5)
  \\ let z2: Num = z.? + 5
  \\ z2 += z2
  \\ let j: Tuple{True, False} = (True, False)
  \\ let q = (1, 2, 'abc', 0xff, 1, 'foo', 'bar')
  \\ let s = [1, 2, 3]
  \\ s[0] = q[0]
  \\ s[1] = q[1]
  \\ s[2] = q[4]
  \\ _ = (s, q)
  ;
  try doRuntimeTest(src);
}

test "empty generic types" {
  const src = 
  \\ let p = ()
  \\ let j: List{Any} = []
  \\ let q: Map{Any, Any} = {}
  ;
  try doRuntimeTest(src);
}

test "blocks" {
  const src =
  \\ let x = 'over the garden wall!'
  \\ do
  \\   let x = 5
  \\   let y = 10
  \\   let z = {x: x * y}
  \\   assert(z[x] == 50, 'should be 50')
  \\ end
  \\ assert(x == 'over the garden wall!', 'x should be "over the garden wall!"')
  \\ do
  \\   let x = 't-rex'
  \\   do
  \\      x = 'foo-foo'
  \\      assert(x == 'foo-foo', 'should be foo-foo')
  \\   end
  \\   assert(x == 'foo-foo', 'should be foo-foo')
  \\ end
  \\ assert(x == 'over the garden wall!', 'x should still be "over the garden wall!"')
  ;
  try doRuntimeTest(src);
}

test "linking" {
  const src =
  \\ alias HashMap{K, V} = Map{K, V}
  \\ alias StringHashMap{V} = HashMap{Str, V}
  \\ alias NumList = List{Num}
  \\ let a: NumList = [1, 2]
  \\ let b: HashMap{Num, Bool} = {0: False}
  \\ let c: StringHashMap{Bool} = {'foo': False}
  \\ let x: Str = 'over the garden wall!'
  \\ let y = 'oops'
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ alias A = Str
  \\ alias B = Num
  \\ alias C{A, B} = Map{A, B}
  \\ alias D{K} = C{K, B}
  \\ alias HashMap{K, V} = C{K, V}
  \\ alias StringHashMap{V} = HashMap{Str, V}
  \\ alias BadList = List{StringHashMap{HashMap{A, D{B}}}}
  \\ alias X = BadList #(D{BadList}? | D{B}? | BadList?)
  \\ let x: X = ([{'fox': {'fin': ({0x12: 0xbee}) as Map{Num, Num}}}])
  \\ x
  \\ let y: D{B}? = Just({10: 5}) as Map{Num, Num}?
  \\ y
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ alias HashMap{K, V} = Map{K, V}
  \\ let x: HashMap{Num, Str} = {5: 'okay'}
  \\ Just(x) as Map{Num, Str}?
  \\ alias HashMap{K, V} = Map{K, V}
  \\ let x: HashMap{Num, Str} = {5: 'okay'}
  \\ Just(x as Map{Num, Str}) as Map{Num, Str}?
  ;
  try doRuntimeTest(src3);
}

test "recursive types" {
  const src =
  \\ # recursive generics
  \\ type R = Strs(Str) | Nums(Num) | Col(List{R})
  \\ let x: R = Strs('fox')
  \\ x = Col([
  \\    Col([
  \\      Col([Col([Strs('foo') as R] as List{R}) as R]) as R,
  \\      Nums(3),
  \\      Col([
  \\        Col([
  \\          Col([Nums(4) as R, Nums(5)]) as R,
  \\          Strs('bar') as R
  \\        ]) as R
  \\      ]) as R, 
  \\      Strs('ok')
  \\    ]) as R,
  \\    Col([ Col([ Col([Strs('duh') as R]) as R ]) as R ]),
  \\    Col([R.Nums(4)])
  \\ ])
  \\ x = Strs('ok')
  \\ x
  \\ # no conflicts
  \\ type A{T} = T
  \\ let x: A{A{Num}} = T
  ;
  try doRuntimeTest(src);
}

test "circularity" {
  const src =
  \\ type T{K} = A1(Str) | B1(S{K})
  \\ type S{K} = A2(Num) | B2(T{Num})
  \\ let p: T{Num} = A1('fox')
  \\ println(p)
  \\ p = B1(A2(5) as S{Num})
  \\ println(p)
  \\ alias T = T
  \\ alias S = S
  \\ type Q = A(T) | B(S)
  \\ let Q: Q? = None #Just(A(T) as Q)
  \\ println(Q)
  ;
  try doRuntimeTest(src);
}

test "circularity-2" {
  const src =
  \\ type T{P} = A1(Str) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{Num})
  \\ let p: T{Str} = A1('fox')
  \\ println(p)
  \\ p = B1(A2('5') as S{Str})
  \\ println(p)
  ;
  try doRuntimeTest(src);
}

test "circularity-3" {
  const src =
  \\ type T{P} = A1(P) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{K})
  \\ let p: T{Str} = A1('fox')
  \\ p = B1(A2('5') as S{Str})
  \\
  \\ type T{P} = A1(P) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{K})
  \\ let p: T{Num} = A1('fox'.len())
  \\ p = B1(A2(5) as S{Num})
  ;
  try doRuntimeTest(src);
}

test "cast-typecheck" {
  const src =
  \\ let x = 5 as Bool
  \\ assert(x, 'should be True')
  \\ _ = !!x and !!'fox'
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ type NumStrBool = Nums(Num) | Strs(Str) | Bools(Bool)
  \\ let p = Just(Nums(5) as NumStr) as NumStr?
  \\ let q: NumStrBool = Nums(5)
  \\ match q
  \\  case Nums(t) => q = Nums(t + 5)
  \\  case _ => assert(False, '')
  \\ end
  \\ alias X = NumStr
  \\ let y: X = (Strs('food') as NumStr)
  \\ y
  \\
  \\ type Cat = A(Str) | B(Str)
  \\ type Dog = A(Num) | B(Str)
  \\ let x: Cat = Cat.A('fox')
  \\ let y: Dog = Dog.A(12)
  \\ println(x, y)
  \\ x = Cat.B('foo')
  \\ y = Dog.A(56)
  \\ type Cat = A(Str)
  \\ type Dog = A(Num)
  \\ let x: Cat = Cat.A('fox')
  \\ let y: Dog = Dog.A(12)
  \\
  \\ type NumStr = A(Num) | B(Str)
  \\ type StrNum = B(Str) | A(Num)
  \\ let a: NumStr = NumStr.A(10)
  \\ let b: StrNum = StrNum.B('foo')
  \\ b = a
  \\
  \\ let x = [] as List{Num}
  \\ x = [1, 2, 3]
  \\ type T = S(Str) | N(Num) | L(List{T})
  \\ let p: T = T.L([T.L([N(1) as T, T.L([T.N(5) as T, S('hey')]), S('2')])])
  ;
  try doRuntimeTest(src);
}

test "indexing .1" {
  const src =
  \\ let t = [5, 6, 7, 4]
  \\ let q = t[0] + 5
  \\ t[0] += 10
  \\ let p = {'a': 5}
  \\ p['b'] = p['a']
  \\ let c = p['a'] + 10
  \\ let d = c * 5
  \\ type NumBool = Nums(Num) | Bools(Bool)
  \\ let x: List{NumBool} = [Nums(1) as NumBool, Nums(2)]
  \\ x[1] = Nums(d)
  \\ x[
  \\  -1 + 1
  \\ ] = Bools(!d)
  \\ let x = [1, 2, 3, 4, 5]
  \\ let y = x[t[3]]
  \\ let r = !!x[2]
  \\ x[-1] <<= 3
  \\ let w = x[-2]
  \\ _ = (t, r, q, p, x, y, d, w)
  \\ # multi type index
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ let _a = Strs('fox')
  \\ let _b = Strs('fun')
  \\ let _c = Nums(5)
  \\ let _d = Strs('fox')
  \\ let y = {_a as NumStr: Strs('fan'), _b: Strs('fox'), _c: _d}
  \\ let p = y[_a] and y[_b] and y[_c as NumStr]
  \\ assert(p == (_d as NumStr), 'should be fox')
  ;
  try doRuntimeTest(src);
}

test "indexing .2" {
  const src =
  \\ let x: Tuple{List{Tuple{Num, List{Num}}}, Num} = ([(9, [] as List{Num})], 5)
  \\ let p = 0
  \\ if x[0] is List{Tuple{Num, List{Num}}}
  \\    if x[0][0] is Tuple{Num, List{Num}}
  \\      if x[0][0][1] is List{Num}
  \\        p /= 5
  \\      end
  \\    end
  \\ elif x[1] is Num
  \\    p += 0
  \\ else
  \\    println(x[0])
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "type summation" {
  const src = 
  \\ let p: List{Num?} = []
  \\ let q: List{Num?} = [None as Num?]
  \\ let r: List{Num?} = [None as Num?, Just(5), None]
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ let r: List{NumStr?} = [None as NumStr?, Just(Nums(5) as NumStr), None, Just(Strs('foo') as NumStr)]
  \\ let t: List{(NumStr)?} = [None as NumStr?, Just(Nums(5) as NumStr), None]
  ;
  try doRuntimeTest(src);
}

test "nil access" {
  const src =
  \\ let x: Num? = Just(5)
  \\ let p = x.? + 10
  \\ let f = {'foo': Just(5) as Num?}
  \\ let j = f['foo'].? + 5
  \\ assert(j == 10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "if statement" {
  // if-elif-else
  const src =
  \\ let x: Num = 5
  \\ let t: Num? = Just(15)
  \\ let p = 0
  \\ if x == t.? / 3 -1
  \\   p = x + 10
  \\ elif t == None
  \\    p = 29
  \\ else
  \\    match t
  \\      case Just(v) => do
  \\        if v > 10
  \\          p = 12
  \\          p = p << 12 - p >> 3
  \\        else
  \\          p = x - 10
  \\          p -= -1111
  \\        end
  \\      end
  \\    end
  \\ end
  \\ assert(p == 1, 'should be 1')
  \\ let x = 5
  \\ let p = 'let'
  \\ if (x > x) and !5 then
  \\ end
  \\ let z = 10
  ;
  try doRuntimeTest(src);
  // if-else
  const src2 =
  \\ let x: Num = 5
  \\ let t: Num? = Just(15)
  \\ let p = 0
  \\ if x == t.? / 3
  \\   p = x + 10
  \\   p *= 3
  \\ else
  \\   p = x - 10
  \\   p -= -1111
  \\ end
  \\ assert(p == 45, 'should be 45')
  \\ p == 0x2d
  \\ # if with optional then
  \\ if 2 > 1 then
  \\  'hooray!'
  \\ elif 4 > 5 then
  \\  'oh no!'
  \\ end
  \\ if 2 > 1
  \\  'hooray!'
  \\ elif 4 > 5 then
  \\  'oh no!'
  \\ end
  \\ if 2 > 1 then
  \\  'hooray!'
  \\ elif 4 > 5
  \\  'oh no!'
  \\ end
  ;
  try doRuntimeTest(src2);

  // if-end local
  const src3 =
  \\ do
  \\    let x: Num = 5
  \\    let t: Num? = Just(15)
  \\    let p = 0
  \\    if x == t.? / 3
  \\      p = x + 10
  \\      p *= 3
  \\      p += x - 10
  \\      p -= -1111
  \\      p += 0b1111_1111_1111
  \\    end
  \\    assert(p == 0b1010001111110, 'it should')
  \\ end
  ;
  try doRuntimeTest(src3);

  // if-end
  const src4 =
  \\ let x: Num = 5
  \\ let t: Num? = Just(15)
  \\ let p = 0
  \\ if x == t.? / 3
  \\   p = x + 10
  \\   p *= 3
  \\   p = x - 10
  \\   p -= -1111
  \\   p += 0b1111_1111_1111
  \\ end
  \\ assert(p == 0b1010001010001, 'should be same')
  ;
  try doRuntimeTest(src4);
  // if-else
  const src5 =
  \\ let x: Num = 5
  \\ let t: Num? = Just(15)
  \\ let p = 0
  \\ if 0xf == x - t.?
  \\   p = x + 10
  \\ else
  \\   p = x - 10
  \\   p -= -1111
  \\ end
  \\ assert(p == 1106, 'should be 1106')
  \\ p == 0o2122 and p == 0x452
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ let p: NumStr = Nums(5)
  \\ let q = NumStr.Strs("fox")
  \\ let w = 0
  \\ if p == q
  \\  match p
  \\    case Nums(t) => w = t
  \\    case _ => assert(False, '-')
  \\  end
  \\ else
  \\   w = 123
  \\ end
  \\ assert(w == 123, 'should be 123')
  \\ match q
  \\  case Strs(t) => if t == 'fox'
  \\    w /= 2
  \\  end
  \\  case _ => @panic('bad')
  \\ end
  \\ assert(w == 61.5, 'should be 61.5')
  ;
  try doRuntimeTest(src5);
}

test "type expressions" {
  const src =
  \\ let p = Num
  \\ p = Str
  \\ let q = List{Num}
  \\ q = p
  \\ let x = Map{Str, Num}
  \\ x = p
  \\ let w = [Num, Str, Bool, List{Str}]
  \\ w = [Num, Bool]
  \\ let q = {'bar': List{Map{Str, Num}}}
  \\ q = {'fox': Num, 'foo': Bool, 'bar': List{Map{Str, Num}}}
  \\ let t: Num = 5
  \\ if Num == Num
  \\   t += 5
  \\ else
  \\  t -= 3
  \\ end
  \\ assert(t==10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "is expression" {
  const src =
  \\ type Hoi = Hog
  \\ alias ni = Hog
  \\ println(ni, Hoi)
  \\ assert(ni == Hoi, 'same cos interned')
  \\ # is
  \\ # direct checks
  \\ assert('fox' is Str, 'same-1')
  \\ assert(5 is Num, 'same-2')
  \\ assert([] is List{Any}, 'same-3')
  \\ assert({} is Map{Any, Any}, 'same-4')
  \\ assert(True is Bool, 'same-5')
  \\ assert(None as List{Num}? == None, 'same-6')
  \\ let p: Str? = None
  \\ assert(p == None, 'same-7')
  \\ assert(!!(Just([5]) as List{Num}? != None), 'same-8')
  \\ assert(None == None, 'same-9')
  \\
  \\ # indirect checks
  \\ type Typ = N(Num) | S(Str) | L(List{Num}) | M(Map{Str, Num})
  \\ let n: Typ = M({} as Map{Str, Num})
  \\ match n
  \\  case L([..]) => assert(False, '.1')
  \\  case M({..} as t) => assert(t is Map{Str, Num} == True, 't should be map')
  \\  case _ => assert(False, '.2')
  \\ end
  \\ n = S('foo')
  \\ match n
  \\  case N(t) => assert(False, 't should not be Num')
  \\  case S(t) => assert(t is Str == True, 't should be Str')
  \\  case _ => assert(False, '.3')
  \\ end
  \\ n = N(5)
  \\ match n
  \\  case N(t) => assert(t is Num, 't should be Num')
  \\  case _ => assert(False, '.4')
  \\ end
  \\ assert(!!n is Bool == True, 'should be boolean')
  \\ assert(Bool == Bool is Bool == True, 'True')
  \\ assert(((Bool == Bool) is Bool) == True, 'True')  # same as above
  \\
  \\ # is not
  \\ # direct checks
  \\ assert(!('fox' is not Str), 'same-1')
  \\ assert(!(5 is not Num), 'same-2')
  \\ assert(!([] is not List{Any}), 'same-3')
  \\ assert(!({} is not Map{Any, Any}), 'same-4')
  \\ assert(!(True is not Bool), 'same-5')
  \\ assert(!(None as List{Num}? != None), 'same-6')
  \\ let p: Str? = None
  \\ assert(!(p != None), 'same-7')
  \\ assert(!(Just([5]) as List{Num}? == None), 'same-8')
  \\ assert(!(None != None), 'same-9')
  \\
  \\ # indirect checks
  \\ type Typ = N(Num) | S(Str) | L(List{Num}) | M(Map{Str, Num})
  \\ let n: Typ = M({} as Map{Str, Num})
  \\ match n
  \\  case L([..]) => assert(False, '.1')
  \\  case M({..} as t) => assert(!(t is not Map{Str, Num} == True), 't should be map')
  \\  case _ => assert(False, '.2')
  \\ end
  \\ n = S('foo')
  \\ match n
  \\  case N(t) => assert(False, 't should not be Num')
  \\  case S(t) => assert(!(t is not Str == True), 't should be Str')
  \\  case _ => assert(False, '.3')
  \\ end
  \\ n = N(5)
  \\ match n
  \\  case N(t) => assert(!(t is not Num), 't should be Num')
  \\  case _ => assert(False, '.4')
  \\ end
  \\ assert(!(!!n is not Bool == True), 'oops')
  \\ assert(Bool == Bool is not Bool == True == False, 'ooops')
  \\ assert(!({} is not Map{Str, Num}), 'a map')
  \\ assert(((Bool == Bool) is not Bool) != True, 'not True')  # same as above
  \\ assert(Num != Str, 'Num is not Str')
  ;
  try doRuntimeTest(src);
}

test "short-circuit operator (Maybe)" {
  const src =
  \\ let t: Num? = Just(5)
  \\ assert(t.? == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "assert operator (Maybe)" {
  const src =
  \\ let t: Num? = Just(5)
  \\ assert(t.?? == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-1" {
  const src =
  \\ type StrNum = Strs(Str) | Nums(Num)
  \\ type Star = Strs(Str)
  \\ let x: StrNum? = Just(Strs('foobar') as StrNum)
  \\ let p = 10
  \\ match x
  \\  case Just(Strs(t)) => assert(True, 'ok')
  \\  case Just(Nums(t)) => assert(False and ((t + p) > 5), 'Num')
  \\  case None => assert(False, 'none')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-2" {
  const src =
  \\ type Col = L(List{Num}) | M(Map{Str, Num})
  \\ let x: Col = L([5])
  \\ let p = 10
  \\ match x
  \\   case L(l) => p += l[0]
  \\   case _ => ""
  \\ end
  \\ assert(p == 15, '15')
  ;
  try doRuntimeTest(src);
}

test "narrowing-3" {
  const src =
  \\ type Col = L(List{Num}) | M(Map{Str, Num})
  \\ let x: Col = L([5])
  \\ let p = 12
  \\ match x
  \\   case L([x] as t) => do
  \\    p = x ^ p
  \\    t[0] = p
  \\   end
  \\   case _ => ""
  \\ end
  \\ assert(p == 9, '9')
  ;
  try doRuntimeTest(src);
}

test "narrowing-4" {
  const src =
  \\ type StrNum = Strs(Str) | Nums(Num)
  \\ type Col = L(List{List{StrNum}}) | M(Map{Str, Num})
  \\ let x: Col = L([[Nums(5) as StrNum]])
  \\ let p = 10
  \\ match x
  \\  case L([[Nums(a)]]) => if a + 2 > 0
  \\    p += a
  \\  end
  \\  case _ => ""
  \\ end
  \\ assert(p == 15, '15')
  ;
  try doRuntimeTest(src);
}

test "narrowing-5" {
  const src =
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ type T = L(List{List{NumStr}}) | M(Map{Str, List{NumStr}})
  \\ let x: T = L([[Nums(5) as NumStr]])
  \\ let p = 0
  \\ match x
  \\  case M({'a': [Nums(g), ..]} as m) => if g + 2 > 0
  \\    m['foobar'] = [Nums(1) as NumStr, Nums(2)]
  \\  end
  \\  case M(_) => assert(False, '')
  \\  case L([[Nums(t)]]) => p = t
  \\  case L(_) => assert(False, '')
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-6" {
  const src =
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ type T = L(List{List{NumStr}}) | M(Map{Str, List{NumStr}})
  \\ let x: T = M({'a': [Nums(5) as NumStr], 'b': [] as List{NumStr}})
  \\ let p = 0
  \\ match x
  \\  case M({'a': [Nums(g), ..], ..} as m) => if g + 2 > 0
  \\    m['foobar'] = [Nums(1) as NumStr, Nums(2)]
  \\    match m['foobar']
  \\      case [Nums(_), Nums(t)] => p += t * 2 + 1
  \\      case [..] => ""
  \\    end
  \\  end
  \\  case M(_) => assert(False, '')
  \\  case L([[Nums(t)]]) => p = t
  \\  case L(_) => assert(False, '')
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-7" {
  const src =
  \\ type NumStr = Nums(Num) | Strs(Str)
  \\ let x: List{NumStr} = [Nums(1) as NumStr, Nums(2)]
  \\ let p = 0
  \\ match x
  \\  case [Nums(_), Nums(t)] => p += t * 2 + 1
  \\  case [..] => let k = p
  \\  case _ => ""
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-8" {
  const src =
  \\ let x: Num? = Just(9)
  \\ let p = 0
  \\ match x
  \\  case Just(t) => p = t
  \\  case None => p = 1
  \\ end
  \\ assert(p == 9, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "narrowing-9" {
  const src =
  \\ type StrNum = S(Str) | N(Num)
  \\ let x: StrNum = N(5)
  \\ let p = 25
  \\ match x
  \\  case N(t) => p /= t
  \\  case S(..) => assert(False, "")
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-10" {
  const src =
  \\ type ListNum = N(Num) | L(List{Num})
  \\ type T = Col(List{ListNum}) | Nu(Num)
  \\ let x: T = Col([N(9) as ListNum])
  \\ let p = 90
  \\ match x
  \\  case Col([N(t)] as q) => do
  \\   p /= t
  \\   q[0] = N(p)
  \\  end
  \\  case _ => assert(False, '')
  \\ end
  \\ assert(p == 10, 'p should be 10')
  ;
  try doRuntimeTest(src);
}

test "narrowing-14" {
  const src =
  \\ type T = L(List{Num}) | S(Str)
  \\ let x: T? = Just(L([5]) as T)
  \\ match x.?
  \\  case L([1]) => assert(False, 'no')
  \\  case L([5]) => assert(True, 'yes')
  \\  case L([..]) => assert(False, 'no')
  \\  case S('a') => assert(False, 'no')
  \\  case S(_) => assert(False, 'no')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-15" {
  const src =
  \\ let x: Tuple{List{Num}, Str?} = ([5], Just('foo') as Maybe{Str})
  \\ if x[0] is List{Num} and x[1].? is Str
  \\    x[0][0] += x[1].?.len()
  \\ else
  \\    assert(False, '')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-16" {
  const src =
  \\ let x: Tuple{Str, Num} = ('a', 5)
  \\ let p = 0
  \\ if (x[1] is Num and 5 > 2) or x[1] is Num
  \\    p = x[1] * 5
  \\ else
  \\    assert(False, 'oops')
  \\ end
  \\ assert(p == 25, 'should be 25')
  ;
  try doRuntimeTest(src);
}

test "narrowing-17" {
  const src =
  \\ let x = 0
  \\ let y = 'a'
  \\ if x is Num and x is Num or y is Str
  \\    x # Num | Str
  \\    x += y.len()
  \\ else
  \\    assert(False, 'never')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-18" {
  const src =
  \\ do
  \\   let x: List{Num?} = [Just(5) as Num?]
  \\   let p = x as List{Num?}
  \\   if p[0].? is Num # redundant but okay
  \\      p[0].?? += 12 # type checks because x is a list with one type
  \\   end
  \\   assert(p[0].? == 17, 'should be 17')
  \\ end
  \\ ()
  \\ let x: List{Num?} = [Just(5) as Num?]
  \\ let p = x as List{Num?}
  \\ if p[0].?? is Num # redundant but okay
  \\    p[0].?? += 12
  \\ end
  \\ assert(p[0].?? == 17, 'should be 17')
  \\ _ = ()
  \\ let t: Num = 0
  \\ let v: Tuple{Map{Num, Num}, Tuple{Num, Str}} = ({1: 4}, (15, 'abc'))
  \\ if v[1] is Tuple{Num, Str}
  \\    let p = v[1][0]
  \\    if p is Num
  \\      t += p + 1
  \\    end
  \\ end
  \\ assert(t == 16, 'should be 16')
  ;
  try doRuntimeTest(src);
}

test "narrowing-19" {
  const src = 
  \\ type NumStr = N(Num) | S(Str)
  \\ def fun(n: NumStr)
  \\  if n is S
  \\    return n
  \\  end
  \\  if n is N
  \\    return n
  \\  end
  \\ end
  \\ fun(S('fancy'))
  \\
  \\ def fun(n: NumStr)
  \\  if n is S
  \\    return n
  \\  end
  \\  return n
  \\ end
  \\ fun(N(12))
  ;
  try doRuntimeTest(src);
}

test "narrowing-20" {
  const src =
  \\ type Stuff = A("a") | B("b") | Five(5)
  \\ def fish(p: Stuff)
  \\  if p is A
  \\    return 'nice'
  \\  elif p is B
  \\    return 'good'
  \\  elif p is Five
  \\    return 'okay'
  \\  end
  \\ end
  \\ assert(fish(Five(5)) == 'okay', 'ok')
  \\
  \\ def fun
  \\  let p = 10
  \\  if p < 5
  \\    @exit(2)
  \\  else
  \\    assert(True, 'oops')
  \\  end
  \\  p -= 2
  \\  return p
  \\ end
  \\ assert(fun() == 8, 'should be 8')
  ;
  try doRuntimeTest(src);
}

test "narrowing-21" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ class Fox
  \\    pub x: NumStr = N(5)
  \\    pub u = 12
  \\ end
  \\
  \\ let f = (Fox(), 5)
  \\ let t = 0
  \\ let x = 0
  \\ if f[0] is Fox
  \\   if f[0].x is N
  \\     match f[0].x
  \\      case N(w) => do
  \\        t = w + f[0].u
  \\        f[0].x = N(t)
  \\      end
  \\     end
  \\     x = f[0].u
  \\   end
  \\ end
  \\ assert(t == 17, 't should be 17')
  \\ assert(x == 12, 'x should be 12')
  ;
  try doRuntimeTest(src);
}

test "narrowing-22" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ type FooFox = Fo(Foo) | Fx(Fox)
  \\ class Fox
  \\    pub x: NumStr = N(5)
  \\    pub u = 12
  \\ end
  \\ class Foo
  \\    pub x = 'ok'
  \\    pub u = 13
  \\ end
  \\
  \\ let f: FooFox = Fx(Fox())
  \\ match f
  \\  case Fx(Fox(x=N(t), ..)) => assert(t == 5, 'this should be Fox.x')
  \\  case Fx(Fox(x=S(_), ..)) => assert(False, 'bad')
  \\  case Fo(_) => assert(False, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-23" {
  const src =
  \\ let j = 5
  \\ if j is Num and j > 5
  \\  j
  \\ else
  \\  assert(j + 4 == 9, 'should be 9')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-24" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ let x: Maybe{NumStr} = Just(N(4) as NumStr) as NumStr?
  \\ let z = 0
  \\ match x.?
  \\  case N(t) => z = t + 5
  \\  case S(_) => assert(False, 'oops')
  \\ end
  \\ assert(z == 9, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "narrowing-27" {
  const src =
  \\ let j: Tuple{List{List{Num}}, List{Str}} = ([[5]], ['ok'])
  \\ if j[0] is List{List{Num}} and j[0].len() == 1
  \\  assert(j[0][0] is List{Num}, 'is list')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "Unit narrowing" {
  const src =
  \\ type UnitNum = V(Unit) | N(Num)
  \\ def voidy
  \\ end
  \\
  \\ def fox(x: Bool)
  \\  if x then
  \\    return N(3)
  \\  end
  \\  return V(voidy())
  \\ end
  \\
  \\ let t = fox(False)
  \\ if t is not V then
  \\  match t
  \\    case N(u) => t = N(u + 5)
  \\  end
  \\ end
  \\ [t]
  \\ t = fox(!!1)
  \\ let z = 0
  \\ if t is not V then
  \\  match t
  \\    case N(u) => z = u + 12
  \\  end
  \\ end
  \\ assert(z == 15, 't should be 15')
  ;
  try doRuntimeTest(src);
}

test "constant types" {
  const src =
  \\ type Animal = C("cat") | D("dog") | F("fox")
  \\ alias Cat = 'cat'
  \\ type Even = T(2) | F(4) | S(6) | E(8.6)
  \\ type boolean = T(True) | F(False)
  \\ let p: Animal = Animal.F('fox')
  \\ let x: Even = Even.S(6)
  \\ let y: boolean = boolean.T(True)
  \\ let t: Bool = !!y
  \\ x = Even.E(8.6)
  \\ p = Animal.D('dog')
  \\ y = boolean.F(False)
  \\ let w = (x, p, y, t)
  \\ println(w)
  \\ do
  \\    let p = 'Cat'
  \\    let j: Animal = Animal.C('cat') as Animal
  \\    let x = 0xff
  \\    let q: 0xff = x
  \\ end
  \\ do
  \\    let p: 'Cat' = 'Cat'
  \\    let j: 'Cat' = 'Cat'
  \\    j = p
  \\ end
  \\ let p: 5? = Just(5)
  \\ let k: Num? = None
  \\ k = Just(p.?? as Num + 25)
  \\ assert(k.?? == 30, 'should be 30')
  \\ alias A = 5
  \\ let x: List{A} = [5, 5, 5]
  \\ let y: List{'foo'} = ['foo', 'foo', 'foo']
  \\ let z: Map{'name', Str} = {'name': 'ziord'}
  \\ assert(z['name'] == 'ziord', 'should be ziord')
  \\ assert(5 as 5 as Num + 5 == 10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "while loop" {
  const src =
  \\ do
  \\    let x = 5
  \\    while x is Num and x < 25 do
  \\     let j = 0
  \\     while j < x
  \\       j += 1
  \\     end
  \\     x += j
  \\     continue
  \\     # x += 5
  \\    end
  \\    assert(x == 40, 'x should be 40')
  \\ end
  \\ let x = 5
  \\ while x is Num and x < 25 do
  \\  let j = 0
  \\  while j < x
  \\    j += 1
  \\    continue
  \\  end
  \\  x += j
  \\  break
  \\  # x += 5
  \\ end
  \\ assert(x == 10, 'x should be 10')
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ let x = 5
  \\ while x is Num and x < 25 do
  \\  let j = 0
  \\  while j < x
  \\    j += 1
  \\    continue
  \\  end
  \\  x += j
  \\  break
  \\  # x += 5
  \\ end
  \\ assert(x == 10, 'should be 10')
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ let x = 5
  \\ while x is Num and x < 25 do
  \\  if x % 5 == 0 then
  \\    break
  \\  end
  \\  x += 5
  \\ end
  \\ x
  \\ let x = 5
  \\ while x as Num < 25 do
  \\  let p = x as Num
  \\  if p % 5 != 0 then
  \\    break
  \\  end
  \\  x = p + 5
  \\ end
  \\ x
  \\ let i = 0
  \\ while i < 0xffff
  \\  i += 1
  \\ end
  \\ assert(i == 0o177777, 'should be 65535')
  ;
  try doRuntimeTest(src3);
}

test "functions-0" {
  const src =
  \\ alias T = Num
  \\ def j(a: T): T
  \\  return (a * 2)
  \\ end
  \\ assert(j(5) + 9 == 19, 'should be 19')
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ do
  \\ def fox(a: Num)
  \\  def foo(b: Num)
  \\    return a + b
  \\  end
  \\  return foo
  \\ end
  \\ fox(5)(9) == 14
  \\ let j = fox(5)(8)
  \\ assert(j == 13, 'should be 13')
  \\ end
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ def fib(n: Num): Num
  \\  if n <= 1 then
  \\    return n
  \\  end
  \\  return fib(n - 1) + fib(n - 2)
  \\ end
  \\ assert(fib(13) == 233, 'should be 233')
  ;
  try doRuntimeTest(src3);
  const src4 =
  \\ alias T = Num
  \\ def foo(a: T): T
  \\  let j = 12
  \\  return a * 5 + j
  \\ end
  \\ let j = 4
  \\ let p = 12 * foo(foo(j))
  \\ let q = {j: p}
  \\ assert(q[j] == 2064, 'should be 2064')
  ;
  try doRuntimeTest(src4);
}

test "functions-1" {
  const src = 
  \\ def funny
  \\    def foo{T}(a: T): T
  \\     return a
  \\    end
  \\    let j = foo{Str}('5')
  \\    let k = foo(10)
  \\    let p = foo(56)
  \\    k += 5
  \\    return (j, k, p)
  \\ end
  \\ funny()
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ def fancy{T}(x: T)
  \\  let j: T = x
  \\  return j
  \\ end
  \\ def id{T}(val: T): T
  \\  return val
  \\ end
  \\ (fancy(5), fancy('oops'), fancy(True), id((1, 2, {'a': 'fox'})))
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ def funny2{U}(x: U)
  \\    def foo{T}(a: T, b: U): T
  \\     return a * (b - 2)
  \\    end
  \\    let k = foo(10, x)
  \\    let p = foo(56, x)
  \\    k += 5
  \\    return (k, p)
  \\ end
  \\ funny2(123)
  \\ def funny2{U}
  \\    def foo{T}(a: T, b: U): T
  \\     return a * (b - 2)
  \\    end
  \\    let x: U = 123
  \\    let k = foo(10, x)
  \\    let p = foo(56, x)
  \\    k += 5
  \\    return (k, p)
  \\ end
  \\ funny2{Num}()
  ;
  try doRuntimeTest(src3);
}

test "functions-3" {
  const src =
  \\ def add{T} (k: T, t: Num)
  \\    return (k, t)
  \\ end
  \\ add(3, 4)
  \\ add('fox', 12)
  \\ def add5{T}(a: T)
  \\  return a + 5
  \\ end
  \\ add5(12)
  \\ def add5(a: Num)
  \\  return a + 5 * 2
  \\ end
  \\ add5(12)
  \\ let minus = [def (a: Num, b: Num) => a - b][0]
  \\ minus(132, 12)
  \\ let p = minus
  \\ p(12, 4)
  ;
  try doRuntimeTest(src);
}

test "functions-4" {
  const src =
  \\ do
  \\ def add {T} (k: T, t: Num)
  \\    return (k, t)
  \\ end
  \\ add(3, 4)
  \\ add('fox', 12)
  \\ def add5{T}(a: T)
  \\  return a + 5
  \\ end
  \\ add5(12)
  \\ def add5(a: Num)
  \\  return a + 5 * 2
  \\ end
  \\ add5(12)
  \\ let minus = [def (a: Num, b: Num): Num
  \\  return a - b
  \\ end][0]
  \\ minus(132, 12)
  \\ let p = minus
  \\ p(12, 4)
  \\ end
  \\ let j = def => 5
  \\ j()
  ;
  try doRuntimeTest(src);
}

test "functions-5" {
  const src =
  \\ let j = (89, def(x: Num) => x * 2, def(y: Num) => y + 5)
  \\ if !!j[1]
  \\  if j[0] is Num
  \\    let p = j[0] + j[1](16)
  \\    assert(p == 121, '121')
  \\  end
  \\ end
  \\ j
  \\
  \\ do
  \\  def higher{T, J}(x: Num): fn(J): T
  \\   return def (y: Num): T => x * y
  \\  end
  \\  let mul = higher{Num, Num}(5)
  \\  assert(mul(6) == 30, 'should be 30')
  \\ end
  \\
  \\ def higher{T, J}(x: Num): fn(J): T
  \\  return def (y: Num): T => x * y
  \\ end
  \\ let mul = higher{Num, Num}(5)
  \\ assert(mul(12) == 60, 'should be 60')
  ;
  try doRuntimeTest(src);
}

test "functions-6" {
  const src =
  \\ do
  \\  assert((def (x: Str) 
  \\   return x
  \\  end)('ppp') == "ppp", 'should be "ppp"')
  \\ end
  \\
  \\ let j = 12
  \\ _ = (def => j * 3)() == 36
  \\ _ = [(def => j * 3)][0]() + 12 == 48
  \\
  \\ do
  \\  let j = 12
  \\  assert((def => j * 3)() == 36, 'should be 36')
  \\  _ = [(def => j * 3)][0]() + 12 == 48
  \\  _ = (def => [(def => j * 3)][0]() + 12 == 48)()
  \\ end
  \\ let j = 6
  \\ assert((def => [(def => j * 3)][0]() + 6 == 24)(), 'should be True')
  ;
  try doRuntimeTest(src);
}

test "functions-7" {
  const src =
  \\ do
  \\  do
  \\    def fun
  \\     def read{T}(x: T): List{T}
  \\       return [x,]
  \\     end
  \\     read(5)
  \\     read('fox')
  \\     read(fun)
  \\    end
  \\    fun()
  \\  end
  \\ end
  \\ def apply{T}(x: fn(T):T, param: T): T
  \\  return x(param)
  \\ end
  \\ apply(def (x: Num) => x * x, 5)
  \\ apply(def (x: Str) => x, 'fox')
  \\ do
  \\  def apply{T}(x: fn(T):T, param: T): T
  \\   return param
  \\  end
  \\  apply(def (x: Num) => x * x, 5)
  \\  apply(def (x: Str) => x, 'fox')
  \\ end
  \\ def fun
  \\  def read{T}(x: T): List{T}
  \\    return [x,]
  \\  end
  \\  read(5)
  \\  read('fox')
  \\  read(fun)
  \\ end
  \\ fun()
  ;
  try doRuntimeTest(src);
}

test "functions-7.b" {
  const src =
  \\ def apply{T, K}(a: Num, param: T, x*: fn(T):K): T
  \\  return x[0](param) + param + a
  \\ end
  \\ assert(apply(12, 5, def (x: Num) => x * x) == 42, 'should be 42')
  ;
  try doRuntimeTest(src);
}

test "functions-8" {
  const src =
  \\ do
  \\  do
  \\    def fun
  \\     def read{T}(x: T): List{T}
  \\       return [x,]
  \\     end
  \\     read(5)
  \\     read('fox')
  \\     read(fun)
  \\    end
  \\    fun()
  \\  end
  \\ end
  \\ def apply{T}(x: fn(T):T, param: T): T
  \\  return x(param)
  \\ end
  \\ apply(def (x: Num) => x * x, 5)
  \\ apply(def (x: Str) => x, 'fox')
  \\ do
  \\  def apply{T}(x: fn(T):T, param: T): T
  \\   return param
  \\  end
  \\  apply(def (x: Num) => x * x, 5)
  \\  apply(def (x: Str) => x, 'fox')
  \\ end
  \\ def fun
  \\  def read{T}(x: T): List{T}
  \\    return [x,]
  \\  end
  \\  read(5)
  \\  read('fox')
  \\  read(fun)
  \\ end
  \\ fun()
  \\ do
  \\  assert((def (x: Str) 
  \\   return x
  \\  end)('ppp') == "ppp", 'should be ppp')
  \\ end
  \\
  \\ let j = 12
  \\ assert((def => j * 3)() == 36, 'should be 36')
  \\ assert([(def => j * 3)][0]() + 12 == 48, 'should be 48')
  \\
  \\ do
  \\  let j = 12
  \\  _ = (def => j * 3)() == 36
  \\  _ = [(def => j * 3)][0]() + 12 == 48
  \\  assert((def => [(def => j * 3)][0]() + 12 == 48)(), 'should be 48')
  \\ end
  \\ let j = 6
  \\ assert((def => [(def => j * 3)][0]() + 6 == 24)(), 'True')
  \\ let j = (89, def(x: Num) => x * 2, def(y: Num) => y + 5)
  \\ if !!j[1]
  \\  if j[0] is Num
  \\    assert(j[0] + j[1](16) == 121, 'should be 121')
  \\  end
  \\ end
  \\ j
  \\
  \\ do
  \\  def higher{T, J}(x: Num): fn(J): T
  \\   return def (y: Num): T => x * y
  \\  end
  \\  let mul = higher{Num, Num}(5)
  \\  assert(mul(6) == 30, 'True')
  \\ end
  \\
  \\ def higher{T, J}(x: Num): fn(J): T
  \\  return def (y: Num): T => x * y
  \\ end
  \\ let mul = higher{Num, Num}(5)
  \\ assert(mul(12) == 60, 'should be 60')
  \\ do
  \\  def add {T} (k: T, t: Num)
  \\     return (k, t)
  \\  end
  \\  add(3, 4)
  \\  add('fox', 12)
  \\  def add5{T}(a: T)
  \\   return a + 5
  \\  end
  \\  add5(12)
  \\  def add5(a: Num)
  \\   return a + 5 * 2
  \\  end
  \\  add5(12)
  \\  let minus = [def (a: Num, b: Num): Num
  \\   return a - b
  \\  end][0]
  \\  assert(minus(132, 12) ==  120, 'should be 120')
  \\  let p = minus
  \\  assert(p(12, 4) == 8, 'should be 8')
  \\ end
  \\ let j = def => 5
  \\ j()
  \\
  \\ def add{T} (k: T, t: Num)
  \\    return (k, t)
  \\ end
  \\ add(3, 4)
  \\ add('fox', 12)
  \\ def add5{T}(a: T)
  \\  return a + 5
  \\ end
  \\ add5(12)
  \\ def add5(a: Num)
  \\  return a + 5 * 2
  \\ end
  \\ add5(12)
  \\ let minus = [def (a: Num, b: Num) => a - b][0]
  \\ assert(minus(132, 12) ==  120, 'should be 120')
  \\ let p = minus
  \\ assert(p(12, 4) == 8, 'should be 8')
  \\
  \\ def funny2{U}(x: U)
  \\    def foo{T}(a: T, b: U): T
  \\     return a * (b - 2)
  \\    end
  \\    let k = foo(10, x)
  \\    let p = foo(56, x)
  \\    k += 5
  \\    return (k, p)
  \\ end
  \\ funny2(123)
  \\ def funny2{U}
  \\    def foo{T}(a: T, b: U): T
  \\     return a * (b - 2)
  \\    end
  \\    let x: U = 123
  \\    let k = foo(10, x)
  \\    let p = foo(56, x)
  \\    k += 5
  \\    assert(k == 1215, 'should be 1215')
  \\    assert(p == 6776, 'should be 6776')
  \\    return (k, p)
  \\ end
  \\ funny2{Num}()
  \\ def funny
  \\    def foo{T}(a: T): T
  \\     return a
  \\    end
  \\    let j = foo{Str}('5')
  \\    let k = foo(10)
  \\    let p = foo(56)
  \\    k += 5
  \\    assert(k == 15, 'should be 15')
  \\    assert(p == 56, 'should be 56')
  \\    return (j, k, p)
  \\ end
  \\ funny()
  \\ def fancy{T}(x: T)
  \\  let j: T = x
  \\  return j
  \\ end
  \\ def id{T}(val: T): T
  \\  return val
  \\ end
  \\ (fancy(5), fancy('oops'), fancy(True), id([1, 2, 12]))
  ;
  try doRuntimeTest(src);
}

test "functions-9" {
  const src =
  \\ type NSB = N(Num) | S(Str) | B(Bool)
  \\ def ret3(n: Num): NSB
  \\  if n < 5
  \\    return N(3)
  \\  end
  \\  if n < 12
  \\    return S('hey')
  \\  end
  \\  if n > 15
  \\    return B(True)
  \\  end
  \\  return S('oops')
  \\ end
  \\
  \\ def ret(n: NSB)
  \\  if n is S
  \\    return n
  \\  end
  \\  if n is B
  \\    return n
  \\  end
  \\  match n
  \\    case N(t) => return N(t + 12)
  \\  end
  \\ end
  \\ 
  \\ _ = (def (x: Num) => x * x)(12)
  \\ _ = (def (x: Num)
  \\  return x * x
  \\ end)(12)
  \\
  \\ ret3(7)
  \\ let t = ret(N(7))
  \\ match t
  \\  case N(t) => assert(t == 19, 'should be 19')
  \\  case _ => assert(False, 'yeah')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "functions-10" {
  const src = 
  \\ def fox{T}(x: T): fn(T): Num
  \\  def fun(p: T): Num
  \\    return (p * x)
  \\  end
  \\  return fun
  \\ end
  \\ let j = fox(5)
  \\ assert(j(3) == 15, 'should be 15')
  \\ assert(fox(5)(3) == 15, 'should be 15')
  ;
  try doRuntimeTest(src);
  const src2 =
   \\ def fox(x: Num): fn(Num): Num
  \\  return def (p: Num): Num  => p * x
  \\ end
  \\
  \\ let j = fox(5)
  \\ assert(j(4) == 20, 'should be 20')
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ type NS = N(Num) | S(Str)
  \\ def big: NS
  \\  return N(5)
  \\ end
  \\ let j = big()
  \\ match j
  \\  case N(t) => assert(t == 5, 'should be 5')
  \\  case _ => assert(False, 'oops')
  \\ end
  ;
  try doRuntimeTest(src3);
  const src4 =
  \\ type Ty{T} = A(T) | B(Str)
  \\ alias Fun = fn(T): Ty{T}
  \\ alias T = Num
  \\
  \\ def fox(x: T): Fun
  \\  def fun(p: T): Ty{T}
  \\    return A(p * x)
  \\  end
  \\  return fun
  \\ end
  \\ 
  \\ let x = fox(2)(3)
  \\ match x
  \\  case A(t) => assert((t) == 6, 'should be 6')
  \\  case _ => assert(False, 'oops')
  \\ end
  ;
  try doRuntimeTest(src4);
}

test "functions-11" {
  const src =
  \\ let j = [def (x: Num) => x * x, def (y: Num) => ~y]
  \\ let t = 1
  \\ let p = j[t]
  \\ assert(p(6) == -7, 'should be -7')
  \\
  \\ let j = [def (x: Num) => x * x, def (y: Num) => ~y]
  \\ let t = 1 * 0
  \\ let p = j[t]
  \\ assert(p(12) == 144, 'should be 144')
  \\
  \\ assert([def (x: Num) => x * x, def (y: Num) => ~y][-1](t + 7) == -8, 'should be -8')
  \\
  \\ let j = [def (x: Num) => x * x, def (y: Num) => y >> (1 << y)]
  \\ let v = [def (x: Num) => x * x, def (y: Num) => ~y][t - 1](t + 7)
  \\ assert(v == -8, 'should be -8')
  ;
  try doRuntimeTest(src);
}

test "functions-13" {
  const src =
  \\ def fun: Unit
  \\  let p = 10
  \\  p += 5
  \\ end
  \\
  \\ let j = fun()
  \\ assert(j is Unit, 'should be Unit')
  ;
  try doRuntimeTest(src);
}

test "functions-14" {
  const src =
  \\ def fun: Unit
  \\  let p = 10
  \\  p += 5
  \\  return assert(!!fun, 'good')
  \\ end
  \\
  \\ fun()
  ;
  try doRuntimeTest(src);
}

test "functions-15" {
  const src =
  \\ def fun
  \\ end
  \\ [fun()]
  \\ assert([fun()][0] is Unit, 'is Unit')
  ;
  try doRuntimeTest(src);
}

test "functions-16-varargs" {
  const src =
  \\ do
  \\ def fun(args*: Num)
  \\  return args
  \\ end
  \\
  \\ let j = fun(1, 2, 3)[0] + 12
  \\ assert(j == 13, 'j should be 13')
  \\ fun()
  \\ end
  \\
  \\ def foo(a: Num, rest*: Num)
  \\  return (a * rest[0], rest)
  \\ end
  \\
  \\ let res = foo(5, 3)
  \\ if res[0] is Num
  \\  assert(res[0]==15, 'should be 15')
  \\ else
  \\  assert(False, 'oops')
  \\ end
  \\ res = foo(12, 2, 3, 4, 5, 6)
  \\ if res[0] is Num
  \\  assert(res[0]==24, 'should be 24')
  \\ else
  \\  assert(False, 'oops')
  \\ end
  \\ if res[1] is List{Num}
  \\  assert(res[1][0]==2, 'should be 2')
  \\  assert(res[1][4]==6, 'should be 6')
  \\ else
  \\  assert(False, 'oopsx')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "functions-17" {
  const src =
  \\ do
  \\  def foo{T}(x*: T)
  \\   println(x)
  \\   return x
  \\  end
  \\
  \\  let j = ['fox']
  \\  foo{Num}(6, 7)
  \\  do
  \\   println(1, 2, 3, 4)
  \\  end
  \\ end
  \\ println(println(), 'jeryr')
  \\ _ = (def (p*:Any) => println(p))(1, 2, 'a', 'b', False, True, None)
  \\ let j: Any? = None
  ;
  try doRuntimeTest(src);
}

test "functions-18" {
  const src =
  \\ def fun(): Num
  \\  if 1 > 2
  \\    return 5
  \\  else
  \\    return 6
  \\  end
  \\ end
  \\
  \\ assert(fun() == 6, "should be 6")
  ;
  try doRuntimeTest(src);
}
  
test "functions-19" {
  const src =
  \\ alias T = Num
  \\ def id{T}(v: T): (fn (T): Tuple{T, T})
  \\  return def (x: T) => (x, v)
  \\ end
  \\ let j = id(8)
  \\ let k = j(6)
  \\ assert(k[0] == 6, 'k[0] == 6')
  \\ assert(k[1] == 8, 'k[1] == 8')
  ;
  try doRuntimeTest(src);
}

test "functions-20" {
  const src =
  \\ assert((def (x: List{Num}) 
  \\   return x
  \\ end)([5]) != [5], 'objects are not strictly equal')
  ;
  try doRuntimeTest(src);
}

test "functions-21" {
  const src =
  \\ def fun(a*: Any)
  \\  println('a is', a)
  \\  assert(!a.len(), 'should be 0')
  \\ end
  \\
  \\ fun()
  ;
  try doRuntimeTest(src);
}

test "functions-22.<narrowing with do-blocks>" {
  const src =
  \\ def test(n: Num)
  \\ let k = 5
  \\  if n >= 1 and n <= 3
  \\    do
  \\      return n
  \\    end
  \\  else
  \\    do
  \\      let k = 'f'
  \\    end
  \\    do
  \\      k -= 1
  \\      return n - 1
  \\    end
  \\  end
  \\ end
  \\ assert(test(5) - 3 == 1, 'should be 1')
  ;
  try doRuntimeTest(src);
}

test "functions-23.<narrowing in do-blocks>" {
  const src =
  \\ def test(n: Num)
  \\  do
  \\    if n > 2
  \\      return 5
  \\    else
  \\      return 10
  \\    end
  \\  end
  \\ end
  \\ assert(test(5) + 1 == 6, 'should be 6')
  ;
  try doRuntimeTest(src);
}

test "functions-24.<function arguments>" {
  const src =
  \\ def funny(t: List{Str})
  \\  println(t)
  \\ end
  \\ funny([] as List{Str})
  ;
  try doRuntimeTest(src);
}

test "functions-26.<dotted types>" {
  const src =
  \\ type Fox = S | T
  \\ def fun(): Fox.S
  \\  return Fox.S
  \\ end
  \\ assert(fun() == Fox.S, 'should be same')
  ;
  try doRuntimeTest(src);
}

test "functions-25.<Unit>" {
  const src =
  \\ def foo()
  \\  println('yay')
  \\ end
  \\ let j:Unit = foo()
  \\ println('j is', j)
  ;
  try doRuntimeTest(src);
}

test "functions-25.<Unit/Never/return>" {
  const src =
  \\ def check(n: Num)
  \\  if n is Num
  \\    return n
  \\  end
  \\ end
  \\ let j = check(12)
  \\ j += 4
  \\ assert(j == 16, 'should be 16')
  ;
  try doRuntimeTest(src);
}

test "builtin-functions" {
  const src =
  \\ assert(True, 'ok')
  \\ assert(!!@exit, '@exit')
  \\ assert(!!assert, 'assert')
  \\ # assert(!!@panic, '@panic')
  \\ assert(!!print, 'print')
  \\ assert(!!println, 'println')
  \\ assert(!!@string, '@string')
  \\ _ = (@exit, assert)
  ;
  try doRuntimeTest(src);
}

test "no-strict-varargs-immutability" {
  const src =
  \\ def fun(x*: Num)
  \\  println('x before', x)
  \\  let before = x.len()
  \\  change(x)
  \\  println('x after', x)
  \\  assert(x.len() > before, 'True')
  \\ end
  \\
  \\ def change(x: List{Num})
  \\  x.append(12)
  \\ end
  \\ 
  \\ fun(1, 2, 3, 4, 5, 6)
  ;
  try doRuntimeTest(src);
}

test "errors-1" {
  const src =
  \\ do
  \\  def fun(x: Num)
  \\   if x > 2
  \\     return Ok(())
  \\   end
  \\   return ('foo')!
  \\  end
  \\  let tmp = fun(1)
  \\  match tmp
  \\    case Ok(() as t) => assert(False, 'bad')
  \\    case Error(t) => assert(t == 'foo', 'value should be foo')
  \\  end
  \\  let j = try fun(3)
  \\  let k = fun(3) orelse |e| @panic(e)
  \\  println(j, k)
  \\ end
  \\
  \\ def fun(x: Num)
  \\  if x > 2
  \\    return Ok(())
  \\  end
  \\  return ('foo')!
  \\ end
  \\ let tmp = fun(1)
  \\ match tmp
  \\   case Ok(() as t) => assert(False, 'bad')
  \\   case Error(t) => assert(t == 'foo', 'value should be foo')
  \\ end
  \\ let j = try fun(3)
  \\ let k = fun(3) orelse |e| @panic(e)
  \\ println(j, k)
  ;
  try doRuntimeTest(src);
}

test "errors-2" {
  const src =
  \\ def stup(x: Num)
  \\  if x > 2
  \\    return None
  \\  else
  \\    return Error('oops')
  \\  end
  \\ end
  \\ match stup(1)
  \\  case Error(t) => assert(t == 'oops', 'ok')
  \\  case None => assert(False, 'nah')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "errors-2b" {
  const src =
  \\ def fun(x: Num)
  \\  if x > 5
  \\    return Ok(x)
  \\  else
  \\    return ('bad')!
  \\  end
  \\ end
  \\
  \\ def test()
  \\  let k = fun(12) orelse |e| 15
  \\  return Ok(k)
  \\ end
  \\ match test()
  \\   case Ok(p) => assert(p == 12, 'p should be 12')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "errors-3" {
  const src =
  \\ do
  \\  def fun(x: Num)
  \\   if x > 5
  \\     return Ok(12)
  \\   else
  \\     return ('bad')!
  \\   end
  \\  end
  \\  
  \\  def fancy()
  \\   let j = fun(2) orelse 4
  \\   return Just(Ok(j))
  \\  end
  \\  let k = fancy()
  \\  match k
  \\    case Just(Ok(t)) => assert(t == 4, 'ok')
  \\  end
  \\ end
  ;
  try doRuntimeTest(src);
  const src2 =
  \\  def fun(x: Num)
  \\   if x > 5
  \\     return Ok(12)
  \\   else
  \\     return ('bad')!
  \\   end
  \\  end
  \\  
  \\  def fancy()
  \\   let j = fun(2) orelse 4
  \\   return Just(Ok(j))
  \\  end
  \\  let k = fancy()
  \\  match k
  \\    case Just(Ok(t)) => assert(t == 4, 'ok')
  \\  end
  ;
  try doRuntimeTest(src2);
}

test "errors-5" {
  const src =
  \\ do
  \\  def fun(x: Num)
  \\   if x > 5
  \\     return Ok(x | 3)
  \\   else
  \\     return ('bad')!
  \\   end
  \\  end
  \\  
  \\  def fancy{T}(x: T)
  \\   let j = fun(x) orelse 5
  \\   match j
  \\    case n => return n + 5
  \\   end
  \\  end
  \\  let k = fancy(12)
  \\  k += 5
  \\  assert(k == 25, 'k should be 25')
  \\ end
  \\
  \\ def fun(x: Num)
  \\  if x > 5
  \\    return Ok(x | 3)
  \\  else
  \\    return ('bad')!
  \\  end
  \\ end
  \\ 
  \\ def fancy{T}(x: T)
  \\  let j = Ok(fun(x) orelse 5)
  \\  match j
  \\   case Ok(n) => return n + 5
  \\  end
  \\ end
  \\ let k = fancy(12)
  \\ k += 5
  \\ assert(k == 25, 'k should be 25')
  ;
  try doRuntimeTest(src);
}

test "errors-6" {
  const src =
  \\ def fancy(x: Num)
  \\  if x > 5
  \\    return ('bad')!
  \\  else 
  \\    return Ok(x * 4)
  \\  end
  \\ end
  \\ let e = 3
  \\ let j = fancy(22) orelse |e| 3
  \\ let k = 0
  \\ match j
  \\  case n => k += n + 5
  \\ end
  \\ # println(e, j)
  \\ assert(e == 3 and k == 8, 'e should not change')
  ;
  try doRuntimeTest(src);
}

test "errors-7" {
  const src =
  \\ let j = ('bad')!
  \\ match j
  \\  case Error('bad') => assert(True, 'value should be bad')
  \\  case Error(_) => assert(False, 'nono')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "simple-classes-1" {
  const src =
  \\ class Fox
  \\    pub x: Num
  \\    pub u = 12
  \\    def init(): Unit
  \\      self.x = 0
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\ end
  \\
  \\ let f = Fox()
  \\ let p = f.x + 5
  \\ assert(f.pulse() == f, "instances should be the same")
  \\ assert(f.pulse().x + 5 == p, "field should be equal")
  \\ assert(5 == p, "p should be 5")
  \\ let j: Fox = f
  \\ assert(j is Fox, "j should be type Fox")
  \\ assert(j.u == 12, 'field "u" should be 12')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-2" {
  const src =
  \\ class Fox
  \\    pub x: Num
  \\    pub u = 12
  \\    def init(): Unit
  \\      self.x = 0
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ class Racoon
  \\    pub x: Num
  \\    pub u = 12
  \\    def init(): Unit
  \\      self.x = self.u
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ let f = Fox()
  \\ let r = Racoon()
  \\ r.x
  \\ assert(r.x == 12, 'r should be 12')
  \\ assert(r.x == r.u, 'fields x and u should be equal')
  \\ assert(f.u == r.x, 'should be equal')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-3" {
  const src =
  \\ class Foo
  \\  pub x: Num = 10
  \\ end
  \\
  \\ let j = Foo()
  \\ assert(j.x == 10, 'j.x should be 10')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-4" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ class Fox
  \\    pub x: NumStr = N(5)
  \\    pub u = 12
  \\ end
  \\
  \\ let f = (Fox(), 5)
  \\ let t = 0
  \\ let x = 0
  \\ if f[0] is Fox
  \\   if f[0].x is N
  \\     match f[0].x
  \\      case N(w) => do
  \\        t = w + f[0].u
  \\        f[0].x = N(t)
  \\      end
  \\     end
  \\     x = f[0].u
  \\   end
  \\ end
  \\ assert(t == 17, 't should be 17')
  \\ assert(x == 12, 'x should be 12')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-5" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ class Fox
  \\    x: NumStr = N(5)
  \\    u = 12
  \\    pub def foo()
  \\      return self.u * 3
  \\    end
  \\ end
  \\ let k = Fox()
  \\ let t = k.foo
  \\ assert(t() == 36, 'should be 36')
  ;
  try doRuntimeTest(src);
}

test "method-calls" {
  const src =
  \\ let j = 'foo'
  \\ println('len is', j.len())
  \\ assert(j.len() == 3, 'len should be 3')
  \\
  \\ def fun{T}(x: List{T})
  \\  x.append('oopsy'.len())
  \\  return x
  \\ end
  \\ let _t = fun{Num}([1, 2, 3, 4])
  \\ println('-->', _t, 'oopsy'.len())
  \\ assert('oopsy'.len() == 5, 'should be 5')
  \\ assert(_t[-1] == 5, 'last item should be 5')
  \\
  \\ let x = []
  \\ _ = x.len
  \\ x.append(8)
  \\ println(x)
  \\ assert(x.len() == 1, 'len should be 1')
  ;
  try doRuntimeTest(src);
}

test "functions-1.<narrowing-return>" {
  const src =
  \\ type NumStr = N(Num) | S(Str)
  \\ def chee(n: NumStr): Num
  \\  if n is N
  \\    return 5
  \\  end
  \\  return 12
  \\ end
  \\ assert(chee(S('oops')) == 12, 'should be 12')
  \\ assert(chee(N(4)) == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-1.<call & dot access mutation>" {
  const src =
  \\ class Foxy
  \\  pub x = [1]
  \\ end
  \\
  \\ def fun(f: Foxy)
  \\  return f
  \\ end
  \\
  \\ let k = Foxy()
  \\ fun(k).x = [1, 2, 3]
  \\ assert(k.x.len() == 3 and k.x.get(2).? == 3, 'should be')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-2.<instance list assignment>" {
  const src =
  \\ class Dod
  \\ end
  \\ let j = [Dod()]
  \\ let k: List{Dod} = j
  \\ assert(k.len() == j.len(), 'should be same')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-1" {
  const src =
  \\ class Fox{T}
  \\    pub x: List{T}
  \\    def init(x*: T): Unit
  \\      self.x = x
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\
  \\    pub def getGen()
  \\      alias T = Tuple{Str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{Num}(6, 7, 8)
  \\ let t: Fox{Num} = x
  \\ assert(t.pulse().x[0] == 6, 'first arg is 6')
  \\ assert(t.pulse().x[1] == 7, 'first arg is 7')
  \\ assert(t.pulse().x[2] == 8, 'first arg is 8')
  \\ # println(t.x)
  \\ assert(t.x[0] == 6, 'first arg is 6')
  \\ assert(t.x[1] == 7, 'second arg is 7')
  \\ assert(t.x[2] == 8, 'third arg is 8')
  \\ let z = Fox{'mia'}('mia')
  \\ assert(z.x[0] == 'mia', 'index 0 gives mia')
  \\ assert(t.getGen()(('a',)) == 'a', 'should be 1')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-2" {
  const src =
  \\ let j = [1, 2, 3]
  \\ let p = j.pop().? + 4
  \\ j.append(4)
  \\ let k = (j, p)
  \\ p += k.len()
  \\ assert(p == 9, 'p should be 9')
  \\ 
  \\ let x = {'a': 5, 'b': 6}
  \\ assert(x.keys().len() == 2, 'length of keys should be 2')
  \\ assert(x.values().len() == 2, 'length of values should be 2')
  \\ assert(x.items().len() == 2, 'length of items should be 2')
  \\ assert(x.get('a').? + 12 == 17, 'sum should be 17')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-3" {
  const src =
  \\ let j = []
  \\ let _t = j.append
  \\ j.append(5)
  \\ _t(1)
  \\ assert(j.len() == 2, 'len should be 2')
  \\ let x = j.pop().?
  \\ assert(x == 1, 'x should be 1')
  \\ assert(j.pop().? == 5, 'should be 5')
  \\ assert(j.len() == 0, 'len should be 0 now')
  \\ let t = {'a': 1, 'b': 5, 'c': 12}
  \\ println(t, t.get('d'))
  \\ println(t.keys())
  \\ println(t.values())
  \\ println(t.items())
  \\ t.set('a', 0xff)
  \\ println(t)
  \\ let foo = t.get('p')
  \\ assert(foo == None, 'foo must be None')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-4" {
  const src =
  \\ class Fox{T}
  \\    pub x: List{T}
  \\    def init(x*: T): Unit
  \\      self.x = x
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\
  \\    pub def getGen()
  \\      alias T = Tuple{Str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{Num}(6, 7, 8)
  \\ let t: Fox{Num} = x
  \\ t.pulse().x[0] + 12
  \\ t.pulse().getGen()(('starters',))
  \\ assert(t.pulse().pulse().x.len() == 3, 'should be 3')
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia', 'mia')
  \\ let j: Fox{'mia'} = w
  \\ assert(j.pulse().x.len() == 4, 'len should be 4')
  \\
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j = Fox{'mia'}('mia')
  \\ assert(j.x.len() == 1, 'len should be 1')
  \\ j = w
  \\ assert(j.pulse().x.len() == 3, 'len should be 3')
  \\
  \\ alias Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j:Poo{'mia'} = Fox{'mia'}('mia', 'mia')
  \\ assert(j.x.len() == 2, 'len should be 2')
  \\ j = w
  \\ assert(j.x.len() == 3, 'len should be 3')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-5" {
  const src =
  \\ class Fox{T}
  \\    pub x: List{T}
  \\    def init(x*: T): Unit
  \\      self.x = x
  \\    end
  \\ end
  \\ let x = Fox{Num}(6, 7, 8)
  \\ let y = Fox{Str}("a", "b")
  \\ type FF{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: FF{Num, Str} = F1(x)
  \\ match j
  \\  case F1(Fox(_) as w) => do
  \\    if w is Fox{Num}
  \\       w.x = [w.x[0] + 5, w.x[1]]
  \\       assert(w.x[0] == 11, 'should be 11')
  \\    else
  \\      assert(False, 'oopsy')
  \\    end
  \\  end
  \\  case F2(_ as q) => do
  \\    if q is Fox{Str}
  \\     q.x = ["hello world", q.x[1]]
  \\     println(q, q.x)
  \\    else 
  \\      assert(False, 'oops')
  \\    end
  \\  end
  \\ end
  ;
  try doRuntimeTest(src);
}

test "generic-classes-6<mutual-recursion>" {
  const src =
  \\ class Foo{T}
  \\   pub def see(x: T): Bar{T}
  \\    return Bar{T}()
  \\   end
  \\ end
  \\
  \\ class Bar{U}
  \\    pub def ees(y: U): Foo{U}
  \\      return Foo{U}()
  \\    end
  \\ end
  \\
  \\ let j = Foo{Num}()
  \\ let t = j.see((5))
  \\ t is Bar{Num} |> assert(*, 'should be True')
  \\ j is Foo{Num} |> assert(*, 'should be True')
  ;
  try doRuntimeTest(src);
}

test "mutual recursion" {
  const src =
  \\ def mutA(x: Num)
  \\  if x > 2
  \\    return mutB(x)
  \\  else
  \\    return x
  \\  end
  \\ end
  \\ def mutB(y: Num)
  \\  if y > 2
  \\    return mutA(y)
  \\  else
  \\    return y
  \\  end
  \\ end
  \\ mutA(1) + mutB(2) |> assert(* == 3, 'should be 3')
  ;
  try doRuntimeTest(src);
}

test "generic call linking" {
  const src =
  \\ class Bar
  \\ end
  \\
  \\ def format{A, B, C}(a: A, b: B, c: C): Str
  \\  return "oops"
  \\ end
  \\
  \\ let x = Bar()
  \\ format{Bar, Bar, Bar}(x, x, x) == "oops" |> assert(*, "should be same")
  ;
  try doRuntimeTest(src);
}

test "labeled-argument" {
  const src =
  \\ def fun(x: Str, y: Num, a: List{Num}, b: Result{Unit, Str})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y=5, a=[2, 3], x='oo', b=('oops')!)
  \\ fun(y=5, a=[2, 3], b=('oops')!, x='oo')
  \\ 
  \\ def fun(x: Str, y: Num, a*: List{Num})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  println('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y=5, a=[2, 3], x='oo', a=[1, 2], a=[5, 6, 7])
  \\ fun(y=5, a=[2, 3], x='oo', a=[1, 2], a=[5, 6, 7])
  \\ fun(y=5, a=[2, 3], x='oo', a=[1, 2], a=[5, 6, 7])
  \\
  \\ def fun(x: Str, y: Num, a*: List{Num})
  \\  assert(y == 5, 'y should not change')
  \\  println('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(a=[0x1, 0x2], y=5, x='a', a=[2, 3])
  ;
  try doRuntimeTest(src);
}

test "labeled-argument-2" {
  const src =
  \\ class Fun
  \\  pub a: Num
  \\  pub b: Str
  \\  def init(a: Num, b: Str)
  \\    self.a = a
  \\    self.b = b
  \\  end
  \\  pub def send(data: List{Any})
  \\    let i = 0
  \\    while i < data.len()
  \\      println('sending...', data[i])
  \\      i += 1
  \\    end
  \\  end
  \\ end
  \\ let f = Fun(b='oops', a=12)
  \\ println(f.a, f.b)
  \\ f.send(data=['a' as Any, 1, f])
  ;
  try doRuntimeTest(src);
}

test "builtin-list" {
  const src =
  \\ let j = [1, 2, 3, 4, 5]
  \\ # append, len
  \\ assert(j.len() == 5, 'should be 5')
  \\ j.append(6)
  \\ assert(j.len() == 6, 'should be 6')
  \\ # pop
  \\ let t = j.pop().??
  \\ assert(t == 6, 'should be 6')
  \\ # get
  \\ assert(j.get(-1).?? == 5, 'should be 5')
  \\ assert(j.get(1).?? == 2, 'should be 2')
  \\ assert(j.get(5) == None, 'should be None')
  ;
  try doRuntimeTest(src);
}

test "builtin-tuple" {
  const src =
  \\ let j = (1, 2, 3, 4, 5)
  \\ # len
  \\ assert(j.len() == 5, 'should be 5')
  \\ # get
  // \\ TODO: constant folding for negative indices assert(j[-1] == 5, 'should be 5')
  \\ assert(j[1] == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "builtin-map" {
  const src =
  \\ let j = {'a': 1, 'b': 2, 'c': 3, 'd': 4}
  \\ # len
  \\ assert(j.len() == 4, 'should be 4')
  \\ # get
  \\ assert(j.get('a').?? == 1, 'should be 1')
  \\ assert(j.get('f') == None, 'should be None')
  \\ # set
  \\ assert(!j.set('a', 12), 'should be True')
  \\ assert(j.set('f', 7), 'should be True')
  \\ assert(j.len() == 5, 'should be 5')
  \\ assert(j.get('f').?? == 7, 'should be 7')
  \\ # delete
  \\ assert(j.delete('a'), 'should be True')
  \\ assert(j.len() == 4, 'should be 4')
  \\ # keys, values
  \\ assert(j.keys().len() == 4, 'should be 4')
  \\ assert(j.keys()[0] == 'b', 'should be b')
  \\ assert(j.keys()[-1] == 'f', 'should be f')
  \\ assert(j.values().len() == 4, 'should be 4')
  \\ assert(j.values()[0] == 2, 'should be 2')
  \\ assert(j.values()[-1] == 7, 'should be 7')
  \\ # remove
  \\ assert(j.delete('b'), 'should be True')
  \\ assert(j.len() == 3, 'should be 3')
  \\ # items
  \\ let itm0 = j.items()[0]
  \\ assert(itm0[0] == 'c', 'should be c')
  \\ assert(itm0[1] == 3, 'should be 3')
  \\ assert(j.items().len() == 3, 'should be 3')
  \\ # entries
  \\ assert(j.entries().len() == 6, 'should be 6')
  ;
  try doRuntimeTest(src);
}

test "builtin-err" {
  const src =
  \\ let t = (1, 2, 3, 4, 5)
  \\ let j = (t)!
  \\ # value
  \\ match j
  \\  case Error(x) => assert(x == t, 'should be same')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "builtin-Str" {
  const src =
  \\ let j = "coolstuff"
  \\ # len
  \\ assert(j.len() == 9, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "tagged unions" {
  const src =
  \\ type Option = Some(List{Num}) | Nil
  \\ let j: Option = Some([5])
  \\ j = Nil
  \\ # -- #
  \\ type Tree{T} = Branch(Tree{T}, Tree{T}) | Node(T)
  \\ let tree: Tree{Num} = Branch(Node(1) as Tree{Num}, Node(2) as Tree{Num})
  \\ # -- #
  \\ type Many{T} = More(Many{T}) | One(T)
  \\ let x: Many{Num} = More(One(45) as Many{Num})
  \\ # -- #
  \\ alias T = Num
  \\ type Tree = Branch(Tree, Tree) | Node(T)
  \\ let tree: Tree = Branch(Node(1), Node(2))
  \\ # -- #
  \\ type Tree{T} = Leaf | Node(T, Tree{T}, Tree{T})
  \\ let tree2: Tree{Num} = Node(5, Node(1, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf)))
  \\ tree2 = Leaf
  \\ let tree2 = Node(5, Node(1, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf)))
  \\ type Tree{T} = Node(val:T, lhs:Tree{T}, rhs:Tree{T}) | Leaf
  \\ let tree2 = Node(val=4, lhs=Leaf, Node(val=3, lhs=Leaf, rhs=Leaf))
  \\ let j: Tree{Num} = Node(2, Leaf, Leaf)
  \\ j = tree2
  \\ # -- #
  \\ type Pair{K, V} = Pair(K, V)
  \\ let p:Pair{Str, Str} = Pair('a', 'b') 
  \\ # -- #
  \\ type Pair{K, V} = Pair(K, V)
  \\ let p:Pair{'a', 'b'} = Pair('a' as 'a', 'b' as 'b')
  \\ println(p)
  \\ let p:Pair{Str, Str} = Pair('a', 'b')
  \\ println(p)
  \\ # -- #
  \\ let k = Just(None)
  \\ println(k)
  \\ # -- #
  \\ let t: Maybe{Maybe{Num}} = Just(Just(5) as Maybe{Num})
  \\ println(Just(Just(None)), t)
  \\ # -- #
  \\ type R = Strs(Str) | Nums(Num) | Col(List{R})
  \\ [Strs('foo') as R] as List{R}
  ;
  try doRuntimeTest(src);
}

test "patterns-1.<ordinary match>" {
  const src =
  \\ let e1 = ''
  \\ let e2 = ''
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a' as a, 'b' as b) as d => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case ('q', 'k') => println('third')
  \\  case _ => println("last")
  \\ end
  \\ assert(e1 == 'a', 'should be a')
  \\ assert(e2 == 'b', 'should be b')
  \\
  ;
  try doRuntimeTest(src);
}

test "patterns-2.<scopes>" {
  const src =
  \\ let o = '--'
  \\ let z = False
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a', 'b' as o) as d => z = True
  \\  case ('q', 'k') => println('third')
  \\  case _ => println("last")
  \\ end
  \\ assert(z, 'should match')
  \\ assert(o == '--', 'o should not change')
  ;
  try doRuntimeTest(src);
}

test "patterns-3.<nested match>" {
  const src =
  \\ let z = False
  \\ match (('a', 'b'), ('x', 'y'))
  \\
  \\  case (('x', 'y'), ..) => do
  \\    let p = z
  \\    println('first')
  \\  end
  \\  case (('a', 'p', ..), u) => do
  \\    let b = z
  \\    println('here!')
  \\  end
  \\  case (('a', t, ..) as d, ..) => do
  \\    let h = z
  \\    println('second')
  \\    z = True
  \\  end
  \\  case (('x', k), y) => do
  \\    let v = z
  \\    println('third')
  \\  end
  \\  case (..) as n => let j = n
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-4.<match on unions>" {
  const src =
  \\ type Cons = A | B | C
  \\ let z = False
  \\ let j: Cons = B
  \\ match j
  \\  case A => println('a')
  \\  case B => do
  \\    println('ok')
  \\    z = True
  \\  end
  \\  case C => println('hmm')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-5.<match on classes>" {
  const src =
  \\ let z = False
  \\ class Ant
  \\  pub a = 5
  \\ end
  \\ class Rat
  \\  pub x = 'yes'
  \\  pub y = 'no'
  \\  pub z = 'ok'
  \\ end
  \\ type Animal = A(Ant) | R(Rat)
  \\ let j = R(Rat()) as Animal
  \\ match j
  \\  case A(Ant(a)) if a > 2 => println(12)
  \\  case A(Ant(5)) => println(13)
  \\  case A(Ant(_)) => println(40)
  \\  case R(Rat(x=x, y='no.', ..)) if x == 'yes' => println(9)
  \\  case R(Rat(x='ok', y='12', ..)) => println(15)
  \\  case R(Rat(x='yes' as a, y='no' as p, ..)) as t => do
  \\    println(19, t, a, p)
  \\    z = True
  \\  end
  \\  case R(Rat(..)) => println(30)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-6.<tuple exhaustiveness>" {
  const src =
  \\ let z = False
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a', 'b' as o) as d => z = True
  \\  case ('q', 'k') => println('third')
  \\  case (..) => println("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-7.<list exhaustiveness>" {
  const src =
  \\ let z = False
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => println('first')
  \\  case [('a', 'b' as o)] as d => z = True
  \\  case [('q', 'k')] => println('third')
  \\  case [..] => println("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-8.<match exhaustiveness>" {
  const src =
  \\ type Animal = C(Cat) | D(Dog)
  \\ let z = False
  \\ class Cat
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Animal = C(Cat())
  \\ let z = False
  \\ match p
  \\  case D(Dog()) => println('good')
  \\  case C(Cat()) => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-9.<nested match on classes>" {
  const src =
  \\ let j = [] as List{Num}
  \\ class Pooh
  \\  pub x = [1, 2]
  \\ end
  \\ class Cat
  \\  pub x = [Dog()]
  \\ end
  \\ class Dog
  \\  pub x = Pooh()
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p: Animal = C(Cat())
  \\ match p
  \\  case C(Cat([Dog(Pooh(a))])) as x => j = a
  \\  case _ => assert(False, 'bad match')
  \\ end
  \\ assert(j[0] == 1, 'should be 1')
  \\ assert(j[1] == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-10.<nested match on classes>" {
  const src =
  \\ class Pooh
  \\  pub x = [1, 2]
  \\ end
  \\ class Cat
  \\  pub y = [Dog()]
  \\ end
  \\ class Dog
  \\  pub z = [Pooh()]
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p = C(Cat())
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case C(Cat([Dog([Pooh([a, b])])])) as x => do
  \\    println('x:', x, 'a:', a, 'b:', b)
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case _ => assert(False, 'bad match') # TODO: warns. False positive
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-11.<nested match on classes>" {
  const src =
  \\ class Pooh
  \\  pub x = [1, 2]
  \\ end
  \\ class Cat
  \\  pub x = [Dog()]
  \\ end
  \\ class Dog
  \\  pub x = [Pooh()]
  \\ end
  \\ type Animal = C(Cat) | D(Dog)
  \\ let p = C(Cat()) as Animal
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case C(Cat([Dog([Pooh([a, b])])])) as x => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case _ => assert(False, 'bad match')
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-12.<nested match on classes>" {
  const src =
  \\ class Pooh
  \\  pub x = [1, 2]
  \\ end
  \\ class Cat
  \\  pub x = [Dog()]
  \\ end
  \\ class Dog
  \\  pub x = [Pooh()]
  \\ end
  \\ type Animal = 
  \\  | C(Cat) 
  \\  | D(Dog)
  \\ let p = C(Cat()) as Animal
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case C(Cat(x=[Dog(x=[Pooh(x=[a, b])])])) as x => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case _ => assert(False, 'bad match')
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-13.<match on Bool>" {
  const src =
  \\ let z = False
  \\ let j = [False]
  \\ match j
  \\  case [a, ..] => match a
  \\    case True => println('yay')
  \\    case False as w => z = !w
  \\  end
  \\  case [..] => println('bad')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-14.<match on Bool>" {
  const src =
  \\ let z = False
  \\ match (1 < 2)
  \\  case False => println('nay')
  \\  case True as t => z = !!t
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-15.<ranges>" {
  const src =
  \\ let z = False
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => println('hey')
  \\    case 3..5 => z = True
  \\    case _ => println('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-16.<ranges>" {
  const src =
  \\ let z = False
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => println('hey')
  \\    case 3..5 => z = True
  \\    case _ => println('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-17.<nullable types>" {
  const src =
  \\ let z = False
  \\ type AB = A("a") | B("b")
  \\ let j: AB? = Just(B("b") as AB)
  \\ match j
  \\   case Just(A("a")) => println('a!')
  \\   case Just(B("b")) => z = True
  \\   case None => println('nah!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-18.<nullable types>" {
  const src =
  \\ let z = False
  \\ type AB = A("a") | B("b")
  \\ let j: AB? = None
  \\ match j
  \\   case Just(A("a")) => println('a!')
  \\   case Just(B("b")) => println('nah!')
  \\   case None => z = !z
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-19.<match on generics>" {
  const src =
  \\ class Fox{T}
  \\  pub j: T
  \\  def init(j: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let z = False
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{Str,Num} = F1(Fox('pin'))
  \\ match j
  \\  case F2(Fox(6)) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya Num')
  \\  case F1(Fox('pin')) as x => z = True
  \\  case F1(Fox(_)) => println('caught ya Str')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-20.<match on generics>" {
  const src =
  \\ class Fox{T}
  \\  pub j: T
  \\  def init(j: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant
  \\ end
  \\ let z = False
  \\ type Fx{T, V} = 
  \\  F1(Fox{T}) 
  \\  | F2(Fox{V}) 
  \\  | F3(Ant)
  \\ let j: Fx{Str,Num} = F3(Ant())
  \\ match j
  \\  case F2(Fox(6)) => println('whew')
  \\  case F1(Fox('pin')) as x => println('pin')
  \\  case F2(Fox(_)) => println('caught ya Str')
  \\  case F1(Fox(_)) => println('caught ya Num')
  \\  case F3(Ant()) => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-21.<match on generics>" {
  const src =
  \\ class Fox{T}
  \\  pub j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{Str, Num} = F1(Fox{Str}('pin', 'pan'))
  \\ let z = False
  \\ match j
  \\  case F1(Fox(['pin', 'pan'])) as x => z = True
  \\  case F1(Fox(_)) => println('caught ya Str')
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya Num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-22.<match on generics>" {
  const src =
  \\ class Fox{T}
  \\  pub j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{Str, Num} = F1(Fox{Str}('pin', 'pan'))
  \\ let z = False
  \\ match j
  \\  case F1(Fox(['^_^',])) => println('caught ya Str')
  \\  case F1(Fox(..)) as x => z = True
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya Num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-23.<match on generics>" {
  const src =
  \\ class Fox{T}
  \\  pub j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{Str, Num} = F1(Fox{Str}('pin', 'pan'))
  \\ let z = False
  \\ match j
  \\  case F1(Fox(['pin', ..])) as x => z = True
  \\  case F1(Fox(_)) => println('caught ya Str')
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya Num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-24.<match on generics>" {
  const src =
  \\ class Fox{T}
  \\  pub j: List{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant{T}
  \\ end
  \\ type Fx{T, V, U} = F1(Fox{T}) | F2(Fox{V}) | F3(Ant{U})
  \\ let j: Fx{Str, Num, Str} = F3(Ant{Str}())
  \\ let z = False
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya Num')
  \\  case F3(Ant()) => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-25.<match on rested>" {
  const src =
  \\ class Bug
  \\  pub x = 5
  \\  pub y = 10
  \\  pub z = 12
  \\ end
  \\ let z = False
  \\ match Bug()
  \\  case Bug(1, _, ..) => println('bad')
  \\  case Bug(5, _, ..) => z = True
  \\  case _ => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-26.<match on rested>" {
  const src =
  \\ let z = False
  \\ let j = ('a', True, 'b', 2, 'c', 3)
  \\ match j
  \\  case ('x', _, ..) => println('yay')
  \\  case ('a', _ as t, ..) => z = t
  \\  case (a, b, ..) => println('nay')
  \\  case _ => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-27.<match on rested>" {
  const src =
  \\ let z = False
  \\ let j = ['a', '1', 'b', '2', 'c', '3']
  \\ match j
  \\  case ['x', _] => println('yay')
  \\  case ['a', _ as t, ..] => z = True
  \\  case [a, b] => println('nay')
  \\  case _ => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-28.<match on generics>" {
  const src =
  \\ let z = False
  \\ class Oky{A}
  \\  pub val: A
  \\  def init(val: A)
  \\    self.val = val
  \\  end
  \\ end
  \\
  \\ class Err{B}
  \\  pub val: B
  \\  def init(val: B)
  \\    self.val = val
  \\  end
  \\ end
  \\ 
  \\ type Res{A, B} = ok(Oky{A}) | err(Err{B})
  \\ let j = err(Err('bad')) as Res{Num, Str}
  \\
  \\ match j
  \\  case err(Err(v)) => z = True
  \\  case ok(Oky(k)) => println('ok:', k)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-29.<match on maps>" {
  const src =
  \\ let z = False
  \\ let j = {'a': 1, 'b': 2, 'c': 3}
  \\ match j
  \\  case {'x': _} => println('yay')
  \\  case {'a': _ as t, ..} => z = True
  \\  case {a: b} => println('nay')
  \\  case _ => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-30.<match on maps>" {
  const src =
  \\ class Fox
  \\  pub url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let z = False
  \\ type Fmt = Class(Fox) | Strs(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Strs("txt")}
  \\ match foo
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Strs("txt"),} => z = True
  \\  case {"sound": _, "format": _} => println(1)
  \\  case {..} => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-31.<match on maps>" {
  const src =
  \\ class Fox
  \\  pub url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let foo = [Fox('fin.co')]
  \\ let z = False
  \\ match foo
  \\  case [ Fox(url) as b ] => do
  \\    assert(url as Bool and b as Bool, 'should be True')
  \\    z = True
  \\  end
  \\  case [ .. ] => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-32.<match on maps>" {
  const src =
  \\ class Fox
  \\  pub url: Str
  \\  def init(url: Str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Strs(Str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Strs("txt")}
  \\ let z = True
  \\ match foo
  \\  case {"sounds": _, "format": _} => println(1)
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": _,} => z = True
  \\  case {..} => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-33.<match on maps>" {
  const src =
  \\ let foo = [{"sound": 1, "format": 2}, {"pin": 3, "pan": 4}]
  \\ let z = False
  \\ match foo
  \\  case [{"sound": 1}] => println('first')
  \\  case [{"sound": 2, ..}] => println('second')
  \\  case [{"sound": _, ..}, ..] => z = True
  \\  case [{..}, ..] => println('fourth')
  \\  case [..] => println('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-34.<match on maps>" {
  const src =
  \\ let foo = [{"sound": 1, "format": 2}, {"pin": 3, "pan": 4}]
  \\ let z = False
  \\ match foo
  \\  case [{"sound": 1}] => println('first')
  \\  case [{"sound": 2, ..}] => println('second')
  \\  case [{..}, ..] => z = True
  \\  case [..] => println('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-35.<guards with blocks>" {
  const src =
  \\ let foo = [5, 3]
  \\ let z = False
  \\ match foo
  \\  case [x, y] if x > y => do
  \\    z = !!x and !!y
  \\  end
  \\  case [..] => println('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-35b.<guards with blocks>" {
  const src =
  \\ let z = False
  \\ match {'a': False, 'b': True, 'c': False}.entries()
  \\  case [..] as t if !z => do
  \\    assert(!z, '...')
  \\    z = !z
  \\  end
  \\  case _ => !z
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-36.<match on maps>" {
  const src =
  \\ let z = False
  \\ match {'a': False, 'b': True, 'c': False}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} => z = p
  \\  case {..} => println('has something or none')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-37.<match in functions>" {
  const src =
  \\ def check(n: Num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let t = check(7)
  \\ t += 3
  \\ assert(t == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "patterns-38.<match in functions>" {
  const src =
  \\ def fib(n: Num)
  \\  match n
  \\    case 0..1 => return n
  \\    case _ => return fib(n - 1) + fib(n - 2)
  \\  end
  \\ end
  \\ let j = fib(12)
  \\ j += 4
  \\ assert(j == 148, 'should be 148')
  ;
  try doRuntimeTest(src);
}

test "patterns-39.<match in functions>" {
  const src =
  \\ def fib(n: Num)
  \\  match n
  \\    case 0..1 => do
  \\      return n
  \\    end
  \\    case _ => do
  \\      return fib(n - 1) + fib(n - 2)
  \\    end
  \\  end
  \\ end
  \\ let j = fib(12)
  \\ j += 4
  \\ assert(j == 148, 'should be 148')
  ;
  try doRuntimeTest(src);
}

test "patterns-40.<match in functions>" {
  const src =
  \\ def check(n: Num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 18..50 as q => return q + n 
  \\   case _ => return 3
  \\  end
  \\ end
  \\ let t = check(26)
  \\ t += 3
  \\ assert(t == 55, 'should be 55')
  ;
  try doRuntimeTest(src);
}

test "patterns-41.<match in functions>" {
  const src =
  \\ def check(n: Num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 18..50 as q => return (q + n) / 2 # same as
  \\   case _ => return 3
  \\  end
  \\ end
  \\ def fib(n: Num)
  \\  match n
  \\    case 0..1 => do
  \\      return n
  \\    end
  \\    case _ => do
  \\      return fib(n - 1) + fib(n - 2)
  \\    end
  \\  end
  \\ end
  \\ let j = fib(check(26))
  \\ j += 4
  \\ assert(j == 121397, 'should be 121397')
  ;
  try doRuntimeTest(src);
}
test "patterns-42.<match in functions>" {
  const src =
  \\ def check(n: Num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 11..50 as q => return q + n 
  \\   case _ => return 3
  \\  end
  \\ end
  \\ def fib(n: Num)
  \\  let t = check(n)
  \\  match t
  \\    case 0..1 => do
  \\      return n
  \\    end
  \\    case _ => do
  \\      return fib(n - 1) + fib(n - 2)
  \\    end
  \\  end
  \\ end
  \\ let j = fib(12)
  \\ j += 4
  \\ assert(j == 203, 'should be 203')
  ;
  try doRuntimeTest(src);
}

test "patterns-43.<match in functions (mutually recursive)>" {
  const src =
  \\ def check(n: Num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 11..50 as q => return fib(q / 2)
  \\   case _ => return 3
  \\  end
  \\ end
  \\ def fib(n: Num)
  \\  let t = check(n)
  \\  match t
  \\    case 0..1 => do
  \\      return n
  \\    end
  \\    case _ => do
  \\      let j = 5
  \\      let p = 10
  \\      p += 12 * j
  \\      return fib(n - 1) + fib(n - 2)
  \\    end
  \\  end
  \\ end
  \\ let j = fib(12)
  \\ j += 4
  \\ assert(fib(12) + 4 == 203, 'should be 203')
  \\ assert(fib(13) + 4 == 326, 'should be 326')
  ;
  try doRuntimeTest(src);
}

test "patterns-44.<match on maps>" {
  const src =
  \\ let z = False
  \\ match {'a': False, 'b': True, 'c': False}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} => println('has something or none')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-45.<match on maps>" {
  const src =
  \\ let z = False
  \\ match {'ab': False, 'b': True, 'c': False}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} as t if !z => z = !!t.len()
  \\  case {..} => println('got something!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-46.<match on maps>" {
  const src =
  \\ let z = False
  \\ match {'ab': False, 'b': True, 'c': False}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} as t if z => z = False
  \\  case {..} => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-47.<match on maps>" {
  const src =
  \\ let z = False
  \\ match {'ab': False, 'b': True, 'c': False}
  \\  case {..} as t if !z => z = !!t.len()
  \\  case {..} => println('got something!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-48.<match on maps (guarded rested)>" {
  const src =
  \\ let z = False
  \\ match {'a': False, 'b': True, 'c': False}
  \\  case {..} as t if z => assert(z, 'should be False')
  \\  case _ => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-49.<match on maps>" {
  const src =
  \\ let z = False
  \\ match {'ab': False, 'b': True, 'c': False}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} as t if z => z = False
  \\  case _ => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-50.<match on lists (guarded rested)" {
  const src =
  \\ let z = False
  \\ match ('a', False, 'b', True, 'c', False)
  \\  case ('x', _, 'c', _, ..) => println('has keys "x" and "c"')
  \\  case ('a', _, 'b', _ as p, ..) if z => println(z)
  \\  case (..) if !z => z = True
  \\  case (..) => println('otherwise')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-51.<match on lists (guarded rested)>" {
  const src =
  \\ let z = False
  \\ match {'a': False, 'b': True, 'c': False}.entries()
  \\  case [@Key('a'), @Value(False), ..] as t if z => assert(z, 'should be False')
  \\  case [..] as t if z => assert(z, 'should be False')
  \\  case _ => z = True
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-52.<error patterns>" {
  const src =
  \\ let z = False
  \\ def goodOrBad(n: Num)
  \\   if n > 25
  \\    return ('oops')!
  \\   end
  \\   return Ok(n * 2)
  \\ end
  \\
  \\ match goodOrBad(30)
  \\  case Error(error) => z = !!error.len()
  \\  case Ok(1..10) as x => println(x)
  \\  case _ as t => println('def is', t)
  \\ end
  \\ assert(z, 'should be matched')
  \\
  \\ z = False
  \\ match goodOrBad(15)
  \\  case Error(error) => println(error)
  \\  case Ok(1..30) as x => z = True
  \\  case _ as t => println('def is', t)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-53.<block transform pattern>" {
  const src =
  \\ let j: Num = 1
  \\ match j
  \\  case t => println(t)
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-54.<non unique tags>" {
  const src =
  \\ type T{K, V} = K(K) | V(V)
  \\ let x: T{Str, Num} = V(6)
  \\ match x
  \\  case K(t) => assert(False, 'bad')
  \\  case V(u) => assert(u == 6, 'okay')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-55.<unused tag params>" {
  const src =
  \\ type T{K, V} = K
  \\ let x: T{Str, Num} = K
  \\ println(x)
  \\ let x: T{Str, Num} = K
  \\ match x
  \\  case K => assert(True, 'okay')
  \\  case _ => assert(False, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<generic qualified tag access>" {
  const src =
  \\ type T{K, V} = K
  \\ let x: T{Str, Num} = K
  \\ println(x)
  \\ let x: T{Str, Num} = K
  \\ match x
  \\  case K => assert(True, 'okay')
  \\  case _ => assert(False, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<or patterns in tags>" {
  const src =
  \\ type StrNum = Strs(Str) | Nums(Num)
  \\ let x: StrNum? = Just(Strs('foobar') as StrNum)
  \\ let p = 10
  \\ match x
  \\  case Just(Strs(f) | Nums(q)) => assert(True, 'yes')
  \\  case None => assert(False, 'yada')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<qualified tag access>" {
  const src =
  \\ type T = K(Str) | J(Num)
  \\ let x: T = T.K('fox')
  \\ println(x)
  \\ let x: T = T.J(5)
  \\ match x
  \\  case J(t) => assert(!!t, 'okay')
  \\  case _ => assert(False, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<shadowing via aliased capture>" {
  const src =
  \\ def fun(x: Num)
  \\  match x
  \\    case 1..100 as x => return x
  \\    case 101..200 => return 12
  \\    case _ => return 0
  \\  end
  \\ end
  \\
  \\ assert(fun(5) == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<annotated tags>" {
  const src =
  \\ type T = Tag(a: Num, b: Str, c: Bool)
  \\  match Tag(a=5, b='oops', c=False)
  \\    case Tag(a=5, b='oopsy', c=False) => assert(False, 'oops')
  \\    case Tag(a=_, b=_, c=_) => assert(True, 'yes!')
  \\  end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<annotated tags - exhaustiveness>" {
  const src =
  \\ type T = Tag(a: Num, b: Str, c: Bool)
  \\  match Tag(a=5, b='oops', c=False)
  \\    case Tag(a=_, b=_, c=False) => assert(True, 'good')
  \\    case Tag(a=_, b=_, c=True) => assert(False, 'bad')
  \\  end
  ;
  try doRuntimeTest(src);
}

test "patterns-57.<complete-patterns>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (Just(a), Just(b)) => assert(True, 'a')
  \\  case (None, Just(_)) => assert(False, 'b')
  \\  case (Just(_), None) => assert(False, 'c')
  \\  case (None, None) => assert(False, 'd')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-58.<near-complete-patterns>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (None, None) => assert(False, 'a')
  \\  case (Just(_), None) => assert(False, 'b')
  \\  case (_, Just(_)) => assert(True, 'd')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-59.<near-complete-patterns>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (None, None) => assert(False, 'a')
  \\  case (Just(a), Just(b)) => assert(True, 'b')
  \\  case (None, _) => assert(False, 'c')
  \\  case (_, None) => assert(False, 'd')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-60.<near-complete-patterns>" {
  const src =
  \\ let i = [1, 2]
  \\ let k = (i.get(0), i.get(1))
  \\ match k
  \\  case (None, None) => assert(False, 'a')
  \\  case (None, _) => assert(False, 'b')
  \\  case (_, None) => assert(False, 'c')
  \\  case (Just(_), Just(_)) => assert(True, 'd')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-61.<near-complete-patterns>" {
  const src =
  \\ match ([], [])
  \\  case ([], _) => assert(True, 'a')
  \\  case (_, []) => assert(False, 'b')
  \\  case ([l], [r]) => assert(False, 'c')
  \\  case ([l], [r, ..rs]) => assert(False, 'd')
  \\  case ([l, ..ls], [r]) => assert(False, 'e')
  \\  case ([l, ..ls], [r, ..rs]) => assert(False, 'f')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-62.<near-complete-patterns>" {
  const src =
  \\ _ = match ([1], ['a'])
  \\  case ([], _) => 'a'
  \\  case (_, []) => 'b'
  \\  case ([l], [r]) => 'c'
  \\  case ([l], [r, ..rs]) => 'd'
  \\  case ([l, ..ls], [r]) => 'e'
  \\  case ([l, ..ls], [r, ..rs]) => 'f'
  \\ end == 'c' |> assert(*, 'should be True')
  ;
  try doRuntimeTest(src);
}

test "binary tree .1" {
  const src =
  \\ type Tree{a} =
  \\  | Node(val: a, lhs: Tree{a}, rhs: Tree{a})
  \\  | Empty
  \\ def print_tree{a}(tree: Tree{a})
  \\  match tree
  \\    case Empty => println("Empty")
  \\    case Node(val, lhs, rhs) => do
  \\      println("Node:", val)
  \\      print("Left: ")
  \\      print_tree(lhs)
  \\      print("Right: ")
  \\      print_tree(rhs)
  \\    end
  \\  end
  \\ end
  \\ let tree: Tree{Num} = Node(
  \\    1,
  \\    Node (
  \\      2,
  \\      Node (3, Empty, Empty),
  \\      Node (4, Empty, Empty)
  \\    ),
  \\    Node (
  \\      5,
  \\      Node (6, Empty, Empty),
  \\      Node (7, Empty, Empty)
  \\    )
  \\  )
  \\ print_tree(tree)
  ;
  try doRuntimeTest(src);
}
test "binary tree .2" {
  const src =
  \\ type Tree{a} =
  \\  | Node(val: a, lhs: Tree{a}, rhs: Tree{a})
  \\  | Empty
  \\ def print_tree{a}(tree: Tree{a})
  \\  match tree
  \\    case Empty => println("Empty")
  \\    case Node(val, lhs, rhs) => do
  \\      println("Node:", val)
  \\      print("Left: ")
  \\      print_tree(lhs)
  \\      print("Right: ")
  \\      print_tree(rhs)
  \\    end
  \\  end
  \\ end
  \\ let tree = Node(
  \\    1,
  \\    Node (
  \\      2,
  \\      Node (3, Empty, Empty),
  \\      Node (4, Empty, Empty)
  \\    ),
  \\    Node (
  \\      5,
  \\      Node (6, Empty, Empty),
  \\      Node (7, Empty, Empty)
  \\    )
  \\  )
  \\ print_tree(tree as Tree{Num})
  ;
  try doRuntimeTest(src);
}

test "match expressions .1" {
  const src =
  \\ def think(n: Num)
  \\  if n > 0xff
  \\    return Ok(n * 0xff)
  \\  end
  \\  return Error('bad')
  \\ end
  \\ let t = [1, 2, 3]
  \\ let j = match t
  \\  case [a, b, c] => a + b + c
  \\  case _ => 0
  \\ end
  \\ # println(j)
  \\ assert(j == 6, 'should be 6')
  \\ let p = j + match think(j)
  \\  case Ok(n) => n
  \\  case Error(_) => j / 2
  \\ end + 15
  \\ assert(p == 24, 'should be 24')
  \\ do
  \\   def think(n: Num)
  \\    if n > 0xff
  \\      return Ok(n * 0xff)
  \\    end
  \\    return Error('bad')
  \\   end
  \\   let t = [1, 2, 3]
  \\   let j = match t
  \\    case [a, b, c] => a + b + c
  \\    case _ => 0
  \\   end
  \\   # println(j)
  \\   assert(j == 6, 'should be 6')
  \\   let p = j + (
  \\      match think(j * 150) with
  \\        case Ok(n) => n
  \\        case Error(_) => j / 2
  \\      end
  \\    ) + 15
  \\   assert(p == 0x38091, 'should be 0x38091')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "match expressions .2" {
  const src =
  \\ def think(n: Num)
  \\  if n > 0xff
  \\    return Ok(n * 0xff)
  \\  end
  \\  return Error('bad')
  \\ end
  \\ 
  \\ def fun(t: List{Num})
  \\  let j = match t
  \\   case [a, b, c] => a + b + c
  \\   case _ => 0
  \\  end
  \\  # println(j)
  \\  assert(j == 6, 'should be 6')
  \\ end
  \\ fun([1, 2, 3])
  \\
  \\ do
  \\  def think(n: Num)
  \\   if n > 0xff
  \\     return Ok(n * 0xff)
  \\   end
  \\   return Error('bad')
  \\  end
  \\  
  \\  def fun(t: List{Num})
  \\   let j = match t
  \\    case [a, b, c] => a + b + c
  \\    case _ => 0
  \\   end
  \\   # println(j)
  \\   assert(j == 6, 'should be 6')
  \\  end
  \\  fun([1, 2, 3])
  \\ end
  ;
  try doRuntimeTest(src);
}

test "match expressions .3" {
  const src =
  \\ let t = (match 5 case 1..3 => 10 case 4..10 as n => n * 2 case _ => 0 end)
  \\ assert(t == 10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "match expressions .4" {
  const src =
  \\ let x = [1, 2, 3, 4, 5, 6, 7]
  \\ let t = match x
  \\  case [1, 2, 3, ..k] if k.len() > 2 => k
  \\  case _ => [5]
  \\ end
  \\ assert(t[0] == 4, 'should be 4')
  ;
  try doRuntimeTest(src);
}

test "match expressions .5 <captured rest pattern>" {
  const src =
  \\ let x = [1, 2, 3, 4, 5, 6, 7]
  \\ def sum_list(l: List{Num})
  \\  return match l
  \\   case [] => 0
  \\   case [h, ..t] => h + sum_list(t)
  \\  end
  \\ end
  \\ assert(sum_list(x) == 28, 'should be 28')
  ;
  try doRuntimeTest(src);
}

test "match expressions .6 <captured rest pattern>" {
  const src =
  \\ let x = [1, 2, 3, 4, 5, 6, 7]
  \\ let add = def (a: Num, b: Num) => a + b
  \\ def reduce(l: List{Num}, func: fn(Num, Num):Num, init: Num)
  \\  return match l
  \\   case [] => init
  \\   case [h, ..t] => func(reduce(t, func, init), h)
  \\  end
  \\ end
  \\ assert(reduce(x, add, 0) == 28, 'should be 28')
  ;
  try doRuntimeTest(src);
}

test "match expressions .7 <captured rest pattern>" {
  const src =
  \\ let x = [1, 2, 3, 4, 5, 6, 7]
  \\ let add = def (a: Num, b: Num) => a + b
  \\ def reduce{T}(l: List{T}, func: fn(T, T):T, init: T)
  \\  return match l
  \\   case [] => init
  \\   case [h, ..t] => func(reduce(t, func, init), h)
  \\  end
  \\ end
  \\ assert(reduce(x, add, 0) == 28, 'should be 28')
  ;
  try doRuntimeTest(src);
}

test "match constant patterns .1" {
  const src =
  \\ class F
  \\ end
  \\ 
  \\ type FooBar = A(F) | B(Num)
  \\ 
  \\ let a: FooBar = B(12)
  \\ 
  \\ match a
  \\   case A(f) => assert(False, 'nope')
  \\   case B(foo) => assert(foo + 5 == 17, 'should be 17')
  \\ end
  \\ 
  \\ const FOO_CONST = 124
  \\ 
  \\ match 124
  \\   case FOO_CONST => assert(FOO_CONST == 124, 'should be 124')
  \\   case _ => assert(False, 'unreachable')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "match dot patterns .1" {
  const src =
  \\ type MyStuff = A | B | C 
  \\
  \\ match MyStuff.A
  \\   case MyStuff.A => assert(True, 'should be True')
  \\   case MyStuff.B => assert(False, 'should be False')
  \\   case MyStuff.C => assert(False, 'should be False')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "aspec.<methods 1>" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{Num}
  \\  end
  \\
  \\  pub def fox()
  \\    return match Fish()
  \\      case Fish(x, y, j) => 2 + y
  \\    end
  \\  end
  \\ end
  \\ assert(Fish().fox() == 8, 'should be 8')
  ;
  try doRuntimeTest(src);
}

test "aspec.<methods 2>" {
  const src =
  \\ class Fish
  \\  pub x: Str
  \\  pub y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{Num}
  \\  end
  \\
  \\  pub def fox()
  \\    return match self
  \\      case Fish(x, y, j) => 2 + y
  \\    end
  \\  end
  \\ end
  \\ assert(Fish().fox() == 8, 'should be 8')
  ;
  try doRuntimeTest(src);
}

test "aspec.<methods 3>" {
  const src =
  \\ class Fish
  \\  x: Str
  \\  y: Num
  \\  j: List{Num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{Num}
  \\  end
  \\
  \\  def doStuff()
  \\    self.y += 1
  \\    return self.y
  \\  end
  \\
  \\  pub def fox()
  \\    return self.doStuff()
  \\  end
  \\ end
  \\ assert(Fish().fox() == 7, 'should be 7')
  ;
  try doRuntimeTest(src);
}

test "unused generic tparams" {
  const src =
  \\ type T{K} = B(K)
  \\ alias F{P} = Num
  \\ let j = B(5) as T{F{Str}}
  \\ match j
  \\  case B(t) => assert(t == 5, 'should be 5')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "unused generic tparams.2" {
  const src =
  \\ type T{K} = B(F{K})
  \\ alias F{P} = Num
  \\ let j = B(5) as T{F{Str}}
  \\ match j
  \\  case B(t) => assert(t == 5, 'should be 5')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "tags with function types" {
  const src =
  \\ type Fun{T} = OneArg(fn(T):T) | TwoArg(fn(T, T): T)
  \\ let one = def (a: Num) => a * a
  \\ let two = def (a: Num, b: Num) => a * b
  \\ let j: Fun{Num} = OneArg(one)
  \\ j = TwoArg(two)
  \\ let k = match j
  \\  case OneArg(f) => f(6)
  \\  case TwoArg(f) => f(6, 7)
  \\ end
  \\ assert(k == 42, 'should be 42')
  ;
  try doRuntimeTest(src);
}

test "non-boolean conditions" {
  const src =
  \\ type T = A | B
  \\ let j = T.A
  \\ if j as Bool
  \\   assert(True, 'should be True')
  \\ else
  \\   assert(False, 'should not be False')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "nullable assertions" {
  const src =
  \\ let x: Tuple{List{Num}, Str?} = ([5], Just('foo') as Maybe{Str})
  \\ if x[0] is List{Num} and x[1].?? is Str
  \\    x[0][0] += x[1].??.len()
  \\ else
  \\    assert(False, '')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "pipelines .1" {
  const src =
  \\ let foo = 5
  \\ let x = "fox".len()
  \\ let bar = def (n: Num) => n * 5
  \\ let foobar = def (k: Num, y: Num) => k + y
  \\ let a = foo 
  \\  |> bar 
  \\  |> foobar(x, *)
  \\ a |> println(*, *, *)
  \\ a |> assert(28 == *, 'should be 28')
  ;
  try doRuntimeTest(src);
}

test "pipelines .2" {
  const src =
  \\ [1, 2, 3] 
  \\ |> *.len() 
  \\ |> assert(* == 3, 'should be 3')
  ;
  try doRuntimeTest(src);
}

test "pipelines .3" {
  const src =
  \\ let j = 1 |> (*, *, *, *)
  \\ j |> println
  \\ j |> assert(*.len() == 4 and *[0] == 1, 'should be these things')
  ;
  try doRuntimeTest(src);
}

test "pipelines .4" {
  const src =
  \\ println |> *(5 |> *)
  \\ assert |> *(5 |> * == 5, 'should be 5')
  \\ assert |> *(6 != 5, 'should not be 5')
  ;
  try doRuntimeTest(src);
}

test "pipelines .5" {
  const src =
  \\ def foo()
  \\  return Error('oops') as Result{Num, Str}
  \\ end
  \\ let j = 5 |> (foo() orelse *)
  \\ j |> println
  \\ j |> assert(* == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "pipelines .6" {
  const src =
  \\ let j = 5
  \\ let k = j |> match *
  \\  case 1..6 as p => p |> println(*)
  \\  case _ as w => w |> println(*, 'oops')
  \\ end
  \\ k is Unit |> assert(*, 'should be True')
  ;
  try doRuntimeTest(src);
}

test "pipelines .7" {
  const src =
  \\ let j = 5
  \\ let k = j |> match *
  \\  case 1..6 as p => p |> println(*)
  \\  case _ as w => w |> println(*, 'oops')
  \\ end
  \\ k is Unit |> assert(*, 'should be True')
  ;
  try doRuntimeTest(src);
}

test "pipelines .8" {
  const src =
  \\ def foo()
  \\  return Error('oops') as Result{Num, Str}
  \\ end
  \\ let j = 5 |> (foo() orelse *)
  \\ j |> println
  \\ j |> assert(* == 5, 'should be 5')
  \\ let k = j |> match *
  \\  case 1..6 as p => True
  \\  case _ as w => False
  \\ end
  \\ k |> assert(*, 'should be True')
  ;
  try doRuntimeTest(src);
}

test "pipelines .9 <statefulness>" {
  const src =
  \\ let x = 0
  \\ def stateful()
  \\  x += 1
  \\  return x
  \\ end
  \\
  \\ let j = stateful() |> (*, *, *, *)
  \\ assert(x == 1, 'should be 1')
  ;
  try doRuntimeTest(src);
}

test "string" {
  const src =
  \\ let j = (1, 2, "a")
  \\ @string(j) |> assert(* == "(1, 2, 'a')", 'should be same')
  \\ let j = (1, 2, "a", [None as Maybe{Num}, Just(5)], Just('oops'))
  \\ @string(j) |> assert(* == "(1, 2, 'a', [None, Just(5)], Just('oops'))", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "Str concat .1" {
  const src =
  \\ let j = "This is "
  \\ j.concat("a very beautiful day!") |> println
  \\ j.concat("a very beautiful day!") |> assert(* == "This is a very beautiful day!", 'should be same')
  \\ let j = "the "
  \\ "quick fox" 
  \\  |> j.concat 
  \\  |> assert(* == "the quick fox", 'should be same')
  \\ "quick fox"
  \\  |> j.concat 
  \\  |> (* == "the quick fox") 
  \\  |> assert(*, 'should be same')
  ;
  try doRuntimeTest(src);
}

test "Str concat .2" {
  const src =
  \\ let j = (1, 2, "a", [None as Maybe{Num}, Just(5)], Just('oops'))
  \\ _ = (@string(j) <> " something " <> @string(0xff)) |> println
  \\ _ =
  \\  @string(j)
  \\  <> " something "
  \\  <> @string(0xff)
  \\  |> assert(
  \\    * == "(1, 2, 'a', [None, Just(5)], Just('oops')) something 255",
  \\    'should be same',)
  ;
  try doRuntimeTest(src);
}

test "Str concat .3" {
  const src =
  \\ (5 |> @string) <> ", yeah" 
  \\  |> * == "5, yeah" 
  \\  |> assert(*, 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <required methods>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\ assert(Foo().fmt() == "Foo()", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <default methods>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\  pub def to_string()
  \\    return "NotImplemented"
  \\  end
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\ assert(Foo().to_string() == "NotImplemented", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <multiple traits>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ class Foo: Display | Clone
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  \\ assert(f1.fmt() == "Foo()", 'should be same')
  \\ let f2 = f1.clone()
  \\ assert(f2 is Foo and f2 != f1, 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <covariance .1>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ let f: Display = Foo() # covariance
  \\ assert(f.fmt() == "Foo()", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <covariance .2>" {
  const src =
  \\ type Ordering = Lt | Gt | Eq
  \\ alias String = Str
  \\ 
  \\ trait Fmt
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Comparable
  \\  pub def cmp(other: Any): Ordering ;
  \\
  \\  pub def sum()
  \\    return 5
  \\  end
  \\ end
  \\
  \\ class Stuff: Comparable | Fmt
  \\  pub def cmp(x: Any): Ordering
  \\    return Ordering.Eq
  \\  end
  \\
  \\  pub def fmt()
  \\    return "Stuff" <> "(" <> ")"
  \\  end
  \\
  \\  pub def fox()
  \\  end
  \\ end
  \\
  \\ def compare(a: Stuff, b: Comparable)
  \\  a.fox()
  \\  return a.cmp(b)
  \\ end
  \\
  \\ let s: Comparable = Stuff()
  \\ assert(compare(Stuff(), s) == Ordering.Eq, 'should be same')
  \\ let j: List{Fmt} = [Stuff(), Stuff()] # covariance
  \\ j[-1].fmt() <> "oops" == "Stuff()oops" |> assert(*, 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <trait-extension .1>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait DisplayExt{T: Display}
  \\  pub def show(): String;
  \\ end
  \\
  \\ class Bar : Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\ end
  \\
  \\ class Foo: DisplayExt{Bar}
  \\  pub def show()
  \\    return "MyShow!"
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  \\ assert(f1.show() == "MyShow!", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <trait-extension .2>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait DisplayExt{T: Display}
  \\  pub def show(): String;
  \\ end
  \\
  \\ class Foo: Display | DisplayExt{Foo}
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def show()
  \\    return "MyShow!"
  \\  end
  \\ end
  \\
  \\ let f1 = Foo()
  \\ assert(f1.fmt() == "Foo()", 'should be same')
  \\ assert(f1.show() == "MyShow!", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <default-method-override>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\  pub def to_string()
  \\    return "NotImplemented"
  \\  end
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def to_string()
  \\    return "Foo_string"
  \\  end
  \\ end
  \\
  \\ assert(Foo().to_string() == "Foo_string", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <generic .1>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\
  \\ trait Fmt{T}
  \\  pub def fmt(x: T): String;
  \\ end
  \\
  \\ class Stuff: Fmt{Num} | Clone{Stuff}
  \\  pub def fmt(x: Num)
  \\    return "Stuff" <> "(" <> ")"
  \\  end
  \\
  \\  pub def clone()
  \\    return Stuff()
  \\  end
  \\ end
  \\
  \\ let s = Stuff()
  \\ let j = s.clone()
  \\ assert(s != j and j is Stuff, 'should be Stuff')
  \\ let k = j.clone()
  \\ assert(k != j and k is Stuff, 'should be Stuff')
  ;
  try doRuntimeTest(src);
}

test "traits <generic .2>" {
  const src =
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{Num}
  \\    x = 12
  \\  pub def shift(shr: Num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  \\ assert(s.shift(2) == 3, 'should be 3')
  ;
  try doRuntimeTest(src);
}

test "traits <generic .3>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\ 
  \\ trait Shifts{T}
  \\  pub def shift(x: T): T;
  \\ end
  \\
  \\ class Stuff: Shifts{Num} | Clone{Stuff}
  \\    x = 12
  \\  pub def clone()
  \\    return Stuff()
  \\  end
  \\  pub def shift(shr: Num)
  \\    return self.x >> shr
  \\  end
  \\ end
  \\
  \\
  \\ let s = Stuff()
  \\ let j = s.clone()
  \\ assert(j is Stuff and j != s, 'should be True')
  \\ assert(j.shift(2) == 3, 'should be 3')
  \\ assert(j.clone().shift(2) == 3, 'should be 3')
  ;
  try doRuntimeTest(src);
}

test "traits <function-bounds>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ def format(t: Display, v: Display)
  \\  return t.fmt() <> " $ " <> v.fmt()
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ class Bar: Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar())
  \\ assert(r == "Foo() $ Bar()", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <generic-function-bounds>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ def format{A: Display, B, C}(a: A, b: B, c: C): Str
  \\  where
  \\    B: Display + Clone,
  \\    C: Display + Clone,
  \\  return a.fmt() <> " $ " <> b.fmt() <> " $ " <> (c.clone() as C).fmt()
  \\ end
  \\
  \\ class Foo: Display | Clone
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ class Bar: Clone | Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Bar()
  \\  end
  \\ end
  \\
  \\ let r = format(Foo(), Bar(), Foo())
  \\ assert(r == "Foo() $ Bar() $ Foo()", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <self-referencing-generic-function-bounds>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone{T}
  \\  pub def clone(): T;
  \\ end
  \\
  \\ class Foo: Display | Clone{Foo}
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\
  \\  pub def clone()
  \\    return Foo()
  \\  end
  \\ end
  \\
  \\ def fun{P, U}(x: P)
  \\  where P: Display + Clone{P}
  \\  return x.clone().fmt()
  \\ end
  \\
  \\ assert(fun{Foo, Foo}(Foo()) == "Foo()", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <class-bounds>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo: Display
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\ end
  \\
  \\ class Bar
  \\  pub x: Display
  \\  pub def init(x: Display)
  \\    self.x = x
  \\  end
  \\ end
  \\
  \\ let r = Bar(Foo())
  \\ assert(r.x.fmt() == "Foo()", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <generic-class-bounds .1>" {
  const src =
  \\ alias String = Str
  \\
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ trait Clone
  \\  pub def clone(): Clone;
  \\ end
  \\
  \\ class Foo: Display | Clone
  \\  pub def fmt()
  \\    return "Foo()"
  \\  end
  \\  pub def clone()
  \\    return self
  \\  end
  \\ end
  \\
  \\ class Bar{T, V}
  \\  where
  \\    T: Display,
  \\    V: Clone,
  \\ end
  \\ assert(!!Bar{Foo, Foo}(), 'should not error')
  ;
  try doRuntimeTest(src);
}

test "traits <generic-class-bounds .2>" {
  const src =
  \\ alias String = Str
  \\ trait Display
  \\  pub def fmt(): String;
  \\ end
  \\
  \\ class Foo{X: Display}
  \\  x: X
  \\  def init(x: X)
  \\    self.x = x
  \\  end
  \\  pub def fmt()
  \\    return self.x.fmt()
  \\  end
  \\ end
  \\
  \\ class Bar : Display
  \\  pub def fmt()
  \\    return "Bar()"
  \\  end
  \\ end
  \\
  \\ let f1 = Foo(Bar())
  \\ assert(f1.fmt() == "Bar()", 'should not error')
  ;
  try doRuntimeTest(src);
}

test "traits <generic-trait-bounds .1>" {
  const src =
  \\ trait Speaks
  \\  pub def speak(): Str;
  \\ end
  \\
  \\ trait Barks{T}
  \\   where
  \\      T: Speaks
  \\ end
  \\
  \\ class Foo: Barks{Foo}
  \\  pub def speak()
  \\    return "Foo speaking here!"
  \\  end
  \\ end
  \\
  \\ let f = Foo()
  \\ assert(f.speak() == "Foo speaking here!", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <generic-trait-bounds .2>" {
  const src =
  \\ trait Speaks
  \\  pub def speak(): Str;
  \\ end
  \\
  \\ trait Barks{T}
  \\   where
  \\      T: Speaks
  \\  pub def bark(): Str;
  \\ end
  \\
  \\ class Foo: Barks{Foo}
  \\  pub def bark()
  \\    return "Foo barking here!"
  \\  end
  \\  pub def speak()
  \\    return "Foo speaking here!"
  \\  end
  \\ end
  \\
  \\ let f = Foo()
  \\ assert(f.bark() == "Foo barking here!", 'should be same')
  \\ assert(f.speak() == "Foo speaking here!", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <generics & resolution>" {
  const src =
  \\ trait Speaks
  \\  pub def speak(): Str;
  \\ end
  \\
  \\ trait Barks{T}
  \\   where
  \\      T: Speaks
  \\  pub def bark(): Str;
  \\ end
  \\
  \\ class Foo{T}: Barks{Foo{T}} | Speaks
  \\  pub def bark()
  \\    return "Foo barking here!"
  \\  end
  \\  pub def speak()
  \\    return "Foo speaking here!"
  \\  end
  \\ end
  \\
  \\ let f = Foo{Num}()
  \\ assert(f.bark() == "Foo barking here!", 'should be same')
  \\ assert(f.speak() == "Foo speaking here!", 'should be same')
  ;
  try doRuntimeTest(src);
}

test "traits <Iterator.next>" {
  const src =
  \\ let k = ['10', '11', '12', '13'].iter()
  \\ assert(k.next().?? == '10', 'should be 10')
  \\ assert(k.next().?? == '11', 'should be 11')
  \\ assert(k.next().?? == '12', 'should be 12')
  \\ assert(k.next().?? == '13', 'should be 13')
  \\ assert(k.next() == None, 'should be None')
  ;
  try doRuntimeTest(src);
}

test "traits <Iterator.count>" {
  const src =
  \\ let j = ['a', 'b', 'c', 'd', 'e']
  \\ let k = ['10', '11', '12', '13']
  \\ assert(j.iter().count() == 5, 'should be 5')
  \\ assert(k.iter().count() == 4, 'should be 4')
  ;
  try doRuntimeTest(src);
}

test "traits <Iterator.zip>" {
  const src =
  \\ let j = ['a', 'b', 'c', 'd', 'e']
  \\ let k = ['10', '11', '12', '13']
  \\ let p = j.iter().zip(k.iter())
  \\ assert(p.iter().count() == 4, 'should be 4')
  \\ assert(p.len() == 4, 'should be 4')
  \\ assert(p[0][0] == 'a', 'should be a')
  \\ assert(p[0][1] == '10', 'should be 10')
  \\ assert(p[-1][0] == 'd', 'should be d')
  \\ assert(p[-1][1] == '13', 'should be 13')
  ;
  try doRuntimeTest(src);
}

test "traits <Iterator.map>" {
  const src =
  \\ let j = [12, 13, 14]
  \\ let k = j.iter().map(def (n: Num) => @string(n))
  \\ assert(k.len() == 3, 'should be 2')
  \\ assert(k[0] == '12', 'should be 12')
  \\ assert(k[1] == '13', 'should be 13')
  \\ assert(k[2] == '14', 'should be 14')
  ;
  try doRuntimeTest(src);
}

test "traits <Iterator.filter>" {
  const src =
  \\ let j = [12, 13, 14]
  \\ let k = j.iter().filter(def (n: Num) => n % 2 == 0)
  \\ assert(k.len() == 2, 'should be 2')
  \\ assert(k[0] == 12, 'should be 12')
  \\ assert(k[1] == 14, 'should be 14')
  ;
  try doRuntimeTest(src);
}

test "traits <Iterator.reduce>" {
  const src =
  \\ let j = [12, 13, 14]
  \\ let r = j.iter().reduce(def (x: Num, y: Num) => x + y, None)
  \\ assert(r == 39, 'should be 39')
  \\
  \\ let j = ['f', 'i', 'n', 'd', 'e', 'r']
  \\ let conc = j.iter().reduce(def (x: Str, y: Str) => x <> y, Just(''))
  \\ conc == 'finder' |> assert(*, 'should be finder')
  ;
  try doRuntimeTest(src);
}

test "traits <std.iter.range>" {
  const src =
  \\ import std.iter
  \\ iter.range(1, Just(12), Just(2)).iter().count()
  \\ |> assert(* == 6, 'should be 6')
  \\
  \\ iter.range(1, Just(12), None).iter().count()
  \\ |> assert(* == 11, 'should be 6')
  \\
  \\ let r = iter.range(1, Just(12), None)
  \\ assert(r.next().?? == 1, 'should be 1')
  \\ assert(r.next().?? == 2, 'should be 2')
  \\
  \\ let r = iter.range(1, Just(12), None).iter()
  \\ assert(r.next().?? == 1, 'should be 1')
  \\ assert(r.next().?? == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "traits <std.iter.zip>" {
  const src =
  \\ import std
  \\ std.iter.zip([1, 2, 3, 4].iter(), ['a', 'b', 'c'].iter())
  \\  .iter().count()
  \\ |> assert(* == 3, 'should be 3')
  \\
  \\ let r = std.iter.zip([1, 2, 3].iter(), ['a', 'b', 'c', 'd'].iter())
  \\ assert(r[0][0] == 1, 'should be 1')
  \\ assert(r[0][1] == 'a', 'should be a')
  \\ assert(r[-1][0] == 3, 'should be 1')
  \\ assert(r[-1][1] == 'c', 'should be a')
  ;
  try doRuntimeTest(src);
}

test "traits <std.iter.range & Iterator.zip>" {
  const src =
  \\ import std.iter as c
  \\ let k = [10, 11, 12, 13]
  \\ let r = k.iter().zip(c.range(1, Just(12), Just(2)).iter())
  \\ assert(r.len() == 4, 'should be 4')
  \\ assert(r[0][0] == 10, 'should be 10')
  \\ assert(r[0][1] == 1, 'should be 1')
  \\ assert(r[-1][0] == 13, 'should be 13')
  \\ assert(r[-1][1] == 7, 'should be 7')
  \\
  \\ let j = ['a', 'b', 'c', 'd']
  \\ let r = k.iter().zip(j.iter())
  \\ assert(r.len() == 4, 'should be 4')
  \\ assert(r[0][0] == 10, 'should be 10')
  \\ assert(r[0][1] == 'a', 'should be a')
  \\ assert(r[-1][0] == 13, 'should be 13')
  \\ assert(r[-1][1] == 'd', 'should be d')
  ;
  try doRuntimeTest(src);
}

test "parameter resolution" {
  const src =
  \\ def iter{U}(itr: Iterator{U})
  \\ end
  \\ class Foo: Iterator{Num} | Iter{Num}
  \\  state = 0
  \\  data: List{Num}
  \\
  \\  def init(d: List{Num})
  \\    self.data = d
  \\  end
  \\
  \\  pub def fun(x: Iterator{Num})
  \\    return x
  \\  end
  \\
  \\  pub def next()
  \\    if self.state >= self.data.len()
  \\      return None
  \\    end
  \\    self.state += 1
  \\    return Just(self.data[self.state - 1])
  \\  end
  \\
  \\  pub def iter()
  \\    return Foo(self.data)
  \\  end
  \\ end
  \\
  \\ let f = Foo([1, 2, 3, 4])
  \\ iter(f)
  \\ assert(f.fun(f) == f, 'should be same')
  \\ assert(f.fun(f) == (f as Iterator{Num}), 'should be same')
  ;
  try doRuntimeTest(src);
}

test "for loop" {
  const src =
  \\ import std.iter
  \\ const range = iter.range
  \\ let i = 'a'
  \\ let j = 'b'
  \\ let l = [] as List{fn(): Num}
  \\ for j in range(1, Just(12), None) do
  \\  _ = l.append(def () => j)
  \\ end
  \\ assert(j == 'b', 'should be same')
  \\
  \\ let expected = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  \\ for i, fun in l
  \\  _ = assert(fun() == expected[i], 'should be same')
  \\ end
  \\ assert(i == 'a' and j == 'b', 'should be same')
  ;
  try doRuntimeTest(src);
}

test "classes <[experimental] generic methods .1>" {
  const src =
  \\ class Fin{T}
  \\  pub x: T
  \\  def init(x: T)
  \\     self.x = x
  \\  end
  \\
  \\  pub def generate{U}(y: U)
  \\    return (self.x, y)
  \\  end
  \\ end
  \\ let j = Fin(12)
  \\ j.generate('xyz')[1] == 'xyz' |> assert(*, 'should be same')
  \\ j.generate(False)[1] == False |> assert(*, 'should be same')
  \\ j.generate([1, 2, 3])[1][-1] == 3 |> assert(*, 'should be same')
  \\ j.generate((2, 4, 6))[1][-1] == 6 |> assert(*, 'should be same')
  ;
  try doRuntimeTest(src);
}

test "classes <[experimental] generic methods .2>" {
  const src =
  \\ trait Debugs{K, T}
  \\  pub def show(x: K, y: T)
  \\    return (x, y)
  \\  end
  \\ end
  \\
  \\ class Oops{P, Q}: Debugs{P, Q}
  \\ end
  \\
  \\ class Fin{X, T, V}
  \\  where X: Debugs{T, V}
  \\  pub x: X
  \\  def init(x: X)
  \\     self.x = x
  \\  end
  \\
  \\  pub def generate{U}(y: U, a: T, b: V)
  \\    where U: Debugs{V, T}
  \\    return (self.x.show(a, b), y.show(b, a))
  \\  end
  \\ end
  \\
  \\ let op = Oops{Num, Str}()
  \\ let res = Fin{Oops{Num, Str}, Num, Str}(op)
  \\  .generate(Oops{Str, Num}(), 2, 'one')
  \\
  \\ assert(res.len() == 2, 'should be 2')
  \\ assert(res[0][0] == 2, 'should be 2')
  \\ assert(res[0][1] == 'one', 'should be one')
  \\ assert(res[1][0] == 'one', 'should be one')
  \\ assert(res[1][1] == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "builtins <Str.methods>" {
  const src =
  \\ const j = "this is kinda"
  \\ # slice
  \\ assert(j.slice(-5, -1) == 'kind', 'should be same')
  \\ assert(j.slice(-8, -1) == 'is kind', 'should be same')
  \\ assert(j.slice(2, 7) == 'is is', 'should be same')
  \\ assert(j.slice(-30, -7) == '', 'should be same')
  \\ assert(j.slice(30, 7) == '', 'should be same')
  \\ assert(j.slice(30, 70) == '', 'should be same')
  \\ assert(j.slice(-30, -70) == '', 'should be same')
  \\
  \\ # at
  \\ assert(j.at(-3) == 'n', 'should be same')
  \\ assert(j.at(0) == 't', 'should be same')
  \\ assert(j.at(-1) == 'a', 'should be same')
  \\ assert(j.at(-1) == j.at(j.len()-1), 'should be same')
  \\ assert(j.at(127) == '', 'should be same')
  \\ assert(j.at(-150) == '', 'should be same')
  \\
  \\ let x = "  this is the end of the world   "
  \\ # strip
  \\ assert(x.strip(" ") == 'this is the end of the world', 'should be same')
  \\ assert('boy'.strip('boymeyer') == 'boy', 'should be same')
  \\
  \\ # lstrip
  \\ assert(x.lstrip(" ") == 'this is the end of the world   ', 'should be same')
  \\
  \\ # rstrip
  \\ assert(x.rstrip(" ") == '  this is the end of the world', 'should be same2')
  \\
  \\ let x = "this is the end of the world"
  \\ # split
  \\ const p = x.split(' ')
  \\ assert(p.len() == 7, 'should be same')
  \\ assert(p[0] == 'this', 'should be same')
  \\ assert(p[-1] == 'world', 'should be same')
  \\ const q = '1234'
  \\ const p = q.split('')
  \\ assert(p.len() == 4, 'should be 4')
  \\ for i, e in p
  \\  assert(e == q.at(i), 'should be same')
  \\ end
  \\
  \\ # find
  \\ assert(x.find("end").? == 12, 'should be 12')
  \\ 
  \\ # starts_with
  \\ assert(x.starts_with("this"), 'should be True')
  \\
  \\ # ends_with
  \\ assert(x.ends_with("world"), 'should be True')
  \\
  \\ # count
  \\ assert(x.count("e") == 3, 'should be 3')
  \\ assert(x.count("o") == 2, 'should be 2')
  \\ assert(x.count("x") == 0, 'should be 0')
  \\
  \\ # upper
  \\ assert(x.upper() == 'THIS IS THE END OF THE WORLD', 'should be same')
  \\
  \\ # lower
  \\ assert(x.lower() == 'this is the end of the world', 'should be same')
  \\
  \\ let x = "thisistheend"
  \\ # is_alpha
  \\ assert(x.is_alpha(), 'should be True')
  \\
  \\ # is_digit
  \\ assert(!x.is_digit(), 'is not digit')
  \\
  \\ # is_alnum
  \\ assert(x.is_alnum(), 'should be True')
  \\
  \\ # is_ascii
  \\ assert(x.is_ascii(), 'should be True')
  \\
  \\ # is_lower
  \\ assert(x.is_lower(), 'should be True')
  \\
  \\ # is_upper
  \\ assert(!x.is_upper(), 'is not lower')
  \\
  \\ # count
  \\ assert(x.count("the") == 1, 'should be 1')
  \\
  \\ const x = "12345"
  \\
  \\ # is_digit
  \\ assert(x.is_digit(), 'is digit')
  \\
  \\ # contains
  \\ assert(x.contains('34'), 'is True')
  ;
  try doRuntimeTest(src);
}

test "builtins <map.methods>" {
  const src =
  \\ const x = {'a': 1, 'b': 2, 'c': 3}
  \\ # copy
  \\ const y = x.copy()
  \\ assert(x.len() == y.len(), 'should be same')
  \\ for kv in x.items()
  \\  assert(y[kv[0]] == kv[1], 'should be same')
  \\ end
  \\ 
  \\ # clear
  \\ x.clear()
  \\ assert(x.len() == 0 and y.len() == 3, 'should be True')
  \\
  \\ # pop
  \\ const v = y.pop('b').??
  \\ assert(v == 2, 'should be same')
  \\ assert(y.len() == 2, 'should be 2')
  \\
  \\ # set
  \\ y.set('b', 4)
  \\ assert(y['b'] == 4, 'should be 4')
  ;
  try doRuntimeTest(src);
}

test "builtins <list.methods>" {
  const src =
  \\ import std.list
  \\ const x = ['a', 'b', 'c', 'd']
  \\ # copy
  \\ const y = x.copy()
  \\ assert(x.len() == y.len(), 'should be same')
  \\ for i, v in x
  \\  assert(y[i] == v, 'should be same')
  \\ end
  \\
  \\ # pop
  \\ const v = x.pop().??
  \\ assert(v == 'd', 'should be d')
  \\
  \\ # extend
  \\ y.extend(x)
  \\ assert(y.len() == 7, 'should be 7')
  \\ assert(y[-1] == 'c', 'should be c')
  \\ assert(y[-2] == 'b', 'should be b')
  \\ assert(y[-3] == 'a', 'should be a')
  \\
  \\ # clear
  \\ x.clear()
  \\ assert(x.len() == 0 and y.len() == 7, 'should be True')
  \\
  \\ # append
  \\ y.append('e')
  \\ assert(y.len() == 8, 'should be 8')
  \\ assert(y[-1] == 'e', 'should be e')
  \\
  \\ const j = '1234'.split('')
  \\ # list.remove
  \\ assert(j.len() == 4, 'should be 4')
  \\ list.remove(j, '2')
  \\ assert(j.len() == 3, 'should be 3')
  \\
  \\ # reverse & list.join
  \\ j.reverse()
  \\ assert(list.join(j, '') == '431', 'should be same')
  \\
  \\ # list.find
  \\ assert(list.find(j, '1').? == 2, 'should be 2')
  \\ 
  \\ let x = ['a', 'b', 'c', 'd', 'c', 'b', 'd', 'c']
  \\ # list.join
  \\ const p = list.join(x, ' <|> ')
  \\ assert(p == 'a <|> b <|> c <|> d <|> c <|> b <|> d <|> c', 'should be same')
  \\
  \\ # list.count_with
  \\ assert(list.count_with(x, def (a: Str) => a == 'c') == 3, 'should be 3')
  \\ let x = [1, 2, 3, 4]
  \\ assert(list.count_with(x, def (a: Num) => a == 2) == 1, 'should be 1')
  \\
  \\ const j = '1234'.split('')
  \\ # list.remove_with
  \\ list.remove_with(j, def (x: Str) => x == '2')
  \\ assert(j.len() == 3, 'should be 3')
  \\
  \\ # list.find_with
  \\ assert(list.find_with(j, def (x: Str) => x == '1').? == 0, 'should be 0')
  ;
  try doRuntimeTest(src);
}

test "trait operators <equality>" {
  const src =
  \\ class Weight: Eq{Weight}
  \\   val: Num = 0
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\   
  \\   pub def eq(other: Weight)
  \\     return self.val == other.val
  \\   end
  \\ end
  \\
  \\ assert(Weight(12) == Weight(12), 'should be same')
  \\ assert(Weight(12.5) != Weight(12), 'should not be same')
  ;
  try doRuntimeTest(src);
}

test "trait operators <ordering .1>" {
  const src =
  \\ class Weight: Ord{Weight}
  \\   val: Num = 0
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\   
  \\   pub def eq(other: Weight)
  \\     return self.val == other.val
  \\   end
  \\
  \\   pub def cmp(other: Weight)
  \\     if self.val > other.val
  \\       return Ordering.Greater 
  \\     elif self.val < other.val
  \\       return Ordering.Less
  \\     else
  \\       return Ordering.Equal
  \\     end
  \\   end
  \\ end
  \\ 
  \\ #assert(Weight(12) > Weight(10), 'should be greater')
  \\ assert(Weight(12) >= Weight(10), 'should be greater or equal')
  \\ assert(Weight(12) < Weight(10) == False, 'should not be less')
  \\ assert(Weight(12) <= Weight(10) == False, 'should not be less or equal')
  \\ assert(Weight(12) == Weight(12), 'should be equal')
  \\ assert(Weight(12.5) != Weight(12), 'should not be equal')
  \\
  \\ let w1 = Weight(13.49)
  \\ let w2 = Weight(13.49)
  \\ assert(w1 == w2, 'w1 == w2')
  ;
  try doRuntimeTest(src);
}

test "trait operators <ordering .2>" {
  const src =
  \\ class Foo: Ord{Foo}
  \\   x: Num
  \\ 
  \\   def init(x: Num)
  \\     self.x = x 
  \\   end
  \\ 
  \\   pub def eq(other: Foo)
  \\     return self.x == other.x
  \\   end
  \\   
  \\   pub def cmp(other: Foo)
  \\     if self.x > other.x 
  \\       return Ordering.Greater
  \\     elif self.x < other.x
  \\       return Ordering.Less
  \\     else
  \\       return Ordering.Equal
  \\     end
  \\   end
  \\ end
  \\ 
  \\ let f1 = Foo(12)
  \\ let f2 = Foo(28)
  \\ 
  \\ assert(!(f1 == f2), 'f1 == f2')
  \\ assert(f1 != f2, 'f1 != f2')
  \\ assert(!(f1 > f2), 'f1 > f2')
  \\ assert(f1 < f2, 'f1 < f2')
  \\ assert(!(f1 >= f2), 'f1 >= f2')
  \\ assert(f1 <= f2, 'f1 <= f2')
  \\ assert(f2 > f1, 'f2 > f1')
  \\ assert(!(f2 < f1), 'f2 < f1')
  \\ assert(f2 >= f1, 'f2 >= f1')
  \\ assert(!(f2 <= f1), 'f2 <= f1')
  ;
  try doRuntimeTest(src);
}

test "trait operators <add & concat>" {
  const src =
  \\ class Weight:
  \\   | Add{Weight, Weight}
  \\   | Eq{Weight}
  \\   val: Num = 0
  \\
  \\   def init(val: Num)
  \\     self.val = val
  \\   end
  \\ 
  \\   pub def add(other: Weight)
  \\     return Weight(self.val + other.val)
  \\   end
  \\
  \\   pub def eq(other: Weight)
  \\     return self.val == other.val
  \\   end
  \\ end
  \\
  \\ let w1 = Weight(12)
  \\ let w2 = Weight(12)
  \\ assert(w1 + w2 == Weight(24), 'sum should be equal')
  \\ assert(w1 <> w2 == Weight(24), 'sum should be equal')
  ;
  try doRuntimeTest(src);
}

test "trait type parameters" {
  const src =
  \\ trait Sings 
  \\   def sing ;
  \\ end
  \\ 
  \\ class Bank: Sings 
  \\   def sing end
  \\ end
  \\
  \\ def foo{T: Sings}(x: T)
  \\   return 5
  \\ end
  \\
  \\ foo(Bank()) |> assert(* == 5, 'should be 5')
  \\ foo{Sings}(Bank()) |> assert(* == 5, 'should be 5')
  \\ foo{Sings}(Bank() as Sings) |> assert(* == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}
