const std = @import("std");
const lib = @import("lib.zig");

const value = lib.value;
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
    \\ assert((0x123 < 4) == false, '0x123 < 4')
    \\ assert((123.45 > 12_40) == false, '123.45 > 12_40')
    \\ assert(0b111_000 <= 0o12_12, 'should be lte')
    \\ assert((123.e-2 >= 0x12_34_5) == false, 'should be gte')
    \\ assert(123.e-2 != 0x12_34_5, 'should be unequal')
    \\ assert(0xdeadbeef == 0o33653337357, 'should be equal')
  ;
  try doRuntimeTest(srcs);
}

test "booleans" {
  const srcs = 
  \\ assert((0x123 < 4 and 1 < 5) == false, '0x123 < 4 and 1 < 5')
  \\ assert((123.45 > 12_40 or 2 > 1), '123.45 > 12_40 or 2 > 1')
  \\ assert((0b111_000 <= 0o12_12 or 1 > 0.5), '0b111_000 <= 0o12_12 or 1 > 0.5')
  \\ assert(!(123.e-2 >= 0x12_34_5 and 7 > 2), '!(123.e-2 >= 0x12_34_5 and 7 > 2)')
  \\ assert(!!(123.e-2 != 0x12_34_5 or 6 > 2), '!!(123.e-2 != 0x12_34_5 or 6 > 2)')
  \\ assert((1 or 2) == 1, '(1 or 2) == 1')
  \\ assert((1 and 2) == 2, '(1 and 2) == 2')
  \\ assert((0b00 and 2) == 0o0, '(0b00 and 2) == 0o0')
  \\ assert((0x0 or 2) == 2, '(0x0 or 2) == 2')
  \\ assert(true or false, 'true or false')
  \\ assert(false or true, 'false or true')
  \\ assert((false or false) == false, 'false or false')
  \\ assert(true or true, 'true or true')
  \\ assert((true and false) == false, 'true and false')
  \\ assert((false and true) == false, 'false and true')
  \\ assert((false and false) == false, 'false and false')
  \\ assert(!false, '!false')
  \\ assert(!true == false, '!true')
  \\ assert(!(0x0_0), '!(0x0_0)')
  \\ assert(!!(1), '!!(1)')
  \\ assert(!(1) == false, '!(1)')
  \\ assert('foxes and pirates' == 'foxes and pirates', 'foxes & pirates eql')
  \\ assert('foxes and pirates' != 'fishes and pirates', 'foxes & pirates & fishes neql')
  \\ assert(('fox' or '') == 'fox', 'should be fox')
  \\ assert(('fox' and '') == '', 'should be empty str')
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
    \\ [1, 2, 3, 4]
    \\ [Ok(1) as Result{num, num}, Error(2), Ok(3), Error(4)]
    \\ (1, 'fox', 3, 'cat')
    \\ [1]
    \\ []
    \\ (1, 'fox', 3, 'cat', (1, 'fox', 3, 'cat'))
    \\ (1, 2, {'a': 'set'})
    \\ (1, 2, {'a': 'set'},)
    \\ (1,)
    \\ ({'abc': 123}, {true: 0xff}, {'obs': 0b101})
    \\ ()
    \\ ([1, 2, 3], [5, 5, 5])
    \\ ('abc',) as Tuple{'abc'}
    \\ ('x', 'y',)
    \\ let x: List{Result{num, num}} = [Ok(1) as Result{num, num}, Error(2), Ok(3), Error(4)]
    \\ println(x, x.len())
  ;
  try doRuntimeTest(src);
}

test "maps" {
  const srcs =
    \\ []
    \\ {'abc': 123}
    \\ type Key = Str_(str) | Bool_(bool)
    \\ {Str_('abc') as Key: 123, Bool_(true): 0xff, Str_('obs'): 0b101}
    \\ {}
    \\ {24: [1, 2, 3]}
    \\ {24: [1, 2, 3],}
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
  \\ [p, k]
  \\ let y = (
  \\     x - 5
  \\ )
  \\ let z = [
  \\     {x: y},
  \\ {
  \\     x: y
  \\ }
  \\ ]
  \\ {123: "foxlike"}
  \\ #let y = 10
  \\ #[x, y]
  \\ z
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
  \\ let x: A{str, num} = (
  \\  {
  \\    'a': (
  \\        {
  \\          'x': Vee(
  \\                  (Cee(5) as VOrC{str, num})
  \\                ) as P{str, num}
  \\        }
  \\      ) as Map{str, P{str, num}}
  \\  }
  \\ )
  \\ println(x)
  \\ let x = {Just(false) as bool?: {true: Just('ok') as str?} as Map{bool, str?}}
  \\ let y = 15 as num
  \\ let j = (15 as num) as num
  \\ let z = {15: ['foxy']} as Map{num, List{str}}
  \\ let z1 = {15: ['foxy']} as (Map{num, List{str}})
  \\ let z2 = Just({12: ['foxy']}) as (Map{num, List{str}})?
  \\ type T{K, V} = K | V
  \\ let p: T{num, str} = K
  \\ p = V
  \\ let x = None
  \\ let y: str? = x
  \\ let z: (num)? = Just(5)
  \\ let z2: num = z.? + 5
  \\ z2 += z2
  \\ let j: Tuple{true, false} = (true, false)
  \\ let q = (1, 2, 'abc', 0xff, 1, 'foo', 'bar')
  \\ let s = [1, 2, 3]
  \\ s[0] = q[0]
  \\ s[1] = q[1]
  \\ s[2] = q[4]
  \\ (s, q)
  ;
  try doRuntimeTest(src);
}

test "empty generic types" {
  const src = 
  \\ let p = ()
  \\ let j: List{any} = []
  \\ let q: Map{any, any} = {}
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
  \\ alias StringHashMap{V} = HashMap{str, V}
  \\ alias NumList = List{num}
  \\ let a: NumList = [1, 2]
  \\ let b: HashMap{num, bool} = {0: false}
  \\ let c: StringHashMap{bool} = {'foo': false}
  \\ let x: str = 'over the garden wall!'
  \\ let y = 'oops'
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ alias A = str
  \\ alias B = num
  \\ alias C{A, B} = Map{A, B}
  \\ alias D{K} = C{K, B}
  \\ alias HashMap{K, V} = C{K, V}
  \\ alias StringHashMap{V} = HashMap{str, V}
  \\ alias BadList = List{StringHashMap{HashMap{A, D{B}}}}
  \\ alias X = BadList #(D{BadList}? | D{B}? | BadList?)
  \\ let x: X = ([{'fox': {'fin': ({0x12: 0xbee}) as Map{num, num}}}])
  \\ x
  \\ let y: D{B}? = Just({10: 5}) as Map{num, num}?
  \\ y
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ alias HashMap{K, V} = Map{K, V}
  \\ let x: HashMap{num, str} = {5: 'okay'}
  \\ Just(x) as Map{num, str}?
  \\ alias HashMap{K, V} = Map{K, V}
  \\ let x: HashMap{num, str} = {5: 'okay'}
  \\ Just(x as Map{num, str}) as Map{num, str}?
  ;
  try doRuntimeTest(src3);
}

test "recursive types" {
  const src =
  \\ # recursive generics
  \\ type R = Str(str) | Num(num) | Col(List{R})
  \\ let x: R = Str('fox')
  \\ x = Col([
  \\    Col([
  \\      Col([Col([Str('foo') as R] as List{R}) as R]) as R,
  \\      Num(3),
  \\      Col([
  \\        Col([
  \\          Col([Num(4) as R, Num(5)]) as R,
  \\          Str('bar') as R
  \\        ]) as R
  \\      ]) as R, 
  \\      Str('ok')
  \\    ]) as R,
  \\    Col([ Col([ Col([Str('duh') as R]) as R ]) as R ]),
  \\    Col([R.Num(4)])
  \\ ])
  \\ x = Str('ok')
  \\ x
  \\ # no conflicts
  \\ type A{T} = T
  \\ let x: A{A{num}} = T
  ;
  try doRuntimeTest(src);
}

test "circularity" {
  const src =
  \\ type T{K} = A1(str) | B1(S{K})
  \\ type S{K} = A2(num) | B2(T{num})
  \\ let p: T{num} = A1('fox')
  \\ println(p)
  \\ p = B1(A2(5) as S{num})
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
  \\ type T{P} = A1(str) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{num})
  \\ let p: T{str} = A1('fox')
  \\ println(p)
  \\ p = B1(A2('5') as S{str})
  \\ println(p)
  ;
  try doRuntimeTest(src);
}

test "circularity-3" {
  const src =
  \\ type T{P} = A1(P) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{K})
  \\ let p: T{str} = A1('fox')
  \\ p = B1(A2('5') as S{str})
  \\
  \\ type T{P} = A1(P) | B1(S{P})
  \\ type S{K} = A2(K) | B2(T{K})
  \\ let p: T{num} = A1('fox'.len())
  \\ p = B1(A2(5) as S{num})
  ;
  try doRuntimeTest(src);
}

test "cast-typecheck" {
  const src =
  \\ let x = 5 as bool
  \\ assert(x, 'should be true')
  \\ !!x and !!'fox'
  \\ type NumStr = Num(num) | Str(str)
  \\ type NumStrBool = Num(num) | Str(str) | Bool(bool)
  \\ let p = Just(Num(5) as NumStr) as NumStr?
  \\ let q: NumStrBool = Num(5)
  \\ match q
  \\  case Num(t) => q = Num(t + 5)
  \\  case _ => assert(false, '')
  \\ end
  \\ alias X = NumStr
  \\ let y: X = (Str('food') as NumStr)
  \\ y
  \\
  \\ type Cat = A(str) | B(str)
  \\ type Dog = A(num) | B(str)
  \\ let x: Cat = Cat.A('fox')
  \\ let y: Dog = Dog.A(12)
  \\ println(x, y)
  \\ x = Cat.B('foo')
  \\ y = Dog.A(56)
  \\ type Cat = A(str)
  \\ type Dog = A(num)
  \\ let x: Cat = Cat.A('fox')
  \\ let y: Dog = Dog.A(12)
  \\
  \\ type NumStr = A(num) | B(str)
  \\ type StrNum = B(str) | A(num)
  \\ let a: NumStr = NumStr.A(10)
  \\ let b: StrNum = StrNum.B('foo')
  \\ b = a
  \\
  \\ let x = [] as List{num}
  \\ x = [1, 2, 3]
  \\ type T = S(str) | N(num) | L(List{T})
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
  \\ type NumBool = Num(num) | Bool(bool)
  \\ let x: List{NumBool} = [Num(1) as NumBool, Num(2)]
  \\ x[1] = Num(d)
  \\ x[
  \\  -1 + 1
  \\ ] = Bool(!d)
  \\ let x = [1, 2, 3, 4, 5]
  \\ let y = x[t[3]]
  \\ let r = !!x[2]
  \\ x[-1] <<= 3
  \\ let w = x[-2]
  \\ (t, r, q, p, x, y, d, w)
  \\ # multi type index
  \\ type NumStr = Num(num) | Str(str)
  \\ let _a = Str('fox')
  \\ let _b = Str('fun')
  \\ let _c = Num(5)
  \\ let _d = Str('fox')
  \\ let y = {_a as NumStr: Str('fan'), _b: Str('fox'), _c: _d}
  \\ let p = y[_a] and y[_b] and y[_c as NumStr]
  \\ assert(p == (_d as NumStr), 'should be fox')
  ;
  try doRuntimeTest(src);
}

test "indexing .2" {
  const src =
  \\ let x: Tuple{List{Tuple{num, List{num}}}, num} = ([(9, [] as List{num})], 5)
  \\ let p = 0
  \\ if x[0] is List{Tuple{num, List{num}}}
  \\    if x[0][0] is Tuple{num, List{num}}
  \\      if x[0][0][1] is List{num}
  \\        p /= 5
  \\      end
  \\    end
  \\ elif x[1] is num
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
  \\ let p: List{num?} = []
  \\ let q: List{num?} = [None as num?]
  \\ let r: List{num?} = [None as num?, Just(5), None]
  \\ type NumStr = Num(num) | Str(str)
  \\ let r: List{NumStr?} = [None as NumStr?, Just(Num(5) as NumStr), None, Just(Str('foo') as NumStr)]
  \\ let t: List{(NumStr)?} = [None as NumStr?, Just(Num(5) as NumStr), None]
  ;
  try doRuntimeTest(src);
}

test "nil access" {
  const src =
  \\ let x: num? = Just(5)
  \\ let p = x.? + 10
  \\ let f = {'foo': Just(5) as num?}
  \\ let j = f['foo'].? + 5
  \\ assert(j == 10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "if statement" {
  // if-elif-else
  const src =
  \\ let x: num = 5
  \\ let t: num? = Just(15)
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
  \\ let x: num = 5
  \\ let t: num? = Just(15)
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
  \\    let x: num = 5
  \\    let t: num? = Just(15)
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
  \\ let x: num = 5
  \\ let t: num? = Just(15)
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
  \\ let x: num = 5
  \\ let t: num? = Just(15)
  \\ let p = 0
  \\ if 0xf == x - t.?
  \\   p = x + 10
  \\ else
  \\   p = x - 10
  \\   p -= -1111
  \\ end
  \\ assert(p == 1106, 'should be 1106')
  \\ p == 0o2122 and p == 0x452
  \\ type NumStr = Num(num) | Str(str)
  \\ let p: NumStr = Num(5)
  \\ let q = NumStr.Str("fox")
  \\ let w = 0
  \\ if p == q
  \\  match p
  \\    case Num(t) => w = t
  \\    case _ => assert(false, '-')
  \\  end
  \\ else
  \\   w = 123
  \\ end
  \\ assert(w == 123, 'should be 123')
  \\ match q
  \\  case Str(t) => if t == 'fox'
  \\    w /= 2
  \\  end
  \\  case _ => panic('bad')
  \\ end
  \\ assert(w == 61.5, 'should be 61.5')
  ;
  try doRuntimeTest(src5);
}

test "type expressions" {
  const src =
  \\ let p = num
  \\ p = str
  \\ let q = List{num}
  \\ q = p
  \\ let x = Map{str, num}
  \\ x = p
  \\ let w = [num, str, bool, List{str}]
  \\ w = [num, bool]
  \\ let q = {'bar': List{Map{str, num}}}
  \\ q = {'fox': num, 'foo': bool, 'bar': List{Map{str, num}}}
  \\ let t: num = 5
  \\ if num == num
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
  \\ assert('fox' is str, 'same-1')
  \\ assert(5 is num, 'same-2')
  \\ assert([] is List{any}, 'same-3')
  \\ assert({} is Map{any, any}, 'same-4')
  \\ assert(true is bool, 'same-5')
  \\ assert(None as List{num}? == None, 'same-6')
  \\ let p: str? = None
  \\ assert(p == None, 'same-7')
  \\ assert(!!(Just([5]) as List{num}? != None), 'same-8')
  \\ assert(None == None, 'same-9')
  \\
  \\ # indirect checks
  \\ type Typ = N(num) | S(str) | L(List{num}) | M(Map{str, num})
  \\ let n: Typ = M({} as Map{str, num})
  \\ match n
  \\  case L([..]) => assert(false, '.1')
  \\  case M({..} as t) => assert(t is Map{str, num} == true, 't should be map')
  \\  case _ => assert(false, '.2')
  \\ end
  \\ n = S('foo')
  \\ match n
  \\  case N(t) => assert(false, 't should not be num')
  \\  case S(t) => assert(t is str == true, 't should be str')
  \\  case _ => assert(false, '.3')
  \\ end
  \\ n = N(5)
  \\ match n
  \\  case N(t) => assert(t is num, 't should be num')
  \\  case _ => assert(false, '.4')
  \\ end
  \\ assert(!!n is bool == true, 'should be boolean')
  \\ assert(bool == bool is bool == true, 'true')
  \\ assert(((bool == bool) is bool) == true, 'true')  # same as above
  \\
  \\ # is not
  \\ # direct checks
  \\ assert(!('fox' is not str), 'same-1')
  \\ assert(!(5 is not num), 'same-2')
  \\ assert(!([] is not List{any}), 'same-3')
  \\ assert(!({} is not Map{any, any}), 'same-4')
  \\ assert(!(true is not bool), 'same-5')
  \\ assert(!(None as List{num}? != None), 'same-6')
  \\ let p: str? = None
  \\ assert(!(p != None), 'same-7')
  \\ assert(!(Just([5]) as List{num}? == None), 'same-8')
  \\ assert(!(None != None), 'same-9')
  \\
  \\ # indirect checks
  \\ type Typ = N(num) | S(str) | L(List{num}) | M(Map{str, num})
  \\ let n: Typ = M({} as Map{str, num})
  \\ match n
  \\  case L([..]) => assert(false, '.1')
  \\  case M({..} as t) => assert(!(t is not Map{str, num} == true), 't should be map')
  \\  case _ => assert(false, '.2')
  \\ end
  \\ n = S('foo')
  \\ match n
  \\  case N(t) => assert(false, 't should not be num')
  \\  case S(t) => assert(!(t is not str == true), 't should be str')
  \\  case _ => assert(false, '.3')
  \\ end
  \\ n = N(5)
  \\ match n
  \\  case N(t) => assert(!(t is not num), 't should be num')
  \\  case _ => assert(false, '.4')
  \\ end
  \\ assert(!(!!n is not bool == true), 'oops')
  \\ assert(bool == bool is not bool == true == false, 'ooops')
  \\ assert(!({} is not Map{str, num}), 'a map')
  \\ assert(((bool == bool) is not bool) != true, 'not true')  # same as above
  \\ assert(num != str, 'num is not str')
  ;
  try doRuntimeTest(src);
}

test "narrowing-1" {
  const src =
  \\ type StrNum = Str(str) | Num(num)
  \\ type Star = Str(str)
  \\ let x: StrNum? = Just(Str('foobar') as StrNum)
  \\ let p = 10
  \\ match x
  \\  case Just(Str(t)) => assert(true, 'ok')
  \\  case Just(Num(t)) => assert(false and ((t + p) > 5), 'num')
  \\  case None => assert(false, 'none')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-2" {
  const src =
  \\ type Col = L(List{num}) | M(Map{str, num})
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
  \\ type Col = L(List{num}) | M(Map{str, num})
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
  \\ type StrNum = Str(str) | Num(num)
  \\ type Col = L(List{List{StrNum}}) | M(Map{str, num})
  \\ let x: Col = L([[Num(5) as StrNum]])
  \\ let p = 10
  \\ match x
  \\  case L([[Num(a)]]) => if a + 2 > 0
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
  \\ type NumStr = Num(num) | Str(str)
  \\ type T = L(List{List{NumStr}}) | M(Map{str, List{NumStr}})
  \\ let x: T = L([[Num(5) as NumStr]])
  \\ let p = 0
  \\ match x
  \\  case M({'a': [Num(g), ..]} as m) => if g + 2 > 0
  \\    m['foobar'] = [Num(1) as NumStr, Num(2)]
  \\  end
  \\  case M(_) => assert(false, '')
  \\  case L([[Num(t)]]) => p = t
  \\  case L(_) => assert(false, '')
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-6" {
  const src =
  \\ type NumStr = Num(num) | Str(str)
  \\ type T = L(List{List{NumStr}}) | M(Map{str, List{NumStr}})
  \\ let x: T = M({'a': [Num(5) as NumStr], 'b': [] as List{NumStr}})
  \\ let p = 0
  \\ match x
  \\  case M({'a': [Num(g), ..], ..} as m) => if g + 2 > 0
  \\    m['foobar'] = [Num(1) as NumStr, Num(2)]
  \\    match m['foobar']
  \\      case [Num(_), Num(t)] => p += t * 2 + 1
  \\      case [..] => ""
  \\    end
  \\  end
  \\  case M(_) => assert(false, '')
  \\  case L([[Num(t)]]) => p = t
  \\  case L(_) => assert(false, '')
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-7" {
  const src =
  \\ type NumStr = Num(num) | Str(str)
  \\ let x: List{NumStr} = [Num(1) as NumStr, Num(2)]
  \\ let p = 0
  \\ match x
  \\  case [Num(_), Num(t)] => p += t * 2 + 1
  \\  case [..] => let k = p
  \\  case _ => ""
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-8" {
  const src =
  \\ let x: num? = Just(9)
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
  \\ type StrNum = S(str) | N(num)
  \\ let x: StrNum = N(5)
  \\ let p = 25
  \\ match x
  \\  case N(t) => p /= t
  \\  case S(..) => assert(false, "")
  \\ end
  \\ assert(p == 5, 'should be 5')
  ;
  try doRuntimeTest(src);
}

test "narrowing-10" {
  const src =
  \\ type ListNum = N(num) | L(List{num})
  \\ type T = Col(List{ListNum}) | Nu(num)
  \\ let x: T = Col([N(9) as ListNum])
  \\ let p = 90
  \\ match x
  \\  case Col([N(t)] as q) => do
  \\   p /= t
  \\   q[0] = N(p)
  \\  end
  \\  case _ => assert(false, '')
  \\ end
  \\ assert(p == 10, 'p should be 10')
  ;
  try doRuntimeTest(src);
}

test "narrowing-14" {
  const src =
  \\ type T = L(List{num}) | S(str)
  \\ let x: T? = Just(L([5]) as T)
  \\ match x.?
  \\  case L([1]) => assert(false, 'no')
  \\  case L([5]) => assert(true, 'yes')
  \\  case L([..]) => assert(false, 'no')
  \\  case S('a') => assert(false, 'no')
  \\  case S(_) => assert(false, 'no')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-15" {
  const src =
  \\ let x: Tuple{List{num}, str?} = ([5], Just('foo') as Maybe{str})
  \\ if x[0] is List{num} and x[1].? is str
  \\    x[0][0] += x[1].?.len()
  \\ else
  \\    assert(false, '')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-16" {
  const src =
  \\ let x: Tuple{str, num} = ('a', 5)
  \\ let p = 0
  \\ if (x[1] is num and 5 > 2) or x[1] is num
  \\    p = x[1] * 5
  \\ else
  \\    assert(false, 'oops')
  \\ end
  \\ assert(p == 25, 'should be 25')
  ;
  try doRuntimeTest(src);
}

test "narrowing-17" {
  const src =
  \\ let x = 0
  \\ let y = 'a'
  \\ if x is num and x is num or y is str
  \\    x # num | str
  \\    x += y.len()
  \\ else
  \\    assert(false, 'never')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-18" {
  const src =
  \\ do
  \\   let x: List{num?} = [Just(5) as num?]
  \\   let p = x as List{num?}
  \\   if p[0].? is num # redundant but okay
  \\      p[0].?? += 12 # type checks because x is a list with one type
  \\   end
  \\   assert(p[0].? == 17, 'should be 17')
  \\ end
  \\ ()
  \\ let x: List{num?} = [Just(5) as num?]
  \\ let p = x as List{num?}
  \\ if p[0].?? is num # redundant but okay
  \\    p[0].?? += 12
  \\ end
  \\ assert(p[0].?? == 17, 'should be 17')
  \\ ()
  \\ let t: num = 0
  \\ let v: Tuple{Map{num, num}, Tuple{num, str}} = ({1: 4}, (15, 'abc'))
  \\ if v[1] is Tuple{num, str}
  \\    let p = v[1][0]
  \\    if p is num
  \\      t += p + 1
  \\    end
  \\ end
  \\ assert(t == 16, 'should be 16')
  ;
  try doRuntimeTest(src);
}

test "narrowing-19" {
  const src = 
  \\ type NumStr = N(num) | S(str)
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
  \\    exit(2)
  \\  else
  \\    assert(true, 'oops')
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
  \\ type NumStr = N(num) | S(str)
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
  \\ type NumStr = N(num) | S(str)
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
  \\  case Fx(Fox(x=S(_), ..)) => assert(false, 'bad')
  \\  case Fo(_) => assert(false, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-23" {
  const src =
  \\ let j = 5
  \\ if j is num and j > 5
  \\  j
  \\ else
  \\  assert(j + 4 == 9, 'should be 9')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-24" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ let x: Maybe{NumStr} = Just(N(4) as NumStr) as NumStr?
  \\ let z = 0
  \\ match x.?
  \\  case N(t) => z = t + 5
  \\  case S(_) => assert(false, 'oops')
  \\ end
  \\ assert(z == 9, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "narrowing-27" {
  const src =
  \\ let j: Tuple{List{List{num}}, List{str}} = ([[5]], ['ok'])
  \\ if j[0] is List{List{num}} and j[0].len() == 1
  \\  assert(j[0][0] is List{num}, 'is list')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "void narrowing" {
  const src =
  \\ type VoidNum = V(void) | N(num)
  \\ def voidy
  \\ end
  \\
  \\ def fox(x: bool)
  \\  if x then
  \\    return N(3)
  \\  end
  \\  return V(voidy())
  \\ end
  \\
  \\ let t = fox(false)
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
  \\ type boolean = T(true) | F(false)
  \\ let p: Animal = Animal.F('fox')
  \\ let x: Even = Even.S(6)
  \\ let y: boolean = boolean.T(true)
  \\ let t: bool = !!y
  \\ x = Even.E(8.6)
  \\ p = Animal.D('dog')
  \\ y = boolean.F(false)
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
  \\ let k: num? = None
  \\ k = Just(p.?? as num + 25)
  \\ assert(k.?? == 30, 'should be 30')
  \\ alias A = 5
  \\ let x: List{A} = [5, 5, 5]
  \\ let y: List{'foo'} = ['foo', 'foo', 'foo']
  \\ let z: Map{'name', str} = {'name': 'ziord'}
  \\ assert(z['name'] == 'ziord', 'should be ziord')
  \\ assert(5 as 5 as num + 5 == 10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "while loop" {
  const src =
  \\ do
  \\    let x = 5
  \\    while x is num and x < 25 do
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
  \\ while x is num and x < 25 do
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
  \\ while x is num and x < 25 do
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
  \\ while x is num and x < 25 do
  \\  if x % 5 == 0 then
  \\    break
  \\  end
  \\  x += 5
  \\ end
  \\ x
  \\ let x = 5
  \\ while x as num < 25 do
  \\  let p = x as num
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
  \\ alias T = num
  \\ def j(a: T): T
  \\  return (a * 2)
  \\ end
  \\ assert(j(5) + 9 == 19, 'should be 19')
  ;
  try doRuntimeTest(src);
  const src2 =
  \\ do
  \\ def fox(a: num)
  \\  def foo(b: num)
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
  \\ def fib(n: num): num
  \\  if n <= 1 then
  \\    return n
  \\  end
  \\  return fib(n - 1) + fib(n - 2)
  \\ end
  \\ assert(fib(13) == 233, 'should be 233')
  ;
  try doRuntimeTest(src3);
  const src4 =
  \\ alias T = num
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
  \\    let j = foo{str}('5')
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
  \\ (fancy(5), fancy('oops'), fancy(true), id((1, 2, {'a': 'fox'})))
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
  \\ funny2{num}()
  ;
  try doRuntimeTest(src3);
}

test "functions-3" {
  const src =
  \\ def add{T} (k: T, t: num)
  \\    return (k, t)
  \\ end
  \\ add(3, 4)
  \\ add('fox', 12)
  \\ def add5{T}(a: T)
  \\  return a + 5
  \\ end
  \\ add5(12)
  \\ def add5(a: num)
  \\  return a + 5 * 2
  \\ end
  \\ add5(12)
  \\ let minus = [def (a: num, b: num) => a - b][0]
  \\ minus(132, 12)
  \\ let p = minus
  \\ p(12, 4)
  ;
  try doRuntimeTest(src);
}

test "functions-4" {
  const src =
  \\ do
  \\ def add {T} (k: T, t: num)
  \\    return (k, t)
  \\ end
  \\ add(3, 4)
  \\ add('fox', 12)
  \\ def add5{T}(a: T)
  \\  return a + 5
  \\ end
  \\ add5(12)
  \\ def add5(a: num)
  \\  return a + 5 * 2
  \\ end
  \\ add5(12)
  \\ let minus = [def (a: num, b: num): num
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
  \\ let j = (89, def(x: num) => x * 2, def(y: num) => y + 5)
  \\ if !!j[1]
  \\  if j[0] is num
  \\    let p = j[0] + j[1](16)
  \\    assert(p == 121, '121')
  \\  end
  \\ end
  \\ j
  \\
  \\ do
  \\  def higher{T, J}(x: num): fn(J): T
  \\   return def (y: num): T => x * y
  \\  end
  \\  let mul = higher{num, num}(5)
  \\  assert(mul(6) == 30, 'should be 30')
  \\ end
  \\
  \\ def higher{T, J}(x: num): fn(J): T
  \\  return def (y: num): T => x * y
  \\ end
  \\ let mul = higher{num, num}(5)
  \\ assert(mul(12) == 60, 'should be 60')
  ;
  try doRuntimeTest(src);
}

test "functions-6" {
  const src =
  \\ do
  \\  assert((def (x: str) 
  \\   return x
  \\  end)('ppp') == "ppp", 'should be "ppp"')
  \\ end
  \\
  \\ let j = 12
  \\ (def => j * 3)() == 36
  \\ [(def => j * 3)][0]() + 12 == 48
  \\
  \\ do
  \\  let j = 12
  \\  assert((def => j * 3)() == 36, 'should be 36')
  \\  [(def => j * 3)][0]() + 12 == 48
  \\  (def => [(def => j * 3)][0]() + 12 == 48)()
  \\ end
  \\ let j = 6
  \\ assert((def => [(def => j * 3)][0]() + 6 == 24)(), 'should be true')
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
  \\ apply(def (x: num) => x * x, 5)
  \\ apply(def (x: str) => x, 'fox')
  \\ do
  \\  def apply{T}(x: fn(T):T, param: T): T
  \\   return param
  \\  end
  \\  apply(def (x: num) => x * x, 5)
  \\  apply(def (x: str) => x, 'fox')
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
  \\ def apply{T, K}(a: num, param: T, x*: fn(T):K): T
  \\  return x[0](param) + param + a
  \\ end
  \\ assert(apply(12, 5, def (x: num) => x * x) == 42, 'should be 42')
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
  \\ apply(def (x: num) => x * x, 5)
  \\ apply(def (x: str) => x, 'fox')
  \\ do
  \\  def apply{T}(x: fn(T):T, param: T): T
  \\   return param
  \\  end
  \\  apply(def (x: num) => x * x, 5)
  \\  apply(def (x: str) => x, 'fox')
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
  \\  assert((def (x: str) 
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
  \\  (def => j * 3)() == 36
  \\  [(def => j * 3)][0]() + 12 == 48
  \\  assert((def => [(def => j * 3)][0]() + 12 == 48)(), 'should be 48')
  \\ end
  \\ let j = 6
  \\ assert((def => [(def => j * 3)][0]() + 6 == 24)(), 'true')
  \\ let j = (89, def(x: num) => x * 2, def(y: num) => y + 5)
  \\ if !!j[1]
  \\  if j[0] is num
  \\    assert(j[0] + j[1](16) == 121, 'should be 121')
  \\  end
  \\ end
  \\ j
  \\
  \\ do
  \\  def higher{T, J}(x: num): fn(J): T
  \\   return def (y: num): T => x * y
  \\  end
  \\  let mul = higher{num, num}(5)
  \\  assert(mul(6) == 30, 'true')
  \\ end
  \\
  \\ def higher{T, J}(x: num): fn(J): T
  \\  return def (y: num): T => x * y
  \\ end
  \\ let mul = higher{num, num}(5)
  \\ assert(mul(12) == 60, 'should be 60')
  \\ do
  \\  def add {T} (k: T, t: num)
  \\     return (k, t)
  \\  end
  \\  add(3, 4)
  \\  add('fox', 12)
  \\  def add5{T}(a: T)
  \\   return a + 5
  \\  end
  \\  add5(12)
  \\  def add5(a: num)
  \\   return a + 5 * 2
  \\  end
  \\  add5(12)
  \\  let minus = [def (a: num, b: num): num
  \\   return a - b
  \\  end][0]
  \\  assert(minus(132, 12) ==  120, 'should be 120')
  \\  let p = minus
  \\  assert(p(12, 4) == 8, 'should be 8')
  \\ end
  \\ let j = def => 5
  \\ j()
  \\
  \\ def add{T} (k: T, t: num)
  \\    return (k, t)
  \\ end
  \\ add(3, 4)
  \\ add('fox', 12)
  \\ def add5{T}(a: T)
  \\  return a + 5
  \\ end
  \\ add5(12)
  \\ def add5(a: num)
  \\  return a + 5 * 2
  \\ end
  \\ add5(12)
  \\ let minus = [def (a: num, b: num) => a - b][0]
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
  \\ funny2{num}()
  \\ def funny
  \\    def foo{T}(a: T): T
  \\     return a
  \\    end
  \\    let j = foo{str}('5')
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
  \\ (fancy(5), fancy('oops'), fancy(true), id([1, 2, 12]))
  ;
  try doRuntimeTest(src);
}

test "functions-9" {
  const src =
  \\ type NSB = N(num) | S(str) | B(bool)
  \\ def ret3(n: num): NSB
  \\  if n < 5
  \\    return N(3)
  \\  end
  \\  if n < 12
  \\    return S('hey')
  \\  end
  \\  if n > 15
  \\    return B(true)
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
  \\ (def (x: num) => x * x)(12)
  \\ (def (x: num)
  \\  return x * x
  \\ end)(12)
  \\
  \\ ret3(7)
  \\ let t = ret(N(7))
  \\ match t
  \\  case N(t) => assert(t == 19, 'should be 19')
  \\  case _ => assert(false, 'yeah')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "functions-10" {
  const src = 
  \\ def fox{T}(x: T): fn(T): num
  \\  def fun(p: T): num
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
   \\ def fox(x: num): fn(num): num
  \\  return def (p: num): num  => p * x
  \\ end
  \\
  \\ let j = fox(5)
  \\ assert(j(4) == 20, 'should be 20')
  ;
  try doRuntimeTest(src2);
  const src3 =
  \\ type NS = N(num) | S(str)
  \\ def big: NS
  \\  return N(5)
  \\ end
  \\ let j = big()
  \\ match j
  \\  case N(t) => assert(t == 5, 'should be 5')
  \\  case _ => assert(false, 'oops')
  \\ end
  ;
  try doRuntimeTest(src3);
  const src4 =
  \\ type Ty{T} = A(T) | B(str)
  \\ alias Fun = fn(T): Ty{T}
  \\ alias T = num
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
  \\  case _ => assert(false, 'oops')
  \\ end
  ;
  try doRuntimeTest(src4);
}

test "functions-11" {
  const src =
  \\ let j = [def (x: num) => x * x, def (y: num) => ~y]
  \\ let t = 1
  \\ let p = j[t]
  \\ assert(p(6) == -7, 'should be -7')
  \\
  \\ let j = [def (x: num) => x * x, def (y: num) => ~y]
  \\ let t = 1 * 0
  \\ let p = j[t]
  \\ assert(p(12) == 144, 'should be 144')
  \\
  \\ assert([def (x: num) => x * x, def (y: num) => ~y][-1](t + 7) == -8, 'should be -8')
  \\
  \\ let j = [def (x: num) => x * x, def (y: num) => y >> (1 << y)]
  \\ let v = [def (x: num) => x * x, def (y: num) => ~y][t - 1](t + 7)
  \\ assert(v == -8, 'should be -8')
  ;
  try doRuntimeTest(src);
}

test "functions-13" {
  const src =
  \\ def fun: void
  \\  let p = 10
  \\  p += 5
  \\ end
  \\
  \\ let j = fun()
  \\ assert(j is void, 'should be void')
  ;
  try doRuntimeTest(src);
}

test "functions-14" {
  const src =
  \\ def fun: void
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
  \\ assert([fun()][0] is void, 'is void')
  ;
  try doRuntimeTest(src);
}

test "functions-16-varargs" {
  const src =
  \\ do
  \\ def fun(args*: num)
  \\  return args
  \\ end
  \\
  \\ let j = fun(1, 2, 3)[0] + 12
  \\ assert(j == 13, 'j should be 13')
  \\ fun()
  \\ end
  \\
  \\ def foo(a: num, rest*: num)
  \\  return (a * rest[0], rest)
  \\ end
  \\
  \\ let res = foo(5, 3)
  \\ if res[0] is num
  \\  assert(res[0]==15, 'should be 15')
  \\ else
  \\  assert(false, 'oops')
  \\ end
  \\ res = foo(12, 2, 3, 4, 5, 6)
  \\ if res[0] is num
  \\  assert(res[0]==24, 'should be 24')
  \\ else
  \\  assert(false, 'oops')
  \\ end
  \\ if res[1] is List{num}
  \\  assert(res[1][0]==2, 'should be 2')
  \\  assert(res[1][4]==6, 'should be 6')
  \\ else
  \\  assert(false, 'oopsx')
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
  \\  foo{num}(6, 7)
  \\  do
  \\   println(1, 2, 3, 4)
  \\  end
  \\ end
  \\ println(println(), 'jeryr')
  \\ (def (p*:any) => println(p))(1, 2, 'a', 'b', false, true, None)
  \\ let j: any? = None
  ;
  try doRuntimeTest(src);
}

test "functions-18" {
  const src =
  \\ def fun(): num
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
  \\ alias T = num
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
  \\ assert((def (x: List{num}) 
  \\   return x
  \\ end)([5]) != [5], 'objects are not strictly equal')
  ;
  try doRuntimeTest(src);
}

test "functions-21" {
  const src =
  \\ def fun(a*: any)
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
  \\ def test(n: num)
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
  \\ def test(n: num)
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
  \\ def funny(t: List{str})
  \\  println(t)
  \\ end
  \\ funny([] as List{str})
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

test "functions-25.<void>" {
  const src =
  \\ def foo()
  \\  println('yay')
  \\ end
  \\ let j:void = foo()
  \\ println('j is', j)
  ;
  try doRuntimeTest(src);
}

test "functions-25.<void/never/return>" {
  const src =
 \\ def check(n: num)
  \\  if n is num
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
  \\ assert(true, 'ok')
  \\ assert(!!exit, 'exit')
  \\ assert(!!assert, 'assert')
  \\ # assert(!!panic, 'panic')
  \\ assert(!!print, 'print')
  \\ assert(!!println, 'println')
  \\ (exit, assert)
  ;
  try doRuntimeTest(src);
}

test "builtin-functions-override" {
  const src =
  \\ def exit(x: num)
  \\  return x - 2
  \\ end
  \\
  \\ assert(exit(10) == 8, 'okay')
  \\
  \\ let check = assert
  \\ def assert{T}(t: T)
  \\  check(t, 'nice')
  \\ end
  \\ assert(!!check)
  \\
  \\ def print(x*: any)
  \\  return x
  \\ end
  \\ check(print('fox', 'fry', 1, 2, 3)[0] == 'fox', 'should be "fox"')
  ;
  try doRuntimeTest(src);
}

test "no-strict-varargs-immutability" {
  const src =
  \\ def fun(x*: num)
  \\  println('x before', x)
  \\  let before = x.len()
  \\  change(x)
  \\  println('x after', x)
  \\  assert(x.len() > before, 'true')
  \\ end
  \\
  \\ def change(x: List{num})
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
  \\  def fun(x: num)
  \\   if x > 2
  \\     return Ok(())
  \\   end
  \\   return ('foo')!
  \\  end
  \\  let tmp = fun(1)
  \\  match tmp
  \\    case Ok(() as t) => assert(false, 'bad')
  \\    case Error(t) => assert(t == 'foo', 'value should be foo')
  \\  end
  \\  let j = try fun(3)
  \\  let k = fun(3) orelse |e| panic(e)
  \\  println(j, k)
  \\ end
  \\
  \\ def fun(x: num)
  \\  if x > 2
  \\    return Ok(())
  \\  end
  \\  return ('foo')!
  \\ end
  \\ let tmp = fun(1)
  \\ match tmp
  \\   case Ok(() as t) => assert(false, 'bad')
  \\   case Error(t) => assert(t == 'foo', 'value should be foo')
  \\ end
  \\ let j = try fun(3)
  \\ let k = fun(3) orelse |e| panic(e)
  \\ println(j, k)
  ;
  try doRuntimeTest(src);
}

test "errors-2" {
  const src =
  \\ def stup(x: num)
  \\  if x > 2
  \\    return None
  \\  else
  \\    return Error('oops')
  \\  end
  \\ end
  \\ match stup(1)
  \\  case Error(t) => assert(t == 'oops', 'ok')
  \\  case None => assert(false, 'nah')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "errors-2b" {
  const src =
  \\ def fun(x: num)
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
  \\  def fun(x: num)
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
  \\  def fun(x: num)
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
  \\  def fun(x: num)
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
  \\ def fun(x: num)
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
  \\ def fancy(x: num)
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
  \\  case Error('bad') => assert(true, 'value should be bad')
  \\  case Error(_) => assert(false, 'nono')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "simple-classes-1" {
  const src =
  \\ class Fox
  \\    pub x: num
  \\    pub u = 12
  \\    def init(): void
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
  \\    pub x: num
  \\    pub u = 12
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ class Racoon
  \\    pub x: num
  \\    pub u = 12
  \\    def init(): void
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
  \\  pub x: num = 10
  \\ end
  \\
  \\ let j = Foo()
  \\ assert(j.x == 10, 'j.x should be 10')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-4" {
  const src =
  \\ type NumStr = N(num) | S(str)
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
  \\ type NumStr = N(num) | S(str)
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
  \\ let _ = fun{num}([1, 2, 3, 4])
  \\ println('-->', _, 'oopsy'.len())
  \\ assert('oopsy'.len() == 5, 'should be 5')
  \\ assert(_[-1] == 5, 'last item should be 5')
  \\
  \\ let x = []
  \\ x.len
  \\ x.append(8)
  \\ println(x)
  \\ assert(x.len() == 1, 'len should be 1')
  ;
  try doRuntimeTest(src);
}

test "functions-1.<narrowing-return>" {
  const src =
  \\ type NumStr = N(num) | S(str)
  \\ def chee(n: NumStr): num
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
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\
  \\    pub def getGen()
  \\      alias T = Tuple{str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{num}(6, 7, 8)
  \\ let t: Fox{num} = x
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
  \\ let _ = j.append
  \\ j.append(5)
  \\ _(1)
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
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    pub def pulse()
  \\      return self
  \\    end
  \\
  \\    pub def getGen()
  \\      alias T = Tuple{str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{num}(6, 7, 8)
  \\ let t: Fox{num} = x
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
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\ end
  \\ let x = Fox{num}(6, 7, 8)
  \\ let y = Fox{str}("a", "b")
  \\ type FF{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: FF{num, str} = F1(x)
  \\ match j
  \\  case F1(Fox(_) as w) => do
  \\    if w is Fox{num}
  \\       w.x = [w.x[0] + 5, w.x[1]]
  \\       assert(w.x[0] == 11, 'should be 11')
  \\    else
  \\      assert(false, 'oopsy')
  \\    end
  \\  end
  \\  case F2(_ as q) => do
  \\    if q is Fox{str}
  \\     q.x = ["hello world", q.x[1]]
  \\     println(q, q.x)
  \\    else 
  \\      assert(false, 'oops')
  \\    end
  \\  end
  \\ end
  ;
  try doRuntimeTest(src);
}

test "labeled-argument" {
  const src =
  \\ def fun(x: str, y: num, a: List{num}, b: Result{void, str})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  println('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y=5, a=[2, 3], x='oo', b=('oops')!)
  \\ fun(y=5, a=[2, 3], b=('oops')!, x='oo')
  \\ 
  \\ def fun(x: str, y: num, a*: List{num})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  println('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y=5, a=[2, 3], x='oo', a=[1, 2], a=[5, 6, 7])
  \\ fun(y=5, a=[2, 3], x='oo', a=[1, 2], a=[5, 6, 7])
  \\ fun(y=5, a=[2, 3], x='oo', a=[1, 2], a=[5, 6, 7])
  \\
  \\ def fun(x: str, y: num, a*: List{num})
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
  \\  pub a: num
  \\  pub b: str
  \\  def init(a: num, b: str)
  \\    self.a = a
  \\    self.b = b
  \\  end
  \\  pub def send(data: List{any})
  \\    let i = 0
  \\    while i < data.len()
  \\      println('sending...', data[i])
  \\      i += 1
  \\    end
  \\  end
  \\ end
  \\ let f = Fun(b='oops', a=12)
  \\ println(f.a, f.b)
  \\ f.send(data=['a' as any, 1, f])
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
  \\ assert(!j.set('a', 12), 'should be true')
  \\ assert(j.set('f', 7), 'should be true')
  \\ assert(j.len() == 5, 'should be 5')
  \\ assert(j.get('f').?? == 7, 'should be 7')
  \\ # delete
  \\ assert(j.delete('a'), 'should be true')
  \\ assert(j.len() == 4, 'should be 4')
  \\ # keys, values
  \\ assert(j.keys().len() == 4, 'should be 4')
  \\ assert(j.keys()[0] == 'b', 'should be b')
  \\ assert(j.keys()[-1] == 'f', 'should be f')
  \\ assert(j.values().len() == 4, 'should be 4')
  \\ assert(j.values()[0] == 2, 'should be 2')
  \\ assert(j.values()[-1] == 7, 'should be 7')
  \\ # remove
  \\ assert(j.delete('b'), 'should be true')
  \\ assert(j.len() == 3, 'should be 3')
  \\ # items
  \\ let itm0 = j.items()[0]
  \\ assert(itm0[0] == 'c', 'should be c')
  \\ assert(itm0[1] == 3, 'should be 3')
  \\ assert(j.items().len() == 3, 'should be 3')
  \\ # listItems
  \\ assert(j.listItems().len() == 6, 'should be 6')
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

test "builtin-str" {
  const src =
  \\ let j = "coolstuff"
  \\ # len
  \\ assert(j.len() == 9, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "tagged unions" {
  const src =
  \\ type Option = Some(List{num}) | Nil
  \\ let j: Option = Some([5])
  \\ j = Nil
  \\ # -- #
  \\ type Tree{T} = Branch(Tree{T}, Tree{T}) | Node(T)
  \\ let tree: Tree{num} = Branch(Node(1) as Tree{num}, Node(2) as Tree{num})
  \\ # -- #
  \\ type Many{T} = More(Many{T}) | One(T)
  \\ let x: Many{num} = More(One(45) as Many{num})
  \\ # -- #
  \\ alias T = num
  \\ type Tree = Branch(Tree, Tree) | Node(T)
  \\ let tree: Tree = Branch(Node(1), Node(2))
  \\ # -- #
  \\ type Tree{T} = Leaf | Node(T, Tree{T}, Tree{T})
  \\ let tree2: Tree{num} = Node(5, Node(1, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf)))
  \\ tree2 = Leaf
  \\ let tree2 = Node(5, Node(1, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf)))
  \\ type Tree{T} = Node(val:T, lhs:Tree{T}, rhs:Tree{T}) | Leaf
  \\ let tree2 = Node(val=4, lhs=Leaf, Node(val=3, lhs=Leaf, rhs=Leaf))
  \\ let j: Tree{num} = Node(2, Leaf, Leaf)
  \\ j = tree2
  \\ # -- #
  \\ type Pair{K, V} = Pair(K, V)
  \\ let p:Pair{str, str} = Pair('a', 'b') 
  \\ # -- #
  \\ type Pair{K, V} = Pair(K, V)
  \\ let p:Pair{'a', 'b'} = Pair('a' as 'a', 'b' as 'b')
  \\ println(p)
  \\ let p:Pair{str, str} = Pair('a', 'b')
  \\ println(p)
  \\ # -- #
  \\ let k = Just(None)
  \\ println(k)
  \\ # -- #
  \\ let t: Maybe{Maybe{num}} = Just(Just(5) as Maybe{num})
  \\ println(Just(Just(None)), t)
  \\ # -- #
  \\ type R = Str(str) | Num(num) | Col(List{R})
  \\ [Str('foo') as R] as List{R}
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
  \\ let z = false
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a', 'b' as o) as d => z = true
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
  \\ let z = false
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
  \\    z = true
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
  \\ let z = false
  \\ let j: Cons = B
  \\ match j
  \\  case A => println('a')
  \\  case B => do
  \\    println('ok')
  \\    z = true
  \\  end
  \\  case C => println('hmm')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-5.<match on classes>" {
  const src =
  \\ let z = false
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
  \\    z = true
  \\  end
  \\  case R(Rat(..)) => println(30)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-6.<tuple exhaustiveness>" {
  const src =
  \\ let z = false
  \\ match ('a', 'b')
  \\  case ('x', 'y') => println('first')
  \\  case ('a', 'b' as o) as d => z = true
  \\  case ('q', 'k') => println('third')
  \\  case (..) => println("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-7.<list exhaustiveness>" {
  const src =
  \\ let z = false
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => println('first')
  \\  case [('a', 'b' as o)] as d => z = true
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
  \\ let z = false
  \\ class Cat
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Animal = C(Cat())
  \\ let z = false
  \\ match p
  \\  case D(Dog()) => println('good')
  \\  case C(Cat()) => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-9.<nested match on classes>" {
  const src =
  \\ let j = [] as List{num}
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
  \\  case _ => assert(false, 'bad match')
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
  \\  case _ => assert(false, 'bad match') # TODO: warns. false positive
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
  \\  case _ => assert(false, 'bad match')
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
  \\  case _ => assert(false, 'bad match')
  \\ end
  \\ assert(e1 == 1, 'should be 1')
  \\ assert(e2 == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-13.<match on bool>" {
  const src =
  \\ let z = false
  \\ let j = [false]
  \\ match j
  \\  case [a, ..] => match a
  \\    case true => println('yay')
  \\    case false as w => z = !w
  \\  end
  \\  case [..] => println('bad')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-14.<match on bool>" {
  const src =
  \\ let z = false
  \\ match (1 < 2)
  \\  case false => println('nay')
  \\  case true as t => z = !!t
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-15.<ranges>" {
  const src =
  \\ let z = false
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => println('hey')
  \\    case 3..5 => z = true
  \\    case _ => println('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-16.<ranges>" {
  const src =
  \\ let z = false
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => println('hey')
  \\    case 3..5 => z = true
  \\    case _ => println('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-17.<nullable types>" {
  const src =
  \\ let z = false
  \\ type AB = A("a") | B("b")
  \\ let j: AB? = Just(B("b") as AB)
  \\ match j
  \\   case Just(A("a")) => println('a!')
  \\   case Just(B("b")) => z = true
  \\   case None => println('nah!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-18.<nullable types>" {
  const src =
  \\ let z = false
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
  \\ let z = false
  \\ type Fx{T, V} = F1(Fox{T}) | F2(Fox{V})
  \\ let j: Fx{str,num} = F1(Fox('pin'))
  \\ match j
  \\  case F2(Fox(6)) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\  case F1(Fox('pin')) as x => z = true
  \\  case F1(Fox(_)) => println('caught ya str')
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
  \\ let z = false
  \\ type Fx{T, V} = 
  \\  F1(Fox{T}) 
  \\  | F2(Fox{V}) 
  \\  | F3(Ant)
  \\ let j: Fx{str,num} = F3(Ant())
  \\ match j
  \\  case F2(Fox(6)) => println('whew')
  \\  case F1(Fox('pin')) as x => println('pin')
  \\  case F2(Fox(_)) => println('caught ya str')
  \\  case F1(Fox(_)) => println('caught ya num')
  \\  case F3(Ant()) => z = true
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
  \\ let j: Fx{str, num} = F1(Fox{str}('pin', 'pan'))
  \\ let z = false
  \\ match j
  \\  case F1(Fox(['pin', 'pan'])) as x => z = true
  \\  case F1(Fox(_)) => println('caught ya str')
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
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
  \\ let j: Fx{str, num} = F1(Fox{str}('pin', 'pan'))
  \\ let z = false
  \\ match j
  \\  case F1(Fox(['^_^',])) => println('caught ya str')
  \\  case F1(Fox(..)) as x => z = true
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
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
  \\ let j: Fx{str, num} = F1(Fox{str}('pin', 'pan'))
  \\ let z = false
  \\ match j
  \\  case F1(Fox(['pin', ..])) as x => z = true
  \\  case F1(Fox(_)) => println('caught ya str')
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
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
  \\ let j: Fx{str, num, str} = F3(Ant{str}())
  \\ let z = false
  \\ match j
  \\  case F1(Fox(..)) as x => println('yes', x)
  \\  case F2(Fox([6,])) => println('whew')
  \\  case F2(Fox(_)) => println('caught ya num')
  \\  case F3(Ant()) => z = true
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
  \\ let z = false
  \\ match Bug()
  \\  case Bug(1, _, ..) => println('bad')
  \\  case Bug(5, _, ..) => z = true
  \\  case _ => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-26.<match on rested>" {
  const src =
  \\ let z = false
  \\ let j = ('a', true, 'b', 2, 'c', 3)
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
  \\ let z = false
  \\ let j = ['a', '1', 'b', '2', 'c', '3']
  \\ match j
  \\  case ['x', _] => println('yay')
  \\  case ['a', _ as t, ..] => z = true
  \\  case [a, b] => println('nay')
  \\  case _ => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-28.<match on generics>" {
  const src =
  \\ let z = false
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
  \\ let j = err(Err('bad')) as Res{num, str}
  \\
  \\ match j
  \\  case err(Err(v)) => z = true
  \\  case ok(Oky(k)) => println('ok:', k)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-29.<match on maps>" {
  const src =
  \\ let z = false
  \\ let j = {'a': 1, 'b': 2, 'c': 3}
  \\ match j
  \\  case {'x': _} => println('yay')
  \\  case {'a': _ as t, ..} => z = true
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
  \\  pub url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let z = false
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ match foo
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": Str("txt"),} => z = true
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
  \\  pub url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let foo = [Fox('fin.co')]
  \\ let z = false
  \\ match foo
  \\  case [ Fox(url) as b ] => do
  \\    assert(url as bool and b as bool, 'should be true')
  \\    z = true
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
  \\  pub url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ type Fmt = Class(Fox) | Str(str)
  \\ let foo = {"sound": Class(Fox('fin.co')) as Fmt, "format": Str("txt")}
  \\ let z = true
  \\ match foo
  \\  case {"sounds": _, "format": _} => println(1)
  \\  case {"sound" as a: Class(Fox(url)) as b, "format": _,} => z = true
  \\  case {..} => println('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-33.<match on maps>" {
  const src =
  \\ let foo = [{"sound": 1, "format": 2}, {"pin": 3, "pan": 4}]
  \\ let z = false
  \\ match foo
  \\  case [{"sound": 1}] => println('first')
  \\  case [{"sound": 2, ..}] => println('second')
  \\  case [{"sound": _, ..}, ..] => z = true
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
  \\ let z = false
  \\ match foo
  \\  case [{"sound": 1}] => println('first')
  \\  case [{"sound": 2, ..}] => println('second')
  \\  case [{..}, ..] => z = true
  \\  case [..] => println('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-35.<guards with blocks>" {
  const src =
  \\ let foo = [5, 3]
  \\ let z = false
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
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}.listItems()
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
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}
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
  \\ def check(n: num)
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
  \\ def fib(n: num)
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
  \\ def fib(n: num)
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
  \\ def check(n: num)
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
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 18..50 as q => return (q + n) / 2 # same as
  \\   case _ => return 3
  \\  end
  \\ end
  \\ def fib(n: num)
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
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 11..50 as q => return q + n 
  \\   case _ => return 3
  \\  end
  \\ end
  \\ def fib(n: num)
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
  \\ def check(n: num)
  \\  match n
  \\   case 1..4 => return 1
  \\   case 5..9 => return 2
  \\    case 11..50 as q => return fib(q / 2)
  \\   case _ => return 3
  \\  end
  \\ end
  \\ def fib(n: num)
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
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}
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
  \\ let z = false
  \\ match {'ab': false, 'b': true, 'c': false}
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
  \\ let z = false
  \\ match {'ab': false, 'b': true, 'c': false}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} as t if z => z = false
  \\  case {..} => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-47.<match on maps>" {
  const src =
  \\ let z = false
  \\ match {'ab': false, 'b': true, 'c': false}
  \\  case {..} as t if !z => z = !!t.len()
  \\  case {..} => println('got something!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-48.<match on maps (guarded rested)>" {
  const src =
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}
  \\  case {..} as t if z => assert(z, 'should be false')
  \\  case _ => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-49.<match on maps>" {
  const src =
  \\ let z = false
  \\ match {'ab': false, 'b': true, 'c': false}
  \\  case {'x': _, 'c': _, ..} => println('has keys "x" and "c"')
  \\  case {'a': _, 'b': _ as p, ..} if p => z = p
  \\  case {..} as t if z => z = false
  \\  case _ => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-50.<match on lists (guarded rested)" {
  const src =
  \\ let z = false
  \\ match ('a', false, 'b', true, 'c', false)
  \\  case ('x', _, 'c', _, ..) => println('has keys "x" and "c"')
  \\  case ('a', _, 'b', _ as p, ..) if z => println(z)
  \\  case (..) if !z => z = true
  \\  case (..) => println('otherwise')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-51.<match on lists (guarded rested)>" {
  const src =
  \\ let z = false
  \\ match {'a': false, 'b': true, 'c': false}.listItems()
  \\  case [Key('a'), Value(false), ..] as t if z => assert(z, 'should be false')
  \\  case [..] as t if z => assert(z, 'should be false')
  \\  case _ => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-52.<error patterns>" {
  const src =
  \\ let z = false
  \\ def goodOrBad(n: num)
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
  \\ z = false
  \\ match goodOrBad(15)
  \\  case Error(error) => println(error)
  \\  case Ok(1..30) as x => z = true
  \\  case _ as t => println('def is', t)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-53.<block transform pattern>" {
  const src =
  \\ let j: num = 1
  \\ match j
  \\  case t => println(t)
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-54.<non unique tags>" {
  const src =
  \\ type T{K, V} = K(K) | V(V)
  \\ let x: T{str, num} = V(6)
  \\ match x
  \\  case K(t) => assert(false, 'bad')
  \\  case V(u) => assert(u == 6, 'okay')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-55.<unused tag params>" {
  const src =
  \\ type T{K, V} = K
  \\ let x: T{str, num} = K
  \\ println(x)
  \\ let x: T{str, num} = K
  \\ match x
  \\  case K => assert(true, 'okay')
  \\  case _ => assert(false, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<generic qualified tag access>" {
  const src =
  \\ type T{K, V} = K
  \\ let x: T{str, num} = K
  \\ println(x)
  \\ let x: T{str, num} = K
  \\ match x
  \\  case K => assert(true, 'okay')
  \\  case _ => assert(false, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<or patterns in tags>" {
  const src =
  \\ type StrNum = Str(str) | Num(num)
  \\ let x: StrNum? = Just(Str('foobar') as StrNum)
  \\ let p = 10
  \\ match x
  \\  case Just(Str(f) | Num(q)) => assert(true, 'yes')
  \\  case None => assert(false, 'yada')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<qualified tag access>" {
  const src =
  \\ type T = K(str) | J(num)
  \\ let x: T = T.K('fox')
  \\ println(x)
  \\ let x: T = T.J(5)
  \\ match x
  \\  case J(t) => assert(!!t, 'okay')
  \\  case _ => assert(false, 'bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<shadowing via aliased capture>" {
  const src =
  \\ def fun(x: num)
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
  \\ type T = Tag(a: num, b: str, c: bool)
  \\  match Tag(a=5, b='oops', c=false)
  \\    case Tag(a=5, b='oopsy', c=false) => assert(false, 'oops')
  \\    case Tag(a=_, b=_, c=_) => assert(true, 'yes!')
  \\  end
  ;
  try doRuntimeTest(src);
}

test "patterns-56.<annotated tags - exhaustiveness>" {
  const src =
  \\ type T = Tag(a: num, b: str, c: bool)
  \\  match Tag(a=5, b='oops', c=false)
  \\    case Tag(a=_, b=_, c=false) => assert(true, 'good')
  \\    case Tag(a=_, b=_, c=true) => assert(false, 'bad')
  \\  end
  ;
  try doRuntimeTest(src);
}

test "binary tree" {
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
  \\ let tree: Tree{num} = Node(
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
  \\ print_tree{num}(tree)
  ;
  try doRuntimeTest(src);
}

test "match expressions .1" {
  const src =
  \\ def think(n: num)
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
  \\   def think(n: num)
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
  \\ def think(n: num)
  \\  if n > 0xff
  \\    return Ok(n * 0xff)
  \\  end
  \\  return Error('bad')
  \\ end
  \\ 
  \\ def fun(t: List{num})
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
  \\  def think(n: num)
  \\   if n > 0xff
  \\     return Ok(n * 0xff)
  \\   end
  \\   return Error('bad')
  \\  end
  \\  
  \\  def fun(t: List{num})
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
  \\ def sum_list(l: List{num})
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
  \\ let add = def (a: num, b: num) => a + b
  \\ def reduce(l: List{num}, func: fn(num, num):num, init: num)
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
  \\ let add = def (a: num, b: num) => a + b
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

test "aspec.<methods 1>" {
  const src =
  \\ class Fish
  \\  pub x: str
  \\  pub y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{num}
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
  \\  pub x: str
  \\  pub y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{num}
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
  \\  x: str
  \\  y: num
  \\  j: List{num}
  \\
  \\  def init()
  \\    self.x = 'a'
  \\    self.y = 6
  \\    self.j = [] as List{num}
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
  \\ alias F{P} = num
  \\ let j = B(5) as T{F{str}}
  \\ match j
  \\  case B(t) => assert(t == 5, 'should be 5')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "unused generic tparams.2" {
  const src =
  \\ type T{K} = B(F{K})
  \\ alias F{P} = num
  \\ let j = B(5) as T{F{str}}
  \\ match j
  \\  case B(t) => assert(t == 5, 'should be 5')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "tags with function types" {
  const src =
  \\ type Fun{T} = OneArg(fn(T):T) | TwoArg(fn(T, T): T)
  \\ let one = def (a: num) => a * a
  \\ let two = def (a: num, b: num) => a * b
  \\ let j: Fun{num} = OneArg(one)
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
  \\ if j as bool
  \\   assert(true, 'should be true')
  \\ else
  \\   assert(false, 'should not be false')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "nullable assertions" {
  const src =
  \\ let x: Tuple{List{num}, str?} = ([5], Just('foo') as Maybe{str})
  \\ if x[0] is List{num} and x[1].?? is str
  \\    x[0][0] += x[1].??.len()
  \\ else
  \\    assert(false, '')
  \\ end
  ;
  try doRuntimeTest(src);
}
