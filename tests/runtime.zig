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
      \\ assert((123.45 > 12_40 or 2) == 2, '123.45 > 12_40 or 2 == 2')
      \\ assert((0b111_000 <= 0o12_12 or 1 > 0.5), '0b111_000 <= 0o12_12 or 1 > 0.5')
      \\ assert(!(123.e-2 >= 0x12_34_5 and 6 and 7 > 2), '123.e-2 >= 0x12_34_5 and 6 and 7 > 2')
      \\ assert(!!(123.e-2 != 0x12_34_5 and 0 or 6 > 2), '123.e-2 != 0x12_34_5 and 0 or 6 > 2')
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

test "lists" {
  const src = 
    \\ [1, 2, 3, 4]
    \\ [1, 'fox', 3, 'cat']
    \\ [1]
    \\ []
    \\ [1, 'fox', 3, 'cat', [1, 'fox', 3, 'cat']]
    \\ [1, 2, {'a': 'set'}]
    \\ [1, 2, {'a': 'set'},]
  ;
  try doRuntimeTest(src);
}

test "maps" {
  const srcs =
    \\ []
    \\ {'abc': 123}
    \\ {'abc' as bool | str: 123, true: 0xff, 'obs': 0b101}
    \\ {}
    \\ {24: [1, 2, 3]}
    \\ {24: [1, 2, 3],}
  ;
  try doRuntimeTest(srcs);
}

test "tuples" {
  const srcs =
    \\ (1,)
    \\ ({'abc' as bool | str: 123}, {true: 0xff}, {'obs': 0b101})
    \\ ()
    \\ ([1, 2, 3], [5, 5, 5])
    \\ ('abc',) as tuple{'abc'}
    \\ ('x', 'y',)
  ;
  try doRuntimeTest(srcs);
}

test "regs" {
  var src = \\ {(1 * 2) * 3 - (3 * 5) : [1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400
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
  var src = 
  \\ let x = 5 * 0xff - 2
  \\ let k = 10
  \\ k = 5
  \\ let p = k
  \\ [p, k]
  \\ let y = (
  \\     x - 5
  \\ )
  \\ let z = [
  \\     x,
  \\ y,
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
  var src2 =
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
  var src = 
  \\ type A{K, V} = (map{K?, map{K, (V | C)? | (C?)}?}?)
  \\ type B{K, V, T} = map{K?, map{K, (V | A? | B | C)? | (A? | B?)}?}? | T
  \\ type C = str
  \\ type Foo = (map{str, bool}? | (list{(A)?})?)?
  //  \\ let x: A{bool, str?} = {false as bool?: {true: 'ok' as str?} as map{bool, str?}}
  \\ let x = {false as bool?: {true: 'ok' as str?} as map{bool, str?}}
  \\ let y = 15 as num
  \\ let j = (15 as num) as num
  \\ let z = {15: ['foxy']} as map{num, list{str}}
  \\ let z1 = {15: ['foxy']} as (map{num, list{str}})
  \\ let z2 = {12: ['foxy']} as (map{num, list{str}})?
  \\ let t = 5 as (num | str)?
  \\ (t as (str | num) ?)
  \\ let t = 5 as num? | str?
  \\ (t as (str? | num?))
  \\ type T{K, V} = K | V
  \\ let p: T{num, str} = 5
  \\ p as num + 3
  \\ let x: num? | str = 5
  \\ let y: num | str? = x
  \\ let z: (num | str)? = x as num + y as num
  \\ let z2: num | str = x as num + y as num
  \\ let q: num | str = z.? or z2
  \\ q == z # okay because types are still related (i.e. not disjoint).
  \\ let x: num | num = 5
  \\ x += x
  \\ let j: tuple{true | false} = (true,)
  \\ let q = (1, 2, 'abc', 0xff, 1, 'foo', 'bar')
  \\ let s = [1, 2, 'ooo']
  \\ s[0] = q[0]
  \\ s[1] = q[1]
  \\ s[2] = q[2]
  \\ (s, q)
  ;
  try doRuntimeTest(src);
}

test "empty generic types" {
  var src = 
  \\ let p: tuple{any} = ()
  \\ let j: list{any} = []
  \\ let q: map{any, any} = {}
  ;
  try doRuntimeTest(src);
}

test "blocks" {
  var src =
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
  var src =
  \\ type HashMap{K, V} = map{K, V}
  \\ type StringHashMap{V} = HashMap{str, V}
  \\ type NumList = list{num}
  \\ let a: NumList = [1, 2]
  \\ let b: HashMap{num, bool} = {0: false}
  \\ let c: StringHashMap{bool} = {'foo': false}
  \\ let x: str = 'over the garden wall!'
  \\ let y = 'oops'
  ;
  try doRuntimeTest(src);
  var src2 =
  \\ type A = str
  \\ type B = num
  \\ type C{A, B} = map{A, B}
  \\ type D{K} = C{K, B}
  \\ type HashMap{K, V} = C{K, V}
  \\ type StringHashMap{V} = HashMap{str, V}
  \\ type BadList = list{StringHashMap{HashMap{A, D{B}?}}}
  \\ type X = (D{BadList}? | D{B}? | BadList?)
  \\ let x: X = [{'fox': {'fin': {0x12: 0xbee} as map{num, num}?}}]
  \\ x
  \\ let y: X = {10: 5} as map{num, num}?
  \\ y
  ;
  try doRuntimeTest(src2);
  var src3 =
  \\ type HashMap{K, V} = map{K, V}
  \\ let x: HashMap{num, str} = {5: 'okay'}
  \\ x as map{num, str} as map{num, str}?
  ;
  try doRuntimeTest(src3);
}

test "recursive types" {
  var src =
  \\ # simple recursive
  \\ type K = str | bool | num
  \\ type V = num | str | map{K, V}
  \\ type V2 = num | str | bool | map{V2, V2}
  \\ let p = [1, [2], 3, [1, 2, [3]]]
  \\ let x = {'abc' as V2: 123 as V2, true: 0xff, 'obs': 'fin', 0.123: {'abc' as V2: 123 as V2, true: 0xff, 'obs': 'fin'}}
  \\ x
  \\ # recursive nullable
  \\ type R = str | num | list{R}
  \\ type S = list{S? | str}
  \\ let x: R = 'fox'
  \\ x = [[[['foo']], 3, [[[4, 5], 'bar']], 'ok'], [[['duh']]], [4]]
  \\ x
  \\ # recursive generics
  \\ type R{K} = str | K | list{R{K}}
  \\ type S{K} = K | str | list{S{K}}
  \\ let x: R{num}? = [5, 'fox', [[[[33]]]]] as R{num}
  \\ let y: S{num} = [1]
  \\ x = y as R{num}
  \\ x
  \\ # no conflicts
  \\ type A{T} = T
  \\ let x: A{A{num}} = 5
  \\
  ;
  try doRuntimeTest(src);
}

test "circularity" {
  var src =
  \\ type T{K} = str | S{str}
  \\ type S{K} = num | T{num}
  \\ let p: T{num} = 5
  \\ print(p)
  \\ p = 'ok'
  \\ print(p)
  \\ type T = T
  \\ type S = S
  \\ type Q = T | S
  \\ let Q: Q? = nil
  \\ Q
  ;
  try doRuntimeTest(src);
}

test "cast-typecheck" {
  var src =
  \\ let x = 5 as bool
  \\ assert(x, 'should be true')
  \\ x and 'fox'
  \\ let p = 5 as (num | str)?
  \\ let q: (num | str | bool) = 5
  \\ q = q as num + 5 # okay, active type is num
  \\ 
  \\ type X = (num | str)?
  \\ let y: X = 'food' as num | str
  \\ y
  \\
  \\ let a: str | num = 10
  \\ let b: num | str = 'foo'
  \\ b = a
  \\ # okay,
  \\ b as num + 5 # okay, since the active type of a is propagated to b.
  \\ let x = [] as list{num}
  \\ x = [1, 2, 3]
  \\ type T = str | num | list{T}
  \\ let p: T = [[1,[5, 'hey'],'2']]
  \\ type X = (num? | str?)?
  \\ type X2 = (num | str)?
  \\ let x: X = 5 as (num? | str?)
  \\ let x: X = 5
  \\ let x: X2 = 5
  \\ type X = (num | str)?
  \\ let y: X = 'food' as str?
  ;
  try doRuntimeTest(src);
}

test "indexing" {
  var src =
  \\ let t = [5, 6, 7, 4]
  \\ let q = t[0] + 5
  \\ t[0] += 10
  \\ let p = {'a': 5}
  \\ p['b'] = p['a']
  \\ let c = p['a'] + 10
  \\ let d = c * 5
  \\ let x: list{num | bool} = [1 as num | bool, 2]
  \\ x[1] = d
  \\ x[
  \\  -1 + 1
  \\ ] = !d
  \\ let x = [1, 2, 3, 4, 5]
  \\ let y = x[t[3]]
  \\ let r = !!x[2]
  \\ x[-1] <<= 3
  \\ let w = x[-2]
  \\ [t, r, q, p, x, y, d, w]
  \\ # multi type index
  \\ let y = {'fox' as str | num: 'fan', 'fun': 'fox', 5: 'fox'}
  \\ let z:  str = 'fox'
  \\ let p = y[z] and y[5] and y[5 as str | num]
  \\ assert(p == 'fox', 'should be fox')
  ;
  try doRuntimeTest(src);
}

test "type summation" {
  var src = 
  \\ let p: list{num?} = []
  \\ let q: list{num?} = [nil as num?]
  \\ let r: list{num?} = [nil, 5, nil]
  \\ let r: list{(num | str)?} = [nil, 5, nil, 'foo']
  \\ let t: list{(num | str)?} = [nil, 5 as num | str, nil]
  ;
  try doRuntimeTest(src);
}

test "nil access" {
  var src =
  \\ let x: num? = 5
  \\ let p = x.? + 10
  \\ let f = {'foo': 5 as num?}
  \\ let j = f['foo'].? + 5
  \\ assert(j == 10, 'should be 10')
  ;
  try doRuntimeTest(src);
}

test "if statement" {
  // if-elif-else
  var src =
  \\ let x: num = 5
  \\ let t: num? = 15
  \\ let p = 0
  \\ if x == t.? / 3 -1
  \\   p = x + 10
  \\ elif t == nil
  \\   p = 29
  \\ elif t > 10
  \\   p = 12
  \\   p = p << 12 - p >> 3
  \\ else
  \\   p = x - 10
  \\   p -= -1111
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
  var src2 =
  \\ let x: num = 5
  \\ let t: num? = 15
  \\ let p = 0
  \\ if x == t.? / 3
  \\   p = x + 10
  \\   p *= 3
  \\ else
  \\   p = x - 10
  \\   p -= -1111
  \\ end
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
  var src3 =
  \\ do
  \\    let x: num = 5
  \\    let t: num? = 15
  \\    let p = 0
  \\    if x == t.? / 3
  \\      p = x + 10
  \\      p *= 3
  \\      p = x - 10
  \\      p -= -1111
  \\      p += 0b1111_1111_1111
  \\    end
  \\    p == 0b1010001010001
  \\ end
  ;
  try doRuntimeTest(src3);

  // if-end
  var src4 =
  \\ let x: num = 5
  \\ let t: num? = 15
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
  var src5 =
  \\ let x: num = 5
  \\ let t: num? = 15
  \\ let p = 0
  \\ if 0xf == x - t.?
  \\   p = x + 10
  \\ else
  \\   p = x - 10
  \\   p -= -1111
  \\ end
  \\ p == 0o2122 and p == 0x452
  \\ let p: str | num = 5
  \\ let q = "fox"
  \\ let w = 0
  \\ if p == q
  \\   w = p as num
  \\ else 
  \\   w = 123
  \\ end
  \\ w == 123
  \\ if q == 'fox'
  \\   w /= 2
  \\ end
  \\ assert(w == 61.5, 'should be 61.5')
  ;
  try doRuntimeTest(src5);
}

test "type expressions" {
  var src =
  \\ let p = num
  \\ p = str
  \\ let q = list{num}
  \\ q = p
  \\ let x = map{str, num}
  \\ x = p
  \\ let w = [num, str, bool, list{str}]
  \\ w = [num, bool]
  \\ let q = {'bar': list{map{str, num}}}
  \\ q = {'fox': num, 'foo': bool, 'bar': list{map{str, num}}}
  \\ let t: num = 5
  \\ if num == num
  \\   t += 5
  \\ else
  \\  t -= 3
  \\ end
  \\ t
  ;
  try doRuntimeTest(src);
}

test "is expression" {
  var src =
  \\ # is
  \\ # direct checks
  \\ 'fox' is str
  \\ 5 is num
  \\ nil is nil
  \\ [] is list{any}
  \\ {} is map{any, any}
  \\ true is bool
  \\ nil is nil
  \\ nil as list{num}? is nil
  \\ let p: str? = nil
  \\ p is nil
  \\ !([5] as list{num}? is nil)
  \\
  \\ # indirect checks
  \\ let n: str | num | list{num} | map{str, num} = {}
  \\ assert(n is list{num} == false, 'n should be map')
  \\ assert(n is map{str, num} == true, 'n should be map')
  \\ n = 'foo'
  \\ assert(n is str == true, 'n should be str')
  \\ assert(n is num == false, 'n should be not num')
  \\ n = 5
  \\ assert(n is num == true, 'n should be num')
  \\ assert(!!n is bool == true, 'should be boolean')
  \\ assert(bool == bool is bool == true, 'true')
  \\ assert(((bool == bool) is bool) == true, 'true')  # same as above
  \\
  \\ # is not
  \\ # direct checks
  \\ 'fox' is not str
  \\ 5 is not num
  \\ nil is not nil
  \\ [] is not list{any}
  \\ {} is not map{any, any}
  \\ true is not bool
  \\ nil is not nil
  \\ nil as list{num}? is not nil
  \\ let p: str? = nil
  \\ p is not nil
  \\ !([5] as list{num}? is not nil)
  \\
  \\ # indirect checks
  \\ let n: str | num | list{num} | map{str, num} = {}
  \\ assert(n is not list{num} != false, 'not list')
  \\ assert(n is not map{str, num} != true, 'not map')
  \\ n = 'foo'
  \\ assert(n is not str != true, 'not str')
  \\ assert(n is not num != false, 'not num')
  \\ n = 5
  \\ assert(n is not num == false, 'not num?')
  \\ !!n is not bool == true
  \\ bool == bool is not bool == true
  \\ assert(!({} is not map{str, num}), 'a map')
  \\ assert(((bool == bool) is not bool) != true, 'not true')  # same as above
  \\ assert(num != str, 'num is not str')
  ;
  try doRuntimeTest(src);
}

test "narrowing-1" {
  var src =
  \\ let x: (str | num)? = 'foobar'
  \\ let p = 10
  \\ if x is not nil and !!!(x is str) and x > p
  \\   x += p
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-2" {
  var src =
  \\ let x: list{num} | map{str, num} = [5]
  \\ let p = 10
  \\ if x is list{num}
  \\   p += x[0]
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "narrowing-3" {
  var src =
  \\ let x: list{num} | map{str, num} = [5]
  \\ let p = 10
  \\ if x is list{num} and x[0] == 5
  \\   x[0] ^= 12
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
}

test "narrowing-4" {
  var src =
  \\ let x: list{list{num | str}} | map{str, num} = [[5 as num | str]]
  \\ let p = 10
  \\ if x is list{list{num | str}} and x[0][0] is num and x[0][0] + 2 > 0
  \\   x[0]
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "narrowing-5" {
  var src =
  \\ let x: list{list{num | str}} | map{str, list{num | str}} = [[5 as num | str]]
  \\ let p = 10
  \\ if x is map{str, list{num | str}} and x['a'][0] is num and x['a'][0] + 2 > 0
  \\   x['foobar'] = [1 as num | str, 2]
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
}

test "narrowing-6" {
  var src =
  \\ let x: list{list{num | str}} | map{str, list{num | str}} = [[5 as num | str]]
  \\ let p = 10
  \\ if x is map{str, list{num | str}} and x['a'][0] is num and x['a'][0] + 2 > 0xff
  \\   x['a'][0] + 5
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "narrowing-7" {
  var src =
  \\ let x: list{list{num | str}} | map{str, list{num | str}} = [[5 as num | str]]
  \\ let p = 10
  \\ if x is map{str, list{num | str}} and x['a'][0] is num and x['a'][0] + 2 > 0
  \\   p += x['a'][0]
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "narrowing-8" {
  var src =
  \\ let x: list{list{num | str}} | map{str, list{num | str}} = [[5 as num | str]]
  \\ let y: str | bool | num = false
  \\ let p = 10
  \\ if y is not str and y is not num
  \\   !y
  \\ elif y is not str
  \\  y + p # num
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "narrowing-9" {
  var src =
  \\ let x: num? = 9
  \\ if x is not nil and x is num
  \\   x + 5
  \\ else 
  \\   !!x
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-10" {
  var src =
  \\ let x: str | num = 5
  \\ let p = 0
  \\ if x is num
  \\    p /= 5
  \\ elif x is str
  \\    !x
  \\ else
  \\    x # never
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
}

test "narrowing-11" {
  var src =
  \\ let x: list{num | list{num}} | num = [9 as num | list{num}]
  \\ let p = 10
  \\ if x is list{num | list{num}}
  \\    if x[0] is list{num}
  \\        p /= 5
  \\    else
  \\        p *= x[0]
  \\    end
  \\ elif x is num
  \\    p += x
  \\ else
  \\    x  # never
  \\ end
  \\ assert(p == 90, 'p should be 90')
  ;
  try doRuntimeTest(src);
}

test "narrowing-12" {
  var src =
  \\ let x: str | num = 5
  \\ let p = 0
  \\ if x is num
  \\    p /= 5
  \\ end
  \\ if x is str
  \\    !x
  \\ else
  \\    p = x * x
  \\ end
  \\ assert(p == 25, 'p should be 25')
  ;
  try doRuntimeTest(src);
}

test "narrowing-13" {
  var src =
  \\ let x: str | num = 5
  \\ let p = 0
  \\ if x is num
  \\    p /= 5
  \\ end
  \\ if x is str
  \\    !x
  \\ elif x is num
  \\    p *= x
  \\ else
  \\    x # never
  \\ end
  \\ assert(p == 0, 'p should be 0')
  ;
  try doRuntimeTest(src);
}


test "narrowing-14" {
  var src =
  \\ let x: (list{num} | str)? = [5]
  \\ if x.? is list{num} and x.?[0] is num
  \\    x.?[0] += 5
  \\ else
  \\    x.?
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
}

test "narrowing-15" {
  var src =
  \\ let x: num? = 9
  \\ if x is not nil and x is num
  \\   x += 5
  \\ else 
  \\   !!x
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
}

test "narrowing-16" {
  var src =
  \\ let x: str | num = 5
  \\ if (x is num and 5 > 2) or x is num
  \\    x += 5
  \\ else
  \\    x # str
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
}

test "narrowing-17" {
  var src =
  \\ let x: num? = 9
  \\ if x is not nil and x is num
  \\   x -= 5
  \\ else 
  \\   !!x
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
}

test "narrowing-18" {
  var src =
  \\ let x: str | num = 5
  \\ let y: str | num = 10
  \\ if x is num and y is num or y is num
  \\    x # num | str
  \\    y *= 5
  \\ else
  \\    x # num | str
  \\    y # str
  \\ end
  \\ y
  ;
  try doRuntimeTest(src);
}

test "narrowing-19" {
  var src =
  \\ let x: list{list{num | str}} | map{str, list{num | str}} = [[5 as num | str]]
  \\ let p = 10
  \\ if x is map {str, list{num | str}}
  \\   if x['a'][0] is num and x['a'][0] + 2 > 0xff
  \\      x['a'][0] + 5
  \\   else
  \\     x # list{...}
  \\   end
  \\ else
  \\   if x[0][0] is num
  \\      p -= x[0][0]
  \\   end
  \\ end
  \\ p
  ;
  try doRuntimeTest(src);
  var src2 =
  \\ let x: (num | str?) = 'fox'
  \\ if x != nil and x is not str
  \\    x += 10
  \\ end
  \\ x
  ;
  try doRuntimeTest(src2);
}

test "narrowing-20" {
  var src =
  \\ let x: str | num? = 5
  \\ if x is not str
  \\    x.? += 10
  \\ end
  \\ x
  ;
  try doRuntimeTest(src);
  var src2 =
  \\ let x: str | num? = 5
  \\ if x is not str and x is not nil
  \\    x += 10
  \\ end
  \\ x
  ;
  try doRuntimeTest(src2);
  var src3 =
  \\ do
  \\   let x: list{num?} = [5 as num?]
  \\   let p = x as list{num?}
  \\   if p[0].? is num # redundant but okay
  \\      p[0].? += 12
  \\   end
  \\   x
  \\ end
  \\ ()
  \\ let t: num = 0
  \\ let v: map{num, num} | tuple{num | str} = (15 as str | num,)
  \\ if v is tuple {num | str}
  \\    let p = v[0]
  \\    if p is not str
  \\      t += p + 1
  \\    elif p is str
  \\      p
  \\    end
  \\ end
  \\ assert(t == 16, 'should be 16')
  ;
  try doRuntimeTest(src3);
}

test "narrowing-21" {
  var src = 
  \\ def fun(n: num | str)
  \\  if n is str
  \\    return n
  \\  end
  \\  if n is num
  \\    return n + 5
  \\  end
  \\  n # never
  \\ end
  \\ fun('fancy')
  \\
  \\ def fun(n: num | str)
  \\  if n is str
  \\    return n
  \\  end
  \\  return n + 5
  \\ end
  \\ fun(12)
  ;
  try doRuntimeTest(src);
}

test "narrowing-22" {
  var src =
  \\ def fish(p: "a" | "b" | 5)
  \\  if p == "a"
  \\    return 'nice'
  \\  elif p == "b"
  \\    return 'good'
  \\  elif p == 5
  \\    return 'okay'
  \\  else
  \\    # type of p here is 'never'
  \\    # TODO: we shouldn't have to return anything here
  \\    #       for this func to be type str
  \\    return 'hmm'
  \\  end
  \\ end
  \\ 
  \\ assert(fish(5) == 'okay', 'ok')
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
  \\ 
  \\ assert(fun() == 8, 'should be 8')
  ;
  try doRuntimeTest(src);
}

test "narrowing-23" {
  var src =
  \\ class Fox
  \\    x: num | str = 5
  \\    u = 12
  \\ end
  \\
  \\ let f = [Fox(), 5]
  \\ let t = 0
  \\ let x = 0
  \\ if f[0] is Fox
  \\   if f[0].x is num
  \\     f[0].x += f[0].u
  \\     t = f[0].x
  \\     x = f[0].u
  \\   end
  \\ else
  \\   t += f[0]
  \\ end
  \\ assert(t == 17, 't should be 17')
  \\ assert(x == 12, 'x should be 12')
  ;
  try doRuntimeTest(src);
}

test "narrowing-24" {
  var src =
  \\ class Fox
  \\    x: num | str = 5
  \\    u = 12
  \\ end
  \\ class Foo
  \\    x = 'ok'
  \\    u = 13
  \\ end
  \\
  \\ let f: Fox | Foo = Fox()
  \\ if f is Foo
  \\  assert(f.u == 13, 'this should be Foo.u')
  \\ elif f is Fox
  \\  assert(f.u == 12, 'this should be Fox.u')
  \\ else
  \\  print(f) # never
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-25" {
  var src =
  \\ let j = 5
  \\ if j is num and j > 5
  \\  j
  \\ else
  \\  assert(j + 4 == 9, 'should be 9')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "narrowing-26" {
  var src =
  \\ let x: num | str? = 4
  \\ if x.? is num
  \\    x.? += 5
  \\ else
  \\    x.?
  \\ end
  \\ assert(x == 9 as num | str, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "narrowing-27" {
  var src =
  \\ let j: list{list{num}} | list{str} = [[5]]
  \\ if j is list{list{num}} and j.len() == 1
  \\  assert(j[0] is list{num}, 'is list')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "void narrowing" {
  var src =
  \\ def fox(x: bool)
  \\  if x then
  \\    return 3
  \\  end
  \\ end
  \\
  \\ let t = fox(false)
  \\ if t is not void then
  \\   t += 5
  \\ end
  \\ [t]
  \\ t = fox(!!1)
  \\ if t is not void then
  \\   t += 12
  \\ end
  \\ assert(t == 15, 't should be 15')
  ;
  try doRuntimeTest(src);
}

test "constant types" {
  var src =
  \\ type Animal = "cat" | "dog" | "fox"
  \\ type Cat = 'cat'
  \\ type Even = 2 | 4 | 6 | 8.6
  \\ type boolean = true | false
  \\ let p: Animal = 'fox'
  \\ let x: Even = 6
  \\ let y: boolean = true
  \\ let t: bool = y
  \\ x = 8.6
  \\ p = 'dog'
  \\ y = false
  \\ [x, p, y, t]
  \\ do
  \\    let p = 'Cat'
  \\    let j: 'Cat' | 'Dog' = p as ('Cat' | 'Dog')
  \\    let x = 0xff
  \\    let q: 0xff = x
  \\ end
  \\ # true | false unions are auto-converted to 'bool'
  \\ let x: bool = false
  \\ let q: true | false = x
  \\ '' as (true | false) == '' as bool
  \\ list{num?} as true | false == list{num?} as bool
  \\ let p: 6 | 5? = 5
  \\ let k: num? = nil
  \\ if p is not nil
  \\    k = p as num + 25
  \\ end
  \\ k == 30
  \\ type A = 5
  \\ let x: list{A} = [5, 5, 5]
  \\ let y: list{'foo'} = ['foo', 'foo', 'foo']
  \\ let z: map{'name', str} = {'name': 'ziord'}
  \\ z['name']
  \\ type A{k} = k | list{k}
  \\ let j: A{5} = [5, 5]
  \\ j = 5
  \\ j as 5 as num + 5
  ;
  try doRuntimeTest(src);
}

test "while loop" {
  var src =
  \\ do
  \\    let x: num | str = 5
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
  \\ let x: num | str = 5
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
  var src2 =
  \\ let x: num | str = 5
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
  var src3 =
  \\ let x: num | str = 5
  \\ while x is num and x < 25 do
  \\  if x % 5 == 0 then
  \\    break
  \\  end
  \\  x += 5
  \\ end
  \\ x
  \\ let x: num | str = 5
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
  var src =
  \\ type T = num
  \\ def j(a: T): T
  \\  return (a * 2)
  \\ end
  \\ assert(j(5) + 9 == 19, 'should be 19')
  ;
  try doRuntimeTest(src);
  var src2 =
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
  var src3 =
  \\ def fib(n: num): num
  \\  if n <= 1 then
  \\    return n
  \\  end
  \\  return fib(n - 1) + fib(n - 2)
  \\ end
  \\ assert(fib(13) == 233, 'should be 233')
  ;
  try doRuntimeTest(src3);
  var src4 =
  \\ type T = num
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
  var src = 
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
  var src2 =
  \\ def fancy{T}(x: T)
  \\  let j: T = x
  \\  return j
  \\ end
  \\ def id{T}(val: T): T
  \\  return val
  \\ end
  \\ [fancy(5), fancy('oops'), fancy(true), id([1, 2, {'a': 'fox'}])]
  ;
  try doRuntimeTest(src2);
  var src3 =
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
  var src =
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
  var src =
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
  var src =
  \\ let j = [89, def(x: num) => x * 2, def(y: num) => y + 5]
  \\ if j[1] is not num
  \\  if j[0] is num
  \\    j[0] += j[1](16)
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
  var src =
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
  var src =
  \\ do
  \\  do
  \\    def fun
  \\     def read{T}(x: T): list{T}
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
  \\  def read{T}(x: T): list{T}
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

test "functions-8" {
    var src =
  \\ do
  \\  do
  \\    def fun
  \\     def read{T}(x: T): list{T}
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
  \\  def read{T}(x: T): list{T}
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
  \\ let j = [89, def(x: num) => x * 2, def(y: num) => y + 5]
  \\ if j[1] is not num
  \\  if j[0] is num
  \\    j[0] += j[1](16)
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
  \\ [fancy(5), fancy('oops'), fancy(true), id([1, 2, {'a': 'fox'}])]
  ;
  try doRuntimeTest(src);
}

test "functions-9" {
  var src =
  \\ def ret3(n: num): num | str | bool 
  \\  if n < 5
  \\    return 3
  \\  end
  \\  if n < 12
  \\    return 'hey'
  \\  end
  \\  if n > 15
  \\    return true
  \\  end
  \\  return 'oops'
  \\ end
  \\
  \\ def ret(n: num | str | bool): num | str | bool
  \\  if n is str
  \\    return n
  \\  end
  \\  if n is bool
  \\    return n
  \\  end
  \\  return n + 12
  \\ end
  \\ 
  \\ (def (x: num) => x * x)(12)
  \\ (def (x: num)
  \\  return x * x
  \\ end)(12)
  \\
  \\ ret3(7)
  \\ let t = ret(7)
  \\ if t is num
  \\  assert(t == 19, 'should be 19')
  \\ else
  \\  assert(false, 'yeah')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "functions-10" {
  var src = 
  \\ def fox{T}(x: T): fn(T): num | str
  \\  def fun(p: T): num | str
  \\    return p * x
  \\  end
  \\  return fun
  \\ end
  \\ let j = fox(5)
  \\ assert(j(3) == 15, 'should be 15')
  \\ assert(fox(5)(3) == 15, 'should be 15')
  ;
  try doRuntimeTest(src);
  var src2 =
   \\ def fox(x: num): fn(num): num | str
  \\  return def (p: num): num | str  => p * x
  \\ end
  \\
  \\ let j = fox(5)
  \\ assert(j(4) == 20, 'should be 20')
  ;
  try doRuntimeTest(src2);
  var src3 =
  \\ def big: num | str
  \\  return 5
  \\ end
  \\ let j = big()
  \\ if j is num 
  \\  j += 5
  \\ end
  \\ assert(j == 10, 'should be 10')
  ;
  try doRuntimeTest(src3);
  var src4 =
  \\ type T = num
  \\ type Fun = fn(T): T | str
  \\
  \\ def fox(x: T): Fun
  \\  def fun(p: T): T | str
  \\    return p * x
  \\  end
  \\  return fun
  \\ end
  \\
  \\ assert(fox(2)(3) == 6, 'should be 6')
  ;
  try doRuntimeTest(src4);
}

test "functions-11" {
  var src =
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
  \\ let j = [def (x: num) => x * x, def (y: num) => !y]
  \\ let v = [def (x: num) => x * x, def (y: num) => ~y][t - 1](t + 7)
  \\ assert(v == -8, 'should be -8')
  ;
  try doRuntimeTest(src);
}

test "functions-13" {
  var src =
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
  var src =
  \\ def fun: void | noreturn
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
  var src =
  \\ def fun
  \\ end
  \\ [fun()]
  ;
  try doRuntimeTest(src);
}

test "functions-16-varargs" {
  var src =
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
  \\  return [a * rest[0], rest]
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
  \\ if res[1] is tuple{num}
  \\  assert(res[1][0]==2, 'should be 2')
  \\  assert(res[1][4]==6, 'should be 6')
  \\ else
  \\  assert(false, 'oops')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "functions-17" {
  var src =
  \\ do
  \\  def foo{T}(x*: T)
  \\   print(x)
  \\   return x
  \\  end
  \\
  \\  let j: any | num | str = ['fox']
  \\  foo{num}(6, 7)
  \\  do
  \\   print(1, 2, 3, 4)
  \\  end
  \\ end
  \\ print(print(), 'jeryr')
  \\ (def (p*:any) => print(p))(1, 2, 'a', 'b', false, true, nil)
  \\ let j: any? = nil
  \\ if j is num
  \\ end
  ;
  try doRuntimeTest(src);
}

test "functions-18" {
  var src =
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
  var src =
  \\ type T = num
  \\ def id{T}(v: T): (fn (T): tuple{T})
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
  var src =
  \\ assert((def (x: list{num}) 
  \\   return x
  \\ end)([5]) != [5], 'objects are not strictly equal')
  ;
  try doRuntimeTest(src);
}

test "functions-21" {
  var src =
  \\ def fun(a*: any)
  \\  print('a is', a)
  \\  assert(!a.len(), 'should be 0')
  \\ end
  \\
  \\ fun()
  ;
  try doRuntimeTest(src);
}

test "functions-22.<narrowing with do-blocks>" {
  var src =
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
  var src =
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
  var src =
  \\ def funny(t: list{str})
  \\  print(t)
  \\ end
  \\ funny([] as list{str})
  ;
  try doRuntimeTest(src);
}

test "functions-25.<void>" {
  var src =
  \\ def foo()
  \\  print('yay')
  \\ end
  \\ let j:void = foo()
  \\ print('j is', j)
  ;
  try doRuntimeTest(src);
}

test "builtin-functions" {
  var src =
  \\ assert(true, 'ok')
  \\ assert(!!exit, 'exit')
  \\ assert(!!assert, 'assert')
  \\ # assert(!!panic, 'panic')
  \\ assert(!!print, 'print')
  \\ [exit, assert]
  ;
  try doRuntimeTest(src);
}

test "builtin-functions-override" {
  var src =
  \\ def panic{T}(x: T)
  \\  return [x]
  \\ end
  \\ assert(panic('nice')[0] == 'nice', 'okay')
  \\
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

test "errors-1" {
  var src =
  \\ do
  \\  def fun(x: num)
  \\   if x > 2
  \\     return
  \\   end
  \\   return ('foo')!
  \\  end
  \\  let j = fun(1) orelse |e| do
  \\   [e]
  \\   assert(e.value() == 'foo', 'value should be foo')
  \\  end
  \\ end
  \\
  \\ def fun(x: num)
  \\  if x > 2
  \\    return
  \\  end
  \\  return ('foo')!
  \\ end
  \\ let j = fun(1) orelse |e| do
  \\  [e]
  \\  assert(e.value() == 'foo', 'value should be foo')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "errors-2" {
  var src =
  \\ def fun(x: num)
  \\  if x > 5
  \\    return x
  \\  else
  \\    return ('bad')!
  \\  end
  \\ end
  \\
  \\ def test()
  \\  let k = fun(12) orelse |e| 15
  \\  return k
  \\ end
  \\ let p = test()
  \\ assert(p == 12, 'p should be 12')
  \\ p += 2
  ;
  try doRuntimeTest(src);
}

test "errors-3" {
  var src =
  \\ def fun(x: num)
  \\  if x > 5
  \\    return
  \\  else
  \\    return ('bad')!
  \\  end
  \\ end
  \\ fun(2) orelse |e| do
  \\  [e, 'oops']
  \\  assert(e.value() == 'bad', 'err should be bad')
  \\  def hehe(p: err{str})
  \\    return [p]
  \\  end
  \\  assert(hehe(e).get(0).?.value() == 'bad', 'should be bad')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "errors-4" {
  var src =
  \\ do
  \\  def fun(x: num)
  \\   if x > 5
  \\     return 12
  \\   else
  \\     return ('bad')!
  \\   end
  \\  end
  \\  
  \\  def fancy()
  \\   let j = fun(2) orelse 4
  \\   return j + 5
  \\  end
  \\  let k = fancy()
  \\  if k is num
  \\   k += 10
  \\  end
  \\  assert(k == 19, 'k should be 19')
  \\ end
  ;
  try doRuntimeTest(src);
  var src2 =
  \\ def fun(x: num)
  \\  if x > 5
  \\    return 12
  \\  else
  \\    return ('bad')!
  \\  end
  \\ end
  \\
  \\ def fancy()
  \\  let j = fun(2) orelse 4
  \\  return j + 5
  \\ end
  \\ let k = fancy()
  \\ if k is num
  \\  k += 10
  \\ end
  \\ assert(k == 19, 'k should be 19')
  ;
  try doRuntimeTest(src2);
}

test "errors-5" {
  var src =
  \\ do
  \\  def fun(x: num)
  \\   if x > 5
  \\     return x | 3
  \\   else
  \\     return ('bad')!
  \\   end
  \\  end
  \\  
  \\  def fancy{T}(x: T)
  \\   let j = try fun(x)
  \\   return j + 5
  \\  end
  \\  let k = fancy(12) orelse 0
  \\  k += 5
  \\  assert(k == 25, 'k should be 25')
  \\ end
  \\
  \\ def fun(x: num)
  \\  if x > 5
  \\    return x | 3
  \\  else
  \\    return ('bad')!
  \\  end
  \\ end
  \\
  \\ def fancy{T}(x: T)
  \\  let j = try fun(x)
  \\  return j + 5
  \\ end
  \\ let k = fancy(12) orelse 0
  \\ k += 5
  \\ assert(k == 25, 'k should be 25')
  ;
  try doRuntimeTest(src);
}

test "errors-6" {
  var src =
  \\ def fancy(x: num)
  \\  if x > 5
  \\    return ('bad')!
  \\  else 
  \\    return x * 4
  \\  end
  \\ end
  \\ let e = 3
  \\ let j = fancy(22) orelse |e| 3
  \\ j += 5
  \\ # print(e, j)
  \\ assert(e == 3 and j == 8, 'e should not change')
  ;
  try doRuntimeTest(src);
}

test "errors-7" {
  var src =
  \\ let j = ('bad')!
  \\ assert(j.value() == 'bad', 'value should be bad')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-1" {
  var src =
  \\ class Fox
  \\    x: num
  \\    u = 12
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    def pulse()
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
  var src =
  \\ class Fox
  \\    x: num
  \\    u = 12
  \\    def init(): void
  \\      self.x = 0
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\ end
  \\ class Racoon
  \\    x: num
  \\    u = 12
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
  var src =
  \\ class Foo
  \\  x: num = 10
  \\ end
  \\
  \\ let j = Foo()
  \\ assert(j.x == 10, 'j.x should be 10')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-4" {
  var src =
  \\ class Fox
  \\    x: num | str = 5
  \\    u = 12
  \\ end
  \\
  \\ let f = [Fox(), 5]
  \\ let t = 0
  \\ let x = 0
  \\ if f[0] is Fox
  \\  if f[0].x is num
  \\    f[0].x += f[0].u
  \\    t = f[0].x
  \\    x = f[0].u
  \\  end
  \\  assert(t == 17, 't should be 17')
  \\  assert(x == 12, 'x should be 12')
  \\ end
  ;
  try doRuntimeTest(src);
}

test "simple-classes-5" {
  var src =
  \\ class Fox
  \\    x: num | str = 5
  \\    u = 12
  \\    def foo()
  \\      return self.u * 3
  \\    end
  \\ end
  \\ let k = Fox()
  \\ let t = k.foo
  \\ assert(t() == 36, 'should be 36')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-6" {
  var src =
  \\ class Fox
  \\    x: num | str = 5
  \\    u = 12
  \\    def foo()
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
  var src =
  \\ let j = 'foo'
  \\ print('len is', j.len())
  \\ assert(j.len() == 3, 'len should be 3')
  \\
  \\ def fun{T}(x: list{T})
  \\  x.append('oopsy'.len())
  \\  return x
  \\ end
  \\ let _ = fun{num}([1, 2, 3, 4])
  \\ print('-->', _, 'oopsy'.len())
  \\ assert('oopsy'.len() == 5, 'should be 5')
  \\ assert(_[-1] == 5, 'last item should be 5')
  \\
  \\ let x = []
  \\ x.len
  \\ x.append(8)
  \\ print(x)
  \\ assert(x.len() == 1, 'len should be 1')
  ;
  try doRuntimeTest(src);
}

test "functions-1.<narrowing-return>" {
  var src =
  \\ def chee(n: num | str): num
  \\  if n is num
  \\    return n
  \\  end
  \\  return n.len()
  \\ end
  \\ assert(chee('oops') == 4, 'should be 4')
  \\ assert(chee(4) == 4, 'should be 4')
  ;
  try doRuntimeTest(src);
}

test "simple-classes-1.<call & dot access mutation>" {
  var src =
  \\ class Foxy
  \\  x = [1]
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
  var src =
  \\ class Dod
  \\ end
  \\ let j = [Dod()]
  \\ let k: list{Dod} = j
  \\ assert(k.len() == j.len(), 'should be same')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-1" {
  var src =
  \\ class Fox{T}
  \\    x: tuple{T}
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\
  \\    def getGen()
  \\      type T = tuple{str}
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
  \\ # print(t.x)
  \\ assert(t.x[0] == 6, 'first arg is 6')
  \\ assert(t.x[1] == 7, 'second arg is 7')
  \\ assert(t.x[2] == 8, 'third arg is 8')
  \\ let z = Fox{'mia'}('mia')
  \\ assert(z.x[0] == 'mia', 'index 0 gives mia')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-2" {
  var src =
  \\ let j = [1, 2, 3]
  \\ let p = (j.pop() orelse 0) + 4
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

// FIXME: This crashes during testing (`zig test tests/test.zig`)
//        for some reason, but runs fine with ./test.sh and in main (`zig build run`)
// test "generic-classes-3" {
//   var src =
//   \\ let j = []
//   \\ let _ = j.append
//   \\ j.append(5)
//   \\ _(1)
//   \\ assert(j.len() == 2, 'len should be 2')
//   \\ let x = j.pop()
//   \\ assert(x == 1, 'x should be 1')
//   \\ assert(j.pop() == 5, 'should be 5')
//   \\ assert(j.len() == 0, 'len should be 0 now')
//   \\ let t = {'a': 1, 'b': 5, 'c': 12}
//   \\ print(t, t.get('d'))
//   \\ print(t.keys())
//   \\ print(t.values())
//   \\ print(t.items())
//   \\ let g = (t)!
//   \\ print(g, g.value())
//   \\ let _ = g.value
//   \\ print(_, _().len())
//   \\ t.set('a', 0xff)
//   \\ print(t)
//   \\ let foo = t.get('p')
//   \\ assert(foo is nil, 'foo must be nil')
//   ;
//   try doRuntimeTest(src);
// }

test "generic-classes-4" {
  var src =
  \\ class Fox{T}
  \\    x: tuple{T}
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\
  \\    def getGen()
  \\      type T = tuple{str}
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
  \\ type Poo{T} = Fox{T}
  \\ let w = Fox{'mia'}('mia', 'mia', 'mia')
  \\ let j:Poo{'mia'} = Fox{'mia'}('mia', 'mia')
  \\ assert(j.x.len() == 2, 'len should be 2')
  \\ j = w
  \\ assert(j.x.len() == 3, 'len should be 3')
  ;
  try doRuntimeTest(src);
}

test "generic-classes-5" {
  var src =
  \\ class Fox{T}
  \\    x: tuple{T}
  \\    def init(x*: T): void
  \\      self.x = x
  \\    end
  \\    def pulse()
  \\      return self
  \\    end
  \\
  \\    def getGen()
  \\      type T = tuple{str}
  \\      def fun(p: T)
  \\        return p[0]
  \\      end
  \\      return fun
  \\    end
  \\ end
  \\ let x = Fox{num}(6, 7, 8)
  \\ let y = Fox{str}("a", "b")
  \\ let j: Fox{num} | Fox{str} = y
  \\
  \\ if j is Fox{num}
  \\  j.x = (j.x[0] + 5, j.x[1])
  \\  assert(j.x[0] == 11, 'should be 11')
  \\ end
  \\ if j is Fox{str}
  \\  j.x = ("hello world", j.x[1])
  \\ end
  \\ assert(j is Fox{str}, 'is Fox{str}')
  \\ assert((j as Fox{str}).x[0] == 'hello world', 'should be "hello world"')
  \\ j = x
  \\ assert(j is Fox{num}, 'is Fox{num}')
  \\ assert((j as Fox{num}).x[0] == 6, 'should be 6')
  ;
  try doRuntimeTest(src);
}

test "labeled-argument" {
  var src =
  \\ def fun(x: str, y: num, a: list{num}, b: err{str})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  print('x is', x, 'y is', y, 'a is', a, 'b is', b)
  \\ end
  \\ fun(y: 5, a: [2, 3], x: 'oo', ('oops')!)
  \\ fun(y: 5, a: [2, 3], b: ('oops')!, x: 'oo')
  \\ 
  \\ def fun(x: str, y: num, a*: list{num})
  \\  assert(x == 'oo', 'x should not be changed')
  \\  print('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(y: 5, a: [2, 3], x: 'oo', a: [1, 2], a: [5, 6, 7])
  \\ fun(y: 5, a: [2, 3], x: 'oo', a: [1, 2], a: [5, 6, 7])
  \\ fun(y: 5, a: [2, 3], x: 'oo', [1, 2], [5, 6, 7])
  \\
  \\ def fun(x: str, y: num, a*: list{num})
  \\  assert(y == 5, 'y should not change')
  \\  print('x is', x, 'y is', y, 'a is', a)
  \\ end
  \\ fun(a: [0x1, 0x2], y: 5, 'a', a: [2, 3])
  ;
  try doRuntimeTest(src);
}

test "labeled-argument-2" {
  var src =
  \\ class Fun
  \\  a: num
  \\  b: str
  \\  def init(a: num, b: str)
  \\    self.a = a
  \\    self.b = b
  \\  end
  \\  def send(data: list{any})
  \\    let i = 0
  \\    while i < data.len()
  \\      print('sending...', data[i])
  \\      i += 1
  \\    end
  \\  end
  \\ end
  \\ let f = Fun(b: 'oops', a: 12)
  \\ print(f.a, f.b)
  \\ f.send(data: ['a', 1, f])
  ;
  try doRuntimeTest(src);
}

test "builtin-list" {
  var src =
  \\ let j = [1, 2, 3, 4, 5]
  \\ # append, len
  \\ assert(j.len() == 5, 'should be 5')
  \\ j.append(6)
  \\ assert(j.len() == 6, 'should be 6')
  \\ # pop
  \\ let t = j.pop()
  \\ assert(t == 6, 'should be 6')
  \\ # get
  \\ assert(j.get(-1).? == 5, 'should be 5')
  \\ assert(j.get(1).? == 2, 'should be 2')
  \\ assert(j.get(5) == nil, 'should be nil')
  ;
  try doRuntimeTest(src);
}

test "builtin-tuple" {
  var src =
  \\ let j = (1, 2, 3, 4, 5)
  \\ # len
  \\ assert(j.len() == 5, 'should be 5')
  \\ # get
  \\ assert(j.get(-1).? == 5, 'should be 5')
  \\ assert(j.get(1).? == 2, 'should be 2')
  \\ assert(j.get(5) == nil, 'should be nil')
  ;
  try doRuntimeTest(src);
}

test "builtin-map" {
  var src =
  \\ let j = {'a': 1, 'b': 2, 'c': 3, 'd': 4}
  \\ # len
  \\ assert(j.len() == 4, 'should be 4')
  \\ # get
  \\ assert(j.get('a') == 1, 'should be 1')
  \\ assert(j.get('f') == nil, 'should be nil')
  \\ # set
  \\ assert(!j.set('a', 12), 'should be true')
  \\ assert(j.set('f', 7), 'should be true')
  \\ assert(j.len() == 5, 'should be 5')
  \\ assert(j.get('f').? == 7, 'should be 7')
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
  var src =
  \\ let j = ((1, 2, 3, 4, 5))!
  \\ # value
  \\ assert(j.value().len() == 5, 'should be 5')
  \\ let j = ("fox")!
  \\ assert(j.value() == "fox", 'should be fox')
  ;
  try doRuntimeTest(src);
}

test "builtin-str" {
  var src =
  \\ let j = "coolstuff"
  \\ # len
  \\ assert(j.len() == 9, 'should be 9')
  ;
  try doRuntimeTest(src);
}

test "patterns-1.<ordinary match>" {
  var src =
  \\ let e1 = ''
  \\ let e2 = ''
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a' as a, 'b' as b) as d => do
  \\    e1 = a
  \\    e2 = b
  \\  end
  \\  case ('q', 'k') => print('third')
  \\  case _ => print("last")
  \\ end
  \\ assert(e1 == 'a', 'should be a')
  \\ assert(e2 == 'b', 'should be b')
  \\ 
  ;
  try doRuntimeTest(src);
}

test "patterns-2.<scopes>" {
  var src =
  \\ let o = '--'
  \\ let z = false
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a', 'b' as o) as d => z = true
  \\  case ('q', 'k') => print('third')
  \\  case _ => print("last")
  \\ end
  \\ assert(z, 'should match')
  \\ assert(o == '--', 'o should not change')
  ;
  try doRuntimeTest(src);
}

test "patterns-3.<nested match>" {
  var src =
  \\ let z = false
  \\ match (('a', 'b'), ('x', 'y'))
  \\
  \\  case (('x', 'y'), ..) => do
  \\    let p = z
  \\    print('first')
  \\  end
  \\  case (('a', 'p', ..), u) => do
  \\    let b = z
  \\    print('here!')
  \\  end
  \\  case (('a', t, ..) as d, ..) => do
  \\    let h = z
  \\    print('second')
  \\    z = true
  \\  end
  \\  case (('x', k), y) => do
  \\    let v = z
  \\    print('third')
  \\  end
  \\  case (..) as n => let j = n
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-4.<match on unions>" {
  var src =
  \\ let z = false
  \\ let j: 'a' | 'b' | 'c' = 'b'
  \\ match j
  \\  case 'a' => print('a')
  \\  case 'b' => do
  \\    print('ok')
  \\    z = true
  \\  end
  \\  case 'c' => print('hmm')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-5.<match on classes>" {
  var src =
  \\ let z = false
  \\ class Ant
  \\  a = 5
  \\ end
  \\ class Rat
  \\  x = 'yes'
  \\  y = 'no'
  \\  z = 'ok'
  \\ end
  \\ let j = Rat() as Ant | Rat
  \\ match j
  \\  case Ant(a) if a > 2 => print(12)
  \\  case Ant(5) => print(13)
  \\  case Ant(_) => print(40)
  \\  case Rat(x, z='ok.', ..) if x == 'yes' => print(9)
  \\  case Rat(x='ok', z='12', ..) => print(15)
  \\  case Rat(x='yes' as a, y='no' as p, ..) as t => do
  \\    print(19, t, a, p)
  \\    z = true
  \\  end
  \\  case Rat(..) => print(30)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-6.<tuple exhaustiveness>" {
  var src =
  \\ let z = false
  \\ match ('a', 'b')
  \\  case ('x', 'y') => print('first')
  \\  case ('a', 'b' as o) as d => z = true
  \\  case ('q', 'k') => print('third')
  \\  case (..) => print("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-7.<list exhaustiveness>" {
  var src =
  \\ let z = false
  \\ match [('a', 'b')]
  \\  case [('x', 'y')] => print('first')
  \\  case [('a', 'b' as o)] as d => z = true
  \\  case [('q', 'k')] => print('third')
  \\  case [..] => print("last")
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-8.<match exhaustiveness>" {
  var src =
  \\ let z = false
  \\ class Cat
  \\ end
  \\ class Dog
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let z = false
  \\ match p
  \\  case Dog() => print('good')
  \\  case Cat() => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-9.<nested match on classes>" {
  var src =
  \\ let j = [] as list{num}
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\  x = Pooh()
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ match p
  \\  case Cat([Dog(Pooh(a))]) as x => j = a
  \\  case _ => assert(false, 'bad match')
  \\ end
  \\ assert(j[0] == 1, 'should be 1')
  \\ assert(j[1] == 2, 'should be 2')
  ;
  try doRuntimeTest(src);
}

test "patterns-10.<nested match on classes>" {
  var src =
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  y = [Dog()]
  \\ end
  \\ class Dog
  \\  z = [Pooh()]
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case Cat([Dog([Pooh([a, b])])]) as x => do
  \\    print('x:', x, 'a:', a, 'b:', b)
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

test "patterns-11.<nested match on classes>" {
  var src =
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\  x = [Pooh()]
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case Cat([Dog([Pooh([a, b])])]) as x => do
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
  var src =
  \\ class Pooh
  \\  x = [1, 2]
  \\ end
  \\ class Cat
  \\  x = [Dog()]
  \\ end
  \\ class Dog
  \\  x = [Pooh()]
  \\ end
  \\ let p: Cat | Dog = Cat()
  \\ let e1 = 0
  \\ let e2 = 0
  \\ match p
  \\  case Cat(x=[Dog(x=[Pooh(x=[a, b])])]) as x => do
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
  var src =
  \\ let z = false
  \\ let j = [false]
  \\ match j
  \\  case [a, ..] => match a
  \\    case true => print('yay')
  \\    case false as w => z = !w
  \\  end
  \\  case [..] => print('bad')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-14.<match on bool>" {
  var src =
  \\ let z = false
  \\ match (1 < 2)
  \\  case false => print('nay')
  \\  case true as t => z = !!t
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-15.<ranges>" {
  var src =
  \\ let z = false
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => print('hey')
  \\    case 3..5 => z = true
  \\    case _ => print('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-16.<ranges>" {
  var src =
  \\ let z = false
  \\ let n = 10 / 2
  \\  match n
  \\    case 0..2 => print('hey')
  \\    case 3..5 => z = true
  \\    case _ => print('hmm')
  \\  end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-17.<nullable types>" {
  var src =
  \\ let z = false
  \\ let j: ("a" | "b")? = "b"
  \\ match j
  \\   case "a" => print('a!')
  \\   case "b" => z = true
  \\   case nil => print('nah!')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-18.<nullable types>" {
  var src =
  \\ let z = false
  \\ let j: ("a" | "b")? = nil
  \\ match j
  \\   case "a" => print('a!')
  \\   case "b" => print('nah!')
  \\   case nil => z = !z
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-19.<match on generics>" {
  var src =
  \\ class Fox{T}
  \\  j: T
  \\  def init(j: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let z = false
  \\ let j: Fox{str} | Fox{num} = Fox{str}('pin')
  \\ match j
  \\  case Fox{num}(6) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\  case Fox{str}('pin') as x => z = true
  \\  case Fox{str}(_) => print('caught ya str')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-20.<match on generics>" {
  var src =
  \\ class Fox{T}
  \\  j: T
  \\  def init(j: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant
  \\ end
  \\ let z = false
  \\ let j: Fox{str} | Fox{num} | Ant = Ant()
  \\ match j
  \\  case Fox{num}(6) => print('whew')
  \\  case Fox{str}('pin') as x => print('pin')
  \\  case Fox{str}(_) => print('caught ya str')
  \\  case Fox{num}(_) => print('caught ya num')
  \\  case Ant() => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-21.<match on generics>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let j: Fox{str} | Fox{num} = Fox{str}('pin', 'pan')
  \\ let z = false
  \\ match j
  \\  case Fox{str}(('pin', 'pan')) as x => z = true
  \\  case Fox{str}(_) => print('caught ya str')
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-22.<match on generics>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let j: Fox{str} | Fox{num} = Fox{str}('pin', 'pan')
  \\ let z = false
  \\ match j
  \\  case Fox{str}(('^_^',)) => print('caught ya str')
  \\  case Fox{str}(..) as x => z = true
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-23.<match on generics>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ let j: Fox{str} | Fox{num} = Fox{str}('pin', 'pan')
  \\ let z = false
  \\ match j
  \\  case Fox{str}(('pin', ..)) as x => z = true
  \\  case Fox{str}(_) => print('caught ya str')
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-24.<match on generics>" {
  var src =
  \\ class Fox{T}
  \\  j: tuple{T}
  \\  def init(j*: T)
  \\    self.j = j
  \\  end
  \\ end
  \\ class Ant{T}
  \\ end
  \\ let z = false
  \\ let j: Fox{str} | Fox{num} | Ant{str} = Ant{str}()
  \\ match j
  \\  case Fox{str}(..) as x => print('yes', x)
  \\  case Fox{num}((6,)) => print('whew')
  \\  case Fox{num}(_) => print('caught ya num')
  \\  case Ant{str}() => z = true
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-25.<match on rested>" {
  var src =
  \\ class Bug
  \\  x = 5
  \\  y = 10
  \\  z = 12
  \\ end
  \\ let z = false
  \\ match Bug()
  \\  case Bug(1, _, ..) => print('bad')
  \\  case Bug(5, _, ..) => z = true
  \\  case _ => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-26.<match on rested>" {
  var src =
  \\ let z = false
  \\ let j = ('a', 1, 'b', 2, 'c', 3)
  \\ match j
  \\  case ('x', _) => print('yay')
  \\  case ('a', _ as t, ..) => z = true
  \\  case (a, b) => print('nay')
  \\  case _ => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-27.<match on rested>" {
  var src =
  \\ let z = false
  \\ let j = ['a', 1, 'b', 2, 'c', 3]
  \\ match j
  \\  case ['x', _] => print('yay')
  \\  case ['a', _ as t, ..] => z = true
  \\  case [a, b] => print('nay')
  \\  case _ => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-28.<match on generics>" {
  var src =
  \\ let z = false
  \\ class Ok{A}
  \\  val: A
  \\  def init(val: A)
  \\    self.val = val
  \\  end
  \\ end
  \\
  \\ class Err{B}
  \\  val: B
  \\  def init(val: B)
  \\    self.val = val
  \\  end
  \\ end
  \\ 
  \\ type Result{A, B} = Ok{A} | Err{B}
  \\ let j = Err{str}('bad') as Result{num, str}
  \\
  \\ match j
  \\  case Err{str}(v) => z = true
  \\  case Ok{num}(k) => print('ok:', k)
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-29.<match on maps>" {
  var src =
  \\ let z = false
  \\ let j = {'a': 1, 'b': 2, 'c': 3}
  \\ match j
  \\  case {'x': _} => print('yay')
  \\  case {'a': _ as t, ..} => z = true
  \\  case {a: b} => print('nay')
  \\  case _ => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-30.<match on maps>" {
  var src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let z = false
  \\ let foo = {"sound": Fox('fin.co') as Fox | str, "format": "txt"}
  \\ match foo
  \\  case {"sound" as a: Fox(url) as b, "format": "txt",} => z = true
  \\  case {"sound": _, "format": _} => print(1)
  \\  case {..} => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-31.<match on maps>" {
  var src =
  \\ class Fox
  \\  url: str
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
  \\  case [ .. ] => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-32.<match on maps>" {
  var src =
  \\ class Fox
  \\  url: str
  \\  def init(url: str)
  \\    self.url = url
  \\  end
  \\ end
  \\ let foo = {"sound": Fox('fin.co') as Fox | str, "format": "txt"}
  \\ let z = true
  \\ match foo
  \\  case {"sounds": _, "format": _} => print(1)
  \\  case {"sound" as a: Fox(url) as b, "format": _,} => z = true
  \\  case {..} => print('default')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-33.<match on maps>" {
  var src =
  \\ let foo = [{"sound": 1, "format": 2}, {"pin": 3, "pan": 4}]
  \\ let z = false
  \\ match foo
  \\  case [{"sound": 1}] => print('first')
  \\  case [{"sound": 2, ..}] => print('second')
  \\  case [{"sound": _, ..}, ..] => z = true
  \\  case [{..}, ..] => print('fourth')
  \\  case [..] => print('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-34.<match on maps>" {
  var src =
  \\ let foo = [{"sound": 1, "format": 2}, {"pin": 3, "pan": 4}]
  \\ let z = false
  \\ match foo
  \\  case [{"sound": 1}] => print('first')
  \\  case [{"sound": 2, ..}] => print('second')
  \\  case [{..}, ..] => z = true
  \\  case [..] => print('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}

test "patterns-35.<guards with block>" {
  var src =
  \\ let foo = [5, 3]
  \\ let z = false
  \\ match foo
  \\  case [x, y] if x > y => do
  \\    z = !!x and !!y
  \\  end
  \\  case [..] => print('fifth')
  \\ end
  \\ assert(z, 'should be matched')
  ;
  try doRuntimeTest(src);
}
