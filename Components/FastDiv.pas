(*
	FastDiv
	Copyright © 2013-16 Balázs Szalkai
	This file is released under the zlib license:

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	   1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgment in the product documentation would be
	   appreciated but is not required.

	   2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.

	   3. This notice may not be removed or altered from any source
	   distribution.
*)
// Generated using the libdivide library by ridiculous_fish
unit FastDiv;

{$mode delphi}
interface

{$ifdef WINDOWS} // TODO Linux ABI?
{$ifdef CPU386}
{$define ASM32}
{$endif}
{$ifdef CPUX86_64}
{$define ASM64}
{$endif}
{$endif}

uses
  SysUtils;

type
  TDivFunc = function(x: Cardinal): Cardinal;

function DivBy0(x: Cardinal): Cardinal;
function DivBy1(x: Cardinal): Cardinal;
function DivBy2(x: Cardinal): Cardinal;
function DivBy3(x: Cardinal): Cardinal;
function DivBy4(x: Cardinal): Cardinal;
function DivBy5(x: Cardinal): Cardinal;
function DivBy6(x: Cardinal): Cardinal;
function DivBy7(x: Cardinal): Cardinal;
function DivBy8(x: Cardinal): Cardinal;
function DivBy9(x: Cardinal): Cardinal;
function DivBy10(x: Cardinal): Cardinal;
function DivBy11(x: Cardinal): Cardinal;
function DivBy12(x: Cardinal): Cardinal;
function DivBy13(x: Cardinal): Cardinal;
function DivBy14(x: Cardinal): Cardinal;
function DivBy15(x: Cardinal): Cardinal;
function DivBy16(x: Cardinal): Cardinal;
function DivBy17(x: Cardinal): Cardinal;
function DivBy18(x: Cardinal): Cardinal;
function DivBy19(x: Cardinal): Cardinal;
function DivBy20(x: Cardinal): Cardinal;
function DivBy21(x: Cardinal): Cardinal;
function DivBy22(x: Cardinal): Cardinal;
function DivBy23(x: Cardinal): Cardinal;
function DivBy24(x: Cardinal): Cardinal;
function DivBy25(x: Cardinal): Cardinal;
function DivBy26(x: Cardinal): Cardinal;
function DivBy27(x: Cardinal): Cardinal;
function DivBy28(x: Cardinal): Cardinal;
function DivBy29(x: Cardinal): Cardinal;
function DivBy30(x: Cardinal): Cardinal;
function DivBy31(x: Cardinal): Cardinal;
function DivBy32(x: Cardinal): Cardinal;
function DivBy33(x: Cardinal): Cardinal;
function DivBy34(x: Cardinal): Cardinal;
function DivBy35(x: Cardinal): Cardinal;
function DivBy36(x: Cardinal): Cardinal;
function DivBy37(x: Cardinal): Cardinal;
function DivBy38(x: Cardinal): Cardinal;
function DivBy39(x: Cardinal): Cardinal;
function DivBy40(x: Cardinal): Cardinal;
function DivBy41(x: Cardinal): Cardinal;
function DivBy42(x: Cardinal): Cardinal;
function DivBy43(x: Cardinal): Cardinal;
function DivBy44(x: Cardinal): Cardinal;
function DivBy45(x: Cardinal): Cardinal;
function DivBy46(x: Cardinal): Cardinal;
function DivBy47(x: Cardinal): Cardinal;
function DivBy48(x: Cardinal): Cardinal;
function DivBy49(x: Cardinal): Cardinal;
function DivBy50(x: Cardinal): Cardinal;
function DivBy51(x: Cardinal): Cardinal;
function DivBy52(x: Cardinal): Cardinal;
function DivBy53(x: Cardinal): Cardinal;
function DivBy54(x: Cardinal): Cardinal;
function DivBy55(x: Cardinal): Cardinal;
function DivBy56(x: Cardinal): Cardinal;
function DivBy57(x: Cardinal): Cardinal;
function DivBy58(x: Cardinal): Cardinal;
function DivBy59(x: Cardinal): Cardinal;
function DivBy60(x: Cardinal): Cardinal;
function DivBy61(x: Cardinal): Cardinal;
function DivBy62(x: Cardinal): Cardinal;
function DivBy63(x: Cardinal): Cardinal;
function DivBy64(x: Cardinal): Cardinal;
function DivBy65(x: Cardinal): Cardinal;
function DivBy66(x: Cardinal): Cardinal;
function DivBy67(x: Cardinal): Cardinal;
function DivBy68(x: Cardinal): Cardinal;
function DivBy69(x: Cardinal): Cardinal;
function DivBy70(x: Cardinal): Cardinal;
function DivBy71(x: Cardinal): Cardinal;
function DivBy72(x: Cardinal): Cardinal;
function DivBy73(x: Cardinal): Cardinal;
function DivBy74(x: Cardinal): Cardinal;
function DivBy75(x: Cardinal): Cardinal;
function DivBy76(x: Cardinal): Cardinal;
function DivBy77(x: Cardinal): Cardinal;
function DivBy78(x: Cardinal): Cardinal;
function DivBy79(x: Cardinal): Cardinal;
function DivBy80(x: Cardinal): Cardinal;
function DivBy81(x: Cardinal): Cardinal;
function DivBy82(x: Cardinal): Cardinal;
function DivBy83(x: Cardinal): Cardinal;
function DivBy84(x: Cardinal): Cardinal;
function DivBy85(x: Cardinal): Cardinal;
function DivBy86(x: Cardinal): Cardinal;
function DivBy87(x: Cardinal): Cardinal;
function DivBy88(x: Cardinal): Cardinal;
function DivBy89(x: Cardinal): Cardinal;
function DivBy90(x: Cardinal): Cardinal;
function DivBy91(x: Cardinal): Cardinal;
function DivBy92(x: Cardinal): Cardinal;
function DivBy93(x: Cardinal): Cardinal;
function DivBy94(x: Cardinal): Cardinal;
function DivBy95(x: Cardinal): Cardinal;
function DivBy96(x: Cardinal): Cardinal;
function DivBy97(x: Cardinal): Cardinal;
function DivBy98(x: Cardinal): Cardinal;
function DivBy99(x: Cardinal): Cardinal;
function DivBy100(x: Cardinal): Cardinal;
function DivBy101(x: Cardinal): Cardinal;
function DivBy102(x: Cardinal): Cardinal;
function DivBy103(x: Cardinal): Cardinal;
function DivBy104(x: Cardinal): Cardinal;
function DivBy105(x: Cardinal): Cardinal;
function DivBy106(x: Cardinal): Cardinal;
function DivBy107(x: Cardinal): Cardinal;
function DivBy108(x: Cardinal): Cardinal;
function DivBy109(x: Cardinal): Cardinal;
function DivBy110(x: Cardinal): Cardinal;
function DivBy111(x: Cardinal): Cardinal;
function DivBy112(x: Cardinal): Cardinal;
function DivBy113(x: Cardinal): Cardinal;
function DivBy114(x: Cardinal): Cardinal;
function DivBy115(x: Cardinal): Cardinal;
function DivBy116(x: Cardinal): Cardinal;
function DivBy117(x: Cardinal): Cardinal;
function DivBy118(x: Cardinal): Cardinal;
function DivBy119(x: Cardinal): Cardinal;
function DivBy120(x: Cardinal): Cardinal;
function DivBy121(x: Cardinal): Cardinal;
function DivBy122(x: Cardinal): Cardinal;
function DivBy123(x: Cardinal): Cardinal;
function DivBy124(x: Cardinal): Cardinal;
function DivBy125(x: Cardinal): Cardinal;
function DivBy126(x: Cardinal): Cardinal;
function DivBy127(x: Cardinal): Cardinal;
function DivBy128(x: Cardinal): Cardinal;
function DivBy129(x: Cardinal): Cardinal;
function DivBy130(x: Cardinal): Cardinal;
function DivBy131(x: Cardinal): Cardinal;
function DivBy132(x: Cardinal): Cardinal;
function DivBy133(x: Cardinal): Cardinal;
function DivBy134(x: Cardinal): Cardinal;
function DivBy135(x: Cardinal): Cardinal;
function DivBy136(x: Cardinal): Cardinal;
function DivBy137(x: Cardinal): Cardinal;
function DivBy138(x: Cardinal): Cardinal;
function DivBy139(x: Cardinal): Cardinal;
function DivBy140(x: Cardinal): Cardinal;
function DivBy141(x: Cardinal): Cardinal;
function DivBy142(x: Cardinal): Cardinal;
function DivBy143(x: Cardinal): Cardinal;
function DivBy144(x: Cardinal): Cardinal;
function DivBy145(x: Cardinal): Cardinal;
function DivBy146(x: Cardinal): Cardinal;
function DivBy147(x: Cardinal): Cardinal;
function DivBy148(x: Cardinal): Cardinal;
function DivBy149(x: Cardinal): Cardinal;
function DivBy150(x: Cardinal): Cardinal;
function DivBy151(x: Cardinal): Cardinal;
function DivBy152(x: Cardinal): Cardinal;
function DivBy153(x: Cardinal): Cardinal;
function DivBy154(x: Cardinal): Cardinal;
function DivBy155(x: Cardinal): Cardinal;
function DivBy156(x: Cardinal): Cardinal;
function DivBy157(x: Cardinal): Cardinal;
function DivBy158(x: Cardinal): Cardinal;
function DivBy159(x: Cardinal): Cardinal;
function DivBy160(x: Cardinal): Cardinal;
function DivBy161(x: Cardinal): Cardinal;
function DivBy162(x: Cardinal): Cardinal;
function DivBy163(x: Cardinal): Cardinal;
function DivBy164(x: Cardinal): Cardinal;
function DivBy165(x: Cardinal): Cardinal;
function DivBy166(x: Cardinal): Cardinal;
function DivBy167(x: Cardinal): Cardinal;
function DivBy168(x: Cardinal): Cardinal;
function DivBy169(x: Cardinal): Cardinal;
function DivBy170(x: Cardinal): Cardinal;
function DivBy171(x: Cardinal): Cardinal;
function DivBy172(x: Cardinal): Cardinal;
function DivBy173(x: Cardinal): Cardinal;
function DivBy174(x: Cardinal): Cardinal;
function DivBy175(x: Cardinal): Cardinal;
function DivBy176(x: Cardinal): Cardinal;
function DivBy177(x: Cardinal): Cardinal;
function DivBy178(x: Cardinal): Cardinal;
function DivBy179(x: Cardinal): Cardinal;
function DivBy180(x: Cardinal): Cardinal;
function DivBy181(x: Cardinal): Cardinal;
function DivBy182(x: Cardinal): Cardinal;
function DivBy183(x: Cardinal): Cardinal;
function DivBy184(x: Cardinal): Cardinal;
function DivBy185(x: Cardinal): Cardinal;
function DivBy186(x: Cardinal): Cardinal;
function DivBy187(x: Cardinal): Cardinal;
function DivBy188(x: Cardinal): Cardinal;
function DivBy189(x: Cardinal): Cardinal;
function DivBy190(x: Cardinal): Cardinal;
function DivBy191(x: Cardinal): Cardinal;
function DivBy192(x: Cardinal): Cardinal;
function DivBy193(x: Cardinal): Cardinal;
function DivBy194(x: Cardinal): Cardinal;
function DivBy195(x: Cardinal): Cardinal;
function DivBy196(x: Cardinal): Cardinal;
function DivBy197(x: Cardinal): Cardinal;
function DivBy198(x: Cardinal): Cardinal;
function DivBy199(x: Cardinal): Cardinal;
function DivBy200(x: Cardinal): Cardinal;
function DivBy201(x: Cardinal): Cardinal;
function DivBy202(x: Cardinal): Cardinal;
function DivBy203(x: Cardinal): Cardinal;
function DivBy204(x: Cardinal): Cardinal;
function DivBy205(x: Cardinal): Cardinal;
function DivBy206(x: Cardinal): Cardinal;
function DivBy207(x: Cardinal): Cardinal;
function DivBy208(x: Cardinal): Cardinal;
function DivBy209(x: Cardinal): Cardinal;
function DivBy210(x: Cardinal): Cardinal;
function DivBy211(x: Cardinal): Cardinal;
function DivBy212(x: Cardinal): Cardinal;
function DivBy213(x: Cardinal): Cardinal;
function DivBy214(x: Cardinal): Cardinal;
function DivBy215(x: Cardinal): Cardinal;
function DivBy216(x: Cardinal): Cardinal;
function DivBy217(x: Cardinal): Cardinal;
function DivBy218(x: Cardinal): Cardinal;
function DivBy219(x: Cardinal): Cardinal;
function DivBy220(x: Cardinal): Cardinal;
function DivBy221(x: Cardinal): Cardinal;
function DivBy222(x: Cardinal): Cardinal;
function DivBy223(x: Cardinal): Cardinal;
function DivBy224(x: Cardinal): Cardinal;
function DivBy225(x: Cardinal): Cardinal;
function DivBy226(x: Cardinal): Cardinal;
function DivBy227(x: Cardinal): Cardinal;
function DivBy228(x: Cardinal): Cardinal;
function DivBy229(x: Cardinal): Cardinal;
function DivBy230(x: Cardinal): Cardinal;
function DivBy231(x: Cardinal): Cardinal;
function DivBy232(x: Cardinal): Cardinal;
function DivBy233(x: Cardinal): Cardinal;
function DivBy234(x: Cardinal): Cardinal;
function DivBy235(x: Cardinal): Cardinal;
function DivBy236(x: Cardinal): Cardinal;
function DivBy237(x: Cardinal): Cardinal;
function DivBy238(x: Cardinal): Cardinal;
function DivBy239(x: Cardinal): Cardinal;
function DivBy240(x: Cardinal): Cardinal;
function DivBy241(x: Cardinal): Cardinal;
function DivBy242(x: Cardinal): Cardinal;
function DivBy243(x: Cardinal): Cardinal;
function DivBy244(x: Cardinal): Cardinal;
function DivBy245(x: Cardinal): Cardinal;
function DivBy246(x: Cardinal): Cardinal;
function DivBy247(x: Cardinal): Cardinal;
function DivBy248(x: Cardinal): Cardinal;
function DivBy249(x: Cardinal): Cardinal;
function DivBy250(x: Cardinal): Cardinal;
function DivBy251(x: Cardinal): Cardinal;
function DivBy252(x: Cardinal): Cardinal;
function DivBy253(x: Cardinal): Cardinal;
function DivBy254(x: Cardinal): Cardinal;
function DivBy255(x: Cardinal): Cardinal;
function DivBy256(x: Cardinal): Cardinal;
function DivBy510(x: Cardinal): Cardinal;

const DivFunc: array[0..256] of TDivFunc = (
DivBy0, 
DivBy1, DivBy2, DivBy3, DivBy4, DivBy5, DivBy6, DivBy7, DivBy8, 
DivBy9, DivBy10, DivBy11, DivBy12, DivBy13, DivBy14, DivBy15, DivBy16, 
DivBy17, DivBy18, DivBy19, DivBy20, DivBy21, DivBy22, DivBy23, DivBy24, 
DivBy25, DivBy26, DivBy27, DivBy28, DivBy29, DivBy30, DivBy31, DivBy32, 
DivBy33, DivBy34, DivBy35, DivBy36, DivBy37, DivBy38, DivBy39, DivBy40, 
DivBy41, DivBy42, DivBy43, DivBy44, DivBy45, DivBy46, DivBy47, DivBy48, 
DivBy49, DivBy50, DivBy51, DivBy52, DivBy53, DivBy54, DivBy55, DivBy56, 
DivBy57, DivBy58, DivBy59, DivBy60, DivBy61, DivBy62, DivBy63, DivBy64, 
DivBy65, DivBy66, DivBy67, DivBy68, DivBy69, DivBy70, DivBy71, DivBy72, 
DivBy73, DivBy74, DivBy75, DivBy76, DivBy77, DivBy78, DivBy79, DivBy80, 
DivBy81, DivBy82, DivBy83, DivBy84, DivBy85, DivBy86, DivBy87, DivBy88, 
DivBy89, DivBy90, DivBy91, DivBy92, DivBy93, DivBy94, DivBy95, DivBy96, 
DivBy97, DivBy98, DivBy99, DivBy100, DivBy101, DivBy102, DivBy103, DivBy104, 
DivBy105, DivBy106, DivBy107, DivBy108, DivBy109, DivBy110, DivBy111, DivBy112, 
DivBy113, DivBy114, DivBy115, DivBy116, DivBy117, DivBy118, DivBy119, DivBy120, 
DivBy121, DivBy122, DivBy123, DivBy124, DivBy125, DivBy126, DivBy127, DivBy128, 
DivBy129, DivBy130, DivBy131, DivBy132, DivBy133, DivBy134, DivBy135, DivBy136, 
DivBy137, DivBy138, DivBy139, DivBy140, DivBy141, DivBy142, DivBy143, DivBy144, 
DivBy145, DivBy146, DivBy147, DivBy148, DivBy149, DivBy150, DivBy151, DivBy152, 
DivBy153, DivBy154, DivBy155, DivBy156, DivBy157, DivBy158, DivBy159, DivBy160, 
DivBy161, DivBy162, DivBy163, DivBy164, DivBy165, DivBy166, DivBy167, DivBy168, 
DivBy169, DivBy170, DivBy171, DivBy172, DivBy173, DivBy174, DivBy175, DivBy176, 
DivBy177, DivBy178, DivBy179, DivBy180, DivBy181, DivBy182, DivBy183, DivBy184, 
DivBy185, DivBy186, DivBy187, DivBy188, DivBy189, DivBy190, DivBy191, DivBy192, 
DivBy193, DivBy194, DivBy195, DivBy196, DivBy197, DivBy198, DivBy199, DivBy200, 
DivBy201, DivBy202, DivBy203, DivBy204, DivBy205, DivBy206, DivBy207, DivBy208, 
DivBy209, DivBy210, DivBy211, DivBy212, DivBy213, DivBy214, DivBy215, DivBy216, 
DivBy217, DivBy218, DivBy219, DivBy220, DivBy221, DivBy222, DivBy223, DivBy224, 
DivBy225, DivBy226, DivBy227, DivBy228, DivBy229, DivBy230, DivBy231, DivBy232, 
DivBy233, DivBy234, DivBy235, DivBy236, DivBy237, DivBy238, DivBy239, DivBy240, 
DivBy241, DivBy242, DivBy243, DivBy244, DivBy245, DivBy246, DivBy247, DivBy248, 
DivBy249, DivBy250, DivBy251, DivBy252, DivBy253, DivBy254, DivBy255, DivBy256
);

implementation

function DivBy0(x: Cardinal): Cardinal;
begin
  raise EDivByZero.Create('Division by zero');
  Result := 0; // suppress warnings
end;

function DivBy1(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
{$ELSE}
begin
  Result := x div 1;
{$ENDIF}
{$ENDIF}
end;

function DivBy2(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 1
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 1
{$ELSE}
begin
  Result := x div 2;
{$ENDIF}
{$ENDIF}
end;

function DivBy3(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 1
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 1
{$ELSE}
begin
  Result := x div 3;
{$ENDIF}
{$ENDIF}
end;

function DivBy4(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 2
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 2
{$ELSE}
begin
  Result := x div 4;
{$ENDIF}
{$ENDIF}
end;

function DivBy5(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3435973837
  mul ecx
  mov eax, edx
  shr eax, 2
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14757395258967641293
  mul rcx
  mov rax, rdx
  shr rax, 2
{$ELSE}
begin
  Result := x div 5;
{$ENDIF}
{$ENDIF}
end;

function DivBy6(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 2
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 2
{$ELSE}
begin
  Result := x div 6;
{$ENDIF}
{$ENDIF}
end;

function DivBy7(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 613566757
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 2
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2635249153387078803
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 2
{$ELSE}
begin
  Result := x div 7;
{$ENDIF}
{$ENDIF}
end;

function DivBy8(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 3
{$ELSE}
begin
  Result := x div 8;
{$ENDIF}
{$ENDIF}
end;

function DivBy9(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3817748708
  mul ecx
  mov eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16397105843297379215
  mul rcx
  mov rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 9;
{$ENDIF}
{$ENDIF}
end;

function DivBy10(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3435973837
  mul ecx
  mov eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14757395258967641293
  mul rcx
  mov rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 10;
{$ENDIF}
{$ENDIF}
end;

function DivBy11(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3123612579
  mul ecx
  mov eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13415813871788764812
  mul rcx
  mov rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 11;
{$ENDIF}
{$ENDIF}
end;

function DivBy12(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 12;
{$ENDIF}
{$ENDIF}
end;

function DivBy13(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2643056798
  mul ecx
  mov eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11351842506898185610
  mul rcx
  mov rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 13;
{$ENDIF}
{$ENDIF}
end;

function DivBy14(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 613566757
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2635249153387078803
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 14;
{$ENDIF}
{$ENDIF}
end;

function DivBy15(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2290649225
  mul ecx
  mov eax, edx
  shr eax, 3
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9838263505978427529
  mul rcx
  mov rax, rdx
  shr rax, 3
{$ELSE}
begin
  Result := x div 15;
{$ENDIF}
{$ENDIF}
end;

function DivBy16(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 4
{$ELSE}
begin
  Result := x div 16;
{$ENDIF}
{$ENDIF}
end;

function DivBy17(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4042322161
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17361641481138401521
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 17;
{$ENDIF}
{$ENDIF}
end;

function DivBy18(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3817748708
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16397105843297379215
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 18;
{$ENDIF}
{$ENDIF}
end;

function DivBy19(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2938661835
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15534100272597517151
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 19;
{$ENDIF}
{$ENDIF}
end;

function DivBy20(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3435973837
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14757395258967641293
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 20;
{$ENDIF}
{$ENDIF}
end;

function DivBy21(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2249744775
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 9662580229085955609
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 21;
{$ENDIF}
{$ENDIF}
end;

function DivBy22(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3123612579
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13415813871788764812
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 22;
{$ENDIF}
{$ENDIF}
end;

function DivBy23(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2987803337
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7218291159277650633
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 23;
{$ENDIF}
{$ENDIF}
end;

function DivBy24(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 24;
{$ENDIF}
{$ENDIF}
end;

function DivBy25(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2748779070
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5165088340638674453
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 25;
{$ENDIF}
{$ENDIF}
end;

function DivBy26(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2643056798
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11351842506898185610
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 26;
{$ENDIF}
{$ENDIF}
end;

function DivBy27(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 795364315
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10931403895531586143
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 27;
{$ENDIF}
{$ENDIF}
end;

function DivBy28(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 613566757
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2635249153387078803
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 28;
{$ENDIF}
{$ENDIF}
end;

function DivBy29(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2369637129
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1908283869694091547
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 29;
{$ENDIF}
{$ENDIF}
end;

function DivBy30(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2290649225
  mul ecx
  mov eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9838263505978427529
  mul rcx
  mov rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 30;
{$ENDIF}
{$ENDIF}
end;

function DivBy31(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 138547333
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 4
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 595056260442243601
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 4
{$ELSE}
begin
  Result := x div 31;
{$ENDIF}
{$ENDIF}
end;

function DivBy32(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 5
{$ELSE}
begin
  Result := x div 32;
{$ENDIF}
{$ENDIF}
end;

function DivBy33(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4164816772
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17887751829051686416
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 33;
{$ENDIF}
{$ENDIF}
end;

function DivBy34(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4042322161
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17361641481138401521
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 34;
{$ENDIF}
{$ENDIF}
end;

function DivBy35(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3558687189
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16865594581677304335
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 35;
{$ENDIF}
{$ENDIF}
end;

function DivBy36(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3817748708
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16397105843297379215
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 36;
{$ENDIF}
{$ENDIF}
end;

function DivBy37(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3134165325
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15953940820505558155
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 37;
{$ENDIF}
{$ENDIF}
end;

function DivBy38(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2938661835
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15534100272597517151
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 38;
{$ENDIF}
{$ENDIF}
end;

function DivBy39(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2753184165
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 11824835944685610011
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 39;
{$ENDIF}
{$ENDIF}
end;

function DivBy40(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3435973837
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14757395258967641293
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 40;
{$ENDIF}
{$ENDIF}
end;

function DivBy41(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3352169597
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14397458789236723213
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 41;
{$ENDIF}
{$ENDIF}
end;

function DivBy42(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2249744775
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 9662580229085955609
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 42;
{$ENDIF}
{$ENDIF}
end;

function DivBy43(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3196254732
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13727809543225712831
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 43;
{$ENDIF}
{$ENDIF}
end;

function DivBy44(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3123612579
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13415813871788764812
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 44;
{$ENDIF}
{$ENDIF}
end;

function DivBy45(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1813430637
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13117684674637903372
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 45;
{$ENDIF}
{$ENDIF}
end;

function DivBy46(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2987803337
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7218291159277650633
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 46;
{$ENDIF}
{$ENDIF}
end;

function DivBy47(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2924233053
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6672226579852391011
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 47;
{$ENDIF}
{$ENDIF}
end;

function DivBy48(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 48;
{$ENDIF}
{$ENDIF}
end;

function DivBy49(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2804876602
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5646962471543740291
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 49;
{$ENDIF}
{$ENDIF}
end;

function DivBy50(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2748779070
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5165088340638674453
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 50;
{$ENDIF}
{$ENDIF}
end;

function DivBy51(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2694881441
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11574427654092267681
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 51;
{$ENDIF}
{$ENDIF}
end;

function DivBy52(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2643056798
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11351842506898185610
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 52;
{$ENDIF}
{$ENDIF}
end;

function DivBy53(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 891408307
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3828569524732171091
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 53;
{$ENDIF}
{$ENDIF}
end;

function DivBy54(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 795364315
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10931403895531586143
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 54;
{$ENDIF}
{$ENDIF}
end;

function DivBy55(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 702812831
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3018558121152472083
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 55;
{$ENDIF}
{$ENDIF}
end;

function DivBy56(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 613566757
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2635249153387078803
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 56;
{$ENDIF}
{$ENDIF}
end;

function DivBy57(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 527452125
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10356066848398344767
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 57;
{$ENDIF}
{$ENDIF}
end;

function DivBy58(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2369637129
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1908283869694091547
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 58;
{$ENDIF}
{$ENDIF}
end;

function DivBy59(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2329473788
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10005013734893316131
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 59;
{$ENDIF}
{$ENDIF}
end;

function DivBy60(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2290649225
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9838263505978427529
  mul rcx
  mov rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 60;
{$ENDIF}
{$ENDIF}
end;

function DivBy61(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2253097598
  mul ecx
  mov eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 907216921657846801
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 61;
{$ENDIF}
{$ENDIF}
end;

function DivBy62(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 138547333
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 595056260442243601
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 62;
{$ENDIF}
{$ENDIF}
end;

function DivBy63(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 68174085
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 5
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 292805461487453201
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 5
{$ELSE}
begin
  Result := x div 63;
{$ENDIF}
{$ENDIF}
end;

function DivBy64(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 6
{$ELSE}
begin
  Result := x div 64;
{$ENDIF}
{$ENDIF}
end;

function DivBy65(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4228890877
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 18162948011037096976
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 65;
{$ENDIF}
{$ENDIF}
end;

function DivBy66(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4164816772
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17887751829051686416
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 66;
{$ENDIF}
{$ENDIF}
end;

function DivBy67(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4102655328
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17620770458468825425
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 67;
{$ENDIF}
{$ENDIF}
end;

function DivBy68(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4042322161
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17361641481138401521
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 68;
{$ENDIF}
{$ENDIF}
end;

function DivBy69(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3983737782
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17110023488658134833
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 69;
{$ENDIF}
{$ENDIF}
end;

function DivBy70(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3558687189
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16865594581677304335
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 70;
{$ENDIF}
{$ENDIF}
end;

function DivBy71(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3871519817
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 14809357918330203411
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 71;
{$ENDIF}
{$ENDIF}
end;

function DivBy72(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3817748708
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16397105843297379215
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 72;
{$ENDIF}
{$ENDIF}
end;

function DivBy73(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3235934265
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16172487955033031554
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 73;
{$ENDIF}
{$ENDIF}
end;

function DivBy74(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3134165325
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15953940820505558155
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 74;
{$ENDIF}
{$ENDIF}
end;

function DivBy75(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3665038760
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15741221609565484046
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 75;
{$ENDIF}
{$ENDIF}
end;

function DivBy76(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2938661835
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15534100272597517151
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 76;
{$ENDIF}
{$ENDIF}
end;

function DivBy77(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3569842948
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15332358710615731214
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 77;
{$ENDIF}
{$ENDIF}
end;

function DivBy78(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2753184165
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 11824835944685610011
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 78;
{$ENDIF}
{$ENDIF}
end;

function DivBy79(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3479467177
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14944197730600143082
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 79;
{$ENDIF}
{$ENDIF}
end;

function DivBy80(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3435973837
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14757395258967641293
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 80;
{$ENDIF}
{$ENDIF}
end;

function DivBy81(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3393554407
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 10703666314374678099
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 81;
{$ENDIF}
{$ENDIF}
end;

function DivBy82(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3352169597
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14397458789236723213
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 82;
{$ENDIF}
{$ENDIF}
end;

function DivBy83(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3311782012
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14223995430330256668
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 83;
{$ENDIF}
{$ENDIF}
end;

function DivBy84(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2249744775
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 9662580229085955609
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 84;
{$ENDIF}
{$ENDIF}
end;

function DivBy85(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3233857729
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13889313184910721217
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 85;
{$ENDIF}
{$ENDIF}
end;

function DivBy86(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3196254732
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13727809543225712831
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 86;
{$ENDIF}
{$ENDIF}
end;

function DivBy87(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3159516172
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13570018628935762109
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 87;
{$ENDIF}
{$ENDIF}
end;

function DivBy88(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3123612579
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13415813871788764812
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 88;
{$ENDIF}
{$ENDIF}
end;

function DivBy89(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3088515809
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 8083404706456994529
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 89;
{$ENDIF}
{$ENDIF}
end;

function DivBy90(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1813430637
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13117684674637903372
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 90;
{$ENDIF}
{$ENDIF}
end;

function DivBy91(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1746305385
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7500324513486301207
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 91;
{$ENDIF}
{$ENDIF}
end;

function DivBy92(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2987803337
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7218291159277650633
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 92;
{$ENDIF}
{$ENDIF}
end;

function DivBy93(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2955676419
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6942323038492842007
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 93;
{$ENDIF}
{$ENDIF}
end;

function DivBy94(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2924233053
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6672226579852391011
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 94;
{$ENDIF}
{$ENDIF}
end;

function DivBy95(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1491936009
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6407816362446475825
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 95;
{$ENDIF}
{$ENDIF}
end;

function DivBy96(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 96;
{$ENDIF}
{$ENDIF}
end;

function DivBy97(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1372618415
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5895351198814392785
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 97;
{$ENDIF}
{$ENDIF}
end;

function DivBy98(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2804876602
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5646962471543740291
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 98;
{$ENDIF}
{$ENDIF}
end;

function DivBy99(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2776544515
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5403591698359363605
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 99;
{$ENDIF}
{$ENDIF}
end;

function DivBy100(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2748779070
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5165088340638674453
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 100;
{$ENDIF}
{$ENDIF}
end;

function DivBy101(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1148159575
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 4931307821684731621
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 101;
{$ENDIF}
{$ENDIF}
end;

function DivBy102(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2694881441
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11574427654092267681
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 102;
{$ENDIF}
{$ENDIF}
end;

function DivBy103(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1042467791
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 4477365066434357189
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 103;
{$ENDIF}
{$ENDIF}
end;

function DivBy104(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2643056798
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11351842506898185610
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 104;
{$ENDIF}
{$ENDIF}
end;

function DivBy105(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 940802361
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11243729721118202890
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 105;
{$ENDIF}
{$ENDIF}
end;

function DivBy106(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 891408307
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3828569524732171091
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 106;
{$ENDIF}
{$ENDIF}
end;

function DivBy107(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 842937507
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3620389023812154991
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 107;
{$ENDIF}
{$ENDIF}
end;

function DivBy108(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 795364315
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10931403895531586143
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 108;
{$ENDIF}
{$ENDIF}
end;

function DivBy109(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 748664025
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10831115786398268839
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 109;
{$ENDIF}
{$ENDIF}
end;

function DivBy110(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 702812831
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3018558121152472083
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 110;
{$ENDIF}
{$ENDIF}
end;

function DivBy111(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 657787785
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2825177020297859257
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 111;
{$ENDIF}
{$ENDIF}
end;

function DivBy112(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 613566757
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2635249153387078803
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 112;
{$ENDIF}
{$ENDIF}
end;

function DivBy113(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 570128403
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10447713457676206225
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 113;
{$ENDIF}
{$ENDIF}
end;

function DivBy114(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 527452125
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10356066848398344767
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 114;
{$ENDIF}
{$ENDIF}
end;

function DivBy115(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 485518043
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2085284112680210183
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 115;
{$ENDIF}
{$ENDIF}
end;

function DivBy116(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2369637129
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1908283869694091547
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 116;
{$ENDIF}
{$ENDIF}
end;

function DivBy117(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 403800345
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10090526672798387209
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 117;
{$ENDIF}
{$ENDIF}
end;

function DivBy118(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2329473788
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10005013734893316131
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 118;
{$ENDIF}
{$ENDIF}
end;

function DivBy119(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2309898378
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1395131904734335837
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 119;
{$ENDIF}
{$ENDIF}
end;

function DivBy120(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2290649225
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9838263505978427529
  mul rcx
  mov rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 120;
{$ENDIF}
{$ENDIF}
end;

function DivBy121(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 248469183
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1067167012528651747
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 121;
{$ENDIF}
{$ENDIF}
end;

function DivBy122(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2253097598
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 907216921657846801
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 122;
{$ENDIF}
{$ENDIF}
end;

function DivBy123(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 174592167
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 749867645272746001
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 123;
{$ENDIF}
{$ENDIF}
end;

function DivBy124(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 138547333
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 595056260442243601
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 124;
{$ENDIF}
{$ENDIF}
end;

function DivBy125(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2199023256
  mul ecx
  mov eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 442721857769029239
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 125;
{$ENDIF}
{$ENDIF}
end;

function DivBy126(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 68174085
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 292805461487453201
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 126;
{$ENDIF}
{$ENDIF}
end;

function DivBy127(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 33818641
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 6
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 145249953336295683
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 6
{$ELSE}
begin
  Result := x div 127;
{$ENDIF}
{$ENDIF}
end;

function DivBy128(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 7
{$ELSE}
begin
  Result := x div 128;
{$ENDIF}
{$ENDIF}
end;

function DivBy129(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4261672976
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 18303746057634283775
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 129;
{$ENDIF}
{$ENDIF}
end;

function DivBy130(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4228890877
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 18162948011037096976
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 130;
{$ENDIF}
{$ENDIF}
end;

function DivBy131(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4196609267
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 18024299552937577152
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 131;
{$ENDIF}
{$ENDIF}
end;

function DivBy132(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4164816772
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17887751829051686416
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 132;
{$ENDIF}
{$ENDIF}
end;

function DivBy133(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4133502361
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17753257454397162458
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 133;
{$ENDIF}
{$ENDIF}
end;

function DivBy134(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4102655328
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17620770458468825425
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 134;
{$ENDIF}
{$ENDIF}
end;

function DivBy135(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4072265289
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17490246232850537829
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 135;
{$ENDIF}
{$ENDIF}
end;

function DivBy136(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4042322161
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17361641481138401521
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 136;
{$ENDIF}
{$ENDIF}
end;

function DivBy137(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 4012816160
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 16023084268404647025
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 137;
{$ENDIF}
{$ENDIF}
end;

function DivBy138(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3983737782
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 17110023488658134833
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 138;
{$ENDIF}
{$ENDIF}
end;

function DivBy139(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3955077798
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16986929794495126668
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 139;
{$ENDIF}
{$ENDIF}
end;

function DivBy140(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3558687189
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16865594581677304335
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 140;
{$ENDIF}
{$ENDIF}
end;

function DivBy141(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3898977404
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16745980435707961751
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 141;
{$ENDIF}
{$ENDIF}
end;

function DivBy142(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3871519817
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 14809357918330203411
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 142;
{$ENDIF}
{$ENDIF}
end;

function DivBy143(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3844446251
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16511770919124633615
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 143;
{$ENDIF}
{$ENDIF}
end;

function DivBy144(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3817748708
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16397105843297379215
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 144;
{$ENDIF}
{$ENDIF}
end;

function DivBy145(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3791419407
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16284022354722914530
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 145;
{$ENDIF}
{$ENDIF}
end;

function DivBy146(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3235934265
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 16172487955033031554
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 146;
{$ENDIF}
{$ENDIF}
end;

function DivBy147(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3739835469
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 13678197986628170927
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 147;
{$ENDIF}
{$ENDIF}
end;

function DivBy148(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 3134165325
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15953940820505558155
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 148;
{$ENDIF}
{$ENDIF}
end;

function DivBy149(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3689636335
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15846867392180017496
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 149;
{$ENDIF}
{$ENDIF}
end;

function DivBy150(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3665038760
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15741221609565484046
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 150;
{$ENDIF}
{$ENDIF}
end;

function DivBy151(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3640766980
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15636975108839884814
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 151;
{$ENDIF}
{$ENDIF}
end;

function DivBy152(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2938661835
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15534100272597517151
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 152;
{$ENDIF}
{$ENDIF}
end;

function DivBy153(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3593175255
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15432570205456356908
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 153;
{$ENDIF}
{$ENDIF}
end;

function DivBy154(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3569842948
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15332358710615731214
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 154;
{$ENDIF}
{$ENDIF}
end;

function DivBy155(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3546811703
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15233440267321436174
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 155;
{$ENDIF}
{$ENDIF}
end;

function DivBy156(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2753184165
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 11824835944685610011
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 156;
{$ENDIF}
{$ENDIF}
end;

function DivBy157(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3501629388
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 15039383703406513420
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 157;
{$ENDIF}
{$ENDIF}
end;

function DivBy158(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3479467177
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14944197730600143082
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 158;
{$ENDIF}
{$ENDIF}
end;

function DivBy159(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2620200175
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14850209065627815138
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 159;
{$ENDIF}
{$ENDIF}
end;

function DivBy160(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3435973837
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14757395258967641293
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 160;
{$ENDIF}
{$ENDIF}
end;

function DivBy161(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3414632385
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 10884724763990108097
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 161;
{$ENDIF}
{$ENDIF}
end;

function DivBy162(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3393554407
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 10703666314374678099
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 162;
{$ENDIF}
{$ENDIF}
end;

function DivBy163(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3372735055
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14485786757268850349
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 163;
{$ENDIF}
{$ENDIF}
end;

function DivBy164(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3352169597
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14397458789236723213
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 164;
{$ENDIF}
{$ENDIF}
end;

function DivBy165(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3331853418
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14310201463241349133
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 165;
{$ENDIF}
{$ENDIF}
end;

function DivBy166(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3311782012
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14223995430330256668
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 166;
{$ENDIF}
{$ENDIF}
end;

function DivBy167(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3291950982
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 14138821804998937766
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 167;
{$ENDIF}
{$ENDIF}
end;

function DivBy168(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2249744775
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 9662580229085955609
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 168;
{$ENDIF}
{$ENDIF}
end;

function DivBy169(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3252992982
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 9496252866347520655
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 169;
{$ENDIF}
{$ENDIF}
end;

function DivBy170(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3233857729
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13889313184910721217
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 170;
{$ENDIF}
{$ENDIF}
end;

function DivBy171(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2134925265
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13808089131197793023
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 171;
{$ENDIF}
{$ENDIF}
end;

function DivBy172(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3196254732
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13727809543225712831
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 172;
{$ENDIF}
{$ENDIF}
end;

function DivBy173(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 2060591247
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13648458042975853219
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 173;
{$ENDIF}
{$ENDIF}
end;

function DivBy174(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3159516172
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13570018628935762109
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 174;
{$ENDIF}
{$ENDIF}
end;

function DivBy175(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3141461794
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13492475665341843468
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 175;
{$ENDIF}
{$ENDIF}
end;

function DivBy176(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3123612579
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13415813871788764812
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 176;
{$ENDIF}
{$ENDIF}
end;

function DivBy177(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1916962805
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13340018313191088175
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 177;
{$ENDIF}
{$ENDIF}
end;

function DivBy178(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3088515809
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 8083404706456994529
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 178;
{$ENDIF}
{$ENDIF}
end;

function DivBy179(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1847555765
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13190967829244819033
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 179;
{$ENDIF}
{$ENDIF}
end;

function DivBy180(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1813430637
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 13117684674637903372
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 180;
{$ENDIF}
{$ENDIF}
end;

function DivBy181(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3037324939
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7643678483581305919
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 181;
{$ENDIF}
{$ENDIF}
end;

function DivBy182(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1746305385
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7500324513486301207
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 182;
{$ENDIF}
{$ENDIF}
end;

function DivBy183(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 3004130131
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7358537253446979607
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 183;
{$ENDIF}
{$ENDIF}
end;

function DivBy184(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2987803337
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 7218291159277650633
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 184;
{$ENDIF}
{$ENDIF}
end;

function DivBy185(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1648338801
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12763152656404446524
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 185;
{$ENDIF}
{$ENDIF}
end;

function DivBy186(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2955676419
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6942323038492842007
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 186;
{$ENDIF}
{$ENDIF}
end;

function DivBy187(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2939870663
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12626648349918837470
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 187;
{$ENDIF}
{$ENDIF}
end;

function DivBy188(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2924233053
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6672226579852391011
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 188;
{$ENDIF}
{$ENDIF}
end;

function DivBy189(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1522554545
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12493033023464669878
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 189;
{$ENDIF}
{$ENDIF}
end;

function DivBy190(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1491936009
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 6407816362446475825
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 190;
{$ENDIF}
{$ENDIF}
end;

function DivBy191(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2878302691
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12362215923742526738
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 191;
{$ENDIF}
{$ENDIF}
end;

function DivBy192(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2863311531
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12297829382473034411
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 192;
{$ENDIF}
{$ENDIF}
end;

function DivBy193(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2848475720
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12234110059247785528
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 193;
{$ENDIF}
{$ENDIF}
end;

function DivBy194(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1372618415
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5895351198814392785
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 194;
{$ENDIF}
{$ENDIF}
end;

function DivBy195(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1343553873
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 12108632007358064651
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 195;
{$ENDIF}
{$ENDIF}
end;

function DivBy196(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2804876602
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5646962471543740291
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 196;
{$ENDIF}
{$ENDIF}
end;

function DivBy197(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1286310003
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11985701733171688360
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 197;
{$ENDIF}
{$ENDIF}
end;

function DivBy198(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2776544515
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5403591698359363605
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 198;
{$ENDIF}
{$ENDIF}
end;

function DivBy199(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2762592030
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5283740764831379107
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 199;
{$ENDIF}
{$ENDIF}
end;

function DivBy200(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2748779070
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5165088340638674453
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 200;
{$ENDIF}
{$ENDIF}
end;

function DivBy201(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2735103552
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 5047616537582215617
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 201;
{$ENDIF}
{$ENDIF}
end;

function DivBy202(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1148159575
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 4931307821684731621
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 202;
{$ENDIF}
{$ENDIF}
end;

function DivBy203(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2708156719
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11631444539087796093
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 203;
{$ENDIF}
{$ENDIF}
end;

function DivBy204(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2694881441
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11574427654092267681
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 204;
{$ENDIF}
{$ENDIF}
end;

function DivBy205(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2681735678
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11517967031389378570
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 205;
{$ENDIF}
{$ENDIF}
end;

function DivBy206(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 1042467791
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 4477365066434357189
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 206;
{$ENDIF}
{$ENDIF}
end;

function DivBy207(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2655825188
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 4366620577834628161
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 207;
{$ENDIF}
{$ENDIF}
end;

function DivBy208(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2643056798
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11351842506898185610
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 208;
{$ENDIF}
{$ENDIF}
end;

function DivBy209(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2630410593
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11297527470980012473
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 209;
{$ENDIF}
{$ENDIF}
end;

function DivBy210(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 940802361
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11243729721118202890
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 210;
{$ENDIF}
{$ENDIF}
end;

function DivBy211(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2605477791
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 11190441902534704298
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 211;
{$ENDIF}
{$ENDIF}
end;

function DivBy212(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 891408307
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3828569524732171091
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 212;
{$ENDIF}
{$ENDIF}
end;

function DivBy213(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2581013211
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3723990587650285069
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 213;
{$ENDIF}
{$ENDIF}
end;

function DivBy214(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 842937507
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3620389023812154991
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 214;
{$ENDIF}
{$ENDIF}
end;

function DivBy215(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2557003786
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10982247634580570265
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 215;
{$ENDIF}
{$ENDIF}
end;

function DivBy216(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 795364315
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10931403895531586143
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 216;
{$ENDIF}
{$ENDIF}
end;

function DivBy217(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 771906565
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10881028762372454410
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 217;
{$ENDIF}
{$ENDIF}
end;

function DivBy218(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 748664025
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10831115786398268839
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 218;
{$ENDIF}
{$ENDIF}
end;

function DivBy219(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 725633745
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10781658636688687703
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 219;
{$ENDIF}
{$ENDIF}
end;

function DivBy220(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 702812831
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 3018558121152472083
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 220;
{$ENDIF}
{$ENDIF}
end;

function DivBy221(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 680198441
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10684087065315939398
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 221;
{$ENDIF}
{$ENDIF}
end;

function DivBy222(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 657787785
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2825177020297859257
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 222;
{$ENDIF}
{$ENDIF}
end;

function DivBy223(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 635578121
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2729787239607243065
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 223;
{$ENDIF}
{$ENDIF}
end;

function DivBy224(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 613566757
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2635249153387078803
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 224;
{$ENDIF}
{$ENDIF}
end;

function DivBy225(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2443359173
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2541551405711093779
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 225;
{$ENDIF}
{$ENDIF}
end;

function DivBy226(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 570128403
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10447713457676206225
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 226;
{$ENDIF}
{$ENDIF}
end;

function DivBy227(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 548696263
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10401688288259130427
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 227;
{$ENDIF}
{$ENDIF}
end;

function DivBy228(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 527452125
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10356066848398344767
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 228;
{$ENDIF}
{$ENDIF}
end;

function DivBy229(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2400680410
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10310843849060360729
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 229;
{$ENDIF}
{$ENDIF}
end;

function DivBy230(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 485518043
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 2085284112680210183
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 230;
{$ENDIF}
{$ENDIF}
end;

function DivBy231(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 464823301
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10221572473743820809
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 231;
{$ENDIF}
{$ENDIF}
end;

function DivBy232(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2369637129
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1908283869694091547
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 232;
{$ENDIF}
{$ENDIF}
end;

function DivBy233(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 423966729
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1820923234743861319
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 233;
{$ENDIF}
{$ENDIF}
end;

function DivBy234(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 403800345
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10090526672798387209
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 234;
{$ENDIF}
{$ENDIF}
end;

function DivBy235(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 383805589
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1648432449140002485
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 235;
{$ENDIF}
{$ENDIF}
end;

function DivBy236(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2329473788
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 10005013734893316131
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 236;
{$ENDIF}
{$ENDIF}
end;

function DivBy237(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 344322273
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1478852900423972493
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 237;
{$ENDIF}
{$ENDIF}
end;

function DivBy238(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2309898378
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1395131904734335837
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 238;
{$ENDIF}
{$ENDIF}
end;

function DivBy239(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2300233531
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1312111503150888609
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 239;
{$ENDIF}
{$ENDIF}
end;

function DivBy240(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2290649225
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9838263505978427529
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 240;
{$ENDIF}
{$ENDIF}
end;

function DivBy241(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2281144456
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9797440835829139448
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 241;
{$ENDIF}
{$ENDIF}
end;

function DivBy242(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 248469183
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 1067167012528651747
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 242;
{$ENDIF}
{$ENDIF}
end;

function DivBy243(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2262369605
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 986862851679934861
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 243;
{$ENDIF}
{$ENDIF}
end;

function DivBy244(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2253097598
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 907216921657846801
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 244;
{$ENDIF}
{$ENDIF}
end;

function DivBy245(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 192835267
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9637482618101316763
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 245;
{$ENDIF}
{$ENDIF}
end;

function DivBy246(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 174592167
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 749867645272746001
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 246;
{$ENDIF}
{$ENDIF}
end;

function DivBy247(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 156496785
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9559446321598472093
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 247;
{$ENDIF}
{$ENDIF}
end;

function DivBy248(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 138547333
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 595056260442243601
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 248;
{$ENDIF}
{$ENDIF}
end;

function DivBy249(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 120742053
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9482663620220171112
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 249;
{$ENDIF}
{$ENDIF}
end;

function DivBy250(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2199023256
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 442721857769029239
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 250;
{$ENDIF}
{$ENDIF}
end;

function DivBy251(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2190262207
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 367465021388636487
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 251;
{$ENDIF}
{$ENDIF}
end;

function DivBy252(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 68174085
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 292805461487453201
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 252;
{$ENDIF}
{$ENDIF}
end;

function DivBy253(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2172947881
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 218736095735686383
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 253;
{$ENDIF}
{$ENDIF}
end;

function DivBy254(x: Cardinal): Cardinal;
{$IFDEF ASM32}
asm
  push eax
  mov ecx, 33818641
  mul ecx
  pop eax
  sub eax, edx
  shr eax, 1
  add eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
asm
  push rcx
  mov rax, 145249953336295683
  mul rcx
  pop rax
  sub rax, rdx
  shr rax, 1
  add rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 254;
{$ENDIF}
{$ENDIF}
end;

function DivBy255(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2155905153
  mul ecx
  mov eax, edx
  shr eax, 7
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9259542123273814145
  mul rcx
  mov rax, rdx
  shr rax, 7
{$ELSE}
begin
  Result := x div 255;
{$ENDIF}
{$ENDIF}
end;

function DivBy256(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  shr eax, 8
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, rcx
  shr rax, 8
{$ELSE}
begin
  Result := x div 256;
{$ENDIF}
{$ENDIF}
end;

function DivBy510(x: Cardinal): Cardinal;
{$IFDEF ASM32}
  assembler; nostackframe;
asm
  mov ecx, 2155905153
  mul ecx
  mov eax, edx
  shr eax, 8
{$ELSE}
{$IFDEF ASM64}
  assembler; nostackframe;
asm
  mov rax, 9259542123273814145
  mul rcx
  mov rax, rdx
  shr rax, 8
{$ELSE}
begin
  Result := x div 510;
{$ENDIF}
{$ENDIF}
end;

end.
