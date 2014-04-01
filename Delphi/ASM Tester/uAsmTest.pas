unit uAsmTest;

interface

uses
  VamLib.MoreTypes;

procedure RunProgram;



implementation

uses
  AsmExample.CopyVector,
  VamLib.PerformanceTuning,
  Windows,
  SysUtils,
  Vcl.Dialogs;



procedure TestFunction(Data:Single; out Result : single);
begin
  result := Data;
end;

procedure TestFunction2(Data:Single; out Result : single);
asm
  mov eax, ebx
end;

procedure TestFunction3Pure(Data1:psingle; Data2 : psingle);
begin
  Data2^ := Data1^;
end;

procedure TestFunction3(Data1:psingle; Data2 : psingle);
const
  X : single = 12;
asm
  {
  push ebp
  mov ebp, esp
  add esp,-$08
  mov [ebp-$08],edx
  mov [ebp-$04],eax

// Data2^ := Data1^;
  mov eax,[ebp-$04]
  mov eax,[eax]
  mov edx,[ebp-$08]
  mov [edx],eax


  pop ecx
  pop ecx
  pop ebp
  ret
  }


  //push ebp
  //mov ebp, esp
  //add esp,-$08
  //mov [ebp-$08],edx
  //mov [ebp-$04],eax

  // Data2^ := Data1^;
  //mov eax,[ebp-$04]
  mov eax,[eax]
  //mov edx,[ebp-$08]
  mov [edx],eax


  //pop ecx
  //pop ecx
  //pop ebp
  //ret


  //mov [edx], X

end;

procedure TestFunction4(Data1:psingle; Data2 : psingle);
const
  X : single = 12;
asm
  mov eax,[eax]
  mov [edx],eax
end;

procedure TestFunction5(Data1:psingle; Data2 : psingle);
asm
  movups XMM0,[eax] //move four singles into the first XMM register
  movups [edx],XMM0 //move four singles to Data2
end;

procedure TestFunction6(Data1:psingle; Data2 : pinteger);
asm
  movups XMM2,[eax] //move four singles into the first XMM register
  CVTTPS2DQ xmm1, xmm2
  movups [edx],XMM1 //move four packed values to Data2
end;

procedure TestFunction7(Data1:psingle; Data2 : pinteger);
begin
  Data2^ := round(Data1^);
  inc(Data1);
  inc(Data2);
  Data2^ := round(Data1^);
  inc(Data1);
  inc(Data2);
  Data2^ := round(Data1^);
  inc(Data1);
  inc(Data2);
  Data2^ := round(Data1^);
end;



function CalcSummedModulationValue(ModSlotValues, ModAmountValues : PSingle):single;
var
  x : single;
  c2: Integer;
begin
  //TODO: this probably can be optimised. It would be a good candiate for a SSE vector operation.
  x := 0;
  for c2 := 0 to 7 do
  begin
    x := x + ModSlotValues^ * ModAmountValues^;
    inc(ModSlotValues);
    inc(ModAmountValues);
  end;
  result := x;
end;

function CalcSummedModulationValue_asm(ModSlotValues, ModAmountValues : PSingle):single;
var
  x : array[0..7] of single;
asm
{
//uAsmTest.pas.126: begin
push ebp
mov ebp,esp
add esp,-$14
mov [ebp-$08],edx
mov [ebp-$04],eax
//uAsmTest.pas.128: x := 0;
xor eax,eax
mov [ebp-$10],eax
//uAsmTest.pas.129: for c2 := 0 to 7 do
xor eax,eax
mov [ebp-$14],eax
//uAsmTest.pas.131: x := x + ModSlotValues^ * ModAmountValues^;
mov eax,[ebp-$04]
fld dword ptr [eax]
mov eax,[ebp-$08]
fmul dword ptr [eax]
fadd dword ptr [ebp-$10]
fstp dword ptr [ebp-$10]
wait
//uAsmTest.pas.132: inc(ModSlotValues);
add dword ptr [ebp-$04],$04
//uAsmTest.pas.133: inc(ModAmountValues);
add dword ptr [ebp-$08],$04
//uAsmTest.pas.134: end;
inc dword ptr [ebp-$14]
//uAsmTest.pas.129: for c2 := 0 to 7 do
cmp dword ptr [ebp-$14],$08
jnz $00511fda
//uAsmTest.pas.135: result := x;
mov eax,[ebp-$10]
mov [ebp-$0c],eax
//uAsmTest.pas.136: end;
fld dword ptr [ebp-$0c]
mov esp,ebp
pop ebp
}

end;

function CalcSummedModulationValue_asm2(ModSlotValues, ModAmountValues : PSingle):single;
{$IFDEF CPUX64}
asm
  movups XMM0,[rcx]
  movups XMM1,[rcx+$10]

  movups XMM2,[rdx]
  movups XMM3,[rdx+$10]


  mulps XMM0, XMM2
  mulps XMM1, XMM3

  addps XMM0, XMM1

  sub esp, 16
  //movups dqword [esp], xmm0
  movdqu dqword [esp], xmm0

  movss xmm0, [esp]
  addss xmm0, [[esp+$04]]
  addss xmm0, [[esp+$08]]
  addss xmm0, [[esp+$0c]]

  add esp, 16
end;
{$ELSE}
asm
  movups XMM0,[eax]
  movups XMM1,[eax+$10]

  movups XMM2,[edx]
  movups XMM3,[edx+$10]


  mulps XMM0, XMM2
  mulps XMM1, XMM3

  addps XMM0, XMM1

  //movups [ebp-$10],XMM0

  sub esp, 16
  //movups dqword [esp], xmm0
  movdqu dqword [esp], xmm0

  fld dword [[esp]]
  fadd dword [[esp+$04]]
  fadd dword [[esp+$08]]
  fadd dword [[esp+$0C]]

  add esp, 16

  //NOTE: This function doesn't work. It ends up cause access violations.
end;
{$ENDIF}


function Optimal4x3_orig(const f, y0, y1, y2, y3:single):single; //inline;
var
  z ,even1, even2, odd1, odd2 : single;
  c0,c1, c2, c3 : single;
begin
  z := f - 1/2.0;

  even1 := y2+y1;
  odd1  := y2-y1;
  even2 := y3+y0;
  odd2  := y3-y0;

  c0 := even1*0.45868970870461956 + even2*0.04131401926395584;
  c1 := odd1*0.48068024766578432 + odd2*0.17577925564495955;
  c2 := even1*-0.246185007019907091 + even2*0.24614027139700284;
  c3 := odd1*-0.36030925263849456 + odd2*0.10174985775982505;

  result := ((c3*z+c2)*z+c1)*z+c0;
end;




function Optimal4x3(const f, y0, y1, y2, y3:single):single;
const
  PointFive : double = 0.5;
  k01 : double = 0.45868970870461956;
  k02 : double = 0.04131401926395584;
  k11 : double = 0.48068024766578432;
  k12 : double = 0.17577925564495955;
  k21 : double = -0.246185007019907091;
  k22 : double = 0.24614027139700284;
  k31 : double = -0.36030925263849456;
  k32 : double = 0.10174985775982505;
var
  z ,even1, even2, odd1, odd2 : single;
  c0,c1, c2, c3 : single;
begin
  z := f - PointFive;

  even1 := y2+y1;
  odd1  := y2-y1;

  even2 := y3+y0;
  odd2  := y3-y0;


  c0 := even1 * k01 + even2 * k02;
  c1 := odd1  * k11 + odd2  * k12;
  c2 := even1 * k21 + even2 * k22;
  c3 := odd1  * k31 + odd2  * k32;

  result := ((c3*z+c2)*z+c1)*z+c0;

  //result := c3 * z;
end;

function Optimal4x3_asm2(const f, y0, y1, y2, y3:single):single;
// f  = [ebp+$18]
// y0 = [ebp+$14]
// y1 = [ebp+$10]
// y2 = [ebp+$0C]
// y3 = [ebp+$08]
type
  float = double;
const
  PointFive : float = 0.5;
  k01 : float = 0.45868970870461956;
  k02 : float = 0.04131401926395584;
  k11 : float = 0.48068024766578432;
  k12 : float = 0.17577925564495955;
  k21 : float = -0.246185007019907091;
  k22 : float = 0.24614027139700284;
  k31 : float = -0.36030925263849456;
  k32 : float = 0.10174985775982505;
asm
  // make space for local variables.
  sub esp, 4

  // z := f - 1/2.0;
  fld dword [[ebp+$18]]
  fsub [[PointFive]]

  //even1 := y2+y1;
  fld dword ptr [ebp+$0C]
  fadd dword ptr [ebp+$10]

  //odd1  := y2-y1;
  fld dword ptr [ebp+$0C]
  fsub dword ptr [ebp+$10]

  //even2 := y3+y0;
  fld dword ptr [ebp+$08]
  fadd dword ptr [ebp+$14]

  //odd2  := y3-y0;
  fld dword ptr [ebp+$08]
  fsub dword ptr [ebp+$14]

  // The floating point stack is now:
  // st0 = odd2
  // st1 = even2
  // st2 = odd1
  // st3 = even1
  // st4 = z


  //c3 := odd1  * -0.36030925263849456  + odd2  * 0.10174985775982505;


  fld qword [k31]
  // The floating point stack is now:
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z
  fmul st(0), st(3)

  fld qword [k32]
  // The floating point stack is now:
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(2)
  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c3
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z


  fmul st(0), st(5)
  // The floating point stack is now:
  // st0 = c3 * z
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z

  //fst dword ptr [esp]


  //c2 := even1 * -0.246185007019907091 + even2 * 0.24614027139700284;

  fld qword [k21]
  // The floating point stack is now:
  // st1 = c3 * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(5)


  fld qword [k22]
  // The floating point stack is now:
  // st2 = c3 * z
  // st3 = odd2
  // st4 = even2
  // st5 = odd1
  // st6 = even1
  // st7 = z
  fmul st(0), st(4)

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c2
  // st1 = c3 * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z


  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = (c3 * z + c2)
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z

  fmul st(0), st(5)
  // The floating point stack is now:
  // st0 = (c3 * z + c2) * z
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z


  //c1 := odd1  * 0.48068024766578432   + odd2  * 0.17577925564495955;

  fld qword [k11]
  // The floating point stack is now:
  // st1 = (c3 * z + c2) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(4)

  fld qword [k12]
  // The floating point stack is now:
  // st2 = (c3 * z + c2) * z
  // st3 = odd2
  // st4 = even2
  // st5 = odd1
  // st6 = even1
  // st7 = z
  fmul st(0), st(3)

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c1
  // st1 = (c3 * z + c2) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = ((c3 * z + c2) * z + c1)
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z

  fmul st(0), st(5)
  // The floating point stack is now:
  // st0 = ((c3 * z + c2) * z + c1) * z
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z




  //c0 := even1 * 0.45868970870461956   + even2 * 0.04131401926395584;

  fld qword [k01]
  // The floating point stack is now:
  // st1 = ((c3 * z + c2) * z + c1) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(5)

  fld qword [k02]
  // The floating point stack is now:
  // st2 = ((c3 * z + c2) * z + c1) * z
  // st3 = odd2
  // st4 = even2
  // st5 = odd1
  // st6 = even1
  // st7 = z
  fmul st(0), st(4)

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c0
  // st1 = ((c3 * z + c2) * z + c1) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z


  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = ((c3 * z + c2) * z + c1) * z + c0
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z


  fxch st(5)

  fstp dword ptr [esp]
  fstp dword ptr [esp]
  fstp dword ptr [esp]
  fstp dword ptr [esp]
  fstp dword ptr [esp]
  //fstp dword ptr [esp + $04]

  //fld dword ptr [esp]
  //fld dword ptr [esp + $08]

  //=========================================
  // The result is now in st0
  // result := ((c3*z+c2)*z+c1)*z+c0;
  //=========================================





  // free local variable space.
  add esp, 4

end;



function Optimal4x3_asm(f:single):single;
const
  Foo : single = 3.14;
asm
  //sub esp, 4
  //mov [esp], eax
  //fld dword ptr [esp]
  //add esp, 4

  //uAsmTest.pas.279: begin
  //push ebp
  //mov ebp,esp
  //push ecx
  //uAsmTest.pas.280: result := f;
  //mov eax,[ebp+$18]
  //mov [ebp-$04],eax
  //uAsmTest.pas.281: end;
  //fld dword [[ebp-$04]]
  //pop ecx
  //pop ebp
  //ret $0014


  fld dword [[ebp+$08]]
end;


function Optimal4x3_asm3(f:single):single;
begin
  result := f;
end;


{
begin
  result := f;
end;
}


  {
  //uAsmTest.pas.247: begin
  push ebp
  mov ebp,esp
  add esp,-$28
  //uAsmTest.pas.248: z := f - 1/2.0;
  fld dword ptr [ebp+$18]
  fsub dword ptr [$005124a0]
  fstp dword ptr [ebp-$08]
  wait
  //uAsmTest.pas.250: even1 := y2+y1;
  fld dword ptr [ebp+$0c]
  fadd dword ptr [ebp+$10]
  fstp dword ptr [ebp-$0c]
  wait
  //uAsmTest.pas.251: odd1  := y2-y1;
  fld dword ptr [ebp+$0c]
  fsub dword ptr [ebp+$10]
  fstp dword ptr [ebp-$14]
  wait
  //uAsmTest.pas.253: even2 := y3+y0;
  fld dword ptr [ebp+$08]
  fadd dword ptr [ebp+$14]
  fstp dword ptr [ebp-$10]
  wait
  //uAsmTest.pas.254: odd2  := y3-y0;
  fld dword ptr [ebp+$08]
  fsub dword ptr [ebp+$14]
  fstp dword ptr [ebp-$18]
  wait
  //uAsmTest.pas.257: c0 := even1*0.45868970870461956 + even2*0.04131401926395584;
  fld tbyte ptr [$005124a4]
  fmul dword ptr [ebp-$0c]
  fld tbyte ptr [$005124b0]
  fmul dword ptr [ebp-$10]
  faddp st(1)
  fstp dword ptr [ebp-$1c]
  wait
  //uAsmTest.pas.258: c1 := odd1*0.48068024766578432 + odd2*0.17577925564495955;
  fld tbyte ptr [$005124bc]
  fmul dword ptr [ebp-$14]
  fld tbyte ptr [$005124c8]
  fmul dword ptr [ebp-$18]
  faddp st(1)
  fstp dword ptr [ebp-$20]
  wait
  //uAsmTest.pas.259: c2 := even1*-0.246185007019907091 + even2*0.24614027139700284;
  fld tbyte ptr [$005124d4]
  fmul dword ptr [ebp-$0c]
  fld tbyte ptr [$005124e0]
  fmul dword ptr [ebp-$10]
  faddp st(1)
  fstp dword ptr [ebp-$24]
  wait
  //uAsmTest.pas.260: c3 := odd1*-0.36030925263849456 + odd2*0.10174985775982505;
  fld tbyte ptr [$005124ec]
  fmul dword ptr [ebp-$14]
  fld tbyte ptr [$005124f8]
  fmul dword ptr [ebp-$18]
  faddp st(1)
  fstp dword ptr [ebp-$28]
  wait
  //uAsmTest.pas.262: result := ((c3*z+c2)*z+c1)*z+c0;
  fld dword ptr [ebp-$28]
  fmul dword ptr [ebp-$08]
  fadd dword ptr [ebp-$24]
  fmul dword ptr [ebp-$08]
  fadd dword ptr [ebp-$20]
  fmul dword ptr [ebp-$08]
  fadd dword ptr [ebp-$1c]
  fstp dword ptr [ebp-$04]
  wait
  //uAsmTest.pas.263: end;
  fld dword ptr [ebp-$04]
  mov esp,ebp
  pop ebp
  ret
  }




procedure RunProgram;
var
  rx : single;
  x1, x2, x3 : single;
  d1, d2, d3 : array[0..7] of single;
  da1, da2 : array[0..7] of integer;
  y:array[0..3] of single;
  c1: Integer;
  counter : TPerformanceTimer;

  WorkA, WorkB : TProc;
  Reset : TProc;
begin
  AsmExample.CopyVector.WorkA;
  AsmExample.CopyVector.WorkB;


  //WriteLn(CalcPerformanceDifference(uAsmExamples.WorkA, uAsmExamples.WorkB, nil));
  //WriteLn(CalcPerformanceDifference(uAsmExamples.WorkA, uAsmExamples.WorkB, nil));
  //WriteLn(CalcPerformanceDifference(uAsmExamples.WorkA, uAsmExamples.WorkB, nil));
  //WriteLn(CalcPerformanceDifference(uAsmExamples.WorkA, uAsmExamples.WorkB, nil));


  ReadLn;


  {
  Optimal4x3_orig(1, 2, 3, 4, 5);
  Optimal4x3(1, 2, 3, 4, 5);
  Optimal4x3_asm2(1, 2, 3, 4, 5);


  for c1 := 0 to 7 do
  begin
    d1[c1] := random;
    d2[c1] := random;
  end;

  Reset := procedure
  begin
  end;

  WorkA := procedure
  var
    c1 : integer;
  begin
    for c1 := 0 to 1000 do
    begin
      Optimal4x3_orig(0.6,1,2,3,4);
    end;
  end;

  WorkB := procedure
  var
    c1 : integer;
  begin
    for c1 := 0 to 1000 do
    begin
      Optimal4x3(0.6,1,2,3,4);
    end;
  end;

  WriteLn('====');


  rx := random;
  x1 := Optimal4x3_orig(rx, 2, 3, 4, 5);
  x2 := Optimal4x3(rx, 2, 3, 4, 5);
  x3 := Optimal4x3_asm2(rx, 2, 3, 4, 5);

  WriteLn('x1 = ' + FloatToStr(x1));
  WriteLn('x2 = ' + FloatToStr(x2));
  WriteLn('x3 = ' + FloatToStr(x3));


  WriteLn('====');

  WriteLn(CalcPerformanceDifference(WorkA, WorkB, nil));



  // finally
  ReadLn;
  }
end;


end.
