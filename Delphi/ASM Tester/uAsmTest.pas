unit uAsmTest;

interface

uses
  VamLib.MoreTypes;

procedure RunProgram;



implementation

uses
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



procedure RunProgram;
var
  x1, x2 : single;
  d1, d2, d3 : array[0..7] of single;
  da1, da2 : array[0..7] of integer;
  c1: Integer;
  counter : TPerformanceTimer;

  WorkA, WorkB : TProc;
  Reset : TProc;
begin
  for c1 := 0 to 7 do
  begin
    d1[c1] := random;
    d2[c1] := random;
  end;

  Reset := procedure
  begin
    {
    for c1 := 0 to 7 do
    begin
      d1[c1] := random;
      d2[c1] := random;
    end;
    }
  end;

  WorkA := procedure
  begin
    CalcSummedModulationValue(@d1[0], @d2[0]);
  end;

  WorkB := procedure
  begin
    CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  end;

  WriteLn('====');

  //WriteLn(CalcPerformanceDifference(WorkA, WorkB, Reset));
  WorkB;

  WriteLn('====');


  for c1 := 0 to 7 do
  begin
    d1[c1] := random;
    d2[c1] := random;
  end;


  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);

  counter.Start;
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  WriteLn(FloatToStr(counter.Stop));
  WriteLn(FloatToStr(x1));

  WriteLn('====');

  counter.Start;
  x1 := CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  x1 := CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  WriteLn(FloatToStr(counter.Stop));
  WriteLn(FloatToStr(x1));

  WriteLn('====');

  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  WriteLn(FloatToStr(x1));

  x1 := CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  WriteLn(FloatToStr(x1));


  {
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  WriteLn(FloatToStr(x1));
  x1 := CalcSummedModulationValue_asm(@d1[0], @d2[0]);
  WriteLn(FloatToStr(x1));
  x1 := CalcSummedModulationValue_asm2(@d1[0], @d2[0]);
  WriteLn(FloatToStr(x1));
  x1 := CalcSummedModulationValue(@d1[0], @d2[0]);
  WriteLn(FloatToStr(x1));
  }





  // finally
  ReadLn;
end;


end.
