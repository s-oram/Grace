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
const
  X : single = 12;
asm
  movups XMM0,[eax] //move four singles into the first XMM register
  movups [edx],XMM0 //move four singles to Data2
end;

procedure TestFunction6(Data1:psingle; Data2 : pinteger);
const
  X : single = 12;
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

procedure RunProgram;
var
  x1, x2 : single;
  d1, d2 : array[0..7] of single;
  da1, da2 : array[0..7] of integer;
  c1: Integer;
  counter : TPerformanceTimer;

  WorkA, WorkB : TProc;
  Reset : TProc;
begin

  Reset := procedure
  begin
    d1[0] := random * 100;
    d1[1] := random * 100;
    d1[2] := random * 100;
    d1[3] := random * 100;
  end;

  WorkA := procedure
  var
    c1 : integer;
  begin
    for c1 := 0 to 1000 do
    begin
      TestFunction6(@d1[0], @da2[0]);
    end;
  end;

  WorkB := procedure
  var
    c1 : integer;
  begin
    for c1 := 0 to 1000 do
    begin
      TestFunction7(@d1[0], @da2[0]);
    end;
  end;


  x1 := 10;
  Testfunction(x1, x2);

  for c1 := 0 to 7 do
  begin
    d1[c1] := random * 100;
  end;

  for c1 := 0 to 7 do
  begin
    WriteLn(FloatToStr(d1[c1]));
  end;

  {
  TestFunction5(@d1[0], @d2[0]);

  for c1 := 0 to 7 do
  begin
    WriteLn(IntToStr(Round(d2[c1])));
  end;
  }

  WriteLn('====');

  counter.Start;

  TestFunction6(@d1[0], @da2[0]);

  WriteLn(FloatToStr(Counter.Stop*1000));

  WriteLn('====');

  for c1 := 0 to 7 do
  begin
    WriteLn(IntToStr(da2[c1]));
  end;


  WriteLn('====');

  WriteLn(CalcPerformanceDifference(WorkA, WorkB, Reset));



  //WriteLn(IntToStr(Round(x2)));

  //WriteLn('bang');
  ReadLn;
end;


end.
