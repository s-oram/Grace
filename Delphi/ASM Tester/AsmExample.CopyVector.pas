unit AsmExample.CopyVector;

interface


procedure RunExample01;

const
  kDataSize = 2;

type
  TQuadData = record
    Data : array[0..kDataSize-1] of double;
  end;


  PDoubleX2 = ^TDoubleX2;
  TDoubleX2 = packed record
    Vector : array[0..kDataSize-1] of double;
  end;



procedure WorkA;
procedure WorkB;

implementation

uses
  SysUtils;

procedure DoWork(var QD:TQuadData);
asm
  movupd XMM0, [QD].TQuadData.Data[0] //move 128 bits to XMM0
  movupd XMM1, [QD].TQuadData.Data[0] //move 128 bits to XMM1

  mulpd XMM0, XMM1

  movupd [QD].TQuadData.Data[0], XMM0
end;

procedure DoWork3(var QD:TDoubleX2);
asm
  movupd XMM0, [QD].TDoubleX2.Vector[0] //move 128 bits to XMM0
  movupd XMM1, [QD].TDoubleX2.Vector[0] //move 128 bits to XMM1

  mulpd XMM0, XMM1

  movupd [QD].TQuadData.Data[0], XMM0
end;

procedure DoWork4(const QD:PDoubleX2);
asm
  movupd XMM0, [QD].TDoubleX2.Vector[0] //move 128 bits to XMM0
  movupd XMM1, [QD].TDoubleX2.Vector[0] //move 128 bits to XMM1

  mulpd XMM0, XMM1

  movupd [QD].TQuadData.Data[0], XMM0
end;

procedure DoWork4_ALT(const QD:PDoubleX2);
begin
  QD^.Vector[0] := QD^.Vector[0] * QD^.Vector[0];
  QD^.Vector[1] := QD^.Vector[1] * QD^.Vector[1];
end;


procedure DoWork2(var QD:TQuadData);
var
  dx : PDouble;
begin
  dx := @QD.Data[0];
  dx^ := dx^ * 2;
  //mov ecx, ptr [eax].TQuadData.Data[0]

end;





procedure RunExample01;
const
  D2Length = 16;
var
  Data2 : array[0..D2Length-1] of double;
  Data : TDoubleX2;
  PData : PDoubleX2;
  x : double;
  s : string;
  c1 : integer;
begin
  WriteLn('Example 1');

  for c1 := 0 to D2Length-1 do
  begin
    Data2[c1] := 4;
  end;

  for c1 := 0 to kDataSize-1 do
  begin
    Data.Vector[c1] := 4;
  end;

  //DoWork3(Data);
  //PData := @Data;

  PData := PDoubleX2(@Data2[2]);

  DoWork4_ALT(PData);

  for c1 := 0 to kDataSize-1 do
  begin
    x := Data.Vector[c1];
    s := FloatToStr(x);
    WriteLn(s);
  end;

  WriteLn('=========');

  for c1 := 0 to D2Length-1 do
  begin
    x := Data2[c1];
    s := FloatToStr(x);
    WriteLn(s);
  end;

  WriteLn('=========');
  WriteLn('Finished');
end;


procedure WorkA;
const
  D2Length = 1024;
var
  Data2 : array[0..D2Length-1] of double;
  PData : PDoubleX2;
  c1 : integer;
  x : double;
  s : string;
begin
  for c1 := 0 to D2Length-1 do
  begin
    Data2[c1] := c1;
  end;

  for c1 := 0 to D2Length div 2 - 1 do
  begin
    PData := PDoubleX2(@Data2[c1 * 2]);
    DoWork4(PData);
  end;

  {
  WriteLn('=========');
  for c1 := 0 to D2Length-1 do
  begin
    x := Data2[c1];
    s := FloatToStr(x);
    WriteLn(s);
  end;
  WriteLn('=========');
  }
end;

procedure WorkB;
const
  D2Length = 1024;
var
  Data2 : array[0..D2Length-1] of double;
  PData : PDoubleX2;
  c1 : integer;
  x : double;
  s : string;
begin
  for c1 := 0 to D2Length-1 do
  begin
    Data2[c1] := c1;
  end;

  for c1 := 0 to D2Length div 2 - 1 do
  begin
    PData := PDoubleX2(@Data2[c1 * 2]);
    DoWork4_ALT(PData);
  end;

  {
  WriteLn('=========');
  for c1 := 0 to D2Length-1 do
  begin
    x := Data2[c1];
    s := FloatToStr(x);
    WriteLn(s);
  end;
  WriteLn('=========');
  }
end;

end.
