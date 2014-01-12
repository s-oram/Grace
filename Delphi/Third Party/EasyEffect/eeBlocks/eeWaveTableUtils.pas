unit eeWaveTableUtils;

interface

uses
  VamLib.MoreTypes, eeWaveTable;

procedure GenSine(WaveTable:TWaveTable);

// GenSawA & GenSawB both create a saw wave with a pronouced roll-off of the high harmonics.
// I can't remember what the difference between the functions were.
procedure GenSawA(WaveTable:TWaveTable; Harmonics:integer);
procedure GenSawB(WaveTable:TWaveTable; Harmonics:integer);
procedure GenSawC(WaveTable:TWaveTable; Harmonics:integer); //SawC has a steeper initial harmonic fall-off but then levels out. Inspired a tiny amount by U-he ACE.  

procedure GenSquareA(WaveTable:TWaveTable; Harmonics:integer);


implementation

uses
  Math, eeDft, eeDftUtils;

procedure GenSine(WaveTable:TWaveTable);
var
  c1:integer;
  ph:single;
  x:single;
  TableLength:integer;
begin
  TableLength := WaveTable.TableLength;

  for c1 := 0 to TableLength - 1 do
  begin
    ph := c1 / (TableLength);
    x := sin(ph * 2 * pi);
    WaveTable.WaveData[c1] := x;
  end;

  WaveTable.CalcDeltaValues;
end;

procedure GenSawA(WaveTable:TWaveTable; Harmonics:integer);
var
  Data:TArrayOfSingle;
  ComplexData:TArrayOfComplex;
  c1:integer;
  m:single;TableLength:integer;
begin
  TableLength := WaveTable.TableLength;

  SetLength(Data,TableLength);
  SetLength(ComplexData,TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    ComplexData[c1].Real := 0;
    ComplexData[c1].Imag := 0;
  end;

  m := 1;
  for c1 := 0 to Harmonics-1 do
  begin
    ComplexData[c1 + 1].Imag := m;
    m := m * 0.9;
  end;

  CalcInverseDFT(ComplexData,Data,TableLength);

  NormaliseValues(@Data[0],TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    WaveTable.WaveData[c1] := Data[c1];
  end;

  WaveTable.CalcDeltaValues;

  SetLength(Data,0);
  SetLength(ComplexData,0);

end;

procedure GenSawB(WaveTable:TWaveTable; Harmonics:integer);
var
  Data:TArrayOfSingle;
  ComplexData:TArrayOfComplex;
  c1:integer;
  m:single;TableLength:integer;
begin
  TableLength := WaveTable.TableLength;

  SetLength(Data,TableLength);
  SetLength(ComplexData,TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    ComplexData[c1].Real := 0;
    ComplexData[c1].Imag := 0;
  end;

  m := 1;
  for c1 := 0 to Harmonics-1 do
  begin
    ComplexData[c1 + 1].Imag := m;
    m := m * 0.96;
  end;

  CalcInverseDFT(ComplexData,Data,TableLength);

  NormaliseValues(@Data[0],TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    WaveTable.WaveData[c1] := Data[c1];
  end;

  WaveTable.CalcDeltaValues;

  SetLength(Data,0);
  SetLength(ComplexData,0);

end;

procedure GenSawC(WaveTable:TWaveTable; Harmonics:integer);
var
  Data:TArrayOfSingle;
  ComplexData:TArrayOfComplex;
  c1:integer;
  m:single;TableLength:integer;
begin
  TableLength := WaveTable.TableLength;

  SetLength(Data,TableLength);
  SetLength(ComplexData,TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    ComplexData[c1].Real := 0;
    ComplexData[c1].Imag := 0;
  end;

  for c1 := 0 to Harmonics-1 do
  begin
    if c1 = 0 then
    begin
      m := 0;
    end else
    if Odd(c1) then
    begin
      m := 1/c1;   
    end else
    begin
      m := 0.65/c1;
    end;

    ComplexData[c1].Imag := m;
  end;

  CalcInverseDFT(ComplexData,Data,TableLength);

  NormaliseValues(@Data[0],TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    WaveTable.WaveData[c1] := Data[c1];
  end;

  WaveTable.CalcDeltaValues;

  SetLength(Data,0);
  SetLength(ComplexData,0);

end;

procedure GenSquareA(WaveTable:TWaveTable; Harmonics:integer);
var
  Data:TArrayOfSingle;
  ComplexData:TArrayOfComplex;
  c1:integer;
  m:single;TableLength:integer;
  Index:integer;
begin
  TableLength := WaveTable.TableLength;

  SetLength(Data,TableLength);
  SetLength(ComplexData,TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    ComplexData[c1].Real := 0;
    ComplexData[c1].Imag := 0;
  end;

  for c1 := 0 to (Harmonics div 2) - 1 do
  begin
    Index := c1 * 2 + 1;
    ComplexData[Index].Imag := 1 / Index;
  end;

  CalcInverseDFT(ComplexData,Data,TableLength);

  NormaliseValues(@Data[0],TableLength);

  for c1 := 0 to TableLength - 1 do
  begin
    WaveTable.WaveData[c1] := Data[c1];
  end;

  WaveTable.CalcDeltaValues;

  SetLength(Data,0);
  SetLength(ComplexData,0);

end;





end.
