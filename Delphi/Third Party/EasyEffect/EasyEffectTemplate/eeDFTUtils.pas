unit eeDFTUtils;

interface

uses
  VamLib.MoreTypes;

function CalcMagnitude(Bin:TComplex):single; inline; overload;
function CalcMagnitude(const RealPart, ImagPart:single):single; inline; overload;

function CalcPhase(Bin:TComplex):single; inline; overload;
function CalcPhase(const RealPart, ImagPart:single):single; inline; overload;

procedure NormaliseValues(Values:PSingle; Count:integer);

implementation

uses
  Math;

function CalcMagnitude(Bin:TComplex):single;
begin
  result := sqrt((Bin.Real * Bin.Real) + (Bin.Imag * Bin.Imag));
end;

function CalcMagnitude(const RealPart, ImagPart:single):single;
begin
  result := sqrt((RealPart * RealPart) + (ImagPart * ImagPart));
end;

function CalcPhase(Bin:TComplex):single;
var
  x:single;
begin
  x :=  ArcTan(Bin.Imag/Bin.Real);
  result := x; //result in radians
end;

function CalcPhase(const RealPart, ImagPart:single):single;
var
  x:single;
begin
  x :=  ArcTan(ImagPart/RealPart);
  result := x; //result in radians
end;

procedure NormaliseValues(Values:PSingle; Count:integer);
var
  max:single;
  v:PSingle;
  c1:integer;
  ScaleFactor:single;
begin
  max := 0;
  v := Values;

  for c1 := 0 to Count - 1 do
  begin
    if abs(v^) > Max then Max := abs(v^);
    inc(v);
  end;

  if Max = 0 then exit;

  ScaleFactor := 1 / Max;


  v := Values;
  for c1 := 0 to Count - 1 do
  begin
    v^ := v^ * ScaleFactor;
    inc(v);
  end;

end;

end.
