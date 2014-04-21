unit eeAudioBufferUtils;

interface

uses
  Math,
  VamLib.MoreTypes;

procedure NormaliseBuffer(Input : PSingle; const SampleFrames : integer; const TargetLevel : single);

procedure RemoveDcOffset(Input : PSingle; const SampleFrames : integer);

function FindMinValue(Input : PSingle; const SampleFrames : integer):single;
function FindMaxValue(Input : PSingle; const SampleFrames : integer):single;

function FindDynamicRange(Input : PSingle; const SampleFrames : integer):single; // result is decibels.

procedure ClearBuffer(Input : PSingle; const SampleFrames : integer);

implementation

uses
  eeDsp;

procedure NormaliseBuffer(Input : PSingle; const SampleFrames : integer; const TargetLevel : single);
var
  c1 : integer;
  tps : PSingle;
  MaxLevel : single;
  Gain : double;
  x1 : single;
begin
  tps := Input;
  MaxLevel := 0;
  for c1 := 0 to SampleFrames-1 do
  begin
    x1 := abs(tps^);
    if x1 > MaxLevel then MaxLevel := x1;
    inc(tps);
  end;

  Gain := TargetLevel / MaxLevel;

  for c1 := 0 to SampleFrames-1 do
  begin
    Input^ := Input^ * Gain;
    inc(Input);
  end;

end;


procedure RemoveDcOffset(Input : PSingle; const SampleFrames : integer);
var
  aMinValue, aMaxValue : single;
  CurrentOffset : single;
  OffsetCorrection : single;
  c1 : integer;
begin
  aMinValue := FindMinValue(Input, SampleFrames);
  aMaxValue := FindMaxValue(Input, SampleFrames);

  CurrentOffset := aMinValue + aMaxValue;
  OffsetCorrection := CurrentOffset * 0.5;

  for c1 := 0 to SampleFrames-1 do
  begin
    Input^ := Input^ - OffsetCorrection;
    inc(Input);
  end;
end;

function FindMinValue(Input : PSingle; const SampleFrames : integer):single;
var
  x1 : single;
  c1: Integer;
begin
  x1 := Input^;

  for c1 := 0 to SampleFrames-1 do
  begin
    if Input^ < x1 then x1 := Input^;
    inc(Input);
  end;

  result := x1;
end;

function FindMaxValue(Input : PSingle; const SampleFrames : integer):single;
var
  x1 : single;
  c1: Integer;
begin
  x1 := Input^;

  for c1 := 0 to SampleFrames-1 do
  begin
    if Input^ > x1 then x1 := Input^;
    inc(Input);
  end;

  result := x1;
end;


function FindDynamicRange(Input : PSingle; const SampleFrames : integer):single;
var
  aMinValue, aMaxValue : single;
  Range : single;
begin
  aMinValue := FindMinValue(Input, SampleFrames);
  aMaxValue := FindMaxValue(Input, SampleFrames);

  Range := aMaxValue - aMinValue;

  result := LinearToDecibels(Range);
end;


procedure ClearBuffer(Input : PSingle; const SampleFrames : integer);
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    Input^ := 0;
    inc(Input);
  end;
end;


end.
