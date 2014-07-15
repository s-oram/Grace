unit SampleOscUtils;

interface

uses
  eeDsp.Interpolation, eePitch, Lucidity.SampleMap, eeSampleFloat, Math;


function CalcSampleStepSize(const TriggerNote, RootNote:Byte; const PitchOne, PitchTwo : single):single; overload;
function CalcSampleStepSize(const TriggerNote, RootNote:Byte; const PitchOne, PitchTwo, SamplePitchOffset : single):single; overload;

function CalcPitchShift(const TriggerNote, RootNote : byte; const PitchOne, PitchTwo, SamplePitchOffset : single): single;

procedure ReadValuesFromSample_LinearInterpolation(const aSample:TSampleFloat; const ReadIndex:cardinal; const ReadIndexFrac:single; Out Out1, Out2 : single); //inline;
procedure ReadValuesFromSample_4x3_Optimal(const aSample:TSampleFloat; const ReadIndex:cardinal; const ReadIndexFrac:single; Out Out1, Out2 : single); //inline;

implementation

function CalcSampleStepSize(const TriggerNote, RootNote:Byte; const PitchOne, PitchTwo : single):single;
var
  SemiToneStep : single;
begin
  SemiToneStep := TriggerNote - RootNote + round(PitchOne * 12) + PitchTwo;
  result := SemiToneShiftToStepSize(SemiToneStep, 1);
end;

function CalcSampleStepSize(const TriggerNote, RootNote:Byte; const PitchOne, PitchTwo, SamplePitchOffset : single):single;
var
  SemiToneStep : single;
begin
  SemiToneStep := TriggerNote - RootNote + round(PitchOne * 12) + PitchTwo + SamplePitchOffset;
  result := SemiToneShiftToStepSize(SemiToneStep, 1);
end;

function CalcPitchShift(const TriggerNote, RootNote : byte; const PitchOne, PitchTwo, SamplePitchOffset : single): single;
var
  SemiToneStep : single;
begin
  SemiToneStep := TriggerNote - RootNote + round(PitchOne * 12) + PitchTwo + SamplePitchOffset;
  result := SemiToneStep;
end;


procedure ReadValuesFromSample_LinearInterpolation(const aSample:TSampleFloat; const ReadIndex:cardinal; const ReadIndexFrac:single; Out Out1, Out2 : single); inline;
var
  x1, x2 : single;
begin
  if aSample.Properties.ChannelCount = 1 then
  begin
    x1 := aSample.Ch1[ReadIndex];
    x2 := aSample.Ch1[ReadIndex + 1];

    Out1 := Optimal2x2Point3rdOrder(ReadIndexFrac, x1, x2);
    Out2 := Out1;

    //Out1 := LinearInterpolation(x1, x2, ReadIndexFrac);
    //Out2 := Out1;
  end else
  begin
    x1 := aSample.Ch1[ReadIndex];
    x2 := aSample.Ch1[ReadIndex + 1];

    //Out1 := LinearInterpolation(x1, x2, ReadIndexFrac);
    Out1 := Optimal2x2Point3rdOrder(ReadIndexFrac, x1, x2);


    x1 := aSample.Ch2[ReadIndex];
    x2 := aSample.Ch2[ReadIndex + 1];

    //Out2 := LinearInterpolation(x1, x2, ReadIndexFrac);
    Out2 := Optimal2x2Point3rdOrder(ReadIndexFrac, x1, x2);
  end;
end;

procedure ReadValuesFromSample_4x3_Optimal(const aSample:TSampleFloat; const ReadIndex:cardinal; const ReadIndexFrac:single; Out Out1, Out2 : single);
// 4-Point, 3-Order Optimal interpolation
var
  x1, x2, x3, x4 : single;
begin
  assert(ReadIndex < cardinal(aSample.Properties.SampleFrames-4));

  if aSample.Properties.ChannelCount = 1 then
  begin
    x1 := aSample.Ch1[ReadIndex];
    x2 := aSample.Ch1[ReadIndex + 1];
    x3 := aSample.Ch1[ReadIndex + 2];
    x4 := aSample.Ch1[ReadIndex + 3];


    //Out1 := Linear(ReadIndexFrac, x2, x3);
    Out1 := Optimal4x3(ReadIndexFrac, x1, x2, x3, x4);
    Out2 := Out1;
  end else
  begin
    x1 := aSample.Ch1[ReadIndex];
    x2 := aSample.Ch1[ReadIndex + 1];
    x3 := aSample.Ch1[ReadIndex + 2];
    x4 := aSample.Ch1[ReadIndex + 3];

    //Out1 := Linear(ReadIndexFrac, x2, x3);
    Out1 := Optimal4x3(ReadIndexFrac, x1, x2, x3, x4);

    x1 := aSample.Ch2[ReadIndex];
    x2 := aSample.Ch2[ReadIndex + 1];
    x3 := aSample.Ch2[ReadIndex + 2];
    x4 := aSample.Ch2[ReadIndex + 3];

    //Out2 := Linear(ReadIndexFrac, x2, x3);
    Out2 := Optimal4x3(ReadIndexFrac, x1, x2, x3, x4);
  end;
end;




end.

