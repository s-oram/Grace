unit eeSampleFloatFunctions;

interface

uses
  eeSampleFloat, eeSampleInt;

procedure AssignFrom(Dest:TSampleFloat; Source:TSampleInt); overload;
procedure AssignFrom(Dest:TSampleFloat; Source:TSampleFloat); overload;

implementation

procedure AssignFrom(Dest:TSampleFloat; Source:TSampleInt);
const
  ScaleFactor = 1 / 32767;
var
  ch, sampleframes, sr, bits:integer;
  c1: Integer;
begin
  ch           := Source.Properties.ChannelCount;
  SampleFrames := Source.Properties.SampleFrames;
  sr           := Source.Properties.SampleRate;
  Bits         := Source.Properties.SourceBitDepth;

  Dest.Init(Ch, sampleframes, sr, bits);

  if ch = 1 then
  begin
    for c1 := 0 to SampleFrames - 1 do
    begin
      Dest.Ch1[c1] := Source.Ch1[c1] * ScaleFactor;
    end;
  end;

  if ch = 2 then
  begin
    for c1 := 0 to SampleFrames - 1 do
    begin
      Dest.Ch1[c1] := Source.Ch1[c1] * ScaleFactor;
      Dest.Ch2[c1] := Source.Ch2[c1] * ScaleFactor;
    end;
  end;

end;

procedure AssignFrom(Dest:TSampleFloat; Source:TSampleFloat); overload;
var
  ch, sampleframes, sr, bits:integer;
  c1: Integer;
begin
  ch           := Source.Properties.ChannelCount;
  SampleFrames := Source.Properties.SampleFrames;
  sr           := Source.Properties.SampleRate;
  Bits         := Source.Properties.SourceBitDepth;

  Dest.Init(Ch, sampleframes, sr, bits);

  if ch = 1 then
  begin
    for c1 := 0 to SampleFrames - 1 do
    begin
      Dest.Ch1[c1] := Source.Ch1[c1];
    end;
  end;

  if ch = 2 then
  begin
    for c1 := 0 to SampleFrames - 1 do
    begin
      Dest.Ch1[c1] := Source.Ch1[c1];
      Dest.Ch2[c1] := Source.Ch2[c1];
    end;
  end;

end;

end.
