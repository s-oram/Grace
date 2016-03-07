unit AudioToolbox.SampleProcesses;

interface

uses
  VamLib.MoreTypes,
  AudioToolbox.SampleData;

type
  SampleProcess = record
    // convert a mono sample to a stereo sample.
    class procedure ConvertToStereo(const Sample : TSampleData); static;

    // extract a slice from the "source" sample, write to "dest" sample.
    class procedure ExtractSample(const Source, Dest : TSampleData; const StartIndex, SampleFrames : integer); static;
  end;

implementation

uses
  VamDsp.Utils;

{ SampleProcess }

class procedure SampleProcess.ConvertToStereo(const Sample: TSampleData);
var
  Temp : TSampleData;
  In1, Out1, Out2 : PSingle;
  c1: Integer;
begin
  assert(Sample.ChannelConfig = ccMono);

  Temp := TSampleData.Create;
  try
    Temp.GetSampleMem(ccStereo, Sample.SampleFrames);
    Temp.SampleRate := Sample.SampleRate;

    In1 := Sample.Data[0];
    Out1 := Temp.Data[0];
    Out2 := Temp.Data[1];

    for c1 := 0 to Sample.SampleFrames-1 do
    begin
      Out1^ := In1^;
      Out2^ := In1^;
      inc(Out1);
      inc(Out2);
      inc(In1);
    end;

    Sample.AssignFrom(Temp);

  finally
    Temp.Free;
  end;
end;

class procedure SampleProcess.ExtractSample(const Source, Dest: TSampleData; const StartIndex, SampleFrames: integer);
var
  SrcChA,  SrcChB : PSingleDynArray;
  DestChA, DestChB : PSingleDynArray;
  c1: Integer;
begin
  assert(Source.ChannelConfig <> ccNotAssigned);
  assert(SampleFrames > 0);
  assert(assigned(Source));
  assert(assigned(Dest));
  assert(StartIndex >= 0);
  assert(StartIndex + SampleFrames < Source.SampleFrames);

  Dest.GetSampleMem(Source.ChannelConfig, SampleFrames);
  Dest.SampleRate := Source.SampleRate;

  if (Source.ChannelConfig = ccMono) then
  begin
    SrcChA  := Source.ChA;

    DestChA := Dest.ChA;

    for c1 := 0 to SampleFrames-1 do
    begin
      DestChA^[c1] := SrcChA^[StartIndex + c1];
    end;
  end;

  if (Source.ChannelConfig = ccStereo) then
  begin
    SrcChA := Source.ChA;
    SrcChB := Source.ChB;

    DestChA := Dest.ChA;
    DestChB := Dest.ChB;

    for c1 := 0 to SampleFrames-1 do
    begin
      DestChA^[c1] := SrcChA^[StartIndex + c1];
      DestChB^[c1] := SrcChB^[StartIndex + c1];
    end;
  end;
end;

end.
