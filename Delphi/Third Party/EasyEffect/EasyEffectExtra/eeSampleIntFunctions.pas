unit eeSampleIntFunctions;

interface

uses
  eeSampleInt;

function MakeStereo(Source:TSampleInt):TSampleInt;

implementation

function MakeStereo(Source:TSampleInt):TSampleInt;
var
  NewSample:TSampleInt;
  c1: Integer;
begin
  NewSample := TSampleInt.Create;
  try
    //Try to get sample data memory.
    if NewSample.Init(2, Source.Properties.SampleFrames, Source.Properties.SampleRate, Source.Properties.SourceBitDepth) = false then
    begin
      NewSample.Free;
      NewSample := nil;
      exit; //==============>>
    end;

    for c1 := 0 to Source.Properties.SampleFrames - 1 do
    begin
      NewSample.Ch1[c1] := Source.Ch1[c1];
      NewSample.Ch2[c1] := Source.Ch1[c1];
    end;
  finally
    Source.Free;
    result := NewSample;
  end;
end;

end.
