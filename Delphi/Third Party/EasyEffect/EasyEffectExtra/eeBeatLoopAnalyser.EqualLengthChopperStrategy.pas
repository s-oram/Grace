unit eeBeatLoopAnalyser.EqualLengthChopperStrategy;

interface


uses
  Contnrs, eeSampleFloat, MoreTypes,
  eeBeatLoopAnalyser.GenericChopperStrategy;

type
  TEqualLengthChopper = class(TGenericChopper)
  private
    fRequiredSliceCount: integer;
  public
    procedure Chop; override;
    property RequiredSliceCount : integer read fRequiredSliceCount write fRequiredSliceCount;
  end;

implementation

uses
  eeBeatLoopAnalyser.CoreTypes, Math;

{ TEqualLengthChopper }

procedure TEqualLengthChopper.Chop;
var
  c1: Integer;
  aSlice : TSliceInfo;
begin
  assert(self.RequiredSliceCount > 0);
  assert(Self.Data.SampleFrames > 0);

  SliceList.Clear;

  //== Create all slices.
  for c1 := 0 to RequiredSliceCount-1 do
  begin
    aSlice.Position := floor(Data.SampleFrames * c1 / RequiredSliceCount);
    aSlice.SampleFrames := floor(Data.SampleFrames / RequiredSliceCount);
    SliceList.Add(aSlice);
  end;

end;



end.
