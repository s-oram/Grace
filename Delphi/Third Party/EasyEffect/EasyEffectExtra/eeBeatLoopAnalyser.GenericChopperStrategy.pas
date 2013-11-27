unit eeBeatLoopAnalyser.GenericChopperStrategy;

interface

uses
  Contnrs, eeSampleFloat, MoreTypes,
  eeBeatLoopAnalyser.CoreTypes;

type
  TGenericChopper = class
  private
  protected
    SliceList : TSliceList;
    fEnv1 : TSampleFloat;
    fEnv2 : TSampleFloat;
    Data : record
      SampleFrames : integer;
      SampleRate   : integer;
      Ch1          : PSingle;
      Ch2          : PSingle;
    end;
  public
    constructor Create(aSliceList : TSliceList; aEnv1, aEnv2: TSampleFloat); virtual;
    procedure Chop; virtual; abstract;
    procedure SetSampleDataSource(const SampleFrames : integer; const SampleRate : integer; Ch1:PSingle; Ch2:PSingle = nil);

    property Env1 : TSampleFloat read fEnv1 write fEnv1;
    property Env2 : TSampleFloat read fEnv2 write fEnv2;
  end;



implementation

{ TGenericChopper }

constructor TGenericChopper.Create(aSliceList: TSliceList; aEnv1, aEnv2: TSampleFloat);
begin
  SliceList := aSliceList;
  fEnv1 := aEnv1;
  fEnv2 := aEnv2;
end;

procedure TGenericChopper.SetSampleDataSource(const SampleFrames : integer; const SampleRate : integer; Ch1:PSingle; Ch2:PSingle = nil);
begin
  Data.SampleFrames := SampleFrames;
  Data.SampleRate   := SampleRate;
  Data.Ch1          := Ch1;
  Data.Ch2          := Ch2;

  Env1.Init(1, SampleFrames, SampleRate, 16);
  Env2.Init(1, SampleFrames, SampleRate, 16);
end;


end.
