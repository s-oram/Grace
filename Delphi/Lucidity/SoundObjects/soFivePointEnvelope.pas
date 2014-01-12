unit soFivePointEnvelope;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes;

type
  TFivePointEnvelope = class
  private
    fSampleRate: single;
    fIsActive: boolean;
    function GetStageTime(const Index: Integer): single;
    procedure SetStageTime(const Index: Integer; const Value: single);
    function GetStageLevel(const Index: Integer): single;
    procedure SetStageLevel(const Index: Integer; const Value: single);
  protected
    fStageStepSize : array[0..4] of single;
    fStageTime  : array[0..4] of single;
    fStageTimeInSamples : array[0..4] of single;
    fStageLevel : array[0..4] of single;

    CurrentStageTime : integer;
    CurEnvStage : integer;
    CurEnvValue : single;
    StepSize : single;
    TargetValue : single;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure StepReset; inline;
    procedure Step; inline; //Process one sample frame.

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure QuickRelease(Time_ms:single);
    procedure Kill;

    property Stage1Time : single index 0 read GetStageTime write SetStageTime; // time in milliseconds
    property Stage2Time : single index 1 read GetStageTime write SetStageTime; // time in milliseconds
    property Stage3Time : single index 2 read GetStageTime write SetStageTime; // time in milliseconds
    property Stage4Time : single index 3 read GetStageTime write SetStageTime; // time in milliseconds
    property Stage5Time : single index 4 read GetStageTime write SetStageTime; // time in milliseconds

    property Stage1Level : single index 0 read GetStageLevel write SetStageLevel; // Level range -1..1
    property Stage2Level : single index 1 read GetStageLevel write SetStageLevel; // Level range -1..1
    property Stage3Level : single index 2 read GetStageLevel write SetStageLevel; // Level range -1..1
    property Stage4Level : single index 3 read GetStageLevel write SetStageLevel; // Level range -1..1
    property Stage5Level : single index 4 read GetStageLevel write SetStageLevel; // Level range -1..1

    property SampleRate : single read fSampleRate write fSampleRate;

    property IsActive : boolean read fIsActive;

    property EnvValue : single read CurEnvValue;
  end;

implementation

uses
  SysUtils,
  eeDsp;

{ TFivePointEnvelope }

constructor TFivePointEnvelope.Create;
var
  c1: Integer;
begin
  for c1 := 0 to 4 do
  begin
    fStageTime[c1] := 0;
    fStageTimeInSamples[c1] := 1;
    fStageLevel[c1] := 0;
    fStageStepSize[c1] := 0;
  end;
end;

destructor TFivePointEnvelope.Destroy;
begin

  inherited;
end;

function TFivePointEnvelope.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'EnvOut' then Exit(@CurEnvValue);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

function TFivePointEnvelope.GetStageLevel(const Index: Integer): single;
begin
  result := fStageLevel[Index];
end;

function TFivePointEnvelope.GetStageTime(const Index: Integer): single;
begin
  result := fStageTime[Index];
end;

procedure TFivePointEnvelope.SetStageLevel(const Index: Integer; const Value: single);
begin
  fStageLevel[Index] := Value;
end;

procedure TFivePointEnvelope.SetStageTime(const Index: Integer; const Value: single);
begin
  fStageTime[Index] := Value;
  fStageTimeInSamples[Index] := MilliSecondsToSamples(Value, SampleRate);
end;


procedure TFivePointEnvelope.Trigger(aVelocity: single);
begin
  fIsActive := true;

  CurEnvStage := 0;
  CurEnvValue := 0;
  StepSize := fStageStepSize[CurEnvStage];

  CurrentStageTime := 0;
end;

procedure TFivePointEnvelope.Release;
begin

end;

procedure TFivePointEnvelope.QuickRelease(Time_ms: single);
begin

end;

procedure TFivePointEnvelope.Kill;
begin
  fIsActive := false;
end;

procedure TFivePointEnvelope.StepReset;
begin

end;

procedure TFivePointEnvelope.Step;
var
  TargetEnvValue  : single;
  TargetStageTime : single;
begin
  if IsActive then
  begin
    TargetEnvValue   := fStageLevel[CurEnvStage];
    TargetStageTime  := fStageTimeInSamples[CurEnvStage];

    if CurrentStageTime < TargetStageTime then
    begin
      CurEnvValue := CurEnvValue + (TargetEnvValue - CurEnvValue) / (TargetStageTime - CurrentStageTime);
      inc(CurrentStageTime);
    end else
    begin
      CurrentStageTime := 0;
      inc(CurEnvStage);
      if CurEnvStage > 4 then fIsActive := false;
    end;
  end;
end;



end.
