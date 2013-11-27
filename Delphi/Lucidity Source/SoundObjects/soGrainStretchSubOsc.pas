unit soGrainStretchSubOsc;

interface

uses
  SysUtils, MoreTypes,
  uSampleMap, soGateEnvelope,
  uConstants, eeSampleFloat,
  eeDsp,
  Math, SampleOscUtils;

type
  TOscState = (osInactive, osPlayForward, osLoopForward, osLoopBackward, osPlayForwardFadeOut, osPlayBackwardFadeOut);

  TUpdateSampleBounds = procedure(const Sender:TObject; const aSampleRegion:IRegion; const SampleBounds:PSampleOsc_SampleBounds) of object;

  TGrainProperties = record
    SampleStart      : integer;
    SampleFrames     : integer;
    GrainFadeSamples : integer;
    GrainStepSize    : single; // 1 = play grain at original pitch, 2 = play at 2x pitch
    procedure AssignFrom(const aSource : TGrainProperties);
  end;

  TGrainStretchSubOsc = class
  private
    fOnUpdateSampleBounds: TUpdateSampleBounds;
    fSampleRate: single;
    fIsActive: boolean;
    procedure SetSampleRate(const Value: single);
  protected
    Sample : TSampleFloat;
    SampleRegion : IRegion;
    SampleBounds : TSampleOsc_SampleBounds;
    fOscState : TOscState;

    GrainProperties : TGrainProperties;
    ReadIndex : single;
    StepSize  : single;
    GrainStop : integer;
    FadePoint : single;
    CheckForFadePoint : boolean;

    Env : TGateEnvelope;

  public
    constructor Create;
    destructor Destroy; override;

    function GetReadIndex : single; inline;

    procedure SetupOsc(const aSampleRegion:IRegion; const aSample:TSampleFloat);

    procedure Trigger(const aGrainProperties:TGrainProperties);
    procedure Kill;

    procedure ControlRateStep; inline;
    procedure AudioRateStep(out Out1, Out2 : Single); inline;

    property SampleRate : single read fSampleRate write SetSampleRate;

    property OnUpdateSampleBounds : TUpdateSampleBounds read fOnUpdateSampleBounds write fOnUpdateSampleBounds;

    property IsActive : boolean read fIsActive;
  end;

implementation

{ TGrainStretchSubOsc }

constructor TGrainStretchSubOsc.Create;
begin
  Env := TGateEnvelope.Create;
end;

destructor TGrainStretchSubOsc.Destroy;
begin
  Env.Free;
  inherited;
end;

function TGrainStretchSubOsc.GetReadIndex: single;
begin
  result := ReadIndex;
end;

procedure TGrainStretchSubOsc.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  Env.SampleRate := Value;
end;

procedure TGrainStretchSubOsc.SetupOsc(const aSampleRegion: IRegion; const aSample: TSampleFloat);
begin
  assert(aSample <> nil);
  Sample       := aSample;
  SampleRegion := aSampleRegion;
  fOscState := osInactive;
end;

procedure TGrainStretchSubOsc.Trigger(const aGrainProperties: TGrainProperties);
begin
  fIsActive := true;

  GrainProperties := aGrainProperties;

  ReadIndex := aGrainProperties.SampleStart;
  GrainStop := aGrainProperties.SampleStart + aGrainProperties.SampleFrames;
  StepSize := aGrainProperties.GrainStepSize;

  Env.AttackTime_Samples := aGrainProperties.GrainFadeSamples;
  Env.ReleaseTime_Samples := aGrainProperties.GrainFadeSamples;

  Env.Kill;
  Env.Trigger;

  FadePoint := GrainStop - (aGrainProperties.GrainFadeSamples * aGrainProperties.GrainStepSize);

  assert(FadePoint > 0, 'It definitely should be bigger then 0.');

  CheckForFadePoint := true;
end;

procedure TGrainStretchSubOsc.Kill;
begin
  Sample       := nil;
  SampleRegion := nil;
  fOscState := osInactive;
  fIsActive := false;
  Env.Kill;
end;



procedure TGrainStretchSubOsc.ControlRateStep;
begin

end;

procedure TGrainStretchSubOsc.AudioRateStep(out Out1, Out2: Single);
var
  ax         : integer;
  frac       : single;
  GainFactor : single;

begin
  assert(fIsActive);

  ax := floor(ReadIndex);
  frac := ReadIndex - ax;

  ReadValuesFromSample_4x3_Optimal(Sample, ax, Frac, Out1, Out2);
  ReadIndex := ReadIndex + StepSize;

  GainFactor := Env.Step;
  Out1 := Out1 * GainFactor;
  Out2 := Out2 * GainFactor;

  if (CheckForFadePoint) and (ReadIndex >= FadePoint) then
  begin
    CheckForFadePoint := false;
    Env.Release;
  end;

  if (ReadIndex >= GrainStop) then
  begin
    ReadIndex := GrainStop - 1;
    fIsActive := false;
  end;

end;



{ TGrainProperties }

procedure TGrainProperties.AssignFrom(const aSource: TGrainProperties);
begin
  self.SampleStart      := aSource.SampleStart;
  self.SampleFrames     := aSource.SampleFrames;
  self.GrainFadeSamples := aSource.GrainFadeSamples;
  self.GrainStepSize    := aSource.GrainStepSize;
end;

end.
