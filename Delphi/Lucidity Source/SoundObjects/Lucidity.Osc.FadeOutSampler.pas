unit Lucidity.Osc.FadeOutSampler;

interface

{$INCLUDE Defines.inc}

uses
  MoreTypes, B2.Filter.CriticallyDampedLowpass,
  uSampleMap, soGateEnvelope,
  uConstants, eeSampleFloat,
  eeDsp,
  eeCounter,
  Math, SampleOscUtils;

type
  TFadeOutSampler = class
  private
    fSampleRate: single;
    fIsActive: boolean;
    fDecayTime: single;
    procedure SetSampleRate(const Value: single);
    procedure SetDecayTime(const Value: single);
  protected
    fRegion : IRegion;
    PhaseCounter : TCounter;

    GainFactor : single;

    CurrentSample : TSampleFloat;
    SampleEnd     : integer;

    GainFilter : TCriticallyDampedLowpass;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Trigger(const SampleRegion : IRegion; const CurrentSamplePos, CurrentStepSize : single);

    procedure Reset;
    procedure Kill;

    procedure AudioRateStep(out Out1, Out2 : Single); //inline;

    property SampleRate : single read fSampleRate write SetSampleRate;
    property DecayTime  : single read fDecayTime write SetDecayTime;

    property IsActive : boolean read fIsActive;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils;

{ TFadeOutSampler }

constructor TFadeOutSampler.Create;
begin
  fDecayTime := 50;
  fSampleRate := 44100;

  GainFilter := TCriticallyDampedLowpass.Create;
end;

destructor TFadeOutSampler.Destroy;
begin
  GainFilter.Free;
  inherited;
end;

procedure TFadeOutSampler.Kill;
begin
  fIsActive := false;
  fRegion := nil;
end;

procedure TFadeOutSampler.Reset;
begin
  fIsActive := false;
end;

procedure TFadeOutSampler.SetDecayTime(const Value: single);
begin
  fDecayTime := Value;

  if (fDecayTime > 0) and (fSampleRate > 0)
    then GainFilter.SetTransitionTime(fDecayTime, fSampleRate);
end;

procedure TFadeOutSampler.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;

  if (fDecayTime > 0) and (fSampleRate > 0)
    then GainFilter.SetTransitionTime(fDecayTime, fSampleRate);
end;

procedure TFadeOutSampler.Trigger(const SampleRegion: IRegion; const CurrentSamplePos, CurrentStepSize: single);
begin
  fRegion   := SampleRegion;
  fIsActive := true;

  PhaseCounter.ResetTo(CurrentSamplePos);
  PhaseCounter.StepSize := CurrentStepSize;

  // TODO: I think the fade out could be improved by using a linear fade out envelope, or better yet again,
  // a 2nd order critically dampled filter (of some type).
  GainFactor := 0;

  CurrentSample := SampleRegion.GetSample^;
  SampleEnd     := SampleRegion.GetSample^.Properties.SampleFrames-1;

  GainFilter.Reset;
end;

procedure TFadeOutSampler.AudioRateStep(out Out1, Out2: Single);
var
  ax         : integer;
  frac       : single;
begin
  assert(IsActive);
  assert(assigned(fRegion));

  ax   := PhaseCounter.IntegerPart;
  frac := PhaseCounter.FractionalPart;

  if ax >= CurrentSample.Properties.SampleFrames-4 then ax := CurrentSample.Properties.SampleFrames-5; //TODO: Can this be removed?

  assert(ax >= 0);
  assert(ax < CurrentSample.Properties.SampleFrames);

  ReadValuesFromSample_4x3_Optimal(CurrentSample, ax, Frac, Out1, Out2);

  Out1 := Out1 * (1 - GainFactor);
  Out2 := Out2 * (1 - GainFactor);

  GainFactor := GainFilter.Step(1.01);

  PhaseCounter.Step;

  if (GainFactor   >= 1)         then fIsActive := false;
  if (PhaseCounter.IntegerPart >= SampleEnd) then fIsActive := false;
end;



end.
