unit soFilter.CombA;

interface

uses
  Math, eeDsp,
  VamLib.MoreTypes,
  B2.DelayLine.StereoDelayBuffer;

type
  TCombA = class
  private
    fKeyFollowFreqMultiplier: single;
    fSampleRate: single;
    fPar1: single;
    fPar2: single;
    fPar3: single;
    fPar4: single;
    procedure SetSampleRate(const Value: single);
    procedure SetPar1(const Value: single);
    procedure SetPar2(const Value: single);
    procedure SetPar3(const Value: single);
    procedure SetPar4(const Value: single);
  protected
    DelayBuffer : TStereoDelayBuffer;
    CurrentDelayInSamples : single;
    TargetDelayInSamples  : single;

    FeedbackFactor : single;

    MixWet, MixDry : single;

    DelayChangeCoefficient : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure FastControlProcess; inline;

    procedure AudioRateStep(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write SetSampleRate;

    property Par1 : single read fPar1 write SetPar1;  // Delay
    property Par2 : single read fPar2 write SetPar2;  // Feedback
    property Par3 : single read fPar3 write SetPar3;  // unused.
    property Par4 : single read fPar4 write SetPar4;  // mix

    property KeyFollowFreqMultiplier : single read fKeyFollowFreqMultiplier write fKeyFollowFreqMultiplier;

    //property Par1 : single read fPar1 write SetPar1;
    //property Par2 : single read fPar2 write SetPar2;
    //property Par3 : single read fPar3 write SetPar3;
  end;

implementation

uses
  VamLib.Utils;

const
  kMinDelay = 1;   //milliseconds
  kMaxDelay = 100; //milliseconds

{ TCombA }

constructor TCombA.Create;
begin
  fSampleRate := 44100;

  DelayBuffer := TStereoDelayBuffer.Create;
  DelayBuffer.BufferSize := round(((kMaxDelay * 3) / 1000) * fSampleRate);
  DelayBuffer.Clear;
  CurrentDelayInSamples := 100;
  TargetDelayInSamples  := 100;

  Par1 := 0.6;
  Par2 := 0.6;
  Par3 := 0.6;

  DelayChangeCoefficient := CalcRcEnvelopeCoefficient(10, fSampleRate);
end;

destructor TCombA.Destroy;
begin
  DelayBuffer.Free;
  inherited;
end;

procedure TCombA.Reset;
begin
  DelayBuffer.Clear;
  CurrentDelayInSamples := TargetDelayInSamples;
end;

procedure TCombA.SetSampleRate(const Value: single);
begin
  fSampleRate := value;
  DelayBuffer.BufferSize := round(((kMaxDelay * 3) / 1000) * fSampleRate);
  DelayChangeCoefficient := CalcRcEnvelopeCoefficient(10, fSampleRate);
end;

procedure TCombA.SetPar1(const Value: single);
var
  DelayTimeMS : single;
begin
  // TODO:HIGH
  // Par1,2,3 are all updated with their own function calls. This should be
  // consolidated into one function call at most.

  fPar1 := Value;
  DelayTimeMS := kMinDelay + (1-Value) * (kMaxDelay - kMinDelay);
  DelayTimeMS := DelayTimeMS * (1/KeyFollowFreqMultiplier);
  DelayTimeMS := Clamp(DelayTimeMS, kMinDelay, kMaxDelay);
  TargetDelayInSamples := MillisecondsToSamples(DelayTimeMS, SampleRate);
  //if DelayInSamples <= 1 then DelayInSamples := 2;
  assert(CurrentDelayInSamples > 1);
end;

procedure TCombA.SetPar2(const Value: single);
begin
  if Value <> fPar2 then
  begin
    fPar2 := Value;
    FeedbackFactor := Value * (2 - Value);
  end;
end;

procedure TCombA.SetPar3(const Value: single);
begin
  fPar3 := Value;
end;

procedure TCombA.SetPar4(const Value: single);
var
  MixAmount : single;
begin
  if Value <> fPar4 then
  begin
    fPar4 := Value;
    MixAmount := Value * Value;
    MixDry := Sqrt(1 - MixAmount);
    MixWet := Sqrt(MixAmount);
  end;
end;

procedure TCombA.FastControlProcess;
begin
  //TODO: DelayInSamples needs to be smoothed.
  CurrentDelayInSamples := RcEnvFilter(CurrentDelayInSamples, TargetDelayInSamples, DelayChangeCoefficient);
end;

procedure TCombA.AudioRateStep(var x1, x2: single);
var
  tx1, tx2 : single;
begin
  DelayBuffer.ReadRelativeTap(CurrentDelayInSamples, tx1, tx2);
  DelayBuffer.StepInput(x1 + tx1 * FeedbackFactor, x2 + tx2 * FeedbackFactor);
  x1 := (x1 * MixDry) + (tx1 * MixWet);
  x2 := (x2 * MixDry) + (tx2 * MixWet);
  //x1 := (x1 * MixDry);
  //x2 := (x2 * MixDry);
end;




end.
