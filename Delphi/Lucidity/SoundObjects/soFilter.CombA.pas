unit soFilter.CombA;

interface

uses
  Math,
  VamLib.MoreTypes,
  B2.DelayLine.StereoDelayBuffer;

type
  TCombA = class
  private
    fSampleRate: single;
    fPar2: single;
    fPar3: single;
    fPar1: single;
    procedure SetSampleRate(const Value: single);
    procedure SetPar1(const Value: single);
    procedure SetPar2(const Value: single);
    procedure SetPar3(const Value: single);
  protected
    DelayBuffer : TStereoDelayBuffer;
    DelayInSamples : single;

    FeedbackFactor : single;

    MixWet, MixDry : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure ControlRateStep; inline;
    procedure AudioRateStep(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write SetSampleRate;

    property Par1 : single read fPar1 write SetPar1;  // Delay
    property Par2 : single read fPar2 write SetPar2;  // Feedback
    property Par3 : single read fPar3 write SetPar3;  // mix

    //property Par1 : single read fPar1 write SetPar1;
    //property Par2 : single read fPar2 write SetPar2;
    //property Par3 : single read fPar3 write SetPar3;
  end;

implementation

uses
  eeDsp;

const
  kMinDelay = 0.01;   //milliseconds
  kMaxDelay = 50; //milliseconds

{ TCombA }

constructor TCombA.Create;
begin
  fSampleRate := 44100;
  DelayBuffer := TStereoDelayBuffer.Create;
  DelayBuffer.BufferSize := round(((kMaxDelay * 3) / 1000) * fSampleRate);
  DelayBuffer.Clear;
  DelayInSamples := 100;



  Par1 := 0.6;
  Par2 := 0.6;
  Par3 := 0.6;
end;

destructor TCombA.Destroy;
begin
  DelayBuffer.Free;
  inherited;
end;

procedure TCombA.Reset;
begin
  DelayBuffer.Clear;
end;

procedure TCombA.SetSampleRate(const Value: single);
begin
  fSampleRate := value;
  DelayBuffer.BufferSize := round(((kMaxDelay * 3) / 1000) * fSampleRate);
end;

procedure TCombA.SetPar1(const Value: single);
var
  DelayTimeMS : single;
begin
  if Value <> fPar1 then
  begin
    fPar1 := Value;
    DelayTimeMS := kMinDelay + (1-Value) * (kMaxDelay - kMinDelay);
    DelayInSamples := MillisecondsToSamples(DelayTimeMS, SampleRate);
    if DelayInSamples <= 1 then DelayInSamples := 2;
    assert(DelayInSamples > 1);
  end;
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
var
  MixAmount : single;
begin
  if Value <> fPar3 then
  begin
    fPar3 := Value;
    MixAmount := Value * Value;
    MixDry := Sqrt(1 - MixAmount);
    MixWet := Sqrt(MixAmount);
  end;

end;

procedure TCombA.ControlRateStep;
begin
  //TODO: DelayInSamples needs to be smoothed.
end;

procedure TCombA.AudioRateStep(var x1, x2: single);
var
  tx1, tx2 : single;
begin
  DelayBuffer.ReadRelativeTap(DelayInSamples, tx1, tx2);
  DelayBuffer.StepInput(x1 + tx1 * FeedbackFactor, x2 + tx2 * FeedbackFactor);
  x1 := (x1 * MixDry) + (tx1 * MixWet);
  x2 := (x2 * MixDry) + (tx2 * MixWet);
  //x1 := (x1 * MixDry);
  //x2 := (x2 * MixDry);
end;




end.
