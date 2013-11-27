{
  TLevelMeter is a class that monitors the volume of audio streams. It is designed to be used as
  input for level meters in mixers and the like.
}

unit eeLevelMeter;

interface

uses
  eeGlobals, eeDelayBuffers, eeDsp, Math;

type
  TLevelMeter = class
  private
    fFallRate: single;
    fValue: single;
  protected
    Globals:TGlobals;
  public
    constructor Create(aGlobals:TGlobals);
	  destructor Destroy; override;

    procedure Update(aLevel:single; SampleFrames:integer);

    property FallRate:single read fFallRate write fFallRate;

    property Value:single read fValue write fValue;
  end;

  TEnvFollower = class
  private
    fSampleRate: single;
    fLevel1: double;
    fDecayTime: single;
    fLevel2: double;
    fAttackTime: single;
    fHoldTime: single;
    procedure SetSampleRate(const Value: single);
    procedure SetDecayTime(const Value: single);
    function GetLevel1AsDB: double;
    function GetLevel2AsDB: double;
    procedure SetAttackTime(const Value: single);
    procedure SetHoldTime(const Value: single);
  protected
    AttackCoefficient : double;
    DecayCoefficient  : double;
    HoldSamples       : integer;
    HoldSampleCount1  : integer;
    HoldSampleCount2  : integer;
    PeakLevel1        : single;
    PeakLevel2        : single;
    procedure CalcAttackCoefficient; inline;
    procedure CalcDecayCoefficient; inline;
    procedure CalcHoldCoefficient; inline;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Step(In1:single); overload; inline;
    procedure Step(In1, In2: single); overload; inline;

    //NOTE: To use the Envelope follower as a peak follower, typically:
    //  AttackTime = 0 ms
    //  DecayTime  = 35 ms plus.
    // With lower decay times, the envelope follower will flucuate with low freqrency signals. (Under ~20hz)
    property AttackTime : single read fAttackTime write SetAttackTime;  //ms
    property HoldTime   : single read fHoldTime   write SetHoldTime;    //ms
    property DecayTime  : single read fDecayTime  write SetDecayTime;   //ms
    property SampleRate : single read fSampleRate write SetSampleRate;

    property Level1     : double read fLevel1;
    property Level2     : double read fLevel2;

    property Level1AsDB : double read GetLevel1AsDB;
    property Level2AsDB : double read GetLevel2AsDB;
  end;


  TMovingAverageFilter = class
  private
    fAverageTime: integer;
    fLevel1: double;
    procedure SetAverageTime(const Value: integer);
  protected
    DelayBuffer:TFixedSizeBuffer1024Mono;
    z1:single;
    OneOverDelayTime:double;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Step(const In1:single); overload; inline;

    //The Average time is set in samples and has a maximum average time of 1023 samples.
    // The maximum time can be increase by dropping in a new delay buffer class.
    property AverageTime : integer read fAverageTime write SetAverageTime; //Samples...

    property Level1  : double read fLevel1;
  end;

  TOverDetector = class
  private
    fSampleRate: single;
    fHoldTime: single;
    fIsOver: boolean;
    procedure SetHoldTime(const Value: single);
    procedure SetSampleRate(const Value: single);
  protected
    HoldSamples     : integer;
    HoldSampleCount : integer;
    procedure CalcHoldTime; inline;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Step(const In1:single); overload; inline;
    procedure Step(const In1, In2:single); overload; inline;

    property SampleRate : single read fSampleRate write SetSampleRate;
    property HoldTime   : single read fHoldTime   write SetHoldTime;   //milliseconds.

    property IsOver     : boolean read fIsOver;
  end;

implementation

uses
  MoreTypes;

{ TLevelMeter }

constructor TLevelMeter.Create(aGlobals: TGlobals);
begin
  Globals := aGlobals;

  Value    := 0;
  FallRate := 2;
end;

destructor TLevelMeter.Destroy;
begin

  inherited;
end;

procedure TLevelMeter.Update(aLevel: single; SampleFrames: integer);
begin
  if (Value < aLevel) then
  begin
    Value := aLevel;
  end
  else if (Value > 0) then
  begin
    Value := Value - (SampleFrames * Globals.OneOverSampleRate * FallRate);
    if Value < 0 then Value := 0;
  end;
end;

{ TEnvFollower }

constructor TEnvFollower.Create;
begin
  PeakLevel1       := 0;
  PeakLevel2       := 0;
  fLevel1          := 0;
  fLevel2          := 0;
  HoldSampleCount1 := 0;
  HoldSampleCount2 := 0;

  // NOTE: 35 milliseconds is very close to the minimum decay time required to smooth out
  // signal fluctuations when the signal is at 20hz. below 20hz the peak follower will
  // fluctuate with the waveform.
  fAttackTime  := 0;
  fHoldTime    := 0;
  fDecayTime   := 35;
  fSampleRate  := 44100;
  CalcAttackCoefficient;
  CalcHoldCoefficient;
  CalcDecayCoefficient;
end;

destructor TEnvFollower.Destroy;
begin

  inherited;
end;

function TEnvFollower.GetLevel1AsDB: double;
begin
  result := VoltageToDecibels(fLevel1);
end;

function TEnvFollower.GetLevel2AsDB: double;
begin
  result := VoltageToDecibels(fLevel2);
end;

procedure TEnvFollower.CalcAttackCoefficient;
begin
  // Algorithm taken from here:
  // http://www.kvraudio.com/forum/viewtopic.php?t=300689

  if fAttackTime <= 0
    then AttackCoefficient := 1
    else AttackCoefficient := 1-exp( -1.0 / (fAttackTime * 0.001 * sampleRate));
end;

procedure TEnvFollower.CalcDecayCoefficient;
begin
  // Algorithm taken from here:
  // http://www.kvraudio.com/forum/viewtopic.php?t=300689

  if fDecayTime <= 0
    then DecayCoefficient := 1
    else DecayCoefficient := 1-exp( -1.0 / (fDecayTime * 0.001 * sampleRate));
end;



procedure TEnvFollower.CalcHoldCoefficient;
begin
  HoldSamples := round(MillisecondsToSamples(fHoldTime, SampleRate));
end;

procedure TEnvFollower.SetAttackTime(const Value: single);
begin
  fAttackTime := Value;
  CalcAttackCoefficient;
end;

procedure TEnvFollower.SetDecayTime(const Value: single);
begin
  fDecayTime := Value;
  CalcDecayCoefficient;
end;

procedure TEnvFollower.SetHoldTime(const Value: single);
begin
  fHoldTime := Value;
  CalcHoldCoefficient;
end;

procedure TEnvFollower.SetSampleRate(const Value: single);

begin
  fSampleRate := Value;
  CalcAttackCoefficient;
  CalcDecayCoefficient;
  CalcHoldCoefficient;
end;

procedure TEnvFollower.Step(In1, In2: single);
const
  kDenorm = 1.0e-24;
begin
  In1 := abs(In1 + kDenorm);
  In2 := abs(In2 + kDenorm);

  if In1 > PeakLevel1 then
  begin
    PeakLevel1 := In1;
    HoldSampleCount1 := 0;
  end else
  begin
    if HoldSampleCount1 < HoldSamples
      then inc(HoldSampleCount1)
      else PeakLevel1 := In1;
  end;


  if In2 > PeakLevel2 then
  begin
    PeakLevel2 := In2;
    HoldSampleCount2 := 0;
  end else
  begin
    if HoldSampleCount2 < HoldSamples
      then inc(HoldSampleCount2)
      else PeakLevel2 := In2;
  end;


  if PeakLevel1 > fLevel1
    then fLevel1 := fLevel1 + (PeakLevel1 - fLevel1) * AttackCoefficient
    else fLevel1 := fLevel1 + (PeakLevel1 - fLevel1) * DecayCoefficient;

  if In2 > fLevel2
    then fLevel2 := fLevel2 + (PeakLevel2 - fLevel2) * AttackCoefficient
    else fLevel2 := fLevel2 + (PeakLevel2 - fLevel2) * DecayCoefficient;

end;

procedure TEnvFollower.Step(In1: single);
const
  kDenorm = 1.0e-24;
begin
  In1 := abs(In1 + kDenorm);

  if In1 > PeakLevel1 then
  begin
    PeakLevel1 := In1;
    HoldSampleCount1 := 0;
  end else
  begin
    if HoldSampleCount1 < HoldSamples
      then inc(HoldSampleCount1)
      else PeakLevel1 := In1;
  end;

  if PeakLevel1 > fLevel1
    then fLevel1 := fLevel1 + (PeakLevel1 - fLevel1) * AttackCoefficient
    else fLevel1 := fLevel1 + (PeakLevel1 - fLevel1) * DecayCoefficient;
end;

{ TMovingAverageFilter }

// TMovingAverageFilter NOTES
// The moving average filter uses a CIC filter to track the running sum.
// The CIC filter is a comb filter followed by an integrator stage.
// The CIC filter is described on page 398 of "Understanding Digital signal processing". 

constructor TMovingAverageFilter.Create;
begin
  DelayBuffer := TFixedSizeBuffer1024Mono.Create;

  z1 := 0;
  AverageTime := 6;

  fLevel1 := 0;
end;

destructor TMovingAverageFilter.Destroy;
begin
  DelayBuffer.Free;
  inherited;
end;

procedure TMovingAverageFilter.SetAverageTime(const Value: integer);
begin
  fAverageTime := Value;

  assert(Value < 1024);
  DelayBuffer.DelayTime := Value;
  OneOverDelayTime      := 1 / Value;
end;

procedure TMovingAverageFilter.Step(const In1: single);
var
  x:double;
begin
  DelayBuffer.StepInput(In1);

  x       := In1 - DelayBuffer.Output;
  x       := x + z1;
  z1      := x;

  fLevel1 := x * OneOverDelayTime;
end;

{ TOverDetector }

constructor TOverDetector.Create;
begin
  fIsOver     := false;
  fSampleRate := 44100;
  fHoldTime   := 500;
  CalcHoldTime;

end;

destructor TOverDetector.Destroy;
begin

  inherited;
end;

procedure TOverDetector.SetHoldTime(const Value: single);
begin
  fHoldTime := Value;
  CalcHoldTime;
end;

procedure TOverDetector.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  CalcHoldTime;
end;

procedure TOverDetector.CalcHoldTime;
begin
  HoldSamples := round(MillisecondsToSamples(fHoldTime, SampleRate));
end;

procedure TOverDetector.Step(const In1: single);
begin
  if abs(In1) > 1 then
  begin
    HoldSampleCount := 0;
    fIsOver         := true;
  end else
  begin
    if HoldSampleCount < HoldSamples
      then inc(HoldSampleCount)
      else fIsOver := false;
  end;
end;

procedure TOverDetector.Step(const In1, In2: single);
begin
  if Max(abs(In1), abs(In2)) > 1 then
  begin
    HoldSampleCount := 0;
    fIsOver         := true;
  end else
  begin
    if HoldSampleCount < HoldSamples
      then inc(HoldSampleCount)
      else fIsOver := false;
  end;
end;



end.
