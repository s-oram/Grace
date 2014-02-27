unit soLfo.RandomLfo;

interface

{$EXCESSPRECISION OFF}

{$SCOPEDENUMS ON}

uses
  Math,
  VamLib.Utils,
  B2.Filter.CriticallyDampedLowpass,
  eeOscPhaseCounter,
  eeDsp;

type
  TRandomLfoShape = (RandomStepped, RandomSmooth);

  TRandomLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    fDensity: single;
    fFlux: single;
    fFreq: single;
    fWaveShape: TRandomLfoShape;
    procedure SetSampleRate(const Value: single);
  protected
    LastValue : single;
    TargetValue : single;
    //TODO: this should be changed to be cardinal values...
    StepSize : TOscPhaseCounter;
    LfoPhase : TOscPhaseCounter;

    LowPass : TCriticallyDampedLowpass;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetPhase;

    procedure UpdateStepSize; //call when the step size needs to be re-calculated. Normally after changing any LFO parameter.

    function Step(out CycleEnd : boolean): single; overload; // generate the next LFO output sample.
    function Step: single; overload;

    property Bpm          : single read fBpm           write fBpm;
    property SampleRate   : single read fSampleRate    write SetSampleRate;

    property Freq         : single read fFreq          write fFreq;       // LFO frequency in hertz.
    property Density      : single read fDensity       write fDensity;    // Range 0..1,   how regularly a new step is caluclated.
    property Flux         : single read fFlux          write fFlux;      // Range 0..1,   how much a step changes by.

    property WaveShape : TRandomLfoShape read fWaveShape write fWaveShape;

  end;

implementation

{ TRandomLfo }

constructor TRandomLfo.Create;
begin
  LastValue := 0.5;
  TargetValue := 0.5;

  StepSize := 0;
  LfoPhase := 0;

  Lowpass := TCriticallyDampedLowpass.Create;

  WaveShape := TRandomLfoShape.RandomStepped;
end;

destructor TRandomLfo.Destroy;
begin
  Lowpass.Free;
  inherited;
end;

procedure TRandomLfo.ResetPhase;
begin
  LastValue := random;
  TargetValue := random;
end;

procedure TRandomLfo.UpdateStepSize;
const
  MinTime = 25;
begin
  StepSize := 1 / SampleRate * Freq;

  case WaveShape of
    TRandomLfoShape.RandomStepped: Lowpass.SetTransitionTime(MinTime, SampleRate);
    TRandomLfoShape.RandomSmooth:  Lowpass.SetTransitionTime(1000 / Freq * 2, SampleRate);
  end;
end;

procedure TRandomLfo.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
end;

function TRandomLfo.Step: single;
var
  x : boolean;
begin
  result := Step(x);
end;

function TRandomLfo.Step(out CycleEnd : boolean):single;
begin
  if LfoPhase.IncByWithOverflowCheck(StepSize) then
  begin
    if Random < Density then
    begin
      TargetValue := LinearInterpolation(LastValue, Random, fFlux);
      CycleEnd := true;
    end;
  end else
  begin
    CycleEnd := false;
  end;

  LastValue := Lowpass.Step(TargetValue);

  result := LastValue;
end;


end.
