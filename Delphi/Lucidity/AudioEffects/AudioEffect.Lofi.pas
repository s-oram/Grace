unit AudioEffect.Lofi;

interface

uses
  eeDsp,
  AudioEffect;

type
  TLofi = class
  private
    fSampleRate: single;
    fRateReduction: single;
    fBitReduction: single;
    fBitEmphasis: single;
    procedure SetRateReduction(const Value: single);
    procedure SetBitReduction(const Value: single);
    procedure SetBitEmpahasis(const Value: single);
  protected
    FracCounter  : single;
    FracStepSize : single;

    OldX1, OldX2 : single;
    LastSampledX1, LastSampledX2 : single;


    SamplesSinceLast : single;
    SamplesToCountTarget : single;
    SamplesToCount   : single;
    TotalX1, TotalX2 : single;

    BitScaleUp, BitScaleDown : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Step(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write fSampleRate;

    property RateReduction : single read fRateReduction write SetRateReduction; //range 0..1. 0 = min reduction, 1 = max reduction.
    property BitReduction  : single read fBitReduction  write SetBitReduction;  //range 0..1. 0 = min reduction, 1 = max reduction.
    property BitEmphasis   : single read fBitEmphasis   write SetBitEmpahasis;  //range 0..1.
  end;

function EmpFunction(const Input, CurveAmount : single):single;
function DeEmpFunction(const Input, CurveAmount : single):single;

implementation

uses
  Math;

function EmpFunction(const Input, CurveAmount : single):single;
  //NOTE: This might make an ok distortion..
var
  a : single;
  AbX : single;
  CFactor : single;
begin
  a := CurveAmount + kDenormal;
  Abx := Abs(Input);
  CFactor := a/(1-a+(-1+2 * a) * Abx);
  result := Input * CFactor;
end;

function DeEmpFunction(const Input, CurveAmount : single):single;
  //NOTE: This might make an ok distortion..
var
  a : single;
  AbX : single;
  CFactor : single;
begin
  a := CurveAmount + kDenormal;
  Abx := Abs(Input);
  CFactor := a/(1-a+(-1+2 * a) * Abx);
  result := Input / CFactor;
end;

{ TLofiA }

constructor TLofi.Create;
begin
  fBitEmphasis := 0;
  LastSampledX1 := 0;
  LastSampledX2 := 0;
  OldX1 := 0;
  OldX2 := 0;
  FracCounter := 0;
  FracStepSize := 1;


end;

destructor TLofi.Destroy;
begin

  inherited;
end;

procedure TLofi.Reset;
begin
  SamplesSinceLast := 0;
  SamplesToCount   := 1;
  SamplesToCountTarget := 1;
  TotalX1 := 0;
  TotalX2 := 0;
  LastSampledX1 := 0;
  LastSampledX2 := 0;
  OldX1 := 0;
  OldX2 := 0;
  FracCounter := 0;
  FracStepSize := 1;
end;

procedure TLofi.SetBitEmpahasis(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  if value <> fBitEmphasis then
  begin
    fBitEmphasis := Value;

  end;
end;

procedure TLofi.SetBitReduction(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fBitReduction := Value;

  BitScaleUp := 4 + 512 * Value * Value;
  BitScaleDown := 1 / BitScaleUp;
end;

procedure TLofi.SetRateReduction(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);


  fRateReduction := Value;
  SamplesToCountTarget := (1-Value) * 63 + 1;
end;

procedure TLofi.Step(var x1, x2: single);
begin
  //x1 := EmpFunction(x1, fBitEmphasis);
  //x2 := EmpFunction(x2, fBitEmphasis);


  if SamplesToCount < SamplesSinceLast then
  begin
    LastSampledX1 := TotalX1 / SamplesToCount;
    LastSampledX2 := TotalX2 / SamplesToCount;

    LastSampledX1 := floor(abs(LastSampledX1) * BitScaleUp) * BitScaleDown * sign(LastSampledX1);
    LastSampledX2 := floor(abs(LastSampledX2) * BitScaleUp) * BitScaleDown * sign(LastSampledX2);

    SamplesSinceLast := SamplesSinceLast - SamplesToCount;

    TotalX1 := 0;
    TotalX2 := 0;

    SamplesToCount := SamplesToCountTarget;
  end;

  TotalX1 := TotalX1 + x1;
  TotalX2 := TotalX2 + x2;

  SamplesSinceLast := SamplesSinceLast + 1;


  x1 := LastSampledX1;
  x2 := LastSampledX2;





  {
  FracCounter := FracCounter + FracStepSize;
  if FracCounter >= 1 then
  begin
    //x1 := EmpFunction(x1, fBitEmphasis);
    //x2 := EmpFunction(x2, fBitEmphasis);

    //x1 := round(x1 * BitScaleUp) * BitScaleDown;
    //x2 := round(x2 * BitScaleUp) * BitScaleDown;

    //x1 := DeEmpFunction(x1, fBitEmphasis);
    //x2 := DeEmpFunction(x2, fBitEmphasis);

    FracCounter := FracCounter-1;
    LastSampledX1 := LinearInterpolation(Oldx1, x1, FracCounter);
    LastSampledX2 := LinearInterpolation(Oldx2, x2, FracCounter);
    OldX1 := x1;
    OldX2 := x2;

    //TODO: The sample rate reducer has jitter. It should interpolate to
    // find the actual reduced sample position instead of taking the nearest
    // integer value.



  end else
  begin
    OldX1 := x1;
    OldX2 := x2;

    x1 := LastSampledX1;
    x2 := LastSampledX2;
  end;
  }

end;

end.
