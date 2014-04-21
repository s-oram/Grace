unit soFilter.LofiA;

interface

type
  TLofiA = class
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
  eeDsp,
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

constructor TLofiA.Create;
begin
  fBitEmphasis := 0;
  LastSampledX1 := 0;
  LastSampledX2 := 0;
  OldX1 := 0;
  OldX2 := 0;
  FracCounter := 0;
  FracStepSize := 1;
end;

destructor TLofiA.Destroy;
begin

  inherited;
end;

procedure TLofiA.Reset;
begin
  LastSampledX1 := 0;
  LastSampledX2 := 0;
  OldX1 := 0;
  OldX2 := 0;
  FracCounter := 0;
  FracStepSize := 1;
end;

procedure TLofiA.SetBitEmpahasis(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  if value <> fBitEmphasis then
  begin
    fBitEmphasis := Value;

  end;
end;

procedure TLofiA.SetBitReduction(const Value: single);
var
  cv : single;
  vx : single;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  if Value <> fBitReduction then
  begin
    fBitReduction := Value;

    //vx := Power(2, (1-Value) * (1-Value) * 15 + 1);
    cv := (1-Value) * (1-Value) * (1-Value) * (1-Value);
    //cv := value * value * value * value; //inverting doesn't work when bits are 0.
    vx := Power(2, cv * 14 + 2);

    BitScaleUp := vx;
    BitScaleDown := 1 / BitScaleUp;
  end;

  //BitScaleDown
end;

procedure TLofiA.SetRateReduction(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  if Value <> fRateReduction then
  begin
    fRateReduction := Value;
    FracStepSize := 1 / ((1 - Value * Value) * 1023 + 1);
  end;
end;

procedure TLofiA.Step(var x1, x2: single);
begin
  //x1 := EmpFunction(x1, fBitEmphasis);
  //x2 := EmpFunction(x2, fBitEmphasis);



  FracCounter := FracCounter + FracStepSize;
  if FracCounter >= 1 then
  begin
    //x1 := EmpFunction(x1, fBitEmphasis);
    //x2 := EmpFunction(x2, fBitEmphasis);

    x1 := round(x1 * BitScaleUp) * BitScaleDown;
    x2 := round(x2 * BitScaleUp) * BitScaleDown;

    //x1 := DeEmpFunction(x1, fBitEmphasis);
    //x2 := DeEmpFunction(x2, fBitEmphasis);

    FracCounter := FracCounter-1;
    LastSampledX1 := x1;
    LastSampledX2 := x2;
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






end;

end.
