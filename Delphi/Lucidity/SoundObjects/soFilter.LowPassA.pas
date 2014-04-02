unit soFilter.LowPassA;

interface

uses
  eeDsp,
  eeFastCode,
  FilterCore.SimperSVF;

type
  TLowPassA = class
  private
    fSampleRate: single;
    fFreq: single;
    fQ: single;
    fInputGain: single;
    procedure SetFreq(const Value: single);
    procedure SetQ(const Value: single);
  protected
    GainIn  : double;
    GainOut : double;
    FilterData1 : TDualSimperSVFData;
    FilterData2 : TDualSimperSVFData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure Step(var x1, x2 : single); inline;

    procedure StepAsLowpass4P(var x1, x2 : single); inline;
    procedure StepAsBandpass4P(var x1, x2 : single); inline;
    procedure StepAsHighpass4P(var x1, x2 : single); inline;

    // Freq range 10?..1/4 Nyquist?
    // Q range 0..1
    // Input gain is a linear multipication
    procedure UpdateParameters(const Freq, Q, InputGain : single);

    property SampleRate : single read fSampleRate write fSampleRate;

    property InputGain : single read fInputGain write fInputGain; //Linear value.
  end;

implementation

uses
  Math;

{ TLowPassA }

constructor TLowPassA.Create;
begin
end;

destructor TLowPassA.Destroy;
begin
  inherited;
end;


procedure TLowPassA.Reset;
begin
  FilterData1.Reset;
  FilterData2.Reset;
end;

procedure TLowPassA.SetFreq(const Value: single);
var
  g : double;
begin
  fFreq := Value;

  //g := tan (pi * Value / samplerate);
  g := Fast_Tan0(pi * Value / samplerate);

  FilterData1.G[0] := G;
  FilterData1.G[1] := G;
  FilterData2.G[0] := G;
  FilterData2.G[1] := G;
end;

procedure TLowPassA.SetQ(const Value: single);
var
  DampingFactor : single;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fQ := Value;

  //Damping factor range is 2..0.    0 = self oscillation.
  DampingFactor := 2 - (Value * 2);

  FilterData1.K[0] := DampingFactor;
  FilterData1.K[1] := DampingFactor;
  FilterData2.K[0] := DampingFactor;
  FilterData2.K[1] := DampingFactor;
end;

procedure TLowPassA.Step(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 + kDenormal;
  FilterData1.Input[1] := x2 + kDenormal;

  TSimperVCF.StepAsLowPass(FilterData1);

  FilterData2.Input[0] := FilterData1.Ouput[0] + kDenormal;
  FilterData2.Input[1] := FilterData1.Ouput[1] + kDenormal;

  TSimperVCF.StepAsLowPass(FilterData2);

  x1 := FilterData2.Ouput[0];
  x2 := FilterData2.Ouput[1];
end;

procedure TLowPassA.StepAsLowpass4P(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 * GainIn + kDenormal;
  FilterData1.Input[1] := x2 * GainIn  + kDenormal;

  TSimperVCF.StepAsLowPass(FilterData1);

  FilterData2.Input[0] := FilterData1.Ouput[0] + kDenormal;
  FilterData2.Input[1] := FilterData1.Ouput[1] + kDenormal;

  TSimperVCF.StepAsLowPass(FilterData2);

  x1 := FilterData2.Ouput[0] * GainOut;
  x2 := FilterData2.Ouput[1] * GainOut;
end;

procedure TLowPassA.StepAsBandpass4P(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 + kDenormal;
  FilterData1.Input[1] := x2 + kDenormal;

  TSimperVCF.StepAsBandPass(FilterData1);

  FilterData2.Input[0] := FilterData1.Ouput[0] + kDenormal;
  FilterData2.Input[1] := FilterData1.Ouput[1] + kDenormal;

  TSimperVCF.StepAsBandPass(FilterData2);

  x1 := FilterData2.Ouput[0];
  x2 := FilterData2.Ouput[1];
end;

procedure TLowPassA.StepAsHighpass4P(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 + kDenormal;
  FilterData1.Input[1] := x2 + kDenormal;

  TSimperVCF.StepAsHighPass(FilterData1);

  FilterData2.Input[0] := FilterData1.Ouput[0] + kDenormal;
  FilterData2.Input[1] := FilterData1.Ouput[1] + kDenormal;

  TSimperVCF.StepAsHighPass(FilterData2);

  x1 := FilterData2.Ouput[0];
  x2 := FilterData2.Ouput[1];
end;

procedure TLowPassA.UpdateParameters(const Freq, Q, InputGain: single);
var
  G : single;
  K : single;
begin
  fFreq := Freq;

  //g := tan (pi * Value / samplerate);
  G := Fast_Tan0(pi * Freq / samplerate);

  //Damping factor range is 2..0.    0 = self oscillation.
  K := 2 - (Q * 2);

  FilterData1.SetGK(G, K);
  FilterData2.SetGK(G, K);

  GainIn  := 1 * (1 + (InputGain * 8));
  GainOut := 1 / (1 + (InputGain * 4));
end;

end.
