unit soFilter.BlueFilter;

interface

uses
  eeDsp,
  eeFastCode,
  FilterCore.SimperSVF;

type
  TBlueFilter = class
  private
    fSampleRate: single;
    fFreq: single;
    fInputGain: single;
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

    procedure StepAsLowpass2P(var x1, x2 : single); inline;
    procedure StepAsBandpass2P(var x1, x2 : single); inline;
    procedure StepAsHighpass2P(var x1, x2 : single); inline;

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

constructor TBlueFilter.Create;
begin
end;

destructor TBlueFilter.Destroy;
begin
  inherited;
end;


procedure TBlueFilter.Reset;
begin
  FilterData1.Reset;
  FilterData2.Reset;
end;

procedure TBlueFilter.Step(var x1, x2: single);
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

procedure TBlueFilter.StepAsLowpass2P(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 * GainIn + kDenormal;
  FilterData1.Input[1] := x2 * GainIn  + kDenormal;

  TSimperVCF.StepAsLowPass(FilterData1);

  x1 := FilterData1.Ouput[0] * GainOut;
  x2 := FilterData1.Ouput[1] * GainOut;
end;

procedure TBlueFilter.StepAsBandpass2P(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 * GainIn + kDenormal;
  FilterData1.Input[1] := x2 * GainIn  + kDenormal;

  TSimperVCF.StepAsBandPass(FilterData1);

  x1 := FilterData1.Ouput[0] * GainOut;
  x2 := FilterData1.Ouput[1] * GainOut;
end;

procedure TBlueFilter.StepAsHighpass2P(var x1, x2: single);
begin
  FilterData1.Input[0] := x1 * GainIn + kDenormal;
  FilterData1.Input[1] := x2 * GainIn  + kDenormal;

  TSimperVCF.StepAsHighPass(FilterData1);

  x1 := FilterData1.Ouput[0] * GainOut;
  x2 := FilterData1.Ouput[1] * GainOut;
end;


procedure TBlueFilter.StepAsLowpass4P(var x1, x2: single);
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

procedure TBlueFilter.StepAsBandpass4P(var x1, x2: single);
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

procedure TBlueFilter.StepAsHighpass4P(var x1, x2: single);
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

procedure TBlueFilter.UpdateParameters(const Freq, Q, InputGain: single);
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
