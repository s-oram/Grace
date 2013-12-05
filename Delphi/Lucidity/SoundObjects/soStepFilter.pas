unit soStepFilter;

interface

uses
  eeBiquadFilterCore;

type
  TStepFilter = class
  private
    fSampleRate: integer;
    fDecayTime: double;
    procedure SetSampleRate(const Value: integer);
    procedure SetDecayTime(const Value: double);
  protected
    FilterL, FilterR : TBiquadFilterCore;
    FilterCoefficient : double;
    FilterCoefficient2 : double;
    OffsetX1, OffsetX2 : double;
    OldXX1, OldXX2 : double;
    OldX1, OldX2 : double;
    procedure CalcFilterCutoff;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddStep(const x1, x2 : single);
    procedure Process(var x1, x2 : single);

    property DecayTime  : double read fDecayTime write SetDecayTime; //in milliseconds.
    property SampleRate : integer read fSampleRate write SetSampleRate;
  end;

implementation

uses
  eeBiquadFilters,
  eeDsp;

{ TStepFilter }

constructor TStepFilter.Create;
begin
  FilterL := TBiquadFilterCore.Create;
  FilterR := TBiquadFilterCore.Create;

  OffsetX1 := 0;
  OffsetX2 := 0;

  OldX1 := 0;
  OldX2 := 0;

  fDecayTime := 25;
  fSampleRate := 44100;
  CalcFilterCutoff;
end;

destructor TStepFilter.Destroy;
begin
  FilterL.Free;
  FilterR.Free;
  Inherited;
end;

procedure TStepFilter.SetDecayTime(const Value: double);
begin
  if Value <> fDecayTime then
  begin
    fDecayTime := Value;
    CalcFilterCutoff;
  end;
end;

procedure TStepFilter.SetSampleRate(const Value: integer);
begin
  if Value <> fSampleRate then
  begin
    fSampleRate := Value;
    CalcFilterCutoff;
  end;
end;

procedure TStepFilter.AddStep(const x1, x2: single);
begin
  OffsetX1 := OffsetX1 - x1;
  OffsetX2 := OffsetX2 - x2;
end;

procedure TStepFilter.Process(var x1, x2: single);
begin
  x1 := x1 + OffsetX1;
  x2 := x2 + OffsetX2;

  OffsetX1 := (OffsetX1 * FilterCoefficient);
  OffsetX2 := (OffsetX2 * FilterCoefficient);
end;

procedure TStepFilter.CalcFilterCutoff;
begin
  FilterCoefficient := CalcRcEnvelopeCoefficient(DecayTime, SampleRate);


  {
  freq := 0.0001;
  CalcBiquad_Cookbook_LowPass(freq, sampleRate, b0, b1, b2, a1, a2);

  // Example working filter coefficients..
  //B0 := 0.034785961822381306;
  //B1 := 0.06957192364476261;
  //B2 := 0.034785961822381306;
  //A1 := -1.407502284220597;
  //A2 := 0.5466461315101225;

  FilterL.B0_Coeff := b0;
  FilterL.B1_Coeff := b1;
  FilterL.B2_Coeff := b2;
  FilterL.A1_Coeff := a1;
  FilterL.A2_Coeff := a2;
  FilterL.Reset;

  FilterR.B0_Coeff := b0;
  FilterR.B1_Coeff := b1;
  FilterR.B2_Coeff := b2;
  FilterR.A1_Coeff := a1;
  FilterR.A2_Coeff := a2;
  FilterR.Reset;
  }
end;




end.
