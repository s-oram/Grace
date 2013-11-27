unit soFilter.BandPassA;

interface

uses
  eeDsp,
  eeFastCode,
  FilterCore.SimperSVF;

type
  TBandPassA = class
  private
    fSampleRate: single;
    fFreq: single;
    fQ: single;
    procedure SetFreq(const Value: single);
    procedure SetQ(const Value: single);
  protected
    CoreL1, CoreL2 : TFilterCore_SimperSVF;
    CoreR1, CoreR2 : TFilterCore_SimperSVF;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Step(var x1, x2 : single);

    property SampleRate : single read fSampleRate write fSampleRate;

    property Freq : single read fFreq write SetFreq; // range 10?..1/4 Nyquist?
    property Q    : single read fQ    write SetQ;    // range 0..1
  end;

implementation

uses
  Math;

{ TBandPassA }

constructor TBandPassA.Create;
begin
  CoreL1 := TFilterCore_SimperSVF.Create;
  CoreL2 := TFilterCore_SimperSVF.Create;
  CoreR1 := TFilterCore_SimperSVF.Create;
  CoreR2 := TFilterCore_SimperSVF.Create;
end;

destructor TBandPassA.Destroy;
begin
  CoreL1.Free;
  CoreL2.Free;
  CoreR1.Free;
  CoreR2.Free;
  inherited;
end;


procedure TBandPassA.Reset;
begin
  CoreL1.Reset;
  CoreL2.Reset;
  CoreR1.Reset;
  CoreR2.Reset;
end;

procedure TBandPassA.SetFreq(const Value: single);
var
  g : double;
begin
  fFreq := Value;

  g := tan (pi * Value / samplerate);
  //g := Fast_Tan0(pi * Value / samplerate);

  CoreL1.G := G;
  CoreL2.G := G;
  CoreR1.G := G;
  CoreR2.G := G;
end;

procedure TBandPassA.SetQ(const Value: single);
var
  DampingFactor : single;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fQ := Value;

  //Damping factor range is 2..0.    0 = self oscillation.
  DampingFactor := 2 - (Value * 2);


  CoreL1.K := DampingFactor;
  CoreL2.K := DampingFactor;
  CoreR1.K := DampingFactor;
  CoreR2.K := DampingFactor;
end;

procedure TBandPassA.Step(var x1, x2: single);
begin
  x1 := CoreL1.StepAsBandPass(x1);
  x1 := CoreL2.StepAsBandPass(x1);

  x2 := CoreR1.StepAsBandPass(x2);
  x2 := CoreR2.StepAsBandPass(x2);
end;

end.
