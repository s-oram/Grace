unit soFilter.LowPassB;

interface

uses
  eeDsp,
  eeFastCode,
  soFilter.FilterBCore;

type
  TLowPassB = class
  private
    fSampleRate: single;
    fFreq: single;
    fQ: single;
    procedure SetFreq(const Value: single);
    procedure SetQ(const Value: single);
  protected
    CoreL1 : TFilterCoreB;
    CoreR1 : TFilterCoreB;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Step(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write fSampleRate;

    property Freq : single read fFreq write SetFreq; // range 10?..1/4 Nyquist?
    property Q    : single read fQ    write SetQ;    // range 0..1
  end;

implementation

uses
  Math;

{ TLowPassA }

constructor TLowPassB.Create;
begin
  CoreL1 := TFilterCoreB.Create;
  CoreR1 := TFilterCoreB.Create;
end;

destructor TLowPassB.Destroy;
begin
  CoreL1.Free;
  CoreR1.Free;
  inherited;
end;


procedure TLowPassB.Reset;
begin
  CoreL1.Reset;
  CoreR1.Reset;
end;

procedure TLowPassB.SetFreq(const Value: single);
var
  g : double;
begin
  fFreq := Value;

  //g := tan (pi * Value / samplerate);
  g := Fast_Tan0(pi * Value / samplerate);

  CoreL1.G := G;
  CoreR1.G := G;
end;

procedure TLowPassB.SetQ(const Value: single);
var
  DampingFactor : single;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fQ := Value;

  //Damping factor range is 2..0.    0 = self oscillation.
  DampingFactor := 2 - (Value * 2);

  CoreL1.K := DampingFactor;
  CoreR1.K := DampingFactor;
end;

procedure TLowPassB.Step(var x1, x2: single);
begin
  x1 := CoreL1.StepAsLowPass(x1);
  x2 := CoreR1.StepAsLowPass(x2);
end;

end.
