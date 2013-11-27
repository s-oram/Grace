unit soFilter.DistortionA;

interface

type
  TDistortionA = class
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
    InputGain : single;
    OutputGain : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure ControlRateStep; inline;
    procedure AudioRateStep(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write SetSampleRate;


    property Par1 : single read fPar1 write SetPar1;
    property Par2 : single read fPar2 write SetPar2;  //Input Gain. range 0..1
    property Par3 : single read fPar3 write SetPar3;  //Output Gain. range 0..1

    //property Par1 : single read fPar1 write SetPar1;
    //property Par2 : single read fPar2 write SetPar2;
    //property Par3 : single read fPar3 write SetPar3;
  end;

implementation

uses
  Math;

{ TDistortionA }

constructor TDistortionA.Create;
begin

end;

destructor TDistortionA.Destroy;
begin

  inherited;
end;

procedure TDistortionA.SetPar1(const Value: single);
begin
  fPar1 := Value;
end;

procedure TDistortionA.SetPar2(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fPar2 := Value;

  InputGain := fPar2 * fPar2 * fPar2  * fPar2 * 16;
end;

procedure TDistortionA.SetPar3(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fPar3 := Value;

  OutputGain := fPar3 * fPar3;
end;

procedure TDistortionA.Reset;
begin

end;

procedure TDistortionA.SetSampleRate(const Value: single);
begin

end;

procedure TDistortionA.ControlRateStep;
begin

end;

procedure TDistortionA.AudioRateStep(var x1, x2: single);
begin
  x1 := Tanh(x1 * InputGain) * OutputGain;
  x2 := Tanh(x2 * InputGain) * OutputGain;
end;




end.
