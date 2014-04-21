unit soFilter.FilterBCore;

interface

uses
  eeFastCode;

const
  //A small value to prevent denormals.
  kDenormal    = 1.0e-24;

type
  TFilterCoreB = class
  private
    v0  : double;
    v1  : double;
    v2  : double;
    v0z : double;
    v1z : double;
    v2z : double;
    fg, fk : double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    //g := tan (pi * Cutoff / SampleRate);
    property G : double read fG write fG;

    // K is a damping factor. Range 2..0.
    property K : double read fK write fK;

    function StepAsLowPass(const x1:double):double; inline;
    function StepAsBandPass(const x1:double):double; inline;
    function StepAsHighPass(const x1:double):double; inline;
    function StepAsNotch(const x1:double):double; inline;  //untested.
    function StepAsPeak(const x1:double):double;  inline;  //untested.
  end;

implementation

{ TFilterCore_SimperSvf }

constructor TFilterCoreB.Create;
begin
  Reset;
end;

destructor TFilterCoreB.Destroy;
begin

  inherited;
end;

procedure TFilterCoreB.Reset;
begin
  v0 := 0;
  v1 := 0;
  v2 := 0;
  v0z := 0;
  v1z := 0;
  v2z := 0;
end;

function TFilterCoreB.StepAsBandPass(const x1: double): double;
begin
  v1z := v1;
  v2z := v2;
  v0  := x1 + kDenormal;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  //== bandpass output ==
  result := v1;
end;

function TFilterCoreB.StepAsHighPass(const x1: double): double;
begin
  v1z := v1;
  v2z := v2;
  v0  := x1 + kDenormal;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  //== Highpass output ==
  result := v0 - k*v1 - v2;
end;

function TFilterCoreB.StepAsLowPass(const x1: double): double;
begin
  v1z := v1;
  v2z := v2;
  v0  := (x1) + kDenormal;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  //== Lowpass output ==
  result := Fast_Tan0(v2 * 2) * 0.5;

  //==Outputs==
  //band  := v1;
  //low   := v2;
  //high  := v0 - k*v1 - v2;
  //notch := high + low;
  //peak  := high - low;
end;

function TFilterCoreB.StepAsNotch(const x1: double): double;
var
  Low, High : double;
begin
  v1z := v1;
  v2z := v2;
  v0  := x1 + kDenormal;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  //== Notch output ==
  low   := v2;
  high  := v0 - k*v1 - v2;
  result := High + Low;
end;

function TFilterCoreB.StepAsPeak(const x1: double): double;
var
  Low, High : double;
begin
  v1z := v1;
  v2z := v2;
  v0  := x1 + kDenormal;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  //== Peak output ==
  low   := v2;
  high  := v0 - k*v1 - v2;
  result := High - Low;
end;

end.
