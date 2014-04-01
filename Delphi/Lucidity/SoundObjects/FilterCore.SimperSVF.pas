// http://music.columbia.edu/pipermail/music-dsp/2011-May/069905.html

unit FilterCore.SimperSVF;

interface

const
  //A small value to prevent denormals.
  kDenormal    = 1.0e-24;

type
  TFilterCore_SimperSvf = class
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

function NonLinearSpice(x:double):double;
function NonLinearSpice2(x:double):double;


type
  TDualSimperSVFData = record
    Ouput  : array[0..1] of double;
    Input  : array[0..1] of double;
    v0     : array[0..1] of double;
    v1     : array[0..1] of double;
    v2     : array[0..1] of double;
    v0z    : array[0..1] of double;
    v1z    : array[0..1] of double;
    v2z    : array[0..1] of double;

    Factor1 : array[0..1] of double;
    Factor2 : array[0..1] of double;

    //G := tan (pi * Cutoff / SampleRate);
    G      : array[0..1] of double;
    // K is a damping factor. Range 2..0. (for resonance)
    K      : array[0..1] of double;

    procedure Reset;
    procedure SetGK(const aG, aK : double);
  end;


  TSimperVCF = class
  public
    class procedure StepAsLowPass(var Data : TDualSimperSVFData);
    class procedure StepAsLowPass_ALT1(var Data : TDualSimperSVFData);
    class procedure StepAsLowPass_asm(var Data : TDualSimperSVFData);
    class procedure StepAsBandPass(var Data : TDualSimperSVFData);
    class procedure StepAsHighPass(var Data : TDualSimperSVFData);
  end;


implementation

uses
  VamLib.Utils,
  Math;

function NonLinearSpice2(x:double):double;
const
  a : double = 0.007825;
  b : double = 1.017189;
  c : double = -0.271803;
  //d : double = ;
begin
  x := Clamp(x, -2, 2);
  result := sign(x) * (a + (b * abs(x)) + (c * x * x));
end;

function NonLinearSpice(x:double):double;
const
  a : double = 0;
  b : double = 0.833588;
  c : double = 0;
  d : double = -0.090188;
var
  tx : double;

  xa  : double;
  x2  : double;
  x3  : double;
  x4  : double;
  x7  : double;
  res : double;
begin
  //result := tanh(x);

  //tx := (x*x + 1);
  //tx := power(tx, 0.5);
  //result := ln(x + tx)

  //result := x + (x * x * x) * 0.3;


  xa := abs(x);
  x2 := xa * xa;
  x3 := xa * x2;
  x4 := x2 * x2;
  x7 := x3 * x4;
  res := (1.0 - 1.0 / (1.0 + xa + x2 + 0.58576695 * x3 + 0.55442112 * x4 + 0.057481508 * x7));
  result := res * sign(x);


  //x := Clamp(x * 1.5, -1.75, 1.75);
  //result := (a + (b * x) + (c * x * x) + (d * x * x * x));
end;


{ TFilterCore_SimperSvf }

constructor TFilterCore_SimperSvf.Create;
begin
  Reset;
end;

destructor TFilterCore_SimperSvf.Destroy;
begin

  inherited;
end;

procedure TFilterCore_SimperSvf.Reset;
begin
  v0 := 0;
  v1 := 0;
  v2 := 0;
  v0z := 0;
  v1z := 0;
  v2z := 0;
end;

function TFilterCore_SimperSvf.StepAsBandPass(const x1: double): double;
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

function TFilterCore_SimperSvf.StepAsHighPass(const x1: double): double;
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

function TFilterCore_SimperSvf.StepAsLowPass(const x1: double): double;
begin
  v1z := v1;
  v2z := v2;
  v0  := x1 + kDenormal;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  //v1 := NonLinearSpice(v1 * 0.5) * 2;
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  //== Lowpass output ==
  v2 := NonLinearSpice(v2 * 0.25) * 4;
  //result := v2;
  result := NonLinearSpice(v2 * 0.75) * 2;

  //==Outputs==
  //band  := v1;
  //low   := v2;
  //high  := v0 - k*v1 - v2;
  //notch := high + low;
  //peak  := high - low;
end;

function TFilterCore_SimperSvf.StepAsNotch(const x1: double): double;
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

function TFilterCore_SimperSvf.StepAsPeak(const x1: double): double;
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

{ TSimperVCF }

class procedure TSimperVCF.StepAsLowPass(var Data: TDualSimperSVFData);
begin
  Data.v1z[0] := Data.v1[0];
  Data.v2z[0] := Data.v2[0];
  Data.v0[0]  := Data.Input[0];
  Data.v1[0]  := Data.v1z[0] + Data.g[0] * (Data.v0[0] + Data.v0z[0] - Data.Factor1[0]*Data.v1z[0] - 2*Data.v2z[0]) * Data.Factor2[0];
  Data.v2[0]  := Data.v2z[0] + Data.g[0] * (Data.v1[0] + Data.v1z[0]);
  Data.v0z[0] := Data.v0[0];

  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - Data.Factor1[1]*Data.v1z[1] - 2*Data.v2z[1]) * Data.Factor2[1];
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];


  //Calc outputs...
  Data.Ouput[0] := Data.v2[0];
  Data.Ouput[1] := Data.v2[1];
end;

class procedure TSimperVCF.StepAsLowPass_ALT1(var Data: TDualSimperSVFData);
var
  Temp1 : array[0..1] of double;
  Temp2 : array[0..1] of double;
  Temp3 : array[0..1] of double;
begin
  Data.v1z[0] := Data.v1[0];
  Data.v2z[0] := Data.v2[0];
  Data.v0[0]  := Data.Input[0];


  Temp1[0] := Data.Factor1[0]*Data.v1z[0];
  Temp2[0] := Data.v0[0] - Temp1[0];
  Temp2[0] := Temp2[0] - Data.v2z[0];
  Temp2[0] := Temp2[0] - Data.v2z[0];
  Temp2[0] := Temp2[0] + Data.v0z[0];
  Temp2[0] := Temp2[0] * Data.Factor2[0];
  Temp2[0] := Temp2[0] * Data.g[0];
  Temp2[0] := Temp2[0] + Data.v1z[0];

  Data.v1[0]  := Temp2[0];


  Temp3[0] := Data.v1[0];
  Temp3[0] := Temp3[0] + Data.v1z[0];
  Temp3[0] := Temp3[0] * Data.g[0];
  Temp3[0] := Temp3[0] + Data.v2z[0];
  Data.v2[0]  := Temp3[0];

  Data.v0z[0] := Data.v0[0];

  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - Data.Factor1[1]*Data.v1z[1] - 2*Data.v2z[1]) * Data.Factor2[1];
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];


  //Calc outputs...
  Data.Ouput[0] := Data.v2[0];
  Data.Ouput[1] := Data.v2[1];
end;

class procedure TSimperVCF.StepAsLowPass_asm(var Data: TDualSimperSVFData);
//Temp1 = xmm5
//Temp2 = xmm6
//Temp3 = xmm7
asm
  //Data.v1z[0] := Data.v1[0];
  movupd xmm0, [Data].TDualSimperSVFData.v1[0]
  movupd [Data].TDualSimperSVFData.v1z[0], xmm0

  //Data.v2z[0] := Data.v2[0];
  movupd xmm1, [Data].TDualSimperSVFData.v2[0]
  movupd [Data].TDualSimperSVFData.v2z[0], xmm1


  //Data.v0[0]  := Data.Input[0];
  movupd xmm2, [Data].TDualSimperSVFData.Input[0]
  movupd [Data].TDualSimperSVFData.v0[0], xmm2


  //Temp1[0] := Data.Factor1[0]*Data.v1z[0];
  movupd xmm5, [Data].TDualSimperSVFData.Factor1[0]
  mulpd xmm5, xmm0

  //Temp2[0] := Data.v0[0];
  movupd xmm6, xmm2

  //Temp2[0] := Temp2[0] - Temp1[0];
  subpd xmm6, xmm5

  //Temp2[0] := Temp2[0] - Data.v2z[0];
  subpd xmm6, xmm1

  //Temp2[0] := Temp2[0] - Data.v2z[0];
  subpd xmm6, xmm1

  //Temp2[0] := Temp2[0] + Data.v0z[0];
  movupd xmm3, [Data].TDualSimperSVFData.v0z[0]
  addpd xmm6, xmm3

  //Temp2[0] := Temp2[0] * Data.Factor2[0];
  movupd xmm3, [Data].TDualSimperSVFData.Factor2[0]
  mulpd xmm6, xmm3

  //Temp2[0] := Temp2[0] * Data.g[0];
  movupd xmm3, [Data].TDualSimperSVFData.g[0]
  mulpd xmm6, xmm3

  //Temp2[0] := Temp2[0] + Data.v1z[0];
  addpd xmm6, xmm0

  //Data.v1[0]  := Temp2[0];
  movupd [Data].TDualSimperSVFData.v1[0], xmm6


  //Temp3[0] := Data.v1[0];
  movupd xmm7, xmm6

  //Temp3[0] := Temp3[0] + Data.v1z[0];
  addpd xmm7, xmm0

  //Temp3[0] := Temp3[0] * Data.g[0];
  movupd xmm3, [Data].TDualSimperSVFData.g[0]
  mulpd xmm7, xmm3

  //Temp3[0] := Temp3[0] + Data.v2z[0];
  addpd xmm7, xmm1

  //Data.v2[0]  := Temp3[0];
  movupd [Data].TDualSimperSVFData.v2[0], xmm7

  //Data.v0z[0] := Data.v0[0];
  movupd [Data].TDualSimperSVFData.v0z[0], xmm2

  //Calc outputs...
  //Data.Ouput[0] := Data.v2[0];
  movupd [Data].TDualSimperSVFData.Ouput[0], xmm7
end;

{
class procedure TSimperVCF.StepAsLowPass_asm(var Data: TDualSimperSVFData);
asm
  //Data.v1z[0] := Data.v1[0];
  movupd xmm0, [Data].TDualSimperSVFData.v1[0]
  movupd [Data].TDualSimperSVFData.v1z[0], xmm0

  //Data.v2z[0] := Data.v2[0];
  movupd xmm1, [Data].TDualSimperSVFData.v2[0]
  movupd [Data].TDualSimperSVFData.v2z[0], xmm1

  //Data.v0[0]  := Data.Input[0];
  movupd xmm2, [Data].TDualSimperSVFData.Input[0]
  movupd [Data].TDualSimperSVFData.v0[0], xmm2

  //Temp3[0]    := Data.Factor1[0]*Data.v1z[0];
  movupd xmm3, [Data].TDualSimperSVFData.Factor1[0]
  mulpd xmm3, xmm0

  //Temp2[0]    := Data.v0[0];
  //Temp2[0]    := Temp2[0] + Data.v0z[0];
  movupd xmm4, [Data].TDualSimperSVFData.v0z[0]
  addpd xmm4, xmm2

  //Temp2[0]    := Temp2[0] - Temp3[0];
  subpd xmm4, xmm3



  //Temp1[0]    := 2*Data.v2z[0];
  // ** xmm3 is reused here **
  movupd xmm3, [Data].TDualSimperSVFData.v2z[0]
  addpd xmm3, xmm3

  //Temp2[0]    := Temp2[0] - Temp1[0];
  subpd xmm2, xmm3

  //Temp2[0]    := Temp2[0] * Data.Factor2[0];
  movupd xmm5, [Data].TDualSimperSVFData.Factor2[0]
  mulpd xmm2, xmm5

  //Temp2[0]    := Temp2[0] * Data.g[0];
  movupd xmm5, [Data].TDualSimperSVFData.g[0]
  mulpd xmm2, xmm5

  //Data.v1[0]  := Temp2[0] + Data.v1z[0];
  addpd xmm5, xmm0
  movupd [Data].TDualSimperSVFData.v1[0], xmm5


  //Temp2[0] := Data.v1[0] + Data.v1z[0];
  addpd xmm5, xmm0

  //Temp2[0] := Temp2[0] * Data.g[0];
  addpd xmm5, [Data].TDualSimperSVFData.g[0]


  //Temp2[0] := Temp2[0] * Data.v2z[0];
  //Data.v2[0]  := Temp2[0];




  //Data.v0z[0] := Data.v0[0];

  //Data.v1z[1] := Data.v1[1];
  //Data.v2z[1] := Data.v2[1];
  //Data.v0[1]  := Data.Input[1];
  //Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - Data.Factor1[1]*Data.v1z[1] - 2*Data.v2z[1]) * Data.Factor2[1];
  //Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  //Data.v0z[1] := Data.v0[1];


  //Calc outputs...
  //Data.Ouput[0] := Data.v2[0];
  //Data.Ouput[1] := Data.v2[1];
end;
}

class procedure TSimperVCF.StepAsBandPass(var Data: TDualSimperSVFData);
begin
  Data.v1z[0] := Data.v1[0];
  Data.v2z[0] := Data.v2[0];
  Data.v0[0]  := Data.Input[0];
  Data.v1[0]  := Data.v1z[0] + Data.g[0] * (Data.v0[0] + Data.v0z[0] - 2*(Data.g[0] + Data.k[0])*Data.v1z[0] - 2*Data.v2z[0]) / (1 + Data.g[0]*(Data.g[0] + Data.k[0]));
  Data.v2[0]  := Data.v2z[0] + Data.g[0] * (Data.v1[0] + Data.v1z[0]);
  Data.v0z[0] := Data.v0[0];

  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - 2*(Data.g[1] + Data.k[1])*Data.v1z[1] - 2*Data.v2z[1]) / (1 + Data.g[1]*(Data.g[1] + Data.k[1]));
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];

  // calc outputs..
  Data.Ouput[0] := Data.v1[0];
  Data.Ouput[1] := Data.v1[1];
end;

class procedure TSimperVCF.StepAsHighPass(var Data: TDualSimperSVFData);
begin
  Data.v1z[0] := Data.v1[0];
  Data.v2z[0] := Data.v2[0];
  Data.v0[0]  := Data.Input[0];
  Data.v1[0]  := Data.v1z[0] + Data.g[0] * (Data.v0[0] + Data.v0z[0] - 2*(Data.g[0] + Data.k[0])*Data.v1z[0] - 2*Data.v2z[0]) / (1 + Data.g[0]*(Data.g[0] + Data.k[0]));
  Data.v2[0]  := Data.v2z[0] + Data.g[0] * (Data.v1[0] + Data.v1z[0]);
  Data.v0z[0] := Data.v0[0];

  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - 2*(Data.g[1] + Data.k[1])*Data.v1z[1] - 2*Data.v2z[1]) / (1 + Data.g[1]*(Data.g[1] + Data.k[1]));
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];

  //== Highpass output ==
  Data.Ouput[0] := Data.v0[0] - Data.k[0]* Data.v1[0] - Data.v2[0];
  Data.Ouput[1] := Data.v0[1] - Data.k[1]* Data.v1[1] - Data.v2[1];
end;



{ TDualSimperSVFData }

procedure TDualSimperSVFData.Reset;
begin
  v0[0] := 0;
  v1[0] := 0;
  v2[0] := 0;
  v0z[0] := 0;
  v1z[0] := 0;
  v2z[0] := 0;

  v0[1] := 0;
  v1[1] := 0;
  v2[1] := 0;
  v0z[1] := 0;
  v1z[1] := 0;
  v2z[1] := 0;
end;

procedure TDualSimperSVFData.SetGK(const aG, aK: double);
begin
  G[0] := aG;
  G[1] := aG;

  k[0] := aK;
  k[1] := aK;

  Factor1[0] := 2 * (aG + aK);
  Factor1[1] := 2 * (aG + aK);

  Factor2[0] := 1 / (1 + aG * (aG + aK));
  Factor2[1] := 1 / (1 + aG * (aG + aK));
end;

end.
