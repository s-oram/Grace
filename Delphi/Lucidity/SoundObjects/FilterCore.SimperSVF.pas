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
  private
    class procedure StepFilter_Pascal(var Data : TDualSimperSVFData);
    class procedure StepFilter_Alt(var Data : TDualSimperSVFData);
    class procedure StepFilter_asm(var Data : TDualSimperSVFData);

    class procedure GetLowpassOutput(var Data : TDualSimperSVFData);
    class procedure GetBandpassOutput(var Data : TDualSimperSVFData);
    class procedure GetHighpassOutput(var Data : TDualSimperSVFData);

    class procedure ApplyNonLinearMagic_Pascal(var Data : TDualSimperSVFData);
  public
    class procedure StepAsLowPass(var Data : TDualSimperSVFData); inline;
    class procedure StepAsBandPass(var Data : TDualSimperSVFData); inline;
    class procedure StepAsHighPass(var Data : TDualSimperSVFData); inline;
  end;



const
  Factorial_0 = 1;
  Factorial_1 = 1;
  Factorial_2 = 2;
  Factorial_3 = 6;
  Factorial_4 = 24;
  Factorial_5 = 120;
  Factorial_6 = 720;
  Factorial_7 = 5040;
  Factorial_8 = 40320;

  OneOverFactorial_0 = 1/1;
  OneOverFactorial_1 = 1/1;
  OneOverFactorial_2 = 1/2;
  OneOverFactorial_3 = 1/6;
  OneOverFactorial_4 = 1/24;
  OneOverFactorial_5 = 1/120;
  OneOverFactorial_6 = 1/720;
  OneOverFactorial_7 = 1/5040;
  OneOverFactorial_8 = 1/40320;

implementation

uses
  VamLib.Utils,
  Math;

function FastExp(const x:double) : double; inline;
// NOTE: an exp() function is approximated with the taylor series.
var
  ax2 : double;
  ax3 : double;
  ax4 : double;
  ax5 : double;
  ax6 : double;
  ax7 : double;
  ax8 : double;
begin
  ax2 := x * x;
  ax3 := ax2 * x;
  ax4 := ax3 * x;
  ax5 := ax4 * x;
  ax6 := ax5 * x;
  ax7 := ax6 * x;
  ax8 := ax7 * x;

  {
  result := 1 + x
         + (ax2 * OneOverFactorial_2)
         + (ax3 * OneOverFactorial_3)
         + (ax4 * OneOverFactorial_4)
         + (ax5 * OneOverFactorial_5)
         + (ax6 * OneOverFactorial_6)
         + (ax7 * OneOverFactorial_7)
         + (ax8 * OneOverFactorial_8);
  }

  result := 1 + x
         + (ax2 * OneOverFactorial_2)
         + (ax3 * OneOverFactorial_3)
         + (ax4 * OneOverFactorial_4);
end;


function FastTanH(const x:double) : double; inline;
// A TanH approximation. Extracted from the delphi Tanh() source.
var
  y : extended;
begin
  case TDoubleRec(x).SpecialType of
    fsPositive,
    fsNegative:
    begin
      y := FastExp(2*x);
      Result := 1 - (2/(y + 1));
    end
  else
    result := x;
  end;
end;

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

class procedure TSimperVCF.StepFilter_Pascal(var Data: TDualSimperSVFData);
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
end;

class procedure TSimperVCF.StepFilter_Alt(var Data: TDualSimperSVFData);
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

end;

class procedure TSimperVCF.StepFilter_asm(var Data: TDualSimperSVFData);
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
  //movupd xmm3, [Data].TDualSimperSVFData.g[0]
  mulpd xmm7, xmm3

  //Temp3[0] := Temp3[0] + Data.v2z[0];
  addpd xmm7, xmm1

  //Data.v2[0]  := Temp3[0];
  movupd [Data].TDualSimperSVFData.v2[0], xmm7

  //Data.v0z[0] := Data.v0[0];
  movupd [Data].TDualSimperSVFData.v0z[0], xmm2
end;

class procedure TSimperVCF.GetLowpassOutput(var Data: TDualSimperSVFData);
asm
  //Calc outputs...
  //Data.Ouput[0] := Data.v2[0];
  movupd xmm7, [Data].TDualSimperSVFData.v2[0]
  movupd [Data].TDualSimperSVFData.Ouput[0], xmm7
end;

class procedure TSimperVCF.GetBandpassOutput(var Data: TDualSimperSVFData);
asm
  //Calc outputs...
  //Data.Ouput[0] := Data.v2[0];
  movupd xmm7, [Data].TDualSimperSVFData.v1[0]
  movupd [Data].TDualSimperSVFData.Ouput[0], xmm7
end;

class procedure TSimperVCF.GetHighpassOutput(var Data: TDualSimperSVFData);
asm
  //Data.Ouput[0] := Data.v0[0] - Data.k[0]* Data.v1[0] - Data.v2[0];
  //Data.Ouput[1] := Data.v0[1] - Data.k[1]* Data.v1[1] - Data.v2[1];

  movupd xmm0, [Data].TDualSimperSVFData.v0[0]
  movupd xmm1, [Data].TDualSimperSVFData.v1[0]
  movupd xmm2, [Data].TDualSimperSVFData.v2[0]
  movupd xmm3, [Data].TDualSimperSVFData.k[0]

  mulpd xmm3, xmm1
  subpd xmm0, xmm3
  subpd xmm0, xmm2

  movupd [Data].TDualSimperSVFData.Ouput[0], xmm0
end;

class procedure TSimperVCF.ApplyNonLinearMagic_Pascal(var Data: TDualSimperSVFData);
begin
  //TODO: maybe this non-linear magic here can be optimised somewhat.
  Data.v2[0] := FastTanH(Data.v2[0] * 0.125) * 8;
  Data.v2[1] := FastTanH(Data.v2[1] * 0.125) * 8;
end;

class procedure TSimperVCF.StepAsLowPass(var Data: TDualSimperSVFData);
begin
  //TODO: There are some small oppurtunities for optimisations here.
  // Perhaps the Data varable can be stored in the register or something
  // and
  StepFilter_asm(Data);
  ApplyNonLinearMagic_Pascal(Data);
  GetLowpassOutput(Data);
end;

class procedure TSimperVCF.StepAsBandPass(var Data: TDualSimperSVFData);
begin
  StepFilter_asm(Data);
  ApplyNonLinearMagic_Pascal(Data);
  GetBandpassOutput(Data);
end;

class procedure TSimperVCF.StepAsHighPass(var Data: TDualSimperSVFData);
begin
  StepFilter_asm(Data);
  ApplyNonLinearMagic_Pascal(Data);
  GetHighpassOutput(Data);
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

procedure TDualSimperSVFData.SetGK(const aG, aK : double);
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
