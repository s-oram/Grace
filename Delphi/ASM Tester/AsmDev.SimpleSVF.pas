unit AsmDev.SimpleSVF;

interface

type

  TDualSimperSVFData = record
    Input  : array[0..1] of double;
    Ouput  : array[0..1] of double;
    v0     : array[0..1] of double;
    v1     : array[0..1] of double;
    v2     : array[0..1] of double;
    v0z    : array[0..1] of double;
    v1z    : array[0..1] of double;
    v2z    : array[0..1] of double;

    //G := tan (pi * Cutoff / SampleRate);
    G      : array[0..1] of double;
    // K is a damping factor. Range 2..0. (for resonance)
    K      : array[0..1] of double;
  end;


  TSimperVCF = class
  public
    class procedure StepAsLowPass(var Data : TDualSimperSVFData);
    class procedure StepAsBandPass(var Data : TDualSimperSVFData);
    class procedure StepAsHighPass(var Data : TDualSimperSVFData);
  end;

implementation


class procedure TSimperVCF.StepAsLowPass(var Data: TDualSimperSVFData);
begin
  Data.v1z[0] := Data.v1[0];
  Data.v2z[0] := Data.v2[0];
  Data.v0[0]  := Data.Input[0];
  Data.v1[0]  := Data.v1z[0] + Data.g[0] * (Data.v0[0] + Data.v0z[0] - 2*(Data.g[0] + Data.k[0])*Data.v1z[0] - 2*Data.v2z[0]) / (1 + Data.g[0]*(Data.g[0] + Data.k[0]));
  Data.v2[0]  := Data.v2z[0] + Data.g[0] * (Data.v1[0] + Data.v1z[0]);
  Data.v0z[0] := Data.v0[0];

  Data.Ouput[0] := Data.v2[0];

  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - 2*(Data.g[1] + Data.k[1])*Data.v1z[1] - 2*Data.v2z[1]) / (1 + Data.g[1]*(Data.g[1] + Data.k[1]));
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];

  Data.Ouput[1] := Data.v2[1];
end;

class procedure TSimperVCF.StepAsBandPass(var Data: TDualSimperSVFData);
begin
  Data.v1z[0] := Data.v1[0];
  Data.v2z[0] := Data.v2[0];
  Data.v0[0]  := Data.Input[0];
  Data.v1[0]  := Data.v1z[0] + Data.g[0] * (Data.v0[0] + Data.v0z[0] - 2*(Data.g[0] + Data.k[0])*Data.v1z[0] - 2*Data.v2z[0]) / (1 + Data.g[0]*(Data.g[0] + Data.k[0]));
  Data.v2[0]  := Data.v2z[0] + Data.g[0] * (Data.v1[0] + Data.v1z[0]);
  Data.v0z[0] := Data.v0[0];

  Data.Ouput[0] := Data.v1[0];

  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - 2*(Data.g[1] + Data.k[1])*Data.v1z[1] - 2*Data.v2z[1]) / (1 + Data.g[1]*(Data.g[1] + Data.k[1]));
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];

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

  //== Highpass output ==
  Data.Ouput[0] := Data.v0[0] - Data.k[0]* Data.v1[0] - Data.v2[0];



  Data.v1z[1] := Data.v1[1];
  Data.v2z[1] := Data.v2[1];
  Data.v0[1]  := Data.Input[1];
  Data.v1[1]  := Data.v1z[1] + Data.g[1] * (Data.v0[1] + Data.v0z[1] - 2*(Data.g[1] + Data.k[1])*Data.v1z[1] - 2*Data.v2z[1]) / (1 + Data.g[1]*(Data.g[1] + Data.k[1]));
  Data.v2[1]  := Data.v2z[1] + Data.g[1] * (Data.v1[1] + Data.v1z[1]);
  Data.v0z[1] := Data.v0[1];

  Data.Ouput[1] := Data.v0[1] - Data.k[1]* Data.v1[1] - Data.v2[1];
end;






end.
