{
  A collection of methods useful for DSP tasks,

  NOTE: Try to keep the 'Uses' clause to a bare minimum so that if this unit
  is used inside components, it doesn't force lots of dependencies.
  [Ideally limit to 'VamLib.MoreTypes']
}

unit uDspFunctions;

interface

uses
  VamLib.MoreTypes, Math;

function Sinc(x:single):single; inline;
function SincM(x:single; Harmonics:integer; PeriodLength:integer):single; inline;
function SincM_2(x:single; Harmonics, PeriodLength:single):single; inline;
function Blit(x, Harmonics, PeriodLength:single):single; inline;

function LinearInterpolation(a,b,f:single):single; inline;
function cubic_interpolation(fr, inm1, inp, inp1, inp2: single): single; inline;



//---------------------------------------------------------------------------------------
// Conversions.
//---------------------------------------------------------------------------------------

function SamplesToPPQPos(Samples, SampleRate, Tempo:double):double;




//---------------------------------------------------------------------------------------
// Buffer manipulation functoions.
//---------------------------------------------------------------------------------------


procedure NaiveResample(SourceLength, TargetLength:integer; aSource, aOutput:PSingle);

procedure Normalise(aSource:PSingle; SampleFrames:integer); inline;
procedure CopyBuffer(aSource,aDest:PSingle; SampleFrames:integer); inline;
procedure ZeroBuffer(aSource:PSingle; SampleFrames:integer); inline;
procedure DecimateBy2(aSource:PSingle; SampleFrames:integer); inline;



//---------------------------------------------------------------------------------------
// Window functoions.
//---------------------------------------------------------------------------------------
procedure GenerateBlackmanWindow(SampleFrames:integer; OutputBuffer:PSingle); inline;


//---------------------------------------------------------------------------------------
// Discreet Fourier Transform functoions.
//---------------------------------------------------------------------------------------
procedure DFT(SampleFrames:integer; aRealTime, aImagTime, aRealFreq, aImagFreq:PSingle); inline;
procedure InverseDFT(SampleFrames:integer; aRealTime, aImagTime, aRealFreq, aImagFreq:PSingle); inline;

procedure HalfBandDftFilter(aSource:PSingle; SampleFrames:integer); inline;
procedure DftResample(aSource:PSingle; SampleFrames:integer); inline;


function CalcBinMagnitude(Real, Imag:single):single; inline;
function CalcBinPhase(Real, Imag:single):single; inline;



implementation




function Sinc(x:single):single;
var
  pix:single;
begin
  if x = 0 then
  begin
    result := 0
  end else
  begin
    pix := PI * x;
    result := sin(pix) / pix;
  end;

end;


// SincM(x:single; Harmonics:integer; PeriodLength:integer):single;
//
// for description of function refer to pg 222 of
// "Efficiently-Variable Non-oversamled algorithms in Virtual Analog Music Synthesis - A Root Locus Perspective"
// by Timothy Scott Stilson
//
//  "Harmonics" should be odd, otherwise, apparent frequency will be halved.
//  To generate the maximum amount of harmonics, set "Harmonics" to the largest odd integer not
//  exceeding PeriodLength.
//
//  x range 0 to PeriodLength-1

function SincM(x:single; Harmonics:integer; PeriodLength:integer):single;
begin
  if x = 0 then
  begin
    result := 0;
  end else
  begin
    result := (sin(pi * x * Harmonics / PeriodLength)) / (PeriodLength * sin(pi * x / PeriodLength));
  end;

end;

// function SincM_2(x:single; Harmonics, PeriodLength:single):single; inline;
//
// The same function has above but allows fractional values for PeiodLength and Harmonics
// which is possibly a bad thing.

function SincM_2(x:single; Harmonics, PeriodLength:single):single; inline;
begin
  if x = 0 then
  begin
    result := 0;
  end else
  begin
    result := (sin(pi * x * Harmonics / PeriodLength)) / (PeriodLength * sin(pi * x / PeriodLength));
  end;

end;


function Blit(x, Harmonics, PeriodLength:single):single;
var
  t:single;
begin
  //result := (Harmonics/PeriodLength) * sin(pi*x*Harmonics) / Harmonics * sin( pi*x );

  t := sin(pi * x / PeriodLength);

  if t = 0 then
  begin
    result := 1;
  end else
  begin
    result := (sin(pi * x * Harmonics / PeriodLength)) / (PeriodLength * t);
  end;
  
end;




function LinearInterpolation(a,b,f:single):single; inline;
begin
  result := a + (b - a) * f;
end;

// function cubic_interpolation(fr, inm1, inp, inp1, inp2: single): single;
// This function was copied from Tobybear's DDspUtils.pas unit included with the Delphi VST template
function cubic_interpolation(fr, inm1, inp, inp1, inp2: single): single;
begin
 result := inp + 0.5 * fr * (inp1 - inm1 + fr * (4 * inp1 +
  2 * inm1 - 5 * inp - inp2 + fr * (3 * (inp - inp1) - inm1 +
  inp2)));
end;


function SamplesToPPQPos(Samples, SampleRate, Tempo:double):double;
const
  OneOver60 = 1 / 60;
begin
  result := Samples / SampleRate * Tempo * OneOver60;
end;




procedure NaiveResample(SourceLength, TargetLength:integer; aSource, aOutput:PSingle);
var
  c1:integer;
  a:integer;
  f:single;
  x1,x2:single;
  Source:TArrayOfSingle absolute aSource;
  Output:TArrayOfSingle absolute aOutput;
begin
  for c1 := 0 to TargetLength - 1 do
  begin
    a := c1 * SourceLength div TargetLength;
    f := (c1 * SourceLength / TargetLength) - a;

    if a + 1 < SourceLength then
    begin
      x1 := Source[a];
      x2 := Source[a+1];
      Output[c1] := LinearInterpolation(x1,x2,f);
    end else
    begin
      x1 := Source[a];
      x2 := 0;
      Output[c1] := LinearInterpolation(x1,x2,f);
    end;
  end;

  Output[TargetLength-1] := Source[SourceLength-1];

end;

procedure Normalise(aSource:PSingle; SampleFrames:integer);
var
  Source:TArrayOfSingle absolute aSource;
  c1: Integer;
  a:single;
begin
  a := 0;

  for c1 := 0 to SampleFrames - 1 do
  begin
    if a < abs(Source[c1]) then a := abs(Source[c1]);
  end;

  if a <> 0 then a := 1 / a;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Source[c1] := Source[c1] * a;
  end;

end;

procedure CopyBuffer(aSource,aDest:PSingle; SampleFrames:integer); inline;
var
  c1:integer;
  Source:TArrayOfSingle absolute aSource;
  Dest:TArrayOfSingle absolute aDest;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    Dest[c1] := Source[c1];
  end;
end;

procedure ZeroBuffer(aSource:PSingle; SampleFrames:integer); inline;
var
  c1:integer;
  Source:TArrayOfSingle absolute aSource;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    Source[c1] := 0;
  end;
end;

// DecimateBy2(aSource:PSingle; SampleFrames:integer);
//
// Is useful when resampling signals. Signal will need to be bandlimited before to
// prevent alaising.
// SampleFrames should equal the currect size of the buffer.
// After processing the "aSource" buffer should be resized by the caller.

procedure DecimateBy2(aSource:PSingle; SampleFrames:integer); inline;
var
  c1:integer;
  Source:TArrayOfSingle absolute aSource;
begin
  for c1 := 0 to SampleFrames div 2 - 1 do
  begin
    Source[c1] := Source[c1 * 2];
  end;
end;

procedure GenerateBlackmanWindow(SampleFrames:integer; OutputBuffer:PSingle);
var
  m:integer;
  c1:integer;
  f1,f2,fm:single;
begin
  //TODO: some basic optimisations are needed.

  m := SampleFrames - 1;
  fm := m - 1;

  for c1 := 0 to SampleFrames - 1 do
  begin
    f1 := (2 * PI * c1) / fm;
    f2 := 2 * f1;
    OutputBuffer^ := 0.42 - (0.5 * cos(f1)) + (0.08 * cos(f2));
    inc(OutputBuffer);
  end;

end;

// Discrete Fourier Transform
procedure DFT(SampleFrames:integer; aRealTime, aImagTime, aRealFreq, aImagFreq:PSingle);
var
  k, i:integer;
  sr, si:single;
  x1, x2, x3:single;

  RealFreq:TArrayOfSingle absolute aRealFreq;
  ImagFreq:TArrayOfSingle absolute aImagFreq;
  RealTime:TArrayOfSingle absolute aRealTime;
  ImagTime:TArrayOfSingle absolute aImagTime;
begin

  for k := 0 to SampleFrames - 1 do
  begin
    RealFreq[k] := 0;
    ImagFreq[k] := 0;
  end;

  x1 := 2 * PI / SampleFrames;

  for k := 0 to SampleFrames - 1 do
  begin
    x2 := x1 * k;

    for i := 0 to SampleFrames - 1 do
    begin
      x3 := x2 * i;
      sr := cos(x3);
      si := -sin(x3);
      RealFreq[k] := RealFreq[k] + ((RealTime[i] * sr) - (ImagTime[i] * si));
      ImagFreq[k] := ImagFreq[k] + ((RealTime[i] * si) + (ImagTime[i] * sr));
    end;

  end;

end;

// Inverse Discrete Fourier Transform
procedure InverseDFT(SampleFrames:integer; aRealTime, aImagTime, aRealFreq, aImagFreq:PSingle);
var
  k, i:integer;
  sr, si:single;
  x1, x2, x3:single;
  OneOverSampleFrames:single;
  RealFreq:TArrayOfSingle absolute aRealFreq;
  ImagFreq:TArrayOfSingle absolute aImagFreq;
  RealTime:TArrayOfSingle absolute aRealTime;
  ImagTime:TArrayOfSingle absolute aImagTime;

begin

  for k := 0 to SampleFrames - 1 do
  begin
    RealTime[k] := 0;
    ImagTime[k] := 0;
  end;

  OneOverSampleFrames := 1 / SampleFrames;

  x1 := 2 * PI  / SampleFrames;

  for k := 0 to SampleFrames - 1 do
  begin
    x2 := x1 * k;

    for i := 0 to SampleFrames - 1 do
    begin
      x3 := x2 * i;
      sr := cos(x3);
      si := -sin(x3);
      RealTime[k] := RealTime[k] + ((RealFreq[i] * sr) + (ImagFreq[i] * si));
      ImagTime[k] := ImagTime[k] + ((RealFreq[i] * si) - (ImagFreq[i] * sr));
    end;

    RealTime[k] := RealTime[k] * OneOverSampleFrames;
    ImagTime[k] := ImagTime[k] * OneOverSampleFrames;

  end;


end;

procedure HalfBandDftFilter(aSource:PSingle; SampleFrames:integer); inline;
var
  c1:integer;
  Index1,Index2:integer;
  xTime:TArrayOfSingle absolute aSource;
  yTime:TArrayOfSingle;
  xFreq:TArrayOfSingle;
  yFreq:TArrayOfSingle;
begin
  SetLength(yTime,SampleFrames);
  SetLength(xFreq,SampleFrames);
  SetLength(yFreq,SampleFrames);

  ZeroBuffer(@yTime[0],SampleFrames);

  DFT(SampleFrames,@xTime[0],@yTime[0],@xFreq[0],@yFreq[0]);

  Index1 := (SampleFrames div 8);
  Index2 := SampleFrames - Index1;
  for c1 := Index1 to Index2 - 1 do
  begin
    xFreq[c1] := 0;
    yFreq[c1] := 0;
  end;

  InverseDFT(SampleFrames,@xTime[0],@yTime[0],@xFreq[0],@yFreq[0]);

  SetLength(yTime,0);
  SetLength(xFreq,0);
  SetLength(yFreq,0);
end;




procedure DftResample(aSource:PSingle; SampleFrames:integer); inline;
var
  xTime:TArrayOfSingle;
  yTime:TArrayOfSingle;
  xFreq:TArrayOfSingle;
  yFreq:TArrayOfSingle;
begin
  SetLength(xTime,SampleFrames);
  SetLength(yTime,SampleFrames);
  SetLength(xFreq,SampleFrames);
  SetLength(yFreq,SampleFrames);

  DFT(SampleFrames,@xTime[0],@yTime[0],@xFreq[0],@yFreq[0]);
  InverseDFT(SampleFrames,@xTime[0],@yTime[0],@xFreq[0],@yFreq[0]);

  SetLength(xTime,0);
  SetLength(yTime,0);
  SetLength(xFreq,0);
  SetLength(yFreq,0);
end;


function CalcBinMagnitude(Real, Imag:single):single; inline;
begin
  result := sqrt((Real * Real) + (Imag * Imag));
end;

function CalcBinPhase(Real, Imag:single):single; inline;
const
  OneOver90 = 1 / 90;
//var
  //x:single;
begin
  //x :=  ArcTan(Imag / Real);
  //result := RadToDeg(x) * OneOver90;
  result := RadToDeg(ArcTan(Imag / Real)) * OneOver90;
end;







end.
