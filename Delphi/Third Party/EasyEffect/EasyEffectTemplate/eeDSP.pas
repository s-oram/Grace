unit eeDsp;

interface

uses
  Math;

const
  //A small value to prevent denormals.
  kDenormal    = 1.0e-24;
  GlobalTuneA4 = 440;

function SameValue_Single(const A, B: Single; Epsilon: Single): Boolean; inline;

function MillisecondsToSamples(ms, SampleRate: single): single; inline;
function SamplesToMilliseconds(Samples, SampleRate: single): single; inline;

function BeatsToSamples(SyncFactor, BPM, SampleRate: double): double; inline;
function SamplesToBeats(Samples, BPM, SampleRate: double): double; inline;

function SyncToSamples(SyncFactor, BPM, SampleRate: double): double; inline;
function SamplesToSync(Samples, BPM, SampleRate: double): double; inline;

function DecibelsToVoltage(dB:single):single; inline;
function VoltageToDecibels(Voltage:single):single; inline;

function DecibelsToLinear(dB:single):single; inline;
function LinearToDecibels(Linear:single):single; inline;

function ExponentialShift(const StartingValue, OctaveShift:single):single; inline;

procedure CalcSyncTimeA(Value:single; out SyncNumerator, SyncDenominator:integer);
function CalcSyncTimeA_Inv(SyncNumerator, SyncDenominator:integer):single;

  // PitchShiftToRate can be used for calculating the playback rate of a sample
  // to achieve a desired pitch shift.
  // ShiftAmount - Semitones.
  // result will be from 0..inf+
function PitchShiftToRate(ShiftAmount:double):double; inline;

  // PolarToCartesian()
  // - Angle is in degress.
  // - Directly left is 0 degrees. Degrees increase counter-clockwise.
  // http://en.wikipedia.org/wiki/Polar_coordinate_system
procedure PolarToCartesian(const Angle, Distance:single; out x, y:single); inline;

function CalcRandomisedValue(const InitialValue, RandomiseAmt:single):single; inline;


// VstFloatToInt() is a special rounding function for converting float values ranging from 0..1 to
// an integer range.
function VstFloatToInt(FloatValue : single; const MinIntValue, MaxIntValue:integer):integer; inline; overload;
function VstFloatToInt(const FloatValue : single; const NumberOfValues : integer):integer; inline; overload;

//=============================================================
// ParScale functions.
// All ParScale functions operate on values ranging 0 to 1,
// With valid inputs, all functions will return values ranging
// between 0 and 1.
//=============================================================

function ParScaleSquare(Value:single):single; inline;
function ParScaleSquareInv(Value:single):single; inline;
function ParScaleCube(Value:single):single; inline;
function ParScaleCubeInv(Value:single):single; inline;


// ParScaleCustom is used to compress/expand sections of the parameter range.
// The 0-1 parameter range is split into 3 segments. The cut points are marked by
// the A and B values.
// The mA and mB values are the transposed values of the cut points after being scaled.
//
// Example. mA = 0.1,   - source range
//           A = 0.3    - target range
//          mB = 0.6,
//           B = 0.6.
// The above values will expand the perceived range of the first segment, at the expense of
// compressing the perceived range of the second segment.

function ParScaleCustom(Value, mA, A, mB, B:single):single; inline;
function ParScaleCustomInv(Value, mA, A, mB, B:single):single; inline;






// TODO: There are a few interpolation routines. But they don't appear to have any naming consistency or much else.
// I think this should be improved.
// NOTE: I'm not sure all of these functions even work. Unit testing should be mandatory here.
function LinearInterpolation(a,b,f:double):double; inline;
function Hermite4Interpolation(frac_pos, xm1, x0, x1, x2:double):double; inline;
function HermiteInterpolation(const f, x0, x1, x2, x3:double):double; inline;
function Hermite32_asm(const frac_pos: Single; pntr : PSingle) : Single;
function SplineInterpolation6Point5thOrder(const f:single; x0,x1,x2,x3,x4,x5:single):single; inline;
function WatteTriLinearInterpolation(f:single; y0,y1,y2,y3:single):single; inline;
function _Optimal2Point3rdOrder(f, y0, y1:double):double; inline; //WARNING: I'm not sure this function works correctly.





function Cube(Value:double):double; inline;


// Calculate the coefficient for RC filter-type envelopes.
// See this thread for details.
// http://www.kvraudio.com/forum/viewtopic.php?t=300689
// -Subject: RC filter / RC decay coefficients and duration
// -- One pole filter: y = x + c * (y-x);
function CalcRcEnvelopeCoefficient(MilliSeconds:single; SampleRate:single):double;
function RcEnvFilter(const CurrentValue, TargetValue, Coefficient: double): double; inline;
function RcDecayFilter(const CurrentValue, TargetValue, Coefficient: double): double; inline;

function IsNear(ValueA, ValueB, Threshold : single):boolean; inline;

function SlewRateLimit(const Current, Target, SlewLimit: single):single; inline;

function MakeBoolean(aValue:single):boolean; inline;

function Sinc(x:double):double; overload; inline;
function Sinc(const x, cutoff : double):double; overload; inline;

type
  TSingleFourElementBuffer = record
    ax1:single;
    ax2:single;
    ax3:single;
    ax4:single;
    procedure Init(Value:single = 0);
    procedure Add(Value:Single);
  end;

  TSingleFourElementStereoBuffer = record
    ax1:single;
    ax2:single;
    ax3:single;
    ax4:single;
    bx1:single;
    bx2:single;
    bx3:single;
    bx4:single;
    procedure Init(Value:single = 0);
    procedure Add(ax, bx:Single);
  end;

  TSingleSixElementBuffer = record
    ax1:single;
    ax2:single;
    ax3:single;
    ax4:single;
    ax5:single;
    ax6:single;
    procedure Init(Value:single = 0);
    procedure Add(Value:Single);
  end;

  TSingleSixElementStereoBuffer = record
    ax1:single;
    ax2:single;
    ax3:single;
    ax4:single;
    ax5:single;
    ax6:single;
    bx1:single;
    bx2:single;
    bx3:single;
    bx4:single;
    bx5:single;
    bx6:single;
    procedure Init(Value:single = 0);
    procedure Add(ax, bx:Single);
  end;


//==== Deprecated functions =======

procedure Swap_Single(var a,b:single); inline; deprecated;   //replace with functions in eeFunctions.pas


//=== These three methods have been taken from Tobybear's PitchConverter unit =====
function CpsToMidi(x:double):integer; inline; deprecated;   //replace with functions in eePitch.pas
function MidiToCps(x: double): double; inline; deprecated;  //replace with functions in eePitch.pas
function MidiToName(x:integer):string; deprecated;          //replace with functions in eePitch.pas
//=================================================================================


procedure Calculate3dbPan(const PanPos : single; out GainCh1, GainCh2 : single); inline;



function Pow2(const x:single):single; inline;
function Pow3(const x:single):single; inline;
function Pow4(const x:single):single; inline;
function Pow5(const x:single):single; inline;

//=============  FFT Related Stuff ================================================

function CalcFFTBinMagnitude(const RealPart, ImagPart : single):single; inline;
function CalcFFTBinPhase(const RealPart, ImagPart : single):single; inline;



//=================================================================================


implementation

uses
  SysUtils;


procedure Swap_Single(var a,b:single);
var
  Temp:single;
begin
  Temp := a;
  a := b;
  b := Temp;
end;

function SameValue_Single(const A, B: Single; Epsilon: Single): Boolean;
const
  FuzzFactor = 1000;
  SingleResolution   = 1E-7 * FuzzFactor;
begin
  if Epsilon = 0 then
    Epsilon := Max(Min(Abs(A), Abs(B)) * SingleResolution, SingleResolution);
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
end;


function CpsToMidi(x:double):integer;
begin
  if x<0 then x:=0;
 result := abs(round(12*log2(x/GlobalTuneA4)+69));
 if result>127 then result:=-1;
end;

function MidiToCps(x: double): double;
begin
 if (x>=0)and(x<=127) then
  result := GlobalTuneA4*power(2,(x-69)/12)
 else
  result:=-1;
end;

function MidiToName(x:integer):string;
const notes:array[0..11] of string=
 ('C ','C#','D ','D#','E ','F ','F#','G ','G#','A ','A#','B ');
begin
  if (x>=0)and(x<=127)
    then result:=notes[x mod 12] + intTostr(x div 12-2)
    else result:='---';
end;




function MillisecondsToSamples(ms, SampleRate: single): single; inline;
begin
 //result := ms * SampleRate / 1000;
 result := ms * SampleRate * 0.001;
end;

function SamplesToMilliseconds(Samples, SampleRate: single): single; inline;
begin
 result := Samples * 1000 / SampleRate;
end;

function SyncToSamples(SyncFactor, BPM, SampleRate: double): double; inline;
begin
  result := (SyncFactor * 4) * SampleRate * 60 / BPM;
end;

function SamplesToSync(Samples, BPM, SampleRate: double): double; inline;
begin
  result := Samples * BPM / (SampleRate * 60) * 0.25;
end;

function BeatsToSamples(SyncFactor, BPM, SampleRate: double): double; inline;
begin
 result := (SyncFactor) * SampleRate * 60 / BPM;
end;

function SamplesToBeats(Samples, BPM, SampleRate: double): double; inline;
begin
 result := Samples * BPM / (SampleRate * 60);
end;

function DecibelsToVoltage(dB:single):single; inline;
const
  OneOver20 = 0.05; // = 1/20
begin
  //result := Power(10, db / 20);
  //result := Power(10, db * OneOver20);
  if dB > -120
    then result := Power(10, db * OneOver20)
    else result := 0;
end;

function VoltageToDecibels(Voltage:single):single; inline;
begin
  result := 20 * Log10(Voltage);
end;

function DecibelsToLinear(dB:single):single; inline;
const
  OneOver20 = 0.05; // = 1/20
begin
  //result := Power(10, db / 20);
  if dB > -120
    then result := Power(10, db * OneOver20)
    else result := 0;
end;


function LinearToDecibels(Linear:single):single; inline;
begin
  result := 20 * Log10(Linear);
end;

function ExponentialShift(const StartingValue, OctaveShift:single):single; inline;
// NOTE: ExponentialShift() is usful for exponential parameter scaling.
// - when OctaveShift = 0, the result will equal StartingValue
// - when OctaveShift = 1, the result will equal StartingValue * 2
// - when OctaveShift = -1, the result will equal StartingValue / 2
begin
  result := StartingValue * power(2,OctaveShift);
end;


procedure CalcSyncTimeA(Value:single; out SyncNumerator, SyncDenominator:integer);
var
  x:integer;
begin
  x := round(Value * 20);

  case x of
  0 : begin
        SyncNumerator   := 1;
        SyncDenominator := 128;
      end;
  1 : begin
        SyncNumerator   := 1;
        SyncDenominator := 64;
      end;
  2 : begin
        SyncNumerator   := 1;
        SyncDenominator := 32;
      end;
  3 : begin
        SyncNumerator   := 1;
        SyncDenominator := 24;
      end;
  4 : begin
        SyncNumerator   := 1;
        SyncDenominator := 16;
      end;
  5 : begin
        SyncNumerator   := 1;
        SyncDenominator := 12;
      end;
  6 : begin
        SyncNumerator   := 1;
        SyncDenominator := 8;
      end;
  7 : begin
        SyncNumerator   := 1;
        SyncDenominator := 6;
      end;
  8 : begin
        SyncNumerator   := 1;
        SyncDenominator := 4;
      end;
  9 : begin
        SyncNumerator   := 1;
        SyncDenominator := 3;
      end;
  10: begin
        SyncNumerator   := 1;
        SyncDenominator := 2;
      end;
  11: begin
        SyncNumerator   := 2;
        SyncDenominator := 3;
      end;
  12: begin
        SyncNumerator   := 3;
        SyncDenominator := 4;
      end;
  13: begin
        SyncNumerator   := 7;
        SyncDenominator := 8;
      end;
  14: begin
        SyncNumerator   := 1;
        SyncDenominator := 1;
      end;
  15: begin
        SyncNumerator   := 2;
        SyncDenominator := 1;
      end;
  16: begin
        SyncNumerator   := 3;
        SyncDenominator := 1;
      end;
  17: begin
        SyncNumerator   := 4;
        SyncDenominator := 1;
      end;
  18: begin
        SyncNumerator   := 8;
        SyncDenominator := 1;
      end;
  19: begin
        SyncNumerator   := 16;
        SyncDenominator := 1;
      end;
  20: begin
        SyncNumerator   := 32;
        SyncDenominator := 1;
      end;
  end;

end;

function CalcSyncTimeA_Inv(SyncNumerator, SyncDenominator:integer):single;
var
  x:integer;
begin
  x := 0;
  if (SyncNumerator = 1)  and (SyncDenominator = 128) then x := 0;
  if (SyncNumerator = 1)  and (SyncDenominator = 64)  then x := 1;
  if (SyncNumerator = 1)  and (SyncDenominator = 32)  then x := 2;
  if (SyncNumerator = 1)  and (SyncDenominator = 24)  then x := 3;
  if (SyncNumerator = 1)  and (SyncDenominator = 16)  then x := 4;
  if (SyncNumerator = 1)  and (SyncDenominator = 12)  then x := 5;
  if (SyncNumerator = 1)  and (SyncDenominator = 8)   then x := 6;
  if (SyncNumerator = 1)  and (SyncDenominator = 6)   then x := 7;
  if (SyncNumerator = 1)  and (SyncDenominator = 4)   then x := 8;
  if (SyncNumerator = 1)  and (SyncDenominator = 3)   then x := 9;
  if (SyncNumerator = 1)  and (SyncDenominator = 2)   then x := 10;
  if (SyncNumerator = 2)  and (SyncDenominator = 3)   then x := 11;
  if (SyncNumerator = 3)  and (SyncDenominator = 4)   then x := 12;
  if (SyncNumerator = 7)  and (SyncDenominator = 8)   then x := 13;
  if (SyncNumerator = 1)  and (SyncDenominator = 1)   then x := 14;
  if (SyncNumerator = 2)  and (SyncDenominator = 1)   then x := 15;
  if (SyncNumerator = 3)  and (SyncDenominator = 1)   then x := 16;
  if (SyncNumerator = 4)  and (SyncDenominator = 1)   then x := 17;
  if (SyncNumerator = 8)  and (SyncDenominator = 1)   then x := 18;
  if (SyncNumerator = 16) and (SyncDenominator = 1)   then x := 19;
  if (SyncNumerator = 32) and (SyncDenominator = 1)   then x := 20;

  result := x / 20;
end;

function PitchShiftToRate(ShiftAmount:double):double;
begin
  result := power(2, ShiftAmount/12);
end;

function ParScaleSquare(Value:single):single;
begin
  result := Value * Value;
end;

function ParScaleSquareInv(Value:single):single;
begin
  if Value <> 0
    then result := Power(Value, 1/2)
    else result := 0;
end;

function ParScaleCube(Value:single):single;
begin
  result := Value * Value * Value;
end;

function ParScaleCubeInv(Value:single):single;
begin
  if Value <> 0
    then result := Power(Value, 1/3)
    else result := 0;
end;


function ParScaleCustom(Value, mA, A, mB, B:single):single;
var
  x, Dist, mDist:single;
  ScaleAmt, mScaleAmt:single;
begin
  assert(A < B);
  assert(mA < mB);
  assert(Value >= 0);
  assert(Value <= 1);

  if Value <= A then
  begin
    Dist  := A;
    mDist := mA;
    ScaleAmt  := 0;
    mScaleAmt := 0;
  end else
  if Value <= B then
  begin
    Dist  := B - A;
    mDist := mB - mA;
    ScaleAmt  := A;
    mScaleAmt := mA;
  end else
  begin
    Dist  := 1 - B;
    mDist := 1 - mB;
    ScaleAmt  := B;
    mScaleAmt := mB;
  end;

  x := (Value - ScaleAmt) / Dist;
  result := x * mDist + mScaleAmt;

end;

function ParScaleCustomInv(Value, mA, A, mB, B:single):single;
var
  x, Dist, mDist:single;
  ScaleAmt, mScaleAmt:single;
begin
  assert(A < B);
  assert(mA < mB);
  assert(Value >= 0);
  assert(Value <= 1);

  if Value <= mA then
  begin
    Dist  := A;
    mDist := mA;
    ScaleAmt  := 0;
    mScaleAmt := 0;
  end else
  if Value <= mB then
  begin
    Dist  := B - A;
    mDist := mB - mA;
    ScaleAmt  := A;
    mScaleAmt := mA;
  end else
  begin
    Dist  := 1 - B;
    mDist := 1 - mB;
    ScaleAmt  := B;
    mScaleAmt := mB;
  end;

  x := (Value - mScaleAmt) / mDist;
  result := x * Dist + ScaleAmt;
end;



// function Sinc(x:double):double;
// http://en.wikipedia.org/wiki/Sinc_function
// Docs: http://www.onesmallclue.com/wiki_private/index.php?title=Sinc&action=submit
function Sinc(x:double):double;
begin
  if x = 0
    then result := 1
    else result := sin(Pi * x) / (pi * x);
end;

function Sinc(const x, cutoff : double):double;
begin
  // Could this be called a Bandlimited sinc function?
  // Cutoff is normalised to 0..1.
  // NOTE: This function produces a kind of bandlimited sinc function.
  assert((Cutoff >= 0) and (Cutoff <= 1));

  if x <> 0
    then result := Sin(pi * x * Cutoff)/(pi * x)
    else result := Cutoff;
end;





function LinearInterpolation(a,b,f:double):double; inline;
begin
  result := (a * (1-f)) + (b * f);
end;


function Hermite4Interpolation(frac_pos, xm1, x0, x1, x2:double):double;
var
  c,v,w,a,b_neg:double;
begin
  c     := (x1 - xm1) * 0.5;
  v     := x0 - x1;
  w     := c + v;
  a     := w + v + (x2 - x0) * 0.5;
  b_neg := w + a;

  result := ((((a * frac_pos) - b_neg) * frac_pos + c) * frac_pos + x0);
end;

function HermiteInterpolation(const f, x0, x1, x2, x3:double):double; inline;
var
  c0, c1, c2, c3:single;
begin
  c0 := x1;
  c1 := (1/2)*(x2-x0);
  c2 := x0 - (5/2) * x1 + 2*x2 - (1/2) * x3;
  c3 := (1/2) *(x3-x0)+(3/2)*(x1-x2);
  result := ((c3*f+c2)*f+c1)*f+c0;
end;

//This method was taken from DDspInterpolation.pas
function Hermite32_asm(const frac_pos: Single; pntr : PSingle) : Single;
// Parameter explanation:
// frac_pos: fractional value [0.0f - 1.0f] to interpolator
// pntr: pointer to float array where:
// pntr[0] = previous sample (idx = -1)
// pntr[1] = current sample (idx = 0)
// pntr[2] = next sample (idx = +1)
// pntr[3] = after next sample (idx = +2)
// The interpolation takes place between pntr[1] and pntr[2].
const c_half : Double = 0.5;
asm
 fld  [pntr+8].Single;         // x1
 fsub [pntr].Single;           // x1-xm1
 fld  [pntr+4].Single;         // x0           x1-xm1
 fsub [pntr+8].Single;         // v            x1-xm1
 fld  [pntr+12].Single;        // x2           v            x1-xm1
 fsub [pntr+4].Single;         // x2-x0        v            x1-xm1
 fxch st(2);                   // x1-m1        v            x2-x0
 fmul c_half;                  // c            v            x2-x0
 fxch st(2);                   // x2-x0        v            c
 fmul c_half;                  // 0.5*(x2-x0)  v            c
 fxch st(2);                   // c            v            0.5*(x2-x0)
 fst st(3);                    // c            v            0.5*(x2-x0)  c
 fadd st(0), st(1);            // w            v            0.5*(x2-x0)  c
 fxch st(2);                   // 0.5*(x2-x0)  v            w            c
 faddp st(1), st(0);           // v+.5(x2-x0)  w            c
 fadd st(0), st(1);            // a            w            c
 fadd st(1), st(0);            // a            b_neg        c
 fmul frac_pos.Single;         // a*frac       b_neg        c
 fsubrp st(1), st(0);          // a*f-b        c
 fmul frac_pos.Single;         // (a*f-b)*f    c
 faddp st(1), st(0);           // res-x0/f
 fmul frac_pos.Single;         // res-x0
 fadd [pntr+4].Single          // res
end;





// function SplineInterpolation6Point5thOrder()
// This method was copied from DAV_DspInterpolation.pas by Christian-W. Budde
// License is MPL 1.1 or LGPL 2.1 with linking exception.
// !!!!!There is something wrong with this routine!!!!!
function SplineInterpolation6Point5thOrder(const f:single; x0,x1,x2,x3,x4,x5:single):single;
//const
//  CHalf32     : Single = 0.5;
//  COneSixth32 : Single = 1/6;
var
  c0,c1,c2,c3,c4,c5,c6,c7:single;
begin
 // 6-point, 5th-order B-spline (x-form)
 c0 := x0 + x4;

 //c1 := COneSixth32 * (x1 + x3);
 c1 := (1/6) * (x1 + x3);

 c2 := x4 - x0;
 c3 := x3 - x1;
 c4 := 1/120 * c0 + 13/10 * c1 + 11/20 * x2;
 c5 := 1/24 * c2 + 5/12 * c3;
 c6 := 1/12 * c0 + c1 - 1/2 * x2;

 //c7 := COneSixth32 * (CHalf32 * c2 - c3);
 c7 := (1/6) * (0.5 * c2 - c3);

 result := (((((1/120 * (x5 - x0) + 1/24 * (x1 - x3) + 1 / 12.0 * (x3 - x2)) * f + (1/24 * c0 - c1 + 1/4 * x2)) * f + c7) * f + c6) * f + c5) * f + c4;
end;


//Has been tested, looks like a straight line.
function WatteTriLinearInterpolation(f:single; y0,y1,y2,y3:single):single;
var
  ym1py2:single;
  c0,c1,c2:single;
begin
  ym1py2 := y0 + y3;
  c0     := y1;
  c1     := 3/2*y2 - 1/2* (y1+ym1py2);
  c2     := 1/2*(ym1py2 - y1 - y2);
  result := (c2 * f + c1) * f + c0;
end;

function _Optimal2Point3rdOrder(f, y0,y1:double):double; inline;
const
  ScaleFactor = 1.6295353174;
var
  z:single;
  c0,c1,c2,c3:single;
  even1, odd1:single;
begin
  z := f - 0.5;
  even1 := y1 + y0;
  odd1  := y1 - y0;
  c0    := even1 * 0.50037842517188658;
  c1    := odd1  * 1.00621089801788210;
  c2    := even1 * -0.004541102062639801;
  c3    := odd1  * -1.57015627178718420;
  result := (((c3*z+c2)*z+c1)*z+c0) * ScaleFactor;
end;






{ TSingleFourElementBuffer }

procedure TSingleFourElementBuffer.Init(Value: single);
begin
  ax1 := Value;
  ax2 := Value;
  ax3 := Value;
  ax4 := Value;
end;

procedure TSingleFourElementBuffer.Add(Value: Single);
begin
  ax4 := ax3;
  ax3 := ax2;
  ax2 := ax1;
  ax1 := Value;
end;



{ TSingleFourElementStereoBuffer }

procedure TSingleFourElementStereoBuffer.Add(ax, bx: Single);
begin
  ax4 := ax3;
  ax3 := ax2;
  ax2 := ax1;
  ax1 := ax;

  bx4 := bx3;
  bx3 := bx2;
  bx2 := bx1;
  bx1 := bx;
end;

procedure TSingleFourElementStereoBuffer.Init(Value: single);
begin
  ax1 := Value;
  ax2 := Value;
  ax3 := Value;
  ax4 := Value;
  bx1 := Value;
  bx2 := Value;
  bx3 := Value;
  bx4 := Value;
end;


{ TSingleSixElementBuffer }

procedure TSingleSixElementBuffer.Add(Value: Single);
begin
  ax6 := ax5;
  ax5 := ax4;
  ax4 := ax3;
  ax3 := ax2;
  ax2 := ax1;
  ax1 := Value;

end;

procedure TSingleSixElementBuffer.Init(Value: single);
begin
  ax1 := Value;
  ax2 := Value;
  ax3 := Value;
  ax4 := Value;
  ax5 := Value;
  ax6 := Value;
end;


{ TSingleSixElementStereoBuffer }

procedure TSingleSixElementStereoBuffer.Add(ax, bx: Single);
begin
  ax6 := ax5;
  ax5 := ax4;
  ax4 := ax3;
  ax3 := ax2;
  ax2 := ax1;
  ax1 := ax;

  bx6 := bx5;
  bx5 := bx4;
  bx4 := bx3;
  bx3 := bx2;
  bx2 := bx1;
  bx1 := bx;

end;

procedure TSingleSixElementStereoBuffer.Init(Value: single);
begin
  ax1 := Value;
  ax2 := Value;
  ax3 := Value;
  ax4 := Value;
  ax5 := Value;
  ax6 := Value;
  bx1 := Value;
  bx2 := Value;
  bx3 := Value;
  bx4 := Value;
  bx5 := Value;
  bx6 := Value;

end;



procedure PolarToCartesian(const Angle, Distance:single; out x, y:single);
const
  OneOver360 = 1/360;
begin
  x := cos(DegToRad(Angle)) * Distance;
  y := sin(DegToRad(Angle)) * Distance;
end;

function CalcRandomisedValue(const InitialValue, RandomiseAmt:single):single; inline;
begin
  assert(InitialValue >= 0);
  assert(InitialValue <= 1);
  assert(RandomiseAmt >= 0);
  assert(RandomiseAmt <= 1);         

  result := InitialValue + (Random - InitialValue) * RandomiseAmt;
end;



function VstFloatToInt(FloatValue:single; const MinIntValue, MaxIntValue:integer):integer;
var
  tx:integer;
begin
  FloatValue := FloatValue * (MaxIntValue - MinIntValue + 1) + MinIntValue;
  tx := Floor(FloatValue);
  if tx > MaxIntValue then tx := MaxIntValue;
  result := tx;
end;

function VstFloatToInt(const FloatValue : single; const NumberOfValues : integer):integer; inline; overload;
var
  tx:integer;
begin
  tx := Floor(FloatValue * NumberOfValues);
  if tx >= NumberOfValues then tx := NumberOfValues-1;
  result := tx;
end;

function Cube(Value:double):double; 
begin
  result := Value * Value * Value;
end;



function CalcRcEnvelopeCoefficient(MilliSeconds:single; SampleRate:single):double;
var
  Tau: double; //decay time in seconds.
begin
  Tau := Milliseconds * 0.001;

  if Tau <= 0
    then result := 0
    else result := exp( -1.0 / (Tau * SampleRate));

end;

function RcEnvFilter(const CurrentValue, TargetValue, Coefficient: double): double; inline;
// NOTE: One pole filter: y = x + c * (y-x);
// TargetValue = x
// CurrentValue = y
begin
  result := TargetValue + Coefficient * (CurrentValue - TargetValue + kDenormal);
end;

function RcDecayFilter(const CurrentValue, TargetValue, Coefficient: double): double; inline;
begin
  if CurrentValue < TargetValue
    then result := TargetValue
    else result := TargetValue + Coefficient * (CurrentValue - TargetValue + kDenormal);
end;

function SlewRateLimit(const Current, Target, SlewLimit: single):single; inline;
var
  Diff : single;
begin
  Diff := Target - Current;

  Diff := min(diff, SlewLimit);
  Diff := max(diff, -SlewLimit);

  result := Current + Diff;
end;


function MakeBoolean(aValue:single):boolean;
begin
  Result := (aValue >= 0.5);
end;


function IsNear(ValueA, ValueB, Threshold : single):boolean; inline;
var
  x : single;
begin
  x := ValueA - ValueB;

  if (x <= Threshold) and (x >= -Threshold)
    then result := true
    else result := false;

end;


procedure Calculate3dbPan(const PanPos : single; out GainCh1, GainCh2 : single); inline;
const
  b = 1.831783;
  c = -0.831783;
var
  InvPanPos : single;
begin
  assert(PanPos >= 0);
  assert(PanPos <= 1);

  InvPanPos := 1 - PanPos;

  GainCh1 := (b * InvPanPos) + (c * InvPanPos * InvPanPos);
  GainCh2 := (b *    PanPos) + (c *    PanPos *    PanPos);
end;


function Pow2(const x:single):single; inline;
begin
  result := x * x;
end;

function Pow3(const x:single):single; inline;
begin
  result := x * x * x;
end;

function Pow4(const x:single):single; inline;
begin
  result := x * x * x * x;
end;

function Pow5(const x:single):single; inline;
begin
  result := x * x * x * x * x;
end;



function CalcFFTBinMagnitude(const RealPart, ImagPart : single):single;
// Source: "Understanding Digital Singal Processing" Second Edition, Richard Lyons. Page 49.
begin
  result := Sqrt((RealPart * RealPart) + (ImagPart * ImagPart));
end;

function CalcFFTBinPhase(const RealPart, ImagPart : single):single;
// Source: "Understanding Digital Singal Processing" Second Edition, Richard Lyons. Page 49.
begin
  result := ArcTan(ImagPart / RealPart);
end;


end.
