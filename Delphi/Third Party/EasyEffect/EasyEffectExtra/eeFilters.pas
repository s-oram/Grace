unit eeFilters;

interface



type

  // TMoogLP is based on code from MusicDSP
  // http://www.musicdsp.org/showone.php?id=253
  //
  // Which in turn is based on:
  // "Analyzing the Moog VCF with Considerations for Digital Implementation"
  // by Tim Stilson and Julius Smith
  // https://ccrma.stanford.edu/~stilti/papers/moogvcf.pdf

  TMoogLP = class
  private
    fSampleRate: integer;
    fFreq, fRes:single;
  protected
    t, t2, x, f, k, p, r, y1, y2, y3, y4, oldx, oldy1, oldy2, oldy3: double;
    procedure CalcCoeffs;

    procedure SetFreq(aFreq:single);
    procedure SetRes(aRes:single);
  public
    constructor Create;

    function Process(In1:single):single;

    property SampleRate:integer read fSampleRate write fSampleRate;
    property Freq : single read fFreq write SetFreq;
    property Res  : single read fRes  write SetRes;
  end;


  //  By Fuzzpilz
  //  http://www.musicdsp.org/showArchiveComment.php?ArchiveID=181
  //  "Vaguely based on the Stilson/Smith Moog paper, but going in a rather different direction from others I've seen here."
  TMoogLpB = class
  private
    fSampleRate: integer;
    fFreq, fRes:single;
  protected
    fCoeffs     : array[0..8] of Double;
    d           : array[0..3] of Double;
    fGain     : Double;
    procedure CalcCoeffs;
    procedure SetFreq(aFreq:single);
    procedure SetRes(aRes:single);
  public
    constructor Create;

    function Process(In1:single):single;

    property SampleRate:integer read fSampleRate write fSampleRate;
    property Freq : single read fFreq write SetFreq;
    property Res  : single read fRes  write SetRes;
  end;


  //This is a basic one pole low pass filter. 
  TOnePoleLowPassFilter = class
  private
    fa0: double;
    fb1: double;
    NegA1:double;
  protected
    Old1:double;
  public
    constructor Create;

    function Process(x:single):single; inline;

    property a0:double read fa0 write fa0;
    property b1:double read fb1 write fb1;
  end;

implementation

uses
  Math;

{ TMoogVcf }

constructor TMoogLP.Create;
begin
  SampleRate := 44100;

  y1    := 0;
  y2    := 0;
  y3    := 0;
  y4    := 0;
  oldx  := 0;
  oldy1 := 0;
  oldy2 := 0;
  oldy3 := 0;
end;

procedure TMoogLP.SetFreq(aFreq: single);
begin
  fFreq := aFreq;
  CalcCoeffs;
end;

procedure TMoogLP.SetRes(aRes: single);
begin
  fRes := aRes;
  CalcCoeffs;
end;

procedure TMoogLP.CalcCoeffs;
begin
  f  := (fFreq+fFreq) / SampleRate;
  p  := f * (1.8 - (0.8 * f));
  k  := p + p - 1;
  t  := (1 - p ) * 1.386249;
  t2 := 12 + (t*t);
  r  := fRes * (t2 + 6 * t) / (t2 - 6 * t);
end;

function TMoogLP.Process(In1: single): single;
var
  Temp:double;
begin
  x     := In1 - (r * y4);
  y1    := (x  * p) + (oldx  * p) - (k * y1);
  y2    := (y1 * p) + (oldy1 * p) - (k * y2);
  y3    := (y2 * p) + (oldy2 * p) - (k * y3);
  y4    := (y3 * p) + (oldy3 * p) - (k * y4);
  y4    := y4 - ((y4 * y4 * y4) / 6);
  oldx  := x;
  oldy1 := y1;
  oldy2 := y2;
  oldy3 := y3;

  result := y4;
end;





{ TJosepLP }

constructor TMoogLpB.Create;
begin
  SampleRate := 44100;
end;

procedure TMoogLpB.SetFreq(aFreq: single);
begin
  fFreq := aFreq;
  CalcCoeffs;
end;

procedure TMoogLpB.SetRes(aRes: single);
begin
  fRes := aRes;
  CalcCoeffs;
end;

procedure TMoogLpB.CalcCoeffs;
var k,p,q,b,s : Double;
    a         : array[0..4] of Double;
begin
  fGain:=1;
  s:=1;

  // calculating coefficients:
  k:=(4.0*fGain-3.0)/(fGain+1.0);
  p:=1.0-0.25*k;
  p:=p*p;
  b:=1.0/(tan(pi*fFreq/SampleRate)*(1.0+p));

  p:=1.0+b;
  q:=s*(1.0-b);

  a[0] := 1.0/(  k+p*p*p*p);
  a[1] := 4.0*(s*k+p*p*p*q);
  a[2] := 6.0*(  k+p*p*q*q);
  a[3] := 4.0*(s*k+p*q*q*q);
  a[4] :=     (  k+q*q*q*q);
  p    := a[0]*(k+1.0);

  fCoeffs[0]:=p;
  fCoeffs[1]:=4.0*p*s;
  fCoeffs[2]:=6.0*p;
  fCoeffs[3]:=4.0*p*s;
  fCoeffs[4]:=p;
  fCoeffs[5]:=-a[1]*a[0];
  fCoeffs[6]:=-a[2]*a[0];
  fCoeffs[7]:=-a[3]*a[0];
  fCoeffs[8]:=-a[4]*a[0];
end;

function TMoogLpB.Process(In1: single): single;
begin
  Result :=fCoeffs[0]*In1+d[0];
  d[0]   :=fCoeffs[1]*In1+fCoeffs[5]*Result+d[1];
  d[1]   :=fCoeffs[2]*In1+fCoeffs[6]*Result+d[2];
  d[2]   :=fCoeffs[3]*In1+fCoeffs[7]*Result+d[3];
  d[3]   :=fCoeffs[4]*In1+fCoeffs[8]*Result;
end;




{ TOnePoleLowPassFilter }

constructor TOnePoleLowPassFilter.Create;
begin
  fa0 := 0;
  fb1 := 0;
end;

function TOnePoleLowPassFilter.Process(x: single): single;
var
  Temp:double;
begin
  Temp := fa0 * x - fb1 * Old1;
  Old1   := Temp;
  result := Temp;
end;


function Limit(x:double):double;
begin
  if x > 1  then x := 1;
  if x < -1 then x := -1;

  result := x;


end;


end.
