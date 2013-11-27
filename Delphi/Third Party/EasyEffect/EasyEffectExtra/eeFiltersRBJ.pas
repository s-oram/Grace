unit eeFiltersRBJ;

interface

type
  TRbjFilter = class
  private
    fFreq:double;
    fQ:double;
    fGain:double;
    fBandWidth: double;
  protected
    f0:double; //The significant frequency
    Fs: integer;
    a0,a1,a2:double;
    b0,b1,b2:double;
    b0a0:double;
    b1a0:double;
    b2a0:double;
    a1a0:double;
    a2a0:double;
    x1L,x2L,x1R,x2R:double;
    y0L,y1L,y2L,y0R,y1R,y2R:double;

    property Freq       :double read fFreq write fFreq; //hertz.
    property BandWidth  :double read fBandWidth write fBandWidth; //0..1
    property Q          :double read fQ write fQ;
    property Gain       :double read fGain write fGain;
  public
    constructor Create;

    procedure Process(var In1, In2:single); inline;

    property SampleRate:integer read Fs write Fs;
  end;


  TRbjLowPass = class(TRbjFilter)
  public
    procedure CalcCoefficients; inline;
    property Freq;
    property Q;
  end;

  TRbjHighPass = class(TRbjFilter)
  public
    procedure CalcCoefficients; inline;
    property Freq;
    property Q;
  end;

  TRbjBandPass = class(TRbjFilter)
  public
    procedure CalcCoefficients; inline;
    property Freq;
    property Q;
    property Gain;
  end;

  TZoelzerPeaking = class(TRbjFilter)
  public
    procedure CalcCoefficients;
    property Freq;
    property Q;
    property Gain;
  end;


implementation

uses
  eeDsp, Math;

{ TRbjFilter }

constructor TRbjFilter.Create;
begin
  Fs   := 44100;
  Freq := 5000;
  Q    := 0.2;
  Gain := 0;
  x1L  := 0;
  x2L  := 0;
  x1R  := 0;
  x2R  := 0;
  y0L  := 0;
  y1L  := 0;
  y2L  := 0;
  y0R  := 0;
  y1R  := 0;
  y2R  := 0;
end;

procedure TRbjFilter.Process(var In1, In2: single);
begin
  //Input1
  y2L := y1L;
  y1L := y0L;
  y0L := (b0a0)*In1 + (b1a0)*x1L + (b2a0)*x2L - (a1a0)*y1L - (a2a0)*y2L;
  x2L := x1L;
  x1L := In1;
  In1 := y0L;

  //Input2
  y2R := y1R;
  y1R := y0R;
  y0R := (b0a0)*In2 + (b1a0)*x1R + (b2a0)*x2R - (a1a0)*y1R - (a2a0)*y2R;
  x2R := x1R;
  x1R := In2;
  In2 := y0R;
  
end;


{ TRbjLowPass }

procedure TRbjLowPass.CalcCoefficients;
var
  w0:double;
  alpha:double;
begin
  // These coefficient calculations are taken from the Audio-EQ cookbook.
  // The equations have been rearranged to fit the filtering scheme but
  // still produce the same results.

  //intermediate variable calculation
  w0    := 2 * pi * Freq / fs;
  alpha := sin(w0)/(2*Q);


  //Coefficient calculation
  b0 :=  (1 - cos(w0))/2;
  b1 :=  (1 - cos(w0));
  b2 :=  (1 - cos(w0))/2;
  a0 :=   1 + alpha;
  a1 :=  (-2 * cos(w0));
  a2 :=  (1 - alpha);

  b0a0 := b0 / a0;
  b1a0 := b1 / a0;
  b2a0 := b2 / a0;
  a1a0 := a1 / a0;
  a2a0 := a2 / a0;
end;





{ TRbjHighPass }

procedure TRbjHighPass.CalcCoefficients;
var
  w0:double;
  alpha:double;

begin
  // These highpass filter coefficients were taken from tobybear's implementation of
  // the RBJ highpass EQ cookbook formula.

  // NOTE: With low res settings, it's not a very effective highpass filter. Almost more like
  // a volume control. Turning up the Q (0.6+) makes it behave more like a proper highpass filter.
  // See Tobybear's FilterExplorer for an illistration of the filter response curve.

  {
  ScaledQ := Q * (1-0.001) + 0.001;

   // calculate temporary variables
  t0 := cos(2 * pi * f0 / fs);
  t1 := sin(2 * pi * f0 / fs) / (2.3 * ScaledQ);
  t2 := 1+t1;

  // calculate coefficients
  a0 := (1+t0)/(2*t2);
  a1 := -(1+t0)/t2;
  a2 := (1+t0)/(2*t2);
  b1 := -2*t0/t2;
  b2 := ((1-t1)/t2);
  }


  w0    := 2 * pi * Freq / fs;
  alpha := sin(w0)/(2*Q);

  b0 :=  (1 + cos(w0))/2;
  b1 := -(1 + cos(w0));
  b2 :=  (1 + cos(w0))/2;
  a0 :=   1 + alpha;
  a1 :=  -2*cos(w0);
  a2 :=   1 - alpha;


  b0a0 := b0 / a0;
  b1a0 := b1 / a0;
  b2a0 := b2 / a0;
  a1a0 := a1 / a0;
  a2a0 := a2 / a0;
end;




{ TZoelzerPeaking }

procedure TZoelzerPeaking.CalcCoefficients;
var
  t0,t1,t2,t3,t4,t5:double;
  c0, c1, c2, c3:double;
begin
  // NOTE: This class hasn't been debugger.....


  // c0: cutoff frequency (0..1), 1 is Nyquist
  // c1: bandwidth/steepness (0..0.25)
  // c2: peak gain (0..0.5: boost, 0.5..1:cut)
  // c3: overall gain

  c0 := 2 * Freq / Fs;
  c1 := Q * 0.25;
  c2 := Gain / 36 + 0.5;
  if c2 > 1 then c2 := 1;
  if c2 < 0 then c2 := 0;
  c2 := 1-c2;
  c3 := 1;

  // calculate temporary variables
  t0 := 0.96*c0+0.01;
  t1 := tan(pi*t0/2);
  t2 := 10*c1+0.1;
  t3 := 1-c2;
  t4 := c2+0.01;
  t5 := 1+(t4*t1/t2)+t1*t1;

  // calculate coefficients
  b0 := c3*(1+(t3*t1/t2)+t1*t1)/t5;
  b1 := c3*(2*(t1*t1-1))/t5;
  b2 := c3*(1-(t3*t1/t2)+t1*t1)/t5;
  a1 := (2*(t1*t1-1))/t5;
  a2 := (1-(t4*t1/t2)+t1*t1)/t5;

  b0a0 := b0;
  b1a0 := b1;
  b2a0 := b2;
  a1a0 := a1;
  a2a0 := a2;
end;



{ TRbjBandPass }

procedure TRbjBandPass.CalcCoefficients;
begin
  //TODO
  assert(false, 'todo');
end;



end.
