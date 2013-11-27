{
These filters are based on information provide by Andrew Simper on the Music-DSP
mailing list. 


================================================================================
  Andrew Simper's Email
================================================================================

  
I'll do a fancy copy of the block diagram in the paper I write, but
here is the linear circuit diagram I worked from, which was written
down directly from looking at the sem 1a schematic. The large
triangles are OTAs and so output a current, and the smaller triangles
are buffers and so output a buffered voltage of the input voltage,
sorry it is a little vague but it was only meant for me to make sure I
got the sum of currents at each node right:

http://dl.dropbox.com/u/14219031/Dsp/sem-1a-linear-svf.jpg

Hopefully that can answer any of the questions about state variables
etc since most of what you have replied with is flying straight over
my head.

For completeness I've duplicated the code I previously posted, this
time with the correct position of the v0z assignment:

init:
v1 = v2 = 0;
v0z = v1z = v2z = 0;

process:
g = tan (pi * cutoff / samplerate);
k = damping factor (typically in the range 2 to 0);
v1z = v1;
v2z = v2;
v0 = input;
v1 = v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
v2 = v2z + g * (v1 + v1z);
v0z = v0;

outputs (the same as the analog circuit):
band = v1;
low = v2;
high = v0 - k*v1 - v2;
notch = high + low;
peak = high - low;


Andy
--
cytomic - sound music software
mobile: +61-450-774-230
skype: andrewsimper


}


unit eeFilters.Trap;

interface

type

  TTrapFilterMono = class
  private
    fSampleRate: integer;
    fFreq, fRes:single;
    procedure SetSampleRate(const Value: integer);
  protected
    g, k : double;
    v1z, v1, v2z, v2, v0, v0z : double;

    procedure CalcCoeffs;
    procedure SetFreq(aFreq:single);
    procedure SetRes(aRes:single);
  public
    constructor Create;

    function Process(In1:single):single;

    property SampleRate : integer read fSampleRate write SetSampleRate;
    property Freq       : single  read fFreq       write SetFreq;        //hertz
    property Res        : single  read fRes        write SetRes;         //0..1
  end;




  TTrapFilterStereo = class
  private
    fSampleRate: integer;
    fFreq, fRes:single;
    procedure SetSampleRate(const Value: integer);
  protected
    g, k    : double;

    ch1_v1z : double;
    ch1_v1  : double;
    ch1_v2z : double;
    ch1_v2  : double;
    ch1_v0  : double;
    ch1_v0z : double;

    ch2_v1z : double;
    ch2_v1  : double;
    ch2_v2z : double;
    ch2_v2  : double;
    ch2_v0  : double;
    ch2_v0z : double;


    procedure CalcCoeffs;
    procedure SetFreq(aFreq:single);
    procedure SetRes(aRes:single);
  public
    constructor Create;

    procedure ProcessAsLowPass(const In1, In2: double; out Out1, Out2:double);
    procedure ProcessAsHighPass(const In1, In2: double; out Out1, Out2:double);

    property SampleRate : integer read fSampleRate write SetSampleRate;
    property Freq       : single  read fFreq       write SetFreq;        //hertz
    property Res        : single  read fRes        write SetRes;         //0..1
  end;

implementation

uses
  Math;

{ TTrapFilterMono }

constructor TTrapFilterMono.Create;
begin
  fSampleRate := 44100;
  fRes        := 0;
  fFreq       := 400;

  v1z := 0;
  v1  := 0;
  v2z := 0;
  v2  := 0;
  v0  := 0;
  v0z := 0;
end;

procedure TTrapFilterMono.SetFreq(aFreq: single);
begin
  fFreq := aFreq;
  CalcCoeffs;
end;

procedure TTrapFilterMono.SetRes(aRes: single);
begin
  fRes := aRes;
  CalcCoeffs;
end;

procedure TTrapFilterMono.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  CalcCoeffs;
end;

procedure TTrapFilterMono.CalcCoeffs;
begin
  g := tan (pi * fFreq / fSampleRate);
  k := 2 - (2 * Res);   //damping factor (typically in the range 2 to 0);  
end;

function TTrapFilterMono.Process(In1: single): single;
begin
  v1z := v1;
  v2z := v2;
  v0  := In1;
  v1  := v1z + g * (v0 + v0z - 2*(g + k)*v1z - 2*v2z) / (1 + g*(g + k));
  v2  := v2z + g * (v1 + v1z);
  v0z := v0;

  result := v2;             //Low
  //result := v0 - k*v1 - v2; //high
  //result := v1;             //band
  //result := high + low;     //notch
  //result := high - low;     //peak
end;




{ TTrapFilterStereo }

constructor TTrapFilterStereo.Create;
begin
  fSampleRate := 44100;
  fRes        := 0;
  fFreq       := 400;

  ch1_v1z := 0;
  ch1_v1  := 0;
  ch1_v2z := 0;
  ch1_v2  := 0;
  ch1_v0  := 0;
  ch1_v0z := 0;

  ch2_v1z := 0;
  ch2_v1  := 0;
  ch2_v2z := 0;
  ch2_v2  := 0;
  ch2_v0  := 0;
  ch2_v0z := 0;
end;

procedure TTrapFilterStereo.SetFreq(aFreq: single);
begin
  fFreq := aFreq;
  CalcCoeffs;
end;

procedure TTrapFilterStereo.SetRes(aRes: single);
begin
  fRes := aRes;
  CalcCoeffs;
end;

procedure TTrapFilterStereo.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  CalcCoeffs;
end;

procedure TTrapFilterStereo.CalcCoeffs;
begin
  g := tan (pi * fFreq / fSampleRate);
  k := 2 - (2 * Res);   //damping factor (typically in the range 2 to 0);

  
end;

procedure TTrapFilterStereo.ProcessAsLowPass(const In1, In2: double; out Out1, Out2:double);
begin
  ch1_v1z := ch1_v1;
  ch1_v2z := ch1_v2;
  ch1_v0  := In1;
  ch1_v1  := ch1_v1z + g * (ch1_v0 + ch1_v0z - 2*(g + k)*ch1_v1z - 2*ch1_v2z) / (1 + g*(g + k));
  ch1_v2  := ch1_v2z + g * (ch1_v1 + ch1_v1z);
  ch1_v0z := ch1_v0;

  ch2_v1z := ch2_v1;
  ch2_v2z := ch2_v2;
  ch2_v0  := In2;
  ch2_v1  := ch2_v1z + g * (ch2_v0 + ch2_v0z - 2*(g + k)*ch2_v1z - 2*ch2_v2z) / (1 + g*(g + k));
  ch2_v2  := ch2_v2z + g * (ch2_v1 + ch2_v1z);
  ch2_v0z := ch2_v0;


  //LowPass.
  Out1 := ch1_v2;
  Out2 := ch2_v2;

  //result := v2;             //Low
  //result := v0 - k*v1 - v2; //high
  //result := v1;             //band
  //result := high + low;     //notch
  //result := high - low;     //peak
end;


procedure TTrapFilterStereo.ProcessAsHighPass(const In1, In2: double; out Out1,
  Out2: double);
begin
  ch1_v1z := ch1_v1;
  ch1_v2z := ch1_v2;
  ch1_v0  := In1;
  ch1_v1  := ch1_v1z + g * (ch1_v0 + ch1_v0z - 2*(g + k)*ch1_v1z - 2*ch1_v2z) / (1 + g*(g + k));
  ch1_v2  := ch1_v2z + g * (ch1_v1 + ch1_v1z);
  ch1_v0z := ch1_v0;

  ch2_v1z := ch2_v1;
  ch2_v2z := ch2_v2;
  ch2_v0  := In2;
  ch2_v1  := ch2_v1z + g * (ch2_v0 + ch2_v0z - 2*(g + k)*ch2_v1z - 2*ch2_v2z) / (1 + g*(g + k));
  ch2_v2  := ch2_v2z + g * (ch2_v1 + ch2_v1z);
  ch2_v0z := ch2_v0;


  //HighPass.
  Out1 := ch1_v0 - k* ch1_v1 - ch1_v2;
  Out2 := ch2_v0 - k* ch2_v1 - ch2_v2;
end;




end.
