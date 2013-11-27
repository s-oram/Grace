{
  A bunch of functions for implementing Zero delay filters as described by
  Vadim Zavalishin in
  "Preserving the LTI system topology in s- to z- plane transforms"
}

unit eeZeroDelayFilters;

interface


function PitchToFrequency(Pitch:single):double;
function PrewarpBT(Frequency, SampleRate:double):double;

function Tan_approx(value:double):double;

function FeedbackSolver(x, g, s, k:double):double;

function ResToD(Res:double):double;
function MultAdd(x,a,y:double):double;





type
  TBiLinearTransform = class
  private
    Oldx:double;
    z1:double;
    fG: double;
    fS: double;
  public
    constructor Create;
    function Calc(x, InG, InS: double; w2:double):double;

    property G:double read fG;
    property S:double read fS;
  end;




implementation

uses
  Math;


// PitchToFrequency(Pitch:single):single;
// Algorithm copied from Vadim Zavalishin's 'Keep Topology' Reaktor
// tutorial ensembles.
function PitchToFrequency(Pitch:single):double;
begin
  result := Power(1.05946, Pitch) * 8.17742;
end;

// PrewarpBT accepts a Frequency in HZ and produces the halved prewarped cutoff for
// bilinear-transofrmed filers. (result is  w/2)
function PrewarpBT(Frequency, SampleRate:double):double;
begin
  assert(Frequency >= 0);     
  result := Tan(Frequency * (PI / SampleRate));
end;




// Tan_approx copied from copied from Vadim Zavalishin's 'Keep Topology' Reaktor
// tutorial ensembles.
// Valid for -1..1.
// max error ~10%
function Tan_approx(value:double):double;
var
  v2:double;
begin
  result := (value * value * value * value * value * 0.13333333) + (value * value * value * 0.333333333) + value;
end;



function FeedbackSolver(x, g, s, k:double):double;
begin
  result := (x * g + s) / (g * k + 1);
end;

function ResToD(Res:double):double;
begin
  result := (1 - Res) + (1 - Res);
end;

function MultAdd(x,a,y:double):double;
begin
  result := x * a + y;
end;

{ TBiLinearTransform }

constructor TBiLinearTransform.Create;
begin
  Oldx := 0;
  z1 := 0;
  fS := 0;
  fG := 0;
end;


function TBiLinearTransform.Calc(x, InG, InS: double; w2: double):double;
var
  y:double;
begin
  x  := x * w2;
  y  := 2 * z1 + x;
  z1 := z1 + x;


  fG := InG * w2;
  fS := InS * w2 + 2 * z1;

  result := y;


end;




end.
