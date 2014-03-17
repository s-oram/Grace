unit FilterCore.MoogLadder;

interface

uses
  VamLib.Utils,
  Math;

type
  TOnePoleSection = record
  private
    zA, zB : double;
  public
    g : double;

    procedure Reset;
    function Step(Input:double):double;
  end;


  TMoogLadderFilterCore = class
  private
    LpA : TOnePoleSection;
    LpB : TOnePoleSection;
    LpC : TOnePoleSection;
    LpD : TOnePoleSection;
    z1  : double;
    fG: double;
    fFeedbackGain: double;
    fCompGain: double;
    procedure SetG(const Value: double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    function Step(Input:double):double;


    // G is cutoff frequency where
    // G = 2 * pi * Cutoff / SampleRate
    // G is only approximate and will need to be
    // compensated.
    property CutoffGain   : double read fG write SetG;
    property FeedbackGain : double read fFeedbackGain write fFeedbackGain;
    property CompGain     : double read fCompGain     write fCompGain;
  end;



implementation

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

function NonLinearSpice_Nasty(x:double):double;
const
  a : double = 0;
  b : double = 0.833588;
  c : double = 0;
  d : double = -0.090188;
begin
  x := Clamp(x * 1.5, -1.75, 1.75);
  result := (a + (b * x) + (c * x * x) + (d * x * x * x));
end;

function NonLinearSpice_orig(x:double):double;
begin
  result := tanh(x);
end;

function NonLinearSpice(x:double):double;
// Source: http://www.kvraudio.com/forum/viewtopic.php?t=262823&start=45
begin
  result := sign(x) * (1 - 1 / (1 + abs(x) + power(x,2) + 0.66422417311781 * power(x,3) + 0.36483285408241 * power(x,4)))
end;

function NonLinearSpice_Flavor2(x:double):double;
// Source: http://www.kvraudio.com/forum/viewtopic.php?t=262823&start=45
var
  xa  : double;
  x2  : double;
  x3  : double;
  x4  : double;
  x7  : double;
  res : double;
begin
  xa := abs(x);
  x2 := xa * xa;
  x3 := xa * x2;
  x4 := x2 * x2;
  x7 := x3 * x4;
  res := (1.0 - 1.0 / (1.0 + xa + x2 + 0.58576695 * x3 + 0.55442112 * x4 + 0.057481508 * x7));
  result := res * sign(x);
end;


{ TOnePoleSection }

procedure TOnePoleSection.Reset;
begin
  zA := 0;
  zB := 0;
  g  := 0;
end;

function TOnePoleSection.Step(Input: double): double;
const
  Gain1 : double = 1/1.3;
  Gain2 : double = 0.3/1.3;
var
  x : double;
begin
  Input := NonLinearSpice_Flavor2(Input);

  x := (Input * Gain1) + (zA * Gain2) - (zB);
  x := x * g;
  x := x + zB;
  result := x;

  zA := Input;
  zB := x;
end;

{ TMoogLadderFilterCore }

constructor TMoogLadderFilterCore.Create;
begin
  LpA.Reset;
  LpB.Reset;
  LpC.Reset;
  LpD.Reset;
  z1 := 0;

  FeedbackGain := 0;
  CompGain     := 0.5;
end;

destructor TMoogLadderFilterCore.Destroy;
begin

  inherited;
end;

procedure TMoogLadderFilterCore.Reset;
begin
  LpA.Reset;
  LpB.Reset;
  LpC.Reset;
  LpD.Reset;
  z1 := 0;
end;

procedure TMoogLadderFilterCore.SetG(const Value: double);
begin
  fG := Value;

  LpA.g := Value;
  LpB.g := Value;
  LpC.g := Value;
  LpD.g := Value;
end;

function TMoogLadderFilterCore.Step(Input: double): double;
var
  x       : double;
  OutputA : double;
  OutputB : double;
  OutputC : double;
  OutputD : double;
  OutputE : double;
  Feedback : double;
begin
  Feedback := z1 - (Input * CompGain);
  Feedback := Feedback * FeedbackGain * 4;

  x := Input - Feedback;
  x := NonLinearSpice_Flavor2(x);

  OutputA := x;
  x := LpA.Step(x);

  OutputB := x;
  x := LpB.Step(x);

  OutputC := x;
  x := LpC.Step(x);

  OutputD := x;
  x := LpD.Step(x);

  OutputE := x;
  z1 := x;

  result := OutputE;
end;

end.
