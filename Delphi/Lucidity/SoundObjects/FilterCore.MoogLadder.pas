unit FilterCore.MoogLadder;

interface

// This moog ladder filter's implementation is based on
// Oscillator and Filter Algortithms for Virtual Analog Synthesis
// by Antti Huovilainen and Vesa Valimaki
// Publised in Computer Music Journal. year and issue ???
//
// NOTE: Possible music paper. Tanh alternatives and
// how they impact the filter spectrum.

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

    fG: double;
    fFeedbackGain: double;
    fCompGain: double;
    procedure SetG(const Value: double);
  protected
    FilterState : record
      z1  : double;
      OutputA : double;
      OutputB : double;
      OutputC : double;
      OutputD : double;
      OutputE : double;
    end;

    procedure UpdateFilterState(const Input : double); inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;


    function StepAs2PoleLP(Input:double):double;
    function StepAs2PoleBP(Input:double):double;
    function StepAs2PoleHP(Input:double):double;

    function StepAs4PoleLP(Input:double):double;
    function StepAs4PoleBP(Input:double):double;
    function StepAs4PoleHP(Input:double):double;

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

function NonLinearSpice_Flavor2(x:double):double; inline;
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
  FilterState.z1 := 0;

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
  FilterState.z1 := 0;
end;

procedure TMoogLadderFilterCore.SetG(const Value: double);
begin
  fG := Value;

  LpA.g := Value;
  LpB.g := Value;
  LpC.g := Value;
  LpD.g := Value;
end;

procedure TMoogLadderFilterCore.UpdateFilterState(const Input: double);
var
  x       : double;
  Feedback : double;
begin
  Feedback := FilterState.z1 - (Input * CompGain);
  Feedback := Feedback * FeedbackGain * 4;

  x := Input - Feedback;
  x := NonLinearSpice_Flavor2(x);

  FilterState.OutputA := x;
  x := LpA.Step(x);

  FilterState.OutputB := x;
  x := LpB.Step(x);

  FilterState.OutputC := x;
  x := LpC.Step(x);

  FilterState.OutputD := x;
  x := LpD.Step(x);

  FilterState.OutputE := x;
  FilterState.z1 := x;
end;

function TMoogLadderFilterCore.StepAs2PoleLP(Input: double): double;
begin
  UpdateFilterState(Input);
  result := FilterState.OutputC;
end;

function TMoogLadderFilterCore.StepAs2PoleBP(Input: double): double;
begin
  UpdateFilterState(Input);
  result := (FilterState.OutputB * 2) + (FilterState.OutputC * -2);
end;

function TMoogLadderFilterCore.StepAs2PoleHP(Input: double): double;
begin
  UpdateFilterState(Input);
  result := (FilterState.OutputA * 1) + (FilterState.OutputB * -2) + (FilterState.OutputC * 1);
end;

function TMoogLadderFilterCore.StepAs4PoleLP(Input: double): double;
begin
  UpdateFilterState(Input);
  result := FilterState.OutputE;
end;

function TMoogLadderFilterCore.StepAs4PoleBP(Input: double): double;
begin
  UpdateFilterState(Input);
  result := (FilterState.OutputC * 4) + (FilterState.OutputD * -8) + (FilterState.OutputE * 4);
end;

function TMoogLadderFilterCore.StepAs4PoleHP(Input: double): double;
begin
  UpdateFilterState(Input);
  result := (FilterState.OutputA *  1) +
            (FilterState.OutputB * -4) +
            (FilterState.OutputC *  6) +
            (FilterState.OutputD * -4) +
            (FilterState.OutputE *  1);
end;




end.
