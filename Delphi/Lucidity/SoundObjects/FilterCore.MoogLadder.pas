unit FilterCore.MoogLadder;

interface

uses
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
    property G : double read fG write SetG;

    property FeedbackGain : double read fFeedbackGain write fFeedbackGain;
    property CompGain     : double read fCompGain     write fCompGain;
  end;

implementation

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
  Input := Tanh(Input);

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
  x := tanh(x);

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
