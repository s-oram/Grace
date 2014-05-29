unit eeParSmoother;

interface

type
  PParSmootherState = ^TParSmootherState;
  TParSmootherState = record
  public
    // TODO:MED There are lots of state storage variables here. Need to consolidate these variables.
    z1A : double;
    z2A : double;
    z1B : double;
    z2B : double;
    In0, In1, In2, Out1, Out2 : double;

    Offset : double;

    procedure Reset(const aOffset : double);
  end;

  TParSmoother = class
  private
  protected
    // Coefficients... NOTE: Depending on where the coefficients are sourced from,
    // the coefficent labels may be swapped. IE. A coefficients are labeled B and
    // vice-versa.
    b0: double;
    b1: double;
    b2: double;
    a1: double;
    a2: double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetTransitionTime(const ms, SampleRate: single);

    // TODO:HIGH it might make more sense to use one of the other filter forms here.
    //TODO:MED Refactor TargetValue + CurrentValue into one "InOutValue" ...maybe
    procedure Step_DirectForm1(const TargetValue : single; var CurrentValue : single; const State : PParSmootherState);
    procedure Step_DirectForm2(const TargetValue : single; var CurrentValue : single; const State : PParSmootherState);
    procedure Step_DirectForm2Transposed(const TargetValue : single; var CurrentValue : single; const State : PParSmootherState);
  end;

implementation

uses
  eeDsp,
  eeBiquadFilters;

{ TParSmootherState }

procedure TParSmootherState.Reset(const aOffset : double);
begin
  z1A := 0;
  z2A := 0;
  z1B := 0;
  z2B := 0;
  In0 := 0;
  In1 := 0;
  In2 := 0;
  Out1 := 0;
  Out2 := 0;
  Offset := aOffset;
end;

{ TParSmoother }

constructor TParSmoother.Create;
begin

end;

destructor TParSmoother.Destroy;
begin

  inherited;
end;

procedure TParSmoother.SetTransitionTime(const ms, SampleRate: single);
var
  Hz : single;
begin
  //TODO: It might be possible to find some optimisations in here somewhere...
  if ms > 0
    then Hz := 1 / (ms * 0.001)
    else Hz := SampleRate * 0.25;

  if Hz > SampleRate * 0.25 then Hz := SampleRate * 0.25;

  CalcBiquad_CriticallyDamped_LowPass(Hz, SampleRate, b0, b1, b2, a1, a2);
end;

procedure TParSmoother.Step_DirectForm1(const TargetValue: single; var CurrentValue: single; const State: PParSmootherState);
var
  outx : double;
begin
  State^.In0 := State^.In1;
  State^.In1 := State^.In2;
  State^.In2 := TargetValue - State^.Offset + kDenormal;

  Outx := (b0 * State^.In0) + (b1 * State^.In1) + (b2 * State^.in2) + (a1 * State^.Out1) + (a2 * State^.Out2);

  State^.Out2 := State^.Out1;
  State^.Out1 := Outx;

  CurrentValue := State^.Offset + Outx;
end;

procedure TParSmoother.Step_DirectForm2(const TargetValue: single; var CurrentValue: single; const State: PParSmootherState);
begin
  //Copy form eeBiquadFilterCore.pas
end;

procedure TParSmoother.Step_DirectForm2Transposed(const TargetValue: single; var CurrentValue: single; const State: PParSmootherState);
begin
  //Copy form eeBiquadFilterCore.pas
end;

end.
