// TSamplePhaseCounter encapsulates a phase counter for reading though
// sample data arrays. The class uses integers internally for sample index accuracy.
// (Floats will eventually loose accuracy as the sample index grows larger).
// Using integers also allows for some optimization.

unit eeSamplePhaseCounter;

interface

const
  OneOver2048 : double = 1 / 2048;

type
  TSamplePhase = record
    A    : integer;
    B    : integer;
    Frac : single;
  end;

  TSamplePhaseCounter = class
  private
    fFirstIndex: integer;
    fLastIndex: integer;
    fStepSize: double;
    procedure SetStepSize(const Value: double);
  protected
    CurPhase         : integer;
    IntPhaseFraction : integer;
    IntStepSize      : integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetPhase(NewPhaseIndex : integer = 0); inline;

    property PhaseIndex : integer read CurPhase;
    property FirstIndex : integer read fFirstIndex write fFirstIndex;
    property LastIndex  : integer read fLastIndex  write fLastIndex;

    property StepSize   : double  read fStepSize   write SetStepSize;

    function StepForward_NoLoop(out EndReached : boolean):TSamplePhase; overload; inline;
    function StepForward_NoLoop:TSamplePhase; overload; inline;
    function StepForward_Loop:TSamplePhase; inline;
  end;

implementation



{ TSamplePhaseCounter }

constructor TSamplePhaseCounter.Create;
begin
  ResetPhase;
  StepSize := 1;
end;

destructor TSamplePhaseCounter.Destroy;
begin

  inherited;
end;

procedure TSamplePhaseCounter.ResetPhase(NewPhaseIndex: integer);
begin
  CurPhase            := NewPhaseIndex;
  IntPhaseFraction := 0;
end;

procedure TSamplePhaseCounter.SetStepSize(const Value: double);
begin
  fStepSize := Value;
  IntStepSize := round(Value * 2048);
end;

function TSamplePhaseCounter.StepForward_NoLoop(out EndReached : boolean):TSamplePhase;
var
  dx : integer;
begin
  inc(IntPhaseFraction, IntStepSize);
  dx := IntPhaseFraction shr 11;
  IntPhaseFraction := IntPhaseFraction and 2047;

  if (CurPhase + dx) < (LastIndex - 1) then
  begin
    inc(CurPhase, dx);
    result.A    := CurPhase;
    result.B    := CurPhase + 1;
    result.Frac := IntPhaseFraction * OneOver2048;
    EndReached  := false;
  end else
  begin
    CurPhase    := LastIndex-1;
    result.A    := CurPhase;
    result.B    := LastIndex;
    result.Frac := 0.5;
    EndReached  := false;
  end;
end;


function TSamplePhaseCounter.StepForward_NoLoop:TSamplePhase;
var
  dx : integer;
begin
  inc(IntPhaseFraction, IntStepSize);
  dx := IntPhaseFraction shr 11;
  IntPhaseFraction := IntPhaseFraction and 2047;

  if (CurPhase + dx) < (LastIndex - 1) then
  begin
    inc(CurPhase, dx);
    result.A    := CurPhase;
    result.B    := CurPhase + 1;
    result.Frac := IntPhaseFraction * OneOver2048;
  end else
  begin
    CurPhase    := LastIndex-1;
    result.A    := CurPhase;
    result.B    := LastIndex;
    result.Frac := 0.5;
  end;
end;



function TSamplePhaseCounter.StepForward_Loop: TSamplePhase;
var
  dx : integer;
begin
  inc(IntPhaseFraction, IntStepSize);
  dx := IntPhaseFraction shr 11;
  IntPhaseFraction := IntPhaseFraction and 2047;

  if (CurPhase + dx) <= (LastIndex - 1) then
  begin
    inc(CurPhase, dx);
    result.A    := CurPhase;
    result.B    := CurPhase + 1;
    result.Frac := IntPhaseFraction * OneOver2048;
  end else
  begin
    inc(CurPhase, dx);
    dec(CurPhase, LastIndex-FirstIndex+1);
    result.A    := CurPhase;
    result.B    := LastIndex;
    result.Frac := IntPhaseFraction * OneOver2048;
  end;
end;



end.
