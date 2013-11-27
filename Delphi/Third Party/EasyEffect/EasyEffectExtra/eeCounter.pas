unit eeCounter;

interface

  {
    TCounter
    In many places I've used floats to store phase or sample index positions.
    Floats are handy as they are easy to use with fractional values. They
    are a inefficient when working with sample data store in arrays however.
    When working with floats, the audio routine needs to call Floor() each time
    a sample index value is required.

    Working with integer based counters is more efficient as bit shift operations
    can be used to calculate the integer and fractional part of the phase/sample index
    value.

    TCounter wraps up all the integer counter handling so it can be used as easily
    as a float counter.
  }

const
  CounterFracOffset      : cardinal = $80000000;
  CounterFracOffsetSHR12 : cardinal = $80000000 shr 12;
  OneOver4096            : double   = 1 / 4096;

type
  PCounter = ^TCounter;
  TCounter = record
  private
    PhaseWhole      : integer;
    PhaseFrac       : integer;
    IntegerStepSize : integer;
    function GetStepSize: double;
    procedure SetStepSize(const Value: double);
  public
    // operator overloading
    // http://docwiki.embarcadero.com/RADStudio/XE4/en/Operator_Overloading_%28Delphi%29

    // ========== Single overloads =============================================
     // ========= integer overloads ============================================
    class operator Add(a: TCounter; b: integer):TCounter; //inline;
    class operator Subtract(a: TCounter; b: integer):TCounter; //inline;

    class operator Equal(a: TCounter; b: integer): Boolean; //inline;
    class operator NotEqual(a: TCounter; b: integer): Boolean; //inline;

    class operator GreaterThan(a: TCounter; b: integer): Boolean; //inline;
    class operator GreaterThanOrEqual(a: TCounter; b: integer): Boolean; //inline;

    class operator LessThan(a: TCounter; b: integer): Boolean; //inline;
    class operator LessThanOrEqual(a: TCounter; b: integer): Boolean; //inline;


    // =========================================================================

    procedure ResetTo(Value : integer); overload; //inline;
    procedure ResetTo(Value : double);  overload; //inline;

    function AsFloat : double; inline;

    function IntegerPart    : integer; inline; // When Counter equals 1.0345, returns 1
    function FractionalPart : double;  inline; // When Counter equals 1.0345, returns 0.0345

    procedure Step; inline;

    property StepSize : double read GetStepSize write SetStepSize;
  end;

implementation

uses
  Math;

{ TCardinalPhaseCounter }

function TCounter.IntegerPart: integer;
begin
  result := PhaseWhole;
end;



function TCounter.FractionalPart: double;
begin
  result := PhaseFrac * OneOver4096;
end;

function TCounter.AsFloat: double;
begin
  result := PhaseWhole + PhaseFrac * OneOver4096;
end;

procedure TCounter.ResetTo(Value: integer);
begin
  PhaseWhole := Value;
  PhaseFrac  := 0;
end;

procedure TCounter.ResetTo(Value: double);
begin
  PhaseWhole := Floor(Value);
  PhaseFrac  := Round((Value - PhaseWhole) * 4096);
end;

procedure TCounter.SetStepSize(const Value: double);
begin
  IntegerStepSize := round(Value * 4096);
end;

function TCounter.GetStepSize: double;
begin
  result := IntegerStepSize * OneOver4096;
end;

procedure TCounter.Step;
begin
  inc(PhaseFrac, IntegerStepSize);
  PhaseWhole := PhaseWhole + (((CounterFracOffset + PhaseFrac) shr 12) - CounterFracOffsetSHR12);
  PhaseFrac  := (PhaseFrac and $00FFF);
end;


//=========  operator overloads ==================

class operator TCounter.GreaterThan(a: TCounter; b: integer): Boolean;
begin
  result := (a.PhaseWhole > b);
end;

class operator TCounter.GreaterThanOrEqual(a: TCounter; b: integer): Boolean;
begin
  result := (a.PhaseWhole >= b);
end;

class operator TCounter.LessThan(a: TCounter; b: integer): Boolean;
begin
  result := (a.PhaseWhole < b);
end;

class operator TCounter.LessThanOrEqual(a: TCounter; b: integer): Boolean;
begin
  result := (a.PhaseWhole <= b);
end;

class operator TCounter.Add(a: TCounter; b: integer): TCounter;
begin
  result.PhaseWhole := a.PhaseWhole + b;
end;

class operator TCounter.Subtract(a: TCounter; b: integer): TCounter;
begin
  result.PhaseWhole := a.PhaseWhole - b;
end;

class operator TCounter.NotEqual(a: TCounter; b: integer): Boolean;
begin
  result := (a.PhaseWhole <> b);
end;

class operator TCounter.Equal(a: TCounter; b: integer): Boolean;
begin
  result := (a.PhaseWhole = b);
end;







end.
