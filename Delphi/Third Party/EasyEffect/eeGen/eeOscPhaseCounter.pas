// OscPhaseCounter is an integer based phase counter designed for
// being used in oscillator type objects.
// An integer based phase counter is good to use because it allows for
// some optimizations.

unit eeOscPhaseCounter;

interface


const
  OneOver2048 : double = 1 / 2048;

type
  TSamplePhase = record
    A    : integer;
    B    : integer;
    Frac : single;
  end;

  TOscPhaseCounter = class
  private
    fSampleRate: integer;
    fTableSize: integer;
    fFreq: single;
    procedure SetTableSize(const Value: integer);
    procedure SetFreq(const Value: single);
  protected
    FloatPhase : single;
    FloatStepSize : single;


    IntStepSize : integer;
    IntPhase    : cardinal;
    IntTableEnd : cardinal;
    ShiftFactor : integer;

    procedure UpdateStepSize; inline;
  public
    constructor Create;
    destructor Destroy; override;

    function Step:TSamplePhase;

    property Freq       : single  read fFreq       write SetFreq; //Hertz
    property SampleRate : integer read fSampleRate write fSampleRate;
    property TableSize  : integer read fTableSize  write SetTableSize;
  end;

implementation

uses
  Math,
  SysUtils, eeDsp;

{ TOscPhaseCounter }

constructor TOscPhaseCounter.Create;
begin
  IntPhase    := 0;
  IntTableEnd := 2048 * 2048-1;

  fSampleRate := 44100;
  fTableSize  := 2048;
  fFreq       := 200;

  FloatPhase := 0;
  FloatStepSize := 0;
end;

destructor TOscPhaseCounter.Destroy;
begin

  inherited;
end;

procedure TOscPhaseCounter.SetTableSize(const Value: integer);
begin
  //Table size must be power of two.
  assert((Value = 16) or (Value = 32) or (Value = 64) or (Value = 128) or (Value = 256) or (Value = 512) or (Value = 1024) or (Value = 2048));

  case Value of
    16   : ShiftFactor := 18;
    32   : ShiftFactor := 17;
    64   : ShiftFactor := 16;
    128  : ShiftFactor := 15;
    256  : ShiftFactor := 14;
    512  : ShiftFactor := 13;
    1024 : ShiftFactor := 12;
    2048 : ShiftFactor := 11;
  else
    raise Exception.Create('Unexpected Table Size');
  end;

  fTableSize := Value;
  IntTableEnd := value * 2048-1;
  UpdateStepSize;
end;

procedure TOscPhaseCounter.SetFreq(const Value: single);
begin
  fFreq := Value;
  UpdateStepSize;
end;

procedure TOscPhaseCounter.UpdateStepSize;
begin
  IntStepSize := round(TableSize / SampleRate * Freq * 2048);
  FloatStepSize := 1 / SampleRate * Freq;
end;

function TOscPhaseCounter.Step: TSamplePhase;
var
  pos : integer;
begin
  pos := floor(FloatPhase * TableSize);
  if Pos >= TableSize then Pos := Pos - TableSize;
  result.A := Pos;

  inc(Pos);
  if Pos >= TableSize then Pos := Pos - TableSize;
  result.B := Pos;

  result.Frac := (FloatPhase * TableSize) - floor(FloatPhase * TableSize);

  FloatPhase := FloatPhase + FloatStepSize;

  if FloatPhase >= 1 then FloatPhase := FloatPhase - 1;
  assert(FloatPhase < 1);

  {
  result.A := IntPhase shr ShiftFactor;
  result.B := result.A + 1;
  result.Frac := (IntPhase - (result.A shl ShiftFactor)) * OneOver2048;

  inc(IntPhase, IntStepSize);
  IntPhase := IntPhase and IntTableEnd; //automatically wrap phase counter.
  }
end;



end.
