unit soLfo.WaveTableLfo;

interface

uses
  Math,
  VamLib.Utils,
  eeDsp;

type
  TWaveTableLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    fPhaseOffset: single;
    fPulseWidthMod: single;
    fFreq: single;
  protected
    //TODO: this should be changed to be cardinal values...
    StepSize : single;
    LfoPhase : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetPhase;

    procedure UpdateStepSize; //call when the step size needs to be re-calculated. Normally after changing any LFO parameter.
    function Step : single; // generate the next LFO output sample.

    property Bpm        : single read fBpm        write fBpm;
    property SampleRate : single read fSampleRate write fSampleRate;

    property Freq          : single read fFreq          write fFreq;        // LFO frequency in hertz.
    property PhaseOffset   : single read fPhaseOffset   write fPhaseOffset; //Range 0..1
    property PulseWidthMod : single read fPulseWidthMod write fPulseWidthMod; //Range 0..1, with 0.5 being no modulation.

  end;

implementation

{$EXCESSPRECISION OFF}

const
  kWaveTableSize = 256;

var
  SineTable : array[0..kWaveTableSize] of single;
  SawTable  : array[0..kWaveTableSize] of single;
  TriTable  : array[0..kWaveTableSize] of single;
  SqrTable  : array[0..kWaveTableSize] of single;

  AreWaveTablesInitialized : boolean;


procedure InitWaveTables;
var
  c1: Integer;
  x : single;

begin
  AreWaveTablesInitialized := true;

  for c1 := 0 to kWaveTableSize-1 do
  begin
    x := c1 / (kWaveTableSize - 1);
    x := Sin(2 * pi * x) * 0.5 + 0.5;
    x := Clamp(x, kDenormal, 1);
    SineTable[c1] := x;
  end;

  SineTable[kWaveTableSize] := SineTable[0];
end;

{ TWaveTableLfo }

constructor TWaveTableLfo.Create;
begin
  if not AreWaveTablesInitialized
    then InitWaveTables;

  StepSize := 0;
  LfoPhase := 0;
end;

destructor TWaveTableLfo.Destroy;
begin

  inherited;
end;

procedure TWaveTableLfo.ResetPhase;
begin
  //TODO:
end;

function TWaveTableLfo.Step: single;
var
  TableIndex : integer;
  Frac       : single;
  ax : single;
  bx : single;
  x : single;
begin
  TableIndex := floor(LfoPhase * kWaveTableSize);
  Frac := (LfoPhase * kWaveTableSize) - TableIndex;

  ax := SineTable[TableIndex];
  bx := SineTable[TableIndex + 1];

  result := LinearInterpolation(ax,bx, Frac);


  LfoPhase := LfoPhase + StepSize;
  if LfoPhase >= 1 then LfoPhase := LfoPhase - 1;

  // output should be ranged 0..1.
  assert(InRange(result,0,1));
end;

procedure TWaveTableLfo.UpdateStepSize;
begin
  StepSize := 1 / SampleRate * Freq;
end;




initialization
  AreWaveTablesInitialized := false;

finalization


end.
