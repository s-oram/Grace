unit soLfo.WaveTableLfo;

interface

{$INCLUDE Defines.inc}

{$SCOPEDENUMS ON}

uses
  Math,
  VamLib.Utils,
  eeDsp;

type
  TWaveTableLfoShape = (Sine, Tri, Sqr, Saw, Ramp);

  TWaveTableLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    fPhaseOffset: single;
    fPulseWidthMod: single;
    fFreq: single;
    fWaveShape: TWaveTableLfoShape;
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

    property Freq          : single read fFreq          write fFreq;          // LFO frequency in hertz.
    property PhaseOffset   : single read fPhaseOffset   write fPhaseOffset;   // Range 0..1
    property PulseWidthMod : single read fPulseWidthMod write fPulseWidthMod; // Range 0..1, with 0.5 being no modulation.


    property WaveShape : TWaveTableLfoShape read fWaveShape write fWaveShape;

  end;

implementation

uses
  SysUtils;

const
  kWaveTableSize = 256;

type
  TLfoWaveTable = array[0..kWaveTableSize] of single;

var
  SineTable   : TLfoWaveTable;
  SawTable    : TLfoWaveTable;
  TriTable    : TLfoWaveTable;
  SqrTable    : TLfoWaveTable;
  OffsetTable : TLfoWaveTable;

  AreWaveTablesInitialized : boolean;


procedure InitWaveTables;
var
  c1: Integer;
  x : single;
  MaxOffset : single;
  MidPoint : single;
begin
  AreWaveTablesInitialized := true;

  //===== Create the sine wave table =====
  for c1 := 0 to kWaveTableSize-1 do
  begin
    x := c1 / (kWaveTableSize - 1);
    x := Sin(2 * pi * x) * 0.5 + 0.5;
    SineTable[c1] := x;
  end;

  SineTable[kWaveTableSize] := SineTable[0];


  //===== Create the saw wave table =====
  // TODO: table should be created using additive synthesis.
  for c1 := 0 to kWaveTableSize-1 do
  begin
    x := c1 / (kWaveTableSize - 1);
    SawTable[c1] := x;
  end;

  SawTable[kWaveTableSize] := SawTable[0];

  //===== Create the triangle wave table =====
  // TODO: table should be created using additive synthesis.
  for c1 := 0 to kWaveTableSize-1 do
  begin
    x := (1-(c1 / (kWaveTableSize - 1))) * 4 - 2;
    if x > 1
      then x := 1 - (x - 1);
    if x < -1
      then x := -1 - (x + 1);

    TriTable[c1] := x * 0.5 + 0.5;
  end;

  TriTable[kWaveTableSize] := TriTable[0];


  //===== Create the triangle wave table =====
  // TODO: table should be created using additive synthesis.
  for c1 := 0 to kWaveTableSize-1 do
  begin
    x := c1 / (kWaveTableSize - 1);
    if x > 0.5
      then x := 1;
    if x < 0.5
      then x := 0;

    SqrTable[c1] := x;
  end;

  SqrTable[kWaveTableSize] := SqrTable[0];


  //======= create the offset table =====
  for c1 := 0 to kWaveTableSize-1 do
  begin
    x := c1 / (kWaveTableSize - 1);
    if x < 1/8 then
    begin
      MaxOffset := x / (1/8) * 0.5;
    end else
    begin
      MaxOffset := (x - (1/8)) / (7/8) * 0.5 + 0.5;
    end;

    MidPoint  := c1 / (kWaveTableSize - 1);

    OffsetTable[c1] := (MaxOffset - MidPoint);
  end;

  OffsetTable[kWaveTableSize] := OffsetTable[0];
end;


function ReadWaveTable(const Phase : single; const wt:TLfoWaveTable):single;
var
  TableIndex : integer;
  Frac       : single;
  ax : single;
  bx : single;
begin
  TableIndex := floor(Phase * kWaveTableSize);
  Frac       := (Phase * kWaveTableSize) - TableIndex;

  ax := wt[TableIndex];
  bx := wt[TableIndex + 1];

  result := LinearInterpolation(ax,bx, Frac);
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
  pwmOffset : single;
  xPhase : single;
begin
  if PulseWidthMod < 0.5
    then pwmOffset := ReadWaveTable(LfoPhase, OffsetTable)
    else pwmOffset := ReadWaveTable(1-LfoPhase, OffsetTable);

  xPhase := LfoPhase + pwmOffset * (fPulseWidthMod * -2 + 1);
  if xPhase < 0
    then xPhase := xPhase + 1;
  if xPhase >= 1
    then xPhase := xPhase - 1;


  case WaveShape of
    TWaveTableLfoShape.Sine: result := ReadWaveTable(xPhase, SineTable);
    TWaveTableLfoShape.Tri:  result := ReadWaveTable(xPhase, TriTable);
    TWaveTableLfoShape.Sqr:  result := ReadWaveTable(xPhase, SqrTable);
    TWaveTableLfoShape.Saw:  result := ReadWaveTable(xPhase, SawTable);
    TWaveTableLfoShape.Ramp: result := 1 - ReadWaveTable(xPhase, SawTable);
  else
    raise Exception.Create('Type not handled.');
  end;







  {
  pwmOffset := ReadWaveTable(LfoPhase, OffsetTable);

  xPhase := LfoPhase + pwmOffset * (fPulseWidthMod * 2 - 1);
  if xPhase < 0  then xPhase := xPhase + 1;
  if xPhase >= 1 then xPhase := xPhase - 1;

  result := ReadWaveTable(xPhase, SineTable);
  }

  //result := ReadWaveTable(LfoPhase, OffsetTable);




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
