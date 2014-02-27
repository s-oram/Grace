unit soLfo.WaveTableLfo;

interface

{$INCLUDE Defines.inc}
{$EXCESSPRECISION OFF}
{$SCOPEDENUMS ON}

uses
  Math,
  VamLib.Utils,
  eeOscPhaseCounter,
  eeDsp;

type
  TWaveTableLfoShape = (Sine, Tri, Sqr, Saw, Ramp);

  TWaveTableLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    fPhaseOffset: single;
    fSymmetry: single;
    fFreq: single;
    fWaveShape: TWaveTableLfoShape;
  protected
    StepSize : TOscPhaseCounter;
    LfoPhase : TOscPhaseCounter;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetPhase;

    procedure UpdateStepSize; //call when the step size needs to be re-calculated. Normally after changing any LFO parameter.

    function Step(out CycleEnd : boolean): single; overload; // generate the next LFO output sample.
    function Step: single; overload;

    property Bpm           : single read fBpm           write fBpm;
    property SampleRate    : single read fSampleRate    write fSampleRate;

    property Freq          : single read fFreq          write fFreq;          // LFO frequency in hertz.
    property PhaseOffset   : single read fPhaseOffset   write fPhaseOffset;   // Range 0..1
    property Symmetry      : single read fSymmetry      write fSymmetry;      // Range 0..1, with 0.5 being the default position.

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
  // TODO: This offset table should be smoothed with a filter or something...
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


function ReadWaveTable(const Phase : TOscPhaseCounter; const wt:TLfoWaveTable):single;
var
  TableIndex : integer;
  Frac       : single;
  ax : single;
  bx : single;
begin
  Phase.GetIndex256(TableIndex, Frac);

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
  LfoPhase := 0;
end;

function TWaveTableLfo.Step: single;
var
  x : boolean;
begin
  result := Step(x);
end;

procedure TWaveTableLfo.UpdateStepSize;
begin
  StepSize := 1 / SampleRate * Freq;
end;

function TWaveTableLfo.Step(out CycleEnd : boolean):single;
var
  pwmOffset      : single;
  xPhase         : TOscPhaseCounter;
  PhaseAndOffset : TOscPhaseCounter;
begin
  //TODO: we are keeping track of the LFO phase with a float variable.
  // This should be changed to a integer implementation to automatically
  // wrap around when overflowing and for quick TableIndex and Frac calculation.
  PhaseAndOffset := LfoPhase + PhaseOffset;

  if Symmetry < 0.5
    then pwmOffset := ReadWaveTable(PhaseAndOffset, OffsetTable)
    else pwmOffset := ReadWaveTable(1-PhaseAndOffset, OffsetTable);

  //TODO: This TOscPhaseCounter conversion involves a Round() call behind the
  // scenes. I wonder if the conversion can be hidden somehow.
  xPhase := PhaseAndOffset + TOscPhaseCounter(pwmOffset * (fSymmetry * -2 + 1));

  case WaveShape of
    TWaveTableLfoShape.Sine: result := ReadWaveTable(xPhase, SineTable);
    TWaveTableLfoShape.Tri:  result := ReadWaveTable(xPhase, TriTable);
    TWaveTableLfoShape.Sqr:  result := ReadWaveTable(xPhase, SqrTable);
    TWaveTableLfoShape.Saw:  result := ReadWaveTable(xPhase, SawTable);
    TWaveTableLfoShape.Ramp: result := 1 - ReadWaveTable(xPhase, SawTable);
  else
    raise Exception.Create('Type not handled.');
  end;

  CycleEnd := LfoPhase.IncByWithOverflowCheck(StepSize);
end;






initialization
  AreWaveTablesInitialized := false;

finalization


end.
