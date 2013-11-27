unit soDynamicWaveTableOsc.WaveGen;

interface

uses
  soDynamicWaveTableOsc;

//====== HIGH Level functions ================
procedure GenerateSawWaveform(var Osc : TDynamicWaveTableOsc; const RootFreq, MaxFreq : single);


//====== Low Level functions ================
// Low level wave building functions that can be used together to build a final waveshape.
procedure ClearWaveData(var Osc : TDynamicWaveTableOsc);

procedure AddHarmonic(var Osc : TDynamicWaveTableOsc; const Harmonic : integer; const Level, PhaseOffset:single);


function CalcMaxHarmonics(RootFreq, MaxFreq:single):integer;


implementation

uses
  Math;

procedure ClearWaveData(var Osc : TDynamicWaveTableOsc);
var
  c1: Integer;
begin
  assert(assigned(Osc));

  for c1 := 0 to Osc.TableSize-1 do
  begin
    Osc.WaveData[c1] := 0;
  end;

end;

procedure AddHarmonic(var Osc : TDynamicWaveTableOsc; const Harmonic : integer; const Level, PhaseOffset:single);
var
  c1 : integer;
  PhaseIndex : single;
  x : single;
begin
  assert(assigned(Osc));
  assert(Harmonic >= 1);
  assert(Harmonic <= Osc.TableSize div 4);

  assert((Level >= 0) and (Level <= 1));
  assert((PhaseOffset >= 0) and (PhaseOffset <= 1));

  for c1 := 0 to Osc.TableSize-1 do
  begin
    PhaseIndex := (c1 * Harmonic/Osc.TableSize) + PhaseOffset;
    x := Sin(PhaseIndex * 2 * pi) * Level;

    Osc.WaveData[c1] := Osc.WaveData[c1] + x;
  end;
end;

procedure GenerateSawWaveform(var Osc : TDynamicWaveTableOsc; const RootFreq, MaxFreq : single);
var
  Harmonics : integer;
  c1: Integer;
  Index : integer;
  Level : single;
begin
  Harmonics := CalcMaxHarmonics(RootFreq, MaxFreq);
  assert(Harmonics >= 1);

  ClearWaveData(Osc);

  //Generate a saw waveform...
  AddHarmonic(Osc, 1, 0.8, 0);
  Level := 0.7;
  for c1 := 1 to (Harmonics div 2) -1 do
  begin
    Index := c1 * 2;
    AddHarmonic(Osc, Index, Level, 0);
    Level := Level * 0.9;
  end;
end;

function CalcMaxHarmonics(RootFreq, MaxFreq:single):integer;
var
  x : single;
  HarmonicCount : integer;
begin
  HarmonicCount := Floor(MaxFreq / RootFreq);
  result := HarmonicCount;
end;

end.
