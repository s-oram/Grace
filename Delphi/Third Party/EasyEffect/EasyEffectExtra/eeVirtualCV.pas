unit eeVirtualCV;

interface

uses
  Math;


//=== Virtual CV Functions ===
// Inspired by analog synths.
// 1 Modular Volt = 0.1 in audio range signals.
// I've decided to use that scaling because a standard
// audio range signal is -1..1 and the maximum standard
// eurorack audio signal is -10..10 volts.
type
  TModularVoltage = single;

function AudioRangeToModularVoltage(x:TModularVoltage):single; inline;
function ModularVoltageToAudioRange(x:single):TModularVoltage; inline;


// CV adjusts the base Freq. Output freq will equal base freq when CV = 0.
// When CV = 1, the base freq will be doubled.
// 1 volt = 1 octave change.
// 0 CV = 0 change.
// -1 CV = 1 octave lower.
function VoltsToFreq(const BaseFreq : single; const CV : TModularVoltage):single; inline;

implementation

function AudioRangeToModularVoltage(x:TModularVoltage):single; inline;
begin
  result := x * 10;
end;

function ModularVoltageToAudioRange(x:single):TModularVoltage; inline;
begin
  result := x * 0.1;
end;

//=== Virtual CV Functions ===
function VoltsToFreq(const BaseFreq : single; const CV : TModularVoltage):single;
begin
  result := BaseFreq*power(2,CV);
end;


end.
