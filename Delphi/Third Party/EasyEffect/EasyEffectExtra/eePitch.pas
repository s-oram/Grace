{
  Some of these methods have been taken from Tobybear's
  PitchConverter unit.
}


unit eePitch;

interface

uses
  SysUtils, Math;

function CpsToMidi(x:double):integer; inline;
function MidiToCps(x: double): double; inline;

function CpsToSamples(x:double; SampleRate:single):single; inline;

function MsToSamples(ms:single; SampleRate:single):single; inline;
function SamplesToMs(Samples:single; SampleRate:single):single; inline;
function BeatsToMs(Beats,Tempo:single):single; inline;
function BeatsToSamples(Beats,Tempo,SampleRate:single):single; inline;

function SemiToneShiftToStepSize(Shift,StepSize:double):double; inline;

function MidiNoteToName(x:integer):string;

implementation

const
  GlobalTuneA4 = 440;

function CpsToMidi(x:double):integer;
begin
  if x<0 then x:=0;
 result := abs(round(12*log2(x/GlobalTuneA4)+69));
 if result>127 then result:=-1;
end;

function MidiToCps(x: double): double;
begin
 if (x>=0)and(x<=127) then
  result := GlobalTuneA4*power(2,(x-69)/12)
 else
  result:=-1;
end;

function SemiToneShiftToStepSize(Shift,StepSize:double):double;
begin
  result := StepSize * power(2,shift/12); 
end;

function MidiNoteToName(x:integer):string;
const notes:array[0..11] of string=
 ('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');
begin
  if (x>=0)and(x<=127)
    then result:=notes[x mod 12] + intTostr(x div 12-2)
    else result:='---';
end;

function MsToSamples(ms:single; SampleRate:single):single; inline;
begin
  result := SampleRate * ms * 0.001;
end;

function SamplesToMs(Samples:single; SampleRate:single):single; inline;
begin
  result := Samples / SampleRate * 1000;
end;

function BeatsToMs(Beats,Tempo:single):single; inline;
begin
  result := 60000 / Tempo * Beats * 4;
end;

function BeatsToSamples(Beats,Tempo,SampleRate:single):single; inline;
begin
  result := SampleRate * 60 / Tempo * Beats * 4;
end;

function CpsToSamples(x:double; SampleRate:single):single; inline;
begin
  result := SampleRate / x;
end;





end.



