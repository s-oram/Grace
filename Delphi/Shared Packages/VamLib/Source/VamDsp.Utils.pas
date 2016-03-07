unit VamDsp.Utils;

interface

// TODO:MED Maybe deprecate this unit and move it to AudioToolbox.Utils.pas

// Look at eeDsp for additional functions.

// TODO:HIGH Need to add testing and documentation for these methods.
function MillisecondsToSamples(ms, SampleRate: single): single; inline;
function SamplesToMilliseconds(Samples, SampleRate: single): single; inline;

function SyncToSamples(SyncFactor, BPM, SampleRate: double): double; inline;
function SamplesToSync(Samples, BPM, SampleRate: double): double; inline;

function SyncToFreq(const SyncFactor, BPM : double):double; inline;
function FreqToSync(const Freq, BPM : double):double; inline;

function DecibelsToLinear(dB:single):single; inline;
function LinearToDecibels(const Linear:single):single; inline;

implementation

uses
  Math;

function MillisecondsToSamples(ms, SampleRate: single): single; inline;
begin
 //result := ms * SampleRate / 1000;
 result := ms * SampleRate * 0.001;
end;

function SamplesToMilliseconds(Samples, SampleRate: single): single; inline;
begin
 result := Samples * 1000 / SampleRate;
end;

function SyncToSamples(SyncFactor, BPM, SampleRate: double): double; inline;
begin
  //TODO:MED This method doesn't match the SyncToFreq conversion. AFAIK
  result := SyncFactor * SampleRate * 60 / BPM;

  // TODO:HIGH need to document the expected outputs for normal input values.
  // need to ensure the results are consistent with the syncToFreq() function.
end;

function SyncToFreq(const SyncFactor, BPM : double):double; inline;
begin
  // TODO:MED This matches Zebra! This is right. I don't think the other conversion methods are!!
  // 1/1 should corrospond to 0.5hz = 1 bar at 120bpm.
  // 1/4 should corrospond to 4/4 kick drum or 0.25hz.
  // 1/4 dot. The dot adds 50%. ie 2->3.
  result := BPM / (SyncFactor * 240);
end;

function FreqToSync(const Freq, BPM : double):double; inline;
const
  OneOver240 = 1/240;
begin
  result := BPM / Freq * OneOver240;
end;

function SamplesToSync(Samples, BPM, SampleRate: double): double; inline;
begin
  //TODO:MED This method doesn't match the SyncToFreq conversion. AFAIK
  result := Samples * BPM / (SampleRate * 60);
end;

function DecibelsToLinear(dB:single):single; inline;
const
  OneOver20 = 1/20;
begin
  if dB > -120
    then result := Power(10, db * OneOver20)
    else result := 0;
end;


function LinearToDecibels(const Linear:single):single; inline;
const
  Neg120dB = 0.000001;
begin
  if Linear > Neg120dB
    then result := 20 * Log10(Linear)
    else result := -120;
end;



end.
