unit B2.Lfo.Extra;

interface


function FrequencyToRate(Frequency, SampleRate:double):double; inline;
function SyncToRate(SyncFactor, BPM, SampleRate: double): double; inline;

implementation

uses
  eeDsp;

function FrequencyToRate(Frequency, SampleRate:double):double;
begin
  //NOTE: FrequencyToRate() assumes the Lfo Rate ranges between 0 and 1. 0 being no movement, 1 being a full oscillation per step.
  // Output range is 0..1. (Rate = Frequency / (SampleRate * 0.5))

  result := Frequency / (SampleRate * 0.5);
end;

function SyncToRate(SyncFactor, BPM, SampleRate: double): double;
var
  Samples : double;
begin
  Samples := SyncToSamples(SyncFactor, BPM, SampleRate);
  result := SampleRate / Samples / SampleRate;
end;

end.
