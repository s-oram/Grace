unit Vst2PerformanceTest;

interface

uses
  TestHost.Vst2.DAEffectX,
  VamLib.MoreTypes,
  TestHost.Vst2;

type
  TVst2PerformanceTest = class
  private
    fSampleRate : integer;
    fBufferSize : integer;
  protected
    Plugin : TVst2Plugin;
    TimeInfo : VstTimeInfo;
    InputPointers  : array of PSingle;
    OutputPointers : array of PSingle;
    InputBuffers  : array of TArrayOfSingle;
    OutputBuffers : array of TArrayOfSingle;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetupTest(const aPlugin : TVst2Plugin; const SampleRate, BufferSize : integer);
    procedure RunTest;
  end;

implementation

uses
  TestHost.Vst2.DAEffect.CustomEx.Vam;

{ TVst2PerformanceTest }

constructor TVst2PerformanceTest.Create;
begin

end;

destructor TVst2PerformanceTest.Destroy;
var
  c1 : integer;
begin

  for c1 := 0 to Length(InputBuffers)-1 do
  begin
    SetLength(InputBuffers[c1], 0);
  end;

  for c1 := 0 to Length(OutputBuffers)-1 do
  begin
    SetLength(OutputBuffers[c1], 0);
  end;

  SetLength(InputBuffers, 0);
  SetLength(OutputBuffers, 0);

  SetLength(InputPointers, 0);
  SetLength(OutputPointers, 0);

  inherited;
end;

procedure TVst2PerformanceTest.SetupTest(const aPlugin : TVst2Plugin; const SampleRate, BufferSize : integer);
var
  c1 : integer;
  InCount : integer;
  OutCount : integer;
  c2: Integer;
  ptr : pointer;
begin
  fSampleRate := SampleRate;
  fBufferSize := BufferSize;

  Plugin := aPlugin;
  Plugin.Suspend;
  Plugin.SetSampleRate(SampleRate);
  Plugin.SetBlockSize(BufferSize);
  Plugin.Resume;

  InCount := Plugin.VstEffect.numInputs;
  OutCount := Plugin.VstEffect.numOutputs;

  SetLength(InputPointers, InCount);
  SetLength(InputBuffers, InCount);
  for c1 := 0 to InCount-1 do
  begin
    SetLength(InputBuffers[c1], BufferSize);
    InputPointers[c1] := @InputBuffers[c1,0];
    // Zero the buffers after creating.
    for c2 := 0 to BufferSize-1 do InputBuffers[c1,c2] := 0;
  end;

  SetLength(OutputPointers, OutCount);
  SetLength(OutputBuffers, OutCount);
  for c1 := 0 to OutCount-1 do
  begin
    SetLength(OutputBuffers[c1], BufferSize);
    OutputPointers[c1] := @OutputBuffers[c1,0];
  end;

  TimeInfo.samplePos          := 0;
  TimeInfo.sampleRate         := SampleRate;
  TimeInfo.nanoSeconds        := 0;
  TimeInfo.ppqPos             := 0;   // Musical Position, in Quarter Note (1.0 equals 1 Quarter Note)
  TimeInfo.tempo              := 126;   // current Tempo in BPM (Beats Per Minute)
  TimeInfo.barStartPos        := 0;   // last Bar Start Position, in Quarter Note
  TimeInfo.cycleStartPos      := 0;   // Cycle Start (left locator), in Quarter Note
  TimeInfo.cycleEndPos        := 0;   // Cycle End (right locator), in Quarter Note
  TimeInfo.timeSigNumerator   := 4;   // Time Signature Numerator (e.g. 3 for 3/4)
  TimeInfo.timeSigDenominator := 4;   // Time Signature Denominator (e.g. 4 for 3/4)
  TimeInfo.smpteOffset        := 0;   // SMPTE offset (in SMPTE subframes (bits; 1/80 of a frame)). The current SMPTE position can be calculated using samplePos, sampleRate, and smpteFrameRate.
  TimeInfo.smpteFrameRate     := 0;   // see VstSmpteFrameRate
  TimeInfo.samplesToNextClock := 0;   // MIDI Clock Resolution (24 Per Quarter Note), can be negative (nearest clock)
  TimeInfo.flags              := 0;   // see VstTimeInfoFlags

  ptr := @TimeInfo;
  Plugin.SetTimeInfoStruct(ptr);
end;

procedure TVst2PerformanceTest.RunTest;
var
  c1 : integer;
  Ins, Outs : TestHost.Vst2.DAEffect.PPSingle;
begin
  if Plugin.Dispatch(VamCustomEffect_TriggerVoices) <> 1 then
  begin
    WriteLn('VamCustomEffect_TriggerVoices opcode not supported.');
  end;

  Ins := @InputPointers[0];
  Outs := @OutputPointers[0];

  for c1 := 0 to 10000 do
  begin
    TimeInfo.samplePos := c1 * fBufferSize;
    Plugin.VstEffect.processReplacing(Plugin.VstEffect, Ins, Outs, fBufferSize);
  end;
end;



end.
