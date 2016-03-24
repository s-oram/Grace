unit AudioPlugin.ProcessController;

interface

uses
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamLib.MoreTypes,
  AudioPlugin.PlugMain,
  AudioPlugin.RunTimeInfo,
  VamVst2.MidiEventInputBuffer;

type
  TProcessController = class;
  TProcessControllerClass = class of TProcessController;

  TProcessController = class
  private
    Plug : TAudioPlug;
    InputPointers  : array of PSingle;
    OutputPointers : array of PSingle;
    MidiInput : TMidiEventInputBuffer;
    InputCount, OutputCount : integer;
    ProcessReplacingOffset : integer;
    ControlRateOffset : integer;
    FastUpdateBufferSize, SlowUpdateBufferSize : integer;
    SlowControlRateStepMax   : integer;
    SlowControlRateStepCount : integer;
    BufferedInputs  : PPSingle;
    BufferedOutputs : PPSingle;
    FRunTimeInfo : TRunTimeInfo;
  public
    constructor Create(const aPlug : TAudioPlug); virtual;
    destructor Destroy; override;

    procedure Suspend; virtual;
    procedure Resume(const RunTimeInfo : TRunTimeInfo); virtual;

    procedure ProcessVstEvents(ev: PVstEvents);
    procedure ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer); virtual;
  end;

implementation

uses
  SysUtils,
  VamVst2.MidiEvent;

{ TProcessController }

constructor TProcessController.Create(const aPlug: TAudioPlug);
begin
  FRunTimeInfo.Init();

  Plug := aPlug;
  MidiInput := TMidiEventInputBuffer.Create(32);
  InputCount := 0;
  OutputCount := 0;
end;

destructor TProcessController.Destroy;
begin
  SetLength(InputPointers, 0);
  SetLength(OutputPointers, 0);
  MidiInput.Free;
  inherited;
end;

procedure TProcessController.Suspend;
begin
  Plug.Suspend;
end;

procedure TProcessController.Resume(const RunTimeInfo : TRunTimeInfo);
begin
  if RunTimeInfo.SlowControlBufferSize < RunTimeInfo.FastControlBufferSize then raise Exception.Create('Slow update buffer size must be larger than the fast update buffersize.');

  if (RunTimeInfo.SlowControlBufferSize mod RunTimeInfo.FastControlBufferSize <> 0) then raise Exception.Create('Slow and Fast control rates are evenly divisable.');

  FRunTimeInfo := RunTimeInfo;

  SetLength(InputPointers, RunTimeInfo.InputCount);
  SetLength(OutputPointers, RunTimeInfo.OutputCount);

  InputCount  := RunTimeInfo.InputCount;
  OutputCount := RunTimeInfo.OutputCount;

  FastUpdateBufferSize := RunTimeInfo.FastControlBufferSize;
  SlowUpdateBufferSize := RunTimeInfo.SlowControlBufferSize;

  SlowControlRateStepMax   := SlowUpdateBufferSize div FastUpdateBufferSize;
  SlowControlRateStepCount := 0;

  ControlRateOffset := 0;

  if InputCount > 0
    then BufferedInputs := @InputPointers[0]
    else BufferedInputs := nil;

  if OutputCount > 0
    then BufferedOutputs := @OutputPointers[0]
    else BufferedOutputs := nil;

  Plug.Resume(RunTimeInfo);
end;

procedure TProcessController.ProcessVstEvents(ev: PVstEvents);
begin
  if ev^.numEvents > MidiInput.Capacity then MidiInput.Capacity := ev^.numEvents;
  MidiInput.AssignFrom(ev);

  // NOTE: MIDI events with delta frames greater than the next processing block size will
  // be dropped silently. Perhaps it would be better to raise an exception in debug mode.
end;

procedure TProcessController.ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer);
var
  c1: Integer;
  NumEv, CurEv:integer;
  NextMidiEvent         : TMidiEvent;
  NextMidiEventDelta    : integer;
  NextControlRateDelta  : integer;
  NextBufferEndDelta    : integer;
  SamplesProcessed      : integer;
  SamplesToProcess      :integer;
begin
  assert(FRunTimeInfo.MaxSampleFrames >= SampleFrames);

  for c1 := 0 to InputCount-1 do
  begin
    InputPointers[c1] := Inputs^;
    inc(Inputs);
  end;

  for c1 := 0 to OutputCount-1 do
  begin
    OutputPointers[c1] := Outputs^;
    inc(Outputs);
  end;

  NumEv := MidiInput.Count;
  CurEv := 0;
  SamplesProcessed := 0;

  if MidiInput.Count > 0 then NextMidiEvent := MidiInput.ReadMidiEvent(0);

  while SampleFrames > 0 do
  begin
    ProcessReplacingOffset := SamplesProcessed;

    //Get the next delta values and calucate how many samples to process.
    NextBufferEndDelta   := SampleFrames;
    NextControlRateDelta := FastUpdateBufferSize - ControlRateOffset;

    if NextBufferEndDelta < NextControlRateDelta
      then SamplesToProcess := NextBufferEndDelta
      else SamplesToProcess := NextControlRateDelta;

    if CurEv < NumEv then
    begin
      NextMidiEventDelta := NextMidiEvent.Deltaframes - SamplesProcessed;
      if NextMidiEventDelta < SamplesToProcess then SamplesToProcess := NextMidiEventDelta;
    end else
    begin
      NextMidiEventDelta := SampleFrames+1;
    end;

    if SamplesToProcess > 0 then
    begin
      //Process those samples..
      //Plugin.AudioProcess(SamplesToProcess);
      Plug.ProcessAudio(BufferedInputs, BufferedOutputs, SamplesToProcess);
      for c1 := 0 to InputCount-1 do
      begin
        inc(InputPointers[c1], SamplesToProcess);
      end;

      for c1 := 0 to OutputCount-1 do
      begin
        inc(OutputPointers[c1], SamplesToProcess);
      end;

      inc(SamplesProcessed, SamplesToProcess);
      dec(SampleFrames, SamplesToProcess);
      inc(ControlRateOffset, SamplesToProcess);
    end else
    begin
      // or process whatever events...
      if NextMidiEventDelta = 0 then
      begin
        Plug.ProcessMidiEvent(@NextMidiEvent);
        Inc(CurEv);
        if (CurEv < NumEv) then NextMidiEvent := MidiInput.ReadMidiEvent(CurEv);
      end;

      if NextControlRateDelta = 0 then
      begin
        Plug.ProcessControlStepFast(FastUpdateBufferSize);
        ControlRateOffset := 0;

        inc(self.SlowControlRateStepCount);
        if SlowControlRateStepCount >= SlowControlRateStepMax then
        begin
          SlowControlRateStepCount := 0;
          Plug.ProcessControlStepSlow(SlowUpdateBufferSize);
        end;
      end;
    end;
  end;

  if MidiInput.Count > 0
    then MidiInput.Clear;
end;




end.
