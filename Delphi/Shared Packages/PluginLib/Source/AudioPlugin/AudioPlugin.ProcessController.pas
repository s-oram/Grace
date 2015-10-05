unit AudioPlugin.ProcessController;

interface

uses
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamLib.MoreTypes,
  AudioPlugin,
  VamVst2.MidiEventInputBuffer;

type
  TProcessController = class;
  TProcessControllerClass = class of TProcessController;

  TProcessController = class
  private
    Plug : TAudioPlugin;
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
  public
    constructor Create(const aPlug : TAudioPlugin); virtual;
    destructor Destroy; override;

    procedure Suspend; virtual;
    procedure Resume(const aInputCount, aOutputCount, aFastUpdateBufferSize, aSlowUpdateBufferSize : integer); virtual;

    procedure ProcessEvents(ev: PVstEvents);
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: integer); virtual;
  end;

implementation

uses
  SysUtils,
  VamVst2.MidiEvent;

{ TProcessController }

constructor TProcessController.Create(const aPlug: TAudioPlugin);
begin
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

end;

procedure TProcessController.Resume(const aInputCount, aOutputCount, aFastUpdateBufferSize, aSlowUpdateBufferSize: integer);
begin
  if aSlowUpdateBufferSize < aFastUpdateBufferSize then raise Exception.Create('Slow update buffer size must be larger than the fast update buffersize.');

  if (aSlowUpdateBufferSize mod aFastUpdateBufferSize <> 0)
    then raise Exception.Create('Slow and Fast control rates are evenly divisable.');

  SetLength(InputPointers, aInputCount);
  SetLength(OutputPointers, aOutputCount);

  InputCount := aInputCount;
  OutputCount := aOutputCount;

  FastUpdateBufferSize := aFastUpdateBufferSize;
  SlowUpdateBufferSize := aSlowUpdateBufferSize;

  SlowControlRateStepMax   := SlowUpdateBufferSize div FastUpdateBufferSize;
  SlowControlRateStepCount := 0;

  ControlRateOffset := 0;

  if InputCount > 0
    then BufferedInputs := @InputPointers[0]
    else BufferedInputs := nil;

  if OutputCount > 0
    then BufferedOutputs := @OutputPointers[0]
    else BufferedOutputs := nil;
end;

procedure TProcessController.ProcessEvents(ev: PVstEvents);
begin
  MidiInput.AssignFrom(ev);
end;

procedure TProcessController.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: integer);
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


  NumEv := MidiInput.EventCount;
  CurEv := 0;
  SamplesProcessed := 0;

  if MidiInput.EventCount > 0 then NextMidiEvent := MidiInput.ReadMidiEvent(0);

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
      Plug.ProcessReplacing(BufferedInputs, BufferedOutputs, SamplesToProcess);
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
end;




end.
