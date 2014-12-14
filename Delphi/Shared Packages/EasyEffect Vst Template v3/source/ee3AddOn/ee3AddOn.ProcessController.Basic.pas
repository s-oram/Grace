unit ee3AddOn.ProcessController.Basic;

{
  The Process Controller

  === Usage Notes ===
  - There should only be one process controller used in a project.

}

interface

uses
  Classes,
  SysUtils,
  DAEffect,
  DAEffectX,
  DAudioEffectX,
  ee3AddOn.VstMidi,
  ee3.ProcessController;

type
  TProcessVstMidiEventProc   = reference to procedure(ev : PVstMidiEvent);
  TProcessReplacing32BitProc = reference to procedure(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
  TProcessReplacing64BitProc = reference to procedure(Inputs, Outputs: PPDouble; SampleFrames: VstInt32);

  TProcessController = class(TCustomProcessController)
  private
    fOnProcessReplacing32Bit: TProcessReplacing32BitProc;
    fOnProcessReplacing64Bit: TProcessReplacing64BitProc;
    fOnProcessVstMidiEvent: TProcessVstMidiEventProc;
    fOnPostProcess: TProc;
    fOnPreProcess: TProc;
    fOnFastControlProcess: TNotifyEvent;
    fOnSlowControlProcess: TNotifyEvent;
  protected
    FastControlBlockSize, SlowControlBlockSize : integer;
    SlowControlFactor : integer;
    FastControlSampleOffset : integer;
    SlowControlCount : integer;
    fNumInputs : integer;
    fNumOutputs : integer;
    MidiInputData  : TVstMidiList;
    MidiOutputData : TVstMidiList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup(aFastControlBlockSize, aSlowControlBlockSize : integer);

    procedure Suspend; override;
    procedure Resume; override;

    procedure ProcessVstEvents(Events: PVstEvents); override;

    procedure SetNumInputs(Inputs: VstInt32); override;   // Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available inputs.
    procedure SetNumOutputs(Outputs: VstInt32); override; // Set the number of outputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available ouputs.

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;       // Process 32 bit (single precision) floats (always in a resume state)
    procedure ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: VstInt32); override; // Process 64 bit (double precision) floats (always in a resume state)

    property OnPreProcess            : TProc                      read fOnPreProcess            write fOnPreProcess;
    property OnPostProcess           : TProc                      read fOnPostProcess           write fOnPostProcess;
    property OnFastControlProcess    : TNotifyEvent               read fOnFastControlProcess    write fOnFastControlProcess;
    property OnSlowControlProcess    : TNotifyEvent               read fOnSlowControlProcess    write fOnSlowControlProcess;
    property OnProcessReplacing32Bit : TProcessReplacing32BitProc read fOnProcessReplacing32Bit write fOnProcessReplacing32Bit;
    property OnProcessReplacing64Bit : TProcessReplacing64BitProc read fOnProcessReplacing64Bit write fOnProcessReplacing64Bit;
    property OnProcessVstMidiEvent   : TProcessVstMidiEventProc   read fOnProcessVstMidiEvent   write fOnProcessVstMidiEvent;
  end;

implementation

uses
  VamLib.Utils;

{ TProcessController }

constructor TProcessController.Create;
begin
  MidiInputData  := TVstMidiList.Create;
  MidiInputData.Capacity := 10;
  MidiOutputData := TVstMidiList.Create;

  Setup(16, 8192);
end;

destructor TProcessController.Destroy;
begin
  MidiInputData.Free;
  MidiOutputData.Free;
  inherited;
end;

procedure TProcessController.Setup(aFastControlBlockSize, aSlowControlBlockSize: integer);
begin
  assert(aSlowControlBlockSize > aFastControlBlockSize);
  assert(aSlowControlBlockSize mod aFastControlBlockSize = 0);

  FastControlBlockSize := aFastControlBlockSize;
  SlowControlBlockSize := aSlowControlBlockSize;
  SlowControlFactor := SlowControlBlockSize div FastControlBlockSize;
end;

procedure TProcessController.SetNumInputs(Inputs: VstInt32);
begin
  fNumInputs := Inputs;
end;

procedure TProcessController.SetNumOutputs(Outputs: VstInt32);
begin
  fNumOutputs := Outputs;
end;

procedure TProcessController.Suspend;
begin

end;

procedure TProcessController.Resume;
begin
  FastControlSampleOffset := 0;
  SlowControlCount := 0;
end;



procedure TProcessController.ProcessVstEvents(Events: PVstEvents);
var
  c1 : integer;
  MidiEv : PVstMidiEvent;
begin
  // Check if range checking is active. If it is we need to toggle it off and on for
  // following section of code.
  {$ifopt R+}
     {$define TOGGLE_RANGE_CHECK}
  {$endif}

  {$ifdef TOGGLE_RANGE_CHECK}
     {$R-}
  {$endif}

  for c1 := 0 to Events^.numEvents - 1 do
  begin
    if (Events^.events[c1]^.vtype = kVstMidiType) then
    begin
      //Cast VstEvent as VstMidiEvent
      MidiEv := PVstMidiEvent(Events^.events[c1]);
      MidiInputData.Add(MidiEv^);
    end;
  end;

  {$ifdef TOGGLE_RANGE_CHECK}
     {$R+}
     {$undef TOGGLE_RANGE_CHECK}
  {$endif}


  {
  TODO:MED
  - read MIDI events.
  - add pointers to individual MIDI events to list.
  - check events are sorted, sort if not.
  - send events to plugin when appropiate.
  - need a way to send input events back to host.
  - need a way to add events to the MIDI output.

  Requirements
  - MIDI event list needs to not request memory when processing. It should use a
    fixed sized MIDI event buffer.
  - Should be possible to sort, add, insert and delete events.
  }
end;


procedure TProcessController.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
var
  MidiEvent : PVstMidiEvent;
  MidiEventIndex : integer;
  MidiEventDelta : integer;
  SampleIndex : integer;
  SamplesToProcess : integer;
  ControlDelta : integer;
begin
  //==================================================================================
  //              Pre-process stuff
  //==================================================================================

  //==================================================================================

  SampleIndex    := 0;
  MidiEventIndex := 0;
  ControlDelta   := FastControlSampleOffset;

  if MidiInputData.Count = 0 then
  begin
    MidiEventDelta := SampleFrames;
    MidiEvent := nil;
  end else
  begin
    MidiEvent := @MidiInputData.Raw[MidiEventIndex];
    MidiEventDelta := MidiEvent^.deltaFrames;
  end;

  while SampleIndex < SampleFrames do
  begin
    //== process midi events ========
    while MidiEventDelta <= SampleIndex do
    begin
      fOnProcessVstMidiEvent(MidiEvent);
      inc(MidiEventIndex);
      if MidiEventIndex < MidiInputData.Count then
      begin
        MidiEvent := @MidiInputData.Raw[MidiEventIndex];
        MidiEventDelta := MidiEvent^.deltaFrames;
      end else
      begin
        MidiEventDelta := SampleFrames;
        Break;
      end;
    end;

    if ControlDelta <= SampleIndex then
    begin
      //=== Slow Control Processing ===
      if SlowControlCount <= 0 then
      begin
        OnSlowControlProcess(self);
        SlowControlCount := SlowControlFactor-1;
      end else
      begin
        dec(SlowControlCount);
      end;

      //=== Fast Control Processing ===
      OnFastControlProcess(self);
      ControlDelta := SampleIndex + FastControlBlockSize;
    end;


    //== calculate how many samples to process ==
    SamplesToProcess := SampleFrames-SampleIndex;
    if SamplesToProcess > MidiEventDelta then SamplesToProcess := MidiEventDelta;
    if SamplesToProcess > ControlDelta   then SamplesToProcess := ControlDelta;

    //== process the required number of samples ==
    fOnProcessReplacing32Bit(Inputs, Outputs, SamplesToProcess);
    inc(SampleIndex, SamplesToProcess);
    IncrementVstAudioBufferPointers(Inputs, fNumInputs, SamplesToProcess);
    IncrementVstAudioBufferPointers(Outputs, fNumOutputs, SamplesToProcess);
  end;

  FastControlSampleOffset := ControlDelta-SampleIndex;
  MidiInputData.Clear;


  //==================================================================================
  //              Post-process stuff
  //==================================================================================

  //==================================================================================
end;


procedure TProcessController.ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: VstInt32);
begin
  fOnProcessReplacing64Bit(Inputs, Outputs, SampleFrames);
end;



end.
