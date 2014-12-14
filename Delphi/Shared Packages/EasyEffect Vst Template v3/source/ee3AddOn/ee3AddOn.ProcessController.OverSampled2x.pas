unit ee3AddOn.ProcessController.OverSampled2x;

{
  The Process Controller

  === Usage Notes ===
  - There should only be one process controller used in a project.

}

interface

uses
  Classes,
  SysUtils,
  VamLib.Types,
  r8Brain,
  DAEffect,
  DAEffectX,
  DAudioEffectX,
  ee3AddOn.VstMidi,
  ee3AddOn.ProcessController.MultiChannelResampler.r8brain,
  ee3.ProcessController;

type
  PProcessControllerConfig = ^TProcessControllerConfig;
  TProcessControllerConfig = record
    MaxBufferSize : integer;
    OverSampleFactor : integer;
    FastControlBlockSize : integer; //
    SlowControlBlockSize : integer; // must be a multiple of FastControlBlockSize.
    procedure AssignFrom(const Source : PProcessControllerConfig);
  end;

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
    SlowControlFactor : integer;
    FastControlSampleOffset : integer;
    SlowControlCount : integer;
    fNumInputs : integer;
    fNumOutputs : integer;
    MidiInputData  : TVstMidiList;
    MidiOutputData : TVstMidiList;
    AudioInputResamplers  : TMultiChannelResampler;
    AudioOutputResamplers : TMultiChannelResampler;

    TempOutputsPointerX32 : PPSingle;
    TempOutputsPointerX64 : PPDouble;
    TempOutputsBufferPointersX32 : array of PSingle;
    TempOutputsBufferPointersX64 : array of PDouble;
    TempOutputsBufferX32 : array of array of single;
    TempOutputsBufferX64 : array of array of double;

    Config : TProcessControllerConfig;
    procedure ConfigChanged;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup(aConfig : PProcessControllerConfig); overload;

    procedure Suspend; override;
    procedure Resume; override;

    procedure ProcessVstEvents(Events: PVstEvents); override;

    procedure SetNumInputs(Inputs: VstInt32); override;  // Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available inputs.
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
  //set some default internal values
  fNumInputs  := 0;
  fNumOutputs := 0;
  Config.MaxBufferSize    := 1024;
  Config.OverSampleFactor := 2;
  Config.FastControlBlockSize := 16;
  Config.SlowControlBlockSize := 16 * 512; //=8192

  MidiInputData  := TVstMidiList.Create;
  MidiInputData.Capacity := 10;
  MidiOutputData := TVstMidiList.Create;

  AudioInputResamplers  := TMultiChannelResampler.Create;
  AudioOutputResamplers := TMultiChannelResampler.Create;

  //finally call config changed to ensure everything is initialized.
  ConfigChanged;
end;

destructor TProcessController.Destroy;
begin
  MidiInputData.Free;
  MidiOutputData.Free;

  AudioInputResamplers.Free;
  AudioOutputResamplers.Free;

  SetLength(TempOutputsBufferPointersX32, 0);
  SetLength(TempOutputsBufferPointersX64, 0);
  SetLength(TempOutputsBufferX32, 0, 0);
  SetLength(TempOutputsBufferX64, 0, 0);

  inherited;
end;

procedure TProcessController.Setup(aConfig: PProcessControllerConfig);
begin
  Config.AssignFrom(aConfig);
  ConfigChanged;
end;

procedure TProcessController.ConfigChanged;
var
  rc : TResampleConfig;
  OverSampledBufferSize : integer;
begin
  assert(Config.SlowControlBlockSize > Config.FastControlBlockSize);
  assert(Config.SlowControlBlockSize mod Config.FastControlBlockSize = 0);

  SlowControlFactor := Config.SlowControlBlockSize div Config.FastControlBlockSize;

  OverSampledBufferSize := Config.MaxBufferSize * Config.OverSampleFactor;

  rc.SourceRate           := 1;
  rc.DestRate             := Config.OverSampleFactor;
  rc.MaxInputBufferFrames := Config.MaxBufferSize;
  rc.TransitionBand       := 6;
  rc.Res                  := 0;
  AudioInputResamplers.Init(@rc);

  rc.SourceRate           := Config.OverSampleFactor;
  rc.DestRate             := 1;
  rc.MaxInputBufferFrames := OverSampledBufferSize;
  rc.TransitionBand       := 6;
  rc.Res                  := 0;
  AudioOutputResamplers.Init(@rc);

  //====== setup output buffers =================
  SetLength(TempOutputsBufferPointersX32, fNumOutputs);
  SetLength(TempOutputsBufferPointersX64, fNumOutputs);
  SetLength(TempOutputsBufferX32, fNumOutputs, OverSampledBufferSize);
  SetLength(TempOutputsBufferX64, fNumOutputs, OverSampledBufferSize);
  //==============================================
end;

procedure TProcessController.SetNumInputs(Inputs: VstInt32);
begin
  fNumInputs := Inputs;
  AudioInputResamplers.SetChannelCount(Inputs);
end;

procedure TProcessController.SetNumOutputs(Outputs: VstInt32);
begin
  fNumOutputs := Outputs;
  AudioOutputResamplers.SetChannelCount(Outputs);
  ConfigChanged;
end;

procedure TProcessController.Suspend;
begin

end;

procedure TProcessController.Resume;
begin
  FastControlSampleOffset := 0;
  SlowControlCount := 0;

  AudioInputResamplers.Clear;
  AudioOutputResamplers.Clear;
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
  UpSampledInputs : PPSingle;
  UpSampleFrames : integer;
  DownSampledOutputs : PPSingle;
  DownSampleFrames : integer;
  c1: Integer;
begin
  //==================================================================================
  //              Pre-process stuff
  //==================================================================================

  // Upsample the input.
  AudioInputResamplers.ProcessFloat32(Inputs, SampleFrames, UpSampledInputs, UpSampleFrames);
  assert(UpSampleFrames = SampleFrames * Config.OverSampleFactor);

  // build the temp output buffers
  for c1 := 0 to fNumOutputs-1 do
  begin
    TempOutputsBufferPointersX32[c1] := @TempOutputsBufferX32[c1, 0];
  end;
  TempOutputsPointerX32 := @TempOutputsBufferPointersX32[0];


  //==================================================================================

  SampleIndex    := 0;
  MidiEventIndex := 0;
  ControlDelta   := FastControlSampleOffset;

  if MidiInputData.Count = 0 then
  begin
    MidiEventDelta := UpSampleFrames;
    MidiEvent := nil;
  end else
  begin
    MidiEvent := @MidiInputData.Raw[MidiEventIndex];
    MidiEventDelta := MidiEvent^.deltaFrames * Config.OverSampleFactor;
  end;

  while SampleIndex < UpSampleFrames do
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
        MidiEventDelta := UpSampleFrames;
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
      ControlDelta := SampleIndex + Config.FastControlBlockSize * Config.OverSampleFactor;
    end;

    //== calculate how many samples to process ==
    SamplesToProcess := UpSampleFrames-SampleIndex;
    if SamplesToProcess > MidiEventDelta then SamplesToProcess := MidiEventDelta;
    if SamplesToProcess > ControlDelta   then SamplesToProcess := ControlDelta;

    //== process the required number of samples ==
    fOnProcessReplacing32Bit(UpSampledInputs, TempOutputsPointerX32, SamplesToProcess);
    inc(SampleIndex, SamplesToProcess);
    IncrementVstAudioBufferPointers(UpSampledInputs, fNumInputs, SamplesToProcess);
    IncrementVstAudioBufferPointers(TempOutputsPointerX32, fNumOutputs, SamplesToProcess);
  end;

  FastControlSampleOffset := ControlDelta-SampleIndex;
  MidiInputData.Clear;

  //==================================================================================
  //              Post-process stuff
  //==================================================================================

  // build the temp output buffers
  for c1 := 0 to fNumOutputs-1 do
  begin
    TempOutputsBufferPointersX32[c1] := @TempOutputsBufferX32[c1, 0];
  end;
  TempOutputsPointerX32 := @TempOutputsBufferPointersX32[0];

  // downsample
  AudioOutputResamplers.ProcessFloat32(TempOutputsPointerX32, UpSampleFrames, DownSampledOutputs, DownSampleFrames);
  assert(DownSampleFrames = SampleFrames);

  CopyVstAudioBuffers(DownSampledOutputs, Outputs, fNumOutputs, SampleFrames);

  //==================================================================================
end;


procedure TProcessController.ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: VstInt32);
begin
  fOnProcessReplacing64Bit(Inputs, Outputs, SampleFrames);
end;





{ TProcessControllerConfig }

procedure TProcessControllerConfig.AssignFrom(const Source: PProcessControllerConfig);
begin
  self.MaxBufferSize        := Source^.MaxBufferSize;
  self.OverSampleFactor     := Source^.OverSampleFactor;
  self.FastControlBlockSize := Source^.FastControlBlockSize;
  self.SlowControlBlockSize := Source^.SlowControlBlockSize;
end;

end.
