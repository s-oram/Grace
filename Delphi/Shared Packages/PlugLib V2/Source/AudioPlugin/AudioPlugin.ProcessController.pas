unit AudioPlugin.ProcessController;

interface

uses
  VamLib.Types,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.MidiEvent,
  VamVst2.MidiEventInputBuffer,
  VamVst2.MidiEventOutputBuffer,
  PlugLib.AirControl,
  AudioPlugin.Interfaces,
  AudioPlugin.PlugMain;


type
  TAbstractProcessController = class(TPureInterfacedObject)
  private
  public
    constructor Create(const AirControl : TAirControl; const Vst2 : IVst2AudioEffectX; const PlugMain : TAudioPlug); virtual; abstract;

    procedure Suspend; virtual; abstract;   // Called when plug-in is switched to off
    procedure Resume; virtual; abstract;    // Called when plug-in is switched to on

    procedure ProcessEvents(ev: PVstEvents); virtual; abstract;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); virtual; abstract;
  end;

  TProcessController = class(TAbstractProcessController);
  TProcessControllerClass = class of TProcessController;

  // Control Rate Process Controller - Processes audio with fast and slow control rate steps.
  TCRProcessController = class(TProcessController, IMidiOut)
  private
    FPlugMain: TAudioPlug;

    FMidiInput : TMidiEventInputBuffer;
    FMidiOutput: TMidiEventOutputBuffer;
    FSlowControlBufferSize: integer;
    FFastControlBufferSize: integer;

    NextControlStep : integer;

    // Track the fast control step count. A slow control step will
    // be triggered every X fast steps.
    fcsTarget : integer; // FastControlStepTarget.
    fcsCount  : integer;
    FVst2: IVst2AudioEffectX;
    FAirControl: TAirControl; // FastControlStepCount.
  protected
    property PlugMain   : TAudioPlug        read FPlugMain;
    property Vst2       : IVst2AudioEffectX read FVst2;
    property AirControl : TAirControl       read FAirControl;
  public
    constructor Create(const AirControl : TAirControl; const Vst2 : IVst2AudioEffectX; const PlugMain : TAudioPlug); override;
    destructor Destroy; override;

    procedure Suspend; override;   // Called when plug-in is switched to off
    procedure Resume; override;    // Called when plug-in is switched to on

    procedure ProcessEvents(ev: PVstEvents); override;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;

    procedure SetControlRate(const FastControlBufferSize, SlowControlBufferSize: integer);
    property FastControlBufferSize : integer read FFastControlBufferSize;
    property SlowControlBufferSize : integer read FSlowControlBufferSize;

    // Realtime Process
    property MidiInput  : TMidiEventInputBuffer  read FMidiInput;
    property MidiOutput : TMidiEventOutputBuffer read FMidiOutput implements IMidiOut;
  end;

implementation

{ TCRProcessController }

constructor TCRProcessController.Create(const AirControl : TAirControl; const Vst2 : IVst2AudioEffectX; const PlugMain: TAudioPlug);
begin
  inherited;

  FAirControl := AirControl;
  FVst2       := Vst2;
  FPlugMain   := PlugMain;

  // Set to default values.
  SetControlRate(64, 1024);

  FMidiInput := TMidiEventInputBuffer.Create(32);
  FMidiOutput := TMidiEventOutputBuffer.Create(32);
end;

destructor TCRProcessController.Destroy;
begin
  FVst2       := nil;
  FAirControl := nil;
  FPlugMain   := nil;

  MidiInput.Free;
  MidiOutput.Free;

  inherited;
end;

procedure TCRProcessController.SetControlRate(const FastControlBufferSize, SlowControlBufferSize: integer);
begin
  // 1) SlowControlBufferSize must be larger than FastControlBufferSize.
  assert( SlowControlBufferSize > FastControlBufferSize );

  // 2) SlowControlBufferSize must be a multiple of FastControlBufferSize.
  assert( SlowControlBufferSize mod FastControlBufferSize = 0 );

  FFastControlBufferSize := FastControlBufferSize;
  FSlowControlBufferSize := SlowControlBufferSize;
end;

procedure TCRProcessController.Suspend;
begin
  PlugMain.Suspend;
end;

procedure TCRProcessController.Resume;
var
  rtConfig : TRunTimeConfig;
begin
  inherited;

  rtConfig.InputCount  := Vst2.GetNumInputs;
  rtConfig.OutputCount := Vst2.GetNumOutputs;
  rtConfig.SampleRate  := round(Vst2.GetSampleRate);
  rtConfig.BlockSize   := Vst2.GetBlockSize;

  rtConfig.SlowControlStepBufferSize := self.SlowControlBufferSize;
  rtConfig.FastControlStepBufferSize := self.FastControlBufferSize;

  PlugMain.Resume(rtConfig);

  fcsTarget := SlowControlBufferSize div FastControlBufferSize;
  fcsCount  := 0;

  NextControlStep := 0;
end;

procedure TCRProcessController.ProcessEvents(ev: PVstEvents);
begin
  if ev^.numEvents > MidiInput.Capacity then MidiInput.Capacity := ev^.numEvents;
  MidiInput.AssignFrom(ev);
end;

procedure TCRProcessController.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
var
  vevs             : PVstEvents;
  NumEv, CurEv     : integer;
  NextMidiEvent    : TMidiEvent;
  SamplesProcessed : integer;
  SamplesToProcess : integer;
  NextEventPoint   : integer;
begin
  assert(SampleFrames <= Vst2.GetBlockSize);

  MidiOutput.ResetGlobalDelta;

  // 1) Get the time info.
  // 2) process block start
  PlugMain.ProcessVstBlockStart(Vst2.GetTimeInfo(32767), Inputs, Outputs);

  // 3) process air control
  AirControl.ProcessAudioSync;

  // 4) Run the audio process loop.
  NumEv := MidiInput.Count;
  CurEv := 0;
  SamplesProcessed := 0;

  if (CurEv < NumEv) then NextMidiEvent := MidiInput.ReadMidiEvent(CurEv);

  while SamplesProcessed < SampleFrames do
  begin
    // Check for control step process..
    if SamplesProcessed >= NextControlStep then
    begin
      inc(NextControlStep, FastControlBufferSize);

      if fcsCount = 0
        then PlugMain.ProcessControlStepSlow
        else PlugMain.ProcessControlStepFast;

      if fcsCount < fcsTarget
        then inc(fcsCount)
        else fcsCount := 0;
    end;

    // Check for the next MIDI event.
    if (CurEv < NumEv) and (SamplesProcessed >= NextMidiEvent.Deltaframes) then
    begin
      PlugMain.ProcessMidiEvent(@NextMidiEvent);
      Inc(CurEv);
      if (CurEv < NumEv) then NextMidiEvent := MidiInput.ReadMidiEvent(CurEv);
    end;

    // Calculate how many samples to process.
    if NextControlStep < SampleFrames
      then NextEventPoint := NextControlStep
      else NextEventPoint := NextControlStep;

    if (CurEv < NumEv) and (NextMidiEvent.Deltaframes < NextEventPoint)
      then NextEventPoint := NextMidiEvent.Deltaframes;

    SamplesToProcess := NextEventPoint - SamplesProcessed;

    if SamplesToProcess > 0 then
    begin
      PlugMain.ProcessAudio(SamplesToProcess);

      inc(SamplesProcessed, SamplesToProcess);
      MidiOutput.IncrementGlobalDelta(SamplesToProcess);
    end
  end;

  dec(NextControlStep, SampleFrames);
  assert(SamplesProcessed = SampleFrames);

  // 5) Clear the MIDI input buffer.
  if MidiInput.Count > 0
    then MidiInput.Clear;

  // 6) Send MIDI to the output buffer.
  vevs := MidiOutput.PrepareOutputBuffer(SamplesProcessed);
  if (vevs.numEvents > 0)
    then Vst2.SendVstEventsToHost(vevs);
  MidiOutput.RemoveStaleEvents(SamplesProcessed);
end;


end.
