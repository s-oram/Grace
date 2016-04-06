unit AudioPlugin.Vst2Adapter;

interface

uses
  Windows,
  ExtCtrls,
  VamLib.MoreTypes,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.DAudioEffect,
  VamVst2.DAudioEffectX,
  VamVst2.MidiEventOutputBuffer,
  AudioPlugin.PlugMain,
  AudioPlugin.Globals,
  AudioPlugin.PlugEdit,
  AudioPlugin.Vst2PluginInfo,
  AudioPlugin.ProcessController;

type
  TVst2AdapterCreateInfo = record
    PlugClass         : TAudioPlugClass;
    EditorClass       : TAudioPlugEditorClass;
    GlobalsClass      : TGlobalsClass;
    ProcessController : TProcessControllerClass;
  end;

  //==== The VST Audio Plugin ====

  TVst2Adapter = class(AudioEffectX)
  private
  protected
    Globals : TGlobals;
    Plug : TAudioPlug;
    ProcessController : TProcessController;
    MidiOutputBuffer : TMidiEventOutputBuffer;

    // These are setup methods and should only be called from the constructor.
    procedure SetNumPrograms(const Value : integer);
    procedure SetNumParameters(const Value : integer);
  public
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2AdapterCreateInfo); reintroduce; virtual;
    destructor Destroy; override;

    function Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; override;

    function CanDo(Text: PAnsiChar): VstInt32; override;

    // State Transitions
    procedure Open; override;      // Called when plug-in is initialized
    procedure Close; override;     // Called when plug-in will be released
    procedure Suspend; override;   // Called when plug-in is switched to off
    procedure Resume; override;    // Called when plug-in is switched to on

    // Parameters
    procedure SetParameter(index: Longint; value: Single); override;
    function GetParameter(index: Longint): Single; override;

    procedure GetParameterLabel(index: Longint; aLabel: PAnsiChar); override;
    procedure GetParameterDisplay(index: Longint; text: PAnsiChar); override;
    procedure GetParameterName(index: Longint; text: PAnsiChar); override;

    function  ProcessEvents(ev: PVstEvents): longint; override;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;
  end;


  //==== The VST Editor ====

  TVst2EditAdapter = class(AEffEditor)
  private
    FGlobals: TGlobals;
  protected
    CurrentGuiSize : record
      OriginalRatio : double; // Original Width/Height
      Width  : integer;
      Height : integer;
    end;

    Effect : AudioEffect;
    Editor      : TAudioPlugEditor;
    EditorClass : TAudioPlugEditorClass;
    UseCount : integer;
    GuiRect : ERect;
    WindowHandle : HWND;
    AirControlTimer : TTimer;

    property Globals : TGlobals read FGlobals;

    procedure ResizeGui(const OffsetX, OffsetY : integer);

    procedure HandleAirControlTimerEvent(Sender : TObject);
  public
    constructor Create(aEffect: AudioEffect; const aPlug : TAudioPlug; const aEditorClass : TAudioPlugEditorClass; const aGlobals : TGlobals); reintroduce;
    destructor Destroy; override;

    function GetRect(var rect: PERect): longint; override;
    function Open(ptr: pointer): longint; override;
    procedure Close; override;
    function IsOpen: boolean; override;
    procedure Idle; override;

    function OnKeyDown(var KeyCode: VstKeyCode): boolean; override;
    function OnKeyUp(var KeyCode: VstKeyCode): boolean; override;
    function SetKnobMode(Val: VstInt32): boolean; override;
    function OnWheel(Distance: single): boolean; override;
  end;

implementation

uses
  MadExcept,
  SysUtils,
  AudioPlugin.Functions,
  AudioPlugin.RunTimeInfo;



const
  kUsingGui = true;

{ TVst2PlugAdapter }

constructor TVst2Adapter.Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2AdapterCreateInfo);
begin
  inherited Create(anAudioMaster, 0, 0);

  MidiOutputBuffer := TMidiEventOutputBuffer.Create(8);

  Globals := CreateInfo.GlobalsClass.Create;

  //== Vst2 Methods ==
  //Globals.Vst2.GetParameter := self.GetParameter;
  //Globals.Vst2.SetParameter := self.SetParameter;
  //Globals.Vst2.SetParameterAutomated := self.SetParameterAutomated;
  //Globals.Vst2.BeginEdit := self.BeginEdit;
  //Globals.Vst2.EndEdit := self.EndEdit;
  //Globals.Vst2.IncrementMidiEventDelta := MidiOutputBuffer.IncrementGlobalDelta;
  //Globals.Vst2.SendMidiEvent := MidiOutputBuffer.AddMidiEvent;
  //Globals.Vst2.SendMidiEventWithDeltaOffset := MidiOutputBuffer.AddMidiEvent;

  Plug := CreateInfo.PlugClass.Create(Globals);



  ProcessController := CreateInfo.ProcessController.Create(Plug);

  if kUsingGui
    then self.Editor := TVst2EditAdapter.Create(self, Plug, CreateInfo.EditorClass, Globals)
    else self.Editor := nil;


  Globals.LinkPlugMain(Plug);
  Globals.LinkVst2Adapter(self);

end;

destructor TVst2Adapter.Destroy;
begin
  // Ordering is important.

  // 1)
  Globals.UnlinkPlugMain;
  Globals.UnlinkVst2Adapter;

  // 2)
  if assigned(self.Editor) then
  begin
    Editor.Free;
    Editor := nil;
  end;

  // 3)
  Plug.Free;
  ProcessController.Free;
  Globals.Free;
  MidiOutputBuffer.Free;

  inherited;
end;

function TVst2Adapter.Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr;
begin
  try
    inherited;
  except
    HandleException;
  end;
end;

procedure TVst2Adapter.Open;
begin
  inherited;
  Plug.Open;
end;

function TVst2Adapter.CanDo(Text: PAnsiChar): VstInt32;
begin
  if SameText(Text, canDoReceiveVstEvents)    then exit(1);
  if SameText(Text, canDoReceiveVstMidiEvent) then exit(1);

  if SameText(Text, canDoSendVstEvents)    then exit(1);
  if SameText(Text, canDoSendVstMidiEvent) then exit(1);

  if SameText(Text, canDoReceiveVstTimeInfo) then exit(1);
  if SameText(Text, canDoOffline) then exit(1);

  result := inherited;
end;

procedure TVst2Adapter.Close;
begin
  inherited;
  Plug.Close;
end;

procedure TVst2Adapter.Suspend;
begin
  inherited;
  ProcessController.Suspend;
end;

procedure TVst2Adapter.Resume;
var
  rtInfo : TRunTimeInfo;
  InputLat, OutputLat : integer;
begin
  inherited;

  InputLat := GetInputLatency;
  OutputLat := GetOutputLatency;

  rtInfo.InputCount      := 2;
  rtInfo.OutputCount     := 2;
  rtInfo.SampleRate      := round(self.SampleRate);
  rtInfo.MaxSampleFrames := self.BlockSize;
  rtInfo.FastControlBufferSize := 4410;
  rtInfo.SlowControlBufferSize := 44100;


  ProcessController.Resume(rtInfo);
end;

procedure TVst2Adapter.SetNumParameters(const Value: integer);
begin
  NumParams := Value;
  FEffect.numParams := Value;
end;

procedure TVst2Adapter.SetNumPrograms(const Value: integer);
begin
  NumPrograms := Value;
  FEffect.numParams := Value;
end;

procedure TVst2Adapter.SetParameter(index: Integer; value: Single);
begin
  // TODO:HIGH
end;

function TVst2Adapter.GetParameter(index: Integer): Single;
begin
  // TODO:HIGH
  result := 0;
end;

procedure TVst2Adapter.GetParameterDisplay(index: Integer; text: PAnsiChar);
begin
  //StrPCopy(text, AnsiString(PlugInfo.GetParameterDisplay(Index)));
end;

procedure TVst2Adapter.GetParameterLabel(index: Integer; aLabel: PAnsiChar);
begin
  //StrPCopy(aLabel, AnsiString(PlugInfo.GetParameterLabel(Index)));
end;

procedure TVst2Adapter.GetParameterName(index: Integer; text: PAnsiChar);
begin
  //StrPCopy(text, AnsiString(PlugInfo.GetParameterName(Index)));
end;

function TVst2Adapter.ProcessEvents(ev: PVstEvents): longint;
begin
  ProcessController.ProcessVstEvents(ev);
  //TODO:HIGH what should the return value be?
end;

procedure TVst2Adapter.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
var
  vevs : PVstEvents;
begin
  MidiOutputBuffer.ResetGlobalDelta;

  Plug.ProcessVstTimeInfo(GetTimeInfo(32767));

  Globals.AirControl.ProcessAudioSync;

  ProcessController.ProcessAudio(VamLib.MoreTypes.PPSingle(Inputs), VamLib.MoreTypes.PPSingle(Outputs), SampleFrames);

  vevs := MidiOutputBuffer.PrepareOutputBuffer(SampleFrames);
  if vevs.numEvents > 0
    then SendVstEventsToHost(vevs);
  MidiOutputBuffer.RemoveStaleEvents(SampleFrames);


end;



{ TVst2EditAdapter }

constructor TVst2EditAdapter.Create(aEffect: AudioEffect; const aPlug: TAudioPlug; const aEditorClass : TAudioPlugEditorClass; const aGlobals : TGlobals);
var
  r : TRect;
begin
  inherited Create(aEffect);

  AirControlTimer := TTimer.Create(nil);
  AirControlTimer.Enabled := false;
  AirControlTimer.Interval := 20;
  AirControlTimer.OnTimer := self.HandleAirControlTimerEvent;

  FGlobals := aGlobals;

  Effect := aEffect;

  UseCount := 0;
  EditorClass := aEditorClass;

  r := EditorClass.GetInitialGuiSize;
  CurrentGuiSize.Width  := r.Width;
  CurrentGuiSize.Height := r.Height;
  CurrentGuiSize.OriginalRatio := r.Width / r.Height;
end;

destructor TVst2EditAdapter.Destroy;
begin
  //Globals.Vst2.ResizeGui := nil;
  AirControlTimer.Free;
  inherited;
end;

function TVst2EditAdapter.GetRect(var rect: PERect): longint;
begin
  GuiRect.top  := 0;
  GuiRect.left := 0;
  GuiRect.right  := CurrentGuiSize.Width;
  GuiRect.bottom := CurrentGuiSize.Height;

  rect := @GuiRect;
  result := 1;
end;

procedure TVst2EditAdapter.HandleAirControlTimerEvent(Sender: TObject);
begin
  assert(assigned(Globals));
  Globals.AirControl.ProcessGuiSync;
end;

procedure TVst2EditAdapter.ResizeGui(const OffsetX, OffsetY: integer);
var
  NewSize : TSize;
  MinGuiSize : TSize;
  ResizeResult : boolean;
begin
  MinGuiSize := EditorClass.GetMinGuiSize;

  NewSize := CalcNewEditorSize(CurrentGuiSize.OriginalRatio, CurrentGuiSize.Width, CurrentGuiSize.Height, OffsetX, OffsetY);

  if NewSize.cx < MinGuiSize.cx
    then NewSize := MinGuiSize;

  ResizeResult := (Effect as AudioEffectX).SizeWindow(NewSize.cx, NewSize.cy);
  if (ResizeResult = true) then
  begin
    CurrentGuiSize.Width  := NewSize.Width;
    CurrentGuiSize.Height := NewSize.Height;
  end else
  begin
    // TODO:MED
    // try to implement GUI sizing by directly manipulating the window handle.
  end;



end;

procedure TVst2EditAdapter.Idle;
begin
  inherited;

end;

function TVst2EditAdapter.IsOpen: boolean;
begin
  if UseCount > 0
    then result := true
    else result := false;
end;

function TVst2EditAdapter.Open(ptr: pointer): longint;
begin
  inherited;

  inc(UseCount);
  if UseCount = 1 then
  begin
    WindowHandle := hwnd(ptr);

    // IMPORTANT: Start the timer before opening the GUI.

    // 1)
    Editor := EditorClass.Create(Globals);
    Editor.Open(WindowHandle, CurrentGuiSize.Width, CurrentGuiSize.Height);

    // 2)
    AirControlTimer.Enabled := true;
  end;
  result := 1;
end;

procedure TVst2EditAdapter.Close;
begin
  inherited;

  dec(UseCount);
  if UseCount = 0 then
  begin
    if assigned(Editor) then
    begin
      // IMPORTANT: Stop the timer before closing the GUI.

      // 1)
      AirControlTimer.Enabled := false;

      // 2)
      Editor.Close;
      FreeAndNil(Editor);
    end;
  end;
end;



function TVst2EditAdapter.OnKeyDown(var KeyCode: VstKeyCode): boolean;
begin

end;

function TVst2EditAdapter.OnKeyUp(var KeyCode: VstKeyCode): boolean;
begin

end;

function TVst2EditAdapter.OnWheel(Distance: single): boolean;
begin

end;



function TVst2EditAdapter.SetKnobMode(Val: VstInt32): boolean;
begin

end;



end.
