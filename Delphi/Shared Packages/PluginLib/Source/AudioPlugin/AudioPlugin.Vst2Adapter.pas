unit AudioPlugin.Vst2Adapter;

interface

uses
  Windows,
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
    PlugInfo          : TVst2PluginInfoClass;
    ProcessController : TProcessControllerClass;
  end;

  //==== The VST Audio Plugin ====

  TVst2PlugAdapter = class(AudioEffectX)
  private
  protected
    Globals : TGlobals;
    Plug : TAudioPlug;
    PlugInfo : TVst2PluginInfo;
    ProcessController : TProcessController;
    MidiOutputBuffer : TMidiEventOutputBuffer;
  public
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2AdapterCreateInfo); reintroduce;
    destructor Destroy; override;

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

    property Globals : TGlobals read FGlobals;

    procedure ResizeGui(const OffsetX, OffsetY : integer);
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
  SysUtils,
  AudioPlugin.Functions,
  AudioPlugin.Events,
  AudioPlugin.RunTimeInfo;



const
  kUsingGui = true;

{ TVst2PlugAdapter }

constructor TVst2PlugAdapter.Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2AdapterCreateInfo);
begin
  MidiOutputBuffer := TMidiEventOutputBuffer.Create(8);

  Globals := TGlobals.Create;

  //== Vst2 Methods ==
  Globals.Vst2.GetParameter := self.GetParameter;
  Globals.Vst2.SetParameter := self.SetParameter;
  Globals.Vst2.SetParameterAutomated := self.SetParameterAutomated;
  Globals.Vst2.BeginEdit := self.BeginEdit;
  Globals.Vst2.EndEdit := self.EndEdit;
  Globals.Vst2.IncrementMidiEventDelta := MidiOutputBuffer.IncrementGlobalDelta;
  Globals.Vst2.SendMidiEvent := MidiOutputBuffer.AddMidiEvent;
  Globals.Vst2.SendMidiEventWithDeltaOffset := MidiOutputBuffer.AddMidiEvent;

  Plug := CreateInfo.PlugClass.Create(Globals);
  PlugInfo := CreateInfo.PlugInfo.Create(Plug);
  ProcessController := CreateInfo.ProcessController.Create(Plug);

  inherited Create(anAudioMaster, PlugInfo.GetNumberOfPrograms, Plug.VstParameterCount);

  if kUsingGui
    then self.Editor := TVst2EditAdapter.Create(self, Plug, CreateInfo.EditorClass, Globals)
    else self.Editor := nil;

  setNumInputs(PlugInfo.GetNumberOfAudioInputs);
  setNumOutputs(PlugInfo.GetNumberOfAudioOutputs);

  IsSynth(PlugInfo.GetIsSynth);

  self.SetInitialDelay(0);


  // finally..
  Plug.LoadDefaultPatch;
end;

destructor TVst2PlugAdapter.Destroy;
begin
  if assigned(self.Editor) then
  begin
    Editor.Free;
    Editor := nil;
  end;

  Plug.Free;
  PlugInfo.Free;
  ProcessController.Free;
  Globals.Free;

  MidiOutputBuffer.Free;

  inherited;
end;

procedure TVst2PlugAdapter.Open;
begin
  inherited;
  Plug.Open;
end;

function TVst2PlugAdapter.CanDo(Text: PAnsiChar): VstInt32;
begin
  if SameText(Text, canDoReceiveVstEvents)    then exit(1);
  if SameText(Text, canDoReceiveVstMidiEvent) then exit(1);

  if SameText(Text, canDoSendVstEvents)    then exit(1);
  if SameText(Text, canDoSendVstMidiEvent) then exit(1);

  if SameText(Text, canDoReceiveVstTimeInfo) then exit(1);
  if SameText(Text, canDoOffline) then exit(1);

  result := inherited;
end;

procedure TVst2PlugAdapter.Close;
begin
  inherited;
  Plug.Close;
end;

procedure TVst2PlugAdapter.Suspend;
begin
  inherited;
  ProcessController.Suspend;
end;

procedure TVst2PlugAdapter.Resume;
var
  rtInfo : TRunTimeInfo;
  InputLat, OutputLat : integer;
begin
  inherited;

  InputLat := GetInputLatency;
  OutputLat := GetOutputLatency;

  rtInfo.InputCount      := PlugInfo.GetNumberOfAudioInputs;
  rtInfo.OutputCount     := PlugInfo.GetNumberOfAudioOutputs;
  rtInfo.SampleRate      := round(self.SampleRate);
  rtInfo.MaxSampleFrames := self.BlockSize;
  rtInfo.FastControlBufferSize := 4410;
  rtInfo.SlowControlBufferSize := 44100;


  ProcessController.Resume(rtInfo);
end;

procedure TVst2PlugAdapter.SetParameter(index: Integer; value: Single);
begin
  Plug.VstParameter[Index] := Value;

  Globals.EventDispatcher.Broadcast(  TVstParameterChanged.Create(Index, Value)  );
end;

function TVst2PlugAdapter.GetParameter(index: Integer): Single;
begin
  result := Plug.VstParameter[Index];
end;

procedure TVst2PlugAdapter.GetParameterDisplay(index: Integer; text: PAnsiChar);
begin
  StrPCopy(text, AnsiString(PlugInfo.GetParameterDisplay(Index)));
end;

procedure TVst2PlugAdapter.GetParameterLabel(index: Integer; aLabel: PAnsiChar);
begin
  StrPCopy(aLabel, AnsiString(PlugInfo.GetParameterLabel(Index)));
end;

procedure TVst2PlugAdapter.GetParameterName(index: Integer; text: PAnsiChar);
begin
  StrPCopy(text, AnsiString(PlugInfo.GetParameterName(Index)));
end;

function TVst2PlugAdapter.ProcessEvents(ev: PVstEvents): longint;
begin
  ProcessController.ProcessVstEvents(ev);
  //TODO:HIGH what should the return value be?
end;

procedure TVst2PlugAdapter.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
var
  vevs : PVstEvents;
begin
  MidiOutputBuffer.ResetGlobalDelta;

  Plug.ProcessVstTimeInfo(GetTimeInfo(32767));

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

  FGlobals := aGlobals;
  Globals.Vst2.ResizeGui := self.ResizeGui;

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
  Globals.Vst2.ResizeGui := nil;
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

    Editor := EditorClass.Create(Globals);
    Editor.Open(WindowHandle, CurrentGuiSize.Width, CurrentGuiSize.Height);
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
