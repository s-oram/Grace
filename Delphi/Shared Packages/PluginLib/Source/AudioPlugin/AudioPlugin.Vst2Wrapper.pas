unit AudioPlugin.Vst2Wrapper;

interface

uses
  VamLib.MoreTypes,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.DAudioEffect,
  VamVst2.DAudioEffectX,
  VamVst2.MidiEventOutputBuffer,
  AudioPlugin,
  AudioPlugin.Globals,
  AudioPlugin.Editor,
  AudioPlugin.Vst2Editor,
  AudioPlugin.Vst2PluginInfo,
  AudioPlugin.ProcessController;

type
  TVst2WrapperCreateInfo = record
    PlugClass         : TAudioPluginClass;
    EditorClass       : TAudioPluginEditorClass;
    PlugInfo          : TVst2PluginInfoClass;
    ProcessController : TProcessControllerClass;
  end;

  TVst2Wrapper = class(AudioEffectX)
  private
  protected
    Globals : TGlobals;
    Plug : TAudioPlugin;
    PlugInfo : TVst2PluginInfo;
    ProcessController : TProcessController;
    MidiOutputBuffer : TMidiEventOutputBuffer;
  public
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2WrapperCreateInfo); reintroduce;
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

implementation

uses
  SysUtils,
  AudioPlugin.Events,
  AudioPlugin.RunTimeInfo;

const
  kUsingGui = true;

{ TVst2Wrapper }

constructor TVst2Wrapper.Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2WrapperCreateInfo);
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
    then self.Editor := TVstEditor.Create(self, Plug, CreateInfo.EditorClass, Globals)
    else self.Editor := nil;

  setNumInputs(PlugInfo.GetNumberOfAudioInputs);
  setNumOutputs(PlugInfo.GetNumberOfAudioOutputs);

  IsSynth(PlugInfo.GetIsSynth);

  self.SetInitialDelay(0);


  // finally..
  Plug.LoadDefaultPatch;
end;

destructor TVst2Wrapper.Destroy;
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

procedure TVst2Wrapper.Open;
begin
  inherited;
  Plug.Open;
end;

function TVst2Wrapper.CanDo(Text: PAnsiChar): VstInt32;
begin
  if SameText(Text, canDoReceiveVstEvents)    then exit(1);
  if SameText(Text, canDoReceiveVstMidiEvent) then exit(1);

  if SameText(Text, canDoSendVstEvents)    then exit(1);
  if SameText(Text, canDoSendVstMidiEvent) then exit(1);

  if SameText(Text, canDoReceiveVstTimeInfo) then exit(1);
  if SameText(Text, canDoOffline) then exit(1);

  result := inherited;
end;

procedure TVst2Wrapper.Close;
begin
  inherited;
  Plug.Close;
end;

procedure TVst2Wrapper.Suspend;
begin
  inherited;
  ProcessController.Suspend;
end;

procedure TVst2Wrapper.Resume;
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

procedure TVst2Wrapper.SetParameter(index: Integer; value: Single);
begin
  Plug.VstParameter[Index] := Value;

  Globals.EventDispatcher.Broadcast(  TVstParameterChanged.Create(Index, Value)  );
end;

function TVst2Wrapper.GetParameter(index: Integer): Single;
begin
  result := Plug.VstParameter[Index];
end;

procedure TVst2Wrapper.GetParameterDisplay(index: Integer; text: PAnsiChar);
begin
  StrPCopy(text, AnsiString(PlugInfo.GetParameterDisplay(Index)));
end;

procedure TVst2Wrapper.GetParameterLabel(index: Integer; aLabel: PAnsiChar);
begin
  StrPCopy(aLabel, AnsiString(PlugInfo.GetParameterLabel(Index)));
end;

procedure TVst2Wrapper.GetParameterName(index: Integer; text: PAnsiChar);
begin
  StrPCopy(text, AnsiString(PlugInfo.GetParameterName(Index)));
end;

function TVst2Wrapper.ProcessEvents(ev: PVstEvents): longint;
begin
  ProcessController.ProcessVstEvents(ev);
  //TODO:HIGH what should the return value be?
end;

procedure TVst2Wrapper.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
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




end.
