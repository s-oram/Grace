unit eeVstAdapter;

interface

{$INCLUDE Defines.inc}

{$DEFINE TEMPLATE_VERSION_2}

{$IFDEF REQUIRE_TEMPLATE_VERSION_2}
  {$IFNDEF TEMPLATE_VERSION_2}
    {$MESSAGE Fatal 'EasyEffectTemplate Version 2 required.'}
  {$ENDIF}
{$ENDIF}

uses
  eeProcessController,
  Windows,
  eePluginGui, eePlugin, // <-- It is important for these file to be project specific.
  eeVstEditorAdapter, eeMidiEvents,
  Classes, VamLib.MoreTypes, uConstants,
  DVstUtils, DAEffect, DAEffectX, DAudioEffect, DAudioEffectX;

type
  // NOTE: I'm not sure if any situations require the "packed record" declaration here.
  // VSTEvents is declared as "record" in DAEffectX.
  //TeeVstEvents = packed record
  TeeVstEvents = record
    numEvents  : VstInt32;		              // number of Events in array
    reserved   : VstIntPtr;		              // zero (Reserved for future use)
    events     : array[0..1] of PVstEvent;  // event pointer array, variable size
  end;

type
  TeeVstAdapter = class(AudioEffectX)
  private
  protected
    ChunkData:TMemoryStream;
    Plugin:TeePlugin;
    ParametersHaveChanged:boolean;

    //Event structures used to send midi data to the host.
    Events        : TeeVstEvents;
    MidiEvents    : array of VstMidiEvent;
    MidiOutBuffer : TeeMidiEventList;
    //----------------------------------------------------

    ProcessControllerV2  : TProcessController;

    procedure EventHandler_ResizeGuiWindow(Sender:TObject; Width, Height:integer; var Allowed:boolean);
    procedure EventHandler_PresetNameChanged(Sender:TObject);

    procedure UpdatePluginTimeInfo(TimeInfo:PVstTimeInfo);

    function GetReportedHostName : string;
  public
    constructor Create(audioMaster: TAudioMasterCallbackFunc; numPrograms, numParams: longint); reintroduce;
    destructor Destroy; override;

    function Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; override;

    function ChangeInputCount(NewInputCount:integer):integer;
    function ChangeOutputCount(NewOutputCount:integer):integer;

    procedure Suspend; override;
    procedure Resume; override;

    function GetChunk(var data: pointer; isPreset: Boolean): longint; override;   // returns byteSize
    function SetChunk(data: pointer; byteSize: longint; isPreset: Boolean): longint; override;

    procedure SetParameter(index: Longint; value: Single); override;
    function GetParameter(index: Longint): Single; override;

    procedure GetParameterLabel(index: Longint; aLabel: PAnsiChar); override;
    procedure GetParameterDisplay(index: Longint; text: PAnsiChar); override;
    procedure GetParameterName(index: Longint; text: PAnsiChar); override;

    function GetProgram: longint; override;
    procedure SetProgram(aProgram: longint); override;
    procedure SetProgramName(name: PAnsiChar); override;
    procedure GetProgramName(name: PAnsiChar); override;
    function GetProgramNameIndexed(category, index: longint; text: PAnsiChar): boolean; override;

    function GetProductString(text: PAnsiChar): boolean; override;
    function GetVendorString(text: PAnsiChar): boolean; override;
    function GetPlugCategory: VstPlugCategory; override;
    function CanDo(text:PAnsiChar):longint;override;
    function GetInputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean; override;
    function GetOutputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean; override;

    function ProcessEvents(ev: PVstEvents): longint; override;

    function SetBypass(OnOff: boolean): boolean; override;

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;
  end;

procedure ProcessClassReplacing_SplitBuffer(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
function DispatchEffectClass(e: PAEffect; Opcode, Index: VstInt32; Value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; cdecl;

implementation

uses
  {$IFDEF MadExcept}MadExcept,{$ENDIF}
  {$IFDEF Logging} siAuto, {$ENDIF}
  {$IFDEF VER230}
    Vcl.Dialogs, System.SysUtils,
  {$ELSE}
    Dialogs, SysUtils,
  {$ENDIF}
  eeCustomGlobals,
  SyncObjs,
  eeTypes, eeFunctions, eePluginSettings, eeVstExtra,
  eeGlobals;

var
  DispatchEffectLock : TMutex;


procedure ProcessClassReplacing_SplitBuffer(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
var
  obj : AudioEffect;
begin
  try
    assert(e <> nil);
    obj := e^.vObject;
    if (obj <> nil) then
    begin
      TeeVstAdapter(e^.vObject).ProcessControllerV2.ProcessReplacing(Inputs, Outputs, SampleFrames);
    end;
  except
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;




function DispatchEffectClass(e: PAEffect; Opcode, Index: VstInt32; Value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; cdecl;
var
   obj : AudioEffect;
begin
  try
    if (Opcode = effEditGetRect) or (Opcode = effEditOpen) or (Opcode = effEditClose)  or (Opcode = effEditIdle) then
    begin
      obj := e^.vObject;
      Result := obj.dispatcher(opCode, index, value, ptr, opt);
    end else
    begin
      DispatchEffectLock.Acquire;
      try
        obj := e^.vObject;
        if opCode = effClose then
        begin
          obj.Dispatcher(opCode, index, value, ptr, opt);
          obj.Free;
          Result := 1;
        end else
        begin
          Result := obj.dispatcher(opCode, index, value, ptr, opt);
        end;
      finally
        DispatchEffectLock.Release;
      end;
    end;
  except
    result := 0;
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;






{ TeeVstAdapter }


constructor TeeVstAdapter.Create(audioMaster: TAudioMasterCallbackFunc; numPrograms, numParams: Integer);
var
  UniqueId:AnsiString;
begin
  //Create the plugin.
  Plugin := TeePlugin.Create;
  Plugin.OnPresetNameChanged := EventHandler_PresetNameChanged;

  // Assign global VST method references so that the GUI can set/get parameter information.
  Plugin.Globals.VstMethods^.SetParameterAutomated := self.SetParameterAutomated;
  Plugin.Globals.VstMethods^.GetParameter          := self.GetParameter;
  Plugin.Globals.VstMethods^.BeginParameterEdit    := self.BeginEdit;
  Plugin.Globals.VstMethods^.EndParameterEdit      := self.EndEdit;
  Plugin.Globals.VstMethods^.GetParameterDisplay   := Plugin.GetParameterDisplay;
  Plugin.Globals.VstMethods^.GetParameterLabel     := Plugin.GetParameterLabel;
  Plugin.Globals.VstMethods^.EnableMidiLearn       := Plugin.MidiAutomation.EnableMidiLearn;
  Plugin.Globals.VstMethods^.SetMidiBinding        := Plugin.MidiAutomation.SetMidiBinding;
  Plugin.Globals.VstMethods^.RemoveMidiBinding     := Plugin.MidiAutomation.RemoveBinding;
  Plugin.Globals.VstMethods^.GetCurrentMidiBiding  := Plugin.MidiAutomation.GetCurrentBinding;

  ProcessControllerV2 := TProcessController.Create(Plugin);
  ProcessControllerV2.TimeInfoMethod := self.GetTimeInfo;

  inherited Create(audioMaster, Plugin.Settings.NumberOfPrograms, Plugin.PublishedVstParameters.Count);

  // Update plugin with some host properties.
  Plugin.Globals.HostProperties^.HostName    := GetReportedHostName;
  Plugin.Globals.HostProperties^.HostVersion := GetHostVendorVersion;

  if Plugin.Settings.UseHostGui <> true
    then self.Editor := TVstEditor.Create(self, Plugin)
    else self.Editor := nil;

  //Set some Plugin function pointers.
  Plugin.ChangeInputCountFunction  := Self.ChangeInputCount;
  Plugin.ChangeOutputCountFunction := Self.ChangeOutputCount;
  Plugin.SetParameter              := Self.SetParameter;
  Plugin.SetParameterAutomated     := Self.SetParameterAutomated;

  Plugin.AudioEffect.BeginEdit     := self.BeginEdit;
  Plugin.AudioEffect.EndEdit       := self.EndEdit;

  //Connected event handlers..
  Plugin.OnResizeGuiWindow := EventHandler_ResizeGuiWindow;

  ChunkData := TMemoryStream.Create;

  //report VST properties to host
  IsSynth(Plugin.Settings.IsSynth);

  setNumInputs(Plugin.Settings.InitialInputCount);
  setNumOutPuts(Plugin.Settings.InitialOutputCount);


  //hasVu(FALSE);
  canProcessReplacing(TRUE);

  UniqueId := Plugin.Settings.VstUniqueId;
  assert(Length(UniqueId) = 4);
  setUniqueID(FourCharToLong(UniqueID[1],UniqueID[2],UniqueID[3],UniqueID[4]));
  ProgramsAreChunks(Plugin.Settings.PresetsAreChunks);

  MidiOutBuffer := TeeMidiEventList.Create;

  FEffect.processReplacing := processClassReplacing_SplitBuffer;
  FEFfect.dispatcher       := DispatchEffectClass;

  Suspend;
  Randomize;

  Plugin.Globals.TriggerEvent(TPluginEvent.SampleRateChanged);
  Plugin.Globals.TriggerEvent(TPluginEvent.BlockSizeChanged);

  //Plugin.InitializeState;

  ParametersHaveChanged := true;

  {$IFDEF Logging}
  SiMain.LogMessage('Create Finished');
  {$ENDIF}
end;

destructor TeeVstAdapter.Destroy;
begin
  SetLength(MidiEvents,0);

  if assigned(Editor) then
  begin
   Editor.Free;
   Editor := nil;
  end;

  if assigned(MidiOutBuffer) then FreeAndNil(MidiOutBuffer);
  if assigned(Plugin)        then FreeAndNil(Plugin);
  if assigned(ChunkData)     then FreeAndNil(ChunkData);

  ProcessControllerV2.Free;
  inherited;
end;

function TeeVstAdapter.Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr;
begin
  result := inherited Dispatcher(Opcode, Index, Value, ptr, opt);
end;

//--------------------------------------------------------------------
//  Plugin information methods.
//--------------------------------------------------------------------
function TeeVstAdapter.GetPlugCategory: VstPlugCategory;
begin
  result:= Plugin.Settings.VstPlugCatagory;
end;

function TeeVstAdapter.GetProductString(text: PAnsiChar): boolean;
var
  s:AnsiString;
begin
  s := AnsiString(Plugin.Settings.PluginName);
  if Length(s) > kVstMaxProductStrLen    then SetLength(s,kVstMaxProductStrLen  );
  StrPCopy(text,s);
  result := true;
end;

function TeeVstAdapter.GetVendorString(text: PAnsiChar): boolean;
var
  s:AnsiString;
begin
  s := AnsiString(Plugin.Settings.PluginVendor);
  if Length(s) > kVstMaxVendorStrLen    then SetLength(s,kVstMaxVendorStrLen  );
  StrPCopy(text,s);
  result := true;
end;

procedure TeeVstAdapter.GetParameterName(index: Integer; text: PAnsiChar);
var
  s:AnsiString;
begin
  if (Index >= 0) and (Index < Plugin.PublishedVstParameters.Count) then
  begin
        //TODO: I think the Vst adapter should access the published parameter directly
    // instead of going via the GetParameter...() method below.
    s := AnsiSTring(Plugin.GetParameterName(Index));

    if Length(s) > kVstMaxParamStrLen  then SetLength(s,kVstMaxParamStrLen);

    StrPCopy(text,s);
  end else
  begin
    StrPCopy(text,'');
  end;

end;

procedure TeeVstAdapter.GetParameterDisplay(index: Integer; text: PAnsiChar);
var
  s:AnsiString;
begin
  if (Index >= 0) and (Index < Plugin.PublishedVstParameters.Count) then
  begin
        //TODO: I think the Vst adapter should access the published parameter directly
    // instead of going via the GetParameter...() method below.
    s := AnsiString(Plugin.GetParameterDisplay(Index));

    if Length(s) > kVstMaxParamStrLen  then SetLength(s,kVstMaxParamStrLen);

    StrPCopy(text,s);
  end else
  begin
    StrPCopy(text,'');
  end;

end;

procedure TeeVstAdapter.GetParameterLabel(index: Integer; aLabel: PAnsiChar);
var
  s:AnsiString;
begin
  if (Index >= 0) and (Index < Plugin.PublishedVstParameters.Count) then
  begin
    //TODO: I think the Vst adapter should access the published parameter directly
    // instead of going via the GetParameter...() method below.
    s := AnsiString(Plugin.GetParameterLabel(Index));

    if Length(s) > kVstMaxParamStrLen  then SetLength(s,kVstMaxParamStrLen);

    StrPCopy(aLabel,s);
  end else
  begin
    StrPCopy(aLabel,'');
  end;

end;

function TeeVstAdapter.CanDo(text: PAnsiChar): longint;
begin
  Result:=-1;
  // there are way more of these so called "CanDo"s, check DAudioEffectX!

  // can plugin receive MIDI?
  if Plugin.Settings.HasMidiIn then
  begin
    if AnsiStrIComp(text, 'receiveVstEvents')    = 0 then result := 1;
    if AnsiStrIComp(text, 'receiveVstMidiEvent') = 0 then result := 1;
  end;

  // can plugin send MIDI?
  if Plugin.Settings.HasMidiOut then
  begin
    if AnsiStrIComp(text,'sendVstEvents')    = 0 then result := 1;
    if AnsiStrIComp(text,'sendVstMidiEvent') = 0 then result := 1;
  end;

  // can plugin sync to host?
  //if kSyncToHost then
  if AnsiStrIComp(text,'receiveVstTimeInfo') = 0 then result := 1;

  if AnsiStrIComp(text, canDoBypass) = 0 then
  begin
    if Plugin.Settings.SoftBypass
      then result := 1
      else result := -1;
  end;
end;



//------------------------------------------------------------------
//    Programs
//------------------------------------------------------------------

// NOTE: Currently the eeVstAdapter doesn't fully support the FXB/FXP
// functionality. EasyEffect-Plugins only know about their current state.
// If desired, the functionality to support banks/program changes could
// be added without changing the EasyEffect-Plugin class.

procedure TeeVstAdapter.SetProgram(aProgram: Integer);
begin
  //Do nothing, feature not supported.
end;

function TeeVstAdapter.GetProgram: longint;
begin
  //Feature not supported, return 0.
  result := 0;
end;

procedure TeeVstAdapter.SetProgramName(name: PAnsiChar);
begin
  Plugin.PresetName := String(Name);
end;

procedure TeeVstAdapter.GetProgramName(name: PAnsiChar);
var
  s:ansistring;
begin
  s := AnsiString(Plugin.PresetName);
  if Length(s) > kVstMaxProgNameLen   then SetLength(s,kVstMaxProgNameLen );
  StrPCopy(name,s);
end;

function TeeVstAdapter.GetProgramNameIndexed(category, index: Integer; text: PAnsiChar): boolean;
var
  s:Ansistring;
begin
  s := AnsiString(Plugin.PresetName);
  if Length(s) > kVstMaxProgNameLen then SetLength(s,kVstMaxProgNameLen );
  StrPCopy(text,s);
  result := true;
end;

function TeeVstAdapter.GetReportedHostName: string;
var
  s3 : array[0..128] of Ansichar;
  TempString : string;
begin
  GetHostProductString(@s3[0]);

  TempString := String(s3);

  // HACK: This is a workaround for Traction. When adding a second vst instance, Tracktion reports "TracktionsSelectionS"
  // as the host. (Dunno what's going on there..)(maybe my fault??)
  if Pos('Tracktion', TempString) > 0 then TempString := 'Tracktion';

  result := TempString;
end;

//------------------------------------------------------------------
//    Input/Output pin
//------------------------------------------------------------------

function TeeVstAdapter.GetInputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean;
var
  PinProperties:TPinProperties;
  s:ansistring;
begin
  Plugin.GetInputPinProperties(Index, @PinProperties);

  s := ansistring(PinProperties.Name);
  if Length(s) > kVstMaxLabelLen then SetLength(s,kVstMaxLabelLen );
  StrPCopy(Properties.vLabel,s);

  s := ansistring(PinProperties.ShortName);
  if Length(s) > kVstMaxShortLabelLen then SetLength(s,kVstMaxShortLabelLen );
  StrPCopy(Properties.shortLabel,s);

  if PinProperties.IsStereo
    then Properties.flags := KVstPinIsActive + kVstPinIsStereo
    else Properties.flags := KVstPinIsActive;

  result := true;

end;

function TeeVstAdapter.GetOutputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean;
var
  PinProperties:TPinProperties;
  s:ansistring;
begin
  Plugin.GetOutputPinProperties(Index, @PinProperties);

  s := AnsiString(PinProperties.Name);
  if Length(s) > kVstMaxLabelLen then SetLength(s,kVstMaxLabelLen );
  StrPCopy(Properties.vLabel,s);

  s := AnsiString(PinProperties.ShortName);
  if Length(s) > kVstMaxShortLabelLen then SetLength(s,kVstMaxShortLabelLen );
  StrPCopy(Properties.shortLabel,s);

  if PinProperties.IsStereo
    then Properties.flags := KVstPinIsActive + kVstPinIsStereo
    else Properties.flags := KVstPinIsActive;

  result := true;

end;

function TeeVstAdapter.ChangeInputCount(NewInputCount: integer): integer;
begin
  SetNumInputs(NewInputCount);
  IOChanged;

  //After asking for the input count to be changed, check for confirmation.
  result := Effect.numInputs;
end;

function TeeVstAdapter.ChangeOutputCount(NewOutputCount: integer): integer;
begin
  SetNumOutputs(NewOutputCount);
  IOChanged;
  //After asking for the output count to be changed, check for confirmation.
  result := Effect.numOutputs;
end;



//------------------------------------------------------------------
//    Set/Get Parameter methods.
//------------------------------------------------------------------

procedure TeeVstAdapter.SetParameter(index: Integer; value: Single);
begin
  try
    Plugin.ParameterChanged(Index, Value);
  except
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;


function TeeVstAdapter.GetParameter(index: Integer): Single;
begin
  try
    result := Plugin.GetParameter(Index);
  except
    {$IFDEF MadExcept}
    result := 0;
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;



//------------------------------------------------------------------
//    Set/Get Chunk methods. (used to save/restore the plugin state)
//------------------------------------------------------------------

function TeeVstAdapter.GetChunk(var data: pointer; isPreset: Boolean): longint;
begin
  //Clear the data storage memory stream
  ChunkData.Clear;
  try
    Plugin.GetPreset(ChunkData);
    data   := ChunkData.Memory;
    result := ChunkData.Size;
  except
    data   := ChunkData.Memory;
    result := 0;
  end;
end;

function TeeVstAdapter.SetBypass(OnOff: boolean): boolean;
begin
  result := Plugin.Settings.SoftBypass;
  if Plugin.Settings.SoftBypass then Plugin.SetBypass(OnOff);
end;

function TeeVstAdapter.SetChunk(data: pointer; byteSize: Integer; isPreset: Boolean): longint;
begin
  result := byteSize;

  ChunkData.Clear;
  ChunkData.Write(data^,bytesize);
  ChunkData.Seek(0,soFromBeginning);

  try
    Plugin.SetPreset(ChunkData);
  except
  end;

  ChunkData.Clear;
end;

//--------------------------------------------------------------------
//  Suspend/Resume
//--------------------------------------------------------------------

procedure TeeVstAdapter.Suspend;
begin
  inherited;
  Plugin.Globals.TriggerVstSuspendEvent;
  Plugin.Suspend;
end;

procedure TeeVstAdapter.Resume;
var
  sr : integer;
  bs : integer;
  cr : integer;
begin
  inherited;

  {$IFDEF OverSampleEnabled}
    sr := round(SampleRate) * Plugin.Settings.OverSampleFactor;
    bs := BlockSize         * Plugin.Settings.OverSampleFactor;
    cr := round(sr / Plugin.Settings.FastControlRateDivision);
  {$ELSE}
    sr := round(SampleRate);
    bs := BlockSize;
    cr := round(sr / Plugin.Settings.ControlRateDivision);
  {$ENDIF}



  if Plugin.Globals.BlockSize   <> bs then Plugin.Globals.BlockSize   := bs;

  Plugin.Globals.UpdateSampleRates(sr, cr, cr);

  ProcessControllerV2.Resume(BlockSize, round(SampleRate), Plugin.Settings.OverSampleFactor, Effect.numInputs, Effect.numOutputs);

  // important for all plugins that want to receive MIDI!
  //wantEvents(1);

  //Any ChunkData can be clear'ed now.
  ChunkData.Clear;

  Plugin.Resume;
  Plugin.Globals.TriggerVstResumeEvent;
end;


procedure TeeVstAdapter.UpdatePluginTimeInfo(TimeInfo: PVstTimeInfo);
var
  Playing, CycleActive, Recording:boolean;
begin
  if (TimeInfo^.flags and kVstTempoValid) >=1 then
  begin
    if TimeInfo^.Tempo <> Plugin.Globals.Tempo then Plugin.Globals.Tempo := TimeInfo^.Tempo;
  end;

  if (TimeInfo^.Flags and kVstPpqPosValid) >= 1 then Plugin.Globals.ppqPos             := TimeInfo^.ppqPos;
  if (TimeInfo^.Flags and kVstBarsValid)   >= 1 then Plugin.Globals.BarStartPos        := TimeInfo^.barStartPos;

  {$IFDEF OverSampleEnabled}
    if (TimeInfo^.Flags and kVstClockValid)  >= 1 then Plugin.Globals.SamplesToNextClock := TimeInfo^.samplesToNextClock * Plugin.Settings.OverSampleFactor;
  {$ELSE}
    if (TimeInfo^.Flags and kVstClockValid)  >= 1 then Plugin.Globals.SamplesToNextClock := TimeInfo^.samplesToNextClock;
  {$ENDIF}


  if (TimeInfo^.Flags and kVstTransportChanged) >= 1 then
  begin
    // TODO: This is depreciated and should be removed.
    if (TimeInfo^.Flags and kVstTransportPlaying) >= 1
      then Plugin.HostPlayState := psHostIsPlaying
      else Plugin.HostPlayState := psHostIsStopped;
    //================================================

    if (TimeInfo^.Flags and kVstTransportPlaying) >= 1
      then Playing := true
      else Playing := false;

    if (TimeInfo^.Flags and kVstTransportCycleActive) >= 1
      then CycleActive := true
      else CycleActive := false;

    if (TimeInfo^.Flags and kVstTransportRecording) >= 1
      then Recording := true
      else Recording := false;

    Plugin.Globals.UpdateTransportState(Playing, CycleActive, Recording);
  end;

  Plugin.Globals.TimeSigNumerator   := TimeInfo^.timeSigNumerator;
  Plugin.Globals.TimeSigDenominator := TimeInfo^.timeSigDenominator;

  Plugin.Globals.UpdateSyncInfo;
end;



//--------------------------------------------------------------------
//  Process methods.
//--------------------------------------------------------------------
function TeeVstAdapter.ProcessEvents(ev: PVstEvents): longint;
begin
  result := ProcessControllerV2.ProcessEvents(ev);
end;

procedure TeeVstAdapter.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
begin
  assert(false, 'This method is never called.');
end;

procedure TeeVstAdapter.EventHandler_PresetNameChanged(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TeeVstAdapter.EventHandler_ResizeGuiWindow(Sender: TObject; Width, Height: integer; var Allowed: boolean);
begin
  Allowed := SizeWindow(Width, Height);
end;

initialization
  DispatchEffectLock := TMutex.Create(false);
finalization
  OutputDebugString(PWideChar('Finalize eeVstAdapter.pas'));
  DispatchEffectLock.Free;

end.


