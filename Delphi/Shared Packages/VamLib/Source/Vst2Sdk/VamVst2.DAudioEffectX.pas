//******************************************************************************
//
//  DAudioEffectX.pas
//  8 December
//
//  Part of the VST 2.4.2 SDK for Delphi
//  by Frederic Vanmol
//     http://www.axiworld.be
//     frederic@axiworld.be
//
//------------------------------------------------------------------------------
//
// VST Plug-Ins SDK
// Version 2.4       $Date: 2006/01/12 09:05:31 $
//
// Category     : VST 2.x Classes
// Filename     : audioeffectx.h
// Created by   : Steinberg Media Technologies
// Description  : Class AudioEffectX extends AudioEffect with new features. You should derive
//                your plug-in from AudioEffectX.
//
// © 2006, Steinberg Media Technologies, All Rights Reserved
//
//******************************************************************************
unit
    VamVst2.DAudioEffectX;

interface

uses
    VamVst2.DAEffect, VamVst2.DAEffectX, VamVst2.DAudioEffect;

{$INCLUDE VamVst2.DVstCommon.inc}

//------------------------------------------------------------------------------
// Extended VST Effect Class (VST 2.x)
//------------------------------------------------------------------------------
type
    AudioEffectX = class(AudioEffect)
    public
      // Parameters
      function CanParameterBeAutomated(Index: VstInt32): boolean; virtual;                                // Indicates if a parameter can be automated
      function String2parameter(Index: VstInt32; Text: PAnsiChar): boolean; virtual;                          // Convert a string representation to a parameter value
      function GetParameterProperties(Index: VstInt32; p: PVstParameterProperties): boolean; virtual;     // Return parameter properties

      {$IFDEF VST_2_1_EXTENSIONS}
      function BeginEdit(Index: VstInt32): boolean; virtual;                                              // To be called before SetParameterAutomated (on Mouse Down). This will be used by the Host for specific Automation Recording.
      function EndEdit(Index: VstInt32): boolean; virtual;                                                // To be called after SetParameterAutomated (on Mouse Up)
      {$ENDIF} // VST_2_1_EXTENSIONS

      // Programs and Persistence
      function GetProgramNameIndexed(Category, Index: VstInt32; Text: PAnsiChar): boolean; virtual; // Fill text with name of program index (category deprecated in VST 2.4)

      {$IFDEF VST_2_1_EXTENSIONS}
      function BeginSetProgram: boolean; virtual;                                               // Called before a program is loaded
      function EndSetProgram: boolean; virtual;                                                 // Called after a program was loaded
      {$ENDIF} // VST_2_1_EXTENSIONS

      {$IFDEF VST_2_3_EXTENSIONS}
      function BeginLoadBank(ptr: PVstPatchChunkInfo): VstInt32; virtual;                       // Called before a Bank is loaded.
      function BeginLoadProgram(ptr: PVstPatchChunkInfo): VstInt32; virtual;                    // Called before a Program is loaded. (called before BeginSetProgram).
      {$ENDIF} // VST_2_3_EXTENSIONS

      // Connections and Configuration
      function IOChanged: boolean; virtual;                                                                      // Tell Host numInputs and/or numOutputs and/or initialDelay (and/or numParameters: to be avoid) have changed

      function UpdateSampleRate: double; virtual;                                                                // Returns sample rate from Host (may issue setSampleRate)
      function UpdateBlockSize: VstInt32; virtual;                                                               // Returns block size from Host (may issue getBlockSize)
      function GetInputLatency: VstInt32; virtual;                                                               // Returns the Audio (maybe ASIO) input latency values
      function GetOutputLatency: VstInt32; virtual;                                                              // Returns the Audio (maybe ASIO) output latency values

      function GetInputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean; virtual;             // Return the properties of output index
      function GetOutputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean; virtual;            // Return the properties of input index

      function SetSpeakerArrangement(PluginInput, PluginOutput: PVstSpeakerArrangement): boolean; virtual;       // Set the plug-in's speaker arrangements
      function GetSpeakerArrangement(PluginInput, PluginOutput: PPVstSpeakerArrangement): boolean; virtual;      // Return the plug-in's speaker arrangements
      function SetBypass(OnOff: boolean): boolean; virtual;                                                      // For 'soft-bypass' (this could be automated (in Audio Thread) that why you could NOT call iochanged (if needed) in this function, do it in fxidle)

      {$IFDEF VST_2_3_EXTENSIONS}
      function SetPanLaw(vType: VstInt32; Value: single): boolean; virtual;                                      // Set the Panning Law used by the Host @see VstPanLawType.
      {$ENDIF} // VST_2_3_EXTENSIONS

      {$IFDEF VST_2_4_EXTENSIONS}
      function SetProcessPrecision(Precision: VstInt32): boolean; virtual;                                       // Set floating-point precision used for processing (32 or 64 bit)

      function GetNumMidiInputChannels: VstInt32; virtual;                                                       // Returns number of MIDI input channels used [0, 16]
      function GetNumMidiOutputChannels: VstInt32; virtual;                                                      // Returns number of MIDI output channels used [0, 16]
      {$ENDIF} // VST_2_4_EXTENSIONS

      // Realtime
      function GetTimeInfo(Filter: VstInt32): PVstTimeInfo; virtual;       // Get time information from Host
      function GetCurrentProcessLevel: VstInt32; virtual;                  // Returns the Host's process level
      function GetAutomationState: VstInt32;                               // Returns the Host's automation state

      function ProcessEvents(Events: PVstEvents): VstInt32; virtual;       // Called when new MIDI events come in
      function SendVstEventsToHost(Events: PVstEvents): boolean; virtual;  // Send MIDI events back to Host application

      {$IFDEF VST_2_3_EXTENSIONS}
      function StartProcess: VstInt32; virtual;                            // Called one time before the start of process call. This indicates that the process call will be interrupted (due to Host reconfiguration or bypass state when the plug-in doesn't support softBypass)
      function StopProcess: VstInt32; virtual;                             // Called after the stop of process call
      {$ENDIF} // VST_2_3_EXTENSIONS

      // Variable I/O (Offline)
      function ProcessVariableIO(VarIO: PVstVariableIO): boolean; virtual;     // Used for variable I/O processing (offline processing like timestreching)

      {$IFDEF VST_2_3_EXTENSIONS}
      function SetTotalSampleToProcess(Value: VstInt32): VstInt32; virtual;    // Called in offline mode before process or processVariableIo
      {$ENDIF} // VST_2_3_EXTENSIONS

      // Host Properties
      function GetHostVendorString(Text: PAnsiChar): boolean; virtual;                                                           // Fills text with a string identifying the vendor
      function GetHostProductString(Text: PAnsiChar): boolean; virtual;                                                          // Fills text with a string with product name
      function GetHostVendorVersion: VstInt32; virtual;                                                                      // Returns vendor-specific version (for example 3200 for Nuendo 3.2)
      function HostVendorSpecific(Arg1: VstInt32; Arg2: VstIntPtr; PtrArg: pointer; FloatArg: single): VstIntPtr; virtual;   // No specific definition
      function CanHostDo(Text: PAnsiChar): VstInt32; virtual;                                                                    // Reports what the Host is able to do (hostCanDos in audioeffectx.cpp)
      function GetHostLanguage: VstInt32;                                                                                    // Returns the Host's language (VstHostLanguage)

      // Plug-in Properties
      procedure IsSynth(State: boolean); virtual;                                                                      // Set if plug-in is a synth
      procedure NoTail(State: boolean); virtual;                                                                       // Plug-in won't produce output signals while there is no input
      function GetTailSize: VstInt32; virtual;                                                                         // Returns tail size; 0 is default (return 1 for 'no tail'), used in offline processing too
      function GetDirectory: pointer; virtual;                                                                         // Returns the plug-in's directory
      function GetEffectName(Name: PAnsiChar): boolean; virtual;                                                           // Fill text with a string identifying the effect
      function GetVendorString(Text: PAnsiChar): boolean; virtual;                                                         // Fill text with a string identifying the vendor
      function GetProductString(Text: PAnsiChar): boolean; virtual;                                                        // Fill text with a string identifying the product name
      function GetVendorVersion: VstInt32; virtual;                                                                    // Return vendor-specific version
      function VendorSpecific(Arg: VstInt32; Arg2: VstIntPtr; PtrArg: pointer; FloatArg: single): VstIntPtr; virtual;  // No definition, vendor specific handling
      function CanDo(Text: PAnsiChar): VstInt32; virtual;                                                                  // Reports what the plug-in is able to do (plugCanDos in audioeffectx.cpp)
      function GetVstVersion: VstInt32; virtual;                                                                       // Returns the current VST Version (kVstVersion)
      function GetPlugCategory: VstPlugCategory; virtual;                                                              // Specify a category that fits the plug (VstPlugCategory)

      // MIDI Channel Programs
      {$IFDEF VST_2_1_EXTENSIONS}
      function GetMidiProgramName(Channel: VstInt32; MidiProgramName: PMidiProgramName): VstInt32; virtual;    // Fill midiProgramName with information for 'thisProgramIndex'.
      function GetCurrentMidiProgram(Channel: VstInt32; CurrentProgram: PMidiProgramName): VstInt32; virtual;  // Fill currentProgram with information for the current MIDI program.
      function GetMidiProgramCategory(Channel: VstInt32; Category: PMidiProgramCategory): VstInt32; virtual;   // Fill category with information for 'thisCategoryIndex'.
      function HasMidiProgramsChanged(Channel: VstInt32): boolean; virtual;                                    // Return true if the MidiProgramNames, MidiKeyNames or MidiControllerNames had changed on this MIDI channel.
      function GetMidiKeyName(Channel: VstInt32; KeyName: PMidiKeyName): boolean; virtual;                     // Fill keyName with information for 'thisProgramIndex' and 'thisKeyNumber'
      {$ENDIF} // VST_2_1_EXTENSIONS

      // Others
      function UpdateDisplay: boolean; virtual;                           // Something has changed in plug-in, request an update display like program (MIDI too) and parameters list in Host
      function SizeWindow(Width, Height: VstInt32): boolean; virtual;     // Requests to resize the editor window

      {$IFDEF VST_2_1_EXTENSIONS}
      function OpenFileSelector(ptr: PVstFileSelect): boolean; virtual;   // Open a Host File selector (see aeffectx.h for #VstFileSelect definition)
      {$ENDIF} // VST_2_1_EXTENSIONS

      {$IFDEF VST_2_2_EXTENSIONS}
      function CloseFileSelector(ptr: PVstFileSelect): boolean; virtual;  // Close the Host File selector which was opened by #openFileSelector
      {$ENDIF} // VST_2_2_EXTENSIONS

      {$IFDEF VST_2_3_EXTENSIONS}
      function GetNextShellPlugin(Name: PAnsiChar): VstInt32; virtual;        // This opcode is only called, if the plug-in is of type kPlugCategShell, in order to extract all included sub-plugin´s names.
      {$ENDIF} // VST_2_3_EXTENSIONS

      // Tools
      {$IFDEF VST_2_3_EXTENSIONS}
      function AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: VstInt32): boolean; virtual;   // Allocate memory for a VstSpeakerArrangement
      function DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): boolean; virtual;                       // Delete/free memory for an allocated speaker arrangement
      function CopySpeaker(Dest, Source: PVstSpeakerProperties): boolean; virtual;                                     // Copy properties from Source to Dest
      function MatchArrangement(var Dest: PVstSpeakerArrangement; Source: PVstSpeakerArrangement): boolean; virtual;   // "Dest" is deleted, then created and initialized with the same values as "Source" ones ("Source" must exist).
      {$ENDIF} // VST_2_3_EXTENSIONS

      // Offline
      function OfflineRead(Offline: PVstOfflineTask; Option: VstOfflineOption; ReadSource: boolean): boolean; virtual;
      function OfflineWrite(Offline: PVstOfflineTask; Option: VstOfflineOption): boolean; virtual;
      function OfflineStart(AudioFiles: PVstAudioFile; NumAudioFiles, NumNewAudioFiles: VstInt32): boolean; virtual;
      function OfflineGetCurrentPass: VstInt32; virtual;
      function OfflineGetCurrentMetaPass: VstInt32; virtual;
      function OfflineNotify(ptr: PVstAudioFile; NumAudioFiles: VstInt32; Start: boolean): boolean; virtual;
      function OfflinePrepare(Offline: PVstOfflineTask; Count: VstInt32): boolean; virtual;
      function OfflineRun(Offline: PVstOfflineTask; Count: VstInt32): boolean; virtual;
      function OfflineGetNumPasses: VstInt32; virtual;
      function OfflineGetNumMetaPasses: VstInt32; virtual;

      // AudioEffect overrides
      function Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; override;
      procedure Resume; override;

      {$IFDEF VST_USE_DEPRECATED}
      // Deprecated methods
      procedure WantEvents(Filter: VstInt32); virtual;
      function TempoAt(Pos: VstInt32): VstInt32; virtual;
      function GetNumAutomatableParameters: VstInt32; virtual;
      function GetParameterQuantization: VstInt32; virtual;
      function GetNumCategories: VstInt32; virtual;
      function copyProgram(Destination: VstInt32): boolean; virtual;
      function NeedIdle: boolean; virtual;
      function GetPreviousPlug(Input: VstInt32): PAEffect; virtual;
      function GetNextPlug(Output: VstInt32): PAEffect; virtual;
      procedure InputConnected(Index: VstInt32; State: boolean); virtual;
      procedure OutputConnected(Index: VstInt32; State: boolean); virtual;
      function WillProcessReplacing: VstInt32; virtual;
      procedure WantAsyncOperation(State: boolean); virtual;
      procedure HasExternalBuffer(State: boolean); virtual;
      function ReportCurrentPosition: VstInt32; virtual;
      function ReportDestinationBuffer: psingle; virtual;
      procedure SetOutputSamplerate(SampleRate: single); virtual;
      function GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
      function GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
      function OpenWindow(Window: PVstWindow): pointer; virtual;
      function CloseWindow(Window: PVstWindow): boolean; virtual;
      procedure SetBlockSizeAndSampleRate(aBlockSize: VstInt32; aSampleRate: single); virtual;
      function GetErrorText(Text: PAnsiChar): boolean; virtual;
      function GetIcon: pointer; virtual;
      function SetViewPosition(x, y: VstInt32): boolean; virtual;
      function FxIdle: VstInt32; virtual;
      function KeysRequired: boolean; virtual;

        {$IFDEF VST_2_2_EXTENSIONS}
      function GetChunkFile(NativePath: PAnsiChar): boolean; virtual;      // Returns in platform format the path of the current chunk (could be called in #setChunk ()) (FSSpec on MAC else AnsiChar*)
        {$ENDIF} // VST_2_2_EXTENSIONS
      {$ENDIF} // VST_USE_DEPRECATED
    end;



//-------------------------------------------------------------------------------------------------------
// hostCanDos strings Plug-in -> Host
const
     canDoSendVstEvents                  = 'sendVstEvents';                     // Host supports send of Vst events to plug-in
     canDoSendVstMidiEvent               = 'sendVstMidiEvent';                  // Host supports send of MIDI events to plug-in
     canDoSendVstTimeInfo                = 'sendVstTimeInfo';                   // Host supports send of VstTimeInfo to plug-in
     canDoReceiveVstEvents               = 'receiveVstEvents';                  // Host can receive Vst events from plug-in
     canDoReceiveVstMidiEvent            = 'receiveVstMidiEvent';               // Host can receive MIDI events from plug-in
     canDoReportConnectionChanges        = 'reportConnectionChanges';           // Host will indicates the plug-in when something change in plug-in´s routing/connections with suspend/resume/setSpeakerArrangement
     canDoAcceptIOChanges                = 'acceptIOChanges';                   // Host supports ioChanged
     canDoSizeWindow                     = 'sizeWindow';                        // used by VSTGUI
     canDoOffline                        = 'offline';                           // Host supports offline feature
     canDoOpenFileSelector               = 'openFileSelector';                  // Host supports function openFileSelector
     canDoCloseFileSelector              = 'closeFileSelector';                 // Host supports function closeFileSelector
     canDoStartStopProcess               = 'startStopProcess';                  // Host supports functions startProcess and stopProcess
     canDoShellCategory                  = 'shellCategory';                     // 'shell' handling via uniqueID. If supported by the Host and the Plug-in has the category kPlugCategShell
     canDoSendVstMidiEventFlagIsRealtime = 'sendVstMidiEventFlagIsRealtime';    // Host supports flags for VstMidiEvent

//-------------------------------------------------------------------------------------------------------
// plugCanDos strings Host -> Plug-in
const
     //canDoSendVstEvents       = 'sendVstEvents';          // plug-in will send Vst events to Host
     //canDoSendVstMidiEvent    = 'sendVstMidiEvent';       // plug-in will send MIDI events to Host
     //canDoReceiveVstEvents    = 'receiveVstEvents';       // plug-in can receive MIDI events from Host
     //canDoReceiveVstMidiEvent = 'receiveVstMidiEvent';    // plug-in can receive MIDI events from Host
     canDoReceiveVstTimeInfo  = 'receiveVstTimeInfo';     // plug-in can receive Time info from Host
     //canDoOffline             = 'offline';                // plug-in supports offline functions (offlineNotify, offlinePrepare, offlineRun)
     canDoMidiProgramNames    = 'midiProgramNames';       // plug-in supports function getMidiProgramName
     canDoBypass              = 'bypass';                 // plug-in supports function setBypass




implementation

uses
    SysUtils, Windows;

{ AudioEffectX }

function AudioEffectX.CanHostDo(Text: PAnsiChar): VstInt32;
begin
  Result := 0;
	if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterCanDo, 0, 0, text, 0);
end;

function AudioEffectX.GetAutomationState: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := audioMaster(@FEffect, audioMasterGetAutomationState, 0, 0, nil, 0);
end;

function AudioEffectX.GetOutputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetOutputLatency: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetOutputLatency, 0, 0, nil, 0);
end;

function AudioEffectX.GetHostVendorString(Text: PAnsiChar): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterGetVendorString, 0, 0, text, 0) <> 0);
end;

function AudioEffectX.ProcessVariableIO(VarIO: PVstVariableIO): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.IOChanged: boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterIOChanged, 0, 0, nil, 0) <> 0);
end;

function AudioEffectX.OfflineGetNumMetaPasses: VstInt32;
begin
  Result := 0;
end;

function AudioEffectX.SetSpeakerArrangement(PluginInput, PluginOutput: PVstSpeakerArrangement): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.OfflineStart(AudioFiles: PVstAudioFile; NumAudioFiles, NumNewAudioFiles: VstInt32): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineStart, NumNewAudioFiles, NumAudioFiles, AudioFiles, 0) <> 0);
end;

function AudioEffectX.SizeWindow(Width, Height: VstInt32): boolean;
begin
  Result := FALSE;
	if Assigned(FAudioMaster) then
		Result := (FAudioMaster(@FEffect, audioMasterSizeWindow, Width, Height, nil, 0) <> 0);
end;

procedure AudioEffectX.IsSynth(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsIsSynth
  else
    FEffect.flags := FEffect.flags and not effFlagsIsSynth;
end;

function AudioEffectX.CanDo(Text: PAnsiChar): VstInt32;
begin
  Result := 0;
end;

function AudioEffectX.GetHostVendorVersion: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetVendorVersion, 0, 0, nil, 0);
end;

function AudioEffectX.GetInputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetInputLatency: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetInputLatency, 0, 0, nil, 0);
end;

function AudioEffectX.GetVstVersion: VstInt32;
begin
  Result := kVstVersion;
end;

function AudioEffectX.GetVendorString(Text: PAnsiChar): boolean;
begin
  Result := FALSE;
end;

// Return the host's block size
// NOTE: will cause application to call AudioEffect's SetSampleRate to be called (when implemented)
function AudioEffectX.UpdateBlockSize: VstInt32;
var
   res: VstInt32;
begin
  if Assigned(FAudioMaster) then
  begin
    res := FAudioMaster(@FEffect, audioMasterGetBlockSize, 0, 0, nil, 0);
    if res > 0 then
      FBlockSize := res;
  end;
  Result := FBlockSize;
end;

function AudioEffectX.GetParameterProperties(Index: VstInt32; p: PVstParameterProperties): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.OfflineGetNumPasses: VstInt32;
begin
  Result := 0;
end;

function AudioEffectX.ProcessEvents(Events: PVstEvents): VstInt32;
begin
  Result := 0;
end;

function AudioEffectX.OfflineRun(Offline: PVstOfflineTask; Count: VstInt32): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetProgramNameIndexed(Category, Index: VstInt32; Text: PAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetVendorVersion: VstInt32;
begin
  Result := 0;
end;

function AudioEffectX.HostVendorSpecific(Arg1: VstInt32; Arg2: VstIntPtr; PtrArg: pointer; FloatArg: single): VstIntPtr;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterVendorSpecific, Arg1, Arg2, PtrArg, FloatArg);
end;

function AudioEffectX.OfflineRead(Offline: PVstOfflineTask; Option: VstOfflineOption; ReadSource: boolean): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineRead, Ord(ReadSource), Option, Offline, 0) <> 0);
end;

function AudioEffectX.String2parameter(Index: VstInt32; Text: PAnsiChar): boolean;
begin
  Result := FALSE;
end;

procedure AudioEffectX.Resume;
begin
  inherited;

  {$IFDEF VST_USE_DEPRECATED}
  if (FEffect.flags and effFlagsIsSynth <> 0) or (CanDo(canDoReceiveVstMidiEvent) = 1) then
    WantEvents(1);
  {$ENDIF}
end;

function AudioEffectX.GetEffectName(Name: PAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.SetBypass(OnOff: boolean): boolean;
begin
  Result := FALSE;
end;

// Can be called inside processReplacing.
// Fill Events with VST events
// Returns true on success
function AudioEffectX.SendVstEventsToHost(Events: PVstEvents): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterProcessEvents, 0, 0, Events, 0) = 1);
end;

function AudioEffectX.GetHostLanguage: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetLanguage, 0, 0, nil, 0);
end;

function AudioEffectX.OfflineWrite(Offline: PVstOfflineTask; Option: VstOfflineOption): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineWrite, 0, Option, Offline, 0) <> 0);
end;

function AudioEffectX.VendorSpecific(Arg: VstInt32; Arg2: VstIntPtr; PtrArg: pointer; FloatArg: single): VstIntPtr;
begin
  Result := 0;
end;

function AudioEffectX.GetHostProductString(Text: PAnsiChar): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterGetProductString, 0, 0, Text, 0) <> 0);
end;

function AudioEffectX.GetTailSize: VstInt32;
begin
  Result := 0;
end;

function AudioEffectX.GetCurrentProcessLevel: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetCurrentProcessLevel, 0, 0, nil, 0);
end;

function AudioEffectX.Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr;
var
   v       : VstIntPtr;
   keyCode : VstKeyCode;
begin
  v := 0;
  case opcode of
    //---VstEvents----------------------
    effProcessEvents             :  v := ProcessEvents(ptr);

    //---Parameters and Programs----------------------
    effCanBeAutomated            :  v := Ord(CanParameterBeAutomated(index));
    effString2Parameter          :  v := Ord(String2parameter(index, ptr));
    effGetProgramNameIndexed     :  v := Ord(GetProgramNameIndexed(value, index, ptr));

    {$IFDEF VST_USE_DEPRECATED}
    effGetNumProgramCategories   :  v := GetNumCategories;
    effCopyProgram               :  v := Ord(CopyProgram(index));

    //---Connections, Configuration----------------------
    effConnectInput              :
      begin
        InputConnected(index, value <> 0);
        v := 1;
      end;
    effConnectOutput             :
      begin
        OutputConnected(index, value <> 0);
        v := 1;
      end;
    {$ENDIF} // VST_USE_DEPRECATED

    effGetInputProperties        :  v := Ord(GetInputProperties(index, ptr));
    effGetOutputProperties       :  v := Ord(GetOutputProperties(index, ptr));
    effGetPlugCategory           :  v := GetPlugCategory;

    {$IFDEF VST_USE_DEPRECATED}
    //---Realtime----------------------
    effGetCurrentPosition        :  v := ReportCurrentPosition;
    effGetDestinationBuffer      :  v := VstIntPtr(ReportDestinationBuffer);
    {$ENDIF} // VST_USE_DEPRECATED

    //---Offline----------------------
    effOfflineNotify             :  v := Ord(OfflineNotify(ptr, value, index <> 0));
    effOfflinePrepare            :  v := Ord(OfflinePrepare(ptr, value));
    effOfflineRun                :  v := Ord(OfflineRun(ptr, value));

    //---Others----------------------
    effSetSpeakerArrangement     :  v := Ord(SetSpeakerArrangement(pointer(value), ptr));
    effProcessVarIo              :  v := Ord(ProcessVariableIO(ptr));

    {$IFDEF VST_USE_DEPRECATED}
    effSetBlockSizeAndSampleRate :
      begin
        SetBlockSizeAndSampleRate(value, opt);
        v := 1;
      end;
    {$ENDIF} // VST_USE_DEPRECATED
    effSetBypass                 :  v := Ord(SetBypass(value <> 0));
    effGetEffectName             :  v := Ord(GetEffectName(ptr));
    effGetVendorString           :  v := Ord(GetVendorString(ptr));
    effGetProductString          :  v := Ord(GetProductString(ptr));
    effGetVendorVersion          :  v := GetVendorVersion;
    effVendorSpecific            :  v := VendorSpecific(index, value, ptr, opt);
    effCanDo                     :  v := CanDo(ptr);
    effGetTailSize               :  v := GetTailSize;

    {$IFDEF VST_USE_DEPRECATED}
    effGetErrorText              :  v := Ord(GetErrorText(ptr));
    effGetIcon                   :  v := VstIntPtr(GetIcon);
    effSetViewPosition           :  v := Ord(SetViewPosition(index, value));
    effIdle                      :  v := FxIdle;
    effKeysRequired              :  v := Ord(not KeysRequired);   // reversed to keep v1 compatibility
    {$ENDIF} // VST_USE_DEPRECATED

    effGetParameterProperties    :  v := Ord(GetParameterProperties(index, ptr));
    effGetVstVersion             :  v := GetVstVersion;

    //---Others----------------------
    {$IFDEF VST_2_1_EXTENSIONS}
    effEditKeyDown:
      if Assigned(FEditor) then
      begin
        keyCode.character := index;
        keyCode.virt := value;
        keyCode.modifier := Round(opt);
        v := Ord(FEditor.onKeyDown(keyCode));
      end;
    effEditKeyUp:
      if Assigned(FEditor) then
      begin
        keyCode.character := index;
        keyCode.virt := value;
        keyCode.modifier := Round(opt);
        v := Ord(FEditor.onKeyUp(keyCode));
      end;

    effSetEditKnobMode           :  if Assigned(FEditor) then  v := Ord(FEditor.SetKnobMode(value));
    effGetMidiProgramName        :  v := GetMidiProgramName(index, ptr);
    effGetCurrentMidiProgram     :  v := GetCurrentMidiProgram(index, ptr);
    effGetMidiProgramCategory    :  v := GetMidiProgramCategory(index, ptr);
    effHasMidiProgramsChanged    :  v := Ord(HasMidiProgramsChanged(index));
    effGetMidiKeyName            :  v := Ord(GetMidiKeyName(index, ptr));
    effBeginSetProgram           :  v := Ord(BeginSetProgram);
    effEndSetProgram             :  v := Ord(EndSetProgram);
    {$ENDIF} // VST_2_1_EXTENSIONS

    {$IFDEF VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement     :  v := Ord(GetSpeakerArrangement(PPVstSpeakerArrangement(value), PPVstSpeakerArrangement(ptr)));
    effSetTotalSampleToProcess   :  v := SetTotalSampleToProcess(value);
    effShellGetNextPlugin        :  v := GetNextShellPlugin(ptr);
    effStartProcess              :  v := StartProcess;
    effStopProcess               :  v := StopProcess;
    effSetPanLaw                 :  v := Ord(SetPanLaw(value, opt));
    effBeginLoadBank             :  v := BeginLoadBank(ptr);
    effBeginLoadProgram          :  v := BeginLoadProgram(ptr);
    {$ENDIF} // VST_2_3_EXTENSIONS

    {$IFDEF VST_2_4_EXTENSIONS}
    effSetProcessPrecision       :  v := Ord(SetProcessPrecision(value));
    effGetNumMidiInputChannels   :  v := GetNumMidiInputChannels;
    effGetNumMidiOutputChannels  :  v := GetNumMidiOutputChannels;
    {$ENDIF} // VST_2_4_EXTENSIONS

    //---Version 1.0 or unknown-----------------
    else
      v := inherited Dispatcher(opcode, index, value, ptr, opt);
  end;

  Result := v;
end;

function AudioEffectX.OfflineNotify(ptr: PVstAudioFile; NumAudioFiles: VstInt32; Start: boolean): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetPlugCategory: VstPlugCategory;
begin
  if FEffect.flags and effFlagsIsSynth <> 0 then
    Result := kPlugCategSynth
  else
    Result := kPlugCategUnknown;
end;

procedure AudioEffectX.NoTail(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsNoSoundInStop
  else
    FEffect.flags := FEffect.flags and not effFlagsNoSoundInStop;
end;

function AudioEffectX.CanParameterBeAutomated(Index: VstInt32): boolean;
begin
  Result := TRUE;
end;

function AudioEffectX.OfflinePrepare(Offline: PVstOfflineTask; Count: VstInt32): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetProductString(Text: PAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.GetSpeakerArrangement(PluginInput, PluginOutput: PPVstSpeakerArrangement): boolean;
begin
  PluginInput^ := nil;
  PluginOutput^ := nil;
  Result := FALSE;
end;

function AudioEffectX.OfflineGetCurrentMetaPass: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterOfflineGetCurrentMetaPass, 0, 0, nil, 0);
end;

function AudioEffectX.UpdateSampleRate: double;
var
   res : VstIntPtr;
begin
  if Assigned(FAudioMaster) then
  begin
    res := FAudioMaster(@FEffect, audioMasterGetSampleRate, 0, 0, nil, 0);
    if res > 0 then
      FSampleRate := res; 
  end;
  Result := FSampleRate;
end;

function AudioEffectX.UpdateDisplay: boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterUpdateDisplay, 0, 0, nil, 0) <> 0);
end;

function AudioEffectX.GetDirectory: pointer;
begin
  Result := nil;
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetDirectory, 0, 0, nil, 0));
end;

function AudioEffectX.OfflineGetCurrentPass: VstInt32;
begin
  Result := 0;
	if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterOfflineGetCurrentPass, 0, 0, nil, 0);
end;

(*------------------------------------------------------------------------------
  A plug-in will request time info by calling the function GetTimeInfo which
  returns a PVstTimeInfo pointer (or NIL if not implemented by the Host).
  The mask parameter is composed of the same flags which will be found in the
  flags field of VstTimeInfo when returned, that is, if you need information
  about tempo, the parameter passed to GetTimeInfo should have the
  kVstTempoValid flag set. This request and delivery system is important, as a
  request like this may cause significant calculations at the application's end,
  which may take a lot of our precious time. This obviously means you should
  only set those flags that are required to get the information you need. Also
  please be aware that requesting information does not necessarily mean that
  that information is provided in return. Check the flags field in the
  VstTimeInfo structure to see if your request was actually met.

  filter = a mask indicating which fields are requested, as some items may
           require extensive conversions.
	         See the flags in VstTimeInfo
	result = a pointer to a VstTimeInfo structure or NIL if not implemented by the
           host
------------------------------------------------------------------------------*)
function AudioEffectX.GetTimeInfo(Filter: VstInt32): PVstTimeInfo;
begin
  Result := nil;
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetTime, 0, filter, nil, 0));
end;

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.BeginEdit(Index: VstInt32): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterBeginEdit, index, 0, nil, 0) <> 0);
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.EndEdit(Index: VstInt32): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterEndEdit, index, 0, nil, 0) <> 0);
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.BeginSetProgram: boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.EndSetProgram: boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.BeginLoadBank(ptr: PVstPatchChunkInfo): VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.BeginLoadProgram(ptr: PVstPatchChunkInfo): VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.SetPanLaw(vType: VstInt32; Value: single): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_4_EXTENSIONS}
function AudioEffectX.SetProcessPrecision(Precision: VstInt32): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_4_EXTENSIONS}
function AudioEffectX.GetNumMidiInputChannels: VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_4_EXTENSIONS}
function AudioEffectX.GetNumMidiOutputChannels: VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.StartProcess: VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.StopProcess: VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.SetTotalSampleToProcess(Value: VstInt32): VstInt32;
begin
  Result := Value;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.GetMidiProgramName(Channel: VstInt32; MidiProgramName: PMidiProgramName): VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.GetCurrentMidiProgram(Channel: VstInt32; CurrentProgram: PMidiProgramName): VstInt32;
begin
  Result := -1;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.GetMidiProgramCategory(Channel: VstInt32; Category: PMidiProgramCategory): VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.HasMidiProgramsChanged(Channel: VstInt32): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.GetMidiKeyName(Channel: VstInt32; KeyName: PMidiKeyName): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.OpenFileSelector(ptr: PVstFileSelect): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) and Assigned(ptr) then
    Result := (FAudioMaster(@FEffect, audioMasterOpenFileSelector, 0, 0, ptr, 0) <> 0);
end;
{$ENDIF}

{$IFDEF VST_2_2_EXTENSIONS}
function AudioEffectX.CloseFileSelector(ptr: PVstFileSelect): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) and Assigned(ptr) then
    Result := (FAudioMaster(@FEffect, audioMasterCloseFileSelector, 0, 0, ptr, 0) <> 0);
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.GetNextShellPlugin(Name: PAnsiChar): VstInt32;
begin
  StrCopy(Name, '');
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: VstInt32): boolean;
var
   size : VstInt32;
begin
	if Arrangement <> nil then
    FreeMem(Arrangement);

	size := 2 * SizeOf(VstInt32) + nbChannels * SizeOf(VstSpeakerProperties);
	GetMem(Arrangement, size);
  if Arrangement = nil then
  begin
    Result := FALSE;
    Exit;
  end;

  FillChar(Arrangement, 0, size);
	Arrangement^.numChannels := nbChannels;
	Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): boolean;
begin
  if Arrangement <> nil then
  begin
    FreeMEm(Arrangement);
    Arrangement := nil;
  end;
  Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.CopySpeaker(Dest, Source: PVstSpeakerProperties): boolean;
begin
  if (Source = nil) or (Dest = nil) then
  begin
    Result := FALSE;
    Exit;
  end;

  StrLCopy(Dest^.name, Source^.name, 63);
  Dest^.vType     := Source^.vType;
  Dest^.azimuth   := Source^.azimuth;
  Dest^.elevation := Source^.elevation;
  Dest^.radius    := Source^.radius;
  Dest^.reserved  := Source^.reserved;
  CopyMemory(@(Dest^.future[0]), @(Source^.future[0]), 28);

  Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.MatchArrangement(var Dest: PVstSpeakerArrangement; Source: PVstSpeakerArrangement): boolean;
var
   i : VstInt32;
begin
  Result := FALSE;
  if Source = nil then
    Exit;

  if not  DeAllocateArrangement(Dest) then
    Exit;
  if not AllocateArrangement(Dest, Source^.numChannels) then
    Exit;

  Dest^.vType := Source^.vType;
  for i := 0 to Dest^.numChannels-1 do
  begin
    if not CopySpeaker(@(Dest^.speakers[i]), @(Source^.speakers[i])) then
      Exit;
  end;

  Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.WantEvents(Filter: VstInt32);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterWantMidi, 0, filter, nil, 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.TempoAt(Pos: VstInt32): VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterTempoAt, 0, pos, nil, 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetNumAutomatableParameters: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetNumAutomatableParameters, 0, 0, nil, 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetParameterQuantization: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetParameterQuantization, 0, 0, nil, 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetNumCategories: VstInt32;
begin
  Result := 1;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.copyProgram(Destination: VstInt32): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.NeedIdle: boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterNeedIdle, 0, 0, nil, 0) <> 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetPreviousPlug(Input: VstInt32): PAEffect;
begin
  Result := nil;
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetPreviousPlug, 0, 0, nil, 0));
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetNextPlug(Output: VstInt32): PAEffect;
begin
  Result := nil;
	if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetNextPlug, 0, 0, nil, 0));
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.InputConnected(Index: VstInt32; State: boolean);
begin
  {}
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.OutputConnected(Index: VstInt32; State: boolean);
begin
  {}
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.WillProcessReplacing: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
     Result := FAudioMaster(@FEffect, audioMasterWillReplaceOrAccumulate, 0, 0, nil, 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.WantAsyncOperation(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsExtIsAsync
  else
    FEffect.flags := FEffect.flags and not effFlagsExtIsAsync;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.HasExternalBuffer(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsExtHasBuffer
  else
    FEffect.flags := FEffect.flags and not effFlagsExtHasBuffer;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.ReportCurrentPosition: VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.ReportDestinationBuffer: System.psingle;
begin
  Result := nil;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.SetOutputSamplerate(SampleRate: single);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterSetOutputSampleRate, 0, 0, nil, sampleRate);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetInputSpeakerArrangement: PVstSpeakerArrangement;
begin
  Result := nil;
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetInputSpeakerArrangement, 0, 0, nil, 0));
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
  Result := nil;
	if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetOutputSpeakerArrangement, 0, 0, nil, 0));
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.OpenWindow(Window: PVstWindow): pointer;
begin
  Result := nil;
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterOpenWindow, 0, 0, window, 0));
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.CloseWindow(Window: PVstWindow): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterCloseWindow, 0, 0, window, 0) <> 0);
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffectX.SetBlockSizeAndSampleRate(aBlockSize: VstInt32; aSampleRate: single);
begin
  FBlockSize := aBlockSize;
  FSampleRate := aSampleRate;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetErrorText(Text: PAnsiChar): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.GetIcon: pointer;
begin
  Result := nil;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.SetViewPosition(x, y: VstInt32): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.FxIdle: VstInt32;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffectX.KeysRequired: boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED} {$IFDEF VST_2_2_EXTENSIONS}
function AudioEffectX.GetChunkFile(NativePath: PAnsiChar): boolean;
begin
  Result := FALSE;
  if Assigned(FAudioMaster) and Assigned(NativePath) then
    Result := (FAudioMaster(@FEffect, audioMasterGetChunkFile, 0, 0, nativePath, 0) <> 0);
end;
{$ENDIF} {$ENDIF}

end.
