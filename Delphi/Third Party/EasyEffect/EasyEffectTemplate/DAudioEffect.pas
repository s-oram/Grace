//******************************************************************************
//
//  DAudioEffect.pas
//  8 December 2006
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
// Filename     : audioeffect.h
// Created by   : Steinberg Media Technologies
// Description  : Class AudioEffect (VST 1.0)
//
// © 2006, Steinberg Media Technologies, All Rights Reserved
//
//------------------------------------------------------------------------------
//
//  Classes : AudioEffect
//            AEffEditor
//
//  Functions : dispatchEffectClass
//              getParameterClass
//              setParameterClass
//              processClass
//              processClassReplacing
//
//******************************************************************************
unit
    DAudioEffect;

interface

uses
    DAEffect, DAEffectX;

{$INCLUDE DVstCommon.inc}

//------------------------------------------------------------------------------
// VST Effect Base Class (VST 1.0)
//------------------------------------------------------------------------------
type
    AEffEditor = class;

    AudioEffect = class
    private
      procedure SetEditor(newvalue: AEffEditor);

    protected
      // members
      FAudioMaster : TAudioMasterCallbackFunc;    // Host callback
      FEffect      : AEffect;
      FEditor      : AEffEditor;
      FSampleRate  : single;
      FBlockSize   : VstInt32;
      NumPrograms  : VstInt32;
      NumParams    : VstInt32;
      CurProgram   : VstInt32;
      procedure SetSampleRate(NewValue: single); virtual;           // Called when the sample rate changes (always in a suspend state)
      procedure SetBlockSize(NewValue: VstInt32); virtual;          // Called when the Maximun block size changes (always in a suspend state). Note that the sampleFrames in Process Calls could be smaller than this block size, but NOT bigger.
      function GetEffect: PAEffect;

    public
      constructor Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32);
      destructor Destroy; override;

      function Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; virtual;     // Opcodes dispatcher

      // State Transitions
      procedure Open; virtual;      // Called when plug-in is initialized
      procedure Close; virtual;     // Called when plug-in will be released
      procedure Suspend; virtual;   // Called when plug-in is switched to off
      procedure Resume; virtual;    // Called when plug-in is switched to on

      // Processing
      procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); virtual; abstract;   // Process 32 bit (single precision) floats (always in a resume state)
      {$IFDEF VST_2_4_EXTENSIONS}
      procedure ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: VstInt32); virtual;       // Process 64 bit (double precision) floats (always in a resume state)
      {$ENDIF} // VST_2_4_EXTENSIONS

      // Parameters
      procedure SetParameter(Index: VstInt32; Value: single); virtual;            // Called when a parameter changed
      function GetParameter(Index: integer): single; virtual;                     // Return the value of the parameter with index
      procedure SetParameterAutomated(Index: VstInt32; Value: single); virtual;   // Called after a control has changed in the editor and when the associated parameter should be automated

      // Programs and Persistence
      function GetProgram: VstInt32; virtual;                                                             // Return the index to the current program
      procedure SetProgram(aProgram: VstInt32); virtual;                                                  // Set the current program to program

      procedure SetProgramName(Name: PAnsiChar); virtual;                                                     // Stuff the name field of the current program with name. Limited to kVstMaxProgNameLen.
      procedure GetProgramName(Name: PAnsiChar); virtual;                                                     // Stuff name with the name of the current program. Limited to kVstMaxProgNameLen.

      procedure GetParameterLabel(Index: VstInt32; aLabel: PAnsiChar); virtual;                               // Stuff label with the units in which parameter index is displayed (i.e. "sec", "dB", "type", etc...). Limited to kVstMaxParamStrLen.
      procedure GetParameterDisplay(Index: VstInt32; Text: PAnsiChar); virtual;                               // Stuff text with a string representation ("0.5", "-3", "PLATE", etc...) of the value of parameter index. Limited to kVstMaxParamStrLen.
      procedure GetParameterName(Index: VstInt32; Text: PAnsiChar); virtual;                                  // Stuff text with the name ("Time", "Gain", "RoomType", etc...) of parameter index. Limited to kVstMaxParamStrLen.

      function GetChunk(var Data: pointer; IsPreset: boolean = FALSE): VstInt32; virtual;                 // Host stores plug-in state. Returns the size in bytes of the chunk (plug-in allocates the data array)
      function SetChunk(Data: pointer; ByteSize: VstInt32; IsPreset: boolean = FALSE): VstInt32; virtual; // Host restores plug-in state

      // Internal Setup
      procedure SetUniqueID(ID: VstInt32); virtual;                  // Must be called to set the plug-ins unique ID!
      procedure SetNumInputs(Inputs: VstInt32); virtual;             // Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available inputs.
      procedure SetNumOutputs(Outputs: VstInt32); virtual;           // Set the number of outputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available ouputs.

      procedure CanProcessReplacing(State: boolean); virtual;        // Tells that ProcessReplacing could be used. Mandatory in VST 2.4!

      {$IFDEF VST_2_4_EXTENSIONS}
      procedure CanDoubleReplacing(State: boolean); virtual;         // Tells that processDoubleReplacing() is implemented.
      {$ENDIF} // VST_2_4_EXTENSIONS

      procedure ProgramsAreChunks(State: boolean); virtual;          // Program data is handled in formatless chunks (using getChunk-setChunks)
      procedure SetInitialDelay(Delay: VstInt32); virtual;           // Use to report the plug-in's latency (Group Delay)

      // Host Communication
      function GetMasterVersion: VstInt32; virtual;                    // Returns the Host's version (for example 2400 for VST 2.4)
      function GetCurrentUniqueId: VstInt32; virtual;                  // Returns current unique identifier when loading shell plug-ins
      procedure MasterIdle; virtual;                                   // Give idle time to Host application

      {$IFDEF VST_USE_DEPRECATED}
      // Deprecated methods
      procedure Process(Inputs, Outputs: ppsingle; SampleFrames: VstInt32); virtual;
      function GetVu: single; virtual;
      procedure HasVu(State: boolean); virtual;
      procedure HasClip(State: boolean); virtual;
      procedure CanMono(State: boolean); virtual;
      procedure SetRealtimeQualities(Qualities: VstInt32); virtual;
      procedure SetOfflineQualities(Qualities: VstInt32); virtual;
      function IsInputConnected(Input: VstInt32): boolean; virtual;    // Returns the input's connection state
      function IsOutputConnected(Output: VstInt32): boolean; virtual;  // Returns the output's connection state      
      {$ENDIF}

      // properties
      property AudioMaster : TAudioMasterCallbackFunc read FAudioMaster;    // Host callback
      property Effect: PAEffect read GetEffect;
      property Editor: AEffEditor read fEditor write SetEditor;
      property SampleRate: Single read fSampleRate write setSampleRate;
      property BlockSize: longint read fBlockSize write setBlockSize;
    end;

    AEffEditor = class
    protected
      FEffect      : AudioEffect;
      SystemWindow : pointer;
    public
      constructor Create(aEffect: AudioEffect); virtual;

      function GetRect(var rect: PERect): longint; virtual;
      function Open(ptr: pointer): longint; virtual;
      procedure Close; virtual;
      function IsOpen: boolean; virtual;
      procedure Idle; virtual;

      {$IFDEF VST_2_1_EXTENSIONS}
      function OnKeyDown(var KeyCode: VstKeyCode): boolean; virtual;
      function OnKeyUp(var KeyCode: VstKeyCode): boolean; virtual;
      function SetKnobMode(Val: VstInt32): boolean; virtual;
      function OnWheel(Distance: single): boolean; virtual;
      {$ENDIF}

      property Effect: AudioEffect read FEffect;
    end;    


function DispatchEffectClass(e: PAEffect; Opcode, Index: VstInt32; Value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; cdecl;
function GetParameterClass(e: PAEffect; Index: VstInt32): single; cdecl;
procedure SetParameterClass(e: PAEffect; Index: VstInt32; Value: single); cdecl;
{$IFDEF VST_USE_DEPRECATED}
procedure processClass(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
{$ENDIF} // VST_USE_DEPRECATED
procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
{$IFDEF VST_2_4_EXTENSIONS}
procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;
{$ENDIF} // VST_2_4_EXTENSIONS




implementation

uses
    SysUtils, DVstUtils;

function DispatchEffectClass(e: PAEffect; Opcode, Index: VstInt32; Value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; cdecl;
var
   obj : AudioEffect;
begin
  obj := e^.vObject;

  if opCode = effClose then
  begin
    obj.Dispatcher(opCode, index, value, ptr, opt);
    obj.Free;
    Result := 1;
    Exit;
  end;

  Result := obj.dispatcher(opCode, index, value, ptr, opt);
end;

function GetParameterClass(e: PAEffect; Index: VstInt32): single; cdecl;
begin
  Result := AudioEffect(e^.vObject).GetParameter(index);
end;

procedure SetParameterClass(e: PAEffect; Index: VstInt32; Value: single); cdecl;
begin
  AudioEffect(e^.vObject).SetParameter(Index, Value);
end;

{$IFDEF VST_USE_DEPRECATED}
procedure processClass(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
begin
  AudioEffect(e^.vObject).Process(Inputs, Outputs, SampleFrames);
end;
{$ENDIF} // VST_USE_DEPRECATED

procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
begin
  AudioEffect(e^.vObject).ProcessReplacing(Inputs, Outputs, SampleFrames);
end;

{$IFDEF VST_2_4_EXTENSIONS}
procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;
begin
  AudioEffect(e^.vObject).ProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
end;
{$ENDIF} // VST_2_4_EXTENSIONS


{ AudioEffect }

function AudioEffect.GetMasterVersion: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, AudioMasterVersion, 0, 0, nil, 0);

  if Result = 0 then  // old
    Result := 1;
end;

constructor AudioEffect.Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32);
begin
  inherited Create;

  FAudioMaster := anAudioMaster;
  FEditor := nil;
  FSampleRate := 44100;
  FBlockSize := 1024;
  NumPrograms := aNumPrograms;
  NumParams := aNumParams;
  CurProgram := 0;

  FillChar(FEffect, SizeOf(AEffect), 0);

	FEffect.magic := FourCharToLong(kEffectMagic[1], kEffectMagic[2], kEffectMagic[3], kEffectMagic[4]);
	FEffect.dispatcher := dispatchEffectClass;
  {$IFDEF VST_USE_DEPRECATED}
	FEffect.process := processClass;
  {$ENDIF}
	FEffect.setParameter := setParameterClass;
	FEffect.getParameter := getParameterClass;
	FEffect.numPrograms := numPrograms;
	FEffect.numParams := numParams;
	FEffect.numInputs := 1;		// mono input
	FEffect.numOutputs := 2;		// stereo output
  {$IFDEF VST_USE_DEPRECATED}
	FEffect.ioRatio := 1.0;
  {$ENDIF}
	FEffect.vObject := Self;
	FEffect.uniqueID := FourCharToLong('N', 'o', 'E', 'f');  // you must set this in your descendant class!
	FEffect.version  := 1;
	FEffect.processReplacing := processClassReplacing;

  {$IFDEF VST_2_4_EXTENSIONS}
	canProcessReplacing(TRUE); // mandatory in VST 2.4!
	FEffect.processDoubleReplacing := processClassDoubleReplacing;
  {$ENDIF}
end;

destructor AudioEffect.Destroy;
begin
  if Assigned(FEditor) then
  begin
    FEditor.Free;
    FEditor := nil;
  end;

  inherited;
end;

procedure AudioEffect.SetSampleRate(NewValue: single);
begin
  FSampleRate := NewValue;
end;

procedure AudioEffect.SetBlockSize(NewValue: VstInt32);
begin
  FBlockSize := NewValue;
end;

procedure AudioEffect.SetProgram(aProgram: VstInt32);
begin
  CurProgram := aProgram;
end;

procedure AudioEffect.SetProgramName(Name: PAnsiChar);
begin
  {}
end;

procedure AudioEffect.MasterIdle;
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterIdle, 0, 0, nil, 0);
end;

function AudioEffect.GetChunk(var Data: pointer; IsPreset: boolean): VstInt32;
begin
  Result := 0;
end;

function AudioEffect.SetChunk(Data: pointer; ByteSize: VstInt32; IsPreset: boolean): VstInt32;
begin
  Result := 0;
end;

procedure AudioEffect.Open;
begin
  {}
end;

procedure AudioEffect.Close;
begin
  {}
end;

function AudioEffect.GetEffect: PAEffect;
begin
  Result := @FEffect;
end;

procedure AudioEffect.Resume;
begin
  {}
end;

procedure AudioEffect.Suspend;
begin
  {}
end;

procedure AudioEffect.ProgramsAreChunks(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsProgramChunks
  else
    FEffect.flags := FEffect.flags and not effFlagsProgramChunks;
end;

function AudioEffect.GetCurrentUniqueId: VstInt32;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterCurrentId, 0, 0, nil, 0);
end;

{$IFDEF VST_USE_DEPRECATED}
function AudioEffect.IsInputConnected(Input: VstInt32): boolean;
begin
  Result := TRUE;
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterPinConnected, Input, 0, nil, 0) = 0);    // return value is 0 for true
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffect.IsOutputConnected(Output: VstInt32): boolean;
begin
  Result := TRUE;
	if Assigned(FAudioMaster) then
		Result := (FAudioMaster(@FEffect, audioMasterPinConnected, Output, 1, nil, 0) = 0);    // return value is 0 for true
end;
{$ENDIF}

procedure AudioEffect.SetInitialDelay(Delay: VstInt32);
begin
  FEffect.initialDelay := Delay;
end;

procedure AudioEffect.CanProcessReplacing(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsCanReplacing
  else
    FEffect.flags := FEffect.flags and not effFlagsCanReplacing;
end;

procedure AudioEffect.SetNumOutputs(Outputs: VstInt32);
begin
  FEffect.numOutputs := Outputs;
end;

function AudioEffect.Dispatcher(opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr;
var
   v  : VstIntPtr;
   pe : PERect;
begin
  v := 0;

  case opcode of
    effOpen             :	 Open;
    effClose            :  Close;
    effSetProgram       :  if value < NumPrograms then
                             SetProgram (value);
    effGetProgram       :  v := GetProgram;
    effSetProgramName   :  SetProgramName(ptr);
    effGetProgramName   :  GetProgramName (ptr);
    effGetParamLabel    :  GetParameterLabel(index, ptr);
    effGetParamDisplay  :  GetParameterDisplay(index, ptr);
    effGetParamName     :  GetParameterName(index, ptr);

    effSetSampleRate    :  SetSampleRate(opt);
    effSetBlockSize     :  SetBlockSize(value);
    effMainsChanged     :  if value = 0 then
                             Suspend
                           else
                             Resume;
    {$IFDEF VST_USE_DEPRECATED}
    effGetVu            :  v := Round(GetVu * 32767);
    {$ENDIF}

    //---Editor------------
    effEditGetRect      :  if Assigned(FEditor) then
                           begin
                             pe := PPERect(ptr)^;
                             v := FEditor.GetRect(pe);
                             PPERect(ptr)^ := pe;
                           end;
    effEditOpen         :  if Assigned(FEditor) then
                             v := FEditor.Open(ptr);
    effEditClose        :  if Assigned(FEditor) then
                             FEditor.Close;
    effEditIdle         :  if Assigned(FEditor) then
                             FEditor.Idle;


    {$IFDEF VST_USE_DEPRECATED}
    // effIdentify has been depreciated since VST 2.4.
    // I've re-enabled it because MuLab expects the NvEf value to be set.
    effIdentify         :  v := FourCharToLong('N', 'v', 'E', 'f');
    {$ENDIF}

    //---Persistence-------
    effGetChunk         :  v := GetChunk(ppointer(ptr)^, (index <> 0));
    effSetChunk         :  v := SetChunk(ptr, value, (index <> 0));
  end;

  Result := v;
end;

procedure AudioEffect.SetEditor(NewValue: AEffEditor);
begin
  FEditor := NewValue;
  if Assigned(FEditor) then
    FEffect.flags := FEffect.flags or effFlagsHasEditor
  else
    FEffect.flags := FEffect.flags and not effFlagsHasEditor;
end;

procedure AudioEffect.SetNumInputs(Inputs: VstInt32);
begin
  FEffect.numInputs := Inputs;
end;

procedure AudioEffect.SetUniqueID(ID: VstInt32);
begin
  FEffect.uniqueID := ID;
end;

function AudioEffect.GetParameter(Index: integer): single;
begin
  Result := 0;
end;

procedure AudioEffect.SetParameter(Index: VstInt32; Value: single);
begin
  {}
end;

procedure AudioEffect.SetParameterAutomated(Index: VstInt32; Value: single);
begin
  SetParameter(Index, Value);
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterAutomate, Index, 0, nil, Value);    // value is in opt
end;

procedure AudioEffect.GetParameterName(Index: VstInt32; Text: PAnsiChar);
begin
  StrCopy(Text, '');
end;

procedure AudioEffect.GetParameterDisplay(Index: VstInt32; Text: PAnsiChar);
begin
  StrCopy(Text, '');
end;

procedure AudioEffect.GetParameterLabel(Index: VstInt32; aLabel: PAnsiChar);
begin
  StrCopy(aLabel, '');
end;

function AudioEffect.GetProgram: VstInt32;
begin
  Result := CurProgram;
end;

procedure AudioEffect.GetProgramName(Name: PAnsiChar);
begin
  StrCopy(Name, '');
end;

{$IFDEF VST_2_4_EXTENSIONS}
procedure AudioEffect.ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: VstInt32);
begin
  {}
end;
{$ENDIF} // VST_2_4_EXTENSIONS

{$IFDEF VST_2_4_EXTENSIONS}
procedure AudioEffect.CanDoubleReplacing(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsCanDoubleReplacing
  else
    FEffect.flags := FEffect.flags and not effFlagsCanDoubleReplacing;
end;
{$ENDIF} // VST_2_4_EXTENSIONS

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffect.Process(Inputs, Outputs: ppsingle; SampleFrames: VstInt32);
begin
  {}
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
function AudioEffect.GetVu: single;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffect.HasVu(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsHasVu
  else
    FEffect.flags := FEffect.flags and not effFlagsHasVu;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffect.HasClip(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsHasClip
  else
    FEffect.flags := FEffect.flags and not effFlagsHasClip;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffect.CanMono(State: boolean);
begin
  if State then
    FEffect.flags := FEffect.flags or effFlagsCanMono
  else
    FEffect.flags := FEffect.flags and not effFlagsCanMono;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffect.SetRealtimeQualities(Qualities: VstInt32);
begin
  FEffect.realQualities := Qualities;
end;
{$ENDIF}

{$IFDEF VST_USE_DEPRECATED}
procedure AudioEffect.SetOfflineQualities(Qualities: VstInt32);
begin
  FEffect.offQualities := Qualities;
end;
{$ENDIF}

{ AEffEditor }

function AEffEditor.GetRect(var rect: PERect): longint;
begin
  rect := nil;
  Result := 0;
end;

procedure AEffEditor.Idle;
begin
  {}
end;

constructor AEffEditor.Create(aEffect: AudioEffect);
begin
  inherited Create;

  FEffect := aEffect;
  SystemWindow := nil;
end;

function AEffEditor.IsOpen: boolean;
begin
  Result := (SystemWindow <> nil);
end;

function AEffEditor.Open(ptr: pointer): longint;
begin
  SystemWindow := ptr;
  Result := 0;
end;

procedure AEffEditor.Close;
begin
  SystemWindow := nil;
end;

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.OnKeyDown(var KeyCode: VstKeyCode): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.OnKeyUp(var KeyCode: VstKeyCode): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.SetKnobMode(Val: VstInt32): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.OnWheel(Distance: single): boolean; 
begin
  Result := FALSE;
end;
{$ENDIF}

end.
