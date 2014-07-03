unit uAudioMaster;

interface

uses
  DAEffect, DAEffectX,
  SyncObjs, uVstWrapper;

type
  PPlugInfo = ^TPlugInfo;
  TPlugInfo = class
  public
    Effect:PAEffect;
    VstWrapper:TVstWrapper;
  end;




procedure AddPlugReference(aEffect:PAEffect; aWrapper:Pointer);
procedure RemovePlugReference(aEffect:PAEffect);

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: longint; ptr: pointer;
                                        opt: Single): longint; cdecl;

var
  LocalSampleRate:single;
  LocalBlockSize:integer;

  CreatePlugLock:TCriticalSection;
  CreatingPlug:boolean;
  CreatePlugInfo:TPlugInfo;


implementation

uses
  SysUtils, Vcl.Dialogs, Contnrs;

var
  Plugs:TObjectList;


function FindPlugInfo(aEffect:PAEffect):TPlugInfo;
var
  c1:integer;
  PlugInfo:TPlugInfo;
begin
  //Look for matching plug info...
  for c1 := 0 to Plugs.Count - 1 do
  begin
    PlugInfo := (Plugs[c1] as TPlugInfo);
    if PlugInfo.Effect = aEffect then
    begin
      result := PlugInfo;
      exit;  //===============================================>
    end;
  end;

  //If control makes it this far, no matching plug info has been found. Return nil.
  result := nil;

end;




function AudioMasterCallBack(effect: PAEffect; opcode, index, value: Integer;
  ptr: pointer; opt: Single): longint; cdecl;
var
  s:shortstring;
  PlugInfo:TPlugInfo;
  p:Pointer;
begin
  //default result. (Usually means the opcode is not supported.
  result := 0;

  //Check to see if the plug exists in the plug list. This process links the
  //AudioMaster call to a specific vst wrapper/host combo.
  PlugInfo := FindPlugInfo(effect);

  //If the vst plugin reference returns nil, check to see if a plugin is currently
  //being created, if so we can assume those details (vst wrapper + host) are valid.
  if (PlugInfo = nil) and (CreatingPlug) then
  begin
    CreatePlugInfo.Effect := Effect;
    PlugInfo := CreatePlugInfo;
  end;

  assert(assigned(PlugInfo));
  assert(assigned(PlugInfo.VstWrapper));

  //if OpCode <> 7 then ShowMessage('OpCode = ' + IntToStr(OpCode));

  case OpCode of
    //---from here VST 1.0 opcodes------------------------------------------------------
    audioMasterAutomate:  result := 0;  //Respond to parameter changes here.
    audioMasterVersion:   result := 2;
    audioMasterCurrentId: result := Effect.uniqueID;
    audioMasterIdle:      result := 0;  //ignore this. I'm not sure if it is needed.
    audioMasterPinConnected: result := 0; //Result = 0; All pins always connected. May change this in future.

    //---from here VST 2.0 extension opcodes------------------------------------------------------
    // VstEvents + VstTimeInfo
    audioMasterWantMidi: PlugInfo.VstWrapper.WantsMidi := false;
    audioMasterGetTime:
    begin
      if PlugInfo.VstWrapper.IsTimeInfoValid then
      begin
        p := PlugInfo.VstWrapper.TimeInfo;
        result := LongInt(p); //The TimeInfo pointer needs to be returned as a LongInt.
      end else
      begin
        result := 0;
      end;
    end;

    audioMasterProcessEvents: PlugInfo.VstWrapper.amProcessEvents(ptr);

    audioMasterSetTime: result := 0;  //not supported
    audioMasterTempoAt: result := PlugInfo.VstWrapper.amGetTempoAt(Value);

    // parameters
    audioMasterGetNumAutomatableParameters: result := 0; //This has been deprecated.
    audioMasterGetParameterQuantization: result := 1; //Host maintains full precision.

    audioMasterIOChanged:
    begin
      PlugInfo.VstWrapper.amIOChanged;
      result := 1;
    end;

    audioMasterNeedIdle:  PlugInfo.VstWrapper.NeedsIdleCalls := true;
    audioMasterSizeWindow: PlugInfo.VstWrapper.amDisplaySizeChanged(Index,Value);
    audioMasterGetSampleRate: result := round(PlugInfo.VstWrapper.SampleRate);
    audioMasterGetBlockSize:  result := PlugInfo.VstWrapper.BlockSize;
    audioMasterGetInputLatency: result := 0;
    audioMasterGetOutputLatency:  result := 0;
    audioMasterGetPreviousPlug: result := 0;
    audioMasterGetNextPlug: result := 0;

    // realtime info
    audioMasterWillReplaceOrAccumulate: ; //Deprecated
    audioMasterGetCurrentProcessLevel: result := 0;
    audioMasterGetAutomationState: result := 0;

    // offline
    audioMasterOfflineStart: ;  //off line stuff isn't supported.
    audioMasterOfflineRead:;
    audioMasterOfflineWrite:;
    audioMasterOfflineGetCurrentPass:;
    audioMasterOfflineGetCurrentMetaPass:;

    // other
    audioMasterSetOutputSampleRate:; //Deprecated
    audioMasterGetOutputSpeakerArrangement:; //Not used
    audioMasterGetVendorString:
    begin
      s := PlugInfo.VstWrapper.HostVendor;
      if Length(s) > 64 then SetLength(s,64);
      ptr := @s[1];
    end;

    audioMasterGetProductString:
    begin
      s := PlugInfo.VstWrapper.HostName;
      if Length(s) > 64 then SetLength(s,64);
      ptr := @s[1];
    end;

    audioMasterGetVendorVersion: result := PlugInfo.VstWrapper.HostVersion;

    audioMasterVendorSpecific:;
    audioMasterSetIcon:;

    audioMasterCanDo:
    begin
      s := String(PChar(ptr));
      result := PlugInfo.VstWrapper.amHostCanDo(s);
    end;


    audioMasterGetLanguage:;
    audioMasterOpenWindow:;
    audioMasterCloseWindow:;
    audioMasterGetDirectory:;
    audioMasterUpdateDisplay:;

    //---from here VST 2.1 extension opcodes------------------------------------------------------
    audioMasterBeginEdit:;
    audioMasterEndEdit:;
    audioMasterOpenFileSelector:;

    //---from here VST 2.2 extension opcodes------------------------------------------------------
    audioMasterCloseFileSelector:;
    audioMasterEditFile:;
    audioMasterGetChunkFile:;

    //---from here VST 2.3 extension opcodes------------------------------------------------------
    audioMasterGetInputSpeakerArrangement:;

  end;



end;


procedure AddPlugReference(aEffect:PAEffect; aWrapper:Pointer);
var
  PlugInfo:TPlugInfo;
begin
  assert(aEffect <> nil);
  assert(aWrapper <> nil);

  //Do some type checking here so the units defining the types can be added
  //to the 'uses' clause in the implementation section.
  //Also by checking the type here and leaving the method declaration very generic
  //allows things to be easily changed to support more plugin types in future.
  assert(TObject(aWrapper^) is TVstWrapper);

  PlugInfo := TPlugInfo.Create;

  PlugInfo.Effect := aEffect;
  PlugInfo.VstWrapper := (TObject(aWrapper^) as TVstWrapper);

  Plugs.Add(PlugInfo);

  //TODO: Could add a check to ensure the plugin hasn't already been added
  //to the plugs list.
end;

procedure RemovePlugReference(aEffect:PAEffect);
var
  c1:integer;
  PlugInfo:TPlugInfo;
begin
  for c1 := 0 to Plugs.Count - 1 do
  begin
    PlugInfo := (Plugs[c1] as TPlugInfo);
    if PlugInfo.Effect = aEffect then
    begin
      Plugs.Delete(c1);
      exit;  //===============================================>
    end;
  end;

end;

initialization
  CreatePlugLock := TCriticalSection.Create;
  CreatePlugInfo := TPlugInfo.Create;
  CreatingPlug := false;

  Plugs := TObjectList.Create;
  Plugs.OwnsObjects := true;


finalization
  CreatePlugLock.Free;
  CreatePlugInfo.Free;
  Plugs.Free;

end.
