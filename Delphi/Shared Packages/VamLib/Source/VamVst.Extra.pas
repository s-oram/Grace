unit VamVst.Extra;

interface

function VstDispatcherOpcodeToStr(Opcode:integer):string;
function VstAudioMasterOpcodeToStr(Opcode:integer):string;


implementation

uses
  SysUtils;

const
  //------------------------------------------------------------------------------
  // VST 2.x dispatcher Opcodes (Plug-in to Host). Extension of AudioMasterOpcodes
  //------------------------------------------------------------------------------
  audioMasterAutomate     = 0;  // [index]: parameter index [opt]: parameter value. See AudioEffect.setParameterAutomated
  audioMasterVersion      = 1;  // [return value]: Host VST version (for example 2400 for VST 2.4). See AudioEffect.getMasterVersion
  audioMasterCurrentId    = 2;  // [return value]: current unique identifier on shell plug-in. See AudioEffect.getCurrentUniqueId
  audioMasterIdle         = 3;  // no arguments. See AudioEffect.masterIdle

  audioMasterPinConnected = 4;  // (deprecated) [return value]: 0=true, 1=false [index]: pin index [value]: 0=input, 1=output. See AudioEffect.isInputConnected & AudioEffect.isOutputConnected

  audioMasterX = audioMasterIdle+1;  // place holder to define the constants below

  audioMasterGetTime                     = audioMasterX + 3;  // [return value] PVstTimeInfo or nil if not supported [value] request mask. See VstTimeInfoFlags & AudioEffectX.getTimeInfo
  audioMasterProcessEvents               = audioMasterX + 4;  // [ptr] pointer to VstEvents. See VstEvents @see AudioEffectX.sendVstEventsToHost

  audioMasterIOChanged                   = audioMasterX + 9;  // [return value] 1 if supported. See AudioEffectX.ioChanged

  audioMasterSizeWindow                  = audioMasterX + 11; // [index] new width [value] new height [return value] 1 if supported. See AudioEffectX.sizeWindow
  audioMasterGetSampleRate               = audioMasterX + 12; // [return value] current sample rate. See AudioEffectX.updateSampleRate
  audioMasterGetBlockSize                = audioMasterX + 13; // [return value] current block size. See AudioEffectX.updateBlockSize
  audioMasterGetInputLatency             = audioMasterX + 14; // [return value] input latency in audio samples. See AudioEffectX.getInputLatency
  audioMasterGetOutputLatency            = audioMasterX + 15; // [return value] output latency in audio samples. See AudioEffectX.getOutputLatency

  audioMasterGetCurrentProcessLevel      = audioMasterX + 19; // [return value] current process level. See VstProcessLevels
  audioMasterGetAutomationState          = audioMasterX + 20; // [return value] current automation state. See VstAutomationStates

  audioMasterOfflineStart                = audioMasterX + 21; // [index] numNewAudioFiles [value] numAudioFiles [ptr] PVstAudioFile. See AudioEffectX.offlineStart
  audioMasterOfflineRead                 = audioMasterX + 22; // [index] bool readSource [value] PVstOfflineOption. See VstOfflineOption [ptr]: PVstOfflineTask. See VstOfflineTask & AudioEffectX.offlineRead
  audioMasterOfflineWrite                = audioMasterX + 23; // see audioMasterOfflineRead & AudioEffectX.offlineRead
  audioMasterOfflineGetCurrentPass       = audioMasterX + 24; // see AudioEffectX.offlineGetCurrentPass
  audioMasterOfflineGetCurrentMetaPass   = audioMasterX + 25; // see AudioEffectX.offlineGetCurrentMetaPass

  audioMasterGetVendorString             = audioMasterX + 28; // [ptr] AnsiChar buffer for vendor string, limited to kVstMaxVendorStrLen. See AudioEffectX.getHostVendorString
  audioMasterGetProductString            = audioMasterX + 29; // [ptr] AnsiChar buffer for vendor string, limited to kVstMaxProductStrLen. See AudioEffectX.getHostProductString
  audioMasterGetVendorVersion            = audioMasterX + 30; // [return value] vendor-specific version. See AudioEffectX.getHostVendorVersion
  audioMasterVendorSpecific              = audioMasterX + 31; // no definition, vendor specific handling. See AudioEffectX.hostVendorSpecific

  audioMasterCanDo                       = audioMasterX + 33; // [ptr] "can do" string [return value] 1 for supported
  audioMasterGetLanguage                 = audioMasterX + 34; // [return value] language code. See VstHostLanguage

  audioMasterGetDirectory                = audioMasterX + 37; // [return value] FSSpec on MacOS, else PAnsiChar. See AudioEffectX.getDirectory
  audioMasterUpdateDisplay               = audioMasterX + 38; // no arguments
  audioMasterBeginEdit                   = audioMasterX + 39; // [index] parameter index. See AudioEffectX.beginEdit
  audioMasterEndEdit                     = audioMasterX + 40; // [index] parameter index. See AudioEffectX.endEdit
  audioMasterOpenFileSelector            = audioMasterX + 41; // [ptr] PVstFileSelect [return value] 1 if supported. See AudioEffectX.openFileSelector
  audioMasterCloseFileSelector           = audioMasterX + 42; // [ptr] PVstFileSelect. See AudioEffectX.closeFileSelector

  audioMasterWantMidi                    = audioMasterX + 2;  // (deprecated) deprecated in VST 2.4
  audioMasterSetTime                     = audioMasterX + 5;  // (deprecated) deprecated in VST 2.4
  audioMasterTempoAt                     = audioMasterX + 6;  // (deprecated) deprecated in VST 2.4
  audioMasterGetNumAutomatableParameters = audioMasterX + 7;  // (deprecated) deprecated in VST 2.4
  audioMasterGetParameterQuantization    = audioMasterX + 8;  // (deprecated) deprecated in VST 2.4
  audioMasterNeedIdle                    = audioMasterX + 10; // (deprecated) deprecated in VST 2.4
  audioMasterGetPreviousPlug             = audioMasterX + 16; // (deprecated) deprecated in VST 2.4
  audioMasterGetNextPlug                 = audioMasterX + 17; // (deprecated) deprecated in VST 2.4
  audioMasterWillReplaceOrAccumulate     = audioMasterX + 18; // (deprecated) deprecated in VST 2.4
  audioMasterSetOutputSampleRate         = audioMasterX + 26; // (deprecated) deprecated in VST 2.4
  audioMasterGetOutputSpeakerArrangement = audioMasterX + 27; // (deprecated) deprecated in VST 2.4
  audioMasterSetIcon                     = audioMasterX + 32; // (deprecated) deprecated in VST 2.4
  audioMasterOpenWindow                  = audioMasterX + 35; // (deprecated) deprecated in VST 2.4
  audioMasterCloseWindow                 = audioMasterX + 36; // (deprecated) deprecated in VST 2.4
  audioMasterEditFile                    = audioMasterX + 43; // (deprecated) deprecated in VST 2.4
  audioMasterGetChunkFile                = audioMasterX + 44; // (deprecated) deprecated in VST 2.4 [ptr] AnsiChar[2048] or sizeof(FSSpec) [return value] 1 if supported. See AudioEffectX.getChunkFile
  audioMasterGetInputSpeakerArrangement  = audioMasterX + 45; // (deprecated) deprecated in VST 2.4

  //------------------------------------------------------------------------------
  // VST 2.x dispatcher Opcodes (Host to Plug-in). Extension of AEffectOpcodes
  //------------------------------------------------------------------------------
  effOpen            = 0;
  effClose           = 1;
  effSetProgram	    = 2;
  effGetProgram      = 3;
  effSetProgramName  = 4;
  effGetProgramName  = 5;
  effGetParamLabel   = 6;
  effGetParamDisplay = 7;
  effGetParamName    = 8;
  effGetVu           = 9;
  effSetSampleRate   = 10;
  effSetBlockSize    = 11;
  effMainsChanged    = 12;
  effEditGetRect     = 13;
  effEditOpen        = 14;
  effEditClose       = 15;
  effEditDraw        = 16;
  effEditMouse       = 17;
  effEditKey         = 18;
  effEditIdle        = 19;
  effEditTop         = 20;
  effEditSleep       = 21;
  effIdentify        = 22;
  effGetChunk        = 23;
  effSetChunk        = 24;
  effNumOpcodes      = 25;
  effProcessEvents   = effSetChunk + 1;
  effCanBeAutomated = effSetChunk + 2;
  effString2Parameter = effSetChunk + 3;
  effGetNumProgramCategories = effSetChunk + 4;
  effGetProgramNameIndexed = effSetChunk + 5;
  effCopyProgram = effSetChunk + 6;
  effConnectInput = effSetChunk + 7;
  effConnectOutput = effSetChunk + 8;
  effGetInputProperties = effSetChunk + 9;
  effGetOutputProperties = effSetChunk + 10;
  effGetPlugCategory = effSetChunk + 11;
  effGetCurrentPosition = effSetChunk + 12;
  effGetDestinationBuffer = effSetChunk + 13;
  effOfflineNotify = effSetChunk + 14;
  effOfflinePrepare = effSetChunk + 15;
  effOfflineRun = effSetChunk + 16;
  effProcessVarIo = effSetChunk + 17;
  effSetSpeakerArrangement = effSetChunk + 18;
  effSetBlockSizeAndSampleRate = effSetChunk + 19;
  effSetBypass = effSetChunk + 20;
  effGetEffectName = effSetChunk + 21;
  effGetErrorText = effSetChunk + 22;
  effGetVendorString = effSetChunk + 23;
  effGetProductString = effSetChunk + 24;
  effGetVendorVersion = effSetChunk + 25;
  effVendorSpecific = effSetChunk + 26;
  effCanDo = effSetChunk + 27;
  effGetTailSize = effSetChunk + 28;
  effIdle = effSetChunk + 29;
  effGetIcon = effSetChunk + 30;
  effSetViewPosition = effSetChunk + 31;
  effGetParameterProperties = effSetChunk + 32;
  effKeysRequired = effSetChunk + 33;
  effGetVstVersion = effSetChunk + 34;
  effNumV2Opcodes = effSetChunk + 35;
  //---from here VST 2.1 extension opcodes---------------------------------------------------------
  effEditKeyDown = effNumV2Opcodes;
  effEditKeyUp = effEditKeyDown+1;
  effSetEditKnobMode = effEditKeyDown+2;
  effGetMidiProgramName = effEditKeyDown+3;
  effGetCurrentMidiProgram = effEditKeyDown+4;
  effGetMidiProgramCategory = effEditKeyDown+5;
  effHasMidiProgramsChanged = effEditKeyDown+6;
  effGetMidiKeyName = effEditKeyDown+7;
  effBeginSetProgram = effEditKeyDown+8;
  effEndSetProgram = effEditKeyDown+9;
  effNumV2_1Opcodes = effEditKeyDown+10;
  //---from here VST 2.3 extension opcodes---------------------------------------------------------
  effGetSpeakerArrangement = effNumV2_1Opcodes;
  effShellGetNextPlugin = effGetSpeakerArrangement+1;
  effStartProcess = effGetSpeakerArrangement+2;
  effStopProcess = effGetSpeakerArrangement+3;
  effSetTotalSampleToProcess = effGetSpeakerArrangement+4;
  effSetPanLaw = effGetSpeakerArrangement+5;
  effBeginLoadBank = effGetSpeakerArrangement+6;
  effBeginLoadProgram = effGetSpeakerArrangement+7;
  effNumV2_3Opcodes = effGetSpeakerArrangement+8;

function VstDispatcherOpcodeToStr(Opcode:integer):string;
begin
  case Opcode  of
    //-- VST 1.0 opcodes ---------------------------------------
    effOpen:                       result := 'effOpen';
    effClose:                      result := 'effClose';
    effSetProgram:                 result := 'effSetProgram';
    effGetProgram:                 result := 'effGetProgram';
    effSetProgramName:             result := 'effSetProgramName';
    effGetProgramName:             result := 'effGetProgramName';
    effGetParamLabel:              result := 'effGetParamLabel';
    effGetParamDisplay:            result := 'effGetParamDisplay';
    effGetParamName:               result := 'effGetParamName';
    effGetVu:                      result := 'effGetVu';
    effSetSampleRate:              result := 'effSetSampleRate';
    effSetBlockSize:               result := 'effSetBlockSize';
    effMainsChanged:               result := 'effMainsChanged';
    effEditGetRect:                result := 'effEditGetRect';
    effEditOpen:                   result := 'effEditOpen';
    effEditClose:                  result := 'effEditClose';
    effEditDraw:                   result := 'effEditDraw';
    effEditMouse:                  result := 'effEditMouse';
    effEditKey:                    result := 'effEditKey';
    effEditIdle:                   result := 'effEditIdle';
    effEditTop:                    result := 'effEditTop';
    effEditSleep:                  result := 'effEditSleep';
    effIdentify:                   result := 'effIdentify';
    effGetChunk:                   result := 'effGetChunk';
    //---from here VST 2.0 extension opcodes---------------------------------------------------------
    effProcessEvents:              result := 'effProcessEvents';
    effCanBeAutomated:             result := 'effCanBeAutomated';
    effString2Parameter:           result := 'effString2Parameter';
    effGetNumProgramCategories:    result := 'effGetNumProgramCategories';
    effGetProgramNameIndexed:      result := 'effGetProgramNameIndexed';
    effCopyProgram:                result := 'effCopyProgram';
    effConnectInput:               result := 'effConnectInput';
    effConnectOutput:              result := 'effConnectOutput';
    effGetInputProperties:         result := 'effGetInputProperties';
    effGetOutputProperties:        result := 'effGetOutputProperties';
    effGetPlugCategory:            result := 'effGetPlugCategory';
    effGetCurrentPosition:         result := 'effGetCurrentPosition';
    effGetDestinationBuffer:       result := 'effGetDestinationBuffer';
    effOfflineNotify:              result := 'effOfflineNotify';
    effOfflinePrepare:             result := 'effOfflinePrepare';
    effOfflineRun:                 result := 'effOfflineRun';
    effProcessVarIo:               result := 'effProcessVarIo';
    effSetSpeakerArrangement:      result := 'effSetSpeakerArrangement';
    effSetBlockSizeAndSampleRate:  result := 'effSetBlockSizeAndSampleRate';
    effSetBypass:                  result := 'effSetBypass';
    effGetEffectName:              result := 'effGetEffectName';
    effGetErrorText:               result := 'effGetErrorText';
    effGetVendorString:            result := 'effGetVendorString';
    effGetProductString:           result := 'effGetProductString';
    effGetVendorVersion:           result := 'effGetVendorVersion';
    effVendorSpecific:             result := 'effVendorSpecific';
    effCanDo:                      result := 'effCanDo';
    effGetTailSize:                result := 'effGetTailSize';
    effIdle:                       result := 'effIdle';
    effGetIcon:                    result := 'effGetIcon';
    effSetViewPosition:            result := 'effSetViewPosition';
    effGetParameterProperties:     result := 'effGetParameterProperties';
    effKeysRequired:               result := 'effKeysRequired';
    effGetVstVersion:              result := 'effGetVstVersion';
    //---from here VST 2.1 extension opcodes---------------------------------------------------------
    effEditKeyDown:                result := 'effEditKeyDown';
    effEditKeyUp:                  result := 'effEditKeyUp';
    effSetEditKnobMode:            result := 'effSetEditKnobMode';
    effGetMidiProgramName:         result := 'effGetMidiProgramName';
    effGetCurrentMidiProgram:      result := 'effGetCurrentMidiProgram';
    effGetMidiProgramCategory:     result := 'effGetMidiProgramCategory';
    effHasMidiProgramsChanged:     result := 'effHasMidiProgramsChanged';
    effGetMidiKeyName:             result := 'effGetMidiKeyName';
    effBeginSetProgram:            result := 'effBeginSetProgram';
    effEndSetProgram:              result := 'effEndSetProgram';
    //---from here VST 2.3 extension opcodes---------------------------------------------------------
    effGetSpeakerArrangement:      result := 'effGetSpeakerArrangement';
    effShellGetNextPlugin:         result := 'effShellGetNextPlugin';
    effStartProcess:               result := 'effStartProcess';
    effStopProcess:                result := 'effStopProcess';
    effSetTotalSampleToProcess:    result := 'effSetTotalSampleToProcess';
    effSetPanLaw:                  result := 'effSetPanLaw';
    effBeginLoadBank:              result := 'effBeginLoadBank';
    effBeginLoadProgram:           result := 'effBeginLoadProgram';
  else
    result := 'Unknown: Opcode = ' + IntToStr(Opcode);
  end;
end;

function VstAudioMasterOpcodeToStr(Opcode:integer):string;
begin
  case Opcode of
    audioMasterAutomate:                             result := 'audioMasterAutomate';
    audioMasterVersion:                              result := 'audioMasterVersion';
    audioMasterCurrentId:                            result := 'audioMasterCurrentId';
    audioMasterIdle:                                 result := 'audioMasterIdle';
    audioMasterPinConnected:                         result := 'audioMasterPinConnected';
    audioMasterGetTime:                              result := 'audioMasterGetTime';
    audioMasterProcessEvents:                        result := 'audioMasterProcessEvents';
    audioMasterIOChanged:                            result := 'audioMasterIOChanged';
    audioMasterSizeWindow:                           result := 'audioMasterSizeWindow';
    audioMasterGetSampleRate:                        result := 'audioMasterGetSampleRate';
    audioMasterGetBlockSize:                         result := 'audioMasterGetBlockSize';
    audioMasterGetInputLatency:                      result := 'audioMasterGetInputLatency';
    audioMasterGetOutputLatency:                     result := 'audioMasterGetOutputLatency';
    audioMasterGetCurrentProcessLevel:               result := 'audioMasterGetCurrentProcessLevel';
    audioMasterGetAutomationState:                   result := 'audioMasterGetAutomationState';
    audioMasterOfflineStart:                         result := 'audioMasterOfflineStart';
    audioMasterOfflineRead:                          result := 'audioMasterOfflineRead';
    audioMasterOfflineWrite:                         result := 'audioMasterOfflineWrite';
    audioMasterOfflineGetCurrentPass:                result := 'audioMasterOfflineGetCurrentPass';
    audioMasterOfflineGetCurrentMetaPass:            result := 'audioMasterOfflineGetCurrentMetaPass';
    audioMasterGetVendorString:                      result := 'audioMasterGetVendorString';
    audioMasterGetProductString:                     result := 'audioMasterGetProductString';
    audioMasterGetVendorVersion:                     result := 'audioMasterGetVendorVersion';
    audioMasterVendorSpecific:                       result := 'audioMasterVendorSpecific';
    audioMasterCanDo:                                result := 'audioMasterCanDo';
    audioMasterGetLanguage:                          result := 'audioMasterGetLanguage';
    audioMasterGetDirectory:                         result := 'audioMasterGetDirectory';
    audioMasterUpdateDisplay:                        result := 'audioMasterUpdateDisplay';
    audioMasterBeginEdit:                            result := 'audioMasterBeginEdit';
    audioMasterEndEdit:                              result := 'audioMasterEndEdit';
    audioMasterOpenFileSelector:                     result := 'audioMasterOpenFileSelector';
    audioMasterCloseFileSelector:                    result := 'audioMasterCloseFileSelector';
    audioMasterWantMidi:                             result := 'audioMasterWantMidi';
    audioMasterSetTime:                              result := 'audioMasterSetTime';
    audioMasterTempoAt:                              result := 'audioMasterTempoAt';
    audioMasterGetNumAutomatableParameters:          result := 'audioMasterGetNumAutomatableParameters';
    audioMasterGetParameterQuantization:             result := 'audioMasterGetParameterQuantization';
    audioMasterNeedIdle:                             result := 'audioMasterNeedIdle';
    audioMasterGetPreviousPlug:                      result := 'audioMasterGetPreviousPlug';
    audioMasterGetNextPlug:                          result := 'audioMasterGetNextPlug';
    audioMasterWillReplaceOrAccumulate:              result := 'audioMasterWillReplaceOrAccumulate';
    audioMasterSetOutputSampleRate:                  result := 'audioMasterSetOutputSampleRate';
    audioMasterGetOutputSpeakerArrangement:          result := 'audioMasterGetOutputSpeakerArrangement';
    audioMasterSetIcon:                              result := 'audioMasterSetIcon';
    audioMasterOpenWindow:                           result := 'audioMasterOpenWindow';
    audioMasterCloseWindow:                          result := 'audioMasterCloseWindow';
    audioMasterEditFile:                             result := 'audioMasterEditFile';
    audioMasterGetChunkFile:                         result := 'audioMasterGetChunkFile';
    audioMasterGetInputSpeakerArrangement:           result := 'audioMasterGetInputSpeakerArrangement';
  else
    result := 'Unknown: Opcode = ' + IntToStr(Opcode);
  end;
end;

end.
