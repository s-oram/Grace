//******************************************************************************
//
//  DAEffectX.pas
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
// Version 2.4		$Date: 2006/01/12 09:04:56 $
//
// Category     : VST 2.x Interfaces
// Filename     : aeffectx.h
// Created by   : Steinberg Media Technologies
// Description  : Definition of auxiliary structures, extensions from VST 1.0 to VST 2.4
//
// © 2006, Steinberg Media Technologies, All Rights Reserved
//
//******************************************************************************
unit
    DAEffectX;

interface

uses
    DAEffect;

{$INCLUDE DVstCommon.inc}

//------------------------------------------------------------------------------
// String length limits (in characters excl. 0 byte).
//------------------------------------------------------------------------------
type
    Vst2StringConstants = VstInt32;

const
     kVstMaxNameLen       = 64;  // used for MidiProgramName, MidiProgramCategory, MidiKeyName, VstSpeakerProperties, VstPinProperties
     kVstMaxLabelLen      = 64;  // used for VstParameterProperties.label, VstPinProperties.label
     kVstMaxShortLabelLen = 8;   // used for VstParameterProperties.shortLabel, VstPinProperties.shortLabel
     kVstMaxCategLabelLen = 24;  // used for VstParameterProperties.label
     kVstMaxFileNameLen   = 100; // used for VstAudioFile.name

//------------------------------------------------------------------------------
// VstEvent
// A generic timestamped event.
//------------------------------------------------------------------------------
type
    PVstEvent = ^VstEvent;
    VstEvent = record
      vType       : VstInt32;               // see VstEventTypes
      byteSize    : VstInt32;               // size of this event, excl. type and byteSize
      deltaFrames : VstInt32;               // sample frames related to the current block start sample position
      flags       : VstInt32;               // generic flags, none defined yet
      data        : array[0..15] of byte;   // data size may vary, depending on event type
    end;

//------------------------------------------------------------------------------
// VstEvent Types used by VstEvent.
//------------------------------------------------------------------------------
type
    VstEventTypes = VstInt32;

const
     kVstMidiType   = 1;    // MIDI event  @see VstMidiEvent
     kVstSysExType  = 6;    // MIDI system exclusive  @see VstMidiSysexEvent

     {$IFDEF VST_USE_DEPRECATED}
     kVstAudioType     = 2; // (deprecated) unused event type
     kVstVideoType     = 3; // (deprecated) unused event type
     kVstParameterType = 4; // (deprecated) unused event type
     kVstTriggerType   = 5; // (deprecated) unused event type
     {$ENDIF}


//------------------------------------------------------------------------------
// A block of events for the current processed audio block. 
//------------------------------------------------------------------------------
type
    PVstEvents = ^VstEvents;
    VstEvents = record
      numEvents  : VstInt32;		              // number of Events in array
      reserved   : VstIntPtr;		              // zero (Reserved for future use)
      events     : array[0..1] of PVstEvent;  // event pointer array, variable size
    end;



//------------------------------------------------------------------------------
// MIDI Event (to be casted from VstEvent)
//------------------------------------------------------------------------------
type
    PVstMidiEvent = ^VstMidiEvent;
    VstMidiEvent = record
      vType           : VstInt32;              // kVstMidiType
      byteSize        : VstInt32;              // sizeof (VstMidiEvent)
      deltaFrames     : VstInt32;              // sample frames related to the current block start sample position
      flags           : VstInt32;              // see VstMidiEventFlags
      noteLength      : VstInt32;              // (in sample frames) of entire note, if available, else 0
      noteOffset      : VstInt32;              // offset (in sample frames) into note from note start if available, else 0
      midiData        : array[0..3] of byte;   // 1 to 3 MIDI bytes; midiData[3] is reserved (zero)
      detune          : shortint;              // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
      noteOffVelocity : byte;                  // Note Off Velocity [0, 127]
      reserved1       : byte;                  // zero (Reserved for future use)
      reserved2       : byte;                  // zero (Reserved for future use)
    end;


//------------------------------------------------------------------------------
// Flags used in #VstMidiEvent
//------------------------------------------------------------------------------
type
    VstMidiEventFlags = VstInt32;

const
     kVstMidiEventIsRealtime = 1 shl 0;   // means that this event is played life (not in playback from a sequencer track).\n This allows the Plug-In to handle these flagged events with higher priority, especially when the Plug-In has a big latency (AEffect::initialDelay)


//------------------------------------------------------------------------------
// MIDI Sysex Event (to be casted from #VstEvent)
//------------------------------------------------------------------------------
type
    PVstMidiSysExEvent = ^VstMidiSysExEvent;
    VstMidiSysExEvent = record
      vType       : VstInt32;   // #kVstSysexType
      byteSize    : VstInt32;   // sizeof(VstMidiSysexEvent)
      deltaFrames : VstInt32;   // sample frames related to the current block start sample position
      flags       : VstInt32;   // none defined yet (should be zero)
      dumpBytes   : VstInt32;   // byte size of sysexDump
      resvd1      : VstIntPtr;  // zero (Reserved for future use)
      sysexDump   : pbyte;      // sysex dump
      resvd2      : VstIntPtr;  // zero (Reserved for future use)
    end;


(*------------------------------------------------------------------------------
// VstTimeInfo
//
// Requested via audioMasterGetTime. See AudioEffectX.getTimeInfo
//
- VstTimeInfo.samplePos: Current Position. It must always be valid, and should not cost a lot to ask for.
                         The sample position is ahead of the time displayed to the user. In sequencer stop mode, its value does not change.
                         A 32 bit integer is too small for sample positions, and it's a double to make it easier to convert between ppq and samples.
- VstTimeInfo.ppqPos: At tempo 120, 1 quarter makes 1/2 second, so 2.0 ppq translates to 48000 samples at 48kHz sample rate.
                      0.25 ppq is one sixteenth note then.
                      if you need something like 480ppq, you simply multiply ppq by that scaler.
- VstTimeInfo.barStartPos: Say we're at bars/beats readout 3.3.3. That's 2 bars + 2 q + 2 sixteenth, makes 2 * 4 + 2 + .25 = 10.25 ppq.
                           At tempo 120, that's 10.25 * .5 = 5.125 seconds, times 48000 = 246000 samples (if my calculator serves me well :-)
- VstTimeInfo.samplesToNextClock : MIDI Clock Resolution (24 per Quarter Note)
                                   Can be negative the distance to the next midi clock (24 ppq, pulses per quarter) in samples.
                                   Unless samplePos falls precicely on a midi clock, this will either be negative such that the previous MIDI clock is addressed, or positive when referencing the following (future) MIDI clock.
------------------------------------------------------------------------------*)
type
    PVstTimeInfo = ^VstTimeInfo;
    VstTimeInfo = record
      samplePos          : double;      // current Position in audio samples (always valid)
      sampleRate         : double;      // current Sample Rate in Herz (always valid)
      nanoSeconds        : double;      // System Time in nanoseconds (10^-9 second)
      ppqPos             : double;      // Musical Position, in Quarter Note (1.0 equals 1 Quarter Note)
      tempo              : double;      // current Tempo in BPM (Beats Per Minute)
      barStartPos        : double;      // last Bar Start Position, in Quarter Note
      cycleStartPos      : double;      // Cycle Start (left locator), in Quarter Note
      cycleEndPos        : double;      // Cycle End (right locator), in Quarter Note
      timeSigNumerator   : VstInt32;    // Time Signature Numerator (e.g. 3 for 3/4)
      timeSigDenominator : VstInt32;    // Time Signature Denominator (e.g. 4 for 3/4)
      smpteOffset        : VstInt32;    // SMPTE offset (in SMPTE subframes (bits; 1/80 of a frame)). The current SMPTE position can be calculated using samplePos, sampleRate, and smpteFrameRate.
      smpteFrameRate     : VstInt32;    // see VstSmpteFrameRate
      samplesToNextClock : VstInt32;    // MIDI Clock Resolution (24 Per Quarter Note), can be negative (nearest clock)
      flags              : VstInt32;    // see VstTimeInfoFlags
    end;


//------------------------------------------------------------------------------
// Flags used in #VstTimeInfo
//------------------------------------------------------------------------------
type
    VstTimeInfoFlags = VstInt32;

const
     kVstTransportChanged     = 1;         // indicates that play, cycle or record state has changed
     kVstTransportPlaying     = 1 shl 1;   // set if Host sequencer is currently playing
     kVstTransportCycleActive = 1 shl 2;   // set if Host sequencer is in cycle mode
     kVstTransportRecording   = 1 shl 3;   // set if Host sequencer is in record mode
     kVstAutomationWriting    = 1 shl 6;   // set if automation write mode active (record parameter changes)
     kVstAutomationReading    = 1 shl 7;   // set if automation read mode active (play parameter changes)
     kVstNanosValid           = 1 shl 8;   // VstTimeInfo.nanoSeconds valid
     kVstPpqPosValid          = 1 shl 9;   // VstTimeInfo.ppqPos valid
     kVstTempoValid           = 1 shl 10;  // VstTimeInfo.tempo valid
     kVstBarsValid            = 1 shl 11;  // VstTimeInfo.barStartPos valid
     kVstCyclePosValid        = 1 shl 12;  // VstTimeInfo.cycleStartPos and VstTimeInfo.cycleEndPos valid
     kVstTimeSigValid         = 1 shl 13;  // VstTimeInfo.timeSigNumerator and VstTimeInfo.timeSigDenominator valid
     kVstSmpteValid           = 1 shl 14;  // VstTimeInfo.smpteOffset and VstTimeInfo.smpteFrameRate valid
     kVstClockValid           = 1 shl 15;  // VstTimeInfo.samplesToNextClock valid


//------------------------------------------------------------------------------
// SMPTE Frame Rates
//------------------------------------------------------------------------------
type
    VstSmpteFrameRate = VstInt32;

const
     kVstSmpte24fps    = 0;   // 24 fps
     kVstSmpte25fps    = 1;   // 25 fps
     kVstSmpte2997fps  = 2;   // 29.97 fps
     kVstSmpte30fps    = 3;   // 30 fps
     kVstSmpte2997dfps = 4;   // 29.97 drop
     kVstSmpte30dfps   = 5;   // 30 drop

     kVstSmpteFilm16mm = 6;   // Film 16mm
     kVstSmpteFilm35mm = 7;   // Film 35mm
     kVstSmpte239fps   = 10;  // HDTV: 23.976 fps
     kVstSmpte249fps   = 11;  // HDTV: 24.976 fps
     kVstSmpte599fps   = 12;  // HDTV: 59.94 fps
     kVstSmpte60fps    = 13;  // HDTV: 60 fps


//------------------------------------------------------------------------------
// Variable IO for Offline Processing 
//------------------------------------------------------------------------------
type
    PVstVariableIo = ^VstVariableIo;
    VstVariableIo = record
      inputs                    : PPSingle;    // input audio buffers
      outputs                   : PPSingle;    // output audio buffers
      numSamplesInput           : VstInt32;    // number of incoming samples
      numSamplesOutput          : VstInt32;    // number of outgoing samples
      numSamplesInputProcessed  : PVstInt32;   // number of samples actually processed of input
      numSamplesOutputProcessed : PVstInt32;   // number of samples actually processed of output
    end;


//------------------------------------------------------------------------------
// Language code returned by audioMasterGetLanguage
//------------------------------------------------------------------------------
type
    VstHostLanguage = VstInt32;

const
     kVstLangEnglish  = 1;  // English
     kVstLangGerman   = 2;  // German
     kVstLangFrench   = 3;  // French
     kVstLangItalian  = 4;  // Italian
     kVstLangSpanish  = 5;  // Spanish
     kVstLangJapanese = 6;  // Japanese


//------------------------------------------------------------------------------
// VST 2.x dispatcher Opcodes (Plug-in to Host). Extension of AudioMasterOpcodes
//------------------------------------------------------------------------------
type
    AudioMasterOpcodesX = AudioMasterOpcodes;

const
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


     {$IFDEF VST_USE_DEPRECATED}
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
     {$ENDIF}


//------------------------------------------------------------------------------
// VST 2.x dispatcher Opcodes (Host to Plug-in). Extension of AEffectOpcodes
//------------------------------------------------------------------------------
type
    AEffectXOpcodes = AEffectOpcodes;

const
     effProcessEvents             = effSetChunk + 1;   // [ptr] PVstEvents. See AudioEffectX.processEvents

     effCanBeAutomated            = effSetChunk + 2;   // [index] parameter index [return value] 1=true, 0=false. See AudioEffectX.canParameterBeAutomated
     effString2Parameter          = effSetChunk + 3;   // [index] parameter index [ptr] parameter string [return value] true for success. See AudioEffectX.string2parameter

     effGetProgramNameIndexed     = effSetChunk + 5;   // [index] program index [ptr] buffer for program name, limited to kVstMaxProgNameLen [return value] true for success. See AudioEffectX.getProgramNameIndexed

     effGetInputProperties        = effSetChunk + 9;   // [index] input index [ptr] PVstPinProperties [return value] 1 if supported. See AudioEffectX.getInputProperties
     effGetOutputProperties       = effSetChunk + 10;  // [index] output index [ptr] PVstPinProperties [return value] 1 if supported. See AudioEffectX.getOutputProperties
     effGetPlugCategory           = effSetChunk + 11;  // [return value] category. See VstPlugCategory. See AudioEffectX.getPlugCategory

     effOfflineNotify             = effSetChunk + 14;  // [ptr] VstAudioFile array [value] count [index] start flag. See AudioEffectX.offlineNotify
     effOfflinePrepare            = effSetChunk + 15;  // [ptr] VstOfflineTask array [value] count. See AudioEffectX.offlinePrepare
     effOfflineRun                = effSetChunk + 16;  // [ptr] VstOfflineTask array [value] count. See AudioEffectX.offlineRun

     effProcessVarIo              = effSetChunk + 17;  // [ptr] PVstVariableIo. See AudioEffectX.processVariableIo
     effSetSpeakerArrangement     = effSetChunk + 18;  // [value] input PVstSpeakerArrangement [ptr]: output PVstSpeakerArrangement. See AudioEffectX.setSpeakerArrangement

     effSetBypass                 = effSetChunk + 20;  // [value] 1 = bypass, 0 = no bypass. See AudioEffectX.setBypass
     effGetEffectName             = effSetChunk + 21;  // [ptr] buffer for effect name, limited to kVstMaxEffectNameLen. See AudioEffectX.getEffectName

     effGetVendorString           = effSetChunk + 23;  // [ptr] buffer for effect vendor string, limited to kVstMaxVendorStrLen. See AudioEffectX.getVendorString
     effGetProductString          = effSetChunk + 24;  // [ptr] buffer for effect vendor string, limited to kVstMaxProductStrLen. See AudioEffectX.getProductString
     effGetVendorVersion          = effSetChunk + 25;  // [return value] vendor-specific version. See AudioEffectX.getVendorVersion
     effVendorSpecific            = effSetChunk + 26;  // no definition, vendor specific handling. See AudioEffectX.vendorSpecific
     effCanDo                     = effSetChunk + 27;  // [ptr] "can do" string [return value]: 0: "don't know" -1: "no" 1: "yes". See AudioEffectX.canDo
     effGetTailSize               = effSetChunk + 28;  // [return value] tail size (for example the reverb time of a reverb plug-in); 0 is default (return 1 for 'no tail')

     effGetParameterProperties    = effSetChunk + 32;  // [index] parameter index [ptr] PVstParameterProperties [return value] 1 if supported. See AudioEffectX.getParameterProperties

     effGetVstVersion             = effSetChunk + 34;  // [return value] VST version. See AudioEffectX.getVstVersion

     {$IFDEF VST_2_1_EXTENSIONS}
     effEditKeyDown               = effSetChunk + 35;  // [index] ASCII character [value] virtual key [opt] modifiers [return value] 1 if key used. See AEffEditor.onKeyDown
     effEditKeyUp                 = effSetChunk + 36;  // [index] ASCII character [value] virtual key [opt] modifiers [return value] 1 if key used. See AEffEditor.onKeyUp
     effSetEditKnobMode           = effSetChunk + 37;  // [value] knob mode 0: circular, 1: circular relativ, 2: linear (CKnobMode in VSTGUI). See AEffEditor.setKnobMode

     effGetMidiProgramName        = effSetChunk + 38;  // [index] MIDI channel [ptr] PMidiProgramName [return value] number of used programs, 0 if unsupported. See AudioEffectX.getMidiProgramName
     effGetCurrentMidiProgram     = effSetChunk + 39;  // [index] MIDI channel [ptr] PMidiProgramName [return value] index of current program. See AudioEffectX.getCurrentMidiProgram
     effGetMidiProgramCategory    = effSetChunk + 40;  // [index] MIDI channel [ptr] PMidiProgramCategory [return value] number of used categories, 0 if unsupported. See AudioEffectX.getMidiProgramCategory
     effHasMidiProgramsChanged    = effSetChunk + 41;  // [index] MIDI channel [return value] 1 if the MidiProgramName(s) or MidiKeyName(s) have changed. See AudioEffectX.hasMidiProgramsChanged
     effGetMidiKeyName            = effSetChunk + 42;  // [index]: MIDI channel [ptr]: #MidiKeyName* [return value]: true if supported, false otherwise. See AudioEffectX.getMidiKeyName

     effBeginSetProgram           = effSetChunk + 43;  // no arguments. See AudioEffectX.beginSetProgram
     effEndSetProgram             = effSetChunk + 44;  // no arguments. See AudioEffectX.endSetProgram
     {$ENDIF} // VST_2_1_EXTENSIONS

     {$IFDEF VST_2_3_EXTENSIONS}
     effGetSpeakerArrangement     = effSetChunk + 45;  // [value] input PVstSpeakerArrangement [ptr] output PVstSpeakerArrangement. See AudioEffectX.getSpeakerArrangement
     effShellGetNextPlugin        = effSetChunk + 46;  // [ptr] buffer for plug-in name, limited to kVstMaxProductStrLen [return value] next plugin's uniqueID. See AudioEffectX.getNextShellPlugin

     effStartProcess              = effSetChunk + 47;  // no arguments. See AudioEffectX.startProcess
     effStopProcess               = effSetChunk + 48;  // no arguments. See AudioEffectX.stopProcess
     effSetTotalSampleToProcess   = effSetChunk + 49;  // [value] number of samples to process, offline only!. See AudioEffectX.setTotalSampleToProcess
     effSetPanLaw                 = effSetChunk + 50;  // [value] pan law [opt] gain. See VstPanLawType. See AudioEffectX.setPanLaw

     effBeginLoadBank             = effSetChunk + 51;  // [ptr] PVstPatchChunkInfo [return value]: -1: bank can't be loaded, 1: bank can be loaded, 0: unsupported. See AudioEffectX.beginLoadBank
     effBeginLoadProgram          = effSetChunk + 52;  // [ptr] PVstPatchChunkInfo [return value]: -1: prog can't be loaded, 1: prog can be loaded, 0: unsupported. See AudioEffectX.beginLoadProgram
     {$ENDIF} // VST_2_3_EXTENSIONS

     {$IFDEF VST_2_4_EXTENSIONS}
     effSetProcessPrecision     	= effSetChunk + 53;  // [value] see VstProcessPrecision. See AudioEffectX.setProcessPrecision
     effGetNumMidiInputChannels   = effSetChunk + 54;  // [return value] number of used MIDI input channels (1-15). See AudioEffectX.getNumMidiInputChannels
     effGetNumMidiOutputChannels  = effSetChunk + 55;  // [return value] number of used MIDI output channels (1-15). See AudioEffectX.getNumMidiOutputChannels
     {$ENDIF} // VST_2_4_EXTENSIONS


     {$IFDEF VST_USE_DEPRECATED}
     effGetNumProgramCategories   = effSetChunk + 4;   // (deprecated) deprecated in VST 2.4
     effCopyProgram               = effSetChunk + 6;   // (deprecated) deprecated in VST 2.4
     effConnectInput              = effSetChunk + 7;   // (deprecated) deprecated in VST 2.4
     effConnectOutput             = effSetChunk + 8;   // (deprecated) deprecated in VST 2.4
     effGetCurrentPosition        = effSetChunk + 12;  // (deprecated) deprecated in VST 2.4
     effGetDestinationBuffer      = effSetChunk + 13;  // (deprecated) deprecated in VST 2.4
     effSetBlockSizeAndSampleRate = effSetChunk + 19;  // (deprecated) deprecated in VST 2.4
     effGetErrorText              = effSetChunk + 22;  // (deprecated) deprecated in VST 2.4
     effIdle                      = effSetChunk + 29;  // (deprecated) deprecated in VST 2.4
     effGetIcon                   = effSetChunk + 30;  // (deprecated) deprecated in VST 2.4
     effSetViewPosition           = effSetChunk + 31;  // (deprecated) deprecated in VST 2.4
     effKeysRequired              = effSetChunk + 33;  // (deprecated) deprecated in VST 2.4     
     {$ENDIF}


//------------------------------------------------------------------------------
// Symbolic precision constants used for effSetProcessPrecision
//------------------------------------------------------------------------------
type
    VstProcessPrecision = VstInt32;

const
     kVstProcessPrecision32 = 0;   // single precision float (32 bit)
     kVstProcessPrecision64 = 1;   // double precision (64 bit)


//------------------------------------------------------------------------------
// Parameter Properties used in #effGetParameterProperties
//------------------------------------------------------------------------------
type
    PVstParameterProperties = ^VstParameterProperties;
    VstParameterProperties = record
      stepFloat               : single;                                     // float step
      smallStepFloat          : single;                                     // small float step
      largeStepFloat          : single;                                     // large float step
      vLabel                  : array[0..kVstMaxLabelLen-1] of AnsiChar;        // parameter label
      flags                   : VstInt32;                                   // @see VstParameterFlags
      minInteger              : VstInt32;                                   // integer minimum
      maxInteger              : VstInt32;                                   // integer maximum
      stepInteger             : VstInt32;                                   // integer step
      largeStepInteger        : VstInt32;                                   // large integer step
      shortLabel              : array[0..kVstMaxShortLabelLen-1] of AnsiChar;   // short label, recommended: 6 + delimiter

      // The following are for remote controller display purposes.
      // Note that the kVstParameterSupportsDisplayIndex flag must be set.
      // Host can scan all parameters, and find out in what order
      // to display them:

      displayIndex            : VstInt16;                                   // index where this parameter should be displayed (starting with 0)

      // Host can also possibly display the parameter group (category), such as...
      // ---------------------------
      // Osc 1
      // Wave  Detune  Octave  Mod
      // ---------------------------
      // ...if the plug-in supports it (flag #kVstParameterSupportsDisplayCategory)

      category                : VstInt16;                                   // 0: no category, else group index + 1
      numParametersInCategory : VstInt16;                                   // number of parameters in category
      reserved                : VstInt16;                                   // zero
      categoryLabel           : array[0..kVstMaxCategLabelLen-1] of AnsiChar;   // category label, e.g. "Osc 1"

      future                  : array[0..15] of AnsiChar;                       // reserved for future use
    end;

//------------------------------------------------------------------------------
// Flags used in VstParameterProperties
//------------------------------------------------------------------------------
type
    VstParameterFlags = VstInt32;

const
     kVstParameterIsSwitch                = 1 shl 0;  // parameter is a switch (on/off)
     kVstParameterUsesIntegerMinMax       = 1 shl 1;  // minInteger, maxInteger valid
     kVstParameterUsesFloatStep           = 1 shl 2;  // stepFloat, smallStepFloat, largeStepFloat valid
     kVstParameterUsesIntStep             = 1 shl 3;  // stepInteger, largeStepInteger valid
     kVstParameterSupportsDisplayIndex    = 1 shl 4;  // displayIndex valid
     kVstParameterSupportsDisplayCategory = 1 shl 5;   // category, etc. valid
     kVstParameterCanRamp                 = 1 shl 6;  // set if parameter value can ramp up/down


//------------------------------------------------------------------------------
// Pin Properties used in effGetInputProperties and effGetOutputProperties
//------------------------------------------------------------------------------
type
    PVstPinProperties = ^VstPinProperties;
    VstPinProperties = record
      vLabel          : array[0..kVstMaxLabelLen-1] of AnsiChar;       // pin name
      flags           : VstInt32;                                  // see VstPinPropertiesFlags
      arrangementType : VstInt32;                                  // see VstSpeakerArrangementType
      shortLabel      : array[0..kVstMaxShortLabelLen-1] of AnsiChar;  // short name (recommended: 6 + delimiter)

      future          : array[0..47] of byte;                      // reserved for future use
    end;

//------------------------------------------------------------------------------
// Flags used in VstPinProperties
//------------------------------------------------------------------------------
type
    VstPinPropertiesFlags = VstInt32;

const
     kVstPinIsActive   = 1 shl 0;    // pin is active, ignored by Host
     kVstPinIsStereo   = 1 shl 1;    // pin is first of a stereo pair
     kVstPinUseSpeaker = 1 shl 2;    // VstPinProperties.arrangementType is valid and can be used to get the wanted arrangement


//------------------------------------------------------------------------------
// Plug-in Categories
//------------------------------------------------------------------------------
type
    VstPlugCategory = VstInt32;

const
     kPlugCategUnknown         = 0;     // Unknown, category not implemented
     kPlugCategEffect          = 1;     // Simple Effect
     kPlugCategSynth           = 2;     // VST Instrument (Synths, samplers,...)
     kPlugCategAnalysis        = 3;     // Scope, Tuner, ...
     kPlugCategMastering       = 4;     // Dynamics, ...
     kPlugCategSpacializer     = 5;     // Panners, ...
     kPlugCategRoomFx          = 6;     // Delays and Reverbs
     kPlugSurroundFx           = 7;     // Dedicated surround processor
     kPlugCategRestoration     = 8;     // Denoiser, ...
     kPlugCategOfflineProcess  = 9;     // Offline Process
     kPlugCategShell           = 10;    // Plug-in is container of other plug-ins. See effShellGetNextPlugin
     kPlugCategGenerator       = 11;    // ToneGenerator, ...

     kPlugCategMaxCount        = 12;    // Marker to count the categories


//------------------------------------------------------------------------------
// MIDI Programs
// MIDI Program Description
//------------------------------------------------------------------------------
type
    PMidiProgramName = ^MidiProgramName;
    MidiProgramName = record
      thisProgramIndex    : VstInt32;                            // 0 or greater: fill struct for this program index
      name                : array[0..kVstMaxNameLen-1] of AnsiChar;  // program name
      midiProgram         : shortint;                            // -1:off, 0-127
      midiBankMsb         : shortint;                            // -1:off, 0-127
      midiBankLsb         : shortint;                            // -1:off, 0-127
      reserved            : byte;                                // zero
      parentCategoryIndex : VstInt32;                            // -1:no parent category
      flags               : VstInt32;                            // omni etc. See VstMidiProgramNameFlags
    end;


//------------------------------------------------------------------------------
// Flags used in MidiProgramName
//------------------------------------------------------------------------------
type
    VstMidiProgramNameFlags = VstInt32;

const
     kMidiIsOmni = 1;	// default is multi. for omni mode, channel 0 is used for inquiries and program changes


//------------------------------------------------------------------------------
// MIDI Program Category
//------------------------------------------------------------------------------
type
    PMidiProgramCategory = ^MidiProgramCategory;
    MidiProgramCategory = record
      thisCategoryIndex   : VstInt32;                             // 0 or greater:  fill struct for this category index.
      name                : array[0..kVstMaxNameLen-1] of AnsiChar;   // name
      parentCategoryIndex : VstInt32;                             // -1:no parent category
      flags               : VstInt32;                             // reserved, none defined yet, zero.
    end;


//------------------------------------------------------------------------------
// MIDI Key Description
//------------------------------------------------------------------------------
type
    PMidiKeyName = ^MidiKeyName;
    MidiKeyName = record
      thisProgramIndex : VstInt32;                            // 0 or greater:  fill struct for this program index.
      thisKeyNumber    : VstInt32;                            // 0 - 127. fill struct for this key number.
      keyName          : array[0..kVstMaxNameLen-1] of AnsiChar;  // key name, empty means regular key names
      reserved         : VstInt32;                            // zero
      flags            : VstInt32;                            // reserved, none defined yet, zero.
    end;


(*------------------------------------------------------------------------------
// Surround Setup

  Speaker Properties.
	The origin for azimuth is right (as by math conventions dealing with radians).
	The elevation origin is also right, visualizing a rotation of a circle across
  the	-pi/pi axis of the horizontal circle. Thus, an elevation of -pi/2
  corresponds	to bottom, and a speaker standing on the left, and 'beaming'
  upwards would have an azimuth of -pi, and an elevation of pi/2.
	For user interface representation, grads are more likely to be used, and the
	origins will obviously 'shift' accordingly.
------------------------------------------------------------------------------*)
type
    PVstSpeakerProperties = ^VstSpeakerProperties;
    VstSpeakerProperties = record
      azimuth   : single;                                // unit: rad, range: -PI...PI, exception: 10.f for LFE channel
      elevation : single;                                // unit: rad, range: -PI/2...PI/2, exception: 10.f for LFE channel
      radius    : single;                                // unit: meter, exception: 0.f for LFE channel
      reserved  : single;                                // zero (reserved for future use)
      name      : array[0..kVstMaxNameLen-1] of AnsiChar;    // for new setups, new names should be given (L/R/C... won't do)
      vType     : VstInt32;                              // see VstSpeakerType

      future    : array[0..27] of byte;                  // reserved for future use
    end;


//------------------------------------------------------------------------------
// Speaker Arrangement
//------------------------------------------------------------------------------
type
    PPVstSpeakerArrangement = ^PVstSpeakerArrangement;
    PVstSpeakerArrangement = ^VstSpeakerArrangement;
    VstSpeakerArrangement = record
      vType       : VstInt32;                             // e.g. #kSpeakerArr51 for 5.1. See VstSpeakerArrangementType
      numChannels : VstInt32;                             // number of channels in this speaker arrangement
      speakers    : array[0..7] of VstSpeakerProperties;  // variable sized speaker array
    end;


//------------------------------------------------------------------------------
// Speaker Types
//------------------------------------------------------------------------------
type
    VstSpeakerType = VstInt32;

const
     kSpeakerUndefined = $7FFFFFFF;   // Undefined
     kSpeakerM         = 0;           // Mono (M)
     kSpeakerL         = 1;           // Left (L)
     kSpeakerR         = 2;           // Right (R)
     kSpeakerC         = 3;           // Center (C)
     kSpeakerLfe       = 4;           // Subbass (Lfe)
     kSpeakerLs        = 5;           // Left Surround (Ls)
     kSpeakerRs        = 6;           // Right Surround (Rs)
     kSpeakerLc        = 7;           // Left of Center (Lc)
     kSpeakerRc        = 8;           // Right of Center (Rc)
     kSpeakerS         = 9;           // Surround (S)
     kSpeakerCs        = kSpeakerS;   // Center of Surround (Cs) = Surround (S)
     kSpeakerSl        = 10;          // Side Left (Sl)
     kSpeakerSr        = 11;          // Side Right (Sr)
     kSpeakerTm        = 12;          // Top Middle (Tm)
     kSpeakerTfl       = 13;          // Top Front Left (Tfl)
     kSpeakerTfc       = 14;          // Top Front Center (Tfc)
     kSpeakerTfr       = 15;          // Top Front Right (Tfr)
     kSpeakerTrl       = 16;          // Top Rear Left (Trl)
     kSpeakerTrc       = 17;          // Top Rear Center (Trc)
     kSpeakerTrr       = 18;          // Top Rear Right (Trr)
     kSpeakerLfe2      = 19;          // Subbass 2 (Lfe2)


(*------------------------------------------------------------------------------
  User-defined speaker types, to be extended in the negative range.
	Will be handled as their corresponding speaker types with abs values:
	e.g abs(kSpeakerU1) == kSpeakerL, abs(kSpeakerU2) == kSpeakerR) */
------------------------------------------------------------------------------*)
type
    VstUserSpeakerType = VstInt32;

const
     kSpeakerU32 = -32;
     kSpeakerU31 = -31;
     kSpeakerU30 = -30;
     kSpeakerU29 = -29;
     kSpeakerU28 = -28;
     kSpeakerU27 = -27;
     kSpeakerU26 = -26;
     kSpeakerU25 = -25;
     kSpeakerU24 = -24;
     kSpeakerU23 = -23;
     kSpeakerU22 = -22;
     kSpeakerU21 = -21;
     kSpeakerU20 = -20;			// == kSpeakerLfe2
     kSpeakerU19 = -19;			// == kSpeakerTrr
     kSpeakerU18 = -18;			// == kSpeakerTrc
     kSpeakerU17 = -17;			// == kSpeakerTrl
     kSpeakerU16 = -16;			// == kSpeakerTfr
     kSpeakerU15 = -15;			// == kSpeakerTfc
     kSpeakerU14 = -14;			// == kSpeakerTfl
     kSpeakerU13 = -13;			// == kSpeakerTm
     kSpeakerU12 = -12;			// == kSpeakerSr
     kSpeakerU11 = -11;			// == kSpeakerSl
     kSpeakerU10 = -10;			// == kSpeakerCs
     kSpeakerU9	 = -9;			// == kSpeakerS
     kSpeakerU8	 = -8;			// == kSpeakerRc
     kSpeakerU7	 = -7;			// == kSpeakerLc
     kSpeakerU6	 = -6;			// == kSpeakerRs
     kSpeakerU5	 = -5;			// == kSpeakerLs
     kSpeakerU4	 = -4;			// == kSpeakerLfe
     kSpeakerU3	 = -3;			// == kSpeakerC
     kSpeakerU2	 = -2;			// == kSpeakerR
     kSpeakerU1	 = -1;			// == kSpeakerL


//------------------------------------------------------------------------------
// Speaker Arrangement Types
//------------------------------------------------------------------------------
type
    VstSpeakerArrangementType = VstInt32;

const
     kSpeakerArrUserDefined     = -2;  // user defined
     kSpeakerArrEmpty           = -1;  // empty arrangement
     kSpeakerArrMono            = 0;   // M
     kSpeakerArrStereo          = 1;   // L R
     kSpeakerArrStereoSurround  = 2;   // Ls Rs
     kSpeakerArrStereoCenter    = 3;   // Lc Rc
     kSpeakerArrStereoSide      = 4;   // Sl Sr
     kSpeakerArrStereoCLfe      = 5;   // C Lfe
     kSpeakerArr30Cine          = 6;   // L R C
     kSpeakerArr30Music         = 7;   // L R S
     kSpeakerArr31Cine          = 8;   // L R C Lfe
     kSpeakerArr31Music         = 9;   // L R Lfe S
     kSpeakerArr40Cine          = 10;  // L R C   S (LCRS)
     kSpeakerArr40Music         = 11;  // L R Ls  Rs (Quadro)
     kSpeakerArr41Cine          = 12;  // L R C   Lfe S (LCRS+Lfe)
     kSpeakerArr41Music         = 13;  // L R Lfe Ls Rs (Quadro+Lfe)
     kSpeakerArr50              = 14;  // L R C Ls  Rs
     kSpeakerArr51              = 15;  // L R C Lfe Ls Rs
     kSpeakerArr60Cine          = 16;  // L R C   Ls  Rs Cs
     kSpeakerArr60Music         = 17;  // L R Ls  Rs  Sl Sr
     kSpeakerArr61Cine          = 18;  // L R C   Lfe Ls Rs Cs
     kSpeakerArr61Music         = 19;  // L R Lfe Ls  Rs Sl Sr
     kSpeakerArr70Cine          = 20;  // L R C Ls  Rs Lc Rc
     kSpeakerArr70Music         = 21;  // L R C Ls  Rs Sl Sr
     kSpeakerArr71Cine          = 22;  // L R C Lfe Ls Rs Lc Rc
     kSpeakerArr71Music         = 23;  // L R C Lfe Ls Rs Sl Sr
     kSpeakerArr80Cine          = 24;  // L R C Ls  Rs Lc Rc Cs
     kSpeakerArr80Music         = 25;  // L R C Ls  Rs Cs Sl Sr
     kSpeakerArr81Cine          = 26;  // L R C Lfe Ls Rs Lc Rc Cs
     kSpeakerArr81Music         = 27;  // L R C Lfe Ls Rs Cs Sl Sr
     kSpeakerArr102             = 28;  // L R C Lfe Ls Rs Tfl Tfc Tfr Trl Trr Lfe2
     kNumSpeakerArr             = 29;


//------------------------------------------------------------------------------
// Offline Processing
// Offline Task Description
//------------------------------------------------------------------------------
type
    PVstOfflineTask = ^VstOfflineTask;
    VstOfflineTask = record
      processName            : array[0..95] of AnsiChar;         // set by plug-in

      // audio access
      readPosition           : double;                       // set by plug-in/Host
      writePosition          : double;                       // set by plug-in/Host
      readCount              : VstInt32;                     // set by plug-in/Host
      writeCount             : VstInt32;                     // set by plug-in
      sizeInputBuffer        : VstInt32;                     // set by Host
      sizeOutputBuffer       : VstInt32;                     // set by Host
      inputBuffer            : pointer;                      // set by Host
      outputBuffer           : pointer;                      // set by Host
      positionToProcessFrom  : double;                       // set by Host
      numFramesToProcess     : double;                       // set by Host
      maxFramesToWrite       : double;                       // set by plug-in

      // other data access
      extraBuffer            : pointer;                      // set by plug-in
      value                  : VstInt32;                     // set by Host or plug-in
      index                  : VstInt32;                     // set by Host or plug-in

      // file attributes
      numFramesInSourceFile  : double;                       // set by Host
      sourceSampleRate       : double;                       // set by Host or plug-in
      destinationSampleRate  : double;                       // set by Host or plug-in
      numSourceChannels      : VstInt32;                     // set by Host or plug-in
      numDestinationChannels : VstInt32;                     // set by Host or plug-in
      sourceFormat           : VstInt32;                     // set by Host
      destinationFormat      : VstInt32;                     // set by plug-in
      outputText             : array[0..511] of AnsiChar;        // set by plug-in or Host

      // progress notification
      progress               : double;                       // set by plug-in
      progressMode           : VstInt32;                     // Reserved for future use
      progressText           : array[0..99] of AnsiChar;         // set by plug-in

      flags                  : VstInt32;                     // set by Host and plug-in; see enum #VstOfflineTaskFlags
      returnValue            : VstInt32;                     // Reserved for future use
      hostOwned              : pointer;                      // set by Host
      plugOwned              : pointer;                      // set by plug-in

      future                 : array[0..1023] of byte;       // Reserved for future use
    end;

//------------------------------------------------------------------------------
// Flags used in VstOfflineTask
//------------------------------------------------------------------------------
type
    VstOfflineTaskFlags = VstInt32;

const
     kVstOfflineUnvalidParameter = 1 shl 0;    // set by Host
     kVstOfflineNewFile          = 1 shl 1;	   // set by Host

     kVstOfflinePlugError        = 1 shl 10;   // set by plug-in
     kVstOfflineInterleavedAudio = 1 shl 11;   // set by plug-in
     kVstOfflineTempOutputFile   = 1 shl 12;   // set by plug-in
     kVstOfflineFloatOutputFile  = 1 shl 13;   // set by plug-in
     kVstOfflineRandomWrite      = 1 shl 14;   // set by plug-in
     kVstOfflineStretch          = 1 shl 15;   // set by plug-in
     kVstOfflineNoThread         = 1 shl 16;   // set by plug-in


//------------------------------------------------------------------------------
// Option passed to offlineRead/offlineWrite
//------------------------------------------------------------------------------
type
    VstOfflineOption = VstInt32;

const
     kVstOfflineAudio      = 0;     // reading/writing audio samples
     kVstOfflinePeaks      = 1;     // reading graphic representation
     kVstOfflineParameter  = 2;     // reading/writing parameters
     kVstOfflineMarker     = 3;     // reading/writing marker
     kVstOfflineCursor     = 4;     // reading/moving edit cursor
     kVstOfflineSelection  = 5;     // reading/changing selection
     kVstOfflineQueryFiles = 6;     // to request the Host to call asynchronously #offlineNotify


//------------------------------------------------------------------------------
// Structure passed to offlineNotify and offlineStart
//------------------------------------------------------------------------------
type
    PVstAudioFile = ^VstAudioFile;
    VstAudioFile = record
      flags                : VstInt32;                                // see enum #VstAudioFileFlags
      hostOwned            : pointer;                                 // any data private to Host
      plugOwned            : pointer;                                 // any data private to plug-in
      name                 : array[0..kVstMaxFileNameLen-1] of AnsiChar;	// file title
      uniqueId             : VstInt32;                                // uniquely identify a file during a session
      sampleRate           : double;                                  // file sample rate
      numChannels          : VstInt32;                                // number of channels (1 for mono, 2 for stereo...)
      numFrames            : double;                                  // number of frames in the audio file
      format               : VstInt32;                                // Reserved for future use
      editCursorPosition   : double;                                  // -1 if no such cursor
      selectionStart       : double;                                  // frame index of first selected frame, or -1
      selectionSize        : double;                                  // number of frames in selection, or 0
      selectedChannelsMask : VstInt32;                                // 1 bit per channel
      numMarkers           : VstInt32;                                // number of markers in the file
      timeRulerUnit        : VstInt32;                                // see doc for possible values
      timeRulerOffset      : double;                                  // offset in time ruler (positive or negative)
      tempo                : double;                                  // as BPM (Beats Per Minute)
      timeSigNumerator     : VstInt32;                                // time signature numerator
      timeSigDenominator   : VstInt32;                                // time signature denominator
      ticksPerBlackNote    : VstInt32;                                // resolution
      smpteFrameRate       : VstInt32;                                // SMPTE rate (set as in #VstTimeInfo)

      future               : array[0..63] of byte;   // Reserved for future use
    end;

    
//------------------------------------------------------------------------------
// Flags used in VstAudioFile
//------------------------------------------------------------------------------
type
    VstAudioFileFlags = VstInt32;

const
     kVstOfflineReadOnly            = 1 shl 0;   // set by Host (in call offlineNotify)
     kVstOfflineNoRateConversion    = 1 shl 1;   // set by Host (in call offlineNotify)
     kVstOfflineNoChannelChange     = 1 shl 2;   // set by Host (in call offlineNotify)

     kVstOfflineCanProcessSelection = 1 shl 10;  // set by plug-in (in call offlineStart)
     kVstOfflineNoCrossfade         = 1 shl 11;  // set by plug-in (in call offlineStart)
     kVstOfflineWantRead            = 1 shl 12;  // set by plug-in (in call offlineStart)
     kVstOfflineWantWrite           = 1 shl 13;  // set by plug-in (in call offlineStart)
     kVstOfflineWantWriteMarker     = 1 shl 14;  // set by plug-in (in call offlineStart)
     kVstOfflineWantMoveCursor      = 1 shl 15;  // set by plug-in (in call offlineStart)
     kVstOfflineWantSelect          = 1 shl 16;  // set by plug-in (in call offlineStart)


//------------------------------------------------------------------------------
// Audio file marker
//------------------------------------------------------------------------------
type
    PVstAudioFileMarker = ^VstAudioFileMarker;
    VstAudioFileMarker = record
      position : double;                 // marker position
      name     : array[0..31] of AnsiChar;   // marker name
      vType    : VstInt32;               // marker type
      id       : VstInt32;               // marker identifier
      reserved : VstInt32;               // reserved for future use
    end;


//------------------------------------------------------------------------------
// Others
//------------------------------------------------------------------------------

{$IFDEF VST_USE_DEPRECATED}
//------------------------------------------------------------------------------
// (deprecated) Structure used for #openWindow and #closeWindow (deprecated in VST 2.4)
//------------------------------------------------------------------------------
type
    PVstWindow = ^VstWindow;
    VstWindow = record
      title      : array[0..127] of AnsiChar;
      xPos       : VstInt16;
      yPos       : VstInt16;
      width      : VstInt16;
      height     : VstInt16;
      style      : VstInt32;
      parent     : pointer;
      userHandle : pointer;
      winHandle  : pointer;

      future     : array[0..103] of byte;
    end;
{$ENDIF}


//------------------------------------------------------------------------------
// Structure used for keyUp/keyDown
//------------------------------------------------------------------------------
type
    PVstKeyCode = ^VstKeyCode;
    VstKeyCode = record
      character : VstInt32; // ASCII character
      virt      : byte;     // see VstVirtualKey
      modifier  : byte;     // see VstModifierKey
    end;

//------------------------------------------------------------------------------
// Platform-independent definition of Virtual Keys (used in VstKeyCode)
//------------------------------------------------------------------------------
type
    VstVirtualKey = VstInt32;
    
const
     VKEY_BACK         = 1;
     VKEY_TAB          = 2;
     VKEY_CLEAR        = 3;
     VKEY_RETURN       = 4;
     VKEY_PAUSE        = 5;
     VKEY_ESCAPE       = 6;
     VKEY_SPACE        = 7;
     VKEY_NEXT         = 8;
     VKEY_END          = 9;
     VKEY_HOME         = 10;

     VKEY_LEFT         = 11;
     VKEY_UP           = 12;
     VKEY_RIGHT        = 13;
     VKEY_DOWN         = 14;
     VKEY_PAGEUP       = 15;
     VKEY_PAGEDOWN     = 16;

     VKEY_SELECT       = 17;
     VKEY_PRINT        = 18;
     VKEY_ENTER        = 19;
     VKEY_SNAPSHOT     = 20;
     VKEY_INSERT       = 21;
     VKEY_DELETE       = 22;
     VKEY_HELP         = 23;
     VKEY_NUMPAD0      = 24;
     VKEY_NUMPAD1      = 25;
     VKEY_NUMPAD2      = 26;
     VKEY_NUMPAD3      = 27;
     VKEY_NUMPAD4      = 28;
     VKEY_NUMPAD5      = 29;
     VKEY_NUMPAD6      = 30;
     VKEY_NUMPAD7      = 31;
     VKEY_NUMPAD8      = 32;
     VKEY_NUMPAD9      = 33;
     VKEY_MULTIPLY     = 34;
     VKEY_ADD          = 35;
     VKEY_SEPARATOR    = 36;
     VKEY_SUBTRACT     = 37;
     VKEY_DECIMAL      = 38;
     VKEY_DIVIDE       = 39;
     VKEY_F1           = 40;
     VKEY_F2           = 41;
     VKEY_F3           = 42;
     VKEY_F4           = 43;
     VKEY_F5           = 44;
     VKEY_F6           = 45;
     VKEY_F7           = 46;
     VKEY_F8           = 47;
     VKEY_F9           = 48;
     VKEY_F10          = 49;
     VKEY_F11          = 50;
     VKEY_F12          = 51;
     VKEY_NUMLOCK      = 52;
     VKEY_SCROLL       = 53;

     VKEY_SHIFT        = 54;
     VKEY_CONTROL      = 55;
     VKEY_ALT          = 56;

     VKEY_EQUALS       = 57;


//------------------------------------------------------------------------------
// Modifier flags used in VstKeyCode
//------------------------------------------------------------------------------
type
    VstModifierKey = VstInt32;

const
     MODIFIER_SHIFT     = 1 shl 0;  // Shift
     MODIFIER_ALTERNATE = 1 shl 1;  // Alt
     MODIFIER_COMMAND   = 1 shl 2;  // Control on Mac
     MODIFIER_CONTROL   = 1 shl 3;  // Ctrl on PC, Apple on Mac


//------------------------------------------------------------------------------
// File filter used in VstFileSelect
//------------------------------------------------------------------------------
type
    PVstFileType = ^VstFileType;
    VstFileType = record
      name      : array[0..127] of AnsiChar;
      macType   : array[0..7] of AnsiChar;
      dosType   : array[0..7] of AnsiChar;
      unixType  : array[0..7] of AnsiChar;
      mimeType1 : array[0..127] of AnsiChar;
      mimeType2 : array[0..127] of AnsiChar;
    end;

//------------------------------------------------------------------------------
// File Selector Description used in audioMasterOpenFileSelector
//------------------------------------------------------------------------------
type
    PVstFileSelect = ^VstFileSelect;
    VstFileSelect = record
      command             : VstInt32;                  // see VstFileSelectCommand
      vType               : VstInt32;                  // see VstFileSelectType
      macCreator          : VstInt32;                  // optional: 0 = no creator
      nbFileTypes         : VstInt32;                  // number of fileTypes
      fileTypes           : PVstFileType;		           // list of fileTypes. See VstFileType
      title               : array[0..1023] of AnsiChar;// text to display in file selector's title
      initialPath         : PAnsiChar;                 // initial path
      returnPath          : PAnsiChar;                 // use with #kVstFileLoad and #kVstDirectorySelect. null: Host allocates memory, plug-in must call #closeOpenFileSelector!
      sizeReturnPath      : VstInt32;                  // size of allocated memory for return paths
      returnMultiplePaths : ^PAnsiChar;                // use with kVstMultipleFilesLoad. Host allocates memory, plug-in must call #closeOpenFileSelector!
      nbReturnPath        : VstInt32;                  // number of selected paths
      reserved            : VstIntPtr;                 // reserved for Host application

      future              : array[0..115] of byte;     // reserved for future use
    end;

//------------------------------------------------------------------------------
// Command constants used in VstFileSelect structure
//------------------------------------------------------------------------------
type
    VstFileSelectCommand = VstInt32;

const
     kVstFileLoad           = 0;   // for loading a file
     kVstFileSave           = 1;   // for saving a file
     kVstMultipleFilesLoad  = 2;   // for loading multiple files
     kVstDirectorySelect    = 3;   // for selecting a directory/folder


//------------------------------------------------------------------------------
// Types used in VstFileSelect structure
//------------------------------------------------------------------------------
type
    VstFileSelectType = VstInt32;

const
     kVstFileType = 0;   // regular file selector


//------------------------------------------------------------------------------
// Structure used for effBeginLoadBank/effBeginLoadProgram
//------------------------------------------------------------------------------
type
    PVstPatchChunkInfo = ^VstPatchChunkInfo;
    VstPatchChunkInfo = record
      version        : VstInt32;                // Format Version (should be 1)
      pluginUniqueID : VstInt32;                // UniqueID of the plug-in
      pluginVersion  : VstInt32;                // Plug-in Version
      numElements    : VstInt32;                // Number of Programs (Bank) or Parameters (Program)

      future         : array[0..47] of AnsiChar;    // Reserved for future use
    end;


//------------------------------------------------------------------------------
// PanLaw Type
//------------------------------------------------------------------------------
type
    VstPanLawType = VstInt32;

const
     kLinearPanLaw     = 0;  // L = pan * M; R = (1 - pan) * M;
     kEqualPowerPanLaw = 1;  // L = pow (pan, 0.5) * M; R = pow ((1 - pan), 0.5) * M;


//------------------------------------------------------------------------------
// Process Levels returned by audioMasterGetCurrentProcessLevel
//------------------------------------------------------------------------------
type
    VstProcessLevels = VstInt32;

const
     kVstProcessLevelUnknown  = 0;   // not supported by Host
     kVstProcessLevelUser     = 1;   // 1: currently in user thread (GUI)
     kVstProcessLevelRealtime = 2;   // 2: currently in audio thread (where process is called)
     kVstProcessLevelPrefetch = 3;   // 3: currently in 'sequencer' thread (MIDI, timer etc)
     kVstProcessLevelOffline  = 4;   // 4: currently offline processing and thus in user thread


//------------------------------------------------------------------------------
// Automation States returned by audioMasterGetAutomationState
//------------------------------------------------------------------------------
type
    VstAutomationStates = VstInt32;

const
     kVstAutomationUnsupported = 0;    // not supported by Host
     kVstAutomationOff         = 1;    // off
     kVstAutomationRead        = 2;    // read
     kVstAutomationWrite       = 3;    // write
     kVstAutomationReadWrite   = 4;    // read and write



implementation

end.
