//******************************************************************************
//
//  DAEffect.pas
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
// Filename     : aeffect.h
// Created by   : Steinberg Media Technologies
// Description  : Definition of AEffect structure
//
// © 2006, Steinberg Media Technologies, All Rights Reserved
//
//******************************************************************************
unit TestHost.Vst2.DAEffect;

interface

{$INCLUDE DVstCommon.inc}

//------------------------------------------------------------------------------
// VST Version
//------------------------------------------------------------------------------

// Current VST Version
const
     {$IFDEF VST_2_4_EXTENSIONS}
     kVstVersion = 2400;
     {$ELSE} {$IFDEF VST_2_3_EXTENSIONS}
	   kVstVersion = 2300;
     {$ELSE} {$IFDEF VST_2_2_EXTENSIONS}
     kVstVersion = 2200;
     {$ELSE} {$IFDEF VST_2_1_EXTENSIONS}
     kVstVersion = 2100;
     {$ELSE}
     kVstVersion = 2;
     {$ENDIF} // VST_2_1_EXTENSIONS
     {$ENDIF} // VST_2_2_EXTENSIONS
     {$ENDIF} // VST_2_3_EXTENSIONS
     {$ENDIF} // VST_2_4_EXTENSIONS

//-------------------------------------------------------------------------------------------------------
// Integral Types
//-------------------------------------------------------------------------------------------------------
type
    VstInt16 = smallint;    // 16 bit integer type
    VstInt32 = longint;     // 32 bit integer type
    VstInt64 = int64;       // 64 bit integer type

    // pointers
    PVstInt16 = ^VstInt16;
    PVstInt32 = ^VstInt32;
    PVstInt64 = ^VstInt64;

//-------------------------------------------------------------------------------------------------------
// Generic Types
//-------------------------------------------------------------------------------------------------------
type
    // platform-dependent integer type, same size as pointer
    {$IFDEF VST_64BIT_PLATFORM}
    VstIntPtr = VstInt64;
    {$ELSE}
    VstIntPtr = VstInt32;
    {$ENDIF}

    // pointer
    PVstIntPtr = ^VStIntPtr;

//------------------------------------------------------------------------------
// Misc. Definition
//------------------------------------------------------------------------------

// AEffect magic number
const
    kEffectMagic:ansistring = 'VstP';

type
    PAEffect = ^AEffect;

    PPSingle = ^psingle;
    PPDouble = ^pdouble;

    TAudioMasterCallbackFunc = function(effect: PAEffect; opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; cdecl;
    TDispatcherFunc = function(effect: PAEffect; opcode, index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr; cdecl;
    TProcessProc = procedure(effect: PAEffect; inputs, outputs: PPSingle; sampleframes: VstInt32); cdecl;
    TProcessDoubleProc = procedure(effect: PAEffect; inputs, outputs: PPDouble; sampleFrames: VstInt32); cdecl;
    TSetParameterProc = procedure(effect: PAEffect; index: VstInt32; parameter: single); cdecl;
    TGetParameterFunc = function(effect: PAEffect; index: VstInt32): single; cdecl;

    // prototype for plug-in main
    TMainProc = function(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl;



//------------------------------------------------------------------------------
// Basic VST Effect Interface
//------------------------------------------------------------------------------
    AEffect = record
      magic                  : VstInt32;             // must be kEffectMagic ('VstP')
      dispatcher             : TDispatcherFunc;
      process                : TProcessProc;         // (deprecated) Accumulating process mode is deprecated in VST 2.4! Use AEffect.processReplacing instead!
      setParameter           : TSetParameterProc;    // Set new value of automatable parameter. See AudioEffect.setParameter
      getParameter           : TGetParameterFunc;    // Returns current value of automatable parameter. See AudioEffect.getParameter
      numPrograms            : VstInt32;             // number of programs
      numParams              : VstInt32;	           // all programs are assumed to have numParams parameters
      numInputs              : VstInt32;	           // number of audio inputs
      numOutputs             : VstInt32;	           // number of audio outputs
      flags                  : VstInt32;		         // see VstAEffectFlags
      reservedForHost        : pointer;              // reserved for Host, must be 0
      resvd2                 : VstIntPtr;		         // reserved for Host, must be 0
      initialDelay           : VstInt32;	           // for algorithms which need input in the first place (Group delay or latency in samples). This value should be initialized in a resume state
      realQualities          : VstInt32;	           // (deprecated) unused
      offQualities           : VstInt32;	           // (deprecated) unused
      ioRatio                : single;		           // (deprecated) unused
      vObject                : pointer;		           // AudioEffect class pointer
      user                   : pointer;		           // user-defined pointer (for use by the plugin)
      uniqueID               : VstInt32;	           // registered unique identifier (register it at Steinberg 3rd party support Web). This is used to identify a plug-in during save+load of preset and project.
      version                : VstInt32;		         // plug-in version (example 1100 for version 1.1.0.0)
      processReplacing       : TProcessProc;         // process audio samples in replacing mode. See AudioEffect.processReplacing

      {$IFDEF VST_2_4_EXTENSIONS}
      processDoubleReplacing : TProcessDoubleProc;   // process double-precision audio samples in replacing mode @see AudioEffect::processDoubleReplacing
      future                 : array[0..55] of byte; // reserved for future use (please zero)
      {$ELSE}
      future                 : array[0..59] of byte; // reserved for future use (please zero)
      {$ENDIF}
    end;


//------------------------------------------------------------------------------
// AEffect flags
//------------------------------------------------------------------------------
type
    VstAEffectFlags = VstInt32;

const
     //-------------------------------------------------------------------------
     effFlagsHasEditor          = 1 shl 0;   // set if the plug-in provides a custom editor
     effFlagsCanReplacing       = 1 shl 4;   // supports replacing process mode (which should the default mode in VST 2.4)
     effFlagsProgramChunks      = 1 shl 5;   // program data is handled in formatless chunks
     effFlagsIsSynth            = 1 shl 8;   // plug-in is a synth (VSTi), Host may assign mixer channels for its outputs
     effFlagsNoSoundInStop      = 1 shl 9;   // plug-in does not produce sound when input is all silence
     {$IFDEF VST_2_4_EXTENSIONS}
     effFlagsCanDoubleReplacing = 1 shl 12;  // plug-in supports double precision processing
     {$ENDIF}

     {$IFDEF VST_USE_DEPRECATED}
     effFlagsHasClip      = 1 shl 1;    // (deprecated) deprecated in VST 2.4
     effFlagsHasVu        = 1 shl 2;    // (deprecated) deprecated in VST 2.4
     effFlagsCanMono      = 1 shl 3;    // (deprecated) deprecated in VST 2.4
     effFlagsExtIsAsync   = 1 shl 10;   // (deprecated) deprecated in VST 2.4
     effFlagsExtHasBuffer = 1 shl 11;   // (deprecated) deprecated in VST 2.4
     {$ENDIF}
     //-------------------------------------------------------------------------



//------------------------------------------------------------------------------
// Basic dispatcher Opcodes (Host to Plug-in)
//------------------------------------------------------------------------------
type
    AEffectOpcodes = VstInt32;

const
     effOpen                    = 0;   // no arguments. See AudioEffect.open
     effClose                   = 1;   // no arguments. See AudioEffect.close

     effSetProgram              = 2;   // [value] new program number. See AudioEffect.setProgram
     effGetProgram              = 3;   // [return value] current program number. See AudioEffect.getProgram
     effSetProgramName          = 4;   // [ptr] PAnsiChar with new program name, limited to kVstMaxProgNameLen. See AudioEffect.setProgramName
     effGetProgramName          = 5;   // [ptr] AnsiChar buffer for current program name, limited to kVstMaxProgNameLen. See AudioEffect.getProgramName

     effGetParamLabel           = 6;   // [ptr] AnsiChar buffer for parameter label, limited to kVstMaxParamStrLen. See AudioEffect.getParameterLabel
     effGetParamDisplay         = 7;   // [ptr] AnsiChar buffer for parameter display, limited to kVstMaxParamStrLen. See AudioEffect.getParameterDisplay
     effGetParamName            = 8;   // [ptr] AnsiChar buffer for parameter name, limited to kVstMaxParamStrLen. See AudioEffect.getParameterName

     effSetSampleRate           = 10;  // [opt] new sample rate for audio processing. See AudioEffect.setSampleRate
     effSetBlockSize            = 11;  // [value] new maximum block size for audio processing. See AudioEffect.setBlockSize
     effMainsChanged            = 12;  // [value] 0 means "turn off", 1 means "turn on". See AudioEffect.suspend & AudioEffect.resume

     effEditGetRect             = 13;  // [ptr] pointer to a PERect receiving pointer to editor size. See ERect & AEffEditor.getRect
     effEditOpen                = 14;  // [ptr] system dependent Window pointer, e.g. HWND on Windows. See AEffEditor.open
     effEditClose               = 15;  // no arguments. See AEffEditor.close

     effEditIdle                = 19;  // no arguments. See AEffEditor.idle

     effGetChunk                = 23;  // [ptr] pointer to a pointer for chunk data address [index] 0 for bank, 1 for program. See AudioEffect.getChunk
     effSetChunk                = 24;  // [ptr] chunk data [value] byte size [index] 0 for bank, 1 for program. See AudioEffect.setChunk

     effNumOpcodes              = 25;

     {$IFDEF VST_USE_DEPRECATED}
     effGetVu                   = 9;   // (deprecated) deprecated in VST 2.4
     effIdentify                = 22;  // (deprecated) deprecated in VST 2.4
     {$ENDIF}




//------------------------------------------------------------------------------
// Basic dispatcher Opcodes (Plug-in to Host)
//------------------------------------------------------------------------------
type
    AudioMasterOpcodes = VstInt32;

const
     audioMasterAutomate     = 0;  // [index]: parameter index [opt]: parameter value. See AudioEffect.setParameterAutomated
     audioMasterVersion      = 1;  // [return value]: Host VST version (for example 2400 for VST 2.4). See AudioEffect.getMasterVersion
     audioMasterCurrentId    = 2;  // [return value]: current unique identifier on shell plug-in. See AudioEffect.getCurrentUniqueId
     audioMasterIdle         = 3;  // no arguments. See AudioEffect.masterIdle
     {$IFDEF VST_USE_DEPRECATED}
     audioMasterPinConnected = 4;  // (deprecated) [return value]: 0=true, 1=false [index]: pin index [value]: 0=input, 1=output. See AudioEffect.isInputConnected & AudioEffect.isOutputConnected
     {$ENDIF}


//------------------------------------------------------------------------------
// String length limits (in characters excl. 0 byte)
//------------------------------------------------------------------------------
type
    VstStringConstants = VstInt32;

const
     kVstMaxProgNameLen   = 24;  // used for effGetProgramName, effSetProgramName, effGetProgramNameIndexed
     kVstMaxParamStrLen   = 8;   // used for effGetParamLabel, effGetParamDisplay, effGetParamName
     kVstMaxVendorStrLen  = 64;  // used for effGetVendorString, audioMasterGetVendorString
     kVstMaxProductStrLen = 64;  // used for effGetProductString, audioMasterGetProductString
     kVstMaxEffectNameLen = 32;  // used for effGetEffectName

//------------------------------------------------------------------------------
// Structure used for effEditGetRect.
//------------------------------------------------------------------------------
type
    PPERect = ^PERect;
    PERect = ^ERect;
    ERect = record
      top    : VstInt16;   // top coordinate
      left   : VstInt16;   // left coordinate
      bottom : VstInt16;   // bottom coordinate
      right  : VstInt16;   // right coordinate
    end;

implementation

end.
