(*
  This file is a part of New Audio Components package v 2.6
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: neaac.pas 1234 2010-07-20 20:18:25Z andrei.borovsky $ *)

unit neaac;

interface

uses
  Windows,
  SysUtils,
  ACS_Classes;

const
  LibneaacPath = 'libfaad2p.dll';

var
  LibneaacLoaded : Boolean = False;


const
  MAIN = 1;
  LC  = 2;
  SSR = 3;
  LTP = 4;
  HE_AAC = 5;
  ER_LC = 17;
  ER_LTP = 19;
  LD = 23;
  DRM_ER_LC = 27; //* special object type for DRM */

//* header types */
  RAW = 0;
  ADIF = 1;
  ADTS = 2;
  LATM = 3;

//* SBR signalling */
  NO_SBR = 0;
  SBR_UPSAMPLED = 1;
  SBR_DOWNSAMPLED = 2;
  NO_SBR_UPSAMPLED = 3;

//* library output formats */
  FAAD_FMT_16BIT = 1;
  FAAD_FMT_24BIT = 2;
  FAAD_FMT_32BIT = 3;
  FAAD_FMT_FLOAT = 4;
  FAAD_FMT_FIXED = FAAD_FMT_FLOAT;
  FAAD_FMT_DOUBLE = 5;

//* Capabilities */
  LC_DEC_CAP = 1; //* Can decode LC */
  MAIN_DEC_CAP = 2; //* Can decode MAIN */
  LTP_DEC_CAP = 4; //* Can decode LTP */
  LD_DEC_CAP = 8; //* Can decode LD */
  ERROR_RESILIENCE_CAP = 16; //* Can decode ER */
  FIXED_POINT_CAP = 32; //* Fixed point */

//* Channel definitions */
  FRONT_CHANNEL_CENTER = 1;
  FRONT_CHANNEL_LEFT = 2;
  FRONT_CHANNEL_RIGHT = 3;
  SIDE_CHANNEL_LEFT = 4;
  SIDE_CHANNEL_RIGHT = 5;
  BACK_CHANNEL_LEFT = 6;
  BACK_CHANNEL_RIGHT = 7;
  BACK_CHANNEL_CENTER = 8;
  LFE_CHANNEL = 9;
  UNKNOWN_CHANNEL = 0;

//* DRM channel definitions */
  DRMCH_MONO = 1;
  DRMCH_STEREO = 2;
  DRMCH_SBR_MONO = 3;
  DRMCH_SBR_STEREO = 4;
  DRMCH_SBR_PS_STEREO = 5;

  DecConf_defObjectType =	1;
  DecConf_defSampleRate	= 2;
  DecConf_outputFormat = 3;
  DecConf_downMatrix = 4;
  DecConf_useOldADTSFormat = 5;
  DecConf_dontUpSampleImplicitSBR	= 6;

  mp4ASC_objectTypeIndex = 1;
  mp4ASC_samplingFrequencyIndex	= 2;
  mp4ASC_samplingFrequency = 3;
  mp4ASC_channelsConfiguration = 4;

  FrameInfo_bytesconsumed	= 1;
  FrameInfo_samples	= 2;
  FrameInfo_channels = 3;
  FrameInfo_error	= 4;
  FrameInfo_samplerate = 5;

//* A decode call can eat up to FAAD_MIN_STREAMSIZE bytes per decoded channel,
//   so at least so much bytes per channel should be available in this stream */

  FAAD_MIN_STREAMSIZE = 768; //* 6144 bits/channel */


type
  NeAACDecHandle = Pointer;

mp4AudioSpecificConfig = record
    objectTypeIndex : Byte;
    samplingFrequencyIndex : Byte;
    samplingFrequency : LongWord;
    channelsConfiguration : Byte;

    //* GA Specific Info */
    frameLengthFlag : Byte;
    dependsOnCoreCoder : Byte;
    coreCoderDelay : Word;
    extensionFlag : Byte;
    aacSectionDataResilienceFlag : Byte;
    aacScalefactorDataResilienceFlag : Byte;
    aacSpectralDataResilienceFlag : Byte;
    epConfig : Byte;

    sbr_present_flag : Byte;
    forceUpSampling : Byte;
    downSampledSBR : Byte;
end;

Pmp4AudioSpecificConfig = ^mp4AudioSpecificConfig;

NeAACDecConfiguration = record
    defObjectType : Byte;
    defSampleRate : LongWord;
    outputFormat : Byte;
    downMatrix : Byte;
    useOldADTSFormat : Byte;
    dontUpSampleImplicitSBR : Byte;
end;
NeAACDecConfigurationPtr = ^NeAACDecConfiguration;

NeAACDecFrameInfo = record
    bytesconsumed : LongWord;
    samples : LongWord;
    channels : Byte;
    error : Byte;
    samplerate : LongWord;

    //* SBR: 0: off, 1: on; upsample, 2: on; downsampled, 3: off; upsampled */
    sbr : Byte;

    //* MPEG-4 ObjectType */
    object_type : Byte;

    //* AAC header type; MP4 will be signalled as RAW also */
    header_type : Byte;

    //* multichannel configuration */
    num_front_channels : Byte;
    num_side_channels : Byte;
    num_back_channels : Byte;
    num_lfe_channels : Byte;
    channel_position : array[0..63] of Byte;

    //* PS: 0: off, 1: on */
    ps : Byte;
end;
PNeAACDecFrameInfo = ^NeAACDecFrameInfo;

AACSetDecConf_t = procedure(_struct : NeAACDecConfigurationPtr; name, value : LongWord); cdecl;

AACGetDecConf_t = function(_struct : NeAACDecConfigurationPtr; name : LongWord) : LongWord; cdecl;

AACGetFrameInfo_t = function(struct : PNeAACDecFrameInfo; name : LongWord) : LongWord; cdecl;

AACNewFrameInfo_t = function : PNeAACDecFrameInfo; cdecl;

AACDeleteFrameInfo_t = procedure(fi : PNeAACDecFrameInfo); cdecl;

NeAACDecGetErrorMessage_t = function(errcode : Byte) : PAnsiChar; cdecl;

NeAACDecGetCapabilities_t = function : LongWord; cdecl;

NeAACDecOpen_t = function : NeAACDecHandle; cdecl;

NeAACDecGetCurrentConfiguration_t = function(hDecoder : NeAACDecHandle) : NeAACDecConfigurationPtr; cdecl;

NeAACDecSetConfiguration_t = function(hDecoder : NeAACDecHandle; config : NeAACDecConfigurationPtr) : Byte; cdecl;

//* Init the library based on info from the AAC file (ADTS/ADIF) */
NeAACDecInit_t = function(hDecoder : NeAACDecHandle;
                              buffer : PByte;
                              buffer_size : LongWord;
                              var samplerate : LongWord;
                              var channels : Byte) : Integer; cdecl;

//* Init the library using a DecoderSpecificInfo */
NeAACDecInit2_t = function(hDecoder : NeAACDecHandle;
                               pBuffer : PByte;
                               SizeOfDecoderSpecificInfo : LongWord;
                              var samplerate : LongWord;
                              var channels : Byte) : Byte; cdecl;

//* Init the library for DRM */
NeAACDecInitDRM_t = function(hDecoder : NeAACDecHandle; samplerate : LongWord;
                                 channels : Byte) : Byte; cdecl;

NeAACDecPostSeekReset_t = procedure(hDecoder : NeAACDecHandle; frame : Integer); cdecl;

NeAACDecClose_t = procedure(hDecoder : NeAACDecHandle); cdecl;

NeAACDecDecode_t = function(hDecoder : NeAACDecHandle;
                                 hInfo : PNeAACDecFrameInfo;
                                 buffer : PByte;
                                 buffer_size : LongWord) : Pointer; cdecl;

NeAACDecDecode2_t = function(hDecoder : NeAACDecHandle;
					   hInfo : PNeAACDecFrameInfo;
                                 buffer : PByte;
                                 buffer_size : LongWord;                                  
					   var sample_buffer : Pointer;
                                 sample_buffer_size : LongWord) : Pointer; cdecl;

NeAACDecAudioSpecificConfig_t = function(pBuffer : PByte; buffer_size : LongWord;
                                             var mp4ASC : mp4AudioSpecificConfig) : Byte;  cdecl;


var
  AACSetDecConf : AACSetDecConf_t;
  AACGetDecConf : AACGetDecConf_t;
  AACGetFrameInfo : AACGetFrameInfo_t;
  AACNewFrameInfo : AACNewFrameInfo_t;
  AACDeleteFrameInfo : AACDeleteFrameInfo_t;
  NeAACDecGetErrorMessage : NeAACDecGetErrorMessage_t;
  NeAACDecGetCapabilities : NeAACDecGetCapabilities_t;
  NeAACDecOpen : NeAACDecOpen_t;
  NeAACDecGetCurrentConfiguration : NeAACDecGetCurrentConfiguration_t;
  NeAACDecSetConfiguration : NeAACDecSetConfiguration_t;
  NeAACDecInit : NeAACDecInit_t;
  NeAACDecInit2 : NeAACDecInit2_t;
  NeAACDecInitDRM : NeAACDecInitDRM_t;
  NeAACDecPostSeekReset : NeAACDecPostSeekReset_t;
  NeAACDecClose : NeAACDecClose_t;
  NeAACDecDecode : NeAACDecDecode_t;
  NeAACDecDecode2 : NeAACDecDecode2_t;
  NeAACDecAudioSpecificConfig : NeAACDecAudioSpecificConfig_t;

procedure Loadneaac;
procedure Freeneaac;

implementation

var
  hlib: THandle;

procedure Freeneaac;
begin
  LoadLibCS.Enter;
  if LibneaacLoaded then
  FreeLibrary(hlib);
  LibneaacLoaded := False;
  LoadLibCS.Leave;
end;

procedure Loadneaac;
begin
  LoadLibCS.Enter;
  if LibneaacLoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  hlib := LoadLibrary(LibneaacPath);
  LibneaacLoaded := hlib <> 0;

  AACSetDecConf := GetProcAddress(hlib, 'AACSetDecConf');
  AACGetDecConf := GetProcAddress(hlib, 'AACGetDecConf');
  AACGetFrameInfo := GetProcAddress(hlib, 'AACGetFrameInfo');
  AACNewFrameInfo := GetProcAddress(hlib, 'AACNewFrameInfo');
  AACDeleteFrameInfo := GetProcAddress(hlib, 'AACDeleteFrameInfo');
  NeAACDecGetErrorMessage := GetProcAddress(hlib, 'NeAACDecGetErrorMessage');
  NeAACDecGetCapabilities := GetProcAddress(hlib, 'NeAACDecGetCapabilities');
  NeAACDecOpen := GetProcAddress(hlib, 'NeAACDecOpen');
  NeAACDecGetCurrentConfiguration := GetProcAddress(hlib, 'NeAACDecGetCurrentConfiguration');
  NeAACDecSetConfiguration := GetProcAddress(hlib, 'NeAACDecSetConfiguration');
  NeAACDecInit := GetProcAddress(hlib, 'NeAACDecInit');
  NeAACDecInit2 := GetProcAddress(hlib, 'NeAACDecInit2');
  NeAACDecInitDRM := GetProcAddress(hlib, 'NeAACDecInitDRM');
  NeAACDecPostSeekReset := GetProcAddress(hlib, 'NeAACDecPostSeekReset');
  NeAACDecClose := GetProcAddress(hlib, 'NeAACDecClose');
  NeAACDecDecode := GetProcAddress(hlib, 'NeAACDecDecode');
  NeAACDecDecode2 := GetProcAddress(hlib, 'NeAACDecDecode2');
  NeAACDecAudioSpecificConfig := GetProcAddress(hlib, 'NeAACDecAudioSpecificConfig');
  LoadLibCS.Leave;
end;
end.
