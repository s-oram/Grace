unit tak_decoder;

interface

uses
  SysUtils, Classes;

const
  TAKDLL_Name = 'tak_deco_lib.dll';

var
  TAKDLL_Loaded: Boolean = False;

type

  TtakSeekableStreamDecoder = type Pointer;
  TtakAPEv2Tag = type Pointer;

  { class ETAKException }

  ETAKException = class(Exception)
  end;

  { class TTAKDecoder }

  TTAKDecoder = class
  private
    FStream: TStream;
    FInstance: TtakSeekableStreamDecoder;
    FTag: TtakAPEv2Tag;

    FSampleBits: Cardinal;
    FSampleRate: Cardinal;
    FChannelNum: Cardinal;
    FSampleNum: Cardinal;

    function GetTagPresents(const TagId: String): Boolean;
    function GetTagValue(const TagId: String): WideString;
  public
    constructor Create(AStream: TStream); reintroduce;
    destructor Destroy; override;

    function Seek(const Sample: Int64): Boolean;
    function Read(Buffer: Pointer; SampleCount: Integer): Integer;

    property SampleBits: Cardinal read FSampleBits;
    property SampleRate: Cardinal read FSampleRate;
    property ChannelNum: Cardinal read FChannelNum;
    property SampleNum: Cardinal read FSampleNum;

    property TagPresents[const TagId: String]: Boolean read GetTagPresents;
    property TagValue[const TagId: String]: WideString read GetTagValue;
  end;

implementation

uses
  Windows;


{=== TAK SDK ========================================================}

{
  Software Developement Kit for TAK, (T)om's lossless (A)udio (K)ompressor:
  Decoder library.

  Version:  1.0.5
  Date:     07-12-05
  Language: Pascal (Delphi 6.0)

  Copyright 2007 by Thomas Becker, D-49080 Osnabrueck.
  All rights reserved.
}


{=== Elementary Types ===============================================}

type
  TtakInt64  = Int64;
  TtakInt32  = Integer;
  TtakUInt32 = Cardinal;
  TtakBool   = TtakInt32;

const
  tak_False = 0;
  tak_True  = 1;



{=== Function results and errors ====================================}

type
  TtakResult = TtakInt32;

const
  tak_ErrorStringLenMax  = 60;
  tak_ErrorStringSizeMax = tak_ErrorStringLenMax + 1;

const
  tak_res_Ok = 0;

  {---    1 to 1023: Object results ---}

  {--- 1024 to 2047: System errors ---}

  tak_res_InternalError       = 1024;
  tak_res_NotImplemented      = 1025;
  tak_res_IncompatibleVersion = 1026;
  tak_res_OutOfMemory         = 1027;

  {--- 2048 to 3071: User errors ---}

  tak_res_InvalidParameter   = 2048;
  tak_res_InvalidIoInterface = 2049;
  tak_res_InvalidMode        = 2050;
  tak_res_BufferTooSmall     = 2051;
  tak_res_NotEnoughAudioData = 2052;
  tak_res_TooMuchAudioData   = 2053;


{=== System =========================================================}

const
  tak_MaxPathLen  = 256;
  tak_MaxPathSize = tak_MaxPathLen + 1;

type
  TtakCpuOptions = TtakInt32;

const
  tak_Cpu_Asm  = $0001;
  tak_Cpu_MMX  = $0002;
  tak_Cpu_SSE  = $0004;
  tak_Cpu_None = $0000;
  tak_Cpu_Any  = tak_Cpu_Asm or tak_Cpu_MMX or tak_Cpu_SSE;



{=== Library ========================================================}

const
  tak_InterfaceVersion = $010001;


type

  t_tak_GetLibraryVersion_func = function (
    var AVersion       : TtakInt32;
    var ACompatibility : TtakInt32) : TtakResult; cdecl;

var

  tak_GetLibraryVersion: t_tak_GetLibraryVersion_func = nil;


{=== Audio-Format ===================================================}

const
  tak_AudioFormat_DataType_PCM = 0;

type
  TtakAudioFormat = packed record
    DataType   : TtakInt32;
    SampleRate : TtakInt32;
    SampleBits : TtakInt32;
    ChannelNum : TtakInt32;
    BlockSize  : TtakInt32;
  end;



{=== Codecs =========================================================}

const
  tak_CodecNameLenMax  = 30;
  tak_CodecNameSizeMax = tak_CodecNameLenMax + 1;


type

  t_tak_GetCodecName_func = function (
    ACodec    : TtakInt32;
    AName     : PChar;
    ANameSize : TtakInt32) : TtakResult; cdecl;

var

  tak_GetCodecName: t_tak_GetCodecName_func = nil;


{=== Presets ========================================================}


  {-- Presets/Profiles ---}

type
  TtakPresets = TtakInt32;


  {-- Evaluation ---}

type
  TtakPresetEvaluations = TtakInt32;

const
  tak_PresetEval_Standard = 0;
  tak_PresetEval_Extra    = 1;
  tak_PresetEval_Max      = 2;
  tak_PresetEval_First    = tak_PresetEval_Standard;
  tak_PresetEval_Last     = tak_PresetEval_Max;
  tak_PresetEval_Num      = tak_PresetEval_Last - tak_PresetEval_First + 1;



{=== Stream / Container =============================================}

const
  tak_FrameSizeMax              = 16384;
  tak_FrameDurationMax          = 250;
  tak_str_SimpleWaveDataSizeMax = 1024 * 1024;

type
  Ttak_str_EncoderInfo = packed record
    Codec   : TtakInt32;
    Profile : TtakPresets;
  end;

type
  TtakFrameSizeTypes = TtakInt32;

const
  tak_FrameSizeType_94_ms  =  0;
  tak_FrameSizeType_125_ms =  1;
  tak_FrameSizeType_188_ms =  2;
  tak_FrameSizeType_250_ms =  3;
  tak_FrameSizeType_4096   =  4;
  tak_FrameSizeType_8192   =  5;
  tak_FrameSizeType_16384  =  6;
  tak_FrameSizeType_512    =  7;
  tak_FrameSizeType_1024   =  8;
  tak_FrameSizeType_2048   =  9;
  tak_FrameSizeType_6144   = 10;
  tak_FrameSizeType_12288  = 11;

  tak_FrameSizeType_First  = tak_FrameSizeType_94_ms;
  tak_FrameSizeType_Last   = tak_FrameSizeType_12288;
  tak_FrameSizeType_Num    = tak_FrameSizeType_Last - tak_FrameSizeType_First + 1;

type
  Ttak_str_SizeInfo = packed record
    FrameSize          : TtakFrameSizeTypes;
    FrameSizeInSamples : TtakInt32;
    SampleNum          : TtakInt64;
  end;

type
  Ttak_str_StreamInfo = packed record
    Encoder : Ttak_str_EncoderInfo;
    Sizes   : Ttak_str_SizeInfo;
    Audio   : TtakAudioFormat;
  end;

type
  Ttak_str_SimpleWaveDataHeader = packed record
    HeadSize : TtakInt32;
    TailSize : TtakInt32;
  end;

type
  Ttak_str_MetaEncoderInfo = packed record
    Version    : TtakInt32;
    Preset     : TtakPresets;
    Evaluation : TtakPresetEvaluations;
  end;



{=== TtakStreamIoInterface ==========================================}

type
  PtakStreamIoInterface = ^TtakStreamIoInterface;
  TtakStreamIoInterface = packed record
    CanRead   : function (AUser : Pointer) : TtakBool; cdecl;
    CanWrite  : function (AUser : Pointer) : TtakBool; cdecl;
    CanSeek   : function (AUser : Pointer) : TtakBool; cdecl;
    Read      : function (    AUser    : Pointer;
                              ABuf     : Pointer;
                              ANum     : TtakInt32;
                          var AReadNum : TtakInt32) : TtakBool; cdecl;
    Write     : function (AUser : Pointer;
                          ABuf  : Pointer;
                          ANum  : TtakInt32) : TtakBool; cdecl;
    Flush     : function (AUser : Pointer) : TtakBool; cdecl;
    Truncate  : function (AUser : Pointer) : TtakBool; cdecl;
    Seek      : function (AUser : Pointer;
                          APos  : TtakInt64) : TtakBool; cdecl;
    GetLength : function (    AUser   : Pointer;
                          var ALength : TtakInt64) : TtakBool; cdecl;
  end;



{=== APEv2-Tag (APE) ================================================}

const
  tak_apev2_Version    = 2000;
  tak_apev2_ItemNumMax = 100;
  tak_apev2_TagSizeMax = 16 * 1024 * 1024;

type
  TtakAPEv2ItemType = TtakInt32;

const
  tak_apev2_ItemType_Text     = 0;
  tak_apev2_ItemType_Binary   = 1;
  tak_apev2_ItemType_External = 2;
  tak_apev2_ItemType_Last     = tak_apev2_ItemType_External;

type
  TtakAPEv2ItemDesc = packed record
    ItemType  : TtakAPEv2ItemType;
    Flags     : TtakUInt32;
    KeySize   : TtakUInt32;
    ValueSize : TtakUInt32;
    ValueNum  : TtakInt32;
  end;

type
  TtakAPEv2TagDesc = packed record
    Version   : TtakUInt32;
    Flags     : TtakUInt32;
    StreamPos : TtakInt64;
    TotSize   : TtakInt64;
  end;


const

  {--- Warnings ---}

  tak_res_ape_NotAvail       = 1;
  tak_res_ape_InvalidType    = 2;
  tak_res_ape_BufferTooSmall = 3;

  {--- Fatal errors ---}

  tak_res_ape_None         = 4;
  tak_res_ape_Incompatible = 5;
  tak_res_ape_Invalid      = 6;
  tak_res_ape_IoErr        = 7;

  tak_res_ape_FatalErrorFirst = tak_res_ape_None;



{--- Info -----------------------------------------------------------}

type

  t_tak_APE_Valid_func = function (
    ATag : TtakAPEv2Tag) : TtakBool; cdecl;

  t_tak_APE_State_func = function (
    ATag : TtakAPEv2Tag) : TtakResult; cdecl;

  t_tak_APE_GetErrorString_func = function (
    AError      : TtakResult;
    AString     : PChar;
    AStringSize : TtakInt32) : TtakResult; cdecl;

  t_tak_APE_ReadOnly_func = function (
    ATag : TtakAPEv2Tag) : TtakBool; cdecl;

  t_tak_APE_GetDesc_func = function (
    ATag  : TtakAPEv2Tag;
    var ADesc : TtakAPEv2TagDesc) : TtakResult; cdecl;

  t_tak_APE_GetItemNum_func = function (
    ATag : TtakAPEv2Tag) : TtakInt32; cdecl;

var

  tak_APE_Valid: t_tak_APE_Valid_func = nil;

  tak_APE_State: t_tak_APE_State_func = nil;

  tak_APE_GetErrorString: t_tak_APE_GetErrorString_func = nil;

  tak_APE_ReadOnly: t_tak_APE_ReadOnly_func = nil;

  tak_APE_GetDesc: t_tak_APE_GetDesc_func = nil;

  tak_APE_GetItemNum: t_tak_APE_GetItemNum_func = nil;


{--- Items ----------------------------------------------------------}

type

  t_tak_APE_GetIndexOfKey_func = function (
    ATag : TtakAPEv2Tag;
    AKey : PChar;
    var AIdx : TtakInt32) : TtakResult; cdecl;

  t_tak_APE_GetItemDesc_func = function (
    ATag : TtakAPEv2Tag;
    AIdx  : TtakInt32;
    var ADesc : TtakAPEv2ItemDesc) : TtakResult; cdecl;

  t_tak_APE_GetItemKey_func = function (
    ATag     : TtakAPEv2Tag;
    AIdx     : TtakInt32;
    AKey     : PChar;
    AMaxSize : TtakInt32;
    var ASize    : TtakInt32) : TtakResult; cdecl;

  t_tak_APE_GetItemValue_func = function (
    ATag     : TtakAPEv2Tag;
    AIdx     : TtakInt32;
    AValue   : Pointer;
    AMaxSize : TtakInt32;
    var ASize    : TtakInt32) : TtakResult; cdecl;

  t_tak_APE_GetTextItemValueAsAnsi_func = function (
    ATag            : TtakAPEv2Tag;
    AIdx            : TtakInt32;
    AValueIdx       : TtakInt32;
    AValueSeparator : Char;
    AValue          : PChar;
    AMaxSize        : TtakInt32;
    var ASize           : TtakInt32) : TtakResult; cdecl;

var

  tak_APE_GetIndexOfKey: t_tak_APE_GetIndexOfKey_func = nil;

  tak_APE_GetItemDesc: t_tak_APE_GetItemDesc_func = nil;

  tak_APE_GetItemKey: t_tak_APE_GetItemKey_func = nil;

  tak_APE_GetItemValue: t_tak_APE_GetItemValue_func = nil;

  tak_APE_GetTextItemValueAsAnsi: t_tak_APE_GetTextItemValueAsAnsi_func = nil;


{=== Seekable Stream Decoder (SSD) ==================================}

type
  PtakSSDDamageItem = ^TtakSSDDamageItem;
  TtakSSDDamageItem = packed record
    SamplePosition : TtakInt64;
    SampleSize     : TtakInt64;
  end;
  TSSDDamageCallback = procedure (AUser   : Pointer;
                                  ADamage : PtakSSDDamageItem); cdecl;

const
  tak_ssd_opt_OpenWriteable     = $00000001;
  tak_ssd_opt_BufferInput       = $00000002;
  tak_ssd_opt_SequentialRead    = $00000004;
  tak_ssd_opt_SkipDamagedFrames = $00000008;

type
  TtakSSDOptions = packed record
    Cpu   : TtakCpuOptions;
    Flags : TtakInt32;
  end;


const

  //--- Warnings ---

  tak_res_ssd_MetaDataMissing = 1;

  //--- Errors ---

  tak_res_ssd_MetaDataDamaged = 2;
  tak_res_ssd_FrameDamaged    = 3;
  tak_res_ssd_ErrorFirst      = tak_res_ssd_MetaDataDamaged;

  //--- Fatal Errors ---

  tak_res_ssd_SourceIoError       = 4;
  tak_res_ssd_IncompatibleVersion = 5;
  tak_res_ssd_Undecodable         = 6;
  tak_res_ssd_FatalErrorFirst     = tak_res_ssd_SourceIoError;

type
  TtakSSDResult = packed record
    OpenResult          : TtakResult;
    SumResult           : TtakResult;
    StreamSampleNum     : TtakInt64;
    ReadSampleNum       : TtakInt64;
    DamagedSampleNum    : TtakInt64;
    SkippedDataBlockNum : TtakInt32;
  end;



{--- Create & Destroy -----------------------------------------------}

type

  t_tak_SSD_Create_FromFile_func = function (
    ASourcePath     : PChar;
    const AOptions        : TtakSSDOptions;
    ADamageCallback : TSSDDamageCallback;
    ACallbackUser   : Pointer): TtakSeekableStreamDecoder; cdecl;

  t_tak_SSD_Create_FromStream_func = function (
    ASourceStream     : PtakStreamIoInterface;
    ASourceStreamUser : Pointer;
    const AOptions    : TtakSSDOptions;
    ADamageCallback   : TSSDDamageCallback;
    ACallbackUser     : Pointer) : TtakSeekableStreamDecoder; cdecl;

  t_tak_SSD_Destroy_proc = procedure (
    ADecoder : TtakSeekableStreamDecoder); cdecl;

var

  tak_SSD_Create_FromFile: t_tak_SSD_Create_FromFile_func = nil;

  tak_SSD_Create_FromStream: t_tak_SSD_Create_FromStream_func = nil;

  tak_SSD_Destroy: t_tak_SSD_Destroy_proc = nil;


{--- Info -----------------------------------------------------------}

type

  t_tak_SSD_Valid_func = function (
    ADecoder : TtakSeekableStreamDecoder) : TtakBool; cdecl;

  t_tak_SSD_State_func = function (
    ADecoder : TtakSeekableStreamDecoder) : TtakResult; cdecl;

  t_tak_SSD_GetStateInfo_func = function (
    ADecoder  : TtakSeekableStreamDecoder;
    var AInfo : TtakSSDResult) : TtakResult; cdecl;

  t_tak_SSD_GetErrorString_func = function (
    AError      : TtakResult;
    AString     : PChar;
    AStringSize : TtakInt32) : TtakResult; cdecl;

  t_tak_SSD_GetStreamInfo_func = function (
    ADecoder  : TtakSeekableStreamDecoder;
    var AInfo : Ttak_str_StreamInfo) : TtakResult; cdecl;

  t_tak_SSD_GetFrameSize_func = function (
    ADecoder : TtakSeekableStreamDecoder) : TtakInt32; cdecl;

var

  tak_SSD_Valid: t_tak_SSD_Valid_func = nil;

  tak_SSD_State: t_tak_SSD_State_func = nil;

  tak_SSD_GetStateInfo: t_tak_SSD_GetStateInfo_func = nil;

  tak_SSD_GetErrorString: t_tak_SSD_GetErrorString_func = nil;

  tak_SSD_GetStreamInfo: t_tak_SSD_GetStreamInfo_func = nil;

  tak_SSD_GetFrameSize: t_tak_SSD_GetFrameSize_func = nil;


{--- IO -------------------------------------------------------------}

type

  t_tak_SSD_Seek_func = function (
    ADecoder   : TtakSeekableStreamDecoder;
    ASamplePos : TtakInt64) : TtakResult; cdecl;

  t_tak_SSD_ReadAudio_func = function (
    ADecoder     : TtakSeekableStreamDecoder;
    ASamples     : Pointer;
    ASampleNum   : TtakInt32;
    var AReadNum : TtakInt32) : TtakResult; cdecl;

  t_tak_SSD_GetReadPos_func = function (
    ADecoder : TtakSeekableStreamDecoder) : TtakInt64; cdecl;

  t_tak_SSD_GetCurFrameBitRate_func = function (
    ADecoder : TtakSeekableStreamDecoder) : TtakInt32; cdecl;

  t_tak_SSD_GetSimpleWaveDataDesc_func = function (
    ADecoder  : TtakSeekableStreamDecoder;
    var ADesc : Ttak_str_SimpleWaveDataHeader) : TtakResult; cdecl;

  t_tak_SSD_ReadSimpleWaveData_func = function (
    ADecoder : TtakSeekableStreamDecoder;
    ABuf     : Pointer;
    ABufSize : TtakInt32) : TtakResult; cdecl;

  t_tak_SSD_GetEncoderInfo_func = function (
    ADecoder  : TtakSeekableStreamDecoder;
    var AInfo : Ttak_str_MetaEncoderInfo) : TtakResult; cdecl;

  t_tak_SSD_GetAPEv2Tag_func = function (
    ADecoder : TtakSeekableStreamDecoder) : TtakAPEv2Tag; cdecl;

var

  tak_SSD_Seek: t_tak_SSD_Seek_func = nil;

  tak_SSD_ReadAudio: t_tak_SSD_ReadAudio_func = nil;

  tak_SSD_GetReadPos: t_tak_SSD_GetReadPos_func = nil;

  tak_SSD_GetCurFrameBitRate: t_tak_SSD_GetCurFrameBitRate_func = nil;

  tak_SSD_GetSimpleWaveDataDesc: t_tak_SSD_GetSimpleWaveDataDesc_func = nil;

  tak_SSD_ReadSimpleWaveData: t_tak_SSD_ReadSimpleWaveData_func = nil;

  tak_SSD_GetEncoderInfo: t_tak_SSD_GetEncoderInfo_func = nil;

  tak_SSD_GetAPEv2Tag: t_tak_SSD_GetAPEv2Tag_func = nil;


const
  tak_GetLibraryVersion_name = 'tak_GetLibraryVersion';
  tak_GetCodecName_name = 'tak_GetCodecName';
  tak_APE_Valid_name = 'tak_APE_Valid';
  tak_APE_State_name = 'tak_APE_State';
  tak_APE_GetErrorString_name = 'tak_APE_GetErrorString';
  tak_APE_ReadOnly_name = 'tak_APE_ReadOnly';
  tak_APE_GetDesc_name = 'tak_APE_GetDesc';
  tak_APE_GetItemNum_name = 'tak_APE_GetItemNum';
  tak_APE_GetIndexOfKey_name = 'tak_APE_GetIndexOfKey';
  tak_APE_GetItemDesc_name = 'tak_APE_GetItemDesc';
  tak_APE_GetItemKey_name = 'tak_APE_GetItemKey';
  tak_APE_GetItemValue_name = 'tak_APE_GetItemValue';
  tak_APE_GetTextItemValueAsAnsi_name = 'tak_APE_GetTextItemValueAsAnsi';
  tak_SSD_Create_FromFile_name = 'tak_SSD_Create_FromFile';
  tak_SSD_Create_FromStream_name = 'tak_SSD_Create_FromStream';
  tak_SSD_Destroy_name = 'tak_SSD_Destroy';
  tak_SSD_Valid_name = 'tak_SSD_Valid';
  tak_SSD_State_name = 'tak_SSD_State';
  tak_SSD_GetStateInfo_name = 'tak_SSD_GetStateInfo';
  tak_SSD_GetErrorString_name = 'tak_SSD_GetErrorString';
  tak_SSD_GetStreamInfo_name = 'tak_SSD_GetStreamInfo';
  tak_SSD_GetFrameSize_name = 'tak_SSD_GetFrameSize';
  tak_SSD_Seek_name = 'tak_SSD_Seek';
  tak_SSD_ReadAudio_name = 'tak_SSD_ReadAudio';
  tak_SSD_GetReadPos_name = 'tak_SSD_GetReadPos';
  tak_SSD_GetCurFrameBitRate_name = 'tak_SSD_GetCurFrameBitRate';
  tak_SSD_GetSimpleWaveDataDesc_name = 'tak_SSD_GetSimpleWaveDataDesc';
  tak_SSD_ReadSimpleWaveData_name = 'tak_SSD_ReadSimpleWaveData';
  tak_SSD_GetEncoderInfo_name = 'tak_SSD_GetEncoderInfo';
  tak_SSD_GetAPEv2Tag_name = 'tak_SSD_GetAPEv2Tag';

procedure CheckFunc(Func: Pointer; const FuncName: String);
begin
  if not TAKDLL_Loaded then
    raise ETAKException.CreateFmt(
      'TAK library "%s" not loaded!', [TAKDLL_Name]);
  if Func = nil then
    raise ETAKException.CreateFmt(
      'Function "%s" not found in TAK library!', [FuncName]);
end;

var
  stream_io_interface: TtakStreamIoInterface;

procedure DamageCallback(Input: TTAKDecoder; ADamage: PtakSSDDamageItem); cdecl;
begin
end;

{ class TTAKDecoder }

constructor TTAKDecoder.Create(AStream: TStream);
var
  options: TtakSSDOptions;
  stream_info: Ttak_str_StreamInfo;
begin
  Assert(AStream <> nil);

  inherited Create();

  CheckFunc(@tak_SSD_Create_FromStream, tak_SSD_Create_FromStream_name);

  FStream := AStream;

  options.Cpu := tak_Cpu_Any;
  options.Flags := 0;
  FInstance := tak_SSD_Create_FromStream(
    @stream_io_interface, FStream,
    options,
    @DamageCallback, Self);

  CheckFunc(@tak_SSD_GetStreamInfo, tak_SSD_GetStreamInfo_name);
  if tak_SSD_GetStreamInfo(FInstance, stream_info) <> tak_res_Ok then
    raise ETAKException.Create('Can not get stream info!');

  FSampleBits := stream_info.Audio.SampleBits;
  FSampleRate := stream_info.Audio.SampleRate;
  FChannelNum := stream_info.Audio.ChannelNum;
  FSampleNum := stream_info.Sizes.SampleNum;

  CheckFunc(@tak_SSD_GetAPEv2Tag, tak_SSD_GetAPEv2Tag_name);
  FTag := tak_SSD_GetAPEv2Tag(FInstance);

  if FTag <> nil then begin
    CheckFunc(@tak_APE_Valid, tak_APE_Valid_name);
    if tak_APE_Valid(FTag) <> tak_True then
      FTag := nil;
  end;
end;

destructor TTAKDecoder.Destroy;
begin
  if FInstance <> nil then
    try
      CheckFunc(@tak_SSD_Destroy, tak_SSD_Destroy_name);
      tak_SSD_Destroy(FInstance);
    finally
      FInstance := nil;
    end;

  inherited;
end;

function TTAKDecoder.GetTagPresents(const TagId: String): Boolean;
var
  n, size: Integer;
begin
  Result := (FTag <> nil);
  if Result then begin
    CheckFunc(@tak_APE_GetIndexOfKey, tak_APE_GetIndexOfKey_name);
    Result :=
      (tak_APE_GetIndexOfKey(FTag, PChar(TagId), n) = tak_res_Ok) and
      (n >= 0);
    if Result then begin
      CheckFunc(@tak_APE_GetItemValue, tak_APE_GetItemValue_name);
      Result := (tak_APE_GetItemValue(FTag, n, nil, 0, size) = tak_res_Ok);
    end;
  end;
end;

function TTAKDecoder.GetTagValue(const TagId: String): WideString;
var
  n, size: Integer;
  value: UTF8String;
begin
  Result := '';

  if FTag <> nil then begin
    CheckFunc(@tak_APE_GetIndexOfKey, tak_APE_GetIndexOfKey_name);
    if (tak_APE_GetIndexOfKey(FTag, PChar(TagId), n) = tak_res_Ok) and
       (n >= 0)
    then begin
      CheckFunc(@tak_APE_GetItemValue, tak_APE_GetItemValue_name);
      if tak_APE_GetItemValue(FTag, n, nil, 0, size) = tak_res_Ok then begin
        SetLength(value, size);
        if tak_APE_GetItemValue(FTag, n, @(value[1]), Length(value), size) = tak_res_Ok then
          {$IF CompilerVersion < 20}
          Result := UTF8Decode(value);
          {$IFEND}
          {$IF CompilerVersion >= 20}
          Result := UTF8ToString(value);
          {$IFEND}
      end;
    end;
  end;
end;

function TTAKDecoder.Seek(const Sample: Int64): Boolean;
begin
  CheckFunc(@tak_SSD_Seek, tak_SSD_Seek_name);
  Result := (tak_SSD_Seek(FInstance, Sample) = tak_res_Ok);
end;

function TTAKDecoder.Read(Buffer: Pointer; SampleCount: Integer): Integer;
begin
  CheckFunc(@tak_SSD_ReadAudio, tak_SSD_ReadAudio_name);
  if tak_SSD_ReadAudio(FInstance, Buffer, SampleCount, Result) <> tak_res_Ok then
    Result := 0;
end;


{ stream_io_interface functions }

const
  tak_bool_results: array [Boolean] of TtakBool = (tak_False, tak_True);

function stream_CanRead(Stream: TStream): TtakBool; cdecl;
begin
  Result := tak_bool_results[Stream <> nil];
end;

function stream_CanWrite(Stream: TStream): TtakBool; cdecl;
begin
  Result := tak_bool_results[False];
end;

function stream_CanSeek(Stream: TStream): TtakBool; cdecl;
begin
  Result := tak_bool_results[Stream <> nil];
end;

function stream_Read(
  Stream: TStream;
  ABuf: Pointer; ANum: TtakInt32; var AReadNum: TtakInt32): TtakBool; cdecl;
var
  res: Boolean;
begin
  res := (Stream <> nil);

  Result := tak_bool_results[res];

  if res then
    AReadNum := Stream.Read(ABuf^, ANum)
  else
    AReadNum := 0;
end;

function stream_Write(
  Stream: TStream;
  ABuf: Pointer; ANum: TtakInt32): TtakBool; cdecl;
begin
  Result := tak_bool_results[False];
end;

function stream_Flush(Stream: TStream): TtakBool; cdecl;
begin
  Result := tak_bool_results[False];
end;

function stream_Truncate(Stream: TStream): TtakBool; cdecl;
begin
  Result := tak_bool_results[False];
end;

function stream_Seek(Stream: TStream; APos: TtakInt64): TtakBool; cdecl;
var
  res: Boolean;
begin
  res := (Stream <> nil);

  Result := tak_bool_results[res];

  if res then
    Stream.Seek(APos, soFromBeginning);
end;

function stream_GetLength(Stream: TStream; var ALength: TtakInt64): TtakBool; cdecl;
var
  res: Boolean;
begin
  res := (Stream <> nil);

  Result := tak_bool_results[res];

  if res then
    ALength := Stream.Size
  else
    ALength := 0;
end;

var
  TAKDLL_Handle: HMODULE = 0;

initialization begin
  TAKDLL_Handle := LoadLibrary(TAKDLL_Name);
  TAKDLL_Loaded := (TAKDLL_Handle <> 0);
  if TAKDLL_Loaded then begin
    tak_GetLibraryVersion := GetProcAddress(TAKDLL_Handle, tak_GetLibraryVersion_name);
    tak_GetCodecName := GetProcAddress(TAKDLL_Handle, tak_GetCodecName_name);

    tak_APE_Valid := GetProcAddress(TAKDLL_Handle, tak_APE_Valid_name);
    tak_APE_State := GetProcAddress(TAKDLL_Handle, tak_APE_State_name);
    tak_APE_GetErrorString := GetProcAddress(TAKDLL_Handle, tak_APE_GetErrorString_name);
    tak_APE_ReadOnly := GetProcAddress(TAKDLL_Handle, tak_APE_ReadOnly_name);
    tak_APE_GetDesc := GetProcAddress(TAKDLL_Handle, tak_APE_GetDesc_name);

    tak_APE_GetItemNum := GetProcAddress(TAKDLL_Handle, tak_APE_GetItemNum_name);
    tak_APE_GetIndexOfKey := GetProcAddress(TAKDLL_Handle, tak_APE_GetIndexOfKey_name);
    tak_APE_GetItemDesc := GetProcAddress(TAKDLL_Handle, tak_APE_GetItemDesc_name);
    tak_APE_GetItemKey := GetProcAddress(TAKDLL_Handle, tak_APE_GetItemKey_name);
    tak_APE_GetItemValue := GetProcAddress(TAKDLL_Handle, tak_APE_GetItemValue_name);
    tak_APE_GetTextItemValueAsAnsi := GetProcAddress(TAKDLL_Handle, tak_APE_GetTextItemValueAsAnsi_name);

    tak_SSD_Create_FromFile := GetProcAddress(TAKDLL_Handle, tak_SSD_Create_FromFile_name);
    tak_SSD_Create_FromStream := GetProcAddress(TAKDLL_Handle, tak_SSD_Create_FromStream_name);
    tak_SSD_Destroy := GetProcAddress(TAKDLL_Handle, tak_SSD_Destroy_name);

    tak_SSD_Valid := GetProcAddress(TAKDLL_Handle, tak_SSD_Valid_name);
    tak_SSD_State := GetProcAddress(TAKDLL_Handle, tak_SSD_State_name);
    tak_SSD_GetStateInfo := GetProcAddress(TAKDLL_Handle, tak_SSD_GetStateInfo_name);
    tak_SSD_GetErrorString := GetProcAddress(TAKDLL_Handle, tak_SSD_GetErrorString_name);
    tak_SSD_GetStreamInfo := GetProcAddress(TAKDLL_Handle, tak_SSD_GetStreamInfo_name);
    tak_SSD_GetFrameSize := GetProcAddress(TAKDLL_Handle, tak_SSD_GetFrameSize_name);
    tak_SSD_Seek := GetProcAddress(TAKDLL_Handle, tak_SSD_Seek_name);
    tak_SSD_ReadAudio := GetProcAddress(TAKDLL_Handle, tak_SSD_ReadAudio_name);

    tak_SSD_GetReadPos := GetProcAddress(TAKDLL_Handle, tak_SSD_GetReadPos_name);
    tak_SSD_GetCurFrameBitRate := GetProcAddress(TAKDLL_Handle, tak_SSD_GetCurFrameBitRate_name);
    tak_SSD_GetSimpleWaveDataDesc := GetProcAddress(TAKDLL_Handle, tak_SSD_GetSimpleWaveDataDesc_name);
    tak_SSD_ReadSimpleWaveData := GetProcAddress(TAKDLL_Handle, tak_SSD_ReadSimpleWaveData_name);
    tak_SSD_GetEncoderInfo := GetProcAddress(TAKDLL_Handle, tak_SSD_GetEncoderInfo_name);
    tak_SSD_GetAPEv2Tag := GetProcAddress(TAKDLL_Handle, tak_SSD_GetAPEv2Tag_name);

    stream_io_interface.CanRead   := @stream_CanRead;
    stream_io_interface.CanWrite  := @stream_CanWrite;
    stream_io_interface.CanSeek   := @stream_CanSeek;
    stream_io_interface.Read      := @stream_Read;
    stream_io_interface.Write     := @stream_Write;
    stream_io_interface.Flush     := @stream_Flush;
    stream_io_interface.Truncate  := @stream_Truncate;
    stream_io_interface.Seek      := @stream_Seek;
    stream_io_interface.GetLength := @stream_GetLength;
  end;
end;

finalization begin
  if TAKDLL_Loaded then
    FreeLibrary(TAKDLL_Handle);
end;

end.

