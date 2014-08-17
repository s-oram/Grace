unit WavPackDLL;

(* Unit: WavPackDLL
    Delphi headers for wavpack.dll. *)

interface

uses
  SysUtils, Classes, ACS_Classes;

const
  WavpackDLL_Name = 'WavpackDLL.dll';

var
  WavpackDLL_Loaded: Boolean = False;

type
  PWavpackContext = type Pointer;

  (* class EWavpackException *)

  EWavpackException = class(Exception)
  end;

  TMD5Sum = packed array [0 .. 15] of Byte;

  (* class TWavpackDecoder *)

  TwvOpenFlag = (
    wvofWVC, wvofTags, wvofWrapper, wvof2chMax,
    wvofNormalize, wvofStreaming,
                                                wvofReserved00000040, wvofReserved00000080,
    wvofReserved00000100, wvofReserved00000200, wvofReserved00000400, wvofReserved00000800,
    wvofReserved00001000, wvofReserved00002000, wvofReserved00004000, wvofReserved00008000,
    wvofReserved00010000, wvofReserved00020000, wvofReserved00040000, wvofReserved00080000,
    wvofReserved00100000, wvofReserved00200000, wvofReserved00400000, wvofReserved00800000,
    wvofReserved01000000, wvofReserved02000000, wvofReserved04000000, wvofReserved08000000,
    wvofReserved10000000, wvofReserved20000000, wvofReserved40000000, wvofReserved80000000);
  TwvOpenFlags = packed set of TwvOpenFlag;

  TwvModeFlag = (
    wvmfWVC, wvmfLossless, wvmfHybrid, wvmfFloat,
    wvmfValidTag, wvmfHigh, wvmfFast, wvmfExtra,
    wvmfAPETag, wvmfSFX, wvmfVeryHigh, wvmfMD5,
    wvmfReserved00001000, wvmfReserved00002000, wvmfReserved00004000, wvmfReserved00008000,
    wvmfReserved00010000, wvmfReserved00020000, wvmfReserved00040000, wvmfReserved00080000,
    wvmfReserved00100000, wvmfReserved00200000, wvmfReserved00400000, wvmfReserved00800000,
    wvmfReserved01000000, wvmfReserved02000000, wvmfReserved04000000, wvmfReserved08000000,
    wvmfReserved10000000, wvmfReserved20000000, wvmfReserved40000000, wvmfReserved80000000);
  TwvModeFlags = packed set of TwvModeFlag;

  TWavpackDecoder = class
  private
    FFileName: AnsiString;

    FStream: TStream;
    FCorrectionsStream: TStream;

    FContext: PWavpackContext;

    function GetMode: TwvModeFlags;
    function GetNumChannels: Integer;
    function GetReducedChannels: Integer;
    function GetChannelMask: Integer;
    function GetSampleRate: Cardinal;
    function GetBitsPerSample: Integer;
    function GetBytesPerSample: Integer;
    function GetVersion: Integer;
    function GetNumSamples: Cardinal;
    function GetFileSize: Cardinal;
    function GetRatio: Double;
    function GetAverageBitrate(CountWVC: Integer): Double;
    function GetFloatNormExp: Integer;
    function GetMD5Sum: TMD5Sum;
    function GetSampleIndex: Cardinal;
    function GetInstantBitrate: Double;
    function GetNumErrors: Integer;
    function GetHasLossyBlocks: Boolean;
    function GetProgress: Integer;

    function GetTag(const Id: AnsiString): WideString;

    function GetLastError: AnsiString;
  public
    constructor Create(const AFileName: String;
      Flags: TwvOpenFlags; NormOffset: Integer = 0); overload;
    constructor Create(AStream, ACorrectionsStream: TStream;
      Flags: TwvOpenFlags; NormOffset: Integer = 0); overload;
    destructor Destroy; override;

    function UnpackSamples(Buffer: Pointer; SampleCount: Cardinal): Cardinal;
    function SeekSample(Sample: Cardinal): Boolean;

    property FileName: AnsiString read FFileName;
    property Stream: TStream read FStream;
    property CorrectionsStream: TStream read FCorrectionsStream;

    property Mode: TwvModeFlags read GetMode;
    property NumChannels: Integer read GetNumChannels;
    property ReducedChannels: Integer read GetReducedChannels;
    property ChannelMask: Integer read GetChannelMask;
    property SampleRate: Cardinal read GetSampleRate;
    property BitsPerSample: Integer read GetBitsPerSample;
    property BytesPerSample: Integer read GetBytesPerSample;
    property Version: Integer read GetVersion;
    property NumSamples: Cardinal read GetNumSamples;
    property FileSize: Cardinal read GetFileSize;
    property Ratio: Double read GetRatio;
    property AverageBitrate[CountWVC: Integer]: Double read GetAverageBitrate;
    property FloatNormExp: Integer read GetFloatNormExp;
    property MD5Sum: TMD5Sum read GetMD5Sum;
    property SampleIndex: Cardinal read GetSampleIndex;
    property InstantBitrate: Double read GetInstantBitrate;
    property NumErrors: Integer read GetNumErrors;
    property HasLossyBlocks: Boolean read GetHasLossyBlocks;
    property Progress: Integer read GetProgress;

    property Tags[const Id: AnsiString]: WideString read GetTag;

    property LastError: AnsiString read GetLastError;
  end;

  (* class TWavpackEncoder *)

  TwvConfigFlag = (
    wvcfReserved00000001, wvcfReserved00000002, wvcfReserved00000004,
                                                                      wvcfHybridFlag,
    wvcfJointStereo,
                          wvcfReserved00000020,
                                                wvcfHybridShape,
                                                                      wvcfReserved00000080,
    wvcfReserved00000100,
                          wvcfFastFlag,
                                                wvcfReserved00000400,
                                                                      wvcfHighFlag,
    wvcfVeryHighFlag,     wvcfBitrateKbps,
                                                wvcfReserved00004000,
                                                                      wvcfShapeOverride,
    wvcfJointOverride,
                          wvcfReserved00020000,
                                                wvcfCreateEXE,        wvcfCreateWVC,
    wvcfOptimizeWVC,
                          wvcfReserved00200000, wvcfReserved00400000,
                                                                      wvcfCalcNoise,
    wvcfReserved01000000,
                          wvcfExtraMode,        wvcfSkipWVX,          wvcfMD5CheckSum,
    wvcfReserved10000000, wvcfReserved20000000, wvcfReserved40000000,
                                                                      wvcfOptimizeMono);
  TwvConfigFlags = packed set of TwvConfigFlag;

  TWavpackConfig = packed record
    bitrate: Single;
    shaping_weight: Single;
    bits_per_sample: Integer;
    bytes_per_sample: Integer;
    qmode: Integer;
    flags: TwvConfigFlags;
    xmode: Integer;
    num_channels: Integer;
    float_norm_exp: Integer;
    block_samples: Integer;
    extra_flags: Integer;
    sample_rate: Integer;
    channel_mask: Integer;
    md5_checksum: TMD5Sum;
    md5_read: Byte;
    num_tag_strings: Integer;
    tag_strings: PPCharArray;
  end;
  PWavpackConfig = ^TWavpackConfig;

  TWavpackEncoder = class
  private
    FStream: TStream;
    FCorrectionsStream: TStream;

    FContext: PWavpackContext;

    FConfig: TWavpackConfig;

    procedure SetBitrate(Value: Single);
    procedure SetBitsPerSample(Value: Integer);
    procedure SetBlockSamples(Value: Integer);
    procedure SetBytesPerSample(Value: Integer);
    procedure SetChannelMask(Value: Integer);
    procedure SetFlags(Value: TwvConfigFlags);
    procedure SetFloatNormExp(Value: Integer);
    procedure SetNumChannels(Value: Integer);
    procedure SetSampleRate(Value: Integer);
    procedure SetShapingWeight(Value: Single);
    procedure SetXMode(Value: Integer);

    procedure SetTag(const Id: AnsiString; const Value: WideString);

    function GetLastError: AnsiString;
  public
    constructor Create(const AStream, ACorrectionsStream: TStream); reintroduce;
    destructor Destroy; override;

    function Init(TotalSamples: Cardinal): Boolean;
    function PackSamples(Buffer: Pointer; SampleCount: Cardinal): Boolean;
    function FlushSamples: Boolean;
    function WriteTags: Boolean;

    property Bitrate: Single write SetBitrate;
    property BitsPerSample: Integer write SetBitsPerSample;
    property BlockSamples: Integer write SetBlockSamples;
    property BytesPerSample: Integer write SetBytesPerSample;
    property ChannelMask: Integer write SetChannelMask;
    property Flags: TwvConfigFlags write SetFlags;
    property FloatNormExp: Integer write SetFloatNormExp;
    property NumChannels: Integer write SetNumChannels;
    property SampleRate: Integer write SetSampleRate;
    property ShapingWeight: Single write SetShapingWeight;
    property XMode: Integer write SetXMode;

    property Tags[const Id: AnsiString]: WideString write SetTag;

    property LastError: AnsiString read GetLastError;
  end;

procedure LoadWavpackDLL;
procedure UnloadWavpackDLL;


implementation

uses
  Windows;

type
  t_read_bytes_func = function(id, data: Pointer; bcount: Integer): Integer; cdecl;
  t_get_pos_func = function(id: Pointer): Cardinal; cdecl;
  t_set_pos_abs_func = function(id: Pointer; pos: Cardinal): Integer; cdecl;
  t_set_pos_rel_func = function(id: Pointer; delta, mode: Integer): Integer; cdecl;
  t_push_back_byte_func = function(id: Pointer; c: Integer): Integer; cdecl;
  t_get_length_func = function(id: Pointer): Cardinal; cdecl;
  t_can_seek_func = function(id: Pointer): LongBool; cdecl;
  t_write_bytes_func = function(id, data: Pointer; bcount: Integer): Integer; cdecl;

  TWavpackStreamReader = packed record
    read_bytes: t_read_bytes_func;
    get_pos: t_get_pos_func;
    set_pos_abs: t_set_pos_abs_func;
    set_pos_rel: t_set_pos_rel_func;
    push_back_byte: t_push_back_byte_func;
    get_length: t_get_length_func;
    can_seek: t_can_seek_func;

    // this callback is for writing edited tags only
    write_bytes: t_write_bytes_func;
  end;
  PWavpackStreamReader = ^TWavpackStreamReader;

  TWavpackBlockOutput = function(id, data: Pointer; bcount: Integer): LongBool; cdecl;

const
  WavpackOpenFileInputEx_name         = 'WavpackOpenFileInputEx';
  WavpackOpenFileInput_name           = 'WavpackOpenFileInput';

  WavpackGetMode_name                 = 'WavpackGetMode';

  WavpackGetErrorMessage_name         = 'WavpackGetErrorMessage';
  WavpackGetVersion_name              = 'WavpackGetVersion';
  WavpackUnpackSamples_name           = 'WavpackUnpackSamples';
  WavpackGetNumSamples_name           = 'WavpackGetNumSamples';
  WavpackGetSampleIndex_name          = 'WavpackGetSampleIndex';
  WavpackGetNumErrors_name            = 'WavpackGetNumErrors';
  WavpackLossyBlocks_name             = 'WavpackLossyBlocks';
  WavpackSeekSample_name              = 'WavpackSeekSample';
  WavpackCloseFile_name               = 'WavpackCloseFile';
  WavpackGetSampleRate_name           = 'WavpackGetSampleRate';
  WavpackGetBitsPerSample_name        = 'WavpackGetBitsPerSample';
  WavpackGetBytesPerSample_name       = 'WavpackGetBytesPerSample';
  WavpackGetNumChannels_name          = 'WavpackGetNumChannels';
  WavpackGetChannelMask_name          = 'WavpackGetChannelMask';
  WavpackGetReducedChannels_name      = 'WavpackGetReducedChannels';
  WavpackGetFloatNormExp_name         = 'WavpackGetFloatNormExp';
  WavpackGetMD5Sum_name               = 'WavpackGetMD5Sum';
  WavpackGetWrapperBytes_name         = 'WavpackGetWrapperBytes';
  WavpackGetWrapperData_name          = 'WavpackGetWrapperData';
  WavpackFreeWrapper_name             = 'WavpackFreeWrapper';
  WavpackSeekTrailingWrapper_name     = 'WavpackSeekTrailingWrapper';
  WavpackGetProgress_name             = 'WavpackGetProgress';
  WavpackGetFileSize_name             = 'WavpackGetFileSize';
  WavpackGetRatio_name                = 'WavpackGetRatio';
  WavpackGetAverageBitrate_name       = 'WavpackGetAverageBitrate';
  WavpackGetInstantBitrate_name       = 'WavpackGetInstantBitrate';
  WavpackGetNumTagItems_name          = 'WavpackGetNumTagItems';
  WavpackGetTagItem_name              = 'WavpackGetTagItem';
  WavpackGetTagItemIndexed_name       = 'WavpackGetTagItemIndexed';
  WavpackAppendTagItem_name           = 'WavpackAppendTagItem';
  WavpackDeleteTagItem_name           = 'WavpackDeleteTagItem';
  WavpackWriteTag_name                = 'WavpackWriteTag';

  WavpackOpenFileOutput_name          = 'WavpackOpenFileOutput';
  WavpackSetConfiguration_name        = 'WavpackSetConfiguration';
  WavpackAddWrapper_name              = 'WavpackAddWrapper';
  WavpackStoreMD5Sum_name             = 'WavpackStoreMD5Sum';
  WavpackPackInit_name                = 'WavpackPackInit';
  WavpackPackSamples_name             = 'WavpackPackSamples';
  WavpackFlushSamples_name            = 'WavpackFlushSamples';
  WavpackUpdateNumSamples_name        = 'WavpackUpdateNumSamples';
  WavpackGetWrapperLocation_name      = 'WavpackGetWrapperLocation';
  WavpackGetEncodedNoise_name         = 'WavpackGetEncodedNoise';

  WavpackFloatNormalize_name          = 'WavpackFloatNormalize';

  WavpackLittleEndianToNative_name    = 'WavpackLittleEndianToNative';
  WavpackNativeToLittleEndian_name    = 'WavpackNativeToLittleEndian';

  WavpackGetLibraryVersion_name       = 'WavpackGetLibraryVersion';
  WavpackGetLibraryVersionString_name = 'WavpackGetLibraryVersionString';

type
  t_WavpackOpenFileInputEx_func = function(reader: PWavpackStreamReader;
    wv_id, wvc_id: Pointer;
    error: PChar; flags, norm_offset: Integer): PWavpackContext; cdecl;
  t_WavpackOpenFileInput_func = function(infilename: PChar;
    error: PChar; flags, norm_offset: Integer): PWavpackContext; cdecl;

  t_WavpackGetMode_func = function(wpc: PWavpackContext): Integer; cdecl;

  t_WavpackGetErrorMessage_func = function(wpc: PWavpackContext): PChar; cdecl;
  t_WavpackGetVersion_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackUnpackSamples_func = function(wpc: PWavpackContext; sample_buffer: PInteger; sample_count: Cardinal): Cardinal; cdecl;
  // Modified by A.B.
  t_WavpackGetNumSamples_func = function(wpc: PWavpackContext): Cardinal; cdecl;
  // End modified
  t_WavpackGetSampleIndex_func = function(wpc: PWavpackContext): Cardinal; cdecl;
  t_WavpackGetNumErrors_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackLossyBlocks_func = function(wpc: PWavpackContext): LongBool; cdecl;
  t_WavpackSeekSample_func = function(wpc: PWavpackContext; sample: Cardinal): LongBool; cdecl;
  t_WavpackCloseFile_func = function(wpc: PWavpackContext): PWavpackContext; cdecl;
  t_WavpackGetSampleRate_func = function(wpc: PWavpackContext): Cardinal; cdecl;
  t_WavpackGetBitsPerSample_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetBytesPerSample_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetNumChannels_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetChannelMask_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetReducedChannels_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetFloatNormExp_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetMD5Sum_func = function(wpc: PWavpackContext; data: TMD5Sum): LongBool; cdecl;
  t_WavpackGetWrapperBytes_func = function(wpc: PWavpackContext): Cardinal; cdecl;
  t_WavpackGetWrapperData_func = function(wpc: PWavpackContext): PByte; cdecl;
  t_WavpackFreeWrapper_proc = procedure(wpc: PWavpackContext); cdecl;
  t_WavpackSeekTrailingWrapper_proc = procedure(wpc: PWavpackContext); cdecl;
  t_WavpackGetProgress_func = function(wpc: PWavpackContext): Double; cdecl;
  t_WavpackGetFileSize_func = function(wpc: PWavpackContext): Cardinal; cdecl;
  t_WavpackGetRatio_func = function(wpc: PWavpackContext): Double; cdecl;
  t_WavpackGetAverageBitrate_func = function(wpc: PWavpackContext; count_wvc: Integer): Double; cdecl;
  t_WavpackGetInstantBitrate_func = function(wpc: PWavpackContext): Double; cdecl;
  t_WavpackGetNumTagItems_func = function(wpc: PWavpackContext): Integer; cdecl;
  t_WavpackGetTagItem_func = function(wpc: PWavpackContext; item, value: PChar; size: Integer): Integer; cdecl;
  t_WavpackGetTagItemIndexed_func = function(wpc: PWavpackContext; index: Integer; item: PChar; size: Integer): Integer; cdecl;
  t_WavpackAppendTagItem_func = function(wpc: PWavpackContext; item, value: PChar; vsize: Integer): LongBool; cdecl;
  t_WavpackDeleteTagItem_func = function(wpc: PWavpackContext; item: PChar): LongBool; cdecl;
  t_WavpackWriteTag_func = function(wpc: PWavpackContext): LongBool; cdecl;

  t_WavpackOpenFileOutput_func = function(blockout: TWavpackBlockOutput; wv_id, wvc_id: Pointer): PWavpackContext; cdecl;
  t_WavpackSetConfiguration_func = function(wpc: PWavpackContext; config: PWavpackConfig; total_samples: Cardinal): LongBool; cdecl;
  t_WavpackAddWrapper_func = function(wpc: PWavpackContext; data: Pointer; bcount: Cardinal): LongBool; cdecl;
  t_WavpackStoreMD5Sum_func = function(wpc: PWavpackContext; data: TMD5Sum): LongBool; cdecl;
  t_WavpackPackInit_func = function(wpc: PWavpackContext): LongBool; cdecl;
  t_WavpackPackSamples_func = function(wpc: PWavpackContext; sample_buffer: PInteger; sample_count: Cardinal): LongBool; cdecl;
  t_WavpackFlushSamples_func = function(wpc: PWavpackContext): LongBool; cdecl;
  t_WavpackUpdateNumSamples_proc = procedure(wpc: PWavpackContext; first_block: Pointer); cdecl;
  t_WavpackGetWrapperLocation_func = function(first_block: Pointer; size: PCardinal): LongBool; cdecl;
  t_WavpackGetEncodedNoise_func = function(wpc: PWavpackContext; peak: PDouble): Double; cdecl;

  t_WavpackFloatNormalize_proc = procedure(values: PInteger; num_values: Integer; delta_exp: Integer); cdecl;

  t_WavpackLittleEndianToNative_proc = procedure(data: Pointer; format: PAnsiChar); cdecl;
  t_WavpackNativeToLittleEndian_proc = procedure(data: Pointer; format: PAnsiChar); cdecl;

  t_WavpackGetLibraryVersion_func = function: Cardinal; cdecl;
  t_WavpackGetLibraryVersionString_func = function: PAnsiChar; cdecl;

var
  WavpackDLL_Handle: HMODULE = 0;

  WavpackOpenFileInputEx: t_WavpackOpenFileInputEx_func = nil;
  WavpackOpenFileInput: t_WavpackOpenFileInput_func = nil;

  WavpackGetMode: t_WavpackGetMode_func = nil;

  WavpackGetErrorMessage: t_WavpackGetErrorMessage_func = nil;
  WavpackGetVersion: t_WavpackGetVersion_func = nil;
  WavpackUnpackSamples: t_WavpackUnpackSamples_func = nil;
  WavpackGetNumSamples: t_WavpackGetNumSamples_func = nil;
  WavpackGetSampleIndex: t_WavpackGetSampleIndex_func = nil;
  WavpackGetNumErrors: t_WavpackGetNumErrors_func = nil;
  WavpackLossyBlocks: t_WavpackLossyBlocks_func = nil;
  WavpackSeekSample: t_WavpackSeekSample_func = nil;
  WavpackCloseFile: t_WavpackCloseFile_func = nil;
  WavpackGetSampleRate: t_WavpackGetSampleRate_func = nil;
  WavpackGetBitsPerSample: t_WavpackGetBitsPerSample_func = nil;
  WavpackGetBytesPerSample: t_WavpackGetBytesPerSample_func = nil;
  WavpackGetNumChannels: t_WavpackGetNumChannels_func = nil;
  WavpackGetChannelMask: t_WavpackGetChannelMask_func = nil;
  WavpackGetReducedChannels: t_WavpackGetReducedChannels_func = nil;
  WavpackGetFloatNormExp: t_WavpackGetFloatNormExp_func = nil;
  WavpackGetMD5Sum: t_WavpackGetMD5Sum_func = nil;
  WavpackGetWrapperBytes: t_WavpackGetWrapperBytes_func = nil;
  WavpackGetWrapperData: t_WavpackGetWrapperData_func = nil;
  WavpackFreeWrapper: t_WavpackFreeWrapper_proc = nil;
  WavpackSeekTrailingWrapper: t_WavpackSeekTrailingWrapper_proc = nil;
  WavpackGetProgress: t_WavpackGetProgress_func = nil;
  WavpackGetFileSize: t_WavpackGetFileSize_func = nil;
  WavpackGetRatio: t_WavpackGetRatio_func = nil;
  WavpackGetAverageBitrate: t_WavpackGetAverageBitrate_func = nil;
  WavpackGetInstantBitrate: t_WavpackGetInstantBitrate_func = nil;
  WavpackGetNumTagItems: t_WavpackGetNumTagItems_func = nil;
  WavpackGetTagItem: t_WavpackGetTagItem_func = nil;
  WavpackGetTagItemIndexed: t_WavpackGetTagItemIndexed_func = nil;
  WavpackAppendTagItem: t_WavpackAppendTagItem_func = nil;
  WavpackDeleteTagItem: t_WavpackDeleteTagItem_func = nil;
  WavpackWriteTag: t_WavpackWriteTag_func = nil;

  WavpackOpenFileOutput: t_WavpackOpenFileOutput_func = nil;
  WavpackSetConfiguration: t_WavpackSetConfiguration_func = nil;
  WavpackAddWrapper: t_WavpackAddWrapper_func = nil;
  WavpackStoreMD5Sum: t_WavpackStoreMD5Sum_func = nil;
  WavpackPackInit: t_WavpackPackInit_func = nil;
  WavpackPackSamples: t_WavpackPackSamples_func = nil;
  WavpackFlushSamples: t_WavpackFlushSamples_func = nil;
  WavpackUpdateNumSamples: t_WavpackUpdateNumSamples_proc = nil;
  WavpackGetWrapperLocation: t_WavpackGetWrapperLocation_func = nil;
  WavpackGetEncodedNoise: t_WavpackGetEncodedNoise_func = nil;

  WavpackFloatNormalize: t_WavpackFloatNormalize_proc = nil;

  WavpackLittleEndianToNative: t_WavpackLittleEndianToNative_proc = nil;
  WavpackNativeToLittleEndian: t_WavpackNativeToLittleEndian_proc = nil;

  WavpackGetLibraryVersion: t_WavpackGetLibraryVersion_func = nil;
  WavpackGetLibraryVersionString: t_WavpackGetLibraryVersionString_func = nil;

  stream_reader: TWavpackStreamReader;


procedure CheckFunc(Func: Pointer; FuncName: String);
begin
  if not WavpackDLL_Loaded then
    raise EWavpackException.CreateFmt(
      'WavPack library "%s" not loaded!', [WavpackDLL_name]);
  if Func = nil then
    raise EWavpackException.CreateFmt(
      'Function "%s" not found in WavPack library!', [FuncName]);
end;


(* class TWavpackDecoder *)

constructor TWavpackDecoder.Create(const AFilename: String;
  Flags: TwvOpenFlags; NormOffset: Integer = 0);
var
  error: packed array [Byte] of Char;
begin
  FFileName := AnsiString(Trim(AFileName));
  if FFileName = '' then
    raise EWavpackException.Create('AFilename is empty!');

  CheckFunc(@WavpackOpenFileInput, WavpackOpenFileInput_name);

  FContext := WavpackOpenFileInput(
    {$WARNINGS OFF}
    PChar(FFileName),
    {$WARNINGS ON}
    @(error[0]), Integer(Flags), NormOffset);
  if FContext = nil then
    raise EWavpackException.CreateFmt(
      'Unable to open WavPack-file! Error: "%s"', [String(error)]);

  inherited Create();
end;

constructor TWavpackDecoder.Create(AStream, ACorrectionsStream: TStream;
  Flags: TwvOpenFlags; NormOffset: Integer = 0);
var
  error: packed array [Byte] of Char;
begin
  if AStream = nil then
    raise EWavpackException.Create('AStream is nil!');

  CheckFunc(@WavpackOpenFileInputEx, WavpackOpenFileInputEx_name);

  FStream := AStream;
  FCorrectionsStream := ACorrectionsStream;

  FContext := WavpackOpenFileInputEx(
    @stream_reader, FStream, FCorrectionsStream,
    @(error[0]), Integer(Flags), NormOffset);
  if FContext = nil then
    raise EWavpackException.CreateFmt(
      'Unable to open WavPack-stream! Error: "%s"', [String(error)]);

  inherited Create();
end;

destructor TWavpackDecoder.Destroy;
begin
  if (FContext <> nil) and (@WavpackCloseFile <> nil) then
    WavpackCloseFile(FContext);

  inherited;
end;

function TWavpackDecoder.GetMode: TwvModeFlags;
begin
  CheckFunc(@WavpackGetMode, WavpackGetMode_name);

  Integer(Result) := WavpackGetMode(FContext);
end;

function TWavpackDecoder.GetNumChannels: Integer;
begin
  CheckFunc(@WavpackGetNumChannels, WavpackGetNumChannels_name);

  Result := WavpackGetNumChannels(FContext);
end;

function TWavpackDecoder.GetReducedChannels: Integer;
begin
  CheckFunc(@WavpackGetReducedChannels, WavpackGetReducedChannels_name);

  Result := WavpackGetReducedChannels(FContext);
end;

function TWavpackDecoder.GetChannelMask: Integer;
begin
  CheckFunc(@WavpackGetChannelMask, WavpackGetChannelMask_name);

  Result := WavpackGetChannelMask(FContext);
end;

function TWavpackDecoder.GetSampleRate: Cardinal;
begin
  CheckFunc(@WavpackGetSampleRate, WavpackGetSampleRate_name);

  Result := WavpackGetSampleRate(FContext);
end;

function TWavpackDecoder.GetBitsPerSample: Integer;
begin
  CheckFunc(@WavpackGetBitsPerSample, WavpackGetBitsPerSample_name);

  Result := WavpackGetBitsPerSample(FContext);
end;

function TWavpackDecoder.GetBytesPerSample: Integer;
begin
  CheckFunc(@WavpackGetBytesPerSample, WavpackGetBytesPerSample_name);

  Result := WavpackGetBytesPerSample(FContext);
end;

function TWavpackDecoder.GetVersion: Integer;
begin
  CheckFunc(@WavpackGetVersion, WavpackGetVersion_name);

  Result := WavpackGetVersion(FContext);
end;

function TWavpackDecoder.GetNumSamples: Cardinal;
begin
  CheckFunc(@WavpackGetNumSamples, WavpackGetNumSamples_name);

  Result := WavpackGetNumSamples(FContext);
end;

function TWavpackDecoder.GetFileSize: Cardinal;
begin
  CheckFunc(@WavpackGetFileSize, WavpackGetFileSize_name);

  Result := WavpackGetFileSize(FContext);
end;

function TWavpackDecoder.GetRatio: Double;
begin
  CheckFunc(@WavpackGetRatio, WavpackGetRatio_name);

  Result := WavpackGetRatio(FContext);
end;

function TWavpackDecoder.GetAverageBitrate(CountWVC: Integer): Double;
begin
  CheckFunc(@WavpackGetAverageBitrate, WavpackGetAverageBitrate_name);

  Result := WavpackGetAverageBitrate(FContext, CountWVC);
end;

function TWavpackDecoder.GetFloatNormExp: Integer;
begin
  CheckFunc(@WavpackGetFloatNormExp, WavpackGetFloatNormExp_name);

  Result := WavpackGetFloatNormExp(FContext);
end;

function TWavpackDecoder.GetMD5Sum: TMD5Sum;
begin
  CheckFunc(@WavpackGetMD5Sum, WavpackGetMD5Sum_name);

  WavpackGetMD5Sum(FContext, Result);
end;

function TWavpackDecoder.GetSampleIndex: Cardinal;
begin
  CheckFunc(@WavpackGetSampleIndex, WavpackGetSampleIndex_name);

  Result := WavpackGetSampleIndex(FContext);
end;

function TWavpackDecoder.GetInstantBitrate: Double;
begin
  CheckFunc(@WavpackGetInstantBitrate, WavpackGetInstantBitrate_name);

  Result := WavpackGetInstantBitrate(FContext);
end;

function TWavpackDecoder.GetNumErrors: Integer;
begin
  CheckFunc(@WavpackGetNumErrors, WavpackGetNumErrors_name);

  Result := WavpackGetNumErrors(FContext);
end;

function TWavpackDecoder.GetHasLossyBlocks: Boolean;
begin
  CheckFunc(@WavpackLossyBlocks, WavpackLossyBlocks_name);

  Result := WavpackLossyBlocks(FContext);
end;

function TWavpackDecoder.GetProgress: Integer;
begin
  CheckFunc(@WavpackGetProgress, WavpackGetProgress_name);

  Result := Trunc(WavpackGetProgress(FContext) * 100);
end;

function TWavpackDecoder.GetTag(const Id: AnsiString): WideString;
var
  len: Integer;
  buf: UTF8String;
begin
  CheckFunc(@WavpackGetTagItem, WavpackGetTagItem_name);

  len := WavpackGetTagItem(FContext, @(Id[1]), nil, 0);
  if len > 0 then begin
    SetLength(buf, len);
    WavpackGetTagItem(FContext, @(Id[1]), @(buf[1]), len + 1{for terminal #0});
    {$IF CompilerVersion < 20}
    Result := UTF8Decode(buf);
    {$IFEND}
    {$IF CompilerVersion >= 20}
    Result := UTF8ToString(buf);
    {$IFEND}
  end
  else
    Result := '';
end;

function TWavpackDecoder.GetLastError: AnsiString;
begin
  CheckFunc(@WavpackGetErrorMessage, WavpackGetErrorMessage_name);
  {$WARNINGS OFF}
  Result := WavpackGetErrorMessage(FContext);
  {$WARNINGS ON}
end;

function TWavpackDecoder.UnpackSamples(Buffer: Pointer; SampleCount: Cardinal): Cardinal;
begin
  CheckFunc(@WavpackUnpackSamples, WavpackUnpackSamples_name);

  Result := WavpackUnpackSamples(FContext, Buffer, SampleCount);
end;

function TWavpackDecoder.SeekSample(Sample: Cardinal): Boolean;
begin
  CheckFunc(@WavpackSeekSample, WavpackSeekSample_name);

  Result := WavpackSeekSample(FContext, Sample);
end;


(* class TWavpackEncoder *)

function blockout(Stream: TStream; Buffer: Pointer; Count: Integer): LongBool; cdecl;
begin
  Result := (Stream <> nil) and (Stream.Write(Buffer^, Count) = Count);
end;

constructor TWavpackEncoder.Create(const AStream, ACorrectionsStream: TStream);
begin
  if AStream = nil then
    raise EWavpackException.Create('AStream is nil!');

  CheckFunc(@WavpackOpenFileInputEx, WavpackOpenFileInputEx_name);

  FStream := AStream;
  FCorrectionsStream := ACorrectionsStream;

  FContext := WavpackOpenFileOutput(@blockout, FStream, FCorrectionsStream);
  if FContext = nil then
    raise EWavpackException.Create(
      'Unable to allocate enought memory for WavPack context!');

  inherited Create();
end;

destructor TWavpackEncoder.Destroy;
begin
  if (FContext <> nil) and (@WavpackCloseFile <> nil) then
    WavpackCloseFile(FContext);

  inherited;
end;

procedure TWavpackEncoder.SetBitrate(Value: Single);
begin
  FConfig.bitrate := Value;
end;

procedure TWavpackEncoder.SetBitsPerSample(Value: Integer);
begin
  FConfig.bits_per_sample := Value;
end;

procedure TWavpackEncoder.SetBlockSamples(Value: Integer);
begin
  FConfig.block_samples := Value;
end;

procedure TWavpackEncoder.SetBytesPerSample(Value: Integer);
begin
  FConfig.bytes_per_sample := Value;
end;

procedure TWavpackEncoder.SetChannelMask(Value: Integer);
begin
  FConfig.channel_mask := Value;
end;

procedure TWavpackEncoder.SetFlags(Value: TwvConfigFlags);
begin
  FConfig.flags := Value;
end;

procedure TWavpackEncoder.SetFloatNormExp(Value: Integer);
begin
  FConfig.float_norm_exp := Value;
end;

procedure TWavpackEncoder.SetNumChannels(Value: Integer);
begin
  FConfig.num_channels := Value;
end;

procedure TWavpackEncoder.SetSampleRate(Value: Integer);
begin
  FConfig.sample_rate := Value;
end;

procedure TWavpackEncoder.SetShapingWeight(Value: Single);
begin
  FConfig.shaping_weight := Value;
end;

procedure TWavpackEncoder.SetXMode(Value: Integer);
begin
  FConfig.xmode := Value;
end;

procedure TWavpackEncoder.SetTag(const Id: AnsiString; const Value: WideString);
var
  buf: UTF8String;
begin
  CheckFunc(@WavpackAppendTagItem, WavpackAppendTagItem_name);

  buf := UTF8Encode(Value);
  WavpackAppendTagItem(FContext, @(Id[1]), @(buf[1]), Length(buf));
end;

function TWavpackEncoder.GetLastError: AnsiString;
begin
  CheckFunc(@WavpackGetErrorMessage, WavpackGetErrorMessage_name);
  {$WARNINGS OFF}
  Result := WavpackGetErrorMessage(FContext);
  {$WARNINGS ON}
end;

function TWavpackEncoder.Init(TotalSamples: Cardinal): Boolean;
begin
  CheckFunc(@WavpackSetConfiguration, WavpackSetConfiguration_name);

  Result := WavpackSetConfiguration(FContext, @FConfig, TotalSamples);

  if Result then begin
    CheckFunc(@WavpackPackInit, WavpackPackInit_name);

    Result := WavpackPackInit(FContext);
  end;
end;

function TWavpackEncoder.PackSamples(Buffer: Pointer; SampleCount: Cardinal): Boolean;
begin
  CheckFunc(@WavpackPackSamples, WavpackPackSamples_name);

  Result := WavpackPackSamples(FContext, Buffer, SampleCount);
end;

function TWavpackEncoder.FlushSamples: Boolean;
begin
  CheckFunc(@WavpackFlushSamples, WavpackFlushSamples_name);

  Result := WavpackFlushSamples(FContext);
end;

function TWavpackEncoder.WriteTags: Boolean;
begin
  CheckFunc(@WavpackWriteTag, WavpackWriteTag_name);

  Result := WavpackWriteTag(FContext) and FlushSamples();
end;


(* stream reader functions *)

function stream_read_bytes(Stream: TStream; Data: Pointer; ByteCount: Integer): Integer; cdecl;
begin
  Result := - 1;
  if Stream <> nil then
    try
      Result := Stream.Read(Data^, ByteCount);
    except
    end;
end;

function stream_get_pos(Stream: TStream): Cardinal; cdecl;
begin
  Result := 0;
  if Stream <> nil then
    try
      Result := Stream.Position;
    except
    end;
end;

function stream_set_pos_abs(Stream: TStream; Pos: Cardinal): Integer; cdecl;
begin
  Result := - 1;
  if Stream <> nil then
    try
      Stream.Position := Pos;
      Result := 0;
    except
    end;
end;

function stream_set_pos_rel(Stream: TStream; Delta, Mode: Integer): Integer; cdecl;
begin
  Result := - 1;
  if Stream <> nil then
    try
      Stream.Seek(Delta, Mode);
      Result := 0;
    except
    end;
end;

function stream_push_back_byte(Stream: TStream; c: Integer): Integer; cdecl;
begin
  Result := $1A; // EOF
  if Stream <> nil then
    try
      Stream.Seek(- 1, soFromCurrent);
    except
    end;
end;

function stream_get_length(Stream: TStream): Cardinal; cdecl;
begin
  Result := 0;
  if Stream <> nil then
    try
      Result := Stream.Size;
    except
    end;
end;

function stream_can_seek(Stream: TStream): LongBool; cdecl;
var
  pos: Int64;
begin
  Result := False;
  if Stream <> nil then
    try
      Result := (Stream.Size = 0);
      if not Result then begin
        pos := Stream.Position;
        try
          Stream.Seek(0, soFromBeginning);
          Stream.Seek(Stream.Size, soFromBeginning);
        finally
          Stream.Position := pos;
        end;

        Result := True;
      end;
    except
    end;
end;

function stream_write_bytes(Stream: TStream; Data: Pointer; ByteCount: Integer): Integer; cdecl;
begin
  Result := - 1;
  if Stream <> nil then
    try
      Result := Stream.Write(Data^, ByteCount);
    except
    end;
end;

procedure LoadWavpackDLL;
begin
  LoadLibCS.Enter;
  if WavpackDLL_Loaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  WavpackDLL_Handle := LoadLibrary(WavpackDLL_Name);
  WavpackDLL_Loaded := (WavpackDLL_Handle <> 0);
  if WavpackDLL_Loaded then begin
    WavpackOpenFileInputEx := GetProcAddress(WavpackDLL_Handle, WavpackOpenFileInputEx_name);
    WavpackOpenFileInput := GetProcAddress(WavpackDLL_Handle, WavpackOpenFileInput_name);

    WavpackGetMode := GetProcAddress(WavpackDLL_Handle, WavpackGetMode_name);

    WavpackGetErrorMessage := GetProcAddress(WavpackDLL_Handle, WavpackGetErrorMessage_name);
    WavpackGetVersion := GetProcAddress(WavpackDLL_Handle, WavpackGetVersion_name);
    WavpackUnpackSamples := GetProcAddress(WavpackDLL_Handle, WavpackUnpackSamples_name);
    WavpackGetNumSamples := GetProcAddress(WavpackDLL_Handle, WavpackGetNumSamples_name);
    WavpackGetSampleIndex := GetProcAddress(WavpackDLL_Handle, WavpackGetSampleIndex_name);
    WavpackGetNumErrors := GetProcAddress(WavpackDLL_Handle, WavpackGetNumErrors_name);
    WavpackLossyBlocks := GetProcAddress(WavpackDLL_Handle, WavpackLossyBlocks_name);
    WavpackSeekSample := GetProcAddress(WavpackDLL_Handle, WavpackSeekSample_name);
    WavpackCloseFile := GetProcAddress(WavpackDLL_Handle, WavpackCloseFile_name);
    WavpackGetSampleRate := GetProcAddress(WavpackDLL_Handle, WavpackGetSampleRate_name);
    WavpackGetBitsPerSample := GetProcAddress(WavpackDLL_Handle, WavpackGetBitsPerSample_name);
    WavpackGetBytesPerSample := GetProcAddress(WavpackDLL_Handle, WavpackGetBytesPerSample_name);
    WavpackGetNumChannels := GetProcAddress(WavpackDLL_Handle, WavpackGetNumChannels_name);
    WavpackGetChannelMask := GetProcAddress(WavpackDLL_Handle, WavpackGetChannelMask_name);
    WavpackGetReducedChannels := GetProcAddress(WavpackDLL_Handle, WavpackGetReducedChannels_name);
    WavpackGetFloatNormExp := GetProcAddress(WavpackDLL_Handle, WavpackGetFloatNormExp_name);
    WavpackGetMD5Sum := GetProcAddress(WavpackDLL_Handle, WavpackGetMD5Sum_name);
    WavpackGetWrapperBytes := GetProcAddress(WavpackDLL_Handle, WavpackGetWrapperBytes_name);
    WavpackGetWrapperData := GetProcAddress(WavpackDLL_Handle, WavpackGetWrapperData_name);
    WavpackFreeWrapper := GetProcAddress(WavpackDLL_Handle, WavpackFreeWrapper_name);
    WavpackSeekTrailingWrapper := GetProcAddress(WavpackDLL_Handle, WavpackSeekTrailingWrapper_name);
    WavpackGetProgress := GetProcAddress(WavpackDLL_Handle, WavpackGetProgress_name);
    WavpackGetFileSize := GetProcAddress(WavpackDLL_Handle, WavpackGetFileSize_name);
    WavpackGetRatio := GetProcAddress(WavpackDLL_Handle, WavpackGetRatio_name);
    WavpackGetAverageBitrate := GetProcAddress(WavpackDLL_Handle, WavpackGetAverageBitrate_name);
    WavpackGetInstantBitrate := GetProcAddress(WavpackDLL_Handle, WavpackGetInstantBitrate_name);
    WavpackGetNumTagItems := GetProcAddress(WavpackDLL_Handle, WavpackGetNumTagItems_name);
    WavpackGetTagItem := GetProcAddress(WavpackDLL_Handle, WavpackGetTagItem_name);
    WavpackGetTagItemIndexed := GetProcAddress(WavpackDLL_Handle, WavpackGetTagItemIndexed_name);
    WavpackAppendTagItem := GetProcAddress(WavpackDLL_Handle, WavpackAppendTagItem_name);
    WavpackDeleteTagItem := GetProcAddress(WavpackDLL_Handle, WavpackDeleteTagItem_name);
    WavpackWriteTag := GetProcAddress(WavpackDLL_Handle, WavpackWriteTag_name);

    WavpackOpenFileOutput := GetProcAddress(WavpackDLL_Handle, WavpackOpenFileOutput_name);
    WavpackSetConfiguration := GetProcAddress(WavpackDLL_Handle, WavpackSetConfiguration_name);
    WavpackAddWrapper := GetProcAddress(WavpackDLL_Handle, WavpackAddWrapper_name);
    WavpackStoreMD5Sum := GetProcAddress(WavpackDLL_Handle, WavpackStoreMD5Sum_name);
    WavpackPackInit := GetProcAddress(WavpackDLL_Handle, WavpackPackInit_name);
    WavpackPackSamples := GetProcAddress(WavpackDLL_Handle, WavpackPackSamples_name);
    WavpackFlushSamples := GetProcAddress(WavpackDLL_Handle, WavpackFlushSamples_name);
    WavpackUpdateNumSamples := GetProcAddress(WavpackDLL_Handle, WavpackUpdateNumSamples_name);
    WavpackGetWrapperLocation := GetProcAddress(WavpackDLL_Handle, WavpackGetWrapperLocation_name);
    WavpackGetEncodedNoise := GetProcAddress(WavpackDLL_Handle, WavpackGetEncodedNoise_name);

    WavpackFloatNormalize := GetProcAddress(WavpackDLL_Handle, WavpackFloatNormalize_name);

    WavpackLittleEndianToNative := GetProcAddress(WavpackDLL_Handle, WavpackLittleEndianToNative_name);
    WavpackNativeToLittleEndian := GetProcAddress(WavpackDLL_Handle, WavpackNativeToLittleEndian_name);

    WavpackGetLibraryVersion := GetProcAddress(WavpackDLL_Handle, WavpackGetLibraryVersion_name);
    WavpackGetLibraryVersionString := GetProcAddress(WavpackDLL_Handle, WavpackGetLibraryVersionString_name);

    stream_reader.read_bytes     := @stream_read_bytes;
    stream_reader.get_pos        := @stream_get_pos;
    stream_reader.set_pos_abs    := @stream_set_pos_abs;
    stream_reader.set_pos_rel    := @stream_set_pos_rel;
    stream_reader.push_back_byte := @stream_push_back_byte;
    stream_reader.get_length     := @stream_get_length;
    stream_reader.can_seek       := @stream_can_seek;
    stream_reader.write_bytes    := @stream_write_bytes;
  end;
  LoadLibCS.Leave;
end;

procedure UnloadWavpackDLL;
begin
  if WavpackDLL_Loaded then
    FreeLibrary(WavpackDLL_Handle);
  WavpackDLL_Loaded := False;
end;

end.

