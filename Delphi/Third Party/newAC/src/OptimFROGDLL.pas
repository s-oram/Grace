unit OptimFROGDLL;

(* $Id: OptimFROGDLL.pas 1117 2010-01-22 09:28:59Z andrei.borovsky $ *)

interface

uses
  Classes, SysUtils;

const
  OptimFROGDLL_Name = 'OptimFROG.dll';

var
  OptimFROGDLL_Loaded: Boolean;

type
  POptimFROGDecoderInstance = type Pointer;

  TOptimFROGTags = packed record
    key_count: Cardinal;
    keys: array [0 .. 63] of PAnsiChar;
    values: array [0 .. 63] of PAnsiChar;
  end;
  POptimFROGTags = ^TOptimFROGTags;

  TOptimFROGInfo = packed record
    channels: Cardinal;
    sample_rate: Cardinal;
    bits_per_sample: Cardinal;
    bitrate: Cardinal;

    version: Cardinal;
    method: PChar;
    speedup: PChar;

    no_points: Int64;
    original_size: Int64;
    compressed_size: Int64;
    length_ms: Int64;

    sample_type: PChar;
    channel_config: PChar;
  end;
  POptimFROGInfo = ^TOptimFROGInfo;

  { class EOptimFROGException }

  EOptimFROGException = class(Exception)
  end;

  { class TOptimFROGDecoder }

  TOptimFROGDecoder = class
  private
    FFileName: String;

    FStream: TStream;

    FInstance: POptimFROGDecoderInstance;

    FInfo: POptimFROGInfo;
    FTags: POptimFROGTags;

    function ReadInfo: Boolean;

    function GetNumChannels: Cardinal;
    function GetSampleRate: Cardinal;
    function GetBitsPerSample: Cardinal;
    function GetBitrate: Cardinal;

    function GetVersion: Cardinal;

    function GetNumSamples: Int64;
    function GetOriginalSize: Int64;
    function GetCompressedSize: Int64;
    function GetLengthMS: Int64;

    function GetSeekable: Boolean;
    function GetRecoverableErrors: Boolean;

    function GetTag(const Key: String): WideString;
  public
    constructor Create(const AFileName: String); overload;
    constructor Create(AStream: TStream); overload;
    destructor Destroy; override;

    function Seek(const Sample: Int64): Boolean;
    function Read(Buffer: Pointer; SampleCount: Cardinal): Integer;

    property FileName: String read FFileName;
    property Stream: TStream read FStream;

    property NumChannels: Cardinal read GetNumChannels;
    property SampleRate: Cardinal read GetSampleRate;
    property BitsPerSample: Cardinal read GetBitsPerSample;
    property Bitrate: Cardinal read GetBitrate;

    property Version: Cardinal read GetVersion;

    property NumSamples: Int64 read GetNumSamples;
    property OriginalSize: Int64 read GetOriginalSize;
    property CompressedSize: Int64 read GetCompressedSize;
    property LengthMS: Int64 read GetLengthMS;

    property Seekable: Boolean read GetSeekable;
    property RecoverableErrors: Boolean read GetRecoverableErrors;

    property Tags[const Key: String]: WideString read GetTag;
  end;

implementation

uses
  Windows;

const
  OptimFROG_getVersion_name        = 'OptimFROG_getVersion';

  OptimFROG_createInstance_name    = 'OptimFROG_createInstance';
  OptimFROG_destroyInstance_name   = 'OptimFROG_destroyInstance';

  OptimFROG_openExt_name           = 'OptimFROG_openExt';

  OptimFROG_open_name              = 'OptimFROG_open';
  OptimFROG_close_name             = 'OptimFROG_close';

  OptimFROG_readHead_name          = 'OptimFROG_readHead';
  OptimFROG_readTail_name          = 'OptimFROG_readTail';

  OptimFROG_seekable_name          = 'OptimFROG_seekable';
  OptimFROG_seekPoint_name         = 'OptimFROG_seekPoint';
  OptimFROG_seekTime_name          = 'OptimFROG_seekTime';
  OptimFROG_getPos_name            = 'OptimFROG_getPos';
  OptimFROG_recoverableErrors_name = 'OptimFROG_recoverableErrors';

  OptimFROG_read_name              = 'OptimFROG_read';

  OptimFROG_getInfo_name           = 'OptimFROG_getInfo';
  OptimFROG_getTags_name           = 'OptimFROG_getTags';
  OptimFROG_freeTags_name          = 'OptimFROG_freeTags';

  OptimFROG_decodeFile_name        = 'OptimFROG_decodeFile';
  OptimFROG_infoFile_name          = 'OptimFROG_infoFile';

  // OptimFROG_decodeFile & OptimFROG_infoFile functions result codes
  OptimFROG_NoError          = 0;
  OptimFROG_MemoryError      = 1;
  OptimFROG_OpenError        = 2;
  OptimFROG_WriteError       = 3;
  OptimFROG_FatalError       = 4;
  OptimFROG_RecoverableError = 5;

type
  t_condition = type ByteBool;

  t_close_func    = function (
    instance: Pointer): t_condition; cdecl;
  t_read_func     = function (
    instance: Pointer; buffer: Pointer; count: Cardinal): Integer; cdecl;
  t_eof_func      = function (
    instance: Pointer): t_condition; cdecl;
  t_seekable_func = function (
    instance: Pointer): t_condition; cdecl;
  t_length_func   = function (
    instance: Pointer): Int64; cdecl;
  t_get_pos_func  = function (
    instance: Pointer): Int64; cdecl;
  t_seek_func     = function (
    instance: Pointer; pos: Int64): t_condition; cdecl;

  TReadInterface = packed record
    close: t_close_func;
    read: t_read_func;
    eof: t_eof_func;
    seekable: t_seekable_func;
    length: t_length_func;
    get_pos: t_get_pos_func;
    seek: t_seek_func;
  end;
  PReadInterface = ^TReadInterface;

  t_OptimFROG_callBack_proc = procedure (
    callBackParam: Cardinal; percentage: Double); cdecl;

  t_OptimFROG_getVersion_func        = function : Cardinal; cdecl;

  t_OptimFROG_createInstance_func    = function : POptimFROGDecoderInstance; cdecl;
  t_OptimFROG_destroyInstance_proc   = procedure (
    decoderInstance: POptimFROGDecoderInstance); cdecl;

  t_OptimFROG_openExt_func           = function (
    decoderInstance: POptimFROGDecoderInstance;
    rInt: PReadInterface;
    readerInstance: Pointer;
    readTags: t_condition = False): t_condition; cdecl;

  t_OptimFROG_open_func              = function (
    decoderInstance: POptimFROGDecoderInstance;
    fileName: PChar;
    readTags: t_condition = False): t_condition; cdecl;
  t_OptimFROG_close_func             = function (
    decoderInstance: POptimFROGDecoderInstance): t_condition; cdecl;

  t_OptimFROG_readHead_func          = function (
    decoderInstance: POptimFROGDecoderInstance;
    headData: Pointer;
    maxSize: Cardinal): Integer; cdecl;
  t_OptimFROG_readTail_func          = function (
    decoderInstance: POptimFROGDecoderInstance;
    tailData: Pointer;
    maxSize: Cardinal): Integer; cdecl;

  t_OptimFROG_seekable_func          = function (
    decoderInstance: POptimFROGDecoderInstance): t_condition; cdecl;
  t_OptimFROG_seekPoint_func         = function (
    decoderInstance: POptimFROGDecoderInstance;
    point: Int64): t_condition; cdecl;
  t_OptimFROG_seekTime_func          = function (
    decoderInstance: POptimFROGDecoderInstance;
    milliseconds: Int64): t_condition; cdecl;
  t_OptimFROG_getPos_func            = function (
    decoderInstance: POptimFROGDecoderInstance): Int64; cdecl;
  t_OptimFROG_recoverableErrors_func = function (
    decoderInstance: POptimFROGDecoderInstance): t_condition; cdecl;

  t_OptimFROG_read_func              = function (
    decoderInstance: POptimFROGDecoderInstance;
    data: Pointer;
    noPoints: Cardinal;
    max16bit: t_condition = False): Integer; cdecl;

  t_OptimFROG_getInfo_func           = function (
    decoderInstance: POptimFROGDecoderInstance;
    info: POptimFROGInfo): t_condition; cdecl;
  t_OptimFROG_getTags_func           = function (
    decoderInstance: POptimFROGDecoderInstance;
    tags: POptimFROGTags): t_condition; cdecl;
  t_OptimFROG_freeTags_proc          = procedure (
    const tags: POptimFROGTags); cdecl;

  t_OptimFROG_decodeFile_func        = function (
    sourceFile, destinationFile: PChar;
    callBack: t_OptimFROG_callBack_proc = nil;
    callBackParam: Pointer = nil): Integer; cdecl;
  t_OptimFROG_infoFile_func          = function (
    sourceFile: PChar;
    info: POptimFROGInfo;
    tags: POptimFROGTags): Integer; cdecl;

var
  OptimFROGDLL_Handle: HMODULE = 0;

  OptimFROG_getVersion: t_OptimFROG_getVersion_func = nil;

  OptimFROG_createInstance: t_OptimFROG_createInstance_func = nil;
  OptimFROG_destroyInstance: t_OptimFROG_destroyInstance_proc = nil;

  OptimFROG_openExt: t_OptimFROG_openExt_func = nil;

  OptimFROG_open: t_OptimFROG_open_func = nil;
  OptimFROG_close: t_OptimFROG_close_func = nil;

  OptimFROG_readHead: t_OptimFROG_readHead_func = nil;
  OptimFROG_readTail: t_OptimFROG_readTail_func = nil;

  OptimFROG_seekable: t_OptimFROG_seekable_func = nil;
  OptimFROG_seekPoint: t_OptimFROG_seekPoint_func = nil;
  OptimFROG_seekTime: t_OptimFROG_seekTime_func = nil;
  OptimFROG_getPos: t_OptimFROG_getPos_func = nil;
  OptimFROG_recoverableErrors: t_OptimFROG_recoverableErrors_func = nil;

  OptimFROG_read: t_OptimFROG_read_func = nil;

  OptimFROG_getInfo: t_OptimFROG_getInfo_func = nil;
  OptimFROG_getTags: t_OptimFROG_getTags_func = nil;
  OptimFROG_freeTags: t_OptimFROG_freeTags_proc = nil;

  OptimFROG_decodeFile: t_OptimFROG_decodeFile_func = nil;
  OptimFROG_infoFile: t_OptimFROG_infoFile_func = nil;


procedure CheckFunc(Func: Pointer; const FuncName: String);
begin
  if not OptimFROGDLL_Loaded then
    raise EOptimFROGException.CreateFmt(
      'OptimFROG library "%s" not loaded!', [OptimFROGDLL_Name]);
  if Func = nil then
    raise EOptimFROGException.CreateFmt(
      'Function "%s" not found in OptimFROG library!', [FuncName]);
end;


{ class TOptimFROGDecoder }

var
  read_interface: TReadInterface;

constructor TOptimFROGDecoder.Create(const AFileName: String);
begin
  FFileName := Trim(AFileName);
  if FFileName = '' then
    raise EOptimFROGException.Create('AFilename is empty!');

  CheckFunc(@OptimFROG_createInstance, OptimFROG_createInstance_name);

  FInstance := OptimFROG_createInstance;
  if FInstance = nil then
    EOptimFROGException.Create('OptimFROG decoder instance was not created!');

  CheckFunc(@OptimFROG_open, OptimFROG_open_name);

  if not OptimFROG_open(FInstance, PChar(FFileName), True) then
    raise EOptimFROGException.Create('Unable to open OptimFROG-file!');

  inherited Create();
end;

constructor TOptimFROGDecoder.Create(AStream: TStream);
begin
  if AStream = nil then
    raise EOptimFROGException.Create('AStream is nil!');

  CheckFunc(@OptimFROG_createInstance, OptimFROG_createInstance_name);

  FInstance := OptimFROG_createInstance;
  if FInstance = nil then
    EOptimFROGException.Create('OptimFROG decoder instance was not created!');

  CheckFunc(@OptimFROG_openExt, OptimFROG_openExt_name);

  FStream := AStream;

  if not OptimFROG_openExt(FInstance, @read_interface, FStream, True) then
    raise EOptimFROGException.Create('Unable to open OptimFROG-file!');

  inherited Create();
end;

destructor TOptimFROGDecoder.Destroy;
begin
  if FInstance <> nil then begin
    if FInfo <> nil then begin
      Dispose(FInfo);
      FInfo := nil;
    end;

    try
      if FTags <> nil then
        try
          CheckFunc(@OptimFROG_freeTags, OptimFROG_freeTags_name);

          OptimFROG_freeTags(FTags);
        finally
          Dispose(FTags);
          FTags := nil;
        end;
    finally
      CheckFunc(@OptimFROG_destroyInstance, OptimFROG_destroyInstance_name);

      OptimFROG_destroyInstance(FInstance);
      FInstance := nil;
    end;
  end;

  inherited;
end;

function TOptimFROGDecoder.ReadInfo: Boolean;
begin
  Result := (FInfo <> nil);
  if not Result then begin
    New(FInfo);
    try
      CheckFunc(@OptimFROG_getInfo, OptimFROG_getInfo_name);

      Result := OptimFROG_getInfo(FInstance, FInfo);
    except
      Dispose(FInfo);
      FInfo := nil;

      raise;
    end;

    if not Result then begin
      Dispose(FInfo);
      FInfo := nil;
    end;
  end;
end;

function TOptimFROGDecoder.GetNumChannels: Cardinal;
begin
  if ReadInfo() then
    Result := FInfo.channels
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetSampleRate: Cardinal;
begin
  if ReadInfo() then
    Result := FInfo.sample_rate
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetBitsPerSample: Cardinal;
begin
  if ReadInfo() then
    Result := FInfo.bits_per_sample
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetBitrate: Cardinal;
begin
  if ReadInfo() then
    Result := FInfo.bitrate
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetVersion: Cardinal;
begin
  if ReadInfo() then
    Result := FInfo.version
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetNumSamples: Int64;
begin
  if ReadInfo() then
    Result := FInfo.no_points
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetOriginalSize: Int64;
begin
  if ReadInfo() then
    Result := FInfo.original_size
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetCompressedSize: Int64;
begin
  if ReadInfo() then
    Result := FInfo.compressed_size
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetLengthMS: Int64;
begin
  if ReadInfo() then
    Result := FInfo.length_ms
  else
    Result := 0;
end;

function TOptimFROGDecoder.GetSeekable: Boolean;
begin
  CheckFunc(@OptimFROG_seekable, OptimFROG_seekable_name);

  Result := OptimFROG_seekable(FInstance);
end;

function TOptimFROGDecoder.GetRecoverableErrors: Boolean;
begin
  CheckFunc(@OptimFROG_recoverableErrors, OptimFROG_recoverableErrors_name);

  Result := OptimFROG_recoverableErrors(FInstance);
end;

function TOptimFROGDecoder.GetTag(const Key: String): WideString;
var
  _key: String;
  i: Integer;
begin
  Result := '';

  _key := Trim(key);
  if _key <> '' then begin
    if FTags = nil then begin
      CheckFunc(@OptimFROG_getTags, OptimFROG_getTags_name);

      New(FTags);
      if not OptimFROG_getTags(FInstance, FTags) then begin
        Dispose(FTags);
        FTags := nil;

        Exit;
      end;
    end;

    for i := Low(FTags.keys) to Low(FTags.keys) + FTags.key_count - 1 do
      if (FTags.keys[i] <> nil) and (FTags.values[i] <> nil) and
         AnsiSameText(_key, Trim(String(FTags.keys[i])))
      then begin
        {$IF CompilerVersion < 20}
        Result := UTF8Decode(FTags.values[i]);
        {$IFEND}
        {$IF CompilerVersion >= 20}
        Result := UTF8ToString(FTags.values[i]);
        {$IFEND}
        Exit;
      end;
  end;
end;

function TOptimFROGDecoder.Seek(const Sample: Int64): Boolean;
begin
  Result := Seekable;
  if Result then begin
    CheckFunc(@OptimFROG_seekPoint, OptimFROG_seekPoint_name);

    Result := OptimFROG_seekPoint(FInstance, Sample);
  end;
end;

function TOptimFROGDecoder.Read(Buffer: Pointer; SampleCount: Cardinal): Integer;
begin
  if Buffer = nil then
    EOptimFROGException.Create('Buffer is nil!');

  if SampleCount > 0 then begin
    CheckFunc(@OptimFROG_read, OptimFROG_read_name);

    Result := OptimFROG_read(FInstance, Buffer, SampleCount);
  end
  else
    Result := 0;
end;


{ read interface functions }

function read_interface_close(stream: TStream): t_condition; cdecl;
begin
  Result := (stream <> nil); // ???
end;

function read_interface_read(stream: TStream; buffer: Pointer; count: Cardinal): Integer; cdecl;
begin
  if stream <> nil then
    Result := stream.Read(buffer^, count)
  else
    Result := 0;
end;

function read_interface_eof(stream: TStream): t_condition; cdecl;
begin
  Result := (stream = nil) or
    (stream.Position >= stream.Size);
end;

function read_interface_seekable(stream: TStream): t_condition; cdecl;
begin
  Result := (stream <> nil);
end;

function read_interface_length(stream: TStream): Int64; cdecl;
begin
  if stream <> nil then
    Result := stream.Size
  else
    Result := 0;
end;

function read_interface_get_pos(stream: TStream): Int64; cdecl;
begin
  if stream <> nil then
    Result := stream.Position
  else
    Result := 0;
end;

function read_interface_seek(stream: TStream; pos: Int64): t_condition; cdecl;
begin
  Result := (stream <> nil) and
    (stream.Seek(pos, soFromBeginning) = pos);
end;

initialization begin
  OptimFROGDLL_Handle := LoadLibrary(OptimFROGDLL_Name);
  OptimFROGDLL_Loaded := (OptimFROGDLL_Handle <> 0);
  if OptimFROGDLL_Loaded then begin
    OptimFROG_getVersion := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_getVersion_name);

    OptimFROG_createInstance := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_createInstance_name);
    OptimFROG_destroyInstance := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_destroyInstance_name);

    OptimFROG_openExt := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_openExt_name);

    OptimFROG_open := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_open_name);
    OptimFROG_close := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_close_name);

    OptimFROG_readHead := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_readHead_name);
    OptimFROG_readTail := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_readTail_name);

    OptimFROG_seekable := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_seekable_name);
    OptimFROG_seekPoint := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_seekPoint_name);
    OptimFROG_seekTime := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_seekTime_name);
    OptimFROG_getPos := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_getPos_name);
    OptimFROG_recoverableErrors := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_recoverableErrors_name);

    OptimFROG_read := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_read_name);

    OptimFROG_getInfo := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_getInfo_name);
    OptimFROG_getTags := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_getTags_name);
    OptimFROG_freeTags := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_freeTags_name);

    OptimFROG_decodeFile := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_decodeFile_name);
    OptimFROG_infoFile := GetProcAddress(OptimFROGDLL_Handle, OptimFROG_infoFile_name);

    read_interface.close    := @read_interface_close;
    read_interface.read     := @read_interface_read;
    read_interface.eof      := @read_interface_eof;
    read_interface.seekable := @read_interface_seekable;
    read_interface.length   := @read_interface_length;
    read_interface.get_pos  := @read_interface_get_pos;
    read_interface.seek     := @read_interface_seek;
  end;
end;

finalization begin
  if OptimFROGDLL_Loaded then
    FreeLibrary(OptimFROGDLL_Handle);
end;

end.

