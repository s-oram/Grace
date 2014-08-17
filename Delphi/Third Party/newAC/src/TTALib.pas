unit TTALib;

(* $Id: TTALib.pas 647 2008-07-02 05:12:26Z andrei.borovsky $ *)

interface

uses
  Windows, SysUtils, ACS_Classes;

const
  TTALib_Name = 'TTALib.dll';

var
  TTALib_Loaded: Boolean = False;

// from TTAError.h

type
  TTTAError = (
    TTA_NO_ERROR = $7FFFFFF1,
		FORMAT_ERROR,
		FILE_ERROR,
		FIND_ERROR,
		CREATE_ERROR,
		OPEN_ERROR,
		WRITE_ERROR,
		READ_ERROR,
		MEMORY_ERROR,
		TTA_CANCELED);

// from WavFile.h

const
  RIFF_SIGN = $46464952;
  WAVE_SIGN = $45564157;
  fmt_SIGN  = $20746D66;
  data_SIGN = $61746164;

type
  TWaveFileStruct = packed record
    // private
    ErrNo: {TTTAError}Integer;

    // public
    fd: THandle;

		// wave_hdr
    ChunkID: Cardinal;
    ChunkSize: Cardinal;
    Format: Cardinal;
    Subchunk1ID: Cardinal;
    Subchunk1Size: Cardinal;
    AudioFormat: Word;
    NumChannels: Word;
    SampleRate: Cardinal;
    ByteRate: Cardinal;
    BlockAlign: Word;
    BitsPerSample: Word;

		// subchunk_hdr;
		SubchunkID: Cardinal;
    SubchunkSize: Cardinal;
  end;
  PWaveFileStruct = ^TWaveFileStruct;

// from TTALib.h

const
  WAVE_FORMAT_PCM        = 1;
  WAVE_FORMAT_IEEE_FLOAT = 3;

type
	TTTAStat = packed record
    ratio: Double;
    input_bytes: Cardinal;
    output_bytes: Cardinal;
  end;
  PTTAStat = ^TTTAStat;

  TTTAEncoderStruct = packed record
    writer: Pointer;
    hFile: THandle;
    stat: TTTAStat;
  end;
  PTTAEncoderStruct = ^TTTAEncoderStruct;

  TTTADecoderStruct = packed record
		reader: Pointer;
    hFile: THandle;
    stat: TTTAStat;
  end;
  PTTADecoderStruct = ^TTTADecoderStruct;

  PPInteger = ^PInteger;

  { class TWaveFile }

  TWaveFile = class
	private
		FThis: TWaveFileStruct;

    function GetError: TTTAError;

		function Getfd: THandle;

    // wave_hdr
    function GetChunkID: Cardinal;
    function GetChunkSize: Cardinal;
    function GetFormat: Cardinal;
    function GetSubchunk1ID: Cardinal;
    function GetSubchunk1Size: Cardinal;
    function GetAudioFormat: Word;
    function GetNumChannels: Word;
    function GetSampleRate: Cardinal;
    function GetByteRate: Cardinal;
    function GetBlockAlign: Word;
    function GetBitsPerSample: Word;

    // subchunk_hdr
		function GetSubchunkID: Cardinal;
    function GetSubchunkSize: Cardinal;
	public
		constructor Create; overload;
    destructor Destroy; override;

		function New(const Filename: String): THandle; overload;
    function Open(const Filename: String): THandle;

		function ReadHeaders: Boolean;
    function Read(Data: Pointer; ByteSize: Integer; var Len: Cardinal): Boolean;
		
		function WriteHeaders: Boolean;
    function Write(Data: Pointer; ByteSize, NumChannels: Integer; Len: PCardinal): Boolean;

		procedure Close;
	
    property Error: TTTAError read GetError;

    property fd: THandle read Getfd;

		// wave_hdr
		property ChunkID: Cardinal read GetChunkID;
    property ChunkSize: Cardinal read GetChunkSize;
    property Format: Cardinal read GetFormat;
    property Subchunk1ID: Cardinal read GetSubchunk1ID;
    property Subchunk1Size: Cardinal read GetSubchunk1Size;
    property AudioFormat: Word read GetAudioFormat;
    property NumChannels: Word read GetNumChannels;
    property SampleRate: Cardinal read GetSampleRate;
    property ByteRate: Cardinal read GetByteRate;
    property BlockAlign: Word read GetBlockAlign;
    property BitsPerSample: Word read GetBitsPerSample;

		// subchunk_hdr	
		property SubchunkID: Cardinal read GetSubchunkID;
    property SubchunkSize: Cardinal read GetSubchunkSize;
  end;

  { class TTTADecoder }

  TTTADecoder = class
  private
    FThis: TTTADecoderStruct;

    FFilename: String;
    FHandle: THandle;

    function GetAudioFormat: Integer;
    function GetNumChannels: Integer;
    function GetBitsPerSample: Integer;
    function GetSampleRate: Integer;
    function GetDataLength: Integer;
    function GetStat: TTTAStat;
  public
    constructor Create(const AFilename: String); overload;
    constructor Create(AHandle: THandle); overload;
    destructor Destroy; override;

    function GetBlock(Buf: PPInteger): Integer;

    property AudioFormat: Integer read GetAudioFormat;
    property NumChannels: Integer read GetNumChannels;
    property BitsPerSample: Integer read GetBitsPerSample;
    property SampleRate: Integer read GetSampleRate;
    property DataLength: Integer read GetDataLength;

    property Stat: TTTAStat read GetStat;
  end;

  { class TTTAEncoder }

  TTTAEncoder = class
  private
    FThis: TTTAEncoderStruct;

    FFilename: String;
    FHandle: THandle;

    function GetStat: TTTAStat;
  public
    constructor Create(const AFilename: String;
      Append: Boolean;
      AudioFormat, NumChannels, BitsPerSample: Word;
      SampleRate, DataLength: Cardinal); overload;
    constructor Create(AHandle: THandle;
      Append: Boolean;
      AudioFormat, NumChannels, BitsPerSample: Word;
      SampleRate, DataLength: Cardinal); overload;
    destructor Destroy; override;

    function CompressBlock(Buf: Pointer; BufLength: Integer): Boolean;

    property Stat: TTTAStat read GetStat;
  end;

type
  ETTAException = class(Exception)
  end;

function GetErrorString(Error: TTTAError): String;

procedure LoadTTALib;
procedure UnloadTTALib;

implementation

var
  TTALib_Handle: HMODULE = 0;

// from WavFile.h

type
  t_WaveFile_Create_proc       = procedure; stdcall;
  t_WaveFile_Destroy_proc      = procedure; stdcall;
  t_WaveFile_GetErrNo_func     = function: Integer; stdcall;
  t_WaveFile_Assign_func       = function(WaveFile: PWaveFileStruct): PWaveFileStruct; stdcall;
  t_WaveFile_New_func          = function(Filename: PChar): THandle; stdcall;
  t_WaveFile_Open_func         = function(Filename: PChar): THandle; stdcall;
  t_WaveFile_ReadHeaders_func  = function: ByteBool; stdcall;
  t_WaveFile_Read_func         = function(Data: Pointer; ByteSize: Integer; Len: PCardinal): ByteBool; stdcall;
  t_WaveFile_WriteHeaders_func = function: ByteBool; stdcall;
  t_WaveFile_Write_func        = function(Data: Pointer; ByteSize, NumChannels: Integer; Len: PCardinal): ByteBool; stdcall;
  t_WaveFile_Close_proc        = procedure; stdcall;

var
  WaveFile_Create      : t_WaveFile_Create_proc = nil;
  WaveFile_Destroy     : t_WaveFile_Destroy_proc = nil;
  WaveFile_GetErrNo    : t_WaveFile_GetErrNo_func = nil;
  WaveFile_Assign      : t_WaveFile_Assign_func = nil;
  WaveFile_New         : t_WaveFile_New_func = nil;
  WaveFile_Open        : t_WaveFile_Open_func = nil;
  WaveFile_ReadHeaders : t_WaveFile_ReadHeaders_func = nil;
  WaveFile_Read        : t_WaveFile_Read_func = nil;
  WaveFile_WriteHeaders: t_WaveFile_WriteHeaders_func = nil;
  WaveFile_Write       : t_WaveFile_Write_func = nil;
  WaveFile_Close       : t_WaveFile_Close_proc = nil;

// from TTALib.h

type
  t_TTADecoder_CreateFromName_proc   = procedure(filename: PChar); stdcall;
  t_TTADecoder_CreateFromHandle_proc = procedure(hInFile: THandle); stdcall;
  t_TTADecoder_Destroy_proc          = procedure; stdcall;
  t_TTADecoder_Assign_func           = function(Decoder: PTTADecoderStruct): PTTADecoderStruct; stdcall;
  t_TTADecoder_GetBlock_func         = function(buf: PPInteger): Integer; stdcall;
  t_TTADecoder_GetAudioFormat_func   = function: Integer; stdcall;
  t_TTADecoder_GetNumChannels_func   = function: Integer; stdcall;
  t_TTADecoder_GetBitsPerSample_func = function: Integer; stdcall;
  t_TTADecoder_GetSampleRate_func    = function: Integer; stdcall;
  t_TTADecoder_GetDataLength_func    = function: Integer; stdcall;
  t_TTADecoder_GetStat_func          = function: TTTAStat; stdcall;

var
  TTADecoder_CreateFromName  : t_TTADecoder_CreateFromName_proc = nil;
  TTADecoder_CreateFromHandle: t_TTADecoder_CreateFromHandle_proc = nil;
  TTADecoder_Destroy         : t_TTADecoder_Destroy_proc = nil;
  TTADecoder_Assign          : t_TTADecoder_Assign_func = nil;
  TTADecoder_GetBlock        : t_TTADecoder_GetBlock_func = nil;
  TTADecoder_GetAudioFormat  : t_TTADecoder_GetAudioFormat_func = nil;
  TTADecoder_GetNumChannels  : t_TTADecoder_GetNumChannels_func = nil;
  TTADecoder_GetBitsPerSample: t_TTADecoder_GetBitsPerSample_func = nil;
  TTADecoder_GetSampleRate   : t_TTADecoder_GetSampleRate_func = nil;
  TTADecoder_GetDataLength   : t_TTADecoder_GetDataLength_func = nil;
  TTADecoder_GetStat         : t_TTADecoder_GetStat_func = nil;

type
  t_TTAEncoder_CreateWithName_proc   = procedure(filename: PChar; append: ByteBool; AudioFormat, NumChannels, BitsPerSample: Word; SampleRate, DataLength: Cardinal); stdcall;
  t_TTAEncoder_CreateWithHandle_proc = procedure(hInFile: THandle; append: ByteBool; AudioFormat, NumChannels, BitsPerSample: Word; SampleRate, DataLength: Cardinal); stdcall;
  t_TTAEncoder_Destroy_proc          = procedure; stdcall;
  t_TTAEncoder_Assign_func           = function(Encoder: PTTAEncoderStruct): PTTAEncoderStruct; stdcall;
  t_TTAEncoder_CompressBlock_func    = function(buf: PInteger; bufLen: Integer): ByteBool; stdcall;
  t_TTAEncoder_GetStat_func          = function: TTTAStat; stdcall;

var
  TTAEncoder_CreateWithName  : t_TTAEncoder_CreateWithName_proc = nil;
  TTAEncoder_CreateWithHandle: t_TTAEncoder_CreateWithHandle_proc = nil;
  TTAEncoder_Destroy         : t_TTAEncoder_Destroy_proc = nil;
  TTAEncoder_Assign          : t_TTAEncoder_Assign_func = nil;
  TTAEncoder_CompressBlock   : t_TTAEncoder_CompressBlock_func = nil;
  TTAEncoder_GetStat         : t_TTAEncoder_GetStat_func = nil;

type
  TTTACallback = function(stat: PTTAStat; uParam: Pointer): ByteBool; cdecl;

  t_GetErrStr_func = function(err: Integer): PChar; cdecl;

  t_CopyId3Header_func = function(hInFile, hOutFile: THandle; CopyID3v2Tag: ByteBool): TTTAError; cdecl;
  t_Wav2TTA_func = function(infile, outfile: PChar; CopyID3v2Tag: ByteBool = True; TTACallback: TTTACallback = nil; uParam: Pointer = nil): TTTAError; cdecl;
  t_TTA2Wav_func = function(infile, outfile: PChar; CopyID3v2Tag: ByteBool = True; TTACallback: TTTACallback = nil; uParam: Pointer = nil): TTTAError; cdecl;
  t_TTATest_func = function(infile: PChar; TTACallback: TTTACallback = nil; uParam: Pointer = nil): TTTAError; cdecl;

var
  _GetErrStr: t_GetErrStr_func = nil;

  _CopyId3Header: t_CopyId3Header_func = nil;
  _Wav2TTA: t_Wav2TTA_func = nil;
  _TTA2Wav: t_TTA2Wav_func = nil;
  _TTATest: t_TTATest_func = nil;


procedure CheckFunc(Func: Pointer; FuncName: String);
begin
  if TTALib_Handle = 0 then
    raise ETTAException.CreateFmt(
      'TrueAudio library "%s" not loaded!', [TTALib_name]);
  if Func = nil then
    raise ETTAException.CreateFmt(
      'Function "%s" not found in TrueAudio library!', [FuncName]);
end;

function GetErrorString(Error: TTTAError): String;
begin
  CheckFunc(@_GetErrStr, 'GetErrStr');

  Result := _GetErrStr(Integer(Error) and $0000000F);
end;


{ class TWaveFile }

constructor TWaveFile.Create;
var
  tmp: Pointer;
begin
  CheckFunc(@WaveFile_Create, 'WaveFile::WaveFile');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call WaveFile_Create
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to construct WAV-file object! Error: "%s"', [E.Message]);
  end;

  inherited Create();
end;

destructor TWaveFile.Destroy;
var
  tmp: Pointer;
begin
  CheckFunc(@WaveFile_Destroy, 'WaveFile::~WaveFile');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call WaveFile_Destroy
    end;
  except
  end;

  inherited;
end;

function TWaveFile.GetError: TTTAError;
var
  tmp: Pointer;
  res: Integer;
begin
  CheckFunc(@WaveFile_GetErrNo, 'WaveFile::GetErrNo');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call WaveFile_GetErrNo
      mov  res, eax
    end;

    Integer(Result) := res or $7FFFFFF0;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get error number from WAV-file! Error: "%s"', [E.Message]);
  end;
end;

function TWaveFile.Getfd: THandle;
begin
  Result := FThis.fd;
end;

function TWaveFile.GetChunkID: Cardinal;
begin
  Result := FThis.ChunkID;
end;

function TWaveFile.GetChunkSize: Cardinal;
begin
  Result := FThis.ChunkSize;
end;

function TWaveFile.GetFormat: Cardinal;
begin
  Result := FThis.Format;
end;

function TWaveFile.GetSubchunk1ID: Cardinal;
begin
  Result := FThis.Subchunk1ID;
end;

function TWaveFile.GetSubchunk1Size: Cardinal;
begin
  Result := FThis.Subchunk1Size;
end;

function TWaveFile.GetAudioFormat: Word;
begin
  Result := FThis.AudioFormat;
end;

function TWaveFile.GetNumChannels: Word;
begin
  Result := FThis.NumChannels;
end;

function TWaveFile.GetSampleRate: Cardinal;
begin
  Result := FThis.SampleRate;
end;

function TWaveFile.GetByteRate: Cardinal;
begin
  Result := FThis.ByteRate;
end;

function TWaveFile.GetBlockAlign: Word;
begin
  Result := FThis.BlockAlign;
end;

function TWaveFile.GetBitsPerSample: Word;
begin
  Result := FThis.BitsPerSample;
end;

function TWaveFile.GetSubchunkID: Cardinal;
begin
  Result := FThis.SubchunkID;
end;

function TWaveFile.GetSubchunkSize: Cardinal;
begin
  Result := FThis.SubchunkSize;
end;

function TWaveFile.New(const Filename: String): THandle;
var
  tmp1, tmp2: Pointer;
begin
  CheckFunc(@WaveFile_Create, 'WaveFile::Create');

  tmp1 := @FThis;
  tmp2 := @(Filename[1]);
  try
    asm
      push tmp2
      mov  ecx, tmp1
      call WaveFile_Create
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to create WAV-file! Error: "%s"', [E.Message]);
  end;
end;

function TWaveFile.Open(const Filename: String): THandle;
var
  tmp1, tmp2: Pointer;
begin
  CheckFunc(@WaveFile_Open, 'WaveFile::Open');

  tmp1 := @FThis;
  tmp2 := @(Filename[1]);
  try
    asm
      push tmp2
      mov  ecx, tmp1
      call WaveFile_Open
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to open WAV-file! Error: "%s"', [E.Message]);
  end;
end;

function TWaveFile.ReadHeaders: Boolean;
var
  tmp: Pointer;
  res: ByteBool;
begin
  CheckFunc(@WaveFile_ReadHeaders, 'WaveFile::ReadHeaders');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call WaveFile_ReadHeaders
      mov  res, al
    end;

    Result := res;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to read headers from WAV-file! Error: "%s"', [E.Message]);
  end;
end;

function TWaveFile.Read(Data: Pointer; ByteSize: Integer; var Len: Cardinal): Boolean;
var
  tmp1, tmp2: Pointer;
  res: ByteBool;
begin
  CheckFunc(@WaveFile_Read, 'WaveFile::Read');

  tmp1 := @FThis;
  tmp2 := @Len;
  try
    asm
      push tmp2
      push ByteSize
      push Data
      mov  ecx, tmp1
      call WaveFile_Read
      mov  res, al
    end;

    Result := res;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to read data from WAV-file! Error: "%s"', [E.Message]);
  end;
end;

function TWaveFile.WriteHeaders: Boolean;
var
  tmp: Pointer;
  res: ByteBool;
begin
  CheckFunc(@WaveFile_WriteHeaders, 'WaveFile::WriteHeaders');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call WaveFile_WriteHeaders
      mov  res, al
    end;

    Result := res;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to write headers to WAV-file! Error: "%s"', [E.Message]);
  end;
end;

function TWaveFile.Write(Data: Pointer; ByteSize, NumChannels: Integer; Len: PCardinal): Boolean;
var
  tmp: Pointer;
  res: ByteBool;
begin
  CheckFunc(@WaveFile_Write, 'WaveFile::Write');

  tmp := @FThis;
  try
    asm
      push Len
      push NumChannels
      push ByteSize
      push Data
      mov  ecx, tmp
      call WaveFile_Write
      mov  res, al
    end;

    Result := res;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to write data to WAV-file! Error: "%s"', [E.Message]);
  end;
end;

procedure TWaveFile.Close;
var
  tmp: Pointer;
begin
  CheckFunc(@WaveFile_Close, 'WaveFile::Close');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call WaveFile_Close
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to close WAV-file! Error: "%s"', [E.Message]);
  end;
end;


{ class TTTADecoder }

constructor TTTADecoder.Create(const AFilename: String);
var
  tmp1, tmp2: Pointer;
begin
  FFilename := Trim(AFilename);
  if FFilename = '' then
    raise ETTAException.Create('AFilename is empty!');

  CheckFunc(@TTADecoder_CreateFromName, 'TTADecoder::TTADecoder');

  tmp1 := @FThis;
  tmp2 := @(FFilename[1]);
  try
    asm
      push tmp2
      mov  ecx, tmp1
      call TTADecoder_CreateFromName
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to open TrueAudio-file! Error: "%s"', [E.Message]);
  end;

  inherited Create();
end;

constructor TTTADecoder.Create(AHandle: THandle);
var
  tmp: Pointer;
begin
  if AHandle = 0 then
    raise ETTAException.Create('AHandle is zero!');

  CheckFunc(@TTADecoder_CreateFromHandle, 'TTADecoder::TTADecoder');

  FHandle := AHandle;

  tmp := @FThis;
  try
    asm
      push AHandle
      mov  ecx, tmp
      call TTADecoder_CreateFromHandle
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to open TrueAudio-file! Error: "%s"', [E.Message]);
  end;

  inherited Create();
end;

destructor TTTADecoder.Destroy;
var
  tmp: Pointer;
begin
  CheckFunc(@TTADecoder_Destroy, 'TTADecoder::~TTADecoder');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTADecoder_Destroy
    end;
  except
  end;

  inherited;
end;

function TTTADecoder.GetAudioFormat: Integer;
var
  tmp: Pointer;
begin
  CheckFunc(@TTADecoder_GetAudioFormat, 'TTADecoder::GetAudioFormat');

  Result := 0;

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTADecoder_GetAudioFormat
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get audio format from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

function TTTADecoder.GetNumChannels: Integer;
var
  tmp: Pointer;
begin
  Result := 0;

  CheckFunc(@TTADecoder_GetNumChannels, 'TTADecoder::GetNumChannels');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTADecoder_GetNumChannels
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get number of channels from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

function TTTADecoder.GetBitsPerSample: Integer;
var
  tmp: Pointer;
begin
  Result := 0;

  CheckFunc(@TTADecoder_GetBitsPerSample, 'TTADecoder::GetBitsPerSample');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTADecoder_GetBitsPerSample
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get bits per sample from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

function TTTADecoder.GetSampleRate: Integer;
var
  tmp: Pointer;
begin
  Result := 0;

  CheckFunc(@TTADecoder_GetSampleRate, 'TTADecoder::GetSampleRate');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTADecoder_GetSampleRate
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get sample rate from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

function TTTADecoder.GetDataLength: Integer;
var
  tmp: Pointer;
begin
  Result := 0;
  CheckFunc(@TTADecoder_GetDataLength, 'TTADecoder::GetDataLength');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTADecoder_GetDataLength
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get data length from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

function TTTADecoder.GetStat: TTTAStat;
var
  tmp1, tmp2: Pointer;
{  i: Integer;
  res: PByteArray;
  tmp3: Byte;}
begin
  CheckFunc(@TTADecoder_GetStat, 'TTADecoder::GetStat');

  Result.ratio := 0;
  Result.input_bytes := 0;
  Result.output_bytes := 0;

  tmp1 := @FThis;
  tmp2 := @Result;
  try
    asm
      push tmp2
      mov  ecx, tmp1
      call TTADecoder_GetStat
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get statistic from TrueAudio-file! Error: "%s"', [E.Message]);
  end;

{  res := @Result;
  for i := 0 to (SizeOf(Result) div 2) - 1 do begin
    tmp3 := res[i];
    res[i] := res[SizeOf(Result) - 1 - i];
    res[SizeOf(Result) - 1 - i] := tmp3;
  end;}
end;

function TTTADecoder.GetBlock(Buf: PPInteger): Integer;
var
  tmp: Pointer;
begin
  CheckFunc(@TTADecoder_GetBlock, 'TTADecoder_GetBlock');

  Result := 0;

  tmp := @FThis;
  try
    asm
      push Buf
      mov  ecx, tmp
      call TTADecoder_GetBlock
      mov  @Result, eax
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get data block from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;


{ class TTTAEncoder }

constructor TTTAEncoder.Create(const AFilename: String;
  Append: Boolean;
  AudioFormat, NumChannels, BitsPerSample: Word;
  SampleRate, DataLength: Cardinal);
var
  tmp1, tmp2: Pointer;
  str: String;
begin
  FFilename := Trim(AFilename);
  if FFilename = '' then
    raise ETTAException.Create('AFilename is empty!');

  CheckFunc(@TTAEncoder_CreateWithName, 'TTAEncoder::TTAEncoder');

  tmp1 := @FThis;
  str := Copy(FFilename, 1, Length(FFilename));
  tmp2 := @(str[1]);
  try
    asm
      push DataLength
      push SampleRate
      push Integer(BitsPerSample)
      push Integer(NumChannels)
      push Integer(AudioFormat)
      push Integer(ByteBool(Append))
      push tmp2
      mov  ecx, tmp1
      call TTAEncoder_CreateWithName
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to open TrueAudio-file! Error: "%s"; OS Error: %d',
        [E.Message, GetLastError()]);
  end;

  inherited Create();
end;

constructor TTTAEncoder.Create(AHandle: THandle;
  Append: Boolean;
  AudioFormat, NumChannels, BitsPerSample: Word;
  SampleRate, DataLength: Cardinal);
var
  tmp: Pointer;
begin
  if (AHandle = 0) or (AHandle = INVALID_HANDLE_VALUE) then
    raise ETTAException.Create('AHandle is invalid!');

  CheckFunc(@TTAEncoder_CreateWithHandle, 'TTAEncoder::TTAEncoder');

  FHandle := AHandle;

  tmp := @FThis;
  try
    asm
      push DataLength
      push SampleRate
      push Integer(BitsPerSample)
      push Integer(NumChannels)
      push Integer(AudioFormat)
      push Integer(ByteBool(Append))
      push AHandle
      mov  ecx, tmp
      call TTAEncoder_CreateWithHandle
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to open TrueAudio-file! Error: "%s"', [E.Message]);
  end;

  inherited Create();
end;

destructor TTTAEncoder.Destroy;
var
  tmp: Pointer;
begin
  CheckFunc(@TTAEncoder_Destroy, 'TTAEncoder::~TTAEncoder');

  tmp := @FThis;
  try
    asm
      mov  ecx, tmp
      call TTAEncoder_Destroy
    end;
  except
  end;

  inherited;
end;

function TTTAEncoder.GetStat: TTTAStat;
var
  tmp1, tmp2: Pointer;
begin
  CheckFunc(@TTAEncoder_GetStat, 'TTAEncoder::GetStat');

  Result.ratio := 0;
  Result.input_bytes := 0;
  Result.output_bytes := 0;

  tmp1 := @FThis;
  tmp2 := @Result;
  try
    asm
      push tmp2
      mov  ecx, tmp1
      call TTAEncoder_GetStat
    end;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to get statistic from TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

function TTTAEncoder.CompressBlock(Buf: Pointer; BufLength: Integer): Boolean;
var
  tmp: Pointer;
  res: ByteBool;
begin
  CheckFunc(@TTAEncoder_CompressBlock, 'TTAEncoder_CompressBlock');

  tmp := @FThis;
  try
    asm
      push BufLength
      push Buf
      mov  ecx, tmp
      call TTAEncoder_CompressBlock
      mov  res, al
    end;

    Result := res;
  except
    on E: Exception do
      raise ETTAException.CreateFmt(
        'Unable to compress block to TrueAudio-file! Error: "%s"', [E.Message]);
  end;
end;

procedure LoadTTALib;
begin
  LoadLibCS.Enter;
  if TTALib_Loaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  TTALib_Handle := LoadLibrary(TTALib_name);
  TTALib_Loaded := (TTALib_Handle <> 0);
  if TTALib_Loaded then begin
    WaveFile_Create  := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::WaveFile::WaveFile(void)')}
      {MAKEINTRESOURCE(5)}
      '??0WaveFile@TTALib@@QAE@XZ');
    WaveFile_Destroy   := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::WaveFile::~WaveFile(void)'}
      {MAKEINTRESOURCE(8)}
      '??1WaveFile@TTALib@@QAE@XZ');
    WaveFile_GetErrNo     := GetProcAddress(TTALib_Handle,
      {'public: enum TTALib::TTAError __thiscall TTALib::WaveFile::GetErrNo(void)const'}
      {MAKEINTRESOURCE(21)}
      '?GetErrNo@WaveFile@TTALib@@QBE?AW4TTAError@2@XZ');
    WaveFile_Assign       := GetProcAddress(TTALib_Handle,
      {'public: class TTALib::WaveFile & __thiscall TTALib::WaveFile::operator=(class TTALib::WaveFile const &)'}
      {MAKEINTRESOURCE(12)}
      '??4WaveFile@TTALib@@QAEAAV01@ABV01@@Z');
    WaveFile_Create       := GetProcAddress(TTALib_Handle,
      {'public: void * __thiscall TTALib::WaveFile::Create(char const *)'}
      {MAKEINTRESOURCE(16)}
      '?Create@WaveFile@TTALib@@QAEPAXPBD@Z');
    WaveFile_Open         := GetProcAddress(TTALib_Handle,
      {'public: void * __thiscall TTALib::WaveFile::Open(char const *)'}
      {MAKEINTRESOURCE(27)}
      '?Open@WaveFile@TTALib@@QAEPAXPBD@Z');
    WaveFile_ReadHeaders  := GetProcAddress(TTALib_Handle,
      {'public: bool __thiscall TTALib::WaveFile::ReadHeaders(void)'}
      {MAKEINTRESOURCE(29)}
      '?ReadHeaders@WaveFile@TTALib@@QAE_NXZ');
    WaveFile_Read         := GetProcAddress(TTALib_Handle,
      {'public: bool __thiscall TTALib::WaveFile::Read(long *,long,unsigned long *)'}
      {MAKEINTRESOURCE(28)}
      '?Read@WaveFile@TTALib@@QAE_NPAJJPAK@Z');
    WaveFile_WriteHeaders := GetProcAddress(TTALib_Handle,
      {'public: bool __thiscall TTALib::WaveFile::WriteHeaders(void)'}
      {MAKEINTRESOURCE(34)}
      '?WriteHeaders@WaveFile@TTALib@@QAE_NXZ');
    WaveFile_Write        := GetProcAddress(TTALib_Handle,
      {'public: bool __thiscall TTALib::WaveFile::Write(long *,long,long,unsigned long *)'}
      {MAKEINTRESOURCE(33)}
      '?Write@WaveFile@TTALib@@QAE_NPAJJJPAK@Z');
    WaveFile_Close        := GetProcAddress(TTALib_Handle,
      {'public: void __thiscall TTALib::WaveFile::Close(void)'}
      {MAKEINTRESOURCE(13)}
      '?Close@WaveFile@TTALib@@QAEXXZ');

    TTADecoder_CreateFromName   := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::TTADecoder::TTADecoder(char const *)'}
      {MAKEINTRESOURCE(2)}
      '??0TTADecoder@TTALib@@QAE@PBD@Z');
    TTADecoder_CreateFromHandle := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::TTADecoder::TTADecoder(void *)'}
      {MAKEINTRESOURCE(1)}
      '??0TTADecoder@TTALib@@QAE@PAX@Z');
    TTADecoder_Destroy          := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::TTADecoder::~TTADecoder(void)'}
      {MAKEINTRESOURCE(6)}
      '??1TTADecoder@TTALib@@QAE@XZ');
    TTADecoder_Assign           := GetProcAddress(TTALib_Handle,
      {'public: class TTALib::TTADecoder & __thiscall TTALib::TTADecoder::operator=(class TTALib::TTADecoder const &)'}
      {MAKEINTRESOURCE(9)}
      '??4TTADecoder@TTALib@@QAEAAV01@ABV01@@Z');
    TTADecoder_GetBlock         := GetProcAddress(TTALib_Handle,
      {'public: long __thiscall TTALib::TTADecoder::GetBlock(long * *)'}
      {MAKEINTRESOURCE(19)}
      '?GetBlock@TTADecoder@TTALib@@QAEJPAPAJ@Z');
    TTADecoder_GetAudioFormat   := GetProcAddress(TTALib_Handle,
      {'public: long __thiscall TTALib::TTADecoder::GetAudioFormat(void)'}
      {MAKEINTRESOURCE(17)}
      '?GetAudioFormat@TTADecoder@TTALib@@QAEJXZ');
    TTADecoder_GetNumChannels   := GetProcAddress(TTALib_Handle,
      {'public: long __thiscall TTALib::TTADecoder::GetNumChannels(void)'}
      {MAKEINTRESOURCE(23)}
      '?GetNumChannels@TTADecoder@TTALib@@QAEJXZ');
    TTADecoder_GetBitsPerSample := GetProcAddress(TTALib_Handle,
      {'public: long __thiscall TTALib::TTADecoder::GetBitsPerSample(void)'}
      {MAKEINTRESOURCE(18)}
      '?GetBitsPerSample@TTADecoder@TTALib@@QAEJXZ');
    TTADecoder_GetSampleRate    := GetProcAddress(TTALib_Handle,
      {'public: long __thiscall TTALib::TTADecoder::GetSampleRate(void)'}
      {MAKEINTRESOURCE(24)}
      '?GetSampleRate@TTADecoder@TTALib@@QAEJXZ');
    TTADecoder_GetDataLength    := GetProcAddress(TTALib_Handle,
      {'public: long __thiscall TTALib::TTADecoder::GetDataLength(void)'}
      {MAKEINTRESOURCE(20)}
      '?GetDataLength@TTADecoder@TTALib@@QAEJXZ');
    TTADecoder_GetStat          := GetProcAddress(TTALib_Handle,
      {'public: struct TTALib::TTAStat __thiscall TTALib::TTADecoder::GetStat(void)'}
      {MAKEINTRESOURCE(25)}
      '?GetStat@TTADecoder@TTALib@@QAE?AUTTAStat@2@XZ');

    TTAEncoder_CreateWithName   := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::TTAEncoder::TTAEncoder(char const *,bool,unsigned short,unsigned short,unsigned short,unsigned long,unsigned long)'}
      {MAKEINTRESOURCE(4)}
      '??0TTAEncoder@TTALib@@QAE@PBD_NGGGKK@Z');
    TTAEncoder_CreateWithHandle := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::TTAEncoder::TTAEncoder(void *,bool,unsigned short,unsigned short,unsigned short,unsigned long,unsigned long)'}
      {MAKEINTRESOURCE(3)}
      '??0TTAEncoder@TTALib@@QAE@PAX_NGGGKK@Z');
    TTAEncoder_Destroy          := GetProcAddress(TTALib_Handle,
      {'public: __thiscall TTALib::TTAEncoder::~TTAEncoder(void)'}
      {MAKEINTRESOURCE(7)}
      '??1TTAEncoder@TTALib@@QAE@XZ');
    TTAEncoder_Assign           := GetProcAddress(TTALib_Handle,
      {'public: class TTALib::TTAEncoder & __thiscall TTALib::TTAEncoder::operator=(class TTALib::TTAEncoder const &)'}
      {MAKEINTRESOURCE(10)}
      '??4TTAEncoder@TTALib@@QAEAAV01@ABV01@@Z');
    TTAEncoder_CompressBlock    := GetProcAddress(TTALib_Handle,
      {'public: bool __thiscall TTALib::TTAEncoder::CompressBlock(long *,long)'}
      {MAKEINTRESOURCE(14)}
      '?CompressBlock@TTAEncoder@TTALib@@QAE_NPAJJ@Z');
    TTAEncoder_GetStat          := GetProcAddress(TTALib_Handle,
      {'public: struct TTALib::TTAStat __thiscall TTALib::TTAEncoder::GetStat(void)'}
      {MAKEINTRESOURCE(26)}
      '?GetStat@TTAEncoder@TTALib@@QAE?AUTTAStat@2@XZ');

    _GetErrStr     := GetProcAddress(TTALib_Handle,
      {'char const * __cdecl TTALib::GetErrStr(enum TTALib::TTAError)'}
      {MAKEINTRESOURCE(22)}
      '?GetErrStr@TTALib@@YAPBDW4TTAError@1@@Z');
    _CopyId3Header := GetProcAddress(TTALib_Handle,
      {'enum TTALib::TTAError __cdecl TTALib::CopyId3Header(void *,void *,bool)'}
      {MAKEINTRESOURCE(15)}
      '?CopyId3Header@TTALib@@YA?AW4TTAError@1@PAX0_N@Z');
    _Wav2TTA       := GetProcAddress(TTALib_Handle,
      {'enum TTALib::TTAError __cdecl TTALib::Wav2TTA(char const *,char const *,bool,bool (__cdecl*)(struct TTALib::TTAStat const &,void *),void *)'}
      {MAKEINTRESOURCE(32)}
      '?Wav2TTA@TTALib@@YA?AW4TTAError@1@PBD0_NP6A_NABUTTAStat@1@PAX@Z3@Z');
    _TTA2Wav       := GetProcAddress(TTALib_Handle,
      {'enum TTALib::TTAError __cdecl TTALib::TTA2Wav(char const *,char const *,bool,bool (__cdecl*)(struct TTALib::TTAStat const &,void *),void *)'}
      {MAKEINTRESOURCE(30)}
      '?TTA2Wav@TTALib@@YA?AW4TTAError@1@PBD0_NP6A_NABUTTAStat@1@PAX@Z3@Z');
    _TTATest       := GetProcAddress(TTALib_Handle,
      {'enum TTALib::TTAError __cdecl TTALib::TTATest(char const *,bool (__cdecl*)(struct TTALib::TTAStat const &,void *),void *)'}
      {MAKEINTRESOURCE(31)}
      '?TTATest@TTALib@@YA?AW4TTAError@1@PBDP6A_NABUTTAStat@1@PAX@Z2@Z');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadTTALib;
begin
  if TTALib_Handle <> 0 then
    FreeLibrary(TTALib_Handle);
  TTALib_Loaded := False;
end;

end.

