unit MACDll;
{
  Delphi interface to MACDll.dll 3.97
  by Thomas la Cour 30-04-2003
  http://www.top-house.dk/~nr161/delphi/

  still missing:
  StartEx for TAPECompress

  Slightly modified by Andrei Borovsky for better ACS complience.
}

interface

uses Classes, SysUtils, Windows, MMSystem;

const
   MACPath = 'MACDll.dll';

var
  MACLoaded : Boolean = False;

type
  TWaveFormatEx = MMSystem.TWaveFormatEx;

////////////////////////////////////////////////////////////////////////////////
// MAC Error Codes
////////////////////////////////////////////////////////////////////////////////
const
  // file and i/o errors (1000's)
  MAC_ERROR_IO_READ = 1000;
  MAC_ERROR_IO_WRITE = 1001;
  MAC_ERROR_INVALID_INPUT_FILE = 1002;
  MAC_ERROR_INVALID_OUTPUT_FILE = 1003;
  MAC_ERROR_INPUT_FILE_TOO_LARGE = 1004;
  MAC_ERROR_INPUT_FILE_UNSUPPORTED_BIT_DEPTH = 1005;
  MAC_ERROR_INPUT_FILE_UNSUPPORTED_SAMPLE_RATE = 1006;
  MAC_ERROR_INPUT_FILE_UNSUPPORTED_CHANNEL_COUNT = 1007;
  MAC_ERROR_INPUT_FILE_TOO_SMALL = 1008;
  MAC_ERROR_INVALID_CHECKSUM = 1009;
  MAC_ERROR_DECOMPRESSING_FRAME = 1010;
  MAC_ERROR_INITIALIZING_UNMAC = 1011;
  MAC_ERROR_INVALID_FUNCTION_PARAMETER = 1012;
  MAC_ERROR_UNSUPPORTED_FILE_TYPE = 1013;
  MAC_ERROR_UPSUPPORTED_FILE_VERSION = 1014;

  // memory errors (2000's)
  MAC_ERROR_INSUFFICIENT_MEMORY = 2000;

  // dll errors (3000's)
  MAC_ERROR_LOADINGAPE_DLL = 3000;
  MAC_ERROR_LOADINGAPE_INFO_DLL = 3001;
  MAC_ERROR_LOADING_UNMAC_DLL = 3002;

  // general and misc errors
  MAC_ERROR_USER_STOPPED_PROCESSING = 4000;
  MAC_ERROR_SKIPPED = 4001;

  // programmer errors
  MAC_ERROR_BAD_PARAMETER = 5000;

  // IAPECompress errors
  MAC_ERROR_APE_COMPRESS_TOO_MUCH_DATA = 6000;

  // unknown error
  MAC_ERROR_UNDEFINED = -1;

////////////////////////////////////////////////////////////////////////////////
// MAC compression codes
////////////////////////////////////////////////////////////////////////////////
const
  COMPRESSION_LEVEL_FAST = 1000;
  COMPRESSION_LEVEL_NORMAL = 2000;
  COMPRESSION_LEVEL_HIGH = 3000;
  COMPRESSION_LEVEL_EXTRA_HIGH = 4000;

  MAC_FORMAT_FLAG_8_BIT = 1; // is 8-bit
  MAC_FORMAT_FLAG_CRC = 2; // uses the new CRC32 error detection
  MAC_FORMAT_FLAG_HAS_PEAK_LEVEL = 4; // unsigned __int32 Peak_Level after the header
  MAC_FORMAT_FLAG_24_BIT = 8; // is 24-bit
  MAC_FORMAT_FLAG_HAS_SEEK_ELEMENTS = 16; // has the number of seek elements after the peak level
  MAC_FORMAT_FLAG_CREATE_WAV_HEADER = 32; // create the wave header on decompression (not stored)

  CREATE_WAV_HEADER_ON_DECOMPRESSION = -1;
  MAX_AUDIO_BYTES_UNKNOWN = -1;

type
  macProgressCallback_t = procedure(PercentageDone: LongInt); stdcall;

var

 macProgressCallback : macProgressCallback_t;

////////////////////////////////////////////////////////////////////////////////
// WAV header structure
////////////////////////////////////////////////////////////////////////////////
type
  TWaveHeader = packed record
    // RIFF header
    cRIFFHeader: array[0..3] of char; //unsigned int nRIFFBytes;
    nRIFFBytes: Longword;
    // data type
    cDataTypeID: array[0..3] of char; //char cDataTypeID[4];
    // wave format
    cFormatHeader: array[0..3] of char; //char cFormatHeader[4];
    nFormatBytes: Longword; //unsigned int nFormatBytes;
    nFormatTag: Word; //unsigned short nFormatTag;
    nChannels: Word; //unsigned short nChannels;
    nSamplesPerSec: Longword; //unsigned int nSamplesPerSec;
    nAvgBytesPerSec: Longword; //unsigned int nAvgBytesPerSec;
    nBlockAlign: Word; //unsigned short nBlockAlign;
    nBitsPerSample: Word; //unsigned short nBitsPerSample;
    // data chunk header
    cDataHeader: array[0..3] of char; //char cDataHeader[4];
    nDataBytes: Longword; //unsigned int nDataBytes;
  end;
  PWaveHeader = ^TWaveHeader;

{*****************************************************************************************
APE header structure (what's at the front of an APE file)
*****************************************************************************************}
type
  TAPEHeader = packed record
    cID: array[0..3] of char; // should equal 'MAC '
    nVersion: SmallInt; // version number * 1000 (3.81 = 3810)
    nCompressionLevel: SmallInt; // the compression level
    nFormatFlags: SmallInt; // any format flags (for future use)
    nChannels: SmallInt; // the number of channels (1 or 2)
    nSampleRate: LongInt; // the sample rate (typically 44100)
    nHeaderBytes: LongInt; // the bytes after the MAC header that compose the WAV header
    nTerminatingBytes: LongInt; // the bytes after that raw data (for extended info)
    nTotalFrames: LongInt; // the number of frames in the file
    nFinalFrameBlocks: LongInt; // the number of samples in the final frame
  end;
  PAPEHeader = ^TAPEHeader;

{*******************************************************************************
TAPEDecompress fields - used when querying for information

Note(s):
-the distinction between APE_INFO_XXXX and APE_DECOMPRESS_XXXX is that the
first is querying the APE information engine, and the other is querying the
decompressor, and since the decompressor can be a range of an APE file (for
APL), differences will arise.  Typically, use the APE_DECOMPRESS_XXXX fields
when querying for info about the length, etc. so APL will work properly.
(i.e. (APE_INFO_TOTAL_BLOCKS != APE_DECOMPRESS_TOTAL_BLOCKS) for APL files)
*******************************************************************************}
type
  APE_DECOMPRESS_FIELDS = Integer;
const
  APE_INFO_FILE_VERSION = 1000; // version of the APE file * 1000 (3.93 = 3930) [ignored, ignored]
  APE_INFO_COMPRESSION_LEVEL = 1001; // compression level of the APE file [ignored, ignored]
  APE_INFO_FORMAT_FLAGS = 1002; // format flags of the APE file [ignored, ignored]
  APE_INFO_SAMPLE_RATE = 1003; // sample rate (Hz) [ignored, ignored]
  APE_INFO_BITS_PER_SAMPLE = 1004; // bits per sample [ignored, ignored]
  APE_INFO_BYTES_PER_SAMPLE = 1005; // number of bytes per sample [ignored, ignored]
  APE_INFO_CHANNELS = 1006; // channels [ignored, ignored]
  APE_INFO_BLOCK_ALIGN = 1007; // block alignment [ignored, ignored]
  APE_INFO_BLOCKS_PER_FRAME = 1008; // number of blocks in a frame (frames are used internally)  [ignored, ignored]
  APE_INFO_FINAL_FRAME_BLOCKS = 1009; // blocks in the final frame (frames are used internally) [ignored, ignored]
  APE_INFO_TOTAL_FRAMES = 1010; // total number frames (frames are used internally) [ignored, ignored]
  APE_INFO_WAV_HEADER_BYTES = 1011; // header bytes of the decompressed WAV [ignored, ignored]
  APE_INFO_WAV_TERMINATING_BYTES = 1012; // terminating bytes of the decompressed WAV [ignored, ignored]
  APE_INFO_WAV_DATA_BYTES = 1013; // data bytes of the decompressed WAV [ignored, ignored]
  APE_INFO_WAV_TOTAL_BYTES = 1014; // total bytes of the decompressed WAV [ignored, ignored]
  APE_INFO_APE_TOTAL_BYTES = 1015; // total bytes of the APE file [ignored, ignored]
  APE_INFO_TOTAL_BLOCKS = 1016; // total blocks of audio data [ignored, ignored]
  APE_INFO_LENGTH_MS = 1017; // length in ms (1 sec = 1000 ms) [ignored, ignored]
  APE_INFO_AVERAGE_BITRATE = 1018; // average bitrate of the APE [ignored, ignored]
  APE_INFO_FRAME_BITRATE = 1019; // bitrate of specified APE frame [frame index, ignored]
  APE_INFO_DECOMPRESSED_BITRATE = 1020; // bitrate of the decompressed WAV [ignored, ignored]
  APE_INFO_PEAK_LEVEL = 1021; // peak audio level (-1 is unknown) [ignored, ignored]
  APE_INFO_SEEK_BIT = 1022; // bit offset [frame index, ignored]
  APE_INFO_SEEK_BYTE = 1023; // byte offset [frame index, ignored]
  APE_INFO_WAV_HEADER_DATA = 1024; // error code [buffer *, max bytes]
  APE_INFO_WAV_TERMINATING_DATA = 1025; // error code [buffer *, max bytes]
  APE_INFO_WAVEFORMATEX = 1026; // error code [waveformatex *, ignored]
  APE_INFO_IO_SOURCE = 1027; // I/O source (CIO *) [ignored, ignored]
  APE_INFO_FRAME_BYTES = 1028; // bytes (compressed) of the frame [frame index, ignored]
  APE_INFO_FRAME_BLOCKS = 1029; // blocks in a given frame [frame index, ignored]
  APE_INFO_TAG = 1030; // point to tag (CAPETag *) [ignored, ignored]

  APE_DECOMPRESS_CURRENT_BLOCK = 2000; // current block location [ignored, ignored]
  APE_DECOMPRESS_CURRENT_MS = 2001; // current millisecond location [ignored, ignored]
  APE_DECOMPRESS_TOTAL_BLOCKS = 2002; // total blocks in the decompressors range [ignored, ignored]
  APE_DECOMPRESS_LENGTH_MS = 2003; // total blocks in the decompressors range [ignored, ignored]
  APE_DECOMPRESS_CURRENT_BITRATE = 2004; // current bitrate [ignored, ignored]
  APE_DECOMPRESS_AVERAGE_BITRATE = 2005; // average bitrate (works with ranges) [ignored, ignored]

////////////////////////////////////////////////////////////////////////////////
// misc types
////////////////////////////////////////////////////////////////////////////////
type
  ID3_TAG = record
    TagHeader: array[1..3] of Char; // should equal 'TAG'
    Title: array[1..30] of Char; // title
    Artist: array[1..30] of Char; // artist
    Album: array[1..30] of Char; // album
    Year: array[1..4] of Char; // year
    Comment: array[1..30] of Char; // comment
    Track: Byte; // track
    Genre: Byte; // genre
  end;
  pID3_TAG = ^ID3_TAG;

type
  APE_DECOMPRESS_HANDLE = LongWord;//Pointer;
  APE_COMPRESS_HANDLE = LongWord;//Pointer;

////////////////////////////////////////////////////////////////////////////////
// Simple functions - see the SDK sample projects for usage examples
////////////////////////////////////////////////////////////////////////////////

type

 macRemoveTag_t = function(pFilename: PChar): LongInt; stdcall;

 macCompressFile_t = function (
  InputFile: PChar;
  OutputFile: PChar;
  CompressionLevel: LongInt;
  PercentageDone: PInteger;
  ProgressCalllback: macProgressCallback_t;
  KillFlag: PInteger
  ): LongInt; stdcall;

  macConvertFile_t = function(
  InputFile: PChar;
  OutputFile: PChar;
  CompressionLevel: LongInt;
  var PercentageDone: Integer;
  ProgressCalllback: macProgressCallback_t;
  var KillFlag: Integer
  ): LongInt; stdcall;

  macDecompressFile_t = function(
  InputFile: PChar;
  OutputFile: PChar;
  var PercentageDone: Integer; //Pointer;
  ProgressCalllback: macProgressCallback_t;
  var KillFlag: Integer
  ): LongInt; stdcall;

  macFillWaveFormatEx_t = function(
  var WaveFormatEx: TWaveFormatEx;
  SampleRate: Integer = 44100;
  BitsPerSample: Integer = 16;
  Channels: Integer = 2
  ): LongInt; stdcall;

  macFillWaveHeader_t = function(
  var WaveHeader: TWaveHeader;
  AudioBytes: Integer;
  var WaveFormatEx: TWaveFormatEx;
  TerminatingBytes: Integer = 0
  ): LongInt; stdcall;

  macGetID3Tag_t = function(
  pFilename: PChar;
  pID3Tag: pID3_TAG
  ): LongInt; stdcall;

  macGetInterfaceCompatibility_t = function(
  nVersion: Integer;
  bDisplayWarningsOnFailure: Boolean;
  hwndParent: HWND
  ): LongInt; stdcall;

  macGetVersionNumber_t = function(): LongInt; stdcall;

  macShowFileInfoDialog_t = function(
  Filename: PChar;
  HWndWindow: HWND
  ): LongInt; stdcall;

  macTagFileSimple_t = function(
  pFilename: PChar;
  pArtist: PChar;
  pAlbum: PChar;
  pTitle: PChar;
  pComment: PChar;
  pGenre: PChar;
  pYear: PChar;
  pTrack: PChar;
  bClearFirst: boolean;
  bUseOldID3: boolean
  ): LongInt; stdcall;

  macVerifyFile_t = function(
  InputFile: PChar;
  var PercentageDone: Integer;
  ProgressCalllback: macProgressCallback_t;
  var KillFlag: Integer
  ): LongInt; stdcall;

//
// Utility functions
//

function macErrorExplanation(MacErrorCode: LongInt): string;

function macMSToTime(MilliSeconds: LongInt): Double;

var

  macRemoveTag : macRemoveTag_t;
  macCompressFile : macCompressFile_t;
  macConvertFile : macConvertFile_t;
  macDecompressFile : macDecompressFile_t;
  macFillWaveFormatEx : macFillWaveFormatEx_t;
  macFillWaveHeader : macFillWaveHeader_t;
  macGetID3Tag : macGetID3Tag_t;
  macGetInterfaceCompatibility : macGetInterfaceCompatibility_t;
  macGetVersionNumber : macGetVersionNumber_t;
  macShowFileInfoDialog : macShowFileInfoDialog_t;
  macTagFileSimple : macTagFileSimple_t;
  macVerifyFile : macVerifyFile_t;

//
// EAPEException
//
type
  EAPEException = class(Exception)
  public
    constructor Create(MacErrorCode: LongInt); overload;
  end;

{*************************************************************************************************
TAPECompress - class for creating APE files

Usage:

 To create an APE file, you Start(...), then add data (in a variety of ways), then Finish(...)
*************************************************************************************************}
  TAPECompress = class
  private
    FHandle: APE_COMPRESS_HANDLE;
    //FFilename: string;
  public
    ErrorCode: Integer;
    constructor Create;
    destructor Destroy; override;

 {*********************************************************************************************
 * Start
 *********************************************************************************************}

 //////////////////////////////////////////////////////////////////////////////////////////////
 // Start(...) / StartEx(...) - starts encoding
 //
 // Parameters:
 //	CIO * pioOutput / const char * pFilename
 //		the output... either a filename or an I/O source
 //	WAVEFORMATEX * pwfeInput
 //		format of the audio to encode (use FillWaveFormatEx() if necessary)
 //	int nMaxAudioBytes
 //		the absolute maximum audio bytes that will be encoded... encoding fails with a
 //		ERROR_APE_COMPRESS_TOO_MUCH_DATA if you attempt to encode more than specified here
 //		(if unknown, use MAX_AUDIO_BYTES_UNKNOWN to allocate as much storage in the seek table as
 //		possible... limit is then 2 GB of data (~4 hours of CD music)... this wastes around
 //		30kb, so only do it if completely necessary)
 //	int nCompressionLevel
 //		the compression level for the APE file (fast - extra high)
 //		(note: extra-high is much slower for little gain)
 //	const unsigned char * pHeaderData
 //		a pointer to a buffer containing the WAV header (data before the data block in the WAV)
 //		(note: use NULL for on-the-fly encoding... see next parameter)
 //	int nHeaderBytes
 //		number of bytes in the header data buffer (use CREATE_WAV_HEADER_ON_DECOMPRESSION and
 //		NULL for the pHeaderData and MAC will automatically create the appropriate WAV header
 //		on decompression)
 //////////////////////////////////////////////////////////////////////////////////////////////

 {virtual int Start(const char * pOutputFilename, const WAVEFORMATEX * pwfeInput,
  int nMaxAudioBytes = MAX_AUDIO_BYTES_UNKNOWN, int nCompressionLevel = COMPRESSION_LEVEL_NORMAL,
  const unsigned char * pHeaderData = NULL, int nHeaderBytes = CREATE_WAV_HEADER_ON_DECOMPRESSION) = 0;}
    function Start(
      pOutputFilename: PChar;
      pwfeInput: PWaveFormatEx;
      nMaxAudioBytes: Integer = MAX_AUDIO_BYTES_UNKNOWN;
      nCompressionLevel: Integer = COMPRESSION_LEVEL_NORMAL;
      pHeaderData: Pointer = nil;
      nHeaderBytes: Integer = CREATE_WAV_HEADER_ON_DECOMPRESSION
      ): LongInt;

 {virtual int StartEx(CIO * pioOutput, const WAVEFORMATEX * pwfeInput,
  int nMaxAudioBytes = MAX_AUDIO_BYTES_UNKNOWN, int nCompressionLevel = COMPRESSION_LEVEL_NORMAL,
  const unsigned char * pHeaderData = NULL, int nHeaderBytes = CREATE_WAV_HEADER_ON_DECOMPRESSION) = 0;}

 {*********************************************************************************************
 * Add / Compress Data
 *	- there are 3 ways to add data:
 *		1) simple call AddData(...)
 *		2) lock MAC's buffer, copy into it, and unlock (LockBuffer(...) / UnlockBuffer(...))
 *		3) from an I/O source (AddDataFromInputSource(...))
 *********************************************************************************************}

 //////////////////////////////////////////////////////////////////////////////////////////////
 // AddData(...) - adds data to the encoder
 //
 // Parameters:
 //	unsigned char * pData
 //		a pointer to a buffer containing the raw audio data
 //	int nBytes
 //		the number of bytes in the buffer
 //////////////////////////////////////////////////////////////////////////////////////////////
   //virtual int AddData(unsigned char * pData, int nBytes) = 0;
    function AddData(
      pData: Pointer;
      nBytes: Integer
      ): LongInt;


 //////////////////////////////////////////////////////////////////////////////////////////////
 // GetBufferBytesAvailable(...) - returns the number of bytes available in the buffer
 //	(helpful when locking)
 //////////////////////////////////////////////////////////////////////////////////////////////
   //virtual int GetBufferBytesAvailable() = 0;
    function GetBufferBytesAvailable(
      ): LongInt; stdcall;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // LockBuffer(...) - locks MAC's buffer so we can copy into it
 //
 // Parameters:
 //	int * pBytesAvailable
 //		returns the number of bytes available in the buffer (DO NOT COPY MORE THAN THIS IN)
 //
 // Return:
 //	pointer to the buffer (add at that location)
 //////////////////////////////////////////////////////////////////////////////////////////////
   //virtual unsigned char * LockBuffer(int * pBytesAvailable) = 0;
    function LockBuffer(
      var pBytesAvailable: Integer
      ): Pointer;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // UnlockBuffer(...) - releases the buffer
 //
 // Parameters:
 //	int nBytesAdded
 //		the number of bytes copied into the buffer
 //	BOOL bProcess
 //		whether MAC should process as much as possible of the buffer
 //////////////////////////////////////////////////////////////////////////////////////////////
   //virtual int UnlockBuffer(int nBytesAdded, BOOL bProcess = TRUE) = 0;
    function UnlockBuffer(
      nBytesAdded: Integer;
      bProcess: Boolean = True
      ): LongInt;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // AddDataFromInputSource(...) - use a CInputSource (input source) to add data
 //
 // Parameters:
 //	CInputSource * pInputSource
 //		a pointer to the input source
 //	int nMaxBytes
 //		the maximum number of bytes to let MAC add (-1 if MAC can add any amount)
 //	int * pBytesAdded
 //		returns the number of bytes added from the I/O source
 //////////////////////////////////////////////////////////////////////////////////////////////
   //virtual int AddDataFromInputSource(CInputSource * pInputSource, int nMaxBytes = -1, int * pBytesAdded = NULL) = 0;

 {*********************************************************************************************
 * Finish / Kill
 *********************************************************************************************}

 //////////////////////////////////////////////////////////////////////////////////////////////
 // Finish(...) - ends encoding and finalizes the file
 //
 // Parameters:
 //	unsigned char * pTerminatingData
 //		a pointer to a buffer containing the information to place at the end of the APE file
 //		(comprised of the WAV terminating data (data after the data block in the WAV) followed
 //		by any tag information)
 //	int nTerminatingBytes
 //		number of bytes in the terminating data buffer
 //	int nWAVTerminatingBytes
 //		the number of bytes of the terminating data buffer that should be appended to a decoded
 //		WAV file (it's basically nTerminatingBytes - the bytes that make up the tag)
 //////////////////////////////////////////////////////////////////////////////////////////////
    //virtual int Finish(unsigned char * pTerminatingData, int nTerminatingBytes, int nWAVTerminatingBytes) = 0;
    function Finish(
      pTerminatingData: Pointer;
      nTerminatingBytes: Integer;
      nWAVTerminatingBytes: Integer
      ): LongInt;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // Kill(...) - stops encoding and deletes the output file
 // --- NOT CURRENTLY IMPLEMENTED ---
 //////////////////////////////////////////////////////////////////////////////////////////////
    //virtual int Kill() = 0;
    function Kill(
      ): LongInt;
  published
    property Handle: APE_DECOMPRESS_HANDLE read FHandle;
  end;

{*************************************************************************************************
TAPEDecompress - class for working with existing APE files (decoding, seeking, analyzing, etc.)
*************************************************************************************************}
  TAPEDecompress = class
  private
    FHandle: APE_DECOMPRESS_HANDLE;
    FFilename: string;
  public
    ErrorCode: Integer;
    constructor Create(Filename: string);
    destructor Destroy; override;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // GetData(...) - gets raw decompressed audio
 //
 // Parameters:
 //	char * pBuffer
 //		a pointer to a buffer to put the data into
 //	int nBlocks
 //		the number of audio blocks desired (see note at intro about blocks vs. samples)
 //	int * pBlocksRetrieved
 //		the number of blocks actually retrieved (could be less at end of file or on critical failure)
 //////////////////////////////////////////////////////////////////////////////////////////////
    //virtual int GetData(char * pBuffer, int nBlocks, int * pBlocksRetrieved) = 0;
    function GetData(pBuffer: Pointer; nBlocks: Integer; var pBlocksRetrieved: Integer): LongInt;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // Seek(...) - seeks
 //
 // Parameters:
 //	int nBlockOffset
 //		the block to seek to (see note at intro about blocks vs. samples)
 //////////////////////////////////////////////////////////////////////////////////////////////
    //virtual int Seek(int nBlockOffset) = 0;
    function Seek(nBlockOffset: Integer): LongInt;

 //////////////////////////////////////////////////////////////////////////////////////////////
 // GetInfo(...) - get information about the APE file or the state of the decompressor
 //
 // Parameters:
 //	APE_DECOMPRESS_FIELDS Field
 //		the field we're querying (see APE_DECOMPRESS_FIELDS above for more info)
 //	int nParam1
 //		generic parameter... usage is listed in APE_DECOMPRESS_FIELDS
 //	int nParam2
 //		generic parameter... usage is listed in APE_DECOMPRESS_FIELDS
 //////////////////////////////////////////////////////////////////////////////////////////////
    //virtual int GetInfo(APE_DECOMPRESS_FIELDS Field, int nParam1 = 0, int nParam2 = 0) = 0;
    function GetInfo(Field: APE_DECOMPRESS_FIELDS; nParam1: Integer = 0; nParam2: Integer = 0): LongInt;

    function AverageBitrate: LongWord;
    function CurrentBitrate: LongWord;
    function CurrentBlock: LongWord;
    function CurrentMS: LongWord;
    function LengthMS: LongWord;
    function TotalBlocks: LongWord;
    function InfoApeTotalBytes: LongInt;
    function InfoAverageBitrage: LongInt;
    function InfoBitsPerSample: LongInt;
    function InfoBlockAlign: LongInt;
    function InfoBlocksPerFrame: LongInt;
    function InfoBytesPerSample: LongInt;
    function InfoChannels: LongInt;
    function InfoCompressionLevel: LongInt;
    function InfoDecompressedBitrate: LongInt;
    function InfoFileVersion: LongInt;
    function InfoFinalFrameBlocks: LongInt;
    function InfoFormatFlags: LongInt;
    function InfoFrameBitrate(FrameIndex: Integer): LongInt;
    function InfoFrameBlocks(FrameIndex: Integer): LongInt;
    function InfoFrameBytes(FrameIndex: Integer): LongInt;
    function InfoIOSource: LongInt;
    function InfoLengthMS: LongInt;
    function InfoPeakLevel: LongInt;
    function InfoSampleRate: LongInt;
    function InfoSeekBit(FrameIndex: Integer): LongInt;
    function InfoSeekByte(FrameIndex: Integer): LongInt;
    function InfoTag: Pointer;
    function InfoTerminatingData(Buffer: Pointer;
      MaxBytes: Integer): LongInt;
    function InfoTotalBlocks: LongInt;
    function InfoTotalFrames: LongInt;
    function InfoWavDataBytes: LongInt;
    function InfoWaveFormatEx(var WaveFormatEx: TWaveFormatEx): LongInt;
    function InfoWavHeaderBytes: LongInt;
    function InfoWavHeaderData(Buffer: Pointer;
      MaxBytes: Integer): LongInt;
    function InfoWavTerminatingBytes: LongInt;
    function InfoWavTotalBytes: LongInt;
  published
    property Handle: APE_DECOMPRESS_HANDLE read FHandle;
    property Filename: string read FFilename;
  end;

implementation

function macErrorExplanation(MACErrorCode: LongInt): string;
begin
  case MACErrorCode of
    0: Result := 'OK';
    MAC_ERROR_IO_READ: Result := 'I/O read error';
    MAC_ERROR_IO_WRITE: Result := 'I/O write error';
    MAC_ERROR_INVALID_INPUT_FILE: Result := 'invalid input file';
    MAC_ERROR_INVALID_OUTPUT_FILE: Result := 'invalid output file';
    MAC_ERROR_INPUT_FILE_TOO_LARGE: Result := 'input file file too large';
    MAC_ERROR_INPUT_FILE_UNSUPPORTED_BIT_DEPTH: Result := 'input file unsupported bit depth';
    MAC_ERROR_INPUT_FILE_UNSUPPORTED_SAMPLE_RATE: Result := 'input file unsupported sample rate';
    MAC_ERROR_INPUT_FILE_UNSUPPORTED_CHANNEL_COUNT: Result := 'input file unsupported channel count';
    MAC_ERROR_INPUT_FILE_TOO_SMALL: Result := 'input file too small';
    MAC_ERROR_INVALID_CHECKSUM: Result := 'invalid checksum';
    MAC_ERROR_DECOMPRESSING_FRAME: Result := 'decompressing frame';
    MAC_ERROR_INITIALIZING_UNMAC: Result := 'initializing unmac';
    MAC_ERROR_INVALID_FUNCTION_PARAMETER: Result := 'invalid function parameter';
    MAC_ERROR_UNSUPPORTED_FILE_TYPE: Result := 'unsupported file type';
    MAC_ERROR_INSUFFICIENT_MEMORY: Result := 'insufficient memory';
    MAC_ERROR_LOADINGAPE_DLL: Result := 'loading MAC.dll';
    MAC_ERROR_LOADINGAPE_INFO_DLL: Result := 'loading MACinfo.dll';
    MAC_ERROR_LOADING_UNMAC_DLL: Result := 'loading UnMAC.dll';
    MAC_ERROR_USER_STOPPED_PROCESSING: Result := 'user stopped processing';
    MAC_ERROR_SKIPPED: Result := 'skipped...';
    MAC_ERROR_BAD_PARAMETER: Result := 'bad parameter';
    MAC_ERROR_APE_COMPRESS_TOO_MUCH_DATA: Result := 'APE compress too much data';
  else
    Result := 'undefined (' + inttostr(MACErrorCode) + ')'; //MAC_ERROR_UNDEFINED
  end;
end;

function macMSToTime(MilliSeconds: LongInt): Double;
const
  MS_PER_DAY = 24 * 60 * 60 * 1000;
begin
  Result := MilliSeconds / MS_PER_DAY;
end;

{ EAPEException }

constructor EAPEException.Create(MacErrorCode: Integer);
begin
  inherited Create('MACDll error: '+macErrorExplanation(MacErrorCode));
end;

////////////////////////////////////////////////////////////////////////////////
// TAPECompress wrapper(s)
////////////////////////////////////////////////////////////////////////////////

{int __stdcall c_APECompress_AddData(
APE_COMPRESS_HANDLE hAPECompress, unsigned char * pData, int nBytes);}

type

  c_APECompress_AddData_t = function(
  hAPECompress: APE_COMPRESS_HANDLE;
  pData: Pointer;
  nBytes: Integer
  ): LongInt; stdcall;

// c_APECompress_Create; Index 14;	Information not available
{APE_COMPRESS_HANDLE __stdcall c_APECompress_Create(
int * pErrorCode = NULL);}

  c_APECompress_Create_t = function(
  var pErrorCode: Integer
  ): APE_COMPRESS_HANDLE; stdcall;

// c_APECompress_Destroy; Index 15;	Information not available
{void __stdcall c_APECompress_Destroy(
APE_COMPRESS_HANDLE hAPECompress);}

  c_APECompress_Destroy_t = procedure(
  hAPECompress: APE_COMPRESS_HANDLE
  ); stdcall;

// c_APECompress_Finish; Index 16;	Information not available
{int __stdcall c_APECompress_Finish(
APE_COMPRESS_HANDLE hAPECompress,
unsigned char * pTerminatingData,
int nTerminatingBytes,
int nWAVTerminatingBytes);}

  c_APECompress_Finish_t = function(
  hAPECompress: APE_COMPRESS_HANDLE;
  pTerminatingData: Pointer;
  nTerminatingBytes: Integer;
  nWAVTerminatingBytes: Integer
  ): LongInt; stdcall;

// c_APECompress_GetBufferBytesAvailable; Index 17;	Information not available
{int __stdcall c_APECompress_GetBufferBytesAvailable(
APE_COMPRESS_HANDLE hAPECompress);}

  c_APECompress_GetBufferBytesAvailable_t = function(
  hAPECompress: APE_COMPRESS_HANDLE
  ): LongInt; stdcall;

// c_APECompress_Kill; Index 18;	Information not available
{int __stdcall c_APECompress_Kill(APE_COMPRESS_HANDLE hAPECompress);}

  c_APECompress_Kill_t = function(
  hAPECompress: APE_COMPRESS_HANDLE
  ): LongInt; stdcall;

// c_APECompress_LockBuffer; Index 19;	Information not available
{unsigned char * __stdcall c_APECompress_LockBuffer(
APE_COMPRESS_HANDLE hAPECompress, int * pBytesAvailable);}

  c_APECompress_LockBuffer_t = function(
  hAPECompress: APE_COMPRESS_HANDLE;
  var BytesAvailable: Integer
  ): Pointer; stdcall;

// c_APECompress_Start; Index 20;	Information not available
{int __stdcall c_APECompress_Start(
APE_COMPRESS_HANDLE hAPECompress,
const char * pOutputFilename,
const WAVEFORMATEX * pwfeInput,
int nMaxAudioBytes = MAX_AUDIO_BYTES_UNKNOWN,
int nCompressionLevel = COMPRESSION_LEVEL_NORMAL,
const unsigned char * pHeaderData = NULL,
int nHeaderBytes = CREATE_WAV_HEADER_ON_DECOMPRESSION);}

  c_APECompress_Start_t = function(
  hAPECompress: APE_COMPRESS_HANDLE;
  pOutputFilename: PChar;
  pwfeInput: PWaveFormatEx;
  nMaxAudioBytes: Integer = MAX_AUDIO_BYTES_UNKNOWN;
  nCompressionLevel: Integer = COMPRESSION_LEVEL_NORMAL;
  pHeaderData: Pointer = nil;
  nHeaderBytes: Integer = CREATE_WAV_HEADER_ON_DECOMPRESSION
  ): LongInt; stdcall;

// c_APECompress_UnlockBuffer; Index 21;	Information not available
{int __stdcall c_APECompress_UnlockBuffer(
APE_COMPRESS_HANDLE hAPECompress,
int nBytesAdded,
BOOL bProcess = TRUE);}

  c_APECompress_UnlockBuffer_t = function(
  hAPECompress: APE_COMPRESS_HANDLE;
  nBytesAdded: Integer;
  bProcess: Boolean = True
  ): LongInt; stdcall;

var

  c_APECompress_AddData : c_APECompress_AddData_t;
  c_APECompress_Create : c_APECompress_Create_t;
  c_APECompress_Destroy : c_APECompress_Destroy_t;
  c_APECompress_Finish : c_APECompress_Finish_t;
  c_APECompress_GetBufferBytesAvailable : c_APECompress_GetBufferBytesAvailable_t;
  c_APECompress_Kill : c_APECompress_Kill_t;
  c_APECompress_LockBuffer : c_APECompress_LockBuffer_t;
  c_APECompress_Start : c_APECompress_Start_t;
  c_APECompress_UnlockBuffer : c_APECompress_UnlockBuffer_t;


////////////////////////////////////////////////////////////////////////////////
// TAPECompress
////////////////////////////////////////////////////////////////////////////////

function TAPECompress.AddData(pData: Pointer; nBytes: Integer): LongInt;
begin
  Result := c_APECompress_AddData(FHandle, pData, nBytes);
end;

function TAPECompress.Kill: LongInt;
begin
  Result := c_APECompress_Kill(FHandle);
end;

function TAPECompress.Start(pOutputFilename: PChar;
  pwfeInput: PWaveFormatEx; nMaxAudioBytes, nCompressionLevel: Integer;
  pHeaderData: Pointer; nHeaderBytes: Integer): LongInt;
begin
  Result := c_APECompress_Start(FHandle, pOutputFilename,
    pwfeInput, nMaxAudioBytes, nCompressionLevel,
    pHeaderData, nHeaderBytes);
end;

constructor TAPECompress.Create;
begin
  FHandle := c_APECompress_Create(ErrorCode);
  if ErrorCode<>0 then
    raise EAPEException.Create(ErrorCode);
end;

destructor TAPECompress.Destroy;
begin
  if FHandle<>0 then
    c_APECompress_Destroy(FHandle);
  inherited;
end;

function TAPECompress.Finish(pTerminatingData: Pointer; nTerminatingBytes,
  nWAVTerminatingBytes: Integer): LongInt;
begin
  Result := c_APECompress_Finish(FHandle, pTerminatingData,
    nTerminatingBytes, nWAVTerminatingBytes);
end;

function TAPECompress.GetBufferBytesAvailable: LongInt;
begin
  Result := c_APECompress_GetBufferBytesAvailable(FHandle);
end;

function TAPECompress.LockBuffer(var pBytesAvailable: Integer): Pointer;
begin
  Result := c_APECompress_LockBuffer(FHandle, pBytesAvailable);
end;

function TAPECompress.UnlockBuffer(nBytesAdded: Integer;
  bProcess: Boolean): LongInt;
begin
  Result := c_APECompress_UnlockBuffer(FHandle, nBytesAdded);
end;

////////////////////////////////////////////////////////////////////////////////
// TAPEDecompress wrapper(s)
////////////////////////////////////////////////////////////////////////////////

// c_APEDecompress_Create; Index 22;	Information not available

type
  c_APEDecompress_Create_t = function(
  pFilename: PChar;
  var pErrorCode: Integer
  ): APE_DECOMPRESS_HANDLE; stdcall;

// c_APEDecompress_Destroy; Index 23;	Information not available

  c_APEDecompress_Destroy_t = procedure(
  hAPEDecompress: APE_DECOMPRESS_HANDLE
  ); stdcall;

// c_APEDecompress_GetData; Index 24;	Information not available
{ __declspec( dllexport ) int __stdcall c_APEDecompress_GetData(
APE_DECOMPRESS_HANDLE hAPEDecompress, char * pBuffer, int nBlocks, int * pBlocksRetrieved);}

  c_APEDecompress_GetData_t = function(
  hAPEDecompress: APE_DECOMPRESS_HANDLE;
  pBuffer: Pointer;
  nBlocks: Integer;
  var pBlocksRetrieved: Integer
  ): LongInt; stdcall;

// c_APEDecompress_GetInfo; Index 25;	Information not available

  c_APEDecompress_GetInfo_t = function(
  hAPEDecompress: APE_DECOMPRESS_HANDLE;
  Field: APE_DECOMPRESS_FIELDS;
  nParam1: Integer = 0;
  nParam2: Integer = 0
  ): LongInt; stdcall;

// c_APEDecompress_Seek; Index 26;	Information not available
{__declspec(dllexport)int __stdcall c_APEDecompress_Seek(
APE_DECOMPRESS_HANDLE hAPEDecompress, int nBlockOffset);}

  c_APEDecompress_Seek_t = function(
  hAPEDecompress: APE_DECOMPRESS_HANDLE;
  nBlockOffset: Integer
  ): LongInt; stdcall;

var

  c_APEDecompress_Create : c_APEDecompress_Create_t;
  c_APEDecompress_Destroy : c_APEDecompress_Destroy_t;
  c_APEDecompress_GetData : c_APEDecompress_GetData_t;
  c_APEDecompress_GetInfo : c_APEDecompress_GetInfo_t;
  c_APEDecompress_Seek : c_APEDecompress_Seek_t;

////////////////////////////////////////////////////////////////////////////////
// TAPEDecompress
////////////////////////////////////////////////////////////////////////////////

constructor TAPEDecompress.Create(Filename: string);
begin
  FFilename := Filename;
  FHandle := c_APEDecompress_Create(PChar(FFilename), ErrorCode);
  if ErrorCode<>0 then
    raise EAPEException.Create(ErrorCode);
end;

destructor TAPEDecompress.Destroy;
begin
  if FHandle<>0 then
    c_APEDecompress_Destroy(FHandle);
  inherited;
end;

function TAPEDecompress.GetData(pBuffer: Pointer; nBlocks: LongInt;
  var pBlocksRetrieved: Integer): LongInt;
begin
  Result := c_APEDecompress_GetData(FHandle, pBuffer, nBlocks, pBlocksRetrieved);
end;

function TAPEDecompress.GetInfo(Field: APE_DECOMPRESS_FIELDS; nParam1,
  nParam2: Integer): LongInt;
begin
  Result := c_APEDecompress_GetInfo(FHandle, Field, nParam1, nParam2);
end;

function TAPEDecompress.Seek(nBlockOffset: Integer): LongInt;
begin
  Result := c_APEDecompress_Seek(FHandle, nBlockOffset);
end;

// querying the information engine

function TAPEDecompress.InfoFileVersion: LongInt;
// version of the APE file * 1000 (3.93 = 3930) [ignored, ignored]
begin // APE_INFO_FILE_VERSION = 1000;
  Result := GetInfo(APE_INFO_FILE_VERSION);
end;

function TAPEDecompress.InfoCompressionLevel: LongInt;
// compression level of the APE file [ignored, ignored]
begin // APE_INFO_COMPRESSION_LEVEL = 1001;
  Result := GetInfo(APE_INFO_COMPRESSION_LEVEL);
end;

function TAPEDecompress.InfoFormatFlags: LongInt;
// format flags of the APE file [ignored, ignored]
begin // APE_INFO_FORMAT_FLAGS = 1002;
  Result := GetInfo(APE_INFO_FORMAT_FLAGS);
end;

function TAPEDecompress.InfoSampleRate: LongInt;
// sample rate (Hz) [ignored, ignored]
begin // APE_INFO_SAMPLE_RATE = 1003;
  Result := GetInfo(APE_INFO_SAMPLE_RATE);
end;

function TAPEDecompress.InfoBitsPerSample: LongInt;
// bits per sample [ignored, ignored]
begin // APE_INFO_BITS_PER_SAMPLE = 1004;
  Result := GetInfo(APE_INFO_BITS_PER_SAMPLE);
end;

function TAPEDecompress.InfoBytesPerSample: LongInt;
// number of bytes per sample [ignored, ignored]
begin // APE_INFO_BYTES_PER_SAMPLE = 1005;
  Result := GetInfo(APE_INFO_BYTES_PER_SAMPLE);
end;

function TAPEDecompress.InfoChannels: LongInt;
// channels [ignored, ignored]
begin // APE_INFO_CHANNELS = 1006;
  Result := GetInfo(APE_INFO_CHANNELS);
end;

function TAPEDecompress.InfoBlockAlign: LongInt;
// block alignment [ignored, ignored]
begin // APE_INFO_BLOCK_ALIGN = 1007;
  Result := GetInfo(APE_INFO_BLOCK_ALIGN);
end;

function TAPEDecompress.InfoBlocksPerFrame: LongInt;
// number of blocks in a frame (frames are used internally)  [ignored, ignored]
begin // APE_INFO_BLOCKS_PER_FRAME = 1008;
  Result := GetInfo(APE_INFO_BLOCKS_PER_FRAME);
end;

function TAPEDecompress.InfoFinalFrameBlocks: LongInt;
// blocks in the final frame (frames are used internally) [ignored, ignored]
begin // APE_INFO_FINAL_FRAME_BLOCKS = 1009;
  Result := GetInfo(APE_INFO_FINAL_FRAME_BLOCKS);
end;

function TAPEDecompress.InfoTotalFrames: LongInt;
begin // APE_INFO_TOTAL_FRAMES = 1010;
// total number frames (frames are used internally) [ignored, ignored]
  Result := GetInfo(APE_INFO_TOTAL_FRAMES);
end;

function TAPEDecompress.InfoWavHeaderBytes: LongInt;
// header bytes of the decompressed WAV [ignored, ignored]
begin // APE_INFO_WAV_HEADER_BYTES = 1011;
  Result := GetInfo(APE_INFO_WAV_HEADER_BYTES);
end;

function TAPEDecompress.InfoWavTerminatingBytes: LongInt;
// terminating bytes of the decompressed WAV [ignored, ignored]
begin // APE_INFO_WAV_TERMINATING_BYTES = 1012;
  Result := GetInfo(APE_INFO_WAV_TERMINATING_BYTES);
end;

function TAPEDecompress.InfoWavDataBytes: LongInt;
// data bytes of the decompressed WAV [ignored, ignored]
begin // APE_INFO_WAV_DATA_BYTES = 1013;
  Result := GetInfo(APE_INFO_WAV_DATA_BYTES);
end;

function TAPEDecompress.InfoWavTotalBytes: LongInt;
// total bytes of the decompressed WAV [ignored, ignored]
begin // APE_INFO_WAV_TOTAL_BYTES = 1014;
  Result := GetInfo(APE_INFO_WAV_TOTAL_BYTES);
end;

function TAPEDecompress.InfoApeTotalBytes: LongInt;
// total bytes of the APE file [ignored, ignored]
begin // APE_INFO_APE_TOTAL_BYTES = 1015;
  Result := GetInfo(APE_INFO_APE_TOTAL_BYTES);
end;

function TAPEDecompress.InfoTotalBlocks: LongInt;
// total blocks of audio data [ignored, ignored]
begin // APE_INFO_TOTAL_BLOCKS = 1016;
  Result := GetInfo(APE_INFO_TOTAL_BLOCKS);
end;

function TAPEDecompress.InfoLengthMS: LongInt;
// length in ms (1 sec = 1000 ms) [ignored, ignored]
begin // APE_INFO_LENGTH_MS = 1017;
  Result := GetInfo(APE_INFO_LENGTH_MS);
end;

function TAPEDecompress.InfoAverageBitrage: LongInt;
// average bitrate of the APE [ignored, ignored]
begin // APE_INFO_AVERAGE_BITRATE = 1018;
  Result := GetInfo(APE_INFO_AVERAGE_BITRATE);
end;

function TAPEDecompress.InfoFrameBitrate(FrameIndex: Integer): LongInt;
// bitrate of specified APE frame [frame index, ignored]
begin // APE_INFO_FRAME_BITRATE = 1019;
  Result := GetInfo(APE_INFO_FRAME_BITRATE, FrameIndex);
end;

function TAPEDecompress.InfoDecompressedBitrate: LongInt;
// bitrate of the decompressed WAV [ignored, ignored]
begin // APE_INFO_DECOMPRESSED_BITRATE = 1020;
  Result := GetInfo(APE_INFO_DECOMPRESSED_BITRATE);
end;

function TAPEDecompress.InfoPeakLevel: LongInt;
// peak audio level (-1 is unknown) [ignored, ignored]
begin // APE_INFO_PEAK_LEVEL = 1021;
  Result := GetInfo(APE_INFO_PEAK_LEVEL);
end;

function TAPEDecompress.InfoSeekBit(FrameIndex: Integer): LongInt;
// bit offset [frame index, ignored]
begin // APE_INFO_SEEK_BIT = 1022;
  Result := GetInfo(APE_INFO_SEEK_BIT, FrameIndex);
end;

function TAPEDecompress.InfoSeekByte(FrameIndex: Integer): LongInt;
// byte offset [frame index, ignored]
begin // APE_INFO_SEEK_BYTE = 1023;
  Result := GetInfo(APE_INFO_SEEK_BYTE, FrameIndex);
end;

function TAPEDecompress.InfoWavHeaderData(Buffer: Pointer; MaxBytes: Integer): LongInt;
// error code [buffer *, max bytes]
begin // APE_INFO_WAV_HEADER_DATA = 1024;
  Result := GetInfo(APE_INFO_WAV_HEADER_DATA, Integer(Buffer), MaxBytes);
end;

function TAPEDecompress.InfoTerminatingData(Buffer: Pointer; MaxBytes: Integer): LongInt;
// error code [buffer *, max bytes]
begin // APE_INFO_WAV_TERMINATING_DATA = 1025;
  Result := GetInfo(APE_INFO_WAV_TERMINATING_DATA, Integer(Buffer), MaxBytes);
end;

function TAPEDecompress.InfoWaveFormatEx(var WaveFormatEx: TWaveFormatEx): LongInt;
// error code [waveformatex *, ignored]
begin // APE_INFO_WAVEFORMATEX = 1026;
  Result := GetInfo(APE_INFO_WAVEFORMATEX, Integer(@WaveFormatEx));
end;

function TAPEDecompress.InfoIOSource: LongInt;
// I/O source (CIO *) [ignored, ignored]
begin // APE_INFO_IO_SOURCE = 1027;
  Result := GetInfo(APE_INFO_IO_SOURCE);
end;

function TAPEDecompress.InfoFrameBytes(FrameIndex: Integer): LongInt;
// bytes (compressed) of the frame [frame index, ignored]
begin // APE_INFO_FRAME_BYTES = 1028;
  Result := GetInfo(APE_INFO_SEEK_BYTE, FrameIndex);
end;

function TAPEDecompress.InfoFrameBlocks(FrameIndex: Integer): LongInt;
// blocks in a given frame [frame index, ignored]
begin // APE_INFO_FRAME_BLOCKS = 1029;
  Result := GetInfo(APE_INFO_FRAME_BLOCKS, FrameIndex);
end;

function TAPEDecompress.InfoTag: Pointer;
// point to tag (CAPETag *) [ignored, ignored]
begin // APE_INFO_TAG = 1030;
  Result := Pointer(GetInfo(APE_INFO_TAG));
end;

// querying the decompressor

function TAPEDecompress.CurrentBlock: LongWord;
// current block location [ignored, ignored]
begin
  Result := GetInfo(APE_DECOMPRESS_CURRENT_BLOCK);
end;

function TAPEDecompress.CurrentMS: LongWord;
// current millisecond location [ignored, ignored]
begin
  Result := GetInfo(APE_DECOMPRESS_CURRENT_MS);
end;

function TAPEDecompress.TotalBlocks: LongWord;
// total blocks in the decompressors range [ignored, ignored]
begin
  Result := GetInfo(APE_DECOMPRESS_TOTAL_BLOCKS);
end;

function TAPEDecompress.LengthMS: LongWord;
// total blocks in the decompressors range [ignored, ignored]
begin
  Result := GetInfo(APE_DECOMPRESS_LENGTH_MS);
end;

function TAPEDecompress.CurrentBitrate: LongWord;
// current bitrate [ignored, ignored]
begin
  Result := GetInfo(APE_DECOMPRESS_CURRENT_BITRATE);
end;

function TAPEDecompress.AverageBitrate: LongWord;
// average bitrate (works with ranges) [ignored, ignored]
begin
  Result := GetInfo(APE_DECOMPRESS_AVERAGE_BITRATE);
end;

var
  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(MACPath, 0, 0);
  if Libhandle <> 0 then
  begin
    MACLoaded := True;
    macProgressCallback := GetProcAddress(Libhandle, 'ProgressCallback');
    macRemoveTag := GetProcAddress(Libhandle, 'RemoveTag');
    macCompressFile := GetProcAddress(Libhandle, 'CompressFile');
    macConvertFile := GetProcAddress(Libhandle, 'ConvertFile');
    macDecompressFile := GetProcAddress(Libhandle, 'DecompressFile');
    macFillWaveFormatEx := GetProcAddress(Libhandle, 'FillWaveFormatEx');
    macFillWaveHeader := GetProcAddress(Libhandle, 'FillWaveHeader');
    macGetID3Tag := GetProcAddress(Libhandle, 'GetID3Tag');
    macGetInterfaceCompatibility := GetProcAddress(Libhandle, 'GetInterfaceCompatibility');
    macGetVersionNumber := GetProcAddress(Libhandle, 'GetVersionNumber');
    macShowFileInfoDialog := GetProcAddress(Libhandle, 'ShowFileInfoDialog');
    macTagFileSimple := GetProcAddress(Libhandle, 'TagFileSimple');
    macVerifyFile := GetProcAddress(Libhandle, 'VerifyFile');
    c_APECompress_AddData := GetProcAddress(Libhandle, 'c_APECompress_AddData');
    c_APECompress_Create := GetProcAddress(Libhandle, 'c_APECompress_Create');
    c_APECompress_Destroy := GetProcAddress(Libhandle, 'c_APECompress_Destroy');
    c_APECompress_Finish := GetProcAddress(Libhandle, 'c_APECompress_Finish');
    c_APECompress_GetBufferBytesAvailable := GetProcAddress(Libhandle, 'c_APECompress_GetBufferBytesAvailable');
    c_APECompress_Kill := GetProcAddress(Libhandle, 'c_APECompress_Kill');
    c_APECompress_LockBuffer := GetProcAddress(Libhandle, 'c_APECompress_LockBuffer');
    c_APECompress_Start := GetProcAddress(Libhandle, 'c_APECompress_Start');
    c_APECompress_UnlockBuffer := GetProcAddress(Libhandle, 'c_APECompress_UnlockBuffer');
    c_APEDecompress_Create := GetProcAddress(Libhandle, 'c_APEDecompress_Create');
    c_APEDecompress_Destroy := GetProcAddress(Libhandle, 'c_APEDecompress_Destroy');
    c_APEDecompress_GetData := GetProcAddress(Libhandle, 'c_APEDecompress_GetData');
    c_APEDecompress_GetInfo := GetProcAddress(Libhandle, 'c_APEDecompress_GetInfo');
    c_APEDecompress_Seek := GetProcAddress(Libhandle, 'c_APEDecompress_Seek');
  end;

  finalization
    if Libhandle <> 0 then FreeLibrary(Libhandle);

end.

