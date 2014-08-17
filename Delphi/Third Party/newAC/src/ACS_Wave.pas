(*
  This file is a part of New Audio Components package v 2.5
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_Wave.pas 1256 2010-11-11 11:26:34Z andrei.borovsky $ *)

unit ACS_Wave;

(* Title: ACS_Wave
    Delphi interface for WAV files copyright (c) 2002-2008, Andrei
    Borovsky (anb@symmetrica.net). All rights reserved. See the LICENSE file
    for more details. *)



interface


uses

// This is required to allow TWaveIn to read MS ACM (MP3-encoded) files.
  {$IFDEF WIN32}
  MMSYSTEM, _MSAcm, waveconverter,
  {$ENDIF}

  {$IFDEF LINUX}
  Math, MAD,
  {$ENDIF}

  Classes, SysUtils, FastMove, ACS_Types, ACS_Classes, ACS_Procs;

const

  BUF_SIZE = $4000;

type

  (* Enum: TWavType
      The format of the WAV.
    
    - wtUnsupported - a WAV format that isn't supported
    - wtPCM - a WAV which is raw PCM (the normal format, use this if you're unsure)
    - wtDVIADPCM - a WAV which is MS DVI IMA ADPCM
    - wtACM - an MP3 packed inside a WAV
    - wtIEEEFloat - floating point encoding (32 or 64 bits)
    - wtExtPCM - raw PCM encoding with WaveFormatExtensible header.
    - wtExtIEEEFloat - floating point encoding with WaveFormatExtensible header.
  *)

  TWavType = (wtUnsupported, wtPCM, wtDVIADPCM, wtMSADPCM, wtACM, wtIEEEFloat, wtExtPCM, wtExtIEEEFloat);

  (* Record: TWaveHeader
      Represents a RIFF file header.

    Properties:    

    RIFF: array [0..3] of Char - 'RIFF'
    FileSize: Integer - FileSize - 8
    RIFFType: array [0..3] of Char - 'WAVE'
    FmtChunkId: array [0..3] of Char - 'fmt' marks the beginning of the format chunk
    FmtChunkSize: Integer - 16 the size of the format chunk
    FormatTag: Word - One of WAVE_FORMAT_XXX constants
    Channels: Word - 1=mono, 2=stereo
    SampleRate: Integer; - sample rate
    BytesPerSecond: Integer; - bytes per second
    BlockAlign: Word; - block alignment?
    BitsPerSample: Word - 8, 16 or 32 Bits/sample
    DataChunkId: array [0..3] of Char - 'data' marks the beginning of the data chunk
    DataSize: Integer - Data size in bytes  
  *)

  TWaveHeader = record
    // RIFF file header
    RIFF: array [0..3] of AnsiChar;          // = 'RIFF' offset : 0000
    FileSize: Integer;                   // = FileSize - 8 offset : 0004
    RIFFType: array [0..3] of AnsiChar;      // = 'WAVE'  offset : 0008
    // Format chunk
    FmtChunkId: array [0..3] of AnsiChar;    // = 'fmt'   offset : 0012
    FmtChunkSize: Integer;               // = 16      offset : 0016
    FormatTag: Word;                     // One of WAVE_FORMAT_XXX constants    offset : 0020
    Channels: Word;                      // = 1 - mono = 2 - stereo             offset : 0022
    SampleRate: Integer;                                                     // offset : 0024
    BytesPerSecond: Integer;                                                 // offset : 0028
    BlockAlign: Word;                                                        // offset : 0032
    BitsPerSample: Word;                 // = 8, 16 or 32 Bits/sample           offset : 0034
    // Data Chunk
    DataChunkId: array [0..3] of AnsiChar;   // = 'data'                          // offset : 0036
    DataSize: Integer;   // Data size in bytes                                // offset : 0040
  end;
(*
  Record: TWaveHeaderExt
    Represents a WaveFormatExtensible header.

  Properties:
    RIFF: array [0..3] of Char - 'RIFF'
    FileSize: Integer - FileSize - 8
    RIFFType: array [0..3] of Char - 'WAVE'
    FmtChunkId: array [0..3] of Char - 'fmt' marks the beginning of the format chunk
    FmtChunkSize: Integer - 16, the size of the format chunk
    Format: <TWaveFormatExtensible> - see <TWaveFormatExtensible>
    DataChunkId: array [0..3] of Char; - 'data' marks the beginning of the data chunk
    DataSize: Integer; - Data size in bytes
*)


  TWaveHeaderExt = packed record
    // RIFF file header
    RIFF: array [0..3] of AnsiChar;          // = 'RIFF'
    FileSize: Integer;                   // = FileSize - 8
    RIFFType: array [0..3] of AnsiChar;      // = 'WAVE'
    // Format chunk
    FmtChunkId: array [0..3] of AnsiChar;    // = 'fmt'
    FmtChunkSize: Integer;               // = 16
    Format : TWaveFormatExtensible;
    // Data Chunk
    DataChunkId: array [0..3] of AnsiChar;   // = 'data'
    DataSize: Integer;   // Data size in bytes
  end;


(* Record: TDVIADPCMHeader
    RIFF file header for DVIADPCM (version NNFCAIWFLDL).

  Properties:
    RIFF: array [0..3] of Char - 'RIFF' begins RIFF file header
    FileSize: Integer - FileSize - 8
    RIFFType: array [0..3] of Char - 'WAVE'
    FmtChunkId: array [0..3] of Char - 'fmt' Format chunk
    FmtChunkSize: Integer - 20
    FormatTag: Word - WAVE_FORMAT_DVI_ADPCM
    Channels: Word - 1=mono, 2=stereo
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BlockAlign: Word;
    BitsPerSample: Word - 3, 4 bits/sample
    cbSize: Word - The size in bytes of the extra information
    SamplesPerBlock: Word - number of samples per channel per Block
    FactChunkId: array [0..3] of Char - 'fact' begin Fact Chunk
    FactChunkSize : Integer - 4
    DataLength : Integer;
    DataChunkId: array [0..3] of Char - 'data' begin Data Chunk
    DataSize: Integer -  Data size in bytes
  *)

  TDVIADPCMHeader = record
    // RIFF file header
    RIFF: array [0..3] of AnsiChar;          // = 'RIFF'
    FileSize: Integer;                   // = FileSize - 8
    RIFFType: array [0..3] of AnsiChar;      // = 'WAVE'
    // Format chunk
    FmtChunkId: array [0..3] of AnsiChar;    // = 'fmt'
    FmtChunkSize: Integer;               // = 20
    FormatTag: Word;                     // WAVE_FORMAT_DVI_ADPCM
    Channels: Word;                      // = 1 - mono = 2 - stereo
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BlockAlign: Word;
    BitsPerSample: Word;                // = 3, 4 Bits/sample
    cbSize : Word;                      // The size in bytes of the extra information
    SamplesPerBlock : Word;             // number of samples per channel per Block
    // Fact Chunk
    FactChunkId: array [0..3] of AnsiChar;  // = 'fact'
    FactChunkSize : Integer;            // = 4
    DataLength : Integer;
    // Data Chunk
    DataChunkId: array [0..3] of AnsiChar;   // = 'data'
    DataSize: Integer;   // Data size in bytes
  end;


  TDVI_ADPCM_INFO = record
    BlockLength : Word;
    SamplesPerBlock : Word;
    DataSize : Integer;
  end;

  TDVI_ADPCM_STATE_STEREO = packed record
    valprev_l : SmallInt;      // Previous output value
    index_l : Byte;            // Index into stepsize table
    valprev_r : SmallInt;      // Previous output value
    index_r : Byte;            // Index into stepsize table
  end;

  TDVI_ADPCM_ENCODE_STATE_STEREO = packed record
    PredSamp_l : SmallInt;
    Index_l : Byte;
    PredSamp_r : SmallInt;
    Index_r : Byte;
  end;

  TMS_ADPCM_COEF_SET = packed record
    Coef1, Coef2 : SmallInt;
  end;

  TMS_ADPCM_INFO = record
    BlockLength : Word;
    SamplesPerBlock : Word;
    DataSize : LongWord;
    NumCoeff : Word;
    CoefSets : array[0..31] of TMS_ADPCM_COEF_SET; // Is that enough?
  end;

  TMSADPCMBlockHeaderMono = packed record
    predictor : Byte;
    Delta : SmallInt;
    Samp1 : SmallInt;
    Samp2 : SmallInt;
  end;

  TMSADPCMBlockHeaderStereo = packed record
    predictor : array[0..1] of Byte;
    Delta : array[0..1] of SmallInt;
    Samp1 : array[0..1] of SmallInt;
    Samp2 : array[0..1] of SmallInt;
  end;

  (* Class: TWaveIn
     Wave file decoder.
     Descends from <TAuFileIn>.*)

  TWaveIn = class(TAuFileIn)
  private
    buf : array[1..BUF_SIZE] of Byte;
    _WavType : TWavType;
    DVI_ADPCM_INFO : TDVI_ADPCM_INFO;
    DVI_ADPCM_STATE : TDVI_ADPCM_STATE_STEREO;
    MS_ADPCM_INFO : TMS_ADPCM_INFO;
    MS_ADPCM_STATE : TMSADPCMBlockHeaderStereo;
    HeaderSize : Word;
    _MS : TMemoryStream;
    OldStream : TStream;
    OldStreamAssigned : Boolean;
    ShortIEEEFloat : Boolean;
    {$IFDEF LINUX}
    // MS ACM stuff
    HasFirstFrame : Boolean;
    InputDone : Boolean;
    Data : Pointer;
    DataLen : Integer;
    {$ENDIF}
    function ReadDVIADPCMBlock(Data : Pointer) : Boolean;
    function ReadMSADPCMBlock(Data : Pointer) : Boolean;
    function GetWavType : TWavType;
    procedure ReadRIFFHeader;
    procedure DecodeDVIADPCMMono(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
    procedure DecodeDVIADPCMStereo(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
    procedure DecodeMSADPCMMono(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
    procedure DecodeMSADPCMStereo(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    (* Property: WavType
      This <TWavType> property indicates the current .wav file encoding. 

    *)
    property WavType : TWavType read GetWavType;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EndSample;
    property StartSample;
  end;

  (* Class: TWaveOut
     Wave file encoder. Descends from <TAuFileOut>.*)

  TWaveOut = class(TAuFileOut)
  private
    EndOfInput : Boolean;
//    Buffer : array [0..BUF_SIZE-1] of Byte;
    FWavType : TWavType;
    FEncodeState : TDVI_ADPCM_ENCODE_STATE_STEREO;
    FPrevSR : LongWord;
    FPrevCh : Word;
    FPrevBPS : Word;
    FPrevLen : Integer;
    FLenOffs : Integer;
    FPrevDataSize : Integer;
    FDataSizeOffs : Integer;
    FPrevWavType : TWavType;
    HeaderSize : Integer;
    FBlockAlign : Word;
    FNonMsHeaders : Boolean;
    procedure SetWavType(WT : TWavType);
    procedure ReadRIFFHeader;
    procedure FillHeaderPCM(var Header : TWaveHeader);
    procedure FillHeaderExtPCM(var Header : TWaveHeaderExt);
    procedure FillHeaderDVIADPCM(var Header : TDVIADPCMHeader);
    procedure SetBlockSize(BS : Word);
    procedure EncodeDVIADPCMMono(InData : PBuffer16; OutData : PBuffer8);
    procedure EncodeDVIADPCMStereo(InData : PBuffer16; OutData : PBuffer8);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure SetFileMode(aMode : TFileOutputMode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: WavType
      Use this <TWavType> property to specify output .wav file encoding. When
      you append data to an existing file (with data in either raw PCM or MS
      DVI IMA ADPCM encoding) this property will be automatically set to the
      file encoding. Only wtPCM, wtExtPCM, and wtDVIADPCM formats are supported for encoding.
      Do not set wtExtPCM directly. This format is chosen automatically if you encode audio with more than 24 bits per sample or more than 2 channels and <CreateNonMsHeaders> is set to False. *)
    property WavType : TWavType read FWavType write SetWavType;
    (*Property: BlockSize
      Use this property to set the size of the DVI IMA ADPCM block in bytes
      (when using DVI IMA ADPCM encoding). The size of the block must be a
      multiple of four. Since all the blocks in the file must be the same
      size, the size of the block will be set automatically when appending
      data to the existing MS DVI IMA ADPCM encoded file.*)
    property BlockSize : Word read FBlockAlign write SetBlockSize;
    (* Property: CreateNonMsHeaders
      Use this property to specify the headers format for output files with more than 16 bits per sample and more than 2 channels.
      Microsoft uses its own headers format for these files and this format is the only one supported by Windows Media Player 9 (although later versions of the player support both types of headers).
      WinAmp and many other programs can also play both formats, but some programs such as Sound Forge and Reaper only understand conventional headers.
      The default value for this property is True which makes the component produce files non-readable by WM Player 9, but readable by most other programs (including later versions of Windows Media Player) out there .
      Set it to False if you want to generate files with MS-specific headers. *)
    property CreateNonMsHeaders : Boolean read FNonMsHeaders write FNonMsHeaders;
    property FileMode;
  end;

    (* Class: TWaveTap
      Descends from <TAudioTap>.
      This is one of the "audio tap" components that sit between input and
      output in the audio chain and optionally record the audio data passing
      through them into a file. This component records data into wav file
      using PCM encoding. *)

  TWaveTap = class(TAudioTap)
  private
    FStream : TAuFileStream;
    BytesCounter : LongWord;
    FShareMode : Word;
    procedure FillHeaderPCM(var Header : TWaveHeader);
  protected
    procedure StartRecordInternal; override;
    procedure StopRecordInternal; override;
    procedure WriteDataInternal(Buffer : Pointer; BufferLength : LongWord); override;
  end;

implementation

const

  WaveHeaderOffs = 44;
  DataSizeOffs = 40;
  WAVE_FORMAT_PCM = 1;
  WAVE_FORMAT_ADPCM = 2;
  WAVE_FORMAT_IEEE_FLOAT = 3;
  WAVE_FORMAT_ALAW = 6;
  WAVE_FORMAT_MULAW = 7;
  WAVE_FORMAT_DVI_IMA_ADPCM = 17;
  WAVE_FORMAT_IMA_ADPCM = 17;
  WAVE_FORMAT_MP3 = 85;

type

  TDVIADPCMBlockHeader = packed record
    Samp0 : SmallInt;
    StepTableIndex : Byte;
    Reserved : Byte;
  end;

const

  // DVI IMA ADPCM stuff

  StepTab : array[0..88] of Integer = (
                    7,     8,     9,    10,    11,    12,    13,    14,
                   16,    17,    19,    21,    23,    25,    28,    31,
                   34,    37,    41,    45,    50,    55,    60,    66,
                   73,    80,    88,    97,   107,   118,   130,   143,
                  157,   173,   190,   209,   230,   253,   279,   307,
                  337,   371,   408,   449,   494,   544,   598,   658,
                  724,   796,   876,   963,  1060,  1166,  1282,  1411,
                 1552,  1707,  1878,  2066,  2272,  2499,  2749,  3024,
                 3327,  3660,  4026,  4428,  4871,  5358,  5894,  6484,
                 7132,  7845,  8630,  9493, 10442, 11487, 12635, 13899,
                15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794,
                32767 );

  IndexTab : array[0..15] of Integer = ( -1, -1, -1, -1, 2, 4, 6, 8,
                               -1, -1, -1, -1, 2, 4, 6, 8 );

  // MS ADPCM Stuff

  adaptive : array[0..15] of SmallInt =
  (
    230, 230, 230, 230, 307, 409, 512, 614,
    768, 614, 512, 409, 307, 230, 230, 230
  );



  procedure TWaveIn.DecodeDVIADPCMMono(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
  var
    i, j, SP : Integer;
    Diff, PSample : Integer;
    Code : Byte;
    Index : Integer;
 begin
    OutData[0] := DVI_ADPCM_STATE.valprev_l;
    SP := 0;
    PSample := DVI_ADPCM_STATE.valprev_l;
    Index := DVI_ADPCM_STATE.index_l;
    for i := 0 to (Len shl 1) -1 do
    begin
      j := i shr 1;
      Code := InData[j];
      if (i and 1) = 0 then
      Code := Code and 15
      else Code := Code shr 4;
      Diff := (StepTab[Index] shr 3 );
      if (Code and 4) <> 0 then
      Diff := Diff + StepTab[Index];
      if (Code and 2) <> 0 then
      Diff := Diff + (StepTab[Index] shr 1);
      if (Code and 1) <> 0 then
      Diff := Diff + (StepTab[Index] shr 2);
      if (Code and 8) <> 0 then Diff := -Diff;
      PSample := PSample + Diff;
      if PSample > 32767 then PSample := 32767;
      if PSample < -32767 then PSample := -32767;
      SP:=SP+1;
      OutData[SP] := PSample;
      Index := Index + IndexTab[Code];
      if Index > 88 then Index := 88;
      if Index < 0 then Index := 0;
    end;
    Len := SP+1;
  end;

  procedure TWaveIn.DecodeDVIADPCMStereo(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
  var
    i, j, SP : Integer;
    Diff, PSample : Integer;
    Code : Byte;
    Index : Integer;
 begin
    OutData[0] := DVI_ADPCM_STATE.valprev_l;
    SP := 0;
    PSample := DVI_ADPCM_STATE.valprev_l;
    Index := DVI_ADPCM_STATE.index_l;
    for i := 0 to (Len -1) do
    begin
      j := i shr 1;
      Code := InData[(j div 4)*8 + (j mod 4)];
      if (i and 1) = 0 then
      Code := Code and 15
      else Code := Code shr 4;
      Diff := (StepTab[Index] shr 3 );
      if (Code and 4) <> 0 then
      Diff := Diff + StepTab[Index];
      if (Code and 2) <> 0 then
      Diff := Diff + (StepTab[Index] shr 1);
      if (Code and 1) <> 0 then
      Diff := Diff + (StepTab[Index] shr 2);
      if (Code and 8) <> 0 then Diff := -Diff;
      PSample := PSample + Diff;
      if PSample > 32767 then PSample := 32767;
      if PSample < -32767 then PSample := -32767;
      SP:=SP+2;
      OutData[SP] := PSample;
      Index := Index + IndexTab[Code];
      if Index > 88 then Index := 88;
      if Index < 0 then Index := 0;
    end;
    i := 1;
    OutData[i] := DVI_ADPCM_STATE.valprev_r;
    SP := 1;
    PSample := DVI_ADPCM_STATE.valprev_r;
    Index := DVI_ADPCM_STATE.index_r;
    for i := 0 to (Len -1) do
    begin
      j := i shr 1;
      Code := InData[(j div 4)*8 + (j mod 4) + 4];
      if (i and 1) = 0 then
      Code := Code and 15
      else Code := Code shr 4;
      Diff := (StepTab[Index] shr 3 );
      if (Code and 4) <> 0 then
      Diff := Diff + StepTab[Index];
      if (Code and 2) <> 0 then
      Diff := Diff + (StepTab[Index] shr 1);
      if (Code and 1) <> 0 then
      Diff := Diff + (StepTab[Index] shr 2);
      if (Code and 8) <> 0 then Diff := -Diff;
      PSample := PSample + Diff;
      if PSample > 32767 then PSample := 32767;
      if PSample < -32767 then PSample := -32767;
      SP:=SP+2;
      OutData[SP] := PSample;
      Index := Index + IndexTab[Code];
      if Index > 88 then Index := 88;
      if Index < 0 then Index := 0;
    end;
    Len := (SP div 2)+1;
  end;

  procedure TWaveIn.DecodeMSADPCMMono(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
  var
    pos, i, PredSamp, ErrorDelta : Integer;
  begin
    pos := 0;
    OutData[pos] := MS_ADPCM_STATE.Samp2[0];
    Inc(pos);
    OutData[pos] := MS_ADPCM_STATE.Samp1[0];
    Inc(pos);
    for i := 0 to (Len shr 1) - 1 do
    begin
      PredSamp := (MS_ADPCM_STATE.Samp1[0]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[0]].Coef1 +
                   MS_ADPCM_STATE.Samp2[0]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[0]].Coef2) div 256;
      ErrorDelta := InData[i] shr 4;
      if (ErrorDelta and 8) <> 0 then
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[0]*(ErrorDelta - 16)
      else
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[0]*(ErrorDelta);
      if PredSamp > 32767 then PredSamp := 32767;
      if PredSamp < -32768 then PredSamp := -32768;
      OutData[pos] := PredSamp;
      Inc(pos);
      MS_ADPCM_STATE.Delta[0] := (MS_ADPCM_STATE.Delta[0]*adaptive[ErrorDelta]) div 256;
      if MS_ADPCM_STATE.Delta[0] < 16 then MS_ADPCM_STATE.Delta[0] := 16;
      MS_ADPCM_STATE.Samp2[0] := MS_ADPCM_STATE.Samp1[0];
      MS_ADPCM_STATE.Samp1[0] := PredSamp;

      PredSamp := (MS_ADPCM_STATE.Samp1[0]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[0]].Coef1 +
                   MS_ADPCM_STATE.Samp2[0]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[0]].Coef2) div 256;
      ErrorDelta := InData[i] and 15;
      if (ErrorDelta and 8) <> 0 then
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[0]*(ErrorDelta - 16)
      else
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[0]*(ErrorDelta);
      if PredSamp > 32767 then PredSamp := 32767;
      if PredSamp < -32768 then PredSamp := -32768;
      OutData[pos] := PredSamp;
      Inc(pos);
      MS_ADPCM_STATE.Delta[0] := (MS_ADPCM_STATE.Delta[0]*adaptive[ErrorDelta]) div 256;
      if MS_ADPCM_STATE.Delta[0] < 16 then MS_ADPCM_STATE.Delta[0] := 16;
      MS_ADPCM_STATE.Samp2[0] := MS_ADPCM_STATE.Samp1[0];
      MS_ADPCM_STATE.Samp1[0] := PredSamp;
    end;
    Len := pos*2;
  end;

  procedure TWaveIn.DecodeMSADPCMStereo(InData : PBuffer8; OutData : PBuffer16; var Len : Integer);
  var
    pos, i, PredSamp, ErrorDelta : Integer;
  begin
    pos := 0;
    OutData[pos] := MS_ADPCM_STATE.Samp2[0];
    Inc(pos);
    OutData[pos] := MS_ADPCM_STATE.Samp2[1];
    Inc(pos);
    OutData[pos] := MS_ADPCM_STATE.Samp1[0];
    Inc(pos);
    OutData[pos] := MS_ADPCM_STATE.Samp1[1];
    Inc(pos);
    for i := 0 to Len - 1 do
    begin
      PredSamp := (MS_ADPCM_STATE.Samp1[0]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[0]].Coef1 +
                   MS_ADPCM_STATE.Samp2[0]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[0]].Coef2) div 256;
      ErrorDelta := InData[i] shr 4;
      if (ErrorDelta and 8) <> 0 then
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[0]*(ErrorDelta - 16)
      else
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[0]*(ErrorDelta);
      if PredSamp > 32767 then PredSamp := 32767;
      if PredSamp < -32768 then PredSamp := -32768;
      OutData[pos] := PredSamp;
      Inc(pos);
      MS_ADPCM_STATE.Delta[0] := (MS_ADPCM_STATE.Delta[0]*adaptive[ErrorDelta]) div 256;
      if MS_ADPCM_STATE.Delta[0] < 16 then MS_ADPCM_STATE.Delta[0] := 16;
      MS_ADPCM_STATE.Samp2[0] := MS_ADPCM_STATE.Samp1[0];
      MS_ADPCM_STATE.Samp1[0] := PredSamp;

      PredSamp := (MS_ADPCM_STATE.Samp1[1]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[1]].Coef1 +
                   MS_ADPCM_STATE.Samp2[1]*MS_ADPCM_INFO.CoefSets[MS_ADPCM_STATE.predictor[1]].Coef2) div 256;
      ErrorDelta := InData[i] and 15;
      if (ErrorDelta and 8) <> 0 then
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[1]*(ErrorDelta - 16)
      else
      PredSamp := PredSamp + MS_ADPCM_STATE.Delta[1]*(ErrorDelta);
      if PredSamp > 32767 then PredSamp := 32767;
      if PredSamp < -32768 then PredSamp := -32768;
      OutData[pos] := PredSamp;
      Inc(pos);
      MS_ADPCM_STATE.Delta[1] := (MS_ADPCM_STATE.Delta[1]*adaptive[ErrorDelta]) div 256;
      if MS_ADPCM_STATE.Delta[1] < 16 then MS_ADPCM_STATE.Delta[1] := 16;
      MS_ADPCM_STATE.Samp2[1] := MS_ADPCM_STATE.Samp1[1];
      MS_ADPCM_STATE.Samp1[1] := PredSamp;
    end;
    Len := pos*2;
  end;

  function Compare4(S1, S2 : PAnsiChar) : Boolean;
  var
    i, Diff : Byte;
  begin
    Result := False;
    for i := 0 to 3 do
    begin
      Diff := Byte(S1[i]) - Byte(S2[i]);
      if not (Diff in [0, 32, 224]) then Exit;
    end;
    Result := True;
  end;

{$IFDEF LINUX}

   function InputFunc(CData : Pointer; Stream : p_mad_stream) : Integer; cdecl;
   var
     WI : TWaveIn;
   begin
     WI := TWaveIn(CData);
     if WI.InputDone then
     begin
       Result := MAD_FLOW_STOP;
       Exit;
     end;
     WI.InputDone := True;
     mad_stream_buffer(Stream, WI.Data, WI.DataLen);
     Result := MAD_FLOW_CONTINUE;
   end;

   function OutputFunc(CData : Pointer; Header : p_mad_header; pcm : p_mad_pcm) : Integer; cdecl;
   var
     WI : TWaveIn;
     WH : TWaveHeader;
     i, framesize : Integer;
     outsamples : array[0..2303] of SmallInt;
     text : array[0..4] of AnsiChar;
   begin
     WI := TWaveIn(CData);
     if not WI.HasFirstFrame then
     begin
       WI.FSR := pcm.samplerate;
       WI.FChan := pcm.channels;
       WI.FBPS := 16;
       framesize := Ceil(144*Header.bitrate/Header.samplerate);
       WI.FSize := Round(WI.DataLen/framesize*1152)*WI.FChan*2;
       WI.FValid := True;
       text := 'RIFF';
       Move(text[0], WH.RIFF[0], 4);
       WH.FileSize := WI.FSize + 44;
       text := 'WAVE';
       Move(text[0], WH.RIFFType[0], 4);
       text := 'fmt ';
       Move(text[0], WH.FmtChunkId[0], 4);
       WH.FmtChunkSize := 16;
       WH.FormatTag := 1;
       WH.Channels := WI.FChan;
       WH.SampleRate := WI.FSR;
       WH.BitsPerSample := 16;
       WH.BlockAlign := 2*WI.FChan;
       WH.BytesPerSecond := WI.FSR * WH.BlockAlign;
       text := 'data';
       Move(text[0], WH.DataChunkId[0], 4);
       WH.DataSize := WI.FSize;
       WI.FStream.Size :=WI.FSize + 44;
       WI.FStream.Seek(0, soFromBeginning);
       WI.FStream.Write(WH, 44);
       WI.HasFirstFrame := True;
     end;
     if pcm.channels = 2 then
     begin
       for i := 0 to pcm.length -1 do
       begin
         if pcm.samples[0][i] >= MAD_F_ONE then
         pcm.samples[0][i] := MAD_F_ONE - 1;
         if pcm.samples[0][i] < -MAD_F_ONE then
         pcm.samples[0][i] := -MAD_F_ONE;
         pcm.samples[0][i] := pcm.samples[0][i] shr (MAD_F_FRACBITS + 1 - 16);
         outsamples[i shl 1] := pcm.samples[0][i];
         if pcm.samples[1][i] >= MAD_F_ONE then
         pcm.samples[1][i] := MAD_F_ONE - 1;
         if pcm.samples[1][i] < -MAD_F_ONE then
         pcm.samples[1][i] := -MAD_F_ONE;
         pcm.samples[1][i] := pcm.samples[1][i] shr (MAD_F_FRACBITS + 1 - 16);
         outsamples[(i shl 1)+1] := pcm.samples[1][i];
       end;
       WI.FStream.Write(outsamples[0], pcm.length*4);
     end else
     begin
       for i := 0 to pcm.length -1 do
       begin
         if pcm.samples[0][i] >= MAD_F_ONE then
         pcm.samples[0][i] := MAD_F_ONE - 1;
         if pcm.samples[0][i] < -MAD_F_ONE then
         pcm.samples[0][i] := -MAD_F_ONE;
         pcm.samples[0][i] := pcm.samples[0][i] shr (MAD_F_FRACBITS + 1 - 16);
         outsamples[i] := pcm.samples[0][i];
       end;
       WI.FStream.Write(outsamples[0], pcm.length*2);
     end;
     Result := MAD_FLOW_CONTINUE;
   end;

   function ErrorFunc(CData : Pointer; Stream : p_mad_stream; Frame : p_mad_frame) : Integer; cdecl;
   begin
     Result := MAD_FLOW_CONTINUE;
   end;

{$ENDIF}

const
  LookingForRIFF = 0;
  LookingForWave = 1;
  LookingForFMT = 2;
  LookingForFACT = 3;
  LookingForDATA = 4;

  constructor TWaveIn.Create;
  begin
    inherited Create(AOwner);
    _WavType := wtUnsupported;
  end;

  destructor TWaveIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWaveIn.OpenFile;
  var
  {$IFDEF WIN32}
    WaveConverter: TWaveConverter;
    ValidItems: LongWord;
    Res: MMResult;
  {$ENDIF}
  {$IFDEF LINUX}
    _decoder : mad_decoder;
  {$ENDIF}
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      FValid := True;
      ShortIEEEFloat := False;
      _WavType := wtUnsupported;
      if not FStreamAssigned then
      FStream := TAuFileStream.Create(FWideFileName, fmOpenRead  or fmShareDenyWrite);
      ReadRIFFHeader;
      case Self._WavType of
        wtUnsupported :
        begin
          FValid := False;
        end;
        wtDVIADPCM :
        begin
          if FBPS <> 4 then
          FValid := False;
          FBPS := 16;
          FSize := LongWord(DVI_ADPCM_INFO.DataSize)*2*FChan;
        end;
        wtMSADPCM :
        begin
          FBPS := 16;
          FSize := MS_ADPCM_INFO.DataSize*2*FChan;
        end;
        wtIEEEFloat, wtExtIEEEFloat :
        begin
          if FBPS = 32 then
            ShortIEEEFloat := True
          else
            FSize := FSize div 2;
          FBPS := 32;
        end;
        wtACM :
        begin

{$IFDEF WIN32}

          (* Checking if the file is ACM encoded and if it is, decoding it.
             Thanks to Jan Henrik Ejme, <jan.h.ejme@xhlp.com> for the
             provided converter units. *)
            WaveConverter := TWaveConverter.Create;
          try

            FStream.Position := 0;
            WaveConverter.LoadStream(FStream);
            with WaveConverter.NewFormat.Format do
            begin
              wFormatTag := WAVE_FORMAT_PCM;
              ValidItems := ACM_FORMATSUGGESTF_WFORMATTAG;
              Res := acmFormatSuggest(nil, WaveConverter.CurrentFormat.format,
              WaveConverter.NewFormat.Format, SizeOf(TACMWaveFormat), ValidItems);
            end;
            if Res <> 0 then
            begin
              FValid := False;
              WaveConverter.Free;
              Exit;
            end;
            if WaveConverter.Convert <> 0 then
            begin
              FValid := False;
              WaveConverter.Free;
              Exit;
            end else
            begin
              _MS := TMemoryStream.Create;
              OldStream := FStream;
              OldStreamAssigned := FStreamAssigned;
              FStream := _MS;
              _MS.Position := 0;
              WaveConverter.SaveWavToStream(_MS);
              FSize := _MS.Size;
              _MS.Seek(0, soFromBeginning);
              ReadRIFFHeader;
              //WaveConverter.CurrentFormat.Format.wFormatTag;
              _wavType := wtACM;
            end;
          finally
            WaveConverter.Free;
          end;
{$ENDIF}
{$IFDEF LINUX}
          if not MADLibLoaded then
          raise EACSException.Create('Cannot play ACM file. The madlib library could not be loaded.');
          DataLen := FStream.Size;
          GetMem(Data, DataLen);
          FStream.Read(Data^, DataLen);
          _MS := TMemoryStream.Create;
          OldStream := FStream;
          OldStreamAssigned := FStreamAssigned;
          FStream := _MS;
          HasFirstFrame := False;
          InputDone := False;
          mad_decoder_init(@_decoder, Self, InputFunc, nil, nil, OutputFunc, ErrorFunc, nil);
          mad_decoder_run(@_decoder, MAD_DECODER_MODE_SYNC);
          mad_decoder_finish(@_decoder);
          FreeMem(Data);
          FStream.Seek(0, soFromBeginning);
          ReadRIFFHeader;
          _wavType := wtACM;
{$ENDIF}
        end;
      end;
      Inc(FOpened);
    end; // if FOpened = 0 then
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWaveIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      if not FStreamAssigned then FStream.Free
      else if FSeekable then FStream.Seek(0, soFromBeginning);
      if _WavType = wtACM then
      begin
        if Assigned(_MS) then
        begin
          if OldStreamAssigned then
          FStream := OldStream
          else
          begin
            FStream := nil;
            OldStream.Free;
          end;
//        _MS.Free;
          _MS := nil;
        end;
      end;
      FOpened := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWaveIn.GetDataInternal;
  var
    l, Aligned, csize : Integer;
    Data : Pointer;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      case _WavType of
        wtDVIADPCM :
        begin
          BufStart := 1;
          csize := DVI_ADPCM_INFO.BlockLength - FChan*4;
          GetMem(Data, csize);
          if ReadDVIADPCMBlock(Data) then
          begin
            if FChan = 2 then
            begin
              DecodeDVIADPCMStereo(Data, @BUF[1], csize);
              BufEnd := (csize)*4;
            end else
            begin
              DecodeDVIADPCMMono(Data, @BUF[1], csize);
              BufEnd := (csize)*2;
              FreeMem(Data);
            end;
          end else
          begin
            FreeMem(Data);
            if not Seekable then
            begin
             Bytes := 0;
             Buffer := nil;
             Exit;
            end
            else BufEnd := 0;
          end;
        end;
        wtMSADPCM :
        begin
          BufStart := 1;
          if FChan = 2 then
          begin
            csize := MS_ADPCM_INFO.BlockLength-SizeOf(TMSADPCMBlockHeaderStereo);
            GetMem(Data, csize);
            if ReadMSADPCMBlock(Data) then
            begin
              csize := MS_ADPCM_INFO.SamplesPerBlock-2;
              DecodeMSADPCMStereo(Data, @BUF[1], csize);
              BufEnd := csize;
              FreeMem(Data);
            end else BufEnd := 0;
          end else
          begin
            csize := MS_ADPCM_INFO.BlockLength-SizeOf(TMSADPCMBlockHeaderMono);
            GetMem(Data, csize);
            if ReadMSADPCMBlock(Data) then
            begin
              csize := MS_ADPCM_INFO.SamplesPerBlock-2;
              DecodeMSADPCMMono(Data, @BUF[1], csize);
              BufEnd := csize;
              FreeMem(Data);
            end else BufEnd := 0;
          end;
        end;
        wtPCM, wtExtPCM:
        begin
          BufStart := 1;
          Aligned := BUF_SIZE - (BUF_SIZE mod (FChan * FBPS shr 3));
          l := FStream.Read(Buf, Aligned);
          BufEnd := l;
        end;
        wtExtIEEEFloat, wtIEEEFloat:
        begin
          BufStart := 1;
          Aligned := BUF_SIZE - (BUF_SIZE mod (FChan * 8));
          l := FStream.Read(Buf, Aligned);
          if ShortIEEEFloat then
          begin
            ConvertShortIEEEFloatTo32(@Buf, l);
            BufEnd := l;
          end else
          begin
            ConvertIEEEFloatTo32(@Buf, l);
            BufEnd := l div 2;
          end;
        end;
        wtACM:
        begin
          BufStart := 1;
          FStream.Read(Buf, BUF_SIZE);
        end;
      end;
    end;
    Dec(Bytes, Bytes mod (FChan * FBPS shr 3));
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    Buffer := @Buf[BufStart];
    Inc(BufStart, Bytes);
  end;

  function TWaveIn.SeekInternal;
  var
    OffsSize : Int64;
  begin
    Result := True;
    BufStart := 1;
    BufEnd := 0;
    case _WavType of
      wtPCM :
      begin
        OffsSize := SampleNum*(Self.FBPS shr 3)*FChan;
        Stream.Seek(OffsSize + HeaderSize, soFromBeginning);
      end;
      wtDVIADPCM:
      begin
        OffsSize := (SampleNum div DVI_ADPCM_INFO.SamplesPerBlock)*DVI_ADPCM_INFO.BlockLength;
        Stream.Seek(OffsSize + HeaderSize, soFromBeginning);
      end;
      wtMSADPCM:
      begin
        OffsSize := (SampleNum div MS_ADPCM_INFO.SamplesPerBlock)*MS_ADPCM_INFO.BlockLength;
        Stream.Seek(OffsSize + HeaderSize, soFromBeginning);
      end;
      else
      begin
        Result := False;
      end;
    end;
  end;

procedure TWaveOut.FillHeaderPCM(var Header : TWaveHeader);
begin
  Header.RIFF := 'RIFF';
  Header.FileSize := FInput.Size + WaveHeaderOffs - 8;
  Header.RIFFType := 'WAVE';
  Header.FmtChunkId := 'fmt ';
  Header.FmtChunkSize := 16;
  Header.FormatTag := WAVE_FORMAT_PCM;
  Header.Channels := FInput.Channels;
  Header.SampleRate := FInput.SampleRate;
  Header.BitsPerSample := FInput.BitsPerSample;
  Header.BlockAlign := (Header.BitsPerSample * Header.Channels) shr 3;
  Header.BytesPerSecond := Header.SampleRate * Header.BlockAlign;
  Header.DataChunkId := 'data';
  Header.DataSize := FInput.Size;
end;

procedure TWaveOut.FillHeaderExtPCM;
begin
  Header.RIFF := 'RIFF';
  Header.FileSize := FInput.Size + SizeOf(Header) - 8;
  Header.RIFFType := 'WAVE';
  Header.FmtChunkId := 'fmt ';
  Header.FmtChunkSize := SizeOf(Header.Format);
  Header.Format.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
  Header.Format.Format.nChannels := FInput.Channels;
  Header.Format.Format.nSamplesPerSec := FInput.SampleRate;
  Header.Format.Format.wBitsPerSample := FInput.BitsPerSample;
  Header.Format.Format.nBlockAlign := (Header.Format.Format.wBitsPerSample * Header.Format.Format.nChannels) shr 3;
  Header.Format.Format.nAvgBytesPerSec := Header.Format.Format.nSamplesPerSec * Header.Format.Format.nBlockAlign;
  Header.Format.wValidBitsPerSample := Header.Format.Format.wBitsPerSample;
  if Header.Format.Format.nChannels = 1 then
    Header.Format.dwChannelMask := 1
  else  
  if Header.Format.Format.nChannels = 2 then
    Header.Format.dwChannelMask := 3
  else
    if Header.Format.Format.nChannels = 6 then
      Header.Format.dwChannelMask := $3F
    else
      if Header.Format.Format.nChannels = 8 then
        Header.Format.dwChannelMask := $FF;
  Header.Format.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
  Header.Format.Format.cbSize := 22;
  Header.DataChunkId := 'data';
  Header.DataSize := FInput.Size;
end;

procedure TWaveOut.FillHeaderDVIADPCM(var Header : TDVIADPCMHeader);
var
  text : array[0..4] of AnsiChar;
  samples : Integer;
begin
  text := 'RIFF';
  Move(text[0], Header.RIFF[0], 4);
  text := 'WAVE';
  Move(text[0], Header.RIFFType[0], 4);
  text := 'fmt ';
  Move(text[0], Header.FmtChunkId[0], 4);
  Header.FmtChunkSize := 20;
  Header.FormatTag := WAVE_FORMAT_DVI_IMA_ADPCM;
  Header.Channels := FInput.Channels;
  Header.SampleRate := FInput.SampleRate;
  Header.BitsPerSample := 4;
  Header.BlockAlign := FBlockAlign;
  Header.SamplesPerBlock := (Header.BlockAlign- 4*FInput.Channels) * (2 div FInput.Channels) + 1;
  Header.cbSize := 2;
  Header.BytesPerSecond := (FInput.SampleRate div Header.SamplesPerBlock)*Header.BlockAlign;
  samples := (FInput.Size div (FInput.BitsPerSample shr 3)) div FInput.Channels;
  Header.DataSize := Round(samples/Header.SamplesPerBlock)*Header.BlockAlign;
  Header.FileSize := Header.DataSize + SizeOf(TDVIADPCMHeader);
  text := 'data';
  Move(text[0], Header.DataChunkId[0], 4);
  text := 'fact';
  Move(text[0], Header.FactChunkId[0], 4);
  Header.FactChunkSize := 4;
  Header.DataLength := samples;
end;

procedure TWaveOut.EncodeDVIADPCMMono(InData : PBuffer16; OutData : PBuffer8);
var
  i, j, Diff, PredSamp, Index : Integer;
  Code : Byte;
  Header : TDVIADPCMBlockHeader;
begin
  FillChar(OutData[0], FBlockAlign, 0);
  Header.Samp0 := FEncodeState.PredSamp_l;
  Header.StepTableIndex := FEncodeState.Index_l;
  Move(Header, OutData[0], SizeOf(Header));
  PredSamp := FEncodeState.PredSamp_l;
  Index := FEncodeState.Index_l;
  for i := 0 to (FBlockAlign-4)*2 - 1 do
  begin
    Diff := InData[i] - PredSamp;
    if Diff < 0 then
    begin
      Code := 8;
      Diff := -Diff;
    end else Code := 0;
    if Diff >= StepTab[Index] then
    begin
      Code := Code or 4;
      Diff := Diff-StepTab[Index];
    end;
    if Diff >= (StepTab[Index] shr 1) then
    begin
      Code := Code or 2;
      Diff := Diff - (StepTab[Index] shr 1);
    end;
    if Diff >= (StepTab[Index] shr 2) then
    Code := Code or 1;
    j := (i shr 1)+4;
    if (i and 1) = 0 then
      OutData[j] := Code
    else
    OutData[j] := OutData[j] or (Code shl 4);
    Diff := (StepTab[Index] shr 3);
    if (Code and 4) <> 0 then Diff := Diff + StepTab[Index];
    if (Code and 2) <> 0 then Diff := Diff + (StepTab[Index] shr 1);
    if (Code and 1) <> 0 then Diff := Diff + (StepTab[Index] shr 2);
    if (Code and 8) <> 0 then Diff := -Diff;
    PredSamp := PredSamp + Diff;
    if PredSamp > 32767 then PredSamp := 32767;
    if PredSamp < -32767 then PredSamp := -32767;
    Index := Index + IndexTab[Code];
    if Index > 88 then Index := 88;
    if Index < 0 then Index := 0;
  end;
  FEncodeState.Index_l := Index;
//  State.PredSamp_l := PredSamp;
end;

procedure TWaveOut.EncodeDVIADPCMStereo(InData : PBuffer16; OutData : PBuffer8);
var
  i, j, Diff, bPos, PredSamp, Index : Integer;
  Code : Byte;
  Header : TDVIADPCMBlockHeader;
begin
  FillChar(OutData[0], FBlockAlign, 0);
  Header.Samp0 := FEncodeState.PredSamp_l;
  Header.StepTableIndex := FEncodeState.Index_l;
  Move(Header, OutData[0], SizeOf(Header));
  Header.Samp0 := FEncodeState.PredSamp_r;
  Header.StepTableIndex := FEncodeState.Index_r;
  i := 4;
  Move(Header, OutData[i], SizeOf(Header));
  PredSamp := FEncodeState.PredSamp_l;
  Index := FEncodeState.Index_l;
  for i := 0 to FBlockAlign - 9 do
  begin
    Diff := InData[i shl 1] - PredSamp;
    if Diff < 0 then
    begin
      Code := 8;
      Diff := -Diff;
    end else Code := 0;
    if Diff >= StepTab[Index] then
    begin
      Code := Code or 4;
      Diff := Diff-StepTab[Index];
    end;
    if Diff >= (StepTab[Index] shr 1) then
    begin
      Code := Code or 2;
      Diff := Diff - (StepTab[Index] shr 1);
    end;
    if Diff >= (StepTab[Index] shr 2) then
    Code := Code or 1;
    j := i shr 1;
    bPos := (j div 4)*8 + (j mod 4) + 8;
    if (i and 1) = 0 then
      OutData[bPos] := Code
    else
    OutData[bPos] := OutData[bPos] or (Code shl 4);
    Diff := (StepTab[Index] shr 3);
    if (Code and 4) <> 0 then Diff := Diff + StepTab[Index];
    if (Code and 2) <> 0 then Diff := Diff + (StepTab[Index] shr 1);
    if (Code and 1) <> 0 then Diff := Diff + (StepTab[Index] shr 2);
    if (Code and 8) <> 0 then Diff := -Diff;
    PredSamp := PredSamp + Diff;
    if PredSamp > 32767 then PredSamp := 32767;
    if PredSamp < -32767 then PredSamp := -32767;
    Index := Index + IndexTab[Code];
    if Index > 88 then Index := 88;
    if Index < 0 then Index := 0;
  end;
  FEncodeState.Index_l := Index;
  FEncodeState.PredSamp_l := PredSamp;
  PredSamp := FEncodeState.PredSamp_r;
  Index := FEncodeState.Index_r;
  for i := 0 to FBlockAlign - 9 do
  begin
    Diff := InData[(i shl 1)+1] - PredSamp;
    if Diff < 0 then
    begin
      Code := 8;
      Diff := -Diff;
    end else Code := 0;
    if Diff >= StepTab[Index] then
    begin
      Code := Code or 4;
      Diff := Diff-StepTab[Index];
    end;
    if Diff >= (StepTab[Index] shr 1) then
    begin
      Code := Code or 2;
      Diff := Diff - (StepTab[Index] shr 1);
    end;
    if Diff >= (StepTab[Index] shr 2) then
    Code := Code or 1;
    j := i shr 1;
    bPos := (j div 4)*8 + (j mod 4) + 12;
    if i and 1 = 0 then
    OutData[bPos] := Code
    else
    OutData[bPos] := OutData[bPos] or (Code shl 4);
    Diff := (StepTab[Index] shr 3);
    if (Code and 4) <> 0 then Diff := Diff + StepTab[Index];
    if (Code and 2) <> 0 then Diff := Diff + (StepTab[Index] shr 1);
    if (Code and 1) <> 0 then Diff := Diff + (StepTab[Index] shr 2);
    if (Code and 8) <> 0 then Diff := -Diff;
    PredSamp := PredSamp + Diff;
    if PredSamp > 32767 then PredSamp := 32767;
    if PredSamp < -32767 then PredSamp := -32767;
    Index := Index + IndexTab[Code];
    if Index > 88 then Index := 88;
    if Index < 0 then Index := 0;
  end;
   FEncodeState.Index_r := Index;
   FEncodeState.PredSamp_r := PredSamp;
end;

constructor TWaveOut.Create;
begin
  inherited Create(AOwner);
  FWavType := wtPCM;
  FBlockAlign := 512;
  FNonMsHeaders := True;
end;

destructor TWaveOut.Destroy;
begin
  inherited Destroy;
end;

procedure TWaveOut.SetWavType;
begin
  if Busy then Exit;
  if (WT = wtPCM) or (WT = wtDVIADPCM) then
  FWavType := WT;
end;

  procedure TWaveOut.Prepare;
  var
    Header : TWaveHeader;
    HeaderExt : TWaveHeaderExt;
    DVIADPCMHeader : TDVIADPCMHeader;
  begin
    EndOfInput := False;
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      if (not FileExists(FWideFileName)) or (FFileMode = foRewrite) then
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or FShareMode, FAccessMask)
      else FStream := TAuFileStream.Create(FWideFileName, fmOpenReadWrite or FShareMode, FAccessMask);
    end;
    FInput.Init;
    if (FInput.Channels > 2) or (FInput.BitsPerSample > 16) then
      if not FNonMsHeaders then
        FWavType := wtExtPCM;
    if (FFileMode = foAppend) and (FStream.Size <> 0) then
    begin
      ReadRIFFHeader;
      if not (FPrevWavType in [wtPCM, wtDVIADPCM]) then
      begin
        FInput.Flush;
        if not FStreamAssigned then
        begin
          FStream.Free;
          FStream := nil;
        end;
        raise EAuException.Create('Cannot append data to this .wav file.');
      end;
      FWavType := FPrevWavType;
    end;
    case FWavType of
      wtPCM :
      begin
        if (FFileMode = foAppend) and (FStream.Size <> 0) then
        begin
          if (FPrevSR <> FInput.SampleRate) or
             (FPrevBPS <> FInput.BitsPerSample) or
             (FPrevCh <> FInput.Channels) then
          begin
            FInput.Flush;
            if not FStreamAssigned then
            begin
              FStream.Free;
              FStream := nil;
            end;
            raise EAuException.Create('Cannot append data in different audio format.');
          end;
          FStream.Seek(0, soFromEnd);
        end else
        begin
          FillHeaderPCM(Header);
          if FStream.Write(Header, WaveHeaderOffs) <> WaveHeaderOffs then
            raise EAuException.Create('Error writing wave file');
        end;
      end;
      wtExtPCM:
      begin
        FillHeaderExtPCM(HeaderExt);
        if FStream.Write(HeaderExt, SizeOf(HeaderExt)) <> SizeOf(HeaderExt) then
          raise EAuException.Create('Error writing wave file');
       end;
      wtDVIADPCM :
      begin
        if FInput.BitsPerSample <> 16 then
        begin
          FInput.Flush;
          if not FStreamAssigned then
          begin
            FStream.Free;
            FStream := nil;
          end;
          raise EAuException.Create('Cannot encode 8 bit sound into ADPCM.');
        end;
//        FBlockAlign := 512;
        if (FFileMode = foAppend) and (FStream.Size <> 0) then
        begin
          if (FPrevSR <> FInput.SampleRate) or
             (FPrevCh <> FInput.Channels) then
          begin
            FInput.Flush;
            if not FStreamAssigned then
            begin
              FStream.Free;
              FStream := nil;
            end;
            raise EAuException.Create('Cannot append data in different audio format.');
          end;
          FStream.Seek(0, soFromEnd);
        end else
        begin
          FillHeaderDVIADPCM(DVIADPCMHeader);
          if FStream.Write(DVIADPCMHeader, SizeOf(DVIADPCMHeader)) <> SizeOf(DVIADPCMHeader) then
            raise EAuException.Create('Error writing wave file');
          FEncodeState.Index_l := 0;
          FEncodeState.Index_r := 0;
        end;
      end;
    end;
  end;

  procedure TWaveOut.Done;
  var
    Size : Integer;
    Hdr : TDVIADPCMHeader;
    BlockSize : Integer;
    P : Pointer;
  begin
    if ((FInput.Size < 0) or (FFileMode = foAppend) or (FStopped = True)) then
//    if FStream <> nil then
    begin
      case FWavType of
        wtPCM:
        begin
          if FFileMode = foAppend then
          begin
            FStream.Seek(FDataSizeOffs, soFromBeginning);
            Size := FStream.Size - HeaderSize;
            FStream.Write(Size, 4);
          end else
          begin
            Size := FStream.Size - 44;
            FStream.Seek(DataSizeOffs, soFromBeginning);
            FStream.Write(Size, 4);
          end;
        end;
        wtExtPCM:
        begin
          Size := FStream.Size - SizeOf(TWaveHeaderExt);
          FStream.Seek(SizeOf(TWaveHeaderExt) - 4, soFromBeginning);
          FStream.Write(Size, 4);
        end;
        wtDVIADPCM:
        begin
          if FFileMode = foAppend then
          begin
            FStream.Seek(FDataSizeOffs, soFromBeginning);
            Size := FStream.Size - HeaderSize;
            FStream.Write(Size, 4);
            FStream.Seek(FLenOffs, soFromBeginning);
            Size := (Size div FBlockAlign)*((FBlockAlign-FPrevCh*4)*(2 div FPrevCh) + 1);
            FStream.Write(Size, 4);
          end else
          begin
            Size := FStream.Size - SizeOf(TDVIADPCMHeader);
            FStream.Seek(0, soFromBeginning);
            Fstream.Read(Hdr, SizeOf(Hdr));
            Hdr.DataSize := Size;
            Hdr.DataLength := (Hdr.DataSize div Hdr.BlockAlign) * Hdr.SamplesPerBlock;
            FStream.Seek(0, soFromBeginning);
            if FStream.Write(Hdr, SizeOf(Hdr)) <> SizeOf(Hdr) then
               raise EAuException.Create('Error writing wave file');
          end;
        end;
      end;
      Size := FStream.Size - 8;
      FStream.Seek(4, soFromBeginning);
      FStream.Write(Size, 4);
    end else  // if ((FInput.Size < 0) or (FFileMode = foAppend) or (FStopped = True)) then
    begin
      BlockSize := 0;
      if FWavType = wtPCM then
      begin
        BlockSize := FInput.Size + 44 - FStream.Position;
      end;
      if FWavType = wtExtPCM then
      begin
        BlockSize := FInput.Size + SizeOf(TWaveHeaderExt) - FStream.Position;
      end;
      if BlockSize > 0 then
      begin
        GetMem(P, BlockSize);
        FillChar(P^, BlockSize, 0);
        if FStream.Write(P^, BlockSize) <> BlockSize then
            raise EAuException.Create('Error writing wave file');
        FreeMem(P);
      end;
    end;
    if (not FStreamAssigned) and (FStream <> nil) then FStream.Free;
    FInput.Flush;
  end;

  function TWaveOut.DoOutput;
  var
    l, m : Integer;
    Len : LongWord;
    DVIInBuf : PBuffer16;
    DVIOutBuf : PBuffer8;
    P : PBuffer8;
    Ptr : Pointer;
    BlockDataSize : Integer;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    Len := 0;
    if Abort then
    begin
      (* We don't close file here to avoid exceptions
        if output component's Stop method is called *)
      Result := False;
      Exit;
    end;
    if EndOfInput then
    begin
      Result := False;
      Exit;
    end;
    case FWavType of
      wtPCM, wtExtPCM :
      begin
        try
          Len := BUF_SIZE;
          Finput.GetData(Ptr, Len);
          if FStream.Write(Ptr^, Len) <> Integer(Len) then
            raise EAuException.Create('Error writing wave file');
        except
        end;
        if Len > 0 then Result := True
        else Result := False;
      end;
      wtDVIADPCM :
      begin
        BlockDataSize := (FBlockAlign - 4*FInput.Channels)*4 +2*FInput.Channels;
        GetMem(DVIInBuf, BlockDataSize);
        GetMem(DVIOutBuf, FBlockAlign);
        P := PBuffer8(DVIInBuf);
        try
          if FInput.Channels = 2 then
          begin
            l := 0;
            FillChar (DVIInBuf[0], BlockDataSize, 0);
            while l <> BlockDataSize do
            begin
              m := Finput.CopyData(@P[l], BlockDataSize-l);
              if m = 0 then
              begin
                EndOfInput := True;
                Break;
              end;
              Inc(l, m);
            end;
            Len := FBlockAlign;
            FEncodeState.PredSamp_l := DVIInBuf[0];
            m := 1;
            FEncodeState.PredSamp_r := DVIInBuf[m];
            m := 2;
            EncodeDVIADPCMStereo(@DVIInBuf[m], @DVIOutBuf[0]);
            if FStream.Write(DVIOutBuf[0], FBlockAlign) <> FBlockAlign then
              raise EAuException.Create('Error writing wave file');
            end else
          begin
            l := 0;
            FillChar (DVIInBuf[0], BlockDataSize, 0);
            while l <> BlockDataSize do
            begin
              m := Finput.CopyData(@P[l], BlockDataSize-l);
              if m = 0 then
              begin
                EndOfInput := True;
                Break;
              end;
              Inc(l, m);
            end;
            Len := FBlockAlign;
            FEncodeState.PredSamp_l := DVIInBuf[0];
            m := 1;
            EncodeDVIADPCMMono(@DVIInBuf[m], @DVIOutBuf[0]);
            if FStream.Write(DVIOutBuf[0], FBlockAlign) <> FBlockAlign then
              raise EAuException.Create('Error writing wave file');
          end;
        except
        end;
        FreeMem(DVIInBuf);
        FreeMem(DVIOutBuf);
        if Len > 0 then Result := True
        else Result := False;
      end;
    end;
  end;

  function TWaveIn.GetWavType;
  begin
    OpenFile;
    Result := _WavType;
  end;

  procedure TWaveOut.SetFileMode;
  begin
    FFileMode := aMode;
  end;

  function TWaveIn.ReadDVIADPCMBlock;
  var
    block : array of Byte;
    BH : TDVIADPCMBlockHeader;
  begin
    Result := False;
    if Seekable then
    if FStream.Position >= FStream.Size then Exit;
    SetLength(Block, DVI_ADPCM_INFO.BlockLength);
    if FStream.Read(Block[0], Length(Block))= 0 then Exit;
    Result := True;
    Move(Block[0], BH, SizeOf(BH));
    DVI_ADPCM_STATE.valprev_l := BH.Samp0;
    DVI_ADPCM_STATE.index_l := BH.StepTableIndex;
    if FChan = 2 then
    begin
      Move(Block[4], BH, SizeOf(BH));
      DVI_ADPCM_STATE.valprev_r := BH.Samp0;
      DVI_ADPCM_STATE.index_r := BH.StepTableIndex;
      Move(Block[8], Data^, DVI_ADPCM_INFO.BlockLength-8);
    end else
    Move(Block[4], Data^, DVI_ADPCM_INFO.BlockLength-4);
  end;

  function TWaveIn.ReadMSADPCMBlock;
  var
    block : array of Byte;
    BHM : TMSADPCMBlockHeaderMono;
    BHS : TMSADPCMBlockHeaderStereo;
  begin
    Result := False;
    if FStream.Position >= FStream.Size then Exit;
    Result := True;
    SetLength(Block, MS_ADPCM_INFO.BlockLength);
    FStream.Read(Block[0], Length(Block));
    if FChan = 1 then
    begin
      Move(Block[0], BHM, SizeOf(BHM));
      MS_ADPCM_STATE.predictor[0] := BHM.predictor;
      MS_ADPCM_STATE.Delta[0] := BHM.Delta;
      MS_ADPCM_STATE.Samp1[0] := BHM.Samp1;
      MS_ADPCM_STATE.Samp2[0] := BHM.Samp2;
      Move(Block[SizeOf(BHM)], Data^, MS_ADPCM_INFO.BlockLength-SizeOf(BHM));
    end else
    begin
      Move(Block[0], BHS, SizeOf(BHS));
      MS_ADPCM_STATE := BHS;
      Move(Block[SizeOf(BHS)], Data^, MS_ADPCM_INFO.BlockLength-SizeOf(BHS));
    end;
  end;

  procedure TWaveIn.ReadRIFFHeader;
  const
    __BufSize = $FFFF;
  var
    i : Integer;
    WordVal : Word;
    IntVal : Integer;
    Buff : array[0..__BufSize] of AnsiChar;
    State : Integer;
    ChunkSize : Integer;
    SubType : TGuid;
  begin
    FSize := 0;
    FBPS := 0;
    FChan := 0;
    FSR := 0;
    _WavType := wtUnsupported;
    State := LookingForRIFF;
    i := 4;
    FStream.Read(Buff[0], 4);
    while i < __BufSize do
    begin
      case State of
        LookingForRIFF :
        begin
          if not Compare4(@Buff[i-4], 'RIFF') then
          begin
            FStream.Read(Buff[i], 1);
            Inc(i);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForWAVE;
          end;
        end;
        LookingForWAVE :
        begin
          if not Compare4(@Buff[i-4], 'WAVE') then
          begin
            FStream.Read(Buff[i], 1);
            Inc(i);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForFMT;
          end;
        end;
        LookingForFMT :
        begin
          if not Compare4(@Buff[i-4], 'fmt ') then
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Move(Buff[i-ChunkSize], WordVal, 2);
            case WordVal of
              WAVE_FORMAT_PCM : _WavType := wtPCM;
              WAVE_FORMAT_IMA_ADPCM : _WavType := wtDVIADPCM;
              WAVE_FORMAT_ADPCM : _WavType := wtMSADPCM;
              WAVE_FORMAT_MP3 : _WavType := wtACM;
              WAVE_FORMAT_IEEE_FLOAT : _WavType := wtIEEEFloat;
              WAVE_FORMAT_EXTENSIBLE : _WavType := wtExtPCM;
              else Exit;
            end;
            Move(Buff[i+2-ChunkSize], WordVal, 2);
            FChan := WordVal;
            Move(Buff[i+4-ChunkSize], IntVal, 4);
            FSR := IntVal;
            Move(Buff[i+12-ChunkSize], WordVal, 2);
            if _WavType = wtDVIADPCM then
            DVI_ADPCM_INFO.BlockLength := WordVal else
            MS_ADPCM_INFO.BlockLength := WordVal;
            Move(Buff[i+14-ChunkSize], WordVal, 2);
            FBPS := WordVal;
            if _WavType = wtExtPCM then
            begin
              Move(Buff[i-16], SubType, 16);
             if GUIDSEqual(SubType, KSDATAFORMAT_SUBTYPE_IEEE_FLOAT) then
             begin
                _WavType := wtExtIEEEFLOAT;
             end else
             if not GUIDSEqual(SubType, KSDATAFORMAT_SUBTYPE_PCM) then
                _WavType := wtUnsupported;
            end;
            if _WavType in [wtDVIADPCM, wtMSADPCM, wtACM] then
            begin
              Move(Buff[i+18-ChunkSize], WordVal, 2);
              if _WavType = wtDVIADPCM then
              DVI_ADPCM_INFO.SamplesPerBlock := WordVal
              else MS_ADPCM_INFO.SamplesPerBlock := WordVal;
              if _WavType = wtMSADPCM then
              begin
                Move(Buff[i+20-ChunkSize], WordVal, 2);
                MS_ADPCM_INFO.NumCoeff := WordVal;
                Move(Buff[i+22-ChunkSize], MS_ADPCM_INFO.CoefSets[0], MS_ADPCM_INFO.NumCoeff*SizeOf(TMS_ADPCM_COEF_SET));
              end;
              State := LookingForFACT;
            end else State := LookingForDATA;
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end;
        end;
        LookingForFACT :
        begin
          if not Compare4(@Buff[i-4], 'fact') then
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Move(Buff[i-ChunkSize], IntVal, 4);
            if _WavType = wtDVIADPCM then
            DVI_ADPCM_INFO.DataSize := IntVal
            else
            MS_ADPCM_INFO.DataSize := IntVal;
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForDATA;
          end;
        end;
        LookingForDATA :
        begin
          if Buff[i-4] = #0 then
          begin
            FStream.Read(Buff[i], 1);
            Inc(i);
          end;
          if not Compare4(@Buff[i-4], 'data') then
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            FStream.Read(Buff[i], 4);
            if _WavType in [wtPCM, wtExtPCM, wtIEEEFloat, wtExtIEEEFloat] then
              Move(Buff[i], FSize, 4);
            Inc(i, 4);
            HeaderSize := i;
            Exit;
          end;
        end;
      end;
      if Seekable then
      if FStream.Position >= FStream.Size then Break;
    end;
    _WavType := wtUnsupported;
  end;

  procedure TWaveOut.ReadRIFFHeader;
  var
    i : Integer;
    WordVal : Word;
    Buff : array[0..$fff] of AnsiChar;
    State : Integer;
    ChunkSize : Integer;
  begin
    FPrevWavType := wtUnsupported;
    State := LookingForRIFF;
    i := 4;
    FStream.Read(Buff[0], 4);
    while i < $2000 do
    begin
      case State of
        LookingForRIFF :
        begin
          if not Compare4(@Buff[i-4], 'RIFF') then
          begin
            FStream.Read(Buff[i], 1);
            Inc(i);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForWAVE;
          end;
        end;
        LookingForWAVE :
        begin
          if not Compare4(@Buff[i-4], 'WAVE') then
          begin
            FStream.Read(Buff[i], 1);
            Inc(i);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForFMT;
          end;
        end;
        LookingForFMT :
        begin
          if not Compare4(@Buff[i-4], 'fmt ') then
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Move(Buff[i-ChunkSize], WordVal, 2);
            case WordVal of
              WAVE_FORMAT_PCM : FPrevWavType := wtPCM;
              WAVE_FORMAT_IMA_ADPCM : FPrevWavType := wtDVIADPCM;
              else Exit;
            end;
            Move(Buff[i+2-ChunkSize], FPrevCh, 2);
            Move(Buff[i+4-ChunkSize], FPrevSR, 4);
            Move(Buff[i+12-ChunkSize], FBlockAlign, 2);
            Move(Buff[i+14-ChunkSize], FPrevBPS, 2);
            if FPrevWavType = wtDVIADPCM then
            State := LookingForFACT
            else State := LookingForDATA;
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end;
        end;
        LookingForFACT :
        begin
          if Buff[i-4] = #0 then
          begin
            FStream.Read(Buff[i], 1);
            Inc(i);
          end;
          if not Compare4(@Buff[i-4], 'fact') then
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FLenOffs := i;
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Move(Buff[i-ChunkSize], FPrevLen, 4);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForDATA;
          end;
        end;
        LookingForDATA :
        begin
          if not Compare4(@Buff[i-4], 'data') then
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            FStream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            FStream.Read(Buff[i], 4);
            Inc(i, 4);
            FDataSizeOffs := i-4;
            Move(Buff[i-4], FPrevDataSize, 4);
            HeaderSize := i;
            Exit;
          end;
        end;
      end;
      if FStream.Position >= FStream.Size then Break;
    end;
    FPrevWavType := wtUnsupported
  end;

  procedure TWaveOut.SetBlockSize;
  begin
    if not Busy then
    if BS <> 0 then
    if (BS mod 4) = 0 then FBlockAlign := BS;
  end;

  procedure TWaveTap.FillHeaderPCM(var Header : TWaveHeader);
  begin
    Header.RIFF := 'RIFF';
    Header.RIFFType := 'WAVE';
    Header.FmtChunkId := 'fmt ';
    Header.FmtChunkSize := 16;
    Header.FormatTag := WAVE_FORMAT_PCM;
    Header.Channels := FInput.Channels;
    Header.SampleRate := FInput.SampleRate;
    Header.BitsPerSample := FInput.BitsPerSample;
    Header.BlockAlign := (Header.BitsPerSample * Header.Channels) shr 3;
    Header.BytesPerSecond := Header.SampleRate * Header.BlockAlign;
    Header.DataChunkId := 'data';
  end;


  procedure TWaveTap.StartRecordInternal;
  var
    Header : TWaveHeader;
  begin
    if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
    FStream := TAuFileStream.Create(FWideFileName, fmCreate or FShareMode);
    BytesCounter := 0;
    FillHeaderPCM(Header);
    FStream.Write(Header, SizeOf(Header));
  end;

  procedure TWaveTap.StopRecordInternal;
  begin
    FStream.Seek(DataSizeOffs, soFromBeginning);
    FStream.Write(BytesCounter, 4);
    Inc(BytesCounter, 36);
    FStream.Seek(4, soFromBeginning);
    FStream.Write(BytesCounter, 4);
    FStream.Free;
  end;

  procedure TWaveTap.WriteDataInternal;
  begin
    Inc(BytesCounter, BufferLength);
    FStream.Write(Buffer^, BufferLength);
  end;  

end.
