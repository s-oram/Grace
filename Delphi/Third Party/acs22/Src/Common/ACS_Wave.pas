(*
  This file is a part of Audio Components Suite v 2.2
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_Wave;

interface

uses

// This is required to enable TWaveIn read MS ACM (MP3-encoded) files.
  {$IFDEF WIN32}
  MMSYSTEM, MSAcm, waveconverter,
  {$ENDIF}

  {$IFDEF LINUX}
  Math, MAD,
  {$ENDIF}

  Classes, SysUtils, ACS_Types, ACS_Classes;

const

  BUF_SIZE = $4000;

type

  TWavType = (wtUnsupported, wtPCM, wtDVIADPCM, wtMSADPCM, wtACM);

  TWaveHeader = record
    // RIFF file header
    RIFF: array [0..3] of Char;          // = 'RIFF'
    FileSize: Integer;                   // = FileSize - 8
    RIFFType: array [0..3] of Char;      // = 'WAVE'
    // Format chunk
    FmtChunkId: array [0..3] of Char;    // = 'fmt'
    FmtChunkSize: Integer;               // = 16
    FormatTag: Word;                     // One of WAVE_FORMAT_XXX constants
    Channels: Word;                      // = 1 - mono = 2 - stereo
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BlockAlign: Word;
    BitsPerSample: Word;                 // = 8, 16 or 32 Bits/sample
    // Data Chunk
    DataChunkId: array [0..3] of Char;   // = 'data'
    DataSize: Integer;   // Data size in bytes
  end;

  TDVIADPCMHeader = record
    // RIFF file header
    RIFF: array [0..3] of Char;          // = 'RIFF'
    FileSize: Integer;                   // = FileSize - 8
    RIFFType: array [0..3] of Char;      // = 'WAVE'
    // Format chunk
    FmtChunkId: array [0..3] of Char;    // = 'fmt'
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
    FactChunkId: array [0..3] of Char;  // = 'fact'
    FactChunkSize : Integer;            // = 4
    DataLength : Integer;
    // Data Chunk
    DataChunkId: array [0..3] of Char;   // = 'data'
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
    DataSize : Integer;
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

  TWaveIn = class(TACSFileIn)
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
  public
    property WavType : TWavType read GetWavType;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    function Seek(SampleNum : Integer) : Boolean; override;
  end;

  TWaveOut = class(TACSFileOut)
  private
    EndOfInput : Boolean;
    Buffer : array [0..BUF_SIZE-1] of Byte;
    FWavType : TWavType;
    FEncodeState : TDVI_ADPCM_ENCODE_STATE_STEREO;
    FPrevSR : Integer;
    FPrevCh : Word;
    FPrevBPS : Word;
    FPrevLen : Integer;
    FLenOffs : Integer;
    FPrevDataSize : Integer;
    FDataSizeOffs : Integer;
    FPrevWavType : TWavType;
    HeaderSize : Integer;
    FBlockAlign : Word;
    procedure SetWavType(WT : TWavType);
    procedure ReadRIFFHeader;
    procedure FillHeaderPCM(var Header : TWaveHeader);
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
    property WavType : TWavType read FWavType write SetWavType;
    property BlockSize : Word read FBlockAlign write SetBlockSize;
  end;

implementation

const

  WaveHeaderOffs = 44;
  DataSizeOffs = 40;
  WAVE_FORMAT_PCM = 1;
  WAVE_FORMAT_ADPCM = 2;
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
      if (Code and 8) <> 0 then	Diff := -Diff;
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
      if (Code and 8) <> 0 then	Diff := -Diff;
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
      if (Code and 8) <> 0 then	Diff := -Diff;
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

  function Compare4(S1, S2 : PChar) : Boolean;
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
     text : array[0..4] of Char;
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
    FValid := True;
    if FOpened = 0 then
    begin
      _WavType := wtUnsupported;
      if not FStreamAssigned then
      FStream := TFileStream.Create(FFileName, fmOpenRead) as TFileStream;
      ReadRIFFHeader;
      case Self._WavType of
        wtUnsupported :
        begin
          FValid := False;
          Inc(FOpened);
          Exit;
        end;
        wtPCM:
        begin
          FTotalSamples := FSize div ((FBPS shr 3) * FChan);
        end;
        wtDVIADPCM :
        begin
          if FBPS <> 4 then
          FValid := False;
          FBPS := 16;
          FTotalSamples := DVI_ADPCM_INFO.DataSize;
          FSize := FTotalSamples*2*FChan;
        end;
        wtMSADPCM :
        begin
          FBPS := 16;
          FSize := MS_ADPCM_INFO.DataSize*2*FChan;
          FTotalSamples := MS_ADPCM_INFO.DataSize;
        end;
        wtACM :
        begin

{$IFDEF WIN32}

          (* Checking if the file is ACM encoded and if it is, decoding it.
             Thanks to Jan Henrik Ejme, <jan.h.ejme@xhlp.com> for the
             provided converter units. *)
          try
            WaveConverter := TWaveConverter.Create;
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
              WaveConverter.Free;
              _wavType := wtACM;
            end;
          except
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
    end;
    Inc(FOpened);
  end;

  procedure TWaveIn.CloseFile;
  begin
    if FOpened = 1 then
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
    end;
    if FOpened > 0 then Dec(FOpened);
  end;

  function TWaveIn.GetData;
  var
    l, csize : Integer;
    offs : Integer;
    Data : Pointer;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      case _WavType of
        wtDVIADPCM :
        begin
          if FOffset <> 0 then
          begin
            offs := (FStream.Position div DVI_ADPCM_INFO.BlockLength)
            * DVI_ADPCM_INFO.SamplesPerBlock + Round((FOffset/100)*FTotalSamples);
            if offs < 0 then offs := 0;
            Seek(offs);
            FPosition := (FStream.Position div DVI_ADPCM_INFO.BlockLength)
            * DVI_ADPCM_INFO.SamplesPerBlock;
            FPosition := FPosition*2*FChan;
            FOffset := 0;
          end;
          if FPosition >= FSize then
          begin
            if FLoop then
            begin
              Flush;
              Init;
            end else
            begin
              Result := 0;
              Exit;
            end;
          end;
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
            end;
          end else BufEnd := 0;
          FreeMem(Data);
        end;
        wtMSADPCM :
        begin
          if FOffset <> 0 then
          begin
            offs := (FStream.Position div MS_ADPCM_INFO.BlockLength)
            * MS_ADPCM_INFO.SamplesPerBlock + Round((FOffset/100)*FTotalSamples);
            if offs < 0 then offs := 0;
            Seek(offs);
            FPosition := (FStream.Position div MS_ADPCM_INFO.BlockLength)
            * MS_ADPCM_INFO.SamplesPerBlock;
            FPosition := FPosition*2*FChan;
            FOffset := 0;
          end;
          if FPosition >= FSize then
          begin
            if FLoop then
            begin
              Flush;
              Init;
            end else
            begin
              Result := 0;
              Exit;
            end;
          end;
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
        wtPCM:
        begin
          if FOffset <> 0 then
          begin
            offs := Round((FOffset/100)*FSize);
            FPosition := FPosition + offs;
            if FPosition < 0 then FPosition := 0
            else if FPosition > FSize then FPosition := FSize;
            FStream.Seek((FPosition shr 2) shl 2, soFromBeginning); // align to 4-byte frame
            FOffset := 0;
          end;
          BufStart := 1;
          l := FStream.Read(Buf, BUF_SIZE);
          if FPosition+l >= FSize then l := FSize - FPosition;
          if l <=0 then
          begin
            if FLoop then
            begin
              Flush;
              Init;
            end else
            begin
              Result := 0;
              Exit;
            end;
          end;
          if l = 0 then
          begin
            if FLoop then
            begin
              Flush;
              Init;
              l := FStream.Read(Buf, BUF_SIZE);
            end else
            begin
              Result := 0;
              Exit;
            end;
          end;
          BufEnd := l;
        end;
        wtACM:
        begin
          if FOffset <> 0 then
          begin
            offs := Round((FOffset/100)*FSize);
            FPosition := FPosition + offs;
            if FPosition < 0 then FPosition := 0
            else if FPosition > FSize then FPosition := FSize;
            FStream.Seek((FPosition shr 2) shl 2, soFromBeginning); // align to 4-byte frame
            FOffset := 0;
          end;
          BufStart := 1;
          l := FStream.Read(Buf, BUF_SIZE);
          if FPosition+l >= FSize then l := FSize - FPosition;
          if l <=0 then
          begin
            if FLoop then
            begin
              Flush;
              Init;
            end else
            begin
              Result := 0;
              Exit;
            end;
          end;
          BufEnd := l;
        end;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(Buf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  function TWaveIn.Seek;
  begin
    Result := False;
    if not FSeekable then Exit;
    Result := True;
    OpenFile;
    case _WavType of
      wtPCM :
      begin
        Stream.Seek(SampleNum*(Self.FBPS shr 3)*FChan+HeaderSize, soFromBeginning);
      end;
      wtDVIADPCM:
      begin
        Stream.Seek((SampleNum div DVI_ADPCM_INFO.SamplesPerBlock)*DVI_ADPCM_INFO.BlockLength + HeaderSize, soFromBeginning);
      end;
      wtMSADPCM:
      begin
        Stream.Seek((SampleNum div MS_ADPCM_INFO.SamplesPerBlock)*MS_ADPCM_INFO.BlockLength + HeaderSize, soFromBeginning);
      end;
      else
      begin
        Result := False;
      end;
    end;
    CloseFile;
  end;

procedure TWaveOut.FillHeaderPCM(var Header : TWaveHeader);
var
  text : array[0..4] of Char;
begin
  text := 'RIFF';
  Move(text[0], Header.RIFF[0], 4);
  Header.FileSize := FInput.Size + WaveHeaderOffs;
  text := 'WAVE';
  Move(text[0], Header.RIFFType[0], 4);
  text := 'fmt ';
  Move(text[0], Header.FmtChunkId[0], 4);
  Header.FmtChunkSize := 16;
  Header.FormatTag := WAVE_FORMAT_PCM;
  Header.Channels := FInput.Channels;
  Header.SampleRate := FInput.SampleRate;
  Header.BitsPerSample := FInput.BitsPerSample;
  Header.BlockAlign := (Header.BitsPerSample * Header.Channels) shr 3;
  Header.BytesPerSecond := Header.SampleRate * Header.BlockAlign;
  text := 'data';
  Move(text[0], Header.DataChunkId[0], 4);
  Header.DataSize := FInput.Size;
end;

procedure TWaveOut.FillHeaderDVIADPCM(var Header : TDVIADPCMHeader);
var
  text : array[0..4] of Char;
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
end;

destructor TWaveOut.Destroy;
begin
  inherited Destroy;
end;

procedure TWaveOut.SetWavType;
begin
  if Buisy then Exit;
  if (WT = wtPCM) or (WT = wtDVIADPCM) then
  FWavType := WT;
end;

  procedure TWaveOut.Prepare;
  var
    Header : TWaveHeader;
    DVIADPCMHeader : TDVIADPCMHeader;
  begin
    EndOfInput := False;
    if not FStreamAssigned then
    begin
      if FFileName = '' then raise EACSException.Create('File name is not assigned.');
      if (not FileExists(FFileName)) or (FFileMode = foRewrite) then
      FStream := TFileStream.Create(FFileName, fmCreate or fmShareExclusive, FAccessMask)
      else FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive, FAccessMask);
    end;
    FInput.Init;
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
        raise EACSException.Create('Cannot append data to this .wav file.');
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
            raise EACSException.Create('Cannot append data in different audio format.');
          end;
          FStream.Seek(0, soFromEnd);
        end else
        begin
          FillHeaderPCM(Header);
          FStream.Write(Header, WaveHeaderOffs);
        end;
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
          raise EACSException.Create('Cannot encode 8 bit sound into ADPCM.');
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
            raise EACSException.Create('Cannot append data in different audio format.');
          end;
          FStream.Seek(0, soFromEnd);
        end else
        begin
          FillHeaderDVIADPCM(DVIADPCMHeader);
          FStream.Write(DVIADPCMHeader, SizeOf(DVIADPCMHeader));
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
  begin
    if ((FInput.Size < 0) or (FFileMode = foAppend)) and (FStream <> nil) then
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
            FStream.Write(Hdr, SizeOf(Hdr));
          end;
        end;
      end;
      Size := FStream.Size;
      FStream.Seek(4, soFromBeginning);
      FStream.Write(Size, 4);
    end;
    if (not FStreamAssigned) and (FStream <> nil) then FStream.Free;
    FInput.Flush;
  end;

  function TWaveOut.DoOutput;
  var
    Len, l, m : Integer;
    DVIInBuf : PBuffer16;
    DVIOutBuf : PBuffer8;
    P : PBuffer8;
    BlockDataSize : Integer;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    if Progress <> CurProgr then
    begin
      CurProgr := Progress;
      if Assigned(FOnProgress) then FOnProgress(Self);
    end;
    Len := 0;
    if Abort then
    begin
      (* We don't close file here to avoide exceptions
        if output componenet's Stop method is called *)
      Result := False;
      Exit;
    end;
    if EndOfInput then
    begin
      Result := False;
      Exit;
    end;
    case FWavType of
      wtPCM :
      begin
        try
          while InputLock do;
          InputLock := True;
          Len := Finput.GetData(@Buffer[0], BUF_SIZE);
          InputLock := False;
          FStream.Write(Buffer[0], Len);
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
            while InputLock do;
            InputLock := True;
            while l <> BlockDataSize do
            begin
              m := Finput.GetData(@P[l], BlockDataSize-l);
              if m = 0 then
              begin
                EndOfInput := True;
                Break;
              end;
              Inc(l, m);
            end;
            Len := FBlockAlign;
            InputLock := False;
            FEncodeState.PredSamp_l := DVIInBuf[0];
            m := 1;
            FEncodeState.PredSamp_r := DVIInBuf[m];
            m := 2;
            EncodeDVIADPCMStereo(@DVIInBuf[m], @DVIOutBuf[0]);
            FStream.Write(DVIOutBuf[0], FBlockAlign);
          end else
          begin
            l := 0;
            FillChar (DVIInBuf[0], BlockDataSize, 0);
            while InputLock do;
            InputLock := True;
            while l <> BlockDataSize do
            begin
              m := Finput.GetData(@P[l], BlockDataSize-l);
              if m = 0 then
              begin
                EndOfInput := True;
                Break;
              end;
              Inc(l, m);
            end;
            Len := FBlockAlign;
            InputLock := False;
            FEncodeState.PredSamp_l := DVIInBuf[0];
            m := 1;
            EncodeDVIADPCMMono(@DVIInBuf[m], @DVIOutBuf[0]);
            FStream.Write(DVIOutBuf[0], FBlockAlign);
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
    CloseFile;
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
    if FStream.Position >= FStream.Size then Exit;
    Result := True;
    SetLength(Block, DVI_ADPCM_INFO.BlockLength);
    FStream.Read(Block[0], Length(Block));
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
  var
    i : Integer;
    WordVal : Word;
    IntVal : Integer;
    Buff : array[0..$fff] of Char;
    State : Integer;
    ChunkSize : Integer;
  begin
    _WavType := wtUnsupported;
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
            if _WavType = wtPCM then
            Move(Buff[i-4], FSize, 4);
            HeaderSize := i;
            Exit;
          end;
        end;
      end;
      if FStream.Position >= FStream.Size then Break;
    end;
    _WavType := wtUnsupported
  end;

  procedure TWaveOut.ReadRIFFHeader;
  var
    i : Integer;
    WordVal : Word;
    Buff : array[0..$fff] of Char;
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
    if not Buisy then
    if BS <> 0 then
    if (BS mod 4) = 0 then FBlockAlign := BS;
  end;

end.
