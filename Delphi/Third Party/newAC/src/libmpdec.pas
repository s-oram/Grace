(*
  This unit contains Musepack decoder library headers transltion.
  Original Musepack decoder library headers Copyright (c) 2005, The Musepack Development Team.
  You can learn more about Musepack audio compression format at www.musepack.net.
*)

(* Written by Sergei Borisov *)

(* $Id: libmpdec.pas 649 2008-07-03 06:35:30Z andrei.borovsky $ *)

unit libmpdec;

interface

uses
  SysUtils, Classes, ACS_Classes;

const
  libMPDec_Name = 'libmpdec.dll';

  MPC_V_MEM = 2304;
  MPC_DECODER_MEMSIZE = 16384;

  MPC_FRAME_LENGTH          = 36 * 32;              /// samples per mpc frame
  MPC_DECODER_BUFFER_LENGTH = 4 * MPC_FRAME_LENGTH; /// required buffer size for decoder

  SEEKING_TABLE_SIZE  = 256;

var
  libMPDec_Loaded: Boolean = False;

type
  t_mpc_bool   = Byte;
  t_mpc_int16  = Smallint;
  t_mpc_uint16 = Word;
  t_mpc_int32  = Integer;
  t_mpc_uint32 = Cardinal;
  p_mpc_uint32 = ^t_mpc_uint32;
  t_mpc_int64  = Int64;

  t_mpc_streaminfo_off = t_mpc_int32;

  t_mpc_sample_format = Single;
  p_mpc_sample_format = ^t_mpc_sample_format;

  t_mpc_streaminfo = record
    /// @name core mpc stream properties
    //@{
    sample_freq       : t_mpc_uint32;         ///< sample frequency of stream
    channels          : t_mpc_uint32;         ///< number of channels in stream
    header_position   : t_mpc_streaminfo_off; ///< byte offset of position of header in stream
    stream_version    : t_mpc_uint32;         ///< streamversion of stream
    bitrate           : t_mpc_uint32;         ///< bitrate of stream file (in bps)
    average_bitrate   : Double;               ///< average bitrate of stream (in bits/sec)
    frames            : t_mpc_uint32;         ///< number of frames in stream
    pcm_samples       : t_mpc_int64;
    max_band          : t_mpc_uint32;         ///< maximum band-index used in stream (0...31)
    _is               : t_mpc_uint32;         ///< intensity stereo (0: off, 1: on)
    ms                : t_mpc_uint32;         ///< mid/side stereo (0: off, 1: on)
    block_size        : t_mpc_uint32;         ///< only needed for SV4...SV6 -> not supported
    profile           : t_mpc_uint32;         ///< quality profile of stream
    profile_name      : PChar;                ///< name of profile used by stream
    //@}

    /// @name replaygain related fields
    //@{
    gain_title        : t_mpc_int16;          ///< replaygain title value
    gain_album        : t_mpc_int16;          ///< replaygain album value
    peak_album        : t_mpc_uint16;         ///< peak album loudness level
    peak_title        : t_mpc_uint16;         ///< peak title loudness level
    //@}

    /// @name true gapless support data
    //@{
    is_true_gapless   : t_mpc_uint32;         ///< true gapless? (0: no, 1: yes)
    last_frame_samples: t_mpc_uint32;         ///< number of valid samples within last frame

    encoder_version   : t_mpc_uint32;         ///< version of encoder used
    encoder           : packed array [0 .. 255] of Char; ///< encoder name

    tag_offset        : t_mpc_streaminfo_off; ///< offset to file tags
    total_file_length : t_mpc_streaminfo_off; ///< total length of underlying file
    //@}

    /// @name fast seeking support
    //@{
    fast_seek         : t_mpc_uint32;         ///< support fast seeking ? (0: no, 1: yes)
    //@}
  end;
  p_mpc_streaminfo = ^t_mpc_streaminfo;

  t_read_func = function (t: Pointer; ptr: Pointer; size: t_mpc_int32): t_mpc_int32; cdecl;
  t_seek_func = function (t: Pointer; offset: t_mpc_int32): t_mpc_bool; cdecl;
  t_tell_func = function (t: Pointer): t_mpc_int32; cdecl;
  t_get_size_func = function (t: Pointer): t_mpc_int32; cdecl;
  t_can_seek_func = function (t: Pointer): t_mpc_bool; cdecl;

  t_mpc_reader = packed record
    /// Reads size bytes of data into buffer at ptr.
    read: t_read_func;

    /// Seeks to byte position offset.
    seek: t_seek_func;

    /// Returns the current byte offset in the stream.
    tell: t_tell_func;

    /// Returns the total length of the source stream, in bytes.
    get_size: t_get_size_func;

    /// True if the stream is a seekable stream.
    can_seek: t_can_seek_func;

    /// Field that can be used to identify a particular instance of
    /// reader or carry along data associated with that reader.
    data: Pointer;
  end;
  p_mpc_reader = ^t_mpc_reader;

  QuantTyp = packed record
    L: packed array [0 .. 35] of t_mpc_int32;
    R: packed array [0 .. 35] of t_mpc_int32;
  end;

  t_mpc_decoder = record
    r: p_mpc_reader;

    /// @name internal state variables
    //@{

    dword               : t_mpc_uint32; /// currently decoded 32bit-word
    pos                 : t_mpc_uint32; /// bit-position within dword
    Speicher            : packed array [0 .. MPC_DECODER_MEMSIZE - 1] of t_mpc_uint32; /// read-buffer
    Zaehler             : t_mpc_uint32; /// actual index within read-buffer

    samples_to_skip     : t_mpc_uint32;

    DecodedFrames       : t_mpc_uint32;
    OverallFrames       : t_mpc_uint32;
    SampleRate          : t_mpc_int32;  // Sample frequency

    StreamVersion       : t_mpc_uint32; // version of bitstream
    Max_Band            : t_mpc_int32;
    MPCHeaderPos        : t_mpc_uint32; // AB: needed to support ID3v2

    FrameWasValid       : t_mpc_uint32;
    MS_used             : t_mpc_uint32; // MS-coding used ?
    TrueGaplessPresent  : t_mpc_uint32;

    WordsRead           : t_mpc_uint32; // counts amount of decoded dwords

    // randomizer state variables
    __r1                : t_mpc_uint32;
    __r2                : t_mpc_uint32;

    // seeking
    seeking_table       : packed array [0 .. SEEKING_TABLE_SIZE - 1] of t_mpc_uint32;
    seeking_pwr         : t_mpc_uint32; // distance between 2 frames in seeking_table = 2^seeking_pwr
    seeking_table_frames: t_mpc_uint32; // last frame in seaking table
    seeking_window      : t_mpc_uint32; // number of frames to look for scalefactors

    SCF_Index_L         : packed array [0 .. 31, 0 .. 2] of t_mpc_int32;
    SCF_Index_R         : packed array [0 .. 31, 0 .. 2] of t_mpc_int32; // holds scalefactor-indices
    Q                   : packed array [0 .. 31] of QuantTyp; // holds quantized samples
    Res_L               : packed array [0 .. 31] of t_mpc_int32;
    Res_R               : packed array [0 .. 31] of t_mpc_int32; // holds the chosen quantizer for each subband
    DSCF_Flag_L         : packed array [0 .. 31] of t_mpc_bool;
    DSCF_Flag_R         : packed array [0 .. 31] of t_mpc_bool; // differential SCF used?
    SCFI_L              : packed array [0 .. 31] of t_mpc_int32;
    SCFI_R              : packed array [0 .. 31] of t_mpc_int32; // describes order of transmitted SCF
    MS_Flag             : packed array [0 .. 31] of t_mpc_bool; // MS used?
{$ifdef MPC_FIXED_POINT}
    SCF_shift           : packed array [0 .. 255] of Byte;
{$endif}

    V_L                 : packed array [0 .. MPC_V_MEM + 957] of t_mpc_sample_format;
    V_R                 : packed array [0 .. MPC_V_MEM + 957] of t_mpc_sample_format;
    Y_L                 : packed array [0 .. 35, 0 .. 31] of t_mpc_sample_format;
    Y_R                 : packed array [0 .. 35, 0 .. 31] of t_mpc_sample_format;
    SCF                 : packed array [0 .. 255] of t_mpc_sample_format; ///< holds adapted scalefactors (for clipping prevention)
    //@}
  end;
  p_mpc_decoder = ^t_mpc_decoder;


  { class EMPCDecException }

  EMPCDecException = class(Exception)
  end;

  { class TMPCDecoder }

  TMPCDecoderBuffer = packed array [0 .. MPC_DECODER_BUFFER_LENGTH - 1] of t_mpc_sample_format;

  TMPCDecoder = class
  private
    FStream: TStream;

    FInstance: t_mpc_decoder;
    FReader: t_mpc_reader;
    FStreamInfo: t_mpc_streaminfo;

    function GetNumChannels: Cardinal;
    function GetSampleRate: Cardinal;
    function GetBitrate: Cardinal;
    function GetAverageBitrate: Cardinal;

    function GetVersion: Cardinal;

    function GetNumSamples: Int64;
  public
    constructor Create(AStream: TStream); reintroduce;

    function Decode(var Buffer: TMPCDecoderBuffer): Cardinal;
    function Seek(Sample: Int64): Boolean;

    property NumChannels: Cardinal read GetNumChannels;
    property SampleRate: Cardinal read GetSampleRate;
    property AverageBitrate: Cardinal read GetAverageBitrate;
    property Bitrate: Cardinal read GetBitrate;
    property Version: Cardinal read GetVersion;

    property NumSamples: Int64 read GetNumSamples;
  end;

procedure LoadLibMPDec;
procedure UnloadLibMPDec;

implementation

uses
  Windows;

const
  mpc_ERROR_CODE_OK        =   0;
  mpc_ERROR_CODE_FILE      = - 1;
  mpc_ERROR_CODE_SV7BETA   =   1;
  mpc_ERROR_CODE_CBR       =   2;
  mpc_ERROR_CODE_IS        =   3;
  mpc_ERROR_CODE_BLOCKSIZE =   4;
  mpc_ERROR_CODE_INVALIDSV =   5;

  mpc_FALSE = 0;
  mpc_TRUE  = 1;

  mpc_bool_value: array [Boolean] of t_mpc_bool = (mpc_FALSE, mpc_TRUE);

  mpc_streaminfo_init_name               = 'mpc_streaminfo_init';
  mpc_streaminfo_read_name               = 'mpc_streaminfo_read';
  mpc_streaminfo_get_length_name         = 'mpc_streaminfo_get_length';
  mpc_streaminfo_get_length_samples_name = 'mpc_streaminfo_get_length_samples';

  mpc_decoder_setup_name          = 'mpc_decoder_setup';
  mpc_decoder_initialize_name     = 'mpc_decoder_initialize';
  mpc_decoder_set_seeking_name    = 'mpc_decoder_set_seeking';
  mpc_decoder_set_streaminfo_name = 'mpc_decoder_set_streaminfo';
  mpc_decoder_scale_output_name   = 'mpc_decoder_scale_output';
  mpc_decoder_decode_name         = 'mpc_decoder_decode';
  mpc_decoder_decode_frame_name   = 'mpc_decoder_decode_frame';
  mpc_decoder_seek_sample_name    = 'mpc_decoder_seek_sample';
  mpc_decoder_seek_seconds_name   = 'mpc_decoder_seek_seconds';

type
  /// Initializes a streaminfo structure.
  /// \param si streaminfo structure to initialize
  t_mpc_streaminfo_init_proc = procedure (
    var si: t_mpc_streaminfo); cdecl;

  /// Reads streaminfo header from the mpc stream supplied by r.
  /// \param si streaminfo pointer to which info will be written
  /// \param r stream reader to supply raw data
  /// \return error code
  t_mpc_streaminfo_read_func = function (
    var si: t_mpc_streaminfo;
    const r: t_mpc_reader): t_mpc_int32; cdecl;

  /// Gets length of stream si, in seconds.
  /// \return length of stream in seconds
  t_mpc_streaminfo_get_length_func = function (
    const si: t_mpc_streaminfo): Double; cdecl;

  /// Returns length of stream si, in samples.
  /// \return length of stream in samples
  t_mpc_streaminfo_get_length_samples_func = function (
    const si: t_mpc_streaminfo): t_mpc_int64; cdecl;

var
  mpc_streaminfo_init: t_mpc_streaminfo_init_proc = nil;
  mpc_streaminfo_read: t_mpc_streaminfo_read_func = nil;
  mpc_streaminfo_get_length: t_mpc_streaminfo_get_length_func = nil;
  mpc_streaminfo_get_length_samples: t_mpc_streaminfo_get_length_samples_func = nil;

type
  /// Sets up decoder library.
  /// Call this first when preparing to decode an mpc stream.
  /// \param r reader that will supply raw data to the decoder
  t_mpc_decoder_setup_proc = procedure (
    var d: t_mpc_decoder;
    const r: t_mpc_reader); cdecl;

  /// Initializes mpc decoder with the supplied stream info parameters.
  /// Call this next after calling mpc_decoder_setup.
  /// \param si streaminfo structure indicating format of source stream
  /// \return TRUE if decoder was initalized successfully, FALSE otherwise    
  t_mpc_decoder_initialize_func = function (
    var d: t_mpc_decoder;
    const si: t_mpc_streaminfo): t_mpc_bool; cdecl;

  /// Call this next after calling mpc_decoder_setup.
  /// \param si streaminfo structure indicating format of source stream
  /// \param fast_seeking boolean 0 = use fast seeking if safe, 1 = force fast seeking
  t_mpc_decoder_set_seeking_proc = procedure (
    var d: t_mpc_decoder;
    const si: t_mpc_streaminfo;
    fast_seeking: t_mpc_bool); cdecl;

  t_mpc_decoder_set_streaminfo_proc = procedure (
    var d: t_mpc_decoder;
    const si: t_mpc_streaminfo); cdecl;

  /// Sets decoder sample scaling factor.  All decoded samples will be multiplied
  /// by this factor.
  /// \param scale_factor multiplicative scaling factor
  t_mpc_decoder_scale_output_proc = procedure (
    const d: t_mpc_decoder;
    scale_factor: Double); cdecl;

  /// Actually reads data from previously initialized stream.  Call
  /// this iteratively to decode the mpc stream.
  /// \param buffer destination buffer for decoded samples
  /// \param vbr_update_acc \todo document me
  /// \param vbr_update_bits \todo document me
  /// \return -1 if an error is encountered
  /// \return 0 if the stream has been completely decoded successfully and there are no more samples
  /// \return > 0 to indicate the number of bytes that were actually read from the stream.
  t_mpc_decoder_decode_func = function (
    var d: t_mpc_decoder;
    {buffer: p_mpc_sample_format;}
    var buffer: TMPCDecoderBuffer;
    vbr_update_acc: p_mpc_uint32;
    vbr_update_bits: p_mpc_uint32): t_mpc_uint32; cdecl;

  t_mpc_decoder_decode_frame_func = function (
    var d: p_mpc_decoder;
    in_buffer: p_mpc_uint32;
    in_len: t_mpc_uint32;
    {out_buffer: p_mpc_sample_format}
    var out_buffer: TMPCDecoderBuffer): t_mpc_uint32; cdecl;

  /// Seeks to the specified sample in the source stream.
  t_mpc_decoder_seek_sample_func = function (
    const d: t_mpc_decoder;
    destsample: t_mpc_int64): t_mpc_bool; cdecl;

  /// Seeks to specified position in seconds in the source stream.
  t_mpc_decoder_seek_seconds_func = function (
    const d: t_mpc_decoder;
    seconds: Double): t_mpc_bool; cdecl;

var
  mpc_decoder_setup: t_mpc_decoder_setup_proc = nil;
  mpc_decoder_initialize: t_mpc_decoder_initialize_func = nil;
  mpc_decoder_set_seeking: t_mpc_decoder_set_seeking_proc = nil;
  mpc_decoder_set_streaminfo: t_mpc_decoder_set_streaminfo_proc = nil;
  mpc_decoder_scale_output: t_mpc_decoder_scale_output_proc = nil;
  mpc_decoder_decode: t_mpc_decoder_decode_func = nil;
  mpc_decoder_decode_frame: t_mpc_decoder_decode_frame_func = nil;
  mpc_decoder_seek_sample: t_mpc_decoder_seek_sample_func = nil;
  mpc_decoder_seek_seconds: t_mpc_decoder_seek_seconds_func = nil;


procedure CheckFunc(Func: Pointer; const FuncName: String);
begin
  if not libMPDec_Loaded then
    raise EMPCDecException.CreateFmt(
      'MPC decoding library "%s" not loaded!', [libMPDec_Name]);
  if Func = nil then
    raise EMPCDecException.CreateFmt(
      'Function "%s" not found in MPC decoding library!', [FuncName]);
end;


{ reader functions }

function reader_read(t: Pointer; ptr: Pointer; size: t_mpc_int32): t_mpc_int32; cdecl;
begin
  if t <> nil then
    Result := TMPCDecoder(t).FStream.Read(ptr^, size)
  else
    Result := - 1;
end;

function reader_seek(t: Pointer; offset: t_mpc_int32): t_mpc_bool; cdecl;
begin
  Result := mpc_bool_value[
    (t <> nil) and
    (TMPCDecoder(t).FStream.Seek(offset, soFromBeginning) = offset)];
end;

function reader_tell(t: Pointer): t_mpc_int32; cdecl;
begin
  if t <> nil then
    Result := TMPCDecoder(t).FStream.Position
  else
    Result := - 1;
end;

function reader_get_size(t: Pointer): t_mpc_int32; cdecl;
begin
  if t <> nil then
    Result := TMPCDecoder(t).FStream.Size
  else
    Result := - 1;
end;

function reader_can_seek(t: Pointer): t_mpc_bool; cdecl;
begin
  Result := mpc_bool_value[t <> nil];
end;

procedure reader_setup(var reader: t_mpc_reader; data: Pointer);
begin
  reader.read := @reader_read;
  reader.seek := @reader_seek;
  reader.tell := @reader_tell;
  reader.get_size := @reader_get_size;
  reader.can_seek := @reader_can_seek;
  reader.data := data;
end;


{ class TMPCDecoder }

constructor TMPCDecoder.Create(AStream: TStream);
begin
  Assert(AStream <> nil);

  inherited Create();

  FStream := AStream;

  reader_setup(FReader, Self);

  CheckFunc(@mpc_streaminfo_init, mpc_streaminfo_init_name);
  mpc_streaminfo_init(FStreamInfo);

  CheckFunc(@mpc_streaminfo_read, mpc_streaminfo_read_name);
  if mpc_streaminfo_read(FStreamInfo, FReader) <> mpc_ERROR_CODE_OK then
    raise EMPCDecException.Create('Unable to read stream info!');

  CheckFunc(@mpc_decoder_setup, mpc_decoder_setup_name);
  mpc_decoder_setup(FInstance, FReader);

  reader_setup(FReader, Self);

  CheckFunc(@mpc_decoder_initialize, mpc_decoder_initialize_name);
  if mpc_decoder_initialize(FInstance, FStreamInfo) <> mpc_TRUE then
    raise EMPCDecException.Create('Unable to initialize MPC decoder!');
end;

function TMPCDecoder.GetNumChannels: Cardinal;
begin
  Result := FStreamInfo.channels;
end;

function TMPCDecoder.GetSampleRate: Cardinal;
begin
  Result := FStreamInfo.sample_freq;
end;

function TMPCDecoder.GetBitrate: Cardinal;
begin
 Result := FStreamInfo.bitrate;
end;

function TMPCDecoder.GetAverageBitrate: Cardinal;
begin
  Result := Round(FStreamInfo.average_bitrate/1000);
end;


function TMPCDecoder.GetVersion: Cardinal;
begin
  Result := FStreamInfo.stream_version;
end;

function TMPCDecoder.GetNumSamples: Int64;
begin
  CheckFunc(@mpc_streaminfo_get_length_samples, mpc_streaminfo_get_length_samples_name);
  Result := mpc_streaminfo_get_length_samples(FStreamInfo);
end;

function TMPCDecoder.Decode(var Buffer: TMPCDecoderBuffer): Cardinal;
begin
  CheckFunc(@mpc_decoder_decode, mpc_decoder_decode_name);
  Result := mpc_decoder_decode(FInstance, Buffer, nil, nil);
end;

function TMPCDecoder.Seek(Sample: Int64): Boolean;
begin
  CheckFunc(@mpc_decoder_seek_sample, mpc_decoder_seek_sample_name);
  Result := (mpc_decoder_seek_sample(FInstance, Sample) = mpc_TRUE);
end;

var
  libMPDec_Handle: HMODULE = 0;

procedure LoadLibMPDec;
begin
  LoadLibCS.Enter;
  if libMPDec_Loaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  libMPDec_Handle := LoadLibrary(libMPDec_Name);
  libMPDec_Loaded := (libMPDec_Handle <> 0);
  if libMPDec_Loaded then begin
    mpc_streaminfo_init := GetProcAddress(libMPDec_Handle, mpc_streaminfo_init_name);
    mpc_streaminfo_read := GetProcAddress(libMPDec_Handle, mpc_streaminfo_read_name);
    mpc_streaminfo_get_length := GetProcAddress(libMPDec_Handle, mpc_streaminfo_get_length_name);
    mpc_streaminfo_get_length_samples := GetProcAddress(libMPDec_Handle, mpc_streaminfo_get_length_samples_name);

    mpc_decoder_setup := GetProcAddress(libMPDec_Handle, mpc_decoder_setup_name);
    mpc_decoder_initialize := GetProcAddress(libMPDec_Handle, mpc_decoder_initialize_name);
    mpc_decoder_set_seeking := GetProcAddress(libMPDec_Handle, mpc_decoder_set_seeking_name);
    mpc_decoder_set_streaminfo := GetProcAddress(libMPDec_Handle, mpc_decoder_set_streaminfo_name);
    mpc_decoder_scale_output := GetProcAddress(libMPDec_Handle, mpc_decoder_scale_output_name);
    mpc_decoder_decode := GetProcAddress(libMPDec_Handle, mpc_decoder_decode_name);
    mpc_decoder_decode_frame := GetProcAddress(libMPDec_Handle, mpc_decoder_decode_frame_name);
    mpc_decoder_seek_sample := GetProcAddress(libMPDec_Handle, mpc_decoder_seek_sample_name);
    mpc_decoder_seek_seconds := GetProcAddress(libMPDec_Handle, mpc_decoder_seek_seconds_name);
  end;
  LoadLibCS.Leave;
end;

procedure UnloadLibMPDec;
begin
  if libMPDec_Loaded then
    FreeLibrary(libMPDec_Handle);
end;

end.

