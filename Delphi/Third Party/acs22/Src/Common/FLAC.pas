unit FLAC;

interface

uses
  {$IFDEF WIN32}
   Windows;
  {$ENDIF}

  {$IFDEF LINUX}
  Libc, ACS_Procs;
  {$ENDIF}

var

  LibFLACLoaded : Boolean = False;

const

 {$IFDEF WIN32}
  LibFLACPath = 'libFLAC.dll';
 {$ENDIF}

 {$IFDEF LINUX}
  LibFLACPath = 'libFLAC.so*'; // libFLAC.so
  {$DEFINE SEARCH_LIBS}
 {$ENDIF}

  FLAC__SEEKABLE_STREAM_ENCODER_OK = 0;
  FLAC__SEEKABLE_STREAM_ENCODER_STREAM_ENCODER_ERROR = 1;
  FLAC__SEEKABLE_STREAM_ENCODER_MEMORY_ALLOCATION_ERROR = 2;
  FLAC__SEEKABLE_STREAM_ENCODER_WRITE_ERROR = 3;
  FLAC__SEEKABLE_STREAM_ENCODER_READ_ERROR = 4;
  FLAC__SEEKABLE_STREAM_ENCODER_SEEK_ERROR = 5;
  FLAC__SEEKABLE_STREAM_ENCODER_ALREADY_INITIALIZED = 6;
  FLAC__SEEKABLE_STREAM_ENCODER_INVALID_CALLBACK = 7;
  FLAC__SEEKABLE_STREAM_ENCODER_INVALID_SEEKTABLE = 8;
  FLAC__SEEKABLE_STREAM_ENCODER_UNINITIALIZED = 9;

  FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_OK = 0;
  FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_ERROR = 1;

  FLAC__SEEKABLE_STREAM_DECODER_OK = 0;
  FLAC__SEEKABLE_STREAM_DECODER_SEEKING = 1;
  FLAC__SEEKABLE_STREAM_DECODER_END_OF_STREAM = 2;
  FLAC__SEEKABLE_STREAM_DECODER_MEMORY_ALLOCATION_ERROR = 3;
  FLAC__SEEKABLE_STREAM_DECODER_STREAM_DECODER_ERROR = 4;
  FLAC__SEEKABLE_STREAM_DECODER_READ_ERROR = 5;
  FLAC__SEEKABLE_STREAM_DECODER_SEEK_ERROR = 6;
  FLAC__SEEKABLE_STREAM_DECODER_ALREADY_INITIALIZED = 7;
  FLAC__SEEKABLE_STREAM_DECODER_INVALID_CALLBACK = 8;
  FLAC__SEEKABLE_STREAM_DECODER_UNINITIALIZED = 10;

  FLAC__SEEKABLE_STREAM_DECODER_READ_STATUS_OK = 0;
  FLAC__SEEKABLE_STREAM_DECODER_READ_STATUS_ERROR = 1;

  FLAC__SEEKABLE_STREAM_DECODER_SEEK_STATUS_OK = 0;
  FLAC__SEEKABLE_STREAM_DECODER_SEEK_STATUS_ERROR = 1;

  FLAC__SEEKABLE_STREAM_DECODER_TELL_STATUS_OK = 0;
  FLAC__SEEKABLE_STREAM_DECODER_TELL_STATUS_ERROR = 1;

  FLAC__SEEKABLE_STREAM_DECODER_LENGTH_STATUS_OK = 0;
  FLAC__SEEKABLE_STREAM_DECODER_LENGTH_STATUS_ERROR = 1;

type

  FLAC__byte = Byte;
  PFLAC__byte = ^FLAC__byte;

  FLAC__uint64 = Int64;
  PFLAC__uint64 = ^FLAC__uint64;

  FLAC__uint32 = LongWord;
  PFLAC__uint32 = ^FLAC__uint32;

  FLAC__int32 = Integer;
  PFLAC__int32 = ^FLAC__int32;

  PFLAC__SeekableStreamEncoder = Pointer;

  PFLAC__SeekableStreamDecoder = Pointer;

  FLAC__MetadataType = Integer;

  PFLAC__Frame = Pointer;

  FLAC__FrameHeader = packed record
    blocksize : LongWord;
    sample_rate : LongWord;
    channels : LongWord;
    channel_assignment : Integer;
    bits_per_sample : LongWord;
    frame_number : FLAC__uint32;
    crc : Byte;
  end;

  FLACInfo = record
    min_blocksize : LongWord;
    max_blocksize : LongWord;
    min_framesize : LongWord;
    max_framesize : LongWord;
    sample_rate : LongWord;
    channels : LongWord;
    bits_per_sample : LongWord;
    total_samples1 : LongWord;
    total_samples2 : LongWord;
  end;

  PFLACInfo = ^FLACInfo;

  PFLAC__StreamMetadata = Pointer;

  PFLAC__FrameHeader = ^FLAC__FrameHeader;

  FLACIntBuf = array[0..0] of FLAC__int32;
  PFLACIntBuf = ^FLACIntBuf;

  FLACChannels = array[0..1] of PFLACIntBuf;
  PFLACChannels = ^FLACChannels;

  FLAC__SeekableStreamEncoderWriteCallback  = function(encoder : PFLAC__SeekableStreamEncoder;
                                              buffer : PFLAC__byte;
                                              bytes,
                                              samples,
                                              current_frame : LongWord;
                                              client_data : Pointer) : Integer; cdecl;


  FLAC__SeekableStreamEncoderSeekCallback  = function(encoder : PFLAC__SeekableStreamEncoder;
                                             absolute_byte_offset : FLAC__uint64;
                                             client_data : Pointer) : Integer; cdecl;

  FLAC__SeekableStreamDecoderReadCallback = function(decoder : PFLAC__SeekableStreamDecoder;
                                                     buffer : PFLAC__byte;
                                                     var bytes : LongWord;
                                                     client_data : Pointer) : Integer; cdecl;

  FLAC__SeekableStreamDecoderSeekCallback = function(decoder : PFLAC__SeekableStreamDecoder;
                                                     absolute_byte_offset : FLAC__uint64;
                                                     client_data : Pointer) : Integer; cdecl;

  FLAC__SeekableStreamDecoderTellCallback = function(decoder : PFLAC__SeekableStreamDecoder;
                                                     var absolute_byte_offset : FLAC__uint64;
                                                     client_data : Pointer) : Integer; cdecl;

  FLAC__SeekableStreamDecoderLengthCallback = function(decoder : PFLAC__SeekableStreamDecoder;
                                                       var stream_length : FLAC__uint64;
                                                       client_data : Pointer) : Integer; cdecl;

  FLAC__SeekableStreamDecoderEofCallback = function(decoder : PFLAC__SeekableStreamDecoder;
                                                    client_data : Pointer) : Boolean; cdecl;

  FLAC__SeekableStreamDecoderWriteCallback = function(decoder : PFLAC__SeekableStreamDecoder;
                                                      frame : PFLAC__Frame;
                                                      buffer : PFLACChannels;
                                                      client_data : Pointer) : Integer; cdecl;

  FLAC__SeekableStreamDecoderMetadataCallback = procedure(decoder : PFLAC__SeekableStreamDecoder;
                                                          metadata : PFLAC__StreamMetadata;
                                                          client_data : Pointer); cdecl;

  FLAC__SeekableStreamDecoderErrorCallback = procedure(decoder : PFLAC__SeekableStreamDecoder;
                                                      status : Integer;
                                                      client_data : Pointer); cdecl;

  FLAC__seekable_stream_encoder_new_t = function : PFLAC__SeekableStreamEncoder; cdecl;
  FLAC__seekable_stream_encoder_delete_t = procedure(encoder : PFLAC__SeekableStreamEncoder); cdecl;
  FLAC__seekable_stream_encoder_set_verify_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_streamable_subset_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_do_mid_side_stereo_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_loose_mid_side_stereo_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_channels_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_bits_per_sample_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_sample_rate_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_blocksize_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_max_lpc_order_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_qlp_coeff_precision_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_do_escape_coding_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_do_exhaustive_model_search_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_min_residual_partition_order_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_max_residual_partition_order_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_rice_parameter_search_dist_t = function( encoder : PFLAC__SeekableStreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_total_samples_estimate_t = function( encoder : PFLAC__SeekableStreamEncoder; value : FLAC__uint64) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_metadata_t = function( encoder : PFLAC__SeekableStreamEncoder; metadata : Pointer; num_blocks : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_seek_callback_t = function( encoder : PFLAC__SeekableStreamEncoder; value : FLAC__SeekableStreamEncoderSeekCallback) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_write_callback_t = function( encoder : PFLAC__SeekableStreamEncoder; value : FLAC__SeekableStreamEncoderWriteCallback) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_set_client_data_t = function( encoder : PFLAC__SeekableStreamEncoder; value : Pointer) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_state_t = function( encoder : PFLAC__SeekableStreamEncoder) : Integer; cdecl;
  FLAC__seekable_stream_encoder_get_stream_encoder_state_t = function( encoder : PFLAC__SeekableStreamEncoder) : Integer; cdecl;
  FLAC__seekable_stream_encoder_get_verify_decoder_state_t = function( encoder : PFLAC__SeekableStreamEncoder) : Integer; cdecl;
  FLAC__seekable_stream_encoder_get_resolved_state_string_t = function( encoder : PFLAC__SeekableStreamEncoder) : PChar; cdecl;
  FLAC__seekable_stream_encoder_get_verify_decoder_error_stats_t = procedure(encoder : PFLAC__SeekableStreamEncoder; absolute_sample : PFLAC__uint64; frame_number, channel, sample : PLongWord; expected, got : FLAC__int32);  cdecl;
  FLAC__seekable_stream_encoder_get_verify_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_streamable_subset_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_do_mid_side_stereo_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_loose_mid_side_stereo_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_channels_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_bits_per_sample_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_sample_rate_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_blocksize_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_max_lpc_order_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_qlp_coeff_precision_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_do_escape_coding_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_do_exhaustive_model_search_t = function( encoder : PFLAC__SeekableStreamEncoder) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_get_min_residual_partition_order_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_max_residual_partition_order_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_rice_parameter_search_dist_t = function( encoder : PFLAC__SeekableStreamEncoder) : LongWord; cdecl;
  FLAC__seekable_stream_encoder_get_total_samples_estimate_t = function( encoder : PFLAC__SeekableStreamEncoder) : FLAC__uint64 cdecl;
  FLAC__seekable_stream_encoder_init_t = function( encoder : PFLAC__SeekableStreamEncoder) : Integer; cdecl;
  FLAC__seekable_stream_encoder_finish_t = procedure(encoder : PFLAC__SeekableStreamEncoder); cdecl;
  FLAC__seekable_stream_encoder_process_t = function(encoder : PFLAC__SeekableStreamEncoder; buffer : PFLAC__int32; samples : LongWord) : Boolean; cdecl;
  FLAC__seekable_stream_encoder_process_interleaved_t = function(encoder : PFLAC__SeekableStreamEncoder; buffer : PFLAC__int32; samples : LongWord) : Boolean; cdecl;

  FLAC__seekable_stream_decoder_new_t = function : PFLAC__SeekableStreamDecoder; cdecl;
  FLAC__seekable_stream_decoder_delete_t = procedure(decoder : PFLAC__SeekableStreamDecoder); cdecl;
  FLAC__seekable_stream_decoder_set_md5_checking_t = function( decoder : PFLAC__SeekableStreamDecoder; value : Boolean) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_read_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderReadCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_seek_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderSeekCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_tell_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderTellCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_length_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderLengthCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_eof_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderEofCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_write_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderWriteCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderMetadataCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_error_callback_t = function( decoder : PFLAC__SeekableStreamDecoder; value : FLAC__SeekableStreamDecoderErrorCallback) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_client_data_t = function( decoder : PFLAC__SeekableStreamDecoder; value : Pointer) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_respond_t = function( decoder : PFLAC__SeekableStreamDecoder; _type : FLAC__MetadataType) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_respond_application_t = function( decoder : PFLAC__SeekableStreamDecoder; id : PFLAC__byte) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_respond_all_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_ignore_t = function( decoder : PFLAC__SeekableStreamDecoder; _type : FLAC__MetadataType) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_ignore_application_t = function( decoder : PFLAC__SeekableStreamDecoder; id : PFLAC__byte) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_set_metadata_ignore_all_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_get_state_t = function( decoder : PFLAC__SeekableStreamDecoder) : Integer; cdecl;
  FLAC__seekable_stream_decoder_get_stream_decoder_state_t = function( decoder : PFLAC__SeekableStreamDecoder) : Integer; cdecl;
  FLAC__seekable_stream_decoder_get_resolved_state_string_t = function( decoder : PFLAC__SeekableStreamDecoder) : PChar; cdecl;
  FLAC__seekable_stream_decoder_get_md5_checking_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_get_channels_t = function( decoder : PFLAC__SeekableStreamDecoder) : Integer; cdecl;
  FLAC__seekable_stream_decoder_get_channel_assignment_t = function( decoder : PFLAC__SeekableStreamDecoder) : Integer; cdecl;
  FLAC__seekable_stream_decoder_get_bits_per_sample_t = function( decoder : PFLAC__SeekableStreamDecoder) : LongWord; cdecl;
  FLAC__seekable_stream_decoder_get_sample_rate_t = function( decoder : PFLAC__SeekableStreamDecoder) : LongWord; cdecl;
  FLAC__seekable_stream_decoder_get_blocksize_t = function( decoder : PFLAC__SeekableStreamDecoder) : LongWord; cdecl;
  FLAC__seekable_stream_decoder_get_decode_position_t = function( decoder : PFLAC__SeekableStreamDecoder; var position : FLAC__uint64) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_init_t = function( decoder : PFLAC__SeekableStreamDecoder) : Integer; cdecl;
  FLAC__seekable_stream_decoder_finish_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_flush_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_reset_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_process_single_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_process_until_end_of_metadata_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_process_until_end_of_stream_t = function( decoder : PFLAC__SeekableStreamDecoder) : Boolean; cdecl;
  FLAC__seekable_stream_decoder_seek_absolute_t = function( decoder : PFLAC__SeekableStreamDecoder; sample : FLAC__uint64) : Boolean; cdecl;

var

  FLAC__seekable_stream_encoder_new : FLAC__seekable_stream_encoder_new_t;
  FLAC__seekable_stream_encoder_delete : FLAC__seekable_stream_encoder_delete_t;
  FLAC__seekable_stream_encoder_set_verify : FLAC__seekable_stream_encoder_set_verify_t;
  FLAC__seekable_stream_encoder_set_streamable_subset : FLAC__seekable_stream_encoder_set_streamable_subset_t;
  FLAC__seekable_stream_encoder_set_do_mid_side_stereo : FLAC__seekable_stream_encoder_set_do_mid_side_stereo_t;
  FLAC__seekable_stream_encoder_set_loose_mid_side_stereo : FLAC__seekable_stream_encoder_set_loose_mid_side_stereo_t;
  FLAC__seekable_stream_encoder_set_channels : FLAC__seekable_stream_encoder_set_channels_t;
  FLAC__seekable_stream_encoder_set_bits_per_sample : FLAC__seekable_stream_encoder_set_bits_per_sample_t;
  FLAC__seekable_stream_encoder_set_sample_rate : FLAC__seekable_stream_encoder_set_sample_rate_t;
  FLAC__seekable_stream_encoder_set_blocksize : FLAC__seekable_stream_encoder_set_blocksize_t;
  FLAC__seekable_stream_encoder_set_max_lpc_order : FLAC__seekable_stream_encoder_set_max_lpc_order_t;
  FLAC__seekable_stream_encoder_set_qlp_coeff_precision : FLAC__seekable_stream_encoder_set_qlp_coeff_precision_t;
  FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search : FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search_t;
  FLAC__seekable_stream_encoder_set_do_escape_coding : FLAC__seekable_stream_encoder_set_do_escape_coding_t;
  FLAC__seekable_stream_encoder_set_do_exhaustive_model_search : FLAC__seekable_stream_encoder_set_do_exhaustive_model_search_t;
  FLAC__seekable_stream_encoder_set_min_residual_partition_order : FLAC__seekable_stream_encoder_set_min_residual_partition_order_t;
  FLAC__seekable_stream_encoder_set_max_residual_partition_order : FLAC__seekable_stream_encoder_set_max_residual_partition_order_t;
//  FLAC__seekable_stream_encoder_set_rice_parameter_search_dist : FLAC__seekable_stream_encoder_set_rice_parameter_search_dist_t;
  FLAC__seekable_stream_encoder_set_total_samples_estimate : FLAC__seekable_stream_encoder_set_total_samples_estimate_t;
  FLAC__seekable_stream_encoder_set_metadata : FLAC__seekable_stream_encoder_set_metadata_t;
  FLAC__seekable_stream_encoder_set_seek_callback : FLAC__seekable_stream_encoder_set_seek_callback_t;
  FLAC__seekable_stream_encoder_set_write_callback : FLAC__seekable_stream_encoder_set_write_callback_t;
  FLAC__seekable_stream_encoder_set_client_data : FLAC__seekable_stream_encoder_set_client_data_t;
  FLAC__seekable_stream_encoder_get_state : FLAC__seekable_stream_encoder_get_state_t;
  FLAC__seekable_stream_encoder_get_stream_encoder_state : FLAC__seekable_stream_encoder_get_stream_encoder_state_t;
  FLAC__seekable_stream_encoder_get_verify_decoder_state : FLAC__seekable_stream_encoder_get_verify_decoder_state_t;
  FLAC__seekable_stream_encoder_get_resolved_state_string : FLAC__seekable_stream_encoder_get_resolved_state_string_t;
  FLAC__seekable_stream_encoder_get_verify_decoder_error_stats : FLAC__seekable_stream_encoder_get_verify_decoder_error_stats_t;
  FLAC__seekable_stream_encoder_get_verify : FLAC__seekable_stream_encoder_get_verify_t;
  FLAC__seekable_stream_encoder_get_streamable_subset : FLAC__seekable_stream_encoder_get_streamable_subset_t;
  FLAC__seekable_stream_encoder_get_do_mid_side_stereo : FLAC__seekable_stream_encoder_get_do_mid_side_stereo_t;
  FLAC__seekable_stream_encoder_get_loose_mid_side_stereo : FLAC__seekable_stream_encoder_get_loose_mid_side_stereo_t;
  FLAC__seekable_stream_encoder_get_channels : FLAC__seekable_stream_encoder_get_channels_t;
  FLAC__seekable_stream_encoder_get_bits_per_sample : FLAC__seekable_stream_encoder_get_bits_per_sample_t;
  FLAC__seekable_stream_encoder_get_sample_rate : FLAC__seekable_stream_encoder_get_sample_rate_t;
  FLAC__seekable_stream_encoder_get_blocksize : FLAC__seekable_stream_encoder_get_blocksize_t;
  FLAC__seekable_stream_encoder_get_max_lpc_order : FLAC__seekable_stream_encoder_get_max_lpc_order_t;
  FLAC__seekable_stream_encoder_get_qlp_coeff_precision : FLAC__seekable_stream_encoder_get_qlp_coeff_precision_t;
  FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search : FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search_t;
  FLAC__seekable_stream_encoder_get_do_escape_coding : FLAC__seekable_stream_encoder_get_do_escape_coding_t;
  FLAC__seekable_stream_encoder_get_do_exhaustive_model_search : FLAC__seekable_stream_encoder_get_do_exhaustive_model_search_t;
  FLAC__seekable_stream_encoder_get_min_residual_partition_order : FLAC__seekable_stream_encoder_get_min_residual_partition_order_t;
  FLAC__seekable_stream_encoder_get_max_residual_partition_order : FLAC__seekable_stream_encoder_get_max_residual_partition_order_t;
// FLAC__seekable_stream_encoder_get_rice_parameter_search_dist  : FLAC__seekable_stream_encoder_get_rice_parameter_search_dist_t;
  FLAC__seekable_stream_encoder_get_total_samples_estimate : FLAC__seekable_stream_encoder_get_total_samples_estimate_t;
  FLAC__seekable_stream_encoder_init : FLAC__seekable_stream_encoder_init_t;
  FLAC__seekable_stream_encoder_finish : FLAC__seekable_stream_encoder_finish_t;
  FLAC__seekable_stream_encoder_process : FLAC__seekable_stream_encoder_process_t;
  FLAC__seekable_stream_encoder_process_interleaved : FLAC__seekable_stream_encoder_process_interleaved_t;

  FLAC__seekable_stream_decoder_new : FLAC__seekable_stream_decoder_new_t;
  FLAC__seekable_stream_decoder_delete : FLAC__seekable_stream_decoder_delete_t;
  FLAC__seekable_stream_decoder_set_md5_checking : FLAC__seekable_stream_decoder_set_md5_checking_t;
  FLAC__seekable_stream_decoder_set_read_callback : FLAC__seekable_stream_decoder_set_read_callback_t;
  FLAC__seekable_stream_decoder_set_seek_callback : FLAC__seekable_stream_decoder_set_seek_callback_t;
  FLAC__seekable_stream_decoder_set_tell_callback : FLAC__seekable_stream_decoder_set_tell_callback_t;
  FLAC__seekable_stream_decoder_set_length_callback : FLAC__seekable_stream_decoder_set_length_callback_t;
  FLAC__seekable_stream_decoder_set_eof_callback : FLAC__seekable_stream_decoder_set_eof_callback_t;
  FLAC__seekable_stream_decoder_set_write_callback : FLAC__seekable_stream_decoder_set_write_callback_t;
  FLAC__seekable_stream_decoder_set_metadata_callback : FLAC__seekable_stream_decoder_set_metadata_callback_t;
  FLAC__seekable_stream_decoder_set_error_callback : FLAC__seekable_stream_decoder_set_error_callback_t;
  FLAC__seekable_stream_decoder_set_client_data : FLAC__seekable_stream_decoder_set_client_data_t;
  FLAC__seekable_stream_decoder_set_metadata_respond : FLAC__seekable_stream_decoder_set_metadata_respond_t;
  FLAC__seekable_stream_decoder_set_metadata_respond_application : FLAC__seekable_stream_decoder_set_metadata_respond_application_t;
  FLAC__seekable_stream_decoder_set_metadata_respond_all : FLAC__seekable_stream_decoder_set_metadata_respond_all_t;
  FLAC__seekable_stream_decoder_set_metadata_ignore : FLAC__seekable_stream_decoder_set_metadata_ignore_t;
  FLAC__seekable_stream_decoder_set_metadata_ignore_application : FLAC__seekable_stream_decoder_set_metadata_ignore_application_t;
  FLAC__seekable_stream_decoder_set_metadata_ignore_all : FLAC__seekable_stream_decoder_set_metadata_ignore_all_t;
  FLAC__seekable_stream_decoder_get_state : FLAC__seekable_stream_decoder_get_state_t;
  FLAC__seekable_stream_decoder_get_stream_decoder_state : FLAC__seekable_stream_decoder_get_stream_decoder_state_t;
  FLAC__seekable_stream_decoder_get_resolved_state_string : FLAC__seekable_stream_decoder_get_resolved_state_string_t;
  FLAC__seekable_stream_decoder_get_md5_checking : FLAC__seekable_stream_decoder_get_md5_checking_t;
  FLAC__seekable_stream_decoder_get_channels : FLAC__seekable_stream_decoder_get_channels_t;
  FLAC__seekable_stream_decoder_get_channel_assignment : FLAC__seekable_stream_decoder_get_channel_assignment_t;
  FLAC__seekable_stream_decoder_get_bits_per_sample : FLAC__seekable_stream_decoder_get_bits_per_sample_t;
  FLAC__seekable_stream_decoder_get_sample_rate : FLAC__seekable_stream_decoder_get_sample_rate_t;
  FLAC__seekable_stream_decoder_get_blocksize : FLAC__seekable_stream_decoder_get_blocksize_t;
  FLAC__seekable_stream_decoder_get_decode_position : FLAC__seekable_stream_decoder_get_decode_position_t;
  FLAC__seekable_stream_decoder_init : FLAC__seekable_stream_decoder_init_t;
  FLAC__seekable_stream_decoder_finish : FLAC__seekable_stream_decoder_finish_t;
  FLAC__seekable_stream_decoder_flush : FLAC__seekable_stream_decoder_flush_t;
  FLAC__seekable_stream_decoder_reset : FLAC__seekable_stream_decoder_reset_t;
  FLAC__seekable_stream_decoder_process_single : FLAC__seekable_stream_decoder_process_single_t;
  FLAC__seekable_stream_decoder_process_until_end_of_metadata : FLAC__seekable_stream_decoder_process_until_end_of_metadata_t;
  FLAC__seekable_stream_decoder_process_until_end_of_stream : FLAC__seekable_stream_decoder_process_until_end_of_stream_t;
  FLAC__seekable_stream_decoder_seek_absolute : FLAC__seekable_stream_decoder_seek_absolute_t;

implementation

{$IFDEF WIN32}

var
  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(LibFLACPath, 0, 0);

  if Libhandle <> 0 then
  begin
    LibFLACLoaded := True;

    FLAC__seekable_stream_encoder_new := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_new');
    FLAC__seekable_stream_encoder_delete := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_delete');
    FLAC__seekable_stream_encoder_set_verify := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_verify');
    FLAC__seekable_stream_encoder_set_streamable_subset := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_streamable_subset');
    FLAC__seekable_stream_encoder_set_do_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_do_mid_side_stereo');
    FLAC__seekable_stream_encoder_set_loose_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_loose_mid_side_stereo');
    FLAC__seekable_stream_encoder_set_channels := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_channels');
    FLAC__seekable_stream_encoder_set_bits_per_sample := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_bits_per_sample');
    FLAC__seekable_stream_encoder_set_sample_rate := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_sample_rate');
    FLAC__seekable_stream_encoder_set_blocksize := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_blocksize');
    FLAC__seekable_stream_encoder_set_max_lpc_order := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_max_lpc_order');
    FLAC__seekable_stream_encoder_set_qlp_coeff_precision := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_qlp_coeff_precision');
    FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search');
    FLAC__seekable_stream_encoder_set_do_escape_coding := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_do_escape_coding');
    FLAC__seekable_stream_encoder_set_do_exhaustive_model_search := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_do_exhaustive_model_search');
    FLAC__seekable_stream_encoder_set_min_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_min_residual_partition_order');
    FLAC__seekable_stream_encoder_set_max_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_max_residual_partition_order');
  //    FLAC__seekable_stream_encoder_set_rice_parameter_search_dist := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_rice_parameter_search_dist');
    FLAC__seekable_stream_encoder_set_total_samples_estimate := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_total_samples_estimate');
    FLAC__seekable_stream_encoder_set_metadata := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_metadata');
    FLAC__seekable_stream_encoder_set_seek_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_seek_callback');
    FLAC__seekable_stream_encoder_set_write_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_write_callback');
    FLAC__seekable_stream_encoder_set_client_data := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_set_client_data');
    FLAC__seekable_stream_encoder_get_state := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_state');
    FLAC__seekable_stream_encoder_get_stream_encoder_state := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_stream_encoder_state');
    FLAC__seekable_stream_encoder_get_verify_decoder_state := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_verify_decoder_state');
    FLAC__seekable_stream_encoder_get_resolved_state_string := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_resolved_state_string');
    FLAC__seekable_stream_encoder_get_verify_decoder_error_stats := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_verify_decoder_error_stats');
    FLAC__seekable_stream_encoder_get_verify := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_verify');
    FLAC__seekable_stream_encoder_get_streamable_subset := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_streamable_subset');
    FLAC__seekable_stream_encoder_get_do_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_do_mid_side_stereo');
    FLAC__seekable_stream_encoder_get_loose_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_loose_mid_side_stereo');
    FLAC__seekable_stream_encoder_get_channels := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_channels');
    FLAC__seekable_stream_encoder_get_bits_per_sample := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_bits_per_sample');
    FLAC__seekable_stream_encoder_get_sample_rate := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_sample_rate');
    FLAC__seekable_stream_encoder_get_blocksize := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_blocksize');
    FLAC__seekable_stream_encoder_get_max_lpc_order := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_max_lpc_order');
    FLAC__seekable_stream_encoder_get_qlp_coeff_precision := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_qlp_coeff_precision');
    FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search');
    FLAC__seekable_stream_encoder_get_do_escape_coding := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_do_escape_coding');
    FLAC__seekable_stream_encoder_get_do_exhaustive_model_search := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_do_exhaustive_model_search');
    FLAC__seekable_stream_encoder_get_min_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_min_residual_partition_order');
    FLAC__seekable_stream_encoder_get_max_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_max_residual_partition_order');
  // FLAC__seekable_stream_encoder_get_rice_parameter_search_dist    := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_rice_parameter_search_dist');
    FLAC__seekable_stream_encoder_get_total_samples_estimate := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_get_total_samples_estimate');
    FLAC__seekable_stream_encoder_init := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_init');
    FLAC__seekable_stream_encoder_finish := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_finish');
    FLAC__seekable_stream_encoder_process := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_process');
    FLAC__seekable_stream_encoder_process_interleaved := GetProcAddress(Libhandle, 'FLAC__seekable_stream_encoder_process_interleaved');

    FLAC__seekable_stream_decoder_new := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_new');
    FLAC__seekable_stream_decoder_delete := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_delete');
    FLAC__seekable_stream_decoder_set_md5_checking := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_md5_checking');
    FLAC__seekable_stream_decoder_set_read_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_read_callback');
    FLAC__seekable_stream_decoder_set_seek_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_seek_callback');
    FLAC__seekable_stream_decoder_set_tell_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_tell_callback');
    FLAC__seekable_stream_decoder_set_length_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_length_callback');
    FLAC__seekable_stream_decoder_set_eof_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_eof_callback');
    FLAC__seekable_stream_decoder_set_write_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_write_callback');
    FLAC__seekable_stream_decoder_set_metadata_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_callback');
    FLAC__seekable_stream_decoder_set_error_callback := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_error_callback');
    FLAC__seekable_stream_decoder_set_client_data := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_client_data');
    FLAC__seekable_stream_decoder_set_metadata_respond := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_respond');
    FLAC__seekable_stream_decoder_set_metadata_respond_application := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_respond_application');
    FLAC__seekable_stream_decoder_set_metadata_respond_all := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_respond_all');
    FLAC__seekable_stream_decoder_set_metadata_ignore := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_ignore');
    FLAC__seekable_stream_decoder_set_metadata_ignore_application := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_ignore_application');
    FLAC__seekable_stream_decoder_set_metadata_ignore_all := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_ignore_all');
    FLAC__seekable_stream_decoder_get_state := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_state');
    FLAC__seekable_stream_decoder_get_stream_decoder_state := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_stream_decoder_state');
    FLAC__seekable_stream_decoder_get_resolved_state_string := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_resolved_state_string');
    FLAC__seekable_stream_decoder_get_md5_checking := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_md5_checking');
    FLAC__seekable_stream_decoder_get_channels := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_channels');
    FLAC__seekable_stream_decoder_get_channel_assignment := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_channel_assignment');
    FLAC__seekable_stream_decoder_get_bits_per_sample := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_bits_per_sample');
    FLAC__seekable_stream_decoder_get_sample_rate := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_sample_rate');
    FLAC__seekable_stream_decoder_get_blocksize := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_blocksize');
    FLAC__seekable_stream_decoder_get_decode_position := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_get_decode_position');
    FLAC__seekable_stream_decoder_init := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_init');
    FLAC__seekable_stream_decoder_finish := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_finish');
    FLAC__seekable_stream_decoder_flush := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_flush');
    FLAC__seekable_stream_decoder_reset := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_reset');
    FLAC__seekable_stream_decoder_process_single := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_process_single');
    FLAC__seekable_stream_decoder_process_until_end_of_metadata := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_process_until_end_of_metadata');
    FLAC__seekable_stream_decoder_process_until_end_of_stream := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_process_until_end_of_stream');
    FLAC__seekable_stream_decoder_seek_absolute := GetProcAddress(Libhandle, 'FLAC__seekable_stream_decoder_seek_absolute');

  end;

finalization

  if Libhandle <> 0 then FreeLibrary(Libhandle);

 {$ENDIF}

 {$IFDEF LINUX}

var
  Libhandle : Pointer;

{$IFDEF SEARCH_LIBS}
  Path : String;
{$ENDIF}

initialization

{$IFDEF SEARCH_LIBS}

  Libhandle := nil;
  Path := FindLibs(LibFLACPath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(LibFLACPath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin
    LibFLACLoaded := True;

    FLAC__seekable_stream_encoder_new := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_new');
    FLAC__seekable_stream_encoder_delete := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_delete');
    FLAC__seekable_stream_encoder_set_verify := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_verify');
    FLAC__seekable_stream_encoder_set_streamable_subset := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_streamable_subset');
    FLAC__seekable_stream_encoder_set_do_mid_side_stereo := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_do_mid_side_stereo');
    FLAC__seekable_stream_encoder_set_loose_mid_side_stereo := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_loose_mid_side_stereo');
    FLAC__seekable_stream_encoder_set_channels := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_channels');
    FLAC__seekable_stream_encoder_set_bits_per_sample := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_bits_per_sample');
    FLAC__seekable_stream_encoder_set_sample_rate := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_sample_rate');
    FLAC__seekable_stream_encoder_set_blocksize := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_blocksize');
    FLAC__seekable_stream_encoder_set_max_lpc_order := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_max_lpc_order');
    FLAC__seekable_stream_encoder_set_qlp_coeff_precision := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_qlp_coeff_precision');
    FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search');
    FLAC__seekable_stream_encoder_set_do_escape_coding := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_do_escape_coding');
    FLAC__seekable_stream_encoder_set_do_exhaustive_model_search := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_do_exhaustive_model_search');
    FLAC__seekable_stream_encoder_set_min_residual_partition_order := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_min_residual_partition_order');
    FLAC__seekable_stream_encoder_set_max_residual_partition_order := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_max_residual_partition_order');
  //    FLAC__seekable_stream_encoder_set_rice_parameter_search_dist := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_rice_parameter_search_dist');
    FLAC__seekable_stream_encoder_set_total_samples_estimate := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_total_samples_estimate');
    FLAC__seekable_stream_encoder_set_metadata := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_metadata');
    FLAC__seekable_stream_encoder_set_seek_callback := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_seek_callback');
    FLAC__seekable_stream_encoder_set_write_callback := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_write_callback');
    FLAC__seekable_stream_encoder_set_client_data := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_set_client_data');
    FLAC__seekable_stream_encoder_get_state := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_state');
    FLAC__seekable_stream_encoder_get_stream_encoder_state := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_stream_encoder_state');
    FLAC__seekable_stream_encoder_get_verify_decoder_state := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_verify_decoder_state');
    FLAC__seekable_stream_encoder_get_resolved_state_string := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_resolved_state_string');
    FLAC__seekable_stream_encoder_get_verify_decoder_error_stats := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_verify_decoder_error_stats');
    FLAC__seekable_stream_encoder_get_verify := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_verify');
    FLAC__seekable_stream_encoder_get_streamable_subset := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_streamable_subset');
    FLAC__seekable_stream_encoder_get_do_mid_side_stereo := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_do_mid_side_stereo');
    FLAC__seekable_stream_encoder_get_loose_mid_side_stereo := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_loose_mid_side_stereo');
    FLAC__seekable_stream_encoder_get_channels := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_channels');
    FLAC__seekable_stream_encoder_get_bits_per_sample := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_bits_per_sample');
    FLAC__seekable_stream_encoder_get_sample_rate := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_sample_rate');
    FLAC__seekable_stream_encoder_get_blocksize := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_blocksize');
    FLAC__seekable_stream_encoder_get_max_lpc_order := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_max_lpc_order');
    FLAC__seekable_stream_encoder_get_qlp_coeff_precision := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_qlp_coeff_precision');
    FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search');
    FLAC__seekable_stream_encoder_get_do_escape_coding := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_do_escape_coding');
    FLAC__seekable_stream_encoder_get_do_exhaustive_model_search := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_do_exhaustive_model_search');
    FLAC__seekable_stream_encoder_get_min_residual_partition_order := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_min_residual_partition_order');
    FLAC__seekable_stream_encoder_get_max_residual_partition_order := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_max_residual_partition_order');
  // FLAC__seekable_stream_encoder_get_rice_parameter_search_dist    := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_rice_parameter_search_dist');
    FLAC__seekable_stream_encoder_get_total_samples_estimate := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_get_total_samples_estimate');
    FLAC__seekable_stream_encoder_init := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_init');
    FLAC__seekable_stream_encoder_finish := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_finish');
    FLAC__seekable_stream_encoder_process := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_process');
    FLAC__seekable_stream_encoder_process_interleaved := dlsym(Libhandle, 'FLAC__seekable_stream_encoder_process_interleaved');

    FLAC__seekable_stream_decoder_new := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_new');
    FLAC__seekable_stream_decoder_delete := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_delete');
    FLAC__seekable_stream_decoder_set_md5_checking := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_md5_checking');
    FLAC__seekable_stream_decoder_set_read_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_read_callback');
    FLAC__seekable_stream_decoder_set_seek_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_seek_callback');
    FLAC__seekable_stream_decoder_set_tell_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_tell_callback');
    FLAC__seekable_stream_decoder_set_length_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_length_callback');
    FLAC__seekable_stream_decoder_set_eof_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_eof_callback');
    FLAC__seekable_stream_decoder_set_write_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_write_callback');
    FLAC__seekable_stream_decoder_set_metadata_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_callback');
    FLAC__seekable_stream_decoder_set_error_callback := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_error_callback');
    FLAC__seekable_stream_decoder_set_client_data := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_client_data');
    FLAC__seekable_stream_decoder_set_metadata_respond := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_respond');
    FLAC__seekable_stream_decoder_set_metadata_respond_application := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_respond_application');
    FLAC__seekable_stream_decoder_set_metadata_respond_all := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_respond_all');
    FLAC__seekable_stream_decoder_set_metadata_ignore := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_ignore');
    FLAC__seekable_stream_decoder_set_metadata_ignore_application := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_ignore_application');
    FLAC__seekable_stream_decoder_set_metadata_ignore_all := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_set_metadata_ignore_all');
    FLAC__seekable_stream_decoder_get_state := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_state');
    FLAC__seekable_stream_decoder_get_stream_decoder_state := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_stream_decoder_state');
    FLAC__seekable_stream_decoder_get_resolved_state_string := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_resolved_state_string');
    FLAC__seekable_stream_decoder_get_md5_checking := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_md5_checking');
    FLAC__seekable_stream_decoder_get_channels := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_channels');
    FLAC__seekable_stream_decoder_get_channel_assignment := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_channel_assignment');
    FLAC__seekable_stream_decoder_get_bits_per_sample := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_bits_per_sample');
    FLAC__seekable_stream_decoder_get_sample_rate := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_sample_rate');
    FLAC__seekable_stream_decoder_get_blocksize := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_blocksize');
    FLAC__seekable_stream_decoder_get_decode_position := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_get_decode_position');
    FLAC__seekable_stream_decoder_init := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_init');
    FLAC__seekable_stream_decoder_finish := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_finish');
    FLAC__seekable_stream_decoder_flush := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_flush');
    FLAC__seekable_stream_decoder_reset := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_reset');
    FLAC__seekable_stream_decoder_process_single := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_process_single');
    FLAC__seekable_stream_decoder_process_until_end_of_metadata := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_process_until_end_of_metadata');
    FLAC__seekable_stream_decoder_process_until_end_of_stream := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_process_until_end_of_stream');
    FLAC__seekable_stream_decoder_seek_absolute := dlsym(Libhandle, 'FLAC__seekable_stream_decoder_seek_absolute');

  end;

finalization

  if Libhandle <> nil then dlclose(Libhandle);

  {$ENDIF}

end.
