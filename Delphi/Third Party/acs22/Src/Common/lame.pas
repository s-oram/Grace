(*
  Delphi/Kylix header for LAME encoder.
  Translated from lame.h C/C++ header
  by Andrei Borovsky, aborovsky@mtu-net.ru
  The original C/C++ header and library
  Copyright (c) 1999 Mark Taylor

  Note by A.B.: Some fragments of the original header file are skipped.
  See the comments below.
*)

// $Id: lame.h,v 1.107 2002/04/10 12:55:47 bouvigne Exp $

unit lame;

interface

uses
{$IFDEF WIN32}
  Windows;
{$ENDIF}

{$IFDEF LINUX}
  Libc;
{$ENDIF}

const

{$IFDEF LINUX}
    LAME_PATH = '/usr/lib/libmp3lame.so';
{$ENDIF}

{$IFDEF WIN32}
    LAME_PATH = 'lame_enc.dll';
{$ENDIF}

type

  vbr_mode = (
    vbr_off = 0,
    vbr_mt,                    // obsolete, same as vbr_mtrh
    vbr_rh,
    vbr_abr,
    vbr_mtrh,
    vbr_max_indicator,        // Don't use this! It's used for sanity checks.
    vbr_default = vbr_rh      // change this to change the default VBR mode of LAME
  );

// MPEG modes

  MPEG_mode = (
    STEREO = 0,
    JOINT_STEREO,
    DUAL_CHANNEL,             // LAME doesn't supports this!
    MONO,
    NOT_SET,
    MAX_INDICATOR            // Don't use this! It's used for sanity checks.
  );

  Padding_type = (
    PAD_NO = 0,
    PAD_ALL,
    PAD_ADJUST,
    PAD_MAX_INDICATOR      // Don't use this! It's used for sanity checks.
  );

  preset_mode = (
    (* values from 8 to 320 should be reserved for abr bitrates
      for abr I'd suggest to directly use the targeted bitrate as a value *)
    ABR_8 = 8,
    ABR_320 = 320,
    R3MIX = 1000,
    STANDARD = 1001,
    EXTREME = 1002,
    INSANE = 1003,
    STANDARD_FAST = 1004,
    EXTREME_FAST = 1005
  );

  asm_optimizations = (
    MMX = 1,
    AMD_3DNOW = 2,
    SSE = 3
  );

(* Note by A.B.:
   The following type is a placeholder for * lame_global_flags.
   We treat this type as integer since it is interpreted as such by some
   functions and the underlying structure is opaque anyway.
*)

  PLame_global_flags = Integer;

  PLame_t = Pointer;

  // function pointers

  (***********************************************************************
  *
  *  The LAME API
  *  These functions should be called, in this order, for each
  *  MP3 file to be encoded
  *
  ***********************************************************************)

 (*
 * REQUIRED:
 * initialize the encoder.  sets default for all encoder paramters,
 * returns -1 if some malloc()'s failed
 * otherwise returns 0
 *)

 lame_init_t = function : PLame_global_flags; cdecl;

 (*
 * OPTIONAL:
 * set as needed to override defaults
 *)

(********************************************************************
 *  input stream description
 ***********************************************************************)
// number of samples.  default = 2^32-1

  lame_set_num_samples_t = function(handle : PLame_global_flags; ul : LongWord)
  : Integer; cdecl;
  lame_get_num_samples_t = function(handle : PLame_global_flags) : LongWord; cdecl;

  // input sample rate in Hz.  default = 44100hz
  lame_set_in_samplerate_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_in_samplerate_t = function(handle : PLame_global_flags) : Integer; cdecl;

  // number of channels in input stream. default=2
  lame_set_num_channels_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_num_channels_t = function(handle : PLame_global_flags) : Integer; cdecl;

  (* scale the input by this amount before encoding.  default=0 (disabled)
   (not used by decoding routines) *)
  lame_set_scale_t = function(handle : PLame_global_flags; d : Single)
  : Integer; cdecl;
  lame_get_scale_t = function(handle : PLame_global_flags) : Single; cdecl;

  (* scale the channel 0 (left) input by this amount before encoding.
   default=0 (disabled)
  (not used by decoding routines) *)
  lame_set_scale_left_t = function(handle : PLame_global_flags; d : Single)
  : Integer; cdecl;
  lame_get_scale_left_t = function(handle : PLame_global_flags) : Single; cdecl;

(* scale the channel 1 (right) input by this amount before encoding.
   default=0 (disabled)
 (not used by decoding routines) *)

  lame_set_scale_right_t = function(handle : PLame_global_flags; d : Single)
  : Integer; cdecl;
  lame_get_scale_right_t = function(handle : PLame_global_flags) : Single; cdecl;

  (* output sample rate in Hz.  default = 0, which means LAME picks best value
     based on the amount of compression.  MPEG only allows:
   MPEG1    32, 44.1,   48khz
   MPEG2    16, 22.05,  24
   MPEG2.5   8, 11.025, 12
  (not used by decoding routines) *)

  lame_set_out_samplerate_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_out_samplerate_t = function(handle : PLame_global_flags) : Integer; cdecl;

 (********************************************************************
 *  general control parameters
 ***********************************************************************)
// 1=cause LAME to collect data for an MP3 frame analyzer. default=0
  lame_set_analysis_t  = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_analysis_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* 1 = write a Xing VBR header frame.
 default = 1 for VBR/ABR modes, 0 for CBR mode
 this variable must have been added by a Hungarian notation Windows programmer :-) *)
 lame_set_bWriteVbrTag_t  = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_bWriteVbrTag_t = function(handle : PLame_global_flags) : Integer; cdecl;

  // 1=decode only.  use lame/mpglib to convert mp3/ogg to wav.  default=0
  lame_set_decode_only_t  = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_decode_only_t = function(handle : PLame_global_flags) : Integer; cdecl;

  // 1=encode a Vorbis .ogg file.  default=0
  lame_set_ogg_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_ogg_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* internal algorithm selection.  True quality is determined by the bitrate
 but this variable will effect quality by selecting expensive or cheap algorithms.
 quality=0..9.  0=best (very slow).  9=worst.
 recommended:  2     near-best quality, not too slow
               5     good quality, fast
               7     ok quality, really fast *)
  lame_set_quality_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_quality_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* mode = 0,1,2,3 = stereo, jstereo, dual channel (not supported), mono
 default: lame picks based on compression ration and input channels *)
  lame_set_mode_t = function(handle : PLame_global_flags; MPEGMode : Integer)
  : Integer; cdecl;
  lame_get_mode_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* mode_automs.  Use a M/S mode with a switching threshold based on
 compression ratio
 default = 0 (disabled) *)
  lame_set_mode_automs_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_mode_automs_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* force_ms.  Force M/S for all frames.  For testing only.
 default = 0 (disabled) *)
  lame_set_force_ms_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_force_ms_t = function(handle : PLame_global_flags) : Integer; cdecl;

// use free_format?  default = 0 (disabled)
  lame_set_free_format_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_free_format_t = function(handle : PLame_global_flags) : Integer; cdecl;

// Note by A.B.: Attention! Skipping optional debug/error messages functions.

// set one of brate compression ratio.  default is compression ratio of 11.
  lame_set_brate_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_brate_t = function(handle : PLame_global_flags) : Integer; cdecl;
  lame_set_compression_ratio_t = function(handle : PLame_global_flags; d : Single)
  : Integer; cdecl;
  lame_get_compression_ratio_t = function(handle : PLame_global_flags) : Single; cdecl;

  lame_set_preset_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_set_asm_optimizations_t = function(handle : PLame_global_flags; i1, i2 : Integer)
  : Integer; cdecl;

 (********************************************************************
 *  frame params
 ***********************************************************************)
// mark as copyright.  default=0
  lame_set_copyright_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_copyright_t = function(handle : PLame_global_flags) : Integer; cdecl;

// mark as original.  default=1
  lame_set_original_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_original_t = function(handle : PLame_global_flags) : Integer; cdecl;

// error_protection.  Use 2 bytes from each frame for CRC checksum. default=0
  lame_set_error_protection_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_error_protection_t = function(handle : PLame_global_flags) : Integer; cdecl;

// padding_type.  0=pad no frames  1=pad all frames 2=adjust padding(default)
  lame_set_padding_type_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_padding_type_t = function(handle : PLame_global_flags) : Integer; cdecl;

// MP3 'private extension' bit  Meaningless.  default=0
  lame_set_extension_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_extension_t = function(handle : PLame_global_flags) : Integer; cdecl;

// enforce strict ISO compliance.  default=0
  lame_set_strict_ISO_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_strict_ISO_t = function(handle : PLame_global_flags) : Integer; cdecl;

  (********************************************************************
   * VBR control
   ***********************************************************************)

// Types of VBR.  default = vbr_off = CBR
   lame_set_VBR_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_VBR_t = function(handle : PLame_global_flags) : Integer; cdecl;

// VBR quality level.  0=highest  9=lowest
  lame_set_VBR_q_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_VBR_q_t = function(handle : PLame_global_flags) : Integer; cdecl;

// Ignored except for VBR=vbr_abr (ABR mode)
  lame_set_VBR_mean_bitrate_kbps_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_VBR_mean_bitrate_kbps_t = function(handle : PLame_global_flags) : Integer; cdecl;

  lame_set_VBR_min_bitrate_kbps_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_VBR_min_bitrate_kbps_t = function(handle : PLame_global_flags) : Integer; cdecl;

  lame_set_VBR_max_bitrate_kbps_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_VBR_max_bitrate_kbps_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* 1=stricetly enforce VBR_min_bitrate.  Normally it will be violated for
 analog silence *)
  lame_set_VBR_hard_min_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_VBR_hard_min_t = function(handle : PLame_global_flags) : Integer; cdecl;

 (********************************************************************
 * Filtering control
 ***********************************************************************)
 // freq in Hz to apply lowpass. Default = 0 = lame chooses.  -1 = disabled
  lame_set_lowpassfreq_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_lowpassfreq_t = function(handle : PLame_global_flags) : Integer; cdecl;
// width of transition band, in Hz.  Default = one polyphase filter band
  lame_set_lowpasswidth_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_lowpasswidth_t = function(handle : PLame_global_flags) : Integer; cdecl;

// freq in Hz to apply highpass. Default = 0 = lame chooses.  -1 = disabled
  lame_set_highpassfreq_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_highpassfreq_t = function(handle : PLame_global_flags) : Integer; cdecl;
// width of transition band, in Hz.  Default = one polyphase filter band
  lame_set_highpasswidth_t = function(handle : PLame_global_flags; i : Integer)
  : Integer; cdecl;
  lame_get_highpasswidth_t = function(handle : PLame_global_flags) : Integer; cdecl;

// Note by A.B.: Attention! Skipping psycho acoustics and other special functions

(************************************************************************
* internal variables, cannot be set...                                 *
* provided because they may be of use to calling application           *
************************************************************************)
// version  0=MPEG-2  1=MPEG-1  (2=MPEG-2.5)
  lame_get_version_t = function(handle : PLame_global_flags) : Integer; cdecl;

// encoder delay
  lame_get_encoder_delay_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* padding appended to the input to make sure decoder can fully decode
   all input.  Note that this value can only be calculated during the
   call to lame_encoder_flush().  Before lame_encoder_flush() has
   been called, the value of encoder_padding = 0. *)
  lame_get_encoder_padding_t = function(handle : PLame_global_flags) : Integer; cdecl;

// size of MPEG frame
  lame_get_framesize_t = function(handle : PLame_global_flags) : Integer; cdecl;

// number of PCM samples buffered, but not yet encoded to mp3 data.
  lame_get_mf_samples_to_encode_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* size (bytes) of mp3 data buffered, but not yet encoded.
 this is the number of bytes which would be output by a call to
 lame_encode_flush_nogap.  NOTE: lame_encode_flush() will return
 more bytes than this because it will encode the reamining buffered
 PCM samples before flushing the mp3 buffers. *)
  lame_get_size_mp3buffer_t = function(handle : PLame_global_flags) : Integer; cdecl;

// number of frames encoded so far
  lame_get_frameNum_t = function(handle : PLame_global_flags) : Integer; cdecl;

(* lame's estimate of the total number of frames to be encoded
 only valid if calling program set num_samples *)
  lame_get_totalframes_t = function(handle : PLame_global_flags) : Integer; cdecl;

(*
 * REQUIRED:
 * sets more internal configuration based on data provided above.
 * returns -1 if something failed.
 *)

  lame_init_params_t = function(handle : PLame_global_flags) : Integer; cdecl;

(*
 * OPTIONAL:
 * get the version number, in a string. of the form:
 * "3.63 (beta)" or just "3.63".
 *)
  get_lame_version_t = function : PChar; cdecl;
  get_lame_short_version_t = function : PChar; cdecl;
  get_lame_very_short_version_t = function : PChar; cdecl;
  get_psy_version_t = function : PChar; cdecl;
  get_lame_url_t = function : PChar; cdecl;

(*
 * OPTIONAL:
 * get the version numbers in numerical form.
 *)

  lame_version_t = record
    // generic LAME version
    major : Integer;
    minor : Integer;
    alpha : Integer;               // 0 if not an alpha version
    beta : Integer;                // 0 if not a beta version

    // version of the psy model
    psy_major : Integer;
    psy_minor : Integer;
    psy_alpha : Integer;           // 0 if not an alpha version
    psy_beta : Integer;            // 0 if not a beta version

    // compile time features
    features : PChar;              // Don't make assumptions about the contents!
  end;

  get_lame_version_numerical_t = procedure(lnv : lame_version_t); cdecl;

// Note by A.B.: Attention! Skipping optional functions printing internal lame configuration

(*
 * input pcm data, output (maybe) mp3 frames.
 * This routine handles all buffering, resampling and filtering for you.
 *
 * return code     number of bytes output in mp3buf. Can be 0
 *                 -1:  mp3buf was too small
 *                 -2:  malloc() problem
 *                 -3:  lame_init_params() not called
 *                 -4:  psycho acoustic problems
 *                 -5:  ogg cleanup encoding error
 *                 -6:  ogg frame encoding error
 *
 * The required mp3buf_size can be computed from num_samples,
 * samplerate and encoding rate, but here is a worst case estimate:
 *
 * mp3buf_size in bytes = 1.25*num_samples + 7200
 *
 * I think a tighter bound could be:  (mt, March 2000)
 * MPEG1:
 *    num_samples*(bitrate/8)/samplerate + 4*1152*(bitrate/8)/samplerate + 512
 * MPEG2:
 *    num_samples*(bitrate/8)/samplerate + 4*576*(bitrate/8)/samplerate + 256
 *
 * but test first if you use that!
 *
 * set mp3buf_size = 0 and LAME will not check if mp3buf_size is
 * large enough.
 *
 * NOTE:
 * if gfp->num_channels=2, but gfp->mode = 3 (mono), the L & R channels
 * will be averaged into the L channel before encoding only the L channel
 * This will overwrite the data in buffer_l[] and buffer_r[].
 *
*)

  lame_encode_buffer_t = function(handle : PLame_global_flags;  // global context handle
                                  buffer_l : PShortInt;         // PCM data for left channel
                                  buffer_r : PShortInt;         // PCM data for right channel
                                  nsamples : Integer;           // number of samples per channel
                                  mp3buf : PByte;               // pointer to encoded MP3 stream
                                  mp3buf_size : Integer         // number of valid octets in this stream
                                 ) : Integer; cdecl;

(*
 * as above, but input has L & R channel data interleaved.
 * NOTE:
 * num_samples = number of samples in the L (or R)
 * channel, not the total number of samples in pcm[]
 *)
 lame_encode_buffer_interleaved_t = function(handle : PLame_global_flags;       // global context handle
                                             pcm : PShortInt;                   (* PCM data for left and right
                                                                                   channel, interleaved *)
                                             num_samples : Integer;             (* number of samples per channel,
                                                                                   _not_ number of samples in pcm[] *)
                                             mp3buf : PByte;                    // pointer to encoded MP3 stream
                                             mp3buf_size : Integer              // number of valid octets in this stream
                                            ) : Integer; cdecl;

(* as lame_encode_buffer, but for 'float's.
 * !! NOTE: !! data must still be scaled to be in the same range as
 * short int, +/- 32768
 *)
  lame_encode_buffer_float_t = function(handle : PLame_global_flags;       // global context handle
                             buffer_l : PSingle;                // PCM data for left channel
                             buffer_r : PSingle;                // PCM data for right channel
                             nsamples : Integer;                // number of samples per channel
                             mp3buf : PByte;                    // pointer to encoded MP3 stream
                             mp3buf_size : Integer              // number of valid octets in this stream
                            ) : Integer; cdecl;

// Note by A.B.: Attention! Skipping several variants of encoding functions

(*
 * REQUIRED:
 * lame_encode_flush will flush the intenal PCM buffers, padding with
 * 0's to make sure the final frame is complete, and then flush
 * the internal MP3 buffers, and thus may return a
 * final few mp3 frames.  'mp3buf' should be at least 7200 bytes long
 * to hold all possible emitted data.
 *
 * will also write id3v1 tags (if any) into the bitstream
 *
 * return code = number of bytes output to mp3buf. Can be 0
 *)
  lame_encode_flush_t = function(handle : PLame_global_flags;       // global context handle
                      mp3buf : PByte;                     // pointer to encoded MP3 stream
                      size : Integer                     // number of valid octets in this stream
                     ) : Integer; cdecl;

(*
 * OPTIONAL:
 * lame_encode_flush_nogap will flush the internal mp3 buffers and pad
 * the last frame with ancillary data so it is a complete mp3 frame.
 *
 * 'mp3buf' should be at least 7200 bytes long
 * to hold all possible emitted data.
 *
 * After a call to this routine, the outputed mp3 data is complete, but
 * you may continue to encode new PCM samples and write future mp3 data
 * to a different file.  The two mp3 files will play back with no gaps
 * if they are concatenated together.
 *
 * This routine will NOT write id3v1 tags into the bitstream.
 *
 * return code = number of bytes output to mp3buf. Can be 0
 *)
  lame_encode_flush_nogap_t = function(handle : PLame_global_flags;       // global context handle
                      mp3buf : PByte;                     // pointer to encoded MP3 stream
                      size : Integer                     // number of valid octets in this stream
                     ) : Integer; cdecl;

(*
 * OPTIONAL:
 * Normally, this is called by lame_init_params().  It writes id3v2 and
 * Xing headers into the front of the bitstream, and sets frame counters
 * and bitrate histogram data to 0.  You can also call this after
 * lame_encode_flush_nogap().
 *)
  lame_init_bitstream_t = function(handle : PLame_global_flags) : Integer; cdecl;

// Note by A.B.: Attention! Skipping several optional functions.

(*
 * REQUIRED:
 * final call to free all remaining buffers
 *)
  lame_close_t = function(handle : PLame_global_flags) : Integer; cdecl;

(*********************************************************************
 *
 * decoding
 *
 * a simple interface to mpglib, part of mpg123, is also included if
 * libmp3lame is compiled with HAVE_MPGLIB
 *
 *********************************************************************)

  PMP3data_struct = ^mp3data_struct;

  mp3data_struct = record
    header_parsed : Integer;            (* 1 if header was parsed and following
                                           data was computed *)
    stereo : Integer;                   // number of channels
    samplerate : Integer;               // sample rate
    bitrate : Integer;                  // bitrate
    mode : Integer;                     // mp3 frame type
    mode_ext : Integer;                 // mp3 frame type
    framesize : Integer;                // number of samples per mp3 frame

    // this data is only computed if mpglib detects a Xing VBR header
    nsamp : LongWord;                   // number of samples in mp3 file.
    totalframes : Integer;              // total number of frames in mp3 file

    // this data is not currently computed by the mpglib routines
    framenum : Integer;                 // frames decoded counter                         */
  end;


// required call to initialize decoder
  lame_decode_init_t = function : Integer; cdecl;

(*********************************************************************
 * input 1 mp3 frame, output (maybe) pcm data.
 * lame_decode() return code:
 *   -1: error
 *    0: need more data
 *  n>0: size of pcm output
 *********************************************************************)
  lame_decode_t = function(mp3buf : PByte; len : Integer; pcm_l, pcm_r : PShortInt)
  : Integer; cdecl;

// same as lame_decode, and also returns mp3 header data
  lame_decode_headers_t = function(mp3buf : PByte; len : Integer; pcm_l, pcm_r : PShortInt;
  mp3data : PMP3data_struct) : Integer; cdecl;

// same as lame_decode, but returns at most one frame
  lame_decode1_t = function(mp3buf : PByte; len : Integer; pcm_l, pcm_r : PShortInt)
  : Integer; cdecl;

// same as lame_decode1, but returns at most one frame and mp3 header data
  lame_decode1_headers_t = function(mp3buf : PByte; len : Integer; pcm_l, pcm_r : PShortInt;
  mp3data : PMP3data_struct) : Integer; cdecl;

(*********************************************************************
 *
 * id3tag stuff
 *
 *********************************************************************)

(*
 * The original header file is
 * id3tag.h -- Interface to write ID3 version 1 and 2 tags.
 * Copyright (C) 2000 Don Melton.
 *)

  id3tag_genre_list_cbf = procedure(Number : Integer; Genre : PChar; CustomData : Pointer); cdecl;

  // utility to obtain alphabetically sorted list of genre names with numbers */
  id3tag_genre_list_t = procedure(handler : id3tag_genre_list_cbf; CustomData : Pointer); cdecl;

  id3tag_init_t = procedure(handle : PLame_global_flags); cdecl;

  // force addition of version 2 tag */
  id3tag_add_v2_t = procedure(handle : PLame_global_flags); cdecl;

  // add only a version 1 tag
  id3tag_v1_only_t = procedure(handle : PLame_global_flags); cdecl;

  // add only a version 2 tag
  id3tag_v2_only_t = procedure(handle : PLame_global_flags); cdecl;

  // pad version 1 tag with spaces instead of nulls
  id3tag_space_v1_t = procedure(handle : PLame_global_flags); cdecl;

  // pad version 2 tag with extra 128 bytes
  id3tag_pad_v2_t = procedure(handle : PLame_global_flags); cdecl;

  id3tag_set_title_t = procedure(handle : PLame_global_flags; title : PChar); cdecl;
  id3tag_set_artist_t = procedure(handle : PLame_global_flags; artist : PChar); cdecl;
  id3tag_set_album_t = procedure(handle : PLame_global_flags; album : PChar); cdecl;
  id3tag_set_year_t = procedure(handle : PLame_global_flags; year : PChar); cdecl;
  id3tag_set_comment_t = procedure(handle : PLame_global_flags; comment : PChar); cdecl;
  id3tag_set_track_t = procedure(handle : PLame_global_flags; track : PChar); cdecl;

  // return non-zero result if genre name or number is invalid
  id3tag_set_genre_t = function(handle : PLame_global_flags; genre : PChar) : Integer; cdecl;

const
  LAME_MAXMP3BUFFER = 16384;

var
  LameLoaded : Boolean = False;

  lame_init : lame_init_t;
  lame_set_num_samples : lame_set_num_samples_t;
  lame_get_num_samples : lame_get_num_samples_t;
  lame_set_in_samplerate : lame_set_in_samplerate_t;
  lame_get_in_samplerate : lame_get_in_samplerate_t;
  lame_set_num_channels : lame_set_num_channels_t;
  lame_get_num_channels : lame_get_num_channels_t;
  lame_set_scale : lame_set_scale_t;
  lame_get_scale : lame_get_scale_t;
  lame_set_scale_left : lame_set_scale_left_t;
  lame_get_scale_left : lame_get_scale_left_t;
  lame_set_scale_right : lame_set_scale_right_t;
  lame_get_scale_right : lame_get_scale_right_t;
  lame_set_out_samplerate : lame_set_out_samplerate_t;
  lame_get_out_samplerate : lame_get_out_samplerate_t;
  lame_set_analysis : lame_set_analysis_t;
  lame_get_analysis : lame_get_analysis_t;
  lame_set_bWriteVbrTag : lame_set_bWriteVbrTag_t;
  lame_get_bWriteVbrTag  : lame_get_bWriteVbrTag_t;
  lame_set_decode_only : lame_set_decode_only_t;
  lame_get_decode_only : lame_get_decode_only_t;
  lame_set_ogg : lame_set_ogg_t;
  lame_get_ogg : lame_get_ogg_t;
  lame_set_quality : lame_set_quality_t;
  lame_get_quality : lame_get_quality_t;
  lame_set_mode : lame_set_mode_t;
  lame_get_mode : lame_get_mode_t;
  lame_set_mode_automs : lame_set_mode_automs_t;
  lame_get_mode_automs : lame_get_mode_automs_t;
  lame_set_force_ms : lame_set_force_ms_t;
  lame_get_force_ms : lame_get_force_ms_t;
  lame_set_free_format : lame_set_free_format_t;
  lame_get_free_format : lame_get_free_format_t;
  lame_set_brate : lame_set_brate_t;
  lame_get_brate : lame_get_brate_t;
  lame_set_compression_ratio : lame_set_compression_ratio_t;
  lame_get_compression_ratio : lame_get_compression_ratio_t;
  lame_set_preset : lame_set_preset_t;
  lame_set_asm_optimizations : lame_set_asm_optimizations_t;
  lame_set_copyright : lame_set_copyright_t;
  lame_get_copyright : lame_get_copyright_t;
  lame_set_original : lame_set_original_t;
  lame_get_original : lame_get_original_t;
  lame_set_error_protection : lame_set_error_protection_t;
  lame_get_error_protection : lame_get_error_protection_t;
  lame_set_padding_type : lame_set_padding_type_t;
  lame_get_padding_type : lame_get_padding_type_t;
  lame_set_extension : lame_set_extension_t;
  lame_get_extension : lame_get_extension_t;
  lame_set_strict_ISO : lame_set_strict_ISO_t;
  lame_get_strict_ISO : lame_get_strict_ISO_t;
  lame_set_VBR : lame_set_VBR_t;
  lame_get_VBR : lame_get_VBR_t;
  lame_set_VBR_q : lame_set_VBR_q_t;
  lame_get_VBR_q : lame_get_VBR_q_t;
  lame_set_VBR_mean_bitrate_kbps : lame_set_VBR_mean_bitrate_kbps_t;
  lame_get_VBR_mean_bitrate_kbps : lame_get_VBR_mean_bitrate_kbps_t;
  lame_set_VBR_min_bitrate_kbps : lame_set_VBR_min_bitrate_kbps_t;
  lame_get_VBR_min_bitrate_kbps : lame_get_VBR_min_bitrate_kbps_t;
  lame_set_VBR_max_bitrate_kbps : lame_set_VBR_max_bitrate_kbps_t;
  lame_get_VBR_max_bitrate_kbps : lame_get_VBR_max_bitrate_kbps_t;
  lame_set_VBR_hard_min : lame_set_VBR_hard_min_t;
  lame_get_VBR_hard_min : lame_get_VBR_hard_min_t;
  lame_set_lowpassfreq : lame_set_lowpassfreq_t;
  lame_get_lowpassfreq : lame_get_lowpassfreq_t;
  lame_set_lowpasswidth : lame_set_lowpasswidth_t;
  lame_get_lowpasswidth : lame_get_lowpasswidth_t;
  lame_set_highpassfreq : lame_set_highpassfreq_t;
  lame_get_highpassfreq : lame_get_highpassfreq_t;
  lame_set_highpasswidth : lame_set_highpasswidth_t;
  lame_get_highpasswidth : lame_get_highpasswidth_t;
  lame_get_version : lame_get_version_t;
  lame_get_encoder_delay : lame_get_encoder_delay_t;
  lame_get_encoder_padding : lame_get_encoder_padding_t;
  lame_get_framesize : lame_get_framesize_t;
  lame_get_mf_samples_to_encode : lame_get_mf_samples_to_encode_t;
  lame_get_size_mp3buffer : lame_get_size_mp3buffer_t;
  lame_get_frameNum : lame_get_frameNum_t;
  lame_get_totalframes : lame_get_totalframes_t;
  lame_init_params : lame_init_params_t;
  get_lame_version : get_lame_version_t;
  get_lame_short_version : get_lame_short_version_t;
  get_lame_very_short_version : get_lame_very_short_version_t;
  get_psy_version : get_psy_version_t;
  get_lame_url : get_lame_url_t;
  get_lame_version_numerical : get_lame_version_numerical_t;
  lame_encode_buffer : lame_encode_buffer_t;
  lame_encode_buffer_interleaved : lame_encode_buffer_interleaved_t;
  lame_encode_buffer_float : lame_encode_buffer_float_t;
  lame_encode_flush : lame_encode_flush_t;
  lame_encode_flush_nogap : lame_encode_flush_nogap_t;
  lame_init_bitstream : lame_init_bitstream_t;
  lame_close : lame_close_t;
  lame_decode_init : lame_decode_init_t;
  lame_decode : lame_decode_t;
  lame_decode_headers : lame_decode_headers_t;
  lame_decode1 : lame_decode1_t;
  lame_decode1_headers : lame_decode1_headers_t;
  id3tag_genre_list : id3tag_genre_list_t;
  id3tag_init : id3tag_init_t;
  id3tag_add_v2 : id3tag_add_v2_t;
  id3tag_v1_only : id3tag_v1_only_t;
  id3tag_v2_only : id3tag_v2_only_t;
  id3tag_space_v1 : id3tag_space_v1_t;
  id3tag_pad_v2 : id3tag_pad_v2_t;
  id3tag_set_title : id3tag_set_title_t;
  id3tag_set_artist : id3tag_set_artist_t;
  id3tag_set_album : id3tag_set_album_t;
  id3tag_set_year : id3tag_set_year_t;
  id3tag_set_comment : id3tag_set_comment_t;
  id3tag_set_track : id3tag_set_track_t;
  id3tag_set_genre : id3tag_set_genre_t;

procedure LoadLAME;
procedure UnloadLAME;

implementation

var

{$IFDEF LINUX}
  Libhandle : Pointer;
{$ENDIF}

{$IFDEF WIN32}
  Libhandle : HMODULE;
{$ENDIF}

procedure LoadLAME;
begin
  if LAMELoaded then Exit;
{$IFDEF LINUX}
  Libhandle := dlopen(LAME_PATH, RTLD_NOW or RTLD_GLOBAL);
  if Libhandle <> nil then
  begin
    LAMELoaded := True;

    lame_init := dlsym(Libhandle, 'lame_init');
    lame_set_num_samples := dlsym(Libhandle, 'lame_set_num_samples');
    lame_get_num_samples := dlsym(Libhandle, 'lame_get_num_samples');
    lame_set_in_samplerate := dlsym(Libhandle, 'lame_set_in_samplerate');
    lame_get_in_samplerate := dlsym(Libhandle, 'lame_get_in_samplerate');
    lame_set_num_channels := dlsym(Libhandle, 'lame_set_num_channels');
    lame_get_num_channels := dlsym(Libhandle, 'lame_get_num_channels');
    lame_set_scale := dlsym(Libhandle, 'lame_set_scale');
    lame_get_scale := dlsym(Libhandle, 'lame_get_scale');
    lame_set_scale_left := dlsym(Libhandle, 'lame_set_scale_left');
    lame_get_scale_left := dlsym(Libhandle, 'lame_get_scale_left');
    lame_set_scale_right := dlsym(Libhandle, 'lame_set_scale_right');
    lame_get_scale_right := dlsym(Libhandle, 'lame_get_scale_right');
    lame_set_out_samplerate := dlsym(Libhandle, 'lame_set_out_samplerate');
    lame_get_out_samplerate := dlsym(Libhandle, 'lame_get_out_samplerate');
    lame_set_analysis := dlsym(Libhandle, 'lame_set_analysis');
    lame_get_analysis := dlsym(Libhandle, 'lame_get_analysis');
    lame_set_bWriteVbrTag := dlsym(Libhandle, 'lame_set_bWriteVbrTag');
    lame_get_bWriteVbrTag    := dlsym(Libhandle, 'lame_get_bWriteVbrTag');
    lame_set_decode_only := dlsym(Libhandle, 'lame_set_decode_only');
    lame_get_decode_only := dlsym(Libhandle, 'lame_get_decode_only');
    lame_set_ogg := dlsym(Libhandle, 'lame_set_ogg');
    lame_get_ogg := dlsym(Libhandle, 'lame_get_ogg');
    lame_set_quality := dlsym(Libhandle, 'lame_set_quality');
    lame_get_quality := dlsym(Libhandle, 'lame_get_quality');
    lame_set_mode := dlsym(Libhandle, 'lame_set_mode');
    lame_get_mode := dlsym(Libhandle, 'lame_get_mode');
    lame_set_mode_automs := dlsym(Libhandle, 'lame_set_mode_automs');
    lame_get_mode_automs := dlsym(Libhandle, 'lame_get_mode_automs');
    lame_set_force_ms := dlsym(Libhandle, 'lame_set_force_ms');
    lame_get_force_ms := dlsym(Libhandle, 'lame_get_force_ms');
    lame_set_free_format := dlsym(Libhandle, 'lame_set_free_format');
    lame_get_free_format := dlsym(Libhandle, 'lame_get_free_format');
    lame_set_brate := dlsym(Libhandle, 'lame_set_brate');
    lame_get_brate := dlsym(Libhandle, 'lame_get_brate');
    lame_set_compression_ratio := dlsym(Libhandle, 'lame_set_compression_ratio');
    lame_get_compression_ratio := dlsym(Libhandle, 'lame_get_compression_ratio');
    lame_set_preset := dlsym(Libhandle, 'lame_set_preset');
    lame_set_asm_optimizations := dlsym(Libhandle, 'lame_set_asm_optimizations');
    lame_set_copyright := dlsym(Libhandle, 'lame_set_copyright');
    lame_get_copyright := dlsym(Libhandle, 'lame_get_copyright');
    lame_set_original := dlsym(Libhandle, 'lame_set_original');
    lame_get_original := dlsym(Libhandle, 'lame_get_original');
    lame_set_error_protection := dlsym(Libhandle, 'lame_set_error_protection');
    lame_get_error_protection := dlsym(Libhandle, 'lame_get_error_protection');
    lame_set_padding_type := dlsym(Libhandle, 'lame_set_padding_type');
    lame_get_padding_type := dlsym(Libhandle, 'lame_get_padding_type');
    lame_set_extension := dlsym(Libhandle, 'lame_set_extension');
    lame_get_extension := dlsym(Libhandle, 'lame_get_extension');
    lame_set_strict_ISO := dlsym(Libhandle, 'lame_set_strict_ISO');
    lame_get_strict_ISO := dlsym(Libhandle, 'lame_get_strict_ISO');
    lame_set_VBR := dlsym(Libhandle, 'lame_set_VBR');
    lame_get_VBR := dlsym(Libhandle, 'lame_get_VBR');
    lame_set_VBR_q := dlsym(Libhandle, 'lame_set_VBR_q');
    lame_get_VBR_q := dlsym(Libhandle, 'lame_get_VBR_q');
    lame_set_VBR_mean_bitrate_kbps := dlsym(Libhandle, 'lame_set_VBR_mean_bitrate_kbps');
    lame_get_VBR_mean_bitrate_kbps := dlsym(Libhandle, 'lame_get_VBR_mean_bitrate_kbps');
    lame_set_VBR_min_bitrate_kbps := dlsym(Libhandle, 'lame_set_VBR_min_bitrate_kbps');
    lame_get_VBR_min_bitrate_kbps := dlsym(Libhandle, 'lame_get_VBR_min_bitrate_kbps');
    lame_set_VBR_max_bitrate_kbps := dlsym(Libhandle, 'lame_set_VBR_max_bitrate_kbps');
    lame_get_VBR_max_bitrate_kbps := dlsym(Libhandle, 'lame_get_VBR_max_bitrate_kbps');
    lame_set_VBR_hard_min := dlsym(Libhandle, 'lame_set_VBR_hard_min');
    lame_get_VBR_hard_min := dlsym(Libhandle, 'lame_get_VBR_hard_min');
    lame_set_lowpassfreq := dlsym(Libhandle, 'lame_set_lowpassfreq');
    lame_get_lowpassfreq := dlsym(Libhandle, 'lame_get_lowpassfreq');
    lame_set_lowpasswidth := dlsym(Libhandle, 'lame_set_lowpasswidth');
    lame_get_lowpasswidth := dlsym(Libhandle, 'lame_get_lowpasswidth');
    lame_set_highpassfreq := dlsym(Libhandle, 'lame_set_highpassfreq');
    lame_get_highpassfreq := dlsym(Libhandle, 'lame_get_highpassfreq');
    lame_set_highpasswidth := dlsym(Libhandle, 'lame_set_highpasswidth');
    lame_get_highpasswidth := dlsym(Libhandle, 'lame_get_highpasswidth');
    lame_get_version := dlsym(Libhandle, 'lame_get_version');
    lame_get_encoder_delay := dlsym(Libhandle, 'lame_get_encoder_delay');
    lame_get_encoder_padding := dlsym(Libhandle, 'lame_get_encoder_padding');
    lame_get_framesize := dlsym(Libhandle, 'lame_get_framesize');
    lame_get_mf_samples_to_encode := dlsym(Libhandle, 'lame_get_mf_samples_to_encode');
    lame_get_size_mp3buffer := dlsym(Libhandle, 'lame_get_size_mp3buffer');
    lame_get_frameNum := dlsym(Libhandle, 'lame_get_frameNum');
    lame_get_totalframes := dlsym(Libhandle, 'lame_get_totalframes');
    lame_init_params := dlsym(Libhandle, 'lame_init_params');
    get_lame_version := dlsym(Libhandle, 'get_lame_version');
    get_lame_short_version := dlsym(Libhandle, 'get_lame_short_version');
    get_lame_very_short_version := dlsym(Libhandle, 'get_lame_very_short_version');
    get_psy_version := dlsym(Libhandle, 'get_psy_version');
    get_lame_url := dlsym(Libhandle, 'get_lame_url');
    get_lame_version_numerical := dlsym(Libhandle, 'get_lame_version_numerical');
    lame_encode_buffer := dlsym(Libhandle, 'lame_encode_buffer');
    lame_encode_buffer_interleaved := dlsym(Libhandle, 'lame_encode_buffer_interleaved');
    lame_encode_buffer_float := dlsym(Libhandle, 'lame_encode_buffer_float');
    lame_encode_flush := dlsym(Libhandle, 'lame_encode_flush');
    lame_encode_flush_nogap := dlsym(Libhandle, 'lame_encode_flush_nogap');
    lame_init_bitstream := dlsym(Libhandle, 'lame_init_bitstream');
    lame_close := dlsym(Libhandle, 'lame_close');
    lame_decode_init := dlsym(Libhandle, 'lame_decode_init');
    lame_decode := dlsym(Libhandle, 'lame_decode');
    lame_decode_headers := dlsym(Libhandle, 'lame_decode_headers');
    lame_decode1 := dlsym(Libhandle, 'lame_decode1');
    lame_decode1_headers := dlsym(Libhandle, 'lame_decode1_headers');
    id3tag_genre_list := dlsym(Libhandle, 'id3tag_genre_list');
    id3tag_init := dlsym(Libhandle, 'id3tag_init');
    id3tag_add_v2 := dlsym(Libhandle, 'id3tag_add_v2');
    id3tag_v1_only := dlsym(Libhandle, 'id3tag_v1_only');
    id3tag_v2_only := dlsym(Libhandle, 'id3tag_v2_only');
    id3tag_space_v1 := dlsym(Libhandle, 'id3tag_space_v1');
    id3tag_pad_v2 := dlsym(Libhandle, 'id3tag_pad_v2');
    id3tag_set_title := dlsym(Libhandle, 'id3tag_set_title');
    id3tag_set_artist := dlsym(Libhandle, 'id3tag_set_artist');
    id3tag_set_album := dlsym(Libhandle, 'id3tag_set_album');
    id3tag_set_year := dlsym(Libhandle, 'id3tag_set_year');
    id3tag_set_comment := dlsym(Libhandle, 'id3tag_set_comment');
    id3tag_set_track := dlsym(Libhandle, 'id3tag_set_track');
    id3tag_set_genre := dlsym(Libhandle, 'id3tag_set_genre');
  end;
{$ENDIF}

{$IFDEF WIN32}
  Libhandle := LoadLibraryEx(LAME_PATH, 0, 0);
  if Libhandle <> 0 then
  begin
    LAMELoaded := True;

    lame_init := GetProcAddress(Libhandle, 'lame_init');
    lame_set_num_samples := GetProcAddress(Libhandle, 'lame_set_num_samples');
    lame_get_num_samples := GetProcAddress(Libhandle, 'lame_get_num_samples');
    lame_set_in_samplerate := GetProcAddress(Libhandle, 'lame_set_in_samplerate');
    lame_get_in_samplerate := GetProcAddress(Libhandle, 'lame_get_in_samplerate');
    lame_set_num_channels := GetProcAddress(Libhandle, 'lame_set_num_channels');
    lame_get_num_channels := GetProcAddress(Libhandle, 'lame_get_num_channels');
    lame_set_scale := GetProcAddress(Libhandle, 'lame_set_scale');
    lame_get_scale := GetProcAddress(Libhandle, 'lame_get_scale');
    lame_set_scale_left := GetProcAddress(Libhandle, 'lame_set_scale_left');
    lame_get_scale_left := GetProcAddress(Libhandle, 'lame_get_scale_left');
    lame_set_scale_right := GetProcAddress(Libhandle, 'lame_set_scale_right');
    lame_get_scale_right := GetProcAddress(Libhandle, 'lame_get_scale_right');
    lame_set_out_samplerate := GetProcAddress(Libhandle, 'lame_set_out_samplerate');
    lame_get_out_samplerate := GetProcAddress(Libhandle, 'lame_get_out_samplerate');
    lame_set_analysis := GetProcAddress(Libhandle, 'lame_set_analysis');
    lame_get_analysis := GetProcAddress(Libhandle, 'lame_get_analysis');
    lame_set_bWriteVbrTag := GetProcAddress(Libhandle, 'lame_set_bWriteVbrTag');
    lame_get_bWriteVbrTag    := GetProcAddress(Libhandle, 'lame_get_bWriteVbrTag');
    lame_set_decode_only := GetProcAddress(Libhandle, 'lame_set_decode_only');
    lame_get_decode_only := GetProcAddress(Libhandle, 'lame_get_decode_only');
    lame_set_ogg := GetProcAddress(Libhandle, 'lame_set_ogg');
    lame_get_ogg := GetProcAddress(Libhandle, 'lame_get_ogg');
    lame_set_quality := GetProcAddress(Libhandle, 'lame_set_quality');
    lame_get_quality := GetProcAddress(Libhandle, 'lame_get_quality');
    lame_set_mode := GetProcAddress(Libhandle, 'lame_set_mode');
    lame_get_mode := GetProcAddress(Libhandle, 'lame_get_mode');
    lame_set_mode_automs := GetProcAddress(Libhandle, 'lame_set_mode_automs');
    lame_get_mode_automs := GetProcAddress(Libhandle, 'lame_get_mode_automs');
    lame_set_force_ms := GetProcAddress(Libhandle, 'lame_set_force_ms');
    lame_get_force_ms := GetProcAddress(Libhandle, 'lame_get_force_ms');
    lame_set_free_format := GetProcAddress(Libhandle, 'lame_set_free_format');
    lame_get_free_format := GetProcAddress(Libhandle, 'lame_get_free_format');
    lame_set_brate := GetProcAddress(Libhandle, 'lame_set_brate');
    lame_get_brate := GetProcAddress(Libhandle, 'lame_get_brate');
    lame_set_compression_ratio := GetProcAddress(Libhandle, 'lame_set_compression_ratio');
    lame_get_compression_ratio := GetProcAddress(Libhandle, 'lame_get_compression_ratio');
    lame_set_preset := GetProcAddress(Libhandle, 'lame_set_preset');
    lame_set_asm_optimizations := GetProcAddress(Libhandle, 'lame_set_asm_optimizations');
    lame_set_copyright := GetProcAddress(Libhandle, 'lame_set_copyright');
    lame_get_copyright := GetProcAddress(Libhandle, 'lame_get_copyright');
    lame_set_original := GetProcAddress(Libhandle, 'lame_set_original');
    lame_get_original := GetProcAddress(Libhandle, 'lame_get_original');
    lame_set_error_protection := GetProcAddress(Libhandle, 'lame_set_error_protection');
    lame_get_error_protection := GetProcAddress(Libhandle, 'lame_get_error_protection');
    lame_set_padding_type := GetProcAddress(Libhandle, 'lame_set_padding_type');
    lame_get_padding_type := GetProcAddress(Libhandle, 'lame_get_padding_type');
    lame_set_extension := GetProcAddress(Libhandle, 'lame_set_extension');
    lame_get_extension := GetProcAddress(Libhandle, 'lame_get_extension');
    lame_set_strict_ISO := GetProcAddress(Libhandle, 'lame_set_strict_ISO');
    lame_get_strict_ISO := GetProcAddress(Libhandle, 'lame_get_strict_ISO');
    lame_set_VBR := GetProcAddress(Libhandle, 'lame_set_VBR');
    lame_get_VBR := GetProcAddress(Libhandle, 'lame_get_VBR');
    lame_set_VBR_q := GetProcAddress(Libhandle, 'lame_set_VBR_q');
    lame_get_VBR_q := GetProcAddress(Libhandle, 'lame_get_VBR_q');
    lame_set_VBR_mean_bitrate_kbps := GetProcAddress(Libhandle, 'lame_set_VBR_mean_bitrate_kbps');
    lame_get_VBR_mean_bitrate_kbps := GetProcAddress(Libhandle, 'lame_get_VBR_mean_bitrate_kbps');
    lame_set_VBR_min_bitrate_kbps := GetProcAddress(Libhandle, 'lame_set_VBR_min_bitrate_kbps');
    lame_get_VBR_min_bitrate_kbps := GetProcAddress(Libhandle, 'lame_get_VBR_min_bitrate_kbps');
    lame_set_VBR_max_bitrate_kbps := GetProcAddress(Libhandle, 'lame_set_VBR_max_bitrate_kbps');
    lame_get_VBR_max_bitrate_kbps := GetProcAddress(Libhandle, 'lame_get_VBR_max_bitrate_kbps');
    lame_set_VBR_hard_min := GetProcAddress(Libhandle, 'lame_set_VBR_hard_min');
    lame_get_VBR_hard_min := GetProcAddress(Libhandle, 'lame_get_VBR_hard_min');
    lame_set_lowpassfreq := GetProcAddress(Libhandle, 'lame_set_lowpassfreq');
    lame_get_lowpassfreq := GetProcAddress(Libhandle, 'lame_get_lowpassfreq');
    lame_set_lowpasswidth := GetProcAddress(Libhandle, 'lame_set_lowpasswidth');
    lame_get_lowpasswidth := GetProcAddress(Libhandle, 'lame_get_lowpasswidth');
    lame_set_highpassfreq := GetProcAddress(Libhandle, 'lame_set_highpassfreq');
    lame_get_highpassfreq := GetProcAddress(Libhandle, 'lame_get_highpassfreq');
    lame_set_highpasswidth := GetProcAddress(Libhandle, 'lame_set_highpasswidth');
    lame_get_highpasswidth := GetProcAddress(Libhandle, 'lame_get_highpasswidth');
    lame_get_version := GetProcAddress(Libhandle, 'lame_get_version');
    lame_get_encoder_delay := GetProcAddress(Libhandle, 'lame_get_encoder_delay');
    lame_get_encoder_padding := GetProcAddress(Libhandle, 'lame_get_encoder_padding');
    lame_get_framesize := GetProcAddress(Libhandle, 'lame_get_framesize');
    lame_get_mf_samples_to_encode := GetProcAddress(Libhandle, 'lame_get_mf_samples_to_encode');
    lame_get_size_mp3buffer := GetProcAddress(Libhandle, 'lame_get_size_mp3buffer');
    lame_get_frameNum := GetProcAddress(Libhandle, 'lame_get_frameNum');
    lame_get_totalframes := GetProcAddress(Libhandle, 'lame_get_totalframes');
    lame_init_params := GetProcAddress(Libhandle, 'lame_init_params');
    get_lame_version := GetProcAddress(Libhandle, 'get_lame_version');
    get_lame_short_version := GetProcAddress(Libhandle, 'get_lame_short_version');
    get_lame_very_short_version := GetProcAddress(Libhandle, 'get_lame_very_short_version');
    get_psy_version := GetProcAddress(Libhandle, 'get_psy_version');
    get_lame_url := GetProcAddress(Libhandle, 'get_lame_url');
    get_lame_version_numerical := GetProcAddress(Libhandle, 'get_lame_version_numerical');
    lame_encode_buffer := GetProcAddress(Libhandle, 'lame_encode_buffer');
    lame_encode_buffer_interleaved := GetProcAddress(Libhandle, 'lame_encode_buffer_interleaved');
    lame_encode_buffer_float := GetProcAddress(Libhandle, 'lame_encode_buffer_float');
    lame_encode_flush := GetProcAddress(Libhandle, 'lame_encode_flush');
    lame_encode_flush_nogap := GetProcAddress(Libhandle, 'lame_encode_flush_nogap');
    lame_init_bitstream := GetProcAddress(Libhandle, 'lame_init_bitstream');
    lame_close := GetProcAddress(Libhandle, 'lame_close');
    lame_decode_init := GetProcAddress(Libhandle, 'lame_decode_init');
    lame_decode := GetProcAddress(Libhandle, 'lame_decode');
    lame_decode_headers := GetProcAddress(Libhandle, 'lame_decode_headers');
    lame_decode1 := GetProcAddress(Libhandle, 'lame_decode1');
    lame_decode1_headers := GetProcAddress(Libhandle, 'lame_decode1_headers');
    id3tag_genre_list := GetProcAddress(Libhandle, 'id3tag_genre_list');
    id3tag_init := GetProcAddress(Libhandle, 'id3tag_init');
    id3tag_add_v2 := GetProcAddress(Libhandle, 'id3tag_add_v2');
    id3tag_v1_only := GetProcAddress(Libhandle, 'id3tag_v1_only');
    id3tag_v2_only := GetProcAddress(Libhandle, 'id3tag_v2_only');
    id3tag_space_v1 := GetProcAddress(Libhandle, 'id3tag_space_v1');
    id3tag_pad_v2 := GetProcAddress(Libhandle, 'id3tag_pad_v2');
    id3tag_set_title := GetProcAddress(Libhandle, 'id3tag_set_title');
    id3tag_set_artist := GetProcAddress(Libhandle, 'id3tag_set_artist');
    id3tag_set_album := GetProcAddress(Libhandle, 'id3tag_set_album');
    id3tag_set_year := GetProcAddress(Libhandle, 'id3tag_set_year');
    id3tag_set_comment := GetProcAddress(Libhandle, 'id3tag_set_comment');
    id3tag_set_track := GetProcAddress(Libhandle, 'id3tag_set_track');
    id3tag_set_genre := GetProcAddress(Libhandle, 'id3tag_set_genre');
  end;
{$ENDIF}
end;

procedure UnloadLAME;
begin

  LAMELoaded := False;

{$IFDEF LINUX}
  if Libhandle <> nil then dlclose(Libhandle);
{$ENDIF}

{$IFDEF WIN32}
  if Libhandle <> 0 then FreeLibrary(Libhandle);
{$ENDIF}

end;
end.
