(*
  Delphi/Kylix headers for OggVorbis software codec.
  Translated from vorbisfile.h header
  by Andrei Borovsky, aborovsky@mtu-net.ru
  The original C/C++ headers and libraries (C) COPYRIGHT 1994-2001
  by the XIPHOPHORUS Company http://www.xiph.org/
*)

unit VorbisFile;


interface

uses

  ACS_Procs,

  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  ogg,
  Codec;

(* The function prototypes for the callbacks are basically the same as for
  the stdio functions fread, fseek, fclose, ftell.
  The one difference is that the FILE* arguments have been replaced with
  a void* - this is to be used as a pointer to whatever internal data these
  functions might need. In the stdio case, it's just a FILE* cast to a void*

 If you use other functions, check the docs for these functions and return
 the right values. For seek_func(), you*MUST* return -1 if the stream is
 unseekable *)

type

  read_func_t = function(ptr : Pointer; size, nmemb : Integer; datasource : Pointer) : Integer; cdecl;
  seek_func_t = function(datasource : Pointer; offset : ogg_int64_t; whence : Integer) : Integer; cdecl;
  close_func_t = function(datasource : Pointer) : Integer; cdecl;
  tell_func_t = function(datasource : Pointer) : Integer; cdecl;

  OV_CALLBACKS = record
    read_func : read_func_t;
    seek_func : seek_func_t;
    close_func : close_func_t;
    tell_func : tell_func_t;
  end;

const

  NOTOPEN = 0;
  PARTOPEN = 1;
  OPENED = 2;
  STREAMSET = 3;
  INITSET = 4;

type

  OGGVORBIS_FILE = record
    datasource: Pointer; // Pointer to a FILE*, etc.
    seekable: Integer;
    offset: OGG_INT64_T;
    _end: OGG_INT64_T;
    oy: OGG_SYNC_STATE;
    (* If the FILE handle isn't seekable (eg, a pipe),
      only the current stream appears *)
    links: Integer;
    offsets: POGG_INT64_T;
    dataoffsets: POGG_INT64_T;
    serialnos: PLongInt;
    pcmlengths: POGG_INT64_T;
    vi: PVORBIS_INFO;
    vc: PVORBIS_COMMENT;
    // Decoding working state local storage
    pcm_offset: OGG_INT64_T;
    ready_state: Integer;
    current_serialno: LongInt;
    current_link: Integer;
    bittrack: Double;
    samptrack: Double;
    os: OGG_STREAM_STATE;
    (* take physical pages, weld into a logical
      stream of packets *)
    vd: VORBIS_DSP_STATE;
    // central working state for the packet->PCM decoder
    vb: VORBIS_BLOCK;
    // local working space for packet->PCM decode
    callbacks: OV_CALLBACKS;
  end;

const

{$IFDEF LINUX}
  LibvorbisfilePath =  'libvorbisfile.so*'; //'/usr/lib/libvorbisfile.so';
  {$DEFINE SEARCH_LIBS}
{$ENDIF}
{$IFDEF WIN32}
  LibvorbisfilePath = 'vorbisfile.dll';
{$ENDIF}

// stdio.h constants

  SEEK_CUR = 1;
  SEEK_END = 2;
  SEEK_SET = 0;


var
  LibvorbisfileLoaded : Boolean = False;

type

{$IFDEF LINUX}

  ov_open_t = function(f: PIOFILE;
                 var vf: OGGVORBIS_FILE;
                 initial: PChar;
                 ibytes: LongInt): Integer; cdecl;

  ov_test_t = function(f: PIOFILE;
                 var vf: OGGVORBIS_FILE;
                 initial: PChar;
                 ibytes: LongInt): Integer; cdecl;
{$ENDIF}


  ov_clear_t = function(var vf: OGGVORBIS_FILE): Integer; cdecl;

  ov_open_callbacks_t = function(datasource: Pointer;
                           var vf: OGGVORBIS_FILE;
                           initial: PChar;
                           ibytes: LongInt;
                           callbacks: OV_CALLBACKS): Integer; cdecl;

  ov_test_callbacks_t = function(datasource: Pointer;
                           var vf: OGGVORBIS_FILE;
                           initial: PChar;
                           ibytes: LongInt;
                           callbacks: OV_CALLBACKS): Integer; cdecl;

  ov_test_open_t = function(var vf: OGGVORBIS_FILE): Integer; cdecl;

  ov_bitrate_t = function(var vf: OGGVORBIS_FILE;
                    i: Integer): LongInt cdecl;

  ov_bitrate_instant_t = function(var vf: OGGVORBIS_FILE): LongInt cdecl;

  ov_streams_t = function(var vf: OGGVORBIS_FILE): LongInt cdecl;

  ov_seekable_t = function(var vf: OGGVORBIS_FILE): LongInt cdecl;

  ov_serialnumber_t = function(var vf: OGGVORBIS_FILE;
                         i: Integer): LongInt cdecl;

  ov_raw_total_t = function(var vf: OGGVORBIS_FILE;
                      i: Integer): OGG_INT64_T cdecl;

  ov_pcm_total_t = function(var vf: OGGVORBIS_FILE;
                      i: Integer): OGG_INT64_T cdecl;

  ov_time_total_t = function(var vf: OGGVORBIS_FILE;
                       i: Integer): Double cdecl;

  ov_raw_seek_t = function(var vf: OGGVORBIS_FILE;
                     pos: LongInt): Integer cdecl;

  ov_pcm_seek_t = function(var vf: OGGVORBIS_FILE;
                     pos: OGG_INT64_T): Integer cdecl;

  ov_pcm_seek_page_t = function(var vf: OGGVORBIS_FILE;
                          pos: OGG_INT64_T): Integer cdecl;

  ov_time_seek_t = function(var vf: OGGVORBIS_FILE;
                      pos: Double): Integer cdecl;

  ov_time_seek_page_t = function(var vf: OGGVORBIS_FILE;
                           pos: Double): Integer cdecl;

  ov_raw_tell_t = function(var vf: OGGVORBIS_FILE): OGG_INT64_T cdecl;

  ov_pcm_tell_t = function(var vf: OGGVORBIS_FILE): OGG_INT64_T cdecl;

  ov_time_tell_t = function(var vf: OGGVORBIS_FILE): Double cdecl;

  ov_info_t = function(var vf: OGGVORBIS_FILE;
                 link : Integer): PVORBIS_INFO cdecl;

  ov_comment_t = function(var vf: OGGVORBIS_FILE;
                    link : Integer): PVORBIS_COMMENT cdecl;

  ov_read_float_t = function(var vf: OGGVORBIS_FILE;
                       var pcm_channels: PPFLOAT;
                       bitstream: PInteger): LongInt cdecl;

  ov_read_t = function(var vf: OGGVORBIS_FILE;
                 buffer: PChar;
                 length: Integer;
                 bigendianp: Integer;
                 word: Integer;
                 sgned: Integer;
                 bitstream: PInteger): LongInt cdecl;

var

{$IFDEF LINUX}

  ov_open : ov_open_t;

  ov_test : ov_test_t;

{$ENDIF}

  ov_clear : ov_clear_t;

  ov_open_callbacks : ov_open_callbacks_t;

  ov_test_callbacks : ov_test_callbacks_t;

  ov_test_open : ov_test_open_t;

  ov_bitrate : ov_bitrate_t;

  ov_bitrate_instant : ov_bitrate_instant_t;

  ov_streams : ov_streams_t;

  ov_seekable : ov_seekable_t;

  ov_serialnumber : ov_serialnumber_t;

  ov_raw_total : ov_raw_total_t;

  ov_pcm_total : ov_pcm_total_t;

  ov_time_total : ov_time_total_t;

  ov_raw_seek : ov_raw_seek_t;

  ov_pcm_seek : ov_pcm_seek_t;

  ov_pcm_seek_page : ov_pcm_seek_page_t;

  ov_time_seek : ov_time_seek_t;

  ov_time_seek_page : ov_time_seek_page_t;

  ov_raw_tell : ov_raw_tell_t;

  ov_pcm_tell : ov_pcm_tell_t;

  ov_time_tell : ov_time_tell_t;

  ov_info : ov_info_t;

  ov_comment : ov_comment_t;

  ov_read_float : ov_read_float_t;

  ov_read : ov_read_t;


implementation

{$IFDEF LINUX}

var
  Libhandle : Pointer;

{$IFDEF SEARCH_LIBS}
  Path : String;
{$ENDIF}

initialization

{$IFDEF SEARCH_LIBS}

  Libhandle := nil;
  Path := FindLibs(LibvorbisfilePath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(LibvorbisfilePath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin
    LibvorbisfileLoaded := True;
    ov_open := dlsym(Libhandle, 'ov_open');
    ov_test := dlsym(Libhandle, 'ov_test');
    ov_clear := dlsym(Libhandle, 'ov_clear');
    ov_open_callbacks := dlsym(Libhandle, 'ov_open_callbacks');
    ov_test_callbacks := dlsym(Libhandle, 'ov_test_callbacks');
    ov_test_open := dlsym(Libhandle, 'ov_test_open');
    ov_bitrate := dlsym(Libhandle, 'ov_bitrate');
    ov_bitrate_instant := dlsym(Libhandle, 'ov_bitrate_instant');
    ov_streams := dlsym(Libhandle, 'ov_streams');
    ov_seekable := dlsym(Libhandle, 'ov_seekable');
    ov_serialnumber := dlsym(Libhandle, 'ov_serialnumber');
    ov_raw_total := dlsym(Libhandle, 'ov_raw_total');
    ov_pcm_total := dlsym(Libhandle, 'ov_pcm_total');
    ov_time_total := dlsym(Libhandle, 'ov_time_total');
    ov_raw_seek := dlsym(Libhandle, 'ov_raw_seek');
    ov_pcm_seek := dlsym(Libhandle, 'ov_pcm_seek');
    ov_pcm_seek_page := dlsym(Libhandle, 'ov_pcm_seek_page');
    ov_time_seek := dlsym(Libhandle, 'ov_time_seek');
    ov_time_seek_page := dlsym(Libhandle, 'ov_time_seek_page');
    ov_raw_tell := dlsym(Libhandle, 'ov_raw_tell');
    ov_pcm_tell := dlsym(Libhandle, 'ov_pcm_tell');
    ov_time_tell := dlsym(Libhandle, 'ov_time_tell');
    ov_info := dlsym(Libhandle, 'ov_info');
    ov_comment := dlsym(Libhandle, 'ov_comment');
    ov_read_float := dlsym(Libhandle, 'ov_read_float');
    ov_read := dlsym(Libhandle, 'ov_read');
  end;

finalization

  if libhandle <> nil then dlclose(Libhandle);

{$ENDIF}


{$IFDEF WIN32}
var
  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(LibvorbisfilePath, 0, 0);

  if Libhandle <> 0 then
  begin
    LibvorbisfileLoaded := True;
    ov_clear := GetProcAddress(Libhandle, 'ov_clear');
    ov_open_callbacks := GetProcAddress(Libhandle, 'ov_open_callbacks');
    ov_test_callbacks := GetProcAddress(Libhandle, 'ov_test_callbacks');
    ov_test_open := GetProcAddress(Libhandle, 'ov_test_open');
    ov_bitrate := GetProcAddress(Libhandle, 'ov_bitrate');
    ov_bitrate_instant := GetProcAddress(Libhandle, 'ov_bitrate_instant');
    ov_streams := GetProcAddress(Libhandle, 'ov_streams');
    ov_seekable := GetProcAddress(Libhandle, 'ov_seekable');
    ov_serialnumber := GetProcAddress(Libhandle, 'ov_serialnumber');
    ov_raw_total := GetProcAddress(Libhandle, 'ov_raw_total');
    ov_pcm_total := GetProcAddress(Libhandle, 'ov_pcm_total');
    ov_time_total := GetProcAddress(Libhandle, 'ov_time_total');
    ov_raw_seek := GetProcAddress(Libhandle, 'ov_raw_seek');
    ov_pcm_seek := GetProcAddress(Libhandle, 'ov_pcm_seek');
    ov_pcm_seek_page := GetProcAddress(Libhandle, 'ov_pcm_seek_page');
    ov_time_seek := GetProcAddress(Libhandle, 'ov_time_seek');
    ov_time_seek_page := GetProcAddress(Libhandle, 'ov_time_seek_page');
    ov_raw_tell := GetProcAddress(Libhandle, 'ov_raw_tell');
    ov_pcm_tell := GetProcAddress(Libhandle, 'ov_pcm_tell');
    ov_time_tell := GetProcAddress(Libhandle, 'ov_time_tell');
    ov_info := GetProcAddress(Libhandle, 'ov_info');
    ov_comment := GetProcAddress(Libhandle, 'ov_comment');
    ov_read_float := GetProcAddress(Libhandle, 'ov_read_float');
    ov_read := GetProcAddress(Libhandle, 'ov_read');
  end;

finalization

  if Libhandle <> 0 then FreeLibrary(Libhandle);

{$ENDIF}

end.
