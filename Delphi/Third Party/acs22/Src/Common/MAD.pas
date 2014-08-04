unit MAD;

interface

uses

{$IFDEF WIN32}
  Windows;
{$ENDIF}

{$IFDEF LINUX}
  Libc, ACS_Procs;
{$ENDIF}


const

{$IFDEF WIN32}
  MADLibPath = 'MADLib.dll';
{$ENDIF}

{$IFDEF LINUX}
  MADLibPath = 'libmad.so*'; // libmad.so
  {$DEFINE SEARCH_LIBS}
{$ENDIF}


var

  MADLibLoaded : Boolean = False;

type

  mad_bitptr  = packed record
    b : PChar;
    Cache, Left : Word;
  end;

  mad_stream = packed record
    buffer : Pointer;
    bufend : Pointer;
    skiplen : LongWord;
    sync : Integer;
    freerate : LongWord;
    this_frame : Pointer;
    next_frame : Pointer;
    ptr : mad_bitptr;
    anc_ptr : mad_bitptr;
    anc_bitlen : LongWord;
    main_data : Pointer;
    md_len : LongWord;
    options : Integer;
    error : Integer;
  end;

  p_mad_stream = ^mad_stream;

  mad_timer_t = packed record
    seconds : Integer;
    fraction : LongWord;
  end;

  mad_header = packed record
    layer : Integer;
    mode : Integer;
    mode_extension : Integer;
    emphasis : Integer;
    bitrate : LongWord;
    samplerate : LongWord;
    crc_check : Word;
    crc_target : Word;
    flags : Integer;
    private_bits : Integer;
    duration : mad_timer_t;
  end;

  p_mad_header = ^mad_header;

  mad_frame = packed record
    header : mad_header;
    options : Integer;
    sbsample : packed array[0..1, 0..35, 0..31] of Integer;
    overlap : Pointer;
  end;

  p_mad_frame = ^mad_frame;

  mad_pcm = packed record
    samplerate : LongWord;
    channels : Word;
    length : Word;
    samples : packed array [0..1, 0..1151] of Integer;
  end;

  p_mad_pcm = ^mad_pcm;

  mad_synth = packed record
    filter : array[0..1, 0..1, 0..1, 0..15, 0..7] of Integer;
    phase : LongWord;
    pcm : mad_pcm;
  end;

  async_struct = packed record
    pid : LongWord;
    _in : Integer;
    _out : Integer;
  end;

  sync_struct = packed record
    stream : mad_stream;
    frame : mad_frame;
    synth : mad_synth;
  end;

  p_sync_struct = ^sync_struct;

  TInputFunc = function(CData : Pointer; Stream : p_mad_stream) : Integer; cdecl;
  THeaderFunc = function(CData : Pointer; Header : p_mad_header) : Integer; cdecl;
  TFilterFunc = function(CData : Pointer; Frame : p_mad_frame) : Integer; cdecl;
  TOutputFunc = function(CData : Pointer; Header : p_mad_header; pcm : p_mad_pcm) : Integer; cdecl;
  TErrorFunc = function(CData : Pointer; Stream : p_mad_stream; Frame : p_mad_frame) : Integer; cdecl;
  TMessageFunc = function(P1, P2 : Pointer; var l : LongWord) : Integer; cdecl;

  mad_decoder = packed record
    mode : Integer;
    options : Integer;
    async : async_struct;
    sync : p_sync_struct;
    data : Pointer;
    InputFunc : TInputFunc;
    HeaderFunc : THeaderFunc;
    FilterFunc : TFilterFunc;
    OutputFunc : TOutputFunc;
    ErrorFunc : TErrorFunc;
    MessageFunc : TMessageFunc;
  end;

  p_mad_decoder = ^mad_decoder;

const

  MAD_F_FRACBITS = 28;
  MAD_F_ONE = $10000000;

  MAD_FLOW_CONTINUE     = $0;
  MAD_FLOW_STOP         = $10;
  MAD_FLOW_BREAK        = $11;
  MAD_FLOW_IGNORE       = $20;

  MAD_DECODER_MODE_SYNC = 0;
  MAD_DECODER_MODE_ASYNC = 1;

type

  mad_decoder_init_t = procedure(mad_decoder : p_mad_decoder; CData : Pointer;
                		 InputFunc : TInputFunc;
                                 HeaderFunc : THeaderFunc;
                                 FilterFunc : TFilterFunc;
                                 OutputFunc : TOutputFunc;
                                 ErrorFunc : TErrorFunc;
                                 MessageFunc : TMessageFunc); cdecl;

  mad_decoder_finish_t = function(mad_decoder : p_mad_decoder) : Integer; cdecl;

  mad_decoder_run_t = function(mad_decoder : p_mad_decoder; mad_decoder_mode : Integer) : Integer; cdecl;

  mad_decoder_message_t = function(mad_decoder : p_mad_decoder; P : Pointer; var l : LongWord) : Integer; cdecl;

  mad_stream_buffer_t = procedure(MadStream : p_mad_stream; Data : Pointer; l : LongWord); cdecl;

  mad_stream_skip_t = procedure(MadStream : p_mad_stream; Skip : LongWord); cdecl;

  mad_stream_sync_t = function(MadStream : p_mad_stream) : Integer; cdecl;

var

  mad_decoder_init:  mad_decoder_init_t;
  mad_decoder_finish : mad_decoder_finish_t;
  mad_decoder_run : mad_decoder_run_t;
  mad_decoder_message : mad_decoder_message_t;
  mad_stream_buffer : mad_stream_buffer_t;
  mad_stream_skip : mad_stream_skip_t;
  mad_stream_sync : mad_stream_sync_t;

implementation


{$IFDEF WIN32}

var
  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(MADLibPath, 0, 0);

  if Libhandle <> 0 then
  begin
    MADLibLoaded := True;

    mad_decoder_init := GetProcAddress(Libhandle, 'mad_decoder_init');
    mad_decoder_finish := GetProcAddress(Libhandle, 'mad_decoder_finish');
    mad_decoder_run := GetProcAddress(Libhandle, 'mad_decoder_run');
    mad_decoder_message := GetProcAddress(Libhandle, 'mad_decoder_message');
    mad_stream_buffer := GetProcAddress(Libhandle, 'mad_stream_buffer');
    mad_stream_skip := GetProcAddress(Libhandle, 'mad_stream_skip');
    mad_stream_sync := GetProcAddress(Libhandle, 'mad_stream_sync');

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
  Path := FindLibs(MADLibPath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(MADLibPath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin

    MADLibLoaded := True;

    mad_decoder_init := dlsym(Libhandle, 'mad_decoder_init');
    mad_decoder_finish := dlsym(Libhandle, 'mad_decoder_finish');
    mad_decoder_run := dlsym(Libhandle, 'mad_decoder_run');
    mad_decoder_message := dlsym(Libhandle, 'mad_decoder_message');
    mad_stream_buffer := dlsym(Libhandle, 'mad_stream_buffer');
    mad_stream_skip := dlsym(Libhandle, 'mad_stream_skip');
    mad_stream_sync := dlsym(Libhandle, 'mad_stream_sync');

  end;

finalization

  if Libhandle <> nil then dlclose(Libhandle);

  {$ENDIF}


end.
