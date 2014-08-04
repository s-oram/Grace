(*
  Kylix header for libao library.
  Translated from ao.h header file
  by Andrei Borovsky, aborovsky@mtu-net.ru
  The original header is
  Original Copyright (C) Aaron Holtzman - May 1999
  Modifications Copyright (C) Stan Seibert - July 2000, July 2001
  More Modifications Copyright (C) Jack Moffitt - October 2000
*)

unit LibAO;

interface

uses
  Libc, ACS_Procs;

var
  LibaoLoaded : Boolean = False;

const

  LibaoPath = 'libao.so*';  // '/usr/lib/libao.so';
  {$DEFINE SEARCH_LIBS}

  AO_TYPE_LIVE = 1;
  AO_TYPE_FILE = 2;
  AO_ENODRIVER = 1;
  AO_ENOTFILE = 2;
  AO_ENOTLIVE = 3;
  AO_EBADOPTION = 4;
  AO_EOPENDEVICE = 5;
  AO_EOPENFILE = 6;
  AO_EFILEEXISTS = 7;

  AO_EFAIL = 100;

  AO_FMT_LITTLE = 1;
  AO_FMT_BIG = 2;
  AO_FMT_NATIVE = 4;

type

  PPChar = array[0..0] of PChar;

  PAOInfo = ^ao_info;
  PPAOInfo = ^PAOInfo;
  ao_info = record
    _type : Integer;    // live output or file output?
    name : PChar;       // full name of driver
    short_name : PChar; // short name of driver */
    author : PChar;     // driver author
    comment : PChar;    // driver comment
    preferred_byte_format : Integer;
    priority : Integer;
    options : PPChar;
    option_count : Integer;
  end;

  PAOFunctions = ^ao_functions;

  PAODevice = ^ao_device;
  ao_device = packed record
    _type : Integer;                // live output or file output?
    driver_id : Integer;
    funcs : PAOFunctions;
    _file : PIOFile;                // File for output if this is a file driver
    client_byte_format : Integer;
    machine_byte_format : Integer;
    driver_byte_format : Integer;
    swap_buffer : PChar;
    swap_buffer_size : Integer;     // Bytes allocated to swap_buffer
    internal : Pointer;             // Pointer to driver-specific data
  end;

  PAOSampleFormat = ^ao_sample_format;
  ao_sample_format = record
    bits : Integer;             // bits per sample
    rate : Integer;             // samples per second (in a single channel)
    channels : Integer;         // number of audio channels
    byte_format : Integer;      // Byte ordering in sample, see constants below
  end;

  f_test = function : Integer; cdecl;
  f_driver_info = function : PAOInfo; cdecl;
  f_device_init = function(device : PAODevice) : Integer; cdecl;
  f_set_option = function(device : PAODevice; const key, value : PChar) : Integer; cdecl;
  f_open = function(device : PAODevice) : Integer; cdecl;
  f_play = function(device : PAODevice; const output_samples : PChar; num_bytes : LongWord) : Integer; cdecl;
  f_close = function(device : PAODevice) : Integer; cdecl;
  f_device_clear = procedure(device : PAODevice); cdecl;
  f_file_extension = function : PChar; cdecl;

  ao_functions = packed record
    test : f_test;
    driver_info : f_driver_info;
    device_init : f_device_init;
    set_option : f_set_option;
    open : f_open;
    play : f_play;
    close : f_close;
    device_clear : f_device_clear;
    file_extension : f_file_extension;
  end;

  PPAOOption = ^PAOOption;
  PAOOption = ^ao_option;
  ao_option = record
    key : PChar;
    value : PChar;
    next : PAOOption;
  end;

  (* --- Functions --- *)

  (* library setup/teardown *)

  ao_initialize_t = procedure; cdecl;
  ao_shutdown_t = procedure; cdecl;

  (* device setup/playback/teardown *)
  ao_append_option_t = function(options : PPAOOption; const key, value : PChar) : Integer; cdecl;
  ao_free_options_t = procedure(options : PAOOption); cdecl;
  ao_open_live_t = function(driver_id : Integer; format : PAOSampleFormat; option : PAOOption) : PAODevice; cdecl;
  ao_open_file_t = function(driver_id : Integer; const filename : PChar; overwrite : Integer; format : PAOSampleFormat; option : PAOOption) : PAODevice; cdecl;

  ao_play_t = function(device : PAODevice; output_samples : PChar; num_bytes : LongWord) : Integer; cdecl;
  ao_close_t = function(device : PAODevice) : Integer; cdecl;

  (* driver information *)
  ao_driver_id_t = function(const short_name : PChar) : Integer; cdecl;
  ao_default_driver_id_t = function : Integer; cdecl;
  ao_driver_info_t = function(driver_id : Integer) : PAOInfo; cdecl;
  ao_driver_info_list_t = function(var driver_count : Integer) : PPAOInfo; cdecl;
  // The following function is declared in ao.h but not exported by libao.
  //ao_file_extension_t = function(driver_id : Integer) : PChar; cdecl;

  (* miscellaneous *)
  ao_is_big_endian_t = function : Integer; cdecl;

var

  ao_initialize : ao_initialize_t;
  ao_shutdown : ao_shutdown_t;
  ao_append_option : ao_append_option_t;
  ao_free_options : ao_free_options_t;
  ao_open_live : ao_open_live_t;
  ao_open_file : ao_open_file_t;
  ao_play : ao_play_t;
  ao_close : ao_close_t;
  ao_driver_id : ao_driver_id_t;
  ao_default_driver_id : ao_default_driver_id_t;
  ao_driver_info : ao_driver_info_t;
  ao_driver_info_list : ao_driver_info_list_t;
  //ao_file_extension : ao_file_extension_t;
  ao_is_big_endian : ao_is_big_endian_t;

  procedure FreeOptionsList(var OL : PAOOption);

implementation

var
  Libhandle : Pointer;

{$IFDEF SEARCH_LIBS}
  Path : String;
{$ENDIF}


procedure FreeOptionsList(var OL : PAOOption);
var
  NextOpt, tmp : PAOOption;
begin
  if OL = nil then Exit;
  NextOpt := OL;
  repeat
    tmp := NextOpt;
    NextOpt := NextOpt.next;
    FreeMem(tmp);
  until NextOpt = nil;
end;

initialization

{$IFDEF SEARCH_LIBS}

  Libhandle := nil;
  Path := FindLibs(LibaoPath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(LibaoPath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin
    LibaoLoaded := True;
    ao_initialize := dlsym(Libhandle, 'ao_initialize');
    ao_shutdown := dlsym(Libhandle, 'ao_shutdown');
    ao_append_option := dlsym(Libhandle, 'ao_append_option');
    ao_free_options := dlsym(Libhandle, 'ao_free_options');
    ao_open_live := dlsym(Libhandle, 'ao_open_live');
    ao_open_file := dlsym(Libhandle, 'ao_open_file');
    ao_play := dlsym(Libhandle, 'ao_play');
    ao_close := dlsym(Libhandle, 'ao_close');
    ao_driver_id := dlsym(Libhandle, 'ao_driver_id');
    ao_default_driver_id := dlsym(Libhandle, 'ao_default_driver_id');
    ao_driver_info := dlsym(Libhandle, 'ao_driver_info');
    ao_driver_info_list := dlsym(Libhandle, 'ao_driver_info_list');
    //ao_file_extension := dlsym(Libhandle, 'ao_file_extension');
    ao_is_big_endian := dlsym(Libhandle, 'ao_is_big_endian');
  end;

finalization

  if Libhandle <> nil then
  begin
    dlclose(Libhandle);
  end;

end.
