(*
** The libsamplerate headers Delphi translation
** By Andrei Borovsky <anb@symmetrica.net>.
**
** The original headers are
** copyright (C) 2002-2004 Erik de Castro Lopo <erikd@mega-nerd.com>
**
*)

unit libsamplerate;

(* Unit: libsamplerate.pas
    Delphi headers for libsamplerate.dll by Andrei Borovsky
(anb@symmetrica.net) The original headers are copyright (C) 2002-2004 Erik de
Castro Lopo (erikd@mega-nerd.com). *)

interface

uses
  Windows, ACS_Classes;

const

  LibsampleratePath = 'libsamplerate.dll';

type

  FloatArray = array[0..0] of Single;

  PFLOATARRAY = ^FloatArray;

  PPFLOATARRAY = ^PFLOATARRAY;

  ShortArray =  array[0..0] of SmallInt;

  PSHORTARRAY = ^ShortArray;

  PSRC_STATE = Pointer;

  SRC_DATA = record
    data_in, data_out : PFLOATARRAY;
    input_frames, output_frames : LongWord;
    nput_frames_used, output_frames_gen : LongWord;
    end_of_input : Integer;
    src_ratio  : Double;
  end;

  PSRC_DATA = ^SRC_DATA;

  SRC_CB_DATA = record
    frames : LongWord;
    data_in  : PFLOATARRAY;
  end;

  PSRC_CB_DATA = ^SRC_CB_DATA;

(*
** User supplied callback function type for use with src_callback_new()
** and src_callback_read(). First parameter is the same pointer that was
** passed into src_callback_new(). Second parameter is pointer to a
** pointer. The user supplied callback function must modify *data to
** point to the start of the user supplied float array. The user supplied
** function must return the number of frames that **data points to.
*)

 TSrcCallback = function(cb_data : Pointer; var data : PFLOATARRAY) : LongWord; cdecl;

const

  // Converter type constants
  SRC_SINC_BEST_QUALITY		= 0;
  SRC_SINC_MEDIUM_QUALITY	= 1;
  SRC_SINC_FASTEST = 2;
  SRC_ZERO_ORDER_HOLD	= 3;
  SRC_LINEAR 	= 4;

type

(*
**	Standard initialisation function : return an anonymous pointer to the
**	internal state of the converter. Choose a converter from the enums below.
**	Error returned in *error.
*)

  src_new_t = function(converter_type, channels : Integer; var error : Integer) : PSRC_STATE cdecl;

(*
**	Initilisation for callback based API : return an anonymous pointer to the
**	internal state of the converter. Choose a converter from the enums below.
**	The cb_data pointer can point to any data or be set to NULL. Whatever the
**	value, when processing, user supplied function "func" gets called with
**	cb_data as first parameter.
*)

  src_callback_new_t = function(func : TSrcCallback; converter_type, channels : Integer;
				var error : Integer; cb_data : Pointer) : PSRC_STATE cdecl;

(*
**	Cleanup all internal allocations.
**	Always returns NULL.
*)

  src_delete_t = function (state : PSRC_STATE) : PSRC_STATE; cdecl;

(*
**	Standard processing function.
**	Returns non zero on error.
*)

  src_process_t = function(state : PSRC_STATE; var data : SRC_DATA) : Integer; cdecl;

(*
**	Callback based processing function. Read up to frames worth of data from
**	the converter int *data and return frames read or -1 on error.
*)

  src_callback_read_t = function(state : PSRC_STATE; src_ratio : Double; frames : LongWord; data : PFLOATARRAY) : LongWord cdecl;

(*
**	Simple interface for performing a single conversion from input buffer to
**	output buffer at a fixed conversion ratio.
**	Simple interface does not require initialisation as it can only operate on
**	a single buffer worth of audio.
*)

 src_simple_t = function(data : PSRC_DATA; converter_type, channels : Integer) : Integer; cdecl;

(*
** This library contains a number of different sample rate converters,
** numbered 0 through N.
**
** Return a string giving either a name or a more full description of each
** sample rate converter or NULL if no sample rate converter exists for
** the given value. The converters are sequentially numbered from 0 to N.
*)

  src_get_name_t = function(converter_type : Integer) : PChar; cdecl;
  src_get_description_t = function(converter_type : Integer) : PChar; cdecl;
  src_get_version_t = function : PChar; cdecl;

(*
**	Set a new SRC ratio. This allows step responses
**	in the conversion ratio.
**	Returns non zero on error.
*)

  src_set_ratio_t = function(state : PSRC_STATE; new_ratio : Double) : Integer cdecl;

(*
**	Reset the internal SRC state.
**	Does not modify the quality settings.
**	Does not free any memory allocations.
**	Returns non zero on error.
*)

  src_reset_t = function(state : PSRC_STATE) : Integer; cdecl;

(*
** Return TRUE if ratio is a valid conversion ratio, FALSE
** otherwise.
*)

 src_is_valid_ratio_t = function(ratio : Double) : Integer; cdecl;

(*
**	Return an error number.
*)

  src_error_t = function(state : PSRC_STATE) : Integer; cdecl;

(*
**	Convert the error number into a string.
*)

  src_strerror_t = function(error : Integer) : PChar; cdecl;

(*
** Extra helper functions for converting from short to float and
** back again.
*)

  src_short_to_float_array_t = procedure(_in : PSHORTARRAY; _out : PFLOATARRAY; len : Integer); cdecl;
  src_float_to_short_array_t = procedure(_in : PFLOATARRAY; _out : PSHORTARRAY; len : Integer); cdecl;

var

  LibsamplerateLoaded : Boolean = False;

  src_new : src_new_t;
  src_callback_new : src_callback_new_t;
  src_delete : src_delete_t;
  src_process : src_process_t;
  src_callback_read : src_callback_read_t;
  src_simple : src_simple_t;
  src_get_name : src_get_name_t;
  src_get_description : src_get_description_t;
  src_get_version : src_get_version_t;
  src_set_ratio : src_set_ratio_t;
  src_reset: src_reset_t;
  src_is_valid_ratio : src_is_valid_ratio_t;
  src_error : src_error_t;
  src_strerror : src_strerror_t;
  src_short_to_float_array : src_short_to_float_array_t;
  src_float_to_short_array : src_float_to_short_array_t;

procedure LoadLibsamplerate;

implementation

var
  Libhandle : HMODULE;

procedure LoadLibsamplerate;
begin
  LoadLibCS.Enter;
  if LibsamplerateLoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;

  Libhandle := LoadLibraryEx(LibsampleratePath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibsamplerateLoaded := True;
    src_new := GetProcAddress(Libhandle, 'src_new');
    src_callback_new := GetProcAddress(Libhandle, 'src_callback_new_t');
    src_delete := GetProcAddress(Libhandle, 'src_delete');
    src_process := GetProcAddress(Libhandle, 'src_process');
    src_callback_read := GetProcAddress(Libhandle, 'src_callback_read');
    src_simple := GetProcAddress(Libhandle, 'src_simple');
    src_get_name := GetProcAddress(Libhandle, 'src_get_name');
    src_get_description := GetProcAddress(Libhandle, 'src_get_description');
    src_get_version := GetProcAddress(Libhandle, 'src_get_version');
    src_set_ratio := GetProcAddress(Libhandle, 'src_set_ratio');
    src_reset := GetProcAddress(Libhandle, 'src_reset');
    src_is_valid_ratio := GetProcAddress(Libhandle, 'src_is_valid_ratio');
    src_error := GetProcAddress(Libhandle, 'src_error');
    src_strerror := GetProcAddress(Libhandle, 'src_strerror');
    src_short_to_float_array := GetProcAddress(Libhandle, 'src_short_to_float_array');
    src_float_to_short_array := GetProcAddress(Libhandle, 'src_float_to_short_array');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadLibsamplerate;
begin
  if Libhandle <> 0 then FreeLibrary(Libhandle);
  LibsamplerateLoaded := False;
end;


end.
