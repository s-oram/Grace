(*
  Delphi headers for ovencode.dll
  (c) 2007 Andrei Borovsky, <anb@symmetrica.net>

*)

(* $Revision: 1.1 $ $Date: 2007/06/20 00:42:14 $ *)

unit ovencode;

(* Unit: ovencode
    Delphi headers for ovencode.dll (c) 2007 Andrei Borovsky,
    (anb@symmetrica.net). For code that has been baked in an oven. (ogg vorbis
    encode?) *)

interface

uses

  {$IFDEF WIN32}
  Windows;
  {$ENDIF}

type

   oe_enc_opt = Pointer;

   // callbacks
   // Callbacks are the driving force of this API.
   // You have to provide them properly.
   // cb_data points to an arbitrary piece of data that you provided when registering callbacks.
   // Note that callbacks are declared using stdcall calling convention
   // while API functions are called using cdecl.

   // This function is called to notify the program when encoding has started.

   enc_start_func = procedure(cb_data : Pointer); stdcall;

   // This function is called by the encoder when it needs to get more raw samples.
   // The function should return the number of bytes it has actualy read.
   // When it returns 0 the encoding process stops.

   audio_read_func = function(cb_data : Pointer; buf : Pointer; bufsize : Integer) : Integer stdcall;

   // The encoder calls this function when it has some data for output.

   output_write_func = procedure(cb_data : Pointer; buf : Pointer; bufsize : Integer); stdcall;

   // This function is called to notify the program of encoding progress.
   // (samples/totalsamples)*100 gives you the progresss in percents.

   progress_func = procedure(cb_data : Pointer; totalsamples : Integer; samples : Integer) stdcall;

   // This function is called to notify the program when encoding is done.

   enc_end_func = procedure(cb_data : Pointer; rate : Integer;
                                samples : Integer; bytes : Integer) stdcall;

   // This function informs the program about the erros occured during encoding.

   error_func = procedure(cb_data : Pointer; errormessage : PChar); stdcall;

   // API functions. Should be called in the same order they are listed below

   oe_encode_state_init_t = function : oe_enc_opt cdecl;

   // See the callbacks description above.

   oe_set_callbacks_t = procedure(encode_state : oe_enc_opt; cb_data : Pointer; start_encode : enc_start_func;
				  read_samples : audio_read_func; write_output : output_write_func;
                                  progress_update : progress_func; end_encode : enc_end_func;
                                  error : error_func); cdecl;

   // You can pass a random number as serialno.

   oe_set_serialno_t = procedure(encode_state : oe_enc_opt; serialno : LongWord); cdecl;

   // This function will add some text tags to the output ogg file.
   // You have to call it even if you don't want to add any tags.
   // You can pass nils to all or any of its parameters.

   oe_add_comments_t = procedure(encode_state : oe_enc_opt; artist, album, title, tracknum,
				 date, genre : PChar); cdecl;

   // Channes value should be betweeen 1 (mono) and 255 (don't know what).
   // Allowed sample_size values are 8, 16, and 24.

   oe_set_audio_params_t = procedure(encode_state : oe_enc_opt;
                                     samplerate, channels, sample_size : Integer); cdecl;

   oe_set_managed_t = procedure(encode_state : oe_enc_opt; managed : LongBool); cdecl;

   // Bitrates are set in kbps. The reasonable values are between 16 and 499.
   // Set min_bitrate and max_bitrate to -1 if you want to specify the nominal bitrate only.
   // If you set the min_bitrate or max_bitrate to any value other than -1, set managed to TRUE
   // (see the function above).

   oe_set_bitrates_t = procedure(encode_state : oe_enc_opt; nominal_bitrate,
                                 min_bitrate, max_bitrate : Integer); cdecl;

   // You can specify quality (in the range of [0.1 .. 1.0]) instead of setting bitrates.

   oe_set_quality_t = procedure(encode_state : oe_enc_opt; quality : Single); cdecl;

   // The total number of samples to incode IN A SINGLE CHANNEL

   oe_set_total_samples_t = procedure(encode_state : oe_enc_opt; total_samples : Integer); cdecl;

   // The actual encoding process starts when this function is called
   // and ends when it returns.
   // The function returns 0 on success and other values on error.
   // The error callback should give you some info on what exactly went wrong.
   // One particular case is setting too high an output bitrate for an audio input with
   // a low bitrate (like setting 128 kbps for the audio input with 8 KHz sample rate).
   // For low input bitrates it is a good rule to set an output bitrate to no more
   // than 1/4 of the input bitrate (that's a compression codec after all ;-)).
   // The input bitrate is calculated as sample_rate*bits_per_sample*channels.
   // The error callback will tell you if you have requested too high a bitrate.

   oe_encode_t =  function(encode_state : oe_enc_opt) : Integer; cdecl;

   oe_encode_state_free_t = procedure(encode_state : oe_enc_opt); cdecl;

var

  LibovencodeLoaded : Boolean = False;

  oe_encode_state_init : oe_encode_state_init_t;
  oe_set_callbacks : oe_set_callbacks_t;
  oe_set_serialno : oe_set_serialno_t;
  oe_add_comments : oe_add_comments_t;
  oe_set_audio_params : oe_set_audio_params_t;
  oe_set_managed : oe_set_managed_t;
  oe_set_bitrates : oe_set_bitrates_t;
  oe_set_quality : oe_set_quality_t;
  oe_set_total_samples : oe_set_total_samples_t;
  oe_encode : oe_encode_t;
  oe_encode_state_free : oe_encode_state_free_t;

implementation

{$IFDEF WIN32}

const

  LibovencodePath = 'ovencode.dll';

var

  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(LibovencodePath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibovencodeLoaded := True;
    oe_encode_state_init := GetProcAddress(Libhandle, 'oe_encode_state_init');
    oe_set_callbacks := GetProcAddress(Libhandle, 'oe_set_callbacks');
    oe_set_serialno := GetProcAddress(Libhandle, 'oe_set_serialno');
    oe_add_comments := GetProcAddress(Libhandle, 'oe_add_comments');
    oe_set_audio_params := GetProcAddress(Libhandle, 'oe_set_audio_params');
    oe_set_managed := GetProcAddress(Libhandle, 'oe_set_managed');
    oe_set_bitrates := GetProcAddress(Libhandle, 'oe_set_bitrates');
    oe_set_quality := GetProcAddress(Libhandle, 'oe_set_quality');
    oe_set_total_samples := GetProcAddress(Libhandle, 'oe_set_total_samples');
    oe_encode := GetProcAddress(Libhandle, 'oe_encode');
    oe_encode_state_free := GetProcAddress(Libhandle, 'oe_encode_state_free');
  end;

finalization

  if libhandle <> 0 then FreeLibrary(Libhandle);

{$ENDIF}


end.
