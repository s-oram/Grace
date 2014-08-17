(*
  This unit contains Musepack encoder library (libmppenc.dll) headers transltion.
  Original Musepack encoder program Copyright (C) 1999-2004 Buschmann/Klemm/Piecha/Wolf.
  You can learn more about Musepack audio compression format at www.musepack.net.
*)

(* $Id: libmppenc.pas 772 2008-09-23 11:26:04Z andrei.borovsky $ *)

unit libmppenc;

interface

uses Windows;

const
  LibMPPEncPath = 'libmppenc.dll';

var
  LibMPPEncLoaded : Boolean = False;

type

  open_callback = procedure(cb_data : Pointer); cdecl;
  read_callback = function(DstBuf : Pointer; _ElementSize, Count : LongWord; cb_data : Pointer): LongWord; cdecl;
  write_callback = function(Buf : Pointer; Size, Count : LongWord; cb_data : Pointer): LongWord; cdecl;
  seek_callback = function(Offset, Origin : Integer; cb_data : Pointer): Integer; cdecl;
  eof_callback = function(cb_data : Pointer): Integer; cdecl;
  close_callback = procedure(cb_data : Pointer); cdecl;

  init_static_t = procedure; cdecl;
  init_enc_state_t = function(var state : Pointer; SampleFreq, PCMSamples, BitsPerSample, Channels : LongWord; Quality : Single) : Integer; cdecl;
  set_callbacks_t = procedure(state : Pointer; cb_input_open : open_callback; cb_input_read : read_callback;
                              cb_input_eof : eof_callback; cb_input_close : close_callback; cb_output_open : open_callback;
                              cb_output_seek : seek_callback; cb_output_read : read_callback;
                              cb_output_write : write_callback; cb_output_close : close_callback; cb_data : Pointer); cdecl;
  add_tag_t = procedure(key : PAnsiChar; key_len : LongWord; value : PAnsiChar; value_len : LongWord); cdecl;
  start_encoder_t = procedure(state : Pointer); cdecl;
  process_block_t = function(state : Pointer) : Integer; cdecl;
  free_encoder_state_t = procedure(state : Pointer); cdecl;

var
  init_static : init_static_t;
  init_enc_state : init_enc_state_t;
  set_callbacks : set_callbacks_t;
  add_tag : add_tag_t;
  start_encoder : start_encoder_t;
  process_block : process_block_t;
  free_encoder_state : free_encoder_state_t;

procedure AddTag(const Key : AnsiString; const Value : WideString);

procedure LoadMPPEncLibrary;
procedure UnloadMPPEncLibrary;

implementation
var
  LibHandle : HMODULE = 0;

procedure AddTag(const Key : AnsiString; const Value : WideString);
var
  S : Utf8String;
begin
  S := Utf8Encode(Value);
  add_tag(PAnsiChar(Key), Length(Key), PAnsiChar(S), Length(S));
end;

procedure LoadMPPEncLibrary;
begin
  LibHandle := LoadLibrary(LibMPPEncPath);
  LibMPPEncLoaded := (LibHandle <> 0);
  if LibMPPEncLoaded then
  begin
    init_static := GetProcAddress(LibHandle, 'init_static');
    init_enc_state := GetProcAddress(LibHandle, 'init_enc_state');
    set_callbacks := GetProcAddress(LibHandle, 'set_callbacks');
    add_tag := GetProcAddress(LibHandle, 'add_tag');
    start_encoder := GetProcAddress(LibHandle, 'start_encoder');
    process_block := GetProcAddress(LibHandle, 'process_block');
    free_encoder_state := GetProcAddress(LibHandle, 'free_encoder_state');
  end;
end;

procedure UnloadMPPEncLibrary;
begin
  FreeLibrary(LibHandle);
end;


initialization

end.
