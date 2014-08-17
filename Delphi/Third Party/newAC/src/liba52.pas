(*
  This file is a part of New Audio Components package 2.2
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: liba52.pas 888 2009-08-21 14:37:55Z andrei.borovsky $ *)

unit liba52;

interface

uses
   SysUtils, Windows, ACS_Classes;

  const

    LibA52Path = 'liba52.dll';

    A52_ACCEL_X86_MMX = $1;
    A52_ACCEL_X86_3DNOW  = $2;
    A52_ACCEL_X86_MMXEXT	= $4;
    A52_ACCEL_DETECT = $80000000;

  var
    mm_accel : LongWord;

  type

    sample_t = Single;
    psample_t = ^sample_t;
    level_t = Single;
    pa52_state = Pointer;

  const

     A52_CHANNEL = 0;
     A52_MONO = 1;
     A52_STEREO = 2;
     A52_3F = 3;
     A52_2F1R = 4;
     A52_3F1R = 5;
     A52_2F2R = 6;
     A52_3F2R = 7;
     A52_CHANNEL1 = 8;
     A52_CHANNEL2 = 9;
     A52_DOLBY = 10;
     A52_CHANNEL_MASK = 15;

     A52_LFE = 16;
     A52_ADJUST_LEVEL = 32;

  type

     nrg_callback = function(level : level_t; data : Pointer) : level_t; cdecl;

     a52_accel_t = function(accel : LongWord) : LongWord; cdecl;
     a52_init_t = function : pa52_state; cdecl;
     a52_syncinfo_t = function(buf : PByte; var flags, sample_rate, bit_rate) : Integer; cdecl;
     a52_frame_t = function(state : pa52_state; buf : PByte; var flags : Integer; var level : level_t; var bias : sample_t) : Integer; cdecl;
     a52_dynrng_t = procedure(state : pa52_state; call : nrg_callback; data : Pointer); cdecl;
     a52_block_t = function(state : pa52_state) : Integer; cdecl;
     a52_samples_t = function(state : pa52_state) : psample_t; cdecl;
     a52_free_t = procedure(state : pa52_state); cdecl;

  var

    LibA52Loaded : Boolean = False;

    a52_accel : a52_accel_t;
    a52_init : a52_init_t;
    a52_syncinfo : a52_syncinfo_t;
    a52_frame : a52_frame_t;
    a52_dynrng : a52_dynrng_t;
    a52_block : a52_block_t;
    a52_samples : a52_samples_t;
    a52_free : a52_free_t;

procedure LoadA52Lib;
procedure UnloadA52Lib;

implementation

var
  Libhandle : HMODULE;

procedure LoadA52Lib;
begin
  LoadLibCS.Enter;
  if LibA52Loaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  Libhandle := LoadLibraryEx(LibA52Path, 0, 0);
  if Libhandle <> 0 then
  begin
    LibA52Loaded := True;

    a52_accel := GetProcAddress(Libhandle, 'a52_accel');
    a52_init := GetProcAddress(Libhandle, 'a52_init');
    a52_syncinfo := GetProcAddress(Libhandle, 'a52_syncinfo');
    a52_frame := GetProcAddress(Libhandle, 'a52_frame');
    a52_dynrng := GetProcAddress(Libhandle, 'a52_dynrng');
    a52_block := GetProcAddress(Libhandle, 'a52_block');
    a52_samples := GetProcAddress(Libhandle, 'a52_samples');
    a52_free := GetProcAddress(Libhandle, 'a52_free');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadA52Lib;
begin
  if Libhandle <> 0 then FreeLibrary(Libhandle);
  LibA52Loaded := False;
end;



end.
