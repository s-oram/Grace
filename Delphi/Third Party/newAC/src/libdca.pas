(*
  This file is a part of New Audio Components package 2.1
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: libdca.pas 865 2009-08-09 19:24:22Z andrei.borovsky $ *)

unit libdca;

interface

uses
   SysUtils, Windows, ACS_Classes;

  const

    LibDCAPath = 'libdca.dll';

    MM_ACCEL_X86_MMX = $80000000;
    MM_ACCEL_X86_3DNOW  = $40000000;
    MM_ACCEL_X86_MMXEXT	= $20000000;

  var
    mm_accel : LongWord;

  type

    sample_t = Single;
    psample_t = ^sample_t;
    level_t = Single;
    pdca_state = Pointer;

  const

     DCA_MONO = 0;
     DCA_CHANNEL = 1;
     DCA_STEREO =  2;
     DCA_STEREO_SUMDIFF = 3;
     DCA_STEREO_TOTAL = 4;
     DCA_3F = 5;
     DCA_2F1R = 6;
     DCA_3F1R = 7;
     DCA_2F2R  = 8;
     DCA_3F2R = 9;
     DCA_4F2R = 10;

     DCA_CHANNEL_MAX = DCA_3F2R;
     DCA_CHANNEL_BITS = 6;
     DCA_CHANNEL_MASK = $3f;
     DCA_LFE = $80;
     DCA_ADJUST_LEVEL = $100;

  type

     nrg_callback = function(level : level_t; data : Pointer) : level_t; cdecl;

     dca_init_t = function(mm_accel : LongWord) : pdca_state; cdecl;
     dca_syncinfo_t = function(state : pdca_state; buf : PByte; var flags, sample_rate, bit_rate, frame_length : Integer) : Integer; cdecl;
     dca_frame_t = function(state : pdca_state; buf : PByte; var flags : Integer; var level : level_t; var bias : sample_t) : Integer; cdecl;
     dca_dynrng_t = procedure(state : pdca_state; call : nrg_callback; data : Pointer); cdecl;
     dca_blocks_num_t = function(state : pdca_state) : Integer; cdecl;
     dca_block_t = function(state : pdca_state) : Integer; cdecl;
     dca_samples_t = function(state : pdca_state) : psample_t; cdecl;
     dca_free_t = procedure(state : pdca_state); cdecl;

  var

    LibDCALoaded : Boolean = False;

    dca_init : dca_init_t;
    dca_syncinfo : dca_syncinfo_t;
    dca_frame : dca_frame_t;
    dca_dynrng : dca_dynrng_t;
    dca_blocks_num : dca_blocks_num_t;
    dca_block : dca_block_t;
    dca_samples : dca_samples_t;
    dca_free : dca_free_t;

procedure LoadDCALib;
procedure UnloadDCALib;

implementation

var
  Libhandle : HMODULE;

procedure LoadDCALib;
begin
  LoadLibCS.Enter;
  if LibDCALoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  Libhandle := LoadLibraryEx(LibDCAPath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibDCALoaded := True;

    dca_init := GetProcAddress(Libhandle, 'dca_init');
    dca_syncinfo := GetProcAddress(Libhandle, 'dca_syncinfo');
    dca_frame := GetProcAddress(Libhandle, 'dca_frame');
    dca_dynrng := GetProcAddress(Libhandle, 'dca_dynrng');
    dca_blocks_num := GetProcAddress(Libhandle, 'dca_blocks_num');
    dca_block := GetProcAddress(Libhandle, 'dca_block');
    dca_samples := GetProcAddress(Libhandle, 'dca_samples');
    dca_free := GetProcAddress(Libhandle, 'dca_free');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadDCALib;
begin
  if Libhandle <> 0 then FreeLibrary(Libhandle);
  LibDCALoaded := False;
end;



end.
