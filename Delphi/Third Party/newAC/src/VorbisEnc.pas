(*
  Delphi/Kylix headers for Ogg Vorbis software codec.
  Translated from vorbisenc.h header
  by Andrei Borovsky, anb@symmetrica.net
  The original C/C++ headers and libraries (C) COPYRIGHT 1994-2001
  by the XIPHOPHORUS Company http://www.xiph.org/
*)

(* $Id: VorbisEnc.pas 647 2008-07-02 05:12:26Z andrei.borovsky $ *)

unit VorbisEnc;

(* Unit: VorbisEnc.pas
    Delphi/Kylix headers for Ogg Vorbis software codec. Translated from
    vorbisenc.h header by Andrei Borovsky (anb@symmetrica.net)
    
    The original C/C++ headers and libraries (C) COPYRIGHT 1994-2001 by the
    XIPHOPHORUS Company http://www.xiph.org/. *)

interface

uses

  ACS_Procs, ACS_Classes,

  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}

  Codec;


const

{$IFDEF LINUX}
  LibvorbisencPath = 'libvorbisenc.so*'; //'/usr/lib/libvorbisenc.so';
  {$DEFINE SEARCH_LIBS}
{$ENDIF}

{$IFDEF WIN32}
  LibvorbisencPath = 'vorbisenc.dll';
{$ENDIF}


OV_ECTL_RATEMANAGE2_GET = $14;
OV_ECTL_RATEMANAGE2_SET = $15;


var
  LibvorbisencLoaded : Boolean = False;

type

  vorbis_encode_init_t = function(vi: PVORBIS_INFO;
                            channels: LongInt;
                            rate: LongInt;
                            max_bitrate: LongInt;
                            nominal_bitrate: LongInt;
                            min_bitrate: LongInt): Integer cdecl;

  vorbis_encode_setup_managed_t = function(vi: PVORBIS_INFO;
                                     channels: LongInt;
                                     rate: LongInt;
                                     max_bitrate: LongInt;
                                     nominal_bitrate: LongInt;
                                     min_bitrate: LongInt): Integer; cdecl;

  vorbis_encode_setup_vbr_t = function(vi: PVORBIS_INFO;
                                 channels: LongInt;
                                 rate: LongInt;
                                 fl: Single): Integer; cdecl;

  vorbis_encode_init_vbr_t = function(vi: PVORBIS_INFO;
                                channels: LongInt;
                                rate: LongInt;
                                base_quality: Single): Integer; cdecl;

  vorbis_encode_setup_init_t = function(vi: PVORBIS_INFO): Integer; cdecl;

  vorbis_encode_ctl_t = function(vi: PVORBIS_INFO;
                           number: Integer;
                           arg: Pointer): Integer; cdecl;

ovectl_ratemanage2_arg = record
  management_active : Integer;
  bitrate_limit_min_kbps : LongWord;
  bitrate_limit_max_kbps : LongWord;
  bitrate_limit_reservoir_bits : LongWord;
  bitrate_limit_reservoir_bias : Double;
  bitrate_average_kbps : LongWord;
  bitrate_average_damping : Double;
end;


var

  vorbis_encode_init : vorbis_encode_init_t;

  vorbis_encode_setup_managed : vorbis_encode_setup_managed_t;

  vorbis_encode_setup_vbr : vorbis_encode_setup_vbr_t;

  vorbis_encode_init_vbr : vorbis_encode_init_vbr_t;

  vorbis_encode_setup_init : vorbis_encode_setup_init_t;

  vorbis_encode_ctl : vorbis_encode_ctl_t;

  procedure LoadVorbisEncLib;
  procedure UnloadVorbisEncLib;

implementation

var
  Libhandle : HMODULE;

procedure LoadVorbisEncLib;
begin
  LoadLibCS.Enter;
  if LibvorbisencLoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  Libhandle := LoadLibraryEx(LibvorbisencPath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibvorbisencLoaded := True;
    vorbis_encode_init := GetProcAddress(Libhandle, 'vorbis_encode_init');
    vorbis_encode_setup_managed := GetProcAddress(Libhandle, 'vorbis_encode_setup_managed');
    vorbis_encode_setup_vbr := GetProcAddress(Libhandle, 'vorbis_encode_setup_vbr');
    vorbis_encode_init_vbr := GetProcAddress(Libhandle, 'vorbis_encode_init_vbr');
    vorbis_encode_setup_init := GetProcAddress(Libhandle, 'vorbis_encode_setup_init');
    vorbis_encode_ctl := GetProcAddress(Libhandle, 'vorbis_encode_ctl');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadVorbisEncLib;
begin
  if libhandle <> 0 then FreeLibrary(Libhandle);
  LibvorbisencLoaded := False;
end;

end.
