(*
  This file is a part of New Audio Components package v 2.6
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: mp4ff.pas 1225 2010-07-17 22:36:06Z andrei.borovsky $ *)

unit mp4ff;
interface

uses
  Windows,
  SysUtils,
  ACS_Classes,
  neaac;

const
  Libmp4ffPath = 'mp4ff.dll';

var
  Libmp4ffLoaded : Boolean = False;


type
read_callback_t = function(user_data : Pointer; buffer : Pointer; length : LongWord) : LongWord; cdecl;
write_callback_t = function(user_data : Pointer; buffer : Pointer; length : LongWord) : LongWord; cdecl;
seek_callback_t = function(user_data : Pointer; Position : Int64) : LongWord; cdecl;
truncate_callback_t = function(user_data : Pointer) : LongWord; cdecl;

mp4ff_callback_t = record
  read : read_callback_t;
  write : write_callback_t;
    seek : seek_callback_t;
    truncate : truncate_callback_t;
    user_data : Pointer;
end;

p_mp4ff_callback_t = ^mp4ff_callback_t;
mp4ff_t = Pointer;
int32_t = Integer;
int64_t = Int64;
uint32_t = LongWord;

mp4ff_open_read_t = function(f : p_mp4ff_callback_t) : mp4ff_t; cdecl;
mp4ff_open_read_metaonly_t = function(f : p_mp4ff_callback_t) : mp4ff_t; cdecl;
mp4ff_close_t = procedure(f : mp4ff_t); cdecl;
mp4ff_get_sample_duration_t = function(f : mp4ff_t; track, sample : int32_t) : int32_t; cdecl;
mp4ff_get_sample_duration_use_offsets_t = function(f : mp4ff_t; track, sample : int32_t) : int32_t; cdecl;
mp4ff_get_sample_position_t = function(f : mp4ff_t; track, sample : int32_t) : int64_t; cdecl;
mp4ff_get_sample_offset_t = function(f : mp4ff_t; track, sample : int32_t) : int32_t; cdecl;
mp4ff_find_sample_t = function(f : mp4ff_t; track : int32_t;  offset : int64_t; var toskip : int32_t) : int32_t; cdecl;
mp4ff_find_sample_use_offsets_t = function(f : mp4ff_t; track : int32_t; offset : int64_t; var toskip : int32_t) : int32_t; cdecl;
mp4ff_set_sample_position_t = function(f : mp4ff_t; track : int32_t; sample : int64_t) : int32_t; cdecl;
mp4ff_read_sample_t = function(f : mp4ff_t; track, sample  : int32_t;
                          var audio_buffer : PByte; var bytes : LongWord) : int32_t; cdecl;

mp4ff_read_sample_v2_t = function(f : mp4ff_t; track, sample : int32_t; buffer : PByte): int32_t; cdecl; //returns 0 on error, number of bytes read on success, use mp4ff_read_sample_getsize_t = function() to check buffer size needed
mp4ff_read_sample_getsize_t = function(f : mp4ff_t; track, sample : Integer) : int32_t; cdecl; //returns 0 on error, buffer size needed for mp4ff_read_sample_v2_t = function() on success

mp4ff_get_decoder_config_t = function(f : mp4ff_t; track : Integer; var ppBuf : PByte; var pBufSize : LongWord) : int32_t; cdecl;
mp4ff_free_decoder_config_t = procedure(Buf : PByte); cdecl;

mp4ff_get_track_type_t = function(f : mp4ff_t; const track : Integer) : int32_t; cdecl;
mp4ff_total_tracks_t = function(f : mp4ff_t) : int32_t; cdecl;
mp4ff_num_samples_t = function(f : mp4ff_t; track : Integer) : int32_t; cdecl;
mp4ff_time_scale_t = function(f : mp4ff_t; track : Integer) : int32_t; cdecl;

mp4ff_get_avg_bitrate_t = function(f : mp4ff_t; track : int32_t) : uint32_t; cdecl;
mp4ff_get_max_bitrate_t = function(f : mp4ff_t; track : int32_t) : uint32_t; cdecl;
mp4ff_get_track_duration_t = function(f : mp4ff_t; track : int32_t) : int64_t; cdecl; //returns _t = function(-1) if unknown
mp4ff_get_track_duration_use_offsets_t = function(f : mp4ff_t; track : int32_t) : Integer; cdecl;  //returns _t = function(-1) if unknown
mp4ff_get_sample_rate_t = function(f : mp4ff_t; track : int32_t) : uint32_t; cdecl;
mp4ff_get_channel_count_t = function(f : mp4ff_t; track : int32_t) : uint32_t; cdecl;
mp4ff_get_audio_type_t = function(f : mp4ff_t; track : int32_t) : uint32_t; cdecl;

//* metadata */
mp4ff_meta_get_num_items_t = function(f : mp4ff_t) : Integer; cdecl;
mp4ff_meta_get_by_index_t = function(f : mp4ff_t; index : LongWord;
                            var item, value : PChar) : Integer; cdecl;
mp4ff_meta_get_title_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_artist_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_writer_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_album_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_date_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_tool_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_comment_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_genre_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_track_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_disc_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_totaltracks_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_totaldiscs_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_compilation_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_tempo_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;
mp4ff_meta_get_coverart_t = function(f : mp4ff_t; var value : PAnsiChar) : Integer; cdecl;

{$IFDEF USE_TAGGING}

//* metadata tag structure */
mp4ff_tag_t = record
    item : PChar;
    value : PChar;
end;

mp4ff_tag_t = ^mp4ff_tag_t;


//* metadata list structure */
mp4ff_metadata_t = record
    tags : mp4ff_tag_t;
    count : uint32_t;
end;

mp4ff_meta_update_t = funtion(f : p_mp4ff_callback_t; data : p_mp4ff_metadata_t) : int32_t; cdecl;

{$ENDIF}

var
  mp4ff_open_read : mp4ff_open_read_t;
  mp4ff_open_read_metaonly : mp4ff_open_read_metaonly_t;
  mp4ff_close : mp4ff_close_t;
  mp4ff_get_sample_duration : mp4ff_get_sample_duration_t;
  mp4ff_get_sample_duration_use_offsets : mp4ff_get_sample_duration_use_offsets_t;
  mp4ff_get_sample_position : mp4ff_get_sample_position_t;
  mp4ff_get_sample_offset : mp4ff_get_sample_offset_t;
  mp4ff_find_sample : mp4ff_find_sample_t;
  mp4ff_find_sample_use_offsets : mp4ff_find_sample_use_offsets_t;
  mp4ff_set_sample_position : mp4ff_set_sample_position_t;

  mp4ff_read_sample : mp4ff_read_sample_t;

  mp4ff_read_sample_v2 : mp4ff_read_sample_v2_t;
  mp4ff_read_sample_getsize : mp4ff_read_sample_getsize_t;

  mp4ff_get_decoder_config : mp4ff_get_decoder_config_t;
  mp4ff_get_track_type : mp4ff_get_track_type_t;
  mp4ff_total_tracks : mp4ff_total_tracks_t;
  mp4ff_num_samples : mp4ff_num_samples_t;
  mp4ff_time_scale : mp4ff_time_scale_t;

  mp4ff_get_avg_bitrate : mp4ff_get_avg_bitrate_t;
  mp4ff_get_max_bitrate : mp4ff_get_max_bitrate_t;
  mp4ff_get_track_duration : mp4ff_get_track_duration_t;
  mp4ff_get_track_duration_use_offsets : mp4ff_get_track_duration_use_offsets_t;
  mp4ff_get_sample_rate : mp4ff_get_sample_rate_t;
  mp4ff_get_channel_count : mp4ff_get_channel_count_t;
  mp4ff_get_audio_type : mp4ff_get_audio_type_t;
  mp4ff_free_decoder_config : mp4ff_free_decoder_config_t;

//* metadata */
  mp4ff_meta_get_num_items : mp4ff_meta_get_num_items_t;
  mp4ff_meta_get_by_index : mp4ff_meta_get_by_index_t;
  mp4ff_meta_get_title : mp4ff_meta_get_title_t;
  mp4ff_meta_get_artist : mp4ff_meta_get_artist_t;
  mp4ff_meta_get_writer : mp4ff_meta_get_writer_t;
  mp4ff_meta_get_album : mp4ff_meta_get_album_t;
  mp4ff_meta_get_date : mp4ff_meta_get_date_t;
  mp4ff_meta_get_tool : mp4ff_meta_get_tool_t;
  mp4ff_meta_get_comment : mp4ff_meta_get_comment_t;
  mp4ff_meta_get_genre : mp4ff_meta_get_genre_t;
  mp4ff_meta_get_track : mp4ff_meta_get_track_t;
  mp4ff_meta_get_disc : mp4ff_meta_get_disc_t;
  mp4ff_meta_get_totaltracks : mp4ff_meta_get_totaltracks_t;
  mp4ff_meta_get_totaldiscs : mp4ff_meta_get_totaldiscs_t;
  mp4ff_meta_get_compilation : mp4ff_meta_get_compilation_t;
  mp4ff_meta_get_tempo : mp4ff_meta_get_tempo_t;
  mp4ff_meta_get_coverart : mp4ff_meta_get_coverart_t;

{$IFDEF USE_TAGGING}
  mp4ff_meta_update : mp4ff_meta_update_t;
{$ENDIF}

function GetAACTrack(infile : mp4ff_t) : Integer;
procedure Loadmp4ff;
procedure Freemp4ff;

implementation

var
  hlib: THandle;

function GetAACTrack(infile : mp4ff_t) : Integer;
var
  i, rc, numTracks : Integer;
  buff : PByte;
  buff_size : LongWord;
  mp4ASC : mp4AudioSpecificConfig;
begin
    numTracks := mp4ff_total_tracks(infile);
    for i := 0 to numTracks - 1 do
    begin
        buff := nil;
        buff_size:=0;
        mp4ff_get_decoder_config(infile, i, buff, buff_size);
        if buff <> nil then
        begin
          rc := NeAACDecAudioSpecificConfig(buff, buff_size, mp4ASC);
          mp4ff_free_decoder_config(buff);
          if rc < 0 then
               continue;
          Result := i;
          Exit;
        end;
    end;
    Result :=  -1;
end;

procedure Freemp4ff;
begin
  LoadLibCS.Enter;
  if Libmp4ffLoaded then
  FreeLibrary(hlib);
  Libmp4ffLoaded := False;
  LoadLibCS.Leave;
end;

procedure Loadmp4ff;
begin
  LoadLibCS.Enter;
  if Libmp4ffLoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  hlib := LoadLibrary(Libmp4ffPath);
  Libmp4ffLoaded := hlib <> 0;


  mp4ff_open_read := GetProcAddress(hlib, 'mp4ff_open_read');
  mp4ff_open_read_metaonly := GetProcAddress(hlib, 'mp4ff_open_read_metaonly');
  mp4ff_close := GetProcAddress(hlib, 'mp4ff_close');
  mp4ff_get_sample_duration := GetProcAddress(hlib, 'mp4ff_get_sample_duration');
  mp4ff_get_sample_duration_use_offsets := GetProcAddress(hlib, 'mp4ff_get_sample_duration_use_offsets');
  mp4ff_get_sample_position := GetProcAddress(hlib, 'mp4ff_get_sample_position');
  mp4ff_get_sample_offset := GetProcAddress(hlib, 'mp4ff_get_sample_offset');
  mp4ff_find_sample := GetProcAddress(hlib, 'mp4ff_find_sample');
  mp4ff_find_sample_use_offsets := GetProcAddress(hlib, 'mp4ff_find_sample_use_offsets');
  mp4ff_set_sample_position := GetProcAddress(hlib, 'mp4ff_set_sample_position');

  mp4ff_read_sample := GetProcAddress(hlib, 'mp4ff_read_sample');

  mp4ff_read_sample_v2 := GetProcAddress(hlib, 'mp4ff_read_sample_v2');
  mp4ff_read_sample_getsize := GetProcAddress(hlib, 'mp4ff_read_sample_getsize');

  mp4ff_get_decoder_config := GetProcAddress(hlib, 'mp4ff_get_decoder_config');
  mp4ff_get_track_type := GetProcAddress(hlib, 'mp4ff_get_track_type');
  mp4ff_total_tracks := GetProcAddress(hlib, 'mp4ff_total_tracks');
  mp4ff_num_samples := GetProcAddress(hlib, 'mp4ff_num_samples');
  mp4ff_time_scale := GetProcAddress(hlib, 'mp4ff_time_scale');

  mp4ff_get_avg_bitrate := GetProcAddress(hlib, 'mp4ff_get_avg_bitrate');
  mp4ff_get_max_bitrate := GetProcAddress(hlib, 'mp4ff_get_max_bitrate');
  mp4ff_get_track_duration := GetProcAddress(hlib, 'mp4ff_get_track_duration');
  mp4ff_get_track_duration_use_offsets := GetProcAddress(hlib, 'mp4ff_get_track_duration_use_offsets');
  mp4ff_get_sample_rate := GetProcAddress(hlib, 'mp4ff_get_sample_rate');
  mp4ff_get_channel_count := GetProcAddress(hlib, 'mp4ff_get_channel_count');
  mp4ff_get_audio_type := GetProcAddress(hlib, 'mp4ff_get_audio_type');
  mp4ff_free_decoder_config := GetProcAddress(hlib, 'mp4ff_free_decoder_config');

//* metadata */
  mp4ff_meta_get_num_items := GetProcAddress(hlib, 'mp4ff_meta_get_num_items');
  mp4ff_meta_get_by_index := GetProcAddress(hlib, 'mp4ff_meta_get_by_index');
  mp4ff_meta_get_title := GetProcAddress(hlib, 'mp4ff_meta_get_title');
  mp4ff_meta_get_artist := GetProcAddress(hlib, 'mp4ff_meta_get_artist');
  mp4ff_meta_get_writer := GetProcAddress(hlib, 'mp4ff_meta_get_writer');
  mp4ff_meta_get_album := GetProcAddress(hlib, 'mp4ff_meta_get_album');
  mp4ff_meta_get_date := GetProcAddress(hlib, 'mp4ff_meta_get_date');
  mp4ff_meta_get_tool := GetProcAddress(hlib, 'mp4ff_meta_get_tool');
  mp4ff_meta_get_comment := GetProcAddress(hlib, 'mp4ff_meta_get_comment');
  mp4ff_meta_get_genre := GetProcAddress(hlib, 'mp4ff_meta_get_genre');
  mp4ff_meta_get_track := GetProcAddress(hlib, 'mp4ff_meta_get_track');
  mp4ff_meta_get_disc := GetProcAddress(hlib, 'mp4ff_meta_get_disc');
  mp4ff_meta_get_totaltracks := GetProcAddress(hlib, 'mp4ff_meta_get_totaltracks');
  mp4ff_meta_get_totaldiscs := GetProcAddress(hlib, 'mp4ff_meta_get_totaldiscs');
  mp4ff_meta_get_compilation := GetProcAddress(hlib, 'mp4ff_meta_get_compilation');
  mp4ff_meta_get_tempo := GetProcAddress(hlib, 'mp4ff_meta_get_tempo');
  mp4ff_meta_get_coverart := GetProcAddress(hlib, 'mp4ff_meta_get_coverart');

{$IFDEF USE_TAGGING}
  mp4ff_meta_update := GetProcAddress(hlib, 'mp4ff_meta_update');
{$ENDIF}
  LoadLibCS.Leave;
end;
end.


