unit taglib;

(* $Id: taglib.pas 1059 2009-12-16 05:17:16Z andrei.borovsky $ *)

interface

uses
  Windows, ACS_Classes;

const

   LibtagPath = 'libtag_c.dll';

type

  TagLib_File_Type = (
    TagLib_File_MPEG = 0,
    TagLib_File_OggVorbis = 1,
    TagLib_File_FLAC = 2,
    TagLib_File_MPC = 3,
    TagLib_File_OggFlac = 4,
    TagLib_File_WavPack = 5,
    TagLib_File_Speex = 6,
    TagLib_File_TrueAudio = 7,
    TagLib_File_MP4 = 8,
    TagLib_File_ASF = 9
  );

  TagLib_ID3v2_Encoding = (
    TagLib_ID3v2_Latin1 = 0,
    TagLib_ID3v2_UTF16 = 1,
    TagLib_ID3v2_UTF16BE = 2,
    TagLib_ID3v2_UTF8  = 3
  );

  PTagLib_File = Pointer;
  PTagLib_Tag = Pointer;
  PTagLib_AudioProperties = Pointer;

  taglib_audioproperties_bitrate_t = function(AudioProperties : PTagLib_AudioProperties) : Integer; cdecl;
  taglib_audioproperties_channels_t = function(AudioProperties : PTagLib_AudioProperties) : Integer; cdecl;
  taglib_audioproperties_length_t = function(AudioProperties : PTagLib_AudioProperties) : Integer; cdecl;
  taglib_audioproperties_samplerate_t = function(AudioProperties : PTagLib_AudioProperties) : Integer; cdecl;
  taglib_file_audioproperties_t = function(tf : PTagLib_File) : PTagLib_AudioProperties; cdecl;
  taglib_file_free_t = procedure(tf : PTagLib_File); cdecl;
  taglib_file_is_valid_t = function(tf : PTagLib_File) : LongBool; cdecl;
  taglib_file_new_t = function(filename : PAnsiChar) : PTagLib_File; cdecl;
  taglib_file_new_type_t = function(filename : PAnsiChar; filetype : LongWord) : PTagLib_File; cdecl;
  taglib_file_save_t = function(tf : PTagLib_File): LongBool; cdecl;
  taglib_file_tag_t = function(tf : PTagLib_File) : PTagLib_Tag; cdecl;
  taglib_id3v2_set_default_text_encoding_t = procedure(encoding : LongWord); cdecl;
  taglib_set_string_management_enabled_t = procedure(enabled : LongBool); cdecl;
  taglib_set_strings_unicode_t = procedure(unicode : LongBool); cdecl;
  taglib_tag_album_t = function(pt : PTagLib_Tag) : PAnsiChar; cdecl;
  taglib_tag_artist_t = function(pt : PTagLib_Tag) : PAnsiChar; cdecl;
  taglib_tag_comment_t = function(pt : PTagLib_Tag) : PAnsiChar; cdecl;
  taglib_tag_free_strings_t = procedure(); cdecl;
  taglib_tag_genre_t = function(pt : PTagLib_Tag) : PAnsiChar; cdecl;
  taglib_tag_set_album_t = procedure(tag : PTagLib_Tag; title : PAnsiChar); cdecl;
  taglib_tag_set_artist_t = procedure(tag : PTagLib_Tag; title : PAnsiChar); cdecl;
  taglib_tag_set_comment_t = procedure(tag : PTagLib_Tag; title : PAnsiChar); cdecl;
  taglib_tag_set_genre_t = procedure(tag : PTagLib_Tag; genre : PAnsiChar); cdecl;
  taglib_tag_set_title_t = procedure(tag : PTagLib_Tag; title : PAnsiChar); cdecl;
  taglib_tag_set_track_t = procedure(tag : PTagLib_Tag; track : LongWord); cdecl;
  taglib_tag_set_year_t = procedure(tag : PTagLib_Tag; year : LongWord); cdecl;
  taglib_tag_title_t = function(pt : PTagLib_Tag) : PAnsiChar; cdecl;
  taglib_tag_track_t = function(pt : PTagLib_Tag) : LongInt; cdecl;
  taglib_tag_year_t = function(pt : PTagLib_Tag) : LongInt; cdecl;

var

  LibtagLoaded : Boolean = False;

  taglib_audioproperties_bitrate : taglib_audioproperties_bitrate_t;
  taglib_audioproperties_channels : taglib_audioproperties_channels_t;
  taglib_audioproperties_length : taglib_audioproperties_length_t;
  taglib_audioproperties_samplerate : taglib_audioproperties_samplerate_t;
  taglib_file_audioproperties : taglib_file_audioproperties_t;
  taglib_file_free : taglib_file_free_t;
  taglib_file_is_valid : taglib_file_is_valid_t;
  taglib_file_new : taglib_file_new_t;
  taglib_file_new_type : taglib_file_new_type_t;
  taglib_file_save : taglib_file_save_t;
  taglib_file_tag : taglib_file_tag_t;
  taglib_id3v2_set_default_text_encoding : taglib_id3v2_set_default_text_encoding_t;
  taglib_set_string_management_enabled : taglib_set_string_management_enabled_t;
  taglib_set_strings_unicode : taglib_set_strings_unicode_t;
  taglib_tag_album : taglib_tag_album_t;
  taglib_tag_artist : taglib_tag_artist_t;
  taglib_tag_comment : taglib_tag_comment_t;
  taglib_tag_free_strings : taglib_tag_free_strings_t;
  taglib_tag_genre : taglib_tag_genre_t;
  taglib_tag_set_album : taglib_tag_set_album_t;
  taglib_tag_set_artist : taglib_tag_set_artist_t;
  taglib_tag_set_comment : taglib_tag_set_comment_t;
  taglib_tag_set_genre : taglib_tag_set_genre_t;
  taglib_tag_set_title : taglib_tag_set_title_t;
  taglib_tag_set_track : taglib_tag_set_track_t;
  taglib_tag_set_year : taglib_tag_set_year_t;
  taglib_tag_title : taglib_tag_title_t;
  taglib_tag_track : taglib_tag_track_t;
  taglib_tag_year : taglib_tag_year_t;

procedure LoadLibtag;
procedure UnloadLibtag;

implementation

var
  Libhandle : HMODULE;

procedure LoadLibtag;
begin
  LoadLibCS.Enter;
  if LibtagLoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  Libhandle := LoadLibraryEx(LibtagPath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibtagLoaded := True;
    taglib_audioproperties_bitrate := GetProcAddress(Libhandle, 'taglib_audioproperties_bitrate');
    taglib_audioproperties_channels := GetProcAddress(Libhandle, 'taglib_audioproperties_channels');
    taglib_audioproperties_length := GetProcAddress(Libhandle, 'taglib_audioproperties_length');
    taglib_audioproperties_samplerate := GetProcAddress(Libhandle, 'taglib_audioproperties_samplerate');
    taglib_file_audioproperties := GetProcAddress(Libhandle, 'taglib_file_audioproperties');
    taglib_file_free := GetProcAddress(Libhandle, 'taglib_file_free');
    taglib_file_is_valid := GetProcAddress(Libhandle, 'taglib_file_is_valid');
    taglib_file_new := GetProcAddress(Libhandle, 'taglib_file_new');
    taglib_file_new_type := GetProcAddress(Libhandle, 'taglib_file_new_type');
    taglib_file_save := GetProcAddress(Libhandle, 'taglib_file_save');
    taglib_file_tag := GetProcAddress(Libhandle, 'taglib_file_tag');
    taglib_id3v2_set_default_text_encoding := GetProcAddress(Libhandle, 'taglib_id3v2_set_default_text_encoding');
    taglib_set_string_management_enabled := GetProcAddress(Libhandle, 'taglib_set_string_management_enabled');
    taglib_set_strings_unicode := GetProcAddress(Libhandle, 'taglib_set_strings_unicode');
    taglib_tag_album := GetProcAddress(Libhandle, 'taglib_tag_album');
    taglib_tag_artist := GetProcAddress(Libhandle, 'taglib_tag_artist');
    taglib_tag_comment := GetProcAddress(Libhandle, 'taglib_tag_comment');
    taglib_tag_free_strings := GetProcAddress(Libhandle, 'taglib_tag_free_strings');
    taglib_tag_genre := GetProcAddress(Libhandle, 'taglib_tag_genre');
    taglib_tag_set_album := GetProcAddress(Libhandle, 'taglib_tag_set_album');
    taglib_tag_set_artist := GetProcAddress(Libhandle, 'taglib_tag_set_artist');
    taglib_tag_set_comment := GetProcAddress(Libhandle, 'taglib_tag_set_comment');
    taglib_tag_set_genre := GetProcAddress(Libhandle, 'taglib_tag_set_genre');
    taglib_tag_set_title := GetProcAddress(Libhandle, 'taglib_tag_set_title');
    taglib_tag_set_track := GetProcAddress(Libhandle, 'taglib_tag_set_track');
    taglib_tag_set_year := GetProcAddress(Libhandle, 'taglib_tag_set_year');
    taglib_tag_title := GetProcAddress(Libhandle, 'taglib_tag_title');
    taglib_tag_track := GetProcAddress(Libhandle, 'taglib_tag_track');
    taglib_tag_year := GetProcAddress(Libhandle, 'taglib_tag_year');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadLibtag;
begin
  if Libhandle <> 0 then FreeLibrary(Libhandle);
  LibtagLoaded := False;
end;


end.
