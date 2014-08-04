//  ACS (Kylix edition) header for smpeg and SDL libraries.

unit smpeg;

interface

uses
  SysUtils, Libc;

const

  SMPEG_ERROR = -1;
  SMPEG_STOPPED = 0;
  SMPEG_PLAYING = 1;

type

  SDL_AudioSpec = record
    freq: Integer;
    format: Word;
    channels: Byte;
    silence: Byte;
    samples: Word;
    padding: Word;
    size: LongWord;
    callback: Pointer;
    userdata: Pointer;
  end;

  SMPEG_Info = packed record
    has_audio : Integer;
    has_video : Integer;
    width : Integer;
    height: Integer;
    current_frame: Integer;
    current_fps: Double;
    audio_string : array[0..79] of Char;
    audio_current_frame : Integer;
    current_offset: LongWord;
    total_size: LongWord;
    current_time: Double;
    total_time: Double;
  end;

const

  LibsmpegPath = '/usr/lib/libsmpeg.so';
  LibSDLPath = '/usr/lib/libSDL.so';

var
  LibsmpegLoaded : Boolean = False;

type

  SMPEG_new_t = function(const filename : PChar; var info : SMPEG_Info; sdl_audio : Integer): Pointer; cdecl;

  SMPEG_delete_t = procedure(mpeg: Pointer); cdecl;

  SMPEG_wantedSpec_t = function(mpeg: Pointer; var spec: SDL_AudioSpec): Integer; cdecl;

  SMPEG_play_t = procedure(mpeg: Pointer); cdecl;

  SMPEG_status_t = function(mpeg: Pointer) : Integer; cdecl;

  SMPEG_stop_t = procedure(mpeg: Pointer); cdecl;

  SMPEG_playAudio_t = function(mpeg: Pointer; stream: Pointer; len: Integer): Integer; cdecl;

  SMPEG_skip_t = procedure(mpeg : Pointer; Pos : Single); cdecl;

  SMPEG_rewind_t = procedure(mpeg : Pointer); cdecl;

var

  SMPEG_new : SMPEG_new_t;

  SMPEG_delete : SMPEG_delete_t;

  SMPEG_wantedSpec : SMPEG_wantedSpec_t;

  SMPEG_play : SMPEG_play_t;

  SMPEG_status : SMPEG_status_t;

  SMPEG_stop : SMPEG_stop_t;

  SMPEG_playAudio : SMPEG_playAudio_t;

  SMPEG_skip : SMPEG_skip_t;

  SMPEG_rewind : SMPEG_rewind_t;

  procedure LoadLibrary;
  procedure UnloadLibrary;


implementation

type
  SDL_Init_t = function(Flags : LongWord) : Integer; cdecl;
  SDL_Quit_t = procedure; cdecl;

const
  SDL_INIT_AUDIO = $00000010;

var
  Libhandle : Pointer;
  SDLhandle : Pointer;
  SDL_Init : SDL_Init_t;
  SDL_Quit : SDL_Quit_t;

procedure LoadLibrary;
begin
  SDLhandle := dlopen(LibSDLPath, RTLD_NOW or RTLD_GLOBAL);
  if SDLhandle = nil then raise Exception.Create('/usr/lib/libSDL.so could not be loaded');
  SDL_Init := dlsym(SDLhandle, 'SDL_Init');
  SDL_Quit := dlsym(SDLhandle, 'SDL_Quit');
  SDL_Init(SDL_INIT_AUDIO);
  Libhandle := dlopen(LibsmpegPath, RTLD_NOW or RTLD_GLOBAL);
  if Libhandle = nil then raise Exception.Create(LibsmpegPath + ' could not be loaded');
  if Libhandle <> nil then
  begin
    LibsmpegLoaded := True;
    SMPEG_new := dlsym(Libhandle, 'SMPEG_new');
    SMPEG_delete := dlsym(Libhandle, 'SMPEG_delete');
    SMPEG_wantedSpec := dlsym(Libhandle, 'SMPEG_wantedSpec');
    SMPEG_play := dlsym(Libhandle, 'SMPEG_play');
    SMPEG_status := dlsym(Libhandle, 'SMPEG_status');
    SMPEG_stop := dlsym(Libhandle, 'SMPEG_stop');
    SMPEG_playAudio := dlsym(Libhandle, 'SMPEG_playAudio');
    SMPEG_skip := dlsym(Libhandle, 'SMPEG_skip');
    SMPEG_rewind := dlsym(Libhandle, 'SMPEG_rewind');
  end;
end;

procedure UnloadLibrary;
begin
  SDL_Quit;
  if Libhandle <> nil then dlclose(Libhandle);
  if SDLhandle <> nil then dlclose(SDLhandle);
end;

end.
