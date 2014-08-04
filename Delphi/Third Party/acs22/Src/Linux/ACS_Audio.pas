(*
  This file is a part of Audio Components Suite v 2.2 (Kylix Edition).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_Audio;

interface

uses // We don't use Libc in this unit
  Classes, SysUtils, ACS_Types, ACS_Classes, Soundcard;

const
  BUF_SIZE = $4000;

type

  TAudioOut = class(TACSOutput)
  private
    FBaseChannel : Integer;
    FVolume : Byte;
    _audio_fd : Integer;
    Buffer : array [0..BUF_SIZE-1] of Byte;
    procedure SetChannel(Ch : Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BaseChannel : Integer read FBaseChannel write SetChannel stored True;
    property Volume : Byte read FVolume write FVolume stored True;
  end;

  TAudioIn = class(TACSInput)
  private
    FBaseChannel : Integer;
    _audio_fd : Integer;
    Buisy : Boolean;
    FBPS, FChan, FFreq : Integer;
    buf : array[1..BUF_SIZE] of Byte;  // ring buffer
    BufStart, BufEnd : Integer;
    FOpened : Integer;
    FRecTime : Integer;
    FRecBytes : Integer;
    procedure OpenAudio;
    procedure CloseAudio;
    procedure SetChannel(Ch : Integer);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTotalTime : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property BaseChannel : Integer read FBaseChannel write SetChannel stored True;
    property InBitsPerSample : Integer read GetBPS write FBPS stored True;
    property InChannels : Integer read GetCh write FChan stored True;
    property InSampleRate : Integer read GetSR write FFreq stored True;
    property RecTime : Integer read FRecTime write FRecTime stored True;
  end;


var
  ChannelsCount : Integer;

implementation

const
  MAX_CHANNELS = 16; // Maximum number of audio channels/devices
  O_RDONLY = 0;
  O_WRONLY = 1;
  libname = 'libc.so.6';

var
  AudioChannels : array[0..MAX_CHANNELS-1] of String;

(* We import libc functions directly to avoid Kylix
  Libc unit limitations *)

function __write(fd : Integer; data : Pointer; size : Integer): Integer; cdecl; external libname;
function __read(Handle: Integer; var Buffer; Count: Integer): Integer; cdecl; external libname;
function ioctl(fd : Integer; command : Integer): Integer; varargs; cdecl; external libname;
function open(PathName: PChar; Flags: Integer): Integer; varargs; cdecl; external libname;
function __close(Handle: Integer): Integer; cdecl; external libname;


procedure TAudioOut.SetChannel;
begin
  if Buisy then raise EACSException.Create('Component is buisy');
  if Ch < ChannelsCount then FBaseChannel := Ch
  else raise EACSException.Create('Channel ' + IntToStr(Ch) + ' is not available');
end;

procedure TAudioOut.Prepare;
var
  parm : Integer;
begin
  // No exceptions here!
  FInput.Init;
  case FInput.BitsPerSample of
    8 : parm := AFMT_U8;
    16 : parm := AFMT_S16_LE;
  end;
  _audio_fd := open(PChar(AudioChannels[BaseChannel]), O_WRONLY);
  ioctl(_audio_fd, SNDCTL_DSP_SETFMT, @parm);
  parm := FInput.Channels;
  ioctl(_audio_fd, SNDCTL_DSP_CHANNELS, @parm);
  parm := FInput.SampleRate;
  ioctl(_audio_fd, SNDCTL_DSP_SPEED, @parm);
end;

procedure TAudioOut.Done;
begin
  __close(_audio_fd);
  _audio_fd := -1;
  FInput.Flush;
end;

function TAudioOut.DoOutput;
var
  Len, i, VCoef : Integer;
  P : Pointer;
  P1 : PBuffer8;
  P2 : PBuffer16;
begin
  // No exceptions Here
  Result := True;
  if not CanOutput then Exit;
  if Progress <> CurProgr then
  begin
    CurProgr := Progress;
    if Assigned(FOnProgress) then FonProgress(Self);
  end;
  Len := 0;
  if Abort then
  begin
    __close(_audio_fd);
    Result := False;
    Exit;
  end;
  try
    P := @Buffer[0];
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, BUF_SIZE);
    InputLock := False;
    if FVolume < 255 then
    begin
      VCoef := Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P2 := @Buffer[0];
        for i := 0 to (Len shr 1) -1 do
        P2[i] := P2[i]*VCoef;
      end else
      begin
        P1 := @Buffer[0];
        for i := 0 to Len - 1 do
        P1[i] := P1[i]*VCoef;
      end;
    end;
    __write(_audio_fd, P, Len);
  except
  end;
  if Len > 0 then Result := True
  else Result := False;
end;

constructor TAudioOut.Create;
begin
  inherited Create(AOwner);
  FVolume := 255;
  _audio_fd := -1;
end;

destructor TAudioOut.Destroy;
begin
  if _audio_fd > 0 then __close(_audio_fd);
  inherited Destroy;
end;

constructor TAudioIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
  FRecTime := 600;
end;

destructor TAudioIn.Destroy;
begin
  inherited Destroy;
  __close(_audio_fd);
end;

procedure TAudioIn.OpenAudio;
begin
  if FOpened = 0 then
  _audio_fd := open(PChar(AudioChannels[BaseChannel]), O_RDONLY);
  Inc(FOpened);
end;

procedure TAudioIn.CloseAudio;
begin
  if FOpened = 1 then __close(_audio_fd);
  if FOpened > 0 then Dec(FOpened);
end;

function TAudioIn.GetBPS;
var
  BPS : Integer;
begin
  OpenAudio;
  BPS := FBPS;
  if (BPS in [8, 16]) = False then BPS := 16;
  ioctl(_audio_fd, SNDCTL_DSP_SETFMT, @BPS);
  FBPS := BPS;
  Result := BPS;
  CloseAudio;
end;

function TAudioIn.GetCh;
var
  Ch : Integer;
begin
  OpenAudio;
  Ch := FChan;
  ioctl(_audio_fd, SNDCTL_DSP_CHANNELS, @Ch);
  FChan := Ch;
  Result := Ch;
  CloseAudio;
end;

function TAudioIn.GetSR;
var
  SR : Integer;
begin
  OpenAudio;
  SR := FFreq;
  ioctl(_audio_fd, SNDCTL_DSP_SPEED, @SR);
  FFreq := SR;
  Result := SR;
  CloseAudio;
end;

procedure TAudioIn.Init;
begin
  if Buisy then raise EACSException.Create('The component is buisy');
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  OpenAudio;
  FRecBytes := FRecTime * (GetBPS div 8) * GetCh * GetSR;
  Buisy := True;
  FSize := FRecBytes;
end;

procedure TAudioIn.Flush;
begin
  CloseAudio;
  Buisy := False;
end;

procedure TAudioIn.SetChannel;
begin
  if Ch > (ChannelsCount - 1) then
  if not (csDesigning in ComponentState) then
  raise EACSException.Create('Channel '+IntToStr(Ch)+' is not available');
  FBaseChannel := Ch;
end;

function TAudioIn.GetData;
var
  l : Integer;
begin
  if not Buisy then  raise EACSException.Create('The Stream is not opened');
  if FRecBytes >= 0 then
  if FPosition >= FRecBytes then
  begin
    Result := 0;
    Exit;
  end;
  if BufStart > BufEnd then
  begin
    BufStart := 1;
    l := __read(_audio_fd, Buf, BUF_SIZE);
    if l < 1 then
    begin
      Result := 0;
      Exit;
    end
    else BufEnd := l;
  end;
  if BufferSize < (BufEnd - BufStart + 1)
  then Result := BufferSize
  else Result := BufEnd - BufStart + 1;
  Move(Buf[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

function CountChannels : Integer;
var
  i, fd : Integer;
  fname : String;
begin
  Result := 0;
  fname := '/dev/dsp0';
  fd := open(PChar(fname), O_RDONLY);
  if fd < 0 then
  begin
    //  Under ALSA there is no /dev/dsp0 device
    fname := '/dev/dsp';
    fd := open(PChar(fname), O_RDONLY);
    if fd < 0 then Exit;
  end;
  AudioChannels[Result] := fname;
  __close(fd);
  Inc(Result);
  for i := 1 to MAX_CHANNELS - 2 do
  begin
    fname := '/dev/dsp' + IntToStr(i);
    fd := open(PChar(fname), O_RDONLY);
    if fd < 0 then Break;
    __close(fd);
    AudioChannels[Result] := fname;
    Inc(Result);
  end;
end;

function TAudioIn.GetTotalTime;
begin
  Result := FRecTime;
end;

initialization
  ChannelsCount := CountChannels;

end.
