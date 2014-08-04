(*
  This file is a part of Audio Components Suite v 2.2 (Kylix Edition).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_ALSA;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, libc, alsa;

const
  BUF_SIZE = $4000;
  ALSAStateIdle = $ffffffff;  // additional DriverState value;


type

  EALSABufferUnderrun = class(EACSException);
  EALSABufferOverrun = class(EACSException);

  TALSAAudioIn = class(TACSInput)
  private
    FDevice : String;
    FPeriodSize, FPeriodNum : Integer;
    _audio_handle : Psnd_pcm_t;
    _hw_params : Psnd_pcm_hw_params_t;
    Buisy : Boolean;
    FBPS, FChan, FFreq : Integer;
    buf : array[1..BUF_SIZE] of Byte;  // ring buffer
    FBufferSize : Integer;
    BufStart, BufEnd : Integer;
    FOpened : Integer;
    FRecTime : Integer;
    FRecBytes : Integer;
    FLatency : Double;
    FSilentOnOverrun : Boolean;
    procedure OpenAudio;
    procedure CloseAudio;
    function GetDriverState : Integer;
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
    property DriverState : Integer read GetDriverState;
    property Latency : Double read FLatency;
  published
    property BufferSize : Integer read FBufferSize write FBufferSize;
    property Device : String read FDevice write FDevice stored True;
    property InBitsPerSample : Integer read GetBPS write FBPS stored True;
    property InChannels : Integer read GetCh write FChan stored True;
    property InSampleRate : Integer read GetSR write FFreq stored True;
    property PeriodSize : Integer read FPeriodSize write FPeriodSize;
    property PeriodNum : Integer read FPeriodNum write FPeriodNum;
    property RecTime : Integer read FRecTime write FRecTime stored True;
    property SilentOnOverrun : Boolean read FSilentOnOverrun write FSilentOnOverrun;
  end;


  TALSAAudioOut = class(TACSOutput)
  private
    FDevice : String;
    FPeriodSize, FPeriodNum : Integer;
    _audio_handle : Psnd_pcm_t;
    _hw_params : Psnd_pcm_hw_params_t;
    FBufferSize : Integer;
    FVolume : Byte;
    _audio_fd : Integer;
    Buffer : array [0..BUF_SIZE-1] of Byte;
    FLatency : Double;
    FSilentOnUnderrun : Boolean;
    function GetDriverState : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DriverState : Integer read GetDriverState;
    property Latency : Double read FLatency;
  published
    property BufferSize : Integer read FBufferSize write FBufferSize;
    property Device : String read FDevice write FDevice stored True;
    property PeriodSize : Integer read FPeriodSize write FPeriodSize;
    property PeriodNum : Integer read FPeriodNum write FPeriodNum;
    property SilentOnUnderrun : Boolean read FSilentOnUnderrun write FSilentOnUnderrun;
    property Volume : Byte read FVolume write FVolume stored True;
  end;

implementation

  constructor TALSAAudioIn.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning	in ComponentState) then
    if not AsoundlibLoaded then
    raise EACSException.Create('Library '+ asoundlib_path + ' could not be loaded.');
    FBPS := 8;
    FChan := 1;
    FFreq := 8000;
    FSize := -1;
    FRecTime := 600;
    FDevice := 'default';
    FBufferSize := 32768;
    FSilentOnOverrun := True;
  end;

  destructor TALSAAudioIn.Destroy;
  begin
    inherited Destroy;
    CloseAudio;
  end;

  procedure TALSAAudioIn.OpenAudio;
  var
    Res : Integer;
  begin
    if FOpened = 0 then
    begin
      Res := snd_pcm_open(_audio_handle, @FDevice[1], SND_PCM_STREAM_CAPTURE, 0);
      if Res < 0 then
        raise EACSException.Create('Could not open device "' + FDevice + '" for input');
   //   snd_pcm_reset(_audio_handle);
    end;
    Inc(FOpened);
  end;

  procedure TALSAAudioIn.CloseAudio;
  begin
    if FOpened = 1 then
    begin
      snd_pcm_drop(_audio_handle);
      snd_pcm_close(_audio_handle);
    end;
    if FOpened > 0 then Dec(FOpened);
  end;

(* Note on the following three methods.
   These methods simply return the values passed by the user.
   As the actual input process begins, ALSAAudioIn may return a bit different
   value of the samplerate that is actually set by the ALSA drivers.*)

function TALSAAudioIn.GetBPS;
begin
  Result := FBPS;
end;

function TALSAAudioIn.GetCh;
begin
  Result := FChan;
end;

function TALSAAudioIn.GetSR;
begin
  Result := FFreq;
end;

procedure TALSAAudioIn.Init;
var
  aBufSize : Integer;
begin
  if Buisy then raise EACSException.Create('The component is buisy');
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  OpenAudio;

  snd_pcm_hw_params_malloc(_hw_params);
  snd_pcm_hw_params_any(_audio_handle, _hw_params);
  snd_pcm_hw_params_set_access(_audio_handle, _hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  if FBPS = 8 then snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_U8)
  else snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S16_LE);
  Self.FFreq := snd_pcm_hw_params_set_rate_near(_audio_handle, _hw_params, FFreq, 0);
  snd_pcm_hw_params_set_channels(_audio_handle, _hw_params, FChan);
  if (FPeriodSize <> 0) and (FPeriodNum <> 0) then
  begin
    snd_pcm_hw_params_set_period_size_near(_audio_handle, _hw_params, FPeriodSize, 0);
    snd_pcm_hw_params_set_periods_near(_audio_handle, _hw_params, FPeriodNum, 0);
    aBufSize := (FPeriodSize * FPeriodNum) div (FChan * (FBPS shr 3));
  end
  else aBufSize := Self.FBufferSize div (FChan * (FBPS shr 3));
  snd_pcm_hw_params_set_buffer_size_near(_audio_handle, _hw_params, aBufSize);
  snd_pcm_hw_params(_audio_handle, _hw_params);
  snd_pcm_hw_params_free(_hw_params);
  if snd_pcm_prepare(_audio_handle) < 0 then
  begin
    CloseAudio;
    raise EACSException.Create('Failed to start input');
  end;
  try
  FLatency := snd_pcm_hw_params_get_period_size(_audio_handle, 0) *
              snd_pcm_hw_params_get_periods(_audio_handle, 0)/(FFreq * FChan * (FBPS shr 3));
  except
  end;
  FRecBytes := FRecTime * (GetBPS div 8) * GetCh * GetSR;
  Buisy := True;
  FSize := FRecBytes;
end;

procedure TALSAAudioIn.Flush;
begin
  snd_pcm_drain(_audio_handle);
  CloseAudio;
  Buisy := False;
end;

function TALSAAudioIn.GetData;
var
  l : Integer;
begin
  if not Buisy then  raise EACSException.Create('The Stream is not opened');
  if FPosition >= FRecBytes then
  begin
    Result := 0;
    Exit;
  end;
  if BufStart > BufEnd then
  begin
    BufStart := 1;
    l := snd_pcm_readi(_audio_handle, @Buf[1], (BUF_SIZE div FChan) div (FBPS shr 3));
    while l < 0 do
    begin
      snd_pcm_prepare(_audio_handle);
      if not FSilentOnOverrun then
         raise EALSABufferOverrun.Create('ALSA buffer overrun.');
      l := snd_pcm_readi(_audio_handle, @Buf[1], (BUF_SIZE div FChan) div (FBPS shr 3));
    end;
    if l <> (BUF_SIZE div FChan) div (FBPS shr 3) then
    begin
      Result := 0;
      Exit;
    end
    else BufEnd := l*FChan*(FBPS shr 3);
  end;
  if BufferSize < (BufEnd - BufStart + 1)
  then Result := BufferSize
  else Result := BufEnd - BufStart + 1;
  Move(Buf[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

function TALSAAudioIn.GetTotalTime;
begin
  Result := FRecTime;
end;

function TALSAAudioIn.GetDriverState;
begin
  if FOpened = 0 then Result := ALSAStateIdle
  else Result := snd_pcm_state(_audio_handle);
end;

constructor TALSAAudioOut.Create;
begin
  inherited Create(AOwner);
  if not (csDesigning	in ComponentState) then
  if not AsoundlibLoaded then
  raise EACSException.Create('Library '+ asoundlib_path + ' could not be loaded.');
  FVolume := 255;
  FDevice := 'default';
  FBufferSize := 32768;
  FSilentOnUnderrun := True;
end;

destructor TALSAAudioOut.Destroy;
begin
  if _audio_handle <> nil then snd_pcm_close(_audio_handle);
  inherited Destroy;
end;

procedure TALSAAudioOut.Prepare;
var
  Res, aBufSize : Integer;
begin
  FInput.Init;
  Res := snd_pcm_open(_audio_handle, @FDevice[1], SND_PCM_STREAM_PLAYBACK, 0);
  if Res < 0 then
     raise EACSException.Create('Could not open device "' + FDevice + '" for output');
  //snd_pcm_reset(_audio_handle);
  snd_pcm_hw_params_malloc(_hw_params);
  snd_pcm_hw_params_any(_audio_handle, _hw_params);
  snd_pcm_hw_params_set_access(_audio_handle, _hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  if FInput.BitsPerSample = 8 then snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_U8)
  else snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S16_LE);
  snd_pcm_hw_params_set_rate_near(_audio_handle, _hw_params, FInput.SampleRate, 0);
  snd_pcm_hw_params_set_channels(_audio_handle, _hw_params, FInput.Channels);
  if (FPeriodSize <> 0) and (FPeriodNum <> 0) then
  begin
    snd_pcm_hw_params_set_period_size_near(_audio_handle, _hw_params, FPeriodSize, 0);
    snd_pcm_hw_params_set_periods_near(_audio_handle, _hw_params, FPeriodNum, 0);
    aBufSize := (FPeriodSize * FPeriodNum) div (Finput.Channels * (Finput.BitsPerSample shr 3));
  end
  else aBufSize := Self.FBufferSize div (Finput.Channels * (Finput.BitsPerSample shr 3));
  snd_pcm_hw_params_set_buffer_size_near(_audio_handle, _hw_params, aBufSize);
  snd_pcm_hw_params(_audio_handle, _hw_params);
  snd_pcm_hw_params_free(_hw_params);
  if snd_pcm_prepare(_audio_handle) < 0 then
  begin
    raise EACSException.Create('Failed to start output');
  end;
  try
  FLatency := snd_pcm_hw_params_get_period_size(_audio_handle, 0) *
              snd_pcm_hw_params_get_periods(_audio_handle, 0)/(Finput.Channels * (Finput.BitsPerSample shr 3));
  except
  end;
end;

procedure TALSAAudioOut.Done;
begin
  snd_pcm_drain(_audio_handle);
  snd_pcm_close(_audio_handle);
  _audio_handle := 0;
  FInput.Flush;
end;

function TALSAAudioOut.DoOutput;
var
  Len, i, VCoef, l : Integer;
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
    snd_pcm_drain(_audio_handle);
    snd_pcm_close(_audio_handle);
    _audio_handle := 0;
    Result := False;
    Exit;
  end;
  try
    P := @Buffer[0];
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, BUF_SIZE);
    InputLock := False;
    if Len = 0 then
    begin
      Result := False;
      Exit;
    end;
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
    l := snd_pcm_writei(_audio_handle, P, (Len div Finput.Channels) div (FInput.BitsPerSample shr 3));
    while l < 0 do
    begin
      snd_pcm_prepare(_audio_handle);
      if not FSilentOnUnderrun then
         raise EALSABufferUnderrun.Create('ALSA buffer underrun.');
      l := snd_pcm_writei(_audio_handle, P, (Len div Finput.Channels) div (FInput.BitsPerSample shr 3));
    end;
    if l = (Len div Finput.Channels) div (FInput.BitsPerSample shr 3)
    then Result := True
    else Result := False;
  except
  end;
end;

function TALSAAudioOut.GetDriverState;
begin
  if not Buisy then Result := ALSAStateIdle
  else Result := snd_pcm_state(_audio_handle);
end;


end.
