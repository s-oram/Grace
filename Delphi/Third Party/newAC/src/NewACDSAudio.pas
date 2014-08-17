(*
  This file is a part of New Audio Components package v. 2.6
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: NewACDSAudio.pas 1251 2010-08-14 10:57:29Z andrei.borovsky $ *)

unit NewACDSAudio;

interface
uses
  SysUtils, Classes, Forms, FastMove, ACS_Types, ACS_Classes, Windows, MMSystem, DSAudio, _DirectSound, inifiles;


const
   DefaultLatency = 100;

type


  (* Class: TDSAudioOut
      Performs audio playback using the DirectSound API.
      Descends from <TAuOutput>.
      TDSAudioOut component buffers its output in order to make it more smooth. This buffering introduces some delay at the beginning of the audio playback with TDXAudioOut.
      See the <Latency> property for more detail.
      This component is more advanced than the TDxAudioOut and should replace the later.
  *)
  TDSAudioOut = class(TAuOutput)
  private
    Freed : Boolean;
    FFrameSize : Word;
    FLatency : LongWord;
    FFramesInBuffer : LongWord;
    DS : DSOut;
    Devices : DSW_Devices;
    Chan, SR, BPS : LongWord;
    EndOfInput, StartInput : Boolean;
    Buf : PBuffer8;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    _BufSize : Integer;
    FillByte : Byte;
    FUnderruns : LongWord;
    FOnUnderrun : TGenericEvent;
    FVolume : longint; //DW - for more reliable volume control
    FSpeedFactor : Single;
    FCalibrate : Boolean;
    FINIFile : String;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    function GetVolume : Integer;
    procedure SetVolume(value : Integer);
    procedure SetLatency(v : LongWord);
    procedure OnLatency(Sender : TComponent);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;
    (* Procedure: Jump
        This method, being a wrapper around <Seek>, simpifies navigation in
        the input stream. Calling Jump moves you backward or forward relative
        to the current position. Jump may be called either before starting
        playback (in this case the playback will be started from the position
        specified) or during the playback.

      Parameters:
        Offs - the amount of file contents, in in units of 1/1000 of the content length, that will be skipped.
        Positive value skips forward, negative value skips backward.
        For example calling Jump(-1000) always sets the playing position at the
        beginning of the file and Jump(100) will skip forward to 1/10 of the file.
        Note:
        Use <Seek> for more exact positioning.
    *)
    procedure Jump(Offs : Integer);
    (* Property: DeviceCount
         This read only property returns the number of logical output DirectSound devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: Underruns
         This read only property returns the number of internal buffer
         underruns that have occurred during playback. *)
    property Underruns : LongWord read FUnderruns;
    (* Property: Volume
         Use this property to set or get the volume of the sound being played.
         The default value is 0 which corresponds to the original volume of
         the sound. Valid values range from -10000 (silence) to 0. The Volume
         property allows you to make the played sound softer than the original
         one, but not louder. *)
    property Volume : Integer read GetVolume write SetVolume;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system. Valid numbers range from 0 to <DeviceCount> -
         1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: Calibrate
         If this property is set to True the <Latency> value is increased until underruns are no more reported. This way the Latency may be ajusted automatically. *)
    property Calibrate : Boolean read FCalibrate write FCalibrate;
    (* Property: INIFile
         Set this property to the ini file name where the <Latency> value should be stored. If <Calibrate> is set to True the calibrated Latency value is written to the file.  *)
    property INIFile : String read FINIFile write FINIFile;
    (* Property: Latency
         This property sets the average audio latency (the delay between the moment the audio data is passed to the component and the moment it is played.
         The latency is set in milliseconds. Lower latencies tend to produce more underruns.
         The reasonable values for this property lie in the range between 30 and 200 milliseconds. When passing audio through RDP or terminal services latency should be set to about 1000 milliseconds.
         If this property's value is changed during the playback, the internal buffer will be dynamically reset to the new value. *)
    property Latency : LongWord read FLatency write SetLatency;
    property SpeedFactor : Single read FSpeedFactor write FSpeedFactor;
    (* Property: OnUnderrun
         OnUnderrun event is raised when the component has run out of data.
         This can happen if the component receives data at slow rate from a
         slow CD-ROM unit or a network link. You may also get OnUnderrun
         event when unpausing paused playback (this is a normal situation).
         Usually TDXAudioOut successfully recovers from underruns by itself,
         but this causes pauses in playback so if you start to receive
         OnUnderrun events, you may try to increase the speed rate of data
         passing to the component, if you can. Yo can check the <Underruns>
         property for the total number of underruns. *)
    property OnUnderrun : TGenericEvent read FOnUnderrun write FOnUnderrun;
  end;


implementation

function _Min(x1, x2 : Integer) : Integer;
begin
  if x1 < x2 then
    Result := x1
  else
    Result := x2;
end;


procedure TDSAudioOut.SetDeviceNumber(i : Integer);
begin
  FDeviceNumber := i
end;

function TDSAudioOut.GetDeviceName(Number : Integer) : String;
begin
  if (Number < FDeviceCount) then Result := PChar(@(Devices.dinfo[Number].Name[0]))
  else Result := '';
end;

procedure TDSAudioOut.SetVolume;
begin
  FVolume := Value; //DW
  DSSetVolume(DS, value);
end;

function TDSAudioOut.GetVolume;
begin
  DSGetVolume(DS, Result);
  FVolume := Result; //DW
end;

procedure TDSAudioOut.Done;
begin
  if not Freed then
  begin
    DSTerminateOutput(DS);
    FreeMem(Buf);
    Freed := True;
  end;
  Finput.Flush;
  Freed := True;
end;

function TDSAudioOut.DoOutput;
var
  Len : Integer;
  lb : Integer;
  IncreaseLatency : Boolean;
//  Res : HRESULT;
  PlayTime, CTime : LongWord;
begin
  Result := True;
  IncreaseLatency := False;
  if not Busy then Exit;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if Abort then
  begin
    DSStopOutput(DS);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  if StartInput then
  begin
    DSQueryOutputSpace(DS, Len);
    if Len  = 0 then
        Len := _BufSize div 2;
    Len := FInput.FillBufferUnprotected(Buf, Len, EndOfInput);
    DSWriteBlock(DS, @Buf[0], Len);
    Volume := FVolume; //DW
    DSStartOutput(DS);
    StartInput := False;
  end;
  if EndOfInput then
  begin
    CanOutput := False;
    PlayTime := Round(_BufSize/(Chan*(BPS div 8)*SR))*1000;
    CTime := 0;
    while CTime < PlayTime do
    begin
      Sleep(50);
      DSFillEmptySpace(DS, FillByte);
      Inc(CTime, 50);
    end;
    DSStopOutput(DS);
    Result := False;
    Exit;
  end;

  Len := _BufSize div 2;
  Len := FInput.FillBufferUnprotected(Buf, Len, EndOfInput);
  if WaitForCursor(DS, 0) then
  begin
    Inc(FUnderruns);
    ResetEvent(DS.events[1]);
    IncreaseLatency := True;
    if Assigned(FOnUnderrun) then
      EventHandler.PostGenericEvent(Self, FOnUnderrun);
  end;
  if Abort then
  begin
    DSStopOutput(DS);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  DSQueryOutputSpace(DS, lb);
  if DS.Underflows > 0 then
  begin
    Inc(FUnderruns);
    IncreaseLatency := True;
    if Assigned(FOnUnderrun) then
      EventHandler.PostGenericEvent(Self, FOnUnderrun);
  end;
  if lb <> 0 then
  begin
    DSWriteBlock(DS, @Buf[0], Len);
    if EndOfInput then
    begin
      DSFillEmptySpace(DS, FillByte);
      Exit;
    end;
    Len := _BufSize div 2;
    Len := FInput.FillBufferUnprotected(Buf, Len, EndOfInput);
    if WaitForCursor(DS, 1) then
    begin
      Inc(FUnderruns);
      ResetEvent(DS.events[0]);
      IncreaseLatency := True;
      if Assigned(FOnUnderrun) then
        EventHandler.PostGenericEvent(Self, FOnUnderrun);
    end;
    if Abort then
    begin
      DSStopOutput(DS);
      CanOutput := False;
      Result := False;
      Exit;
    end;
    DSQueryOutputSpace(DS, lb);
    if DS.Underflows > 0 then
    begin
      Inc(FUnderruns);
      IncreaseLatency := True;
      if Assigned(FOnUnderrun) then
        EventHandler.PostGenericEvent(Self, FOnUnderrun);
    end;
    if lb = 0 then Exit;
    DSWriteBlock(DS, @Buf[0], Len);
    if EndOfInput then
    begin
      DSFillEmptySpace(DS, FillByte);
      Exit;
    end;
  end;
  if IncreaseLatency and FCalibrate then
      EventHandler.PostNonGuiEvent(Self, OnLatency);
end;

constructor TDSAudioOut.Create;
var
  Ini : TIniFile;
begin
  inherited Create(AOwner);
  FSpeedFactor := 1;
  if FINIFile <> '' then
  begin
    Ini := TIniFile.Create(FINIFile);
    try
      FLatency := Ini.ReadInteger('AudioOutput', 'Latency', DefaultLatency);
    except
       FLatency :=  DefaultLatency;
    end;
    Ini.Free;
  end else
    FLatency :=  DefaultLatency;;
  FVolume := 0; //DW
  if not (csDesigning in ComponentState) then
  begin
    DSEnumerateOutputDevices(@Devices);
    FDeviceCount := Devices.devcount;
    Thread.Priority := tpHighest;
  end;
  Freed := True;
end;

destructor TDSAudioOut.Destroy;
var
  Ini : TIniFile;
begin
  try
    if (FINIFile <> '') and FCalibrate then
    begin
      Ini := TIniFile.Create(FINIFile);
      try
      Ini.WriteInteger('AudioOutput', 'Latency', FLatency);
      finally
        Ini.Free;
      end;
    end;
  except
  end;
  //DW
  inherited Destroy;
end;

procedure TDSAudioOut.Prepare;
var
  Res : HResult;
  Wnd : HWND;
  Form : TForm;
  FormatExt : TWaveFormatExtensible;
begin
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
  FInput.Init;
  Chan := FInput.Channels;
  SR := FInput.SampleRate;
  if FSpeedFactor <> 1 then
    SR := Round(SR*FSpeedFactor);
  BPS := FInput.BitsPerSample;
  if FLatency < 10 then Flatency := 10;
  FFramesInBuffer := FLatency*SR div 2000;
  FFramesInBuffer := FFramesInBuffer*2;
  Res := DSInitOutputDevice(DS, @(Devices.dinfo[FDeviceNumber].guid));
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound device');
  if Owner is TForm then
  begin
    Form := Owner as TForm;
    Wnd := Form.Handle;
  end else Wnd := 0;
  {$WARNINGS OFF}
  _BufSize := Integer(FFramesInBuffer*(BPS shr 3)*Chan);
  {$WARNINGS ON}
  GetMem(Buf, _BufSize);
  if BPS <> 8 then
    FillByte := 0
  else
    FillByte := 128;
//    Res := DSW_InitOutputBuffer(DSW, Wnd, BPS, SR, Chan, _BufSize);
  FillChar(FormatExt, SizeOf(FormatExt), 0);
  if (Chan < 3) then
  begin
    FormatExt.Format.wFormatTag := 1; //WAVE_FORMAT_PCM;
    FormatExt.Format.cbSize := 0;

    FormatExt.Format.nChannels := Chan;
    FormatExt.Format.nSamplesPerSec := SR;
    FormatExt.Format.wBitsPerSample := BPS;
    FormatExt.Format.nBlockAlign := Chan*BPS shr 3;
    FormatExt.Format.nAvgBytesPerSec :=  SR*FormatExt.Format.nBlockAlign;

//    Res := DSInitOutputBuffer(DS, Wnd, BPS, SR, Chan, _BufSize);
  end else
  begin
    FormatExt.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
    FormatExt.Format.cbSize := SizeOf(FormatExt) - SizeOf(FormatExt.Format);
    FormatExt.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
    if Chan = 2 then
       FormatExt.dwChannelMask := $3;
    if Chan = 6 then
      FormatExt.dwChannelMask := $3F;
    if Chan = 8 then
      FormatExt.dwChannelMask := $FF;
    FormatExt.Format.nChannels := Chan;
    FormatExt.Format.nSamplesPerSec := SR;
    FormatExt.Format.wBitsPerSample := BPS;
    FormatExt.Format.nBlockAlign := Chan*BPS shr 3;
    FormatExt.Format.nAvgBytesPerSec :=  SR*FormatExt.Format.nBlockAlign;
  end;
  Res := DSInitOutputBufferEx(DS, Wnd, FormatExt, _BufSize);
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound buffer' + IntToHex(Res, 8));
  StartInput := True;
  EndOfInput := False;
  FFrameSize := (BPS shr 3)*Chan;
  FUnderruns := 0;
  Freed := False;
end;

procedure TDSAudioOut.Pause;
begin
  inherited Pause;
  if EndOfInput then Exit;
  DSStopOutput(DS);
end;

procedure TDSAudioOut.Resume;
begin
  if EndOfInput then Exit;
  StartInput := True;
  inherited Resume;
  DSRestartOutput(DS);
end;

procedure TDSAudioOut.Jump(Offs : Integer);
begin
  if not Assigned(Finput)
    then Exit;
  if Status <> tosPlaying then Exit;
  Pause;
  //DSFillEmptySpace(DS, FillByte);
  DSFlush(DS, FillByte);
  if Assigned(Finput) then
    FInput._Jump(Offs);
  Resume;
  StartInput := True;
end;

procedure TDSAudioOut.SetLatency(v: Cardinal);
var
  Res : HResult;
  Wnd : HWND;
  Form : TForm;
  FormatExt : TWaveFormatExtensible;
begin
  if (csDesigning in ComponentState) or Freed then
  begin
    FLatency := v;
  end else
  begin
    Pause;
    FLatency := v;
    FreeMem(Buf);
    DS.DirectSoundBuffer := nil;
    FFramesInBuffer := FLatency*SR div 2000;
    FFramesInBuffer := FFramesInBuffer*2;
    {$WARNINGS OFF}
    _BufSize := Integer(FFramesInBuffer*(BPS shr 3)*Chan);
    {$WARNINGS ON}
    GetMem(Buf, _BufSize);
    if (Chan < 3) then
    begin
      FormatExt.Format.wFormatTag := 1; //WAVE_FORMAT_PCM;
      FormatExt.Format.cbSize := 0;
      FormatExt.Format.nChannels := Chan;
      FormatExt.Format.nSamplesPerSec := SR;
      FormatExt.Format.wBitsPerSample := BPS;
      FormatExt.Format.nBlockAlign := Chan*BPS shr 3;
      FormatExt.Format.nAvgBytesPerSec :=  SR*FormatExt.Format.nBlockAlign;
    end else
    begin
      FormatExt.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
      FormatExt.Format.cbSize := SizeOf(FormatExt) - SizeOf(FormatExt.Format);
      FormatExt.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
      if Chan = 2 then
         FormatExt.dwChannelMask := $3;
      if Chan = 6 then
        FormatExt.dwChannelMask := $3F;
      if Chan = 8 then
        FormatExt.dwChannelMask := $FF;
      FormatExt.Format.nChannels := Chan;
      FormatExt.Format.nSamplesPerSec := SR;
      FormatExt.Format.wBitsPerSample := BPS;
      FormatExt.Format.nBlockAlign := Chan*BPS shr 3;
      FormatExt.Format.nAvgBytesPerSec :=  SR*FormatExt.Format.nBlockAlign;
    end;
    if Owner is TForm then
    begin
      Form := Owner as TForm;
      Wnd := Form.Handle;
    end else Wnd := 0;
    Res := DSInitOutputBufferEx(DS, Wnd, FormatExt, _BufSize);
    if Res <> 0 then raise EAuException.Create('Failed to create DirectSound buffer' + IntToHex(Res, 8));
    StartInput := True;
    EndOfInput := False;
    FFrameSize := (BPS shr 3)*Chan;
    Resume;
  end;
end;

procedure TDSAudioOut.OnLatency(Sender: TComponent);
begin
  SetLatency(FLatency + 10);
end;

end.
