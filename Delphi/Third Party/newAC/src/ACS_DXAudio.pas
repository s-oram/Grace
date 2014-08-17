(*
  This file is a part of New Audio Components package v. 2.6
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_DXAudio.pas 1232 2010-07-19 06:37:52Z andrei.borovsky $ *)

unit ACS_DXAudio;

(* Title: ACS_DXAudio
    Components which deal with audio hardware I/O using the DirectX API. *)

interface

uses
  SysUtils, Classes, Forms, FastMove, ACS_Types, ACS_Classes, Windows, MMSystem, DSWrapper, _DirectSound;

{$DEFINE USE_EXTENDED_SPEC_FOR_24_BPS }

type

  TUnderrunEvent = procedure(Sender : TComponent) of object;
  TOverrunEvent = procedure(Sender : TComponent) of object;

  (* Class: TDXAudioOut
      Performs audio playback using the DirectX API.
      Descends from <TAuOutput>.
      TDXAudioOut component buffers its output in order to make it more smooth. This buffering introduces some delay at the beginning of the audio playback with TDXAudioOut.
      You can decrease the delay by decreasing the size of the TDXAudioOut buffer. The size of this buffer is set up by the DS_BUFFER_SIZE constant in the ACS_DxAudio.pas file.
      If you decrease the buffer size you may also want to decrease the DS_POLLING_INTERVAL value which determines how often the component requests data from its input. *)

  TDXAudioOut = class(TAuOutput)
  private
    Freed : Boolean;
    FLatency : LongWord;
    FFramesInBuffer : LongWord;
    FPollingInterval : LongWord;
    DSW : DSoundWrapper;
    Devices : DSW_Devices;
    Chan, SR, BPS : LongWord;
    EndOfInput, StartInput : Boolean;
    Buf : PBuffer8;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    _BufSize : Integer;
    FillByte : Byte;
    FUnderruns, _TmpUnderruns : LongWord;
    FOnUnderrun : TUnderrunEvent;
    FVolume : longint; //DW - for more reliable volume control
    FPrefetchData : Boolean;
    FSpeedFactor : Single;
    procedure Usleep(Interval : Word; Prefetch : Boolean);
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    function GetVolume : Integer;
    procedure SetVolume(value : Integer);
    procedure SetFramesInBuffer(value : LongWord);
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
    (* Property: Latency
         This property sets the audio latency (the delay between the moment the audio data is passed to the component and the moment it is played.
         The latency is set in milliseconds.
         This is a convenience property that overrides the <FramesInBuffer> and the <PollingInterval>. If the Latency is greater than zero these properties are ignored.
         The reasonable values for this property lie in the range between 50 (0.05 second) and 250 (0.25 second). *)
    property Latency : LongWord read FLatency write FLatency;
    (* Property: PrefetchData
       This property tells the component whenever the audio data should be prefetched while playing. Prefetching data makes it run more smoothly and allows lower buffre sizes (see <FramesInBuffer>). *)
    property PrefetchData : Boolean read FPrefetchData write FPrefetchData;
     (* Property: PollingInterval
         This property sets the audio output device polling interval in milliseconds. The smaller <FramesInBuffer> value is the smaller this polling interval should be.
         The condition for appropriate values for the polling interval is: PollingInterval < (FramesInBuffer/SampleRate)*1000
         Otherwise many underruns will occur. *)
    property  PollingInterval : LongWord read FPollingInterval write FPollingInterval;
    (* Property: FramesInBuffer
         Use this property to set the length of the internal playback buffer.
         The duration of the buffer depends on this value and the sample rate. For example
         if FramesInBuffer's value is 12000 and the sample rate is 44100, the buffer duration is
         12000/44100=0.272 sec.
         Smaller values result in lower latency and (possibly) more underruns. See also <PollingInterval>. *)
    property FramesInBuffer : LongWord read FFramesInBuffer write SetFramesInBuffer;
    (* Property: OnUnderrun
         OnUnderrun event is raised when the component has run out of data.
         This can happen if the component receives data at slow rate from a
         slow CD-ROM unit or a network link. You will also get OnUnderrun
         event when unpausing paused playback (this is a normal situation).
         Usually TDXAudioOut successfully recovers from underruns by itself,
         but this causes pauses in playback so if you start to receive
         OnUnderrun events, you may try to increase the speed rate of data
         passing to the component, if you can. Yo can check the <Underruns>
         property for the total number of underruns. *)
    property OnUnderrun : TUnderrunEvent read FOnUnderrun write FOnUnderrun;
    property SpeedFactor : Single read FSpeedFactor write FSpeedFactor;
  end;

  (* Class: TDXAudioIn
      Performs audio recording from a sound card using the DirectX API.
      Descends from <TAuInput>. *)

  TDXAudioIn = class(TAuInput)
  private
    DSW : DSoundWrapper;
    FLatency : LongWord;
    Devices : DSW_Devices;
    FFramesInBuffer : LongWord;
    FPollingInterval : LongWord;
    _BufSize : Integer;
    Buf : PBuffer8;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FBPS, FChan, FFreq : LongWord;
    FOpened : Integer;
    FSamplesToRead : Int64;
    FRecTime : Integer;
    FOverruns : LongWord;
    FOnOverrun : TOverrunEvent;
    FEchoRecording, RecordingEchoed : Boolean;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure OpenAudio;
    procedure CloseAudio;
    procedure SetRecTime(aRecTime : Integer);
    procedure SetFramesInBuffer(value : LongWord);
  protected
    function GetTotalTime : LongWord; override;
    function GetTotalSamples : Int64; override;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure _Pause; override;
    procedure _Resume; override;
    (* Property: DeviceCount
         This read only property returns the number of logical DirectSound
         input devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName[Number : Integer]
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: Overruns
         This read only property returns the number of internal buffer
         overruns that have occurred during recording. *)
    property Overruns : LongWord read FOverruns;
  published
    (* Property: Latency
         This property sets the audio latency (the delay between the moment the audio data comes into the system and the moment it exits the component.
         The latency is set in milliseconds.
         This is a convenience property that overrides the <FramesInBuffer> and the <PollingInterval>. If the Latency is greater than zero these properties are ignored.
         The reasonable values for this property lie in the range between 50 (0.05 second) and 250 (0.25 second). *)
    property Latency : LongWord read FLatency write FLatency;
    (* Property: SamplesToRead
         Use this property to set the number of samples (frames) the component
         should record. If you set this property value to -1 the component
         will be endlessly recording until you stop it. *)
    property SamplesToRead : Int64 read FSamplesToRead write FSamplesToRead;
    (* Property: DeviceNumber
         Use this property to select the recording device by number. The
         property default value is 0 which corresponds to the default audio
         input device in your system. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: InBitsPerSample
        Use this property to set the number of bits per sample in the audio
        stream the component will provide. Possible values are 8, 16, and 24
        (the last one depends on the capabilities of your hardware). *)
    property InBitsPerSample : LongWord read GetBPS write FBPS stored True;
    (* Property: InChannels
        Use this property to set the number of channels in the audio stream
        the component will provide. Possible values are 1 (mono), and 2
        (stereo). *)
    property InChannels : LongWord read GetCh write FChan stored True;
    (* Property: InSampleRate
        Use this property to set the sample rate of the audio stream the
        component will provide. Possible values range from 4000 to 128000
        (depends on the capabilities of your hardware). *)
    property InSampleRate : LongWord read GetSR write FFreq stored True;
    (* Property: RecTime
         Use this property to set the recording duration (in seconds). If set,
         this property overrides the value of <BytesToRead>. If you set this
         property value to -1 (the default) the component will be endlessly
         recording until you stop it. *)
    property RecTime : Integer read FRecTime write SetRecTime;
    (* Property: EchoRecording
         When this property is set to True, the component plays back audio data what is being recorded.
         Currently this option works only if you choose the primary recording driver as the input device.
         If you want to echo recording you should set this property to True before you start recording.
         Later you can set it to False to turn echoing off and then back to True to turn it on. *)
    property EchoRecording : Boolean read FEchoRecording write FEchoRecording;
    (* Property: FramesInBuffer
         Use this property to set the length of the internal recording buffer.
         The duration of the buffer depends on this value and the sample rate. For example
         if FramesInBuffer's value is 12000 and the sample rate is 44100, the buffer duration is
         12000/44100=0.272 sec.
         Smaller values result in lower latency and (possibly) more overruns. See also <PollingInterval>. *)
    property FramesInBuffer : LongWord read FFramesInBuffer write SetFramesInBuffer;
    (* Property: PollingInterval
         This property sets the audio input device polling interval in milliseconds. The less <FramesInBuffer> value is the less this polling interval should be.
         Otherwise many overruns will occur. *)
    property  PollingInterval : LongWord read FPollingInterval write FPollingInterval;
    (* Property: OnOverrun
         OnOverrun event is raised when this component provides data faster
         than the rest of audio-processing chain can consume. It indicates
         that some data is lost. You may also get OnOverrun event when
         unpausing paused recording (this is a normal situation). To get the
         total number of overruns read the <Overruns> property. *)
    property OnOverrun : TOverrunEvent read FOnOverrun write FOnOverrun;
  end;

implementation

function _Min(x1, x2 : Integer) : Integer;
begin
  if x1 < x2 then
    Result := x1
  else
    Result := x2;
end;

procedure TDXAudioOut.Prepare;
var
  Res : HResult;
  Wnd : HWND;
  Form : TForm;
  FormatExt : TWaveFormatExtensible;
begin
  Freed := False;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
  FInput.Init;
  Chan := FInput.Channels;
  SR := FInput.SampleRate;
  if FSpeedFactor <> 1 then
    SR := Round(SR*FSpeedFactor);
  BPS := FInput.BitsPerSample;
  if FLatency > 0 then
  begin
    if FLatency < 10 then Flatency := 10;
    FFramesInBuffer := FLatency*SR div 1000;
    FPollingInterval := FLatency div 3;
  end;
  DSW_Init(DSW);
  Res := DSW_InitOutputDevice(DSW, @(Devices.dinfo[FDeviceNumber].guid));
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

{$IFDEF USE_EXTENDED_SPEC_FOR_24_BPS }
    if (Chan < 3) and (BPS <> 24) then
{$ENDIF}

{$IFNDEF USE_EXTENDED_SPEC_FOR_24_BPS }
     if Chan < 3 then
{$ENDIF}

    begin
      FormatExt.Format.wFormatTag := 1; //WAVE_FORMAT_PCM;
      FormatExt.Format.cbSize := 0;
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
    end;
    FormatExt.Format.nChannels := Chan;
    FormatExt.Format.nSamplesPerSec := SR;
    FormatExt.Format.wBitsPerSample := BPS;
    FormatExt.Format.nBlockAlign := Chan*BPS shr 3;
    FormatExt.Format.nAvgBytesPerSec :=  SR*FormatExt.Format.nBlockAlign;
 //   FormatExt.wValidBitsPerSample := BPS;
//    FormatExt.wSamplesPerBlock := 0;
//   FormatExt.wReserved := 0;
//    FormatExt.SubFormat := 1;

    Res := DSW_InitOutputBufferEx(DSW, Wnd, FormatExt, _BufSize);
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound buffer' + IntToHex(Res, 8));
  StartInput := True;
  EndOfInput := False;
  _TmpUnderruns := 0;
end;

procedure TDXAudioOut.Done;
begin
  if not Freed then
  begin
    DSW_Term(DSW);
    FreeMem(Buf);
  end;
  Finput.Flush;
  Freed := True;
end;

function TDXAudioOut.DoOutput;
var
  Len, lb, counter : LongWord;
//  Res : HRESULT;
  PlayTime, CTime : LongWord;
  TmpBuf : Pointer;
begin
  Result := True;
  if not Busy then Exit;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if StartInput then
  begin
    Len := FInput.FillBuffer(Buf, _BufSize, EndOfInput);
    DSW_WriteBlock(DSW, @Buf[0], Len);
    Volume := FVolume; //DW
    DSW_StartOutput(DSW);
    StartInput := False;
  end;
  if Abort then
  begin
    DSW_StopOutput(DSW);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  if EndOfInput then
  begin
    CanOutput := False;
    PlayTime := Round(_BufSize/(Chan*(BPS div 8)*SR))*1000;
    CTime := 0;
    while CTime < PlayTime do
    begin
      Sleep(100);
      DSW_FillEmptySpace(DSW, FillByte);
      Inc(CTime, 100);
    end;
    DSW_StopOutput(DSW);
    Result := False;
    Exit;
  end;
  counter := 0;
  repeat
    Usleep(FPollingInterval, FPrefetchData);
    DSW_QueryOutputSpace(DSW, lb);
    lb := lb - (lb mod DSW.dsw_BytesPerFrame);
    Inc(counter);
    if counter > 16 then
    begin
      FExceptionMessage := 'Audio output error';
      if Assigned(FOnThreadException) then
        EventHandler.PostGenericEvent(Self, FOnThreadException);
      Result := False;
      Exit;
    end;
  until lb <> 0;
  if FPrefetchData then
  begin
    Len := _Min(lb, _BufSize);
    FInput.GetData(TmpBuf, Len);
  end else
  begin
    Len := FInput.FillBuffer(Buf, _Min(lb, _BufSize), EndOfInput);
    TmpBuf := Buf;
  end;
  EndOfInput := Len = 0;
  DSW_WriteBlock(DSW, TmpBuf, Len);
  if EndOfInput then
    DSW_FillEmptySpace(DSW, FillByte);
  if _TmpUnderruns <> DSW.dsw_OutputUnderflows then
  begin
    FUnderruns := DSW.dsw_OutputUnderflows;
    _TmpUnderruns := DSW.dsw_OutputUnderflows;
    DSW_StopOutput(DSW);
    DSW_FillEmptySpace(DSW, FillByte);
    if Assigned(FOnUnderrun) then
      EventHandler.PostGenericEvent(Self, FOnUnderrun);
    //Usleep(FPollingInterval, FPrefetchData);
    DSW_RestartOutput(DSW); //StartInput := True;
  end;
end;

constructor TDXAudioOut.Create;
begin
  inherited Create(AOwner);
  FSpeedFactor := 1;
  FFramesInBuffer := $6000;
  FPollingInterval := 100;
  FLatency := 100;
  FVolume := 0; //DW
  if not (csDesigning in ComponentState) then
  begin
    DSW_EnumerateOutputDevices(@Devices);
    FDeviceCount := Devices.devcount;
    Thread.Priority := tpHighest;
  end;
  FPrefetchData := True;
end;

destructor TDXAudioOut.Destroy;
begin
  inherited Destroy;
end;

procedure TDXAudioOut.Pause;
begin
  inherited Pause;
  if EndOfInput then Exit;
  DSW_StopOutput(DSW);
end;

procedure TDXAudioOut.Resume;
begin
  if EndOfInput then Exit;
  DSW_RestartOutput(DSW);
  inherited Resume;
end;

procedure TDXAudioOut.SetDeviceNumber(i : Integer);
begin
  FDeviceNumber := i
end;

procedure TDXAudioOut.Usleep(Interval : Word; Prefetch : Boolean);
var
  Start, Elapsed {, DataSize, SampleSize} : LongWord;
begin
  Start := timeGetTime;
  if Prefetch then
  begin
//    SampleSize := Chan*(BPS shr 3);
//    DataSize := ((Interval * Self.SR) div 1000)*SampleSize;
  //  DataSize := (DataSize div 4)*3;
//    DataSize := DataSize + (DataSize shr 2);
//    DataSize := DataSize - (DataSize mod SampleSize);
  end;
  Elapsed := timeGetTime - Start;
  if Elapsed >= Interval then Exit;
  Sleep(Interval - Elapsed);
end;

function TDXAudioOut.GetDeviceName(Number : Integer) : String;
begin
  if (Number < FDeviceCount) then Result := PChar(@(Devices.dinfo[Number].Name[0]))
  else Result := '';
end;

constructor TDXAudioIn.Create;
begin
  inherited Create(AOwner);
  FLatency := 100;
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
  FRecTime := -1;
  FSamplesToRead := -1;
  FFramesInBuffer := $6000;
  FPollingInterval := 100;
  DSW_EnumerateInputDevices(@Devices);
  FDeviceCount := Devices.devcount;
end;

destructor TDXAudioIn.Destroy;
begin
  DSW_Term(DSW);
  inherited Destroy;
end;

procedure TDXAudioIn.OpenAudio;
var
  Res : HResult;
  S : String;
begin
  if FOpened = 0 then
  begin
    if FLatency > 0 then
    begin
      if FLatency < 10 then Flatency := 10;
      FFramesInBuffer := FLatency*FFreq div 1000;
      FPollingInterval := (FLatency div 2); //4)*3;
    end;
    DSW_Init(DSW);
    //if not Assigned(DSW_InitInputDevice) then raise EACSException.Create('Failed');
    Res := DSW_InitInputDevice(DSW, @(Devices.dinfo[FDeviceNumber].guid));
    if Res <> 0 then
    begin
      case res of
        DSERR_ALLOCATED : S := 'DSERR_ALLOCATED';
        DSERR_INVALIDPARAM : S := 'DSERR_INVALIDPARAM';
        DSERR_INVALIDCALL : S := 'DSERR_INVALIDCALL';
        DSERR_GENERIC : S := 'DSERR_GENERIC';
        DSERR_BADFORMAT : S := 'DSERR_BADFORMAT';
        DSERR_UNSUPPORTED : S:= 'DSERR_UNSUPPORTED';
        DSERR_NODRIVER : S := 'DSERR_NODRIVER';
        DSERR_ALREADYINITIALIZED : S := 'DSERR_ALREADYINITIALIZED';
        else S := 'Unknown';
      end;
      raise EAuException.Create('Failed to create DirectSound device: ' + S);
    end;
    _BufSize := FFramesInBuffer*(FBPS shr 3)*FChan;
    GetMem(Buf, _BufSize);
    Res := DSW_InitInputBuffer(DSW, FBPS, FFreq, FChan, _BufSize);
    if Res <> 0 then raise EAuException.Create('Failed to create DirectSound buffer');
  end;
  Inc(FOpened);
end;

procedure TDXAudioIn.CloseAudio;
begin
  if FOpened = 1 then
  begin
    DSW_Term(DSW);
    FreeMem(Buf);
  end;
  if FOpened > 0 then Dec(FOpened);
end;

function TDXAudioIn.GetBPS;
begin
  Result := FBPS;
end;

function TDXAudioIn.GetCh;
begin
  Result := FChan;
end;

function TDXAudioIn.GetSR;
begin
  Result := FFreq;
end;

procedure TDXAudioIn.InitInternal;
begin
  if Busy then raise EAuException.Create('The component is busy');
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
{$WARNINGS OFF}
  if FRecTime > 0 then FSamplesToRead := FRecTime*FFreq;
{$WARNINGS ON}
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  Busy := True;
  FSampleSize := FChan*FBPS div 8;
  if FSamplesToRead > 0 then
    FSize := FSamplesToRead*FSampleSize
  else
    FSize := -1;
  OpenAudio;
  DSW_StartInput(DSW);
  if FEchoRecording then
  begin
    if DSW_InitOutputDevice(DSW, @(Devices.dinfo[FDeviceNumber].guid)) = DS_OK then
    begin
      DSW_InitOutputBuffer(DSW, 0, FBPS, FFreq, FChan, _BufSize);
      DSW_StartOutput(DSW);
    end else
      FEchoRecording := False;
  end;
  RecordingEchoed := FEchoRecording;
end;

procedure TDXAudioIn.FlushInternal;
begin
  DSW_StopInput(DSW);
  if RecordingEchoed then
    DSW_StopOutput(DSW);
  CloseAudio;
  Busy := False;
end;

procedure TDXAudioIn.GetDataInternal;
var
  l : Integer;
  l1, res : LongWord;
begin
  if not Busy then  raise EAuException.Create('The Stream is not opened');
  if  (FSamplesToRead >=0) and (FPosition >= FSize) then
  begin
    Buffer := nil;
    Bytes := 0;
    Exit;
  end;
  if BufStart >= BufEnd then
  begin
    BufStart := 0;
    repeat
      Sleep(FPollingInterval);
      res :=  DSW_QueryInputFilled(DSW, l);
      if res <> DS_OK then
         raise EAuException.Create(Format('Input failed: DirectSound error 0x%x', [res]));
    until l <> 0;
    if l > _BufSize then
    begin
      l := _BufSize; (* We have lost some data.
                        Generally this shouldn't happen. *)
      Inc(FOverruns);
      if Assigned(FOnOverrun) then
        EventHandler.PostGenericEvent(Self, FOnOverrun);
    end;
//    l := l - (l mod 1024);
    res := DSW_ReadBlock(DSW, @Buf[0], l);
    if res <> DS_OK then
       raise EAuException.Create(Format('Input failed: DirectSound error 0x%x', [res]));
    if RecordingEchoed then
    begin
        DSW_QueryOutputSpace(DSW, l1);
        if Integer(l) < Integer(l1) then l1 := LongWord(l);
        if FEchoRecording then
          DSW_WriteBlock(DSW, @Buf[0], l1)
        else
          DSW_FillEmptySpace(DSW, 0);
    end;
    BufEnd := l;
  end;
  if Bytes > (BufEnd - BufStart) then
    Bytes := BufEnd - BufStart;
  if (FSize > 0) and (Bytes > FSize - FPosition) then
    Bytes := FSize - FPosition;
  Buffer := @Buf[BufStart];
  Inc(BufStart, Bytes);
  Inc(FPosition, Bytes);
end;

procedure TDXAudioIn.SetRecTime;
begin
  FRecTime := aRecTime;
{$WARNINGS OFF}
  if FRecTime >= 0 then FSamplesToRead := FRecTime*FFreq
  else FSamplesToRead := -1;
 {$WARNINGS ON}

end;

procedure TDXAudioIn.SetDeviceNumber(i : Integer);
begin
  FDeviceNumber := i
end;

function TDXAudioIn.GetDeviceName(Number : Integer) : String;
begin
  if (Number < FDeviceCount) then Result := PChar(@(Devices.dinfo[Number].Name[0]))
  else Result := '';
end;

function TDXAudioIn.GetTotalTime : LongWord;
var
  BytesPerSec : Integer;
begin
  BytesPerSec := FFreq*FSampleSize;
  if FSamplesToRead < 0 then Result := 0
  else
  Result := Round(FSamplesToRead/BytesPerSec);
end;

function TDXAudioIn.GetTotalSamples : Int64;
begin
  Result := FSamplesToRead;
end;


procedure TDXAudioIn._Pause;
begin
  DSW_StopInput(DSW);
  if RecordingEchoed then
    DSW_StopOutput(DSW);
end;

procedure TDXAudioIn._Resume;
begin
  DSW_StartInput(DSW);
  if RecordingEchoed then
    DSW_RestartOutput(DSW);
end;

procedure TDXAudioOut.SetVolume;
begin
  FVolume := Value; //DW
  dsw_SetVolume(DSW, value);
end;

function TDXAudioOut.GetVolume;
begin
  dsw_GetVolume(DSW, Result);
  FVolume := Result; //DW
end;

procedure TDXAudioOut.SetFramesInBuffer;
begin
  if not Busy then
    FFramesInBuffer := value;
end;

procedure TDXAudioIn.SetFramesInBuffer;
begin
  if not Busy then
    FFramesInBuffer := value;
end;

procedure TDXAudioOut.Jump(Offs : Integer);
begin
  Pause;
  DSW_FillEmptySpace(DSW, FillByte);
  if Assigned(Finput) then
  begin
    FInput._Jump(Offs);
  end;
  Resume;
end;

end.
