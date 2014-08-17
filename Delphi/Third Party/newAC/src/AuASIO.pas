(*
  This file is a part of New Audio Components package 2.5
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: AuASIO.pas 1192 2010-02-18 14:39:19Z andrei.borovsky $ *)

unit AuASIO;

(* Title: AuASIO
    Components for working with ASIO drivers *)

interface

uses
  SysUtils, Classes, Forms, SyncObjs, FastMove, ACS_Types, ACS_Procs, ACS_Classes, Windows, AsioList, OpenAsio, Asio, Math;

type

  TASIOPositionEvent = procedure(Sender : TComponent; SamplePosition, TimeStamp : Int64) of object;
  TASIOBufferSize = (absPreferred, absMinimum, absMaximum);

  (* Class: TASIOAudioOut
      Performs audio playback using low latency ASIO drivers.
      Descends from <TAuOutput>.
      On Windows ASIO drivers bypass some OS layaers which makes them suitable for a real-time audio processing.
      You will need an ASIO audo driver to use this component.
      Free ASIO driver that can be installed on top of any WDM (Windows) driver is available at http://www.asio4all.com.
      This component also requires openasio.dll which you will find along with other third-party NewAC libraries.
      One important feature of ASIO drivers is that they offer only a limited choise of sampling rates and sample formats.
      It is your software that should tune itself up to an ASIO driver and not vice versa.
      The sample formats currently supported by the NewAC ASIO components are 16/24/32 bps mono/stereo/5.1 surround.
 *)

  TASIOAudioOut = class(TAuOutput)
  private
    device : IOpenASIO;
    Devices : TAsioDriverList;
    Chan, SR, BPS : LongWord;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
//<<<<<<< .mine//    ACS : TCriticalSection;
    ACS : TCriticalSection;
    _Prefetched : Boolean;
//>>>>>>> .theirs//    FOnUnderrun : TUnderrunEvent;
    FLatency, FBufferSize : LongWord;
    FSupportedChannels : LongWord;
    FOutputChannels : LongWord;
    FOutputBPS : LongWord;
    FFloat,  FPacked32 : Boolean;
    ASIOStarted : Boolean;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    FOnSampleRateChanged : TGenericEvent;
    FOnLatencyChanged : TGenericEvent;
    FOnDriverReset : TGenericEvent;
    FNewSampleRate : Integer;
    DoReset : Boolean;
    DevStopped : Boolean;
    FOnPositionChanged : TASIOPositionEvent;
    FASIOBufferSize : TASIOBufferSize;
    L : LongWord;
    FLastBlock : Boolean;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    function GetOutputBPS : Integer;
    function GetMaxOutputChannels : Integer;
    function GetLatency : Integer;
    function GetSampleRate : Integer;
    procedure SetSampleRate(SR : Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure ProcessBuffer(sender : TComponent);
    procedure CallProcessBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: ReleaseASIODriver
        Call this method to release hold on ASIO driver so thst other applications could use audio subsystem.
        Driver is released automatically when the audio playback is done. *)
    procedure ReleaseASIODriver;
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
    (* Function: IsSampleRateSupported
        Returns True if the specified sample rate is supported by the driver and False otherwise. Call it before you start any other ASIO operation or the current operation will be aborted. *)
    function IsSampleRateSupported(SR : Integer) : Boolean;
    (* Function: ShowSetupDlg
        Brings up ASIO properties dialog window.  *)
    procedure ShowSetupDlg;
    (* Property: DeviceCount
         This read only property returns the number of logical ASIO devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: OutputBPS
         This read only property returns the number of bits per sample supported by the driver. Whatever this value is TASIOAudioOut also supports 16 bps input using internal conversion. *)
    property OutputBPS : Integer read GetOutputBPS;
    (* Property: MaxOutputChannels
         This read only property returns the maximum number of output channels provided by the driver. You can use input with less channels than this value but not more.
         ASIO properties setup may change this value (see <ShowSetupDlg>). *)
    property MaxOutputChannels : Integer read GetMaxOutputChannels;
    (* Property: Latency
         Read this property to get the latency value (in samples). *)
    property Latency : Integer read GetLatency;
    (* Property: SampleRate
         Use this property to change the ASIO driver sample rate while doing playback.  Note however that not all sample rates are supported.
         If the requested sample rate is not supported, the property's value doesn't change. *)
    property SampleRate : Integer read GetSampleRate write SetSampleRate;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system. Valid numbers range from 0 to <DeviceCount> -
         1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: ASIOBufferSize
         Use this property to select the ASIO buffer size. Available options are absMinimum - minimum allowed buffer size, absPreferred - preferred buffer size, absMaximum - maximum aloowed buffer size.
         Larger buffer sizes increase latency. *)
    property ASIOBufferSize : TASIOBufferSize read FASIOBufferSize write FASIOBufferSize;
    property OnSampleRateChanged : TGenericEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnLatencyChanged : TGenericEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnDriverReset : TGenericEvent read FOnDriverReset write FOnDriverReset;
    (* Property: OnPositionChanged
         If assigned this event handler is called every time  the system is about to fetch a new audio data block.
         The blocks are about 512 audio samlples in size so the operation is almost immediate.
         The event handler is passed the current sample number and a timestamp from the beginning of playback.
         Note that unlike most other NewAC events, this event is a real-time one. Performing some lengthy operation in this event handler may cause gaps in playback. *)
    property OnPositionChanged : TASIOPositionEvent read FOnPositionChanged write FOnPositionChanged;
  end;

  (* Class: TASIOAudioIn
      Performs audio recording from a sound card using low latency ASIO drivers.
      Descends from <TAuInput>.
      On Windows ASIO drivers bypass some OS layaers which makes them suitable for a real-time audio processing.
      You will need an ASIO audo driver to use this component.
      Free ASIO driver that can be installed on top of any WDM (Windows) driver is available at http://www.asio4all.com.
      This component also requires openasio.dll which you will find along with other third-party NewAC libraries.
      One important feature of ASIO drivers is that they offer only a limited choise of sampling rates and sample formats.
      It is your software that should tune itself up to an ASIO driver and not vice versa.
      The sample formats currently supported by the NewAC ASIO components are 16/32 bps mono/stereo.
  *)

  TASIOAudioIn = class(TAuInput)
  private
    device : IOpenASIO;
    Devices : TAsioDriverList;
    _BufSize : LongWord;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FBPS, FChan, FFreq : LongWord;
    FMaxChan : LongWord;
    FSamplesToRead : Int64;
    FRecTime : Integer;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    FLatency : LongWord;
    BytesInBuf : LongWord;
    FOnLatencyChanged : TGenericEvent;
    ASIOStarted : Boolean;
    FASIOBufferSize : TASIOBufferSize;
    FOnPositionChanged : TASIOPositionEvent;
    HoldEvent : TEvent;
    FPlayRecording : Boolean;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    procedure SetRecTime(aRecTime : Integer);
    function GetMaxOutputChannels : Integer;
    function GetLatency : Integer;
    procedure SetSampleRate(SR : LongWord);
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
    (* Function: HoldOff
        Pass True to HoldOff if you want to temporarily disable recording. The Whole processing chain is bloacked untill you call HoldOff(False). *)
    procedure HoldOff(b : Boolean);
    (* Function: IsSampleRateSupported
        Returns True if the specified sample rate is supported by the driver and False otherwise. Call it before you start any other ASIO operation or the current operation will be aborted. *)
    function IsSampleRateSupported(SR : Integer) : Boolean;
    (* Function: ShowSetupDlg
        Brings up ASIO properties dialog window.  *)
    procedure ShowSetupDlg;
    (* Property: DeviceCount
         This read only property returns the number of logical DirectSound
         input devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName[Number : Integer]
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: MaxOutputChannels
         This read only property returns the maximum number of output channels provided by the driver. You can use input with less channels than this value but not more.
         ASIO properties setup may change this value (see <ShowSetupDlg>). *)
    property MaxOutputChannels : Integer read GetMaxOutputChannels;
    (* Property: Latency
         Read this property to get the latency value in samples. *)
    property Latency : Integer read GetLatency;
    (* Property: InSampleRate
        Use this property to set the sample rate of the audio stream the
        component will provide. Note that only limited number of sample rates is supported by ASIO (see <IsSampleRateSupported>).  *)
     property InSampleRate : LongWord read GetSR write SetSampleRate;
  published
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
    (* Property: InChannels
        Use this property to set the number of channels in the audio stream
        the component will provide. Possible values are 1 (mono), and 2
        (stereo). *)
    property InChannels : LongWord read GetCh write FChan stored True;
    (* Property: RecTime
         Use this property to set the recording duration (in seconds). If set
         this property overrides the value of <BytesToRead>. If you set this
         property value to -1 (the default) the component will be endlessly
         recording until you stop it. *)
    property RecTime : Integer read FRecTime write SetRecTime;
    (* Property: ASIOBufferSize
         Use this property to select the ASIO buffer size. Available options are absMinimum - minimum allowed buffer size, absPreferred - preferred buffer size, absMaximum - maximum aloowed buffer size.
         Larger buffer sizes increase latency. *)
    property ASIOBufferSize : TASIOBufferSize read FASIOBufferSize write FASIOBufferSize;
    (* Property: EchoRecording
         If this property is set to True the system plays out the audio data being recorded. Otherwise recording goes mutely. *)
    property EchoRecording : Boolean read FPlayRecording write FPlayRecording;
    property OnLatencyChanged : TGenericEvent read FOnLatencyChanged write FOnLatencyChanged;
    (* Property: OnPositionChanged
         If assigned this event handler is called every time  the ASIO is about to record a new audio data block.
         The blocks are about 512 audio samlples in size so the operation is almost immediate.
         The event handler is passed the current sample number and a timestamp from the beginning of playback.
         Note that unlike most other NewAC events, this event is a real-time one. Performing some lengthy operation in this event handler may cause gaps in playback.
         You can call the output component's Stop method  from this handler but not pause. Use the <HoldOff> method if you want to pause recording from this event handler. *)
    property OnPositionChanged : TASIOPositionEvent read FOnPositionChanged write FOnPositionChanged;

  end;

 (* Class: TASIOAudioDuplex
      Allows simultaneous recording and playback using ASIO drivers.
      Descends from <TAuConverter>.
      THis component reads its input and plays it vis ASIO output device while recording data from ASIO input device.
      The recorded data may be mixed with the input.
      Technically this component is a converter so it sits in the middle of the audio-processing chain like this^

      -> input to be played -> [TASIOAudioDuplex instance (input is played, output is recorded)] -> output (recorded data possibly mixed with the input data) ->

      The number of bits per sample of the input should be the same as the recodrded number of bits per sample (currently either 16 or 32).
      The sample rates also should be the same.
      On Windows ASIO drivers bypass some OS layaers which makes them suitable for a real-time audio processing.
      You will need an ASIO audo driver to use this component.
      Free ASIO driver that can be installed on top of any WDM (Windows) driver is available at http://www.asio4all.com.
      This component also requires openasio.dll which you will find along with other third-party NewAC libraries.
      One important feature of ASIO drivers is that they offer only a limited choise of sampling rates and sample formats.
      It is your software that should tune itself up to an ASIO driver and not vice versa.
 *)

  TASIOAudioDuplex = class(TAuConverter)
  private
    device : IOpenASIO;
    Devices : TAsioDriverList;
    _BufSize : LongWord;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FBPS, FChan, FFreq : LongWord;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    FLatency : LongWord;
    BytesInBuf : LongWord;
    FOnLatencyChanged : TGenericEvent;
    ASIOStarted : Boolean;
    FASIOBufferSize : TASIOBufferSize;
    FOnPositionChanged : TASIOPositionEvent;
    FEchoRecording, FRecordInput : Boolean;
    HoldEvent : TEvent;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    function GetLatency : Integer;
    procedure ProcessBuffers(Sender : TComponent);
  protected
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
    (* Function: HoldOff
        Pass True to HoldOff if you want to temporarily disable recording. The Whole processing chain is bloacked untill you call HoldOff(False). *)
    procedure HoldOff(b : Boolean);
    (* Function: ShowSetupDlg
        Brings up ASIO properties dialog window.  *)
    procedure ShowSetupDlg;
    (* Property: DeviceCount
         This read only property returns the number of logical DirectSound
         input devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName[Number : Integer]
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: Latency
         Read this property to get the latency value in samples. *)
    property Latency : Integer read GetLatency;
    (* Property: SampleRate
        Read this property to determine what the input data sample rate should be. *)
     property SampleRate : LongWord read GetSR;
    (* Property: Channels
        Read this property to get the number of channels for the input data in the audio stream
        the component will provide. Possible values are 1 (mono), and 2
        (stereo). *)
    property Channels : LongWord read GetCh;
    (* Property: BitsPerSample
        Read this property to get the sample format for the input data in the audio stream
        the component will provide. Possible values are 32. *)
    property BitsPerSample : LongWord read GetBPS;

  published
    (* Property: DeviceNumber
         Use this property to select the recording device by number. The
         property default value is 0 which corresponds to the default audio
         input device in your system. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: ASIOBufferSize
         Use this property to select the ASIO buffer size. Available options are absMinimum - minimum allowed buffer size, absPreferred - preferred buffer size, absMaximum - maximum aloowed buffer size.
         Larger buffer sizes increase latency. *)
    property ASIOBufferSize : TASIOBufferSize read FASIOBufferSize write FASIOBufferSize;
    property OnLatencyChanged : TGenericEvent read FOnLatencyChanged write FOnLatencyChanged;
    (* Property: OnPositionChanged
         If assigned this event handler is called every time  the ASIO is about to record a new audio data block.
         The blocks are about 512 audio samlples in size so the operation is almost immediate.
         The event handler is passed the current sample number and a timestamp from the beginning of playback.
         Note that unlike most other NewAC events, this event is a real-time one. Performing some lengthy operation in this event handler may cause gaps in playback.
         You can call the output component's Stop method  from this handler but not pause. Use the <HoldOff> method if you want to pause recording from this event handler. *)
    property OnPositionChanged : TASIOPositionEvent read FOnPositionChanged write FOnPositionChanged;
    (* Property: EchoRecording
         If this property is set to True the system mixes input audio data with data being recorded and plays both. *)
    property EchoRecording : Boolean read FEchoRecording write FEchoRecording;
    (* Property: RecordInput
         If this property is set to True the system mixes input audio data with data being recorded and passes the mix as its output. *)
    property RecordInput : Boolean read FRecordInput write FRecordInput;
  end;


  //IMPORTANT
  //     ASIOTrue :  begin EventHandler.PostGenericEvent(OutputComponent, OutputComponent.ProcessBuffer); sleep(2); end;
 //     ASIOFalse :  OutputComponent.ProcessBuffer(OutputComponent);



implementation

const
  oBufSize = $3FFFF;
  BlockSize = $8000;
  BlockAlign = 8;

var
  OutputComponent : TASIOAudioOut;
  InputComponent : TASIOAudioIn;
  DuplexComponent  : TASIOAudioDuplex;
  GStop : Boolean = False;
  CallOutputReady : Boolean = True;
  BufferIndex : Integer;
  iBuf : array[0..$FFFF] of Byte;
  oBuf : array[0..oBufSize] of Byte;
  WriteIndex, ReadIndex, ReadOffset : LongWord;

type
  TBufferInfoArray = array[0..15] of TASIOBufferInfo;
  PBufferInfoArray = ^TBufferInfoArray;
  TBuffer64 = array[0..0] of Int64;
  PBuffer64 = ^TBuffer64;

procedure Deinterleave32(InputBuffer : PBuffer32; OutputBuffer : PBufferInfoArray; Samples, BufferIndex : LongWord; Channels : LongWord);
var
  i, j : LongWord;
  Dest : array[0..15] of PBuffer32;
begin
  for i := 0 to Channels - 1 do
    Dest[i] := OutputBuffer[i].buffers[BufferIndex];
  if Channels mod 2 = 0 then
  begin
    i := 0;
    case Channels of
      2: while i < Samples do
         begin
            j := i shl 1;
            Dest[0][i] := InputBuffer[j];
            Dest[1][i] := InputBuffer[j+1];
            Inc(i);
         end;
      4: while i < Samples do
         begin
            j := i shl 2;
            Dest[0][i] := InputBuffer[j];
            Dest[1][i] := InputBuffer[j+1];
            Dest[2][i] := InputBuffer[j+2];
            Dest[3][i] := InputBuffer[j+3];
            Inc(i);
         end;
      6: while i < Samples do
         begin
            j := i*6;
            Dest[0][i] := InputBuffer[j];
            Dest[1][i] := InputBuffer[j+1];
            Dest[2][i] := InputBuffer[j+2];
            Dest[3][i] := InputBuffer[j+3];
            Dest[4][i] := InputBuffer[j+4];
            Dest[5][i] := InputBuffer[j+5];
            Inc(i);
         end;
      8: while i < Samples do
         begin
            j := i*8;
            Dest[0][i] := InputBuffer[j];
            Dest[1][i] := InputBuffer[j+1];
            Dest[2][i] := InputBuffer[j+2];
            Dest[3][i] := InputBuffer[j+3];
            Dest[4][i] := InputBuffer[j+4];
            Dest[5][i] := InputBuffer[j+5];
            Dest[6][i] := InputBuffer[j+6];
            Dest[7][i] := InputBuffer[j+7];
            Inc(i);
         end;
    end;
        //Dest[j][i] := InputBuffer[i*Channels + j];
  end else
  begin
    for i := 0 to Samples - 1 do
      for j := 0 to Channels - 1 do
        Dest[j][i] := InputBuffer[i*Channels + j];
  end;
end;

procedure Deinterleave16(InputBuffer : PBuffer16; OutputBuffer : PBufferInfoArray; Samples, BufferIndex : LongWord; Channels : LongWord);
var
  i, j : LongWord;
  Dest : array[0..15] of PBuffer16;
begin
  for i := 0 to Channels - 1 do
    Dest[i] := OutputBuffer[i].buffers[BufferIndex];
  for i := 0 to Samples - 1 do
    for j := 0 to Channels - 1 do
      Dest[j][i] := InputBuffer[i*Channels + j];
end;


procedure TASIOAudioOut.ProcessBuffer(sender : TComponent);
var
  s1, s2 : TASIOInt64;
begin
   if Device = nil then Exit;
    ///////////////
   //FillBuffer(tmpStop);
  ACS.Enter;
  if not _Prefetched then
  begin
    _Prefetched := True;
    L := FBufferSize*(BPS shr 3)*FOutputChannels;
    L := FInput.FillBufferUnprotected(@iBuf[0], L, FLastBlock);
  end;
  if (BPS = 16) and (OutputBPS = 32) then
  begin
    Convert16To32(@iBuf[0], L);
    L := L shl 1;
  end else
    if (BPS = 24) and (OutputBPS = 32) then
    begin
      Convert24To32(@iBuf[0], L);
      L := (L shl 2) div 3;
    end else
    begin
      raise EAuException.Create(Format('TASIOAudioOut cannot play %d bps stream in this set up (actual output bps is %d). Use BPS converter.', [BPS, OutputBPS]));
    end;
    _Prefetched := False;
    ACS.Leave;

   ////////////////
 //  OldStopped := Thread.Stopped;
 //  Thread.Stopped := False;
   if Assigned(FOnPositionChanged) then
   begin
     Device.GetSamplePosition(s1, s2);
     FOnPositionChanged(Self, (s1.hi shl 32) + s1.lo, (s2.hi shl 32) + s2.lo)
   end;
   //   if FPrefetchData then
//     EventHandler.PostNonGuiEvent(Self, FPrefetch);
   if Self.FOutputBPS = 16 then
   begin
     L := (L shr 1) div FOutputChannels;
     Deinterleave16(@iBuf[0], @BufferInfo[0], L, BufferIndex, FOutputChannels);
   end;
   if Self.FOutputBPS = 32 then
   begin
     L := (L shr 2) div FOutputChannels;
     Deinterleave32(@iBuf[0], @BufferInfo[0], L, BufferIndex, FOutputChannels);
   end;
   if CallOutputReady then
      CallOutputReady := TASIOAudioOut(sender).Device.OutputReady = ASE_OK;
   GStop := FLastBlock;
 //  Thread.Stopped := OldStopped;
end;

procedure AsioBufferSwitchOutput(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
begin
  BufferIndex := doubleBufferIndex;
   case directProcess of
     ASIOFalse :  begin EventHandler.PostNonGuiEvent(OutputComponent, OutputComponent.ProcessBuffer); sleep(2); end;
     ASIOTrue :  OutputComponent.ProcessBuffer(OutputComponent);
   end;
end;

procedure AsioSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
begin
  OutputComponent.Stop();
  OutputComponent.FNewSampleRate := Round(sRate);
  if Assigned(OutputComponent.FOnSampleRateChanged) then
    EventHandler.PostGenericEvent(OutputComponent, OutputComponent.FOnSampleRateChanged);
end;

function AsioMessage(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;

  case selector of
    kAsioSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kAsioEngineVersion        :  Result := 1;
          kAsioResetRequest         :  Result := 1;
          kAsioBufferSizeChange     :  Result := 0;
          kAsioResyncRequest        :  Result := 1;
          kAsioLatenciesChanged     :  Result := 1;
          kAsioSupportsTimeInfo     :  Result := 1;
          kAsioSupportsTimeCode     :  Result := 1;
          kAsioSupportsInputMonitor :  Result := 0;
        end;
      end;
    kAsioEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kAsioResetRequest         :
      begin
        OutputComponent.DoReset := True;
        Result := 1;
      end;
    kAsioBufferSizeChange     :
      begin
        OutputComponent.DoReset := True;
        Result := 1;
      end;
    kAsioResyncRequest        :  ;
    kAsioLatenciesChanged     :
      begin
        if Assigned(OutputComponent.FOnLatencyChanged) then
        EventHandler.PostGenericEvent(OutputComponent, OutputComponent.FOnLatencyChanged);
        Result := 1;
      end;
    kAsioSupportsTimeInfo     :  Result := 1;
    kAsioSupportsTimeCode     :  Result := 0;
    kAsioSupportsInputMonitor :  ;
  end;
end;

function AsioBufferSwitchTimeInfo(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  params.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;
  AsioBufferSwitchOutput(doubleBufferIndex, directProcess);
  Result := nil;
end;

procedure AsioBufferSwitchInput(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
var
   s1, s2 : TASIOInt64;
 begin
  if GStop  then
    Exit;
  if InputComponent.FChan = 1 then
    Move(InputComponent.BufferInfo[0].buffers[doubleBufferIndex]^, oBuf[(WriteIndex mod BlockAlign)*BlockSize], InputComponent._BufSize*4)
  else
  begin
    if InputComponent.FBPS = 32 then
      InterleaveStereo32(InputComponent.BufferInfo[0].buffers[doubleBufferIndex], InputComponent.BufferInfo[1].buffers[doubleBufferIndex],
         @oBuf[(WriteIndex mod BlockAlign)*BlockSize], InputComponent._BufSize)
    else
    if InputComponent.FBPS = 16 then
      InterleaveStereo16(InputComponent.BufferInfo[0].buffers[doubleBufferIndex], InputComponent.BufferInfo[1].buffers[doubleBufferIndex],
         @oBuf[(WriteIndex mod BlockAlign)*BlockSize], InputComponent._BufSize)
  end;
  Inc(InputComponent.FPosition, InputComponent._BufSize*(InputComponent.FBPS shr 3));
  if (InputComponent.FSamplesToRead >= 0) then
    if InputComponent.FPosition >= InputComponent.FSize then
        GStop := True;
  Inc(WriteIndex);
  if InputComponent.FPlayRecording then
  begin
    Move(InputComponent.BufferInfo[0].buffers[doubleBufferIndex]^, InputComponent.BufferInfo[InputComponent.FChan].buffers[doubleBufferIndex]^, InputComponent._BufSize*(InputComponent.FBPS shr 3));
    if InputComponent.FChan = 1 then
       Move(InputComponent.BufferInfo[0].buffers[doubleBufferIndex]^, InputComponent.BufferInfo[InputComponent.FChan+1].buffers[doubleBufferIndex]^, InputComponent._BufSize*(InputComponent.FBPS shr 3))
    else
       Move(InputComponent.BufferInfo[1].buffers[doubleBufferIndex]^, InputComponent.BufferInfo[InputComponent.FChan+1].buffers[doubleBufferIndex]^, InputComponent._BufSize*(InputComponent.FBPS shr 3))
  end else
  begin
    FillChar(InputComponent.BufferInfo[InputComponent.FChan].buffers[doubleBufferIndex]^, InputComponent._BufSize*(InputComponent.FBPS shr 3), 0);
    FillChar(InputComponent.BufferInfo[InputComponent.FChan+1].buffers[doubleBufferIndex]^, InputComponent._BufSize*(InputComponent.FBPS shr 3), 0);
  end;
  if Assigned(InputComponent.FOnPositionChanged) then
  begin
    if InputComponent.Device <> nil then
      InputComponent.Device.GetSamplePosition(s1, s2);
    InputComponent.FOnPositionChanged(InputComponent, (s1.hi shl 32) + s1.lo, (s2.hi shl 32) + s2.lo);
  end;
end;

procedure AsioSampleRateDidChange2(sRate: TASIOSampleRate); cdecl;
begin
end;

function AsioMessage2(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;

  case selector of
    kAsioSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kAsioEngineVersion        :  Result := 1;
          kAsioResetRequest         :  Result := 1;
          kAsioBufferSizeChange     :  Result := 0;
          kAsioResyncRequest        :  Result := 1;
          kAsioLatenciesChanged     :  Result := 1;
          kAsioSupportsTimeInfo     :  Result := 1;
          kAsioSupportsTimeCode     :  Result := 1;
          kAsioSupportsInputMonitor :  Result := 0;
        end;
      end;
    kAsioEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kAsioResetRequest         :
      begin
        InputComponent.ASIODone;
        InputComponent.ASIOInit;
        Result := 1;
      end;
    kAsioBufferSizeChange     :
      begin
        InputComponent.ASIODone;
        InputComponent.ASIOInit;
        Result := 1;
      end;
    kAsioResyncRequest        :  ;
    kAsioLatenciesChanged     :
      begin
        if Assigned(InputComponent.FOnLatencyChanged) then
        EventHandler.PostGenericEvent(InputComponent, InputComponent.FOnLatencyChanged);
        Result := 1;
      end;
    kAsioSupportsTimeInfo     :  Result := 1;
    kAsioSupportsTimeCode     :  Result := 0;
    kAsioSupportsInputMonitor :  ;
  end;
end;

function AsioBufferSwitchTimeInfo2(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  params.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;
  AsioBufferSwitchInput(doubleBufferIndex, directProcess);
  Result := nil;
end;

procedure AsioBufferSwitchDuplex(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
begin
  BufferIndex := doubleBufferIndex;
  if directProcess = ASIOTrue then
     DuplexComponent.ProcessBuffers(DuplexComponent)
  else begin
    sleep(2);
    EventHandler.PostNonGuiEvent(DuplexComponent, DuplexComponent.ProcessBuffers);
  end;
end;

procedure AsioSampleRateDidChange3(sRate: TASIOSampleRate); cdecl;
begin
end;

function AsioMessage3(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;

  case selector of
    kAsioSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kAsioEngineVersion        :  Result := 1;
          kAsioResetRequest         :  Result := 1;
          kAsioBufferSizeChange     :  Result := 0;
          kAsioResyncRequest        :  Result := 1;
          kAsioLatenciesChanged     :  Result := 1;
          kAsioSupportsTimeInfo     :  Result := 1;
          kAsioSupportsTimeCode     :  Result := 1;
          kAsioSupportsInputMonitor :  Result := 0;
        end;
      end;
    kAsioEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kAsioResetRequest         :
      begin
        DuplexComponent.ASIODone;
        DuplexComponent.ASIOInit;
        Result := 1;
      end;
    kAsioBufferSizeChange     :
      begin
        DuplexComponent.ASIODone;
        DuplexComponent.ASIOInit;
        Result := 1;
      end;
    kAsioResyncRequest        :  ;
    kAsioLatenciesChanged     :
      begin
        if Assigned(DuplexComponent.FOnLatencyChanged) then
        EventHandler.PostGenericEvent(DuplexComponent, DuplexComponent.FOnLatencyChanged);
        Result := 1;
      end;
    kAsioSupportsTimeInfo     :  Result := 0;
    kAsioSupportsTimeCode     :  Result := 0;
    kAsioSupportsInputMonitor :  ;
  end;
end;

function AsioBufferSwitchTimeInfo3(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  params.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;
  AsioBufferSwitchDuplex(doubleBufferIndex, directProcess);
  Result := nil;
end;


constructor TASIOAudioOut.Create;
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    Thread.Priority := tpTimeCritical;
  end;
  OutputComponent := Self;
  Self.Devices := nil;
  ListAsioDrivers(Self.Devices);
  FDeviceCount := Length(Devices);
  FOutputChannels := 2;
  Callbacks.bufferSwitch := AuAsio.AsioBufferSwitchOutput;
  Callbacks.sampleRateDidChange := AuAsio.AsioSampleRateDidChange;
  Callbacks.asioMessage := AuAsio.AsioMessage;
  Callbacks.bufferSwitchTimeInfo := AuAsio.AsioBufferSwitchTimeInfo;
//  Thread.Priority := tpTimeCritical;
end;

destructor TASIOAudioOut.Destroy;
begin
  AsioDone;
  SetLength(Devices, 0);
  inherited Destroy;
 end;

procedure TASIOAudioOut.SetDeviceNumber(i : Integer);
begin
  if FDeviceCount = 0 then Exit;
  if (i < 0) or (i >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [i]));
  FDeviceNumber := i;
end;

function TASIOAudioOut.GetDeviceName(Number : Integer) : String;
begin
  if (Number < 0) or (Number >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [Number]));
  Result := String(Devices[Number].name);
end;

procedure TASIOAudioOut.Prepare;
var
  i : Integer;
begin
  ACS := TCriticalSection.Create;
  FInput.Init;
  FOutputChannels := Finput.Channels;
  ASIOInit;
  Device.SetSampleRate(FInput.SampleRate);
  ASIODone;
  ASIOInit;
  Chan := FOutputChannels;
  SR := FInput.SampleRate;
  if Device.CanSampleRate(SR) <> ASE_OK then
    raise EAuException.Create(Format('ASIO driver doesn''t support sample rate of %d. Use resampler.', [SR]))
  else  Device.SetSampleRate(Round(SR));
  BPS := FInput.BitsPerSample;
  GStop := False;
  DoReset := False;
  for i := 0 to Chan - 1 do
  begin
    FillChar(BufferInfo[i].buffers[0]^, FBufferSize, 0);
    FillChar(BufferInfo[i].buffers[1]^, FBufferSize, 0);
  end;
//  AsioBufferSwitchOutput(1, AsioTrue);
//  Move(BufferInfo[0].buffers[0]^, BufferInfo[0].buffers[1]^, FBufferSize);
  if Device.Start <> ASE_OK then
  raise EAuException.Create('Cannot start ASIO driver');
  DevStopped := False;
  FLastBlock := False;
  _Prefetched := False;
end;

function TASIOAudioOut.DoOutput(Abort: Boolean) : Boolean;
begin
  if Abort or GStop then
  begin
    if not DevStopped then
    begin
     if Device <> nil then
      Device.Stop;
      DevStopped := True;
    end;
    Result := False;
    Exit;
  end;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if DoReset then
  begin
    DoReset := False;
    AsioDone;
    AsioInit;
    if Assigned(FOnDriverReset) then
        EventHandler.PostGenericEvent(Self, FOnDriverReset);
  end;
      if not (FLastBlock or GStop) then
      begin
        ACS.Enter;
        if not _Prefetched then
        begin
          L :=  FBufferSize*(BPS shr 3)*FOutputChannels;
          L := FInput.FillBufferUnprotected(@iBuf[0], L, FLastBlock);
          _Prefetched := True;
        end;
        ACS.Leave;
      end;
  Result := True;
end;

procedure TASIOAudioOut.Done;
begin
  GStop := True;
  if not DevStopped then
  begin
    if Device <> nil then
    Device.Stop;
    DevStopped := True;
  end;
  AsioDone;
  DoReset := False;
  FInput.Flush;
  FreeAndNil(ACS);
end;

procedure TASIOAudioOut.ASIOInit;
var
  min, max, pref : LongWord;
  i, Dummie : LongWord;
  chi : TAsioChannelInfo;
begin
  FFloat := False;
  FPacked32 := False;
  if ASIOStarted then Exit;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid ASIO device number');
  if OpenAsioCreate(Devices[FDeviceNumber].id, Device) then
  begin
      if (Device <> nil) then
      begin
        if not Succeeded(Device.Init(TForm(Self.Owner).Handle)) then
        begin
          Device := nil;  // RELEASE
          raise EAuException.Create('Failed to initialize ASIO device');
        end;
      end else
        raise EAuException.Create('Failed to open ASIO device');
  end else
    raise EAuException.Create('Failed to open ASIO device');
  Device.GetChannels(Dummie, FSupportedChannels);
  Device.GetBufferSize(min, max, pref, Dummie);
  case FASIOBufferSize of
    absPreferred: FBufferSize := pref;
    absMinimum: FBufferSize := min;
    absMaximum: FBufferSize := max;
  end;
  if (FoutputChannels < 1) or (FOutputChannels > FSupportedChannels) then
     raise EAuException.Create(Format('ASIO: %d channels are not available.', [FOutputChannels]));
  for i := 0  to FOutputChannels - 1 do
  begin
    BufferInfo[i].isInput := ASIOFalse;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
 (* for i := FOutputChannels  to FOutputChannels + 1 do
  begin
    BufferInfo[i].isInput := ASIOTrue;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end; *)

  // TODO: Add multichannel support
  if Device.CreateBuffers(@BufferInfo, FOutputChannels, FBufferSize, Callbacks) <> ASE_OK then
     raise EAuException.Create('ASIO: failed to create output buffers.');
  chi.channel := 0;
  chi.isInput := ASIOFalse;
  Device.GetChannelInfo(chi);
   case chi.vType of
      ASIOSTInt16LSB   :  FoutputBPS := 16;
      ASIOSTInt24LSB   :  FoutputBPS := 24;
      ASIOSTInt32LSB   :  FoutputBPS := 32;
      ASIOSTFloat32LSB :
                        begin
                          FoutputBPS := 32;
                          FFloat := True;
                        end;
    ASIOSTInt32LSB16 :
                        begin
                          FoutputBPS := 16;
                          FPacked32 := True;
                        end;
    ASIOSTInt32LSB24 :
                        begin
                          FoutputBPS := 24;
                          FPacked32 := True;
                        end;
    else raise EAuException.Create('ASIO: Unsupported sample format.');
  end;
  Device.GetLatencies(Dummie, FLatency);
  ASIOStarted := True;
end;

procedure TASIOAudioOut.ASIODone;
begin
  if not ASIOStarted then Exit;
  ASIOStarted := False;
  if Device <> nil then
  Device.DisposeBuffers;
  Device := nil;
end;

procedure TASIOAudioOut.ReleaseASIODriver;
begin
  ASIODone;
end;

function TASIOAudioOut.GetOutputBPS;
begin
  ASIOInit;
  Result := FoutputBPS;
end;

function TASIOAudioOut.GetMaxOutputChannels;
begin
  ASIOInit;
  Result := FSupportedChannels;
end;

function TASIOAudioOut.GetLatency;
begin
  ASIOInit;
  Result := FLatency;
end;


function TASIOAudioOut.IsSampleRateSupported(SR: Integer) : Boolean;
var
  D : Double;
begin
  ASIOInit;
  D := SR;
  Result := Device.CanSampleRate(D) = ASE_OK;
  ASIODone;
end;


procedure TASIOAudioOut.CallProcessBuffer;
var
 m : TGenericEvent;
begin
  m := ProcessBuffer;
  EventHandler.PostGenericEvent(TComponent(Self), m);
end;

procedure TASIOAudioOut.Pause;
begin
  inherited Pause;
  Device.Stop;
end;

procedure TASIOAudioOut.Resume;
begin
  Device.Start;
  inherited Resume;
end;

procedure TASIOAudioOut.ShowSetupDlg;
begin
  ASIODone;
  ASIOInit;
  Device.ControlPanel;
  ASIODone;
  ASIOInit;
end;

procedure TASIOAudioOut.SetSampleRate;
begin
  if Device <> nil then
    Device.SetSampleRate(SR);
end;


function TASIOAudioOut.GetSampleRate;
var
  D : Double;
begin
  if Device <> nil then
    Device.GetSampleRate(D)
  else D := 44100;
  Result := Round(D);
end;

constructor TASIOAudioIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HoldEvent := TEvent.Create(nil, True, True, 'holdevent');
  InputComponent := Self;
  Self.Devices := nil;
  ListAsioDrivers(Self.Devices);
  FDeviceCount := Length(Devices);
  Self.FChan := 2;
  Self.FFreq := 44100;
  FSize := -1;
  FRecTime := -1;
  FSamplesToRead := -1;
  Callbacks.bufferSwitch := AuAsio.AsioBufferSwitchInput;
  Callbacks.sampleRateDidChange := AuAsio.AsioSampleRateDidChange2;
  Callbacks.asioMessage := AuAsio.AsioMessage2;
  Callbacks.bufferSwitchTimeInfo := AuAsio.AsioBufferSwitchTimeInfo2;
  ASIOStarted := False;
end;

destructor TASIOAudioIn.Destroy;
begin
  HoldEvent.Free;
  ASIODone;
  inherited Destroy;
end;

procedure TASIOAudioIn.ASIOInit;
var
  i, Dummie, min, max, pref : LongWOrd;
  chi : TAsioChannelInfo;
begin
  if ASIOStarted then Exit;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid ASIO device number');
  if OpenAsioCreate(Devices[FDeviceNumber].id, Device) then
  begin
      if (Device <> nil) then
      begin
        if not Succeeded(Device.Init(TForm(Self.Owner).Handle)) then
        begin
          Device := nil;  // RELEASE
          raise EAuException.Create('Failed to initialize ASIO device');
        end;
      end else
        raise EAuException.Create('Failed to open ASIO device');
  end else
    raise EAuException.Create('Failed to open ASIO device');
  Device.GetChannels(Dummie, FMaxChan);
  Device.GetBufferSize(min, max, pref, Dummie);
  case FASIOBufferSize of
    absPreferred: _BufSize := pref;
    absMinimum: _BufSize := min;
    absMaximum: _BufSize := max;
  end;
  if (FChan < 1) or (FChan > FMaxChan) then
     raise EAuException.Create(Format('ASIO: %d channels are not available.', [FMaxChan]));
  for i := 0  to FChan - 1 do
  begin
    BufferInfo[i].isInput := ASIOTrue;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  for i := FChan  to FChan + 1 do
  begin
    BufferInfo[i].isInput := ASIOFalse;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  if Device.CreateBuffers(@BufferInfo, FChan + 2, _BufSize, Callbacks) <> ASE_OK then
     raise EAuException.Create('ASIO: failed to create output buffers.');
  chi.channel := 0;
  chi.isInput := ASIOFalse;
  Device.GetChannelInfo(chi);
   case chi.vType of
      ASIOSTInt16LSB   :  FBPS := 16;
      ASIOSTInt24LSB   :  FBPS := 24;
      ASIOSTInt32LSB   :  FBPS := 32;
    else raise EAuException.Create('ASIO: Unsupported sample format.');
  end;
  Device.GetLatencies(Dummie, FLatency);
  FSampleSize := FBPS*FChan div 8;
  ASIOStarted := True;
end;

procedure TASIOAudioIn.ASIODone;
begin
  if not ASIOStarted then Exit;
  ASIOStarted := False;
  if Device <> nil then
  Device.DisposeBuffers;
  Device := nil;
end;

function TASIOAudioIn.GetMaxOutputChannels;
begin
  ASIOInit;
  Result := FMaxChan;
end;

function TASIOAudioIn.GetLatency;
begin
  ASIOInit;
  Result := FLatency;
end;


function TASIOAudioIn.IsSampleRateSupported(SR: Integer) : Boolean;
var
  D : Double;
begin
  ASIOInit;
  D := SR;
  Result := Device.CanSampleRate(D) = ASE_OK;
  ASIODone;
end;

procedure TASIOAudioIn.SetDeviceNumber(i : Integer);
begin
  if FDeviceCount = 0 then Exit;
  if (i < 0) or (i >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [i]));
  FDeviceNumber := i;
end;

function TASIOAudioIn.GetDeviceName(Number : Integer) : String;
begin
  if (Number < 0) or (Number >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [Number]));
  Result := String(Devices[Number].name[0]);
end;

procedure TASIOAudioIn.SetSampleRate;
begin
  ASIOInit;
  Device.SetSampleRate(SR);
  FFreq := GetSR;
  ASIODone;
end;

function TASIOAudioIn.GetTotalTime : LongWord;
var
  BytesPerSec : Integer;
begin
  ASIOInit;
  BytesPerSec := Self.InSampleRate*FSampleSize;
  if FSamplesToRead < 0 then Result := 0
  else
  Result := Round(FSamplesToRead/BytesPerSec);
end;

function TASIOAudioIn.GetTotalSamples : Int64;
begin
  Result := FSamplesToRead;
end;

function TASIOAudioIn.GetBPS : LongWord;
begin
  ASIOInit;
  Result := FBPS;
end;

function TASIOAudioIn.GetCh : LongWord;
begin
  Result := FChan;
end;

function TASIOAudioIn.GetSR : LongWord;
var
  D : Double;
begin
  ASIOInit;
  Device.GetSampleRate(D);
  Result := Round(D);
end;

procedure TASIOAudioIn.SetRecTime;
begin
  FRecTime := aRecTime;
{$WARNINGS OFF}
  if FRecTime >= 0 then FSamplesToRead := FRecTime*FFreq
  else FSamplesToRead := -1;
 {$WARNINGS ON}
end;

procedure TASIOAudioIn.InitInternal;
begin
  if Busy then raise EAuException.Create('The component is busy');
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
{$WARNINGS OFF}
  if FRecTime > 0 then FSamplesToRead := FRecTime*FFreq;
{$WARNINGS ON}
  FPosition := 0;
  WriteIndex := 0;
  ReadIndex := 0;
  ReadOffset := 0;
  GStop := False;
  Busy := True;
  ASIOInit;
//  Device.SetSampleRate(FFreq); // If you forget to set the input sample rate, let it record at the default.
  FSampleSize := FChan*FBPS div 8;
  if FSamplesToRead > 0 then
    FSize := FSamplesToRead*FSampleSize
  else
    FSize := -1;
  BytesInBuf := _BufSize*FChan*FBPS div 8;
  Device.Start;
end;

procedure TASIOAudioIn.FlushInternal;
begin
  GStop := True;
  Device.Stop;
  ASIODone;
  Busy := False;
end;

procedure TASIOAudioIn.GetDataInternal;
begin
  if not Busy then  raise EAuException.Create('The Stream is not opened');
  HoldEvent.WaitFor(INFINITE);
  if ReadIndex >= WriteIndex then
  begin
    if GStop then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end else
      Sleep(50);
  end;
  if ReadIndex >= WriteIndex then
     raise EAuException.Create('Reading data from ASIO failed.');
  Buffer := @oBuf[(ReadIndex mod BlockAlign)*BlockSize + ReadOffset];
  if Bytes >= BytesInBuf - ReadOffset then
  begin
    Bytes := BytesInBuf - ReadOffset;
    ReadOffset := 0;
    Inc(ReadIndex);
  end else
    Inc (ReadOffset, Bytes);
end;

procedure TASIOAudioIn._Pause;
begin
  if Device <> nil then
    Device.Stop;
end;

procedure TASIOAudioIn._Resume;
begin
  if Device <> nil then
    Device.Start;
end;

procedure TASIOAudioIn.ShowSetupDlg;
begin
  ASIODone;
  ASIOInit;
  Device.ControlPanel;
  ASIODone;
  ASIOInit;
end;

procedure TASIOAudioIn.HoldOff(b: Boolean);
begin
  if b then
    HoldEvent.ResetEvent
  else
    HoldEvent.SetEvent;
end;

constructor TASIOAudioDuplex.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HoldEvent := TEvent.Create(nil, True, True, 'holdevent');
  DuplexComponent := Self;
  Self.Devices := nil;
  ListAsioDrivers(Self.Devices);
  FDeviceCount := Length(Devices);
  Self.FChan := 2;
  Self.FFreq := 44100;
  FSize := -1;
  Callbacks.bufferSwitch := AuAsio.AsioBufferSwitchDuplex;
  Callbacks.sampleRateDidChange := AuAsio.AsioSampleRateDidChange3;
  Callbacks.asioMessage := AuAsio.AsioMessage3;
  Callbacks.bufferSwitchTimeInfo := AuAsio.AsioBufferSwitchTimeInfo3;
  ASIOStarted := False;
end;

destructor TASIOAudioDuplex.Destroy;
begin
  if Self.Busy then
     if Finput <> nil then
       FInput.Flush;
  HoldEvent.Free;
  ASIODone;
  inherited Destroy;
end;

procedure TASIOAudioDuplex.ASIOInit;
var
  i, Dummie, min, max, pref : LongWord;
  chi : TAsioChannelInfo;
begin
  if ASIOStarted then Exit;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid ASIO device number');
  if OpenAsioCreate(Devices[FDeviceNumber].id, Device) then
  begin
      if (Device <> nil) then
      begin
        if not Succeeded(Device.Init(TForm(Self.Owner).Handle)) then
        begin
          Device := nil;  // RELEASE
          raise EAuException.Create('Failed to initialize ASIO device');
        end;
      end else
        raise EAuException.Create('Failed to open ASIO device');
  end else
    raise EAuException.Create('Failed to open ASIO device');
  Device.GetChannels(Dummie, FChan);
  if FChan > 2 then FChan := 2;

  Device.GetBufferSize(min, max, pref, Dummie);
  case FASIOBufferSize of
    absPreferred: _BufSize := pref;
    absMinimum: _BufSize := min;
    absMaximum: _BufSize := max;
  end;
  for i := 0  to FChan - 1 do
  begin
    BufferInfo[i].isInput := ASIOTrue;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  for i := FChan  to FChan + 1 do
  begin
    BufferInfo[i].isInput := ASIOFalse;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  if Device.CreateBuffers(@BufferInfo, FChan + 2, _BufSize, Callbacks) <> ASE_OK then
     raise EAuException.Create('ASIO: failed to create output buffers.');
  chi.channel := 0;
  chi.isInput := ASIOFalse;
  Device.GetChannelInfo(chi);
   case chi.vType of
      ASIOSTInt16LSB   :  FBPS := 16;
      ASIOSTInt24LSB   :  FBPS := 24;
      ASIOSTInt32LSB   :  FBPS := 32;
    else raise EAuException.Create('ASIO: Unsupported sample format.');
  end;
  Device.GetLatencies(Dummie, FLatency);
  FSampleSize := FBPS*FChan div 8;
  ASIOStarted := True;
end;

procedure TASIOAudioDuplex.ASIODone;
begin
  if not ASIOStarted then Exit;
  ASIOStarted := False;
  if Device <> nil then
  Device.DisposeBuffers;
  Device := nil;
end;

function TASIOAudioDuplex.GetBPS : LongWord;
begin
  ASIOInit;
  Result := FBPS;
end;

function TASIOAudioDuplex.GetCh : LongWord;
begin
  Result := FChan;
end;

function TASIOAudioDuplex.GetSR : LongWord;
var
  D : Double;
begin
  ASIOInit;
  Device.GetSampleRate(D);
  Result := Round(D);
end;

procedure TASIOAudioDuplex.SetDeviceNumber(i : Integer);
begin
  if FDeviceCount = 0 then Exit;
  if (i < 0) or (i >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [i]));
  FDeviceNumber := i;
end;

function TASIOAudioDuplex.GetDeviceName(Number : Integer) : String;
begin
  if (Number < 0) or (Number >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [Number]));
  Result := String(Devices[Number].name[0]);
end;

function TASIOAudioDuplex.GetLatency;
begin
  ASIOInit;
  Result := FLatency;
end;

 procedure Mix32(Op1, Op2 : PBuffer32; DataSize : Integer);
var
  i : Integer;
begin
  for i:= 0 to DataSize - 1 do
  begin
    Op1[i] := Round(Op1[i]/2.01 + Op2[i]/2.01);
  end;
end;

 procedure Mix16(Op1, Op2 : PBuffer16; DataSize : Integer);
var
  i : Integer;
begin
  for i:= 0 to DataSize - 1 do
  begin
    Op1[i] := Round(Op1[i]/2.01 + Op2[i]/2.01);
  end;
end;


(*procedure Mix32(Op1, Op2 : PBuffer32; DataSize : Integer);
var
  i : Integer;
  f1, f2 : Single;
begin
  for i:= 0 to DataSize - 1 do
  begin
    f1 := Op1[i]/$80000000;
    f2 :=  Op2[i]/$80000000;
    f1 := (f1 + f2)/2;
    if f1 > 1 then f1 :=1;
    if f1 < -1 then f1 := -1;
    Op1[i] := Floor(f1*$80000000);
  end;
//    Op1[i] := Round(Int64(Op1[i] + Op2[i])/3);
end; *)

procedure TASIOAudioDuplex.ProcessBuffers(Sender: TComponent);
var
  s1, s2 : TASIOInt64;
begin
   if Device = nil then Exit;
   if Assigned(FOnPositionChanged) then
   begin
     Device.GetSamplePosition(s1, s2);
     FOnPositionChanged(Self, (s1.hi shl 32) + s1.lo, (s2.hi shl 32) + s2.lo)
   end;
   FInput.FillBuffer(@iBuf, _BufSize*2*FBPS div 8, GStop);
   InterleaveStereo32(BufferInfo[0].buffers[BufferIndex], BufferInfo[1].buffers[BufferIndex],
       @oBuf[(WriteIndex mod BlockAlign)*BlockSize], _BufSize);
   if FEchoRecording then
   begin
     if FBPS = 32 then
       Mix32(@iBuf, @oBuf[(WriteIndex mod BlockAlign)*BlockSize], _BufSize*2)
     else
     if FBPS = 16 then
       Mix16(@iBuf, @oBuf[(WriteIndex mod BlockAlign)*BlockSize], _BufSize*2);
     if FRecordInput then
       Move(iBuf, oBuf[(WriteIndex mod BlockAlign)*BlockSize], _BufSize*2*FBPS div 8);
   end else
   begin
     if FRecordInput then
     begin
       if FBPS = 32 then
         Mix32(@oBuf[(WriteIndex mod BlockAlign)*BlockSize], @iBuf, _BufSize*2)
       else
       if FBPS = 16 then
         Mix16(@iBuf, @oBuf[(WriteIndex mod BlockAlign)*BlockSize], _BufSize*2);
     end;
   end;
   Inc(WriteIndex);
   if FBPS = 32 then
   DeinterleaveStereo32(@iBuf, BufferInfo[2].buffers[BufferIndex], BufferInfo[3].buffers[BufferIndex], _BufSize)
   else
   if FBPS = 16 then
   DeinterleaveStereo16(@iBuf, BufferInfo[2].buffers[BufferIndex], BufferInfo[3].buffers[BufferIndex], _BufSize);
   Device.OutputReady;
end;

procedure TASIOAudioDuplex.InitInternal;
begin
  if not Assigned(FInput) then
  raise EAuException.Create('Input not assigned');
  Busy := True;
  FInput.Init;
  ASIOInit;
  if Device.SetSampleRate(FInput.SampleRate) <> ASE_OK then
  begin
    ASIODone;
    raise EAuException.Create(Format('Sample rate of %d Hz is not supported', [FFreq]));
  end;
  ASIODone;
  ASIOInit;
  if FInput.BitsPerSample <> FBPS then
    raise EAuException.Create(Format('Input sample is not %d bit', [FBPS]));
  if FInput.Channels <> FChan then
    raise EAuException.Create(Format('Input is not %d channel', [FChan]));
  FPosition := 0;
  FSize := -1;
  Device.Start;
  BytesInBuf := _BufSize*FChan*FBPS div 8;
end;

procedure TASIOAudioDuplex.FlushInternal;
begin
  GStop := True;
  Device.Stop;
  Finput.Flush;
  ASIODone;
  Busy := False;
end;

procedure TASIOAudioDuplex.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
begin
  if not Busy then  raise EAuException.Create('The Stream is not opened');
  HoldEvent.WaitFor(INFINITE);
  if ReadIndex >= WriteIndex then
  begin
    if GStop then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end else
      Sleep(50);
  end;
  if ReadIndex >= WriteIndex then
     raise EAuException.Create('Reading data from ASIO failed.');
  Buffer := @oBuf[(ReadIndex mod BlockAlign)*BlockSize + ReadOffset];
  if Bytes >= BytesInBuf - ReadOffset then
  begin
    Bytes := BytesInBuf - ReadOffset;
    ReadOffset := 0;
    Inc(ReadIndex);
  end else
    Inc (ReadOffset, Bytes);
end;

procedure TASIOAudioDuplex._Pause;
begin
  if Device <> nil then
    Device.Stop;
  if FInput <> nil then
    FInput._Pause;
end;

procedure TASIOAudioDuplex._Resume;
begin
  if FInput <> nil then
    FInput._Resume;
  if Device <> nil then
    Device.Start;
end;

procedure TASIOAudioDuplex.ShowSetupDlg;
begin
  ASIODone;
  ASIOInit;
  Device.ControlPanel;
  ASIODone;
  ASIOInit;
end;

procedure TASIOAudioDuplex.HoldOff(b: Boolean);
begin
  if b then
    HoldEvent.ResetEvent
  else
    HoldEvent.SetEvent;
end;

procedure TASIOAudioOut.Jump(Offs : Integer);
begin
  Pause;
  if Assigned(Finput) then
  begin
    if FInput is TAuConverter then
      TAuConverter(FInput)._Jump(Offs);
    if FInput is TAuFileIn then
      TAuFileIn(FInput)._Jump(Offs);
  end;
  Resume;
end;


end.
