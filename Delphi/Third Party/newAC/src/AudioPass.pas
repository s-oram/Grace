(*
  This file is a part of New Audio Components package 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: AudioPass.pas 304 2008-02-04 12:41:10Z andrei.borovsky $ *)

(* Title: AudioPass
   This unit contains the <TAudioPass> component. *)


unit AudioPass;

interface

uses
  Classes, SysUtils, Forms, ACS_Types, ACS_Procs, ACS_Classes, Windows, DSWrapper, _DirectSound, Math;

type

  (* Class: TAudioPass
      A <TAuConverter> descendant which plays sound passing through it.
      Technically, it's a converter, as it sits between input and output
      components in the audio chain. But since its purpose is simply to play
      sound, it passes the sound stream unchanged. You can use this component
      if you want to listen to the sound being processed. For an example look
      at the <Rip'n'Listen Demo>. *)
  TAudioPass = class(TAuConverter)
  private
    FMute : Boolean;
    Freed : Boolean;
    DSW : DSoundWrapper;
    Devices : DSW_Devices;
    Chan, SR, BPS : LongWord;
    StartInput : Boolean;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    _BufSize : LongWord;
    FBufferSize : LongWord;
    FillByte : Byte;
    FUnderruns, _TmpUnderruns : LongWord;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure SetMute(b : Boolean);
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
   (* Property: DeviceCount
         This read only property returns the number of logical DirectSound
         output devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName
         This read only array property returns the name of the device
         specified by Number.
       
       Number:
        An integer ranging from 0 to <DeviceCount> -1.
    *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: Underruns
         This read only property returns the number of internal buffer
         underruns that have occured during playback. *)
    property Underruns : LongWord read FUnderruns;
    (* Property: BufferSize
         Use this property to set the component's internal buffer size if the
         defult one doesn't suit you. *)
    property BufferSize : LongWord read FBufferSize write FBufferSize;
    property BufSize : LongWord read _BufSize;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system.        
         Values range from 0 to <DeviceCount> - 1
    *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: Mute
         If this property is set to True the component doesn't play audio, but just lets it pass through. Target audio operations usually go faster in Mute mode, since when the component plays sound, audio data is passed along the chain at playback speed. You can change the value of Mute while the component is processing audio.
    *)
    property Mute : Boolean read FMute write SetMute;
  end;

const

  (* Constants:
      DS_BUFFER_SIZE = $10000 - Size in frames, not bytes
      DS_POLLING_INTERVAL = 30 - milliseconds   
  *)
  DS_BUFFER_SIZE = $10000; 
  DS_POLLING_INTERVAL = 30;

implementation

constructor TAudioPass.Create;
begin
  inherited Create(AOwner);
  FBufferSize := DS_BUFFER_SIZE;
  DSW_EnumerateOutputDevices(@Devices);
  FDeviceCount := Devices.devcount;
end;

destructor TAudioPass.Destroy;
begin
  inherited Destroy;
end;

procedure TAudioPass.SetDeviceNumber(i : Integer);
begin
  FDeviceNumber := i
end;

function TAudioPass.GetDeviceName(Number : Integer) : String;
begin
  if (Number < FDeviceCount) then Result := PChar(@(Devices.dinfo[Number].Name[0]))
  else Result := '';
end;

function TAudioPass.GetBPS;
begin
  Result := FInput.BitsPerSample;
end;

function TAudioPass.GetCh;
begin
  Result := FInput.Channels;
end;

function TAudioPass.GetSR;
begin
  Result := FInput.SampleRate;
end;

procedure TAudioPass.InitInternal;
var
  Res : HResult;
  Form : TForm;
  Wnd : HWND;
begin
  if not Assigned(FInput) then
  raise EAuException.Create('Input not assigned');
  FInput.Init;
  Busy := True;
  FSize := FInput.Size;
  FPosition := 0;
  Freed := False;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
  Chan := FInput.Channels;
  SR := FInput.SampleRate;
  BPS := FInput.BitsPerSample;
  DSW_Init(DSW);
  Res := DSW_InitOutputDevice(DSW, @(Devices.dinfo[FDeviceNumber].guid));
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound device');
  if Owner is TForm then
  begin
    Form := Owner as TForm;
    Wnd := Form.Handle;
  end else Wnd := 0;
  _BufSize := FBufferSize*(BPS shr 3)*Chan;
  if BPS <> 8 then
    FillByte := 0
  else
    FillByte := 128;
  Res := DSW_InitOutputBuffer(DSW, Wnd, BPS, SR, Chan, _BufSize);
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound buffer');
  StartInput := True;
  _TmpUnderruns := 0;
end;

procedure TAudioPass.FlushInternal;
begin
  Finput.Flush;
  if not Freed then
  begin
    DSW_Term(DSW);
  end;
  Freed := True;
  Busy := False;
end;

procedure TAudioPass.GetDataInternal;
var
  lb : LongWord;
//  Res : HRESULT;
  PlayTime, CTime : LongWord;
begin
  if StartInput then
  begin
    if FPosition = _BufSize then
    begin
      DSW_StartOutput(DSW);
      if FMute then
         DSW_StopOutput(DSW);
      StartInput := False;
    end else
    begin
      if _BufSize - FPosition < Bytes then Bytes := _BufSize - FPosition;
      FInput.GetData(Buffer, Bytes);
      if Bytes = 0 then
        Exit;
      DSW_WriteBlock(DSW, Buffer, Bytes);
      Exit;
    end;
  end;
  if FMute then
  begin
    FInput.GetData(Buffer, Bytes);
    Exit;
  end;
  repeat
    Sleep(DS_POLLING_INTERVAL);
    DSW_QueryOutputSpace(DSW, lb);
    lb := lb - (lb mod DSW.dsw_BytesPerFrame);
  until lb > 0;
  if lb < Bytes then Bytes := lb;
  FInput.GetData(Buffer, Bytes);
  if Bytes > 0 then
    DSW_WriteBlock(DSW, Buffer, Bytes)
  else
  begin
    PlayTime := Round(_BufSize/(Chan*(BPS div 8)*SR))*1000;
    CTime := 0;
    while CTime < PlayTime do
    begin
      Sleep(100);
      DSW_FillEmptySpace(DSW, FillByte);
      Inc(CTime, 100);
    end;
    DSW_StopOutput(DSW);
  end;
  if _TmpUnderruns <> DSW.dsw_OutputUnderflows then
  begin
    FUnderruns := DSW.dsw_OutputUnderflows - _TmpUnderruns;
    _TmpUnderruns := DSW.dsw_OutputUnderflows;
    DSW_StopOutput(DSW);
    DSW_FillEmptySpace(DSW, FillByte);
    DSW_RestartOutput(DSW);
  end;
end;

procedure TAudioPass.SetMute;
begin
  if StartInput then
  begin
    _Lock;
    FMute := b;
    Exit;
    _Unlock;
  end;
  _Lock;
  if (FMute = False) and (b = True) then
  begin
    DSW_StopOutput(DSW);
  end else
  if FMute <> b then
      DSW_RestartOutput(DSW);
  FMute := b;
  _Unlock;
end;

procedure TAudioPass._Pause;
begin
  DSW_StopOutput(DSW);
  FInput._Pause;
end;

procedure TAudioPass._Resume;
begin
  DSW_RestartOutput(DSW);
  FInput._Resume;
end;

end.
