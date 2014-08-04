(*
  This file is a part of Audio Components Suite v 2.2 (Delphi version).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
  This is the ACS for Delphi (Windows) version of the unit.
*)


unit ACS_Audio;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, Windows, MMSystem;

const

  // Constants for TAudioIn
  INBUF_SIZE = $4000;
  BLOCKS_COUNT = 4;
 // Constants for TAudioOut
  OUTBUF_SIZE = $2000;  // 4K
  AUDIO_DELAY = 6;
  READ_CHUNKS = 8;

  (*
    Note about input and output buffer sizes.
    In order to avoid dropouts we buffer
    input in TAudioIn. The size of input buffer is about
    INBUF_SIZE*BLOCKS_COUNT. Try to change these values to obtain
    better performance.
    The total length of the output buffer is OUTBUF_SIZE*READ_CHUNKS.
    Because output is buffered the TAudioOut component keeps playing
    for some time after all data is read from the input. We make buffer size
    smaller to reduce the time of this delay in case of wav files.
    AUDIO_DELAY is the output components Delay property value
    (see ACS_Classes.pas for details).
    AUDIO_DELAY and READ_CHUNKS represent default values for the Delay and
    ReadChunks properties of TAudioOut. You may set other values at
    runtime to fine-tune the system performance.
  *)

type

  PPWaveHdr = ^PWaveHdr;

  (* TAudioFormat format constants mask : af<SampleRate><Mono/Stereo><BitsPerSample>
  where 1, 2, 4 means sample rate of 11025, 22050, and 44100 Hz respectively
  M, S means mono or stereo, 08, 16 means 8 or 16 bits per sample.
  For example, af4S16 corresponds to 44100 Hz stereo 16 bit format. *)

  TAudioFormat = (af1M08, af1M16, af1S08, af1S16, af2M08, af2M16, af2S08, af2S16,
                  af4M08, af4M16, af4S08, af4S16);

  TAudioFormats = set of TAudioFormat;

  TDeviceInfo = record
    DeviceName : String;
    DrvVersion : LongWord;
    Formats : TAudioFormats;
    Stereo : Boolean;
  end;

  TAudioOut = class(TACSOutput)
  private
    BlockChain : PWaveHdr;
    EOC : PPWaveHdr;
    BlocksCount : Integer;
    FBaseChannel : Integer;
    FVolume : Byte;
    _audio_fd : Integer;
    FReadChunks : Integer;
    procedure SetChannel(Ch : Integer);
    function GetDeviceInfo : TDeviceInfo;
    procedure WriteBlock(P : Pointer; Len : Integer);
    procedure AddBlockToChain(WH : PWaveHdr);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DeviceInfo : TDeviceInfo read GetDeviceInfo;
    property ReadChunks : Integer read FReadChunks write FReadChunks;
  published
    property BaseChannel : Integer read FBaseChannel write SetChannel stored True;
    property Volume : Byte read FVolume write FVolume;
  end;

  TAudioIn = class(TACSInput)
  private
    BlockChain : PWaveHdr;
    EOC : PPWaveHdr;
    BlocksCount : Integer;
    FBaseChannel : Integer;
    _audio_fd : Integer;
    FBPS, FChan, FFreq : Integer;
    buf : array[1..INBUF_SIZE] of Byte;
    FOpened : Integer;
    FRecTime : Integer;
    FRecBytes : Integer;
    procedure OpenAudio;
    procedure CloseAudio;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure SetChannel(Ch : Integer);
    function GetDeviceInfo : TDeviceInfo;
    procedure NewBlock;
    function GetTotalTime : Integer; override;
    procedure AddBlockToChain(WH : PWaveHdr);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property DeviceInfo : TDeviceInfo read GetDeviceInfo;
  published
    property BaseChannel : Integer read FBaseChannel write SetChannel stored True;
    property InBitsPerSample : Integer read GetBPS write FBPS stored True;
    property InChannels : Integer read GetCh write FChan stored True;
    property InSampleRate : Integer read GetSR write FFreq stored True;
    property RecTime : Integer read FRecTime write FRecTime stored True;
  end;

var
  InputChannelsCount : Integer;
  OutputChannelsCount : Integer;

function GetAudioDeviceInfo(DevID : Integer; OutputDev : Boolean) : TDeviceInfo;

implementation

var
  CrSecI, CrSecO : TRTLCriticalSection;

function GetAudioDeviceInfo(DevID : Integer; OutputDev : Boolean) : TDeviceInfo;
var
  WIC : TWaveInCaps;
  i : Integer;
begin
  if OutputDev then
  begin
    if DevID >= OutputChannelsCount then
    raise EACSException.Create('Device '+ IntToStr(DevID) + ' is not available.');
  end else
  begin
    if DevID >= InputChannelsCount then
    raise EACSException.Create('Device '+ IntToStr(DevID) + ' is not available.');
  end;
  if OutputDev then waveOutGetDevCaps(DevID, @WIC, SizeOf(WIC))
  else waveInGetDevCaps(DevID, @WIC, SizeOf(WIC));
  i := 0;
  while WIC.szPname[i] <> #0 do Inc(i);
  SetLength(Result.DeviceName, i);
  Move(WIC.szPname[0], Result.DeviceName[1], i);
  Result.Formats := [];
  if (WIC.dwFormats and WAVE_FORMAT_1M08) <> 0 then Result.Formats := Result.Formats + [af1M08];
  if (WIC.dwFormats and WAVE_FORMAT_1M16) <> 0 then Result.Formats := Result.Formats + [af1M16];
  if (WIC.dwFormats and WAVE_FORMAT_1S08) <> 0 then Result.Formats := Result.Formats + [af1S08];
  if (WIC.dwFormats and WAVE_FORMAT_1S16) <> 0 then Result.Formats := Result.Formats + [af1S16];
  if (WIC.dwFormats and WAVE_FORMAT_2M08) <> 0 then Result.Formats := Result.Formats + [af2M08];
  if (WIC.dwFormats and WAVE_FORMAT_2M16) <> 0 then Result.Formats := Result.Formats + [af2M16];
  if (WIC.dwFormats and WAVE_FORMAT_2S08) <> 0 then Result.Formats := Result.Formats + [af2S08];
  if (WIC.dwFormats and WAVE_FORMAT_2S16) <> 0 then Result.Formats := Result.Formats + [af2S16];
  if (WIC.dwFormats and WAVE_FORMAT_4M08) <> 0 then Result.Formats := Result.Formats + [af4M08];
  if (WIC.dwFormats and WAVE_FORMAT_4M16) <> 0 then Result.Formats := Result.Formats + [af4M16];
  if (WIC.dwFormats and WAVE_FORMAT_4S08) <> 0 then Result.Formats := Result.Formats + [af4S08];
  if (WIC.dwFormats and WAVE_FORMAT_4S16) <> 0 then Result.Formats := Result.Formats + [af4S16];
  Result.DrvVersion := WIC.vDriverVersion;
  if WIC.wChannels = 1 then Result.Stereo := False else Result.Stereo := True;
end;

procedure WaveOutProc(hwo, Msg : LongWord; Instance : Pointer; Param1, Param2 : LongWord); stdcall;
var
  Audio : TAudioOut;
begin
  EnterCriticalSection(CrSecO);
  if Msg = WOM_DONE then
  begin
    Audio := TAudioOut(Instance);
    Audio.AddBlockToChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecO);
end;

procedure WaveInProc(hwi, Msg : LongWord; Instance : Pointer; Param1, Param2 : LongWord); stdcall;
var
  Audio : TAudioIn;
begin
  EnterCriticalSection(CrSecI);
  if Msg = WIM_DATA then
  begin
    Audio := TAudioIn(Instance);
    Audio.AddBlockToChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecI);
end;

procedure TAudioOut.AddBlockToChain(WH : PWaveHdr);
begin
  WH.lpNext := nil;
  EOC^ := WH;
  EOC := @WH.lpNext;
  Dec(BlocksCount);
end;

procedure TAudioOut.SetChannel;
begin
  if Buisy then raise EACSException.Create('Component is buisy');
  if OutputChannelsCount = 0 then  FBaseChannel := 0 else
  if Ch < OutputChannelsCount then FBaseChannel := Ch
  else raise EACSException.Create('Channel ' + IntToStr(Ch) + ' is not available');
end;

procedure TAudioOut.Prepare;
var
  WF : TPCMWaveFormat;
begin
  // No exceptions here!
  FInput.Init;
  WF.wf.wFormatTag :=  WAVE_FORMAT_PCM;
  WF.wf.nChannels := FInput.Channels;
  WF.wf.nSamplesPerSec := FInput.SampleRate;
  WF.wBitsPerSample := FInput.BitsPerSample;
  WF.wf.nAvgBytesPerSec := WF.wf.nSamplesPerSec*WF.wBitsPerSample div 8;
  WF.wf.nBlockAlign := WF.wf.nChannels * WF.wBitsPerSample div 8;
  waveOutOpen(@_audio_fd, FBaseChannel, @WF, DWORD(@WaveOutProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
  BlocksCount := 0;
  EOC := @BlockChain;
end;

procedure TAudioOut.Done;
var
  Tmp : PWaveHdr;
begin
  if _audio_fd <> -1 then
  begin
    while BlocksCount > 0 do;
    Tmp := BlockChain;
    while Tmp <> nil do
    begin
      BlockChain := Tmp.lpNext;
      waveOutUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
      FreeMem(Tmp.lpData);
      Dispose(Tmp);
      Tmp := BlockChain;
    end;
    EOC := @BlockChain;
    waveOutClose(_audio_fd);
    _audio_fd := -1;
  end;
  FInput.Flush;
end;

function TAudioOut.DoOutput;
var
  Len, i, k, vCoef : Integer;
  P : Pointer;
  P1 : PBuffer8;
  P2 : PBuffer16;
  Tmp : PWaveHdr;
begin
  // No exceptions Here
  Result := True;
  if not Buisy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  Tmp := BlockChain;         // clear pending data blocks
  while Tmp <> nil do
  begin
    BlockChain := Tmp.lpNext;
    waveOutUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
    Tmp := BlockChain;
  end;
  EOC := @BlockChain;
  (* Write more than one block. This is needed for audio sources like
     Vorbis codec that return data in small chunks. *)
  for k := BlocksCount to FReadChunks do
  begin
    GetMem(P, OUTBUF_SIZE);
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, OUTBUF_SIZE);
    InputLock := False;
    if Len > 0 then Result := True
    else
    begin
      Result := False;
      FreeMem(P);
      Exit;
    end;
    if FVolume < 255 then
    begin
      vCoef := Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P2 := P;
        for i := 0 to (Len shr 1) -1 do
        P2[i] := P2[i]*vCoef;
      end else
      begin
        P1 := P;
        for i := 0 to Len - 1 do
        P1[i] := P1[i]*vCoef;
      end;
    end;
    WriteBlock(P, Len);
  end;
  if Assigned(FOnProgress) then
  begin
    if FInput.Size > 0 then
    if CurProgr <> GetProgress then
    begin
      CurProgr := GetProgress;
      FOnProgress(Self);
    end;
  end;
end;

constructor TAudioOut.Create;
begin
  inherited Create(AOwner);
  FBaseChannel := 0;
  FVolume := 255;
  _audio_fd := -1;
  Delay := AUDIO_DELAY;
  FReadChunks := READ_CHUNKS;
end;

destructor TAudioOut.Destroy;
begin
  if _audio_fd <> -1 then WaveOutClose(_audio_fd);
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
  waveInClose(_audio_fd);
  inherited Destroy;
end;

procedure TAudioIn.OpenAudio;
var
  WF : TPCMWaveFormat;
begin
  WF.wf.wFormatTag :=  WAVE_FORMAT_PCM;
  WF.wf.nChannels := FChan;
  WF.wf.nSamplesPerSec := FFreq;
  WF.wBitsPerSample := FBPS;
  WF.wf.nAvgBytesPerSec := WF.wf.nSamplesPerSec*WF.wBitsPerSample div 8;
  WF.wf.nBlockAlign := WF.wf.nChannels * WF.wBitsPerSample div 8;
  if FOpened = 0 then
  waveInOpen(@_audio_fd, FBaseChannel, @WF, DWORD(@WaveInProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
  Inc(FOpened);
end;

procedure TAudioIn.CloseAudio;
begin
  if FOpened = 1 then waveInClose(_audio_fd);
  if FOpened > 0 then Dec(FOpened);
end;

function TAudioIn.GetBPS;
begin
  Result := FBPS;
end;

function TAudioIn.GetCh;
begin
  Result := FChan;
end;

function TAudioIn.GetSR;
begin
  Result := FFreq;
end;

procedure TAudioIn.Init;
begin
  if Buisy then raise EACSException.Create('The component is buisy');
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  FRecBytes := FRecTime * (GetBPS div 8) * GetCh * GetSR;
  Buisy := True;
  OpenAudio;
  waveInStart(_audio_fd);
  BlockChain := nil;
  FSize := FRecBytes;
  BlocksCount := 0;
  EOC := @BlockChain;
end;

procedure TAudioIn.Flush;
var
  Tmp : PWaveHdr;
begin
  while BlocksCount > 0 do;  // wait until pending data blocks are put to the chain
  waveInReset(_audio_fd);    // return all pending data blocks
  sleep(10);
  Tmp := BlockChain;         // clear pending data blocks
  while Tmp <> nil do
  begin
    BlockChain := Tmp.lpNext;
    waveInUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
    Tmp := BlockChain;
  end;
  CloseAudio;
  Buisy := False;
end;

procedure TAudioIn.SetChannel;
begin
  if Buisy then raise EACSException.Create('Component is buisy');
  if Ch < InputChannelsCount then FBaseChannel := Ch
  else raise EACSException.Create('Channel ' + IntToStr(Ch) + ' is not available');
end;

function TAudioIn.GetData;
var
  Tmp : PWaveHdr;
begin
  if not Buisy then  raise EACSException.Create('The Stream is not opened');
  if FRecBytes >= 0 then
  if (FPosition >= FRecBytes) then
  begin
    Result := 0;
    Exit;
  end;
  while BlocksCount < BLOCKS_COUNT do
  NewBlock;
  if BufStart > BufEnd then
  begin
    BufStart := 1;
    while BlockChain = nil do sleep(10);
    TMP := BlockChain;
    BlockChain := BlockChain.lpNext;
    if BlockChain = nil then EOC := @BlockChain;
    Move(Tmp.lpData[0],  buf[1], Tmp.dwBytesRecorded);
    BufEnd := Tmp.dwBytesRecorded;
    waveInUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
  end;
  if BufferSize < (BufEnd - BufStart + 1)
  then Result := BufferSize
  else Result := BufEnd - BufStart + 1;
  Move(Buf[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure TAudioOut.WriteBlock;
var
  WH : PWaveHdr;
begin
  Inc(BlocksCount);
  New(WH);
  WH.lpData := P;
  WH.dwBufferLength := Len;
  WH.dwLoops := 0;
  WH.dwFlags := 0;
  waveOutPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  waveOutWrite(_audio_fd, WH, SizeOf(TWaveHdr));
end;

procedure TAudioIn.NewBlock;
var
  WH : PWaveHdr;
begin
  New(WH);
  GetMem(WH.lpData, INBUF_SIZE);
  WH.dwBufferLength := INBUF_SIZE;
  WH.dwFlags := 0;
  waveInPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  waveInAddBuffer(_audio_fd, WH, SizeOf(TWaveHdr));
  Inc(BlocksCount);
end;

function CountChannels : Integer;
begin
  OutputChannelsCount := waveOutGetNumDevs;
  InputChannelsCount := waveInGetNumDevs;
end;

function TAudioOut.GetDeviceInfo;
begin
  Result := GetAudioDeviceInfo(FBaseChannel, True);
end;

function TAudioIn.GetDeviceInfo;
begin
  Result := GetAudioDeviceInfo(FBaseChannel, False);
end;

function TAudioIn.GetTotalTime;
begin
  Result := RecTime;
end;

procedure TAudioIn.AddBlockToChain(WH : PWaveHdr);
begin
  WH.lpNext := nil;
  EOC^ := WH;
  EOC := @WH.lpNext;
  Dec(BlocksCount);
end;

initialization

  InitializeCriticalSection(CrSecI);
  InitializeCriticalSection(CrSecO);
  CountChannels;

finalization

  DeleteCriticalSection(CrSecI);
  DeleteCriticalSection(CrSecO);

end.
