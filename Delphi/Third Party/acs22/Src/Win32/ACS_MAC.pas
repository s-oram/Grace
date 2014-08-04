(*
  The original version of this file is written by Thomas la Cour,
  http://www.top-house.dk/~nr161/delphi/

  This file is a part of Audio Components Suite v 2.2
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_MAC;

interface

uses
  Classes, SysUtils, Windows, ACS_Classes, MACDll;

const
  OUT_BUF_SIZE = $10000;
  IN_BUF_SIZE = $2000;

type

  (* Note by A.B.: It seams that APE compressor supports file output only.
     So TMACOut inherits from TACSOutput, without stream support.  *)

  TMACOut = class(TACSFileOut)
  private
    buf: array[0..OUT_BUF_SIZE - 1] of Byte;
    APECompress: TAPECompress;
    WaveFormatEx: TWaveFormatEx;
    EndOfStream: Boolean;
    FCompressionLevel: Integer;
    FMaxAudioBytes: Integer;
    procedure SetCompressionLevel(Value: Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort: Boolean): Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CompressionLevel: LongInt read FCompressionLevel write SetCompressionLevel stored True;
    property MaxAudioBytes: Integer read FMaxAudioBytes write FMaxAudioBytes;
  end;

  (* Note by A.B.: Due to the reasons described above this component
     ignores streamed input *)

  TMACIn = class(TACSFileIn)
  private
    //buf: array[1..IN_BUF_SIZE] of Byte;
    buf: array[0..IN_BUF_SIZE - 1] of Byte;
    APEDecompress: TAPEDecompress;
    EndOfStream: Boolean;
    function GetAverageBitrate: Integer;
    function GetCurrentBitrate: Integer;
    function GetCurrentBlock: Integer;
    function GetCurrentMS: Integer;
    function GetLengthMS: Integer;
    function GetTotalBlocks: Integer;
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    function GetTotalTime: Integer; override;
    procedure OpenFile; override;
    procedure CloseFile; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;

    function Seek(Sample : Integer) : Boolean; override;

    procedure Flush; override;
    procedure Init; override;

    property AverageBitrate: Integer read GetAverageBitrate;
    property CurrentBitrate: Integer read GetCurrentBitrate;
    property CurrentBlock: Integer read GetCurrentBlock;
    property CurrentMS: Integer read GetCurrentMS;
    property LengthMS: Integer read GetLengthMS;
    property TotalBlocks: Integer read GetTotalBlocks;
  end;

implementation

constructor TMACOut.Create;
begin
  inherited Create(AOwner);
  FCompressionLevel := COMPRESSION_LEVEL_NORMAL;
  FMaxAudioBytes := MAX_AUDIO_BYTES_UNKNOWN;
  if not (csDesigning in ComponentState) then
  begin
    if not MACLoaded then
      raise EACSException.Create(MACPath + ' library could not be loaded.');
  end;
end;

destructor TMACOut.Destroy;
begin
  if Assigned(APECompress) then
    APECompress.Free;
  inherited Destroy;
end;

procedure TMACOut.Prepare;
var
  r: Integer;
begin
  if FFileName = '' then raise EACSException.Create('File name is not assigned.');
  FInput.Init;
  EndOfStream := False;

  APECompress := TAPECompress.Create;

  macFillWaveFormatEx(WaveFormatEx, FInput.SampleRate, FInput.BitsPerSample, FInput.Channels);

  r := APECompress.Start(
    PChar(FFileName),
    @WaveFormatEx,
    FMaxAudioBytes,
    FCompressionLevel,
    nil,
    CREATE_WAV_HEADER_ON_DECOMPRESSION);

  CanOutput := (r = 0);

  if r <> 0 then
  raise EACSException.Create('Error starting APECompress.' + #13#10 +
      macErrorExplanation(r));
end;

procedure TMACOut.Done;
begin
  APECompress.Finish(nil, 0, 0);
  APECompress.Free;
  APECompress := nil;
  FInput.Flush;
end;

function TMACOut.DoOutput;
var
  Len, i, x, z: Integer;
  pBuffer: PByteArray;
  nAudioBytesLeft, nBufferBytesAvailable, nNoiseBytes, nRetVal: Integer;
begin
    // No exceptions Here
  Result := True;
  if not CanOutput then Exit;
  if Progress <> CurProgr then
  begin
    CurProgr := Progress;
    if Assigned(FOnProgress) then FOnProgress(Self);
  end;
  if Abort or EndOfStream then
  begin
      (* We don't close file here to avoide exceptions
        if output componenet's Stop method is called *)
    Result := False;
    Exit;
  end;
  Len := Finput.GetData(@buf[0], OUT_BUF_SIZE);
  x := 0;
  if Len <> 0 then
  begin
    nAudioBytesLeft := Len;
    while (nAudioBytesLeft > 0) do
    begin
      nBufferBytesAvailable := 0;
      pBuffer := APECompress.LockBuffer(nBufferBytesAvailable);

      nNoiseBytes := nBufferBytesAvailable;
      if nNoiseBytes > nAudioBytesLeft then
        nNoiseBytes := nAudioBytesLeft;

      for z := 0 to nNoiseBytes - 1 do
      begin
        pBuffer[z] := buf[x];
        inc(x);
      end;

      nRetVal := APECompress.UnlockBuffer(nNoiseBytes, TRUE);
      if (nRetVal <> 0) then
        raise EACSException.Create('APECompress.UnlockBuffer Error: ' + inttostr(nRetVal));

      dec(nAudioBytesLeft, nNoiseBytes);
    end
  end
  else
    EndOfStream := True;
end;


constructor TMACIn.Create;
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    if not MACLoaded then
      raise EACSException.Create(MACPath + ' library could not be loaded.');
  end;
end;

destructor TMACIn.Destroy;
begin
  if Assigned(APEDecompress) then
    APEDecompress.Free;
  inherited Destroy;
end;

procedure TMACIn.OpenFile;
begin
  FValid := True;
  if FOpened = 0 then
  begin
    EndOfStream := False;

    APEDecompress := TAPEDecompress.Create(FileName);
    if APEDecompress.Handle <> 0 then
    begin
      FSize := APEDecompress.InfoWavTotalBytes;
      FSR := APEDecompress.InfoSampleRate;
      FBPS := APEDecompress.InfoBitsPerSample;
      FChan := APEDecompress.InfoChannels;
      FTime := APEDecompress.InfoLengthMS div 1000; // Round(ov_time_total(VFile, 0));
      FTotalSamples := (FSize div (FBPS shr 3)) div FChan;
    end
    else
    begin
      FValid := False;
      FOpened := -1;
    end;
  end;
  Inc(FOpened);
end;

procedure TMACIn.CloseFile;
begin
  if FOpened = 1 then
  begin
    if Assigned(APEDecompress) then
      APEDecompress.Free;
    APEDecompress := nil;
  end;
  if FOpened > 0 then Dec(FOpened);
end;

function TMACIn.GetData;
var
  l, csize, offs: Integer;
  blocks: Integer;
  tmp: Double;
begin
  if not Buisy then raise EACSException.Create('The Stream is not opened');
  if BufStart > BufEnd then
  begin
    if FOffset <> 0 then
    begin
      offs := Round((FOffset / 100) * FSize);
      FPosition := FPosition + offs;
      if FPosition < 0 then FPosition := 0
      else if FPosition > FSize then FPosition := FSize;
      APEDecompress.Seek(FPosition shr 2);
      FOffset := 0;
    end;
    BufStart := 1;
    BufEnd := 0;
    if not EndOfStream then
    begin
      while BufEnd < IN_BUF_SIZE do
      begin
        //l := ov_read(VFile, @buf[BufEnd + 1], BUF_SIZE - BufEnd, 0, 2, 1, @cursec);
        blocks := (IN_BUF_SIZE - BufEnd) div 4;
        APEDecompress.GetData(@buf[BufEnd], blocks, l);
        l := l * 4;
        if l <= 0 then
        begin
          EndOfStream := True;
          Break;
        end;
        Inc(BufEnd, l);
        if (FEndSample <> -1) then
        begin
          csize := (FEndSample-FStartSample)*(FBPS shr 3)*FChan;
          if (csize - FPosition) <= 0 then
          begin
            EndOfStream := True;
            Break;
          end;
          if (csize - FPosition) < BufEnd then
          begin
            BufEnd := csize - FPosition;
            Break;
          end;
        end;
      end;
    end;
    if EndOfStream and FLoop then
    begin
      Flush;
      Init;
      EndOfStream := False;
      while BufEnd < IN_BUF_SIZE do
      begin
        //l := ov_read(VFile, @buf[BufEnd + 1], BUF_SIZE - BufEnd, 0, 2, 1, @cursec);
        blocks := (IN_BUF_SIZE - BufEnd) div 4;
        APEDecompress.GetData(@buf[BufEnd], blocks, l);
        l := l * 4;
        if l <= 0 then
        begin
          EndOfStream := True;
          Break;
        end;
        Inc(BufEnd, l);
      end;
    end;
  end;
  if BufferSize < (BufEnd - BufStart + 1) then
    Result := BufferSize
  else
    Result := BufEnd - BufStart + 1;
  Move(buf[BufStart - 1], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

function TMACIn.GetTotalTime: Integer;
begin
  OpenFile;
  if Assigned(APEDecompress) then
    Result := APEDecompress.LengthMS div 1000;
  CloseFile;  
end;

function TMACIn.GetAverageBitrate: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.AverageBitrate;
end;

function TMACIn.GetCurrentBitrate: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentBitrate;
end;

function TMACIn.GetCurrentBlock: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentBlock;
end;

function TMACIn.GetCurrentMS: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentMS;
end;

function TMACIn.GetLengthMS: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.LengthMS;
end;

function TMACIn.GetTotalBlocks: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.TotalBlocks;
end;

function TMACIn.GetBPS: Integer;
begin
  OpenFile;
  Result := FBPS;
  CloseFile;
end;

function TMACIn.GetCh: Integer;
begin
  OpenFile;
  Result := FChan;
  CloseFile;
end;

function TMACIn.GetSR: Integer;
begin
  OpenFile;
  Result := FSR;
  CloseFile;
end;

procedure TMACOut.SetCompressionLevel(Value: Integer);
begin
  case Value of
    COMPRESSION_LEVEL_FAST,
      COMPRESSION_LEVEL_NORMAL,
      COMPRESSION_LEVEL_HIGH,
      COMPRESSION_LEVEL_EXTRA_HIGH: FCompressionLevel := Value;
  else
    FCompressionLevel := COMPRESSION_LEVEL_NORMAL;
  end;
end;

procedure TMACIn.Flush;
begin
  inherited Flush;
end;

procedure TMACIn.Init;
begin
  inherited Init;
  BufStart := 1;
  BufEnd := 0;
end;

function TMACIn.Seek;
begin
  Result := False;
  if not FSeekable then Exit;
  Result := True;
  OpenFile;
  APEDecompress.Seek(Sample);
  CloseFile;
end;

end.

