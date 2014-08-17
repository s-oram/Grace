(*
  This file is a part of New Audio Components package v 1.4
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  The original version of this file is written by Thomas la Cour.

  Updated by Sergei Borisov < jr_ross@mail.ru >

*)

(* $Id: ACS_MAC.pas 1126 2010-01-23 17:52:50Z andrei.borovsky $ *)

unit ACS_MAC;

(* Title: ACS_MAC
    Delphi interface for Monkey's Audio (.ape) files via MACDll.dll *)

interface

uses
  Classes, SysUtils, Windows, ACS_Classes, ACS_Tags, MACDll;

const

  mclFast = 1000;
  mclNormal = 2000;
  mclHigh = 3000;
  mclExtraHigh = 4000;
  mclInsane = 5000;

  OUT_BUF_SIZE = $10000;
  IN_BUF_SIZE = $2000;

type

  // Note by A.B.: It seems that APE compressor supports file output only.

  (* Class: TMACOut
     Monkey's Audio (APE) encoder.
     Descends from <TAuTaggedFileOut>.
     Requires MACDll.dll. *)

  TMACOut = class(TAuTaggedFileOut)
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
   (* Property: APEv2Tags
       Use this property to add APE v. 2 tags to the file *)
    property APEv2Tags;
    (* Property: CompressionLevel
       Use this property to set the compression level for the APE file being created.
       The pssible values are 1000 (fastest time, lowest compression rate), 2000 (the default), 3000, 4000 (slow comression performance very high compression rate), 5000 (very slow performance, maximum compression). *)
    property CompressionLevel: LongInt read FCompressionLevel write SetCompressionLevel stored True;
    (* Property: MaxAudioBytes
       Use this property to set the absolute maximum audio bytes that will be encoded.
       If this number is unknown, set it to -1 (the default value).*)
    property MaxAudioBytes: Integer read FMaxAudioBytes write FMaxAudioBytes;
  end;

  (* Note by A.B.: Due to the reasons described above this component
     ignores streamed input *)

  (* Class: TMACIn
     Monkey's Audio (APE) decoder.
     Descends from <TAuTaggedFileIn>.
     Requires MACDll.dll. *)

  TMACIn = class(TAuTaggedFileIn)
  private
    //buf: array[1..IN_BUF_SIZE] of Byte;
    buf: array[0..IN_BUF_SIZE - 1] of Byte;
    APEDecompress: TAPEDecompress;
    function GetAverageBitrate: Integer;
    function GetCurrentBitrate: Integer;
    function GetCurrentBlock: Integer;
    function GetCurrentMS: Integer;
    function GetLengthMS: Integer;
    function GetTotalBlocks: Integer;
    function GetAPEv2Tags : TAPEv2Tags;
  protected
    function GetBPS: LongWord; override;
    function GetCh: LongWord; override;
    function GetSR: LongWord; override;
    function GetTotalTime: LongWord; override;
    procedure OpenFile; override;
    procedure CloseFile; override;
    function SeekInternal(var Sample : Int64) : Boolean; override;
    procedure FlushInternal; override;
    procedure InitInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes : LongWord); override;
    (* Property: APEv2Tags
       Use this property to read APE v. 2 tags from the file *)
     property APEv2Tags : TAPEv2Tags read GetAPEv2Tags;
    (* Property: AverageBitrate
       This property shows the average bitrate for the ape file being played.*)
    property AverageBitrate: Integer read GetAverageBitrate;
    (* Property: CurrentBitrate
       This property shows the current bitrate for the ape file being played.*)
    property CurrentBitrate: Integer read GetCurrentBitrate;
    (* Property: CurrentBlock
       Read this property to get the number of block being read from file.*)
    property CurrentBlock: Integer read GetCurrentBlock;
    (* Property: CurrentMS
       This property shows how many milliseconds have passed since the beginning of the file.*)
    property CurrentMS: Integer read GetCurrentMS;
    (* Property: LengthMS
       Read this property to get the length of the file in milliseconds.*)
    property LengthMS: Integer read GetLengthMS;
    (* Property: TotalBlocks
       The total number of blocks in the file.*)
    property TotalBlocks: Integer read GetTotalBlocks;
  published
    property EndSample;
    property StartSample;
  end;

implementation

constructor TMACOut.Create;
begin
  inherited Create(AOwner);
  FCompressionLevel := COMPRESSION_LEVEL_NORMAL;
  FMaxAudioBytes := MAX_AUDIO_BYTES_UNKNOWN;
end;

destructor TMACOut.Destroy;
begin
  if Assigned(APECompress) then
    APECompress.Free;
//  UnloadMACDll;
  inherited Destroy;
end;

procedure TMACOut.Prepare;
var
  r: Integer;
begin
  LoadMACDll;
  if not MACLoaded then
     raise EAuException.Create(MACPath + ' library could not be loaded.');
  if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
  FInput.Init;
  EndOfStream := False;

  APECompress := TAPECompress.Create;

  macFillWaveFormatEx(WaveFormatEx, FInput.SampleRate, FInput.BitsPerSample, FInput.Channels);

  r := APECompress.Start(
(* Ross---
    PChar(String((FWideFileName))),
 ---Ross *)
(* Ross--- *)
    PAnsiChar(AnsiString(FWideFileName)),
(* ---Ross *)
    @WaveFormatEx,
    FMaxAudioBytes,
    FCompressionLevel,
    nil,
    0);

  CanOutput := (r = 0);

  if r <> 0 then
  raise EAuException.Create('Error starting APECompress.' + #13#10 +
      macErrorExplanation(r));
end;

procedure TMACOut.Done;
begin
  APECompress.Finish(nil, 0, 0);
  APECompress.Free;
  APECompress := nil;
  FInput.Flush;
  if not APEv2Tags.Empty then
      macTagFileSimple(PAnsiChar(AnsiString(FWideFileName)), PAnsiChar(Utf8Encode(APEv2Tags.Artist)), PAnsiChar(Utf8Encode(APEv2Tags.Album)), PAnsiChar(Utf8Encode(APEv2Tags.Title)),
        PAnsiChar(Utf8Encode(APEv2Tags.Comment)), PAnsiChar(Utf8Encode(APEv2Tags.Genre)), PAnsiChar(Utf8Encode(APEv2Tags.Year)), PAnsiChar(Utf8Encode(APEv2Tags.Track)), True, False);
end;

function TMACOut.DoOutput;
var
  Aligned, BytesRead, Len, x, z, SampleSize: Integer;
  pBuffer: PByteArray;
  nAudioBytesLeft, nBufferBytesAvailable, nNoiseBytes, nRetVal: Integer;
begin
  Result := True;
  if not CanOutput then Exit;
  if Abort or EndOfStream then
  begin
    Result := False;
    Exit;
  end;
  SampleSize := FInput.Channels*(FInput.BitsPerSample shr 3);
  Aligned := OUT_BUF_SIZE - (OUT_BUF_SIZE mod SampleSize);
  BytesRead := FInput.CopyData(@buf[0], Aligned);
  Len := BytesRead;
  while (BytesRead > 0) and (Len < Aligned) do
  begin
//    Dec(Aligned, BytesRead);
    BytesRead := FInput.CopyData(@buf[Len], Aligned - Len);
    Inc(Len, BytesRead);
  end;
  if BytesRead = 0 then EndOfStream := True;
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
        raise EAuException.Create('APECompress.UnlockBuffer Error: ' + inttostr(nRetVal));

      dec(nAudioBytesLeft, nNoiseBytes);
    end
  end
//  else
//    EndOfStream := True;
end;


constructor TMACIn.Create;
begin
  inherited Create(AOwner);
end;

destructor TMACIn.Destroy;
begin
//  if Assigned(APEDecompress) then
//    APEDecompress.Free;
//  UnloadMACDll;
  inherited Destroy;
end;

procedure TMACIn.OpenFile;
var
  Tag : ID3_TAG;
begin
  LoadMACDll;
    if not MACLoaded then
     raise EAuException.Create(MACPath + ' library could not be loaded.');
  OpenCS.Enter;
  try
  if FOpened = 0 then
  begin
     FValid := True;
     _APEv2Tags.Clear;
    if macGetID3Tag(PAnsiChar(AnsiString(FWideFileName)), @Tag) = 0 then
    begin
      _CommonTags.Clear;
      {$WARNINGS OFF}
      _APEv2Tags.Album := Tag.Album;
      _CommonTags.Album := Tag.Album;
      _APEv2Tags.Artist := Tag.Artist;
      _CommonTags.Artist := Tag.Artist;
      _APEv2Tags.Title := Tag.Title;
      _CommonTags.Title := Tag.Title;
//      _APEv2Tags.Genre := Tag.Genre;
      _CommonTags.Genre := '';
      _APEv2Tags.Year := Tag.Year;
      _CommonTags.Year := Tag.Year;
      _APEv2Tags.Comment := Tag.Comment;
      _APEv2Tags.Track := IntToStr(Tag.Track);
      _CommonTags.Track := _APEv2Tags.Track;
      {$WARNINGS ON}
    end;

{Ross---
    APEDecompress := TAPEDecompress.Create(FileName);
 ---Ross}
//Ross---
   // Fixed here broken by Wane.
    APEDecompress := TAPEDecompress.Create(FWideFileName);
// ---Ross
    if APEDecompress.Handle <> 0 then
    begin
      FSize := APEDecompress.InfoWavTotalBytes;
      FSR := APEDecompress.InfoSampleRate;
      FBPS := APEDecompress.InfoBitsPerSample;
      FChan := APEDecompress.InfoChannels;
    end
    else
    begin
      FValid := False;
      FOpened := -1;
    end;
    Inc(FOpened);
  end;
  finally
    OpenCS.Leave;
  end;
end;

procedure TMACIn.CloseFile;
begin
  OpenCS.Enter;
  try
  if FOpened > 0 then
  begin
    if Assigned(APEDecompress) then
      APEDecompress.Free;
    APEDecompress := nil;
    FOpened := 0;
  end;
  finally
    OpenCS.Leave;
  end;
end;

procedure TMACIn.GetDataInternal;
var
  l : Integer;
  blocks, Aligned : LongWord;
begin
  if not Busy then raise EAuException.Create('The Stream is not opened');
  if BufStart > BufEnd then
  begin
    Aligned := IN_BUF_SIZE - (IN_BUF_SIZE mod FSampleSize);
    BufStart := 1;
    BufEnd := 0;
    if not _EndOfStream then
    begin
      while BufEnd < Aligned do
      begin
        //l := ov_read(VFile, @buf[BufEnd + 1], BUF_SIZE - BufEnd, 0, 2, 1, @cursec);
        blocks := (IN_BUF_SIZE - BufEnd) div FSampleSize;
        APEDecompress.GetData(@buf[BufEnd], blocks, l);
        l := l * FSampleSize;
        if l <= 0 then
        begin
          _EndOfStream := True;
          Break;
        end;
        Inc(BufEnd, l);
      end;
    end;
  end;
  Bytes := Bytes - (Bytes mod FSampleSize);
  if Bytes > (BufEnd - BufStart + 1) then
    Bytes := BufEnd - BufStart + 1;
  Buffer := @buf[BufStart - 1];
  Inc(BufStart, Bytes);
end;

function TMACIn.GetTotalTime: LongWord;
begin
  OpenFile;
  if Assigned(APEDecompress) then
    Result := APEDecompress.LengthMS div 1000
  else Result := 0;  
  //CloseFile;
end;

function TMACIn.GetAverageBitrate: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.AverageBitrate
  else
  Result := 0;
end;

function TMACIn.GetCurrentBitrate: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentBitrate
   else
  Result := 0;

end;

function TMACIn.GetCurrentBlock: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentBlock
   else
  Result := 0;

end;

function TMACIn.GetCurrentMS: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentMS
   else
  Result := 0;

end;

function TMACIn.GetLengthMS: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.LengthMS
   else
  Result := 0;

end;

function TMACIn.GetTotalBlocks: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.TotalBlocks
   else
  Result := 0;
   
end;

function TMACIn.GetBPS: LongWord;
begin
  OpenFile;
  Result := FBPS;
  //CloseFile;
end;

function TMACIn.GetCh: LongWord;
begin
  OpenFile;
  Result := FChan;
  //CloseFile;
end;

function TMACIn.GetSR: LongWord;
begin
  OpenFile;
  Result := FSR;
  //CloseFile;
end;

procedure TMACOut.SetCompressionLevel(Value: Integer);
begin
  case Value of
    mclFast,
    mclNormal,
    mclHigh,
    mclExtraHigh,
    mclInsane : FCompressionLevel := Value;
  else
    FCompressionLevel := mclNormal;
  end;
end;

procedure TMACIn.FlushInternal;
begin
  inherited FlushInternal;
end;

procedure TMACIn.InitInternal;
begin
  inherited InitInternal;
  BufStart := 1;
  BufEnd := 0;
end;

function TMACIn.SeekInternal;
begin
  Result := True;
  APEDecompress.Seek(Sample);
end;

function TMACIn.GetAPEv2Tags;
begin
  OpenFile;
  Result := _APEv2Tags;
end;

end.

