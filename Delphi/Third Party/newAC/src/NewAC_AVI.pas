(*
  This file is a part of New Audio Components package 2.1
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: NewAC_AVI.pas 907 2009-09-04 01:44:54Z andrei.borovsky $ *)

(* Title: NewAC_AVI
    This Unit contains the TAVIIn component which extracts audio streams from AVI files. *)

unit NewAC_AVI;

interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ACS_Types, ActiveX, MMSystem, _MSACM;

type
  TStreamType = packed array[0..3] of Char;

const

  streamtypeAUDIO : TStreamType = ('a', 'u', 'd', 's');

  AVIERR_OK = $00000000;
  AVIERR_UNSUPPORTED = $80044065;
  AVIERR_BADFORMAT = $80044066;
  AVIERR_MEMORY = $80044067;
  AVIERR_INTERNAL = $80044068;
  AVIERR_BADFLAGS = $80044069;
  AVIERR_BADPARAM = $8004406A;
  AVIERR_BADSIZE = $8004406B;
  AVIERR_BADHANDLE = $8004406C;
  AVIERR_FILEREAD = $8004406D;
  AVIERR_FILEWRITE = $8004406E;
  AVIERR_FILEOPEN = $8004406F;
  AVIERR_COMPRESSOR = $80044070;
  AVIERR_NOCOMPRESSOR = $80044071;
  AVIERR_READONLY = $80044072;
  AVIERR_NODATA = $80044073;

type

  LONG = Longint;
  PVOID = Pointer;

  TAVIFileInfoW = record
    dwMaxBytesPerSec,           // max. transfer rate
    dwFlags,                     // the ever-present flags
    dwCaps,
    dwStreams,
    dwSuggestedBufferSize,

    dwWidth,
    dwHeight,

    dwScale,
    dwRate,           // dwRate / dwScale == samples/second
    dwLength,

    dwEditCount: DWORD;

    szFileType: array[0..63] of WideChar;
  end;
  PAVIFileInfoW = ^TAVIFileInfoW;

  TAVIStreamInfoW = record
    fccType,
    fccHandler,
    dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority,
    wLanguage: WORD;
    dwScale,
    dwRate, // dwRate / dwScale == samples/second
    dwStart,
    dwLength, // In units above...
    dwInitialFrames,
    dwSuggestedBufferSize,
    dwQuality,
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount : DWORD;
    dwFormatChangeCount : DWORD;
    szName:  array[0..63] of WideChar;
  end;

  IAVIStream = interface
  ['{00020021-0000-0000-C000-000000000046}']
    function Create(lParam1, lParam2: LPARAM): HResult; stdcall;
    function Info(var psi: TAVIStreamInfoW; lSize: LONG): HResult; stdcall;
    function FindSample(lPos, lFlags: LongWord): LongWord; stdcall;
    function ReadFormat(lPos: LONG; lpFormat: PVOID; var lpcbFormat: LONG): HResult; stdcall;
    function SetFormat(lPos: LONG; lpFormat: PVOID; lpcbFormat: LONG): HResult; stdcall;
    function Read(lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; var plBytes: LONG; var plSamples: LONG): HResult; stdcall;
    function Write(lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; dwFlags: DWORD; var plSampWritten: LONG; var plBytesWritten: LONG): HResult; stdcall;
    function Delete(lStart, lSamples: LONG): HResult; stdcall;
    function ReadData(fcc: DWORD; lp: PVOID; var lpcb: LONG): HResult; stdcall;
    function WriteData(fcc: DWORD; lp: PVOID; cb:  LONG): HResult; stdcall;
    function SetInfo(var lpInfo: TAVIStreamInfoW; cbInfo: LONG): HResult; stdcall;
  end;
  PAVIStream = ^IAVIStream;

  IAVIFile = interface(IUnknown)
  ['{00020020-0000-0000-C000-000000000046}']
    function Info(var pfi: TAVIFileInfoW; lSize: LONG): HResult; stdcall;
    function GetStream(var ppStream: IAVIStream; const fccType: TStreamType; lParam: LONG): HResult; stdcall;
    function CreateStream(var ppStream: PAVIStream; var pfi: TAVIFileInfoW): HResult; stdcall;
    function WriteData(ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; stdcall;
    function ReadData(ckid: DWORD; lpData: PVOID; var lpcbData: LONG): HResult; stdcall;
    function EndRecord: HResult; stdcall;
    function DeleteStream(fccType: DWORD; lParam: LONG): HResult; stdcall;
  end;
  PAVIFile = ^IAVIFile;


  (* Class: TAVIIn
    This component extracts audio streams from AVI files. The AVI format is
    actually a container which can contain video, audio, and text data in
    different formats. TAVIIn decodes audio data contained in an AVI file
    using audio codecs installed in the Windows system (these may be built in
    or third party Windows codecs). The component is able to decode any AVI
    file playable on the system.
    Descends from <TAuFileIn>.  *)

  TAVIIn = class(TAuFileIn)
  private
    AVIFile : IAVIFile;
    AVIStream : IAVIStream;
    sh : ACMSTREAMHEADER;
    _Buf : Pointer;
    _BufSize : Integer;
    _SampleSize : Word;
    _StartSample, _StartFrom : LongWord;
    FHasAudio : Boolean;
    InFormat, OutFormat : TWaveFormatExtensible;
    NeedsDecoding : Boolean;
    ACMStream : HACMSTREAM;
    OutBuf : PBuffer8;
    OutBufSize : Integer;
    Offset, BufEnd : Integer;
    FReportSize : Boolean;
    BytesToRead : Integer;
    TotalChunks : Integer;
    function GetHasAudio : Boolean;
    procedure ACMInit;
    procedure ACMDecode;
    procedure ACMDone;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
   (* Property: ReportSize
       Due to internal limitations the component cannot report the exact audio
       stream size when dealing with some compression formats. Setting
       ReportSize to False forces the component not to report the audio stream
       size at all. Set this property to False if you are saving AVI audio
       data as a .wav file. *)
    property ReportSize : Boolean read FReportSize write FReportSize;
    (* Property: HasAudio
       Read this property to determine if the input file has an audio stream.
       A False value indicates that either an audio stream is missing or the
       input file is corrupt. If HasAudio repots True and TAuFileIn.Valid
       reports False, it means that an audio stream was found in the input
       file but the system couldn't find a codec to decode the audio
       compression format. You should proceed with the file only if both
       TAuFileIn.Valid and HasAudio return True.
    *)
    property HasAudio : Boolean read GetHasAudio;
  published
    property EndSample;
    property StartSample;
  end;


implementation

  procedure AVIFileInit; stdcall; external 'avifil32.dll' name 'AVIFileInit';
  procedure AVIFileExit; stdcall; external 'avifil32.dll' name 'AVIFileExit';
  function AVIFileOpen(var ppfile: IAVIFILE; szFile: PWideChar; uMode: LongWord; lpHandler: PCLSID): HResult; stdcall; external 'avifil32.dll' name 'AVIFileOpenW';
  function AVIFileRelease(pfile: IAVIFile): LongWord; stdcall external 'avifil32.dll' name 'AVIFileRelease';

  constructor TAVIIn.Create;
  begin
    inherited Create(AOwner);
    FReportSize := True;
  end;

  destructor TAVIIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TAVIIn.OpenFile;
  var
    wf : TWaveFormatExtensible;
    wfl : Integer;
    StreamInfo : TAVIStreamInfoW;
    fs : LongWord;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      if FWideFileName = '' then
        raise EAuException.Create('File name is not assigned');
      AVIFileInit;
      if AVIFileOpen(AVIFile, PWideChar(FWideFileName), OF_READ or OF_SHARE_DENY_WRITE, nil) <> AVIERR_OK then
        raise EAuException.Create('Failed to open input file');
      AVIFile._AddRef;
      if AVIFile.GetStream(AVIStream, streamtypeAUDIO, 0) = AVIERR_OK then
        FHasAudio := True
      else
      begin
        FHasAudio := False;
        Exit;
      end;
      AVIStream.Info(StreamInfo, SizeOf(StreamInfo));
      _StartSample := StreamInfo.dwStart;
      _StartFrom := _StartSample;
      wfl := SizeOf(wf);
      AVIStream.ReadFormat(0, @wf, wfl);
      if wf.Format.wFormatTag <> 1 then
      begin
        InFormat := wf;
        ACMInit;
        wf := OutFormat;
        acmStreamSize(ACMStream, StreamInfo.dwLength*StreamInfo.dwSampleSize, fs, ACM_STREAMSIZEF_SOURCE);
        FSize := fs;
        TotalChunks := StreamInfo.dwLength;
        BytesToRead := StreamInfo.dwSuggestedBufferSize div StreamInfo.dwSampleSize;
        if BytesToRead = 0 then BytesToRead := 1;
        NeedsDecoding := True;
      end else
      begin
        FSize := StreamInfo.dwSampleSize * StreamInfo.dwLength;
        NeedsDecoding := False;
      end;
      FValid := True;
      FChan := wf.Format.nChannels;
      FBPS := wf.Format.wBitsPerSample;
      FSR := wf.Format.nSamplesPerSec;
      _SampleSize := (FBPS div 8) * wf.Format.nChannels;
      FTotalSamples := FSize div _SampleSize;
      FSeekable := True;
      Inc(FOpened);
      _Buf := nil;
      _BufSize := 0;
      if not FReportSize then
      begin
        FSize := -1;
        FSeekable := False;
      end;  
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAVIIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      if NeedsDecoding then
        ACMDone;
      AVIStream := nil;
      AVIFileRelease(AVIFile);
      AVIFile := nil;
      AVIFileExit;
      if _Buf <> nil then FreeMem(_Buf);
      FOpened := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAVIIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  var
    br, sr : Integer;
  begin
    if not NeedsDecoding then
    begin
      if (FSize > 0) and (FSize - FPosition < Bytes) then
        Bytes := FSize - FPosition;
      Bytes := Bytes - (Bytes mod _SampleSize);
      if Bytes > LongWord(_BufSize) then
      begin
        if _Buf <> nil then FreeMem(_Buf);
        GetMem(_Buf, Bytes);
        _BufSize := Bytes;
      end;
      AVIStream.Read(_StartSample, Bytes div _SampleSize, _Buf, Integer(Bytes), br, sr);
      Inc(_StartSample, Bytes div _SampleSize);
      Buffer := _Buf;
    end else
    begin
      if Offset >= BufEnd then
        ACMDecode;
      if _EndOfStream then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;  
      if Bytes > LongWord(BufEnd - Offset) then
         Bytes := BufEnd - Offset;
      Buffer := @OutBuf[Offset];
      Inc(Offset, Bytes);
    end;
  end;

  function TAVIIn.SeekInternal(var SampleNum : Int64) : Boolean;
  begin
    Result := False;
    if Busy then
    begin
      if not NeedsDecoding then
        _StartSample := SampleNum + _StartFrom
      else
        _StartSample := Round (TotalChunks*SampleNum/FSize);
      Result := True;
    end;
  end;

  function TAVIIn.GetHasAudio;
  begin
    OpenFile;
    Result := Self.FHasAudio;
  end;

  procedure TAVIIn.ACMInit;
  begin
    FillChar(OutFormat, 0, SizeOf(OutFormat));
    OutFormat.Format.wFormatTag := 1;
    if acmFormatSuggest(nil, InFormat.Format, OutFormat.Format, SizeOf(OutFormat), ACM_FORMATSUGGESTF_WFORMATTAG) <> 0 then
      raise EAuException.Create('Cannot set up decoder');
    if acmStreamOpen(ACMStream, nil, InFormat.Format, OutFormat.Format, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME)  <> 0 then
      raise EAuException.Create('Cannot set up decoder');
    _Buf := nil;
    _BufSize := 0;
    OutBuf := nil;
    OutBufSize := 0;
    Offset := 0;
    BufEnd := 0;
  end;

  procedure TAVIIn.ACMDecode;
  var
    br, sr, res : Integer;
  begin
    res := AVIStream.Read(_StartSample, BytesToRead, nil, 0, br, sr);
    if (res <> 0) and (_StartSample = 0) then
      raise EAuException.Create('Error reading from AVI file: ' + IntToHex(res, 8));
    if sr = 0  then
    begin
       sr :=1;
       br := 2048;
    end;
    if br > _BufSize then
    begin
      if _Buf <> nil then FreeMem(_Buf);
      GetMem(_Buf, br);
      _BufSize := br;
    end;
    AVIStream.Read(_StartSample, sr, _Buf, br, br, sr);
    if sr = 0 then
    begin
      _EndOfStream := True;
      Exit;
    end;
    Inc(_StartSample, sr);
    acmStreamSize(ACMStream, br, LongWord(BufEnd), ACM_STREAMSIZEF_SOURCE);
    if BufEnd > OutBufSize then
    begin
      if OutBuf <> nil then
      begin
        acmStreamUnprepareHeader(ACMStream, sh, 0);
        FreeMem(OutBuf);
      end;
      GetMem(OutBuf, BufEnd);
      OutBufSize := BufEnd;
      sh.cbStruct := SizeOf(sh);
      sh.fdwStatus := 0;
      sh.pbSrc := _Buf;
      sh.cbSrcLength := br;
      sh.pbDst := PByte(OutBuf);
      sh.cbDstLength := OutBufSize;
      res := acmStreamPrepareHeader(ACMStream, sh, 0);
      if res <> 0 then
        raise EAuException.Create('Decoding failed : ' + IntToStr(res));
    end;
    res := acmStreamConvert(ACMStream, sh, ACM_STREAMCONVERTF_BLOCKALIGN {or ACM_STREAMCONVERTF_START});
    if res <> 0 then
      raise EAuException.Create('Decoding failed : ' + IntToStr(res));
    BufEnd := sh.cbDstLengthUsed;
    if BufEnd = 0 then EAuException.Create('Decoding failed');  // Reason unknown.
    Offset := 0;
  end;

  procedure TAVIIn.ACMDone;
  begin
    acmStreamUnprepareHeader(ACMStream, sh, 0);
    acmStreamClose(ACMStream, 0);
    if _Buf <> nil then FreeMem(_Buf);
    _Buf := nil;
    if OutBuf <> nil then FreeMem(OutBuf);
  end;

end.
