(*
  This file is a part of New Audio Components package v. 1.5
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: libwma1.pas 1259 2014-03-13 07:55:46Z sleuthhound@gmail.com $ *)

unit libwma1;

(*  This Delphi unit provides a simple C-style interface for reading and writing WMA files.
    (c) 2007-2008 Andrei Borovsky *)

{$I NewAC.inc}

interface

uses
  Windows, FastMove, Classes, SysUtils, ActiveX, MMSystem, wmfintf, ACS_Classes, ACS_Types, ACS_Procs, SyncObjs;

type
                              
   TMediaType = (mtV8, mtV9, mtLossless);

   QWORD = Int64;

   TNotifyProc = procedure(Dest : TComponent);
   TErrorNotifyProc = procedure(Dest : TComponent; Reason : LongWord);

   TAudioStream = class(TInterfacedObject, IStream)
   private
     FStream : TStream;
   public
     procedure AssignStream(Stream : TStream); stdcall;
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
   end;

   TSampleInfo = record
     Data : PBuffer8;
     Length : LongWord;
     Offset : LongWord;
   end;
   PSampleInfo = ^TSampleInfo;

   wma_async_reader = record
     status : TWMTStatus;
     reader : IWMReader;
     MaxWaitMilliseconds : LongWord;
     BufferingTime : LongWord;
     StretchFactor : Single;
     EnableTCP : Boolean;
     EnableHTTP : Boolean;
     EnableUDP : Boolean;
     BlockList : TList;
     WMReaderCallback : IWMReaderCallback;
     Event : TEvent;
     CriticalSection : TCriticalSection;
     HeaderInfo : IWMHeaderInfo;
     TimedOut : Boolean;
     Paused : Boolean;
     output : LongWord;
     has_audio : Boolean;
     channels : Integer;
     SampleRate :  Integer;
     BitsPerSample : Integer;
     Bitrate : LongWord;
     duration : QWORD;
   end;
   pwma_async_reader = ^wma_async_reader;

   TWMReaderCallback = class(TInterfacedObject, IWMReaderCallback)
   private
      FReader : pwma_async_reader;
   public
      constructor Create(reader : pwma_async_reader);
      function OnStatus(Status: TWMTStatus; hr: HRESULT; dwType: TWMTAttrDataType;
        pValue: PBYTE; pvContext: Pointer): HRESULT; stdcall;
      function OnSample(dwOutputNum: LongWord; cnsSampleTime, cnsSampleDuration: Int64;
        dwFlags: LongWord; pSample: INSSBuffer; pvContext: Pointer): HRESULT; stdcall;
   end;

   wma_sync_reader = record
     AudioStream : TAudioStream;
     reader : IWMSyncReader;
     buffer : INSSBuffer;
     HeaderInfo : IWMHeaderInfo;
     offset : LongWord;
     AuStream : IStream;
     stream : Word;
     output : LongWord;
     has_audio : Boolean;
     _protected : Boolean;
     channels : Integer;
     SampleRate :  Integer;
     BitsPerSample : Integer;
     duration : QWORD;
   end;
   pwma_sync_reader = ^wma_sync_reader;

   wma_writer = record
     Owner : TComponent;
     writer : IWMWriter;
     buffer : INSSBuffer;
     input : LongWord;
     BytesPerSecond : LongWord;
     TotalTime : QWORD;
     Initialized : Boolean;
     file_sink : IWMWriterFileSink;
     network_sink : IWMWriterNetworkSink;
     StatusCallback : IWMStatusCallback;
     ErrorCallback : IWMStatusCallback;
     OnConnected, OnDisconnected : TNotifyProc;
     OnError : TErrorNotifyProc;
   end;

   pwma_writer = ^wma_writer;

   wma_preprocessor = record
     WMWriterPreprocess : IWMWriterPreprocess;
   end;

   pwma_preprocessor = ^wma_preprocessor;


   TWMStatusCallback = class(TInterfacedObject, IWMStatusCallback)
   private
      FWriter : pwma_writer;
   public
      constructor Create(writer : pwma_writer);
      function OnStatus(Status: TWMTStatus; hr: HRESULT; dwType: TWMTAttrDataType;
        pValue: PBYTE; pvContext: Pointer): HRESULT; stdcall;
   end;

   TWMErrorCallback = class(TInterfacedObject, IWMStatusCallback)
   private
      FWriter : pwma_writer;
   public
      constructor Create(writer : pwma_writer);
      function OnStatus(Status: TWMTStatus; hr: HRESULT; dwType: TWMTAttrDataType;
        pValue: PBYTE; pvContext: Pointer): HRESULT; stdcall;
   end;


 const
   VOICE_AUDIO = 1;
   AUDIO_64 = 2;
   AUDIO_96 = 3;
   AUDIO_128 = 4;


   procedure lwma_enumerate_codecs(var Codecs : TStringList);
   procedure lwma_enumerate_codec_formats(CodecIndex : LongWord; VBR : LongBool; var Formats : TStringList);
   procedure lwma_enumerate_codec_formats2(CodecIndex : LongWord; VBR : LongBool; var Formats : TStringList);

   procedure lwma_reader_init(var sync_reader : wma_sync_reader; Stream : TStream; Descrete : Boolean; Speakers : ShortInt);
//   function lwma_reader_has_audio(reader : Pointer) : Byte;
//   function lwma_reader_is_protected(reader : Pointer) : Byte;
   function lwma_reader_get_duration(var sync_reader : wma_sync_reader) : LongWord;
   function lwma_reader_get_bitrate(var sync_reader : wma_sync_reader) : LongWord;

   procedure lwma_reader_init1(var sync_reader : wma_sync_reader; Stream : TStream);
   function lwma_reader_get_format_count(var sync_reader : wma_sync_reader; Descrete : Boolean) : LongWord;
   procedure lwma_reader_get_format(var sync_reader : wma_sync_reader; Descrete : Boolean; FormatIndex : LongWord; var channels, bps, samplerate : LongWord);
   procedure lwma_reader_set_format(var sync_reader : wma_sync_reader; Descrete : Boolean; FormatIndex : LongWord);

   function lwma_reader_get_is_vbr(var sync_reader : wma_sync_reader) : LongBool;
   function lwma_reader_get_author(var sync_reader : wma_sync_reader) : WideString;
   function lwma_reader_get_title(var sync_reader : wma_sync_reader) : WideString;
   function lwma_reader_get_album(var sync_reader : wma_sync_reader) : WideString;
   function lwma_reader_get_genre(var sync_reader : wma_sync_reader) : WideString;
   function lwma_reader_get_track(var sync_reader : wma_sync_reader) : WideString;
   function lwma_reader_get_year(var sync_reader : wma_sync_reader) : WideString;
   function lwma_reader_get_copyright(var sync_reader : wma_sync_reader) : WideString;
   procedure lwma_reader_get_audio_properties(var sync_reader : wma_sync_reader; var channels, BitsPerSample : Word; var SampleRate : LongWord);
   procedure lwma_reader_seek(var sync_reader : wma_sync_reader; start_from : LongWord);
   procedure lwma_reader_get_data(var sync_reader : wma_sync_reader; var buffer : Pointer; var bufsize : LongWord);
   procedure lwma_reader_free(var sync_reader : wma_sync_reader);

   procedure lwma_async_reader_init(var async_reader : wma_async_reader);
   procedure lwma_async_reader_open(var async_reader : wma_async_reader; const URL : WideString);
   procedure lwma_async_reader_add_logging_url(var async_reader : wma_async_reader; const URL : WideString);
   procedure lwma_async_reader_clear_logging_urls(var async_reader : wma_async_reader);
   procedure lwma_async_reader_pause(var async_reader : wma_async_reader);
   procedure lwma_async_reader_resume(var async_reader : wma_async_reader);
   procedure lwma_async_reader_set_proxy(var async_reader : wma_async_reader; const Protocol, Host : WideString; Port : LongWord);
   procedure lwma_async_reader_reset_stretch(var async_reader : wma_async_reader; new_stretch : Single);
   function lwma_async_reader_get_author(var async_reader : wma_async_reader) : WideString;
   function lwma_async_reader_get_title(var async_reader : wma_async_reader) : WideString;
   function lwma_async_reader_get_album(var async_reader : wma_async_reader) : WideString;
   function lwma_async_reader_get_genre(var async_reader : wma_async_reader) : WideString;
   function lwma_async_reader_get_track(var async_reader : wma_async_reader) : WideString;
   function lwma_async_reader_get_year(var async_reader : wma_async_reader) : WideString;
   function lwma_async_reader_get_copyright(var async_reader : wma_async_reader) : WideString;
   procedure lwma_async_reader_get_data(var async_reader : wma_async_reader; var buffer : Pointer; var bufsize : LongWord);
   procedure lwma_async_reader_free(var async_reader : wma_async_reader);

  procedure lwma_writer_init(var writer : wma_writer; pwszFilename : PWChar; Owner : TComponent; CallError : TErrorNotifyProc);
  procedure lwma_writer_set_audio_properties(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; Lossless, vbr : Boolean; DesiredBitrate : LongWord);
  procedure lwma_writer_set_audio_properties2(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; VBR : LongBool; CodecIndex, FormatIndex : LongWord);
  procedure lwma_writer_set_audio_properties3(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; VBR : LongBool; CodecIndex, FormatIndex : LongWord);
  procedure lwma_writer_set_author(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_title(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_album(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_genre(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_year(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_copyright(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_track(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_begin(var writer : wma_writer);
  procedure lwma_create_preprocessor(var writer : wma_writer; var preprocessor : wma_preprocessor);
  procedure lwma_writer_begin_preprocess(var writer : wma_writer; var preprocessor : wma_preprocessor);
  procedure lwma_writer_preprocess(var writer : wma_writer; var preprocessor : wma_preprocessor; Buffer : Pointer; len : LongWord);
  procedure lwma_preprocessor_free(var writer : wma_writer; var preprocessor : wma_preprocessor);
  procedure lwma_writer_write(var writer : wma_writer; Buffer : Pointer; len : LongWord);
  procedure lwma_writer_free(var writer : wma_writer);

  procedure lwma_network_writer_init(var writer : wma_writer; PortNumber, MaxClients : LongWord; Owner : TComponent; CallConnected, CallDisconnected : TNotifyProc; CallError : TErrorNotifyProc);
  procedure lwma_network_writer_free(var writer : wma_writer);
  function lwma_network_writer_get_connections_count(var writer : wma_writer) : LongWord;
  procedure lwma_network_writer_get_connection_info(var writer : wma_writer; connection : LongWord; var Address, Port : LongWord);
  function lwm_network_writer_get_url(var writer : wma_writer) : WideString;

  procedure lwma_get_codec_names(var SL : TStringList);
  procedure create_configs(var List : TList);

implementation

   procedure lwma_async_reader_init(var async_reader : wma_async_reader);
   begin
     FillChar(async_reader, SizeOf(async_reader), 0);
     async_reader.MaxWaitMilliseconds := 10000;
     async_reader.StretchFactor := 1.0;
     CoInitialize(nil);
     if WMCreateReader(nil, 1, async_reader.reader) <> S_OK then
     Exit;
   end;

   procedure lwma_async_reader_open(var async_reader : wma_async_reader; const URL : WideString);
   var
     WMReaderCallback : TWMReaderCallback;
     OutputCount : LongWord;
     i, size : LongWord;
     MediaProps : IWMOutputMediaProps;
     MediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     datatype : WMT_ATTR_DATATYPE;
     len, astream : Word;
     res : HResult;
     NetworkConfig : IWMReaderNetworkConfig;
   begin
     NetworkConfig := async_reader.reader as IWMReaderNetworkConfig;
     if async_reader.EnableTCP then
       NetworkConfig.SetEnableTCP(True);
     if async_reader.EnableHTTP then
       NetworkConfig.SetEnableHTTP(True);
     if async_reader.EnableUDP then
       NetworkConfig.SetEnableUDP(True);
     if async_reader.BufferingTime <> 0 then
       if NetworkConfig.SetBufferingTime(async_reader.BufferingTime) <> S_OK then
         raise EAuException.Create('Failed seting buffering.');
     async_reader.Event := TEvent.Create(nil, False, False, '');
     async_reader.CriticalSection := TCriticalSection.Create;
     WMReaderCallback := TWMReaderCallback.Create(@async_reader);
     async_reader.WMReaderCallback := WMReaderCallback as IWMReaderCallback;
     res := async_reader.reader.Open(PWideChar(URL), async_reader.WMReaderCallback, nil);
     if res <> S_OK then
     begin
       raise EAuException.Create('Result ' + IntToStr(res));
       async_reader.reader := nil;
       async_reader.WMReaderCallback := nil;
       async_reader.Event.Free;
       async_reader.CriticalSection.Free;
       Exit;
     end;
     async_reader.Event.WaitFor(async_reader.MaxWaitMilliseconds);
     if async_reader.status <> WMT_OPENED then
     begin
       async_reader.reader := nil;
       async_reader.WMReaderCallback := nil;
       async_reader.Event.Free;
       async_reader.CriticalSection.Free;
       async_reader.TimedOut := True;
       Exit;
     end;
     if async_reader.reader.GetOutputCount(OutputCount) <> S_OK then
     begin
       async_reader.reader := nil;
       async_reader.WMReaderCallback := nil;
       async_reader.Event.Free;
       async_reader.CriticalSection.Free;
       Exit;
     end;
     if OutputCount > 0 then
     for i := 0 to OutputCount - 1 do
     begin
       async_reader.reader.GetOutputProps(i, MediaProps);
       MediaProps.GetMediaType(nil, size);
       GetMem(MediaType, size);
       MediaProps.GetMediaType(MediaType, size);
       if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
       begin
         async_reader.has_audio := True;
      	 async_reader.output := i;
      	 format := PWAVEFORMATEX(MediaType.pbFormat);
      	 async_reader.channels := format.nChannels;
      	 async_reader.SampleRate := format.nSamplesPerSec;
      	 async_reader.BitsPerSample := format.wBitsPerSample;
       end;
       FreeMem(MediaType);
       MediaProps := nil;
     end;
     if async_reader.has_audio then
     begin
       len := 8;
       astream := 0;
       async_reader.reader.QueryInterface(IID_IWMHeaderInfo, async_reader.HeaderInfo);
       if async_reader.HeaderInfo.GetAttributeByName(astream, g_wszWMDuration, datatype, PByte(@(async_reader.duration)), len) <> S_OK then
         async_reader.duration := 0
       else
         async_reader.duration := Round(async_reader.duration/{$IFDEF DELPHI16_UP}1e7{$ELSE}1.e7{$ENDIF DELPHI16_UP});
       len := 4;
       if async_reader.HeaderInfo.GetAttributeByName(astream, g_wszWMBitrate, datatype, PByte(@async_reader.Bitrate), len) <> S_OK then
         async_reader.Bitrate :=0;
       async_reader.BlockList := TList.Create;
       res := async_reader.reader.Start(0, 0, async_reader.StretchFactor, nil);
     if res <> S_OK then
     begin
       raise EAuException.Create('Result ' + IntToHex(res, 8));
       async_reader.reader := nil;
       async_reader.WMReaderCallback := nil;
       async_reader.Event.Free;
       async_reader.CriticalSection.Free;
       Exit;
     end;
     end;
   end;

   procedure lwma_async_reader_pause(var async_reader : wma_async_reader);
   begin
     async_reader.reader.Pause;
     async_reader.Paused := True;
   end;

   procedure lwma_async_reader_resume(var async_reader : wma_async_reader);
   begin
       async_reader.reader.Resume;
   end;

   procedure lwma_async_reader_set_proxy(var async_reader : wma_async_reader; const Protocol, Host : WideString; Port : LongWord);
   var
    NetworkConfig : IWMReaderNetworkConfig;
   begin
     NetworkConfig := async_reader.reader as IWMReaderNetworkConfig;
     NetworkConfig.SetProxyHostName(PWideChar(Protocol), PWideChar(Host));
     NetworkConfig.SetProxyPort(PWideChar(Protocol), Port)
   end;

   procedure lwma_async_reader_add_logging_url(var async_reader : wma_async_reader; const URL : WideString);
   var
    NetworkConfig : IWMReaderNetworkConfig;
   begin
     NetworkConfig := async_reader.reader as IWMReaderNetworkConfig;
     if NetworkConfig.AddLoggingUrl(PWideChar(URL)) <> S_OK then
       raise EauException.Create('Canoot add a logging URL');
   end;

   procedure lwma_async_reader_clear_logging_urls(var async_reader : wma_async_reader);
   var
    NetworkConfig : IWMReaderNetworkConfig;
   begin
     NetworkConfig := async_reader.reader as IWMReaderNetworkConfig;
     NetworkConfig.ResetLoggingUrlList;
   end;


   procedure lwma_async_reader_reset_stretch(var async_reader : wma_async_reader; new_stretch : Single);
   begin
     async_reader.reader.Pause;
     async_reader.reader.Start(WM_START_CURRENTPOSITION, 0, new_stretch, nil);
   end;

   function get_async_tag_length(var async_reader : wma_async_reader; name : PWideChar) : Word;
   var
     stream : Word;
     datatype : WMT_ATTR_DATATYPE;
   begin
     stream := 0;
     if async_reader.HeaderInfo.GetAttributeByName(stream, name, datatype, nil, Result) <> S_OK then
     begin
       Result := 0;
       Exit;
     end;
   end;

   procedure get_async_tag(var async_reader : wma_async_reader; name : PWideChar; var value : WideString);
   var
     stream : Word;
     datatype : WMT_ATTR_DATATYPE;
     len : Word;
   begin
     stream := 0;
     len := Length(value);
     datatype := WMT_TYPE_STRING;
     if async_reader.HeaderInfo.GetAttributeByName(stream, name, datatype, PByte(PWideChar(value)), len) <> S_OK then
     begin
       value := '';
       Exit;
     end;
   end;

   function lwma_async_reader_get_author(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMAuthor);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMAuthor, Result);
   end;

   function lwma_async_reader_get_title(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMTitle);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMTitle, Result);
   end;

   function lwma_async_reader_get_album(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMAlbumTitle);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMAlbumTitle, result);
   end;

   function lwma_async_reader_get_genre(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMGenre);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMGenre, Result);
   end;

   function lwma_async_reader_get_track(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMTrack);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMTrack, Result);
   end;

   function lwma_async_reader_get_year(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMYear);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMYear, result);
   end;

   function lwma_async_reader_get_copyright(var async_reader : wma_async_reader) : WideString;
   var
     len : Word;
   begin
     len := get_async_tag_length(async_reader, g_wszWMCopyright);
     Result := StringOfChar(#0,len);
     get_async_tag(async_reader, g_wszWMCopyright, result);
   end;

   function has_data_block(var async_reader : wma_async_reader) : Boolean;
   begin
     Result := False;
     if async_reader.BlockList.Count = 0 then
     begin
       if not async_reader.Paused then
       begin
         if (async_reader.status = WMT_CLOSED) or async_reader.TimedOut or (async_reader.status = WMT_EOF) then
         begin
           async_reader.StretchFactor := 1.0;
           Exit;
         end;
       end else
       begin
         async_reader.status := WMT_STARTED;
         async_reader.Paused := False;
       end;  
       async_reader.Event.WaitFor(async_reader.MaxWaitMilliseconds);
       if async_reader.BlockList.Count = 0 then
       begin
         async_reader.TimedOut := True;
         Exit;
       end;
     end;
     Result := True;
   end;

   procedure lwma_async_reader_get_data(var async_reader : wma_async_reader; var buffer : Pointer; var bufsize : LongWord);
   var
     SI : PSampleInfo;
   begin
     buffer := nil;
     while buffer = nil do
     begin
       if not has_data_block(async_reader) then
       begin
         bufsize := 0;
         Exit;
       end;
       async_reader.CriticalSection.Enter;
       SI := PSampleInfo(async_reader.BlockList.First);
       async_reader.CriticalSection.Leave;
       if SI.Offset = SI.Length then
       begin
         async_reader.CriticalSection.Enter;
         async_reader.BlockList.Delete(0);
         async_reader.CriticalSection.Leave;
         Freemem(SI.Data);
         Freemem(SI);
       end else
       begin
         buffer := @(SI.Data[SI.Offset]);
         if bufsize > SI.Length - SI.Offset then
         begin
           bufsize := SI.Length - SI.Offset;
           SI.Offset := SI.Length;
         end else
           Inc(SI.Offset, bufsize)
       end;
     end;
   end;

   procedure lwma_async_reader_free(var async_reader : wma_async_reader);
   var
     i : Integer;
     SI : PSampleInfo;
   begin
     async_reader.reader.Stop;
     async_reader.reader.Close;
     async_reader.reader := nil;
     async_reader.WMReaderCallback := nil;
     async_reader.Event.SetEvent;
     async_reader.Event.Free;
     async_reader.CriticalSection.Release;
     async_reader.CriticalSection.Free;
     for i := 0 to async_reader.BlockList.Count - 1 do
     begin
       SI := async_reader.BlockList.Items[i];
       Freemem(SI.Data);
       Freemem(SI);
     end;
     async_reader.BlockList.Free;
   end;

   procedure lwma_reader_init(var sync_reader : wma_sync_reader; Stream : TStream; Descrete : Boolean; Speakers : ShortInt);
   var
     OutputCount, FormatCount : LongWord;
     i, j, size : LongWord;
     MediaProps : IWMOutputMediaProps;
     OutputMediaType, FormatMediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     datatype : WMT_ATTR_DATATYPE;
     len, astream : Word;
     NP : Int64;
     Enable : LongBool;
     Value : LongWord;
     Cond : Boolean;
   begin
     CoInitialize(nil);
     FillChar(sync_reader, SizeOf(sync_reader), 0);
     if WMCreateSyncReader(nil, 0, sync_reader.reader) <> S_OK then
     Exit;
     sync_reader.AudioStream := TAudioStream.Create;
     sync_reader.AudioStream.AssignStream(Stream);
     sync_reader.AuStream := sync_reader.AudioStream as IStream;
     if sync_reader.reader.OpenStream(sync_reader.AuStream) <> S_OK then
     begin
       sync_reader.AudioStream.Seek(30, 0, NP);
       if sync_reader.reader.OpenStream(sync_reader.AuStream) <> S_OK then
       begin
         sync_reader.reader := nil;
         sync_reader.AudioStream := nil;
         Exit;
       end;
     end;
     if sync_reader.reader.GetOutputCount(OutputCount) <> S_OK then
     begin
       sync_reader.reader := nil;
       sync_reader.AudioStream := nil;
       Exit;
     end;
     if OutputCount > 0 then
     for i := 0 to OutputCount - 1 do
     begin
       sync_reader.reader.GetOutputProps(i, MediaProps);
       MediaProps.GetMediaType(nil, size);
       GetMem(OutputMediaType, size);
       MediaProps.GetMediaType(OutputMediaType, size);
       if GUIDSEqual(OutputMediaType.majortype, WMMEDIATYPE_Audio) then
       begin
         Enable := LongBool(Descrete);
         sync_reader.reader.SetOutputSetting(i, g_wszEnableDiscreteOutput, WMT_TYPE_BOOL, PByte(@Enable), 4);
         if Speakers = 0 then
         begin
           format := PWAVEFORMATEX(OutputMediaType.pbFormat);
           sync_reader.SampleRate := format.nSamplesPerSec;
           sync_reader.channels := format.nChannels;
           sync_reader.reader.GetOutputFormatCount(i, FormatCount);
           if FormatCount > 0 then
           for j := 0 to FormatCount  - 1 do
           begin
             sync_reader.reader.GetOutputFormat(i, j, MediaProps);
             MediaProps.GetMediaType(nil, size);
             GetMem(FormatMediaType, size);
             MediaProps.GetMediaType(FormatMediaType, size);
             if GUIDSEqual(FormatMediaType.formattype, WMFORMAT_WaveFormatEx) then
             begin
               format := PWAVEFORMATEX(FormatMediaType.pbFormat);
               Cond := (format.wBitsPerSample >= sync_reader.BitsPerSample)
                 and (format.nSamplesPerSec = LongWord(sync_reader.SampleRate))
                 and (sync_reader.channels = format.nChannels);

               if Cond then
               begin
                 sync_reader.has_audio := True;
                 sync_reader.output := i;
                // sync_reader.channels := format.nChannels;
                // sync_reader.SampleRate := format.nSamplesPerSec;
                 sync_reader.BitsPerSample := format.wBitsPerSample;
                 sync_reader.reader.SetOutputProps(i, MediaProps);
               end;
             end; // if GUIDSEqual(FormatMediaType.formattype, WMFORMAT_WaveFormatEx) then
     	     FreeMem(FormatMediaType);
           end; // for j := 0 to FormatCount  - 1 do
         end else // if Speakers = 0 then
         begin
           if Speakers > 2 then
           begin
             Value := Speakers;
             sync_reader.reader.SetOutputSetting(i, g_wszSpeakerConfig, WMT_TYPE_DWORD, PByte(@Value), 4);
           end;
           sync_reader.reader.GetOutputFormatCount(i, FormatCount);
           if FormatCount > 0 then
           for j := 0 to FormatCount  - 1 do
           begin
             sync_reader.reader.GetOutputFormat(i, j, MediaProps);
             MediaProps.GetMediaType(nil, size);
             GetMem(FormatMediaType, size);
             MediaProps.GetMediaType(FormatMediaType, size);
             if GUIDSEqual(FormatMediaType.formattype, WMFORMAT_WaveFormatEx) then
             begin
               format := PWAVEFORMATEX(FormatMediaType.pbFormat);
                if Speakers > 0 then
                  Cond := format.nChannels = Word(Speakers)
                else
                begin
                  Cond := (format.nChannels >= sync_reader.channels) and
                     (format.nSamplesPerSec >= LongWord(sync_reader.SampleRate))
                     and (format.wBitsPerSample >= LongWord(sync_reader.BitsPerSample));
                end;
                if Cond then
                begin
                 sync_reader.has_audio := True;
              	 sync_reader.output := i;
                 sync_reader.channels := format.nChannels;
	               sync_reader.SampleRate := format.nSamplesPerSec;
                 sync_reader.BitsPerSample := format.wBitsPerSample;
                 sync_reader.reader.SetOutputProps(i, MediaProps);
               end;
             end; // if GUIDSEqual(FormatMediaType.formattype, WMFORMAT_WaveFormatEx) then
     	       FreeMem(FormatMediaType);
           end; // for j := 0 to FormatCount  - 1 do
         end; // if Speakers = 0 then ... else
       end; // if GUIDSEqual(OutputMediaType.majortype, WMMEDIATYPE_Audio) then
       FreeMem(OutputMediaType);
    	 MediaProps := nil;
     end; // for i := 0 to OutputCount - 1 do

     if sync_reader.has_audio then
     begin
       sync_reader.reader.GetStreamNumberForOutput(sync_reader.output, sync_reader.stream);
       if sync_reader.reader.SetReadStreamSamples(sync_reader.stream, false) = NS_E_PROTECTED_CONTENT then
	 sync_reader._protected := True
       else
       sync_reader._protected := False;
       len := 8;
       astream := 0;
       sync_reader.reader.QueryInterface(IID_IWMHeaderInfo, sync_reader.HeaderInfo);
       if sync_reader.HeaderInfo.GetAttributeByName(astream, g_wszWMDuration, datatype, PByte(@(sync_reader.duration)), len) <> S_OK then
         sync_reader.duration := 0;
     end;
   end;

   procedure lwma_reader_init1(var sync_reader : wma_sync_reader; Stream : TStream);
   var
     OutputCount : LongWord;
     i, size : LongWord;
     MediaProps : IWMOutputMediaProps;
     OutputMediaType : PWMMEDIATYPE;
     datatype : WMT_ATTR_DATATYPE;
     len, astream : Word;
     NP : Int64;
   begin
     CoInitialize(nil);
     FillChar(sync_reader, SizeOf(sync_reader), 0);
     if WMCreateSyncReader(nil, 0, sync_reader.reader) <> S_OK then
     Exit;
     sync_reader.AudioStream := TAudioStream.Create;
     sync_reader.AudioStream.AssignStream(Stream);
     sync_reader.AuStream := sync_reader.AudioStream as IStream;
     if sync_reader.reader.OpenStream(sync_reader.AuStream) <> S_OK then
     begin
       sync_reader.AudioStream.Seek(30, 0, NP);
       if sync_reader.reader.OpenStream(sync_reader.AuStream) <> S_OK then
       begin
         sync_reader.reader := nil;
         sync_reader.AudioStream := nil;
         Exit;
       end;
     end;
     if sync_reader.reader.GetOutputCount(OutputCount) <> S_OK then
     begin
       sync_reader.reader := nil;
       sync_reader.AudioStream := nil;
       Exit;
     end;
     if OutputCount > 0 then
     for i := 0 to OutputCount - 1 do
     begin
       sync_reader.reader.GetOutputProps(i, MediaProps);
       MediaProps.GetMediaType(nil, size);
       GetMem(OutputMediaType, size);
       MediaProps.GetMediaType(OutputMediaType, size);
       if GUIDSEqual(OutputMediaType.majortype, WMMEDIATYPE_Audio) then
       begin
         sync_reader.output := i;
         sync_reader.has_audio := True;
         FreeMem(OutputMediaType);
         MediaProps := nil;
         Break;
       end;
       FreeMem(OutputMediaType);
       MediaProps := nil;
     end; // for i := 0 to OutputCount - 1 do

     if sync_reader.has_audio then
     begin
       sync_reader.reader.GetStreamNumberForOutput(sync_reader.output, sync_reader.stream);
       if sync_reader.reader.SetReadStreamSamples(sync_reader.stream, false) = NS_E_PROTECTED_CONTENT then
	 sync_reader._protected := True
       else
       sync_reader._protected := False;
       len := 8;
       astream := 0;
       sync_reader.reader.QueryInterface(IID_IWMHeaderInfo, sync_reader.HeaderInfo);
       if sync_reader.HeaderInfo.GetAttributeByName(astream, g_wszWMDuration, datatype, PByte(@(sync_reader.duration)), len) <> S_OK then
         sync_reader.duration := 0;
     end;
   end;

   function lwma_reader_get_format_count(var sync_reader : wma_sync_reader; Descrete : Boolean) : LongWord;
   var
     Enable : LongBool;
   begin
     Enable := LongBool(Descrete);
     sync_reader.reader.SetOutputSetting(sync_reader.output, g_wszEnableDiscreteOutput, WMT_TYPE_BOOL, PByte(@Enable), 4);
     sync_reader.reader.GetOutputFormatCount(sync_reader.output, Result);
   end;

   procedure lwma_reader_get_format(var sync_reader : wma_sync_reader; Descrete : Boolean; FormatIndex : LongWord; var channels, bps, samplerate : LongWord);
   var
     MediaProps : IWMOutputMediaProps;
     FormatMediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     Enable : LongBool;
     size : LongWord;
   begin
     Enable := LongBool(Descrete);
     sync_reader.reader.SetOutputSetting(sync_reader.output, g_wszEnableDiscreteOutput, WMT_TYPE_BOOL, PByte(@Enable), 4);
     sync_reader.reader.GetOutputFormat(sync_reader.output, FormatIndex, MediaProps);
     MediaProps.GetMediaType(nil, size);
     GetMem(FormatMediaType, size);
     MediaProps.GetMediaType(FormatMediaType, size);
     if GUIDSEqual(FormatMediaType.formattype, WMFORMAT_WaveFormatEx) then
     begin
       format := PWAVEFORMATEX(FormatMediaType.pbFormat);
       channels := format.nChannels;
       bps := format.wBitsPerSample;
       samplerate := format.nSamplesPerSec;
     end else
     begin
       channels := 0;
       bps := 0;
       samplerate := 0;
     end;
     FreeMem(FormatMediaType);
     MediaProps := nil;
   end;

   procedure lwma_reader_set_format(var sync_reader : wma_sync_reader; Descrete : Boolean; FormatIndex : LongWord);
   var
     MediaProps : IWMOutputMediaProps;
     FormatMediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     Enable : LongBool;
     size : LongWord;
   begin
     Enable := LongBool(Descrete);
     sync_reader.reader.SetOutputSetting(sync_reader.output, g_wszEnableDiscreteOutput, WMT_TYPE_BOOL, PByte(@Enable), 4);
     sync_reader.reader.GetOutputFormat(sync_reader.output, FormatIndex, MediaProps);
     MediaProps.GetMediaType(nil, size);
     GetMem(FormatMediaType, size);
     MediaProps.GetMediaType(FormatMediaType, size);
     if GUIDSEqual(FormatMediaType.formattype, WMFORMAT_WaveFormatEx) then
     begin
       format := PWAVEFORMATEX(FormatMediaType.pbFormat);
       sync_reader.channels := format.nChannels;
       sync_reader.BitsPerSample := format.wBitsPerSample;
       sync_reader.SampleRate := format.nSamplesPerSec;
     end;
     sync_reader.reader.SetOutputProps(sync_reader.output, MediaProps);
     FreeMem(FormatMediaType);
     MediaProps := nil;
   end;

   function lwma_reader_has_audio(reader : Pointer) : Byte;
   var
     sync_reader : pwma_sync_reader;
   begin
     sync_reader := reader;
     Result := Byte(sync_reader.has_audio);
   end;

   function lwma_reader_is_protected(reader : Pointer) : Byte;
   var
     sync_reader : pwma_sync_reader;
   begin
     sync_reader := reader;
     Result := Byte(sync_reader._protected);
   end;

   function lwma_reader_get_duration(var sync_reader : wma_sync_reader) : LongWord;
   begin
     Result := Round(sync_reader.duration/0.1e6);
   end;

   function lwma_reader_get_bitrate(var sync_reader : wma_sync_reader) : LongWord;
   var
     len, stream : Word;
     datatype : WMT_ATTR_DATATYPE;
   begin
     len := 4;
     stream := 0;
     if sync_reader.HeaderInfo.GetAttributeByName(stream, g_wszWMBitrate, datatype, PByte(@Result), len) <> S_OK then
       Result:=0;
   end;

   function lwma_reader_get_is_vbr(var sync_reader : wma_sync_reader) : LongBool;
   var
     len, stream : Word;
     datatype : WMT_ATTR_DATATYPE;
   begin
     len := 4;
     stream := 0;
     if sync_reader.HeaderInfo.GetAttributeByName(stream, g_wszWMIsVBR, datatype, PByte(@Result), len) <> S_OK then
       Result:=False;
   end;

   function get_tag_length(var sync_reader : wma_sync_reader; name : PWideChar) : Word;
   var
     stream : Word;
     datatype : WMT_ATTR_DATATYPE;
   begin
     stream := 0;
     if sync_reader.HeaderInfo.GetAttributeByName(stream, name, datatype, nil, Result) <> S_OK then
     begin
       Result := 0;
       Exit;
     end;
   end;

   procedure get_tag(var sync_reader : wma_sync_reader; name : PWideChar; var value : WideString);
   var
     stream : Word;
     datatype : WMT_ATTR_DATATYPE;
     len : Word;
     res : HResult;
   begin
     stream := 0;
     len := Length(value);
     datatype := WMT_TYPE_STRING;
     res := sync_reader.HeaderInfo.GetAttributeByName(stream, name, datatype, PByte(PWideChar(value)), len);
     if res <> S_OK then
     begin
       value := '';
       Exit;
     end;
   end;

   function lwma_reader_get_author(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMAuthor);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMAuthor, Result);
   end;

   function lwma_reader_get_title(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMTitle);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMTitle, Result);
   end;

   function lwma_reader_get_album(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMAlbumTitle);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMAlbumTitle, Result);
   end;

   function lwma_reader_get_genre(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMGenre);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMGenre, result);
   end;

   function lwma_reader_get_track(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMTrack);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMTrack, Result);
   end;

   function lwma_reader_get_year(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMYear);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMYear, Result);
   end;

   function lwma_reader_get_copyright(var sync_reader : wma_sync_reader) : WideString;
   var
     len : Word;
   begin
     len := get_tag_length(sync_reader, g_wszWMCopyright);
     SetLength(Result, len);
     get_tag(sync_reader, g_wszWMCopyright, Result);
   end;

   procedure lwma_reader_get_audio_properties(var sync_reader : wma_sync_reader; var channels, BitsPerSample : Word; var SampleRate : LongWord);
   begin
     channels := sync_reader.channels;
     BitsPerSample := sync_reader.BitsPerSample;
     SampleRate := sync_reader.SampleRate;
   end;

   procedure lwma_reader_seek(var sync_reader : wma_sync_reader; start_from : LongWord);
   begin
     sync_reader.reader.SetRange(Round(start_from*0.1e6), 0);
   end;

   procedure lwma_reader_get_data(var sync_reader : wma_sync_reader; var buffer : Pointer; var bufsize : LongWord);
   var
     buf : PByte;
     len : LongWord;
     copylen : LongWord;
     time, duration : Int64;
     flags : LongWord;
     Output : LongWord;
     w : Word;
     res : HResult;
   begin
     while (True) do
     begin
       if sync_reader.buffer <> nil then
       begin
	       sync_reader.buffer.GetBufferAndLength(buf, len);
         if sync_reader.offset < len then
         begin
           if bufsize < (len - sync_reader.offset) then
             copylen :=  bufsize
           else
             copylen := len - sync_reader.offset;
	         LongWord(buffer) := LongWord(buf) + sync_reader.offset;
	         bufsize := copylen;
	         sync_reader.offset := sync_reader.offset + copylen;
	         Exit;
	       end;
	// sync_reader.buffer._Release;
         sync_reader.buffer := nil;
	       sync_reader.offset := 0;
       end;
       res := sync_reader.reader.GetNextSample(sync_reader.stream, sync_reader.buffer, time, duration, flags, Output, w);
       if res <> S_OK then
       begin
//         raise Exception.Create(IntToHex(Res, 8));
	       buffer := nil;
	       bufsize := 0;
         Exit;
       end;
     end;
   end;

   procedure lwma_reader_free(var sync_reader : wma_sync_reader);
   begin
     sync_reader.reader.Close;
      if sync_reader.buffer <> nil then
		sync_reader.buffer := nil;
     sync_reader.reader := nil;
     sync_reader.HeaderInfo := nil;
     sync_reader.AudioStream := nil;
     sync_reader.AudioStream.Free;
     sync_reader.AudioStream := nil;
   end;

   procedure lwma_get_codec_names(var SL : TStringList);
   var
     ProfileManager : IWMProfileManager;
     CodecInfo : IWMCodecInfo3;
     Codecs, CodecIndex : LongWord;
     CNameSize : LongWord;
     Name : WideString;
   begin
     WMCreateProfileManager(ProfileManager);
     CodecInfo := ProfileManager as IWMCodecInfo3;
     CodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, Codecs);
     if Codecs > 0 then
     for CodecIndex := 0 to Codecs - 1 do
     begin
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, nil, CNameSize);
       SetLength(Name, CNameSize);
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, @Name[1], CNameSize);
       SL.Add(String(Name));
     end;
     CodecInfo := nil;
     ProfileManager := nil;

   end;

   procedure create_configs(var List : TList);
   var
     ProfileManager : IWMProfileManager;
     CodecInfo : IWMCodecInfo3;
     Codecs, CodecIndex, Formats, FormatIndex : LongWord;
     CNameSize : LongWord;
     Name : WideString;
     Profile : IWMProfile;
     VBREnabled : LongBool;
     Passes : LongWord;
     StreamConfig : IWMStreamConfig;
   begin
     WMCreateProfileManager(ProfileManager);
     CodecInfo := ProfileManager as IWMCodecInfo3;
     CodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, Codecs);
     if Codecs > 0 then
     for CodecIndex := 0 to Codecs - 1 do
     begin
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, nil, CNameSize);
       SetLength(Name, CNameSize);
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, @Name[1], CNameSize);
       if PWideChar(Name) = 'WM-AUDIO' then
       begin
         Passes := 1;
         VBREnabled := False;
           CodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@VBREnabled), 4);
           CodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszNumPasses, WMT_TYPE_DWORD, PByte(@Passes), 4);
           Formats := 0;
           CodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, CodecIndex, Formats);
           if Formats > 0 then
           for FormatIndex := 0 to Formats - 1 do
           begin
             Name := ' ';
             CodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, FormatIndex, StreamConfig, nil, CNameSize);
             SetLength(Name, CNameSize);
             CodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, FormatIndex, StreamConfig, @Name[1], CNameSize);
           end;
       end;
     end;

     ProfileManager.CreateEmptyProfile($00090000, Profile);
 //    Profile.CreateNewStream()
    CodecInfo := nil;
    Profile := nil;
    ProfileManager := nil;
   StreamConfig := nil;
  end;

   procedure lwma_writer_init(var writer : wma_writer; pwszFilename : PWChar; Owner : TComponent; CallError : TErrorNotifyProc);
   var
     pSinkBase : IWMWriterSink;
     pWriterAdvanced : IWMWriterAdvanced;
     RegisterCallback : IWMRegisterCallback;
    begin
     CoInitialize(nil);
     FillChar(writer, SizeOf(writer), 0);
     writer.Owner := Owner;
     writer.OnError := CallError;
     if WMCreateWriterFileSink(writer.file_sink) <> S_OK then
       Exit;
     if WMCreateWriter(nil, writer.writer) <> S_OK then
     begin
       writer.file_sink := nil;
       Exit;
     end;
     writer.file_sink.QueryInterface(IID_IWMWriterSink, pSinkBase);
     writer.writer.QueryInterface(IID_IWMWriterAdvanced, pWriterAdvanced);
     pWriterAdvanced.AddSink(pSinkBase);
     pWriterAdvanced := nil;
     pSinkBase := nil;
     if writer.file_sink.Open(pwszFilename) <> S_OK then
     begin
       writer.file_sink := nil;
       writer.writer := nil;
       exit;
     end;
     writer.ErrorCallback := TWMErrorCallback.Create(@writer) as IWMStatusCallback;
     RegisterCallback := writer.file_sink as IWMRegisterCallback;
     RegisterCallback.Advise(writer.ErrorCallback, nil);
     RegisterCallback := nil;
     Writer.Initialized := True;
   end;

   procedure lwma_network_writer_init(var writer : wma_writer; PortNumber, MaxClients : LongWord; Owner : TComponent; CallConnected, CallDisconnected : TNotifyProc; CallError : TErrorNotifyProc);
   var
    // pSinkBase : IWMWriterSink;
     pWriterAdvanced : IWMWriterAdvanced;
     RegisterCallback : IWMRegisterCallback;
   begin
     CoInitialize(nil);
     FillChar(writer, SizeOf(writer), 0);
     writer.Owner := Owner;
     writer.OnConnected := CallConnected;
     writer.OnDisconnected := CallDisconnected;
     if WMCreateWriterNetworkSink(writer.network_sink) <> S_OK then
       Exit;
     if WMCreateWriter(nil, writer.writer) <> S_OK then
     begin
       writer.network_sink := nil;
       Exit;
     end;
     //writer.network_sink.QueryInterface(IID_IWMWriterSink, pSinkBase);
     writer.network_sink.SetNetworkProtocol(WMT_PROTOCOL_HTTP);
     if writer.network_sink.Open(PortNumber) <> S_OK then
     begin
       writer.network_sink := nil;
       writer.writer := nil;
       exit;
     end;
     if MaxClients > 0 then
       writer.network_sink.SetMaximumClients(MaxClients);
     writer.writer.QueryInterface(IID_IWMWriterAdvanced, pWriterAdvanced);
     if pWriterAdvanced.AddSink(writer.network_sink) <> S_OK then
     begin
       writer.network_sink := nil;
       writer.writer := nil;
       exit;
     end;
     pWriterAdvanced := nil;
 //    pSinkBase := nil;
     writer.StatusCallback := TWMStatusCallback.Create(@writer) as IWMStatusCallback;
     RegisterCallback := writer.network_sink as IWMRegisterCallback;
     RegisterCallback.Advise(writer.StatusCallback, nil);
     RegisterCallback := nil;
     writer.Initialized := True;
   end;

   function lwm_network_writer_get_url(var writer : wma_writer) : WideString;
   var
     len : LongWord;
   begin
     if writer.Initialized then
     begin
       writer.network_sink.GetHostURL(nil, len);
       SetLength(Result, len);
       writer.network_sink.GetHostURL(PWideChar(Result), len);
     end;
   end;

 (*  function SeletBestConfig(SubType : TGUID; pWaveLimits : pWAVEFORMATEX; dwMaxRate : LongWord; out ppStreamConfig : IWMStreamConfig) : Boolean;
   var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo3;
    pStreamConfig : IWMStreamConfig;
    pBestMatch : IWMStreamConfig;
    pProps : IWMMediaProps;
    pType : PWMMEDIATYPE;
    cbType : LongWord;
    pWave  : PWAVEFORMATEX;
    index, cEntries, dwBestRate, dwCurRate, PacketsPerSecond, CodecIndex, CurSR : LongWord;
    const INVALID_INDEX = $FFFFF;
   begin
     dwCurRate := 0;
     CurSR := 0;
     cEntries := 0;
     dwBestRate := 0;
     PacketsPerSecond := 0;
     CodecIndex := 0;
     pBestMatch := nil;

     WMCreateProfileManager(pProfileMgr);
     pCodecInfo := pProfileMgr as IWMCodecInfo3;
     pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
     CodecIndex := INVALID_INDEX;
     for index := 0 to  cEntries -1 do
     begin
       pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, index, 0, pStreamConfig);
       if pStreamConfig = nil then
        Break;
       pProps := pStreamConfig as IWMMediaProps;
       pProps.GetMediaType(nil, cbType);
       if pProps = nil then
        Break;

       GetMem(pType, cbType);
       pProps.GetMediaType(pType, cbType);
       // Check this codec against the one requested.

       if GUIDSEqual(pType.subtype, SubType) then
         CodecIndex := index;
       pStreamConfig := nil;
       pProps := nil;
       FreeMem(pType);
       if CodecIndex <> INVALID_INDEX then
         break;
     end;
     if CodecIndex = INVALID_INDEX then
     begin
       Result := False;
       Exit;
     end;
     pCodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, CodecIndex, cEntries);

     for index := 0 to cEntries - 1 do
     begin
       pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, CodecIndex, index, pStreamConfig);
       pProps := pStreamConfig as IWMMediaProps;
       pProps.GetMediaType(nil, cbType);
       GetMem(pType, cbType);
       pProps.GetMediaType(pType, cbType);
       pWave := PWAVEFORMATEX(pType.pbFormat);
       pStreamConfig.GetBitrate(dwCurRate);
       if Abs(dwCurRate - dwMaxRate) < Abs(dwBestRate - dwMaxRate)
       then
       begin
         dwBestRate := dwCurRate;
         if (pWave.nChannels = pWaveLimits.nChannels) then
         if (pWave.wBitsPerSample = pWaveLimits.wBitsPerSample) then
         if Abs(pWave.nSamplesPerSec - pWaveLimits.nSamplesPerSec) < Abs(CurSR - pWaveLimits.nSamplesPerSec) then
         begin
           CurSR := pWave.nSamplesPerSec;
           pBestMatch := nil;
           pBestMatch := pStreamConfig;
           //pStreamConfig._AddRef;
           dwBestRate := (pWave.nAvgBytesPerSec * 8);
         end;
       end;
       pStreamConfig := nil;
       pProps := nil;
       FreeMem(pType);
    end;
    if pBestMatch = nil then
    begin
      Result := False;
      Exit;
    end;
    ppStreamConfig := pBestMatch;
    Result := True;
  end;  *)

  procedure lwma_enumerate_codecs(var Codecs : TStringList);
  var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo2;
    cEntries : LongWord;
    CodecName : WideString;
    len : LongWord;
    i : LongWord;
  begin
    CoInitialize(nil);
    pProfileMgr := nil;
    pCodecInfo := nil;
    WMCreateProfileManager(pProfileMgr); 
    pCodecInfo := pProfileMgr as IWMCodecInfo2;
    pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
    if cEntries > 0 then
    for i := 0 to cEntries - 1 do
    begin
      pCodecInfo.GetCodecName(WMMEDIATYPE_Audio, i, nil, len);
      len := len * 2;
      SetLength(CodecName, len);
      pCodecInfo.GetCodecName(WMMEDIATYPE_Audio, i, PWideChar(CodecName), len);
      Codecs.Add(CodecName);
    end;
    pCodecInfo := nil;
    pProfileMgr := nil;
  end;

  procedure lwma_enumerate_codec_formats(CodecIndex : LongWord; VBR : LongBool; var Formats : TStringList);
  var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo3;
    cEntries, cEntries2 : LongWord;
    FormatDesc : WideString;
    len : LongWord;
    i : Integer;
    Dummie : IWMStreamConfig;
  begin
    CoInitialize(nil);
    WMCreateProfileManager(pProfileMgr);
    pCodecInfo := pProfileMgr as IWMCodecInfo3;
    pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
    if CodecIndex >= cEntries then Exit;
    if VBR then
       if pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@VBR), SizeOf(VBR)) <> S_OK then
          Exit;
    pCodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, CodecIndex, cEntries2);
    if cEntries2 > 0 then
    for i := 0 to cEntries2 - 1 do
    begin
      pCodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, i, Dummie, nil, len);
      Dummie := nil;
      len := len * 2;
      SetLength(FormatDesc, len);
      pCodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, i, Dummie, PWideChar(FormatDesc), len);
      Dummie := nil;
      Formats.Add(FormatDesc);
    end;
    pCodecInfo := nil;
    pProfileMgr := nil;
  end;

  procedure lwma_enumerate_codec_formats2(CodecIndex : LongWord; VBR : LongBool; var Formats : TStringList);
  var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo3;
    cEntries, cEntries2 : LongWord;
    FormatDesc : WideString;
    len : LongWord;
    i : Integer;
    Dummie : IWMStreamConfig;
    Passes : LongWord;
  begin
    CoInitialize(nil);
    WMCreateProfileManager(pProfileMgr);
    pCodecInfo := pProfileMgr as IWMCodecInfo3;
    pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
    if CodecIndex >= cEntries then Exit;
    Passes := 2;
    pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszNumPasses, WMT_TYPE_DWORD, PByte(@Passes), SizeOf(Passes));
    if pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@VBR), SizeOf(VBR)) <> S_OK then
          Exit;
    pCodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, CodecIndex, cEntries2);
    if cEntries2 > 0 then
    for i := 0 to cEntries2 - 1 do
    begin
      pCodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, i, Dummie, nil, len);
      Dummie := nil;
      len := len * 2;
      SetLength(FormatDesc, len);
      pCodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, i, Dummie, PWideChar(FormatDesc), len);
      Dummie := nil;
      Formats.Add(FormatDesc);
    end;
    pCodecInfo := nil;
    pProfileMgr := nil;
  end;


   procedure lwma_writer_set_audio_properties2(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; VBR : LongBool; CodecIndex, FormatIndex : LongWord);
   var
     Size : LongWord;
     cInputs, inputIndex : LongWord;
     pProps : IWMInputMediaProps;
     MediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     Config : IWMStreamConfig;
     ProfileManager : IWMProfileManager;
     Profile : IWMProfile;
     pCodecInfo : IWMCodecInfo3;
     cEntries : LongWord;
   begin
     WMCreateProfileManager(ProfileManager);
     ProfileManager.CreateEmptyProfile(WMT_VER_9_0, Profile);
     pCodecInfo := ProfileManager as IWMCodecInfo3;
     pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
     if CodecIndex >= cEntries then Exit;
        if VBR then
           if pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@VBR), SizeOf(VBR)) <> S_OK then
              Exit;
     pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, CodecIndex, FormatIndex, Config);
     Config.SetStreamNumber(1);
     Config.SetStreamName('NAAudioStream');
     Config.SetConnectionName('NAAudioConnection');
     if Profile.AddStream(Config) <> S_OK then
       raise exception.Create('fff');
     writer.writer.SetProfile(Profile);
     Config := nil;
     Profile := nil;
     writer.writer.GetInputCount(cInputs);
     if cInputs > 0 then
     for inputIndex := 0 to cInputs - 1 do
     begin
       writer.writer.GetInputProps(inputIndex, pProps);
       pProps.GetMediaType(nil, size);
       GetMem(MediaType, size);
       pProps.GetMediaType(MediaType, size);
       if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
       begin
   	 writer.input := inputIndex;
	 format := PWAVEFORMATEX(MediaType.pbFormat);
	 format.nChannels := channels;
	 format.nSamplesPerSec := SampleRate;
	 format.wBitsPerSample := BitsPerSample;
	 format.wFormatTag := 1;
	 format.nBlockAlign := (BitsPerSample*channels) div 8;
	 format.nAvgBytesPerSec := SampleRate*format.nBlockAlign;
	 writer.BytesPerSecond := format.nAvgBytesPerSec;
	 pProps.SetMediaType(MediaType);
	 writer.writer.SetInputProps(inputIndex, pProps);
 	 FreeMem(MediaType);
	 pProps := nil;
	 Exit;
       end;
       FreeMem(MediaType);
       pProps := nil;
     end;
     pCodecInfo := nil;
   end;

   procedure lwma_writer_set_audio_properties3(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; VBR : LongBool; CodecIndex, FormatIndex : LongWord);
   var
     Size : LongWord;
     cInputs, inputIndex : LongWord;
     pProps : IWMInputMediaProps;
     MediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     Config : IWMStreamConfig;
     ProfileManager : IWMProfileManager;
     Profile : IWMProfile;
     pCodecInfo : IWMCodecInfo3;
     cEntries, Passes : LongWord;
   begin
     WMCreateProfileManager(ProfileManager);
     ProfileManager.CreateEmptyProfile(WMT_VER_9_0, Profile);
     pCodecInfo := ProfileManager as IWMCodecInfo3;
     pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
     if CodecIndex >= cEntries then Exit;
     Passes := 2;
     pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszNumPasses, WMT_TYPE_DWORD, PByte(@Passes), SizeOf(Passes));
     if pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@VBR), SizeOf(VBR)) <> S_OK then
        Exit;
     pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, CodecIndex, FormatIndex, Config);
     Config.SetStreamNumber(1);
     Config.SetStreamName('NAAudioStream');
     Config.SetConnectionName('NAAudioConnection');
     if Profile.AddStream(Config) <> S_OK then
       raise exception.Create('fff');
     writer.writer.SetProfile(Profile);
     Config := nil;
     Profile := nil;
     writer.writer.GetInputCount(cInputs);
     if cInputs > 0 then
     for inputIndex := 0 to cInputs - 1 do
     begin
       writer.writer.GetInputProps(inputIndex, pProps);
       pProps.GetMediaType(nil, size);
       GetMem(MediaType, size);
       pProps.GetMediaType(MediaType, size);
       if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
       begin
   	 writer.input := inputIndex;
	 format := PWAVEFORMATEX(MediaType.pbFormat);
	 format.nChannels := channels;
	 format.nSamplesPerSec := SampleRate;
	 format.wBitsPerSample := BitsPerSample;
	 format.wFormatTag := 1;
	 format.nBlockAlign := (BitsPerSample*channels) div 8;
	 format.nAvgBytesPerSec := SampleRate*format.nBlockAlign;
	 writer.BytesPerSecond := format.nAvgBytesPerSec;
	 pProps.SetMediaType(MediaType);
	 writer.writer.SetInputProps(inputIndex, pProps);
 	 FreeMem(MediaType);
	 pProps := nil;
	 Exit;
       end;
       FreeMem(MediaType);
       pProps := nil;
     end;
     pCodecInfo := nil;
   end;


  function SeletBestConfig(pWaveLimits : pWAVEFORMATEX; dwMaxRate : LongWord; Lossless : Boolean; VBR : Boolean; out ppStreamConfig : IWMStreamConfig) : Boolean;
   var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo3;
    pStreamConfig : IWMStreamConfig;
    pBestMatch : IWMStreamConfig;
    pProps : IWMMediaProps;
    pType : PWMMEDIATYPE;
    cbType : LongWord;
    pWave  : PWAVEFORMATEX;
    index, index2, cEntries, cEntries2, dwBestRate, dwCurRate, CurSR : LongWord;
    ES :  LongBool;
    Cond : Boolean;
    res : HResult;
    DType : WMT_ATTR_DATATYPE;
    Q, TmpQ, Size : LongWord;
    const INVALID_INDEX = $FFFFF;
   begin
     dwCurRate := 0;
     CurSR := 0;
     cEntries := 0;
     dwBestRate := 0;
     pBestMatch := nil;
     Cond := False;

     TmpQ := 0;
     WMCreateProfileManager(pProfileMgr);
     pCodecInfo := pProfileMgr as IWMCodecInfo3;
     pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
     ES := True;
     if cEntries > 0 then
     for index := 0 to  cEntries -1 do
     begin
        if Lossless or VBR then
        begin
          res := pCodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, index, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@ES), SizeOf(ES));
          if res  <> S_OK then Continue;
        end;
//       pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, index, 0, pStreamConfig);
//       if pStreamConfig = nil then
//        Break;
       pCodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, Index, cEntries2);
       if cEntries2 > 0 then
       for index2 := 0 to cEntries2 - 1 do
       begin
         pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, Index, index2, pStreamConfig);
         if pStreamConfig = nil then
          Break;
         pProps := pStreamConfig as IWMMediaProps;
         pProps.GetMediaType(nil, cbType);
         GetMem(pType, cbType);
         pProps.GetMediaType(pType, cbType);
         pWave := PWAVEFORMATEX(pType.pbFormat);
         if Lossless then
           Cond := GUIDsEqual(pType.subtype, WMMEDIASUBTYPE_WMAudio_Lossless)
         else
         if VBR then
         begin
           pCodecInfo.GetCodecFormatProp(WMMEDIATYPE_Audio, Index, Index2, g_wszVBRQuality, DType, PByte(@Q), Size);
           if Abs(dwMaxRate - Q) < Abs(dwMaxRate - tmpQ) then
           begin
             tmpQ := Q;
             Cond := True;
           end;
         end else
         begin
           pStreamConfig.GetBitrate(dwCurRate);
           Cond := Abs(dwCurRate - dwMaxRate) < Abs(dwBestRate - dwMaxRate);
         end;
         if Cond then
         begin
           if (Lossless or VBR) and (pBestMatch = nil) then pBestMatch := pStreamConfig;
           if  (pWave.nChannels = pWaveLimits.nChannels) then
           if (pWave.wBitsPerSample = pWaveLimits.wBitsPerSample) then
           if Abs(pWave.nSamplesPerSec - pWaveLimits.nSamplesPerSec) <= Abs(CurSR - pWaveLimits.nSamplesPerSec) then
           begin
             dwBestRate := dwCurRate;
             CurSR := pWave.nSamplesPerSec;
             pBestMatch := nil;
             pBestMatch := pStreamConfig;
             //pStreamConfig._AddRef;
//             dwBestRate := (pWave.nAvgBytesPerSec * 8);
           end;
         end;
         pStreamConfig := nil;
         pProps := nil;
         FreeMem(pType);
       end;
    end;
    if pBestMatch = nil then
    begin
      Result := False;
      Exit;
    end;
 //   end else
 //    pBestMatch.SetBitrate(dwMaxRate);
    ppStreamConfig := pBestMatch;
    Result := True;
  end;


   procedure lwma_writer_set_audio_properties(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; Lossless, vbr : Boolean; DesiredBitrate : LongWord);
   var
     Size : LongWord;
     cInputs, inputIndex : LongWord;
     pProps : IWMInputMediaProps;
     MediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     Config : IWMStreamConfig;
     ProfileManager : IWMProfileManager;
     Profile : IWMProfile;
   begin
     if not VBR then
       if DesiredBitRate < 1000 then DesiredBitRate := DesiredBitRate*1000;
     New(format);
     format.nChannels := channels;
     format.nSamplesPerSec := SampleRate;
     format.wBitsPerSample := BitsPerSample;
     format.wFormatTag := 1;
     if not SeletBestConfig(format, DesiredBitrate, Lossless, vbr, Config) then
       raise exception.Create('Failed to init codec');
     Dispose(format);
     WMCreateProfileManager(ProfileManager);
     ProfileManager.CreateEmptyProfile(WMT_VER_9_0, Profile);
     Config.SetStreamNumber(1);
     Config.SetStreamName('NAAudioStream');
     Config.SetConnectionName('NAAudioConnection');
     if Profile.AddStream(Config) <> S_OK then
       raise exception.Create('fff');
     writer.writer.SetProfile(Profile);
     Config := nil;
     Profile := nil;
     writer.writer.GetInputCount(cInputs);
     if cInputs > 0 then
     for inputIndex := 0 to cInputs - 1 do
     begin
       writer.writer.GetInputProps(inputIndex, pProps);
       pProps.GetMediaType(nil, size);
       GetMem(MediaType, size);
       pProps.GetMediaType(MediaType, size);
       if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
       begin
   	 writer.input := inputIndex;
	 format := PWAVEFORMATEX(MediaType.pbFormat);
	 format.nChannels := channels;
	 format.nSamplesPerSec := SampleRate;
	 format.wBitsPerSample := BitsPerSample;
	 format.wFormatTag := 1;
	 format.nBlockAlign := (BitsPerSample*channels) div 8;
	 format.nAvgBytesPerSec := SampleRate*format.nBlockAlign;
	 writer.BytesPerSecond := format.nAvgBytesPerSec;
	 pProps.SetMediaType(MediaType);
	 writer.writer.SetInputProps(inputIndex, pProps);
 	 FreeMem(MediaType);
	 pProps := nil;
	 Exit;
       end;
       FreeMem(MediaType);
       pProps := nil;
     end;
   end;

   procedure set_tag(var writer : wma_writer; name : PWideChar; const tag : WideString);
   var
     hi : IWMHeaderInfo;
     Stream : Word;
   begin
     stream := 0;
     writer.writer.QueryInterface(IID_IWMHeaderInfo, hi);
     hi.SetAttribute(stream, name, WMT_TYPE_STRING, PByte(@tag[1]), Length(tag)*2);
     hi := nil;
   end;

   procedure lwma_writer_set_author(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMAuthor, value);
   end;

   procedure lwma_writer_set_title(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMTitle, value);
   end;

   procedure lwma_writer_set_album(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMAlbumTitle, value);
   end;

   procedure lwma_writer_set_genre(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMGenre, value);
   end;

   procedure lwma_writer_set_year(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMYear, value);
   end;

   procedure lwma_writer_set_copyright(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMCopyright, value);
   end;

   procedure lwma_writer_set_track(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMTrack, value);
   end;

   procedure lwma_writer_begin(var writer : wma_writer);
   var
     res : HResult;
   begin
     res := writer.writer.BeginWriting;
     if res <> S_OK then
       raise EAuException.Create('Filed to set data writer, possible reason: required WMA codec not installed.');
     writer.TotalTime := 0;
   end;

   procedure lwma_writer_write(var writer : wma_writer; Buffer : Pointer; len : LongWord);
   var
     buf : PByte;
     res : HResult;
    begin
     writer.writer.AllocateSample(len, writer.buffer);
     writer.buffer.GetBuffer(buf);
     Move(Buffer^, Buf^, len);
     writer.buffer.SetLength(len);
     writer.TotalTime := writer.TotalTime + Round(len/writer.BytesPerSecond/0.1e-6);
     res := writer.writer.WriteSample(writer.input, writer.TotalTime, 0, writer.buffer);
     if res <> S_OK then
       raise EAuException.Create('Failed to write data: ' + IntToHex(res, 8));
     writer.buffer := nil;
   end;

   procedure lwma_writer_free(var writer : wma_writer);
   begin
     writer.writer.EndWriting();
     writer.file_sink := nil;
     writer.writer := nil;
     writer.ErrorCallback := nil;
     writer.Initialized := False;
   end;

   procedure lwma_network_writer_free(var writer : wma_writer);
   begin
     writer.writer.EndWriting();
     writer.network_sink.Close;
     (writer.writer as IWMWriterAdvanced).RemoveSink(writer.network_sink);
     writer.network_sink := nil;
     writer.writer := nil;
     writer.StatusCallback := nil;
     writer.ErrorCallback := nil;
     writer.Initialized := False;
   end;

   procedure TAudioStream.AssignStream;
   begin
     FStream := Stream;
   end;

   function TAudioStream.Read;
   begin
     pcbRead^ := FStream.Read(pv^, cb);
     if pcbRead^ <> 0 then Result := S_OK
     else Result := -1;
   end;

   function TAudioStream.Write;
   begin
     pcbWritten^ := FStream.Write(pv^, cb);
     if pcbWritten^ <> 0 then Result := S_OK
     else Result := -1;
   end;

   function TAudioStream.Seek;
   begin
     FStream.Seek(dlibMove, dwOrigin);
//     libNewPosition := FStream.Position;
     Result := S_OK;
   end;

   function TAudioStream.SetSize;
   begin
     FStream.Seek(libNewSize, 0);
     Result := S_OK;
   end;

   function TAudioStream.CopyTo;
   begin
     Result := -1;
   end;

   function TAudioStream.Commit;
   begin
     Result := -1;
   end;

   function TAudioStream.Revert;
   begin
     Result := -1;
   end;

   function TAudioStream.LockRegion;
   begin
     Result := -1;
   end;

   function TAudioStream.UnlockRegion;
   begin
     Result := -1;
   end;


   function TAudioStream.Stat;
   begin
     statstg.cbSize := FStream.Size;
     statstg.grfLocksSupported := 0;
     Result := S_OK;
   end;

   function TAudioStream.Clone;
   begin
     Result := -1;
   end;

   constructor TWMReaderCallback.Create;
   begin
     inherited Create;
     FReader := reader;
   end;

   function TWMReaderCallback.OnStatus;
   begin

     FReader.status := Status;
     if Status = WMT_Opened then
       FReader.Event.SetEvent;
     if hr <> S_OK then
       FReader.status := WMT_CLOSED;
       //raise EAuException.Create(IntToHex(hr, 8));
     Result := S_OK;
   end;

   function TWMReaderCallback.OnSample;
   var
     Buffer : PByte;
     Length : DWord;
     SI : PSampleInfo;
   begin
     if dwOutputNum = FReader.output then
     begin
       pSample.GetBufferAndLength(Buffer, Length);
       GetMem(SI, SizeOf(TSampleInfo));
       GetMem(SI.Data, Length);
       Move(Buffer^, SI.Data^, Length);
       SI.Length := Length;
       SI.Offset := 0;
       FReader.CriticalSection.Enter;
       FReader.BlockList.Add(Pointer(SI));
       FReader.CriticalSection.Leave;
       FReader.Event.SetEvent;
       FReader.Event.ResetEvent;
     end;
     Result := S_OK;
   end;

   constructor TWMStatusCallback.Create;
   begin
     inherited Create;
     FWriter := writer;
   end;

   function TWMStatusCallback.OnStatus;
   begin
     if Status = WMT_CLIENT_CONNECT_EX then
       FWriter.OnConnected(FWriter.Owner);
     if Status = WMT_CLIENT_DISCONNECT_EX then
       FWriter.OnDisconnected(FWriter.Owner);
     Result := S_OK;
   end;

   constructor TWMErrorCallback.Create;
   begin
     inherited Create;
     FWriter := writer;
   end;

   function TWMErrorCallback.OnStatus;
   begin
     if Status in [WMT_ERROR, WMT_NO_RIGHTS, WMT_NO_RIGHTS_EX, WMT_ERROR_WITHURL] then
        FWriter.OnError(FWriter.Owner, LongWord(Status));
     Result := S_OK;
   end;


  function lwma_network_writer_get_connections_count(var writer : wma_writer) : LongWord;
  var
    ClientConnections : IWMClientConnections;
  begin
    if Assigned(writer.network_sink) then
    begin
      ClientConnections := writer.network_sink as IWMClientConnections;
      ClientConnections.GetClientCount(Result);
    end else
       Result := 0;
  end;

  procedure lwma_network_writer_get_connection_info(var writer : wma_writer; connection : LongWord; var Address, Port : LongWord);
  var
    ClientConnections : IWMClientConnections2;
    CP : TWMClientProperties;
  begin
    if Assigned(writer.network_sink) then
    begin
      ClientConnections := writer.network_sink as IWMClientConnections2;
      ClientConnections.GetClientProperties(connection, CP);
      Address := CP.dwIPAddress;
      Port := CP.dwPort;
    end;
  end;

  procedure lwma_create_preprocessor(var writer : wma_writer; var preprocessor : wma_preprocessor);
  var
    res : HResult;
    MaxPasses : LongWord;
  begin
    FillChar(preprocessor, SizeOf(preprocessor), 0);
    res := writer.writer.BeginWriting;
    if res <> S_OK then
      raise EAuException.Create('Failed to set data writer: ' + IntToHex(res, 8));
    preprocessor.WMWriterPreprocess := writer.writer as IWMWriterPreprocess;
    res := preprocessor.WMWriterPreprocess.GetMaxPreprocessingPasses(writer.input, 0, MaxPasses);
    if res <> S_OK then
      raise EAuException.Create('Failed to get max passes: ' + IntToHex(res, 8));
    if MaxPasses = 0 then
      raise EAuException.Create('No double pass fr this format');
    res := preprocessor.WMWriterPreprocess.SetNumPreprocessingPasses(writer.input, 0, 1);
    if res <> S_OK then
      raise EAuException.Create('Failed to start data preprocessor: ' + IntToHex(res, 8));
  end;

  procedure lwma_writer_begin_preprocess(var writer : wma_writer; var preprocessor : wma_preprocessor);
  var
    res : HResult;
  begin
    writer.TotalTime := 0;
    res := preprocessor.WMWriterPreprocess.BeginPreprocessingPass(writer.input, 0);
    if res <> S_OK then
      raise EAuException.Create('Filed to start data preprocessor: ' + IntToHex(res, 8));

  end;

  procedure lwma_writer_preprocess(var writer : wma_writer; var preprocessor : wma_preprocessor; Buffer : Pointer; len : LongWord);
  var
    buf : PByte;
    res : HResult;
  begin
     writer.writer.AllocateSample(len, writer.buffer);
     writer.buffer.GetBuffer(buf);
     Move(Buffer^, Buf^, len);
     writer.buffer.SetLength(len);
     writer.TotalTime := writer.TotalTime + Round(len/writer.BytesPerSecond/0.1e-6);
     res := preprocessor.WMWriterPreprocess.PreprocessSample(writer.input, writer.TotalTime, 0, writer.buffer);
     if res < 0 then
       raise EAuException.Create('Failed to preprocess data: ' + IntToHex(res, 8));
     writer.buffer := nil;
   end;

   procedure lwma_preprocessor_free(var writer : wma_writer; var preprocessor : wma_preprocessor);
   begin
     preprocessor.WMWriterPreprocess.EndPreprocessingPass(writer.input, 0);
     writer.TotalTime := 0;
     preprocessor.WMWriterPreprocess := nil;
   end;


end.
