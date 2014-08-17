(*
  This file is a part of New Audio Components package v 2.5
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

*)

(* $Id: ACS_Vorbis.pas 1164 2010-01-31 08:47:53Z andrei.borovsky $ *)

unit ACS_Vorbis;

(* Title: ACS_Vorbis
    NewAC interface for Ogg Vorbis format. *)

{$DEFINE USE_VORBIS_11} // {$DEFINE USE_VORBIS_10} for old versions

interface

uses
  Classes, SysUtils, FastMove, ACS_Classes, ACS_Types, ACS_Tags, ACS_Procs, ogg, Codec, VorbisFile, VorbisEnc,
{$IFDEF LINUX}
  libc;
{$ENDIF}

{$IFDEF WIN32}
  Windows;
{$ENDIF}

const
  OUT_BUF_SIZE = $6000;
  BUF_SIZE = $3000;

type

  TVorbisBitRate = (brAutoSelect, bitrate24, bitrate32, bitrate45, bitrate48, bitrate56, bitrate64, bitrate80, bitrate96,
                 bitrate112, bitrate128, bitrate144, bitrate160, bitrate192, bitrate224, bitrate256, bitrate320, bitrate499);

  TBuffer = array[0..BUF_SIZE] of Byte;
  PBuffer = ^TBuffer;

  (* Class: TVorbisOut
      The Ogg Vorbis encoder component. Descends from <TAuFileOut>. More
      information on the Ogg Vorbis format may be found at
      http://xiph.org/vorbis/.

    Requires:
      - ogg.dll
      - vorbis.dll
      - vorbisenc.dll
      - vorbisfile.dll
  *)

  TVorbisOut = class(TAuFileOut)
  private
    Buffer : PSmallIntArray;
//   Buffer : array [0..OUT_BUF_SIZE - 1] of Byte;
    FSerial : Integer;
    FDesiredNominalBitrate : TVorbisBitRate;
    FDesiredMaximumBitrate : TVorbisBitRate;
    FMinimumBitrate : TVorbisBitRate;
    OggSS : ogg_stream_state;
    OggPg : ogg_page;
    OggPk : ogg_packet;
    VInfo : vorbis_info;
    VComm : vorbis_comment;
    Vdsp : vorbis_dsp_state;
    VBlock : vorbis_block;
    header, header_comm, header_code : ogg_packet;
    FCompression : Single;
//    FVendor : String;
    EndOfStream : Boolean;
    FComments : TVorbisTags;
    procedure SetComments(Value : TVorbisTags);
    procedure SetDesiredNominalBitrate(Value : TVorbisBitRate);
    procedure SetDesiredMaximumBitrate(Value : TVorbisBitRate);
    procedure SetMinimumBitrate(Value : TVorbisBitRate);
    procedure InitVorbis;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure SetFileMode(aMode : TFileOutputMode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Compression
      Set the compression ratio for the file being
      created. The valid values vary from -0.1 (maximum compression, lowest
      quality) to 1.0 (minimum compression, highest quality). If
      <DesiredNominalBitrate> property is assigned a value other than
      brAutoSelect, Compression property is ignored. *)
    property Compression : Single read FCompression write FCompression stored True;
   (* Property: Comments
      Add tags (comments) to an Ogg Vorbis file. The
      standard comments include Artist, Album, Title, Date, Genre, and Track. *)
    property Comments : TVorbisTags read FComments write SetComments;
   (* Property: DesiredMaximumBitrate
      Set the desired maximum bitrate limit for the file
      being created. The values of this property are brXXX constants,
      indicating bitrates in kbps. Depending on the parameters of the incoming
      audio data the actual maximum bitrate may be higher than that specified
      with this property. This property has an effect only if
      <DesiredNominalBitrate> property is assigned a value other than
      brAutoSelect. *)
    property DesiredMaximumBitrate : TVorbisBitRate read FDesiredMaximumBitrate write SetDesiredMaximumBitrate;
   (* Property: DesiredNominalBitrate
      If this property is set to a value other than brAutoSelect (the default
      value), the Compression property is ignored and the size/quality of the
      output file are determined by the values of <DesiredNominalBitrate>,
      <DesiredMaximumBitrate>, and <MinimumBitrate> properties. The values of
      this property are brXXX constants, indicating bitrates in kbps.
      Depending on the parameters of the incoming audio data the output file's
      actual nominal bitrate may be different from that specified with this
      property.

      Note:
      It is recommended by Ogg Vorbis developers to use the <Compression>
      property rather than specify bitrates directly. *)
    property DesiredNominalBitrate : TVorbisBitRate read FDesiredNominalBitrate write SetDesiredNominalBitrate;
    property FileMode;
   (* Property: MinimumBitrate
     Set the minimum bitrate limit for the file being
     created. The values of this property are brXXX constants, indicating
     bitrates in kbps. This property has an effect only if
     DesiredNominalBitrate property is assigned a value other than
     brAutoSelect.*)
    property MinimumBitrate : TVorbisBitRate read FMinimumBitrate write SetMinimumBitrate;
   (* Property: Serial
      Set the serial number of the logical bitstream in
      the Vorbis file. The value of this property is of concern only if you
      create multi-streamed Vorbis files (in foAppend mode). *)
    property Serial : Integer read FSerial write FSerial;
//    property Vendor : String read FVendor write FVendor;
  end;

  (* Class: TVorbisIn
    The Ogg  Vorbis decoder component. Descends from <TAuTaggedFileIn>. More
      information on the Ogg Vorbis format may be found at
      http://xiph.org/vorbis/.

    Requires:
      - ogg.dll
      - vorbis.dll
      - vorbisenc.dll
      - vorbisfile.dll
  *)

  TVorbisIn = class(TAuTaggedFileIn)
  private
    FComments : TVorbisTags;
//    FVendor : String;
    buf : array[1..BUF_SIZE] of Byte;
    VFile : OggVorbis_File;
    cursec : Integer;
    FMaxBitrate: Integer;
    FNominalBitrate: Integer;
    FMinBitrate : Integer;
    function GetMaxBitrate: Integer;
    function GetNominalBitrate: Integer;
    function GetMinBitrate : Integer;
    function GetComments : TVorbisTags;
    function GetBitStreams : Integer;
    function GetInstantBitRate : Integer;
    function GetCurrentBitStream : Integer;
    procedure SetCurrentBitStream(BS : Integer);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Property: BitStreams
      Read this property to get number of logical bitstreams in the
      multi-streamed file. By default the component plays all the bitstreams
      from the first to the last just as if they were the same bitstream. The
      playback time also refers to the total time of all the bitstreams. You
      need to handle bitstream-related properties only if you want to
      navigate between several bitstreams in a multi-streamed file. *)
    property BitStreams : Integer read GetBitStreams;
    (* Property: Comments
      Read tags (comments) added to an Ogg Vorbis file.
      The standard comments include Artist, Album, Title, Date, Genre, and
      Track. *)
    property Comments : TVorbisTags read GetComments;
    (* Property: CurrentBitStream
      Read this property to get the number of the current bitstream being
      played (0 < = CurrentBitStream < BitStreams). Assigning a value to this
      property makes the component start playback from the beginning of the
      specified logical bitstream. This property can be used only during
      actual playback process. *)
    property CurrentBitStream : Integer read GetCurrentBitStream write SetCurrentBitStream;
    (* Property: InstantBitRate
      Get current bitrate (in bits per second) of the VBR-encoded Vorbis file. *)
    property InstantBitRate : Integer read GetInstantBitRate;
    //property Vendor : String read FVendor;
    (* Property: MaxBitrate
      Get the maximum bitrate (in bits per second) of the Vorbis file. *)
    property MaxBitrate: Integer read GetMaxBitrate;
    (* Property: MinBitrate
      Get the minimum bitrate (in bits per second) of the Vorbis file. *)
    property MinBitrate: Integer read GetMinBitrate;
    (* Property: NominalBitrate
      Get the nominal bitrate (in bits per second) of the Vorbis file. *)
    property NominalBitrate: Integer read GetNominalBitrate;
  published
    property EndSample;
    property StartSample;
  end;

implementation

  function cbRead(ptr : Pointer; size, nmemb : Integer; datasource : Pointer) : Integer; cdecl;
  var
    VI : TVorbisIn;
    Buffer : array of Byte;
  begin
    VI := TVorbisIn(datasource);
    SetLength(Buffer, size*nmemb);
    Result :=  VI.FStream.Read(Buffer[0], size*nmemb);
    Move(Buffer[0], ptr^, Result);
    Buffer := nil;
  end;

  function cbSeek(datasource : Pointer; offset : ogg_int64_t; whence : Integer) : Integer; cdecl;
  var
    VI : TVorbisIn;
    Origin : TSeekOrigin;
  begin
    VI := TVorbisIn(datasource);
    if not VI.Seekable then
    begin
      Result := -1;
      Exit;
    end;  
    case whence of
      SEEK_SET : Origin := TSeekOrigin(soFromBeginning);
      SEEK_CUR : Origin := TSeekOrigin(soFromCurrent);
      SEEK_END : Origin := TSeekOrigin(soFromEnd);
      else Origin := TSeekOrigin(soFromBeginning);
    end;
    Result := VI.FStream.Seek(offset, Origin);
  end;

  function cbClose(datasource : Pointer) : Integer; cdecl;
  var
    VI : TVorbisIn;
  begin
    VI := TVorbisIn(datasource);
    if not VI.FStreamAssigned then VI.FStream.Free
    else VI.FStream.Seek(0, soFromBeginning);

    Result := 0;
  end;

  function cbTell(datasource : Pointer) : Integer; cdecl;
  var
    VI : TVorbisIn;
  begin
    VI := TVorbisIn(datasource);
    Result := VI.FStream.Position
  end;

  function VorbisBitrateToInt(Bitrate : TVorbisBitrate) : Integer;
  begin
    case Bitrate of
      bitrate24 : Result := 24000;
      bitrate32 : Result := 32000;
      bitrate45 : Result := 45000;
      bitrate48 : Result := 48000;
      bitrate56 : Result := 56000;
      bitrate64 : Result := 64000;
      bitrate80 : Result := 80000;
      bitrate96 : Result := 96000;
      bitrate112 : Result := 112000;
      bitrate128 : Result := 128000;
      bitrate144 : Result := 144000;
      bitrate160 : Result := 160000;
      bitrate192 : Result := 192000;
      bitrate224 : Result := 224000;
      bitrate256 : Result := 256000;
      bitrate320 : Result := 320000;
      bitrate499 : Result := 499000;
      else Result := -1;
    end;
  end;

  constructor TVorbisOut.Create;
  begin
    inherited Create(AOwner);
//    Self.Thread.bSuspend := True;
    FCompression := 0.2;
    FComments := TVorbisTags.Create;
    FDesiredNominalBitrate := brAutoSelect;
    FDesiredMaximumBitrate := brAutoSelect;
    FMinimumBitrate := brAutoSelect;
  end;

  destructor TVorbisOut.Destroy;
  begin
    FComments.Free;
//    UnloadOggLib;
//  UnloadCodecLib;
//    UnloadVorbisFileLib;
//    UnloadVorbisEncLib;
    inherited Destroy;
  end;

  procedure TVorbisOut.SetComments;
  begin
    FComments.Assign(Value);
  end;

  procedure TVorbisOut.Prepare;
  begin
    LoadOggLib;
    LoadCodecLib;
    LoadVorbisFileLib;
    LoadVorbisEncLib;
    if not LiboggLoaded then
    raise EAuException.Create(LiboggPath + ' library could not be loaded.');
    if not LibvorbisLoaded then
    raise EAuException.Create(LibvorbisPath + ' library could not be loaded.');
    if not LibvorbisfileLoaded then
    raise EAuException.Create(LibvorbisfilePath + ' library could not be loaded.');
    if not LibvorbisencLoaded then
    raise EAuException.Create(LibvorbisencPath + ' library could not be loaded.');
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      if (not FileExists(FWideFileName)) or (FFileMode = foRewrite) then
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or FShareMode, FAccessMask)
      else FStream := TAuFileStream.Create(FWideFileName, fmOpenReadWrite or FShareMode, FAccessMask);
    end;
    FInput.Init;
    if FFileMode = foAppend then
    FStream.Seek(0, soFromEnd);
    EndOfStream := False;
   //_FirstCall := True;
//   Thread.bSuspend := True;
   InitVorbis;
  end;

  procedure TVorbisOut.Done;
  begin
    if not FStreamAssigned then
    FStream.Free;
    FInput.Flush;
    FComments.Clear;
    ogg_stream_clear(OggSS);
    vorbis_block_clear(VBlock);
    vorbis_dsp_clear(Vdsp);
    vorbis_comment_clear(@VComm);
    vorbis_info_clear(@VInfo);
  end;

  function TVorbisOut.DoOutput;
  var
    i, j, SamplesRead, BytesPerSample : LongWord;
    Len : LongWord;
    out_buf : PPFloat;
    tmpBuf : array[0..16] of PFloat;
    buf_u : PBuffer8;
    Ptr : Pointer;
    wres : Integer;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    if Abort or EndOfStream then
    begin
      (* We don't close file here to avoid exceptions
        if output component's Stop method is called *)
      Result := False;
      Exit;
    end;
    Len := OUT_BUF_SIZE;
    Finput.GetData(Ptr, Len);
    Buffer := Ptr;
    BytesPerSample := FInput.Channels * FInput.BitsPerSample div 8;
    SamplesRead := Len div BytesPerSample;
    out_buf := vorbis_analysis_buffer(Vdsp, OUT_BUF_SIZE);
    if Len <> 0 then
    begin
      if Finput.BitsPerSample = 16 then
      begin
        tmpBuf[0] := out_buf^;
        for j := 1 to FInput.Channels - 1 do
        begin
          Inc(out_buf);
          tmpBuf[j] := out_buf^;
        end;
        for i:=0 to SamplesRead - 1 do
          for j := 0 to Finput.Channels - 1 do
            tmpBuf[j][i] := Buffer[i*FInput.Channels + j]/$8000;
      end else  // if Finput.BitsPerSample = 16
      begin
        if Finput.BitsPerSample = 8 then
        begin
          buf_u := @Buffer[0];
          tmpBuf[0] := out_buf^;
          for j := 1 to FInput.Channels - 1 do
          begin
            Inc(out_buf);
            tmpBuf[j] := out_buf^;
          end;
          for i := 0 to SamplesRead - 1 do
            for j := 0 to FInput.Channels - 1 do
              tmpBuf[j][i] := (buf_u[i*Finput.Channels + j] - 128)/128;
        end else
        begin
          buf_u := @Buffer[0];
          tmpBuf[0] := out_buf^;
          for j := 1 to FInput.Channels - 1 do
          begin
            Inc(out_buf);
            tmpBuf[j] := out_buf^;
          end;
          for i := 0 to SamplesRead - 1 do
            for j := 0 to FInput.Channels - 1 do
              tmpBuf[j][i] := ((PSmallInt(@buf_u[i*BytesPerSample + j*3 + 1])^ shl 8)
               + buf_u[i*BytesPerSample + j*3])/8388608;
        end;
      end;
      vorbis_analysis_wrote(Vdsp, SamplesRead);
    end else // if Len <> 0
    vorbis_analysis_wrote(Vdsp, 0);
    while vorbis_analysis_blockout(Vdsp, VBlock) = 1 do
    begin
      vorbis_analysis(VBlock, nil);
      vorbis_bitrate_addblock(VBlock);
      while vorbis_bitrate_flushpacket(Vdsp, OggPk) <> 0 do
      begin
        ogg_stream_packetin(OggSS, OggPk);
        while not EndOfStream do
        begin
          if ogg_stream_pageout(OggSS, OggPg) = 0 then Break;
          wres := FStream.Write(OggPg.header^, OggPg.header_len);
          if wres <> OggPg.header_len then
            raise EAuException.Create('Error writing ogg file');
          wres := FStream.Write(OggPg.body^, OggPg.body_len);
          if wres <> OggPg.body_len then
            raise EAuException.Create('Error writing ogg file');
          if ogg_page_eos(OggPg) <> 0 then EndOfStream := True;
        end;
      end;
    end;
  end;

  constructor TVorbisIn.Create;
  begin
    inherited Create(AOwner);
    FComments := TVorbisTags.Create;
  end;

  destructor TVorbisIn.Destroy;
  begin
    FComments.Free;
//    UnloadOggLib;
//    UnloadCodecLib;
//    UnloadVorbisFileLib;
 //   UnloadVorbisEncLib;
    inherited Destroy;
  end;

  procedure TVorbisIn.OpenFile;
  var
    PVComm : PVORBIS_COMMENT;
    PVInfo : PVORBIS_INFO;
    PComment : PPAnsiChar;
    Comment : PAnsiChar;
    Callbacks : OV_CALLBACKS;
    res : Integer;
    CN, CV : String;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      LoadOggLib;
      LoadCodecLib;
      LoadVorbisFileLib;
      LoadVorbisEncLib;
      if not LiboggLoaded then
      raise EAuException.Create(LiboggPath + ' library could not be loaded.');
      if not LibvorbisLoaded then
      raise EAuException.Create(LibvorbisPath + ' library could not be loaded.');
      if not LibvorbisfileLoaded then
      raise EAuException.Create(LibvorbisfilePath + ' library could not be loaded.');
      if not LibvorbisencLoaded then
      raise EAuException.Create(LibvorbisencPath + ' library could not be loaded.');
      FValid := True;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        FValid := False;
        raise EAuException.Create('Failed to open stream');
      end;
      Callbacks.read_func := cbRead;
      Callbacks.close_func := cbClose;
      Callbacks.seek_func := cbSeek;
      Callbacks.tell_func := cbTell;
      res := ov_test_callbacks(Self, VFile, nil, 0, Callbacks);
      if res <> 0 then
        raise EAuException.Create('Failed to open an ogg file: ' + IntToStr(res));
      ov_test_open(VFile);
//      ov_open_callbacks(Self, VFile, nil, 0, Callbacks);
      FComments.Clear;
      PVComm := ov_comment(VFile, -1);
      PComment := PVComm.user_comments;
      Comment := PComment^;
      {$WARNINGS OFF}
      while Comment <> nil do
      begin
        CN := GetLeftOf('=', AnsiString(Comment));
        CV := GetRightOf('=', AnsiString(Comment));
        CN := AnsiLowerCase(CN);
        if CN = _vorbis_Artist then
          FComments.Artist := UTF8Decode(CV)
        else
        if CN = _vorbis_Album then
          FComments.Album := UTF8Decode(CV)
        else
        if CN = _vorbis_Title then
          FComments.Title := UTF8Decode(CV)
        else
        if CN = _vorbis_Date then
          FComments.Date := UTF8Decode(CV)
        else
        if CN = _vorbis_Genre then
          FComments.Genre := UTF8Decode(CV)
        else
        if CN = _vorbis_Track then
          FComments.Track := UTF8Decode(CV);
        Inc(LongWord(PComment), 4);
        Comment := PComment^;
      end;
      _CommonTags.Clear;
      _CommonTags.Artist := FComments.Artist;
      _CommonTags.Album := FComments.Album;
      _CommonTags.Title := FComments.Title;
      _CommonTags.Year := FComments.Date;
      _CommonTags.Track := FComments.Track;
      _CommonTags.Genre := FComments.Genre;
     {$WARNINGS ON}
      //      FVendor := PVComm.vendor;
      PVInfo := ov_info(VFile, -1);
      FChan := PVInfo.channels;
      FSR := PVInfo.rate;
      FBPS := 16;
      FMaxBitrate := PVInfo.bitrate_upper;
      FNominalBitrate := PVInfo.bitrate_nominal;
      FMinBitrate := PVInfo.bitrate_lower;
      FSize := (ov_pcm_total(VFile, -1) shl 1) * PVInfo.channels;
      cursec := -1;
  //    ov_pcm_seek(VFile, FOffset);
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TVorbisIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      ov_clear(VFile);
 //     if not FStreamAssigned then
 //        FStream.Free;
      FOpened  := 0;
    end;  
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TVorbisIn.GetDataInternal;
  var
    l : Integer;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      BufEnd := 0;
      if not _EndOfStream then
      begin
        (* The ov_read function can return data in quite small chunks (of
        about 512 bytes). We keep reading data until the buffer is filled or
        there is no more data to read. *)
        while BufEnd < BUF_SIZE do
        begin
          l := ov_read(VFile, @buf[BufEnd + 1], BUF_SIZE - BufEnd, 0, 2, 1, @cursec);
          if l <= 0 then
          begin
            _EndOfStream := True;
            Break;
          end;
          Inc(BufEnd, l);
          if (FPosition + BufEnd) >= FSize then
          begin
            BufEnd := FSize - FPosition;
            if BufEnd <= 0 then _EndOfStream := True;
            Break;
          end;
        end;
      end;
    end;
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    Buffer := @Buf[BufStart];
    Inc(BufStart, Bytes);
  end;

  function TVorbisIn.GetMaxBitrate;
  begin
    OpenFile;
    Result := FMaxBitrate;
//    CloseFile;
  end;

  function TVorbisIn.GetNominalBitrate;
  begin
    OpenFile;
    Result := FNominalBitrate;
//    CloseFile;
  end;

  function TVorbisIn.GetComments;
  begin
    OpenFile;
    Result := FComments;
//    CloseFile;
  end;

  function TVorbisIn.GetMinBitrate;
  begin
    OpenFile;
    Result := FMinBitrate;
//    CloseFile;
  end;

  procedure TVorbisOut.SetFileMode;
  begin
    FFileMode := aMode;
  end;

  function TVorbisIn.GetBitStreams;
  begin
    Result := 0;
    if Busy then
    begin
      if ov_seekable(VFile)<>0 then
      Result := ov_streams(VFile);
    end;
  end;

  function TVorbisIn.GetInstantBitRate;
  begin
    Result := 0;
    if Busy then
    begin
      Result := ov_bitrate_instant(VFile);
    end;
  end;

  function TVorbisIn.GetCurrentBitStream;
  begin
    Result := -1;
    if Busy then
    begin
      if ov_seekable(VFile)<>0 then
      Result := VFile.current_link;
    end;
  end;

  procedure TVorbisIn.SetCurrentBitStream;
  var
    Offset : POGG_INT64_T;
  begin
    if Busy then
    begin
      if ov_seekable(VFile)<>0 then
      if (BS >= 0) and (BS < ov_streams(VFile)) then
      begin
        Offset := VFile.offsets;
        Inc(Offset, BS);
        FStream.Seek(Offset^, soFromBeginning);
      end;
    end;
  end;

  procedure TVorbisOut.SetDesiredNominalBitrate;
  begin
    FDesiredNominalBitrate := Value;
    if FMinimumBitrate > FDesiredNominalBitrate then
    FMinimumBitrate := FDesiredNominalBitrate;
    if FDesiredMaximumBitrate < FDesiredNominalBitrate then
    FDesiredMaximumBitrate := FDesiredNominalBitrate;
    if FDesiredNominalBitrate = brAutoSelect then
    FDesiredMaximumBitrate := brAutoSelect;
  end;

  procedure TVorbisOut.SetDesiredMaximumBitrate;
  begin
    if FDesiredNominalBitrate = brAutoSelect then Exit;
    if (Value = brAutoSelect) or (Value >= FDesiredNominalBitrate) then
    FDesiredMaximumBitrate := Value;
  end;

  procedure TVorbisOut.SetMinimumBitrate;
  begin
    if Value <= FDesiredNominalBitrate then
    FMinimumBitrate := Value;
  end;

  function TVorbisIn.SeekInternal;
  begin
    Result := True;
    OpenFile;
    ov_pcm_seek(VFile, SampleNum);
//    CloseFile;
    BufStart := 1;
    BufEnd := 0;
  end;

  procedure TVorbisOut.InitVorbis;
  var
    i, maxbr, minbr, nombr : Integer;
    Name, Value : AnsiString;
  begin
    vorbis_info_init(@VInfo);
    if DesiredNominalBitrate = brAutoSelect then
    begin
(*      {$IFNDEF USE_VORBIS_10}
      if vorbis_encode_init_vbr(@VInfo, FInput.Channels, FInput.SampleRate, FCompression) <> 0 then
        raise EACSException.Create('Vorbis init failed');
      if vorbis_encode_setup_init(@VInfo) <> 0 then
        raise EACSException.Create('Vorbis setup failed');
      {$ENDIF} *)
      {$IFDEF USE_VORBIS_11}
      vorbis_encode_setup_vbr(@VInfo, FInput.Channels, FInput.SampleRate, FCompression);
      vorbis_encode_setup_init(@VInfo);
      {$ENDIF}
    end else
    begin
      nombr := VorbisBitrateToInt(FDesiredNominalBitrate);
      maxbr := VorbisBitrateToInt(FDesiredMaximumBitrate);
      //if maxbr < nombr then maxbr := nombr;
      minbr := VorbisBitrateToInt(Self.FMinimumBitrate);
      if minbr < 0 then minbr := nombr;
      if vorbis_encode_init(@VInfo, FInput.Channels, FInput.SampleRate, maxbr, nombr, minbr) <> 0 then
        raise EAuException.Create('Vorbis codec setup with the requested bitrate failed. Try a lower bitrate.');

    end;
    vorbis_comment_init(@VComm);
    for i := 0 to FComments.IdCount - 1 do
    begin
      Name := Utf8Encode(WideString(FComments.Ids[i]));
      Value := Utf8Encode(FComments.AsWideString[FComments.Ids[i]]);
      if Value <> '' then
        vorbis_comment_add_tag(@VComm, PAnsiChar(@Name[1]), PAnsiChar(@Value[1]));
    end;
    vorbis_analysis_init(Vdsp, VInfo);
    vorbis_block_init(Vdsp, VBlock);
    ogg_stream_init(OggSS, FSerial);
    vorbis_analysis_headerout(Vdsp, VComm, header, header_comm, header_code);
    ogg_stream_packetin(OggSS, header);
    ogg_stream_packetin(OggSS, header_comm);
    ogg_stream_packetin(OggSS, header_code);
    while ogg_stream_flush(OggSS, OggPg) <> 0 do
    begin
      FStream.Write(OggPg.header^, OggPg.header_len);
      FStream.Write(OggPg.body^, OggPg.body_len);
    end;
  end;


  const
    CW = DWord($133f);

initialization
  Set8087CW(CW);
end.
