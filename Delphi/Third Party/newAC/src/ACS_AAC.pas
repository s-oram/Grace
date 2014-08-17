(*
  This file is a part of New Audio Components package v 2.6
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_AAC.pas 1245 2010-07-22 09:45:33Z andrei.borovsky $ *)

unit ACS_AAC;

(* Title: ACS_AAC
    Components for the AAC playback. *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, ACS_WinMedia, mp4ff, neaac;


type

(* Class: TMP4In
   This component can decode AAC audio from M4A and MP4 files. It requires mp4ff.dll (derived from the faad package) and libfaad2p.dll.
   Descends from <TAuTaggedFileIn>.
   Exact positioning isn't implemented yet.
 *)

  TMP4In = class(TAuTaggedFileIn)
  private
      MP4Handle : mp4ff_t;
      cbs : mp4ff_callback_t;
      HDecoder : NeAACDecHandle;
      FTrack : Integer;
      FBytesRead, FBOffset : LongWord;
      FSampleId, FSamples : Integer;
      FTimescale : LongWord;
      _Buf : array[0..2024*1024-1] of Byte;
      FBitrate : LongWord;
      function GetBitrate : LongWord;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
      (* Property: Bitrate
       Read this property to get the file's average bitrate. *)
     property Bitrate : LongWord read GetBitrate;
  end;

  TAACBuffer = record
    bytes_into_buffer : Integer;
    bytes_consumed : Integer;
    file_offset : Integer;
    buffer : PByteArray;
    at_eof : Boolean;
    infile : TStream;
  end;

(* Class: TAACIn
   This component can decode raw AAC files. It requires libfaad2p.dll.
   The component is experimenatal and will probably not work for all types of AAC file.
   Please send me samples of the files that it cannot play.
   Descends from <TAuTaggedFileIn>.
   Seeking isn't implemented yet.
 *)

  TAACIn = class(TAuTaggedFileIn)
  private
      b : TAACBuffer;
      HDecoder : NeAACDecHandle;
      FBytesRead, FBOffset : LongWord;
      _Buf : array[0..FAAD_MIN_STREAMSIZE*8-1] of Byte;
      Buf2 : array[0..1024*1024-1] of Byte;
      FBitrate : LongWord;
      function GetBitrate : LongWord;
      procedure ReadAACSamples;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
//    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
      (* Property: Bitrate
       Read this property to get the file's average bitrate. *)
     property Bitrate : LongWord read GetBitrate;
  end;


implementation

  function CBMP4Read(user_data : Pointer; buffer : Pointer; length : LongWord) : LongWord; cdecl;
  begin
    try
      Result := LongWord(TMP4In(user_data).FStream.Read(buffer^, Integer(length)));
    except
      Result := 0;
    end;
  end;

  function CBMP4Write(user_data : Pointer; buffer : Pointer; length : LongWord) : LongWord; cdecl;
  begin
    try
      Result := LongWord(TMP4In(user_data).FStream.Write(buffer^, Integer(length)));
    except
      Result := 0;
    end;
  end;

  function CBMP4Seek(user_data : Pointer; Position : Int64) : LongWord; cdecl;
  begin
    try
      Result := LongWord(TMP4In(user_data).FStream.Seek(Position, soBeginning));
    except
      Result := 0;
    end;
  end;

  function CBMP4Truncate(user_data : Pointer) : LongWord; cdecl;
  begin
      Result := 0;
  end;


  procedure TMP4In.OpenFile;
  var
    config : NeAACDecConfigurationPtr;
    buffer : PByte;
    buffer_size : LongWord;
    samplerate : LongWord;
    channels : Byte;
//    useAacLength : LongWord;
//    initial : LongWord;
//    framesize : LongWord;
    mp4ASC : mp4AudioSpecificConfig;
    f, seconds : Double;
    tag : PAnsiChar;
  begin
    Loadmp4ff;
    if not Libmp4ffLoaded then
    raise EAuException.Create(Libmp4ffPath + ' library could not be loaded.');
    Loadneaac;
    if not LibneaacLoaded then
    raise EAuException.Create(LibneaacPath + ' library could not be loaded.');
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      cbs.read := CBMP4Read;
      cbs.write := CBMP4Write;
      cbs.seek := CBMP4Seek;
      cbs.truncate := CBMP4Truncate;
      cbs.user_data := Pointer(Self);
      hDecoder := NeAACDecOpen();
      config := NeAACDecGetCurrentConfiguration(hDecoder);
      case config.outputFormat of
       FAAD_FMT_16BIT : FBPS := 16;
       FAAD_FMT_24BIT : FBPS := 24;
       FAAD_FMT_32BIT : FBPS := 32;
       else  raise EAuException.Create('Unsupported sample format.');
      end;
      config.downMatrix := 1; //!!!!
      NeAACDecSetConfiguration(hDecoder, config);
      MP4Handle := mp4ff_open_read(@cbs);
      if MP4Handle = nil then
        raise EAuException.Create('Unable to open file.');
      FTrack := GetAACTrack(MP4Handle);
      if FTrack < 0 then
        raise EAuException.Create('Unable to find correct AAC sound track in the MP4 file.');
      buffer := nil;
      mp4ff_get_decoder_config(MP4Handle, FTrack, buffer, buffer_size);
      NeAACDecInit2(HDecoder, buffer, buffer_size, samplerate, channels);
      FSR := mp4ff_get_sample_rate(MP4Handle, FTrack); //samplerate;
      FChan := mp4ff_get_channel_count(MP4Handle, FTrack);
      FTimescale := mp4ff_time_scale(MP4Handle, FTrack);
//      framesize := 1024;
//      useAacLength := 0;
      if buffer <> nil then
      begin
        NeAACDecAudioSpecificConfig(buffer, buffer_size, mp4ASC);
        //if mp4ASC.frameLengthFlag = 1 then framesize := 960;
        //if mp4ASC.sbr_present_flag = 1 then framesize := framesize*2;
        mp4ff_free_decoder_config(buffer);
      end;
      FSamples := mp4ff_num_samples(MP4Handle, FTrack);
      f := 1024.0;
      if mp4ASC.sbr_present_flag = 1 then
         f := f * 2.0;
      seconds := FSamples*(f-1.0)/mp4ASC.samplingFrequency;
      //FSR :=  mp4ASC.samplingFrequency;
      FTotalSamples := Trunc(seconds*mp4ASC.samplingFrequency);
      if mp4ASC.samplingFrequency > FSR then FSR := mp4ASC.samplingFrequency;
      FSize := FTotalSamples*FChan*(FBPS div 8);
      if Fsize > 0 then
        FSeekable := True;
      (*
        j = mp4ff_meta_get_num_items(infile);
        for (k = 0; k < j; k++)
        {
            if (mp4ff_meta_get_by_index(infile, k, &item, &tag))
            {
                if (item != NULL && tag != NULL)
                {
                    faad_fprintf(stderr, "%s: %s\n", item, tag);
                    free(item); item = NULL;
                    free(tag); tag = NULL;
                }
            }
        *)
      _CommonTags.Clear;
      mp4ff_meta_get_artist(MP4Handle, tag);
      _CommonTags.Artist := tag;
      mp4ff_meta_get_title(MP4Handle, tag);
      _CommonTags.Title := tag;
      mp4ff_meta_get_album(MP4Handle, tag);
      _CommonTags.Album := tag;
      mp4ff_meta_get_date(MP4Handle, tag);
      _CommonTags.Year := tag;
      mp4ff_meta_get_genre(MP4Handle, tag);
      _CommonTags.Genre := tag;
      mp4ff_meta_get_track(MP4Handle, tag);
      _CommonTags.Track := tag;
      FBitrate := mp4ff_get_avg_bitrate(MP4Handle, FTrack);
      FValid := True;
      FBOffset := 0;
      FBytesRead := 0;
      FSampleId := 0;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TMP4In.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    dur : LongWord;
    rc : Integer;
    audio_buffer : PByte;
    audio_buffer_size : LongWord;
    sample_buffer : Pointer;
    frameInfo : NeAACDecFrameInfo;
    sample_count : LongWord;
  begin
    if FSampleId > FSamples then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    if FBOffset = FBytesRead then
    begin
      frameInfo.samples := 0;
      while frameInfo.samples = 0 do
      begin
        dur := mp4ff_get_sample_duration(MP4Handle, FTrack, FSampleId);
        audio_buffer_size := 0;
        audio_buffer := nil;
        rc := mp4ff_read_sample(MP4Handle, FTrack, FSampleId, audio_buffer,  audio_buffer_size);
        if rc = 0 then
        begin
          if FPosition >= FSize then
          begin
            Buffer := nil;
            Bytes := 0;
            FSampleId := FSamples + 1;
            Exit;
          end;
          raise EAuException.Create('Error reading data.');
        end;
        sample_buffer := NeAACDecDecode(hDecoder, @frameInfo, audio_buffer, audio_buffer_size);
        if audio_buffer <> nil then mp4ff_free_decoder_config(audio_buffer);
        if frameInfo.error <> 0 then
          raise EAuException.Create('Error reading data.');

                (*                        if (!noGapless)
                {
                        if (sampleId == 0) dur = 0;

                        if (useAacLength || (timescale != samplerate)) {
                                sample_count = frameInfo.samples;
                        } else {
                                sample_count = (unsigned int)(dur * frameInfo.channels);
                                if (sample_count > frameInfo.samples)
                                        sample_count = frameInfo.samples;

                                if (!useAacLength && !initial && (sampleId < numSamples/2) && (sample_count != frameInfo.samples))
                                {
                                        faad_fprintf(stderr, "MP4 seems to have incorrect frame duration, using values from AAC data.\n");
                                        useAacLength = 1;
                                        sample_count = frameInfo.samples;
                                }
                        }

                        if (initial && (sample_count < framesize*frameInfo.channels) && (frameInfo.samples > sample_count))
                                delay = frameInfo.samples - sample_count;
                } else {
                        sample_count = frameInfo.samples;
                } *)

        sample_count := frameInfo.samples;
        //  sample_count := (dur * frameInfo.channels);
        FBytesRead := sample_count*(FBPS div 8);
        Move(sample_buffer^, _Buf[0], FBytesRead);
        FBOffset := 0;
        Inc(FSampleId);
      end;  // while frameInfo.samples = 0 do
    end; // if FBOffset = FBytesRead then
    if Bytes > FBytesRead - FBOffset then
      Bytes := FBytesRead - FBOffset;
    Buffer := @_Buf[FBOffset];
    Inc(FBOffset, Bytes);
  end;

  procedure TMP4In.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      NeAACDecClose(hDecoder);
      mp4ff_close(MP4Handle);
      if not FStreamAssigned then
         FStream.Free;
      FOpened  := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  function TMP4In.SeekInternal(var SampleNum: Int64) : Boolean;
  var
    aac_sample : Integer;
  begin
    Result := False;
    if (FSamples= 0) or (FTotalSamples = 0) then Exit;
    if FSeekable then
    begin
      if SampleNum > FTotalSamples then SampleNum := FTotalSamples;
      aac_sample := Trunc(SampleNum/FTotalSamples*FSamples);
      Self.FSampleId := aac_sample;
//      aac_sample := Trunc(SampleNum/FTotalSamples*FSize);
//      FPosition := FStream.Seek(aac_sample, soFromBeginning);
//      mp4ff_find_sample(MP4Handle, FTrack, 0, aac_sample);
//      mp4ff_set_sample_position(MP4Handle, FTrack,aac_sample);
      FPosition := Trunc(FSize*aac_sample/FSamples);
       //mp4ff_get_sample_position(MP4Handle, FTrack, aac_sample);
      Self.FBOffset := 0;
      Self.FBytesRead := 0;
      SampleNum := FPosition div (FChan*(FBPS div 8));
      Result := True;
    end;
  end;

  function TMP4In.GetBitrate : LongWord;
  begin
    OpenFile;
    Result := FBitrate;
  end;

  function memcmp(B1 : PByteArray; B2 : PAnsiChar; Length : Integer) : Integer;
  var
    i : Integer;
  begin
    Result := 0;
    for i := 0 to Length-1 do
      if B1[i] <> Byte(B2[i]) then
        Result := -1;
  end;

 function fill_buffer(var b : TAACBuffer) : Integer;
 var
   bread : Integer;
   i : Integer;
 begin
    if b.bytes_consumed > 0 then
    begin
      if b.bytes_into_buffer <> 0 then
      begin
        for i := 0 to b.bytes_into_buffer - 1 do
            b.buffer[i] := b.buffer[i+b.bytes_consumed];
      end;
      if not b.at_eof then
      begin
        bread := b.infile.Read(b.buffer[b.bytes_into_buffer], b.bytes_consumed);
        if bread <> b.bytes_consumed then
        begin
          b.at_eof := True;
        end;
        Inc(b.bytes_into_buffer, bread);
      end;
      b.bytes_consumed := 0;
      if b.bytes_into_buffer > 3 then
        if memcmp(b.buffer, 'TAG', 3) = 0 then
              b.bytes_into_buffer := 0;
      if b.bytes_into_buffer > 11 then
        if memcmp(b.buffer, 'LYRICSBEGIN', 11) = 0 then
          b.bytes_into_buffer := 0;
      if b.bytes_into_buffer > 8 then
        if memcmp(b.buffer, 'APETAGEX', 8) = 0 then
          b.bytes_into_buffer := 0;
    end;
    Result := 1;
 end;

 procedure advance_buffer(var b : TAACBuffer; bytes : Integer);
 begin
    Inc(b.file_offset, bytes);
    b.bytes_consumed := bytes;
    Dec(b.bytes_into_buffer, bytes);
  	if b.bytes_into_buffer < 0 then
	  	b.bytes_into_buffer := 0;
 end;

var
  adts_sample_rates : array [0..15] of Integer = (96000,88200,64000,48000,44100,32000,24000,22050,16000,12000,11025,8000,7350,0,0,0);

function adts_parse(var b : TAACBuffer; var bitrate : Integer; var length : Double) : Integer;
var
  frames, frame_length : Integer;
  t_framelength : Integer;
  samplerate : Integer;
  frames_per_sec, bytes_per_frame : Double;
begin
  t_framelength := 0;
  frames := 0;
  while frames >= 0 do
  begin
    fill_buffer(b);
    if b.bytes_into_buffer > 7 then
    begin
      if not ((b.buffer[0] = $FF) and ((b.buffer[1] and $F6) = $F0)) then  break;
      if frames = 0 then samplerate := adts_sample_rates[(b.buffer[2] and $3c) div 4];
      frame_length := (LongWord(b.buffer[3] and 3) shr 11) or (LongWord(b.buffer[4]) shr 3) or (b.buffer[5] shl 5);
      Inc(t_framelength,frame_length);
      if frame_length > b.bytes_into_buffer then break;
      advance_buffer(b, frame_length);
    end else  break;
    Inc(frames);
  end;
  frames_per_sec := samplerate/1024;
  if frames <> 0 then
    bytes_per_frame := t_framelength/(frames*1000)
  else
    bytes_per_frame := 0;
  bitrate := Trunc(8*bytes_per_frame*frames_per_sec + 0.5);
  if frames_per_sec <> 0 then
    length := frames/frames_per_sec
  else
    length := 1;
  Result := 1;
end;


  procedure TAACIn.OpenFile;
  var
    config : NeAACDecConfigurationPtr;
    mp4ASC : mp4AudioSpecificConfig;
    bread, tagsize : Integer;
    bitrate, header_type, skip_size : Integer;
    length  : Double;
    samplerate : LongWord;
    channels : Byte;
  begin
    Loadneaac;
    if not LibneaacLoaded then
    raise EAuException.Create(LibneaacPath + ' library could not be loaded.');
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      b.bytes_into_buffer := 0;
      b.bytes_consumed := 0;
      b.file_offset := 0;
      b.buffer := @_Buf[0];
      b.at_eof := False;
      b.infile := FStream;
      bread := b.infile.Read(b.buffer[0], FAAD_MIN_STREAMSIZE*8);
      b.bytes_into_buffer := bread;
      b.bytes_consumed := 0;
      b.file_offset := 0;
      if bread <> FAAD_MIN_STREAMSIZE*8 then
         b.at_eof := True;
      tagsize := 0;
      if memcmp(b.buffer, 'ID3', 3) = 0 then
      begin
//      tagsize := ((b.buffer[6] and 127) shr 21) or ((b.buffer[7] and 127) shr 14) or
//          ((b.buffer[8] and 127) shr  7) or ((b.buffer[9] and 127) shr  0);
        tagsize := (b.buffer[8] and 127)*127 + (b.buffer[9] and 127);
        Inc(tagsize, 11);
        advance_buffer(b, tagsize);
        fill_buffer(b);
      end;
 {    tagsize := 196;
      advance_buffer(b, tagsize);
      fill_buffer(b);}
      hDecoder := NeAACDecOpen();
      config := NeAACDecGetCurrentConfiguration(hDecoder);
{      config.defSampleRate := 44100;
      config.defObjectType := LC;
      config.outputFormat := FAAD_FMT_16BIT;
      config.downMatrix := 0;
      config.useOldADTSFormat := 0;
      NeAACDecSetConfiguration(hDecoder, config);}
      if (b.buffer[0] = $FF) and ((b.buffer[1] and $F6) = $F0) then
      begin
        adts_parse(b, bitrate, length);
        b.infile.Seek(tagsize, soFromBeginning);
        bread := b.infile.Read(b.buffer[0], FAAD_MIN_STREAMSIZE*8);
        if bread <> FAAD_MIN_STREAMSIZE*8 then
            b.at_eof := True
        else
            b.at_eof := False;
        b.bytes_into_buffer := bread;
        b.bytes_consumed := 0;
        b.file_offset := tagsize;
        header_type := 1;
      end else
      if memcmp(b.buffer, 'ADIF', 4) = 0 then
      begin
        if(b.buffer[4] and $80) <> 0 then
        skip_size := 9
        else
        skip_size := 0;
        bitrate := (LongWord((b.buffer[4 + skip_size]) and $0F)shr 19) or
            (LongWord(b.buffer[5 + skip_size]) shr 11) or
            (LongWord(b.buffer[6 + skip_size]) shr 3) or
            (LongWord(b.buffer[7 + skip_size]) and $E0);

        length := b.infile.Size;
        if length <> 0 then
            length := (length*8)/bitrate + 0.5;
        bitrate := Trunc(bitrate/1000 + 0.5);
         header_type := 2;
      end;
      fill_buffer(b);
      bread := NeAACDecInit(hDecoder, @(b.buffer[0]), b.bytes_into_buffer, samplerate, channels);
      if bread < 0 then
      begin
        NeAACDecClose(hDecoder);
        raise EAuException.Create('Error nitializing the decoder');
      end;
      advance_buffer(b, bread);
      fill_buffer(b);
      FSR := samplerate;
      FChan := channels;
      case config.outputFormat of
       FAAD_FMT_16BIT : FBPS := 16;
       FAAD_FMT_24BIT : FBPS := 24;
       FAAD_FMT_32BIT : FBPS := 32;
       else  raise EAuException.Create('Unsupported sample format.');
      end;
      ReadAACSamples;
      FBitrate := bitrate;
      FValid := True;
      FBOffset := 0;
      FBytesRead := 0;
      FSize := -1;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAACIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      NeAACDecClose(hDecoder);
      if not FStreamAssigned then
         FStream.Free;
      FOpened  := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAACIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  begin
    if FBOffset = FBytesRead then
    begin
      if b.at_eof and (b.bytes_into_buffer = 0) then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      ReadAACSamples;
    end;
    if Bytes > FBytesRead - FBOffset then
      Bytes := FBytesRead - FBOffset;
    Buffer := @Buf2[FBOffset];
    Inc(FBOffset, Bytes);
  end;

  procedure TAACIn.ReadAACSamples;
  var
    S : PAnsiChar;
    sample_buffer : Pointer;
    frameInfo : NeAACDecFrameInfo;
  begin
      FBytesRead := 0;
      FBOffset := 0;
      while FBytesRead = 0 do
      begin
        if b.at_eof and (b.bytes_into_buffer = 0) then
          break;
        sample_buffer := NeAACDecDecode(hDecoder, @frameInfo, @(b.buffer[0]), b.bytes_into_buffer);
        advance_buffer(b, frameInfo.bytesconsumed);
        fill_buffer(b);
        if frameInfo.error > 0 then
        begin
          S := NeAACDecGetErrorMessage(frameInfo.error);
          raise EAuException.Create(S);
        end;
        //  sample_count := (dur * frameInfo.channels);
        FBytesRead := frameInfo.samples*(FBPS div 8);
        Move(sample_buffer^, Buf2[0], FBytesRead);
      end;
      FChan := frameInfo.channels;
      FSR := frameInfo.samplerate;
 //     FSize := Trunc(frameInfo.samples/frameInfo.bytesconsumed*FStream.Size)*(FBPS div 8);
//      FTotalSamples := FSize div (FChan*(FBPS div 8));
  end;

  function TAACIn.GetBitrate : LongWord;
  begin
    OpenFile;
    Result := FBitrate;
  end;

end.


