(*
  This file is a part of New Audio Components package v 1.9
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
  *************************************************************

   TMPCIn component is written by Sergei Borisov, <jr_ross@mail.ru>

   TMPCOut component is written by Andrei Borovsky
*)

(*

*)

(* Title: ACS_MPC
    NewAC interface to the Musepack codec. This unit contains Musepack decoder
    and encoder components. You can learn more about Musepack at
    www.musepack.net. The TMPCIn component requires libmpdec.dll library and
    TMPCOut requires libmppenc.dll library. Both libraies can be downloaded
    from the project site www.symmetrica.net. *)

unit ACS_MPC;

(* $Id: ACS_MPC.pas 1117 2010-01-22 09:28:59Z andrei.borovsky $ *)

interface

uses
  Classes,
  libmpdec,
  libmppenc,
  ACS_Classes,
  ACS_Tags;

const
  DefaultMPCBitsPerSample = 16;

type

  (* class TMPCIn *)

  TMPCInBuffer = array of Byte;

(* Class: TMPCIn
    A descenant of <TAuTaggedFileIn> which decodes Musepack. Requires
    libmpdec.dll. You can learn more about Musepack audio compression format
    at http://www.musepack.net. *)

  TMPCIn = class(TAuTaggedFileIn)
  private
    FDecoder: TMPCDecoder;
    FBuffer: TMPCInBuffer;
    FBufferRest: TMPCInBuffer;
//    function GetBitrate : LongWord;
    function GetAverageBitrate: Cardinal;
    procedure SetBPS(Value: Cardinal);
    function GetAPEv2Tags : TAPEv2Tags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    function SeekInternal(var Sample: Int64): Boolean; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
//    property CurrentBitrate : LongWord read GetBitrate;
  (* Property: APEv2Tags
     Read this property to get Apev2 tags from the input file. *)
    property APEv2Tags : TAPEv2Tags read GetAPEv2Tags;
   (* Property: AverageBitrate
        Read this property to get the input file's average bitrate in kbps. *)
    property AverageBitrate : Cardinal read GetAverageBitrate;

  published
   (* Property: OutBitsPerSample
        Regardless of an encoder's input bits per sample value Musepack always
        encodes its data in 32-bit samples. You can use this property to set
        output bits per sample value for the decoder (16, 24, or 32). *)
    property OutBitsPerSample: Cardinal read FBPS write SetBPS default DefaultMPCBitsPerSample;
    property StartSample;
    property EndSample;
  end;

(* Class: TMPCOut
    Descendant of <TAuTaggedFileOut> which decodes Musepack. Requires
    libmppenc.dll. You can learn more about Musepack audio compression format
    at http://www.musepack.net.

    Important Note:
    Musepack supports encoding in the limited range of sample rates. Currently
    only 32000, 37800, 44100, and 48000 Hz sample rates are supported. Trying
    to use any other input sample rates will result in exception. *)

  TMPCOut = class(TAuTaggedFileOut)
  private
    EndOfInput : Boolean;
    encoder : Pointer;
    FQuality : Single;
  protected
    procedure Prepare; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Done; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
   (* Property: Quality
        The output file quality. Valid values range from 1.0 to 10.0. *)
    property Quality : Single read FQuality write FQuality;
   (* Property: APEv2Tags
        This property allows youto append Apev2 tags to the file being encoded. *)
    property APEv2Tags;
  end;


implementation

uses
  SysUtils, Math;

type
  t_byte_sample = type Byte;
  t_byte_sample_array = packed array [0 .. 0] of t_byte_sample;
  p_byte_sample_array = ^t_byte_sample_array;

  t_int16_sample = type Smallint;
  t_int16_sample_array = packed array [0 .. 0] of t_int16_sample;
  p_int16_sample_array = ^t_int16_sample_array;

  t_int24_sample = packed record
    lo: Word;
    hi: Shortint;
  end;
  t_int24_sample_array = packed array [0 .. 0] of t_int24_sample;
  p_int24_sample_array = ^t_int24_sample_array;

  t_int32_sample = type Integer;
  t_int32_sample_array = packed array [0 .. 0] of t_int32_sample;
  p_int32_sample_array = ^t_int32_sample_array;

const
  byte_sample_base = t_byte_sample((High(t_byte_sample) shr 1) + 1);

{ class TMPCIn }

constructor TMPCIn.Create(AOwner: TComponent);
begin
  inherited;
  SetBPS(DefaultMPCBitsPerSample);
end;

procedure TMPCIn.SetBPS(Value: Cardinal);
begin
  OpenCS.Enter();
  try
    if FOpened = 0 then
      if Value < 8 then
        FBPS := 8
      else
      if Value > 32 then
        FBPS := 32
      else
        FBPS := ((Value + 7) shr 3) shl 3;
  finally
    OpenCS.Leave();
  end;
end;

procedure TMPCIn.OpenFile;
begin
  LoadLibMPDec;
  if not libMPDec_Loaded then
    raise EAuException.Create(libMPDec_Name + ' library could not be loaded.');
  OpenCS.Enter();
  try
    if FOpened = 0 then begin
      FValid := False;
      _APEv2Tags.Clear;
      if not FStreamAssigned then
        if FWideFileName = '' then
          raise EAuException.Create('File name is not assigned')
        else
          FStream := TAuFileStream.Create(
            FWideFileName, fmOpenRead or fmShareDenyWrite);
      ReadApe2Tags(FStream, _APEv2Tags);
      _CommonTags.Clear;
      _CommonTags.Artist := _Apev2Tags.Artist;
      _CommonTags.Album := _Apev2Tags.Album;
      _CommonTags.Title := _Apev2Tags.Title;
      _CommonTags.Year := _Apev2Tags.Year;
      _CommonTags.Track := _Apev2Tags.Track;
      _CommonTags.Genre := _Apev2Tags.Genre;
      Stream.Seek(0, soFromBeginning);
      FDecoder := TMPCDecoder.Create(FStream);

      FValid := True;
      FSeekable := True;

      FSR := FDecoder.SampleRate;
      FChan := FDecoder.NumChannels;
      FTotalSamples := FDecoder.NumSamples;

      FPosition := 0;
      if FTotalSamples >= 0 then
        FSize := FTotalSamples * ((FBPS + 7) shr 3) * FChan
      else
        FSize := - 1;

      Inc(FOpened);
    end;
  finally
    OpenCS.Leave();
  end;
end;

procedure TMPCIn.CloseFile;
begin
  OpenCS.Enter();
  try
    if FOpened > 0 then begin
      Dec(FOpened);

      if FOpened = 0 then begin
        try
          FDecoder.Free();
        finally
          FDecoder := nil;
        end;

        if not FStreamAssigned then
          FreeAndNil(FStream);

        SetLength(FBuffer, 0);
      end;
    end;
  finally
    OpenCS.Leave;
  end;
end;

function TMPCIn.SeekInternal(var Sample: Int64): Boolean;
begin
  Result := FDecoder.Seek(Sample);
end;

procedure TMPCIn.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
type
  t_int24_sample_in_int32 = packed record
    sample: t_int24_sample;
    sign: Shortint;
  end;
  p_int24_sample_in_int32 = ^t_int24_sample_in_int32;
const
  int24_sample_high: t_int24_sample = (
    lo: High(int24_sample_high.lo); hi: High(int24_sample_high.hi));
  int24_sample_low: t_int24_sample = (
    lo: Low(int24_sample_low.lo); hi: Low(int24_sample_low.hi));
var
  new_bytes, status, samples, n: Cardinal;
  buf: TMPCDecoderBuffer;
  i: Integer;
  p_byte_samples: p_byte_sample_array;
  p_int16_samples: p_int16_sample_array;
  p_int24_samples: p_int24_sample_array;
  p_int32_samples: p_int32_sample_array;
  temp: t_int32_sample;
begin
  if not Busy then
    raise EAuException.Create('File for input is not opened!');

  if Bytes > 0 then begin
    if FSize >= 0 then begin
      new_bytes := FSize - FPosition;
      if Bytes > new_bytes then
        Bytes := new_bytes;
    end;

    FBuffer := FBufferRest;

    while Bytes > Cardinal(Length(FBuffer)) do begin
      FillChar(buf[Low(buf)], Length(buf) * SizeOf(buf[Low(buf)]), 0);

      status := FDecoder.Decode(buf);
      if (status = 0) or (status = Cardinal(Integer(- 1))) then begin
        Bytes := Length(FBuffer);

        Break;
      end;

      samples := status * FDecoder.NumChannels;
      new_bytes := samples * ((FBPS + 7) shr 3);

      n := Length(FBuffer);
      SetLength(FBuffer, n + new_bytes);

      case (FBPS + 7) shr 3 of
        1: begin // 8 bits per sample
          p_byte_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_byte_samples[i] := High(p_byte_samples[i])
            else
            if buf[i] <= - 1 then
              p_byte_samples[i] := Low(p_byte_samples[i])
            else
              p_byte_samples[i] := byte_sample_base +
                Floor(buf[i] * byte_sample_base);
        end;
        2: begin // 16 bits per sample
          p_int16_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_int16_samples[i] := High(p_int16_samples[i])
            else
            if buf[i] <= - 1 then
              p_int16_samples[i] := Low(p_int16_samples[i])
            else
              p_int16_samples[i] := Floor(buf[i] * High(p_int16_samples[i]));
        end;
        3: begin // 24 bits per sample
          p_int24_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_int24_samples[i] := int24_sample_high
            else
            if buf[i] <= - 1 then
              p_int24_samples[i] := int24_sample_low
            else begin
              temp := Floor(buf[i] * High(temp)) shr 8;

              p_int24_samples[i] := t_int24_sample_in_int32(temp).sample;
            end;
        end;
        4: begin // 32 bits per sample
          p_int32_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_int32_samples[i] := High(p_int32_samples[i])
            else
            if buf[i] <= - 1 then
              p_int32_samples[i] := Low(p_int32_samples[i])
            else
              p_int32_samples[i] := Floor(buf[i] * High(p_int32_samples[i]));
        end;
      end;
    end;

    if Cardinal(Length(FBuffer)) > Bytes then begin
      SetLength(FBufferRest, Cardinal(Length(FBuffer)) - Bytes);
      Move(FBuffer[Bytes], FBufferRest[0], Length(FBufferRest));
    end
    else
      SetLength(FBufferRest, 0);

    Buffer := @(FBuffer[0]);
  end;
end;

(*function TMPCIn.GetBitrate;
begin
  if Busy then
    Result := FDecoder.Bitrate
  else
    Result := 0;
end;*)

function TMPCIn.GetAverageBitrate;
begin
  OpenFile;
  Result := FDecoder.AverageBitrate;
end;

  procedure cb_input_open(cb_data : Pointer); cdecl;
  begin
  end;

  function cb_input_read(DstBuf : Pointer; _ElementSize, Count : LongWord; cb_data : Pointer): LongWord; cdecl;
  var
    MPCOut : TMPCOut;
  begin
    MPCOut := TMPCOut(cb_data);
    if MPCOut.EndOfInput then
      Result := 0
    else
      Result := MPCOut.Input.FillBuffer(DstBuf, _ElementSize*Count, MPCOut.EndOfInput);
      Result := Result div _ElementSize;
  end;

  function cb_input_eof(cb_data : Pointer): Integer; cdecl;
  var
    MPCOut : TMPCOut;
  begin
    MPCOut := TMPCOut(cb_data);
    Result := Integer(MPCOut.EndOfInput);
  end;

  procedure cb_input_close(cb_data : Pointer); cdecl;
  begin
  end;

  procedure cb_output_open(cb_data : Pointer); cdecl;
  begin
  end;

  function cb_output_read(DstBuf : Pointer; _ElementSize, Count : LongWord; cb_data : Pointer): LongWord; cdecl;
  var
    MPCOut : TMPCOut;
  begin
    MPCOut := TMPCOut(cb_data);
    Result := MPCOut.FStream.Read(DstBuf^, _ElementSize*Count);
    Result := Result div _ElementSize;
  end;

  function cb_output_write(Buf : Pointer; Size, Count : LongWord; cb_data : Pointer): LongWord; cdecl;
  var
    MPCOut : TMPCOut;
  begin
    MPCOut := TMPCOut(cb_data);
    Result := MPCOut.FStream.Write(Buf^, Size*Count);
    Result := Result div Size;
  end;

  function cb_output_seek(Offset, Origin : Integer; cb_data : Pointer): Integer; cdecl;
  var
    MPCOut : TMPCOut;
  begin
    MPCOut := TMPCOut(cb_data);
    Result := MPCOut.FStream.Seek(Offset, Word(Origin));
  end;

  procedure cb_output_close(cb_data : Pointer); cdecl;
  begin
  end;



constructor TMPCOut.Create;
begin
  inherited Create(AOwner);
  FQuality := 5.0;
end;

procedure TMPCOut.Prepare;
begin
  FExceptionMessage := '';
  if not FStreamAssigned then
  begin
    if FFileName = '' then raise EAuException.Create('File name is not assigned.');
    FStream := TAuFileStream.Create(FWideFileName, fmCreate or FShareMode);
  end;
  EndOfInput := False;
  FInput.Init;
  LoadMPPEncLibrary;
  if not libMPPEncLoaded then
    raise EAuException.Create(LibMPPEncPath + ' library could not be loaded.');
  if not ((Finput.SampleRate div 1000) in [32, 37, 44, 48]) then
  begin
    EndOfInput := True;
    raise EAuException.Create('Unsupported sample rate.');
  end;
  init_static;
  if init_enc_state(encoder, FInput.SampleRate, FInput.Size div ((FInput.BitsPerSample div 8)* FInput.Channels), Finput.BitsPerSample, Finput.Channels, FQuality) <> 0 then
  begin
    UnloadMPPEncLibrary;
    raise EAuException.Create('Failed to initialize encoder');
  end;
  set_callbacks(encoder, cb_input_open, cb_input_read, cb_input_eof, cb_input_close,
                cb_output_open, cb_output_seek, cb_output_read, cb_output_write, cb_output_close, Self);
  if not APEv2Tags.Empty then
  begin
    {$WARNINGS OFF}
    if APEv2Tags.Artist <> '' then
       AddTag(_ape_Artist, APEv2Tags.Artist);
    if APEv2Tags.Album <> '' then
       AddTag(_ape_Album, APEv2Tags.Album);
    if APEv2Tags.Title <> '' then
       AddTag(_ape_Title, APEv2Tags.Title);
    if APEv2Tags.Year <> '' then
       AddTag(_ape_Year, APEv2Tags.Year);
    if APEv2Tags.Genre <> '' then
       AddTag(_ape_Genre, APEv2Tags.Genre);
    if APEv2Tags.Copyright <> '' then
       AddTag(_ape_Copyright, APEv2Tags.Copyright);
    if APEv2Tags.Composer <> '' then
       AddTag(_ape_Composer, APEv2Tags.Composer);
    if APEv2Tags.Track <> '' then
       AddTag(_ape_Track, APEv2Tags.Track);
    {$WARNINGS ON}
  end;
  start_encoder(encoder);
end;

function TMPCOut.DoOutput;
begin
  Result := True;
  if Abort then
      EndOfInput := True;
  if EndOfInput then
  begin
    Result := False;
    Exit;
  end;
  if process_block(encoder) <> 0 then
    EndOfInput := True;
end;

procedure TMPCOut.Done;
begin
  free_encoder_state(encoder);
  UnloadMPPEncLibrary;
  FInput.Flush;
  if not FStreamAssigned then FStream.Free;
end;

function TMPCIn.GetAPEv2Tags;
begin
  OpenFile;
  Result := _APEv2Tags;
end;


  const
    CW = LongWord($133f);

initialization
  Set8087CW(CW);
end.

