(*
  This file is a part of New Audio Components package v 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
  *************************************************************

   TTTAIn and TTTAOut components are written by Sergei Borisov, <jr_ross@mail.ru>
*)


unit ACS_TTA;

(* $Id: ACS_TTA.pas 647 2008-07-02 05:12:26Z andrei.borovsky $ *)

(* Title: ACS_TTA
    TTA (True Audio codec) encoder and decoder components.
    Requires TTALib.dll. *)


interface

uses
  Windows, Classes,
  TTALib,
  ACS_Classes,
  ACS_Tags;

type

  { class TTTAIn }

  TTTAInBuffer = array of Byte;

  (* Class: TTTAIn
    The TTA decoder component, descends from <TAuTaggedFileIn>. Note that
    TTA files are not seekable. Requires TTALib.dll.
  *)
  TTTAIn = class(TAuTaggedFileIn)
  private
    FFile: HFILE;
    FDecoder: TTTADecoder;
    FBuffer: TTTAInBuffer;
    FBufferRest: TTTAInBuffer;

    function GetId3v1Tags: TId3v1Tags;
    function GetId3v2Tags: TId3v2Tags;

    procedure InitDecoder;
    procedure DoneDecoder;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;

    function SeekInternal(var Sample: Int64): Boolean; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;

  (* Property: Id3v1Tags
       This property returns the file tags in Id3v1Tags format. *)
    property Id3v1Tags: TId3v1Tags read GetId3v1Tags;
    property Id3v2Tags: TId3v2Tags read GetId3v2Tags;
  published
    property Loop;
  end;

  { class TTTAOut }

    (* Class: TTTAOut
      The TTA encoder component, descends from <TAuTaggedFileOut>. 
      Requires TTALib.dll. *)

  TTTAOut = class(TAuTaggedFileOut)
  private
    FFile: HFILE;
    FEncoder: TTTAEncoder;
    FBufferInStart: Cardinal;
    FBufferIn: array of Byte;
    FBufferOut: array of Byte;
  protected
    procedure Prepare; override;
    procedure Done; override;

    function DoOutput(Abort: Boolean): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    (* Property: Id3v1Tags
         This property allows you to set the file tags in Id3v1Tags format. *)
    property Id3v1Tags;
  (*   Property: Id3v2Tags
         This property allows you to set the file tags in Id3v2Tags format. *)
    property Id3v2Tags;
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

  t_float_sample = packed record
    sample: Single;
    dumb: Cardinal;
  end;
  t_float_sample_array = packed array [0 .. 0] of t_float_sample;
  p_float_sample_array = ^t_float_sample_array;

const
  max_frame_count = 1024;
  byte_sample_base = t_byte_sample((High(t_byte_sample) shr 1) + 1);

function CreateTempFileName(const Prefix: WideString): WideString;
var
  temp_file_path: WideString;
begin
  SetLength(temp_file_path, MAX_PATH);
  GetTempPathW(Length(temp_file_path), @(temp_file_path[1]));
  SetLength(Result, MAX_PATH);
  GetTempFileNameW(@(temp_file_path[1]), @(Prefix[1]), 0, @(Result[1]));
end;


{ class TTTAIn }

constructor TTTAIn.Create(AOwner: TComponent);
begin
  inherited;
  FSeekable := True;
end;

function TTTAIn.GetId3v1Tags: TId3v1Tags;
begin
  Result := _Id3v1Tags;
end;

function TTTAIn.GetId3v2Tags: TId3v2Tags;
begin
  Result := _Id3v2Tags;
end;

procedure TTTAIn.InitDecoder;
var
  file_pos: Cardinal;
begin
  file_pos := SetFilePointer(FFile, 0, nil, FILE_CURRENT);
  if file_pos <> $FFFFFFFF then
    try
      _Id3v1Tags.ReadFromFile(FFile);
    finally
      SetFilePointer(FFile, file_pos, nil, FILE_BEGIN);
    end;

  if _Id3v2Tags.Presents(FFile) and not _Id3v2Tags.ReadFromFile(FFile) then
    EAuException.Create('Broken or unsupported Id3v2 tags was found!');

  FDecoder := TTTADecoder.Create(FFile);

  FSeekable := False;

  FPosition := 0;
end;

procedure TTTAIn.DoneDecoder;
begin
  FreeAndNil(FDecoder);

  FSeekable := True;

  if FFile <> INVALID_HANDLE_VALUE then begin
    CloseHandle(FFile);
    FFile := INVALID_HANDLE_VALUE;
  end;

  SetLength(FBuffer, 0);
end;

procedure TTTAIn.OpenFile;
const
  access: array [Boolean] of Cardinal = (
    GENERIC_READ, GENERIC_READ or GENERIC_WRITE);
  share: array [Boolean] of Cardinal = (
    FILE_SHARE_READ, 0);
  disposition: array [Boolean] of Cardinal = (
    OPEN_EXISTING, CREATE_ALWAYS);
  attrs: array [Boolean] of Cardinal = (
    FILE_ATTRIBUTE_NORMAL,
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE);
  error: array [Boolean] of String = (
    'Unable to open file for input!', 'Unable to create temporary file!');
var
  file_name: WideString;
  size: Cardinal;
  h_mapping: THandle;
  p_mapping: Pointer;
begin
  LoadTTALib;
  if not TTALib_Loaded then
    raise EAuException.Create(TTALib_Name + ' library could not be loaded.');
  OpenCS.Enter();
  try
    if FOpened = 0 then begin
      FValid := False;

      if FStreamAssigned then
        file_name := CreateTempFileName('pcm')
      else
      if FWideFileName <> '' then
        file_name := FWideFileName
      else
        raise EAuException.Create('File name is not defined!');

      FFile := CreateFileW(
        @(file_name[1]),
        access[FStreamAssigned],
        share[FStreamAssigned],
        nil,
        disposition[FStreamAssigned],
        attrs[FStreamAssigned] or FILE_FLAG_RANDOM_ACCESS,
        0);
      if FFile = INVALID_HANDLE_VALUE then
        raise EAuException.Create(error[FStreamAssigned]);

      if FStreamAssigned then begin
        size := FStream.Size;
        if size > 0 then begin
          h_mapping := CreateFileMapping(
            FFile,
            nil,
            PAGE_READWRITE,
            0, size,
            nil);
          if h_mapping <> 0 then
            try
              p_mapping := MapViewOfFile(h_mapping, FILE_MAP_WRITE, 0, 0, 0);
              if p_mapping <> nil then
                try
                  FStream.Seek(0, soFromBeginning);
                  FStream.Read(p_mapping^, size);
                finally
                  UnmapViewOfFile(p_mapping);
                end;
            finally
              CloseHandle(h_mapping);
            end
          else
            raise EAuException.Create('Unable to map temporary file to memory!');
        end;
      end;

      SetFilePointer(FFile, 0, nil, FILE_BEGIN);

      try
        InitDecoder();
      except
        CloseHandle(FFile);
        FFile := INVALID_HANDLE_VALUE;

        raise;
      end;

      FValid := True;

      FBPS := FDecoder.BitsPerSample;
      FSR := FDecoder.SampleRate;
      FChan := FDecoder.NumChannels;
      FTotalSamples := FDecoder.DataLength;

      if FTotalSamples >= 0 then begin
        FSize := FTotalSamples * ((FBPS + 7) shr 3) * FChan;
        FTime := FTotalSamples div FSR;
      end
      else
        FSize := - 1;
      Inc(FOpened);
    end;
  finally
    OpenCS.Leave();
  end;
end;

procedure TTTAIn.CloseFile;
begin
  OpenCS.Enter();
  try
    if FOpened > 0 then begin
      Dec(FOpened);

      if FOpened = 0 then
        DoneDecoder();
    end;
  finally
    OpenCS.Leave();
  end;
end;

function TTTAIn.SeekInternal(var Sample: Int64): Boolean;
begin
  Result := False;
end;

procedure TTTAIn.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
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
  new_bytes,
  frame_size, frames, n, i: Cardinal;
  p_buf: Pointer;
  p_in_int32_samples: p_int32_sample_array;
  p_in_float_samples: p_float_sample_array;
  p_out_byte_samples: p_byte_sample_array;
  p_out_int16_samples: p_int16_sample_array;
  p_out_int24_samples: p_int24_sample_array;
  p_out_int32_samples: p_int32_sample_array;
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

    frame_size := ((FBPS + 7) shr 3) * FChan;

    while Bytes > Cardinal(Length(FBuffer)) do begin
      frames := FDecoder.GetBlock(@p_buf);
      if frames = 0 then begin
        Bytes := Length(FBuffer);

        Break;
      end;

      n := Length(FBuffer);
      SetLength(FBuffer, n + (frames * frame_size));

      case (FBPS + 7) shr 3 of
        1: begin // 8 bits per sample
          p_in_int32_samples := p_buf;
          p_out_byte_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            p_out_byte_samples[i] := byte_sample_base +
              t_byte_sample(p_in_int32_samples[i]);
        end;
        2: begin // 16 bits per sample
          p_in_int32_samples := p_buf;
          p_out_int16_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            p_out_int16_samples[i] := p_in_int32_samples[i];
        end;
        3: begin // 24 bits per sample
          p_in_int32_samples := p_buf;
          p_out_int24_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            p_out_int24_samples[i] := t_int24_sample_in_int32(p_in_int32_samples[i]).sample;
        end;
        4: begin // 32 bits per sample
          p_in_float_samples := p_buf;
          p_out_int32_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            if p_in_float_samples[i].sample < - 1 then
              p_out_int32_samples[i] := Low(p_out_int32_samples[i])
            else
            if p_in_float_samples[i].sample >= 1 then
              p_out_int32_samples[i] := High(p_out_int32_samples[i])
            else
            if p_in_float_samples[i].sample <> 0 then
              p_out_int32_samples[i] :=
                Floor(p_in_float_samples[i].sample * - Int64(Low(p_out_int32_samples[i])))
            else // p_in_float_samples[i].sample = 0
              p_out_int32_samples[i] := 0;
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


{ class TTTAOut }

constructor TTTAOut.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TTTAOut.Prepare;
const
  attrs: array [Boolean] of Cardinal = (
    FILE_ATTRIBUTE_NORMAL,
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE);
  error: array [Boolean] of String = (
    'Unable to create file for output!', 'Unable to create temporary file!');
  wave_formats: array [Boolean] of Word = (
    WAVE_FORMAT_PCM, WAVE_FORMAT_IEEE_FLOAT);
var
  file_name: WideString;
  frame_size: Integer;
begin
  LoadTTALib;
  if not TTALib_Loaded then
    raise EAuException.Create(TTALib_Name + ' library could not be loaded.');
  if FStreamAssigned then
    file_name := CreateTempFileName('tta')
  else
  if FWideFileName <> '' then
    file_name := FWideFileName
  else
    raise EAuException.Create('File name is not defined!');

  FFile := CreateFileW(
    @(file_name[1]),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    CREATE_ALWAYS,
    attrs[FStreamAssigned] or FILE_FLAG_RANDOM_ACCESS,
    0);
  if FFile = INVALID_HANDLE_VALUE then
    raise EAuException.Create(error[FStreamAssigned]);
  FInput.Init();
  try
    frame_size := FInput.Channels * ((FInput.BitsPerSample + 7) shr 3);

    try
      FEncoder := TTTAEncoder.Create(
        FFile,
        False,
        wave_formats[FInput.BitsPerSample = 32],
        FInput.Channels, FInput.BitsPerSample, FInput.SampleRate,
        FInput.Size div frame_size);
    except
      CloseHandle(FFile);
      FFile := INVALID_HANDLE_VALUE;
    end;
  except
    FInput.Reset();

    raise;
  end;
end;

procedure TTTAOut.Done;
var
  size: Cardinal;
  h_mapping: THandle;
  p_mapping: Pointer;
begin
  FreeAndNil(FEncoder);

  if FFile <> INVALID_HANDLE_VALUE then
    try
      if not ID3v1Tags.Empty then
        ID3v1Tags.WriteToFile(FFile);
      if not Id3v2Tags.Empty then
        Id3v2Tags.WriteToFile(FFile, False);

      if FStreamAssigned then begin
        size := GetFileSize(FFile, nil);
        if size > 0 then begin
          h_mapping := CreateFileMapping(
            FFile,
            nil,
            PAGE_READONLY,
            0, size,
            nil);
          if h_mapping <> 0 then
            try
              p_mapping := MapViewOfFile(h_mapping, FILE_MAP_READ, 0, 0, 0);
              if p_mapping <> nil then
                try
                  if FFileMode = foRewrite then
                    FStream.Seek(0, soFromBeginning)
                  else // foAppend
                    FStream.Seek(0, soFromEnd);

                  FStream.Write(p_mapping^, size);
                finally
                  UnmapViewOfFile(p_mapping);
                end;
            finally
              CloseHandle(h_mapping);
            end
          else
            raise EAuException.Create('Unable to map temporary file to memory!');
        end;
      end
      else
        FlushFileBuffers(FFile);
    finally
      CloseHandle(FFile);
      FFile := INVALID_HANDLE_VALUE;
    end;

  FInput.Flush();

  if not FStreamAssigned then
    FreeAndNil(FStream);
end;

function TTTAOut.DoOutput(Abort: Boolean) : Boolean;
type
  t_int24_sample_in_int32 = packed record
    lo: Word;
    hi: Smallint;
  end;
  p_int24_sample_in_int32 = ^t_int24_sample_in_int32;
var
  buffer: Pointer;
  frame_size, bytes, buffer_in_size, buffer_out_size, frames, samples, i: Cardinal;
  p_in_byte_samples: p_byte_sample_array;
  p_in_int16_samples: p_int16_sample_array;
  p_in_int24_samples: p_int24_sample_array;
  p_in_int32_samples: p_int32_sample_array;
  p_out_int32_samples: p_int32_sample_array;
  p_out_float_samples: p_float_sample_array;
begin
  Result := CanOutput;
  if not Result then
    Exit; 

  if Abort then begin
    Result := False;

    Exit;
  end;

  frame_size := FInput.Channels * ((FInput.BitsPerSample + 7) shr 3);
  bytes := max_frame_count * frame_size;
  FInput.GetData(buffer, bytes);
  Result := (bytes > 0);
  if Result then begin
    buffer_in_size := FBufferInStart + bytes;
    if Cardinal(Length(FBufferIn)) < buffer_in_size then
      SetLength(FBufferIn, buffer_in_size);
    Move(buffer^, FBufferIn[FBufferInStart], bytes);

    frames := buffer_in_size div frame_size;
    samples := frames * FInput.Channels;

    if FInput.BitsPerSample = 32 then
      buffer_out_size := samples *
        SizeOf(p_out_float_samples[Low(p_out_float_samples^)])
    else
      buffer_out_size := samples *
        SizeOf(p_out_int32_samples[Low(p_out_int32_samples^)]);

    if Cardinal(Length(FBufferOut)) < buffer_out_size then
      SetLength(FBufferOut, buffer_out_size);

    case (FInput.BitsPerSample + 7) shr 3 of
      1: begin // 8 bits per sample
        p_in_byte_samples := @(FBufferIn[0]);
        p_out_int32_samples := @(FBufferOut[0]);
        for i := 0 to samples - 1 do
          p_out_int32_samples[i] :=
            (t_int32_sample(p_in_byte_samples[i]) - t_int32_sample(byte_sample_base)) and $FF;
      end;
      2: begin // 16 bits per sample
        p_in_int16_samples := @(FBufferIn[0]);
        p_out_int32_samples := @(FBufferOut[0]);
        for i := 0 to samples - 1 do
          p_out_int32_samples[i] := p_in_int16_samples[i]; 
      end;
      3: begin // 24 bits per sample
        p_in_int24_samples := @(FBufferIn[0]);
        p_out_int32_samples := @(FBufferOut[0]);
        for i := 0 to samples - 1 do begin
          t_int24_sample_in_int32(p_out_int32_samples[i]).lo := p_in_int24_samples[i].lo;
          t_int24_sample_in_int32(p_out_int32_samples[i]).hi := p_in_int24_samples[i].hi;
        end;
      end;
      4: begin
        p_in_int32_samples := @(FBufferIn[0]);
        p_out_float_samples := @(FBufferOut[0]);
        for i := 0 to samples - 1  do
          if p_in_int32_samples[i] <> 0 then
            p_out_float_samples[i].sample :=
              p_in_int32_samples[i] / - Int64(Low(p_in_int32_samples[i]))
          else // if p_in_int32_samples[i] = 0
            p_out_float_samples[i].sample := 0;
      end;
    end;

    Result := FEncoder.CompressBlock(@(FBufferOut[0]), frames);

    bytes := frames * frame_size;
    if bytes < buffer_in_size then begin
      FBufferInStart := buffer_in_size - bytes;
      Move(FBufferIn[bytes], FBufferIn[0], FBufferInStart);
    end
    else
      FBufferInStart := 0;
  end;
end;

end.

