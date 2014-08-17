(*
  This file is a part of New Audio Components package 2.2
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: NewACAC3.pas 1117 2010-01-22 09:28:59Z andrei.borovsky $ *)

unit NewACAC3;

(* Title: NewACAC3
    Dolby Digital (AC-3) decoder component *)

interface

uses
  Windows, Classes, SysUtils, math, ACS_Classes, ACS_Procs, ACS_Types, liba52, DMXStreams;

type

  TAC3OutputChannels = (acc5dot1 = 0, accMono, accStereo);

 (* Class: TAC3In
      Descends from <TAuFileIn>.
      This component decodes AC-3 encoded audio streams either from AC-3 files or from VOB files.
      Currently it cannot decode Wave-encoded AC-3 data.
      You will need liba52.dll (included with other NewAC dlls) to use the component.
 *)

  TAC3In = class(TAuFileIn)
  private
    state : pa52_state;
    _Buf : array of SmallInt;
    FrameBuf : array of Byte;
    FrameSize : Integer;
    _BufSize : Integer;
    _SampleSize : Word;
//    _StartSample, _StartFrom : LongWord;
    Offset, BufEnd : LongWord;
    BlockCount, CurrentBlock : Integer;
    FBitRate : LongWord;
    FFlags : Integer;
    __EOF : Boolean;
    FExtract : Boolean;
    FDemuxer : TAuVOBAC3Demuxer;
    FVobStream : TAC3VOBStream;
    StreamSize : Int64;
    FOutputChannels : TAC3OutputChannels;
    function ReadFrame : Boolean;
    function GetBitrate : LongWord;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
  public
    (* Property: BitRate
       Read this property to determine the bit rate for the AC-3 file.
    *)
    property BitRate : LongWord read GetBitrate;
    published
    (* Property: VobAudioSubstream
      VOB files may contain two AC-3 audio substreams. This property allows you to choose which audio stream to extract.
      This property has any effect only if you are extracting AC-3 from VOB/MPEG-2 files.
     *)
     property VobAudioSubstream : TAC3VOBStream read FVobStream write FVOBStream;
    (* Property: OutputChannels
       This property controls the number of output channels. The default value is acc5dot1 which corresponds to the maximum available channels (5.1 or less if the original stream contains less channels).
       If any other value is selected the down-mixing is used by the decoder to provide less channels.
    *)
     property OutputChannels : TAC3OutputChannels read FOutputChannels write FOutputChannels;
  end;


implementation

  const
    SafeOffset : Integer = 0;

  function TAC3In.ReadFrame;
  var
    i : Integer;
    sample_rate, bit_rate : Integer;
    ChanInfo : Integer;
  begin
    Result := False;
    FStream.Read(FrameBuf[0], 6);
    while FStream.Position < StreamSize do
    begin
      FStream.Read(FrameBuf[6], 1);
      begin
        FrameSize := a52_syncinfo(@FrameBuf[0], FFlags, sample_rate, bit_rate);
        if FrameSize <> 0 then
        begin
          FSR := sample_rate;
          FBitRate := bit_rate;
          FBPS := 16;
          if FOutputChannels > acc5dot1 then
          begin
            FFlags := FFlags and (not A52_CHANNEL_MASK);
            FFlags := FFlags and (not A52_LFE);
            case FOutputChannels of
              accMono: FFlags := FFlags or A52_MONO;
              accStereo: FFlags := FFlags or A52_STEREO;
              else ;
            end;
          end;
          ChanInfo := FFlags and a52_CHANNEL_MASK;
          case ChanInfo of
            0, 1 : FChan := 1;
            2, 10 : FChan := 2;
            3,4 : FChan := 3;
            5,6 : FChan := 4;
            7 : FChan := 5;
          end;
          if (FFlags and a52_LFE) <> 0 then
            Inc(FChan);
          if (FChan > 8) or ((FSR <> 44100) and (FSR <> 48000) and (FSR <> 32000)) then
          begin
            for i := 0 to 5 do FrameBuf[i] := FrameBuf[i+1];  //FStream.Seek(-5, soFromCurrent);
            Continue;
          end;
          if FStream.Position >= FStream.Size - FrameSize + 7 then
          begin
            STreamSize := FStream.Position;
            Break;
          end;

          FStream.Read(FrameBuf[7], FrameSize-7);
          Result := True;
          Break;
        end else
        for i := 0 to 5 do FrameBuf[i] := FrameBuf[i+1]; //FStream.Seek(-5, soFromCurrent);
      end;
    end;
  end;

  procedure TAC3In.OpenFile;
  var
    Magic : LongWord;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      LoadA52Lib;
      FValid := False;
      if not LibA52Loaded then
      raise EAuException.Create(LibA52Path + ' library could not be loaded.');
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      FStream.Read(Magic, 4);
      if Magic = $BA010000 then
      begin
        FExtract := True;
        FStream.Free;
      end else
      begin
        FExtract := False;
        FStream.Seek(0, soFromBeginning);
      end;
      if FExtract then
      begin
        FDemuxer := TAuVOBAC3Demuxer.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
        FDemuxer.Init(FVobStream);
        FStream := FDemuxer;
      end;
      SetLength(FrameBuf, 32*1024);
      state := a52_init();
      CurrentBlock := 1;
      BlockCount := 6;
      StreamSize := FStream.Size;
      FValid := ReadFrame;
      if FValid = False then
      begin
        Exit;
      end;
      FSize := -1;
      _SampleSize := (FBPS div 8) * FChan;
      FSeekable := False;
      Inc(FOpened);
      SetLength(_Buf, 256*FChan);
      _BufSize := 256*FChan*2;
      Offset := 0;
      BufEnd := 0;
      __EOF := False;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TAC3In.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    level, bias : Single;
    samples : psingle;
    i, j : LongWord;
    Res : Integer;
    SamplesReq : LongWord;
  begin
    if Offset >= BufEnd then
    begin
      if CurrentBlock > BlockCount then
      begin
        if not ReadFrame then
        if FStream.Position < StreamSize then
        begin
          raise EAuException.Create('Sync lost')
        end else
        begin
          Bytes := 0;
          Buffer := nil;
        end;
        CurrentBlock := 1;
      end;
      if CurrentBlock = 1 then
      begin
        FFlags := FFlags or A52_ADJUST_LEVEL;
        level := 1;
        bias := 0;
        res := a52_frame(state, @FrameBuf[0], FFlags, level, bias);
        if res <> 0 then
          raise EAuException.Create('Error reading AC3 frame');
      end;
      a52_block(state);
      samples := psingle(a52_samples(state));
      for i := 0 to FChan - 1 do
        for j := 0 to 255 do
        begin
          _Buf[j*FChan + i] := Floor(samples^*(High(SmallInt)));
          Inc(samples);
        end;
      Offset := 0;
      BufEnd := 256;
      Inc(CurrentBlock);
    end;
    Bytes := Bytes - (Bytes mod _SampleSize);
    SamplesReq := Bytes div _SampleSize;
    if SamplesReq > BufEnd - Offset then
       SamplesReq := BufEnd - Offset;
    Buffer := @_Buf[Offset*FChan];
    Bytes := SamplesReq*_SampleSize;
    Inc(Offset, SamplesReq);
  end;

  procedure TAC3In.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      a52_free(state);
      _buf := nil;
      FrameBuf := nil;
      if not FStreamAssigned then
         FStream.Free;
      FOpened  := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  function TAC3In.GetBitrate;
  begin
    OpenFile;
    Result := FBitRate;
  end;

end.
