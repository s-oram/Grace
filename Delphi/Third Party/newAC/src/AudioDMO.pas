(*
  This file is a part of New Audio Components package 1.7
  Copyright (c) 2002-2008 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* Title: AudioDMO
    This unit contains component wrappers around several DMOs. *)


(* $Id: AudioDMO.pas 574 2008-04-06 16:46:46Z wayne@wootcomputers.com $ *)

unit AudioDMO;

interface

uses
  SysUtils, Classes, Windows, ACS_Classes, ACS_Types, msdmo;

const
  RESAMPLER_INPUT_BUF_SIZE = $300000;
  VOICEFILTER_INPUT_BUF_SIZE = $3000;
type

 (* Class: TMSResampler
     A fast high-quality resampler based on Windows Vista resampler DMO.
     Descends from <TAuConverter>. 
     
     *This component only works under Windows Vista or later version.*  *)

  TMSResampler = class(TAuConverter)
  private
    FBuffer : PBuffer8;
    Offset : LongWord;
    FBufSize : LongWord;
    FEndOfInput : Boolean;
    HasMoreOutput : Boolean;
    FOutSampleRate : LongWord;
    resampler : dmo_resampler;
    TailBuf : Pointer;
  protected
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: OutSampleRate
      Use this property to set the component's output sample rate.
      If OutSampleRate is set to 0 the component switches to the pass-through mode. *)
    property OutSampleRate : LongWord read FOutSampleRate write FOutSampleRate;
  end;

   (* Class: TVoiceFilter
     A wrapper around Windows DMO that implements several voice-related DSPs.
     TVoiceFilter can perform automatic level adjustment (automatic gain
     control), noise removal, voice activity detection. Descends from
     <TAuConverter>.

     *This component only works under Windows Vista or later.*  *)

  TVoiceFilter = class(TAuConverter)
  private
    FBuffer : PBuffer8;
    Offset : LongWord;
    FBufSize : LongWord;
    FEndOfInput : Boolean;
    HasMoreOutput : Boolean;
    FOutSampleRate : Word;
    FEnableAGC : Boolean;
    FEnableNoiseReduction : Boolean;
    FEnableVAD : Boolean;
    filter : dmo_vcfilter;
    TailBuf : Pointer;
    procedure SetOutSampleRate(value : Word);
  protected
    function GetSR : LongWord; override;
    function GetCh : LongWord; override;
    function GetBPS : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: OutSampleRate
       Use this property to set the output sample rate. TVoiceFilter can
       accept 16 bit mono or stereo audio at different sample rates. The
       output will always be mono with 16 bits per sample. You can select
       output sample rate though. The values allowed for OutSampleRate are
       8000, 11025, 16000, and 22050. *)
    property OutSampleRate : Word read FOutSampleRate write SetOutSampleRate;
    (* Property: EnableAGC
       This property enables or disables the automatic gain control. *)
    property EnableAGC : Boolean read FEnableAGC write FEnableAGC;
    (* Property: EnableNoiseReduction
       This property enables or disables noise filtering. *)
    property EnableNoiseReduction : Boolean read FEnableNoiseReduction write FEnableNoiseReduction;
    (* Property: EnableVAD
       This property enables or disables voice activity detection. If VAD is
       enabled, the silent periods are removed from the component's output. If
       VAD mode is enabled the component cannot report its output size and
       progress. *)
    property EnableVAD : Boolean read FEnableVAD write FEnableVAD;
  end;


implementation

  constructor TMSResampler.Create(AOwner: TComponent);
  var
    VersionInfo : TOSVersionInfo;
  begin
    inherited Create(AOwner);
    if not (csDesigning in ComponentState) then
    begin
      VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
      GetVersionEx(VersionInfo);
      if VersionInfo.dwMajorVersion < 6 then
        raise EAuException.Create('TMSResampler component requires Windows Vista or later version');
    end;
  end;

  destructor TMSResampler.Destroy;
  begin
    inherited Destroy;
  end;

  function TMSResampler.GetSR;
  begin
    Result := 0;
    if not Assigned(FInput) then Exit;
    if FOutSampleRate = 0 then Result := FInput.SampleRate
    else Result := FOutSampleRate;
  end;

  procedure TMSResampler.InitInternal;
  var
    BytePerFrame : Word;
  begin
    if not Assigned(FInput) then
      raise EAuException.Create('Input not assigned');
    FInput.Init;
    Busy := True;
    if FInput.Size = -1 then FSize := -1
    else
    begin
      FSize := Round(FInput.Size*GetSR/FInput.SampleRate);
      BytePerFrame := (Finput.Channels * FInput.BitsPerSample) shr 3;
      FSize := FSize - (FSize mod BytePerFrame);
    end;
    FPosition := 0;
    if GetSR <> FInput.SampleRate then
    begin
      dmo_resampler_init(resampler);
      resampler.input_spec.sample_rate := FInput.SampleRate;
      resampler.input_spec.channels := FInput.Channels;
      resampler.input_spec.bps := FInput.BitsPerSample;
      resampler.output_sr := GetSR;
      dmo_resampler_set_formats(resampler);
    end;
    FEndOfInput := False;
    Offset := 0;
    FBufSize := 0;
    HasMoreOutput := False;
    TailBuf := nil;
  end;

  procedure TMSResampler.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    InBuf : PBuffer8;
    l, s : LongWord;
  begin
    if GetSR = FInput.SampleRate then
    begin
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    if Offset = FBufSize then
    begin
      if not HasMoreOutput then
      begin
        if not FEndOfInput then
        begin
          l := RESAMPLER_INPUT_BUF_SIZE -
              (RESAMPLER_INPUT_BUF_SIZE mod (resampler.input_spec.channels*resampler.input_spec.bps div 8));
          dmo_resampler_prepare_input(resampler, Pointer(InBuf), l);
          s := FInput.FillBuffer(InBuf, l, FEndOfInput);
          if FEndOfInput then
            FillChar(InBuf[s], l-s, 0);
 //            dmo_resampler_reset_input(resampler, l);
          dmo_resampler_write_input(resampler);
        end else //if not FEndOfInput then
        begin
          if FPosition < FSize then
          begin
            if Bytes > FSize - FPosition then
              Bytes := FSize - FPosition;
            if TailBuf <> nil then FreeMem(TailBuf);
            GetMem(TailBuf, Bytes);
            FillChar(TailBuf^, Bytes, 0);
            Buffer := TailBuf;
            Exit;
          end else
          begin
            if TailBuf <> nil then FreeMem(TailBuf);
            TailBuf := nil;
            Buffer := nil;
            Bytes := 0;
            Exit;
          end;
        end; // if not FEndOfInput then... else
      end; // if not HasMoreOutput then
      dmo_resampler_free_output(resampler);
      HasMoreOutput := not dmo_resampler_get_output(resampler, Pointer(FBuffer), FBufSize);
      HasMoreOutput := HasMoreOutput or (not dmo_resampler_accepts_data(resampler));
      Offset := 0;
    end; // if Offset = FBufSize
    if Bytes > FBufSize - Offset then
      Bytes := FBufSize - Offset;
    Buffer := @FBuffer[Offset];
    Inc(Offset, Bytes);
  end;

  procedure TMSResampler.FlushInternal;
  begin
    FInput.Flush;
    if GetSR <> FInput.SampleRate then
    begin
      dmo_resampler_free_output(resampler);
      dmo_resampler_free(resampler);
    end;
  end;

  constructor TVoiceFilter.Create(AOwner: TComponent);
  var
    VersionInfo : TOSVersionInfo;
  begin
    inherited Create(AOwner);
    FOutSampleRate := 8000;
    if not (csDesigning in ComponentState) then
    begin
      VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
      GetVersionEx(VersionInfo);
      if VersionInfo.dwMajorVersion < 6 then
        raise EAuException.Create('TVoiceFilter component requires Windows Vista or later version');
    end;
  end;

  destructor TVoiceFilter.Destroy;
  begin
    inherited Destroy;
  end;

  function TVoiceFilter.GetSR;
  begin
    Result := 0;
    if not Assigned(FInput) then Exit;
    Result := FOutSampleRate;
  end;

  function TVoiceFilter.GetCh;
  begin
    Result := 0;
    if not Assigned(FInput) then Exit;
    Result := 1;
  end;

  function TVoiceFilter.GetBPS;
  begin
    Result := 0;
    if not Assigned(FInput) then Exit;
    Result := 16;
  end;

  procedure TVoiceFilter.InitInternal;
  begin
    if not Assigned(FInput) then
      raise EAuException.Create('Input not assigned');
    FInput.Init;
    Busy := True;
    FPosition := 0;
    //filter.EnableAES := True;
    filter.EnableAGC := FEnableAGC;
    filter.EnableNoiseSuppression := FEnableNoiseReduction;
    filter.EnableVAD := FEnableVAD;
    filter.input_spec.sample_rate := FInput.SampleRate;
    filter.input_spec.channels := FInput.Channels;
    filter.input_spec.bps := FInput.BitsPerSample;
    filter.output_spec.sample_rate := FOutSampleRate;
    filter.output_spec.bps := 16;
    filter.output_spec.channels := 1;
    if (not FEnableVAD) and (FInput.Size >= 0) then
    begin
      FSize := Round(FOutSampleRate*16*FInput.Size/(FInput.SampleRate*Finput.Channels*FInput.BitsPerSample));
      FSize := FSize - (FSize mod 2);
    end
    else
      FSize := -1;
    dmo_vcfilter_init(filter);
//      dmo_vcfilter_set_formats(filter);
    FEndOfInput := False;
    Offset := 0;
    FBufSize := 0;
    HasMoreOutput := False;
    TailBuf := nil;
    if not dmo_vcfilter_accepts_data(filter) then
      raise EAuException.Create('Cannot process data');
  end;

  procedure TVoiceFilter.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    InBuf : Pointer;
    l : LongWord;
    cb : Integer;
    i : Integer;
    B : PBuffer16;

  begin
    if Offset = FBufSize then
    begin
      if not HasMoreOutput then
      begin
        if not FEndOfInput then
        begin
          l := VOICEFILTER_INPUT_BUF_SIZE -
            (VOICEFILTER_INPUT_BUF_SIZE mod (filter.input_spec.channels*filter.input_spec.bps div 8));
          dmo_vcfilter_prepare_input(filter, InBuf, l);
          l := FInput.FillBuffer(InBuf, l, FEndOfInput);
          if FEndOfInput then
            dmo_vcfilter_reset_input(filter, l);
          dmo_vcfilter_write_input(filter);
        end else //if not FEndOfInput then
        begin
          if FPosition < FSize then
          begin
            if Bytes > FSize - FPosition then
              Bytes := FSize - FPosition;
            if TailBuf <> nil then FreeMem(TailBuf);
            GetMem(TailBuf, Bytes);
            FillChar(TailBuf^, Bytes, 0);
            Buffer := TailBuf;
            Exit;
          end else
          begin
            if TailBuf <> nil then FreeMem(TailBuf);
            TailBuf := nil;
            Buffer := nil;
            Bytes := 0;
            Exit;
          end;
        end; // if not FEndOfInput then... else
      end; // if not HasMoreOutput then
      dmo_vcfilter_free_output(filter);
      HasMoreOutput := not dmo_vcfilter_get_output(filter, Pointer(FBuffer), FBufSize);
      if FEnableVAD then
      begin
        B := Pointer(FBuffer);
        cb := 0;
        while (cb < Integer(FBufSize) div 2 - 1) and (FBufSize > 2*filter.FrameSize) do
        begin
          if not Odd(B[cb]) then
          begin
            for i := cb to FBufSize div 2 - filter.FrameSize - 1 do
              B[i] := B[i+Integer(filter.FrameSize)];
            Dec(FBufSize, filter.FrameSize*2);
          end else
          Inc(cb, filter.FrameSize)
        end;
      end;
      HasMoreOutput := HasMoreOutput or (not dmo_vcfilter_accepts_data(filter));
      Offset := 0;
    end; // if Offset = FBufSize
    if Bytes > FBufSize - Offset then
      Bytes := FBufSize - Offset;
    Buffer := @FBuffer[Offset];
    Inc(Offset, Bytes);
  end;

  procedure TVoiceFilter.FlushInternal;
  begin
    FInput.Flush;
    dmo_vcfilter_free_output(filter);
    dmo_vcfilter_free(filter);
  end;

  procedure TVoiceFilter.SetOutSampleRate(value: Word);
  begin
    FOutSampleRate := 8000;
    if value > 8000 then
      FOutSampleRate := 11025;
    if value > 11025 then
      FOutSampleRate := 16000;
    if value > 16000 then
      FOutSampleRate := 22050;
  end;


end.
