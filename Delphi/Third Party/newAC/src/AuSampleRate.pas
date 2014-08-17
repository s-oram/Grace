(*
  This file is a part of New Audio Components package v 1.7
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: AuSampleRate.pas 652 2008-07-04 06:21:49Z andrei.borovsky $ *)

unit AuSampleRate;

(* Title: AuSampleRate
    Components which implement resampling of audio data. *)

interface

uses

  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes, libsamplerate, Math;

const

  InitialBufferSize = $180000; // Internal constants
  IOBufSize = $300000;

type


  TResamplerQuality = (rqBest, rqMedium, rqFastest);

  (* Class: TResampler
     A high-quality audio resampler.
     Descends from <TAuConverter>.
     Requires libsamplerate.dll.
     More information on this resampler can be found at http://www.mega-nerd.com/SRC/.
     This component can convert 8, 16, 24, 32 bps audio with one or more channels.
     If <OutSampleRate> proprty is set to the same value as the input sample rate,
     the component switches into pass-through mode when all the input is passed unmodified.
*)

  TResampler = class(TAuConverter)
  private
    FPassThrough : Boolean;
    _State : Pointer;
    Data : SRC_DATA;
    FOutSampleRate : LongWord;
    FQuality : TResamplerQuality;
    InputBuffer : array [0..IOBufSize - 1] of Byte;
    IBufferEnd : Integer; // Points to the position after the last byte of the array
    OutputBuffer : array [0..IOBufSize - 1] of Byte;
    OBufferStart, // Points to the position to read from
    OBufferEnd : LongWord; // Points to the position after the last byte of the array
    IFloatBuffer : array[0..InitialBufferSize - 1] of Single;
    OFloatBuffer : array[0..IOBufSize - 1] of Single;
    EndOfInput : Boolean;
    procedure SetOutSampleRate(aSR : LongWord);
  protected
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Quality
       Use this property to set the trade-off between resampling quality and
       speed. Possible values are <rqBest>, <rqMedium>, and <rqFastest>, of
       which rqBest sets the best quality but slow resampling time and
       rqFastest sets the lower (but still very good) quality and the fastest
       resampling time. *)
    property Quality : TResamplerQuality read FQuality write FQuality;
    (* Property: OutSampleRate
      Use this property to set the sample rate for the resulting audio stream.
      Valid values range from 2000 to 120000 and include the special value of
      0. If you set the output sample rate to the same value as the input
      sample rate, or set it to 0, the component will switch to a pass-through
      mode. In this mode all the input will be passed on unchanged. This
      feature is useful when chaining the TResampler component wih other
      components in the chain that may not always need resampled data. *)
    property OutSampleRate : LongWord read FOutSampleRate write SetOutSampleRate;
  end;

  (* Enum: TResamplerQuality
    
      rqBest - High quality, but slowest resampling time.
      rqMedium - Medium quality
      rqFastest - Lowest quality, but still not too bad. Also the fastest resampling time.
  *)
  
implementation

  constructor TResampler.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TResampler.Destroy;
  begin
    inherited Destroy;
  end;


  function  TResampler.GetSR;
  begin
    Result := FOutSampleRate;
    if FOutSampleRate = 0 then
       Result := FInput.SampleRate;
  end;

  procedure TResampler.SetOutSampleRate;
  begin
    if ((aSR >= 2000) and (aSR <= 120000)) or (aSR = 0) then
      FOutSampleRate := aSR
    else
      FOutSampleRate := 44100;
  end;

  procedure TResampler.InitInternal;
  var
    Quality, error : Integer;
  begin
    LoadLibsamplerate;
    if not LibsamplerateLoaded then
      raise EAuException.Create(LibsampleratePath + ' library could not be loaded.');
    if Busy then
      raise EAuException.Create('The component is busy.');
    EndOfInput := False;
    IBufferEnd := 0;
    OBufferStart := 0;
    OBufferEnd := 0;
    Busy := True;
    FInput.Init;
    FPosition := 0;
    Data.src_ratio := GetSR/Finput.SampleRate;
    if src_is_valid_ratio(Data.src_ratio) = 0 then
      raise EAuException.Create(Format('Frequences ratio %d is invalid', [Data.src_ratio]));
    Data.data_in := @IFloatBuffer;
    Data.data_out := @OFloatBuffer;
    case FQuality of
      rqBest : Quality := SRC_SINC_BEST_QUALITY;
      rqMedium : Quality := SRC_SINC_MEDIUM_QUALITY;
      rqFastest : Quality := SRC_SINC_FASTEST;
      else Quality := SRC_SINC_MEDIUM_QUALITY;
    end;
    _State := src_new(Quality, FInput.Channels, error);
    if error <> 0 then
      raise EAuException.Create('Failed to initialize the resampler');
   // src_set_ratio(_State, FOutSampleRate/Finput.SampleRate);
   if GetSR = FInput.SampleRate then
     FPassThrough := True
   else
    FPassThrough := False;
    if FPassThrough or (FInput.Size = -1) then
      FSize := FInput.Size
    else
    begin
      FSize := Round(FInput.Size * GetSR/Finput.SampleRate);
      FSize := FSize - (FSize mod (Finput.BitsPerSample* FInput.Channels div 8));
    end;
  end;

  procedure TResampler.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  var
    l, ilen, i : LongWord;
    res : Integer;
  begin
    if FPassThrough then
    begin
    // Pass through.
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    if OBufferStart >= OBufferEnd then
    begin
      OBufferStart := 0;
//      IOBufSize := IOBufSize - (IOBufSize mod ((Finput.BitsPerSample div 8) * FInput.Channels));
      if (IBufferEnd < IOBufSize) and (not EndOfInput) then
      begin
        if FInput.BitsPerSample <>8 then
          l := FInput.CopyData(@InputBuffer[IBufferEnd], IOBufSize - IBufferEnd)
        else
        begin
          l := FInput.CopyData(@InputBuffer[IBufferEnd], (IOBufSize - IBufferEnd) div 2);
          Convert8To16(@InputBuffer[IBufferEnd], l);
          l := l*2;
        end;
        if l = 0 then
        begin
          l := IOBufSize - IBufferEnd;
          FillChar(InputBuffer[IBufferEnd], l, 0);
        end;
           //EndOfInput := True;
        Inc(IBufferEnd, l);
      end;
      if IBufferEnd = 0 then
      begin
        EndOfInput := True;
        if FSize > FPosition then
          OBufferEnd := FSize - FPosition
        else
          OBufferEnd := 0;
        FillChar(OutputBuffer[0], OBufferEnd, 0);
      end else
      begin
        ilen := IBufferEnd;
        if IBufferEnd > InitialBufferSize then
          ilen := InitialBufferSize;
        if FInput.BitsPerSample < 24 then
        begin
          src_short_to_float_array(@InputBuffer, @IFloatBuffer, ilen);
          Data.input_frames := (ilen div 2) div FInput.Channels;
          Data.output_frames := (IOBufSize div 2) div FInput.Channels;
        end else
        if FInput.BitsPerSample = 24 then
        begin
          Int24ToSingle(@InputBuffer, @IFloatBuffer, ilen div 3);
          Data.input_frames := (ilen div 3) div FInput.Channels;
          Data.output_frames := (IOBufSize div 3) div FInput.Channels;
        end else
        begin
          Int32ToSingle(@InputBuffer, @IFloatBuffer, ilen div 4);
          Data.input_frames := (ilen div 4) div FInput.Channels;
          Data.output_frames := (IOBufSize div 4) div FInput.Channels;
        end;
        if EndOfInput and (IBufferEnd <= InitialBufferSize) then
          Data.end_of_input := 1
        else
          Data.end_of_input := 0;
        res := src_process(_State, Data);
        if res <> 0 then
        begin
          EndOfInput := True;
          Buffer := nil;
          Bytes := 0;
          raise EAuException.Create(src_strerror(res));
        end;
        if FInput.BitsPerSample < 24 then
        begin
          SingleToSmallInt(@OFloatBuffer, @OutputBuffer, Data.output_frames_gen * FInput.Channels);
          //src_float_to_short_array(@OFloatBuffer, @OutputBuffer, Data.output_frames_gen * FInput.Channels);
          OBufferEnd := Data.output_frames_gen * FInput.Channels * 2;
          ilen := Data.nput_frames_used  * FInput.Channels * 2;
          for i := ilen to IBufferEnd - 1 do
            InputBuffer[i - ilen] := InputBuffer[i];
          Dec(IBufferEnd, ilen);
          if FInput.BitsPerSample = 8 then
          begin
            Convert16To8(@OutputBuffer, OBufferEnd);
            OBufferEnd := OBufferEnd div 2;
          end;
        end else
        if FInput.BitsPerSample = 24 then
        begin
          SingleToInt24(@OFloatBuffer, @OutputBuffer, Data.output_frames_gen * FInput.Channels);
          OBufferEnd := Data.output_frames_gen * FInput.Channels * 3;
          ilen := Data.nput_frames_used  * FInput.Channels * 3;
          for i := ilen to IBufferEnd - 1 do
            InputBuffer[i - ilen] := InputBuffer[i];
          Dec(IBufferEnd, ilen);
        end else
        begin
          SingleToInt32(@OFloatBuffer, @OutputBuffer, Data.output_frames_gen * FInput.Channels);
          OBufferEnd := Data.output_frames_gen * FInput.Channels * 4;
          ilen := Data.nput_frames_used  * FInput.Channels * 4;
          for i := ilen to IBufferEnd - 1 do
            InputBuffer[i - ilen] := InputBuffer[i];
          Dec(IBufferEnd, ilen);
        end;
      end;
    end; // OBufferStart >= OBufferEnd
    if OBufferEnd = 0 then
    begin
      EndOfInput := True;
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    if Bytes > OBufferEnd - OBufferStart then
      Bytes := OBufferEnd - OBufferStart;
    Buffer := @OutputBuffer[OBufferStart];
    Inc(OBufferStart, Bytes);
  end;

  procedure TResampler.FlushInternal;
  begin
//    FSize := 0;
    FInput.Flush;
    src_delete(_State);
    Busy := False;
  end;


end.
