(*
  This file is a part of New Audio Components package 2.5
  Copyright (c) 2002-2008 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_AudioMix.pas 1165 2010-02-01 14:42:59Z andrei.borovsky $ *)

unit ACS_AudioMix;

(* Title: ACS_AudioMix
    Classes that mix audio. *)

interface

uses
  Classes, SysUtils, ACS_Types, FastCodeCPUID, ACS_Procs, ACS_Classes, SyncObjs, Math;

const
  BUF_SIZE = $1000;
  amMaxVolume = High(Word);

type

  (* Enum: TAudioMixerMode
    This enumeration represents the different modes of operation for a 
    <TAudioMixer>.
      
      amMix - the mixer mixes input streams and the size of the resulting
      stream is equal to the size of the longest input stream. 
      
      amConcatenate - the two streams are concatenated together and the size
      of the resulting stream is the sum of the sizes of the input streams.
      Input1 is written before Input2.
  *)

  TAudioMixerMode = (amMix, amConcatenate);

  (* Class: TAudioMixer
     This component can mix or concatenate two input audio streams. Unlike
     other stream converter components, TAudioMixer component has two input
     properties: Input1 and Input2. The input streams should have the same
     number of channels and sample rates (but may have a different number of
     bits per sample). Note that input streams may be of different sizes. In
     amMix mode the streams start at the same time, but the longer stream will
     play alone after the shorter stream has ended. In amConcatenate mode the
     second input will play after the first input has ended. Volume1 and Volume2 properties
     can contronl level of the first and second input respectively in both mixing and concatenation modes.

     This cpmonent decends from <TAuInput>. *)
   
  TAudioMixer = class(TAuInput)
  private
    FInput1, FInput2 : TAuInput;
    EndOfInput1, EndOfInput2 : Boolean;
    FVolume1, FVolume2 : Word;
    InBuf1, InBuf2 : PBuffer8;
    FloatBuf1, FloatBuf2 : PBufferSingle;
    BytesPerSample1, BytesPerSample2 : Byte;
    Busy : Boolean;
    FMode : TAudioMixerMode;
    FInput2Start: Int64;
    SamplesCount : Int64;
    procedure SetInput1(aInput : TAuInput);
    procedure SetInput2(aInput : TAuInput);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Input1
    Use this property to set the first input stream to be mixed or concatenated. *)
    property Input1 : TAuInput read FInput1 write SetInput1;
    (* Property: Input2
    Use this property to set the second input stream to be mixed or concatenated. *)
    property Input2 : TAuInput read FInput2 write SetInput2;
    (* Property: Mode
     This property sets the mode for the TAudioMixer.
     The possible values for this property are <amMix> and <amConcatenate>.
    *)
    property Mode : TAudioMixerMode read FMode write FMode;
   (*property Input2StartSample:
      The delay in samples before the second input starts playing (in the amMix mode) *)
     property Input2StartSample : Int64 read FInput2Start write FInput2Start;
    (*property Volume1:
      The volume of the first input. The value of 0 means silence. The maximum possible integer for this property
       corresponds to playback at the original level. There is the amMaxVolume constant that holds this value. *)
    property Volume1 : Word read FVolume1 write FVolume1;
    (*property Volume2:
      The volume of the second input. The value of 0 means silence. The maximum possible integer for this property
       corresponds to playback at the original level. There is the amMaxVolume constant that holds this value. *)
    property Volume2 : Word read FVolume2 write FVolume2;
  end;

  (* Class: TRealTimeMixer
     This component can mix two input audio streams in real timr. Unlike
     other stream converter components, TRealTime component has two input
     properties: Input1 and Input2. The input streams should have the same
     number of channels and sample rates (but may have a different number of
     bits per sample).

     The difference between TRealTimeMixer and TAudioMixer is that you can change the inputs while the mixer is running.
     In fact once the real time mixer is started it will be generating output untill it is stopped explicitly.
     The mixer will generate silens if it has no input currently. See the docs below on how to assign inputs to the real time mixer.
     Volume1 and Volume2 properties  can contronl level of the first and second input respectively.

     This cpmonent decends from <TAuInput>. *)

  TRealTimeMixer = class(TAuInput)
  private
    ReadInput1, ReadInput2 : Boolean;
    FInput1, FInput2 : TAuInput;
    FloatBuf1P, FloatBuf2P : Pointer;
    EndOfInput1, EndOfInput2 : Boolean;
    FVolume1, FVolume2 : Word;
    InBuf1, InBuf2 : PBuffer8;
    FloatBuf1, FloatBuf2 : PBufferSingle;
    BytesPerSample1, BytesPerSample2 : Byte;
    Busy : Boolean;
    SamplesCount : Int64;
    FOutSampleRate : LongWord;
    FOutBitsPerSample, FOutBytesPerSample : Byte;
    FOutChannels : Byte;
    procedure SetInput1(aInput : TAuInput);
    procedure SetInput2(aInput : TAuInput);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Input1
    Use this property to set the first input stream to be mixed.
    Assigning input to the real time mixer is tricky. The component assigned to the mixer as an input should be ready to provide data at once.
    For example if you assign a file input component as a mixing input, the file name should be assigned first to that input component.
    If you want to change the file to be mixed on the fly without changing the input component t could lok like this:

    > WaveIn1.FileName := 'File.wav';
    > RealTimeMixer1.Input1 := WaveIn1;
    ...
    > RealTimeMixer1.Input1 := nil;
    > WaveIn1.FileName := 'NewFile.wav';
    > RealTimeMixer1.Input1 := WaveIn1;

    Assigning nil to Input1 or Input2 makes the correspondent channel silent until you assign to it something else. *)
    property Input1 : TAuInput read FInput1 write SetInput1;
    (* Property: Input2
    Use this property to set the second input stream to be mixed or concatenated.
    See the note at <Input1> *)
    property Input2 : TAuInput read FInput2 write SetInput2;
    (* Property: OutSampleRate
    Use this property to set the output sample rate for the mixer. All the mixer inputs should have this sample rate.
    IF YOU FORGET TO SET THIS PROPERTY MULTIPLE ERRORS MAY OCCUR. *)
    property OutSampleRate : LongWord read FOutSampleRate write FOutSampleRate;
    (* Property: OutBitsPerSample
    Use this property to set the output number of bits per sample for the mixer. The mixer inputs may have different bit depths.
    IF YOU FORGET TO SET THIS PROPERTY THE ZERO-DIVISION ERROR WILL OCCUR. *)
    property OutBitsPerSample : Byte read FOutBitsPerSample write FOutBitsPerSample;
    (* Property: OutChannels
    Use this property to set the number of channels for the mixer. All the mixer inputs should have the same number of channels.
    IF YOU FORGET TO SET THIS PROPERTY THE ZERO-DIVISION ERROR WILL OCCUR. *)
    property OutChannels : Byte read FOutChannels write FOutChannels;
    (*property Volume1:
      The volume of the first input (in the amMix mode) *)
    property Volume1 : Word read FVolume1 write FVolume1;
    (*property Volume2:
      The volume of the second input (in the amMix mode) *)
    property Volume2 : Word read FVolume2 write FVolume2;
  end;

implementation

constructor TAudioMixer.Create;
begin
  inherited Create(AOwner);
  FInput2Start := 0;
  FVolume1 := amMaxVolume;
  FVolume2 := amMaxVolume;
end;

  destructor TAudioMixer.Destroy;
  begin
    inherited Destroy;
  end;

  function TAudioMixer.GetBPS;
  begin
    if not Assigned(FInput1) then
    raise EAuException.Create('Input not assigned');
    Result := FInput1.BitsPerSample;
  end;

  function TAudioMixer.GetCh;
  begin
    if not Assigned(FInput1) then
    raise EAuException.Create('Input not assigned');
    Result:= FInput1.Channels;
  end;

  function TAudioMixer.GetSR;
  begin
    if not Assigned(FInput1) then
    raise EAuException.Create('Input not assigned');
    Result := FInput1.SampleRate;
  end;

  function _Min(X, Y : Int64) : Int64;
  begin
    if X < Y then Result := X
    else Result := Y;
  end;

  procedure TAudioMixer.InitInternal;
  var
    SamplesCount1, SamplesCount2  : Int64;
  begin
    Busy := True;
    FPosition := 0;
    SamplesCount := 0;
    EndOfInput1 := False;
    EndOfInput2 := False;
    if not Assigned(FInput1) then raise
      EAuException.Create('Input1 is not assigned');
    if not Assigned(FInput2) then raise
      EAuException.Create('Input2 is not assigned');
    FInput1.Init;
    FInput2.Init;
    BytesPerSample1 := (FInput1.BitsPerSample div 8);
    BytesPerSample2 := (FInput2.BitsPerSample div 8);
    SamplesCount1 := FInput1.Size div BytesPerSample1;
    if FMode = amMix then
    begin
      SamplesCount2 := FInput2.Size div BytesPerSample2 + FInput2Start;
      FSize := SamplesCount1 + SamplesCount2 - _Min(SamplesCount1, SamplesCount2)
    end else
    begin
      SamplesCount2 := FInput2.Size div BytesPerSample2;
      FSize := SamplesCount1 + SamplesCount2;
    end;
    FSize := FSize * BytesPerSample1;
    GetMem(InBuf1, BUF_SIZE * BytesPerSample1);
    GetMem(InBuf2, BUF_SIZE * BytesPerSample2);
    GetMem(FloatBuf1, BUF_SIZE * SizeOf(Single));
    GetMem(FloatBuf2, BUF_SIZE * SizeOf(Single));
  end;

  procedure TAudioMixer.FlushInternal;
  begin
    FInput1.Flush;
    FInput2.Flush;
    FreeMem(InBuf1, BUF_SIZE * BytesPerSample1);
    FreeMem(InBuf2, BUF_SIZE * BytesPerSample2);
    FreeMem(FloatBuf1);
    FreeMem(FloatBuf2);
    Busy := False;
  end;

  procedure MixLive_SSE_4(Op1, Op2 : PSingle; coeff1, coeff2 : Int64; DataSize : Integer);
  asm
    PUSH ESI;
    PUSH EDI;
    MOV ECX, DataSize;
    MOV ESI, Op1;
    MOV EDI, Op2;
    MOVLPS XMM2, QWORD PTR Coeff1;
    MOVLHPS XMM2, XMM2;
    MOVLPS XMM3, QWORD PTR Coeff2;
    MOVLHPS XMM3, XMM3;
    SUB EDI, ESI;
    @loop:
    MOVAPS XMM0, DQWORD PTR [ESI];
    MULPS XMM0, XMM2;
    MOVAPS XMM1, DQWORD PTR [ESI+EDI];
    MULPS XMM1, XMM3;
    ADDPS  XMM0, XMM1;
    MOVAPS DQWORD PTR [ESI], XMM0;
    ADD ESI, 16;
    SUB ECX, 4;
    JNE @loop;
    POP EDI;
    POP ESI;
  end;

  procedure TAudioMixer.GetDataInternal;
  var
    l : LongWord;
    i : Integer;
    Bytes1, Samples1 : LongWord;
    Bytes2, Samples2 : LongWord;
    SamplesReq : LongWord;
    v1, v2 : Single;
  begin
    if EndOfInput1 and EndOfInput2 then
    begin
      Bytes := 0;
      Buffer := nil;
      Exit;
    end;
    SamplesReq := BUF_SIZE;
    if Bytes div BytesPerSample1 < SamplesReq then SamplesReq := Bytes div BytesPerSample1;
    Bytes1 := SamplesReq * BytesPerSample1;
    Bytes2 := SamplesReq * BytesPerSample2;
    v1 := FVolume1 / High(Word);
    v2 := FVolume2 / High(Word);
    if FMode = amMix then
    begin
      FillChar(InBuf1^, Bytes1, 0);
      FillChar(FloatBuf1^, SamplesReq * SizeOf(Single), 0);
      FillChar(FloatBuf2^, SamplesReq * SizeOf(Single), 0);
      l := 0;
      if not EndOfInput1 then
       l := FInput1.FillBuffer(InBuf1, Bytes1, EndOfInput1);
      Samples1 := l div BytesPerSample1;
      Inc(SamplesCount, Samples1);
      FillChar(InBuf2^, Bytes2, 0);
      l := 0;
      if SamplesCount >= FInput2Start*FInput2.Channels then
        if not EndOfInput2 then
          l := FInput2.FillBuffer(InBuf2, Bytes2, EndOfInput2);
      Samples2 := l div BytesPerSample2;
      case BytesPerSample1 of
        1 : ByteToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
        2 : SmallIntToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
        3 : Int24ToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
        4 : Int32ToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
      end;
      case BytesPerSample2 of
        1 : ByteToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
        2 : SmallIntToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
        3 : Int24ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
        4 : Int32ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
      end;
      for i := 0 to BUF_SIZE - 1 do
        FloatBuf1[i] := (FloatBuf1[i]*v1 + FloatBuf2[i]*v2)/2;
      case BytesPerSample1 of
        1 : SingleToByte(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
        2 : SingleToSmallInt(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
        3 : SingleToInt24(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
        4 : SingleToInt32(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
      end;
      if Samples1 > Samples2 then Bytes := Samples1*BytesPerSample1
      else Bytes := Samples2*BytesPerSample1;
      Buffer := InBuf1;
    end else
    begin
      if not EndOfInput1 then
      begin
        l := FInput1.FillBuffer(InBuf2, Bytes2, EndOfInput1);
        Samples1 := l div BytesPerSample1;
        case BytesPerSample1 of
          1 : ByteToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
          2 : SmallIntToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
          3 : Int24ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
          4 : Int32ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
        end;
        for i := 0 to Samples1 - 1 do
          FloatBuf2[i] := FloatBuf2[i]*v1;
        case BytesPerSample1 of
          1 : SingleToByte(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
          2 : SingleToSmallInt(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
          3 : SingleToInt24(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
          4 : SingleToInt32(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
        end;
        Bytes := Samples1*BytesPerSample1;
        Buffer := InBuf1;
        if l <> 0 then Exit;
      end;
      if not EndOfInput2 then
      begin
        l := FInput2.FillBuffer(InBuf2, Bytes2, EndOfInput2);
        Samples2 := l div BytesPerSample2;
        case BytesPerSample2 of
          1 : ByteToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
          2 : SmallIntToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
          3 : Int24ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
          4 : Int32ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
        end;
        for i := 0 to Samples2 - 1 do
          FloatBuf2[i] := FloatBuf2[i]*v2;
        case BytesPerSample1 of
          1 : SingleToByte(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
          2 : SingleToSmallInt(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
          3 : SingleToInt24(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
          4 : SingleToInt32(FloatBuf2, Pointer(InBuf1), BUF_SIZE);
        end;
        Bytes := Samples2*BytesPerSample1;
        Buffer := InBuf1;
      end;
    end;
  end;

  procedure TAudioMixer.SetInput1;
  begin
    if Busy then
    raise EAuException.Create('The component is busy.');
    FInput1 := aInput;
  end;

  procedure TAudioMixer.SetInput2;
  begin
    if Busy then
    raise EAuException.Create('The component is busy.');
    FInput2 := aInput;
  end;

  procedure TRealTimeMixer.InitInternal;
  begin
    ReadInput1 := True;
    ReadInput2 := True;
    Busy := True;
    FPosition := 0;
    SamplesCount := 0;
    EndOfInput1 := False;
    EndOfInput2 := False;
    FSize := -1;
    FOutBytesPerSample := FOutBitsPerSample div 8;
    if Assigned(FInput1) then
    begin
      FInput1.Init;
      EndOfInput1 := False;
      BytesPerSample1 := FInput1.BitsPerSample div 8;
    end else
      BytesPerSample1 := FOutBytesPerSample;
    GetMem(InBuf1, BUF_SIZE * BytesPerSample1);
    if Assigned(FInput2) then
    begin
      FInput2.Init;
      EndOfInput2 := False;
      BytesPerSample2 := FInput2.BitsPerSample div 8;
    end else
      BytesPerSample2 := FOutBytesPerSample;
    GetMem(InBuf2, BUF_SIZE * BytesPerSample2);
    GetMem(FloatBuf1P, BUF_SIZE * SizeOf(Single)+15);
    GetMem(FloatBuf2P, BUF_SIZE * SizeOf(Single)+15);
    LongWord(FloatBuf1) := LongWord(FloatBuf1P) + (16 - (LongWord(FloatBuf1P) mod 16));
    LongWord(FloatBuf2) := LongWord(FloatBuf2P) + (16 - (LongWord(FloatBuf2P) mod 16));
  end;

  procedure TRealTimeMixer.GetDataInternal;
  var
    l : LongWord;
    i : Integer;
    Bytes1, Samples1 : LongWord;
    Bytes2, Samples2 : LongWord;
    SamplesReq : LongWord;
    v1, v2 : Single;
    v11, v12 : array[0..1] of Single;
  begin
    if EndOfInput1 then
    begin
      EndOfInput1 := False;
      ReadInput1 := False;
//      FInput1.Flush;
      FreeMem(InBuf1);
//      FInput1 := nil;
      BytesPerSample1 := FOutBytesPerSample;
      GetMem(InBuf1, BUF_SIZE * BytesPerSample1);
    end;
    if EndOfInput2 then
    begin
      EndOfInput2 := False;
      ReadInput2 := False;
//      FInput2.Flush;
      FreeMem(InBuf2);
//      FInput2 := nil;
      BytesPerSample2 := FOutBytesPerSample;
      GetMem(InBuf2, BUF_SIZE * BytesPerSample2);
    end;
    SamplesReq := Bytes div FOutBytesPerSample;
    if SamplesReq > BUF_SIZE then SamplesReq := BUF_SIZE;
    Bytes1 := SamplesReq * BytesPerSample1;
    Bytes2 := SamplesReq * BytesPerSample2;
    v1 := FVolume1 / High(Word);
    v2 := FVolume2 / High(Word);
    FillChar(InBuf1^, Bytes1, 0);
    FillChar(FloatBuf1^, SamplesReq * SizeOf(Single), 0);
    FillChar(FloatBuf2^, SamplesReq * SizeOf(Single), 0);
    l := Bytes1;
    if ReadInput1 then
       l := FInput1.FillBuffer(InBuf1, Bytes1, EndOfInput1);
    Samples1 := l div BytesPerSample1;
    Inc(SamplesCount, Samples1);
    FillChar(InBuf2^, Bytes2, 0);
    l := Bytes2;
    if ReadInput2 then
        l := FInput2.FillBuffer(InBuf2, Bytes2, EndOfInput2);
    Samples2 := l div BytesPerSample2;
    case BytesPerSample1 of
      1 : ByteToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
      2 : SmallIntToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
      3 : Int24ToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
      4 : Int32ToSingle(Pointer(InBuf1), FloatBuf1, BUF_SIZE);
    end;
    case BytesPerSample2 of
      1 : ByteToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
      2 : SmallIntToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
      3 : Int24ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
      4 : Int32ToSingle(Pointer(InBuf2), FloatBuf2, BUF_SIZE);
    end;

      if isSSE in CPU.InstructionSupport then
      begin
        v11[0] := v1/2;
        v11[1] := v1/2;
        v12[0] := v2/2;
        v12[1] := v2/2;
        MixLive_SSE_4(@FloatBuf1[0], @FloatBuf2[0], Int64(v11), Int64(v12), BUF_SIZE);
      end else
    for i := 0 to BUF_SIZE - 1 do
      FloatBuf1[i] := (FloatBuf1[i]*v1 + FloatBuf2[i]*v2)/2;
    case FOutBytesPerSample of
      1 : SingleToByte(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
      2 : SingleToSmallInt(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
      3 : SingleToInt24(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
      4 : SingleToInt32(FloatBuf1, Pointer(InBuf1), BUF_SIZE);
    end;
    if Samples1 > Samples2 then Bytes := Samples1*FOutBytesPerSample
    else Bytes := Samples2*FOutBytesPerSample;
    Buffer := InBuf1;
  end;

  procedure TRealTimeMixer.FlushInternal;
  begin
    if Assigned(FInput1) then
      FInput1.Flush;
    if Assigned(FInput2) then
      FInput2.Flush;
//    FInput2 := nil;
//    FInput1 := nil;
    FreeMem(InBuf1, BUF_SIZE * BytesPerSample1);
    FreeMem(InBuf2, BUF_SIZE * BytesPerSample2);
    FreeMem(FloatBuf1P);
    FreeMem(FloatBuf2P);
    Busy := False;
  end;

  procedure TRealTimeMixer.SetInput1;
  begin
    if not (csDesigning in ComponentState) then
    begin
      DataCS.Enter;
      try
      if Busy then
      begin
        if Assigned(FInput1) then
        begin
          FInput1.Flush;
        end;
        FreeMem(InBuf1);
        FInput1 := aInput;
        if Assigned(FInput1) then
        begin
          FInput1.Init;
          EndOfInput1 := False;
          BytesPerSample1 := FInput1.BitsPerSample div 8;
        end else
        BytesPerSample1 := FOutBytesPerSample;
        GetMem(InBuf1, BUF_SIZE * BytesPerSample1);
      end else // if Busy then
        FInput1 := aInput;
      finally
      DataCS.Leave;
      end;
    end else // if not (csDesigning in ComponentState) then
      FInput1 := aInput;
  end;

  procedure TRealTimeMixer.SetInput2;
  begin
    if not (csDesigning in ComponentState) then
    begin
      DataCS.Enter;
      try
      if Busy then
      begin
        if Assigned(FInput2) then
        begin
          FInput2.Flush;
        end;
        FreeMem(InBuf2);
        FInput2 := aInput;
        if Assigned(FInput2) then
        begin
          FInput2.Init;
          EndOfInput2 := False;
          BytesPerSample2 := FInput2.BitsPerSample div 8;
        end else
          BytesPerSample2 := FOutBytesPerSample;
        GetMem(InBuf2, BUF_SIZE * BytesPerSample2);
      end else // if Busy then
        FInput2 := aInput;
      finally
      DataCS.Leave;
      end;
    end else // if not (csDesigning in ComponentState) then
    FInput2 := aInput;
  end;

  function TRealTimeMixer.GetBPS;
  begin
    Result := FOutBitsPerSample;
  end;

  function TRealTimeMixer.GetCh;
  begin
    Result:= FOutChannels;
  end;

  function TRealTimeMixer.GetSR;
  begin
    Result := FOutSampleRate;
  end;

  constructor TRealTimeMixer.Create;
  begin
    inherited Create(AOwner);
    FVolume1 := amMaxVolume;
    FVolume2 := amMaxVolume;
  end;

  destructor TRealTimeMixer.Destroy;
  begin
    inherited Destroy;
  end;


end.
