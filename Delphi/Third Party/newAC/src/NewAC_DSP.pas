(*
  This file is a part of New Audio Components package v 1.8
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: NewAC_DSP.pas 1259 2014-03-13 07:55:46Z sleuthhound@gmail.com $ *)


unit NewAC_DSP;

(* Title: NewAC_DSP
    This unit contains components performing various DSP related tasks.

    (c) 2008 Andrei Borovsky (anb@symmetrica.net). All rights reserved. See the
    LICENSE file for more details. *)

{$I NewAC.inc}

interface

uses
  Windows, Classes, SysUtils, FastMove, ACS_Classes, ACS_Procs, ACS_Types, FFTReal, Math, GainAnalysis;

const
  BufSize : Integer = $6000;

type

(* Class: TFrequencyAnalysis
     A descendent of <TAuOutput> which generates input's frequency spectrum
     using averaged real DFT.

     TFrequencyAnalysis is an output component, but unlike other output components
     it doesn't provide audio data. TFrequencyAnalysis's output is an audio
     frequency spectrum. TFrequencyAnalysis normalizes its input values so all the output frequency and power values are normalized. *)

  TFrequencyAnalysis = class(TAuOutput)
  private
    _N : Word;
    Core : TFFTReal;
    FWindow : TFilterWindowType;
    FStartSample, FEndSample : Int64;
    FMagnitude : array[0..7] of array of Single;
    TmpData, OutData, InputData, W : array of Single;
    MaxChannels : Integer;
    FDataSize : Int64;
    _Buffer : PByte;
    FNumSamples : Int64;
    FCurSample : Int64;
    ChunkCount : Integer;
    FSeparator : Char;
    function GetMagnitude(Channel, Index : Word) : Single;
    function GetLogMagnitude(Channel, Index : Word) : Single;
    function GetPower(Channel, Index : Word) : Single;
    function GetLogPower(Channel, Index : Word) : Single;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: SaveMagnitude
       This method saves magnitude series obtained at channel Channel to the
       file specified by FileName. *)
    procedure SaveMagnitude(Channel : Word; const FileName : String);
    (* Function: SaveLogMagnitude
       This method saves logarithmed magnitude series obtained at channel
       Channel to the file specified by FileName. *)
    procedure SaveLogMagnitude(Channel : Word; const FileName : String);
    (* Property: Magnitude
       Returns the value of the magnitude specified by channel number and
       index. Valid indeces range from 0 to <N>/2. *)
    property Magnitude[Channel, Index : Word] : Single read GetMagnitude;
    (* Property: LogMagnitude
       Returns the logarithm of the magnitude specified by channel number and
       index. Valid indeces range from 0 to <N>/2. *)
    property LogMagnitude[Channel, Index : Word] : Single read GetLogMagnitude;
    (* Property: Power
       Returns the power (square of magnitude) specified by channel number and
       index. Valid indeces range from 0 to <N>/2. *)
    property Power[Channel, Index : Word] : Single read GetPower;
    (* Property: LogMagnitude
       Returns the logarithm of the power specified by channel number and
       index. Valid indeces range from 0 to <N>/2. *)
    property LogPower[Channel, Index : Word] : Single read GetLogPower;
    (* property: Separator
       Use this property to specify the character used to delimit the values
       being saved to a file. *)
    property Separator : Char read FSeparator write FSeparator;
  published
    (* Property: N
       The number of input points for performing real DFT.
       Magnitude calculation produces N/2 + 1 values that represent the frequency
       distrbution between 0 and samplerate/2. *)
    property N : Word read _N write _N;
    (* Property: Window
       Use this property to select the type of the window applied to the input data. *)
    property Window : TFilterWindowType read FWindow write FWindow;
    property StartSample : Int64 read FStartSample write FStartSample;
    property EndSample : Int64 read FEndSample write FEndSample;
  end;

(* Class: TConvolver
     This component performs convolution.
     Descends from <TAuOutput>.*)

  TConvolver = class(TAuConverter)
  private
    Kernel : array of Single;
    InputBuffer, OutputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: SetKernel
       Call this method to set the convolution kernel (impulse response
       function). *)
    procedure SetKernel(const K : array of Single);
  end;

  (* Class: TDifferenceEquation
     This component calculates its output following the equation y[n] = a0*x[n]
     + a1*x[n-1] + ... + b0*y[n-1] + b1*y[n-2]...

     Descends from <TAuConverter>.
  *)

  TDifferenceEquation = class(TAuConverter)
  private
    _A, _B : array of Single;
    X, Y : array[0..7] of array of Single;
    offsX, OffsY : Integer;
    InputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: SetCoefficients
       Sets coefficients for the equation.
       A is a vector of a0...an, B is a vector of b0...bn.
       The same set of coefficients is applied to all input channels.
    *)
    procedure SetCoefficients(const A, B : array of Single);
  end;


    TGCState = (gcPassing, gcSkipping);

   {$IF CompilerVersion < 20}
    const
       lFilterLength = 8;
     type
    {$IFEND}


  (* Class: TGainProcessor
     This component processes audio data passing throug depending on tge gain level.
     Carrently it can detect the periods of silence and skip them (like TVoiceFilter does on Windows Vista and above).
     The component requires libgain.dll.
     Descends from <TAuConverter>.
  *)

    TGainProcessor = class(TAuConverter)
    private
     {$IF CompilerVersion >= 20}
      const
         lFilterLength = 8;
      var
      {$IFEND}
      FSkipSilenceEnabled : Boolean;
      TmpBuffer1, TmpBuffer2 : array of Byte;
      InBuffer : array of Single;
      LBuffer, RBuffer : array of Double;
      FBufferSize, FramesInBuffer : Word;
      FOffset : Word;
      FIsEndOdSource : Boolean;
      FrameSize : Word;
      FCount : LongWord;
      FMinSilenceInterval, FMinSoundLevel : Word;
      FState : TGCState;
      FInternalState : Byte;
      FOnStateChanged : TGenericEvent;
      FLevel : Single;
      FFilterOffset : LongWord;
      FFilterBuffer : array[0..1, 0..lFilterLength-1] of Integer;
      FLastY : array[0..1] of Integer;
      procedure GetNewData;
//      procedure CalculateInternal(var Buffer : Pointer; var Bytes : LongWord);
    protected
      procedure InitInternal; override;
      procedure FlushInternal; override;
      procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    public
      constructor Create(AOwner: TComponent); override;
      (* Property: State
         Returns the current silence skipping state which is one of the two: gcPassing - a sound is detected the data is passing throug, gcSkipping - the silence is detected and the data doesn't pass through. *)
      property State : TGCState read FState;
    published
      (* Property: SkipSilenceEnabled
         Sets the selince-skipping mode on and off. *)
      property SkipSilenceEnabled : Boolean read FSkipSilenceEnabled write FSkipSilenceEnabled;
      (* Property: MinSilenceInterval
         Sets the interval for the silence detection in seconds. It the input is silent for more than this value, the component starts skipping further silence. *)
      property MinSilenceInterval : Word read FMinSilenceInterval write FMinSilenceInterval;
      (* Property: SilenceThreshold
         Sets the gain threshold to distinguish a sound from the silence. Values range from 0 to 100. *)
      property SilenceThreshold : Word read FMinSoundLevel write FMinSoundLevel;
      (* Property: OnStateChanged
         This event indicates thet the component's <State> has changed. Unlike most other NewAC events it is delivered synchronously, so you should avoid updating GUI directly from its handler. *)
      property OnStateChanged : TGenericEvent read FOnStateChanged write FOnStateChanged;
    end;


implementation

  constructor TFrequencyAnalysis.Create(AOwner: TComponent);
  begin
    inherited;
    FStartSample := 0;
    FEndSample := -1;
    MaxChannels := -1;
    FSeparator := ' ';
  end;

  destructor TFrequencyAnalysis.Destroy;
  begin
    inherited;
  end;

  procedure TFrequencyAnalysis.Prepare;
  var
    i : Integer;
  begin
    if FInput = nil then raise EAuException.Create('Input is not assigned');
    FInput.Init;
    MaxChannels := FInput.Channels -1;
    FDataSize := _N*(FInput.BitsPerSample div 8)*FInput.Channels;
    GetMem(_Buffer, FDataSize);
    SetLength(W, _N);
    case FWindow of
      fwHamming : HammingWindowS(@W[0], _N, False);
      fwHann : HannWindowS(@W[0], _N, False);
      fwBlackman : BlackmanWindowS(@W[0], _N, False);
    end;
    SetLength(TmpData, _N);
    SetLength(OutData, _N);
    SetLength(InputData, _N*(MaxChannels + 1));
    for i := 0 to MaxChannels do
    begin
      SetLength(FMagnitude[i], _N div 2 + 1);
      FillChar(FMagnitude[i][0], _N*2 + 4, 0);
    end;
    Core := TFFTReal.Create(_N);
    if FInput is TAuFileIn then
    begin
      TAuFileIn(FInput).Seek(FStartSample);
      if FEndSample = -1 then
        FNumSamples := Finput.TotalSamples - FStartSample
      else
        FNumSamples := FEndSample - FStartSample;
    end else
    FNumSamples := High(Int64) - 1;
    FCurSample := FStartSample;
    ChunkCount := 0;
  end;

  procedure TFrequencyAnalysis.Done;
  var
    i, j : Integer;
  begin
    FInput.Flush;
    FreeMem(_Buffer);
    SetLength(W, 0);
    SetLength(TmpData, 0);
    SetLength(InputData, 0);
    SetLength(OutData, 0);
    if ChunkCount <> 0 then
      for i := 0 to MaxChannels do
        for j := 0 to _N div 2 do
          FMagnitude[i][j] := FMagnitude[i][j]/(ChunkCount*_N);
    Core.Free;
  end;

  function TFrequencyAnalysis.DoOutput(Abort : Boolean) : Boolean;
  var
    FEOF : Boolean;
    Len : LongWord;
    i, j : Integer;
    P : PBufferSingle;
  begin
    if (FCurSample >= FStartSample + FNumSamples) or Abort then
    begin
      Result := False;
      Exit;
    end;
    Len := FInput.FillBuffer(Pointer(_Buffer), FDataSize, FEOF);
    if FEOF and (Len < FDataSize) then
    begin
      FCurSample := FStartSample + FNumSamples;
      Result := False;
      Exit;
    end;
    P := PBufferSingle(@InputData[0]);
    case FInput.BitsPerSample of
      8 : ByteToSingle(PBuffer8(_Buffer), P, _N*(MaxChannels + 1));
      16 : SmallIntToSingle(PBuffer16(_Buffer), P, _N*(MaxChannels + 1));
      24 : Int24ToSingle(PBuffer8(_Buffer), PBufferSingle(P), _N*(MaxChannels + 1));
      32 : Int32ToSingle(PBuffer32(_Buffer), PBufferSingle(P), _N*(MaxChannels + 1));
    end;
    for i := 0 to MaxChannels do
    begin
      for j := 0 to _N -1 do
         TmpData[j] := InputData[j*(MaxChannels + 1) + i];
      MultSingleArrays(@TmpData[0], @W[0], _N);
      Core.do_fft(@OutData[0], @TmpData[0]);
      for j := 1 to _N div 2 - 1 do
        FMagnitude[i][j] := FMagnitude[i][j] + Hypot(OutData[j], OutData[j + N div 2]);
      FMagnitude[i][0] := FMagnitude[i][0] + Abs(OutData[0]);
      FMagnitude[i][N div 2] := FMagnitude[i][N div 2] + Abs(OutData[N div 2]);
    end;
    Inc(FCurSample, _N);
    Inc(ChunkCount);
    Result := True;
  end;


  function TFrequencyAnalysis.GetMagnitude(Channel, Index : Word) : Single;
  begin
    Result := FMagnitude[Channel, Index];
  end;

  function TFrequencyAnalysis.GetPower(Channel, Index : Word) : Single;
  begin
    Result := Sqr(FMagnitude[Channel, Index]);
  end;

  function TFrequencyAnalysis.GetLogMagnitude(Channel, Index : Word) : Single;
  begin
    if GetMagnitude(Channel, Index) <> 0 then
      Result := Log10(GetMagnitude(Channel, Index))
    else
      Result := MinSingle;
  end;

  function TFrequencyAnalysis.GetLogPower(Channel, Index : Word) : Single;
  begin
    if GetMagnitude(Channel, Index) <> 0 then
      Result := 2*Log10(GetMagnitude(Channel, Index))
    else
      Result := MinSingle;
  end;

  procedure TFrequencyAnalysis.SaveMagnitude(Channel : Word; const FileName : String);
  var
    F : System.Text;
    i : Integer;
    OldSep : Char;
  begin
    System.Assign(F, FileName);
    System.Rewrite(F);
    OldSep := {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    for i := 0 to _N div 2 do
      Write(F, FloatToStrF(GetMagnitude(Channel, i), ffFixed, 7, 7), FSeparator);
    WriteLn(F, FloatToStrF(GetMagnitude(Channel, _N div 2 -1), ffFixed, 7, 7));
    {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}DecimalSeparator := OldSep;
    System.Close(F);
  end;


  procedure TFrequencyAnalysis.SaveLogMagnitude(Channel : Word; const FileName : String);
  var
    F : System.Text;
    i : Integer;
    OldSep : Char;
  begin
    System.Assign(F, FileName);
    System.Rewrite(F);
    OldSep := {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    for i := 0 to _N div 2 do
      Write(F, FloatToStrF(GetLogMagnitude(Channel, i), ffFixed, 7, 7), FSeparator);
    WriteLn(F, FloatToStrF(GetLogMagnitude(Channel, _N div 2 -1), ffFixed, 7, 7));
    {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}DecimalSeparator := OldSep;
    System.Close(F);
  end;

  constructor TConvolver.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TConvolver.Destroy;
  begin
    Inherited Destroy;
  end;

  function TConvolver.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TConvolver.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TConvolver.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TConvolver.InitInternal;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize*FInput.Channels;
    FPosition := 0;
    SetLength(_Buffer, BufSize);
    SetLength(InputBuffer, BufSize div SampleSize);
    {$WARNINGS OFF}
    SetLength(OutputBuffer, BufSize div SampleSize + (Length(Kernel) - 1)*FInput.Channels);
    {$WARNINGS ON}
    FillChar(OutputBuffer[0], Length(OutputBuffer)*SizeOf(Single), 0);
    BufStart := 0;
    BufEnd := 0;
    SamplesInFrame := Finput.Channels;
    FSize := FInput.Size;
  end;

  procedure TConvolver.FlushInternal;
  begin
    FInput.Flush;
    SetLength(_Buffer, 0);
    SetLength(InputBuffer, 0);
    SetLength(OutputBuffer, 0);
    Busy := False;
  end;

  procedure TConvolver.GetDataInternal;
  var
    i, j, k, SamplesRead, FramesRead : Integer;
    P : PBufferSingle;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := FInput.CopyData(@_Buffer[0], BufSize);
      if BufEnd = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      SamplesRead := BufEnd div SampleSize;
      FramesRead := BufEnd div FrameSize;
      P := PBufferSingle(@InputBuffer[0]);
      case SampleSize of
        1 : ByteToSingle(PBuffer8(_Buffer), P, SamplesRead);
        2 : SmallIntToSingle(PBuffer16(_Buffer), P, SamplesRead);
        3 : Int24ToSingle(PBuffer8(_Buffer), PBufferSingle(P), SamplesRead);
        4 : Int32ToSingle(PBuffer32(_Buffer), PBufferSingle(P), SamplesRead);
      end;
      for i := 0 to FramesRead -1 do
       for j := 0 to Length(Kernel) - 1 do
         for k := 0 to SamplesInFrame - 1 do
           OutputBuffer[(i + j)*SamplesInFrame + k] := OutputBuffer[(i + j)*SamplesInFrame + k] + InputBuffer[i*SamplesInFrame + k]*Kernel[j];
      P := PBufferSingle(@OutputBuffer[0]);
      case SampleSize of
        1 : SingleToByte(P, PBuffer8(_Buffer), SamplesRead);
        2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesRead);
        3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesRead);
        4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesRead);
      end;
      for i := 0 to SamplesRead - 1 do
        OutputBuffer[i] := 0;
      for i := 0 to (Length(Kernel) -1)*SamplesInFrame - 1 do
      begin
        OutputBuffer[i] := OutputBuffer[i + SamplesRead];
        OutputBuffer[i + SamplesRead] := 0;
      end;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TConvolver.SetKernel;
  var
    i : Integer;
  begin
    SetLength(Kernel, Length(K));
    for i := 0 to  Length(Kernel) - 1 do
      Kernel[i] := K[i];
  end;

  constructor TDifferenceEquation.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TDifferenceEquation.Destroy;
  begin
    Inherited Destroy;
  end;

  function TDifferenceEquation.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TDifferenceEquation.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TDifferenceEquation.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TDifferenceEquation.InitInternal;
  var
    i : Integer;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize*FInput.Channels;
    SamplesInFrame := Finput.Channels;
    FPosition := 0;
    SetLength(_Buffer, BufSize);
    SetLength(InputBuffer, BufSize div SampleSize);
    OffsX := Length(_A) - 1;
    OffsY := Length(_B);
    for i := 0 to SamplesInFrame - 1 do
    begin
      SetLength(X[i], BufSize div SampleSize + OffsX);
      FillChar(X[i][0], OffsX*SizeOf(Single), 0);
      SetLength(Y[i], BufSize div SampleSize + OffsY);
      FillChar(Y[i][0], OffsY*SizeOf(Single), 0);
    end;
    BufStart := 0;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TDifferenceEquation.FlushInternal;
  var
    i : Integer;
  begin
    FInput.Flush;
    SetLength(_Buffer, 0);
    SetLength(InputBuffer, 0);
    for i := 0 to SamplesInFrame - 1 do
    begin
      SetLength(X[i], 0);
      SetLength(Y[i], 0);
    end;
    Busy := False;
  end;

  procedure TDifferenceEquation.GetDataInternal;
  var
    i, j, SamplesRead, FramesRead : Integer;
    P : PBufferSingle;
    Acc : Single;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := FInput.CopyData(@_Buffer[0], BufSize);
      if BufEnd = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      SamplesRead := BufEnd div SampleSize;
      FramesRead := BufEnd div FrameSize;
      P := PBufferSingle(@InputBuffer[0]);
      case SampleSize of
        1 : ByteToSingle(PBuffer8(_Buffer), P, SamplesRead);
        2 : SmallIntToSingle(PBuffer16(_Buffer), P, SamplesRead);
        3 : Int24ToSingle(PBuffer8(_Buffer), PBufferSingle(P), SamplesRead);
        4 : Int32ToSingle(PBuffer32(_Buffer), PBufferSingle(P), SamplesRead);
      end;
      for i := 0 to SamplesRead -1 do
        X[i mod SamplesInFrame][OffsX + i div SamplesInFrame] := InputBuffer[i];
      for i := 0 to FramesRead -1 do
        for j :=  0 to SamplesInFrame - 1 do
        begin
          Acc := 0;
          MultAndSumSingleArrays(@(X[j][i]), @_A[0], Acc, OffsX + 1);
          MultAndSumSingleArrays(@(Y[j][i]), @_B[0], Acc, OffsY);
          Y[j][i + OffsY] := Acc;
        end;
      for i := OffsY to FramesRead + OffsY -1 do
        for j :=  0 to SamplesInFrame - 1 do
          InputBuffer[(i - OffsY)*SamplesInFrame + j] :=  Y[j][i];
      for j :=  0 to SamplesInFrame - 1 do
      begin
        for i := 0 to OffsX - 1 do
          X[j][i] := X[j][i + FramesRead];
        for i := 0 to OffsY - 1 do
          Y[j][i] := Y[j][i + FramesRead];
      end;
      P := PBufferSingle(@InputBuffer[0]);
      case SampleSize of
        1 : SingleToByte(P, PBuffer8(_Buffer), SamplesRead);
        2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesRead);
        3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesRead);
        4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesRead);
      end;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TDifferenceEquation.SetCoefficients;
  var
    i : Integer;
  begin
    SetLength(_A, Length(A));
    for i := 0 to  Length(A) - 1 do
      _A[i] := A[Length(A) - 1 - i];
    SetLength(_B, Length(B));
    for i := 0 to  Length(B) - 1 do
      _B[i] := B[Length(B) - 1 - i];
  end;


  constructor TGainProcessor.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FMinSilenceInterval := 1;
    FMinSoundLevel := 5;
  end;

  procedure TGainProcessor.GetNewData;
  var
    i : Integer;
  begin
    Move(TmpBuffer2[0], TmpBuffer1[0], FBufferSize);
    FillChar(TmpBuffer2[0], FBufferSize, 0);
    FOffset := 0;
    FInput.FillBuffer(@TmpBuffer2[0], FBufferSize, FIsEndOdSource);
    case FSampleSize of
      1 : ByteToSingle(PBuffer8(@TmpBuffer2[0]), @InBuffer[0], FBufferSize div FSampleSize);
      2 : SmallIntToSingle(PBuffer16(@TmpBuffer2[0]), @InBuffer[0], FBufferSize div FSampleSize);
      3 : Int24ToSingle(PBuffer8(@TmpBuffer2[0]), @InBuffer[0], FBufferSize div FSampleSize);
      4 : Int32ToSingle(PBuffer32(@TmpBuffer2[0]), @InBuffer[0], FBufferSize div FSampleSize);
    end;
    if Finput.Channels = 2 then
    begin
      for i := 0 to FramesInBuffer - 1 do
      begin
        RBuffer[i] := InBuffer[i*2]*$8000;
        LBuffer[i] := InBuffer[i*2+1]*$8000;
      end;
    end else
      for i := 0 to FramesInBuffer - 1 do
    begin
      RBuffer[i] := InBuffer[i]*$8000;
      LBuffer[i] := RBuffer[i];
    end;
    AnalyzeSamples(@LBuffer[0], @RBuffer[0], FramesInBuffer, 2);
    Flevel := 32 - GetTitleGain;
  end;

  procedure TGainProcessor.InitInternal;
  begin
    LoadLibGain;
    if not LibGainLoaded then
      raise EAuException.Create(Format('Could not load the %s library.', [LibGainPath]));
    Busy := True;
    FInput.Init;
    if FInput.Channels > 2 then
    begin
      FInput.Flush;
      Busy := False;
      raise EAuException.Create('Only mono or stereo soures are supported.');
    end;
    if InitGainAnalysis(FInput.SampleRate) <> GAIN_ANALYSIS_OK then
    begin
      FInput.Flush;
      Busy := False;
      raise EAuException.Create(Format('Failed to set up gain analysis. Possible cause: sample rate %d is not supported.', [FInput.SampleRate]));
    end;
    FSampleSize := FInput.BitsPerSample div 8;
    FrameSize := FSampleSize * FInput.Channels;
    FSize := -1;
    FPosition := 0;
    FBufferSize := FSampleSize*FInput.SampleRate*FInput.Channels div 5;
    FramesInBuffer := FInput.SampleRate div 5;
    SetLength(TmpBuffer1, FBufferSize);
    SetLength(TmpBuffer2, FBufferSize);
    FillChar(TmpBuffer2[0], FBufferSize, 0);
    SetLength(InBuffer, FBufferSize div FSampleSize);
    SetLength(RBuffer, FBufferSize div FSampleSize);
    SetLength(LBuffer, FBufferSize div FSampleSize);
    FOffset := FBufferSize;
    FState := gcPassing;
    FInternalState := 1;
    FCount := 0;
    FFilterOffset := 0;
    FLastY[0] := 0;
    FLastY[1] := 0;
    FillChar(FFilterBuffer[0], 8*lFilterLength, 0);
  end;

  procedure TGainProcessor.FlushInternal;
  begin
    FLevel := 0;
    Finput.Flush;
    SetLength(TmpBuffer1, 0);
    SetLength(TmpBuffer2, 0);
    SetLength(InBuffer, 0);
    SetLength(RBuffer, 0);
    SetLength(LBuffer, 0);
    FState := gcPassing;
    Busy := False;
  end;

  procedure TGainProcessor.GetDataInternal;
  var
    i, j : Integer;
    t : Int64;
    B16 : PBuffer16;
    B32 : PBuffer32;
    B8 : PBuffer8;
    tB : LongWord;
  begin
    if FOffset >= FBufferSize then
    begin
      if FIsEndOdSource then
      begin
        Bytes := 0;
        Buffer := nil;
        FState := gcPassing;
        Exit;
      end;
      GetNewData;
    end;
    case FInternalState of
      0:
      begin
        if Flevel < FMinSoundLevel then
        begin
          FInternalState := 1;
          FCount :=0;
        end;
      end;
      1:
      begin
        if Flevel >= FMinSoundLevel then
        begin
          FInternalState := 0;
        end else
        begin
          FCount := FCount + (Bytes*10000) div (FrameSize*FInput.SampleRate);
          if FCount >= FMinSilenceInterval*10000 then
          FInternalState := 2;
          FState := gcSkipping;
          if Assigned(FOnStateChanged) then
            FOnStateChanged(Self);
        end;
      end;
      2:
      begin
        if Flevel >= FMinSoundLevel then
        begin
          FInternalState := 0;
          FState := gcPassing;
          if Assigned(FOnStateChanged) then
            FOnStateChanged(Self);
        end;
      end;
      else;
    end;
    if not FSkipSilenceEnabled then
       FInternalState := 0;
    if FInternalState < 2 then
    begin
      {$WARNINGS OFF}
      if Bytes > FBufferSize - FOffset then
        Bytes := FBufferSize - FOffset;
      {$WARNINGS ON}
      Buffer := @TmpBuffer1[FOffset];
      Inc(FOffset, Bytes);
    end else
    begin
 //     PInt64(@TmpBuffer1[0])^ := 0;
      Bytes := FrameSize;
      Buffer := @TmpBuffer1[0];
      FOffset := FBufferSize;
    end;
    for i := 0 to (Bytes div FrameSize) - 1 do
    begin
      if FInput.BitsPerSample = 16 then
      begin
        B16 := Buffer;
        if Finput.Channels = 1 then
        begin
          t := (2*FLastY[0] + (B16[i*2] - FFilterBuffer[0,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[0,FFilterOffset] := B16[i];
          FFilterOffset := (FFilterOffset + 1) mod lFilterLength;
          if t > 32767 then
               t := 32767;
          if t < -32767 then
               t := -32767;
          B16[i] := 2*t;
          FLastY[0] := t;
        end else
        begin
          t := (2*FLastY[0] + (B16[i*2] - FFilterBuffer[0,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[0,FFilterOffset] := B16[i*2];
//        FFilterOffset := (FFilterOffset + 1) mod FilterLength;
          if t > 32767 then
               t := 32767;
          if t < -32767 then
               t := -32767;
          B16[i*2] := 2*t;
          FLastY[0] := t;
//          t := B16[i*2+1];
//          t := FLastY[1] + (B16[i*2+1] - FFilterBuffer[1,FFilterOffset]) div FilterLength;
          t := (2*FLastY[1] + (B16[i*2+1] - FFilterBuffer[1,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[1,FFilterOffset] := B16[i*2+1];
          FFilterOffset := (FFilterOffset + 1) mod lFilterLength;
          if t > 32767 then
               t := 32767;
          if t < -32767 then
               t := -32767;
          B16[i*2+1] := 2*t;
          FLastY[1] := t;
        end;
      end else
      if FInput.BitsPerSample = 32 then
      begin
        B32 := Buffer;
        if Finput.Channels = 1 then
        begin
          t := (2*FLastY[0] + (B32[i] - FFilterBuffer[0,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[0,FFilterOffset] := B32[i];
          FFilterOffset := (FFilterOffset + 1) mod lFilterLength;
          if t > 32767 then
               t := 32767;
          if t < -32767 then
               t := -32767;
          B32[i] := t;
          FLastY[0] := t;
        end else
        begin
          t := (2*FLastY[0] + (B32[i*2] - FFilterBuffer[0,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[0,FFilterOffset] := B32[i*2];
//        FFilterOffset := (FFilterOffset + 1) mod FilterLength;
          if t > 32767 then
               t := 32767;
          if t < -32767 then
               t := -32767;
          B32[i*2] := t;
          FLastY[0] := t;

          t := FLastY[1] + (B32[i*2+1] - FFilterBuffer[1,FFilterOffset]) div lFilterLength;
          FFilterBuffer[1,FFilterOffset] := B32[i*2+1];
          FFilterOffset := (FFilterOffset + 1) mod lFilterLength;
          if t > Int64($80000000) then
               t := Int64($80000000);
          if t < -Int64($7FFFFFFF) then
               t := -Int64($7FFFFFFF);
          B32[i*2+1] := t;
          FLastY[1] := t;
        end;
      end else
      if FInput.BitsPerSample = 8 then
      begin
        B8 := Buffer;
        if Finput.Channels = 1 then
        begin
          t := (2*FLastY[0] + (B8[i] - FFilterBuffer[0,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[0,FFilterOffset] := B8[i];
          FFilterOffset := (FFilterOffset + 1) mod lFilterLength;
          if t > 255 then
               t := 255;
          B8[i] := t;
          FLastY[0] := t;
        end else
        begin
          t := (2*FLastY[0] + (B8[i*2] - FFilterBuffer[0,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[0,FFilterOffset] := B8[i*2];
//        FFilterOffset := (FFilterOffset + 1) mod FilterLength;
          if t > 8767 then
               t := 255;
          B8[i*2] := t;
          FLastY[0] := t;

          t := (2*FLastY[1] + (B8[i*2+1] - FFilterBuffer[1,FFilterOffset]) div lFilterLength) div 2;
          FFilterBuffer[1,FFilterOffset] := B8[i*2+1];
          FFilterOffset := (FFilterOffset + 1) mod lFilterLength;
          if t > 255 then
               t := 255;
          B8[i*2+1] := t;
          FLastY[1] := t;
        end;
      end;
    end;

  end;

    (*
    procedure TGainProcessor.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
      var
        i, j : Integer;
        t : Int64;
        B16 : PBuffer16;
        B32 : PBuffer32;
        B8 : PBuffer8;
        tB : LongWord;
      begin
        tB := Bytes;
        CalculateInternal(Buffer, Bytes);
        while Bytes = FrameSize do
        begin
          Bytes := Tb;
          CalculateInternal(Buffer, Bytes);
        end;
        if Bytes = 0 then Exit;
        B16 := Buffer;
        for i := 0 to (Bytes div FrameSize) - 1 do
        begin
          if FInput.BitsPerSample = 16 then
          begin
            if Finput.Channels = 1 then
            begin
              t := FLastY[0] + (B16[i] - FFilterBuffer[0,FFilterOffset]) div FilterLength;
              FFilterBuffer[0,FFilterOffset] := B16[i];
              FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 32767 then
                   t := 32767;
              if t < -32767 then
                   t := -32767;
              B16[i] := t;
              FLastY[0] := t;
            end else
            begin
              t := FLastY[0] + (B16[i*2] - FFilterBuffer[0,FFilterOffset]) div FilterLength;
              FFilterBuffer[0,FFilterOffset] := B16[i*2];
    //        FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 32767 then
                   t := 32767;
              if t < -32767 then
                   t := -32767;
              B16[i*2] := t;
              FLastY[0] := t;

              t := FLastY[1] + (B16[i*2+1] - FFilterBuffer[1,FFilterOffset]) div FilterLength;
              FFilterBuffer[1,FFilterOffset] := B16[i*2+1];
              FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 32767 then
                   t := 32767;
              if t < -32767 then
                   t := -32767;
              B16[i*2+1] := t;
              FLastY[1] := t;
            end;
          end else
          if FInput.BitsPerSample = 32 then
          begin
            if Finput.Channels = 1 then
            begin
              t := FLastY[0] + (B32[i] - FFilterBuffer[0,FFilterOffset]) div FilterLength;
              FFilterBuffer[0,FFilterOffset] := B32[i];
              FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 32767 then
                   t := 32767;
              if t < -32767 then
                   t := -32767;
              B32[i] := t;
              FLastY[0] := t;
            end else
            begin
              t := FLastY[0] + (B32[i*2] - FFilterBuffer[0,FFilterOffset]) div FilterLength;
              FFilterBuffer[0,FFilterOffset] := B32[i*2];
    //        FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 32767 then
                   t := 32767;
              if t < -32767 then
                   t := -32767;
              B32[i*2] := t;
              FLastY[0] := t;

              t := FLastY[1] + (B32[i*2+1] - FFilterBuffer[1,FFilterOffset]) div FilterLength;
              FFilterBuffer[1,FFilterOffset] := B32[i*2+1];
              FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > $FFFFFFFF then
                   t := $FFFFFFFF;
              if t < -$FFFFFFFE then
                   t := -$FFFFFFFE;
              B32[i*2+1] := t;
              FLastY[1] := t;
            end;
          end else
          if FInput.BitsPerSample = 8 then
          begin
            if Finput.Channels = 1 then
            begin
              t := FLastY[0] + (B8[i] - FFilterBuffer[0,FFilterOffset]) div FilterLength;
              FFilterBuffer[0,FFilterOffset] := B8[i];
              FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 255 then
                   t := 255;
              B8[i] := t;
              FLastY[0] := t;
            end else
            begin
              t := FLastY[0] + (B8[i*2] - FFilterBuffer[0,FFilterOffset]) div FilterLength;
              FFilterBuffer[0,FFilterOffset] := B8[i*2];
    //        FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 8767 then
                   t := 8767;
              if t < -8767 then
                   t := -8767;
              B8[i*2] := t;
              FLastY[0] := t;

              t := FLastY[1] + (B8[i*2+1] - FFilterBuffer[1,FFilterOffset]) div FilterLength;
              FFilterBuffer[1,FFilterOffset] := B8[i*2+1];
              FFilterOffset := (FFilterOffset + 1) mod FilterLength;
              if t > 8767 then
                   t := 8767;
              if t < -8767 then
                   t := -8767;
              B8[i*2+1] := t;
              FLastY[1] := t;
            end;
          end;
        end;
      end;
  *)

end.
