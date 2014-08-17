(*
  This file is a part of New Audio Components package 1.8
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_Filters.pas 1007 2009-10-09 22:17:10Z andrei.borovsky $ *)

unit ACS_Filters;

(* Title: ACS_Filters
    Classes which run filters on audio data. *)

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Math;


const
  BUF_SIZE = $4000;
  BufSize = $6000;

type

  TFilterType = (ftBandPass, ftBandReject, ftHighPass, ftLowPass, ftAllPass);

  TBWFilter = class(TAuConverter)
  private
    a3 : array[0..2] of Double;
    b2 : array[0..1] of Double;
    x0, x1, y0, y1 : array[0..1] of Double;
    FLowFreq, FHighFreq : Integer;
    FAmplification : Word;
    FFilterType : TFilterType;
    InBuf : array[1..BUF_SIZE] of Byte;
    procedure SetHighFreq(aFreq : Integer);
    procedure SetLowFreq(aFreq : Integer);
    procedure SetAmplification(Ampl : Word);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  published
    property Amplification : Word read FAmplification write SetAmplification;
    property FilterType : TFilterType read FFilterType write FFilterType;
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    property LowFreq : Integer read FLowFreq write SetLowFreq;
  end;

  (* Class: TSincFilter
    This component implements a windowed-sinc filter.
    The filter can work in low-pass, high-pass, band-pass, band-reject, or all-pass mode.
    A descendant of <TAuConverter>. *)

  TSincFilter = class(TAuConverter)
  private
    Kernel : array of Single;
    InputBuffer, OutputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
    FFilterType : TFilterType;
    FKernelWidth : Integer;
    FLowFreq, FHighFreq : Integer;
    FWindowType  : TFilterWindowType;
    procedure SetFilterType(aFT  : TFilterType);
    procedure SetKernelWidth(aKW : Integer);
    procedure SetWindowType(aWT : TFilterWindowType);
    procedure SetHighFreq(aFreq : Integer);
    procedure SetLowFreq(aFreq : Integer);
    procedure CalculateFilter;
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
    (* Function: GetKernel
     Returns the pointer to the current filter kernel. May return nil. *)
    procedure GetKernel(var K : PSingleArray);
  published
    (* Property: FilterType
     Use this property to set the desired filter type: low-pass, high-pass, band-pass, band-reject, or all-pass. *)
    property FilterType : TFilterType read FFilterType write SetFilterType;
    (* Property: HighFreq
     Use this property to set the high cut-off frequency. This property applies to high-pass, band-pass, and band-reject filters. *)
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    (* Property: KernelWidth
     Use this property to set the number of points in the filter kernel. *)
    property KernelWidth : Integer read FKernelWidth write SetKernelWidth;
    (* Property: LowFreq
     Use this property to set the low cut-off frequency. This property applies to low-pass, band-pass, and band-reject filters. *)
    property LowFreq : Integer read FLowFreq write SetLowFreq;
    (* Property: WindowType
     Use this property to set the type of the window applied to the filter kernel. *)
    property WindowType  : TFilterWindowType read FWindowType write SetWindowType;
  end;

  (* Class: TChebyshevFilter
    This component implements Chebyshev or  Butterworth filters (see the description below).
    A descendant of <TAuConverter>. *)

  TChebyshevFilter = class(TAuConverter)
  private
    A, B : array of Single;
    X, Y : array[0..7] of array of Single;
    A1, B1 : array of Single;
    X1, Y1 : array[0..7] of array of Single;
    FRipple : Single;
    FHighFreq, FLowFreq : Word;
    FNumberOfPoles  : Word;
    FFilterType : TFilterType;
    offsX, OffsY : Integer;
    InputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
    procedure CalculateFilter;
    procedure SetNumPoles(NP : Word);
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
  published
   (* Property: FilterType
     Use this property to set the desired filter type: low-pass, high-pass, band-pass, or band-reject
     Curretnly only low-pass and high-pass filters are stable. *)
    property FilterType : TFilterType read FFilterType write FFilterType;
   (* Property: NumberOfPoles
     Use this property to set the number of poles for the filter.
     Allowed values are 2, 4, 6 ... 16. Note that filters with high number of poles may become unstable in which case their operation results in floating pointoverflow. *)
    property NumberOfPoles : Word read FNumberOfPoles write SetNumPoles;
   (* Property: HighFreq
     Use this property to set the high cut-off frequency. This property applies to high-pass, band-pass, and band-reject filters.
     The value of this property should not be greater than the half of the samping rate.
    *)
    property HighFreq : Word read FHighFreq write FHighFreq;
   (* Property: LowFreq
     Use this property to set the low cut-off frequency. This property applies to low-pass, band-pass, and band-reject filters.
     The value of this property should not be greater than the half of the samping rate.
    *)
    property LowFreq : Word read FLowFreq write FLowFreq;
   (* Property: Ripple
     Use this property to set the band-pass ripple. The reasonable values for Chebyshev filters are are 0.3 - 0.5.
     If you set this property to zero the filter's response becomes that of Butterworth filter.
    *)
    property Ripple : Single read FRipple write FRipple;
  end;


implementation

  constructor TBWFilter.Create;
  begin
    inherited Create(AOwner);
    FFilterType := ftBandPass;
    FAmplification := 1;
  end;

  destructor TBWFilter.Destroy;
  begin
    inherited Destroy;
  end;

  function TBWFilter.GetBPS;
  begin
    Result := 16;
  end;

  function TBWFilter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.Channels;
  end;

  function TBWFilter.GetSR;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TBWFilter.SetHighFreq;
  begin
    FHighFreq := aFreq;
  end;

  procedure TBWFilter.SetLowFreq;
  begin
    FLowFreq := aFreq;
  end;

  procedure TBWFilter.SetAmplification;
  begin
    if Ampl > 0 then FAmplification := Ampl;
  end;

  procedure TBWFilter.InitInternal;
  var
    C, D : Double;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    FInput.Init;
    if ((FHighFreq - FlowFreq) < 0) or (((FHighFreq - FlowFreq) * 2) >= FInput.SampleRate) then
    begin
      FInput.Flush;
      raise EAuException.Create('Illegal frequency');
    end;
    Busy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    FSize := FInput.Size;
    x0[0] := 0.0;
    x0[1] := 0.0;
    x1[0] := 0.0;
    x1[1] := 0.0;
    y0[0] := 0.0;
    y0[1] := 0.0;
    y1[0] := 0.0;
    y1[1] := 0.0;
    case FFilterType of
      ftBandPass :
      begin
        C := 1 / Tan(Pi * (FHighFreq-FLowFreq+1) / FInput.SampleRate);
        D := 2 * Cos(2 * Pi * ((FHighFreq+FLowFreq) shr 1) / FInput.SampleRate);
        a3[0] := 1 / (1 + C);
        a3[1] := 0.0;
        a3[2] := -a3[0];
        b2[0] := -C * D * a3[0];
        b2[1] := (C - 1) * a3[0];
      end;
      ftBandReject:  // This doesn't seem to work well
      begin
        C := Tan(Pi * (FHighFreq-FLowFreq+1) / FInput.SampleRate);
        D := 2 * Cos(2 * Pi * ((FHighFreq+FLowFreq) shr 1) / FInput.SampleRate);
        a3[0] := 1 / (1 + C);
        a3[1] := -D * a3[0];
        a3[2] := a3[0];
        b2[0] := a3[1];
        b2[1] := (1 - C) * a3[0];
      end;
      ftLowPass:
      begin
        C := 1 / Tan(Pi * FLowFreq / FInput.SampleRate);
        a3[0] := 1 / (1 + Sqrt(2) * C + C * C);
        a3[1] := 2 * a3[0];
        a3[2] := a3[0];
        b2[0] := 2 * (1 - C * C) * a3[0];
        b2[1] := (1 - Sqrt(2) * C + C * C) * a3[0];
      end;
      ftHighPass:
      begin
        C := Tan(Pi * FHighFreq / FInput.SampleRate);
        a3[0] := 1 / (1 + Sqrt(2) * C + C * C);
        a3[1] := -2 * a3[0];
        a3[2] := a3[0];
        b2[0] := 2 * (C * C - 1) * a3[0];
        b2[1] := (1 - Sqrt(2) * C + C * C) * a3[0];
      end;
    end;
  end;

  procedure TBWFilter.FlushInternal;
  begin
    FInput.Flush;
    Busy := False;
  end;

  procedure TBWFilter.GetDataInternal;
  var
    i : Integer;
    InBufMono : PBuffer16;
    InBufStereo : PStereoBuffer16;
    arg, res : Double;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      BufEnd := FInput.CopyData(@InBuf[1], BUF_SIZE);
      if BufEnd = 0 then
      begin
        Bytes := 0;
        Buffer := nil;
        Exit;
      end;
      if Self.Channels = 1 then
      begin
        InBufMono := @InBuf[1];
        for i := 0 to (BufEnd shr 1) - 1 do
        begin
          arg := InBufMono[i];
          res := a3[0] * arg + a3[1] * x0[0] + a3[2] * x1[0] -
                 b2[0] * y0[0] - b2[1] * y1[0];
          InBufMono[i] := Round(res);
          x1[0] := x0[0];
          x0[0] := arg;
          y1[0] := y0[0];
          y0[0] := res;
          InBufMono[i] := FAmplification * InBufMono[i];
        end;
      end else
      begin
        InBufStereo := @InBuf[1];
        for i := 0 to (BufEnd shr 2) - 1 do
        begin
          arg := InBufStereo[i].Left;
          res := a3[0] * arg + a3[1] * x0[0] + a3[2] * x1[0] -
                 b2[0] * y0[0] - b2[1] * y1[0];
          InBufStereo[i].Left := Round(res);
          x1[0] := x0[0];
          x0[0] := arg;
          y1[0] := y0[0];
          y0[0] := res;
          arg := InBufStereo[i].Right;
          res := a3[0] * arg + a3[1] * x0[1] + a3[2] * x1[1] -
                 b2[0] * y0[1] - b2[1] * y1[1];
          InBufStereo[i].Right := Round(res);
          x1[1] := x0[1];
          x0[1] := arg;
          y1[1] := y0[1];
          y0[1] := res;
          InBufStereo[i].Right := FAmplification * InBufStereo[i].Right;
          InBufStereo[i].Left := FAmplification * InBufStereo[i].Left;
        end;
      end;
    end;
    if Bytes > (BufEnd - BufStart + 1) then
    Bytes := BufEnd - BufStart + 1;
    Buffer := @InBuf[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  constructor TSincFilter.Create;
  begin
    inherited Create(AOwner);
    FKernelWidth := 63;
    FWindowType := fwBlackman;
    FLowFreq := 8000;
    FHighFreq := 16000;
  end;

  destructor TSincFilter.Destroy;
  begin
    Kernel := nil;
    Inherited Destroy;
  end;

  procedure TSincFilter.GetKernel(var K: PSingleArray);
  begin
    if Busy then
      K := @Kernel[0]
    else
      K := nil;
  end;

  procedure TSincFilter.CalculateFilter;
  var
    Kernel1, Kernel2 : array of Single;
    CutOff : Single;
    i, j : Integer;
  begin
    if csDesigning in ComponentState then Exit;
    if not Assigned(FInput) then Exit;
    if (FLowFreq > FInput.SampleRate/2) or (FHighFreq > FInput.SampleRate/2) then
    raise EAuException.Create('Cut-off frequencies are greater than the half of the sample rate.');
    case FilterType of
      ftLowPass:
      begin
        SetLength(Kernel, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernelSingle(@Kernel[0], CutOff, FKernelWidth, FWindowType);
      end;
      ftHighPass:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernelSingle(@Kernel[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel[i] := -Kernel[i];
        Kernel[(FKernelWidth shr 1)] := Kernel[(FKernelWidth shr 1)] + 1;
      end;
      ftBandPass:
      begin
        FKernelWidth := FKernelWidth div 2;
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel1, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernelSingle(@Kernel1[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel1[i] := -Kernel1[i];
        Kernel1[(FKernelWidth shr 1)] := Kernel1[(FKernelWidth shr 1)] + 1;
        SetLength(Kernel2, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernelSingle(@Kernel2[0], CutOff, FKernelWidth, FWindowType);
        SetLength(Kernel, FKernelWidth*2 - 1);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Single), 0);
        for i := 0 to KernelWidth - 1 do
        for j := 0 to KernelWidth - 1 do
        Kernel[i+j] := Kernel[i+j] + Kernel1[i]*Kernel2[j];
//        SetLength(Kernel, FKernelWidth);
        FKernelWidth := 2*FKernelWidth - 1;
//        for i := 0 to KernelWidth - 1 do
//          Kernel[i] := Kernel[i]*10;
        Kernel1 := nil;
        Kernel2 := nil;
      end;
      ftBandReject:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel1, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernelSingle(@Kernel1[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel1[i] := -Kernel1[i];
        Kernel1[(FKernelWidth shr 1)] := Kernel1[(FKernelWidth shr 1)] + 1;
        SetLength(Kernel2, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernelSingle(@Kernel2[0], CutOff, FKernelWidth, FWindowType);
        SetLength(Kernel, FKernelWidth);
        for i := 0 to FKernelWidth - 1 do
        Kernel[i] := Kernel1[i] + Kernel2[i];
        Kernel1 := nil;
        Kernel2 := nil;
      end;
      ftAllPass:
      begin
        SetLength(Kernel, FKernelWidth);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Single), 0);
        Kernel[FKernelWidth shr 1] := 1;
      end;
    end;
  end;

  procedure TSincFilter.SetFilterType;
  begin
    DataCS.Enter;
    FFilterType := aFT;
    if Busy then CalculateFilter;
    DataCS.Leave;
  end;

  procedure TSincFilter.SetKernelWidth;
  begin
    DataCS.Enter;
    if aKW > 2 then
    if not Busy then FKernelWidth := aKW;
    DataCS.Leave;
  end;

  procedure TSincFilter.SetWindowType;
  begin
    DataCS.Enter;
    FWindowType := aWT;
    if Busy then CalculateFilter;
    DataCS.Leave;
  end;

  procedure TSincFilter.SetHighFreq;
  begin
    DataCS.Enter;
    if aFreq > 0 then
    FHighFreq := aFreq;
    if csDesigning in ComponentState then Exit;
    if Assigned(Finput) then
    if FHighFreq > Finput.SampleRate div 2 then
    FHighFreq := Finput.SampleRate div 2;
    if FHighFreq < FLowFreq then
    FLowFreq := FHighFreq;
    if Busy then CalculateFilter;
    DataCS.Leave;
  end;

  procedure TSincFilter.SetLowFreq;
  begin
    DataCS.Enter;
    if aFreq > 0 then
    FLowFreq := aFreq;
    if csDesigning in ComponentState then Exit;
    if Assigned(Finput) then
    if FlowFreq > Finput.SampleRate div 2 then
    FLowFreq := Finput.SampleRate div 2;
    if FHighFreq < FLowFreq then
    FHighFreq := FLowFreq;
    if Busy then CalculateFilter;
    DataCS.Leave;
  end;

  function TSincFilter.GetBPS;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TSincFilter.GetCh;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TSincFilter.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TSincFilter.InitInternal;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize*FInput.Channels;
    FPosition := 0;
    CalculateFilter;
    SetLength(_Buffer, BufSize);
    SetLength(InputBuffer, BufSize div SampleSize);
    SetLength(OutputBuffer, BufSize div SampleSize + (Length(Kernel) - 1)*FInput.Channels);
    FillChar(OutputBuffer[0], Length(OutputBuffer)*SizeOf(Single), 0);
    BufStart := 0;
    BufEnd := 0;
    SamplesInFrame := Finput.Channels;
    FSize := FInput.Size;
  end;

  procedure TSincFilter.FlushInternal;
  begin
    FInput.Flush;
    SetLength(_Buffer, 0);
    SetLength(InputBuffer, 0);
    SetLength(OutputBuffer, 0);
    Busy := False;
  end;

  procedure TSincFilter.GetDataInternal;
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
//    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  constructor TChebyshevFilter.Create;
  begin
    inherited Create(AOwner);
    FRipple := 0.5;
    FFilterType := ftLowPass;
    NumberOfPoles := 6;
  end;

  destructor TChebyshevFilter.Destroy;
  begin
    Inherited Destroy;
  end;

  function TChebyshevFilter.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TChebyshevFilter.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TChebyshevFilter.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TChebyshevFilter.CalculateFilter;
  var
    i : Integer;
    CutOff : Single;
  begin
    SetLength(A, FNumberOfPoles + 4);
    SetLength(B, FNumberOfPoles + 4);
    SetLength(A1, FNumberOfPoles + 4);
    SetLength(B1, FNumberOfPoles + 4);
    for i := 0 to FNumberOfPoles + 3 do
    begin
      A[i] := 0;
      B[i] := 0;
      A1[i] := 0;
      B1[i] := 0;
    end;
    case FFilterType of
      ftLowpass :
      begin
        CutOff := FLowFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency should be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, False, A, B);
        SetLength(A, FNumberOfPoles + 1);
        SetLength(B, FNumberOfPoles);
      end;
      ftHighPass:
      begin
        CutOff := FHighFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency should be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, True, A, B);
        SetLength(A, FNumberOfPoles + 1);
        SetLength(B, FNumberOfPoles);
      end;
      ftBandPass:
      begin
        CutOff := FLowFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency should be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, True, A, B);
        SetLength(A, FNumberOfPoles +1);
        SetLength(B, FNumberOfPoles);
        CutOff := FHighFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency should be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, False, A1, B1);
        SetLength(A1, FNumberOfPoles +1);
        SetLength(B1, FNumberOfPoles);
      end;
      ftBandReject:
      begin
        CutOff := FLowFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency should be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, False, A, B);
        SetLength(A, FNumberOfPoles +1);
        SetLength(B, FNumberOfPoles);
        CutOff := FHighFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency should be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, True, A1, B1);
        SetLength(A1, FNumberOfPoles +1);
        SetLength(B1, FNumberOfPoles);
      end;
    end;
  end;

  procedure TChebyshevFilter.InitInternal;
  var
    i : Integer;
  procedure Swap(var S1, S2 : Single);
  var
    Tmp : Single;
  begin
    Tmp := S1;
    S1 := S2;
    S2 := Tmp;
  end;
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
    CalculateFilter;
    for i := 0 to Length(A) div 2 -1 do
      Swap(A[i], A[Length(A) - i -1]);
    for i := 0 to Length(B) div 2 -1 do
      Swap(B[i], B[Length(B) - i -1]);
    OffsX := Length(A) - 1;
    OffsY := Length(B);
    for i := 0 to Length(A1) div 2 -1 do
      Swap(A1[i], A1[Length(A1) - i -1]);
    for i := 0 to Length(B1) div 2 -1 do
      Swap(B1[i], B1[Length(B1) - i -1]);
    for i := 0 to SamplesInFrame - 1 do
    begin
      SetLength(X[i], BufSize div SampleSize + OffsX);
      FillChar(X[i][0], OffsX*SizeOf(Single), 0);
      SetLength(Y[i], BufSize div SampleSize + OffsY);
      FillChar(Y[i][0], OffsY*SizeOf(Single), 0);
      SetLength(X1[i], BufSize div SampleSize + OffsX);
      FillChar(X1[i][0], OffsX*SizeOf(Single), 0);
      SetLength(Y1[i], BufSize div SampleSize + OffsY);
      FillChar(Y1[i][0], OffsY*SizeOf(Single), 0);
    end;
    BufStart := 0;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TChebyshevFilter.FlushInternal;
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
      SetLength(X1[i], 0);
      SetLength(Y1[i], 0);
    end;
    Busy := False;
  end;

  procedure TChebyshevFilter.GetDataInternal;
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
      case FilterType of
        ftLowPass, ftHighPass:
        begin
          for i := 0 to FramesRead -1 do
            for j :=  0 to SamplesInFrame - 1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X[j][i]), @A[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y[j][i]), @B[0], Acc, OffsY);
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
        end;
        ftBandPass :
        begin
          for i := 0 to FramesRead -1 do
            for j :=  0 to SamplesInFrame - 1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X[j][i]), @A[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y[j][i]), @B[0], Acc, OffsY);
              Y[j][i + OffsY] := Acc;
            end;
          for i := 0 to FramesRead -1 do
            for j :=  0 to SamplesInFrame - 1 do
              X1[j][i + OffsX] := Y[j][i + OffsY];
          for j :=  0 to SamplesInFrame - 1 do
          begin
            for i := 0 to OffsX - 1 do
              X[j][i] := X[j][i + FramesRead];
            for i := 0 to OffsY - 1 do
              Y[j][i] := Y[j][i + FramesRead];
          end;
          for i := 0 to FramesRead -1 do
            for j :=  0 to SamplesInFrame - 1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X1[j][i]), @A1[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y1[j][i]), @B1[0], Acc, OffsY);
              Y1[j][i + OffsY] := Acc;
            end;
          for i := OffsY to FramesRead + OffsY -1 do
            for j :=  0 to SamplesInFrame - 1 do
              InputBuffer[(i - OffsY)*SamplesInFrame + j] :=  Y1[j][i];
          for j :=  0 to SamplesInFrame - 1 do
          begin
            for i := 0 to OffsX - 1 do
              X1[j][i] := X1[j][i + FramesRead];
            for i := 0 to OffsY - 1 do
              Y1[j][i] := Y1[j][i + FramesRead];
          end;
        end;
        ftBandReject :
        begin
          for i := OffsX to FramesRead + OffsX - 1 do
            for j :=  0 to SamplesInFrame - 1 do
              X1[j][i] := X[j][i];
          for i := 0 to FramesRead -1 do
            for j :=  0 to SamplesInFrame - 1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X[j][i]), @A[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y[j][i]), @B[0], Acc, OffsY);
              Y[j][i + OffsY] := Acc;
            end;
          for i := 0 to FramesRead -1 do
            for j :=  0 to SamplesInFrame - 1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X1[j][i]), @A1[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y1[j][i]), @B1[0], Acc, OffsY);
              Y1[j][i + OffsY] := Acc;
            end;
          for i := OffsY to FramesRead + OffsY -1 do
            for j :=  0 to SamplesInFrame - 1 do
              InputBuffer[(i - OffsY)*SamplesInFrame + j] :=  Y[j][i] + Y1[j][i];
          for j :=  0 to SamplesInFrame - 1 do
          begin
            for i := 0 to OffsX - 1 do
            begin
              X[j][i] := X[j][i + FramesRead];
              X1[j][i] := X1[j][i + FramesRead];
            end;
            for i := 0 to OffsY - 1 do
            begin
              Y[j][i] := Y[j][i + FramesRead];
              Y1[j][i] := Y1[j][i + FramesRead];
            end;
          end;
        end;
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
//    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TChebyshevFilter.SetNumPoles(NP : Word);
  begin
    FNumberOfPoles := ((NP + 1) shr 1)*2;
    if FNumberOfPoles > 16 then FNumberOfPoles := 16;
  end;




{$WARNINGS ON}

end.
