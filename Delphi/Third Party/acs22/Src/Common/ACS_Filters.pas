(*
  This file is a part of Audio Components Suite v 2.2.
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_Filters;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes, Math;

const
  BUF_SIZE = $4000;

type

  TFilterType = (ftBandPass, ftBandReject, ftHighPass, ftLowPass, ftAllPass);

  TBWFilter = class(TACSConverter)
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
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property Amplification : Word read FAmplification write SetAmplification;
    property FilterType : TFilterType read FFilterType write FFilterType;
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    property LowFreq : Integer read FLowFreq write SetLowFreq;
  end;

  TSincFilter = class(TACSConverter)
  private
    Lock : Boolean;
    Kernel : array of Double;
    DA : PDoubleArray;
    DAS : PStereoBufferD;
    inBuf : array[1..BUF_SIZE] of Byte;
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
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    procedure GetKernel(var K : PDoubleArray);
  published
    property FilterType : TFilterType read FFilterType write SetFilterType;
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    property KernelWidth : Integer read FKernelWidth write SetKernelWidth;
    property LowFreq : Integer read FLowFreq write SetLowFreq;
    property WindowType  : TFilterWindowType read FWindowType write SetWindowType;
  end;

  TConvolver = class(TACSConverter)
  private
    Lock : Boolean;
    Kernel : array of Double;
    DA : PDoubleArray;
    DAS : PStereoBufferD;
    inBuf : array[1..BUF_SIZE] of Byte;
    FKernelWidth : Integer;
    FAllPass : Boolean;
    procedure SetKernelWidth(a : Integer);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    procedure SetKernel(K : PDoubleArray; Inverted : Boolean);
    property KrenelWidth : Integer read FKernelWidth write SetKernelWidth;
  published
    property AllPass : Boolean read FAllPass write FAllPass;
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
    raise EACSException.Create('Input not assigned');
    Result := FInput.Channels;
  end;

  function TBWFilter.GetSR;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TBWFilter.SetHighFreq;
  begin
    if FFilterType = ftLowPass then
    FHighFreq := 0
    else FHighFreq := aFreq;
  end;

  procedure TBWFilter.SetLowFreq;
  begin
    if FFilterType = ftHighPass then
    FLowFreq := 0
    else FLowFreq := aFreq;
  end;

  procedure TBWFilter.SetAmplification;
  begin
    if Ampl > 0 then FAmplification := Ampl;
  end;

  procedure TBWFilter.Init;
  var
    C, D : Double;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    FInput.Init;
    if ((FHighFreq - FlowFreq) < 0) or (((FHighFreq - FlowFreq) * 2) >= FInput.SampleRate) then
    begin
      FInput.Flush;
      raise EACSException.Create('Illegal frequency');
    end;
    Buisy := True;
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

  procedure TBWFilter.Flush;
  begin
    FInput.Flush;
    Buisy := False;
  end;

  function TBWFilter.GetData;
  var
    i : Integer;
    InBufMono : PBuffer16;
    InBufStereo : PStereoBuffer16;
    arg, res : Double;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      BufEnd := FInput.GetData(@InBuf[1], BUF_SIZE);
      if BufEnd = 0 then
      begin
        Result := 0;
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
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(InBuf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  constructor TSincFilter.Create;
  begin
    inherited Create(AOwner);
    FKernelWidth := 31;
    FWindowType := fwBlackman;
    FLowFreq := 8000;
    FHighFreq := 16000;
    DA := nil;
    DAS := nil;
  end;

  destructor TSincFilter.Destroy;
  begin
    Kernel := nil;
    if DA <> nil then FreeMem(DA);
    if DAS <> nil then FreeMem(DAS);
    Inherited Destroy;
  end;

  procedure TSincFilter.CalculateFilter;
  var
    Kernel1, Kernel2 : array of Double;
    CutOff : Double;
    i, j : Integer;
    Sum : Double;
  begin
    if csDesigning in ComponentState then Exit;
    if not Assigned(FInput) then Exit;
    if (FLowFreq > FInput.SampleRate/2) or (FHighFreq > FInput.SampleRate/2) then
    raise EACSException.Create('Cut-off frequencies are greater than the half of the sample rate.');
    while Lock do;
    Lock := True;
    case FilterType of
      ftLowPass:
      begin
        SetLength(Kernel, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel[0], CutOff, FKernelWidth, FWindowType);
      end;
      ftHighPass:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel[i] := -Kernel[i];
        Kernel[(FKernelWidth shr 1)] := Kernel[(FKernelWidth shr 1)] + 1;
      end;
      ftBandPass:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel1, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel1[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel1[i] := -Kernel1[i];
        Kernel1[(FKernelWidth shr 1)] := Kernel1[(FKernelWidth shr 1)] + 1;
        SetLength(Kernel2, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel2[0], CutOff, FKernelWidth, FWindowType);
        SetLength(Kernel, 2*FKernelWidth);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Double), 0);
        for i := 0 to KernelWidth - 1 do
        for j := 0 to KernelWidth - 1 do
        Kernel[i+j] := Kernel[i+j] + Kernel1[i]*Kernel2[j];
        SetLength(Kernel, FKernelWidth);
        Kernel1 := nil;
        Kernel2 := nil;
      end;
      ftBandReject:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel1, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel1[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel1[i] := -Kernel1[i];
        Kernel1[(FKernelWidth shr 1)] := Kernel1[(FKernelWidth shr 1)] + 1;
        SetLength(Kernel2, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel2[0], CutOff, FKernelWidth, FWindowType);
        SetLength(Kernel, FKernelWidth);
        for i := 0 to FKernelWidth - 1 do
        Kernel[i] := Kernel1[i] + Kernel2[i];
        Kernel1 := nil;
        Kernel2 := nil;
      end;
      ftAllPass :
      begin
        SetLength(Kernel, FKernelWidth);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Double), 0);
        Kernel[FKernelWidth shr 1] := 1;
      end;
    end;
    Lock := False;
  end;

  procedure TSincFilter.SetFilterType;
  begin
    FFilterType := aFT;
    if Buisy then CalculateFilter;
  end;

  procedure TSincFilter.SetKernelWidth;
  begin
    if aKW > 2 then
    if not Buisy then FKernelWidth := aKW;
  end;

  procedure TSincFilter.SetWindowType;
  begin
    FWindowType := aWT;
    if Buisy then CalculateFilter;
  end;

  procedure TSincFilter.SetHighFreq;
  begin
    if aFreq > 0 then
    FHighFreq := aFreq;
    if csDesigning in ComponentState then Exit;
    if Assigned(Finput) then
    if FHighFreq > Finput.SampleRate div 2 then
    FHighFreq := Finput.SampleRate div 2;
    if FHighFreq < FLowFreq then
    FLowFreq := FHighFreq;
    if Buisy then CalculateFilter;
  end;

  procedure TSincFilter.SetLowFreq;
  begin
    if aFreq > 0 then
    FLowFreq := aFreq;
    if csDesigning in ComponentState then Exit;
    if Assigned(Finput) then
    if FlowFreq > Finput.SampleRate div 2 then
    FLowFreq := Finput.SampleRate div 2;
    if FHighFreq < FLowFreq then
    FHighFreq := FLowFreq;
    if Buisy then CalculateFilter;
  end;

  function TSincFilter.GetBPS;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TSincFilter.GetCh;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TSincFilter.GetSR;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TSincFilter.Init;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Lock := False;
    InputLock := False;
    Buisy := True;
    FInput.Init;
    FPosition := 0;
    CalculateFilter;
    if FInput.Channels = 1 then
    begin
      GetMem(DA, ((BUF_SIZE div 2)+FKernelWidth-1)*SizeOf(Double));
      FillChar(DA[0], ((BUF_SIZE div 2)+FKernelWidth-1)*SizeOf(Double), 0);
    end else
    begin
      GetMem(DAS, ((BUF_SIZE div 2)+(FKernelWidth-1)*2)*SizeOf(Double));
      FillChar(DAS[0], ((BUF_SIZE div 2)+(FKernelWidth-1)*2)*SizeOf(Double), 0);
    end;
    BufStart := 1;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TSincFilter.Flush;
  begin
    FInput.Flush;
    if DA <> nil then FreeMem(DA);
    if DAS <> nil then FreeMem(DAS);
    DA := nil;
    DAS := nil;
    Buisy := False;
  end;

  function TSincFilter.GetData;
  var
    i, j, NumSamples : Integer;
    InBufMono : PBuffer16;
    InBufStereo : PStereoBuffer16;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      while Lock do;
      Lock := True;
      BufStart := 1;
      while InputLock do;
      InputLock := True;
      BufEnd := FInput.GetData(@InBuf[1], BUF_SIZE);
      InputLock := False;
      if BufEnd = 0 then
      begin
        Result := 0;
        Exit;
      end;
      if FInput.Channels = 1 then
      begin
        InBufMono := @InBuf[1];
        NumSamples := BufEnd div 2;
        for i := 0 to NumSamples-1 do
        for j := 0 to FKernelWidth-1 do
        DA[i+j] := DA[i+j] + InbufMono[i]*Kernel[j];
        for i := 0 to NumSamples-1 do
        InBufMono[i] := Round(DA[i]);
        BufEnd := NumSamples*2;
        FillChar(DA[0], NumSamples*SizeOf(Double), 0);
        Move(DA[NumSamples], DA[0], (FKernelWidth-1)*SizeOf(Double));
      end else
      begin
        InBufStereo := @InBuf[1];
        NumSamples := BufEnd div 4;
        for i := 0 to NumSamples-1 do
        for j := 0 to FKernelWidth-1 do
        begin
          DAS[i+j].Left := DAS[i+j].Left + InbufStereo[i].Left*Kernel[j];
          DAS[i+j].Right := DAS[i+j].Right + InbufStereo[i].Right*Kernel[j];
        end;
        for i := 0 to NumSamples-1 do
        begin
          InBufStereo[i].Left := Round(DAS[i].Left);
          InBufStereo[i].Right := Round(DAS[i].Right);
        end;
        BufEnd := NumSamples*4;
        FillChar(DAS[0], NumSamples*2*SizeOf(Double), 0);
        for i := 0 to FKernelWidth-2 do
        begin
          DAS[i] := DAS[NumSamples+i];
          DAS[NumSamples+i].Left := 0;
          DAS[NumSamples+i].Right := 0;
        end;
        //Move(DAS[NumSamples], DAS[0], (FKernelWidth-1)*2*SizeOf(Double));
      end;
      Lock := False;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(InBuf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TSincFilter.GetKernel;
  begin
    K := @Kernel[0];
  end;

  constructor TConvolver.Create;
  begin
    inherited Create(AOwner);
    FKernelWidth := 31;
    if csDesigning in ComponentState then Exit;
    SetLength(Kernel, FKernelWidth);
    FillChar(Kernel[1], Length(Kernel)*SizeOf(Double), 0);
    DA := nil;
    DAS := nil;
  end;

  destructor TConvolver.Destroy;
  begin
    Kernel := nil;
    if DA <> nil then FreeMem(DA);
    if DAS <> nil then FreeMem(DAS);
    Inherited Destroy;
  end;

  procedure TConvolver.SetKernelWidth;
  begin
    if a > 2 then
    if not Buisy then FKernelWidth := a;
  end;

  function TConvolver.GetBPS;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TConvolver.GetCh;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TConvolver.GetSR;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TConvolver.Init;
  begin
    if not Assigned(Input) then
    raise EACSException.Create('Input is not assigned');
    Lock := False;
    InputLock := False;
    Buisy := True;
    FInput.Init;
    FPosition := 0;
    if FInput.Channels = 1 then
    begin
      GetMem(DA, ((BUF_SIZE div 2)+FKernelWidth-1)*SizeOf(Double));
      FillChar(DA[0], ((BUF_SIZE div 2)+FKernelWidth-1)*SizeOf(Double), 0);
    end else
    begin
      GetMem(DAS, ((BUF_SIZE div 2)+(FKernelWidth-1)*2)*SizeOf(Double));
      FillChar(DAS[0], ((BUF_SIZE div 2)+(FKernelWidth-1)*2)*SizeOf(Double), 0);
    end;
    BufStart := 1;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TConvolver.Flush;
  begin
    FInput.Flush;
    if DA <> nil then FreeMem(DA);
    if DAS <> nil then FreeMem(DAS);
    DA := nil;
    DAS := nil;
    Buisy := False;
  end;

  function TConvolver.GetData;
  var
    i, j, NumSamples : Integer;
    InBufMono : PBuffer16;
    InBufStereo : PStereoBuffer16;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      while Lock do;
      Lock := True;
      BufStart := 1;
      while InputLock do;
      InputLock := True;
      BufEnd := FInput.GetData(@InBuf[1], BUF_SIZE);
      InputLock := False;
      if BufEnd = 0 then
      begin
        Result := 0;
        Exit;
      end;
      if not FAllPass then
      begin
        if FInput.Channels = 1 then
        begin
          InBufMono := @InBuf[1];
          NumSamples := BufEnd div 2;
          for i := 0 to NumSamples-1 do
          for j := 0 to FKernelWidth-1 do
          DA[i+j] := DA[i+j] + InbufMono[i]*Kernel[j];
          for i := 0 to NumSamples-1 do
          InBufMono[i] := Round(DA[i]);
          BufEnd := NumSamples*2;
          FillChar(DA[0], NumSamples*SizeOf(Double), 0);
          Move(DA[NumSamples], DA[0], (FKernelWidth-1)*SizeOf(Double));
        end else
        begin
          InBufStereo := @InBuf[1];
          NumSamples := BufEnd div 4;
          for i := 0 to NumSamples-1 do
          for j := 0 to FKernelWidth-1 do
          begin
            DAS[i+j].Left := DAS[i+j].Left + InbufStereo[i].Left*Kernel[j];
            DAS[i+j].Right := DAS[i+j].Right + InbufStereo[i].Right*Kernel[j];
          end;
          for i := 0 to NumSamples-1 do
          begin
            InBufStereo[i].Left := Round(DAS[i].Left);
            InBufStereo[i].Right := Round(DAS[i].Right);
          end;
          BufEnd := NumSamples*4;
          FillChar(DAS[0], NumSamples*2*SizeOf(Double), 0);
          for i := 0 to FKernelWidth-2 do
          begin
            DAS[i] := DAS[NumSamples+i];
            DAS[NumSamples+i].Left := 0;
            DAS[NumSamples+i].Right := 0;
          end;
          //Move(DAS[NumSamples], DAS[0], (FKernelWidth-1)*2*SizeOf(Double));
        end;
      end;
      Lock := False;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(InBuf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TConvolver.SetKernel;
  var
    i : Integer;
  begin
    while Lock do;
    Lock := True;
    if not Inverted then
    for i := 0 to FKernelWidth - 1 do
    Kernel[i] := K[i]
    else
    for i := 0 to FKernelWidth - 1 do
    Kernel[i] := K[FKernelWidth - 1-i];
    Lock := False;
  end;


end.
