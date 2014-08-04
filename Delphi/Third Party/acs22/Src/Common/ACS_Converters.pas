(*
  This file is a part of Audio Components Suite v 2.2
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at audiocomps@mail.ru
*)

unit ACS_Converters;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes, Math;

const
  BUF_SIZE = $8000;

  KERNEL_WIDTH = 64;

type

  TMSConverterMode = (msmMonoToBoth, msmMonoToLeft, msmMonoToRight);

  TDA = array[0..63] of Double;
  PDA = ^TDA;


  TRateConverter = class(TACSConverter)
  private
    FOutSampleRate : Integer;
    WantedSize : Integer;
    EndOfInput : Boolean;
    remainder : Integer;
    InBufM, OutBufM : PBuffer16;
    InBufS, OutBufS : PStereoBuffer16;
    DAM : array of Double;
    DAS : array of TStereoSampleD;
    Kernel : array of Double;
    FKernelWidth : Integer;
    FFilterWindow : TFilterWindowType;
    Tail : Pointer;
    LBS : TStereoSample16;
    function ConvertFreqs16Mono(InSize : Integer): Integer;
    function ConvertFreqs16Stereo(InSize : Integer): Integer;
    procedure SetOutSampleRate(aSR : Integer);
    procedure SetKernelWidth(aKW : Integer);
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
    property FilterWindow : TFilterWindowType read FFilterWindow write FFilterWindow;
    property KernelWidth : Integer read FKernelWidth write SetKernelWidth;
    property OutSampleRate : Integer read FOutSampleRate write SetOutSampleRate;
  end;

  TMSConverter = class(TACSConverter)
  private
    WantedSize : Integer;
    EndOfInput : Boolean;
    InOutBuf : array[1..BUF_SIZE] of Byte;
    FMode : TMSConverterMode;
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
    property Mode : TMSConverterMode read FMode write FMode;
  end;

  TSampleConverter = class(TACSConverter)
  private
    WantedSize : Integer;
    EndOfInput : Boolean;
    InOutBuf : array[1..BUF_SIZE] of Byte;
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
  end;

  TStereoBalance = class(TACSConverter)
  private
    FBalance : Single;
    procedure SetBalance(a : Single);
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
    property Balance : Single read FBalance write SetBalance;
  end;

implementation

  function TRateConverter.ConvertFreqs16Mono(InSize : Integer): Integer;
  var
    i, step, j, k, s, m : Integer;
    D : Double;
    TailMono : PBuffer16;
    TailMonoD : PDoubleArray;
  begin
    TailMono := Tail;
    s := InSize shr 1;
    if FInput.SampleRate > FOutSampleRate then
    begin
      step := FInput.SampleRate - FOutSampleRate;
      j := 0;
      if remainder < 0 then remainder := FOutSampleRate;
      for i := 0 to s - 1 do
      begin
        if remainder > FOutSampleRate then Dec(remainder, FOutSampleRate)
        else begin
          D := 0;
          for k := 0 to FKernelWidth - 1 do
          if i-k >= 0 then
          D := D + InBufM[i-k]*Kernel[FKernelWidth - 1 - k]
          else
          D := D + TailMono[FKernelWidth-1+i-k]*Kernel[FKernelWidth - 1 - k];
          OutBufM[j] := Round(D);
          Inc(j);
          Inc(remainder, step);
        end;
      end;
      for i := 0 to FKernelWidth-2 do TailMono[i] := InBufM[i+s-FKernelWidth+1]
    end else
    begin
      TailMonoD := Tail;
      FillChar(DAM[0], Length(DAM)*8, 0);
      for i := 0 to FKernelWidth-2 do
      begin
        DAM[i] := TailMonoD[i];
        TailMonoD[i] := 0;
      end;
      Step := Finput.SampleRate;
      j := 0;
      if remainder < 0 then remainder := 0;
      while remainder < FOutSampleRate do
      begin
        m := Round(((FOutSampleRate - remainder)*LBS.Left +  remainder*InBufM[0])/FOutSampleRate);
        for k := 0 to FKernelWidth-1 do
        DAM[j+k] := DAM[j+k] + m*Kernel[k];
        Inc(j);
        Inc(remainder, step);
      end;
      Dec(remainder, FOutSampleRate);
      for i := 0 to s - 2 do
      begin
        while remainder < FOutSampleRate do
        begin
          m := Round(((FOutSampleRate - remainder)*InBufM[i] +  remainder*InBufM[i+1])/FOutSampleRate);
          for k := 0 to FKernelWidth-1 do
          DAM[j+k] := DAM[j+k] + m*Kernel[k];
          Inc(j);
          Inc(remainder, step);
        end;
        Dec(remainder, FOutSampleRate);
      end;
      LBS.Left := InBufM[s-1];
      for i := 0 to j-1 do
      OutBufM[i] := Round(DAM[i]);
      for i := 0 to FKernelWidth-2 do TailMonoD[i] := DAM[i+j];
    end;
    Result := j shl 1;
  end;

  function TRateConverter.ConvertFreqs16Stereo(InSize : Integer): Integer;
  var
    i, step, j, k, s, m1, m2 : Integer;
    D1, D2 : Double;
    TailStereo : PStereoBuffer16;
    TailStereoD : PStereoBufferD;
  begin
    TailStereo := Tail;
    s := InSize shr 2;
    if FInput.SampleRate > FOutSampleRate then
    begin
      step := FInput.SampleRate - FOutSampleRate;
      j := 0;
      if remainder < 0 then remainder := FOutSampleRate;
      for i := 0 to s - 1 do
      begin
        if remainder > FOutSampleRate then Dec(remainder, FOutSampleRate)
        else begin
          D1 := 0;
          D2 := 0;
          for k := 0 to FKernelWidth - 1 do
          if i-k >= 0 then
          begin
            D1 := D1 + InBufS[i-k].Left*Kernel[FKernelWidth - 1 - k];
            D2 := D2 + InBufS[i-k].Right*Kernel[FKernelWidth - 1 - k];
          end else
          begin
            D1 := D1 + TailStereo[FKernelWidth-1+i-k].Left*Kernel[FKernelWidth - 1 - k];
            D2 := D2 + TailStereo[FKernelWidth-1+i-k].Right*Kernel[FKernelWidth - 1 - k];
          end;
          OutBufS[j].Left := Round(D1);
          OutBufS[j].Right := Round(D2);
          Inc(j);
          Inc(remainder, step);
        end;
      end;
      for i := 0 to FKernelWidth-2 do TailStereo[i] := InBufS[i+s-FKernelWidth+1]
      //Move(InBufS[s-FKernelWidth+1], TailStereo[0], FKernelWidth-1);
    end else
    begin
      TailStereoD := Tail;
      FillChar(DAS[0], Length(DAS)*16, 0);
      for i := 0 to FKernelWidth-2 do
      begin
        DAS[i] := TailStereoD[i];
        TailStereoD[i].Left := 0;
        TailStereoD[i].Right := 0;
      end;
      Step := Finput.SampleRate;
      j := 0;
      if remainder < 0 then remainder := 0;
      while remainder < FOutSampleRate do
      begin
        m1 := Round(((FOutSampleRate - remainder)*LBS.Left +  remainder*InBufS[0].Left)/FOutSampleRate);
        m2 := Round(((FOutSampleRate - remainder)*LBS.Right +  remainder*InBufS[0].Right)/FOutSampleRate);
        for k := 0 to FKernelWidth-1 do
        begin
          DAS[j+k].Left := DAS[j+k].Left + m1*Kernel[k]; //InBufS[i].Left*Kernel[k];
          DAS[j+k].Right := DAS[j+k].Right + m2*Kernel[k]; //InBufS[i].Right*Kernel[k];
        end;
        Inc(j);
        Inc(remainder, step);
      end;
      Dec(remainder, FOutSampleRate);
      for i := 0 to s - 2 do
      begin
        while remainder < FOutSampleRate do
        begin
          m1 := Round(((FOutSampleRate - remainder)*InBufS[i].Left +  remainder*InBufS[i+1].Left)/FOutSampleRate);
          m2 := Round(((FOutSampleRate - remainder)*InBufS[i].Right +  remainder*InBufS[i+1].Right)/FOutSampleRate);
          for k := 0 to FKernelWidth-1 do
          begin
           DAS[j+k].Left := DAS[j+k].Left + m1*Kernel[k]; //InBufS[i].Left*Kernel[k];
           DAS[j+k].Right := DAS[j+k].Right + m2*Kernel[k]; //InBufS[i].Right*Kernel[k];
          end;
          Inc(j);
          Inc(remainder, step);
        end;
        Dec(remainder, FOutSampleRate);
      end;
      LBS := InBufS[s-1];
      for i := 0 to j-1 do
      begin
        OutBufS[i].Left := Round(DAS[i].Left);
        OutBufS[i].Right := Round(DAS[i].Right);
      end;
      for i := 0 to FKernelWidth-2 do TailStereoD[i] := DAS[i+j];
    end;
    Result := j shl 2;
  end;

  procedure Convert16To8(InOutBuf : PBuffer8; InSize : Integer);
  var
    i : Integer;
    P : PBuffer16;
  begin
    P := @InOutBuf[0];
    for i := 0 to (Insize shr 1) -1 do
    InOutBuf[i] := Hi(P[i]+$8000);
  end;

  procedure Convert8To16(InOutBuf : PBuffer8; InSize : Integer);
  var
    i : Integer;
    P : PBuffer16;
  begin
    P := @InOutBuf[0];
    for i := Insize - 1 downto 0 do P[i] := (InOutBuf[i] shl 8) - $8000;
  end;

  procedure ConvertStereoToMono16(InOutBuf : PBuffer16; InSize : Integer);
  var
    i : Integer;
  begin
    for i := 0 to (Insize shr 2) - 1 do
    begin
      InOutBuf[i] := (InOutBuf[i shl 1] + InOutBuf[(i shl 1)+1]) div 2;
    end;
  end;


  procedure ConvertMonoToStereo16(InOutBuf : PBuffer16; InSize : Integer; Mode : TMSConverterMode);
  var
    i : Integer;
  begin
    case Mode of
      msmMonoToBoth :
      for i := (Insize shr 1) - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := InOutBuf[i];
        InOutBuf[(i shl 1)+1] := InOutBuf[i];
      end;
      msmMonoToLeft :
      for i := (Insize shr 1) - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := 0;
        InOutBuf[(i shl 1)+1] := InOutBuf[i];
      end;
      msmMonoToRight :
      for i := (Insize shr 1) - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := InOutBuf[i];
        InOutBuf[(i shl 1)+1] := 0;
      end;
    end;
  end;

  function GCD(a, b : Integer) : Integer;
  var
    p, q, r : Integer;
  begin
    p := a;
    q := b;
    r := p mod q;
    while r <> 0 do
    begin
      p := q;
      q := r;
      r := p mod q;
    end;
    Result := q;
  end;

  constructor TRateConverter.Create;
  begin
    inherited Create(AOwner);
    FOutSampleRate := 22050;
    FKernelWidth := 30;
    FFilterWindow := fwBlackman;
  end;

  destructor TRateConverter.Destroy;
  begin
    Kernel := nil;
    DAS := nil;
    DAM := nil;
    inherited Destroy;
  end;

  function TRateConverter.GetBPS;
  begin
    Result := 16;
  end;

  function TRateConverter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := FInput.Channels;
  end;

  function TRateConverter.GetSR;
  begin
    Result := FOutSampleRate;
  end;

  procedure TRateConverter.Init;
  var
    Ratio : Single;
    TailSize : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    FInput.Init;
    InputLock := False;
    Buisy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    EndOfInput := False;
    Ratio := FOutSampleRate/Finput.SampleRate;
    if Ratio > 1. then
    WantedSize := (Trunc(BUF_SIZE/Ratio) shr 2) * 4
    else WantedSize := BUF_SIZE;
    if Finput.Channels = 1  then
    begin
      GetMem(InBufM, WantedSize);
      GetMem(OutBufM, BUF_SIZE);
      if Ratio < 1. then
      TailSize := (KernelWidth-1)*2
      else
      begin
        SetLength(DAM, (BUF_SIZE div 2)+KernelWidth);
        TailSize := (KernelWidth-1)*8;
      end;
      FillChar(DAM[0], Length(DAM)*Sizeof(DAM[0]), 0);
    end else
    begin
      GetMem(InBufS, WantedSize);
      GetMem(OutBufS, BUF_SIZE);
      if Ratio < 1. then
      TailSize := (KernelWidth-1)*4
      else
      begin
        SetLength(DAS, (BUF_SIZE div 4)+KernelWidth);
        TailSize := (KernelWidth-1)*16;
      end;
    end;
    GetMem(Tail, TailSize);
    FillChar(Tail^, TailSize, 0);
    FSize := Round(FInput.Size*Ratio);
    remainder := -1;
    if Ratio > 1. then Ratio := 1/Ratio;
    Ratio := Ratio*0.4;
    SetLength(Kernel, FKernelWidth);
    CalculateSincKernel(@Kernel[0], Ratio, FKernelWidth, FFilterWindow);
  end;

  procedure TRateConverter.Flush;
  begin
    FreeMem(Tail);
    FInput.Flush;
    if Finput.Channels = 1  then
    begin
      FreeMem(InBufM);
      FreeMem(OutBufM);
    end else
    begin
      FreeMem(InBufS);
      FreeMem(OutBufS);
    end;
    Buisy := False;
  end;

  function TRateConverter.GetData;
  var
    l : Integer;
    InSize : Integer;
    P : PBuffer8;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if EndOfInput then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;
      if FInput.Channels = 1 then P := Pointer(InBufM)
      else P := Pointer(InBufS);
      while InputLock do;
      InputLock := True;
      l := Finput.GetData(@P[0], WantedSize);
      InputLock := False;
      if l = 0 then
      begin
        Result := 0;
        Exit;
      end;
      InSize := l;
      while (l<>0) and (InSize < WantedSize) do
      begin
        while InputLock do;
        InputLock := True;
        l := Finput.GetData(@P[InSize], WantedSize - InSize);
        InputLock := False;
        Inc(InSize, l);
      end;
      if l = 0 then EndOfInput := True;
      if Self.Channels = 1 then
      begin
        BufEnd := ConvertFreqs16Mono(InSize);
      end else
      begin
        BufEnd := ConvertFreqs16Stereo(InSize);
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    if FInput.Channels = 1 then P := Pointer(OutBufM)
    else P := Pointer(OutBufS);
    Move(P[BufStart-1], Buffer^, Result);
    Inc(BufStart, Result);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  constructor TMSConverter.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TMSConverter.Destroy;
  begin
    inherited Destroy;
  end;

  function TMSConverter.GetBPS;
  begin
    Result := 16;
  end;

  function TMSConverter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    if FInput.Channels = 1 then Result := 2
    else Result := 1;
  end;

  function TMSConverter.GetSR;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TMSConverter.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    FInput.Init;
    Buisy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    InputLock := False;
    EndOfInput := False;
    if FInput.Channels = 2 then WantedSize := BUF_SIZE else
    WantedSize := BUF_SIZE shr 1;
    if FInput.Channels = 2 then
    FSize := FInput.Size shr 1
    else FSize := FInput.Size shl 1;
  end;

  procedure TMSConverter.Flush;
  begin
    FInput.Flush;
    Buisy := False;
  end;

  function TMSConverter.GetData;
  var
    l : Integer;
    InSize : Integer;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if EndOfInput then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;
      while InputLock do;
      InputLock := True;
      l := Finput.GetData(@InOutBuf[1], WantedSize);
      InputLock := False;
      if l = 0 then
      begin
        Result := 0;
        Exit;
      end;
      InSize := l;
      while (l<>0) and (InSize < WantedSize) do
      begin
        while InputLock do;
        InputLock := True;
        l := Finput.GetData(@InOutBuf[InSize+1], WantedSize - InSize);
        InputLock := False;
        Inc(InSize, l);
      end;
      if l = 0 then EndOfInput := True;
      if FInput.Channels = 2 then
      begin
        ConvertStereoToMono16(@InOutBuf[1], InSize);
        BufEnd := InSize shr 1;
      end else
      begin
        ConvertMonoToStereo16(@InOutBuf[1], InSize, FMode);
        BufEnd := InSize shl 1;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(InOutBuf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  constructor TSampleConverter.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TSampleConverter.Destroy;
  begin
    inherited Destroy;
  end;

  function TSampleConverter.GetBPS;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    if FInput.BitsPerSample = 16 then Result := 8
    else Result := 16;
  end;

  function TSampleConverter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result:= FInput.Channels;
  end;

  function TSampleConverter.GetSR;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TSampleConverter.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    FInput.Init;
    Buisy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    InputLock := False;
    EndOfInput := False;
    if FInput.BitsPerSample = 16 then WantedSize := BUF_SIZE else
    WantedSize := BUF_SIZE shr 1;
    if FInput.BitsPerSample = 16 then
    FSize := FInput.Size shr 1
    else FSize := FInput.Size shl 1;
  end;

  procedure TSampleConverter.Flush;
  begin
    FInput.Flush;
    Buisy := False;
  end;

  function TSampleConverter.GetData;
  var
    l : Integer;
    InSize : Integer;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if EndOfInput then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;
      while InputLock do;
      InputLock := True;
      l := Finput.GetData(@InOutBuf[1], WantedSize);
      InputLock := False;
      if l = 0 then
      begin
        Result := 0;
        Exit;
      end;
      InSize := l;
      while (l<>0) and (InSize < WantedSize) do
      begin
        while InputLock do;
        InputLock := True;
        l := Finput.GetData(@InOutBuf[InSize+1], WantedSize - InSize);
        InputLock := False;
        Inc(InSize, l);
      end;
      if l = 0 then EndOfInput := True;
      if FInput.BitsPerSample = 16 then
      begin
        Convert16To8(@InOutBuf[1], InSize);
        BufEnd := InSize shr 1;
      end else
      begin
        Convert8To16(@InOutBuf[1], InSize);
        BufEnd := InSize shl 1;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(InOutBuf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  procedure TRateConverter.SetOutSampleRate(aSR : Integer);
  begin
    if (aSR > 0) and (not Buisy) then FOutSampleRate := aSR;
  end;

  procedure TRateConverter.SetKernelWidth;
  begin
    if (aKW > 1) and (not Buisy) then FKernelWidth := aKW;
  end;

  constructor TStereoBalance.Create;
  begin
    inherited Create(AOwner);
    FBalance := 0.5;
  end;

  destructor TStereoBalance.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TStereoBalance.SetBalance;
  begin
    if (a >= 0) and (a <=1) then FBalance := a;
  end;

  function TStereoBalance.GetBPS;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TStereoBalance.GetCh;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := 2;
  end;

  function TStereoBalance.GetSR;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TStereoBalance.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input not assigned');
    FInput.Init;
    Buisy := True;
    if FInput.Channels = 2 then FSize := FInput.Size
    else FSize := FInput.Size*2;
    FPosition := 0;
    InputLock := False;
  end;

  procedure TStereoBalance.Flush;
  begin
    FInput.Flush;
    Buisy := False;
  end;

  function TStereoBalance.GetData;
  var
    WantedSize, i : Integer;
    P16 : PBuffer16;
    P8 : PBuffer8;
    Diff : Double;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    while InputLock do;
    InputLock := True;
    if FInput.Channels = 2 then WantedSize := BufferSize
    else WantedSize := BufferSize shr 1;
    Result := Finput.GetData(Buffer, WantedSize);
    InputLock := False;
    if Result = 0 then Exit;
    if FInput.Channels = 1 then
    begin
      if FInput.BitsPerSample = 8 then
      begin
        P8 := Buffer;
        for i := Result*2-1 downto 1 do P8[i] := P8[i shr 1];
      end else
      begin
        P16 := Buffer;
        for i := Result-1 downto 1 do
        P16[i] := P16[i shr 1];
      end;
      Result := Result*2;
    end;
    if FInput.BitsPerSample = 8 then
    begin
      P8 := Buffer;
      if FBalance > 0.5 then
      begin
        Diff := 1-Balance;
        for i := 0 to (Result shr 1) -1 do
        P8[i*2] := Round(P8[i*2]*Diff);
      end else
      begin
        for i := 0 to (Result shr 1) -1 do
        P8[i*2+1] := Round(P8[i*2+1]*FBalance);
      end;
    end else
    begin
      P16 := Buffer;
      if FBalance > 0.5 then
      begin
        Diff := 1-Balance;
        for i := 0 to (Result shr 2) -1 do
        P16[i*2] := Round(P16[i*2]*Diff);
      end else
      begin
        for i := 0 to (Result shr 2) -1 do
        P16[i*2+1] := Round(P16[i*2+1]*FBalance);
      end;
    end;
    FPosition := Round(FSize/FInput.Size)*FInput.Position;
  end;
end.
