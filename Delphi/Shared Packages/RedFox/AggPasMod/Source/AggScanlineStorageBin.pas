unit AggScanLineStorageBin;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$Q-}
{$R-}

uses
  AggBasics,
  AggArray,
  AggScanLine,
  AggRendererScanLine,
  AggRenderScanLines,
  AggRasterizerScanLine;

type
  PAggSpanData = ^TAggSpanData;
  TAggSpanData = record
    X, Len: Int32;
  end;

  PAggScanLineData = ^TAggScanLineData;
  TAggScanLineData = record
    Y: Integer;
    NumSpans, Start_Span: Cardinal;
  end;

  TAggScanLineStorageBin = class;
  TAggEmbeddedScanLineBin = class;

  TAggConstIteratorBin = class(TAggCustomSpan)
  private
    FStorage: TAggScanLineStorageBin;
    FSpanIndex: Cardinal;
    FSpan: TAggSpanData;
  public
    constructor Create(ScanLine: TAggEmbeddedScanLineBin);

    function X: Integer; virtual;
    function Len: Integer; virtual;

    procedure IncOperator; virtual;
  end;

  TAggEmbeddedScanLineBin = class(TAggCustomScanLine)
  private
    FStorage: TAggScanLineStorageBin;
    FScanLine: TAggScanLineData;

    FScanLineIndex: Cardinal;

    FResult: TAggConstIteratorBin;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;

    function GetSizeOfSpan: Cardinal; override;
    function GetIsPlainSpan: Boolean; override;
    function GetIsEmbedded: Boolean; override;
  public
    constructor Create(Storage: TAggScanLineStorageBin);

    procedure Reset(MinX, MaxX: Integer); override;

    function GetBegin: Pointer; override;

    procedure Setup(ScanLineIndex: Cardinal); override;
  end;

  TAggScanLineStorageBin = class(TAggCustomRendererScanLine)
  private
    FSpans, FScanLines: TAggPodDeque;

    FFakeSpan: TAggSpanData;
    FFakeScanLine: TAggScanLineData;

    FMin, FMax: TPointInteger;

    FCurrentScanLine: Cardinal;
  protected
    // Iterate ScanLines interface
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;

    // Renderer Interface
    procedure Prepare(U: Cardinal); override;
    procedure Render(ScanLine: TAggCustomScanLine); override;

    function RewindScanLines: Boolean; override;
    function SweepScanLine(ScanLine: TAggCustomScanLine): Boolean; override;

    // Specialization for embedded_ScanLine
    function SweepScanLineEm(ScanLine: TAggCustomScanLine): Boolean; override;

    function ByteSize: Cardinal;
    procedure WriteInt32(Dst: PInt8u; Val: Int32);
    procedure Serialize(Data: PInt8u);

    function ScanLineByIndex(I: Cardinal): PAggScanLineData;
    function SpanByIndex(I: Cardinal): PAggSpanData;
  end;

  TAggEmbeddedScanLineA = class;

  TAggConstIteratorA = class(TAggCustomSpan)
  private
    FInternalData: PInt8u;
    FSpan: TAggSpanData;
    FDeltaX: Integer;
  protected
    function GetX: Integer; override;
    function GetLength: Integer; override;
  public
    constructor Create(ScanLine: TAggEmbeddedScanLineA);

    procedure IncOperator; virtual;

    function ReadInt32: Integer;
  end;

  TAggEmbeddedScanLineA = class(TAggCustomScanLine)
  private
    FInternalData: PInt8u;
    FY: Integer;

    FNumSpans: Cardinal;

    FDeltaX: Integer;

    FResult: TAggConstIteratorA;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;

    function GetSizeOfSpan: Cardinal; override;
    function GetIsPlainSpan: Boolean; override;
    function GetIsEmbedded: Boolean; override;
  public
    constructor Create;

    procedure Reset(MinX, MaxX: Integer); override;

    function GetBegin: Pointer; override;

    function ReadInt32: Integer;
    procedure Init(Ptr: PInt8u; Dx, Dy: Integer); override;
  end;

  TAggSerializedScanLinesAdaptorBin = class(TAggRasterizerScanLine)
  private
    FData, FEnd, FInternalData: PInt8u;

    FDelta, FMin, FMax: TPointInteger;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; Size: Cardinal;
      Dx, Dy: Double); overload;

    procedure Init(Data: PInt8u; Size: Cardinal; Dx, Dy: Double);
    function ReadInt32: Integer;

    // Iterate ScanLines interface
    function RewindScanLines: Boolean; override;

    function SweepScanLine(ScanLine: TAggCustomScanLine): Boolean; override;

    // Specialization for embedded_ScanLine
    function SweepScanLineEm(ScanLine: TAggCustomScanLine): Boolean; override;
  end;

implementation


{ TAggConstIteratorBin }

constructor TAggConstIteratorBin.Create;
begin
  FStorage := ScanLine.FStorage;
  FSpanIndex := ScanLine.FScanLine.Start_Span;

  FSpan := FStorage.SpanByIndex(FSpanIndex)^;
end;

function TAggConstIteratorBin.X;
begin
  Result := FSpan.X;
end;

function TAggConstIteratorBin.Len;
begin
  Result := FSpan.Len;
end;

procedure TAggConstIteratorBin.IncOperator;
begin
  Inc(FSpanIndex);

  FSpan := FStorage.SpanByIndex(FSpanIndex)^;
end;


{ TAggEmbeddedScanLineBin }

constructor TAggEmbeddedScanLineBin.Create;
begin
  FStorage := Storage;

  Setup(0);
end;

procedure TAggEmbeddedScanLineBin.Reset;
begin
end;

function TAggEmbeddedScanLineBin.GetY;
begin
  Result := FScanLine.Y;
end;

function TAggEmbeddedScanLineBin.GetNumSpans;
begin
  Result := FScanLine.NumSpans;
end;

function TAggEmbeddedScanLineBin.GetBegin;
begin
  FResult := TAggConstIteratorBin.Create(@Self);

  Result := FResult;
end;

function TAggEmbeddedScanLineBin.GetSizeOfSpan;
begin
  Result := SizeOf(TAggSpanData);
end;

function TAggEmbeddedScanLineBin.GetIsPlainSpan;
begin
  Result := False;
end;

function TAggEmbeddedScanLineBin.GetIsEmbedded;
begin
  Result := True;
end;

procedure TAggEmbeddedScanLineBin.Setup;
begin
  FScanLineIndex := ScanLineIndex;
  FScanLine := FStorage.ScanLineByIndex(FScanLineIndex)^;
end;


{ TAggScanLineStorageBin }

constructor TAggScanLineStorageBin.Create;
begin
  FSpans := TAggPodDeque.Create(256 - 2, SizeOf(TAggSpanData), 10); // Block increment size
  FScanLines := TAggPodDeque.Create(SizeOf(TAggScanLineData), 8);

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FCurrentScanLine := 0;
  FFakeScanLine.Y := 0;

  FFakeScanLine.NumSpans := 0;
  FFakeScanLine.Start_Span := 0;

  FFakeSpan.X := 0;
  FFakeSpan.Len := 0;
end;

destructor TAggScanLineStorageBin.Destroy;
begin
  FSpans.Free;
  FScanLines.Free;
  inherited;
end;

procedure TAggScanLineStorageBin.Prepare(U: Cardinal);
begin
  FScanLines.RemoveAll;
  FSpans.RemoveAll;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FCurrentScanLine := 0;
end;

procedure TAggScanLineStorageBin.Render(ScanLine: TAggCustomScanLine);
var
  Y, X1, X2: Integer;

  ScanLineData  : TAggScanLineData;
  NumSpans: Cardinal;

  SpanData : PAggSpanData;
  Span: TAggCustomSpan;

  Ss: Cardinal;
  Sp: TAggSpanData;
begin
  Y := ScanLine.Y;

  if Y < FMin.Y then
    FMin.Y := Y;

  if Y > FMax.Y then
    FMax.Y := Y;

  ScanLineData.Y := Y;
  ScanLineData.NumSpans := ScanLine.NumSpans;
  ScanLineData.Start_Span := FSpans.Size;

  NumSpans := ScanLineData.NumSpans;

  SpanData := nil;
  Span := nil;

  if ScanLine.IsPlainSpan then
  begin
    SpanData := ScanLine.GetBegin;

    Ss := ScanLine.SizeOfSpan;

  end
  else
    Span := ScanLine.GetBegin;

  repeat
    if SpanData <> nil then
    begin
      Sp.X := SpanData.X;
      Sp.Len := SpanData.Len;

    end
    else
    begin
      Sp.X := Span.X;
      Sp.Len := Span.Len;
    end;

    FSpans.Add(@Sp);

    X1 := Sp.X;
    X2 := Sp.X + Sp.Len - 1;

    if X1 < FMin.X then
      FMin.X := X1;

    if X2 > FMax.X then
      FMax.X := X2;

    Dec(NumSpans);

    if NumSpans = 0 then
      Break;

    if SpanData <> nil then
      Inc(PtrComp(SpanData), Ss)
    else
      Span.IncOperator;

  until False;

  FScanLines.Add(@ScanLineData);
end;

function TAggScanLineStorageBin.GetMinX: Integer;
begin
  Result := FMin.X;
end;

function TAggScanLineStorageBin.GetMinY: Integer;
begin
  Result := FMin.Y;
end;

function TAggScanLineStorageBin.GetMaxX: Integer;
begin
  Result := FMax.X;
end;

function TAggScanLineStorageBin.GetMaxY: Integer;
begin
  Result := FMax.Y;
end;

function TAggScanLineStorageBin.RewindScanLines: Boolean;
begin
  FCurrentScanLine := 0;

  Result := FScanLines.Size > 0;
end;

function TAggScanLineStorageBin.SweepScanLine(ScanLine: TAggCustomScanLine): Boolean;
var
  ScanLineData: PAggScanLineData;
  NumSpans, SpanIndex: Cardinal;
  Sp: PAggSpanData;
begin
  ScanLine.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
    begin
      Result := False;

      Exit;
    end;

    ScanLineData := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.Start_Span;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      ScanLine.AddSpan(Sp.X, Sp.Len, CAggCoverFull);

      Dec(NumSpans);

    until NumSpans = 0;

    Inc(FCurrentScanLine);

    if ScanLine.NumSpans <> 0 then
    begin
      ScanLine.Finalize(ScanLineData.Y);

      Break;
    end;

  until False;

  Result := True;
end;

function TAggScanLineStorageBin.SweepScanLineEm(
  ScanLine: TAggCustomScanLine): Boolean;
begin
  repeat
    if FCurrentScanLine >= FScanLines.Size then
    begin
      Result := False;

      Exit;
    end;

    ScanLine.Setup(FCurrentScanLine);

    Inc(FCurrentScanLine);

  until ScanLine.NumSpans <> 0;

  Result := True;
end;

function TAggScanLineStorageBin.ByteSize: Cardinal;
var
  I, Size: Cardinal;
begin
  Size := SizeOf(Int32) * 4; // MinX, min_y, MaxX, max_y

  I := 0;

  while I < FScanLines.Size do
  begin
    Size := Size + SizeOf(Int32) * 2 + // Y, NumSpans
      Cardinal(PAggScanLineData(FScanLines[I]).NumSpans) *
      SizeOf(Int32) * 2; // X, Span_len

    Inc(I);
  end;

  Result := Size;
end;

procedure TAggScanLineStorageBin.WriteInt32(Dst: PInt8u; Val: Int32);
begin
  PInt8u(Dst)^ := TInt32Int8uAccess(Val).Values[0];
  PInt8u(PtrComp(Dst) + SizeOf(Int8u))^ := TInt32Int8uAccess(Val).Values[1];
  PInt8u(PtrComp(Dst) + 2 * SizeOf(Int8u))^ := TInt32Int8uAccess(Val).Values[2];
  PInt8u(PtrComp(Dst) + 3 * SizeOf(Int8u))^ := TInt32Int8uAccess(Val).Values[3];
end;

procedure TAggScanLineStorageBin.Serialize(Data: PInt8u);
var
  I, NumSpans, SpanIndex: Cardinal;
  ScanLineData: PAggScanLineData;
  Sp: PAggSpanData;
begin
  WriteInt32(Data, GetMinX); // MinX
  Inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMinY); // min_y
  Inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxX); // MaxX
  Inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxY); // max_y
  Inc(PtrComp(Data), SizeOf(Int32));

  I := 0;

  while I < FScanLines.Size do
  begin
    ScanLineData := FScanLines[I];

    WriteInt32(Data, ScanLineData.Y); // Y
    Inc(PtrComp(Data), SizeOf(Int32));

    WriteInt32(Data, ScanLineData.NumSpans); // NumSpans
    Inc(PtrComp(Data), SizeOf(Int32));

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.Start_Span;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      WriteInt32(Data, Sp.X); // X
      Inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, Sp.Len); // len
      Inc(PtrComp(Data), SizeOf(Int32));

      Dec(NumSpans);

    until NumSpans = 0;

    Inc(I);
  end;
end;

function TAggScanLineStorageBin.ScanLineByIndex(I: Cardinal): PAggScanLineData;
begin
  if I < FScanLines.Size then
    Result := FScanLines[I]
  else
    Result := @FFakeScanLine;
end;

function TAggScanLineStorageBin.SpanByIndex(I: Cardinal): PAggSpanData;
begin
  if I < FSpans.Size then
    Result := FSpans[I]
  else
    Result := @FFakeSpan;
end;


{ TAggConstIteratorA }

constructor TAggConstIteratorA.Create;
begin
  FInternalData := ScanLine.FInternalData;
  FDeltaX := ScanLine.FDeltaX;

  FSpan.X := ReadInt32 + FDeltaX;
  FSpan.Len := ReadInt32;
end;

function TAggConstIteratorA.GetX;
begin
  Result := FSpan.X;
end;

function TAggConstIteratorA.GetLength;
begin
  Result := FSpan.Len;
end;

procedure TAggConstIteratorA.IncOperator;
begin
  FSpan.X := ReadInt32 + FDeltaX;
  FSpan.Len := ReadInt32;
end;

function TAggConstIteratorA.ReadInt32;
begin
  TInt32Int8uAccess(Result).Values[0] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
end;


{ TAggEmbeddedScanLineA }

constructor TAggEmbeddedScanLineA.Create;
begin
  FInternalData := nil;
  FY := 0;

  FNumSpans := 0;
end;

procedure TAggEmbeddedScanLineA.Reset;
begin
end;

function TAggEmbeddedScanLineA.GetY;
begin
  Result := FY;
end;

function TAggEmbeddedScanLineA.GetNumSpans;
begin
  Result := FNumSpans;
end;

function TAggEmbeddedScanLineA.GetBegin;
begin
  FResult := TAggConstIteratorA.Create(Self);

  Result := FResult;
end;

function TAggEmbeddedScanLineA.GetSizeOfSpan;
begin
  Result := SizeOf(TAggSpanData);
end;

function TAggEmbeddedScanLineA.GetIsPlainSpan;
begin
  Result := False;
end;

function TAggEmbeddedScanLineA.GetIsEmbedded;
begin
  Result := True;
end;

function TAggEmbeddedScanLineA.ReadInt32;
begin
  TInt32Int8uAccess(Result).Values[0] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
end;

procedure TAggEmbeddedScanLineA.Init;
begin
  FInternalData := Ptr;
  FY := ReadInt32 + Dy;
  FNumSpans := Cardinal(ReadInt32);
  FDeltaX := Dx;
end;


{ TAggSerializedScanLinesAdaptorBin }

constructor TAggSerializedScanLinesAdaptorBin.Create;
begin
  FData := nil;
  FEnd := nil;
  FInternalData := nil;

  FDelta.X := 0;
  FDelta.Y := 0;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

constructor TAggSerializedScanLinesAdaptorBin.Create(Data: PInt8u; Size: Cardinal;
  Dx, Dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.X := Trunc(Dx + 0.5);
  FDelta.Y := Trunc(Dy + 0.5);

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

procedure TAggSerializedScanLinesAdaptorBin.Init(Data: PInt8u; Size: Cardinal;
  Dx, Dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.X := Trunc(Dx + 0.5);
  FDelta.Y := Trunc(Dy + 0.5);

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

function TAggSerializedScanLinesAdaptorBin.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).Values[0] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FInternalData^;
  Inc(PtrComp(FInternalData), SizeOf(Int8u));
end;

function TAggSerializedScanLinesAdaptorBin.RewindScanLines: Boolean;
begin
  FInternalData := FData;

  if PtrComp(FInternalData) < PtrComp(FEnd) then
  begin
    FMin.X := ReadInt32 + FDelta.X;
    FMin.Y := ReadInt32 + FDelta.Y;
    FMax.X := ReadInt32 + FDelta.X;
    FMax.Y := ReadInt32 + FDelta.Y;

    Result := True;

  end
  else
    Result := False;
end;

function TAggSerializedScanLinesAdaptorBin.GetMinX: Integer;
begin
  Result := FMin.X;
end;

function TAggSerializedScanLinesAdaptorBin.GetMinY: Integer;
begin
  Result := FMin.Y;
end;

function TAggSerializedScanLinesAdaptorBin.GetMaxX: Integer;
begin
  Result := FMax.X;
end;

function TAggSerializedScanLinesAdaptorBin.GetMaxY: Integer;
begin
  Result := FMax.Y;
end;

function TAggSerializedScanLinesAdaptorBin.SweepScanLine(
  ScanLine: TAggCustomScanLine): Boolean;
var
  Y, X, Len: Integer;
  NumSpans: Cardinal;
begin
  ScanLine.ResetSpans;

  repeat
    if PtrComp(FInternalData) >= PtrComp(FEnd) then
    begin
      Result := False;

      Exit;
    end;

    Y := ReadInt32 + FDelta.Y;
    NumSpans := ReadInt32;

    repeat
      X := ReadInt32 + FDelta.X;
      Len := ReadInt32;

      if Len < 0 then
        Len := -Len;

      ScanLine.AddSpan(X, Cardinal(Len), CAggCoverFull);

      Inc(NumSpans);

    until NumSpans = 0;

    if ScanLine.NumSpans <> 0 then
    begin
      ScanLine.Finalize(Y);

      Break;
    end;

  until False;

  Result := True;
end;

function TAggSerializedScanLinesAdaptorBin.SweepScanLineEm;
var
  NumSpans: Integer;

begin
  repeat
    if PtrComp(FInternalData) >= PtrComp(FEnd) then
    begin
      Result := False;

      Exit;
    end;

    ScanLine.Init(FInternalData, FDelta.X, FDelta.Y);

    // Jump to the next ScanLine
    ReadInt32; // Y

    NumSpans := ReadInt32; // NumSpans

    Inc(PtrComp(FInternalData), NumSpans * SizeOf(Int32) * 2);

  until ScanLine.NumSpans <> 0;

  Result := True;
end;

end.
