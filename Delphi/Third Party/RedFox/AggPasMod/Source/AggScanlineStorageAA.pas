unit AggScanLineStorageAA;

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
  PAggExtraSpan = ^TAggExtraSpan;
  TAggExtraSpan = record
    Len: Cardinal;
    Ptr: Pointer;
  end;

  TAggScanLineCellStorage = class
  private
    FCells, FExtraStorage: TAggPodDeque;
    function ArrayOperator(Index: Integer): Pointer;
  public
    constructor Create(EntrySize: Cardinal); overload;
    constructor Create(V: TAggScanLineCellStorage); overload;
    destructor Destroy; override;

    procedure RemoveAll;
    function AddCells(Cells: Pointer; NumCells: Cardinal): Integer;

    function AssignOperator(V: TAggScanLineCellStorage): TAggScanLineCellStorage;

    procedure CopyExtraStorage(V: TAggScanLineCellStorage);

    property CellPointer[Index: Integer]: Pointer read ArrayOperator; default;
  end;

  PAggSpanDataSS = ^TAggSpanDataSS;
  TAggSpanDataSS = record
    X, Len: Int32; // If negative, it's a solid Span, covers is valid
    CoversID: Integer; // The index of the cells in the TAggScanLineCellStorage
  end;

  PAggScanLineDataSS = ^TAggScanLineDataSS;
  TAggScanLineDataSS = record
    Y: Integer;
    NumSpans, StartSpan: Cardinal;
  end;

  PAggSpanSS = ^TAggSpanSS;
  TAggSpanSS = record
    X, Len: Int32; // If negative, it's a solid Span, covers is valid

    Covers: Pointer;
  end;

  TAggCustomScanLineStorageAA = class;
  TAggEmbeddedScanLineSS = class;

  TAggConstIteratorSS = class(TAggCustomSpan)
  private
    FStorage: TAggCustomScanLineStorageAA;
    FSpanIdx: Cardinal;
    FSpan: TAggSpanSS;
  protected
    function GetX: Integer; override;
    function GetLength: Integer; override;
  public
    constructor Create(Sl: TAggEmbeddedScanLineSS);

    function Covers: PInt8u; override;

    procedure IncOperator; override;
    procedure IniTAggSpan;
  end;

  TAggEmbeddedScanLineSS = class(TAggCustomScanLine)
  private
    FStorage: TAggCustomScanLineStorageAA;
    FScanLine: TAggScanLineDataSS;
    FScanLineIndex: Cardinal;

    FResult: TAggConstIteratorSS;
  public
    constructor Create(Storage: TAggCustomScanLineStorageAA);

    procedure Reset(MinX, MaxX: Integer); override;

    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
    function GetBegin: Pointer; override;

    function GetSizeOfSpan: Cardinal; override;
    function GetIsPlainSpan: Boolean; override;
    function GetIsEmbedded: Boolean; override;

    procedure Setup(ScanLineIndex: Cardinal); override;
  end;

  TAggCustomScanLineStorageAA = class(TAggCustomRendererScanLine)
  private
    FCovers: TAggScanLineCellStorage;
    FSpans, FScanLines: TAggPodDeque;

    FFakeSpan: TAggSpanDataSS;
    FFakeScanLine: TAggScanLineDataSS;

    FMin, FMax: TPointInteger;

    FCurrentScanLine: Cardinal;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Renderer Interface
    procedure Prepare(U: Cardinal); override;
    procedure Render(Sl: TAggCustomScanLine); override;

    // Iterate ScanLines interface
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;

    function RewindScanLines: Boolean; override;
    function SweepScanLine(Sl: TAggCustomScanLine): Boolean; override;

    // Specialization for embedded_ScanLine
    function SweepScanLineEm(Sl: TAggCustomScanLine): Boolean; override;

    function ByteSize: Cardinal; virtual;
    procedure WriteInt32(Dst: PInt8u; Val: Int32);
    procedure Serialize(Data: PInt8u); virtual;

    function ScanLineByIndex(I: Cardinal): PAggScanLineDataSS;
    function SpanByIndex(I: Cardinal): PAggSpanDataSS;
    function CoversByIndex(I: Integer): Pointer;
  end;

  TAggScanLineStorageAA8 = class(TAggCustomScanLineStorageAA)
  public
    constructor Create; override;
  end;

  TAggScanLineStorageAA16 = class(TAggCustomScanLineStorageAA)
  public
    constructor Create; override;

    function SweepScanLine(Sl: TAggCustomScanLine): Boolean; override;

    function ByteSize: Cardinal; override;
    procedure Serialize(Data: PInt8u); override;
  end;

  TAggScanLineStorageAA32 = class(TAggCustomScanLineStorageAA)
  public
    constructor Create; override;

    function SweepScanLine(Sl: TAggCustomScanLine): Boolean; override;

    function ByteSize: Cardinal; override;
    procedure Serialize(Data: PInt8u); override;
  end;

  TAggEmbeddedScanLineSA = class;

  TAggConstIteratorSA = class(TAggCustomSpan)
  private
    FPtr: PInt8u;
    FSpan: TAggSpanSS;

    FDeltaX: Integer;
    FSize: Cardinal;
    function GetSize: Cardinal;
  protected
    function GetX: Integer; override;
    function GetLength: Integer; override;
  public
    constructor Create(Sl: TAggEmbeddedScanLineSA; Sz: Cardinal);

    function Covers: PInt8u; override;

    procedure IncOperator; override;
    procedure IniTAggSpan;

    function ReadInt32: Integer;

    property Size: Cardinal read GetSize;
  end;

  TAggEmbeddedScanLineSA = class(TAggCustomScanLine)
  private
    FPtr: PInt8u;
    FY: Integer;

    FNumSpans: Cardinal;

    FDeltaX: Integer;
    FSize: Cardinal;

    FResult: TAggConstIteratorSA;

    function GetSize: Cardinal;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;

    function GetSizeOfSpan: Cardinal; override;
    function GetIsPlainSpan: Boolean; override;
    function GetIsEmbedded: Boolean; override;
  public
    constructor Create(Size: Cardinal);

    procedure Reset(MinX, MaxX: Integer); override;

    function GetBegin: Pointer; override;
    procedure Init(Ptr: PInt8u; Dx, Dy: Integer); override;

    function ReadInt32: Integer;

    property Size: Cardinal read GetSize;
  end;

  TAggSerializedScanLinesAdaptorAA = class(TAggRasterizerScanLine)
  private
    FData, FEnd, FPtr: PInt8u;

    FDelta, FMin, FMax: TPointInteger;

    FSize: Cardinal;

    function GetSize: Cardinal;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;
  public
    constructor Create(Sz: Cardinal); overload;
    constructor Create(Sz: Cardinal; Data: PInt8u; ASize: Cardinal;
      Dx, Dy: Double); overload;

    procedure Init(Data: PInt8u; ASize: Cardinal; Dx, Dy: Double);

    function ReadInt32: Integer;
    function ReadInt32u: Cardinal;

    // Iterate ScanLines interface
    function RewindScanLines: Boolean; override;

    function SweepScanLine(Sl: TAggCustomScanLine): Boolean; override;

    // Specialization for embedded_ScanLine
    function SweepScanLineEm(Sl: TAggCustomScanLine): Boolean; override;

    property Size: Cardinal read GetSize;
  end;

  TAggSerializedScanLinesAdaptorAA8 = class(TAggSerializedScanLinesAdaptorAA)
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; ASize: Cardinal;
      Dx, Dy: Double); overload;
  end;

  TAggSerializedScanLinesAdaptorAA16 = class(TAggSerializedScanLinesAdaptorAA)
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; ASize: Cardinal;
      Dx, Dy: Double); overload;
  end;

  TAggSerializedScanLinesAdaptorAA32 = class(TAggSerializedScanLinesAdaptorAA)
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; ASize: Cardinal;
      Dx, Dy: Double); overload;
  end;

implementation


{ TAggScanLineCellStorage }

constructor TAggScanLineCellStorage.Create(EntrySize: Cardinal);
begin
  FCells := TAggPodDeque.Create(128 - 2, EntrySize, 12);
  FExtraStorage := TAggPodDeque.Create(SizeOf(TAggExtraSpan), 6);
end;

constructor TAggScanLineCellStorage.Create(V: TAggScanLineCellStorage);
begin
  FCells := TAggPodDeque.Create(V.FCells.EntrySize);
  FExtraStorage := TAggPodDeque.Create(SizeOf(TAggExtraSpan), 6);

  AssignOperator(V);
  CopyExtraStorage(V);
end;

destructor TAggScanLineCellStorage.Destroy;
begin
  RemoveAll;

  FCells.Free;
  FExtraStorage.Free;

  inherited;
end;

procedure TAggScanLineCellStorage.RemoveAll;
var
  I: Integer;
  S: PAggExtraSpan;
begin
  I := FExtraStorage.Size;
  Dec(I);

  while I >= 0 do
  begin
    S := FExtraStorage[I];

    AggFreeMem(S.Ptr, S.Len * FCells.EntrySize);

    Dec(I);
  end;

  FExtraStorage.RemoveAll;
  FCells.RemoveAll;
end;

function TAggScanLineCellStorage.AddCells;
var
  Index: Integer;
  Ptr: Pointer;

  S: TAggExtraSpan;

begin
  Index := FCells.AllocateContinuousBlock(NumCells);

  if Index >= 0 then
  begin
    Ptr := FCells[Index];

    Move(Cells^, Ptr^, FCells.EntrySize * NumCells);

    Result := Index;

    Exit;
  end;

  S.Len := NumCells;

  AggGetMem(S.Ptr, S.Len * FCells.EntrySize);

  Move(Cells^, S.Ptr^, S.Len * FCells.EntrySize);

  FExtraStorage.Add(@S);

  Result := -Integer(FExtraStorage.Size);
end;

function TAggScanLineCellStorage.AssignOperator;
begin
  RemoveAll;

  FCells.AssignOperator(@V.FCells);
  CopyExtraStorage(V);

  Result := @Self;
end;

function TAggScanLineCellStorage.ArrayOperator;
var
  I: Cardinal;

begin
  if Index >= 0 then
  begin
    if Index >= FCells.Size then
    begin
      Result := nil;

      Exit;
    end;

    Result := FCells[Index];

    Exit;
  end;

  I := Cardinal(-Index - 1);

  if I >= FExtraStorage.Size then
  begin
    Result := 0;

    Exit;
  end;

  Result := PAggExtraSpan(FExtraStorage[I]).Ptr;
end;

procedure TAggScanLineCellStorage.CopyExtraStorage;
var
  I: Cardinal;

  Src: PAggExtraSpan;
  Dst: TAggExtraSpan;
begin
  I := 0;

  while I < V.FExtraStorage.Size do
  begin
    Src := V.FExtraStorage[I];

    Dst.Len := Src.Len;

    AggGetMem(Dst.Ptr, Dst.Len * V.FCells.EntrySize);

    Move(Src.Ptr^, Dst.Ptr^, Dst.Len * V.FCells.EntrySize);

    FExtraStorage.Add(@Dst);

    Inc(I);
  end;
end;


{ TAggConstIteratorSS }

constructor TAggConstIteratorSS.Create;
begin
  FStorage := Sl.FStorage;
  FSpanIdx := Sl.FScanLine.StartSpan;

  IniTAggSpan;
end;

function TAggConstIteratorSS.GetX;
begin
  Result := FSpan.X;
end;

function TAggConstIteratorSS.GetLength;
begin
  Result := FSpan.Len;
end;

function TAggConstIteratorSS.Covers;
begin
  Result := FSpan.Covers;
end;

procedure TAggConstIteratorSS.IncOperator;
begin
  Inc(FSpanIdx);

  IniTAggSpan;
end;

procedure TAggConstIteratorSS.IniTAggSpan;
var
  S: PAggSpanDataSS;

begin
  S := FStorage.SpanByIndex(FSpanIdx);

  FSpan.X := S.X;
  FSpan.Len := S.Len;
  FSpan.Covers := FStorage.CoversByIndex(S.CoversID);
end;


{ TAggEmbeddedScanLineSS }

constructor TAggEmbeddedScanLineSS.Create;
begin
  FStorage := Storage;

  Setup(0);
end;

procedure TAggEmbeddedScanLineSS.Reset;
begin
end;

function TAggEmbeddedScanLineSS.GetY;
begin
  Result := FScanLine.Y;
end;

function TAggEmbeddedScanLineSS.GetNumSpans;
begin
  Result := FScanLine.NumSpans;
end;


{ TAggEmbeddedScanLineSS }

function TAggEmbeddedScanLineSS.GetBegin;
begin
  FResult := TAggConstIteratorSS.Create(Self);

  Result := FResult;
end;

function TAggEmbeddedScanLineSS.GetSizeOfSpan;
begin
  Result := SizeOf(TAggSpanSS);
end;

function TAggEmbeddedScanLineSS.GetIsPlainSpan;
begin
  Result := False;
end;

function TAggEmbeddedScanLineSS.GetIsEmbedded;
begin
  Result := True;
end;

procedure TAggEmbeddedScanLineSS.Setup;
begin
  FScanLineIndex := ScanLineIndex;
  FScanLine := FStorage.ScanLineByIndex(FScanLineIndex)^;
end;


{ TAggCustomScanLineStorageAA }

constructor TAggCustomScanLineStorageAA.Create;
begin
  FSpans := TAggPodDeque.Create(256 - 2, SizeOf(TAggSpanDataSS), 10); // Block increment size
  FScanLines := TAggPodDeque.Create(SizeOf(TAggScanLineDataSS), 8);

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FCurrentScanLine := 0;

  FFakeScanLine.Y := 0;
  FFakeScanLine.NumSpans := 0;
  FFakeScanLine.StartSpan := 0;

  FFakeSpan.X := 0;
  FFakeSpan.Len := 0;
  FFakeSpan.CoversID := 0;
end;

destructor TAggCustomScanLineStorageAA.Destroy;
begin
  FCovers.Free;
  FSpans.Free;
  FScanLines.Free;
  inherited;
end;

procedure TAggCustomScanLineStorageAA.Prepare;
begin
  FCovers.RemoveAll;
  FScanLines.RemoveAll;
  FSpans.RemoveAll;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FCurrentScanLine := 0;
end;

procedure TAggCustomScanLineStorageAA.Render;
var
  ScanLineData: TAggScanLineDataSS;

  Y, X1, X2, Len: Integer;

  NumSpans, Ss: Cardinal;

  SpanData : PAggSpanRecord;
  Span: TAggCustomSpan;

  Sp: TAggSpanDataSS;

begin
  Y := Sl.Y;

  if Y < FMin.Y then
    FMin.Y := Y;

  if Y > FMax.Y then
    FMax.Y := Y;

  ScanLineData.Y := Y;
  ScanLineData.NumSpans := Sl.NumSpans;
  ScanLineData.StartSpan := FSpans.Size;

  NumSpans := ScanLineData.NumSpans;

  SpanData := nil;
  Span := nil;

  if Sl.IsPlainSpan then
  begin
    SpanData := Sl.GetBegin;

    Ss := Sl.SizeOfSpan;
  end
  else
    Span := Sl.GetBegin;

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

    Len := Abs(Sp.Len);

    if SpanData <> nil then
      Sp.CoversID := FCovers.AddCells(SpanData.Covers, Cardinal(Len))
    else
      Sp.CoversID := FCovers.AddCells(Span.Covers, Cardinal(Len));

    FSpans.Add(@Sp);

    X1 := Sp.X;
    X2 := Sp.X + Len - 1;

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

function TAggCustomScanLineStorageAA.GetMinX;
begin
  Result := FMin.X;
end;

function TAggCustomScanLineStorageAA.GetMinY;
begin
  Result := FMin.Y;
end;

function TAggCustomScanLineStorageAA.GetMaxX;
begin
  Result := FMax.X;
end;

function TAggCustomScanLineStorageAA.GetMaxY;
begin
  Result := FMax.Y;
end;

function TAggCustomScanLineStorageAA.RewindScanLines;
begin
  FCurrentScanLine := 0;

  Result := FScanLines.Size > 0;
end;

function TAggCustomScanLineStorageAA.SweepScanLine;
var
  ScanLineData: PAggScanLineDataSS;
  NumSpans, SpanIndex: Cardinal;
  Sp: PAggSpanDataSS;
  Covers: PInt8u;
begin
  Sl.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
    begin
      Result := False;

      Exit;
    end;

    ScanLineData := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      Covers := CoversByIndex(Sp.CoversID);

      if Sp.Len < 0 then
        Sl.AddSpan(Sp.X, Cardinal(-Sp.Len), Covers^)
      else
        Sl.AddCells(Sp.X, Sp.Len, Covers);

      Dec(NumSpans);
    until NumSpans = 0;

    Inc(FCurrentScanLine);

    if Sl.NumSpans <> 0 then
    begin
      Sl.Finalize(ScanLineData.Y);

      Break;
    end;

  until False;

  Result := True;
end;

function TAggCustomScanLineStorageAA.SweepScanLineEm;
begin
  repeat
    if FCurrentScanLine >= FScanLines.Size then
    begin
      Result := False;

      Exit;
    end;

    Sl.Setup(FCurrentScanLine);

    Inc(FCurrentScanLine);

  until Sl.NumSpans <> 0;

  Result := True;
end;

function TAggCustomScanLineStorageAA.ByteSize;
var
  I, Size, NumSpans, SpanIndex: Cardinal;

  ScanLineData: PAggScanLineDataSS;

  Sp: PAggSpanDataSS;

begin
  Size := SizeOf(Int32) * 4; // MinX, MinY, MaxX, MaxY

  I := 0;

  while I < FScanLines.Size do
  begin
    Inc(Size, SizeOf(Int32) * 3); // ScanLine size in bytes, Y, NumSpans

    ScanLineData := FScanLines[I];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);
      Inc(Size, SizeOf(Int32) * 2); // X, Span Length

      if Sp.Len < 0 then
        Inc(Size, SizeOf(Int8u)) // cover
      else
        Inc(Size, SizeOf(Int8u) * Cardinal(Sp.Len)); // covers

      Dec(NumSpans);
    until NumSpans = 0;

    Inc(I);
  end;

  Result := Size;
end;

procedure TAggCustomScanLineStorageAA.WriteInt32;
begin
  PInt8u(Dst)^ := TInt32Int8uAccess(Val).Values[0];
  PInt8u(PtrComp(Dst) + SizeOf(Int8u))^ := TInt32Int8uAccess(Val).Values[1];
  PInt8u(PtrComp(Dst) + 2 * SizeOf(Int8u))^ := TInt32Int8uAccess(Val).Values[2];
  PInt8u(PtrComp(Dst) + 3 * SizeOf(Int8u))^ := TInt32Int8uAccess(Val).Values[3];
end;

procedure TAggCustomScanLineStorageAA.Serialize;
var
  I, NumSpans, SpanIndex: Cardinal;

  ScanLineThis: PAggScanLineDataSS;

  Sp: PAggSpanDataSS;

  Covers: PInt8u;

  SizePointer: PInt8u;

begin
  WriteInt32(Data, GetMinX); // MinX
  Inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMinY); // MinY
  Inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxX); // MaxX
  Inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxY); // MaxY
  Inc(PtrComp(Data), SizeOf(Int32));

  I := 0;

  while I < FScanLines.Size do
  begin
    ScanLineThis := FScanLines[I];
    SizePointer := Data;

    Inc(PtrComp(Data), SizeOf(Int32));
    // Reserve space for ScanLine size in bytes

    WriteInt32(Data, ScanLineThis.Y); // Y
    Inc(PtrComp(Data), SizeOf(Int32));

    WriteInt32(Data, ScanLineThis.NumSpans); // NumSpans
    Inc(PtrComp(Data), SizeOf(Int32));

    NumSpans := ScanLineThis.NumSpans;
    SpanIndex := ScanLineThis.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      Covers := CoversByIndex(Sp.CoversID);

      WriteInt32(Data, Sp.X); // X
      Inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, Sp.Len); // Span Length
      Inc(PtrComp(Data), SizeOf(Int32));

      if Sp.Len < 0 then
      begin
        Move(Covers^, Data^, SizeOf(Int8u));
        Inc(PtrComp(Data), SizeOf(Int8u));
      end
      else
      begin
        Move(Covers^, Data^, Cardinal(Sp.Len) * SizeOf(Int8u));
        Inc(PtrComp(Data), SizeOf(Int8u) * Cardinal(Sp.Len));
      end;

      Dec(NumSpans);
    until NumSpans = 0;

    WriteInt32(SizePointer, PtrComp(Data) - PtrComp(SizePointer));

    Inc(I);
  end;
end;

function TAggCustomScanLineStorageAA.ScanLineByIndex;
begin
  if I < FScanLines.Size then
    Result := FScanLines[I]
  else
    Result := @FFakeScanLine;
end;

function TAggCustomScanLineStorageAA.SpanByIndex;
begin
  if I < FSpans.Size then
    Result := FSpans[I]
  else
    Result := @FFakeSpan;
end;

function TAggCustomScanLineStorageAA.CoversByIndex;
begin
  Result := FCovers[I];
end;


{ TAggScanLineStorageAA8 }

constructor TAggScanLineStorageAA8.Create;
begin
  inherited;
  FCovers := TAggScanLineCellStorage.Create(SizeOf(Int8u));
end;


{ TAggScanLineStorageAA16 }

constructor TAggScanLineStorageAA16.Create;
begin
  inherited;
  FCovers := TAggScanLineCellStorage.Create(SizeOf(Int16u));
end;

function TAggScanLineStorageAA16.SweepScanLine(Sl: TAggCustomScanLine): Boolean;
var
  ScanLineData: PAggScanLineDataSS;
  NumSpans, SpanIndex: Cardinal;
  Sp: PAggSpanDataSS;
  Covers: PInt16u;
begin
  Sl.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
    begin
      Result := False;

      Exit;
    end;

    ScanLineData := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      Covers := CoversByIndex(Sp.CoversID);

      if Sp.Len < 0 then
        Sl.AddSpan(Sp.X, Cardinal(-Sp.Len), Covers^)
      else
        Sl.AddCells(Sp.X, Sp.Len, PInt8u(Covers));

      Dec(NumSpans);
    until NumSpans = 0;

    Inc(FCurrentScanLine);

    if Sl.NumSpans <> 0 then
    begin
      Sl.Finalize(ScanLineData.Y);

      Break;
    end;

  until False;

  Result := True;
end;

function TAggScanLineStorageAA16.ByteSize;
var
  I, Size, NumSpans, SpanIndex: Cardinal;

  ScanLineData: PAggScanLineDataSS;

  Sp: PAggSpanDataSS;

begin
  Size := SizeOf(Int32) * 4; // MinX, min_y, MaxX, max_y

  I := 0;

  while I < FScanLines.Size do
  begin
    Inc(Size, SizeOf(Int32) * 3); // ScanLine size in bytes, Y, NumSpans

    ScanLineData := FScanLines[I];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);
      Inc(Size, SizeOf(Int32) * 2); // X, Span Length

      if Sp.Len < 0 then
        Inc(Size, SizeOf(Int16u)) // cover
      else
        Inc(Size, SizeOf(Int16u) * Cardinal(Sp.Len)); // covers

      Dec(NumSpans);
    until NumSpans = 0;

    Inc(I);
  end;

  Result := Size;
end;

procedure TAggScanLineStorageAA16.Serialize;
var
  I, NumSpans, SpanIndex: Cardinal;
  ScanLineThis: PAggScanLineDataSS;
  Sp: PAggSpanDataSS;
  Covers: PInt16u;
  SizePointer: PInt8u;
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
    ScanLineThis := FScanLines[I];
    SizePointer := Data;

    Inc(PtrComp(Data), SizeOf(Int32));
    // Reserve space for ScanLine size in bytes

    WriteInt32(Data, ScanLineThis.Y); // Y
    Inc(PtrComp(Data), SizeOf(Int32));

    WriteInt32(Data, ScanLineThis.NumSpans); // NumSpans
    Inc(PtrComp(Data), SizeOf(Int32));

    NumSpans := ScanLineThis.NumSpans;
    SpanIndex := ScanLineThis.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      Covers := CoversByIndex(Sp.CoversID);

      WriteInt32(Data, Sp.X); // X
      Inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, Sp.Len); // Span Length
      Inc(PtrComp(Data), SizeOf(Int32));

      if Sp.Len < 0 then
      begin
        Move(Covers^, Data^, SizeOf(Int16u));
        Inc(PtrComp(Data), SizeOf(Int16u));
      end
      else
      begin
        Move(Covers^, Data^, Cardinal(Sp.Len) * SizeOf(Int16u));
        Inc(PtrComp(Data), SizeOf(Int16u) * Cardinal(Sp.Len));
      end;

      Dec(NumSpans);
    until NumSpans = 0;

    WriteInt32(SizePointer, PtrComp(Data) - PtrComp(SizePointer));

    Inc(I);
  end;
end;


{ TAggScanLineStorageAA32 }

constructor TAggScanLineStorageAA32.Create;
begin
  inherited;
  FCovers := TAggScanLineCellStorage.Create(SizeOf(Int32u));
end;

function TAggScanLineStorageAA32.SweepScanLine;
var
  ScanLineThis: PAggScanLineDataSS;

  NumSpans, SpanIndex: Cardinal;

  Sp: PAggSpanDataSS;

  Covers: PInt32u;

begin
  Sl.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
    begin
      Result := False;

      Exit;
    end;

    ScanLineThis := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineThis.NumSpans;
    SpanIndex := ScanLineThis.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      Covers := CoversByIndex(Sp.CoversID);

      if Sp.Len < 0 then
        Sl.AddSpan(Sp.X, Cardinal(-Sp.Len), Covers^)
      else
        Sl.AddCells(Sp.X, Sp.Len, PInt8u(Covers));

      Dec(NumSpans);
    until NumSpans = 0;

    Inc(FCurrentScanLine);

    if Sl.NumSpans <> 0 then
    begin
      Sl.Finalize(ScanLineThis.Y);

      Break;
    end;

  until False;

  Result := True;
end;

function TAggScanLineStorageAA32.ByteSize;
var
  I, Size, NumSpans, SpanIndex: Cardinal;
  ScanLineThis: PAggScanLineDataSS;
  Sp: PAggSpanDataSS;
begin
  Size := SizeOf(Int32) * 4; // MinX, min_y, MaxX, max_y

  I := 0;

  while I < FScanLines.Size do
  begin
    Inc(Size, SizeOf(Int32) * 3); // ScanLine size in bytes, Y, NumSpans

    ScanLineThis := FScanLines[I];

    NumSpans := ScanLineThis.NumSpans;
    SpanIndex := ScanLineThis.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);
      Inc(Size, SizeOf(Int32) * 2); // X, Span Length

      if Sp.Len < 0 then
        Inc(Size, SizeOf(Int32u)) // cover
      else
        Inc(Size, SizeOf(Int32u) * Cardinal(Sp.Len)); // covers

      Dec(NumSpans);
    until NumSpans = 0;

    Inc(I);
  end;

  Result := Size;
end;

procedure TAggScanLineStorageAA32.Serialize;
var
  I, NumSpans, SpanIndex: Cardinal;

  ScanLineThis: PAggScanLineDataSS;

  Sp: PAggSpanDataSS;

  Covers: PInt32u;

  SizePointer: PInt8u;

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
    ScanLineThis := FScanLines[I];
    SizePointer := Data;

    Inc(PtrComp(Data), SizeOf(Int32));
    // Reserve space for ScanLine size in bytes

    WriteInt32(Data, ScanLineThis.Y); // Y
    Inc(PtrComp(Data), SizeOf(Int32));

    WriteInt32(Data, ScanLineThis.NumSpans); // NumSpans
    Inc(PtrComp(Data), SizeOf(Int32));

    NumSpans := ScanLineThis.NumSpans;
    SpanIndex := ScanLineThis.StartSpan;

    repeat
      Sp := FSpans[SpanIndex];

      Inc(SpanIndex);

      Covers := CoversByIndex(Sp.CoversID);

      WriteInt32(Data, Sp.X); // X
      Inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, Sp.Len); // Span Length
      Inc(PtrComp(Data), SizeOf(Int32));

      if Sp.Len < 0 then
      begin
        Move(Covers^, Data^, SizeOf(Int32u));
        Inc(PtrComp(Data), SizeOf(Int32u));

      end
      else
      begin
        Move(Covers^, Data^, Cardinal(Sp.Len) * SizeOf(Int32u));
        Inc(PtrComp(Data), SizeOf(Int32u) * Cardinal(Sp.Len));
      end;

      Dec(NumSpans);

    until NumSpans = 0;

    WriteInt32(SizePointer, PtrComp(Data) - PtrComp(SizePointer));

    Inc(I);
  end;
end;


{ TAggConstIteratorSA }

constructor TAggConstIteratorSA.Create(Sl: TAggEmbeddedScanLineSA; Sz: Cardinal);
begin
  FPtr := Sl.FPtr;
  FDeltaX := Sl.FDeltaX;
  FSize := Sz;

  IniTAggSpan;
end;

function TAggConstIteratorSA.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggConstIteratorSA.GetX;
begin
  Result := FSpan.X;
end;

function TAggConstIteratorSA.GetLength;
begin
  Result := FSpan.Len;
end;

function TAggConstIteratorSA.Covers;
begin
  Result := FSpan.Covers;
end;

procedure TAggConstIteratorSA.IncOperator;
begin
  if FSpan.Len < 0 then
    Inc(PtrComp(FPtr), FSize)
  else
    Inc(PtrComp(FPtr), FSpan.Len * FSize);

  IniTAggSpan;
end;

procedure TAggConstIteratorSA.IniTAggSpan;
begin
  FSpan.X := ReadInt32 + FDeltaX;
  FSpan.Len := ReadInt32;
  FSpan.Covers := FPtr;
end;

function TAggConstIteratorSA.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).Values[0] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
end;


{ TAggEmbeddedScanLineSA }

constructor TAggEmbeddedScanLineSA.Create(Size: Cardinal);
begin
  FPtr := nil;
  FY := 0;
  FSize := Size;

  FNumSpans := 0;
end;

function TAggEmbeddedScanLineSA.GetSize: Cardinal;
begin
  Result := FSize;
end;

procedure TAggEmbeddedScanLineSA.Reset(MinX, MaxX: Integer);
begin
end;

function TAggEmbeddedScanLineSA.GetY: Integer;
begin
  Result := FY;
end;

function TAggEmbeddedScanLineSA.GetNumSpans: Cardinal;
begin
  Result := FNumSpans;
end;

function TAggEmbeddedScanLineSA.GetBegin: Pointer;
begin
  FResult := TAggConstIteratorSA.Create(Self, FSize);

  Result := FResult;
end;

function TAggEmbeddedScanLineSA.GetSizeOfSpan: Cardinal;
begin
  Result := SizeOf(TAggSpanSS);
end;

function TAggEmbeddedScanLineSA.GetIsPlainSpan: Boolean;
begin
  Result := False;
end;

function TAggEmbeddedScanLineSA.GetIsEmbedded: Boolean;
begin
  Result := True;
end;

procedure TAggEmbeddedScanLineSA.Init(Ptr: PInt8u; Dx, Dy: Integer);
begin
  FPtr := Ptr;
  FY := ReadInt32 + Dy;
  FNumSpans := Cardinal(ReadInt32);
  FDeltaX := Dx;
end;

function TAggEmbeddedScanLineSA.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).Values[0] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
end;


{ TAggSerializedScanLinesAdaptorAA }

constructor TAggSerializedScanLinesAdaptorAA.Create(Sz: Cardinal);
begin
  FData := nil;
  FEnd := nil;
  FPtr := nil;

  FDelta.X := 0;
  FDelta.Y := 0;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FSize := Sz;
end;

constructor TAggSerializedScanLinesAdaptorAA.Create(Sz: Cardinal;
  Data: PInt8u; ASize: Cardinal; Dx, Dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + ASize);
  FPtr := Data;

  FDelta.X := Trunc(Dx + 0.5);
  FDelta.Y := Trunc(Dy + 0.5);

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FSize := Sz;
end;

procedure TAggSerializedScanLinesAdaptorAA.Init(Data: PInt8u; ASize: Cardinal; Dx, Dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + ASize);
  FPtr := Data;

  FDelta.X := Trunc(Dx + 0.5);
  FDelta.Y := Trunc(Dy + 0.5);

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

function TAggSerializedScanLinesAdaptorAA.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).Values[0] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
end;

function TAggSerializedScanLinesAdaptorAA.ReadInt32u: Cardinal;
begin
  TInt32Int8uAccess(Result).Values[0] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[1] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[2] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).Values[3] := FPtr^;
  Inc(PtrComp(FPtr), SizeOf(Int8u));
end;

function TAggSerializedScanLinesAdaptorAA.RewindScanLines;
begin
  Result := False;
  FPtr := FData;

  if PtrComp(FPtr) < PtrComp(FEnd) then
  begin
    FMin.X := ReadInt32 + FDelta.X;
    FMin.Y := ReadInt32 + FDelta.Y;
    FMax.X := ReadInt32 + FDelta.X;
    FMax.Y := ReadInt32 + FDelta.Y;

    Result := True;
  end;
end;

function TAggSerializedScanLinesAdaptorAA.GetMinX;
begin
  Result := FMin.X;
end;

function TAggSerializedScanLinesAdaptorAA.GetMinY;
begin
  Result := FMin.Y;
end;

function TAggSerializedScanLinesAdaptorAA.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggSerializedScanLinesAdaptorAA.GetMaxX;
begin
  Result := FMax.X;
end;

function TAggSerializedScanLinesAdaptorAA.GetMaxY;
begin
  Result := FMax.Y;
end;

function TAggSerializedScanLinesAdaptorAA.SweepScanLine(Sl: TAggCustomScanLine): Boolean;
var
  Y, X, Len: Integer;
  NumSpans: Cardinal;
begin
  Sl.ResetSpans;

  repeat
    if PtrComp(FPtr) >= PtrComp(FEnd) then
    begin
      Result := False;
      Exit;
    end;

    ReadInt32; // Skip ScanLine size in bytes
    Y := ReadInt32 + FDelta.Y;
    NumSpans := ReadInt32;

    repeat
      X := ReadInt32 + FDelta.X;
      Len := ReadInt32;

      if Len < 0 then
      begin
        Sl.AddSpan(X, Cardinal(-Len), FPtr^);
        Inc(PtrComp(FPtr), FSize);
      end
      else
      begin
        Sl.AddCells(X, Len, FPtr);
        Inc(PtrComp(FPtr), Len * FSize);
      end;
      Dec(NumSpans);
    until NumSpans = 0;

    if Sl.NumSpans <> 0 then
    begin
      Sl.Finalize(Y);
      Break;
    end;
  until False;
  Result := True;
end;

function TAggSerializedScanLinesAdaptorAA.SweepScanLineEm;
var
  ByteSize: Cardinal;
begin
  repeat
    if PtrComp(FPtr) >= PtrComp(FEnd) then
    begin
      Result := False;
      Exit;
    end;
    ByteSize := ReadInt32u;
    Sl.Init(FPtr, FDelta.X, FDelta.Y);
    Inc(PtrComp(FPtr), ByteSize - SizeOf(Int32));
  until Sl.NumSpans <> 0;
  Result := True;
end;


{ TAggSerializedScanLinesAdaptorAA8 }

constructor TAggSerializedScanLinesAdaptorAA8.Create;
begin
  inherited Create(SizeOf(Int8u));
end;

constructor TAggSerializedScanLinesAdaptorAA8.Create(Data: PInt8u;
  ASize: Cardinal; Dx, Dy: Double);
begin
  inherited Create(SizeOf(Int8u), Data, ASize, Dx, Dy);
end;


{ TAggSerializedScanLinesAdaptorAA16 }

constructor TAggSerializedScanLinesAdaptorAA16.Create;
begin
  inherited Create(SizeOf(Int16u));
end;

constructor TAggSerializedScanLinesAdaptorAA16.Create(Data: PInt8u;
  ASize: Cardinal; Dx, Dy: Double);
begin
  inherited Create(SizeOf(Int8u), Data, ASize, Dx, Dy);
end;


{ TAggSerializedScanLinesAdaptorAA32 }

constructor TAggSerializedScanLinesAdaptorAA32.Create;
begin
  inherited Create(SizeOf(Int32u));
end;

constructor TAggSerializedScanLinesAdaptorAA32.Create(Data: PInt8u;
  ASize: Cardinal; Dx, Dy: Double);
begin
  inherited Create(SizeOf(Int8u), Data, ASize, Dx, Dy);
end;

end.
