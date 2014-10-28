unit AggRasterizerScanLineAA;

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
//                                                                            //
// Class TAggOutlineAA - implementation.                                      //
//                                                                            //
// Initially the rendering algorithm was designed by David Turner and the     //
// other authors of the FreeType library - see the above notice. I nearly     //
// created a similar Renderer, but still I was far from David's work.         //
// I completely redesigned the original code and adapted it for Anti-Grain    //
// ideas. Two functions - RenderLine and RenderHorizontalLine are the core    //
// of the algorithm - they calculate the exact coverage of each pixel cell    //
// of the polygon. I left these functions almost as is, because there's       //
// no way to improve the perfection - hats off to David and his group!        //
//                                                                            //
// All other code is very different from the original.                        //
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
  AggRasterizerScanLine,
  AggVertexSource,
  AggGammaFunctions,
  AggClipLiangBarsky;

const
  CAggAntiAliasingShift = 8;
  CAggAntiAliasingNum = 1 shl CAggAntiAliasingShift;
  CAggAntiAliasingMask = CAggAntiAliasingNum - 1;
  CAggAntiAliasing2Num = CAggAntiAliasingNum * 2;
  CAggAntiAliasing2Mask = CAggAntiAliasing2Num - 1;

  CAggCellBlockShift = 12;
  CAggCellBlockSize = 1 shl CAggCellBlockShift;
  CAggCellBlockMask = CAggCellBlockSize - 1;
  CAggCellBlockPool = 256;
  CAggCellBlockLimit = 1024;

  // These constants determine the subpixel accuracy, to be more precise,
  // the number of bits of the fractional part of the coordinates.
  // The possible coordinate capacity in bits can be calculated by formula:
  // SizeOf(Integer) * 8 - CAggPolyBaseShift * 2, i.e, for 32-bit integers and
  // 8-bits fractional part the capacity is 16 bits or [-32768...32767].
  CAggPolyBaseShift = 8; // ----CAggPolyBaseShift
  CAggPolyBaseSize = 1 shl CAggPolyBaseShift; // ----CAggPolyBaseSize
  CAggPolyBaseMask = CAggPolyBaseSize - 1; // ----CAggPolyBaseMask

type
  // A pixel cell. There're no constructors defined and it was done
  // intentionally in order to avoid extra overhead when allocating an
  // array of cells.

  PPAggCellAA = ^PAggCellAA;
  PAggCellAA = ^TAggCellAA;
  TAggCellAA = record
    X, Y, Cover, Area: Integer;
  end;

  // An internal class that implements the main rasterization algorithm.
  // Used in the Rasterizer. Should not be used direcly.

  PAggSortedY = ^TAggSortedY;
  TAggSortedY = record
    Start, Num: Cardinal;
  end;

  TAggOutlineAA = class
  private
    FNumBlocks, FMaxBlocks, FCurBlock, FNumCells: Cardinal;

    FCur, FMin, FMax: TPointInteger;

    FSorted: Boolean;

    FCells: PPAggCellAA;
    FCurCellPointer: PAggCellAA;
    FCurCell: TAggCellAA;

    FSortedCells: TAggPodArray;
    FSortedY: TAggPodArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);

    procedure Reset;

    procedure AddCurrentCell;
    procedure SetCurrentCell(X, Y: Integer);

    procedure SortCells;
    function ScanLineNumCells(Y: Cardinal): Cardinal;
    function ScanLineCells(Y: Cardinal): PPAggCellAA;

    procedure RenderLine(X1, Y1, X2, Y2: Integer);
    procedure RenderHorizontalLine(Ey, X1, Y1, X2, Y2: Integer);

    procedure AllocateBlock;

    property TotalCells: Cardinal read FNumCells;
    property Sorted: Boolean read FSorted;
    property MinX: Integer read FMin.X;
    property MinY: Integer read FMin.Y;
    property MaxX: Integer read FMax.X;
    property MaxY: Integer read FMax.Y;
  end;

  TAggScanLineHitTest = class(TAggCustomScanLine)
  private
    FX: Integer;
    FHit: Boolean;
  protected
    function GetNumSpans: Cardinal; override;
  public
    constructor Create(X: Integer);

    procedure ResetSpans; override;

    procedure Finalize(Y: Integer); override;
    procedure AddCell(X: Integer; Cover: Cardinal); override;
    procedure AddSpan(X: Integer; Len, Cover: Cardinal); override;

    property Hit: Boolean read FHit;
  end;

  // Polygon Rasterizer that is used to render filled polygons with
  // High-quality Anti-Aliasing. Internally, by default, the class uses
  // integer coordinates in format 24.8, i.e. 24 bits for integer part
  // and 8 bits for fractional - see CAggPolyBaseShift. This class can be
  // used in the following  way:
  //
  // 1. SetFillingRule(TAggFillingRule ft) - optional.
  //
  // 2. Gamma() - optional.
  //
  // 3. reset()
  //
  // 4. MoveTo(x, y) / LineTo(x, y) - make the polygon. One can create
  // more than one contour, but each contour must consist of at least 3
  // vertices, i.e. MoveTo(x1, y1); LineTo(x2, y2); LineTo(x3, y3);
  // is the absolute minimum of vertices that define a triangle.
  // The algorithm does not check either the number of vertices nor
  // coincidence of their coordinates, but in the worst case it just
  // won't draw anything.
  // The orger of the vertices (clockwise or counterclockwise)
  // is important when using the non-zero filling rule (frNonZero).
  // In this case the vertex order of all the contours must be the same
  // if you want your intersecting polygons to be without "holes".
  // You actually can use different vertices order. If the contours do not
  // intersect each other the order is not important anyway. If they do,
  // contours with the same vertex order will be rendered without "holes"
  // while the intersecting contours with different orders will have "holes".
  //
  // SetFillingRule() and Gamma() can be called anytime before "sweeping".
  // ------------------------------------------------------------------------
  // TAggFillingRule = (frNonZero ,frEvenOdd );

  TInitialStatus = (siStatusInitial, siStatusLineTo, siStatusClosed);

  TAggRasterizerScanLineAA = class(TAggRasterizerScanLine)
  private
    FOutline: TAggOutlineAA;
    FGamma: array [0..CAggAntiAliasingNum - 1] of Integer;

    FFillingRule: TAggFillingRule;
    FClippedStart: TPointInteger;
    FStart, FPrev: TPointInteger;

    FPrevFlags: Cardinal;
    FStatus: TInitialStatus;

    FClipBox: TRectInteger;
    FClipping: Boolean;

    FCurY, FXScale: Integer;

    FAutoClose: Boolean;

    procedure ClosePolygon;
    procedure ClosePolygonNoClip;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;

    function GetFillingRule: TAggFillingRule; override;
    procedure SetFillingRule(Value: TAggFillingRule); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; override;
    procedure AutoClose(Flag: Boolean);
    procedure SetClipBox(X1, Y1, X2, Y2: Double); override;
    procedure SetClipBox(Rect: TRectDouble); override;

    procedure Gamma(AGammaFunction: TAggCustomVertexSource); override;
    function ApplyGamma(Cover: Cardinal): Cardinal;

    procedure MoveToNoClip(X, Y: Integer); overload;
    procedure MoveToNoClip(Point: TPointInteger); overload;
    procedure LineToNoClip(X, Y: Integer); overload;
    procedure LineToNoClip(Point: TPointInteger); overload;

    procedure ClipSegment(X, Y: Integer); overload;
    procedure ClipSegment(Point: TPointInteger); overload;

    procedure MoveToDouble(X, Y: Double); overload;
    procedure MoveToDouble(Point: TPointDouble); overload;
    procedure LineToDouble(X, Y: Double); overload;
    procedure LineToDouble(Point: TPointDouble); overload;

    procedure MoveTo(X, Y: Integer); overload;
    procedure MoveTo(Point: TPointInteger); overload;
    procedure LineTo(X, Y: Integer); overload;
    procedure LineTo(Point: TPointInteger); overload;

    procedure Sort; override;
    function RewindScanLines: Boolean; override;
    function SweepScanLine(Sl: TAggCustomScanLine): Boolean; override;

    function NavigateScanLine(Y: Integer): Boolean;

    function HitTest(Tx, Ty: Integer): Boolean; override;

    function CalculateAlpha(Area: Integer): Cardinal;

    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0); override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;
  end;

function PolyCoord(Value: Double): Integer; overload;
function PolyCoord(Value: TPointDouble): TPointInteger; overload;
function PolyCoord(Value: TRectDouble): TRectInteger; overload;

implementation


function PolyCoord(Value: Double): Integer;
begin
  Result := Trunc(Value * CAggPolyBaseSize);
end;

function PolyCoord(Value: TPointDouble): TPointInteger;
begin
  Result.X := Trunc(Value.X * CAggPolyBaseSize);
  Result.Y := Trunc(Value.Y * CAggPolyBaseSize);
end;

function PolyCoord(Value: TRectDouble): TRectInteger; overload;
begin
  Result.X1 := Trunc(Value.X1 * CAggPolyBaseSize);
  Result.Y1 := Trunc(Value.Y1 * CAggPolyBaseSize);
  Result.X2 := Trunc(Value.X2 * CAggPolyBaseSize);
  Result.Y2 := Trunc(Value.Y2 * CAggPolyBaseSize);
end;


{ TAggOutlineAA }

constructor TAggOutlineAA.Create;
begin
  FSortedCells := TAggPodArray.Create(SizeOf(PAggCellAA));
  FSortedY := TAggPodArray.Create(SizeOf(TAggSortedY));

  FNumBlocks := 0;
  FMaxBlocks := 0;
  FCurBlock := 0;
  FNumCells := 0;

  FCur.X := 0;
  FCur.Y := 0;
  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FSorted := False;

  FCells := nil;
  FCurCellPointer := nil;

  with FCurCell do
  begin
    X := $7FFF;
    Y := $7FFF;

    Cover := 0;
    Area := 0;
  end;
end;

destructor TAggOutlineAA.Destroy;
begin
  FSortedCells.Free;
  FSortedY.Free;

  if FNumBlocks > 0 then
  begin
    repeat
      Dec(FNumBlocks);

      AggFreeMem(Pointer(Pointer(PtrComp(FCells) + FNumBlocks *
        SizeOf(PAggCellAA))^), CAggCellBlockSize * SizeOf(TAggCellAA));
    until FNumBlocks = 0;

    AggFreeMem(Pointer(FCells), SizeOf(PAggCellAA) * FMaxBlocks);
  end;

  inherited;
end;

procedure TAggOutlineAA.MoveTo(X, Y: Integer);
begin
  if FSorted then
    Reset;

  // SetCurrentCell(x shr CAggPolyBaseShift ,y shr CAggPolyBaseShift );
  SetCurrentCell(ShrInt32(X, CAggPolyBaseShift), ShrInt32(Y, CAggPolyBaseShift));

  FCur.X := X;
  FCur.Y := Y;
end;

procedure TAggOutlineAA.LineTo(X, Y: Integer);
begin
  RenderLine(FCur.X, FCur.Y, X, Y);

  FCur.X := X;
  FCur.Y := Y;

  FSorted := False;
end;

procedure TAggOutlineAA.Reset;
begin
  FNumCells := 0;
  FCurBlock := 0;

  FCurCell.X := $7FFF;
  FCurCell.Y := $7FFF;
  FCurCell.Cover := 0;
  FCurCell.Area := 0;

  FSorted := False;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

procedure TAggOutlineAA.AddCurrentCell;
begin
  if (FCurCell.Area or FCurCell.Cover) <> 0 then
  begin
    if (FNumCells and CAggCellBlockMask) = 0 then
    begin
      if FNumBlocks >= CAggCellBlockLimit then
        Exit;

      AllocateBlock;
    end;

    FCurCellPointer^ := FCurCell;

    Inc(PtrComp(FCurCellPointer), SizeOf(TAggCellAA));
    Inc(FNumCells);

    if FCurCell.X < FMin.X then
      FMin.X := FCurCell.X;

    if FCurCell.X > FMax.X then
      FMax.X := FCurCell.X;

    if FCurCell.Y < FMin.Y then
      FMin.Y := FCurCell.Y;

    if FCurCell.Y > FMax.Y then
      FMax.Y := FCurCell.Y;
  end;
end;

procedure TAggOutlineAA.SetCurrentCell(X, Y: Integer);
begin
  if (FCurCell.X <> X) or (FCurCell.Y <> Y) then
  begin
    AddCurrentCell;

    FCurCell.X := X;
    FCurCell.Y := Y;

    FCurCell.Cover := 0;
    FCurCell.Area := 0;
  end;
end;

procedure QuickSortCells(Start: PPAggCellAA; Num: Cardinal);
var
  Len, X: Integer;

  Stack       : array [0..79] of PPAggCellAA;
  Limit, Base : PPAggCellAA;
  I, J, Pivot : PPAggCellAA;
  Top         : ^PPAggCellAA;
const
  CQSortThreshold = 9;
begin
  Limit := PPAggCellAA(PtrComp(Start) + Num * SizeOf(Pointer));
  Base := Start;
  Top := @Stack[0];

  repeat
    Len := (PtrComp(Limit) - PtrComp(Base)) div SizeOf(Pointer);

    if Len > CQSortThreshold then
    begin
      // we use base + len/2 as the pivot
      Pivot := PPAggCellAA(PtrComp(Base) + (Len div 2) * SizeOf(Pointer));

      SwapPointers(Base, Pivot);

      I := PPAggCellAA(PtrComp(Base) + SizeOf(Pointer));
      J := PPAggCellAA(PtrComp(Limit) - SizeOf(Pointer));

      // now ensure that *i <= *base <= *j
      if J^.X < I^.X then
        SwapPointers(J, I);

      if Base^.X < I^.X then
        SwapPointers(Base, I);

      if J^.X < Base^.X then
        SwapPointers(Base, J);

      repeat
        X := Base^.X;

        Inc(PtrComp(I), SizeOf(PAggCellAA));

        while I^.X < X do
          Inc(PtrComp(I), SizeOf(PAggCellAA));

        Dec(PtrComp(J), SizeOf(PAggCellAA));

        while X < J^.X do
          Dec(PtrComp(J), SizeOf(PAggCellAA));

        if PtrComp(I) > PtrComp(J) then
          Break;

        SwapPointers(I, J);
      until False;

      SwapPointers(Base, J);

      // now, push the largest sub-array
      if (PtrComp(J) - PtrComp(Base)) div SizeOf(Pointer) >
        (PtrComp(Limit) - PtrComp(I)) div SizeOf(Pointer)
      then
      begin
        Top^ := Base;

        Inc(PtrComp(Top), SizeOf(PPAggCellAA));

        Top^ := J;
        Base := I;
      end
      else
      begin
        Top^ := I;

        Inc(PtrComp(Top), SizeOf(PPAggCellAA));

        Top^ := Limit;
        Limit := J;
      end;

      Inc(PtrComp(Top), SizeOf(PPAggCellAA));
    end
    else
    begin
      // the sub-array is small, perform insertion sort
      J := Base;
      I := PPAggCellAA(PtrComp(J) + SizeOf(Pointer));

      while PtrComp(I) < PtrComp(Limit) do
      begin
        while PPAggCellAA(PtrComp(J) + SizeOf(Pointer))^^.X < J^.X do
        begin
          SwapPointers(PPAggCellAA(PtrComp(j) + SizeOf(Pointer)), j);
          if J = Base then
            Break;

          Dec(J);
        end;

        J := I;

        Inc(PtrComp(I), SizeOf(PAggCellAA));
      end;

      if PtrComp(Top) > PtrComp(@Stack[0]) then
      begin
        Dec(PtrComp(Top), SizeOf(PPAggCellAA));

        Limit := Top^;

        Dec(PtrComp(Top), SizeOf(PPAggCellAA));

        Base := Top^;
      end
      else
        Break;
    end;

  until False;
end;

procedure TAggOutlineAA.SortCells;
var
  Nb, I, V, Start: Cardinal;
  CurY, CurMinY: PAggSortedY;

  BlockPtr: PPAggCellAA;
  CellPtr: PAggCellAA;
begin
  // Perform sort only the first time
  if FSorted then
    Exit;

  AddCurrentCell;

  if FNumCells = 0 then
    Exit;

  // Allocate the array of cell pointers
  FSortedCells.Allocate(FNumCells, 16);

  // Allocate and zero the Y array
  FSortedY.Allocate(FMax.Y - FMin.Y + 1, 16);
  FSortedY.Zero;

  // Create the Y-histogram (count the numbers of cells for each Y)
  BlockPtr := FCells;

  Nb := FNumCells shr CAggCellBlockShift;

  CurMinY := PAggSortedY(FSortedY.ArrayPointer);
  Dec(CurMinY, FMin.Y);

  while Nb > 0 do
  begin
    Dec(Nb);

    CellPtr := BlockPtr^;
    Inc(BlockPtr);
    I := CAggCellBlockSize;

    while I > 0 do
    begin
      Dec(I);
      CurY := CurMinY;
      Inc(CurY, CellPtr^.Y);
      Inc(CurY^.Start);
      Inc(CellPtr);
    end;
  end;

  CellPtr := BlockPtr^;

  Inc(BlockPtr);

  I := FNumCells and CAggCellBlockMask;

  while I > 0 do
  begin
    Dec(I);
    CurY := CurMinY;
    Inc(CurY, CellPtr^.Y);
    Inc(CurY^.Start);
    Inc(CellPtr);
  end;

  // Convert the Y-histogram into the array of starting indexes
  Start := 0;

  CurY := PAggSortedY(FSortedY.ArrayPointer);
  for I := 0 to FSortedY.Size - 1 do
  begin
    V := CurY^.Start;

    CurY^.Start := Start;
    Inc(CurY);

    Inc(Start, V);
  end;

  // Fill the cell pointer array sorted by Y
  BlockPtr := FCells;

  Nb := FNumCells shr CAggCellBlockShift;

  while Nb > 0 do
  begin
    Dec(Nb);

    CellPtr := BlockPtr^;

    Inc(BlockPtr);

    I := CAggCellBlockSize;

    while I > 0 do
    begin
      Dec(I);

      CurY := CurMinY;
      Inc(CurY, CellPtr.Y);

      PPointer(PtrComp(FSortedCells.ArrayPointer) +
        Cardinal(CurY.Start + CurY.Num) * FSortedCells.EntrySize)^ := CellPtr;

      Inc(CurY.Num);
      Inc(CellPtr);
    end;
  end;

  CellPtr := BlockPtr^;
  Inc(BlockPtr);
  I := FNumCells and CAggCellBlockMask;

  while I > 0 do
  begin
    Dec(I);

    CurY := CurMinY;
    Inc(CurY, CellPtr.Y);

    PPointer(PtrComp(FSortedCells.ArrayPointer) +
      Cardinal(CurY.Start + CurY.Num) * FSortedCells.EntrySize)^ := CellPtr;

    Inc(CurY.Num);
    Inc(CellPtr);
  end;

  // Finally arrange the X-arrays
  CurY := PAggSortedY(FSortedY.ArrayPointer);
  for I := 0 to FSortedY.Size - 1 do
  begin
    if CurY.Num > 0 then
      QuickSortCells(PPAggCellAA(PtrComp(FSortedCells.ArrayPointer) + CurY.Start
        * FSortedCells.EntrySize), CurY.Num);
    Inc(CurY);
  end;

  FSorted := True;
end;

function TAggOutlineAA.ScanLineNumCells(Y: Cardinal): Cardinal;
begin
  Result := PAggSortedY(PtrComp(FSortedY.ArrayPointer) + Cardinal(Y - FMin.Y) *
    FSortedY.EntrySize).Num;
end;

function TAggOutlineAA.ScanLineCells(Y: Cardinal): PPAggCellAA;
begin
  Result := PPAggCellAA(PtrComp(FSortedCells.ArrayPointer) +
    PAggSortedY(PtrComp(FSortedY.ArrayPointer) + Cardinal(Y - FMin.Y) *
    FSortedY.EntrySize).Start * FSortedCells.EntrySize);
end;

procedure TAggOutlineAA.RenderLine(X1, Y1, X2, Y2: Integer);
var
  Center, Delta: TPointInteger;
  P, Ex, Ey1, Ey2, Fy1, Fy2, Rem, DeltaMod,
  FromX, ToX, Lift, DeltaVal, First, Incr, TwoFx, Area: Integer;
const
  CDxLimit = 16384 shl CAggPolyBaseShift;
begin
  Delta.X := X2 - X1;

  if (Delta.X >= CDxLimit) or (Delta.X <= -CDxLimit) then
  begin
    Center.X := (X1 + X2) shr 1;
    Center.Y := (Y1 + Y2) shr 1;

    RenderLine(X1, Y1, Center.X, Center.Y);
    RenderLine(Center.X, Center.Y, X2, Y2);
  end;

  Delta.Y := Y2 - Y1;

  // ey1:=y1 shr CAggPolyBaseShift;
  // ey2:=y2 shr CAggPolyBaseShift;
  Ey1 := ShrInt32(Y1, CAggPolyBaseShift);
  Ey2 := ShrInt32(Y2, CAggPolyBaseShift);

  Fy1 := Y1 and CAggPolyBaseMask;
  Fy2 := Y2 and CAggPolyBaseMask;

  // everything is on a single HorizontalLine
  if Ey1 = Ey2 then
  begin
    RenderHorizontalLine(Ey1, X1, Fy1, X2, Fy2);
    Exit;
  end;

  // Vertical line - we have to calculate start and end cells,
  // and then - the common values of the area and coverage for
  // all cells of the line. We know exactly there's only one
  // cell, so, we don't have to call render_HorizontalLine().
  Incr := 1;

  if Delta.X = 0 then
  begin
    // Ex := x1 shr CAggPolyBaseShift;
    Ex := ShrInt32(X1, CAggPolyBaseShift);

    TwoFx := (X1 - (Ex shl CAggPolyBaseShift)) shl 1;
    First := CAggPolyBaseSize;

    if Delta.Y < 0 then
    begin
      First := 0;
      Incr := -1;
    end;

    FromX := X1;

    // RenderHorizontalLine(ey1 ,FromX ,fy1 ,FromX ,first );
    DeltaVal := First - Fy1;

    Inc(FCurCell.Cover, DeltaVal);
    Inc(FCurCell.Area, TwoFx * DeltaVal);
    Inc(Ey1, Incr);

    SetCurrentCell(Ex, Ey1);

    DeltaVal := First + First - CAggPolyBaseSize;
    Area := TwoFx * DeltaVal;

    while Ey1 <> Ey2 do
    begin
      // RenderHorizontalLine(ey1 ,FromX ,CAggPolyBaseSize - first ,FromX ,first );
      FCurCell.Cover := DeltaVal;
      FCurCell.Area := Area;

      Inc(Ey1, Incr);

      SetCurrentCell(Ex, Ey1);
    end;

    // RenderHorizontalLine(ey1, FromX, CAggPolyBaseSize - first, FromX, fy2);
    DeltaVal := Fy2 - CAggPolyBaseSize + First;

    Inc(FCurCell.Cover, DeltaVal);
    Inc(FCurCell.Area, TwoFx * DeltaVal);

    Exit;
  end;

  // ok, we have to render several HorizontalLines
  P := (CAggPolyBaseSize - Fy1) * Delta.X;
  First := CAggPolyBaseSize;

  if Delta.Y < 0 then
  begin
    P := Fy1 * Delta.X;
    First := 0;
    Incr := -1;
    Delta.Y := -Delta.Y;
  end;

  DeltaVal := P div Delta.Y;
  DeltaMod := P mod Delta.Y;

  if DeltaMod < 0 then
  begin
    Dec(DeltaVal);
    Inc(DeltaMod, Delta.Y);
  end;

  FromX := X1 + DeltaVal;

  RenderHorizontalLine(Ey1, X1, Fy1, FromX, First);

  Inc(Ey1, Incr);

  // SetCurrentCell(FromX shr CAggPolyBaseShift ,ey1 );
  SetCurrentCell(ShrInt32(FromX, CAggPolyBaseShift), Ey1);

  if Ey1 <> Ey2 then
  begin
    P := CAggPolyBaseSize * Delta.X;
    Lift := P div Delta.Y;
    Rem := P mod Delta.Y;

    if Rem < 0 then
    begin
      Dec(Lift);
      Inc(Rem, Delta.Y);
    end;

    Dec(DeltaMod, Delta.Y);

    while Ey1 <> Ey2 do
    begin
      DeltaVal := Lift;

      Inc(DeltaMod, Rem);

      if DeltaMod >= 0 then
      begin
        Dec(DeltaMod, Delta.Y);
        Inc(DeltaVal);
      end;

      ToX := FromX + DeltaVal;

      RenderHorizontalLine(Ey1, FromX, CAggPolyBaseSize - First, ToX, First);

      FromX := ToX;

      Inc(Ey1, Incr);

      // SetCurrentCell(FromX shr CAggPolyBaseShift ,ey1 );
      SetCurrentCell(ShrInt32(FromX, CAggPolyBaseShift), Ey1);
    end;
  end;

  RenderHorizontalLine(Ey1, FromX, CAggPolyBaseSize - First, X2, Fy2);
end;

procedure TAggOutlineAA.RenderHorizontalLine(Ey, X1, Y1, X2, Y2: Integer);
var
  P, DeltaX, Ex1, Ex2, Fx1, Fx2: Integer;
  Delta, First, Incr, Lift, DeltaMod, Rem: Integer;
begin
  Ex1 := ShrInt32(X1, CAggPolyBaseShift);
  Ex2 := ShrInt32(X2, CAggPolyBaseShift);

  // trivial case. Happens often
  if Y1 = Y2 then
  begin
    SetCurrentCell(Ex2, Ey);

    Exit;
  end;

  Fx1 := X1 and CAggPolyBaseMask;
  Fx2 := X2 and CAggPolyBaseMask;

  // everything is located in a single cell.  That is easy!
  if Ex1 = Ex2 then
  begin
    Delta := Y2 - Y1;

    Inc(FCurCell.Cover, Delta);
    Inc(FCurCell.Area, (Fx1 + Fx2) * Delta);

    Exit;
  end;

  // ok, we'll have to render a run of adjacent cells on the same
  // HorizontalLine...
  P := (CAggPolyBaseSize - Fx1) * (Y2 - Y1);
  First := CAggPolyBaseSize;
  Incr := 1;
  DeltaX := X2 - X1;

  if DeltaX < 0 then
  begin
    P := Fx1 * (Y2 - Y1);
    First := 0;
    Incr := -1;
    DeltaX := -DeltaX;
  end;

  Delta := P div DeltaX;
  DeltaMod := P mod DeltaX;

  if DeltaMod < 0 then
  begin
    Dec(Delta);
    Inc(DeltaMod, DeltaX);
  end;

  Inc(FCurCell.Cover, Delta);
  Inc(FCurCell.Area, (Fx1 + First) * Delta);

  Inc(Ex1, Incr);

  SetCurrentCell(Ex1, Ey);

  Inc(Y1, Delta);

  if Ex1 <> Ex2 then
  begin
    P := CAggPolyBaseSize * (Y2 - Y1 + Delta);
    Lift := P div DeltaX;
    Rem := P mod DeltaX;

    if Rem < 0 then
    begin
      Dec(Lift);
      Inc(Rem, DeltaX);
    end;

    Dec(DeltaMod, DeltaX);

    while Ex1 <> Ex2 do
    begin
      Delta := Lift;

      Inc(DeltaMod, Rem);

      if DeltaMod >= 0 then
      begin
        Dec(DeltaMod, DeltaX);
        Inc(Delta);
      end;

      Inc(FCurCell.Cover, Delta);
      Inc(FCurCell.Area, (CAggPolyBaseSize) * Delta);
      Inc(Y1, Delta);
      Inc(Ex1, Incr);

      SetCurrentCell(Ex1, Ey);
    end;
  end;

  Delta := Y2 - Y1;

  Inc(FCurCell.Cover, Delta);
  Inc(FCurCell.Area, (Fx2 + CAggPolyBaseSize - First) * Delta);
end;

procedure TAggOutlineAA.AllocateBlock;
var
  NewCells: PPAggCellAA;
begin
  if FCurBlock >= FNumBlocks then
  begin
    if FNumBlocks >= FMaxBlocks then
    begin
      AggGetMem(Pointer(NewCells), SizeOf(PAggCellAA) *
        (FMaxBlocks + CAggCellBlockPool));

      if FCells <> nil then
      begin
        Move(FCells^, NewCells^, SizeOf(PAggCellAA) * FMaxBlocks);

        AggFreeMem(Pointer(FCells), SizeOf(PAggCellAA) * FMaxBlocks);
      end;

      FCells := NewCells;

      Inc(FMaxBlocks, CAggCellBlockPool);
    end;

    AggGetMem(Pointer(Pointer(PtrComp(FCells) + FNumBlocks *
      SizeOf(PAggCellAA))^), CAggCellBlockSize * SizeOf(TAggCellAA));

    Inc(FNumBlocks);
  end;

  FCurCellPointer := PPAggCellAA(PtrComp(FCells) + FCurBlock *
    SizeOf(PAggCellAA))^;

  Inc(FCurBlock);
end;


{ TAggScanLineHitTest }

constructor TAggScanLineHitTest.Create;
begin
  FX := X;
  FHit := False;
end;

procedure TAggScanLineHitTest.ResetSpans;
begin
end;

procedure TAggScanLineHitTest.Finalize(Y: Integer);
begin
end;

procedure TAggScanLineHitTest.AddCell(X: Integer; Cover: Cardinal);
begin
  if FX = X then
    FHit := True;
end;

procedure TAggScanLineHitTest.AddSpan(X: Integer; Len, Cover: Cardinal);
begin
  if (FX >= X) and (FX < X + Len) then
    FHit := True;
end;

function TAggScanLineHitTest.GetNumSpans: Cardinal;
begin
  Result := 1;
end;


{ TAggRasterizerScanLineAA }

constructor TAggRasterizerScanLineAA.Create;
var
  I: Integer;
begin
  FOutline := TAggOutlineAA.Create;

  FFillingRule := frNonZero;
  FAutoClose := True;

  FClippedStart := PointInteger(0);
  FStart := PointInteger(0);
  FPrev := PointInteger(0);

  FPrevFlags := 0;
  FStatus := siStatusInitial;
  FClipping := False;

  for I := 0 to CAggAntiAliasingNum - 1 do
    FGamma[I] := I;

  FXScale := 1;
end;

destructor TAggRasterizerScanLineAA.Destroy;
begin
  FOutline.Free;
  inherited;
end;

procedure TAggRasterizerScanLineAA.Reset;
begin
  FOutline.Reset;

  FStatus := siStatusInitial;
end;

procedure TAggRasterizerScanLineAA.SetClipBox(X1, Y1, X2, Y2: Double);
begin
  Reset;

  FClipBox := PolyCoord(RectDouble(X1, Y1, X2, Y2));
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineAA.SetClipBox(Rect: TRectDouble);
begin
  Reset;

  FClipBox := PolyCoord(Rect);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineAA.SetFillingRule(Value: TAggFillingRule);
begin
  FFillingRule := Value;
end;

procedure TAggRasterizerScanLineAA.AutoClose(Flag: Boolean);
begin
  FAutoClose := Flag;
end;

procedure TAggRasterizerScanLineAA.Gamma(AGammaFunction: TAggCustomVertexSource);
var
  I: Integer;
begin
  for I := 0 to CAggAntiAliasingNum - 1 do
    FGamma[I] := Trunc(AGammaFunction.FuncOperatorGamma(
      I / CAggAntiAliasingMask) * CAggAntiAliasingMask + 0.5);
end;

function TAggRasterizerScanLineAA.ApplyGamma(Cover: Cardinal): Cardinal;
begin
  Result := FGamma[Cover];
end;

procedure TAggRasterizerScanLineAA.MoveToNoClip(X, Y: Integer);
begin
  if (FStatus = siStatusLineTo) and FAutoClose then
    ClosePolygonNoClip;

  FOutline.MoveTo(X * FXScale, Y);

  FClippedStart := PointInteger(X, Y);

  FStatus := siStatusLineTo;
end;

procedure TAggRasterizerScanLineAA.MoveToNoClip(Point: TPointInteger);
begin
  if (FStatus = siStatusLineTo) and FAutoClose then
    ClosePolygonNoClip;

  FOutline.MoveTo(Point.X * FXScale, Point.Y);

  FClippedStart := Point;

  FStatus := siStatusLineTo;
end;

procedure TAggRasterizerScanLineAA.LineToNoClip(X, Y: Integer);
begin
  if FStatus <> siStatusInitial then
  begin
    FOutline.LineTo(X * FXScale, Y);

    FStatus := siStatusLineTo;
  end;
end;

procedure TAggRasterizerScanLineAA.LineToNoClip(Point: TPointInteger);
begin
  if FStatus <> siStatusInitial then
  begin
    FOutline.LineTo(Point.X * FXScale, Point.Y);

    FStatus := siStatusLineTo;
  end;
end;

procedure TAggRasterizerScanLineAA.ClosePolygon;
begin
  if FClipping then
    ClipSegment(FStart);

  if FAutoClose then
    ClosePolygonNoClip;
end;

procedure TAggRasterizerScanLineAA.ClosePolygonNoClip;
begin
  if FStatus = siStatusLineTo then
  begin
    FOutline.LineTo(FClippedStart.X * FXScale, FClippedStart.Y);

    FStatus := siStatusClosed;
  end;
end;

procedure TAggRasterizerScanLineAA.ClipSegment(X, Y: Integer);
var
  Flags, N: Cardinal;

  Center: array [0..3] of TPointInteger;
  Pnt: PPointInteger;
begin
  Flags := ClippingFlagsInteger(X, Y, FClipBox);

  if FPrevFlags = Flags then
    if Flags = 0 then
      if FStatus = siStatusInitial then
        MoveToNoClip(X, Y)
      else
        LineToNoClip(X, Y)
    else
  else
  begin
    N := ClipLiangBarskyInteger(FPrev.X, FPrev.Y, X, Y, FClipBox, @Center[0]);

    Pnt := @Center[0];

    while N > 0 do
    begin
      if FStatus = siStatusInitial then
        MoveToNoClip(Pnt^)
      else
        LineToNoClip(Pnt^);

      Inc(Pnt);
      Dec(N);
    end;
  end;

  FPrevFlags := Flags;
  FPrev := PointInteger(X, Y);
end;

procedure TAggRasterizerScanLineAA.ClipSegment(Point: TPointInteger);
var
  Flags, N: Cardinal;

  Center: array [0..3] of TPointInteger;
  Pnt: PPointInteger;
begin
  Flags := ClippingFlagsInteger(Point.X, Point.Y, FClipBox);

  if FPrevFlags = Flags then
    if Flags = 0 then
      if FStatus = siStatusInitial then
        MoveToNoClip(Point)
      else
        LineToNoClip(Point)
    else
  else
  begin
    N := ClipLiangBarskyInteger(FPrev.X, FPrev.Y, Point.X, Point.Y, FClipBox,
      @Center[0]);

    Pnt := @Center[0].X;

    while N > 0 do
    begin
      if FStatus = siStatusInitial then
        MoveToNoClip(Pnt^)
      else
        LineToNoClip(Pnt^);

      Inc(Pnt);
      Dec(N);
    end;
  end;

  FPrevFlags := Flags;
  FPrev := Point;
end;

procedure TAggRasterizerScanLineAA.MoveToDouble(X, Y: Double);
begin
  MoveTo(PolyCoord(X), PolyCoord(Y));
end;

procedure TAggRasterizerScanLineAA.LineToDouble(X, Y: Double);
begin
  LineTo(PolyCoord(X), PolyCoord(Y));
end;

procedure TAggRasterizerScanLineAA.MoveToDouble(Point: TPointDouble);
begin
  MoveTo(PolyCoord(Point));
end;

procedure TAggRasterizerScanLineAA.LineToDouble(Point: TPointDouble);
begin
  LineTo(PolyCoord(Point));
end;

procedure TAggRasterizerScanLineAA.MoveTo(Point: TPointInteger);
begin
  if FClipping then
  begin
    if FOutline.Sorted then
      Reset;

    if (FStatus = siStatusLineTo) and FAutoClose then
      ClosePolygon;

    FPrev := Point;
    FStart := Point;
    FStatus := siStatusInitial;

    FPrevFlags := ClippingFlagsInteger(Point.X, Point.Y, FClipBox);

    if FPrevFlags = 0 then
      MoveToNoClip(Point);
  end
  else
    MoveToNoClip(Point);
end;

procedure TAggRasterizerScanLineAA.MoveTo(X, Y: Integer);
begin
  if FClipping then
  begin
    if FOutline.Sorted then
      Reset;

    if (FStatus = siStatusLineTo) and FAutoClose then
      ClosePolygon;

    FPrev := PointInteger(X, Y);
    FStart := PointInteger(X, Y);
    FStatus := siStatusInitial;

    FPrevFlags := ClippingFlagsInteger(X, Y, FClipBox);

    if FPrevFlags = 0 then
      MoveToNoClip(X, Y);
  end
  else
    MoveToNoClip(X, Y);
end;

procedure TAggRasterizerScanLineAA.LineTo(Point: TPointInteger);
begin
  if FClipping then
    ClipSegment(Point)
  else
    LineToNoClip(Point);
end;

procedure TAggRasterizerScanLineAA.LineTo(X, Y: Integer);
begin
  if FClipping then
    ClipSegment(X, Y)
  else
    LineToNoClip(X, Y);
end;

procedure TAggRasterizerScanLineAA.Sort;
begin
  FOutline.SortCells;
end;

function TAggRasterizerScanLineAA.RewindScanLines: Boolean;
begin
  if FAutoClose then
    ClosePolygon;

  FOutline.SortCells;

  if FOutline.TotalCells = 0 then
  begin
    Result := False;

    Exit;
  end;

  FCurY := FOutline.MinY;
  Result := True;
end;

function TAggRasterizerScanLineAA.SweepScanLine(Sl: TAggCustomScanLine): Boolean;
var
  X, Area: Integer;
  Cover: Integer;
  Alpha: Cardinal;
  Cells: PPAggCellAA;

  CurCell: PAggCellAA;
  NumCells: Cardinal;
begin
  repeat
    if FCurY > FOutline.MaxY then
    begin
      Result := False;

      Exit;
    end;

    Sl.ResetSpans;

    NumCells := FOutline.ScanLineNumCells(FCurY);
    Cells := FOutline.ScanLineCells(FCurY);

    Cover := 0;

    while NumCells > 0 do
    begin
      CurCell := Cells^;

      X := CurCell.X;
      Area := CurCell.Area;

      Inc(Cover, CurCell.Cover);

      // accumulate all cells with the same X
      Dec(NumCells);

      while NumCells > 0 do
      begin
        Inc(Cells);

        CurCell := Cells^;

        if CurCell.X <> X then
          Break;

        Inc(Area, CurCell.Area);
        Inc(Cover, CurCell.Cover);

        Dec(NumCells);
      end;

      if Area <> 0 then
      begin
        Alpha := CalculateAlpha((Cover shl (CAggPolyBaseShift + 1)) - Area);

        if Alpha <> 0 then
          Sl.AddCell(X, Alpha);

        Inc(X);
      end;

      if (NumCells <> 0) and (CurCell.X > X) then
      begin
        Alpha := CalculateAlpha(Cover shl (CAggPolyBaseShift + 1));

        if Alpha <> 0 then
          Sl.AddSpan(X, CurCell.X - X, Alpha);
      end;
    end;

    if Boolean(Sl.NumSpans) then
      Break;

    Inc(FCurY);
  until False;

  Sl.Finalize(FCurY);

  Inc(FCurY);

  Result := True;
end;

function TAggRasterizerScanLineAA.NavigateScanLine(Y: Integer): Boolean;
begin
  if FAutoClose then
    ClosePolygon;

  FOutline.SortCells;

  if (FOutline.TotalCells = 0) or (Y < FOutline.MinY) or
    (Y > FOutline.MaxY) then
  begin
    Result := False;

    Exit;
  end;

  FCurY := Y;
  Result := True;
end;

function TAggRasterizerScanLineAA.HitTest(Tx, Ty: Integer): Boolean;
var
  Sl: TAggScanLineHitTest;
begin
  if not NavigateScanLine(Ty) then
  begin
    Result := False;

    Exit;
  end;

  Sl := TAggScanLineHitTest.Create(Tx);
  try
    SweepScanLine(Sl);

    Result := Sl.Hit;
  finally
    Sl.Free
  end;
end;

function TAggRasterizerScanLineAA.GetMinX;
begin
  Result := FOutline.MinX;
end;

function TAggRasterizerScanLineAA.GetMinY;
begin
  Result := FOutline.MinY;
end;

function TAggRasterizerScanLineAA.GetFillingRule: TAggFillingRule;
begin
  Result := FFillingRule
end;

function TAggRasterizerScanLineAA.GetMaxX;
begin
  Result := FOutline.MaxX;
end;

function TAggRasterizerScanLineAA.GetMaxY;
begin
  Result := FOutline.MaxY;
end;

function TAggRasterizerScanLineAA.CalculateAlpha(Area: Integer): Cardinal;
var
  Cover: System.Integer;
begin
  // 1: cover:=area shr (CAggPolyBaseShift * 2 + 1 - CAggAntiAliasingShift );
  // 2: cover:=round(area / (1 shl (CAggPolyBaseShift * 2 + 1 - CAggAntiAliasingShift ) ) );
  Cover := ShrInt32(Area, CAggPolyBaseShift shl 1 + 1 - CAggAntiAliasingShift);

  if Cover < 0 then
    Cover := -Cover;

  if FFillingRule = frEvenOdd then
  begin
    Cover := Cover and CAggAntiAliasing2Mask;

    if Cover > CAggAntiAliasingNum then
      Cover := CAggAntiAliasing2Num - Cover;
  end;

  if Cover > CAggAntiAliasingMask then
    Cover := CAggAntiAliasingMask;

  Result := FGamma[Cover];
end;

procedure TAggRasterizerScanLineAA.AddPath;
var
  Cmd : Cardinal;
  X, Y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    AddVertex(X, Y, Cmd);

    Cmd := Vs.Vertex(@X, @Y);
  end;
end;

procedure TAggRasterizerScanLineAA.AddVertex;
begin
  if IsClose(Cmd) then
    ClosePolygon
  else if IsMoveTo(Cmd) then
    MoveTo(PolyCoord(X), PolyCoord(Y))
  else if IsVertex(Cmd) then
    LineTo(PolyCoord(X), PolyCoord(Y));
end;

end.
