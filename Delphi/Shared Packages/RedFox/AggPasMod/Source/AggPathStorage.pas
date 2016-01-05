unit AggPathStorage;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012-2015                                                      //
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

uses
  AggBasics,
  AggMath,
  AggBezierArc,
  AggVertexSource,
  AggTransAffine;


// Allocation parameters
const
  CAggBlockShift = 8;
  CAggBlockSize = 1 shl CAggBlockShift;
  CAggBlockMask = CAggBlockSize - 1;
  CAggBlockPool = 256;

type
  TAggPathStorage = class;

  TAggPathStorageVertexSource = class(TAggVertexSource)
  private
    FPath: TAggPathStorage;

    FVertexIndex: Cardinal;
  public
    constructor Create; overload;
    constructor Create(P: TAggPathStorage); overload;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  // A container to store vertices with their flags.
  // A path consists of a number of contours separated with "MoveTo"
  // commands. The path storage can keep and maintain more than one
  // path.
  // To navigate to the beginning of a particular path, use rewind(PathID);
  // Where PathID is what StartNewPath() returns. So, when you call
  // StartNewPath() you need to store its return value somewhere else
  // to navigate to the path afterwards.
  TAggPathStorage = class(TAggVertexSource)
  private
    FTotalVertices, FTotalBlocks, FMaxBlocks: Cardinal;

    FCoordBlocks: PPDouble;
    FCmdBlocks: PPInt8u;

    FIterator: Cardinal;

    // Private
    procedure AllocateBlock(Nb: Cardinal);
    function StoragePtrs(Xy_ptr: PPDouble): PInt8u;

    function PerceivePolygonOrientation(Start, Stop: Cardinal): Cardinal;

    // Allows you to modify vertex command. The caller must know
    // the index of the vertex.
    function GetCommand(Index: Cardinal): Cardinal;
    procedure SetCommand(Index, Cmd: Cardinal);
  public
    constructor Create; overload;
    constructor Create(Ps: TAggPathStorage); overload;
    destructor Destroy; override;

    procedure RemoveAll; override;

    function LastVertex(X, Y: PDouble): Cardinal;
    function PrevVertex(X, Y: PDouble): Cardinal;

    function LastX: Double;
    function LastY: Double;

    procedure RelativeToAbsolute(X, Y: PDouble);

    procedure MoveTo(X, Y: Double);
    procedure MoveRelative(Dx, Dy: Double);

    procedure LineTo(X, Y: Double);
    procedure LineRelative(Dx, Dy: Double);

    procedure HorizontalLineTo(X: Double);
    procedure HorizontalLineRelative(Dx: Double);

    procedure VerticalLineTo(Y: Double);
    procedure VerticalLineRelative(Dy: Double);

    procedure ArcTo(Rx, Ry, Angle: Double; LargeArcFlag, SweepFlag: Boolean;
      X, Y: Double); overload;
    procedure ArcRelative(Rx, Ry, Angle: Double;
      LargeArcFlag, SweepFlag: Boolean; Dx, Dy: Double); overload;

    procedure Curve3(ControlX, ControlY, ToX, ToY: Double); overload;
    procedure Curve3Relative(DeltaControlX, DeltaControlY, DeltaToX, DeltaToY: Double); overload;

    procedure Curve3(ToX, ToY: Double); overload;
    procedure Curve3Relative(DeltaToX, DeltaToY: Double); overload;

    procedure Curve4(Control1X, Control1Y, Control2X, Control2Y, ToX,
      ToY: Double); overload;
    procedure Curve4Relative(DeltaControl1X, DeltaControl1Y, DeltaControl2X,
      DeltaControl2Y, DeltaToX, DeltaToY: Double); overload;

    procedure Curve4(Control2X, Control2Y, ToX, ToY: Double); overload;
    procedure Curve4Relative(DeltaControl2X, DeltaControl2Y,
      DeltaToX, DeltaToY: Double); overload;

    procedure EndPoly(Flags: Cardinal = CAggPathFlagsClose);
    procedure ClosePolygon(Flags: Cardinal = CAggPathFlagsNone);

    procedure AddPoly(Vertices: PPointDouble; Num: Cardinal;
      SolidPath: Boolean = False; EndFlags: Cardinal = CAggPathFlagsNone);
    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0;
      SolidPath: Boolean = True);

    function StartNewPath: Cardinal;

    procedure CopyFrom(Ps: TAggPathStorage);

    function SetVertex(Index: Cardinal; X, Y: PDouble): Cardinal;
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    // Arrange the orientation of a polygon, all polygons in a path,
    // or in all paths. After calling arrangeOrientations() or
    // ArrangeOrientationsAllPaths(), all the polygons will have
    // the same orientation, i.e. CAggPathFlagsCw or CAggPathFlagsCcw
    function ArrangePolygonOrientation(Start, Orientation: Cardinal)
      : Cardinal;
    function ArrangeOrientations(Start, Orientation: Cardinal): Cardinal;
    procedure ArrangeOrientationsAllPaths(Orientation: Cardinal);

    // Flip all the vertices horizontally or vertically
    procedure FlipX(X1, X2: Double);
    procedure FlipY(Y1, Y2: Double);

    // This function adds a vertex with its flags directly. Since there's no
    // checking for errors, keeping proper path integrity is the responsibility
    // of the caller. It can be said the function is "not very public".
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Allows you to modify vertex coordinates. The caller must know
    // the index of the vertex.
    procedure ModifyVertex(Index: Cardinal; X, Y: Double);

    // Path Affine Transformations
    procedure Transform(Trans: TAggTransAffine; PathID: Cardinal = 0);
    procedure TransformAllPaths(Trans: TAggTransAffine);

    // from 2.4
    procedure ConcatPath(Vs: TAggVertexSource; PathID: Cardinal = 0);

    procedure InvertPolygon(Start, Stop: Cardinal); overload;
    procedure InvertPolygon(Start: Cardinal); overload;

    property TotalVertices: Cardinal read FTotalVertices;
    property Command[Index: Cardinal]: Cardinal read GetCommand write SetCommand;
  end;

implementation


{ TAggPathStorageVertexSource }

constructor TAggPathStorageVertexSource.Create;
begin
  FPath := nil;

  FVertexIndex := 0;
end;

constructor TAggPathStorageVertexSource.Create(P: TAggPathStorage);
begin
  FPath := P;

  FVertexIndex := 0;
end;

procedure TAggPathStorageVertexSource.Rewind(PathID: Cardinal);
begin
  FVertexIndex := PathID;
end;

function TAggPathStorageVertexSource.Vertex(X, Y: PDouble): Cardinal;
begin
  if FVertexIndex < FPath.TotalVertices then
  begin
    Result := FPath.SetVertex(FVertexIndex, X, Y);

    Inc(FVertexIndex);
  end
  else
    Result := CAggPathCmdStop;
end;


{ TAggPathStorage }

constructor TAggPathStorage.Create;
begin
  FTotalVertices := 0;
  FTotalBlocks := 0;
  FMaxBlocks := 0;

  FCoordBlocks := nil;
  FCmdBlocks := nil;

  FIterator := 0;
end;

constructor TAggPathStorage.Create(Ps: TAggPathStorage);
begin
  FTotalVertices := 0;
  FTotalBlocks := 0;
  FMaxBlocks := 0;

  FCoordBlocks := nil;
  FCmdBlocks := nil;

  FIterator := 0;

  CopyFrom(Ps);
end;

destructor TAggPathStorage.Destroy;
var
  CoordBulk: PPDouble;
  DataSize: Cardinal;
begin
  if FTotalBlocks <> 0 then
  begin
    CoordBulk := PPDouble(PtrComp(FCoordBlocks) + (FTotalBlocks - 1) *
      SizeOf(PDouble));

    while FTotalBlocks > 0 do
    begin
      DataSize := (CAggBlockSize * 2 +
        CAggBlockSize div (SizeOf(Double) div SizeOf(Int8u))) * SizeOf(Double);
      AggFreeMem(TPointer32(CoordBulk^).Ptr, DataSize);

      Dec(PtrComp(CoordBulk), SizeOf(PDouble));
      Dec(FTotalBlocks);
    end;

    AggFreeMem(Pointer(FCoordBlocks), FMaxBlocks * 2 * SizeOf(PDouble));
  end;

  inherited;
end;

procedure TAggPathStorage.RemoveAll;
begin
  FTotalVertices := 0;
  FIterator := 0;
end;

function TAggPathStorage.LastVertex;
begin
  if FTotalVertices <> 0 then
    Result := SetVertex(FTotalVertices - 1, X, Y)
  else
    Result := CAggPathCmdStop;
end;

function TAggPathStorage.PrevVertex;
begin
  if FTotalVertices > 1 then
    Result := SetVertex(FTotalVertices - 2, X, Y)
  else
    Result := CAggPathCmdStop;
end;

function TAggPathStorage.LastX;
var
  Index: Cardinal;
begin
  if FTotalVertices <> 0 then
  begin
    Index := FTotalVertices - 1;

    Result := PDouble
      (PtrComp(PPointer32(PtrComp(FCoordBlocks) + (Index shr CAggBlockShift) *
      SizeOf(PDouble)).Ptr) + ((Index and CAggBlockMask) shl 1) *
      SizeOf(Double))^;
  end
  else
    Result := 0.0;
end;

function TAggPathStorage.LastY;
var
  Index: Cardinal;
begin
  if FTotalVertices <> 0 then
  begin
    Index := FTotalVertices - 1;

    Result := PDouble
      (PtrComp(PPointer32(PtrComp(FCoordBlocks) + (Index shr CAggBlockShift) *
      SizeOf(PDouble)).Ptr) + (((Index and CAggBlockMask) shl 1) + 1) *
      SizeOf(Double))^;
  end
  else
    Result := 0.0;
end;

procedure TAggPathStorage.RelativeToAbsolute;
var
  X2, Y2: Double;
begin
  if FTotalVertices <> 0 then
    if IsVertex(SetVertex(FTotalVertices - 1, @X2, @Y2)) then
    begin
      X^ := X^ + X2;
      Y^ := Y^ + Y2;
    end;
end;

procedure TAggPathStorage.MoveTo;
begin
  AddVertex(X, Y, CAggPathCmdMoveTo);
end;

procedure TAggPathStorage.MoveRelative;
begin
  RelativeToAbsolute(@Dx, @Dy);
  AddVertex(Dx, Dy, CAggPathCmdMoveTo);
end;

procedure TAggPathStorage.LineTo;
begin
  AddVertex(X, Y, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.LineRelative;
begin
  RelativeToAbsolute(@Dx, @Dy);
  AddVertex(Dx, Dy, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.HorizontalLineTo;
begin
  AddVertex(X, LastY, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.HorizontalLineRelative;
var
  Dy: Double;
begin
  Dy := 0;

  RelativeToAbsolute(@Dx, @Dy);
  AddVertex(Dx, Dy, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.VerticalLineTo;
begin
  AddVertex(LastX, Y, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.VerticalLineRelative;
var
  Dx: Double;
begin
  Dx := 0;

  RelativeToAbsolute(@Dx, @Dy);
  AddVertex(Dx, Dy, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.ArcTo(Rx, Ry, Angle: Double; LargeArcFlag,
  SweepFlag: Boolean; X, Y: Double);
var
  A: TAggBezierArcSvg;
  X0, Y0, Epsilon: Double;
begin
  A := nil;

  if (FTotalVertices <> 0) and IsVertex(Command[FTotalVertices - 1]) then
  begin
    Epsilon := 1E-30;

    X0 := 0.0;
    Y0 := 0.0;

    LastVertex(@X0, @Y0);

    Rx := Abs(Rx);
    Ry := Abs(Ry);

    // Ensure radii are valid
    if (Rx < Epsilon) or (Ry < Epsilon) then
    begin
      LineTo(X, Y);
      Exit;
    end;

    // If the endpoints (x, y) and (x0, y0) are identical, then this
    // is equivalent to omitting the elliptical arc segment entirely.
    if CalculateDistance(X0, Y0, X, Y) < Epsilon then
      Exit;

    A := TAggBezierArcSvg.Create(X0, Y0, Rx, Ry, Angle, LargeArcFlag,
      SweepFlag, X, Y);

    if A.RadiiOK then
      AddPath(A, 0, True)
    else
      LineTo(X, Y);
  end
  else
    MoveTo(X, Y);

  if A <> nil then
    A.Free;
end;

procedure TAggPathStorage.ArcRelative(Rx, Ry, Angle: Double;
  LargeArcFlag, SweepFlag: Boolean; Dx, Dy: Double);
begin
  RelativeToAbsolute(@Dx, @Dy);
  ArcTo(Rx, Ry, Angle, LargeArcFlag, SweepFlag, Dx, Dy);
end;

procedure TAggPathStorage.Curve3(ControlX, ControlY, ToX, ToY: Double);
begin
  AddVertex(ControlX, ControlY, CAggPathCmdCurve3);
  AddVertex(ToX, ToY, CAggPathCmdCurve3);
end;

procedure TAggPathStorage.Curve3Relative(DeltaControlX, DeltaControlY,
  DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaControlX, @DeltaControlY);
  RelativeToAbsolute(@DeltaToX, @DeltaToY);
  AddVertex(DeltaControlX, DeltaControlY, CAggPathCmdCurve3);
  AddVertex(DeltaToX, DeltaToY, CAggPathCmdCurve3);
end;

procedure TAggPathStorage.Curve3(ToX, ToY: Double);
var
  Cmd: Cardinal;
  X0, Y0, ControlX, ControlY: Double;
begin
  if IsVertex(LastVertex(@X0, @Y0)) then
  begin
    Cmd := PrevVertex(@ControlX, @ControlY);

    if IsCurve(Cmd) then
    begin
      ControlX := X0 + X0 - ControlX;
      ControlY := Y0 + Y0 - ControlY;
    end
    else
    begin
      ControlX := X0;
      ControlY := Y0;
    end;

    Curve3(ControlX, ControlY, ToX, ToY);
  end;
end;

procedure TAggPathStorage.Curve3Relative(DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaToX, @DeltaToY);
  Curve3(DeltaToX, DeltaToY);
end;

procedure TAggPathStorage.Curve4(Control1X, Control1Y, Control2X, Control2Y,
  ToX, ToY: Double);
begin
  AddVertex(Control1X, Control1Y, CAggPathCmdCurve4);
  AddVertex(Control2X, Control2Y, CAggPathCmdCurve4);
  AddVertex(ToX, ToY, CAggPathCmdCurve4);
end;

procedure TAggPathStorage.Curve4Relative(DeltaControl1X, DeltaControl1Y,
  DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaControl1X, @DeltaControl1Y);
  RelativeToAbsolute(@DeltaControl2X, @DeltaControl2Y);
  RelativeToAbsolute(@DeltaToX, @DeltaToY);
  AddVertex(DeltaControl1X, DeltaControl1Y, CAggPathCmdCurve4);
  AddVertex(DeltaControl2X, DeltaControl2Y, CAggPathCmdCurve4);
  AddVertex(DeltaToX, DeltaToY, CAggPathCmdCurve4);
end;

procedure TAggPathStorage.Curve4(Control2X, Control2Y, ToX, ToY: Double);
var
  Cmd: Cardinal;
  X0, Y0, Control1X, Control1Y: Double;
begin
  if IsVertex(LastVertex(@X0, @Y0)) then
  begin
    Cmd := PrevVertex(@Control1X, @Control1Y);

    if IsCurve(Cmd) then
    begin
      Control1X := X0 + X0 - Control1X;
      Control1Y := Y0 + Y0 - Control1Y;
    end
    else
    begin
      Control1X := X0;
      Control1Y := Y0;
    end;

    Curve4(Control1X, Control1Y, Control2X, Control2Y, ToX, ToY);
  end;
end;

procedure TAggPathStorage.Curve4Relative(DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaControl2X, @DeltaControl2Y);
  RelativeToAbsolute(@DeltaToX, @DeltaToY);

  Curve4(DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY);
end;

procedure TAggPathStorage.EndPoly(Flags: Cardinal = CAggPathFlagsClose);
begin
  if FTotalVertices <> 0 then
    if IsVertex(Command[FTotalVertices - 1]) then
      AddVertex(0.0, 0.0, CAggPathCmdEndPoly or Flags);
end;

procedure TAggPathStorage.ClosePolygon(Flags: Cardinal = CAggPathFlagsNone);
begin
  EndPoly(CAggPathFlagsClose or Flags);
end;

procedure TAggPathStorage.AddPoly(Vertices: PPointDouble; Num: Cardinal;
  SolidPath: Boolean = False; EndFlags: Cardinal = CAggPathFlagsNone);
begin
  if Num <> 0 then
  begin
    if not SolidPath then
    begin
      MoveTo(Vertices.X, Vertices.Y);

      Inc(PtrComp(Vertices), 2 * SizeOf(Double));
      Dec(Num);
    end;

    while Num > 0 do
    begin
      LineTo(Vertices.X, Vertices.Y);

      Inc(PtrComp(Vertices), 2 * SizeOf(Double));
      Dec(Num);
    end;

    if EndFlags <> 0 then
      EndPoly(EndFlags);
  end;
end;

procedure TAggPathStorage.AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0;
  SolidPath: Boolean = True);
var
  Cmd: Cardinal;
  X, Y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsMoveTo(Cmd) and SolidPath and (FTotalVertices <> 0) then
      Cmd := CAggPathCmdLineTo;

    AddVertex(X, Y, Cmd);

    Cmd := Vs.Vertex(@X, @Y);
  end;
end;

function TAggPathStorage.StartNewPath: Cardinal;
begin
  if FTotalVertices <> 0 then
    if not IsStop(Command[FTotalVertices - 1]) then
      AddVertex(0.0, 0.0, CAggPathCmdStop);

  Result := FTotalVertices;
end;

procedure TAggPathStorage.CopyFrom(Ps: TAggPathStorage);
var
  I, Cmd: Cardinal;
  X, Y  : Double;
begin
  RemoveAll;

  for I := 0 to Ps.TotalVertices - 1 do
  begin
    Cmd := Ps.SetVertex(I, @X, @Y);

    AddVertex(X, Y, Cmd);
  end;
end;

function TAggPathStorage.SetVertex(Index: Cardinal; X, Y: PDouble): Cardinal;
var
  Nb: Cardinal;
  Pv: PDouble;
begin
  Nb := Index shr CAggBlockShift;

  Pv := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + Nb *
    SizeOf(PDouble)).Ptr) + ((Index and CAggBlockMask) shl 1) * SizeOf(Double));

  X^ := Pv^;
  Inc(PtrComp(Pv), SizeOf(Double));
  Y^ := Pv^;

  Result := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + Nb *
    SizeOf(PInt8u)).Ptr) + (Index and CAggBlockMask) * SizeOf(Int8u))^;
end;

function TAggPathStorage.GetCommand(Index: Cardinal): Cardinal;
begin
  Result := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) +
    (Index shr CAggBlockShift) * SizeOf(PInt8u)).Ptr) + (Index and CAggBlockMask) *
    SizeOf(Int8u))^;
end;

procedure TAggPathStorage.Rewind(PathID: Cardinal);
begin
  FIterator := PathID;
end;

function TAggPathStorage.Vertex(X, Y: PDouble): Cardinal;
begin
  if FIterator >= FTotalVertices then
    Result := CAggPathCmdStop
  else
  begin
    Result := SetVertex(FIterator, X, Y);

    Inc(FIterator);
  end;
end;

function TAggPathStorage.ArrangePolygonOrientation(Start, Orientation: Cardinal)
      : Cardinal;
var
  Cmd, Stop: Cardinal;
begin
  if Orientation = CAggPathFlagsNone then
  begin
    Result := Start;

    Exit;
  end;

  // Skip all non-vertices at the beginning
  while (Start < FTotalVertices) and not IsVertex(Command[Start]) do
    Inc(Start);

  // Skip all insignificant MoveTo
  while (Start + 1 < FTotalVertices) and IsMoveTo(Command[Start]) and
    IsMoveTo(Command[Start + 1]) do
    Inc(Start);

  // Find the last vertex
  Stop := Start + 1;

  while (Stop < FTotalVertices) and not IsNextPoly(Command[Stop]) do
    Inc(Stop);

  if Stop - Start > 2 then
    if PerceivePolygonOrientation(Start, Stop) <> Orientation then
    begin
      // Invert polygon, set orientation flag, and skip all end_poly
      InvertPolygon(Start, Stop);

      Cmd := Command[Stop];

      while (Stop < FTotalVertices) and IsEndPoly(Cmd) do
      begin
        Command[Stop] := SetOrientation(Cmd, Orientation);

        Inc(Stop);

        Cmd := Command[Stop];
      end;
    end;

  Result := Stop;
end;

function TAggPathStorage.ArrangeOrientations(Start, Orientation: Cardinal):
  Cardinal;
begin
  if Orientation <> CAggPathFlagsNone then
    while Start < FTotalVertices do
    begin
      Start := ArrangePolygonOrientation(Start, Orientation);

      if IsStop(Command[Start]) then
      begin
        Inc(Start);

        Break;
      end;
    end;

  Result := Start;
end;

procedure TAggPathStorage.ArrangeOrientationsAllPaths(Orientation: Cardinal);
var
  Start: Cardinal;
begin
  if Orientation <> CAggPathFlagsNone then
  begin
    Start := 0;

    while Start < FTotalVertices do
      Start := ArrangeOrientations(Start, Orientation);
  end;
end;

procedure TAggPathStorage.FlipX(X1, X2: Double);
var
  I, Cmd: Cardinal;
  X, Y  : Double;
begin
  if FTotalVertices > 0 then
    for I := 0 to FTotalVertices - 1 do
    begin
      Cmd := SetVertex(I, @X, @Y);

      if IsVertex(Cmd) then
        ModifyVertex(I, X2 - X + X1, Y);
    end;
end;

procedure TAggPathStorage.FlipY(Y1, Y2: Double);
var
  I, Cmd: Cardinal;
  X, Y  : Double;
begin
  if FTotalVertices > 0 then
    for I := 0 to FTotalVertices - 1 do
    begin
      Cmd := SetVertex(I, @X, @Y);

      if IsVertex(Cmd) then
        ModifyVertex(I, X, Y2 - Y + Y1);
    end;
end;

procedure TAggPathStorage.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  CoordPointer: PDouble;
  CmdPointer  : PInt8u;
begin
  CoordPointer := nil;

  CmdPointer := StoragePtrs(@CoordPointer);

  CmdPointer^ := Int8u(Cmd);

  CoordPointer^ := X;
  Inc(PtrComp(CoordPointer), SizeOf(Double));
  CoordPointer^ := Y;

  Inc(FTotalVertices);
end;

procedure TAggPathStorage.ModifyVertex(Index: Cardinal; X, Y: Double);
var
  Pv: PDouble;
begin
  Pv := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) +
    (Index shr CAggBlockShift) * SizeOf(PDouble)).Ptr) +
    ((Index and CAggBlockMask) shl 1) * SizeOf(Double));

  Pv^ := X;
  Inc(PtrComp(Pv), SizeOf(Double));
  Pv^ := Y;
end;

procedure TAggPathStorage.SetCommand(Index, Cmd: Cardinal);
begin
  PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + (Index shr CAggBlockShift) *
    SizeOf(PInt8u)).Ptr) + (Index and CAggBlockMask) * SizeOf(Int8u))^ :=
    Int8u(Cmd);
end;

procedure TAggPathStorage.Transform(Trans: TAggTransAffine;
  PathID: Cardinal = 0);
var
  X, Y: Double;
  Cmd : Cardinal;
begin
  while PathID < FTotalVertices do
  begin
    Cmd := SetVertex(PathID, @X, @Y);

    if IsStop(Cmd) then
      Break;

    if IsVertex(Cmd) then
    begin
      Trans.Transform(Trans, @X, @Y);
      ModifyVertex(PathID, X, Y);
    end;

    Inc(PathID);
  end;
end;

procedure TAggPathStorage.TransformAllPaths(Trans: TAggTransAffine);
var
  X, Y: Double;
  Index : Cardinal;
begin
  Index := 0;

  while Index < FTotalVertices do
  begin
    if IsVertex(SetVertex(Index, @X, @Y)) then
    begin
      Trans.Transform(Trans, @X, @Y);
      ModifyVertex(Index, X, Y);
    end;

    Inc(Index);
  end;
end;

procedure TAggPathStorage.AllocateBlock(Nb: Cardinal);
var
  NewCoords: PPDouble;
  NewCmds  : PPInt8u;
  NewSize  : Cardinal;
  Ptr      : PPDouble;
begin
  if Nb >= FMaxBlocks then
  begin
    AggGetMem(Pointer(NewCoords), 2 * (FMaxBlocks + CAggBlockPool) *
      SizeOf(PDouble));

    NewCmds := PPInt8u(PtrComp(NewCoords) + (FMaxBlocks + CAggBlockPool)
      * SizeOf(PDouble));

    if FCoordBlocks <> nil then
    begin
      Move(FCoordBlocks^, NewCoords^, FMaxBlocks * SizeOf(PDouble));
      Move(FCmdBlocks^, NewCmds^, FMaxBlocks * SizeOf(PInt8u));

      AggFreeMem(Pointer(FCoordBlocks), 2 * FMaxBlocks * SizeOf(PDouble));
    end;

    FCoordBlocks := NewCoords;
    FCmdBlocks := NewCmds;

    Inc(FMaxBlocks, CAggBlockPool);
  end;

  NewSize := (CAggBlockSize * 2 + CAggBlockSize div
    (SizeOf(Double) div SizeOf(Int8u))) * SizeOf(Double);
  Ptr := FCoordBlocks;
  Inc(Ptr, Nb);
  AggGetMem(Pointer(Ptr^), NewSize);

  PPointer32(PtrComp(FCmdBlocks) + Nb * SizeOf(PInt8u)).Ptr :=
    Pointer(PtrComp(Ptr^) + 2 * CAggBlockSize * SizeOf(Double));

  Inc(FTotalBlocks);
end;

function TAggPathStorage.StoragePtrs(Xy_ptr: PPDouble): PInt8u;
var
  NumBlocks: Cardinal;
begin
  NumBlocks := FTotalVertices shr CAggBlockShift;

  if NumBlocks >= FTotalBlocks then
    AllocateBlock(NumBlocks);

  Xy_ptr^ := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + NumBlocks *
    SizeOf(PDouble)).Ptr) + ((FTotalVertices and CAggBlockMask) shl 1) *
    SizeOf(Double));

  Result := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + NumBlocks *
    SizeOf(PInt8u)).Ptr) + (FTotalVertices and CAggBlockMask) *
    SizeOf(Int8u));
end;

function TAggPathStorage.PerceivePolygonOrientation;
var
  Np, I: Cardinal;
  Area, X1, Y1, X2, Y2: Double;
begin
  // Calculate signed area (double area to be exact)
  Np := Stop - Start;
  Area := 0.0;

  if Np > 0 then
    for I := 0 to Np - 1 do
    begin
      SetVertex(Start + I, @X1, @Y1);
      SetVertex(Start + (I + 1) mod Np, @X2, @Y2);

      Area := Area + (X1 * Y2 - Y1 * X2);
    end;

  if Area < 0.0 then
    Result := CAggPathFlagsCw
  else
    Result := CAggPathFlagsCcw;
end;

procedure TAggPathStorage.InvertPolygon(Start, Stop: Cardinal);
var
  I, TmpCmd, StartNb, StopNb: Cardinal;
  StartPointer, StopPointer: PDouble;
  TmpXY: Double;
begin
  TmpCmd := Command[Start];

  Dec(Stop); // Make "end" inclusive

  // Shift all commands to one position
  I := Start;

  while I < Stop do
  begin
    Command[I] := Command[I + 1];

    Inc(I);
  end;

  // Assign starting command to the ending command
  Command[Stop] := TmpCmd;

  // Reverse the polygon
  while Stop > Start do
  begin
    StartNb := Start shr CAggBlockShift;
    StopNb := Stop shr CAggBlockShift;

    StartPointer := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + StartNb *
      SizeOf(PDouble)).Ptr) + ((Start and CAggBlockMask) shl 1) *
      SizeOf(Double));

    StopPointer := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + StopNb *
      SizeOf(PDouble)).Ptr) + ((Stop and CAggBlockMask) shl 1) *
      SizeOf(Double));

    TmpXY := StartPointer^;
    StartPointer^ := StopPointer^;
    Inc(PtrComp(StartPointer), SizeOf(Double));
    StopPointer^ := TmpXY;
    Inc(PtrComp(StopPointer), SizeOf(Double));

    TmpXY := StartPointer^;
    StartPointer^ := StopPointer^;
    StopPointer^ := TmpXY;

    TmpCmd := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StartNb *
      SizeOf(PInt8u)).Ptr) + (Start and CAggBlockMask) * SizeOf(Int8u))^;

    PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StartNb *
      SizeOf(PInt8u)).Ptr) + (Start and CAggBlockMask) * SizeOf(Int8u))^ :=
      PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StopNb *
      SizeOf(PInt8u)).Ptr) + (Stop and CAggBlockMask) * SizeOf(Int8u))^;

    PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StopNb * SizeOf(PInt8u)
      ).Ptr) + (Stop and CAggBlockMask) * SizeOf(Int8u))^ := Int8u(TmpCmd);

    Inc(Start);
    Dec(Stop);
  end;
end;

procedure TAggPathStorage.InvertPolygon(Start: Cardinal);
var
  Stop: Cardinal;
begin
  // Skip all non-vertices at the beginning
  while (Start < FTotalVertices) and not IsVertex(Command[Start]) do
    Inc(Start);

  // Skip all insignificant MoveTo
  while (Start + 1 < FTotalVertices) and IsMoveTo(Command[Start]) and
    IsMoveTo(Command[Start + 1]) do
    Inc(Start);

  // Find the last vertex
  Stop := Start + 1;

  while (Stop < FTotalVertices) and not IsNextPoly(Command[Stop]) do
    Inc(Stop);

  InvertPolygon(Start, Stop);
end;

procedure TAggPathStorage.ConcatPath(Vs: TAggVertexSource;
  PathID: Cardinal = 0);
var
  X, Y: Double;
  Cmd : Cardinal;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    AddVertex(X, Y, Cmd);

    Cmd := Vs.Vertex(@X, @Y);
  end;
end;

end.
