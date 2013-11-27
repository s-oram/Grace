unit AggInteractivePolygon;

interface

{$I AggCompiler.inc}

uses
  Math,
  AggBasics,
  AggConvStroke,
  AggEllipse,
  AggVertexSource;

type
  TSimplePolygonVertexSource = class(TAggVertexSource)
  private
    FPolygon: PPointDouble;
    FNumPoints, FVertex: Cardinal;
    FRoundOff, FClose: Boolean;

    procedure SetClose(F: Boolean);
  public
    constructor Create(Polygon: PPointDouble; Np: Cardinal;
      RoundOff: Boolean = False; Close: Boolean = True);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Close: Boolean read FClose write SetClose;
  end;

  TInteractivePolygon = class(TAggVertexSource)
  private
    FPolygon: PPointDouble;
    FNumPoints: Cardinal;

    FNode, FEdge: Integer;

    FVertexSource: TSimplePolygonVertexSource;

    FStroke: TAggConvStroke;
    FEllipse: TAggEllipse;

    FPointRadius: Double;
    FStatus: Cardinal;

    FDelta: TPointDouble;

    function GetNode: Integer;
    function GetClose: Boolean;

    function GetPoint(Index: Cardinal): TPointDouble;
    function GetX(Index: Cardinal): Double;
    function GetY(Index: Cardinal): Double;
    function GetXPointer(Index: Cardinal): PDouble;
    function GetYPointer(Index: Cardinal): PDouble;

    procedure SetClose(F: Boolean);
    procedure SetNode(Index: Integer);
    procedure SetPoint(Index: Cardinal; const Value: TPointDouble);
    procedure SetX(Index: Cardinal; const Value: Double);
    procedure SetY(Index: Cardinal; const Value: Double);
  protected
    function GetPolygon: PPointDouble;
  public
    constructor Create(Np: Cardinal; PointRadius: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    function OnMouseMove(X, Y: Double): Boolean;

    function OnMouseButtonDown(X, Y: Double): Boolean;
    function OnMouseButtonUp(X, Y: Double): Boolean;

    function CheckEdge(I: Cardinal; X, Y: Double): Boolean;

    function PointInPolygon(Tx, Ty: Double): Boolean;

    property Node: Integer read GetNode write SetNode;
    property Close: Boolean read GetClose write SetClose;

    property Point[Index: Cardinal]: TPointDouble read GetPoint write SetPoint;
    property Xn[Index: Cardinal]: Double read GetX write SetX;
    property Yn[Index: Cardinal]: Double read GetY write SetY;
    property XnPtr[Index: Cardinal]: PDouble read GetXPointer;
    property YnPtr[Index: Cardinal]: PDouble read GetYPointer;

    property NumPoints: Cardinal read FNumPoints;
    property Polygon: PPointDouble read GetPolygon;
  end;

implementation


{ TSimplePolygonVertexSource }

constructor TSimplePolygonVertexSource.Create(Polygon: PPointDouble; Np: Cardinal;
      RoundOff: Boolean = False; Close: Boolean = True);
begin
  inherited Create;

  FPolygon := Polygon;
  FNumPoints := Np;

  FVertex := 0;
  FRoundOff := RoundOff;
  FClose := Close;
end;

procedure TSimplePolygonVertexSource.SetClose;
begin
  FClose := F;
end;

procedure TSimplePolygonVertexSource.Rewind(PathID: Cardinal);
begin
  FVertex := 0;
end;

function TSimplePolygonVertexSource.Vertex(X, Y: PDouble): Cardinal;
begin
  if FVertex > FNumPoints then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  if FVertex = FNumPoints then
  begin
    Inc(FVertex);

    if FClose then
      Result := CAggPathCmdEndPoly or CAggPathFlagsClose
    else
      Result := CAggPathCmdEndPoly or 0;

    Exit;
  end;

  X^ := PDouble(PtrComp(FPolygon) + (FVertex * 2) * SizeOf(Double))^;
  Y^ := PDouble(PtrComp(FPolygon) + (FVertex * 2 + 1) * SizeOf(Double))^;

  if FRoundOff then
  begin
    X^ := Floor(X^) + 0.5;
    Y^ := Floor(Y^) + 0.5;
  end;

  Inc(FVertex);

  if FVertex = 1 then
    Result := CAggPathCmdMoveTo
  else
    Result := CAggPathCmdLineTo;
end;


{ TInteractivePolygon }

constructor TInteractivePolygon.Create;
begin
  inherited Create;

  AggGetMem(Pointer(FPolygon), Np * 2 * SizeOf(Double));

  FNumPoints := Np;

  FNode := -1;
  FEdge := -1;

  FVertexSource := TSimplePolygonVertexSource.Create(FPolygon, FNumPoints,
    False);
  FStroke := TAggConvStroke.Create(FVertexSource);
  FEllipse := TAggEllipse.Create;

  FPointRadius := PointRadius;
  FStatus := 0;

  FDelta.X := 0.0;
  FDelta.Y := 0.0;

  FStroke.Width := 1.0;
end;

destructor TInteractivePolygon.Destroy;
begin
  AggFreeMem(Pointer(FPolygon), FNumPoints * 2 * SizeOf(Double));

  FVertexSource.Free;
  FEllipse.Free;
  FStroke.Free;

  inherited;
end;

function TInteractivePolygon.GetPoint(Index: Cardinal): TPointDouble;
var
  P: PPointDouble;
begin
  P := FPolygon;
  Inc(P, Index);
  Result := P^;
end;

function TInteractivePolygon.GetX(Index: Cardinal): Double;
var
  P: PPointDouble;
begin
  P := FPolygon;
  Inc(P, Index);
  Result := P^.X;
end;

function TInteractivePolygon.GetY(Index: Cardinal): Double;
var
  P: PPointDouble;
begin
  P := FPolygon;
  Inc(P, Index);
  Result := P^.Y;
end;

function TInteractivePolygon.GetXPointer(Index: Cardinal): PDouble;
begin
  Result := PDouble(FPolygon);
  Inc(Result, 2 * Index);
end;

function TInteractivePolygon.GetYPointer(Index: Cardinal): PDouble;
begin
  Result := PDouble(FPolygon);
  Inc(Result, 2 * Index + 1);
end;

function TInteractivePolygon.GetPolygon;
begin
  Result := FPolygon;
end;

function TInteractivePolygon.GetNode;
begin
  Result := FNode;
end;

procedure TInteractivePolygon.SetNode;
begin
  FNode := Index;
end;

procedure TInteractivePolygon.SetPoint(Index: Cardinal; const Value: TPointDouble);
var
  P: PPointDouble;
begin
  P := FPolygon;
  Inc(P, Index);
  P^ := Value;
end;

procedure TInteractivePolygon.SetX(Index: Cardinal; const Value: Double);
begin
  PDouble(PtrComp(FPolygon) + (Index * 2) * SizeOf(Double))^ := Value
end;

procedure TInteractivePolygon.SetY(Index: Cardinal; const Value: Double);
begin
  PDouble(PtrComp(FPolygon) + (Index * 2 + 1) * SizeOf(Double))^ := Value;
end;

function TInteractivePolygon.GetClose;
begin
  Result := FVertexSource.Close;
end;

procedure TInteractivePolygon.SetClose;
begin
  FVertexSource.SetClose(F);
end;

procedure TInteractivePolygon.Rewind(PathID: Cardinal);
begin
  FStatus := 0;

  FStroke.Rewind(0);
end;

function TInteractivePolygon.Vertex(X, Y: PDouble): Cardinal;
var
  R: Double;
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdStop;
  R := FPointRadius;

  if FStatus = 0 then
  begin
    Cmd := FStroke.Vertex(X, Y);

    if not IsStop(Cmd) then
    begin
      Result := Cmd;

      Exit;
    end;

    if (FNode >= 0) and (FNode = Integer(FStatus)) then
      R := R * 1.2;

    FEllipse.Initialize(Point[FStatus], PointDouble(R), 32);

    Inc(FStatus);
  end;

  Cmd := FEllipse.Vertex(X, Y);

  if not IsStop(Cmd) then
  begin
    Result := Cmd;

    Exit;
  end;

  if FStatus >= FNumPoints then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  if (FNode >= 0) and (FNode = Integer(FStatus)) then
    R := R * 1.2;

  FEllipse.Initialize(Point[FStatus], PointDouble(R), 32);

  Inc(FStatus);

  Result := FEllipse.Vertex(X, Y);
end;

function TInteractivePolygon.OnMouseMove(X, Y: Double): Boolean;
var
  Ret: Boolean;
  I, N1, N2: Cardinal;
  Dx, Dy: Double;
begin
  Ret := False;

  if FNode = Integer(FNumPoints) then
  begin
    Dx := X - FDelta.X;
    Dy := Y - FDelta.Y;

    for I := 0 to FNumPoints - 1 do
    begin
      Xn[I] := Xn[I] + Dx;
      Yn[I] := Yn[I] + Dy;
    end;

    FDelta.X := X;
    FDelta.Y := Y;

    Ret := True;
  end
  else if FEdge >= 0 then
  begin
    N1 := FEdge;
    N2 := (N1 + FNumPoints - 1) mod FNumPoints;

    Dx := X - FDelta.X;
    Dy := Y - FDelta.Y;

    Xn[N1] := Xn[N1] + Dx;
    Yn[N1] := Yn[N1] + Dy;
    Xn[N2] := Xn[N2] + Dx;
    Yn[N2] := Yn[N2] + Dy;

    FDelta.X := X;
    FDelta.Y := Y;

    Ret := True;
  end
  else if FNode >= 0 then
  begin
    Xn[FNode] := X - FDelta.X;
    Yn[FNode] := Y - FDelta.Y;

    Ret := True;
  end;

  Result := Ret;
end;

function TInteractivePolygon.OnMouseButtonDown(X, Y: Double): Boolean;
var
  I: Cardinal;
  Ret: Boolean;
begin
  Ret := False;

  FNode := -1;
  FEdge := -1;

  for I := 0 to FNumPoints - 1 do
    if Sqrt((X - Xn[I]) * (X - Xn[I]) + (Y - Yn[I]) * (Y - Yn[I])) < FPointRadius
    then
    begin
      FDelta.X := X - Xn[I];
      FDelta.Y := Y - Yn[I];

      FNode := Integer(I);

      Ret := True;

      Break;
    end;

  if not Ret then
    for I := 0 to FNumPoints - 1 do
      if CheckEdge(I, X, Y) then
      begin
        FDelta.X := X;
        FDelta.Y := Y;

        FEdge := Integer(I);

        Ret := True;

        Break;
      end;

  if not Ret then
    if PointInPolygon(X, Y) then
    begin
      FDelta.X := X;
      FDelta.Y := Y;

      FNode := Integer(FNumPoints);

      Ret := True;
    end;

  Result := Ret;
end;

function TInteractivePolygon.OnMouseButtonUp(X, Y: Double): Boolean;
var
  Ret: Boolean;
begin
  Ret := (FNode >= 0) or (FEdge >= 0);

  FNode := -1;
  FEdge := -1;
  Result := Ret;
end;

function TInteractivePolygon.CheckEdge;
var
  Ret: Boolean;
  N1, N2: Cardinal;
  X1, Y1, X2, Y2, Dx, Dy, X3, Y3, X4, Y4, Den, U1, Xi, Yi: Double;
begin
  Ret := False;

  N1 := I;
  N2 := (I + FNumPoints - 1) mod FNumPoints;

  X1 := Xn[N1];
  Y1 := Yn[N1];
  X2 := Xn[N2];
  Y2 := Yn[N2];

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Sqrt(Dx * Dx + Dy * Dy) > 0.0000001 then
  begin
    X3 := X;
    Y3 := Y;
    X4 := X3 - Dy;
    Y4 := Y3 + Dx;

    Den := (Y4 - Y3) * (X2 - X1) - (X4 - X3) * (Y2 - Y1);
    U1 := ((X4 - X3) * (Y1 - Y3) - (Y4 - Y3) * (X1 - X3)) / Den;

    Xi := X1 + U1 * (X2 - X1);
    Yi := Y1 + U1 * (Y2 - Y1);

    Dx := Xi - X;
    Dy := Yi - Y;

    if (U1 > 0.0) and (U1 < 1.0) and (Sqrt(Dx * Dx + Dy * Dy) <= FPointRadius)
    then
      Ret := True;
  end;

  Result := Ret;
end;

// ======= Crossings Multiply algorithm of InsideTest ========================
//
// By Eric Haines, 3D/Eye Inc, erich@eye.com
//
// This version is usually somewhat faster than the original published in
// Graphics Gems IV; by turning the division for testing the X axis crossing
// into a tricky multiplication test this part of the test became faster,
// which had the additional effect of making the test for "both to left or
// both to right" a bit sLower for triangles than simply computing the
// intersection each time.  The main increase is in triangle testing speed,
// which was about 15% faster; all other polygon complexities were pretty much
// the same as before.  On machines where division is very expensive (not the
// case on the HP 9000 series on which I tested) this test should be much
// faster overall than the old code.  Your mileage may (in fact, will) vary,
// depending on the machine and the test data, but in general I believe this
// code is both shorter and faster.  This test was inspired by unpublished
// Graphics Gems submitted by Joseph Samosky and Mark Haigh-Hutchinson.
// Related work by Samosky is in:
//
// Samosky, Joseph, "SectionView: A system for interactively specifying and
// visualizing sections through three-dimensional medical image data",
// M.S. Thesis, Department of Electrical Engineering and Computer Science,
// Massachusetts Institute of Technology, 1993.
//
// Shoot a test ray along +X axis.  The strategy is to compare vertex Y values
// to the testing point's Y and quickly discard edges which are entirely to one
// side of the test ray.  Note that CONVEX and WINDING code can be added as
// for the CrossingsTest() code; it is left out here for clarity.
//
// Input 2D polygon with number of vertices and test point, returns 1 if
// inside, 0 if outside.

function TInteractivePolygon.PointInPolygon(Tx, Ty: Double): Boolean;
var
  J, K: Cardinal;
  Yflag0, Yflag1, InsideFlag: Integer;
  Vtx0, Vty0, Vtx1, Vty1: Double;
begin
  if FNumPoints < 3 then
  begin
    Result := False;

    Exit;
  end;

  Vtx0 := Xn[FNumPoints - 1];
  Vty0 := Yn[FNumPoints - 1];

  // get test bit for above/below X axis
  Yflag0 := Integer(Vty0 >= Ty);

  Vtx1 := Xn[0];
  Vty1 := Yn[0];

  InsideFlag := 0;

  for J := 1 to FNumPoints do
  begin
    Yflag1 := Integer(Vty1 >= Ty);

    // Check if endpoints straddle (are on opposite sides) of X axis
    // (i.e. the Y's differ); if so, +X ray could intersect this edge.
    // The old test also checked whether the endpoints are both to the
    // right or to the left of the test point.  However, given the faster
    // intersection point computation used below, this test was found to
    // be a break-even proposition for most polygons and a loser for
    // triangles (where 50% or more of the edges which survive this test
    // will cross quadrants and so have to have the X intersection computed
    // anyway).  I credit Joseph Samosky with inspiring me to try dropping
    // the "both left or both right" part of my code.
    if Yflag0 <> Yflag1 then
      // Check intersection of pgon segment with +X ray.
      // Note if >= point's X; if so, the ray hits it.
      // The division operation is avoided for the ">=" test by checking
      // the sign of the first vertex wrto the test point; idea inspired
      // by Joseph Samosky's and Mark Haigh-Hutchinson's different
      // polygon inclusion tests.
      if Integer((Vty1 - Ty) * (Vtx0 - Vtx1) >= (Vtx1 - Tx) * (Vty0 - Vty1)) = Yflag1
      then
        InsideFlag := InsideFlag xor 1;

    // Move to the next pair of vertices, retaining info as possible.
    Yflag0 := Yflag1;

    Vtx0 := Vtx1;
    Vty0 := Vty1;

    if J >= FNumPoints then
      K := J - FNumPoints
    else
      K := J;

    Vtx1 := Xn[K];
    Vty1 := Yn[K];
  end;

  Result := InsideFlag <> 0;
end;

end.
