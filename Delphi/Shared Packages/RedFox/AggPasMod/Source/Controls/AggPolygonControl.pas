unit AggPolygonControl;

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
  Math,
  AggBasics,
  AggConvStroke,
  AggEllipse,
  AggColor,
  AggControl,
  AggVertexSource;

type
  TAggSimplePolygonVertexSource = class(TAggVertexSource)
  private
    FPolygon: PPointDouble;
    FNumPoints, FVertex: Cardinal;
    FRoundOff, FClose: Boolean;
    procedure SetClose(Value: Boolean);
  public
    constructor Create(Polygon: PDouble; NumPoints: Cardinal;
      RoundOff: Boolean = False; Close: Boolean = True);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Close: Boolean read FClose write SetClose;
  end;

  TAggCustomPolygonControl = class(TAggCustomAggControl)
  private
    FPolygon: PPointDouble;
    FNumPoints: Cardinal;

    FNode, FEdge: Integer;

    FVertexSource: TAggSimplePolygonVertexSource;
    FStroke: TAggConvStroke;

    FEllipse: TAggEllipse;
    FPointRadius: Double;
    FStatus: Cardinal;

    FDelta: TPointDouble;

    FInPolygonCheck: Boolean;

    function GetClose: Boolean;
    function GetInPolygonCheck: Boolean;
    function GetLineWidth: Double;

    procedure SetClose(Value: Boolean);
    procedure SetInPolygonCheck(Value: Boolean);
    procedure SetLineWidth(Value: Double);
    procedure SetPointRadius(Value: Double);

    // Private
    function CheckEdge(I: Cardinal; X, Y: Double): Boolean;
    function PointInPolygon(X, Y: Double): Boolean;

    function GetX(Index: Cardinal): Double;
    function GetY(Index: Cardinal): Double;
    function GetPoint(Index: Cardinal): TPointDouble;
    procedure SetX(Index: Cardinal; const Value: Double);
    procedure SetY(Index: Cardinal; const Value: Double);
    procedure SetPoint(Index: Cardinal; const Value: TPointDouble);

    function GetXPointer(Index: Cardinal): PDouble;
    function GetYPointer(Index: Cardinal): PDouble;

    function GetPolygon: PPointDouble;
    function GetNumPoints: Cardinal;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create(NumPoints: Cardinal; PointRadius: Double = 5); virtual;
    destructor Destroy; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    // Event handlers
    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;
    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;

    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    property Close: Boolean read GetClose write SetClose;
    property InPolygonCheck: Boolean read GetInPolygonCheck write SetInPolygonCheck;
    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property PointRadius: Double read FPointRadius write SetPointRadius;

    property NumPoints: Cardinal read GetNumPoints;

    property Polygon: PPointDouble read GetPolygon;
    property Point[Index: Cardinal]: TPointDouble read GetPoint write
      SetPoint; default;
    property Xn[Index: Cardinal]: Double read GetX write SetX;
    property Yn[Index: Cardinal]: Double read GetY write SetY;
    property XnPtr[Index: Cardinal]: PDouble read GetXPointer;
    property YnPtr[Index: Cardinal]: PDouble read GetYPointer;
  end;

  TPolygonControl = class(TAggCustomPolygonControl)
  private
    FColor: TAggColor;
    procedure SetLineColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(NumPoints: Cardinal; PointRadius: Double = 5); override;
    property LineColor: TAggColor read FColor write SetLineColor;
  end;

implementation


{ TAggSimplePolygonVertexSource }

constructor TAggSimplePolygonVertexSource.Create(Polygon: PDouble; NumPoints: Cardinal;
  RoundOff: Boolean = False; Close: Boolean = True);
begin
  FPolygon := PPointDouble(Polygon);
  FNumPoints := NumPoints;
  FVertex := 0;
  FRoundOff := RoundOff;
  FClose := Close;
end;

procedure TAggSimplePolygonVertexSource.SetClose(Value: Boolean);
begin
  FClose := Value;
end;

procedure TAggSimplePolygonVertexSource.Rewind(PathID: Cardinal);
begin
  FVertex := 0;
end;

function TAggSimplePolygonVertexSource.Vertex(X, Y: PDouble): Cardinal;
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


{ TAggCustomPolygonControl }

constructor TAggCustomPolygonControl.Create(NumPoints: Cardinal;
  PointRadius: Double = 5);
begin
  inherited Create(0, 0, 1, 1, False);

  AggGetMem(Pointer(FPolygon), NumPoints * SizeOf(TPointDouble));

  FNumPoints := NumPoints;

  FNode := -1;
  FEdge := -1;

  FVertexSource := TAggSimplePolygonVertexSource.Create(PDouble(FPolygon),
    FNumPoints, False);
  FStroke := TAggConvStroke.Create(FVertexSource);
  FEllipse := TAggEllipse.Create;

  FPointRadius := PointRadius;

  FStatus := 0;

  FDelta.X := 0.0;
  FDelta.Y := 0.0;

  FInPolygonCheck := False;

  FStroke.Width := 1.0;
end;

destructor TAggCustomPolygonControl.Destroy;
begin
  AggFreeMem(Pointer(FPolygon), FNumPoints * SizeOf(TPointDouble));

  FVertexSource.Free;
  FEllipse.Free;
  FStroke.Free;

  inherited;
end;

function TAggCustomPolygonControl.GetNumPoints: Cardinal;
begin
  Result := FNumPoints;
end;

function TAggCustomPolygonControl.GetX(Index: Cardinal): Double;
begin
  Result := PDouble(PtrComp(FPolygon) + (Index * 2) * SizeOf(Double))^;
end;

function TAggCustomPolygonControl.GetY(Index: Cardinal): Double;
begin
  Result := PDouble(PtrComp(FPolygon) + (Index * 2 + 1) * SizeOf(Double))^;
end;

function TAggCustomPolygonControl.GetPoint(Index: Cardinal): TPointDouble;
var
  P: PPointDouble;
begin
  P := FPolygon;
  Inc(P, Index);
  Result := P^;
end;

procedure TAggCustomPolygonControl.SetX(Index: Cardinal; const Value: Double);
begin
  PDouble(PtrComp(FPolygon) + (Index * 2) * SizeOf(Double))^ := Value;
end;

procedure TAggCustomPolygonControl.SetY(Index: Cardinal; const Value: Double);
begin
  PDouble(PtrComp(FPolygon) + (Index * 2 + 1) * SizeOf(Double))^ := Value;
end;

procedure TAggCustomPolygonControl.SetPoint(Index: Cardinal;
  const Value: TPointDouble);
var
  P: PPointDouble;
begin
  P := FPolygon;
  Inc(P, Index);
  P^ := Value;
end;

function TAggCustomPolygonControl.GetXPointer(Index: Cardinal): PDouble;
begin
  Result := PDouble(FPolygon);
  Inc(Result, 2 * Index);
end;

function TAggCustomPolygonControl.GetYPointer(Index: Cardinal): PDouble;
begin
  Result := PDouble(FPolygon);
  Inc(Result, 2 * Index + 1);
end;

function TAggCustomPolygonControl.GetPolygon: PPointDouble;
begin
  Result := FPolygon;
end;

procedure TAggCustomPolygonControl.SetLineWidth(Value: Double);
begin
  FStroke.Width := Value;
end;

function TAggCustomPolygonControl.GetLineWidth: Double;
begin
  Result := FStroke.Width;
end;

procedure TAggCustomPolygonControl.SetPointRadius(Value: Double);
begin
  FPointRadius := Value;
end;

procedure TAggCustomPolygonControl.SetInPolygonCheck(Value: Boolean);
begin
  FInPolygonCheck := Value;
end;

function TAggCustomPolygonControl.GetInPolygonCheck: Boolean;
begin
  Result := FInPolygonCheck;
end;

procedure TAggCustomPolygonControl.SetClose(Value: Boolean);
begin
  FVertexSource.SetClose(Value);
end;

function TAggCustomPolygonControl.GetClose: Boolean;
begin
  Result := FVertexSource.Close;
end;

function TAggCustomPolygonControl.GetPathCount: Cardinal;
begin
  Result := 1;
end;

procedure TAggCustomPolygonControl.Rewind(PathID: Cardinal);
begin
  FStatus := 0;

  FStroke.Rewind(0);
end;

function TAggCustomPolygonControl.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
  R  : Double;
begin
  Cmd := CAggPathCmdStop;
  R := FPointRadius;

  if FStatus = 0 then
  begin
    Cmd := FStroke.Vertex(X, Y);

    if not IsStop(Cmd) then
    begin
      TransformXY(X, Y);

      Result := Cmd;

      Exit;
    end;

    if (FNode >= 0) and (FNode = FStatus) then
      R := R * 1.2;

    FEllipse.Initialize(GetPoint(FStatus), PointDouble(R), 32);

    Inc(FStatus);
  end;

  Cmd := FEllipse.Vertex(X, Y);

  if not IsStop(Cmd) then
  begin
    TransformXY(X, Y);

    Result := Cmd;

    Exit;
  end;

  if FStatus >= FNumPoints then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  if (FNode >= 0) and (FNode = FStatus) then
    R := R * 1.2;

  FEllipse.Initialize(GetPoint(FStatus), PointDouble(R), 32);

  Inc(FStatus);

  Cmd := FEllipse.Vertex(X, Y);

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;

function TAggCustomPolygonControl.InRect(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomPolygonControl.OnMouseButtonDown(X, Y: Double): Boolean;
var
  I  : Cardinal;
begin
  Result := False;

  FNode := -1;
  FEdge := -1;

  InverseTransformXY(@X, @Y);

  for I := 0 to FNumPoints - 1 do
    if Hypot(X - GetX(I), Y - GetY(I)) < FPointRadius then
    begin
      FDelta.X := X - GetX(I);
      FDelta.Y := Y - GetY(I);
      FNode := I;
      Result := True;

      Break;
    end;

  if not Result then
    for I := 0 to FNumPoints - 1 do
      if CheckEdge(I, X, Y) then
      begin
        FDelta.X := X;
        FDelta.Y := Y;
        FEdge := I;
        Result := True;
        Break;
      end;

  if not Result then
    if PointInPolygon(X, Y) then
    begin
      FDelta.X := X;
      FDelta.Y := Y;
      FNode := FNumPoints;
      Result := True;
    end;
end;

function TAggCustomPolygonControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := (FNode >= 0) or (FEdge >= 0);
  FNode := -1;
  FEdge := -1;
end;

function TAggCustomPolygonControl.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
var
  I, N1, N2: Cardinal;
  Delta: TPointDouble;
begin
  Result := False;

  InverseTransformXY(@X, @Y);

  if FNode = FNumPoints then
  begin
    Delta := PointDouble(X - FDelta.X, Y - FDelta.Y);

    for I := 0 to FNumPoints - 1 do
      Point[I] := PointDouble(Xn[I] + Delta.X, Yn[I] + Delta.Y);

    FDelta := PointDouble(X, Y);
    Result := True;
  end
  else if FEdge >= 0 then
  begin
    N1 := FEdge;
    N2 := (N1 + FNumPoints - 1) mod FNumPoints;
    Delta := PointDouble(X - FDelta.X, Y - FDelta.Y);

    Point[N1] := PointDouble(Xn[N1] + Delta.X, Yn[N1] + Delta.Y);
    Point[N2] := PointDouble(Xn[N2] + Delta.X, Yn[N2] + Delta.Y);

    FDelta := PointDouble(X, Y);
    Result := True;
  end
  else if FNode >= 0 then
  begin
    Point[FNode] := PointDouble(X - FDelta.X, Y - FDelta.Y);
    Result := True;
  end;
end;

function TAggCustomPolygonControl.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
begin
  Result := False;
end;

function TAggCustomPolygonControl.CheckEdge(I: Cardinal; X, Y: Double): Boolean;
var
  N1, N2: Cardinal;
  Delta: TPointDouble;
  X1, Y1, X2, Y2, X3, Y3, X4, Y4, Den, U1, Xi, Yi: Double;
begin
  Result := False;

  N1 := I;
  N2 := (I + FNumPoints - 1) mod FNumPoints;
  X1 := GetX(N1);
  Y1 := GetY(N1);
  X2 := GetX(N2);
  Y2 := GetY(N2);

  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;

  if Hypot(Delta.X, Delta.Y) > 1E-7 then
  begin
    X3 := X;
    Y3 := Y;
    X4 := X3 - Delta.Y;
    Y4 := Y3 + Delta.X;

    Den := (Y4 - Y3) * (X2 - X1) - (X4 - X3) * (Y2 - Y1);
    U1 := ((X4 - X3) * (Y1 - Y3) - (Y4 - Y3) * (X1 - X3)) / Den;

    Xi := X1 + U1 * (X2 - X1);
    Yi := Y1 + U1 * (Y2 - Y1);

    Delta.X := Xi - X;
    Delta.Y := Yi - Y;

    Result := (U1 > 0.0) and (U1 < 1.0) and (Hypot(Delta.X, Delta.Y) <=
      FPointRadius);
  end;
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
// for the CrossingsTest code; it is left out here for clarity.

function TAggCustomPolygonControl.PointInPolygon(X, Y: Double): Boolean;
var
  J, K: Cardinal;
  FlagY: array [0..1] of Integer;
  InsideFlag: Integer;
  Vertex: array [0..1] of TPointDouble;
begin
  if (FNumPoints < 3) or (not FInPolygonCheck) then
  begin
    Result := False;
    Exit;
  end;

  Vertex[0].X := GetX(FNumPoints - 1);
  Vertex[0].Y := GetY(FNumPoints - 1);

  // get test bit for above/below X axis
  FlagY[0] := Integer(Vertex[0].Y >= Y);

  Vertex[1].X := GetX(0);
  Vertex[1].Y := GetY(0);

  InsideFlag := 0;

  for J := 1 to FNumPoints do
  begin
    FlagY[1] := Integer(Vertex[1].Y >= Y);

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
    if FlagY[0] <> FlagY[1] then
      // Check intersection of pgon segment with +X ray.
      // Note if >= point's X; if so, the ray hits it.
      // The division operation is avoided for the ">=" test by checking
      // the sign of the first vertex wrto the test point; idea inspired
      // by Joseph Samosky's and Mark Haigh-Hutchinson's different
      // polygon inclusion tests.
      if Integer((Vertex[1].Y - Y) * (Vertex[0].X - Vertex[1].X) >=
        (Vertex[1].X - X) * (Vertex[0].Y - Vertex[1].Y)) = FlagY[1]
      then
        InsideFlag := InsideFlag xor 1;

    // Move to the next pair of vertices, retaining info as possible.
    FlagY[0] := FlagY[1];
    Vertex[0] := Vertex[1];

    if J >= FNumPoints then
      K := J - FNumPoints
    else
      K := J;

    Vertex[1].X := GetX(K);
    Vertex[1].Y := GetY(K);
  end;

  Result := InsideFlag <> 0;
end;


{ TPolygonControl }

constructor TPolygonControl.Create(NumPoints: Cardinal; PointRadius: Double = 5);
begin
  inherited Create(NumPoints, PointRadius);

  FColor.Black;
end;

procedure TPolygonControl.SetLineColor(Value: TAggColor);
begin
  FColor := Value;
end;

function TPolygonControl.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := @FColor;
end;

end.
