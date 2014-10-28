unit AggCurves;

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

uses
  Math,
  AggBasics,
  AggArray,
  AggVertexSource;

type
  TAggCurveApproximationMethod = (camInc, camDiv);
  TAggCurve3PointArray = array [0..2] of TPointDouble;
  TAggCurve4PointArray = array [0..3] of TPointDouble;

  PAggCurve3Points = ^TAggCurve3Points;
  TAggCurve3Points = record
    Point: TAggCurve3PointArray;
  public
    procedure Init(X1, Y1, X2, Y2, X3, Y3: Double); overload;
    procedure Init(Point1, Point2, Point3: TPointDouble); overload;
  end;

  PAggCurve4Points = ^TAggCurve4Points;
  TAggCurve4Points = record
    Point: TAggCurve4PointArray;
  public
    procedure Init(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;
    procedure Init(Point1, Point2, Point3, Point4: TPointDouble); overload;
  end;

  TAggCustomCurve = class(TAggVertexSource)
  protected
    procedure SetApproximationScale(Value: Double); virtual; abstract;
    function GetApproximationScale: Double; virtual; abstract;

    procedure SetAngleTolerance(Value: Double); virtual; abstract;
    function GetAngleTolerance: Double; virtual; abstract;

    procedure SetCuspLimit(Value: Double); virtual; abstract;
    function GetCuspLimit: Double; virtual; abstract;

    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod);
      virtual; abstract;
    function GetApproximationMethod: TAggCurveApproximationMethod;
      virtual; abstract;
  public
    procedure Reset; virtual; abstract;
    procedure Init3(Point1, Point2, Point3: TPointDouble); virtual;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); virtual;

    property ApproximationScale: Double read GetApproximationScale
      write SetApproximationScale;
    property AngleTolerance: Double read GetAngleTolerance write
      SetAngleTolerance;
    property CuspLimit: Double read GetCuspLimit write SetCuspLimit;
    property ApproximationMethod: TAggCurveApproximationMethod read
      GetApproximationMethod write SetApproximationMethod;
  end;

  TAggCurve3Inc = class(TAggCustomCurve)
  private
    FNumSteps, FStep: Integer;
    FScale: Double;
    FStart, FEnd, FDeltaF, FSavedF: TPointDouble;
    FDeltaDelta, FPoint, FSavedDelta: TPointDouble;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3: Double); overload;
    constructor Create(Point1, Point2, Point3: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve3Points); overload;

    procedure Reset; override;
    procedure Init3(Point1, Point2, Point3: TPointDouble); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggCurve3Div = class(TAggCustomCurve)
  private
    FApproximationScale, FDistanceToleranceSquare: Double;
    FDistanceToleranceManhattan, FAngleTolerance: Double;

    FCount: Cardinal;
    FPoints: TAggPodDeque;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3: Double); overload;
    constructor Create(Point1, Point2, Point3: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve3Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init3(Point1, Point2, Point3: TPointDouble); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    procedure Bezier(X1, Y1, X2, Y2, X3, Y3: Double); overload;
    procedure Bezier(Point1, Point2, Point3: TPointDouble); overload;
    procedure RecursiveBezier(X1, Y1, X2, Y2, X3, Y3: Double; Level: Cardinal);
  end;

  TAggCurve4Inc = class(TAggCustomCurve)
  private
    FNumSteps, FStep: Integer;

    FStart, FEnd, FDeltaF, FSavedF, FSavedDelta: TPointDouble;
    FDeltaDelta, FPoint: TPointDouble;
    FSavedDeltaDelta, FDeltaDeltaDelta: TPointDouble;
    FScale: Double;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;
    constructor Create(Point1, Point2, Point3, Point4: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve4Points); overload;

    procedure Reset; override;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); override;
    procedure Init(CurvePoints: PAggCurve4Points);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggCurve4Div = class(TAggCustomCurve)
  private
    FApproximationScale, FDistanceToleranceSquare: Double;
    FDistanceToleranceManhattan, FAngleTolerance, FCuspLimit: Double;

    FCount: Cardinal;
    FPoints: TAggPodDeque;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;
    constructor Create(Point1, Point2, Point3, Point4: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve4Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); override;

    procedure Init(CurvePoints: PAggCurve4Points);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    procedure Bezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;
    procedure Bezier(Point1, Point2, Point3, Point4: TPointDouble); overload;
    procedure RecursiveBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double;
      Level: Cardinal);
  end;

  TAggCurve3 = class(TAggCustomCurve)
  private
    FCurveInc: TAggCurve3Inc;
    FCurveDiv: TAggCurve3Div;

    FApproximationMethod: TAggCurveApproximationMethod;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3: Double); overload;
    constructor Create(Point1, Point2, Point3: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve3Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init3(Point1, Point2, Point3: TPointDouble); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggCurve4 = class(TAggCustomCurve)
  private
    FCurveInc: TAggCurve4Inc;
    FCurveDiv: TAggCurve4Div;

    FApproximationMethod: TAggCurveApproximationMethod;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;
    constructor Create(Point1, Point2, Point3, Point4: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve4Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); override;
    procedure Init(CurvePoints: PAggCurve4Points);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

function CatromToBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double)
  : TAggCurve4Points; overload;
function CatromToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points; overload;

function UbSplineToBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double)
  : TAggCurve4Points; overload;
function UbSplineToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points; overload;

function HermiteToBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double)
  : TAggCurve4Points; overload;
function HermiteToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points; overload;

implementation


const
  CCurveDistanceEpsilon = 1E-30;
  CCurveCollinearityEpsilon = 1E-30;
  CCurveAngleToleranceEpsilon = 0.01;
  CCurveRecursionLimit = 32;

function CatromToBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double): TAggCurve4Points;
begin
  // Trans. matrix Catmull-Rom to Bezier
  //
  // 0       1       0       0
  // -1/6    1       1/6     0
  // 0       1/6     1       -1/6
  // 0       0       1       0
  //
  Result.Init(X2, Y2, (-X1 + 6 * X2 + X3) / 6, (-Y1 + 6 * Y2 + Y3) / 6,
    (X2 + 6 * X3 - X4) / 6, (Y2 + 6 * Y3 - Y4) / 6, X3, Y3);
end;

function CatromToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points;
begin
  with CurvePoints^ do
    Result := CatromToBezier(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y,
      Point[2].X, Point[2].Y, Point[3].X, Point[3].Y)
end;

function UbSplineToBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double)
  : TAggCurve4Points;
begin
  // Trans. matrix Uniform BSpline to Bezier
  //
  // 1/6     4/6     1/6     0
  // 0       4/6     2/6     0
  // 0       2/6     4/6     0
  // 0       1/6     4/6     1/6
  //
  Result.Init((X1 + 4 * X2 + X3) / 6, (Y1 + 4 * Y2 + Y3) / 6,
    (4 * X2 + 2 * X3) / 6, (4 * Y2 + 2 * Y3) / 6, (2 * X2 + 4 * X3) / 6,
    (2 * Y2 + 4 * Y3) / 6, (X2 + 4 * X3 + X4) / 6, (Y2 + 4 * Y3 + Y4) / 6);
end;

function UbSplineToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points;
begin
  with CurvePoints^ do
    Result := UbSplineToBezier(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y,
      Point[2].X, Point[2].Y, Point[3].X, Point[3].Y)
end;

function HermiteToBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double): TAggCurve4Points;
begin
  // Trans. matrix Hermite to Bezier
  //
  // 1       0       0       0
  // 1       0       1/3     0
  // 0       1       0       -1/3
  // 0       1       0       0
  //
  Result.Init(X1, Y1, (3 * X1 + X3) / 3, (3 * Y1 + Y3) / 3,
    (3 * X2 - X4) / 3, (3 * Y2 - Y4) / 3, X2, Y2);
end;

function HermiteToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points;
begin
  with CurvePoints^ do
    Result := HermiteToBezier(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y,
      Point[2].X, Point[2].Y, Point[3].X, Point[3].Y)
end;


{ TAggCustomCurve }

procedure TAggCustomCurve.Init3(Point1, Point2, Point3: TPointDouble);
begin
end;

procedure TAggCustomCurve.Init4(Point1, Point2, Point3, Point4: TPointDouble);
begin
end;


{ TAggCurve3Points }

procedure TAggCurve3Points.Init(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  Point[0].X := X1;
  Point[0].Y := Y1;
  Point[1].X := X2;
  Point[1].Y := Y2;
  Point[2].X := X3;
  Point[2].Y := Y3;
end;

procedure TAggCurve3Points.Init(Point1, Point2, Point3: TPointDouble);
begin
  Point[0] := Point1;
  Point[1] := Point2;
  Point[2] := Point3;
end;


{ TAggCurve3Inc }

constructor TAggCurve3Inc.Create;
begin
  FNumSteps := 0;
  FStep := 0;
  FScale := 1.0;
end;

constructor TAggCurve3Inc.Create(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  Create;

  Init3(PointDouble(X1, Y1), PointDouble(X2, Y2), PointDouble(X3, Y3));
end;

constructor TAggCurve3Inc.Create(Point1, Point2, Point3: TPointDouble);
begin
  Create;

  Init3(Point1, Point2, Point3);
end;

constructor TAggCurve3Inc.Create(CurvePoints: PAggCurve3Points);
begin
  Create;

  Init3(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2]);
end;

procedure TAggCurve3Inc.Reset;
begin
  FNumSteps := 0;
  FStep := -1;
end;

procedure TAggCurve3Inc.Init3(Point1, Point2, Point3: TPointDouble);
var
  Delta: array [0..1] of TPointDouble;
  Len: Double;
  Tmp: TPointDouble;
  SubDivideStep: array [0..1] of Double;
begin
  FStart := Point1;
  FEnd := Point3;

  Delta[0] := PointDouble(Point2.X - Point1.X, Point2.Y - Point1.Y);
  Delta[1] := PointDouble(Point3.X - Point2.X, Point3.Y - Point2.Y);

  Len := Hypot(Delta[0].X, Delta[0].Y) + Hypot(Delta[1].X, Delta[1].Y);

  FNumSteps := Trunc(Len * 0.25 * FScale);

  if FNumSteps < 4 then
    FNumSteps := 4;

  SubDivideStep[0] := 1 / FNumSteps;
  SubDivideStep[1] := Sqr(SubDivideStep[0]);

  Tmp.X := (Point1.X - Point2.X - Point2.X + Point3.X) * SubDivideStep[1];
  Tmp.Y := (Point1.Y - Point2.Y - Point2.X + Point3.Y) * SubDivideStep[1];

  FSavedF := Point1;
  FPoint := Point1;

  FSavedDelta.X := Tmp.X + (Point2.X - Point1.X) * (2 * SubDivideStep[0]);
  FSavedDelta.Y := Tmp.Y + (Point2.Y - Point1.Y) * (2 * SubDivideStep[0]);
  FDeltaF := FSavedDelta;

  FDeltaDelta := PointDouble(2 * Tmp.X, 2 * Tmp.Y);

  FStep := FNumSteps;
end;

procedure TAggCurve3Inc.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
end;

function TAggCurve3Inc.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camInc;
end;

procedure TAggCurve3Inc.SetApproximationScale(Value: Double);
begin
  FScale := Value;
end;

function TAggCurve3Inc.GetApproximationScale: Double;
begin
  Result := FScale;
end;

procedure TAggCurve3Inc.SetAngleTolerance(Value: Double);
begin
end;

function TAggCurve3Inc.GetAngleTolerance: Double;
begin
  Result := 0.0;
end;

procedure TAggCurve3Inc.SetCuspLimit(Value: Double);
begin
end;

function TAggCurve3Inc.GetCuspLimit: Double;
begin
  Result := 0.0;
end;

procedure TAggCurve3Inc.Rewind(PathID: Cardinal);
begin
  if FNumSteps = 0 then
  begin
    FStep := -1;

    Exit;
  end;

  FStep := FNumSteps;
  FPoint.X := FSavedF.X;
  FPoint.Y := FSavedF.Y;
  FDeltaF.X := FSavedDelta.X;
  FDeltaF.Y := FSavedDelta.Y;
end;

function TAggCurve3Inc.Vertex(X, Y: PDouble): Cardinal;
begin
  if FStep < 0 then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  if FStep = FNumSteps then
  begin
    X^ := FStart.X;
    Y^ := FStart.Y;

    Dec(FStep);

    Result := CAggPathCmdMoveTo;

    Exit;
  end;

  if FStep = 0 then
  begin
    X^ := FEnd.X;
    Y^ := FEnd.Y;

    Dec(FStep);

    Result := CAggPathCmdLineTo;

    Exit;
  end;

  FPoint.X := FPoint.X + FDeltaF.X;
  FPoint.Y := FPoint.Y + FDeltaF.Y;
  FDeltaF.X := FDeltaF.X + FDeltaDelta.X;
  FDeltaF.Y := FDeltaF.Y + FDeltaDelta.Y;

  X^ := FPoint.X;
  Y^ := FPoint.Y;

  Dec(FStep);

  Result := CAggPathCmdLineTo;
end;


{ TAggCurve3Div }

constructor TAggCurve3Div.Create;
begin
  FPoints := TAggPodDeque.Create(SizeOf(TPointDouble));

  FApproximationScale := 1;
  FAngleTolerance := 0;
  FCount := 0;
end;

constructor TAggCurve3Div.Create(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  Create;

  Init3(PointDouble(X1, Y1), PointDouble(X2, Y2), PointDouble(X3, Y3));
end;

constructor TAggCurve3Div.Create(Point1, Point2, Point3: TPointDouble);
begin
  Create;

  Init3(Point1, Point2, Point3);
end;

constructor TAggCurve3Div.Create(CurvePoints: PAggCurve3Points);
begin
  Create;

  Init3(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2]);
end;

destructor TAggCurve3Div.Destroy;
begin
  FPoints.Free;
  inherited
end;

procedure TAggCurve3Div.Reset;
begin
  FPoints.RemoveAll;

  FCount := 0;
end;

procedure TAggCurve3Div.Init3(Point1, Point2, Point3: TPointDouble);
begin
  FPoints.RemoveAll;

  FDistanceToleranceSquare := Sqr(0.5 / FApproximationScale);
  FDistanceToleranceManhattan := 4.0 / FApproximationScale;

  Bezier(Point1, Point2, Point3);

  FCount := 0;
end;

procedure TAggCurve3Div.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
end;

function TAggCurve3Div.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camDiv;
end;

procedure TAggCurve3Div.SetApproximationScale(Value: Double);
begin
  if Value = 0 then
    FApproximationScale := 0.00001
  else
    FApproximationScale := Value;
end;

function TAggCurve3Div.GetApproximationScale: Double;
begin
  Result := FApproximationScale;
end;

procedure TAggCurve3Div.SetAngleTolerance(Value: Double);
begin
  FAngleTolerance := Value;
end;

function TAggCurve3Div.GetAngleTolerance: Double;
begin
  Result := FAngleTolerance;
end;

procedure TAggCurve3Div.SetCuspLimit(Value: Double);
begin
end;

function TAggCurve3Div.GetCuspLimit: Double;
begin
  Result := 0;
end;

procedure TAggCurve3Div.Rewind(PathID: Cardinal);
begin
  FCount := 0;
end;

function TAggCurve3Div.Vertex(X, Y: PDouble): Cardinal;
var
  P: PPointDouble;
begin
  if FCount >= FPoints.Size then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  P := FPoints[FCount];

  Inc(FCount);

  X^ := P.X;
  Y^ := P.Y;

  if FCount = 1 then
    Result := CAggPathCmdMoveTo
  else
    Result := CAggPathCmdLineTo;
end;

procedure TAggCurve3Div.Bezier(X1, Y1, X2, Y2, X3, Y3: Double);
var
  Pt: TPointDouble;
begin
  Pt.X := X1;
  Pt.Y := Y1;

  FPoints.Add(@Pt);

  RecursiveBezier(X1, Y1, X2, Y2, X3, Y3, 0);

  Pt.X := X3;
  Pt.Y := Y3;

  FPoints.Add(@Pt);
end;

procedure TAggCurve3Div.Bezier(Point1, Point2, Point3: TPointDouble);
begin
  FPoints.Add(@Point1);

  RecursiveBezier(Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y,
    0);

  FPoints.Add(@Point3);
end;

procedure TAggCurve3Div.RecursiveBezier(X1, Y1, X2, Y2, X3, Y3: Double;
  Level: Cardinal);
var
  Delta: TPointDouble;
  X12, Y12, X23, Y23, X123, Y123, D, Da: Double;
  Pt: TPointDouble;
begin
  if Level > CCurveRecursionLimit then
    Exit;

  // Calculate all the mid-points of the line segments
  X12 := (X1 + X2) * 0.5;
  Y12 := (Y1 + Y2) * 0.5;
  X23 := (X2 + X3) * 0.5;
  Y23 := (Y2 + Y3) * 0.5;
  X123 := (X12 + X23) * 0.5;
  Y123 := (Y12 + Y23) * 0.5;

  Delta.X := X3 - X1;
  Delta.Y := Y3 - Y1;
  D := Abs((X2 - X3) * Delta.Y - (Y2 - Y3) * Delta.X);

  if D > CCurveCollinearityEpsilon then
    // Regular care
    if D * D <= FDistanceToleranceSquare * (Sqr(Delta.X) + Sqr(Delta.Y)) then
    begin
      // If the curvature doesn't exceed the DistanceTolerance value
      // we tend to finish subdivisions.
      if FAngleTolerance < CCurveAngleToleranceEpsilon then
      begin
        Pt.X := X123;
        Pt.Y := Y123;

        FPoints.Add(@Pt);

        Exit;
      end;

      // Angle & Cusp Condition
      Da := Abs(ArcTan2(Y3 - Y2, X3 - X2) - ArcTan2(Y2 - Y1, X2 - X1));

      if Da >= Pi then
        Da := 2 * Pi - Da;

      if Da < FAngleTolerance then
      begin
        // Finally we can stop the recursion
        Pt.X := X123;
        Pt.Y := Y123;

        FPoints.Add(@Pt);

        Exit;
      end;

    end
    else
  else if Abs(X1 + X3 - X2 - X2) + Abs(Y1 + Y3 - Y2 - Y2) <= FDistanceToleranceManhattan
  then
  begin
    Pt.X := X123;
    Pt.Y := Y123;

    FPoints.Add(@Pt);

    Exit;
  end;

  // Continue subdivision
  RecursiveBezier(X1, Y1, X12, Y12, X123, Y123, Level + 1);
  RecursiveBezier(X123, Y123, X23, Y23, X3, Y3, Level + 1);
end;


{ TAggCurve4Points }

procedure TAggCurve4Points.Init(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
begin
  Point[0].X := X1;
  Point[0].Y := Y1;
  Point[1].X := X2;
  Point[1].Y := Y2;
  Point[2].X := X3;
  Point[2].Y := Y3;
  Point[3].X := X4;
  Point[3].Y := Y4;
end;

procedure TAggCurve4Points.Init(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Point[0] := Point1;
  Point[1] := Point2;
  Point[2] := Point3;
  Point[3] := Point4;
end;


{ TAggCurve4Inc }

constructor TAggCurve4Inc.Create;
begin
  FNumSteps := 0;
  FStep := 0;
  FScale := 1.0;
end;

constructor TAggCurve4Inc.Create(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
begin
  Create(PointDouble(X1, Y1), PointDouble(X2, Y2), PointDouble(X3, Y3),
    PointDouble(X4, Y4));
end;

constructor TAggCurve4Inc.Create(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Create;

  Init4(Point1, Point2, Point3, Point3);
end;

constructor TAggCurve4Inc.Create(CurvePoints: PAggCurve4Points);
begin
  Create;

  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1],
    CurvePoints^.Point[2], CurvePoints^.Point[3]);
end;

procedure TAggCurve4Inc.Reset;
begin
  FNumSteps := 0;
  FStep := -1;
end;

procedure TAggCurve4Inc.Init4(Point1, Point2, Point3, Point4: TPointDouble);
var
  Delta: array [0..2] of TPointDouble;
  SubDivideStep: array [0..2] of Double;
  Len: Double;
  Pre: array [0..3] of Double;
  Temp: array [0..1] of TPointDouble;
begin
  FStart := Point1;
  FEnd := Point4;

  Delta[0].X := Point2.X - Point1.X;
  Delta[0].Y := Point2.Y - Point1.Y;
  Delta[1].X := Point3.X - Point2.X;
  Delta[1].Y := Point3.Y - Point2.Y;
  Delta[2].X := Point4.X - Point3.X;
  Delta[2].Y := Point4.Y - Point3.Y;

  Len := Hypot(Delta[0].X, Delta[0].Y) + Hypot(Delta[1].X, Delta[1].Y) +
    Hypot(Delta[2].X, Delta[2].Y);

  FNumSteps := Trunc(0.25 * Len * FScale);

  if FNumSteps < 4 then
    FNumSteps := 4;

  SubDivideStep[0] := 1 / FNumSteps;
  SubDivideStep[1] := Sqr(SubDivideStep[0]);
  SubDivideStep[2] := SubDivideStep[1] * SubDivideStep[0];

  Pre[0] := 3 * SubDivideStep[0];
  Pre[1] := 3 * SubDivideStep[1];
  Pre[2] := 6 * SubDivideStep[1];
  Pre[3] := 6 * SubDivideStep[2];

  Temp[0].X := Point1.X - Point2.X - Point2.X + Point3.X;
  Temp[0].Y := Point1.Y - Point2.Y - Point2.Y + Point3.Y;

  Temp[1].X := (Point2.X - Point3.X) * 3.0 - Point1.X + Point4.X;
  Temp[1].Y := (Point2.Y - Point3.Y) * 3.0 - Point1.Y + Point4.Y;

  FSavedF := Point1;
  FPoint := Point1;

  FSavedDelta.X := (Point2.X - Point1.X) * Pre[0] + Temp[0].X * Pre[1] +
    Temp[1].X * SubDivideStep[2];
  FSavedDelta.Y := (Point2.Y - Point1.Y) * Pre[0] + Temp[0].Y * Pre[1] +
    Temp[1].Y * SubDivideStep[2];
  FDeltaF := FSavedDelta;

  FSavedDeltaDelta.X := Temp[0].X * Pre[2] + Temp[1].X * Pre[3];
  FSavedDeltaDelta.Y := Temp[0].Y * Pre[2] + Temp[1].Y * Pre[3];
  FDeltaDelta := FSavedDeltaDelta;

  FDeltaDeltaDelta.X := Temp[1].X * Pre[3];
  FDeltaDeltaDelta.Y := Temp[1].Y * Pre[3];

  FStep := FNumSteps;
end;

procedure TAggCurve4Inc.Init(CurvePoints: PAggCurve4Points);
begin
  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1],
    CurvePoints^.Point[2], CurvePoints^.Point[3]);
end;

function TAggCurve4Inc.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camInc;
end;

procedure TAggCurve4Inc.SetAngleTolerance(Value: Double);
begin
end;

procedure TAggCurve4Inc.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
end;

procedure TAggCurve4Inc.SetApproximationScale(Value: Double);
begin
  FScale := Value;
end;

procedure TAggCurve4Inc.SetCuspLimit(Value: Double);
begin
end;

function TAggCurve4Inc.GetApproximationScale: Double;
begin
  Result := FScale;
end;

function TAggCurve4Inc.GetAngleTolerance: Double;
begin
  Result := 0;
end;

function TAggCurve4Inc.GetCuspLimit: Double;
begin
  Result := 0;
end;

procedure TAggCurve4Inc.Rewind(PathID: Cardinal);
begin
  if FNumSteps = 0 then
  begin
    FStep := -1;

    Exit;
  end;

  FStep := FNumSteps;
  FPoint := FSavedF;
  FDeltaF := FSavedDelta;
  FDeltaDelta := FSavedDeltaDelta;
end;

function TAggCurve4Inc.Vertex(X, Y: PDouble): Cardinal;
begin
  if FStep < 0 then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  if FStep = FNumSteps then
  begin
    X^ := FStart.X;
    Y^ := FStart.Y;

    Dec(FStep);

    Result := CAggPathCmdMoveTo;

    Exit;
  end;

  if FStep = 0 then
  begin
    X^ := FEnd.X;
    Y^ := FEnd.Y;

    Dec(FStep);

    Result := CAggPathCmdLineTo;

    Exit;
  end;

  FPoint.X := FPoint.X + FDeltaF.X;
  FPoint.Y := FPoint.Y + FDeltaF.Y;
  FDeltaF.X := FDeltaF.X + FDeltaDelta.X;
  FDeltaF.Y := FDeltaF.Y + FDeltaDelta.Y;
  FDeltaDelta.X := FDeltaDelta.X + FDeltaDeltaDelta.X;
  FDeltaDelta.Y := FDeltaDelta.Y + FDeltaDeltaDelta.Y;

  X^ := FPoint.X;
  Y^ := FPoint.Y;

  Dec(FStep);

  Result := CAggPathCmdLineTo;
end;


{ TAggCurve4Div }

constructor TAggCurve4Div.Create;
begin
  FPoints := TAggPodDeque.Create(SizeOf(TPointDouble));

  FApproximationScale := 1;
  FAngleTolerance := 0;

  FCuspLimit := 0;
  FCount := 0;
end;

constructor TAggCurve4Div.Create(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
begin
  Create(PointDouble(X1, Y1), PointDouble(X2, Y2), PointDouble(X3, Y3),
    PointDouble(X4, Y4));
end;

constructor TAggCurve4Div.Create(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Create;

  Init4(Point1, Point2, Point3, Point4);
end;

constructor TAggCurve4Div.Create(CurvePoints: PAggCurve4Points);
begin
  Create;

  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

destructor TAggCurve4Div.Destroy;
begin
  FPoints.Free;
  inherited
end;

procedure TAggCurve4Div.Reset;
begin
  FPoints.RemoveAll;

  FCount := 0;
end;

procedure TAggCurve4Div.Init4(Point1, Point2, Point3, Point4: TPointDouble);
begin
  FPoints.RemoveAll;

  FDistanceToleranceSquare := Sqr(0.5 / FApproximationScale);
  FDistanceToleranceManhattan := 4 / FApproximationScale;

  Bezier(Point1, Point2, Point3, Point4);

  FCount := 0;
end;

procedure TAggCurve4Div.Init;
begin
  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

procedure TAggCurve4Div.SetApproximationMethod(Value: TAggCurveApproximationMethod);
begin
end;

function TAggCurve4Div.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camDiv;
end;

procedure TAggCurve4Div.SetApproximationScale(Value: Double);
begin
  if Value = 0 then
    FApproximationScale := 0.00001
  else
    FApproximationScale := Value;
end;

function TAggCurve4Div.GetApproximationScale: Double;
begin
  Result := FApproximationScale;
end;

procedure TAggCurve4Div.SetAngleTolerance(Value: Double);
begin
  FAngleTolerance := Value;
end;

function TAggCurve4Div.GetAngleTolerance: Double;
begin
  Result := FAngleTolerance;
end;

procedure TAggCurve4Div.SetCuspLimit(Value: Double);
begin
  if Value = 0.0 then
    FCuspLimit := 0
  else
    FCuspLimit := Pi - Value;
end;

function TAggCurve4Div.GetCuspLimit: Double;
begin
  if FCuspLimit = 0.0 then
    Result := 0
  else
    Result := Pi - FCuspLimit;
end;

procedure TAggCurve4Div.Rewind(PathID: Cardinal);
begin
  FCount := 0;
end;

function TAggCurve4Div.Vertex(X, Y: PDouble): Cardinal;
var
  P: PPointDouble;
begin
  if FCount >= FPoints.Size then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  P := FPoints[FCount];

  Inc(FCount);

  X^ := P.X;
  Y^ := P.Y;

  if FCount = 1 then
    Result := CAggPathCmdMoveTo
  else
    Result := CAggPathCmdLineTo;
end;

procedure TAggCurve4Div.Bezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
var
  Pt: TPointDouble;
begin
  Pt := PointDouble(X1, Y1);
  FPoints.Add(@Pt);

  RecursiveBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4, 0);

  Pt := PointDouble(X4, Y4);
  FPoints.Add(@Pt);
end;

procedure TAggCurve4Div.Bezier(Point1, Point2, Point3, Point4: TPointDouble);
begin
  FPoints.Add(@Point1);

  RecursiveBezier(Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y,
    Point4.X, Point4.Y, 0);

  FPoints.Add(@Point4);
end;

procedure TAggCurve4Div.RecursiveBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double;
  Level: Cardinal);
var
  X12, Y12,
  X23, Y23,
  X34, Y34,
  X123, Y123,
  X234, Y234,
  X1234, Y1234: Double;
  Delta : TPointDouble;
  D2, D3,
  Da1, Da2, A23: Double;

  Pt: TPointDouble;
begin
  if Level > CCurveRecursionLimit then
    Exit;

  // Calculate all the mid-points of the line segments
  X12 := (X1 + X2) * 0.5;
  Y12 := (Y1 + Y2) * 0.5;
  X23 := (X2 + X3) * 0.5;
  Y23 := (Y2 + Y3) * 0.5;
  X34 := (X3 + X4) * 0.5;
  Y34 := (Y3 + Y4) * 0.5;
  X123 := (X12 + X23) * 0.5;
  Y123 := (Y12 + Y23) * 0.5;
  X234 := (X23 + X34) * 0.5;
  Y234 := (Y23 + Y34) * 0.5;
  X1234 := (X123 + X234) * 0.5;
  Y1234 := (Y123 + Y234) * 0.5;

  // Try to approximate the full cubic TCurve by Value single straight line
  Delta.X := X4 - X1;
  Delta.Y := Y4 - Y1;

  D2 := Abs((X2 - X4) * Delta.Y - (Y2 - Y4) * Delta.X);
  D3 := Abs((X3 - X4) * Delta.Y - (Y3 - Y4) * Delta.X);

  case ((Integer(D2 > CCurveCollinearityEpsilon) shl 1) +
    Integer(D3 > CCurveCollinearityEpsilon)) of
    // All collinear OR p1==p4
    0:
      if Abs(X1 + X3 - X2 - X2) + Abs(Y1 + Y3 - Y2 - Y2) +
        Abs(X2 + X4 - X3 - X3) + Abs(Y2 + Y4 - Y3 - Y3) <= FDistanceToleranceManhattan
      then
      begin
        Pt.X := X1234;
        Pt.Y := Y1234;

        FPoints.Add(@Pt);

        Exit;
      end;

    // p1,p2,p4 are collinear, p3 is considerable
    1:
      if Sqr(D3) <= FDistanceToleranceSquare * (Sqr(Delta.X) + Sqr(Delta.Y)) then
      begin
        if FAngleTolerance < CCurveAngleToleranceEpsilon then
        begin
          Pt.X := X23;
          Pt.Y := Y23;

          FPoints.Add(@Pt);

          Exit;
        end;

        // Angle Condition
        Da1 := Abs(ArcTan2(Y4 - Y3, X4 - X3) - ArcTan2(Y3 - Y2, X3 - X2));

        if Da1 >= Pi then
          Da1 := 2 * Pi - Da1;

        if Da1 < FAngleTolerance then
        begin
          Pt.X := X2;
          Pt.Y := Y2;

          FPoints.Add(@Pt);

          Pt.X := X3;
          Pt.Y := Y3;

          FPoints.Add(@Pt);

          Exit;
        end;

        if FCuspLimit <> 0.0 then
          if Da1 > FCuspLimit then
          begin
            Pt.X := X3;
            Pt.Y := Y3;

            FPoints.Add(@Pt);

            Exit;
          end;
      end;

    // p1,p3,p4 are collinear, p2 is considerable
    2:
      if Sqr(D2) <= FDistanceToleranceSquare * (Sqr(Delta.X) + Sqr(Delta.Y)) then
      begin
        if FAngleTolerance < CCurveAngleToleranceEpsilon then
        begin
          Pt.X := X23;
          Pt.Y := Y23;

          FPoints.Add(@Pt);

          Exit;
        end;

        // Angle Condition
        Da1 := Abs(ArcTan2(Y3 - Y2, X3 - X2) - ArcTan2(Y2 - Y1, X2 - X1));

        if Da1 >= Pi then
          Da1 := 2 * Pi - Da1;

        if Da1 < FAngleTolerance then
        begin
          Pt.X := X2;
          Pt.Y := Y2;

          FPoints.Add(@Pt);

          Pt.X := X3;
          Pt.Y := Y3;

          FPoints.Add(@Pt);

          Exit;
        end;

        if FCuspLimit <> 0.0 then
          if Da1 > FCuspLimit then
          begin
            Pt.X := X2;
            Pt.Y := Y2;

            FPoints.Add(@Pt);

            Exit;
          end;
      end;

    // Regular care
    3:
      if (D2 + D3) * (D2 + D3) <= FDistanceToleranceSquare *
        (Delta.X * Delta.X + Delta.Y * Delta.Y) then
      begin
        // If the curvature doesn't exceed the DistanceTolerance value
        // we tend to finish subdivisions.
        if FAngleTolerance < CCurveAngleToleranceEpsilon then
        begin
          Pt.X := X23;
          Pt.Y := Y23;

          FPoints.Add(@Pt);

          Exit;
        end;

        // Angle & Cusp Condition
        A23 := ArcTan2(Y3 - Y2, X3 - X2);
        Da1 := Abs(A23 - ArcTan2(Y2 - Y1, X2 - X1));
        Da2 := Abs(ArcTan2(Y4 - Y3, X4 - X3) - A23);

        if Da1 >= Pi then
          Da1 := 2 * Pi - Da1;

        if Da2 >= Pi then
          Da2 := 2 * Pi - Da2;

        if Da1 + Da2 < FAngleTolerance then
        begin
          // Finally we can stop the recursion
          Pt.X := X23;
          Pt.Y := Y23;

          FPoints.Add(@Pt);

          Exit;
        end;

        if FCuspLimit <> 0.0 then
        begin
          if Da1 > FCuspLimit then
          begin
            Pt.X := X2;
            Pt.Y := Y2;

            FPoints.Add(@Pt);

            Exit;
          end;

          if Da2 > FCuspLimit then
          begin
            Pt.X := X3;
            Pt.Y := Y3;

            FPoints.Add(@Pt);

            Exit;
          end;
        end;
      end;
  end;

  // Continue subdivision
  RecursiveBezier(X1, Y1, X12, Y12, X123, Y123, X1234, Y1234, Level + 1);
  RecursiveBezier(X1234, Y1234, X234, Y234, X34, Y34, X4, Y4, Level + 1);
end;


{ TAggCurve3 }

constructor TAggCurve3.Create;
begin
  FCurveInc := TAggCurve3Inc.Create;
  FCurveDiv := TAggCurve3Div.Create;

  FApproximationMethod := camDiv;
end;

constructor TAggCurve3.Create(Point1, Point2, Point3: TPointDouble);
begin
  Create;

  Init3(Point1, Point2, Point3);
end;

constructor TAggCurve3.Create(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  Create(PointDouble(X1, Y1), PointDouble(X2, Y2), PointDouble(X3, Y3));
end;

constructor TAggCurve3.Create(CurvePoints: PAggCurve3Points);
begin
  Create;

  Init3(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2]);
end;

destructor TAggCurve3.Destroy;
begin
  FCurveInc.Free;
  FCurveDiv.Free;

  inherited;
end;

procedure TAggCurve3.Reset;
begin
  FCurveInc.Reset;
  FCurveDiv.Reset;
end;

procedure TAggCurve3.Init3(Point1, Point2, Point3: TPointDouble);
begin
  if FApproximationMethod = camInc then
    FCurveInc.Init3(Point1, Point2, Point3)
  else
    FCurveDiv.Init3(Point1, Point2, Point3);
end;

procedure TAggCurve3.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
  FApproximationMethod := Value;
end;

function TAggCurve3.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := FApproximationMethod;
end;

procedure TAggCurve3.SetApproximationScale(Value: Double);
begin
  FCurveInc.SetApproximationScale(Value);
  FCurveDiv.SetApproximationScale(Value);
end;

function TAggCurve3.GetApproximationScale: Double;
begin
  Result := FCurveInc.GetApproximationScale;
end;

procedure TAggCurve3.SetAngleTolerance(Value: Double);
begin
  FCurveDiv.SetAngleTolerance(Value);
end;

function TAggCurve3.GetAngleTolerance: Double;
begin
  Result := FCurveDiv.GetAngleTolerance;
end;

procedure TAggCurve3.SetCuspLimit(Value: Double);
begin
  FCurveDiv.SetCuspLimit(Value);
end;

function TAggCurve3.GetCuspLimit: Double;
begin
  Result := FCurveDiv.GetCuspLimit;
end;

procedure TAggCurve3.Rewind(PathID: Cardinal);
begin
  if FApproximationMethod = camInc then
    FCurveInc.Rewind(PathID)
  else
    FCurveDiv.Rewind(PathID)
end;

function TAggCurve3.Vertex(X, Y: PDouble): Cardinal;
begin
  if FApproximationMethod = camInc then
    Result := FCurveInc.Vertex(X, Y)
  else
    Result := FCurveDiv.Vertex(X, Y);
end;


{ TAggCurve4 }

constructor TAggCurve4.Create;
begin
  FCurveInc := TAggCurve4Inc.Create;
  FCurveDiv := TAggCurve4Div.Create;

  FApproximationMethod := camDiv;
end;

constructor TAggCurve4.Create(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
begin
  Create(PointDouble(X1, Y1), PointDouble(X2, Y2), PointDouble(X3, Y3),
    PointDouble(X4, Y4));
end;

constructor TAggCurve4.Create(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Create;

  Init4(Point1, Point2, Point3, Point4);
end;

constructor TAggCurve4.Create(CurvePoints: PAggCurve4Points);
begin
  Create;

  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

destructor TAggCurve4.Destroy;
begin
  FCurveInc.Free;
  FCurveDiv.Free;

  inherited;
end;

procedure TAggCurve4.Reset;
begin
  FCurveInc.Reset;
  FCurveDiv.Reset;
end;

procedure TAggCurve4.Init4(Point1, Point2, Point3, Point4: TPointDouble);
begin
  if FApproximationMethod = camInc then
    FCurveInc.Init4(Point1, Point2, Point3, Point4)
  else
    FCurveDiv.Init4(Point1, Point2, Point3, Point4);
end;

procedure TAggCurve4.Init(CurvePoints: PAggCurve4Points);
begin
  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

procedure TAggCurve4.SetApproximationMethod(Value: TAggCurveApproximationMethod);
begin
  FApproximationMethod := Value;
end;

function TAggCurve4.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := FApproximationMethod;
end;

procedure TAggCurve4.SetApproximationScale(Value: Double);
begin
  FCurveInc.SetApproximationScale(Value);
  FCurveDiv.SetApproximationScale(Value);
end;

function TAggCurve4.GetApproximationScale: Double;
begin
  Result := FCurveInc.GetApproximationScale;
end;

procedure TAggCurve4.SetAngleTolerance(Value: Double);
begin
  FCurveDiv.SetAngleTolerance(Value);
end;

function TAggCurve4.GetAngleTolerance: Double;
begin
  Result := FCurveDiv.GetAngleTolerance;
end;

procedure TAggCurve4.SetCuspLimit(Value: Double);
begin
  FCurveDiv.SetCuspLimit(Value);
end;

function TAggCurve4.GetCuspLimit: Double;
begin
  Result := FCurveDiv.GetCuspLimit;
end;

procedure TAggCurve4.Rewind(PathID: Cardinal);
begin
  if FApproximationMethod = camInc then
    FCurveInc.Rewind(PathID)
  else
    FCurveDiv.Rewind(PathID);
end;

function TAggCurve4.Vertex(X, Y: PDouble): Cardinal;
begin
  if FApproximationMethod = camInc then
    Result := FCurveInc.Vertex(X, Y)
  else
    Result := FCurveDiv.Vertex(X, Y);
end;

end.
