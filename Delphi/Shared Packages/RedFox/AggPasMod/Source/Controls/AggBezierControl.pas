unit AggBezierControl;

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
  AggBasics,
  AggControl,
  AggMath,
  AggEllipse,
  AggTransAffine,
  AggColor,
  AggCurves,
  AggConvStroke,
  AggConvCurve,
  AggPolygonControl;

type
  TAggCustomBezierControl = class(TAggCustomAggControl)
  private
    FCurve: TAggCurve4;
    FCircle: TAggCircle;
    FStroke: TAggConvStroke;
    FPoly: TAggCustomPolygonControl;
    FIndex: Cardinal;
    function GetLineWidth: Double;
    function GetPointRadius: Double;
    procedure SetLineWidth(Value: Double);
    procedure SetPointRadius(Value: Double);

    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;
    function GetX3: Double;
    function GetY3: Double;
    function GetX4: Double;
    function GetY4: Double;

    procedure SetX1(X: Double);
    procedure SetY1(Y: Double);
    procedure SetX2(X: Double);
    procedure SetY2(Y: Double);
    procedure SetX3(X: Double);
    procedure SetY3(Y: Double);
    procedure SetX4(X: Double);
    procedure SetY4(Y: Double);
    function GetPoint1: TPointDouble;
    function GetPoint2: TPointDouble;
    function GetPoint3: TPointDouble;
    function GetPoint4: TPointDouble;
    procedure SetPoint1(const Value: TPointDouble);
    procedure SetPoint2(const Value: TPointDouble);
    procedure SetPoint3(const Value: TPointDouble);
    procedure SetPoint4(const Value: TPointDouble);
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetCurve(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;
    procedure SetCurve(Point1, Point2, Point3, Point4: TPointDouble); overload;
    function GetCurve: TAggCurve4;

    // Event handlers
    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property PointRadius: Double read GetPointRadius write SetPointRadius;

    property X1: Double read GetX1 write SetX1;
    property Y1: Double read GetY1 write SetY1;
    property X2: Double read GetX2 write SetX2;
    property Y2: Double read GetY2 write SetY2;
    property X3: Double read GetX3 write SetX3;
    property Y3: Double read GetY3 write SetY3;
    property X4: Double read GetX4 write SetX4;
    property Y4: Double read GetY4 write SetY4;

    property Point1: TPointDouble read GetPoint1 write SetPoint1;
    property Point2: TPointDouble read GetPoint2 write SetPoint2;
    property Point3: TPointDouble read GetPoint3 write SetPoint3;
    property Point4: TPointDouble read GetPoint4 write SetPoint4;
  end;

  TBezierControl = class(TAggCustomBezierControl)
  private
    FColor: TAggColor;
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create;
    procedure SetLineColor(C: PAggColor);
  end;

  TAggCustomCurve3Control = class(TAggCustomAggControl)
  private
    FCurve: TAggCurve3;
    FCircle: TAggCircle;
    FStroke: TAggConvStroke;
    FPoly: TAggCustomPolygonControl;
    FIndex: Cardinal;
    function GetLineWidth: Double;
    function GetPointRadius: Double;
    procedure SetLineWidth(Value: Double);
    procedure SetPointRadius(Value: Double);
    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;
    function GetX3: Double;
    function GetY3: Double;

    procedure SetX1(X: Double);
    procedure SetY1(Y: Double);
    procedure SetX2(X: Double);
    procedure SetY2(Y: Double);
    procedure SetX3(X: Double);
    procedure SetY3(Y: Double);
    function GetPoint1: TPointDouble;
    function GetPoint2: TPointDouble;
    function GetPoint3: TPointDouble;
    procedure SetPoint1(const Value: TPointDouble);
    procedure SetPoint2(const Value: TPointDouble);
    procedure SetPoint3(const Value: TPointDouble);
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetCurve(X1, Y1, X2, Y2, X3, Y3: Double);
    function GetCurve: TAggCurve3;

    // Event handlers
    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property PointRadius: Double read GetPointRadius write SetPointRadius;

    property X1: Double read GetX1 write SetX1;
    property Y1: Double read GetY1 write SetY1;
    property X2: Double read GetX2 write SetX2;
    property Y2: Double read GetY2 write SetY2;
    property X3: Double read GetX3 write SetX3;
    property Y3: Double read GetY3 write SetY3;

    property Point1: TPointDouble read GetPoint1 write SetPoint1;
    property Point2: TPointDouble read GetPoint2 write SetPoint2;
    property Point3: TPointDouble read GetPoint3 write SetPoint3;
  end;

  TAggCurve3Control = class(TAggCustomCurve3Control)
  private
    FColor: TAggColor;
    procedure SetLineColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create;
    property LineColor: TAggColor read FColor write SetLineColor;
  end;

implementation


{ TAggCustomBezierControl }

constructor TAggCustomBezierControl.Create;
begin
  inherited Create(0, 0, 1, 1, False);

  FCurve := TAggCurve4.Create;
  FCircle := TAggCircle.Create;
  FStroke := TAggConvStroke.Create(FCurve);
  FPoly := TAggCustomPolygonControl.Create(4, 5.0);

  FIndex := 0;

  FPoly.InPolygonCheck := False;

  FPoly.Xn[0] := 100.0;
  FPoly.Yn[0] := 0.0;
  FPoly.Xn[1] := 100.0;
  FPoly.Yn[1] := 50.0;
  FPoly.Xn[2] := 50.0;
  FPoly.Yn[2] := 100.0;
  FPoly.Xn[3] := 0.0;
  FPoly.Yn[3] := 100.0;
end;

destructor TAggCustomBezierControl.Destroy;
begin
  FCircle.Free;
  FCurve.Free;
  FStroke.Free;
  FPoly.Free;
  inherited;
end;

procedure TAggCustomBezierControl.SetCurve(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
begin
  FPoly.Xn[0] := X1;
  FPoly.Yn[0] := Y1;
  FPoly.Xn[1] := X2;
  FPoly.Yn[1] := Y2;
  FPoly.Xn[2] := X3;
  FPoly.Yn[2] := Y3;
  FPoly.Xn[3] := X4;
  FPoly.Yn[3] := Y4;

  GetCurve;
end;

procedure TAggCustomBezierControl.SetCurve(Point1, Point2, Point3,
  Point4: TPointDouble);
begin
  FPoly.Point[0] := Point1;
  FPoly.Point[1] := Point2;
  FPoly.Point[2] := Point3;
  FPoly.Point[3] := Point4;

  GetCurve;
end;

function TAggCustomBezierControl.GetCurve: TAggCurve4;
begin
  FCurve.Init4(FPoly.Point[0], FPoly.Point[1], FPoly.Point[2], FPoly.Point[3]);

  Result := FCurve;
end;

function TAggCustomBezierControl.GetX1: Double;
begin
  Result := FPoly.Xn[0];
end;

function TAggCustomBezierControl.GetY1: Double;
begin
  Result := FPoly.Yn[0];
end;

function TAggCustomBezierControl.GetX2: Double;
begin
  Result := FPoly.Xn[1];
end;

function TAggCustomBezierControl.GetY2: Double;
begin
  Result := FPoly.Yn[1];
end;

function TAggCustomBezierControl.GetX3: Double;
begin
  Result := FPoly.Xn[2];
end;

function TAggCustomBezierControl.GetY3: Double;
begin
  Result := FPoly.Yn[2];
end;

function TAggCustomBezierControl.GetX4: Double;
begin
  Result := FPoly.Xn[3];
end;

function TAggCustomBezierControl.GetY4: Double;
begin
  Result := FPoly.Yn[3];
end;

procedure TAggCustomBezierControl.SetX1;
begin
  FPoly.Xn[0] := X;
end;

procedure TAggCustomBezierControl.SetY1(Y: Double);
begin
  FPoly.Yn[0] := Y;
end;

procedure TAggCustomBezierControl.SetX2;
begin
  FPoly.Xn[1] := X;
end;

procedure TAggCustomBezierControl.SetY2(Y: Double);
begin
  FPoly.Yn[1] := Y;
end;

procedure TAggCustomBezierControl.SetX3;
begin
  FPoly.Xn[2] := X;
end;

procedure TAggCustomBezierControl.SetY3(Y: Double);
begin
  FPoly.Yn[2] := Y;
end;

procedure TAggCustomBezierControl.SetX4;
begin
  FPoly.Xn[3] := X;
end;

procedure TAggCustomBezierControl.SetY4(Y: Double);
begin
  FPoly.Yn[3] := Y;
end;

procedure TAggCustomBezierControl.SetLineWidth(Value: Double);
begin
  FStroke.Width := Value;
end;

function TAggCustomBezierControl.GetLineWidth: Double;
begin
  Result := FStroke.Width;
end;

procedure TAggCustomBezierControl.SetPoint1(const Value: TPointDouble);
begin
  FPoly.Point[0] := Value
end;

procedure TAggCustomBezierControl.SetPoint2(const Value: TPointDouble);
begin
  FPoly.Point[1] := Value
end;

procedure TAggCustomBezierControl.SetPoint3(const Value: TPointDouble);
begin
  FPoly.Point[2] := Value
end;

procedure TAggCustomBezierControl.SetPoint4(const Value: TPointDouble);
begin
  FPoly.Point[3] := Value
end;

procedure TAggCustomBezierControl.SetPointRadius(Value: Double);
begin
  FPoly.PointRadius := Value;
end;

function TAggCustomBezierControl.GetPoint1: TPointDouble;
begin
  Result := FPoly.Point[0];
end;

function TAggCustomBezierControl.GetPoint2: TPointDouble;
begin
  Result := FPoly.Point[1];
end;

function TAggCustomBezierControl.GetPoint3: TPointDouble;
begin
  Result := FPoly.Point[2];
end;

function TAggCustomBezierControl.GetPoint4: TPointDouble;
begin
  Result := FPoly.Point[3];
end;

function TAggCustomBezierControl.GetPointRadius: Double;
begin
  Result := FPoly.PointRadius;
end;

function TAggCustomBezierControl.InRect(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomBezierControl.OnMouseButtonDown(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := FPoly.OnMouseButtonDown(X, Y);
end;

function TAggCustomBezierControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := FPoly.OnMouseButtonUp(X, Y);
end;

function TAggCustomBezierControl.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := FPoly.OnMouseMove(X, Y, ButtonFlag);
end;

function TAggCustomBezierControl.OnArrowKeys(Left, Right, Down,
  Up: Boolean): Boolean;
begin
  Result := FPoly.OnArrowKeys(Left, Right, Down, Up);
end;

function TAggCustomBezierControl.GetPathCount: Cardinal;
begin
  Result := 7;
end;

procedure TAggCustomBezierControl.Rewind(PathID: Cardinal);
begin
  FIndex := PathID;

  FCurve.ApproximationScale := Scale;

  case PathID of
    0: // Control line 1
      begin
        FCurve.Init4(FPoly.Point[0],
          PointDouble((FPoly.Xn[0] + FPoly.Xn[1]) * 0.5,
            (FPoly.Yn[0] + FPoly.Yn[1]) * 0.5),
          PointDouble((FPoly.Xn[0] + FPoly.Xn[1]) * 0.5,
            (FPoly.Yn[0] + FPoly.Yn[1]) * 0.5), FPoly.Point[1]);

        FStroke.Rewind(0);
      end;

    1: // Control line 2
      begin
        FCurve.Init4(FPoly.Point[2],
          PointDouble((FPoly.Xn[2] + FPoly.Xn[3]) * 0.5,
            (FPoly.Yn[2] + FPoly.Yn[3]) * 0.5),
          PointDouble((FPoly.Xn[2] + FPoly.Xn[3]) * 0.5,
            (FPoly.Yn[2] + FPoly.Yn[3]) * 0.5),
          FPoly.Point[3]);

        FStroke.Rewind(0);
      end;

    2: // Curve itself
      begin
        FCurve.Init4(FPoly.Point[0], FPoly.Point[1], FPoly.Point[2],
          FPoly.Point[3]);

        FStroke.Rewind(0);
      end;

    3: // Point 1
      begin
        FCircle.Initialize(FPoly.Point[0], PointRadius, 20);
        FCircle.Rewind(0);
      end;

    4: // Point 2
      begin
        FCircle.Initialize(FPoly.Point[1], PointRadius, 20);
        FCircle.Rewind(0);
      end;

    5: // Point 3
      begin
        FCircle.Initialize(FPoly.Point[2], PointRadius, 20);
        FCircle.Rewind(0);
      end;

    6: // Point 4
      begin
        FCircle.Initialize(FPoly.Point[3], PointRadius, 20);
        FCircle.Rewind(0);
      end;
  end;
end;

function TAggCustomBezierControl.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdStop;

  case FIndex of
    0, 1, 2:
      Cmd := FStroke.Vertex(X, Y);

    3, 4, 5, 6, 7:
      Cmd := FCircle.Vertex(X, Y);
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;


{ TBezierControl }

constructor TBezierControl.Create;
begin
  inherited Create;

  FColor.Black;
end;

procedure TBezierControl.SetLineColor;
begin
  FColor := C^;
end;

function TBezierControl.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := @FColor;
end;


{ TAggCustomCurve3Control }

constructor TAggCustomCurve3Control.Create;
begin
  inherited Create(0, 0, 1, 1, False);

  FCurve := TAggCurve3.Create;
  FCircle := TAggCircle.Create;
  FStroke := TAggConvStroke.Create(FCurve);
  FPoly := TPolygonControl.Create(3, 5.0);

  FIndex := 0;

  FPoly.InPolygonCheck := False;

  FPoly.Xn[0] := 100.0;
  FPoly.Yn[0] := 0.0;
  FPoly.Xn[1] := 100.0;
  FPoly.Yn[1] := 50.0;
  FPoly.Xn[2] := 50.0;
  FPoly.Yn[2] := 100.0;
end;

destructor TAggCustomCurve3Control.Destroy;
begin
  FCircle.Free;
  FCurve.Free;
  FStroke.Free;
  FPoly.Free;
  inherited;
end;

procedure TAggCustomCurve3Control.SetCurve;
begin
  FPoly.Xn[0] := X1;
  FPoly.Yn[0] := Y1;
  FPoly.Xn[1] := X2;
  FPoly.Yn[1] := Y2;
  FPoly.Xn[2] := X3;
  FPoly.Yn[2] := Y3;

  GetCurve;
end;

function TAggCustomCurve3Control.GetCurve: TAggCurve3;
begin
  FCurve.Init3(FPoly.Point[0], FPoly.Point[1], FPoly.Point[2]);

  Result := @FCurve;
end;

function TAggCustomCurve3Control.GetX1: Double;
begin
  Result := FPoly.Xn[0];
end;

function TAggCustomCurve3Control.GetY1: Double;
begin
  Result := FPoly.Yn[0];
end;

function TAggCustomCurve3Control.GetX2: Double;
begin
  Result := FPoly.Xn[1];
end;

function TAggCustomCurve3Control.GetY2: Double;
begin
  Result := FPoly.Yn[1];
end;

function TAggCustomCurve3Control.GetX3: Double;
begin
  Result := FPoly.Xn[2];
end;

function TAggCustomCurve3Control.GetY3: Double;
begin
  Result := FPoly.Yn[2];
end;

function TAggCustomCurve3Control.GetPoint1: TPointDouble;
begin
  Result := FPoly.Point[0];
end;

function TAggCustomCurve3Control.GetPoint2: TPointDouble;
begin
  Result := FPoly.Point[1];
end;

function TAggCustomCurve3Control.GetPoint3: TPointDouble;
begin
  Result := FPoly.Point[2];
end;

procedure TAggCustomCurve3Control.SetX1(X: Double);
begin
  FPoly.Xn[0] := X;
end;

procedure TAggCustomCurve3Control.SetY1(Y: Double);
begin
  FPoly.Yn[0] := Y;
end;

procedure TAggCustomCurve3Control.SetX2(X: Double);
begin
  FPoly.Xn[1] := X;
end;

procedure TAggCustomCurve3Control.SetY2(Y: Double);
begin
  FPoly.Yn[1] := Y;
end;

procedure TAggCustomCurve3Control.SetX3(X: Double);
begin
  FPoly.Xn[2] := X;
end;

procedure TAggCustomCurve3Control.SetY3(Y: Double);
begin
  FPoly.Yn[2] := Y;
end;

procedure TAggCustomCurve3Control.SetPoint1(const Value: TPointDouble);
begin
  FPoly.Point[0] := Value;
end;

procedure TAggCustomCurve3Control.SetPoint2(const Value: TPointDouble);
begin
  FPoly.Point[1] := Value;
end;

procedure TAggCustomCurve3Control.SetPoint3(const Value: TPointDouble);
begin
  FPoly.Point[2] := Value;
end;

procedure TAggCustomCurve3Control.SetLineWidth(Value: Double);
begin
  FStroke.Width := Value;
end;

function TAggCustomCurve3Control.GetLineWidth: Double;
begin
  Result := FStroke.Width;
end;

procedure TAggCustomCurve3Control.SetPointRadius(Value: Double);
begin
  FPoly.PointRadius := Value;
end;

function TAggCustomCurve3Control.GetPointRadius: Double;
begin
  Result := FPoly.PointRadius;
end;

function TAggCustomCurve3Control.InRect(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomCurve3Control.OnMouseButtonDown(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := FPoly.OnMouseButtonDown(X, Y);
end;

function TAggCustomCurve3Control.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := FPoly.OnMouseButtonUp(X, Y);
end;

function TAggCustomCurve3Control.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := FPoly.OnMouseMove(X, Y, ButtonFlag);
end;

function TAggCustomCurve3Control.OnArrowKeys(Left, Right, Down,
  Up: Boolean): Boolean;
begin
  Result := FPoly.OnArrowKeys(Left, Right, Down, Up);
end;

function TAggCustomCurve3Control.GetPathCount: Cardinal;
begin
  Result := 6;
end;

procedure TAggCustomCurve3Control.Rewind(PathID: Cardinal);
var
  Point: array [0..1] of TPointDouble;
begin
  FIndex := PathID;

  case PathID of
    0: // Control line
      begin
        Point[0] := FPoly.Point[0];
        Point[1] := FPoly.Point[1];
        FCurve.Init3(Point[0], PointDouble((Point[0].X + Point[1].X) * 0.5,
          (Point[0].Y + Point[1].Y) * 0.5), FPoly.Point[1]);

        FStroke.Rewind(0);
      end;

    1: // Control line 2
      begin
        Point[0] := FPoly.Point[1];
        Point[1] := FPoly.Point[2];
        FCurve.Init3(Point[0], PointDouble((Point[0].X + Point[1].X) * 0.5,
          (Point[0].Y + Point[1].Y) * 0.5), FPoly.Point[1]);

        FStroke.Rewind(0);
      end;

    2: // Curve itself
      begin
        FCurve.Init3(FPoly.Point[0], FPoly.Point[1], FPoly.Point[2]);

        FStroke.Rewind(0);
      end;

    3: // Point 1
      begin
        FCircle.Initialize(FPoly.Point[0], PointRadius, 20);
        FCircle.Rewind(0);
      end;

    4: // Point 2
      begin
        FCircle.Initialize(FPoly.Point[1], PointRadius, 20);
        FCircle.Rewind(0);
      end;

    5: // Point 3
      begin
        FCircle.Initialize(FPoly.Point[2], PointRadius, 20);
        FCircle.Rewind(0);
      end;
  end;
end;

function TAggCustomCurve3Control.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdStop;

  case FIndex of
    0, 1, 2:
      Cmd := FStroke.Vertex(X, Y);

    3, 4, 5, 6:
      Cmd := FCircle.Vertex(X, Y);
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;


{ TAggCurve3Control }

constructor TAggCurve3Control.Create;
begin
  inherited Create;

  FColor.Black;
end;

procedure TAggCurve3Control.SetLineColor(Value: TAggColor);
begin
  FColor := Value;
end;

function TAggCurve3Control.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := @FColor;
end;

end.
