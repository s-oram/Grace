unit AggSplineControl;

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
//                                                                            //
// classes CustomSplineControl, SplineControl                                 //
//                                                                            //
// Class that can be used to create an interactive control to set up          //
// Gamma arrays.                                                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  AggBasics,
  AggEllipse,
  AggBSpline,
  AggConvStroke,
  AggPathStorage,
  AggTransAffine,
  AggColor,
  AggControl,
  AggMath;

type
  TAggCustomSplineControl = class(TAggCustomAggControl)
  private
    FNumPoints: Cardinal;

    FPoints: array [0..31] of TPointDouble; // 2 x Quad ?

    FSpline: TAggBSpline;

    FSplineValues: array [0..255] of Double;
    FSplineValues8: array [0..255] of Int8u;
    FBorderWidth, FBorderExtra, FCurveWidth, FPointSize: Double;

    FXS1, FYS1, FXS2, FYS2: Double;

    FCurvePnt: TAggPathStorage;
    FCurvePoly: TAggConvStroke;
    FCircle: TAggCircle;

    FIndex, FVertex: Cardinal;

    FVX, FVY: array [0..8] of Double;

    FActivePoint, FMovePoint: Integer;

    FPDX, FPDY: Double;

    // Private
    procedure CalcSplineBox;
    procedure CalcCurve;
    function CalcPoint(Index: Cardinal): TPointDouble;
    function CalcXp(Index: Cardinal): Double;
    function CalcYp(Index: Cardinal): Double;
    procedure SetXp(Index: Cardinal; Val: Double);
    procedure SetYp(Index: Cardinal; Val: Double);

    procedure SetX(Index: Cardinal; X: Double);
    procedure SetY(Index: Cardinal; Y: Double);
    function GetX(Index: Cardinal): Double;
    function GetY(Index: Cardinal): Double;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; NumPnt: Cardinal;
      FlipY: Boolean = False);
    destructor Destroy; override;

    // Set other parameters
    procedure SetBorderWidth(T: Double; Extra: Double = 0.0);
    procedure SetCurveWidth(T: Double);
    procedure SetPointSize(S: Double);

    // Event handlers. Just call them if the respective events
    // in your system occure. The functions return true if redrawing
    // is required.
    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Spline
    procedure SetActivePoint(I: Integer);

    function GetSpline: PDouble;
    function GetSpline8: PInt8u;

    function GetValue(X: Double): Double;
    procedure SetValue(Index: Cardinal; Y: Double);
    procedure SetPoint(Index: Cardinal; X, Y: Double);

    procedure UpdateSpline;

    // Vertex soutce interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property X[Index: Cardinal]: Double read GetX write SetX;
    property Y[Index: Cardinal]: Double read GetY write SetY;
//    property Value[Index: Cardinal]: Double read GetValue write SetValue;
  end;

  TSplineControl = class(TAggCustomSplineControl)
  private
    FBackgroundColor: TAggColor;
    FBorderColor: TAggColor;
    FCurveColor: TAggColor;
    FInactivePointColor: TAggColor;
    FActivePointColor: TAggColor;

    FColors: array [0..4] of PAggColor;
    procedure SetBackgroundColor(Value: TAggColor);
    procedure SetBorderColor(Value: TAggColor);
    procedure SetCurveColor(Value: TAggColor);
    procedure SetInactivePointColor(Value: TAggColor);
    procedure SetActivePointColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; NumPnt: Cardinal;
      FlipY: Boolean = False);

    property BackgroundColor: TAggColor read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TAggColor read FBorderColor write SetBorderColor;
    property CurveColor: TAggColor read FCurveColor write SetCurveColor;
    property InactivePointColor: TAggColor read FInactivePointColor write SetInactivePointColor;
    property ActivePointColor: TAggColor read FActivePointColor write SetActivePointColor;
  end;

implementation


{ TAggCustomSplineControl }

constructor TAggCustomSplineControl.Create(X1, Y1, X2, Y2: Double;
  NumPnt: Cardinal; FlipY: Boolean = False);
var
  I: Cardinal;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FNumPoints := NumPnt;

  FBorderWidth := 1.0;
  FBorderExtra := 0.0;
  FCurveWidth := 1.0;
  FPointSize := 3.0;

  FSpline := TAggBSpline.Create;
  FCurvePnt := TAggPathStorage.Create;
  FCurvePoly := TAggConvStroke.Create(FCurvePnt);
  FCircle := TAggCircle.Create;

  FIndex := 0;
  FVertex := 0;
  FActivePoint := -1;
  FMovePoint := -1;

  FPDX := 0.0;
  FPDY := 0.0;

  if FNumPoints < 4 then
    FNumPoints := 4;

  if FNumPoints > 32 then
    FNumPoints := 32;

  for I := 0 to FNumPoints - 1 do
    FPoints[I] := PointDouble(I / (FNumPoints - 1), 0.5);

  CalcSplineBox;
  UpdateSpline;
end;

destructor TAggCustomSplineControl.Destroy;
begin
  FSpline.Free;

  FCircle.Free;
  FCurvePnt.Free;
  FCurvePoly.Free;
  inherited;
end;

procedure TAggCustomSplineControl.SetBorderWidth;
begin
  FBorderWidth := T;
  FBorderExtra := Extra;

  CalcSplineBox;
end;

procedure TAggCustomSplineControl.SetCurveWidth;
begin
  FCurveWidth := T;
end;

procedure TAggCustomSplineControl.SetPointSize;
begin
  FPointSize := S;
end;

function TAggCustomSplineControl.InRect(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := (X >= FRect.X1) and (X <= FRect.X2) and (Y >= FRect.Y1) and (Y <= FRect.Y2);
end;

function TAggCustomSplineControl.OnMouseButtonDown(X, Y: Double): Boolean;
var
  I: Cardinal;

  Xp, Yp: Double;

begin
  InverseTransformXY(@X, @Y);

  for I := 0 to FNumPoints - 1 do
  begin
    Xp := CalcXp(I);
    Yp := CalcYp(I);

    if CalculateDistance(X, Y, Xp, Yp) <= FPointSize + 1 then
    begin
      FPDX := Xp - X;
      FPDY := Yp - Y;

      FActivePoint := I;
      FMovePoint := I;

      Result := True;

      Exit;
    end;
  end;

  Result := False;
end;

function TAggCustomSplineControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  if FMovePoint >= 0 then
  begin
    FMovePoint := -1;

    Result := True;

  end
  else
    Result := False;
end;

function TAggCustomSplineControl.OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
var
  Xp, Yp: Double;
begin
  InverseTransformXY(@X, @Y);

  if not ButtonFlag then
  begin
    Result := OnMouseButtonUp(X, Y);

    Exit;
  end;

  if FMovePoint >= 0 then
  begin
    Xp := X + FPDX;
    Yp := Y + FPDY;

    SetXp(FMovePoint, (Xp - FXS1) / (FXS2 - FXS1));
    SetYp(FMovePoint, (Yp - FYS1) / (FYS2 - FYS1));

    UpdateSpline;

    Result := True;

  end
  else
    Result := False;
end;

function TAggCustomSplineControl.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
var
  Key: TPointDouble;
  Ret: Boolean;
begin
  Key.X := 0.0;
  Key.Y := 0.0;
  Ret := False;

  if FActivePoint >= 0 then
  begin
    Key.X := FPoints[FActivePoint].X;
    Key.Y := FPoints[FActivePoint].Y;

    if Left then
    begin
      Key.X := Key.X - 0.001;
      Ret := True;
    end;

    if Right then
    begin
      Key.X := Key.X + 0.001;
      Ret := True;
    end;

    if Down then
    begin
      Key.Y := Key.Y - 0.001;
      Ret := True;
    end;

    if Up then
    begin
      Key.Y := Key.Y + 0.001;
      Ret := True;
    end;
  end;

  if Ret then
  begin
    SetXp(FActivePoint, Key.X);
    SetYp(FActivePoint, Key.Y);

    UpdateSpline;
  end;

  Result := Ret;
end;

procedure TAggCustomSplineControl.SetActivePoint;
begin
  FActivePoint := I;
end;

function TAggCustomSplineControl.GetSpline;
begin
  Result := @FSplineValues;
end;

function TAggCustomSplineControl.GetSpline8;
begin
  Result := @FSplineValues8;
end;

function TAggCustomSplineControl.GetValue;
begin
  X := FSpline.Get(X);

  if X < 0.0 then
    X := 0.0;

  if X > 1.0 then
    X := 1.0;

  Result := X;
end;

procedure TAggCustomSplineControl.SetValue;
begin
  if Index < FNumPoints then
    SetYp(Index, Y);
end;

procedure TAggCustomSplineControl.SetPoint;
begin
  if Index < FNumPoints then
  begin
    SetXp(Index, X);
    SetYp(Index, Y);
  end;
end;

procedure TAggCustomSplineControl.SetX(Index: Cardinal; X: Double);
begin
  FPoints[Index].X := X;
end;

procedure TAggCustomSplineControl.SetY(Index: Cardinal; Y: Double);
begin
  FPoints[Index].Y := Y;
end;

function TAggCustomSplineControl.GetX(Index: Cardinal): Double;
begin
  Result := FPoints[Index].X;
end;

function TAggCustomSplineControl.GetY(Index: Cardinal): Double;
begin
  Result := FPoints[Index].Y;
end;

procedure TAggCustomSplineControl.UpdateSpline;
var
  I: Integer;
begin
  FSpline.Init(FNumPoints, @FPoints);

  for I := 0 to 255 do
  begin
    FSplineValues[I] := FSpline.Get(I / 255.0);

    if FSplineValues[I] < 0.0 then
      FSplineValues[I] := 0.0;

    if FSplineValues[I] > 1.0 then
      FSplineValues[I] := 1.0;

    FSplineValues8[I] := Int8u(Trunc(FSplineValues[I] * 255.0));
  end;
end;

function TAggCustomSplineControl.GetPathCount: Cardinal;
begin
  Result := 5;
end;

procedure TAggCustomSplineControl.Rewind(PathID: Cardinal);
var
  I: Cardinal;
begin
  FIndex := PathID;

  case PathID of
    0: // Background
      begin
        FVertex := 0;

        FVX[0] := FRect.X1 - FBorderExtra;
        FVY[0] := FRect.Y1 - FBorderExtra;
        FVX[1] := FRect.X2 + FBorderExtra;
        FVY[1] := FRect.Y1 - FBorderExtra;
        FVX[2] := FRect.X2 + FBorderExtra;
        FVY[2] := FRect.Y2 + FBorderExtra;
        FVX[3] := FRect.X1 - FBorderExtra;
        FVY[3] := FRect.Y2 + FBorderExtra;
      end;

    1: // Border
      begin
        FVertex := 0;

        FVX[0] := FRect.X1;
        FVY[0] := FRect.Y1;
        FVX[1] := FRect.X2;
        FVY[1] := FRect.Y1;
        FVX[2] := FRect.X2;
        FVY[2] := FRect.Y2;
        FVX[3] := FRect.X1;
        FVY[3] := FRect.Y2;
        FVX[4] := FRect.X1 + FBorderWidth;
        FVY[4] := FRect.Y1 + FBorderWidth;
        FVX[5] := FRect.X1 + FBorderWidth;
        FVY[5] := FRect.Y2 - FBorderWidth;
        FVX[6] := FRect.X2 - FBorderWidth;
        FVY[6] := FRect.Y2 - FBorderWidth;
        FVX[7] := FRect.X2 - FBorderWidth;
        FVY[7] := FRect.Y1 + FBorderWidth;
      end;

    2: // Curve
      begin
        CalcCurve;

        FCurvePoly.Width := FCurveWidth;
        FCurvePoly.Rewind(0);
      end;

    3: // Inactive points
      begin
        FCurvePnt.RemoveAll;

        for I := 0 to FNumPoints - 1 do
          if I <> FActivePoint then
          begin
            FCircle.Initialize(CalcPoint(I), FPointSize, 32);

            FCurvePnt.AddPath(FCircle, 0, False);
          end;

        FCurvePoly.Rewind(0);
      end;

    4: // Active point
      begin
        FCurvePnt.RemoveAll;

        if FActivePoint >= 0 then
        begin
          FCircle.Initialize(CalcPoint(FActivePoint), FPointSize, 32);

          FCurvePnt.AddPath(FCircle);
        end;

        FCurvePoly.Rewind(0);
      end;
  end;
end;

function TAggCustomSplineControl.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdLineTo;

  case FIndex of
    0:
      begin
        if FVertex = 0 then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 4 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end;

    1:
      begin
        if (FVertex = 0) or (FVertex = 4) then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 8 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end;

    2:
      Cmd := FCurvePoly.Vertex(X, Y);

    3, 4:
      Cmd := FCurvePnt.Vertex(X, Y);

  else
    Cmd := CAggPathCmdStop;
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;

procedure TAggCustomSplineControl.CalcSplineBox;
begin
  FXS1 := FRect.X1 + FBorderWidth;
  FYS1 := FRect.Y1 + FBorderWidth;
  FXS2 := FRect.X2 - FBorderWidth;
  FYS2 := FRect.Y2 - FBorderWidth;
end;

procedure TAggCustomSplineControl.CalcCurve;
var
  I: Integer;
begin
  FCurvePnt.RemoveAll;
  FCurvePnt.MoveTo(FXS1, FYS1 + (FYS2 - FYS1) * FSplineValues[0]);

  for I := 1 to 255 do
    FCurvePnt.LineTo(FXS1 + (FXS2 - FXS1) * I / 255.0,
      FYS1 + (FYS2 - FYS1) * FSplineValues[I]);
end;

function TAggCustomSplineControl.CalcPoint(Index: Cardinal): TPointDouble;
begin
  Result.X := FXS1 + (FXS2 - FXS1) * FPoints[Index].X;
  Result.Y := FYS1 + (FYS2 - FYS1) * FPoints[Index].Y;
end;

function TAggCustomSplineControl.CalcXp(Index: Cardinal): Double;
begin
  Result := FXS1 + (FXS2 - FXS1) * FPoints[Index].X;
end;

function TAggCustomSplineControl.CalcYp(Index: Cardinal): Double;
begin
  Result := FYS1 + (FYS2 - FYS1) * FPoints[Index].Y;
end;

procedure TAggCustomSplineControl.SetXp(Index: Cardinal; Val: Double);
begin
  if Val < 0.0 then
    Val := 0.0;

  if Val > 1.0 then
    Val := 1.0;

  if Index = 0 then
    Val := 0.0
  else if Index = FNumPoints - 1 then
    Val := 1.0
  else
  begin
    if Val < FPoints[Index - 1].X + 0.001 then
      Val := FPoints[Index - 1].X + 0.001;

    if Val > FPoints[Index + 1].X - 0.001 then
      Val := FPoints[Index + 1].X - 0.001;
  end;

  FPoints[Index].X := Val;
end;

procedure TAggCustomSplineControl.SetYp(Index: Cardinal; Val: Double);
begin
  if Val < 0.0 then
    Val := 0.0;

  if Val > 1.0 then
    Val := 1.0;

  FPoints[Index].Y := Val;
end;


{ TSplineControl }

constructor TSplineControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, NumPnt, FlipY);

  FBackgroundColor.FromRgbaDouble(1, 1, 0.9);
  FBorderColor.Black;
  FCurveColor.Black;
  FInactivePointColor.Black;
  FActivePointColor.FromRgbaInteger($FF, 0, 0, $FF);

  FColors[0] := @FBackgroundColor;
  FColors[1] := @FBorderColor;
  FColors[2] := @FCurveColor;
  FColors[3] := @FInactivePointColor;
  FColors[4] := @FActivePointColor;
end;

procedure TSplineControl.SetBackgroundColor(Value: TAggColor);
begin
  FBackgroundColor := Value;
end;

procedure TSplineControl.SetBorderColor(Value: TAggColor);
begin
  FBorderColor := Value;
end;

procedure TSplineControl.SetCurveColor(Value: TAggColor);
begin
  FCurveColor := Value;
end;

procedure TSplineControl.SetInactivePointColor(Value: TAggColor);
begin
  FInactivePointColor := Value;
end;

procedure TSplineControl.SetActivePointColor(Value: TAggColor);
begin
  FActivePointColor := Value;
end;

function TSplineControl.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := nil;
  if Index <= 4 then
    Result := FColors[Index];
end;

end.
