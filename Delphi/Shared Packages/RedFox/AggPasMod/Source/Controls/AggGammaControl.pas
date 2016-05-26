unit AggGammaControl;

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

{$I ../AggCompiler.inc}

uses
  SysUtils,
  AggBasics,
  AggGammaSpline,
  AggEllipse,
  AggConvStroke,
  AggGsvText,
  AggTransAffine,
  AggColor,
  AggControl,
  AggMath,
  AggMathStroke;

type
  TAggCustomGammaControl = class(TAggCustomAggControl)
  private
    FGammaSpline: TAggGammaSpline;

    FBorderWidth, FBorderExtra, FCurveWidth, FGridWidth: Double;
    FTextThickness, FPointSize, FTextHeight, FTextWidth: Double;

    FControlX1, FControlY1, FControlX2, FControlY2: Double;
    FSplineX1, FSplineY1, FSplineX2, FSplineY2: Double;
    FTextX1, FTextY1, FTextX2, FTextY2: Double;

    FCurvePoly: TAggConvStroke;
    FCircle: TAggCircle;
    FText: TAggGsvText;
    FTextPoly: TAggConvStroke;

    FIndex, FVertex: Cardinal;

    FVX, FVY: array [0..20] of Double;

    FCenter: array [0..1] of TPointDouble;

    FP1Active: Boolean;
    FMousePoint: Cardinal;

    FPDX, FPDY: Double;

    // Private
    procedure CalculateSplineBox;
    procedure CalculatePoints;
    procedure CalculateValues;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);
    destructor Destroy; override;

    // Set other parameters
    procedure SetBorderWidth(T: Double; Extra: Double = 0.0);
    procedure SetCurveWidth(T: Double);
    procedure SetGridWidth(T: Double);
    procedure SetTextThickness(T: Double);
    procedure SetTextSize(H: Double; W: Double = 0.0);
    procedure SetPointSize(S: Double);

    // Event handlers. Just call them if the respective events
    // in your system occure. The functions return true if redrawing
    // is required.
    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    procedure ChangeActivePoint;

    // A copy of TAggGammaSpline interface
    procedure Values(Kx1, Ky1, Kx2, Ky2: Double); overload;
    procedure Values(Kx1, Ky1, Kx2, Ky2: PDouble); overload;

    function Gamma: PInt8u;
    function GetY(X: Double): Double;

    function FuncOperatorGamma(X: Double): Double; override;

    // Vertex soutce interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggGammaControl = class(TAggCustomGammaControl)
  private
    FBackgroundColor: TAggColor;
    FBorderColor: TAggColor;
    FCurveColor: TAggColor;
    FGridColor: TAggColor;
    FInactivePointColor: TAggColor;
    FActivePointColor: TAggColor;
    FTextColor: TAggColor;

    FColors: array [0..6] of PAggColor;

    procedure SetBackgroundColor(Value: TAggColor);
    procedure SetBorderColor(Value: TAggColor);
    procedure SetCurveColor(Value: TAggColor);
    procedure SetGridColor(Value: TAggColor);
    procedure SetInactivePointColor(Value: TAggColor);
    procedure SetActivePointColor(Value: TAggColor);
    procedure SetTextColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);

    property BackgroundColor: TAggColor read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TAggColor read FBorderColor write SetBorderColor;
    property CurveColor: TAggColor read FCurveColor write SetCurveColor;
    property GridColor: TAggColor read FGridColor write SetGridColor;
    property InactivePointColor: TAggColor read FInactivePointColor write SetInactivePointColor;
    property ActivePointColor: TAggColor read FActivePointColor write SetActivePointColor;
    property TextColor: TAggColor read FTextColor write SetTextColor;
  end;

implementation


{ TAggCustomGammaControl }

constructor TAggCustomGammaControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FBorderWidth := 2.0;
  FBorderExtra := 0.0;
  FCurveWidth := 2.0;
  FGridWidth := 0.2;
  FTextThickness := 1.5;
  FPointSize := 5.0;
  FTextHeight := 9.0;
  FTextWidth := 0.0;

  FControlX1 := X1;
  FControlY1 := Y1;
  FControlX2 := X2;
  FControlY2 := Y2 - FTextHeight * 2.0;
  FTextX1 := X1;
  FTextY1 := Y2 - FTextHeight * 2.0;
  FTextX2 := X2;
  FTextY2 := Y2;

  FGammaSpline := TAggGammaSpline.Create;
  FCircle := TAggCircle.Create;
  FCurvePoly := TAggConvStroke.Create(FGammaSpline);
  FText := TAggGsvText.Create;
  FTextPoly := TAggConvStroke.Create(FText);

  FIndex := 0;
  FVertex := 0;

  FP1Active := True;
  FMousePoint := 0;

  FPDX := 0.0;
  FPDY := 0.0;

  CalculateSplineBox;
end;

destructor TAggCustomGammaControl.Destroy;
begin
  FGammaSpline.Free;
  FCurvePoly.Free;
  FText.Free;
  FTextPoly.Free;
  FCircle.Free;

  inherited;
end;

procedure TAggCustomGammaControl.SetBorderWidth;
begin
  FBorderWidth := T;
  FBorderExtra := Extra;

  CalculateSplineBox;
end;

procedure TAggCustomGammaControl.SetCurveWidth;
begin
  FCurveWidth := T;
end;

procedure TAggCustomGammaControl.SetGridWidth;
begin
  FGridWidth := T;
end;

procedure TAggCustomGammaControl.SetTextThickness;
begin
  FTextThickness := T;
end;

procedure TAggCustomGammaControl.SetTextSize(H: Double; W: Double = 0.0);
begin
  FTextWidth := W;
  FTextHeight := H;

  FControlY2 := FRect.Y2 - FTextHeight * 2.0;
  FTextY1 := FRect.Y2 - FTextHeight * 2.0;

  CalculateSplineBox;
end;

procedure TAggCustomGammaControl.SetPointSize;
begin
  FPointSize := S;
end;

function TAggCustomGammaControl.InRect(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := (X >= FRect.X1) and (X <= FRect.X2) and (Y >= FRect.Y1) and
    (Y <= FRect.Y2);
end;

function TAggCustomGammaControl.OnMouseButtonDown(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);
  CalculatePoints;

  if CalculateDistance(X, Y, FCenter[0].X, FCenter[0].Y) <= FPointSize + 1 then
  begin
    FMousePoint := 1;

    FPDX := FCenter[0].X - X;
    FPDY := FCenter[0].Y - Y;

    FP1Active := True;

    Result := True;

    Exit;
  end;

  if CalculateDistance(X, Y, FCenter[1].X, FCenter[1].Y) <= FPointSize + 1 then
  begin
    FMousePoint := 2;

    FPDX := FCenter[1].X - X;
    FPDY := FCenter[1].Y - Y;

    FP1Active := False;

    Result := True;

    Exit;
  end;

  Result := False;
end;

function TAggCustomGammaControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  if FMousePoint <> 0 then
  begin
    FMousePoint := 0;

    Result := True;
  end
  else
    Result := False;
end;

function TAggCustomGammaControl.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
begin
  InverseTransformXY(@X, @Y);

  if not ButtonFlag then
    Result := OnMouseButtonUp(X, Y)
  else
  begin
    if FMousePoint = 1 then
    begin
      FCenter[0].X := X + FPDX;
      FCenter[0].Y := Y + FPDY;

      CalculateValues;

      Result := True;

      Exit;
    end;

    if FMousePoint = 2 then
    begin
      FCenter[1].X := X + FPDX;
      FCenter[1].Y := Y + FPDY;

      CalculateValues;

      Result := True;

      Exit;
    end;

    Result := False;
  end;
end;

function TAggCustomGammaControl.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
var
  Kx1, Ky1, Kx2, Ky2: Double;

  Ret: Boolean;

begin
  Ret := False;

  FGammaSpline.Values(@Kx1, @Ky1, @Kx2, @Ky2);

  if FP1Active then
  begin
    if Left then
    begin
      Kx1 := Kx1 - 0.005;
      Ret := True;
    end;

    if Right then
    begin
      Kx1 := Kx1 + 0.005;
      Ret := True;
    end;

    if Down then
    begin
      Ky1 := Ky1 - 0.005;
      Ret := True;
    end;

    if Up then
    begin
      Ky1 := Ky1 + 0.005;
      Ret := True;
    end;

  end
  else
  begin
    if Left then
    begin
      Kx2 := Kx2 + 0.005;
      Ret := True;
    end;

    if Right then
    begin
      Kx2 := Kx2 - 0.005;
      Ret := True;
    end;

    if Down then
    begin
      Ky2 := Ky2 + 0.005;
      Ret := True;
    end;

    if Up then
    begin
      Ky2 := Ky2 - 0.005;
      Ret := True;
    end;
  end;

  if Ret then
    FGammaSpline.Values(Kx1, Ky1, Kx2, Ky2);

  Result := Ret;
end;

procedure TAggCustomGammaControl.ChangeActivePoint;
begin
  if FP1Active then
    FP1Active := False
  else
    FP1Active := True;
end;

procedure TAggCustomGammaControl.Values(Kx1, Ky1, Kx2, Ky2: Double);
begin
  FGammaSpline.Values(Kx1, Ky1, Kx2, Ky2);
end;

procedure TAggCustomGammaControl.Values(Kx1, Ky1, Kx2, Ky2: PDouble);
begin
  FGammaSpline.Values(Kx1, Ky1, Kx2, Ky2);
end;

function TAggCustomGammaControl.Gamma: PInt8u;
begin
  Result := FGammaSpline.Gamma;
end;

function TAggCustomGammaControl.GetY(X: Double): Double;
begin
  Result := FGammaSpline.GetY(X);
end;

function TAggCustomGammaControl.FuncOperatorGamma(X: Double): Double;
begin
  Result := FGammaSpline.GetY(X);
end;

function TAggCustomGammaControl.GetPathCount: Cardinal;
begin
  Result := 7;
end;

procedure TAggCustomGammaControl.Rewind(PathID: Cardinal);
var
  Kx1, Ky1, Kx2, Ky2: Double;
  Text: AnsiString;
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
        FVX[8] := FControlX1 + FBorderWidth;
        FVY[8] := FControlY2 - FBorderWidth * 0.5;
        FVX[9] := FControlX2 - FBorderWidth;
        FVY[9] := FControlY2 - FBorderWidth * 0.5;
        FVX[10] := FControlX2 - FBorderWidth;
        FVY[10] := FControlY2 + FBorderWidth * 0.5;
        FVX[11] := FControlX1 + FBorderWidth;
        FVY[11] := FControlY2 + FBorderWidth * 0.5;
      end;

    2: // Curve
      begin
        FGammaSpline.Box(FSplineX1, FSplineY1, FSplineX2, FSplineY2);

        FCurvePoly.Width := FCurveWidth;
        FCurvePoly.Rewind(0);
      end;

    3: // Grid
      begin
        FVertex := 0;

        FVX[0] := FSplineX1;
        FVY[0] := (FSplineY1 + FSplineY2) * 0.5 - FGridWidth * 0.5;
        FVX[1] := FSplineX2;
        FVY[1] := (FSplineY1 + FSplineY2) * 0.5 - FGridWidth * 0.5;
        FVX[2] := FSplineX2;
        FVY[2] := (FSplineY1 + FSplineY2) * 0.5 + FGridWidth * 0.5;
        FVX[3] := FSplineX1;
        FVY[3] := (FSplineY1 + FSplineY2) * 0.5 + FGridWidth * 0.5;
        FVX[4] := (FSplineX1 + FSplineX2) * 0.5 - FGridWidth * 0.5;
        FVY[4] := FSplineY1;
        FVX[5] := (FSplineX1 + FSplineX2) * 0.5 - FGridWidth * 0.5;
        FVY[5] := FSplineY2;
        FVX[6] := (FSplineX1 + FSplineX2) * 0.5 + FGridWidth * 0.5;
        FVY[6] := FSplineY2;
        FVX[7] := (FSplineX1 + FSplineX2) * 0.5 + FGridWidth * 0.5;
        FVY[7] := FSplineY1;

        CalculatePoints;

        FVX[8] := FSplineX1;
        FVY[8] := FCenter[0].Y - FGridWidth * 0.5;
        FVX[9] := FCenter[0].X - FGridWidth * 0.5;
        FVY[9] := FCenter[0].Y - FGridWidth * 0.5;
        FVX[10] := FCenter[0].X - FGridWidth * 0.5;
        FVY[10] := FSplineY1;
        FVX[11] := FCenter[0].X + FGridWidth * 0.5;
        FVY[11] := FSplineY1;
        FVX[12] := FCenter[0].X + FGridWidth * 0.5;
        FVY[12] := FCenter[0].Y + FGridWidth * 0.5;
        FVX[13] := FSplineX1;
        FVY[13] := FCenter[0].Y + FGridWidth * 0.5;
        FVX[14] := FSplineX2;
        FVY[14] := FCenter[1].Y + FGridWidth * 0.5;
        FVX[15] := FCenter[1].X + FGridWidth * 0.5;
        FVY[15] := FCenter[1].Y + FGridWidth * 0.5;
        FVX[16] := FCenter[1].X + FGridWidth * 0.5;
        FVY[16] := FSplineY2;
        FVX[17] := FCenter[1].X - FGridWidth * 0.5;
        FVY[17] := FSplineY2;
        FVX[18] := FCenter[1].X - FGridWidth * 0.5;
        FVY[18] := FCenter[1].Y - FGridWidth * 0.5;
        FVX[19] := FSplineX2;
        FVY[19] := FCenter[1].Y - FGridWidth * 0.5;
      end;

    4: // Point1
      begin
        CalculatePoints;

        if FP1Active then
          FCircle.Initialize(FCenter[1], FPointSize, 32)
        else
          FCircle.Initialize(FCenter[0], FPointSize, 32);
      end;

    5: // Point2
      begin
        CalculatePoints;

        if FP1Active then
          FCircle.Initialize(FCenter[0], FPointSize, 32)
        else
          FCircle.Initialize(FCenter[1], FPointSize, 32);
      end;

    6: // Text
      begin
        FGammaSpline.Values(@Kx1, @Ky1, @Kx2, @Ky2);

        Text := Format('%.3f  %.3f  %.3f  %.3f', [Kx1, Ky1, Kx2, Ky2]);

        FText.SetText(Text);
        FText.SetSize(FTextHeight, FTextWidth);

        FText.SetStartPoint(FTextX1 + FBorderWidth * 2.0,
          (FTextY1 + FTextY2) * 0.5 - FTextHeight * 0.5);
        FTextPoly.Width := FTextThickness;

        FTextPoly.LineJoin := ljRound;
        FTextPoly.LineCap := lcRound;

        FTextPoly.Rewind(0);
      end;
  end;
end;

function TAggCustomGammaControl.Vertex(X, Y: PDouble): Cardinal;
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
        if (FVertex = 0) or (FVertex = 4) or (FVertex = 8) then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 12 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end;

    2:
      Cmd := FCurvePoly.Vertex(X, Y);

    3:
      begin
        if (FVertex = 0) or (FVertex = 4) or (FVertex = 8) or (FVertex = 14)
        then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 20 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end;

    4, 5:
      Cmd := FCircle.Vertex(X, Y);

    6:
      Cmd := FTextPoly.Vertex(X, Y);

  else
    Cmd := CAggPathCmdStop;
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;

procedure TAggCustomGammaControl.CalculateSplineBox;
begin
  FSplineX1 := FControlX1 + FBorderWidth;
  FSplineY1 := FControlY1 + FBorderWidth;
  FSplineX2 := FControlX2 - FBorderWidth;
  FSplineY2 := FControlY2 - FBorderWidth * 0.5;
end;

procedure TAggCustomGammaControl.CalculatePoints;
var
  Kx1, Ky1, Kx2, Ky2: Double;
begin
  FGammaSpline.Values(@Kx1, @Ky1, @Kx2, @Ky2);

  FCenter[0].X := FSplineX1 + (FSplineX2 - FSplineX1) * Kx1 * 0.25;
  FCenter[0].Y := FSplineY1 + (FSplineY2 - FSplineY1) * Ky1 * 0.25;
  FCenter[1].X := FSplineX2 - (FSplineX2 - FSplineX1) * Kx2 * 0.25;
  FCenter[1].Y := FSplineY2 - (FSplineY2 - FSplineY1) * Ky2 * 0.25;
end;

procedure TAggCustomGammaControl.CalculateValues;
var
  Kx1, Ky1, Kx2, Ky2: Double;
begin
  Kx1 := (FCenter[0].X - FSplineX1) * 4.0 / (FSplineX2 - FSplineX1);
  Ky1 := (FCenter[0].Y - FSplineY1) * 4.0 / (FSplineY2 - FSplineY1);
  Kx2 := (FSplineX2 - FCenter[1].X) * 4.0 / (FSplineX2 - FSplineX1);
  Ky2 := (FSplineY2 - FCenter[1].Y) * 4.0 / (FSplineY2 - FSplineY1);

  FGammaSpline.Values(Kx1, Ky1, Kx2, Ky2);
end;


{ TAggGammaControl }

constructor TAggGammaControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FBackgroundColor.FromRgbaDouble(1.0, 1.0, 0.9);
  FBorderColor.Black;
  FCurveColor.Black;
  FGridColor.FromRgbaDouble(0.2, 0.2, 0.0);
  FInactivePointColor.Black;
  FActivePointColor.FromRgbaInteger($FF, 0, 0, $FF);
  FTextColor.Black;

  FColors[0] := @FBackgroundColor;
  FColors[1] := @FBorderColor;
  FColors[2] := @FCurveColor;
  FColors[3] := @FGridColor;
  FColors[4] := @FInactivePointColor;
  FColors[5] := @FActivePointColor;
  FColors[6] := @FTextColor;
end;

procedure TAggGammaControl.SetBackgroundColor(Value: TAggColor);
begin
  FBackgroundColor := Value;
end;

procedure TAggGammaControl.SetBorderColor(Value: TAggColor);
begin
  FBorderColor := Value;
end;

procedure TAggGammaControl.SetCurveColor(Value: TAggColor);
begin
  FCurveColor := Value;
end;

procedure TAggGammaControl.SetGridColor(Value: TAggColor);
begin
  FGridColor := Value;
end;

procedure TAggGammaControl.SetInactivePointColor(Value: TAggColor);
begin
  FInactivePointColor := Value;
end;

procedure TAggGammaControl.SetActivePointColor(Value: TAggColor);
begin
  FActivePointColor := Value;
end;

procedure TAggGammaControl.SetTextColor(Value: TAggColor);
begin
  FTextColor := Value;
end;

function TAggGammaControl.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := nil;
  if Index <= 6 then
    Result := FColors[Index];
end;

end.
