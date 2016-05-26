unit AggScaleControl;

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
  AggBasics,
  AggControl,
  AggColor,
  AggEllipse,
  AggMath,
  AggTransAffine;

type
  TMoveEnum = (meNothing, meValue1, meValue2, meSlider);

  TAggCustomScaleControl = class(TAggCustomAggControl)
  private
    FBorderThickness, FBorderExtra, FValue1, FValue2, FMinDelta: Double;
    FXS1, FYS1, FXS2, FYS2, FPDX, FPDY: Double;

    FMoveWhat: TMoveEnum;
    FVertices: array [0..8] of TPointDouble;

    FEllipse: TAggEllipse;

    FIndex, FVertex: Cardinal;

    // Private
    procedure CalcBox;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False); virtual;
    destructor Destroy; override;

    procedure SetBorderThickness(T: Double; Extra: Double = 0.0);

    procedure Resize(X1, Y1, X2, Y2: Double);

    function GetMinDelta: Double;
    procedure SetMinDelta(D: Double);

    function GetValue1: Double;
    procedure SetValue1(Value: Double);

    function GetValue2: Double;
    procedure SetValue2(Value: Double);

    procedure Move(D: Double);

    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TScaleControl = class(TAggCustomScaleControl)
  private
    FBackgroundColor: TAggColor;
    FBorderColor: TAggColor;
    FPointersColor: TAggColor;
    FSliderColor: TAggColor;

    FColors: array [0..5] of PAggColor;
    procedure SetBackgroundColor(Value: TAggColor);
    procedure SetBorderColor(Value: TAggColor);
    procedure SetPointersColor(Value: TAggColor);
    procedure SetSliderColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);
      override;

    property BackgroundColor: TAggColor read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TAggColor read FBorderColor write SetBorderColor;
    property PointersColor: TAggColor read FPointersColor write SetPointersColor;
    property SliderColor: TAggColor read FSliderColor write SetSliderColor;
  end;

implementation


{ TAggCustomScaleControl }

constructor TAggCustomScaleControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FBorderThickness := 1.0;

  if Abs(X2 - X1) > Abs(Y2 - Y1) then
    FBorderExtra := (Y2 - Y1) * 0.5
  else
    FBorderExtra := (X2 - X1) * 0.5;

  FPDX := 0.0;
  FPDY := 0.0;

  FMoveWhat := meNothing;

  FValue1 := 0.3;
  FValue2 := 0.7;
  FMinDelta := 0.01;

  FEllipse := TAggEllipse.Create;

  CalcBox;
end;

destructor TAggCustomScaleControl.Destroy;
begin
  FEllipse.Free;
  inherited;
end;

procedure TAggCustomScaleControl.SetBorderThickness;
begin
  FBorderThickness := T;
  FBorderExtra := Extra;

  CalcBox;
end;

procedure TAggCustomScaleControl.Resize;
begin
  FRect := RectDouble(X1, Y1, X2, Y2);

  CalcBox;

  if Abs(X2 - X1) > Abs(Y2 - Y1) then
    FBorderExtra := (Y2 - Y1) * 0.5
  else
    FBorderExtra := (X2 - X1) * 0.5;
end;

function TAggCustomScaleControl.GetMinDelta;
begin
  Result := FMinDelta;
end;

procedure TAggCustomScaleControl.SetMinDelta;
begin
  FMinDelta := D;
end;

function TAggCustomScaleControl.GetValue1;
begin
  Result := FValue1;
end;

procedure TAggCustomScaleControl.SetValue1;
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value > 1.0 then
    Value := 1.0;

  if FValue2 - Value < FMinDelta then
    Value := FValue2 - FMinDelta;

  FValue1 := Value;
end;

function TAggCustomScaleControl.GetValue2;
begin
  Result := FValue2;
end;

procedure TAggCustomScaleControl.SetValue2;
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value > 1.0 then
    Value := 1.0;

  if FValue1 + Value < FMinDelta then
    Value := FValue1 + FMinDelta;

  FValue2 := Value;
end;

procedure TAggCustomScaleControl.Move;
begin
  FValue1 := FValue1 + D;
  FValue2 := FValue2 + D;

  if FValue1 < 0.0 then
  begin
    FValue2 := FValue2 - FValue1;
    FValue1 := 0.0;
  end;

  if FValue2 > 1.0 then
  begin
    FValue1 := FValue1 - FValue2 - 1.0;
    FValue2 := 1.0;
  end;
end;

function TAggCustomScaleControl.InRect(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := (X >= FRect.X1) and (X <= FRect.X2) and (Y >= FRect.Y1) and
    (Y <= FRect.Y2);
end;

function TAggCustomScaleControl.OnMouseButtonDown(X, Y: Double): Boolean;
var
  Xp1, Xp2, Ys1, Ys2, Xp, Yp: Double;
begin
  InverseTransformXY(@X, @Y);

  if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
  begin
    Xp1 := FXS1 + (FXS2 - FXS1) * FValue1;
    Xp2 := FXS1 + (FXS2 - FXS1) * FValue2;
    Ys1 := FRect.Y1 - FBorderExtra * 0.5;
    Ys2 := FRect.Y2 + FBorderExtra * 0.5;
    Yp := (FYS1 + FYS2) * 0.5;

    if (X > Xp1) and (Y > Ys1) and (X < Xp2) and (Y < Ys2) then
    begin
      FPDX := Xp1 - X;

      FMoveWhat := meSlider;

      Result := True;

      Exit;
    end;

    // if(x < xp1 && CalculateDistance(x, y, xp1, yp) <= FRect.Y2 - FRect.Y1)
    if CalculateDistance(X, Y, Xp1, Yp) <= FRect.Y2 - FRect.Y1 then
    begin
      FPDX := Xp1 - X;

      FMoveWhat := meValue1;

      Result := True;

      Exit;
    end;

    // if(x > xp2 && CalculateDistance(x, y, xp2, yp) <= FRect.Y2 - FRect.Y1)
    if CalculateDistance(X, Y, Xp2, Yp) <= FRect.Y2 - FRect.Y1 then
    begin
      FPDX := Xp2 - X;

      FMoveWhat := meValue2;

      Result := True;
    end;
  end
  else
  begin
    Xp1 := FRect.X1 - FBorderExtra * 0.5;
    Xp2 := FRect.X2 + FBorderExtra * 0.5;
    Ys1 := FYS1 + (FYS2 - FYS1) * FValue1;
    Ys2 := FYS1 + (FYS2 - FYS1) * FValue2;
    Xp := (FXS1 + FXS2) * 0.5;

    if (X > Xp1) and (Y > Ys1) and (X < Xp2) and (Y < Ys2) then
    begin
      FPDY := Ys1 - Y;

      FMoveWhat := meSlider;

      Result := True;

      Exit;
    end;

    // if(y < ys1 && CalculateDistance(x, y, xp, ys1) <= FRect.X2 - FRect.X1)
    if CalculateDistance(X, Y, Xp, Ys1) <= FRect.X2 - FRect.X1 then
    begin
      FPDY := Ys1 - Y;

      FMoveWhat := meValue1;

      Result := True;

      Exit;
    end;

    // if(y > ys2 && CalculateDistance(x, y, xp, ys2) <= FRect.X2 - FRect.X1)
    if CalculateDistance(X, Y, Xp, Ys2) <= FRect.X2 - FRect.X1 then
    begin
      FPDY := Ys2 - Y;

      FMoveWhat := meValue2;

      Result := True;

      Exit;
    end;
  end;

  Result := False;
end;

function TAggCustomScaleControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  FMoveWhat := meNothing;

  Result := False;
end;

function TAggCustomScaleControl.OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
var
  Xp, Yp, Dv: Double;
begin
  InverseTransformXY(@X, @Y);

  if not ButtonFlag then
    Result := OnMouseButtonUp(X, Y)
  else
  begin
    Xp := X + FPDX;
    Yp := Y + FPDY;

    case FMoveWhat of
      meValue1:
        begin
          if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
            FValue1 := (Xp - FXS1) / (FXS2 - FXS1)
          else
            FValue1 := (Yp - FYS1) / (FYS2 - FYS1);

          if FValue1 < 0.0 then
            FValue1 := 0.0;

          if FValue1 > FValue2 - FMinDelta then
            FValue1 := FValue2 - FMinDelta;

          Result := True;
        end;

      meValue2:
        begin
          if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
            FValue2 := (Xp - FXS1) / (FXS2 - FXS1)
          else
            FValue2 := (Yp - FYS1) / (FYS2 - FYS1);

          if FValue2 > 1.0 then
            FValue2 := 1.0;

          if FValue2 < FValue1 + FMinDelta then
            FValue2 := FValue1 + FMinDelta;

          Result := True;
        end;

      meSlider:
        begin
          Dv := FValue2 - FValue1;

          if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
            FValue1 := (Xp - FXS1) / (FXS2 - FXS1)
          else
            FValue1 := (Yp - FYS1) / (FYS2 - FYS1);

          FValue2 := FValue1 + Dv;

          if FValue1 < 0.0 then
          begin
            Dv := FValue2 - FValue1;

            FValue1 := 0.0;
            FValue2 := FValue1 + Dv;
          end;

          if FValue2 > 1.0 then
          begin
            Dv := FValue2 - FValue1;

            FValue2 := 1.0;
            FValue1 := FValue2 - Dv;
          end;

          Result := True;
        end;

    else
      Result := False;
    end;
  end;
end;

function TAggCustomScaleControl.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
begin
  Result := False
end;

function TAggCustomScaleControl.GetPathCount: Cardinal;
begin
  Result := 5;
end;

procedure TAggCustomScaleControl.Rewind(PathID: Cardinal);
begin
  FIndex := PathID;

  case PathID of
    0: // Background
      begin
        FVertex := 0;

        FVertices[0].X := FRect.X1 - FBorderExtra;
        FVertices[0].Y := FRect.Y1 - FBorderExtra;
        FVertices[1].X := FRect.X2 + FBorderExtra;
        FVertices[1].Y := FRect.Y1 - FBorderExtra;
        FVertices[2].X := FRect.X2 + FBorderExtra;
        FVertices[2].Y := FRect.Y2 + FBorderExtra;
        FVertices[3].X := FRect.X1 - FBorderExtra;
        FVertices[3].Y := FRect.Y2 + FBorderExtra;
      end;

    1: // Border
      begin
        FVertex := 0;

        FVertices[0] := FRect.Point1;
        FVertices[1].X := FRect.X2;
        FVertices[1].Y := FRect.Y1;
        FVertices[2] := FRect.Point2;
        FVertices[3].X := FRect.X1;
        FVertices[3].Y := FRect.Y2;
        FVertices[4].X := FRect.X1 + FBorderThickness;
        FVertices[4].Y := FRect.Y1 + FBorderThickness;
        FVertices[5].X := FRect.X1 + FBorderThickness;
        FVertices[5].Y := FRect.Y2 - FBorderThickness;
        FVertices[6].X := FRect.X2 - FBorderThickness;
        FVertices[6].Y := FRect.Y2 - FBorderThickness;
        FVertices[7].X := FRect.X2 - FBorderThickness;
        FVertices[7].Y := FRect.Y1 + FBorderThickness;
      end;

    2: // pointer1
      begin
        if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
          FEllipse.Initialize(FXS1 + (FXS2 - FXS1) * FValue1,
            (FYS1 + FYS2) * 0.5, FRect.Y2 - FRect.Y1, FRect.Y2 - FRect.Y1, 32)
        else
          FEllipse.Initialize((FXS1 + FXS2) * 0.5, FYS1 + (FYS2 - FYS1) *
            FValue1, FRect.X2 - FRect.X1, FRect.X2 - FRect.X1, 32);

        FEllipse.Rewind(0);
      end;

    3: // pointer2
      begin
        if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
          FEllipse.Initialize(FXS1 + (FXS2 - FXS1) * FValue2,
            (FYS1 + FYS2) * 0.5, FRect.Y2 - FRect.Y1, FRect.Y2 - FRect.Y1, 32)
        else
          FEllipse.Initialize((FXS1 + FXS2) * 0.5, FYS1 + (FYS2 - FYS1) *
            FValue2, FRect.X2 - FRect.X1, FRect.X2 - FRect.X1, 32);

        FEllipse.Rewind(0);
      end;

    4: // slider
      begin
        FVertex := 0;

        if Abs(FRect.X2 - FRect.X1) > Abs(FRect.Y2 - FRect.Y1) then
        begin
          FVertices[0].X := FXS1 + (FXS2 - FXS1) * FValue1;
          FVertices[0].Y := FRect.Y1 - FBorderExtra * 0.5;
          FVertices[1].X := FXS1 + (FXS2 - FXS1) * FValue2;
          FVertices[1].Y := FVertices[0].Y;
          FVertices[2].X := FVertices[1].X;
          FVertices[2].Y := FRect.Y2 + FBorderExtra * 0.5;
          FVertices[3].X := FVertices[0].X;
          FVertices[3].Y := FVertices[2].Y;

        end
        else
        begin
          FVertices[0].X := FRect.X1 - FBorderExtra * 0.5;
          FVertices[0].Y := FYS1 + (FYS2 - FYS1) * FValue1;
          FVertices[1].X := FVertices[0].X;
          FVertices[1].Y := FYS1 + (FYS2 - FYS1) * FValue2;
          FVertices[2].X := FRect.X2 + FBorderExtra * 0.5;
          FVertices[2].Y := FVertices[1].Y;
          FVertices[3].X := FVertices[2].X;
          FVertices[3].Y := FVertices[0].Y;
        end;
      end;
  end;
end;

function TAggCustomScaleControl.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdLineTo;

  case FIndex of
    0, 4:
      begin
        if FVertex = 0 then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 4 then
          Cmd := CAggPathCmdStop;

        X^ := FVertices[FVertex].X;
        Y^ := FVertices[FVertex].Y;

        Inc(FVertex);
      end;

    1:
      begin
        if (FVertex = 0) or (FVertex = 4) then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 8 then
          Cmd := CAggPathCmdStop;

        X^ := FVertices[FVertex].X;
        Y^ := FVertices[FVertex].Y;

        Inc(FVertex);
      end;

    2, 3:
      Cmd := FEllipse.Vertex(X, Y);

  else
    Cmd := CAggPathCmdStop;
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;

procedure TAggCustomScaleControl.CalcBox;
begin
  FXS1 := FRect.X1 + FBorderThickness;
  FYS1 := FRect.Y1 + FBorderThickness;
  FXS2 := FRect.X2 - FBorderThickness;
  FYS2 := FRect.Y2 - FBorderThickness;
end;


{ TScaleControl }

constructor TScaleControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FBackgroundColor.FromRgbaDouble(1.0, 0.9, 0.8);
  FBorderColor.Black;
  FPointersColor.FromRgbaDouble(0.8, 0.0, 0.0, 0.8);
  FSliderColor.FromRgbaDouble(0.2, 0.1, 0.0, 0.6);

  FColors[0] := @FBackgroundColor;
  FColors[1] := @FBorderColor;
  FColors[2] := @FPointersColor;
  FColors[3] := @FPointersColor;
  FColors[4] := @FSliderColor;
end;

procedure TScaleControl.SetBackgroundColor(Value: TAggColor);
begin
  FBackgroundColor := Value;
end;

procedure TScaleControl.SetBorderColor(Value: TAggColor);
begin
  FBorderColor := Value;
end;

procedure TScaleControl.SetPointersColor(Value: TAggColor);
begin
  FPointersColor := Value;
end;

procedure TScaleControl.SetSliderColor(Value: TAggColor);
begin
  FSliderColor := Value;
end;

function TScaleControl.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := nil;
  if Index <= 5 then
    Result := FColors[Index];
end;

end.
