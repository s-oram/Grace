 unit AggSliderControl;

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
  SysUtils,
  AggBasics,
  AggControl,
  AggColor,
  AggEllipse,
  AggPathStorage,
  AggConvStroke,
  AggGsvText,
  AggMath,
  AggMathStroke;

type
  TAggCustomSliderControl = class(TAggCustomAggControl)
  private
    FBorderWidth, FBorderExtra, FTextThickness, FValue: Double;
    FPreviewValue, FMin, FMax: Double;

    FNumSteps: Cardinal;
    FDescending: Boolean;

    FCaption: AnsiString;

    FXS1, FYS1, FXS2, FYS2, FPDX: Double;

    FMouseMove: Boolean;

    FVX, FVY: array [0..31] of Double;

    FEllipse: TAggEllipse;

    FIndex, FVertex: Cardinal;

    FText: TAggGsvText;
    FTextPoly: TAggConvStroke;
    FStorage: TAggPathStorage;

    function GetDescending: Boolean;
    function GetValue: Double;
    procedure SetDescending(V: Boolean);
    procedure SetValue(Value: Double);
    procedure SetNumSteps(Value: Cardinal);
    procedure SetCaption(Value: AnsiString);
    procedure SetTextThickness(Value: Double);
  protected
    function NormalizeValue(PreviewValueFlag: Boolean): Boolean;
    function GetPathCount: Cardinal; override;
    procedure CalcBox;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);
      override;
    destructor Destroy; override;

    procedure SetBorderWidth(Value: Double; Extra: Double = 0.0);
    procedure SetRange(Min, Max: Double);
    procedure SetClipBox(ClipBox: TRectDouble); override;

    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Caption: AnsiString read FCaption write SetCaption;
    property TextThickness: Double read FTextThickness write SetTextThickness;
    property Descending: Boolean read GetDescending write SetDescending;
    property Value: Double read GetValue write SetValue;
    property NumSteps: Cardinal read FNumSteps write SetNumSteps;
  end;

  TAggControlSlider = class(TAggCustomSliderControl)
  private
    FBackgroundColor: TAggColor;
    FTriangleColor: TAggColor;
    FTextColor: TAggColor;
    FPointerPreviewColor: TAggColor;
    FPointerColor: TAggColor;

    FColors: array [0..5] of PAggColor;

    procedure SetBackgroundColor(Value: TAggColor);
    procedure SetPointerColor(Value: TAggColor);
    procedure SetPointerPreviewColor(Value: TAggColor);
    procedure SetTriangleColor(Value: TAggColor);
    procedure SetTextColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);
      override;

    property BackgroundColor: TAggColor read FBackgroundColor write SetBackgroundColor;
    property TriangleColor: TAggColor read FTriangleColor write SetTriangleColor;
    property TextColor: TAggColor read FTextColor write SetTextColor;
    property PointerPreviewColor: TAggColor read FPointerPreviewColor write SetPointerPreviewColor;
    property PointerColor: TAggColor read FPointerColor write SetPointerColor;
  end;


implementation


{ TAggCustomSliderControl }

constructor TAggCustomSliderControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FEllipse := TAggEllipse.Create;
  FText := TAggGsvText.Create;
  FTextPoly := TAggConvStroke.Create(FText);
  FStorage := TAggPathStorage.Create;

  FBorderWidth := 1.0;
  FBorderExtra := (Y2 - Y1) * 0.5;
  FTextThickness := 1.0;
  FPDX := 0.0;
  FMouseMove := False;
  FValue := 0.5;
  FPreviewValue := 0.5;
  FMin := 0.0;
  FMax := 1.0;
  FNumSteps := 0;
  FDescending := False;

  FCaption := '';

  CalcBox;
end;

destructor TAggCustomSliderControl.Destroy;
begin
  FStorage.Free;
  FTextPoly.Free;
  FEllipse.Free;
  FText.Free;
  inherited;
end;

procedure TAggCustomSliderControl.SetBorderWidth(Value: Double; Extra: Double = 0.0);
begin
  FBorderWidth := Value;
  FBorderExtra := Extra;

  CalcBox;
end;

procedure TAggCustomSliderControl.SetRange(Min, Max: Double);
begin
  FMin := Min;
  FMax := Max;
end;

procedure TAggCustomSliderControl.SetNumSteps(Value: Cardinal);
begin
  FNumSteps := Value;
end;

procedure TAggCustomSliderControl.SetCaption(Value: AnsiString);
begin
  FCaption := Value;
end;

procedure TAggCustomSliderControl.SetClipBox(ClipBox: TRectDouble);
begin
  inherited;
  CalcBox;
end;

procedure TAggCustomSliderControl.SetTextThickness(Value: Double);
begin
  FTextThickness := Value;
end;

function TAggCustomSliderControl.GetDescending: Boolean;
begin
  Result := FDescending;
end;

procedure TAggCustomSliderControl.SetDescending(V: Boolean);
begin
  FDescending := V;
end;

function TAggCustomSliderControl.GetValue: Double;
begin
  Result := FValue * (FMax - FMin) + FMin;
end;

procedure TAggCustomSliderControl.SetValue(Value: Double);
begin
  FPreviewValue := (Value - FMin) / (FMax - FMin);

  if FPreviewValue > 1.0 then
    FPreviewValue := 1.0;

  if FPreviewValue < 0.0 then
    FPreviewValue := 0.0;

  NormalizeValue(True);
end;

function TAggCustomSliderControl.InRect(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := (X >= FRect.X1) and (X <= FRect.X2) and (Y >= FRect.Y1) and
    (Y <= FRect.Y2);
end;

function TAggCustomSliderControl.OnMouseButtonDown(X, Y: Double): Boolean;
var
  Xp, Yp: Double;
begin
  InverseTransformXY(@X, @Y);

  Xp := FXS1 + (FXS2 - FXS1) * FValue;
  Yp := (FYS1 + FYS2) * 0.5;

  if CalculateDistance(X, Y, Xp, Yp) <= FRect.Y2 - FRect.Y1 then
  begin
    FPDX := Xp - X;

    FMouseMove := True;

    Result := True;
  end
  else
    Result := False;
end;

function TAggCustomSliderControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  FMouseMove := False;

  NormalizeValue(True);

  Result := True;
end;

function TAggCustomSliderControl.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
var
  Xp: Double;
begin
  InverseTransformXY(@X, @Y);

  if not ButtonFlag then
  begin
    OnMouseButtonUp(X, Y);

    Result := False;

    Exit;
  end;

  if FMouseMove then
  begin
    Xp := X + FPDX;

    FPreviewValue := (Xp - FXS1) / (FXS2 - FXS1);

    if FPreviewValue < 0.0 then
      FPreviewValue := 0.0;

    if FPreviewValue > 1.0 then
      FPreviewValue := 1.0;

    Result := True;

  end
  else
    Result := False;
end;

function TAggCustomSliderControl.OnArrowKeys(Left, Right, Down,
  Up: Boolean): Boolean;
var
  D: Double;
begin
  D := 0.005;

  if FNumSteps <> 0 then
    D := 1.0 / FNumSteps;

  if Right or Up then
  begin
    FPreviewValue := FPreviewValue + D;

    if FPreviewValue > 1.0 then
      FPreviewValue := 1.0;

    NormalizeValue(True);

    Result := True;

    Exit;
  end;

  if Left or Down then
  begin
    FPreviewValue := FPreviewValue - D;

    if FPreviewValue < 0.0 then
      FPreviewValue := 0.0;

    NormalizeValue(True);

    Result := True;

  end
  else
    Result := False;
end;

function TAggCustomSliderControl.GetPathCount: Cardinal;
begin
  Result := 6;
end;

procedure TAggCustomSliderControl.Rewind(PathID: Cardinal);
var
  Index: Cardinal;
  D, X: Double;
begin
  FIndex := PathID;

  case PathID of
    1: // Triangle
      begin
        FVertex := 0;

        if FDescending then
        begin
          FVX[0] := FRect.X1;
          FVY[0] := FRect.Y1;
          FVX[1] := FRect.X2;
          FVY[1] := FRect.Y1;
          FVX[2] := FRect.X1;
          FVY[2] := FRect.Y2;
          FVX[3] := FRect.X1;
          FVY[3] := FRect.Y1;

        end
        else
        begin
          FVX[0] := FRect.X1;
          FVY[0] := FRect.Y1;
          FVX[1] := FRect.X2;
          FVY[1] := FRect.Y1;
          FVX[2] := FRect.X2;
          FVY[2] := FRect.Y2;
          FVX[3] := FRect.X1;
          FVY[3] := FRect.Y1;
        end;
      end;

    2:
      begin
        if FCaption <> '' then
        begin
          X := GetValue;
          FText.SetText(Format(FCaption, [X]));
        end;

        FText.SetStartPoint(FRect.X1, FRect.Y1);
        FText.SetSize((FRect.Y2 - FRect.Y1) * 1.2, FRect.Y2 - FRect.Y1);

        FTextPoly.Width := FTextThickness;
        FTextPoly.LineJoin := ljRound;
        FTextPoly.LineCap := lcRound;

        FTextPoly.Rewind(0);
      end;

    3: // pointer preview
      FEllipse.Initialize(FXS1 + (FXS2 - FXS1) * FPreviewValue,
        (FYS1 + FYS2) * 0.5, FRect.Y2 - FRect.Y1, FRect.Y2 - FRect.Y1, 32);

    4: // pointer
      begin
        NormalizeValue(False);

        FEllipse.Initialize(FXS1 + (FXS2 - FXS1) * FValue, (FYS1 + FYS2) * 0.5,
          FRect.Y2 - FRect.Y1, FRect.Y2 - FRect.Y1, 32);

        FEllipse.Rewind(0);
      end;

    5:
      begin
        FStorage.RemoveAll;

        if FNumSteps <> 0 then
        begin
          D := (FXS2 - FXS1) / FNumSteps;

          if D > 0.004 then
            D := 0.004;

          for Index := 0 to FNumSteps do
          begin
            X := FXS1 + (FXS2 - FXS1) * Index / FNumSteps;

            FStorage.MoveTo(X, FRect.Y1);

            FStorage.LineTo(X - D * (FRect.X2 - FRect.X1), FRect.Y1 - FBorderExtra);
            FStorage.LineTo(X + D * (FRect.X2 - FRect.X1), FRect.Y1 - FBorderExtra);
          end;
        end;
      end;

  else
    begin
      // Background
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
  end;
end;

function TAggCustomSliderControl.Vertex(X, Y: PDouble): Cardinal;
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
        if FVertex = 0 then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 4 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end;

    2:
      Cmd := FTextPoly.Vertex(X, Y);

    3, 4:
      Cmd := FEllipse.Vertex(X, Y);

    5:
      Cmd := FStorage.Vertex(X, Y);

  else
    Cmd := CAggPathCmdStop;
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;

procedure TAggCustomSliderControl.CalcBox;
begin
  FXS1 := FRect.X1 + FBorderWidth;
  FYS1 := FRect.Y1 + FBorderWidth;
  FXS2 := FRect.X2 - FBorderWidth;
  FYS2 := FRect.Y2 - FBorderWidth;
end;

function TAggCustomSliderControl.NormalizeValue(PreviewValueFlag: Boolean): Boolean;
var
  Temp: Double;
  Step: Integer;
begin
  Result := True;

  if FNumSteps <> 0 then
  begin
    Step := Trunc(FPreviewValue * FNumSteps + 0.5);
    Temp := (Step / FNumSteps);
    Result := FValue <> Temp;
    FValue := Temp;
  end
  else
    FValue := FPreviewValue;

  if PreviewValueFlag then
    FPreviewValue := FValue;
end;


{ TAggControlSlider }

constructor TAggControlSlider.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FBackgroundColor.FromRgbaDouble(1.0, 0.9, 0.8);
  FTriangleColor.FromRgbaDouble(0.7, 0.6, 0.6);
  FTextColor.FromRgbaInteger(0, 0, 0);
  FPointerPreviewColor.FromRgbaDouble(0.6, 0.4, 0.4, 0.4);
  FPointerColor.FromRgbaDouble(0.8, 0, 0, 0.6);

  FColors[0] := @FBackgroundColor;
  FColors[1] := @FTriangleColor;
  FColors[2] := @FTextColor;
  FColors[3] := @FPointerPreviewColor;
  FColors[4] := @FPointerColor;
  FColors[5] := @FTextColor;
end;

procedure TAggControlSlider.SetBackgroundColor(Value: TAggColor);
begin
  FBackgroundColor := Value;
end;

procedure TAggControlSlider.SetPointerColor(Value: TAggColor);
begin
  FPointerColor := Value;
end;

procedure TAggControlSlider.SetPointerPreviewColor(Value: TAggColor);
begin
  FPointerPreviewColor := Value;
end;

procedure TAggControlSlider.SetTextColor(Value: TAggColor);
begin
  FTextColor := Value;
end;

procedure TAggControlSlider.SetTriangleColor(Value: TAggColor);
begin
  FTriangleColor := Value;
end;

function TAggControlSlider.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := nil;
  if Index <= 5 then
    Result := FColors[Index];
end;

end.
