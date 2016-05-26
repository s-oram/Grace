unit AggCheckBoxControl;

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
  AggControl,
  AggConvStroke,
  AggGsvText,
  AggColor,
  AggMathStroke;

type
  TAggCustomCheckBoxControl = class(TAggCustomAggControl)
  private
    FTextThickness, FTextHeight, FTextWidth: Double;
    FCheckBoxRect: TRectDouble;

    FCaption: AnsiString;
    FStatus: Boolean;

    FVX, FVY: array [0..8] of Double;  // 2 x Quad?

    FText: TAggGsvText;
    FTextPoly: TAggConvStroke;

    FIndex, FVertex: Cardinal;

    procedure SetStatus(Value: Boolean);
    procedure SetCaption(Value: AnsiString);
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create(X, Y: Double; L: PAnsiChar; FlipY: Boolean = False);
    destructor Destroy; override;

    procedure SetTextThickness(T: Double);
    procedure SetTextSize(H: Double; W: Double = 0);
    procedure SetClipBox(ClipBox: TRectDouble); override;

    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Status: Boolean read FStatus write SetStatus;
    property Caption: AnsiString read FCaption write SetCaption;
  end;

  TAggControlCheckBox = class(TAggCustomCheckBoxControl)
  private
    FTextColor: TAggColor;
    FInactiveColor: TAggColor;
    FActiveColor: TAggColor;

    FColors: array [0..2] of PAggColor;
    procedure SetTextColor(Value: TAggColor);
    procedure SetInactiveColor(Value: TAggColor);
    procedure SetActiveColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(X, Y: Double; L: PAnsiChar; FlipY: Boolean = False);

    property TextColor: TAggColor read FTextColor write SetTextColor;
    property InactiveColor: TAggColor read FInactiveColor write SetInactiveColor;
    property ActiveColor: TAggColor read FActiveColor write SetActiveColor;
  end;

implementation


{ TAggCustomCheckBoxControl }

constructor TAggCustomCheckBoxControl.Create(X, Y: Double; L: PAnsiChar;
  FlipY: Boolean = False);
begin
  FTextThickness := 1.5;
  FTextHeight := 9.0;
  FTextWidth := 0.0;
  FCheckBoxRect := RectDouble(X, Y, X + 9.0 * FTextThickness,
    Y + 9.0 * FTextThickness);

  inherited Create(FCheckBoxRect.X1, FCheckBoxRect.Y1, FCheckBoxRect.X2,
    FCheckBoxRect.Y2, FlipY);

  FText := TAggGsvText.Create;
  FTextPoly := TAggConvStroke.Create(FText);

  FStatus := False;
  FCaption := L;
end;

destructor TAggCustomCheckBoxControl.Destroy;
begin
  FTextPoly.Free;
  FText.Free;
  inherited;
end;

procedure TAggCustomCheckBoxControl.SetTextThickness(T: Double);
begin
  FTextThickness := T;
end;

procedure TAggCustomCheckBoxControl.SetTextSize(H: Double; W: Double = 0);
begin
  FTextWidth := W;
  FTextHeight := H;
end;

procedure TAggCustomCheckBoxControl.SetCaption(Value: AnsiString);
begin
  FCaption := Value;
end;

procedure TAggCustomCheckBoxControl.SetClipBox(ClipBox: TRectDouble);
begin
  inherited;
end;

procedure TAggCustomCheckBoxControl.SetStatus(Value: Boolean);
begin
  FStatus := Value;
end;

function TAggCustomCheckBoxControl.InRect(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := (X >= FCheckBoxRect.X1) and (Y >= FCheckBoxRect.Y1) and
    (X <= FCheckBoxRect.X2) and (Y <= FCheckBoxRect.Y2);
end;

function TAggCustomCheckBoxControl.OnMouseButtonDown(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := False;
  if (X >= FCheckBoxRect.X1) and (Y >= FCheckBoxRect.Y1) and
    (X <= FCheckBoxRect.X2) and (Y <= FCheckBoxRect.Y2) then
  begin
    FStatus := not FStatus;
    Result := True;
  end
end;

function TAggCustomCheckBoxControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomCheckBoxControl.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
begin
  Result := False;
end;

function TAggCustomCheckBoxControl.OnArrowKeys(Left, Right, Down,
  Up: Boolean): Boolean;
begin
  Result := False;
end;

function TAggCustomCheckBoxControl.GetPathCount: Cardinal;
begin
  Result := 3;
end;

procedure TAggCustomCheckBoxControl.Rewind(PathID: Cardinal);
var
  D2, T: Double;
begin
  FIndex := PathID;

  case PathID of
    0: // Border
      begin
        FVertex := 0;

        FVX[0] := FCheckBoxRect.X1;
        FVY[0] := FCheckBoxRect.Y1;
        FVX[1] := FCheckBoxRect.X2;
        FVY[1] := FCheckBoxRect.Y1;
        FVX[2] := FCheckBoxRect.X2;
        FVY[2] := FCheckBoxRect.Y2;
        FVX[3] := FCheckBoxRect.X1;
        FVY[3] := FCheckBoxRect.Y2;
        FVX[4] := FCheckBoxRect.X1 + FTextThickness;
        FVY[4] := FCheckBoxRect.Y1 + FTextThickness;
        FVX[5] := FCheckBoxRect.X1 + FTextThickness;
        FVY[5] := FCheckBoxRect.Y2 - FTextThickness;
        FVX[6] := FCheckBoxRect.X2 - FTextThickness;
        FVY[6] := FCheckBoxRect.Y2 - FTextThickness;
        FVX[7] := FCheckBoxRect.X2 - FTextThickness;
        FVY[7] := FCheckBoxRect.Y1 + FTextThickness;
      end;

    1: // Text
      begin
        FText.SetText(FCaption);
        FText.SetStartPoint(FRect.X1 + 2 * FTextHeight,
          FRect.Y1 + FTextHeight / 5.0);
        FText.SetSize(FTextHeight, FTextWidth);

        FTextPoly.Width := FTextThickness;
        FTextPoly.LineJoin := ljRound;
        FTextPoly.LineCap := lcRound;

        FTextPoly.Rewind(0);
      end;

    2: // Active item
      begin
        FVertex := 0;

        D2 := (FRect.Y2 - FRect.Y1) * 0.5;
        T := FTextThickness * 1.5;

        FVX[0] := FCheckBoxRect.X1 + FTextThickness;
        FVY[0] := FCheckBoxRect.Y1 + FTextThickness;
        FVX[1] := FCheckBoxRect.X1 + D2;
        FVY[1] := FCheckBoxRect.Y1 + D2 - T;
        FVX[2] := FCheckBoxRect.X2 - FTextThickness;
        FVY[2] := FCheckBoxRect.Y1 + FTextThickness;
        FVX[3] := FCheckBoxRect.X1 + D2 + T;
        FVY[3] := FCheckBoxRect.Y1 + D2;
        FVX[4] := FCheckBoxRect.X2 - FTextThickness;
        FVY[4] := FCheckBoxRect.Y2 - FTextThickness;
        FVX[5] := FCheckBoxRect.X1 + D2;
        FVY[5] := FCheckBoxRect.Y1 + D2 + T;
        FVX[6] := FCheckBoxRect.X1 + FTextThickness;
        FVY[6] := FCheckBoxRect.Y2 - FTextThickness;
        FVX[7] := FCheckBoxRect.X1 + D2 - T;
        FVY[7] := FCheckBoxRect.Y1 + D2;
      end;
  end;
end;

function TAggCustomCheckBoxControl.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdLineTo;

  case FIndex of
    0:
      begin
        if (FVertex = 0) or (FVertex = 4) then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 8 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end;

    1:
      Cmd := FTextPoly.Vertex(X, Y);

    2:
      if FStatus then
      begin
        if FVertex = 0 then
          Cmd := CAggPathCmdMoveTo;

        if FVertex >= 8 then
          Cmd := CAggPathCmdStop;

        X^ := FVX[FVertex];
        Y^ := FVY[FVertex];

        Inc(FVertex);
      end
      else
        Cmd := CAggPathCmdStop;

  else
    Cmd := CAggPathCmdStop;
  end;

  if not IsStop(Cmd) then
    TransformXY(X, Y);

  Result := Cmd;
end;


{ TAggControlCheckBox }

constructor TAggControlCheckBox.Create(X, Y: Double; L: PAnsiChar;
  FlipY: Boolean = False);
begin
  inherited Create(X, Y, L, FlipY);

  FTextColor.Black;
  FInactiveColor.Black;
  FActiveColor.FromRgbaDouble(0.4, 0, 0);

  FColors[0] := @FInactiveColor;
  FColors[1] := @FTextColor;
  FColors[2] := @FActiveColor;
end;

procedure TAggControlCheckBox.SetTextColor(Value: TAggColor);
begin
  FTextColor := Value;
end;

procedure TAggControlCheckBox.SetInactiveColor(Value: TAggColor);
begin
  FInactiveColor := Value;
end;

procedure TAggControlCheckBox.SetActiveColor(Value: TAggColor);
begin
  FActiveColor := Value;
end;

function TAggControlCheckBox.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := nil;
  if Index <= 2 then
    Result := FColors[Index];
end;

end.
