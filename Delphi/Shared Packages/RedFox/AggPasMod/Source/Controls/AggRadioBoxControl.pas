unit AggRadioBoxControl;

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
  SysUtils,
  AggBasics,
  AggControl,
  AggConvStroke,
  AggGsvText,
  AggColor,
  AggEllipse,
  AggMath,
  AggMathStroke;

type
  TAggCustomRadioBoxControl = class(TAggCustomAggControl)
  private
    FBorderWidth, FBorderExtra, FTextThickness, FTextHeight,
      FTextWidth: Double;

    FItems: array of AnsiString;

    FCurItem: Integer;

    FBounds: TRectDouble;
    FDeltaY: Double;

    FVertex: array [0..8] of TPointDouble;

    FDrawItem: Cardinal;

    FCircle: TAggCircle;
    FEllipsePoly: TAggConvStroke;
    FText: TAggGsvText;
    FTextPoly: TAggConvStroke;

    FIndex, FVertexIndex: Cardinal;

    procedure CalcRenderingBox;
    procedure SetTextThickness(Value: Double);
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);
    destructor Destroy; override;

    procedure SetBorderWidth(T: Double; Extra: Double = 0.0);
    procedure SetTextSize(H: Double; W: Double = 0.0);
    procedure SetClipBox(ClipBox: TRectDouble); override;

    procedure Clear;
    procedure AddItem(Text: AnsiString);
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(I: Integer);

    function InRect(X, Y: Double): Boolean; override;

    function OnMouseButtonDown(X, Y: Double): Boolean; override;
    function OnMouseButtonUp(X, Y: Double): Boolean; override;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; override;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; override;

    // Vertex source interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property TextThickness: Double read FTextThickness write SetTextThickness;
  end;

  TAggControlRadioBox = class(TAggCustomRadioBoxControl)
  private
    FBackgroundColor: TAggColor;
    FBorderColor: TAggColor;
    FTextColor: TAggColor;
    FInactiveColor: TAggColor;
    FActiveColor: TAggColor;
    FColors: array [0..4] of PAggColor;

    procedure SetBackgroundColor(Value: TAggColor);
    procedure SetBorderColor(Value: TAggColor);
    procedure SetTextColor(Value: TAggColor);
    procedure SetInactiveColor(Value: TAggColor);
    procedure SetActiveColor(Value: TAggColor);
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean = False);

    property BackgroundColor: TAggColor read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TAggColor read FBorderColor write SetBorderColor;
    property TextColor: TAggColor read FTextColor write SetTextColor;
    property InactiveColor: TAggColor read FInactiveColor write SetInactiveColor;
    property ActiveColor: TAggColor read FActiveColor write SetActiveColor;
  end;

implementation


{ TAggCustomRadioBoxControl }

constructor TAggCustomRadioBoxControl.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FCircle := TAggCircle.Create;
  FText := TAggGsvText.Create;

  FEllipsePoly := TAggConvStroke.Create(FCircle);
  FTextPoly := TAggConvStroke.Create(FText);

  FBorderWidth := 1.0;
  FBorderExtra := 0.0;
  FTextThickness := 1.5;
  FTextHeight := 9.0;
  FTextWidth := 0.0;

  FCurItem := -1;

  FIndex := 0;
  FVertexIndex := 0;

  CalcRenderingBox;
end;

destructor TAggCustomRadioBoxControl.Destroy;
begin
  FCircle.Free;
  FEllipsePoly.Free;
  FTextPoly.Free;

  FText.Free;

  inherited;
end;

procedure TAggCustomRadioBoxControl.SetBorderWidth;
begin
  FBorderWidth := T;
  FBorderExtra := Extra;

  CalcRenderingBox;
end;

procedure TAggCustomRadioBoxControl.SetTextThickness(Value: Double);
begin
  FTextThickness := Value;
end;

procedure TAggCustomRadioBoxControl.SetTextSize(H: Double; W: Double = 0.0);
begin
  FTextWidth := W;
  FTextHeight := H;
end;

procedure TAggCustomRadioBoxControl.AddItem(Text: AnsiString);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[Length(FItems) - 1] := Text;
end;

function TAggCustomRadioBoxControl.GetCurrentItem;
begin
  Result := FCurItem;
end;

procedure TAggCustomRadioBoxControl.SetClipBox(ClipBox: TRectDouble);
begin
  inherited;
  CalcRenderingBox;
end;

procedure TAggCustomRadioBoxControl.SetCurrentItem;
begin
  FCurItem := I;
end;

function TAggCustomRadioBoxControl.InRect(X, Y: Double): Boolean;
begin
  InverseTransformXY(@X, @Y);

  Result := (X >= FRect.X1) and (X <= FRect.X2) and (Y >= FRect.Y1) and (Y <= FRect.Y2);
end;

function TAggCustomRadioBoxControl.OnMouseButtonDown(X, Y: Double): Boolean;
var
  I: Cardinal;
  Xp, Yp: Double;
begin
  InverseTransformXY(@X, @Y);

  for I := 0 to Length(FItems) - 1 do
  begin
    Xp := FBounds.X1 + FDeltaY / 1.3;
    Yp := FBounds.Y1 + FDeltaY * I + FDeltaY / 1.3;

    if CalculateDistance(X, Y, Xp, Yp) <= FTextHeight / 1.5 then
    begin
      FCurItem := I;

      Result := True;

      Exit;
    end;
  end;

  Result := False;
end;

function TAggCustomRadioBoxControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomRadioBoxControl.OnMouseMove(X, Y: Double;
  ButtonFlag: Boolean): Boolean;
begin
  Result := False;
end;

function TAggCustomRadioBoxControl.OnArrowKeys(Left, Right, Down,
  Up: Boolean): Boolean;
begin
  if FCurItem >= 0 then
  begin
    if Up or Right then
    begin
      Inc(FCurItem);

      if FCurItem >= Length(FItems) then
        FCurItem := 0;

      Result := True;

      Exit;
    end;

    if Down or Left then
    begin
      Dec(FCurItem);

      if FCurItem < 0 then
        FCurItem := Length(FItems) - 1;

      Result := True;

      Exit;
    end;
  end;

  Result := False;
end;

function TAggCustomRadioBoxControl.GetPathCount: Cardinal;
begin
  Result := 5;
end;

procedure TAggCustomRadioBoxControl.Rewind(PathID: Cardinal);
begin
  FIndex := PathID;
  FDeltaY := FTextHeight * 2.0;

  FDrawItem := 0;

  case PathID of
    0: // Background
      begin
        FVertexIndex := 0;

        FVertex[0] := PointDouble(FRect.X1 - FBorderExtra, FRect.Y1 - FBorderExtra);
        FVertex[1] := PointDouble(FRect.X2 + FBorderExtra, FRect.Y1 - FBorderExtra);
        FVertex[2] := PointDouble(FRect.X2 + FBorderExtra, FRect.Y2 + FBorderExtra);
        FVertex[3] := PointDouble(FRect.X1 - FBorderExtra, FRect.Y2 + FBorderExtra);
      end;

    1: // Border
      begin
        FVertexIndex := 0;

        FVertex[0] := PointDouble(FRect.X1, FRect.Y1);
        FVertex[1] := PointDouble(FRect.X2, FRect.Y1);
        FVertex[2] := PointDouble(FRect.X2, FRect.Y2);
        FVertex[3] := PointDouble(FRect.X1, FRect.Y2);
        FVertex[4] := PointDouble(FRect.X1 + FBorderWidth, FRect.Y1 + FBorderWidth);
        FVertex[5] := PointDouble(FRect.X1 + FBorderWidth, FRect.Y2 - FBorderWidth);
        FVertex[6] := PointDouble(FRect.X2 - FBorderWidth, FRect.Y2 - FBorderWidth);
        FVertex[7] := PointDouble(FRect.X2 - FBorderWidth, FRect.Y1 + FBorderWidth);
      end;

    2: // Text
      begin
        if Length(FItems) > 0 then
        begin
          FText.SetText(FItems[0]);
          FText.SetStartPoint(FBounds.X1 + FDeltaY * 1.5, FBounds.Y1 +
            FDeltaY * 0.5);
          FText.SetSize(FTextHeight, FTextWidth);
        end;

        FTextPoly.Width := FTextThickness;
        FTextPoly.LineJoin := ljRound;
        FTextPoly.LineCap := lcRound;

        FTextPoly.Rewind(0);
      end;

    3: // Inactive items
      begin
        FCircle.Initialize(FBounds.X1 + FDeltaY * 0.77, FBounds.Y1 +
          FDeltaY * 0.77, FTextHeight / 1.5, 32);

        FEllipsePoly.Width := FTextThickness;
        FEllipsePoly.Rewind(0);
      end;

    4: // Active Item
      if FCurItem >= 0 then
      begin
        FCircle.Initialize(FBounds.X1 + FDeltaY * 0.77, FBounds.Y1 +
          FDeltaY * FCurItem + FDeltaY * 0.77, FTextHeight * 0.5, 32);

        FCircle.Rewind(0);
      end;
  end;
end;

function TAggCustomRadioBoxControl.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := CAggPathCmdLineTo;

  case FIndex of
    0:
      begin
        if FVertexIndex = 0 then
          Cmd := CAggPathCmdMoveTo;

        if FVertexIndex >= 4 then
          Cmd := CAggPathCmdStop;

        X^ := FVertex[FVertexIndex].X;
        Y^ := FVertex[FVertexIndex].Y;

        Inc(FVertexIndex);
      end;

    1:
      begin
        if (FVertexIndex = 0) or (FVertexIndex = 4) then
          Cmd := CAggPathCmdMoveTo;

        if FVertexIndex >= 8 then
          Cmd := CAggPathCmdStop;

        X^ := FVertex[FVertexIndex].X;
        Y^ := FVertex[FVertexIndex].Y;

        Inc(FVertexIndex);
      end;

    2:
      begin
        Cmd := FTextPoly.Vertex(X, Y);

        if IsStop(Cmd) then
        begin
          Inc(FDrawItem);

          if FDrawItem >= Length(FItems) then
          begin
            if not IsStop(Cmd) then
              TransformXY(X, Y);

            Result := Cmd;
            Exit;
          end
          else
          begin
            FText.SetText(FItems[FDrawItem]);
            FText.SetStartPoint(FBounds.X1 + FDeltaY * 1.5,
              FBounds.Y1 + FDeltaY * (FDrawItem + 1) - FDeltaY * 0.5);

            FTextPoly.Rewind(0);

            Cmd := FTextPoly.Vertex(X, Y);
          end;
        end;
      end;

    3:
      begin
        Cmd := FEllipsePoly.Vertex(X, Y);

        if IsStop(Cmd) then
        begin
          Inc(FDrawItem);

          if FDrawItem >= Length(FItems) then
          begin
            if not IsStop(Cmd) then
              TransformXY(X, Y);

            Result := Cmd;
            Exit;
          end
          else
          begin
            FCircle.Initialize(FBounds.X1 + FDeltaY * 0.77, FBounds.Y1 +
              FDeltaY * FDrawItem + FDeltaY * 0.77,
              FTextHeight / 1.5, 32);

            FEllipsePoly.Rewind(0);

            Cmd := FEllipsePoly.Vertex(X, Y);
          end;
        end;
      end;

    4:
      if FCurItem >= 0 then
        Cmd := FCircle.Vertex(X, Y)
      else
        Cmd := CAggPathCmdStop;

  else
    Cmd := CAggPathCmdStop;
  end;
            if not IsStop(Cmd) then
              TransformXY(X, Y);

            Result := Cmd;
end;

procedure TAggCustomRadioBoxControl.CalcRenderingBox;
begin
  FBounds.X1 := FRect.X1 + FBorderWidth;
  FBounds.Y1 := FRect.Y1 + FBorderWidth;
  FBounds.X2 := FRect.X2 - FBorderWidth;
  FBounds.Y2 := FRect.Y2 - FBorderWidth;
end;

procedure TAggCustomRadioBoxControl.Clear;
begin
  SetLength(FItems, 0);
end;


{ TAggControlRadioBox }

constructor TAggControlRadioBox.Create;
begin
  inherited Create(X1, Y1, X2, Y2, FlipY);

  FBackgroundColor.FromRgbaDouble(1, 1, 0.9);
  FBorderColor.Black;
  FTextColor.Black;
  FInactiveColor.Black;
  FActiveColor.FromRgbaDouble(0.4, 0, 0);

  FColors[0] := @FBackgroundColor;
  FColors[1] := @FBorderColor;
  FColors[2] := @FTextColor;
  FColors[3] := @FInactiveColor;
  FColors[4] := @FActiveColor;
end;

procedure TAggControlRadioBox.SetBackgroundColor(Value: TAggColor);
begin
  FBackgroundColor := Value;
end;

procedure TAggControlRadioBox.SetBorderColor(Value: TAggColor);
begin
  FBorderColor := Value;
end;

procedure TAggControlRadioBox.SetTextColor(Value: TAggColor);
begin
  FTextColor := Value;
end;

procedure TAggControlRadioBox.SetInactiveColor(Value: TAggColor);
begin
  FInactiveColor := Value;
end;

procedure TAggControlRadioBox.SetActiveColor(Value: TAggColor);
begin
  FActiveColor := Value;
end;

function TAggControlRadioBox.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := nil;
  if Index <= 4 then
    Result := FColors[Index];
end;

end.
