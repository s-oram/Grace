unit AggRendererMarkers;

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
  AggBasics,
  AggColor,
  AggRendererBase,
  AggRendererPrimitives,
  AggEllipseBresenham;

type
  TAggMarker = (
    meSquare, meDiamond, meCircle, meCrossedCircle, meSemiEllipseLeft,
    meSemiEllipseRight, meSemiEllipseUp, meSemiEllipseDown,
    meTriangleLeft, meTriangleRight, meTriangleUp, meTriangleDown,
    meFourRays, meCross, meX, meDash, meDot, mePixel);

  TAggRendererMarkers = class(TAggRendererPrimitives)
  public
    constructor Create(Rbuf: TAggRendererBase);

    function Visible(X, Y, R: Integer): Boolean;

    procedure Square(X, Y, R: Integer);
    procedure Diamond(X, Y, R: Integer);

    procedure Circle(X, Y, R: Integer);
    procedure CrossedCircle(X, Y, R: Integer);

    procedure SemiEllipseLeft(X, Y, R: Integer);
    procedure SemiEllipseRight(X, Y, R: Integer);
    procedure SemiEllipseUp(X, Y, R: Integer);
    procedure SemiEllipseDown(X, Y, R: Integer);

    procedure TriangleLeft(X, Y, R: Integer);
    procedure TriangleRight(X, Y, R: Integer);
    procedure TriangleUp(X, Y, R: Integer);
    procedure TriangleDown(X, Y, R: Integer);

    procedure FourRays(X, Y, R: Integer);

    procedure Cross(X, Y, R: Integer);
    procedure Xing(X, Y, R: Integer);
    procedure Dash(X, Y, R: Integer);
    procedure Dot(X, Y, R: Integer);
    procedure Pixel(X, Y, R: Integer);

    procedure Marker(X, Y, R: Integer; MarkerType: TAggMarker);

    procedure Markers(N: Integer; X, Y: PInteger; R: Integer;
      MarkerType: TAggMarker); overload;
    procedure Markers(N: Integer; X, Y, R: PInteger;
      MarkerType: TAggMarker); overload;
    procedure Markers(N: Integer; X, Y, R: PInteger; Fc: PAggColor;
      MarkerType: TAggMarker); overload;
    procedure Markers(N: Integer; X, Y, R: PInteger; Fc, Lc: PAggColor;
      MarkerType: TAggMarker); overload;
  end;

implementation


{ TAggRendererMarkers }

constructor TAggRendererMarkers.Create(Rbuf: TAggRendererBase);
begin
  Assert(Rbuf is TAggRendererBase);
  inherited Create(Rbuf);
end;

function TAggRendererMarkers.Visible(X, Y, R: Integer): Boolean;
var
  Rc: TRectInteger;
begin
  Rc := RectInteger(X - R, Y - R, X + Y, Y + R);

  Result := Rc.Clip(RenderBase.BoundingClipBox^);
end;

procedure TAggRendererMarkers.Square(X, Y, R: Integer);
begin
  if Visible(X, Y, R) then
    if R <> 0 then
      OutlinedRectangle(X - R, Y - R, X + R, Y + R)
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Diamond(X, Y, R: Integer);
var
  Delta: TPointInteger;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Delta := PointInteger(0, -R);

      repeat
        RenderBase.BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);

        if Delta.X <> 0 then
        begin
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y + Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y - Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);
        end;

        Inc(Delta.Y);
        Inc(Delta.X);
      until Delta.Y > 0;
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Circle(X, Y, R: Integer);
begin
  if Visible(X, Y, R) then
    if R <> 0 then
      OutlinedEllipse(X, Y, R, R)
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.CrossedCircle(X, Y, R: Integer);
var
  R6: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      OutlinedEllipse(X, Y, R, R);

      R6 := R + ShrInt32(R, 1);

      if R <= 2 then
        Inc(R6);

      R := ShrInt32(R, 1);

      RenderBase.BlendHorizontalLine(X - R6, Y, X - R, @FLineColor,
        CAggCoverFull);
      RenderBase.BlendHorizontalLine(X + R, Y, X + R6, @FLineColor,
        CAggCoverFull);
      RenderBase.BlendVerticalLine(X, Y - R6, Y - R, @FLineColor,
        CAggCoverFull);
      RenderBase.BlendVerticalLine(X, Y + R, Y + R6, @FLineColor,
        CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseLeft(X, Y, R: Integer);
var
  R8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      R8 := R * 4 div 5;
      Delta := PointInteger(0, -R);

      Ei.Initialize(R * 3 div 5, R + R8);

      repeat
        Inc(Delta.X, Ei.DeltaX);
        Inc(Delta.Y, Ei.DeltaY);

        RenderBase.BlendPixel(X + Delta.Y, Y + Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.Y, Y - Delta.X, @FLineColor,
          CAggCoverFull);

        if (Ei.DeltaY <> 0) and (Delta.X <> 0) then
          RenderBase.BlendVerticalLine(X + Delta.Y, Y - Delta.X + 1,
            Y + Delta.X - 1, @FFillColor, CAggCoverFull);

        Ei.IncOperator;
      until Delta.Y >= R8;

      RenderBase.BlendVerticalLine(X + Delta.Y, Y - Delta.X, Y + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseRight(X, Y, R: Integer);
var
  R8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      R8 := R * 4 div 5;
      Delta := PointInteger(0, -R);

      Ei.Initialize(R * 3 div 5, R + R8);

      repeat
        Inc(Delta.X, Ei.DeltaX);
        Inc(Delta.Y, Ei.DeltaY);

        RenderBase.BlendPixel(X - Delta.Y, Y + Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.Y, Y - Delta.X, @FLineColor,
          CAggCoverFull);

        if (Ei.DeltaY <> 0) and (Delta.X <> 0) then
          RenderBase.BlendVerticalLine(X - Delta.Y, Y - Delta.X + 1,
            Y + Delta.X - 1, @FFillColor, CAggCoverFull);

        Ei.IncOperator;
      until Delta.Y >= R8;

      RenderBase.BlendVerticalLine(X - Delta.Y, Y - Delta.X, Y + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseUp(X, Y, R: Integer);
var
  R8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      R8 := R * 4 div 5;
      Delta := PointInteger(0, -R);

      Ei.Initialize(R * 3 div 5, R + R8);

      repeat
        Inc(Delta.X, Ei.DeltaX);
        Inc(Delta.Y, Ei.DeltaY);

        RenderBase.BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);

        if (Ei.DeltaY <> 0) and (Delta.X <> 0) then
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y - Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);

        Ei.IncOperator;
      until Delta.Y >= R8;

      RenderBase.BlendHorizontalLine(X - Delta.X, Y - Delta.Y - 1, X + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseDown;
var
  R8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      R8 := R * 4 div 5;
      Delta := PointInteger(0, -R);

      Ei.Initialize(R * 3 div 5, R + R8);

      repeat
        Inc(Delta.X, Ei.DeltaX);
        Inc(Delta.Y, Ei.DeltaY);

        RenderBase.BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);

        if (Ei.DeltaY <> 0) and (Delta.X <> 0) then
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y + Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);

        Ei.IncOperator;
      until Delta.Y >= R8;

      RenderBase.BlendHorizontalLine(X - Delta.X, Y + Delta.Y + 1, X + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleLeft(X, Y, R: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Delta := PointInteger(0, -R);
      Flip := 0;
      R6 := R * 3 div 5;

      repeat
        RenderBase.BlendPixel(X + Delta.Y, Y - Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.Y, Y + Delta.X, @FLineColor,
          CAggCoverFull);

        if Delta.X <> 0 then
          RenderBase.BlendVerticalLine(X + Delta.Y, Y - Delta.X + 1,
            Y + Delta.X - 1, @FFillColor, CAggCoverFull);

        Inc(Delta.Y);
        Inc(Delta.X, Flip);

        Flip := Flip xor 1;
      until Delta.Y >= R6;

      RenderBase.BlendVerticalLine(X + Delta.Y, Y - Delta.X, Y + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleRight(X, Y, R: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Delta.Y := -R;
      Delta.X := 0;
      Flip := 0;
      R6 := R * 3 div 5;

      repeat
        RenderBase.BlendPixel(X - Delta.Y, Y - Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.Y, Y + Delta.X, @FLineColor,
          CAggCoverFull);

        if Delta.X <> 0 then
          RenderBase.BlendVerticalLine(X - Delta.Y, Y - Delta.X + 1,
            Y + Delta.X - 1, @FFillColor, CAggCoverFull);

        Inc(Delta.Y);
        Inc(Delta.X, Flip);

        Flip := Flip xor 1;
      until Delta.Y >= R6;

      RenderBase.BlendVerticalLine(X - Delta.Y, Y - Delta.X, Y + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleUp(X, Y, R: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Delta.Y := -R;
      Delta.X := 0;
      Flip := 0;
      R6 := R * 3 div 5;

      repeat
        RenderBase.BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);

        if Delta.X <> 0 then
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y - Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);

        Inc(Delta.Y);
        Inc(Delta.X, Flip);

        Flip := Flip xor 1;
      until Delta.Y >= R6;

      RenderBase.BlendHorizontalLine(X - Delta.X, Y - Delta.Y, X + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleDown(X, Y, R: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Delta.Y := -R;
      Delta.X := 0;
      Flip := 0;
      R6 := R * 3 div 5;

      repeat
        RenderBase.BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);

        if Delta.X <> 0 then
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y + Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);

        Inc(Delta.Y);
        Inc(Delta.X, Flip);

        Flip := Flip xor 1;
      until Delta.Y >= R6;

      RenderBase.BlendHorizontalLine(X - Delta.X, Y + Delta.Y, X + Delta.X,
        @FLineColor, CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.FourRays(X, Y, R: Integer);
var
  Delta: TPointInteger;
  Flip, R3: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Delta.Y := -R;
      Delta.X := 0;
      Flip := 0;
      R3 := -(R div 3);

      repeat
        RenderBase.BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.Y, Y - Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X + Delta.Y, Y + Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.Y, Y - Delta.X, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendPixel(X - Delta.Y, Y + Delta.X, @FLineColor,
          CAggCoverFull);

        if Delta.X <> 0 then
        begin
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y + Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);
          RenderBase.BlendHorizontalLine(X - Delta.X + 1, Y - Delta.Y,
            X + Delta.X - 1, @FFillColor, CAggCoverFull);
          RenderBase.BlendVerticalLine(X + Delta.Y, Y - Delta.X + 1,
            Y + Delta.X - 1, @FFillColor, CAggCoverFull);
          RenderBase.BlendVerticalLine(X - Delta.Y, Y - Delta.X + 1,
            Y + Delta.X - 1, @FFillColor, CAggCoverFull);
        end;

        Inc(Delta.Y);
        Inc(Delta.X, Flip);

        Flip := Flip xor 1;
      until Delta.Y > R3;

      SolidRectangle(X + R3 + 1, Y + R3 + 1, X - R3 - 1, Y - R3 - 1);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Cross(X, Y, R: Integer);
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      RenderBase.BlendVerticalLine(X, Y - R, Y + R, @FLineColor, CAggCoverFull);
      RenderBase.BlendHorizontalLine(X - R, Y, X + R, @FLineColor,
        CAggCoverFull);
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Xing(X, Y, R: Integer);
var
  Dy: Integer;
begin
  if Visible(X, Y, R) then
    if R <> 0 then
    begin
      Dy := -R * 7 div 10;

      repeat
        RenderBase.BlendPixel(X + Dy, Y + Dy, @FLineColor, CAggCoverFull);
        RenderBase.BlendPixel(X - Dy, Y + Dy, @FLineColor, CAggCoverFull);
        RenderBase.BlendPixel(X + Dy, Y - Dy, @FLineColor, CAggCoverFull);
        RenderBase.BlendPixel(X - Dy, Y - Dy, @FLineColor, CAggCoverFull);

        Inc(Dy);
      until Dy >= 0;
    end
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Dash(X, Y, R: Integer);
begin
  if Visible(X, Y, R) then
    if R <> 0 then
      RenderBase.BlendHorizontalLine(X - R, Y, X + R, @FLineColor, CAggCoverFull)
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Dot(X, Y, R: Integer);
begin
  if Visible(X, Y, R) then
    if R <> 0 then
      SolidEllipse(X, Y, R, R)
    else
      RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Pixel(X, Y, R: Integer);
begin
  RenderBase.BlendPixel(X, Y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Marker(X, Y, R: Integer; MarkerType: TAggMarker);
begin
  case MarkerType of
    meSquare:
      Square(X, Y, R);
    meDiamond:
      Diamond(X, Y, R);
    MeCircle:
      Circle(X, Y, R);
    meCrossedCircle:
      CrossedCircle(X, Y, R);
    meSemiEllipseLeft:
      SemiEllipseLeft(X, Y, R);
    meSemiEllipseRight:
      SemiEllipseRight(X, Y, R);
    meSemiEllipseUp:
      SemiEllipseUp(X, Y, R);
    meSemiEllipseDown:
      SemiEllipseDown(X, Y, R);
    meTriangleLeft:
      TriangleLeft(X, Y, R);
    meTriangleRight:
      TriangleRight(X, Y, R);
    meTriangleUp:
      TriangleUp(X, Y, R);
    meTriangleDown:
      TriangleDown(X, Y, R);
    meFourRays:
      FourRays(X, Y, R);
    meCross:
      Cross(X, Y, R);
    meX:
      Xing(X, Y, R);
    meDash:
      Dash(X, Y, R);
    meDot:
      Dot(X, Y, R);
    mePixel:
      Pixel(X, Y, R);
  end;
end;

procedure TAggRendererMarkers.Markers(N: Integer; X, Y: PInteger; R: Integer;
  MarkerType: TAggMarker);
begin
  if N <= 0 then
    Exit;

  if R = 0 then
  begin
    repeat
      RenderBase.BlendPixel(X^, Y^, @FFillColor, CAggCoverFull);

      Inc(X);
      Inc(Y);
      Dec(N);
    until N = 0;

    Exit;
  end;

  case MarkerType of
    meSquare:
      repeat
        Square(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meDiamond:
      repeat
        Diamond(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    MeCircle:
      repeat
        Circle(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meCrossedCircle:
      repeat
        CrossedCircle(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meSemiEllipseLeft:
      repeat
        SemiEllipseLeft(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meSemiEllipseRight:
      repeat
        SemiEllipseRight(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meSemiEllipseUp:
      repeat
        SemiEllipseUp(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meSemiEllipseDown:
      repeat
        SemiEllipseDown(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meTriangleLeft:
      repeat
        TriangleLeft(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meTriangleRight:
      repeat
        TriangleRight(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meTriangleUp:
      repeat
        TriangleUp(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meTriangleDown:
      repeat
        TriangleDown(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meFourRays:
      repeat
        FourRays(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meCross:
      repeat
        Cross(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meX:
      repeat
        Xing(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meDash:
      repeat
        Dash(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    meDot:
      repeat
        Dot(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;

    mePixel:
      repeat
        Pixel(X^, Y^, R);

        Inc(X);
        Inc(Y);
        Dec(N);
      until N = 0;
  end;
end;

procedure TAggRendererMarkers.Markers(N: Integer; X, Y, R: PInteger;
  MarkerType: TAggMarker);
begin
  if N <= 0 then
    Exit;

  case MarkerType of
    meSquare:
      repeat
        Square(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meDiamond:
      repeat
        Diamond(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    MeCircle:
      repeat
        Circle(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meCrossedCircle:
      repeat
        CrossedCircle(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meSemiEllipseLeft:
      repeat
        SemiEllipseLeft(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meSemiEllipseRight:
      repeat
        SemiEllipseRight(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meSemiEllipseUp:
      repeat
        SemiEllipseUp(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meSemiEllipseDown:
      repeat
        SemiEllipseDown(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meTriangleLeft:
      repeat
        TriangleLeft(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meTriangleRight:
      repeat
        TriangleRight(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meTriangleUp:
      repeat
        TriangleUp(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meTriangleDown:
      repeat
        TriangleDown(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meFourRays:
      repeat
        FourRays(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meCross:
      repeat
        Cross(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meX:
      repeat
        Xing(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meDash:
      repeat
        Dash(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    meDot:
      repeat
        Dot(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;

    mePixel:
      repeat
        Pixel(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Dec(N);
      until N = 0;
  end;
end;

procedure TAggRendererMarkers.Markers(N: Integer; X, Y, R: PInteger; Fc: PAggColor;
  MarkerType: TAggMarker);
begin
  if N <= 0 then
    Exit;

  case MarkerType of
    meSquare:
      repeat
        FillColor := Fc^;

        Square(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meDiamond:
      repeat
        FillColor := Fc^;

        Diamond(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    MeCircle:
      repeat
        FillColor := Fc^;

        Circle(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meCrossedCircle:
      repeat
        FillColor := Fc^;

        CrossedCircle(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseLeft:
      repeat
        FillColor := Fc^;

        SemiEllipseLeft(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseRight:
      repeat
        FillColor := Fc^;

        SemiEllipseRight(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseUp:
      repeat
        FillColor := Fc^;

        SemiEllipseUp(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseDown:
      repeat
        FillColor := Fc^;

        SemiEllipseDown(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleLeft:
      repeat
        FillColor := Fc^;

        TriangleLeft(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleRight:
      repeat
        FillColor := Fc^;

        TriangleRight(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleUp:
      repeat
        FillColor := Fc^;

        TriangleUp(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleDown:
      repeat
        FillColor := Fc^;

        TriangleDown(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meFourRays:
      repeat
        FillColor := Fc^;

        FourRays(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meCross:
      repeat
        FillColor := Fc^;

        Cross(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meX:
      repeat
        FillColor := Fc^;

        Xing(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meDash:
      repeat
        FillColor := Fc^;

        Dash(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meDot:
      repeat
        FillColor := Fc^;

        Dot(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    mePixel:
      repeat
        FillColor := Fc^;

        Pixel(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;
  end;
end;

procedure TAggRendererMarkers.Markers(N: Integer; X, Y, R: PInteger;
  Fc, Lc: PAggColor; MarkerType: TAggMarker);
begin
  if N <= 0 then
    Exit;

  case MarkerType of
    meSquare:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Square(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meDiamond:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Diamond(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    MeCircle:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Circle(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meCrossedCircle:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        CrossedCircle(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseLeft:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        SemiEllipseLeft(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseRight:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        SemiEllipseRight(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseUp:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        SemiEllipseUp(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meSemiEllipseDown:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        SemiEllipseDown(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleLeft:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        TriangleLeft(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleRight:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        TriangleRight(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleUp:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        TriangleUp(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meTriangleDown:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        TriangleDown(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meFourRays:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        FourRays(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meCross:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Cross(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meX:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Xing(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meDash:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Dash(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    meDot:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Dot(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;

    mePixel:
      repeat
        FillColor := Fc^;
        LineColor := Lc^;

        Pixel(X^, Y^, R^);

        Inc(X);
        Inc(Y);
        Inc(R);
        Inc(PtrComp(Fc), SizeOf(TAggColor));
        Inc(PtrComp(Lc), SizeOf(TAggColor));
        Dec(N);
      until N = 0;
  end;
end;

end.
