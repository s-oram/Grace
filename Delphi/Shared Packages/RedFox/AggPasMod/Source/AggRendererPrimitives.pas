unit AggRendererPrimitives;

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
  AggRendererBase,
  AggColor,
  AggDdaLine,
  AggEllipseBresenham;

type
  TAggRendererPrimitives = class
  private
    FRenderBase: TAggRendererBase;
    FCurrent: TPointInteger;
    procedure SetFillColor(Value: TAggColor);
    procedure SetLineColor(Value: TAggColor);
  protected
    FFillColor, FLineColor: TAggColor;
  public
    constructor Create(RendBase: TAggRendererBase);

    function Coord(C: Double): Integer;

    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure Rectangle(Rect: TRectInteger); overload;
    procedure SolidRectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure SolidRectangle(Rect: TRectInteger); overload;
    procedure OutlinedRectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure OutlinedRectangle(Rect: TRectInteger); overload;

    procedure Ellipse(X, Y, Rx, Ry: Integer);
    procedure SolidEllipse(X, Y, Rx, Ry: Integer);
    procedure OutlinedEllipse(X, Y, Rx, Ry: Integer);

    procedure Line(X1, Y1, X2, Y2: Integer; Last: Boolean = False); overload;
    procedure Line(Point1, Point2: TPointInteger; Last: Boolean = False);
      overload;

    procedure MoveTo(X, Y: Integer); overload;
    procedure MoveTo(Point: TPointInteger); overload;
    procedure LineTo(X, Y: Integer; Last: Boolean = False); overload;
    procedure LineTo(Point: TPointInteger; Last: Boolean = False); overload;

    property RenderBase: TAggRendererBase read FRenderBase;

    property FillColor: TAggColor read FFillColor write SetFillColor;
    property LineColor: TAggColor read FLineColor write SetLineColor;
  end;

implementation


{ TAggRendererPrimitives }

constructor TAggRendererPrimitives.Create(RendBase: TAggRendererBase);
begin
  Assert(RendBase is TAggRendererBase);

  FRenderBase := RendBase;

  FCurrent := PointInteger(0);
end;

function TAggRendererPrimitives.Coord(C: Double): Integer;
begin
  Result := Trunc(C * CAggSubpixelSize);
end;

procedure TAggRendererPrimitives.SetFillColor(Value: TAggColor);
begin
  FFillColor := Value;
end;

procedure TAggRendererPrimitives.SetLineColor(Value: TAggColor);
begin
  FLineColor := Value;
end;

procedure TAggRendererPrimitives.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  with FRenderBase do
  begin
    BlendHorizontalLine(X1, Y1, X2 - 1, @FLineColor, CAggCoverFull);
    BlendVerticalLine(X2, Y1, Y2 - 1, @FLineColor, CAggCoverFull);
    BlendHorizontalLine(X1 + 1, Y2, X2, @FLineColor, CAggCoverFull);
    BlendVerticalLine(X1, Y1 + 1, Y2, @FLineColor, CAggCoverFull);
  end;
end;

procedure TAggRendererPrimitives.Rectangle(Rect: TRectInteger);
begin
  with Rect, FRenderBase do
  begin
    BlendHorizontalLine(X1, Y1, X2 - 1, @FLineColor, CAggCoverFull);
    BlendVerticalLine(X2, Y1, Y2 - 1, @FLineColor, CAggCoverFull);
    BlendHorizontalLine(X1 + 1, Y2, X2, @FLineColor, CAggCoverFull);
    BlendVerticalLine(X1, Y1 + 1, Y2, @FLineColor, CAggCoverFull);
  end;
end;

procedure TAggRendererPrimitives.SolidRectangle(X1, Y1, X2, Y2: Integer);
begin
  FRenderBase.BlendBar(X1, Y1, X2, Y2, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.SolidRectangle(Rect: TRectInteger);
begin
  FRenderBase.BlendBar(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2, @FFillColor,
    CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedRectangle(X1, Y1, X2, Y2: Integer);
begin
  Rectangle(X1, Y1, X2, Y2);
  FRenderBase.BlendBar(X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, @FFillColor,
    CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedRectangle(Rect: TRectInteger);
begin
  Rectangle(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
  FRenderBase.BlendBar(Rect.X1 + 1, Rect.Y1 + 1, Rect.X2 - 1, Rect.Y2 - 1,
    @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.Ellipse(X, Y, Rx, Ry: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta: TPointInteger;
begin
  Ei.Initialize(Rx, Ry);

  Delta := PointInteger(0, -Ry);

  repeat
    Inc(Delta.X, Ei.DeltaX);
    Inc(Delta.Y, Ei.DeltaY);

    with FRenderBase do
    begin
      BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);
      BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
      BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
      BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);
    end;

    Ei.IncOperator;
  until Delta.Y >= 0;
end;

procedure TAggRendererPrimitives.SolidEllipse(X, Y, Rx, Ry: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta, LastDelta: TPointInteger;
begin
  Ei.Initialize(Rx, Ry);

  Delta := PointInteger(0, -Ry);
  LastDelta := Delta;

  repeat
    Inc(Delta.X, Ei.DeltaX);
    Inc(Delta.Y, Ei.DeltaY);

    if Delta.Y <> LastDelta.Y then
    begin
      FRenderBase.BlendHorizontalLine(X - LastDelta.X, Y + LastDelta.Y,
        X + LastDelta.X, @FFillColor, CAggCoverFull);
      FRenderBase.BlendHorizontalLine(X - LastDelta.X, Y - LastDelta.Y,
        X + LastDelta.X, @FFillColor, CAggCoverFull);
    end;

    LastDelta := Delta;

    Ei.IncOperator;
  until Delta.Y >= 0;

  FRenderBase.BlendHorizontalLine(X - LastDelta.X, Y + LastDelta.Y,
    X + LastDelta.X, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedEllipse(X, Y, Rx, Ry: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta: TPointInteger;
begin
  Ei.Initialize(Rx, Ry);

  Delta := PointInteger(0, -Ry);
  repeat
    Inc(Delta.X, Ei.DeltaX);
    Inc(Delta.Y, Ei.DeltaY);

    FRenderBase.BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);

    if (Ei.DeltaY <> 0) and (Delta.X <> 0) then
    begin
      FRenderBase.BlendHorizontalLine(X - Delta.X + 1, Y + Delta.Y,
        X + Delta.X - 1, @FFillColor, CAggCoverFull);
      FRenderBase.BlendHorizontalLine(X - Delta.X + 1, Y - Delta.Y,
        X + Delta.X - 1, @FFillColor, CAggCoverFull);
    end;

    Ei.IncOperator;
  until Delta.Y >= 0;
end;

procedure TAggRendererPrimitives.Line(X1, Y1, X2, Y2: Integer;
  Last: Boolean = False);
var
  Li : TAggLineBresenhamInterpolator;
  Len: Cardinal;
begin
  Li.Initialize(X1, Y1, X2, Y2);

  Len := Li.Length;

  if Len = 0 then
  begin
    if Last then
      FRenderBase.BlendPixel(Li.LineLowResolution(X1),
        Li.LineLowResolution(Y1), @FLineColor, CAggCoverFull);

    Exit;
  end;

  if Last then
    Inc(Len);

  if Li.IsVer then
    repeat
      FRenderBase.BlendPixel(Li.X2, Li.Y1, @FLineColor, CAggCoverFull);

      Li.Vstep;

      Dec(Len);
    until Len = 0
  else
    repeat
      FRenderBase.BlendPixel(Li.X1, Li.Y2, @FLineColor, CAggCoverFull);

      Li.Hstep;

      Dec(Len);
    until Len = 0;
end;

procedure TAggRendererPrimitives.Line(Point1, Point2: TPointInteger; Last: Boolean);
var
  Li : TAggLineBresenhamInterpolator;
  Len: Cardinal;
begin
  Li.Initialize(Point1, Point2);

  Len := Li.Length;

  if Len = 0 then
  begin
    if Last then
      FRenderBase.BlendPixel(Li.LineLowResolution(Point1.X),
        Li.LineLowResolution(Point1.Y), @FLineColor, CAggCoverFull);

    Exit;
  end;

  if Last then
    Inc(Len);

  if Li.IsVer then
    repeat
      FRenderBase.BlendPixel(Li.X2, Li.Y1, @FLineColor, CAggCoverFull);

      Li.Vstep;

      Dec(Len);
    until Len = 0
  else
    repeat
      FRenderBase.BlendPixel(Li.X1, Li.Y2, @FLineColor, CAggCoverFull);

      Li.Hstep;

      Dec(Len);
    until Len = 0;
end;

procedure TAggRendererPrimitives.MoveTo(X, Y: Integer);
begin
  FCurrent := PointInteger(X, Y);
end;

procedure TAggRendererPrimitives.MoveTo(Point: TPointInteger);
begin
  FCurrent := Point;
end;

procedure TAggRendererPrimitives.LineTo(X, Y: Integer; Last: Boolean = False);
begin
  Line(FCurrent.X, FCurrent.Y, X, Y, Last);
  FCurrent := PointInteger(X, Y);
end;

procedure TAggRendererPrimitives.LineTo(Point: TPointInteger; Last: Boolean);
begin
  Line(FCurrent, Point, Last);
  FCurrent := Point;
end;

end.
