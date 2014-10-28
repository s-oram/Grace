unit AggVpGenClipPolyline;

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
  AggVpGen,
  AggVertexSource;

type
  TAggClippingFlag = (cfX1, cfX2, cfY1, cfY2);
  TAggClippingFlags = set of TAggClippingFlag;

  TAggVpgenClipPolyline = class(TAggCustomVpgen)
  private
    FClipBox: TRectDouble;

    FPoint: array [0..1] of TPointDouble;
    FF1: TAggClippingFlags;
    FF2: TAggClippingFlags;

    FX, FY: array [0..1] of Double;
    FCmd: array [0..1] of Cardinal;

    FNumVertices, FVertex: Cardinal;
  protected
    function MovePoint(X, Y: PDouble; var Flags: TAggClippingFlags): Boolean;

    // Determine the clipping code of the vertex according to the
    // Cyrus-Beck line clipping algorithm
    function ClippingFlagsX(X: Double): TAggClippingFlags;
    function ClippingFlagsY(Y: Double): TAggClippingFlags;
    function ClippingFlags(X, Y: Double): TAggClippingFlags;

    procedure ClipLineSegment;

    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;

    function GetAutoClose: Boolean;
    function GetAutoUnclose: Boolean;
  public
    constructor Create; override;

    procedure Reset; override;
    procedure MoveTo(X, Y: Double); override;
    procedure LineTo(X, Y: Double); override;

    function Vertex(X, Y: PDouble): Cardinal; override;

    procedure SetClipBox(X1, Y1, X2, Y2: Double); overload;
    procedure SetClipBox(Bounds: TRectDouble); overload;

    property X1: Double read GetX1;
    property Y1: Double read GetY1;
    property X2: Double read GetX2;
    property Y2: Double read GetY2;
  end;

implementation

const
  CClipEpsilon = 1E-10;


{ TAggVpgenClipPolyline }

constructor TAggVpgenClipPolyline.Create;
begin
  FClipBox := RectDouble(0, 0, 1, 1);

  FPoint[0] := PointDouble(0);
  FPoint[1] := PointDouble(0);
  FF1 := [];
  FF2 := [];

  FNumVertices := 0;
  FVertex := 0;
end;

procedure TAggVpgenClipPolyline.SetClipBox(X1, Y1, X2, Y2: Double);
begin
  FClipBox := RectDouble(X1, Y1, X2, Y2);
  FClipBox.Normalize;
end;

procedure TAggVpgenClipPolyline.SetClipBox(Bounds: TRectDouble);
begin
  FClipBox := Bounds;
  FClipBox.Normalize;
end;

function TAggVpgenClipPolyline.GetX1;
begin
  Result := FClipBox.X1;
end;

function TAggVpgenClipPolyline.GetY1;
begin
  Result := FClipBox.Y1;
end;

function TAggVpgenClipPolyline.GetX2;
begin
  Result := FClipBox.X2;
end;

function TAggVpgenClipPolyline.GetY2;
begin
  Result := FClipBox.Y2;
end;

function TAggVpgenClipPolyline.GetAutoClose;
begin
  Result := False;
end;

function TAggVpgenClipPolyline.GetAutoUnclose;
begin
  Result := True;
end;

procedure TAggVpgenClipPolyline.Reset;
begin
  FVertex := 0;
  FNumVertices := 0;
end;

procedure TAggVpgenClipPolyline.MoveTo;
begin
  FVertex := 0;
  FNumVertices := 0;

  FF1 := ClippingFlags(X, Y);

  if FF1 = [] then
  begin
    FX[0] := X;
    FY[0] := Y;

    FCmd[0] := CAggPathCmdMoveTo;

    FNumVertices := 1;
  end;

  FPoint[0] := PointDouble(X, Y);
end;

procedure TAggVpgenClipPolyline.LineTo;
var
  F: TAggClippingFlags;
begin
  FVertex := 0;
  FNumVertices := 0;

  FPoint[1].X := X;
  FPoint[1].Y := Y;

  F := ClippingFlags(FPoint[1].X, FPoint[1].Y);
  FF2 := F;

  if FF2 = FF1 then
    if FF2 = [] then
    begin
      FX[0] := X;
      FY[0] := Y;

      FCmd[0] := CAggPathCmdLineTo;

      FNumVertices := 1;
    end
    else
  else
    ClipLineSegment;

  FF1 := F;
  FPoint[0].X := X;
  FPoint[0].Y := Y;
end;

function TAggVpgenClipPolyline.Vertex(X, Y: PDouble): Cardinal;
begin
  if FVertex < FNumVertices then
  begin
    X^ := FX[FVertex];
    Y^ := FY[FVertex];

    Result := FCmd[FVertex];

    Inc(FVertex);
  end
  else
    Result := CAggPathCmdStop;
end;

function TAggVpgenClipPolyline.ClippingFlagsX(X: Double): TAggClippingFlags;
var
  F: TAggClippingFlags;
begin
  F := [];

  if X < FClipBox.X1 then
    F := F + [cfX1];

  if X > FClipBox.X2 then
    F := F + [cfX2];

  Result := F;
end;

function TAggVpgenClipPolyline.ClippingFlagsY(Y: Double): TAggClippingFlags;
var
  F: TAggClippingFlags;
begin
  F := [];

  if Y < FClipBox.Y1 then
    F := F + [cfY1];

  if Y > FClipBox.Y2 then
    F := F + [cfY2];

  Result := F;
end;

function TAggVpgenClipPolyline.ClippingFlags;
begin
  Result := ClippingFlagsX(X) + ClippingFlagsY(Y);
end;

function TAggVpgenClipPolyline.MovePoint(X, Y: PDouble; var Flags: TAggClippingFlags): Boolean;
var
  Bound: Double;
begin
  if (cfX1 in Flags) or (cfX2 in Flags) then
  begin
    if cfX1 in Flags then
      Bound := FClipBox.X1
    else
      Bound := FClipBox.X2;

    Y^ := (Bound - FPoint[0].X) * (FPoint[1].Y - FPoint[0].Y) /
      (FPoint[1].X - FPoint[0].X) + FPoint[0].Y;
    X^ := Bound;

    Flags := ClippingFlagsY(Y^);
  end;

  if (Abs(FPoint[1].Y - FPoint[0].Y) < CClipEpsilon) and
    (Abs(FPoint[1].X - FPoint[0].X) < CClipEpsilon) then
  begin
    Result := False;

    Exit;
  end;

  if (cfY1 in Flags) or (cfY2 in Flags) then
  begin
    if cfY1 in Flags then
      Bound := FClipBox.Y1
    else
      Bound := FClipBox.Y2;

    X^ := (Bound - FPoint[0].Y) * (FPoint[1].X - FPoint[0].X) /
      (FPoint[1].Y - FPoint[0].Y) + FPoint[0].X;
    Y^ := Bound;
  end;

  Flags := [];
  Result := True;
end;

procedure TAggVpgenClipPolyline.ClipLineSegment;
begin
  if FF1 + FF2 = [] then
  begin
    if FF1 <> [] then
    begin
      if not MovePoint(@FPoint[0].X, @FPoint[0].Y, FF1) then
        Exit;

      if FF1 <> [] then
        Exit;

      FX[0] := FPoint[0].X;
      FY[0] := FPoint[0].Y;

      FCmd[0] := CAggPathCmdMoveTo;

      FNumVertices := 1;
    end;

    if FF2 <> [] then // Move Point 2
      if not MovePoint(@FPoint[1].X, @FPoint[1].Y, FF2) then
        Exit;

    FX[FNumVertices] := FPoint[1].X;
    FY[FNumVertices] := FPoint[1].Y;

    FCmd[FNumVertices] := CAggPathCmdLineTo;

    Inc(FNumVertices);
  end;
end;

end.
