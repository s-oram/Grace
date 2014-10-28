unit AggVpGenClipPolygon;

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
  AggVertexSource,
  AggVpGen,
  AggClipLiangBarsky;

type
  TAggVpgenClipPolygon = class(TAggCustomVpgen)
  private
    FClipBox: TRectDouble;
    FX1, FY1: Double;
    FClipFlags: Cardinal;
    FX, FY: array [0..3] of Double;
    FNumVertices, FVertex, FCmd: Cardinal;
    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;

    function GetAutoClose: Boolean;
    function GetAutoUnclose: Boolean;
  protected
    function ClippingFlags(X, Y: Double): Cardinal;
  public
    constructor Create; override;

    function Vertex(X, Y: PDouble): Cardinal; override;

    procedure SetClipBox(X1, Y1, X2, Y2: Double); overload;
    procedure SetClipBox(Bounds: TRectDouble); overload;

    procedure Reset; override;
    procedure MoveTo(X, Y: Double); override;
    procedure LineTo(X, Y: Double); override;

    property X1: Double read GetX1;
    property Y1: Double read GetY1;
    property X2: Double read GetX2;
    property Y2: Double read GetY2;

    property AutoClose: Boolean read GetAutoClose;
    property AutoUnclose: Boolean read GetAutoUnclose;
  end;

implementation


{ TAggVpgenClipPolygon }

constructor TAggVpgenClipPolygon.Create;
begin
  FClipBox := RectDouble(0, 0, 1, 1);

  FX1 := 0;
  FY1 := 0;

  FClipFlags := 0;
  FNumVertices := 0;

  FVertex := 0;
  FCmd := CAggPathCmdMoveTo;
end;

function TAggVpgenClipPolygon.GetX1;
begin
  Result := FClipBox.X1;
end;

function TAggVpgenClipPolygon.GetY1;
begin
  Result := FClipBox.Y1;
end;

function TAggVpgenClipPolygon.GetX2;
begin
  Result := FClipBox.X2;
end;

function TAggVpgenClipPolygon.GetY2;
begin
  Result := FClipBox.Y2;
end;

function TAggVpgenClipPolygon.GetAutoClose;
begin
  Result := True;
end;

function TAggVpgenClipPolygon.GetAutoUnclose;
begin
  Result := False;
end;

procedure TAggVpgenClipPolygon.Reset;
begin
  FVertex := 0;
  FNumVertices := 0;
end;

procedure TAggVpgenClipPolygon.SetClipBox(X1, Y1, X2, Y2: Double);
begin
  FClipBox.X1 := X1;
  FClipBox.Y1 := Y1;
  FClipBox.X2 := X2;
  FClipBox.Y2 := Y2;

  FClipBox.Normalize;
end;

procedure TAggVpgenClipPolygon.SetClipBox(Bounds: TRectDouble);
begin
  FClipBox := Bounds;
  FClipBox.Normalize;
end;

procedure TAggVpgenClipPolygon.MoveTo(X, Y: Double);
begin
  FVertex := 0;
  FNumVertices := 0;
  FClipFlags := ClippingFlags(X, Y);

  if FClipFlags = 0 then
  begin
    FX[0] := X;
    FY[0] := Y;

    FNumVertices := 1;
  end;

  FX1 := X;
  FY1 := Y;
  FCmd := CAggPathCmdMoveTo;
end;

procedure TAggVpgenClipPolygon.LineTo(X, Y: Double);
var
  Flags: Cardinal;
begin
  FVertex := 0;
  FNumVertices := 0;

  Flags := ClippingFlags(X, Y);

  if FClipFlags = Flags then
    if Flags = 0 then
    begin
      FX[0] := X;
      FY[0] := Y;

      FNumVertices := 1;
    end
    else
  else
    FNumVertices := ClipLiangBarskyDouble(FX1, FY1, X, Y, @FClipBox,
      @FX, @FY);

  FClipFlags := Flags;

  FX1 := X;
  FY1 := Y;
end;

function TAggVpgenClipPolygon.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;

begin
  if FVertex < FNumVertices then
  begin
    X^ := FX[FVertex];
    Y^ := FY[FVertex];

    Inc(FVertex);

    Cmd := FCmd;
    FCmd := CAggPathCmdLineTo;

    Result := Cmd;

  end
  else
    Result := CAggPathCmdStop;
end;

// Determine the clipping code of the vertex according to the
// Cyrus-Beck line clipping algorithm
//
//        |        |
//  0110  |  0010  | 0011
//        |        |
// -------+--------+-------- ClipBox.y2
//        |        |
//  0100  |  0000  | 0001
//        |        |
// -------+--------+-------- ClipBox.y1
//        |        |
//  1100  |  1000  | 1001
//        |        |
//  ClipBox.x1  ClipBox.x2
function TAggVpgenClipPolygon.ClippingFlags;
begin
  if X < FClipBox.X1 then
  begin
    if Y > FClipBox.Y2 then
    begin
      Result := 6;

      Exit;
    end;

    if Y < FClipBox.Y1 then
    begin
      Result := 12;

      Exit;
    end;

    Result := 4;

    Exit;
  end;

  if X > FClipBox.X2 then
  begin
    if Y > FClipBox.Y2 then
    begin
      Result := 3;

      Exit;
    end;

    if Y < FClipBox.Y1 then
    begin
      Result := 9;

      Exit;
    end;

    Result := 1;

    Exit;
  end;

  if Y > FClipBox.Y2 then
  begin
    Result := 2;

    Exit;
  end;

  if Y < FClipBox.Y1 then
  begin
    Result := 8;

    Exit;
  end;

  Result := 0;
end;

end.
