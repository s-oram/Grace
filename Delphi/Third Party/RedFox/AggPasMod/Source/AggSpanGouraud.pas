unit AggSpanGouraud;

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
  AggMath,
  AggSpanAllocator,
  AggSpanGenerator,
  AggColor,
  AggVertexSource;

type
  PAggCoordType = ^TAggCoordType;
  TAggCoordType = record
    X, Y: Double;
    Color: TAggColor;
  end;

  TAggSpanGouraud = class(TAggSpanGenerator)
  private
    FCoord: array [0..2] of TAggCoordType;
    FX, FY: array [0..7] of Double;
    FCmd: array [0..7] of Cardinal;
    FVertex: Cardinal;
  protected
    procedure ArrangeVertices(Coord: PAggCoordType);
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; C1, C2, C3: PAggColor;
      X1, Y1, X2, Y2, X3, Y3, D: Double); overload;

    procedure SetColors(C1, C2, C3: PAggColor); overload;
    procedure SetColors(C1, C2, C3: TAggRgba8); overload;
    procedure Triangle(X1, Y1, X2, Y2, X3, Y3, D: Double);

    // Vertex Source Interface to feed the coordinates to the Rasterizer
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation


{ TAggSpanGouraud }

constructor TAggSpanGouraud.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FCmd[0] := CAggPathCmdStop;
end;

constructor TAggSpanGouraud.Create(Alloc: TAggSpanAllocator;
  C1, C2, C3: PAggColor; X1, Y1, X2, Y2, X3, Y3, D: Double);
begin
  inherited Create(Alloc);

  SetColors(C1, C2, C3);
  Triangle(X1, Y1, X2, Y2, X3, Y3, D);
end;

// Sets the triangle and dilates it if needed.
// The trick here is to calculate beveled joins in the vertices of the
// triangle and render it as a 6-vertex polygon.
// It's necessary to achieve numerical stability.
// However, the coordinates to interpolate colors are calculated
// as miter joins (CalculateIntersection).
procedure TAggSpanGouraud.Triangle(X1, Y1, X2, Y2, X3, Y3, D: Double);
begin
  FCoord[0].X := X1;
  FX[0] := X1;
  FCoord[0].Y := Y1;
  FY[0] := Y1;
  FCoord[1].X := X2;
  FX[1] := X2;
  FCoord[1].Y := Y2;
  FY[1] := Y2;
  FCoord[2].X := X3;
  FX[2] := X3;
  FCoord[2].Y := Y3;
  FY[2] := Y3;

  FCmd[0] := CAggPathCmdMoveTo;
  FCmd[1] := CAggPathCmdLineTo;
  FCmd[2] := CAggPathCmdLineTo;
  FCmd[3] := CAggPathCmdStop;

  if D <> 0.0 then
  begin
    DilateTriangle(FCoord[0].X, FCoord[0].Y, FCoord[1].X, FCoord[1].Y,
      FCoord[2].X, FCoord[2].Y, @FX, @FY, D);

    CalculateIntersection(FX[4], FY[4], FX[5], FY[5], FX[0], FY[0], FX[1],
      FY[1], @FCoord[0].X, @FCoord[0].Y);

    CalculateIntersection(FX[0], FY[0], FX[1], FY[1], FX[2], FY[2], FX[3],
      FY[3], @FCoord[1].X, @FCoord[1].Y);

    CalculateIntersection(FX[2], FY[2], FX[3], FY[3], FX[4], FY[4], FX[5],
      FY[5], @FCoord[2].X, @FCoord[2].Y);

    FCmd[3] := CAggPathCmdLineTo;
    FCmd[4] := CAggPathCmdLineTo;
    FCmd[5] := CAggPathCmdLineTo;
    FCmd[6] := CAggPathCmdStop;
  end;
end;

procedure TAggSpanGouraud.Rewind(PathID: Cardinal);
begin
  FVertex := 0;
end;

procedure TAggSpanGouraud.SetColors(C1, C2, C3: PAggColor);
begin
  FCoord[0].Color := C1^;
  FCoord[1].Color := C2^;
  FCoord[2].Color := C3^;
end;

procedure TAggSpanGouraud.SetColors(C1, C2, C3: TAggRgba8);
var
  Color: TAggColor;
begin
  Color.Rgba8 := C1;
  FCoord[0].Color := Color;
  Color.Rgba8 := C2;
  FCoord[1].Color := Color;
  Color.Rgba8 := C3;
  FCoord[2].Color := Color;
end;

function TAggSpanGouraud.Vertex(X, Y: PDouble): Cardinal;
begin
  X^ := FX[FVertex];
  Y^ := FY[FVertex];

  Result := FCmd[FVertex];

  Inc(FVertex);
end;

procedure TAggSpanGouraud.ArrangeVertices(Coord: PAggCoordType);
var
  Tmp: TAggCoordType;
begin
  PAggCoordType(PtrComp(Coord))^ := FCoord[0];
  PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^ := FCoord[1];
  PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^ := FCoord[2];

  if FCoord[0].Y > FCoord[2].Y then
  begin
    PAggCoordType(Coord)^ := FCoord[2];
    PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^ := FCoord[0];
  end;

  if PAggCoordType(Coord).Y >
    PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType)).Y then
  begin
    Tmp := PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^;

    PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^ :=
      PAggCoordType(Coord)^;

    PAggCoordType(Coord)^ := Tmp;
  end;

  if PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType)).Y >
    PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType)).Y then
  begin
    Tmp := PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^;

    PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^ :=
      PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^;

    PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^ := Tmp;
  end;
end;

end.
