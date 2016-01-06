unit AggSpanGouraudGray;

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
  AggDdaLine,
  AggSpanGouraud,
  AggSpanAllocator,
  AggMath;

const
  CAggSubpixelShift = 4;
  CAggSubpixelSize = 1 shl CAggSubpixelShift;

type
  PAggGrayCalc = ^TAggGrayCalc;
  TAggGrayCalc = record
  private
    F1, FDelta: TPointDouble;
    FColorC1V, FColorC1A, FColorDeltaV, FColorDeltaA,
    FColorV, FColorA, FX: Integer;
  public
    procedure Init(C1, C2: PAggCoordType);
    procedure Calculate(Y: Double);
  end;

  TAggSpanGouraudGray = class(TAggSpanGouraud)
  private
    FSwap: Boolean;
    FY2: Integer;
    FC1, FC2, FC3: TAggGrayCalc;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; C1, C2, C3: PAggColor;
      X1, Y1, X2, Y2, X3, Y3, D: Double); overload;

    procedure Prepare(MaxSpanLength: Cardinal); override;
    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggGrayCalc }

procedure TAggGrayCalc.Init;
var
  Dy: Double;
begin
  F1 := PointDouble(C1.X - 0.5, C1.Y - 0.5);
  FDelta.X := C2.X - C1.X;

  Dy := C2.Y - C1.Y;

  if Abs(Dy) < 1E-10 then
    FDelta.Y := 1E10
  else
    FDelta.Y := 1.0 / Dy;

  FColorC1V := C1.Color.V;
  FColorC1A := C1.Color.Rgba8.A;
  FColorDeltaV := C2.Color.V - FColorC1V;
  FColorDeltaA := C2.Color.Rgba8.A - FColorC1A;
end;

procedure TAggGrayCalc.Calculate;
var
  K: Double;
begin
  K := EnsureRange((Y - F1.Y) * FDelta.Y, 0, 1);

  FColorV := FColorC1V + IntegerRound(FColorDeltaV * K);
  FColorA := FColorC1A + IntegerRound(FColorDeltaA * K);
  FX := IntegerRound((F1.X + FDelta.X * K) * CAggSubpixelSize);
end;


{ TAggSpanGouraudGray }

constructor TAggSpanGouraudGray.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanGouraudGray.Create(Alloc: TAggSpanAllocator;
  C1, C2, C3: PAggColor; X1, Y1, X2, Y2, X3, Y3, D: Double);
begin
  inherited Create(Alloc, C1, C2, C3, X1, Y1, X2, Y2, X3, Y3, D);
end;

procedure TAggSpanGouraudGray.Prepare;
var
  Coord: array [0..2] of TAggCoordType;
begin
  inherited Prepare(MaxSpanLength);

  ArrangeVertices(@Coord);

  FY2 := Trunc(Coord[1].Y);

  FSwap := CalculatePointLocation(Coord[0].X, Coord[0].Y, Coord[2].X, Coord[2].Y,
    Coord[1].X, Coord[1].Y) < 0.0;

  FC1.Init(@Coord[0], @Coord[2]);
  FC2.Init(@Coord[0], @Coord[1]);
  FC3.Init(@Coord[1], @Coord[2]);
end;

function TAggSpanGouraudGray.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
const
  CLim = AggColor.CAggBaseMask;
var
  Pc1, Pc2, T: PAggGrayCalc;

  Nlen, Start: Integer;

  V, A: TAggDdaLineInterpolator;
  Span: PAggColor;
begin
  FC1.Calculate(Y);

  Pc1 := @FC1;
  Pc2 := @FC2;

  if Y < FY2 then
    // Bottom part of the triangle (first subtriangle)
    FC2.Calculate(Y + FC2.FDelta.Y)
  else
  begin
    // Upper part (second subtriangle)
    FC3.Calculate(Y - FC3.FDelta.Y);

    Pc2 := @FC3;
  end;

  // It means that the triangle is oriented clockwise,
  // so that we need to swap the controlling structures
  if FSwap then
  begin
    T := Pc2;
    Pc2 := Pc1;
    Pc1 := T;
  end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  Nlen := Abs(Pc2.FX - Pc1.FX);

  if Nlen <= 0 then
    Nlen := 1;

  V.Initialize(Pc1.FColorV, Pc2.FColorV, Nlen, 14);
  A.Initialize(Pc1.FColorA, Pc2.FColorA, Nlen, 14);

  // Calculate the starting point of the Gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the Span
  // if necessary.
  Start := Pc1.FX - (X shl CAggSubpixelShift);

  V.DecOperator(Start);
  A.DecOperator(Start);

  Inc(Nlen, Start);

  Span := Allocator.Span;

  // Beginning part of the Span. Since we rolled back the
  // interpolators, the color values may have overfLow.
  // So that, we render the beginning part with checking
  // for overfLow. It lasts until "start" is positive;
  // typically it's 1-2 pixels, but may be more in some cases.
  while (Len <> 0) and (Start > 0) do
  begin
    Span.V := Int8u(EnsureRange(V.Y, 0, CLim));
    Span.Rgba8.A := Int8u(EnsureRange(A.Y, 0, CLim));

    V.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Dec(Nlen, CAggSubpixelSize);
    Dec(Start, CAggSubpixelSize);
    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;

  // Middle part, no checking for overfLow.
  // Actual Spans can be longer than the calculated length
  // because of anti-aliasing, thus, the interpolators can
  // overfLow. But while "nlen" is positive we are safe.
  while (Len <> 0) and (Nlen > 0) do
  begin
    Span.V := Int8u(V.Y);
    Span.Rgba8.A := Int8u(A.Y);

    V.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Dec(Nlen, CAggSubpixelSize);
    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;

  // Ending part; checking for overfLow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while Len <> 0 do
  begin
    Span.V := Int8u(EnsureRange(V.Y, 0, CLim));
    Span.Rgba8.A := Int8u(EnsureRange(A.Y, 0, CLim));

    V.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;

  Result := Allocator.Span;
end;

end.
