unit AggSpanGouraudRgba;

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
  AggColor,
  AggDdaLine,
  AggSpanGouraud,
  AggSpanAllocator,
  AggMath;

const
  CAggSubpixelShift = 4;
  CAggSubpixelSize = 1 shl CAggSubpixelShift;

type
  PAggRgbaCalc = ^TAggRgbaCalc;
  TAggRgbaCalc = record
  private
    F1, FDelta: TPointDouble;

    FRed1, FGreen1, FBlue1, FAlpha1, FDeltaRed, FDeltaGreen, FDeltaBlue,
    FDeltaAlpha, FRed, FGreen, FBlue, FAlpha, FX: Integer;
  public
    function Round(V: Double): Integer;
    procedure Init(C1, C2: PAggCoordType);
    procedure Calc(Y: Double);
  end;

  TAggSpanGouraudRgba = class(TAggSpanGouraud)
  private
    FSwap: Boolean;
    FY2: Integer;
    FMagic: Integer;

    FRgba1, FRgba2, FRgba3: TAggRgbaCalc;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; C1, C2, C3: PAggColor;
      X1, Y1, X2, Y2, X3, Y3, D: Double); overload;
    constructor Create(C1, C2, C3: PAggColor; X1, Y1, X2, Y2,
      X3, Y3: Double; D: Double = 0); overload;

    procedure Prepare; overload;
    procedure Prepare(MaxSpanLength: Cardinal); overload; override;
    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; overload; override;
    procedure Generate(Span: PAggColor; X, Y: Integer; Len: Cardinal); overload;
  end;

implementation


{ TAggRgbaCalc }

function TAggRgbaCalc.Round;
begin
  if V < 0.0 then
    Result := Trunc(V - 0.5)
  else
    Result := Trunc(V + 0.5);
end;

procedure TAggRgbaCalc.Init;
var
  Dy: Double;
begin
  F1.X := C1.X - 0.5;
  F1.Y := C1.Y - 0.5;
  FDelta.X := C2.X - C1.X;

  Dy := C2.Y - C1.Y;

  if Dy < 1E-5 then
    FDelta.Y := 1E5
  else
    FDelta.Y := 1.0 / Dy;

  FRed1 := C1.Color.Rgba8.R;
  FGreen1 := C1.Color.Rgba8.G;
  FBlue1 := C1.Color.Rgba8.B;
  FAlpha1 := C1.Color.Rgba8.A;
  FDeltaRed := C2.Color.Rgba8.R - FRed1;
  FDeltaGreen := C2.Color.Rgba8.G - FGreen1;
  FDeltaBlue := C2.Color.Rgba8.B - FBlue1;
  FDeltaAlpha := C2.Color.Rgba8.A - FAlpha1;
end;

procedure TAggRgbaCalc.Calc;
var
  K: Double;
begin
  K := (Y - F1.Y) * FDelta.Y;

  if K < 0.0 then
    K := 0.0;

  if K > 1.0 then
    K := 1.0;

  FRed := FRed1 + Self.Round(FDeltaRed * K);
  FGreen := FGreen1 + Self.Round(FDeltaGreen * K);
  FBlue := FBlue1 + Self.Round(FDeltaBlue * K);
  FAlpha := FAlpha1 + Self.Round(FDeltaAlpha * K);
  FX := Self.Round((F1.X + FDelta.X * K) * CAggSubpixelSize);
end;


{ TAggSpanGouraudRgba }

constructor TAggSpanGouraudRgba.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
  FMagic := 12345;
end;

constructor TAggSpanGouraudRgba.Create(Alloc: TAggSpanAllocator;
  C1, C2, C3: PAggColor; X1, Y1, X2, Y2, X3, Y3, D: Double);
begin
  inherited Create(Alloc, C1, C2, C3, X1, Y1, X2, Y2, X3, Y3, D);
  FMagic := 12345;
end;

constructor TAggSpanGouraudRgba.Create(C1, C2, C3: PAggColor;
  X1, Y1, X2, Y2, X3, Y3: Double; D: Double = 0);
begin
  inherited Create(nil, C1, C2, C3, X1, Y1, X2, Y2, X3, Y3, D);
  FMagic := 12345;
end;

procedure TAggSpanGouraudRgba.Prepare(MaxSpanLength: Cardinal);
var
  Coord: array [0..2] of TAggCoordType;
begin
  inherited Prepare(MaxSpanLength);

  ArrangeVertices(@Coord);

  FY2 := Trunc(Coord[1].Y);

  FSwap := CalculatePointLocation(Coord[0].X, Coord[0].Y, Coord[2].X, Coord[2].Y,
    Coord[1].X, Coord[1].Y) < 0.0;

  FRgba1.Init(@Coord[0], @Coord[2]);
  FRgba2.Init(@Coord[0], @Coord[1]);
  FRgba3.Init(@Coord[1], @Coord[2]);
end;

procedure TAggSpanGouraudRgba.Prepare;
var
  Coord: array [0..2] of TAggCoordType;
begin
  ArrangeVertices(@Coord);

  FY2 := Integer(Trunc(Coord[1].Y));

  FSwap := CrossProduct(Coord[0].X, Coord[0].Y, Coord[2].X, Coord[2].Y,
    Coord[1].X, Coord[1].Y) < 0.0;

  FRgba1.Init(@Coord[0], @Coord[2]);
  FRgba2.Init(@Coord[0], @Coord[1]);
  FRgba3.Init(@Coord[1], @Coord[2]);
end;

function TAggSpanGouraudRgba.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
const
  Lim = AggColor.CAggBaseMask;
var
  Pc1, Pc2, T: PAggRgbaCalc;
  Nlen, Start, Vr, Vg, Vb, Va: Integer;
  R, G, B, A: TAggDdaLineInterpolator;
  Span: PAggColor;
begin
  Assert(FMagic = 12345);
  FRgba1.Calc(Y); // (FRgba1.FDelta.Y > 2) ? FRgba1.F1.Y : y);

  Pc1 := @FRgba1;
  Pc2 := @FRgba2;

  if Y <= FY2 then
    // Bottom part of the triangle (first subtriangle)
    FRgba2.Calc(Y + FRgba2.FDelta.Y)
  else
  begin
    // Upper part (second subtriangle)
    FRgba3.Calc(Y - FRgba3.FDelta.Y);

    Pc2 := @FRgba3;
  end;

  if FSwap then
  begin
    // It means that the triangle is oriented clockwise,
    // so that we need to swap the controlling structures
    T := Pc2;
    Pc2 := Pc1;
    Pc1 := T;
  end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  Nlen := Abs(Pc2.FX - Pc1.FX);

  if Nlen <= 0 then
    Nlen := 1;

  R.Initialize(Pc1.FRed, Pc2.FRed, Nlen, 14);
  G.Initialize(Pc1.FGreen, Pc2.FGreen, Nlen, 14);
  B.Initialize(Pc1.FBlue, Pc2.FBlue, Nlen, 14);
  A.Initialize(Pc1.FAlpha, Pc2.FAlpha, Nlen, 14);

  // Calculate the starting point of the Gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the Span
  // if necessary.
  Start := Pc1.FX - (X shl CAggSubpixelShift);

  R.DecOperator(Start);
  G.DecOperator(Start);
  B.DecOperator(Start);
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
    Vr := R.Y;
    Vg := G.Y;
    Vb := B.Y;
    Va := A.Y;

    if Vr < 0 then
      Vr := 0;

    if Vr > Lim then
      Vr := Lim;

    if Vg < 0 then
      Vg := 0;

    if Vg > Lim then
      Vg := Lim;

    if Vb < 0 then
      Vb := 0;

    if Vb > Lim then
      Vb := Lim;

    if Va < 0 then
      Va := 0;

    if Va > Lim then
      Va := Lim;

    Span.Rgba8.R := Int8u(Vr);
    Span.Rgba8.G := Int8u(Vg);
    Span.Rgba8.B := Int8u(Vb);
    Span.Rgba8.A := Int8u(Va);

    R.IncOperator(CAggSubpixelSize);
    G.IncOperator(CAggSubpixelSize);
    B.IncOperator(CAggSubpixelSize);
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
    Span.Rgba8.R := Int8u(R.Y);
    Span.Rgba8.G := Int8u(G.Y);
    Span.Rgba8.B := Int8u(B.Y);
    Span.Rgba8.A := Int8u(A.Y);

    R.IncOperator(CAggSubpixelSize);
    G.IncOperator(CAggSubpixelSize);
    B.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Dec(Nlen, CAggSubpixelSize);
    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;

  // Ending part; checking for overfLow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while Len <> 0 do
  begin
    Vr := R.Y;
    Vg := G.Y;
    Vb := B.Y;
    Va := A.Y;

    if Vr < 0 then
      Vr := 0;

    if Vr > Lim then
      Vr := Lim;

    if Vg < 0 then
      Vg := 0;

    if Vg > Lim then
      Vg := Lim;

    if Vb < 0 then
      Vb := 0;

    if Vb > Lim then
      Vb := Lim;

    if Va < 0 then
      Va := 0;

    if Va > Lim then
      Va := Lim;

    Span.Rgba8.R := Int8u(Vr);
    Span.Rgba8.G := Int8u(Vg);
    Span.Rgba8.B := Int8u(Vb);
    Span.Rgba8.A := Int8u(Va);

    R.IncOperator(CAggSubpixelSize);
    G.IncOperator(CAggSubpixelSize);
    B.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;

  Result := Allocator.Span;
end;

procedure TAggSpanGouraudRgba.Generate(Span: PAggColor; X, Y: Integer;
  Len: Cardinal);
const
  Lim = AggColor.CAggBaseMask;
var
  Pc1, Pc2, T: PAggRgbaCalc;
  Nlen, Start, Vr, Vg, Vb, Va: Integer;
  R, G, B, A: TAggDdaLineInterpolator;
begin
  FRgba1.Calc(Y); // (FRgba1.FDelta.Y > 2) ? FRgba1.F1.Y : y);

  Pc1 := @FRgba1;
  Pc2 := @FRgba2;

  if Y <= FY2 then
    // Bottom part of the triangle (first subtriangle)
    FRgba2.Calc(Y + FRgba2.FDelta.Y)
  else
  begin
    // Upper part (second subtriangle)
    FRgba3.Calc(Y - FRgba3.FDelta.Y);

    Pc2 := @FRgba3;
  end;

  if FSwap then
  begin
    // It means that the triangle is oriented clockwise,
    // so that we need to swap the controlling structures
    T := Pc2;
    Pc2 := Pc1;
    Pc1 := T;
  end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  Nlen := Abs(Pc2.FX - Pc1.FX);

  if Nlen <= 0 then
    Nlen := 1;

  R.Initialize(Pc1.FRed, Pc2.FRed, Nlen, 14);
  G.Initialize(Pc1.FGreen, Pc2.FGreen, Nlen, 14);
  B.Initialize(Pc1.FBlue, Pc2.FBlue, Nlen, 14);
  A.Initialize(Pc1.FAlpha, Pc2.FAlpha, Nlen, 14);

  // Calculate the starting point of the Gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the Span
  // if necessary.
  Start := Pc1.FX - (X shl CAggSubpixelShift);

  R.DecOperator(Start);
  G.DecOperator(Start);
  B.DecOperator(Start);
  A.DecOperator(Start);

  Inc(Nlen, Start);

  // Beginning part of the Span. Since we rolled back the
  // interpolators, the color values may have overfLow.
  // So that, we render the beginning part with checking
  // for overfLow. It lasts until "start" is positive;
  // typically it's 1-2 pixels, but may be more in some cases.
  while (Len <> 0) and (Start > 0) do
  begin
    Vr := R.Y;
    Vg := G.Y;
    Vb := B.Y;
    Va := A.Y;

    if Vr < 0 then
      Vr := 0;

    if Vr > Lim then
      Vr := Lim;

    if Vg < 0 then
      Vg := 0;

    if Vg > Lim then
      Vg := Lim;

    if Vb < 0 then
      Vb := 0;

    if Vb > Lim then
      Vb := Lim;

    if Va < 0 then
      Va := 0;

    if Va > Lim then
      Va := Lim;

    Span.Rgba8.R := Int8u(Vr);
    Span.Rgba8.G := Int8u(Vg);
    Span.Rgba8.B := Int8u(Vb);
    Span.Rgba8.A := Int8u(Va);

    R.IncOperator(CAggSubpixelSize);
    G.IncOperator(CAggSubpixelSize);
    B.IncOperator(CAggSubpixelSize);
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
    Span.Rgba8.R := Int8u(R.Y);
    Span.Rgba8.G := Int8u(G.Y);
    Span.Rgba8.B := Int8u(B.Y);
    Span.Rgba8.A := Int8u(A.Y);

    R.IncOperator(CAggSubpixelSize);
    G.IncOperator(CAggSubpixelSize);
    B.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Dec(Nlen, CAggSubpixelSize);
    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;

  // Ending part; checking for overfLow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while Len <> 0 do
  begin
    Vr := R.Y;
    Vg := G.Y;
    Vb := B.Y;
    Va := A.Y;

    if Vr < 0 then
      Vr := 0;

    if Vr > Lim then
      Vr := Lim;

    if Vg < 0 then
      Vg := 0;

    if Vg > Lim then
      Vg := Lim;

    if Vb < 0 then
      Vb := 0;

    if Vb > Lim then
      Vb := Lim;

    if Va < 0 then
      Va := 0;

    if Va > Lim then
      Va := Lim;

    Span.Rgba8.R := Int8u(Vr);
    Span.Rgba8.G := Int8u(Vg);
    Span.Rgba8.B := Int8u(Vb);
    Span.Rgba8.A := Int8u(Va);

    R.IncOperator(CAggSubpixelSize);
    G.IncOperator(CAggSubpixelSize);
    B.IncOperator(CAggSubpixelSize);
    A.IncOperator(CAggSubpixelSize);

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  end;
end;

end.
