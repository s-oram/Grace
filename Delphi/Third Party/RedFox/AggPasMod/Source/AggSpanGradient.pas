unit AggSpanGradient;

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
{$Q-}
{$R-}

uses
  Math,
  AggBasics,
  AggSpanAllocator,
  AggSpanGenerator,
  AggMath,
  AggArray,
  AggSpanInterpolatorLinear,
  AggColor;

const
  CAggGradientSubpixelShift = 4;
  CAggGradientSubpixelSize = 1 shl CAggGradientSubpixelShift;
  CAggGradientSubpixelMask = CAggGradientSubpixelSize - 1;

type
  TAggCustomGradient = class;

  TAggSpanGradient = class(TAggSpanGenerator)
  private
    FDownscaleShift: Integer;

    FInterpolator: TAggSpanInterpolator;
    FGradientFunction: TAggCustomGradient;
    FColorFunction: TAggCustomArray;

    FD1, FD2: Integer;
    function GetD1: Double;
    function GetD2: Double;
    procedure SetD1(Value: Double);
    procedure SetD2(Value: Double);
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator;
      Inter: TAggSpanInterpolator; GradientFunction: TAggCustomGradient;
      ColorFunction: TAggCustomArray; AD1, AD2: Double); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;

    property D1: Double read GetD1 write SetD1;
    property D2: Double read GetD2 write SetD2;

    property Interpolator: TAggSpanInterpolator read FInterpolator write
      FInterpolator;
    property GradientFunction: TAggCustomGradient read FGradientFunction write
      FGradientFunction;
    property ColorFunction: TAggCustomArray read FColorFunction write
      FColorFunction;
  end;

  TAggGradientLinearColor = class(TAggPodAutoArray)
  private
    FC1, FC2, FRes: TAggColor;
  protected
    function GetSize: Cardinal; override;
    function ArrayOperator(I: Cardinal): Pointer; override;
  public
    constructor Create(C1, C2: PAggColor; ASize: Cardinal = 256);

    procedure SetColors(C1, C2: PAggColor; ASize: Cardinal = 256);
  end;

  TAggCustomGradient = class
  public
    constructor Create; virtual;

    function Calculate(X, Y, D: Integer): Integer; virtual; abstract;
  end;
  TAggCustomGradientClass = class of TAggCustomGradient;


  TAggGradientRadial = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  // Actually the same as radial. Just for compatibility
  TAggGradientCircle = TAggGradientRadial;

  TAggGradientRadialDouble = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientRadialFocus = class(TAggCustomGradient)
  private
    FRadius: Integer;
    FFocus: TPointInteger;

    FRadius2, FTrivial: Double;
  public
    constructor Create; overload; override;
    constructor Create(R, Fx, Fy: Double); overload;

    procedure Init(R, Fx, Fy: Double);

    function Radius: Double;
    function GetFocusX: Double;
    function GetFocusY: Double;

    function Calculate(X, Y, D: Integer): Integer; override;

    procedure UpdateValues;
  end;

  TAggGradientRadialFocusExtended = class(TAggCustomGradient)
  private
    FRadius: Integer;
    FFocus: TPointInteger;
    FRadius2, FMul: Double;
    FFocusSquared: TPointDouble;
  public
    constructor Create; overload; override;
    constructor Create(R, Fx, Fy: Double); overload;

    procedure Init(R, Fx, Fy: Double);

    function Radius: Double;
    function GetFocusX: Double;
    function GetFocusY: Double;

    function Calculate(X, Y, D: Integer): Integer; override;

    procedure UpdateValues;
  end;

  TAggGradientX = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientY = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientDiamond = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientXY = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientSqrtXY = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientConic = class(TAggCustomGradient)
  public
    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientRepeatAdaptor = class(TAggCustomGradient)
  private
    FGradient: TAggCustomGradient;
  public
    constructor Create(Gradient: TAggCustomGradient);

    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggGradientReflectAdaptor = class(TAggCustomGradient)
  private
    FGradient: TAggCustomGradient;
  public
    constructor Create(Gradient: TAggCustomGradient);

    function Calculate(X, Y, D: Integer): Integer; override;
  end;

implementation


{ TAggSpanGradient }

constructor TAggSpanGradient.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FInterpolator := nil;
  FGradientFunction := nil;
  FColorFunction := nil;
end;

constructor TAggSpanGradient.Create(Alloc: TAggSpanAllocator;
  Inter: TAggSpanInterpolator; GradientFunction: TAggCustomGradient;
  ColorFunction: TAggCustomArray; AD1, AD2: Double);
begin
  inherited Create(Alloc);

  FInterpolator := Inter;
  FGradientFunction := GradientFunction;
  FColorFunction := ColorFunction;

  FDownscaleShift := FInterpolator.SubpixelShift - CAggGradientSubpixelShift;

  FD1 := Trunc(AD1 * CAggGradientSubpixelSize);
  FD2 := Trunc(AD2 * CAggGradientSubpixelSize);
end;

function TAggSpanGradient.GetD1: Double;
begin
  Result := FD1 / CAggGradientSubpixelSize;
end;

function TAggSpanGradient.GetD2: Double;
begin
  Result := FD2 / CAggGradientSubpixelSize;
end;

procedure TAggSpanGradient.SetD1(Value: Double);
begin
  FD1 := Trunc(Value * CAggGradientSubpixelSize);
end;

procedure TAggSpanGradient.SetD2(Value: Double);
begin
  FD2 := Trunc(Value * CAggGradientSubpixelSize);
end;

function TAggSpanGradient.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Dd, D: Integer;
begin
  Span := Allocator.Span;

  Dd := FD2 - FD1;

  if Dd < 1 then
    Dd := 1;

  FInterpolator.SetBegin(X + 0.5, Y + 0.5, Len);

  repeat
    FInterpolator.Coordinates(@X, @Y);

    D := FGradientFunction.Calculate(ShrInt32(X, FDownscaleShift),
      ShrInt32(Y, FDownscaleShift), FD2);

    D := ((D - FD1) * FColorFunction.Size) div Dd;

    if D < 0 then
      D := 0;

    if D >= FColorFunction.Size then
      D := FColorFunction.Size - 1;

    Span^ := PAggColor(FColorFunction[D])^;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    FInterpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggGradientLinearColor }

constructor TAggGradientLinearColor.Create(C1, C2: PAggColor;
  ASize: Cardinal = 256);
begin
  FC1 := C1^;
  FC2 := C2^;

  FSize := ASize;
end;

function TAggGradientLinearColor.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggGradientLinearColor.ArrayOperator(I: Cardinal): Pointer;
begin
  FRes := Gradient(FC1, FC2, I / (FSize - 1));
  Result := @FRes;
end;

procedure TAggGradientLinearColor.SetColors(C1, C2: PAggColor;
  ASize: Cardinal = 256);
begin
  FC1 := C1^;
  FC2 := C2^;

  FSize := Size;
end;


{ TAggCustomGradient }

constructor TAggCustomGradient.Create;
begin
end;


{ TAggGradientRadial }

function TAggGradientRadial.Calculate(X, Y, D: Integer): Integer;
begin
  Result := FastSqrt(X * X + Y * Y);
end;


{ TAggGradientRadialDouble }

function TAggGradientRadialDouble.Calculate(X, Y, D: Integer): Integer;
begin
  Result := Trunc(Hypot(X, Y));
end;


{ TAggGradientRadialFocus }

constructor TAggGradientRadialFocus.Create;
begin
  FRadius := 100 * CAggGradientSubpixelSize;
  FFocus := PointInteger(0);

  UpdateValues;
end;

constructor TAggGradientRadialFocus.Create(R, Fx, Fy: Double);
begin
  FRadius := Trunc(R * CAggGradientSubpixelSize);
  FFocus.X := Trunc(Fx * CAggGradientSubpixelSize);
  FFocus.Y := Trunc(Fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

procedure TAggGradientRadialFocus.Init;
begin
  FRadius := Trunc(R * CAggGradientSubpixelSize);
  FFocus.X := Trunc(Fx * CAggGradientSubpixelSize);
  FFocus.Y := Trunc(Fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

function TAggGradientRadialFocus.Radius;
begin
  Result := FRadius / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocus.GetFocusX;
begin
  Result := FFocus.X / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocus.GetFocusY;
begin
  Result := FFocus.Y / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocus.Calculate(X, Y, D: Integer): Integer;
var
  Solution: TPointDouble;
  Slope, Yint, A, B, C, Det, IntToFocus, CurToFocus: Double;
begin
  // Special case to avoid divide by zero or very near zero
  if X = FFocus.X then
  begin
    Solution := PointDouble(FFocus.X, 0.0);

    if Y > FFocus.Y then
      Solution.Y := Solution.Y + FTrivial
    else
      Solution.Y := Solution.Y - FTrivial;
  end
  else
  begin
    // Slope of the focus-current line
    Slope := (Y - FFocus.Y) / (X - FFocus.X);

    // y-intercept of that same line
    Yint := Y - (Slope * X);

    // Use the classical quadratic formula to calculate
    // the intersection point
    A := Sqr(Slope) + 1;
    B := 2 * Slope * Yint;
    C := Sqr(Yint) - FRadius2;

    Det := Sqrt(Sqr(B) - (4 * A * C));

    Solution.X := -B;

    // Choose the positive or negative root depending
    // on where the X coord lies with respect to the focus.
    if X < FFocus.X then
      Solution.X := Solution.X - Det
    else
      Solution.X := Solution.X + Det;

    Solution.X := Solution.X / (2 * A);

    // Calculating of Y is trivial
    Solution.Y := (Slope * Solution.X) + Yint;
  end;

  // Calculate the percentage (0..1) of the current point along the
  // focus-circumference line and return the normalized (0..d) value
  Solution.X := Solution.X - FFocus.X;
  Solution.Y := Solution.Y - FFocus.Y;

  IntToFocus := Sqr(Solution.X) + Sqr(Solution.Y);
  CurToFocus := Sqr(X - FFocus.X) + Sqr(Y - FFocus.Y);

  Result := Trunc(Sqrt(CurToFocus / IntToFocus) * FRadius);
end;

procedure TAggGradientRadialFocus.UpdateValues;
var
  Dist, R: Double;
  Sn, Cn: Double;
begin
  // For use in the quadratic equation
  FRadius2 := Sqr(FRadius);

  Dist := Hypot(FFocus.X, FFocus.Y);

  // Test if distance from focus to center is greater than the radius
  // For the sake of assurance factor restrict the point to be
  // no further than 99% of the radius.
  R := FRadius * 0.99;

  if Dist > R then
  begin
    // clamp focus to radius
    // x = r cos theta, y = r sin theta
    SinCos(ArcTan2(FFocus.Y, FFocus.X), Sn, Cn);
    FFocus.X := Trunc(R * Cn);
    FFocus.Y := Trunc(R * Sn);
  end;

  // Calculate the solution to be used in the case where x == GetFocusX
  FTrivial := Sqrt(FRadius2 - Sqr(FFocus.X));
end;


{ TAggGradientRadialFocusExtended }

constructor TAggGradientRadialFocusExtended.Create;
begin
  FRadius := 100 * CAggGradientSubpixelSize;
  FFocus := PointInteger(0);

  UpdateValues;
end;

constructor TAggGradientRadialFocusExtended.Create(R, Fx, Fy: Double);
begin
  FRadius := IntegerRound(R * CAggGradientSubpixelSize);
  FFocus.X := IntegerRound(Fx * CAggGradientSubpixelSize);
  FFocus.Y := IntegerRound(Fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

procedure TAggGradientRadialFocusExtended.Init(R, Fx, Fy: Double);
begin
  FRadius := IntegerRound(R * CAggGradientSubpixelSize);
  FFocus.X := IntegerRound(Fx * CAggGradientSubpixelSize);
  FFocus.Y := IntegerRound(Fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

function TAggGradientRadialFocusExtended.Radius: Double;
begin
  Result := FRadius / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocusExtended.GetFocusX: Double;
begin
  Result := FFocus.X / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocusExtended.GetFocusY: Double;
begin
  Result := FFocus.Y / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocusExtended.Calculate(X, Y, D: Integer): Integer;
var
  Dx, Dy, D2, D3: Double;

begin
  Dx := X - FFocus.X;
  Dy := Y - FFocus.Y;
  D2 := Dx * FFocus.Y - Dy * FFocus.X;
  D3 := FRadius2 * (Sqr(Dx) + Sqr(Dy)) - Sqr(D2);

  Result := IntegerRound((Dx * FFocus.X + Dy * FFocus.Y + Sqrt(Abs(D3))) * FMul);
end;

// Calculate the invariant values. In case the focal center
// lies exactly on the Gradient circle the divisor degenerates
// into zero. In this case we just move the focal center by
// one subpixel unit possibly in the direction to the origin (0,0)
// and calculate the values again.
procedure TAggGradientRadialFocusExtended.UpdateValues;
var
  D: Double;
begin
  FRadius2 := Sqr(FRadius);
  FFocusSquared := PointDouble(Sqr(FFocus.X), Sqr(FFocus.Y));

  D := (FRadius2 - (FFocusSquared.X + FFocusSquared.Y));

  if D = 0 then
  begin
    if FFocus.X <> 0 then
      if FFocus.X < 0 then
        Inc(FFocus.X)
      else
        Dec(FFocus.X);

    if FFocus.Y <> 0 then
      if FFocus.Y < 0 then
        Inc(FFocus.Y)
      else
        Dec(FFocus.Y);

    FFocusSquared := PointDouble(Sqr(FFocus.X), Sqr(FFocus.Y));

    D := (FRadius2 - (FFocusSquared.X + FFocusSquared.Y));
  end;

  FMul := FRadius / D;
end;


{ TAggGradientX }

function TAggGradientX.Calculate(X, Y, D: Integer): Integer;
begin
  Result := X;
end;


{ TAggGradientY }

function TAggGradientY.Calculate(X, Y, D: Integer): Integer;
begin
  Result := Y;
end;


{ TAggGradientDiamond }

function TAggGradientDiamond.Calculate(X, Y, D: Integer): Integer;
var
  Ax, Ay: Integer;
begin
  Ax := Abs(X);
  Ay := Abs(Y);

  if Ax > Ay then
    Result := Ax
  else
    Result := Ay;
end;


{ TAggGradientXY }

function TAggGradientXY.Calculate(X, Y, D: Integer): Integer;
begin
  if D = 0 then
    Result := 0
  else
    Result := Abs(X) * Abs(Y) div D;
end;


{ TAggGradientSqrtXY }

function TAggGradientSqrtXY.Calculate(X, Y, D: Integer): Integer;
begin
  Result := FastSqrt(Abs(X) * Abs(Y));
end;


{ TAggGradientConic }

function TAggGradientConic.Calculate(X, Y, D: Integer): Integer;
begin
  Result := Trunc(Abs(ArcTan2(Y, X)) * D / Pi);
end;


{ TAggGradientRepeatAdaptor }

constructor TAggGradientRepeatAdaptor.Create(Gradient: TAggCustomGradient);
begin
  inherited Create;
  FGradient := Gradient;
end;

function TAggGradientRepeatAdaptor.Calculate(X, Y, D: Integer): Integer;
begin
  if D = 0 then
    Result := 0
  else
    Result := FGradient.Calculate(X, Y, D) mod D;

  if Result < 0 then
    Inc(Result, D);
end;


{ TAggGradientReflectAdaptor }

constructor TAggGradientReflectAdaptor.Create(Gradient: TAggCustomGradient);
begin
  inherited Create;
  FGradient := Gradient;
end;

function TAggGradientReflectAdaptor.Calculate(X, Y, D: Integer): Integer;
var
  D2: Integer;
begin
  D2 := D shl 1;

  if D2 = 0 then
    Result := 0
  else
    Result := FGradient.Calculate(X, Y, D) mod D2;

  if Result < 0 then
    Inc(Result, D2);

  if Result >= D then
    Result := D2 - Result;
end;

end.
