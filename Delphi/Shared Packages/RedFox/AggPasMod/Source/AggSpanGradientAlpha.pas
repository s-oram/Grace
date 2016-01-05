unit AggSpanGradientAlpha;

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
{$Q-}
{$R-}

uses
  AggBasics,
  AggColor,
  AggArray,
  AggSpanGradient,
  AggSpanInterpolatorLinear,
  AggSpanConverter;

type
  TAggGradientAlpha = class(TAggPodAutoArray);

  TAggSpanGradientAlpha = class(TAggSpanConvertor)
  private
    FDownscaleShift: Cardinal;

    FInterpolator: TAggSpanInterpolator;
    FGradientFunction: TAggCustomGradient;
    FAlphaFunction: TAggGradientAlpha;

    FD1, FD2: Integer;
    function GetD1: Double;
    function GetD2: Double;
    procedure SetD1(Value: Double);
    procedure SetD2(Value: Double);
  public
    constructor Create; overload;
    constructor Create(Inter: TAggSpanInterpolator;
      Gradient: TAggCustomGradient; Alpha_fnc: TAggGradientAlpha;
      D1, D2: Double); overload;

    procedure Convert(Span: PAggColor; X, Y: Integer; Len: Cardinal); override;

    property D1: Double read GetD1 write SetD1;
    property D2: Double read GetD2 write SetD2;

    property Interpolator: TAggSpanInterpolator read FInterpolator write
      FInterpolator;
    property GradientFunction: TAggCustomGradient read FGradientFunction
      write FGradientFunction;
    property AlphaFunction: TAggGradientAlpha read FAlphaFunction write
      FAlphaFunction;
  end;

  TAggGradientAlphaX = class
  public
    function ArrayOperator(X: TAggColor): TAggColor;
  end;

  TAggGradientAlphaXU8 = class
  public
    function ArrayOperator(X: Integer): Int8u;
  end;

  TAggGradientAlphaOneMinusXU8 = class
  public
    function ArrayOperator(X: Integer): Int8u;
  end;

implementation


{ TAggSpanGradientAlpha }

constructor TAggSpanGradientAlpha.Create;
begin
  FInterpolator := nil;
  FGradientFunction := nil;
  FAlphaFunction := nil;

  FDownscaleShift := 0;

  FD1 := 0;
  FD2 := 0;
end;

constructor TAggSpanGradientAlpha.Create(Inter: TAggSpanInterpolator;
  Gradient: TAggCustomGradient; Alpha_fnc: TAggGradientAlpha; D1, D2: Double);
begin
  FInterpolator := Inter;
  FGradientFunction := Gradient;
  FAlphaFunction := Alpha_fnc;

  FDownscaleShift := FInterpolator.SubpixelShift - CAggGradientSubpixelShift;

  FD1 := Trunc(D1 * CAggGradientSubpixelSize);
  FD2 := Trunc(D2 * CAggGradientSubpixelSize);
end;

function TAggSpanGradientAlpha.GetD1;
begin
  Result := FD1 / CAggGradientSubpixelSize;
end;

function TAggSpanGradientAlpha.GetD2;
begin
  Result := FD2 / CAggGradientSubpixelSize;
end;

procedure TAggSpanGradientAlpha.SetD1(Value: Double);
begin
  FD1 := Trunc(Value * CAggGradientSubpixelSize);
end;

procedure TAggSpanGradientAlpha.SetD2(Value: Double);
begin
  FD2 := Trunc(Value * CAggGradientSubpixelSize);
end;

procedure TAggSpanGradientAlpha.Convert(Span: PAggColor; X, Y: Integer;
  Len: Cardinal);
var
  Dd, D: Integer;
begin
  Dd := FD2 - FD1;

  if Dd < 1 then
    Dd := 1;

  FInterpolator.SetBegin(X + 0.5, Y + 0.5, Len);

  repeat
    FInterpolator.Coordinates(@X, @Y);

    D := FGradientFunction.Calculate(ShrInt32(X, FDownscaleShift),
      ShrInt32(Y, FDownscaleShift), FD2);

    D := ((D - FD1) * FAlphaFunction.Size) div Dd;

    if D < 0 then
      D := 0;

    if D >= FAlphaFunction.Size then
      D := FAlphaFunction.Size - 1;

    Span.Rgba8.A := PInt8u(FAlphaFunction.ArrayOperator(D))^;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    FInterpolator.IncOperator;

    Dec(Len);
  until Len = 0;
end;


{ TAggGradientAlphaX }

function TAggGradientAlphaX.ArrayOperator(X: TAggColor): TAggColor;
begin
  Result := X;
end;


{ TAggGradientAlphaXU8 }

function TAggGradientAlphaXU8.ArrayOperator(X: Integer): Int8u;
begin
  Result := Int8u(X);
end;


{ TAggGradientAlphaOneMinusXU8 }

function TAggGradientAlphaOneMinusXU8.ArrayOperator(X: Integer): Int8u;
begin
  Result := Int8u(255 - X);
end;

end.
