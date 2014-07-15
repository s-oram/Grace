unit AggDdaLine;

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
  AggBasics;

const
  CAggSubpixelShift = 8;
  CAggSubpixelSize = 1 shl CAggSubpixelShift;
  CAggSubpixelMask = CAggSubpixelSize - 1;

type
  TAggDdaLineInterpolator = record
  private
    FY, FInc, FDeltaY, FFractionShift, YShift: Integer;
    function GetY: Integer;
  public
    procedure Initialize(FS: Integer; YS: Integer = 0); overload;
    procedure Initialize(Y1, Y2: Integer; Count: Cardinal; FS: Integer;
      YS: Integer = 0); overload;

    procedure PlusOperator;
    procedure MinusOperator;
    procedure IncOperator(N: Integer);
    procedure DecOperator(N: Integer);

    property Y: Integer read GetY;
    property DeltaY: Integer read FDeltaY;
  end;

  TAggDda2LineInterpolator = record
  private
    FCount, FLft, FRem, FMod, FY: Integer;
  public
    procedure Initialize(Y1, Y2, Count: Integer); overload; // Forward-adjusted line
    procedure Initialize(Y, Count: Integer); overload; // Backward-adjusted line

    procedure PlusOperator;
    procedure MinusOperator;

    procedure AdjustForward;
    procedure AdjustBackward;

    property ModValue: Integer read FMod;
    property RemValue: Integer read FRem;
    property Lft: Integer read FLft;
    property Y: Integer read FY;
  end;

  TAggLineBresenhamInterpolator = record
  private
    FLowResolution: TRectInteger;

    FVer: Boolean;
    FLength: Cardinal;
    FInc: Integer;

    FInterpolator: TAggDda2LineInterpolator;
    function GetX2: Integer;
    function GetY2: Integer;
    function GetX2HighResolution: Integer;
    function GetY2HighResolution: Integer;
  public
    procedure Initialize(X1, Y1, X2, Y2: Integer); overload;
    procedure Initialize(Point1, Point2: TPointInteger); overload;

    function LineLowResolution(V: Integer): Integer;

    function Inc: Integer;

    procedure Hstep;
    procedure Vstep;

    property IsVer: Boolean read FVer;
    property Length: Cardinal read FLength;
    property X1: Integer read FLowResolution.X1;
    property Y1: Integer read FLowResolution.Y1;
    property X2: Integer read GetX2;
    property Y2: Integer read GetY2;
    property X2HighResolution: Integer read GetX2HighResolution;
    property Y2HighResolution: Integer read GetY2HighResolution;
  end;

implementation


{ TAggDdaLineInterpolator }

procedure TAggDdaLineInterpolator.Initialize(FS: Integer; YS: Integer = 0);
begin
  FFractionShift := FS;

  YShift := YS;
end;

procedure TAggDdaLineInterpolator.Initialize(Y1, Y2: Integer; Count: Cardinal;
  FS: Integer; YS: Integer = 0);
begin
  Initialize(FS, YS);

  FY := Y1;
  FInc := ((Y2 - Y1) shl FFractionShift) div Count;
  FDeltaY := 0;
end;

procedure TAggDdaLineInterpolator.PlusOperator;
begin
  Inc(FDeltaY, FInc);
end;

procedure TAggDdaLineInterpolator.MinusOperator;
begin
  Dec(FDeltaY, FInc);
end;

procedure TAggDdaLineInterpolator.IncOperator;
begin
  Inc(FDeltaY, FInc * N);
end;

procedure TAggDdaLineInterpolator.DecOperator;
begin
  Dec(FDeltaY, FInc * N);
end;

function TAggDdaLineInterpolator.GetY: Integer;
begin
  Result := FY + (ShrInt32(FDeltaY, FFractionShift - YShift));
end;


{ TAggDda2LineInterpolator }

procedure TAggDda2LineInterpolator.Initialize(Y1, Y2, Count: Integer);
begin
  if Count <= 0 then
    FCount := 1
  else
    FCount := Count;

  FLft := Trunc((Y2 - Y1) / FCount);
  FRem := Trunc((Y2 - Y1) mod FCount);
  FMod := FRem;
  FY := Y1;

  if FMod <= 0 then
  begin
    FMod := FMod + Count;
    FRem := FRem + Count;

    Dec(FLft);
  end;

  FMod := FMod - Count;
end;

procedure TAggDda2LineInterpolator.Initialize(Y, Count: Integer);
begin
  if Count <= 0 then
    FCount := 1
  else
    FCount := Count;

  FLft := Y div FCount;
  FRem := Y mod FCount;
  FMod := FRem;
  FY := 0;

  if FMod <= 0 then
  begin
    Inc(FMod, Count);
    Inc(FRem, Count);
    Dec(FLft);
  end;
end;

procedure TAggDda2LineInterpolator.PlusOperator;
begin
  Inc(FMod, FRem);
  Inc(FY, FLft);

  if FMod > 0 then
  begin
    Dec(FMod, FCount);
    Inc(FY);
  end;
end;

procedure TAggDda2LineInterpolator.MinusOperator;
begin
  if FMod <= FRem then
  begin
    Inc(FMod, FCount);
    Dec(FY);
  end;

  Dec(FMod, FRem);
  Dec(FY, FLft);
end;

procedure TAggDda2LineInterpolator.AdjustForward;
begin
  Dec(FMod, FCount);
end;

procedure TAggDda2LineInterpolator.AdjustBackward;
begin
  Inc(FMod, FCount);
end;


{ TAggLineBresenhamInterpolator }

procedure TAggLineBresenhamInterpolator.Initialize(X1, Y1, X2, Y2: Integer);
begin
  FLowResolution.X1 := LineLowResolution(X1);
  FLowResolution.Y1 := LineLowResolution(Y1);
  FLowResolution.X2 := LineLowResolution(X2);
  FLowResolution.Y2 := LineLowResolution(Y2);

  FVer := Abs(FLowResolution.X2 - FLowResolution.X1) < Abs(FLowResolution.Y2 - FLowResolution.Y1);

  if FVer then
    FLength := Abs(FLowResolution.Y2 - FLowResolution.Y1)
  else
    FLength := Abs(FLowResolution.X2 - FLowResolution.X1);

  if FVer then
    if Y2 > Y1 then
      FInc := 1
    else
      FInc := -1
  else if X2 > X1 then
    FInc := 1
  else
    FInc := -1;

  if FVer then
    FInterpolator.Initialize(X1, X2, FLength)
  else
    FInterpolator.Initialize(Y1, Y2, FLength);
end;

procedure TAggLineBresenhamInterpolator.Initialize(Point1,
  Point2: TPointInteger);
begin
  FLowResolution.Point1 := Point1;
  FLowResolution.Point2 := Point2;

  FVer := Abs(FLowResolution.X2 - FLowResolution.X1) < Abs(FLowResolution.Y2 - FLowResolution.Y1);

  if FVer then
    FLength := Abs(FLowResolution.Y2 - FLowResolution.Y1)
  else
    FLength := Abs(FLowResolution.X2 - FLowResolution.X1);

  if FVer then
    if Y2 > Y1 then
      FInc := 1
    else
      FInc := -1
  else if X2 > X1 then
    FInc := 1
  else
    FInc := -1;

  if FVer then
    FInterpolator.Initialize(X1, X2, FLength)
  else
    FInterpolator.Initialize(Y1, Y2, FLength);
end;

function TAggLineBresenhamInterpolator.LineLowResolution;
begin
  Result := ShrInt32(V, CAggSubpixelShift);
end;

function TAggLineBresenhamInterpolator.Inc;
begin
  Result := FInc;
end;

procedure TAggLineBresenhamInterpolator.Hstep;
begin
  FInterpolator.PlusOperator;

  FLowResolution.X1 := FLowResolution.X1 + FInc;
end;

procedure TAggLineBresenhamInterpolator.Vstep;
begin
  FInterpolator.PlusOperator;

  FLowResolution.Y1 := FLowResolution.Y1 + FInc;
end;

function TAggLineBresenhamInterpolator.GetX2;
begin
  Result := LineLowResolution(FInterpolator.Y);
end;

function TAggLineBresenhamInterpolator.GetY2;
begin
  Result := LineLowResolution(FInterpolator.Y);
end;

function TAggLineBresenhamInterpolator.GetX2HighResolution;
begin
  Result := FInterpolator.Y;
end;

function TAggLineBresenhamInterpolator.GetY2HighResolution;
begin
  Result := FInterpolator.Y;
end;

end.
