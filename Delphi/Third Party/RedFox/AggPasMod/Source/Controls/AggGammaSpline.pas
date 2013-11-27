unit AggGammaSpline;

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
  AggBSpline,
  AggVertexSource;

type
  // Class-helper for calculation Gamma-correction arrays. A Gamma-correction
  // array is an array of 256 Cardinal chars that determine the actual values
  // of Anti-Aliasing for each pixel coverage value from 0 to 255. If all the
  // values in the array are equal to its index, i.e. 0,1,2,3,... there's
  // no Gamma-correction. Class agg::polyfill allows you to use custom
  // Gamma-correction arrays. You can calculate it using any approach, and
  // class TAggGammaSpline allows you to calculate almost any reasonable shape
  // of the Gamma-curve with using only 4 values - kx1, ky1, kx2, ky2.
  //
  //                               kx2
  //     +----------------------------------+
  //     |                 |        |    .  |
  //     |                 |        | .     |
  //     |                 |       .  ------| ky2
  //     |                 |    .           |
  //     |                 | .              |
  //     |----------------.|----------------|
  //     |             .   |                |
  //     |          .      |                |
  // ky1 |-------.         |                |
  //     |    .   |        |                |
  //     | .      |        |                |
  //     +----------------------------------+
  //             kx1
  //
  // Each value can be in range [0...2]. Value 1.0 means one quarter of the
  // bounding rectangle. Function values() calculates the curve by these
  // 4 values. After calling it one can get the Gamma-array with call Gamma().
  // Class also supports the vertex source interface, i.e rewind() and
  // vertex(). It's made for convinience and used in class GammaControl.
  // Before calling rewind/vertex one must set the bounding box
  // box() using pixel coordinates.

  TAggGammaSpline = class(TAggVertexSource)
  private
    FGamma: array [0..255] of Int8u;

    FX, FY: array [0..3] of Double;
    FX1, FY1, FX2, FY2,

    FCurrentX: Double;
    FSpline: TAggBSpline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Values(Kx1, Ky1, Kx2, Ky2: Double); overload;
    procedure Values(Kx1, Ky1, Kx2, Ky2: PDouble); overload;

    function Gamma: PInt8u;
    function GetY(X: Double): Double;
    procedure Box(X1, Y1, X2, Y2: Double);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation


{ TAggGammaSpline }

constructor TAggGammaSpline.Create;
begin
  FSpline := TAggBSpline.Create;

  FX1 := 0;
  FY1 := 0;
  FX2 := 10;
  FY2 := 10;

  FCurrentX := 0.0;

  Values(1.0, 1.0, 1.0, 1.0);
end;

destructor TAggGammaSpline.Destroy;
begin
  FSpline.Free;
  inherited;
end;

procedure TAggGammaSpline.Values(Kx1, Ky1, Kx2, Ky2: Double);
var
  I: Integer;
begin
  if Kx1 < 0.001 then
    Kx1 := 0.001;

  if Kx1 > 1.999 then
    Kx1 := 1.999;

  if Ky1 < 0.001 then
    Ky1 := 0.001;

  if Ky1 > 1.999 then
    Ky1 := 1.999;

  if Kx2 < 0.001 then
    Kx2 := 0.001;

  if Kx2 > 1.999 then
    Kx2 := 1.999;

  if Ky2 < 0.001 then
    Ky2 := 0.001;

  if Ky2 > 1.999 then
    Ky2 := 1.999;

  FX[0] := 0.0;
  FY[0] := 0.0;
  FX[1] := Kx1 * 0.25;
  FY[1] := Ky1 * 0.25;
  FX[2] := 1.0 - Kx2 * 0.25;
  FY[2] := 1.0 - Ky2 * 0.25;
  FX[3] := 1.0;
  FY[3] := 1.0;

  FSpline.Init(4, @FX, @FY);

  for I := 0 to 255 do
    FGamma[I] := Trunc(GetY(I / 255.0) * 255.0);
end;

procedure TAggGammaSpline.Values(Kx1, Ky1, Kx2, Ky2: PDouble);
begin
  Kx1^ := FX[1] * 4.0;
  Ky1^ := FY[1] * 4.0;
  Kx2^ := (1.0 - FX[2]) * 4.0;
  Ky2^ := (1.0 - FY[2]) * 4.0;
end;

function TAggGammaSpline.Gamma;
begin
  Result := @FGamma[0];
end;

function TAggGammaSpline.GetY;
var
  Val: Double;

begin
  if X < 0.0 then
    X := 0.0;

  if X > 1.0 then
    X := 1.0;

  Val := FSpline.Get(X);

  if Val < 0.0 then
    Val := 0.0;

  if Val > 1.0 then
    Val := 1.0;

  Result := Val;
end;

procedure TAggGammaSpline.Box;
begin
  FX1 := X1;
  FY1 := Y1;
  FX2 := X2;
  FY2 := Y2;
end;

procedure TAggGammaSpline.Rewind(PathID: Cardinal);
begin
  FCurrentX := 0.0;
end;

function TAggGammaSpline.Vertex(X, Y: PDouble): Cardinal;
begin
  if FCurrentX = 0.0 then
  begin
    X^ := FX1;
    Y^ := FY1;

    FCurrentX := FCurrentX + (1.0 / (FX2 - FX1));
    Result := CAggPathCmdMoveTo;

    Exit;
  end;

  if FCurrentX > 1.0 then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  X^ := FX1 + FCurrentX * (FX2 - FX1);
  Y^ := FY1 + GetY(FCurrentX) * (FY2 - FY1);

  FCurrentX := FCurrentX + (1.0 / (FX2 - FX1));
  Result := CAggPathCmdLineTo;
end;

end.
