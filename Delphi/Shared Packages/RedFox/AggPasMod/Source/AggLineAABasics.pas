unit AggLineAABasics;

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
  AggBasics;

const
  CAggLineSubpixelShift = 8;
  CAggLineSubpixelSize = 1 shl CAggLineSubpixelShift;
  CAggLineSubpixelMask = CAggLineSubpixelSize - 1;

  CAggLineMrSubpixelShift = 4;
  CAggLineMrSubpixelSize = 1 shl CAggLineMrSubpixelShift;
  CAggLineMrSubpixelMask = CAggLineMrSubpixelSize - 1;

type
  PAggLineParameters = ^TAggLineParameters;
  TAggLineParameters = record
    X1, Y1, X2, Y2, Sx, Sy: Integer;
    Delta: TPointInteger;
    Vertical: Boolean;
    IncValue, Len, Octant: Integer;
  public
    procedure Initialize(X1, Y1, X2, Y2, Len: Integer); overload;

    function OrthogonalQuadrant: Cardinal;
    function DiagonalQuadrant: Cardinal;

    function SameOrthogonalQuadrant(Lp: PAggLineParameters): Boolean;
    function SameDiagonalQuadrant(Lp: PAggLineParameters): Boolean;
  end;

function LineMedResolution(X: Integer): Integer;
function LineHighResolution(X: Integer): Integer;

function LineDoubleHighResolution(X: Integer): Integer;
function LineCoord(X: Double): Integer;

procedure Bisectrix(L1, L2: PAggLineParameters; X, Y: PInteger);

procedure FixDegenerateBisectrixStart(Lp: PAggLineParameters; X, Y: PInteger);
procedure FixDegenerateBisectrixEnd(Lp: PAggLineParameters; X, Y: PInteger);

implementation


const
  // The number of the octant is determined as a 3-bit value as follows:
  // bit 0 = vertical flag
  // bit 1 = sx < 0
  // bit 2 = sy < 0
  //
  // [N] shows the number of the orthogonal quadrant
  // <M> shows the number of the diagonal quadrant
  //               <1>
  //   [1]          |          [0]
  //       . (3)011 | 001(1) .
  //         .      |      .
  //           .    |    .
  //             .  |  .
  //    (2)010     .|.     000(0)
  // <2> ----------.+.----------- <0>
  //    (6)110   .  |  .   100(4)
  //           .    |    .
  //         .      |      .
  //       .        |        .
  //         (7)111 | 101(5)
  //   [2]          |          [3]
  //               <3>
  // 0 ,1 ,2 ,3 ,4 ,5 ,6 ,7
  COrthogonalQuadrant: array [0..7] of Int8u = (0, 0, 1, 1, 3, 3, 2, 2);
  CDiagonalQuadrant: array [0..7] of Int8u = (0, 1, 2, 1, 0, 3, 2, 3);


{ TAggLineParameters }

procedure TAggLineParameters.Initialize(X1, Y1, X2, Y2, Len: Integer);
begin
  Self.X1 := X1;
  Self.Y1 := Y1;
  Self.X2 := X2;
  Self.Y2 := Y2;
  Self.Delta.X := Abs(X2 - X1);
  Self.Delta.Y := Abs(Y2 - Y1);

  if X2 > X1 then
    Sx := 1
  else
    Sx := -1;

  if Y2 > Y1 then
    Sy := 1
  else
    Sy := -1;

  Vertical := Self.Delta.Y >= Self.Delta.X;

  if Vertical then
    IncValue := Sy
  else
    IncValue := Sx;

  Self.Len := Len;

  Octant := (Sy and 4) or (Sx and 2) or Integer(Vertical);
end;

function TAggLineParameters.OrthogonalQuadrant;
begin
  Result := COrthogonalQuadrant[Octant];
end;

function TAggLineParameters.DiagonalQuadrant;
begin
  Result := CDiagonalQuadrant[Octant];
end;

function TAggLineParameters.SameOrthogonalQuadrant;
begin
  Result := COrthogonalQuadrant[Octant] = COrthogonalQuadrant[Lp.Octant];
end;

function TAggLineParameters.SameDiagonalQuadrant;
begin
  Result := CDiagonalQuadrant[Octant] = CDiagonalQuadrant[Lp.Octant];
end;

function LineMedResolution;
begin
  Result := ShrInt32(X, CAggLineSubpixelShift - CAggLineMrSubpixelShift);
end;

function LineHighResolution;
begin
  Result := X shl (CAggLineSubpixelShift - CAggLineMrSubpixelShift);
end;

function LineDoubleHighResolution;
begin
  Result := X shl CAggLineSubpixelShift;
end;

function LineCoord;
begin
  Result := Trunc(X * CAggLineSubpixelSize);
end;

procedure Bisectrix(L1, L2: PAggLineParameters; X, Y: PInteger);
var
  K, Tx, Ty, Dx, Dy: Double;
begin
  K := L2.Len / L1.Len;
  Tx := L2.X2 - (L2.X1 - L1.X1) * K;
  Ty := L2.Y2 - (L2.Y1 - L1.Y1) * K;

  // All bisectrices must be on the right of the line
  // If the next point is on the left (l1 => l2.2)
  // then the bisectix should be rotated by 180 degrees.
  Dx := L2.X2 - L2.X1;
  Dy := L2.Y2 - L2.Y1;
  if Dx * (L2.Y1 - L1.Y1) < Dy * (L2.X1 - L1.X1) + 100.0 then
  begin
    Tx := Tx - 2 * (Tx - L2.X1);
    Ty := Ty - 2 * (Ty - L2.Y1);
  end;

  // Check if the bisectrix is too short
  Dx := Tx - L2.X1;
  Dy := Ty - L2.Y1;

  if Trunc(Sqrt(Sqr(Dx) + Sqr(Dy))) < CAggLineSubpixelSize then
  begin
    X^ := ShrInt32(L2.X1 + L2.X1 + (L2.Y1 - L1.Y1) + (L2.Y2 - L2.Y1), 1);
    Y^ := ShrInt32(L2.Y1 + L2.Y1 - (L2.X1 - L1.X1) - (L2.X2 - L2.X1), 1);

    Exit;
  end;

  X^ := Trunc(Tx);
  Y^ := Trunc(Ty);
end;

procedure FixDegenerateBisectrixStart(Lp: PAggLineParameters;
  X, Y: PInteger);
var
  D: Integer;
begin
  Assert(Assigned(Lp));
  D := Trunc((IntToDouble(X^ - Lp.X2) * IntToDouble(Lp.Y2 - Lp.Y1) -
    IntToDouble(Y^ - Lp.Y2) * IntToDouble(Lp.X2 - Lp.X1)) / Lp.Len);

  if D < CAggLineSubpixelSize then
  begin
    X^ := Lp.X1 + (Lp.Y2 - Lp.Y1);
    Y^ := Lp.Y1 - (Lp.X2 - Lp.X1);
  end;
end;

procedure FixDegenerateBisectrixEnd(Lp: PAggLineParameters; X, Y: PInteger);
var
  D: Integer;
begin
  Assert(Assigned(Lp));
  D := Trunc((IntToDouble(X^ - Lp.X2) * IntToDouble(Lp.Y2 - Lp.Y1) -
    IntToDouble(Y^ - Lp.Y2) * IntToDouble(Lp.X2 - Lp.X1)) / Lp.Len);

  if D < CAggLineSubpixelSize then
  begin
    X^ := Lp.X2 + (Lp.Y2 - Lp.Y1);
    Y^ := Lp.Y2 - (Lp.X2 - Lp.X1);
  end;
end;

end.
