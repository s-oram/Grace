unit AggTransBilinear;

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
  AggTransAffine,
  AggSimulEq;

type
  PAggIteratorX = ^TAggIteratorX;
  TAggIteratorX = record
  private
    IncX, IncY, X, Y: Double;
  public
    procedure Initialize(Tx, Ty, Step: Double; M: PDoubleArray42); overload;

    procedure IncOperator;
  end;

  TAggTransBilinear = class(TAggTransAffine)
  private
    FValid: Boolean;
    FMatrix: array [0..3, 0..1] of Double;
  public
    constructor Create; override;

    // Arbitrary quadrangle transformations
    constructor Create(Src, Dst: PQuadDouble); overload;

    // Direct transformations
    constructor Create(X1, Y1, X2, Y2: Double; Quad: PQuadDouble); overload;
    constructor Create(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Reverse transformations
    constructor Create(Quad: PQuadDouble; X1, Y1, X2, Y2: Double); overload;
    constructor Create(Quad: PQuadDouble; Rect: TRectDouble); overload;

    // Set the transformations using two arbitrary quadrangles.
    procedure QuadToQuad(Src, Dst: PQuadDouble);

    // Set the direct transformations, i.e., rectangle -> quadrangle
    procedure RectToQuad(X1, Y1, X2, Y2: Double; Quad: PQuadDouble); overload;
    procedure RectToQuad(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Set the reverse transformations, i.e., quadrangle -> rectangle
    procedure QuadToRect(Quad: PQuadDouble; X1, Y1, X2, Y2: Double); overload;
    procedure QuadToRect(Quad: PQuadDouble; Rect: TRectDouble); overload;

    function GetBegin(X, Y, Step: Double): TAggIteratorX;

    // Check if the equations were solved successfully
    property IsValid: Boolean read FValid;
  end;


implementation


{ TAggIteratorX }

procedure TAggIteratorX.Initialize(Tx, Ty, Step: Double; M: PDoubleArray42);
begin
  IncX := M^[1, 0] * Step * Ty + M^[2, 0] * Step;
  IncY := M^[1, 1] * Step * Ty + M^[2, 1] * Step;

  X := M^[0, 0] + M^[1, 0] * Tx * Ty + M^[2, 0] * Tx + M^[3, 0] * Ty;
  Y := M^[0, 1] + M^[1, 1] * Tx * Ty + M^[2, 1] * Tx + M^[3, 1] * Ty;
end;

procedure TAggIteratorX.IncOperator;
begin
  X := X + IncX;
  Y := Y + IncY;
end;


procedure BilinearTransform(This: TAggTransBilinear; X, Y: PDouble);
var
  Temp: TPointDouble;
  Xy: Double;
begin
  Temp.X := X^;
  Temp.Y := Y^;
  Xy := Temp.X * Temp.Y;

  X^ := This.FMatrix[0, 0] + This.FMatrix[1, 0] * Xy +
    This.FMatrix[2, 0] * Temp.X + This.FMatrix[3, 0] * Temp.Y;
  Y^ := This.FMatrix[0, 1] + This.FMatrix[1, 1] * Xy +
    This.FMatrix[2, 1] * Temp.X + This.FMatrix[3, 1] * Temp.Y;
end;


{ TAggTransBilinear }

constructor TAggTransBilinear.Create;
begin
  inherited Create;

  FValid := False;

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Src, Dst: PQuadDouble);
begin
  inherited Create;

  QuadToQuad(Src, Dst);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(X1, Y1, X2, Y2: Double; Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(X1, Y1, X2, Y2, Quad);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Rect: TRectDouble; Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(Rect, Quad);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Quad: PQuadDouble; X1, Y1, X2, Y2: Double);
begin
  inherited Create;

  QuadToRect(Quad, X1, Y1, X2, Y2);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Quad: PQuadDouble; Rect: TRectDouble);
begin
  inherited Create;

  QuadToRect(Quad, Rect);

  Transform := @BilinearTransform;
end;

procedure TAggTransBilinear.QuadToQuad(Src, Dst: PQuadDouble);
var
  Left : TDoubleMatrix4x4;
  Right: TDoubleArray42;

  I, Ix, Iy: Cardinal;
begin
  for I := 0 to 3 do
  begin
    Ix := I * 2;
    Iy := Ix + 1;

    Left[I, 0] := 1.0;
    Left[I, 1] := PDouble(PtrComp(Src) + Ix * SizeOf(Double))^ *
      PDouble(PtrComp(Src) + Iy * SizeOf(Double))^;
    Left[I, 2] := PDouble(PtrComp(Src) + Ix * SizeOf(Double))^;
    Left[I, 3] := PDouble(PtrComp(Src) + Iy * SizeOf(Double))^;

    Right[I, 0] := PDouble(PtrComp(Dst) + Ix * SizeOf(Double))^;
    Right[I, 1] := PDouble(PtrComp(Dst) + Iy * SizeOf(Double))^;
  end;

  FValid := SimulEqSolve(@Left, @Right, @FMatrix, 4, 2);
end;

procedure TAggTransBilinear.RectToQuad(X1, Y1, X2, Y2: Double; Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src.Values[0] := X1;
  Src.Values[1] := Y1;
  Src.Values[2] := X2;
  Src.Values[3] := Y1;
  Src.Values[4] := X2;
  Src.Values[5] := Y2;
  Src.Values[6] := X1;
  Src.Values[7] := Y2;

  QuadToQuad(@Src, Quad);
end;

procedure TAggTransBilinear.RectToQuad(Rect: TRectDouble; Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggTransBilinear.QuadToRect(Quad: PQuadDouble; X1, Y1, X2, Y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst.Values[0] := X1;
  Dst.Values[1] := Y1;
  Dst.Values[2] := X2;
  Dst.Values[3] := Y1;
  Dst.Values[4] := X2;
  Dst.Values[5] := Y2;
  Dst.Values[6] := X1;
  Dst.Values[7] := Y2;

  QuadToQuad(Quad, @Dst);
end;

procedure TAggTransBilinear.QuadToRect(Quad: PQuadDouble; Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggTransBilinear.GetBegin(X, Y, Step: Double): TAggIteratorX;
begin
  Result.Initialize(X, Y, Step, @FMatrix);
end;

end.
