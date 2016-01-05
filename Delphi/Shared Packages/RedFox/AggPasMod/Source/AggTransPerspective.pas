unit AggTransPerspective;

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
//                                                                            //
// Perspective 2D transformations                                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  Math,
  AggBasics,
  AggMath,
  AggSimulEq,
  AggTransAffine;

type
  PAggIteratorX23 = ^TAggIteratorX23;
  TAggIteratorX23 = record
  public
    Den, DenStep: Double;
    NomStep, Nom: TPointDouble;

    X, Y: Double;
  public
    procedure Initialize(Tx, Ty, Step: Double; M: PDoubleArray8); overload;

    procedure IncOperator;
  end;

  TAggTransPerspective23 = class(TAggTransAffine)
  private
    FValid: Boolean;
    FMatrix: TDoubleArray8;
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

    function GetBegin(X, Y, Step: Double): TAggIteratorX23;

    // Check if the equations were solved successfully
    property IsValid: Boolean read FValid;
  end;

  PAggQuadrilateral = ^TAggQuadrilateral;
  TAggQuadrilateral = array [0..8] of Double;

  TAggTransPerspective = class;

  TAggIteratorXRecord = record
  public
    Den, DenStep: Double;
    Nom, NomStep: TPointDouble;

    X, Y: Double;
  public
    procedure Initialize(X, Y, Step: Double; M: TAggTransPerspective); overload;

    procedure OperatorInc;
  end;

  TAggTransPerspective = class(TAggTransAffine)
  private
    Sx, Shy, W0, Shx, Sy, W1, Tx, Ty, W2: Double;

    TransformAffine: TAggProcTransform;

    // private
    function GetBegin(X, Y, Step: Double): TAggIteratorXRecord;
    procedure InitializeTransforms;
  public
    // ------------------------------------------------------- Construction
    // Identity matrix
    constructor Create; overload;

    // Custom matrix
    constructor Create(V0, V1, V2, V3, V4, V5, V6, V7, V8: Double); overload;

    // Custom matrix from m[9]
    constructor Create(M: PAggQuadrilateral); overload;

    // From affine
    constructor CreateAffine(A: TAggTransAffine);

    // From affine
    constructor Create(P: TAggTransPerspective); overload;

    // Rectangle to quadrilateral
    constructor Create(X1, Y1, X2, Y2: Double;
      Quad: PAggQuadrilateral); overload;
    constructor Create(Rect: TRectDouble; Quad: PAggQuadrilateral); overload;

    // Quadrilateral to rectangle
    constructor Create(Quad: PAggQuadrilateral;
      X1, Y1, X2, Y2: Double); overload;
    constructor Create(Quad: PAggQuadrilateral; Rect: TRectDouble); overload;

    // Arbitrary quadrilateral transformations
    constructor Create(Src, Dst: PAggQuadrilateral); overload;

    // -------------------------------------- Quadrilateral transformations
    // The arguments are double[8] that are mapped to quadrilaterals:
    // x1,y1, x2,y2, x3,y3, x4,y4
    function QuadToQuad(Qs, Qd: PAggQuadrilateral): Boolean;

    function RectToQuad(X1, Y1, X2, Y2: Double; Q: PAggQuadrilateral): Boolean;
      overload;
    function RectToQuad(Rect: TRectDouble; Q: PAggQuadrilateral): Boolean;
      overload;

    function QuadToRect(Q: PAggQuadrilateral; X1, Y1, X2, Y2: Double): Boolean;
      overload;
    function QuadToRect(Q: PAggQuadrilateral; Rect: TRectDouble): Boolean;
      overload;
    function QuadToRect(var Q: TAggQuadrilateral; Rect: TRectDouble): Boolean;
      overload;

    // Map square (0,0,1,1) to the quadrilateral and vice versa
    function SquareToQuad(Q: PAggQuadrilateral): Boolean;
    function QuadToSquare(Q: PAggQuadrilateral): Boolean;

    // --------------------------------------------------------- Operations
    // Reset - load an identity matrix
    procedure Reset; override;

    // Invert matrix. Returns false in degenerate case
    function Invert: Boolean;

    // Direct transformations operations
    function Translate(X, Y: Double): TAggTransPerspective;
    function Rotate(A: Double): TAggTransPerspective;
    function Scale(S: Double): TAggTransPerspective; overload;
    function Scale(X, Y: Double): TAggTransPerspective; overload;

    // Multiply the matrix by another one
    function Multiply(A: TAggTransPerspective): TAggTransPerspective;

    // Multiply "m" by "this" and assign the result to "this"
    function PreMultiply(B: TAggTransPerspective): TAggTransPerspective;

    // Multiply matrix to inverse of another one
    function MultiplyInv(M: TAggTransPerspective): TAggTransPerspective;

    // Multiply inverse of "m" by "this" and assign the result to "this"
    function PreMultiplyInv(M: TAggTransPerspective): TAggTransPerspective;

    // Multiply the matrix by another one
    function MultiplyAffine(A: TAggTransAffine): TAggTransPerspective;

    // Multiply "m" by "this" and assign the result to "this"
    function PreMultiplyAffine(B: TAggTransAffine): TAggTransPerspective;

    // Multiply the matrix by inverse of another one
    function MultiplyInvAffine(M: TAggTransAffine): TAggTransPerspective;

    // Multiply inverse of "m" by "this" and assign the result to "this"
    function PreMultiplyInvAffine(M: TAggTransAffine): TAggTransPerspective;

    // --------------------------------------------------------- Load/Store
    procedure StoreTo(M: PAggQuadrilateral);
    function LoadFrom(M: PAggQuadrilateral): TAggTransPerspective;

    // ---------------------------------------------------------- Auxiliary
    function FromAffine(A: TAggTransAffine): TAggTransPerspective;

    function Determinant: Double;
    function DeterminantReciprocal: Double;

    function IsValid(Epsilon: Double = CAggAffineEpsilon): Boolean;
    function IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;
    function IsEqual(M: TAggTransPerspective;
      Epsilon: Double = CAggAffineEpsilon): Boolean;

    // Determine the major affine parameters. Use with caution
    // considering possible degenerate cases.
    function Scale: Double; overload;
    function Rotation: Double;

    procedure Translation(Dx, Dy: PDouble);
    procedure Scaling(X, Y: PDouble);
    procedure ScalingAbs(X, Y: PDouble);
  end;

implementation


{ TAggIteratorX23 }

procedure TAggIteratorX23.Initialize(Tx, Ty, Step: Double; M: PDoubleArray8);
var
  D: Double;
begin
  Den := M[6] * Tx + M[7] * Ty + 1;
  DenStep := M[6] * Step;

  Nom.X := M[0] + M[1] * Tx + M[2] * Ty;
  Nom.Y := M[3] + M[4] * Tx + M[5] * Ty;
  NomStep := PointDouble(M[1] * Step, M[4] * Step);

  D := 1 / Den;
  X := Nom.X * D;
  Y := Nom.Y * D;
end;

procedure TAggIteratorX23.IncOperator;
var
  D: Double;
begin
  Den := Den + DenStep;
  Nom.X := Nom.X + NomStep.X;
  Nom.Y := Nom.Y + NomStep.Y;

  D := 1 / Den;
  X := Nom.X * D;
  Y := Nom.Y * D;
end;

procedure PerspectiveTransform23(This: TAggTransPerspective23; X, Y: PDouble);
var
  Tx, Ty, D: Double;
begin
  Tx := X^;
  Ty := Y^;
  D := 1 / (This.FMatrix[6] * Tx + This.FMatrix[7] * Ty + 1);

  X^ := (This.FMatrix[0] + This.FMatrix[1] * Tx + This.FMatrix[2] * Ty) * D;
  Y^ := (This.FMatrix[3] + This.FMatrix[4] * Tx + This.FMatrix[5] * Ty) * D;
end;


{ TAggTransPerspective23 }

constructor TAggTransPerspective23.Create;
begin
  inherited Create;

  FValid := False;

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Src, Dst: PQuadDouble);
begin
  inherited Create;

  QuadToQuad(Src, Dst);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(X1, Y1, X2, Y2: Double;
  Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(X1, Y1, X2, Y2, Quad);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Rect: TRectDouble; Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(Rect, Quad);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Quad: PQuadDouble;
  X1, Y1, X2, Y2: Double);
begin
  inherited Create;

  QuadToRect(Quad, X1, Y1, X2, Y2);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Quad: PQuadDouble; Rect: TRectDouble);
begin
  inherited Create;

  QuadToRect(Quad, Rect);

  Transform := @PerspectiveTransform23;
end;

procedure TAggTransPerspective23.QuadToQuad(Src, Dst: PQuadDouble);
var
  Left : TDoubleMatrix8x8;
  Right: TDoubleMatrix8x1;

  I, Ix, Iy: Cardinal;
begin
  for I := 0 to 3 do
  begin
    Ix := I * 2;
    Iy := Ix + 1;

    Left[Ix, 0] := 1.0;
    Left[Ix, 1] := PDouble(PtrComp(Src) + Ix * SizeOf(Double))^;
    Left[Ix, 2] := PDouble(PtrComp(Src) + Iy * SizeOf(Double))^;
    Left[Ix, 3] := 0.0;
    Left[Ix, 4] := 0.0;
    Left[Ix, 5] := 0.0;
    Left[Ix, 6] := -PDouble(PtrComp(Src) + Ix * SizeOf(Double))^ *
      PDouble(PtrComp(Dst) + Ix * SizeOf(Double))^;
    Left[Ix, 7] := -PDouble(PtrComp(Src) + Iy * SizeOf(Double))^ *
      PDouble(PtrComp(Dst) + Ix * SizeOf(Double))^;
    Right[Ix, 0] := PDouble(PtrComp(Dst) + Ix * SizeOf(Double))^;

    Left[Iy, 0] := 0.0;
    Left[Iy, 1] := 0.0;
    Left[Iy, 2] := 0.0;
    Left[Iy, 3] := 1.0;
    Left[Iy, 4] := PDouble(PtrComp(Src) + Ix * SizeOf(Double))^;
    Left[Iy, 5] := PDouble(PtrComp(Src) + Iy * SizeOf(Double))^;
    Left[Iy, 6] := -PDouble(PtrComp(Src) + Ix * SizeOf(Double))^ *
      PDouble(PtrComp(Dst) + Iy * SizeOf(Double))^;
    Left[Iy, 7] := -PDouble(PtrComp(Src) + Iy * SizeOf(Double))^ *
      PDouble(PtrComp(Dst) + Iy * SizeOf(Double))^;
    Right[Iy, 0] := PDouble(PtrComp(Dst) + Iy * SizeOf(Double))^;
  end;

  FValid := SimulEqSolve(@Left, @Right, @FMatrix, 8, 1);
end;

procedure TAggTransPerspective23.RectToQuad(X1, Y1, X2, Y2: Double;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  QuadToQuad(@Src, Quad);
end;

procedure TAggTransPerspective23.RectToQuad(Rect: TRectDouble;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggTransPerspective23.QuadToRect(Quad: PQuadDouble; X1, Y1, X2,
  Y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  QuadToQuad(Quad, @Dst);
end;

procedure TAggTransPerspective23.QuadToRect(Quad: PQuadDouble; Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggTransPerspective23.GetBegin(X, Y, Step: Double): TAggIteratorX23;
begin
  Result.Initialize(X, Y, Step, @FMatrix);
end;


{ TAggIteratorXRecord }

procedure TAggIteratorXRecord.Initialize(X, Y, Step: Double;
  M: TAggTransPerspective);
var
  D: Double;
begin
  Den := X * M.W0 + Y * M.W1 + M.W2;
  DenStep := M.W0 * Step;

  Nom.X := X * M.Sx + Y * M.Shx + M.Tx;
  Nom.Y := X * M.Shy + Y * M.Sy + M.Ty;
  NomStep := PointDouble(Step * M.Sx, Step * M.Shy);

  D := 1 / Den;
  Self.X := Nom.X * D;
  Self.Y := Nom.Y * D;
end;

procedure TAggIteratorXRecord.OperatorInc;
var
  D: Double;
begin
  Den := Den + DenStep;
  Nom := PointDouble(Nom.X + NomStep.X, Nom.Y + NomStep.Y);

  D := 1 / Den;
  X := Nom.X * D;
  Y := Nom.Y * D;
end;

// Direct transformation of x and y
procedure TransPerspectiveTransform(This: TAggTransPerspective; Px,
  Py: PDouble);
var
  X, Y, M: Double;
begin
  X := Px^;
  Y := Py^;

  try
    M := 1 / (X * This.W0 + Y * This.W1 + This.W2);
  except
    M := 0;
  end;

  Px^ := M * (X * This.Sx + Y * This.Shx + This.Tx);
  Py^ := M * (X * This.Shy + Y * This.Sy + This.Ty);
end;

// Direct transformation of x and y, affine part only
procedure TransPerspectiveTransformAffine(This: TAggTransPerspective; X,
  Y: PDouble);
var
  Tmp: Double;
begin
  Tmp := X^;

  X^ := Tmp * This.Sx + Y^ * This.Shx + This.Tx;
  Y^ := Tmp * This.Shy + Y^ * This.Sy + This.Ty;
end;

// Direct transformation of x and y, 2x2 matrix only, no translation
procedure TransPerspectiveTransform2x2(This: TAggTransPerspective; X,
  Y: PDouble);
var
  Tmp: Double;
begin
  Tmp := X^;

  X^ := Tmp * This.Sx + Y^ * This.Shx;
  Y^ := Tmp * This.Shy + Y^ * This.Sy;
end;

// Inverse transformation of x and y. It works sLow because
// it explicitly inverts the matrix on every call. For massive
// operations it's better to invert() the matrix and then use
// direct transformations.
procedure TransPerspectiveInverseTransform(This: TAggTransPerspective; X,
  Y: PDouble);
var
  T: TAggTransPerspective;
begin
  T := TAggTransPerspective.Create(This);
  try
    if T.Invert then
      T.Transform(T, X, Y);
  finally
    T.Free;
  end;
end;


{ TAggTransPerspective }

constructor TAggTransPerspective.Create;
begin
  inherited Create;
  InitializeTransforms;

  Sx := 1;
  Shy := 0;
  W0 := 0;
  Shx := 0;
  Sy := 1;
  W1 := 0;
  Tx := 0;
  Ty := 0;
  W2 := 1;
end;

constructor TAggTransPerspective.Create(V0, V1, V2, V3, V4, V5, V6, V7,
  V8: Double);
begin
  inherited Create;
  InitializeTransforms;

  Sx := V0;
  Shy := V1;
  W0 := V2;
  Shx := V3;
  Sy := V4;
  W1 := V5;
  Tx := V6;
  Ty := V7;
  W2 := V8;
end;

constructor TAggTransPerspective.Create(M: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  Sx := M[0];
  Shy := M[1];
  W0 := M[2];
  Shx := M[3];
  Sy := M[4];
  W1 := M[5];
  Tx := M[6];
  Ty := M[7];
  W2 := M[8];
end;

constructor TAggTransPerspective.CreateAffine(A: TAggTransAffine);
begin
  inherited Create;
  InitializeTransforms;

  Sx := A.M0;
  Shy := A.M1;
  W0 := 0;
  Shx := A.M2;
  Sy := A.M3;
  W1 := 0;
  Tx := A.M4;
  Ty := A.M5;
  W2 := 1;
end;

constructor TAggTransPerspective.Create(P: TAggTransPerspective);
begin
  inherited Create;
  InitializeTransforms;

  Sx := P.Sx;
  Shy := P.Shy;
  W0 := P.W0;
  Shx := P.Shx;
  Sy := P.Sy;
  W1 := P.W1;
  Tx := P.Tx;
  Ty := P.Ty;
  W2 := P.W2;
end;

constructor TAggTransPerspective.Create(X1, Y1, X2, Y2: Double;
  Quad: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  RectToQuad(X1, Y1, X2, Y2, Quad);
end;

constructor TAggTransPerspective.Create(Quad: PAggQuadrilateral;
  X1, Y1, X2, Y2: Double);
begin
  inherited Create;
  InitializeTransforms;

  QuadToRect(Quad, X1, Y1, X2, Y2);
end;

constructor TAggTransPerspective.Create(Src, Dst: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  QuadToQuad(Src, Dst);
end;

constructor TAggTransPerspective.Create(Rect: TRectDouble;
  Quad: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  RectToQuad(Rect, Quad);
end;

constructor TAggTransPerspective.Create(Quad: PAggQuadrilateral;
  Rect: TRectDouble);
begin
  inherited Create;
  InitializeTransforms;

  QuadToRect(Quad, Rect);
end;

procedure TAggTransPerspective.InitializeTransforms;
begin
  Transform := @TransPerspectiveTransform;
  Transform2x2 := @TransPerspectiveTransform2x2;
  InverseTransform := @TransPerspectiveInverseTransform;
  TransformAffine := @TransPerspectiveTransformAffine;
end;

function TAggTransPerspective.QuadToQuad(Qs, Qd: PAggQuadrilateral): Boolean;
var
  P: TAggTransPerspective;
begin
  Result := False;

  if not QuadToSquare(Qs) then
    Exit;

  P := TAggTransPerspective.Create;
  try
    if not P.SquareToQuad(Qd) then
      Exit;

    Multiply(P);
  finally
    P.Free;
  end;

  Result := True;
end;

function TAggTransPerspective.RectToQuad(X1, Y1, X2, Y2: Double;
  Q: PAggQuadrilateral): Boolean;
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  Result := QuadToQuad(@Src, Q);
end;

function TAggTransPerspective.RectToQuad(Rect: TRectDouble;
  Q: PAggQuadrilateral): Boolean;
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  Result := QuadToQuad(@Src, Q);
end;

function TAggTransPerspective.QuadToRect(Q: PAggQuadrilateral;
  X1, Y1, X2, Y2: Double): Boolean;
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  Result := QuadToQuad(Q, @Dst);
end;

function TAggTransPerspective.QuadToRect(var Q: TAggQuadrilateral;
  Rect: TRectDouble): Boolean;
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  Result := QuadToQuad(@Q, @Dst);
end;

function TAggTransPerspective.QuadToRect(Q: PAggQuadrilateral;
  Rect: TRectDouble): Boolean;
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  Result := QuadToQuad(Q, @Dst);
end;

function TAggTransPerspective.SquareToQuad(Q: PAggQuadrilateral): Boolean;
var
  Delta: TPointDouble;
  Dx1, Dy1, Dx2, Dy2, Den, U, V: Double;
begin
  Delta.X := Q[0] - Q[2] + Q[4] - Q[6];
  Delta.Y := Q[1] - Q[3] + Q[5] - Q[7];

  if (Delta.X = 0.0) and (Delta.Y = 0.0) then
  begin
    // Affine case (parallelogram)
    Sx := Q[2] - Q[0];
    Shy := Q[3] - Q[1];
    W0 := 0.0;
    Shx := Q[4] - Q[2];
    Sy := Q[5] - Q[3];
    W1 := 0.0;
    Tx := Q[0];
    Ty := Q[1];
    W2 := 1.0;
  end
  else
  begin
    Dx1 := Q[2] - Q[4];
    Dy1 := Q[3] - Q[5];
    Dx2 := Q[6] - Q[4];
    Dy2 := Q[7] - Q[5];
    Den := Dx1 * Dy2 - Dx2 * Dy1;

    if Den = 0.0 then
    begin
      // Singular case
      Sx := 0.0;
      Shy := 0.0;
      W0 := 0.0;
      Shx := 0.0;
      Sy := 0.0;
      W1 := 0.0;
      Tx := 0.0;
      Ty := 0.0;
      W2 := 0.0;

      Result := False;

      Exit;
    end;

    // General case
    Den := 1 / Den;
    U := (Delta.X * Dy2 - Delta.Y * Dx2) * Den;
    V := (Delta.Y * Dx1 - Delta.X * Dy1) * Den;

    Sx := Q[2] - Q[0] + U * Q[2];
    Shy := Q[3] - Q[1] + U * Q[3];
    W0 := U;
    Shx := Q[6] - Q[0] + V * Q[6];
    Sy := Q[7] - Q[1] + V * Q[7];
    W1 := V;
    Tx := Q[0];
    Ty := Q[1];
    W2 := 1.0;
  end;

  Result := True;
end;

function TAggTransPerspective.QuadToSquare(Q: PAggQuadrilateral): Boolean;
begin
  Result := False;
  if SquareToQuad(Q) then
  begin
    Invert;
    Result := True;
  end;
end;

procedure TAggTransPerspective.Reset;
begin
  Sx := 1;
  Shy := 0;
  W0 := 0;
  Shx := 0;
  Sy := 1;
  W1 := 0;
  Tx := 0;
  Ty := 0;
  W2 := 1;
end;

function TAggTransPerspective.Invert: Boolean;
var
  D0, D1, D2, D: Double;

  A: TAggTransPerspective;
begin
  D0 := Sy * W2 - W1 * Ty;
  D1 := W0 * Ty - Shy * W2;
  D2 := Shy * W1 - W0 * Sy;
  D := Sx * D0 + Shx * D1 + Tx * D2;

  if D = 0.0 then
  begin
    Sx := 0.0;
    Shy := 0.0;
    W0 := 0.0;
    Shx := 0.0;
    Sy := 0.0;
    W1 := 0.0;
    Tx := 0.0;
    Ty := 0.0;
    W2 := 0.0;

    Result := False;

    Exit;
  end;

  D := 1.0 / D;

  A := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Sx := D * D0;
    Shy := D * D1;
    W0 := D * D2;
    Shx := D * (A.W1 * A.Tx - A.Shx * A.W2);
    Sy := D * (A.Sx * A.W2 - A.W0 * A.Tx);
    W1 := D * (A.W0 * A.Shx - A.Sx * A.W1);
    Tx := D * (A.Shx * A.Ty - A.Sy * A.Tx);
    Ty := D * (A.Shy * A.Tx - A.Sx * A.Ty);
    W2 := D * (A.Sx * A.Sy - A.Shy * A.Shx);
  finally
    A.Free;
  end;

  Result := True;
end;

function TAggTransPerspective.Translate(X, Y: Double): TAggTransPerspective;
begin
  Tx := Tx + X;
  Ty := Ty + Y;

  Result := Self;
end;

function TAggTransPerspective.Rotate(A: Double): TAggTransPerspective;
var
  Tar: TAggTransAffineRotation;
begin
  Tar := TAggTransAffineRotation.Create(A);
  try
    MultiplyAffine(Tar);
  finally
    Tar.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.Scale(S: Double): TAggTransPerspective;
var
  Tas: TAggTransAffineScaling;
begin
  Tas := TAggTransAffineScaling.Create(S);
  try
    MultiplyAffine(Tas);
  finally
    Tas.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.Scale(X, Y: Double): TAggTransPerspective;
var
  Tas: TAggTransAffineScaling;
begin
  Tas := TAggTransAffineScaling.Create(X, Y);
  try
    MultiplyAffine(Tas);
  finally
    Tas.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.Multiply(A: TAggTransPerspective)
  : TAggTransPerspective;
var
  B: TAggTransPerspective;
begin
  B := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Sx := A.Sx * B.Sx + A.Shx * B.Shy + A.Tx * B.W0;
    Shx := A.Sx * B.Shx + A.Shx * B.Sy + A.Tx * B.W1;
    Tx := A.Sx * B.Tx + A.Shx * B.Ty + A.Tx * B.W2;
    Shy := A.Shy * B.Sx + A.Sy * B.Shy + A.Ty * B.W0;
    Sy := A.Shy * B.Shx + A.Sy * B.Sy + A.Ty * B.W1;
    Ty := A.Shy * B.Tx + A.Sy * B.Ty + A.Ty * B.W2;
    W0 := A.W0 * B.Sx + A.W1 * B.Shy + A.W2 * B.W0;
    W1 := A.W0 * B.Shx + A.W1 * B.Sy + A.W2 * B.W1;
    W2 := A.W0 * B.Tx + A.W1 * B.Ty + A.W2 * B.W2;
  finally
    B.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiply(B: TAggTransPerspective)
  : TAggTransPerspective;
var
  A: TAggTransPerspective;
begin
  A := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Sx := A.Sx * B.Sx + A.Shx * B.Shy + A.Tx * B.W0;
    Shx := A.Sx * B.Shx + A.Shx * B.Sy + A.Tx * B.W1;
    Tx := A.Sx * B.Tx + A.Shx * B.Ty + A.Tx * B.W2;
    Shy := A.Shy * B.Sx + A.Sy * B.Shy + A.Ty * B.W0;
    Sy := A.Shy * B.Shx + A.Sy * B.Sy + A.Ty * B.W1;
    Ty := A.Shy * B.Tx + A.Sy * B.Ty + A.Ty * B.W2;
    W0 := A.W0 * B.Sx + A.W1 * B.Shy + A.W2 * B.W0;
    W1 := A.W0 * B.Shx + A.W1 * B.Sy + A.W2 * B.W1;
    W2 := A.W0 * B.Tx + A.W1 * B.Ty + A.W2 * B.W2;
  finally
    A.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.MultiplyInv(M: TAggTransPerspective)
  : TAggTransPerspective;
var
  T: TAggTransPerspective;
begin
  T := TAggTransPerspective.Create(M);
  try
    T.Invert;
    Result := Multiply(T);
  finally
    T.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiplyInv(M: TAggTransPerspective)
  : TAggTransPerspective;
var
  T: TAggTransPerspective;
begin
  T := TAggTransPerspective.Create(M);
  try
    T.Invert;
    T.Multiply(Self);
  finally
    T.Free;
  end;

  // Create(TAggTransPerspective(Self)); //???????

  Result := Self;
end;

function TAggTransPerspective.MultiplyAffine(A: TAggTransAffine)
  : TAggTransPerspective;
var
  B: TAggTransPerspective;
begin
  B := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Sx := A.M0 * B.Sx + A.M2 * B.Shy + A.M4 * B.W0;
    Shx := A.M0 * B.Shx + A.M2 * B.Sy + A.M4 * B.W1;
    Tx := A.M0 * B.Tx + A.M2 * B.Ty + A.M4 * B.W2;
    Shy := A.M1 * B.Sx + A.M3 * B.Shy + A.M5 * B.W0;
    Sy := A.M1 * B.Shx + A.M3 * B.Sy + A.M5 * B.W1;
    Ty := A.M1 * B.Tx + A.M3 * B.Ty + A.M5 * B.W2;
  finally
    B.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiplyAffine(B: TAggTransAffine)
  : TAggTransPerspective;
var
  A: TAggTransPerspective;
begin
  A := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Sx := A.Sx * B.M0 + A.Shx * B.M1;
    Shx := A.Sx * B.M2 + A.Shx * B.M3;
    Tx := A.Sx * B.M4 + A.Shx * B.M5 + A.Tx;
    Shy := A.Shy * B.M0 + A.Sy * B.M1;
    Sy := A.Shy * B.M2 + A.Sy * B.M3;
    Ty := A.Shy * B.M4 + A.Sy * B.M5 + A.Ty;
    W0 := A.W0 * B.M0 + A.W1 * B.M1;
    W1 := A.W0 * B.M2 + A.W1 * B.M3;
    W2 := A.W0 * B.M4 + A.W1 * B.M5 + A.W2;
  finally
    A.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.MultiplyInvAffine(M: TAggTransAffine)
  : TAggTransPerspective;
var
  T: TAggTransAffine;
begin
  T := TAggTransAffine.Create(M.M0, M.M1, M.M2, M.M3, M.M4, M.M5);
  try
    T.Invert;
    Result := MultiplyAffine(T);
  finally
    T.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiplyInvAffine(M: TAggTransAffine)
  : TAggTransPerspective;
var
  T: TAggTransPerspective;
begin
  T := TAggTransPerspective.CreateAffine(M);
  try
    T.Invert;

    T.Multiply(Self);

//    Create(TAggTransPerspective(T)); // ??????
  finally
    T.Free;
  end;

  Result := Self;
end;

procedure TAggTransPerspective.StoreTo(M: PAggQuadrilateral);
begin
  M[0] := Sx;
  M[1] := Shy;
  M[2] := W0;
  M[3] := Shx;
  M[4] := Sy;
  M[5] := W1;
  M[6] := Tx;
  M[7] := Ty;
  M[8] := W2;
end;

function TAggTransPerspective.LoadFrom(M: PAggQuadrilateral)
  : TAggTransPerspective;
begin
  Sx := M[0];
  Shy := M[1];
  W0 := M[2];
  Shx := M[3];
  Sy := M[4];
  W1 := M[5];
  Tx := M[6];
  Ty := M[7];
  W2 := M[8];
end;

function TAggTransPerspective.FromAffine(A: TAggTransAffine)
  : TAggTransPerspective;
begin
  Sx := A.M0;
  Shy := A.M1;
  W0 := 0;
  Shx := A.M2;
  Sy := A.M3;
  W1 := 0;
  Tx := A.M4;
  Ty := A.M5;
  W2 := 1;

  Result := Self;
end;

function TAggTransPerspective.Determinant: Double;
begin
  Result := Sx * (Sy * W2 - Ty * W1) + Shx * (Ty * W0 - Shy * W2) + Tx *
    (Shy * W1 - Sy * W0);
end;

function TAggTransPerspective.DeterminantReciprocal: Double;
begin
  Result := 1.0 / Determinant;
end;

function TAggTransPerspective.IsValid(Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := (Abs(Sx) > Epsilon) and (Abs(Sy) > Epsilon) and (Abs(W2) > Epsilon);
end;

function TAggTransPerspective.IsIdentity
  (Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(Sx, 1.0, Epsilon) and
    IsEqualEpsilon(Shy, 0.0, Epsilon) and
    IsEqualEpsilon(W0, 0.0, Epsilon) and
    IsEqualEpsilon(Shx, 0.0, Epsilon) and
    IsEqualEpsilon(Sy, 1.0, Epsilon) and
    IsEqualEpsilon(W1, 0.0, Epsilon) and
    IsEqualEpsilon(Tx, 0.0, Epsilon) and
    IsEqualEpsilon(Ty, 0.0, Epsilon) and
    IsEqualEpsilon(W2, 1.0, Epsilon);
end;

function TAggTransPerspective.IsEqual(M: TAggTransPerspective;
  Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(Sx, M.Sx, Epsilon) and
    IsEqualEpsilon(Shy, M.Shy, Epsilon) and
    IsEqualEpsilon(W0, M.W0, Epsilon) and
    IsEqualEpsilon(Shx, M.Shx, Epsilon) and
    IsEqualEpsilon(Sy, M.Sy, Epsilon) and
    IsEqualEpsilon(W1, M.W1, Epsilon) and
    IsEqualEpsilon(Tx, M.Tx, Epsilon) and
    IsEqualEpsilon(Ty, M.Ty, Epsilon) and
    IsEqualEpsilon(W2, M.W2, Epsilon);
end;

function TAggTransPerspective.Scale: Double;
var
  X, Y: Double;
const
  CSqrt2Half: Double = 0.70710678118654752440084436210485;
begin
  X := CSqrt2Half * Sx + CSqrt2Half * Shx;
  Y := CSqrt2Half * Shy + CSqrt2Half * Sy;

  Result := Hypot(X, Y);
end;

function TAggTransPerspective.Rotation: Double;
var
  X1, Y1, X2, Y2: Double;
begin
  X1 := 0.0;
  Y1 := 0.0;
  X2 := 1.0;
  Y2 := 0.0;

  TransPerspectiveTransform(Self, @X1, @Y1);
  TransPerspectiveTransform(Self, @X2, @Y2);

  Result := ArcTan2(Y2 - Y1, X2 - X1);
end;

procedure TAggTransPerspective.Translation(Dx, Dy: PDouble);
begin
  Dx^ := Tx;
  Dy^ := Ty;
end;

procedure TAggTransPerspective.Scaling(X, Y: PDouble);
var
  X1, Y1, X2, Y2: Double;
  T : TAggTransPerspective;
  Tar: TAggTransAffineRotation;
begin
  X1 := 0.0;
  Y1 := 0.0;
  X2 := 1.0;
  Y2 := 1.0;

  T := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Tar := TAggTransAffineRotation.Create(-Rotation);
    try
      T.MultiplyAffine(Tar);
    finally
      Tar.Free;
    end;

    T.Transform(T, @X1, @Y1);
    T.Transform(T, @X2, @Y2);
  finally
    T.Free;
  end;

  X^ := X2 - X1;
  Y^ := Y2 - Y1;
end;

procedure TAggTransPerspective.ScalingAbs(X, Y: PDouble);
begin
  X^ := Hypot(Sx, Shx);
  Y^ := Hypot(Shy, Sy);
end;

function TAggTransPerspective.GetBegin(X, Y, Step: Double): TAggIteratorXRecord;
begin
  Result.Initialize(X, Y, Step, Self);
end;

end.
