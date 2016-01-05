unit AggTransAffine;

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
// Affine transformation classes.                                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Affine transformation are linear transformations in Cartesian coordinates //
//  (strictly speaking not only in Cartesian, but for the beginning we will   //
//  think so). They are rotation, scaling, translation and skewing.           //
//  After any affine transformation a line segment remains a line segment     //
//  and it will never become a curve.                                         //
//                                                                            //
//  There will be no math about matrix calculations, since it has been        //
//  described many times. Ask yourself a very simple question:                //
//  "why do we need to understand and use some matrix stuff instead of just   //
//  rotating, scaling and so on". The answers are:                            //
//                                                                            //
//  1. Any combination of transformations can be done by only 4               //
//  multiplications and 4 additions in floating point.                        //
//  2. One matrix transformation is equivalent to the number of consecutive   //
//  discrete transformations, i.e. the matrix "accumulates" all               //
//  transformations in the order of their settings. Suppose we have 4         //
//  transformations:                                                          //
//    * rotate by 30 degrees,                                                 //
//    * scale X to 2,                                                         //
//    * scale Y to 1.5,                                                       //
//    * move to (100, 100).                                                   //
//  The result will depend on the order of these transformations,             //
//  and the advantage of matrix is that the sequence of discret calls:        //
//  rotate(30), scaleX(2.0), scaleY(1.5), move(100,100)                       //
//  will have exactly the same result as the following matrix                 //
//  transformations:                                                          //
//                                                                            //
//  m : TAffineMatrix;                                                        //
//  m := m * RotateMatrix(30);                                                //
//  m := m * ScaleXMatrix(2.0);                                               //
//  m := m * ScaleYMatrix(1.5);                                               //
//  m := m * MoveMatrix(100,100);                                             //
//                                                                            //
//  m.TransformMyPointAtLast(x, y);                                           //
//                                                                            //
//  What is the good of it? In real life we will set-up the matrix only once  //
//  and then transform many points, let alone the convenience to set any      //
//  combination of transformations.                                           //
//                                                                            //
//  So, how to use it? Very easy - literally as it's shown above. Not quite,  //
//  let us write a correct example:                                           //
//                                                                            //
//  m : TAggTransAffine;                                                      //
//  m := m * TAggTransAffineRotation(30.0 * Pi / 180.0);                      //
//  m := m * TAggTransAffineScaling(2.0, 1.5);                                //
//  m := m * TAggTransAffineTranslation(100.0, 100.0);                        //
//  m.Transform(@X, @Y);                                                      //
//                                                                            //
//  The affine matrix is all you need to perform any linear transformation,   //
//  but all transformations have origin point (0, 0). It means that we need   //
//  to use 2 translations if we want to rotate someting around (100, 100):    //
//                                                                            //
//  m := m * TAggTransAffineTranslation(-100, -100); // move to (0,0)         //
//  m := m * TAggTransAffineRotation(30 * Pi / 180); // rotate                //
//  m := m * TAggTransAffineTranslation(100, 100);   // move back to origin   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  Math,
  AggMath,
  AggBasics;

const
  CAggAffineEpsilon = 1E-14; // About of precision of doubles

type
  TAggTransAffine = class;

  TAggProcTransform = procedure(This: TAggTransAffine; X, Y: PDouble);

  PAggParallelogram = ^TAggParallelogram;
  TAggParallelogram = array [0..5] of Double;

  TAggTransAffine = class
  protected
    FData: TAggParallelogram;
  public
    Transform, Transform2x2, InverseTransform: TAggProcTransform;

    // Construct an identity matrix - it does not transform anything
    constructor Create; overload; virtual;

    // Construct a custom matrix. Usually used in derived classes
    constructor Create(V0, V1, V2, V3, V4, V5: Double); overload;

    // Construct a matrix to transform a parallelogram to another one
    constructor Create(Rect, Parl: PAggParallelogram); overload;

    // Construct a matrix to transform a rectangle to a parallelogram
    constructor Create(X1, Y1, X2, Y2: Double; Parl: PAggParallelogram);
      overload;
    constructor Create(Rect: TRectDouble; Parl: PAggParallelogram);
      overload;

    // Construct a matrix to transform a parallelogram to a rectangle
    constructor Create(Parl: PAggParallelogram; X1, Y1, X2, Y2: Double);
      overload;
    constructor Create(Parl: PAggParallelogram; Rect: TRectDouble);
      overload;

    // Construct a matrix with different transform function
    constructor Create(Tr: TAggProcTransform); overload;

    // Calculate a matrix to transform a parallelogram to another one.
    // src and dst are pointers to arrays of three points
    // (double[6], x,y,...) that identify three corners of the
    // parallelograms assuming implicit fourth points.
    // There are also transformations rectangtle to parallelogram and
    // parellelogram to rectangle
    procedure ParlToParl(Src, Dst: PAggParallelogram);
    procedure RectToParl(Rect: TRectDouble; Parl: PAggParallelogram); overload;
    procedure RectToParl(X1, Y1, X2, Y2: Double; Parl: PAggParallelogram);
      overload;
    procedure ParlToRect(Parl: PAggParallelogram; Rect: TRectDouble); overload;
    procedure ParlToRect(Parl: PAggParallelogram; X1, Y1, X2, Y2: Double);
      overload;

    // Reset - actually load an identity matrix
    procedure Reset; virtual;

    // Initialize Transforms
    procedure InitializeTransforms;

    // Multiply matrix to another one
    procedure Multiply(M: TAggTransAffine);

    // Multiply "m" to "this" and assign the result to "this"
    procedure PreMultiply(M: TAggTransAffine);

    // Multiply matrix to inverse of another one
    procedure MultiplyInv(M: TAggTransAffine);

    // Multiply inverse of "m" to "this" and assign the result to "this"
    procedure PreMultiplyInv(M: TAggTransAffine);

    // Invert matrix. Do not try to invert degenerate matrices,
    // there's no check for validity. If you set scale to 0 and
    // then try to invert matrix, expect unpredictable result.
    procedure Invert;

    // Mirroring around X
    procedure FlipX;

    // Mirroring around Y
    procedure FlipY;

    // ------------------------------------------- Load/Store
    // Store matrix to an array [6] of double
    procedure StoreTo(M: PAggParallelogram); overload;
    procedure StoreTo(out M: TAggParallelogram); overload;

    // Load matrix from an array [6] of double
    procedure LoadFrom(M: PAggParallelogram); overload;
    procedure LoadFrom(var M: TAggParallelogram); overload;

    // -------------------------------------------- Transformations
    // Direct transformation x and y
    // see: transform : TAggProcTransform; above

    // Direct transformation x and y, 2x2 matrix only, no translation
    // procedure Transform2x2(x ,y : PDouble );

    // Inverse transformation x and y. It works sLower than the
    // direct transformation, so if the performance is critical
    // it's better to invert() the matrix and then use transform()
    // procedure InverseTransform(x ,y : PDouble );

    // -------------------------------------------- Auxiliary
    // Calculate the determinant of matrix
    function Determinant: Double;

    // Get the average scale (by X and Y).
    // Basically used to calculate the ApproximationScale when
    // decomposinting curves into line segments.
    function GetScale: Double; overload;

    // Check to see if it's an identity matrix
    function IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;

    // Check to see if two matrices are equal
    function IsEqual(M: TAggTransAffine; Epsilon: Double = CAggAffineEpsilon): Boolean;

    // Determine the major parameters. Use carefully considering degenerate matrices
    function GetRotation: Double;
    procedure GetTranslation(out Dx, Dy: Double);
    procedure GetScaling(out Sx, Sy: Double);
    procedure GetScalingAbs(out Sx, Sy: Double);

    // Trans Affine Assignations
    procedure Assign(From: TAggTransAffine);
    procedure AssignAll(From: TAggTransAffine);

    // Direct transformations operations
    function Translate(X, Y: Double): TAggTransAffine; overload;
    function Translate(Value: TPointDouble): TAggTransAffine; overload;
    function Rotate(A: Double): TAggTransAffine;
    function Scale(S: Double): TAggTransAffine; overload;
    function Scale(X, Y: Double): TAggTransAffine; overload;
    function Scale(Value: TPointDouble): TAggTransAffine; overload;
    function Skew(X, Y: Double): TAggTransAffine; overload;
    function Skew(Value: TPointDouble): TAggTransAffine; overload;

    property M0: Double read FData[0];
    property M1: Double read FData[1];
    property M2: Double read FData[2];
    property M3: Double read FData[3];
    property M4: Double read FData[4];
    property M5: Double read FData[5];
  end;

  // Rotation matrix.
  TAggTransAffineRotation = class(TAggTransAffine)
  public
    constructor Create(Angle: Double);
  end;

  // Scaling matrix. ScaleX, ScaleY - scale coefficients by X and Y respectively
  TAggTransAffineScaling = class(TAggTransAffine)
  public
    constructor Create(Scale: Double); overload;
    constructor Create(ScaleX, ScaleY: Double); overload;
  end;

  // Translation matrix
  TAggTransAffineTranslation = class(TAggTransAffine)
  public
    constructor Create(Tx, Ty: Double);
  end;

  // Skewing (shear) matrix
  TAggTransAffineSkewing = class(TAggTransAffine)
  public
    constructor Create(Sx, Sy: Double);
  end;

  // Rotate, Scale and Translate, associating 0...dist with line segment
  // x1,y1,x2,y2
  TAggTransAffineLineSegment = class(TAggTransAffine)
  public
    constructor Create(X1, Y1, X2, Y2, Dist: Double);
  end;

  // Reflection matrix. Reflect coordinates across the line through
  // the origin containing the unit vector (ux, uy).
  // Contributed by John Horigan
  TAggTransAffineReflectionUnit = class(TAggTransAffine)
  public
    constructor Create(Ux, Uy: Double);
  end;

  // Reflection matrix. Reflect coordinates across the line through
  // the origin at the angle a or containing the non-unit vector (x, y).
  // Contributed by John Horigan
  TAggTransAffineReflection = class(TAggTransAffineReflectionUnit)
  public
    constructor Create(A: Double); overload;
    constructor Create(X, Y: Double); overload;
  end;

function IsEqualEpsilon(V1, V2, Epsilon: Double): Boolean;

procedure TransAffineRotation(Matrix: TAggTransAffine; Rotation: Double);
procedure TransAffineScaling(Matrix: TAggTransAffine; Scaling: Double); overload;
procedure TransAffineScaling(Matrix: TAggTransAffine; ScalingX,
  ScalingY: Double); overload;
procedure TransAffineSkewing(Matrix: TAggTransAffine; SkewingX,
  SkewingY: Double);
procedure TransAffineTranslation(Matrix: TAggTransAffine; TranslationX,
  TranslationY: Double);

implementation


function IsEqualEpsilon;
begin
  Result := Abs(V1 - V2) < Epsilon;
end;

procedure TransAffineRotation(Matrix: TAggTransAffine; Rotation: Double);
var
  TransAffine: TAggTransAffineRotation;
begin
  TransAffine := TAggTransAffineRotation.Create(Rotation);
  try
    Matrix.Multiply(TransAffine);
  finally
    TransAffine.Free;
  end;
end;

procedure TransAffineScaling(Matrix: TAggTransAffine; Scaling: Double);
var
  TransAffine: TAggTransAffineScaling;
begin
  TransAffine := TAggTransAffineScaling.Create(Scaling);
  try
    Matrix.Multiply(TransAffine);
  finally
    TransAffine.Free;
  end;
end;

procedure TransAffineScaling(Matrix: TAggTransAffine; ScalingX,
  ScalingY: Double);
var
  TransAffine: TAggTransAffineScaling;
begin
  TransAffine := TAggTransAffineScaling.Create(ScalingX, ScalingY);
  try
    Matrix.Multiply(TransAffine);
  finally
    TransAffine.Free;
  end;
end;

procedure TransAffineSkewing(Matrix: TAggTransAffine; SkewingX,
  SkewingY: Double);
var
  TransAffine: TAggTransAffineSkewing;
begin
  TransAffine := TAggTransAffineSkewing.Create(SkewingX, SkewingY);
  try
    Matrix.Multiply(TransAffine);
  finally
    TransAffine.Free;
  end;
end;

procedure TransAffineTranslation(Matrix: TAggTransAffine; TranslationX,
  TranslationY: Double);
var
  TransAffine: TAggTransAffineTranslation;
begin
  TransAffine := TAggTransAffineTranslation.Create(TranslationX, TranslationY);
  try
    Matrix.Multiply(TransAffine);
  finally
    TransAffine.Free;
  end;
end;

procedure TransAffineTransform(This: TAggTransAffine; X, Y: PDouble);
var
  Tx: Double;
begin
  Tx := X^;
  X^ := Tx * This.FData[0] + Y^ * This.FData[2] + This.FData[4];
  Y^ := Tx * This.FData[1] + Y^ * This.FData[3] + This.FData[5];
end;

procedure TransAffineTransform2x2(This: TAggTransAffine; X, Y: PDouble);
var
  Tx: Double;
begin
  Tx := X^;
  X^ := Tx * This.FData[0] + Y^ * This.FData[2];
  Y^ := Tx * This.FData[1] + Y^ * This.FData[3];
end;

procedure TransAffineInverseTransform(This: TAggTransAffine;
  X, Y: PDouble);
var
  D, A, B: Double;
begin
  D := This.Determinant;
  A := (X^ - This.FData[4]) * D;
  B := (Y^ - This.FData[5]) * D;

  X^ := A * This.FData[3] - B * This.FData[2];
  Y^ := B * This.FData[0] - A * This.FData[1];
end;


{ TAggTransAffine }

constructor TAggTransAffine.Create;
begin
  FData[0] := 1;
  FData[1] := 0;
  FData[2] := 0;
  FData[3] := 1;
  FData[4] := 0;
  FData[5] := 0;

  InitializeTransforms;
end;

constructor TAggTransAffine.Create(V0, V1, V2, V3, V4, V5: Double);
begin
  FData[0] := V0;
  FData[1] := V1;
  FData[2] := V2;
  FData[3] := V3;
  FData[4] := V4;
  FData[5] := V5;

  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Rect, Parl: PAggParallelogram);
begin
  ParlToParl(Rect, Parl);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(X1, Y1, X2, Y2: Double; Parl: PAggParallelogram);
begin
  RectToParl(X1, Y1, X2, Y2, Parl);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Rect: TRectDouble; Parl: PAggParallelogram);
begin
  RectToParl(Rect, Parl);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Parl: PAggParallelogram; X1, Y1, X2, Y2: Double);
begin
  ParlToRect(Parl, X1, Y1, X2, Y2);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Parl: PAggParallelogram; Rect: TRectDouble);
begin
  ParlToRect(Parl, Rect);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Tr: TAggProcTransform);
begin
  FData[0] := 1;
  FData[1] := 0;
  FData[2] := 0;
  FData[3] := 1;
  FData[4] := 0;
  FData[5] := 0;

  Transform := Tr;
  Transform2x2 := @TransAffineTransform2x2;
  InverseTransform := @TransAffineInverseTransform;
end;

procedure TAggTransAffine.InitializeTransforms;
begin
  Transform := @TransAffineTransform;
  Transform2x2 := @TransAffineTransform2x2;
  InverseTransform := @TransAffineInverseTransform;
end;

procedure TAggTransAffine.ParlToParl(Src, Dst: PAggParallelogram);
var
  M: TAggTransAffine;
begin
  FData[0] := Src[2] - Src[0];
  FData[1] := Src[3] - Src[1];
  FData[2] := Src[4] - Src[0];
  FData[3] := Src[5] - Src[1];
  FData[4] := Src[0];
  FData[5] := Src[1];

  Invert;

  M := TAggTransAffine.Create(Dst[2] - Dst[0], Dst[3] - Dst[1], Dst[4] - Dst[0],
    Dst[5] - Dst[1], Dst[0], Dst[1]);
  try
    Multiply(M);
  finally
    M.Free;
  end;
end;

procedure TAggTransAffine.RectToParl(X1, Y1, X2, Y2: Double;
  Parl: PAggParallelogram);
var
  Src: TAggParallelogram;
begin
  Src[0] := X1;
  Src[1] := Y1;
  Src[2] := X2;
  Src[3] := Y1;
  Src[4] := X2;
  Src[5] := Y2;

  ParlToParl(@Src, Parl);
end;

procedure TAggTransAffine.ParlToRect(Parl: PAggParallelogram;
  Rect: TRectDouble);
var
  Src: TAggParallelogram;
begin
  Src[0] := Rect.X1;
  Src[1] := Rect.Y1;
  Src[2] := Rect.X2;
  Src[3] := Rect.Y1;
  Src[4] := Rect.X2;
  Src[5] := Rect.Y2;

  ParlToParl(@Src, Parl);
end;

procedure TAggTransAffine.ParlToRect(Parl: PAggParallelogram; X1, Y1, X2,
  Y2: Double);
var
  Dst: TAggParallelogram;
begin
  Dst[0] := X1;
  Dst[1] := Y1;
  Dst[2] := X2;
  Dst[3] := Y1;
  Dst[4] := X2;
  Dst[5] := Y2;

  ParlToParl(Parl, @Dst);
end;

procedure TAggTransAffine.RectToParl(Rect: TRectDouble;
  Parl: PAggParallelogram);
var
  Dst: TAggParallelogram;
begin
  Dst[0] := Rect.X1;
  Dst[1] := Rect.Y1;
  Dst[2] := Rect.X2;
  Dst[3] := Rect.Y1;
  Dst[4] := Rect.X2;
  Dst[5] := Rect.Y2;

  ParlToParl(Parl, @Dst);
end;

procedure TAggTransAffine.Reset;
begin
  FData[0] := 1;
  FData[1] := 0;
  FData[2] := 0;
  FData[3] := 1;
  FData[4] := 0;
  FData[5] := 0;
end;

procedure TAggTransAffine.Multiply(M: TAggTransAffine);
var
  T0, T2, T4: Double;
begin
  T0 := FData[0] * M.FData[0] + FData[1] * M.FData[2];
  T2 := FData[2] * M.FData[0] + FData[3] * M.FData[2];
  T4 := FData[4] * M.FData[0] + FData[5] * M.FData[2] + M.FData[4];
  FData[1] := FData[0] * M.FData[1] + FData[1] * M.FData[3];
  FData[3] := FData[2] * M.FData[1] + FData[3] * M.FData[3];
  FData[5] := FData[4] * M.FData[1] + FData[5] * M.FData[3] + M.FData[5];
  FData[0] := T0;
  FData[2] := T2;
  FData[4] := T4;
end;

procedure TAggTransAffine.PreMultiply(M: TAggTransAffine);
var
  T: TAggTransAffine;
begin
  Transform := @M.Transform;
  Transform2x2 := @M.Transform2x2;
  InverseTransform := @M.InverseTransform;
  Multiply(M);
end;

procedure TAggTransAffine.MultiplyInv(M: TAggTransAffine);
var
  T: TAggTransAffine;
begin
  T.AssignAll(M);
  T.Invert;

  Multiply(@T);
end;

procedure TAggTransAffine.PreMultiplyInv(M: TAggTransAffine);
var
  T: TAggTransAffine;
begin
  T.AssignAll(M);

  T.Invert;
  T.Multiply(Self);

  Assign(@T);
end;

procedure TAggTransAffine.Invert;
var
  D, T0, T4: Double;
begin
  D := Determinant;

  T0 := FData[3] * D;
  FData[3] := FData[0] * D;
  FData[1] := -FData[1] * D;
  FData[2] := -FData[2] * D;

  T4 := -FData[4] * T0 - FData[5] * FData[2];
  FData[5] := -FData[4] * FData[1] - FData[5] * FData[3];

  FData[0] := T0;
  FData[4] := T4;
end;

procedure TAggTransAffine.FlipX;
begin
  FData[0] := -FData[0];
  FData[1] := -FData[1];
  FData[4] := -FData[4];
end;

procedure TAggTransAffine.FlipY;
begin
  FData[2] := -FData[2];
  FData[3] := -FData[3];
  FData[5] := -FData[5];
end;

procedure TAggTransAffine.StoreTo(M: PAggParallelogram);
begin
  M^ := FData;
end;

procedure TAggTransAffine.StoreTo(out M: TAggParallelogram);
begin
  M := FData;
end;

procedure TAggTransAffine.LoadFrom(M: PAggParallelogram);
begin
  FData := M^;
end;

procedure TAggTransAffine.LoadFrom(var M: TAggParallelogram);
begin
  FData := M;
end;

function TAggTransAffine.Determinant: Double;
begin
  try
    Result := 1 / (FData[0] * FData[3] - FData[1] * FData[2]);
  except
    Result := 0;
  end;
end;

function TAggTransAffine.GetScale: Double;
var
  X, Y: Double;
const
  CSqrt2Half: Double = 0.70710678118654752440084436210485;
begin
  X := CSqrt2Half * FData[0] + CSqrt2Half * FData[2];
  Y := CSqrt2Half * FData[1] + CSqrt2Half * FData[3];

  Result := Sqrt(X * X + Y * Y);
end;

function TAggTransAffine.IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(FData[0], 1, Epsilon) and
    IsEqualEpsilon(FData[1], 0, Epsilon) and
    IsEqualEpsilon(FData[2], 0, Epsilon) and
    IsEqualEpsilon(FData[3], 1, Epsilon) and
    IsEqualEpsilon(FData[4], 0, Epsilon) and
    IsEqualEpsilon(FData[5], 0, Epsilon);
end;

function TAggTransAffine.IsEqual(M: TAggTransAffine;
  Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(FData[0], M.FData[0], Epsilon) and
    IsEqualEpsilon(FData[1], M.FData[1], Epsilon) and
    IsEqualEpsilon(FData[2], M.FData[2], Epsilon) and
    IsEqualEpsilon(FData[3], M.FData[3], Epsilon) and
    IsEqualEpsilon(FData[4], M.FData[4], Epsilon) and
    IsEqualEpsilon(FData[5], M.FData[5], Epsilon);
end;

function TAggTransAffine.GetRotation: Double;
var
  X1, Y1, X2, Y2: Double;
begin
  X1 := 0;
  Y1 := 0;
  X2 := 1;
  Y2 := 0;

  Transform(Self, @X1, @Y1);
  Transform(Self, @X2, @Y2);

  Result := ArcTan2(Y2 - Y1, X2 - X1);
end;

procedure TAggTransAffine.GetTranslation(out Dx, Dy: Double);
begin
  Dx := 0;
  Dy := 0;

  Transform(Self, @Dx, @Dy);
end;

procedure TAggTransAffine.GetScaling(out Sx, Sy: Double);
var
  T: TAggTransAffineRotation;
  X1, Y1, X2, Y2: Double;
begin
  X1 := 0;
  Y1 := 0;
  X2 := 1;
  Y2 := 1;

  TAggTransAffine(T) := Self;

  T := TAggTransAffineRotation.Create(-GetRotation);
  try
    T.Transform(Self, @X1, @Y1);
    T.Transform(Self, @X2, @Y2);
  finally
    T.Free;
  end;

  Sx := X2 - X1;
  Sy := Y2 - Y1;
end;

procedure TAggTransAffine.GetScalingAbs(out Sx, Sy: Double);
begin
  Sx := Hypot(FData[0], FData[2]);
  Sy := Hypot(FData[1], FData[3]);
end;

procedure TAggTransAffine.Assign(From: TAggTransAffine);
begin
  FData := From.FData;
end;

procedure TAggTransAffine.AssignAll(From: TAggTransAffine);
begin
  FData := From.FData;

  Transform := @From.Transform;
  Transform2x2 := @From.Transform2x2;
  InverseTransform := @From.InverseTransform;
end;

function TAggTransAffine.Translate(X, Y: Double): TAggTransAffine;
begin
  FData[4] := FData[4] + X;
  FData[5] := FData[5] + Y;

  Result := Self;
end;

function TAggTransAffine.Translate(Value: TPointDouble): TAggTransAffine;
begin
  FData[4] := FData[4] + Value.X;
  FData[5] := FData[5] + Value.Y;

  Result := Self;
end;

function TAggTransAffine.Rotate(A: Double): TAggTransAffine;
var
  Ca, Sa, Temp: Double;
begin
  SinCos(A, Sa, Ca);

  Temp := FData[0] * Ca - FData[1] * Sa;
  FData[1] := FData[0] * Sa + FData[1] * Ca;
  FData[0] := Temp;

  Temp := FData[2] * Ca - FData[3] * Sa;
  FData[3] := FData[2] * Sa + FData[3] * Ca;
  FData[2] := Temp;

  Temp := FData[4] * Ca - FData[5] * Sa;
  FData[5] := FData[4] * Sa + FData[5] * Ca;
  FData[4] := Temp;

  Result := Self;
end;

function TAggTransAffine.Scale(S: Double): TAggTransAffine;
begin
  FData[0] := FData[0] * S;
  FData[1] := FData[1] * S;
  FData[2] := FData[2] * S;
  FData[3] := FData[3] * S;
  FData[4] := FData[4] * S;
  FData[5] := FData[5] * S;

  Result := Self;
end;

function TAggTransAffine.Scale(X, Y: Double): TAggTransAffine;
begin
  FData[0] := FData[0] * X;
  FData[2] := FData[2] * X;
  FData[4] := FData[4] * X;
  FData[1] := FData[1] * Y;
  FData[3] := FData[3] * Y;
  FData[5] := FData[5] * Y;

  Result := Self;
end;

function TAggTransAffine.Scale(Value: TPointDouble): TAggTransAffine;
begin
  FData[0] := FData[0] * Value.X;
  FData[2] := FData[2] * Value.X;
  FData[4] := FData[4] * Value.X;
  FData[1] := FData[1] * Value.Y;
  FData[3] := FData[3] * Value.Y;
  FData[5] := FData[5] * Value.Y;

  Result := Self;
end;

function TAggTransAffine.Skew(X, Y: Double): TAggTransAffine;
var
  Ty, Tx, Temp: Double;
begin
  Ty := Tan(Y);
  Tx := Tan(X);

  Temp := FData[0] + FData[1] * Tx;
  FData[1] := FData[0] * Ty + FData[1];
  FData[0] := Temp;

  Temp := FData[2] + FData[3] * Tx;
  FData[3] := FData[2] * Ty + FData[3];
  FData[2] := Temp;

  Temp := FData[4] + FData[5] * Tx;
  FData[5] := FData[4] * Ty + FData[5];
  FData[4] := Temp;

  Result := Self;
end;

function TAggTransAffine.Skew(Value: TPointDouble): TAggTransAffine;
begin
  Result := Skew(Value.X, Value.Y);
end;


{ TAggTransAffineRotation }

constructor TAggTransAffineRotation.Create(Angle: Double);
var
  Sn, Cn: Double;
begin
  SinCos(Angle, Sn, Cn);
  inherited Create(Cn, Sn, -Sn, Cn, 0, 0);
end;


{ TAggTransAffineScaling }

constructor TAggTransAffineScaling.Create(ScaleX, ScaleY: Double);
begin
  inherited Create(ScaleX, 0, 0, ScaleY, 0, 0);
end;

constructor TAggTransAffineScaling.Create(Scale: Double);
begin
  inherited Create(Scale, 0, 0, Scale, 0, 0);
end;


{ TAggTransAffineTranslation }

constructor TAggTransAffineTranslation.Create(Tx, Ty: Double);
begin
  inherited Create(1, 0, 0, 1, Tx, Ty);
end;

{ TAggTransAffineSkewing }

constructor TAggTransAffineSkewing.Create(Sx, Sy: Double);
begin
  inherited Create(1, Tan(Sy), Tan(Sx), 1, 0, 0);
end;


{ TAggTransAffineLineSegment }

constructor TAggTransAffineLineSegment.Create(X1, Y1, X2, Y2, Dist: Double);
var
  Delta: TPointDouble;
begin
  Delta := PointDouble(X2 - X1, Y2 - Y1);

  if Dist > 0 then
    Scale(Hypot(Delta.X, Delta.Y) / Dist);

  Rotate(ArcTan2(Delta.Y, Delta.X));
  Translate(X1, Y1);
end;


{ TAggTransAffineReflectionUnit }

constructor TAggTransAffineReflectionUnit.Create(Ux, Uy: Double);
begin
  inherited Create(2 * Sqr(Ux) - 1, 2 * Ux * Uy, 2 * Ux * Uy, 2 * Sqr(Uy) - 1,
    0, 0);
end;


{ TAggTransAffineReflection }

constructor TAggTransAffineReflection.Create(A: Double);
var
  Sn, Cn: Double;
begin
  SinCos(A, Sn, Cn);
  inherited Create(Cn, Sn);
end;

constructor TAggTransAffineReflection.Create(X, Y: Double);
var
  Nx, Ny: Double;
  Tmp: Double;
begin
  if (X = 0) and (Y = 0) then
  begin
    X := 0;
    Y := 0;
  end
  else
  begin
    Tmp := 1 / Hypot(X, Y);
    Nx := X * Tmp;
    Ny := Y * Tmp;
  end;

  inherited Create(Nx, Ny);
end;

end.
