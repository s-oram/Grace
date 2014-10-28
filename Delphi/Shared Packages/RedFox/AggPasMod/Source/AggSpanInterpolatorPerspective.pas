unit AggSpanInterpolatorPerspective;

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
  AggTransPerspective,
  AggSpanInterpolatorLinear,
  AggDdaLine;

type
  TAggSpanInterpolatorPerspectiveExact = class(TAggSpanInterpolator)
  private
    FTransDir, FTransInv: TAggTransPerspective23;
    FIterator: AggTransPerspective.TAggIteratorX23;
    FScaleX, FScaleY: TAggDda2LineInterpolator;
  public
    constructor Create(SS: Cardinal = 8); overload;

    // Arbitrary quadrangle transformations
    constructor Create(Src, Dst: PQuadDouble; SS: Cardinal = 8); overload;

    // Direct transformations
    constructor Create(X1, Y1, X2, Y2: Double; Quad: PQuadDouble;
      SS: Cardinal = 8); overload;
    constructor Create(Rect: TRectDouble; Quad: PQuadDouble;
      SS: Cardinal = 8); overload;

    // Reverse transformations
    constructor Create(Quad: PQuadDouble; X1, Y1, X2, Y2: Double;
      SS: Cardinal = 8); overload;
    constructor Create(Quad: PQuadDouble; Rect: TRectDouble;
      SS: Cardinal = 8); overload;

    destructor Destroy; override;

    // Set the transformations using two arbitrary quadrangles.
    procedure QuadToQuad(Src, Dst: PQuadDouble);

    // Set the direct transformations, i.e., rectangle -> quadrangle
    procedure RectToQuad(X1, Y1, X2, Y2: Double; Quad: PQuadDouble); overload;
    procedure RectToQuad(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Set the reverse transformations, i.e., quadrangle -> rectangle
    procedure QuadToRect(Quad: PQuadDouble; X1, Y1, X2, Y2: Double); overload;
    procedure QuadToRect(Quad: PQuadDouble; Rect: TRectDouble); overload;

    // Check if the equations were solved successfully
    function IsValid: Boolean;

    // Span interpolator interface
    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure Resynchronize(Xe, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;

    procedure LocalScale(X, Y: PInteger); override;

    procedure Transform(X, Y: PDouble);
  end;

  TAggSpanInterpolatorPerspectiveLerp = class(TAggSpanInterpolator)
  private
    FTransDir, FTransInv: TAggTransPerspective23;

    FCoordX, FCoordY, FScaleX, FScaleY: TAggDda2LineInterpolator;
    function GetIsValid: Boolean;
  public
    constructor Create(SS: Cardinal = 8); overload;

    // Arbitrary quadrangle transformations
    constructor Create(Src, Dst: PQuadDouble; SS: Cardinal = 8); overload;

    // Direct transformations
    constructor Create(X1, Y1, X2, Y2: Double; Quad: PQuadDouble;
      SS: Cardinal = 8); overload;

    // Reverse transformations
    constructor Create(Quad: PQuadDouble; X1, Y1, X2, Y2: Double;
      SS: Cardinal = 8); overload;
    constructor Create(Quad: PQuadDouble; Rect: TRectDouble;
      SS: Cardinal = 8); overload;

    destructor Destroy; override;

    // Set the transformations using two arbitrary quadrangles.
    procedure QuadToQuad(Src, Dst: PQuadDouble);

    // Set the direct transformations, i.e., rectangle -> quadrangle
    procedure RectToQuad(X1, Y1, X2, Y2: Double; Quad: PQuadDouble); overload;
    procedure RectToQuad(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Set the reverse transformations, i.e., quadrangle -> rectangle
    procedure QuadToRect(Quad: PQuadDouble; X1, Y1, X2, Y2: Double); overload;
    procedure QuadToRect(Quad: PQuadDouble; Rect: TRectDouble); overload;

    // Span interpolator interface
    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure Resynchronize(Xe, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;

    procedure LocalScale(X, Y: PInteger); override;

    procedure Transform(X, Y: PDouble);

    // Check if the equations were solved successfully
    property IsValid: Boolean read GetIsValid;
  end;


implementation


{ TAggSpanInterpolatorPerspectiveExact }

constructor TAggSpanInterpolatorPerspectiveExact.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Src, Dst: PQuadDouble;
  SS: Cardinal = 8);
begin
  Create(SS);
  QuadToQuad(Src, Dst);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(X1, Y1, X2, Y2: Double;
  Quad: PQuadDouble; SS: Cardinal = 8);
begin
  Create(SS);
  RectToQuad(X1, Y1, X2, Y2, Quad);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Rect: TRectDouble;
  Quad: PQuadDouble; SS: Cardinal = 8);
begin
  Create(SS);
  RectToQuad(Rect, Quad);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Quad: PQuadDouble;
  X1, Y1, X2, Y2: Double; SS: Cardinal = 8);
begin
  Create(SS);
  QuadToRect(Quad, X1, Y1, X2, Y2);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Quad: PQuadDouble;
 Rect: TRectDouble; SS: Cardinal = 8);
begin
  Create(SS);
  QuadToRect(Quad, Rect);
end;

destructor TAggSpanInterpolatorPerspectiveExact.Destroy;
begin
  FTransDir.Free;
  FTransInv.Free;

  inherited;
end;

procedure TAggSpanInterpolatorPerspectiveExact.QuadToQuad(Src,
  Dst: PQuadDouble);
begin
  FTransDir.QuadToQuad(Src, Dst);
  FTransInv.QuadToQuad(Dst, Src);
end;

procedure TAggSpanInterpolatorPerspectiveExact.RectToQuad(X1, Y1, X2, Y2: Double;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveExact.RectToQuad(Rect: TRectDouble;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveExact.QuadToRect(Quad: PQuadDouble;
  X1, Y1, X2, Y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  QuadToQuad(Quad, @Dst);
end;

procedure TAggSpanInterpolatorPerspectiveExact.QuadToRect(Quad: PQuadDouble;
  Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggSpanInterpolatorPerspectiveExact.IsValid;
begin
  Result := FTransDir.IsValid;
end;

procedure TAggSpanInterpolatorPerspectiveExact.SetBegin;
var
  Delta, Temp: TPointDouble;
  TempDelta: Double;
  Sx1, Sy1, Sx2, Sy2: Integer;
begin
  FIterator := FTransDir.GetBegin(X, Y, 1.0);

  Temp.X := FIterator.X;
  Temp.Y := FIterator.Y;

  TempDelta := 1 / CAggSubpixelSize;

  Delta.X := Temp.X + TempDelta;
  Delta.Y := Temp.Y;

  FTransInv.Transform(FTransInv, @Delta.X, @Delta.Y);

  Delta.X := Delta.X - X;
  Delta.Y := Delta.Y - Y;
  Sx1 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.X, Delta.Y)),
    CAggSubpixelShift);
  Delta.X := Temp.X;
  Delta.Y := Temp.Y + TempDelta;

  FTransInv.Transform(FTransInv, @Delta.X, @Delta.Y);

  Delta.X := Delta.X - X;
  Delta.Y := Delta.Y - Y;
  Sy1 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.X, Delta.Y)),
    CAggSubpixelShift);

  X := X + Len;
  Temp.X := X;
  Temp.Y := Y;

  FTransDir.Transform(FTransDir, @Temp.X, @Temp.Y);

  Delta.X := Temp.X + TempDelta;
  Delta.Y := Temp.Y;

  FTransInv.Transform(FTransInv, @Delta.X, @Delta.Y);

  Delta.X := Delta.X - X;
  Delta.Y := Delta.Y - Y;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.X, Delta.Y)),
    CAggSubpixelShift);
  Delta.X := Temp.X;
  Delta.Y := Temp.Y + TempDelta;

  FTransInv.Transform(FTransInv, @Delta.X, @Delta.Y);

  Delta.X := Delta.X - X;
  Delta.Y := Delta.Y - Y;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.X, Delta.Y)),
    CAggSubpixelShift);

  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveExact.Resynchronize;
var
  Sx1, Sy1, Sx2, Sy2: Integer;
  TempDelta: Double;
  Temp, Delta: TPointDouble;
begin
  // Assume x1,y1 are equal to the ones at the previous end point
  Sx1 := FScaleX.Y;
  Sy1 := FScaleY.Y;

  // Calculate transformed coordinates at x2,y2
  Temp.X := Xe;
  Temp.Y := Ye;

  FTransDir.Transform(FTransDir, @Temp.X, @Temp.Y);

  TempDelta := 1 / CAggSubpixelSize;

  // Calculate scale by X at x2,y2
  Delta.X := Temp.X + TempDelta;
  Delta.Y := Temp.Y;

  FTransInv.Transform(FTransInv, @Delta.X, @Delta.Y);

  Delta.X := Delta.X - Xe;
  Delta.Y := Delta.Y - Ye;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.X, Delta.Y)),
    CAggSubpixelShift);

  // Calculate scale by Y at x2,y2
  Delta.X := Temp.X;
  Delta.Y := Temp.Y + TempDelta;

  FTransInv.Transform(FTransInv, @Delta.X, @Delta.Y);

  Delta.X := Delta.X - Xe;
  Delta.Y := Delta.Y - Ye;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.X, Delta.Y)),
    CAggSubpixelShift);

  // Initialize the interpolators
  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveExact.IncOperator;
begin
  FIterator.IncOperator;
  FScaleX.PlusOperator;
  FScaleY.PlusOperator;
end;

procedure TAggSpanInterpolatorPerspectiveExact.Coordinates(X, Y: PInteger);
begin
  X^ := Trunc(FIterator.X * CAggSubpixelSize + 0.5);
  Y^ := Trunc(FIterator.Y * CAggSubpixelSize + 0.5);
end;

procedure TAggSpanInterpolatorPerspectiveExact.Coordinates(var X, Y: Integer);
begin
  X := Trunc(FIterator.X * CAggSubpixelSize + 0.5);
  Y := Trunc(FIterator.Y * CAggSubpixelSize + 0.5);
end;

procedure TAggSpanInterpolatorPerspectiveExact.LocalScale(X, Y: PInteger);
begin
  X^ := FScaleX.Y;
  Y^ := FScaleY.Y;
end;

procedure TAggSpanInterpolatorPerspectiveExact.Transform(X, Y: PDouble);
begin
  FTransDir.Transform(FTransDir, X, Y);
end;


{ TAggSpanInterpolatorPerspectiveLerp }

constructor TAggSpanInterpolatorPerspectiveLerp.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(Src, Dst: PQuadDouble;
  SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  QuadToQuad(Src, Dst);
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(X1, Y1, X2, Y2: Double;
  Quad: PQuadDouble; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  RectToQuad(X1, Y1, X2, Y2, Quad);
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(Quad: PQuadDouble;
  X1, Y1, X2, Y2: Double; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  QuadToRect(Quad, X1, Y1, X2, Y2);
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(Quad: PQuadDouble;
  Rect: TRectDouble; SS: Cardinal);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  QuadToRect(Quad, Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
end;

destructor TAggSpanInterpolatorPerspectiveLerp.Destroy;
begin
  FTransDir.Free;
  FTransInv.Free;
  inherited;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.QuadToQuad(Src, Dst: PQuadDouble);
begin
  FTransDir.QuadToQuad(Src, Dst);
  FTransInv.QuadToQuad(Dst, Src);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.RectToQuad(X1, Y1, X2, Y2: Double;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.RectToQuad(Rect: TRectDouble;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.QuadToRect(Quad: PQuadDouble;
  X1, Y1, X2, Y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(X1, Y1, X2, Y2));
  QuadToQuad(Quad, @Dst);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.QuadToRect(Quad: PQuadDouble;
  Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggSpanInterpolatorPerspectiveLerp.GetIsValid;
begin
  Result := FTransDir.IsValid;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.SetBegin(X, Y: Double;
  Len: Cardinal);
var
  Xt, Yt, Dx, Dy, Delta: Double;
  X1, Y1, Sx1, Sy1, X2, Y2, Sx2, Sy2: Integer;
begin
  // Calculate transformed coordinates at x1,y1
  Xt := X;
  Yt := Y;

  FTransDir.Transform(FTransDir, @Xt, @Yt);

  X1 := Trunc(Xt * CAggSubpixelSize);
  Y1 := Trunc(Yt * CAggSubpixelSize);

  Delta := 1 / CAggSubpixelSize;

  // Calculate scale by X at x1,y1
  Dx := Xt + Delta;
  Dy := Yt;

  FTransInv.Transform(FTransInv, @Dx, @Dy);

  Dx := Dx - X;
  Dy := Dy - Y;
  Sx1 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(Dx * Dx + Dy * Dy)),
    CAggSubpixelShift);

  // Calculate scale by Y at x1,y1
  Dx := Xt;
  Dy := Yt + Delta;

  FTransInv.Transform(FTransInv, @Dx, @Dy);

  Dx := Dx - X;
  Dy := Dy - Y;
  Sy1 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(Dx * Dx + Dy * Dy)),
    CAggSubpixelShift);

  // Calculate transformed coordinates at x2,y2
  X := X + Len;
  Xt := X;
  Yt := Y;

  FTransDir.Transform(FTransDir, @Xt, @Yt);

  X2 := Trunc(Xt * CAggSubpixelSize);
  Y2 := Trunc(Yt * CAggSubpixelSize);

  // Calculate scale by X at x2,y2
  Dx := Xt + Delta;
  Dy := Yt;

  FTransInv.Transform(FTransInv, @Dx, @Dy);

  Dx := Dx - X;
  Dy := Dy - Y;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(Dx * Dx + Dy * Dy)),
    CAggSubpixelShift);

  // Calculate scale by Y at x2,y2
  Dx := Xt;
  Dy := Yt + Delta;

  FTransInv.Transform(FTransInv, @Dx, @Dy);

  Dx := Dx - X;
  Dy := Dy - Y;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(Dx * Dx + Dy * Dy)),
    CAggSubpixelShift);

  // Initialize the interpolators
  FCoordX.Initialize(X1, X2, Len);
  FCoordY.Initialize(Y1, Y2, Len);
  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Resynchronize;
var
  X1, Y1, Sx1, Sy1, X2, Y2, Sx2, Sy2: Integer;

  Xt, Yt, Delta, Dx, Dy: Double;

begin
  // Assume x1,y1 are equal to the ones at the previous end point
  X1 := FCoordX.Y;
  Y1 := FCoordY.Y;
  Sx1 := FScaleX.Y;
  Sy1 := FScaleY.Y;

  // Calculate transformed coordinates at x2,y2
  Xt := Xe;
  Yt := Ye;

  FTransDir.Transform(FTransDir, @Xt, @Yt);

  X2 := Trunc(Xt * CAggSubpixelSize);
  Y2 := Trunc(Yt * CAggSubpixelSize);

  Delta := 1 / CAggSubpixelSize;

  // Calculate scale by X at x2,y2
  Dx := Xt + Delta;
  Dy := Yt;

  FTransInv.Transform(FTransInv, @Dx, @Dy);

  Dx := Dx - Xe;
  Dy := Dy - Ye;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(Dx * Dx + Dy * Dy)),
    CAggSubpixelShift);

  // Calculate scale by Y at x2,y2
  Dx := Xt;
  Dy := Yt + Delta;

  FTransInv.Transform(FTransInv, @Dx, @Dy);

  Dx := Dx - Xe;
  Dy := Dy - Ye;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(Dx * Dx + Dy * Dy)),
    CAggSubpixelShift);

  // Initialize the interpolators
  FCoordX.Initialize(X1, X2, Len);
  FCoordY.Initialize(Y1, Y2, Len);
  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.IncOperator;
begin
  FCoordX.PlusOperator;
  FCoordY.PlusOperator;
  FScaleX.PlusOperator;
  FScaleY.PlusOperator;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Coordinates(X, Y: PInteger);
begin
  X^ := FCoordX.Y;
  Y^ := FCoordY.Y;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Coordinates(var X, Y: Integer);
begin
  X := FCoordX.Y;
  Y := FCoordY.Y;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.LocalScale;
begin
  X^ := FScaleX.Y;
  Y^ := FScaleY.Y;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Transform;
begin
  FTransDir.Transform(FTransDir, X, Y);
end;

end.
