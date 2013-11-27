unit AggMathStroke;

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
  Math,
  AggBasics,
  AggMath,
  AggVertexSequence,
  AggArray;

const
  // Minimal angle to calculate round joins, less than 0.1 degree.
  CAggStrokeTheta = 0.001;

type
  TAggMathStroke = class
  private
    FWidth: Double;
    FWidthAbs: Double;
    FWidthEps: Double;
    FMiterLimit: Double;
    FInnerMiterLimit: Double;
    FApproxScale: Double;

    FWidthSign: Integer;

    FLineCap: TAggLineCap;
    FLineJoin: TAggLineJoin;
    FInnerJoin: TAggInnerJoin;

    function GetWidth: Double;
    procedure SetApproximationScale(Value: Double);
    procedure SetInnerJoin(Value: TAggInnerJoin);
    procedure SetInnerMiterLimit(Value: Double);
    procedure SetLineCap(Value: TAggLineCap);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetMiterLimit(Value: Double);
    procedure SetWidth(Value: Double);
  public
    constructor Create;

    procedure CalculateArc(Vc: TAggPodBVector; X, Y, Dx1, Dy1, Dx2, Dy2: Double);
    procedure CalculateMiter(Vc: TAggPodBVector; V0, V1, V2: PAggVertexDistance;
      Dx1, Dy1, Dx2, Dy2: Double; Lj: TAggLineJoin; Mlimit, Dbevel: Double);
    procedure CalculateCap(Vc: TAggPodBVector; V0, V1: PAggVertexDistance;
      Len: Double);
    procedure CalculateJoin(Vc: TAggPodBVector; V0, V1, V2: PAggVertexDistance;
      Len1, Len2: Double);

    procedure SetMiterLimitTheta(Value: Double);

    procedure AddVertex(Vc: TAggPodBVector; X, Y: Double);

    property LineCap: TAggLineCap read FLineCap write SetLineCap;
    property LineJoin: TAggLineJoin read FLineJoin write SetLineJoin;
    property InnerJoin: TAggInnerJoin read FInnerJoin write SetInnerJoin;

    property ApproximationScale: Double read FApproxScale write SetApproximationScale;
    property InnerMiterLimit: Double read FInnerMiterLimit write SetInnerMiterLimit;
    property MiterLimit: Double read FMiterLimit write SetMiterLimit;
    property Width: Double read GetWidth write SetWidth;
  end;

procedure StrokeCalcArc(OutVertices: TAggPodDeque;
  X, Y, Dx1, Dy1, Dx2, Dy2, Width, ApproximationScale: Double);

procedure StrokeCalcMiter(OutVertices: TAggPodDeque;
  V0, V1, V2: PAggVertexDistance; Dx1, Dy1, Dx2, Dy2, Width: Double;
  LineJoin: TAggLineJoin; MiterLimit, ApproximationScale: Double);

procedure StrokeCalcCap(OutVertices: TAggPodDeque; V0, V1: PAggVertexDistance;
  Len: Double; LineCap: TAggLineCap; Width, ApproximationScale: Double);

procedure StrokeCalcJoin(OutVertices: TAggPodDeque;
  V0, V1, V2: PAggVertexDistance; Len1, Len2, Width: Double;
  LineJoin: TAggLineJoin; InnerJoin: TAggInnerJoin; MiterLimit, InnerMiterLimit,
  ApproximationScale: Double);

implementation


{ TAggMathStroke }

constructor TAggMathStroke.Create;
begin
  FWidth := 0.5;
  FWidthAbs := 0.5;
  FWidthEps := 1 / 2048;
  FWidthSign := 1;

  FMiterLimit := 4.0;
  FInnerMiterLimit := 1.01;
  FApproxScale := 1.0;

  FLineCap := lcButt;
  FLineJoin := ljMiter;
  FInnerJoin := ijMiter;
end;

procedure TAggMathStroke.SetLineCap(Value: TAggLineCap);
begin
  FLineCap := Value;
end;

procedure TAggMathStroke.SetLineJoin(Value: TAggLineJoin);
begin
  FLineJoin:= Value;
end;

procedure TAggMathStroke.SetInnerJoin(Value: TAggInnerJoin);
begin
  FInnerJoin := Value;
end;

procedure TAggMathStroke.SetWidth(Value: Double);
const
  CScale = 1 / 1024;
begin
  FWidth := Value * 0.5;

  if FWidth < 0 then
  begin
    FWidthAbs := -FWidth;
    FWidthSign := -1;
  end
  else
  begin
    FWidthAbs := FWidth;
    FWidthSign := 1;
  end;

  FWidthEps := FWidth * CScale;
end;

procedure TAggMathStroke.SetMiterLimit(Value: Double);
begin
  FMiterLimit := Value;
end;

procedure TAggMathStroke.SetMiterLimitTheta(Value: Double);
begin
  FMiterLimit := 1.0 / Sin(Value * 0.5);
end;

procedure TAggMathStroke.SetInnerMiterLimit(Value: Double);
begin
  FInnerMiterLimit := Value;
end;

procedure TAggMathStroke.SetApproximationScale(Value: Double);
begin
  FApproxScale := Value;
end;

function TAggMathStroke.GetWidth: Double;
begin
  Result := 2 * FWidth;
end;

procedure TAggMathStroke.CalculateCap(Vc: TAggPodBVector; V0,
  V1: PAggVertexDistance; Len: Double);
var
  Delta: array [0..1] of TPointDouble;
  Da, A1: Double;
  Sn, Cn: Double;
  I, N: Integer;
begin
  Vc.RemoveAll;

  Da := 1 / Len;
  Delta[0].X := (V1.Pos.Y - V0.Pos.Y) * Da;
  Delta[0].Y := (V1.Pos.X - V0.Pos.X) * Da;
  Delta[1] := PointDouble(0);

  Delta[0].X := Delta[0].X * FWidth;
  Delta[0].Y := Delta[0].Y * FWidth;

  if FLineCap <> lcRound then
  begin
    if FLineCap = lcSquare then
    begin
      Delta[1].X := Delta[0].Y * FWidthSign;
      Delta[1].Y := Delta[0].X * FWidthSign;
    end;

    AddVertex(Vc, V0.Pos.X - Delta[0].X - Delta[1].X,
      V0.Pos.Y + Delta[0].Y - Delta[1].Y);
    AddVertex(Vc, V0.Pos.X + Delta[0].X - Delta[1].X,
      V0.Pos.Y - Delta[0].Y - Delta[1].Y);
  end
  else
  begin
    Da := ArcCos(FWidthAbs / (FWidthAbs + 0.125 / FApproxScale)) * 2;
    N := Integer(Trunc(Pi / Da));
    Da := Pi / (N + 1);

    AddVertex(Vc, V0.Pos.X - Delta[0].X, V0.Pos.Y + Delta[0].Y);

    if FWidthSign > 0 then
    begin
      A1 := ArcTan2(Delta[0].Y, -Delta[0].X);
      A1 := A1 + Da;
      I := 0;

      while I < N do
      begin
        SinCosScale(A1, Sn, Cn, FWidth);
        AddVertex(Vc, V0.Pos.X + Cn, V0.Pos.Y + Sn);

        A1 := A1 + Da;

        Inc(I);
      end;
    end
    else
    begin
      A1 := ArcTan2(-Delta[0].Y, Delta[0].X);
      A1 := A1 - Da;
      I := 0;

      while I < N do
      begin
        SinCosScale(A1, Sn, Cn, FWidth);
        AddVertex(Vc, V0.Pos.X + Cn, V0.Pos.Y + Sn);

        A1 := A1 - Da;

        Inc(I);
      end;
    end;

    AddVertex(Vc, V0.Pos.X + Delta[0].X, V0.Pos.Y - Delta[0].Y);
  end;
end;

procedure TAggMathStroke.CalculateJoin(Vc: TAggPodBVector;
  V0, V1, V2: PAggVertexDistance; Len1, Len2: Double);
var
  Dx1, Dy1, Dx2, Dy2, Cp, Limit, Dx, Dy, Dbevel, Temp: Double;
begin
  Temp := FWidth / Len1;
  Dx1 := (V1.Pos.Y - V0.Pos.Y) * Temp;
  Dy1 := (V1.Pos.X - V0.Pos.X) * Temp;
  Temp := FWidth / Len2;
  Dx2 := (V2.Pos.Y - V1.Pos.Y) * Temp;
  Dy2 := (V2.Pos.X - V1.Pos.X) * Temp;

  Vc.RemoveAll;

  Cp := CrossProduct(V0.Pos.X, V0.Pos.Y, V1.Pos.X, V1.Pos.Y, V2.Pos.X, V2.Pos.Y);

  if (Cp <> 0) and ((Cp > 0) = (FWidth > 0)) then
  begin
    // Inner join
    if Len1 < Len2 then
      Limit := Len1 / FWidthAbs
    else
      Limit := Len2 / FWidthAbs;

    if Limit < FInnerMiterLimit then
      Limit := FInnerMiterLimit;

    case FInnerJoin of
      ijMiter:
        CalculateMiter(Vc, V0, V1, V2, Dx1, Dy1, Dx2, Dy2, ljMiterRevert,
          Limit, 0);

      ijJag, ijRound:
        begin
          Cp := (Dx1 - Dx2) * (Dx1 - Dx2) + (Dy1 - Dy2) * (Dy1 - Dy2);

          if (Cp < Len1 * Len1) and (Cp < Len2 * Len2) then
            CalculateMiter(Vc, V0, V1, V2, Dx1, Dy1, Dx2, Dy2,
              ljMiterRevert, Limit, 0)
          else if FInnerJoin = ijJag then
          begin
            AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);
            AddVertex(Vc, V1.Pos.X, V1.Pos.Y);
            AddVertex(Vc, V1.Pos.X + Dx2, V1.Pos.Y - Dy2);

          end
          else
          begin
            AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);
            AddVertex(Vc, V1.Pos.X, V1.Pos.Y);
            CalculateArc(Vc, V1.Pos.X, V1.Pos.Y, Dx2, -Dy2, Dx1, -Dy1);
            AddVertex(Vc, V1.Pos.X, V1.Pos.Y);
            AddVertex(Vc, V1.Pos.X + Dx2, V1.Pos.Y - Dy2);
          end;
        end;
    else
      begin
        // ijBevel
        AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);
        AddVertex(Vc, V1.Pos.X + Dx2, V1.Pos.Y - Dy2);
      end;
    end;
  end
  else
  begin
    // Outer join
    // ---------------
    // Calculate the distance between v1 and
    // the central point of the bevel line segment
    Dx := (Dx1 + Dx2) * 0.5;
    Dy := (Dy1 + Dy2) * 0.5;

    Dbevel := Hypot(Dx, Dy);

    if (FLineJoin = ljRound) or (FLineJoin = ljBevel) then
    begin
      // This is an optimization that reduces the number of points
      // in cases of almost collinear segments. If there's no
      // visible difference between bevel and miter joins we'd rather
      // use miter join because it adds only one point instead of two.
      //
      // Here we calculate the middle point between the bevel points
      // and then, the distance between v1 and this middle point.
      // At outer joins this distance always less than stroke width,
      // because it's actually the height of an isosceles triangle of
      // v1 and its two bevel points. If the difference between this
      // width and this value is small (no visible bevel) we can
      // add just one point.
      //
      // The constant in the expression makes the result approximately
      // the same as in round joins and caps. You can safely comment
      // out this entire "if".
      if FApproxScale * (FWidthAbs - Dbevel) < FWidthEps then
      begin
        if CalculateIntersection(V0.Pos.X + Dx1, V0.Pos.Y - Dy1,
          V1.Pos.X + Dx1, V1.Pos.Y - Dy1, V1.Pos.X + Dx2, V1.Pos.Y - Dy2,
          V2.Pos.X + Dx2, V2.Pos.Y - Dy2, @Dx, @Dy) then
          AddVertex(Vc, Dx, Dy)
        else
          AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);

        Exit;
      end;
    end;

    case FLineJoin of
      ljMiter, ljMiterRevert, ljMiterRound:
        CalculateMiter(Vc, V0, V1, V2, Dx1, Dy1, Dx2, Dy2, FLineJoin,
          FMiterLimit, Dbevel);

      ljRound:
        CalculateArc(Vc, V1.Pos.X, V1.Pos.Y, Dx1, -Dy1, Dx2, -Dy2);
    else
      begin
        // Bevel join
        AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);
        AddVertex(Vc, V1.Pos.X + Dx2, V1.Pos.Y - Dy2);
      end;
    end;
  end;
end;

procedure TAggMathStroke.AddVertex(Vc: TAggPodBVector; X, Y: Double);
var
  Pt: TPointDouble;
begin
  Pt.X := X;
  Pt.Y := Y;

  Vc.Add(@Pt);
end;

procedure TAggMathStroke.CalculateArc(Vc: TAggPodBVector;
  X, Y, Dx1, Dy1, Dx2, Dy2: Double);
var
  A1, A2, Da: Double;
  Sn, Cn: Double;
  I, N: Integer;
begin
  A1 := ArcTan2(Dy1 * FWidthSign, Dx1 * FWidthSign);
  A2 := ArcTan2(Dy2 * FWidthSign, Dx2 * FWidthSign);
  Da := A1 - A2;
  Da := ArcCos(FWidthAbs / (FWidthAbs + 0.125 / FApproxScale)) * 2;

  AddVertex(Vc, X + Dx1, Y + Dy1);

  if FWidthSign > 0 then
  begin
    if A1 > A2 then
      A2 := A2 + 2 * Pi;

    N := Integer(Trunc((A2 - A1) / Da));
    Da := (A2 - A1) / (N + 1);
    A1 := A1 + Da;
    I := 0;

    while I < N do
    begin
      SinCos(A1, Sn, Cn);
      AddVertex(Vc, X + Cn * FWidth, Y + Sn * FWidth);

      A1 := A1 + Da;

      Inc(I);
    end;
  end
  else
  begin
    if A1 < A2 then
      A2 := A2 - 2 * Pi;

    N := Integer(Trunc((A1 - A2) / Da));
    Da := (A1 - A2) / (N + 1);
    A1 := A1 - Da;
    I := 0;

    while I < N do
    begin
      SinCos(A1, Sn, Cn);
      AddVertex(Vc, X + Cn * FWidth, Y + Sn * FWidth);

      A1 := A1 - Da;

      Inc(I);
    end;
  end;

  AddVertex(Vc, X + Dx2, Y + Dy2);
end;

procedure TAggMathStroke.CalculateMiter(Vc: TAggPodBVector;
  V0, V1, V2: PAggVertexDistance; Dx1, Dy1, Dx2, Dy2: Double; Lj: TAggLineJoin;
  Mlimit, Dbevel: Double);
var
  Xi, Yi, Di, Lim, X2, Y2, X1, Y1: Double;
  MiterLimitExceeded, IntersectionFailed: Boolean;
begin
  Xi := V1.Pos.X;
  Yi := V1.Pos.Y;
  Di := 1;
  Lim := FWidthAbs * Mlimit;

  MiterLimitExceeded := True; // Assume the worst
  IntersectionFailed := True; // Assume the worst

  if CalculateIntersection(V0.Pos.X + Dx1, V0.Pos.Y - Dy1, V1.Pos.X + Dx1,
    V1.Pos.Y - Dy1, V1.Pos.X + Dx2, V1.Pos.Y - Dy2, V2.Pos.X + Dx2,
    V2.Pos.Y - Dy2, @Xi, @Yi) then
  begin
    // Calculation of the intersection succeeded
    Di := CalculateDistance(V1.Pos.X, V1.Pos.Y, Xi, Yi);

    if Di <= Lim then
    begin
      // Inside the miter limit
      AddVertex(Vc, Xi, Yi);

      MiterLimitExceeded := False;
    end;

    IntersectionFailed := False;
  end
  else
  begin
    // Calculation of the intersection failed, most probably
    // the three points lie one straight line.
    // First check if v0 and v2 lie on the opposite sides of vector:
    // (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
    // to the line determined by vertices v0 and v1.
    // This condition determines whether the next line segments continues
    // the previous one or goes back.
    X2 := V1.Pos.X + Dx1;
    Y2 := V1.Pos.Y - Dy1;

    if (CrossProduct(V0.Pos.X, V0.Pos.Y, V1.Pos.X, V1.Pos.Y, X2, Y2) < 0.0)
      = (CrossProduct(V1.Pos.X, V1.Pos.Y, V2.Pos.X, V2.Pos.Y, X2, Y2) < 0.0) then
    begin
      // This case means that the next segment continues
      // the previous one (straight line)
      AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);

      MiterLimitExceeded := False;
    end;
  end;

  // Miter limit exceeded
  if MiterLimitExceeded then
    case Lj of
      ljMiterRevert:
        begin
          // For the compatibility with SVG, PDF, etc,
          // we use a simple bevel join instead of
          // "smart" bevel
          AddVertex(Vc, V1.Pos.X + Dx1, V1.Pos.Y - Dy1);
          AddVertex(Vc, V1.Pos.X + Dx2, V1.Pos.Y - Dy2);
        end;

      ljMiterRound:
        CalculateArc(Vc, V1.Pos.X, V1.Pos.Y, Dx1, -Dy1, Dx2, -Dy2);

      // If no miter-revert, calculate new dx1, dy1, dx2, dy2
    else
      if IntersectionFailed then
      begin
        Mlimit := Mlimit * FWidthSign;

        AddVertex(Vc, V1.Pos.X + Dx1 + Dy1 * Mlimit, V1.Pos.Y - Dy1 + Dx1 * Mlimit);

        AddVertex(Vc, V1.Pos.X + Dx2 - Dy2 * Mlimit, V1.Pos.Y - Dy2 - Dx2 * Mlimit);

      end
      else
      begin
        X1 := V1.Pos.X + Dx1;
        Y1 := V1.Pos.Y - Dy1;
        X2 := V1.Pos.X + Dx2;
        Y2 := V1.Pos.Y - Dy2;
        Di := (Lim - Dbevel) / (Di - Dbevel);

        AddVertex(Vc, X1 + (Xi - X1) * Di, Y1 + (Yi - Y1) * Di);

        AddVertex(Vc, X2 + (Xi - X2) * Di, Y2 + (Yi - Y2) * Di);
      end;
    end;
end;

procedure StrokeCalcArc(OutVertices: TAggPodDeque;
  X, Y, Dx1, Dy1, Dx2, Dy2, Width, ApproximationScale: Double);
var
  Pt: TPointDouble;

  A1, A2, Da: Double;
  Sn, Cn: Double;

  Ccw: Boolean;
begin
  A1 := ArcTan2(Dy1, Dx1);
  A2 := ArcTan2(Dy2, Dx2);
  Da := A1 - A2;

  // Possible optimization. Not important at all; consumes time but happens rarely
  // if Abs(da ) < CAggStrokeTheta then
  // begin
  // pt.x:=(x + x + dx1 + dx2 ) * 0.5;
  // pt.y:=(y + y + dy1 + dy2 ) * 0.5;
  //
  // OutVertices.add(@pt );
  // exit;
  //
  // end;
  Ccw := (Da > 0.0) and (Da < Pi);

  if Width < 0 then
    Width := -Width;

  if ApproximationScale = 0 then
    ApproximationScale := 0.00001;

  Da := ArcCos(Width / (Width + 0.125 / ApproximationScale)) * 2;

  Pt.X := X + Dx1;
  Pt.Y := Y + Dy1;

  OutVertices.Add(@Pt);

  if not Ccw then
  begin
    if A1 > A2 then
      A2 := A2 + (2 * Pi);

    A2 := A2 - 0.25 * Da;
    A1 := A1 + Da;

    while A1 < A2 do
    begin
      SinCosScale(A1, Sn, Cn, Width);
      Pt.X := X + Cn;
      Pt.Y := Y + Sn;

      OutVertices.Add(@Pt);

      A1 := A1 + Da;
    end;
  end
  else
  begin
    if A1 < A2 then
      A2 := A2 - (2 * Pi);

    A2 := A2 + 0.25 * Da;
    A1 := A1 - Da;

    while A1 > A2 do
    begin
      SinCosScale(A1, Sn, Cn, Width);
      Pt.X := X + Cn;
      Pt.Y := Y + Sn;

      OutVertices.Add(@Pt);

      A1 := A1 - Da;
    end;
  end;

  Pt.X := X + Dx2;
  Pt.Y := Y + Dy2;

  OutVertices.Add(@Pt);
end;

procedure StrokeCalcMiter(OutVertices: TAggPodDeque;
  V0, V1, V2: PAggVertexDistance; Dx1, Dy1, Dx2, Dy2, Width: Double;
  LineJoin: TAggLineJoin; MiterLimit, ApproximationScale: Double);
var
  Pt: TPointDouble;
  Xi, Yi, D1, Lim, X2, Y2: Double;
  MiterLimitExceeded: Boolean;
begin
  Xi := V1.Pos.X;
  Yi := V1.Pos.Y;

  MiterLimitExceeded := True; // Assume the worst

  if CalculateIntersection(V0.Pos.X + Dx1, V0.Pos.Y - Dy1, V1.Pos.X + Dx1,
    V1.Pos.Y - Dy1, V1.Pos.X + Dx2, V1.Pos.Y - Dy2, V2.Pos.X + Dx2,
    V2.Pos.Y - Dy2, @Xi, @Yi) then
  begin
    // Calculation of the intersection succeeded
    // ---------------------
    D1 := CalculateDistance(V1.Pos.X, V1.Pos.Y, Xi, Yi);
    Lim := Width * MiterLimit;

    if D1 <= Lim then
    begin
      // Inside the miter limit
      // ---------------------
      Pt.X := Xi;
      Pt.Y := Yi;

      OutVertices.Add(@Pt);

      MiterLimitExceeded := False;
    end;
  end
  else
  begin
    // Calculation of the intersection failed, most probably
    // the three points lie one straight line.
    // First check if v0 and v2 lie on the opposite sides of vector:
    // (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
    // to the line determined by vertices v0 and v1.
    // This condition determines whether the next line segments continues
    // the previous one or goes back.
    // ----------------
    X2 := V1.Pos.X + Dx1;
    Y2 := V1.Pos.Y - Dy1;

    if (((X2 - V0.Pos.X) * Dy1 - (V0.Pos.Y - Y2) * Dx1 < 0.0) <>
      ((X2 - V2.Pos.X) * Dy1 - (V2.Pos.Y - Y2) * Dx1 < 0.0)) then
    begin
      // This case means that the next segment continues
      // the previous one (straight line)
      // -----------------
      Pt.X := V1.Pos.X + Dx1;
      Pt.Y := V1.Pos.Y - Dy1;

      OutVertices.Add(@Pt);

      MiterLimitExceeded := False;
    end;
  end;

  if MiterLimitExceeded then
    // Miter limit exceeded
    // ------------------------
    case LineJoin of
      ljMiterRevert:
        begin
          // For the compatibility with SVG, PDF, etc,
          // we use a simple bevel join instead of
          // "smart" bevel
          // -------------------
          Pt.X := V1.Pos.X + Dx1;
          Pt.Y := V1.Pos.Y - Dy1;

          OutVertices.Add(@Pt);

          Pt.X := V1.Pos.X + Dx2;
          Pt.Y := V1.Pos.Y - Dy2;

          OutVertices.Add(@Pt);
        end;

      ljMiterRound:
        StrokeCalcArc(OutVertices, V1.Pos.X, V1.Pos.Y, Dx1, -Dy1, Dx2, -Dy2, Width,
          ApproximationScale);

    else
      begin
        // If no miter-revert, calculate new dx1, dy1, dx2, dy2
        // ----------------
        Pt.X := V1.Pos.X + Dx1 + Dy1 * MiterLimit;
        Pt.Y := V1.Pos.Y - Dy1 + Dx1 * MiterLimit;

        OutVertices.Add(@Pt);

        Pt.X := V1.Pos.X + Dx2 - Dy2 * MiterLimit;
        Pt.Y := V1.Pos.Y - Dy2 - Dx2 * MiterLimit;

        OutVertices.Add(@Pt);
      end;
    end;
end;

procedure StrokeCalcCap(OutVertices: TAggPodDeque; V0, V1: PAggVertexDistance;
  Len: Double; LineCap: TAggLineCap; Width, ApproximationScale: Double);
var
  Pt: TPointDouble;
  Dx1, Dy1, Dx2, Dy2, A1, A2, Da: Double;
  Sn, Cn: Double;
begin
  OutVertices.RemoveAll;

  Da := 1 / Len;
  Dx1 := (V1.Pos.Y - V0.Pos.Y) * Da;
  Dy1 := (V1.Pos.X - V0.Pos.X) * Da;
  Dx2 := 0;
  Dy2 := 0;

  Dx1 := Dx1 * Width;
  Dy1 := Dy1 * Width;

  if LineCap <> lcRound then
  begin
    if LineCap = lcSquare then
    begin
      Dx2 := Dy1;
      Dy2 := Dx1;
    end;

    Pt.X := V0.Pos.X - Dx1 - Dx2;
    Pt.Y := V0.Pos.Y + Dy1 - Dy2;

    OutVertices.Add(@Pt);

    Pt.X := V0.Pos.X + Dx1 - Dx2;
    Pt.Y := V0.Pos.Y - Dy1 - Dy2;

    OutVertices.Add(@Pt);
  end
  else
  begin
    A1 := ArcTan2(Dy1, -Dx1);
    A2 := A1 + Pi;

    if ApproximationScale = 0 then
      ApproximationScale := 0.00001;

    Da := ArcCos(Width / (Width + 0.125 / ApproximationScale)) * 2;

    Pt.X := V0.Pos.X - Dx1;
    Pt.Y := V0.Pos.Y + Dy1;

    OutVertices.Add(@Pt);

    A1 := A1 + Da;
    A2 := A2 - 0.25 * Da;

    while A1 < A2 do
    begin
      SinCosScale(A1, Sn, Cn, Width);
      Pt.X := V0.Pos.X + Cn;
      Pt.Y := V0.Pos.Y + Sn;

      OutVertices.Add(@Pt);

      A1 := A1 + Da;
    end;

    Pt.X := V0.Pos.X + Dx1;
    Pt.Y := V0.Pos.Y - Dy1;

    OutVertices.Add(@Pt);
  end;
end;

procedure StrokeCalcJoin(OutVertices: TAggPodDeque;
  V0, V1, V2: PAggVertexDistance; Len1, Len2, Width: Double;
  LineJoin: TAggLineJoin; InnerJoin: TAggInnerJoin; MiterLimit, InnerMiterLimit,
  ApproximationScale: Double);
var
  Pt: TPointDouble;

  D, Dx1, Dy1, Dx2, Dy2: Double;
begin
  D := Width / Len1;
  Dx1 := (V1.Pos.Y - V0.Pos.Y) * D;
  Dy1 := (V1.Pos.X - V0.Pos.X) * D;

  D := Width / Len2;
  Dx2 := (V2.Pos.Y - V1.Pos.Y) * D;
  Dy2 := (V2.Pos.X - V1.Pos.X) * D;

  OutVertices.RemoveAll;

  if CalculatePointLocation(V0.Pos.X, V0.Pos.Y, V1.Pos.X, V1.Pos.Y, V2.Pos.X, V2.Pos.Y) > 0 then
    // Inner join
    // ---------------
    case InnerJoin of
      ijMiter:
        StrokeCalcMiter(OutVertices, V0, V1, V2, Dx1, Dy1, Dx2, Dy2, Width,
          ljMiterRevert, InnerMiterLimit, 1.0);

      ijJag, ijRound:
        begin
          D := Sqr(Dx1 - Dx2) + Sqr(Dy1 - Dy2);

          if (D < Len1 * Len1) and (D < Len2 * Len2) then
            StrokeCalcMiter(OutVertices, V0, V1, V2, Dx1, Dy1, Dx2, Dy2,
              Width, ljMiterRevert, InnerMiterLimit, 1.0)

          else if InnerJoin = ijJag then
          begin
            Pt.X := V1.Pos.X + Dx1;
            Pt.Y := V1.Pos.Y - Dy1;

            OutVertices.Add(@Pt);

            Pt.X := V1.Pos.X;
            Pt.Y := V1.Pos.Y;

            OutVertices.Add(@Pt);

            Pt.X := V1.Pos.X + Dx2;
            Pt.Y := V1.Pos.Y - Dy2;

            OutVertices.Add(@Pt);
          end
          else
          begin
            Pt.X := V1.Pos.X + Dx1;
            Pt.Y := V1.Pos.Y - Dy1;

            OutVertices.Add(@Pt);

            Pt.X := V1.Pos.X;
            Pt.Y := V1.Pos.Y;

            OutVertices.Add(@Pt);

            StrokeCalcArc(OutVertices, V1.Pos.X, V1.Pos.Y, Dx2, -Dy2, Dx1, -Dy1,
              Width, ApproximationScale);

            Pt.X := V1.Pos.X;
            Pt.Y := V1.Pos.Y;

            OutVertices.Add(@Pt);

            Pt.X := V1.Pos.X + Dx2;
            Pt.Y := V1.Pos.Y - Dy2;

            OutVertices.Add(@Pt);
          end;
        end;
    else // ijBevel
      begin
        Pt.X := V1.Pos.X + Dx1;
        Pt.Y := V1.Pos.Y - Dy1;

        OutVertices.Add(@Pt);

        Pt.X := V1.Pos.X + Dx2;
        Pt.Y := V1.Pos.Y - Dy2;

        OutVertices.Add(@Pt);
      end;

    end
  else
    // Outer join
    // ---------------
    case LineJoin of
      ljMiter, ljMiterRevert, ljMiterRound:
        StrokeCalcMiter(OutVertices, V0, V1, V2, Dx1, Dy1, Dx2, Dy2, Width,
          LineJoin, MiterLimit, ApproximationScale);

      ljRound:
        StrokeCalcArc(OutVertices, V1.Pos.X, V1.Pos.Y, Dx1, -Dy1, Dx2, -Dy2,
          Width, ApproximationScale);

    else // Bevel join
      begin
        Pt.X := V1.Pos.X + Dx1;
        Pt.Y := V1.Pos.Y - Dy1;

        OutVertices.Add(@Pt);

        Pt.X := V1.Pos.X + Dx2;
        Pt.Y := V1.Pos.Y - Dy2;

        OutVertices.Add(@Pt);
      end;
    end;
end;

end.
