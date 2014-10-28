unit AggBezierArc;

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
  AggMath,
  AggBasics,
  AggVertexSource,
  AggTransAffine;

type
  TAggBezierArc = class(TAggCustomVertexSource)
  private
    FVertex, FNumVertices: Cardinal;

    FVertices: TDoubleMatrix2x6;

    FCmd: Cardinal;
  protected
    // Supplemantary functions. NumVertices() actually returns doubled
    // number of vertices. That is, for 1 vertex it returns 2.
    function GetVertices: PDoubleMatrix2x6;
    function GetNumVertices: Cardinal;

    procedure Init(X, Y, Rx, Ry, StartAngle, SweepAngle: Double);
  public
    constructor Create; overload;
    constructor Create(X, Y, Rx, Ry, StartAngle,
      SweepAngle: Double); overload;
    constructor Create(X, Y: Double; Radius: TPointDouble; StartAngle,
      SweepAngle: Double); overload;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property NumVertices: Cardinal read GetNumVertices;
  end;

  // Compute an SVG-style bezier arc.
  //
  // Computes an elliptical arc from (x1, y1) to (x2, y2). The size and
  // orientation of the Ellipse are defined by two radii (rx, ry)
  // and an x-axis-rotation, which indicates how the Ellipse as a whole
  // is rotated relative to the current coordinate system. The center
  // (cx, cy) of the Ellipse is calculated automatically to satisfy the
  // constraints imposed by the other parameters.
  // large-arc-flag and sweep-flag contribute to the automatic calculations
  // and help determine how the arc is drawn.
  TAggBezierArcSvg = class(TAggBezierArc)
  private
    FRadiiOK: Boolean;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, Rx, Ry, Angle: Double;
      LargeArcFlag, SweepFlag: Boolean; X2, Y2: Double); overload;

    procedure Init(X0, Y0, Rx, Ry, Angle: Double;
      LargeArcFlag, SweepFlag: Boolean; X2, Y2: Double);

    property RadiiOK: Boolean read FRadiiOK;
  end;

procedure ArcToBezier(Cx, Cy, Rx, Ry, StartAngle, SweepAngle: Double;
  Curve: PDoubleArray8);

implementation

const
  CBezierArcAngleEpsilon = 0.01;


procedure ArcToBezier(Cx, Cy, Rx, Ry, StartAngle, SweepAngle: Double;
  Curve: PDoubleArray8);
var
  I: Cardinal;

  Sn, Cs, X0, Y0, Tx, Ty: Double;
  Px, Py                : array [0..3] of Double;
begin
  SinCos(SweepAngle * 0.5, Y0, X0);
  Tx := (1.0 - X0) * 4.0 / 3.0;
  Ty := Y0 - Tx * X0 / Y0;

  Px[0] := X0;
  Py[0] := -Y0;
  Px[1] := X0 + Tx;
  Py[1] := -Ty;
  Px[2] := X0 + Tx;
  Py[2] := Ty;
  Px[3] := X0;
  Py[3] := Y0;

  SinCos(StartAngle + SweepAngle * 0.5, Sn, Cs);

  for I := 0 to 3 do
  begin
    Curve[I * 2] := Cx + Rx * (Px[I] * Cs - Py[I] * Sn);
    Curve[I * 2 + 1] := Cy + Ry * (Px[I] * Sn + Py[I] * Cs);
  end;
end;


{ TAggBezierArc }

constructor TAggBezierArc.Create;
begin
  FVertex := 26;

  FNumVertices := 0;

  FCmd := CAggPathCmdLineTo;
end;

constructor TAggBezierArc.Create(X, Y, Rx, Ry, StartAngle,
  SweepAngle: Double);
begin
  Init(X, Y, Rx, Ry, StartAngle, SweepAngle);
end;

constructor TAggBezierArc.Create(X, Y: Double; Radius: TPointDouble; StartAngle,
  SweepAngle: Double);
begin
  Init(X, Y, Radius.X, Radius.Y, StartAngle, SweepAngle);
end;

procedure TAggBezierArc.Init(X, Y, Rx, Ry, StartAngle, SweepAngle: Double);
var
  I: Integer;
  F: Double;

  Sn, Cn: Double;
  TotalSweep, LocalSweep, PrevSweep: Double;

  Done: Boolean;
begin
  I := Trunc(StartAngle / (2.0 * Pi));
  F := StartAngle - (I * 2.0 * Pi);

  StartAngle := F;

  if SweepAngle >= 2.0 * Pi then
    SweepAngle := 2.0 * Pi;

  if SweepAngle <= -2.0 * Pi then
    SweepAngle := -2.0 * Pi;

  if Abs(SweepAngle) < 1E-10 then
  begin
    FNumVertices := 4;

    FCmd := CAggPathCmdLineTo;

    SinCosScale(StartAngle, Sn, Cn, Ry, Rx);
    FVertices[0] := X + Cn;
    FVertices[1] := Y + Sn;
    SinCosScale(StartAngle + SweepAngle, Sn, Cn, Ry, Rx);
    FVertices[2] := X + Cn;
    FVertices[3] := Y + Sn;

    Exit;
  end;

  TotalSweep := 0.0;
  LocalSweep := 0.0;

  FNumVertices := 2;

  FCmd := CAggPathCmdCurve4;
  Done := False;

  repeat
    if SweepAngle < 0.0 then
    begin
      PrevSweep := TotalSweep;
      LocalSweep := -Pi * 0.5;
      TotalSweep := TotalSweep - (Pi * 0.5);

      if TotalSweep <= SweepAngle + CBezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;

        Done := True;
      end;

    end
    else
    begin
      PrevSweep := TotalSweep;
      LocalSweep := Pi * 0.5;
      TotalSweep := TotalSweep + (Pi * 0.5);

      if TotalSweep >= SweepAngle - CBezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;

        Done := True;
      end;
    end;

    ArcToBezier(X, Y, Rx, Ry, StartAngle, LocalSweep,
      @FVertices[FNumVertices - 2]);

    FNumVertices := FNumVertices + 6;
    StartAngle := StartAngle + LocalSweep;

  until Done or (FNumVertices >= 26);
end;

procedure TAggBezierArc.Rewind(PathID: Cardinal);
begin
  FVertex := 0;
end;

function TAggBezierArc.Vertex(X, Y: PDouble): Cardinal;
begin
  if FVertex >= FNumVertices then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  X^ := FVertices[FVertex];
  Y^ := FVertices[FVertex + 1];

  Inc(FVertex, 2);

  if FVertex = 2 then
    Result := CAggPathCmdMoveTo
  else
    Result := FCmd;
end;

function TAggBezierArc.GetNumVertices;
begin
  Result := FNumVertices;
end;

function TAggBezierArc.GetVertices;
begin
  Result := @FVertices;
end;


{ TAggBezierArcSvg }

constructor TAggBezierArcSvg.Create;
begin
  inherited Create;

  FRadiiOK := False;
end;

constructor TAggBezierArcSvg.Create(X1, Y1, Rx, Ry, Angle: Double;
  LargeArcFlag, SweepFlag: Boolean; X2, Y2: Double);
begin
  inherited Create;

  FRadiiOK := False;

  Init(X1, Y1, Rx, Ry, Angle, LargeArcFlag, SweepFlag, X2, Y2);
end;

procedure TAggBezierArcSvg.Init;
var
  I: Cardinal;

  V, P, N, Sq, X1, Y1, Cx, Cy, Ux, Uy, Vx, Vy, Dx2, Dy2: Double;
  Prx, Pry, Px1, Py1, Cx1, Cy1, Sx2, Sy2, Sign, Coef: Double;
  RadiiCheck, StartAngle, SweepAngle, Cn, Sn: Double;

  Mtx: TAggTransAffineRotation;
begin
  FRadiiOK := True;

  if Rx < 0.0 then
    Rx := -Rx;

  if Ry < 0.0 then
    Ry := -Rx;

  // Calculate the middle point between
  // the current and the final points
  Dx2 := (X0 - X2) * 0.5;
  Dy2 := (Y0 - Y2) * 0.5;

  // Convert angle from degrees to radians
  SinCos(Angle, Sn, Cn);

  // Calculate (x1, y1)
  X1 := Cn * Dx2 + Sn * Dy2;
  Y1 := -Sn * Dx2 + Cn * Dy2;

  // Ensure radii are large enough
  Prx := Rx * Rx;
  Pry := Ry * Ry;
  Px1 := X1 * X1;
  Py1 := Y1 * Y1;

  // Check that radii are large enough
  RadiiCheck := Px1 / Prx + Py1 / Pry;

  if RadiiCheck > 1.0 then
  begin
    Rx := Sqrt(RadiiCheck) * Rx;
    Ry := Sqrt(RadiiCheck) * Ry;
    Prx := Rx * Rx;
    Pry := Ry * Ry;

    if RadiiCheck > 10.0 then
      FRadiiOK := False;
  end;

  // Calculate (cx1, cy1)
  if LargeArcFlag = SweepFlag then
    Sign := -1.0
  else
    Sign := 1.0;

  Sq := (Prx * Pry - Prx * Py1 - Pry * Px1) / (Prx * Py1 + Pry * Px1);

  if Sq < 0 then
    Coef := Sign * Sqrt(0)
  else
    Coef := Sign * Sqrt(Sq);

  Cx1 := Coef * ((Rx * Y1) / Ry);
  Cy1 := Coef * -((Ry * X1) / Rx);

  // Calculate (cx, cy) from (cx1, cy1)
  Sx2 := (X0 + X2) / 2.0;
  Sy2 := (Y0 + Y2) / 2.0;
  Cx := Sx2 + (Cn * Cx1 - Sn * Cy1);
  Cy := Sy2 + (Sn * Cx1 + Cn * Cy1);

  // Calculate the StartAngle (angle1) and the SweepAngle (dangle)
  Ux := (X1 - Cx1) / Rx;
  Uy := (Y1 - Cy1) / Ry;
  Vx := (-X1 - Cx1) / Rx;
  Vy := (-Y1 - Cy1) / Ry;

  // Calculate the angle start
  N := Sqrt(Ux * Ux + Uy * Uy);
  P := Ux; // (1 * ux ) + (0 * uy )

  if Uy < 0 then
    Sign := -1.0
  else
    Sign := 1.0;

  V := P / N;

  if V < -1.0 then
    V := -1.0;

  if V > 1.0 then
    V := 1.0;

  StartAngle := Sign * ArcCos(V);

  // Calculate the sweep angle
  N := Sqrt((Ux * Ux + Uy * Uy) * (Vx * Vx + Vy * Vy));
  P := Ux * Vx + Uy * Vy;

  if Ux * Vy - Uy * Vx < 0 then
    Sign := -1.0
  else
    Sign := 1.0;

  V := P / N;

  if V < -1.0 then
    V := -1.0;

  if V > 1.0 then
    V := 1.0;

  SweepAngle := Sign * ArcCos(V);

  if (not SweepFlag) and (SweepAngle > 0) then
    SweepAngle := SweepAngle - Pi * 2.0
  else if SweepFlag and (SweepAngle < 0) then
    SweepAngle := SweepAngle + Pi * 2.0;

  // We can now build and transform the resulting arc
  inherited Init(0.0, 0.0, Rx, Ry, StartAngle, SweepAngle);

  Mtx := TAggTransAffineRotation.Create(Angle);
  try
    Mtx.Translate(Cx, Cy);

    I := 2;

    while I < NumVertices - 2 do
    begin
      // Mtx.Transform(@FArc.Vertices[i], @FArc.Vertices[i + 1 ]);
      Mtx.Transform(Mtx, PDouble(PtrComp(GetVertices) + I * SizeOf(Double)),
        PDouble(PtrComp(GetVertices) + (I + 1) * SizeOf(Double)));

      Inc(I, 2);
    end;
  finally
    Mtx.Free;
  end;

  // We must make sure that the starting and ending points
  // exactly coincide with the initial (x0,y0) and (x2,y2)
  GetVertices[0] := X0;
  GetVertices[1] := Y0;

  if NumVertices > 2 then
  begin
    GetVertices[NumVertices - 2] := X2;
    GetVertices[NumVertices - 1] := Y2;
  end;
end;

end.
