unit AggConvCurve;

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

uses
  AggBasics,
  AggCurves,
  AggVertexSource;

type
  // Curve converter class. Any path storage can have Bezier curves defined
  // by their control points. There're two types of curves supported: Curve3
  // and Curve4. Curve3 is a conic Bezier curve with 2 endpoints and 1 control
  // point. Curve4 has 2 control points (4 points in total) and can be used
  // to interpolate more complicated curves. Curve4, unlike Curve3 can be used
  // to approximate arcs, both circular and elliptical. Curves are approximated
  // with straight lines and one of the approaches is just to store the whole
  // sequence of vertices that approximate our curve. It takes additional
  // memory, and at the same time the consecutive vertices can be calculated
  // on demand.
  //
  // Initially, path storages are not suppose to keep all the vertices of the
  // curves (although, nothing prevents us from doing so). Instead, PathStorage
  // keeps only vertices, needed to calculate a curve on demand. Those vertices
  // are marked with special commands. So, if the PathStorage contains curves
  // (which are not real curves yet), and we render this storage directly,
  // all we will see is only 2 or 3 straight line segments (for Curve3 and
  // Curve4 respectively). If we need to see real curves drawn we need to
  // include this class into the conversion pipeline.
  //
  // Class TAggConvCurve recognizes commands CAggPathCmdCurve3 and
  // CAggPathCmdCurve4 and converts these vertices into a MoveTo/LineTo
  // sequence.

  TAggConvCurve = class(TAggCustomCurve)
  private
    FSource: TAggVertexSource;
    FLast: TPointDouble;
    FCurve3: TAggCurve3;
    FCurve4: TAggCurve4;
    procedure SetSource(Source: TAggVertexSource);
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create(Source: TAggVertexSource; C3: TAggCurve3 = nil;
      C4: TAggCurve4 = nil);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Source: TAggVertexSource read FSource write SetSource;
  end;

implementation


{ TAggConvCurve }

constructor TAggConvCurve.Create(Source: TAggVertexSource; C3: TAggCurve3 = nil;
  C4: TAggCurve4 = nil);
begin
  if C3 <> nil then
    FCurve3 := C3
  else
    FCurve3 := TAggCurve3.Create;

  if C4 <> nil then
    FCurve4 := C4
  else
    FCurve4 := TAggCurve4.Create;

  FSource := Source;
  FLast := PointDouble(0);
end;

destructor TAggConvCurve.Destroy;
begin
  if Assigned(FCurve3) then
    FCurve3.Free;

  if FCurve4 <> nil then
    FCurve4.Free;

  inherited;
end;

procedure TAggConvCurve.SetSource(Source: TAggVertexSource);
begin
  FSource := Source;
end;

procedure TAggConvCurve.SetApproximationMethod(Value: TAggCurveApproximationMethod);
begin
  FCurve3.ApproximationMethod := Value;
  FCurve4.ApproximationMethod := Value;
end;

function TAggConvCurve.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := FCurve4.ApproximationMethod;
end;

procedure TAggConvCurve.SetApproximationScale(Value: Double);
begin
  FCurve3.ApproximationScale := Value;
  FCurve4.ApproximationScale := Value;
end;

function TAggConvCurve.GetApproximationScale: Double;
begin
  Result := FCurve4.ApproximationScale;
end;

procedure TAggConvCurve.SetAngleTolerance(Value: Double);
begin
  FCurve3.AngleTolerance := Value;
  FCurve4.AngleTolerance := Value;
end;

function TAggConvCurve.GetAngleTolerance: Double;
begin
  Result := FCurve4.AngleTolerance;
end;

procedure TAggConvCurve.SetCuspLimit(Value: Double);
begin
  FCurve3.CuspLimit := Value;
  FCurve4.CuspLimit := Value;
end;

function TAggConvCurve.GetCuspLimit: Double;
begin
  Result := FCurve4.CuspLimit;
end;

procedure TAggConvCurve.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);

  FLast := PointDouble(0);

  FCurve3.Reset;
  FCurve4.Reset;
end;

function TAggConvCurve.Vertex(X, Y: PDouble): Cardinal;
var
  Ct2Pt, EndPt: TPointDouble;
  Cmd: Cardinal;
begin
  if not IsStop(FCurve3.Vertex(X, Y)) then
  begin
    FLast := PointDouble(X^, Y^);

    Result := CAggPathCmdLineTo;

    Exit;
  end;

  if not IsStop(FCurve4.Vertex(X, Y)) then
  begin
    FLast := PointDouble(X^, Y^);

    Result := CAggPathCmdLineTo;

    Exit;
  end;

  Cmd := FSource.Vertex(X, Y);

  case Cmd of
    CAggPathCmdMoveTo, CAggPathCmdLineTo:
      FLast := PointDouble(X^, Y^);

    CAggPathCmdCurve3:
      begin
        FSource.Vertex(@EndPt.X, @EndPt.Y);
        FCurve3.Init3(FLast, PointDouble(X^, Y^), EndPt);

        FCurve3.Vertex(X, Y); // First call returns CAggPathCmdMoveTo
        FCurve3.Vertex(X, Y); // This is the first vertex of the curve

        Cmd := CAggPathCmdLineTo;
      end;

    CAggPathCmdCurve4:
      begin
        FSource.Vertex(@Ct2Pt.x, @Ct2Pt.y);
        FSource.Vertex(@EndPt.X, @EndPt.Y);

        FCurve4.Init4(FLast, PointDouble(X^, Y^), Ct2Pt, EndPt);

        FCurve4.Vertex(X, Y); // First call returns CAggPathCmdMoveTo
        FCurve4.Vertex(X, Y); // This is the first vertex of the curve

        Cmd := CAggPathCmdLineTo;
      end;
  end;

  Result := Cmd;
end;

end.
