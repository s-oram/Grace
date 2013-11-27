unit AggVcgenBSpline;

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
  AggArray,
  AggVertexSource;

type
  TAggInternalStatus = (siInitial, siReady, siPolygon, siEndPoly, siStop);

  TAggVcgenBSpline = class(TAggVertexSource)
  private
    FSourceVertices: TAggPodDeque;

    FSplineX, FSplineY: TAggBSpline;

    FInterpolationStep: Double;

    FClosed: Cardinal;
    FStatus: TAggInternalStatus;

    FSourceVertex: Cardinal;

    FCurrentAbscissa, FMaxAbscissa: Double;

    procedure SetInterpolationStep(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property InterpolationStep: Double read FInterpolationStep write
      SetInterpolationStep;
  end;

implementation


{ TAggVcgenBSpline }

constructor TAggVcgenBSpline.Create;
begin
  inherited Create;

  FSourceVertices := TAggPodDeque.Create(SizeOf(TPointDouble), 6);
  FSplineX := TAggBSpline.Create;
  FSplineY := TAggBSpline.Create;

  FInterpolationStep := 1.0 / 50.0;

  FClosed := 0;
  FStatus := siInitial;

  FSourceVertex := 0;
end;

destructor TAggVcgenBSpline.Destroy;
begin
  FSourceVertices.Free;
  FSplineX.Free;
  FSplineY.Free;

  inherited;
end;

procedure TAggVcgenBSpline.SetInterpolationStep(Value: Double);
begin
  FInterpolationStep := Value;
end;

procedure TAggVcgenBSpline.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FStatus := siInitial;

  FSourceVertex := 0;
end;

procedure TAggVcgenBSpline.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  Pt: TPointDouble;
begin
  FStatus := siInitial;

  if IsMoveTo(Cmd) then
  begin
    Pt.X := X;
    Pt.Y := Y;

    FSourceVertices.ModifyLast(@Pt);
  end
  else if IsVertex(Cmd) then
  begin
    Pt.X := X;
    Pt.Y := Y;

    FSourceVertices.Add(@Pt);
  end
  else
    FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenBSpline.Rewind(PathID: Cardinal);
var
  I: Cardinal;
  X: Double;
begin
  FCurrentAbscissa := 0.0;
  FMaxAbscissa := 0.0;

  FSourceVertex := 0;

  if (FStatus = siInitial) and (FSourceVertices.Size > 2) then
  begin
    if FClosed <> 0 then
    begin
      FSplineX.Init(FSourceVertices.Size + 8);
      FSplineY.Init(FSourceVertices.Size + 8);

      FSplineX.AddPoint(0.0,
        PPointDouble(FSourceVertices.Prev(FSourceVertices.Size - 3))^.X);
      FSplineY.AddPoint(0.0,
        PPointDouble(FSourceVertices.Prev(FSourceVertices.Size - 3))^.Y);
      FSplineX.AddPoint(1.0,
        PPointDouble(FSourceVertices[FSourceVertices.Size - 3])^.X);
      FSplineY.AddPoint(1.0,
        PPointDouble(FSourceVertices[FSourceVertices.Size - 3])^.Y);
      FSplineX.AddPoint(2.0,
        PPointDouble(FSourceVertices[FSourceVertices.Size - 2])^.X);
      FSplineY.AddPoint(2.0,
        PPointDouble(FSourceVertices[FSourceVertices.Size - 2])^.Y);
      FSplineX.AddPoint(3.0,
        PPointDouble(FSourceVertices[FSourceVertices.Size - 1])^.X);
      FSplineY.AddPoint(3.0,
        PPointDouble(FSourceVertices[FSourceVertices.Size - 1])^.Y);
    end
    else
    begin
      FSplineX.Init(FSourceVertices.Size);
      FSplineY.Init(FSourceVertices.Size);
    end;

    for I := 0 to FSourceVertices.Size - 1 do
    begin
      if FClosed <> 0 then
        X := I + 4
      else
        X := I;

      FSplineX.AddPoint(X,
        PPointDouble(FSourceVertices[I])^.X);
      FSplineY.AddPoint(X,
        PPointDouble(FSourceVertices[I])^.Y);
    end;

    FCurrentAbscissa := 0.0;
    FMaxAbscissa := FSourceVertices.Size - 1;

    if FClosed <> 0 then
    begin
      FCurrentAbscissa := 4.0;
      FMaxAbscissa := FMaxAbscissa + 5.0;

      FSplineX.AddPoint(FSourceVertices.Size + 4,
        PPointDouble(FSourceVertices[0])^.X);

      FSplineY.AddPoint(FSourceVertices.Size + 4,
        PPointDouble(FSourceVertices[0])^.Y);

      FSplineX.AddPoint(FSourceVertices.Size + 5,
        PPointDouble(FSourceVertices[1])^.X);

      FSplineY.AddPoint(FSourceVertices.Size + 5,
        PPointDouble(FSourceVertices[1])^.Y);

      FSplineX.AddPoint(FSourceVertices.Size + 6,
        PPointDouble(FSourceVertices[2])^.X);

      FSplineY.AddPoint(FSourceVertices.Size + 6,
        PPointDouble(FSourceVertices[2])^.Y);

      FSplineX.AddPoint(FSourceVertices.Size + 7,
        PPointDouble(FSourceVertices.Next(2))^.X);

      FSplineY.AddPoint(FSourceVertices.Size + 7,
        PPointDouble(FSourceVertices.Next(2))^.Y);
    end;

    FSplineX.Prepare;
    FSplineY.Prepare;
  end;

  FStatus := siReady;
end;

function TAggVcgenBSpline.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;

label
  _next, _ready, _polygon;

begin
  Cmd := CAggPathCmdLineTo;

_next:
  while not IsStop(Cmd) do
    case FStatus of
      siInitial:
        begin
          Rewind(0);

          goto _ready;
        end;

      siReady:
      _ready:
        begin
          if FSourceVertices.Size < 2 then
          begin
            Cmd := CAggPathCmdStop;

            goto _next;
          end;

          if FSourceVertices.Size = 2 then
          begin
            X^ := PPointDouble
              (FSourceVertices[FSourceVertex])^.X;
            Y^ := PPointDouble
              (FSourceVertices[FSourceVertex])^.Y;

            Inc(FSourceVertex);

            if FSourceVertex = 1 then
            begin
              Result := CAggPathCmdMoveTo;

              Exit;
            end;

            if FSourceVertex = 2 then
            begin
              Result := CAggPathCmdLineTo;

              Exit;
            end;

            Cmd := CAggPathCmdStop;

            goto _next;
          end;

          Cmd := CAggPathCmdMoveTo;

          FStatus := siPolygon;

          FSourceVertex := 0;

          goto _polygon;
        end;

      siPolygon:
      _polygon:
        begin
          if FCurrentAbscissa >= FMaxAbscissa then
            if FClosed <> 0 then
            begin
              FStatus := siEndPoly;

              goto _next;
            end
            else
            begin
              X^ := PPointDouble
                (FSourceVertices[FSourceVertices.Size - 1])^.X;
              Y^ := PPointDouble
                (FSourceVertices[FSourceVertices.Size - 1])^.Y;

              FStatus := siEndPoly;
              Result := CAggPathCmdLineTo;

              Exit;
            end;

          X^ := FSplineX.GetStateful(FCurrentAbscissa);
          Y^ := FSplineY.GetStateful(FCurrentAbscissa);

          Inc(FSourceVertex);

          FCurrentAbscissa := FCurrentAbscissa + FInterpolationStep;

          if FSourceVertex = 1 then
            Result := CAggPathCmdMoveTo
          else
            Result := CAggPathCmdLineTo;
          Exit;
        end;

      siEndPoly:
        begin
          FStatus := siStop;
          Result := CAggPathCmdEndPoly or FClosed;
          Exit;
        end;

      siStop:
        begin
          Result := CAggPathCmdStop;
          Exit;
        end;
    end;

  Result := Cmd;
end;

end.
