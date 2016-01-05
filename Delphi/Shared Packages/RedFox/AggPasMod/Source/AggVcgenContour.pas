unit AggVcgenContour;

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
  AggArray,
  AggMath,
  AggMathStroke,
  AggVertexSource,
  AggVertexSequence;

type
  TAggInternalStatus = (siInitial, siReady, siOutline, siOutVertices, siEndPoly,
    siStop);

  TAggVcgenContour = class(TAggVertexSource)
  private
    FSourceVertices: TAggVertexSequence;
    FOutVertices: TAggPodDeque;

    FWidth: Double;

    FLineJoin: TAggLineJoin;
    FInnerJoin: TAggInnerJoin;
    FApproxScale, FAbsWidth, FSignedWidth: Double;
    FMiterLimit, FInnerMiterLimit: Double;

    FStatus: TAggInternalStatus;

    FSourceVertex, FOutVertex, FClosed, FOrientation: Cardinal;
    FAutoDetect: Boolean;

    function GetWidth: Double;
    procedure SetApproximationScale(Value: Double);
    procedure SetAutoDetectOrientation(Value: Boolean);
    procedure SetInnerJoin(Value: TAggInnerJoin);
    procedure SetInnerMiterLimit(Value: Double);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetMiterLimit(Value: Double);
    procedure SetWidth(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMiterLimitTheta(Value: Double);

    // Generator interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property LineJoin: TAggLineJoin read FLineJoin write SetLineJoin;
    property InnerJoin: TAggInnerJoin read FInnerJoin write SetInnerJoin;
    property AutoDetectOrientation: Boolean read FAutoDetect write
      SetAutoDetectOrientation;

    property Width: Double read GetWidth write SetWidth;
    property MiterLimit: Double read FMiterLimit write SetMiterLimit;
    property InnerMiterLimit: Double read FInnerMiterLimit write
      SetInnerMiterLimit;
    property ApproximationScale: Double read FApproxScale write
      SetApproximationScale;
  end;

implementation


{ TAggVcgenContour }

constructor TAggVcgenContour.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));
  FOutVertices := TAggPodDeque.Create(SizeOf(TPointDouble));

  FWidth := 1;

  FLineJoin := ljBevel;
  FInnerJoin := ijMiter;
  FApproxScale := 1;
  FAbsWidth := 1;
  FSignedWidth := 1;
  FMiterLimit := 4;

  FInnerMiterLimit := 1 + 1 / 64;

  FStatus := siInitial;
  FSourceVertex := 0;
  FClosed := 0;
  FOrientation := 0;
  FAutoDetect := False;
end;

destructor TAggVcgenContour.Destroy;
begin
  FSourceVertices.Free;
  FOutVertices.Free;

  inherited;
end;

procedure TAggVcgenContour.SetLineJoin(Value: TAggLineJoin);
begin
  FLineJoin := Value;
end;

procedure TAggVcgenContour.SetInnerJoin(Value: TAggInnerJoin);
begin
  FInnerJoin := Value;
end;

procedure TAggVcgenContour.SetWidth(Value: Double);
begin
  FWidth := 0.5 * Value;
end;

procedure TAggVcgenContour.SetMiterLimit(Value: Double);
begin
  FMiterLimit := Value;
end;

procedure TAggVcgenContour.SetMiterLimitTheta(Value: Double);
begin
  FMiterLimit := 1 / Sin(Value * 0.5);
end;

procedure TAggVcgenContour.SetInnerMiterLimit(Value: Double);
begin
  FInnerMiterLimit := Value;
end;

procedure TAggVcgenContour.SetApproximationScale;
begin
  FApproxScale := Value;
end;

procedure TAggVcgenContour.SetAutoDetectOrientation(Value: Boolean);
begin
  FAutoDetect := Value;
end;

function TAggVcgenContour.GetWidth: Double;
begin
  Result := 2 * FWidth;
end;

procedure TAggVcgenContour.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FOrientation := 0;
  FAbsWidth := Abs(FWidth);
  FSignedWidth := FWidth;
  FStatus := siInitial;
end;

procedure TAggVcgenContour.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  Vd: TAggVertexDistance;
begin
  FStatus := siInitial;

  Vd.Pos := PointDouble(X, Y);
  Vd.Dist := 0;

  if IsMoveTo(Cmd) then
    FSourceVertices.ModifyLast(@Vd)
  else if IsVertex(Cmd) then
    FSourceVertices.Add(@Vd)
  else if IsEndPoly(Cmd) then
  begin
    FClosed := GetCloseFlag(Cmd);

    if FOrientation = CAggPathFlagsNone then
      FOrientation := GetOrientation(Cmd);
  end;
end;

procedure TAggVcgenContour.Rewind(PathID: Cardinal);
begin
  if FStatus = siInitial then
  begin
    FSourceVertices.Close(True);

    FSignedWidth := FWidth;

    if FAutoDetect then
      if not IsOriented(FOrientation) then
        if CalculatePolygonAreaVertexSequence(FSourceVertices) > 0.0 then
          FOrientation := CAggPathFlagsCcw
        else
          FOrientation := CAggPathFlagsCw;

    if IsOriented(FOrientation) then
      if IsCounterClockwise(FOrientation) then
        FSignedWidth := FWidth
      else
        FSignedWidth := -FWidth;
  end;

  FStatus := siReady;
  FSourceVertex := 0;
end;

function TAggVcgenContour.Vertex(X, Y: PDouble): Cardinal;
var
  C: PPointDouble;

label
  _ready, _Outline, _Out_vertices;

begin
  Result := CAggPathCmdLineTo;

  while not IsStop(Result) do
    case FStatus of
      siInitial:
        begin
          Rewind(0);

          goto _ready;
        end;

      siReady:
      _ready:
        begin
          if FSourceVertices.Size < 2 + Cardinal(FClosed <> 0) then
          begin
            Result := CAggPathCmdStop;
            Continue;
          end;

          FStatus := siOutline;

          Result := CAggPathCmdMoveTo;

          FSourceVertex := 0;
          FOutVertex := 0;

          goto _Outline;
        end;

      siOutline:
      _Outline:
        begin
          if FSourceVertex >= FSourceVertices.Size then
          begin
            FStatus := siEndPoly;
            Continue;
          end;

          StrokeCalcJoin(FOutVertices, FSourceVertices.Prev(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Next(FSourceVertex),
            PAggVertexDistance(FSourceVertices.Prev(FSourceVertex)).Dist,
            PAggVertexDistance(FSourceVertices.Curr(FSourceVertex)).Dist,
            FSignedWidth, FLineJoin, FInnerJoin, FMiterLimit,
            FInnerMiterLimit, FApproxScale);

          Inc(FSourceVertex);

          FStatus := siOutVertices;
          FOutVertex := 0;

          goto _Out_vertices;
        end;

      siOutVertices:
      _Out_vertices:
        if FOutVertex >= FOutVertices.Size then
          FStatus := siOutline
        else
        begin
          C := FOutVertices[FOutVertex];

          Inc(FOutVertex);

          X^ := C.X;
          Y^ := C.Y;

          Exit;
        end;

      siEndPoly:
        begin
          if FClosed = 0 then
          begin
            Result := CAggPathCmdStop;

            Exit;
          end;

          FStatus := siStop;
          Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

          Exit;
        end;

      siStop:
        begin
          Result := CAggPathCmdStop;

          Exit;
        end;
    end;
end;

end.
