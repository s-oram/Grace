unit AggVcgenStroke;

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
  AggArray,
  AggVertexSource,
  AggVertexSequence,
  AggMathStroke,
  AggShortenPath;

type
  TAggStatusEnum = (seInitial, seReady, seCap1, seCap2, seOutline1,
    seCloseFirst, seOutline2, seOutVertices, seEndPoly1, seEndPoly2, seStop);

  TAggVcgenStroke = class(TAggVertexSource)
  private
    FSourceVertices: TAggVertexSequence;
    FOutVertices: TAggPodDeque;
    FWidth, FMiterLimit, FInnerMiterLimit: Double;
    FApproxScale, FShorten: Double;
    FLineCap: TAggLineCap;
    FLineJoin: TAggLineJoin;
    FInnerJoin: TAggInnerJoin;
    FClosed: Cardinal;
    FStatus, FPrevStatus: TAggStatusEnum;
    FSourceVertex, FOutVertex: Cardinal;
    function GetWidth: Double;
    procedure SetApproximationScale(Value: Double);
    procedure SetInnerJoin(Value: TAggInnerJoin);
    procedure SetInnerMiterLimit(Value: Double);
    procedure SetLineCap(Value: TAggLineCap);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetMiterLimit(Value: Double);
    procedure SetShorten(Value: Double);
    procedure SetWidth(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMiterLimitTheta(Value: Double);

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property LineCap: TAggLineCap read FLineCap write SetLineCap;
    property LineJoin: TAggLineJoin read FLineJoin write SetLineJoin;
    property InnerJoin: TAggInnerJoin read FInnerJoin write SetInnerJoin;
    property Shorten: Double read FShorten write SetShorten;

    property Width: Double read GetWidth write SetWidth;
    property MiterLimit: Double read FMiterLimit write SetMiterLimit;
    property InnerMiterLimit: Double read FInnerMiterLimit write SetInnerMiterLimit;
    property ApproximationScale: Double read FApproxScale write SetApproximationScale;
  end;

  TAggVcgenStrokeMath = class(TAggVertexSource)
  private
    FStroker: TAggMathStroke;
    FSourceVertices: TAggVertexSequence;
    FOutVertices: TAggPodDeque;

    FShorten: Double;
    FClosed: Cardinal;

    FStatus: TAggStatusEnum;
    FPrevStatus: TAggStatusEnum;

    FSourceVertex: Cardinal;
    FOutVertex: Cardinal;

    function GetApproximationScale: Double;
    function GetInnerJoin: TAggInnerJoin;
    function GetInnerMiterLimit: Double;
    function GetLineCap: TAggLineCap;
    function GetLineJoin: TAggLineJoin;
    function GetMiterLimit: Double;
    function GetWidth: Double;
    procedure SetApproximationScale(Value: Double);
    procedure SetInnerJoin(Value: TAggInnerJoin);
    procedure SetInnerMiterLimit(Value: Double);
    procedure SetLineCap(Value: TAggLineCap);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetMiterLimit(Value: Double);
    procedure SetShorten(Value: Double);
    procedure SetWidth(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMiterLimitTheta(Value: Double);

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property LineCap: TAggLineCap read GetLineCap write SetLineCap;
    property LineJoin: TAggLineJoin read GetLineJoin write SetLineJoin;
    property InnerJoin: TAggInnerJoin read GetInnerJoin write SetInnerJoin;
    property Shorten: Double read FShorten write SetShorten;

    property Width: Double read GetWidth write SetWidth;
    property MiterLimit: Double read GetMiterLimit write SetMiterLimit;
    property InnerMiterLimit: Double read GetInnerMiterLimit write SetInnerMiterLimit;
    property ApproximationScale: Double read GetApproximationScale write SetApproximationScale;
  end;

implementation


{ TAggVcgenStroke }

constructor TAggVcgenStroke.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));
  FOutVertices := TAggPodDeque.Create(SizeOf(TPointDouble));

  FWidth := 0.5;
  FMiterLimit := 4.0;
  FInnerMiterLimit := 1.01;
  FApproxScale := 1.0;
  FShorten := 0.0;
  FLineCap := lcButt;
  FLineJoin:= ljMiter;
  FInnerJoin := ijMiter;
  FClosed := 0;
  FStatus := seInitial;
  FSourceVertex := 0;
  FOutVertex := 0;
end;

destructor TAggVcgenStroke.Destroy;
begin
  FSourceVertices.Free;
  FOutVertices.Free;
  inherited;
end;

procedure TAggVcgenStroke.SetLineCap(Value: TAggLineCap);
begin
  FLineCap := Value;
end;

procedure TAggVcgenStroke.SetLineJoin(Value: TAggLineJoin);
begin
  FLineJoin:= Value;
end;

procedure TAggVcgenStroke.SetMiterLimit(Value: Double);
begin
  FMiterLimit := Value;
end;

procedure TAggVcgenStroke.SetMiterLimitTheta(Value: Double);
begin
  FMiterLimit := 1.0 / Sin(Value * 0.5);
end;

procedure TAggVcgenStroke.SetApproximationScale(Value: Double);
begin
  FApproxScale := Value;
end;

procedure TAggVcgenStroke.SetInnerJoin(Value: TAggInnerJoin);
begin
  FInnerJoin := Value;
end;

procedure TAggVcgenStroke.SetInnerMiterLimit(Value: Double);
begin
  FInnerMiterLimit := Value;
end;

procedure TAggVcgenStroke.SetShorten(Value: Double);
begin
  FShorten := Value;
end;

procedure TAggVcgenStroke.SetWidth(Value: Double);
begin
  FWidth := Value * 0.5;
end;

function TAggVcgenStroke.GetWidth: Double;
begin
  Result := 2 * FWidth;
end;

procedure TAggVcgenStroke.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FStatus := seInitial;
end;

procedure TAggVcgenStroke.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  Vd: TAggVertexDistance;
begin
  FStatus := seInitial;

  Vd.Pos.X := X;
  Vd.Pos.Y := Y;

  Vd.Dist := 0;

  if IsMoveTo(Cmd) then
    FSourceVertices.ModifyLast(@Vd)
  else if IsVertex(Cmd) then
    FSourceVertices.Add(@Vd)
  else
    FClosed := GetCloseFlag(Cmd);
end;

procedure CalculateButtCap(Cap: PDoubleArray4; V0, V1: PAggVertexDistance;
  Len, Width: Double);
var
  Dx, Dy, Temp: Double;
begin
  Temp := Width / Len;
  Dx := (V1.Pos.Y - V0.Pos.Y) * Temp;
  Dy := (V1.Pos.X - V0.Pos.X) * Temp;

  Cap^[0] := V0.Pos.X - Dx;
  Cap^[1] := V0.Pos.Y + Dy;
  Cap^[2] := V0.Pos.X + Dx;
  Cap^[3] := V0.Pos.Y - Dy;
end;

procedure TAggVcgenStroke.Rewind(PathID: Cardinal);
begin
  if FStatus = seInitial then
  begin
    FSourceVertices.Close(Boolean(FClosed <> 0));

    ShortenPath(FSourceVertices, FShorten, FClosed);

    if FSourceVertices.Size < 3 then
      FClosed := 0;
  end;

  FStatus := seReady;

  FSourceVertex := 0;
  FOutVertex := 0;
end;

function TAggVcgenStroke.Vertex(X, Y: PDouble): Cardinal;
var
  C: PPointDouble;
  Cmd: Cardinal;

label
  _rdy, Out2, _end;
begin
  Cmd := CAggPathCmdLineTo;

  while not IsStop(Cmd) do
  begin
    case FStatus of
      seInitial:
        begin
          Rewind(0);

          goto _rdy;
        end;

      seReady:
        begin
        _rdy:
          if FSourceVertices.Size < 2 + Cardinal(FClosed <> 0) then
          begin
            Cmd := CAggPathCmdStop;
            goto _end;
          end;

          if (FClosed <> 0) then
            FStatus := seOutline1
          else
            FStatus := seCap1;

          Cmd := CAggPathCmdMoveTo;

          FSourceVertex := 0;
          FOutVertex := 0;
        end;

      seCap1:
        begin
          StrokeCalcCap(FOutVertices, FSourceVertices[0],
            FSourceVertices[1],
            PAggVertexDistance(FSourceVertices[0])^.Dist, FLineCap,
            FWidth, FApproxScale);

          FSourceVertex := 1;
          FPrevStatus := seOutline1;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seCap2:
        begin
          StrokeCalcCap(FOutVertices,
            FSourceVertices[FSourceVertices.Size - 1],
            FSourceVertices[FSourceVertices.Size - 2],
            PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2])^.Dist,
              FLineCap, FWidth, FApproxScale);

          FPrevStatus := seOutline2;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seOutline1:
        begin
          if FClosed <> 0 then
            if FSourceVertex >= FSourceVertices.Size then
            begin
              FPrevStatus := seCloseFirst;
              FStatus := seEndPoly1;
              goto _end;

            end
            else
          else if FSourceVertex >= FSourceVertices.Size - 1 then
          begin
            FStatus := seCap2;
            goto _end;
          end;

          StrokeCalcJoin(FOutVertices, FSourceVertices.Prev(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Next(FSourceVertex),
            PAggVertexDistance(FSourceVertices.Prev(FSourceVertex))^.Dist,
            PAggVertexDistance(FSourceVertices.Curr(FSourceVertex))^.Dist,
            FWidth, FLineJoin, FInnerJoin, FMiterLimit, FInnerMiterLimit,
            FApproxScale);

          Inc(FSourceVertex);

          FPrevStatus := FStatus;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seCloseFirst:
        begin
          FStatus := seOutline2;

          Cmd := CAggPathCmdMoveTo;

          goto Out2;
        end;

      seOutline2:
        begin
        Out2:
          if FSourceVertex <= Cardinal(FClosed = 0) then
          begin
            FStatus := seEndPoly2;
            FPrevStatus := seStop;
            goto _end;
          end;

          Dec(FSourceVertex);

          StrokeCalcJoin(FOutVertices, FSourceVertices.Next(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Prev(FSourceVertex),
            PAggVertexDistance(FSourceVertices.Curr(FSourceVertex))^.Dist,
            PAggVertexDistance(FSourceVertices.Prev(FSourceVertex))^.Dist,
            FWidth, FLineJoin, FInnerJoin, FMiterLimit, FInnerMiterLimit,
            FApproxScale);

          FPrevStatus := FStatus;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seOutVertices:
        if FOutVertex >= FOutVertices.Size then
          FStatus := FPrevStatus

        else
        begin
          C := FOutVertices[FOutVertex];

          Inc(FOutVertex);

          X^ := C.X;
          Y^ := C.Y;

          Result := Cmd;

          Exit;
        end;

      seEndPoly1:
        begin
          FStatus := FPrevStatus;

          Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

          Exit;
        end;

      seEndPoly2:
        begin
          FStatus := FPrevStatus;

          Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCw;

          Exit;
        end;

      seStop:
        Cmd := CAggPathCmdStop;
    end;

  _end:
  end;

  Result := Cmd;
end;


{ TAggVcgenStrokeMath }

constructor TAggVcgenStrokeMath.Create;
begin
  FStroker := TAggMathStroke.Create;
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));
  FOutVertices := TAggPodDeque.Create(SizeOf(TPointDouble));

  FShorten := 0.0;
  FClosed := 0;
  FStatus := seInitial;

  FSourceVertex := 0;
  FOutVertex := 0;
end;

destructor TAggVcgenStrokeMath.Destroy;
begin
  FStroker.Free;
  FSourceVertices.Free;
  FOutVertices.Free;
  inherited;
end;

procedure TAggVcgenStrokeMath.SetLineCap(Value: TAggLineCap);
begin
  FStroker.LineCap := Value;
end;

procedure TAggVcgenStrokeMath.SetLineJoin(Value: TAggLineJoin);
begin
  FStroker.LineJoin := Value;
end;

procedure TAggVcgenStrokeMath.SetInnerJoin(Value: TAggInnerJoin);
begin
  FStroker.InnerJoin := Value;
end;

function TAggVcgenStrokeMath.GetLineCap: TAggLineCap;
begin
  Result := FStroker.LineCap;
end;

function TAggVcgenStrokeMath.GetLineJoin: TAggLineJoin;
begin
  Result := FStroker.LineJoin;
end;

function TAggVcgenStrokeMath.GetInnerJoin: TAggInnerJoin;
begin
  Result := FStroker.InnerJoin;
end;

procedure TAggVcgenStrokeMath.SetWidth(Value: Double);
begin
  FStroker.Width := Value;
end;

procedure TAggVcgenStrokeMath.SetMiterLimit(Value: Double);
begin
  FStroker.MiterLimit := Value;
end;

procedure TAggVcgenStrokeMath.SetMiterLimitTheta(Value: Double);
begin
  FStroker.SetMiterLimitTheta(Value);
end;

procedure TAggVcgenStrokeMath.SetInnerMiterLimit(Value: Double);
begin
  FStroker.InnerMiterLimit := Value;
end;

procedure TAggVcgenStrokeMath.SetApproximationScale(Value: Double);
begin
  FStroker.ApproximationScale := Value;
end;

function TAggVcgenStrokeMath.GetWidth: Double;
begin
  Result := FStroker.Width;
end;

function TAggVcgenStrokeMath.GetMiterLimit: Double;
begin
  Result := FStroker.MiterLimit;
end;

function TAggVcgenStrokeMath.GetInnerMiterLimit: Double;
begin
  Result := FStroker.InnerMiterLimit;
end;

function TAggVcgenStrokeMath.GetApproximationScale: Double;
begin
  Result := FStroker.ApproximationScale;
end;

procedure TAggVcgenStrokeMath.SetShorten(Value: Double);
begin
  FShorten := Value;
end;

procedure TAggVcgenStrokeMath.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FStatus := seInitial;
end;

procedure TAggVcgenStrokeMath.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  Vd: TAggVertexDistance;
begin
  FStatus := seInitial;

  Vd.Pos := PointDouble(X, Y);
  Vd.Dist := 0;

  if IsMoveTo(Cmd) then
    FSourceVertices.ModifyLast(@Vd)
  else if IsVertex(Cmd) then
    FSourceVertices.Add(@Vd)
  else
    FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenStrokeMath.Rewind(PathID: Cardinal);
begin
  if FStatus = seInitial then
  begin
    FSourceVertices.Close(Boolean(FClosed <> 0));

    ShortenPath(FSourceVertices, FShorten, FClosed);

    if FSourceVertices.Size < 3 then
      FClosed := 0;
  end;

  FStatus := seReady;

  FSourceVertex := 0;
  FOutVertex := 0;
end;

function TAggVcgenStrokeMath.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;

  C: PPointDouble;

label
  _rdy, Out2, _end;

begin
  Cmd := CAggPathCmdLineTo;

  while not IsStop(Cmd) do
  begin
    case FStatus of
      seInitial:
        begin
          Rewind(0);

          goto _rdy;
        end;

      seReady:
        begin
        _rdy:
          if FSourceVertices.Size < 2 + Cardinal(FClosed <> 0) then
          begin
            Cmd := CAggPathCmdStop;

            goto _end;
          end;

          if (FClosed <> 0) then
            FStatus := seOutline1
          else
            FStatus := seCap1;

          Cmd := CAggPathCmdMoveTo;

          FSourceVertex := 0;
          FOutVertex := 0;
        end;

      seCap1:
        begin
          FStroker.CalculateCap(FOutVertices, FSourceVertices[0],
            FSourceVertices[1], PAggVertexDistance(FSourceVertices[0])^.Dist);

          FSourceVertex := 1;
          FPrevStatus := seOutline1;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seCap2:
        begin
          FStroker.CalculateCap(FOutVertices,
            FSourceVertices[FSourceVertices.Size - 1],
            FSourceVertices[FSourceVertices.Size - 2],
            PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2])^.Dist);

          FPrevStatus := seOutline2;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seOutline1:
        begin
          if FClosed <> 0 then
            if FSourceVertex >= FSourceVertices.Size then
            begin
              FPrevStatus := seCloseFirst;
              FStatus := seEndPoly1;
              goto _end;
            end
            else
          else if FSourceVertex >= FSourceVertices.Size - 1 then
          begin
            FStatus := seCap2;
            goto _end;
          end;

          FStroker.CalculateJoin(FOutVertices,
            FSourceVertices.Prev(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Next(FSourceVertex),
            PAggVertexDistance(FSourceVertices.Prev(FSourceVertex))^.Dist,
            PAggVertexDistance(FSourceVertices.Curr(FSourceVertex))^.Dist);

          Inc(FSourceVertex);

          FPrevStatus := FStatus;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seCloseFirst:
        begin
          FStatus := seOutline2;

          Cmd := CAggPathCmdMoveTo;

          goto Out2;
        end;

      seOutline2:
        begin
        Out2:
          if FSourceVertex <= Cardinal(FClosed = 0) then
          begin
            FStatus := seEndPoly2;
            FPrevStatus := seStop;
            goto _end;
          end;

          Dec(FSourceVertex);

          FStroker.CalculateJoin(FOutVertices,
            FSourceVertices.Next(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Prev(FSourceVertex),
            PAggVertexDistance(FSourceVertices.Curr(FSourceVertex))^.Dist,
            PAggVertexDistance(FSourceVertices.Prev(FSourceVertex))^.Dist);

          FPrevStatus := FStatus;
          FStatus := seOutVertices;
          FOutVertex := 0;
        end;

      seOutVertices:
        if FOutVertex >= FOutVertices.Size then
          FStatus := FPrevStatus

        else
        begin
          C := FOutVertices[FOutVertex];

          Inc(FOutVertex);

          X^ := C.X;
          Y^ := C.Y;

          Result := Cmd;

          Exit;
        end;

      seEndPoly1:
        begin
          FStatus := FPrevStatus;

          Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

          Exit;
        end;

      seEndPoly2:
        begin
          FStatus := FPrevStatus;

          Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCw;

          Exit;
        end;

      seStop:
        Cmd := CAggPathCmdStop;
    end;

  _end:
  end;

  Result := Cmd;
end;

end.
