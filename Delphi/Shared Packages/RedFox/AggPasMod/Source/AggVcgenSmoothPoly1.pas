unit AggVcgenSmoothPoly1;

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
  AggVertexSource,
  AggVertexSequence;

type
  TAggStatusEnum = (seInitial, seReady, sePolygon, seCtrlB, seCtrlE, seCtrl1,
    seCtrl2, seEndPoly, seStop);

  TAggVcgenSmoothPoly1 = class(TAggVertexSource)
  private
    FSourceVertices: TAggVertexSequence;
    FSmoothValue: Double;

    FClosed: Cardinal;
    FStatus: TAggStatusEnum;

    FSourceVertex: Cardinal;

    FControl: array [0..1] of TPointDouble;
    procedure SetSmoothValue(V: Double);
    function GetSmoothValue: Double;
  protected
    procedure Calculate(V0, V1, V2, V3: PAggVertexDistance);
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property SmoothValue: Double read GetSmoothValue write SetSmoothValue;
  end;

implementation

{ TAggVcgenSmoothPoly1 }

constructor TAggVcgenSmoothPoly1.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance), 6);

  FSmoothValue := 0.5;

  FClosed := 0;
  FStatus := seInitial;

  FSourceVertex := 0;
end;

destructor TAggVcgenSmoothPoly1.Destroy;
begin
  FSourceVertices.Free;

  inherited;
end;

procedure TAggVcgenSmoothPoly1.SetSmoothValue;
begin
  FSmoothValue := V * 0.5;
end;

function TAggVcgenSmoothPoly1.GetSmoothValue;
begin
  Result := 2 * FSmoothValue;
end;

procedure TAggVcgenSmoothPoly1.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FStatus := seInitial;
end;

procedure TAggVcgenSmoothPoly1.AddVertex;
var
  Vd: TAggVertexDistance;
begin
  FStatus := seInitial;

  if IsMoveTo(Cmd) then
  begin
    Vd.Pos := PointDouble(X, Y);
    Vd.Dist := 0;

    FSourceVertices.ModifyLast(@Vd);
  end
  else if IsVertex(Cmd) then
  begin
    Vd.Pos := PointDouble(X, Y);
    Vd.Dist := 0;

    FSourceVertices.Add(@Vd);
  end
  else
    FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenSmoothPoly1.Rewind(PathID: Cardinal);
begin
  if FStatus = seInitial then
    FSourceVertices.Close(Boolean(FClosed <> 0));

  FStatus := seReady;
  FSourceVertex := 0;
end;

function TAggVcgenSmoothPoly1.Vertex(X, Y: PDouble): Cardinal;
label
  _ready, _polygon;
begin
  Result := CAggPathCmdLineTo;

  while not IsStop(Result) do
    case FStatus of
      seInitial:
        begin
          Rewind(0);

          goto _ready;
        end;

      seReady:
      _ready:
        begin
          if FSourceVertices.Size < 2 then
          begin
            Result := CAggPathCmdStop;
            Continue;
          end;

          if FSourceVertices.Size = 2 then
          begin
            X^ := PAggVertexDistance
              (FSourceVertices[FSourceVertex]).Pos.X;
            Y^ := PAggVertexDistance
              (FSourceVertices[FSourceVertex]).Pos.Y;

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

            Result := CAggPathCmdStop;
            Continue;
          end;

          Result := CAggPathCmdMoveTo;

          FStatus := sePolygon;

          FSourceVertex := 0;

          goto _polygon;
        end;

      sePolygon:
      _polygon:
        begin
          if FClosed <> 0 then
            if FSourceVertex >= FSourceVertices.Size then
            begin
              X^ := PAggVertexDistance(FSourceVertices[0]).Pos.X;
              Y^ := PAggVertexDistance(FSourceVertices[0]).Pos.Y;

              FStatus := seEndPoly;
              Result := CAggPathCmdCurve4;

              Exit;
            end
            else
          else if FSourceVertex >= FSourceVertices.Size - 1 then
          begin
            X^ := PAggVertexDistance
              (FSourceVertices[FSourceVertices.Size - 1]).Pos.X;
            Y^ := PAggVertexDistance
              (FSourceVertices[FSourceVertices.Size - 1]).Pos.Y;

            FStatus := seEndPoly;
            Result := CAggPathCmdCurve3;

            Exit;
          end;

          Calculate(FSourceVertices.Prev(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Next(FSourceVertex),
            FSourceVertices.Next(FSourceVertex + 1));

          X^ := PAggVertexDistance(FSourceVertices[FSourceVertex]).Pos.X;
          Y^ := PAggVertexDistance(FSourceVertices[FSourceVertex]).Pos.Y;

          Inc(FSourceVertex);

          if FClosed <> 0 then
          begin
            FStatus := seCtrl1;

            if FSourceVertex = 1 then
              Result := CAggPathCmdMoveTo
            else
              Result := CAggPathCmdCurve4;
            Exit;
          end
          else
          begin
            if FSourceVertex = 1 then
            begin
              FStatus := seCtrlB;
              Result := CAggPathCmdMoveTo;
              Exit;
            end;

            if FSourceVertex >= FSourceVertices.Size - 1 then
            begin
              FStatus := seCtrlE;
              Result := CAggPathCmdCurve3;
              Exit;
            end;

            FStatus := seCtrl1;
            Result := CAggPathCmdCurve4;
            Exit;
          end;
        end;

      seCtrlB:
        begin
          X^ := FControl[1].X;
          Y^ := FControl[1].Y;

          FStatus := sePolygon;
          Result := CAggPathCmdCurve3;
          Exit;
        end;

      seCtrlE:
        begin
          X^ := FControl[0].X;
          Y^ := FControl[0].Y;

          FStatus := sePolygon;
          Result := CAggPathCmdCurve3;
          Exit;
        end;

      seCtrl1:
        begin
          X^ := FControl[0].X;
          Y^ := FControl[0].Y;

          FStatus := seCtrl2;
          Result := CAggPathCmdCurve4;
          Exit;
        end;

      seCtrl2:
        begin
          X^ := FControl[1].X;
          Y^ := FControl[1].Y;

          FStatus := sePolygon;
          Result := CAggPathCmdCurve4;
          Exit;
        end;

      seEndPoly:
        begin
          FStatus := seStop;
          Result := CAggPathCmdEndPoly or FClosed;
          Exit;
        end;

      seStop:
        begin
          Result := CAggPathCmdStop;
          Exit;
        end;
    end;
end;

procedure TAggVcgenSmoothPoly1.Calculate;
var
  K1, K2, Xm1, Ym1, Xm2, Ym2: Double;
begin
  K1 := V0.Dist / (V0.Dist + V1.Dist);
  K2 := V1.Dist / (V1.Dist + V2.Dist);

  Xm1 := V0.Pos.X + (V2.Pos.X - V0.Pos.X) * K1;
  Ym1 := V0.Pos.Y + (V2.Pos.Y - V0.Pos.Y) * K1;
  Xm2 := V1.Pos.X + (V3.Pos.X - V1.Pos.X) * K2;
  Ym2 := V1.Pos.Y + (V3.Pos.Y - V1.Pos.Y) * K2;

  FControl[0].X := V1.Pos.X + FSmoothValue * (V2.Pos.X - Xm1);
  FControl[0].Y := V1.Pos.Y + FSmoothValue * (V2.Pos.Y - Ym1);
  FControl[1].X := V2.Pos.X + FSmoothValue * (V1.Pos.X - Xm2);
  FControl[1].Y := V2.Pos.Y + FSmoothValue * (V1.Pos.Y - Ym2);
end;

end.
