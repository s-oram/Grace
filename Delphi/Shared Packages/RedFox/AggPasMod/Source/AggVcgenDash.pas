unit AggVcgenDash;

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
  AggVertexSource,
  AggVertexSequence,
  AggShortenPath;

const
  CMaxDashes = 32;

type
  TAggInternalStatus = (siInitial, siReady, siPolyline, siStop);

  TAggVcgenDash = class(TAggVertexSource)
  private
    FDashes: array [0..CMaxDashes - 1] of Double;

    FTotalDashLen: Double;
    FNumDashes: Cardinal;
    FDashStart, FShorten, FCurrDashStart: Double;

    FCurrentDash: Cardinal;
    FCurrentRest: Double;

    FVertex1, FVertex2: PAggVertexDistance;

    FSourceVertices: TAggVertexSequence;

    FClosed: Cardinal;
    FStatus: TAggInternalStatus;

    FSourceVertex: Cardinal;

    function GetShorten: Double;
    procedure SetShorten(Value: Double);
    procedure SetDashStart(Value: Double);
  protected
    procedure CalculateDashStart(Ds: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAllDashes;
    procedure AddDash(DashLength, GapLength: Double);

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Shorten: Double read GetShorten write SetShorten;
    property DashStart: Double read FDashStart write SetDashStart;
  end;

implementation


{ TAggVcgenDash }

constructor TAggVcgenDash.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));

  FTotalDashLen := 0.0;
  FNumDashes := 0;
  FDashStart := 0.0;
  FShorten := 0.0;
  FCurrDashStart := 0.0;
  FCurrentDash := 0;

  FClosed := 0;
  FStatus := siInitial;
  FSourceVertex := 0;
end;

destructor TAggVcgenDash.Destroy;
begin
  FSourceVertices.Free;
  inherited;
end;

procedure TAggVcgenDash.RemoveAllDashes;
begin
  FTotalDashLen := 0.0;
  FNumDashes := 0;
  FCurrDashStart := 0.0;
  FCurrentDash := 0;
end;

procedure TAggVcgenDash.AddDash;
begin
  if FNumDashes < CMaxDashes then
  begin
    FTotalDashLen := FTotalDashLen + DashLength + GapLength;

    FDashes[FNumDashes] := DashLength;

    Inc(FNumDashes);

    FDashes[FNumDashes] := GapLength;

    Inc(FNumDashes);
  end;
end;

procedure TAggVcgenDash.SetDashStart(Value: Double);
begin
  FDashStart := Value;

  CalculateDashStart(Abs(Value));
end;

procedure TAggVcgenDash.SetShorten(Value: Double);
begin
  FShorten := Value;
end;

function TAggVcgenDash.GetShorten: Double;
begin
  Result := FShorten;
end;

procedure TAggVcgenDash.RemoveAll;
begin
  FStatus := siInitial;

  FSourceVertices.RemoveAll;

  FClosed := 0;
end;

procedure TAggVcgenDash.AddVertex;
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
  else
    FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenDash.Rewind(PathID: Cardinal);
begin
  if FStatus = siInitial then
  begin
    FSourceVertices.Close(Boolean(FClosed <> 0));

    ShortenPath(FSourceVertices, FShorten, FClosed);
  end;

  FStatus := siReady;
  FSourceVertex := 0;
end;

function TAggVcgenDash.Vertex(X, Y: PDouble): Cardinal;
var
  DashRest, Temp: Double;
  Cmd: Cardinal;
label
  _next, _ready;

begin
  Cmd := CAggPathCmdMoveTo;

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
          if (FNumDashes < 2) or (FSourceVertices.Size < 2) then
          begin
            Cmd := CAggPathCmdStop;

            goto _next;
          end;

          FStatus := siPolyline;
          FSourceVertex := 1;

          FVertex1 := FSourceVertices[0];
          FVertex2 := FSourceVertices[1];

          FCurrentRest := FVertex1.Dist;

          X^ := FVertex1.Pos.X;
          Y^ := FVertex1.Pos.Y;

          if FDashStart >= 0.0 then
            CalculateDashStart(FDashStart);

          Result := CAggPathCmdMoveTo;

          Exit;
        end;

      siPolyline:
        begin
          DashRest := FDashes[FCurrentDash] - FCurrDashStart;

          if FCurrentDash and 1 <> 0 then
            Cmd := CAggPathCmdMoveTo
          else
            Cmd := CAggPathCmdLineTo;

          if FCurrentRest > DashRest then
          begin
            FCurrentRest := FCurrentRest - DashRest;

            Inc(FCurrentDash);

            if FCurrentDash >= FNumDashes then
              FCurrentDash := 0;

            FCurrDashStart := 0.0;

            Temp := FCurrentRest / FVertex1.Dist;
            X^ := FVertex2.Pos.X - (FVertex2.Pos.X - FVertex1.Pos.X) * Temp;
            Y^ := FVertex2.Pos.Y - (FVertex2.Pos.Y - FVertex1.Pos.Y) * Temp;
          end
          else
          begin
            FCurrDashStart := FCurrDashStart + FCurrentRest;

            X^ := FVertex2.Pos.X;
            Y^ := FVertex2.Pos.Y;

            Inc(FSourceVertex);

            FVertex1 := FVertex2;

            FCurrentRest := FVertex1.Dist;

            if FClosed <> 0 then
              if FSourceVertex > FSourceVertices.Size then
                FStatus := siStop
              else if FSourceVertex >= FSourceVertices.Size then
                FVertex2 := FSourceVertices[0]
              else
                FVertex2 := FSourceVertices[FSourceVertex]
            else if FSourceVertex >= FSourceVertices.Size then
              FStatus := siStop
            else
              FVertex2 := FSourceVertices[FSourceVertex];
          end;

          Result := Cmd;

          Exit;
        end;

      siStop:
        Cmd := CAggPathCmdStop;
    end;

  Result := CAggPathCmdStop;
end;

procedure TAggVcgenDash.CalculateDashStart;
begin
  FCurrentDash := 0;
  FCurrDashStart := 0.0;

  while Ds > 0.0 do
    if Ds > FDashes[FCurrentDash] then
    begin
      Ds := Ds - FDashes[FCurrentDash];

      Inc(FCurrentDash);

      FCurrDashStart := 0.0;

      if FCurrentDash >= FNumDashes then
        FCurrentDash := 0;

    end
    else
    begin
      FCurrDashStart := Ds;

      Ds := 0.0;
    end;
end;

end.
