unit AggVcgenVertexSequence;

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
  AggVertexSequence,
  AggShortenPath,
  AggVertexSource;

type
  TAggVcgenVertexSequence = class(TAggVertexSource)
  private
    FSourceVertices: TAggVertexSequence;
    FFlags, FCurrentVertex: Cardinal;

    FShorten: Double;
    FReady: Boolean;

    procedure SetShorten(S: Double);
    function GetShorten: Double;
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Shorten: Double read GetShorten write SetShorten;
  end;

implementation


{ TAggVcgenVertexSequence }

constructor TAggVcgenVertexSequence.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistCmd), 6);

  FFlags := 0;
  FCurrentVertex := 0;
  FShorten := 0.0;
  FReady := False;
end;

destructor TAggVcgenVertexSequence.Destroy;
begin
  FSourceVertices.Free;

  inherited;
end;

procedure TAggVcgenVertexSequence.SetShorten(S: Double);
begin
  FShorten := S;
end;

function TAggVcgenVertexSequence.GetShorten: Double;
begin
  Result := FShorten;
end;

procedure TAggVcgenVertexSequence.RemoveAll;
begin
  FReady := False;

  FSourceVertices.RemoveAll;

  FCurrentVertex := 0;
  FFlags := 0;
end;

procedure TAggVcgenVertexSequence.AddVertex;
var
  Vc: TAggVertexDistCmd;
begin
  FReady := False;

  Vc.Pos := PointDouble(X, Y);
  Vc.Dist := 0;
  Vc.Cmd := Cmd;

  if IsMoveTo(Cmd) then
    FSourceVertices.ModifyLast(@Vc)
  else if IsVertex(Cmd) then
    FSourceVertices.Add(@Vc)
  else
    FFlags := Cmd and CAggPathFlagsMask;
end;

procedure TAggVcgenVertexSequence.Rewind(PathID: Cardinal);
begin
  if not FReady then
  begin
    FSourceVertices.Close(IsClosed(FFlags));

    ShortenPath(FSourceVertices, FShorten, GetCloseFlag(FFlags));
  end;

  FReady := True;
  FCurrentVertex := 0;
end;

function TAggVcgenVertexSequence.Vertex(X, Y: PDouble): Cardinal;
var
  V: PAggVertexDistCmd;
begin
  if not FReady then
    Rewind(0);

  if FCurrentVertex = FSourceVertices.Size then
  begin
    Inc(FCurrentVertex);

    Result := CAggPathCmdEndPoly or FFlags;

    Exit;
  end;

  if FCurrentVertex > FSourceVertices.Size then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  V := FSourceVertices[FCurrentVertex];

  Inc(FCurrentVertex);

  X^ := V.Pos.X;
  Y^ := V.Pos.Y;

  Result := V.Cmd;
end;

end.
