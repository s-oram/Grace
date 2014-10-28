unit AggVcgenMarkersTerm;

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
  AggVertexSequence;

type
  TAggVcgenMarkersTerm = class(TAggVertexSource)
  private
    FMarkers: TAggPodDeque;
    FCurrentID, FCurrentIndex: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation


{ TAggVcgenMarkersTerm }

constructor TAggVcgenMarkersTerm.Create;
begin
  FMarkers := TAggPodDeque.Create(SizeOf(TPointDouble), 6);

  FCurrentID := 0;
  FCurrentIndex := 0;
end;

destructor TAggVcgenMarkersTerm.Destroy;
begin
  FMarkers.Free;

  inherited
end;

procedure TAggVcgenMarkersTerm.RemoveAll;
begin
  FMarkers.RemoveAll;
end;

procedure TAggVcgenMarkersTerm.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  Ct: TPointDouble;
begin
  if IsMoveTo(Cmd) then
    if FMarkers.Size and 1 <> 0 then
    begin
      // Initial state, the first coordinate was added.
      // If two of more calls of StartVertex() occures
      // we just modify the last one.
      Ct := PointDouble(X, Y);
      FMarkers.ModifyLast(@Ct);
    end
    else
    begin
      Ct := PointDouble(X, Y);
      FMarkers.Add(@Ct);
    end
  else if IsVertex(Cmd) then
    if FMarkers.Size and 1 <> 0 then
    begin
      // Initial state, the first coordinate was added.
      // Add three more points, 0,1,1,0
      Ct := PointDouble(X, Y);
      FMarkers.Add(@Ct);
      FMarkers.Add(FMarkers[FMarkers.Size - 1]);
      FMarkers.Add(FMarkers[FMarkers.Size - 3]);
    end
    else if FMarkers.Size <> 0 then
    begin
      // Replace two last points: 0,1,1,0 -> 0,1,2,1
      Ct := PointDouble(X, Y);

      Move(FMarkers[FMarkers.Size - 2]^,
        FMarkers[FMarkers.Size - 1]^, SizeOf(TPointDouble));

      Move(Ct, FMarkers[FMarkers.Size - 2]^,
        SizeOf(TPointDouble));
    end;
end;

procedure TAggVcgenMarkersTerm.Rewind(PathID: Cardinal);
begin
  FCurrentID := PathID * 2;
  FCurrentIndex := FCurrentID;
end;

function TAggVcgenMarkersTerm.Vertex(X, Y: PDouble): Cardinal;
var
  C: PPointDouble;
begin
  if (FCurrentID > 2) or (FCurrentIndex >= FMarkers.Size) then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  C := FMarkers[FCurrentIndex];

  X^ := C.X;
  Y^ := C.Y;

  if FCurrentIndex and 1 <> 0 then
  begin
    Inc(FCurrentIndex, 3);

    Result := CAggPathCmdLineTo;

    Exit;
  end;

  Inc(FCurrentIndex);

  Result := CAggPathCmdMoveTo;
end;

end.
