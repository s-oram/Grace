unit AggTransSinglePath;

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
  AggTransAffine;

type
  TAggInternalStatus = (siInitial, siMakingPath, siReady);

  TAggTransSinglePath = class(TAggTransAffine)
  private
    FSourceVertices: TAggVertexSequence;
    FBaseLength, FKIndex: Double;

    FStatus: TAggInternalStatus;

    FPreserveXScale: Boolean;

    procedure SetBaseLength(V: Double);
    procedure SetPreserveXScale(F: Boolean);
    function GetTotalLength: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; virtual;

    procedure MoveTo(X, Y: Double);
    procedure LineTo(X, Y: Double);
    procedure FinalizePath;

    procedure AddPath(Vs: TAggVertexSource; PathID: Cardinal = 0);

    property TotalLength: Double read GetTotalLength;
    property BaseLength: Double read FBaseLength write SetBaseLength;
    property PreserveXScale: Boolean read FPreserveXScale write SetPreserveXScale;
  end;

implementation


procedure SinglePathTransform(This: TAggTransSinglePath; X, Y: PDouble);
var
  Rect: TRectDouble;
  Delta: TPointDouble;
  D, Dd: Double;
  I, J, K: Cardinal;
begin
  if This.FStatus = siReady then
  begin
    if This.FBaseLength > 1E-10 then
      X^ := X^ * (PAggVertexDistance(This.FSourceVertices[
        This.FSourceVertices.Size - 1]).Dist / This.FBaseLength);

    Rect.X1 := 0;
    Rect.Y1 := 0;
    Delta.X := 1;
    Delta.Y := 1;
    D := 0;
    Dd := 1;

    if X^ < 0 then
    begin
      // Extrapolation on the left
      Rect.X1 := PAggVertexDistance(This.FSourceVertices[0]).Pos.X;
      Rect.Y1 := PAggVertexDistance(This.FSourceVertices[0]).Pos.Y;
      Delta.X := PAggVertexDistance(This.FSourceVertices[1]).Pos.X - Rect.X1;
      Delta.Y := PAggVertexDistance(This.FSourceVertices[1]).Pos.Y - Rect.Y1;

      Dd := PAggVertexDistance(This.FSourceVertices[1]).Dist -
        PAggVertexDistance(This.FSourceVertices[0]).Dist;

      D := X^;
    end
    else if X^ > PAggVertexDistance(This.FSourceVertices[
      This.FSourceVertices.Size - 1]).Dist then
    begin
      // Extrapolation on the right
      I := This.FSourceVertices.Size - 2;
      J := This.FSourceVertices.Size - 1;

      Rect.X1 := PAggVertexDistance(This.FSourceVertices[J]).Pos.X;
      Rect.Y1 := PAggVertexDistance(This.FSourceVertices[J]).Pos.Y;
      Delta.X := Rect.X1 - PAggVertexDistance(This.FSourceVertices[I]).Pos.X;
      Delta.Y := Rect.Y1 - PAggVertexDistance(This.FSourceVertices[I]).Pos.Y;

      Dd := PAggVertexDistance(This.FSourceVertices[J]).Dist -
        PAggVertexDistance(This.FSourceVertices[I]).Dist;

      D := X^ - PAggVertexDistance(This.FSourceVertices[J]).Dist;
    end
    else
    begin
      // Interpolation
      I := 0;
      J := This.FSourceVertices.Size - 1;

      if This.FPreserveXScale then
      begin
        I := 0;

        while J - I > 1 do
        begin
          K := (I + J) shr 1;

          if X^ < PAggVertexDistance(This.FSourceVertices[K]).Dist
          then
            J := K
          else
            I := K;
        end;

        D := PAggVertexDistance(This.FSourceVertices[I]).Dist;
        Dd := PAggVertexDistance(This.FSourceVertices[J]).Dist - D;
        D := X^ - D;
      end
      else
      begin
        I := Trunc(X^ * This.FKIndex);
        J := I + 1;

        Dd := PAggVertexDistance(This.FSourceVertices[J]).Dist -
          PAggVertexDistance(This.FSourceVertices[I]).Dist;

        D := ((X^ * This.FKIndex) - I) * Dd;
      end;

      Rect.X1 := PAggVertexDistance(This.FSourceVertices[I]).Pos.X;
      Rect.Y1 := PAggVertexDistance(This.FSourceVertices[I]).Pos.Y;
      Delta.X := PAggVertexDistance(This.FSourceVertices[J]).Pos.X - Rect.X1;
      Delta.Y := PAggVertexDistance(This.FSourceVertices[J]).Pos.Y - Rect.Y1;
    end;

    Dd := 1 / Dd;
    Rect.X2 := Rect.X1 + Delta.X * D * Dd;
    Rect.Y2 := Rect.Y1 + Delta.Y * D * Dd;
    X^ := Rect.X2 - Y^ * Delta.Y * Dd;
    Y^ := Rect.Y2 + Y^ * Delta.X * Dd;
  end;
end;


{ TAggTransSinglePath }

constructor TAggTransSinglePath.Create;
begin
  inherited Create;

  Transform := @SinglePathTransform;

  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));

  FBaseLength := 0.0;
  FKIndex := 0.0;

  FStatus := siInitial;

  FPreserveXScale := True;
end;

destructor TAggTransSinglePath.Destroy;
begin
  FSourceVertices.Free;
  inherited
end;

procedure TAggTransSinglePath.SetBaseLength(V: Double);
begin
  FBaseLength := V;
end;

procedure TAggTransSinglePath.SetPreserveXScale(F: Boolean);
begin
  FPreserveXScale := F;
end;

procedure TAggTransSinglePath.Reset;
begin
  FSourceVertices.RemoveAll;

  FKIndex := 0.0;
  FStatus := siInitial;
end;

procedure TAggTransSinglePath.MoveTo(X, Y: Double);
var
  Vd: TAggVertexDistance;
begin
  if FStatus = siInitial then
  begin
    Vd.Pos := PointDouble(X, Y);
    Vd.Dist := 0;

    FSourceVertices.ModifyLast(@Vd);

    FStatus := siMakingPath;
  end
  else
    LineTo(X, Y);
end;

procedure TAggTransSinglePath.LineTo(X, Y: Double);
var
  Vd: TAggVertexDistance;
begin
  if FStatus = siMakingPath then
  begin
    Vd.Pos := PointDouble(X, Y);
    Vd.Dist := 0;

    FSourceVertices.Add(@Vd);
  end;
end;

procedure TAggTransSinglePath.FinalizePath;
var
  I: Cardinal;
  V: PAggVertexDistance;
  Dist, D: Double;
begin
  if (FStatus = siMakingPath) and (FSourceVertices.Size > 1) then
  begin
    FSourceVertices.Close(False);

    if FSourceVertices.Size > 2 then
      if PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2]).Dist * 10
        < PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 3]).Dist
        then
      begin
        D := PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 3]).Dist
          + PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2]).Dist;

        Move(FSourceVertices[FSourceVertices.Size - 1]^,
          FSourceVertices[FSourceVertices.Size - 2]^,
          SizeOf(TAggVertexDistance));

        FSourceVertices.RemoveLast;

        PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2]).Dist := D;
      end;

    Dist := 0.0;

    for I := 0 to FSourceVertices.Size - 1 do
    begin
      V := FSourceVertices[I];
      D := V.Dist;

      V.Dist := Dist;
      Dist := Dist + D;
    end;

    FKIndex := (FSourceVertices.Size - 1) / Dist;
    FStatus := siReady;
  end;
end;

procedure TAggTransSinglePath.AddPath;
var
  X, Y: Double;
  Cmd : Cardinal;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsMoveTo(Cmd) then
      MoveTo(X, Y)
    else if IsVertex(Cmd) then
      LineTo(X, Y);

    Cmd := Vs.Vertex(@X, @Y);
  end;

  FinalizePath;
end;

function TAggTransSinglePath.GetTotalLength;
begin
  if FBaseLength >= 1E-10 then
  begin
    Result := FBaseLength;
    Exit;
  end;

  if FStatus = siReady then
    Result := PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 1]).Dist
  else
    Result := 0.0
end;

end.
