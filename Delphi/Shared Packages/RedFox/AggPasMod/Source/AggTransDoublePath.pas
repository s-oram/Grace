unit AggTransDoublePath;

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

  TAggTransDoublePath = class(TAggTransAffine)
  private
    FSourceVertices: array [0..1] of TAggVertexSequence;

    FBaseLength, FBaseHeight: Double;
    FKIndex: array [0..1] of Double;

    FStatus: array [0..1] of TAggInternalStatus;

    FPreserveXScale: Boolean;

    procedure SetBaseLength(V: Double);
    function GetBaseLength: Double;

    procedure SetBaseHeight(V: Double);
    function GetBaseHeight: Double;

    procedure SetPreserveXScale(F: Boolean);
    function GetPreserveXScale: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; virtual;

    procedure MoveTo1(X, Y: Double);
    procedure LineTo1(X, Y: Double);
    procedure MoveTo2(X, Y: Double);
    procedure LineTo2(X, Y: Double);
    procedure FinalizePaths;

    procedure AddPaths(Vs1, Vs2: TAggVertexSource; Path1ID: Cardinal = 0;
      Path2ID: Cardinal = 0);

    function TotalLength1: Double;
    function TotalLength2: Double;

    function FinalizePath(Vertices: TAggVertexSequence): Double;

    procedure Transform1(Vertices: TAggVertexSequence; Kindex, Kx: Double;
      X, Y: PDouble);

    property BaseLength: Double read GetBaseLength write SetBaseLength;
    property BaseHeight: Double read GetBaseHeight write SetBaseHeight;
    property PreserveXScale: Boolean read GetPreserveXScale
      write SetPreserveXScale;
  end;

implementation


procedure DoublePathTransform(This: TAggTransDoublePath; X, Y: PDouble);
var
  Rect: TRectDouble;
  Dd: Double;
begin
  if (This.FStatus[0] = siReady) and (This.FStatus[1] = siReady) then
  begin
    if This.FBaseLength > 1E-10 then
      X^ := X^ * (PAggVertexDistance(This.FSourceVertices[0][
        This.FSourceVertices[0].Size - 1]).Dist / This.FBaseLength);

    Rect.X1 := X^;
    Rect.Y1 := Y^;
    Rect.X2 := X^;
    Rect.Y2 := Y^;
    Dd := PAggVertexDistance(This.FSourceVertices[1][
      This.FSourceVertices[1].Size - 1]).Dist /
      PAggVertexDistance(This.FSourceVertices[0][
      This.FSourceVertices[0].Size - 1]).Dist;

    This.Transform1(This.FSourceVertices[0], This.FKIndex[0], 1.0, @Rect.X1,
      @Rect.Y1);
    This.Transform1(This.FSourceVertices[1], This.FKIndex[1], Dd, @Rect.X2,
      @Rect.Y2);

    X^ := Rect.X1 + Y^ * (Rect.X2 - Rect.X1) / This.FBaseHeight;
    Y^ := Rect.Y1 + Y^ * (Rect.Y2 - Rect.Y1) / This.FBaseHeight;
  end;
end;


{ TAggTransDoublePath }

constructor TAggTransDoublePath.Create;
begin
  inherited Create;

  Transform := @DoublePathTransform;

  FSourceVertices[0] := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));
  FSourceVertices[1] := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));

  FKIndex[0] := 0;
  FKIndex[1] := 0;

  FBaseLength := 0;
  FBaseHeight := 1;

  FStatus[0] := siInitial;
  FStatus[1] := siInitial;

  FPreserveXScale := True;
end;

destructor TAggTransDoublePath.Destroy;
begin
  FSourceVertices[0].Free;
  FSourceVertices[1].Free;
  inherited
end;

procedure TAggTransDoublePath.SetBaseLength;
begin
  FBaseLength := V;
end;

function TAggTransDoublePath.GetBaseLength;
begin
  Result := FBaseLength;
end;

procedure TAggTransDoublePath.SetBaseHeight;
begin
  FBaseHeight := V;
end;

function TAggTransDoublePath.GetBaseHeight;
begin
  Result := FBaseHeight;
end;

procedure TAggTransDoublePath.SetPreserveXScale;
begin
  FPreserveXScale := F;
end;

function TAggTransDoublePath.GetPreserveXScale;
begin
  Result := FPreserveXScale;
end;

procedure TAggTransDoublePath.Reset;
begin
  FSourceVertices[0].RemoveAll;
  FSourceVertices[1].RemoveAll;

  FKIndex[0] := 0.0;
  FKIndex[0] := 0.0;
  FStatus[0] := siInitial;
  FStatus[1] := siInitial;
end;

procedure TAggTransDoublePath.MoveTo1;
var
  Vd: TAggVertexDistance;
begin
  if FStatus[0] = siInitial then
  begin
    Vd.Pos := PointDouble(X, Y);

    Vd.Dist := 0;

    FSourceVertices[0].ModifyLast(@Vd);

    FStatus[0] := siMakingPath;
  end
  else
    LineTo1(X, Y);
end;

procedure TAggTransDoublePath.LineTo1;
var
  Vd: TAggVertexDistance;
begin
  if FStatus[0] = siMakingPath then
  begin
    Vd.Pos := PointDouble(X, Y);

    Vd.Dist := 0;

    FSourceVertices[0].Add(@Vd);
  end;
end;

procedure TAggTransDoublePath.MoveTo2;
var
  Vd: TAggVertexDistance;
begin
  if FStatus[1] = siInitial then
  begin
    Vd.Pos := PointDouble(X, Y);

    Vd.Dist := 0;

    FSourceVertices[1].ModifyLast(@Vd);

    FStatus[1] := siMakingPath;
  end
  else
    LineTo2(X, Y);
end;

procedure TAggTransDoublePath.LineTo2;
var
  Vd: TAggVertexDistance;
begin
  if FStatus[1] = siMakingPath then
  begin
    Vd.Pos := PointDouble(X, Y);

    Vd.Dist := 0;

    FSourceVertices[1].Add(@Vd);
  end;
end;

procedure TAggTransDoublePath.FinalizePaths;
begin
  if (FStatus[0] = siMakingPath) and (FSourceVertices[0].Size > 1) and
    (FStatus[1] = siMakingPath) and (FSourceVertices[1].Size > 1) then
  begin
    FKIndex[0] := FinalizePath(FSourceVertices[0]);
    FKIndex[1] := FinalizePath(FSourceVertices[1]);
    FStatus[0] := siReady;
    FStatus[1] := siReady;
  end;
end;

procedure TAggTransDoublePath.AddPaths;
var
  X, Y: Double;
  Cmd : Cardinal;
begin
  Vs1.Rewind(Path1ID);

  Cmd := Vs1.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsMoveTo(Cmd) then
      MoveTo1(X, Y)
    else if IsVertex(Cmd) then
      LineTo1(X, Y);

    Cmd := Vs1.Vertex(@X, @Y);
  end;

  Vs2.Rewind(Path2ID);

  Cmd := Vs2.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsMoveTo(Cmd) then
      MoveTo2(X, Y)
    else if IsVertex(Cmd) then
      LineTo2(X, Y);

    Cmd := Vs2.Vertex(@X, @Y);
  end;

  FinalizePaths;
end;

function TAggTransDoublePath.TotalLength1;
begin
  if FBaseLength >= 1E-10 then
    Result := FBaseLength
  else if FStatus[0] = siReady then
    Result := PAggVertexDistance(FSourceVertices[0][
      FSourceVertices[0].Size - 1]).Dist
  else
    Result := 0.0;
end;

function TAggTransDoublePath.TotalLength2;
begin
  if FBaseLength >= 1E-10 then
    Result := FBaseLength
  else if FStatus[1] = siReady then
    Result := PAggVertexDistance(FSourceVertices[1][
      FSourceVertices[1].Size - 1]).Dist
  else
    Result := 0.0;
end;

function TAggTransDoublePath.FinalizePath;
var
  I: Cardinal;
  V: PAggVertexDistance;

  D, Dist: Double;

begin
  Vertices.Close(False);

  if Vertices.Size > 2 then
    if PAggVertexDistance(Vertices[Vertices.Size - 2]).Dist * 10 <
      PAggVertexDistance(Vertices[Vertices.Size - 3]).Dist then
    begin
      D := PAggVertexDistance(Vertices[Vertices.Size - 3]).Dist +
        PAggVertexDistance(Vertices[Vertices.Size - 2]).Dist;

      Move(Vertices[Vertices.Size - 1]^, Vertices[Vertices.Size - 2]^,
        SizeOf(TAggVertexDistance));

      Vertices.RemoveLast;

      PAggVertexDistance(Vertices[Vertices.Size - 2]).Dist := D;
    end;

  Dist := 0;

  for I := 0 to Vertices.Size - 1 do
  begin
    V := Vertices[I];
    D := V.Dist;

    V.Dist := Dist;
    Dist := Dist + D;
  end;

  Result := (Vertices.Size - 1) / Dist;
end;

procedure TAggTransDoublePath.Transform1(Vertices: TAggVertexSequence;
  Kindex, Kx: Double; X, Y: PDouble);
var
  Delta: TPointDouble;
  X1, Y1, D, Dd: Double;
  I, J, K: Cardinal;
begin
  X1 := 0;
  Y1 := 0;
  Delta.X := 1;
  Delta.Y := 1;
  D := 0;
  Dd := 1;

  X^ := X^ * Kx;

  if X^ < 0.0 then
  begin
    // Extrapolation on the left
    X1 := PAggVertexDistance(Vertices[0]).Pos.X;
    Y1 := PAggVertexDistance(Vertices[0]).Pos.Y;
    Delta.X := PAggVertexDistance(Vertices[1]).Pos.X - X1;
    Delta.Y := PAggVertexDistance(Vertices[1]).Pos.Y - Y1;
    Dd := PAggVertexDistance(Vertices[1]).Dist -
      PAggVertexDistance(Vertices[0]).Dist;
    D := X^;
  end else
  if X^ > PAggVertexDistance(Vertices[Vertices.Size - 1]).Dist then
  begin
    I := Vertices.Size - 2;
    J := Vertices.Size - 1;

    X1 := PAggVertexDistance(Vertices[J]).Pos.X;
    Y1 := PAggVertexDistance(Vertices[J]).Pos.Y;
    Delta.X := X1 - PAggVertexDistance(Vertices[I]).Pos.X;
    Delta.Y := Y1 - PAggVertexDistance(Vertices[I]).Pos.Y;
    Dd := PAggVertexDistance(Vertices[J]).Dist -
      PAggVertexDistance(Vertices[I]).Dist;
    D := X^ - PAggVertexDistance(Vertices[J]).Dist;
  end
  else
  begin
    // Interpolation
    I := 0;
    J := Vertices.Size - 1;

    if FPreserveXScale then
    begin
      I := 0;

      while J - I > 1 do
      begin
        K := (I + J) shr 1;

        if X^ < PAggVertexDistance(Vertices[K]).Dist then
          J := K
        else
          I := K;
      end;

      D := PAggVertexDistance(Vertices[I]).Dist;
      Dd := PAggVertexDistance(Vertices[J]).Dist - D;
      D := X^ - D;
    end
    else
    begin
      I := Trunc(X^ * Kindex);
      J := I + 1;
      Dd := PAggVertexDistance(Vertices[J]).Dist -
        PAggVertexDistance(Vertices[I]).Dist;
      D := ((X^ * Kindex) - I) * Dd;
    end;

    X1 := PAggVertexDistance(Vertices[I]).Pos.X;
    Y1 := PAggVertexDistance(Vertices[I]).Pos.Y;
    Delta.X := PAggVertexDistance(Vertices[J]).Pos.X - X1;
    Delta.Y := PAggVertexDistance(Vertices[J]).Pos.Y - Y1;
  end;

  Dd := D / Dd;
  X^ := X1 + Delta.X * Dd;
  Y^ := Y1 + Delta.Y * Dd;
end;

end.
