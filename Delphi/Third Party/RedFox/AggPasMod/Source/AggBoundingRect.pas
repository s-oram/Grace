unit AggBoundingRect;

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
  AggVertexSource;

function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal; Start, Num: Cardinal;
  X1, Y1, X2, Y2: PDouble): Boolean; overload;
function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal; Start, Num: Cardinal;
  var Rect: TRectDouble): Boolean; overload;

function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal;
  X1, Y1, X2, Y2: PDouble): Boolean; overload;
function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal;
  var Rect: TRectDouble): Boolean; overload;

function BoundingRectInteger(Vs: TAggVertexSource; Ul: TCardinalList;
  Start, Num: Cardinal; X1, Y1, X2, Y2: PDouble): Boolean;

function BoundingRectSingle(Vs: TAggVertexSource; PathID: Cardinal;
  X1, Y1, X2, Y2: PDouble): Boolean;

function BoundingRectAllPaths(Vs: TAggVertexSource; X1, Y1, X2,
  Y2: PDouble): Boolean; overload;
function BoundingRectAllPaths(Vs: TAggVertexSource; Rect: TRectDouble): Boolean;
  overload;

implementation

function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal;
  Start, Num: Cardinal; X1, Y1, X2, Y2: PDouble): Boolean;
var
  I, Cmd: Cardinal;
  X, Y: Double;
  First: Boolean;
begin
  First := True;

  X1^ := 1;
  Y1^ := 1;
  X2^ := 0;
  Y2^ := 0;

  I := 0;

  while I < Num do
  begin
    Vs.Rewind(PCardinal(PtrComp(Gi) + (Start + I) * SizeOf(Cardinal))^);

    Cmd := Vs.Vertex(@X, @Y);

    while not IsStop(Cmd) do
    begin
      if IsVertex(Cmd) then
        if First then
        begin
          X1^ := X;
          Y1^ := Y;
          X2^ := X;
          Y2^ := Y;

          First := False;
        end
        else
        begin
          if X < X1^ then
            X1^ := X;

          if Y < Y1^ then
            Y1^ := Y;

          if X > X2^ then
            X2^ := X;

          if Y > Y2^ then
            Y2^ := Y;
        end;

      Cmd := Vs.Vertex(@X, @Y);
    end;

    Inc(I);
  end;

  Result := (X1^ <= X2^) and (Y1^ <= Y2^);
end;

function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal; Start, Num: Cardinal;
  var Rect: TRectDouble): Boolean;
begin
  BoundingRect(Vs, Gi, Start, Num, @Rect.X1, @Rect.Y1, @Rect.X2, @Rect.Y2);
end;

function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal;
  X1, Y1, X2, Y2: PDouble): Boolean;
var
  I, Cmd: Cardinal;
  X, Y: Double;
  First: Boolean;
begin
  First := True;

  X1^ := 1;
  Y1^ := 1;
  X2^ := 0;
  Y2^ := 0;

  I := 0;

  while I < Num do
  begin
    Vs.Rewind(Gi.PathID[Start + I]);

    Cmd := Vs.Vertex(@X, @Y);

    while not IsStop(Cmd) do
    begin
      if IsVertex(Cmd) then
        if First then
        begin
          X1^ := X;
          Y1^ := Y;
          X2^ := X;
          Y2^ := Y;

          First := False;

        end
        else
        begin
          if X < X1^ then
            X1^ := X;

          if Y < Y1^ then
            Y1^ := Y;

          if X > X2^ then
            X2^ := X;

          if Y > Y2^ then
            Y2^ := Y;
        end;

      Cmd := Vs.Vertex(@X, @Y);
    end;

    Inc(I);
  end;

  Result := (X1^ <= X2^) and (Y1^ <= Y2^);
end;

function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal;
  var Rect: TRectDouble): Boolean;
begin
  BoundingRectVertexSource(Vs, Gi, Start, Num, @Rect.X1, @Rect.Y1, @Rect.X2,
    @Rect.Y2)
end;

function BoundingRectInteger(Vs: TAggVertexSource; Ul: TCardinalList;
  Start, Num: Cardinal; X1, Y1, X2, Y2: PDouble): Boolean;
var
  I, Cmd: Cardinal;

  X, Y: Double;

  First: Boolean;
begin
  First := True;

  X1^ := 1;
  Y1^ := 1;
  X2^ := 0;
  Y2^ := 0;

  I := 0;

  while I < Num do
  begin
    Vs.Rewind(Ul.ArrayOperator(Start + I));

    Cmd := Vs.Vertex(@X, @Y);

    while not IsStop(Cmd) do
    begin
      if IsVertex(Cmd) then
        if First then
        begin
          X1^ := X;
          Y1^ := Y;
          X2^ := X;
          Y2^ := Y;

          First := False;
        end
        else
        begin
          if X < X1^ then
            X1^ := X;

          if Y < Y1^ then
            Y1^ := Y;

          if X > X2^ then
            X2^ := X;

          if Y > Y2^ then
            Y2^ := Y;
        end;

      Cmd := Vs.Vertex(@X, @Y);
    end;

    Inc(I);
  end;

  Result := (X1^ <= X2^) and (Y1^ <= Y2^);
end;

function BoundingRectSingle(Vs: TAggVertexSource; PathID: Cardinal;
  X1, Y1, X2, Y2: PDouble): Boolean;
var
  Cmd  : Cardinal;
  X, Y : Double;
  First: Boolean;
begin
  First := True;

  X1^ := 1;
  Y1^ := 1;
  X2^ := 0;
  Y2^ := 0;

  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsVertex(Cmd) then
      if First then
      begin
        X1^ := X;
        Y1^ := Y;
        X2^ := X;
        Y2^ := Y;

        First := False;
      end
      else
      begin
        if X < X1^ then
          X1^ := X;

        if Y < Y1^ then
          Y1^ := Y;

        if X > X2^ then
          X2^ := X;

        if Y > Y2^ then
          Y2^ := Y;
      end;

    Cmd := Vs.Vertex(@X, @Y);
  end;

  Result := (X1^ <= X2^) and (Y1^ <= Y2^);
end;

function BoundingRectAllPaths(Vs: TAggVertexSource;
  X1, Y1, X2, Y2: PDouble): Boolean;
var
  I, Paths: Cardinal;
  Sx1, Sy1, Sx2, Sy2: Double;
  First: Boolean;
begin
  First := True;
  Paths := Vs.PathCount;

  X1^ := 1;
  Y1^ := 1;
  X2^ := 0;
  Y2^ := 0;

  I := 0;

  while I < Paths do
  begin
    if BoundingRectSingle(Vs, I, @Sx1, @Sy1, @Sx2, @Sy2) then
    begin
      if First then
      begin
        X1^ := Sx1;
        Y1^ := Sy1;
        X2^ := Sx2;
        Y2^ := Sy2;
      end
      else
      begin
        if Sx1 < X1^ then
          X1^ := Sx1;

        if Sy1 < Y1^ then
          Y1^ := Sy1;

        if Sx2 > X2^ then
          X2^ := Sx2;

        if Sy2 > Y2^ then
          Y2^ := Sy2;
      end;

      First := False;
    end;

    Inc(I);
  end;

  Result := (X1^ <= X2^) and (Y1^ <= Y2^);
end;

function BoundingRectAllPaths(Vs: TAggVertexSource; Rect: TRectDouble): Boolean;
  overload;
begin
  BoundingRectAllPaths(Vs, @Rect.X1, @Rect.Y1, @Rect.X2, @Rect.Y2);
end;

end.
