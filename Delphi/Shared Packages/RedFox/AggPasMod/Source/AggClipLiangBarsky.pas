unit AggClipLiangBarsky;

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
  AggBasics;

function ClippingFlagsInteger(X, Y: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsDouble(X, Y: Double; ClipBox: PRectDouble): Cardinal;

function ClippingFlagsXInteger(X: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsXDouble(X: Double; ClipBox: PRectDouble): Cardinal;

function ClippingFlagsYInteger(Y: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsYDouble(Y: Double; ClipBox: PRectDouble): Cardinal;

function ClipLiangBarskyInteger(X1, Y1, X2, Y2: Integer;
  const ClipBox: TRectInteger; X, Y: PInteger): Cardinal; overload;
function ClipLiangBarskyInteger(X1, Y1, X2, Y2: Integer;
  const ClipBox: TRectInteger; Point: PPointInteger): Cardinal; overload;

  function ClipLiangBarskyDouble(X1, Y1, X2, Y2: Double; ClipBox: PRectDouble;
  X, Y: PDouble): Cardinal;

implementation

// Determine the clipping code of the vertex according to the
// Cyrus-Beck line clipping algorithm
//
//          |        |
//    0110  |  0010  | 0011
//          |        |
//   -------+--------+-------- ClipBox.y2
//          |        |
//    0100  |  0000  | 0001
//          |        |
//   -------+--------+-------- ClipBox.y1
//          |        |
//    1100  |  1000  | 1001
//          |        |
//    ClipBox.x1  ClipBox.x2
//

function ClippingFlagsInteger(X, Y: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := Cardinal(X > ClipBox.X2) or (Cardinal(Y > ClipBox.Y2) shl 1) or
    (Cardinal(X < ClipBox.X1) shl 2) or (Cardinal(Y < ClipBox.Y1) shl 3);
end;

function ClippingFlagsDouble(X, Y: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := Cardinal(X > ClipBox.X2) or (Cardinal(Y > ClipBox.Y2) shl 1) or
    (Cardinal(X < ClipBox.X1) shl 2) or (Cardinal(Y < ClipBox.Y1) shl 3);
end;

function ClippingFlagsXInteger(X: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := Cardinal(X > ClipBox.X2) or (Cardinal(X < ClipBox.X1) shl 2);
end;

function ClippingFlagsXDouble(X: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := Cardinal(X > ClipBox.X2) or (Cardinal(X < ClipBox.X1) shl 2);
end;

function ClippingFlagsYInteger(Y: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := (Cardinal(Y > ClipBox.Y2) shl 1) or
    (Cardinal(Y < ClipBox.Y1) shl 3);
end;

function ClippingFlagsYDouble(Y: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := (Cardinal(Y > ClipBox.Y2) shl 1) or
    (Cardinal(Y < ClipBox.Y1) shl 3);
end;

function ClipLiangBarskyInteger(X1, Y1, X2, Y2: Integer;
  const ClipBox: TRectInteger; X, Y: PInteger): Cardinal;
const
  CNearZero = 1E-30;
var
  Inside, Outside, TempIn, TempOut, Delta: TPointDouble;
  TIn1, TIn2, TOut1: Double;
  Np: Cardinal;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;

  Np := 0;

  // bump off of the vertical
  if Delta.X = 0.0 then
    if X1 > ClipBox.X1 then
      Delta.X := -CNearZero
    else
      Delta.X := CNearZero;

  // bump off of the horizontal
  if Delta.Y = 0.0 then
    if Y1 > ClipBox.Y1 then
      Delta.Y := -CNearZero
    else
      Delta.Y := CNearZero;

  if Delta.X > 0.0 then
  begin
    // points to right
    Inside.X := ClipBox.X1;
    Outside.X := ClipBox.X2;
  end
  else
  begin
    Inside.X := ClipBox.X2;
    Outside.X := ClipBox.X1;
  end;

  if Delta.Y > 0.0 then
  begin
    // points up
    Inside.Y := ClipBox.Y1;
    Outside.Y := ClipBox.Y2;
  end
  else
  begin
    Inside.Y := ClipBox.Y2;
    Outside.Y := ClipBox.Y1;
  end;

  TempIn.X := (Inside.X - X1) / Delta.X;
  TempIn.Y := (Inside.Y - Y1) / Delta.Y;

  if TempIn.X < TempIn.Y then
  begin
    // hits x first
    TIn1 := TempIn.X;
    TIn2 := TempIn.Y;
  end
  else
  begin
    // hits y first
    TIn1 := TempIn.Y;
    TIn2 := TempIn.X;
  end;

  if TIn1 <= 1.0 then
  begin
    if 0.0 < TIn1 then
    begin
      X^ := Trunc(Inside.X);
      Y^ := Trunc(Inside.Y);

      Inc(PtrComp(X), SizeOf(Integer));
      Inc(PtrComp(Y), SizeOf(Integer));
      Inc(Np);
    end;

    if TIn2 <= 1.0 then
    begin
      TempOut.X := (Outside.X - X1) / Delta.X;
      TempOut.Y := (Outside.Y - Y1) / Delta.Y;

      if TempOut.X < TempOut.Y then
        TOut1 := TempOut.X
      else
        TOut1 := TempOut.Y;

      if (TIn2 > 0.0) or (TOut1 > 0.0) then
        if TIn2 <= TOut1 then
        begin
          if TIn2 > 0.0 then
          begin
            if TempIn.X > TempIn.Y then
            begin
              X^ := Trunc(Inside.X);
              Y^ := Trunc(Y1 + TempIn.X * Delta.Y);
            end
            else
            begin
              X^ := Trunc(X1 + TempIn.Y * Delta.X);
              Y^ := Trunc(Inside.Y);
            end;

            Inc(PtrComp(X), SizeOf(Integer));
            Inc(PtrComp(Y), SizeOf(Integer));
            Inc(Np);
          end;

          if TOut1 < 1.0 then
            if TempOut.X < TempOut.Y then
            begin
              X^ := Trunc(Outside.X);
              Y^ := Trunc(Y1 + TempOut.X * Delta.Y);
            end
            else
            begin
              X^ := Trunc(X1 + TempOut.Y * Delta.X);
              Y^ := Trunc(Outside.Y);
            end
          else
          begin
            X^ := X2;
            Y^ := Y2;
          end;

          Inc(PtrComp(X), SizeOf(Integer));
          Inc(PtrComp(Y), SizeOf(Integer));
          Inc(Np);
        end
        else
        begin
          if TempIn.X > TempIn.Y then
          begin
            X^ := Trunc(Inside.X);
            Y^ := Trunc(Outside.Y);
          end
          else
          begin
            X^ := Trunc(Outside.X);
            Y^ := Trunc(Inside.Y);
          end;

          Inc(PtrComp(X), SizeOf(Integer));
          Inc(PtrComp(Y), SizeOf(Integer));
          Inc(Np);
        end;
    end;
  end;

  Result := Np;
end;

function ClipLiangBarskyInteger(X1, Y1, X2, Y2: Integer;
  const ClipBox: TRectInteger; Point: PPointInteger): Cardinal;
const
  CNearZero = 1E-30;
var
  Delta, Inside, Outside, TempIn, TempOut: TPointDouble;
  TempIn1, TempIn2, TempOut1: Double;
  Np: Cardinal;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;

  Np := 0;

  // bump off of the vertical
  if Delta.X = 0.0 then
    if X1 > ClipBox.X1 then
      Delta.X := -CNearZero
    else
      Delta.X := CNearZero;

  // bump off of the horizontal
  if Delta.Y = 0.0 then
    if Y1 > ClipBox.Y1 then
      Delta.Y := -CNearZero
    else
      Delta.Y := CNearZero;

  if Delta.X > 0.0 then
  begin
    // points to right
    Inside.X := ClipBox.X1;
    Outside.X := ClipBox.X2;
  end
  else
  begin
    Inside.X := ClipBox.X2;
    Outside.X := ClipBox.X1;
  end;

  if Delta.Y > 0.0 then
  begin
    // points up
    Inside.Y := ClipBox.Y1;
    Outside.Y := ClipBox.Y2;
  end
  else
  begin
    Inside.Y := ClipBox.Y2;
    Outside.Y := ClipBox.Y1;
  end;

  TempIn.X := (Inside.X - X1) / Delta.X;
  TempIn.Y := (Inside.Y - Y1) / Delta.Y;

  if TempIn.X < TempIn.Y then
  begin
    // hits x first
    TempIn1 := TempIn.X;
    TempIn2 := TempIn.Y;
  end
  else
  begin
    // hits y first
    TempIn1 := TempIn.Y;
    TempIn2 := TempIn.X;
  end;

  if TempIn1 <= 1.0 then
  begin
    if 0.0 < TempIn1 then
    begin
      Point^.X := Trunc(Inside.X);
      Point^.Y := Trunc(Inside.Y);

      Inc(Point);
      Inc(Np);
    end;

    if TempIn2 <= 1.0 then
    begin
      TempOut.X := (Outside.X - X1) / Delta.X;
      TempOut.Y := (Outside.Y - Y1) / Delta.Y;

      if TempOut.X < TempOut.Y then
        TempOut1 := TempOut.X
      else
        TempOut1 := TempOut.Y;

      if (TempIn2 > 0.0) or (TempOut1 > 0.0) then
        if TempIn2 <= TempOut1 then
        begin
          if TempIn2 > 0.0 then
          begin
            if TempIn.X > TempIn.Y then
            begin
              Point^.X := Trunc(Inside.X);
              Point^.Y := Trunc(Y1 + TempIn.X * Delta.Y);
            end
            else
            begin
              Point^.X := Trunc(X1 + TempIn.Y * Delta.X);
              Point^.Y := Trunc(Inside.Y);
            end;

            Inc(Point);
            Inc(Np);
          end;

          if TempOut1 < 1.0 then
            if TempOut.X < TempOut.Y then
            begin
              Point^.X := Trunc(Outside.X);
              Point^.Y := Trunc(Y1 + TempOut.X * Delta.Y);
            end
            else
            begin
              Point^.X := Trunc(X1 + TempOut.Y * Delta.X);
              Point^.Y := Trunc(Outside.Y);
            end
          else
          begin
            Point^.X := X2;
            Point^.Y := Y2;
          end;

          Inc(Point);
          Inc(Np);
        end
        else
        begin
          if TempIn.X > TempIn.Y then
          begin
            Point^.X := Trunc(Inside.X);
            Point^.Y := Trunc(Outside.Y);
          end
          else
          begin
            Point^.X := Trunc(Outside.X);
            Point^.Y := Trunc(Inside.Y);
          end;

          Inc(Point);
          Inc(Np);
        end;
    end;
  end;

  Result := Np;
end;

function ClipLiangBarskyDouble(X1, Y1, X2, Y2: Double; ClipBox: PRectDouble;
  X, Y: PDouble): Cardinal;
const
  CNearZero = 1E-30;
var
  Delta, Inside, Outside, TempIn, TempOut: TPointDouble;
  Tin1, Tin2, Tout1: Double;
  Np: Cardinal;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;

  Np := 0;

  // bump off of the vertical
  if Delta.X = 0.0 then
    if X1 > ClipBox.X1 then
      Delta.X := -CNearZero
    else
      Delta.X := CNearZero;

  // bump off of the horizontal
  if Delta.Y = 0.0 then
    if Y1 > ClipBox.Y1 then
      Delta.Y := -CNearZero
    else
      Delta.Y := CNearZero;

  if Delta.X > 0.0 then
  begin
    // points to right
    Inside.X := ClipBox.X1;
    Outside.X := ClipBox.X2;
  end
  else
  begin
    Inside.X := ClipBox.X2;
    Outside.X := ClipBox.X1;
  end;

  if Delta.Y > 0.0 then
  begin
    // points up
    Inside.Y := ClipBox.Y1;
    Outside.Y := ClipBox.Y2;
  end
  else
  begin
    Inside.Y := ClipBox.Y2;
    Outside.Y := ClipBox.Y1;
  end;

  TempIn.X := (Inside.X - X1) / Delta.X;
  TempIn.Y := (Inside.Y - Y1) / Delta.Y;

  if TempIn.X < TempIn.Y then
  begin
    // hits x first
    Tin1 := TempIn.X;
    Tin2 := TempIn.Y;
  end
  else
  begin
    // hits y first
    Tin1 := TempIn.Y;
    Tin2 := TempIn.X;
  end;

  if Tin1 <= 1.0 then
  begin
    if 0.0 < Tin1 then
    begin
      X^ := Inside.X;
      Y^ := Inside.Y;

      Inc(PtrComp(X), SizeOf(Integer));
      Inc(PtrComp(Y), SizeOf(Integer));
      Inc(Np);
    end;

    if Tin2 <= 1.0 then
    begin
      TempOut.X := (Outside.X - X1) / Delta.X;
      TempOut.Y := (Outside.Y - Y1) / Delta.Y;

      if TempOut.X < TempOut.Y then
        Tout1 := TempOut.X
      else
        Tout1 := TempOut.Y;

      if (Tin2 > 0.0) or (Tout1 > 0.0) then
        if Tin2 <= Tout1 then
        begin
          if Tin2 > 0.0 then
          begin
            if TempIn.X > TempIn.Y then
            begin
              X^ := Inside.X;
              Y^ := Y1 + TempIn.X * Delta.Y;

            end
            else
            begin
              X^ := X1 + TempIn.Y * Delta.X;
              Y^ := Inside.Y;
            end;

            Inc(PtrComp(X), SizeOf(Integer));
            Inc(PtrComp(Y), SizeOf(Integer));
            Inc(Np);
          end;

          if Tout1 < 1.0 then
            if TempOut.X < TempOut.Y then
            begin
              X^ := Outside.X;
              Y^ := Y1 + TempOut.X * Delta.Y;
            end
            else
            begin
              X^ := X1 + TempOut.Y * Delta.X;
              Y^ := Outside.Y;
            end
          else
          begin
            X^ := X2;
            Y^ := Y2;
          end;

          Inc(PtrComp(X), SizeOf(Integer));
          Inc(PtrComp(Y), SizeOf(Integer));
          Inc(Np);

        end
        else
        begin
          if TempIn.X > TempIn.Y then
          begin
            X^ := Inside.X;
            Y^ := Outside.Y;
          end
          else
          begin
            X^ := Outside.X;
            Y^ := Inside.Y;
          end;

          Inc(PtrComp(X), SizeOf(Integer));
          Inc(PtrComp(Y), SizeOf(Integer));
          Inc(Np);
        end;
    end;
  end;

  Result := Np;
end;

end.
