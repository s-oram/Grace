unit AggSimulEq;

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
  AggBasics;

procedure SwapArrays(A1, A2: PDouble; N: Cardinal);
function MatrixPivot(M: PDouble; Row, Rows, Cols: Cardinal): Integer;
function SimulEqSolve(Left, Right, EqResult: PDouble; Size,
  RightCols: Cardinal): Boolean;

implementation

procedure SwapArrays(A1, A2: PDouble; N: Cardinal);
var
  I  : Cardinal;
  Tmp: Double;
begin
  I := 0;

  while I < N do
  begin
    Tmp := A1^;
    A1^ := A2^;
    A2^ := Tmp;

    Inc(PtrComp(A1), SizeOf(Double));
    Inc(PtrComp(A2), SizeOf(Double));
    Inc(I);
  end;
end;

function MatrixPivot(M: PDouble; Row, Rows, Cols: Cardinal): Integer;
var
  I: Cardinal;
  K: Integer;
  MaxVal, Tmp: Double;
begin
  K := Row;

  MaxVal := -1.0;

  I := Row;

  while I < Rows do
  begin
    Tmp := Abs(PDouble(PtrComp(M) + (I * Cols + Row) * SizeOf(Double))^);

    if (Tmp > MaxVal) and (Tmp <> 0.0) then
    begin
      MaxVal := Tmp;

      K := I;
    end;

    Inc(I);
  end;

  if PDouble(PtrComp(M) + (K * Cols + Row) * SizeOf(Double))^ = 0.0 then
  begin
    Result := -1;

    Exit;
  end;

  if K <> Row then
  begin
    SwapArrays(PDouble(PtrComp(M) + K * Cols * SizeOf(Double)),
      PDouble(PtrComp(M) + Row * Cols * SizeOf(Double)), Cols);

    Result := K;

    Exit;
  end;

  Result := 0;
end;

function SimulEqSolve(Left, Right, EqResult: PDouble; Size,
  RightCols: Cardinal): Boolean;
var
  M: Integer;

  I, J, K, Adx: Cardinal;

  A1 : Double;
  Tmp: PDouble;
begin
  Result := False;

  // Alloc
  Adx := Size + RightCols;

  AggGetMem(Pointer(Tmp), Size * Adx * SizeOf(Double));
  try
    for I := 0 to Size - 1 do
    begin
      for J := 0 to Size - 1 do
        PDouble(PtrComp(Tmp) + (I * Adx + J) * SizeOf(Double))^ :=
          PDouble(PtrComp(Left) + (I * Size + J) * SizeOf(Double))^;

      for J := 0 to RightCols - 1 do
        PDouble(PtrComp(Tmp) + (I * Adx + Size + J) * SizeOf(Double))^ :=
          PDouble(PtrComp(Right) + (I * RightCols + J) * SizeOf(Double))^;
    end;

    for K := 0 to Size - 1 do
    begin
      if MatrixPivot(Tmp, K, Size, Size + RightCols) < 0 then
        Exit;

      A1 := PDouble(PtrComp(Tmp) + (K * Adx + K) * SizeOf(Double))^;
      J := K;

      while J < Size + RightCols do
      begin
        PDouble(PtrComp(Tmp) + (K * Adx + J) * SizeOf(Double))^ :=
          PDouble(PtrComp(Tmp) + (K * Adx + J) * SizeOf(Double))^ / A1;

        Inc(J);
      end;

      I := K + 1;

      while I < Size do
      begin
        A1 := PDouble(PtrComp(Tmp) + (I * Adx + K) * SizeOf(Double))^;
        J := K;

        while J < Size + RightCols do
        begin
          PDouble(PtrComp(Tmp) + (I * Adx + J) * SizeOf(Double))^ :=
            PDouble(PtrComp(Tmp) + (I * Adx + J) * SizeOf(Double))^ - A1 *
            PDouble(PtrComp(Tmp) + (K * Adx + J) * SizeOf(Double))^;

          Inc(J);
        end;

        Inc(I);
      end;
    end;

    for K := 0 to RightCols - 1 do
    begin
      M := Integer(Size - 1);

      while M >= 0 do
      begin
        PDouble(PtrComp(EqResult) + (M * RightCols + K) * SizeOf(Double))^ :=
          PDouble(PtrComp(Tmp) + (M * Adx + Size + K) * SizeOf(Double))^;

        J := M + 1;

        while J < Size do
        begin
          PDouble(PtrComp(EqResult) + (M * RightCols + K) * SizeOf(Double))^ :=
            PDouble(PtrComp(EqResult) + (M * RightCols + K) * SizeOf(Double))^ -
            (PDouble(PtrComp(Tmp) + (M * Adx + J) * SizeOf(Double))^ *
            PDouble(PtrComp(EqResult) + (J * RightCols + K) * SizeOf(Double))^);

          Inc(J);
        end;

        Dec(M);
      end;
    end;

    Result := True;
  finally
    AggFreeMem(Pointer(Tmp), Size * Adx * SizeOf(Double));
  end;
end;

end.
