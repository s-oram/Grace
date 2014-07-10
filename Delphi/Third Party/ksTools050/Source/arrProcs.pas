{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit arrProcs;

{$I ksTools.inc}

interface

{ Addition primitives }
function arrAdd(A, B, Res: PLongWord; LA, LB: LongInt): Boolean;
function arrSelfAdd(A, B: PLongWord; LA, LB: LongInt): Boolean;
function arrAddLimb(A: PLongWord; Limb: LongWord; Res: PLongWord; L: LongWord): Boolean;
function arrSelfAddLimb(A: PLongWord; Limb: LongWord; L: LongInt): Boolean;

{ Subtraction primitives }
function arrSub(A, B, Res: PLongWord; LA, LB: LongInt): Boolean;
function arrSelfSub(A, B: PLongWord; LA, LB: LongInt): Boolean;
function arrSubLimb(A: PLongWord; Limb: LongWord; Res: PLongWord; L: LongWord): Boolean;
function arrSelfSubLimb(A: PLongWord; Limb: LongWord; L: LongWord): Boolean;

{ Multiplication primitives }
procedure arrMul(A, B, Res: PLongWord; LA, LB: LongInt);
function arrSelfMulLimb(A: PLongWord; Limb: LongWord; L: LongInt): Boolean;

{ Division primitives }
procedure arrNormDivMod(Dividend, Divisor, Quotient: PLongWord;
                        DndLen, DsrLen: LongWord);
function arrDivModLimb(A, Q: PLongWord; L, D: LongWord): LongWord;
function arrSelfDivModLimb(A: PLongWord; L, D: LongWord): LongWord;

function arrCmp(A, B: PLongWord; L: LongInt): LongInt;

function arrShlShort(A, Res: PLongWord; LA, Shift: LongInt): LongInt;
function arrShrShort(A, Res: PLongWord; LA, Shift: LongInt): LongInt;

implementation

{$IFDEF OldVersion}
function GetLimb(P: PLongWord; Offset: Integer): LongWord;
begin
  Inc(P, Offset);
  Result:= P^;
end;
{$ELSE}
{$POINTERMATH ON}
{$ENDIF}

type
  UInt64Rec = packed record
    case Byte of
      0: (Lo, Hi: LongWord);
      1: (Value: UInt64);
  end;

{
  Description:
    Res:= A + B
  Asserts:
    LA >= LB >= 1
    Res must have enough space for LA + 1 limbs
  Remarks:
    function returns True if carry is propagated out of A[LA-1];
    if function returns True the Res senior limb is set: Res[LA] = 1;
    any (A = B = Res) coincidence is allowed
}
function arrAdd(A, B, Res: PLongWord; LA, LB: LongInt): Boolean;
var
  CarryOut, CarryIn: Boolean;
  Tmp: LongWord;

begin
  Dec(LA, LB);
  CarryIn:= False;
  while LB > 0 do begin
    Tmp:= A^ + B^;
    CarryOut:= Tmp < A^;
    Inc(A);
    Inc(B);
    if CarryIn then begin
      Inc(Tmp);
      CarryOut:= CarryOut or (Tmp = 0);
    end;
    CarryIn:= CarryOut;
    Res^:= Tmp;
    Inc(Res);
    Dec(LB);
  end;
  while (LA > 0) and CarryIn do begin
    Tmp:= A^ + 1;
    CarryIn:= Tmp = 0;
    Inc(A);
    Res^:= Tmp;
    Inc(Res);
    Dec(LA);
  end;
  while (LA > 0) do begin
    Res^:= A^;
    Inc(A);
    Inc(Res);
    Dec(LA);
  end;
  if CarryIn then Res^:= 1;
//  else Res^:= 0;
  Result:= CarryIn;
end;

{
  Description:
    A:= A + B
  Asserts:
    LA >= LB >= 1
    A must have enough space for LA + 1 limbs
  Remarks:
    function returns True if carry is propagated out of A[LA-1];
    if function returns True the A senior limb is set: A[LA] = 1
    (A = B) coincidence is allowed
}
function arrSelfAdd(A, B: PLongWord; LA, LB: LongInt): Boolean;
var
  CarryOut, CarryIn: Boolean;
  Tmp: LongWord;

begin
  Dec(LA, LB);
  CarryIn:= False;
  while LB > 0 do begin
    Tmp:= A^ + B^;
    CarryOut:= Tmp < A^;
//    Inc(A);
    Inc(B);
    if CarryIn then begin
      Inc(Tmp);
      CarryOut:= CarryOut or (Tmp = 0);
    end;
    CarryIn:= CarryOut;
    A^:= Tmp;
    Inc(A);
    Dec(LB);
  end;
  while (LA > 0) and CarryIn do begin
    Tmp:= A^ + 1;
    CarryIn:= Tmp = 0;
    A^:= Tmp;
    Inc(A);
    Dec(LA);
  end;
  if CarryIn then A^:= 1;
  Result:= CarryIn;
end;

{
  Description:
    Res:= A + Limb
  Asserts:
    L >= 1
    Res must have enough space for L + 1 limbs
  Remarks:
    function returns True if carry is propagated out of A[L-1];
    if function returns True the Res senior limb is set: Res[L] = 1
}
function arrAddLimb(A: PLongWord; Limb: LongWord; Res: PLongWord; L: LongWord): Boolean;
var
  CarryIn: Boolean;
  Tmp: LongWord;

begin
  Tmp:= A^ + Limb;
  CarryIn:= Tmp < Limb;
  Inc(A);
  Dec(L);
  Res^:= Tmp;
  Inc(Res);
  while (L > 0) and CarryIn do begin
    Tmp:= A^ + 1;
    CarryIn:= Tmp = 0;
    Inc(A);
    Res^:= Tmp;
    Inc(Res);
    Dec(L);
  end;
  while (L > 0) do begin
    Res^:= A^;
    Inc(A);
    Inc(Res);
    Dec(L);
  end;
//  Res^:= LongWord(CarryIn);
  if CarryIn then Res^:= 1;
//  else Res^:= 0;
  Result:= CarryIn;
end;

{
  Description:
    A:= A + Limb
  Asserts:
    L >= 1
    A must have enougth space for L + 1 limbs
  Remarks:
    function returns True if carry is propagated out of A[L-1];
    if function returns True the A senior limb is set: A[L] = 1
}
function arrSelfAddLimb(A: PLongWord; Limb: LongWord; L: LongInt): Boolean;
var
  CarryIn: Boolean;
  Tmp: LongWord;

begin
  Tmp:= A^ + Limb;
  CarryIn:= Tmp < Limb;
  A^:= Tmp;
  Inc(A);
  Dec(L);
  while (L > 0) and CarryIn do begin
    Tmp:= A^ + 1;
    CarryIn:= Tmp = 0;
    A^:= Tmp;
    Inc(A);
    Dec(L);
  end;
  if CarryIn then A^:= 1;
//  if (L = 0) then A^:= LongWord(CarryIn);
  Result:= CarryIn;
end;

{
  Description:
    Res:= A - B
  Asserts:
    LA >= LB >= 1
    Res must have enough space for LA limbs
  Remarks:
    function returns True if borrow is propagated out of A[LA-1] (A < B);
    if function returns True the Res is invalid
    any (A = B = Res) coincidence is allowed
}
function arrSub(A, B, Res: PLongWord; LA, LB: LongInt): Boolean;
var
  BorrowOut, BorrowIn: Boolean;
  Tmp: LongWord;

begin
  Dec(LA, LB);
  BorrowIn:= False;
  while LB > 0 do begin
    Tmp:= A^ - B^;
    BorrowOut:= Tmp > A^;
    Inc(A);
    Inc(B);
    if BorrowIn then begin
      BorrowOut:= BorrowOut or (Tmp = 0);
      Dec(Tmp);
    end;
    BorrowIn:= BorrowOut;
    Res^:= Tmp;
    Inc(Res);
    Dec(LB);
  end;
  while (LA > 0) and BorrowIn do begin
    Tmp:= A^;
    BorrowIn:= Tmp = 0;
    Dec(Tmp);
    Inc(A);
    Res^:= Tmp;
    Inc(Res);
    Dec(LA);
  end;
  while (LA > 0) do begin
    Res^:= A^;
    Inc(A);
    Inc(Res);
    Dec(LA);
  end;
  Result:= BorrowIn;
end;

{
  Description:
    A:= A - B
  Asserts:
    LA >= LB >= 1
  Remarks:
    function returns True if borrow is propagated out of A[LA-1] (A < B);
    if function returns True the A is invalid
    (A = B) coincidence is allowed
}
function arrSelfSub(A, B: PLongWord; LA, LB: LongInt): Boolean;
var
  BorrowOut, BorrowIn: Boolean;
  Tmp: LongWord;

begin
  Dec(LA, LB);
  BorrowIn:= False;
  while LB > 0 do begin
    Tmp:= A^ - B^;
    BorrowOut:= Tmp > A^;
    Inc(B);
    if BorrowIn then begin
      BorrowOut:= BorrowOut or (Tmp = 0);
      Dec(Tmp);
    end;
    BorrowIn:= BorrowOut;
    A^:= Tmp;
    Inc(A);
    Dec(LB);
  end;
  while (LA > 0) and BorrowIn do begin
    Tmp:= A^;
    BorrowIn:= Tmp = 0;
    Dec(Tmp);
    A^:= Tmp;
    Inc(A);
    Dec(LA);
  end;
  Result:= BorrowIn;
end;

{
  Description:
    Res:= A - Limb
  Asserts:
    L >= 1
    Res must have enough space for L limbs
  Remarks:
    function returns True if borrow is propagated out of A[L-1] (A < B);
    if function returns True the Res is invalid
}
function arrSubLimb(A: PLongWord; Limb: LongWord; Res: PLongWord; L: LongWord): Boolean;
var
  BorrowIn: Boolean;
  Tmp: LongWord;

begin
  Tmp:= A^ - Limb;
  BorrowIn:= Tmp > A^;
  Inc(A);
  Dec(L);
  Res^:= Tmp;
  while (L > 0) and BorrowIn do begin
    Tmp:= A^;
    BorrowIn:= Tmp = 0;
    Dec(Tmp);
    Inc(A);
    Inc(Res);
    Res^:= Tmp;
    Dec(L);
  end;
  while (L > 0) do begin
    Inc(Res);
    Res^:= A^;
    Inc(A);
    Dec(L);
  end;
{
  if BorrowIn then
// we get here if L = 1 and A[0] < Limb; set Res[0] = Limb - A[0]
    Res^:= LongWord(-LongInt(Res^));
}
  Result:= BorrowIn;
end;

{
  Description:
    A:= A - Limb
  Asserts:
    L >= 1
  Remarks:
    function returns True if borrow is propagated out of A[L-1] (A < B);
    if function returns True the A is invalid
}
function arrSelfSubLimb(A: PLongWord; Limb: LongWord; L: LongWord): Boolean;
var
  BorrowIn: Boolean;
  Tmp: LongWord;

begin
  Tmp:= A^ - Limb;
  BorrowIn:= Tmp > A^;
  A^:= Tmp;
  Inc(A);
  Dec(L);
  while (L > 0) and BorrowIn do begin
    Tmp:= A^;
    BorrowIn:= Tmp = 0;
    Dec(Tmp);
    A^:= Tmp;
    Inc(A);
    Dec(L);
  end;

  Result:= BorrowIn;
end;

{
  Description:
    Res:= A * B
  Asserts:
    LA >= 1, LB >= 1
    Res must have enough space for LA + LB limbs
  Remarks:
    none
}
procedure arrMul(A, B, Res: PLongWord; LA, LB: LongInt);
var
  PA, PRes: PLongWord;
  Cnt: LongInt;
  TmpB: UInt64;
  TmpRes: UInt64Rec;
  Carry: LongWord;

begin
  FillChar(Res^, (LA + LB) * SizeOf(LongWord), 0);
  while LB > 0 do begin
    if B^ <> 0 then begin
      TmpB:= B^;
      PA:= A;
      PRes:= Res;
      Cnt:= LA;
      Carry:= 0;
      while Cnt > 0 do begin
        TmpRes.Value:= TmpB * PA^ + Carry;
        TmpRes.Value:= TmpRes.Value + PRes^;
        PRes^:= TmpRes.Lo;
        Inc(PRes);
        Carry:= TmpRes.Hi;
        Inc(PA);
        Dec(Cnt);
      end;
      PRes^:= Carry;
    end;
    Inc(B);
    Inc(Res);
    Dec(LB);
  end;
end;

// A:= A * Limb;
// A must have enough space for L + 1 limbs
// returns: True if senior (L+1)-th limb of the multiplication result is nonzero
function arrSelfMulLimb(A: PLongWord; Limb: LongWord; L: LongInt): Boolean;
var
  Tmp: UInt64Rec;
  Carry: LongWord;

begin
  Carry:= 0;
  while L > 0 do begin
    Tmp.Lo:= A^;
    Tmp.Hi:= 0;
    Tmp.Value:= Tmp.Value * Limb + Carry;
    A^:= Tmp.Lo;
    Inc(A);
    Carry:= Tmp.Hi;
    Dec(L);
  end;
  A^:= Carry;
  Result:= Carry <> 0;
end;

function arrCmp(A, B: PLongWord; L: LongInt): LongInt;
begin
  if L > 0 then begin
    Inc(A, L - 1);
    Inc(B, L - 1);
    repeat
{$IFDEF OldVersion}
      if A^ > B^ then begin
        Result:= 1;
        Exit;
      end;
      if A^ < B^ then begin
        Result:= -1;
        Exit;
      end;
{$ELSE}
      if A^ > B^ then Exit(1);
      if A^ < B^ then Exit(-1);
{$ENDIF}
      Dec(A);
      Dec(B);
      Dec(L);
    until L = 0;
  end;
{$IFDEF OldVersion}
  Result:= 0;
  Exit;
{$ELSE}
  Exit(0);
{$ENDIF}
end;

function arrShlShort(A, Res: PLongWord; LA, Shift: LongInt): LongInt;
var
  Tmp, Carry: LongWord;

begin
  Assert(Shift < 32);
  Result:= LA;
  if Shift = 0 then begin
    Move(A^, Res^, LA * SizeOf(LongWord));
  end
  else begin
    Carry:= 0;
    repeat
      Tmp:= (A^ shl Shift) or Carry;
      Carry:= A^ shr (32 - Shift);
      Res^:= Tmp;
      Inc(A);
      Inc(Res);
      Dec(LA);
    until (LA = 0);
    if Carry <> 0 then begin
      Res^:= Carry;
      Inc(Result);
    end;
  end;
end;

// Short Shift Right
// A = Res is acceptable
// LA >= 1
// Shift < 32
function arrShrShort(A, Res: PLongWord; LA, Shift: LongInt): LongInt;
var
  Carry: LongWord;

begin
//  Assert(Shift < 32);
  Result:= LA;
  if Shift = 0 then begin
    Move(A^, Res^, LA * SizeOf(LongWord));
  end
  else begin
    Carry:= A^ shr Shift;
    Inc(A);
    Dec(LA);
    while (LA > 0) do begin
      Res^:= (A^ shl (32 - Shift)) or Carry;
      Carry:= A^ shr Shift;
      Inc(A);
      Inc(Res);
      Dec(LA);
    end;
    if (Carry <> 0) or (Result = 1) then begin
      Res^:= Carry;
    end
    else begin
      Dec(Result);
    end;
  end;
end;

function arrDivModLimb(A, Q: PLongWord; L, D: LongWord): LongWord;
var
  Tmp: UInt64Rec;

begin
  Dec(L);
  Inc(A, L);
  Inc(Q, L);
  Tmp.Lo:= A^;
  if Tmp.Lo >= D then begin
    Q^:= Tmp.Lo div D;
    Tmp.Hi:= Tmp.Lo mod D;
  end
  else begin
    Q^:= 0;
    Tmp.Hi:= Tmp.Lo;
  end;
  while L > 0 do begin
    Dec(A);
    Dec(Q);
    Tmp.Lo:= A^;
    Q^:= LongWord(Tmp.Value div D);
    Tmp.Hi:= LongWord(Tmp.Value mod D);
    Dec(L);
  end;
  Result:= Tmp.Hi;
end;

function arrSelfDivModLimb(A: PLongWord; L, D: LongWord): LongWord;
var
  Tmp: UInt64Rec;

begin
  Dec(L);
  Inc(A, L);
  Tmp.Lo:= A^;
  if Tmp.Lo >= D then begin
    A^:= Tmp.Lo div D;
    Tmp.Hi:= Tmp.Lo mod D;
  end
  else begin
    A^:= 0;
    Tmp.Hi:= Tmp.Lo;
  end;
  while L > 0 do begin
    Dec(A);
    Tmp.Lo:= A^;
    A^:= LongWord(Tmp.Value div D);
    Tmp.Hi:= LongWord(Tmp.Value mod D);
    Dec(L);
  end;
  Result:= Tmp.Hi;
end;

// normalized division (Divisor[DsrLen-1] and $80000000 <> 0)
// in: Dividend: Dividend;
//     Divisor: Divisor;
//     DndLen: Dividend Length
//     DsrLen: Divisor Length
// out: Quotient:= Dividend div Divisor
//      Dividend:= Dividend mod Divisor
procedure arrNormDivMod(Dividend, Divisor, Quotient: PLongWord;
                        DndLen, DsrLen: LongWord);
var
  Tmp: UInt64Rec;
  PDnd, PDsr: PLongWord;
  QGuess, RGuess: UInt64Rec;
//  QLen: LongWord;
  LoopCount, Count: Integer;
  Tmp32, Carry: LongWord;
  CarryIn, CarryOut: Boolean;

begin
  Assert(DndLen > DsrLen);
  Assert(DsrLen >= 2);
//  QLen:= DndLen - DsrLen + 1;

//  Inc(Quotient, QLen);

  LoopCount:= DndLen - DsrLen;
  Inc(Quotient, LoopCount);

{$IFDEF OldVersion}
  PDnd:= Dividend;
  Inc(PDnd, DndLen);
  PDsr:= Divisor;
  Inc(PDsr, DsrLen);
{$ELSE}
  PDnd:= Dividend + DndLen;
  PDsr:= Divisor + DsrLen;
{$ENDIF}

  repeat
    Dec(PDnd);    // PDnd points to (current) senior dividend/remainder limb
    Dec(PDsr);    // PDns points to senior divisor limb
    Dec(Quotient);

// Делим число, составленное из двух старших цифр делимого на старшую цифру
//   делителя; это даст нам оценку очередной цифры частного QGuess

    if PDnd^ < PDsr^ then begin
{$IFDEF OldVersion}
      Tmp.Lo:= GetLimb(PDnd, -1);
{$ELSE}
      Tmp.Lo:= (PDnd - 1)^;
{$ENDIF}
      Tmp.Hi:= PDnd^;
      QGuess.Lo:= Tmp.Value div PDsr^;
      QGuess.Hi:= 0;
      RGuess.Lo:= Tmp.Value mod PDsr^;
      RGuess.Hi:= 0;
    end
    else begin
      QGuess.Lo:= 0;
      QGuess.Hi:= 1;
{$IFDEF OldVersion}
      RGuess.Lo:= GetLimb(PDnd, -1);
{$ELSE}
      RGuess.Lo:= (PDnd - 1)^;
{$ENDIF}
      RGuess.Hi:= 0;
    end;

// Для точного значения цифры частного Q имеем
//   QGuess - 2 <= Q <= QGuess;
//   улучшаем оценку

    repeat
      if (QGuess.Hi = 0) then begin
//   yмножаем вторую по старшинству цифру делителя на QGuess
{$IFDEF OldVersion}
        Tmp.Value:= UInt64(GetLimb(PDsr, -1)) * QGuess.Value;
        if (Tmp.Hi < RGuess.Lo) then Break;
        if (Tmp.Hi = RGuess.Lo) and
           (Tmp.Lo <= GetLimb(PDnd, -2)) then Break;
{$ELSE}
        Tmp.Value:= UInt64((PDsr - 1)^) * QGuess.Value;
        if (Tmp.Hi < RGuess.Lo) then Break;
        if (Tmp.Hi = RGuess.Lo) and
           (Tmp.Lo <= (PDnd - 2)^) then Break;
{$ENDIF}
        Dec(QGuess.Lo);
      end
      else begin
        QGuess.Lo:= $FFFFFFFF;
        QGuess.Hi:= 0;
      end;
      RGuess.Value:= RGuess.Value + PDsr^;
    until RGuess.Hi <> 0;

// Здесь имеем QGuess - 1 <= Q <= QGuess;
// Вычитаем из делимого умноженный на QGuess делитель

    Count:= DsrLen;
{$IFDEF OldVersion}
    PDnd:= PDnd;
    Dec(PDnd, Count);
{$ELSE}
    PDnd:= PDnd - Count;
{$ENDIF}
    PDsr:= Divisor;
    Carry:= 0;
    repeat
      Tmp.Value:= PDsr^ * QGuess.Value + Carry;
      Carry:= Tmp.Hi;
      Tmp32:= PDnd^ - Tmp.Lo;
      if (Tmp32 > PDnd^) then Inc(Carry);
      PDnd^:= Tmp32;
      Inc(PDnd);
      Inc(PDsr);
      Dec(Count);
    until Count = 0;

//todo:
    Tmp32:= PDnd^ - Carry;
    if (Tmp32 > PDnd^) then begin
// если мы попали сюда значит QGuess = Q + 1;
// прибавляем делитель
      Count:= DsrLen;
{$IFDEF OldVersion}
      PDnd:= PDnd;
      Dec(PDnd, Count);
{$ELSE}
      PDnd:= PDnd - Count;
{$ENDIF}
      PDsr:= Divisor;
      CarryIn:= False;

      repeat
        Tmp32:= PDnd^ + PDsr^;
        CarryOut:= Tmp32 < PDnd^;
        Inc(PDsr);
        if CarryIn then begin
          Inc(Tmp32);
          CarryOut:= CarryOut or (Tmp32 = 0);
        end;
        CarryIn:= CarryOut;
        PDnd^:= Tmp32;
        Inc(PDnd);
        Dec(Count);
      until Count = 0;

      Assert(CarryIn);

      Dec(QGuess.Lo);
    end;

    Quotient^:= QGuess.Lo;
//    Dec(Quotient);
    Dec(LoopCount);
  until LoopCount = 0;

end;

end.
