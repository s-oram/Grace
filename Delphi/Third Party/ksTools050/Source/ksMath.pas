{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksMath;

interface

uses SysUtils, Math;

type
  PksComplex = ^TksComplex;
  TksComplex = packed record
    Re, Im: Extended;

    procedure Assign(R, I: Extended);

    class operator Implicit(const A: Extended): TksComplex;

    class operator Add(const A, B: TksComplex): TksComplex;
    class operator Subtract(const A, B: TksComplex): TksComplex;
    class operator Multiply(const A, B: TksComplex): TksComplex;
    class operator Divide(const A, B: TksComplex): TksComplex;
    class operator Negative(const A: TksComplex): TksComplex;

    class operator Equal(const A, B: TksComplex): Boolean;
    class operator NotEqual(const A, B: TksComplex): Boolean;

    class function Abs(const AValue: TksComplex): Extended; static;
    class function Phase(const AValue: TksComplex): Extended; static;
    class function Sqr(const AValue: TksComplex): TksComplex; static;
    class function Sqrt(const AValue: TksComplex): TksComplex; static;

    class function Exp(const AValue: TksComplex): TksComplex; static;
    class function Ln(const AValue: TksComplex): TksComplex; static;
    class function Cos(const AValue: TksComplex): TksComplex; static;
    class function Sin(const AValue: TksComplex): TksComplex; static;
    class function Tan(const AValue: TksComplex): TksComplex; static;
  end;

  PksInteger = ^TksInteger;
  TksInteger = packed record
  private
    FData: array of LongWord;
    function GetData(Index: Integer): LongWord;
//    procedure SetData(Index: Integer; Value: LongWord);
    procedure Normalize;
    procedure SetCapacity(Value: Integer);
    procedure Grow;
  public
    procedure AssignData(const Value: array of LongWord; Sign: Integer = 0);
    function RefCount: LongWord;
    function Capacity: LongWord;
    function IsZero: Boolean;
    function AsString: string;
    function FromString(const S: string): Boolean;
    procedure Assign(const Value: TksInteger);

// the Self- prefixed routines may lead to side-effect
//   if FData array is not unique (shared between several TksIntegers)
    procedure SelfAdd(const Value: TksInteger);
    procedure SelfAddCardinal(Value: LongWord);

    procedure SelfSub(const Value: TksInteger);
    procedure SelfSubCardinal(Value: LongWord);

    procedure SelfMulCardinal(Value: LongWord);
    function SelfDivModCardinal(Limb: LongWord): LongWord;
    procedure SelfDivMod(const B: TksInteger; var Q, R: TksInteger);

    property Data[Index: Integer]: LongWord read GetData;// write SetData;

    class operator Implicit(const A: LongWord): TksInteger;
    class operator Implicit(const A: LongInt): TksInteger;

    class operator Add(const A, B: TksInteger): TksInteger;
    class operator Subtract(const A, B: TksInteger): TksInteger;
    class operator Multiply(const A, B: TksInteger): TksInteger;
    class operator IntDivide(const A, B: TksInteger): TksInteger;
    class operator Modulus(const A, B: TksInteger): TksInteger;

//    class operator Add(const A: TksInteger; B: LongWord): TksInteger;
  end;

procedure ComplexFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
procedure RealFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
procedure RealFFT2(Data1, Data2: Pointer; fft1, fft2: Pointer; DataSize: Integer);
procedure SinFFT(Data: Pointer; DataSize: Integer);
procedure Cos1FFT(Data: Pointer; DataSize: Integer);
procedure Cos2FFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
procedure RealCorr(Data1, Data2: Pointer; Corr: Pointer; DataSize: Integer);
procedure RealAutoCorr(Data: Pointer; DataSize: Integer; Spectrum: Boolean = False);
function MaxPowerOfTwo(Value: Integer): Integer;
function RFTSpectrum(Data: Pointer; DataSize, Index: Integer): TksComplex;
function RFTAmplitude(Data: Pointer; DataSize, Index: Integer): Extended;
function RFTPhase(Data: Pointer; DataSize, Index: Integer): Extended;

implementation

uses arrProcs;

class operator TksComplex.Implicit(const A: Extended): TksComplex;
begin
  Result.Re:= A;
  Result.Im:= 0;
end;

class operator TksComplex.Add(const A, B: TksComplex): TksComplex;
begin
  Result.Re:= A.Re + B.Re;
  Result.Im:= A.Im + B.Im;
end;

class operator TksComplex.Subtract(const A, B: TksComplex): TksComplex;
begin
  Result.Re:= A.Re - B.Re;
  Result.Im:= A.Im - B.Im;
end;

class operator TksComplex.Multiply(const A, B: TksComplex): TksComplex;
begin
  Result.Re:= A.Re * B.Re - A.Im * B.Im;
  Result.Im:= A.Re * B.Im + A.Im * B.Re;
end;

class operator TksComplex.Negative(const A: TksComplex): TksComplex;
begin
  Result.Re:= - A.Re;
  Result.Im:= - A.Im;
end;

class operator TksComplex.Divide(const A, B: TksComplex): TksComplex;
var
  D: Extended;

begin
  D:= System.Sqr(B.Re) + System.Sqr(B.Im);
  Result.Re:= (A.Re * B.Re + A.Im * B.Im) / D;
  Result.Im:= (A.Im * B.Re - A.Re * B.Im) / D;
end;

class operator TksComplex.Equal(const A, B: TksComplex): Boolean;
begin
  Result:= (A.Re = B.Re) and (A.Im = B.Im);
end;

class operator TksComplex.NotEqual(const A, B: TksComplex): Boolean;
begin
  Result:= (A.Re <> B.Re) or (A.Im <> B.Im);
end;

{
function TksComplex.IsZero: Boolean;
begin
  Result:= (Abs(Re) < UMinValue) and (Abs(Im) < UMinValue);
end;
}
procedure TksComplex.Assign(R, I: Extended);
begin
  Re:= R;
  Im:= I;
end;

class function TksComplex.Abs(const AValue: TksComplex): Extended;
begin
  Result:= System.Sqrt(System.Sqr(AValue.Re) + System.Sqr(AValue.Im));
end;

class function TksComplex.Phase(const AValue: TksComplex): Extended;
begin
  Result:= ArcTan2(AValue.Im, AValue.Re);
end;

class function TksComplex.Sqr(const AValue: TksComplex): TksComplex;
begin
  Result.Re:= System.Sqr(AValue.Re) - System.Sqr(AValue.Im);
  Result.Im:= 2 * AValue.Re * AValue.Im;
end;

class function TksComplex.Sqrt(const AValue: TksComplex): TksComplex;
var
  A: Extended;

begin
  if AValue.Re >= 0 then begin
    A:= Abs(AValue) + AValue.Re;
    if A = 0 then begin
      Result.Re:= 0;
      Result.Im:= 0;
    end
    else begin
      Result.Re:= System.Sqrt(A / 2);
      Result.Im:= AValue.Im / System.Sqrt(A * 2);
    end;
  end
  else begin
    A:= Abs(AValue) - AValue.Re;
    Result.Re:= System.Abs(AValue.Im) / System.Sqrt(A * 2);
    if AValue.Im < 0 then
      Result.Im:= - System.Sqrt(A / 2)
    else
      Result.Im:= System.Sqrt(A / 2);
  end;
end;

class function TksComplex.Exp(const AValue: TksComplex): TksComplex;
var
  A: Extended;

begin
  A:= System.Exp(AValue.Re);
  Result.Re:= A * System.Cos(AValue.Im);
  Result.Im:= A * System.Sin(AValue.Im);
end;

class function TksComplex.Ln(const AValue: TksComplex): TksComplex;
begin
  Result.Re:= System.Ln(System.Sqr(AValue.Re) + System.Sqr(AValue.Im)) * 0.5;
  Result.Im:= Math.ArcTan2(AValue.Im, AValue.Re);
end;

class function TksComplex.Cos(const AValue: TksComplex): TksComplex;
var
  Exp1, Exp2: Extended;

begin
  Exp1:= System.Exp(AValue.Im);
  Exp2:= 1/Exp1;
  Result.Re:= System.Cos(AValue.Re) * (Exp1 + Exp2) * 0.5;
  Result.Im:= System.Sin(AValue.Re) * (Exp2 - Exp1) * 0.5;
end;

class function TksComplex.Sin(const AValue: TksComplex): TksComplex;
var
  Exp1, Exp2: Extended;

begin
  Exp1:= System.Exp(AValue.Im);
  Exp2:= 1/Exp1;
  Result.Re:= System.Sin(AValue.Re) * (Exp1 + Exp2) * 0.5;
  Result.Im:= System.Cos(AValue.Re) * (Exp1 - Exp2) * 0.5;
end;

class function TksComplex.Tan(const AValue: TksComplex): TksComplex;
var
  A, Exp1, Exp2: Extended;

begin
  Exp1:= System.Exp(2 * AValue.Im);
  Exp2:= 1/Exp1;
  A:= System.Cos(2 * AValue.Re) + (Exp1 + Exp2) * 0.5;
  Result.Re:= System.Sin(2.0 * AValue.Re) / A;
  Result.Im:= (Exp1 - Exp2) * 0.5 / A;
end;

{ TksInteger }

function TksInteger.GetData(Index: Integer): LongWord;
var
  Used: LongInt;

begin
  Used:= Abs(LongInt(FData[0]));
  if (Index <= Used) then Result:= FData[Index]
  else Result:= 0;
end;

{
procedure TksInteger.SetData(Index: Integer; Value: LongWord);
var
  Used, NewUsed: LongInt;

begin
  Used:= Abs(LongInt(FData[0]));
  if Index = 0 then begin
    NewUsed:= Abs(LongInt(Value));
    if (NewUsed > Used) then SetLength(FData, NewUsed + 1)
    else FData:= Copy(FData);
    FData[0]:= Value;
  end
  else if (Index <= Used) then begin
    FData:= Copy(FData);
    FData[Index]:= Value;
  end;
end;
}

// TksInteger uses 256-bit (8 longwords) allocation granularity
//   add 1 limb space for Used field and align the total allocation value
procedure TksInteger.SetCapacity(Value: Integer);
begin
  if Length(FData) < Value + 1 then
    SetLength(FData, (Value + 1 + 7) and not 7);
end;

procedure TksInteger.Grow;
begin
  SetLength(FData, (Length(FData) + 8) and not 7);
end;

procedure TksInteger.Normalize;
var
  Used: LongInt;

begin
  Used:= Abs(LongInt(FData[0]));
  while (Used > 0) and (FData[Used] = 0) do
    Dec(Used);
  if Used = 0 then FData[0]:= 1
  else if (LongInt(FData[0]) >= 0)
    then FData[0]:= LongWord(Used)
    else FData[0]:= LongWord(-Used);
end;

// Unit test support
function TksInteger.RefCount: LongWord;
var
  P: PLongWord;

begin
  P:= Pointer(FData);
  Dec(P, 2);
  Result:= P^;
end;

function TksInteger.Capacity: LongWord;
//var
//  P: PLongWord;

begin
  Result:= Length(FData) - 1;
//  P:= Pointer(FData);
//  Dec(P, 1);
//  Result:= P^ - 1;
end;

procedure TksInteger.AssignData(const Value: array of LongWord;  Sign: Integer);
var
  N: Integer;

begin
  N:= Length(Value);
  if N = 0 then begin
    SetCapacity(1);
    FData[0]:= 1;
    FData[1]:= 0;
    Exit;
  end;
  SetCapacity(N);
  if (Sign < 0) then FData[0]:= -N
  else FData[0]:= N;
  Move(Value[0], FData[1], N * SizeOf(LongWord));
// normalization is required since the senior Value limbs can be zero
  Normalize;
end;

procedure TksInteger.Assign(const Value: TksInteger);
var
  L: Integer;

begin
  if not Assigned(Value.FData) then FData:= nil
  else begin
    L:= Abs(LongInt(Value.FData[0]));
    SetCapacity(L);
    Move(Value.FData[0], FData[0], (L + 1) * SizeOf(LongWord));
  end;
end;

function TksInteger.IsZero: Boolean;
begin
//  Result:= (not Assigned(FData)) or ((FData[0] = 1) and (FData[1] = 0));
  Result:= (FData[0] = 1) and (FData[1] = 0);
end;

{ TksInteger <--> string conversions }
function TksInteger.AsString: string;
var
  Tmp: TksInteger;
  Used: LongInt;
  I, J: Integer;
  Digits: array of Byte;
  IsMinus: Boolean;

begin
  Result:= '';
  if (FData = nil) then Exit;
  if IsZero then begin
    Result:= '0';
    Exit;
  end;

  Used:= LongInt(FData[0]);
  IsMinus:= (Used < 0);
  Used:= Abs(Used);

// 10 decimal digits per 1 limb is sufficient
  SetLength(Digits, Used * 10);

  Tmp.Assign(Self);
  I:= 0;
  while not Tmp.IsZero do begin
    Digits[I]:= Tmp.SelfDivModCardinal(10);
    Inc(I);
  end;

  if IsMinus then begin
//    Inc(I);
    SetLength(Result, I + 1);
    Result[1]:= '-';
  end
  else begin
    SetLength(Result, I);
  end;

  for J:= 1 to I do begin
    Result[J + Ord(IsMinus)]:= Chr(Ord('0') + Digits[I - J]);
  end;

end;

function TksInteger.FromString(const S: string): Boolean;
var
  Minus: Boolean;
  I, L, N: Integer;
  Limb: LongWord;
  Digit: LongWord;
  Ch: Char;

begin
  L:= Length(S);
  if L = 0 then begin
    SetLength(FData, 2);
    FData[0]:= 1;
    FData[1]:= 0;
    Result:= True;
    Exit;
  end;
  I:= 1;
  Minus:= S[1] = '-';
  if Minus then Inc(I);
  if L < I then begin
    SetLength(FData, 2);
    FData[0]:= 1;
    FData[1]:= 0;
    Result:= False;
    Exit;
  end;
  if S[I] = '$' then begin
    Inc(I);
    if L < I then begin
      SetLength(FData, 2);
      FData[0]:= 1;
      FData[1]:= 0;
      Result:= False;
      Exit;
    end;
    N:= L - I + 1;           // number of hex digits
    SetLength(FData, (N + 7) shr 3 + 1);
    N:= 0;
    Limb:= 0;
    repeat
      Ch:= S[L - N];
      case Ch of
        '0'..'9': Digit:= Ord(Ch) - Ord('0');
        'A'..'F': Digit:= 10 + Ord(Ch) - Ord('A');
        'a'..'f': Digit:= 10 + Ord(Ch) - Ord('a');
      else
        FData[0]:= 1;
        FData[1]:= 0;
        Result:= False;
        Exit;
      end;
      Limb:= Limb + (Digit shl ((N and 7) shl 2));
      Inc(N);
      if N and 7 = 0 then begin
        FData[N shr 3]:= Limb;
        Limb:= 0;
      end;
    until I + N > L;
    if N and 7 <> 0 then
      FData[(N shr 3) + 1] := Limb;

    N:= (N + 7) shr 3;
    if Minus then
      FData[0]:= LongWord(-N)
    else
      FData[0]:= LongWord(N);
    Normalize;
  end
  else begin
               // number of decimal digits
    N:= L - I + 1;

// верхняя оценка числа 32-битных двойных слов для размещения N десятичных цифр
//   на основе того что 9 десятичных цифр заведомо умещаются в двойном слове
    SetCapacity((N + 8) div 9);

    FData[0]:= 1;
    FData[1]:= 0;
    repeat
      Ch:= S[I];
      case Ch of
        '0'..'9': Digit:= Ord(Ch) - Ord('0');
      else
        FData[0]:= 1;
        FData[1]:= 0;
        Result:= False;
        Exit;
      end;
      Inc(I);
      SelfMulCardinal(10);
      SelfAddCardinal(Digit);
    until I > L;
    if Minus and ((FData[0] > 1) or (FData[1] <> 0)) then
      FData[0]:= LongWord(-LongInt(FData[0]));
  end;
  Result:= True;
end;

procedure TksInteger.SelfAdd(const Value: TksInteger);
var
  UsedA, UsedB: LongInt;
//  Tmp: array of LongWord;

begin
  if (Value.FData[0] = 1) and (Value.FData[1] = 0) then Exit;

  if (FData[0] = 1) and (FData[1] = 0) then begin
    Assign(Value);
    Exit;
  end;

  UsedA:= Abs(LongInt(FData[0]));
  UsedB:= Abs(LongInt(Value.FData[0]));

  if LongInt(FData[0]) xor LongInt(Value.FData[0]) >= 0 then begin
// operands have the same sign - add arrays

    if UsedA < UsedB then begin
      SetCapacity(UsedB + 1);   // allocate 1 limb more
                              //   the additional limb is not zeroed
      FillChar(FData[UsedA + 1], (UsedB - UsedA) * SizeOf(LongWord), 0);
      UsedA:= UsedB;
    end
    else if UsedA = Length(FData) then Grow;

    if arrSelfAdd(@FData[1], @Value.FData[1], UsedA, UsedB) then begin
      Inc(UsedA);
    end;
    if LongInt(FData[0]) < 0 then UsedA:= -UsedA;
    FData[0]:= LongWord(UsedA);
  end
  else begin
// operands have opposite signs - SUB lesser from greater
    if (UsedA > UsedB) or ((UsedA = UsedB)
        and (arrCmp(@FData[1], @Value.FData[1], UsedA) >= 0)) then begin
      arrSelfSub(@FData[1], @Value.FData[1], UsedA, UsedB);
      Normalize;
    end
    else begin
//      SetLength(Tmp, UsedA);
//      Move(FData[1], Tmp[0], UsedA * SizeOf(LongWord));
      SetCapacity(UsedB);
//      arrSub(@Value.FData[1], @Tmp[0], @FData[1], UsedB, UsedA);
      arrSub(@Value.FData[1], @FData[1], @FData[1], UsedB, UsedA);
      FData[0]:= Value.FData[0];
      Normalize;
    end;
  end;
end;

procedure TksInteger.SelfSub(const Value: TksInteger);
var
  UsedA, UsedB: LongInt;
//  Tmp: array of LongWord;

begin
  if (Value.FData[0] = 1) and (Value.FData[1] = 0) then Exit;

  if (FData[0] = 1) and (FData[1] = 0) then begin
    Assign(Value);
    FData[0]:= LongWord(-LongInt(FData[0]));
    Exit;
  end;

  UsedA:= Abs(LongInt(FData[0]));
  UsedB:= Abs(LongInt(Value.FData[0]));

  if LongInt(FData[0]) xor LongInt(Value.FData[0]) < 0 then begin
// operands have opposite sign - add arrays

    if UsedA < UsedB then begin
      SetCapacity(UsedB + 1);   // allocate 1 limb more
                              //   the additional limb is not zeroed
      FillChar(FData[UsedA + 1], (UsedB - UsedA) * SizeOf(LongWord), 0);
      UsedA:= UsedB;
    end
    else if UsedA = Length(FData) then Grow;

    if arrSelfAdd(@FData[1], @Value.FData[1], UsedA, UsedB) then begin
      Inc(UsedA);
    end;
    if LongInt(FData[0]) < 0 then UsedA:= -UsedA;
    FData[0]:= LongWord(UsedA);
  end
  else begin
// operands have same signs - SUB lesser from greater
    if (UsedA > UsedB) or ((UsedA = UsedB)
        and (arrCmp(@FData[1], @Value.FData[1], UsedA) >= 0)) then begin
      arrSelfSub(@FData[1], @Value.FData[1], UsedA, UsedB);
      Normalize;
    end
    else begin
//      SetLength(Tmp, UsedA);
//      Move(FData[1], Tmp[0], UsedA * SizeOf(LongWord));
      SetCapacity(UsedB);
//      arrSub(@Value.FData[1], @Tmp[0], @FData[1], UsedB, UsedA);
      arrSub(@Value.FData[1], @FData[1], @FData[1], UsedB, UsedA);
      FData[0]:= LongWord(-LongInt(Value.FData[0]));
      Normalize;
    end;
  end;
end;

procedure TksInteger.SelfAddCardinal(Value: LongWord);
var
  Used: LongInt;
  Minus: Boolean;

begin
  Used:= LongInt(FData[0]);
  Minus:= Used < 0;
  Used:= Abs(Used);
  if not Minus then begin
    if Used = Length(FData) then Grow;
    if arrSelfAddLimb(@FData[1], Value, Used) then begin
      FData[0]:= LongWord(Used + 1);
    end;
  end
  else if (Used > 1) then begin
    arrSelfSubLimb(@FData[1], Value, Used);
    if FData[Used] = 0 then begin
      FData[0]:= LongWord(-Used + 1);
    end;
  end
  else begin
    if FData[1] > Value then begin
      Dec(FData[1], Value);
    end
    else begin
      FData[1]:= Value - FData[1];
      FData[0]:= LongWord(1);
    end;
  end;
end;

procedure TksInteger.SelfSubCardinal(Value: LongWord);
var
  Used: LongInt;
  Minus: Boolean;

begin
  Used:= LongInt(FData[0]);
  Minus:= Used < 0;
  Used:= Abs(Used);
  if Minus then begin
    if Used = Length(FData) then Grow;
    if arrSelfAddLimb(@FData[1], Value, Used) then begin
      FData[0]:= LongWord(-(Used + 1));
    end;
  end
  else if (Used > 1) then begin
    arrSelfSubLimb(@FData[1], Value, Used);
    if FData[Used] = 0 then begin
      FData[0]:= LongWord(Used - 1);
    end;
  end
  else begin
    if FData[1] >= Value then begin
      Dec(FData[1], Value);
    end
    else begin
      FData[1]:= Value - FData[1];
      FData[0]:= LongWord(-1);
    end;
  end;
end;

procedure TksInteger.SelfMulCardinal(Value: LongWord);
var
  Used: LongInt;

begin
  Used:= Abs(LongInt(FData[0]));
  if Used = Length(FData) then Grow;
  if arrSelfMulLimb(@FData[1], Value, Used) then begin
    Inc(Used);
    if LongInt(FData[0]) < 0 then Used:= -Used;
    FData[0]:= LongWord(Used);
  end;
end;

function SeniorBit(Value: LongWord): Integer;
asm
        OR    EAX,EAX
        JZ    @@Done
        BSR   EAX,EAX
        INC   EAX
@@Done:
end;

procedure TksInteger.SelfDivMod(const B: TksInteger; var Q, R: TksInteger);
var
  Cond: Boolean;
  Diff: Integer;
  Dividend, Divisor: TksInteger;
  Limb: LongWord;
  UsedA, UsedB, UsedD, UsedQ: LongInt;
  Shift: Integer;

begin
  if B.IsZero then
    raise EZeroDivide.Create('Zero Divide');

  Cond:= IsZero;

  if not Cond then begin
    UsedA:= Abs(LongInt(FData[0]));
    UsedB:= Abs(LongInt(B.FData[0]));
    Cond:= (UsedA < UsedB);
    if not Cond and (UsedA = UsedB) then begin
      Diff:= arrCmp(@FData[1], @B.FData[1], UsedB);
      if Diff = 0 then begin
        Q:= LongWord(1);
        R:= LongWord(0);
        Exit;
      end
      else if Diff < 0 then Cond:= True;
    end;
  end;

  if Cond then begin
    Q:= LongWord(0);
    R.Assign(Self);
    Exit;
  end;

  SetLength(Q.FData, UsedA - UsedB + 2);
  SetLength(R.FData, UsedB + 1);
  if (UsedB = 1) then begin
    if (UsedA = 1) then begin
      Q.FData[1]:= FData[1] div B.FData[1];
      R.FData[1]:= FData[1] mod B.FData[1];
    end
    else begin
      R.FData[1]:= arrDivModLimb(@FData[1], @Q.FData[1], UsedA, B.FData[1]);
      if Q.FData[UsedA] = 0 then Dec(UsedA);
    end;

    if (LongInt(FData[0]) xor LongInt(B.FData[0]) >= 0)
//   or ((UsedA = 1) and (Q.FData[1] = 0))  never happens since dividend > divisor
        then
          Q.FData[0]:= LongWord(UsedA)
        else
          Q.FData[0]:= LongWord(-UsedA);

    if (LongInt(FData[0]) >= 0) or (R.FData[1] = 0)
      then
        R.FData[0]:= 1
      else
        R.FData[0]:= LongWord(-1);

    Exit;
  end;

// calc the number of bits to shift
  Limb:= B.FData[UsedB];
  Shift:= 32 - SeniorBit(Limb);
  SetLength(Divisor.FData, UsedB + 1);
  Divisor.FData[0]:= UsedB;
  arrShlShort(@B.FData[1], @Divisor.FData[1], UsedB, Shift);

  SetLength(Dividend.FData, UsedA + 2);
  UsedD:= arrShlShort(@FData[1], @Dividend.FData[1], UsedA, Shift);

  if UsedD = UsedA then
    Dividend.FData[UsedA + 1]:= 0;
  Dividend.FData[0]:= UsedA + 1;    // ??

  UsedQ:= UsedA - UsedB + 1;
  SetLength(Q.FData, UsedQ + 1);

  arrNormDivMod(@Dividend.FData[1], @Divisor.FData[1], @Q.FData[1],
                UsedA + 1, UsedB);
  arrShrShort(@Dividend.FData[1], @R.FData[1], UsedB, Shift);

  if LongInt(FData[0]) xor LongInt(B.FData[0]) >= 0
    then
      Q.FData[0]:= LongWord(UsedQ)
    else
      Q.FData[0]:= LongWord(-UsedQ);

  if (LongInt(FData[0]) >= 0)
    then
      R.FData[0]:= LongWord(UsedB)
    else
      R.FData[0]:= LongWord(-UsedB);

  Q.Normalize;
  R.Normalize;
end;

function TksInteger.SelfDivModCardinal(Limb: LongWord): LongWord;
var
  L: LongInt;
//  R: LongWord;

begin
  L:= Abs(LongInt(FData[0]));
  Result:= arrSelfDivModLimb(@FData[1], L, Limb);
  if (FData[L] = 0) then begin
    if (L > 1) then begin
      Dec(L);
      if LongInt(FData[0]) < 0 then L:= -L;
    end;
    FData[0]:= LongWord(L);
  end;
end;

{
class operator TksInteger.Add(const A: TksInteger; B: LongWord): TksInteger;
var
  UsedA: LongInt;

begin
  if B = 0 then begin
    Result.Assign(A);
    Exit;
  end;

  if A.IsZero then begin
    SetLength(Result.FData, 2);
    Result.FData[0]:= 1;
    Result.FData[1]:= B;
    Exit;
  end;

  UsedA:= Abs(LongInt(A.FData[0]));

  if LongInt(A.FData[0]) >= 0 then begin
    SetLength(Result.FData, UsedA + 2);
    if arrAddLimb(@A.FData[1], B, @Result.FData[1], UsedA)
      then
        Inc(UsedA);
    Result.FData[0]:= UsedA;
  end
  else begin
    SetLength(Result.FData, UsedA + 1);
    if arrSubLimb(@A.FData[1], B, @Result.FData[1], UsedA)
      or ((UsedA = 1) and (Result.FData[1] = 0))
        then
          Result.FData[0]:= 1
        else begin
          if Result.FData[UsedA] = 0 then Dec(UsedA);
          Result.FData[0]:= LongWord(-UsedA);
        end;
  end;
end;
}
class operator TksInteger.Add(const A, B: TksInteger): TksInteger;
var
  UsedA, UsedB: LongInt;

begin
  if B.IsZero then begin
    Result.Assign(A);
    Exit;
  end;

  if A.IsZero then begin
    Result.Assign(B);
    Exit;
  end;

  UsedA:= Abs(LongInt(A.FData[0]));
  UsedB:= Abs(LongInt(B.FData[0]));

  if LongInt(A.FData[0]) xor LongInt(B.FData[0]) >= 0 then begin
// Values have the same sign - ADD lesser to greater

    if UsedA >= UsedB then begin
      SetLength(Result.FData, UsedA + 2);
      if arrAdd(@A.FData[1], @B.FData[1], @Result.FData[1], UsedA, UsedB) then begin
        if LongInt(A.FData[0]) >= 0 then Result.FData[0]:= LongWord(UsedA + 1)
        else Result.FData[0]:= LongWord(-(UsedA + 1));
//        Result.Data[UsedA + 1]:= 1;
      end
      else begin
        Result.FData[0]:= A.FData[0];
      end
    end
    else begin
      SetLength(Result.FData, UsedB + 2);
      if arrAdd(@B.FData[1], @A.FData[1], @Result.FData[1], UsedB, UsedA) then begin
        if LongInt(B.FData[0]) >= 0 then Result.FData[0]:= LongWord(UsedB + 1)
        else Result.FData[0]:= LongWord(-(UsedB + 1));
//        Result.FData[UsedB + 1]:= 1;
      end
      else begin
        Result.FData[0]:= B.FData[0];
      end
    end
  end
  else begin
// Values have opposite signs - SUB lesser from greater
    if (UsedA > UsedB) or ((UsedA = UsedB)
        and (arrCmp(@A.FData[1], @B.FData[1], UsedA) >= 0)) then begin
      SetLength(Result.FData, UsedA + 1);
      Result.FData[0]:= A.FData[0];
      arrSub(@A.FData[1], @B.FData[1], @Result.FData[1], UsedA, UsedB);
      Result.Normalize;
    end
    else begin
      SetLength(Result.FData, UsedB + 1);
      Result.FData[0]:= B.FData[0];
      arrSub(@B.FData[1], @A.FData[1], @Result.FData[1], UsedB, UsedA);
      Result.Normalize;
    end;
  end;
end;

class operator TksInteger.Subtract(const A, B: TksInteger): TksInteger;
var
  UsedA, UsedB: LongInt;

begin
  if B.IsZero then begin
    Result.Assign(A);
    Exit;
  end;

  if A.IsZero then begin
    Result.Assign(B);
    if (Result.FData[0] > 1) or (Result.FData[1] <> 0) then begin
      Result.FData[0]:= LongWord(-LongInt(Result.FData[0]));
    end;
    Exit;
  end;

  UsedA:= Abs(LongInt(A.FData[0]));
  UsedB:= Abs(LongInt(B.FData[0]));

// check for undefined values - just return undefined value
//  if (UsedA = 0) then Exit(A);          // A undefined
//  if (UsedB = 0) then Exit(B);          // B undefined

  if LongInt(A.FData[0]) xor LongInt(B.FData[0]) >= 0 then begin
// Values have the same sign - SUB lesser from greater
    if (UsedA > UsedB) or ((UsedA = UsedB)
        and (arrCmp(@A.FData[1], @B.FData[1], UsedA) >= 0)) then begin
      SetLength(Result.FData, UsedA + 1);
      Result.FData[0]:= A.FData[0];
      arrSub(@A.FData[1], @B.FData[1], @Result.FData[1], UsedA, UsedB);
      Result.Normalize;
    end
    else begin
      SetLength(Result.FData, UsedB + 1);
      Result.FData[0]:= -B.FData[0];
      arrSub(@B.FData[1], @A.FData[1], @Result.FData[1], UsedB, UsedA);
      Result.Normalize;
    end;
  end
  else begin
// Values have opposite signs - ADD lesser to greater
    if UsedA >= UsedB then begin
      SetLength(Result.FData, UsedA + 2);
      if arrAdd(@A.FData[1], @B.FData[1], @Result.FData[1], UsedA, UsedB) then begin
        if LongInt(A.FData[0]) >= 0 then Result.FData[0]:= LongWord(UsedA + 1)
        else Result.FData[0]:= LongWord(-(UsedA + 1));
      end
      else begin
        Result.FData[0]:= A.FData[0];
      end
    end
    else begin
      SetLength(Result.FData, UsedB + 2);
      if arrAdd(@B.FData[1], @A.FData[1], @Result.FData[1], UsedB, UsedA) then begin
        if LongInt(B.FData[0]) <= 0 then Result.FData[0]:= LongWord(UsedB + 1)
        else Result.FData[0]:= LongWord(-(UsedB + 1));
      end
      else begin
        Result.FData[0]:= -B.FData[0];
      end
    end
  end;
end;

class operator TksInteger.Multiply(const A, B: TksInteger): TksInteger;
var
  UsedA, UsedB, Used: LongInt;

begin
  if A.IsZero or B.IsZero then begin
    Result:= LongWord(0);
    Exit;
  end;

  UsedA:= Abs(LongInt(A.FData[0]));
  UsedB:= Abs(LongInt(B.FData[0]));

// check for undefined values - just return undefined value
//  if (UsedA = 0) then Exit(A);          // A undefined
//  if (UsedB = 0) then Exit(B);          // B undefined

  Used:= UsedA + UsedB;
  SetLength(Result.FData, Used + 1);
  if UsedA >= UsedB then
    arrMul(@A.FData[1], @B.FData[1], @Result.FData[1], UsedA, UsedB)
  else
    arrMul(@B.FData[1], @A.FData[1], @Result.FData[1], UsedB, UsedA);

//  if Result.FData[Used] = 0 then Dec(Used);
  if LongInt(A.FData[0]) xor LongInt(B.FData[0]) < 0 then Used:= -Used;
  Result.FData[0]:= LongWord(Used);
  Result.Normalize;
end;

class operator TksInteger.IntDivide(const A, B: TksInteger): TksInteger;
var
  Remainder: TksInteger;

begin
  A.SelfDivMod(B, Result, Remainder);
end;

class operator TksInteger.Modulus(const A, B: TksInteger): TksInteger;
var
  Quotient: TksInteger;

begin
  A.SelfDivMod(B, Quotient, Result);
end;

class operator TksInteger.Implicit(const A: LongWord): TksInteger;
begin
  SetLength(Result.FData, 2);
  Result.FData[0]:= 1;
  Result.FData[1]:= A;
end;

class operator TksInteger.Implicit(const A: LongInt): TksInteger;
begin
  SetLength(Result.FData, 2);
  if A >= 0 then begin
    Result.FData[0]:= 1;
    Result.FData[1]:= LongWord(A);
  end
  else begin
    Result.FData[0]:= LongWord(-1);
    Result.FData[1]:= LongWord(-A);
  end;
end;

(* ************************************************************************ *)

type
  PCArray = ^TCArray;
  TCArray = array[0..$FFFFFF] of TksComplex;

  PDArray = ^TDArray;
  TDArray = array[0..$FFFFFF] of Extended;

procedure ComplexFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
var
  I, J, L, M, Step: Integer;
  Tmp: TksComplex;
  P1, P2: PksComplex;
  Theta, WT, TR, TI: Extended;
  WPR, WPI, WR, WI: Extended;

begin
                    // Бит - реверсивная перестановка исходных данных
  I:= 0;
  J:= 0;
  while (I < DataSize - 1) do begin
    if (J > I) then begin
      Tmp:= PCArray(Data)^[I];        // перестановка комплексных чисел
      PCArray(Data)^[I]:= PCArray(Data)^[J];
      PCArray(Data)^[J]:= Tmp;
    end;
    M:= DataSize shr 1;
    while (M >= 1) and (J >= M) do begin
      Dec(J, M);
      M:= M shr 1;
    end;
    Inc(J, M);
    Inc(I);
  end;
  L:= 1;
  while L < DataSize do begin
    Step:= L shl 1;
    Theta:= Pi/L;
    if Sign < 0 then Theta:= -Theta;

    WT:= Sin(0.5 * Theta);
    WPR:= -2.0 * WT * WT;                 // = Cos(Theta) - 1;
    WPI:= Sin(Theta);
    WR:= 1.0;
    WI:= 0.0;
    M:= 0;
    while M < L do begin
      I:= M;
      while I < DataSize do begin
        P2:= @PCArray(Data)^[I + L];      // умножаем 2-ю точку
        TR:= WR * P2^.Re - WI * P2^.Im;   //  на поворачивающий множитель
        TI:= WR * P2^.Im + WI * P2^.Re;
        P1:= @PCArray(Data)^[I];
        P2^.Re:= P1^.Re - TR;             // 2-я точка
        P2^.Im:= P1^.Im - TI;
        P1^.Re:= P1^.Re + TR;             // 1-я точка
        P1^.Im:= P1^.Im + TI;
        Inc(I, Step);
      end;
      WT:= WR;
      WR:= WR * WPR - WI * WPI + WR;
      WI:= WI * WPR + WT * WPI + WI;
      Inc(M);
    end;
    L:= Step;
  end;
end;

procedure RealFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
var
  Theta: Extended;
  C1, C2: Extended;
  P1, P2: PksComplex;
  H1, H2: TksComplex;
  WT, WPR, WPI, WR, WI: Extended;
  I: Integer;

begin
  DataSize:= DataSize shr 1;
  Theta:= Pi / DataSize;
  C1:= 0.5;
  if Sign < 0 then begin
    C2:= 0.5;
    Theta:= - Theta;
  end
  else begin
    C2:= -0.5;
    ComplexFFT(Data, DataSize, Sign);
  end;

// W = exp(pi * i * theta) = WR + i*WI

  WT:= Sin(0.5 * Theta);
  WPR:= -2.0 * WT * WT;                 // = Cos(Theta) - 1;
  WPI:= Sin(Theta);
  WR:= 1.0 + WPR;                       // = cos(theta)
  WI:= WPI;                             // = sin(theta)
  I:= 1;
  while I < DataSize shr 1 do begin     // case I=0 done separately
    P1:= @PCArray(Data)^[I];
    P2:= @PCArray(Data)^[DataSize - I];

    H1.Re:= C1 * (P1^.Re + P2^.Re);
    H1.Im:= C1 * (P1^.Im - P2^.Im);
    H2.Re:= -C2 * (P1^.Im + P2^.Im);
    H2.Im:= C2 * (P1^.Re - P2^.Re);
    P1^.Re:= H1.Re + WR * H2.Re - WI * H2.Im;
    P1^.Im:= H1.Im + WR * H2.Im + WI * H2.Re;
    P2^.Re:= H1.Re - WR * H2.Re + WI * H2.Im;
    P2^.Im:= -H1.Im + WR * H2.Im + WI * H2.Re;

    WT:= WR;
    WR:= WR + WR * WPR - WI * WPI;
    WI:= WI + WI * WPR + WT * WPI;
    Inc(I);
  end;
  P1:= @PCArray(Data)^[0];
  H1.Re:= P1^.Re;
  P1^.Re:= P1^.Re + P1^.Im;
  P1^.Im:= H1.Re - P1^.Im;
  if Sign < 0 then begin
    P1^.Re:= C1 * P1^.Re;
    P1^.Im:= C1 * P1^.Im;
    ComplexFFT(Data, DataSize, Sign);
  end;
end;

procedure RealFFT2(Data1, Data2: Pointer; fft1, fft2: Pointer; DataSize: Integer);
var
  I: Integer;
  P, M: TksComplex;
  P1, P2: PksComplex;

begin
  for I:= 0 to DataSize - 1 do begin
    with PCArray(fft1)^[I] do begin
      Re:= PDArray(Data1)^[I];
      Im:= PDArray(Data2)^[I];
    end;
  end;
  ComplexFFT(fft1, DataSize, 0);
  PCArray(fft2)^[0].Re:= PCArray(fft1)^[0].Im;
  PCArray(fft1)^[0].Im:= 0;
  PCArray(fft2)^[0].Im:= 0;
  for I:= 1 to (DataSize shr 1) do begin
    P1:= @PCArray(fft1)^[I];
    P2:= @PCArray(fft1)^[DataSize - I];
    P.Re:= 0.5 * (P1^.Re + P2^.Re);
    P.Im:= 0.5 * (P1^.Im + P2^.Im);
    M.Re:= 0.5 * (P1^.Re - P2^.Re);
    M.Im:= 0.5 * (P1^.Im - P2^.Im);
    with P1^ do begin
      Re:= P.Re;
      Im:= M.Im;
    end;
    with P2^ do begin       // комплесно сопр. P1^
      Re:= P.Re;
      Im:= -M.Im;
    end;
    with PCArray(fft2)^[I] do begin
      Re:= P.Im;
      Im:= -M.Re;
    end;
    with PCArray(fft2)^[DataSize - I] do begin
      Re:= P.Im;
      Im:= M.Re;
    end;
  end;
end;

procedure SinFFT(Data: Pointer; DataSize: Integer);
var
  N: Integer;
  Theta, Y1, Y2, Sum: Extended;
  WT, WPR, WPI, WR, WI: Extended;

begin
                          // initialize trigonometric recurrence
  WI:= 0.0;
  WR:= 1.0;
  Theta:= Pi/DataSize;
  WT:= Sin(0.5 * Theta);
  WPR:= -2.0 * WT * WT;   // = Cos(Theta) - 1
  WPI:= Sin(Theta);
                          // PDArray(Data)^[0] must be zero for Sine Transform
  PDArray(Data)^[0]:= 0.0;
                          // construct the auxiliary array from the Data
  for N:= 1 to DataSize shr 1 do begin
// calculate Sin(N * Theta) recursively:
//   Sin(N * Theta) = Sin((N-1) * Theta) + Sin((N-1) * Theta) * WPR
//                                       + Cos((N-1) * Theta) * WPI
    WT:= WR;
    WR:= WR*WPR - WI*WPI + WR;    // = Cos(N * Theta)
    WI:= WI*WPR + WT*WPI + WI;    // = Sin(N * Theta)
    Y1:= WI*(PDArray(Data)^[N] + PDArray(Data)^[DataSize - N]);
    Y2:= 0.5*(PDArray(Data)^[N] - PDArray(Data)^[DataSize - N]);
    PDArray(Data)^[N]:= Y1 + Y2;
    PDArray(Data)^[DataSize - N]:= Y1 - Y2;
  end;
                        // FFT the auxiliary array
  RealFFT(Data, DataSize, 0);
  PDArray(Data)^[0]:= PDArray(Data)^[0] * 0.5;
  PDArray(Data)^[1]:= 0;
  Sum:= 0.0;
  for N:= 0 to DataSize shr 1 - 1 do begin
    Sum:= Sum + PDArray(Data)^[2*N];
    PDArray(Data)^[2*N]:= PDArray(Data)^[2*N + 1];
    PDArray(Data)^[2*N+1]:= Sum;
  end;
end;

procedure Cos1FFT(Data: Pointer; DataSize: Integer);
var
  N: Integer;
  Theta, Y1, Y2, Sum: Extended;
  WT, WPR, WPI, WR, WI: Extended;

begin
                          // initialize trigonometric recurrence
  WI:= 0.0;               // = Sin(0 * Theta)
  WR:= 1.0;               // = Cos(0 * Theta)
  Theta:= Pi/DataSize;
  WT:= Sin(0.5 * Theta);
  WPR:= -2.0 * WT * WT;   // = Cos(Theta) - 1
  WPI:= Sin(Theta);

  Sum:= 0.5*(PDArray(Data)^[0] - PDArray(Data)^[DataSize]);
  PDArray(Data)^[0]:= 0.5*(PDArray(Data)^[0] + PDArray(Data)^[DataSize]);
  for N:= 1 to DataSize shr 1 - 1 do begin
    WT:= WR;
    WR:= WR*WPR - WI*WPI + WR;    // = Cos(N * Theta)
    WI:= WI*WPR + WT*WPI + WI;    // = Sin(N * Theta)
    Y1:= 0.5*(PDArray(Data)^[N] + PDArray(Data)^[DataSize - N]);
    Y2:= PDArray(Data)^[N] - PDArray(Data)^[DataSize - N];
    PDArray(Data)^[N]:= Y1 - WI * Y2;
    PDArray(Data)^[DataSize - N]:= Y1 + WI * Y2;
    Sum:= Sum + WR * Y2;
  end;
  RealFFT(Data, DataSize, 0);
  PDArray(Data)^[DataSize]:= PDArray(Data)^[1];
  PDArray(Data)^[1]:= Sum;
  N:= 3;
  while N < DataSize do begin
    Sum:= Sum + PDArray(Data)^[N];
    PDArray(Data)^[N]:= Sum;
    N:= N + 2;
  end;
end;

procedure Cos2FFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
var
  N: Integer;
  Theta, Y1, Y2, YT, Sum, Sum1: Extended;
  WT, WPR, WPI, WR, WI, WR1, WI1: Extended;

begin
                          // initialize trigonometric recurrence
  WI:= 0.0;               // = Sin(0 * Theta)
  WR:= 1.0;               // = Cos(0 * Theta)
  Theta:= Pi/DataSize;

  WR1:= Cos(0.5 * Theta);
  WI1:= Sin(0.5 * Theta);
  WPR:= -2.0 * Sqr(WI1);
  WPI:= Sin(Theta);
  if (Sign >= 0) then begin     // Forward transform.
    for N:= 0 to DataSize shr 1 - 1 do begin
                                // Calculate the auxiliary function.
      Y1:= 0.5 * (PDArray(Data)^[N] + PDArray(Data)^[DataSize - N - 1]);
      Y2:= WI1 * (PDArray(Data)^[N] - PDArray(Data)^[DataSize - N - 1]);
      PDArray(Data)^[N]:= Y1 + Y2;
      PDArray(Data)^[DataSize - N - 1]:= Y1 - Y2;
                                        // Carry out the recurrence.
      WT:= WR1;
      WR1:= WR1*WPR - WI1*WPI + WR1;    // = Cos(N * Theta)
      WI1:= WI1*WPR + WT*WPI + WI1;     // = Sin(N * Theta)
    end;
                                // Transform the auxiliary function.
    RealFFT(Data, DataSize, 0);

    N:= 2;
    while N < DataSize do begin
      WT:= WR;
      WR:= WR*WPR - WI*WPI + WR;    // = Cos(N * Theta)
      WI:= WI*WPR + WT*WPI + WI;    // = Sin(N * Theta)
      Y1:= PDArray(Data)^[N] * WR - PDArray(Data)^[N + 1] * WI;
      Y2:= PDArray(Data)^[N + 1] * WR + PDArray(Data)^[N] * WI;
      PDArray(Data)^[N]:= Y1;
      PDArray(Data)^[N + 1]:= Y2;
      Inc(N, 2);
    end;

    Sum:= 0.5 * PDArray(Data)^[1]; // Initialize recurrence for odd terms
    N:= DataSize - 1;
    while N > 0 do begin
      Sum1:= Sum;
      Sum:= Sum + PDArray(Data)^[N];
      PDArray(Data)^[N]:= Sum1;
      Dec(N, 2);
    end;
  end
  else if (Sign < 0) then begin
                                // Inverse transform.
    YT:= PDArray(Data)^[DataSize - 1];
    N:= DataSize - 1;
    while N >= 3 do begin
                                // Form difference of odd terms.
      PDArray(Data)^[N]:= PDArray(Data)^[N - 2] - PDArray(Data)^[N];
      Dec(N, 2);
    end;
    PDArray(Data)^[1]:= 2.0 * YT;

                                    //Calculate Rk and Ik.
    N:= 2;
    while (N < DataSize) do begin
      WT:= WR;
      WR:= WR*WPR - WI*WPI + WR;    // = Cos(N * Theta)
      WI:= WI*WPR + WT*WPI + WI;    // = Sin(N * Theta)
      Y1:= PDArray(Data)^[N] * WR + PDArray(Data)^[N + 1] * WI;
      Y2:= PDArray(Data)^[N + 1] * WR - PDArray(Data)^[N] * WI;
      PDArray(Data)^[N]:= Y1;
      PDArray(Data)^[N + 1]:= Y2;
      Inc(N, 2);
    end;
    RealFFT(Data, DataSize, -1);

                                    //Invert auxiliary array.
    for N:= 0 to DataSize shr 1 - 1 do begin
      Y1:= PDArray(Data)^[N] + PDArray(Data)^[DataSize - N - 1];
      Y2:= (0.5/WI1) * (PDArray(Data)^[N] - PDArray(Data)^[DataSize - N - 1]);
      PDArray(Data)^[N]:= 0.5*(Y1 + Y2);
      PDArray(Data)^[DataSize - N - 1]:= 0.5*(Y1 - Y2);

      WT:= WR1;
      WR1:= WR1*WPR - WI1*WPI + WR1;    // = Cos(N * Theta)
      WI1:= WI1*WPR + WT*WPI + WI1;     // = Sin(N * Theta)
    end;
  end;

end;

procedure RealCorr(Data1, Data2: Pointer; Corr: Pointer; DataSize: Integer);
var
  fft1: Pointer;
  I: Integer;
  C1, C2: TksComplex;
  P1, P2, P3: PksComplex;
  D: Extended;

begin
  GetMem(fft1, DataSize * SizeOf(TksComplex));
  try
    for I:= 0 to DataSize - 1 do begin
      with PCArray(fft1)^[I] do begin
        Re:= PDArray(Data1)^[I];
        Im:= PDArray(Data2)^[I];
      end;
    end;
    ComplexFFT(fft1, DataSize, 0);

    D:= 1 / (DataSize shr 1);         // нормализующий множитель

// умножаем фурье-образ первой функции
//   на комплесно-сопряжённый фурье-образ второй функции

    P1:= @PCArray(fft1)^[0];
    P2:= @PCArray(fft1)^[DataSize];   // out of bounds - that's OK
    P3:= @PCArray(Corr)^[0];

    P3^.Re:= P1^.Re * P1^.Im * D;

    for I:= 1 to (DataSize shr 1) - 1 do begin
      Inc(P1);
      Dec(P2);                    // decremented - not out of bounds
      Inc(P3);

// значение первой функции
      C1.Re:= 0.5 * (P1^.Re + P2^.Re);
      C1.Im:= 0.5 * (P1^.Im - P2^.Im);

// комплексно сопряжённое значение второй функции
      C2.Re:= 0.5 * (P1^.Im + P2^.Im);
      C2.Im:= 0.5 * (P1^.Re - P2^.Re);

// первое умножается на второе:
//   P3^:= (C1.Re, C1.Im) * (C2.Re, C2.Im);

      P3^.Re:= (C1.Re * C2.Re - C1.Im * C2.Im) * D;
      P3^.Im:= (C1.Re * C2.Im + C1.Im * C2.Re) * D;
    end;

    Inc(P1);
    P3:= @PCArray(Corr)^[0];
    P3^.Im:= P1^.Re * P1^.Im * D;

    RealFFT(Corr, DataSize, -1);

  finally
    FreeMem(fft1);
  end;
end;

procedure RealAutoCorr(Data: Pointer; DataSize: Integer; Spectrum: Boolean = False);
var
  I: Integer;
  PC: PksComplex;

begin
  if not Spectrum then RealFFT(Data, DataSize, 0);
  DataSize:= DataSize shr 1;
  PC:= Data;
  PC^.Re:= Sqr(PC^.Re) / DataSize;
  PC^.Im:= Sqr(PC^.Im) / DataSize;
  I:= DataSize - 1;
  while I > 0 do begin
    Inc(PC);
    PC^.Re:= (Sqr(PC^.Re) + Sqr(PC^.Im)) / DataSize;
    PC^.Im:= 0;
    Dec(I);
  end;
  RealFFT(Data, DataSize shl 1, -1);
end;

function MaxPowerOfTwo(Value: Integer): Integer;
begin
  Result:= 1;
  repeat
    Result:= Result shl 1;
  until (Result > Value) or (Result < 0);
  Result:= Result shr 1;
end;

function RFTSpectrum(Data: Pointer; DataSize, Index: Integer): TksComplex;
var
  SecondHalf: Boolean;

begin
  if Index = 0 then Result:= PExtended(Data)^
  else if Index = DataSize shr 1 then Result:= PDArray(Data)^[1]
  else if Index < DataSize then begin
    SecondHalf:= Index > DataSize shr 1;
    if SecondHalf then
      Index:= DataSize - Index;
    Result:= PCArray(Data)^[Index];
    if not SecondHalf then
      Result.Im:= - Result.Im;
  end
  else Result:= 0;
end;

function RFTAmplitude(Data: Pointer; DataSize, Index: Integer): Extended;
var
  C: TksComplex;

begin
  C:= RFTSpectrum(Data, DataSize, Index);
  Result:= Sqrt(Sqr(C.Re) + Sqr(C.Im));
end;

function RFTPhase(Data: Pointer; DataSize, Index: Integer): Extended;
var
  C: TksComplex;

begin
  C:= RFTSpectrum(Data, DataSize, Index);
  if C.Re = 0 then begin
    if C.Im > 0 then Result:= Pi/2
    else if C.Im < 0 then Result:= -Pi/2
    else Result:= 0;
  end
  else Result:= Math.ArcTan2(C.Im, C.Re);
end;

{=========== Windows for RealFFT ================}

{ Bartlett (triangle) window }
function RFWBartlett(Data: Pointer; DataSize: Integer): Extended;
var
  P: PExtended;
  Half: Extended;
  Factor: Extended;
  Index: Integer;

begin
  Index:= 0;
  P:= Data;
  Half:= DataSize shr 1;
  Result:= 0;
  while Index < Half do begin
    Factor:= Index / Half;
    P^:= P^ * Factor;
    Result:= Result + Sqr(Factor);
    Inc(P);
    Inc(Index);
  end;
  while Index > 0 do begin
    Factor:= Index / Half;
    P^:= P^ * Factor;
    Result:= Result + Sqr(Factor);
    Inc(P);
    Dec(Index);
  end;
  Result:= Result * DataSize;
end;

{ Hann (cosine) window }
function RFWHann(Data: Pointer; DataSize: Integer): Pointer;
var
  P: PExtended;
  Index: Integer;

begin
  Index:= 0;
  P:= Data;
  while Index < DataSize do begin
    P^:= P^ * (0.50 - 0.50 * Cos(2 * Pi * Index / (DataSize - 1)));
    Inc(P);
    Inc(Index);
  end;
  Result:= Data;
end;

{ Hamming (raised cosine) window }
function RFWHamming(Data: Pointer; DataSize: Integer): Pointer;
var
  P: PExtended;
  Index: Integer;

begin
  Index:= 0;
  P:= Data;
  while Index < DataSize do begin
    P^:= P^ * (0.54 - 0.46 * Cos(2 * Pi * Index / (DataSize - 1)));
    Inc(P);
    Inc(Index);
  end;
  Result:= Data;
end;

end.
