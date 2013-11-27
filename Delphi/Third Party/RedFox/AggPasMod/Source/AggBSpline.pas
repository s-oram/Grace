unit AggBSpline;

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

type
  // ----------------------------------------------------------------
  // A very simple class of Bi-cubic Spline interpolation.
  // First call init(num, x[], y[]) where num - number of source points,
  // x, y - arrays of X and Y values respectively. Here Y must be a function
  // of X. It means that all the X-coordinates must be arranged in the ascending
  // order.
  // Then call get(x) that calculates a value Y for the respective X.
  // The class supports extrapolation, i.e. you can call get(x) where x is
  // outside the given with init() X-range. Extrapolation is a simple linear
  // function.

  TAggBSpline = class
  private
    FMax, FNum: Integer;
    FX, FY, FSplineBuffer: PDouble;
    FLastIdx: Integer;
  protected
    procedure BSearch(N: Integer; X: PDouble; X0: Double; I: PInteger);

    function ExtrapolationLeft(X: Double): Double;
    function ExtrapolationRight(X: Double): Double;

    function Interpolation(X: Double; I: Integer): Double;
  public
    constructor Create; overload;
    constructor Create(Num: Integer); overload;
    constructor Create(Num: Integer; X, Y: PDouble); overload;
    destructor Destroy; override;

    procedure Init(Max: Integer); overload;
    procedure Init(Num: Integer; X, Y: PDouble); overload;
    procedure Init(Num: Integer; Points: PPointDouble); overload;

    procedure AddPoint(X, Y: Double);
    procedure Prepare;

    function Get(X: Double): Double;
    function GetStateful(X: Double): Double;
  end;

implementation


{ TAggBSpline }

constructor TAggBSpline.Create;
begin
  FMax := 0;
  FNum := 0;

  FX := nil;
  FY := nil;
  FSplineBuffer := nil;

  FLastIdx := -1;
end;

constructor TAggBSpline.Create(Num: Integer);
begin
  Create;

  Init(Num);
end;

constructor TAggBSpline.Create(Num: Integer; X, Y: PDouble);
begin
  Create;

  Init(Num, X, Y);
end;

destructor TAggBSpline.Destroy;
begin
  AggFreeMem(Pointer(FSplineBuffer), FMax * 3 * SizeOf(Double));

  inherited;
end;

procedure TAggBSpline.Init(Max: Integer);
begin
  if (Max > 2) and (Max > FMax) then
  begin
    AggFreeMem(Pointer(FSplineBuffer), FMax * 3 * SizeOf(Double));
    AggGetMem(Pointer(FSplineBuffer), Max * 3 * SizeOf(Double));

    FMax := Max;

    FX := PDouble(PtrComp(FSplineBuffer) + FMax * SizeOf(Double));
    FY := PDouble(PtrComp(FSplineBuffer) + FMax * 2 * SizeOf(Double));
  end;

  FNum := 0;
  FLastIdx := -1;
end;

procedure TAggBSpline.Init(Num: Integer; X, Y: PDouble);
var
  I: Integer;
begin
  if Num > 2 then
  begin
    Init(Num);

    for I := 0 to Num - 1 do
    begin
      AddPoint(X^, Y^);

      Inc(X);
      Inc(Y);
    end;

    Prepare;
  end;

  FLastIdx := -1;
end;

procedure TAggBSpline.Init(Num: Integer; Points: PPointDouble);
var
  I: Integer;
begin
  if Num > 2 then
  begin
    Init(Num);

    for I := 0 to Num - 1 do
    begin
      AddPoint(Points^.X, Points^.Y);

      Inc(Points);
    end;

    Prepare;
  end;

  FLastIdx := -1;
end;

procedure TAggBSpline.AddPoint(X, Y: Double);
begin
  if FNum < FMax then
  begin
    PDouble(PtrComp(FX) + FNum * SizeOf(Double))^ := X;
    PDouble(PtrComp(FY) + FNum * SizeOf(Double))^ := Y;

    Inc(FNum);
  end;
end;

procedure TAggBSpline.Prepare;
var
  I, K, N1, Sz  : Integer;
  Temp, R, S, Al: PDouble;
  H, P, D, F, E : Double;
begin
  if FNum > 2 then
  begin
    for K := 0 to FNum - 1 do
      PDouble(PtrComp(FSplineBuffer) + K * SizeOf(Double))^ := 0;

    N1 := 3 * FNum;
    Sz := N1;

    AggGetMem(Pointer(Al), N1 * SizeOf(Double));

    Temp := Al;

    for K := 0 to N1 - 1 do
      PDouble(PtrComp(Temp) + K * SizeOf(Double))^ := 0;

    R := PDouble(PtrComp(Temp) + FNum * SizeOf(Double));
    S := PDouble(PtrComp(Temp) + FNum * 2 * SizeOf(Double));

    N1 := FNum - 1;
    D := PDouble(PtrComp(FX) + SizeOf(Double))^ - FX^;
    E := (PDouble(PtrComp(FY) + SizeOf(Double))^ - FY^) / D;

    K := 1;

    while K < N1 do
    begin
      H := D;
      D := PDouble(PtrComp(FX) + (K + 1) * SizeOf(Double))^ -
        PDouble(PtrComp(FX) + K * SizeOf(Double))^;
      F := E;
      E := (PDouble(PtrComp(FY) + (K + 1) * SizeOf(Double))^ -
        PDouble(PtrComp(FY) + K * SizeOf(Double))^) / D;

      PDouble(PtrComp(Al) + K * SizeOf(Double))^ := D / (D + H);
      PDouble(PtrComp(R) + K * SizeOf(Double))^ :=
        1.0 - PDouble(PtrComp(Al) + K * SizeOf(Double))^;
      PDouble(PtrComp(S) + K * SizeOf(Double))^ := 6.0 * (E - F) / (H + D);

      Inc(K);
    end;

    K := 1;

    while K < N1 do
    begin
      P := 1.0 / (PDouble(PtrComp(R) + K * SizeOf(Double))^ *
        PDouble(PtrComp(Al) + (K - 1) * SizeOf(Double))^ + 2.0);

      PDouble(PtrComp(Al) + K * SizeOf(Double))^ :=
        PDouble(PtrComp(Al) + K * SizeOf(Double))^ * -P;

      PDouble(PtrComp(S) + K * SizeOf(Double))^ :=
        (PDouble(PtrComp(S) + K * SizeOf(Double))^ -
        PDouble(PtrComp(R) + K * SizeOf(Double))^ *
        PDouble(PtrComp(S) + (K - 1) * SizeOf(Double))^) * P;

      Inc(K);
    end;

    PDouble(PtrComp(FSplineBuffer) + N1 * SizeOf(Double))^ := 0.0;

    PDouble(PtrComp(Al) + (N1 - 1) * SizeOf(Double))^ :=
      PDouble(PtrComp(S) + (N1 - 1) * SizeOf(Double))^;

    PDouble(PtrComp(FSplineBuffer) + (N1 - 1) * SizeOf(Double))^ :=
      PDouble(PtrComp(Al) + (N1 - 1) * SizeOf(Double))^;

    K := N1 - 2;
    I := 0;

    while I < FNum - 2 do
    begin
      PDouble(PtrComp(Al) + K * SizeOf(Double))^ :=
        PDouble(PtrComp(Al) + K * SizeOf(Double))^ *
        PDouble(PtrComp(Al) + (K + 1) * SizeOf(Double))^ +
        PDouble(PtrComp(S) + K * SizeOf(Double))^;

      PDouble(PtrComp(FSplineBuffer) + K * SizeOf(Double))^ :=
        PDouble(PtrComp(Al) + K * SizeOf(Double))^;

      Inc(I);
      Dec(K);
    end;

    AggFreeMem(Pointer(Al), Sz * SizeOf(Double));
  end;

  FLastIdx := -1;
end;

function TAggBSpline.Get(X: Double): Double;
var
  I: Integer;
begin
  if FNum > 2 then
  begin
    // Extrapolation on the left
    if X < FX^ then
    begin
      Result := ExtrapolationLeft(X);

      Exit;
    end;

    // Extrapolation on the right
    if X >= PDouble(PtrComp(FX) + (FNum - 1) * SizeOf(Double))^ then
    begin
      Result := ExtrapolationRight(X);

      Exit;
    end;

    // Interpolation
    BSearch(FNum, FX, X, @I);

    Result := Interpolation(X, I);

    Exit;
  end;

  Result := 0.0;
end;

function TAggBSpline.GetStateful(X: Double): Double;
begin
  if FNum > 2 then
  begin
    // Extrapolation on the left
    if X < FX^ then
    begin
      Result := ExtrapolationLeft(X);

      Exit;
    end;

    // Extrapolation on the right
    if X >= PDouble(PtrComp(FX) + (FNum - 1) * SizeOf(Double))^ then
    begin
      Result := ExtrapolationRight(X);

      Exit;
    end;

    if FLastIdx >= 0 then
    begin
      // Check if x is not in current range
      if (X < PDouble(PtrComp(FX) + FLastIdx * SizeOf(Double))^) or
        (X > PDouble(PtrComp(FX) + (FLastIdx + 1) * SizeOf(Double))^) then
        // Check if x between next points (most probably)
        if (FLastIdx < FNum - 2) and
          (X >= PDouble(PtrComp(FX) + (FLastIdx + 1) * SizeOf(Double))^)
          and (X <= PDouble(PtrComp(FX) + (FLastIdx + 2) *
          SizeOf(Double))^) then
          Inc(FLastIdx)
        else if (FLastIdx > 0) and
          (X >= PDouble(PtrComp(FX) + (FLastIdx - 1) * SizeOf(Double))^)
          and (X <= PDouble(PtrComp(FX) + FLastIdx * SizeOf(Double))^)
        then
          // x is between pevious points
          Dec(FLastIdx)
        else
          // Else perform full search
          BSearch(FNum, FX, X, @FLastIdx);

      Result := Interpolation(X, FLastIdx);

      Exit;
    end
    else
    begin
      // Interpolation
      BSearch(FNum, FX, X, @FLastIdx);

      Result := Interpolation(X, FLastIdx);

      Exit;
    end;
  end;

  Result := 0.0;
end;

procedure TAggBSpline.BSearch(N: Integer; X: PDouble; X0: Double; I: PInteger);
var
  J, K: Integer;
begin
  J := N - 1;
  I^ := 0;

  while J - I^ > 1 do
  begin
    K := ShrInt32(I^ + J, 1);

    if X0 < PDouble(PtrComp(X) + K * SizeOf(Double))^ then
      J := K
    else
      I^ := K;
  end;
end;

function TAggBSpline.ExtrapolationLeft(X: Double): Double;
var
  D: Double;
begin
  D := PDouble(PtrComp(FX) + SizeOf(Double))^ - FX^;

  Result := (-D * PDouble(PtrComp(FSplineBuffer) + SizeOf(Double))^ / 6 +
    (PDouble(PtrComp(FY) + SizeOf(Double))^ - FY^) / D) *
    (X - FX^) + FY^;
end;

function TAggBSpline.ExtrapolationRight(X: Double): Double;
var
  D: Double;
begin
  D := PDouble(PtrComp(FX) + (FNum - 1) * SizeOf(Double))^ -
    PDouble(PtrComp(FX) + (FNum - 2) * SizeOf(Double))^;

  Result := (D * PDouble(PtrComp(FSplineBuffer) + (FNum - 2) * SizeOf(Double))^ / 6 +
    (PDouble(PtrComp(FY) + (FNum - 1) * SizeOf(Double))^ -
    PDouble(PtrComp(FY) + (FNum - 2) * SizeOf(Double))^) / D) *
    (X - PDouble(PtrComp(FX) + (FNum - 1) * SizeOf(Double))^) +
    PDouble(PtrComp(FY) + (FNum - 1) * SizeOf(Double))^;
end;

function TAggBSpline.Interpolation(X: Double; I: Integer): Double;
var
  J: Integer;
  D, H, R, P: Double;
begin
  J := I + 1;
  D := PDouble(PtrComp(FX) + I * SizeOf(Double))^ -
    PDouble(PtrComp(FX) + J * SizeOf(Double))^;
  H := X - PDouble(PtrComp(FX) + J * SizeOf(Double))^;
  R := PDouble(PtrComp(FX) + I * SizeOf(Double))^ - X;
  P := D * D / 6.0;

  Result := (PDouble(PtrComp(FSplineBuffer) + J * SizeOf(Double))^ * R * R * R +
    PDouble(PtrComp(FSplineBuffer) + I * SizeOf(Double))^ * H * H * H) / 6.0 / D +
    ((PDouble(PtrComp(FY) + J * SizeOf(Double))^ - PDouble(PtrComp(FSplineBuffer)
    + J * SizeOf(Double))^ * P) * R +
    (PDouble(PtrComp(FY) + I * SizeOf(Double))^ - PDouble(PtrComp(FSplineBuffer) +
    I * SizeOf(Double))^ * P) * H) / D;
end;

end.
