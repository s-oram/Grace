unit AggBlur;

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
//                                                                            //
//  The Stack Blur Algorithm was invented by Mario Klingemann,                //
//  mario@quasimondo.com and described here:                                  //
//  http://incubator.quasimondo.com/processing/fast_blur_deluxe.php           //
//  (search phrase "StackBlur: Fast But Goodlooking").                        //
//  The major improvement is that there's no more division table              //
//  that was very expensive to create for large blur radii. Insted,           //
//  for 8-bit per channel and radius not exceeding 254 the division is        //
//  replaced by multiplication and shift.                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$Q-}
{$R-}

uses
  AggBasics,
  AggArray,
  AggColor,
  AggPixelFormat,
  AggPixelFormatTransposer;

type
  TAggStackBlur = class
  private
    FBuffer, FStack: TAggPodVector;
    procedure BlurX(Img: TAggPixelFormatProcessor; Radius: Cardinal);
    procedure BlurY(Img: TAggPixelFormatProcessor; Radius: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Blur(Img: TAggPixelFormatProcessor; Radius: Cardinal);
  end;

  TAggRecursiveBlur = class
  private
    FSum1, FSum2, FBuffer: TAggPodVector;
    procedure BlurX(Img: TAggPixelFormatProcessor; Radius: Double);
    procedure BlurY(Img: TAggPixelFormatProcessor; Radius: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Blur(Img: TAggPixelFormatProcessor; Radius: Double);
  end;

procedure StackBlurGray8(Img: TAggPixelFormatProcessor; Rx, Ry: Cardinal);
procedure StackBlurRgb24(Img: TAggPixelFormatProcessor; Rx, Ry: Cardinal);
procedure StackBlurRgba32(Img: TAggPixelFormatProcessor; Rx, Ry: Cardinal);

implementation

const
  GStackBlur8Mul: array [0..254] of Int16u = (512, 512, 456, 512, 328, 456,
    335, 512, 405, 328, 271, 456, 388, 335, 292, 512, 454, 405, 364, 328, 298,
    271, 496, 456, 420, 388, 360, 335, 312, 292, 273, 512, 482, 454, 428, 405,
    383, 364, 345, 328, 312, 298, 284, 271, 259, 496, 475, 456, 437, 420, 404,
    388, 374, 360, 347, 335, 323, 312, 302, 292, 282, 273, 265, 512, 497, 482,
    468, 454, 441, 428, 417, 405, 394, 383, 373, 364, 354, 345, 337, 328, 320,
    312, 305, 298, 291, 284, 278, 271, 265, 259, 507, 496, 485, 475, 465, 456,
    446, 437, 428, 420, 412, 404, 396, 388, 381, 374, 367, 360, 354, 347, 341,
    335, 329, 323, 318, 312, 307, 302, 297, 292, 287, 282, 278, 273, 269, 265,
    261, 512, 505, 497, 489, 482, 475, 468, 461, 454, 447, 441, 435, 428, 422,
    417, 411, 405, 399, 394, 389, 383, 378, 373, 368, 364, 359, 354, 350, 345,
    341, 337, 332, 328, 324, 320, 316, 312, 309, 305, 301, 298, 294, 291, 287,
    284, 281, 278, 274, 271, 268, 265, 262, 259, 257, 507, 501, 496, 491, 485,
    480, 475, 470, 465, 460, 456, 451, 446, 442, 437, 433, 428, 424, 420, 416,
    412, 408, 404, 400, 396, 392, 388, 385, 381, 377, 374, 370, 367, 363, 360,
    357, 354, 350, 347, 344, 341, 338, 335, 332, 329, 326, 323, 320, 318, 315,
    312, 310, 307, 304, 302, 299, 297, 294, 292, 289, 287, 285, 282, 280, 278,
    275, 273, 271, 269, 267, 265, 263, 261, 259);

  GStackBlur8Shr: array [0..254] of Int8u = (9, 11, 12, 13, 13, 14, 14, 15,
    15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18,
    18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
    21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24);

type
  PStackCalculator = ^TStackCalculator;
  TStackCalculator = record
    V, R, G, B, A: Cardinal;
  public
    procedure Clear;

    procedure Add(C: TAggColor); overload;
    procedure Add(C: TAggColor; K: Cardinal); overload;
    procedure Add(const C: TStackCalculator); overload;
    procedure Sub(C: TAggColor); overload;
    procedure Sub(const C: TStackCalculator); overload;

    procedure CalculatePixel(C: PAggColor; ADiv: Cardinal); overload;
    procedure CalculatePixel(C: PAggColor; AMul, AShr: Cardinal); overload;
  end;

  PGaussCalculator = ^TGaussCalculator;
  TGaussCalculator = record
    V, R, G, B, A: Double;
  public
    procedure FromPixel(C: TAggColor);

    procedure Calculate(B1, B2, B3, B4: Double;
      const C1, C2, C3, C4: TGaussCalculator);

    procedure ToPixel(C: PAggColor);
  end;


{ TStackCalculator }

procedure TStackCalculator.Clear;
begin
  V := 0;
  R := 0;
  G := 0;
  B := 0;
  A := 0;
end;

procedure TStackCalculator.Add(C: TAggColor);
begin
  Inc(V, C.V);
  Inc(R, C.Rgba8.R);
  Inc(G, C.Rgba8.G);
  Inc(B, C.Rgba8.B);
  Inc(A, C.Rgba8.A);
end;

procedure TStackCalculator.Add(const C: TStackCalculator);
begin
  Inc(V, C.V);
  Inc(R, C.R);
  Inc(G, C.G);
  Inc(B, C.B);
  Inc(A, C.A);
end;

procedure TStackCalculator.Add(C: TAggColor; K: Cardinal);
begin
  Inc(V, C.V * K);
  Inc(R, C.Rgba8.R * K);
  Inc(G, C.Rgba8.G * K);
  Inc(B, C.Rgba8.B * K);
  Inc(A, C.Rgba8.A * K);
end;

procedure TStackCalculator.Sub(C: TAggColor);
begin
  Dec(V, C.V);
  Dec(R, C.Rgba8.R);
  Dec(G, C.Rgba8.G);
  Dec(B, C.Rgba8.B);
  Dec(A, C.Rgba8.A);
end;

procedure TStackCalculator.Sub(const C: TStackCalculator);
begin
  Dec(V, C.V);
  Dec(R, C.R);
  Dec(G, C.G);
  Dec(B, C.B);
  Dec(A, C.A);
end;

procedure TStackCalculator.CalculatePixel(C: PAggColor; ADiv: Cardinal);
begin
  C.V := Int8u(V div ADiv);
  C.Rgba8.R := Int8u(R div ADiv);
  C.Rgba8.G := Int8u(G div ADiv);
  C.Rgba8.B := Int8u(B div ADiv);
  C.Rgba8.A := Int8u(A div ADiv);
end;

procedure TStackCalculator.CalculatePixel(C: PAggColor; AMul, AShr: Cardinal);
begin
  C.V := Int8u((V * AMul) shr AShr);
  C.Rgba8.R := Int8u((R * AMul) shr AShr);
  C.Rgba8.G := Int8u((G * AMul) shr AShr);
  C.Rgba8.B := Int8u((B * AMul) shr AShr);
  C.Rgba8.A := Int8u((A * AMul) shr AShr);
end;


{ TGaussCalculator }

procedure TGaussCalculator.FromPixel(C: TAggColor);
begin
  V := C.V;
  R := C.Rgba8.R;
  G := C.Rgba8.G;
  B := C.Rgba8.B;
  A := C.Rgba8.A;
end;

procedure TGaussCalculator.Calculate(B1, B2, B3, B4: Double;
  const C1, C2, C3, C4: TGaussCalculator);
begin
  V := B1 * C1.V + B2 * C2.V + B3 * C3.V + B4 * C4.V;
  R := B1 * C1.R + B2 * C2.R + B3 * C3.R + B4 * C4.R;
  G := B1 * C1.G + B2 * C2.G + B3 * C3.G + B4 * C4.G;
  B := B1 * C1.B + B2 * C2.B + B3 * C3.B + B4 * C4.B;
  A := B1 * C1.A + B2 * C2.A + B3 * C3.A + B4 * C4.A;
end;

procedure TGaussCalculator.ToPixel(C: PAggColor);
begin
  C.V := Int8u(UnsignedRound(V));
  C.Rgba8.R := Int8u(UnsignedRound(R));
  C.Rgba8.G := Int8u(UnsignedRound(G));
  C.Rgba8.B := Int8u(UnsignedRound(B));
  C.Rgba8.A := Int8u(UnsignedRound(A));
end;


{ TAggStackBlur }

constructor TAggStackBlur.Create;
begin
  FBuffer := TAggPodVector.Create(SizeOf(TAggColor));
  FStack := TAggPodVector.Create(SizeOf(TAggColor));
end;

destructor TAggStackBlur.Destroy;
begin
  FBuffer.Free;
  FStack.Free;
  inherited;
end;

procedure TAggStackBlur.BlurX(Img: TAggPixelFormatProcessor; Radius: Cardinal);
var
  X, Y, Xp, I, StackPointer, StackStart, W, H, Wm: Cardinal;
  ADiv, DivSum, MulSum, ShrSum, MaxVal: Cardinal;
  Pix: TAggColor;
  StackPix, TempColor: PAggColor;
  Sum, SumIn, SumOut: TStackCalculator;
begin
  if Radius < 1 then
    Exit;

  W := Img.Width;
  H := Img.Height;
  Wm := W - 1;
  ADiv := Radius * 2 + 1;

  DivSum := (Radius + 1) * (Radius + 1);
  MulSum := 0;
  ShrSum := 0;
  MaxVal := CAggBaseMask;

  if (MaxVal <= 255) and (Radius < 255) then
  begin
    MulSum := GStackBlur8Mul[Radius];
    ShrSum := GStackBlur8Shr[Radius];
  end;

  FBuffer.Allocate(W, 128);
  FStack.Allocate(ADiv, 32);

  Y := 0;

  while Y < H do
  begin
    Sum.Clear;
    SumIn.Clear;
    SumOut.Clear;

    Pix := Img.Pixel(Img, 0, Y);

    I := 0;

    while I <= Radius do
    begin
      Move(Pix, FStack[I]^, SizeOf(TAggColor));

      Sum.Add(Pix, I + 1);
      SumOut.Add(Pix);

      Inc(I);
    end;

    I := 1;

    while I <= Radius do
    begin
      if I > Wm then
        Pix := Img.Pixel(Img, Wm, Y)
      else
        Pix := Img.Pixel(Img, I, Y);

      Move(Pix, FStack[I + Radius]^, SizeOf(TAggColor));

      Sum.Add(Pix, Radius + 1 - I);
      SumIn.Add(Pix);

      Inc(I);
    end;

    StackPointer := Radius;

    X := 0;

    while X < W do
    begin
      if MulSum <> 0 then
        Sum.CalculatePixel(PAggColor(FBuffer[X]), MulSum, ShrSum)
      else
        Sum.CalculatePixel(PAggColor(FBuffer[X]), DivSum);

      Sum.Sub(SumOut);

      StackStart := StackPointer + ADiv - Radius;

      if StackStart >= ADiv then
        Dec(StackStart, ADiv);

      StackPix := FStack[StackStart];

      SumOut.Sub(StackPix^);

      Xp := X + Radius + 1;

      if Xp > Wm then
        Xp := Wm;

      Pix := Img.Pixel(Img, Xp, Y);

      StackPix^ := Pix;

      SumIn.Add(Pix);
      Sum.Add(SumIn);

      Inc(StackPointer);

      if StackPointer >= ADiv then
        StackPointer := 0;

      StackPix := FStack[StackPointer];

      SumOut.Add(StackPix^);
      SumIn.Sub(StackPix^);

      Inc(X);
    end;

    TempColor := FBuffer[0];

    Img.CopyColorHSpan(Img, 0, Y, W, TempColor);

    Inc(Y);
  end;
end;

procedure TAggStackBlur.BlurY(Img: TAggPixelFormatProcessor; Radius: Cardinal);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  PixelFormatTransposer(Img2, Img);
  try
    BlurX(Img2, Radius);
  finally
    Img2.Free;
  end;
end;

procedure TAggStackBlur.Blur(Img: TAggPixelFormatProcessor; Radius: Cardinal);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  BlurX(Img, Radius);
  PixelFormatTransposer(Img2, Img);
  try
    BlurX(Img2, Radius);
  finally
    Img2.Free;
  end;
end;


{ TAggRecursiveBlur }

constructor TAggRecursiveBlur.Create;
begin
  FSum1 := TAggPodVector.Create(SizeOf(TGaussCalculator));
  FSum2 := TAggPodVector.Create(SizeOf(TGaussCalculator));
  FBuffer := TAggPodVector.Create(SizeOf(TAggColor));
end;

destructor TAggRecursiveBlur.Destroy;
begin
  FSum1.Free;
  FSum2.Free;
  FBuffer.Free;
  inherited;
end;

procedure TAggRecursiveBlur.BlurX(Img: TAggPixelFormatProcessor; Radius: Double);
var
  S, Q, Q2, Q3, B0, B1, B2, B3, B: Double;
  W, H, Wm, X, Y: Integer;
  C: TGaussCalculator;
  G0, G1: PGaussCalculator;
begin
  if Radius < 0.62 then
    Exit;

  if Img.Width < 3 then
    Exit;

  S := Radius * 0.5;

  if S < 2.5 then
    Q := 3.97156 - 4.14554 * Sqrt(1 - 0.26891 * S)
  else
    Q := 0.98711 * S - 0.96330;

  Q2 := Q * Q;
  Q3 := Q2 * Q;
  B0 := 1.0 / (1.578250 + 2.444130 * Q + 1.428100 * Q2 + 0.422205 * Q3);
  B1 := 2.44413 * Q + 2.85619 * Q2 + 1.26661 * Q3;
  B2 := -1.42810 * Q2 + -1.26661 * Q3;
  B3 := 0.422205 * Q3;
  B := 1 - (B1 + B2 + B3) * B0;
  B1 := B1 * B0;
  B2 := B2 * B0;
  B3 := B3 * B0;
  W := Img.Width;
  H := Img.Height;
  Wm := W - 1;

  FSum1.Allocate(W);
  FSum2.Allocate(W);
  FBuffer.Allocate(W);

  Y := 0;

  while Y < H do
  begin
    G0 := PGaussCalculator(FSum1[0]);

    C.FromPixel(Img.Pixel(Img, 0, Y));
    G0.Calculate(B, B1, B2, B3, C, C, C, C);

    G1 := PGaussCalculator(FSum1[1]);

    C.FromPixel(Img.Pixel(Img, 1, Y));
    G1.Calculate(B, B1, B2, B3, C, G0^, G0^, G0^);

    C.FromPixel(Img.Pixel(Img, 2, Y));
    PGaussCalculator(FSum1[2]).Calculate(B, B1, B2, B3, C,
      G1^, G0^, G0^);

    X := 3;

    while X < W do
    begin
      C.FromPixel(Img.Pixel(Img, X, Y));

      PGaussCalculator(FSum1[X]).Calculate(B, B1, B2, B3, C,
        PGaussCalculator(FSum1[X - 1])^,
        PGaussCalculator(FSum1[X - 2])^,
        PGaussCalculator(FSum1[X - 3])^);

      Inc(X);
    end;

    G0 := PGaussCalculator(FSum1[Wm]);
    G1 := PGaussCalculator(FSum2[Wm]);

    G1.Calculate(B, B1, B2, B3, G0^, G0^, G0^, G0^);

    PGaussCalculator(FSum2[Wm - 1])
      .Calculate(B, B1, B2, B3, PGaussCalculator(FSum1[Wm - 1])^,
      G1^, G1^, G1^);

    PGaussCalculator(FSum2[Wm - 2])
      .Calculate(B, B1, B2, B3, PGaussCalculator(FSum1[Wm - 2])^,
      PGaussCalculator(FSum2[Wm - 1])^, G1^, G1^);

    G1.ToPixel(PAggColor(FBuffer[Wm]));

    PGaussCalculator(FSum2[Wm - 1])
      .ToPixel(PAggColor(FBuffer[Wm - 1]));

    PGaussCalculator(FSum2[Wm - 2])
      .ToPixel(PAggColor(FBuffer[Wm - 2]));

    X := Wm - 3;

    while X >= 0 do
    begin
      PGaussCalculator(FSum2[X])
        .Calculate(B, B1, B2, B3, PGaussCalculator(FSum1[X])^,
        PGaussCalculator(FSum2[X + 1])^,
        PGaussCalculator(FSum2[X + 2])^,
        PGaussCalculator(FSum2[X + 3])^);

      PGaussCalculator(FSum2[X])
        .ToPixel(PAggColor(FBuffer[X]));

      Dec(X);
    end;

    Img.CopyColorHSpan(Img, 0, Y, W, FBuffer[0]);

    Inc(Y);
  end;
end;

procedure TAggRecursiveBlur.BlurY(Img: TAggPixelFormatProcessor; Radius: Double);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  PixelFormatTransposer(Img2, Img);
  try
    BlurX(Img2, Radius);
  finally
    Img2.Free;
  end;
end;

procedure TAggRecursiveBlur.Blur(Img: TAggPixelFormatProcessor; Radius: Double);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  BlurX(Img, Radius);
  PixelFormatTransposer(Img2, Img);
  try
    BlurX(Img2, Radius);
  finally
    Img2.Free;
  end;
end;

procedure StackBlurGray8(Img: TAggPixelFormatProcessor; Rx, Ry: Cardinal);
var
  Stride: Integer;
  X, Y, Xp, Yp, I, Pix, StackPixel, Sum, SumIn, SumOut: Cardinal;
  StackPointer, StackStart, W, H, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;
  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  Stack: TAggPodVector;
begin
  W := Img.Width;
  H := Img.Height;
  Wm := W - 1;
  Hm := H - 1;

  Stack := TAggPodVector.Create(SizeOf(Int8u));

  if Rx > 0 then
  begin
    if Rx > 254 then
      Rx := 254;

    ADiv := Rx * 2 + 1;

    MulSum := GStackBlur8Mul[Rx];
    ShrSum := GStackBlur8Shr[Rx];

    Stack.Allocate(ADiv);

    Y := 0;

    while Y < H do
    begin
      Sum := 0;
      SumIn := 0;
      SumOut := 0;

      SourcePixelPointer := Img.GetPixelPointer(0, Y);
      Pix := SourcePixelPointer^;

      I := 0;

      while I <= Rx do
      begin
        PInt8u(Stack[I])^ := Pix;

        Inc(Sum, Pix * (I + 1));
        Inc(SumOut, Pix);

        Inc(I);
      end;

      I := 1;

      while I <= Rx do
      begin
        if I <= Wm then
          Inc(PtrComp(SourcePixelPointer), Img.Step);

        Pix := SourcePixelPointer^;

        PInt8u(Stack[I + Rx])^ := Pix;

        Inc(Sum, Pix * (Rx + 1 - I));
        Inc(SumIn, Pix);

        Inc(I);
      end;

      StackPointer := Rx;
      Xp := Rx;

      if Xp > Wm then
        Xp := Wm;

      SourcePixelPointer := Img.GetPixelPointer(Xp, Y);
      DestinationPixelPointer := Img.GetPixelPointer(0, Y);

      X := 0;

      while X < W do
      begin
        DestinationPixelPointer^ := Int8u((Sum * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), Img.Step);
        Dec(Sum, SumOut);

        StackStart := StackPointer + ADiv - Rx;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        Dec(SumOut, PInt8u(Stack[StackStart])^);

        if Xp < Wm then
        begin
          Inc(PtrComp(SourcePixelPointer), Img.Step);

          Pix := SourcePixelPointer^;

          Inc(Xp);
        end;

        PInt8u(Stack[StackStart])^ := Pix;

        Inc(SumIn, Pix);
        Inc(Sum, SumIn);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixel := PInt8u(Stack[StackPointer])^;

        Inc(SumOut, StackPixel);
        Dec(SumIn, StackPixel);

        Inc(X);
      end;

      Inc(Y);
    end;
  end;

  if Ry > 0 then
  begin
    if Ry > 254 then
      Ry := 254;

    ADiv := Ry * 2 + 1;

    MulSum := GStackBlur8Mul[Ry];
    ShrSum := GStackBlur8Shr[Ry];

    Stack.Allocate(ADiv);

    Stride := Img.Stride;

    X := 0;

    while X < W do
    begin
      Sum := 0;
      SumIn := 0;
      SumOut := 0;

      SourcePixelPointer := Img.GetPixelPointer(X, 0);
      Pix := SourcePixelPointer^;

      I := 0;

      while I <= Ry do
      begin
        PInt8u(Stack[I])^ := Pix;

        Inc(Sum, Pix * (I + 1));
        Inc(SumOut, Pix);

        Inc(I);
      end;

      I := 1;

      while I <= Ry do
      begin
        if I <= Hm then
          Inc(PtrComp(SourcePixelPointer), Stride);

        Pix := SourcePixelPointer^;

        PInt8u(Stack[I + Ry])^ := Pix;

        Inc(Sum, Pix * (Ry + 1 - I));
        Inc(SumIn, Pix);

        Inc(I);
      end;

      StackPointer := Ry;
      Yp := Ry;

      if Yp > Hm then
        Yp := Hm;

      SourcePixelPointer := Img.GetPixelPointer(X, Yp);
      DestinationPixelPointer := Img.GetPixelPointer(X, 0);

      Y := 0;

      while Y < H do
      begin
        DestinationPixelPointer^ := Int8u((Sum * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), Stride);
        Dec(Sum, SumOut);

        StackStart := StackPointer + ADiv - Ry;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        Dec(SumOut, PInt8u(Stack[StackStart])^);

        if Yp < Hm then
        begin
          Inc(PtrComp(SourcePixelPointer), Stride);

          Pix := SourcePixelPointer^;

          Inc(Yp);
        end;

        PInt8u(Stack[StackStart])^ := Pix;

        Inc(SumIn, Pix);
        Inc(Sum, SumIn);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixel := PInt8u(Stack[StackPointer])^;

        Inc(SumOut, StackPixel);
        Dec(SumIn, StackPixel);

        Inc(Y);
      end;

      Inc(X);
    end;
  end;

  Stack.Free;
end;

procedure StackBlurRgb24(Img: TAggPixelFormatProcessor; Rx, Ry: Cardinal);
var
  R, G, B, Stride: Integer;
  X, Y, Xp, Yp, I, StackPointer, StackStart: Cardinal;
  SumRed, SumGreen, SumBlue: Cardinal;
  SumInRed, SumInGreen, SumInBlue: Cardinal;
  SumOutRed, SumOutGreen, SumOutBlue: Cardinal;
  W, H, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;
  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  StackPixelPointer: PAggColor;
  Stack: TAggPodArray;
begin
  R := Img.Order.R;
  G := Img.Order.G;
  B := Img.Order.B;

  W := Img.Width;
  H := Img.Height;
  Wm := W - 1;
  Hm := H - 1;

  Stack := TAggPodArray.Create(SizeOf(TAggColor));

  if Rx > 0 then
  begin
    if Rx > 254 then
      Rx := 254;

    ADiv := Rx * 2 + 1;
    MulSum := GStackBlur8Mul[Rx];
    ShrSum := GStackBlur8Shr[Rx];

    Stack.Allocate(ADiv);

    Y := 0;

    while Y < H do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;

      SourcePixelPointer := Img.GetPixelPointer(0, Y);

      I := 0;

      while I <= Rx do
      begin
        StackPixelPointer := Stack[I];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (I + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (I + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (I + 1));

        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);

        Inc(I);
      end;

      I := 1;

      while I <= Rx do
      begin
        if I <= Wm then
          Inc(PtrComp(SourcePixelPointer), Img.PixWidth);

        StackPixelPointer := Stack[I + Rx];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (Rx + 1 - I));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (Rx + 1 - I));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (Rx + 1 - I));

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);

        Inc(I);
      end;

      StackPointer := Rx;
      Xp := Rx;

      if Xp > Wm then
        Xp := Wm;

      SourcePixelPointer := Img.GetPixelPointer(Xp, Y);
      DestinationPixelPointer := Img.GetPixelPointer(0, Y);

      X := 0;

      while X < W do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + G)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + B)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), Img.PixWidth);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);

        StackStart := StackPointer + ADiv - Rx;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.G);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.B);

        if Xp < Wm then
        begin
          Inc(PtrComp(SourcePixelPointer), Img.PixWidth);
          Inc(Xp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);

        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.G);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.B);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.G);
        Dec(SumInBlue, StackPixelPointer.Rgba8.B);

        Inc(X);
      end;

      Inc(Y);
    end;
  end;

  if Ry > 0 then
  begin
    if Ry > 254 then
      Ry := 254;

    ADiv := Ry * 2 + 1;

    MulSum := GStackBlur8Mul[Ry];
    ShrSum := GStackBlur8Shr[Ry];

    Stack.Allocate(ADiv);

    Stride := Img.Stride;

    X := 0;

    while X < W do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;

      SourcePixelPointer := Img.GetPixelPointer(X, 0);

      I := 0;

      while I <= Ry do
      begin
        StackPixelPointer := Stack[I];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (I + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (I + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (I + 1));
        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);

        Inc(I);
      end;

      I := 1;

      while I <= Ry do
      begin
        if I <= Hm then
          Inc(PtrComp(SourcePixelPointer), Stride);

        StackPixelPointer := Stack[I + Ry];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (Ry + 1 - I));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (Ry + 1 - I));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (Ry + 1 - I));
        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);

        Inc(I);
      end;

      StackPointer := Ry;
      Yp := Ry;

      if Yp > Hm then
        Yp := Hm;

      SourcePixelPointer := Img.GetPixelPointer(X, Yp);
      DestinationPixelPointer := Img.GetPixelPointer(X, 0);

      Y := 0;

      while Y < H do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + G)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + B)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), Stride);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);

        StackStart := StackPointer + ADiv - Ry;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.G);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.B);

        if Yp < Hm then
        begin
          Inc(PtrComp(SourcePixelPointer), Stride);

          Inc(Yp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.G);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.B);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.G);
        Dec(SumInBlue, StackPixelPointer.Rgba8.B);

        Inc(Y);
      end;

      Inc(X);
    end;
  end;

  Stack.Free;
end;

procedure StackBlurRgba32(Img: TAggPixelFormatProcessor; Rx, Ry: Cardinal);
var
  R, G, B, A, Stride: Integer;
  X, Y, Xp, Yp, I, StackPointer, StackStart: Cardinal;
  SumRed, SumGreen, SumBlue, SumAlpha: Cardinal;
  SumInRed, SumInGreen, SumInBlue, SumInAlpha: Cardinal;
  SumOutRed, SumOutGreen, SumOutBlue, SumOutAlpha: Cardinal;
  W, H, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;

  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  StackPixelPointer: PAggColor;
  Stack: TAggPodArray;
begin
  R := Img.Order.R;
  G := Img.Order.G;
  B := Img.Order.B;
  A := Img.Order.A;

  W := Img.Width;
  H := Img.Height;
  Wm := W - 1;
  Hm := H - 1;

  Stack := TAggPodArray.Create(SizeOf(TAggColor));

  if Rx > 0 then
  begin
    if Rx > 254 then
      Rx := 254;

    ADiv := Rx * 2 + 1;
    MulSum := GStackBlur8Mul[Rx];
    ShrSum := GStackBlur8Shr[Rx];

    Stack.Allocate(ADiv);

    Y := 0;

    while Y < H do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumAlpha := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumInAlpha := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;
      SumOutAlpha := 0;

      SourcePixelPointer := Img.GetPixelPointer(0, Y);

      I := 0;

      while I <= Rx do
      begin
        StackPixelPointer := Stack[I];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (I + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (I + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (I + 1));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (I + 1));

        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumOutAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(I);
      end;

      I := 1;

      while I <= Rx do
      begin
        if I <= Wm then
          Inc(PtrComp(SourcePixelPointer), Img.PixWidth);

        StackPixelPointer := Stack[I + Rx];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (Rx + 1 - I));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (Rx + 1 - I));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (Rx + 1 - I));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (Rx + 1 - I));

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(I);
      end;

      StackPointer := Rx;
      Xp := Rx;

      if Xp > Wm then
        Xp := Wm;

      SourcePixelPointer := Img.GetPixelPointer(Xp, Y);
      DestinationPixelPointer := Img.GetPixelPointer(0, Y);

      X := 0;

      while X < W do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + G)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + B)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + A)^ :=
          Int8u((SumAlpha * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), Img.PixWidth);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);
        Dec(SumAlpha, SumOutAlpha);

        StackStart := StackPointer + ADiv - Rx;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.G);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.B);
        Dec(SumOutAlpha, StackPixelPointer.Rgba8.A);

        if Xp < Wm then
        begin
          Inc(PtrComp(SourcePixelPointer), Img.PixWidth);
          Inc(Xp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);
        Inc(SumAlpha, SumInAlpha);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.G);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.B);
        Inc(SumOutAlpha, StackPixelPointer.Rgba8.A);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.G);
        Dec(SumInBlue, StackPixelPointer.Rgba8.B);
        Dec(SumInAlpha, StackPixelPointer.Rgba8.A);

        Inc(X);
      end;

      Inc(Y);
    end;
  end;

  if Ry > 0 then
  begin
    if Ry > 254 then
      Ry := 254;

    ADiv := Ry * 2 + 1;

    MulSum := GStackBlur8Mul[Ry];
    ShrSum := GStackBlur8Shr[Ry];

    Stack.Allocate(ADiv);

    Stride := Img.Stride;

    X := 0;

    while X < W do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumAlpha := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumInAlpha := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;
      SumOutAlpha := 0;

      SourcePixelPointer := Img.GetPixelPointer(X, 0);

      I := 0;

      while I <= Ry do
      begin
        StackPixelPointer := Stack[I];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (I + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (I + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (I + 1));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (I + 1));
        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumOutAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(I);
      end;

      I := 1;

      while I <= Ry do
      begin
        if I <= Hm then
          Inc(PtrComp(SourcePixelPointer), Stride);

        StackPixelPointer := Stack[I + Ry];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (Ry + 1 - I));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^ * (Ry + 1 - I));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^ * (Ry + 1 - I));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (Ry + 1 - I));
        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(I);
      end;

      StackPointer := Ry;
      Yp := Ry;

      if Yp > Hm then
        Yp := Hm;

      SourcePixelPointer := Img.GetPixelPointer(X, Yp);
      DestinationPixelPointer := Img.GetPixelPointer(X, 0);

      Y := 0;

      while Y < H do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + G)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + B)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + A)^ :=
          Int8u((SumAlpha * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), Stride);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);
        Dec(SumAlpha, SumOutAlpha);

        StackStart := StackPointer + ADiv - Ry;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.G);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.B);
        Dec(SumOutAlpha, StackPixelPointer.Rgba8.A);

        if Yp < Hm then
        begin
          Inc(PtrComp(SourcePixelPointer), Stride);

          Inc(Yp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.G := PInt8u(PtrComp(SourcePixelPointer) + G)^;
        StackPixelPointer.Rgba8.B := PInt8u(PtrComp(SourcePixelPointer) + B)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + G)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + B)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);
        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);
        Inc(SumAlpha, SumInAlpha);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.G);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.B);
        Inc(SumOutAlpha, StackPixelPointer.Rgba8.A);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.G);
        Dec(SumInBlue, StackPixelPointer.Rgba8.B);
        Dec(SumInAlpha, StackPixelPointer.Rgba8.A);

        Inc(Y);
      end;

      Inc(X);
    end;
  end;

  Stack.Free;
end;

end.
