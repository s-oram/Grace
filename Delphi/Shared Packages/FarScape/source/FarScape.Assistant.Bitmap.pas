unit FarScape.Assistant.Bitmap;

interface

uses
  WinApi.Windows,
  Graphics;

type
  BitmapAssistant = record
  public
    class procedure SetAlpha(const Bitmap : TBitmap; const Alpha : cardinal); static;
    class procedure PreMultiplyAlpha(const Bitmap : TBitmap); static;

    class procedure ClearAll(const Bitmap : TBitmap; R, G, B, A: Cardinal); overload; static;
    class procedure ClearAll(const Bitmap : TBitmap; const aColor : TColor; const A: Cardinal); overload; static;
  end;

//TODO: add a bitblit function. Clean these two functions up. Ensure it is as neat
// as possible.
function WinAlphaBlend(hdcDest: HDC; xoriginDest, yoriginDest, wDest, hDest: integer;
  hdcSrc: HDC; xoriginSrc, yoriginSrc, wSrc, hSrc: integer; ftn: BLENDFUNCTION): LongBool;
  stdcall; external 'Msimg32.dll' name 'AlphaBlend';

implementation

type
  P32BitPixel = ^T32BitPixel;
  T32BitPixel = packed record
    B : byte;
    G : byte;
    R : byte;
    A : byte;
  end;


{ BitmapAssistant }

class procedure BitmapAssistant.ClearAll(const Bitmap: TBitmap; R, G, B, A: Cardinal);
var
  DstPixel : P32BitPixel;
  c1: Integer;
begin
  assert(R <= 255);
  assert(G <= 255);
  assert(B <= 255);
  assert(A <= 255);
  assert(Bitmap.PixelFormat = pf32Bit);

  if Bitmap.AlphaFormat = afPreMultiplied then
  begin
    // TODO:LOW Is calling MulDiv in an external DLL faster than a native Delphi function.
    R := MulDiv(R, A, 255);
    G := MulDiv(G, A, 255);
    B := MulDiv(B, A, 255);
  end;

  DstPixel := Bitmap.ScanLine[Bitmap.Height-1];
  for c1 := 0 to Bitmap.Height * Bitmap.Width -1 do
  begin
    DstPixel^.R := R;
    DstPixel^.G := G;
    DstPixel^.B := B;
    DstPixel^.A := A;
    inc(DstPixel);
  end;
end;

class procedure BitmapAssistant.ClearAll(const Bitmap: TBitmap; const aColor: TColor; const A: Cardinal);
var
  c : Longint;
  R, G, B : cardinal;
begin
  c := ColorToRgb(aColor);
  R := GetRValue(c);
  G := GetGValue(c);
  B := GetBValue(c);
  BitmapAssistant.ClearAll(Bitmap, R, G, B, A);
end;

class procedure BitmapAssistant.PreMultiplyAlpha(const Bitmap: TBitmap);
var
  DstPixel : P32BitPixel;
  c1: Integer;
begin
  assert(Bitmap.PixelFormat = pf32Bit);

  DstPixel := Bitmap.ScanLine[Bitmap.Height-1];
  for c1 := 0 to Bitmap.Height * Bitmap.Width -1 do
  begin
    // TODO:LOW: Check TBitmap.PreMultiplyAlpha();
    // for an alternative implementation using pointers that might be quicker.
    DstPixel^.R := MulDiv(DstPixel^.R, DstPixel^.A, 255);
    DstPixel^.G := MulDiv(DstPixel^.G, DstPixel^.A, 255);
    DstPixel^.B := MulDiv(DstPixel^.B, DstPixel^.A, 255);
    inc(DstPixel);
  end;
end;

class procedure BitmapAssistant.SetAlpha(const Bitmap: TBitmap; const Alpha: cardinal);
var
  DstPixel : P32BitPixel;
  c1: Integer;
begin
  assert(Bitmap.PixelFormat = pf32Bit);
  assert(Alpha <= 255);

  DstPixel := Bitmap.ScanLine[Bitmap.Height-1];
  for c1 := 0 to Bitmap.Height * Bitmap.Width -1 do
  begin
    DstPixel^.A := Alpha;
    inc(DstPixel);
  end;
end;


end.
