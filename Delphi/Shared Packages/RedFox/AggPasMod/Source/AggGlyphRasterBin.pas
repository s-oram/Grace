unit AggGlyphRasterBin;

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
  PAggGlyphRect = ^TAggGlyphRect;
  TAggGlyphRect = record
    X1, Y1, X2, Y2: Integer;

    Dx, Dy: Double;
  end;

  PAggGlyphRasterBin = ^TAggGlyphRasterBin;
  TAggGlyphRasterBin = class
  private
    FFont: PInt8u;

    FBigEndian: Boolean;

    FSpan: array [0..31] of Int8u;
    FBits: PInt8u;

    FGlyphWidth, FGlyphByteWidth: Cardinal;
  public
    constructor Create(Font: PInt8u);

    function GetFont: PInt8u;
    procedure SetFont(F: PInt8u);

    function Height: Double;
    function BaseLine: Double;

    function Width(Str: PAnsiChar): Double;
    procedure Prepare(R: PAggGlyphRect; X, Y: Double; Glyph: Cardinal;
      Flip: Boolean);
    function Span(I: Cardinal): PInt8u;

    function Value(P: PInt8u): Int16u;
  end;

implementation


{ TAggGlyphRasterBin }

constructor TAggGlyphRasterBin.Create;
var
  T: Integer;

begin
  FFont := Font;

  T := 1;

  if Byte(Pointer(@T)^) = 0 then
    FBigEndian := True
  else
    FBigEndian := False;

  FillChar(FSpan, SizeOf(FSpan), 0);
end;

function TAggGlyphRasterBin.GetFont;
begin
  Result := FFont;
end;

procedure TAggGlyphRasterBin.SetFont;
begin
  FFont := F;
end;

function TAggGlyphRasterBin.Height;
begin
  Result := PInt8u(FFont)^;
end;

function TAggGlyphRasterBin.BaseLine;
begin
  Result := PInt8u(PtrComp(FFont) + SizeOf(Int8u))^;
end;

function TAggGlyphRasterBin.Width;
var
  StartChar, NumChars, W, Glyph: Cardinal;
  Bits: PInt8u;
begin
  StartChar := PInt8u(PtrComp(FFont) + 2 * SizeOf(Int8u))^;
  NumChars := PInt8u(PtrComp(FFont) + 3 * SizeOf(Int8u))^;

  W := 0;

  while Str <> #0 do
  begin
    Glyph := PInt8u(Str)^;

    Bits := PInt8u(PtrComp(FFont) + 4 + NumChars * 2 +
      Value(PInt8u(PtrComp(FFont) + 4 + (Glyph - StartChar) * 2)));

    Inc(W, Bits^);
    Inc(PtrComp(Str));
  end;

  Result := W;
end;

procedure TAggGlyphRasterBin.Prepare;
var
  StartChar, NumChars: Cardinal;
begin
  StartChar := PInt8u(PtrComp(FFont) + 2 * SizeOf(Int8u))^;
  NumChars := PInt8u(PtrComp(FFont) + 3 * SizeOf(Int8u))^;

  FBits := PInt8u(PtrComp(FFont) + 4 + NumChars * 2 +
    Value(PInt8u(PtrComp(FFont) + 4 + (Glyph - StartChar) * 2)));

  FGlyphWidth := FBits^;

  Inc(PtrComp(FBits));

  FGlyphByteWidth := (FGlyphWidth + 7) shr 3;

  R.X1 := Trunc(X);
  R.X2 := R.X1 + FGlyphWidth - 1;

  if Flip then
  begin
    R.Y1 := Trunc(Y) - PInt8u(FFont)^ +
      PInt8u(PtrComp(FFont) + 1 * SizeOf(Int8u))^;

    R.Y2 := R.Y1 + PInt8u(FFont)^ - 1;

  end
  else
  begin
    R.Y1 := Trunc(Y) - PInt8u(PtrComp(FFont) + SizeOf(Int8u))^ + 1;
    R.Y2 := R.Y1 + PInt8u(FFont)^ - 1;
  end;

  R.Dx := FGlyphWidth;
  R.Dy := 0;
end;

function TAggGlyphRasterBin.Span(I: Cardinal): PInt8u;
var
  Bits: PInt8u;
  J, Val, Nb: Cardinal;
begin
  I := PInt8u(FFont)^ - I - 1;

  Bits := PInt8u(PtrComp(FBits) + I * FGlyphByteWidth);
  Val := Bits^;
  Nb := 0;

  for J := 0 to FGlyphWidth - 1 do
  begin
    if Val and $80 <> 0 then
      FSpan[J] := Int8u(CAggCoverFull)
    else
      FSpan[J] := Int8u(CAggCoverNone);

    Val := Val shl 1;

    Inc(Nb);

    if Nb >= 8 then
    begin
      Inc(PtrComp(Bits));

      Val := Bits^;
      Nb := 0;
    end;
  end;

  Result := @FSpan[0];
end;

function TAggGlyphRasterBin.Value(P: PInt8u): Int16u;
var
  V: Int16u;
begin
  if FBigEndian then
  begin
    TInt16uAccess(V).Low := PInt8u(PtrComp(P) + 1)^;
    TInt16uAccess(V).High := P^;
  end
  else
  begin
    TInt16uAccess(V).Low := P^;
    TInt16uAccess(V).High := PInt8u(PtrComp(P) + 1)^;
  end;

  Result := V;
end;

end.
