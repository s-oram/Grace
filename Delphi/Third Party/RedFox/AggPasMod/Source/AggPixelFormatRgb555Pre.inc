//
// AggPas 2.4 RM3 pixel format definition file
//

function MakePix555Pre(R, G, B: Cardinal): Int16u;
begin
  Result := Int16u(((R and $F8) shl 7) or ((G and $F8) shl 2) or (B shr 3)
    or $8000);
end;

procedure MakeColor555Pre(var Color: TAggColor; P: PInt16u);
begin
  Color.FromRgbaInteger((P^ shr 7) and $F8, (P^ shr 2) and $F8, (P^ shl 3)
    and $F8)
end;

procedure BlendPix555Pre(P: PInt16u; Cr, Cg, Cb, Alpha, Cover: Cardinal);
var
  Rgb: Int16u;
  R, G, B: Integer;
begin
  Alpha := CAggBaseMask - Alpha;
  Rgb := P^;

  R := (Rgb shr 7) and $F8;
  G := (Rgb shr 2) and $F8;
  B := (Rgb shl 3) and $F8;

  P^ := Int16u((((R * Alpha + Cr * Cover) shr 1) and $7C00) or
    (((G * Alpha + Cg * Cover) shr 6) and $03E0) or
    ((B * Alpha + Cb * Cover) shr 11) or $8000);
end;

procedure CopyOrBlendPix555Pre(P: PInt16u; C: PAggColor; Cover: Cardinal);
var
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      P^ := MakePix555Pre(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B)
    else
      BlendPix555Pre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
  end;
end;

procedure Rgb555PreCopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u))^ :=
    MakePix555Pre(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);
end;

procedure Rgb555PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPix555Pre(PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    SizeOf(Int16u)), C, Cover);
end;

function Rgb555PrePixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
begin
  MakeColor555Pre(Result, PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    SizeOf(Int16u)));
end;

procedure Rgb555PreCopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PInt16u;
  V: Int16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  V := MakePix555Pre(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

  repeat
    P^ := V;

    Inc(PtrComp(P), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreCopyVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PInt16u;
  V: Int16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  V := MakePix555Pre(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

  repeat
    P^ := V;
    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt16u;
  V: Int16u;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      V := MakePix555Pre(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

      repeat
        P^ := V;

        Inc(PtrComp(P), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPix555Pre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        Inc(PtrComp(P), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb555PreBlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt16u;
  V: Int16u;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      V := MakePix555Pre(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

      repeat
        P^ := V;
        P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPix555Pre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb555PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPix555Pre(P, C, Covers^);

    Inc(PtrComp(Covers));
    Inc(PtrComp(P), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPix555Pre(P, C, Covers^);

    Inc(PtrComp(Covers));

    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreBlendColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPix555Pre(P, Colors, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPix555Pre(P, Colors, Cover);

    Inc(PtrComp(P), SizeOf(Int16u));
    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreBlendColorVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPix555Pre(P, Colors, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPix555Pre(P, Colors, Cover);

    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreCopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt16u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int16u))^,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16))^,
    Len * SizeOf(Int16u));
end;

procedure Rgb555PreBlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; SourcePtr: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Pdst: PInt16u;

  Alpha: Cardinal;
begin
  Pdst := PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) +
    Xdst * SizeOf(Int16u));

  repeat
    Alpha := PInt8u(PtrComp(SourcePtr) + From.Order.A)^;

    if Alpha <> 0 then
      if (Alpha = CAggBaseMask) and (Cover = 255) then
        Pdst^ := MakePix555Pre(PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.G)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.B)^)
      else
        BlendPix555Pre(Pdst, PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.G)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.B)^, Alpha, Cover);

    Inc(PtrComp(SourcePtr), 4);
    Inc(PtrComp(Pdst), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreCopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    P^ := MakePix555Pre(Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B);

    Inc(PtrComp(P), SizeOf(Int16u));
    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreCopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt16u;

begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    P^ := MakePix555Pre(Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B);
    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555PreBlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Color: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc: PInt8u;
  Pdst: PInt16u;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt16u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * SizeOf(Int16u));

    repeat
      BlendPix555Pre(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb555PreBlendFromLUT(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc: PInt8u;
  Pdst: PInt16u;

  Color: PAggColor;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt16u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * SizeOf(Int16u));

    repeat
      Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

      BlendPix555Pre(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end;
