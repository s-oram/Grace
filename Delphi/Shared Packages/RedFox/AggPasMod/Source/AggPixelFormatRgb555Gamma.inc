// AggPas 2.4 RM3 pixel format definition file

function MakePixel555Gamma(R, G, B: Cardinal): Int16u;
begin
  Result := Int16u(((R and $F8) shl 7) or ((G and $F8) shl 2) or (B shr 3)
    or $8000);
end;

procedure MakeColor555Gamma(var Color: TAggColor; P: PInt16u);
begin
  Color.FromRgbaInteger((P^ shr 7) and $F8, (P^ shr 2) and $F8, (P^ shl 3)
    and $F8)
end;

procedure BlendPixel555Gamma(Gamma: TAggGamma; P: PInt16u;
  Cr, Cg, Cb, Alpha, Cover: Cardinal);
var
  Rgb: Int16u;
  R, G, B: Integer;
begin
  Rgb := P^;

  R := Gamma.Dir[(Rgb shr 7) and $F8];
  G := Gamma.Dir[(Rgb shr 2) and $F8];
  B := Gamma.Dir[(Rgb shl 3) and $F8];

  P^ := Int16u(((Gamma.Inv[((Gamma.Dir[Cr] - R) * Alpha + (R shl 8)) shr 8]
    shl 7) and $7C00) or ((Gamma.Inv[((Gamma.Dir[Cg] - G) * Alpha + (G shl 8))
    shr 8] shl 2) and $03E0) or (Gamma.Inv[((Gamma.Dir[Cb] - B) * Alpha +
    (B shl 8)) shr 8] shr 3) or $8000);
end;

procedure CopyOrBlendPixel555Gamma(Gamma: TAggGamma; P: PInt16u;
  C: PAggColor; Cover: Cardinal);
var
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      P^ := MakePixel555Gamma(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B)
    else
      BlendPixel555Gamma(Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
  end;
end;

procedure Rgb555GammaCopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u))^ :=
    MakePixel555Gamma(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);
end;

procedure Rgb555GammaBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixel555Gamma(This.Gamma,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u)), C, Cover);
end;

function Rgb555GammaPixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
begin
  MakeColor555Gamma(Result, PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    SizeOf(Int16u)));
end;

procedure Rgb555GammaCopyHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PInt16u;
  V: Int16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  V := MakePixel555Gamma(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

  repeat
    P^ := V;

    Inc(PtrComp(P), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaCopyVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PInt16u;
  V: Int16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  V := MakePixel555Gamma(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

  repeat
    P^ := V;
    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaBlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
      V := MakePixel555Gamma(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

      repeat
        P^ := V;

        Inc(PtrComp(P), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPixel555Gamma(This.Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
          Alpha, Cover);

        Inc(PtrComp(P), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb555GammaBlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
      V := MakePixel555Gamma(C.Rgba8.R, C.Rgba8.G, C.Rgba8.B);

      repeat
        P^ := V;
        P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPixel555Gamma(This.Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
          Alpha, Cover);

        P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb555GammaBlendSolidHSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPixel555Gamma(This.Gamma, P, C, Covers^);

    Inc(PtrComp(Covers));
    Inc(PtrComp(P), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaBlendSolidVSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPixel555Gamma(This.Gamma, P, C, Covers^);

    Inc(PtrComp(Covers));

    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaBlendColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPixel555Gamma(This.Gamma, P, Colors, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPixel555Gamma(This.Gamma, P, Colors, Cover);

    Inc(PtrComp(P), SizeOf(Int16u));
    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaBlendColorVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPixel555Gamma(This.Gamma, P, Colors, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPixel555Gamma(This.Gamma, P, Colors, Cover);

    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaCopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt16u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int16u))^,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16))^,
    Len * SizeOf(Int16u));
end;

procedure Rgb555GammaBlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; SourcePtr: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Pdst: PInt16u;

  Alpha: Cardinal;
begin
  Pdst := PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16u));

  repeat
    Alpha := PInt8u(PtrComp(SourcePtr) + From.Order.A)^;

    if Alpha <> 0 then
      if (Alpha = CAggBaseMask) and (Cover = 255) then
        Pdst^ := MakePixel555Gamma(PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.G)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.B)^)
      else
        BlendPixel555Gamma(This.Gamma, Pdst,
          PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.G)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.B)^, Alpha, Cover);
    Inc(PtrComp(SourcePtr), 4);
    Inc(PtrComp(Pdst), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaCopyColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    P^ := MakePixel555Gamma(Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B);

    Inc(PtrComp(P), SizeOf(Int16u));
    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaCopyColorVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor);
var
  P: PInt16u;
begin
  P := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    P^ := MakePixel555Gamma(Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B);
    P := PInt16u(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb555GammaBlendFromColor(This: TAggPixelFormatProcessor;
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
      BlendPixel555Gamma(This.Gamma, Pdst, Color.Rgba8.R, Color.Rgba8.G,
        Color.Rgba8.B, Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb555GammaBlendFromLUT(This: TAggPixelFormatProcessor;
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

      BlendPixel555Gamma(This.Gamma, Pdst, Color.Rgba8.R, Color.Rgba8.G,
        Color.Rgba8.B, Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end;
