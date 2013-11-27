//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgb(P: PInt8u; Cr, Cg, Cb, Alpha: Integer;
  Cover: Cardinal = 0);
begin
  TAggOrderRGB(Pointer(P)^).R := TAggOrderRGB(Pointer(P)^).R +
    ((Cr - TAggOrderRGB(Pointer(P)^).R) * Alpha) shr CAggBaseShift;
  TAggOrderRGB(Pointer(P)^).G := TAggOrderRGB(Pointer(P)^).G +
    ((Cg - TAggOrderRGB(Pointer(P)^).G) * Alpha) shr CAggBaseShift;
  TAggOrderRGB(Pointer(P)^).B := TAggOrderRGB(Pointer(P)^).B +
    ((Cb - TAggOrderRGB(Pointer(P)^).B) * Alpha) shr CAggBaseShift;
end;

procedure CopyOrBlendPixRgb(P: PInt8u; C: PAggColor;
  Cover: Cardinal); overload;
var
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
      TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
      TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;
    end
    else
      BlendPixRgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
  end;
end;

procedure CopyOrBlendPixRgb(P: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
      TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
      TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;
    end
    else
      BlendPixRgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A);
end;

procedure Rgb24CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  P: PAggOrderRGB;
begin
  P := PAggOrderRGB(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  P^.R := C.Rgba8.R;
  P^.G := C.Rgba8.G;
  P^.B := C.Rgba8.B;
end;

procedure Rgb24BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgb(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X),
    C, Cover);
end;

function Rgb24Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PAggOrderRGB;
begin
  P := PAggOrderRGB(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  Result.Rgba8.R := P^.R;
  Result.Rgba8.G := P^.G;
  Result.Rgba8.B := P^.B;
  Result.Rgba8.A := $FF;
end;

procedure Rgb24CopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
    TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
    TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;

    Inc(PtrComp(P), 3);
    Dec(Len);
  until Len = 0;
end;

procedure Rgb24CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
    TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
    TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;

    P := This.RenderingBuffer.NextRow(P);

    Dec(Len);
  until Len = 0;
end;

procedure Rgb24BlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      repeat
        TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      repeat
        TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;

        P := PInt8u(This.RenderingBuffer.NextRow(P));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PInt8u(This.RenderingBuffer.NextRow(P));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;

      end
      else
        BlendPixRgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(PtrComp(P), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        TAggOrderRGB(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderRGB(Pointer(P)^).B := C.Rgba8.B;
      end
      else
        BlendPixRgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgb(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgb(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgb(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Rgb24BlendColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgb(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgb(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgb(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Dec(Len);
    until Len = 0;
end;

procedure Rgb24BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PInt8u;

  Color: TAggColor;
  Alpha: Cardinal;
begin
  Psrc := SourcePtr;
  Pdst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * 3 *
    SizeOf(Int8u));

  if Cover = 255 then
    repeat
      Alpha := PInt8u(PtrComp(Psrc) + From.Order.A)^;

      if Alpha <> 0 then
        if Alpha = CAggBaseMask then
        begin
          TAggOrderRGB(Pointer(Pdst)^).R :=
            PInt8u(PtrComp(Psrc) + From.Order.R)^;
          TAggOrderRGB(Pointer(Pdst)^).G :=
            PInt8u(PtrComp(Psrc) + From.Order.G)^;
          TAggOrderRGB(Pointer(Pdst)^).B :=
            PInt8u(PtrComp(Psrc) + From.Order.B)^;

        end
        else
          BlendPixRgb(Pdst, PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^, Alpha);

      Inc(PtrComp(Psrc), 4);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);

    until Len = 0
  else
    repeat
      Color.FromRgbaInteger(PInt8u(PtrComp(Psrc) + From.Order.R)^,
        PInt8u(PtrComp(Psrc) + From.Order.G)^,
        PInt8u(PtrComp(Psrc) + From.Order.B)^,
        PInt8u(PtrComp(Psrc) + From.Order.A)^);

      CopyOrBlendPixRgb(Pdst, @Color, Cover);

      Inc(PtrComp(Psrc), 4);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Rgb24CopyColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt8u;

begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(P)^).R := Colors.Rgba8.R;
    TAggOrderRGB(Pointer(P)^).G := Colors.Rgba8.G;
    TAggOrderRGB(Pointer(P)^).B := Colors.Rgba8.B;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(PtrComp(P), 3);
    Dec(Len);
  until Len = 0;
end;

procedure Rgb24CopyColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(P)^).R := Colors.Rgba8.R;
    TAggOrderRGB(Pointer(P)^).G := Colors.Rgba8.G;
    TAggOrderRGB(Pointer(P)^).B := Colors.Rgba8.B;

    P := PInt8u(This.RenderingBuffer.NextRow(P));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb24BlendFromColor(This, From: TAggPixelFormatProcessor;
  Color: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  Psrc, Pdst: PInt8u;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * 3 * SizeOf(Int8u));

    repeat
      CopyOrBlendPixRgb(Pdst, Color, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  Psrc, Pdst: PInt8u;
  Color: PAggColor;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * 3 * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        BlendPixRgb(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixRgb(Pdst,
          PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 3);
        Dec(Len);
      until Len = 0;
  end;
end;
