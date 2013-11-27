//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgrPre(P: PInt8u;
  Cr, Cg, Cb, Alpha, Cover: Cardinal); overload;
begin
  Alpha := CAggBaseMask - Alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderBGR(Pointer(P)^).R :=
    Int8u((TAggOrderBGR(Pointer(P)^).R * Alpha + Cr * Cover) shr CAggBaseShift);
  TAggOrderBGR(Pointer(P)^).G :=
    Int8u((TAggOrderBGR(Pointer(P)^).G * Alpha + Cg * Cover) shr CAggBaseShift);
  TAggOrderBGR(Pointer(P)^).B :=
    Int8u((TAggOrderBGR(Pointer(P)^).B * Alpha + Cb * Cover) shr CAggBaseShift);

end;

procedure BlendPixBgrPre(P: PInt8u; Cr, Cg, Cb, Alpha: Cardinal); overload;
begin
  Alpha := CAggBaseMask - Alpha;

  TAggOrderBGR(Pointer(P)^).R :=
    Int8u(((TAggOrderBGR(Pointer(P)^).R * Alpha) shr CAggBaseShift) + Cr);
  TAggOrderBGR(Pointer(P)^).G :=
    Int8u(((TAggOrderBGR(Pointer(P)^).G * Alpha) shr CAggBaseShift) + Cg);
  TAggOrderBGR(Pointer(P)^).B :=
    Int8u(((TAggOrderBGR(Pointer(P)^).B * Alpha) shr CAggBaseShift) + Cb);

end;

procedure CopyOrBlendPixBgrPre(P: PInt8u; C: PAggColor;
  Cover: Cardinal); overload;
var
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      TAggOrderBGR(Pointer(P)^).R := C.Rgba8.R;
      TAggOrderBGR(Pointer(P)^).G := C.Rgba8.G;
      TAggOrderBGR(Pointer(P)^).B := C.Rgba8.B;
    end
    else
      BlendPixBgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgrPre(P: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      TAggOrderBGR(Pointer(P)^).R := C.Rgba8.R;
      TAggOrderBGR(Pointer(P)^).G := C.Rgba8.G;
      TAggOrderBGR(Pointer(P)^).B := C.Rgba8.B;
    end
    else
      BlendPixBgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A);
end;

procedure Bgr24PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixBgrPre(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X),
    C, Cover);
end;

procedure Bgr24PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        TAggOrderBGR(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderBGR(Pointer(P)^).B := C.Rgba8.B;

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24PreBlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        TAggOrderBGR(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderBGR(Pointer(P)^).B := C.Rgba8.B;

        P := PInt8u(This.RenderingBuffer.NextRow(P));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PInt8u(This.RenderingBuffer.NextRow(P));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        TAggOrderBGR(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderBGR(Pointer(P)^).B := C.Rgba8.B;
      end
      else
        BlendPixBgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(PtrComp(P), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        TAggOrderBGR(Pointer(P)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(P)^).G := C.Rgba8.G;
        TAggOrderBGR(Pointer(P)^).B := C.Rgba8.B;
      end
      else
        BlendPixBgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrPre(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrPre(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrPre(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrPre(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Covers);

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrPre(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrPre(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PInt8u(This.RenderingBuffer.NextRow(P));

      Dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PInt8u;

  Color: TAggColor;
  Alpha: Cardinal;
begin
  Psrc := SourcePtr;
  Pdst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + 3 * Xdst *
    SizeOf(Int8u));

  if Cover = 255 then
    repeat
      Alpha := PInt8u(PtrComp(Psrc) + From.Order.A)^;

      if Alpha <> 0 then
        if Alpha = CAggBaseMask then
        begin
          TAggOrderBGR(Pointer(Pdst)^).R :=
            PInt8u(PtrComp(Psrc) + From.Order.R)^;
          TAggOrderBGR(Pointer(Pdst)^).G :=
            PInt8u(PtrComp(Psrc) + From.Order.G)^;
          TAggOrderBGR(Pointer(Pdst)^).B :=
            PInt8u(PtrComp(Psrc) + From.Order.B)^;
        end
        else
          BlendPixBgrPre(Pdst, PInt8u(PtrComp(Psrc) + From.Order.R)^,
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

      CopyOrBlendPixBgrPre(Pdst, @Color, Cover);

      Inc(PtrComp(Psrc), 4);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      3 * Xdst * SizeOf(Int8u));

    repeat
      CopyOrBlendPixBgrPre(Pdst, Color,
        ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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
      3 * Xdst * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        BlendPixBgrPre(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgrPre(Pdst,
          PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 3);
        Dec(Len);
      until Len = 0;
  end;
end;
