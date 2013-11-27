//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixAbgrPre(P: PCardinal;
  Cr, Cg, Cb, Alpha: Cardinal); overload;
begin
  Alpha := CAggBaseMask - Alpha;

  TAggOrderAbgr(P^).R := Int8u(((TAggOrderAbgr(P^).R * Alpha) shr CAggBaseShift) + Cr);
  TAggOrderAbgr(P^).G := Int8u(((TAggOrderAbgr(P^).G * Alpha) shr CAggBaseShift) + Cg);
  TAggOrderAbgr(P^).B := Int8u(((TAggOrderAbgr(P^).B * Alpha) shr CAggBaseShift) + Cb);
  TAggOrderAbgr(P^).A := Int8u(CAggBaseMask - ((Alpha * (CAggBaseMask -
    TAggOrderAbgr(P^).A)) shr CAggBaseShift));
end;

procedure BlendPixAbgrPre(P: PCardinal;
  Cr, Cg, Cb, Alpha, Cover: Cardinal); overload;
begin
  Alpha := CAggBaseMask - Alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderAbgr(P^).R := Int8u((TAggOrderAbgr(P^).R * Alpha + Cr * Cover)
    shr CAggBaseShift);
  TAggOrderAbgr(P^).G := Int8u((TAggOrderAbgr(P^).G * Alpha + Cg * Cover)
    shr CAggBaseShift);
  TAggOrderAbgr(P^).B := Int8u((TAggOrderAbgr(P^).B * Alpha + Cb * Cover)
    shr CAggBaseShift);
  TAggOrderAbgr(P^).A := Int8u(CAggBaseMask - ((Alpha * (CAggBaseMask -
    TAggOrderAbgr(P^).A)) shr CAggBaseShift));
end;

procedure CopyOrBlendPixAbgrPre(P: PCardinal;
  Cr, Cg, Cb, Alpha: Cardinal); overload;
begin
  if Alpha <> 0 then
    if Alpha = CAggBaseMask then
    begin
      TAggOrderAbgr(P^).R := Cr;
      TAggOrderAbgr(P^).G := Cg;
      TAggOrderAbgr(P^).B := Cb;
      TAggOrderAbgr(P^).A := CAggBaseMask;
    end
    else
      BlendPixAbgrPre(P, Cr, Cg, Cb, Alpha);
end;

procedure CopyOrBlendPixAbgrPre(P: PCardinal;
  Cr, Cg, Cb, Alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixAbgrPre(P, Cr, Cg, Cb, Alpha)
  else if Alpha <> 0 then
  begin
    Alpha := (Alpha * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      TAggOrderAbgr(P^).R := Cr;
      TAggOrderAbgr(P^).G := Cg;
      TAggOrderAbgr(P^).B := Cb;
      TAggOrderAbgr(P^).A := CAggBaseMask;
    end
    else
      BlendPixAbgrPre(P, Cr, Cg, Cb, Alpha, Cover);
  end;
end;

procedure Abgr32PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixAbgrPre(PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) +
    (X shl 2)), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
end;

procedure Abgr32PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PCardinal;
  V: Cardinal;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      TAggOrderAbgr(V).R := C.Rgba8.R;
      TAggOrderAbgr(V).G := C.Rgba8.G;
      TAggOrderAbgr(V).B := C.Rgba8.B;
      TAggOrderAbgr(V).A := C.Rgba8.A;

      repeat
        P^ := V;

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixAbgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32PreBlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PCardinal;
  V: Cardinal;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      TAggOrderAbgr(V).R := C.Rgba8.R;
      TAggOrderAbgr(V).G := C.Rgba8.G;
      TAggOrderAbgr(V).B := C.Rgba8.B;
      TAggOrderAbgr(V).A := C.Rgba8.A;

      repeat
        P^ := V;
        P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixAbgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PCardinal;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        TAggOrderAbgr(P^).R := C.Rgba8.R;
        TAggOrderAbgr(P^).G := C.Rgba8.G;
        TAggOrderAbgr(P^).B := C.Rgba8.B;
        TAggOrderAbgr(P^).A := CAggBaseMask;
      end
      else
        BlendPixAbgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PCardinal;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        TAggOrderAbgr(P^).R := C.Rgba8.R;
        TAggOrderAbgr(P^).G := C.Rgba8.G;
        TAggOrderAbgr(P^).B := C.Rgba8.B;
        TAggOrderAbgr(P^).A := CAggBaseMask;
      end
      else
        BlendPixAbgrPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgrPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(P), 4);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendColorVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgrPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PCardinal;

  Incp: Integer;
begin
  Psrc := PCardinal(SourcePtr);
  Pdst := PCardinal(PtrComp(This.RenderingBuffer.Row(Ydst)) + (Xdst shl 2) *
    SizeOf(Int8u));
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(Psrc), ((Len - 1) shl 2) * SizeOf(Int8u));
    Inc(PtrComp(Pdst), ((Len - 1) shl 2) * SizeOf(Int8u));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(Pdst, TAggOrderAbgr(Psrc^).R, TAggOrderAbgr(Psrc^).G,
        TAggOrderAbgr(Psrc^).B, TAggOrderAbgr(Psrc^).A);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(Pdst, TAggOrderAbgr(Psrc^).R, TAggOrderAbgr(Psrc^).G,
        TAggOrderAbgr(Psrc^).B, TAggOrderAbgr(Psrc^).A, Cover);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
  Color: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc: PInt8u;
  Pdst: PCardinal;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    repeat
      CopyOrBlendPixAbgrPre(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc: PInt8u;
  Pdst: PCardinal;

  Color: PAggColor;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgrPre(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgrPre(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A, Cover);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 4);
        Dec(Len);
      until Len = 0;
  end;
end;
