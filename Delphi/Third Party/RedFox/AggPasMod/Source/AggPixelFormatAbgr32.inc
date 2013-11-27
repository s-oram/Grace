//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixAbgr(P: PCardinal; Cr, Cg, Cb, Alpha: Integer;
  Cover: Cardinal = 0);
var
  R, G, B, A: Int8u;

begin
  R := TAggOrderAbgr(P^).R;
  G := TAggOrderAbgr(P^).G;
  B := TAggOrderAbgr(P^).B;
  A := TAggOrderAbgr(P^).A;

  TAggOrderAbgr(P^).R := ((Cr - R) * Alpha + (R shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderAbgr(P^).G := ((Cg - G) * Alpha + (G shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderAbgr(P^).B := ((Cb - B) * Alpha + (B shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderAbgr(P^).A := (Alpha + A) - ((Alpha * A + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixAbgr(P: PCardinal;
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
      BlendPixAbgr(P, Cr, Cg, Cb, Alpha);
end;

procedure CopyOrBlendPixAbgr(P: PCardinal;
  Cr, Cg, Cb, Alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixAbgr(P, Cr, Cg, Cb, Alpha)
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
      BlendPixAbgr(P, Cr, Cg, Cb, Alpha, Cover);
  end;
end;

procedure Abgr32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  TAggOrderAbgr(P^).R := C.Rgba8.R;
  TAggOrderAbgr(P^).G := C.Rgba8.G;
  TAggOrderAbgr(P^).B := C.Rgba8.B;
  TAggOrderAbgr(P^).A := C.Rgba8.A;
end;

procedure Abgr32BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixAbgr(PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2)
    ), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
end;

function Abgr32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PAggOrderAbgr;
begin
  P := PAggOrderAbgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  Result.FromRgbaInteger(P^.R, P^.G, P^.B, P^.A);
end;

procedure Abgr32CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
  V: Cardinal;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  TAggOrderAbgr(V).R := C.Rgba8.R;
  TAggOrderAbgr(V).G := C.Rgba8.G;
  TAggOrderAbgr(V).B := C.Rgba8.B;
  TAggOrderAbgr(V).A := C.Rgba8.A;

  if Len > 0 then
    repeat
      Cardinal(Pointer(P)^) := V;

      Inc(PtrComp(P), 4);
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PCardinal;
  V: Cardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  TAggOrderAbgr(V).R := C.Rgba8.R;
  TAggOrderAbgr(V).G := C.Rgba8.G;
  TAggOrderAbgr(V).B := C.Rgba8.B;
  TAggOrderAbgr(V).A := C.Rgba8.A;

  if Len > 0 then
    repeat
      P^ := V;
      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0;
end;

procedure Abgr32BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
        BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32BlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(P), 4);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32BlendFrom(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixAbgr(Pdst, TAggOrderAbgr(Psrc^).R, TAggOrderAbgr(Psrc^).G,
        TAggOrderAbgr(Psrc^).B, TAggOrderAbgr(Psrc^).A);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgr(Pdst, TAggOrderAbgr(Psrc^).R, TAggOrderAbgr(Psrc^).G,
        TAggOrderAbgr(Psrc^).B, TAggOrderAbgr(Psrc^).A, Cover);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  repeat
    TAggOrderAbgr(Pointer(P)^).R := Colors.Rgba8.R;
    TAggOrderAbgr(Pointer(P)^).G := Colors.Rgba8.G;
    TAggOrderAbgr(Pointer(P)^).B := Colors.Rgba8.B;
    TAggOrderAbgr(Pointer(P)^).A := Colors.Rgba8.A;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(PtrComp(P), 4);
    Dec(Len);
  until Len = 0;
end;

procedure Abgr32CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  repeat
    TAggOrderAbgr(Pointer(P)^).R := Colors.Rgba8.R;
    TAggOrderAbgr(Pointer(P)^).G := Colors.Rgba8.G;
    TAggOrderAbgr(Pointer(P)^).B := Colors.Rgba8.B;
    TAggOrderAbgr(Pointer(P)^).A := Colors.Rgba8.A;

    P := PCardinal(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Abgr32BlendFromColor(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixAbgr(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32BlendFromLUT(This, From: TAggPixelFormatProcessor;
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

        CopyOrBlendPixAbgr(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgr(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A, Cover);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 4);
        Dec(Len);
      until Len = 0;
  end;
end;
