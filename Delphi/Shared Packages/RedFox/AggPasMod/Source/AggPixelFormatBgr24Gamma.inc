// AggPas 2.4 RM3 pixel format definition file

procedure BlendPixBgrGamma(Gamma: TAggGamma; P: PAggOrderBgr;
  Cr, Cg, Cb, Alpha: Integer; Cover: Cardinal = 0);
var
  R, G, B: Cardinal;
begin
  R := Gamma.Dir[TAggOrderBGR(Pointer(P)^).R];
  G := Gamma.Dir[TAggOrderBGR(Pointer(P)^).G];
  B := Gamma.Dir[TAggOrderBGR(Pointer(P)^).B];

  P^.R := Int8u(Gamma.Inv[(((Integer(Gamma.Dir[Cr]) - R) * Alpha)
    shr CAggBaseShift) + R]);
  P^.G := Int8u(Gamma.Inv[(((Integer(Gamma.Dir[Cg]) - G) * Alpha)
    shr CAggBaseShift) + G]);
  P^.B := Int8u(Gamma.Inv[(((Integer(Gamma.Dir[Cb]) - B) * Alpha)
    shr CAggBaseShift) + B]);
end;

procedure CopyOrBlendPixBgrGamma(Gamma: TAggGamma; P: PAggOrderBgr;
  C: PAggColor; Cover: Cardinal); overload;
var
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      P^.R := C.Rgba8.R;
      P^.G := C.Rgba8.G;
      P^.B := C.Rgba8.B;
    end
    else
      BlendPixBgrGamma(Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgrGamma(Gamma: TAggGamma; P: PAggOrderBgr;
  C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      P^.R := C.Rgba8.R;
      P^.G := C.Rgba8.G;
      P^.B := C.Rgba8.B;
    end
    else
      BlendPixBgrGamma(Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A);
end;

procedure Bgr24GammaBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);
  CopyOrBlendPixBgrGamma(This.Gamma, P, C, Cover);
end;

procedure Bgr24GammaBlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgr;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      repeat
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrGamma(This.Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24GammaBlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgr;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      repeat
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;

        P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8U(P)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrGamma(This.Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha,
          Cover);

        P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8U(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24GammaBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PAggOrderBgr;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;
      end
      else
        BlendPixBgrGamma(This.Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
          Alpha, Covers^);

      Inc(PtrComp(P), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PAggOrderBgr;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;
      end
      else
        BlendPixBgrGamma(This.Gamma, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha,
          Covers^);

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Covers);

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PAggOrderBgr;

  Color: TAggColor;
  Alpha: Cardinal;
begin
  Psrc := PAggOrderBgr(SourcePtr);
  Pdst := PAggOrderBgr(This.RenderingBuffer.Row(Ydst));
  Inc(Pdst, Xdst);

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
          BlendPixBgrGamma(This.Gamma, Pdst,
            PInt8u(PtrComp(Psrc) + From.Order.R)^,
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

      CopyOrBlendPixBgrGamma(This.Gamma, Pdst, @Color, Cover);

      Inc(PtrComp(Psrc), 4);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendFromColor(This, From: TAggPixelFormatProcessor;
  Color: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  Psrc: PInt8u;
  Pdst: PAggOrderBgr;
begin
  PixWidth := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Pdst, Xdst);
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, Pdst, Color,
        ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), PixWidth);
      Inc(PtrComp(Pdst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  Psrc: PInt8u;
  Pdst: PAggOrderBgr;
  Color: PAggColor;
begin
  PixWidth := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Pdst, Xdst);

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        BlendPixBgrGamma(This.Gamma, Pdst, Color.Rgba8.R, Color.Rgba8.G,
          Color.Rgba8.B, Color.Rgba8.A);

        Inc(PtrComp(Psrc), PixWidth);
        Inc(PtrComp(Pdst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgrGamma(This.Gamma, Pdst,
          PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(Psrc), PixWidth);
        Inc(PtrComp(Pdst), 3);
        Dec(Len);
      until Len = 0;
  end;
end;
