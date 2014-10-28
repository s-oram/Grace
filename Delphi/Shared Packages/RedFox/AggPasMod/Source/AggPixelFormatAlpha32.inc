// AggPas 2.4 RM3 pixel format definition file

procedure Alpha32CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PCardinal;
  V: Cardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

  PInt8u(PtrComp(@V) + This.Order.R)^ := C.Rgba8.R;
  PInt8u(PtrComp(@V) + This.Order.G)^ := C.Rgba8.G;
  PInt8u(PtrComp(@V) + This.Order.B)^ := C.Rgba8.B;
  PInt8u(PtrComp(@V) + This.Order.A)^ := C.Rgba8.A;

  if Len > 0 then
    repeat
      P^ := V;

      Inc(PtrComp(P), 4);
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PCardinal;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        PInt8u(PtrComp(P) + This.Order.R)^ := C.Rgba8.R;
        PInt8u(PtrComp(P) + This.Order.G)^ := C.Rgba8.G;
        PInt8u(PtrComp(P) + This.Order.B)^ := C.Rgba8.B;
        PInt8u(PtrComp(P) + This.Order.A)^ := CAggBaseMask;
      end
      else
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Covers^);
          1:
            BlendPixArgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);
          2:
            BlendPixBgra(PAggOrderBgra(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Covers^);
          3:
            BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);
        end;

      Inc(PtrComp(P), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)));
  Inc(P, X);

  if Covers <> nil then
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Covers^);
        1:
          CopyOrBlendPixArgb(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Covers^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Covers^);
        3:
          CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Covers^);
      end;

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A);
        1:
          CopyOrBlendPixArgb(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A);
        3:
          CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A);
      end;

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Cover);
        1:
          CopyOrBlendPixArgb(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Cover);
        3:
          CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Cover);
      end;

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PCardinal;
  Incp: Integer;
begin
  Psrc := PCardinal(SourcePtr);
  Pdst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
    (Xdst shl 2) * SizeOf(Int8u));
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(Psrc), ((Len - 1) shl 2) * SizeOf(Int8u));
    Inc(PtrComp(Pdst), ((Len - 1) shl 2) * SizeOf(Int8u));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(Pdst),
            PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^);
        1:
          CopyOrBlendPixArgb(Pdst, PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(Pdst),
            PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^);
        3:
          CopyOrBlendPixAbgr(Pdst, PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^);
      end;

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(Pdst),
            PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^, Cover);
        1:
          CopyOrBlendPixArgb(Pdst, PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(Pdst),
            PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^, Cover);
        3:
          CopyOrBlendPixAbgr(Pdst, PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^,
            PInt8u(PtrComp(Psrc) + From.Order.A)^, Cover);
      end;

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0;
end;

function Alpha32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PCardinal;
begin
  P := PCardinal(This.RenderingBuffer.Row(Y));

  if P <> nil then
  begin
    Inc(PtrComp(P), X shl 2);

    Result.FromRgbaInteger(PInt8u(PtrComp(P) + This.Order.R)^,
      PInt8u(PtrComp(P) + This.Order.G)^, PInt8u(PtrComp(P) + This.Order.B)
      ^, PInt8u(PtrComp(P) + This.Order.A)^);
  end;
end;

procedure Alpha32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

  PInt8u(PtrComp(P) + This.Order.R)^ := C.Rgba8.R;
  PInt8u(PtrComp(P) + This.Order.G)^ := C.Rgba8.G;
  PInt8u(PtrComp(P) + This.Order.B)^ := C.Rgba8.B;
  PInt8u(PtrComp(P) + This.Order.A)^ := C.Rgba8.A;
end;

procedure Alpha32BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  case This.Order.R of
    0:
      CopyOrBlendPixRgba(PAggOrderRgba(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
    1:
      CopyOrBlendPixArgb(PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
    2:
      CopyOrBlendPixBgra(PAggOrderBgra(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
    3:
      CopyOrBlendPixAbgr(PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
  end;
end;

procedure Alpha32CopyVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PCardinal;
  V: Cardinal;
begin
  PInt8u(PtrComp(@V) + This.Order.R)^ := C.Rgba8.R;
  PInt8u(PtrComp(@V) + This.Order.G)^ := C.Rgba8.G;
  PInt8u(PtrComp(@V) + This.Order.B)^ := C.Rgba8.B;
  PInt8u(PtrComp(@V) + This.Order.A)^ := C.Rgba8.A;

  if Len > 0 then
    repeat
      P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));
      P^ := V;

      Inc(Y);
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PCardinal;
  V: Cardinal;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      PInt8u(PtrComp(@V) + This.Order.R)^ := C.Rgba8.R;
      PInt8u(PtrComp(@V) + This.Order.G)^ := C.Rgba8.G;
      PInt8u(PtrComp(@V) + This.Order.B)^ := C.Rgba8.B;
      PInt8u(PtrComp(@V) + This.Order.A)^ := C.Rgba8.A;

      repeat
        P^ := V;

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha);
          1:
            BlendPixArgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);
          2:
            BlendPixBgra(PAggOrderBgra(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha);
          3:
            BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);
        end;

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Cover);
          1:
            BlendPixArgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
          2:
            BlendPixBgra(PAggOrderBgra(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Cover);
          3:
            BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
        end;

        Inc(PtrComp(P), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Alpha32BlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PCardinal;
  V: Cardinal;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      PInt8u(PtrComp(@V) + This.Order.R)^ := C.Rgba8.R;
      PInt8u(PtrComp(@V) + This.Order.G)^ := C.Rgba8.G;
      PInt8u(PtrComp(@V) + This.Order.B)^ := C.Rgba8.B;
      PInt8u(PtrComp(@V) + This.Order.A)^ := C.Rgba8.A;

      repeat
        P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));
        P^ := V;

        Inc(Y);
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha);
          1:
            BlendPixArgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);
          2:
            BlendPixBgra(PAggOrderBgra(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha);
          3:
            BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);
        end;

        Inc(Y);
        Dec(Len);
      until Len = 0
    else
      repeat
        P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Cover);
          1:
            BlendPixArgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
          2:
            BlendPixBgra(PAggOrderBgra(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Cover);
          3:
            BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
        end;

        Inc(Y);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Alpha32BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PCardinal;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    repeat
      P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        PInt8u(PtrComp(P) + This.Order.R)^ := C.Rgba8.R;
        PInt8u(PtrComp(P) + This.Order.G)^ := C.Rgba8.G;
        PInt8u(PtrComp(P) + This.Order.B)^ := C.Rgba8.B;
        PInt8u(PtrComp(P) + This.Order.A)^ := CAggBaseMask;
      end
      else
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Covers^);
          1:
            BlendPixArgb(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);
          2:
            BlendPixBgra(PAggOrderBgra(P), C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
              Alpha, Covers^);
          3:
            BlendPixAbgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);
        end;

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(Y);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PCardinal;

begin
  if Covers <> nil then
    repeat
      P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Covers^);
        1:
          CopyOrBlendPixArgb(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Covers^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Covers^);
        3:
          CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Covers^);
      end;

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Y);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A);
        1:
          CopyOrBlendPixArgb(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A);
        3:
          CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A);
      end;

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Y);
      Dec(Len);
    until Len = 0
  else
    repeat
      P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Cover);
        1:
          CopyOrBlendPixArgb(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(P), Colors.Rgba8.R, Colors.Rgba8.G,
            Colors.Rgba8.B, Colors.Rgba8.A, Cover);
        3:
          CopyOrBlendPixAbgr(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
            Colors.Rgba8.A, Cover);
      end;

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Y);
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PCardinal;
begin
  P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

  repeat
    PInt8u(PtrComp(P) + This.Order.R)^ := Colors.Rgba8.R;
    PInt8u(PtrComp(P) + This.Order.G)^ := Colors.Rgba8.G;
    PInt8u(PtrComp(P) + This.Order.B)^ := Colors.Rgba8.B;
    PInt8u(PtrComp(P) + This.Order.A)^ := Colors.Rgba8.A;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(PtrComp(P), 4);
    Dec(Len);
  until Len = 0;
end;

procedure Alpha32CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PCardinal;
begin
  repeat
    P := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

    PInt8u(PtrComp(P) + This.Order.R)^ := Colors.Rgba8.R;
    PInt8u(PtrComp(P) + This.Order.G)^ := Colors.Rgba8.G;
    PInt8u(PtrComp(P) + This.Order.B)^ := Colors.Rgba8.B;
    PInt8u(PtrComp(P) + This.Order.A)^ := Colors.Rgba8.A;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(Y);
    Dec(Len);
  until Len = 0;
end;

procedure Alpha32BlendFromColor(This, From: TAggPixelFormatProcessor;
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
    Pdst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)));
    Inc(Pdst, Xdst);

    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(Pdst), Color.Rgba8.R, Color.Rgba8.G,
            Color.Rgba8.B, Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask,
            CAggBaseShift));
        1:
          CopyOrBlendPixArgb(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
            Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(Pdst), Color.Rgba8.R, Color.Rgba8.G,
            Color.Rgba8.B, Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask,
            CAggBaseShift));
        3:
          CopyOrBlendPixAbgr(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
            Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));
      end;

      Inc(PtrComp(Psrc), Ppsz);
      Inc(Pdst);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendFromLUT(This, From: TAggPixelFormatProcessor;
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
    Pdst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)));
    Inc(Pdst, Xdst);

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        case This.Order.R of
          0:
            CopyOrBlendPixRgba(PAggOrderRgba(Pdst), Color.Rgba8.R,
              Color.Rgba8.G, Color.Rgba8.B, Color.Rgba8.A);
          1:
            CopyOrBlendPixArgb(Pdst, Color.Rgba8.R, Color.Rgba8.G,
              Color.Rgba8.B, Color.Rgba8.A);
          2:
            CopyOrBlendPixBgra(PAggOrderBgra(Pdst), Color.Rgba8.R,
              Color.Rgba8.G, Color.Rgba8.B, Color.Rgba8.A);
          3:
            CopyOrBlendPixAbgr(Pdst, Color.Rgba8.R, Color.Rgba8.G,
              Color.Rgba8.B, Color.Rgba8.A);
        end;

        Inc(PtrComp(Psrc), Ppsz);
        Inc(Pdst);
        Dec(Len);
      until Len = 0
    else
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        case This.Order.R of
          0:
            CopyOrBlendPixRgba(PAggOrderRgba(Pdst), Color.Rgba8.R,
              Color.Rgba8.G, Color.Rgba8.B, Color.Rgba8.A, Cover);
          1:
            CopyOrBlendPixArgb(Pdst, Color.Rgba8.R, Color.Rgba8.G,
              Color.Rgba8.B, Color.Rgba8.A, Cover);
          2:
            CopyOrBlendPixBgra(PAggOrderBgra(Pdst), Color.Rgba8.R,
              Color.Rgba8.G, Color.Rgba8.B, Color.Rgba8.A, Cover);
          3:
            CopyOrBlendPixAbgr(Pdst, Color.Rgba8.R, Color.Rgba8.G,
              Color.Rgba8.B, Color.Rgba8.A, Cover);
        end;

        Inc(PtrComp(Psrc), Ppsz);
        Inc(Pdst);
        Dec(Len);
      until Len = 0;
  end;
end;
