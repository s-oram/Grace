//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixGray(P: PInt8u; Cv, Alpha: Cardinal; Cover: Cardinal = 0);
begin
  P^ := Int8u((((Cv - P^) * Alpha) + (P^ shl CAggBaseShift)) shr CAggBaseShift);
end;

procedure CopyOrBlendPixGray(P: PInt8u; C: PAggColor;
  Cover: Cardinal); overload;
var
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      P^ := C.V
    else
      BlendPixGray(P, C.V, Alpha, Cover);
  end;
end;

procedure CopyOrBlendPixGray(P: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
      P^ := C.V
    else
      BlendPixGray(P, C.V, C.Rgba8.A);
end;

procedure Gray8CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset)^ := C.V;
end;

procedure Gray8BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixGray(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) +
    X * This.Step + This.Offset), C, Cover);
end;

function Gray8Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  Result.FromValueInteger(P^);
end;

procedure Gray8CopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    P^ := C.V;

    Inc(PtrComp(P), This.Step);
    Dec(Len);
  until Len = 0;
end;

procedure Gray8CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    P^ := C.V;
    P := This.RenderingBuffer.NextRow(P);

    Dec(Len);
  until Len = 0;
end;

procedure Gray8BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      repeat
        P^ := C.V;

        Inc(PtrComp(P), This.Step);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixGray(P, C.V, Alpha, Cover);

        Inc(PtrComp(P), This.Step);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt8u;

  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
      repeat
        P^ := C.V;
        P := This.RenderingBuffer.NextRow(P);

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixGray(P, C.V, Alpha, Cover);

        P := This.RenderingBuffer.NextRow(P);

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8BlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
        P^ := C.V
      else
        BlendPixGray(P, C.V, Alpha, Covers^);

      Inc(PtrComp(P), This.Step);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt8u;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
        P^ := C.V
      else
        BlendPixGray(P, C.V, Alpha, Covers^);

      P := This.RenderingBuffer.NextRow(P);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGray(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(P), This.Step);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      if Colors.Rgba8.A = CAggBaseMask then
        P^ := Colors.V
      else
        CopyOrBlendPixGray(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), This.Step);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGray(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), This.Step);
      Dec(Len);
    until Len = 0;
end;

procedure Gray8BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGray(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));

      P := This.RenderingBuffer.NextRow(P);

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      if Colors.Rgba8.A = CAggBaseMask then
        P^ := Colors.V
      else
        CopyOrBlendPixGray(P, Colors);

      P := This.RenderingBuffer.NextRow(P);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGray(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := This.RenderingBuffer.NextRow(P);

      Dec(Len);
    until Len = 0;
end;

procedure Gray8CopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt8u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int8u))^,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int8u))^,
    Len * SizeOf(Int8u));
end;

procedure Gray8CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    P^ := Colors.V;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(PtrComp(P), This.Step);
    Dec(Len);
  until Len = 0;
end;

procedure Gray8CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    P^ := Colors.V;

    Inc(PtrComp(Colors), SizeOf(TAggColor));

    P := This.RenderingBuffer.NextRow(P);

    Dec(Len);
  until Len = 0;
end;

procedure Gray8BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Color: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Psrc, Pdst: PInt8u;
begin
  PixWidth := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGray(Pdst, Color, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), PixWidth);
      Inc(PtrComp(Pdst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Psrc, Pdst: PInt8u;
begin
  PixWidth := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGray(Pdst, PAggColor(PtrComp(ColorLUT) + Psrc^ *
        SizeOf(TAggColor)), Cover);

      Inc(PtrComp(Psrc), PixWidth);
      Inc(PtrComp(Pdst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end;
