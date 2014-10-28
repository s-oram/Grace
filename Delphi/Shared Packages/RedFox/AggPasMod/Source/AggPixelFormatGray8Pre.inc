//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixGrayPre(P: PInt8u; Cv, Alpha, Cover: Cardinal); overload;
begin
  Alpha := CAggBaseMask - Alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  P^ := Int8u((P^ * Alpha + Cv * Cover) shr CAggBaseShift);
end;

procedure BlendPixGrayPre(P: PInt8u; Cv, Alpha: Cardinal); overload;
begin
  P^ := Int8u(((P^ * (CAggBaseMask - Alpha)) shr CAggBaseShift) + Cv);
end;

procedure CopyOrBlendPixGrayPre(P: PInt8u; C: PAggColor;
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
      BlendPixGrayPre(P, C.V, Alpha, Cover);
  end;
end;

procedure CopyOrBlendPixPre(P: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
      P^ := C.V
    else
      BlendPixGrayPre(P, C.V, C.Rgba8.A);
end;

procedure Gray8PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixGrayPre(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    This.Step + This.Offset), C, Cover);
end;

procedure Gray8PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
        BlendPixGrayPre(P, C.V, Alpha, Cover);

        Inc(PtrComp(P), This.Step);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8PreBlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixGrayPre(P, C.V, Alpha, Cover);

        P := This.RenderingBuffer.NextRow(P);

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixGrayPre(P, C.V, Alpha, Covers^);

      Inc(PtrComp(P), This.Step);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixGrayPre(P, C.V, Alpha, Covers^);

      P := This.RenderingBuffer.NextRow(P);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGrayPre(P, Colors, Covers^);

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
        CopyOrBlendPixPre(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), This.Step);
      Dec(Len);

    until Len = 0
  else
    repeat
      CopyOrBlendPixGrayPre(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), This.Step);
      Dec(Len);
    until Len = 0;
end;

procedure Gray8PreBlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGrayPre(P, Colors, Covers^);

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
        CopyOrBlendPixPre(P, Colors);

      P := This.RenderingBuffer.NextRow(P);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGrayPre(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := This.RenderingBuffer.NextRow(P);

      Dec(Len);
    until Len = 0;
end;

procedure Gray8PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst
      * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGrayPre(Pdst, Color, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc, Pdst: PInt8u;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst
      * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGrayPre(Pdst,
        PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor)), Cover);

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end;
