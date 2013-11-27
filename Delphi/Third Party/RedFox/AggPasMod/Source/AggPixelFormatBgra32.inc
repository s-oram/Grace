// AggPas 2.4 RM3 pixel format definition file

procedure BlendPixBgra(P: PAggOrderBgra; Cr, Cg, Cb, Alpha: Integer;
  Cover: Cardinal = 0);
{$IFDEF PUREPASCAL}
begin
  P^.R := ((Cr - P^.R) * Alpha + (P^.R shl CAggBaseShift)) shr CAggBaseShift;
  P^.G := ((Cg - P^.G) * Alpha + (P^.G shl CAggBaseShift)) shr CAggBaseShift;
  P^.B := ((Cb - P^.B) * Alpha + (P^.B shl CAggBaseShift)) shr CAggBaseShift;
  P^.A := (Alpha + P^.A) - ((Alpha * P^.A + CAggBaseMask) shr CAggBaseShift);
{$ELSE}
asm
{$IFDEF AGG_CPU_386}
    PUSH    EDI
    MOV     EDI, EAX

    // P^.R := ((Cr - P^.R) * Alpha + (P^.R shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [EDI + 2]
    SUB     EDX, EAX
    IMUL    EDX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, EDX
    SHR     EAX, CAggBaseShift
    MOV     [EDI + 2], AL

    // P^.G := ((Cg - P^.G) * Alpha + (P^.G shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [EDI + 1]
    SUB     ECX, EAX
    IMUL    ECX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, ECX
    SHR     EAX, CAggBaseShift
    MOV     [EDI + 1], AL

    // P^.B := ((Cb - P^.B) * Alpha + (P^.B shl CAggBaseShift)) shr CAggBaseShift;
    MOV     EDX, Cb
    MOVZX   EAX, [EDI]
    SUB     EDX, EAX
    IMUL    EDX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, EDX
    SHR     EAX, CAggBaseShift
    MOV     [EDI], AL

    // P^.A := (Alpha + P^.A) - ((Alpha * P^.A + CAggBaseMask) shr CAggBaseShift);
    MOVZX   EAX, [EDI + 3]
    MOV     EDX, EAX
    ADD     EDX, Alpha

    IMUL    EAX, Alpha
    ADD     EAX, CAggBaseMask
    SHR     EAX, CAggBaseShift
    SUB     DL, AL
    MOV     [EDI + 3], DL

    POP    EDI
{$ENDIF}
{$IFDEF AGG_CPU_64}
    // P^.R := ((Cr - P^.R) * Alpha + (P^.R shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [RCX + 2]
    SUB     EDX, EAX
    IMUL    EDX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, EDX
    SHR     EAX, CAggBaseShift
    MOV     [RCX + 2], AL

    // P^.G := ((Cg - P^.G) * Alpha + (P^.G shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [RCX + 1]
    SUB     R8D, EAX
    IMUL    R8D, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, R8D
    SHR     EAX, CAggBaseShift
    MOV     [RCX + 1], AL

    // P^.B := ((Cb - P^.B) * Alpha + (P^.B shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [RCX]
    SUB     R9D, EAX
    IMUL    R9D, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, R9D
    SHR     EAX, CAggBaseShift
    MOV     [RCX], AL

    // P^.A := (Alpha + P^.A) - ((Alpha * P^.A + CAggBaseMask) shr CAggBaseShift);
    MOVZX   EAX, [RCX + 3]
    MOV     EDX, EAX
    ADD     EDX, Alpha

    IMUL    EAX, Alpha
    ADD     EAX, CAggBaseMask
    SHR     EAX, CAggBaseShift
    SUB     DL, AL
    MOV     [RCX + 3], DL
{$ENDIF}
{$IFDEF AGG_CPU_PPC}
    yet undefined, please use PUREPASCAL implementation
{$ENDIF}
{$ENDIF}
end;

procedure CopyOrBlendPixBgra(P: PAggOrderBgra;
  Cr, Cg, Cb, Alpha: Cardinal); inline; overload;
begin
  if Alpha <> 0 then
    if Alpha = CAggBaseMask then
    begin
      P^.R := Cr;
      P^.G := Cg;
      P^.B := Cb;
      P^.A := CAggBaseMask;
    end
    else
      BlendPixBgra(P, Cr, Cg, Cb, Alpha);
end;

procedure CopyOrBlendPixBgra(P: PAggOrderBgra;
  Cr, Cg, Cb, Alpha, Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixBgra(P, Cr, Cg, Cb, Alpha)
  else if Alpha <> 0 then
  begin
    Alpha := (Alpha * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      P^.R := Cr;
      P^.G := Cg;
      P^.B := Cb;
      P^.A := CAggBaseMask;
    end
    else
      BlendPixBgra(P, Cr, Cg, Cb, Alpha, Cover);
  end;
end;

procedure Bgra32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  P^.R := C.Rgba8.R;
  P^.G := C.Rgba8.G;
  P^.B := C.Rgba8.B;
  P^.A := C.Rgba8.A;
end;

procedure Bgra32BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  CopyOrBlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
end;

function Bgra32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  Result.FromRgbaInteger(P^.R, P^.G, P^.B, P^.A);
end;

procedure Bgra32CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PAggOrderBgra;
  V: TAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  V.R := C.Rgba8.R;
  V.G := C.Rgba8.G;
  V.B := C.Rgba8.B;
  V.A := C.Rgba8.A;

  Fill32Bit(P^, Len, V);
end;

procedure Bgra32CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PAggOrderBgra;
  V: TAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  V.R := C.Rgba8.R;
  V.G := C.Rgba8.G;
  V.B := C.Rgba8.B;
  V.A := C.Rgba8.A;

  if Len > 0 then
    repeat
      P^ := V;
      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgra;
  V: TAggOrderBgra;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgra(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      V.R := C.Rgba8.R;
      V.G := C.Rgba8.G;
      V.B := C.Rgba8.B;
      V.A := C.Rgba8.A;

      Fill32Bit(P^, Len, V);
    end
    else if Cover = 255 then
      repeat
        BlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(P);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        Inc(P);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32BlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgra;
  V: TAggOrderBgra;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    Alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if Alpha = CAggBaseMask then
    begin
      V.R := C.Rgba8.R;
      V.G := C.Rgba8.G;
      V.B := C.Rgba8.B;
      V.A := C.Rgba8.A;

      repeat
        P^ := V;
        P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PAggOrderBgra;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;
        P^.A := CAggBaseMask;
      end
      else
        BlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(P);
      Inc(Covers);

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PAggOrderBgra;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    repeat
      Alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;
        P^.A := CAggBaseMask;
      end
      else
        BlendPixBgra(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgra(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(Covers);
      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgra(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(Covers);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PAggOrderBgra;
  Incp: Integer;
begin
  Psrc := PAggOrderBgra(SourcePtr);
  Pdst := PAggOrderBgra(This.RenderingBuffer.Row(Ydst));
  Inc(Pdst, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(Psrc), ((Len - 1) shl 2));
    Inc(PtrComp(Pdst), ((Len - 1) shl 2));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(Pdst, Psrc^.R, Psrc^.G, Psrc^.B, Psrc^.A);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(Pdst, Psrc^.R, Psrc^.G, Psrc^.B, Psrc^.A, Cover);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32CopyColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := Colors.Rgba8.R;
    P^.G := Colors.Rgba8.G;
    P^.B := Colors.Rgba8.B;
    P^.A := Colors.Rgba8.A;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(P);
    Dec(Len);
  until Len = 0;
end;

procedure Bgra32CopyColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; Colors: PAggColor);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := Colors.Rgba8.R;
    P^.G := Colors.Rgba8.G;
    P^.B := Colors.Rgba8.B;
    P^.A := Colors.Rgba8.A;

    P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Bgra32BlendFromColor(This, From: TAggPixelFormatProcessor;
  Color: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  Dest: PAggOrderBgra;
begin
  PixWidth := From.PixWidth;
  Source := Pointer(PtrComp(From.GetRowPointer(Ysrc)) + Xsrc * PixWidth);

  if Source <> nil then
  begin
    Dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Dest, Xdst);

    repeat
      CopyOrBlendPixBgra(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Source^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Source), PixWidth);
      Inc(Dest);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Source: PInt8u;
  Dest: PAggOrderBgra;

  Color: PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := Pointer(PtrComp(From.GetRowPointer(Ysrc)) + Xsrc * PixWidth);

  if Source <> nil then
  begin
    Dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Dest, Xdst);

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgra(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(Dest), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgra(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A, Cover);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(Dest), 4);
        Dec(Len);
      until Len = 0;
  end;
end;
