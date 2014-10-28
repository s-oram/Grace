// AggPas 2.4 RM3 pixel format definition file

procedure BlendPixRgba(P: PAggOrderRgba; Cr, Cg, Cb, Alpha: Integer;
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
    MOVZX   EAX, [EDI]
    SUB     EDX, EAX
    IMUL    EDX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, EDX
    SHR     EAX, CAggBaseShift
    MOV     [EDI], AL

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
    MOVZX   EAX, [EDI + 2]
    SUB     EDX, EAX
    IMUL    EDX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, EDX
    SHR     EAX, CAggBaseShift
    MOV     [EDI + 2], AL

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
    MOVZX   EAX, [RCX]
    SUB     EDX, EAX
    IMUL    EDX, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, EDX
    SHR     EAX, CAggBaseShift
    MOV     [RCX], AL

    // P^.G := ((Cg - P^.G) * Alpha + (P^.G shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [RCX + 1]
    SUB     R8D, EAX
    IMUL    R8D, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, R8D
    SHR     EAX, CAggBaseShift
    MOV     [RCX + 1], AL

    // P^.B := ((Cb - P^.B) * Alpha + (P^.B shl CAggBaseShift)) shr CAggBaseShift;
    MOVZX   EAX, [RCX + 2]
    SUB     R9D, EAX
    IMUL    R9D, Alpha
    SHL     EAX, CAggBaseShift
    ADD     EAX, R9D
    SHR     EAX, CAggBaseShift
    MOV     [RCX + 2], AL

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

procedure CopyOrBlendPixRgba(P: PAggOrderRgba;
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
      BlendPixRgba(P, Cr, Cg, Cb, Alpha);
end;

procedure CopyOrBlendPixRgba(P: PAggOrderRgba;
  Cr, Cg, Cb, Alpha, Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixRgba(P, Cr, Cg, Cb, Alpha)
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
      BlendPixRgba(P, Cr, Cg, Cb, Alpha, Cover);
  end;
end;

procedure Rgba32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  P^.R := C.Rgba8.R;
  P^.G := C.Rgba8.G;
  P^.B := C.Rgba8.B;
  P^.A := C.Rgba8.A;
end;

procedure Rgba32BlendPixel(This: TAggPixelFormatProcessor;
  X, Y: Integer; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  CopyOrBlendPixRgba(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
end;

function Rgba32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  Result.FromRgbaInteger(P^.R, P^.G, P^.B, P^.A);
end;

procedure Rgba32CopyHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PAggOrderRgba;
  V: TAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  V.R := C.Rgba8.R;
  V.G := C.Rgba8.G;
  V.B := C.Rgba8.B;
  V.A := C.Rgba8.A;

  Fill32Bit(P^, Len, V);
end;

procedure Rgba32CopyVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
var
  P: PAggOrderRgba;
  V: TAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  V.R := C.Rgba8.R;
  V.G := C.Rgba8.G;
  V.B := C.Rgba8.B;
  V.A := C.Rgba8.A;

  if Len > 0 then
    repeat
      P^ := V;
      P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderRgba;
  V: TAggOrderRgba;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
    Inc(P, X);

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
        BlendPixRgba(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(P);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgba(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        Inc(P);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32BlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PAggOrderRgba;
  V: TAggOrderRgba;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
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
        P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixRgba(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgba(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PAggOrderRgba;
  Alpha: Cardinal;
begin
  if C^.Rgba8.A <> 0 then
  begin
    P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
    Inc(P, X);

    repeat
      Alpha := (C^.Rgba8.A * (Covers^ + 1)) shr 8;

      if Alpha = CAggBaseMask then
      begin
        P^.R := C.Rgba8.R;
        P^.G := C.Rgba8.G;
        P^.B := C.Rgba8.B;
        P^.A := CAggBaseMask;
      end
      else
        BlendPixRgba(P, C^.Rgba8.R, C^.Rgba8.G, C^.Rgba8.B, Alpha, Covers^);

      Inc(P);
      Inc(Covers);

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PAggOrderRgba;
  Alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
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
        BlendPixRgba(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgba(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(Covers);
      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendColorVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgba(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(Covers);

      P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Psrc_: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Psrc, Pdst: PAggOrderRgba;
  Incp: Integer;
begin
  Psrc := PAggOrderRgba(Psrc_);
  Pdst := PAggOrderRgba(This.RenderingBuffer.Row(Ydst));
  Inc(Pdst, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(Psrc), (Len - 1) shl 2);
    Inc(PtrComp(Pdst), (Len - 1) shl 2);

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(Pdst, TAggOrderRgba(Psrc^).R, TAggOrderRgba(Psrc^).G,
        TAggOrderRgba(Psrc^).B, TAggOrderRgba(Psrc^).A);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(Pdst, TAggOrderRgba(Psrc^).R, TAggOrderRgba(Psrc^).G,
        TAggOrderRgba(Psrc^).B, TAggOrderRgba(Psrc^).A, Cover);

      Inc(PtrComp(Psrc), Incp);
      Inc(PtrComp(Pdst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32CopyColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor);
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
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

procedure Rgba32CopyColorVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor);
var
  P: PAggOrderRgba;
begin
  P := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := Colors.Rgba8.R;
    P^.G := Colors.Rgba8.G;
    P^.B := Colors.Rgba8.B;
    P^.A := Colors.Rgba8.A;

    P := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgba32BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Color: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc: PInt8u;
  Pdst: PAggOrderRgba;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PAggOrderRgba(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Pdst, Xdst);

    repeat
      CopyOrBlendPixRgba(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendFromLUT(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  Psrc: PInt8u;
  Pdst: PAggOrderRgba;

  Color: PAggColor;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PAggOrderRgba(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Pdst, Xdst);

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgba(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgba(Pdst, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A, Cover);

        Inc(PtrComp(Psrc), Ppsz);
        Inc(PtrComp(Pdst), 4);
        Dec(Len);
      until Len = 0;
  end;
end;
