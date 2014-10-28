// AggPas 2.4 RM3 pixel format definition file

procedure BlendPixBgr(P: PAggOrderBgr; Cr, Cg, Cb, Alpha: Integer;
  Cover: Cardinal = 0);
{$IFDEF PUREPASCAL}
begin
  P^.R := P^.R + ((Cr - P^.R) * Alpha) shr CAggBaseShift;
  P^.G := P^.G + ((Cg - P^.G) * Alpha) shr CAggBaseShift;
  P^.B := P^.B + ((Cb - P^.B) * Alpha) shr CAggBaseShift;
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
{$ENDIF}
{$IFDEF AGG_CPU_PPC}
    yet undefined, please use PUREPASCAL implementation
{$ENDIF}
{$ENDIF}
end;

procedure CopyOrBlendPixBgr(P: PAggOrderBgr; C: PAggColor; Cover: Cardinal);
  overload;
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
      BlendPixBgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgr(P: PAggOrderBgr; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      P^.R := C.Rgba8.R;
      P^.G := C.Rgba8.G;
      P^.B := C.Rgba8.B;
    end
    else
      BlendPixBgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A);
end;

procedure Bgr24CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  P^.R := C.Rgba8.R;
  P^.G := C.Rgba8.G;
  P^.B := C.Rgba8.B;
end;

procedure Bgr24BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);
  CopyOrBlendPixBgr(P, C, Cover);
end;

function Bgr24Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  Result.FromRgbaInteger(P^.R, P^.G, P^.B);
end;

procedure Bgr24CopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := C.Rgba8.R;
    P^.G := C.Rgba8.G;
    P^.B := C.Rgba8.B;

    Inc(P);
    Dec(Len);
  until Len = 0;
end;

procedure Bgr24CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := C.Rgba8.R;
    P^.G := C.Rgba8.G;
    P^.B := C.Rgba8.B;

    P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8U(P)));

    Dec(Len);
  until Len = 0;
end;

procedure Bgr24BlendHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        BlendPixBgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(PtrComp(P), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
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

        P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixBgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(PtrComp(P), 3);
      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixBgr(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgr(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Covers, 1);
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgr(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgr(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24BlendColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgr(P, Colors, Covers^);

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Inc(Covers);

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgr(P, Colors);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgr(P, Colors, Cover);

      Inc(PtrComp(Colors), SizeOf(TAggColor));

      P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

      Dec(Len);
    until Len = 0;
end;

procedure Bgr24BlendFrom(This, From: TAggPixelFormatProcessor;
  Psrc: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Dest: PAggOrderBgr;
  Color: TAggColor;
  Alpha: Cardinal;
begin
  Dest := PAggOrderBgr(This.RenderingBuffer.Row(Ydst));
  Inc(Dest, Xdst);

  if Cover = 255 then
    repeat
      Alpha := PInt8u(PtrComp(Psrc) + From.Order.A)^;

      if Alpha <> 0 then
        if Alpha = CAggBaseMask then
        begin
          Dest^.R := PInt8u(PtrComp(Psrc) + From.Order.R)^;
          Dest^.G := PInt8u(PtrComp(Psrc) + From.Order.G)^;
          Dest^.B := PInt8u(PtrComp(Psrc) + From.Order.B)^;
        end
        else
          BlendPixBgr(Dest, PInt8u(PtrComp(Psrc) + From.Order.R)^,
            PInt8u(PtrComp(Psrc) + From.Order.G)^,
            PInt8u(PtrComp(Psrc) + From.Order.B)^, Alpha);

      Inc(PtrComp(Psrc), 4);
      Inc(Dest);
      Dec(Len);
    until Len = 0
  else
    repeat
      Color.FromRgbaInteger(PInt8u(PtrComp(Psrc) + From.Order.R)^,
        PInt8u(PtrComp(Psrc) + From.Order.G)^,
        PInt8u(PtrComp(Psrc) + From.Order.B)^,
        PInt8u(PtrComp(Psrc) + From.Order.A)^);

      CopyOrBlendPixBgr(Dest, @Color, Cover);

      Inc(PtrComp(Psrc), 4);
      Inc(PtrComp(Dest), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24CopyColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := Colors.Rgba8.R;
    P^.G := Colors.Rgba8.G;
    P^.B := Colors.Rgba8.B;

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Inc(PtrComp(P), 3);
    Dec(Len);
  until Len = 0;
end;

procedure Bgr24CopyColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PAggOrderBgr;
begin
  P := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  repeat
    P^.R := Colors.Rgba8.R;
    P^.G := Colors.Rgba8.G;
    P^.B := Colors.Rgba8.B;

    P := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(P)));

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Bgr24BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Color: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8U;
  Dest: PAggOrderBgr;
begin
  PixWidth := From.PixWidth;
  Source := PInt8U(From.GetRowPointer(Ysrc));

  if Source <> nil then
  begin
    Dest := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Dest, Xdst);

    repeat
      CopyOrBlendPixBgr(Dest, Color, ShrInt32(Source^ * Cover +
        CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Source), PixWidth);
      Inc(PtrComp(Dest), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendFromLut(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8U;
  Dest: PAggOrderBgr;
  Color: PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := PInt8U(From.GetRowPointer(Ysrc));

  if Source <> nil then
  begin
    Dest := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Dest, Xdst);

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        BlendPixBgr(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(Dest), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgr(Dest,
          PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(Dest), 3);
        Dec(Len);
      until Len = 0;
  end;
end;
