//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgraPre(P: PAggOrderBgra; Cr, Cg, Cb, Alpha: Cardinal);
  overload;
{$IFDEF PUREPASCAL}
begin
  Alpha := CAggBaseMask - Alpha;

  P^.R := Int8u(((P^.R * Alpha) shr CAggBaseShift) + Cr);
  P^.G := Int8u(((P^.G * Alpha) shr CAggBaseShift) + Cg);
  P^.B := Int8u(((P^.B * Alpha) shr CAggBaseShift) + Cb);
  P^.A := Int8u(CAggBaseMask - ((Alpha * (CAggBaseMask - P^.A)) shr
    CAggBaseShift));
{$ELSE}
asm
{$IFDEF AGG_CPU_386}
    PUSH    EBX
    PUSH    EDI
    MOV     EDI, EAX

    // Alpha := CAggBaseMask - Alpha;
    MOV     EBX, CAggBaseMask
    SUB     EBX, Alpha

    // P^.R := Int8u(((P^.R * Alpha) shr CAggBaseShift) + Cr);
    MOVZX   EAX, [EDI + $2]
    IMUL    EAX, EBX
    SHR     EAX, $08
    ADD     EAX, EDX
    MOV     [EDI + $2], AL

    // P^.G := Int8u(((P^.G * Alpha) shr CAggBaseShift) + Cg);
    MOVZX   EAX, [EDI + $1]
    IMUL    EAX, EBX
    SHR     EAX, $08
    ADD     EAX, ECX
    MOV     [EDI + $1], AL

    // P^.B := Int8u(((P^.B * Alpha) shr CAggBaseShift) + Cb);
    MOVZX   EAX, [EDI]
    IMUL    EAX, EBX
    SHR     EAX, $08
    ADD     EAX, Cb
    MOV     [EDI], AL

    // P^.A := Int8u(CAggBaseMask - ((Alpha * (CAggBaseMask - P^.A)) shr CAggBaseShift
    MOV     EAX, CAggBaseMask
    MOVZX   EDX, [EDI]
    SUB     EAX, EDX
    IMUL    EAX, EBX
    SHR     EAX, $08
    MOV     EDX, CAggBaseMask
    SUB     EDX, EAX
    MOV     [EDI + $3], DL

    POP    EDI
    POP    EBX
{$ENDIF}
{$IFDEF AGG_CPU_64}
    // Alpha := CAggBaseMask - Alpha;
    MOV     R10D, CAggBaseMask
    SUB     R10D, Alpha

    // P^.R := Int8u(((P^.R * Alpha) shr CAggBaseShift) + Cr);
    MOVZX   EAX, [RCX + $2]
    IMUL    EAX, R10D
    SHR     EAX, $08
    ADD     EAX, EDX
    MOV     [RCX + $2], AL

    // P^.G := Int8u(((P^.G * Alpha) shr CAggBaseShift) + Cg);
    MOVZX   EAX, [RCX + $1]
    IMUL    EAX, R10D
    SHR     EAX, $08
    ADD     EAX, R8D
    MOV     [RCX + $1], AL

    // P^.B := Int8u(((P^.B * Alpha) shr CAggBaseShift) + Cb);
    MOVZX   EAX, [RCX]
    IMUL    EAX, R10D
    SHR     EAX, $08
    ADD     EAX, R9D
    MOV     [RCX], AL

    // P^.A := Int8u(CAggBaseMask - ((Alpha * (CAggBaseMask - P^.A)) shr CAggBaseShift
    MOV     EAX, CAggBaseMask
    MOVZX   EDX, [RCX]
    SUB     EAX, EDX
    IMUL    EAX, R10
    SHR     EAX, $08
    MOV     EDX, CAggBaseMask
    SUB     EDX, EAX
    MOV     [RCX + $3], DL
{$ENDIF}
{$IFDEF AGG_CPU_PPC}
    yet undefined, please use PUREPASCAL implementation
{$ENDIF}
{$ENDIF}
end;

procedure BlendPixBgraPre(P: PAggOrderBgra; Cr, Cg, Cb, Alpha, Cover: Cardinal);
  overload;
begin
  Alpha := CAggBaseMask - Alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  P^.R := Int8u((P^.R * Alpha + Cr * Cover) shr CAggBaseShift);
  P^.G := Int8u((P^.G * Alpha + Cg * Cover) shr CAggBaseShift);
  P^.B := Int8u((P^.B * Alpha + Cb * Cover) shr CAggBaseShift);
  P^.A := Int8u(CAggBaseMask - ((Alpha * (CAggBaseMask - P^.A)) shr
    CAggBaseShift));
end;

procedure CopyOrBlendPixBgraPre(P: PAggOrderBgra; Cr, Cg, Cb,
  Alpha: Cardinal); inline; overload;
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
      BlendPixBgraPre(P, Cr, Cg, Cb, Alpha);
end;

procedure CopyOrBlendPixBgraPre(P: PAggOrderBgra; Cr, Cg, Cb, Alpha,
  Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixBgraPre(P, Cr, Cg, Cb, Alpha)
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
      BlendPixBgraPre(P, Cr, Cg, Cb, Alpha, Cover);
  end;
end;

procedure Bgra32PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  CopyOrBlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
end;

procedure Bgra32PreBlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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

      Fill32Bit(P^, Len, V);
    end
    else if Cover = 255 then
      repeat
        BlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        Inc(P);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        Inc(P);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32PreBlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha);

        P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Cover);

        P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
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
        BlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      Inc(P);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
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
        BlendPixBgraPre(P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, Alpha, Covers^);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgraPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(Covers);
      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      Inc(P);
      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendColorVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  P: PAggOrderBgra;
begin
  P := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(P, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgraPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Covers^);

      Inc(Covers);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(P, Colors.Rgba8.R, Colors.Rgba8.G, Colors.Rgba8.B,
        Colors.Rgba8.A, Cover);

      P := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(P)));

      Inc(PtrComp(Colors), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; xdst, ydst, xsrc, ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Source, Dest: PAggOrderBgra;
  Incp: Integer;
begin
  Source := PAggOrderBgra(SourcePtr);
  Dest := PAggOrderBgra(This.RenderingBuffer.Row(Ydst));
  Inc(Dest, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(Source, Len - 1);
    Inc(Dest, Len - 1);

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(Dest, Source^.R, Source^.G, Source^.B, Source^.A);

      Inc(PtrComp(Source), Incp);
      Inc(PtrComp(Dest), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(Dest, Source^.R, Source^.G, Source^.B, Source^.A,
        Cover);

      Inc(PtrComp(Source), Incp);
      Inc(PtrComp(Dest), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
  Color: PAggColor; xdst, ydst, xsrc, ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Source: PInt8u;
  Dest: PAggOrderBgra;
begin
  PixWidth := From.PixWidth;
  Source := From.GetRowPointer(Ysrc);

  if Source <> nil then
  begin
    Dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Dest, Xdst);

    repeat
      CopyOrBlendPixBgraPre(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
        Color.Rgba8.A, ShrInt32(Source^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Source), PixWidth);
      Inc(PtrComp(Dest), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; xdst, ydst, xsrc, ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
 PixWidth : Cardinal;

 Source : PInt8u;
 Dest : PAggOrderBgra;

 Color : PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := From.GetRowPointer(Ysrc);

  if Source <> nil then
  begin
    Dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(Dest, Xdst);

    if Cover = 255 then
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgraPre(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A);

        Inc(PtrComp(Source), PixWidth);
        Inc(Dest);
        Dec(Len);
      until Len = 0
    else
      repeat
        Color := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgraPre(Dest, Color.Rgba8.R, Color.Rgba8.G, Color.Rgba8.B,
          Color.Rgba8.A, Cover);

        Inc(PtrComp(Source), PixWidth);
        Inc(Dest);
        Dec(Len);
      until Len = 0;
  end;
end;
