// AggPas 2.4 RM3 pixel format definition file

procedure CublCopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  This.Blender(This, This.BlendMode,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u)),
    C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, 255);
end;

procedure CublBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor;
  Cover: Int8u);
begin
  This.Blender(This, This.BlendMode,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u)),
    C.Rgba8.R, C.Rgba8.G, C.Rgba8.B, C.Rgba8.A, Cover);
end;

function CublPixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  Result.FromRgbaInteger(PInt8u(PtrComp(P) + This.Order.R)^,
    PInt8u(PtrComp(P) + This.Order.G)^, PInt8u(PtrComp(P) + This.Order.B)^,
    PInt8u(PtrComp(P) + This.Order.A)^);
end;

procedure CublCopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
      C.Rgba8.A, 255);

    Inc(PtrComp(P), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublCopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
      C.Rgba8.A, 255);

    P := This.RenderingBuffer.NextRow(P);

    Dec(Len);
  until Len = 0;
end;

procedure CublBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
      C.Rgba8.A, Cover);

    Inc(PtrComp(P), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
      C.Rgba8.A, Cover);

    P := This.RenderingBuffer.NextRow(P);

    Dec(Len);
  until Len = 0;
end;

procedure CublBlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
      C.Rgba8.A, Covers^);

    Inc(PtrComp(Covers), SizeOf(Int8u));
    Inc(PtrComp(P), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, P, C.Rgba8.R, C.Rgba8.G, C.Rgba8.B,
      C.Rgba8.A, Covers^);

    Inc(PtrComp(Covers), SizeOf(Int8u));

    P := This.RenderingBuffer.NextRow(P);

    Dec(Len);
  until Len = 0;
end;

procedure CublBlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    if Covers <> nil then
    begin
      This.Blender(This, This.BlendMode, P, Colors.Rgba8.R, Colors.Rgba8.G,
        Colors.Rgba8.B, Colors.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      This.Blender(This, This.BlendMode, P, Colors.Rgba8.R, Colors.Rgba8.G,
        Colors.Rgba8.B, Colors.Rgba8.A, Cover);

    Inc(PtrComp(P), 4);
    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    if Covers <> nil then
    begin
      This.Blender(This, This.BlendMode, P, Colors.Rgba8.R, Colors.Rgba8.G,
        Colors.Rgba8.B, Colors.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      This.Blender(This, This.BlendMode, P, Colors.Rgba8.R, Colors.Rgba8.G,
        Colors.Rgba8.B, Colors.Rgba8.A, Cover);

    P := This.RenderingBuffer.NextRow(P);

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Psrc, Pdst: PInt8u;
  Incp      : Integer;
begin
  Psrc := SourcePtr;
  Pdst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + (Xdst shl 2) *
    SizeOf(Int8u));
  Incp := 4;

  if Xdst > Xsrc then
  begin
    Inc(PtrComp(Psrc), (Len - 1) shl 2);
    Inc(PtrComp(Pdst), (Len - 1) shl 2);

    Incp := -4;
  end;

  repeat
    This.Blender(This, This.BlendMode, Pdst,
      PInt8u(PtrComp(Psrc) + From.Order.R)^,
      PInt8u(PtrComp(Psrc) + From.Order.G)^,
      PInt8u(PtrComp(Psrc) + From.Order.B)^,
      PInt8u(PtrComp(Psrc) + From.Order.A)^, Cover);

    Inc(PtrComp(Psrc), Incp);
    Inc(PtrComp(Pdst), Incp);
    Dec(Len);
  until Len = 0;
end;

procedure CublCopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

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

procedure CublCopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  P: PInt8u;
begin
  P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    PInt8u(PtrComp(P) + This.Order.R)^ := Colors.Rgba8.R;
    PInt8u(PtrComp(P) + This.Order.G)^ := Colors.Rgba8.G;
    PInt8u(PtrComp(P) + This.Order.B)^ := Colors.Rgba8.B;
    PInt8u(PtrComp(P) + This.Order.A)^ := Colors.Rgba8.A;

    P := This.RenderingBuffer.NextRow(P);

    Inc(PtrComp(Colors), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendFromColor(This, From: TAggPixelFormatProcessor;
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
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    repeat
      This.Blender(This, This.BlendMode, Pdst, Color.Rgba8.R, Color.Rgba8.G,
        Color.Rgba8.B, Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure CublBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  Psrc, Pdst: PInt8u;
  Color: PAggColor;
begin
  Ppsz := From.PixWidth;
  Psrc := From.GetRowPointer(Ysrc);

  if Psrc <> nil then
  begin
    Pdst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    repeat
      Color := PAggColor(PtrComp(ColorLUT) + Psrc^ * SizeOf(TAggColor));

      This.Blender(This, This.BlendMode, Pdst, Color.Rgba8.R, Color.Rgba8.G,
        Color.Rgba8.B, Color.Rgba8.A, ShrInt32(Psrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(Psrc), Ppsz);
      Inc(PtrComp(Pdst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;
