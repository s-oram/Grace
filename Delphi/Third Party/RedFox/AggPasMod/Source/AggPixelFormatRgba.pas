unit AggPixelFormatRgba;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$Q-}
{$R-}

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  AggBasics,
  AggPixelFormat,
  AggColor,
  AggRenderingBuffer;

procedure PixelFormatBgra32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgba32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatArgb32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatAbgr32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatBgra32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgba32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatArgb32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatAbgr32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure BlendModeAdaptorRgba(This: TAggPixelFormatProcessor;
  BlendMode: TAggBlendMode; P: PInt8u; Cr, Cg, Cb, Ca, Cover: Cardinal);
procedure BlendModeAdaptorClipToDestinationRgbaPre(This: TAggPixelFormatProcessor;
  BlendMode: TAggBlendMode; P: PInt8u; Cr, Cg, Cb, Ca, Cover: Cardinal);

procedure PixelFormatAlphaBlendRgba(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Order: TAggOrder);
procedure PixelFormatCustomBlendRgba(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Bl: TAggFuncBlender; Order: TAggOrder);

implementation

function Format32Row(This: TAggPixelFormatProcessor; X, Y: Integer): TAggRowDataType;
var
  P : PCardinal;
begin
  P := PCardinal(This.RenderingBuffer.Row(Y));
  Inc(P, X);
  Result.Initialize(X, This.Width - 1, PInt8u(P));
end;

procedure Format32CopyFrom(This: TAggPixelFormatProcessor; From: TAggRenderingBuffer;
  Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
var
  PSrc, PDst : PCardinal;
begin
  PSrc := PCardinal(From.Row(Ysrc));
  Inc(PSrc, Xsrc);

  PDst := PCardinal(This.RenderingBuffer.Row(Ydst));
  Inc(PDst, Xdst);

  Move(PSrc^, PDst^, Len * 4);
end;

procedure Order32ForEachPixel(This: TAggPixelFormatProcessor; F: TAggFuncApplyGamma);
var
  Y, Len: Cardinal;
  P: PInt8u;
begin
  Y := 0;

  while Y < This.Height do
  begin
    Len := This.Width;

    P := This.RenderingBuffer.Row(Y);

    repeat
      F(This, P);

      Inc(PtrComp(P), 4);
      Dec(Len);
    until Len = 0;

    Inc(Y);
  end;
end;

procedure Order32GammaDirApply(This: TAggPixelFormatProcessor; P: PInt8u);
begin
  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(This.Apply.Dir[PInt8u(PtrComp(P) + This.Order.R)^]);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(This.Apply.Dir[PInt8u(PtrComp(P) + This.Order.G)^]);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(This.Apply.Dir[PInt8u(PtrComp(P) + This.Order.B)^]);
end;

procedure Order32GammaInvApply(This: TAggPixelFormatProcessor; P: PInt8u);
begin
  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(P) + This.Order.R)^]);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(P) + This.Order.G)^]);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(P) + This.Order.B)^]);
end;

procedure Order32PixelPreMultiply(This: TAggPixelFormatProcessor; P: PInt8u);
var
  A: Cardinal;
begin
  A := PInt8u(PtrComp(P) + This.Order.A)^;

  if A = 0 then
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ := 0;
    PInt8u(PtrComp(P) + This.Order.G)^ := 0;
    PInt8u(PtrComp(P) + This.Order.B)^ := 0;
  end
  else
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.R)^ * A + CAggBaseMask)
      shr CAggBaseShift);

    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.G)^ * A + CAggBaseMask)
      shr CAggBaseShift);

    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.B)^ * A + CAggBaseMask)
      shr CAggBaseShift);
  end;
end;

procedure Order32PixelDeMultiply(This: TAggPixelFormatProcessor; P: PInt8u);
var
  R, G, B, A: Cardinal;
begin
  A := PInt8u(PtrComp(P) + This.Order.A)^;

  if A = 0 then
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ := 0;
    PInt8u(PtrComp(P) + This.Order.G)^ := 0;
    PInt8u(PtrComp(P) + This.Order.B)^ := 0;
  end
  else
  begin
    R := (PInt8u(PtrComp(P) + This.Order.R)^ * CAggBaseMask) div A;
    G := (PInt8u(PtrComp(P) + This.Order.G)^ * CAggBaseMask) div A;
    B := (PInt8u(PtrComp(P) + This.Order.B)^ * CAggBaseMask) div A;

    if R > CAggBaseMask then
      PInt8u(PtrComp(P) + This.Order.R)^ := CAggBaseMask
    else
      PInt8u(PtrComp(P) + This.Order.R)^ := R;

    if G > CAggBaseMask then
      PInt8u(PtrComp(P) + This.Order.G)^ := CAggBaseMask
    else
      PInt8u(PtrComp(P) + This.Order.G)^ := G;

    if B > CAggBaseMask then
      PInt8u(PtrComp(P) + This.Order.B)^ := CAggBaseMask
    else
      PInt8u(PtrComp(P) + This.Order.B)^ := B;
  end;
end;

{$I AggPixelFormatBgra32.inc }

procedure PixelFormatBgra32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgra;
  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Bgra32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgra32BlendPixel;

  PixelFormatProcessor.Pixel := @Bgra32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgra32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgra32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgra32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgra32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgra32BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgra32BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgra32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgra32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgra32BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgra32BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgra32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgra32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgra32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatRgba32.inc }

procedure PixelFormatRgba32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgba;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Rgba32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgba32BlendPixel;

  PixelFormatProcessor.Pixel := @Rgba32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgba32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgba32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgba32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgba32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgba32BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgba32BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgba32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgba32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgba32BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgba32BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgba32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgba32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgba32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatArgb32.inc }

procedure PixelFormatArgb32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderArgb;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Argb32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Argb32BlendPixel;

  PixelFormatProcessor.Pixel := @Argb32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Argb32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Argb32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Argb32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Argb32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Argb32BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Argb32BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Argb32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Argb32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Argb32BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Argb32BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Argb32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Argb32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Argb32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatAbgr32.inc }

procedure PixelFormatAbgr32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderAbgr;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Abgr32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Abgr32BlendPixel;

  PixelFormatProcessor.Pixel := @Abgr32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Abgr32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Abgr32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Abgr32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Abgr32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Abgr32BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Abgr32BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Abgr32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Abgr32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Abgr32BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Abgr32BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Abgr32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Abgr32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Abgr32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatBgra32Pre.inc }

procedure PixelFormatBgra32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgra;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Bgra32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgra32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Bgra32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgra32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgra32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgra32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgra32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgra32PreBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgra32PreBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgra32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgra32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgra32PreBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgra32PreBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgra32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgra32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgra32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatRgba32Pre.inc }

procedure PixelFormatRgba32Pre(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgba;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Rgba32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgba32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Rgba32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgba32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgba32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgba32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgba32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgba32PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgba32PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgba32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgba32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgba32PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgba32PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgba32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgba32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgba32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatArgb32Pre.inc }

procedure PixelFormatArgb32Pre(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderArgb;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Argb32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Argb32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Argb32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Argb32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Argb32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Argb32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Argb32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Argb32PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Argb32PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := Argb32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := Argb32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Argb32PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Argb32PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Argb32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Argb32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Argb32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatAbgr32Pre.inc}

procedure PixelFormatAbgr32Pre(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderAbgr;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Abgr32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Abgr32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Abgr32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Abgr32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Abgr32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Abgr32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Abgr32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Abgr32PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Abgr32PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := Abgr32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := Abgr32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Abgr32PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Abgr32PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Abgr32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Abgr32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Abgr32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatAlpha32.inc }

procedure PixelFormatAlphaBlendRgba(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Order: TAggOrder);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := Order;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Alpha32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Alpha32BlendPixel;

  PixelFormatProcessor.Pixel := @Alpha32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Alpha32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Alpha32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Alpha32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Alpha32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Alpha32BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Alpha32BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Alpha32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Alpha32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Alpha32BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Alpha32BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Alpha32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Alpha32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Alpha32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$I AggPixelFormatCubl32.inc }

procedure PixelFormatCustomBlendRgba(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Bl: TAggFuncBlender; Order: TAggOrder);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := Order;
  PixelFormatProcessor.Blender := Bl;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @CublCopyPixel;
  PixelFormatProcessor.BlendPixel := @CublBlendPixel;

  PixelFormatProcessor.Pixel := @CublPixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @CublCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @CublCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @CublBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @CublBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @CublBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @CublBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @CublCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @CublCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @CublBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @CublBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @CublBlendFrom;

  PixelFormatProcessor.BlendFromColor := @CublBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @CublBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

procedure coRgbaClear(This: TAggPixelFormatProcessor; P: PInt8u;
  Cr, Cg, Cb, Alpha, Cover: Cardinal);
begin
  if Cover < 255 then
  begin
    Cover := 255 - Cover;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.R)^ * Cover + 255) shr 8);
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.G)^ * Cover + 255) shr 8);
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.B)^ * Cover + 255) shr 8);
    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.A)^ * Cover + 255) shr 8);
  end
  else
    PCardinal(P)^ := 0;
end;

procedure coRgbaSrc(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Alpha: Cardinal;
begin
  if Cover < 255 then
  begin
    Alpha := 255 - Cover;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.R)^ * Alpha + 255) shr 8) +
      ((Sr * Cover + 255) shr 8));
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.G)^ * Alpha + 255) shr 8) +
      ((Sg * Cover + 255) shr 8));
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.B)^ * Alpha + 255) shr 8) +
      ((Sb * Cover + 255) shr 8));
    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.A)^ * Alpha + 255) shr 8) +
      ((Sa * Cover + 255) shr 8));
  end
  else
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ := Sr;
    PInt8u(PtrComp(P) + This.Order.G)^ := Sg;
    PInt8u(PtrComp(P) + This.Order.B)^ := Sb;
    PInt8u(PtrComp(P) + This.Order.A)^ := Sa;
  end;
end;

procedure coRgbaDst(This: TAggPixelFormatProcessor; P: PInt8u;
  Cr, Cg, Cb, Alpha, Cover: Cardinal);
begin
end;

// Dca' = Sca + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaSrcOver(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  S1a: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  S1a := CAggBaseMask - Sa;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(Sr + ((PInt8u(PtrComp(P) + This.Order.R)^ * S1a + CAggBaseMask)
    shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(Sg + ((PInt8u(PtrComp(P) + This.Order.G)^ * S1a + CAggBaseMask)
    shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(Sb + ((PInt8u(PtrComp(P) + This.Order.B)^ * S1a + CAggBaseMask)
    shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + PInt8u(PtrComp(P) + This.Order.A)^ -
    ((Sa * PInt8u(PtrComp(P) + This.Order.A)^ + CAggBaseMask)
    shr CAggBaseShift));
end;

// Dca' = Dca + Sca.(1 - Da)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaDstOver(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(PInt8u(PtrComp(P) + This.Order.R)^ +
    ((Sr * D1a + CAggBaseMask) shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(PInt8u(PtrComp(P) + This.Order.G)^ +
    ((Sg * D1a + CAggBaseMask) shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(PInt8u(PtrComp(P) + This.Order.B)^ +
    ((Sb * D1a + CAggBaseMask) shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + PInt8u(PtrComp(P) + This.Order.A)^ -
    ((Sa * PInt8u(PtrComp(P) + This.Order.A)^ + CAggBaseMask)
    shr CAggBaseShift));
end;

// Dca' = Sca.Da
// Da'  = Sa.Da
procedure coRgbaSrcIn(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Da, Alpha: Cardinal;
begin
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  if Cover < 255 then
  begin
    Alpha := 255 - Cover;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.R)^ * Alpha + 255) shr 8) +
      ((((Sr * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.G)^ * Alpha + 255) shr 8) +
      ((((Sg * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.B)^ * Alpha + 255) shr 8) +
      ((((Sb * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.A)^ * Alpha + 255) shr 8) +
      ((((Sa * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));
  end
  else
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((Sr * Da + CAggBaseMask) shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((Sg * Da + CAggBaseMask) shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((Sb * Da + CAggBaseMask) shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u((Sa * Da + CAggBaseMask) shr CAggBaseShift);
  end;
end;

// Dca' = Dca.Sa
// Da'  = Sa.Da
procedure coRgbaDstIn(This: TAggPixelFormatProcessor; P: PInt8u;
  Cr, Cg, Cb, Sa, Cover: Cardinal);
begin
  if Cover < 255 then
    Sa := CAggBaseMask - ((Cover * (CAggBaseMask - Sa) + 255) shr 8);

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.R)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.G)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.B)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.A)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
end;

// Dca' = Sca.(1 - Da)
// Da'  = Sa.(1 - Da)
procedure coRgbaSrcOut(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Da, Alpha: Cardinal;
begin
  Da := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;

  if Cover < 255 then
  begin
    Alpha := 255 - Cover;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.R)^ * Alpha + 255) shr 8) +
      ((((Sr * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.G)^ * Alpha + 255) shr 8) +
      ((((Sg * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.B)^ * Alpha + 255) shr 8) +
      ((((Sb * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.A)^ * Alpha + 255) shr 8) +
      ((((Sa * Da + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));
  end
  else
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((Sr * Da + CAggBaseMask) shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((Sg * Da + CAggBaseMask) shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((Sb * Da + CAggBaseMask) shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u((Sa * Da + CAggBaseMask) shr CAggBaseShift);
  end;
end;

// Dca' = Dca.(1 - Sa)
// Da'  = Da.(1 - Sa)
procedure coRgbaDstOut(This: TAggPixelFormatProcessor; P: PInt8u;
  Cr, Cg, Cb, Sa, Cover: Cardinal);
begin
  if Cover < 255 then
    Sa := (Sa * Cover + 255) shr 8;

  Sa := CAggBaseMask - Sa;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.R)^ * Sa + CAggBaseShift)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.G)^ * Sa + CAggBaseShift)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.B)^ * Sa + CAggBaseShift)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.A)^ * Sa + CAggBaseShift)
    shr CAggBaseShift);
end;

// Dca' = Sca.Da + Dca.(1 - Sa)
// Da'  = Da
procedure coRgbaSrcATop(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  Da := PInt8u(PtrComp(P) + This.Order.A)^;
  Sa := CAggBaseMask - Sa;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((Sr * Da + PInt8u(PtrComp(P) + This.Order.R)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((Sg * Da + PInt8u(PtrComp(P) + This.Order.G)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((Sb * Da + PInt8u(PtrComp(P) + This.Order.B)^ * Sa + CAggBaseMask)
    shr CAggBaseShift);
end;

// Dca' = Dca.Sa + Sca.(1 - Da)
// Da'  = Sa
procedure coRgbaDstATop(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Da, Alpha: Cardinal;
begin
  Da := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;

  if Cover < 255 then
  begin
    Alpha := 255 - Cover;

    Sr := (PInt8u(PtrComp(P) + This.Order.R)^ * Sa + Sr * Da + CAggBaseMask)
      shr CAggBaseShift;
    Sg := (PInt8u(PtrComp(P) + This.Order.G)^ * Sa + Sg * Da + CAggBaseMask)
      shr CAggBaseShift;
    Sb := (PInt8u(PtrComp(P) + This.Order.B)^ * Sa + Sb * Da + CAggBaseMask)
      shr CAggBaseShift;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.R)^ * Alpha + 255) shr 8) +
      ((Sr * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.G)^ * Alpha + 255) shr 8) +
      ((Sg * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.B)^ * Alpha + 255) shr 8) +
      ((Sb * Cover + 255) shr 8));

    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u(((PInt8u(PtrComp(P) + This.Order.A)^ * Alpha + 255) shr 8) +
      ((Sa * Cover + 255) shr 8));
  end
  else
  begin
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.R)^ * Sa + Sr * Da + CAggBaseMask)
      shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.G)^ * Sa + Sg * Da + CAggBaseMask)
      shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((PInt8u(PtrComp(P) + This.Order.B)^ * Sa + Sb * Da + CAggBaseMask)
      shr CAggBaseShift);
    PInt8u(PtrComp(P) + This.Order.A)^ := Int8u(Sa);
  end;
end;

// Dca' = Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - 2.Sa.Da
procedure coRgbaXor(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  S1a, D1a: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  S1a := CAggBaseMask - Sa;
  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.R)^ * S1a + Sr * D1a + CAggBaseMask)
    shr CAggBaseShift);

  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.G)^ * S1a + Sg * D1a + CAggBaseMask)
    shr CAggBaseShift);

  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((PInt8u(PtrComp(P) + This.Order.B)^ * S1a + Sb * D1a + CAggBaseMask)
    shr CAggBaseShift);

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + PInt8u(PtrComp(P) + This.Order.A)^ -
    ((Sa * PInt8u(PtrComp(P) + This.Order.A)^ + CAggBaseMask div 2)
    shr (CAggBaseShift - 1)));
end;

// Dca' = Sca + Dca
// Da'  = Sa + Da
procedure coRgbaPlus(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Dr, Dg, Db, Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  Dr := PInt8u(PtrComp(P) + This.Order.R)^ + Sr;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^ + Sg;
  Db := PInt8u(PtrComp(P) + This.Order.B)^ + Sb;
  Da := PInt8u(PtrComp(P) + This.Order.A)^ + Sa;

  if Dr > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.R)^ := CAggBaseMask
  else
    PInt8u(PtrComp(P) + This.Order.R)^ := Int8u(Dr);

  if Dg > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.G)^ := CAggBaseMask
  else
    PInt8u(PtrComp(P) + This.Order.G)^ := Int8u(Dg);

  if Db > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.B)^ := CAggBaseMask
  else
    PInt8u(PtrComp(P) + This.Order.B)^ := Int8u(Db);

  if Da > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.A)^ := CAggBaseMask
  else
    PInt8u(PtrComp(P) + This.Order.A)^ := Int8u(Da);
end;

// Dca' = Dca - Sca
// Da' = 1 - (1 - Sa).(1 - Da)
procedure coRgbaMinus(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Dr, Dg, Db: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  Dr := PInt8u(PtrComp(P) + This.Order.R)^ - Sr;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^ - Sg;
  Db := PInt8u(PtrComp(P) + This.Order.B)^ - Sb;

  if Dr > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.R)^ := 0
  else
    PInt8u(PtrComp(P) + This.Order.R)^ := Int8u(Dr);

  if Dg > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.G)^ := 0
  else
    PInt8u(PtrComp(P) + This.Order.G)^ := Int8u(Dg);

  if Db > CAggBaseMask then
    PInt8u(PtrComp(P) + This.Order.B)^ := 0
  else
    PInt8u(PtrComp(P) + This.Order.B)^ := Int8u(Db);

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(CAggBaseMask - (((CAggBaseMask - Sa) * (CAggBaseMask -
    PInt8u(PtrComp(P) + This.Order.A)^) + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaMultiply(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  S1a, D1a, Dr, Dg, Db: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  S1a := CAggBaseMask - Sa;
  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((Sr * Dr + Sr * D1a + Dr * S1a + CAggBaseMask) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((Sg * Dg + Sg * D1a + Dg * S1a + CAggBaseMask) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((Sb * Db + Sb * D1a + Db * S1a + CAggBaseMask) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + PInt8u(PtrComp(P) + This.Order.A)^ -
    ((Sa * PInt8u(PtrComp(P) + This.Order.A)^ + CAggBaseMask)
    shr CAggBaseShift));
end;

// Dca' = Sca + Dca - Sca.Dca
// Da'  = Sa + Da - Sa.Da
procedure coRgbaScreen(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Dr, Dg, Db, Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(Sr + Dr - ((Sr * Dr + CAggBaseMask) shr CAggBaseShift));
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(Sg + Dg - ((Sg * Dg + CAggBaseMask) shr CAggBaseShift));
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(Sb + Db - ((Sb * Db + CAggBaseMask) shr CAggBaseShift));
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// if 2.Dca < Da
// Dca' = 2.Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Sa.Da - 2.(Da - Dca).(Sa - Sca) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da' = Sa + Da - Sa.Da
procedure coRgbaOverlay(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da, Sada: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;
  Sada := Sa * PInt8u(PtrComp(P) + This.Order.A)^;

  if 2 * Dr < Da then
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((2 * Sr * Dr + Sr * D1a + Dr * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((Sada - 2 * (Da - Dr) * (Sa - Sr) + Sr * D1a + Dr * S1a)
      shr CAggBaseShift);

  if 2 * Dg < Da then
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((2 * Sg * Dg + Sg * D1a + Dg * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((Sada - 2 * (Da - Dg) * (Sa - Sg) + Sg * D1a + Dg * S1a)
      shr CAggBaseShift);

  if 2 * Db < Da then
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((2 * Sb * Db + Sb * D1a + Db * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((Sada - 2 * (Da - Db) * (Sa - Sb) + Sb * D1a + Db * S1a)
      shr CAggBaseShift);

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

function Sd_min(A, B: Cardinal): Cardinal; inline;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Sd_max(A, B: Cardinal): Cardinal; inline;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

// Dca' = min(Sca.Da, Dca.Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaDarken(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((Sd_min(Sr * Da, Dr * Sa) + Sr * D1a + Dr * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((Sd_min(Sg * Da, Dg * Sa) + Sg * D1a + Dg * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((Sd_min(Sb * Da, Db * Sa) + Sb * D1a + Db * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = max(Sca.Da, Dca.Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaLighten(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((Sd_max(Sr * Da, Dr * Sa) + Sr * D1a + Dr * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((Sd_max(Sg * Da, Dg * Sa) + Sg * D1a + Dg * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((Sd_max(Sb * Da, Db * Sa) + Sb * D1a + Db * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// if Sca.Da + Dca.Sa >= Sa.Da
// Dca' = Sa.Da + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Dca.Sa/(1-Sca/Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaColorDodge(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da: Cardinal;
  Drsa, Dgsa, Dbsa, Srda, Sgda, Sbda, Sada: Integer;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;
  Drsa := Dr * Sa;
  Dgsa := Dg * Sa;
  Dbsa := Db * Sa;
  Srda := Sr * Da;
  Sgda := Sg * Da;
  Sbda := Sb * Da;
  Sada := Sa * Da;

  if Srda + Drsa >= Sada then
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(ShrInt32(Sada + Sr * D1a + Dr * S1a, CAggBaseShift))
  else
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(Drsa div (CAggBaseMask - (Sr shl CAggBaseShift) div Sa) +
      ((Sr * D1a + Dr * S1a) shr CAggBaseShift));

  if Sgda + Dgsa >= Sada then
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(ShrInt32(Sada + Sg * D1a + Dg * S1a, CAggBaseShift))
  else
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(Dgsa div (CAggBaseMask - (Sg shl CAggBaseShift) div Sa) +
      ((Sg * D1a + Dg * S1a) shr CAggBaseShift));

  if Sbda + Dbsa >= Sada then
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(ShrInt32(Sada + Sb * D1a + Db * S1a, CAggBaseShift))
  else
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(Dbsa div (CAggBaseMask - (Sb shl CAggBaseShift) div Sa) +
      ((Sb * D1a + Db * S1a) shr CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// if Sca.Da + Dca.Sa <= Sa.Da
// Dca' = Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Sa.(Sca.Da + Dca.Sa - Sa.Da)/Sca + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaColorBurn(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da: Cardinal;

  Drsa, Dgsa, Dbsa, Srda, Sgda, Sbda, Sada: Integer;

begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;
  Drsa := Dr * Sa;
  Dgsa := Dg * Sa;
  Dbsa := Db * Sa;
  Srda := Sr * Da;
  Sgda := Sg * Da;
  Sbda := Sb * Da;
  Sada := Sa * Da;

  if Srda + Drsa <= Sada then
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((Sr * D1a + Dr * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(ShrInt32(Sa * (Srda + Drsa - Sada) div Sr + Sr * D1a + Dr * S1a,
      CAggBaseShift));

  if Sgda + Dgsa <= Sada then
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((Sg * D1a + Dg * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(ShrInt32(Sa * (Sgda + Dgsa - Sada) div Sg + Sg * D1a + Dg * S1a,
      CAggBaseShift));

  if Sbda + Dbsa <= Sada then
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((Sb * D1a + Db * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(ShrInt32(Sa * (Sbda + Dbsa - Sada) div Sb + Sb * D1a + Db * S1a,
      CAggBaseShift));

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// if 2.Sca < Sa
// Dca' = 2.Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Sa.Da - 2.(Da - Dca).(Sa - Sca) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaHardLight(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da, Sada: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;
  Sada := Sa * Da;

  if 2 * Sr < Sa then
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((2 * Sr * Dr + Sr * D1a + Dr * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u((Sada - 2 * (Da - Dr) * (Sa - Sr) + Sr * D1a + Dr * S1a)
      shr CAggBaseShift);

  if 2 * Sg < Sa then
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((2 * Sg * Dg + Sg * D1a + Dg * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u((Sada - 2 * (Da - Dg) * (Sa - Sg) + Sg * D1a + Dg * S1a)
      shr CAggBaseShift);

  if 2 * Sb < Sa then
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((2 * Sb * Db + Sb * D1a + Db * S1a) shr CAggBaseShift)
  else
    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u((Sada - 2 * (Da - Db) * (Sa - Sb) + Sb * D1a + Db * S1a)
      shr CAggBaseShift);

  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// if 2.Sca < Sa
// Dca' = Dca.(Sa + (1 - Dca/Da).(2.Sca - Sa)) + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise if 8.Dca <= Da
// Dca' = Dca.(Sa + (1 - Dca/Da).(2.Sca - Sa).(3 - 8.Dca/Da)) + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = (Dca.Sa + ((Dca/Da)^(0.5).Da - Dca).(2.Sca - Sa)) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaSoftLight(This: TAggPixelFormatProcessor; P: PInt8u;
  R, G, B, A, Cover: Cardinal);
var
  Sr, Sg, Sb, Sa, Dr, Dg, Db, Da: Double;
begin
  Sr := (R * Cover) / (CAggBaseMask * 255);
  Sg := (G * Cover) / (CAggBaseMask * 255);
  Sb := (B * Cover) / (CAggBaseMask * 255);
  Sa := (A * Cover) / (CAggBaseMask * 255);
  Dr := PInt8u(PtrComp(P) + This.Order.R)^ / CAggBaseMask;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^ / CAggBaseMask;
  Db := PInt8u(PtrComp(P) + This.Order.B)^ / CAggBaseMask;

  if PInt8u(PtrComp(P) + This.Order.A)^ <> 0 then
    Da := PInt8u(PtrComp(P) + This.Order.A)^ / CAggBaseMask
  else
    Da := 1 / CAggBaseMask;

  if Cover < 255 then
    A := (A * Cover + 255) shr 8;

  if 2 * Sr < Sa then
    Dr := Dr * (Sa + (1 - Dr / Da) * (2 * Sr - Sa)) + Sr * (1 - Da) + Dr
      * (1 - Sa)
  else if 8 * Dr <= Da then
    Dr := Dr * (Sa + (1 - Dr / Da) * (2 * Sr - Sa) * (3 - 8 * Dr / Da)) + Sr *
      (1 - Da) + Dr * (1 - Sa)
  else
    Dr := (Dr * Sa + (Sqrt(Dr / Da) * Da - Dr) * (2 * Sr - Sa)) + Sr * (1 - Da)
      + Dr * (1 - Sa);

  if 2 * Sg < Sa then
    Dg := Dg * (Sa + (1 - Dg / Da) * (2 * Sg - Sa)) + Sg * (1 - Da) + Dg
      * (1 - Sa)
  else if 8 * Dg <= Da then
    Dg := Dg * (Sa + (1 - Dg / Da) * (2 * Sg - Sa) * (3 - 8 * Dg / Da)) + Sg *
      (1 - Da) + Dg * (1 - Sa)
  else
    Dg := (Dg * Sa + (Sqrt(Dg / Da) * Da - Dg) * (2 * Sg - Sa)) + Sg * (1 - Da)
      + Dg * (1 - Sa);

  if 2 * Sb < Sa then
    Db := Db * (Sa + (1 - Db / Da) * (2 * Sb - Sa)) + Sb * (1 - Da) + Db
      * (1 - Sa)
  else if 8 * Db <= Da then
    Db := Db * (Sa + (1 - Db / Da) * (2 * Sb - Sa) * (3 - 8 * Db / Da)) + Sb *
      (1 - Da) + Db * (1 - Sa)
  else
    Db := (Db * Sa + (Sqrt(Db / Da) * Da - Db) * (2 * Sb - Sa)) + Sb * (1 - Da)
      + Db * (1 - Sa);

  PInt8u(PtrComp(P) + This.Order.R)^ := Int8u(Trunc(Dr * CAggBaseMask));
  PInt8u(PtrComp(P) + This.Order.G)^ := Int8u(Trunc(Dg * CAggBaseMask));
  PInt8u(PtrComp(P) + This.Order.B)^ := Int8u(Trunc(Db * CAggBaseMask));
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(A + PInt8u(PtrComp(P) + This.Order.A)^ -
    ((A * PInt8u(PtrComp(P) + This.Order.A)^ + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = Sca + Dca - 2.min(Sca.Da, Dca.Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaDifference(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Dr, Dg, Db, Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(Sr + Dr - ((2 * Sd_min(Sr * Da, Dr * Sa)) shr CAggBaseShift));
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(Sg + Dg - ((2 * Sd_min(Sg * Da, Dg * Sa)) shr CAggBaseShift));
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(Sb + Db - ((2 * Sd_min(Sb * Da, Db * Sa)) shr CAggBaseShift));
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = (Sca.Da + Dca.Sa - 2.Sca.Dca) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaExclusion(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  D1a, S1a, Dr, Dg, Db, Da: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  D1a := CAggBaseMask - PInt8u(PtrComp(P) + This.Order.A)^;
  S1a := CAggBaseMask - Sa;
  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u((Sr * Da + Dr * Sa - 2 * Sr * Dr + Sr * D1a + Dr * S1a)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u((Sg * Da + Dg * Sa - 2 * Sg * Dg + Sg * D1a + Dg * S1a)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u((Sb * Da + Db * Sa - 2 * Sb * Db + Sb * D1a + Db * S1a)
    shr CAggBaseShift);
  PInt8u(PtrComp(P) + This.Order.A)^ :=
    Int8u(Sa + Da - ((Sa * Da + CAggBaseMask) shr CAggBaseShift));
end;

procedure coRgbaContrast(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Dr, Dg, Db, Da, D2a, R, G, B: Integer;
  S2a: Cardinal;
begin
  if Cover < 255 then
  begin
    Sr := (Sr * Cover + 255) shr 8;
    Sg := (Sg * Cover + 255) shr 8;
    Sb := (Sb * Cover + 255) shr 8;
    Sa := (Sa * Cover + 255) shr 8;
  end;

  Dr := PInt8u(PtrComp(P) + This.Order.R)^;
  Dg := PInt8u(PtrComp(P) + This.Order.G)^;
  Db := PInt8u(PtrComp(P) + This.Order.B)^;
  Da := PInt8u(PtrComp(P) + This.Order.A)^;
  D2a := ShrInt32(Da, 1);
  S2a := Sa shr 1;

  R := ShrInt32((Dr - D2a) * ((Sr - S2a) * 2 + CAggBaseMask), CAggBaseShift) + D2a;
  G := ShrInt32((Dg - D2a) * ((Sg - S2a) * 2 + CAggBaseMask), CAggBaseShift) + D2a;
  B := ShrInt32((Db - D2a) * ((Sb - S2a) * 2 + CAggBaseMask), CAggBaseShift) + D2a;

  if R < 0 then
    R := 0;

  if G < 0 then
    G := 0;

  if B < 0 then
    B := 0;

  if R > Da then
    PInt8u(PtrComp(P) + This.Order.R)^ := Int8u(Trunc(Da))
  else
    PInt8u(PtrComp(P) + This.Order.R)^ := Int8u(Trunc(R));

  if G > Da then
    PInt8u(PtrComp(P) + This.Order.G)^ := Int8u(Trunc(Da))
  else
    PInt8u(PtrComp(P) + This.Order.G)^ := Int8u(Trunc(G));

  if B > Da then
    PInt8u(PtrComp(P) + This.Order.B)^ := Int8u(Trunc(Da))
  else
    PInt8u(PtrComp(P) + This.Order.B)^ := Int8u(Trunc(B));
end;

// Dca' = (Da - Dca) * Sa + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaInvert(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Da, Dr, Dg, Db, S1a: Integer;
begin
  Sa := (Sa * Cover + 255) shr 8;

  if Sa <> 0 then
  begin
    Da := PInt8u(PtrComp(P) + This.Order.A)^;
    Dr := ShrInt32((Da - PInt8u(PtrComp(P) + This.Order.R)^) * Sa +
      CAggBaseMask, CAggBaseShift);
    Dg := ShrInt32((Da - PInt8u(PtrComp(P) + This.Order.G)^) * Sa +
      CAggBaseMask, CAggBaseShift);
    Db := ShrInt32((Da - PInt8u(PtrComp(P) + This.Order.B)^) * Sa +
      CAggBaseMask, CAggBaseShift);
    S1a := CAggBaseMask - Sa;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(Dr + ShrInt32(PInt8u(PtrComp(P) + This.Order.R)^ * S1a +
      CAggBaseMask, CAggBaseShift));

    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(Dg + ShrInt32(PInt8u(PtrComp(P) + This.Order.G)^ * S1a +
      CAggBaseMask, CAggBaseShift));

    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(Db + ShrInt32(PInt8u(PtrComp(P) + This.Order.B)^ * S1a +
      CAggBaseMask, CAggBaseShift));

    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u(Sa + Da - ShrInt32(Sa * Da + CAggBaseMask, CAggBaseShift));
  end;
end;

// Dca' = (Da - Dca) * Sca + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaInvertRgb(This: TAggPixelFormatProcessor; P: PInt8u;
  Sr, Sg, Sb, Sa, Cover: Cardinal);
var
  Da, Dr, Dg, Db, S1a: Integer;
begin
  if Cover < 255 then
  begin
    Sr := ShrInt32(Sr * Cover + 255, 8);
    Sg := ShrInt32(Sg * Cover + 255, 8);
    Sb := ShrInt32(Sb * Cover + 255, 8);
    Sa := ShrInt32(Sa * Cover + 255, 8);
  end;

  if Sa <> 0 then
  begin
    Da := PInt8u(PtrComp(P) + This.Order.A)^;
    Dr := ShrInt32((Da - PInt8u(PtrComp(P) + This.Order.R)^) * Sr +
      CAggBaseMask, CAggBaseShift);
    Dg := ShrInt32((Da - PInt8u(PtrComp(P) + This.Order.G)^) * Sg +
      CAggBaseMask, CAggBaseShift);
    Db := ShrInt32((Da - PInt8u(PtrComp(P) + This.Order.B)^) * Sb +
      CAggBaseMask, CAggBaseShift);
    S1a := CAggBaseMask - Sa;

    PInt8u(PtrComp(P) + This.Order.R)^ :=
      Int8u(Dr + ShrInt32(PInt8u(PtrComp(P) + This.Order.R)^ * S1a +
      CAggBaseMask, CAggBaseShift));

    PInt8u(PtrComp(P) + This.Order.G)^ :=
      Int8u(Dg + ShrInt32(PInt8u(PtrComp(P) + This.Order.G)^ * S1a +
      CAggBaseMask, CAggBaseShift));

    PInt8u(PtrComp(P) + This.Order.B)^ :=
      Int8u(Db + ShrInt32(PInt8u(PtrComp(P) + This.Order.B)^ * S1a +
      CAggBaseMask, CAggBaseShift));

    PInt8u(PtrComp(P) + This.Order.A)^ :=
      Int8u(Sa + Da - ShrInt32(Sa * Da + CAggBaseMask, CAggBaseShift));
  end;
end;

const
  CBlendModeTableRgba: array [TAggBlendMode] of TAggFuncBlendPix = (coRgbaClear,
    coRgbaSrc, coRgbaDst, coRgbaSrcOver, coRgbaDstOver, coRgbaSrcIn,
    coRgbaDstIn, coRgbaSrcOut, coRgbaDstOut, coRgbaSrcATop, coRgbaDstATop,
    coRgbaXor, coRgbaPlus, coRgbaMinus, coRgbaMultiply, coRgbaScreen,
    coRgbaOverlay, coRgbaDarken, coRgbaLighten, coRgbaColorDodge,
    coRgbaColorBurn, coRgbaHardLight, coRgbaSoftLight, coRgbaDifference,
    coRgbaExclusion, coRgbaContrast, coRgbaInvert, coRgbaInvertRgb, nil);

procedure BlendModeAdaptorRgba(This: TAggPixelFormatProcessor;
  BlendMode: TAggBlendMode; P: PInt8u; Cr, Cg, Cb, Ca, Cover: Cardinal);
begin
  CBlendModeTableRgba[BlendMode](This, P, (Cr * Ca + CAggBaseMask) shr CAggBaseShift,
    (Cg * Ca + CAggBaseMask) shr CAggBaseShift, (Cb * Ca + CAggBaseMask) shr CAggBaseShift,
    Ca, Cover);
end;

procedure BlendModeAdaptorClipToDestinationRgbaPre(This: TAggPixelFormatProcessor;
  BlendMode: TAggBlendMode; P: PInt8u; Cr, Cg, Cb, Ca, Cover: Cardinal);
var
  Da: Cardinal;
begin
  Da := PInt8u(PtrComp(P) + This.Order.A)^;

  CBlendModeTableRgba[BlendMode](This, P, (Cr * Da + CAggBaseMask) shr CAggBaseShift,
    (Cg * Da + CAggBaseMask) shr CAggBaseShift, (Cb * Da + CAggBaseMask) shr CAggBaseShift,
    (Ca * Da + CAggBaseMask) shr CAggBaseShift, Cover);
end;

end.
