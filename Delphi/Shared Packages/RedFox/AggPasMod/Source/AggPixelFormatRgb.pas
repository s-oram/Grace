unit AggPixelFormatRgb;

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

procedure PixelFormatBgr24(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgb24(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatBgr24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgb24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatBgr24Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);
procedure PixelFormatRgb24Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);

implementation

function Fmt24Row(This: TAggPixelFormatProcessor; X, Y: Integer): TAggRowDataType;
begin
  Result.Initialize(X, This.Width - 1,
    PInt8u(PtrComp(This.GetRenderingBuffer.Row(Y)) + X * 3 * SizeOf(Int8u)));
end;

procedure Fmt24CopyFrom(This: TAggPixelFormatProcessor; From: TAggRenderingBuffer;
  Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt8u(PtrComp(From.Row(Ysrc)) + Xsrc * 3 * SizeOf(Int8u))^,
    PInt8u(PtrComp(This.GetRenderingBuffer.Row(Ydst)) + Xdst * 3 * SizeOf(Int8u))^,
    SizeOf(Int8u) * 3 * Len);
end;

procedure Order24GammaDirApply(This: TAggPixelFormatProcessor; P: PInt8u);
begin
  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(This.Apply.Dir[PInt8u(PtrComp(P) + This.Order.R)^]);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(This.Apply.Dir[PInt8u(PtrComp(P) + This.Order.G)^]);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(This.Apply.Dir[PInt8u(PtrComp(P) + This.Order.B)^]);
end;

procedure Order24GammaInvApply(This: TAggPixelFormatProcessor; P: PInt8u);
begin
  PInt8u(PtrComp(P) + This.Order.R)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(P) + This.Order.R)^]);
  PInt8u(PtrComp(P) + This.Order.G)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(P) + This.Order.G)^]);
  PInt8u(PtrComp(P) + This.Order.B)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(P) + This.Order.B)^]);
end;

procedure Order24ForEachPixel(This: TAggPixelFormatProcessor; F: TAggFuncApplyGamma);
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

      Inc(PtrComp(P), 3);
      Dec(Len);
    until Len = 0;

    Inc(Y);
  end;
end;

{$I AggPixelFormatBgr24.inc}

procedure PixelFormatBgr24(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgr;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Bgr24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgr24BlendPixel;

  PixelFormatProcessor.Pixel := @Bgr24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgr24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgr24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgr24BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgr24BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgr24BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgr24BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgr24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgr24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgr24BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgr24BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgr24BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgr24BlendFromColor;
  PixelFormatProcessor.BlendFromLut := @Bgr24BlendFromLut;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$I AggPixelFormatRgb24.inc}

procedure PixelFormatRgb24(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgb;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Rgb24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb24BlendPixel;

  PixelFormatProcessor.Pixel := @Rgb24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb24BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb24BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb24BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb24BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb24BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb24BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb24BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb24BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb24BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$I AggPixelFormatBgr24Pre.inc}

procedure PixelFormatBgr24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgr;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Bgr24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgr24PreBlendPixel;

  PixelFormatProcessor.Pixel := @Bgr24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgr24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgr24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgr24PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgr24PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgr24PreBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgr24PreBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgr24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgr24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgr24PreBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgr24PreBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgr24PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgr24PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgr24PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$I AggPixelFormatRgb24Pre.inc}

procedure PixelFormatRgb24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgb;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Rgb24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb24PreBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb24PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb24PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb24PreBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb24PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb24PreBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb24PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb24PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb24PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb24PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$I AggPixelFormatBgr24Gamma.inc}

procedure PixelFormatBgr24Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);
begin
  Assert(Assigned(Gamma));

  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgr;
  PixelFormatProcessor.Gamma := Gamma;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Bgr24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgr24GammaBlendPixel;

  PixelFormatProcessor.Pixel := @Bgr24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgr24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgr24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgr24GammaBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgr24GammaBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgr24GammaBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgr24GammaBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgr24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgr24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgr24GammaBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgr24GammaBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgr24GammaBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgr24GammaBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgr24GammaBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$I AggPixelFormatRgb24Gamma.inc}

procedure PixelFormatRgb24Gamma;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgb;
  PixelFormatProcessor.Gamma := Gamma;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Rgb24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb24GammaBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb24GammaBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb24GammaBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb24GammaBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb24GammaBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb24GammaBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb24GammaBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb24GammaBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb24GammaBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb24GammaBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

end.
