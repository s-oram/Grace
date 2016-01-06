unit AggPixelFormatRgbPacked;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012-2015                                                      //
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

uses
  AggBasics,
  AggPixelFormat,
  AggColor,
  AggRenderingBuffer;

procedure PixelFormatRgb555(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgb565(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatRgb555Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgb565Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatRgb555Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);
procedure PixelFormatRgb565Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);

implementation

function Fmt5x5Row(This: TAggPixelFormatProcessor; X, Y: Integer): TAggRowDataType;
begin
  Result.Initialize(X, This.Width - 1,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u)));
end;

{$I AggPixelFormatRgb555.inc}

procedure PixelFormatRgb555;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.PixWidth := 2;

  PixelFormatProcessor.CopyPixel := @Rgb555CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb555BlendPixel;

  PixelFormatProcessor.Pixel := @Rgb555Pixel;
  PixelFormatProcessor.Row := @Fmt5x5Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb555CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb555CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb555BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb555BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb555BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb555BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb555CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb555CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb555BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb555BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Rgb555CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb555BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb555BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb555BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := nil; // not implemented in aggPixelFormatRgb_packed.h
  PixelFormatProcessor.GammaDirApply := nil;
  PixelFormatProcessor.GammaInvApply := nil;
end;

{$I AggPixelFormatRgb565.inc}

procedure PixelFormatRgb565;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.PixWidth := 2;

  PixelFormatProcessor.CopyPixel := @Rgb565CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb565BlendPixel;

  PixelFormatProcessor.Pixel := @Rgb565Pixel;
  PixelFormatProcessor.Row := @Fmt5x5Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb565CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb565CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb565BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb565BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb565BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb565BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb565CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb565CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb565BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb565BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Rgb565CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb565BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb565BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb565BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := nil; // not implemented in aggPixelFormatRgb_packed.h
  PixelFormatProcessor.GammaDirApply := nil;
  PixelFormatProcessor.GammaInvApply := nil;
end;

{$I AggPixelFormatRgb555Pre.inc}

procedure PixelFormatRgb555Pre;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.PixWidth := 2;

  PixelFormatProcessor.CopyPixel := @Rgb555PreCopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb555PreBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb555PrePixel;
  PixelFormatProcessor.Row := @Fmt5x5Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb555PreCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb555PreCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb555PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb555PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb555PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb555PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb555PreCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb555PreCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb555PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb555PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Rgb555PreCopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb555PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb555PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb555PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := nil; // not implemented in aggPixelFormatRgb_packed.h
  PixelFormatProcessor.GammaDirApply := nil;
  PixelFormatProcessor.GammaInvApply := nil;
end;

{$I AggPixelFormatRgb565Pre.inc}

procedure PixelFormatRgb565Pre;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.PixWidth := 2;

  PixelFormatProcessor.CopyPixel := @Rgb565PreCopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb565PreBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb565PrePixel;
  PixelFormatProcessor.Row := @Fmt5x5Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb565PreCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb565PreCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb565PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb565PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb565PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb565PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb565PreCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb565PreCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb565PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb565PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Rgb565PreCopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb565PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb565PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb565PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := nil; // not implemented in aggPixelFormatRgb_packed.h
  PixelFormatProcessor.GammaDirApply := nil;
  PixelFormatProcessor.GammaInvApply := nil;
end;

{$I AggPixelFormatRgb555Gamma.inc}

procedure PixelFormatRgb555Gamma;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.PixWidth := 2;

  PixelFormatProcessor.Gamma := Gamma;

  PixelFormatProcessor.CopyPixel := @Rgb555GammaCopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb555GammaBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb555GammaPixel;
  PixelFormatProcessor.Row := @Fmt5x5Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb555GammaCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb555GammaCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb555GammaBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb555GammaBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb555GammaBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb555GammaBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb555GammaCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb555GammaCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb555GammaBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb555GammaBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Rgb555GammaCopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb555GammaBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb555GammaBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb555GammaBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := nil; // not implemented in aggPixelFormatRgb_packed.h
  PixelFormatProcessor.GammaDirApply := nil;
  PixelFormatProcessor.GammaInvApply := nil;
end;

{$I AggPixelFormatRgb565Gamma.inc}

procedure PixelFormatRgb565Gamma;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.PixWidth := 2;

  PixelFormatProcessor.Gamma := Gamma;

  PixelFormatProcessor.CopyPixel := @Rgb565GammaCopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb565GammaBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb565GammaPixel;
  PixelFormatProcessor.Row := @Fmt5x5Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb565GammaCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb565GammaCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb565GammaBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb565GammaBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb565GammaBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb565GammaBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb565GammaCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb565GammaCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb565GammaBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb565GammaBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Rgb565GammaCopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb565GammaBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb565GammaBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb565GammaBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := nil; // not implemented in aggPixelFormatRgb_packed.h
  PixelFormatProcessor.GammaDirApply := nil;
  PixelFormatProcessor.GammaInvApply := nil;
end;

end.
