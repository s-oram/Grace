unit AggPixelFormatGray;

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

uses
  AggBasics,
  AggPixelFormat,
  AggColor,
  AggRenderingBuffer;

procedure PixelFormatGray8(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatGray8Bgr24r(var PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8Bgr24g(var PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8Bgr24b(var PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatGray8Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatGray8PreBgr24r(var PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8PreBgr24g(var PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8PreBgr24b(var PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);

implementation

function Fmt8Row(This: TAggPixelFormatProcessor; X, Y: Integer): TAggRowDataType;
begin
  Result.Initialize(X, This.Width - 1,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step + This.Offset));
end;

procedure GrayGammaDirApply(This: TAggPixelFormatProcessor; P: PInt8u);
begin
  P^ := This.Apply.Dir[P^];
end;

procedure GrayGammaInvApply(This: TAggPixelFormatProcessor; P: PInt8u);
begin
  P^ := This.Apply.Inv[P^];
end;

procedure GrayForEachPixel(This: TAggPixelFormatProcessor; F: TAggFuncApplyGamma);
var
  Y, Len: Cardinal;
  P: PInt8u;
begin
  Y := 0;

  while Y < This.Height do
  begin
    Len := This.Width;
    P := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + This.Offset);

    repeat
      F(This, P);

      Inc(PtrComp(P), This.Step);
      Dec(Len);

    until Len = 0;

    Inc(Y);
  end;
end;

{$I AggPixelFormatGray8.inc }

procedure PixelFormatGray8;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 1, 0);

  PixelFormatProcessor.PixWidth := 1;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8Bgr24r;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 2);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8Bgr24g;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 1);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8Bgr24b;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 0);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

{$I AggPixelFormatGray8Pre.inc }

procedure PixelFormatGray8Pre;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 1, 0);

  PixelFormatProcessor.PixWidth := 1;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8PreBgr24r;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 2);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8PreBgr24g;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 1);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8PreBgr24b;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 0);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

end.
