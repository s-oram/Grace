unit AggPixelFormatTransposer;

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

uses
  AggBasics,
  AggPixelFormat,
  AggColor,
  AggRenderingBuffer;

type
  TAggPixelFormatProcessorTransposer = class(TAggPixelFormatProcessor)
  private
    FPixelFormats: TAggPixelFormatProcessor;
  protected
    function GetWidth: Cardinal; override;
    function GetHeight: Cardinal; override;
  public
    constructor Create(Source: TAggPixelFormatProcessor);

    procedure Attach(Source: TAggPixelFormatProcessor);
  end;

procedure PixelFormatTransposer(
  var PixelFormatProcessor: TAggPixelFormatProcessorTransposer;
  Source: TAggPixelFormatProcessor);

implementation


{ TAggPixelFormatProcessorTransposer }

constructor TAggPixelFormatProcessorTransposer.Create(
  Source: TAggPixelFormatProcessor);
begin
  inherited Create(nil);

  Attach(Source);
end;

procedure TAggPixelFormatProcessorTransposer.Attach(
  Source: TAggPixelFormatProcessor);
begin
  FPixelFormats := Source;
  FRenderingBuffer := Source.RenderingBuffer;

  FGamma := Source.Gamma;
  FApply := Source.Apply;
  FOrder := Source.Order;

  FBlendMode := Source.BlendMode;
  FStep := Source.Step;
  FOffset := Source.Offset;
  FPixWidth := Source.PixWidth;

  Blender := Source.Blender;
  Row := Source.Row;

  CopyFrom := Source.CopyFrom;
  BlendFrom := Source.BlendFrom;

  ForEachPixel := Source.ForEachPixel;
  GammaDirApply := Source.GammaDirApply;
  GammaInvApply := Source.GammaInvApply;
end;

function TAggPixelFormatProcessorTransposer.GetWidth: Cardinal;
begin
  Result := FPixelFormats.Height;
end;

function TAggPixelFormatProcessorTransposer.GetHeight: Cardinal;
begin
  Result := FPixelFormats.Width;
end;


procedure TransposerCopyPixel(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; C: PAggColor);
begin
  This.FPixelFormats.CopyPixel(This.FPixelFormats, Y, X, C);
end;

procedure TransposerBlendPixel(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; C: PAggColor; Cover: Int8u);
begin
  This.FPixelFormats.BlendPixel(This.FPixelFormats, Y, X, C, Cover);
end;

function TransposerPixel(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer): TAggColor;
begin
  Result := This.FPixelFormats.Pixel(This.FPixelFormats, Y, X);
end;

procedure TransposerCopyHorizontalLine(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
begin
  This.FPixelFormats.CopyVerticalLine(This.FPixelFormats, Y, X, Len, C);
end;

procedure TransposerCopyVerticalLine(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
begin
  This.FPixelFormats.CopyHorizontalLine(This.FPixelFormats, Y, X, Len, C);
end;

procedure TransposerBlendHorizontalLine(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
begin
  This.FPixelFormats.BlendVerticalLine(This.FPixelFormats, Y, X, Len, C, Cover);
end;

procedure TransposerBlendVerticalLine(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
begin
  This.FPixelFormats.BlendHorizontalLine(This.FPixelFormats, Y, X, Len, C, Cover);
end;

procedure TransposerBlendSolidHSpan(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
begin
  This.FPixelFormats.BlendSolidVSpan(This.FPixelFormats, Y, X, Len, C, Covers);
end;

procedure TransposerBlendSolidVSpan(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
begin
  This.FPixelFormats.BlendSolidHSpan(This.FPixelFormats, Y, X, Len, C, Covers);
end;

procedure TransposerCopyColorHSpan(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor);
begin
  This.FPixelFormats.CopyColorVSpan(This.FPixelFormats, Y, X, Len, Colors);
end;

procedure TransposerCopyColorVSpan(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor);
begin
  This.FPixelFormats.CopyColorHSpan(This.FPixelFormats, Y, X, Len, Colors);
end;

procedure TransposerBlendColorHSpan(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
begin
  This.FPixelFormats.BlendColorVSpan(This.FPixelFormats, Y, X, Len, Colors,
    Covers, Cover);
end;

procedure TransposerBlendColorVSpan(This: TAggPixelFormatProcessorTransposer;
  X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
begin
  This.FPixelFormats.BlendColorHSpan(This.FPixelFormats, Y, X, Len, Colors,
    Covers, Cover);
end;

procedure PixelFormatTransposer(
  var PixelFormatProcessor: TAggPixelFormatProcessorTransposer;
  Source: TAggPixelFormatProcessor);
begin
  PixelFormatProcessor := TAggPixelFormatProcessorTransposer.Create(Source);

  PixelFormatProcessor.CopyPixel := @TransposerCopyPixel;
  PixelFormatProcessor.BlendPixel := @TransposerBlendPixel;

  PixelFormatProcessor.Pixel := @TransposerPixel;

  PixelFormatProcessor.CopyHorizontalLine := @TransposerCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @TransposerCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @TransposerBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @TransposerBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @TransposerBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @TransposerBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @TransposerCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @TransposerCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @TransposerBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @TransposerBlendColorVSpan;
end;

end.
