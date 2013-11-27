unit AggPixelFormat;

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
//                                                                            //
//  This unit is originaly not the part of the AGG library.                   //
//  aggPixelFormat unit & pixelformats object substitutes the templetized     //
//  concept of a pixel polymorphism in c++.                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$Q-}
{$R-}

uses
  AggBasics,
  AggRenderingBuffer,
  AggColor;

type
  TAggPixelFormatProcessor = class;

  TAggBlendMode = (bmClear, bmSource, bmDestination, bmSourceOver,
    bmDestinationOver, bmSourceIn, bmDestinationIn, bmSourceOut,
    bmDestinationOut, bmSourceATop, bmDestinationATop, bmXor, bmPlus, bmMinus,
    bmMultiply, bmScreen, bmOverlay, bmDarken, bmLighten, bmColorDodge,
    bmColorBurn, bmHardLight, bmSoftLight, bmDifference, bmExclusion,
    bmContrast, bmInvert, bmInvertRgb, bmAlpha);

  TAggFuncBlender = procedure(This: TAggPixelFormatProcessor;
    Op: TAggBlendMode; P: PInt8u; Cr, Cg, Cb, Ca, Cover: Cardinal);
  TAggFuncBlendPix = procedure(This: TAggPixelFormatProcessor; P: PInt8u;
    Cr, Cg, Cb, Alpha, Cover: Cardinal);

  TAggFuncCopyPixel = procedure(This: TAggPixelFormatProcessor; X, Y: Integer;
    C: PAggColor);
  TAggFuncBlendPixel = procedure(This: TAggPixelFormatProcessor; X, Y: Integer;
    C: PAggColor; Cover: Int8u);

  TAggFuncPixel = function(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
  TAggFuncRow = function(This: TAggPixelFormatProcessor; X, Y: Integer): TAggRowDataType;

  TAggFuncCopyHorizontalLine = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; C: PAggColor);
  TAggFuncCopyVerticalLine = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; C: PAggColor);

  TAggFuncBlendHorizontalLine = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
  TAggFuncBlendVerticalLine = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);

  TAggFuncBlendSolidHorizontalSpan = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
  TAggFuncBlendsolidVerticalSpan = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);

  TAggFuncCopyColorHorizontalSpan = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; Colors: PAggColor);
  TAggFuncCopyColorVerticalSpan = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; Colors: PAggColor);

  TAggFuncBlendColorHorizontalSpan = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
    Cover: Int8u);
  TAggFuncBlendColorVerticalSpan = procedure(This: TAggPixelFormatProcessor;
    X, Y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
    Cover: Int8u);

  TAggFuncCopyFrom = procedure(This: TAggPixelFormatProcessor;
    From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
  TAggFuncBlendFrom = procedure(This: TAggPixelFormatProcessor;
    From: TAggPixelFormatProcessor; SourcePtr: PInt8u; Xdst, Ydst, Xsrc,
    Ysrc: Integer; Len: Cardinal; Cover: Int8u);

  TAggFuncBlendFromColor = procedure(This: TAggPixelFormatProcessor;
    From: TAggPixelFormatProcessor; Color: PAggColor; Xdst, Ydst, Xsrc,
    Ysrc: Integer; Len: Cardinal; Cover: Int8u);
  TAggFuncBlendFromLUT = procedure(This: TAggPixelFormatProcessor;
    From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
    Ysrc: Integer; Len: Cardinal; Cover: Int8u);

  TAggFuncApplyGamma = procedure(This: TAggPixelFormatProcessor; P: PInt8u);
  TAggFuncForEachPixel = procedure(This: TAggPixelFormatProcessor;
    F: TAggFuncApplyGamma);

  TAggPixelFormatProcessor = class
  protected
    FGamma, FApply: TAggGamma;
    FOrder: TAggOrder;
    FStep, FOffset, FPixWidth: Cardinal;
    FBlendMode: TAggBlendMode;

    FRenderingBuffer: TAggRenderingBuffer;
    function GetStride: Integer;
  protected
    function GetWidth: Cardinal; virtual;
    function GetHeight: Cardinal; virtual;
  public
    Blender: TAggFuncBlender;

    CopyPixel: TAggFuncCopyPixel;
    BlendPixel: TAggFuncBlendPixel;

    Pixel: TAggFuncPixel;
    Row: TAggFuncRow;

    CopyHorizontalLine: TAggFuncCopyHorizontalLine;
    CopyVerticalLine: TAggFuncCopyVerticalLine;

    BlendHorizontalLine: TAggFuncBlendHorizontalLine;
    BlendVerticalLine: TAggFuncBlendVerticalLine;

    BlendSolidHSpan: TAggFuncBlendSolidHorizontalSpan;
    BlendSolidVSpan: TAggFuncBlendsolidVerticalSpan;

    CopyColorHSpan: TAggFuncCopyColorHorizontalSpan;
    CopyColorVSpan: TAggFuncCopyColorVerticalSpan;

    BlendColorHSpan: TAggFuncBlendColorHorizontalSpan;
    BlendColorVSpan: TAggFuncBlendColorVerticalSpan;

    CopyFrom: TAggFuncCopyFrom;
    BlendFrom: TAggFuncBlendFrom;

    BlendFromColor: TAggFuncBlendFromColor;
    BlendFromLUT: TAggFuncBlendFromLUT;

    ForEachPixel: TAggFuncForEachPixel;
    GammaDirApply, GammaInvApply: TAggFuncApplyGamma;

    PixelPreMultiply, PixelDeMultiply: TAggFuncApplyGamma;

    constructor Create(Rb: TAggRenderingBuffer; St: Cardinal = 1;
      Off: Cardinal = 0);

    function Attach(Pixf: TAggPixelFormatProcessor; X1, Y1, X2, Y2: Integer): Boolean;
    function GetPixelPointer(X, Y: Integer): PInt8u;
    function GetRowPointer(Y: Integer): PInt8u;

    procedure ApplyGammaDir(Gamma: TAggGamma; Order: TAggOrder);
    procedure ApplyGammaInv(Gamma: TAggGamma; Order: TAggOrder);

    function GetRenderingBuffer: TAggRenderingBuffer;

    procedure PreMultiply;
    procedure DeMultiply;


    property RenderingBuffer: TAggRenderingBuffer read FRenderingBuffer;
    property Gamma: TAggGamma read FGamma write FGamma;
    property Apply: TAggGamma read FApply;
    property Order: TAggOrder read FOrder write FOrder;

    property BlendMode: TAggBlendMode read FBlendMode write FBlendMode;
    property Step: Cardinal read FStep;
    property Offset: Cardinal read FOffset;
    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetHeight;
    property Stride: Integer read GetStride;
    property PixWidth: Cardinal read FPixWidth write FPixWidth;
  end;
  TAggPixelFormatProcessorClass = class of TAggPixelFormatProcessor;

  DefinePixelFormat = procedure(out Pixf: TAggPixelFormatProcessor; Rb: TAggRenderingBuffer);
  DefinePixelFormatGamma = procedure(out Pixf: TAggPixelFormatProcessor;
    Rb: TAggRenderingBuffer; Gamma: TAggGamma);
  DefinePixelFormatBlender = procedure(out Pixf: TAggPixelFormatProcessor;
    Rb: TAggRenderingBuffer; Bl: TAggFuncBlender; Order: TAggOrder);

procedure PixelFormatUndefined(var Pixf: TAggPixelFormatProcessor);

implementation


{ TAggPixelFormatProcessor }

constructor TAggPixelFormatProcessor.Create(Rb: TAggRenderingBuffer; St: Cardinal = 1;
  Off: Cardinal = 0);
begin
  FRenderingBuffer := Rb;
  FGamma := nil;
  FApply := nil;
  FOrder := CAggOrderBgra;

  FBlendMode := bmSourceOver;
  FStep := St;
  FOffset := Off;

  FPixWidth := 0;

  Blender := nil;

  CopyPixel := nil;
  BlendPixel := nil;

  Pixel := nil;
  Row := nil;

  CopyHorizontalLine := nil;
  CopyVerticalLine := nil;

  BlendHorizontalLine := nil;
  BlendVerticalLine := nil;

  BlendSolidHSpan := nil;
  BlendSolidVSpan := nil;

  CopyColorHSpan := nil;
  CopyColorVSpan := nil;

  BlendColorHSpan := nil;
  BlendColorVSpan := nil;

  CopyFrom := nil;
  BlendFrom := nil;

  BlendFromColor := nil;
  BlendFromLUT := nil;

  ForEachPixel := nil;
  GammaDirApply := nil;
  GammaInvApply := nil;

  PixelPreMultiply := nil;
  PixelDeMultiply := nil;
end;

function TAggPixelFormatProcessor.Attach(Pixf: TAggPixelFormatProcessor;
  X1, Y1, X2, Y2: Integer): Boolean;
var
  R, C: TRectInteger;
  Stride, Y: Integer;
begin
  R := RectInteger(X1, Y1, X2, Y2);
  C := RectInteger(0, 0, Pixf.Width - 1, Pixf.Height - 1);

  if R.Clip(C) then
  begin
    Stride := Pixf.FRenderingBuffer.Stride;

    if Stride < 0 then
      Y := R.Y2
    else
      Y := R.Y1;

    FRenderingBuffer.Attach(Pixf.GetPixelPointer(R.X1, Y), (R.X2 - R.X1) + 1,
      (R.Y2 - R.Y1) + 1, Stride);

    Result := True;

  end
  else
    Result := False;
end;

function TAggPixelFormatProcessor.GetPixelPointer(X, Y: Integer): PInt8u;
begin
  Result := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + X * FPixWidth + FOffset);
end;

function TAggPixelFormatProcessor.GetRowPointer(Y: Integer): PInt8u;
begin
  Result := FRenderingBuffer.Row(Y);
end;

function TAggPixelFormatProcessor.GetWidth;
begin
  Result := FRenderingBuffer.Width;
end;

function TAggPixelFormatProcessor.GetHeight;
begin
  Result := FRenderingBuffer.Height;
end;

function TAggPixelFormatProcessor.GetStride;
begin
  Result := FRenderingBuffer.Stride;
end;

function TAggPixelFormatProcessor.GetRenderingBuffer: TAggRenderingBuffer;
begin
  Result := FRenderingBuffer;
end;

procedure TAggPixelFormatProcessor.ApplyGammaDir(Gamma: TAggGamma; Order: TAggOrder);
begin
  FApply := Gamma;
  FOrder := Order;

  ForEachPixel(Self, @GammaDirApply);
end;

procedure TAggPixelFormatProcessor.ApplyGammaInv(Gamma: TAggGamma; Order: TAggOrder);
begin
  FApply := Gamma;
  FOrder := Order;

  ForEachPixel(Self, @GammaInvApply);
end;

procedure TAggPixelFormatProcessor.PreMultiply;
begin
  ForEachPixel(@Self, @PixelPreMultiply);
end;

procedure TAggPixelFormatProcessor.DeMultiply;
begin
  ForEachPixel(@Self, @PixelDeMultiply);
end;

procedure PixelFormatUndefined(var Pixf: TAggPixelFormatProcessor);
begin
  Pixf := TAggPixelFormatProcessor.Create(nil);

  Pixf.CopyPixel := nil;
  Pixf.BlendPixel := nil;

  Pixf.Pixel := nil;
  Pixf.Row := nil;

  Pixf.CopyHorizontalLine := nil;
  Pixf.CopyVerticalLine := nil;

  Pixf.BlendHorizontalLine := nil;
  Pixf.BlendVerticalLine := nil;

  Pixf.BlendSolidHSpan := nil;
  Pixf.BlendSolidVSpan := nil;

  Pixf.CopyColorHSpan := nil;
  Pixf.CopyColorVSpan := nil;

  Pixf.BlendColorHSpan := nil;
  Pixf.BlendColorVSpan := nil;

  Pixf.CopyFrom := nil;
  Pixf.BlendFrom := nil;

  Pixf.BlendFromColor := nil;
  Pixf.BlendFromLUT := nil;

  Pixf.ForEachPixel := nil;
  Pixf.GammaDirApply := nil;
  Pixf.GammaInvApply := nil;
end;

end.
