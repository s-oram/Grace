unit AggSpanPatternRgba;

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
  AggColor,
  AggPixelFormat,
  AggPixelFormatRgba,
  AggSpanPattern,
  AggSpanAllocator,
  AggRenderingBuffer;

const
  CAggBaseShift = AggColor.CAggBaseShift;
  CAggBaseMask = AggColor.CAggBaseMask;

type
  TAggSpanPatternRgba = class(TAggSpanPatternBase)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
    FOrder: TAggOrder;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; WX, WY: TAggWrapMode;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      OffsetX, OffsetY: Cardinal; WX, WY: TAggWrapMode;
      Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanPatternRgba }

constructor TAggSpanPatternRgba.Create(Alloc: TAggSpanAllocator;
  WX, WY: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternRgba.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; OffsetX, OffsetY: Cardinal;
  WX, WY: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, OffsetX, OffsetY, 0);

  FOrder := Order;

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

procedure TAggSpanPatternRgba.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternRgba.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  RowPointer, P: PInt8u;
  Sx: Cardinal;
begin
  Span := Allocator.Span;
  Sx := FWrapModeX.FuncOperator(OffsetX + X);

  RowPointer := SourceImage.Row(FWrapModeY.FuncOperator(OffsetY + Y));

  repeat
    P := PInt8u(PtrComp(RowPointer) + (Sx shl 2) * SizeOf(Int8u));

    Span.Rgba8.R := PInt8u(PtrComp(P) + FOrder.R * SizeOf(Int8u))^;
    Span.Rgba8.G := PInt8u(PtrComp(P) + FOrder.G * SizeOf(Int8u))^;
    Span.Rgba8.B := PInt8u(PtrComp(P) + FOrder.B * SizeOf(Int8u))^;
    Span.Rgba8.A := PInt8u(PtrComp(P) + FOrder.A * SizeOf(Int8u))^;

    Sx := FWrapModeX.IncOperator;

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
