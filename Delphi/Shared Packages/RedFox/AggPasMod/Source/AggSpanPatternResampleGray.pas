unit AggSpanPatternResampleGray;

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
  AggSpanPattern,
  AggSpanImageResample,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

const
  CAggBaseShift = AggColor.CAggBaseShift;
  CAggBaseMask = AggColor.CAggBaseMask;
  CAggDownscaleShift = CAggImageFilterShift;

type
  TAggSpanPatternResampleGrayAffine = class(TAggSpanImageResampleAffine)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      WX, WY: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
      WX, WY: TAggWrapMode); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternResampleGray = class(TAggSpanImageResample)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      WX, WY: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
      WX, WY: TAggWrapMode); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanPatternResampleGrayAffine }

constructor TAggSpanPatternResampleGrayAffine.Create
  (Alloc: TAggSpanAllocator; WX, WY: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternResampleGrayAffine.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  WX, WY: TAggWrapMode);
var
  Rgba: TAggColor;

begin
  Rgba.Clear;

  inherited Create(Alloc, Src, @Rgba, Interpolator, Filter);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

procedure TAggSpanPatternResampleGrayAffine.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternResampleGrayAffine.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Intr: TAggSpanInterpolator;
  Radius, Max, LoRes, HiRes: TPointInteger;
  Fg, Diameter, FilterSize, TotalWeight, WeightY, Weight: Integer;
  InitialLoResX, InitialHiResX: Integer;
  RowPointer, ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
begin
  Span := Allocator.Span;
  Intr := Interpolator;

  Intr.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  Radius.X := ShrInt32(Diameter * FRadiusX, 1);
  Radius.Y := ShrInt32(Diameter * FRadiusY, 1);

  Max.X := SourceImage.Width - 1;
  Max.Y := SourceImage.Height - 1;

  WeightArray := Filter.WeightArray;

  repeat
    Intr.Coordinates(@X, @Y);

    Inc(X, FilterDeltaXInteger - Radius.X);
    Inc(Y, FilterDeltaYInteger - Radius.Y);

    Fg := CAggImageFilterSize div 2;

    LoRes.Y := FWrapModeY.FuncOperator(ShrInt32(Y, CAggImageSubpixelShift));
    HiRes.Y := ShrInt32((CAggImageSubpixelMask -
      (Y and CAggImageSubpixelMask)) * FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(X, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (X and CAggImageSubpixelMask)) * FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      LoRes.X := FWrapModeX.FuncOperator(InitialLoResX);
      HiRes.X := InitialHiResX;

      RowPointer := SourceImage.Row(LoRes.Y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) +
          LoRes.X * SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

        Inc(Fg, ForeGroundPointer^ * Weight);
        Inc(TotalWeight, Weight);
        Inc(HiRes.X, FRadiusXInv);

        LoRes.X := FWrapModeX.IncOperator;

      until HiRes.X >= FilterSize;

      Inc(HiRes.Y, FRadiusYInv);

      LoRes.Y := FWrapModeY.IncOperator;

    until HiRes.Y >= FilterSize;

    Fg := Fg div TotalWeight;

    if Fg < 0 then
      Fg := 0;

    if Fg > CAggBaseMask then
      Fg := CAggBaseMask;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(CAggBaseMask);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Intr.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanPatternResampleGray }

constructor TAggSpanPatternResampleGray.Create(Alloc: TAggSpanAllocator;
  WX, WY: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternResampleGray.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; WX, WY: TAggWrapMode);
var
  Rgba: TAggColor;

begin
  Rgba.Clear;

  inherited Create(Alloc, Src, @Rgba, Interpolator, Filter);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

procedure TAggSpanPatternResampleGray.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternResampleGray.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Intr: TAggSpanInterpolator;

  Radius, Max, LoRes, HiRes: TPointInteger;
  Fg, Diameter, FilterSize, Rx, Ry, RxInv, RyInv, TotalWeight, InitialLoResX,
    InitialHiResX, WeightY, Weight: Integer;

  RowPointer, ForeGroundPointer: PInt8u;

  WeightArray: PInt16;
begin
  Span := Allocator.Span;
  Intr := Interpolator;

  Intr.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;
  WeightArray := Filter.WeightArray;

  repeat
    RxInv := CAggImageSubpixelSize;
    RyInv := CAggImageSubpixelSize;

    Intr.Coordinates(@X, @Y);
    Intr.LocalScale(@Rx, @Ry);

    Rx := ShrInt32(Rx * FBlur.X, CAggImageSubpixelShift);
    Ry := ShrInt32(Ry * FBlur.Y, CAggImageSubpixelShift);

    if Rx < CAggImageSubpixelSize then
      Rx := CAggImageSubpixelSize
    else
    begin
      if Rx > CAggImageSubpixelSize * FScaleLimit then
        Rx := CAggImageSubpixelSize * FScaleLimit;

      RxInv := CAggImageSubpixelSize * CAggImageSubpixelSize div Rx;
    end;

    if Ry < CAggImageSubpixelSize then
      Ry := CAggImageSubpixelSize
    else
    begin
      if Ry > CAggImageSubpixelSize * FScaleLimit then
        Ry := CAggImageSubpixelSize * FScaleLimit;

      RyInv := CAggImageSubpixelSize * CAggImageSubpixelSize div Ry;
    end;

    Radius.X := ShrInt32(Diameter * Rx, 1);
    Radius.Y := ShrInt32(Diameter * Ry, 1);

    Max.X := SourceImage.Width - 1;
    Max.Y := SourceImage.Height - 1;

    Inc(X, FilterDeltaXInteger - Radius.X);
    Inc(Y, FilterDeltaYInteger - Radius.Y);

    Fg := CAggImageFilterSize div 2;

    LoRes.Y := FWrapModeY.FuncOperator(ShrInt32(Y, CAggImageSubpixelShift));
    HiRes.Y := ShrInt32((CAggImageSubpixelMask -
      (Y and CAggImageSubpixelMask)) * RyInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(X, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (X and CAggImageSubpixelMask)) * RxInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      LoRes.X := FWrapModeX.FuncOperator(InitialLoResX);
      HiRes.X := InitialHiResX;

      RowPointer := SourceImage.Row(LoRes.Y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) +
          LoRes.X * SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

        Inc(Fg, ForeGroundPointer^ * Weight);
        Inc(TotalWeight, Weight);
        Inc(HiRes.X, RxInv);

        LoRes.X := FWrapModeX.IncOperator;
      until HiRes.X > FilterSize;

      Inc(HiRes.Y, RyInv);

      LoRes.Y := FWrapModeY.IncOperator;
    until HiRes.Y >= FilterSize;

    Fg := Fg div TotalWeight;

    if Fg < 0 then
      Fg := 0;

    if Fg > CAggBaseMask then
      Fg := CAggBaseMask;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(CAggBaseMask);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Intr.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
