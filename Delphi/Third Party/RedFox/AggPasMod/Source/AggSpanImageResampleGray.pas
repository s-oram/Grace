unit AggSpanImageResampleGray;

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
  AggColor,
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
  TAggSpanImageResampleGrayAffine = class(TAggSpanImageResampleAffine)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageResampleGray = class(TAggSpanImageResample)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageResampleGrayAffine }

constructor TAggSpanImageResampleGrayAffine.Create
  (Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageResampleGrayAffine.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);
end;

function TAggSpanImageResampleGrayAffine.Generate;
var
  Fg, SourceAlpha, Diameter, FilterSize, TotalWeight, WeightY, Weight: Integer;

  IniLowResX, IniHighResX: Integer;
  Radius, Max: TPointInteger;
  LowRes, HighRes: TPointInteger;

  BackV, BackA: Int8u;
  Span: PAggColor;
  ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.V;
  BackA := GetBackgroundColor.Rgba8.A;

  Span := Allocator.Span;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  Radius.X := ShrInt32(Diameter * FRadiusX, 1);
  Radius.Y := ShrInt32(Diameter * FRadiusY, 1);

  Max.X := SourceImage.Width - 1;
  Max.Y := SourceImage.Height - 1;

  WeightArray := Filter.WeightArray;

  repeat
    Interpolator.Coordinates(@X, @Y);

    Inc(X, FilterDeltaXInteger - Radius.X);
    Inc(Y, FilterDeltaYInteger - Radius.Y);

    Fg := CAggImageFilterSize div 2;
    SourceAlpha := Fg;

    LowRes.Y := ShrInt32(Y, CAggImageSubpixelShift);
    HighRes.Y := ShrInt32((CAggImageSubpixelMask - (Y and CAggImageSubpixelMask)) *
      FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    IniLowResX := ShrInt32(X, CAggImageSubpixelShift);
    IniHighResX := ShrInt32((CAggImageSubpixelMask - (X and CAggImageSubpixelMask)) *
      FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HighRes.Y * SizeOf(Int16))^;

      LowRes.X := IniLowResX;
      HighRes.X := IniHighResX;

      if (LowRes.Y >= 0) and (LowRes.Y <= Max.Y) then
      begin
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LowRes.Y)) +
          LowRes.X * SizeOf(Int8u));

        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          if (LowRes.X >= 0) and (LowRes.X <= Max.X) then
          begin
            Inc(Fg, ForeGroundPointer^ * Weight);
            Inc(SourceAlpha, CAggBaseMask * Weight);
          end
          else
          begin
            Inc(Fg, BackV * Weight);
            Inc(SourceAlpha, BackA * Weight);
          end;

          Inc(TotalWeight, Weight);
          Inc(HighRes.X, FRadiusXInv);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(LowRes.X);
        until HighRes.X >= FilterSize;
      end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          Inc(TotalWeight, Weight);
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackA * Weight);
          Inc(HighRes.X, FRadiusXInv);
        until HighRes.X >= FilterSize;

      Inc(HighRes.Y, FRadiusYInv);
      Inc(LowRes.Y);
    until HighRes.Y >= FilterSize;

    Fg := Fg div TotalWeight;
    SourceAlpha := SourceAlpha div TotalWeight;

    if Fg < 0 then
      Fg := 0;

    if SourceAlpha < 0 then
      SourceAlpha := 0;

    if SourceAlpha > CAggBaseMask then
      SourceAlpha := CAggBaseMask;

    if Fg > SourceAlpha then
      Fg := SourceAlpha;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(SourceAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageResampleGray }

constructor TAggSpanImageResampleGray.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageResampleGray.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);
end;

function TAggSpanImageResampleGray.Generate;
var
  Span: PAggColor;
  Fg, SourceAlpha, Diameter, TotalWeight: Integer;
  IniLowResX, IniHighResX: Integer;
  Weight, FilterSize, WeightY: Integer;

  Radius, Max, LowRes, HighRes, RadiusInv: TPointInteger;

  BackV, BackA: Int8u;
  WeightArray: PInt16;
  ForeGroundPointer: PInt8u;
begin
  Span := Allocator.Span;

  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.V;
  BackA := GetBackgroundColor.Rgba8.A;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  WeightArray := Filter.WeightArray;

  repeat
    RadiusInv.x := CAggImageSubpixelSize;
    RadiusInv.Y := CAggImageSubpixelSize;

    Interpolator.Coordinates(@X, @Y);
    Interpolator.LocalScale(@Radius.X, @Radius.Y);

    Radius.X := ShrInt32(Radius.X * FBlur.X, CAggImageSubpixelShift);
    Radius.Y := ShrInt32(Radius.Y * FBlur.Y, CAggImageSubpixelShift);

    if Radius.X < CAggImageSubpixelSize then
      Radius.X := CAggImageSubpixelSize
    else
    begin
      if Radius.X > CAggImageSubpixelSize * FScaleLimit then
        Radius.X := CAggImageSubpixelSize * FScaleLimit;

      RadiusInv.x := CAggImageSubpixelSize * CAggImageSubpixelSize div Radius.X;
    end;

    if Radius.Y < CAggImageSubpixelSize then
      Radius.Y := CAggImageSubpixelSize
    else
    begin
      if Radius.Y > CAggImageSubpixelSize * FScaleLimit then
        Radius.Y := CAggImageSubpixelSize * FScaleLimit;

      RadiusInv.Y := CAggImageSubpixelSize * CAggImageSubpixelSize div Radius.Y;
    end;

    Radius.X := ShrInt32(Diameter * Radius.X, 1);
    Radius.Y := ShrInt32(Diameter * Radius.Y, 1);

    Max.X := SourceImage.Width - 1;
    Max.Y := SourceImage.Height - 1;

    Inc(X, FilterDeltaXInteger - Radius.X);
    Inc(Y, FilterDeltaYInteger - Radius.Y);

    Fg := CAggImageFilterSize div 2;
    SourceAlpha := Fg;

    LowRes.Y := ShrInt32(Y, CAggImageSubpixelShift);
    HighRes.Y := ShrInt32((CAggImageSubpixelMask - (Y and CAggImageSubpixelMask)) *
      RadiusInv.Y, CAggImageSubpixelShift);

    TotalWeight := 0;

    IniLowResX := ShrInt32(X, CAggImageSubpixelShift);
    IniHighResX := ShrInt32((CAggImageSubpixelMask - (X and CAggImageSubpixelMask)) *
      RadiusInv.x, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HighRes.Y * SizeOf(Int16))^;

      LowRes.X := IniLowResX;
      HighRes.X := IniHighResX;

      if (LowRes.Y >= 0) and (LowRes.Y <= Max.Y) then
      begin
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LowRes.Y)) +
          LowRes.X * SizeOf(Int8u));

        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          if (LowRes.X >= 0) and (LowRes.X <= Max.X) then
          begin
            Inc(Fg, ForeGroundPointer^ * Weight);
            Inc(SourceAlpha, CAggBaseMask * Weight);
          end
          else
          begin
            Inc(Fg, BackV * Weight);
            Inc(SourceAlpha, BackA * Weight);
          end;

          Inc(TotalWeight, Weight);
          Inc(HighRes.X, RadiusInv.x);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(LowRes.X);
        until HighRes.X >= FilterSize;
      end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          Inc(TotalWeight, Weight);
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackA * Weight);
          Inc(HighRes.X, RadiusInv.x);
        until HighRes.X >= FilterSize;

      Inc(HighRes.Y, RadiusInv.Y);
      Inc(LowRes.Y);

    until HighRes.Y >= FilterSize;

    Fg := Fg div TotalWeight;
    SourceAlpha := SourceAlpha div TotalWeight;

    if Fg < 0 then
      Fg := 0;

    if SourceAlpha < 0 then
      SourceAlpha := 0;

    if SourceAlpha > CAggBaseMask then
      SourceAlpha := CAggBaseMask;

    if Fg > SourceAlpha then
      Fg := SourceAlpha;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(SourceAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
