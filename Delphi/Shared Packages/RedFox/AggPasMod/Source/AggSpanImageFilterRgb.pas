unit AggSpanImageFilterRgb;

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
  AggColor,
  AggImageFilters,
  AggSpanImageFilter,
  AggSpanAllocator,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer;

const
  CAggBaseShift = AggColor.CAggBaseShift;
  CAggBaseMask = AggColor.CAggBaseMask;

type
  TAggSpanImageFilterRgbNN = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator;
      Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgbBilinear = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator;
      Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgb2x2 = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgb = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageFilterRgbNN }

constructor TAggSpanImageFilterRgbNN.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Inter, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbNN.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0..2] of Cardinal;
  SrcAlpha: Cardinal;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max: TPointInteger;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  Max.X := SourceImage.Width - 1;
  Max.Y := SourceImage.Height - 1;

  repeat
    Interpolator.Coordinates(@X, @Y);

    X := ShrInt32(X, CAggImageSubpixelShift);
    Y := ShrInt32(Y, CAggImageSubpixelShift);

    if (X >= 0) and (Y >= 0) and (X <= Max.X) and (Y <= Max.Y) then
    begin
      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(Y)) + (X + X + X) *
        SizeOf(Int8u));

      Fg[0] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Fg[1] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Fg[2] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      SrcAlpha := CAggBaseMask;

    end
    else
    begin
      Fg[FOrder.R] := GetBackgroundColor.Rgba8.R;
      Fg[FOrder.G] := GetBackgroundColor.Rgba8.G;
      Fg[FOrder.B] := GetBackgroundColor.Rgba8.B;
      SrcAlpha := GetBackgroundColor.Rgba8.A;
    end;

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.G := Int8u(Fg[FOrder.G]);
    Span.Rgba8.B := Int8u(Fg[FOrder.B]);
    Span.Rgba8.A := Int8u(SrcAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterRgbBilinear }

constructor TAggSpanImageFilterRgbBilinear.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbBilinear.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Inter, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbBilinear.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0..2] of Cardinal;
  SrcAlpha, Weight: Cardinal;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Backup := GetBackgroundColor.Rgba8;

  Span := Allocator.Span;

  Max.X := SourceImage.Width - 1;
  Max.Y := SourceImage.Height - 1;

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X < Max.X) and (LoRes.Y < Max.Y) then
    begin
      Fg[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
      Fg[1] := Fg[0];
      Fg[2] := Fg[0];

      HiRes.X := HiRes.X and CAggImageSubpixelMask;
      HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
        (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));
      Weight := (CAggImageSubpixelSize - HiRes.X) * (CAggImageSubpixelSize - HiRes.Y);

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := HiRes.X * (CAggImageSubpixelSize - HiRes.Y);

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGroundPointer := SourceImage.NextRow
        (PInt8u(PtrComp(ForeGroundPointer) - 6 * SizeOf(Int8u)));
      Weight := (CAggImageSubpixelSize - HiRes.X) * HiRes.Y;

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := HiRes.X * HiRes.Y;

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Fg[0] := Fg[0] shr (CAggImageSubpixelShift * 2);
      Fg[1] := Fg[1] shr (CAggImageSubpixelShift * 2);
      Fg[2] := Fg[2] shr (CAggImageSubpixelShift * 2);

      SrcAlpha := CAggBaseMask;
    end
    else
    begin
      if (LoRes.X < -1) or (LoRes.Y < -1) or (LoRes.X > Max.X) or (LoRes.Y > Max.Y) then
      begin
        Fg[FOrder.R] := Backup.R;
        Fg[FOrder.G] := Backup.G;
        Fg[FOrder.B] := Backup.B;
        SrcAlpha := Backup.A;
      end
      else
      begin
        Fg[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
        Fg[1] := Fg[0];
        Fg[2] := Fg[0];
        SrcAlpha := Fg[0];

        HiRes.X := HiRes.X and CAggImageSubpixelMask;
        HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

        Weight := (CAggImageSubpixelSize - HiRes.X) * (CAggImageSubpixelSize - HiRes.Y);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := HiRes.X * (CAggImageSubpixelSize - HiRes.Y);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Dec(LoRes.X);
        Inc(LoRes.Y);

        Weight := (CAggImageSubpixelSize - HiRes.X) * HiRes.Y;

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := HiRes.X * HiRes.Y;

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Fg[0] := Fg[0] shr (CAggImageSubpixelShift * 2);
        Fg[1] := Fg[0] shr (CAggImageSubpixelShift * 2);
        Fg[2] := Fg[0] shr (CAggImageSubpixelShift * 2);

        SrcAlpha := SrcAlpha shr (CAggImageSubpixelShift * 2);
      end;
    end;

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.G := Int8u(Fg[FOrder.G]);
    Span.Rgba8.B := Int8u(Fg[FOrder.B]);
    Span.Rgba8.A := Int8u(SrcAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterRgb2x2 }

constructor TAggSpanImageFilterRgb2x2.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgb2x2.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Inter, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgb2x2.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0..2] of Cardinal;
  SrcAlpha, Weight: Cardinal;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  WeightArray: PInt16;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Backup := GetBackgroundColor.Rgba8;

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ((Filter.Diameter div 2 - 1) shl CAggImageSubpixelShift) * SizeOf(Int16));

  Max := PointInteger(SourceImage.Width - 1, SourceImage.Height - 1);

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X < Max.X) and (LoRes.Y < Max.Y) then
    begin
      Fg[0] := CAggImageFilterSize div 2;
      Fg[1] := Fg[0];
      Fg[2] := Fg[0];

      HiRes.X := HiRes.X and CAggImageSubpixelMask;
      HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
        (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));
      Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
        (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
        PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
        SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16)
        )^ * PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
        SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGroundPointer := SourceImage.NextRow
        (PInt8u(PtrComp(ForeGroundPointer) - 6 * SizeOf(Int8u)));
      Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
        (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
        PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
        CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16)
        )^ * PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
        CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(Fg[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(Fg[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Fg[0] := Fg[0] shr CAggImageFilterShift;
      Fg[1] := Fg[1] shr CAggImageFilterShift;
      Fg[2] := Fg[2] shr CAggImageFilterShift;

      SrcAlpha := CAggBaseMask;

      if Fg[0] > CAggBaseMask then
        Fg[0] := CAggBaseMask;

      if Fg[1] > CAggBaseMask then
        Fg[1] := CAggBaseMask;

      if Fg[2] > CAggBaseMask then
        Fg[2] := CAggBaseMask;
    end
    else
    begin
      if (LoRes.X < -1) or (LoRes.Y < -1) or (LoRes.X > Max.X) or (LoRes.Y > Max.Y) then
      begin
        Fg[FOrder.R] := Backup.R;
        Fg[FOrder.G] := Backup.G;
        Fg[FOrder.B] := Backup.B;
        SrcAlpha := Backup.A;
      end
      else
      begin
        Fg[0] := CAggImageFilterSize div 2;
        Fg[1] := Fg[0];
        Fg[2] := Fg[0];
        SrcAlpha := Fg[0];

        HiRes.X := HiRes.X and CAggImageSubpixelMask;
        HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
          (HiRes.Y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Dec(LoRes.X);
        Inc(LoRes.Y);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.Y *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X + LoRes.X + LoRes.X) * SizeOf(Int8u));

          Inc(Fg[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(SrcAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg[FOrder.R], Backup.R * Weight);
          Inc(Fg[FOrder.G], Backup.G * Weight);
          Inc(Fg[FOrder.B], Backup.B * Weight);

          Inc(SrcAlpha, Backup.A * Weight);
        end;

        Fg[0] := Fg[0] shr CAggImageFilterShift;
        Fg[1] := Fg[0] shr CAggImageFilterShift;
        Fg[2] := Fg[0] shr CAggImageFilterShift;

        SrcAlpha := SrcAlpha shr CAggImageFilterShift;

        if SrcAlpha > CAggBaseMask then
          SrcAlpha := CAggBaseMask;

        if Fg[0] > SrcAlpha then
          Fg[0] := SrcAlpha;

        if Fg[1] > SrcAlpha then
          Fg[1] := SrcAlpha;

        if Fg[2] > SrcAlpha then
          Fg[2] := SrcAlpha;
      end;
    end;

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.G := Int8u(Fg[FOrder.G]);
    Span.Rgba8.B := Int8u(Fg[FOrder.B]);
    Span.Rgba8.A := Int8u(SrcAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterRgb }

constructor TAggSpanImageFilterRgb.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgb.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Inter, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgb.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Fg: array [0..2] of Integer;
  Max, Max2: TPointInteger;
  SrcAlpha, Start, Start1, CountX, WeightY, Weight, FractX: Integer;
  HiRes, LoRes: TPointInteger;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Diameter, StepBack, CountY: Cardinal;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Backup := GetBackgroundColor.Rgba8;

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  Start1 := Start - 1;
  WeightArray := Filter.WeightArray;

  StepBack := Diameter * 3;

  Span := Allocator.Span;

  Max.X := SourceImage.Width + Start - 2;
  Max.Y := SourceImage.Height + Start - 2;

  Max2.X := SourceImage.Width - Start - 1;
  Max2.Y := SourceImage.Height - Start - 1;

  repeat
    Interpolator.Coordinates(@X, @Y);

    Dec(X, FilterDeltaXInteger);
    Dec(Y, FilterDeltaYInteger);

    HiRes.X := X;
    HiRes.Y := Y;

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];

    FractX := HiRes.X and CAggImageSubpixelMask;
    CountY := Diameter;

    if (LoRes.X >= -Start) and (LoRes.Y >= -Start) and (LoRes.X <= Max.X) and
      (LoRes.Y <= Max.Y) then
    begin
      HiRes.Y := CAggImageSubpixelMask - (HiRes.Y and CAggImageSubpixelMask);
      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y + Start)) +
        (LoRes.X + Start) * 3 * SizeOf(Int8u));

      repeat
        CountX := Diameter;
        WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;
        HiRes.X := CAggImageSubpixelMask - FractX;

        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

          Inc(Fg[0], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[1], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(Fg[2], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(HiRes.X, CAggImageSubpixelSize);
          Dec(CountX);
        until CountX = 0;

        Inc(HiRes.Y, CAggImageSubpixelSize);

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - StepBack * SizeOf(Int8u)));

        Dec(CountY);
      until CountY = 0;

      Fg[0] := ShrInt32(Fg[0], CAggImageFilterShift);
      Fg[1] := ShrInt32(Fg[1], CAggImageFilterShift);
      Fg[2] := ShrInt32(Fg[2], CAggImageFilterShift);

      if Fg[0] < 0 then
        Fg[0] := 0;

      if Fg[1] < 0 then
        Fg[1] := 0;

      if Fg[2] < 0 then
        Fg[2] := 0;

      if Fg[0] > CAggBaseMask then
        Fg[0] := CAggBaseMask;

      if Fg[1] > CAggBaseMask then
        Fg[1] := CAggBaseMask;

      if Fg[2] > CAggBaseMask then
        Fg[2] := CAggBaseMask;

      SrcAlpha := CAggBaseMask;
    end
    else
    begin
      if (LoRes.X < Start1) or (LoRes.Y < Start1) or (LoRes.X > Max2.X) or
        (LoRes.Y > Max2.Y) then
      begin
        Fg[FOrder.R] := Backup.R;
        Fg[FOrder.G] := Backup.G;
        Fg[FOrder.B] := Backup.B;
        SrcAlpha := Backup.A;
      end
      else
      begin
        SrcAlpha := CAggImageFilterSize div 2;

        LoRes.Y := ShrInt32(Y, CAggImageSubpixelShift) + Start;
        HiRes.Y := CAggImageSubpixelMask - (HiRes.Y and CAggImageSubpixelMask);

        repeat
          CountX := Diameter;
          WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

          LoRes.X := ShrInt32(X, CAggImageSubpixelShift) + Start;
          HiRes.X := CAggImageSubpixelMask - FractX;

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) +
              HiRes.X * SizeOf(Int16))^ + CAggImageFilterSize div 2,
              CAggImageFilterShift);

            if (LoRes.X >= 0) and (LoRes.Y >= 0) and
              (LoRes.X < Trunc(SourceImage.Width)) and
              (LoRes.Y < Trunc(SourceImage.Height)) then
            begin
              ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
                LoRes.X * 3 * SizeOf(Int8u));

              Inc(Fg[0], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              Inc(Fg[1], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              Inc(Fg[2], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

              Inc(SrcAlpha, CAggBaseMask * Weight);
            end
            else
            begin
              Inc(Fg[FOrder.R], Backup.R * Weight);
              Inc(Fg[FOrder.G], Backup.G * Weight);
              Inc(Fg[FOrder.B], Backup.B * Weight);

              Inc(SrcAlpha, Backup.A * Weight);
            end;

            Inc(HiRes.X, CAggImageSubpixelSize);
            Inc(LoRes.X);
            Dec(CountX);
          until CountX = 0;

          Inc(HiRes.Y, CAggImageSubpixelSize);
          Inc(LoRes.Y);
          Dec(CountY);

        until CountY = 0;

        Fg[0] := ShrInt32(Fg[0], CAggImageFilterShift);
        Fg[1] := ShrInt32(Fg[1], CAggImageFilterShift);
        Fg[2] := ShrInt32(Fg[2], CAggImageFilterShift);

        SrcAlpha := ShrInt32(SrcAlpha, CAggImageFilterShift);

        if Fg[0] < 0 then
          Fg[0] := 0;

        if Fg[1] < 0 then
          Fg[1] := 0;

        if Fg[2] < 0 then
          Fg[2] := 0;

        if SrcAlpha < 0 then
          SrcAlpha := 0;

        if SrcAlpha > CAggBaseMask then
          SrcAlpha := CAggBaseMask;

        if Fg[0] > SrcAlpha then
          Fg[0] := SrcAlpha;

        if Fg[1] > SrcAlpha then
          Fg[1] := SrcAlpha;

        if Fg[2] > SrcAlpha then
          Fg[2] := SrcAlpha;
      end;
    end;

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.G := Int8u(Fg[FOrder.G]);
    Span.Rgba8.B := Int8u(Fg[FOrder.B]);
    Span.Rgba8.A := Int8u(SrcAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
