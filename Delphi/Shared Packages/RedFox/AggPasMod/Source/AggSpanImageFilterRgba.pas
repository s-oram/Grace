unit AggSpanImageFilterRgba;

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
  TAggSpanImageFilterRgbaNN = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgbaBilinear = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgba2x2 = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgba = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageFilterRgbaNN }

constructor TAggSpanImageFilterRgbaNN.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbaNN.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbaNN.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0..3] of Cardinal;
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
      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(Y)) + (X shl 2) *
        SizeOf(Int8u));

      ForeGround[0] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      ForeGround[1] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      ForeGround[2] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      ForeGround[3] := ForeGroundPointer^;
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    end
    else
    begin
      ForeGround[FOrder.R] := GetBackgroundColor.Rgba8.R;
      ForeGround[FOrder.G] := GetBackgroundColor.Rgba8.G;
      ForeGround[FOrder.B] := GetBackgroundColor.Rgba8.B;
      ForeGround[FOrder.A] := GetBackgroundColor.Rgba8.A;
    end;

    Span.Rgba8.R := ForeGround[FOrder.R];
    Span.Rgba8.G := ForeGround[FOrder.G];
    Span.Rgba8.B := ForeGround[FOrder.B];
    Span.Rgba8.A := ForeGround[FOrder.A];

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterRgbaBilinear }

constructor TAggSpanImageFilterRgbaBilinear.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbaBilinear.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbaBilinear.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0..3] of Cardinal;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max, HiRes, LoRes: TPointInteger;
  Weight: Cardinal;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Backup.R := GetBackgroundColor.Rgba8.R;
  Backup.G := GetBackgroundColor.Rgba8.G;
  Backup.B := GetBackgroundColor.Rgba8.B;
  Backup.A := GetBackgroundColor.Rgba8.A;

  Span := Allocator.Span;

  Max.X := SourceImage.Width - 1;
  Max.Y := SourceImage.Height - 1;

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X < Max.X) and
      (LoRes.Y < Max.Y) then
    begin
      ForeGround[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
      ForeGround[1] := ForeGround[0];
      ForeGround[2] := ForeGround[0];
      ForeGround[3] := ForeGround[0];

      HiRes.X := HiRes.X and CAggImageSubpixelMask;
      HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
        (LoRes.X shl 2) * SizeOf(Int8u));
      Weight := (CAggImageSubpixelSize - HiRes.X) * (CAggImageSubpixelSize -
        HiRes.Y);

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := HiRes.X * (CAggImageSubpixelSize - HiRes.Y);

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGroundPointer := SourceImage.NextRow
        (PInt8u(PtrComp(ForeGroundPointer) - 8 * SizeOf(Int8u)));
      Weight := (CAggImageSubpixelSize - HiRes.X) * HiRes.Y;

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := HiRes.X * HiRes.Y;

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGround[0] := ForeGround[0] shr (CAggImageSubpixelShift * 2);
      ForeGround[1] := ForeGround[1] shr (CAggImageSubpixelShift * 2);
      ForeGround[2] := ForeGround[2] shr (CAggImageSubpixelShift * 2);
      ForeGround[3] := ForeGround[3] shr (CAggImageSubpixelShift * 2);
    end
    else
    begin
      if (LoRes.X < -1) or (LoRes.Y < -1) or (LoRes.X > Max.X) or
        (LoRes.Y > Max.Y) then
      begin
        ForeGround[FOrder.R] := Backup.R;
        ForeGround[FOrder.G] := Backup.G;
        ForeGround[FOrder.B] := Backup.B;
        ForeGround[FOrder.A] := Backup.A;
      end
      else
      begin
        ForeGround[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
        ForeGround[1] := ForeGround[0];
        ForeGround[2] := ForeGround[0];
        ForeGround[3] := ForeGround[0];

        HiRes.X := HiRes.X and CAggImageSubpixelMask;
        HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

        Weight := (CAggImageSubpixelSize - HiRes.X) *
          (CAggImageSubpixelSize - HiRes.Y);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := HiRes.X * (CAggImageSubpixelSize - HiRes.Y);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        Dec(LoRes.X);
        Inc(LoRes.Y);

        Weight := (CAggImageSubpixelSize - HiRes.X) * HiRes.Y;

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := HiRes.X * HiRes.Y;

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        ForeGround[0] := ForeGround[0] shr (CAggImageSubpixelShift * 2);
        ForeGround[1] := ForeGround[1] shr (CAggImageSubpixelShift * 2);
        ForeGround[2] := ForeGround[2] shr (CAggImageSubpixelShift * 2);
        ForeGround[3] := ForeGround[3] shr (CAggImageSubpixelShift * 2);
      end;
    end;

    Span.Rgba8.R := Int8u(ForeGround[FOrder.R]);
    Span.Rgba8.G := Int8u(ForeGround[FOrder.G]);
    Span.Rgba8.B := Int8u(ForeGround[FOrder.B]);
    Span.Rgba8.A := Int8u(ForeGround[FOrder.A]);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterRgba2x2 }

constructor TAggSpanImageFilterRgba2x2.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgba2x2.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgba2x2.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0..3] of Cardinal;
  Backup: TAggRgba8;
  BackgroundColor: PAggColor;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  WeightArray: PInt16;
  Max, HiRes, LoRes: TPointInteger;
  Weight: Cardinal;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackgroundColor := GetBackgroundColor;
  Backup := BackgroundColor.Rgba8;

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ((Filter.Diameter div 2 - 1) shl CAggImageSubpixelShift) * SizeOf(Int16));

  Max.X := SourceImage.Width - 1;
  Max.Y := SourceImage.Height - 1;

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    ForeGround[0] := CAggImageFilterSize div 2;
    ForeGround[1] := ForeGround[0];
    ForeGround[2] := ForeGround[0];
    ForeGround[3] := ForeGround[0];

    if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X < Max.X) and
      (LoRes.Y < Max.Y) then
    begin
      HiRes.X := HiRes.X and CAggImageSubpixelMask;
      HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
        (LoRes.X shl 2) * SizeOf(Int8u));
      Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
        (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
        PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
        SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16)
        )^ * PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
        SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGroundPointer := SourceImage.NextRow
        (PInt8u(PtrComp(ForeGroundPointer) - 8 * SizeOf(Int8u)));
      Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
        (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
        PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
        CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16)
        )^ * PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
        CAggImageFilterSize div 2, CAggImageFilterShift);

      Inc(ForeGround[0], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[1], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[2], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      Inc(ForeGround[3], Weight * ForeGroundPointer^);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGround[0] := ForeGround[0] shr CAggImageFilterShift;
      ForeGround[1] := ForeGround[1] shr CAggImageFilterShift;
      ForeGround[2] := ForeGround[2] shr CAggImageFilterShift;
      ForeGround[3] := ForeGround[3] shr CAggImageFilterShift;

      if ForeGround[FOrder.A] > CAggBaseMask then
        ForeGround[FOrder.A] := CAggBaseMask;

      if ForeGround[FOrder.R] > ForeGround[FOrder.A] then
        ForeGround[FOrder.R] := ForeGround[FOrder.A];

      if ForeGround[FOrder.G] > ForeGround[FOrder.A] then
        ForeGround[FOrder.G] := ForeGround[FOrder.A];

      if ForeGround[FOrder.B] > ForeGround[FOrder.A] then
        ForeGround[FOrder.B] := ForeGround[FOrder.A];
    end
    else
    begin
      if (LoRes.X < -1) or (LoRes.Y < -1) or (LoRes.X > Max.X) or
        (LoRes.Y > Max.Y) then
      begin
        ForeGround[FOrder.R] := Backup.R;
        ForeGround[FOrder.G] := Backup.G;
        ForeGround[FOrder.B] := Backup.B;
        ForeGround[FOrder.A] := Backup.A;
      end
      else
      begin
        HiRes.X := HiRes.X and CAggImageSubpixelMask;
        HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
          (HiRes.Y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        Dec(LoRes.X);
        Inc(LoRes.Y);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        Inc(LoRes.X);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.Y *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and
          (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            (LoRes.X shl 2) * SizeOf(Int8u));

          Inc(ForeGround[0], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], Weight * ForeGroundPointer^);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        end
        else
        begin
          Inc(ForeGround[FOrder.R], Backup.R * Weight);
          Inc(ForeGround[FOrder.G], Backup.G * Weight);
          Inc(ForeGround[FOrder.B], Backup.B * Weight);
          Inc(ForeGround[FOrder.A], Backup.A * Weight);
        end;

        ForeGround[0] := ForeGround[0] shr CAggImageFilterShift;
        ForeGround[1] := ForeGround[1] shr CAggImageFilterShift;
        ForeGround[2] := ForeGround[2] shr CAggImageFilterShift;
        ForeGround[3] := ForeGround[3] shr CAggImageFilterShift;

        if ForeGround[FOrder.A] > CAggBaseMask then
          ForeGround[FOrder.A] := CAggBaseMask;

        if ForeGround[FOrder.R] > ForeGround[FOrder.A] then
          ForeGround[FOrder.R] := ForeGround[FOrder.A];

        if ForeGround[FOrder.G] > ForeGround[FOrder.A] then
          ForeGround[FOrder.G] := ForeGround[FOrder.A];

        if ForeGround[FOrder.B] > ForeGround[FOrder.A] then
          ForeGround[FOrder.B] := ForeGround[FOrder.A];
      end;
    end;

    Span.Rgba8.R := Int8u(ForeGround[FOrder.R]);
    Span.Rgba8.G := Int8u(ForeGround[FOrder.G]);
    Span.Rgba8.B := Int8u(ForeGround[FOrder.B]);
    Span.Rgba8.A := Int8u(ForeGround[FOrder.A]);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterRgba }

constructor TAggSpanImageFilterRgba.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgba.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgba.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0..3] of Integer;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  BackgroundColor: PAggColor;
  Diameter, StepBack, CountY: Cardinal;

  Max, Max2, LoRes, HiRes: TPointInteger;
  Start, Start1, CountX, WeightY, Weight, FractX: Integer;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackgroundColor := GetBackgroundColor;
  Backup := BackgroundColor.Rgba8;

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  Start1 := Start - 1;
  WeightArray := Filter.WeightArray;

  StepBack := Diameter shl 2;

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

    ForeGround[0] := CAggImageFilterSize div 2;
    ForeGround[1] := ForeGround[0];
    ForeGround[2] := ForeGround[0];
    ForeGround[3] := ForeGround[0];

    FractX := HiRes.X and CAggImageSubpixelMask;
    CountY := Diameter;

    if (LoRes.X >= -Start) and (LoRes.Y >= -Start) and (LoRes.X <= Max.X) and
      (LoRes.Y <= Max.Y) then
    begin
      HiRes.Y := CAggImageSubpixelMask - (HiRes.Y and CAggImageSubpixelMask);
      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y + Start)) +
        ((LoRes.X + Start) shl 2) * SizeOf(Int8u));

      repeat
        CountX := Diameter;
        WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;
        HiRes.X := CAggImageSubpixelMask - FractX;

        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

          Inc(ForeGround[0], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[1], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[2], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
          Inc(ForeGround[3], ForeGroundPointer^ * Weight);
          Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

          Inc(HiRes.X, CAggImageSubpixelSize);
          Dec(CountX);

        until CountX = 0;

        Inc(HiRes.Y, CAggImageSubpixelSize);

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - StepBack));

        Dec(CountY);
      until CountY = 0;

      ForeGround[0] := ShrInt32(ForeGround[0], CAggImageFilterShift);
      ForeGround[1] := ShrInt32(ForeGround[1], CAggImageFilterShift);
      ForeGround[2] := ShrInt32(ForeGround[2], CAggImageFilterShift);
      ForeGround[3] := ShrInt32(ForeGround[3], CAggImageFilterShift);

      if ForeGround[0] < 0 then
        ForeGround[0] := 0;

      if ForeGround[1] < 0 then
        ForeGround[1] := 0;

      if ForeGround[2] < 0 then
        ForeGround[2] := 0;

      if ForeGround[3] < 0 then
        ForeGround[3] := 0;

      if ForeGround[FOrder.A] > CAggBaseMask then
        ForeGround[FOrder.A] := CAggBaseMask;

      if ForeGround[FOrder.R] > ForeGround[FOrder.A] then
        ForeGround[FOrder.R] := ForeGround[FOrder.A];

      if ForeGround[FOrder.G] > ForeGround[FOrder.A] then
        ForeGround[FOrder.G] := ForeGround[FOrder.A];

      if ForeGround[FOrder.B] > ForeGround[FOrder.A] then
        ForeGround[FOrder.B] := ForeGround[FOrder.A];
    end
    else
    begin
      if (LoRes.X < Start1) or (LoRes.Y < Start1) or (LoRes.X > Max2.X) or
        (LoRes.Y > Max2.Y) then
      begin
        ForeGround[FOrder.R] := Backup.R;
        ForeGround[FOrder.G] := Backup.G;
        ForeGround[FOrder.B] := Backup.B;
        ForeGround[FOrder.A] := Backup.A;
      end
      else
      begin
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
                (LoRes.X shl 2) * SizeOf(Int8u));

              Inc(ForeGround[0], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              Inc(ForeGround[1], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              Inc(ForeGround[2], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              Inc(ForeGround[3], ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            end
            else
            begin
              Inc(ForeGround[FOrder.R], Backup.R * Weight);
              Inc(ForeGround[FOrder.G], Backup.G * Weight);
              Inc(ForeGround[FOrder.B], Backup.B * Weight);
              Inc(ForeGround[FOrder.A], Backup.A * Weight);
            end;

            Inc(HiRes.X, CAggImageSubpixelSize);
            Inc(LoRes.X);
            Dec(CountX);
          until CountX = 0;

          Inc(HiRes.Y, CAggImageSubpixelSize);
          Inc(LoRes.Y);
          Dec(CountY);
        until CountY = 0;

        ForeGround[0] := ShrInt32(ForeGround[0], CAggImageFilterShift);
        ForeGround[1] := ShrInt32(ForeGround[1], CAggImageFilterShift);
        ForeGround[2] := ShrInt32(ForeGround[2], CAggImageFilterShift);
        ForeGround[3] := ShrInt32(ForeGround[3], CAggImageFilterShift);

        if ForeGround[0] < 0 then
          ForeGround[0] := 0;

        if ForeGround[1] < 0 then
          ForeGround[1] := 0;

        if ForeGround[2] < 0 then
          ForeGround[2] := 0;

        if ForeGround[3] < 0 then
          ForeGround[3] := 0;

        if ForeGround[FOrder.A] > CAggBaseMask then
          ForeGround[FOrder.A] := CAggBaseMask;

        if ForeGround[FOrder.R] > ForeGround[FOrder.A] then
          ForeGround[FOrder.R] := ForeGround[FOrder.A];

        if ForeGround[FOrder.G] > ForeGround[FOrder.A] then
          ForeGround[FOrder.G] := ForeGround[FOrder.A];

        if ForeGround[FOrder.B] > ForeGround[FOrder.A] then
          ForeGround[FOrder.B] := ForeGround[FOrder.A];
      end;
    end;

    Span.Rgba8.R := ForeGround[FOrder.R];
    Span.Rgba8.G := ForeGround[FOrder.G];
    Span.Rgba8.B := ForeGround[FOrder.B];
    Span.Rgba8.A := ForeGround[FOrder.A];

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
