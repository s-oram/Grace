unit AggSpanImageFilterGray;

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
  AggImageFilters,
  AggSpanImageFilter,
  AggSpanAllocator,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer;

const
  CAggBaseShift = AggColor.CAggBaseShift;
  CAggBaseMask = AggColor.CAggBaseMask;

type
  TAggSpanImageFilterGrayNN = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterGrayBilinear = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterGray2x2 = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterGray = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Inter: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageFilterGrayNN }

constructor TAggSpanImageFilterGrayNN.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGrayNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Inter: TAggSpanInterpolator);
begin
  inherited Create(Alloc, Src, BackColor, Inter, nil);
end;

function TAggSpanImageFilterGrayNN.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Max: TPointInteger;
  Fg, SourceAlpha: Cardinal;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  Max := PointInteger(SourceImage.Width - 1, SourceImage.Height - 1);

  repeat
    Interpolator.Coordinates(@X, @Y);

    X := ShrInt32(X, CAggImageSubpixelShift);
    Y := ShrInt32(Y, CAggImageSubpixelShift);

    if (X >= 0) and (Y >= 0) and (X <= Max.X) and (Y <= Max.Y) then
    begin
      Fg := PInt8u(PtrComp(SourceImage.Row(Y)) + X * SizeOf(Int8u))^;

      SourceAlpha := CAggBaseMask;
    end
    else
    begin
      Fg := GetBackgroundColor.V;

      SourceAlpha := GetBackgroundColor.Rgba8.A;
    end;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(SourceAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterGrayBilinear }

constructor TAggSpanImageFilterGrayBilinear.Create
  (Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGrayBilinear.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator);
begin
  inherited Create(Alloc, Src, BackColor, Inter, nil);
end;

function TAggSpanImageFilterGrayBilinear.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg, SourceAlpha, Weight: Cardinal;
  BackV, BackAlpha: Int8u;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.V;
  BackAlpha := GetBackgroundColor.Rgba8.A;

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
      Fg := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;

      HiRes.X := HiRes.X and CAggImageSubpixelMask;
      HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
        SizeOf(Int8u));

      Inc(Fg, ForeGroundPointer^ * (CAggImageSubpixelSize - HiRes.X) *
        (CAggImageSubpixelSize - HiRes.Y));
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Inc(Fg, ForeGroundPointer^ * (CAggImageSubpixelSize - HiRes.Y) * HiRes.X);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGroundPointer := SourceImage.NextRow(PInt8u(PtrComp(ForeGroundPointer) - 2));

      Inc(Fg, ForeGroundPointer^ * (CAggImageSubpixelSize - HiRes.X) * HiRes.Y);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Inc(Fg, ForeGroundPointer^ * HiRes.X * HiRes.Y);
      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Fg := Fg shr (CAggImageSubpixelShift * 2);

      SourceAlpha := CAggBaseMask;
    end
    else
    begin
      if (LoRes.X < -1) or (LoRes.Y < -1) or (LoRes.X > Max.X) or (LoRes.Y > Max.Y) then
      begin
        Fg := BackV;
        SourceAlpha := BackAlpha;
      end
      else
      begin
        Fg := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
        SourceAlpha := Fg;

        HiRes.X := HiRes.X and CAggImageSubpixelMask;
        HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

        Weight := (CAggImageSubpixelSize - HiRes.X) * (CAggImageSubpixelSize - HiRes.Y);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Inc(LoRes.X);

        Weight := HiRes.X * (CAggImageSubpixelSize - HiRes.Y);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Dec(LoRes.X);
        Inc(LoRes.Y);

        Weight := (CAggImageSubpixelSize - HiRes.X) * HiRes.Y;

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Inc(LoRes.X);

        Weight := HiRes.X * HiRes.Y;

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Fg := Fg shr (CAggImageSubpixelShift * 2);
        SourceAlpha := SourceAlpha shr (CAggImageSubpixelShift * 2);
      end;
    end;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(SourceAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterGray2x2 }

constructor TAggSpanImageFilterGray2x2.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGray2x2.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Inter, Filter);
end;

function TAggSpanImageFilterGray2x2.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg, SourceAlpha, Weight: Cardinal;
  BackV, BackAlpha: Int8u;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  WeightArray: PInt16;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.V;
  BackAlpha := GetBackgroundColor.Rgba8.A;

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

    if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X < Max.X) and (LoRes.Y < Max.Y) then
    begin
      Fg := CAggImageFilterSize div 2;

      HiRes.X := HiRes.X and CAggImageSubpixelMask;
      HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
        SizeOf(Int8u));

      Inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) +
        (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
        PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
        SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift));

      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
        SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
        (HiRes.Y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
        CAggImageFilterSize div 2, CAggImageFilterShift));

      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      ForeGroundPointer := SourceImage.NextRow
        (PInt8u(PtrComp(ForeGroundPointer) - 2 * SizeOf(Int8u)));

      Inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) +
        (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
        PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
        CAggImageFilterSize div 2, CAggImageFilterShift));

      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
        SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16)
        )^ + CAggImageFilterSize div 2, CAggImageFilterShift));

      Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

      Fg := Fg shr CAggImageFilterShift;

      if Fg > CAggBaseMask then
        Fg := CAggBaseMask;

      SourceAlpha := CAggBaseMask;

    end
    else
    begin
      if (LoRes.X < -1) or (LoRes.Y < -1) or (LoRes.X > Max.X) or (LoRes.Y > Max.Y) then
      begin
        Fg := BackV;
        SourceAlpha := BackAlpha;
      end
      else
      begin
        Fg := CAggImageFilterSize div 2;
        SourceAlpha := Fg;

        HiRes.X := HiRes.X and CAggImageSubpixelMask;
        HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Inc(LoRes.X);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
          (HiRes.Y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
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
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Inc(LoRes.X);

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.Y *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        if (LoRes.X >= 0) and (LoRes.Y >= 0) and (LoRes.X <= Max.X) and (LoRes.Y <= Max.Y)
        then
        begin
          Inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
            SizeOf(Int8u))^);
          Inc(SourceAlpha, Weight * CAggBaseMask);
        end
        else
        begin
          Inc(Fg, BackV * Weight);
          Inc(SourceAlpha, BackAlpha * Weight);
        end;

        Fg := Fg shr CAggImageFilterShift;
        SourceAlpha := SourceAlpha shr CAggImageFilterShift;

        if SourceAlpha > CAggBaseMask then
          SourceAlpha := CAggBaseMask;

        if Fg > SourceAlpha then
          Fg := SourceAlpha;
      end;
    end;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(SourceAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanImageFilterGray }

constructor TAggSpanImageFilterGray.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGray.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; Inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Inter, Filter);
end;

function TAggSpanImageFilterGray.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Fg, SourceAlpha, Start, Start1, FractX, Weight, CountX, WeightY: Integer;
  Max, Max2, HiRes, LoRes: TPointInteger;
  BackV, BackAlpha: Int8u;
  ForeGroundPointer: PInt8u;
  Diameter, CountY: Cardinal;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.V;
  BackAlpha := GetBackgroundColor.Rgba8.A;

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  Start1 := Start - 1;
  WeightArray := Filter.WeightArray;

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

    Fg := CAggImageFilterSize div 2;

    FractX := HiRes.X and CAggImageSubpixelMask;
    CountY := Diameter;

    if (LoRes.X >= -Start) and (LoRes.Y >= -Start) and (LoRes.X <= Max.X) and
      (LoRes.Y <= Max.Y) then
    begin
      HiRes.Y := CAggImageSubpixelMask - (HiRes.Y and CAggImageSubpixelMask);
      ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y + Start)) +
        (LoRes.X + Start) * SizeOf(Int8u));

      repeat
        CountX := Diameter;
        WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;
        HiRes.X := CAggImageSubpixelMask - FractX;

        repeat
          Inc(Fg, ForeGroundPointer^ * ShrInt32(WeightY * PInt16(PtrComp(WeightArray)
            + HiRes.X * SizeOf(Int16))^ + CAggImageFilterSize div 2,
            CAggImageFilterShift));

          Inc(HiRes.X, CAggImageSubpixelSize);
          Dec(CountX);
        until CountX = 0;

        Inc(HiRes.Y, CAggImageSubpixelSize);

        ForeGroundPointer := SourceImage.NextRow(PInt8u(PtrComp(ForeGroundPointer) - Diameter));

        Dec(CountY);
      until CountY = 0;

      Fg := ShrInt32(Fg, CAggImageFilterShift);

      if Fg < 0 then
        Fg := 0;

      if Fg > CAggBaseMask then
        Fg := CAggBaseMask;

      SourceAlpha := CAggBaseMask;
    end
    else
    begin
      if (LoRes.X < Start1) or (LoRes.Y < Start1) or (LoRes.X > Max2.X) or (LoRes.Y > Max2.Y)
      then
      begin
        Fg := BackV;
        SourceAlpha := BackAlpha;
      end
      else
      begin
        SourceAlpha := CAggImageFilterSize div 2;

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
              ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) + LoRes.X *
                SizeOf(Int8u));

              Inc(Fg, ForeGroundPointer^ * Weight);
              Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              Inc(SourceAlpha, CAggBaseMask * Weight);
            end
            else
            begin
              Inc(Fg, BackV * Weight);
              Inc(SourceAlpha, BackAlpha * Weight);
            end;

            Inc(HiRes.X, CAggImageSubpixelSize);
            Inc(LoRes.X);
            Dec(CountX);
          until CountX = 0;

          Inc(HiRes.Y, CAggImageSubpixelSize);
          Inc(LoRes.Y);
          Dec(CountY);
        until CountY = 0;

        Fg := ShrInt32(Fg, CAggImageFilterShift);
        SourceAlpha := ShrInt32(SourceAlpha, CAggImageFilterShift);

        if Fg < 0 then
          Fg := 0;

        if SourceAlpha < 0 then
          SourceAlpha := 0;

        if SourceAlpha > CAggBaseMask then
          SourceAlpha := CAggBaseMask;

        if Fg > SourceAlpha then
          Fg := SourceAlpha;
      end;
    end;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := Int8u(SourceAlpha);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
