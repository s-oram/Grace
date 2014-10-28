unit AggSpanPatternFilterGray;

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
  AggSpanImageFilter,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

const
  CAggBaseShift = AggColor.CAggBaseShift;
  CAggBaseMask = AggColor.CAggBaseMask;

type
  TAggSpanPatternFilterGrayNN = class(TAggSpanImageFilter)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      WX, WY: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      Interpolator: TAggSpanInterpolator; WX, WY: TAggWrapMode); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterGrayBilinear = class(TAggSpanImageFilter)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator;
      WX, WY: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      Interpolator: TAggSpanInterpolator; WX, WY: TAggWrapMode); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterGray2x2 = class(TAggSpanImageFilter)
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

  TAggSpanPatternFilterGray = class(TAggSpanImageFilter)
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


{ TAggSpanPatternFilterGrayNN }

constructor TAggSpanPatternFilterGrayNN.Create(Alloc: TAggSpanAllocator;
  WX, WY: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGrayNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  WX, WY: TAggWrapMode);
var
  Rgba: TAggColor;

begin
  Rgba.Clear;

  inherited Create(Alloc, Src, @Rgba, Interpolator, nil);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

procedure TAggSpanPatternFilterGrayNN.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternFilterGrayNN.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@X, @Y);

    X := FWrapModeX.FuncOperator(ShrInt32(X, CAggImageSubpixelShift));
    Y := FWrapModeY.FuncOperator(ShrInt32(Y, CAggImageSubpixelShift));

    Span.V := PInt8u(PtrComp(SourceImage.Row(Y)) + X * SizeOf(Int8u))^;
    Span.Rgba8.A := CAggBaseMask;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanPatternFilterGrayBilinear }

constructor TAggSpanPatternFilterGrayBilinear.Create
  (Alloc: TAggSpanAllocator; WX, WY: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGrayBilinear.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; WX, WY: TAggWrapMode);
var
  Rgba: TAggColor;

begin
  Rgba.Clear;

  inherited Create(Alloc, Src, @Rgba, Interpolator, nil);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

procedure TAggSpanPatternFilterGrayBilinear.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternFilterGrayBilinear.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg, X1, X2, Y1, Y2: Cardinal;

  HiRes, LoRes: TPointInteger;

  Span: PAggColor;

  Ptr1, Ptr2: PInt8u;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    X1 := FWrapModeX.FuncOperator(LoRes.X);
    X2 := FWrapModeX.IncOperator;

    Y1 := FWrapModeY.FuncOperator(LoRes.Y);
    Y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(Y1);
    Ptr2 := SourceImage.Row(Y2);

    Fg := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;

    HiRes.X := HiRes.X and CAggImageSubpixelMask;
    HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

    Inc(Fg, PInt8u(PtrComp(Ptr1) + X1 * SizeOf(Int8u))^ *
      (CAggImageSubpixelSize - HiRes.X) * (CAggImageSubpixelSize - HiRes.Y));
    Inc(Fg, PInt8u(PtrComp(Ptr1) + X2 * SizeOf(Int8u))^ * HiRes.X *
      (CAggImageSubpixelSize - HiRes.Y));
    Inc(Fg, PInt8u(PtrComp(Ptr2) + X1 * SizeOf(Int8u))^ *
      (CAggImageSubpixelSize - HiRes.X) * HiRes.Y);
    Inc(Fg, PInt8u(PtrComp(Ptr2) + X2 * SizeOf(Int8u))^ * HiRes.X * HiRes.Y);

    Span.V := Int8u(Fg shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.A := CAggBaseMask;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanPatternFilterGray2x2 }

constructor TAggSpanPatternFilterGray2x2.Create(Alloc: TAggSpanAllocator;
  WX, WY: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGray2x2.Create(Alloc: TAggSpanAllocator;
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

procedure TAggSpanPatternFilterGray2x2.SetSourceImage(Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternFilterGray2x2.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg, X1, X2, Y1, Y2: Cardinal;

  HiRes, LoRes: TPointInteger;

  Ptr1, Ptr2: PInt8u;

  Span: PAggColor;

  WeightArray: PInt16;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ShrInt32(Filter.Diameter div 2 - 1, CAggImageSubpixelShift));

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Inc(HiRes.X, FilterDeltaXInteger);
    Inc(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    X1 := FWrapModeX.FuncOperator(LoRes.X);
    X2 := FWrapModeX.IncOperator;

    Y1 := FWrapModeY.FuncOperator(LoRes.Y);
    Y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(Y1);
    Ptr2 := SourceImage.Row(Y2);

    Fg := CAggImageFilterSize div 2;

    HiRes.X := HiRes.X and CAggImageSubpixelMask;
    HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

    Inc(Fg, PInt8u(PtrComp(Ptr1) + X1 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + (HiRes.X + CAggImageSubpixelSize) *
      SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
      (HiRes.Y + CAggImageSubpixelSize) * SizeOf(Int16))^ + CAggImageFilterSize div 2,
      CAggImageFilterShift));

    Inc(Fg, PInt8u(PtrComp(Ptr1) + X2 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
      SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift));

    Inc(Fg, PInt8u(PtrComp(Ptr2) + X1 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + (HiRes.X + CAggImageSubpixelSize) *
      SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^
      + CAggImageFilterSize div 2, CAggImageFilterShift));

    Inc(Fg, PInt8u(PtrComp(Ptr2) + X2 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
      CAggImageFilterSize div 2, CAggImageFilterShift));

    Fg := Fg shr CAggImageFilterShift;

    if Fg > CAggBaseMask then
      Fg := CAggBaseMask;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := CAggBaseMask;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);

  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggSpanPatternFilterGray }

constructor TAggSpanPatternFilterGray.Create(Alloc: TAggSpanAllocator;
  WX, WY: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := WX;
  FWrapModeY := WY;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGray.Create(Alloc: TAggSpanAllocator;
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

procedure TAggSpanPatternFilterGray.SetSourceImage(Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.Width);
  FWrapModeY.Init(Src.Height);
end;

function TAggSpanPatternFilterGray.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg, Start, CountX, WeightY, FractX, XInt: Integer;
  HiRes, LoRes: TPointInteger;
  Diameter, Y_count: Cardinal;
  RowPointer: PInt8u;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  Start := Filter.Start;

  WeightArray := Filter.WeightArray;

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@X, @Y);

    Dec(X, FilterDeltaXInteger);
    Dec(Y, FilterDeltaYInteger);

    HiRes.X := X;
    HiRes.Y := Y;

    FractX := HiRes.X and CAggImageSubpixelMask;
    Y_count := Diameter;

    LoRes.Y := FWrapModeY.FuncOperator
      (ShrInt32(Y, CAggImageSubpixelShift) + Start);
    XInt := ShrInt32(X, CAggImageSubpixelShift) + Start;

    HiRes.Y := CAggImageSubpixelMask - (HiRes.Y and CAggImageSubpixelMask);
    Fg := CAggImageFilterSize div 2;

    repeat
      CountX := Diameter;
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      HiRes.X := CAggImageSubpixelMask - FractX;
      LoRes.X := FWrapModeX.FuncOperator(XInt);

      RowPointer := SourceImage.Row(LoRes.Y);

      repeat
        Inc(Fg, PInt8u(PtrComp(RowPointer) + LoRes.X * SizeOf(Int8u))^ *
          ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift));

        Inc(HiRes.X, CAggImageSubpixelSize);

        LoRes.X := FWrapModeX.IncOperator;

        Dec(CountX);

      until CountX = 0;

      Inc(HiRes.Y, CAggImageSubpixelSize);

      LoRes.Y := FWrapModeY.IncOperator;

      Dec(Y_count);

    until Y_count = 0;

    Fg := ShrInt32(Fg, CAggImageFilterShift);

    if Fg < 0 then
      Fg := 0;

    if Fg > CAggBaseMask then
      Fg := CAggBaseMask;

    Span.V := Int8u(Fg);
    Span.Rgba8.A := CAggBaseMask;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
