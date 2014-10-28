unit AggSpanImageResample;

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
  AggSpanImageFilter,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

type
  TAggCustomSpanImageResample = class(TAggSpanImageFilter)
  protected
    function GetBlurX: Double; virtual; abstract;
    function GetBlurY: Double; virtual; abstract;
    function GetScaleLimit: Integer; virtual; abstract;
    procedure SetBlurX(Value: Double); virtual; abstract;
    procedure SetBlurY(Value: Double); virtual; abstract;
    procedure SetScaleLimit(Value: Integer); virtual; abstract;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload; virtual;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload; virtual;

    procedure SetBlur(Value: Double); virtual; abstract;

    property BlurX: Double read GetBlurX write SetBlurX;
    property BlurY: Double read GetBlurY write SetBlurY;
    property ScaleLimit: Integer read GetScaleLimit write SetScaleLimit;
  end;

  TAggSpanImageResample = class(TAggCustomSpanImageResample)
  protected
    FScaleLimit: Integer;
    FBlur: TPointInteger;
    function GetBlurX: Double; override;
    function GetBlurY: Double; override;
    function GetScaleLimit: Integer; override;
    procedure SetBlurX(Value: Double); override;
    procedure SetBlurY(Value: Double); override;
    procedure SetScaleLimit(Value: Integer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator); override;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); override;

    procedure SetBlur(Value: Double); override;
  end;

  TAggSpanImageResampleAffine = class(TAggCustomSpanImageResample)
  private
    FScaleLimit: Double;
    procedure SetScaleLimitDouble(Value: Double);
  protected
    FBlur: TPointDouble;
    FRadiusX, FRadiusY: Integer;
    FRadiusXInv, FRadiusYInv: Integer;
    function GetBlurX: Double; override;
    function GetBlurY: Double; override;
    function GetScaleLimit: Integer; override;
    procedure SetBlurX(Value: Double); override;
    procedure SetBlurY(Value: Double); override;
    procedure SetScaleLimit(Value: Integer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator); override;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); override;

    procedure SetBlur(Value: Double); override;

    procedure Prepare(MaxSpanLength: Cardinal); override;

    property RadiusX: Integer read FRadiusX;
    property RadiusY: Integer read FRadiusY;
    property ScaleLimitDouble: Double read FScaleLimit write
      SetScaleLimitDouble;
  end;

implementation


{ TAggCustomSpanImageResample }

constructor TAggCustomSpanImageResample.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggCustomSpanImageResample.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);
end;


{ TAggSpanImageResample }

constructor TAggSpanImageResample.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FScaleLimit := 20;

  FBlur.X := CAggImageSubpixelSize;
  FBlur.Y := CAggImageSubpixelSize;
end;

constructor TAggSpanImageResample.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);

  FScaleLimit := 20;

  FBlur.X := CAggImageSubpixelSize;
  FBlur.Y := CAggImageSubpixelSize;
end;

procedure TAggSpanImageResample.SetScaleLimit(Value: Integer);
begin
  FScaleLimit := Value;
end;

function TAggSpanImageResample.GetBlurX: Double;
begin
  Result := FBlur.X / CAggImageSubpixelSize;
end;

function TAggSpanImageResample.GetBlurY: Double;
begin
  Result := FBlur.Y / CAggImageSubpixelSize;
end;

function TAggSpanImageResample.GetScaleLimit: Integer;
begin
  Result := FScaleLimit;
end;

procedure TAggSpanImageResample.SetBlurX(Value: Double);
begin
  FBlur.X := Trunc(Value * CAggImageSubpixelSize + 0.5);
end;

procedure TAggSpanImageResample.SetBlurY(Value: Double);
begin
  FBlur.Y := Trunc(Value * CAggImageSubpixelSize + 0.5);
end;

procedure TAggSpanImageResample.SetBlur(Value: Double);
begin
  FBlur.X := Trunc(Value * CAggImageSubpixelSize + 0.5);
  FBlur.Y := FBlur.X;
end;


{ TAggSpanImageResampleAffine }

constructor TAggSpanImageResampleAffine.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FScaleLimit := 200.0;

  FBlur.X := 1.0;
  FBlur.Y := 1.0;
end;

constructor TAggSpanImageResampleAffine.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);

  FScaleLimit := 200.0;

  FBlur.X := 1.0;
  FBlur.Y := 1.0;
end;

function TAggSpanImageResampleAffine.GetBlurX: Double;
begin
  Result := FBlur.X;
end;

function TAggSpanImageResampleAffine.GetBlurY: Double;
begin
  Result := FBlur.Y;
end;

function TAggSpanImageResampleAffine.GetScaleLimit: Integer;
begin
  Result := Trunc(FScaleLimit);
end;

procedure TAggSpanImageResampleAffine.SetScaleLimit(Value: Integer);
begin
  FScaleLimit := Value;
end;

procedure TAggSpanImageResampleAffine.SetScaleLimitDouble(Value: Double);
begin
  FScaleLimit := Value;
end;

procedure TAggSpanImageResampleAffine.SetBlurX(Value: Double);
begin
  FBlur.X := Value;
end;

procedure TAggSpanImageResampleAffine.SetBlurY(Value: Double);
begin
  FBlur.Y := Value;
end;

procedure TAggSpanImageResampleAffine.SetBlur(Value: Double);
begin
  FBlur.X := Value;
  FBlur.Y := Value;
end;

procedure TAggSpanImageResampleAffine.Prepare(MaxSpanLength: Cardinal);
var
  Scale: TPointDouble;
begin
  inherited Prepare(MaxSpanLength);

  Interpolator.Transformer.GetScalingAbs(Scale.X, Scale.Y);

  FRadiusX := CAggImageSubpixelSize;
  FRadiusY := CAggImageSubpixelSize;
  FRadiusXInv := CAggImageSubpixelSize;
  FRadiusYInv := CAggImageSubpixelSize;

  Scale.X := Scale.X * FBlur.X;
  Scale.Y := Scale.Y * FBlur.Y;

  if Scale.X * Scale.Y > FScaleLimit then
  begin
    Scale.X := Scale.X * FScaleLimit / (Scale.X * Scale.Y);
    Scale.Y := Scale.Y * FScaleLimit / (Scale.X * Scale.Y);
  end;

  if Scale.X > 1.0001 then
  begin
    if Scale.X > FScaleLimit then
      Scale.X := FScaleLimit;

    FRadiusX := Trunc(Scale.X * CAggImageSubpixelSize + 0.5);
    FRadiusXInv := Trunc(1.0 / Scale.X * CAggImageSubpixelSize + 0.5);
  end;

  if Scale.Y > 1.0001 then
  begin
    if Scale.Y > FScaleLimit then
      Scale.Y := FScaleLimit;

    FRadiusY := Trunc(Scale.Y * CAggImageSubpixelSize + 0.5);
    FRadiusYInv := Trunc(1.0 / Scale.Y * CAggImageSubpixelSize + 0.5);
  end;
end;

end.
