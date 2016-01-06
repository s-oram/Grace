unit AggRendererOutlineImage;

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
  Math,
  AggBasics,
  AggColor,
  AggLineAABasics,
  AggDdaLine,
  AggRenderingBuffer,
  AggRendererBase,
  AggRendererOutlineAA,
  AggPatternFiltersRgba;

type
  TAggPixelSource = class
  protected
    function GetWidth: Cardinal; virtual; abstract;
    function GetHeight: Cardinal; virtual; abstract;

    function GetWidthDouble: Double; virtual;
    function GetHeightDouble: Double; virtual;
  public
    function Pixel(X, Y: Integer): TAggRgba8; virtual; abstract;

    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetHeight;
    property WidthDouble: Double read GetWidthDouble;
    property HeightDouble: Double read GetHeightDouble;
  end;

  TAggLineImageScale = class(TAggPixelSource)
  private
    FSource: TAggPixelSource;
    FHeight, FScale: Double;
  protected
    function GetWidthDouble: Double; override;
    function GetHeightDouble: Double; override;
  public
    constructor Create(Src: TAggPixelSource; AHeight: Double);

    function Pixel(X, Y: Integer): TAggRgba8; override;
  end;

  TAggLineImagePattern = class
  private
    FRenderingBuffer: TAggRenderingBuffer;
    FFilter: TAggPatternFilter;

    FDilation: Cardinal;
    FDilationHR: Integer;

    FData: PAggColor;
    FDataSize, FWidth, FHeight: Cardinal;

    FWidthHR, FHalfHeightHR, FOffsetYhr: Integer;
    procedure SetFilter(AFilter: TAggPatternFilter); overload;
  public
    constructor Create(AFilter: TAggPatternFilter); overload; virtual;
    constructor Create(AFilter: TAggPatternFilter; Src: TAggPixelSource); overload;
      virtual;
    destructor Destroy; override;

    procedure Build(Src: TAggPixelSource); virtual;

    procedure Pixel(P: PAggColor; X, Y: Integer); virtual;

    property Filter: TAggPatternFilter read FFilter write SetFilter;
    property PatternWidth: Integer read FWidthHR;
    property LineWidth: Integer read FHalfHeightHR;
  end;

  TAggLineImagePatternPow2 = class(TAggLineImagePattern)
  private
    FMask: Cardinal;
  public
    constructor Create(AFilter: TAggPatternFilter); overload; override;
    constructor Create(AFilter: TAggPatternFilter; Src: TAggPixelSource); overload;
      override;

    procedure Build(Src: TAggPixelSource); override;
    procedure Pixel(P: PAggColor; X, Y: Integer); override;
  end;

  TAggDistanceInterpolator4 = class(TAggCustomDistance2Interpolator)
  private
    FDelta, FDeltaStart, FDeltaPict, FDeltaEnd: TPointInteger;
    FDist, FDistStart, FDistPict, FDistEnd, FLength: Integer;
  protected
    function GetDeltaX: Integer; override;
    function GetDeltaXEnd: Integer; override;
    function GetDeltaXStart: Integer; override;
    function GetDeltaY: Integer; override;
    function GetDeltaYEnd: Integer; override;
    function GetDeltaYStart: Integer; override;
    function GetDistance: Integer; override;
    function GetDistanceStart: Integer; override;
    function GetDistPict: Integer;
    function GetDistanceEnd: Integer; override;
  public
    constructor Create(X1, Y1, X2, Y2, Sx, Sy, Ex, Ey, Length: Integer;
      Scale: Double; X, Y: Integer); overload;

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;

    property Length: Integer read FLength;
    property DxPict: Integer read FDeltaPict.X;
    property DyPict: Integer read FDeltaPict.Y;
  end;

  TAggRendererOutlineImage = class;

  TAggLineInterpolatorImage = class // (lineInterpolator)
  private
    FLineParameters: PAggLineParameters;
    FLineInterpolator: TAggDda2LineInterpolator;
    FDistanceInterpolator: TAggDistanceInterpolator4;
    FRendererOutlineImage: TAggRendererOutlineImage;

    FX, FY, FOldX, FOldY, FCount, FWidth: Integer;
    FMaxExtent, FStart, FStep: Integer;

    FDistPos: array [0..CMaxHalfWidth + 1 - 1] of Integer;
    FColors: array [0..CMaxHalfWidth * 2 + 4 - 1] of TAggColor;
  protected
    function GetPatternEnd: Integer;
    function GetVertical: Boolean;
  public
    constructor Create(Ren: TAggRendererOutlineImage;
      Lp: PAggLineParameters; Sx, Sy, Ex, Ey, PatternStart: Integer;
      ScaleX: Double);
    destructor Destroy; override;

    function StepHorizontal: Boolean;
    function StepVertical: Boolean;

    property PatternEnd: Integer read GetPatternEnd;
    property Vertical: Boolean read GetVertical;
    property Width: Integer read FWidth;
    property Count: Integer read FCount;
  end;

  TAggRendererOutlineImage = class(TAggRendererOutline)
  private
    FRendererOutlineImage: TAggRendererBase;
    FPattern: TAggLineImagePattern;
    FStart: Integer;
    FScaleX: Double;

    procedure SetScaleX(S: Double);
    function GetScaleX: Double;

    procedure SetStartX(S: Double);
    function GetStartX: Double;
    procedure SetPattern(P: TAggLineImagePattern);
  protected
    function GetPatternWidth: Integer;
    function GetAccurateJoinOnly: Boolean; override;
    function GetSubpixelWidth: Integer; override;
  public
    constructor Create(Ren: TAggRendererBase; Patt: TAggLineImagePattern);

    procedure Pixel(P: PAggColor; X, Y: Integer);

    procedure BlendColorHSpan(X, Y: Integer; Len: Cardinal; Colors: PAggColor);
    procedure BlendColorVSpan(X, Y: Integer; Len: Cardinal; Colors: PAggColor);

    procedure SemiDot(Cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2: Integer); override;

    procedure Line0(Lp: PAggLineParameters); override;
    procedure Line1(Lp: PAggLineParameters; Sx, Sy: Integer); override;
    procedure Line2(Lp: PAggLineParameters; Ex, Ey: Integer); override;
    procedure Line3(Lp: PAggLineParameters; Sx, Sy, Ex, Ey: Integer); override;

    property StartX: Double read GetStartX write SetStartX;
    property ScaleX: Double read GetScaleX write SetScaleX;

    property PatternWidth: Integer read GetPatternWidth;
    property Pattern: TAggLineImagePattern read FPattern write SetPattern;
  end;


implementation

{ TAggPixelSource }

function TAggPixelSource.GetWidthDouble;
begin
  Result := GetWidth;
end;

function TAggPixelSource.GetHeightDouble;
begin
  Result := GetHeight;
end;


{ TAggLineImageScale }

constructor TAggLineImageScale.Create(Src: TAggPixelSource; AHeight: Double);
begin
  FSource := Src;
  FHeight := AHeight;

  if AHeight <> 0 then
    FScale := Src.GetHeight / AHeight
  else
    FScale := 0;
end;

function TAggLineImageScale.GetWidthDouble;
begin
  Result := FSource.GetWidth;
end;

function TAggLineImageScale.GetHeightDouble;
begin
  Result := FHeight;
end;

function TAggLineImageScale.Pixel(X, Y: Integer): TAggRgba8;
var
  SourceY: Double;
  H, Y1, Y2: Integer;
  Pix1, Pix2: TAggColor;
begin
  SourceY := (Y + 0.5) * FScale - 0.5;

  H := Trunc(FSource.GetHeight) - 1;
  Y1 := Trunc(SourceY);
  Y2 := Y1 + 1;

  if Y1 >= 0 then
    Pix1.FromRgba8(FSource.Pixel(X, Y1));

  if Y2 <= H then
    Pix2.FromRgba8(FSource.Pixel(X, Y2));

  Result := Pix1.Gradient8(Pix2, SourceY - Y1);
end;


{ TAggLineImagePattern }

constructor TAggLineImagePattern.Create(AFilter: TAggPatternFilter);
begin
  FRenderingBuffer := TAggRenderingBuffer.Create;

  Assert(Assigned(AFilter));
  FFilter := AFilter;

  FDilation := AFilter.Dilation + 1;
  FDilationHR := FDilation shl CAggLineSubpixelShift;

  FData := 0;
  FDataSize := 0;
  FWidth := 0;
  FHeight := 0;

  FWidthHR := 0;
  FHalfHeightHR := 0;
  FOffsetYhr := 0;
end;

constructor TAggLineImagePattern.Create(AFilter: TAggPatternFilter;
  Src: TAggPixelSource);
begin
  FRenderingBuffer := TAggRenderingBuffer.Create;

  Assert(Assigned(AFilter));
  FFilter := AFilter;

  FDilation := AFilter.Dilation + 1;
  FDilationHR := FDilation shl CAggLineSubpixelShift;

  FData := 0;
  FDataSize := 0;
  FWidth := 0;
  FHeight := 0;

  FWidthHR := 0;
  FHalfHeightHR := 0;
  FOffsetYhr := 0;

  Build(Src);
end;

destructor TAggLineImagePattern.Destroy;
begin
  AggFreeMem(Pointer(FData), FDataSize);
  FRenderingBuffer.Free;
  inherited;
end;

procedure TAggLineImagePattern.Build(Src: TAggPixelSource);
var
  X, Y, H: Cardinal;
  D1, D2, S1, S2: PAggRgba8;
begin
  FHeight := Ceil(Src.GetHeightDouble);
  FWidth := Ceil(Src.GetWidthDouble);

  FWidthHR := Trunc(Src.GetWidthDouble * CAggLineSubpixelSize);
  FHalfHeightHR := Trunc(Src.GetHeightDouble * CAggLineSubpixelSize * 0.5);
  FOffsetYhr := FDilationHR + FHalfHeightHR - CAggLineSubpixelSize div 2;

  Inc(FHalfHeightHR, CAggLineSubpixelSize div 2);

  AggFreeMem(Pointer(FData), FDataSize);

  FDataSize := (FWidth + FDilation * 2) * (FHeight + FDilation * 2) *
    SizeOf(TAggRgba8);

  AggGetMem(Pointer(FData), FDataSize);

  FRenderingBuffer.Attach(PInt8u(FData), FWidth + FDilation * 2,
    FHeight + FDilation * 2, (FWidth + FDilation * 2) * SizeOf(TAggRgba8));

  if FHeight > 0 then
    for Y := 0 to FHeight - 1 do
    begin
      D1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y + FDilation)) + FDilation *
        SizeOf(TAggRgba8));

      for X := 0 to FWidth - 1 do
      begin
        D1^ := Src.Pixel(X, Y);

        Inc(PtrComp(D1), SizeOf(TAggRgba8));
      end;
    end;

  for Y := 0 to FDilation - 1 do
  begin
    D1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(FDilation + FHeight + Y)) +
      FDilation * SizeOf(TAggRgba8));
    D2 := PAggRgba8(PtrComp(FRenderingBuffer.Row(FDilation - Y - 1)) +
      FDilation * SizeOf(TAggRgba8));

    for X := 0 to FWidth - 1 do
    begin
      D1^.NoColor;
      D2^.NoColor;

      Inc(PtrComp(D1), SizeOf(TAggRgba8));
      Inc(PtrComp(D2), SizeOf(TAggRgba8));
    end;
  end;

  H := FHeight + FDilation * 2;

  for Y := 0 to H - 1 do
  begin
    S1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) +
      FDilation * SizeOf(TAggRgba8));
    S2 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) + (FDilation + FWidth) *
      SizeOf(TAggRgba8));
    D1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) + (FDilation + FWidth) *
      SizeOf(TAggRgba8));
    D2 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) +
      FDilation * SizeOf(TAggRgba8));

    for X := 0 to FDilation - 1 do
    begin
      D1^ := S1^;

      Inc(PtrComp(D1), SizeOf(TAggRgba8));
      Inc(PtrComp(S1), SizeOf(TAggRgba8));
      Dec(PtrComp(D2), SizeOf(TAggRgba8));
      Dec(PtrComp(S2), SizeOf(TAggRgba8));

      D2^ := S2^;
    end;
  end;
end;

procedure TAggLineImagePattern.Pixel(P: PAggColor; X, Y: Integer);
begin
  FFilter.PixelHighResolution(FRenderingBuffer.Rows, P, X mod FWidthHR + FDilationHR,
    Y + FOffsetYhr);
end;

procedure TAggLineImagePattern.SetFilter(AFilter: TAggPatternFilter);
begin
  FFilter := AFilter;
end;


{ TAggLineImagePatternPow2 }

constructor TAggLineImagePatternPow2.Create(AFilter: TAggPatternFilter);
begin
  inherited Create(AFilter);

  FMask := 0;
end;

constructor TAggLineImagePatternPow2.Create(AFilter: TAggPatternFilter;
  Src: TAggPixelSource);
begin
  inherited Create(AFilter, Src);

  Build(Src);
end;

procedure TAggLineImagePatternPow2.Build(Src: TAggPixelSource);
begin
  inherited Build(Src);

  FMask := 1;

  while FMask < FWidth do
  begin
    FMask := FMask shl 1;
    FMask := FMask or 1;
  end;

  FMask := FMask shl (CAggLineSubpixelShift - 1);
  FMask := FMask or CAggLineSubpixelMask;

  FWidthHR := FMask + 1;
end;

procedure TAggLineImagePatternPow2.Pixel(P: PAggColor; X, Y: Integer);
begin
  FFilter.PixelHighResolution(FRenderingBuffer.Rows, P, (X and FMask) +
    FDilationHR, Y + FOffsetYhr);
end;


{ TAggDistanceInterpolator4 }

constructor TAggDistanceInterpolator4.Create(X1, Y1, X2, Y2, Sx, Sy, Ex, Ey,
  Length: Integer; Scale: Double; X, Y: Integer);
var
  D: Double;
  Delta: TPointInteger;
begin
  FDelta := PointInteger(X2 - X1, Y2 - Y1);

  FDeltaStart.X := LineMedResolution(Sx) - LineMedResolution(X1);
  FDeltaStart.Y := LineMedResolution(Sy) - LineMedResolution(Y1);
  FDeltaEnd.X := LineMedResolution(Ex) - LineMedResolution(X2);
  FDeltaEnd.Y := LineMedResolution(Ey) - LineMedResolution(Y2);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - X2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - Y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(Sx)) * FDeltaStart.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(Sy)) * FDeltaStart.X;

  FDistEnd := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(Ex)) * FDeltaEnd.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(Ey)) * FDeltaEnd.X;

  if Scale <> 0 then
    FLength := Trunc(Length / Scale)
  else
    FLength := 0;

  D := Length * Scale;

  if D <> 0 then
  begin
    D := 1 / D;
    Delta.X := Trunc(((X2 - X1) shl CAggLineSubpixelShift) * D);
    Delta.Y := Trunc(((Y2 - Y1) shl CAggLineSubpixelShift) * D);
  end
  else
    Delta := PointInteger(0, 0);

  FDeltaPict := PointInteger(-Delta.Y, Delta.X);

  FDistPict := ShrInt32((X + CAggLineSubpixelSize div 2 - (X1 - Delta.Y)) *
    FDeltaPict.Y - (Y + CAggLineSubpixelSize div 2 - (Y1 + Delta.X)) *
    FDeltaPict.X, CAggLineSubpixelShift);

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;
  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;
  FDeltaEnd.X := FDeltaEnd.X shl CAggLineMrSubpixelShift;
  FDeltaEnd.Y := FDeltaEnd.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator4.IncX;
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistPict, FDeltaPict.Y);
  Inc(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator4.DecX;
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistPict, FDeltaPict.Y);
  Dec(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator4.IncY;
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistPict, FDeltaPict.X);
  Dec(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator4.DecY;
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistPict, FDeltaPict.X);
  Inc(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator4.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistPict, FDeltaPict.Y);
  Inc(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
  begin
    Dec(FDist, FDelta.X);
    Dec(FDistStart, FDeltaStart.X);
    Dec(FDistPict, FDeltaPict.X);
    Dec(FDistEnd, FDeltaEnd.X);
  end;

  if Value < 0 then
  begin
    Inc(FDist, FDelta.X);
    Inc(FDistStart, FDeltaStart.X);
    Inc(FDistPict, FDeltaPict.X);
    Inc(FDistEnd, FDeltaEnd.X);
  end;
end;

procedure TAggDistanceInterpolator4.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistPict, FDeltaPict.Y);
  Dec(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
  begin
    Dec(FDist, FDelta.X);
    Dec(FDistStart, FDeltaStart.X);
    Dec(FDistPict, FDeltaPict.X);
    Dec(FDistEnd, FDeltaEnd.X);
  end;

  if Value < 0 then
  begin
    Inc(FDist, FDelta.X);
    Inc(FDistStart, FDeltaStart.X);
    Inc(FDistPict, FDeltaPict.X);
    Inc(FDistEnd, FDeltaEnd.X);
  end;
end;

procedure TAggDistanceInterpolator4.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistPict, FDeltaPict.X);
  Dec(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
  begin
    Inc(FDist, FDelta.Y);
    Inc(FDistStart, FDeltaStart.Y);
    Inc(FDistPict, FDeltaPict.Y);
    Inc(FDistEnd, FDeltaEnd.Y);
  end;

  if Value < 0 then
  begin
    Dec(FDist, FDelta.Y);
    Dec(FDistStart, FDeltaStart.Y);
    Dec(FDistPict, FDeltaPict.Y);
    Dec(FDistEnd, FDeltaEnd.Y);
  end;
end;

procedure TAggDistanceInterpolator4.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistPict, FDeltaPict.X);
  Inc(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
  begin
    Inc(FDist, FDelta.Y);
    Inc(FDistStart, FDeltaStart.Y);
    Inc(FDistPict, FDeltaPict.Y);
    Inc(FDistEnd, FDeltaEnd.Y);
  end;

  if Value < 0 then
  begin
    Dec(FDist, FDelta.Y);
    Dec(FDistStart, FDeltaStart.Y);
    Dec(FDistPict, FDeltaPict.Y);
    Dec(FDistEnd, FDeltaEnd.Y);
  end;
end;

function TAggDistanceInterpolator4.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator4.GetDistanceStart;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator4.GetDistPict;
begin
  Result := FDistPict;
end;

function TAggDistanceInterpolator4.GetDistanceEnd;
begin
  Result := FDistEnd;
end;

function TAggDistanceInterpolator4.GetDeltaX: Integer;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator4.GetDeltaY: Integer;
begin
  Result := FDelta.Y;
end;

function TAggDistanceInterpolator4.GetDeltaXStart: Integer;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator4.GetDeltaYStart: Integer;
begin
  Result := FDeltaStart.Y;
end;

function TAggDistanceInterpolator4.GetDeltaXEnd: Integer;
begin
  Result := FDeltaEnd.X;
end;

function TAggDistanceInterpolator4.GetDeltaYEnd: Integer;
begin
  Result := FDeltaEnd.Y;
end;


{ TAggLineInterpolatorImage }

constructor TAggLineInterpolatorImage.Create(Ren: TAggRendererOutlineImage;
  Lp: PAggLineParameters; Sx, Sy, Ex, Ey, PatternStart: Integer;
  ScaleX: Double);
var
  I: Cardinal;
  Delta: TPointInteger;
  DistStart: array [0..1] of Integer;
  Stop, Npix : Integer;
  Li: TAggDda2LineInterpolator;
begin
  FLineParameters := Lp;

  if Lp.Vertical then
    FLineInterpolator.Initialize(LineDoubleHighResolution(Lp.X2 - Lp.X1),
      Abs(Lp.Y2 - Lp.Y1))
  else
    FLineInterpolator.Initialize(LineDoubleHighResolution(Lp.Y2 - Lp.Y1),
      Abs(Lp.X2 - Lp.X1) + 1);

  FDistanceInterpolator := TAggDistanceInterpolator4.Create(Lp.X1, Lp.Y1, Lp.X2,
    Lp.Y2, Sx, Sy, Ex, Ey, Lp.Len, ScaleX, Lp.X1 and not CAggLineSubpixelMask,
    Lp.Y1 and not CAggLineSubpixelMask);

  FRendererOutlineImage := Ren;

  FX := ShrInt32(Lp.X1, CAggLineSubpixelShift);
  FY := ShrInt32(Lp.Y1, CAggLineSubpixelShift);

  FOldX := FX;
  FOldY := FY;

  if Lp.Vertical then
    FCount := Abs(ShrInt32(Lp.Y2, CAggLineSubpixelShift) - FY)
  else
    FCount := Abs(ShrInt32(Lp.X2, CAggLineSubpixelShift) - FX);

  FWidth := Ren.GetSubpixelWidth;
  FMaxExtent := ShrInt32(FWidth, CAggLineSubpixelShift - 2);

  try
    FStart := PatternStart + (FMaxExtent + 2) * Ren.GetPatternWidth;
  except
    FStart := 0 + (FMaxExtent + 2) * Ren.GetPatternWidth;
  end;

  FStep := 0;

  if Lp.Vertical then
    Li.Initialize(0, Lp.Delta.Y shl CAggLineSubpixelShift, Lp.Len)
  else
    Li.Initialize(0, Lp.Delta.X shl CAggLineSubpixelShift, Lp.Len);

  Stop := FWidth + CAggLineSubpixelSize * 2;
  I := 0;

  while I < CMaxHalfWidth do
  begin
    FDistPos[I] := Li.Y;

    if FDistPos[I] >= Stop then
      Break;

    Li.PlusOperator;

    Inc(I);
  end;

  FDistPos[I] := $7FFF0000;

  Npix := 1;

  if Lp.Vertical then
    repeat
      FLineInterpolator.MinusOperator;

      Dec(FY, Lp.IncValue);

      FX := ShrInt32(FLineParameters.X1 + FLineInterpolator.Y,
        CAggLineSubpixelShift);

      if Lp.IncValue > 0 then
        FDistanceInterpolator.SetDecY(FX - FOldX)
      else
        FDistanceInterpolator.SetIncY(FX - FOldX);

      FOldX := FX;

      DistStart[0] := FDistanceInterpolator.GetDistanceStart;
      DistStart[1] := DistStart[0];

      Delta.X := 0;

      if DistStart[0] < 0 then
        Inc(Npix);

      repeat
        Inc(DistStart[0], FDistanceInterpolator.DyStart);
        Dec(DistStart[1], FDistanceInterpolator.DyStart);

        if DistStart[0] < 0 then
          Inc(Npix);

        if DistStart[1] < 0 then
          Inc(Npix);

        Inc(Delta.X);

      until FDistPos[Delta.X] > FWidth;

      if Npix = 0 then
        Break;

      Npix := 0;

      Dec(FStep);

    until FStep < -FMaxExtent
  else
    repeat
      FLineInterpolator.MinusOperator;

      Dec(FX, Lp.IncValue);

      FY := ShrInt32(FLineParameters.Y1 + FLineInterpolator.Y,
        CAggLineSubpixelShift);

      if Lp.IncValue > 0 then
        FDistanceInterpolator.SetDecX(FY - FOldY)
      else
        FDistanceInterpolator.SetIncX(FY - FOldY);

      FOldY := FY;

      DistStart[0] := FDistanceInterpolator.GetDistanceStart;
      DistStart[1] := DistStart[0];

      Delta.Y := 0;

      if DistStart[0] < 0 then
        Inc(Npix);

      repeat
        Dec(DistStart[0], FDistanceInterpolator.DxStart);
        Inc(DistStart[1], FDistanceInterpolator.DxStart);

        if DistStart[0] < 0 then
          Inc(Npix);

        if DistStart[1] < 0 then
          Inc(Npix);

        Inc(Delta.Y);

      until FDistPos[Delta.Y] > FWidth;

      if Npix = 0 then
        Break;

      Npix := 0;

      Dec(FStep);

    until FStep < -FMaxExtent;

  FLineInterpolator.AdjustForward;

  Dec(FStep, FMaxExtent);
end;

destructor TAggLineInterpolatorImage.Destroy;
begin
  FDistanceInterpolator.Free;

  inherited;
end;

function TAggLineInterpolatorImage.StepHorizontal: Boolean;
var
  S1, S2, DistanceStart, DistPict, DistanceEnd, DeltaY, Distance, Npix: Integer;
  P0, P1: PAggColor;
begin
  FLineInterpolator.PlusOperator;

  Inc(FX, FLineParameters.IncValue);

  FY := ShrInt32(FLineParameters.Y1 + FLineInterpolator.Y,
    CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
    FDistanceInterpolator.SetIncX(FY - FOldY)
  else
    FDistanceInterpolator.SetDecX(FY - FOldY);

  FOldY := FY;

  S1 := FDistanceInterpolator.GetDistance div FLineParameters.Len;
  S2 := -S1;

  if FLineParameters.IncValue < 0 then
    S1 := -S1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  P0 := PAggColor(PtrComp(@FColors[0]) + (CMaxHalfWidth + 2) *
    SizeOf(TAggColor));
  P1 := P0;

  Npix := 0;

  P1.Clear;

  if DistanceEnd > 0 then
  begin
    if DistanceStart <= 0 then
      FRendererOutlineImage.Pixel(P1, DistPict, S2);

    Inc(Npix);
  end;

  Inc(PtrComp(P1), SizeOf(TAggColor));

  DeltaY := 1;
  Distance := FDistPos[DeltaY];

  while Distance - S1 <= FWidth do
  begin
    Dec(DistanceStart, FDistanceInterpolator.DxStart);
    Dec(DistPict, FDistanceInterpolator.DxPict);
    Dec(DistanceEnd, FDistanceInterpolator.DxEnd);

    P1.Clear;

    if (DistanceEnd > 0) and (DistanceStart <= 0) then
    begin
      if FLineParameters.IncValue > 0 then
        Distance := -Distance;

      FRendererOutlineImage.Pixel(P1, DistPict, S2 - Distance);

      Inc(Npix);
    end;

    Inc(PtrComp(P1), SizeOf(TAggColor));
    Inc(DeltaY);

    Distance := FDistPos[DeltaY];
  end;

  DeltaY := 1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  Distance := FDistPos[DeltaY];

  while Distance + S1 <= FWidth do
  begin
    Inc(DistanceStart, FDistanceInterpolator.DxStart);
    Inc(DistPict, FDistanceInterpolator.DxPict);
    Inc(DistanceEnd, FDistanceInterpolator.DxEnd);

    Dec(PtrComp(P0), SizeOf(TAggColor));

    P0.Clear;

    if (DistanceEnd > 0) and (DistanceStart <= 0) then
    begin
      if FLineParameters.IncValue > 0 then
        Distance := -Distance;

      FRendererOutlineImage.Pixel(P0, DistPict, S2 + Distance);

      Inc(Npix);
    end;

    Inc(DeltaY);

    Distance := FDistPos[DeltaY];
  end;

  FRendererOutlineImage.BlendColorVSpan(FX, FY - DeltaY + 1,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(TAggColor)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorImage.StepVertical;
var
  S1, S2, DistanceStart, DistPict, DistanceEnd, DeltaX, Distance, Npix: Integer;
  P0, P1: PAggColor;
begin
  FLineInterpolator.PlusOperator;

  Inc(FY, FLineParameters.IncValue);

  FX := ShrInt32(FLineParameters.X1 + FLineInterpolator.Y,
    CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
    FDistanceInterpolator.SetIncY(FX - FOldX)
  else
    FDistanceInterpolator.SetDecY(FX - FOldX);

  FOldX := FX;

  S1 := FDistanceInterpolator.GetDistance div FLineParameters.Len;
  S2 := -S1;

  if FLineParameters.IncValue > 0 then
    S1 := -S1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  P0 := PAggColor(PtrComp(@FColors[0]) + (CMaxHalfWidth + 2) *
    SizeOf(TAggColor));
  P1 := P0;

  Npix := 0;

  P1.Clear;

  if DistanceEnd > 0 then
  begin
    if DistanceStart <= 0 then
      FRendererOutlineImage.Pixel(P1, DistPict, S2);

    Inc(Npix);
  end;

  Inc(PtrComp(P1), SizeOf(TAggColor));

  DeltaX := 1;
  Distance := FDistPos[DeltaX];

  while Distance - S1 <= FWidth do
  begin
    Inc(DistanceStart, FDistanceInterpolator.DyStart);
    Inc(DistPict, FDistanceInterpolator.DyPict);
    Inc(DistanceEnd, FDistanceInterpolator.DyEnd);

    P1.Clear;

    if (DistanceEnd > 0) and (DistanceStart <= 0) then
    begin
      if FLineParameters.IncValue > 0 then
        Distance := -Distance;

      FRendererOutlineImage.Pixel(P1, DistPict, S2 + Distance);

      Inc(Npix);
    end;

    Inc(PtrComp(P1), SizeOf(TAggColor));
    Inc(DeltaX);

    Distance := FDistPos[DeltaX];
  end;

  DeltaX := 1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  Distance := FDistPos[DeltaX];

  while Distance + S1 <= FWidth do
  begin
    Dec(DistanceStart, FDistanceInterpolator.DyStart);
    Dec(DistPict, FDistanceInterpolator.DyPict);
    Dec(DistanceEnd, FDistanceInterpolator.DyEnd);

    Dec(PtrComp(P0), SizeOf(TAggColor));

    P0.Clear;

    if (DistanceEnd > 0) and (DistanceStart <= 0) then
    begin
      if FLineParameters.IncValue > 0 then
        Distance := -Distance;

      FRendererOutlineImage.Pixel(P0, DistPict, S2 - Distance);

      Inc(Npix);
    end;

    Inc(DeltaX);

    Distance := FDistPos[DeltaX];
  end;

  FRendererOutlineImage.BlendColorHSpan(FX - DeltaX + 1, FY,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(TAggColor)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorImage.GetPatternEnd;
begin
  Result := FStart + FDistanceInterpolator.Length;
end;

function TAggLineInterpolatorImage.GetVertical;
begin
  Result := FLineParameters.Vertical;
end;


{ TAggRendererOutlineImage }

constructor TAggRendererOutlineImage.Create(Ren: TAggRendererBase;
  Patt: TAggLineImagePattern);
begin
  Assert(Ren is TAggRendererBase);
  FRendererOutlineImage := Ren;
  FPattern := Patt;
  FStart := 0;
  FScaleX := 1.0;
end;

procedure TAggRendererOutlineImage.SetPattern;
begin
  FPattern := P;
end;

procedure TAggRendererOutlineImage.SetScaleX;
begin
  FScaleX := S;
end;

function TAggRendererOutlineImage.GetScaleX;
begin
  Result := FScaleX;
end;

procedure TAggRendererOutlineImage.SetStartX;
begin
  FStart := Trunc(S * CAggLineSubpixelSize);
end;

function TAggRendererOutlineImage.GetStartX;
begin
  Result := FStart / CAggLineSubpixelSize;
end;

function TAggRendererOutlineImage.GetSubpixelWidth;
begin
  Result := FPattern.LineWidth;
end;

function TAggRendererOutlineImage.GetPatternWidth;
begin
  Result := FPattern.PatternWidth;
end;

procedure TAggRendererOutlineImage.Pixel;
begin
  FPattern.Pixel(P, X, Y);
end;

procedure TAggRendererOutlineImage.BlendColorHSpan;
begin
  FRendererOutlineImage.BlendColorHSpan(X, Y, Len, Colors, nil);
end;

procedure TAggRendererOutlineImage.BlendColorVSpan;
begin
  FRendererOutlineImage.BlendColorVSpan(X, Y, Len, Colors, nil);
end;

function TAggRendererOutlineImage.GetAccurateJoinOnly;
begin
  Result := True;
end;

procedure TAggRendererOutlineImage.SemiDot;
begin
end;

procedure TAggRendererOutlineImage.Line0;
begin
end;

procedure TAggRendererOutlineImage.Line1;
begin
end;

procedure TAggRendererOutlineImage.Line2;
begin
end;

procedure TAggRendererOutlineImage.Line3(Lp: PAggLineParameters; Sx, Sy,
  Ex, Ey: Integer);
var
  Li: TAggLineInterpolatorImage;
begin
  FixDegenerateBisectrixStart(Lp, @Sx, @Sy);
  FixDegenerateBisectrixEnd(Lp, @Ex, @Ey);

  Li := TAggLineInterpolatorImage.Create(Self, Lp, Sx, Sy, Ex, Ey, FStart,
    FScaleX);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;

    FStart := Li.PatternEnd;
  finally
    Li.Free;
  end;
end;

end.
