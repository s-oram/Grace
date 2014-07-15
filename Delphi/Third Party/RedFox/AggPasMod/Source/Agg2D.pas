unit Agg2D;

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

// With this define uncommented you can use FreeType font engine
{-$DEFINE AGG2D_USE_FREETYPE }

uses
  AggBasics,
  AggMath,
  AggArray,
  AggTransAffine,
  AggTransViewport,
  AggPathStorage,
  AggConvStroke,
  AggConvDash,
  AggConvTransform,
  AggConvCurve,
  AggRenderingBuffer,
  AggRendererBase,
  AggRendererScanLine,
  AggSpanGradient,
  AggSpanImageFilterRgba,
  AggSpanImageResampleRgba,
  AggSpanConverter,
  AggSpanInterpolatorLinear,
  AggSpanAllocator,
  AggRasterizerScanLineAA,
  AggGammaFunctions,
  AggScanlineUnpacked,
  AggArc,
  AggBezierArc,
  AggRoundedRect,
  AggFontEngine,
  AggFontCacheManager,
  AggPixelFormat,
  AggPixelFormatRgba,
  AggColor,
  AggMathStroke,
  AggImageFilters,
  AggVertexSource,
  AggRenderScanLines,

{$IFDEF AGG2D_USE_FREETYPE}
  AggFontFreeType,
{$ELSE}
  AggFontWin32TrueType,
  Windows,
{$ENDIF}

  Math;

type
  PAggColorRgba8 = ^TAggColorRgba8;
  TAggColorRgba8 = TAggRgba8;

  TAggFontRasterizer = TAggGray8Adaptor;

  TAggFontScanLine = TAggGray8ScanLine;

{$IFDEF AGG2D_USE_FREETYPE }
  TAggFontEngine = TAggFontEngineFreetypeInt32;
{$ELSE }
  TAggFontEngine = TAggFontEngineWin32TrueTypeInt32;
{$ENDIF}

  TAggGradient = (grdSolid, grdLinear, grdRadial);
  TAggDirection = (dirCW, dirCCW);

  TAggTextAlignmentHorizontal = (tahLeft, tahRight, tahCenter);
  TAggTextAlignmentVertical = (tavTop, tavBottom, tavCenter);

  TAggDrawPathFlag = (dpfFillOnly, dpfStrokeOnly, dpfFillAndStroke,
    dpfFillWithHorizontalLineColor);

  TAggViewportOption = (voAnisotropic, voXMinYMin, voXMidYMin, voXMaxYMin,
    voXMinYMid, voXMidYMid, voXMaxYMid, voXMinYMax, voXMidYMax, voXMaxYMax);

  TAggImageFilterType = (ifNoFilter, ifBilinear, ifHanning, ifHermite, ifQuadric,
    ifBicubic, ifCatrom, ifSpline16, ifSpline36, ifBlackman144);

  TAggImageResample = (irNever, irAlways, irOnZoomOut);

  TAggFontCache = (fcRaster, fcVector);

  PAggTransformations = ^TAggTransformations;
  TAggTransformations = record
    AffineMatrix: TAggParallelogram;
  end;

  TAgg2DImage = class
  private
    FRenderingBuffer: TAggRenderingBuffer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetScanLine(Index: Cardinal): Pointer;
  public
    constructor Create(Buffer: PInt8u; AWidth, AHeight: Cardinal;
      Stride: Integer);
    destructor Destroy; override;

    procedure Attach(Buffer: PInt8u; AWidth, AHeight: Cardinal;
      Stride: Integer);

    procedure PreMultiply;
    procedure DeMultiply;

    property ScanLine[Index: Cardinal]: Pointer read GetScanLine;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  TAgg2DRasterizerGamma = class(TAggVertexSource)
  private
    FAlpha: TAggGammaMultiply;
    FGamma: TAggGammaPower;
  public
    constructor Create(Alpha, Gamma: Double);
    destructor Destroy; override;

    function FuncOperatorGamma(X: Double): Double; override;
  end;

  TAgg2D = class
  private
    FRenderingBuffer: TAggRenderingBuffer;

    FRendererBase: TAggRendererBase;
    FRendererBaseComp: TAggRendererBase;
    FRendererBasePre: TAggRendererBase;
    FRendererBaseCompPre: TAggRendererBase;

    FRendererSolid: TAggRendererScanLineAASolid;
    FRendererSolidComp: TAggRendererScanLineAASolid;

    FAllocator: TAggSpanAllocator;
    FClipBox: TRectDouble;

    FBlendMode: TAggBlendMode;
    FImageBlendMode: TAggBlendMode;

    FScanLine: TAggScanLineUnpacked8;
    FRasterizer: TAggRasterizerScanLineAA;

    FMasterAlpha: Double;
    FAntiAliasGamma: Double;

    FFillGradient: TAggPodAutoArray;
    FLineGradient: TAggPodAutoArray;

    FLineCap: TAggLineCap;
    FLineJoin: TAggLineJoin;

    FFillGradientFlag: TAggGradient;
    FLineGradientFlag: TAggGradient;

    FFillGradientMatrix: TAggTransAffine;
    FLineGradientMatrix: TAggTransAffine;

    FFillGradientD1: Double;
    FLineGradientD1: Double;
    FFillGradientD2: Double;
    FLineGradientD2: Double;

    FTextAngle: Double;

    FTextAlignX: TAggTextAlignmentHorizontal;
    FTextAlignY: TAggTextAlignmentVertical;

    FTextHints: Boolean;
    FFontHeight: Double;
    FFontAscent: Double;
    FFontDescent: Double;
    FFontCacheType: TAggFontCache;

    FImageFilter: TAggImageFilterType;
    FImageResample: TAggImageResample;
    FImageFilterLUT: TAggImageFilter;

    FFillGradientInterpolator: TAggSpanInterpolatorLinear;
    FLineGradientInterpolator: TAggSpanInterpolatorLinear;

    FLinearGradientFunction: TAggGradientX;
    FRadialGradientFunction: TAggGradientCircle;

    FLineWidth: Double;
    FEvenOddFlag: Boolean;

    FPath: TAggPathStorage;
    FTransform: TAggTransAffine;

    FConvCurve : TAggConvCurve;
    FConvDash: TAggConvDash;
    FConvStroke: TAggConvStroke;

    FPathTransform: TAggConvTransform;
    FStrokeTransform: TAggConvTransform;

{$IFNDEF AGG2D_USE_FREETYPE}
    FFontDC: HDC;
{$ENDIF}

    FFontEngine: TAggFontEngine;
    FFontCacheManager: TAggFontCacheManager;

    // Other Pascal-specific members
    FGammaNone: TAggGammaNone;
    FGammaAgg2D: TAgg2DRasterizerGamma;

    FImageFilterBilinear: TAggImageFilterBilinear;
    FImageFilterHanning: TAggImageFilterHanning;
    FImageFilterHermite: TAggImageFilterHermite;
    FImageFilterQuadric: TAggImageFilterQuadric;
    FImageFilterBicubic: TAggImageFilterBicubic;
    FImageFilterCatrom: TAggImageFilterCatrom;
    FImageFilterSpline16: TAggImageFilterSpline16;
    FImageFilterSpline36: TAggImageFilterSpline36;
    FImageFilterBlackman144: TAggImageFilterBlackman144;

    procedure Render(AFillColor: Boolean); overload;
    procedure Render(Ras: TAggFontRasterizer; Sl: TAggFontScanLine); overload;

    procedure AddLine(X1, Y1, X2, Y2: Double);
    procedure AddEllipse(Cx, Cy, Rx, Ry: Double; Dir: TAggDirection); overload;
    procedure AddEllipse(Center, Radius: TPointDouble; Dir: TAggDirection);
      overload;

    procedure RenderImage(Img: TAgg2DImage; X1, Y1, X2, Y2: Integer;
      Parl: PDouble); overload;
    procedure RenderImage(Img: TAgg2DImage; Rect: TRectInteger;
      Parl: PDouble); overload;
    procedure SetImageFilter(F: TAggImageFilterType);

    function GetFlipText: Boolean;

    procedure SetImageResample(F: TAggImageResample); overload;
    procedure SetTextHints(Value: Boolean); overload;
    procedure SetLineCap(Cap: TAggLineCap); overload;
    procedure SetLineJoin(Join: TAggLineJoin); overload;
    procedure SetFillEvenOdd(EvenOddFlag: Boolean); overload;
    procedure SetBlendMode(Value: TAggBlendMode);
    procedure SetImageBlendMode(Value: TAggBlendMode);
    procedure SetFillColor(C: TAggColorRgba8); overload;
    procedure SetLineColor(C: TAggColorRgba8); overload;
    procedure SetImageBlendColor(C: TAggColorRgba8); overload;
    procedure SetMasterAlpha(A: Double); overload;
    procedure SetAntiAliasGamma(G: Double);
    procedure SetLineWidth(W: Double);
    procedure SetFlipText(Value: Boolean);
    function GetRow(Y: Cardinal): PInt8U;
  protected
    FImageBlendColor: TAggColorRgba8;
    FFillColor: TAggColorRgba8;
    FLineColor: TAggColorRgba8;

    FPixelFormat: TAggPixelFormatProcessor;
    FPixelFormatComp: TAggPixelFormatProcessor;
    FPixelFormatPre: TAggPixelFormatProcessor;
    FPixelFormatCompPre: TAggPixelFormatProcessor;

    procedure UpdateApproximationScale;
    procedure UpdateRasterizerGamma;

    property RenderingBuffer: TAggRenderingBuffer read FRenderingBuffer;
    property RendererBase: TAggRendererBase read FRendererBase;
    property RendererBaseComp: TAggRendererBase read FRendererBaseComp;
    property RendererBasePre: TAggRendererBase read FRendererBasePre;
    property RendererBaseCompPre: TAggRendererBase read FRendererBaseCompPre;
    property Rasterizer: TAggRasterizerScanLineAA read FRasterizer;
    property Path: TAggPathStorage read FPath;
  public
    constructor Create; overload; virtual;
    constructor Create(Buffer: PInt8u; Width, Height: Cardinal;
      Stride: Integer); overload; virtual;
    destructor Destroy; override;

    // Setup
    procedure Attach(Buffer: PInt8u; Width, Height: Cardinal;
      Stride: Integer); overload;
    procedure Attach(Img: TAgg2DImage); overload;

    procedure ClipBox(X1, Y1, X2, Y2: Double); overload;
    function ClipBox: TRectDouble; overload;

    procedure ClearAll(C: TAggColorRgba8); overload;
    procedure ClearAll(R, G, B: Cardinal; A: Cardinal = 255); overload;

    procedure ClearClipBox(C: TAggColorRgba8); overload;
    procedure ClearClipBox(R, G, B: Cardinal; A: Cardinal = 255); overload;

    // Conversions
    procedure WorldToScreen(X, Y: PDouble); overload;
    procedure WorldToScreen(var X, Y: Double); overload;
    procedure ScreenToWorld(X, Y: PDouble); overload;
    procedure ScreenToWorld(var X, Y: Double); overload;
    function WorldToScreen(Scalar: Double): Double; overload;
    function ScreenToWorld(Scalar: Double): Double; overload;

    procedure AlignPoint(X, Y: PDouble); overload;
    procedure AlignPoint(var X, Y: Double); overload;

    function InBox(WorldX, WorldY: Double): Boolean; overload;
    function InBox(World: TPointDouble): Boolean; overload;

    // General Attributes
    procedure SetFillColor(R, G, B: Cardinal; A: Cardinal = 255); overload;
    procedure SetLineColor(R, G, B: Cardinal; A: Cardinal = 255); overload;
    procedure SetImageBlendColor(R, G, B: Cardinal; A: Cardinal = 255); overload;
    procedure NoFill;
    procedure NoLine;

    procedure FillLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TAggColorRgba8;
      Profile: Double = 1);
    procedure LineLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TAggColorRgba8;
      Profile: Double = 1);

    procedure FillRadialGradient(X, Y, R: Double; C1, C2: TAggColorRgba8;
      Profile: Double = 1); overload;
    procedure LineRadialGradient(X, Y, R: Double; C1, C2: TAggColorRgba8;
      Profile: Double = 1); overload;

    procedure FillRadialGradient(X, Y, R: Double; C1, C2, C3: TAggColorRgba8); overload;
    procedure LineRadialGradient(X, Y, R: Double; C1, C2, C3: TAggColorRgba8); overload;

    procedure FillRadialGradient(X, Y, R: Double); overload;
    procedure LineRadialGradient(X, Y, R: Double); overload;

    procedure RemoveAllDashes;
    procedure AddDash(DashLength, GapLength: Double);

    // Transformations
    function GetTransformations: TAggTransformations;
    procedure SetTransformations(var Tr: TAggTransformations); overload;
    procedure SetTransformations(V0, V1, V2, V3, V4, V5: Double); overload;
    procedure ResetTransformations;

    procedure Affine(Tr: TAggTransAffine); overload;
    procedure Affine(var Tr: TAggTransformations); overload;

    procedure Rotate(Angle: Double);
    procedure Scale(Sx, Sy: Double);
    procedure Skew(Sx, Sy: Double);
    procedure Translate(X, Y: Double);

    procedure Parallelogram(X1, Y1, X2, Y2: Double; Para: PDouble);

    procedure Viewport(WorldX1, WorldY1, WorldX2, WorldY2, ScreenX1, ScreenY1,
      ScreenX2, ScreenY2: Double; Opt: TAggViewportOption = voXMidYMid); overload;
    procedure Viewport(World, Screen: TRectDouble; Opt: TAggViewportOption =
      voXMidYMid); overload;

    // Basic Shapes
    procedure Line(X1, Y1, X2, Y2: Double);
    procedure Triangle(X1, Y1, X2, Y2, X3, Y3: Double);
    procedure Rectangle(X1, Y1, X2, Y2: Double);

    procedure RoundedRect(X1, Y1, X2, Y2, R: Double); overload;
    procedure RoundedRect(Rect: TRectDouble; R: Double); overload;
    procedure RoundedRect(X1, Y1, X2, Y2, Rx, Ry: Double); overload;
    procedure RoundedRect(Rect: TRectDouble; Rx, Ry: Double); overload;
    procedure RoundedRect(X1, Y1, X2, Y2, RxBottom, RyBottom, RxTop,
      RyTop: Double); overload;

    procedure Ellipse(Cx, Cy, Rx, Ry: Double);
    procedure Circle(Cx, Cy, Radius: Double);

    procedure Arc(Cx, Cy, Rx, Ry, Start, Sweep: Double);
    procedure Star(Cx, Cy, R1, R2, StartAngle: Double; NumRays: Integer);

    procedure Curve(X1, Y1, X2, Y2, X3, Y3: Double); overload;
    procedure Curve(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double); overload;

    procedure Polygon(Xy: PPointDouble; NumPoints: Integer;
      Flag: TAggDrawPathFlag = dpfFillAndStroke);
    procedure Polyline(Xy: PPointDouble; NumPoints: Integer);

    // Text
    procedure Font(FileName: PAnsiChar; Height: Double; Bold: Boolean = False;
      Italic: Boolean = False; Ch: TAggFontCache = fcRaster; Angle: Double = 0);

    function FontHeight: Double;

    procedure TextAlignment(AlignX: TAggTextAlignmentHorizontal;
      AlignY: TAggTextAlignmentVertical);

    function TextWidth(Str: PAnsiChar): Double; overload;
    function TextWidth(Str: AnsiString): Double; overload;

    procedure Text(X, Y: Double; Str: PAnsiChar; RoundOff: Boolean = False;
      Ddx: Double = 0; Ddy: Double = 0); overload;
    procedure Text(X, Y: Double; Str: AnsiString; RoundOff: Boolean = False;
      Ddx: Double = 0; Ddy: Double = 0); overload;

    // Path commands
    procedure ResetPath;

    procedure MoveTo(X, Y: Double);
    procedure MoveRel(Dx, Dy: Double);

    procedure LineTo(X, Y: Double);
    procedure LineRel(Dx, Dy: Double);

    procedure HorizontalLineTo(X: Double);
    procedure HorizontalLineRel(Dx: Double);

    procedure VerticalLineTo(Y: Double);
    procedure VerticalLineRel(Dy: Double);

    procedure ArcTo(Rx, Ry, Angle: Double; LargeArcFlag, SweepFlag: Boolean;
      X, Y: Double);
    procedure ArcRel(Rx, Ry, Angle: Double; LargeArcFlag, SweepFlag: Boolean;
      Dx, Dy: Double);

    procedure QuadricCurveTo(XCtrl, YCtrl, XTo, YTo: Double); overload;
    procedure QuadricCurveRel(DxCtrl, DyCtrl, DxTo, DyTo: Double); overload;
    procedure QuadricCurveTo(XTo, YTo: Double); overload;
    procedure QuadricCurveRel(DxTo, DyTo: Double); overload;

    procedure CubicCurveTo(XCtrl1, YCtrl1, XCtrl2, YCtrl2, XTo,
      YTo: Double); overload;
    procedure CubicCurveRel(DxCtrl1, DyCtrl1, DxCtrl2, DyCtrl2, DxTo,
      DyTo: Double); overload;
    procedure CubicCurveTo(XCtrl2, YCtrl2, XTo, YTo: Double); overload;
    procedure CubicCurveRel(XCtrl2, YCtrl2, XTo, YTo: Double); overload;

    procedure ClosePolygon;

    procedure DrawPath(Flag: TAggDrawPathFlag = dpfFillAndStroke);
    procedure DrawPathNoTransform(Flag: TAggDrawPathFlag = dpfFillAndStroke);

    // Image Transformations
    procedure TransformImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2,
      ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImage(Img: TAgg2DImage; ImgRect: TRectInteger;
      DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImage(Img: TAgg2DImage; ImgRect: TRectInteger;
      Destination: TRectDouble); overload;

    procedure TransformImage(Img: TAgg2DImage;
      DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImage(Img: TAgg2DImage;
      Destination: TRectDouble); overload;

    procedure TransformImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2,
      ImgY2: Integer; Parallelogram: PDouble); overload;
    procedure TransformImage(Img: TAgg2DImage; ImgRect: TRectInteger;
      Parallelogram: PDouble); overload;

    procedure TransformImage(Img: TAgg2DImage; Parallelogram: PDouble); overload;

    procedure TransformImagePath(Img: TAgg2DImage;
      ImgX1, ImgY1, ImgX2, ImgY2: Integer;
      DstX1, DstY1, DstX2, DstY2: Double); overload;

    procedure TransformImagePath(Img: TAgg2DImage;
      DstX1, DstY1, DstX2, DstY2: Double); overload;

    procedure TransformImagePath(Img: TAgg2DImage;
      ImgX1, ImgY1, ImgX2, ImgY2: Integer; Parallelogram: PDouble); overload;

    procedure TransformImagePath(Img: TAgg2DImage;
      Parallelogram: PDouble); overload;

    // Image Blending (no transformations available)
    procedure BlendImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer;
      DstX, DstY: Double; Alpha: Cardinal = 255); overload;

    procedure BlendImage(Img: TAgg2DImage; DstX, DstY: Double;
      Alpha: Cardinal = 255); overload;

    // Copy image directly, together with alpha-channel
    procedure CopyImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer;
      DstX, DstY: Double); overload;
    procedure CopyImage(Img: TAgg2DImage; ImgRect: TRectInteger;
      Destination: TPointDouble); overload;

    procedure CopyImage(Img: TAgg2DImage; DstX, DstY: Double); overload;
    procedure CopyImage(Img: TAgg2DImage; Destination: TPointDouble); overload;

    property AntiAliasGamma: Double read FAntiAliasGamma write SetAntiAliasGamma;
    property ImageBlendColor: TAggColorRgba8 read FImageBlendColor write SetImageBlendColor;
    property FillColor: TAggColorRgba8 read FFillColor write SetFillColor;
    property LineColor: TAggColorRgba8 read FLineColor write SetLineColor;
    property ImageFilter: TAggImageFilterType read FImageFilter write SetImageFilter;
    property BlendMode: TAggBlendMode read FBlendMode write SetBlendMode;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property LineJoin: TAggLineJoin read FLineJoin write SetLineJoin;
    property LineCap: TAggLineCap read FLineCap write SetLineCap;
    property FillEvenOdd: Boolean read FEvenOddFlag write SetFillEvenOdd;
    property FlipText: Boolean read GetFlipText write SetFlipText;
    property TextHints: Boolean read FTextHints write SetTextHints;
    property ImageResample: TAggImageResample read FImageResample write SetImageResample;
    property Row[Y: Cardinal]: PInt8U read GetRow;

    property ImageBlendMode: TAggBlendMode read FImageBlendMode write SetImageBlendMode;
    property MasterAlpha: Double read FMasterAlpha write SetMasterAlpha;
  end;

  TAggSpanConvImageBlend = class(TAggSpanConvertor)
  private
    FMode : TAggBlendMode;
    FColor: TAggColorRgba8;
    FPixel: TAggPixelFormatProcessor; // FPixelFormatCompPre
  public
    constructor Create(BlendMode: TAggBlendMode; C: TAggColorRgba8; P: TAggPixelFormatProcessor);

    procedure Convert(Span: PAggColor; X, Y: Integer; Len: Cardinal); override;
  end;

function OperatorIsEqual(C1, C2: PAggColorRgba8): Boolean;
function OperatorIsNotEqual(C1, C2: PAggColorRgba8): Boolean;

procedure Agg2DRendererRender(Gr: TAgg2D; RendererBase: TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid; FillColor: Boolean); overload;

procedure Agg2DRendererRender(Gr: TAgg2D; RendererBase: TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid; Ras: TAggGray8Adaptor;
  Sl: TAggGray8ScanLine); overload;

procedure Agg2DRendererRenderImage(Gr: TAgg2D; Img: TAgg2DImage;
  RendererBase: TAggRendererBase; Interpolator: TAggSpanInterpolatorLinear);

function Agg2DUsesFreeType: Boolean;

implementation

var
  GApproxScale: Double = 2;


{ TAgg2DImage }

constructor TAgg2DImage.Create(Buffer: PInt8u; AWidth, AHeight: Cardinal;
  Stride: Integer);
begin
  FRenderingBuffer := TAggRenderingBuffer.Create(Buffer, AWidth, AHeight, Stride);
end;

destructor TAgg2DImage.Destroy;
begin
  FRenderingBuffer.Free;
  inherited;
end;

procedure TAgg2DImage.Attach(Buffer: PInt8u; AWidth, AHeight: Cardinal; Stride: Integer);
begin
  FRenderingBuffer.Attach(Buffer, AWidth, AHeight, Stride);
end;

function TAgg2DImage.GetWidth: Integer;
begin
  Result := FRenderingBuffer.Width;
end;

function TAgg2DImage.GetHeight: Integer;
begin
  Result := FRenderingBuffer.Height;
end;

function TAgg2DImage.GetScanLine(Index: Cardinal): Pointer;
begin
  Result := FRenderingBuffer.Row(Index)
end;

procedure TAgg2DImage.PreMultiply;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
begin
  PixelFormatRgba32(PixelFormatProcessor, FRenderingBuffer);
  PixelFormatProcessor.PreMultiply;
end;

procedure TAgg2DImage.DeMultiply;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
begin
  PixelFormatRgba32(PixelFormatProcessor, FRenderingBuffer);
  PixelFormatProcessor.DeMultiply;
end;


{ TAgg2DRasterizerGamma }

constructor TAgg2DRasterizerGamma.Create(Alpha, Gamma: Double);
begin
  FAlpha := TAggGammaMultiply.Create(Alpha);
  FGamma := TAggGammaPower.Create(Gamma);
end;

destructor TAgg2DRasterizerGamma.Destroy;
begin
  FAlpha.Free;
  FGamma.Free;

  inherited;
end;

function TAgg2DRasterizerGamma.FuncOperatorGamma(X: Double): Double;
begin
  Result := FAlpha.FuncOperatorGamma(FGamma.FuncOperatorGamma(X));
end;


{ TAgg2D }

constructor TAgg2D.Create;
begin
  FGammaAgg2D := nil;

  FRenderingBuffer := TAggRenderingBuffer.Create;

  PixelFormatBgra32(FPixelFormat, FRenderingBuffer);
  PixelFormatCustomBlendRgba(FPixelFormatComp, FRenderingBuffer,
    @BlendModeAdaptorRgba, CAggOrderBgra);
  PixelFormatBgra32(FPixelFormatPre, FRenderingBuffer);
  PixelFormatCustomBlendRgba(FPixelFormatCompPre, FRenderingBuffer,
    @BlendModeAdaptorRgba, CAggOrderBgra);

  FRendererBase := TAggRendererBase.Create(FPixelFormat);
  FRendererBaseComp := TAggRendererBase.Create(FPixelFormatComp);
  FRendererBasePre := TAggRendererBase.Create(FPixelFormatPre);
  FRendererBaseCompPre := TAggRendererBase.Create(FPixelFormatCompPre);

  FRendererSolid := TAggRendererScanLineAASolid.Create(FRendererBase);
  FRendererSolidComp  := TAggRendererScanLineAASolid.Create(FRendererBaseComp);

  FAllocator := TAggSpanAllocator.Create;
  FClipBox := RectDouble(0, 0, 0, 0);

  FBlendMode := bmAlpha;
  FImageBlendMode := bmDestination;

  FImageBlendColor.Initialize(0, 0, 0);

  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FMasterAlpha := 1;
  FAntiAliasGamma := 1;

  FFillColor.Initialize(255, 255, 255);
  FLineColor.Initialize(0, 0, 0);

  FFillGradient := TAggPodAutoArray.Create(256, SizeOf(TAggColor));
  FLineGradient := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  FLineCap := lcRound;
  FLineJoin := ljRound;

  FFillGradientFlag := grdSolid;
  FLineGradientFlag := grdSolid;

  FFillGradientMatrix := TAggTransAffine.Create;
  FLineGradientMatrix := TAggTransAffine.Create;

  FFillGradientD1 := 0;
  FLineGradientD1 := 0;
  FFillGradientD2 := 100;
  FLineGradientD2 := 100;

  FTextAngle := 0;
  FTextAlignX := tahLeft;
  FTextAlignY := tavBottom;
  FTextHints := True;
  FFontHeight := 0;
  FFontAscent := 0;
  FFontDescent := 0;

  FFontCacheType := fcRaster;
  FImageFilter := ifBilinear;
  FImageResample := irNever;

  FGammaNone := TAggGammaNone.Create;

  FImageFilterBilinear := TAggImageFilterBilinear.Create;
  FImageFilterHanning := TAggImageFilterHanning.Create;
  FImageFilterHermite := TAggImageFilterHermite.Create;
  FImageFilterQuadric := TAggImageFilterQuadric.Create;
  FImageFilterBicubic := TAggImageFilterBicubic.Create;
  FImageFilterCatrom := TAggImageFilterCatrom.Create;
  FImageFilterSpline16 := TAggImageFilterSpline16.Create;
  FImageFilterSpline36 := TAggImageFilterSpline36.Create;
  FImageFilterBlackman144 := TAggImageFilterBlackman144.Create;

  FImageFilterLUT := TAggImageFilter.Create(FImageFilterBilinear, True);

  FLinearGradientFunction := TAggGradientX.Create;
  FRadialGradientFunction := TAggGradientCircle.Create;

  FFillGradientInterpolator := TAggSpanInterpolatorLinear.Create(FFillGradientMatrix);
  FLineGradientInterpolator := TAggSpanInterpolatorLinear.Create(FLineGradientMatrix);

  FLineWidth := 1;
  FEvenOddFlag := False;

  FPath := TAggPathStorage.Create;
  FTransform := TAggTransAffine.Create;

  FConvCurve := TAggConvCurve.Create(FPath);
  FConvDash := TAggConvDash.Create(FConvCurve);
  FConvStroke := TAggConvStroke.Create(FConvCurve);

  FPathTransform := TAggConvTransform.Create(FConvCurve, FTransform);
  FStrokeTransform := TAggConvTransform.Create(FConvStroke, FTransform);

{$IFDEF AGG2D_USE_FREETYPE}
  FFontEngine := TAggFontEngineFreetypeInt32.Create;
{$ELSE}
  FFontDC := GetDC(0);

  FFontEngine := TAggFontEngineWin32TrueTypeInt32.Create(FFontDC);
{$ENDIF}

  FFontCacheManager := TAggFontCacheManager.Create(FFontEngine);

  SetLineCap(FLineCap);
  SetLineJoin(FLineJoin);
end;

constructor TAgg2D.Create(Buffer: PInt8u; Width, Height: Cardinal;
  Stride: Integer);
begin
  Create;
  Attach(Buffer, Width, Height, Stride);
end;


destructor TAgg2D.Destroy;
begin
  FRendererBase.Free;
  FRendererBaseComp.Free;
  FRendererBasePre.Free;
  FRendererBaseCompPre.Free;

  FRendererSolid.Free;
  FRendererSolidComp.Free;
  FRenderingBuffer.Free;

  FPathTransform.Free;
  FStrokeTransform.Free;

  FAllocator.Free;

  FScanLine.Free;
  FRasterizer.Free;
  FTransform.Free;

  FFillGradient.Free;
  FLineGradient.Free;

  FLinearGradientFunction.Free;
  FRadialGradientFunction.Free;

  FFillGradientInterpolator.Free;
  FLineGradientInterpolator.Free;

  FFillGradientMatrix.Free;
  FLineGradientMatrix.Free;

  FImageFilterBilinear.Free;
  FImageFilterHanning.Free;
  FImageFilterHermite.Free;
  FImageFilterQuadric.Free;
  FImageFilterBicubic.Free;
  FImageFilterCatrom.Free;
  FImageFilterSpline16.Free;
  FImageFilterSpline36.Free;
  FImageFilterBlackman144.Free;

  FImageFilterLUT.Free;
  FPath.Free;
  FGammaNone.Free;
  FGammaAgg2D.Free;

  FConvCurve.Free;
  FConvDash.Free;
  FConvStroke.Free;

  FFontEngine.Free;
  FFontCacheManager.Free;

  FPixelFormat.Free;
  FPixelFormatComp.Free;
  FPixelFormatPre.Free;
  FPixelFormatCompPre.Free;

{$IFNDEF AGG2D_USE_FREETYPE}
  ReleaseDC(0, FFontDC);
{$ENDIF}
end;

procedure TAgg2D.Attach(Buffer: PInt8u; Width, Height: Cardinal; Stride: Integer);
begin
  FRenderingBuffer.Attach(Buffer, Width, Height, Stride);

  FRendererBase.ResetClipping(True);
  FRendererBaseComp.ResetClipping(True);
  FRendererBasePre.ResetClipping(True);
  FRendererBaseCompPre.ResetClipping(True);

  ResetTransformations;

  SetLineWidth(1);
  SetLineColor(0, 0, 0);
  SetFillColor(255, 255, 255);

  TextAlignment(tahLeft, tavBottom);

  ClipBox(0, 0, Width, Height);
  LineCap := lcRound;
  LineJoin := ljRound;
  FlipText := False;

  SetImageFilter(ifBilinear);
  SetImageResample(irNever);

  FMasterAlpha := 1;
  FAntiAliasGamma := 1;

  FRasterizer.Gamma(FGammaNone);

  FBlendMode := bmAlpha;
end;

procedure TAgg2D.Attach(Img: TAgg2DImage);
begin
  Attach(Img.FRenderingBuffer.Buffer, Img.FRenderingBuffer.Width,
    Img.FRenderingBuffer.Height, Img.FRenderingBuffer.Stride);
end;

procedure TAgg2D.ClipBox(X1, Y1, X2, Y2: Double);
var
  Rect: TRectInteger;
begin
  FClipBox := RectDouble(X1, Y1, X2, Y2);

  Rect := RectInteger(Trunc(X1), Trunc(Y1), Trunc(X2), Trunc(Y2));

  FRendererBase.SetClipBox(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
  FRendererBaseComp.SetClipBox(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
  FRendererBasePre.SetClipBox(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
  FRendererBaseCompPre.SetClipBox(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);

  FRasterizer.SetClipBox(X1, Y1, X2, Y2);
end;

function TAgg2D.ClipBox: TRectDouble;
begin
  Result := FClipBox;
end;

procedure TAgg2D.ClearAll(C: TAggColorRgba8);
begin
  FRendererBase.Clear(C);
end;

procedure TAgg2D.ClearAll(R, G, B: Cardinal; A: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(R, G, B, A);
  ClearAll(Clr);
end;

procedure TAgg2D.ClearClipBox(C: TAggColorRgba8);
var
  Clr: TAggColor;
begin
  Clr.FromRgba8(C);

  FRendererBase.CopyBar(0, 0, FRendererBase.Width, FRendererBase.Height,
    @Clr);
end;

procedure TAgg2D.ClearClipBox(R, G, B: Cardinal; A: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(R, G, B, A);
  ClearClipBox(Clr);
end;

procedure TAgg2D.WorldToScreen(X, Y: PDouble);
begin
  FTransform.Transform(FTransform, X, Y);
end;

procedure TAgg2D.WorldToScreen(var X, Y: Double);
begin
  FTransform.Transform(FTransform, @X, @Y);
end;

procedure TAgg2D.ScreenToWorld(X, Y: PDouble);
begin
  FTransform.InverseTransform(FTransform, X, Y);
end;

procedure TAgg2D.ScreenToWorld(var X, Y: Double);
begin
  FTransform.InverseTransform(FTransform, @X, @Y);
end;

function TAgg2D.WorldToScreen(Scalar: Double): Double;
var
  Rect: TRectDouble;
begin
  Rect.X1 := 0;
  Rect.Y1 := 0;
  Rect.X2 := Scalar;
  Rect.Y2 := Scalar;

  WorldToScreen(Rect.X1, Rect.Y1);
  WorldToScreen(Rect.X2, Rect.Y2);

  Result := Sqrt(0.5 * (Sqr(Rect.X2 - Rect.X1) + Sqr(Rect.Y2 - Rect.Y1)));
end;

function TAgg2D.ScreenToWorld(Scalar: Double): Double;
var
  X1, Y1, X2, Y2: Double;
begin
  X1 := 0;
  Y1 := 0;
  X2 := Scalar;
  Y2 := Scalar;

  ScreenToWorld(@X1, @Y1);
  ScreenToWorld(@X2, @Y2);

  Result := Sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) * 0.7071068;
end;

procedure TAgg2D.AlignPoint(X, Y: PDouble);
begin
  WorldToScreen(X, Y);

  X^ := Floor(X^) + 0.5;
  Y^ := Floor(Y^) + 0.5;

  ScreenToWorld(X, Y);
end;

procedure TAgg2D.AlignPoint(var X, Y: Double);
begin
  WorldToScreen(X, Y);

  X := Floor(X) + 0.5;
  Y := Floor(Y) + 0.5;

  ScreenToWorld(X, Y);
end;

function TAgg2D.InBox(WorldX, WorldY: Double): Boolean;
begin
  WorldToScreen(@WorldX, @WorldY);

  Result := FRendererBase.Inbox(Trunc(WorldX), Trunc(WorldY));
end;

function TAgg2D.InBox(World: TPointDouble): Boolean;
begin
  WorldToScreen(World.X, World.Y);

  Result := FRendererBase.Inbox(Trunc(World.X), Trunc(World.Y));
end;

procedure TAgg2D.SetBlendMode(Value: TAggBlendMode);
begin
  FBlendMode := Value;

  FPixelFormatComp.BlendMode := Value;
  FPixelFormatCompPre.BlendMode := Value;
end;

procedure TAgg2D.SetImageBlendMode(Value: TAggBlendMode);
begin
  FImageBlendMode := Value;
end;

procedure TAgg2D.SetImageBlendColor(C: TAggColorRgba8);
begin
  FImageBlendColor := C;
end;

procedure TAgg2D.SetImageBlendColor(R, G, B: Cardinal; A: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(R, G, B, A);
  SetImageBlendColor(Clr);
end;

procedure TAgg2D.SetMasterAlpha(A: Double);
begin
  FMasterAlpha := A;

  UpdateRasterizerGamma;
end;

procedure TAgg2D.SetAntiAliasGamma(G: Double);
begin
  FAntiAliasGamma := G;

  UpdateRasterizerGamma;
end;

procedure TAgg2D.SetFillColor(C: TAggColorRgba8);
begin
  FFillColor := C;
  FFillGradientFlag := grdSolid;
end;

procedure TAgg2D.SetFillColor(R, G, B: Cardinal; A: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(R, G, B, A);
  SetFillColor(Clr);
end;

procedure TAgg2D.NoFill;
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(0, 0, 0, 0);
  SetFillColor(Clr);
end;

procedure TAgg2D.SetLineColor(C: TAggColorRgba8);
begin
  FLineColor := C;
  FLineGradientFlag := grdSolid;
end;

procedure TAgg2D.SetLineColor(R, G, B: Cardinal; A: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(R, G, B, A);
  SetLineColor(Clr);
end;

procedure TAgg2D.NoLine;
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(0, 0, 0, 0);
  SetLineColor(Clr);
end;

procedure TAgg2D.FillLinearGradient(X1, Y1, X2, Y2: Double;
  C1, C2: TAggColorRgba8; Profile: Double = 1);
var
  I, StartGradient, StopGradient: Integer;
  K, Angle: Double;
  C: TAggColorRgba8;
  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 127);
  StopGradient := 128 + Trunc(Profile * 127);

  if StopGradient <= StartGradient then
    StopGradient := StartGradient + 1;

  K := 1 / (StopGradient - StartGradient);
  I := 0;

  while I < StartGradient do
  begin
    Clr.FromRgba8(C1);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < StopGradient do
  begin
    C := C1.Gradient(C2, (I - StartGradient) * K);

    Clr.FromRgba8(C);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < 256 do
  begin
    Clr.FromRgba8(C2);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  Angle := ArcTan2(Y2 - Y1, X2 - X1);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Rotate(Angle);
  FFillGradientMatrix.Translate(X1, Y1);
  FFillGradientMatrix.Multiply(FTransform);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
  FFillGradientD2 := Sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1));
  FFillGradientFlag := grdLinear;

  FFillColor.Initialize(0, 0, 0); // Set some real TAggColorRgba8
end;

procedure TAgg2D.LineLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TAggColorRgba8;
  Profile: Double = 1);
var
  I, StartGradient, StopGradient: Integer;
  K, Angle: Double;
  C: TAggColorRgba8;
  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 128);
  StopGradient := 128 + Trunc(Profile * 128);

  if StopGradient <= StartGradient then
    StopGradient := StartGradient + 1;

  K := 1 / (StopGradient - StartGradient);
  I := 0;

  while I < StartGradient do
  begin
    Clr.FromRgba8(C1);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < StopGradient do
  begin
    C := C1.Gradient(C2, (I - StartGradient) * K);

    Clr.FromRgba8(C);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < 256 do
  begin
    Clr.FromRgba8(C2);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  Angle := ArcTan2(Y2 - Y1, X2 - X1);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Rotate(Angle);
  FLineGradientMatrix.Translate(X1, Y1);
  FLineGradientMatrix.Multiply(FTransform); { ! }
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
  FLineGradientD2 := Hypot((X2 - X1), (Y2 - Y1));
  FLineGradientFlag := grdLinear;
end;

procedure TAgg2D.FillRadialGradient(X, Y, R: Double; C1, C2: TAggColorRgba8;
  Profile: Double = 1);
var
  I, StartGradient, StopGradient: Integer;

  K: Double;
  C: TAggColorRgba8;

  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 127);
  StopGradient := 128 + Trunc(Profile * 127);

  if StopGradient <= StartGradient then
    StopGradient := StartGradient + 1;

  K := 1 / (StopGradient - StartGradient);
  I := 0;

  while I < StartGradient do
  begin
    Clr.FromRgba8(C1);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < StopGradient do
  begin
    C := C1.Gradient(C2, (I - StartGradient) * K);

    Clr.FromRgba8(C);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < 256 do
  begin
    Clr.FromRgba8(C2);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  FFillGradientD2 := WorldToScreen(R);

  WorldToScreen(@X, @Y);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Translate(X, Y);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
  FFillGradientFlag := grdRadial;

  FFillColor.Initialize(0, 0, 0); // Set some real TAggColorRgba8
end;

procedure TAgg2D.LineRadialGradient(X, Y, R: Double; C1, C2: TAggColorRgba8;
  Profile: Double = 1);
var
  I, StartGradient, StopGradient: Integer;
  K: Double;
  C: TAggColorRgba8;
  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 128);
  StopGradient := 128 + Trunc(Profile * 128);

  if StopGradient <= StartGradient then
    StopGradient := StartGradient + 1;

  K := 1 / (StopGradient - StartGradient);
  I := 0;

  while I < StartGradient do
  begin
    Clr.FromRgba8(C1);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < StopGradient do
  begin
    C := C1.Gradient(C2, (I - StartGradient) * K);

    Clr.FromRgba8(C);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < 256 do
  begin
    Clr.FromRgba8(C2);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  FLineGradientD2 := WorldToScreen(R);

  WorldToScreen(X, Y);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Translate(X, Y);
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
  FLineGradientFlag := grdRadial;
end;

procedure TAgg2D.FillRadialGradient(X, Y, R: Double; C1, C2, C3: TAggColorRgba8);
var
  I: Integer;
  C: TAggColorRgba8;
  Clr: TAggColor;
begin
  I := 0;

  while I < 128 do
  begin
    C := C1.Gradient(C2, I / 127);

    Clr.FromRgba8(C);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < 256 do
  begin
    C := C2.Gradient(C3, (I - 128) / 127);

    Clr.FromRgba8(C);

    Move(Clr, FFillGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  FFillGradientD2 := WorldToScreen(R);

  WorldToScreen(@X, @Y);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Translate(X, Y);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
  FFillGradientFlag := grdRadial;

  FFillColor.Initialize(0, 0, 0); // Set some real TAggColorRgba8
end;

procedure TAgg2D.LineRadialGradient(X, Y, R: Double; C1, C2, C3: TAggColorRgba8);
var
  I: Integer;
  C: TAggColorRgba8;
  Clr: TAggColor;
begin
  I := 0;

  while I < 128 do
  begin
    C := C1.Gradient(C2, I / 127);

    Clr.FromRgba8(C);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  while I < 256 do
  begin
    C := C2.Gradient(C3, (I - 128) / 127);

    Clr.FromRgba8(C);

    Move(Clr, FLineGradient[I]^, SizeOf(TAggColor));
    Inc(I);
  end;

  FLineGradientD2 := WorldToScreen(R);

  WorldToScreen(@X, @Y);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Translate(X, Y);
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
  FLineGradientFlag := grdRadial;
end;

procedure TAgg2D.FillRadialGradient(X, Y, R: Double);
begin
  FFillGradientD2 := WorldToScreen(R);

  WorldToScreen(@X, @Y);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Translate(X, Y);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
end;

procedure TAgg2D.LineRadialGradient(X, Y, R: Double);
begin
  FLineGradientD2 := WorldToScreen(R);

  WorldToScreen(@X, @Y);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Translate(X, Y);
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
end;

procedure TAgg2D.SetLineWidth(W: Double);
begin
  FLineWidth := W;
  FConvStroke.Width := W;
end;

procedure TAgg2D.SetLineCap(Cap: TAggLineCap);
begin
  FLineCap := Cap;
  FConvStroke.LineCap := Cap;
end;

procedure TAgg2D.SetLineJoin(Join: TAggLineJoin);
begin
  FLineJoin := Join;

  FConvStroke.LineJoin := Join;
end;

procedure TAgg2D.SetFillEvenOdd(EvenOddFlag: Boolean);
begin
  FEvenOddFlag := EvenOddFlag;

  if EvenOddFlag then
    FRasterizer.FillingRule := frEvenOdd
  else
    FRasterizer.FillingRule  := frNonZero;
end;

function TAgg2D.GetTransformations: TAggTransformations;
begin
  FTransform.StoreTo(@Result.AffineMatrix[0]);
end;

procedure TAgg2D.SetTransformations(var Tr: TAggTransformations);
begin
  FTransform.LoadFrom(@Tr.AffineMatrix[0]);
  UpdateApproximationScale;
end;

procedure TAgg2D.SetTransformations(V0, V1, V2, V3, V4, V5: Double);
var
  M: TAggParallelogram;
begin
  M[0] := V0;
  M[1] := V1;
  M[2] := V2;
  M[3] := V3;
  M[4] := V4;
  M[5] := V5;

  FTransform.LoadFrom(@M);
  UpdateApproximationScale;
end;

procedure TAgg2D.ResetTransformations;
begin
  FTransform.Reset;
end;

procedure TAgg2D.Affine(Tr: TAggTransAffine);
begin
  FTransform.Multiply(Tr);
  UpdateApproximationScale;
end;

procedure TAgg2D.Affine(var Tr: TAggTransformations);
var
  Ta: TAggTransAffine;
begin
  Ta := TAggTransAffine.Create(Tr.AffineMatrix[0], Tr.AffineMatrix[1],
    Tr.AffineMatrix[2], Tr.AffineMatrix[3], Tr.AffineMatrix[4],
    Tr.AffineMatrix[5]);
  try
    Affine(Ta);
  finally
    Ta.Free;
  end;
end;

procedure TAgg2D.Rotate(Angle: Double);
begin
  FTransform.Rotate(Angle);
end;

procedure TAgg2D.Scale(Sx, Sy: Double);
begin
  FTransform.Scale(Sx, Sy);
  UpdateApproximationScale;
end;

procedure TAgg2D.Skew(Sx, Sy: Double);
var
  Tas: TAggTransAffineSkewing;
begin
  Tas := TAggTransAffineSkewing.Create(Sx, Sy);
  try
    FTransform.Multiply(Tas);
  finally
    Tas.Free;
  end;
end;

procedure TAgg2D.Translate(X, Y: Double);
begin
  FTransform.Translate(X, Y);
end;

procedure TAgg2D.Parallelogram(X1, Y1, X2, Y2: Double; Para: PDouble);
var
  Ta: TAggTransAffine;
begin
  Ta := TAggTransAffine.Create(X1, Y1, X2, Y2, PAggParallelogram(Para));
  try
    FTransform.Multiply(Ta);
  finally
    Ta.Free;
  end;

  UpdateApproximationScale;
end;

procedure TAgg2D.Viewport(WorldX1, WorldY1, WorldX2, WorldY2, ScreenX1, ScreenY1,
  ScreenX2, ScreenY2: Double; Opt: TAggViewportOption = voXMidYMid);
var
  Vp: TAggTransViewport;
  Mx: TAggTransAffine;
begin
  Vp := TAggTransViewport.Create;
  try
    case Opt of
      voAnisotropic:
        Vp.PreserveAspectRatio(0, 0, arStretch);

      voXMinYMin:
        Vp.PreserveAspectRatio(0, 0, arMeet);

      voXMidYMin:
        Vp.PreserveAspectRatio(0.5, 0, arMeet);

      voXMaxYMin:
        Vp.PreserveAspectRatio(1, 0, arMeet);

      voXMinYMid:
        Vp.PreserveAspectRatio(0, 0.5, arMeet);

      voXMidYMid:
        Vp.PreserveAspectRatio(0.5, 0.5, arMeet);

      voXMaxYMid:
        Vp.PreserveAspectRatio(1, 0.5, arMeet);

      voXMinYMax:
        Vp.PreserveAspectRatio(0, 1, arMeet);

      voXMidYMax:
        Vp.PreserveAspectRatio(0.5, 1, arMeet);

      voXMaxYMax:
        Vp.PreserveAspectRatio(1, 1, arMeet);
    end;

    Vp.WorldViewport(WorldX1, WorldY1, WorldX2, WorldY2);
    Vp.DeviceViewport(ScreenX1, ScreenY1, ScreenX2, ScreenY2);

    Mx := TAggTransAffine.Create;
    try
      Vp.ToAffine(Mx);
      FTransform.Multiply(Mx);
    finally
      Mx.Free;
    end;
  finally
    Vp.Free;
  end;

  UpdateApproximationScale;
end;

procedure TAgg2D.Viewport(World, Screen: TRectDouble; Opt: TAggViewportOption);
var
  Vp: TAggTransViewport;
  Mx: TAggTransAffine;
begin
  Vp := TAggTransViewport.Create;
  try
    case Opt of
      voAnisotropic:
        Vp.PreserveAspectRatio(0, 0, arStretch);

      voXMinYMin:
        Vp.PreserveAspectRatio(0, 0, arMeet);

      voXMidYMin:
        Vp.PreserveAspectRatio(0.5, 0, arMeet);

      voXMaxYMin:
        Vp.PreserveAspectRatio(1, 0, arMeet);

      voXMinYMid:
        Vp.PreserveAspectRatio(0, 0.5, arMeet);

      voXMidYMid:
        Vp.PreserveAspectRatio(0.5, 0.5, arMeet);

      voXMaxYMid:
        Vp.PreserveAspectRatio(1, 0.5, arMeet);

      voXMinYMax:
        Vp.PreserveAspectRatio(0, 1, arMeet);

      voXMidYMax:
        Vp.PreserveAspectRatio(0.5, 1, arMeet);

      voXMaxYMax:
        Vp.PreserveAspectRatio(1, 1, arMeet);
    end;

    Vp.WorldViewport(World);
    Vp.DeviceViewport(Screen);

    Mx := TAggTransAffine.Create;
    try
      Vp.ToAffine(Mx);
      FTransform.Multiply(Mx);
    finally
      Mx.Free;
    end;
  finally
    Vp.Free;
  end;

  UpdateApproximationScale;
end;

procedure TAgg2D.Line(X1, Y1, X2, Y2: Double);
begin
  FPath.RemoveAll;

  AddLine(X1, Y1, X2, Y2);
  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Triangle(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(X1, Y1);
  FPath.LineTo(X2, Y2);
  FPath.LineTo(X3, Y3);
  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Rectangle(X1, Y1, X2, Y2: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(X1, Y1);
  FPath.LineTo(X2, Y1);
  FPath.LineTo(X2, Y2);
  FPath.LineTo(X1, Y2);
  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RemoveAllDashes;
begin
  FConvDash.RemoveAllDashes;
  FConvStroke.Source := FConvCurve;
end;

procedure TAgg2D.RoundedRect(X1, Y1, X2, Y2, R: Double);
var
  Rc: TAggRoundedRect;
begin
  FPath.RemoveAll;
  Rc := TAggRoundedRect.Create(X1, Y1, X2, Y2, R);
  try
    Rc.NormalizeRadius;
    Rc.ApproximationScale := WorldToScreen(1) * GApproxScale;

    FPath.AddPath(Rc, 0, False);
  finally
    Rc.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(Rect: TRectDouble; R: Double);
var
  Rc: TAggRoundedRect;
begin
  FPath.RemoveAll;
  Rc := TAggRoundedRect.Create(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2, R);
  try
    Rc.NormalizeRadius;
    Rc.ApproximationScale := WorldToScreen(1) * GApproxScale;

    FPath.AddPath(Rc, 0, False);
  finally
    Rc.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(X1, Y1, X2, Y2, Rx, Ry: Double);
var
  Rc: TAggRoundedRect;
begin
  FPath.RemoveAll;
  Rc := TAggRoundedRect.Create;
  try
    Rc.Rect(X1, Y1, X2, Y2);
    Rc.Radius(Rx, Ry);
    Rc.NormalizeRadius;

    FPath.AddPath(Rc, 0, False);
  finally
    Rc.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(Rect: TRectDouble; Rx, Ry: Double);
var
  Rc: TAggRoundedRect;
begin
  FPath.RemoveAll;
  Rc := TAggRoundedRect.Create;
  try
    Rc.Rect(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
    Rc.Radius(Rx, Ry);
    Rc.NormalizeRadius;

    FPath.AddPath(Rc, 0, False);
  finally
    Rc.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(X1, Y1, X2, Y2, RxBottom, RyBottom, RxTop,
  RyTop: Double);
var
  Rc: TAggRoundedRect;
begin
  FPath.RemoveAll;
  Rc := TAggRoundedRect.Create;
  try
    Rc.Rect(X1, Y1, X2, Y2);
    Rc.Radius(RxBottom, RyBottom, RxTop, RyTop);
    Rc.NormalizeRadius;

    Rc.ApproximationScale := WorldToScreen(1) * GApproxScale;

    FPath.AddPath(Rc, 0, False);
  finally
    Rc.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Ellipse(Cx, Cy, Rx, Ry: Double);
var
  El: TAggBezierArc;
begin
  FPath.RemoveAll;

  El := TAggBezierArc.Create(Cx, Cy, Rx, Ry, 0, 2 * Pi);
  try
    FPath.AddPath(El, 0, False);
  finally
    El.Free;
  end;

  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Circle(Cx, Cy, Radius: Double);
var
  El: TAggBezierArc;
begin
  FPath.RemoveAll;

  El := TAggBezierArc.Create(Cx, Cy, Radius, Radius, 0, 2 * Pi);
  try
    FPath.AddPath(El, 0, False);
  finally
    El.Free;
  end;

  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Arc(Cx, Cy, Rx, Ry, Start, Sweep: Double);
var
  Ar: TAggArc;
begin
  FPath.RemoveAll;

  Ar := TAggArc.Create(Cx, Cy, Rx, Ry, Start, Sweep, False);
  try
    FPath.AddPath(Ar, 0, False);
  finally
    Ar.Free;
  end;

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Star(Cx, Cy, R1, R2, StartAngle: Double; NumRays: Integer);
var
  Da, A, X, Y: Double;
  I: Integer;
begin
  FPath.RemoveAll;

  Da := Pi / NumRays;
  A := StartAngle;

  I := 0;

  while I < NumRays do
  begin
    SinCosScale(A, Y, X, R2);
    X := X + Cx;
    Y := Y + Cy;

    if I <> 0 then
      FPath.LineTo(X, Y)
    else
      FPath.MoveTo(X, Y);

    A := A + Da;

    SinCosScale(A, Y, X, R1);
    FPath.LineTo(X + Cx, Y + Cy);

    A := A + Da;

    Inc(I);
  end;

  ClosePolygon;
  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Curve(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(X1, Y1);
  FPath.Curve3(X2, Y2, X3, Y3);

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Curve(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(X1, Y1);
  FPath.Curve4(X2, Y2, X3, Y3, X4, Y4);

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Polygon(XY: PPointDouble; NumPoints: Integer;
  Flag: TAggDrawPathFlag = dpfFillAndStroke);
begin
  FPath.RemoveAll;
  FPath.AddPoly(Xy, NumPoints);

  ClosePolygon;
  DrawPath(Flag);
end;

procedure TAgg2D.Polyline(Xy: PPointDouble; NumPoints: Integer);
begin
  FPath.RemoveAll;
  FPath.AddPoly(Xy, NumPoints);

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.SetFlipText(Value: Boolean);
begin
  FFontEngine.FlipY := Value;
end;

function TAgg2D.GetFlipText: Boolean;
begin
  Result := FFontEngine.FlipY;
end;

function TAgg2D.GetRow(Y: Cardinal): PInt8U;
begin
  Result := FRenderingBuffer.Row(Y);
end;

procedure TAgg2D.Font(FileName: PAnsiChar; Height: Double; Bold: Boolean = False;
  Italic: Boolean = False; Ch: TAggFontCache = fcRaster;
  Angle: Double = 0);
var
  B: Integer;
begin
  FTextAngle := Angle;
  FFontHeight := Height;
  FFontCacheType := Ch;

{$IFDEF AGG2D_USE_FREETYPE}
  if Ch = fcVector then
    FFontEngine.LoadFont(PAnsiChar(FileName), 0, grOutline)
  else
    FFontEngine.LoadFont(PAnsiChar(FileName), 0, grAgggray8);

  FFontEngine.Hinting := FTextHints;

  if Ch = fcVector then
    FFontEngine.SetHeight(Height)
  else
    FFontEngine.SetHeight(WorldToScreen(Height));
{$ELSE}
  FFontEngine.Hinting := FTextHints;

  if Bold then
    B := 700
  else
    B := 400;

  if Ch = fcVector then
    FFontEngine.CreateFont(PAnsiChar(FileName), grOutline, Height, 0, B,
      Italic)
  else
    FFontEngine.CreateFont(PAnsiChar(FileName), grAgggray8,
      WorldToScreen(Height), 0, B, Italic);
{$ENDIF}
end;

function TAgg2D.FontHeight: Double;
begin
  Result := FFontHeight;
end;

procedure TAgg2D.TextAlignment(AlignX: TAggTextAlignmentHorizontal;
  AlignY: TAggTextAlignmentVertical);
begin
  FTextAlignX := AlignX;
  FTextAlignY := AlignY;
end;

procedure TAgg2D.SetTextHints(Value: Boolean);
begin
  FTextHints := Value;
end;

function TAgg2D.TextWidth(Str: PAnsiChar): Double;
var
  X, Y : Double;
  First: Boolean;
  Glyph: PAggGlyphCache;
begin
  X := 0;
  Y := 0;

  First := True;

  while Str^ <> #0 do
  begin
    Glyph := FFontCacheManager.Glyph(Int32u(Str^));

    if Glyph <> nil then
    begin
      if not First then
        FFontCacheManager.AddKerning(@X, @Y);

      X := X + Glyph.AdvanceX;
      Y := Y + Glyph.AdvanceY;

      First := False; { ! }
    end;

    Inc(PtrComp(Str));
  end;

  if FFontCacheType = fcVector then
    Result := X
  else
    Result := ScreenToWorld(X);
end;

function TAgg2D.TextWidth(Str: AnsiString): Double;
var
  X, Y : Double;
  First: Boolean;
  Glyph: PAggGlyphCache;
  I: Integer;
begin
  X := 0;
  Y := 0;

  First := True;

  for I := 1 to Length(Str) do
  begin
    Glyph := FFontCacheManager.Glyph(Int32u(Str[I]));

    if Glyph <> nil then
    begin
      if not First then
        FFontCacheManager.AddKerning(@X, @Y);

      X := X + Glyph.AdvanceX;
      Y := Y + Glyph.AdvanceY;

      First := False; { ! }
    end;
  end;

  if FFontCacheType = fcVector then
    Result := X
  else
    Result := ScreenToWorld(X);
end;

procedure TAgg2D.Text(X, Y: Double; Str: PAnsiChar; RoundOff: Boolean = False;
  Ddx: Double = 0; Ddy: Double = 0);
var
  Asc: Double;
  Delta, Start: TPointDouble;
  Glyph: PAggGlyphCache;
  Mtx: TAggTransAffine;
  I: Integer;
  Tr: TAggConvTransform;
begin
  Delta.X := 0;
  Delta.Y := 0;

  case FTextAlignX of
    tahCenter:
      Delta.X := -TextWidth(Str) * 0.5;
    tahRight:
      Delta.X := -TextWidth(Str);
  end;

  Asc := FontHeight;
  Glyph := FFontCacheManager.Glyph(Int32u('H'));

  if Glyph <> nil then
    Asc := Glyph.Bounds.Y2 - Glyph.Bounds.Y1;

  if FFontCacheType = fcRaster then
    Asc := ScreenToWorld(Asc);

  case FTextAlignY of
    tavCenter:
      Delta.Y := -Asc * 0.5;

    tavTop:
      Delta.Y := -Asc;
  end;

  if FFontEngine.FlipY then
    Delta.Y := -Delta.Y;

  Start.X := X + Delta.X;
  Start.Y := Y + Delta.Y;

  if RoundOff then
  begin
    Start.X := Trunc(Start.X);
    Start.Y := Trunc(Start.Y);
  end;

  Start.X := Start.X + Ddx;
  Start.Y := Start.Y + Ddy;

  Mtx := TAggTransAffine.Create;
  try
    Mtx.Translate(-X, -Y);
    Mtx.Rotate(FTextAngle);
    Mtx.Translate(X, Y);

    Tr := TAggConvTransform.Create(FFontCacheManager.PathAdaptor, Mtx);

    if FFontCacheType = fcRaster then
      WorldToScreen(@Start.X, @Start.Y);

    I := 0;

    while PAnsiChar(PtrComp(Str) + I * SizeOf(AnsiChar))^ <> #0 do
    begin
      Glyph := FFontCacheManager.Glyph
        (Int32u(PAnsiChar(PtrComp(Str) + I * SizeOf(AnsiChar))^));

      if Glyph <> nil then
      begin
        if I <> 0 then
          FFontCacheManager.AddKerning(@X, @Y);

        FFontCacheManager.InitEmbeddedAdaptors(Glyph, Start.X, Start.Y);

        if Glyph.DataType = gdOutline then
        begin
          FPath.RemoveAll;
          FPath.AddPath(Tr, 0, False);

          DrawPath;
        end;

        if Glyph.DataType = gdGray8 then
        begin
          Render(FFontCacheManager.Gray8Adaptor,
            FFontCacheManager.Gray8ScanLine);
        end;

        Start.X := Start.X + Glyph.AdvanceX;
        Start.Y := Start.Y + Glyph.AdvanceY;
      end;
      Inc(I);
    end;
  finally
    Tr.Free;
    Mtx.Free
  end;
end;

procedure TAgg2D.Text(X, Y: Double; Str: AnsiString; RoundOff: Boolean; Ddx,
  Ddy: Double);
var
  Asc: Double;
  Delta, Start: TPointDouble;
  Glyph: PAggGlyphCache;
  Mtx: TAggTransAffine;
  I: Integer;
  Tr: TAggConvTransform;
begin
  Delta.X := 0;
  Delta.Y := 0;

  case FTextAlignX of
    tahCenter:
      Delta.X := -TextWidth(Str) * 0.5;
    tahRight:
      Delta.X := -TextWidth(Str);
  end;

  Asc := FontHeight;
  Glyph := FFontCacheManager.Glyph(Int32u('H'));

  if Glyph <> nil then
    Asc := Glyph.Bounds.Y2 - Glyph.Bounds.Y1;

  if FFontCacheType = fcRaster then
    Asc := ScreenToWorld(Asc);

  case FTextAlignY of
    tavCenter:
      Delta.Y := -Asc * 0.5;

    tavTop:
      Delta.Y := -Asc;
  end;

  if FFontEngine.FlipY then
    Delta.Y := -Delta.Y;

  Start.X := X + Delta.X;
  Start.Y := Y + Delta.Y;

  if RoundOff then
  begin
    Start.X := Trunc(Start.X);
    Start.Y := Trunc(Start.Y);
  end;

  Start.X := Start.X + Ddx;
  Start.Y := Start.Y + Ddy;

  Mtx := TAggTransAffine.Create;
  try
    Mtx.Translate(-X, -Y);
    Mtx.Rotate(FTextAngle);
    Mtx.Translate(X, Y);

    Tr := TAggConvTransform.Create(FFontCacheManager.PathAdaptor, Mtx);

    if FFontCacheType = fcRaster then
      WorldToScreen(@Start.X, @Start.Y);

    for I := 1 to Length(Str) do
    begin
      Glyph := FFontCacheManager.Glyph(Int32u(Str[I]));

      if Glyph <> nil then
      begin
        if I <> 0 then
          FFontCacheManager.AddKerning(@X, @Y);

        FFontCacheManager.InitEmbeddedAdaptors(Glyph, Start.X, Start.Y);

        if Glyph.DataType = gdOutline then
        begin
          FPath.RemoveAll;
          FPath.AddPath(Tr, 0, False);

          DrawPath;
        end;

        if Glyph.DataType = gdGray8 then
        begin
          Render(FFontCacheManager.Gray8Adaptor,
            FFontCacheManager.Gray8ScanLine);
        end;

        Start.X := Start.X + Glyph.AdvanceX;
        Start.Y := Start.Y + Glyph.AdvanceY;
      end;
    end;
  finally
    Tr.Free;
    Mtx.Free
  end;
end;

procedure TAgg2D.ResetPath;
begin
  FPath.RemoveAll;
end;

procedure TAgg2D.MoveTo(X, Y: Double);
begin
  FPath.MoveTo(X, Y);
end;

procedure TAgg2D.MoveRel(Dx, Dy: Double);
begin
  FPath.MoveRelative(Dx, Dy);
end;

procedure TAgg2D.LineTo(X, Y: Double);
begin
  FPath.LineTo(X, Y);
end;

procedure TAgg2D.LineRel(Dx, Dy: Double);
begin
  FPath.LineRelative(Dx, Dy);
end;

procedure TAgg2D.HorizontalLineTo(X: Double);
begin
  FPath.HorizontalLineTo(X);
end;

procedure TAgg2D.HorizontalLineRel(Dx: Double);
begin
  FPath.HorizontalLineRelative(Dx);
end;

procedure TAgg2D.VerticalLineTo(Y: Double);
begin
  FPath.VerticalLineTo(Y);
end;

procedure TAgg2D.VerticalLineRel(Dy: Double);
begin
  FPath.VerticalLineRelative(Dy);
end;

procedure TAgg2D.ArcTo(Rx, Ry, Angle: Double; LargeArcFlag, SweepFlag: Boolean;
  X, Y: Double);
begin
  FPath.ArcTo(Rx, Ry, Angle, LargeArcFlag, SweepFlag, X, Y);
end;

procedure TAgg2D.ArcRel(Rx, Ry, Angle: Double; LargeArcFlag, SweepFlag: Boolean;
  Dx, Dy: Double);
begin
  FPath.ArcRelative(Rx, Ry, Angle, LargeArcFlag, SweepFlag, Dx, Dy);
end;

procedure TAgg2D.QuadricCurveTo(XCtrl, YCtrl, XTo, YTo: Double);
begin
  FPath.Curve3(XCtrl, YCtrl, XTo, YTo);
end;

procedure TAgg2D.QuadricCurveRel(DxCtrl, DyCtrl, DxTo, DyTo: Double);
begin
  FPath.Curve3Relative(DxCtrl, DyCtrl, DxTo, DyTo);
end;

procedure TAgg2D.QuadricCurveTo(XTo, YTo: Double);
begin
  FPath.Curve3(XTo, YTo);
end;

procedure TAgg2D.QuadricCurveRel(DxTo, DyTo: Double);
begin
  FPath.Curve3Relative(DxTo, DyTo);
end;

procedure TAgg2D.CubicCurveTo(XCtrl1, YCtrl1, XCtrl2, YCtrl2, XTo, YTo: Double);
begin
  FPath.Curve4(XCtrl1, YCtrl1, XCtrl2, YCtrl2, XTo, YTo);
end;

procedure TAgg2D.CubicCurveRel(DxCtrl1, DyCtrl1, DxCtrl2, DyCtrl2, DxTo,
  DyTo: Double);
begin
  FPath.Curve4Relative(DxCtrl1, DyCtrl1, DxCtrl2, DyCtrl2, DxTo, DyTo);
end;

procedure TAgg2D.CubicCurveTo(XCtrl2, YCtrl2, XTo, YTo: Double);
begin
  FPath.Curve4(XCtrl2, YCtrl2, XTo, YTo);
end;

procedure TAgg2D.CubicCurveRel(XCtrl2, YCtrl2, XTo, YTo: Double);
begin
  FPath.Curve4Relative(XCtrl2, YCtrl2, XTo, YTo);
end;

procedure TAgg2D.AddDash(DashLength, GapLength: Double);
begin
  FConvDash.AddDash(DashLength, GapLength);
  FConvStroke.Source := FConvDash;
end;

procedure TAgg2D.AddEllipse(Cx, Cy, Rx, Ry: Double; Dir: TAggDirection);
var
  Ar: TAggBezierArc;
begin
  if Dir = dirCCW then
    Ar := TAggBezierArc.Create(Cx, Cy, Rx, Ry, 0, 2 * Pi)
  else
    Ar := TAggBezierArc.Create(Cx, Cy, Rx, Ry, 0, -2 * Pi);
  try
    FPath.AddPath(Ar, 0, False);
  finally
    Ar.Free;
  end;

  FPath.ClosePolygon;
end;

procedure TAgg2D.AddEllipse(Center, Radius: TPointDouble; Dir: TAggDirection);
var
  Ar: TAggBezierArc;
begin
  if Dir = dirCCW then
    Ar := TAggBezierArc.Create(Center.X, Center.Y, Radius, 0, 2 * Pi)
  else
    Ar := TAggBezierArc.Create(Center.X, Center.Y, Radius, 0, -2 * Pi);
  try
    FPath.AddPath(Ar, 0, False);
  finally
    Ar.Free;
  end;

  FPath.ClosePolygon;
end;

procedure TAgg2D.ClosePolygon;
begin
  FPath.ClosePolygon;
end;

procedure TAgg2D.DrawPath(Flag: TAggDrawPathFlag = dpfFillAndStroke);
begin
  FRasterizer.Reset;

  case Flag of
    dpfFillOnly:
      if FFillColor.A <> 0 then
      begin
        FRasterizer.AddPath(FPathTransform);

        Render(True);
      end;

    dpfStrokeOnly:
      if (FLineColor.A <> 0) and (FLineWidth > 0) then
      begin
        FRasterizer.AddPath(FStrokeTransform);

        Render(False);
      end;

    dpfFillAndStroke:
      begin
        if FFillColor.A <> 0 then
        begin
          FRasterizer.AddPath(FPathTransform);

          Render(True);
        end;

        if (FLineColor.A <> 0) and (FLineWidth > 0) then
        begin
          FRasterizer.AddPath(FStrokeTransform);

          Render(False);
        end;
      end;

    dpfFillWithHorizontalLineColor:
      if FLineColor.A <> 0 then
      begin
        FRasterizer.AddPath(FPathTransform);

        Render(False);
      end;
  end;
end;

procedure TAgg2D.DrawPathNoTransform(Flag: TAggDrawPathFlag = dpfFillAndStroke);
begin
end;

procedure TAgg2D.SetImageFilter(F: TAggImageFilterType);
begin
  FImageFilter := F;

  case F of
    ifBilinear:
      FImageFilterLUT.Calculate(FImageFilterBilinear, True);

    ifHanning:
      FImageFilterLUT.Calculate(FImageFilterHanning, True);

    ifHermite:
      FImageFilterLUT.Calculate(FImageFilterHermite, True);

    ifQuadric:
      FImageFilterLUT.Calculate(FImageFilterQuadric, True);

    ifBicubic:
      FImageFilterLUT.Calculate(FImageFilterBicubic, True);

    ifCatrom:
      FImageFilterLUT.Calculate(FImageFilterCatrom, True);

    ifSpline16:
      FImageFilterLUT.Calculate(FImageFilterSpline16, True);

    ifSpline36:
      FImageFilterLUT.Calculate(FImageFilterSpline36, True);

    ifBlackman144:
      FImageFilterLUT.Calculate(FImageFilterBlackman144, True);
  end;
end;

procedure TAgg2D.SetImageResample(F: TAggImageResample);
begin
  FImageResample := F;
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2,
  ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  ResetPath;
  MoveTo(DstX1, DstY1);
  LineTo(DstX2, DstY1);
  LineTo(DstX2, DstY2);
  LineTo(DstX1, DstY2);
  ClosePolygon;

  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(Img, ImgX1, ImgY1, ImgX2, ImgY2, @Parall[0]);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage;
  DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  ResetPath;
  MoveTo(DstX1, DstY1);
  LineTo(DstX2, DstY1);
  LineTo(DstX2, DstY2);
  LineTo(DstX1, DstY2);
  ClosePolygon;

  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(Img, 0, 0, Img.FRenderingBuffer.Width,
    Img.FRenderingBuffer.Height, @Parall[0]);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; Destination: TRectDouble);
var
  Parall: TAggParallelogram;
begin
  ResetPath;
  MoveTo(Destination.X1, Destination.Y1);
  LineTo(Destination.X2, Destination.Y1);
  LineTo(Destination.X2, Destination.Y2);
  LineTo(Destination.X1, Destination.Y2);
  ClosePolygon;

  Parall[0] := Destination.X1;
  Parall[1] := Destination.Y1;
  Parall[2] := Destination.X2;
  Parall[3] := Destination.Y1;
  Parall[4] := Destination.X2;
  Parall[5] := Destination.Y2;

  RenderImage(Img, 0, 0, Img.FRenderingBuffer.Width,
    Img.FRenderingBuffer.Height, @Parall[0]);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer;
  Parallelogram: PDouble);
begin
  ResetPath;

  MoveTo(PDouble(PtrComp(Parallelogram))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram))^ +
    PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^ +
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  ClosePolygon;

  RenderImage(Img, ImgX1, ImgY1, ImgX2, ImgY2, Parallelogram);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; ImgRect: TRectInteger;
  Parallelogram: PDouble);
begin
  ResetPath;

  MoveTo(PDouble(PtrComp(Parallelogram))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram))^ +
    PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^ +
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  ClosePolygon;

  RenderImage(Img, ImgRect, Parallelogram);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; Parallelogram: PDouble);
begin
  ResetPath;

  MoveTo(PDouble(Parallelogram)^, PDouble(PtrComp(Parallelogram) +
    SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^);

  LineTo(PDouble(Parallelogram)^ +
    PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^ +
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  ClosePolygon;

  RenderImage(Img, 0, 0, Img.FRenderingBuffer.Width,
    Img.FRenderingBuffer.Height, Parallelogram);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; ImgRect: TRectInteger; DstX1,
  DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(Img, ImgRect, @Parall[0]);
end;

procedure TAgg2D.TransformImage(Img: TAgg2DImage; ImgRect: TRectInteger;
  Destination: TRectDouble);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := Destination.X1;
  Parall[1] := Destination.Y1;
  Parall[2] := Destination.X2;
  Parall[3] := Destination.Y1;
  Parall[4] := Destination.X2;
  Parall[5] := Destination.Y2;

  RenderImage(Img, ImgRect, @Parall[0]);
end;

procedure TAgg2D.TransformImagePath(Img: TAgg2DImage;
  ImgX1, ImgY1, ImgX2, ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(Img, ImgX1, ImgY1, ImgX2, ImgY2, @Parall[0]);
end;

procedure TAgg2D.TransformImagePath(Img: TAgg2DImage;
  DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(Img, 0, 0, Img.FRenderingBuffer.Width,
    Img.FRenderingBuffer.Height, @Parall[0]);
end;

procedure TAgg2D.TransformImagePath(Img: TAgg2DImage;
  ImgX1, ImgY1, ImgX2, ImgY2: Integer; Parallelogram: PDouble);
begin
  RenderImage(Img, ImgX1, ImgY1, ImgX2, ImgY2, Parallelogram);
end;

procedure TAgg2D.TransformImagePath(Img: TAgg2DImage; Parallelogram: PDouble);
begin
  RenderImage(Img, 0, 0, Img.FRenderingBuffer.Width,
    Img.FRenderingBuffer.Height, Parallelogram);
end;

procedure TAgg2D.BlendImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer;
  DstX, DstY: Double; Alpha: Cardinal = 255);
var
  PixF: TAggPixelFormatProcessor;
  R: TRectInteger;
begin
  WorldToScreen(@DstX, @DstY);
  PixelFormatRgba32(PixF, Img.FRenderingBuffer);
  R := RectInteger(ImgX1, ImgY1, ImgX2, ImgY2);

  if FBlendMode = bmAlpha then
    FRendererBasePre.BlendFrom(PixF, @R, Trunc(DstX) - ImgX1,
      Trunc(DstY) - ImgY1, Alpha)
  else
    FRendererBaseCompPre.BlendFrom(PixF, @R, Trunc(DstX) - ImgX1,
      Trunc(DstY) - ImgY1, Alpha);
end;

procedure TAgg2D.BlendImage(Img: TAgg2DImage; DstX, DstY: Double;
  Alpha: Cardinal = 255);
var
  PixF: TAggPixelFormatProcessor;
begin
  WorldToScreen(@DstX, @DstY);
  PixelFormatRgba32(PixF, Img.FRenderingBuffer);

  FRendererBasePre.BlendFrom(PixF, nil, Trunc(DstX), Trunc(DstY), Alpha);

  if FBlendMode = bmAlpha then
    FRendererBasePre.BlendFrom(PixF, nil, Trunc(DstX), Trunc(DstY), Alpha)
  else
    FRendererBaseCompPre.BlendFrom(PixF, nil, Trunc(DstX), Trunc(DstY), Alpha);
end;

procedure TAgg2D.CopyImage(Img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer;
  DstX, DstY: Double);
var
  R: TRectInteger;
begin
  WorldToScreen(@DstX, @DstY);
  R := RectInteger(ImgX1, ImgY1, ImgX2, ImgY2);

  FRendererBase.CopyFrom(Img.FRenderingBuffer, @R, Trunc(DstX) - ImgX1,
    Trunc(DstY) - ImgY1);
end;

procedure TAgg2D.CopyImage(Img: TAgg2DImage; ImgRect: TRectInteger;
  Destination: TPointDouble);
begin
  WorldToScreen(@Destination.X, @Destination.Y);

  FRendererBase.CopyFrom(Img.FRenderingBuffer, @ImgRect,
    Trunc(Destination.X) - ImgRect.X1, Trunc(Destination.Y) - ImgRect.Y1);
end;

procedure TAgg2D.CopyImage(Img: TAgg2DImage; DstX, DstY: Double);
begin
  WorldToScreen(@DstX, @DstY);

  FRendererBase.CopyFrom(Img.FRenderingBuffer, nil, Trunc(DstX), Trunc(DstY));
end;

procedure TAgg2D.CopyImage(Img: TAgg2DImage; Destination: TPointDouble);
begin
  WorldToScreen(@Destination.X, @Destination.Y);

  FRendererBase.CopyFrom(Img.FRenderingBuffer, nil, Trunc(Destination.X),
    Trunc(Destination.Y));
end;

procedure TAgg2D.Render(AFillColor: Boolean);
begin
  if FBlendMode = bmAlpha then
    Agg2DRendererRender(Self, FRendererBase, FRendererSolid, AFillColor)
  else
    Agg2DRendererRender(Self, FRendererBaseComp, FRendererSolidComp, AFillColor);
end;

procedure TAgg2D.Render(Ras: TAggFontRasterizer; Sl: TAggFontScanLine);
begin
  if FBlendMode = bmAlpha then
    Agg2DRendererRender(Self, FRendererBase, FRendererSolid, Ras, Sl)
  else
    Agg2DRendererRender(Self, FRendererBaseComp, FRendererSolidComp, Ras, Sl);
end;

procedure TAgg2D.AddLine(X1, Y1, X2, Y2: Double);
begin
  FPath.MoveTo(X1, Y1);
  FPath.LineTo(X2, Y2);
end;

procedure TAgg2D.UpdateApproximationScale;
begin
  FConvCurve.ApproximationScale := WorldToScreen(1) * GApproxScale;
  FConvStroke.ApproximationScale := WorldToScreen(1) * GApproxScale;
end;

procedure TAgg2D.UpdateRasterizerGamma;
begin
  if Assigned(FGammaAgg2D) then
    FGammaAgg2D.Free;

  FGammaAgg2D := TAgg2DRasterizerGamma.Create(FMasterAlpha, FAntiAliasGamma);
  FRasterizer.Gamma(FGammaAgg2D);
end;

procedure TAgg2D.RenderImage(Img: TAgg2DImage; X1, Y1, X2, Y2: Integer;
  Parl: PDouble);
var
  Mtx: TAggTransAffine;
  Interpolator: TAggSpanInterpolatorLinear;
begin
  FRasterizer.Reset;
  FRasterizer.AddPath(FPathTransform);

  Mtx := TAggTransAffine.Create(X1, Y1, X2, Y2, PAggParallelogram(Parl));
  try
    Mtx.Multiply(FTransform);
    Mtx.Invert;

    Interpolator := TAggSpanInterpolatorLinear.Create(Mtx);
    try
      if FBlendMode = bmAlpha then
        Agg2DRendererRenderImage(Self, Img, FRendererBasePre, Interpolator)
      else
        Agg2DRendererRenderImage(Self, Img, FRendererBaseCompPre, Interpolator);
    finally
      Interpolator.Free;
    end;
  finally
    Mtx.Free;
  end;
end;

procedure TAgg2D.RenderImage(Img: TAgg2DImage; Rect: TRectInteger;
  Parl: PDouble);
var
  Mtx: TAggTransAffine;
  Interpolator: TAggSpanInterpolatorLinear;
begin
  FRasterizer.Reset;
  FRasterizer.AddPath(FPathTransform);

  Mtx := TAggTransAffine.Create(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2,
    PAggParallelogram(Parl));
  try
    Mtx.Multiply(FTransform);
    Mtx.Invert;

    Interpolator := TAggSpanInterpolatorLinear.Create(Mtx);
    try
      if FBlendMode = bmAlpha then
        Agg2DRendererRenderImage(Self, Img, FRendererBasePre, Interpolator)
      else
        Agg2DRendererRenderImage(Self, Img, FRendererBaseCompPre, Interpolator);
    finally
      Interpolator.Free;
    end;
  finally
    Mtx.Free;
  end;
end;


{ TAggSpanConvImageBlend }

constructor TAggSpanConvImageBlend.Create(BlendMode: TAggBlendMode; C: TAggColorRgba8;
  P: TAggPixelFormatProcessor);
begin
  FMode := BlendMode;
  FColor := C;
  FPixel := P;
end;

procedure TAggSpanConvImageBlend.Convert(Span: PAggColor; X, Y: Integer;
  Len: Cardinal);
var
  L2, A: Cardinal;
  S2: PAggColorRgba8;
begin
  if FMode <> bmDestination then
  begin
    L2 := Len;
    S2 := PAggColorRgba8(Span);

    repeat
      BlendModeAdaptorClipToDestinationRgbaPre(FPixel, FMode,
        PInt8u(S2), FColor.R, FColor.G, FColor.B, CAggBaseMask, CAggCoverFull);

      Inc(PtrComp(S2), SizeOf(TAggColorRgba8));
      Dec(L2);
    until L2 = 0;
  end;

  if FColor.A < CAggBaseMask then
  begin
    L2 := Len;
    S2 := PAggColorRgba8(Span);
    A := FColor.A;

    repeat
      S2.R := (S2.R * A) shr CAggBaseShift;
      S2.G := (S2.G * A) shr CAggBaseShift;
      S2.B := (S2.B * A) shr CAggBaseShift;
      S2.A := (S2.A * A) shr CAggBaseShift;

      Inc(PtrComp(S2), SizeOf(TAggColorRgba8));
      Dec(L2);
    until L2 = 0;
  end;
end;

function OperatorIsEqual(C1, C2: PAggColorRgba8): Boolean;
begin
  Result := (C1.R = C2.R) and (C1.G = C2.G) and (C1.B = C2.B) and (C1.A = C2.A);
end;

function OperatorIsNotEqual(C1, C2: PAggColorRgba8): Boolean;
begin
  Result := not OperatorIsEqual(C1, C2);
end;

procedure Agg2DRendererRender(Gr: TAgg2D; RendererBase: TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid; FillColor: Boolean);
var
  Span: TAggSpanGradient;
  Ren : TAggRendererScanLineAA;
  Clr : TAggColor;
begin
  if (FillColor and (Gr.FFillGradientFlag = grdLinear)) or
    (not FillColor and (Gr.FLineGradientFlag = grdLinear)) then
    if FillColor then
    begin
      Span := TAggSpanGradient.Create(Gr.FAllocator,
        Gr.FFillGradientInterpolator, Gr.FLinearGradientFunction,
        Gr.FFillGradient, Gr.FFillGradientD1, Gr.FFillGradientD2);
      try
        Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
        try
          RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
        finally
          Ren.Free;
        end;
      finally
        Span.Free;
      end;
    end
    else
    begin
      Span := TAggSpanGradient.Create(Gr.FAllocator,
        Gr.FLineGradientInterpolator, Gr.FLinearGradientFunction,
        Gr.FLineGradient, Gr.FLineGradientD1, Gr.FLineGradientD2);
      try
        Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
        try
          RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
        finally
          Ren.Free;
        end;
      finally
        Span.Free;
      end;
    end
  else if (FillColor and (Gr.FFillGradientFlag = grdRadial)) or
    (not FillColor and (Gr.FLineGradientFlag = grdRadial)) then
    if FillColor then
    begin
      Span := TAggSpanGradient.Create(Gr.FAllocator,
        Gr.FFillGradientInterpolator, Gr.FRadialGradientFunction,
        Gr.FFillGradient, Gr.FFillGradientD1, Gr.FFillGradientD2);
      try
        Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
        try
          RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
        finally
          Ren.Free;
        end;
      finally
        Span.Free;
      end;
    end
    else
    begin
      Span := TAggSpanGradient.Create(Gr.FAllocator,
        Gr.FLineGradientInterpolator, Gr.FRadialGradientFunction,
        Gr.FLineGradient, Gr.FLineGradientD1, Gr.FLineGradientD2);
      try
        Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
        try
          RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
        finally
          Ren.Free;
        end;
      finally
        Span.Free;
      end;
    end
  else
  begin
    if FillColor then
      Clr.FromRgba8(Gr.FFillColor)
    else
      Clr.FromRgba8(Gr.FLineColor);

    RenSolid.SetColor(@Clr);
    RenderScanLines(Gr.FRasterizer, Gr.FScanLine, RenSolid);
  end;
end;

procedure Agg2DRendererRender(Gr: TAgg2D; RendererBase: TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid; Ras: TAggGray8Adaptor;
  Sl: TAggGray8ScanLine);
var
  Span: TAggSpanGradient;
  Ren : TAggRendererScanLineAA;
  Clr : TAggColor;
begin
  if Gr.FFillGradientFlag = grdLinear then
  begin
    Span := TAggSpanGradient.Create(Gr.FAllocator, Gr.FFillGradientInterpolator,
      Gr.FLinearGradientFunction, Gr.FFillGradient, Gr.FFillGradientD1,
      Gr.FFillGradientD2);
    try
      Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
      try
        RenderScanLines(Ras, Sl, Ren);
      finally
        Ren.Free;
      end;
    finally
      Span.Free;
    end;
  end
  else if Gr.FFillGradientFlag = grdRadial then
  begin
    Span := TAggSpanGradient.Create(Gr.FAllocator, Gr.FFillGradientInterpolator,
      Gr.FRadialGradientFunction, Gr.FFillGradient, Gr.FFillGradientD1,
      Gr.FFillGradientD2);
    try
      Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
      try
        RenderScanLines(Ras, Sl, Ren);
      finally
        Ren.Free;
      end;
    finally
      Span.Free;
    end;
  end
  else
  begin
    Clr.FromRgba8(Gr.FFillColor);
    RenSolid.SetColor(@Clr);
    RenderScanLines(Ras, Sl, RenSolid);
  end;
end;

procedure Agg2DRendererRenderImage(Gr: TAgg2D; Img: TAgg2DImage;
  RendererBase: TAggRendererBase; Interpolator: TAggSpanInterpolatorLinear);
var
  Blend: TAggSpanConvImageBlend;

  Si: TAggSpanImageFilterRgba;
  Sg: TAggSpanImageFilterRgbaNN;
  Sb: TAggSpanImageFilterRgbaBilinear;
  S2: TAggSpanImageFilterRgba2x2;
  Sa: TAggSpanImageResampleRgbaAffine;
  Sc: TAggSpanConverter;
  Ri: TAggRendererScanLineAA;
  Clr: TAggColor;
  Resample: Boolean;
  Sx, Sy: Double;
begin
  Blend := TAggSpanConvImageBlend.Create(Gr.FImageBlendMode,
    Gr.FImageBlendColor, Gr.FPixelFormatCompPre);
  try
    if Gr.FImageFilter = ifNoFilter then
    begin
      Clr.Clear;
      Sg := TAggSpanImageFilterRgbaNN.Create(Gr.FAllocator, Img.FRenderingBuffer,
        @Clr, Interpolator, CAggOrderRgba);
      try
        Sc := TAggSpanConverter.Create(Sg, Blend);
        try
          Ri := TAggRendererScanLineAA.Create(RendererBase, Sc);
          try
            RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
          finally
            Ri.Free;
          end;
        finally
          Sc.Free;
        end;
      finally
        Sg.Free;
      end;
    end
    else
    begin
      Resample := Gr.FImageResample = irAlways;

      if Gr.FImageResample = irOnZoomOut then
      begin
        Interpolator.Transformer.GetScalingAbs(Sx, Sy);

        if (Sx > 1.125) or (Sy > 1.125) then
          Resample := True;
      end;

      if Resample then
      begin
        Clr.Clear;
        Sa := TAggSpanImageResampleRgbaAffine.Create(Gr.FAllocator,
          Img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT,
          CAggOrderRgba);
        try
          Sc := TAggSpanConverter.Create(Sa, Blend);
          try
            Ri := TAggRendererScanLineAA.Create(RendererBase, Sc);
            try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
            finally
              Ri.Free;
            end;
          finally
            Sc.Free;
          end;
        finally
          Sa.Free;
        end;
      end
      else if Gr.FImageFilter = ifBilinear then
      begin
        Clr.Clear;
        Sb := TAggSpanImageFilterRgbaBilinear.Create(Gr.FAllocator,
          Img.FRenderingBuffer, @Clr, Interpolator, CAggOrderRgba);
        try
          Sc := TAggSpanConverter.Create(Sb, Blend);
          try
            Ri := TAggRendererScanLineAA.Create(RendererBase, Sc);
            try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
            finally
              Ri.Free;
            end;
          finally
            Sc.Free;
          end;
        finally
          Sb.Free;
        end;
      end
      else if Gr.FImageFilterLUT.Diameter = 2 then
      begin
        Clr.Clear;
        S2 := TAggSpanImageFilterRgba2x2.Create(Gr.FAllocator,
          Img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT,
          CAggOrderRgba);
        try
          Sc := TAggSpanConverter.Create(S2, Blend);
          try
            Ri := TAggRendererScanLineAA.Create(RendererBase, Sc);
            try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
            finally
              Ri.Free;
            end;
          finally
            Sc.Free;
          end;
        finally
          S2.Free;
        end;
      end
      else
      begin
        Clr.Clear;
        Si := TAggSpanImageFilterRgba.Create(Gr.FAllocator, Img.FRenderingBuffer,
          @Clr, Interpolator, Gr.FImageFilterLUT, CAggOrderRgba);
        try
          Sc := TAggSpanConverter.Create(Si, Blend);
          try
            Ri := TAggRendererScanLineAA.Create(RendererBase, Sc);
            try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
            finally
              Ri.Free;
            end;
          finally
            Sc.Free;
          end;
        finally
          Si.Free;
        end;
      end;
    end;
  finally
    Blend.Free;
  end;
end;

function Agg2DUsesFreeType: Boolean;
begin
{$IFDEF AGG2D_USE_FREETYPE}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.
