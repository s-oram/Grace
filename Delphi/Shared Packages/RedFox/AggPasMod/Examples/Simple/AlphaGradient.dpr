program AlphaGradient;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics,

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSplineControl in '..\..\Source\Controls\AggSplineControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggArray in '..\..\Source\AggArray.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanGradientAlpha in '..\..\Source\AggSpanGradientAlpha.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanConverter in '..\..\Source\AggSpanConverter.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggVcgenStroke in '..\..\Source\AggVcgenStroke.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggMath in '..\..\Source\AggMath.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FPoint: array [0..2] of TPointDouble;
    FDelta: TPointDouble;
    FIndex: Integer;
    FSplineControlAlpha: TSplineControl;

    FPixelFormat: TAggPixelFormatProcessor;
    FRendererBase: TAggRendererBase;
    FRenSolid : TAggRendererScanLineAASolid;
    FScanLine: TAggScanLineUnpacked8;
    FRasterizer: TAggRasterizerScanLineAA;

    FGradient: TAggGradientCircle;
    FAlphaGradient: TAggGradientXY;

    FSpanAllocator: TAggSpanAllocator;
    FEllipse: TAggEllipse;
    FGradientMatrix: TAggTransAffine;
    FAlphaMatrix: TAggTransAffine;
  protected
    procedure FillColorArray(AArray: TAggPodAutoArray;
      ABegin, AMiddle, AEnd: PAggColor);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;
    procedure OnResize(Width: Integer; Height: Integer); override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  PixelFormatBgr24(FPixelFormat, RenderingBufferWindow);
  FRendererBase := TAggRendererBase.Create(FPixelFormat, True);
  FRenSolid := TAggRendererScanLineAASolid.Create(FRendererBase);

  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FEllipse := TAggEllipse.Create;
  FAlphaMatrix := TAggTransAffine.Create;
  FGradientMatrix := TAggTransAffine.Create;
  FGradient := TAggGradientCircle.Create;
  FAlphaGradient := TAggGradientXY.Create;

  FSpanAllocator := TAggSpanAllocator.Create;

  FSplineControlAlpha := TSplineControl.Create(2, 2, 200, 30, 6, not FlipY);
  FSplineControlAlpha.SetPoint(0, 0, 0);
  FSplineControlAlpha.SetPoint(1, 1 / 5, 1 / 5);
  FSplineControlAlpha.SetPoint(2, 2 / 5, 2 / 5);
  FSplineControlAlpha.SetPoint(3, 3 / 5, 3 / 5);
  FSplineControlAlpha.SetPoint(4, 4 / 5, 4 / 5);
  FSplineControlAlpha.SetPoint(5, 1, 1);
  FSplineControlAlpha.UpdateSpline;
  AddControl(FSplineControlAlpha);

  FIndex := -1;

  FPoint[0] := PointDouble(257, 60);
  FPoint[1] := PointDouble(369, 170);
  FPoint[2] := PointDouble(143, 310);
end;

destructor TAggApplication.Destroy;
begin
  FSplineControlAlpha.Free;

  FGradient.Free;
  FAlphaGradient.Free;

  FSpanAllocator.Free;

  FGradientMatrix.Free;
  FAlphaMatrix.Free;
  FEllipse.Free;
  FRasterizer.Free;
  FScanLine.Free;
  FRenSolid.Free;
  FRendererBase.Free;

  inherited;
end;

procedure TAggApplication.FillColorArray(AArray: TAggPodAutoArray;
  ABegin, AMiddle, AEnd: PAggColor);
var
  I: Cardinal;
const
  CScale: Double = 1 / 128.0;
begin
  I := 0;

  while I < 128 do
  begin
    PAggColor(AArray[I])^ := Gradient(ABegin^, AMiddle^, I * CScale);

    Inc(I);
  end;

  while I < 256 do
  begin
    PAggColor(AArray[I])^ := Gradient(AMiddle^, AEnd^, (I - 128) * CScale);

    Inc(I);
  end;
end;

procedure TAggApplication.OnDraw;
var
  Rgba, Rgbb, Rgbc: TAggColor;

  I, W, H: Cardinal;

  Parallelogram: TAggParallelogram;

  Stroke: TAggVcgenStroke;

  // The gradient objects declarations
  SpanInterpolator, // Span Gradient interpolator
  SpanInterpolatorAlpha: TAggSpanInterpolatorLinear; // Span alpha interpolator
  ColorArray: TAggPodAutoArray; // The Gradient colors

  SpanGradient: TAggSpanGradient;
  AlphaArray: TAggGradientAlpha;
  SpanGradientAlpha: TAggSpanGradientAlpha;
  SpanConv: TAggSpanConverter;
  RenGradient: TAggRendererScanLineAA;
begin
  FRendererBase.Clear(CRgba8White);

  // Draw some background
  RandSeed := 1234;

  W := Trunc(Width);
  H := Trunc(Height);

  for I := 0 to 99 do
  begin
    FEllipse.Initialize(Random(W), Random(H), Random(65), Random(65), 50);

    Rgba.FromRgbaDouble(Random, Random, Random, 0.5 * Random);

    FRenSolid.SetColor(@Rgba);
    FRasterizer.AddPath(FEllipse);
    RenderScanLines(FRasterizer, FScanLine, FRenSolid);
  end;

  Parallelogram[0] := FPoint[0].X;
  Parallelogram[1] := FPoint[0].Y;
  Parallelogram[2] := FPoint[1].X;
  Parallelogram[3] := FPoint[1].Y;
  Parallelogram[4] := FPoint[2].X;
  Parallelogram[5] := FPoint[2].Y;

  // The gradient objects initializations
  FGradientMatrix.Reset;
  FAlphaMatrix.Reset;
  SpanInterpolator := TAggSpanInterpolatorLinear.Create(FGradientMatrix);
  SpanInterpolatorAlpha := TAggSpanInterpolatorLinear.Create(FAlphaMatrix);
  ColorArray := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  // Initialize the Gradient Span itself.
  // The last two arguments are so called "d1" and "d2"
  // defining two distances in pixels, where the Gradient starts
  // and where it ends. The actual meaning of "d1" and "d2" depands
  // on the Gradient function.
  SpanGradient := TAggSpanGradient.Create(FSpanAllocator, SpanInterpolator,
    FGradient, ColorArray, 0, 150);

  AlphaArray := TAggGradientAlpha.Create(256, SizeOf(Int8u));

  SpanGradientAlpha := TAggSpanGradientAlpha.Create(SpanInterpolatorAlpha,
    FAlphaGradient, AlphaArray, 0, 100);
  try
    // Span converter initialization
    SpanConv := TAggSpanConverter.Create(SpanGradient, SpanGradientAlpha);

    // The Gradient Renderer
    RenGradient := TAggRendererScanLineAA.Create(FRendererBase, SpanConv);

    // Finally we can draw a circle
    FGradientMatrix.Scale(0.75, 1.2);
    FGradientMatrix.Rotate(Pi / 3);
    FGradientMatrix.Translate(Width * 0.5, Height * 0.5);
    FGradientMatrix.Invert;

    FAlphaMatrix.ParlToRect(@Parallelogram, -100, -100, 100, 100);

    Rgba.FromRgbaDouble(0, 0.19, 0.19);
    Rgbb.FromRgbaDouble(0.7, 0.7, 0.19);
    Rgbc.FromRgbaDouble(0.31, 0, 0);

    FillColorArray(ColorArray, @Rgba, @Rgbb, @Rgbc);

    // Fill Alpha array
    for I := 0 to 255 do
      PInt8u(AlphaArray[I])^ := Int8u(Trunc(FSplineControlAlpha.GetValue(
        I / 255.0) * CAggBaseMask));

    FEllipse.Initialize(PointDouble(Width * 0.5, Height * 0.5),
      PointDouble(150), 100);

    FRasterizer.AddPath(FEllipse);
    RenderScanLines(FRasterizer, FScanLine, RenGradient);

    // Draw the control points and the parallelogram
    Rgba.FromRgbaDouble(0, 0.4, 0.4, 0.31);
    FRenSolid.SetColor(@Rgba);

    FEllipse.Initialize(FPoint[0], PointDouble(5), 20);
    FRasterizer.AddPath(FEllipse);
    RenderScanLines(FRasterizer, FScanLine, FRenSolid);

    FEllipse.Initialize(FPoint[1], PointDouble(5), 20);
    FRasterizer.AddPath(FEllipse);
    RenderScanLines(FRasterizer, FScanLine, FRenSolid);

    FEllipse.Initialize(FPoint[2], PointDouble(5), 20);
    FRasterizer.AddPath(FEllipse);
    RenderScanLines(FRasterizer, FScanLine, FRenSolid);

    Stroke := TAggVcgenStroke.Create;
    try
      Stroke.AddVertex(FPoint[0].X, FPoint[0].Y, CAggPathCmdMoveTo);
      Stroke.AddVertex(FPoint[1].X, FPoint[1].Y, CAggPathCmdLineTo);
      Stroke.AddVertex(FPoint[2].X, FPoint[2].Y, CAggPathCmdLineTo);
      Stroke.AddVertex(FPoint[0].X + FPoint[2].X - FPoint[1].X,
        FPoint[0].Y + FPoint[2].Y - FPoint[1].Y, CAggPathCmdLineTo);
      Stroke.AddVertex(0, 0, CAggPathCmdEndPoly or CAggPathFlagsClose);

      FRenSolid.SetColor(CRgba8Black);
      FRasterizer.AddPath(Stroke);
    finally
      Stroke.Free;
    end;
    RenderScanLines(FRasterizer, FScanLine, FRenSolid);

    // Render the controls
    RenderControl(FRasterizer, FScanLine, FRenSolid, FSplineControlAlpha);

    // Free AGG resources
    ColorArray.Free;
    AlphaArray.Free;

    RenGradient.Free;
    SpanConv.Free;
    SpanGradient.Free;
  finally
    SpanGradientAlpha.Free;
  end;

  SpanInterpolator.Free;
  SpanInterpolatorAlpha.Free;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Delta: TPointDouble;
begin
  if mkfMouseLeft in Flags then
  begin
    if FIndex = 3 then
    begin
      Delta.X := X - FDelta.X;
      Delta.Y := Y - FDelta.Y;

      FPoint[1].X := FPoint[1].X - (FPoint[0].X - Delta.X);
      FPoint[1].Y := FPoint[1].Y - (FPoint[0].Y - Delta.Y);
      FPoint[2].X := FPoint[2].X - (FPoint[0].X - Delta.X);
      FPoint[2].Y := FPoint[2].Y - (FPoint[0].Y - Delta.Y);
      FPoint[0] := Delta;

      ForceRedraw;

      Exit;
    end;

    if FIndex >= 0 then
    begin
      FPoint[FIndex].X := X - FDelta.X;
      FPoint[FIndex].Y := Y - FDelta.Y;

      ForceRedraw;
    end;
  end
  else
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
begin
  inherited;
  FRendererBase.SetClipBox(0, 0, Width, Height);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  I: Cardinal;
begin
  if mkfMouseLeft in Flags then
  begin
    I := 0;

    while I < 3 do
    begin
      if Hypot(X - FPoint[I].X, Y - FPoint[I].Y) < 10.0 then
      begin
        FDelta.X := X - FPoint[I].X;
        FDelta.Y := Y - FPoint[I].Y;
        FIndex := I;

        Break;
      end;

      Inc(I);
    end;

    if I = 3 then
      if PointInTriangle(FPoint[0], FPoint[1], FPoint[2], X, Y) then
      begin
        FDelta.X := X - FPoint[0].X;
        FDelta.Y := Y - FPoint[0].Y;
        FIndex := 3;
      end;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FIndex := -1;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
var
  Delta: TPointDouble;
begin
  Delta.X := 0;
  Delta.Y := 0;

  case TKeyCode(Key) of
    kcLeft:
      Delta.X := -0.1;
    kcRight:
      Delta.X := 0.1;
    kcUp:
      Delta.Y := 0.1;
    kcDown:
      Delta.Y := -0.1;
  end;

  FPoint[0].X := FPoint[0].X + Delta.X;
  FPoint[0].Y := FPoint[0].Y + Delta.Y;
  FPoint[1].X := FPoint[1].X + Delta.X;
  FPoint[1].Y := FPoint[1].Y + Delta.Y;

  ForceRedraw;

  if Key = Cardinal(kcF1) then
    DisplayMessage('The demo shows how to combine any Span generator with '
      + 'alpha-channel Gradient.'#13#13
      + 'How to play with:'#13#13
      + 'Use the mouse to move the parallelogram around.'#13
      + 'Use the arrow keys to move the parallelogram very precisely.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.  ');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Alpha channel Gradient (F1-Help)';

    if Init(400, 320, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
