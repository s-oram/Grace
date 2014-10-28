program Compositing;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggMath in '..\..\Source\AggMath.pas',
  AggArray in '..\..\Source\AggArray.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggRoundedRect in '..\..\Source\AggRoundedRect.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas';

const
  CFlipY = True;
  CAggBaseShift = AggColor.CAggBaseShift;

type
  TAggGradientLinearColor = class(TAggCustomArray)
  private
    FC1, FC2, FRes: TAggColor;
  protected
    function GetSize: Cardinal; override;
    function ArrayOperator(I: Cardinal): Pointer; override;
  public
    constructor Create; overload;
    constructor Create(C1, C2: PAggColor); overload;

    procedure Colors(C1, C2: PAggColor);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSliderAlphaSource: TAggControlSlider;
    FSliderAlphaDestination: TAggControlSlider;
    FBlendMode: TAggControlRadioBox;
    FPixelFormat: TAggPixelFormatProcessor;
    FScanLine: TAggScanLineUnpacked8;
    FRasterizer: TAggRasterizerScanLineAA;
  protected
    procedure Circle(RendererBase: TAggRendererBase; C1, C2: PAggColor; X1, Y1,
      X2, Y2, ShadowAlpha: Double);
    procedure DestinationShape(RendererBase: TAggRendererBase; C1,
      C2: PAggColor; X1, Y1, X2, Y2: Double);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure RenderScene(RenderingBuffer: TAggRenderingBuffer;
      PixelFormat: TAggPixelFormatProcessor);

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal;
      Flags: TMouseKeyboardFlags); override;
  end;

procedure GradientAffine(out Mtx: TAggTransAffine; X1, Y1, X2, Y2: Double;
  GradientD2: Double = 100.0);
var
  Dx, Dy: Double;
begin
  Mtx := TAggTransAffine.Create;

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  Mtx.Reset;
  Mtx.Scale(Sqrt(Dx * Dx + Dy * Dy) / GradientD2);
  Mtx.Rotate(ArcTan2(Dy, Dx));
  Mtx.Translate(X1, Y1);
  Mtx.Invert;
end;


{ TAggGradientLinearColor }

constructor TAggGradientLinearColor.Create;
begin
end;

constructor TAggGradientLinearColor.Create(C1, C2: PAggColor);
begin
  FC1 := C1^;
  FC2 := C2^;
end;

procedure TAggGradientLinearColor.Colors;
begin
  FC1 := C1^;
  FC2 := C2^;
end;

function TAggGradientLinearColor.GetSize;
begin
  Result := 256
end;

function TAggGradientLinearColor.ArrayOperator;
begin
  I := I shl (CAggBaseShift - 8);

  FRes.Rgba8.R := Int8u((((FC2.Rgba8.R - FC1.Rgba8.R) * I) +
    (FC1.Rgba8.R shl CAggBaseShift)) shr CAggBaseShift);
  FRes.Rgba8.G := Int8u((((FC2.Rgba8.G - FC1.Rgba8.G) * I) +
    (FC1.Rgba8.G shl CAggBaseShift)) shr CAggBaseShift);
  FRes.Rgba8.B := Int8u((((FC2.Rgba8.B - FC1.Rgba8.B) * I) +
    (FC1.Rgba8.B shl CAggBaseShift)) shr CAggBaseShift);
  FRes.Rgba8.A := Int8u((((FC2.Rgba8.A - FC1.Rgba8.A) * I) +
    (FC1.Rgba8.A shl CAggBaseShift)) shr CAggBaseShift);

  Result := @FRes;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  PixelFormatBgra32(FPixelFormat, RenderingBufferWindow);
  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FSliderAlphaSource := TAggControlSlider.Create(5, 5, 400, 11, not FlipY);
  FSliderAlphaDestination := TAggControlSlider.Create(5, 20, 400, 26,
    not FlipY);
  FBlendMode := TAggControlRadioBox.Create(420, 5, 590, 395, not FlipY);

  FSliderAlphaSource.Caption := 'Src Alpha=%.2f';
  FSliderAlphaSource.Value := 1.0;

  AddControl(FSliderAlphaSource);

  FSliderAlphaDestination.Caption := 'Dst Alpha=%.2f';
  FSliderAlphaDestination.Value := 0.75;

  AddControl(FSliderAlphaDestination);

  FBlendMode.SetTextSize(7);
  FBlendMode.AddItem('clear');
  FBlendMode.AddItem('src');
  FBlendMode.AddItem('dst');
  FBlendMode.AddItem('src-over');
  FBlendMode.AddItem('dst-over');
  FBlendMode.AddItem('src-in');
  FBlendMode.AddItem('dst-in');
  FBlendMode.AddItem('src-out');
  FBlendMode.AddItem('dst-out');
  FBlendMode.AddItem('src-atop');
  FBlendMode.AddItem('dst-atop');
  FBlendMode.AddItem('xor');
  FBlendMode.AddItem('plus');
  FBlendMode.AddItem('minus');
  FBlendMode.AddItem('multiply');
  FBlendMode.AddItem('screen');
  FBlendMode.AddItem('overlay');
  FBlendMode.AddItem('darken');
  FBlendMode.AddItem('lighten');
  FBlendMode.AddItem('color-dodge');
  FBlendMode.AddItem('color-burn');
  FBlendMode.AddItem('hard-light');
  FBlendMode.AddItem('soft-light');
  FBlendMode.AddItem('difference');
  FBlendMode.AddItem('exclusion');
  FBlendMode.AddItem('contrast');
  FBlendMode.SetCurrentItem(3);

  AddControl(FBlendMode);
end;

destructor TAggApplication.Destroy;
begin
  FPixelFormat.Free;
  FScanLine.Free;
  FRasterizer.Free;

  FSliderAlphaSource.Free;
  FSliderAlphaDestination.Free;
  FBlendMode.Free;

  inherited;
end;

procedure TAggApplication.Circle(RendererBase: TAggRendererBase;
  C1, C2: PAggColor; X1, Y1, X2, Y2, ShadowAlpha: Double);
var
  ColorFunc: TAggGradientLinearColor;

  GradientFunc: TAggGradientX;
  GradientMatrix: TAggTransAffine;
  SpanGradient: TAggSpanGradient;
  RenGradient: TAggRendererScanLineAA;
  RenSolid: TAggRendererScanLineAASolid;

  SpanInterpolator: TAggSpanInterpolatorLinear;
  SpanAllocator: TAggSpanAllocator;

  Rgba: TAggColor;
  Circle: TAggCircle;

  R: Double;
begin
  GradientFunc := TAggGradientX.Create;
  try
    GradientAffine(GradientMatrix, X1, Y1, X2, Y2, 100);
    try
      SpanInterpolator := TAggSpanInterpolatorLinear.Create(GradientMatrix);
      try
        SpanAllocator := TAggSpanAllocator.Create;
        try
          ColorFunc := TAggGradientLinearColor.Create(C1, C2);
          try
            SpanGradient := TAggSpanGradient.Create(SpanAllocator,
              SpanInterpolator, GradientFunc, ColorFunc, 0, 100);
            try
              RenGradient := TAggRendererScanLineAA.Create(RendererBase,
                SpanGradient);
              try
                R := CalculateDistance(X1, Y1, X2, Y2) * 0.5;

                Circle := TAggCircle.Create((X1 + X2) * 0.5 + 5,
                  (Y1 + Y2) * 0.5 - 3, R, 100);
                try
                  RenSolid := TAggRendererScanLineAASolid.Create(RendererBase);
                  try
                    Rgba.FromRgbaDouble(0.6, 0.6, 0.6, 0.7 * ShadowAlpha);
                    RenSolid.SetColor(@Rgba);
                    FRasterizer.AddPath(Circle);
                    RenderScanLines(FRasterizer, FScanLine, RenSolid);
                  finally
                    RenSolid.Free;
                  end;

                  Circle.Initialize(PointDouble((X1 + X2) * 0.5,
                    (Y1 + Y2) * 0.5), R, 100);
                  FRasterizer.AddPath(Circle);
                finally
                  Circle.Free;
                end;

                RenderScanLines(FRasterizer, FScanLine, RenGradient);
              finally
                RenGradient.Free;
              end;
            finally
              SpanGradient.Free;
            end;
          finally
            ColorFunc.Free;
          end;
        finally
          SpanAllocator.Free;
        end;
      finally
        SpanInterpolator.Free;
      end;
    finally
      GradientMatrix.Free;
    end;
  finally
    GradientFunc.Free;
  end;
end;

procedure TAggApplication.DestinationShape(RendererBase: TAggRendererBase;
  C1, C2: PAggColor; X1, Y1, X2, Y2: Double);
var
  ColorFunc: TAggGradientLinearColor;
  Shape: TAggRoundedRect;

  GradientFunc: TAggGradientX;
  GradientMatrix : TAggTransAffine;
  RenGradient : TAggRendererScanLineAA;

  SpanInterpolator: TAggSpanInterpolatorLinear;
  SpanAllocator: TAggSpanAllocator;
  SpanGradient: TAggSpanGradient;
begin
  GradientFunc := TAggGradientX.Create;
  try
    GradientAffine(GradientMatrix, X1, Y1, X2, Y2, 100);
    try
      SpanInterpolator := TAggSpanInterpolatorLinear.Create(GradientMatrix);
      try
        SpanAllocator := TAggSpanAllocator.Create;
        try
          ColorFunc := TAggGradientLinearColor.Create(C1, C2);
          try
            SpanGradient := TAggSpanGradient.Create(SpanAllocator,
              SpanInterpolator, GradientFunc, ColorFunc, 0, 100);
            try
              RenGradient := TAggRendererScanLineAA.Create(RendererBase,
                SpanGradient);
              try
                Shape := TAggRoundedRect.Create(X1, Y1, X2, Y2, 40);
                try
                  FRasterizer.AddPath(Shape);
                  RenderScanLines(FRasterizer, FScanLine, RenGradient);
                finally
                  Shape.Free;
                end;
              finally
                RenGradient.Free;
              end;
            finally
              SpanGradient.Free;
            end;
          finally
            ColorFunc.Free;
          end;
        finally
          SpanAllocator.Free;
        end;
      finally
        SpanInterpolator.Free;
      end;
    finally
      GradientMatrix.Free;
    end;
  finally
    GradientFunc.Free;
  end;
end;

procedure TAggApplication.RenderScene(RenderingBuffer: TAggRenderingBuffer;
  PixelFormat: TAggPixelFormatProcessor);
var
  RenPixFormats, Pixf: TAggPixelFormatProcessor;
  Renderer, RendererBase: TAggRendererBase;
  Rgba, Rgbb: TAggColor;
  V: Double;
begin
  PixelFormatCustomBlendRgba(RenPixFormats, RenderingBuffer,
    @BlendModeAdaptorRgba, CAggOrderBgra);

  Renderer := TAggRendererBase.Create(RenPixFormats, True);
  RendererBase := TAggRendererBase.Create(PixelFormat);
  try
    PixelFormatBgra32(Pixf, RenderingBufferImage[1]);
    RendererBase.BlendFrom(Pixf, nil, 250, 180, Cardinal(Trunc(
      FSliderAlphaSource.Value * 255)));

    Rgba.FromRgbaInteger($FD, $F0, $6F, Cardinal(Trunc(
      FSliderAlphaSource.Value * 255)));
    Rgbb.FromRgbaInteger($FE, $9F, $34, Cardinal(Trunc(
      FSliderAlphaSource.Value * 255)));

    Circle(RendererBase, @Rgba, @Rgbb, 210, 172, 37 * 3,
      100 + 79 * 3, FSliderAlphaSource.Value);

    RenPixFormats.BlendMode := TAggBlendMode(FBlendMode.GetCurrentItem);

    if FBlendMode.GetCurrentItem = 25 then // Contrast
    begin
      V := FSliderAlphaDestination.Value;

      Rgba.FromRgbaDouble(V, V, V);

      DestinationShape(Renderer, @Rgba, @Rgba, 350, 172, 157,
        100 + 79 * 3);
    end
    else
    begin
      Rgba.FromRgbaInteger($7F, $C1, $FF, Cardinal(Trunc(
        FSliderAlphaDestination.Value * 255)));
      Rgbb.FromRgbaInteger($05, $00, $5F, Cardinal(Trunc(
        FSliderAlphaDestination.Value * 255)));

      DestinationShape(Renderer, @Rgba, @Rgbb, 350, 172, 157,
        100 + 79 * 3);
    end;

    Pixf.Free;
  finally
    Renderer.Free;
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnInit;
begin
end;

procedure TAggApplication.OnDraw;
var
  PixelFormat2, PixelFormatPre: TAggPixelFormatProcessor;

  Rgba: TAggColor;

  RendererBase: array [0..2] of TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;

  Y, X: Cardinal;

  Tm : Double;
  Txt: TAggGsvText;
  PtStr : TAggConvStroke;

begin
  RendererBase[0] := TAggRendererBase.Create(FPixelFormat);
  RendererBase[0].Clear(CRgba8White);

  // render chess pattern
  Y := 0;
  while Y < RendererBase[0].Height do
  begin
    X := ((Y shr 3) and 1) shl 3;

    while X < RendererBase[0].Width do
    begin
      Rgba.FromRgbaInteger($DF, $DF, $DF);

      RendererBase[0].CopyBar(X, Y, X + 7, Y + 7, @Rgba);

      Inc(X, 16);
    end;

    Inc(Y, 8);
  end;

  CreateImage(0, RenderingBufferWindow.Width, RenderingBufferWindow.Height);
  // AggPlatformSupport functionality

  PixelFormatBgra32(PixelFormat2, RenderingBufferImage[0]);
  RendererBase[1] := TAggRendererBase.Create(PixelFormat2, True);

  Rgba.Clear;
  RendererBase[1].Clear(@Rgba);

  PixelFormatBgra32Pre(PixelFormatPre, RenderingBufferWindow);
  RendererBase[2] := TAggRendererBase.Create(PixelFormatPre, True);

  // Render Scene
  StartTimer;

  RenderScene(RenderingBufferImage[0], PixelFormat2);

  Tm := GetElapsedTime;

  RendererBase[2].BlendFrom(PixelFormat2);

  // Render Text
  RenScan := TAggRendererScanLineAASolid.Create(RendererBase[0]);
  try
    Txt := TAggGsvText.Create;
    Txt.SetSize(10.0);

    PtStr := TAggConvStroke.Create(Txt);
    PtStr.Width := 1.5;

    Txt.SetStartPoint(10.0, 35.0);
    Txt.SetText(Format('%3.2f ms', [Tm]));

    FRasterizer.AddPath(PtStr);
    RenScan.SetColor(CRgba8Black);
    RenderScanLines(FRasterizer, FScanLine, RenScan);

    // Render the controls
    RenderControl(FRasterizer, FScanLine, RenScan, FSliderAlphaSource);
    RenderControl(FRasterizer, FScanLine, RenScan, FSliderAlphaDestination);
    RenderControl(FRasterizer, FScanLine, RenScan, FBlendMode);

    // Free AGG resources
    Txt.Free;
    PtStr.Free;
  finally
    RenScan.Free;
  end;

  RendererBase[0].Free;
  RendererBase[2].Free;
  RendererBase[1].Free;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('AGG is fully compatible with all SVG 1.2 extended '
      + 'compositing modes.'#13#13
      + 'How to play with:'#13#13
      + 'Try to change the alpha values of the source an destination '
      + 'images, to see, how a particular operation composes the resulting '
      + 'image.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  ImageName, P, N, X: ShortString;
  Text: AnsiString;

begin
  ImageName := 'compositing';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF }

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example. Compositing Modes (F1-Help)';

    if not LoadImage(1, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'compositing' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(600, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
