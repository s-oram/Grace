program Compositing2;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggColor in '..\..\Source\AggColor.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggRoundedRect in '..\..\Source\AggRoundedRect.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggArray in '..\..\Source\AggArray.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderAlphaSource: TAggControlSlider;
    FSliderAlphaDestination: TAggControlSlider;
    FRadioBoxBlendMode: TAggControlRadioBox;

    FRamp: array [0..1] of TAggPodAutoArray;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine : TAggScanLineUnpacked8;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure RadialShape(RendererBase: TAggRendererBase;
      Colors: TAggCustomArray; Rect: TRectDouble);

    procedure RenderScene;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSliderAlphaDestination := TAggControlSlider.Create(5, 5, 400, 11,
    not FlipY);
  FSliderAlphaSource := TAggControlSlider.Create(5, 20, 400, 26, not FlipY);
  FRadioBoxBlendMode := TAggControlRadioBox.Create(420, 5, 590, 395, not FlipY);

  FRamp[0] := TAggPodAutoArray.Create(256, SizeOf(TAggColor));
  FRamp[1] := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;

  FSliderAlphaDestination.Caption := 'Dst Alpha=%.2f';
  FSliderAlphaDestination.Value := 1.0;

  AddControl(FSliderAlphaDestination);

  FSliderAlphaSource.Caption := 'Src Alpha=%.2f';
  FSliderAlphaSource.Value := 1.0;

  AddControl(FSliderAlphaSource);

  FRadioBoxBlendMode.SetTextSize(6.8);
  FRadioBoxBlendMode.AddItem('clear');
  FRadioBoxBlendMode.AddItem('src');
  FRadioBoxBlendMode.AddItem('dst');
  FRadioBoxBlendMode.AddItem('src-over');
  FRadioBoxBlendMode.AddItem('dst-over');
  FRadioBoxBlendMode.AddItem('src-in');
  FRadioBoxBlendMode.AddItem('dst-in');
  FRadioBoxBlendMode.AddItem('src-out');
  FRadioBoxBlendMode.AddItem('dst-out');
  FRadioBoxBlendMode.AddItem('src-atop');
  FRadioBoxBlendMode.AddItem('dst-atop');
  FRadioBoxBlendMode.AddItem('xor');
  FRadioBoxBlendMode.AddItem('plus');
  FRadioBoxBlendMode.AddItem('minus');
  FRadioBoxBlendMode.AddItem('multiply');
  FRadioBoxBlendMode.AddItem('screen');
  FRadioBoxBlendMode.AddItem('overlay');
  FRadioBoxBlendMode.AddItem('darken');
  FRadioBoxBlendMode.AddItem('lighten');
  FRadioBoxBlendMode.AddItem('color-dodge');
  FRadioBoxBlendMode.AddItem('color-burn');
  FRadioBoxBlendMode.AddItem('hard-light');
  FRadioBoxBlendMode.AddItem('soft-light');
  FRadioBoxBlendMode.AddItem('difference');
  FRadioBoxBlendMode.AddItem('exclusion');
  FRadioBoxBlendMode.AddItem('contrast');
  FRadioBoxBlendMode.AddItem('invert');
  FRadioBoxBlendMode.AddItem('invert-rgb');
  FRadioBoxBlendMode.SetCurrentItem(3);

  AddControl(FRadioBoxBlendMode);
end;

destructor TAggApplication.Destroy;
begin
  FSliderAlphaDestination.Free;
  FSliderAlphaSource.Free;
  FRadioBoxBlendMode.Free;

  FRamp[0].Free;
  FRamp[1].Free;

  FRasterizer.Free;
  FScanLine.Free;

  inherited;
end;

procedure TAggApplication.RadialShape(RendererBase: TAggRendererBase;
  Colors: TAggCustomArray; Rect: TRectDouble);
var
  GradientRadial: TAggGradientRadial;
  GradientMatrix: TAggTransAffine;
  SpanInterpolator: TAggSpanInterpolatorLinear;
  SpanAllocator: TAggSpanAllocator;
  SpanGradient: TAggSpanGradient;

  Center: TPointDouble;
  R: Double;

  Circle: TAggCircle;

  Trans: TAggConvTransform;

  Rg: TAggRendererScanLineAA;
begin
  GradientRadial := TAggGradientRadial.Create;
  try
    GradientMatrix := TAggTransAffine.Create;
    try
      SpanInterpolator := TAggSpanInterpolatorLinear.Create(GradientMatrix);
      SpanAllocator := TAggSpanAllocator.Create;
      SpanGradient := TAggSpanGradient.Create(SpanAllocator, SpanInterpolator,
        GradientRadial, Colors, 0, 100);

      Center.X := Rect.CenterX;
      Center.Y := Rect.CenterY;

      if (Rect.X2 - Rect.X1) < (Rect.Y2 - Rect.Y1) then
        R := 0.5 * (Rect.X2 - Rect.X1)
      else
        R := 0.5 * (Rect.Y2 - Rect.Y1);

      GradientMatrix.Scale(R * 0.01);
      GradientMatrix.Translate(Center.X, Center.Y);
      GradientMatrix.Multiply(GetTransAffineResizing);
      GradientMatrix.Invert;

      Circle := TAggCircle.Create(Center, R, 100);
      try
        Trans := TAggConvTransform.Create(Circle, GetTransAffineResizing);
        try
          FRasterizer.AddPath(Trans);
          Rg := TAggRendererScanLineAA.Create(RendererBase, SpanGradient);
          try
            RenderScanLines(FRasterizer, FScanLine, Rg);
          finally
            Rg.Free;
          end;
        finally
          Trans.Free;
        end;
      finally
        Circle.Free;
      end;
      SpanAllocator.Free;
      SpanGradient.Free;
      SpanInterpolator.Free;
    finally
      GradientMatrix.Free;
    end;
  finally
    GradientRadial.Free;
  end;
end;

procedure TAggApplication.RenderScene;
var
  Pixf: TAggPixelFormatProcessor;
  Ren : TAggRendererBase;

  Top: TPointDouble;
begin
  PixelFormatCustomBlendRgba(Pixf, RenderingBufferWindow, @BlendModeAdaptorRgba,
    CAggOrderBgra);
  Ren := TAggRendererBase.Create(Pixf, True);
  try
    Top.X := 50;
    Top.Y := 50;
    Pixf.BlendMode := TAggBlendMode(bmDifference);

    RadialShape(Ren, FRamp[0], RectDouble(Top, PointDoubleOffset(Top, 320)));

    Pixf.BlendMode := TAggBlendMode(FRadioBoxBlendMode.GetCurrentItem);

    RadialShape(Ren, FRamp[1], RectDouble(Top.X + 50, Top.Y + 50,
      Top.X + 190, Top.Y + 190));
    RadialShape(Ren, FRamp[1], RectDouble(Top.X + 130, Top.Y + 50,
      Top.X + 270, Top.Y + 190));
    RadialShape(Ren, FRamp[1], RectDouble(Top.X + 50, Top.Y + 130,
      Top.X + 190, Top.Y + 270));
    RadialShape(Ren, FRamp[1], RectDouble(Top.X + 130, Top.Y + 130,
      Top.X + 270, Top.Y + 270));
  finally
    Ren.Free;
  end;
end;

procedure GenerateColorRamp(C: TAggPodAutoArray;
  C1, C2, C3, C4: PAggColor);
var
  I: Cardinal;
begin
  I := 0;

  while I < 85 do
  begin
    PAggColor(C[I])^ := Gradient(C1^, C2^, I / 85.0);

    Inc(I);
  end;

  while I < 170 do
  begin
    PAggColor(C[I])^ := Gradient(C2^, C3^, (I - 85) / 85.0);

    Inc(I);
  end;

  while I < 256 do
  begin
    PAggColor(C[I])^ := Gradient(C3^, C4^, (I - 170) / 85.0);

    Inc(I);
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  Rb: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  C1, C2, C3, C4: TAggColor;
begin
  // Initialize structures
  // PixelFormatAlphaBlendRgba(Pixf, RenderingBufferWindow, CAggOrderBgra); {!}
  PixelFormatBgra32(Pixf, RenderingBufferWindow);

  Rb := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(Rb);
    try
      Rgba.White;
      Rb.Clear(@Rgba);

      // Render
      C1.FromRgbaDouble(0, 0, 0, FSliderAlphaDestination.Value);
      C2.FromRgbaDouble(0, 0, 1, FSliderAlphaDestination.Value);
      C3.FromRgbaDouble(0, 1, 0, FSliderAlphaDestination.Value);
      C4.FromRgbaDouble(1, 0, 0, 0);

      GenerateColorRamp(FRamp[0], @C1, @C2, @C3, @C4);

      C1.FromRgbaDouble(0, 0, 0, FSliderAlphaSource.Value);
      C2.FromRgbaDouble(0, 0, 1, FSliderAlphaSource.Value);
      C3.FromRgbaDouble(0, 1, 0, FSliderAlphaSource.Value);
      C4.FromRgbaDouble(1, 0, 0, 0);

      GenerateColorRamp(FRamp[1], @C1, @C2, @C3, @C4);

      RenderScene;

      // Render the controls
      RenderControl(FRasterizer, FScanLine, RenScan, FSliderAlphaDestination);
      RenderControl(FRasterizer, FScanLine, RenScan, FSliderAlphaSource);
      RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxBlendMode);
    finally
      RenScan.Free;
    end;
  finally
    Rb.Free
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Another demo example with extended compositing modes.');
end;

begin
  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example. Compositing Modes (F1-Help)';

    if Init(600, 400, [wfResize, wfKeepAspectRatio]) then
      Run;
  finally
    Free;
  end;
end.
