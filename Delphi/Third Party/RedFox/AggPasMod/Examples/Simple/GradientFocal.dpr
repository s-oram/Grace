program GradientFocal;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggGradientLut in '..\..\Source\AggGradientLut.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderGamma: TAggControlSlider;
    FCheckBoxExtend, FCheckBoxFrWave: TAggControlCheckBox;

    FScanLine: TAggScanLineUnpacked8;
    FRasterizer: TAggRasterizerScanLineAA;
    FSpanAllocator: TAggSpanAllocator;

    FGradientLUT: TAggGradientLut;
    FGammaLut: TAggGammaLut8;

    FOldGamma: Double;
    FMouse: TPointDouble;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure BuildGradientLUT;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
    procedure OnControlChange; override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  // create and setup gamma slider
  FSliderGamma := TAggControlSlider.Create(5, 5, 340, 12, not FlipY);
  FSliderGamma.SetRange(0.5, 2.5);
  FSliderGamma.Value := 1.8;
  FSliderGamma.Caption := 'Gamma = %.3f';
  AddControl(FSliderGamma);
  FSliderGamma.NoTransform;

  // create and setup "Extended Radial Focus" check box
  FCheckBoxExtend := TAggControlCheckBox.Create(10, 25, 'Extended Radial Focus',
    not FlipY);
  AddControl(FCheckBoxExtend);
  FCheckBoxExtend.NoTransform;


  // create and setup "Wavelength Radial" check box
  FCheckBoxFrWave := TAggControlCheckBox.Create(10, 45, 'Wavelength Radial',
    not FlipY);
  AddControl(FCheckBoxFrWave);
  FCheckBoxFrWave.NoTransform;

  // create and setup gammm look-up table
  FGammaLut := TAggGammaLut8.Create;
  FGammaLut.Gamma := FSliderGamma.Value;

  FOldGamma := FSliderGamma.Value;

  FMouse.X := 200;
  FMouse.Y := 200;

  // create and build gradient look-up table
  FGradientLUT := TAggGradientLut.Create(1024);
  BuildGradientLUT;

  // create scanline, rasterizer and span allocator
  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;
  FSpanAllocator := TAggSpanAllocator.Create;
end;

destructor TAggApplication.Destroy;
begin
  FSpanAllocator.Free;
  FRasterizer.Free;
  FScanLine.Free;

  FGradientLUT.Free;

  FGammaLut.Free;

  FCheckBoxExtend.Free;
  FCheckBoxFrWave.Free;
  FSliderGamma.Free;

  inherited;
end;

procedure TAggApplication.BuildGradientLUT;
var
  Rgba: TAggColor;
begin
  FGradientLUT.RemoveAll;

  if not FCheckBoxFrWave.Status then
  begin
    Rgba.FromRgbaInteger(FGammaLut.Dir[0], FGammaLut.Dir[255], FGammaLut.Dir[0]);
    FGradientLUT.AddColor(0, @Rgba);

    Rgba.FromRgbaInteger(FGammaLut.Dir[120], FGammaLut.Dir[0], FGammaLut.Dir[0]);
    FGradientLUT.AddColor(0.2, @Rgba);

    Rgba.FromRgbaInteger(FGammaLut.Dir[120], FGammaLut.Dir[120], FGammaLut.Dir[0]);
    FGradientLUT.AddColor(0.7, @Rgba);

    Rgba.FromRgbaInteger(FGammaLut.Dir[0], FGammaLut.Dir[0], FGammaLut.Dir[255]);
    FGradientLUT.AddColor(1, @Rgba);
  end
  else
  begin
    Rgba.FromWavelength(380, FSliderGamma.Value);
    FGradientLUT.AddColor(0, @Rgba);

    Rgba.FromWavelength(420, FSliderGamma.Value);
    FGradientLUT.AddColor(0.1, @Rgba);

    Rgba.FromWavelength(460, FSliderGamma.Value);
    FGradientLUT.AddColor(0.2, @Rgba);

    Rgba.FromWavelength(500, FSliderGamma.Value);
    FGradientLUT.AddColor(0.3, @Rgba);

    Rgba.FromWavelength(540, FSliderGamma.Value);
    FGradientLUT.AddColor(0.4, @Rgba);

    Rgba.FromWavelength(580, FSliderGamma.Value);
    FGradientLUT.AddColor(0.5, @Rgba);

    Rgba.FromWavelength(620, FSliderGamma.Value);
    FGradientLUT.AddColor(0.6, @Rgba);

    Rgba.FromWavelength(660, FSliderGamma.Value);
    FGradientLUT.AddColor(0.7, @Rgba);

    Rgba.FromWavelength(700, FSliderGamma.Value);
    FGradientLUT.AddColor(0.8, @Rgba);

    Rgba.FromWavelength(740, FSliderGamma.Value);
    FGradientLUT.AddColor(0.9, @Rgba);

    Rgba.FromWavelength(780, FSliderGamma.Value);
    FGradientLUT.AddColor(1, @Rgba);
  end;

  FGradientLUT.BuildLut;
end;

procedure TAggApplication.OnInit;
begin
  FMouse.Y := FInitialHeight * 0.5;
  FMouse.X := FInitialWidth * 0.5;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Rg: TAggRendererScanLineAA;

  Circle: TAggCircle;

  Estr  : TAggConvStroke;
  Etrans: TAggConvTransform;

  Center: TPointDouble;
  R, Fx, Fy, Tm: Double;

  GfStd: TAggGradientRadialFocus;
  GfExt: TAggGradientRadialFocusExtended;

  GradientAdaptor: TAggGradientReflectAdaptor;

  GradientMatrix: TAggTransAffine;

  SpanInterpolator: TAggSpanInterpolatorLinear;

  SpanGradient: TAggSpanGradient;

  Txt: TAggGsvText;
  Pt: TAggConvStroke;
begin
  GfExt := nil;
  GfStd := nil;

  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Gradient center. All Gradient functions assume the
      // center being in the origin (0,0) and you can't
      // change it. But you can apply arbitrary transformations
      // to the Gradient (see below).
      Center := PointDouble(FInitialWidth * 0.5, FInitialHeight * 0.5);
      R := 100;

      // Focal center. Defined in the Gradient coordinates,
      // that is, with respect to the origin (0,0)
      Fx := FMouse.X - Center.X;
      Fy := FMouse.Y - Center.Y;

      if FCheckBoxExtend.Status then
      begin
        GfExt := TAggGradientRadialFocusExtended.Create(R, Fx, Fy);
        GradientAdaptor := TAggGradientReflectAdaptor.Create(GfExt);
      end
      else
      begin
        GfStd := TAggGradientRadialFocus.Create(R, Fx, Fy);
        GradientAdaptor := TAggGradientReflectAdaptor.Create(GfStd);
      end;

      GradientMatrix := TAggTransAffine.Create;

      // Making the affine matrix. Move to (Center.X, Center.Y),
      // apply the resizing transformations and invert
      // the matrix. Gradients and images always assume the
      // inverse transformations.
      GradientMatrix.Translate(Center);
      GradientMatrix.Multiply(GetTransAffineResizing);
      GradientMatrix.Invert;

      SpanInterpolator := TAggSpanInterpolatorLinear.Create(GradientMatrix);
      try
        SpanGradient := TAggSpanGradient.Create(FSpanAllocator, SpanInterpolator,
          GradientAdaptor, FGradientLUT, 0, R);

        // Form the simple rectangle
        FRasterizer.Reset;
        FRasterizer.MoveToDouble(0, 0);
        FRasterizer.LineToDouble(Width, 0);
        FRasterizer.LineToDouble(Width, Height);
        FRasterizer.LineToDouble(0, Height);

        // Render the Gradient to the whole screen and measure the time
        StartTimer;

        Rg := TAggRendererScanLineAA.Create(RendererBase, SpanGradient);
        try
          RenderScanLines(FRasterizer, FScanLine, Rg);

          Tm := GetElapsedTime;

          // Draw the transformed circle that shows the Gradient boundary
          Circle := TAggCircle.Create(Center, R);
          Estr := TAggConvStroke.Create(Circle);
          Etrans := TAggConvTransform.Create(Estr, GetTransAffineResizing);

          FRasterizer.AddPath(Etrans);

          RenScan.SetColor(CRgba8White);

          RenderScanLines(FRasterizer, FScanLine, RenScan);

          // Show the Gradient time
          Txt := TAggGsvText.Create;
          Txt.SetSize(10);

          Pt := TAggConvStroke.Create(Txt);
          Pt.Width := 1.5;

          Txt.SetStartPoint(25, 70);
          Txt.SetText(Format('%3.2f ms', [Tm]));

          FRasterizer.AddPath(Pt);

          RenScan.SetColor(CRgba8Black);

          RenderScanLines(FRasterizer, FScanLine, RenScan);

          // Render the controls
          RenderControl(FRasterizer, FScanLine, RenScan, FSliderGamma);
          RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxExtend);
          RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxFrWave);

          // Apply the inverse Gamma to the whole buffer
          // (transform the colors to the perceptually uniform space)
          Pixf.ApplyGammaInv(FGammaLut, CAggOrderBgr);

          // Free AGG resources
          SpanGradient.Free;
          Estr.Free;
          Txt.Free;
          Pt.Free;

          Circle.Free;
          Etrans.Free;

          GradientMatrix.Free;
          GradientAdaptor.Free;

          if Assigned(GfExt) then
            GfExt.Free;
          if Assigned(GfStd) then
            GfStd.Free;
        finally
          Rg.Free;
        end;
      finally
        SpanInterpolator.Free;
      end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Tar: TAggTransAffine;
begin
  if mkfMouseLeft in Flags then
  begin
    FMouse.X := X;
    FMouse.Y := Y;

    Tar := GetTransAffineResizing;

    Tar.InverseTransform(Tar, @FMouse.X, @FMouse.Y);

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Tar: TAggTransAffine;
begin
  if mkfMouseLeft in Flags then
  begin
    FMouse.X := X;
    FMouse.Y := Y;

    Tar := GetTransAffineResizing;

    Tar.InverseTransform(Tar, @FMouse.X, @FMouse.Y);

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This demo evolved from testing code and performance '
      + 'measurements. In particular, it shows you how to calculate the '
      + 'parameters of a radial Gradient with a separate focal point, '
      + 'considering arbitrary affine transformations. In this example window '
      + 'resizing transformations are taken into account. It also '
      + 'demonstrates the use case of TAggGradientLut and Gamma correction.');
end;

procedure TAggApplication.OnControlChange;
begin
  FGammaLut.Gamma := FSliderGamma.Value;

  BuildGradientLUT;

  FOldGamma := FSliderGamma.Value;
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. PDF linear and radial Gradients (F1-Help)';

    if Init(600, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
