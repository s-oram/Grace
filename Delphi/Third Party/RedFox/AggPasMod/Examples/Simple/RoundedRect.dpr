program RoundedRect;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggRoundedRect in '..\..\Source\AggRoundedRect.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FScanLine: TAggScanLinePacked8;
    FRasterizer: TAggRasterizerScanLineAA;
    FCircle: TAggCircle;
    FRoundedRect: TRectDouble;
    FDelta: TPointDouble;
    FIndex: Integer;
    FSliderRadius, FSliderGamma, FSliderOffset: TAggControlSlider;
    FCheckBoxWhiteOnBlack: TAggControlCheckBox;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

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
var
  Rgba8: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;

  FCircle := TAggCircle.Create;

  FSliderRadius := TAggControlSlider.Create(10, 10, 590, 19, not FlipY);
  FSliderGamma := TAggControlSlider.Create(10, 30, 590, 39, not FlipY);
  FSliderOffset := TAggControlSlider.Create(10, 50, 590, 59, not FlipY);

  FCheckBoxWhiteOnBlack := TAggControlCheckBox.Create(10, 70, 'White on black');

  FIndex := -1;

  FRoundedRect.Point1 := PointDouble(100);
  FRoundedRect.Point2 := PointDouble(500, 350);

  AddControl(FSliderRadius);
  AddControl(FSliderGamma);
  AddControl(FSliderOffset);
  AddControl(FCheckBoxWhiteOnBlack);

  FSliderGamma.Caption := 'Gamma = %4.3f';
  FSliderGamma.SetRange(0.0, 3.0);
  FSliderGamma.Value := 1.8;

  FSliderRadius.Caption := 'Radius = %4.3f';
  FSliderRadius.SetRange(0.0, 50.0);
  FSliderRadius.Value := 25.0;

  FSliderOffset.Caption := 'Subpixel Offset = %4.3f';
  FSliderOffset.SetRange(-2.0, 3.0);

  Rgba8.FromRgbaInteger(127, 127, 127);
  FCheckBoxWhiteOnBlack.TextColor := Rgba8;
  FCheckBoxWhiteOnBlack.InactiveColor := Rgba8;
end;

destructor TAggApplication.Destroy;
begin
  FScanLine.Free;
  FRasterizer.Free;

  FCircle.Free;

  FSliderRadius.Free;
  FSliderGamma.Free;
  FSliderOffset.Free;

  FCheckBoxWhiteOnBlack.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  Ren: TAggRendererScanLineAASolid;

  Gamma: TAggGammaLut8;
  GammaNo: TAggGammaNone;

  RndRct: TAggRoundedRect;
  Stroke: TAggConvStroke;
  D: Double;
begin
  // Initialize structures
  Gamma := TAggGammaLut8.Create(FSliderGamma.Value);

  PixelFormatBgr24Gamma(Pixf, RenderingBufferWindow, Gamma);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Ren := TAggRendererScanLineAASolid.Create(RendererBase);

    if FCheckBoxWhiteOnBlack.Status then
      RendererBase.Clear(CRgba8Black)
    else
      RendererBase.Clear(CRgba8White);

    // Render two "control" circles
    Ren.SetColor(CRgba8Gray);

    FCircle.Initialize(FRoundedRect.Point1, 3, 16);
    FRasterizer.AddPath(FCircle);
    RenderScanLines(FRasterizer, FScanLine, Ren);

    FCircle.Initialize(FRoundedRect.Point2, 3, 16);
    FRasterizer.AddPath(FCircle);
    RenderScanLines(FRasterizer, FScanLine, Ren);

    // Creating a rounded rectangle
    D := FSliderOffset.Value;

    RndRct := TAggRoundedRect.Create(FRoundedRect.Point1.X + D,
      FRoundedRect.Point1.Y + D, FRoundedRect.Point2.X + D,
      FRoundedRect.Point2.Y + D, FSliderRadius.Value);
    RndRct.NormalizeRadius;

    // Drawing as an outline
    Stroke := TAggConvStroke.Create(RndRct);
    Stroke.Width := 1.0;

    FRasterizer.AddPath(Stroke);

    if FCheckBoxWhiteOnBlack.Status then
      Ren.SetColor(CRgba8White)
    else
      Ren.SetColor(CRgba8Black);

    RenderScanLines(FRasterizer, FScanLine, Ren);

    GammaNo := TAggGammaNone.Create;
    FRasterizer.Gamma(GammaNo);

    // Render the controls
    RenderControl(FRasterizer, FScanLine, Ren, FSliderRadius);
    RenderControl(FRasterizer, FScanLine, Ren, FSliderGamma);
    RenderControl(FRasterizer, FScanLine, Ren, FSliderOffset);
    RenderControl(FRasterizer, FScanLine, Ren, FCheckBoxWhiteOnBlack);

    // Free AGG resources
    Ren.Free;

    RndRct.Free;
    GammaNo.Free;
    Gamma.Free;
    Stroke.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FIndex >= 0 then
    begin
      FRoundedRect.Points[FIndex].X := X - FDelta.X;
      FRoundedRect.Points[FIndex].Y := Y - FDelta.Y;

      ForceRedraw;
    end
    else
  else
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  I: Cardinal;
begin
  if mkfMouseLeft in Flags then
    for I := 0 to 1 do
      if Hypot((X - FRoundedRect.Points[I].X),
        (Y - FRoundedRect.Points[I].Y)) < 5.0 then
      begin
        FDelta.X := X - FRoundedRect.Points[I].X;
        FDelta.Y := Y - FRoundedRect.Points[I].Y;
        FIndex := I;

        Break;
      end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FIndex := -1;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Yet another example dedicated to Gamma Correction. If you '
      + 'have a CRT monitor: The rectangle looks bad - the rounded corners are '
      + 'thicker than its side lines. First try to drag the "subpixel offset" '
      + 'control — it simply adds some fractional value to the coordinates. '
      + 'When dragging you will see that the rectangle is "blinking". Then '
      + 'increase "Gamma" to about 1.5. The result will look almost perfect — '
      + 'the visual thickness of the rectangle remains the same. That''s good, '
      + 'but turn the checkbox "White on black" on — what do we see ? Our '
      + 'rounded rectangle looks terrible. Drag the "subpixel offset" slider — '
      + 'it''s blinking as hell. Now decrease "Gamma" to about 0.6. What do we '
      + 'see now? Perfect result! If you use an LCD monitor, the good value '
      + 'of Gamma will be closer to 1.0 in both cases — black on white or '
      + 'white on black. There''s no perfection in this world, but at least '
      + 'you can control Gamma in Anti-Grain Geometry :-)'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Rounded rectangle with gamma-correction &' +
      ' stuff (F1-Help)';

    if Init(600, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
