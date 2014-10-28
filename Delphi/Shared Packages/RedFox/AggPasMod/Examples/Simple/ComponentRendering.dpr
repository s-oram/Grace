program ComponentRendering;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggSliderControl in '..\..\Source\AggSliderControl.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatGray in '..\..\Source\AggPixelFormatGray.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggControl in '..\..\Source\AggControl.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderAlpha: TAggControlSlider;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;

    FPixelFormat: TAggPixelFormatProcessor;
    FPixelFormatRed, FPixelFormatGreen, FPixelFormatBlue: TAggPixelFormatProcessor;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSliderAlpha := TAggControlSlider.Create(5, 5, 320 - 5, 10 + 5, not CFlipY);
  FSliderAlpha.Caption := 'Alpha=%1.0f';
  FSliderAlpha.SetRange(0, 255);
  FSliderAlpha.Value := 255;
  AddControl(FSliderAlpha);

  FScanLine := TAggScanLinePacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  PixelFormatBgr24(FPixelFormat, RenderingBufferWindow);
  PixelFormatGray8Bgr24r(FPixelFormatRed, RenderingBufferWindow);
  PixelFormatGray8Bgr24g(FPixelFormatGreen, RenderingBufferWindow);
  PixelFormatGray8Bgr24b(FPixelFormatBlue, RenderingBufferWindow);
end;

destructor TAggApplication.Destroy;
begin
  FSliderAlpha.Free;

  FRasterizer.Free;
  FScanLine.Free;

  FPixelFormat.Free;
  FPixelFormatRed.Free;
  FPixelFormatGreen.Free;
  FPixelFormatBlue.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  RendBase, RendBaseRed, RendBaseGreen, RendBaseBlue: TAggRendererBase;

  RenScan, Rr, Rg, Rb: TAggRendererScanLineAASolid;

  Rgba, Gray: TAggColor;

  Circle: TAggCircle;
begin
  RendBase := TAggRendererBase.Create(FPixelFormat);
  RendBaseRed := TAggRendererBase.Create(FPixelFormatRed);
  RendBaseGreen := TAggRendererBase.Create(FPixelFormatGreen);
  RendBaseBlue := TAggRendererBase.Create(FPixelFormatBlue);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendBase);
    Rr := TAggRendererScanLineAASolid.Create(RendBaseRed);
    Rg := TAggRendererScanLineAASolid.Create(RendBaseGreen);
    Rb := TAggRendererScanLineAASolid.Create(RendBaseBlue);

    // Setup colors & background
    Rgba.White;
    Gray.FromValueInteger(0, Trunc(FSliderAlpha.Value));

    RendBase.Clear(@Rgba);

    // Draw Circle
    Circle := TAggCircle.Create(0.5 * Width - 43.5, 0.5 * Height - 25, 100,
      100);
    try
      Rr.SetColor(@Gray);
      FRasterizer.AddPath(Circle);
    finally
      Circle.Free;
    end;
    RenderScanLines(FRasterizer, FScanLine, Rr);

    Circle := TAggCircle.Create(0.5 * Width + 43.5, 0.5 * Height - 25, 100,
      100);
    try
      Rg.SetColor(@Gray);
      FRasterizer.AddPath(Circle);
    finally
      Circle.Free;
    end;
    RenderScanLines(FRasterizer, FScanLine, Rg);

    Circle := TAggCircle.Create(0.5 * Width, 0.5 * Height + 50, 100, 100);
    try
      Rb.SetColor(@Gray);
      FRasterizer.AddPath(Circle);
    finally
      Circle.Free;
    end;
    RenderScanLines(FRasterizer, FScanLine, Rb);

    // Render control
    RenderControl(FRasterizer, FScanLine, RenScan, FSliderAlpha);

    // Free AGG resources
    RenScan.Free;
    Rr.Free;
    Rg.Free;
    Rb.Free;
  finally
    RendBase.Free;
    RendBaseRed.Free;
    RendBaseGreen.Free;
    RendBaseBlue.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('AGG has a gray-scale Renderer that can use any 8-bit color '
      + 'channel of an RGB or RGBA frame buffer. Most likely it will be used '
      + 'to draw gray-scale images directly in the alpha-channel.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Component Rendering (F1-Help)';

    if Init(320, 320, []) then
      Run;
  finally
    Free;
  end;
end.
