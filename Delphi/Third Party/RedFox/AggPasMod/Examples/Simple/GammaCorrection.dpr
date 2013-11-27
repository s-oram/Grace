program GammaCorrection;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{$DEFINE AGG_BGR24 }
{ DEFINE AGG_Rgb24 }
{ DEFINE AGG_Rgb565 }
{ DEFINE AGG_Rgb555 }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\AggControl.pas',
  AggSliderControl in '..\..\Source\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas'
{$I Pixel_Formats.inc }

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderThickness, FSliderGamma, FSliderContrast: TAggControlSlider;
    FRadius: TPointDouble;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSliderThickness := TAggControlSlider.Create(5, 5, 395, 11, not FlipY);
  FSliderGamma := TAggControlSlider.Create(5, 20, 395, 26, not FlipY);
  FSliderContrast := TAggControlSlider.Create(5, 35, 395, 41, not FlipY);

  AddControl(FSliderThickness);
  AddControl(FSliderGamma);
  AddControl(FSliderContrast);

  FSliderThickness.Caption := 'Thickness=%3.2f';
  FSliderGamma.Caption := 'Gamma=%3.2f';
  FSliderContrast.Caption := 'Contrast';

  FSliderThickness.SetRange(0, 3);
  FSliderGamma.SetRange(0.5, 3);
  FSliderContrast.SetRange(0, 1);

  FSliderThickness.Value := 1;
  FSliderGamma.Value := 1;
  FSliderContrast.Value := 1;
end;

destructor TAggApplication.Destroy;
begin
  FSliderThickness.Free;
  FSliderGamma.Free;
  FSliderContrast.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  FRadius := PointDouble(Width / 3, Height / 3);
end;

procedure TAggApplication.OnDraw;
var
  G, Dark, Light, X, Y, V, Gval, Dy: Double;

  I: Cardinal;

  Gamma: TAggGammaLut8;
  Pixf : TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Ras: TAggRasterizerScanLineAA;
  Sl : TAggScanLineUnpacked8;

  Rgba: TAggColor;
  Path: TAggPathStorage;

  GamPow: TAggGammaPower;
  Gpoly, Poly: TAggConvStroke;
  Ellipse: TAggEllipse;
begin
  // Initialize structures
  G := FSliderGamma.Value;

  Gamma := TAggGammaLut8.Create(G);

  CPixelFormatGamma(Pixf, RenderingBufferWindow, Gamma);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Dark := 1 - FSliderContrast.Value;
      Light := FSliderContrast.Value;

      Rgba.FromRgbaDouble(Dark, Dark, Dark);
      RendererBase.CopyBar(0, 0, Trunc(Width) div 2, Trunc(Height), @Rgba);

      Rgba.FromRgbaDouble(Light, Light, Light);
      RendererBase.CopyBar(Trunc(Width) div 2 + 1, 0, Trunc(Width),
        Trunc(Height), @Rgba);

      Rgba.FromRgbaDouble(1, Dark, Dark);
      RendererBase.CopyBar(0, Trunc(Height) div 2 + 1, Trunc(Width),
        Trunc(Height), @Rgba);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLineUnpacked8.Create;

      // Graph line
      Path := TAggPathStorage.Create;

      X := (Width - 256) * 0.5;
      Y := 50;

      for I := 0 to 255 do
      begin
        V := I / 255;

        GamPow := TAggGammaPower.Create(G);
        try
          Gval := GamPow.FuncOperatorGamma(V);
        finally
          GamPow.Free;
        end;

        Dy := Gval * 255;

        if I = 0 then
          Path.MoveTo(X + I, Y + Dy)
        else
          Path.LineTo(X + I, Y + Dy);
      end;

      Gpoly := TAggConvStroke.Create(Path);
      Gpoly.Width := 2;

      Ras.Reset;
      Ras.AddPath(Gpoly);

      Rgba.FromRgbaInteger(80, 127, 80);
      RenScan.SetColor(@Rgba);

      RenderScanLines(Ras, Sl, RenScan);

      // Ellipse
      Ellipse := TAggEllipse.Create(PointDouble(Width * 0.5, Height * 0.5),
        FRadius, 150);
      Poly := TAggConvStroke.Create(Ellipse);
      Poly.Width := FSliderThickness.Value;

      Ras.Reset;
      Ras.AddPath(Poly);

      Rgba.FromRgbaInteger(255, 0, 0);
      RenScan.SetColor(@Rgba);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(Width * 0.5, Height * 0.5),
        PointDouble(FRadius.X - 5, FRadius.Y - 5), 150);

      Ras.Reset;
      Ras.AddPath(Poly);

      Rgba.FromRgbaInteger(0, 255, 0);
      RenScan.SetColor(@Rgba);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(Width * 0.5, Height * 0.5),
        PointDouble(FRadius.X - 10, FRadius.Y - 10), 150);

      Ras.Reset;
      Ras.AddPath(Poly);

      Rgba.FromRgbaInteger(0, 0, 255);
      RenScan.SetColor(@Rgba);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(Width * 0.5, Height * 0.5),
        PointDouble(FRadius.X - 15, FRadius.Y - 15), 150);

      Ras.Reset;
      Ras.AddPath(Poly);

      Rgba.Black;
      RenScan.SetColor(@Rgba);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(Width * 0.5, Height * 0.5),
        PointDouble(FRadius.X - 20, FRadius.Y - 20), 150);

      Ras.Reset;
      Ras.AddPath(Poly);

      RenScan.SetColor(CRgba8White);

      RenderScanLines(Ras, Sl, RenScan);

      // Render the controls
      RenderControl(Ras, Sl, RenScan, FSliderThickness);
      RenderControl(Ras, Sl, RenScan, FSliderGamma);
      RenderControl(Ras, Sl, RenScan, FSliderContrast);

      // Free AGG resources
      Ras.Free;
      Sl.Free;
      Path.Free;

      Ellipse.Free;

      Gamma.Free;
      Gpoly.Free;
      Poly.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FRadius := PointDouble(Abs(Width * 0.5 - X), Abs(Height * 0.5 - Y));

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Anti-Aliasing is very tricky because everything depends. '
      + 'Particularly, having straight linear dependence "pixel coverage" -> '
      + '"brightness" may be not the best. It depends on the type of display '
      + '(CRT, LCD), contrast, black-on-white vs white-on-black, it even '
      + 'depends on your personal vision. There are no linear dependencies in '
      + 'this World. This example demonstrates the importance of so called '
      + 'Gamma Correction in Anti-Aliasing. There a traditional power function '
      + 'is used, in terms of C++ it''s brighness = pow(brighness, Gamma). '
      + 'Note, that if you improve the quality on the white side, it becomes '
      + 'worse on the black side and vice versa.'#13#13
      + 'How to play with:'#13#13
      + 'Change "Gamma" and see how the quality changes.'#13
      + 'Use the left mouse button to resize the circles.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Thin red TEllipse (F1-Help)';

    if Init(400, 320, []) then
      Run;
  finally
    Free;
  end;
end.
