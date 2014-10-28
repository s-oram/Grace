program BSpline;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{ DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{ DEFINE AGG_Rgb24 }
{ DEFINE AGG_BGRA32 }
{ DEFINE AGG_RgbA32 }
{ DEFINE AGG_ARGB32 }
{ DEFINE AGG_ABGR32 }
{ DEFINE AGG_Rgb565 }
{ DEFINE AGG_Rgb555 }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggConvBSpline in '..\..\Source\AggConvBSpline.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggInteractivePolygon in 'AggInteractivePolygon.pas'

{$I Pixel_Formats.inc }

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FPoly: TInteractivePolygon;
    FScanLine: TAggScanLinePacked8;
    FRasterizer: TAggRasterizerScanLineAA;

    FSliderNumPoints: TAggControlSlider;

    FCheckBoxClose: TAggControlCheckBox;
    FClip: Integer;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
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
begin
  inherited Create(PixelFormat, FlipY);

  FScanLine := TAggScanLinePacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;
  FPoly := TInteractivePolygon.Create(6, 5.0);

  FSliderNumPoints := TAggControlSlider.Create(5, 5, 340, 12, not FlipY);
  FCheckBoxClose := TAggControlCheckBox.Create(350, 5, 'Close', not FlipY);

  FClip := 0;

  AddControl(FCheckBoxClose);

  FSliderNumPoints.SetRange(1.0, 40.0);
  FSliderNumPoints.Value := 20.0;
  FSliderNumPoints.Caption := 'Number of intermediate Points = %.3f';

  AddControl(FSliderNumPoints);
end;

destructor TAggApplication.Destroy;
begin
  FSliderNumPoints.Free;
  FCheckBoxClose.Free;

  FPoly.Free;
  FRasterizer.Free;
  FScanLine.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  if FClip <> 0 then
  begin
    FPoly.Point[0] := PointDouble(100, Height - 100);
    FPoly.Point[1] := PointDouble(Width - 100, Height - 100);
    FPoly.Point[2] := PointDouble(Width - 100, 100);
    FPoly.Point[3] := PointDouble(100);
  end
  else
  begin
    FPoly.Point[0] := PointDouble(100);
    FPoly.Point[1] := PointDouble(Width - 100, 100);
    FPoly.Point[2] := PointDouble(Width - 100, Height - 100);
    FPoly.Point[3] := PointDouble(100, Height - 100);
  end;

  FPoly.Point[4] := PointDouble(0.5 * Width, 0.5 * Height);
  FPoly.Point[5] := PointDouble(0.5 * Width, Height / 3);
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Rgba: TAggColor;
  Path: TSimplePolygonVertexSource;

  BSpline: TAggConvBSpline;
  Stroke : TAggConvStroke;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Draw
      Path := TSimplePolygonVertexSource.Create(FPoly.Polygon, FPoly.NumPoints,
        False, FCheckBoxClose.Status);

      BSpline := TAggConvBSpline.Create(Path);
      BSpline.InterpolationStep := 1 / FSliderNumPoints.Value;

      Stroke := TAggConvStroke.Create(BSpline);
      Stroke.Width := 2;

      RenScan.SetColor(CRgba8Black);

      FRasterizer.AddPath(Stroke);

      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Render the "poly" tool
      Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.6);
      RenScan.SetColor(@Rgba);

      FRasterizer.AddPath(FPoly);

      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Render the controls
      RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxClose);
      RenderControl(FRasterizer, FScanLine, RenScan, FSliderNumPoints);

      // Free AGG resources
      Stroke.Free;
      BSpline.Free;
      Path.Free;
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
  if mkfMouseLeft in Flags then
    if FPoly.OnMouseMove(X, Y) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FPoly.OnMouseButtonDown(X, Y) then
      ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FPoly.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(' ') then
  begin
    FClip := FClip xor 1;

    OnInit;
    ForceRedraw;
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('Demostration of a very simple class of Bi-cubic Spline '
      + 'interpolation. The class supports extrapolation which is a simple '
      + 'linear function.'#13#13
      + 'How to play with:'#13#13
      + 'Use the mouse to change curve''s shape.'#13
      + 'Press the spacebar to flip the curve. '#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. BSpline Interpolator (F1-Help)';

    if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
