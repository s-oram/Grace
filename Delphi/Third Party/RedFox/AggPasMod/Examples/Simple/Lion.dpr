program Lion;

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
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggParseLion
{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderAlpha: TAggControlSlider;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine  : TAggScanLinePacked8;

    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;
    FBoundingRect: TRectDouble;
    FAngle, FScale: Double;
    FSkew, FBaseDelta: TPointDouble;

    FNumClick: Integer;
  protected
    procedure ParseLion;
    procedure Transform(AWidth, AHeight, X, Y: Double);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnResize(Width, Height: Integer); override;
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

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;
  FPath := TAggPathStorage.Create;

  FPathCount := 0;

  FBoundingRect := RectDouble(0, 0 , 0, 0);

  FBaseDelta := PointDouble(0);
  FSkew := PointDouble(0);

  FAngle := 0;
  FScale := 1.0;

  FNumClick := 0;

  FSliderAlpha := TAggControlSlider.Create(5, 5, 512 - 5, 12, not FlipY);
  AddControl(FSliderAlpha);
  FSliderAlpha.NoTransform;
  FSliderAlpha.Caption := 'Alpha%3.3f';
  FSliderAlpha.Value := 0.1;

  ParseLion;
end;

destructor TAggApplication.Destroy;
begin
  FRasterizer.Free;
  FScanLine.Free;
  FPath.Free;

  FSliderAlpha.Free;

  inherited;
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta := PointDouble(FBoundingRect.CenterX, FBoundingRect.CenterY);
end;

procedure TAggApplication.OnDraw;
var
  I: Cardinal;

  AWidth, AHeight: Integer;

  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Mtx: TAggTransAffine;

  Trans: TAggConvTransform;
begin
  AWidth := RenderingBufferWindow.Width;
  AHeight := RenderingBufferWindow.Height;

  for I := 0 to FPathCount - 1 do
    PAggColor(PtrComp(@FColors[0]) + I * SizeOf(TAggColor)).Rgba8.A :=
      Int8u(Trunc(FSliderAlpha.Value * 255));

  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
  try
    // Transform lion
    Mtx := TAggTransAffine.Create;

    Mtx.Translate(-FBaseDelta.X, -FBaseDelta.Y);
    Mtx.Scale(FScale, FScale);
    Mtx.Rotate(FAngle + Pi);
    Mtx.Skew(FSkew.X * 1E-3, FSkew.Y * 1E-3);
    Mtx.Translate(AWidth * 0.5, AHeight * 0.5);

    // This code renders the lion
    Trans := TAggConvTransform.Create(FPath, Mtx);
    try
      RenderAllPaths(FRasterizer, FScanLine, RenScan, Trans, @FColors,
        @FPathIndex, FPathCount);
    finally
      Trans.Free;
    end;

    // Render the control
    RenderControl(FRasterizer, FScanLine, RenScan, FSliderAlpha);

    Mtx.Free;
  finally
    RenScan.Free;
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
var
  Pixf: TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
begin
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RendererBase.Clear(CRgba8White);
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.Transform(AWidth, AHeight, X, Y: Double);
begin
  X := X - (AWidth * 0.5);
  Y := Y - (AHeight * 0.5);

  FAngle := ArcTan2(Y, X);
  FScale := Hypot(X, Y) * 1E-2;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  AWidth, AHeight: Integer;
begin
  if mkfMouseLeft in Flags then
  begin
    AWidth := RenderingBufferWindow.Width;
    AHeight := RenderingBufferWindow.Height;

    Transform(AWidth, AHeight, X, Y);
    ForceRedraw;
  end;

  if mkfMouseRight in Flags then
  begin
    FSkew.X := X;
    FSkew.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This is the first example I used to implement and debug '
      + 'the ScanLine Rasterizer, affine transformer, and basic Renderers. '
      + 'The image is drawn over the old one with a cetrain opacity '
      + 'value.'#13#13
      + 'How to play with:'#13#13
      + 'You can rotate and scale the "Lion" with the left mouse button. '
      + 'Right mouse button adds "skewing" transformations, proportional to '
      + 'the "X" coordinate. Change "Alpha" to draw funny looking "lions". '
      + 'Change window size to clear the window.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Lion (F1-Help)';

    if Init(512, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
