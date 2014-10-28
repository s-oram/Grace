program LionOutline;

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
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggParseLion

{$I Pixel_Formats.inc}
const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;

    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FAngle, FScale: Double;
    FBaseDelta, FSkew: TPointDouble;
    FSliderWidth: TAggControlSlider;
    FCheckBoxScanLineRasterizer: TAggControlCheckBox;
  protected
    procedure Transform(AWidth, AHeight, X, Y: Double);
    procedure ParseLion;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

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

  // Rendering
  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;
  FPath := TAggPathStorage.Create;

  FPathCount := 0;

  FBoundingRect := RectDouble(0, 0, 0, 0);

  FBaseDelta := PointDouble(0);

  FAngle := 0;
  FScale := 1;

  FSkew := PointDouble(0);

  ParseLion;

  FSliderWidth := TAggControlSlider.Create(5, 5, 150, 12, not FlipY);
  AddControl(FSliderWidth);
  FSliderWidth.NoTransform;
  FSliderWidth.SetRange(0.0, 4.0);
  FSliderWidth.Value := 1.0;
  FSliderWidth.Caption := 'Width %3.2f';

  FCheckBoxScanLineRasterizer := TAggControlCheckBox.Create(160, 5,
    'Use ScanLine Rasterizer', not FlipY);
  AddControl(FCheckBoxScanLineRasterizer);
  FCheckBoxScanLineRasterizer.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FRasterizer.Free;
  FScanLine.Free;
  FPath.Free;

  FSliderWidth.Free;
  FCheckBoxScanLineRasterizer.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  AWidth, AHeight: Integer;

  W: Double;

  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Rgba: TAggColor;

  Mtx: TAggTransAffine;

  Stroke: TAggConvStroke;
  Trans : TAggConvTransform;

  GammaNone: TAggGammaNone;
  Profile: TAggLineProfileAA;

  Ren: TAggRendererOutlineAA;
  Ras: TAggRasterizerOutlineAA;
begin
  AWidth := RenderingBufferWindow.Width;
  AHeight := RenderingBufferWindow.Height;

  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Transform lion
      Mtx := TAggTransAffine.Create;
      try
        Mtx.Translate(-FBaseDelta.X, -FBaseDelta.Y);
        Mtx.Scale(FScale, FScale);
        Mtx.Rotate(FAngle + Pi);
        Mtx.Skew(FSkew.X * 1E-3, FSkew.Y * 1E-3);
        Mtx.Translate(AWidth * 0.5, AHeight * 0.5);

        // Render lion
        if FCheckBoxScanLineRasterizer.Status then
        begin
          Stroke := TAggConvStroke.Create(FPath);
          try
            Stroke.Width := FSliderWidth.Value;
            Trans := TAggConvTransform.Create(Stroke, Mtx);
            try
              RenderAllPaths(FRasterizer, FScanLine, RenScan, Trans, @FColors,
                @FPathIndex, FPathCount);
            finally
              Trans.Free;
            end;
          finally
            Stroke.Free;
          end;
        end
        else
        begin
          W := FSliderWidth.Value * Mtx.GetScale;

          GammaNone := TAggGammaNone.Create;
          try
            Profile := TAggLineProfileAA.Create(W, GammaNone);
            try
              Ren := TAggRendererOutlineAA.Create(RendererBase, Profile);
              try
                Ras := TAggRasterizerOutlineAA.Create(Ren);
                try
                  Trans := TAggConvTransform.Create(FPath, Mtx);
                  try
                    Ras.RenderAllPaths(Trans, @FColors, @FPathIndex, FPathCount);
                  finally
                    Trans.Free;
                  end;
                finally
                  Ras.Free;
                end;
              finally
                Ren.Free;
              end;
            finally
              Profile.Free;
            end;
          finally
            GammaNone.Free;
          end;
        end;
      finally
        Mtx.Free;
      end;

      // Render the control
      RenderControl(FRasterizer, FScanLine, RenScan, FSliderWidth);
      RenderControl(FRasterizer, FScanLine, RenScan,
        FCheckBoxScanLineRasterizer);
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.Transform(AWidth, AHeight, X, Y: Double);
begin
  X := X - (AWidth * 0.5);
  Y := Y - (AHeight * 0.5);

  FAngle := ArcTan2(Y, X);
  FScale := Sqrt(Y * Y + X * X) / 100.0;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta.X := FBoundingRect.CenterX;
  FBaseDelta.Y := FBoundingRect.CenterY;
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

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('The example demonstrates my new algorithm of drawing '
      + 'Anti-Aliased lines. The algorithm works about 2.5 times faster than '
      + 'the ScanLine Rasterizer but has some restrictions, particularly, '
      + 'line joins can be only of the "miter" type, and when so called miter '
      + 'limit is exceded, they are not as accurate as generated by the stroke '
      + 'converter (TAggConvStroke).'#13#13
      + 'How to play with:'#13#13
      + 'To see the difference, maximize the window and try to rotate and '
      + 'scale the "lion" with and without using the ScanLine Rasterizer '
      + '(a checkbox at the bottom). The difference in performance is '
      + 'obvious.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Lion (F1-Help)';

    if Init(512, 512, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
