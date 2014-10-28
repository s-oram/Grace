program LionLens;

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

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggTransWarpMagnifier in '..\..\Source\AggTransWarpMagnifier.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvSegmentator in '..\..\Source\AggConvSegmentator.pas',
  AggParseLion

{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderMagnitude, FSliderRadius: TAggControlSlider;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;

    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FAngle: Double;
    FBaseDelta, FSkew: TPointDouble;
  protected
    procedure ParseLion;
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

  // Rendering
  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;
  FPath := TAggPathStorage.Create;

  FPathCount := 0;

  FBoundingRect.X1 := 0;
  FBoundingRect.Y1 := 0;
  FBoundingRect.X2 := 0;
  FBoundingRect.Y2 := 0;

  FBaseDelta.X := 0;
  FBaseDelta.Y := 0;

  FAngle := 0;

  ParseLion;


  FSliderMagnitude := TAggControlSlider.Create(5, 5, 495, 12, not FlipY);
  AddControl(FSliderMagnitude);
  FSliderMagnitude.NoTransform;
  FSliderMagnitude.SetRange(0.01, 4.0);
  FSliderMagnitude.Value := 3.0;
  FSliderMagnitude.Caption := 'Scale=%3.2f';

  FSliderRadius := TAggControlSlider.Create(5, 20, 495, 27, not FlipY);
  AddControl(FSliderRadius);
  FSliderRadius.NoTransform;
  FSliderRadius.SetRange(0.0, 100.0);
  FSliderRadius.Value := 70.0;
  FSliderRadius.Caption := 'Radius=%3.2f';
end;

destructor TAggApplication.Destroy;
begin
  FRasterizer.Free;
  FScanLine.Free;
  FPath.Free;

  FSliderMagnitude.Free;
  FSliderRadius.Free;

  inherited;
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta.X := FBoundingRect.CenterX;
  FBaseDelta.Y := FBoundingRect.CenterY;
end;

procedure TAggApplication.OnInit;
begin
  FBoundingRect.X1 := 200;
  FBoundingRect.Y1 := 150;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Lens: TAggTransWarpMagnifier;
  Segm: TAggConvSegmentator;

  Mtx: TAggTransAffine;

  TransMatrix, TransLens: TAggConvTransform;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Transform lion
      Lens := TAggTransWarpMagnifier.Create;
      try
        Lens.SetCenter(FBoundingRect.X1, FBoundingRect.Y1);
        Lens.Magnification := FSliderMagnitude.Value;
        Lens.Radius := FSliderRadius.Value / FSliderMagnitude.Value;

        Segm := TAggConvSegmentator.Create(FPath);
        try
          Mtx := TAggTransAffine.Create;
          try
            Mtx.Translate(-FBaseDelta.X, -FBaseDelta.Y);
            Mtx.Rotate(FAngle + Pi);
            Mtx.Translate(Width * 0.5, Height * 0.5);

            TransMatrix := TAggConvTransform.Create(Segm, Mtx);
            try
              TransLens := TAggConvTransform.Create(TransMatrix, Lens);
              try
                RenderAllPaths(FRasterizer, FScanLine, RenScan, TransLens,
                  @FColors, @FPathIndex, FPathCount);
              finally
                TransLens.Free;
              end;
            finally
              TransMatrix.Free;
            end;
          finally
            Mtx.Free;
          end;

          // Render the controls
          RenderControl(FRasterizer, FScanLine, RenScan, FSliderMagnitude);
          RenderControl(FRasterizer, FScanLine, RenScan, FSliderRadius);
        finally
          Lens.Free;
        end;
      finally
        Segm.Free;
      end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FBoundingRect.X1 := X;
    FBoundingRect.Y1 := Y;

    ForceRedraw;
  end;

  if mkfMouseRight in Flags then
  begin
    FBoundingRect.X2 := X;
    FBoundingRect.Y2 := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This example exhibits a non-linear transformer that '
      + '"magnifies" vertices that fall inside a circle and extends the rest '
      + '(trans_warp_magnifier). Non-linear transformations are tricky because '
      + 'straight lines become curves. To achieve the correct result we need '
      + 'to divide long line segments into short ones. The example also '
      + 'demonstrates the use of conv_segmentator that does this division job. '
      + 'The transformer can also shrink away the image if the scaling value '
      + 'is less than 1.'#13#13
      + 'How to play with:'#13#13
      + 'Drag the center of the "lens" with the left mouse button and change '
      + 'the "Scale" and "Radius". To watch for an amazing effect, set the '
      + 'scale to the minimum (0.01), decrease the radius to about 1 and drag '
      + 'the "lens". You will see it behaves like a black hole consuming '
      + 'space around it. Move the lens somewhere to the side of the window '
      + 'and change the radius. It looks like changing the event horizon of '
      + 'the "black hole".'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Lion (F1-Help)';

    if Init(500, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
