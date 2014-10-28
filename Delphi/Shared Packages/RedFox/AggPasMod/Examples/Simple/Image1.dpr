program Image1;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$DEFINE AGG_BGR24}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgb in '..\..\Source\AggSpanImageFilterRgb.pas',
  AggSpanImageFilterRgba in '..\..\Source\AggSpanImageFilterRgba.pas',
  AggSpanImageFilterGray in '..\..\Source\AggSpanImageFilterGray.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas'
{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderAngle, FSliderScale: TAggControlSlider;
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

  FSliderAngle := TAggControlSlider.Create(5, 5, 300, 12, not FlipY);
  FSliderScale := TAggControlSlider.Create(5, 20, 300, 27, not FlipY);

  AddControl(FSliderAngle);
  AddControl(FSliderScale);

  FSliderAngle.Caption := 'Angle=%3.2f';
  FSliderScale.Caption := 'Scale=%3.2f';
  FSliderAngle.SetRange(-180, 180);
  FSliderAngle.Value := 0;
  FSliderScale.SetRange(0.1, 5);
  FSliderScale.Value := 1;
end;

destructor TAggApplication.Destroy;
begin
  FSliderAngle.Free;
  FSliderScale.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  PixFormat, PixFormatPre: TAggPixelFormatProcessor;

  Rgba: TAggColor;

  RendererBase, RendererBasePre: TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl: TAggScanLineUnpacked8;
  SpanAllocator: TAggSpanAllocator;
  Sg: TAggSpanImageFilter;
  Rsi: TAggRendererScanLineAA;
  ConvTransform: TAggConvTransform;
  Fi: TAggImageFilter;

  Filter: TAggCustomImageFilter;

  Interpolator: TAggSpanInterpolatorLinear;

  SrcMatrix, ImgMatrix: TAggTransAffine;

  Circle: TAggCircle;
  Radius: Double;
begin
  Filter := nil;

  // Initialize structures
  CPixelFormat(PixFormat, RenderingBufferWindow);
  CPixelFormatPre(PixFormatPre, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixFormat, True);
  RendererBasePre := TAggRendererBase.Create(PixFormatPre, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    Ras := TAggRasterizerScanLineAA.Create;
    Sl := TAggScanLineUnpacked8.Create;

    // Calc
    SrcMatrix := TAggTransAffine.Create;

    SrcMatrix.Translate(-FInitialWidth * 0.5 - 10, -FInitialHeight * 0.5 - 30);
    SrcMatrix.Rotate(Deg2Rad(FSliderAngle.Value));

    SrcMatrix.Scale(FSliderScale.Value);
    SrcMatrix.Translate(FInitialWidth * 0.5, FInitialHeight * 0.5 + 20);
    SrcMatrix.Multiply(GetTransAffineResizing);

    ImgMatrix := TAggTransAffine.Create;

    ImgMatrix.Translate(-FInitialWidth * 0.5 + 10, -FInitialHeight * 0.5 + 30);
    ImgMatrix.Rotate(Deg2Rad(FSliderAngle.Value));

    ImgMatrix.Scale(FSliderScale.Value);
    ImgMatrix.Translate(FInitialWidth * 0.5, FInitialHeight * 0.5 + 20);

    ImgMatrix.Multiply(GetTransAffineResizing);
    ImgMatrix.Invert;

    SpanAllocator := TAggSpanAllocator.Create;
    Interpolator := TAggSpanInterpolatorLinear.Create(ImgMatrix);

    Rgba.FromRgbaDouble(0, 0.2, 0, 0.5);

    // Version without filtering (nearest neighbor)
    {
    Sg := TAggSpanImageFilterRgbNN.Create(SpanAllocator,
      RenderingBufferImage[0], @Rgba, Interpolator, CComponentOrder);
    }

    // Version with "hardcoded" bilinear filter
    Sg := TAggSpanImageFilterRgbBilinear.Create(SpanAllocator,
      RenderingBufferImage[0], @Rgba, Interpolator, CComponentOrder);

    // Version with arbitrary filter
    {
    Filter := TAggImageFilterMitchell.Create;
    Fi := TAggImageFilter.Create(filter);
    Sg := TAggSpanImageFilterRgb.Create(SpanAllocator, RenderingBufferImage[0],
      @Rgba, Interpolator, Fi, CComponentOrder));
    }

    // Render
    Radius := FInitialWidth;

    if FInitialHeight - 60 < Radius then
      Radius := FInitialHeight - 60;

    Circle := TAggCircle.Create(FInitialWidth * 0.5 + 10,
      FInitialHeight * 0.5 + 30, Radius * 0.5 + 16, 200);

    ConvTransform := TAggConvTransform.Create(Circle, SrcMatrix);
    try
      Ras.AddPath(ConvTransform);
    finally
      ConvTransform.Free;
    end;

    Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
    try
      RenderScanLines(Ras, Sl, Rsi);
    finally
      Rsi.Free;
    end;

    // Render the controls
    RenderControl(Ras, Sl, RenScan, FSliderAngle);
    RenderControl(Ras, Sl, RenScan, FSliderScale);

    // Free AGG resources
    Ras.Free;
    RenScan.Free;
    Sl.Free;
    Circle.Free;

    SrcMatrix.Free;
    ImgMatrix.Free;
    SpanAllocator.Free;

    if Sg <> nil then
      Sg.Free;

    if Filter <> nil then
    begin
      Filter.Free;

      Fi.Free;
    end;
    Interpolator.Free;
  finally
    RendererBase.Free;
    RendererBasePre.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This is the first example with the new "reincarnation" of '
      + 'the image transformation algorithms. The example allows you to rotate '
      + 'and scale the image with respect to its center. Also, the image is '
      + 'scaled when resizing the window.'#13#13
      + 'How to play with:'#13#13
      + 'Try to recompile the source code with different image filtering '
      + 'methods.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Text: AnsiString;
  ImageName, P, N, X: ShortString;
begin
  ImageName := 'spheres';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF}

  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'Image Affine Transformations with filtering (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(RenderingBufferImage[0].Width + 20,
      RenderingBufferImage[0].Height + 60, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
