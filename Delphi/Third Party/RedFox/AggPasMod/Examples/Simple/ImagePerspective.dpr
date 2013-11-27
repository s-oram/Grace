program ImagePerspective;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggTransBilinear in '..\..\Source\AggTransBilinear.pas',
  AggTransPerspective in '..\..\Source\AggTransPerspective.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanInterpolatorTrans in '..\..\Source\AggSpanInterpolatorTrans.pas',
  AggSpanSubdivAdaptor in '..\..\Source\AggSpanSubdivAdaptor.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgba in '..\..\Source\AggSpanImageFilterRgba.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggInteractivePolygon;

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FInteractiveQuad: TInteractivePolygon;
    FRadioBoxTransType: TAggControlRadioBox;
  protected
    FBounds: TRectDouble;
    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;
    FPixelFormatProcessor: TAggPixelFormatProcessor;
    FPixelFormatProcessorPre: TAggPixelFormatProcessor;
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

  PixelFormatBgra32(FPixelFormatProcessor, RenderingBufferWindow);
  PixelFormatBgra32Pre(FPixelFormatProcessorPre, RenderingBufferWindow);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;

  FInteractiveQuad := TInteractivePolygon.Create(4, 5);
  FRadioBoxTransType := TAggControlRadioBox.Create(420, 5, 590, 70, not FlipY);

  FRadioBoxTransType.AddItem('Affine Parallelogram');
  FRadioBoxTransType.AddItem('Bilinear');
  FRadioBoxTransType.AddItem('Perspective');
  FRadioBoxTransType.SetCurrentItem(2);

  AddControl(FRadioBoxTransType);
end;

destructor TAggApplication.Destroy;
begin
  FInteractiveQuad.Free;
  FRadioBoxTransType.Free;

  FScanLine.Free;
  FRasterizer.Free;

  FPixelFormatProcessorPre.Free;
  FPixelFormatProcessor.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
var
  Bounds: TRectDouble;
  Delta: TPointDouble;
begin
  FBounds := RectDouble(0, 0, RenderingBufferImage[0].Width,
    RenderingBufferImage[0].Height);

  Bounds.X1 := FBounds.X1; // * 100;
  Bounds.Y1 := FBounds.Y1; // * 100;
  Bounds.X2 := FBounds.X2; // * 100;
  Bounds.Y2 := FBounds.Y2; // * 100;

  Delta := PointDouble(Width * 0.5 - Bounds.CenterX,
    Height * 0.5 - Bounds.CenterY);

  { FInteractiveQuad.xn[0] := Floor(x1 + Delta.X + 50);
    FInteractiveQuad.yn[0] := Floor(y1 + Delta.Y + 50);
    FInteractiveQuad.xn[1] := Floor(x2 + Delta.X);
    FInteractiveQuad.yn[1] := Floor(y1 + Delta.Y);
    FInteractiveQuad.xn[2] := Floor(x2 + Delta.X);
    FInteractiveQuad.yn[2] := Floor(y2 + Delta.Y);
    FInteractiveQuad.xn[3] := Floor(x1 + Delta.X);
    FInteractiveQuad.yn[3] := Floor(y2 + Delta.Y); { }

  FInteractiveQuad.Point[0] := PointDouble(100 + 50);
  FInteractiveQuad.Point[1] := PointDouble(Width - 100, 100);
  FInteractiveQuad.Point[2] := PointDouble(Width - 100, Height - 100);
  FInteractiveQuad.Point[3] := PointDouble(100, Height - 100);
end;

procedure TAggApplication.OnDraw;
var
  RendererBase, RendererBasePre: TAggRendererBase;

  Rgba: TAggColor;

  RenScan: TAggRendererScanLineAASolid;
  SpanAllocator: TAggSpanAllocator;
  Tr: TAggTransAffine;
  Sg: TAggSpanImageFilter;
  Rsi: TAggRendererScanLineAA;

  Trb: TAggTransBilinear;
  Trp: TAggTransPerspective;

  Interpolator: TAggSpanInterpolatorLinear;
  InterpolatorTrans: TAggSpanInterpolatorTrans;

  FilterKernel: TAggImageFilterHermite;
  Filter: TAggImageFilterLUT;

  Tm : Double;
  Txt: TAggGsvText;
  Pt : TAggConvStroke;
  Pd : PPointDouble;

begin
  RendererBase := TAggRendererBase.Create(FPixelFormatProcessor);
  RendererBasePre := TAggRendererBase.Create(FPixelFormatProcessorPre);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      if FRadioBoxTransType.GetCurrentItem = 0 then
      begin
        // For the affine parallelogram transformations we
        // calculate the 4-th (implicit) point of the parallelogram
        FInteractiveQuad.Xn[3] := FInteractiveQuad.Xn[0] +
          (FInteractiveQuad.Xn[2] - FInteractiveQuad.Xn[1]);
        FInteractiveQuad.Yn[3] := FInteractiveQuad.Yn[0] +
          (FInteractiveQuad.Yn[2] - FInteractiveQuad.Yn[1]);
      end;

      // Render the "quad" tool
      FRasterizer.AddPath(FInteractiveQuad);

      Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.6);
      RenScan.SetColor(@Rgba);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Prepare the polygon to rasterize. Here we need to fill
      // the destination (transformed) polygon.
      FRasterizer.SetClipBox(0, 0, Width, Height);
      FRasterizer.Reset;
      FRasterizer.MoveToDouble(FInteractiveQuad.Point[0]);
      FRasterizer.LineToDouble(FInteractiveQuad.Point[1]);
      FRasterizer.LineToDouble(FInteractiveQuad.Point[2]);
      FRasterizer.LineToDouble(FInteractiveQuad.Point[3]);

      SpanAllocator := TAggSpanAllocator.Create;
      FilterKernel := TAggImageFilterHermite.Create;
      Filter := TAggImageFilterLUT.Create(FilterKernel, False);

      Rgba.FromRgbaDouble(0, 0, 0, 0);

      StartTimer;

      case FRadioBoxTransType.GetCurrentItem of
        0:
          begin
            // Note that we consruct an affine matrix that transforms
            // a parallelogram to a rectangle, i.e., it's inverted.
            // It's actually the same as:
            // Tr := TAggTransAffine.Create(FBounds.X1, FBounds.Y1, FBounds.X2, FBounds.Y2, FInteractiveQuad.Polygon);
            // Tr.Invert;
            Tr := TAggTransAffine.Create(PAggParallelogram(FInteractiveQuad.Polygon), FBounds.X1, FBounds.Y1, FBounds.X2, FBounds.Y2);
            try
              // Also note that we can use the linear interpolator instead of
              // arbitrary TAggSpanInterpolatorTrans. It works much faster,
              // but the transformations must be linear and parellel.
              Interpolator := TAggSpanInterpolatorLinear.Create(Tr);

              Sg := TAggSpanImageFilterRgbaNN.Create(SpanAllocator,
                RenderingBufferImage[0], @Rgba, Interpolator, CAggOrderBgra);

              Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
              try
                RenderScanLines(FRasterizer, FScanLine, Rsi);
              finally
                Rsi.Free;
              end;
            finally
              Tr.Free;
            end;
            Interpolator.Free;
          end;

        1:
          begin
            Pd := FInteractiveQuad.Polygon;
            Trb := TAggTransBilinear.Create(PQuadDouble(Pd), FBounds);

            try
              if Trb.IsValid then
              begin
                Interpolator := TAggSpanInterpolatorLinear.Create(Trb);

                Sg := TAggSpanImageFilterRgba2x2.Create(SpanAllocator,
                  RenderingBufferImage[0], @Rgba, Interpolator, Filter,
                  CAggOrderBgra);

                Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                try
                  RenderScanLines(FRasterizer, FScanLine, Rsi);
                finally
                  Rsi.Free;
                end;
                Interpolator.Free;
              end;
            finally
              Trb.Free;
            end;
          end;

        2:
          begin
            Trp := TAggTransPerspective.Create(
              Pointer(FInteractiveQuad.Polygon), FBounds);
            try
              if Trp.IsValid then
              begin
                InterpolatorTrans := TAggSpanInterpolatorTrans.Create(Trp);

                Sg := TAggSpanImageFilterRgba2x2.Create(SpanAllocator,
                  RenderingBufferImage[0], @Rgba, InterpolatorTrans, Filter,
                  CAggOrderBgra);

                Rsi := TAggRendererScanLineAA.Create(RendererBase, Sg);
                try
                  RenderScanLines(FRasterizer, FScanLine, Rsi);
                finally
                  Rsi.Free;
                end;
                InterpolatorTrans.Free;
              end;
            finally
              Trp.Free;
            end;
          end;
      end;

      // Render Text
      Tm := GetElapsedTime;

      Txt := TAggGsvText.Create;
      Txt.SetSize(10);

      Pt := TAggConvStroke.Create(Txt);
      Pt.Width := 1.5;

      Txt.SetStartPoint(10, 10);
      Txt.SetText(Format('%3.2f ms', [Tm]));

      FRasterizer.AddPath(Pt);

      RenScan.SetColor(CRgba8Black);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Render the controls
      RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxTransType);

      // Free AGG resources
      SpanAllocator.Free;
      Filter.Free;

      if Assigned(Sg) then
        Sg.Free;

      Txt.Free;
      Pt.Free;
      FilterKernel.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
    RendererBasePre.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FInteractiveQuad.OnMouseMove(X, Y) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FInteractiveQuad.OnMouseButtonDown(X, Y) then
      ForceRedraw;

  if mkfMouseRight in Flags then
    DisplayMessage(Format('%d %d', [X, Y]));
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FInteractiveQuad.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Image perspective transformations. There are two types of '
      + 'arbitrary quadrangle transformations, Perspective and Bilinear. The '
      + 'image transformer always uses reverse transformations, and there is '
      + 'a problem. The Perspective transformations are perfectly reversible, '
      + 'so they work correctly with images, but the Bilinear transformer '
      + 'behave somehow strange. It can transform a rectangle to a quadrangle, '
      + 'but not vice versa. In this example you can see this effect, when the '
      + 'edges of the image "sag". I''d Highly appreciate if someone could '
      + 'help me with math for transformations similar to Bilinear ones, but '
      + 'correctly reversible (i.e., that can transform an arbitrary '
      + 'quadrangle to a rectangle). The bilinear transformations are simple, '
      + 'see AggTransBilinear.pas and AggSimulEq.pas'#13#13
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
{$ENDIF }

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example. Image Perspective Transformations (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another '
          + 'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(600, 600, [wfResize]) then
      Run;

  finally
    Free;
  end;
end.
