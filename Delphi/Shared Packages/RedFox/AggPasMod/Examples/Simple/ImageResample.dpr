program ImageResample;

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

  AggBasics,

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

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
  AggTransPerspective in '..\..\Source\AggTransPerspective.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanInterpolatorTrans in '..\..\Source\AggSpanInterpolatorTrans.pas',
  AggSpanInterpolatorPerspective in '..\..\Source\AggSpanInterpolatorPerspective.pas',
  AggSpanSubdivAdaptor in '..\..\Source\AggSpanSubdivAdaptor.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgba in '..\..\Source\AggSpanImageFilterRgba.pas',
  AggSpanImageResample in '..\..\Source\AggSpanImageResample.pas',
  AggSpanImageResampleRgba in '..\..\Source\AggSpanImageResampleRgba.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggInteractivePolygon;

const
  CFlipY = True;

var
  GImgName: ShortString;

type
  TAggApplication = class(TPlatformSupport)
  private
    FBounds: TRectDouble;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine  : TAggScanLineUnpacked8;

    FGammaLut: TAggGammaLut8;
    FInteractiveQuad: TInteractivePolygon;
    FRadioBoxTransType: TAggControlRadioBox;
    FSliderGamma, FSliderBlur: TAggControlSlider;
    FOldGamma: Double;
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

  FOldGamma := 2;

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;

  FGammaLut := TAggGammaLut8.Create(2);
  FInteractiveQuad := TInteractivePolygon.Create(4, 5);

  FRadioBoxTransType := TAggControlRadioBox.Create(400, 5, 600, 100, not FlipY);
  FSliderGamma := TAggControlSlider.Create(5, 5, 395, 10, not FlipY);
  FSliderBlur := TAggControlSlider.Create(5, 20, 395, 25, not FlipY);

  FRadioBoxTransType.SetTextSize(7);
  FRadioBoxTransType.AddItem('Affine No Resample');
  FRadioBoxTransType.AddItem('Affine Resample');
  FRadioBoxTransType.AddItem('Perspective No Resample LERP');
  FRadioBoxTransType.AddItem('Perspective No Resample Exact');
  FRadioBoxTransType.AddItem('Perspective Resample LERP');
  FRadioBoxTransType.AddItem('Perspective Resample Exact');
  FRadioBoxTransType.SetCurrentItem(4);

  AddControl(FRadioBoxTransType);

  FSliderGamma.SetRange(0.5, 3);
  FSliderGamma.Value := 2;
  FSliderGamma.Caption := 'Gamma=%.3f';

  AddControl(FSliderGamma);

  FSliderBlur.SetRange(0.5, 2);
  FSliderBlur.Value := 1;
  FSliderBlur.Caption := 'Blur=%.3f';

  AddControl(FSliderBlur);
end;

destructor TAggApplication.Destroy;
begin
  FGammaLut.Free;
  FInteractiveQuad.Free;

  FRadioBoxTransType.Free;
  FSliderGamma.Free;
  FSliderBlur.Free;

  FRasterizer.Free;
  FScanLine.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
var
  Delta: TPointDouble;
  Pixf: TAggPixelFormatProcessor;
begin
  FBounds := RectDouble(0, 0, RenderingBufferImage[0].Width,
    RenderingBufferImage[0].Height);

  Delta.X := Width * 0.5 - (FBounds.X2 - FBounds.X1) * 0.5;
  Delta.Y := Height * 0.5 - (FBounds.Y2 - FBounds.Y1) * 0.5;

  FInteractiveQuad.Point[0] := PointDouble(Floor(FBounds.X1 + Delta.X),
    Floor(FBounds.Y1 + Delta.Y));
  FInteractiveQuad.Point[1] := PointDouble(Floor(FBounds.X2 + Delta.X),
    Floor(FBounds.Y1 + Delta.Y));
  FInteractiveQuad.Point[2] := PointDouble(Floor(FBounds.X2 + Delta.X),
    Floor(FBounds.Y2 + Delta.Y));
  FInteractiveQuad.Point[3] := PointDouble(Floor(FBounds.X1 + Delta.X),
    Floor(FBounds.Y2 + Delta.Y));

  PixelFormatBgra32(Pixf, RenderingBufferImage[0]);
  try
    Pixf.ApplyGammaDir(FGammaLut, CAggOrderBgra);
  finally
    Pixf.Free;
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf, PixfPre: TAggPixelFormatProcessor;

  RendererBase, RendererBasePre: TAggRendererBase;

  Rgba: TAggColor;

  RenScan: TAggRendererScanLineAASolid;
  B: Integer;

  SpanAllocator: TAggSpanAllocator;
  Tr: TAggTransAffine;
  Sg: TAggSpanImageFilter;
  Rsi: TAggRendererScanLineAA;

  Trp: TAggTransPerspective23;

  Interpolator: TAggSpanInterpolatorLinear;
  InterpSubdiv: TAggSpanInterpolatorLinearSubdiv;
  InterpTrans: TAggSpanInterpolatorTrans;
  InterpPerspectiveLerp: TAggSpanInterpolatorPerspectiveLerp;
  InterpExact: TAggSpanInterpolatorPerspectiveExact;
  SubdivAdaptor: TAggSpanSubdivAdaptor;

  FilterKernel: TAggImageFilterHanning;
  Filter: TAggImageFilterLUT;

  Tm : Double;
  Txt: TAggGsvText;
  Pt : TAggConvStroke;
  Pd : PPointDouble;
begin
  if FSliderGamma.Value <> FOldGamma then
  begin
    FGammaLut.Gamma := FSliderGamma.Value;

    LoadImage(0, GImgName);

    PixelFormatBgra32(Pixf, RenderingBufferImage[0]);
    try
      Pixf.ApplyGammaDir(FGammaLut, CAggOrderBgra);
    finally
      Pixf.Free;
    end;

    FOldGamma := FSliderGamma.Value;
  end;

  // Initialize structures
  PixelFormatBgra32(Pixf, RenderingBufferWindow);
  PixelFormatBgra32Pre(PixfPre, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  RendererBasePre := TAggRendererBase.Create(PixfPre, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      if FRadioBoxTransType.GetCurrentItem < 2 then
      begin
        // For the affine parallelogram transformations we
        // calculate the 4-th (implicit) point of the parallelogram
        FInteractiveQuad.Xn[3] := FInteractiveQuad.Xn[0] +
          (FInteractiveQuad.Xn[2] - FInteractiveQuad.Xn[1]);
        FInteractiveQuad.Yn[3] := FInteractiveQuad.Yn[0] +
          (FInteractiveQuad.Yn[2] - FInteractiveQuad.Yn[1]);
      end;

      // Render the "quad" tool and controls
      FRasterizer.AddPath(FInteractiveQuad);

      Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.1);
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
      FilterKernel := TAggImageFilterHanning.Create;
      Filter := TAggImageFilterLUT.Create(FilterKernel, True);

      Rgba.FromRgbaDouble(0, 0, 0, 0);

      StartTimer;

      case FRadioBoxTransType.GetCurrentItem of
        0:
          begin
            Tr := TAggTransAffine.Create(
              PAggParallelogram(FInteractiveQuad.Polygon),
              FBounds.X1, FBounds.Y1, FBounds.X2, FBounds.Y2);
            try
              Interpolator := TAggSpanInterpolatorLinear.Create(Tr);
              try
                Sg := TAggSpanImageFilterRgba2x2.Create(SpanAllocator,
                  RenderingBufferImage[0], @Rgba, Interpolator, Filter,
                  CAggOrderBgra);

                Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                try
                  RenderScanLines(FRasterizer, FScanLine, Rsi);
                finally
                  Rsi.Free;
                end;
              finally
                Interpolator.Free;
              end;
            finally
              Tr.Free;
            end;
          end;

        1:
          begin
            Tr := TAggTransAffine.Create(PAggParallelogram(FInteractiveQuad.Polygon),
              FBounds.X1, FBounds.Y1, FBounds.X2, FBounds.Y2);
            try
              Interpolator := TAggSpanInterpolatorLinear.Create(Tr);
              try
                Sg := TAggSpanImageResampleRgbaAffine.Create(SpanAllocator,
                  RenderingBufferImage[0], @Rgba, Interpolator, Filter, CAggOrderBgra);

                TAggCustomSpanImageResample(Sg).SetBlur(FSliderBlur.Value);

                Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                try
                  RenderScanLines(FRasterizer, FScanLine, Rsi);
                finally
                  Rsi.Free;
                end;
              finally
                Interpolator.Free;
              end;
            finally
              Tr.Free;
            end;
          end;

        2:
          begin
            Pd := FInteractiveQuad.Polygon;
            Trp := TAggTransPerspective23.Create(PQuadDouble(Pd), FBounds);
            try
              if Trp.IsValid then
              begin
                InterpSubdiv := TAggSpanInterpolatorLinearSubdiv.Create(Trp);
                try
                  Sg := TAggSpanImageFilterRgba2x2.Create(SpanAllocator,
                    RenderingBufferImage[0], @Rgba, Interpsubdiv, Filter,
                    CAggOrderBgra);
                  Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                  try
                    RenderScanLines(FRasterizer, FScanLine, Rsi);
                  finally
                    Rsi.Free;
                  end;
                finally
                  InterpSubdiv.Free;
                end;
              end;
            finally
              Trp.Free;
            end;
          end;

        3:
          begin
            Pd := FInteractiveQuad.Polygon;
            Trp := TAggTransPerspective23.Create(PQuadDouble(Pd), FBounds);
            try
              if Trp.IsValid then
              begin
                InterpTrans := TAggSpanInterpolatorTrans.Create(Trp);
                try
                  Sg := TAggSpanImageFilterRgba2x2.Create(SpanAllocator,
                    RenderingBufferImage[0], @Rgba, InterpTrans, Filter,
                    CAggOrderBgra);

                  Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                  try
                    RenderScanLines(FRasterizer, FScanLine, Rsi);
                  finally
                    Rsi.Free;
                  end;
                finally
                  InterpTrans.Free;
                end;
              end;
            finally
              Trp.Free;
            end;
          end;

        4:
          begin
            InterpPerspectiveLerp := TAggSpanInterpolatorPerspectiveLerp.Create(
              PQuadDouble(FInteractiveQuad.Polygon), FBounds);
            try
              SubdivAdaptor := TAggSpanSubdivAdaptor.Create(InterpPerspectiveLerp);
              try
                if InterpPerspectiveLerp.IsValid then
                begin
                  Sg := TAggSpanImageResampleRgba.Create(SpanAllocator,
                    RenderingBufferImage[0], @Rgba, SubdivAdaptor, Filter,
                    CAggOrderBgra);

                  TAggSpanImageResample(Sg).SetBlur(FSliderBlur.Value);

                  Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                  try
                    RenderScanLines(FRasterizer, FScanLine, Rsi);
                  finally
                    Rsi.Free;
                  end;
                end;
              finally
                SubdivAdaptor.Free;
              end;
            finally
              InterpPerspectiveLerp.Free;
            end;
          end;

        5:
          begin
            InterpExact := TAggSpanInterpolatorPerspectiveExact.Create(
              PQuadDouble(FInteractiveQuad.Polygon), FBounds);
            try
              SubdivAdaptor := TAggSpanSubdivAdaptor.Create(InterpExact);
              try
                if InterpExact.IsValid then
                begin
                  Sg := TAggSpanImageResampleRgba.Create(SpanAllocator,
                    RenderingBufferImage[0], @Rgba, SubdivAdaptor, Filter,
                    CAggOrderBgra);

                  TAggSpanImageResample(Sg).SetBlur(FSliderBlur.Value);

                  Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                  try
                    RenderScanLines(FRasterizer, FScanLine, Rsi);
                  finally
                    Rsi.Free;
                  end;
                end;
              finally
                SubdivAdaptor.Free;
              end;
            finally
              InterpExact.Free
            end;
          end;
      end;

      // Render Text
      Tm := GetElapsedTime;

      Pixf.ApplyGammaInv(FGammaLut, CAggOrderBgra);

      Txt := TAggGsvText.Create;
      Txt.SetSize(10);

      Pt := TAggConvStroke.Create(Txt);
      Pt.Width := 1.5;

      Txt.SetStartPoint(10, 70);
      Txt.SetText(Format('%3.2f ms', [Tm]));

      FRasterizer.AddPath(Pt);

      RenScan.SetColor(CRgba8Black);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Render the controls
      RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxTransType);
      RenderControl(FRasterizer, FScanLine, RenScan, FSliderGamma);
      RenderControl(FRasterizer, FScanLine, RenScan, FSliderBlur);

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
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FInteractiveQuad.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
var
  Center: TPointDouble;
  Tr : TAggTransAffine;
begin
  if Key = Byte(' ') then
  begin
    Center.X := (FInteractiveQuad.Xn[0] + FInteractiveQuad.Xn[1] +
      FInteractiveQuad.Xn[2] + FInteractiveQuad.Xn[3]) * 0.25;
    Center.Y := (FInteractiveQuad.Yn[0] + FInteractiveQuad.Yn[1] +
      FInteractiveQuad.Yn[2] + FInteractiveQuad.Yn[3]) * 0.25;

//    Tr.Create;
    Tr := TAggTransAffineTranslation.Create(-Center.x, -Center.Y);

    Tr.Rotate(Pi / 20 { 2 } );
    Tr.Translate(Center.X, Center.Y);

    Tr.Transform(Tr, FInteractiveQuad.XnPtr[0], FInteractiveQuad.YnPtr[0]);
    Tr.Transform(Tr, FInteractiveQuad.XnPtr[1], FInteractiveQuad.YnPtr[1]);
    Tr.Transform(Tr, FInteractiveQuad.XnPtr[2], FInteractiveQuad.YnPtr[2]);
    Tr.Transform(Tr, FInteractiveQuad.XnPtr[3], FInteractiveQuad.YnPtr[3]);

    ForceRedraw;
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('The demonstration of image transformations with '
      + 'resampling. You can see the difference in quality between regular '
      + 'image transformers and the ones with resampling. Of course, image '
      + 'tranformations with resampling work sLower because they provide the '
      + 'best possible quality.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to manipulate with image. Press the '
      + 'spacebar to rotate the image by 1/20 of pi.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Text: AnsiString;
  P, N, X: ShortString;

begin
  GImgName := 'spheres';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    GImgName := FoldName(P, N, '');
  end;
{$ENDIF }

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example. Image Transformations with Resampling (F1-Help)';

    if not LoadImage(0, GImgName) then
    begin
      Text := 'File not found: ' + GImgName + ImageExtension;
      if GImgName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + GImgName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
