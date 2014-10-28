program ImageFilters;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgb in '..\..\Source\AggSpanImageFilterRgb.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas';

const
  CFlipY = True;

  CPixelFormat: DefinePixelFormat = PixelFormatBgr24;
  CPixelFormatPre: DefinePixelFormat = PixelFormatBgr24Pre;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderRadius, FSliderStep: TAggControlSlider;
    FRadioBoxFilters: TAggControlRadioBox;
    FCheckBoxNormalize, FCheckBoxRun: TAggControlCheckBox;
    FCheckBoxSingleStep, FCheckBoxRefresh: TAggControlCheckBox;

    FCurrentAngle: Double;
    FCurrentFilter, FNumSteps: Integer;

    FNumPix, FTime1, FTime2: Double;
    FPixelFormat: TAggPixelFormatProcessor;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure TransformImage(Angle: Double);

    procedure OnControlChange; override;
    procedure OnIdle; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  Rgba: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  // Initialize structures
  CPixelFormat(FPixelFormat, RenderingBufferWindow);

  FSliderStep := TAggControlSlider.Create(115, 5, 400, 11, not FlipY);
  FSliderRadius := TAggControlSlider.Create(115, 30, 400, 26, not FlipY);
  FRadioBoxFilters := TAggControlRadioBox.Create(0, 0, 110, 210, not FlipY);
  FCheckBoxNormalize := TAggControlCheckBox.Create(8, 215, 'Normalize Filter',
    not FlipY);
  FCheckBoxRun := TAggControlCheckBox.Create(8, 245, 'RUN Test!', not FlipY);
  FCheckBoxSingleStep := TAggControlCheckBox.Create(8, 230, 'Single Step',
    not FlipY);
  FCheckBoxRefresh := TAggControlCheckBox.Create(8, 265, 'Refresh', not FlipY);

  FCurrentAngle := 0;
  FCurrentFilter := 1;
  FNumSteps := 0;

  FNumPix := 0;
  FTime1 := 0;
  FTime2 := 0;

  AddControl(FSliderRadius);
  AddControl(FSliderStep);
  AddControl(FRadioBoxFilters);
  AddControl(FCheckBoxRun);
  AddControl(FCheckBoxSingleStep);
  AddControl(FCheckBoxNormalize);
  AddControl(FCheckBoxRefresh);

  FCheckBoxRun.SetTextSize(7.5);
  FCheckBoxSingleStep.SetTextSize(7.5);
  FCheckBoxNormalize.SetTextSize(7.5);
  FCheckBoxRefresh.SetTextSize(7.5);
  FCheckBoxNormalize.Status := True;

  FSliderRadius.Caption := 'Filter Radius=%.3f';
  FSliderStep.Caption := 'Step=%3.2f';
  FSliderRadius.SetRange(2, 8);
  FSliderRadius.Value := 4;
  FSliderStep.SetRange(1, 10);
  FSliderStep.Value := 5;

  FRadioBoxFilters.AddItem('simple (NN)');
  FRadioBoxFilters.AddItem('bilinear');
  FRadioBoxFilters.AddItem('bicubic');
  FRadioBoxFilters.AddItem('spline16');
  FRadioBoxFilters.AddItem('spline36');
  FRadioBoxFilters.AddItem('hanning');
  FRadioBoxFilters.AddItem('hamming');
  FRadioBoxFilters.AddItem('hermite');
  FRadioBoxFilters.AddItem('kaiser');
  FRadioBoxFilters.AddItem('quadric');
  FRadioBoxFilters.AddItem('catrom');
  FRadioBoxFilters.AddItem('gaussian');
  FRadioBoxFilters.AddItem('bessel');
  FRadioBoxFilters.AddItem('mitchell');
  FRadioBoxFilters.AddItem('sinc');
  FRadioBoxFilters.AddItem('lanczos');
  FRadioBoxFilters.AddItem('blackman');
  FRadioBoxFilters.SetCurrentItem(1);

  FRadioBoxFilters.SetBorderWidth(0, 0);
  Rgba.FromRgbaDouble(0, 0, 0, 0.1);
  FRadioBoxFilters.BackgroundColor := Rgba;
  FRadioBoxFilters.SetTextSize(6);
  FRadioBoxFilters.TextThickness := 0.85;
end;

destructor TAggApplication.Destroy;
begin
  FSliderStep.Free;
  FSliderRadius.Free;
  FRadioBoxFilters.Free;
  FCheckBoxNormalize.Free;
  FCheckBoxRun.Free;
  FCheckBoxSingleStep.Free;
  FCheckBoxRefresh.Free;
  FPixelFormat.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Ras: TAggRasterizerScanLineAA;
  Sl: TAggScanLinePacked8;

  Txt: TAggGsvText;
  Pt: TAggConvStroke;
begin
  RendererBase := TAggRendererBase.Create(FPixelFormat);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);
      RendererBase.CopyFrom(RenderingBufferImage[0], nil, 110, 35);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLinePacked8.Create;

      // Text
      Txt := TAggGsvText.Create;
      Txt.SetStartPoint(10, 295);
      Txt.SetSize(10);
      Txt.SetText(Format('NSteps=%d', [FNumSteps]));

      Pt := TAggConvStroke.Create(Txt);
      Pt.Width := 1.5;

      Ras.AddPath(Pt);
      RenScan.SetColor(CRgba8Black);
      RenderScanLines(Ras, Sl, RenScan);

      // Time
      if (FTime1 <> FTime2) and (FNumPix > 0) then
      begin
        Txt.SetStartPoint(10, 310);
        Txt.SetText(Format('%3.2f Kpix/sec', [FNumPix / (FTime2 - FTime1)]));
        Ras.AddPath(Pt);
        RenderScanLines(Ras, Sl, RenScan);
      end;

      // Render the controls
      if FRadioBoxFilters.GetCurrentItem >= 14 then
        RenderControl(Ras, Sl, RenScan, FSliderRadius);

      RenderControl(Ras, Sl, RenScan, FSliderStep);
      RenderControl(Ras, Sl, RenScan, FRadioBoxFilters);
      RenderControl(Ras, Sl, RenScan, FCheckBoxRun);
      RenderControl(Ras, Sl, RenScan, FCheckBoxNormalize);
      RenderControl(Ras, Sl, RenScan, FCheckBoxSingleStep);
      RenderControl(Ras, Sl, RenScan, FCheckBoxRefresh);

      // Free AGG resources
      Ras.Free;
      Sl.Free;

      Txt.Free;
      Pt.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.TransformImage(Angle: Double);
var
  W, H: Double;
  Pixf, PixfPre : TAggPixelFormatProcessor;

  RendererBase, RendererBasePre: TAggRendererBase;

  Rgba: TAggColor;

  Ras: TAggRasterizerScanLineAA;
  Circle: TAggCircle;

  Sl: TAggScanLineUnpacked8;
  SpanAllocator: TAggSpanAllocator;
  ConvTransform: TAggConvTransform;
  Fi: TAggCustomImageFilter;
  Sg: TAggSpanImageFilter;
  Rsi: TAggRendererScanLineAA;

  Filter: TAggImageFilterLUT;

  Interpolator: TAggSpanInterpolatorLinear;

  SourceMatrix, ImageMatrix: TAggTransAffine;

  R: Double;

  Norm: Boolean;
begin
  Fi := nil;

  // Initialize
  W := RenderingBufferImage[0].Width;
  H := RenderingBufferImage[0].Height;

  CPixelFormat(Pixf, RenderingBufferImage[0]);
  CPixelFormatPre(PixfPre, RenderingBufferImage[0]);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  RendererBasePre := TAggRendererBase.Create(PixfPre);
  try
    Rgba.White;
    RendererBase.Clear(CRgba8White);

    Ras := TAggRasterizerScanLineAA.Create;
    Sl := TAggScanLineUnpacked8.Create;
    SpanAllocator := TAggSpanAllocator.Create;

    SourceMatrix := TAggTransAffine.Create;

    SourceMatrix.Translate(-W * 0.5, -H * 0.5);
    SourceMatrix.Rotate(Deg2Rad(Angle));
    SourceMatrix.Translate(W * 0.5, H * 0.5);

    ImageMatrix := TAggTransAffine.Create;
    ImageMatrix.Assign(SourceMatrix);
    ImageMatrix.Invert;

    R := W;

    if H < R then
      R := H;

    R := R * 0.5;
    R := R - 4;

    Circle := TAggCircle.Create(PointDouble(W * 0.5, H * 0.5), R, 200);
    ConvTransform := TAggConvTransform.Create(Circle, SourceMatrix);
    try
      FNumPix := FNumPix + (R * R * Pi);

      Interpolator := TAggSpanInterpolatorLinear.Create(ImageMatrix);

      Filter := TAggImageFilterLUT.Create;
      try
        Norm := FCheckBoxNormalize.Status;

        Rgba.FromRgbaDouble(0, 0, 0, 0);

        // Render
        case FRadioBoxFilters.GetCurrentItem of
          0:
            begin
              Sg := TAggSpanImageFilterRgbNN.Create(SpanAllocator,
                RenderingBufferImage[1], @Rgba, Interpolator, CAggOrderBgr);

              Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
              try
                Ras.AddPath(ConvTransform);
                RenderScanLines(Ras, Sl, Rsi);
              finally
                Rsi.Free;
              end;
            end;

          1:
            begin
              Sg := TAggSpanImageFilterRgbBilinear.Create(SpanAllocator,
                RenderingBufferImage[1], @Rgba, Interpolator, CAggOrderBgr);

              Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
              try
                Ras.AddPath(ConvTransform);
                RenderScanLines(Ras, Sl, Rsi);
              finally
                Rsi.Free;
              end;
            end;

          5, 6, 7:
            begin
              case FRadioBoxFilters.GetCurrentItem of
                5:
                  Fi := TAggImageFilterHanning.Create;
                6:
                  Fi := TAggImageFilterHamming.Create;
                7:
                  Fi := TAggImageFilterHermite.Create;
              end;

              Filter.Calculate(Fi, Norm);

              Sg := TAggSpanImageFilterRgb2x2.Create(SpanAllocator,
                RenderingBufferImage[1], @Rgba, Interpolator, Filter,
                CAggOrderBgr);

              Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
              try
                Ras.AddPath(ConvTransform);
                RenderScanLines(Ras, Sl, Rsi);
              finally
                Rsi.Free;
              end;
            end;

          2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16:
            begin
              case FRadioBoxFilters.GetCurrentItem of
                2:
                  Fi := TAggImageFilterBicubic.Create;
                3:
                  Fi := TAggImageFilterSpline16.Create;
                4:
                  Fi := TAggImageFilterSpline36.Create;
                8:
                  Fi := TAggImageFilterKaiser.Create;
                9:
                  Fi := TAggImageFilterQuadric.Create;
                10:
                  Fi := TAggImageFilterCatrom.Create;
                11:
                  Fi := TAggImageFilterGaussian.Create;
                12:
                  Fi := TAggImageFilterBessel.Create;
                13:
                  Fi := TAggImageFilterMitchell.Create;
                14:
                  Fi := TAggImageFilterSinc.Create(FSliderRadius.Value);
                15:
                  Fi := TAggImageFilterLanczos.Create(FSliderRadius.Value);
                16:
                  Fi := TAggImageFilterBlackman.Create(FSliderRadius.Value);
              end;

              Filter.Calculate(Fi, Norm);

              Sg := TAggSpanImageFilterRgb.Create(SpanAllocator,
                RenderingBufferImage[1], @Rgba, Interpolator, Filter,
                CAggOrderBgr);

              Rsi := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
              try
                Ras.AddPath(ConvTransform);
                RenderScanLines(Ras, Sl, Rsi);
              finally
                Rsi.Free;
              end;
            end;
        end;

        // Free
        Ras.Free;
        Sl.Free;
        SpanAllocator.Free;
        Interpolator.Free;
      finally
        ConvTransform.Free;
      end;
    finally
      Filter.Free;
    end;

    if Sg <> nil then
      Sg.Free;

    if Fi <> nil then
      Fi.Free;

    Circle.Free;
    ImageMatrix.Free;
    SourceMatrix.Free;
  finally
    RendererBase.Free;
    RendererBasePre.Free;
  end;

  PixfPre.Free;
end;

procedure TAggApplication.OnControlChange;
begin
  if FCheckBoxSingleStep.Status then
  begin
    FCurrentAngle := FCurrentAngle + FSliderStep.Value;

    CopyImageToImage(1, 0);
    TransformImage(FSliderStep.Value);

    Inc(FNumSteps);

    ForceRedraw;

    FCheckBoxSingleStep.Status := False;
  end;

  if FCheckBoxRun.Status then
  begin
    StartTimer;

    FTime1 := GetElapsedTime;
    FTime2 := FTime1;

    FNumPix := 0;

    WaitMode := False;
  end;

  if FCheckBoxRefresh.Status or (FRadioBoxFilters.GetCurrentItem <>
    FCurrentFilter) then
  begin
    StartTimer;

    FTime1 := 0;
    FTime2 := 0;

    FNumPix := 0;
    FCurrentAngle := 0;

    CopyImageToImage(1, 2);
    TransformImage(0);

    FCheckBoxRefresh.Status := False;

    FCurrentFilter := FRadioBoxFilters.GetCurrentItem;
    FNumSteps := 0;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnIdle;
begin
  if FCheckBoxRun.Status then
  begin
    if FCurrentAngle < 360 then
    begin
      FCurrentAngle := FCurrentAngle + FSliderStep.Value;

      CopyImageToImage(1, 0);
      StartTimer;
      TransformImage(FSliderStep.Value);

      FTime2 := FTime2 + GetElapsedTime;

      Inc(FNumSteps);
    end
    else
    begin
      FCurrentAngle := 0;

      WaitMode := True;
      FCheckBoxRun.Status := False;
    end;

    ForceRedraw;
  end
  else
    WaitMode := True;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('The image transformer algorithm can work with different '
      + 'interpolation filters, such as Bilinear, Bicubic, Sinc, Blackman. '
      + 'The example demonstrates the difference in quality between different '
      + 'filters. When switch the "Run Test" on, the image starts rotating. '
      + 'But at each step there is the previously rotated image taken, so the '
      + 'quality degrades. This degradation as well as the performance depend '
      + 'on the type of the interpolation filter.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Text: AnsiString;
  ImageName, P, N, X: ShortString;

  W, H: Cardinal;

begin
  ImageName := 'spheres';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF}

  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'Image transformation filters comparison (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else
    begin
      CopyImageToImage(1, 0);
      CopyImageToImage(2, 0);
      TransformImage(0);

      W := RenderingBufferImage[0].Width + 110;
      H := RenderingBufferImage[0].Height + 40;

      if W < 305 then
        W := 305;

      if H < 325 then
        H := 325;

      if Init(W, H, []) then
        Run;
    end;
  finally
    Free;
  end;
end.
