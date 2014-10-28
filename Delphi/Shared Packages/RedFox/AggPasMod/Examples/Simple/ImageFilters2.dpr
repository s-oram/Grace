program ImageFilters2;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

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
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas';

const
  CFlipY = True;

  GImage: array [0..47] of Int8u = (0, 255, 0, 0, 0, 255, 255, 255, 255, 255,
    0, 0, 255, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 0, 0, 255, 255, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0,
    0, 255, 0);

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderGamma, FSliderRadius: TAggControlSlider;

    FRadioBoxFilters: TAggControlRadioBox;
    FCheckBoxNormalize: TAggControlCheckBox;

    FCurAngle: Double;
    FCurFilter, FNumSteps: Integer;

    FNumPixels, FTime1, FTime2: Double;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  Rgba: TAggColor;

begin
  inherited Create(PixelFormat, FlipY);

  FSliderGamma := TAggControlSlider.Create(115, 5, 495, 11, not FlipY);
  FSliderRadius := TAggControlSlider.Create(115, 30, 495, 26, not FlipY);
  FRadioBoxFilters := TAggControlRadioBox.Create(0, 0, 110, 210, not FlipY);
  FCheckBoxNormalize := TAggControlCheckBox.Create(8, 215, 'Normalize Filter',
    not FlipY);

  FCurAngle := 0;
  FCurFilter := 1;
  FNumSteps := 0;

  FNumPixels := 0;
  FTime1 := 0;
  FTime2 := 0;

  AddControl(FSliderGamma);
  AddControl(FSliderRadius);
  AddControl(FRadioBoxFilters);
  AddControl(FCheckBoxNormalize);

  FCheckBoxNormalize.SetTextSize(7.5);
  FCheckBoxNormalize.Status := True;

  FSliderRadius.Caption := 'Filter Radius=%.3f';
  FSliderRadius.SetRange(2, 8);
  FSliderRadius.Value := 4;

  FSliderGamma.Caption := 'Gamma=%.3f';
  FSliderGamma.SetRange(0.5, 3);
  FSliderGamma.Value := 1;

  FRadioBoxFilters.AddItem('Simple (NN)');
  FRadioBoxFilters.AddItem('Bilinear');
  FRadioBoxFilters.AddItem('Bicubic');
  FRadioBoxFilters.AddItem('Spline16');
  FRadioBoxFilters.AddItem('Spline36');
  FRadioBoxFilters.AddItem('Hanning');
  FRadioBoxFilters.AddItem('Hamming');
  FRadioBoxFilters.AddItem('Hermite');
  FRadioBoxFilters.AddItem('Kaiser');
  FRadioBoxFilters.AddItem('Quadric');
  FRadioBoxFilters.AddItem('Catrom');
  FRadioBoxFilters.AddItem('Gaussian');
  FRadioBoxFilters.AddItem('Bessel');
  FRadioBoxFilters.AddItem('Mitchell');
  FRadioBoxFilters.AddItem('Sinc');
  FRadioBoxFilters.AddItem('Lanczos');
  FRadioBoxFilters.AddItem('Blackman');
  FRadioBoxFilters.SetCurrentItem(1);

  FRadioBoxFilters.SetBorderWidth(0, 0);
  Rgba.FromRgbaDouble(0, 0, 0, 0.1);
  FRadioBoxFilters.BackgroundColor := Rgba;
  FRadioBoxFilters.SetTextSize(6);
  FRadioBoxFilters.TextThickness := 0.85;
end;

destructor TAggApplication.Destroy;
begin
  FSliderGamma.Free;
  FSliderRadius.Free;
  FRadioBoxFilters.Free;
  FCheckBoxNormalize.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
const
  CPara: array [0..7] of Double = (200, 40, 500, 40, 500, 340, 200, 340);
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Sl: TAggScanLineUnpacked8;
  SpanAllocator: TAggSpanAllocator;
  Rsi: TAggRendererScanLineAA;
  Sg: TAggSpanImageFilter;
  Fi: TAggCustomImageFilter;

  Ras: TAggRasterizerScanLineAA;

  Norm: Boolean;

  Gamma : TAggGammaLut8;
  Stroke: TAggConvStroke;
  Filter: TAggImageFilterLUT;

  ImgRenderingBuffer: TAggRenderingBuffer;
  ImgMatrix : TAggTransAffine;
  Weights : PInt16;

  Interpolator: TAggSpanInterpolatorLinear;

  Start, Stop: TPointDouble;
  X, Xs, Ys, Radius: Double;
  Delta: TPointDouble;

  I, N, Nn: Cardinal;

  PathStorage: TAggPathStorage;
begin
  Fi := nil;

  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);
      RendererBase.CopyFrom(RenderingBufferImage[0], 0, 110, 35);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLineUnpacked8.Create;
      ImgRenderingBuffer := TAggRenderingBuffer.Create(@GImage[0], 4, 4, 12);

      // Render
      ImgMatrix := TAggTransAffine.Create(@CPara[0], 0, 0, 4, 4);

      Interpolator := TAggSpanInterpolatorLinear.Create(ImgMatrix);
      SpanAllocator := TAggSpanAllocator.Create;

      Ras.Reset;
      Ras.MoveToDouble(CPara[0], CPara[1]);
      Ras.LineToDouble(CPara[2], CPara[3]);
      Ras.LineToDouble(CPara[4], CPara[5]);
      Ras.LineToDouble(CPara[6], CPara[7]);

      Rgba.White;
      Filter := TAggImageFilterLUT.Create;

      case FRadioBoxFilters.GetCurrentItem of
        0:
          begin
            Sg := TAggSpanImageFilterRgbNN.Create(SpanAllocator,
              ImgRenderingBuffer, @Rgba, Interpolator, CAggOrderBgr);

            Rsi := TAggRendererScanLineAA.Create(RendererBase, Sg);
            RenderScanLines(Ras, Sl, Rsi);
          end;

        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16:
          begin
            Norm := FCheckBoxNormalize.Status;

            case FRadioBoxFilters.GetCurrentItem of
              1:
                Fi := TAggImageFilterBilinear.Create;
              2:
                Fi := TAggImageFilterBicubic.Create;
              3:
                Fi := TAggImageFilterSpline16.Create;
              4:
                Fi := TAggImageFilterSpline36.Create;
              5:
                Fi := TAggImageFilterHanning.Create;
              6:
                Fi := TAggImageFilterHamming.Create;
              7:
                Fi := TAggImageFilterHermite.Create;
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
              ImgRenderingBuffer, @Rgba, Interpolator, Filter, CAggOrderBgr);

            Rsi := TAggRendererScanLineAA.Create(RendererBase, Sg);
            RenderScanLines(Ras, Sl, Rsi);

            // Draw Graph
            Gamma := TAggGammaLut8.Create(FSliderGamma.Value);
            Pixf.ApplyGammaInv(Gamma, CAggOrderBgr);

            Start := PointDouble(5, 235);
            Stop := PointDouble(195, FInitialHeight - 5);

            PathStorage := TAggPathStorage.Create;
            Stroke := TAggConvStroke.Create(PathStorage);
            Stroke.Width := 0.8;

            for I := 0 to 16 do
            begin
              X := Start.X + (Stop.X - Start.X) * I / 16;

              PathStorage.RemoveAll;
              PathStorage.MoveTo(X + 0.5, Start.Y);
              PathStorage.LineTo(X + 0.5, Stop.Y);

              Ras.AddPath(Stroke);

              if I = 8 then
                Rgba.FromRgbaInteger(0, 0, 0, 255)
              else
                Rgba.FromRgbaInteger(0, 0, 0, 100);

              RenScan.SetColor(@Rgba);
              RenderScanLines(Ras, Sl, RenScan);
            end;

            Ys := Start.Y + (Stop.Y - Start.Y) / 6;

            PathStorage.RemoveAll;
            PathStorage.MoveTo(Start.X, Ys);
            PathStorage.LineTo(Stop.X, Ys);

            Ras.AddPath(Stroke);
            RenScan.SetColor(CRgba8Black);
            RenderScanLines(Ras, Sl, RenScan);

            Radius := Filter.Radius;

            N := Trunc(Radius * 512);
            Delta.X := (Stop.X - Start.X) * Radius * 0.125;
            Delta.Y := Stop.Y - Ys;

            Weights := Filter.WeightArray;

            Xs := (Stop.X + Start.X) * 0.5 -
              (Filter.Diameter * (Stop.X - Start.X) / 32);
            Nn := Filter.Diameter * 256;

            PathStorage.RemoveAll;
            PathStorage.MoveTo(Xs + 0.5, Ys + Delta.Y * PInt16(Weights)^ /
              CAggImageFilterSize);

            I := 1;

            while I < Nn do
            begin
              PathStorage.LineTo(Xs + Delta.X * I / N + 0.5,
                Ys + Delta.Y * PInt16(PtrComp(Weights) + I * SizeOf(Int16))^ /
                CAggImageFilterSize);

              Inc(I);
            end;

            Ras.AddPath(Stroke);
            Rgba.FromRgbaInteger(100, 0, 0);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Ras, Sl, RenScan);

            // Free
            Gamma.Free;
            PathStorage.Free;
            Stroke.Free;
            Interpolator.Free;
          end;
      end;

      // Render the controls
      RenderControl(Ras, Sl, RenScan, FSliderGamma);

      if FRadioBoxFilters.GetCurrentItem >= 14 then
        RenderControl(Ras, Sl, RenScan, FSliderRadius);

      RenderControl(Ras, Sl, RenScan, FRadioBoxFilters);
      RenderControl(Ras, Sl, RenScan, FCheckBoxNormalize);

      // Free AGG resources
      Ras.Free;
      Sl.Free;
      Rsi.Free;
      ImgRenderingBuffer.Free;
      ImgMatrix.Free;

      SpanAllocator.Free;
      Filter.Free;

      if Sg <> nil then
        Sg.Free;

      if Fi <> nil then
        Fi.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Another example that demonstrates the difference of image '
      + 'filters. It just displays a simple 4x4 pixels image with huge zoom. '
      + 'You can see how different filters affect the result. Also see how '
      + 'Gamma correction works.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'Image transformation filters comparison (F1-Help)';

    if Init(500, 340, []) then
      Run;
  finally
    Free;
  end;
end.
