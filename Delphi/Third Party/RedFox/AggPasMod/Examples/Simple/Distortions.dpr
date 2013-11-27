program Distortions;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{$DEFINE HardcodedFilter}


uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,
  Math,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

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

  AggArray in '..\..\Source\AggArray.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgb in '..\..\Source\AggSpanImageFilterRgb.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanInterpolatorAdaptor in '..\..\Source\AggSpanInterpolatorAdaptor.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas';

const
  CFlipY = True;

  GGradientColors: array [0..1023] of Int8u = (
    255, 255, 255, 255, 255, 255, 254, 255, 255, 255, 254, 255, 255, 255, 254,
    255, 255, 255, 253, 255, 255, 255, 253, 255, 255, 255, 252, 255, 255, 255,
    251, 255, 255, 255, 250, 255, 255, 255, 248, 255, 255, 255, 246, 255, 255,
    255, 244, 255, 255, 255, 241, 255, 255, 255, 238, 255, 255, 255, 235, 255,
    255, 255, 231, 255, 255, 255, 227, 255, 255, 255, 222, 255, 255, 255, 217,
    255, 255, 255, 211, 255, 255, 255, 206, 255, 255, 255, 200, 255, 255, 254,
    194, 255, 255, 253, 188, 255, 255, 252, 182, 255, 255, 250, 176, 255, 255,
    249, 170, 255, 255, 247, 164, 255, 255, 246, 158, 255, 255, 244, 152, 255,
    254, 242, 146, 255, 254, 240, 141, 255, 254, 238, 136, 255, 254, 236, 131,
    255, 253, 234, 126, 255, 253, 232, 121, 255, 253, 229, 116, 255, 252, 227,
    112, 255, 252, 224, 108, 255, 251, 222, 104, 255, 251, 219, 100, 255, 251,
    216, 96, 255, 250, 214, 93, 255, 250, 211, 89, 255, 249, 208, 86, 255, 249,
    205, 83, 255, 248, 202, 80, 255, 247, 199, 77, 255, 247, 196, 74, 255, 246,
    193, 72, 255, 246, 190, 69, 255, 245, 187, 67, 255, 244, 183, 64, 255, 244,
    180, 62, 255, 243, 177, 60, 255, 242, 174, 58, 255, 242, 170, 56, 255, 241,
    167, 54, 255, 240, 164, 52, 255, 239, 161, 51, 255, 239, 157, 49, 255, 238,
    154, 47, 255, 237, 151, 46, 255, 236, 147, 44, 255, 235, 144, 43, 255, 235,
    141, 41, 255, 234, 138, 40, 255, 233, 134, 39, 255, 232, 131, 37, 255, 231,
    128, 36, 255, 230, 125, 35, 255, 229, 122, 34, 255, 228, 119, 33, 255, 227,
    116, 31, 255, 226, 113, 30, 255, 225, 110, 29, 255, 224, 107, 28, 255, 223,
    104, 27, 255, 222, 101, 26, 255, 221, 99, 25, 255, 220, 96, 24, 255, 219,
    93, 23, 255, 218, 91, 22, 255, 217, 88, 21, 255, 216, 86, 20, 255, 215, 83,
    19, 255, 214, 81, 18, 255, 213, 79, 17, 255, 212, 77, 17, 255, 211, 74, 16,
    255, 210, 72, 15, 255, 209, 70, 14, 255, 207, 68, 13, 255, 206, 66, 13, 255,
    205, 64, 12, 255, 204, 62, 11, 255, 203, 60, 10, 255, 202, 58, 10, 255, 201,
    56, 9, 255, 199, 55, 9, 255, 198, 53, 8, 255, 197, 51, 7, 255, 196, 50, 7,
    255, 195, 48, 6, 255, 193, 46, 6, 255, 192, 45, 5, 255, 191, 43, 5, 255,
    190, 42, 4, 255, 188, 41, 4, 255, 187, 39, 3, 255, 186, 38, 3, 255, 185, 37,
    2, 255, 183, 35, 2, 255, 182, 34, 1, 255, 181, 33, 1, 255, 179, 32, 1, 255,
    178, 30, 0, 255, 177, 29, 0, 255, 175, 28, 0, 255, 174, 27, 0, 255, 173, 26,
    0, 255, 171, 25, 0, 255, 170, 24, 0, 255, 168, 23, 0, 255, 167, 22, 0, 255,
    165, 21, 0, 255, 164, 21, 0, 255, 163, 20, 0, 255, 161, 19, 0, 255, 160, 18,
    0, 255, 158, 17, 0, 255, 156, 17, 0, 255, 155, 16, 0, 255, 153, 15, 0, 255,
    152, 14, 0, 255, 150, 14, 0, 255, 149, 13, 0, 255, 147, 12, 0, 255, 145, 12,
    0, 255, 144, 11, 0, 255, 142, 11, 0, 255, 140, 10, 0, 255, 139, 10, 0, 255,
    137, 9, 0, 255, 135, 9, 0, 255, 134, 8, 0, 255, 132, 8, 0, 255, 130, 7, 0,
    255, 128, 7, 0, 255, 126, 6, 0, 255, 125, 6, 0, 255, 123, 5, 0, 255, 121, 5,
    0, 255, 119, 4, 0, 255, 117, 4, 0, 255, 115, 4, 0, 255, 113, 3, 0, 255, 111,
    3, 0, 255, 109, 2, 0, 255, 107, 2, 0, 255, 105, 2, 0, 255, 103, 1, 0, 255,
    101, 1, 0, 255, 99, 1, 0, 255, 97, 0, 0, 255, 95, 0, 0, 255, 93, 0, 0, 255,
    91, 0, 0, 255, 90, 0, 0, 255, 88, 0, 0, 255, 86, 0, 0, 255, 84, 0, 0, 255,
    82, 0, 0, 255, 80, 0, 0, 255, 78, 0, 0, 255, 77, 0, 0, 255, 75, 0, 0, 255,
    73, 0, 0, 255, 72, 0, 0, 255, 70, 0, 0, 255, 68, 0, 0, 255, 67, 0, 0, 255,
    65, 0, 0, 255, 64, 0, 0, 255, 63, 0, 0, 255, 61, 0, 0, 255, 60, 0, 0, 255,
    59, 0, 0, 255, 58, 0, 0, 255, 57, 0, 0, 255, 56, 0, 0, 255, 55, 0, 0, 255,
    54, 0, 0, 255, 53, 0, 0, 255, 53, 0, 0, 255, 52, 0, 0, 255, 52, 0, 0, 255,
    51, 0, 0, 255, 51, 0, 0, 255, 51, 0, 0, 255, 50, 0, 0, 255, 50, 0, 0, 255,
    51, 0, 0, 255, 51, 0, 0, 255, 51, 0, 0, 255, 51, 0, 0, 255, 52, 0, 0, 255,
    52, 0, 0, 255, 53, 0, 0, 255, 54, 1, 0, 255, 55, 2, 0, 255, 56, 3, 0, 255,
    57, 4, 0, 255, 58, 5, 0, 255, 59, 6, 0, 255, 60, 7, 0, 255, 62, 8, 0, 255,
    63, 9, 0, 255, 64, 11, 0, 255, 66, 12, 0, 255, 68, 13, 0, 255, 69, 14, 0,
    255, 71, 16, 0, 255, 73, 17, 0, 255, 75, 18, 0, 255, 77, 20, 0, 255, 79, 21,
    0, 255, 81, 23, 0, 255, 83, 24, 0, 255, 85, 26, 0, 255, 87, 28, 0, 255, 90,
    29, 0, 255, 92, 31, 0, 255, 94, 33, 0, 255, 97, 34, 0, 255, 99, 36, 0, 255,
    102, 38, 0, 255, 104, 40, 0, 255, 107, 41, 0, 255, 109, 43, 0, 255, 112, 45,
    0, 255, 115, 47, 0, 255, 117, 49, 0, 255, 120, 51, 0, 255, 123, 52, 0, 255,
    126, 54, 0, 255, 128, 56, 0, 255, 131, 58, 0, 255, 134, 60, 0, 255, 137, 62,
    0, 255, 140, 64, 0, 255, 143, 66, 0, 255, 145, 68, 0, 255, 148, 70, 0, 255,
    151, 72, 0, 255, 154, 74, 0, 255);

type
  TPeriodicDistortion = class(TAggDistortion)
  private
    FPeriod, FAmplitude, FPhase: Double;
    FCenter: TPointDouble;
    procedure SetPeriod(Value: Double);
    procedure SetAmplitude(Value: Double);
    procedure SetPhase(Value: Double);
  public
    constructor Create;

    procedure SetCenter(X, Y: Double); overload;
    procedure SetCenter(Center: TPointDouble); overload;

    property Amplitude: Double read FAmplitude write SetAmplitude;
    property Period: Double read FPeriod write SetPeriod;
    property Phase: Double read FPhase write SetPhase;
  end;

  TAggDistortionWave = class(TPeriodicDistortion)
  public
//    procedure Calculate(X, Y: PInteger); override;
    procedure Calculate(var X, Y: Integer); override;
  end;

  TAggDistortionSwirl = class(TPeriodicDistortion)
  public
//    procedure Calculate(X, Y: PInteger); override;
    procedure Calculate(var X, Y: Integer); override;
  end;

  TAggDistortionSwirlWave = class(TPeriodicDistortion)
  public
//    procedure Calculate(X, Y: PInteger); override;
    procedure Calculate(var X, Y: Integer); override;
  end;

  TAggDistortionWaveSwirl = class(TPeriodicDistortion)
  public
//    procedure Calculate(X, Y: PInteger); override;
    procedure Calculate(var X, Y: Integer); override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSliderAngle, FSliderScale: TAggControlSlider;
    FSliderAmplitude, FSliderPeriod: TAggControlSlider;
    FRadioBoxDistortion: TAggControlRadioBox;

    FCenter : TPointDouble;
    FPhase: Double;

    FGradientColors: TAggPodAutoArray;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnIdle; override;
    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;

procedure CalculateWave(var X, Y: Integer; Center: TPointDouble; Period,
  Amplitude, Phase: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
var
  Xd, Yd, D, A: Double;
const
  CAggImageSubpixelSizeInv : Double = 1 / CAggImageSubpixelSize;
begin
  Xd := X * CAggImageSubpixelSizeInv - Center.X;
  Yd := Y * CAggImageSubpixelSizeInv - Center.Y;
  D := Hypot(Xd, Yd);

  if D > 1 then
  begin
    A := Cos(D / (16 * Period) - Phase) / (Amplitude * D) + 1;
    X := Trunc((Xd * A + Center.X) * CAggImageSubpixelSize);
    Y := Trunc((Yd * A + Center.Y) * CAggImageSubpixelSize);
  end;
end;

procedure CalculateSwirl(var X, Y: Integer; Center: TPointDouble; Amplitude,
  Phase: Double); inline; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
var
  Xd, Yd, A, Sa, Ca: Double;
const
  CAggImageSubpixelSizeInv : Double = 1 / CAggImageSubpixelSize;
begin
  Xd := X * CAggImageSubpixelSizeInv - Center.X;
  Yd := Y * CAggImageSubpixelSizeInv - Center.Y;
  A := (Hypot(Xd, Yd) - 100) * 0.001 / Amplitude;
  SinCos(A - Phase * 0.04, Sa, Ca);
  X := Trunc((Xd * Ca - Yd * Sa + Center.X) * CAggImageSubpixelSize);
  Y := Trunc((Xd * Sa + Yd * Ca + Center.Y) * CAggImageSubpixelSize);
end;


{ TPeriodicDistortion }

constructor TPeriodicDistortion.Create;
begin
  FCenter := PointDouble(0);
  FPeriod := 0.5;
  FAmplitude := 0.5;
  FPhase := 0.0;
  inherited;
end;

procedure TPeriodicDistortion.SetCenter(X, Y: Double);
begin
  FCenter.X := X;
  FCenter.Y := Y;
end;

procedure TPeriodicDistortion.SetCenter(Center: TPointDouble);
begin
  FCenter := Center;
end;

procedure TPeriodicDistortion.SetPeriod(Value: Double);
begin
  FPeriod := Value;
end;

procedure TPeriodicDistortion.SetAmplitude(Value: Double);
begin
  FAmplitude := 1.0 / Value;
end;

procedure TPeriodicDistortion.SetPhase(Value: Double);
begin
  FPhase := Value;
end;


{ TAggDistortionWave }

procedure TAggDistortionWave.Calculate(var X, Y: Integer);
begin
  CalculateWave(X, Y, FCenter, FPeriod, FAmplitude, FPhase);
end;


{ TAggDistortionSwirl }

procedure TAggDistortionSwirl.Calculate(var X, Y: Integer);
begin
  CalculateSwirl(X, Y, FCenter, FAmplitude, FPhase);
end;


{ TAggDistortionSwirlWave }

procedure TAggDistortionSwirlWave.Calculate(var X, Y: Integer);
begin
  CalculateSwirl(X, Y, FCenter, FAmplitude, FPhase);
  CalculateWave(X, Y, FCenter, FPeriod, FAmplitude, FPhase);
end;


{ TAggDistortionWaveSwirl }

procedure TAggDistortionWaveSwirl.Calculate(var X, Y: Integer);
begin
  CalculateWave(X, Y, FCenter, FPeriod, FAmplitude, FPhase);
  CalculateSwirl(X, Y, FCenter, FAmplitude, FPhase);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  I: Cardinal;
  P: PInt8u;

  Rgba: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FGradientColors := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  FSliderAngle := TAggControlSlider.Create(5, 5, 150, 12, not FlipY);
  FSliderScale := TAggControlSlider.Create(5, 5 + 15, 150, 12 + 15, not FlipY);
  FSliderPeriod := TAggControlSlider.Create(5 + 170, 5, 150 + 170, 12,
    not FlipY);
  FSliderAmplitude := TAggControlSlider.Create(5 + 170, 5 + 15, 150 + 170,
    12 + 15, not FlipY);
  FRadioBoxDistortion := TAggControlRadioBox.Create(480, 5, 600, 90, not FlipY);

  FCenter.X := 0.0;
  FCenter.Y := 0.0;
  FPhase := 0.0;

  AddControl(FSliderAngle);
  AddControl(FSliderScale);
  AddControl(FSliderAmplitude);
  AddControl(FSliderPeriod);
  AddControl(FRadioBoxDistortion);

  FSliderAngle.Caption := 'Angle=%3.2f';
  FSliderScale.Caption := 'Scale=%3.2f';
  FSliderAngle.SetRange(-180.0, 180.0);
  FSliderAngle.Value := 20.0;
  FSliderScale.SetRange(0.1, 5.0);
  FSliderScale.Value := 1.0;

  FSliderAmplitude.Caption := 'Amplitude=%3.2f';
  FSliderPeriod.Caption := 'Period=%3.2f';
  FSliderAmplitude.SetRange(0.1, 40.0);
  FSliderPeriod.SetRange(0.1, 2.0);
  FSliderAmplitude.Value := 10.0;
  FSliderPeriod.Value := 1.0;

  FRadioBoxDistortion.AddItem('Wave');
  FRadioBoxDistortion.AddItem('Swirl');
  FRadioBoxDistortion.AddItem('Wave-Swirl');
  FRadioBoxDistortion.AddItem('Swirl-Wave');
  FRadioBoxDistortion.SetCurrentItem(0);

  P := @GGradientColors[0];

  for I := 0 to 255 do
  begin
    Rgba.FromRgbaInteger(PInt8u(P)^, PInt8u(PtrComp(P) + SizeOf(Int8u))^,
      PInt8u(PtrComp(P) + 2 * SizeOf(Int8u))^,
      PInt8u(PtrComp(P) + 3 * SizeOf(Int8u))^);

    Move(Rgba, FGradientColors[I]^, SizeOf(TAggColor));

    Inc(PtrComp(P), 4 * SizeOf(Int8u));
  end;
end;

destructor TAggApplication.Destroy;
begin
  FGradientColors.Free;

  FSliderAngle.Free;
  FSliderScale.Free;
  FSliderPeriod.Free;
  FSliderAmplitude.Free;
  FRadioBoxDistortion.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  FCenter.X := RenderingBufferImage[0].Width * 0.5 + 10;
  FCenter.Y := RenderingBufferImage[0].Height * 0.5 + 10 + 40;
end;

procedure TAggApplication.OnDraw;
type
  TFilterType = (ftNearestNeighbor, ftHardcodedFilter, ftArbitraryFilter);
var
  ImageWidth, ImageHeight, R: Double;
  Center: TPointDouble;

  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl: TAggScanLineUnpacked8;
  Sa: TAggSpanAllocator;
  Sg: TAggSpanImageFilter;
  Ri: TAggRendererScanLineAA;

  Filter: TAggImageFilter;
  FilterType: TFilterType;

  Interpolator: TAggSpanInterpolatorAdaptor;

  Dist: TPeriodicDistortion;

  SourceMatrix, ImageMatrix, Gr1Matrix, Gr2Matrix: TAggTransAffine;

  Inv: TAggTransAffine;

  Circle: TAggCircle;
  ConvTrans: TAggConvTransform;

  GradientFunction: TAggGradientCircle;
  SpanGradient: TAggSpanGradient;

  Rg: TAggRendererScanLineAA;

  Fi: TAggImageFilterSpline36;
begin
  Filter := nil;
  Fi := nil;

  // Initialize structures
  ImageWidth := RenderingBufferImage[0].Width;
  ImageHeight := RenderingBufferImage[0].Height;

  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      SourceMatrix := TAggTransAffine.Create;

      SourceMatrix.Translate(-ImageWidth * 0.5, -ImageHeight * 0.5);
      SourceMatrix.Rotate(Deg2Rad(FSliderAngle.Value));
      SourceMatrix.Translate(ImageWidth * 0.5 + 10, ImageHeight * 0.5 + 50);
      SourceMatrix.Multiply(GetTransAffineResizing);

      ImageMatrix := TAggTransAffine.Create;

      ImageMatrix.Translate(-ImageWidth * 0.5, -ImageHeight * 0.5);
      ImageMatrix.Rotate(Deg2Rad(FSliderAngle.Value));
      ImageMatrix.Scale(FSliderScale.Value);

      ImageMatrix.Translate(ImageWidth * 0.5 + 10, ImageHeight * 0.5 + 50);

      ImageMatrix.Multiply(GetTransAffineResizing);
      ImageMatrix.Invert;

      Sa := TAggSpanAllocator.Create;

      try
        case FRadioBoxDistortion.GetCurrentItem of
          0:
            Dist := TAggDistortionWave.Create;
          1:
            Dist := TAggDistortionSwirl.Create;
          2:
            Dist := TAggDistortionWaveSwirl.Create;
          3:
            Dist := TAggDistortionSwirlWave.Create;
        end;

        Dist.Period := FSliderPeriod.Value;
        Dist.Amplitude := FSliderAmplitude.Value;
        Dist.Phase := FPhase;

        Center := FCenter;

        ImageMatrix.Transform(ImageMatrix, @Center.X, @Center.Y);

        Dist.SetCenter(Center);

        Interpolator := TAggSpanInterpolatorAdaptor.Create(ImageMatrix, Dist);

        Rgba.FromRgbaDouble(1, 1, 1, 0);

        FilterType := ftHardcodedFilter;
        case FilterType of
          ftNearestNeighbor:
            Sg := TAggSpanImageFilterRgbNN.Create(Sa,
              RenderingBufferImage[0],  @Rgba, Interpolator, CAggOrderBgr);
          ftHardcodedFilter:
            Sg := TAggSpanImageFilterRgbBilinear.Create(Sa,
              RenderingBufferImage[0], @Rgba, Interpolator, CAggOrderBgr);
          ftArbitraryFilter:
            begin
              // Version with arbitrary filter
              Fi := TAggImageFilterSpline36.Create;

              Filter := TAggImageFilter.Create(Fi);

              Sg := TAggSpanImageFilterRgb.Create(Sa, RenderingBufferImage[0],
                @Rgba, Interpolator, Filter, CAggOrderBgr);
            end;
          else
            raise Exception.Create('Unknown Filter Type');
        end;

        // Render
        Ri := TAggRendererScanLineAA.Create(RendererBase, Sg);
        try
          Ras := TAggRasterizerScanLineAA.Create;
          Sl := TAggScanLineUnpacked8.Create;

          R := ImageWidth;

          if ImageHeight < R then
            R := ImageHeight;

          Circle := TAggCircle.Create(ImageWidth * 0.5, ImageHeight * 0.5,
            R * 0.5 - 20.0, 200);

          ConvTrans := TAggConvTransform.Create(Circle, SourceMatrix);
          try
            Ras.AddPath(ConvTrans);

            RenderScanLines(Ras, Sl, Ri);

            Inv := TAggTransAffine.Create;

            Inv.Assign(GetTransAffineResizing);

            Inv.Invert;

            SourceMatrix.Multiply(Inv);
            SourceMatrix.Translate(ImageWidth - ImageWidth * 0.1, 0.0);
            SourceMatrix.Multiply(GetTransAffineResizing);

            Ras.AddPath(ConvTrans);
          finally
            ConvTrans.Free;
          end;
        finally
          Ri.Free;
        end;

        RenScan.SetColor(CRgba8Black);
        RenderScanLines(Ras, Sl, RenScan);

        GradientFunction := TAggGradientCircle.Create;
        try
          SpanGradient := TAggSpanGradient.Create(Sa, Interpolator,
            GradientFunction, FGradientColors, 0, 180);

          Rg := TAggRendererScanLineAA.Create(RendererBase, SpanGradient);
          try
            Gr1Matrix := TAggTransAffine.Create;

            Gr1Matrix.Translate(-ImageWidth * 0.5, -ImageHeight * 0.5);
            Gr1Matrix.Scale(0.8);
            Gr1Matrix.Rotate(Deg2Rad(FSliderAngle.Value));
            Gr1Matrix.Translate(ImageWidth - ImageWidth * 0.1 +
              ImageWidth * 0.5 + 10, ImageHeight * 0.5 + 50);
            Gr1Matrix.Multiply(GetTransAffineResizing);

            Gr2Matrix := TAggTransAffine.Create;

            Gr2Matrix.Rotate(Deg2Rad(FSliderAngle.Value));
            Gr2Matrix.Scale(FSliderScale.Value);
            Gr2Matrix.Translate(ImageWidth - ImageWidth * 0.1 +
              ImageWidth * 0.5 + 60, ImageHeight * 0.5 + 100);

            Gr2Matrix.Multiply(GetTransAffineResizing);
            Gr2Matrix.Invert;

            Center.X := FCenter.X + ImageWidth - ImageWidth * 0.1;
            Center.Y := FCenter.Y;

            Gr2Matrix.Transform(Gr2Matrix, @Center.X, @Center.Y);

            Dist.SetCenter(Center);

            Interpolator.Transformer := Gr2Matrix;

            ConvTrans := TAggConvTransform.Create(Circle, Gr1Matrix);
            try
              Ras.AddPath(ConvTrans);
            finally
              ConvTrans.Free;
            end;
            RenderScanLines(Ras, Sl, Rg);
          finally
            Rg.Free;
          end;

          // Render the controls
          RenderControl(Ras, Sl, RenScan, FSliderAngle);
          RenderControl(Ras, Sl, RenScan, FSliderScale);
          RenderControl(Ras, Sl, RenScan, FSliderAmplitude);
          RenderControl(Ras, Sl, RenScan, FSliderPeriod);
          RenderControl(Ras, Sl, RenScan, FRadioBoxDistortion);

          // Free AGG resources
          Circle.Free;
          SpanGradient.Free;
          Ras.Free;
          Sl.Free;
          Sa.Free;
          Inv.Free;

          if Sg <> nil then
            Sg.Free;

          if Assigned(Fi) then
            Fi.Free;

          if Filter <> nil then
            Filter.Free;
        finally
          GradientFunction.Free;
        end;

        Gr1Matrix.Free;
        Gr2Matrix.Free;
        SourceMatrix.Free;
        ImageMatrix.Free;
      finally
        Dist.Free;
        Interpolator.Free;
      end;
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
  begin
    FCenter.X := X;
    FCenter.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if Flags <> [] then
  begin
    FCenter.X := X;
    FCenter.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnIdle;
begin
  FPhase := FPhase + Deg2Rad(15.0);

  if FPhase > Pi * 200.0 then
    FPhase := FPhase - (Pi * 200.0);

  ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('To transform an image as well as to define a color '
      + 'gradient you have to write several declarations. This approach can '
      + 'seem difficult to handle (compared with one function call), but it''s '
      + 'very flexible. For example, you can add an arbitrary distortion '
      + 'function. This mechanism is pretty much the same in image '
      + 'transformers and color gradients.'#13#13 + 'How to play with:'#13#13
      + 'Try changing different parameters of the distortions.'#13#13
      + 'Use any mouse button to move the distortion''s epicentre.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  ImageName, P, N, X: ShortString;
  Text: AnsiString;

begin
  ImageName := 'spheres';

{$IFDEF WIN32 }
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF }

  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'Image and Gradient Distortions (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(RenderingBufferImage[0].Width + 300,
      RenderingBufferImage[0].Height + 40 + 20, [wfResize]) then
    begin
      WaitMode := False;
      Run;
    end;
  finally
    Free;
  end;
end.
