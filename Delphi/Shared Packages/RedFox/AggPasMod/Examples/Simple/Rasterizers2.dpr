program Rasterizers2;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{-$DEFINE AntiAliasedOutlineRenderer}

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
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutline in '..\..\Source\AggRasterizerOutline.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggSpiral in '..\..\Source\AggSpiral.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRendererOutlineImage in '..\..\Source\AggRendererOutlineImage.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggPatternFiltersRgba in '..\..\Source\AggPatternFiltersRgba.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas'
{$I Pixel_Formats.inc}

const
  CFlipY = True;

  CPixelMapChain: array [0 .. 113] of Int32u = (16, 7, $00FFFFFF, $00FFFFFF,
    $00FFFFFF, $00FFFFFF, $B4C29999, $FF9A5757, $FF9A5757, $FF9A5757, $FF9A5757,
    $FF9A5757, $FF9A5757, $B4C29999, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF,
    $00FFFFFF, $00FFFFFF, $0CFBF9F9, $FF9A5757, $FF660000, $FF660000, $FF660000,
    $FF660000, $FF660000, $FF660000, $FF660000, $FF660000, $B4C29999, $00FFFFFF,
    $00FFFFFF, $00FFFFFF, $00FFFFFF, $5AE0CCCC, $FFA46767, $FF660000, $FF975252,
    $7ED4B8B8, $5AE0CCCC, $5AE0CCCC, $5AE0CCCC, $5AE0CCCC, $A8C6A0A0, $FF7F2929,
    $FF670202, $9ECAA6A6, $5AE0CCCC, $00FFFFFF, $FF660000, $FF660000, $FF660000,
    $FF660000, $FF660000, $FF660000, $A4C7A2A2, $3AFFFF00, $3AFFFF00, $FF975151,
    $FF660000, $FF660000, $FF660000, $FF660000, $FF660000, $FF660000, $00FFFFFF,
    $5AE0CCCC, $FFA46767, $FF660000, $FF954F4F, $7ED4B8B8, $5AE0CCCC, $5AE0CCCC,
    $5AE0CCCC, $5AE0CCCC, $A8C6A0A0, $FF7F2929, $FF670202, $9ECAA6A6, $5AE0CCCC,
    $00FFFFFF, $00FFFFFF, $00FFFFFF, $0CFBF9F9, $FF9A5757, $FF660000, $FF660000,
    $FF660000, $FF660000, $FF660000, $FF660000, $FF660000, $FF660000, $B4C29999,
    $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF,
    $B4C29999, $FF9A5757, $FF9A5757, $FF9A5757, $FF9A5757, $FF9A5757, $FF9A5757,
    $B4C29999, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF);

type
  TPatternPixelMapArgb32 = class(TAggPixelSource)
  private
    FPixMap: PInt32u;
  protected
    function GetWidth: Cardinal; override;
    function GetHeight: Cardinal; override;
  public
    constructor Create(Pixmap: PInt32u);

    function Pixel(X, Y: Integer): TAggRgba8; override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSliderStep: TAggControlSlider;
    FSliderWidth: TAggControlSlider;
    FCheckBoxTest: TAggControlCheckBox;
    FCheckBoxRotate: TAggControlCheckBox;
    FCheckBoxAccurateJoins: TAggControlCheckBox;
    FCheckBoxScalePattern: TAggControlCheckBox;
    FStartAngle: Double;
    FPixelFormat: TAggPixelFormatProcessor;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure DrawAliasedPixAccuracy(RasOutline: TAggRasterizerOutline;
      Prim: TAggRendererPrimitives);
    procedure DrawAliasedSubpixAccuracy(RasOutline: TAggRasterizerOutline;
      Prim: TAggRendererPrimitives);

    procedure DrawAntiAliasedOutline(Rasterizer: TAggRasterizerOutlineAA;
      Renderer: TAggRendererOutlineAA);

    procedure DrawAntiAliasedScanLine(Rasterizer: TAggRasterizerScanLineAA;
      ScanLine: TAggCustomScanLine; Renderer: TAggRendererScanLineAASolid);

    procedure DrawAntiAliasedOutlineImage(Rasterizer: TAggRasterizerOutlineAA;
      Renderer: TAggRendererOutline);

    procedure Text(Rasterizer: TAggRasterizerScanLineAA; ScanLine: TAggCustomScanLine;
      Renderer: TAggRendererScanLineAASolid; X, Y: Double; Txt: PAnsiChar);

    procedure OnDraw; override;
    procedure OnIdle; override;
    procedure OnControlChange; override;
    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); override;
  end;


{ TPatternPixelMapArgb32 }

constructor TPatternPixelMapArgb32.Create(Pixmap: PInt32u);
begin
  FPixMap := Pixmap;
end;

function TPatternPixelMapArgb32.GetWidth: Cardinal;
begin
  Result := FPixMap^;
end;

function TPatternPixelMapArgb32.GetHeight;
begin
  Result := PInt32u(PtrComp(FPixMap) + SizeOf(Int32u))^;
end;

function TPatternPixelMapArgb32.Pixel;
var
  P: Int32u;
begin
  P := PInt32u(PtrComp(FPixMap) + (Y * Width + X + 2) * SizeOf(Int32u))^;

  Result.R := (P shr 16) and $FF;
  Result.G := (P shr 8) and $FF;
  Result.B := P and $FF;
  Result.A := P shr 24;
end;

procedure RoundOff(This: TAggTransAffine; X, Y: PDouble);
begin
  X^ := Floor(X^);
  Y^ := Floor(Y^);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  CPixelFormat(FPixelFormat, RenderingBufferWindow);

  FSliderStep := TAggControlSlider.Create(10.0, 10.0 + 4.0, 150.0,
    10.0 + 8.0 + 4.0, not FlipY);
  FSliderWidth := TAggControlSlider.Create(150.0 + 10.0, 10.0 + 4.0, 400 - 10.0,
    10.0 + 8.0 + 4.0, not FlipY);
  FCheckBoxTest := TAggControlCheckBox.Create(10.0, 10.0 + 4.0 + 16.0,
    'Test Performance', not FlipY);
  FCheckBoxRotate:= TAggControlCheckBox.Create(130 + 10.0, 10.0 + 4.0 + 16.0,
    'Rotate', not FlipY);
  FCheckBoxAccurateJoins:= TAggControlCheckBox.Create(200 + 10.0,
    10.0 + 4.0 + 16.0, 'Accurate Joins', not FlipY);
  FCheckBoxScalePattern:= TAggControlCheckBox.Create(310 + 10.0,
    10.0 + 4.0 + 16.0, 'Scale Pattern', not FlipY);

  FStartAngle := 0.0;

  AddControl(FSliderStep);

  FSliderStep.SetRange(0.0, 2.0);
  FSliderStep.Value := 0.1;
  FSliderStep.Caption := 'Step=%1.2f';
  FSliderStep.NoTransform;

  AddControl(FSliderWidth);

  FSliderWidth.SetRange(0.0, 7.0);
  FSliderWidth.Value := 3.0;
  FSliderWidth.Caption := 'Width=%1.2f';
  FSliderWidth.NoTransform;

  AddControl(FCheckBoxTest);

  FCheckBoxTest.SetTextSize(9.0, 7.0);
  FCheckBoxTest.NoTransform;

  AddControl(FCheckBoxRotate);

  FCheckBoxRotate.SetTextSize(9.0, 7.0);
  FCheckBoxRotate.NoTransform;

  AddControl(FCheckBoxAccurateJoins);

  FCheckBoxAccurateJoins.SetTextSize(9.0, 7.0);
  FCheckBoxAccurateJoins.NoTransform;

  AddControl(FCheckBoxScalePattern);

  FCheckBoxScalePattern.SetTextSize(9.0, 7.0);
  FCheckBoxScalePattern.Status := True;
  FCheckBoxScalePattern.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FPixelFormat.Free;
  FSliderStep.Free;
  FSliderWidth.Free;
  FCheckBoxTest.Free;
  FCheckBoxRotate.Free;

  FCheckBoxAccurateJoins.Free;
  FCheckBoxScalePattern.Free;
  inherited;
end;

procedure TAggApplication.DrawAntiAliasedOutline(
  Rasterizer: TAggRasterizerOutlineAA; Renderer: TAggRendererOutlineAA);
var
  S3: TSpiral;
  Rgba: TAggColor;
begin
  S3 := TSpiral.Create(Width / 5, Height - Height / 4 + 20, 5, 70, 8,
    FStartAngle);
  try
    Rgba.FromRgbaDouble(0.4, 0.3, 0.1);
    Renderer.SetColor(@Rgba);
    Rasterizer.AddPath(S3);
  finally
    S3.Free;
  end;
end;

procedure TAggApplication.DrawAntiAliasedScanLine(
  Rasterizer: TAggRasterizerScanLineAA; ScanLine: TAggCustomScanLine;
  Renderer: TAggRendererScanLineAASolid);
var
  S4: TSpiral;
  Rgba: TAggColor;
  Stroke: TAggConvStroke;
begin
  S4 := TSpiral.Create(Width * 0.5, Height - Height / 4 + 20, 5, 70, 8,
    FStartAngle);
  try
    Stroke := TAggConvStroke.Create(S4);
    try
      Stroke.Width := FSliderWidth.Value;
      Stroke.LineCap := lcRound;

      Rgba.FromRgbaDouble(0.4, 0.3, 0.1);
      Renderer.SetColor(@Rgba);

      Rasterizer.AddPath(Stroke);
      RenderScanLines(Rasterizer, ScanLine, Renderer);
    finally
      Stroke.Free;
    end;
  finally
    S4.Free;
  end;
end;

procedure TAggApplication.DrawAliasedPixAccuracy(
  RasOutline: TAggRasterizerOutline; Prim: TAggRendererPrimitives);
var
  S1: TSpiral;
  Rn: TAggTransAffine;
  Rgba : TAggColor;
  Trans: TAggConvTransform;
begin
  S1 := TSpiral.Create(Width / 5, Height / 4 + 50, 5, 70, 8, FStartAngle);
  try
    Rn := TAggTransAffine.Create(@RoundOff);
    try
      Trans := TAggConvTransform.Create(S1, Rn);
      try
        Rgba.FromRgbaDouble(0.4, 0.3, 0.1);
        Prim.LineColor := Rgba;
        RasOutline.AddPath(Trans);
      finally
        Trans.Free;
      end;
    finally
      Rn.Free;
    end;
  finally
    S1.Free;
  end;
end;

procedure TAggApplication.DrawAliasedSubpixAccuracy(
  RasOutline: TAggRasterizerOutline; Prim: TAggRendererPrimitives);
var
  S2: TSpiral;
  Rgba: TAggColor;
begin
  S2 := TSpiral.Create(Width * 0.5, Height / 4 + 50, 5, 70, 8,
    FStartAngle);
  try
    Rgba.FromRgbaDouble(0.4, 0.3, 0.1);
    Prim.LineColor := Rgba;
    RasOutline.AddPath(S2);
  finally
    S2.Free;
  end;
end;

procedure TAggApplication.DrawAntiAliasedOutlineImage(
  Rasterizer: TAggRasterizerOutlineAA; Renderer: TAggRendererOutline);
var
  S5: TSpiral;
begin
  S5 := TSpiral.Create(Width - Width / 5, Height - Height / 4 + 20,
    5, 70, 8, FStartAngle);
  try
    Rasterizer.AddPath(S5);
  finally
    S5.Free;
  end;
end;

procedure TAggApplication.Text(Rasterizer: TAggRasterizerScanLineAA;
  ScanLine: TAggCustomScanLine; Renderer: TAggRendererScanLineAASolid;
  X, Y: Double; Txt: PAnsiChar);
var
  T: TAggGsvText;
  Stroke: TAggConvStroke;
  Rgba  : TAggColor;
begin
  T := TAggGsvText.Create;
  try
    T.SetSize(8);
    T.SetText(Txt);

    T.SetStartPoint(X, Y);
    Stroke := TAggConvStroke.Create(T);
    try
      Stroke.Width := 0.7;

      Rasterizer.AddPath(Stroke);
    finally
      Stroke.Free;
    end;

    Rgba.Black;
    Renderer.SetColor(@Rgba);

    RenderScanLines(Rasterizer, ScanLine, Renderer);
  finally
    T.Free;
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RendererAA: TAggRendererScanLineAASolid;
  RendererPrimitives: TAggRendererPrimitives;
  RasAA: TAggRasterizerScanLineAA;
  RasOutline: TAggRasterizerOutline;
  ScanLine: TAggScanLinePacked8;

  Rgba: TAggColor;

  RendererOutlineAA: TAggRendererOutlineAA;
  RasOutlineAA: TAggRasterizerOutlineAA;

  Filter: TAggPatternFilterBilinearRgba;
  Source: TPatternPixelMapArgb32;
  SourceScaled: TAggLineImageScale;
  Pattern: TAggLineImagePatternPow2;

  RenImage: TAggRendererOutlineImage;
  RasterizerOutlineImage: TAggRasterizerOutlineAA;

  W : Double;
  Profile: array [0..1] of TAggLineProfileAA;
  GammaPower: TAggGammaPower;

  Renderer: TAggRendererOutlineAA;
  Rasterizer: TAggRasterizerOutlineAA;

  Patt: TAggLineImagePatternPow2;
  RendererImage: TAggRendererOutlineImage;
  RasterizerImage: TAggRasterizerOutlineAA;
begin
  RendererBase := TAggRendererBase.Create(FPixelFormat);
  try
    RendererAA := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererPrimitives := TAggRendererPrimitives.Create(RendererBase);
      try
        RasAA := TAggRasterizerScanLineAA.Create;
        try
          ScanLine := TAggScanLinePacked8.Create;
          try
            RasOutline := TAggRasterizerOutline.Create(RendererPrimitives);

            Profile[0] := TAggLineProfileAA.Create;
            Profile[0].SetWidth(FSliderWidth.Value);

            RendererOutlineAA := TAggRendererOutlineAA.Create(RendererBase,
              Profile[0]);
            RasOutlineAA := TAggRasterizerOutlineAA.Create(RendererOutlineAA);

            RasOutlineAA.AccurateJoin := FCheckBoxAccurateJoins.Status;
            RasOutlineAA.RoundCap := True;

            // Image pattern
            Filter := TAggPatternFilterBilinearRgba.Create;
            Source := TPatternPixelMapArgb32.Create(@CPixelMapChain);

            SourceScaled := TAggLineImageScale.Create(Source, FSliderWidth.Value);
            Pattern := TAggLineImagePatternPow2.Create(Filter);

            if FCheckBoxScalePattern.Status then
              Pattern.Build(SourceScaled)
            else
              Pattern.Build(Source);

            RenImage := TAggRendererOutlineImage.Create(RendererBase, Pattern);
            try
              if FCheckBoxScalePattern.Status then
                RenImage.ScaleX := FSliderWidth.Value / Source.Height;

              RasterizerOutlineImage := TAggRasterizerOutlineAA.Create(RenImage);

              // Circles
              Rgba.FromRgbaDouble(1.0, 1.0, 0.95);
              RendererBase.Clear(@Rgba);

              DrawAliasedPixAccuracy(RasOutline, RendererPrimitives);
              DrawAliasedSubpixAccuracy(RasOutline, RendererPrimitives);
              DrawAntiAliasedOutline(RasOutlineAA, RendererOutlineAA);
              DrawAntiAliasedScanLine(RasAA, ScanLine, RendererAA);
              DrawAntiAliasedOutlineImage(RasterizerOutlineImage, RenImage);

              // Text
              Text(RasAA, ScanLine, RendererAA, 50, 80, 'Bresenham lines,'#13#13
                + 'regular accuracy');
              Text(RasAA, ScanLine, RendererAA, Width * 0.5 - 50, 80,
                'Bresenham lines,'#13#13'subpixel accuracy');
              Text(RasAA, ScanLine, RendererAA, 50, Height * 0.5 + 50,
                'Anti-aliased lines');
              Text(RasAA, ScanLine, RendererAA, Width * 0.5 - 50,
                Height * 0.5 + 50, 'ScanLine Rasterizer');
              Text(RasAA, ScanLine, RendererAA, Width - Width / 5 - 50,
                Height * 0.5 + 50, 'Arbitrary Image Pattern');

              // Render the controls
              RenderControl(RasAA, ScanLine, RendererAA, FSliderStep);
              RenderControl(RasAA, ScanLine, RendererAA, FSliderWidth);
              RenderControl(RasAA, ScanLine, RendererAA, FCheckBoxTest);
              RenderControl(RasAA, ScanLine, RendererAA, FCheckBoxRotate);
              RenderControl(RasAA, ScanLine, RendererAA, FCheckBoxAccurateJoins);
              RenderControl(RasAA, ScanLine, RendererAA, FCheckBoxScalePattern);

              // An example of using anti-aliased outline Rasterizer.
              {$IFDEF AntiAliasedOutlineRenderer}
              W := 5.0 + FSliderWidth.Value - 3.0;

              Profile[1] := TAggLineProfileAA.Create;
              GammaPower := TAggGammaPower.Create(1.2);
              try
                Profile[1].SetGamma(GammaPower);
              finally
                GammaPower.Free;
              end;


              Profile[1].MinWidth := 0.75;
              Profile[1].SmootherWidth := 3.0;
              Profile[1].SetWidth(W);

              CPixelFormat(Pixf, RenderingBufferWindow);

              RendererBase := TAggRendererBase.Create(Pixf, True);
              Renderer.Create(RendererBase, Profile[1]);

              Rgba.CreateInt(0 ,0 ,0 );
              Renderer.SetColor(@rgba);

              Rasterizer := TAggRasterizerOutlineAA.Create(@Renderer);
              Rasterizer.RoundCap := True;
              Rasterizer.AccurateJoin := True;

              Rasterizer.MoveToDouble(100 ,100 );
              Rasterizer.LineToDouble(150 ,200 );
              Rasterizer.Render(False); // false means "don't close the polygon", i.e. polyline

              Rasterizer.Free;
              Profile[1].Free;
              {$ENDIF}

              // An example of using image pattern outline Rasterizer
              // Uncomment it to see the result
              { fltr.Create;                      // Filtering functor

                patt_src.Create(@CPixelMapChain );  // Source. Must have an interface:
                // width() const
                // height() const
                // pixel(int x, int y) const
                // Any TAggRendererBase or derived
                // is good for the use as a source.

                // agg::line_image_pattern is the main container for the patterns. It creates
                // a copy of the patterns extended according to the needs of the filter.
                // agg::line_image_pattern can operate with arbitrary image width, but if the
                // width of the pattern is power of 2, it's better to use the modified
                // version TAggLineImagePatternPow2 because it works about 15-25 percent
                // faster than agg::line_image_pattern (because of using simple masking instead
                // of expensive '%' operation).
                Patt := TAggLineImagePatternPow2.Create(@fltr ,@Source );

                pixfmt(pixf ,RenderingBufferWindow );

                RendererBase.Create(@pixf );
                RendererImage.Create(RendererBase, Patt);
                //RendererImage._scale_x   (1.3 );            // Optional

                RasterizerImage.Create(@RendererImage );
                RasterizerImage.MoveToDouble(100 ,150 );
                RasterizerImage.LineToDouble(0 ,0 );
                RasterizerImage.LineToDouble(300 ,200 );
                RasterizerImage.render   (false );

                Patt.Free;
                RasterizerImage.Free;{ }
            finally
              RenImage.Free;
            end;

            // Free AGG resources
            Profile[0].Free;
            RasOutlineAA.Free;

            RendererOutlineAA.Free;
            Pattern.Free;
            RasterizerOutlineImage.Free;
            SourceScaled.Free;
            Source.Free;

            RasOutline.Free;
            Filter.Free;
          finally
            ScanLine.Free;
          end;
        finally
          RasAA.Free;
        end;
      finally
        RendererPrimitives.Free;
      end;
    finally
      RendererAA.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnIdle;
begin
  FStartAngle := FStartAngle + Deg2Rad(FSliderStep.Value);

  if FStartAngle > Deg2Rad(360.0) then
    FStartAngle := FStartAngle - Deg2Rad(360.0);

  ForceRedraw;
end;

procedure TAggApplication.OnControlChange;
var
  I: Cardinal;
  T2, T3, T4, T5: Double;
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RendererAA : TAggRendererScanLineAASolid;
  RendererPrimitives: TAggRendererPrimitives;
  RasAA : TAggRasterizerScanLineAA;
  RasOutline: TAggRasterizerOutline;
  ScanLine: TAggScanLinePacked8;

  Rgba: TAggColor;

  Profile: TAggLineProfileAA;
  RendererOutlineAA: TAggRendererOutlineAA;
  RasOutlineAA: TAggRasterizerOutlineAA;

  Filter: TAggPatternFilterBilinearRgba;
  Source: TPatternPixelMapArgb32;
  SourceScaled: TAggLineImageScale;
  Pattern: TAggLineImagePatternPow2;

  RenImage: TAggRendererOutlineImage;
  RasImage: TAggRasterizerOutlineAA;
begin
  WaitMode := not FCheckBoxRotate.Status;

  if FCheckBoxTest.Status then
  begin
    OnDraw;
    UpdateWindow;

    // Initialize structures
    CPixelFormat(Pixf, RenderingBufferWindow);

    RendererBase := TAggRendererBase.Create(Pixf, True);
    try
      RendererAA := TAggRendererScanLineAASolid.Create(RendererBase);
      try
        RendererPrimitives := TAggRendererPrimitives.Create(RendererBase);
        try
          RasAA := TAggRasterizerScanLineAA.Create;
          try
            ScanLine := TAggScanLinePacked8.Create;
            try
              RasOutline := TAggRasterizerOutline.Create(RendererPrimitives);
              try
                Profile := TAggLineProfileAA.Create;
                Profile.SetWidth(FSliderWidth.Value);

                RendererOutlineAA := TAggRendererOutlineAA.Create(RendererBase,
                  Profile);
                RasOutlineAA := TAggRasterizerOutlineAA.Create(RendererOutlineAA);

                RasOutlineAA.AccurateJoin := FCheckBoxAccurateJoins.Status;
                RasOutlineAA.RoundCap := True;

                // Image pattern
                Filter := TAggPatternFilterBilinearRgba.Create;
                Source := TPatternPixelMapArgb32.Create(@CPixelMapChain);

                SourceScaled := TAggLineImageScale.Create(Source,
                  FSliderWidth.Value);
                Pattern := TAggLineImagePatternPow2.Create(Filter);

                if FCheckBoxScalePattern.Status then
                  Pattern.Build(SourceScaled)
                else
                  Pattern.Build(Source);

                RenImage := TAggRendererOutlineImage.Create(RendererBase, Pattern);

                if FCheckBoxScalePattern.Status then
                  RenImage.ScaleX := Source.Height / FSliderWidth.Value;

                RasImage := TAggRasterizerOutlineAA.Create(RenImage);

                // Do Test
                StartTimer;
                for I := 1 to 200 do
                begin
                  DrawAliasedSubpixAccuracy(RasOutline, RendererPrimitives);

                  FStartAngle := FStartAngle + Deg2Rad(FSliderStep.Value);
                end;
                T2 := GetElapsedTime;

                StartTimer;
                for I := 1 to 200 do
                begin
                  DrawAntiAliasedOutline(RasOutlineAA, RendererOutlineAA);

                  FStartAngle := FStartAngle + Deg2Rad(FSliderStep.Value);
                end;
                T3 := GetElapsedTime;

                StartTimer;
                for I := 1 to 200 do
                begin
                  DrawAntiAliasedScanLine(RasAA, ScanLine, RendererAA);

                  FStartAngle := FStartAngle + Deg2Rad(FSliderStep.Value);
                end;
                T4 := GetElapsedTime;

                StartTimer;
                for I := 1 to 200 do
                begin
                  DrawAntiAliasedOutlineImage(RasImage, RenImage);

                  FStartAngle := FStartAngle + Deg2Rad(FSliderStep.Value);
                end;
                T5 := GetElapsedTime;

                // Display results
                FCheckBoxTest.Status := False;
                ForceRedraw;

                DisplayMessage(Format('Aliased=%1.2fms, Anti-Aliased=%1.2fms, '
                  + 'ScanLine=%1.2fms, Image-Pattern=%1.2fms'#0, [T2, T3, T4,
                  T5]));

                // Free AGG resources

                Profile.Free;
                RenImage.Free;
                RasOutlineAA.Free;
                RendererOutlineAA.Free;

                Pattern.Free;
                RasImage.Free;
                Filter.Free;
                SourceScaled.Free;
                Source.Free;
              finally
                RasOutline .Free;
              end;
            finally
              ScanLine.Free;
            end;
          finally
            RasAA.Free;
          end;
        finally
          RendererPrimitives.Free;
        end;
      finally
        RendererAA.Free;
      end;
    finally
      RendererBase.Free;
    end;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('More complex example demostrating different Rasterizers. '
      + 'Here you can see how the outline Rasterizer works, and how to use an '
      + 'image as the line pattern. This capability can be very useful to draw '
      + 'geographical maps.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Line Join (F1-Help)';

    if Init(500, 450, []) then
      Run;
  finally
    Free;
  end;
end.
