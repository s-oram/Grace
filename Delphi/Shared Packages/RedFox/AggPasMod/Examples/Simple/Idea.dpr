program Idea;

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
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas'
{$I Pixel_Formats.inc}

const
  CFlipY = False;

type
  TPathAttributes = class
  private
    FIndex: Cardinal;

    FFillColor, FStrokeColor: TAggColor;
    FStrokeWidth: Double;
  public
    constructor Create; overload;
    constructor Create(Index: Cardinal; Fill, Stroke: PAggColor;
      Width: Double); overload;
  end;

const
  GPolyBulb: array [0..39] of Double = (-6, -67, -6, -71, -7, -74, -8, -76,
    -10, -79, -10, -82, -9, -84, -6, -86, -4, -87, -2, -86, -1, -86, 1, -84, 2,
    -82, 2, -79, 0, -77, -2, -73, -2, -71, -2, -69, -3, -67, -4, -65);

  GPolyBeam1: array [0..9] of Double = (-14, -84, -22, -85, -23, -87, -22,
    -88, -21, -88);

  GPolyBeam2: array [0..9] of Double = (-10, -92, -14, -96, -14, -98, -12,
    -99, -11, -97);

  GPolyBeam3: array [0..9] of Double = (-1, -92, -2, -98, 0, -100, 2,
    -100, 1, -98);

  GPolyBeam4: array [0..9] of Double = (5, -89, 11, -94, 13, -93, 13,
    -92, 12, -91);

  GPolyFigure1: array [0..41] of Double = (1, -48, -3, -54, -7, -58, -12, -58,
    -17, -55, -20, -52, -21, -47, -20, -40, -17, -33, -11, -28, -6, -26, -2,
    -25, 2, -26, 4, -28, 5, -33, 5, -39, 3, -44, 12, -48, 12, -50, 12,
    -51, 3, -46);

  GPolyFigure2: array [0..75] of Double = (11, -27, 6, -23, 4, -22, 3, -19, 5,
    -16, 6, -15, 11, -17, 19, -23, 25, -30, 32, -38, 32, -41, 32, -50, 30, -64,
    32, -72, 32, -75, 31, -77, 28, -78, 26, -80, 28, -87, 27, -89, 25, -88, 24,
    -79, 24, -76, 23, -75, 20, -76, 17, -76, 17, -74, 19, -73, 22, -73, 24, -71,
    26, -69, 27, -64, 28, -55, 28, -47, 28, -40, 26, -38, 20, -33, 14, -30);

  GPolyFigure3: array [0..69] of Double = (-6, -20, -9, -21, -15, -21, -20,
    -17, -28, -8, -32, -1, -32, 1, -30, 6, -26, 8, -20, 10, -16, 12, -14, 14,
    -15, 16, -18, 20, -22, 20, -25, 19, -27, 20, -26, 22, -23, 23, -18, 23, -14,
    22, -11, 20, -10, 17, -9, 14, -11, 11, -16, 9, -22, 8, -26, 5, -28, 2, -27,
    -2, -23, -8, -19, -11, -12, -14, -6, -15, -6, -18);

  GPolyFigure4: array [0..39] of Double = (11, -6, 8, -16, 5, -21, -1, -23, -7,
    -22, -10, -17, -9, -10, -8, 0, -8, 10, -10, 18, -11, 22, -10, 26, -7, 28,
    -3, 30, 0, 31, 5, 31, 10, 27, 14, 18, 14, 11, 11, 2);

  GPolyFigure5: array [0..55] of Double = (0, 22, -5, 21, -8, 22, -9, 26, -8,
    49, -8, 54, -10, 64, -10, 75, -9, 81, -10, 84, -16, 89, -18, 95, -18, 97,
    -13, 100, -12, 99, -12, 95, -10, 90, -8, 87, -6, 86, -4, 83, -3, 82, -5, 80,
    -6, 79, -7, 74, -6, 63, -3, 52, 0, 42, 1, 31);

  GPolyFigure6: array [0..61] of Double = (12, 31, 12, 24, 8, 21, 3, 21, 2, 24,
    3, 30, 5, 40, 8, 47, 10, 56, 11, 64, 11, 71, 10, 76, 8, 77, 8, 79, 10, 81,
    13, 82, 17, 82, 26, 84, 28, 87, 32, 86, 33, 81, 32, 80, 25, 79, 17, 79, 14,
    79, 13, 76, 14, 72, 14, 64, 13, 55, 12, 44, 12, 34);

var
  GPathCount: Cardinal;
  GFillingRule: TAggFillingRule;
  GAngle: Double;

  GAttributes: array [0..2] of TPathAttributes;
  GPath: TAggPathStorage;

  GRasterizer: TAggRasterizerScanLineAA;
  GScanLine: TAggScanLinePacked8;

  // AGG_POLY_SIZE: SizeOf(p ) / (SizeOf(double ) * 2

type
  TAggApplication = class(TPlatformSupport)
  private
    FDelta: TPointDouble;

    FCheckBoxRotate, FCheckBoxEvenOdd: TAggControlCheckBox;
    FCheckBoxDraft, FCheckBoxRoundOff: TAggControlCheckBox;

    FSliderAngleDelta: TAggControlSlider;
    FRedrawFlag: Boolean;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnResize(Width, Height: Integer); override;
    procedure OnDraw; override;

    procedure OnIdle; override;
    procedure OnControlChange; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;

procedure TransRoundOff(This: TAggTransAffine; X, Y: PDouble);
begin
  X^ := Floor(X^ + 0.5);
  Y^ := Floor(Y^ + 0.5);
end;


{ TPathAttributes }

constructor TPathAttributes.Create;
begin
  FIndex := 0;
  FStrokeWidth := 0;
end;

constructor TPathAttributes.Create(Index: Cardinal; Fill, Stroke: PAggColor;
  Width: Double);
begin
  FIndex := Index;
  FFillColor := Fill^;
  FStrokeColor := Stroke^;
  FStrokeWidth := Width;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  Rgbs, Rgbf: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  // Controls
  FCheckBoxRotate := TAggControlCheckBox.Create(10, 3, 'Rotate', not FlipY);
  FCheckBoxEvenOdd := TAggControlCheckBox.Create(60, 3, 'Even-Odd', not FlipY);
  FCheckBoxDraft := TAggControlCheckBox.Create(130, 3, 'Draft', not FlipY);
  FCheckBoxRoundOff := TAggControlCheckBox.Create(175, 3, 'RoundOff',
     not FlipY);

  FSliderAngleDelta := TAggControlSlider.Create(10, 21, 250 - 10, 27,
    not FlipY);
  FSliderAngleDelta.Caption := 'Step=%4.3f degree';

  FRedrawFlag := True;

  FCheckBoxRotate.SetTextSize(7);
  FCheckBoxEvenOdd.SetTextSize(7);
  FCheckBoxDraft.SetTextSize(7);
  FCheckBoxRoundOff.SetTextSize(7);

  AddControl(FCheckBoxRotate);
  AddControl(FCheckBoxEvenOdd);
  AddControl(FCheckBoxDraft);
  AddControl(FCheckBoxRoundOff);
  AddControl(FSliderAngleDelta);

  FSliderAngleDelta.Value := 0.01;

  // Polygon
  Rgbf.FromRgbaInteger(255, 255, 0);
  Rgbs.Black;

  GAttributes[GPathCount] := TPathAttributes.Create(GPath.StartNewPath, @Rgbf,
    @Rgbs, 1.0);

  Inc(GPathCount);

  GPath.AddPoly(@GPolyBulb[0], SizeOf(GPolyBulb) div (SizeOf(Double) * 2),
    False, CAggPathFlagsClose);

  Rgbf.FromRgbaInteger(255, 255, 200);
  Rgbs.FromRgbaInteger(90, 0, 0);

  GAttributes[GPathCount] := TPathAttributes.Create(GPath.StartNewPath, @Rgbf,
    @Rgbs, 0.7);

  Inc(GPathCount);

  GPath.AddPoly(@GPolyBeam1[0], SizeOf(GPolyBeam1)
    div (SizeOf(Double) * 2), False, CAggPathFlagsClose);
  GPath.AddPoly(@GPolyBeam2[0], SizeOf(GPolyBeam2)
    div (SizeOf(Double) * 2), False, CAggPathFlagsClose);
  GPath.AddPoly(@GPolyBeam3[0], SizeOf(GPolyBeam3)
    div (SizeOf(Double) * 2), False, CAggPathFlagsClose);
  GPath.AddPoly(@GPolyBeam4[0], SizeOf(GPolyBeam4)
    div (SizeOf(Double) * 2), False, CAggPathFlagsClose);

  Rgbf.Black;
  Rgbs.Black;

  GAttributes[GPathCount] := TPathAttributes.Create(GPath.StartNewPath, @Rgbf,
    @Rgbs, 0.0);

  Inc(GPathCount);

  GPath.AddPoly(@GPolyFigure1[0], SizeOf(GPolyFigure1)
    div (SizeOf(Double) * 2));
  GPath.AddPoly(@GPolyFigure2[0], SizeOf(GPolyFigure2)
    div (SizeOf(Double) * 2));
  GPath.AddPoly(@GPolyFigure3[0], SizeOf(GPolyFigure3)
    div (SizeOf(Double) * 2));
  GPath.AddPoly(@GPolyFigure4[0], SizeOf(GPolyFigure4)
    div (SizeOf(Double) * 2));
  GPath.AddPoly(@GPolyFigure5[0], SizeOf(GPolyFigure5)
    div (SizeOf(Double) * 2));
  GPath.AddPoly(@GPolyFigure6[0], SizeOf(GPolyFigure6)
    div (SizeOf(Double) * 2));
end;

destructor TAggApplication.Destroy;
begin
  FCheckBoxRotate.Free;
  FCheckBoxEvenOdd.Free;
  FCheckBoxDraft.Free;
  FCheckBoxRoundOff.Free;

  FSliderAngleDelta.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  FDelta.X := RenderingBufferWindow.Width;
  FDelta.Y := RenderingBufferWindow.Height;
end;

procedure TAggApplication.OnResize;
begin
  FRedrawFlag := True;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;
  Rb: TAggRendererScanLineBinSolid;

  Rgba: TAggColor;

  GammaNo: TAggGammaNone;
  GammaThrs: TAggGammaThreshold;

  Mtx: TAggTransAffine;

  RoundOff: TAggTransAffine;

  Fill, FillRoundOff: TAggConvTransform;

  Stroke, StrokeRoundOff: TAggConvStroke;

  I: Cardinal;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    Rb := TAggRendererScanLineBinSolid.Create(RendererBase);

    RoundOff := TAggTransAffine.Create(@TransRoundOff);

    // Render the controls
    if FRedrawFlag then
    begin
      GammaNo := TAggGammaNone.Create;
      try
        GRasterizer.Gamma(GammaNo);
      finally
        GammaNo.Free;
      end;

      RendererBase.Clear(CRgba8White);

      GRasterizer.FillingRule := frNonZero;

      RenderControl(GRasterizer, GScanLine, RenScan, FCheckBoxRotate);
      RenderControl(GRasterizer, GScanLine, RenScan, FCheckBoxEvenOdd);
      RenderControl(GRasterizer, GScanLine, RenScan, FCheckBoxDraft);
      RenderControl(GRasterizer, GScanLine, RenScan, FCheckBoxRoundOff);
      RenderControl(GRasterizer, GScanLine, RenScan, FSliderAngleDelta);

      FRedrawFlag := False;
    end
    else
    begin
      Rgba.White;
      RendererBase.CopyBar(0, Trunc(32.0 * RenderingBufferWindow.Height /
        FDelta.Y), RenderingBufferWindow.Width, RenderingBufferWindow.Height,
        @Rgba);
    end;

    // Draft mode
    if FCheckBoxDraft.Status then
    begin
      GammaThrs := TAggGammaThreshold.Create(0.4);
      try
        GRasterizer.Gamma(GammaThrs);
      finally
        GammaThrs.Free;
      end;
    end;

    // Rotate polygon
    Mtx := TAggTransAffine.Create;
    Mtx.Reset;

    Mtx.Rotate(Deg2Rad(GAngle));
    Mtx.Translate(FDelta.X * 0.5, FDelta.Y * 0.5 + 10);
    Mtx.Scale(RenderingBufferWindow.Width / FDelta.X,
      RenderingBufferWindow.Height / FDelta.Y);

    Fill := TAggConvTransform.Create(GPath, Mtx);
    FillRoundOff := TAggConvTransform.Create(Fill, RoundOff);

    Stroke := TAggConvStroke.Create(Fill);
    StrokeRoundOff := TAggConvStroke.Create(FillRoundOff);

    if FCheckBoxEvenOdd.Status then
      GFillingRule := frEvenOdd
    else
      GFillingRule := frNonZero;

    // Render polygon
    for I := 0 to GPathCount - 1 do
    begin
      GRasterizer.FillingRule := GFillingRule;

      RenScan.SetColor(@GAttributes[I].FFillColor);
      Rb.SetColor(@GAttributes[I].FFillColor);

      if FCheckBoxRoundOff.Status then
        GRasterizer.AddPath(FillRoundOff, GAttributes[I].FIndex)
      else
        GRasterizer.AddPath(Fill, GAttributes[I].FIndex);

      if FCheckBoxDraft.Status then
        RenderScanLines(GRasterizer, GScanLine, Rb)
      else
        RenderScanLines(GRasterizer, GScanLine, RenScan);

      if GAttributes[I].FStrokeWidth > 0.001 then
      begin
        RenScan.SetColor(@GAttributes[I].FStrokeColor);
        Rb.SetColor(@GAttributes[I].FStrokeColor);

        Stroke.Width := GAttributes[I].FStrokeWidth * Mtx.GetScale;
        StrokeRoundOff.Width := GAttributes[I].FStrokeWidth * Mtx.GetScale;

        if FCheckBoxRoundOff.Status then
          GRasterizer.AddPath(StrokeRoundOff, GAttributes[I].FIndex)
        else
          GRasterizer.AddPath(Stroke, GAttributes[I].FIndex);

        if FCheckBoxDraft.Status then
          RenderScanLines(GRasterizer, GScanLine, Rb)
        else
          RenderScanLines(GRasterizer, GScanLine, RenScan);
      end;
    end;

    // Free AGG resources
    FillRoundOff.Free;
    Fill.Free;
    Mtx.Free;
    Rb.Free;
    RoundOff.Free;
    Stroke.Free;
    StrokeRoundOff.Free;
    RenScan.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnIdle;
begin
  GAngle := GAngle + FSliderAngleDelta.Value;

  if GAngle > 360.0 then
    GAngle := GAngle - 360.0;

  ForceRedraw;
end;

procedure TAggApplication.OnControlChange;
begin
  WaitMode := not FCheckBoxRotate.Status;

  FRedrawFlag := True;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('The polygons for this "idea" were taken from the book '
      + '"Dynamic HTML in Action" by Eric Schurman. An example of using '
      + 'Microsoft Direct Animation can be found here: '
      + '"http://www.antigrain.com/demo/ideaDA.html." If you use Microsoft '
      + 'Internet Explorer you can compare the quality of rendering in AGG '
      + 'and Microsoft Direct Animation.'#13
      + 'Note that even when you click "Rotate with High Quality", you will '
      + 'see it "jitters". It''s because there are actually no Subpixel '
      + 'Accuracy used in the Microsoft Direct Animation.In the AGG example, '
      + 'there''s no jitter even in the "Draft" (Low quality) mode. You can '
      + 'see the simulated jittering if you turn on the "RoundOff" mode, in '
      + 'which there integer pixel coordinated are used. As for the '
      + 'performance, note, that the image in AGG is rotated with step of '
      + '0.01 degree (initially), while in the Direct Animation Example the '
      + 'angle step is 0.1 degree.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  GPathCount := 0;
  GFillingRule := frNonZero;
  GAngle := 0.0;

  GPath := TAggPathStorage.Create;
  GRasterizer := TAggRasterizerScanLineAA.Create;
  GScanLine := TAggScanLinePacked8.Create;

  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Idea (F1-Help)';

    if Init(250, 280, [wfResize]) then
      Run;
  finally
    Free;
  end;

  GAttributes[0].Free;
  GAttributes[1].Free;
  GAttributes[2].Free;
  GRasterizer.Free;
  GScanLine.Free;
  GPath.Free;
end.
