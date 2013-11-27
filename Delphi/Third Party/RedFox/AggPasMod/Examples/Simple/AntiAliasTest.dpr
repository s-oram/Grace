program AntiAliasTest;

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

  AggBasics in '..\..\Source\AggBasics.pas',
  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggQuadratureOscillator in '..\..\Source\AggQuadratureOscillator.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvDash in '..\..\Source\AggConvDash.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanGouraudRgba in '..\..\Source\AggSpanGouraudRgba.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggArray in '..\..\Source\AggArray.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas';

const
  CFlipY = False;

type
  TSimpleVertexSource = class(TAggVertexSource)
  private
    FNumVertices, FCount: Cardinal;

    FX, FY: array [0..7] of Double;
    FCmd: array [0..7] of Cardinal;
  public
    constructor Create; overload;
    constructor Create(X1, Y1, X2, Y2: Double); overload;
    constructor Create(X1, Y1, X2, Y2, X3, Y3: Double); overload;

    procedure Init(X1, Y1, X2, Y2: Double); overload;
    procedure Init(X1, Y1, X2, Y2, X3, Y3: Double); overload;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TDashedLine = class
  private
    FRas: TAggRasterizerScanLineAA;
    FRen: TAggRendererScanLineAASolid;
    FScanLine: TAggCustomScanLine;
    FSource: TSimpleVertexSource;
    FDash: TAggConvDash;

    FStroke, FDashStroke: TAggConvStroke;
  public
    constructor Create(Ras: TAggRasterizerScanLineAA;
      Ren: TAggRendererScanLineAASolid; Sl: TAggCustomScanLine);
    destructor Destroy; override;

    procedure Draw(X1, Y1, X2, Y2, LineWidth, DashLength: Double);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FGamma: TAggGammaLut8;
    FPixelFormat: TAggPixelFormatProcessor;
    FRendererBase: TAggRendererBase;
    FRenScan: TAggRendererScanLineAASolid;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;

    FGradientMatrix: TAggTransAffine;
    FGradientX: TAggGradientX;
    FSpanAllocator: TAggSpanAllocator;
    FSpanInterpolator: TAggSpanInterpolatorLinear;
    FDashGradient: TDashedLine;

    FCircle: TAggCircle;
    FSliderGamma: TAggControlSlider;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;
    procedure OnResize(Width: Integer; Height: Integer); override;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;

// Calculate the affine transformation matrix for the linear Gradient
// from (x1, y1) to (x2, y2). GradientD2 is the "base" to scale the
// Gradient. Here d1 must be 0.0, and d2 must equal GradientD2.
procedure CalculatLinearGradientTransform(X1, Y1, X2, Y2: Double;
  Mtx: TAggTransAffine; InvGradientD2: Double = 0.01);
var
  Delta: TPointDouble;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;

  Mtx.Reset;
  Mtx.Scale(Hypot(Delta.X, Delta.Y) * InvGradientD2);
  Mtx.Rotate(ArcTan2(Delta.Y, Delta.X));
  Mtx.Translate(X1 + 0.5, Y1 + 0.5);
  Mtx.Invert;
end;

// A simple function to form the Gradient color array
// consisting of 3 colors, "begin", "middle", "end"
procedure FillColorArray(ColorArray: TAggPodAutoArray;
  ColorStart, ColorEnd: PAggColor); overload;
var
  I: Cardinal;
const
  CScale: Double = 1 / 255;
begin
  for I := 0 to 255 do
    PAggColor(ColorArray[I])^ := Gradient(ColorStart^, ColorEnd^, I * CScale);
end;

procedure FillColorArray(ColorArray: TAggPodAutoArray;
  ColorStart, ColorEnd: TAggRgba8); overload;
var
  I: Cardinal;
  C: array [0..1] of TAggColor;
const
  CScale: Double = 1 / 255;
begin
  for I := 0 to 255 do
  begin
    C[0].Rgba8 := ColorStart;
    C[1].Rgba8 := ColorEnd;
    PAggColor(ColorArray[I])^ := Gradient(C[0], C[1], I * CScale);
  end;
end;


{ TSimpleVertexSource }

constructor TSimpleVertexSource.Create;
begin
  FNumVertices := 0;
  FCount := 0;

  FCmd[0] := CAggPathCmdStop;
end;

constructor TSimpleVertexSource.Create(X1, Y1, X2, Y2: Double);
begin
  Init(X1, Y1, X2, Y2);
end;

constructor TSimpleVertexSource.Create(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  Init(X1, Y1, X2, Y2, X3, Y3);
end;

procedure TSimpleVertexSource.Init(X1, Y1, X2, Y2: Double);
begin
  FNumVertices := 2;
  FCount := 0;

  FX[0] := X1;
  FY[0] := Y1;
  FX[1] := X2;
  FY[1] := Y2;

  FCmd[0] := CAggPathCmdMoveTo;
  FCmd[1] := CAggPathCmdLineTo;
  FCmd[2] := CAggPathCmdStop;
end;

procedure TSimpleVertexSource.Init(X1, Y1, X2, Y2, X3, Y3: Double);
begin
  FNumVertices := 3;
  FCount := 0;

  FX[0] := X1;
  FY[0] := Y1;
  FX[1] := X2;
  FY[1] := Y2;
  FX[2] := X3;
  FY[2] := Y3;
  FX[3] := 0;
  FY[3] := 0;
  FX[4] := 0;
  FY[4] := 0;

  FCmd[0] := CAggPathCmdMoveTo;
  FCmd[1] := CAggPathCmdLineTo;
  FCmd[2] := CAggPathCmdLineTo;
  FCmd[3] := CAggPathCmdEndPoly or CAggPathFlagsClose;
  FCmd[4] := CAggPathCmdStop;
end;

procedure TSimpleVertexSource.Rewind(PathID: Cardinal);
begin
  FCount := 0;
end;

function TSimpleVertexSource.Vertex(X, Y: PDouble): Cardinal;
begin
  X^ := FX[FCount];
  Y^ := FY[FCount];

  Result := FCmd[FCount];

  Inc(FCount);
end;


{ DashedLine }

constructor TDashedLine.Create(Ras: TAggRasterizerScanLineAA;
  Ren: TAggRendererScanLineAASolid; Sl: TAggCustomScanLine);
begin
  inherited Create;

  FRas := Ras;
  FRen := Ren;
  FScanLine := Sl;

  FSource := TSimpleVertexSource.Create;
  FDash := TAggConvDash.Create(FSource);
  FStroke := TAggConvStroke.Create(FSource);
  FDashStroke := TAggConvStroke.Create(FDash);
end;

destructor TDashedLine.Destroy;
begin
  FDash.Free;
  FSource.Free;
  FStroke.Free;
  FDashStroke.Free;
  inherited;
end;

procedure TDashedLine.Draw(X1, Y1, X2, Y2, LineWidth, DashLength: Double);
begin
  FSource.Init(X1 + 0.5, Y1 + 0.5, X2 + 0.5, Y2 + 0.5);
  FRas.Reset;

  if DashLength > 0.0 then
  begin
    FDash.RemoveAllDashes;
    FDash.AddDash(DashLength, DashLength);

    FDashStroke.Width := LineWidth;
    FDashStroke.LineCap := lcRound;

    FRas.AddPath(FDashStroke);
  end
  else
  begin
    FStroke.Width := LineWidth;
    FStroke.LineCap := lcRound;

    FRas.AddPath(FStroke);
  end;

  RenderScanLines(FRas, FScanLine, FRen);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FGamma := TAggGammaLut8.Create(1.0);
  PixelFormatBgr24Gamma(FPixelFormat, RenderingBufferWindow, FGamma);
  FRendererBase := TAggRendererBase.Create(FPixelFormat);
  FRenScan := TAggRendererScanLineAASolid.Create(FRendererBase);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;

  FGradientX := TAggGradientX.Create;
  FGradientMatrix := TAggTransAffine.Create;
  FSpanAllocator := TAggSpanAllocator.Create;
  FSpanInterpolator := TAggSpanInterpolatorLinear.Create(FGradientMatrix);
  FDashGradient := TDashedLine.Create(FRasterizer, FRenScan, FScanLine);

  FCircle := TAggCircle.Create;
  FSliderGamma := TAggControlSlider.Create(3, 3, 477, 8, not FlipY);
  FSliderGamma.SetRange(0.1, 3.0);
  FSliderGamma.Value := 1.6;
  FSliderGamma.Caption := 'Gamma=%4.3f';
  AddControl(FSliderGamma);
end;

destructor TAggApplication.Destroy;
begin
  FSliderGamma.Free;
  FCircle.Free;

  FDashGradient.Free;
  FSpanInterpolator.Free;
  FSpanAllocator.Free;
  FGradientMatrix.Free;
  FGradientX.Free;

  FScanLine.Free;
  FRasterizer.Free;

  FRenScan.Free;
  FRendererBase.Free;
  FPixelFormat.Free;
  FGamma.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Rgba, Rgbb: TAggColor;
  GammaPower: TAggGammaPower;

  I: Integer;

  Dash: TDashedLine;

  Center: TPointDouble;
  Cm, X1, Y1, X2, Y2: Double;

  GradientColors: TAggPodAutoArray; // The Gradient colors

  SpanGradient: TAggSpanGradient;
  RenGradient: TAggRendererScanLineAA;
begin
  // clear buffer
  FRendererBase.Clear(CRgba8Black);

  // Gamma correction
  FGamma.Gamma := FSliderGamma.Value;

  // radial line test
  Dash := TDashedLine.Create(FRasterizer, FRenScan, FScanLine);

  Center.X := 0.5 * Width;
  Center.Y := 0.5 * Height;

  Rgba.FromRgbaDouble(1, 1, 1, 0.2);
  FRenScan.SetColor(@Rgba);

  with TAggQuadratureOscillator.Create(Deg2Rad(2)) do
  try
    Cm := Min(Center.X, Center.Y);
    for I := 180 downto 1 do
    begin
      if I < 90 then
        Dash.Draw(Center.X + Cm * Sine, Center.Y + Cm * Cosine, Center.X,
          Center.Y, 1, I)
      else
        Dash.Draw(Center.X + Cm * Sine, Center.Y + Cm * Cosine, Center.X,
          Center.Y, 1, 0);
      Next;
    end;
  finally
    Free;
  end;

  // Initialize Gradients
  FGradientMatrix.Reset;
  GradientColors := TAggPodAutoArray.Create(256, SizeOf(TAggColor));
  SpanGradient := TAggSpanGradient.Create(FSpanAllocator, FSpanInterpolator,
    FGradientX, GradientColors, 0, 100);

  RenGradient := TAggRendererScanLineAA.Create(FRendererBase, SpanGradient);

  // Top patterns
  for I := 1 to 20 do
  begin
    FRenScan.SetColor(CRgba8White);

    // integral point sizes 1..20
    FCircle.Initialize(PointDouble(20.5 + I * (I + 1), 20.5), 0.5 * I, 8 + I);

    FRasterizer.Reset;
    FRasterizer.AddPath(FCircle);

    RenderScanLines(FRasterizer, FScanLine, FRenScan);

    // fractional point sizes 0..2
    FCircle.Initialize(PointDouble(18.5 + 4 * I, 33.5), 0.05 * I, 8);

    FRasterizer.Reset;
    FRasterizer.AddPath(FCircle);

    RenderScanLines(FRasterizer, FScanLine, FRenScan);

    // fractional point positioning
    FCircle.Initialize(PointDouble(18.4 + 4.1 * I, 27.4 + 0.1 * I), 0.5, 8);

    FRasterizer.Reset;
    FRasterizer.AddPath(FCircle);

    RenderScanLines(FRasterizer, FScanLine, FRenScan);

    // integral line widths 1..20
    Rgba.White;
    Rgbb.FromRgbaDouble(I mod 2, (I mod 3) * 0.5, (I mod 5) * 0.25);
    FillColorArray(GradientColors, @Rgba, @Rgbb);

    X1 := 20 + I * (I + 1);
    Y1 := 40.5;
    X2 := 20 + I * (I + 1) + (I - 1) * 4;
    Y2 := 100.5;

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, I, 0);

    Rgba.FromRgbaInteger($FF, 0, 0, $FF);
    Rgbb.FromRgbaInteger(0, 0, $FF, $FF);
    FillColorArray(GradientColors, @Rgba, @Rgbb);

    // fractional line lengths H (red/blue)
    X1 := 17.5 + 4 * I;
    Y1 := 107;
    X2 := 17.5 + 4.15 * I;
    Y2 := 107;

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, 1, 0);

    // fractional line lengths V (red/blue)
    X1 := 18 + 4 * I;
    Y1 := 112.5;
    X2 := 18 + 4 * I;
    Y2 := 112.5 + I * 0.15;

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, 1.0, 0);

    // fractional line positioning (red)
    Rgba.FromRgbaInteger($FF, 0, 0, $FF);
    Rgbb.White;
    FillColorArray(GradientColors, @Rgba, @Rgbb);

    X1 := 21.5;
    Y1 := 120 + (I - 1) * 3.1;
    X2 := 52.5;
    Y2 := 120 + (I - 1) * 3.1;

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, 1.0, 0);

    // fractional line width 2..0 (green)
    Rgba.FromRgbaInteger(0, $FF, 0, $FF);
    Rgbb.White;
    FillColorArray(GradientColors, @Rgba, @Rgbb);

    X1 := 52.5;
    Y1 := 118 + I * 3;
    X2 := 83.5;
    Y2 := 118 + I * 3;

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, 2.0 - (I - 1) * 0.1, 0);

    // stippled fractional width 2..0 (blue)
    Rgba.FromRgbaInteger(0, 0, $FF, $FF);
    Rgbb.White;
    FillColorArray(GradientColors, @Rgba, @Rgbb);

    X1 := 83.5;
    Y1 := 119 + I * 3;
    X2 := 114.5;
    Y2 := 119 + I * 3;

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, 2.0 - (I - 1) * 0.1, 3.0);

    // integral line width, horz aligned (mipmap test)
    FRenScan.SetColor(CRgba8White);

    if I <= 10 then
      Dash.Draw(125.5, 119.5 + (I + 2) * (I * 0.5), 135.5,
        119.5 + (I + 2) * (I * 0.5), I, 0.0);

    // fractional line width 0..2, 1 px H
    // -----------------
    Dash.Draw(17.5 + 4 * I, 192, 18.5 + 4 * I, 192, I * 0.1, 0);

    // fractional line positioning, 1 px H
    // -----------------
    Dash.Draw(17.5 + 4.1 * I - 0.1, 186, 18.5 + 4.1 * I - 0.1, 186, 1, 0);
  end;

  // Triangles
  for I := 1 to 13 do
  begin
    Rgba.White;
    Rgbb.FromRgbaDouble(I mod 2, (I mod 3) * 0.5, (I mod 5) * 0.25);
    FillColorArray(GradientColors, @Rgba, @Rgbb);

    CalculatLinearGradientTransform(Width - 150, Height - 20 - I * (I + 1.5),
      Width - 20, Height - 20 - I * (I + 1), FGradientMatrix);

    FRasterizer.Reset;
    FRasterizer.MoveToDouble(Width - 150, Height - 20 - I * (I + 1.5));
    FRasterizer.LineToDouble(Width - 20, Height - 20 - I * (I + 1));
    FRasterizer.LineToDouble(Width - 20, Height - 20 - I * (I + 2));

    RenderScanLines(FRasterizer, FScanLine, RenGradient);
  end;

  // Reset AA Gamma and render the controls
  GammaPower := TAggGammaPower.Create(1.0);
  try
    FRasterizer.Gamma(GammaPower);

    RenderControl(FRasterizer, FScanLine, FRenScan, FSliderGamma);
  finally
    GammaPower.Free;
  end;

  // Free AGG resources
  RenGradient.Free;
  GradientColors.Free;
  SpanGradient.Free;

  Dash.Free;
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
begin
  FRendererBase.SetClipBox(0, 0, Width, Height);
  inherited;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  I: Integer;

  Width, Height, T1, T2, T3, Radius: Double;
  X1, Y1, X2, Y2, X3, Y3: Double;

  Text: string;

  GradientColors: TAggPodAutoArray; // The Gradient colors

  SpanGouraud: TAggSpanGouraudRgba;
  RenGouraud : TAggRendererScanLineAA;
begin
  RandSeed := 123;

  FRendererBase.Clear(CRgba8Black);

  Width := Self.Width;
  Height := Self.Height;

  // Points
  StartTimer;
  for I := 0 to 19999 do
  begin
    Radius := 10 * Random + 0.5;

    FCircle.Initialize(PointDouble(Width * Random, Height * Random), Radius,
      Trunc(2 * Radius) + 10);

    FRasterizer.Reset;
    FRasterizer.AddPath(FCircle);

    RenderScanLines(FRasterizer, FScanLine, FRenScan);

    FRenScan.SetColor(RandomRgba8($80 + Random($80)));
  end;
  T1 := GetElapsedTime;

  // Strokes
  FGradientMatrix.Reset;
  GradientColors := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  StartTimer;
  for I := 0 to 1999 do
  begin
    X1 := Width * Random;
    Y1 := Height * Random;
    X2 := X1 + 0.5 * Width * (Random - 0.5);
    Y2 := Y1 + 0.5 * Height * (Random - 0.5);

    FillColorArray(GradientColors, RandomRgba8($80 + Random($80)), RandomRgba8);

    CalculatLinearGradientTransform(X1, Y1, X2, Y2, FGradientMatrix);
    FDashGradient.Draw(X1, Y1, X2, Y2, 10, 0);
  end;
  T2 := GetElapsedTime;

  // Gouraud triangles
  SpanGouraud := TAggSpanGouraudRgba.Create(FSpanAllocator);
  RenGouraud := TAggRendererScanLineAA.Create(FRendererBase, SpanGouraud);

  StartTimer;
  for I := 0 to 1999 do
  begin
    X1 := Width * Random;
    Y1 := Height * Random;
    X2 := X1 + 0.2 * Width * (2 * Random - 1);
    Y2 := Y1 + 0.2 * Height * (2 * Random - 1);;
    X3 := X1 + 0.2 * Width * (2 * Random - 1);;
    Y3 := Y1 + 0.2 * Height * (2 * Random - 1);;

    SpanGouraud.SetColors(RandomRgba8($80 + Random($80)), RandomRgba8,
      RandomRgba8);
    SpanGouraud.Triangle(X1, Y1, X2, Y2, X3, Y3, 0);

    FRasterizer.AddPath(SpanGouraud);

    RenderScanLines(FRasterizer, FScanLine, RenGouraud);
  end;
  T3 := GetElapsedTime;

  // Test results & Update
  Text := Format('Points=%.2fK/sec, Lines=%.2fK/sec, Triangles=%.2fK/sec',
    [20000 / T1, 2000 / T2, 2000 / T3]);
  DisplayMessage(Text);

  UpdateWindow;

  // Free AGG resources
  SpanGouraud.Free;
  RenGouraud.Free;
  GradientColors.Free;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('A test of Anti-Aliasing the same as in '
      + 'http://homepage.mac.com/arekkusu/bugs/invariance'
      + 'The performance of AGG on a typical P-IV 2GHz is: '
      + 'Points: 37.46K/sec, Lines: 5.04K/sec, Triangles: 7.43K/sec'#13#13
      + 'How to play with:'#13#13
      + 'Click any mouse button to run the performance test. Then, after '
      + 'you''ll see the triangles, resize the window to return to the '
      + 'original rendering.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Anti-Aliasing Test (F1-Help)';

    if Init(480, 350, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
