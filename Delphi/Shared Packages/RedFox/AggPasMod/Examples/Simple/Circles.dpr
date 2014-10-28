program Circles;

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

{$WARNINGS ON}
{$HINTS ON}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggScaleControl in '..\..\Source\Controls\AggScaleControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggBSpline in '..\..\Source\AggBSpline.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas'

{$I Pixel_Formats.inc }

const
  CFlipY = True;

  CDefaultNumPoints = 10000;

  CStartWidth = 400;
  CStartHeight = 400;

  CSplineRedX: TAggParallelogram = (0, 0.2, 0.4, 0.910484, 0.957258, 1);
  CSplineRedY: TAggParallelogram = (1, 0.8, 0.6, 0.066667, 0.169697, 0.6);

  CSplineGreenX: TAggParallelogram = (0, 0.292244, 0.485655, 0.564859,
    0.795607, 1);
  CSplineGreenY: TAggParallelogram = (0, 0.607260, 0.964065, 0.892558,
    0.435571, 0);

  CSplineBlueX: TAggParallelogram = (0, 0.055045, 0.143034, 0.433082,
    0.764859, 1);
  CSplineBlueY: TAggParallelogram = (0.385480, 0.128493, 0.021416, 0.271507,
    0.713974, 1);

type
  PScatterPoint = ^TScatterPoint;
  TScatterPoint = record
    X, Y, Z: Double;
    Color: TAggColor;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FNumPoints: Cardinal;
    FPoints: PScatterPoint;

    FScaleCtrlZ: TScaleControl;
    FSliderControlSel: TAggControlSlider;
    FSliderControlSize: TAggControlSlider;

    FSplineR, FSplineG, FSplineB: TAggBSpline;

    FPixelFormat: TAggPixelFormatProcessor;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean;
      NumPoints: Cardinal);
    destructor Destroy; override;

    procedure Generate;

    procedure OnInit; override;
    procedure OnDraw; override;
    procedure OnIdle; override;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean;
  NumPoints: Cardinal);
begin
  inherited Create(PixelFormat, FlipY);

  CPixelFormat(FPixelFormat, RenderingBufferWindow);

  FNumPoints := NumPoints;

  AggGetMem(Pointer(FPoints), NumPoints * SizeOf(TScatterPoint));

  FScaleCtrlZ := TScaleControl.Create(5, 5, CStartWidth - 5, 12, not FlipY);
  FSliderControlSel := TAggControlSlider.Create(5, 20, CStartWidth - 5, 27,
    not FlipY);
  FSliderControlSize := TAggControlSlider.Create(5, 35, CStartWidth - 5, 42,
    not FlipY);

  FSplineR := TAggBSpline.Create;
  FSplineG := TAggBSpline.Create;
  FSplineB := TAggBSpline.Create;

  FSplineR.Init(6, @CSplineRedX, @CSplineRedY);
  FSplineG.Init(6, @CSplineGreenX, @CSplineGreenY);
  FSplineB.Init(6, @CSplineBlueX, @CSplineBlueY);

  AddControl(FScaleCtrlZ);
  AddControl(FSliderControlSel);
  AddControl(FSliderControlSize);

  FSliderControlSize.Caption := 'Size';
  FSliderControlSel.Caption := 'Selectivity';
end;

destructor TAggApplication.Destroy;
begin
  FSplineR.Free;
  FSplineG.Free;
  FSplineB.Free;

  FScaleCtrlZ.Free;
  FSliderControlSel.Free;
  FSliderControlSize.Free;

  AggFreeMem(Pointer(FPoints), FNumPoints * SizeOf(TScatterPoint));

  FPixelFormat.Free;

  inherited;
end;

procedure TAggApplication.Generate;
var
  I: Cardinal;
  Rx, Ry, Z, Dist, Angle: Double;
  ScatterPnt, Pnt: TPointDouble;
begin
  Rx := InitialWidth / 3.5;
  Ry := InitialHeight / 3.5;

  for I := 0 to FNumPoints - 1 do
  begin
    Z := RandomMinMax(0.0, 1.0);

    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z := Z;

    SinCosScale(Z * 2.0 * Pi, Pnt.Y, Pnt.X, Ry, Rx);

    Dist := RandomMinMax(0.0, Rx * 0.5);
    Angle := RandomMinMax(0.0, Pi * 2.0);

    SinCos(Angle, ScatterPnt.Y, ScatterPnt.X);

    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).X :=
      InitialWidth * 0.5 + Pnt.X + ScatterPnt.X * Dist;

    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Y :=
      InitialHeight * 0.5 + Pnt.Y + ScatterPnt.Y * Dist;

    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint))
      .Color.FromRgbaDouble(FSplineR.Get(Z) * 0.8, FSplineG.Get(Z) * 0.8,
      FSplineB.Get(Z) * 0.8, 1.0);
  end;
end;

procedure TAggApplication.OnInit;
begin
  Generate;
end;

procedure TAggApplication.OnDraw;
var
  Ras: TAggRasterizerScanLineAA;
  Sl: TAggScanLinePacked8;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Circle: TAggCircle;
  T1: TAggConvTransform;

  Rgba, Temp: TAggColor;

  I, NumDrawn: Cardinal;

  Z, Alpha: Double;
  ScatterPoint: PScatterPoint;

  Txt: TAggGsvText;

  TxtOutline: TAggGsvTextOutline;
begin
  // Initialize structures
  Ras := TAggRasterizerScanLineAA.Create;
  Sl := TAggScanLinePacked8.Create;

  RendererBase := TAggRendererBase.Create(FPixelFormat);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Draw circles
      Circle := TAggCircle.Create;
      T1 := TAggConvTransform.Create(Circle, GetTransAffineResizing);

      NumDrawn := 0;

      for I := 0 to FNumPoints - 1 do
      begin
        ScatterPoint := FPoints;
        Inc(ScatterPoint, I);
        Z := ScatterPoint^.Z;

        Alpha := 1.0;

        if Z < FScaleCtrlZ.GetValue1 then
          Alpha := 1.0 - (FScaleCtrlZ.GetValue1 - Z) *
            FSliderControlSel.Value * 100.0;

        if Z > FScaleCtrlZ.GetValue2 then
          Alpha := 1.0 - (Z - FScaleCtrlZ.GetValue2) *
            FSliderControlSel.Value * 100.0;

        if Alpha > 1.0 then
          Alpha := 1.0;

        if Alpha < 0.0 then
          Continue;

        Circle.Initialize(PPointDouble(ScatterPoint)^,
          FSliderControlSize.Value * 5.0, 8);

        Ras.AddPath(T1);

        Temp := ScatterPoint^.Color;
        Rgba.FromRgbInteger(Temp.Rgba8.R, Temp.Rgba8.G, Temp.Rgba8.B, Alpha);

        RenScan.SetColor(@Rgba);
        RenderScanLines(Ras, Sl, RenScan);

        Inc(NumDrawn);
      end;

      // Render the controls
      RenderControl(Ras, Sl, RenScan, FScaleCtrlZ);
      RenderControl(Ras, Sl, RenScan, FSliderControlSel);
      RenderControl(Ras, Sl, RenScan, FSliderControlSize);

      // Render the Text
      Txt := TAggGsvText.Create;
      Txt.SetSize(15.0);
      Txt.SetText(Format('%08u', [NumDrawn]));
      Txt.SetStartPoint(10.0, InitialHeight - 20.0);

      TxtOutline := TAggGsvTextOutline.Create(Txt, GetTransAffineResizing);

      Ras.AddPath(TxtOutline);

      RenScan.SetColor(CRgba8Black);
      RenderScanLines(Ras, Sl, RenScan);

      // Free AGG resources
      T1.Free;
      Ras.Free;
      Sl.Free;
      Circle.Free;

      Txt.Free;
      TxtOutline.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnIdle;
var
  I: Cardinal;

begin
  for I := 0 to FNumPoints - 1 do
  begin
    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).X :=
      PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).X +
      RandomMinMax(0, FSliderControlSel.Value) - FSliderControlSel.Value * 0.5;

    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Y :=
      PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Y +
      RandomMinMax(0, FSliderControlSel.Value) - FSliderControlSel.Value * 0.5;

    PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z :=
      PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z +
      RandomMinMax(0, FSliderControlSel.Value * 0.01) -
      FSliderControlSel.Value * 0.005;

    if PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z < 0.0
    then
      PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z := 0.0;

    if PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z > 1.0
    then
      PScatterPoint(PtrComp(FPoints) + I * SizeOf(TScatterPoint)).Z := 1.0;
  end;

  ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    Generate;
    ForceRedraw;
  end;

  if mkfMouseRight in Flags then
    WaitMode := not WaitMode;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This example just demonstrates that AGG can be used in '
      + 'different scatter plot apllications. There''s a number of small '
      + 'circles drawn. You can change the parameters of drawing, watching '
      + 'for the performance and the number of circles simultaneously '
      + 'rendered. Note, that the circles are drawn with High quality, '
      + 'possibly translucent, and with subpixel accuracy.'#13#13
      + 'How to play with:'#13#13
      + 'Press the left mouse button to generate a new set of points. '#13
      + 'Press the right mouse button to make the points randomly change '
      + 'their coordinates.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Err: Integer;
  NumPoints: Cardinal;

begin
  NumPoints := CDefaultNumPoints;

  if ParamCount > 0 then
  begin
    Val(Paramstr(1), NumPoints, Err);

    if NumPoints = 0 then
      NumPoints := CDefaultNumPoints;

    if NumPoints > 20000 then
      NumPoints := 20000;
  end;

  with TAggApplication.Create(CPixFormat, CFlipY, NumPoints) do
  try
    Caption := 'AGG Drawing random circles - A scatter plot prototype (F1-Help)';

    if Init(CStartWidth, CStartHeight, [wfResize, wfKeepAspectRatio]) then
      Run;
  finally
    Free;
  end;
end.
