program ParticleDemo;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{-$DEFINE DISORDER}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggColor in '..\..\Source\AggColor.pas',
  AggArray in '..\..\Source\AggArray.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggRenderingBufferDynaRow in '..\..\Source\AggRenderingBufferDynaRow.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',

  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggQuadratureOscillator in '..\..\Source\AggQuadratureOscillator.pas';

const
  CFlipY = True;

type
  TAggGradientTricolor = class(TAggCustomArray)
  private
    FC1, FC2, FC3, FRc: TAggRgba8;
  protected
    function GetSize: Cardinal; override;
    function ArrayOperator(Index: Cardinal): Pointer; override;
  public
    constructor Create(C1, C2, C3: TAggRgba8);

    procedure Colors(C1, C2, C3: TAggRgba8);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FAngle, FCenterScale: Double;
    FParticlesValue: Double;
    FSpeedValue: Double;
    FDeltaCenter: Double;

    FCenter, FDelta: array [0..999] of TPointDouble;
    FRadius: array [0..999] of Double;
    FColor: array [0..999, 0..2] of TAggRgba8;

    FGradients: array [0..999] of TAggPodAutoArray;
    FCache: array [0..999] of TAggRenderingBufferDynaRow;

    FSliderParticles, FSliderSpeed: TAggControlSlider;
    FCheckBoxUseCache, FCheckBoxRun: TAggControlCheckBox;

    FGradientCircle: TAggGradientCircle;
    FSpanAllocator: TAggSpanAllocator;
    FTxt: TAggGsvText;
    FPt: TAggConvStroke;

    FRunFlag, FUseCacheFlag, FFirstTime: Boolean;
    FPixelFormat: TAggPixelFormatProcessor;
    FPixelFormatPre: TAggPixelFormatProcessor;
    FRendererBase: TAggRendererBase;
    FRendererBasePre : TAggRendererBase;
    FRenScan: TAggRendererScanLineAASolid;
    FScanLine: TAggScanLineUnpacked8;
    FRasterizer: TAggRasterizerScanLineAA;
  protected
    procedure RenderParticle(Ren: TAggRendererBase; I: Cardinal; X, Y: Double);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;
    procedure OnResize(Width: Integer; Height: Integer); override;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
    procedure OnIdle; override;
    procedure OnControlChange; override;
  end;


{ GradientTricolor }

constructor TAggGradientTricolor.Create(C1, C2, C3: TAggRgba8);
begin
  FC1 := C1;
  FC2 := C2;
  FC3 := C3;
end;

function TAggGradientTricolor.GetSize: Cardinal;
begin
  Result := 256;
end;

function TAggGradientTricolor.ArrayOperator(Index: Cardinal): Pointer;
begin
  if Index <= 127 then
  begin
    Index := Index * 2;

    FRc.R := Int8u((((FC2.R - FC1.R) * Integer(Index)) + (FC1.R shl 8)) shr 8);
    FRc.G := Int8u((((FC2.G - FC1.G) * Integer(Index)) + (FC1.G shl 8)) shr 8);
    FRc.B := Int8u((((FC2.B - FC1.B) * Integer(Index)) + (FC1.B shl 8)) shr 8);
    FRc.A := Int8u((((FC2.A - FC1.A) * Integer(Index)) + (FC1.A shl 8)) shr 8);
  end
  else
  begin
    Index := (Index - 127) * 2;

    FRc.R := Int8u((((FC3.R - FC2.R) * Integer(Index)) + (FC2.R shl 8)) shr 8);
    FRc.G := Int8u((((FC3.G - FC2.G) * Integer(Index)) + (FC2.G shl 8)) shr 8);
    FRc.B := Int8u((((FC3.B - FC2.B) * Integer(Index)) + (FC2.B shl 8)) shr 8);
    FRc.A := Int8u((((FC3.A - FC2.A) * Integer(Index)) + (FC2.A shl 8)) shr 8);
  end;

  Result := @FRc;
end;

procedure TAggGradientTricolor.Colors(C1, C2, C3: TAggRgba8);
begin
  FC1 := C1;
  FC2 := C2;
  FC3 := C3;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  I: Integer;
  C: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  PixelFormatBgra32(FPixelFormat, RenderingBufferWindow);
  PixelFormatBgra32Pre(FPixelFormatPre, RenderingBufferWindow);
  FRendererBase := TAggRendererBase.Create(FPixelFormat);
  FRendererBasePre := TAggRendererBase.Create(FPixelFormatPre);
  FRenScan := TAggRendererScanLineAASolid.Create(FRendererBase);

  FGradientCircle := TAggGradientCircle.Create;
  FSpanAllocator := TAggSpanAllocator.Create;

  FSliderParticles := TAggControlSlider.Create(5, 5, 300, 12, not FlipY);
  FSliderParticles.SetRange(10, 1000);
  FSliderParticles.Value := 200;
  FSliderParticles.Caption := 'Number of Particles=%.0f';
  AddControl(FSliderParticles);
  FSliderParticles.NoTransform;
  FParticlesValue := FSliderParticles.Value;

  FSliderSpeed := TAggControlSlider.Create(5, 20, 300, 12 + 15, not FlipY);
  FSliderSpeed.SetRange(0.025, 2.0);
  FSliderSpeed.Value := 1.0;
  FSliderSpeed.Caption := 'Dark Energy=%.3f';
  AddControl(FSliderSpeed);
  FSliderSpeed.NoTransform;

  FCheckBoxUseCache := TAggControlCheckBox.Create(320, 5, 'Use Bitmap Cache',
    not FlipY);
  C.White;
  FCheckBoxUseCache.TextColor := C;
  FCheckBoxUseCache.InactiveColor := C;
  C.FromRgbaDouble(0.8, 0, 0);
  FCheckBoxUseCache.ActiveColor := C;
  AddControl(FCheckBoxUseCache);
  FCheckBoxUseCache.NoTransform;

  FCheckBoxRun := TAggControlCheckBox.Create(320, 20, 'Start the Universe!',
    not FlipY);
  C.White;
  FCheckBoxRun.TextColor := C;
  FCheckBoxRun.InactiveColor := C;
  C.FromRgbaDouble(0.8, 0, 0);
  FCheckBoxRun.ActiveColor := C;
  FCheckBoxRun.Status := True;
  AddControl(FCheckBoxRun);
  FCheckBoxRun.NoTransform;

  FRunFlag := True;
  FUseCacheFlag := False;
  FSpeedValue := FSliderSpeed.Value;
  FFirstTime := True;

  FAngle := 0;
  FCenterScale := 0;
  FDeltaCenter := 0.5;

  FTxt := TAggGsvText.Create;
  FTxt.SetSize(10);
  FPt := TAggConvStroke.Create(FTxt);
  FPt.Width := 1.5;

  for I := 0 to 999 do
  begin
    FCache[I] := nil;
    FGradients[I] := TAggPodAutoArray.Create(256, SizeOf(TAggColor));
  end;
end;

destructor TAggApplication.Destroy;
var
  I: Integer;
begin
  FPt.Free;
  FTxt.Free;

  FSliderParticles.Free;
  FSliderSpeed.Free;
  FCheckBoxUseCache.Free;
  FCheckBoxRun.Free;

  for I := 0 to 999 do
  begin
    if FCache[I] <> nil then
      FCache[I].Free;

    FGradients[I].Free;
  end;

  FSpanAllocator.Free;
  FGradientCircle.Free;

  FRenScan.Free;
  FRendererBase.Free;
  FRendererBasePre.Free;
  FPixelFormatPre.Free;
  FPixelFormat.Free;

  FRasterizer.Free;
  FScanLine.Free;

  inherited;
end;

procedure TAggApplication.RenderParticle(Ren: TAggRendererBase; I: Cardinal;
  X, Y: Double);
var
  Grm: TAggTransAffine;
  Circle: TAggCircle;

  Sg: TAggSpanGradient;
  Rg: TAggRendererScanLineAA;

  SpanInterpolator: TAggSpanInterpolatorLinear;

  R: Double;
begin
  Grm := TAggTransAffine.Create;
  try
    R := FRadius[I];

    Grm.Reset;
    Grm.Scale(R * 0.1);
    Grm.Translate(X, Y);
    Grm.Invert;

    Circle := TAggCircle.Create(PointDouble(X, Y), R, 32);
    try
      FRasterizer.AddPath(Circle);
    finally
      Circle.Free;
    end;

    SpanInterpolator := TAggSpanInterpolatorLinear.Create(Grm);
    try
      Sg := TAggSpanGradient.Create(FSpanAllocator, SpanInterpolator,
        FGradientCircle, FGradients[I], 0, 10);
      try
        Rg := TAggRendererScanLineAA.Create(Ren, Sg);
        try
          RenderScanLines(FRasterizer, FScanLine, Rg);
        finally
          Rg.Free;
        end;
      finally
        Sg.Free;
      end;
    finally
      SpanInterpolator.Free;
    end;
  finally
    Grm.Free;
  end;
end;

procedure TAggApplication.OnInit;
var
  N, I, J, D: Cardinal;
  Component: Integer;
{$IFDEF DISORDER}
  Da: Integer;
{$ENDIF}
  Divisor, Angle, Speed, K: Double;
  Grc: TAggGradientTricolor;
  Gr: TAggPodAutoArray;
  Pixf: TAggPixelFormatProcessor;
  RendererBase : TAggRendererBase;
begin
  N := Trunc(FSliderParticles.Value);

  Randomize;

  if FCheckBoxUseCache.Status then
    Divisor := 250.0
  else
    Divisor := 500.0;

  if FFirstTime then
  begin
    for I := 0 to N - 1 do
    begin
      FCenter[I].X := 0.5 * Width {$IFDEF DISORDER} + Random(10) - 5 {$ENDIF};
      FCenter[I].Y := 0.5 * Height {$IFDEF DISORDER} + Random(10) - 5 {$ENDIF};

{$IFDEF DISORDER}
      if Rand and 1 <> 0 then
        FDelta.X[I] := (Random(5000) + 1000) / Divisor
      else
        FDelta.X[I] := -(Random(5000) + 1000) / Divisor;

      FDelta.Y[I] := FDelta.X[I];

      if Rand and 1 <> 0 then
        FDelta.Y[I] := -FDelta.Y[I];

      // ---
      Angle := 0.25 * Pi * Random;
      Da := Random(4);
      Angle := Angle + Pi * 0.1 * (5 * Da + 1));

{$ELSE}
      Angle := 2 * Pi * Random;

{$ENDIF}
      Speed := (Random(5000) + 1000) / Divisor;

      SinCosScale(Angle, FDelta[I].Y, FDelta[I].X, Speed);

      K := 1.0 - N / 2000.0;

      FRadius[I] := (Random(30) + 15) * K;

      FColor[I, 0].Initialize(Random($100), Random($100), Random($100), 0);
      FColor[I, 1].Initialize(Random($100), Random($100), Random($100), 255);

      Component := Random(4);

      if Component = 0 then
        FColor[I, 1].R := 255;

      if Component = 1 then
        FColor[I, 1].G := 255;

      if Component = 2 then
        FColor[I, 1].B := 255;

{$IFDEF DISORDER }
      FColor[I, 0] := FColor[I, 1];
      FColor[I, 0].A := 0;
{$ENDIF }

      FColor[I, 2].Initialize(Random($100), Random($100), Random($100), 0);

      Grc := TAggGradientTricolor.Create(FColor[I, 0], FColor[I, 1],
        FColor[I, 2]);
      try
        Gr := FGradients[I];
        for J := 0 to Gr.Size - 1 do
          Move(Grc[J]^, Gr[J]^, SizeOf(TAggColor));
      finally
        Grc.Free;
      end;
    end;

    FFirstTime := False;
  end;

  if FCheckBoxUseCache.Status then
  begin
    for I := 0 to 999 do
    begin
      if FCache[I] <> nil then
        FCache[I].Free;

      FCache[I] := nil;
    end;

    for I := 0 to N - 1 do
    begin
      D := Trunc(FRadius[I]) * 2;

      FCache[I] := TAggRenderingBufferDynaRow.Create(D, D, D * 4);

      PixelFormatAlphaBlendRgba(Pixf, FCache[I], CAggOrderBgra);

      RendererBase := TAggRendererBase.Create(Pixf, True);
      try
        RenderParticle(RendererBase, I, D * 0.5, D * 0.5);
      finally
        RendererBase.Free;
      end;
    end;
  end;
end;

procedure TAggApplication.OnDraw;
var
  I: Cardinal;
  Width, Height, X, Y: Integer;
  PixelFormatCache: TAggPixelFormatProcessor;
  N : Cardinal;
  Tm: Double;
begin
  Width := RenderingBufferWindow.Width;
  Height := RenderingBufferWindow.Height;

  FRasterizer.SetClipBox(0, 0, Width, Height);

  FRendererBase.Clear(CRgba8Black);

  // Render
  if FCheckBoxRun.Status then
  begin
    StartTimer;

    N := Trunc(FSliderParticles.Value);

    if FCheckBoxUseCache.Status then
      for I := 0 to N - 1 do
      begin
        if FCache[I] <> nil then
        begin
          PixelFormatAlphaBlendRgba(PixelFormatCache, FCache[I], CAggOrderBgra);
          try
            X := Trunc(FCenter[I].X - FRadius[I]) + 1;
            Y := Trunc(FCenter[I].Y - FRadius[I]) + 1;

            FRendererBasePre.BlendFrom(PixelFormatCache, 0, X, Y);
          finally
            PixelFormatCache.Free;
          end;
        end;
      end
    else
      for I := 0 to N - 1 do
        RenderParticle(FRendererBase, I, FCenter[I].X, FCenter[I].Y);

    Tm := GetElapsedTime;

    FTxt.SetStartPoint(10, 35);
    FTxt.SetText(Format('%6.1f fps', [1000.0 / Tm]));

    FRasterizer.AddPath(FPt);

    FRenScan.SetColor(CRgba8White);

    RenderScanLines(FRasterizer, FScanLine, FRenScan);
  end;

  // Render the controls
  RenderControl(FRasterizer, FScanLine, FRenScan, FSliderParticles);
  RenderControl(FRasterizer, FScanLine, FRenScan, FSliderSpeed);
  RenderControl(FRasterizer, FScanLine, FRenScan, FCheckBoxUseCache);
  RenderControl(FRasterizer, FScanLine, FRenScan, FCheckBoxRun);
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
begin
  inherited;
  FRendererBase.SetClipBox(0, 0, Width, Height);
  FRendererBasePre.SetClipBox(0, 0, Width, Height);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if (mkfMouseLeft in Flags) or (mkfMouseRight in Flags) then
    ForceRedraw;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Demonstration of using the bitmap cache.'#13#13
      + 'Cached bitmaps are descended from TAggRendererBase '
      + 'and OnDraw method just alpha blended to the scene.');
end;

procedure TAggApplication.OnIdle;
var
  N, I: Cardinal;
  Delta, Center: TPointDouble;
  MaxDist, D: Double;
{$IFDEF DISORDER}
  X1, Y1, X2, Y2: Double;
{$ENDIF}
begin
  N := Trunc(FSliderParticles.Value);

{$IFDEF DISORDER}
  X1 := -100;
  Y1 := -100;
  X2 := Width + 100;
  Y2 := Height + 100;
{$ENDIF}
  SinCosScale(FAngle, Delta.Y, Delta.X, FCenterScale);
  Center := PointDouble(Delta.X + Width * 0.5, Delta.Y + Height * 0.5);

  MaxDist := Sqrt(0.5 * (Sqr(Width) + Sqr(Height)));

  FAngle := FAngle + Deg2Rad(5);
  FCenterScale := FCenterScale + FDeltaCenter;

  if FCenterScale > MaxDist * 0.5 then
  begin
    FCenterScale := MaxDist * 0.5;
    FDeltaCenter := -FDeltaCenter;
  end;

  if FCenterScale < 10.0 then
  begin
    FCenterScale := 10;
    FDeltaCenter := -FDeltaCenter;
  end;

  for I := 0 to N - 1 do
  begin
    FCenter[I].X := FCenter[I].X + FDelta[I].X * FSliderSpeed.Value;
    FCenter[I].Y := FCenter[I].Y + FDelta[I].Y * FSliderSpeed.Value;

    D := CalculateDistance(FCenter[I], Center);

    if D > MaxDist then
    begin
      FCenter[I].X := Center.X;
      FCenter[I].Y := Center.Y;
    end;

{$IFDEF DISORDER}
    {
      if FCenter.X[i] < x1 then
      begin
        FCenter.X[i] := Center.X;
        FCenter.Y[i] := Center.Y;
      end;

      if FCenter.X[i] > x2 then
      begin
        FCenter.X[i] := Center.X;
        FCenter.Y[i] := Center.Y;
      end;

      if FCenter.Y[i] < y1 then
      begin
        FCenter.X[i] := Center.X;
        FCenter.Y[i] := Center.Y;
      end;

      if FCenter.Y[i] > y2 then
      begin
        FCenter.X[i] := Center.X;
        FCenter.Y[i] := Center.Y;
      end;

      { }
    if FCenter[I].X < X1 then
    begin
      FCenter[I].X := X1;
      FDelta[I].X := -FDelta[I].X;
    end;

    if FCenter[I].X > X2 then
    begin
      FCenter[I].X := X2;
      FDelta[I].X := -FDelta[I].X;
    end;

    if FCenter[I].Y < Y1 then
    begin
      FCenter[I].Y := Y1;
      FDelta[I].Y := -FDelta[I].Y;
    end;

    if FCenter[I].Y > Y2 then
    begin
      FCenter[I].Y := Y2;
      FDelta[I].Y := -FDelta[I].Y;
    end;
{$ENDIF }
  end;

  ForceRedraw;
end;

procedure TAggApplication.OnControlChange;
var
  Stop, Over: Boolean;
begin
  if FRunFlag <> FCheckBoxRun.Status then
  begin
    WaitMode := not FCheckBoxRun.Status;

    FRunFlag := FCheckBoxRun.Status;

    if FRunFlag then
    begin
      FFirstTime := True;

      OnInit;
    end;
  end
  else
  begin
    Stop := False;
    Over := False;

    if FCheckBoxUseCache.Status <> FUseCacheFlag then
    begin
      FUseCacheFlag := FCheckBoxUseCache.Status;

      Stop := False;
      Over := True;
    end;

    if FSliderParticles.Value <> FParticlesValue then
    begin
      FParticlesValue := FSliderParticles.Value;

      Stop := True;
      Over := False;
    end;

    if FSliderSpeed.Value <> FSpeedValue then
    begin
      FSpeedValue := FSliderSpeed.Value;

      Stop := False;
      Over := False;
    end;

    if Stop then
    begin
      WaitMode := True;
      FCheckBoxRun.Status := False;

    end
    else if Over then
      OnInit;
  end;
end;

begin
  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'Renesis project -- Particles demo. (F1-Help)';

    if Init(600, 500, [wfResize]) then
    begin
      WaitMode := False;
      Run;
    end;
  finally
    Free;
  end;
end.
