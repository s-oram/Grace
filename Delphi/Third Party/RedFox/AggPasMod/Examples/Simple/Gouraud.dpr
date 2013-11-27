program Gouraud;

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
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\AggControl.pas',
  AggSliderControl in '..\..\Source\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggMath in '..\..\Source\AggMath.pas',
  AggDdaLine in '..\..\Source\AggDdaLine.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGouraudRgba in '..\..\Source\AggSpanGouraudRgba.pas',
  AggSpanGouraudGray in '..\..\Source\AggSpanGouraudGray.pas',
  AggSpanSolid in '..\..\Source\AggSpanSolid.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas'

{$I Pixel_Formats.inc }

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FX, FY: array [0..2] of Double;
    FDelta: TPointDouble;
    FDeltaIndex: Integer;
    FSliderDilation, FSliderGamma, FSliderAlpha: TAggControlSlider;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure RenderGouraud(ScanLine: TAggCustomScanLine;
      Rasterizer: TAggRasterizerScanLineAA);

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

  FSliderDilation := TAggControlSlider.Create(5, 5, 395, 11, not FlipY);
  FSliderGamma := TAggControlSlider.Create(5, 20, 395, 26, not FlipY);
  FSliderAlpha := TAggControlSlider.Create(5, 35, 395, 41, not FlipY);

  FDeltaIndex := -1;

  FX[0] := 57;
  FY[0] := 60;
  FX[1] := 369;
  FY[1] := 170;
  FX[2] := 143;
  FY[2] := 310;

  AddControl(FSliderDilation);
  AddControl(FSliderGamma);
  AddControl(FSliderAlpha);

  FSliderDilation.Caption := 'Dilation=%3.2f';
  FSliderGamma.Caption := 'Linear Gamma=%3.2f';
  FSliderAlpha.Caption := 'Opacity=%3.2f';

  FSliderDilation.Value := 0.175;
  FSliderGamma.Value := 0.809;
  FSliderAlpha.Value := 1;
end;

destructor TAggApplication.Destroy;
begin
  FSliderDilation.Free;
  FSliderGamma.Free;
  FSliderAlpha.Free;

  inherited;
end;

procedure TAggApplication.RenderGouraud(ScanLine: TAggCustomScanLine;
  Rasterizer: TAggRasterizerScanLineAA);
var
  Alpha, Brc, D, Xc, Yc, X1, Y1, X2, Y2, X3, Y3: Double;

  Pixf: TAggPixelFormatProcessor;

{$IFDEF AGG_GRAY8 }
  SpanGen: TAggSpanGouraudGray;
{$ELSE }
  SpanGen: TAggSpanGouraudRgba;
{$ENDIF }
  RendererBase   : TAggRendererBase;
  SpanAllocator : TAggSpanAllocator;
  RendGouraud: TAggRendererScanLineAA;

  GamLin: TAggGammaLinear;

  Rgba, Rgbb, Rgbc: TAggColor;
begin
  Alpha := FSliderAlpha.Value;
  Brc := 1;

  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    SpanAllocator := TAggSpanAllocator.Create;
{$IFDEF AGG_GRAY8 }
    SpanGen := TAggSpanGouraudGray.Create(SpanAllocator);
{$ELSE }
    SpanGen := TAggSpanGouraudRgba.Create(SpanAllocator);
{$ENDIF }
    try
      RendGouraud := TAggRendererScanLineAA.Create(RendererBase, SpanGen);
      try
        GamLin := TAggGammaLinear.Create(0, FSliderGamma.Value);
        try
          Rasterizer.Gamma(GamLin);
        finally
          GamLin.Free;
        end;

        D := FSliderDilation.Value;

        // Single triangle
        { rgba.ConstrDbl   (1 ,0 ,0 ,alpha );
          rgbb.ConstrDbl   (0 ,1 ,0 ,alpha );
          rgbc.ConstrDbl   (0 ,0 ,1 ,alpha );
          SpanGen.colors  (@rgba ,@rgbb ,@rgbc );
          SpanGen.triangle(FX[0 ] ,FY[0 ] ,FX[1 ] ,FY[1 ] ,FX[2 ] ,FY[2 ] ,d );

          Rasterizer.AddPath    (@SpanGen);
          RenderScanLines(Rasterizer, ScanLine, RendGouraud);{ }

        // Six triangles
        Xc := (FX[0] + FX[1] + FX[2]) / 3;
        Yc := (FY[0] + FY[1] + FY[2]) / 3;

        X1 := (FX[1] + FX[0]) * 0.5 - (Xc - (FX[1] + FX[0]) * 0.5);
        Y1 := (FY[1] + FY[0]) * 0.5 - (Yc - (FY[1] + FY[0]) * 0.5);

        X2 := (FX[2] + FX[1]) * 0.5 - (Xc - (FX[2] + FX[1]) * 0.5);
        Y2 := (FY[2] + FY[1]) * 0.5 - (Yc - (FY[2] + FY[1]) * 0.5);

        X3 := (FX[0] + FX[2]) * 0.5 - (Xc - (FX[0] + FX[2]) * 0.5);
        Y3 := (FY[0] + FY[2]) * 0.5 - (Yc - (FY[0] + FY[2]) * 0.5);

        Rgba.FromRgbaDouble(1, 0, 0, Alpha);
        Rgbb.FromRgbaDouble(0, 1, 0, Alpha);
        Rgbc.FromRgbaDouble(Brc, Brc, Brc, Alpha);
        SpanGen.SetColors(@Rgba, @Rgbb, @Rgbc);
        SpanGen.Triangle(FX[0], FY[0], FX[1], FY[1], Xc, Yc, D);

        Rasterizer.AddPath(SpanGen);
        RenderScanLines(Rasterizer, ScanLine, RendGouraud);

        Rgba.FromRgbaDouble(0, 1, 0, Alpha);
        Rgbb.FromRgbaDouble(0, 0, 1, Alpha);
        Rgbc.FromRgbaDouble(Brc, Brc, Brc, Alpha);
        SpanGen.SetColors(@Rgba, @Rgbb, @Rgbc);
        SpanGen.Triangle(FX[1], FY[1], FX[2], FY[2], Xc, Yc, D);

        Rasterizer.AddPath(SpanGen);
        RenderScanLines(Rasterizer, ScanLine, RendGouraud);

        Rgba.FromRgbaDouble(0, 0, 1, Alpha);
        Rgbb.FromRgbaDouble(1, 0, 0, Alpha);
        Rgbc.FromRgbaDouble(Brc, Brc, Brc, Alpha);
        SpanGen.SetColors(@Rgba, @Rgbb, @Rgbc);
        SpanGen.Triangle(FX[2], FY[2], FX[0], FY[0], Xc, Yc, D);

        Rasterizer.AddPath(SpanGen);
        RenderScanLines(Rasterizer, ScanLine, RendGouraud);

        Brc := 1 - Brc;

        Rgba.FromRgbaDouble(1, 0, 0, Alpha);
        Rgbb.FromRgbaDouble(0, 1, 0, Alpha);
        Rgbc.FromRgbaDouble(Brc, Brc, Brc, Alpha);
        SpanGen.SetColors(@Rgba, @Rgbb, @Rgbc);
        SpanGen.Triangle(FX[0], FY[0], FX[1], FY[1], X1, Y1, D);

        Rasterizer.AddPath(SpanGen);
        RenderScanLines(Rasterizer, ScanLine, RendGouraud);

        Rgba.FromRgbaDouble(0, 1, 0, Alpha);
        Rgbb.FromRgbaDouble(0, 0, 1, Alpha);
        Rgbc.FromRgbaDouble(Brc, Brc, Brc, Alpha);
        SpanGen.SetColors(@Rgba, @Rgbb, @Rgbc);
        SpanGen.Triangle(FX[1], FY[1], FX[2], FY[2], X2, Y2, D);

        Rasterizer.AddPath(SpanGen);
        RenderScanLines(Rasterizer, ScanLine, RendGouraud);

        Rgba.FromRgbaDouble(0, 0, 1, Alpha);
        Rgbb.FromRgbaDouble(1, 0, 0, Alpha);
        Rgbc.FromRgbaDouble(Brc, Brc, Brc, Alpha);
        SpanGen.SetColors(@Rgba, @Rgbb, @Rgbc);
        SpanGen.Triangle(FX[2], FY[2], FX[0], FY[0], X3, Y3, D);

        Rasterizer.AddPath(SpanGen);
        RenderScanLines(Rasterizer, ScanLine, RendGouraud); { }

        // Free AGG resources
        SpanAllocator.Free;
      finally
        RendGouraud.Free;
      end;
    finally
      SpanGen.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase : TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Rasterizer: TAggRasterizerScanLineAA;
  ScanLine : TAggScanLineUnpacked8;

  Rgba : TAggColor;
  GammaNo: TAggGammaNone;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      ScanLine := TAggScanLineUnpacked8.Create;
      try
        Rasterizer := TAggRasterizerScanLineAA.Create;
        try
          // Render Gouraud
          RenderGouraud(ScanLine, Rasterizer);

          // Render the controls
          GammaNo := TAggGammaNone.Create;
          try
            Rasterizer.Gamma(GammaNo);
          finally
            GammaNo.Free;
          end;

          RenderControl(Rasterizer, ScanLine, RenScan, FSliderDilation);
          RenderControl(Rasterizer, ScanLine, RenScan, FSliderGamma);
          RenderControl(Rasterizer, ScanLine, RenScan, FSliderAlpha);
        finally
          Rasterizer.Free;
        end;
      finally
        ScanLine.Free;
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
var
  Dx, Dy: Double;
begin
  if mkfMouseLeft in Flags then
  begin
    if FDeltaIndex = 3 then
    begin
      Dx := X - FDelta.X;
      Dy := Y - FDelta.Y;

      FX[1] := FX[1] - (FX[0] - Dx);
      FY[1] := FY[1] - (FY[0] - Dy);
      FX[2] := FX[2] - (FX[0] - Dx);
      FY[2] := FY[2] - (FY[0] - Dy);
      FX[0] := Dx;
      FY[0] := Dy;

      ForceRedraw;
      Exit;
    end;

    if FDeltaIndex >= 0 then
    begin
      FX[FDeltaIndex] := X - FDelta.X;
      FY[FDeltaIndex] := Y - FDelta.Y;

      ForceRedraw;
    end;

  end
  else
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  I: Cardinal;
  ScanLine : TAggScanLineUnpacked8;
  Rasterizer: TAggRasterizerScanLineAA;
  T1: Double;
begin
  if mkfMouseRight in Flags then
  begin
    ScanLine := TAggScanLineUnpacked8.Create;
    try
      Rasterizer := TAggRasterizerScanLineAA.Create;

      StartTimer;

      for I := 0 to 99 do
        RenderGouraud(ScanLine, Rasterizer);

      T1 := GetElapsedTime;

      DisplayMessage(Format('Time=%2.2f ms', [T1]));
    finally
      ScanLine.Free;
    end;
    Rasterizer.Free;
  end;

  if mkfMouseLeft in Flags then
  begin
    I := 0;

    while I < 3 do
    begin
      if Sqrt((X - FX[I]) * (X - FX[I]) + (Y - FY[I]) * (Y - FY[I])) < 10
      then
      begin
        FDelta.X := X - FX[I];
        FDelta.Y := Y - FY[I];
        FDeltaIndex := I;

        Break;
      end;

      Inc(I);
    end;

    if I = 3 then
      if PointInTriangle(FX[0], FY[0], FX[1], FY[1], FX[2], FY[2], X, Y)
      then
      begin
        FDelta.X := X - FX[0];
        FDelta.Y := Y - FY[0];
        FDeltaIndex := 3;
      end;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FDeltaIndex := -1;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
var
  Delta: TPointDouble;
begin
  Delta.X := 0;
  Delta.Y := 0;

  case TKeyCode(Key) of
    kcLeft:
      Delta.X := -0.1;
    kcRight:
      Delta.X := 0.1;
    kcUp:
      Delta.Y := 0.1;
    kcDown:
      Delta.Y := -0.1;
  end;

  FX[0] := FX[0] + Delta.X;
  FY[0] := FY[0] + Delta.Y;
  FX[1] := FX[1] + Delta.X;
  FY[1] := FY[1] + Delta.Y;

  ForceRedraw;

  if Key = Cardinal(kcF1) then
    DisplayMessage('Gouraud shading. It''s a simple method of interpolating '
      + 'colors in a triangle. There''s no "cube" drawn, there''re just 6 '
      + 'triangles. You define a triangle and colors in its vertices. When '
      + 'rendering, the colors will be linearly interpolated. But there''s a '
      + 'problem that appears when drawing adjacent triangles with Anti-'
      + 'Aliasing. Anti-Aliased polygons do not "dock" to each other '
      + 'correctly, there visual artifacts at the edges appear. I call it '
      + '"the problem of adjacent edges". AGG has a simple mechanism that '
      + 'allows you to get rid of the artifacts, just dilating the polygons '
      + 'and/or changing the Gamma-correction value. But it''s tricky, because '
      + 'the values depend on the opacity of the polygons. In this example you '
      + 'can change the opacity, the dilation value and Gamma.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to drag the Red, Green and Blue corners of '
      + 'the "cube". Use the right mouse button to issue a performance test '
      + '(100x).'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Gouraud Shading (F1-Help)';

    if Init(400, 320, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
