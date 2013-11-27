program TransCurve2_ft;

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

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggMath in '..\..\Source\AggMath.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggTransDoublePath in '..\..\Source\AggTransDoublePath.pas',
  AggConvBSpline in '..\..\Source\AggConvBSpline.pas',
  AggConvSegmentator in '..\..\Source\AggConvSegmentator.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggFontEngine in '..\..\Source\AggFontEngine.pas',
  AggFontFreeType in '..\..\Source\AggFontFreeType.pas',
  AggFontCacheManager in '..\..\Source\AggFontCacheManager.pas',
  AggInteractivePolygon
{$I Pixel_Formats.inc}

const
  CFlipY = True;
  CDisplayText: PAnsiChar = 'Anti-Grain Geometry is designed as a set of '
    + 'loosely coupled algorithms and class templates united with a common '
    + 'idea, so that all the components can be easily combined. Also, the '
    + 'template based design allows you to replace any part of the library '
    + 'without the necessity to modify a single byte in the existing code.';

type
  TAggApplication = class(TPlatformSupport)
  private
    FFontEngine: TAggFontEngineFreetypeInt16;
    FFontCacheManager: TAggFontCacheManager;
    FInteractivePolygon: array [0..1] of TInteractivePolygon;

    FSliderNumPoints: TAggControlSlider;
    FCheckBoxFixedLength: TAggControlCheckBox;
    FCheckBoxPreserveXScale: TAggControlCheckBox;
    FCheckBoxAnimate: TAggControlCheckBox;

    FDelta: array [0..1, 0..5] of TPointDouble;

    FPrevAnimate: Boolean;
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
    procedure OnControlChange; override;

    procedure MovePoint(X, Y, Dx, Dy: PDouble);
    procedure NormalizePoint(Index: Cardinal);

    procedure OnIdle; override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FFontEngine := TAggFontEngineFreetypeInt16.Create;
  FFontCacheManager := TAggFontCacheManager.Create(FFontEngine);
  FInteractivePolygon[0] := TInteractivePolygon.Create(6, 5.0);
  FInteractivePolygon[1] := TInteractivePolygon.Create(6, 5.0);

  FSliderNumPoints := TAggControlSlider.Create(5.0, 5.0, 340.0, 12.0,
    not FlipY);
  FCheckBoxFixedLength := TAggControlCheckBox.Create(350, 5.0, 'Fixed Length',
    not FlipY);
  FCheckBoxPreserveXScale := TAggControlCheckBox.Create(465, 5.0,
    'Preserve X scale', not FlipY);
  FCheckBoxAnimate := TAggControlCheckBox.Create(350, 25.0, 'Animate',
    not FlipY);

  FPrevAnimate := False;

  AddControl(FCheckBoxFixedLength);
  AddControl(FCheckBoxPreserveXScale);
  AddControl(FCheckBoxAnimate);

  FCheckBoxFixedLength.Status := True;
  FCheckBoxPreserveXScale.Status := True;

  FSliderNumPoints.SetRange(10.0, 400.0);
  FSliderNumPoints.Value := 200.0;
  FSliderNumPoints.Caption := 'Number of intermediate Points = %.3f';

  AddControl(FSliderNumPoints);

  FInteractivePolygon[0].Close := False;
  FInteractivePolygon[1].Close := False;
end;

destructor TAggApplication.Destroy;
begin
  FSliderNumPoints.Free;
  FCheckBoxFixedLength.Free;
  FCheckBoxPreserveXScale.Free;
  FCheckBoxAnimate.Free;

  FInteractivePolygon[0].Free;
  FInteractivePolygon[1].Free;

  FFontEngine.Free;
  FFontCacheManager.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  FInteractivePolygon[0].Xn[0] := 60;
  FInteractivePolygon[0].Yn[0] := 40;
  FInteractivePolygon[0].Xn[1] := 180;
  FInteractivePolygon[0].Yn[1] := 120;
  FInteractivePolygon[0].Xn[2] := 240;
  FInteractivePolygon[0].Yn[2] := 260;
  FInteractivePolygon[0].Xn[3] := 380;
  FInteractivePolygon[0].Yn[3] := 320;
  FInteractivePolygon[0].Xn[4] := 440;
  FInteractivePolygon[0].Yn[4] := 460;
  FInteractivePolygon[0].Xn[5] := 560;
  FInteractivePolygon[0].Yn[5] := 540;

  FInteractivePolygon[1].Xn[0] := 40;
  FInteractivePolygon[1].Yn[0] := 60;
  FInteractivePolygon[1].Xn[1] := 160;
  FInteractivePolygon[1].Yn[1] := 140;
  FInteractivePolygon[1].Xn[2] := 220;
  FInteractivePolygon[1].Yn[2] := 280;
  FInteractivePolygon[1].Xn[3] := 360;
  FInteractivePolygon[1].Yn[3] := 340;
  FInteractivePolygon[1].Xn[4] := 420;
  FInteractivePolygon[1].Yn[4] := 480;
  FInteractivePolygon[1].Xn[5] := 540;
  FInteractivePolygon[1].Yn[5] := 560;
end;

procedure TAggApplication.OnDraw;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RendererScanLine: TAggRendererScanLineAASolid;
  ScanLine: TAggScanLinePacked8;

  Rasterizer: TAggRasterizerScanLineAA;

  Path: array [0..1] of TSimplePolygonVertexSource;
  BSpline: array [0..1] of TAggConvBSpline;
  TransCurve: TAggTransDoublePath;
  ConvCurves: TAggConvCurve;
  ConvSegm: TAggConvSegmentator;
  ConvTransform : TAggConvTransform;

  X, Y: Double;

  P: PInt8u;

  Glyph: PAggGlyphCache;

  Stroke: array [0..1] of TAggConvStroke;
begin
  // Initialize structures
  CPixelFormat(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RendererScanLine := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      ScanLine := TAggScanLinePacked8.Create;
      Rasterizer := TAggRasterizerScanLineAA.Create;

      // Render the text
      Path[0] := TSimplePolygonVertexSource.Create
        (FInteractivePolygon[0].Polygon, FInteractivePolygon[0].NumPoints,
        False, False);
      Path[1] := TSimplePolygonVertexSource.Create
        (FInteractivePolygon[1].Polygon, FInteractivePolygon[1].NumPoints,
        False, False);

      BSpline[0] := TAggConvBSpline.Create(Path[0]);
      BSpline[1] := TAggConvBSpline.Create(Path[1]);

      BSpline[0].InterpolationStep := 1.0 / FSliderNumPoints.Value;
      BSpline[1].InterpolationStep := 1.0 / FSliderNumPoints.Value;

      TransCurve := TAggTransDoublePath.Create;
      ConvCurves := TAggConvCurve.Create(FFontCacheManager.PathAdaptor);
      ConvSegm := TAggConvSegmentator.Create(ConvCurves);
      ConvTransform := TAggConvTransform.Create(ConvSegm, TransCurve);

      TransCurve.PreserveXScale := FCheckBoxPreserveXScale.Status;

      if FCheckBoxFixedLength.Status then
        TransCurve.BaseLength := 1140.0;

      TransCurve.BaseHeight := 30.0;
      TransCurve.AddPaths(BSpline[0], BSpline[1]);

      ConvSegm.ApproximationScale := 3.0;
      ConvCurves.ApproximationScale := 5.0;

      if FFontEngine.LoadFont('timesi.ttf', 0, grOutline) then
      begin
        X := 0.0;
        Y := 3.0;
        P := @CDisplayText[0];

        FFontEngine.Hinting := False;
        FFontEngine.SetHeight(40.0);

        while P^ <> 0 do
        begin
          Glyph := FFontCacheManager.Glyph(P^);

          if Glyph <> nil then
          begin
            if X > TransCurve.TotalLength1 then
              Break;

            FFontCacheManager.AddKerning(@X, @Y);
            FFontCacheManager.InitEmbeddedAdaptors(Glyph, X, Y);

            if Glyph.DataType = gdOutline then
            begin
              Rasterizer.Reset;
              Rasterizer.AddPath(ConvTransform);

              RendererScanLine.SetColor(CRgba8Black);

              RenderScanLines(Rasterizer, ScanLine, RendererScanLine);
            end;

            // increment pen position
            X := X + Glyph.AdvanceX;
            Y := Y + Glyph.AdvanceY;
          end;

          Inc(PtrComp(P), SizeOf(Int8u));
        end;

      end
      else
        DisplayMessage('Please copy file timesi.ttf to the current directory'#13
          + 'or download it from http://www.antigrain.com/timesi.zip');

      // Render the path curve
      Stroke[0] := TAggConvStroke.Create(BSpline[0]);
      Stroke[1] := TAggConvStroke.Create(BSpline[1]);

      Stroke[0].Width := 2.0;
      Stroke[1].Width := 2.0;

      Rgba.FromRgbaInteger(170, 50, 20, 100);
      RendererScanLine.SetColor(@Rgba);

      Rasterizer.AddPath(Stroke[0]);
      RenderScanLines(Rasterizer, ScanLine, RendererScanLine);

      Rasterizer.AddPath(Stroke[1]);
      RenderScanLines(Rasterizer, ScanLine, RendererScanLine);

      // Render the "poly" tool
      Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.2);
      RendererScanLine.SetColor(@Rgba);

      Rasterizer.AddPath(FInteractivePolygon[0]);
      RenderScanLines(Rasterizer, ScanLine, RendererScanLine);

      Rasterizer.AddPath(FInteractivePolygon[1]);
      RenderScanLines(Rasterizer, ScanLine, RendererScanLine);

      // Render the controls
      RenderControl(Rasterizer, ScanLine, RendererScanLine,
        FCheckBoxFixedLength);
      RenderControl(Rasterizer, ScanLine, RendererScanLine,
        FCheckBoxPreserveXScale);
      RenderControl(Rasterizer, ScanLine, RendererScanLine, FCheckBoxAnimate);
      RenderControl(Rasterizer, ScanLine, RendererScanLine, FSliderNumPoints);

      // Free AGG resources
      ScanLine.Free;
      Rasterizer.Free;

      BSpline[0].Free;
      BSpline[1].Free;
      TransCurve.Free;
      ConvCurves.Free;
      ConvSegm.Free;

      ConvTransform.Free;

      Path[0].Free;
      Path[1].Free;
      Stroke[0].Free;
      Stroke[1].Free;
    finally
      RendererScanLine.Free;
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
    if FInteractivePolygon[0].OnMouseMove(X, Y) then
      ForceRedraw;

    if FInteractivePolygon[1].OnMouseMove(X, Y) then
      ForceRedraw;
  end;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    if FInteractivePolygon[0].OnMouseButtonDown(X, Y) then
      ForceRedraw;

    if FInteractivePolygon[1].OnMouseButtonDown(X, Y) then
      ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FInteractivePolygon[0].OnMouseButtonUp(X, Y) then
    ForceRedraw;

  if FInteractivePolygon[1].OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Similar to the "TransCurve1" demo, but here the '
      + 'transformer operates with two arbitrary curves. It requires more '
      + 'calculations, but gives you more freedom. In other words you will '
      + 'see :-).'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

procedure TAggApplication.OnControlChange;
var
  Index: Integer;
begin
  if FCheckBoxAnimate.Status <> FPrevAnimate then
  begin
    if FCheckBoxAnimate.Status then
    begin
      OnInit;

      for Index := 0 to 5 do
      begin
        FDelta[0][Index].X := 10 * Random - 5;
        FDelta[0][Index].Y := 10 * Random - 5;
        FDelta[1][Index].X := 10 * Random - 5;
        FDelta[1][Index].Y := 10 * Random - 5;
      end;

      WaitMode := False;
    end
    else
      WaitMode := True;

    FPrevAnimate := FCheckBoxAnimate.Status;
  end;
end;

procedure TAggApplication.MovePoint(X, Y, Dx, Dy: PDouble);
begin
  if X^ < 0.0 then
  begin
    X^ := 0.0;
    Dx^ := -Dx^;
  end;

  if X^ > Width then
  begin
    X^ := Width;
    Dx^ := -Dx^;
  end;

  if Y^ < 0.0 then
  begin
    Y^ := 0.0;
    Dy^ := -Dy^;
  end;

  if Y^ > Height then
  begin
    Y^ := Height;
    Dy^ := -Dy^;
  end;

  X^ := X^ + Dx^;
  Y^ := Y^ + Dy^;
end;

procedure TAggApplication.NormalizePoint;
var
  D: Double;
begin
  D := CalculateDistance(FInteractivePolygon[0].Point[Index],
    FInteractivePolygon[1].Point[Index]);

  // 28.8 is 20 * sqrt(2)
  if D > 28.28 then
  begin
    FInteractivePolygon[1].Xn[Index] := FInteractivePolygon[0].Xn[Index] +
      (FInteractivePolygon[1].Xn[Index] - FInteractivePolygon[0].Xn[Index]) *
      28.28 / D;

    FInteractivePolygon[1].Yn[Index] := FInteractivePolygon[0].Yn[Index] +
      (FInteractivePolygon[1].Yn[Index] - FInteractivePolygon[0].Yn[Index]) *
      28.28 / D;
  end;
end;

procedure TAggApplication.OnIdle;
var
  Index: Integer;
begin
  for Index := 0 to 5 do
  begin
    MovePoint(FInteractivePolygon[0].XnPtr[Index],
      FInteractivePolygon[0].YnPtr[Index], @FDelta[0][Index].X,
      @FDelta[0][Index].Y);
    MovePoint(FInteractivePolygon[1].XnPtr[Index],
      FInteractivePolygon[1].YnPtr[Index], @FDelta[1][Index].X,
      @FDelta[1][Index].Y);

    NormalizePoint(Index);
  end;

  ForceRedraw;
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Non-linear "Along-A-Curve" ' +
      'Transformer - FreeType (F1-Help)';

    if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
