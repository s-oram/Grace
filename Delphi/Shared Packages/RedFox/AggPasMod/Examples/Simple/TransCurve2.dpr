program TransCurve2;

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
  Windows,

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
  AggFontWin32TrueType in '..\..\Source\AggFontWin32TrueType.pas',
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
    FFontEngine: TAggFontEngineWin32TrueTypeInt16;
    FFontCacheManager: TAggFontCacheManager;
    FInteractivePolygon: array [0..1] of TInteractivePolygon;

    FSliderNumPoints: TAggControlSlider;
    FCheckBoxFixedLength: TAggControlCheckBox;
    FCheckBoxPreserveXScale: TAggControlCheckBox;
    FCheckBoxAnimate: TAggControlCheckBox;

    FDelta: array [0..1, 0..5] of TPointDouble;

    FPrevAnimate: Boolean;
  public
    constructor Create(Dc: HDC; PixelFormat: TPixelFormat; FlipY: Boolean);
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

    procedure MovePoint(X, Y: PDouble; var Delta: TPointDouble);
    procedure NormalizePoint(I: Cardinal);

    procedure OnIdle; override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(Dc: HDC; PixelFormat: TPixelFormat;
  FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FFontEngine := TAggFontEngineWin32TrueTypeInt16.Create(Dc);
  FFontCacheManager := TAggFontCacheManager.Create(FFontEngine);
  FInteractivePolygon[0] := TInteractivePolygon.Create(6, 5);
  FInteractivePolygon[1] := TInteractivePolygon.Create(6, 5);

  FSliderNumPoints := TAggControlSlider.Create(5, 5, 340, 12, not FlipY);
  FCheckBoxFixedLength := TAggControlCheckBox.Create(350, 5, 'Fixed Length',
    not FlipY);
  FCheckBoxPreserveXScale := TAggControlCheckBox.Create(465, 5,
    'Preserve X scale', not FlipY);
  FCheckBoxAnimate := TAggControlCheckBox.Create(350, 25, 'Animate', not FlipY);

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
  FInteractivePolygon[0].Xn[0] := 10 + 50;
  FInteractivePolygon[0].Yn[0] := -10 + 50;
  FInteractivePolygon[0].Xn[1] := 10 + 150 + 20;
  FInteractivePolygon[0].Yn[1] := -10 + 150 - 20;
  FInteractivePolygon[0].Xn[2] := 10 + 250 - 20;
  FInteractivePolygon[0].Yn[2] := -10 + 250 + 20;
  FInteractivePolygon[0].Xn[3] := 10 + 350 + 20;
  FInteractivePolygon[0].Yn[3] := -10 + 350 - 20;
  FInteractivePolygon[0].Xn[4] := 10 + 450 - 20;
  FInteractivePolygon[0].Yn[4] := -10 + 450 + 20;
  FInteractivePolygon[0].Xn[5] := 10 + 550;
  FInteractivePolygon[0].Yn[5] := -10 + 550;

  FInteractivePolygon[1].Xn[0] := -10 + 50;
  FInteractivePolygon[1].Yn[0] := 10 + 50;
  FInteractivePolygon[1].Xn[1] := -10 + 150 + 20;
  FInteractivePolygon[1].Yn[1] := 10 + 150 - 20;
  FInteractivePolygon[1].Xn[2] := -10 + 250 - 20;
  FInteractivePolygon[1].Yn[2] := 10 + 250 + 20;
  FInteractivePolygon[1].Xn[3] := -10 + 350 + 20;
  FInteractivePolygon[1].Yn[3] := 10 + 350 - 20;
  FInteractivePolygon[1].Xn[4] := -10 + 450 - 20;
  FInteractivePolygon[1].Yn[4] := 10 + 450 + 20;
  FInteractivePolygon[1].Xn[5] := -10 + 550;
  FInteractivePolygon[1].Yn[5] := 10 + 550;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Sl: TAggScanLinePacked8;

  Ras: TAggRasterizerScanLineAA;

  Path: array [0..1] of TSimplePolygonVertexSource;

  Bspline: array [0..1] of TAggConvBSpline;

  TransCurve: TAggTransDoublePath;
  ConvCurves: TAggConvCurve;
  ConvSegm  : TAggConvSegmentator;
  ConvTrans : TAggConvTransform;

  X, Y: Double;

  P: PInt8u;

  Glyph: PAggGlyphCache;

  Stroke: array [0..1] of TAggConvStroke;

begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    Sl := TAggScanLinePacked8.Create;
    Ras := TAggRasterizerScanLineAA.Create;

    // Render the text
    Path[0] := TSimplePolygonVertexSource.Create(FInteractivePolygon[0].Polygon,
      FInteractivePolygon[0].NumPoints, False, False);
    Path[1] := TSimplePolygonVertexSource.Create(FInteractivePolygon[1].Polygon,
    FInteractivePolygon[1].NumPoints, False, False);

    Bspline[0] := TAggConvBSpline.Create(Path[0]);
    Bspline[1] := TAggConvBSpline.Create(Path[1]);

    Bspline[0].InterpolationStep := (1.0 / FSliderNumPoints.Value);
    Bspline[1].InterpolationStep := (1.0 / FSliderNumPoints.Value);

    TransCurve := TAggTransDoublePath.Create;
    ConvCurves := TAggConvCurve.Create(FFontCacheManager.PathAdaptor);
    ConvSegm := TAggConvSegmentator.Create(ConvCurves);
    ConvTrans := TAggConvTransform.Create(ConvSegm, TransCurve);

    TransCurve.PreserveXScale := FCheckBoxPreserveXScale.Status;

    if FCheckBoxFixedLength.Status then
      TransCurve.BaseLength := 1140.0;

    TransCurve.BaseHeight := 30.0;
    TransCurve.AddPaths(Bspline[0], Bspline[1]);

    ConvSegm.ApproximationScale := 3.0;
    ConvCurves.ApproximationScale := 5.0;

    FFontEngine.Height := 40.0;
    FFontEngine.Hinting := False;
    FFontEngine.Italic := True;

    if FFontEngine.CreateFont('Times New Roman', grOutline) then
    begin
      X := 0.0;
      Y := 3.0;
      P := @CDisplayText[0];

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
            Ras.Reset;
            Ras.AddPath(ConvTrans);

            RenScan.SetColor(CRgba8Black);

            RenderScanLines(Ras, Sl, RenScan);
          end;

          // increment pen position
          X := X + Glyph.AdvanceX;
          Y := Y + Glyph.AdvanceY;
        end;

        Inc(PtrComp(P), SizeOf(Int8u));
      end;
    end;

    // Render the path curve
    Stroke[0] := TAggConvStroke.Create(Bspline[0]);
    Stroke[1] := TAggConvStroke.Create(Bspline[1]);

    Stroke[0].Width := 2.0;
    Stroke[1].Width := 2.0;

    Rgba.FromRgbaInteger(170, 50, 20, 100);
    RenScan.SetColor(@Rgba);

    Ras.AddPath(Stroke[0]);
    RenderScanLines(Ras, Sl, RenScan);

    Ras.AddPath(Stroke[1]);
    RenderScanLines(Ras, Sl, RenScan);

    // Render the "poly" tool
    Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.2);
    RenScan.SetColor(@Rgba);

    Ras.AddPath(FInteractivePolygon[0]);
    RenderScanLines(Ras, Sl, RenScan);

    Ras.AddPath(FInteractivePolygon[1]);
    RenderScanLines(Ras, Sl, RenScan);

    // Render the controls
    RenderControl(Ras, Sl, RenScan, FCheckBoxFixedLength);
    RenderControl(Ras, Sl, RenScan, FCheckBoxPreserveXScale);
    RenderControl(Ras, Sl, RenScan, FCheckBoxAnimate);
    RenderControl(Ras, Sl, RenScan, FSliderNumPoints);

    // Free AGG resources
    Sl.Free;
    Ras.Free;
    RenScan.Free;

    Bspline[0].Free;
    Bspline[1].Free;
    TransCurve.Free;
    ConvCurves.Free;
    ConvSegm.Free;
    ConvTrans.Free;

    Path[0].Free;
    Path[1].Free;

    Stroke[0].Free;
    Stroke[1].Free;
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
  I: Integer;
begin
  if FCheckBoxAnimate.Status <> FPrevAnimate then
  begin
    if FCheckBoxAnimate.Status then
    begin
      OnInit;

      for I := 0 to 5 do
      begin
        FDelta[0][I].X := 10 * Random - 5;
        FDelta[0][I].Y := 10 * Random - 5;
        FDelta[1][I].X := 10 * Random - 5;
        FDelta[1][I].Y := 10 * Random - 5;
      end;

      WaitMode := False;
    end
    else
      WaitMode := True;

    FPrevAnimate := FCheckBoxAnimate.Status;
  end;
end;

procedure TAggApplication.MovePoint(X, Y: PDouble; var Delta: TPointDouble);
begin
  if X^ < 0.0 then
  begin
    X^ := 0.0;
    Delta.X := -Delta.X;
  end;

  if X^ > Width then
  begin
    X^ := Width;
    Delta.X := -Delta.X;
  end;

  if Y^ < 0.0 then
  begin
    Y^ := 0.0;
    Delta.Y := -Delta.Y;
  end;

  if Y^ > Height then
  begin
    Y^ := Height;
    Delta.Y := -Delta.Y;
  end;

  X^ := X^ + Delta.X;
  Y^ := Y^ + Delta.Y;
end;

procedure TAggApplication.NormalizePoint(I: Cardinal);
var
  D: Double;
begin
  D := CalculateDistance(FInteractivePolygon[0].Point[I],
    FInteractivePolygon[1].Point[I]);

  // 28.8 is 20 * sqrt(2)
  if D > 28.28 then
  begin
    D := 28.28 / D;

    FInteractivePolygon[1].Xn[I] := FInteractivePolygon[0].Xn[I] +
      (FInteractivePolygon[1].Xn[I] - FInteractivePolygon[0].Xn[I]) * D;

    FInteractivePolygon[1].Yn[I] := FInteractivePolygon[0].Yn[I] +
      (FInteractivePolygon[1].Yn[I] - FInteractivePolygon[0].Yn[I]) * D;
  end;
end;

procedure TAggApplication.OnIdle;
var
  I: Integer;
begin
  for I := 0 to 5 do
  begin
    MovePoint(FInteractivePolygon[0].XnPtr[I], FInteractivePolygon[0].YnPtr[I],
      FDelta[0][I]);
    MovePoint(FInteractivePolygon[1].XnPtr[I], FInteractivePolygon[1].YnPtr[I],
      FDelta[1][I]);

    NormalizePoint(I);
  end;

  ForceRedraw;
end;

var
  Dc : HDC;
begin
  Dc := GetDC(0);

  with TAggApplication.Create(Dc, CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Non-linear "Along-A-Curve" ' +
      'Transformer - Win32 (F1-Help)';

    if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;

  ReleaseDC(0, Dc);
end.
