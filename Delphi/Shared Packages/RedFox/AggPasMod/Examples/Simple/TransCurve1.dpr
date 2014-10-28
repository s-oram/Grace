program TransCurve1;

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

  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvBSpline in '..\..\Source\AggConvBSpline.pas',
  AggConvSegmentator in '..\..\Source\AggConvSegmentator.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggFontEngine in '..\..\Source\AggFontEngine.pas',
  AggFontWin32TrueType in '..\..\Source\AggFontWin32TrueType.pas',
  AggFontCacheManager in '..\..\Source\AggFontCacheManager.pas',
  AggTransSinglePath in '..\..\Source\AggTransSinglePath.pas',
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
    FInteractivePolygon: TInteractivePolygon;

    FSliderNumPoints: TAggControlSlider;
    FCheckBoxClose: TAggControlCheckBox;
    FCheckBoxPreserveXScale: TAggControlCheckBox;
    FCheckBoxFixedLength: TAggControlCheckBox;
    FCheckBoxAnimate: TAggControlCheckBox;

    FDelta: array [0..5] of TPointDouble;

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

    procedure OnIdle; override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(Dc: HDC; PixelFormat: TPixelFormat;
  FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FFontEngine := TAggFontEngineWin32TrueTypeInt16.Create(Dc);
  FFontCacheManager := TAggFontCacheManager.Create(FFontEngine);
  FInteractivePolygon := TInteractivePolygon.Create(6, 5);

  FSliderNumPoints := TAggControlSlider.Create(5, 5, 340, 12, not FlipY);
  FCheckBoxClose := TAggControlCheckBox.Create(350, 5, 'Close', not FlipY);
  FCheckBoxPreserveXScale := TAggControlCheckBox.Create(460, 5,
    'Preserve X scale', not FlipY);
  FCheckBoxFixedLength := TAggControlCheckBox.Create(350, 25, 'Fixed Length',
    not FlipY);
  FCheckBoxAnimate := TAggControlCheckBox.Create(460, 25, 'Animate', not FlipY);

  FPrevAnimate := False;

  AddControl(FCheckBoxClose);
  AddControl(FCheckBoxPreserveXScale);
  AddControl(FCheckBoxFixedLength);
  AddControl(FCheckBoxAnimate);

  FCheckBoxPreserveXScale.Status := True;
  FCheckBoxFixedLength.Status := True;

  FSliderNumPoints.SetRange(10, 400);
  FSliderNumPoints.Value := 200;
  FSliderNumPoints.Caption := 'Number of intermediate Points = %.3f';

  AddControl(FSliderNumPoints);
end;

destructor TAggApplication.Destroy;
begin
  FInteractivePolygon.Free;

  FSliderNumPoints.Free;
  FCheckBoxClose.Free;
  FCheckBoxPreserveXScale.Free;
  FCheckBoxFixedLength.Free;
  FCheckBoxAnimate.Free;

  FFontEngine.Free;
  FFontCacheManager.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  FInteractivePolygon.Xn[0] := 50;
  FInteractivePolygon.Yn[0] := 50;
  FInteractivePolygon.Xn[1] := 150 + 20;
  FInteractivePolygon.Yn[1] := 150 - 20;
  FInteractivePolygon.Xn[2] := 250 - 20;
  FInteractivePolygon.Yn[2] := 250 + 20;
  FInteractivePolygon.Xn[3] := 350 + 20;
  FInteractivePolygon.Yn[3] := 350 - 20;
  FInteractivePolygon.Xn[4] := 450 - 20;
  FInteractivePolygon.Yn[4] := 450 + 20;
  FInteractivePolygon.Xn[5] := 550;
  FInteractivePolygon.Yn[5] := 550;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Sl: TAggScanLinePacked8;

  Ras: TAggRasterizerScanLineAA;

  Path: TSimplePolygonVertexSource;
  Bspline: TAggConvBSpline;
  TransCurve: TAggTransSinglePath;
  ConvCurves: TAggConvCurve;
  ConvSegm: TAggConvSegmentator;
  ConvTrans: TAggConvTransform;

  X, Y: Double;

  P: PInt8u;

  Glyph: PAggGlyphCache;

  Stroke: TAggConvStroke;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    Sl := TAggScanLinePacked8.Create;
    Ras := TAggRasterizerScanLineAA.Create;

    FInteractivePolygon.Close := FCheckBoxClose.Status;

    // Render the text
    Path := TSimplePolygonVertexSource.Create(FInteractivePolygon.Polygon,
      FInteractivePolygon.NumPoints, False, FCheckBoxClose.Status);
    Bspline := TAggConvBSpline.Create(Path);

    Bspline.InterpolationStep := 1.0 / FSliderNumPoints.Value;

    TransCurve := TAggTransSinglePath.Create;
    TransCurve.AddPath(Bspline);
    TransCurve.PreserveXScale := FCheckBoxPreserveXScale.Status;

    if FCheckBoxFixedLength.Status then
      TransCurve.BaseLength := 1120;

    ConvCurves := TAggConvCurve.Create(FFontCacheManager.PathAdaptor);
    ConvSegm := TAggConvSegmentator.Create(ConvCurves);
    ConvTrans := TAggConvTransform.Create(ConvSegm, TransCurve);

    ConvSegm.ApproximationScale := 3.0;
    ConvCurves.ApproximationScale := 2.0;

    FFontEngine.Height := 40.0;
    // FFontEngine.italic_(true );

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
          if X > TransCurve.TotalLength then
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

        Inc(P);
      end;
    end;

    // Render the path curve
    Stroke := TAggConvStroke.Create(Bspline);
    Stroke.Width := 2.0;

    Rgba.FromRgbaInteger(170, 50, 20, 100);
    RenScan.SetColor(@Rgba);

    Ras.AddPath(Stroke);
    RenderScanLines(Ras, Sl, RenScan);

    // Render the "poly" tool
    Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.3);
    RenScan.SetColor(@Rgba);

    Ras.AddPath(FInteractivePolygon);
    RenderScanLines(Ras, Sl, RenScan);

    // Render the controls
    RenderControl(Ras, Sl, RenScan, FCheckBoxClose);
    RenderControl(Ras, Sl, RenScan, FCheckBoxPreserveXScale);
    RenderControl(Ras, Sl, RenScan, FCheckBoxFixedLength);
    RenderControl(Ras, Sl, RenScan, FCheckBoxAnimate);
    RenderControl(Ras, Sl, RenScan, FSliderNumPoints);

    // Free AGG resources
    Sl.Free;
    Ras.Free;

    Bspline.Free;
    TransCurve.Free;
    ConvCurves.Free;
    ConvSegm.Free;
    ConvTrans.Free;

    Path.Free;
    Stroke.Free;
    RenScan.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FInteractivePolygon.OnMouseMove(X, Y) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FInteractivePolygon.OnMouseButtonDown(X, Y) then
      ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FInteractivePolygon.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This is a "kinda-cool-stuff" demo that performs non-linear '
      + 'transformations and draws vector text along a curve. Note that it''s '
      + 'not just calculating of the glyph angles and positions, they are '
      + 'transformed as if they were elastic. The curve is calculated as a '
      + 'bicubic spline. The option "Preserve X scale" makes the converter '
      + 'distribute all the points uniformly along the curve. If it''s '
      + 'unchechked, the scale will be proportional to the distance between '
      + 'the control points.'#13#13
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
        FDelta[I].X:= 10 * Random - 5;
        FDelta[I].Y:= 10 * Random - 5;
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

procedure TAggApplication.OnIdle;
var
  I: Integer;
begin
  for I := 0 to 5 do
    MovePoint(FInteractivePolygon.XnPtr[I], FInteractivePolygon.YnPtr[I],
      FDelta[I]);

  ForceRedraw;
end;

var
  Dc : HDC;
begin
  Dc := GetDC(0);
  with TAggApplication.Create(Dc, CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Non-linear "Along-A-Curve" Transformer ' +
      '- Win32 (F1-Help)';

    if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
  ReleaseDC(0, Dc);
end.
