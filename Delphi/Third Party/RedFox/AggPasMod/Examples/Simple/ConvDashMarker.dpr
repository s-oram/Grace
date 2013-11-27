program ConvDashMarker;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvDash in '..\..\Source\AggConvDash.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvContour in '..\..\Source\AggConvContour.pas',
  AggConvSmoothPoly in '..\..\Source\AggConvSmoothPoly.pas',
  AggConvMarker in '..\..\Source\AggConvMarker.pas',
  AggArrowHead in '..\..\Source\AggArrowHead.pas',
  AggVcgenMarkersTerm in '..\..\Source\AggVcgenMarkersTerm.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FX, FY: array [0..2] of Double;
    FDelta: TPointDouble;
    FIndex: Integer;

    FRadioBoxCap: TAggControlRadioBox;
    FSliderWidth, FSliderSmooth: TAggControlSlider;
    FCheckBoxClose, FCheckBoxEvenOdd: TAggControlCheckBox;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

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

  FRadioBoxCap := TAggControlRadioBox.Create(10, 10, 130, 80, not FlipY);
  FSliderWidth := TAggControlSlider.Create(140, 14, 280, 22, not FlipY);
  FSliderSmooth := TAggControlSlider.Create(290, 14, 490, 22, not FlipY);
  FCheckBoxClose := TAggControlCheckBox.Create(140, 30, 'Close Polygons',
    not FlipY);
  FCheckBoxEvenOdd := TAggControlCheckBox.Create(290, 30, 'Even-Odd Fill',
    not FlipY);

  FIndex := -1;

  FX[0] := 57 + 100;
  FY[0] := 60;
  FX[1] := 369 + 100;
  FY[1] := 170;
  FX[2] := 143 + 100;
  FY[2] := 310;

  AddControl(FRadioBoxCap);

  FRadioBoxCap.AddItem('Butt Cap');
  FRadioBoxCap.AddItem('Square Cap');
  FRadioBoxCap.AddItem('Round Cap');
  FRadioBoxCap.SetCurrentItem(0);
  FRadioBoxCap.NoTransform;

  AddControl(FSliderWidth);

  FSliderWidth.SetRange(0.0, 10.0);
  FSliderWidth.Value := 3.0;
  FSliderWidth.Caption := 'Width=%1.2f';
  FSliderWidth.NoTransform;

  AddControl(FSliderSmooth);

  FSliderSmooth.SetRange(0.0, 2.0);
  FSliderSmooth.Value := 1.0;
  FSliderSmooth.Caption := 'Smooth=%1.2f';
  FSliderSmooth.NoTransform;

  AddControl(FCheckBoxClose);

  FCheckBoxClose.NoTransform;

  AddControl(FCheckBoxEvenOdd);

  FCheckBoxEvenOdd.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxCap.Free;
  FSliderWidth.Free;
  FSliderSmooth.Free;
  FCheckBoxClose.Free;
  FCheckBoxEvenOdd.Free;

  inherited
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;
  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Rasterizer: TAggRasterizerScanLineAA;
  ScanLine: TAggScanLineUnpacked8;

  Cap: TAggLineCap;
  Path: TAggPathStorage;

  Smooth: TAggConvSmoothPoly;

  SmoothOutline: TAggConvStroke;

  Curve: TAggConvCurve;
  Dash: TAggConvDash;
  Stroke: TAggConvStroke;
  Arrow: TAggConvMarker;
  Marker: TAggVcgenMarkersTerm;

  K : Double;
  Ah: TAggArrowHead;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Rasterizer := TAggRasterizerScanLineAA.Create;
      ScanLine := TAggScanLineUnpacked8.Create;

      // Render
      Cap := lcButt;

      if FRadioBoxCap.GetCurrentItem = 1 then
        Cap := lcSquare;

      if FRadioBoxCap.GetCurrentItem = 2 then
        Cap := lcRound;

      Path := TAggPathStorage.Create;

      Path.MoveTo(FX[0], FY[0]);
      Path.LineTo(FX[1], FY[1]);
      Path.LineTo((FX[0] + FX[1] + FX[2]) / 3.0,
        (FY[0] + FY[1] + FY[2]) / 3.0);
      Path.LineTo(FX[2], FY[2]);

      if FCheckBoxClose.Status then
        Path.ClosePolygon;

      Path.MoveTo((FX[0] + FX[1]) * 0.5, (FY[0] + FY[1]) * 0.5);
      Path.LineTo((FX[1] + FX[2]) * 0.5, (FY[1] + FY[2]) * 0.5);
      Path.LineTo((FX[2] + FX[0]) * 0.5, (FY[2] + FY[0]) * 0.5);

      if FCheckBoxClose.Status then
        Path.ClosePolygon;

      if FCheckBoxEvenOdd.Status then
        Rasterizer.FillingRule := frEvenOdd;

      // (1)
      Rasterizer.AddPath(Path);
      Rgba.FromRgbaDouble(0.7, 0.5, 0.1, 0.5);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Rasterizer, ScanLine, RenScan);

      // Start of (2, 3, 4)
      Smooth := TAggConvSmoothPoly.Create(Path);
      Smooth.SmoothValue := FSliderSmooth.Value;

      // (2)
      Rasterizer.AddPath(Smooth);
      Rgba.FromRgbaDouble(0.1, 0.5, 0.7, 0.1);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Rasterizer, ScanLine, RenScan);

      // (3)
      SmoothOutline := TAggConvStroke.Create(Smooth);

      Rasterizer.AddPath(SmoothOutline);
      Rgba.FromRgbaDouble(0.0, 0.6, 0.0, 0.8);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Rasterizer, ScanLine, RenScan);

      // (4)
      Curve := TAggConvCurve.Create(Smooth);
      Dash := TAggConvDash.Create(Curve);

      Marker := TAggVcgenMarkersTerm.Create;
      Dash.Markers := Marker;

      Stroke := TAggConvStroke.Create(Dash);
      Stroke.LineCap := Cap;
      Stroke.Width := FSliderWidth.Value;

      K := Power(FSliderWidth.Value, 0.7);

      Ah := TAggArrowHead.Create;
      try
        Ah.SetHead(4 * K, 4 * K, 3 * K, 2 * K);

        if not FCheckBoxClose.Status then
          Ah.SetTail(1 * K, 1.5 * K, 3 * K, 5 * K);

        Arrow := TAggConvMarker.Create(Dash.Markers, Ah);
        try
          Dash.AddDash(20.0, 5.0);
          Dash.AddDash(5.0, 5.0);
          Dash.AddDash(5.0, 5.0);
          Dash.DashStart := 10;

          Rasterizer.AddPath(Stroke);
          Rasterizer.AddPath(Arrow);
        finally
          Arrow.Free;
        end;
      finally
        Ah.Free
      end;
      RenScan.SetColor(CRgba8Black);
      RenderScanLines(Rasterizer, ScanLine, RenScan);

      // Render the controls
      Rasterizer.FillingRule := frNonZero;

      RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxCap);
      RenderControl(Rasterizer, ScanLine, RenScan, FSliderWidth);
      RenderControl(Rasterizer, ScanLine, RenScan, FSliderSmooth);
      RenderControl(Rasterizer, ScanLine, RenScan, FCheckBoxClose);
      RenderControl(Rasterizer, ScanLine, RenScan, FCheckBoxEvenOdd);

      // Free AGG resources
      Rasterizer.Free;
      ScanLine.Free;

      Path.Free;
      Smooth.Free;
      SmoothOutline.Free;
      Curve.Free;
      Dash.Free;
      Marker.Free;
      Stroke.Free;
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
  Delta: TPointDouble;
begin
  if mkfMouseLeft in Flags then
  begin
    if FIndex = 3 then
    begin
      Delta.X := X - FDelta.X;
      Delta.Y := Y - FDelta.Y;

      FX[1] := FX[1] - (FX[0] - Delta.X);
      FY[1] := FY[1] - (FY[0] - Delta.Y);
      FX[2] := FX[2] - (FX[0] - Delta.X);
      FY[2] := FY[2] - (FY[0] - Delta.Y);

      FX[0] := Delta.X;
      FY[0] := Delta.Y;

      ForceRedraw;

      Exit;
    end;

    if FIndex >= 0 then
    begin
      FX[FIndex] := X - FDelta.X;
      FY[FIndex] := Y - FDelta.Y;

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
begin
  if mkfMouseLeft in Flags then
  begin
    I := 0;

    while I < 3 do
    begin
      if Sqrt((X - FX[I]) * (X - FX[I]) + (Y - FY[I]) * (Y - FY[I])) < 20.0
      then
      begin
        FDelta.X := X - FX[I];
        FDelta.Y := Y - FY[I];
        FIndex := I;

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
        FIndex := 3;
      end;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FIndex := -1;
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
    DisplayMessage('The example demonstrates rather a complex pipeline that '
      + 'consists of diffrerent converters, particularly, of the dash '
      + 'generator, marker generator, and of course, the stroke converter. '
      + 'There is also a converter that allows you to draw smooth curves '
      + 'based on polygons, see "Interpolation with Bezier Curves" on '
      + 'www.antigrain.com. '#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse in the corners of the triangle to move the '
      + 'particular vertices.'#13
      + 'Drag and move the whole picture with mouse or arrow keys.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Line Join (F1-Help)';

    if Init(500, 330, []) then
      Run;
  finally
    Free;
  end;
end.
