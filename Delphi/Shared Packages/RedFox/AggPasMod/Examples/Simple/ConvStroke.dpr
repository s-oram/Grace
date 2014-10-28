program ConvStroke;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

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

    FRadioBoxJoin, FRadioBoxCap: TAggControlRadioBox;

    FSliderWidth, FSliderMiterLimit: TAggControlSlider;
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

  FIndex := -1;

  FX[0] := 57 + 100;
  FY[0] := 60;
  FX[1] := 369 + 100;
  FY[1] := 170;
  FX[2] := 143 + 100;
  FY[2] := 310;

  FRadioBoxJoin := TAggControlRadioBox.Create(10, 10, 133, 80, not FlipY);
  AddControl(FRadioBoxJoin);
  FRadioBoxJoin.SetTextSize(7.5);
  FRadioBoxJoin.TextThickness := 1.0;
  FRadioBoxJoin.AddItem('Miter Join');
  FRadioBoxJoin.AddItem('Miter Join Revert');
  FRadioBoxJoin.AddItem('Round Join');
  FRadioBoxJoin.AddItem('Bevel Join');
  FRadioBoxJoin.SetCurrentItem(2);


  FRadioBoxCap := TAggControlRadioBox.Create(10, 90, 133, 160, not FlipY);
  AddControl(FRadioBoxCap);
  FRadioBoxCap.AddItem('Butt Cap');
  FRadioBoxCap.AddItem('Square Cap');
  FRadioBoxCap.AddItem('Round Cap');
  FRadioBoxCap.SetCurrentItem(2);

  FSliderWidth := TAggControlSlider.Create(140, 14, 490, 22, not FlipY);
  AddControl(FSliderWidth);
  FSliderWidth.SetRange(3.0, 40.0);
  FSliderWidth.Value := 20.0;
  FSliderWidth.Caption := 'Width=%1.2f';

  FSliderMiterLimit := TAggControlSlider.Create(140, 34, 490, 42, not FlipY);
  AddControl(FSliderMiterLimit);
  FSliderMiterLimit.SetRange(1.0, 10.0);
  FSliderMiterLimit.Value := 4.0;
  FSliderMiterLimit.Caption := 'Miter Limit=%1.2f';

  FRadioBoxJoin.NoTransform;
  FRadioBoxCap.NoTransform;
  FSliderWidth.NoTransform;
  FSliderMiterLimit.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxJoin.Free;
  FRadioBoxCap.Free;
  FSliderWidth.Free;
  FSliderMiterLimit.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;
  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl  : TAggScanLineUnpacked8;

  Cap: TAggLineCap;
  Join: TAggLineJoin;
  Path: TAggPathStorage;

  Stroke, Poly1, Poly2: TAggConvStroke;
  Poly2Dash: TAggConvDash;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLineUnpacked8.Create;

      // Render
      Path := TAggPathStorage.Create;

      Path.MoveTo(FX[0], FY[0]);
      Path.LineTo((FX[0] + FX[1]) * 0.5, (FY[0] + FY[1]) * 0.5);
      Path.LineTo(FX[1], FY[1]);
      Path.LineTo(FX[2], FY[2]);
      Path.LineTo(FX[2], FY[2]); // numerical stability

      Path.MoveTo((FX[0] + FX[1]) * 0.5, (FY[0] + FY[1]) * 0.5);
      Path.LineTo((FX[1] + FX[2]) * 0.5, (FY[1] + FY[2]) * 0.5);
      Path.LineTo((FX[2] + FX[0]) * 0.5, (FY[2] + FY[0]) * 0.5);
      Path.ClosePolygon;

      Cap := lcButt;

      if FRadioBoxCap.GetCurrentItem = 1 then
        Cap := lcSquare;

      if FRadioBoxCap.GetCurrentItem = 2 then
        Cap := lcRound;

      Join := ljMiter;

      if FRadioBoxJoin.GetCurrentItem = 1 then
        Join := ljMiterRevert;

      if FRadioBoxJoin.GetCurrentItem = 2 then
        Join := ljRound;

      if FRadioBoxJoin.GetCurrentItem = 3 then
        Join := ljBevel;

      // (1)
      Stroke := TAggConvStroke.Create(Path);
      Stroke.LineJoin := Join;
      Stroke.LineCap := Cap;
      Stroke.MiterLimit := FSliderMiterLimit.Value;
      Stroke.Width := FSliderWidth.Value;

      Ras.AddPath(Stroke);
      Rgba.FromRgbaDouble(0.8, 0.7, 0.6);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Ras, Sl, RenScan);

      // (2)
      Poly1 := TAggConvStroke.Create(Path);
      Poly1.Width := 1.5;
      Ras.AddPath(Poly1);

      RenScan.SetColor(CRgba8Black);
      RenderScanLines(Ras, Sl, RenScan);

      // (3)
      Poly2Dash := TAggConvDash.Create(Stroke);
      Poly2 := TAggConvStroke.Create(Poly2Dash);
      Poly2.MiterLimit := 4.0;
      Poly2.Width := FSliderWidth.Value / 5.0;
      Poly2.LineCap := Cap;
      Poly2.LineJoin := Join;
      Poly2Dash.AddDash(20.0, FSliderWidth.Value / 2.5);

      Ras.AddPath(Poly2);
      Rgba.FromRgbaDouble(0, 0, 0.3);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Ras, Sl, RenScan);

      // (4)
      Ras.AddPath(Path);
      Rgba.FromRgbaDouble(0.0, 0.0, 0.0, 0.2);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Ras, Sl, RenScan);

      // Render the controls
      RenderControl(Ras, Sl, RenScan, FRadioBoxJoin);
      RenderControl(Ras, Sl, RenScan, FRadioBoxCap);
      RenderControl(Ras, Sl, RenScan, FSliderWidth);
      RenderControl(Ras, Sl, RenScan, FSliderMiterLimit);

      // Free AGG resources
      Ras.Free;
      Sl.Free;

      Path.Free;
      Stroke.Free;
      Poly1.Free;
      Poly2Dash.Free;
      Poly2.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
var
  Dx, Dy: Double;
begin
  if mkfMouseLeft in Flags then
  begin
    if FIndex = 3 then
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
    DisplayMessage('Another example that demonstrates the power of the custom '
      + 'pipeline concept. First, we calculate a thick outline (stroke), then '
      + 'generate dashes, and then, calculate the outlines (strokes) of the '
      + 'dashes again.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse in the corners of the triangle to move the '
      + 'particular vertices. Drag and move the whole picture with mouse or '
      + 'arrow keys.'#13#13
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
