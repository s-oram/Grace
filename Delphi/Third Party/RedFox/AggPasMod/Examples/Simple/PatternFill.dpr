program PatternFill;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$DEFINE AGG_BGR24 }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvSmoothPoly in '..\..\Source\AggConvSmoothPoly.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanPattern in '..\..\Source\AggSpanPattern.pas',
  AggSpanPatternRgba in '..\..\Source\AggSpanPatternRgba.pas'

{$I Pixel_Formats.inc}
{$Q-}
{$R-}
const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderPolygonAngle: TAggControlSlider;
    FSliderPolygonScale: TAggControlSlider;
    FSliderPatternAngle: TAggControlSlider;
    FSliderPatternAlpha: TAggControlSlider;
    FSliderPatternSize: TAggControlSlider;

    FCheckBoxRotatePolygon: TAggControlCheckBox;
    FCheckBoxRotatePattern: TAggControlCheckBox;
    FCheckBoxTiePattern: TAggControlCheckBox;

    FPolygonCenter, FDelta: TPointDouble;

    FFlag: Integer;

    FPattern: PInt8u;
    FPointAloc: Cardinal;

    FPatternRenderingBuffer: TAggRenderingBuffer;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;
    FPathStorage: TAggPathStorage;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure BuildStar(Xc, Yc, R1, R2: Double; N: Cardinal;
      StartAngle: Double = 2.0);
    procedure GeneratePattern;

    procedure OnDraw; override;
    procedure OnInit; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnControlChange; override;
    procedure OnIdle; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSliderPolygonAngle := TAggControlSlider.Create(5, 5, 145, 12, not FlipY);
  FSliderPolygonScale := TAggControlSlider.Create(5, 19, 145, 26, not FlipY);
  FSliderPatternAngle := TAggControlSlider.Create(155, 5, 300, 12, not FlipY);
  FSliderPatternSize := TAggControlSlider.Create(155, 19, 300, 26, not FlipY);
  FSliderPatternAlpha := TAggControlSlider.Create(310, 5, 460, 12, not FlipY);
  FCheckBoxRotatePolygon := TAggControlCheckBox.Create(5, 33, 'Rotate Polygon',
    not FlipY);
  FCheckBoxRotatePattern := TAggControlCheckBox.Create(5, 47, 'Rotate Pattern',
    not FlipY);
  FCheckBoxTiePattern := TAggControlCheckBox.Create(155, 33,
    'Tie pattern to polygon', not FlipY);

  FPatternRenderingBuffer := TAggRenderingBuffer.Create;

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;
  FPathStorage := TAggPathStorage.Create;

  FFlag := 0;
  FPattern := nil;
  FPointAloc := 0;

  AddControl(FSliderPolygonAngle);
  AddControl(FSliderPolygonScale);
  AddControl(FSliderPatternAngle);
  AddControl(FSliderPatternSize);
  AddControl(FSliderPatternAlpha);
  AddControl(FCheckBoxRotatePolygon);
  AddControl(FCheckBoxRotatePattern);
  AddControl(FCheckBoxTiePattern);

  FSliderPolygonAngle.Caption := 'Polygon Angle=%3.2f';
  FSliderPolygonAngle.SetRange(-180, 180);

  FSliderPolygonScale.Caption := 'Polygon Scale=%3.2f';
  FSliderPolygonScale.SetRange(0.1, 5);
  FSliderPolygonScale.Value := 1.0;

  FSliderPatternAngle.Caption := 'Pattern Angle=%3.2f';
  FSliderPatternAngle.SetRange(-180, 180);

  FSliderPatternSize.Caption := 'Pattern Size=%3.2f';
  FSliderPatternSize.SetRange(10, 40);
  FSliderPatternSize.Value := 30;

  FSliderPatternAlpha.Caption := 'Background Alpha=%.2f';
  FSliderPatternAlpha.Value := 0.1;
end;

destructor TAggApplication.Destroy;
begin
  FSliderPolygonAngle.Free;
  FSliderPolygonScale.Free;
  FSliderPatternAngle.Free;
  FSliderPatternSize.Free;
  FSliderPatternAlpha.Free;
  FCheckBoxRotatePolygon.Free;
  FCheckBoxRotatePattern.Free;
  FCheckBoxTiePattern.Free;

  FPatternRenderingBuffer.Free;

  FRasterizer.Free;
  FScanLine.Free;
  FPathStorage.Free;

  AggFreeMem(Pointer(FPattern), FPointAloc);

  inherited;
end;

procedure TAggApplication.BuildStar(Xc, Yc, R1, R2: Double; N: Cardinal;
  StartAngle: Double = 2.0);
var
  A: Double;
  Delta: TPointDouble;
  Index: Cardinal;
begin
  FPathStorage.RemoveAll;

  StartAngle := Deg2Rad(StartAngle);

  Index := 0;

  while Index < N do
  begin
    A := Pi * 2.0 * Index / N - Pi * 0.5;
    SinCos(A + StartAngle, Delta.Y, Delta.X);

    if Index and 1 <> 0 then
      FPathStorage.LineTo(Xc + Delta.X * R1, Yc + Delta.Y * R1)
    else if Index <> 0 then
      FPathStorage.LineTo(Xc + Delta.X * R2, Yc + Delta.Y * R2)
    else
      FPathStorage.MoveTo(Xc + Delta.X * R2, Yc + Delta.Y * R2);

    Inc(Index);
  end;

  FPathStorage.ClosePolygon;
end;

procedure TAggApplication.GeneratePattern;
var
  Size: Cardinal;
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  Smooth: TAggConvSmoothPolyCurve;
  Stroke: TAggConvStroke;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
begin
  Size := Trunc(FSliderPatternSize.Value);

  BuildStar(FSliderPatternSize.Value * 0.5, FSliderPatternSize.Value * 0.5,
    FSliderPatternSize.Value * 0.4, FSliderPatternSize.Value * 0.166666, 6,
    FSliderPatternAngle.Value);

  Smooth := TAggConvSmoothPolyCurve.Create(FPathStorage);
  Stroke := TAggConvStroke.Create(Smooth);

  Smooth.SmoothValue := 0.0;
  Smooth.ApproximationScale := 4.0;

  Stroke.Width := FSliderPatternSize.Value / 15.0;

  AggFreeMem(Pointer(FPattern), FPointAloc);

  FPointAloc := 4 * Sqr(Size) * SizeOf(Int8u);

  AggGetMem(Pointer(FPattern), FPointAloc);

  FPatternRenderingBuffer.Attach(FPattern, Size, Size, 4 * Size);

  PixelFormatRgba32(Pixf, FPatternRenderingBuffer);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);

    Rgba.FromRgbaDouble(0.4, 0.0, 0.1, FSliderPatternAlpha.Value);
    // Pattern background color
    RendererBase.Clear(@Rgba);

    FRasterizer.AddPath(Smooth);
    Rgba.FromRgbaInteger(110, 130, 50);
    RenScan.SetColor(@Rgba);
    RenderScanLines(FRasterizer, FScanLine, RenScan);

    FRasterizer.AddPath(Stroke);
    Rgba.FromRgbaInteger(0, 50, 80);
    RenScan.SetColor(@Rgba);
    RenderScanLines(FRasterizer, FScanLine, RenScan);

    Smooth.Free;
    Stroke.Free;
    RenScan.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnDraw;
var
  W, H: Double;

  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScanSolid: TAggRendererScanLineAASolid;

  PolygonMatrix: TAggTransAffine;

  R: Double;

  OffsetX, OffsetY: Cardinal;

  Tr: TAggConvTransform;
  Wx, Wy: TAggWrapModeReflectAutoPow2;
  SpanAllocator: TAggSpanAllocator;
  Sg: TAggSpanPatternRgba;
  RenScan: TAggRendererScanLineAA;
begin
  // Initialize structures
  W := RenderingBufferWindow.Width;
  H := RenderingBufferWindow.Height;

  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScanSolid := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Render
      PolygonMatrix := TAggTransAffine.Create;

      PolygonMatrix.Translate(-FPolygonCenter.X, -FPolygonCenter.Y);
      PolygonMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));
      PolygonMatrix.Scale(FSliderPolygonScale.Value);
      PolygonMatrix.Translate(FPolygonCenter.X, FPolygonCenter.Y);

      R := FInitialWidth / 3.0 - 8.0;

      BuildStar(FPolygonCenter.X, FPolygonCenter.Y, R, R / 1.45, 14);

      Tr := TAggConvTransform.Create(FPathStorage, PolygonMatrix);

      OffsetX := 0;
      OffsetY := 0;

      if FCheckBoxTiePattern.Status then
      begin
        OffsetX := Trunc(W - FPolygonCenter.X);
        OffsetY := Trunc(H - FPolygonCenter.Y);
      end;

      SpanAllocator := TAggSpanAllocator.Create;
      Wx := TAggWrapModeReflectAutoPow2.Create;
      Wy := TAggWrapModeReflectAutoPow2.Create;
      Sg := TAggSpanPatternRgba.Create(SpanAllocator, FPatternRenderingBuffer,
        OffsetX, OffsetY, Wx, Wy, CAggOrderRgba);
      RenScan := TAggRendererScanLineAA.Create(RendererBase, Sg);

      {
        FRasterizer.SetClipBox(-1, 0, W, H);
        FRasterizer.MoveToDouble(-1, 100);
        FRasterizer.LineToDouble(100, 100);
        FRasterizer.LineToDouble(100, 200);
        FRasterizer.LineToDouble(-1, 200);
        FRasterizer.ClosePolygon;{
      }

      FRasterizer.AddPath(Tr);
      RenScanSolid.SetColor(CRgba8Black);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Render the controls
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FSliderPolygonAngle);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FSliderPolygonScale);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FSliderPatternAngle);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FSliderPatternSize);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FSliderPatternAlpha);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FCheckBoxRotatePolygon);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FCheckBoxRotatePattern);
      RenderControl(FRasterizer, FScanLine, RenScanSolid, FCheckBoxTiePattern);

      // Free AGG resources
      SpanAllocator.Free;
      PolygonMatrix.Free;
      Sg.Free;
      Tr.Free;
      RenScan.Free;
    finally
      RenScanSolid.Free;
    end;
  finally
    Wx.Free;
    Wy.Free;
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnInit;
begin
  inherited;
  FPolygonCenter := PointDouble(FInitialWidth * 0.5, FInitialHeight * 0.5);
  GeneratePattern;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FFlag <> 0 then
    begin
      FPolygonCenter.X := X - FDelta.X;
      FPolygonCenter.Y := Y - FDelta.Y;

      ForceRedraw;
    end
    else
  else
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  PolygonMatrix: TAggTransAffine;

  R, V: Double;
  Tr: TAggConvTransform;
begin
  if mkfMouseLeft in Flags then
  begin
    PolygonMatrix := TAggTransAffine.Create;

    PolygonMatrix.Translate(-FPolygonCenter.X, -FPolygonCenter.Y);
    PolygonMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));
    PolygonMatrix.Scale(FSliderPolygonScale.Value);
    PolygonMatrix.Translate(FPolygonCenter.X, FPolygonCenter.Y);

    R := FInitialWidth / 3.0 - 8.0;

    BuildStar(FPolygonCenter.X, FPolygonCenter.Y, R, R / 1.45, 14);

    Tr := TAggConvTransform.Create(FPathStorage, PolygonMatrix);
    try
      FRasterizer.AddPath(Tr);
    finally
      Tr.Free;
    end;

    if FRasterizer.HitTest(X, Y) then
    begin
      FDelta.X := X - FPolygonCenter.X;
      FDelta.Y := Y - FPolygonCenter.Y;

      FFlag := 1;
    end;

    PolygonMatrix.Free;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FFlag := 0;
end;

procedure TAggApplication.OnControlChange;
begin
  WaitMode := not (FCheckBoxRotatePolygon.Status or
    FCheckBoxRotatePattern.Status);

  GeneratePattern;
  ForceRedraw;
end;

procedure TAggApplication.OnIdle;
var
  Redraw: Boolean;
begin
  Redraw := False;
  if FCheckBoxRotatePolygon.Status then
  begin
    FSliderPolygonAngle.Value := FSliderPolygonAngle.Value + 0.5;

    if FSliderPolygonAngle.Value >= 180.0 then
      FSliderPolygonAngle.Value := FSliderPolygonAngle.Value - 360.0;

    Redraw := True;
  end;

  if FCheckBoxRotatePattern.Status then
  begin
    FSliderPatternAngle.Value := FSliderPatternAngle.Value - 0.5;

    if FSliderPatternAngle.Value <= -180.0 then
      FSliderPatternAngle.Value := FSliderPatternAngle.Value + 360.0;

    GeneratePattern;

    Redraw := True;
  end;

  if Redraw then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('The example demonstrates how to use arbitrary images as '
      + 'fill patterns. This Span generator is very simple, so, it doesn''t '
      + 'alLow you to apply arbitrary transformations to the pattern, i.e., it '
      + 'cannot be used as a texturing tool. But it works pretty fast and can '
      + 'be useful in some applications.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to move the polygon around.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Pattern Filling (F1-Help)';

    if Init(640, 480, []) then
      Run;
  finally
    Free;
  end;
end.
