program ConvContour;

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
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvContour in '..\..\Source\AggConvContour.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxClose: TAggControlRadioBox;
    FSliderWidth: TAggControlSlider;
    FCheckBoxAutoDetect: TAggControlCheckBox;
    FPathStorage: TAggPathStorage;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure ComposePath;
    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxClose := TAggControlRadioBox.Create(10, 10, 130, 80, not FlipY);
  FSliderWidth := TAggControlSlider.Create(140, 14, 430, 22, not FlipY);
  FCheckBoxAutoDetect := TAggControlCheckBox.Create(140, 30, 'Autodetect '
    + 'orientation if not defined', not FlipY);
  FPathStorage := TAggPathStorage.Create;

  AddControl(FRadioBoxClose);

  FRadioBoxClose.AddItem('Close');
  FRadioBoxClose.AddItem('Close CW');
  FRadioBoxClose.AddItem('Close CCW');
  FRadioBoxClose.SetCurrentItem(0);

  AddControl(FSliderWidth);

  FSliderWidth.SetRange(-100.0, 100.0);
  FSliderWidth.Value := 0.0;
  FSliderWidth.Caption := 'Width=%1.2f';

  AddControl(FCheckBoxAutoDetect);
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxClose.Free;
  FSliderWidth.Free;
  FCheckBoxAutoDetect.Free;
  FPathStorage.Free;

  inherited;
end;

procedure TAggApplication.ComposePath;
var
  Flag: Cardinal;
begin
  Flag := 0;

  if FRadioBoxClose.GetCurrentItem = 1 then
    Flag := CAggPathFlagsCw;

  if FRadioBoxClose.GetCurrentItem = 2 then
    Flag := CAggPathFlagsCcw;

  FPathStorage.RemoveAll;

  FPathStorage.MoveTo(28.47, 6.45);
  FPathStorage.Curve3(21.58, 1.12, 19.82, 0.29);
  FPathStorage.Curve3(17.19, -0.93, 14.21, -0.93);
  FPathStorage.Curve3(9.57, -0.93, 6.57, 2.25);
  FPathStorage.Curve3(3.56, 5.42, 3.56, 10.60);
  FPathStorage.Curve3(3.56, 13.87, 5.03, 16.26);
  FPathStorage.Curve3(7.03, 19.58, 11.99, 22.51);
  FPathStorage.Curve3(16.94, 25.44, 28.47, 29.64);
  FPathStorage.LineTo(28.47, 31.40);
  FPathStorage.Curve3(28.47, 38.09, 26.34, 40.58);
  FPathStorage.Curve3(24.22, 43.07, 20.17, 43.07);
  FPathStorage.Curve3(17.09, 43.07, 15.28, 41.41);
  FPathStorage.Curve3(13.43, 39.75, 13.43, 37.60);
  FPathStorage.LineTo(13.53, 34.77);
  FPathStorage.Curve3(13.53, 32.52, 12.38, 31.30);
  FPathStorage.Curve3(11.23, 30.08, 9.38, 30.08);
  FPathStorage.Curve3(7.57, 30.08, 6.42, 31.35);
  FPathStorage.Curve3(5.27, 32.62, 5.27, 34.81);
  FPathStorage.Curve3(5.27, 39.01, 9.57, 42.53);
  FPathStorage.Curve3(13.87, 46.04, 21.63, 46.04);
  FPathStorage.Curve3(27.59, 46.04, 31.40, 44.04);
  FPathStorage.Curve3(34.28, 42.53, 35.64, 39.31);
  FPathStorage.Curve3(36.52, 37.21, 36.52, 30.71);
  FPathStorage.LineTo(36.52, 15.53);
  FPathStorage.Curve3(36.52, 9.13, 36.77, 7.69);
  FPathStorage.Curve3(37.01, 6.25, 37.57, 5.76);
  FPathStorage.Curve3(38.13, 5.27, 38.87, 5.27);
  FPathStorage.Curve3(39.65, 5.27, 40.23, 5.62);
  FPathStorage.Curve3(41.26, 6.25, 44.19, 9.18);
  FPathStorage.LineTo(44.19, 6.45);
  FPathStorage.Curve3(38.72, -0.88, 33.74, -0.88);
  FPathStorage.Curve3(31.35, -0.88, 29.93, 0.78);
  FPathStorage.Curve3(28.52, 2.44, 28.47, 6.45);

  FPathStorage.ClosePolygon(Flag);

  FPathStorage.MoveTo(28.47, 9.62);
  FPathStorage.LineTo(28.47, 26.66);
  FPathStorage.Curve3(21.09, 23.73, 18.95, 22.51);
  FPathStorage.Curve3(15.09, 20.36, 13.43, 18.02);
  FPathStorage.Curve3(11.77, 15.67, 11.77, 12.89);
  FPathStorage.Curve3(11.77, 9.38, 13.87, 7.06);
  FPathStorage.Curve3(15.97, 4.74, 18.70, 4.74);
  FPathStorage.Curve3(22.41, 4.74, 28.47, 9.62);

  FPathStorage.ClosePolygon(Flag);
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Ras: TAggRasterizerScanLineAA;
  ScanLine : TAggScanLinePacked8;

  Mtx: TAggTransAffine;

  Trans: TAggConvTransform;
  Curve: TAggConvCurve;

  Contour: TAggConvContour;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Ras := TAggRasterizerScanLineAA.Create;
      ScanLine := TAggScanLinePacked8.Create;

      // Render
      Mtx := TAggTransAffine.Create;

      Mtx.Scale(4.0);
      Mtx.Translate(150, 100);

      Trans := TAggConvTransform.Create(FPathStorage, Mtx);
      try
        Curve := TAggConvCurve.Create(Trans);
        try
          Contour := TAggConvContour.Create(Curve);
          try
            Contour.Width := FSliderWidth.Value;
            // contour.SetLineJoin(MiterJoin );
            // contour.SetInnerJoin(MiterJoin );
            // contour.SetInnerMiterLimit(4.0);
            Contour.AutoDetectOrientation := FCheckBoxAutoDetect.Status;

            ComposePath;
            Ras.AddPath(Contour);

            RenScan.SetColor(CRgba8Black);

            RenderScanLines(Ras, ScanLine, RenScan);

            // Render the controls
            RenderControl(Ras, ScanLine, RenScan, FRadioBoxClose);
            RenderControl(Ras, ScanLine, RenScan, FSliderWidth);
            RenderControl(Ras, ScanLine, RenScan, FCheckBoxAutoDetect);

            // Free AGG resources
            Mtx.Free;
            Ras.Free;
            ScanLine.Free;
          finally
            Contour.Free;
          end;
        finally
          Curve.Free;
        end;
      finally
        Trans.Free;
      end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('One of the converters in AGG is TAggConvContour. It allows '
      + 'you to extend or shrink polygons. Initially, it was implemented to '
      + 'eliminate the "problem of adjacent edges" in the SVG Viewer, but it '
      + 'can be very useful in many other applications, for example, to change '
      + 'the font weight on the fly. The trick here is that the sign (dilation '
      + 'or shrinking) depends on the vertex order - clockwise or '
      + 'counterclockwise. '#13
      + 'In the TAggConvContour you can control the behavior. Sometimes you need '
      + 'to preserve the dilation regardless of the initial orientation, '
      + 'sometimes it should depend on the orientation. The glyph ‘a’ has a '
      + '"hole" whose orientation differs from the main contour. To change '
      + 'the "weight" correctly, you need to keep the orientation as it is '
      + 'originally defined. If you turn "Autodetect orientation…" on, the '
      + 'glyph will be extended or shrinked incorrectly.'#13#13
      + 'How to play with:'#13#13
      + 'The radio buttons control the orientation flad assigned to all '
      + 'polygons. "Close" doesn''t add the flag.'#13
      + '"Close CW" and "Close CCW" add "clockwise" or "counterclockwise" '
      + 'flag respectively.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Contour Tool & Polygon Orientation (F1-Help)';

    if Init(440, 330, []) then
      Run;
  finally
    Free;
  end;
end.
