program Blur;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggPolygonControl in '..\..\Source\Controls\AggPolygonControl.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggPixelFormatGray in '..\..\Source\AggPixelFormatGray.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvContour in '..\..\Source\AggConvContour.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransPerspective in '..\..\Source\AggTransPerspective.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggBlur in '..\..\Source\AggBlur.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxMethod: TAggControlRadioBox;
    FSliderRadius: TAggControlSlider;

    FShadowControl: TPolygonControl;

    FCheckBoxChannelRed: TAggControlCheckBox;
    FCheckBoxChannelGreen: TAggControlCheckBox;
    FCheckBoxChannelBlue: TAggControlCheckBox;
    FCheckBoxControlsFirst: TAggControlCheckBox;

    FPath: TAggPathStorage;
    FShape: TAggConvCurve;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;
    FRenderingBuffer: TAggRenderingBuffer;

    FStackBlur: TAggStackBlur;
    FRecursiveBlur: TAggRecursiveBlur;

    FShapeBounds: TRectDouble;
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
var
  Mtx: TAggTransAffine;
  Clr: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxMethod := TAggControlRadioBox.Create(10, 10, 150, 85, not FlipY);
  FSliderRadius := TAggControlSlider.Create(160, 14, 430, 22, not FlipY);

  FShadowControl := TPolygonControl.Create(4);

  FCheckBoxChannelRed := TAggControlCheckBox.Create(10, 95, 'Red', not FlipY);
  FCheckBoxChannelGreen := TAggControlCheckBox.Create(10, 110, 'Green',
    not FlipY);
  FCheckBoxChannelBlue := TAggControlCheckBox.Create(10, 125, 'Blue',
    not FlipY);
  FCheckBoxControlsFirst := TAggControlCheckBox.Create(285, 30,
    'Draw controls first', not FlipY);

  FPath := TAggPathStorage.Create;
  FShape := TAggConvCurve.Create(FPath);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;
  FRenderingBuffer := TAggRenderingBuffer.Create;

  FStackBlur := TAggStackBlur.Create;
  FRecursiveBlur := TAggRecursiveBlur.Create;

  AddControl(FRadioBoxMethod);

  FRadioBoxMethod.SetTextSize(8);
  FRadioBoxMethod.AddItem('Stack Blur - Fast');
  FRadioBoxMethod.AddItem('Stack Blur');
  FRadioBoxMethod.AddItem('Recursive Blur');
  FRadioBoxMethod.AddItem('Channels');
  FRadioBoxMethod.SetCurrentItem(0);
  FRadioBoxMethod.NoTransform;

  AddControl(FSliderRadius);

  FSliderRadius.SetRange(0.0, 40.0);
  FSliderRadius.Value := 15.0;
  FSliderRadius.Caption := 'Blur Radius=%1.2f';
  FSliderRadius.NoTransform;

  AddControl(FShadowControl);

  FShadowControl.InPolygonCheck := True;
  FShadowControl.NoTransform;

  AddControl(FCheckBoxChannelRed);

  FCheckBoxChannelRed.NoTransform;

  AddControl(FCheckBoxChannelGreen);

  FCheckBoxChannelGreen.Status := True;
  FCheckBoxChannelGreen.NoTransform;

  AddControl(FCheckBoxChannelBlue);

  FCheckBoxChannelBlue.NoTransform;

  AddControl(FCheckBoxControlsFirst);

  FCheckBoxControlsFirst.NoTransform;

  FPath.RemoveAll;
  FPath.MoveTo(28.47, 6.45);
  FPath.Curve3(21.58, 1.12, 19.82, 0.29);
  FPath.Curve3(17.19, -0.93, 14.21, -0.93);
  FPath.Curve3(9.57, -0.93, 6.57, 2.25);
  FPath.Curve3(3.56, 5.42, 3.56, 10.60);
  FPath.Curve3(3.56, 13.87, 5.03, 16.26);
  FPath.Curve3(7.03, 19.58, 11.99, 22.51);
  FPath.Curve3(16.94, 25.44, 28.47, 29.64);
  FPath.LineTo(28.47, 31.40);
  FPath.Curve3(28.47, 38.09, 26.34, 40.58);
  FPath.Curve3(24.22, 43.07, 20.17, 43.07);
  FPath.Curve3(17.09, 43.07, 15.28, 41.41);
  FPath.Curve3(13.43, 39.75, 13.43, 37.60);
  FPath.LineTo(13.53, 34.77);
  FPath.Curve3(13.53, 32.52, 12.38, 31.30);
  FPath.Curve3(11.23, 30.08, 9.38, 30.08);
  FPath.Curve3(7.57, 30.08, 6.42, 31.35);
  FPath.Curve3(5.27, 32.62, 5.27, 34.81);
  FPath.Curve3(5.27, 39.01, 9.57, 42.53);
  FPath.Curve3(13.87, 46.04, 21.63, 46.04);
  FPath.Curve3(27.59, 46.04, 31.40, 44.04);
  FPath.Curve3(34.28, 42.53, 35.64, 39.31);
  FPath.Curve3(36.52, 37.21, 36.52, 30.71);
  FPath.LineTo(36.52, 15.53);
  FPath.Curve3(36.52, 9.13, 36.77, 7.69);
  FPath.Curve3(37.01, 6.25, 37.57, 5.76);
  FPath.Curve3(38.13, 5.27, 38.87, 5.27);
  FPath.Curve3(39.65, 5.27, 40.23, 5.62);
  FPath.Curve3(41.26, 6.25, 44.19, 9.18);
  FPath.LineTo(44.19, 6.45);
  FPath.Curve3(38.72, -0.88, 33.74, -0.88);
  FPath.Curve3(31.35, -0.88, 29.93, 0.78);
  FPath.Curve3(28.52, 2.44, 28.47, 6.45);
  FPath.ClosePolygon;

  FPath.MoveTo(28.47, 9.62);
  FPath.LineTo(28.47, 26.66);
  FPath.Curve3(21.09, 23.73, 18.95, 22.51);
  FPath.Curve3(15.09, 20.36, 13.43, 18.02);
  FPath.Curve3(11.77, 15.67, 11.77, 12.89);
  FPath.Curve3(11.77, 9.38, 13.87, 7.06);
  FPath.Curve3(15.97, 4.74, 18.70, 4.74);
  FPath.Curve3(22.41, 4.74, 28.47, 9.62);
  FPath.ClosePolygon;

  Mtx := TAggTransAffine.Create;
  try
    Mtx.Scale(4.0);
    Mtx.Translate(150, 100);

    FPath.Transform(Mtx);
  finally
    Mtx.Free;
  end;

  BoundingRectSingle(FShape, 0, @FShapeBounds.X1, @FShapeBounds.Y1,
    @FShapeBounds.X2, @FShapeBounds.Y2);

  FShadowControl.Xn[0] := FShapeBounds.X1;
  FShadowControl.Yn[0] := FShapeBounds.Y1;
  FShadowControl.Xn[1] := FShapeBounds.X2;
  FShadowControl.Yn[1] := FShapeBounds.Y1;
  FShadowControl.Xn[2] := FShapeBounds.X2;
  FShadowControl.Yn[2] := FShapeBounds.Y2;
  FShadowControl.Xn[3] := FShapeBounds.X1;
  FShadowControl.Yn[3] := FShapeBounds.Y2;

  Clr.FromRgbaDouble(0, 0.3, 0.5, 0.3);
  FShadowControl.LineColor := Clr;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxMethod.Free;
  FSliderRadius.Free;
  FShadowControl.Free;
  FCheckBoxChannelRed.Free;
  FCheckBoxChannelGreen.Free;
  FCheckBoxChannelBlue.Free;
  FCheckBoxControlsFirst.Free;

  FPath.Free;
  FShape.Free;

  FRasterizer.Free;
  FScanLine.Free;
  FRenderingBuffer.Free;

  FStackBlur.Free;
  FRecursiveBlur.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Pixf, Pixf2: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Rgba: TAggColor;

  ShadowPersp: TAggTransPerspective23;
  ShadowTrans: TAggConvTransform;

  Bbox: TRectDouble;

  Tm: Double;

  Txt: TAggGsvText;
  St: TAggConvStroke;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);
      FRasterizer.SetClipBox(0, 0, Width, Height);

      ShadowPersp := TAggTransPerspective23.Create(FShapeBounds,
        PQuadDouble(FShadowControl.Polygon));

      ShadowTrans := TAggConvTransform.Create(FShape, ShadowPersp);

      if FCheckBoxControlsFirst.Status then
      begin
        RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxMethod);
        RenderControl(FRasterizer, FScanLine, RenScan, FSliderRadius);
        RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxChannelRed);
        RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxChannelGreen);
        RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxChannelBlue);
      end;

      // Render shadow
      FRasterizer.AddPath(ShadowTrans);

      Rgba.FromRgbaDouble(0.2, 0.3, 0);
      RenScan.SetColor(@Rgba);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Calculate the bounding box and extend it by the blur radius
      BoundingRectSingle(ShadowTrans, 0, @Bbox.X1, @Bbox.Y1, @Bbox.X2,
        @Bbox.Y2);

      Bbox.X1 := Bbox.X1 - FSliderRadius.Value;
      Bbox.Y1 := Bbox.Y1 - FSliderRadius.Value;
      Bbox.X2 := Bbox.X2 + FSliderRadius.Value;
      Bbox.Y2 := Bbox.Y2 + FSliderRadius.Value;

      if FRadioBoxMethod.GetCurrentItem = 2 then
      begin
        // The recursive blur method represents the true Gussian Blur,
        // with theoretically infinite kernel. The restricted window size
        // results in extra influence of edge pixels. It's impossible to
        // solve correctly, but extending the right and top areas to another
        // radius value produces fair result.
        Bbox.X2 := Bbox.X2 + FSliderRadius.Value;
        Bbox.Y2 := Bbox.Y2 + FSliderRadius.Value;
      end;

      StartTimer;

      if FRadioBoxMethod.GetCurrentItem <> 3 then
      begin
        // Create a new pixel Renderer and attach it to the main one as a child image.
        // It returns true if the attachment suceeded. It fails if the rectangle
        // (bbox) is fully clipped.
        PixelFormatBgr24(Pixf2, FRenderingBuffer);
        try
          if Pixf2.Attach(Pixf, Trunc(Bbox.X1), Trunc(Bbox.Y1), Trunc(Bbox.X2),
            Trunc(Bbox.Y2)) then
            // Blur it
            if FRadioBoxMethod.GetCurrentItem = 0 then
              // Faster, but bore specific.
              // Works only for 8 bits per channel and only with radii <= 254.
              StackBlurRgb24(Pixf2, UnsignedRound(FSliderRadius.Value),
                UnsignedRound(FSliderRadius.Value))

            else if FRadioBoxMethod.GetCurrentItem = 1 then
              // More general method, but 30-40% sLower.
              FStackBlur.Blur(Pixf2, UnsignedRound(FSliderRadius.Value))
            else
              // True Gaussian Blur, 3-5 times sLower than Stack Blur,
              // but still constant time of radius. Very sensitive
              // to precision, doubles are must here.
              FRecursiveBlur.Blur(Pixf2, FSliderRadius.Value);
        finally
          Pixf2.Free;
        end;
      end
      else
      begin
        // Blur separate channels
        if FCheckBoxChannelRed.Status then
        begin
          PixelFormatGray8Bgr24r(Pixf2, FRenderingBuffer);

          if Pixf2.Attach(Pixf, Trunc(Bbox.X1), Trunc(Bbox.Y1), Trunc(Bbox.X2),
            Trunc(Bbox.Y2)) then
            StackBlurGray8(Pixf2, UnsignedRound(FSliderRadius.Value),
              UnsignedRound(FSliderRadius.Value));
          Pixf2.Free;
        end;

        if FCheckBoxChannelGreen.Status then
        begin
          PixelFormatGray8Bgr24g(Pixf2, FRenderingBuffer);

          if Pixf2.Attach(Pixf, Trunc(Bbox.X1), Trunc(Bbox.Y1), Trunc(Bbox.X2),
            Trunc(Bbox.Y2)) then
            StackBlurGray8(Pixf2, UnsignedRound(FSliderRadius.Value),
              UnsignedRound(FSliderRadius.Value));
          Pixf2.Free;
        end;

        if FCheckBoxChannelBlue.Status then
        begin
          PixelFormatGray8Bgr24b(Pixf2, FRenderingBuffer);

          if Pixf2.Attach(Pixf, Trunc(Bbox.X1), Trunc(Bbox.Y1), Trunc(Bbox.X2),
            Trunc(Bbox.Y2)) then
            StackBlurGray8(Pixf2, UnsignedRound(FSliderRadius.Value),
              UnsignedRound(FSliderRadius.Value));
          Pixf2.Free;
        end;
      end;

      Tm := GetElapsedTime;

      RenderControl(FRasterizer, FScanLine, RenScan, FShadowControl);

      // Render the shape itself
      FRasterizer.AddPath(FShape);

      Rgba.FromRgbaDouble(0.6, 0.9, 0.7, 0.8);
      RenScan.SetColor(@Rgba);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      Txt := TAggGsvText.Create;
      try
        Txt.SetSize(10.0);
        St := TAggConvStroke.Create(Txt);
        try
          St.Width := 1.5;
          Txt.SetStartPoint(150.0, 30.0);
          Txt.SetText(Format('%3.2f ms', [Tm]));

          FRasterizer.AddPath(St);
        finally
          St.Free;
        end;
      finally
        Txt.Free;
      end;

      RenScan.SetColor(CRgba8Black);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      // Render the controls
      if not FCheckBoxControlsFirst.Status then
      begin
        RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxMethod);
        RenderControl(FRasterizer, FScanLine, RenScan, FSliderRadius);
        RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxChannelRed);
        RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxChannelGreen);
        RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxChannelBlue);
      end;

      RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxControlsFirst);

      ShadowPersp.Free;
      ShadowTrans.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FShadowControl.OnMouseMove(X, Y, False) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FShadowControl.OnMouseButtonDown(X, Y) then
      ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FShadowControl.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Now you can blur rendered images rather fast!' +
      'There two algorithms are used: Stack Blur by Mario Klingemann' +
      'and Fast Recursive Gaussian Filter. The speed of both methods' +
      'does not depend on the filter radius. Mario''s method works 3-5' +
      'times faster; it doesn''t produce exactly Gaussian response,' +
      'but pretty fair for most practical purposes. The recursive filter' +
      'uses floating point arithmetic and works sLower. But it is true' +
      'Gaussian filter, with theoretically infinite impulse response.' +
      'The radius (actually 2*sigma value) can be fractional and the' +
      'filter produces quite adequate result.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Gaussian and Stack Blur (F1-Help)';

    if Init(440, 330, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
