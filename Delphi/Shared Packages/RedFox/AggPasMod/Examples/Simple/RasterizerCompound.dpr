program RasterizerCompound;

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
  AggArray in '..\..\Source\AggArray.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerCompoundAA in '..\..\Source\AggRasterizerCompoundAA.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas';

const
  CFlipY = True;

type
  TStyleHandler = class(TAggCustomStyleHandler)
  private
    FTransparent: TAggColor;

    FStyles: PAggColor;
    FCount : Cardinal;
  public
    constructor Create(Styles: PAggColor; Count: Cardinal);

    function IsSolid(Style: Cardinal): Boolean; override;
    function GetColor(Style: Cardinal): PAggColor; override;

    procedure GenerateSpan(Span: PAggColor; X, Y: Integer;
      Len, Style: Cardinal); override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSliderWidth: TAggControlSlider;
    FSliderAlpha: array [1..4] of TAggControlSlider;
    FCheckBoxInvertOrder: TAggControlCheckBox;
    FPath: TAggPathStorage;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure ComposePath;

    procedure OnDraw; override;
    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ StyleHandler }

constructor TStyleHandler.Create(Styles: PAggColor; Count: Cardinal);
begin
  FTransparent.Clear;

  FStyles := Styles;
  FCount := Count;
end;

function TStyleHandler.IsSolid(Style: Cardinal): Boolean;
begin
  Result := True;
end;

function TStyleHandler.GetColor(Style: Cardinal): PAggColor;
begin
  if Style < FCount then
    Result := PAggColor(PtrComp(FStyles) + Style * SizeOf(TAggColor))
  else
    Result := @FTransparent;
end;

procedure TStyleHandler.GenerateSpan(Span: PAggColor; X, Y: Integer;
  Len, Style: Cardinal);
begin
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSliderWidth := TAggControlSlider.Create(190, 5, 430, 12, not FlipY);
  FSliderAlpha[1] := TAggControlSlider.Create(5, 5, 180, 12, not FlipY);
  FSliderAlpha[2] := TAggControlSlider.Create(5, 25, 180, 32, not FlipY);
  FSliderAlpha[3] := TAggControlSlider.Create(5, 45, 180, 52, not FlipY);
  FSliderAlpha[4] := TAggControlSlider.Create(5, 65, 180, 72, not FlipY);

  FCheckBoxInvertOrder := TAggControlCheckBox.Create(190, 25, 'Invert Z-Order');
  FPath := TAggPathStorage.Create;

  AddControl(FSliderWidth);

  FSliderWidth.SetRange(-20.0, 50.0);
  FSliderWidth.Value := 10.0;
  FSliderWidth.Caption := 'Width=%1.2f';

  AddControl(FSliderAlpha[1]);

  FSliderAlpha[1].SetRange(0, 1);
  FSliderAlpha[1].Value := 1;
  FSliderAlpha[1].Caption := 'Alpha1=%1.3f';

  AddControl(FSliderAlpha[2]);

  FSliderAlpha[2].SetRange(0, 1);
  FSliderAlpha[2].Value := 1;
  FSliderAlpha[2].Caption := 'Alpha2=%1.3f';

  AddControl(FSliderAlpha[3]);

  FSliderAlpha[3].SetRange(0, 1);
  FSliderAlpha[3].Value := 1;
  FSliderAlpha[3].Caption := 'Alpha3=%1.3f';

  AddControl(FSliderAlpha[4]);

  FSliderAlpha[4].SetRange(0, 1);
  FSliderAlpha[4].Value := 1;
  FSliderAlpha[4].Caption := 'Alpha4=%1.3f';

  AddControl(FCheckBoxInvertOrder);
end;

destructor TAggApplication.Destroy;
begin
  FSliderWidth.Free;
  FSliderAlpha[1].Free;
  FSliderAlpha[2].Free;
  FSliderAlpha[3].Free;
  FSliderAlpha[4].Free;

  FCheckBoxInvertOrder.Free;
  FPath.Free;

  inherited;
end;

procedure TAggApplication.ComposePath;
begin
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
end;

procedure TAggApplication.OnDraw;
var
  Pixf, PixfPre: TAggPixelFormatProcessor;

  Rgba, C1, C2: TAggColor;

  RendererBase, RendererBasePre: TAggRendererBase;

  Rs: TAggRendererScanLineAASolid;
  Gr: TAggPodVector;
  I : Cardinal;

  Lut : TAggGammaLut8;
  Ras : TAggRasterizerScanLineAA;
  Rasc: TAggRasterizerCompoundAADouble;

  Sl: TAggScanLineUnpacked8;
  Sh: TStyleHandler;

  SpanAllocator: TAggSpanAllocator;

  Mtx: TAggTransAffine;

  Trans: TAggConvTransform;
  Curve: TAggConvCurve;

  Stroke, StrokeEllipse: TAggConvStrokeMath;

  Styles: array [0..3] of TAggColor;

  Ellipse: TAggEllipse;
begin
  // Initialize structures
  PixelFormatBgra32(Pixf, RenderingBufferWindow);
  PixelFormatBgra32Pre(PixfPre, RenderingBufferWindow);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  RendererBasePre := TAggRendererBase.Create(PixfPre, True);
  try
    Rs := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      // Clear the window with a gradient
      Gr := TAggPodVector.Create(SizeOf(TAggColor), Pixf.Width);
      try
        I := 0;

        while I < Pixf.Width do
        begin
          C1.FromRgbaInteger(255, 255, 0);
          C2.FromRgbaInteger(0, 255, 255);

          Rgba := Gradient(C1, C2, I / Pixf.Width);

          Gr.Add(@Rgba);
          Inc(I);
        end;

        I := 0;

        while I < Pixf.Height do
        begin
          RendererBase.CopyColorHSpan(0, I, Pixf.Width, Gr[0]);

          Inc(I);
        end;

        Lut := TAggGammaLut8.Create(2.0);
        try
          Pixf.ApplyGammaDir(Lut, CAggOrderBgra);

          Ras := TAggRasterizerScanLineAA.Create;
          Rasc := TAggRasterizerCompoundAADouble.Create;
          Sl := TAggScanLineUnpacked8.Create;
          SpanAllocator := TAggSpanAllocator.Create;

          // Draw two triangles
          Ras.MoveToDouble(0, 0);
          Ras.LineToDouble(Width, 0);
          Ras.LineToDouble(Width, Height);

          Rgba.FromRgbaInteger(Lut.Dir[0], Lut.Dir[100], Lut.Dir[0]);

          RenderScanLinesAASolid(Ras, Sl, RendererBase, @Rgba);

          Ras.MoveToDouble(0, 0);
          Ras.LineToDouble(0, Height);
          Ras.LineToDouble(Width, 0);

          Rgba.FromRgbaInteger(Lut.Dir[0], Lut.Dir[100], Lut.Dir[100]);

          RenderScanLinesAASolid(Ras, Sl, RendererBase, @Rgba);

          Mtx := TAggTransAffine.Create;

          Mtx.Scale(4.0);
          Mtx.Translate(150, 100);

          Trans := TAggConvTransform.Create(FPath, Mtx);
          Curve := TAggConvCurve.Create(Trans);
          Stroke := TAggConvStrokeMath.Create(Curve);

          ComposePath;

          if FCheckBoxInvertOrder.Status then
            Rasc.LayerOrder(loInverse)
          else
            Rasc.LayerOrder(loDirect);

          Styles[3].FromRgbaInteger(Lut.Dir[255], Lut.Dir[0], Lut.Dir[108], 200);
          Styles[3].PreMultiply;
          Styles[2].FromRgbaInteger(Lut.Dir[51], Lut.Dir[0], Lut.Dir[151], 180);
          Styles[2].PreMultiply;
          Styles[1].FromRgbaInteger(Lut.Dir[143], Lut.Dir[90], Lut.Dir[6], 200);
          Styles[1].PreMultiply;
          Styles[0].FromRgbaInteger(Lut.Dir[0], Lut.Dir[0], Lut.Dir[255], 220);
          Styles[0].PreMultiply;

          Sh := TStyleHandler.Create(@Styles[0], 4);
          Stroke.Width := FSliderWidth.Value;

          Rasc.Reset;
          Rasc.MasterAlpha(3, FSliderAlpha[1].Value);
          Rasc.MasterAlpha(2, FSliderAlpha[2].Value);
          Rasc.MasterAlpha(1, FSliderAlpha[3].Value);
          Rasc.MasterAlpha(0, FSliderAlpha[4].Value);

          Ellipse := TAggEllipse.Create(220.0, 180.0, 120.0, 10.0, 128, False);
          StrokeEllipse := TAggConvStrokeMath.Create(Ellipse);
          StrokeEllipse.Width := FSliderWidth.Value  * 0.5;

          Rasc.Styles(3, -1);
          Rasc.AddPath(StrokeEllipse);

          Rasc.Styles(2, -1);
          Rasc.AddPath(Ellipse);

          Rasc.Styles(1, -1);
          Rasc.AddPath(Stroke);

          Rasc.Styles(0, -1);
          Rasc.AddPath(Curve);

          RenderScanLinesCompoundLayered(Rasc, Sl, RendererBasePre, SpanAllocator,
            Sh);

          // Render the controls
          RenderControl(Ras, Sl, Rs, FSliderWidth);
          RenderControl(Ras, Sl, Rs, FSliderAlpha[1]);
          RenderControl(Ras, Sl, Rs, FSliderAlpha[2]);
          RenderControl(Ras, Sl, Rs, FSliderAlpha[3]);
          RenderControl(Ras, Sl, Rs, FSliderAlpha[4]);
          RenderControl(Ras, Sl, Rs, FCheckBoxInvertOrder);

          Pixf.ApplyGammaInv(Lut, CAggOrderBgra);

        finally
          Lut.Free;
        end;
        // Free AGG resources
        Mtx.Free;
        Trans.Free;
        Ellipse.Free;

        Ras.Free;
        Rasc.Free;
        Sl.Free;
        SpanAllocator.Free;
        Sh.Free;

        Curve.Free;
        Stroke.Free;
        StrokeEllipse.Free;
      finally
        Gr.Free;
      end;
    finally
      Rs.Free;
    end;
  finally
    RendererBase.Free;
    RendererBasePre.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This simple example demonstrates a rather advanced '
      + 'technique of using the Compound Rasterizer. The idea is you assign '
      + 'styles to the polygons (left=style, right=-1) and rasterize this '
      + '"multi-styled" Compound shape as a whole. If the polygons in the '
      + 'shape overlap, the greater styles have higher priority. That is, the '
      + 'result is as if greater styles were painted last, but the geometry is '
      + 'flattened before rendering. It means there are no pixels will be '
      + 'painted twice. Then the style are associated with colors, Gradients, '
      + 'images, etc. in a special style handler. It simulates Constructive '
      + 'Solid Geometry so that, you can, for example draw a translucent fill '
      + 'plus translucent stroke without the overlapped part of the fill being '
      + 'visible through the stroke.');
end;

begin
  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example. Compound Rasterizer -- Geometry Flattening ' +
      '(F1-Help)';

    if Init(440, 330, []) then
      Run;
  finally
    Free;
  end;
end.
