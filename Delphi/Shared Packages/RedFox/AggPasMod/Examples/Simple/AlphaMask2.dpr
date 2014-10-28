program AlphaMask2;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{-$DEFINE AGG_GRAY8}
{$DEFINE AGG_BGR24}
{-$DEFINE AGG_Rgb24}
{-$DEFINE AGG_BGRA32}
{-$DEFINE AGG_RgbA32}
{-$DEFINE AGG_ARGB32}
{-$DEFINE AGG_ABGR32}
{-$DEFINE AGG_Rgb565}
{-$DEFINE AGG_Rgb555}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggArray in '..\..\Source\AggArray.pas',
{$IFNDEF AGG_GRAY8}
  AggPixelFormatGray in '..\..\Source\AggPixelFormatGray.pas',
{$ENDIF}
  AggPixelFormatAlphaMaskAdaptor in '..\..\Source\AggPixelFormatAlphaMaskAdaptor.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRendererMarkers in '..\..\Source\AggRendererMarkers.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggAlphaMaskUnpacked8 in '..\..\Source\AggAlphaMaskUnpacked8.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggParseLion in 'AggParseLion.pas'

{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggGradientLinearColor = class(TAggPodAutoArray)
  private
    FColor1, FColor2, C: TAggColor;
  protected
    function GetSize: Cardinal; override;
    function ArrayOperator(I: Cardinal): Pointer; override;
  public
    constructor Create(C1, C2: PAggColor);

    procedure Colors(C1, C2: PAggColor);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSliderNumCb: TAggControlSlider;
    FAlphaBuffer: PAnsiChar;
    FAlphaSize: Cardinal;
    FAlphaMask: TAggAlphaMaskNoClipGray8;

    FAlphaMaskRenderingBuffer: TAggRenderingBuffer;
    FPixelFormat: TAggPixelFormatProcessor;
    FSliderValue: Double;
    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;

    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FAngle, FScale: Double;
    FBaseDelta, FSkew: TPointDouble;
  protected
    procedure ParseLion;
    procedure GenerateAlphaMask(Cx, Cy: Integer);
    procedure Transform(AWidth, AHeight, X, Y: Double);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnResize(Width, Height: Integer); override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggGradientLinearColor }

constructor TAggGradientLinearColor.Create;
begin
  FColor1 := C1^;
  FColor2 := C2^;
end;

function TAggGradientLinearColor.GetSize;
begin
  Result := 256;
end;

function TAggGradientLinearColor.ArrayOperator;
begin
  C.V := Int8u((((FColor2.V - FColor1.V) * I) + (FColor1.V shl 8)) shr 8);
  C.Rgba8.R := Int8u((((FColor2.Rgba8.R - FColor1.Rgba8.R) * I) +
    (FColor1.Rgba8.R shl 8)) shr 8);
  C.Rgba8.G := Int8u((((FColor2.Rgba8.G - FColor1.Rgba8.G) * I) +
    (FColor1.Rgba8.G shl 8)) shr 8);
  C.Rgba8.B := Int8u((((FColor2.Rgba8.B - FColor1.Rgba8.B) * I) +
    (FColor1.Rgba8.B shl 8)) shr 8);
  C.Rgba8.A := Int8u((((FColor2.Rgba8.A - FColor1.Rgba8.A) * I) +
    (FColor1.Rgba8.A shl 8)) shr 8);

  Result := @C;
end;

procedure TAggGradientLinearColor.Colors;
begin
  FColor1 := C1^;
  FColor2 := C2^;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FAlphaBuffer := nil;
  FAlphaSize := 0;

  FAlphaMaskRenderingBuffer := TAggRenderingBuffer.Create;
  FAlphaMask := TAggAlphaMaskNoClipGray8.Create(FAlphaMaskRenderingBuffer);

  FSliderValue := 0.0;

  FPath := TAggPathStorage.Create;
  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;
  FPathCount := 0;

  FBoundingRect := RectDouble(0, 0, 0, 0);
  FBaseDelta := PointDouble(0);
  FSkew := PointDouble(0);

  FAngle := 0;
  FScale := 1;

  ParseLion;

  FSliderNumCb := TAggControlSlider.Create(5, 5, 150, 12, not FlipY);
  FSliderNumCb.SetRange(5, 100);
  FSliderNumCb.Value := 10;
  FSliderNumCb.Caption := 'N=%.2f';
  AddControl(FSliderNumCb);
  FSliderNumCb.NoTransform;

  // Initialize structures
  CPixelFormat(FPixelFormat, RenderingBufferWindow);
end;

destructor TAggApplication.Destroy;
begin
  inherited;

  FRasterizer.Free;
  FScanLine.Free;
  FPath.Free;

  FPixelFormat.Free;
  FSliderNumCb.Free;
  FAlphaMask.Free;
  FAlphaMaskRenderingBuffer.Free;

  AggFreeMem(Pointer(FAlphaBuffer), FAlphaSize);
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta.X := FBoundingRect.CenterX;
  FBaseDelta.Y := FBoundingRect.CenterY;
end;

procedure TAggApplication.GenerateAlphaMask(Cx, Cy: Integer);
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Sl: TAggScanLinePacked8;

  Ellipse: TAggEllipse;

  I: Integer;
begin
  AggFreeMem(Pointer(FAlphaBuffer), FAlphaSize);

  FAlphaSize := Cx * Cy;

  AggGetMem(Pointer(FAlphaBuffer), FAlphaSize);

  FAlphaMaskRenderingBuffer.Attach(PInt8u(FAlphaBuffer), Cx, Cy, Cx);

  PixelFormatGray8(Pixf, FAlphaMaskRenderingBuffer);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Sl := TAggScanLinePacked8.Create;
      try
        Rgba.Clear;
        RendererBase.Clear(@Rgba);

        Ellipse := TAggEllipse.Create;
        try
          RandSeed := 1432;

          I := 0;

          while I < FSliderNumCb.Value do
          begin
            Ellipse.Initialize(PointDouble(Random(Cx), Random(Cy)),
              PointDouble(20 + Random(100), 20 + Random(100)), 100);

            FRasterizer.AddPath(Ellipse);

            Rgba.FromValueInteger($7F + Random($80), $7F + Random($80));
            RenScan.SetColor(@Rgba);

            RenderScanLines(FRasterizer, Sl, RenScan);

            Inc(I);
          end;
        finally
          Ellipse.Free;
        end;
      finally
        Sl.Free;
      end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
begin
  GenerateAlphaMask(Width, Height);
end;

procedure TAggApplication.OnDraw;
var
  I: Cardinal;

  AWidth, AHeight, X, Y: Integer;

  PixfAlpha: TAggPixelFormatProcessorAlphaMaskAdaptor;
  Rb, Rs: TAggRendererScanLineAASolid;

  RendererBase: array [0..1] of TAggRendererBase;
  Rgba, Rgbb: TAggColor;

  Mtx: TAggTransAffine;

  Trans: TAggConvTransform;

  Markers: TAggRendererMarkers;
  W: Double;

  Profile: TAggLineProfileAA;

  Ren: TAggRendererOutlineAA;
  Ras: TAggRasterizerOutlineAA;
  Grm: TAggTransAffine;
  Grf: TAggGradientCircle;
  Grc: TAggGradientLinearColor;
  Circle: TAggCircle;

  SpanAllocator: TAggSpanAllocator;
  Sg: TAggSpanGradient;
  Rg: TAggRendererScanLineAA;

  Inter: TAggSpanInterpolatorLinear;
begin
  AWidth := RenderingBufferWindow.Width;
  AHeight := RenderingBufferWindow.Height;

  if FSliderNumCb.Value <> FSliderValue then
  begin
    GenerateAlphaMask(AWidth, AHeight);

    FSliderValue := FSliderNumCb.Value;
  end;

  PixfAlpha := TAggPixelFormatProcessorAlphaMaskAdaptor.Create(FPixelFormat,
    FAlphaMask);
  try
    RendererBase[0] := TAggRendererBase.Create(PixfAlpha);
    RendererBase[1] := TAggRendererBase.Create(FPixelFormat);
    try
      Rs := TAggRendererScanLineAASolid.Create(RendererBase[0]);
      Rb := TAggRendererScanLineAASolid.Create(RendererBase[1]);
      try
        // Transform lion
        Mtx := TAggTransAffine.Create;

        Mtx.Translate(-FBaseDelta.X, -FBaseDelta.Y);
        Mtx.Scale(FScale, FScale);
        Mtx.Rotate(FAngle + Pi);
        Mtx.Skew(FSkew.X * 1E-3, FSkew.Y * 1E-3);
        Mtx.Translate(0.5 * Width, 0.5 * Height);

        Rgba.White;
        RendererBase[1].Clear(@Rgba);

        // Render the lion
        Trans := TAggConvTransform.Create(FPath, Mtx);
        RenderAllPaths(FRasterizer, FScanLine, Rs, Trans, @FColors,
          @FPathIndex, FPathCount);

        // Render random Bresenham lines and markers
        Markers := TAggRendererMarkers.Create(RendererBase[0]);

        for I := 0 to 49 do
        begin
          Rgba.FromRgbaInteger(Random($80), Random($80), Random($80),
            Random($80) + $7F);

          Markers.LineColor := Rgba;

          Rgba.FromRgbaInteger(Random($80), Random($80), Random($80),
            Random($80) + $7F);

          Markers.FillColor := Rgba;

          Markers.Line(Markers.Coord(Random(AWidth)),
            Markers.Coord(Random(AHeight)),
            Markers.Coord(Random(AWidth)),
            Markers.Coord(Random(AHeight)));

          Markers.Marker(Random(AWidth), Random(AHeight), Random(10) + 5,
            TAggMarker(Random(Integer(mePixel) + 1)));
        end;

        // Render random anti-aliased lines
        W := 5.0;

        Profile := TAggLineProfileAA.Create;
        Profile.SetWidth(W);

        Ren := TAggRendererOutlineAA.Create(RendererBase[0], Profile);
        Ras := TAggRasterizerOutlineAA.Create(Ren);
        Ras.RoundCap := True;

        for I := 0 to 49 do
        begin
          Rgba.FromRgbaInteger(Random($80), Random($80), Random($80),
            Random($80) + $7F);

          Ren.SetColor(@Rgba);

          Ras.MoveToDouble(Random(AWidth), Random(AHeight));
          Ras.LineToDouble(Random(AWidth), Random(AHeight));
          Ras.Render(False);
        end;

        // Render random circles with Gradient
        Grm := TAggTransAffine.Create;
        Grf := TAggGradientCircle.Create;
        Rgba.Black;
        Rgbb.Black;
        Grc := TAggGradientLinearColor.Create(@Rgba, @Rgbb);
        Circle := TAggCircle.Create;
        SpanAllocator := TAggSpanAllocator.Create;
        Inter := TAggSpanInterpolatorLinear.Create(Grm);

        Sg := TAggSpanGradient.Create(SpanAllocator, Inter, Grf, Grc, 0, 10);
        Rg := TAggRendererScanLineAA.Create(RendererBase[0], Sg);
        try
          for I := 0 to 49 do
          begin
            X := Random(AWidth);
            Y := Random(AHeight);
            W := Random(10) + 5;

            Grm.Reset;

            Grm.Scale(W / 10.0);
            Grm.Translate(X, Y);

            Grm.Invert;

            Rgba.FromRgbaInteger($FF, $FF, $FF, 0);
            Rgbb.FromRgbaInteger(Random($80), Random($80), Random($80), 255);

            Grc.Colors(@Rgba, @Rgbb);
            Sg.ColorFunction := Grc;

            Circle.Initialize(PointDouble(X, Y), W, 32);

            FRasterizer.AddPath(Circle);
            RenderScanLines(FRasterizer, FScanLine, Rg);
          end;

          // Render the controls
          RenderControl(FRasterizer, FScanLine, Rb, FSliderNumCb);
        finally
          Rg.Free;
          Sg.Free;
        end;

        // Free AGG resources
        Grf.Free;
        Grm.Free;
        Trans.Free;
        Markers.Free;
        Mtx.Free;
        Circle.Free;
        Profile.Free;
        Ren.Free;
        Ras.Free;
        SpanAllocator.Free;
        Inter.Free;
        Grc.Free;
      finally
        Rb.Free;
        Rs.Free;
      end;
    finally
      RendererBase[0].Free;
      RendererBase[1].Free;
    end;
  finally
    PixfAlpha.Free;
  end;
end;

procedure TAggApplication.Transform(AWidth, AHeight, X, Y: Double);
begin
  X := X - AWidth * 0.5;
  Y := Y - AHeight * 0.5;

  FAngle := ArcTan2(Y, X);
  FScale := Hypot(X, Y) * 0.01;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  AWidth, AHeight: Integer;
begin
  if mkfMouseLeft in Flags then
  begin
    AWidth := RenderingBufferWindow.Width;
    AHeight := RenderingBufferWindow.Height;

    Transform(AWidth, AHeight, X, Y);

    ForceRedraw;
  end;

  if mkfMouseRight in Flags then
  begin
    FSkew := PointDouble(X, Y);

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Another example of alpha-masking. In the "alpha_mask" '
      + 'example the alpha-mask is applied to the scan line container with '
      + 'unpacked data (ScanLineUnpacked), while in this one there a special '
      + 'adapter of a pixel format Renderer is used (pixfmtAMaskAdaptor). It '
      + 'allows you to use the alpha-mask with all possible primitives and '
      + 'renderers. Besides, if the alpha-mask buffer is of the same size '
      + 'as the main rendering buffer (usually it is) we don''t have to '
      + 'perform clipping for the alpha-mask, because all the primitives are '
      + 'already clipped at the Higher level, see class AmaskNoClipU8. '#13#13
      + 'How to play with:'#13#13
      + 'Press and drag the left mouse button to scale and rotate the lion '
      + 'and generate a new set of other primitives.'#13
      + 'Change the "N" value to generate a new set of masking TEllipse. '
      + 'Use the right mouse button to skew the lion.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Clipping to multiple rectangle regions (F1-Help)';

    if Init(512, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
