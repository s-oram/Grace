program MultiClip;

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
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggArray in '..\..\Source\AggArray.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRendererMarkers in '..\..\Source\AggRendererMarkers.pas',
  AggRendererMultiClip in '..\..\Source\AggRendererMultiClip.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggParseLion
{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggGradientLinearColor = class(TAggPodAutoArray)
  private
    FCenter: array [0..1] of TAggColor;
    C: TAggColor;
  protected
    function GetSize: Cardinal; override;
    function ArrayOperator(I: Cardinal): Pointer; override;
  public
    constructor Create(C1, C2: PAggColor);

    procedure Colors(C1, C2: PAggColor);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;

    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FAngle, FScale: Double;
    FBaseDelta, FSkew: TPointDouble;
    FSliderNumClipBoxes: TAggControlSlider;
  protected
    procedure ParseLion;
    procedure Transform(AWidth, AHeight, X, Y: Double);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

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
  FCenter[0] := C1^;
  FCenter[1] := C2^;
end;

function TAggGradientLinearColor.GetSize;
begin
  Result := 256;
end;

function TAggGradientLinearColor.ArrayOperator;
begin
  C.V := Int8u((((FCenter[1].V - FCenter[0].V) * I) + (FCenter[0].V shl 8)) shr 8);
  C.Rgba8.R := Int8u((((FCenter[1].Rgba8.R - FCenter[0].Rgba8.R) * I) +
    (FCenter[0].Rgba8.R shl 8)) shr 8);
  C.Rgba8.G := Int8u((((FCenter[1].Rgba8.G - FCenter[0].Rgba8.G) * I) +
    (FCenter[0].Rgba8.G shl 8)) shr 8);
  C.Rgba8.B := Int8u((((FCenter[1].Rgba8.B - FCenter[0].Rgba8.B) * I) +
    (FCenter[0].Rgba8.B shl 8)) shr 8);
  C.Rgba8.A := Int8u((((FCenter[1].Rgba8.A - FCenter[0].Rgba8.A) * I) +
    (FCenter[0].Rgba8.A shl 8)) shr 8);

  Result := @C;
end;

procedure TAggGradientLinearColor.Colors;
begin
  FCenter[0] := C1^;
  FCenter[1] := C2^;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  // Rendering
  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;
  FPath := TAggPathStorage.Create;

  FPathCount := 0;

  FBoundingRect.X1 := 0;
  FBoundingRect.Y1 := 0;
  FBoundingRect.X2 := 0;
  FBoundingRect.Y2 := 0;

  FBaseDelta.X := 0;
  FBaseDelta.Y := 0;

  FAngle := 0;
  FScale := 1.0;

  FSkew.X := 0;
  FSkew.Y := 0;

  ParseLion;

  FSliderNumClipBoxes := TAggControlSlider.Create(5, 5, 150, 12, not FlipY);
  FSliderNumClipBoxes.SetRange(2, 10);
  // FSliderNumClipBoxes.SetNumSteps(8 );
  FSliderNumClipBoxes.Caption := 'N=%.2f';
  AddControl(FSliderNumClipBoxes);
  FSliderNumClipBoxes.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FSliderNumClipBoxes.Free;

  FRasterizer.Free;
  FScanLine.Free;
  FPath.Free;

  inherited;
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta.X := FBoundingRect.CenterX;
  FBaseDelta.Y := FBoundingRect.CenterY;
end;

procedure TAggApplication.OnDraw;
var
  I: Cardinal;

  AWidth, AHeight, X, Y, X1, Y1, X2, Y2: Integer;

  Pixf: TAggPixelFormatProcessor;
  RenMul : TAggRendererMultiClip;
  RenScan: TAggRendererScanLineAASolid;

  Rgba, Rgbb: TAggColor;

  Mtx: TAggTransAffine;

  Trans: TAggConvTransform;

  Markers: TAggRendererMarkers;
  W, N: Double;

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

  SpanInterpolator: TAggSpanInterpolatorLinear;
begin
  AWidth := RenderingBufferWindow.Width;
  AHeight := RenderingBufferWindow.Height;

  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RenMul := TAggRendererMultiClip.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RenMul);
    try
      // Transform lion
      Mtx := TAggTransAffine.Create;

      Mtx.Translate(-FBaseDelta.X, -FBaseDelta.Y);
      Mtx.Scale(FScale);
      Mtx.Rotate(FAngle + Pi);
      Mtx.Skew(FSkew.X * 1E-3, FSkew.Y * 1E-3);
      Mtx.Translate(Width * 0.5, Height * 0.5);

      Rgba.White;
      RenMul.Clear(@Rgba);

      // Custom Clip Renderer Base
      RenMul.ResetClipping(False); // Visibility: "false" means "no visible regions"

      N := FSliderNumClipBoxes.Value;
      X := 0;

      while X < N do
      begin
        Y := 0;

        while Y < N do
        begin
          X1 := Trunc(AWidth * X / N);
          Y1 := Trunc(AHeight * Y / N);
          X2 := Trunc(AWidth * (X + 1) / N);
          Y2 := Trunc(AHeight * (Y + 1) / N);

          RenMul.AddClipBox(X1 + 5, Y1 + 5, X2 - 5, Y2 - 5);

          Inc(Y);
        end;

        Inc(X);

      end; { }

      // Render the lion
      Trans := TAggConvTransform.Create(FPath, Mtx);
      RenderAllPaths(FRasterizer, FScanLine, RenScan, Trans, @FColors,
        @FPathIndex, FPathCount); { }

      // The ScanLine Rasterizer allows you to perform clipping to multiple
      // regions "manually", like in the following code, but the "embedded" method
      // shows much better performance.
      { for i:=0 to FPathCount - 1 do
        begin
        FRasterizer.reset;
        FRasterizer.AddPath(trans ,FPathIndex[i ] );

        rs.SetColor(@FColors[i ] );

        n:=FSliderNumClipBoxes.GetValue;
        x:=0;

        while x < n do
        begin
        y:=0;

        while y < n do
        begin
        x1:=trunc(AWidth  * x / n );
        y1:=trunc(AHeight * y / n );
        x2:=trunc(AWidth  * (x + 1 ) / n );
        y2:=trunc(AHeight * (y + 1 ) / n );

        // r should be of type TAggRendererBase
        r.clip_box_(x1 + 5 ,y1 + 5 ,x2 - 5 ,y2 - 5 );

        RenderScanLines(FRasterizer, FScanLine, rs);

        inc(y );
        end;

        inc(x );
        end;

        end;{ }

      // Render random Bresenham lines and markers
      Markers := TAggRendererMarkers.Create(RenMul);

      for I := 0 to 49 do
      begin
        Rgba.FromRgbaInteger(Random($100), Random($100), Random($100),
          Random($100) + $7F);

        Markers.LineColor := Rgba;

        Rgba.FromRgbaInteger(Random($100), Random($100), Random($100),
          (Random($100)) + $7F);

        with Markers do
        begin
          FillColor := Rgba;

          Line(Coord(Random(AWidth)), Coord(Random(AHeight)),
            Coord(Random(AWidth)), Coord(Random(AHeight)));

          Marker(Random(AWidth), Random(AHeight), 5 + Random(10),
            TAggMarker(Random($7FFF) mod (Integer(mePixel) + 1)));
        end;
      end;

      // Render random anti-aliased lines
      W := 5.0;

      Profile := TAggLineProfileAA.Create;
      Profile.SetWidth(W);

      Ren := TAggRendererOutlineAA.Create(RenMul, Profile);
      Ras := TAggRasterizerOutlineAA.Create(Ren);
      Ras.RoundCap := True;

      for I := 0 to 49 do
      begin
        Rgba.FromRgbaInteger(Random($100), Random($100),
          Random($100), Random($100) + $7F);

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
      SpanInterpolator := TAggSpanInterpolatorLinear.Create(Grm);

      Sg := TAggSpanGradient.Create(SpanAllocator, SpanInterpolator, Grf, Grc, 0, 10);
      Rg := TAggRendererScanLineAA.Create(RenMul, Sg);

      for I := 0 to 49 do
      begin
        X := Random(AWidth);
        Y := Random(AHeight);
        W := 5 + Random(10);

        Grm.Reset;
        Grm.Scale(W * 0.1);
        Grm.Translate(X, Y);
        Grm.Invert;

        Rgba.FromRgbaInteger(255, 255, 255, 0);
        Rgbb.FromRgbaInteger(Random($100), Random($100), Random($100), $FF);

        Grc.Colors(@Rgba, @Rgbb);
        Sg.ColorFunction := Grc;

        Circle.Initialize(PointDouble(X, Y), W, 32);

        FRasterizer.AddPath(Circle);
        RenderScanLines(FRasterizer, FScanLine, Rg);
      end;

      // Render the controls
      RenMul.ResetClipping(True); // "true" means "all rendering buffer is visible".

      // Free AGG resources
      Mtx.Free;
      Circle.Free;
      Profile.Free;
      Ras.Free;
      Sg.Free;
      Grm.Free;
      Rg.Free;
      Trans.Free;
      Markers.Free;
      SpanInterpolator.Free;
      SpanAllocator.Free;
      Ren.Free;
      Grf.Free;
      Grc.Free;

      RenderControl(FRasterizer, FScanLine, RenScan, FSliderNumClipBoxes);
    finally
      RenScan.Free;
    end;
  finally
    RenMul.Free;
  end;
end;

procedure TAggApplication.Transform(AWidth, AHeight, X, Y: Double);
begin
  X := X - AWidth * 0.5;
  Y := Y - AHeight * 0.5;

  FAngle := ArcTan2(Y, X);
  FScale := Sqrt(Y * Y + X * X) * 0.01;
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
    FSkew.X := X;
    FSkew.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('A testing example that demonstrates clipping to multiple '
      + 'rectangular regions. It''s a Low-level (pixel) clipping that can be '
      + 'useful to draw images clipped to a complex region with orthogonal '
      + 'boundaries. It can be useful in some window interfaces that use a '
      + 'custom mechanism to draw window content. The example uses all '
      + 'possible rendering mechanisms.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to rotate and resize the lion.'
      + 'Use the right mouse button to skew the lion.' + #13#13
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
