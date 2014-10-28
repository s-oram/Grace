program RasterText;

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

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggEmbeddedRasterFonts in '..\..\Source\AggEmbeddedRasterFonts.pas',
  AggGlyphRasterBin in '..\..\Source\AggGlyphRasterBin.pas',
  AggRendererRasterText in '..\..\Source\AggRendererRasterText.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas';

const
  CFlipY = True;

type
  TAggGradientSineRepeatAdaptor = class(TAggCustomGradient)
  private
    FGradient: TAggCustomGradient;
    FPeriods: Double;
  public
    constructor Create(GF: TAggCustomGradient);

    procedure SetPeriods(P: Double);

    function Calculate(X, Y, D: Integer): Integer; override;
  end;

  TAggApplication = class(TPlatformSupport)
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggGradientSineRepeatAdaptor }

constructor TAggGradientSineRepeatAdaptor.Create(GF: TAggCustomGradient);
begin
  inherited Create;
  FGradient := GF;
  FPeriods := Pi * 2.0;
end;

procedure TAggGradientSineRepeatAdaptor.SetPeriods(P: Double);
begin
  FPeriods := P * Pi * 2.0;
end;

function TAggGradientSineRepeatAdaptor.Calculate(X, Y, D: Integer): Integer;
begin
  Result := Trunc((1.0 + Sin(FGradient.Calculate(X, Y, D) * FPeriods / D))
    * D * 0.5);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);
end;

procedure TAggApplication.OnDraw;
type
  TFontRecord = record
    Font: PInt8u;
    Name: PAnsiChar;
  end;

const
  Fonts: array [0..34] of TFontRecord = (
    (Font: @CAggGse4x6; name: 'Gse4x6'),
    (Font: @CAggGse4x8; name: 'Gse4x8'),
    (Font: @CAggGse5x7; name: 'Gse5x7'),
    (Font: @CAggGse5x9; name: 'Gse5x9'),
    (Font: @CAggGse6x9; name: 'Gse6x9'),
    (Font: @CAggGse6x12; name: 'Gse6x12'),
    (Font: @CAggGse7x11; name: 'Gse7x11'),
    (Font: @CAggGse7x11Bold; name: 'Gse7x11Bold'),
    (Font: @CAggGse7x15; name: 'Gse7x15'),
    (Font: @CAggGse7x15Bold; name: 'Gse7x15Bold'),
    (Font: @CAggGse8x16; name: 'Gse8x16'),
    (Font: @CAggGse8x16Bold; name: 'Gse8x16Bold'),
    (Font: @CAggMcs11Prop; name: 'Mcs11Prop'),
    (Font: @CAggMcs11PropCondensed; name: 'Mcs11PropCondensed'),
    (Font: @CAggMcs12Prop; name: 'Mcs12Prop'),
    (Font: @CAggMcs13Prop; name: 'Mcs13Prop'),
    (Font: @CAggMcs5x10Mono; name: 'Mcs5x10Mono'),
    (Font: @CAggMcs5x11Mono; name: 'Mcs5x11Mono'),
    (Font: @CAggMcs6x10Mono; name: 'Mcs6x10Mono'),
    (Font: @CAggMcs6x11Mono; name: 'Mcs6x11Mono'),
    (Font: @CAggMcs7x12MonoHigh; name: 'Mcs7x12MonoHigh'),
    (Font: @CAggMcs7x12MonoLow; name: 'Mcs7x12MonoLow'),
    (Font: @CAggVerdana12; name: 'Verdana12'),
    (Font: @CAggVerdana12Bold; name: 'Verdana12Bold'),
    (Font: @CAggVerdana13; name: 'Verdana13'),
    (Font: @CAggVerdana13Bold; name: 'Verdana13Bold'),
    (Font: @CAggVerdana14; name: 'Verdana14'),
    (Font: @CAggVerdana14Bold; name: 'Verdana14Bold'),
    (Font: @CAggVerdana16; name: 'Verdana16'),
    (Font: @CAggVerdana16Bold; name: 'Verdana16Bold'),
    (Font: @CAggVerdana17; name: 'Verdana17'),
    (Font: @CAggVerdana17Bold; name: 'Verdana17Bold'),
    (Font: @CAggVerdana18; name: 'Verdana18'),
    (Font: @CAggVerdana18Bold; name: 'Verdana18Bold'),
    (Font: nil; name: nil));

var
  Pixf: TAggPixelFormatProcessor;
  Rgba, Rgbb: TAggColor;

  RendererBase: TAggRendererBase;
  Rt: TAggRendererRasterHorizontalTextSolid;

  Glyph: TAggGlyphRasterBin;

  I: Integer;
  Y: Double;

  Buf: string[100];
  Mtx: TAggTransAffine;

  GradFunc : TAggGradientSineRepeatAdaptor;
  GradCircle: TAggGradientCircle;
  ColorFunc: TAggGradientLinearColor;

  SpanInterpolator: TAggSpanInterpolatorLinear;

  SpanAllocator: TAggSpanAllocator;
  Sg : TAggSpanGradient;
  Ren: TAggRendererScanLineAA;
  Rt2: TAggRendererRasterHorizontalText;
begin
  // Initialize structures
  Glyph := TAggGlyphRasterBin.Create(nil);

  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RendererBase.Clear(CRgba8White);

    Rt := TAggRendererRasterHorizontalTextSolid.Create(RendererBase, Glyph);
    try
      Y := 5;

      Rt.SetColor(CRgba8Black);

      // Render all raster fonts
      I := 0;

      while Fonts[I].Font <> nil do
      begin
        Buf := 'A quick brown fox jumps over the lazy dog 0123456789: ' +
          Fonts[I].Name + #0;

        Glyph.SetFont(Fonts[I].Font);
        Rt.RenderText(5, Y, @Buf[1], not CFlipY);

        Y := Y + Glyph.Height + 1;

        Inc(I);
      end;
    finally
      Rt.Free;
    end;

    // Render Gradient font
    GradCircle := TAggGradientCircle.Create;
    GradFunc := TAggGradientSineRepeatAdaptor.Create(GradCircle);
    GradFunc.SetPeriods(5);

    Rgba.FromRgbaDouble(1.0, 0, 0);
    Rgbb.FromRgbaDouble(0, 0.5, 0);
    ColorFunc := TAggGradientLinearColor.Create(@Rgba, @Rgbb);

    Mtx := TAggTransAffine.Create;
    SpanInterpolator := TAggSpanInterpolatorLinear.Create(Mtx);

    SpanAllocator := TAggSpanAllocator.Create;
    Sg := TAggSpanGradient.Create(SpanAllocator, SpanInterpolator, GradFunc,
      ColorFunc, 0, 150.0);
    Ren := TAggRendererScanLineAA.Create(RendererBase, Sg);
    try
      Rt2 := TAggRendererRasterHorizontalText.Create(Ren, Glyph);
      try
        Rt2.RenderText(5, 465, 'RADIAL REPEATING Gradient: A quick brown fox ' +
          'jumps over the lazy dog', not CFlipY);
      finally
        Rt2.Free;
      end;
    finally
      Ren.Free;
    end;

    // Free AGG resources
    GradCircle.Free;
    ColorFunc.Free;
    Mtx.Free;
    Sg.Free;
    SpanInterpolator.Free;
    SpanAllocator.Free;
    GradFunc.Free;
  finally
    RendererBase.Free;
  end;

  Glyph.Free;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Classes that render raster text was added in AGG mostly to '
      + 'prove the concept of the design. They can be used to draw simple '
      + '(aliased) raster text. The example demonstrates how to use text as a '
      + 'custom ScanLine generator together with any Span generator (in this '
      + 'example it''s Gradient filling). The font format is propriatory, but '
      + 'there are some predefined fonts that are shown in the example.'#13#13
      + 'How to play with:'#13#13
      + 'Change the Renderer "rt" to "TAggRendererRasterVerticalTextSolid" in '
      + 'the source code and recompile it, to get the vertical raster font '
      + 'orientation.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Raster Text (F1-Help)';

    if Init(640, 480, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
