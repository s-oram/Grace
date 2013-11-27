program TrueTypeTest;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Windows,
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLineBin in '..\..\Source\AggScanLineBin.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggCurves in '..\..\Source\AggCurves.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvContour in '..\..\Source\AggConvContour.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggFontEngine in '..\..\Source\AggFontEngine.pas',
  AggFontWin32TrueType in '..\..\Source\AggFontWin32TrueType.pas',
  AggFontCacheManager in '..\..\Source\AggFontCacheManager.pas';

const
  CFlipY = True;

  CAngleStep = 0.5;

  CDisplayText: PAnsiChar =
  // '0123456789ABCDEFGHIJKLMNOPRSTUVWXYZabcdefghijklmnoprstuvwxyz ' +
    'Anti-Grain Geometry is designed as a set of loosely coupled ' +
    'algorithms and class templates united with a common idea, ' +
    'so that all the components can be easily combined. Also, ' +
    'the template based design allows you to replace any part of ' +
    'the library without the necessity to modify a single byte in ' +
    'the existing code. ' +
    'AGG is designed keeping in mind extensibility and flexibility. ' +
    'Basically I just wanted to create a toolkit that would alLow me ' +
    '(and anyone else) to add new fancy algorithms very easily. ' +
    'AGG does not dictate you any style of its use, you are free to ' +
    'use any part of it. However, AGG is often associated with a tool ' +
    'for rendering images in memory. That is not quite true, but it can ' +
    'be a good starting point in studying. The tutorials describe the ' +
    'use of AGG starting from the Low level functionality that deals with ' +
    'frame buffers and pixels. Then you will gradually understand how to ' +
    'abstract different parts of the library and how to use them separately. ' +
    'Remember, the raster picture is often not the only thing you want to ' +
    'obtain, you will probably want to print your graphics with Highest ' +
    'possible quality and in this case you can easily combine the "vectorial" '
    + 'part of the library with some API like Windows GDI, having a common ' +
    'external interface. If that API can render multi-polygons with non-zero ' +
    'and even-odd filling rules it''s all you need to incorporate AGG into ' +
    'your application. For example, Windows API PolyPolygon perfectly fits ' +
    'these needs, except certain advanced things like gradient filling, ' +
    'Gouraud shading, image transformations, and so on. Or, as an alternative, '
    + 'you can use all AGG algorithms producing High resolution pixel images '
    + 'and then to send the result to the printer as a pixel map.' +
    'BeLow is a typical brief scheme of the AGG rendering pipeline. ' +
    'Please note that any component between the Vertex Source ' +
    'and Screen Output is not mandatory. It all depends on your ' +
    'particular needs. For example, you can use your own Rasterizer, ' +
    'based on Windows API. In this case you won''t need the AGG Rasterizer ' +
    'and Renderers. Or, if you need to draw only lines, you can use the ' +
    'AGG outline Rasterizer that has certain restrictions but works faster. ' +
    'The number of possibilities is endless. ' +
    'Vertex Source is some object that produces polygons or polylines as ' +
    'a set of consecutive 2D vertices with commands like MoveTo, LineTo. ' +
    'It can be a container or some other object that generates vertices ' +
    'on demand. ' +
    'Coordinate conversion pipeline consists of a number of coordinate ' +
    'converters. It always works with vectorial data (X,Y) represented ' +
    'as floating point numbers (double). For example, it can contain an ' +
    'affine transformer, outline (stroke) generator, some marker ' +
    'generator (like arrowheads/arrowtails), dashed lines generator, ' +
    'and so on. The pipeline can have branches and you also can have ' +
    'any number of different pipelines. You also can write your own ' +
    'converter and include it into the pipeline. ' +
    'ScanLine Rasterizer converts vectorial data into a number of ' +
    'horizontal ScanLines. The ScanLines usually (but not obligatory) ' +
    'carry information about Anti-Aliasing as coverage values. ' +
    'Renderers render ScanLines, sorry for the tautology. The simplest ' +
    'example is solid filling. The Renderer just adds a color to the ' +
    'ScanLine and writes the result into the rendering buffer. ' +
    'More complex Renderers can produce multi-color result, ' +
    'like Gradients, Gouraud shading, image transformations, ' +
    'patterns, and so on. Rendering Buffer is a buffer in memory ' +
    'that will be displayed afterwards. Usually but not obligatory ' +
    'it contains pixels in format that fits your video system. ' +
    'For example, 24 bits B-G-R, 32 bits B-G-R-A, or 15 ' +
    'bits R-G-B-555 for Windows. But in general, there''re no ' +
    'restrictions on pixel formats or color space if you write ' +
    'your own Low level class that supports that format. ' +
    'Colors in AGG appear only in Renderers, that is, when you ' +
    'actually put some data to the rendering buffer. In general, ' +
    'there''s no general purpose structure or class like color, ' +
    'instead, AGG always operates with concrete color space. ' +
    'There are plenty of color spaces in the world, like RGB, ' +
    'HSV, CMYK, etc., and all of them have certain restrictions. ' +
    'For example, the RGB color space is just a poor subset of ' +
    'colors that a human eye can recognize. If you look at the full ' +
    'CIE Chromaticity Diagram, you will see that the RGB triangle ' +
    'is just a little part of it. ' +
    'In other words there are plenty of colors in the real world ' +
    'that cannot be reproduced with RGB, CMYK, HSV, etc. Any color ' +
    'space except the one existing in Nature is restrictive. Thus, ' +
    'it was decided not to introduce such an object like color in ' +
    'order not to restrict the possibilities in advance. Instead, ' +
    'there are objects that operate with concrete color spaces. ' +
    'Currently there are agg::rgba and agg::rgba8 that operate ' +
    'with the most popular RGB color space (strictly speaking there''s ' +
    'RGB plus Alpha). The RGB color space is used with different ' +
    'pixel formats, like 24-bit RGB or 32-bit RGBA with different ' +
    'order of color components. But the common property of all of ' +
    'them is that they are essentially RGB. Although, AGG doesn''t ' +
    'explicitly support any other color spaces, there is at least ' +
    'a potential possibility of adding them. It means that all ' +
    'class and function templates that depend on the color type ' +
    'are parameterized with the ColorT argument. ' +
    'Basically, AGG operates with coordinates of the output device. ' +
    'On your screen there are pixels. But unlike many other libraries ' +
    'and APIs AGG initially supports Subpixel Accuracy. It means ' +
    'that the coordinates are represented as doubles, where fractional ' +
    'values actually take effect. AGG doesn''t have an embedded ' +
    'conversion mechanism from world to screen coordinates in order ' +
    'not to restrict your freedom. It''s very important where and when ' +
    'you do that conversion, so, different applications can require ' +
    'different approaches. AGG just provides you a transformer of ' +
    'that kind, namely, that can convert your own view port to the ' +
    'device one. And it''s your responsibility to include it into ' +
    'the proper place of the pipeline. You can also write your ' +
    'own very simple class that will alLow you to operate with ' +
    'millimeters, inches, or any other physical units. ' +
    'Internally, the Rasterizers use integer coordinates of the ' +
    'format 24.8 bits, that is, 24 bits for the integer part and 8 ' +
    'bits for the fractional one. In other words, all the internal ' +
    'coordinates are multiplied by 256. If you intend to use AGG in ' +
    'some embedded system that has inefficient floating point ' +
    'processing, you still can use the Rasterizers with their ' +
    'integer interfaces. Although, you won''t be able to use the ' +
    'floating point coordinate pipelines in this case. ';

var
  GTextFlip: Boolean;
  GFontName: AnsiString;

type
  TAggApplication = class(TPlatformSupport)
  private
    FPixelFormats: TAggPixelFormatProcessor;
    FRadioBoxRenderingType: TAggControlRadioBox;

    FSliderHeight, FSliderWidth: TAggControlSlider;
    FSliderWeight, FSliderGamma: TAggControlSlider;

    FCheckBoxHinting: TAggControlCheckBox;
    FCheckBoxKerning: TAggControlCheckBox;
    FCheckBoxPerformance: TAggControlCheckBox;

    FFontEngine: TAggFontEngineWin32TrueTypeInt32;
    FFontCacheManager: TAggFontCacheManager;

    FOldHeight: Double;
    FGammaLut: TAggGammaLut;

    // Pipeline to process the vectors glyph paths (curves + contour)
    FCurves: TAggConvCurve;
    FContour: TAggConvContour;

    FAngle: Double;
  public
    constructor Create(Dc: HDC; PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    function DrawText(Rasterizer: TAggRasterizerScanLineAA;
      ScanLine: TAggCustomScanLine; RendererSolid: TAggRendererScanLineAASolid;
      RendererBinary: TAggRendererScanLineBinSolid): Cardinal;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
    procedure OnControlChange; override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(Dc: HDC; PixelFormat: TPixelFormat;
  FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  // Initialize structures
  FGammaLut := TAggGammaLUT.Create(8, 16);
  PixelFormatBgr24Gamma(FPixelFormats, RenderingBufferWindow, FGammaLut);

  FRadioBoxRenderingType := TAggControlRadioBox.Create(5, 5, 155, 110,
    not FlipY);
  FSliderHeight := TAggControlSlider.Create(160, 10, 635, 18, not FlipY);
  FSliderWidth := TAggControlSlider.Create(160, 30, 635, 38, not FlipY);
  FSliderWeight := TAggControlSlider.Create(160, 50, 635, 58, not FlipY);
  FSliderGamma := TAggControlSlider.Create(260, 70, 635, 78, not FlipY);
  FCheckBoxHinting := TAggControlCheckBox.Create(160, 65, 'Hinting', not FlipY);
  FCheckBoxKerning := TAggControlCheckBox.Create(160, 80, 'Kerning', not FlipY);
  FCheckBoxPerformance := TAggControlCheckBox.Create(160, 95,
    'Test Performance', not FlipY);

  FFontEngine := TAggFontEngineWin32TrueTypeInt32.Create(Dc);
  FFontCacheManager := TAggFontCacheManager.Create(FFontEngine);

  FOldHeight := 0.0;

  FCurves := TAggConvCurve.Create(FFontCacheManager.PathAdaptor);
  FContour := TAggConvContour.Create(FCurves);

  FRadioBoxRenderingType.AddItem('Native Mono');
  FRadioBoxRenderingType.AddItem('Native Gray 8');
  FRadioBoxRenderingType.AddItem('AGG Outline');
  FRadioBoxRenderingType.AddItem('AGG Mono');
  FRadioBoxRenderingType.AddItem('AGG Gray 8');
  FRadioBoxRenderingType.SetCurrentItem(1);

  AddControl(FRadioBoxRenderingType);

  FRadioBoxRenderingType.NoTransform;

  FSliderHeight.Caption := 'Font Height=%.2f';
  FSliderHeight.SetRange(8, 32);
  FSliderHeight.Value := 18;

  FSliderHeight.NumSteps := 24;
  FSliderHeight.TextThickness := 1.5;

  AddControl(FSliderHeight);

  FSliderHeight.NoTransform;

  FSliderWidth.Caption := 'Font Width=%.2f';
  FSliderWidth.SetRange(8, 32);
  FSliderWidth.Value := 18;

  FSliderWidth.NumSteps := 24;
  FSliderWidth.TextThickness := 1.5;

  AddControl(FSliderWidth);

  FSliderWidth.NoTransform;

  FSliderWeight.Caption := 'Font Weight=%.2f';
  FSliderWeight.SetRange(-1, 1);

  FSliderWeight.TextThickness := 1.5;

  AddControl(FSliderWeight);

  FSliderWeight.NoTransform;

  FSliderGamma.Caption := 'Gamma=%.2f';
  FSliderGamma.SetRange(0.1, 2.0);
  FSliderGamma.Value := 1.0;

  FSliderGamma.TextThickness := 1.5;

  AddControl(FSliderGamma);

  FSliderGamma.NoTransform;

  AddControl(FCheckBoxHinting);

  FCheckBoxHinting.Status := True;
  FCheckBoxHinting.NoTransform;

  AddControl(FCheckBoxKerning);

  FCheckBoxKerning.Status := True;
  FCheckBoxKerning.NoTransform;

  AddControl(FCheckBoxPerformance);

  FCheckBoxPerformance.NoTransform;

  // FCurves.ApproximationMethod := CurveDiv;
  // FCurves.ApproximationScale := 0.5;
  // FCurves.AngleTolerance := 0.3;

  FContour.AutoDetectOrientation := False;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxRenderingType.Free;
  FSliderHeight.Free;
  FSliderWidth.Free;
  FSliderWeight.Free;
  FSliderGamma.Free;
  FCheckBoxHinting.Free;
  FCheckBoxKerning.Free;
  FCheckBoxPerformance.Free;

  FFontEngine.Free;
  FFontCacheManager.Free;

  FGammaLut.Free;
  FCurves.Free;
  FContour.Free;

  FPixelFormats.Free;

  inherited;
end;

function TAggApplication.DrawText(Rasterizer: TAggRasterizerScanLineAA;
  ScanLine: TAggCustomScanLine; RendererSolid: TAggRendererScanLineAASolid;
  RendererBinary: TAggRendererScanLineBinSolid): Cardinal;
var
  GlyphRendering: TAggGlyphRendering;

  NumGlyphs: Cardinal;

  Mtx: TAggTransAffine;

  X, Y0, Y: Double;

  P: PInt8u;

  Rgba : TAggColor;
  Glyph: PAggGlyphCache;
begin
  Assert(Rasterizer is TAggRasterizerScanLineAA);
  Assert(RendererSolid is TAggRendererScanLineAASolid);
  Assert(RendererBinary is TAggRendererScanLineBinSolid);

  GlyphRendering := grNativeMono;

  case FRadioBoxRenderingType.GetCurrentItem of
    0:
      GlyphRendering := grNativeMono;
    1:
      GlyphRendering := grNativeGray8;
    2:
      GlyphRendering := grOutline;
    3:
      GlyphRendering := grAggMono;
    4:
      GlyphRendering := grAggGray8;
  end;

  NumGlyphs := 0;

  FContour.Width := -FSliderWeight.Value * FSliderHeight.Value * 0.05;

  FFontEngine.Hinting := FCheckBoxHinting.Status;
  FFontEngine.Height := FSliderHeight.Value;

  // Font width in Windows is strange. MSDN says,
  // "specifies the average width", but there's no clue what
  // this "average width" means. It'd be logical to specify
  // the width with regard to the font height, like it's done in
  // FreeType. That is, width == height should mean the "natural",
  // not distorted glyphs. In Windows you have to specify
  // the absolute width, which is very stupid and hard to use
  // in practice.
  if FSliderWidth.Value = FSliderHeight.Value then
    FFontEngine.Width := 0.0
  else
    FFontEngine.Width := FSliderWidth.Value / 2.4;

  // FFontEngine.italic_(true );
  FFontEngine.FlipY := GTextFlip;

  Mtx := TAggTransAffine.Create;
  try
    if FAngle <> 0 then
      Mtx.Rotate(Deg2Rad(FAngle));

    // taw.Create(-0.3 ,0 ); mtx.multiply(@taw );

    FFontEngine.SetTransform(Mtx);

    if FFontEngine.CreateFont(GFontName, GlyphRendering) then
    begin
      FFontCacheManager.PreCache(Cardinal(' '), 127);

      X := 10.0;
      Y0 := Height - FSliderHeight.Value - 10.0;
      Y := Y0;
      P := @CDisplayText[0];

      while P^ <> 0 do
      begin
        Glyph := FFontCacheManager.Glyph(P^);

        if Glyph <> nil then
        begin
          if FCheckBoxKerning.Status then
            FFontCacheManager.AddKerning(@X, @Y);

          if X >= Width - FSliderHeight.Value then
          begin
            X := 10.0;
            Y0 := Y0 - FSliderHeight.Value;

            if Y0 <= 120 then
              Break;

            Y := Y0;
          end;

          FFontCacheManager.InitEmbeddedAdaptors(Glyph, X, Y);

          case Glyph.DataType of
            gdMono:
              begin
                Rgba.Black;
                RendererBinary.SetColor(@Rgba);

                Assert(FFontCacheManager.MonoAdaptor <> nil);
                RenderScanLines(FFontCacheManager.MonoAdaptor,
                  FFontCacheManager.MonoScanLine, RendererBinary);
              end;

            gdGray8:
              begin
                Rgba.Black;
                RendererSolid.SetColor(@Rgba);

                Assert(FFontCacheManager.Gray8Adaptor <> nil);
                RenderScanLines(FFontCacheManager.Gray8Adaptor,
                  FFontCacheManager.Gray8ScanLine, RendererSolid);
              end;

            gdOutline:
              begin
                Rasterizer.Reset;

                if Abs(FSliderWeight.Value) <= 0.01 then
                  // For the sake of efficiency skip the
                  // contour converter if the weight is about zero.
                  Rasterizer.AddPath(FCurves)
                else
                  Rasterizer.AddPath(FContour);

                Rgba.Black;
                RendererSolid.SetColor(@Rgba);

                RenderScanLines(Rasterizer, ScanLine, RendererSolid);
              end;
          end;

          // increment pen position
          X := X + Glyph.AdvanceX;
          Y := Y + Glyph.AdvanceY;

          Inc(NumGlyphs);
        end;

        Inc(PtrComp(P), SizeOf(Int8u));
      end;
    end;
  finally
    Mtx.Free;
  end;

  Result := NumGlyphs;
end;

procedure TAggApplication.OnDraw;
var
  RendererBase : TAggRendererBase;
  RendererSolid: TAggRendererScanLineAASolid;
  RendererBinary: TAggRendererScanLineBinSolid;

  ScanLine : TAggScanLineUnpacked8;
  Rasterizer: TAggRasterizerScanLineAA;

  GammaThreshold: TAggGammaThreshold;
  GammaNone: TAggGammaNone;
  GammaPower: TAggGammaPower;
begin
  RendererBase := TAggRendererBase.Create(FPixelFormats);
  try
    RendererSolid := TAggRendererScanLineAASolid.Create(RendererBase);
    RendererBinary := TAggRendererScanLineBinSolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    ScanLine := TAggScanLineUnpacked8.Create;
    Rasterizer := TAggRasterizerScanLineAA.Create;

    if FSliderHeight.Value <> FOldHeight then
    begin
      FOldHeight := FSliderHeight.Value;

      FSliderWidth.Value := FOldHeight;
    end;

    // Setup Gamma
    if FRadioBoxRenderingType.GetCurrentItem = 3 then
    begin
      // When rendering in mono format,
      // Set threshold Gamma = 0.5
      GammaThreshold := TAggGammaThreshold.Create(FSliderGamma.Value * 0.5);
      try
        FFontEngine.SetGamma(GammaThreshold);
      finally
        GammaThreshold.Free
      end;
    end
    else
    begin
      GammaNone := TAggGammaNone.Create;
      try
        FFontEngine.SetGamma(GammaNone);
      finally
        GammaNone.Free;
      end;

      FGammaLut.Gamma := FSliderGamma.Value;
    end;

    // Render the text
    DrawText(Rasterizer, ScanLine, RendererSolid, RendererBinary);

    // Render the controls
    GammaPower := TAggGammaPower.Create(1.0);
    try
      Rasterizer.Gamma(GammaPower);
    finally
      GammaPower.Free;
    end;

    RenderControl(Rasterizer, ScanLine, RendererSolid, FRadioBoxRenderingType);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FSliderHeight);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FSliderWidth);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FSliderWeight);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FSliderGamma);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FCheckBoxHinting);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FCheckBoxKerning);
    RenderControl(Rasterizer, ScanLine, RendererSolid, FCheckBoxPerformance);

    // Free AGG resources
    ScanLine.Free;
    Rasterizer.Free;

    RendererSolid.Free;
    RendererBinary.Free;
  finally
    RendererBase.Free
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Byte(' ') then
  begin
    GTextFlip := not GTextFlip;

    ForceRedraw;
  end;

  if Key = Cardinal(kcPadMinus) then
  begin
    FAngle := FAngle + CAngleStep;

    if FAngle > 360 then
      FAngle := 0;

    ForceRedraw;
  end;

  if Key = Cardinal(kcPadPlus) then
  begin
    FAngle := FAngle - CAngleStep;

    if FAngle < 0 then
      FAngle := 360 - CAngleStep;

    ForceRedraw;
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('This example demonstrates the use of the Win32 TrueType '
      + 'font engine with cache. Cache can keep three types of data, vector '
      + 'path, Anti-Aliased ScanLine shape, and monochrome ScanLine shape. '
      + 'In case of caching ScanLine shapes the speed is pretty good and '
      + 'comparable with Windows hardware accelerated font rendering.'#13#13
      + 'How to play with:'#13#13
      + 'Press the spacebar to flip the text vertically.'#13#13
      + 'Key Plus - Increase font angle (not for Natives)'#13
      + 'Key Minus - Decrease font angle (not for Natives)'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

procedure TAggApplication.OnControlChange;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;

  RendererBase : TAggRendererBase;
  RendererSolid: TAggRendererScanLineAASolid;
  RendererBinary  : TAggRendererScanLineBinSolid;

  ScanLine : TAggScanLineUnpacked8;
  Rasterizer: TAggRasterizerScanLineAA;

  NumGlyphs, I: Cardinal;

  T: Double;
begin
  if FCheckBoxPerformance.Status then
  begin
    PixelFormatBgr24Gamma(PixelFormatProcessor, RenderingBufferWindow,
      FGammaLut);

    RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
    try
      RendererSolid := TAggRendererScanLineAASolid.Create(@RendererBase);
      RendererBinary := TAggRendererScanLineBinSolid.Create(@RendererBase);

      RendererBase.Clear(CRgba8White);

      ScanLine := TAggScanLineUnpacked8.Create;
      Rasterizer := TAggRasterizerScanLineAA.Create;

      NumGlyphs := 0;

      StartTimer;

      for I := 0 to 49 do
        Inc(NumGlyphs, DrawText(Rasterizer, ScanLine, RendererSolid,
          RendererBinary));

      T := GetElapsedTime;

      DisplayMessage(Format('Glyphs=%u, Time=%.3fms, %.3f glyps/sec, %.3f '
        + 'microsecond/glyph', [NumGlyphs, T, (NumGlyphs / T) * 1000.0,
        (T / NumGlyphs) * 1000.0]));

      FCheckBoxPerformance.Status := False;

      ForceRedraw;

      ScanLine.Free;
      Rasterizer.Free;
      RendererSolid.Free;
      RendererBinary.Free;
    finally
      RendererBase.Free;
    end;
  end;
end;

var
  Dc : HDC;

begin
  GTextFlip := False;
  GFontName := 'Arial';

{$IFDEF WIN32}
  if ParamCount > 0 then
    GFontName := ParamStr(1);
{$ENDIF}

  Dc := GetDC(0);

  with TAggApplication.Create(Dc, pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Rendering TrueType Fonts with WinAPI (F1-Help)';
    if Init(640, 520, [wfResize]) then
      Run;
  finally
    Free;
  end;

  ReleaseDC(0, Dc);
end.
