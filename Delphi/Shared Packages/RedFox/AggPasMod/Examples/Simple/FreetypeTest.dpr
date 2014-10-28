program FreetypeTest;

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
  AggFontFreeType in '..\..\Source\AggFontFreeType.pas',
  AggFontCacheManager in '..\..\Source\AggFontCacheManager.pas';

const
  CFlipY = True;

  CAngleStep = 0.5;

  CDisplayText: PAnsiChar =
  // '0123456789ABCDEFGHIJKLMNOPRSTUVWXYZabcdefghijklmnoprstuvwxyz ' +
  // '[ BRAVO ][ VALUE ] [ T.W.Lewis ] [ Kerning Examples ] ' +
    'Anti-Grain Geometry is designed as a set of loosely coupled '
    + 'algorithms and class templates united with a common idea, '
    + 'so that all the components can be easily combined. Also, '
    + 'the template based design allows you to replace any part of '
    + 'the library without the necessity to modify a single byte in '
    + 'the existing code. '
    + 'AGG is designed keeping in mind extensibility and flexibility. '
    + 'Basically I just wanted to create a toolkit that would alLow me '
    + '(and anyone else) to add new fancy algorithms very easily. '
    + 'AGG does not dictate you any style of its use, you are free to '
    + 'use any part of it. However, AGG is often associated with a tool '
    + 'for rendering images in memory. That is not quite true, but it can '
    + 'be a good starting point in studying. The tutorials describe the '
    + 'use of AGG starting from the Low level functionality that deals with '
    + 'frame buffers and pixels. Then you will gradually understand how to '
    + 'abstract different parts of the library and how to use them separately. '
    + 'Remember, the raster picture is often not the only thing you want to '
    + 'obtain, you will probably want to print your graphics with Highest '
    + 'possible quality and in this case you can easily combine the '
    + '"vectorial" part of the library with some API like Windows GDI, having '
    + 'a common external interface. If that API can render multi-polygons with '
    + 'non-zero and even-odd filling rules it''s all you need to incorporate '
    + 'AGG into your application. For example, Windows API PolyPolygon '
    + 'perfectly fits these needs, except certain advanced things like '
    + 'Gradient filling, Gouraud shading, image transformations, and so on. '
    + 'Or, as an alternative, you can use all AGG algorithms producing High '
    + 'resolution pixel images and then to send the result to the printer as a '
    + 'pixel map. BeLow is a typical brief scheme of the AGG rendering '
    + 'pipeline. Please note that any component between the Vertex Source '
    + 'and Screen Output is not mandatory. It all depends on your '
    + 'particular needs. For example, you can use your own Rasterizer, '
    + 'based on Windows API. In this case you won''t need the AGG Rasterizer '
    + 'and Renderers. Or, if you need to draw only lines, you can use the '
    + 'AGG outline Rasterizer that has certain restrictions but works faster. '
    + 'The number of possibilities is endless. '
    + 'Vertex Source is some object that produces polygons or polylines as '
    + 'a set of consecutive 2D vertices with commands like MoveTo, LineTo. '
    + 'It can be a container or some other object that generates vertices '
    + 'on demand. '
    + 'Coordinate conversion pipeline consists of a number of coordinate '
    + 'converters. It always works with vectorial data (X,Y) represented '
    + 'as floating point numbers (double). For example, it can contain an '
    + 'affine transformer, outline (stroke) generator, some marker '
    + 'generator (like arrowheads/arrowtails), dashed lines generator, '
    + 'and so on. The pipeline can have branches and you also can have '
    + 'any number of different pipelines. You also can write your own '
    + 'converter and include it into the pipeline. '
    + 'ScanLine Rasterizer converts vectorial data into a number of '
    + 'horizontal ScanLines. The ScanLines usually (but not obligatory) '
    + 'carry information about Anti-Aliasing as coverage values. '
    + 'Renderers render ScanLines, sorry for the tautology. The simplest '
    + 'example is solid filling. The Renderer just adds a color to the '
    + 'ScanLine and writes the result into the rendering buffer. '
    + 'More complex Renderers can produce multi-color result, '
    + 'like Gradients, Gouraud shading, image transformations, '
    + 'patterns, and so on. Rendering Buffer is a buffer in memory '
    + 'that will be displayed afterwards. Usually but not obligatory '
    + 'it contains pixels in format that fits your video system. '
    + 'For example, 24 bits B-G-R, 32 bits B-G-R-A, or 15 '
    + 'bits R-G-B-555 for Windows. But in general, there''re no '
    + 'restrictions on pixel formats or color space if you write '
    + 'your own Low level class that supports that format. '
    + 'Colors in AGG appear only in Renderers, that is, when you '
    + 'actually put some data to the rendering buffer. In general, '
    + 'there''s no general purpose structure or class like color, '
    + 'instead, AGG always operates with concrete color space. '
    + 'There are plenty of color spaces in the world, like RGB, '
    + 'HSV, CMYK, etc., and all of them have certain restrictions. '
    + 'For example, the RGB color space is just a poor subset of '
    + 'colors that a human eye can recognize. If you look at the full '
    + 'CIE Chromaticity Diagram, you will see that the RGB triangle '
    + 'is just a little part of it. '
    + 'In other words there are plenty of colors in the real world '
    + 'that cannot be reproduced with RGB, CMYK, HSV, etc. Any color '
    + 'space except the one existing in Nature is restrictive. Thus, '
    + 'it was decided not to introduce such an object like color in '
    + 'order not to restrict the possibilities in advance. Instead, '
    + 'there are objects that operate with concrete color spaces. '
    + 'Currently there are agg::rgba and agg::rgba8 that operate '
    + 'with the most popular RGB color space (strictly speaking there''s '
    + 'RGB plus Alpha). The RGB color space is used with different '
    + 'pixel formats, like 24-bit RGB or 32-bit RGBA with different '
    + 'order of color components. But the common property of all of '
    + 'them is that they are essentially RGB. Although, AGG doesn''t '
    + 'explicitly support any other color spaces, there is at least '
    + 'a potential possibility of adding them. It means that all '
    + 'class and function templates that depend on the color type '
    + 'are parameterized with the ColorT argument. '
    + 'Basically, AGG operates with coordinates of the output device. '
    + 'On your screen there are pixels. But unlike many other libraries '
    + 'and APIs AGG initially supports Subpixel Accuracy. It means '
    + 'that the coordinates are represented as doubles, where fractional '
    + 'values actually take effect. AGG doesn''t have an embedded '
    + 'conversion mechanism from world to screen coordinates in order '
    + 'not to restrict your freedom. It''s very important where and when '
    + 'you do that conversion, so, different applications can require '
    + 'different approaches. AGG just provides you a transformer of '
    + 'that kind, namely, that can convert your own view port to the '
    + 'device one. And it''s your responsibility to include it into '
    + 'the proper place of the pipeline. You can also write your '
    + 'own very simple class that will alLow you to operate with '
    + 'millimeters, inches, or any other physical units. '
    + 'Internally, the Rasterizers use integer coordinates of the '
    + 'format 24.8 bits, that is, 24 bits for the integer part and 8 '
    + 'bits for the fractional one. In other words, all the internal '
    + 'coordinates are multiplied by 256. If you intend to use AGG in '
    + 'some embedded system that has inefficient floating point '
    + 'processing, you still can use the Rasterizers with their '
    + 'integer interfaces. Although, you won''t be able to use the '
    + 'floating point coordinate pipelines in this case. ';

var
  GFontFlipY: Boolean;
  GFontName  : AnsiString;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxRenderType: TAggControlRadioBox;

    FSliderHeight, FSliderWidth: TAggControlSlider;
    FSliderWeight, FSliderGamma: TAggControlSlider;

    FCheckBoxHinting, FCheckBoxKerning: TAggControlCheckBox;
    FCheckBoxPerformance: TAggControlCheckBox;

    FFontEngine: TAggFontEngineFreetypeInt32;
    FFontCacheManager: TAggFontCacheManager;

    FOldHeight: Double;
    FGammaLut: TAggGammaLut;

    // Pipeline to process the vectors glyph paths (curves + contour)
    FCurves: TAggConvCurve;
    FContour: TAggConvContour;

    FAngle: Double;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    function DrawText(Ras: TAggRasterizerScanLineAA; Sl: TAggCustomScanLine;
      RenSolid: TAggRendererScanLineAASolid;
      RenBin: TAggRendererScanLineBinSolid): Cardinal;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); override;
    procedure OnControlChange; override;
  end;

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxRenderType := TAggControlRadioBox.Create(5, 5, 155, 110, not FlipY);
  FSliderHeight := TAggControlSlider.Create(160, 10, 635, 18, not FlipY);
  FSliderWidth := TAggControlSlider.Create(160, 30, 635, 38, not FlipY);
  FSliderWeight := TAggControlSlider.Create(160, 50, 635, 58, not FlipY);
  FSliderGamma := TAggControlSlider.Create(260, 70, 635, 78, not FlipY);
  FCheckBoxHinting := TAggControlCheckBox.Create(160, 65, 'Hinting', not FlipY);
  FCheckBoxKerning := TAggControlCheckBox.Create(160, 80, 'Kerning', not FlipY);
  FCheckBoxPerformance := TAggControlCheckBox.Create(160, 95,
    'Test Performance', not FlipY);

  FFontEngine := TAggFontEngineFreetypeInt32.Create;
  FFontCacheManager := TAggFontCacheManager.Create(FFontEngine);

  FOldHeight := 0;

  FGammaLut := TAggGammaLut.Create(8, 16);
  FCurves := TAggConvCurve.Create(FFontCacheManager.PathAdaptor);
  FContour := TAggConvContour.Create(FCurves);

  FRadioBoxRenderType.AddItem('Native Mono');
  FRadioBoxRenderType.AddItem('Native Gray 8');
  FRadioBoxRenderType.AddItem('Outline');
  FRadioBoxRenderType.AddItem('AGG Mono');
  FRadioBoxRenderType.AddItem('AGG Gray 8');
  FRadioBoxRenderType.SetCurrentItem(1);
  AddControl(FRadioBoxRenderType);
  FRadioBoxRenderType.NoTransform;

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
  FSliderGamma.SetRange(0.1, 2);
  FSliderGamma.Value := 1;
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

  FCurves.ApproximationScale := 2;
  FContour.AutoDetectOrientation := False;

  FAngle := 0;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxRenderType.Free;
  FSliderHeight.Free;
  FSliderWidth.Free;
  FSliderWeight.Free;
  FSliderGamma.Free;
  FCheckBoxHinting.Free;
  FCheckBoxKerning.Free;
  FCheckBoxPerformance.Free;

  FFontEngine.Free;
  FFontCacheManager.Free;

  FGammaLUT.Free;
  FCurves.Free;
  FContour.Free;

  inherited;
end;

function TAggApplication.DrawText(Ras: TAggRasterizerScanLineAA;
  Sl: TAggCustomScanLine; RenSolid: TAggRendererScanLineAASolid;
  RenBin: TAggRendererScanLineBinSolid): Cardinal;
var
  Gren: TAggGlyphRendering;
  NumGlyphs: Cardinal;
  Mtx: TAggTransAffine;
  X, Y0, Y: Double;
  P: PInt8u;
  Glyph: PAggGlyphCache;
begin
  Assert(Ras is TAggRasterizerScanLineAA);
  Assert(RenSolid is TAggRendererScanLineAASolid);
  Assert(RenBin is TAggRendererScanLineBinSolid);

  case FRadioBoxRenderType.GetCurrentItem of
    0:
      Gren := grNativeMono;
    1:
      Gren := grNativeGray8;
    2:
      Gren := grOutline;
    3:
      Gren := grAggMono;
    4:
      Gren := grAggGray8;
    else
      Gren := grNativeMono;
  end;

  NumGlyphs := 0;

  FContour.Width := -FSliderWeight.Value * FSliderHeight.Value * 0.05;

  if FFontEngine.LoadFont(@GFontName[1], 0, Gren) then
  begin
    FFontEngine.Hinting := FCheckBoxHinting.Status;
    FFontEngine.SetHeight(FSliderHeight.Value);
    FFontEngine.SetWidth(FSliderWidth.Value);
    FFontEngine.FlipY := GFontFlipY;

    Mtx := TAggTransAffine.Create;

    if FAngle <> 0 then
      Mtx.Rotate(Deg2Rad(FAngle));

    // taw := .Create(-0.4 ,0 );        mtx.multiply(taw );
    // tat := .Create(1 ,0 );           mtx.multiply(tat );

    FFontEngine.SetTransform(Mtx);

    X := 10;
    Y0 := Height - FSliderHeight.Value - 10;
    Y := Y0;
    P := @CDisplayText[0];

    Assert(Assigned(FFontCacheManager));
    while P^ <> 0 do
    begin
      Glyph := FFontCacheManager.Glyph(P^);

      if Glyph <> nil then
      begin
        if FCheckBoxKerning.Status then
          FFontCacheManager.AddKerning(@X, @Y);

        if X >= Width - FSliderHeight.Value then
        begin
          X := 10;
          Y0 := Y0 - FSliderHeight.Value;

          if Y0 <= 120 then
            Break;

          Y := Y0;
        end;

        FFontCacheManager.InitEmbeddedAdaptors(Glyph, X, Y);

        case Glyph.DataType of
          gdMono:
            begin
              RenBin.SetColor(CRgba8Black);

              RenderScanLines(FFontCacheManager.MonoAdaptor,
                FFontCacheManager.MonoScanLine, RenBin);
            end;

          gdGray8:
            begin
              RenSolid.SetColor(CRgba8Black);

              RenderScanLines(FFontCacheManager.Gray8Adaptor,
                FFontCacheManager.Gray8ScanLine, RenSolid);
            end;

          gdOutline:
            begin
              Ras.Reset;

              if Abs(FSliderWeight.Value) <= 0.01 then
                // For the sake of efficiency skip the
                // contour converter if the weight is about zero.
                Ras.AddPath(FCurves)
              else
                Ras.AddPath(FContour);

              RenSolid.SetColor(CRgba8Black);

              RenderScanLines(Ras, Sl, RenSolid);
            end;
        end;

        // increment pen position
        X := X + Glyph.AdvanceX;
        Y := Y + Glyph.AdvanceY;

        Inc(NumGlyphs);
      end;

      Inc(PtrComp(P), SizeOf(Int8u));
    end;

    Mtx.Free;
  end
  else
    DisplayMessage('Please copy file timesi.ttf to the current directory'#13 +
      'or download it from http://www.antigrain.com/timesi.zip');

  Result := NumGlyphs;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid;
  RenBin: TAggRendererScanLineBinSolid;

  Sl : TAggScanLineUnpacked8;
  Ras: TAggRasterizerScanLineAA;

  GammaThreshold: TAggGammaThreshold;
  GammaNone: TAggGammaNone;
  GammaPower: TAggGammaPower;

begin
  // Initialize structures
  PixelFormatBgr24Gamma(Pixf, RenderingBufferWindow, FGammaLUT);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenSolid := TAggRendererScanLineAASolid.Create(RendererBase);
    RenBin := TAggRendererScanLineBinSolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Sl := TAggScanLineUnpacked8.Create;
      Ras := TAggRasterizerScanLineAA.Create;

      if FSliderHeight.Value <> FOldHeight then
      begin
        FOldHeight := FSliderHeight.Value;

        FSliderWidth.Value := FOldHeight;
      end;

      // Setup Gamma
      if FRadioBoxRenderType.GetCurrentItem = 3 then
      begin
        // When rendering in mono format,
        // Set threshold Gamma = 0.5
        GammaThreshold := TAggGammaThreshold.Create(FSliderGamma.Value * 0.5);
        try
        FFontEngine.SetGamma(GammaThreshold);
        finally
          GammaThreshold.Free;
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

        FGammaLUT.Gamma := FSliderGamma.Value;
      end;

      if FRadioBoxRenderType.GetCurrentItem = 2 then
      begin
        // For outline cache set Gamma for the Rasterizer
        GammaPower := TAggGammaPower.Create(FSliderGamma.Value);
        try
          Ras.Gamma(GammaPower);
        finally
          GammaPower.Free;
        end;
      end;

      // Render the text
      DrawText(Ras, Sl, RenSolid, RenBin);

      // Render the controls
      GammaPower := TAggGammaPower.Create(1);
      try
        Ras.Gamma(GammaPower);
      finally
        GammaPower.Free;
      end;

      RenderControl(Ras, Sl, RenSolid, FRadioBoxRenderType);
      RenderControl(Ras, Sl, RenSolid, FSliderHeight);
      RenderControl(Ras, Sl, RenSolid, FSliderWidth);
      RenderControl(Ras, Sl, RenSolid, FSliderWeight);
      RenderControl(Ras, Sl, RenSolid, FSliderGamma);
      RenderControl(Ras, Sl, RenSolid, FCheckBoxHinting);
      RenderControl(Ras, Sl, RenSolid, FCheckBoxKerning);
      RenderControl(Ras, Sl, RenSolid, FCheckBoxPerformance);

      // Free AGG resources
      Sl.Free;
      Ras.Free;
    finally
      RenSolid.Free;
      RenBin.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Byte(' ') then
  begin
    GFontFlipY := not GFontFlipY;

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
    DisplayMessage('This example demonstrates the use of the FreeType font '
      + 'engine with cache. Cache can keep three types of data, vector path, '
      + 'Anti-Aliased ScanLine shape, and monochrome ScanLine shape. In case '
      + 'of caching ScanLine shapes the speed is pretty good and comparable '
      + 'with Windows hardware accelerated font rendering.'#13#13
      + 'How to play with:'#13#13
      + 'Press the spacebar to flip the text vertically.'#13
      + 'Key Plus - Increase font angle (not for Natives)'#13
      + 'Key Minus - Decrease font angle (not for Natives)'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

procedure TAggApplication.OnControlChange;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid;
  RenBin: TAggRendererScanLineBinSolid;

  Sl : TAggScanLineUnpacked8;
  Ras: TAggRasterizerScanLineAA;

  NumGlyphs, I: Cardinal;

  T: Double;
begin
  if FCheckBoxPerformance.Status then
  begin
    PixelFormatBgr24Gamma(Pixf, RenderingBufferWindow, FGammaLUT);

    RendererBase := TAggRendererBase.Create(Pixf, True);
    try
      RenSolid := TAggRendererScanLineAASolid.Create(RendererBase);
      RenBin := TAggRendererScanLineBinSolid.Create(RendererBase);

      RendererBase.Clear(CRgba8White);

      Sl := TAggScanLineUnpacked8.Create;
      Ras := TAggRasterizerScanLineAA.Create;

      NumGlyphs := 0;

      StartTimer;

      for I := 0 to 49 do
        Inc(NumGlyphs, DrawText(Ras, Sl, RenSolid, RenBin));

      T := GetElapsedTime;

      DisplayMessage(Format('Glyphs=%u, Time=%.3fms, %.3f glyps/sec, %.3f ' +
        'microsecond/glyph', [NumGlyphs, T, (NumGlyphs / T) * 1000,
        (T / NumGlyphs) * 1000]));

      FCheckBoxPerformance.Status := False;

      ForceRedraw;

      RenSolid.Free;
      RenBin.Free;
      Sl.Free;
      Ras.Free;
    finally
      RendererBase.Free;
    end;
  end;
end;

begin
  GFontFlipY := False;
  GFontName := 'timesi.ttf';

{$IFDEF WIN32}
  if ParamCount > 0 then
    GFontName := ParamStr(1);
{$ENDIF}

  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Rendering Fonts with FreeType (F1-Help)';

    if Init(640, 520, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
