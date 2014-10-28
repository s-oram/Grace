program LinePatterns;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$I-}

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
  AggBezierControl in '..\..\Source\Controls\AggBezierControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRendererOutlineImage in '..\..\Source\AggRendererOutlineImage.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPatternFiltersRgba in '..\..\Source\AggPatternFiltersRgba.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvClipPolyline in '..\..\Source\AggConvClipPolyline.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas';

const
  CFlipY = True;

  CBrightnessToAlpha: array [0..256 * 3 - 1] of Int8u = (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 254, 254, 254, 254, 254, 254, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 254, 254, 254, 254, 254, 254, 254,
    254, 254, 254, 254, 254, 254, 254, 253, 253, 253, 253, 253, 253, 253, 253,
    253, 253, 253, 253, 253, 253, 253, 253, 253, 252, 252, 252, 252, 252, 252,
    252, 252, 252, 252, 252, 252, 251, 251, 251, 251, 251, 251, 251, 251, 251,
    250, 250, 250, 250, 250, 250, 250, 250, 249, 249, 249, 249, 249, 249, 249,
    248, 248, 248, 248, 248, 248, 248, 247, 247, 247, 247, 247, 246, 246, 246,
    246, 246, 246, 245, 245, 245, 245, 245, 244, 244, 244, 244, 243, 243, 243,
    243, 243, 242, 242, 242, 242, 241, 241, 241, 241, 240, 240, 240, 239, 239,
    239, 239, 238, 238, 238, 238, 237, 237, 237, 236, 236, 236, 235, 235, 235,
    234, 234, 234, 233, 233, 233, 232, 232, 232, 231, 231, 230, 230, 230, 229,
    229, 229, 228, 228, 227, 227, 227, 226, 226, 225, 225, 224, 224, 224, 223,
    223, 222, 222, 221, 221, 220, 220, 219, 219, 219, 218, 218, 217, 217, 216,
    216, 215, 214, 214, 213, 213, 212, 212, 211, 211, 210, 210, 209, 209, 208,
    207, 207, 206, 206, 205, 204, 204, 203, 203, 202, 201, 201, 200, 200, 199,
    198, 198, 197, 196, 196, 195, 194, 194, 193, 192, 192, 191, 190, 190, 189,
    188, 188, 187, 186, 186, 185, 184, 183, 183, 182, 181, 180, 180, 179, 178,
    177, 177, 176, 175, 174, 174, 173, 172, 171, 171, 170, 169, 168, 167, 166,
    166, 165, 164, 163, 162, 162, 161, 160, 159, 158, 157, 156, 156, 155, 154,
    153, 152, 151, 150, 149, 148, 148, 147, 146, 145, 144, 143, 142, 141, 140,
    139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 129, 128, 128, 127, 125,
    124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110,
    109, 108, 107, 106, 105, 104, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93, 91,
    90, 89, 88, 87, 86, 85, 84, 82, 81, 80, 79, 78, 77, 75, 74, 73, 72, 71, 70,
    69, 67, 66, 65, 64, 63, 61, 60, 59, 58, 57, 56, 54, 53, 52, 51, 50, 48, 47,
    46, 45, 44, 42, 41, 40, 39, 37, 36, 35, 34, 33, 31, 30, 29, 28, 27, 25, 24,
    23, 22, 20, 19, 18, 17, 15, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1);

type
  TPatternSourceBrightnessToAlphaRgba8 = class(TAggPixelSource)
  private
    FRenderingBuffer: TAggRenderingBuffer;
    FPixelFormatProcessor: TAggPixelFormatProcessor;
  protected
    function GetWidth: Cardinal; override;
    function GetHeight: Cardinal; override;
  public
    constructor Create(Rb: TAggRenderingBuffer);
    destructor Destroy; override;

    function Pixel(X, Y: Integer): TAggRgba8; override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FControlColor: TAggColor;

    FCurve: array [0..8] of TBezierControl;
    FSliderScaleX, FSliderStartX: TAggControlSlider;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure DrawCurve(LineImagePattern: TAggLineImagePattern;
      Rasterizer: TAggRasterizerOutlineAA; Ren: TAggRendererOutlineImage;
      Src: TAggPixelSource; Vs: TAggVertexSource);

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); override;
  end;


{ TPatternSourceBrightnessToAlphaRgba8 }

constructor TPatternSourceBrightnessToAlphaRgba8.Create(Rb: TAggRenderingBuffer);
begin
  FRenderingBuffer := Rb;

  PixelFormatBgr24(FPixelFormatProcessor, FRenderingBuffer);
end;

function TPatternSourceBrightnessToAlphaRgba8.GetWidth;
begin
  Result := FPixelFormatProcessor.Width;
end;

destructor TPatternSourceBrightnessToAlphaRgba8.Destroy;
begin
  FPixelFormatProcessor.Free;
  inherited;
end;

function TPatternSourceBrightnessToAlphaRgba8.GetHeight;
begin
  Result := FPixelFormatProcessor.Height;
end;

function TPatternSourceBrightnessToAlphaRgba8.Pixel(X, Y: Integer): TAggRgba8;
var
  C: TAggColor;
begin
  C := FPixelFormatProcessor.Pixel(FPixelFormatProcessor, X, Y);
  C.Rgba8.A := CBrightnessToAlpha[C.Rgba8.R + C.Rgba8.G + C.Rgba8.B];

  Result := C.Rgba8;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FControlColor.FromRgbaDouble(0, 0.3, 0.5, 0.3);

  FSliderScaleX := TAggControlSlider.Create(5.0, 5.0, 240.0, 12.0, not FlipY);
  FSliderStartX := TAggControlSlider.Create(250.0, 5.0, 495.0, 12.0, not FlipY);

  FCurve[0] := TBezierControl.Create;
  FCurve[1] := TBezierControl.Create;
  FCurve[2] := TBezierControl.Create;
  FCurve[3] := TBezierControl.Create;
  FCurve[4] := TBezierControl.Create;
  FCurve[5] := TBezierControl.Create;
  FCurve[6] := TBezierControl.Create;
  FCurve[7] := TBezierControl.Create;
  FCurve[8] := TBezierControl.Create;

  FCurve[0].SetLineColor(@FControlColor);
  FCurve[1].SetLineColor(@FControlColor);
  FCurve[2].SetLineColor(@FControlColor);
  FCurve[3].SetLineColor(@FControlColor);
  FCurve[4].SetLineColor(@FControlColor);
  FCurve[5].SetLineColor(@FControlColor);
  FCurve[6].SetLineColor(@FControlColor);
  FCurve[7].SetLineColor(@FControlColor);
  FCurve[8].SetLineColor(@FControlColor);

  FCurve[0].SetCurve(64, 19, 14, 126, 118, 266, 19, 265);
  FCurve[1].SetCurve(112, 113, 178, 32, 200, 132, 125, 438);
  FCurve[2].SetCurve(401, 24, 326, 149, 285, 11, 177, 77);
  FCurve[3].SetCurve(188, 427, 129, 295, 19, 283, 25, 410);
  FCurve[4].SetCurve(451, 346, 302, 218, 265, 441, 459, 400);
  FCurve[5].SetCurve(454, 198, 14, 13, 220, 291, 483, 283);
  FCurve[6].SetCurve(301, 398, 355, 231, 209, 211, 170, 353);
  FCurve[7].SetCurve(484, 101, 222, 33, 486, 435, 487, 138);
  FCurve[8].SetCurve(143, 147, 11, 45, 83, 427, 132, 197);

  AddControl(FCurve[0]);
  AddControl(FCurve[1]);
  AddControl(FCurve[2]);
  AddControl(FCurve[3]);
  AddControl(FCurve[4]);
  AddControl(FCurve[5]);
  AddControl(FCurve[6]);
  AddControl(FCurve[7]);
  AddControl(FCurve[8]);

  FCurve[0].NoTransform;
  FCurve[1].NoTransform;
  FCurve[2].NoTransform;
  FCurve[3].NoTransform;
  FCurve[4].NoTransform;
  FCurve[5].NoTransform;
  FCurve[6].NoTransform;
  FCurve[7].NoTransform;
  FCurve[8].NoTransform;

  FSliderScaleX.Caption := 'Scale X=%.2f';
  FSliderScaleX.SetRange(0.2, 3.0);
  FSliderScaleX.Value := 1.0;
  AddControl(FSliderScaleX);
  FSliderScaleX.NoTransform;

  FSliderStartX.Caption := 'Start X=%.2f';
  FSliderStartX.SetRange(0.0, 10.0);
  FSliderStartX.Value := 0.0;
  AddControl(FSliderStartX);
  FSliderStartX.NoTransform;
end;

destructor TAggApplication.Destroy;
var
  Index: Integer;
begin
  FSliderScaleX.Free;
  FSliderStartX.Free;

  for Index := 0 to Length(FCurve) - 1 do
    FCurve[Index].Free;

  inherited;
end;

procedure TAggApplication.DrawCurve(LineImagePattern: TAggLineImagePattern;
  Rasterizer: TAggRasterizerOutlineAA; Ren: TAggRendererOutlineImage;
  Src: TAggPixelSource; Vs: TAggVertexSource);
begin
  LineImagePattern.Build(Src);
  Ren.ScaleX := FSliderScaleX.Value;
  Ren.StartX := FSliderStartX.Value;
  Rasterizer.AddPath(Vs);
end;

procedure TAggApplication.OnDraw;
var
  Pf : TAggPixelFormatProcessor;
  RenScan: TAggRendererScanLineAASolid;
  Rasterizer: TAggRasterizerScanLineAA;
  ScanLine: TAggScanLinePacked8;

  Rgba: TAggColor;

  RendererBase: TAggRendererBase;

  P: array [0..8] of TPatternSourceBrightnessToAlphaRgba8;

  PatternFilter: TAggPatternFilterBilinearRgba;
  LineImagePattern: TAggLineImagePattern;

  RenImage: TAggRendererOutlineImage;
  RasImage: TAggRasterizerOutlineAA;
begin
  // Initialize structures
  PixelFormatBgr24(Pf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Rgba.FromRgbaDouble(1.0, 1.0, 0.95);
      RendererBase.Clear(@Rgba);

      Rasterizer := TAggRasterizerScanLineAA.Create;
      try
        ScanLine := TAggScanLinePacked8.Create;
        try

          // Pattern source. Must have an interface:
          // width() const
          // height() const
          // pixel(int x, int y) const
          // Any TAggRendererBase or derived
          // is good for the use as a source.
          P[0] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[0]);
          P[1] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[1]);
          P[2] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[2]);
          P[3] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[3]);
          P[4] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[4]);
          P[5] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[5]);
          P[6] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[6]);
          P[7] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[7]);
          P[8] := TPatternSourceBrightnessToAlphaRgba8.Create(RenderingBufferImage[8]);

          PatternFilter := TAggPatternFilterBilinearRgba.Create; // Filtering functor

          // TAggLineImagePattern is the main container for the patterns. It creates
          // a copy of the patterns extended according to the needs of the filter.
          // TAggLineImagePattern can operate with arbitrary image width, but if the
          // width of the pattern is power of 2, it's better to use the modified
          // version TAggLineImagePatternPow2 because it works about 15-25 percent
          // faster than TAggLineImagePattern (because of using simple masking instead
          // of expensive '%' operation).

          // -- Create with specifying the source
          // LineImagePattern := TAggLineImagePattern.Create(PatternFilter, Src);

          // -- Create uninitialized and set the source
          LineImagePattern := TAggLineImagePattern.Create(PatternFilter);
          RenImage := TAggRendererOutlineImage.Create(RendererBase, LineImagePattern);
          RasImage := TAggRasterizerOutlineAA.Create(RenImage);
          try
            DrawCurve(LineImagePattern, RasImage, RenImage, P[0], FCurve[0].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[1], FCurve[1].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[2], FCurve[2].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[3], FCurve[3].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[4], FCurve[4].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[5], FCurve[5].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[6], FCurve[6].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[7], FCurve[7].GetCurve);
            DrawCurve(LineImagePattern, RasImage, RenImage, P[8], FCurve[8].GetCurve);
          finally
            P[0].Free;
            P[1].Free;
            P[2].Free;
            P[3].Free;
            P[4].Free;
            P[5].Free;
            P[6].Free;
            P[7].Free;
            P[8].Free;

            PatternFilter.Free;
            LineImagePattern.Free;
            RenImage.Free;
            RasImage.Free;
          end;

          // Render the controls
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[0]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[1]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[2]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[3]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[4]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[5]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[6]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[7]);
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve[8]);

          RenderControl(Rasterizer, ScanLine, RenScan, FSliderScaleX);
          RenderControl(Rasterizer, ScanLine, RenScan, FSliderStartX);
        finally
          ScanLine.Free;
        end;
      finally
        Rasterizer.Free;
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
var
  TextFile : Text;
  Str: AnsiString;
begin
  if Key = Byte(' ') then
  begin
    AssignFile(TextFile, 'coord');
    Rewrite(TextFile);

    Str := Format('%.0f, %.0f, %.0f, %.0f, %.0f, %.0f, %.0f, %.0f', [
      FCurve[0].X1, FCurve[0].Y1, FCurve[0].X2, FCurve[0].Y2,
      FCurve[0].X3, FCurve[0].Y3, FCurve[0].X4, FCurve[0].Y4]);

    Write(TextFile, PAnsiChar(@Str[1]));
    Close(TextFile);
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('The demo shows a very powerful mechanism of using '
      + 'arbitrary images as line patterns. The main point of it is that '
      + 'the images are drawn along the path. It allows you to draw very '
      + 'fancy looking lines quite easily and very useful in GIS/cartography '
      + 'applications. There the bilinear filtering is used, but it''s also '
      + 'possible to add any other filtering methods, or just use the nearest '
      + 'neighbour one for the sake of speed. Actually, the algorithm uses '
      + '32bit images with alpha channel, but in this demo alpha is '
      + 'simulated in such a way that wite is transparent, black is opaque. '
      + 'The intermediate colors have intermediate opacity that is defined by '
      + 'the BrightnessToAlpha array.'#13 + 'How to play with:'#13
      + 'In the demo you can drag the control points of the curves and observe '
      + 'that the images are transformed quite consistently and smoothly. '
      + 'You can also try to replace the image files (1…9) with your own. '
      + 'The BMP files must have 24bit colors (TrueColor), the PPM ones must '
      + 'be of type "P6". Also, the heigh should not exceed 64 pixels, and the '
      + 'background should be white or very close to white.'
      + 'Press the spacebar to write down the "coord" file of the curve 1 '
      + '(of 1.bmp).'#13#13 + 'Note: F2 key saves current "screenshot"'
      + 'file in this demo''s directory.');
end;

var
  Ext: AnsiString;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Drawing Lines with Image Patterns (F1-Help)';

    if not LoadImage(0, '1') or not LoadImage(1, '2') or
      not LoadImage(2, '3') or not LoadImage(3, '4') or
      not LoadImage(4, '5') or not LoadImage(5, '6') or
      not LoadImage(6, '7') or not LoadImage(7, '8') or
      not LoadImage(8, '9') then
    begin
      Ext := ImageExtension;

      DisplayMessage(Format('There must be files 1%s...9%s'#13 +
        'Download and unzip:'#13 +
        'http://www.antigrain.com/line_patterns.bmp.zip'#13 + 'or'#13 +
        'http://www.antigrain.com/line_patterns.ppm.tar.gz'#13, [Ext, Ext]));
    end
    else if Init(500, 450, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
