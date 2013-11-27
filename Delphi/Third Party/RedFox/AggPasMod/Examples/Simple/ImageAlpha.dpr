program ImageAlpha;

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
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSplineControl in '..\..\Source\Controls\AggSplineControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggSpanImageFilterRgb in '..\..\Source\AggSpanImageFilterRgb.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanConverter in '..\..\Source\AggSpanConverter.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas';

const
  CFlipY = True;
  CArraySize = 256 * 3;

type
  TAggSpanConvBrightnessAlphaRgb8 = class(TAggSpanConvertor)
  private
    FAlphaArray: PInt8u;
  public
    constructor Create(AlphaArray: PInt8u);

    procedure Convert(Span: PAggColor; X, Y: Integer; Len: Cardinal); override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSplineControlAlpha: TSplineControl;

    FPosition, FRadius: array [0..49] of TPointDouble;

    FColors: array [0..49] of TAggColor;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggSpanConvBrightnessAlphaRgb8 }

constructor TAggSpanConvBrightnessAlphaRgb8.Create;
begin
  FAlphaArray := AlphaArray;
end;

procedure TAggSpanConvBrightnessAlphaRgb8.Convert;
begin
  repeat
    Span.Rgba8.A := PInt8u(PtrComp(FAlphaArray) + (Span.Rgba8.R +
      Span.Rgba8.G + Span.Rgba8.B) * SizeOf(Int8u))^;

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSplineControlAlpha := TSplineControl.Create(2, 2, 200, 30, 6, not FlipY);

  FSplineControlAlpha.SetValue(0, 1.0);
  FSplineControlAlpha.SetValue(1, 1.0);
  FSplineControlAlpha.SetValue(2, 1.0);
  FSplineControlAlpha.SetValue(3, 0.5);
  FSplineControlAlpha.SetValue(4, 0.5);
  FSplineControlAlpha.SetValue(5, 1.0);
  FSplineControlAlpha.UpdateSpline;

  AddControl(FSplineControlAlpha);
end;

destructor TAggApplication.Destroy;
begin
  FSplineControlAlpha.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
var
  I: Integer;

begin
  for I := 0 to 49 do
  begin
    FPosition[I] := PointDouble(Width * Random, Height * Random);
    FRadius[I] := PointDouble(Random(60) + 10, Random(60) + 10);

    FColors[I].FromRgbaDouble(Random, Random, Random, Random);
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  SpanAllocator: TAggSpanAllocator;
  Sg: TAggSpanImageFilterRgbBilinear;
  Sc: TAggSpanConverter;
  Ri: TAggRendererScanLineAA;
  Sl: TAggScanLineUnpacked8;
  Tr: TAggConvTransform;

  Ras: TAggRasterizerScanLineAA;
  Ellipse: TAggEllipse;

  Interpolator: TAggSpanInterpolatorLinear;

  SourceMatrix, ImageMatrix: TAggTransAffine;

  I: Cardinal;

  BrightnessAlphaArray: array [0..CArraySize - 1] of Int8u;

  ColorAlpha: TAggSpanConvBrightnessAlphaRgb8;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Compute
      SourceMatrix := TAggTransAffine.Create;

      SourceMatrix.Translate(-FInitialWidth * 0.5, -FInitialHeight * 0.5);
      SourceMatrix.Rotate(Deg2Rad(10));
      SourceMatrix.Translate(FInitialWidth * 0.5, FInitialHeight * 0.5);

      SourceMatrix.Multiply(GetTransAffineResizing);

      ImageMatrix := TAggTransAffine.Create;
      ImageMatrix.Assign(SourceMatrix);
      ImageMatrix.Invert;

      for I := 0 to CArraySize - 1 do
        BrightnessAlphaArray[I] :=
          Int8u(Trunc(FSplineControlAlpha.GetValue(I / CArraySize) * 255.0));

      ColorAlpha := TAggSpanConvBrightnessAlphaRgb8.Create(@BrightnessAlphaArray);
      try
        // Render
        SpanAllocator := TAggSpanAllocator.Create;
        Interpolator := TAggSpanInterpolatorLinear.Create(ImageMatrix);

        Rgba.FromRgbaDouble(1, 1, 1, 0);
        Sg := TAggSpanImageFilterRgbBilinear.Create(SpanAllocator,
          RenderingBufferImage[0], @Rgba, Interpolator, CAggOrderBgr);

        Sc := TAggSpanConverter.Create(Sg, ColorAlpha);
        Ri := TAggRendererScanLineAA.Create(RendererBase, Sc);
        try
          Ellipse := TAggEllipse.Create;
          Ras := TAggRasterizerScanLineAA.Create;
          Sl := TAggScanLineUnpacked8.Create;

          for I := 0 to 49 do
          begin
            Ellipse.Initialize(FPosition[I], FRadius[I], 50);
            RenScan.SetColor(@FColors[I]);

            Ras.AddPath(Ellipse);
            RenderScanLines(Ras, Sl, RenScan);
          end;

          Ellipse.Initialize(FInitialWidth * 0.5, FInitialHeight * 0.5,
            FInitialWidth / 1.9, FInitialHeight / 1.9, 200);

          Tr := TAggConvTransform.Create(Ellipse, SourceMatrix);
          try
            Ras.AddPath(Tr);
          finally
            Tr.Free;
          end;
          RenderScanLines(Ras, Sl, Ri);

          // Render the controls
          RenderControl(Ras, Sl, RenScan, FSplineControlAlpha);
        finally
          Ri.Free;
        end;

        // Free AGG resources
        Ras.Free;
        Sl.Free;

        SourceMatrix.Free;
        ImageMatrix.Free;
        SpanAllocator.Free;
        Interpolator.Free;

        Sg.Free;
        Sc.Free;
        Ellipse.Free;
      finally
        ColorAlpha.Free;
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
  Fd: Text;
  I, Alpha: Integer;
begin
  if Key = Byte(' ') then
  begin
    AssignFile(Fd, 'alpha');
    Rewrite(Fd);

    for I := 0 to CArraySize - 1 do
    begin
      Alpha := Int8u(Trunc(FSplineControlAlpha.GetValue(I / CArraySize) * 255));

      if I mod 32 = 0 then
        Writeln(Fd);

      write(Fd, Alpha:3, ', ');
    end;

    Close(Fd);
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('A very powerful feature that allows you to simulate the '
      + 'alpha-channel on the basis of some functioon. In this example it''s '
      + 'brightness, but it can be of any complexity. In the example you can '
      + 'form the brightness function and watch for the translucency.'#13#13
      + 'How to play with:'#13#13
      + 'Resize the windows to move the image over the background. Press the '
      + 'spacebar to write down file "alpha" with current alpha values.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Text: AnsiString;
  ImageName, P, N, X: ShortString;

begin
  ImageName := 'spheres';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF}

  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'Image Affine Transformations with Alpha-function (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(RenderingBufferImage[0].Width,
      RenderingBufferImage[0].Height, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
