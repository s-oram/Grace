program AlphaMask;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',
  AggPixelFormatGray in '..\..\Source\AggPixelFormatGray.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggAlphaMaskUnpacked8 in '..\..\Source\AggAlphaMaskUnpacked8.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggParseLion in 'AggParseLion.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FAlphaSize: Cardinal;
    FAlphaBuffer: PInt8U;
    FPixelFormats: TAggPixelFormatProcessor;

    FPath : TAggPathStorage;
    FColors : array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FAngle, FScale: Double;
    FBaseDelta, FSkew: TPointDouble;

    FAlphaMaskRenderingBuffer: TAggRenderingBuffer;
    FAlphaMask: TAggAlphaMaskGray8;
    FRasterizer: TAggRasterizerScanLineAA;
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


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FAlphaSize := 0;
  FAlphaBuffer := nil;

  // Initialize structures
  PixelFormatBgr24(FPixelFormats, RenderingBufferWindow);

  // Rendering
  FPath := TAggPathStorage.Create;
  FPathCount := 0;

  FBoundingRect := RectDouble(0, 0, 0, 0);

  FBaseDelta.x := 0;
  FBaseDelta.y := 0;

  FAngle := 0;
  FScale := 1;

  FSkew.X := 0;
  FSkew.Y := 0;

  FAlphaMaskRenderingBuffer := TAggRenderingBuffer.Create;
  FAlphaMask := TAggAlphaMaskGray8.Create(FAlphaMaskRenderingBuffer);
  FRasterizer := TAggRasterizerScanLineAA.Create;

  ParseLion;
end;

destructor TAggApplication.Destroy;
begin
  FRasterizer.Free;
  FAlphaMask.Free;
  FAlphaMaskRenderingBuffer.Free;
  FPath.Free;

  FPixelFormats.Free;

  AggFreeMem(Pointer(FAlphaBuffer), FAlphaSize);

  inherited;
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);
  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);
  FBaseDelta := PointDouble(FBoundingRect.CenterX, FBoundingRect.CenterY);
end;

procedure TAggApplication.GenerateAlphaMask(Cx, Cy: Integer);
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RendererScanLine: TAggRendererScanLineAASolid;
  ScanLine: TAggScanLinePacked8;

  Ellipse: TAggEllipse;

  I: Cardinal;
begin
  AggFreeMem(Pointer(FAlphaBuffer), FAlphaSize);

  FAlphaSize := Cx * Cy;

  AggGetMem(Pointer(FAlphaBuffer), FAlphaSize);

  FAlphaMaskRenderingBuffer.Attach(FAlphaBuffer, Cx, Cy, Cx);

  PixelFormatGray8(PixelFormatProcessor, FAlphaMaskRenderingBuffer);
  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RendererScanLine := TAggRendererScanLineAASolid.Create(RendererBase);
    ScanLine := TAggScanLinePacked8.Create;
    try
      Rgba.Clear;
      RendererBase.Clear(@Rgba);

      Ellipse := TAggEllipse.Create;
      try
        for I := 0 to 9 do
        begin
          Ellipse.Initialize(PointDouble(Random(Cx), Random(Cy)),
            PointDouble(Random(100) + 20, Random(100) + 20), 100);

          FRasterizer.AddPath(Ellipse);

          Rgba.FromValueInteger(Random($100), Random($100));
          RendererScanLine.SetColor(@Rgba);

          RenderScanLines(FRasterizer, ScanLine, RendererScanLine);
        end;
      finally
        Ellipse.Free;
      end;
    finally
      ScanLine.Free;
      RendererScanLine.Free;
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
  AWidth, AHeight: Integer;

  RendererBase: TAggRendererBase;
  RendererScanLine: TAggRendererScanLineAASolid;
  ScanLine: TAggScanLineUnpacked8AlphaMask;

  Trans: TAggConvTransform;

  Mtx: TAggTransAffine;
begin
  AWidth := RenderingBufferWindow.Width;
  AHeight := RenderingBufferWindow.Height;

  RendererBase := TAggRendererBase.Create(FPixelFormats);
  try
    RendererScanLine := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      ScanLine := TAggScanLineUnpacked8AlphaMask.Create(FAlphaMask);
      try
        RendererBase.Clear(CRgba8White);

        // Transform lion
        Mtx := TAggTransAffine.Create;
        try
          // Transformation
          TransAffineTranslation(Mtx, -FBaseDelta.x, -FBaseDelta.y);
          Mtx.Scale(FScale);
          Mtx.Rotate(FAngle + Pi);
          Mtx.Translate(AWidth * 0.5, AHeight * 0.5);
          Mtx.Skew(FSkew.X * 1E-3, FSkew.Y * 1E-3);

          // This code renders the lion
          Trans := TAggConvTransform.Create(FPath, Mtx);
          try
            RenderAllPaths(FRasterizer, ScanLine, RendererScanLine, Trans,
              @FColors, @FPathIndex, FPathCount);
          finally
            Trans.Free;
          end;
        finally
          Mtx.Free;
        end;
      finally
        ScanLine.Free;
      end;
    finally
      RendererScanLine.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.Transform;
begin
  X := X - AWidth * 0.5;
  Y := Y - AHeight * 0.5;

  FAngle := ArcTan2(Y, X);
  FScale := Sqrt(Sqr(Y) + Sqr(X)) * 1E-2;
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
    DisplayMessage('Alpha-mask is a simple method of clipping and masking '
      + 'polygons to a number of other arbitrary polygons. Alpha mask is a '
      + 'buffer that is mixed to the scanLine container and controls the '
      + 'Anti-Aliasing values in it. It''s not the perfect mechanism of '
      + 'clipping, but it allows you not only to clip the polygons, but also '
      + 'to change the opacity in certain areas, i.e., the clipping can be '
      + 'translucent. '#13#13
      + 'How to play with:'#13#13
      + 'Press and drag the left mouse button to scale and rotate the lion.'#13
      + 'Resize the window to generate new alpha-mask.'#13
      + 'Use the right mouse button to skew the lion.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  Randomize;
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Lion with Alpha-Masking (F1-Help)';

    if Init(512, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
