program PolymorphicRenderer;

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
  AggPixelFormatRgbPacked in '..\..\Source\AggPixelFormatRgbPacked.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas';

const
  CFlipY = True;

//  CPixelFormat = pfRgb555;
//  CPixelFormat = pfRgb565;
//  CPixelFormat = pfRgb24;
  CPixelFormat = pfBgr24;
//  CPixelFormat = pfRgba32;
//  CPixelFormat = pfArgb32;
//  CPixelFormat = pfAbgr32;
//  CPixelFormat = pfBgra32;

type
  TAggCustomPolymorphicRendererSolidRgba8Adaptor = class
  public
    procedure Clear(C: PAggColor); virtual; abstract;
    procedure SetColor(C: PAggColor); virtual; abstract;
  end;

  TPolymorphicRendererSolidRgba8Adaptor = class(
    TAggCustomPolymorphicRendererSolidRgba8Adaptor)
  private
    FPixelFormat: TAggPixelFormatProcessor;
    FRendererBase: TAggRendererBase;
    FRen: TAggRendererScanLineAASolid;
    FOwnsRenderer: Boolean;
    function GetRendererBase: TAggRendererScanLineAASolid;
  public
    constructor Create(PixelFormat: TPixelFormat;
      RenderingBuffer: TAggRenderingBuffer);
    destructor Destroy; override;

    procedure Clear(C: PAggColor); override;
    procedure SetColor(C: PAggColor); override;

    property RendererBase: TAggRendererScanLineAASolid read GetRendererBase;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FTrianglePoint: array [0..2] of TPointDouble;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal;
      Flags: TMouseKeyboardFlags); override;
  end;


{ TPolymorphicRendererSolidRgba8Adaptor }

constructor TPolymorphicRendererSolidRgba8Adaptor.Create(
  PixelFormat: TPixelFormat; RenderingBuffer: TAggRenderingBuffer);
begin
  FOwnsRenderer := False;

  case PixelFormat of
    pfRgb555:
      PixelFormatRgb555(FPixelFormat, RenderingBuffer);

    pfRgb565:
      PixelFormatRgb565(FPixelFormat, RenderingBuffer);

    pfRgb24:
      PixelFormatRgb24(FPixelFormat, RenderingBuffer);

    pfBgr24:
      PixelFormatBgr24(FPixelFormat, RenderingBuffer);

    pfRgba32:
      PixelFormatRgba32(FPixelFormat, RenderingBuffer);

    pfArgb32:
      PixelFormatArgb32(FPixelFormat, RenderingBuffer);

    pfAbgr32:
      PixelFormatAbgr32(FPixelFormat, RenderingBuffer);

    pfBgra32:
      PixelFormatBgra32(FPixelFormat, RenderingBuffer);
    else
//      PixelFormatUndefined(FPixelFormat);
  end;

  if FPixelFormat.RenderingBuffer <> nil then
  begin
    FRendererBase := TAggRendererBase.Create(FPixelFormat);
    FRen := TAggRendererScanLineAASolid.Create(FRendererBase);
    FOwnsRenderer := True;
  end;
end;

destructor TPolymorphicRendererSolidRgba8Adaptor.Destroy;
begin
  if FOwnsRenderer then
  begin
    FRen.Free;
    FRendererBase.Free;
  end;

  FPixelFormat.Free;

  inherited;
end;

procedure TPolymorphicRendererSolidRgba8Adaptor.Clear(C: PAggColor);
begin
  if FPixelFormat.RenderingBuffer <> nil then
    FRendererBase.Clear(C);
end;

procedure TPolymorphicRendererSolidRgba8Adaptor.SetColor(C: PAggColor);
begin
  if FPixelFormat.RenderingBuffer <> nil then
    FRen.SetColor(C);
end;

function TPolymorphicRendererSolidRgba8Adaptor.GetRendererBase:
  TAggRendererScanLineAASolid;
begin
  if FPixelFormat.RenderingBuffer <> nil then
    Result := FRen
  else
    Result := nil;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FTrianglePoint[0].X := 100;
  FTrianglePoint[0].Y := 60;
  FTrianglePoint[1].X := 369;
  FTrianglePoint[1].Y := 170;
  FTrianglePoint[2].X := 143;
  FTrianglePoint[2].Y := 310;
end;

procedure TAggApplication.OnDraw;
var
  Ren: TPolymorphicRendererSolidRgba8Adaptor;
  Ras: TAggRasterizerScanLineAA;
  Sl : TAggScanLinePacked8;

  Rgba: TAggColor;
  Path: TAggPathStorage;
begin
  // Create Path
  Path := TAggPathStorage.Create;

  Path.MoveTo(FTrianglePoint[0].X, FTrianglePoint[0].Y);
  Path.LineTo(FTrianglePoint[1].X, FTrianglePoint[1].Y);
  Path.LineTo(FTrianglePoint[2].X, FTrianglePoint[2].Y);

  Path.ClosePolygon;

  // Rasterizer, ScanLines & Polymorphic Renderer class factory
  Ren := TPolymorphicRendererSolidRgba8Adaptor.Create(CPixelFormat,
    RenderingBufferWindow);
  try
    Ras := TAggRasterizerScanLineAA.Create;
    Sl := TAggScanLinePacked8.Create;

    // Render
    if (Ren <> nil) and (Ren.RendererBase <> nil) then
    begin
      Rgba.White;
      Ren.Clear(@Rgba);

      Rgba.FromRgbaInteger(80, 30, 20);
      Ren.SetColor(@Rgba);

      Ras.AddPath(Path);

      RenderScanLines(Ras, Sl, Ren.RendererBase);
    end;

    // Free AGG resources
    Ras.Free;
    Sl.Free;
  finally
    Ren.Free;
  end;

  Path.Free;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('There''s nothing looking effective. AGG has Renderers '
      + 'for different pixel formats in memory, particularly, for different '
      + 'byte order (RGB or BGR). But the Renderers are class templates'
      + '(only C++), where byte order is defined at the compile time. '
      + 'It''s done for the sake of performance and in most cases it fits '
      + 'all your needs. Still, if you need to switch between different '
      + 'pixel formats dynamically, you can write a simple polymorphic '
      + 'class wrapper, like the one in this example.'#13#13
      + 'How to play with:'#13#13
      + 'To use another pixel format for rendering, comment/uncomment'#13
      + 'the CPixelFormat constant in the demo source code and recompile it.'
      + #13#13 + 'Note: F2 key saves current "screenshot" file in this '
      + 'demo''s directory.');
end;

begin
  with TAggApplication.Create(CPixelFormat, CFlipY) do
  try
    Caption := 'AGG Example. Polymorphic Renderers (F1-Help)';

    if Init(400, 330, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
