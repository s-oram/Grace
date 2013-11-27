program AntiAliasDemo;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas';

const
  CFlipY = True;

type
  TSquare = class
  private
    FSize: Double;
  public
    constructor Create(Size: Double);

    procedure Draw(Rasterizer: TAggRasterizerScanLineAA; ScanLine: TAggCustomScanLine;
      Renderer: TAggCustomRendererScanLineSolid; X, Y: Double);
  end;

  TRendererEnlarged = class(TAggCustomRendererScanLineSolid)
  private
    FRastrizerScanLine: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;
    FRenderScanLine: TAggRendererScanLineAASolid;

    FSquare: TSquare;
    FColor: TAggColor;
    FSize: Double;
  public
    constructor Create(Renderer: TAggRendererScanLineAASolid; Size: Double);
    destructor Destroy; override;

    procedure SetColor(C: PAggColor); override;
    procedure SetColor(C: TAggRgba8); override;
    procedure Prepare(U: Cardinal); override;
    procedure Render(ScanLine: TAggCustomScanLine); override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FX, FY: array [0..2] of Double;
    FDelta: TPointDouble;
    FIndex: Integer;
    FPixelformats: TAggPixelFormatProcessor;
    FSlider: array [0..1] of TAggControlSlider;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ Square }

constructor TSquare.Create;
begin
  FSize := Size;
end;

procedure TSquare.Draw;
begin
  Rasterizer.Reset;

  Rasterizer.MoveToDouble(X * FSize, Y * FSize);
  Rasterizer.LineToDouble(X * FSize + FSize, Y * FSize);
  Rasterizer.LineToDouble(X * FSize + FSize, Y * FSize + FSize);
  Rasterizer.LineToDouble(X * FSize, Y * FSize + FSize);

  RenderScanLines(Rasterizer, ScanLine, Renderer);
end;


{ TRendererEnlarged }

constructor TRendererEnlarged.Create(Renderer: TAggRendererScanLineAASolid;
  Size: Double);
begin
  FSquare := TSquare.Create(Size);

  FRastrizerScanLine := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;

  FRenderScanLine := Renderer;
  FSize := Size;
end;

destructor TRendererEnlarged.Destroy;
begin
  FRastrizerScanLine.Free;
  FScanLine.Free;
  FSquare.Free;

  inherited;
end;

procedure TRendererEnlarged.SetColor(C: PAggColor);
begin
  FColor := C^;
end;

procedure TRendererEnlarged.SetColor(C: TAggRgba8);
begin
  FColor.Rgba8 := C;
end;

procedure TRendererEnlarged.Prepare(U: Cardinal);
begin
end;

procedure TRendererEnlarged.Render(ScanLine: TAggCustomScanLine);
var
  Y, X, NumPixel: Integer;
  A: Cardinal;
  NumSpans: Cardinal;
  Span: PAggSpanUnpacked8;
  Covers: PInt8u;
  Rgba: TAggColor;
begin
  Y := ScanLine.Y;
  NumSpans := ScanLine.NumSpans;
  Span := ScanLine.GetBegin;

  repeat
    X := Span.X;
    Covers := Span.Covers;
    NumPixel := Span.Len;

    repeat
      A := ShrInt32(Covers^ * FColor.Rgba8.A, 8);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      Rgba.FromRgbaInteger(FColor.Rgba8.R, FColor.Rgba8.G, FColor.Rgba8.B, A);
      FRenderScanLine.SetColor(@Rgba);
      FSquare.Draw(FRastrizerScanLine, FScanLine, FRenderScanLine, X, Y);

      Inc(X);
      Dec(NumPixel);
    until NumPixel = 0;

    Dec(NumSpans);
  until NumSpans = 0;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FIndex := -1;

  FX[0] := 57;
  FY[0] := 100;
  FX[1] := 369;
  FY[1] := 170;
  FX[2] := 143;
  FY[2] := 310;

  FSlider[0] := TAggControlSlider.Create(80, 10, 600 - 10, 19, not FlipY);
  FSlider[1] := TAggControlSlider.Create(80, 10 + 20, 600 - 10, 19 + 20,
    not FlipY);

  FSlider[0].SetRange(8.0, 100.0);
  FSlider[0].NumSteps := 23;
  FSlider[0].Value := 32.0;

  FSlider[1].SetRange(0.1, 3.0);
  FSlider[1].Value := 1.0;

  FSlider[0].Caption := 'Pixel size=%1.0f';
  FSlider[1].Caption := 'Gamma=%4.3f';

  AddControl(FSlider[0]);
  AddControl(FSlider[1]);

  FSlider[0].NoTransform;
  FSlider[1].NoTransform;

  // Initialize structures
  PixelFormatBgr24(FPixelformats, RenderingBufferWindow);
end;

destructor TAggApplication.Destroy;
begin
  FSlider[0].Free;
  FSlider[1].Free;

  FPixelformats.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  SizeMul: Integer;
  SizeMulInv: Double;
  RendererBase : TAggRendererBase;
  Renderer: TAggRendererScanLineAASolid;
  Rasterizer: TAggRasterizerScanLineAA;
  ScanLine : TAggScanLineUnpacked8;

  Rgba : TAggColor;
  GammaNone: TAggGammaNone;
  GammaPower: TAggGammaPower;

  RendererEnlarged: TRendererEnlarged;

  PathStroke: TAggPathStorage;
  Pg: TAggConvStroke;
begin
  RendererBase := TAggRendererBase.Create(FPixelFormats);
  try
    Renderer := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      ScanLine := TAggScanLineUnpacked8.Create;
      Rasterizer := TAggRasterizerScanLineAA.Create;

      // Draw Zoomed Triangle
      SizeMul := Trunc(FSlider[0].Value);

      GammaPower := TAggGammaPower.Create(FSlider[1].Value);
      try
        Rasterizer.Gamma(GammaPower);
      finally
        GammaPower.Free;
      end;

      RendererEnlarged := TRendererEnlarged.Create(Renderer, SizeMul);
      try
        RendererEnlarged.SetColor(CRgba8Black);

        SizeMulInv := 1 / SizeMul;
        Rasterizer.Reset;
        Rasterizer.MoveToDouble(FX[0] * SizeMulInv, FY[0] * SizeMulInv);
        Rasterizer.LineToDouble(FX[1] * SizeMulInv, FY[1] * SizeMulInv);
        Rasterizer.LineToDouble(FX[2] * SizeMulInv, FY[2] * SizeMulInv);

        RenderScanLines(Rasterizer, ScanLine, RendererEnlarged);

        // Draw final triangle bottom-left
        Renderer.SetColor(CRgba8Black);

        RenderScanLines(Rasterizer, ScanLine, Renderer);

        // Draw The Supposed Triangle over
        GammaNone := TAggGammaNone.Create;
        try
          Rasterizer.Gamma(GammaNone);
        finally
          GammaNone.Free;
        end;

        PathStroke := TAggPathStorage.Create;
        Pg := TAggConvStroke.Create(PathStroke);
        Pg.Width := 2.0;
        try
          Rgba.FromRgbaInteger(0, 150, 160, 200);
          Renderer.SetColor(@Rgba);

          PathStroke.RemoveAll;
          PathStroke.MoveTo(FX[0], FY[0]);
          PathStroke.LineTo(FX[1], FY[1]);

          Rasterizer.AddPath(Pg);

          RenderScanLines(Rasterizer, ScanLine, Renderer);

          PathStroke.RemoveAll;
          PathStroke.MoveTo(FX[1], FY[1]);
          PathStroke.LineTo(FX[2], FY[2]);

          Rasterizer.AddPath(Pg);

          RenderScanLines(Rasterizer, ScanLine, Renderer);

          PathStroke.RemoveAll;
          PathStroke.MoveTo(FX[2], FY[2]);
          PathStroke.LineTo(FX[0], FY[0]);

          Rasterizer.AddPath(Pg);
        finally
          Pg.Free;
          PathStroke.Free;
        end;

        RenderScanLines(Rasterizer, ScanLine, Renderer);

        // Render the controls
        RenderControl(Rasterizer, ScanLine, Renderer, FSlider[0]);
        RenderControl(Rasterizer, ScanLine, Renderer, FSlider[1]);

        // Free AGG resources
        Rasterizer.Free;
        ScanLine.Free;
      finally
        RendererEnlarged.Free;
      end;
    finally
      Renderer.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Delta: TPointDouble;
begin
  if mkfMouseLeft in Flags then
  begin
    if FIndex = 3 then
    begin
      Delta.X := X - FDelta.X;
      Delta.Y := Y - FDelta.Y;

      FX[1] := FX[1] - (FX[0] - Delta.X);
      FY[1] := FY[1] - (FY[0] - Delta.Y);
      FX[2] := FX[2] - (FX[0] - Delta.X);
      FY[2] := FY[2] - (FY[0] - Delta.Y);
      FX[0] := Delta.X;
      FY[0] := Delta.Y;

      ForceRedraw;
      Exit;
    end;

    if FIndex >= 0 then
    begin
      FX[FIndex] := X - FDelta.X;
      FY[FIndex] := Y - FDelta.Y;

      ForceRedraw;
    end;
  end
  else
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  I: Cardinal;
begin
  if mkfMouseLeft in Flags then
  begin
    I := 0;

    while I < 3 do
    begin
      if Sqrt((X - FX[I]) * (X - FX[I]) + (Y - FY[I]) * (Y - FY[I])) < 10
      then
      begin
        FDelta.X := X - FX[I];
        FDelta.Y := Y - FY[I];
        FIndex := I;

        Break;
      end;

      Inc(I);
    end;

    if I = 3 then
      if PointInTriangle(FX[0], FY[0], FX[1], FY[1], FX[2], FY[2], X, Y)
      then
      begin
        FDelta.X := X - FX[0];
        FDelta.Y := Y - FY[0];
        FIndex := 3;
      end;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FIndex := -1;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Demonstration of the Anti-Aliasing principle with subpixel '
      + 'accuracy. The triangle is rendered two times, with its "natural" size '
      + '(at the bottom-left) and enlarged. To draw the enlarged version there '
      + 'was a special scanLine renderer written (see class TRendererEnlarged '
      + 'in the source code).'#13#13
      + 'How to play with:'#13#13
      + 'You can drag the whole triangle as well as each vertex of it.'#13
      + 'Also change "Gamma" to see how it affects the quality of Anti-'
      + 'Aliasing.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Anti-Aliasing Demo (F1-Help)';

    if Init(600, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
