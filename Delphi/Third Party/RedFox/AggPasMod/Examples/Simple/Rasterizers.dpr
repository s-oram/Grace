program Rasterizers;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$DEFINE AGG_BGR24}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutline in '..\..\Source\AggRasterizerOutline.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggScanLineBin in '..\..\Source\AggScanLineBin.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas'
{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FX, FY: array [0..2] of Double;

    FDelta: TPointDouble;

    FIndex: Integer;

    FSliderGamma, FSliderAlpha: TAggControlSlider;
    FCheckBoxTest: TAggControlCheckBox;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLineP8: TAggScanLinePacked8;
    FScanLineBin: TAggScanLineBin;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure DrawAntiAliased;
    procedure DrawAliased;

    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); override;
    procedure OnControlChange; override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLineP8 := TAggScanLinePacked8.Create;
  FScanLineBin := TAggScanLineBin.Create;

  FIndex := -1;

  FX[0] := 100 + 120;
  FY[0] := 60;
  FX[1] := 369 + 120;
  FY[1] := 170;
  FX[2] := 143 + 120;
  FY[2] := 310;

  FSliderGamma := TAggControlSlider.Create(140, 14, 280, 22, not CFlipY);

  AddControl(FSliderGamma);

  FSliderGamma.SetRange(0, 1);
  FSliderGamma.Value := 0.5;
  FSliderGamma.Caption := 'Gamma=%1.2f';
  FSliderGamma.NoTransform;

  FSliderAlpha := TAggControlSlider.Create(290, 14, 490, 22, not CFlipY);
  FSliderAlpha.SetRange(0, 1);
  FSliderAlpha.Value := 1;
  FSliderAlpha.Caption := 'Alpha=%1.2f';
  AddControl(FSliderAlpha);
  FSliderAlpha.NoTransform;

  FCheckBoxTest := TAggControlCheckBox.Create(140.0, 30, 'Test Performance',
    not CFlipY);
  AddControl(FCheckBoxTest);
  FCheckBoxTest.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FSliderAlpha.Free;
  FSliderGamma.Free;
  FCheckBoxTest.Free;

  FRasterizer.Free;
  FScanLineP8.Free;
  FScanLineBin.Free;

  inherited;
end;

procedure TAggApplication.DrawAntiAliased;
var
  Pixf  : TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
  RenAA: TAggRendererScanLineAASolid;

  Path: TAggPathStorage;
  Rgba: TAggColor;

  Gamma: TAggGammaPower;
begin
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenAA := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      // Path & Color
      Path := TAggPathStorage.Create;
      try
        Path.MoveTo(FX[0], FY[0]);
        Path.LineTo(FX[1], FY[1]);
        Path.LineTo(FX[2], FY[2]);
        Path.ClosePolygon;

        Rgba.FromRgbaDouble(0.7, 0.5, 0.1, FSliderAlpha.Value);
        RenAA.SetColor(@Rgba);

        // Draw
        Gamma := TAggGammaPower.Create(FSliderGamma.Value * 2.0);
        try
          FRasterizer.Gamma(Gamma);
        finally
          Gamma.Free;
        end;
        FRasterizer.AddPath(Path);

        RenderScanLines(FRasterizer, FScanLineP8, RenAA);
      finally
        Path.Free;
      end;
    finally
      RenAA.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.DrawAliased;
var
  Pixf: TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
  RenBin: TAggRendererScanLineBinSolid;

  Path: TAggPathStorage;
  Rgba: TAggColor;

  Gamma: TAggGammaThreshold;

  RenPrim: TAggRendererPrimitives;
  RasLine: TAggRasterizerOutline;
begin
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenBin := TAggRendererScanLineBinSolid.Create(RendererBase);
    try
      // Path & Color
      Path := TAggPathStorage.Create;
      try
        Path.MoveTo(FX[0] - 200, FY[0]);
        Path.LineTo(FX[1] - 200, FY[1]);
        Path.LineTo(FX[2] - 200, FY[2]);
        Path.ClosePolygon;

        Rgba.FromRgbaDouble(0.1, 0.5, 0.7, FSliderAlpha.Value);
        RenBin.SetColor(@Rgba);

        // Draw
        Gamma := TAggGammaThreshold.Create(FSliderGamma.Value);
        try
          FRasterizer.Gamma(Gamma);
          FRasterizer.AddPath(Path);

          RenderScanLines(FRasterizer, FScanLineBin, RenBin);

          // -- Drawing an outline with subpixel accuracy (aliased)
          (*
            RenPrim := TAggRendererPrimitives(rb);
            RasLine := TAggRasterizerOutline.Create(@RenPrim);

            Rgba.Black;
            RenPrim.GetLineColor(@rgba);
            RasLine.AddPath(Path);
            RenPrim.Free;
          (* *)
        finally
          Gamma.Free;
        end;
      finally
        Path.Free;
      end;
    finally
      RenBin.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf  : TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
  RenAA: TAggRendererScanLineAASolid;

  RasAA: TAggRasterizerScanLineAA;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenAA := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RasAA := TAggRasterizerScanLineAA.Create;

      // Setup colors & background
      RendererBase.Clear(CRgba8White);

      // Draw
      DrawAntiAliased;
      DrawAliased;

      // Render controls
      RenderControl(RasAA, FScanLineP8, RenAA, FSliderGamma);
      RenderControl(RasAA, FScanLineP8, RenAA, FSliderAlpha);
      RenderControl(RasAA, FScanLineP8, RenAA, FCheckBoxTest);

      // Free AGG resources
      RasAA.Free;
    finally
      RenAA.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Dx, Dy: Double;
begin
  if mkfMouseLeft in Flags then
  begin
    if FIndex = 3 then
    begin
      Dx := X - FDelta.X;
      Dy := Y - FDelta.Y;

      FX[1] := FX[1] - (FX[0] - Dx);
      FY[1] := FY[1] - (FY[0] - Dy);
      FX[2] := FX[2] - (FX[0] - Dx);
      FY[2] := FY[2] - (FY[0] - Dy);
      FX[0] := Dx;
      FY[0] := Dy;

      ForceRedraw;
      Exit;
    end;

    if FIndex >= 0 then
    begin
      FX[FIndex] := X - FDelta.X;
      FY[FIndex] := Y - FDelta.Y;

      ForceRedraw;
    end;
  end;
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
      if (Sqrt((X - FX[I]) * (X - FX[I]) + (Y - FY[I]) * (Y - FY[I])) < 20)
        or (Sqrt((X - FX[I] + 200) * (X - FX[I] + 200) + (Y - FY[I]) *
        (Y - FY[I])) < 20) then
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
        or PointInTriangle(FX[0] - 200, FY[0], FX[1] - 200, FY[1],
        FX[2] - 200, FY[2], X, Y) then
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
var
  Delta: TPointDouble;
begin
  Delta.X := 0;
  Delta.Y := 0;

  case TKeyCode(Key) of
    kcLeft:
      Delta.X := -0.1;
    kcRight:
      Delta.X := 0.1;
    kcUp:
      Delta.Y := 0.1;
    kcDown:
      Delta.Y := -0.1;
  end;

  FX[0] := FX[0] + Delta.X;
  FY[0] := FY[0] + Delta.Y;
  FX[1] := FX[1] + Delta.X;
  FY[1] := FY[1] + Delta.Y;

  ForceRedraw;

  if Key = Cardinal(kcF1) then
    DisplayMessage('It''s a very simple example that was written to compare '
      + 'the performance between Anti-Aliased and regular polygon filling. '
      + 'It appears that the most expensive operation is rendering of '
      + 'horizontal ScanLines. So that, we can use the very same rasterization '
      + 'algorithm to draw regular, aliased polygons. Of course, it''s '
      + 'possible to write a special version of the Rasterizer that will work '
      + 'faster, but won''t calculate the pixel coverage values. But on the '
      + 'other hand, the existing version of the TAggRasterizerScanLineAA allows '
      + 'you to change Gamma, and to "dilate" or "shrink" the polygons in '
      + 'range of ± 1 pixel.'#13#13
      + 'How to play with:'#13#13
      + 'As usual, you can drag the triangles as well as the vertices of '
      + 'them. Compare the performance with different shapes and opacity.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

procedure TAggApplication.OnControlChange;
var
  I: Integer;
  T1, T2: Double;
begin
  if FCheckBoxTest.Status then
  begin
    OnDraw;
    UpdateWindow;
    FCheckBoxTest.Status := False;

    StartTimer;

    for I := 0 to 999 do
      DrawAliased;

    T1 := GetElapsedTime;

    StartTimer;

    for I := 0 to 999 do
      DrawAntiAliased;

    T2 := GetElapsedTime;

    UpdateWindow;

    DisplayMessage(Format('Time Aliased=%.2fms '#13 +
      'Time Anti-Aliased=%.2fms', [T1, T2]));
  end;
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Line Join (F1-Help)';

    if Init(500, 330, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
