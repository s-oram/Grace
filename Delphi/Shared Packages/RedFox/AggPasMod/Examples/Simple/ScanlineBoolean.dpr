program ScanLineBoolean;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{ DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{ DEFINE AGG_RGB24 }
{ DEFINE AGG_BGRA32 }
{ DEFINE AGG_RGBA32 }
{ DEFINE AGG_ARGB32 }
{ DEFINE AGG_ABGR32 }
{ DEFINE AGG_RGB565 }
{ DEFINE AGG_RGB555 }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLineBin in '..\..\Source\AggScanLineBin.pas',
  AggScanLineBooleanAlgebra in '..\..\Source\AggScanLineBooleanAlgebra.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggInteractivePolygon

{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FQuad: array [0..1] of TInteractivePolygon;

    FRadioBoxTransType: TAggControlRadioBox;

    FCheckBoxReset: TAggControlCheckBox;
    FSliderMul: array [0..1] of TAggControlSlider;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnControlChange; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;

procedure GenerateCircles(Ps: TAggPathStorage; Quad: PPointDouble;
  NumCircles: Cardinal; Radius: Double);
var
  Circle: TAggCircle;
  I, J: Cardinal;
  P: array [0..1] of PPointDouble;
  Scale: Double;
begin
  Ps.RemoveAll;
  Scale := 1 / NumCircles;
  Circle := TAggCircle.Create;
  try
    for I := 0 to 3 do
    begin
      P[0] := Quad;
      P[1] := Quad;
      Inc(P[0], I);

      if I < 3 then
        Inc(P[1], I + 1);

      for J := 0 to NumCircles - 1 do
      begin
        Circle.Initialize(P[0]^.X + (P[1]^.X - P[0]^.X) * J * Scale,
          P[0]^.Y + (P[1]^.Y - P[0]^.Y) * J * Scale, Radius, 100);

        Ps.AddPath(Circle, 0, False);
      end;
    end;
  finally
    Circle.Free;
  end;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FQuad[0] := TInteractivePolygon.Create(4, 5.0);
  FQuad[1] := TInteractivePolygon.Create(4, 5.0);

  FRadioBoxTransType := TAggControlRadioBox.Create(420, 5.0, 420 + 130.0, 145.0,
    not FlipY);

  FCheckBoxReset := TAggControlCheckBox.Create(350, 5.0, 'Reset', not FlipY);

  FSliderMul[0] := TAggControlSlider.Create(5.0, 5.0, 340.0, 12.0, not FlipY);
  FSliderMul[1] := TAggControlSlider.Create(5.0, 20.0, 340.0, 27.0, not FlipY);

  FRadioBoxTransType.AddItem('Union');
  FRadioBoxTransType.AddItem('Intersection');
  FRadioBoxTransType.AddItem('Linear XOR');
  FRadioBoxTransType.AddItem('Saddle XOR');
  FRadioBoxTransType.AddItem('Abs Diff XOR');
  FRadioBoxTransType.AddItem('A-B');
  FRadioBoxTransType.AddItem('B-A');
  FRadioBoxTransType.SetCurrentItem(0);

  AddControl(FRadioBoxTransType);
  AddControl(FCheckBoxReset);
  AddControl(FSliderMul[0]);
  AddControl(FSliderMul[1]);

  FSliderMul[0].Value := 1.0;
  FSliderMul[1].Value := 1.0;
  FSliderMul[0].Caption := 'Opacity1=%.3f';
  FSliderMul[1].Caption := 'Opacity2=%.3f';
end;

destructor TAggApplication.Destroy;
begin
  FQuad[0].Free;
  FQuad[1].Free;

  FRadioBoxTransType.Free;
  FCheckBoxReset.Free;
  FSliderMul[0].Free;
  FSliderMul[1].Free;

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  FQuad[0].Xn[0] := 50;
  FQuad[0].Yn[0] := 180;
  FQuad[0].Xn[1] := Width * 0.5 - 25;
  FQuad[0].Yn[1] := 200;
  FQuad[0].Xn[2] := Width * 0.5 - 25;
  FQuad[0].Yn[2] := Height - 70;
  FQuad[0].Xn[3] := 50;
  FQuad[0].Yn[3] := Height - 50;

  FQuad[1].Xn[0] := Width * 0.5 + 25;
  FQuad[1].Yn[0] := 180;
  FQuad[1].Xn[1] := Width - 50;
  FQuad[1].Yn[1] := 200;
  FQuad[1].Xn[2] := Width - 50;
  FQuad[1].Yn[2] := Height - 70;
  FQuad[1].Xn[3] := Width * 0.5 + 25;
  FQuad[1].Yn[3] := Height - 50;
end;

procedure TAggApplication.OnDraw;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RendererScanLine : TAggRendererScanLineAASolid;
  ScanLine: TAggScanLinePacked8;

  Ras: array [0..2] of TAggRasterizerScanLineAA;
  Rgba: TAggColor;

  Op: TAggBoolScanLineOp;
  GammaMultiply: TAggGammaMultiply;

  PathStorage: array [0..1] of TAggPathStorage;

  ScanLines: array [0..1] of TAggScanLinePacked8;
  ScanLineResult: TAggScanLinePacked8;

  ScanRen: TAggRendererScanLineAASolid;

begin
  // Initialize structures
  CPixelFormat(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RendererScanLine := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    ScanLine := TAggScanLinePacked8.Create;
    Ras[0] := TAggRasterizerScanLineAA.Create;
    Ras[1] := TAggRasterizerScanLineAA.Create;
    Ras[2] := TAggRasterizerScanLineAA.Create;

    // Draw
    Op := TAggBoolScanLineOp(FRadioBoxTransType.GetCurrentItem);

    GammaMultiply := TAggGammaMultiply.Create(FSliderMul[0].Value);
    try
      Ras[1].Gamma(GammaMultiply);
    finally
      GammaMultiply.Free;
    end;

    GammaMultiply := TAggGammaMultiply.Create(FSliderMul[1].Value);
    try
      Ras[2].Gamma(GammaMultiply);
    finally
      GammaMultiply.Free;
    end;

    Ras[0].SetClipBox(0, 0, Width, Height);

    PathStorage[0] := TAggPathStorage.Create;
    GenerateCircles(PathStorage[0], FQuad[0].Polygon, 5, 20);

    PathStorage[1] := TAggPathStorage.Create;
    GenerateCircles(PathStorage[1], FQuad[1].Polygon, 5, 20);

    Ras[1].FillingRule := frEvenOdd;

    // Bottom Layer of Polygon1
    Rgba.FromRgbaInteger(240, 255, 200, 100);
    RendererScanLine.SetColor(@Rgba);

    Ras[1].AddPath(PathStorage[0]);
    RenderScanLines(Ras[1], ScanLine, RendererScanLine);

    // Bottom Layer of Polygon2
    Rgba.FromRgbaInteger(255, 240, 240, 100);
    RendererScanLine.SetColor(@Rgba);

    Ras[2].AddPath(PathStorage[1]);
    RenderScanLines(Ras[2], ScanLine, RendererScanLine);

    // Combine shapes
    ScanLineResult := TAggScanLinePacked8.Create;
    ScanLines[0] := TAggScanLinePacked8.Create;
    ScanLines[1] := TAggScanLinePacked8.Create;
    ScanRen := TAggRendererScanLineAASolid.Create(RendererBase);

    ScanRen.SetColor(CRgba8Black);

    BoolScanLineCombineShapesAA(Op, Ras[1], Ras[2], ScanLines[0], ScanLines[1],
      ScanLineResult, ScanRen);

    // Render the "quad" tools
    Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.6);
    RendererScanLine.SetColor(@Rgba);

    Ras[0].AddPath(FQuad[0]);
    RenderScanLines(Ras[0], ScanLine, RendererScanLine);

    Ras[0].AddPath(FQuad[1]);
    RenderScanLines(Ras[0], ScanLine, RendererScanLine);

    // Render the controls
    RenderControl(Ras[0], ScanLine, RendererScanLine, FRadioBoxTransType);
    RenderControl(Ras[0], ScanLine, RendererScanLine, FCheckBoxReset);
    RenderControl(Ras[0], ScanLine, RendererScanLine, FSliderMul[0]);
    RenderControl(Ras[0], ScanLine, RendererScanLine, FSliderMul[1]);

    // Free AGG resources
    PathStorage[0].Free;
    PathStorage[1].Free;

    RendererScanLine.Free;
    ScanRen.Free;
    Ras[2].Free;
    Ras[1].Free;
    Ras[0].Free;

    ScanLineResult.Free;
    ScanLines[0].Free;
    ScanLines[1].Free;

    ScanLine.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FQuad[0].OnMouseMove(X, Y) or FQuad[1].OnMouseMove(X, Y) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FQuad[0].OnMouseButtonDown(X, Y) or FQuad[1].OnMouseButtonDown(X, Y)
    then
      ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FQuad[0].OnMouseButtonUp(X, Y) or FQuad[1].OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnControlChange;
begin
  if FCheckBoxReset.Status then
  begin
    OnInit;
    FCheckBoxReset.Status := False;
    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('A new method to perform boolean operations on polygons '
      + '(Union, Intersection, XOR, and Difference). It uses the ScanLine '
      + 'approach and in typical screen resolutions works much faster (about '
      + '10 times) than vectorial algorithms like General Polygon Clipper. It '
      + 'preserves perfect Anti-Aliasing and besides, can work with '
      + 'translucency. There are two XOR operations, Linear XOR and Saddle '
      + 'XOR. The only difference is in the formula of XORing of the two '
      + 'cells with Anti-Aliasing. The first one is:'#13#13
      + 'cover = a+b; if(cover > 1) cover = 2.0 - cover;'#13#13
      + 'The second uses the classical "Saddle" formula:'#13#13
      + 'cover = 1.0 - (1.0 - a + a*b) * (1.0 - b + a*b);'#13#13
      + 'The Linear XOR produces more correct intersections and works '
      + 'constistently with the ScanLine Rasterizer algorithm. The Saddle XOR '
      + 'works better with semi-transparent polygons. '#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.  ');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. ScanLine Boolean (F1-Help)';

    if Init(800, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
