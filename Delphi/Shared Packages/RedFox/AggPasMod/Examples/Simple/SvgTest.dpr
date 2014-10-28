program SvgTest;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

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
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',

  AggSvgParser in '..\..\Source\Svg\AggSvgParser.pas',
  AggSvgPathRenderer in '..\..\Source\Svg\AggSvgPathRenderer.pas',
  AggSvgException in '..\..\Source\Svg\AggSvgException.pas';

const
  CFlipY = False;

type
  TAggApplication = class(TPlatformSupport)
  private
    FPath: TPathRenderer;
    FSliderExpand, FSliderGamma: TAggControlSlider;
    FSliderScale, FSliderRotate: TAggControlSlider;
    FX, FY: Double;
    FMin, FMax, FDelta: TPointDouble;
    FDragFlag: Boolean;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure ParseSvg(FileName: ShortString);

    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FPath := TPathRenderer.Create;

  FSliderExpand := TAggControlSlider.Create(5, 5, 256 - 5, 11, not FlipY);
  FSliderGamma := TAggControlSlider.Create(5, 5 + 15, 256 - 5, 11 + 15,
    not FlipY);
  FSliderScale := TAggControlSlider.Create(256 + 5, 5, 512 - 5, 11, not FlipY);
  FSliderRotate := TAggControlSlider.Create(256 + 5, 5 + 15, 512 - 5, 11 + 15,
    not FlipY);

  FMin.X := 0.0;
  FMin.Y := 0.0;
  FMax.X := 0.0;
  FMax.Y := 0.0;

  FX := 0.0;
  FY := 0.0;
  FDelta.X := 0.0;
  FDelta.Y := 0.0;

  FDragFlag := False;

  AddControl(FSliderExpand);
  AddControl(FSliderGamma);
  AddControl(FSliderScale);
  AddControl(FSliderRotate);

  FSliderExpand.Caption := 'Expand=%3.2f';
  FSliderExpand.SetRange(-1, 1.2);
  FSliderExpand.Value := 0.0;

  FSliderGamma.Caption := 'Gamma=%3.2f';
  FSliderGamma.SetRange(0.0, 3.0);
  FSliderGamma.Value := 1.0;

  FSliderScale.Caption := 'Scale=%3.2f';
  FSliderScale.SetRange(0.2, 10.0);
  FSliderScale.Value := 1.0;

  FSliderRotate.Caption := 'Rotate=%3.2f';
  FSliderRotate.SetRange(-180.0, 180.0);
  FSliderRotate.Value := 0.0;
end;

destructor TAggApplication.Destroy;
begin
  FPath.Free;

  FSliderExpand.Free;
  FSliderGamma.Free;
  FSliderScale.Free;
  FSliderRotate.Free;

  inherited
end;

procedure TAggApplication.ParseSvg;
var
  P: TParser;
begin
  P := TParser.Create(FPath);
  try
    P.Parse(FileName);

    FPath.ArrangeOrientations;
    FPath.BoundingRect(@FMin.X, @FMin.Y, @FMax.X, @FMax.Y);

    Caption := P.Title + ' (F1-Help)';
  finally
    P.Free;
  end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase : TAggRendererBase;
  Ren: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl : TAggScanLinePacked8;
  Mtx: TAggTransAffine;

  Gmpw: TAggGammaPower;
  Gmno: TAggGammaNone;

  Tm: Double;

  VertexCount: Cardinal;

  Txt: TAggGsvText;
  Pt: TAggConvStroke;
  V: Double;
begin
  // Initialize structures
  PixelFormatBgra32(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Ren := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    Ras := TAggRasterizerScanLineAA.Create;
    Sl := TAggScanLinePacked8.Create;
    Mtx := TAggTransAffine.Create;

    // Render
    Gmpw.Create(FSliderGamma.Value);
    Ras.Gamma(@Gmpw);

    Mtx.Translate((FMin.X + FMax.X) * -0.5, (FMin.Y + FMax.Y) * -0.5);
    Mtx.Scale(FSliderScale.Value);
    Mtx.Rotate(Deg2Rad(FSliderRotate.Value));
    Mtx.Translate((FMin.X + FMax.X) * 0.5 + FX,
      (FMin.Y + FMax.Y) * 0.5 + FY + 30);

    FPath.Expand(FSliderExpand.Value);

    StartTimer;

    FPath.Render(Ras, Sl, Ren, @Mtx, RendererBase.GetClipBox^, 1.0);

    Tm := GetElapsedTime;

    VertexCount := FPath.GetVertexCount;

    // Render the controls
    Gmno.Create;
    Ras.Gamma(@Gmno);

    RenderControl(Ras, Sl, Ren, FSliderExpand);
    RenderControl(Ras, Sl, Ren, FSliderGamma);
    RenderControl(Ras, Sl, Ren, FSliderScale);
    RenderControl(Ras, Sl, Ren, FSliderRotate);

    // Display text
    Txt := TAggGsvText.Create;
    Txt.SetSize(10.0);
    Txt.Flip := True;

    Pt := TAggConvStroke.Create(Txt);
    Pt.Width := 1.5;

    Txt.SetStartPoint(10.0, 40.0);
    Txt.SetText(Format('Vertices=%d Time=%.3f ms', [VertexCount, Tm]));

    Ras.AddPath(Pt);
    Ren.SetColor(CRgba8Black);
    RenderScanLines(Ras, Sl, Ren);

    // Free AGG resources
    Mtx.Free;
    Ren.Free;
    Ras.Free;
    Sl.Free;

    Txt.Free;
    Pt.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if Flags = [] then
    FDragFlag := False;

  if FDragFlag then
  begin
    FX := X - FDelta.X;
    FY := Y - FDelta.Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FDelta.X := X - FX;
  FDelta.Y := Y - FY;

  FDragFlag := True;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FDragFlag := False;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
var
  Mtx: TAggTransAffine;

  M  : TAggParallelogram;
  Fd : Text;
  V  : Double;
begin
  if Key = Byte(' ') then
  begin
    Mtx := TAggTransAffine.Create;

    Mtx.Translate((FMin.X + FMax.X) * -0.5, (FMin.Y + FMax.Y) * -0.5);
    Mtx.Scale(FSliderScale.Value);
    Mtx.Rotate(Deg2Rad(FSliderRotate.Value));
    Mtx.Translate((FMin.X + FMax.X) * 0.5, (FMin.Y + FMax.Y) * 0.5);
    Mtx.Translate(FX, FY);

    Mtx.StoreTo(@M);

    DisplayMessage(Format('%3.3f, %3.3f, %3.3f, %3.3f, %3.3f, %3.3f', [M[0],
      M[1], M[2], M[3], M[4], M[5]]));

{$I- }
    AssignFile(Fd, 'transform.txt');
    Rewrite(Fd);
//    Writeln(Fd, PAnsiChar(@Buf[0]));
    Close(Fd);
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('The SVG viewer is just another example of using Anti-Grain '
      + 'Geometry. The viewer supports absolute minimum of the SVG '
      + 'specification, it basically can be used as a simple example of AGG '
      + 'plus SVG. But of course, its functionality can be extended. The main '
      + 'point of the viewer is High quality and High performance. The '
      + 'Anti-Aliasing algorithm produces 256 levels of transparency. '
      + 'Actually, AGG computes the exact coverage of the outline on each '
      + 'pixel cell.'#13
      + 'Besides, the viewer has a very nice feature that I haven''t seen in '
      + 'any other ones. It''s eliminating of the "problem of adjacent edges". '
      + 'It appears when rendering adjacent polygons with anti-aliasing and '
      + 'looks like thin "web" upon the image. Strictly speaking, it''s '
      + 'possible to get rid of it completely only when the polygons are fully '
      + 'opaque. When they are translucent, the effect will appear anyway. '
      + 'However, it''s possible to reduce the effect so that it becomes '
      + 'almost invisible.'#13#13
      + 'How to play with:'#13#13
      + 'Use mouse to move the drawing around. Change size & rotation with '
      + 'top controls. Press the spacebar key to display and save the current '
      + 'transformation matrix.');
end;

var
  FileName: ShortString;
  Af   : TApiFile;

begin
  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    FileName := FileSource('svg', 'tiger.svg');

    if ParamCount > 0 then
      FileName := Param_str(1)
    else
    begin
      if ApiOpenFile(Af, FileName) then
        ApiCloseFile(Af)
      else
      begin
        DisplayMessage('Usage: svg_test <svg_file>');
        Exit;
      end;
    end;

    try
      ParseSvg(FileName);

      if Init(512, 600, [wfResize]) then
        Run;
    except
      on E: TSvgException do
      begin
        DisplayMessage(PAnsiChar(E.GetMessage));
        E.Free;
      end;

      else
        DisplayMessage('Unknown exception');
    end;
  finally
    Free;
  end;
end.
