program GammaControlDemo;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$I- }

{ DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{ DEFINE AGG_Rgb24 }
{ DEFINE AGG_BGRA32 }
{ DEFINE AGG_RgbA32 }
{ DEFINE AGG_ARGB32 }
{ DEFINE AGG_ABGR32 }
{ DEFINE AGG_Rgb565 }
{ DEFINE AGG_Rgb555 }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggGammaControl in '..\..\Source\Controls\AggGammaControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas'

{$I Pixel_Formats.inc}

const
  CFlipY = True;

var
  GControl: TAggGammaControl;

type
  TAggApplication = class(TPlatformSupport)
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;

procedure ReadGamma(Fname: ShortString);
var
  Fd: Text;
  Kx1, Ky1, Kx2, Ky2: Double;
begin
  Assignfile(Fd, Fname);
  Reset(Fd);

  Readln(Fd, Kx1);
  Readln(Fd, Ky1);
  Readln(Fd, Kx2);
  Readln(Fd, Ky2);

  GControl.Values(Kx1, Ky1, Kx2, Ky2);

  Close(Fd);
end;

procedure WriteGammaBin(Fname: ShortString);
var
  Fd: file;
  Gamma: PInt8U;
begin
  Gamma := GControl.Gamma;

  Assignfile(Fd, Fname);
  Rewrite(Fd, 1);
  Blockwrite(Fd, Gamma^, 256);
  Close(Fd);
end;

procedure WriteGammaTxt(Fname: ShortString);
var
  Fd: Text;

  Gamma: PInt8U;
  I, J : Integer;

  Kx1, Ky1, Kx2, Ky2: Double;
begin
  Gamma := GControl.Gamma;

  Assignfile(Fd, Fname);
  Rewrite(Fd);

  GControl.Values(@Kx1, @Ky1, @Kx2, @Ky2);

  Writeln(Fd, Kx1:1:3);
  Writeln(Fd, Ky1:1:3);
  Writeln(Fd, Kx2:1:3);
  Writeln(Fd, Ky2:1:3);

  for I := 0 to 15 do
  begin
    for J := 0 to 15 do
      Write(Fd, PInt8u(PtrComp(Gamma) + I * 16 + J)^:3, ',');

    Writeln(Fd);
  end;

  Close(Fd);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  AddControl(GControl);
end;

destructor TAggApplication.Destroy;
begin
  WriteGammaTxt('Gamma.txt');
  WriteGammaBin('Gamma.bin');

  inherited;
end;

procedure TAggApplication.OnInit;
begin
  ReadGamma('Gamma.txt');
end;

procedure TAggApplication.OnDraw;
var
  EWidth, ECenter: Double;

  I: Integer;

  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Ras: TAggRasterizerScanLineAA;
  Sl : TAggScanLinePacked8;

  Rgba : TAggColor;
  Ellipse : TAggEllipse;
  Poly : TAggConvStroke;
  ConvTransPoly: TAggConvTransform;

  Mtx: TAggTransAffine;

  Text : TAggGsvText;
  Text1: TAggGsvTextOutline;
  Path : TAggPathStorage;
  Trans: TAggConvTransform;
begin
  // Initialize structures
  EWidth := InitialWidth * 0.5 - 10;
  ECenter := InitialWidth * 0.5;

  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      GControl.SetTextSize(10, 12);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLinePacked8.Create;

      // Render the controls
      RenderControl(Ras, Sl, RenScan, GControl);

      Ras.Gamma(GControl);

      // Ellipse
      Ellipse := TAggEllipse.Create;
      Poly := TAggConvStroke.Create(Ellipse);
      ConvTransPoly := TAggConvTransform.Create(Poly, GetTransAffineResizing);

      RenScan.SetColor(CRgba8Black);

      Ellipse.Initialize(ECenter, 220, EWidth, 15, 100);
      Poly.Width := 2;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(ECenter, 220), PointDouble(11), 100);
      Poly.Width := 2;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Rgba.FromRgbaInteger(127, 127, 127);
      RenScan.SetColor(@Rgba);

      Ellipse.Initialize(ECenter, 260, EWidth, 15, 100);
      Poly.Width := 2;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(ECenter, 260), PointDouble(11), 100);
      Poly.Width := 2;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Rgba.FromRgbaInteger(192, 192, 192);
      RenScan.SetColor(@Rgba);

      Ellipse.Initialize(ECenter, 300, EWidth, 15, 100);
      Poly.Width := 2;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(ECenter, 300), PointDouble(11), 100);
      Poly.Width := 2;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Rgba.FromRgbaDouble(0, 0, 0.4);
      RenScan.SetColor(@Rgba);

      Ellipse.Initialize(ECenter, 340, EWidth, 15.5, 100);
      Poly.Width := 1;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(ECenter, 340), PointDouble(10.5), 100);
      Poly.Width := 1;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(ECenter, 380, EWidth, 15.5, 100);
      Poly.Width := 0.4;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(ECenter, 380), PointDouble(10.5), 100);
      Poly.Width := 0.4;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(ECenter, 420, EWidth, 15.5, 100);
      Poly.Width := 0.1;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      Ellipse.Initialize(PointDouble(ECenter, 420), PointDouble(10.5), 100);
      Poly.Width := 0.1;
      Ras.AddPath(ConvTransPoly, 0);

      RenderScanLines(Ras, Sl, RenScan);

      // Text
      Mtx := TAggTransAffine.Create;
      try
        Mtx.Skew(0.15, 0);
        Mtx.Multiply(GetTransAffineResizing);

        Text := TAggGsvText.Create;
        try
          Text1 := TAggGsvTextOutline.Create(Text, Mtx);
          try
            Text.SetText('Text 2345');
            Text.SetSize(50, 20);
            Text1.Width := 2;
            Text.SetStartPoint(320, 10);

            Rgba.FromRgbaDouble(0, 0.5, 0);
            RenScan.SetColor(@Rgba);

            Ras.AddPath(Text1, 0);
            RenderScanLines(Ras, Sl, RenScan);
          finally
            Text1.Free;
          end;
        finally
          Text.Free;
        end;

        // Triangled circle
        Rgba.FromRgbaDouble(0.5, 0, 0);
        RenScan.SetColor(@Rgba);

        Path := TAggPathStorage.Create;
        Path.MoveTo(30, -1);
        Path.LineTo(60, 0);
        Path.LineTo(30, 1);

        Path.MoveTo(27, -1);
        Path.LineTo(10, 0);
        Path.LineTo(27, 1);

        Trans := TAggConvTransform.Create(Path, Mtx);
        try
          for I := 0 to 34 do
          begin
            Mtx.Reset;

            Mtx.Rotate(2 * Pi * I / 35);
            Mtx.Translate(400, 130);

            Mtx.Multiply(GetTransAffineResizing);

            Ras.AddPath(Trans, 0);
            RenderScanLines(Ras, Sl, RenScan);
          end;
        finally
          Trans.Free;
        end;
      finally
        Mtx.Free;
      end;

      // Free AGG resources
      ConvTransPoly.Free;
      Ras.Free;
      Sl.Free;
      Ellipse.Free;
      Poly.Free;
      Path.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This is another experiment with Gamma correction. See also '
      + 'Gamma Correction. I presumed that we can do better than with a '
      + 'traditional power function. So, I created a special control to have '
      + 'an arbitrary Gamma function. The conclusion is that we can really '
      + 'achieve a better visual result with this control, but still, in '
      + 'practice, the traditional power function is good enough too.'#13#13
      + 'How to play with:'#13#13
      + 'Feel free to change the Gamma curve. The shape you''ll set up, '
      + 'will be stored in external file "Gamma.txt".'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  GControl := TAggGammaControl.Create(10, 10, 300, 200, not CFlipY);
  try
    with TAggApplication.Create(CPixFormat, CFlipY) do
    try
      Caption := 'Anti-Aliasing Gamma Correction (F1-Help)';

      if Init(500, 400, [wfResize]) then
        Run;
    finally
      Free;
    end;
  finally
    GControl.Free;
  end;
end.
