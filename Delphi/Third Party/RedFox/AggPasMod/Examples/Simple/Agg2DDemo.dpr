program Agg2DDemo;

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
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  Agg2D in '..\..\Source\Agg2D.pas';

{-$DEFINE ViewportOptionAnisotropic}
{-$DEFINE FontCacheRaster}
{$DEFINE More}
{$DEFINE UseTextHints}
{-$DEFINE BlackBackground}
{-$DEFINE UseClipBox}

const
  CFlipY = True;
  CAngleStep = 5;
  CGammaStep = 0.1;

  {$IFDEF ViewportOptionAnisotropic}
  CViewportOption: TAggViewportOption = voAnisotropic;
  {$ELSE}
  CViewportOption: TAggViewportOption = voXMidYMid;
  {$ENDIF}

  {$IFDEF FontCacheRaster}
  CFontCache: TAggFontCache = fcRaster;
  {$ELSE}
  CFontCache: TAggFontCache = fcVector;
  {$ENDIF}

var
  GFontTimes: AnsiString = 'Times New Roman';
  GFontArial: AnsiString = 'Arial';
  GFontVerdana: AnsiString = 'Verdana';

type
  TAggApplication = class(TPlatformSupport)
  private
    FGraphics, FTimer: TAgg2D;
    FAngle, FGamma: Double;
    FImage: Integer;
    FGmText: AnsiString;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FGraphics := TAgg2D.Create;
  FTimer := TAgg2D.Create;

  FAngle := 0;
  FGamma := 1.4;
  FImage := 6;

  Str(FGamma: 0: 2, FGmText);
  FGmText := 'Gamma: ' + FGmText;
end;

destructor TAggApplication.Destroy;
begin
  FGraphics.Free;
  FTimer.Free;

  Finalize(FGmText);

  inherited;
end;

procedure TAggApplication.OnInit;
begin
end;

procedure TAggApplication.OnDraw;
var
  Bounds: TRectDouble;

  Clr, C1, C2, C3: TAggColorRgba8;

  Img: TAgg2DImage;

  Parl: TAggParallelogram;
  Poly: array [0..11] of Double;

  Tm : Double;
  Fps: Integer;

  Timer, Rate: AnsiString;
begin
  StartTimer;

  FGraphics.Attach(RenderingBufferWindow.Buffer,
    RenderingBufferWindow.Width, RenderingBufferWindow.Height,
    RenderingBufferWindow.Stride);

  {$IFDEF BlackBackground}
  FGraphics.ClearAll(0, 0, 0);
  FGraphics.BlendMode := bmPlus;
  {$ELSE}
  FGraphics.ClearAll(255, 255, 255);
  {$ENDIF}

  FGraphics.AntiAliasGamma := FGamma;

  // Set FlipText := true if you have the Y axis upside down.
  FGraphics.FlipText := not CFlipY;

  {$IFDEF UseClipBox}
  FGraphics.ClipBox(50, 50, RenderingBufferWindow.Width - 50,
    RenderingBufferWindow.Height - 50);
  {$ENDIF}

  // Transformations - Rotate around (300, 300) to 5 degree
  FGraphics.Translate(-300, -300);
  FGraphics.Rotate(Deg2Rad(FAngle));
  FGraphics.Translate(300, 300);
  // FGraphics.Skew(0.1, 0.1);

  // Viewport - set 0,0,600,600 to the actual window size
  // preserving aspect ratio and placing the viewport in the center.
  // To ignore aspect ratio use Agg2D::Anisotropic
  // Note that the viewport just adds transformations to the current
  // affine matrix. So that, set the viewport *after* all transformations!
  FGraphics.Viewport(0, 0, 600, 600, 0, 0, Width, Height, CViewportOption);

  // Rounded Rect
  FGraphics.SetLineColor(0, 0, 0);
  FGraphics.NoFill;
  FGraphics.RoundedRect(0.5, 0.5, 600 - 0.5, 600 - 0.5, 20);

  // Regular Text
  {$IFDEF More}
  FGraphics.Font(PAnsiChar(GFontTimes), 14, False, False);
  FGraphics.SetFillColor(0, 0, 0);
  FGraphics.NoLine;
  FGraphics.Text(100, 20, PAnsiChar('Regular Raster Text -- Fast, but can''t '
    + 'be rotated'));{ }
  {$ENDIF}

  // Outlined Text
  FGraphics.Font(PAnsiChar(GFontTimes), 50, False, False, CFontCache);
  FGraphics.SetLineColor(50, 0, 0);
  FGraphics.SetFillColor(180, 200, 100);
  FGraphics.LineWidth := 1;
  FGraphics.Text(100.5, 50.5, 'Outlined Text');

  // Gamma Text
  {$IFDEF More}
  FGraphics.Font(PAnsiChar(GFontArial), 38, True, True, fcVector);
  C1.Initialize(255, 0, 0, 255);
  C2.Initialize(0, 255, 0, 255);
  FGraphics.FillLinearGradient(50, 1, 300, 10, C1, C2);

  FGraphics.NoLine;
  FGraphics.Text(12.5, 565.5, FGmText);
  //FGraphics.Rectangle(12.5, 565.5, 290, 590);
  {$ENDIF}

  // Text Alignment
  FGraphics.SetLineColor(0, 0, 0);
  FGraphics.Line(100.5, 150.5, 400.5, 150.5);
  FGraphics.Line(250.5, 130.5, 250.5, 170.5);
  FGraphics.Line(100.5, 200.5, 400.5, 200.5);
  FGraphics.Line(250.5, 180.5, 250.5, 220.5);
  FGraphics.Line(100.5, 250.5, 400.5, 250.5);
  FGraphics.Line(250.5, 230.5, 250.5, 270.5);
  FGraphics.Line(100.5, 300.5, 400.5, 300.5);
  FGraphics.Line(250.5, 280.5, 250.5, 320.5);
  FGraphics.Line(100.5, 350.5, 400.5, 350.5);
  FGraphics.Line(250.5, 330.5, 250.5, 370.5);
  FGraphics.Line(100.5, 400.5, 400.5, 400.5);
  FGraphics.Line(250.5, 380.5, 250.5, 420.5);
  FGraphics.Line(100.5, 450.5, 400.5, 450.5);
  FGraphics.Line(250.5, 430.5, 250.5, 470.5);
  FGraphics.Line(100.5, 500.5, 400.5, 500.5);
  FGraphics.Line(250.5, 480.5, 250.5, 520.5);
  FGraphics.Line(100.5, 550.5, 400.5, 550.5);
  FGraphics.Line(250.5, 530.5, 250.5, 570.5);

  FGraphics.SetFillColor(100, 50, 50);
  FGraphics.NoLine;
  {$IFDEF UseTextHints}
  FGraphics.TextHints := True;
  {$ELSE}
  FGraphics.TextHints := False;
  {$ENDIF}
  FGraphics.Font(PAnsiChar(GFontTimes), 40, False, False, fcVector);

  FGraphics.TextAlignment(tahLeft, tavBottom);
  FGraphics.Text(250, 150, 'Left-Bottom', True, 0, 0);

  FGraphics.TextAlignment(tahCenter, tavBottom);
  FGraphics.Text(250, 200, 'Center-Bottom', True, 0, 0);

  FGraphics.TextAlignment(tahRight, tavBottom);
  FGraphics.Text(250, 250, 'Right-Bottom', True, 0, 0);

  FGraphics.TextAlignment(tahLeft, tavCenter);
  FGraphics.Text(250, 300, 'Left-Center', True, 0, 0);

  FGraphics.TextAlignment(tahCenter, tavCenter);
  FGraphics.Text(250, 350, 'Center-Center', True, 0, 0);

  FGraphics.TextAlignment(tahRight, tavCenter);
  FGraphics.Text(250, 400, 'Right-Center', True, 0, 0);

  FGraphics.TextAlignment(tahLeft, tavTop);
  FGraphics.Text(250, 450, 'Left-Top', True, 0, 0);

  FGraphics.TextAlignment(tahCenter, tavTop);
  FGraphics.Text(250, 500, 'Center-Top', True, 0, 0);

  FGraphics.TextAlignment(tahRight, tavTop);
  FGraphics.Text(250, 550, 'Right-Top', True, 0, 0);

  // Gradients (Aqua Buttons)
  // =======================================
  FGraphics.Font(PAnsiChar(GFontVerdana), 20, False, False, fcVector);

  Bounds.X1 := 400;
  Bounds.Y1 := 80;
  Bounds.X2 := Bounds.X1 + 150;
  Bounds.Y2 := Bounds.Y1 + 36;

  Clr.Initialize(0, 50, 180, 180);
  FGraphics.FillColor := Clr;
  Clr.Initialize(0, 0, 80, 255);
  FGraphics.LineColor := Clr;
  FGraphics.LineWidth := 1;
  FGraphics.RoundedRect(Bounds, 12, 18);

  Clr.Initialize(0, 0, 0, 0);
  FGraphics.LineColor := Clr;

  C1.Initialize(100, 200, 255, 255);
  C2.Initialize(255, 255, 255, 0);
  FGraphics.FillLinearGradient(Bounds.X1, Bounds.Y1, Bounds.X1, Bounds.Y1 + 30, C1, C2);
  FGraphics.RoundedRect(Bounds.X1 + 3, Bounds.Y1 + 2.5, Bounds.X2 - 3, Bounds.Y1 + 30, 9, 18, 1, 1);

  Clr.Initialize(0, 0, 50, 200);
  FGraphics.FillColor := Clr;
  FGraphics.NoLine;
  FGraphics.TextAlignment(tahCenter, tavCenter);
  FGraphics.Text(Bounds.CenterX, Bounds.CenterY, 'Aqua Button', True, 0, 0);

  C1.Initialize(0, 0, 255, 0);
  C2.Initialize(100, 255, 255, 255);
  FGraphics.FillLinearGradient(Bounds.X1, Bounds.Y2 - 20, Bounds.X1, Bounds.Y2 - 3, C1, C2);
  FGraphics.RoundedRect(Bounds.X1 + 3, Bounds.Y2 - 20, Bounds.X2 - 3, Bounds.Y2 - 2, 1, 1, 9, 18);

  // Aqua Button Pressed
  Bounds.X1 := 400;
  Bounds.Y1 := 30;
  Bounds.X2 := Bounds.X1 + 150;
  Bounds.Y2 := Bounds.Y1 + 36;

  Clr.Initialize(0, 50, 180, 180);
  FGraphics.FillColor := Clr;
  Clr.Initialize(0, 0, 0, 255);
  FGraphics.LineColor := Clr;
  FGraphics.LineWidth := 2;
  FGraphics.RoundedRect(Bounds, 12, 18);

  Clr.Initialize(0, 0, 0, 0);
  FGraphics.LineColor := Clr;

  C1.Initialize(60, 160, 255, 255);
  C2.Initialize(100, 255, 255, 0);
  FGraphics.FillLinearGradient(Bounds.X1, Bounds.Y1 + 2, Bounds.X1, Bounds.Y1 + 25, C1, C2);
  FGraphics.RoundedRect(Bounds.X1 + 3, Bounds.Y1 + 2.5, Bounds.X2 - 3, Bounds.Y1 + 30, 9, 18, 1, 1);

  Clr.Initialize(0, 0, 50, 255);
  FGraphics.FillColor := Clr;
  FGraphics.NoLine;
  FGraphics.TextAlignment(tahCenter, tavCenter);
  FGraphics.Text(Bounds.CenterX, Bounds.CenterY, 'Aqua Pressed', False, 0);

  C1.Initialize(0, 180, 255, 0);
  C2.Initialize(0, 200, 255, 255);
  FGraphics.FillLinearGradient(Bounds.X1, Bounds.Y2 - 25, Bounds.X1, Bounds.Y2 - 5, C1, C2);
  FGraphics.RoundedRect(Bounds.X1 + 3, Bounds.Y2 - 25, Bounds.X2 - 3, Bounds.Y2 - 2, 1, 1, 9, 18);

  // Basic Shapes -- Ellipse
  // ===========================================
  FGraphics.LineWidth := 3.5;
  FGraphics.SetLineColor(20, 80, 80);
  FGraphics.SetFillColor(200, 255, 80, 200);
  FGraphics.Ellipse(450, 200, 50, 90);

  // Paths
  // ===========================================
  FGraphics.ResetPath;
  FGraphics.SetFillColor(255, 0, 0, 100);
  FGraphics.SetLineColor(0, 0, 255, 100);
  FGraphics.LineWidth := 2;
  FGraphics.MoveTo(150, 100);
  FGraphics.HorizontalLineRel(-75);
  FGraphics.ArcRel(75, 75, 0, True, False, 75, -75);
  FGraphics.ClosePolygon;
  FGraphics.DrawPath;

  FGraphics.ResetPath;
  FGraphics.SetFillColor(255, 255, 0, 100);
  FGraphics.SetLineColor(0, 0, 255, 100);
  FGraphics.LineWidth := 2;
  FGraphics.MoveTo(275 * 0.5, 175 * 0.5);
  FGraphics.VerticalLineRel(-75);
  FGraphics.ArcRel(75, 75, 0, False, False, -75, 75);
  FGraphics.ClosePolygon;
  FGraphics.DrawPath;

  FGraphics.ResetPath;
  FGraphics.NoFill;
  FGraphics.SetLineColor(127, 0, 0);
  FGraphics.LineWidth := 5;
  FGraphics.MoveTo(300, 175);
  FGraphics.LineRel(25, -12.5);
  FGraphics.ArcRel(12.5, 12.5, Deg2Rad(-30), False, True, 25, -12.5);
  FGraphics.LineRel(25, -12.5);
  FGraphics.ArcRel(12.5, 25, Deg2Rad(-30), False, True, 25, -12.5);
  FGraphics.LineRel(25, -12.5);
  FGraphics.ArcRel(12.5, 27.5, Deg2Rad(-30), False, True, 25, -12.5);
  FGraphics.LineRel(50, -25);
  FGraphics.ArcRel(12.5, 50, Deg2Rad(-30), False, True, 25, -12.5);
  FGraphics.LineRel(25, -12.5);
  FGraphics.DrawPath;

  // Master Alpha. From now on everything will be translucent
  // ===========================================
  FGraphics.MasterAlpha := 0.85;

  // Image Transformations
  // ===========================================
  with RenderingBufferImage[0] do
    Img := TAgg2DImage.Create(Buffer, Width, Height, Stride);

  FGraphics.ImageFilter := ifBilinear;

  // FGraphics.imageResample(NoResample );
  // FGraphics.imageResample(ResampleAlways );
  FGraphics.ImageResample := irOnZoomOut;

  // Set the initial image blending operation as BlendDst, that actually
  // does nothing.
  // -----------------
  FGraphics.ImageBlendMode := bmDestination;


  case FImage of
    1 : // Transform the whole image to the destination rectangle
      FGraphics.TransformImage(Img, 450, 200, 595, 350);

    2 : // Transform the rectangular part of the image to the destination
        // rectangle
      FGraphics.TransformImage(Img, 60, 60, Img.Width - 60, Img.Height - 60,
        450, 200, 595, 350);

    3 : // Transform the whole image to the destination parallelogram
      begin
        Parl[0] := 450;
        Parl[1] := 200;
        Parl[2] := 595;
        Parl[3] := 220;
        Parl[4] := 575;
        Parl[5] := 350;

        FGraphics.TransformImage(Img, @Parl[0]);
      end;

    4 : // Transform the rectangular part of the image to the destination
        // parallelogram
      begin
        Parl[0] := 450;
        Parl[1] := 200;
        Parl[2] := 595;
        Parl[3] := 220;
        Parl[4] := 575;
        Parl[5] := 350;

        FGraphics.TransformImage(Img, 60, 60, Img.Width - 60, Img.Height - 60,
          @Parl[0]);
      end;


    5 : // Transform image to the destination path. The scale is determined
        // by a rectangle
      begin
        FGraphics.ResetPath;
        FGraphics.MoveTo(450, 200);
        FGraphics.CubicCurveTo(595, 220, 575, 350, 595, 350);
        FGraphics.LineTo(470, 340);
        FGraphics.TransformImagePath(Img, 450, 200, 595, 350);
      end;

    6 : // Transform image to the destination path. The scale is determined
        // by a rectangle
      begin
        FGraphics.ResetPath;
        FGraphics.MoveTo(450, 200);
        FGraphics.CubicCurveTo(595, 220, 575, 350, 595, 350);
        FGraphics.LineTo(470, 340);
        FGraphics.TransformImagePath(Img, 60, 60, Img.Width - 60,
          Img.Height - 60, 450, 200, 595, 350);
      end;


    7 : // Transform image to the destination path. The transformation is
        // determined by a parallelogram
      begin
        FGraphics.ResetPath;
        FGraphics.MoveTo(450, 200);
        FGraphics.CubicCurveTo(595, 220, 575, 350, 595, 350);
        FGraphics.LineTo(470, 340);

        Parl[0] := 450;
        Parl[1] := 200;
        Parl[2] := 595;
        Parl[3] := 220;
        Parl[4] := 575;
        Parl[5] := 350;

        FGraphics.TransformImagePath(Img, @Parl[0]); { 7 }
      end;


    8 : // Transform the rectangular part of the image to the destination
        // path. The transformation is determined by a parallelogram
      begin
        FGraphics.ResetPath;
        FGraphics.MoveTo(450, 200);
        FGraphics.CubicCurveTo(595, 220, 575, 350, 595, 350);
        FGraphics.LineTo(470, 340);

        Parl[0] := 450;
        Parl[1] := 200;
        Parl[2] := 595;
        Parl[3] := 220;
        Parl[4] := 575;
        Parl[5] := 350;

        FGraphics.TransformImagePath(Img, 60, 60, Img.Width - 60,
          Img.Height - 60, @Parl[0]);
      end;
  end;

  // Free Image
  Img.Free;

  // Add/Sub/Contrast Blending Modes
  FGraphics.NoLine;
  FGraphics.SetFillColor(70, 70, 0);
  FGraphics.BlendMode := bmAlpha;
  FGraphics.Ellipse(500, 280, 20, 40);

  FGraphics.SetFillColor(255, 255, 255);
  FGraphics.BlendMode := bmContrast;
  FGraphics.Ellipse(500 + 40, 280, 20, 40);

  // Radial Gradient.
  FGraphics.BlendMode := bmAlpha;
  C1.Initialize(255, 255, 0, 0);
  C2.Initialize(0, 0, 127);
  C3.Initialize(0, 255, 0, 0);
  FGraphics.FillRadialGradient(400, 500, 40, C1, C2, C3);
  FGraphics.Ellipse(400, 500, 40, 40);

  // More ...
  {$IFDEF More}
  FGraphics.MasterAlpha := 1;

  FGraphics.SetLineColor(50, 60, 70);

  C1.Initialize(255, 0, 0, 255);
  C2.Initialize(0, 255, 0, 255);
  FGraphics.LineLinearGradient(0, 0, 500, 0, C1, C2);

  FGraphics.SetFillColor(255, 0, 0);
  FGraphics.LineJoin := ljMiter;
  FGraphics.LineWidth := 15;
  FGraphics.Triangle(10, 10, 100, 20, 50, 150);

  FGraphics.LineJoin := ljRound;
  FGraphics.LineWidth := 4;
  FGraphics.NoFill;
  FGraphics.AddDash(7, 7);
  FGraphics.Rectangle(55, 540, 135, 495);
  FGraphics.RemoveAllDashes;

  FGraphics.MasterAlpha := 0.5;

  FGraphics.SetFillColor(255, 127, 65);
  FGraphics.Star(300, 300, 30, 70, 55, 5);
  FGraphics.Arc(400, 400, 30, 30, 300, 1150);

  FGraphics.LineWidth := 20;
  FGraphics.LineCap := lcRound;
  FGraphics.Curve(80, 400, 90, 220, 190, 390 );
  FGraphics.Curve(80, 500, 90, 320, 190, 490, 310, 330);

  Poly[0] := 400;
  Poly[1] := 580;

  Poly[2] := 530;
  Poly[3] := 400;

  Poly[4] := 590;
  Poly[5] := 500;

  Poly[6] := 450;
  Poly[7] := 380;

  Poly[8] := 490;
  Poly[9] := 570;

  Poly[10] := 420;
  Poly[11] := 420;

  FGraphics.FillEvenOdd := False;
  FGraphics.LineWidth := 3;
  FGraphics.Polygon(@Poly[0], 6);

  FGraphics.SetLineColor(221, 160, 221);
  FGraphics.LineWidth := 6;
  FGraphics.Polyline(@Poly[0], 6);
  {$ENDIF}

  // TIMER DRAW
  // ----------
  Tm := GetElapsedTime;

  FTimer.Attach(RenderingBufferWindow.Buffer, RenderingBufferWindow.Width,
    RenderingBufferWindow.Height, RenderingBufferWindow.Stride);

  FTimer.AntiAliasGamma := 1.4;

  FTimer.FlipText := not CFlipY;
  FTimer.Viewport(0, 0, 600, 600, 0, 0, Width, Height, CViewportOption);

  Str(Tm: 0: 2, Timer);

  Timer := 'Frame time: ' + Timer + ' ms';

  Fps := Trunc(1000 / Tm);

  Str(Fps, Rate);

  Timer := Timer + ' (' + Rate + ' FPS)';

  FTimer.Font(PAnsiChar(GFontArial), 15, True, False, fcVector);
  FTimer.NoLine;
  FTimer.SetFillColor(255, 0, 0);
  FTimer.Text(350, 8, Timer);
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('"Quick and dirty prototype" of 2D drawing API for AGG.'#13#13
      + 'Written and published by Maxim Shemanarev (c) 2005 - 2006.   '#13
      + 'Ported to Object Pascal by Milan Marusinec (c) 2007.'#13#13
      + 'How to play with:'#13#13
      + 'Key Down - Rotate clockwise'#13
      + 'Key Up - Rotate counterclockwise'#13
      + 'Key Right - Next image transformation'#13
      + 'Key Left - Previous image transformation'#13
      + 'Key Plus - Increase Gamma'#13 + 'Key Minus - Decrease Gamma');

  if Key = Cardinal(kcDown) then
  begin
    FAngle := FAngle - CAngleStep;

    if FAngle < 0 then
      FAngle := 360 - CAngleStep;

    ForceRedraw;
  end;

  if Key = Cardinal(kcUp) then
  begin
    FAngle := FAngle + CAngleStep;

    if FAngle > 360 then
      FAngle := CAngleStep;

    ForceRedraw;
  end;

  if Key = Cardinal(kcRight) then
  begin
    Inc(FImage);

    if FImage > 8 then
      FImage := 1;

    ForceRedraw;
  end;

  if Key = Cardinal(kcLeft) then
  begin
    Dec(FImage);

    if FImage < 1 then
      FImage := 8;

    ForceRedraw;
  end;

  if Key = Cardinal(kcPadPlus) then
  begin
    FGamma := FGamma + CGammaStep;

    Str(FGamma: 0: 2, FGmText);
    FGmText := 'Gamma: ' + FGmText;

    ForceRedraw;
  end;

  if Key = Cardinal(kcPadMinus) then
  begin
    FGamma := FGamma - CGammaStep;

    Str(FGamma: 0: 2, FGmText);
    FGmText := 'Gamma: ' + FGmText;

    ForceRedraw;
  end;
end;

var
  ImageName, P, N, X: ShortString;
  Text: AnsiString;

begin
  if Agg2DUsesFreeType then
  begin
    GFontTimes := 'times.ttf';
    GFontArial := 'arial.ttf';
    GFontVerdana := 'verdana.ttf';
  end;

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'Agg2DDemo (F1-Help)';

    ImageName := 'spheres2';

{$IFDEF WIN32}
    if ParamCount > 0 then
    begin
      SpreadName(ParamStr(1), P, N, X);

      ImageName := FoldName(P, N, '');
    end;
{$ENDIF}

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres2' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
