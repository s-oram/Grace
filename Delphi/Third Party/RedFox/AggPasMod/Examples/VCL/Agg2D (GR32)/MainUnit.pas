unit MainUnit;

interface

{$DEFINE More}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Agg2D, AggBasics, AggPixelFormat,
  AggTransAffine, Agg2D;

type
  TFmGr32Agg2D = class(TForm)
    PaintBox32: TPaintBox32;
    Bitmap32List: TBitmap32List;
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGr32Agg2D: TGr32Agg2D;
    FImageTransformationIndex: Integer;
    FAngle: Double;
  public
    { Public-Deklarationen }
  end;

var
  FmGr32Agg2D: TFmGr32Agg2D;

implementation

{$R *.dfm}

{ TFmGr32Agg2D }

procedure TFmGr32Agg2D.FormCreate(Sender: TObject);
begin
  FGr32Agg2D := TGr32Agg2D.Create;
  PaintBox32.BufferOversize := 0;
  FImageTransformationIndex := 6;
end;

procedure TFmGr32Agg2D.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGr32Agg2D);
end;

procedure TFmGr32Agg2D.PaintBox32PaintBuffer(Sender: TObject);
var
  Bounds: TRectDouble;

  Img: TGr32Agg2DImage;

  Parl: TAggParallelogram;
  {$IFDEF More}
  Poly: array [0..11] of Double;
  {$ENDIF}
begin
  with FGr32Agg2D do
  begin
    Attach(PaintBox32.Buffer);

    {$IFDEF BlackBackground}
    ClearAll(0, 0, 0);
    BlendMode := bmPlus;
    {$ELSE}
    ClearAll(255, 255, 255);
    {$ENDIF}

    AntiAliasGamma := 1.4;

    // Set FlipText := true if you have the Y axis upside down.
    FlipText := False;

    {$IFDEF UseClipBox}
    ClipBox(50, 50, PaintBox32.Width - 50,
      PaintBox32.Height - 50);
    {$ENDIF}

    // Transformations - Rotate around (300, 300) to 5 degree
    Translate(-300, -300);
    Rotate(Deg2Rad(FAngle));
    Translate(300, 300);
    // Skew(0.1, 0.1);

    // Viewport - set 0,0,600,600 to the actual window size
    // preserving aspect ratio and placing the viewport in the center.
    // To ignore aspect ratio use Agg2D::Anisotropic
    // Note that the viewport just adds transformations to the current
    // affine matrix. So that, set the viewport *after* all transformations!
    Viewport(0, 0, 600, 600, 0, 0, PaintBox32.Width,
      PaintBox32.Height, voAnisotropic);

    // Rounded Rect
    LineColor := clBlack32;
    NoFill;
    RoundedRect(0.5, 0.5, 600 - 0.5, 600 - 0.5, 20);

    // Regular Text
    {$IFDEF More}
    Font(PAnsiChar(Self.Font.Name), 14, False, False);
    FillColor := clBlack32;
    NoLine;
    Text(100, 20, PAnsiChar('Regular Raster Text -- Fast, but can''t '
      + 'be rotated'));{ }
    {$ENDIF}

    // Outlined Text
    Font(PAnsiChar(Self.Font.Name), 50, False, False, fcVector);
    LineColor := Color32(50, 0, 0);
    FillColor := Color32(180, 200, 100);
    LineWidth := 1;
    Text(100.5, 50.5, 'Outlined Text');

    // Gamma Text
    {$IFDEF More}
    Font(PAnsiChar(Self.Font.Name), 38, True, True, fcVector);
    FillLinearGradient(50, 1, 300, 10, clRed32, clLime32);

    NoLine;
    Text(12.5, 565.5, 'Gradient Text');
    //Rectangle(12.5, 565.5, 290, 590);
    {$ENDIF}

    // Text Alignment
    LineColor := clBlack32;
    Line(100.5, 150.5, 400.5, 150.5);
    Line(250.5, 130.5, 250.5, 170.5);
    Line(100.5, 200.5, 400.5, 200.5);
    Line(250.5, 180.5, 250.5, 220.5);
    Line(100.5, 250.5, 400.5, 250.5);
    Line(250.5, 230.5, 250.5, 270.5);
    Line(100.5, 300.5, 400.5, 300.5);
    Line(250.5, 280.5, 250.5, 320.5);
    Line(100.5, 350.5, 400.5, 350.5);
    Line(250.5, 330.5, 250.5, 370.5);
    Line(100.5, 400.5, 400.5, 400.5);
    Line(250.5, 380.5, 250.5, 420.5);
    Line(100.5, 450.5, 400.5, 450.5);
    Line(250.5, 430.5, 250.5, 470.5);
    Line(100.5, 500.5, 400.5, 500.5);
    Line(250.5, 480.5, 250.5, 520.5);
    Line(100.5, 550.5, 400.5, 550.5);
    Line(250.5, 530.5, 250.5, 570.5);

    FillColor := Color32(100, 50, 50);
    NoLine;
    {$IFDEF UseTextHints}
    TextHints := True;
    {$ELSE}
    TextHints := False;
    {$ENDIF}
    Font(PAnsiChar(Self.Font.Name), 40, False, False, fcVector);

    TextAlignment(tahLeft, tavBottom);
    Text(250, 150, 'Left-Bottom', True, 0, 0);

    TextAlignment(tahCenter, tavBottom);
    Text(250, 200, 'Center-Bottom', True, 0, 0);

    TextAlignment(tahRight, tavBottom);
    Text(250, 250, 'Right-Bottom', True, 0, 0);

    TextAlignment(tahLeft, tavCenter);
    Text(250, 300, 'Left-Center', True, 0, 0);

    TextAlignment(tahCenter, tavCenter);
    Text(250, 350, 'Center-Center', True, 0, 0);

    TextAlignment(tahRight, tavCenter);
    Text(250, 400, 'Right-Center', True, 0, 0);

    TextAlignment(tahLeft, tavTop);
    Text(250, 450, 'Left-Top', True, 0, 0);

    TextAlignment(tahCenter, tavTop);
    Text(250, 500, 'Center-Top', True, 0, 0);

    TextAlignment(tahRight, tavTop);
    Text(250, 550, 'Right-Top', True, 0, 0);

    // Gradients (Aqua Buttons)
    // =======================================
    Font(PAnsiChar(Self.Font.Name), 20, False, False, fcVector);

    Bounds.X1 := 400;
    Bounds.Y1 := 80;
    Bounds.X2 := Bounds.X1 + 150;
    Bounds.Y2 := Bounds.Y1 + 36;

    FillColor := Color32(0, 50, 180, 180);
    LineColor := Color32(0, 0, 80, 255);
    LineWidth := 1;
    RoundedRect(Bounds, 12, 18);

    LineColor := Color32(0, 0, 0, 0);

    FillLinearGradient(Bounds.X1, Bounds.Y1, Bounds.X1,
      Bounds.Y1 + 30, Color32(100, 200, 255, 255), Color32(255, 255, 255, 0));
    RoundedRect(Bounds.X1 + 3, Bounds.Y1 + 2.5, Bounds.X2 - 3,
      Bounds.Y1 + 30, 9, 18, 1, 1);

    FillColor := Color32(0, 0, 50, 200);
    NoLine;
    TextAlignment(tahCenter, tavCenter);
    Text(Bounds.CenterX, Bounds.CenterY, 'Aqua Button', True, 0, 0);

    FillLinearGradient(Bounds.X1, Bounds.Y2 - 20, Bounds.X1,
      Bounds.Y2 - 3, Color32(0, 0, 255, 0), Color32(100, 255, 255, 255));
    RoundedRect(Bounds.X1 + 3, Bounds.Y2 - 20, Bounds.X2 - 3,
      Bounds.Y2 - 2, 1, 1, 9, 18);

    // Aqua Button Pressed
    Bounds.X1 := 400;
    Bounds.Y1 := 30;
    Bounds.X2 := Bounds.X1 + 150;
    Bounds.Y2 := Bounds.Y1 + 36;

    FillColor := Color32(0, 50, 180, 180);
    LineColor := Color32(0, 0, 0, 255);
    LineWidth := 2;
    RoundedRect(Bounds, 12, 18);

    LineColor := Color32(0, 0, 0, 0);

    FillLinearGradient(Bounds.X1, Bounds.Y1 + 2, Bounds.X1,
      Bounds.Y1 + 25, Color32(60, 160, 255, 255), Color32(100, 255, 255, 0));
    RoundedRect(Bounds.X1 + 3, Bounds.Y1 + 2.5, Bounds.X2 - 3,
      Bounds.Y1 + 30, 9, 18, 1, 1);

    FillColor := Color32(0, 0, 50, 255);
    NoLine;
    TextAlignment(tahCenter, tavCenter);
    Text(Bounds.CenterX, Bounds.CenterY, 'Aqua Pressed', False, 0);

    FillLinearGradient(Bounds.X1, Bounds.Y2 - 25, Bounds.X1,
      Bounds.Y2 - 5, Color32(0, 180, 255, 0), Color32(0, 200, 255, 255));
    RoundedRect(Bounds.X1 + 3, Bounds.Y2 - 25, Bounds.X2 - 3,
      Bounds.Y2 - 2, 1, 1, 9, 18);

    // Basic Shapes -- Ellipse
    // ===========================================
    LineWidth := 3.5;
    LineColor := Color32(20, 80, 80);
    FillColor := Color32(200, 255, 80, 200);
    Ellipse(450, 200, 50, 90);

    // Paths
    // ===========================================
    ResetPath;
    FillColor := Color32(255, 0, 0, 100);
    LineColor := Color32(0, 0, 255, 100);
    LineWidth := 2;
    MoveTo(150, 100);
    HorizontalLineRel(-75);
    ArcRel(75, 75, 0, True, False, 75, -75);
    ClosePolygon;
    DrawPath;

    ResetPath;
    FillColor := Color32(255, 255, 0, 100);
    LineColor := Color32(0, 0, 255, 100);
    LineWidth := 2;
    MoveTo(275 * 0.5, 175 * 0.5);
    VerticalLineRel(-75);
    ArcRel(75, 75, 0, False, False, -75, 75);
    ClosePolygon;
    DrawPath;

    ResetPath;
    NoFill;
    LineColor := Color32(127, 0, 0);
    LineWidth := 5;
    MoveTo(300, 175);
    LineRel(25, -12.5);
    ArcRel(12.5, 12.5, Deg2Rad(-30), False, True, 25, -12.5);
    LineRel(25, -12.5);
    ArcRel(12.5, 25, Deg2Rad(-30), False, True, 25, -12.5);
    LineRel(25, -12.5);
    ArcRel(12.5, 27.5, Deg2Rad(-30), False, True, 25, -12.5);
    LineRel(50, -25);
    ArcRel(12.5, 50, Deg2Rad(-30), False, True, 25, -12.5);
    LineRel(25, -12.5);
    DrawPath;

    // Master Alpha. From now on everything will be translucent
    // ===========================================
    MasterAlpha := 0.85;

    // Image Transformations
    // ===========================================
    Img := TGr32Agg2DImage.Create(Bitmap32List.Bitmap[0]);

    ImageFilter := ifBilinear;

    // imageResample(NoResample);
    // imageResample(ResampleAlways);
    ImageResample := irOnZoomOut;

    // Set the initial image blending operation as BlendDst, that actually
    // does nothing.
    // -----------------
    ImageBlendMode := bmDestination;

    case FImageTransformationIndex of
      1 : // Transform the whole image to the destination rectangle
        TransformImage(Img, 450, 200, 595, 350);

      2 : // Transform the rectangular part of the image to the destination
          // rectangle
        TransformImage(Img, 60, 60, Img.Width - 60, Img.Height - 60,
          450, 200, 595, 350);

      3 : // Transform the whole image to the destination parallelogram
        begin
          Parl[0] := 450;
          Parl[1] := 200;
          Parl[2] := 595;
          Parl[3] := 220;
          Parl[4] := 575;
          Parl[5] := 350;

          TransformImage(Img, @Parl[0]);
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

          TransformImage(Img, 60, 60, Img.Width - 60, Img.Height - 60,
            @Parl[0]);
        end;


      5 : // Transform image to the destination path. The scale is determined
          // by a rectangle
        begin
          ResetPath;
          MoveTo(450, 200);
          CubicCurveTo(595, 220, 575, 350, 595, 350);
          LineTo(470, 340);
          TransformImagePath(Img, 450, 200, 595, 350);
        end;

      6 : // Transform image to the destination path. The scale is determined
          // by a rectangle
        begin
          ResetPath;
          MoveTo(450, 200);
          CubicCurveTo(595, 220, 575, 350, 595, 350);
          LineTo(470, 340);
          TransformImagePath(Img, 60, 60, Img.Width - 60,
            Img.Height - 60, 450, 200, 595, 350);
        end;


      7 : // Transform image to the destination path. The transformation is
          // determined by a parallelogram
        begin
          ResetPath;
          MoveTo(450, 200);
          CubicCurveTo(595, 220, 575, 350, 595, 350);
          LineTo(470, 340);

          Parl[0] := 450;
          Parl[1] := 200;
          Parl[2] := 595;
          Parl[3] := 220;
          Parl[4] := 575;
          Parl[5] := 350;

          TransformImagePath(Img, @Parl[0]); { 7 }
        end;


      8 : // Transform the rectangular part of the image to the destination
          // path. The transformation is determined by a parallelogram
        begin
          ResetPath;
          MoveTo(450, 200);
          CubicCurveTo(595, 220, 575, 350, 595, 350);
          LineTo(470, 340);

          Parl[0] := 450;
          Parl[1] := 200;
          Parl[2] := 595;
          Parl[3] := 220;
          Parl[4] := 575;
          Parl[5] := 350;

          TransformImagePath(Img, 60, 60, Img.Width - 60,
            Img.Height - 60, @Parl[0]);
        end;
    end;

    // Free Image
    Img.Free;

    // Add/Sub/Contrast Blending Modes
    NoLine;
    FillColor := Color32(70, 70, 0);
    BlendMode := bmAlpha;
    Ellipse(500, 280, 20, 40);

    FillColor := Color32(255, 255, 255);
    BlendMode := bmContrast;
    Ellipse(500 + 40, 280, 20, 40);

    // Radial Gradient.
    BlendMode := bmAlpha;
    FillRadialGradient(400, 500, 40, Color32(255, 255, 0, 0),
      Color32(0, 0, 127), Color32(0, 255, 0, 0));
    Ellipse(400, 500, 40, 40);

    // More ...
    {$IFDEF More}
    MasterAlpha := 1;
    LineColor := Color32(50, 60, 70);
    LineLinearGradient(0, 0, 500, 0, clRed32, clLime32);

    FillColor := Color32(255, 0, 0);
    LineJoin := ljMiter;
    LineWidth := 15;
    Triangle(10, 10, 100, 20, 50, 150);

    LineJoin := ljRound;
    LineWidth := 4;
    NoFill;
    AddDash(7, 7);
    Rectangle(55, 540, 135, 495);
    RemoveAllDashes;

    MasterAlpha := 0.5;

    FillColor := Color32(255, 127, 65);
    Star(300, 300, 30, 70, 55, 5);
    Arc(400, 400, 30, 30, 300, 1150);

    LineWidth := 20;
    LineCap := lcRound;
    Curve(80, 400, 90, 220, 190, 390 );
    Curve(80, 500, 90, 320, 190, 490, 310, 330);

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

    FillEvenOdd := False;
    LineWidth := 3;
    Polygon(@Poly[0], 6);

    LineColor := Color32(221, 160, 221);
    LineWidth := 6;
    Polyline(@Poly[0], 6);
    {$ENDIF}
  end;
end;

procedure TFmGr32Agg2D.PaintBox32Resize(Sender: TObject);
begin
  //;
end;

end.

