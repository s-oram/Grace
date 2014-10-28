unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, AggBasics, AggColor, Agg2D, Agg2DControl;

type
  TFmAgg2D = class(TForm)
    MainMenu: TMainMenu;
    MiFile: TMenuItem;
    MiExit: TMenuItem;
    N1: TMenuItem;
    MiSaveAs: TMenuItem;
    SaveDialog: TSaveDialog;
    MiExample: TMenuItem;
    MiEllipses: TMenuItem;
    MiLines: TMenuItem;
    MiLinesRandom: TMenuItem;
    MiEllipsesRandom: TMenuItem;
    MiLinesCircle: TMenuItem;
    MiText: TMenuItem;
    MiTextLoremIpsum: TMenuItem;
    MiTextRandom: TMenuItem;
    MiLinesAnimation: TMenuItem;
    Timer: TTimer;
    MiSettings: TMenuItem;
    MiAlwaysClear: TMenuItem;
    MiCurves: TMenuItem;
    MiCurvesRandom: TMenuItem;
    MiCurvesCircle: TMenuItem;
    MiArcs: TMenuItem;
    MiArcsRandom: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    MiTriangles: TMenuItem;
    MiTrianglesRandom: TMenuItem;
    MiEllipsesCircles: TMenuItem;
    MiCircleAnimation: TMenuItem;
    MiTrianglesAnimation: TMenuItem;
    MiTextAnimation: TMenuItem;
    MiCurvesAnimation: TMenuItem;
    MiRectangles: TMenuItem;
    MiRectanglesRandom: TMenuItem;
    N4: TMenuItem;
    MiTransformations: TMenuItem;
    MiRotate: TMenuItem;
    MiLinesGradient: TMenuItem;
    MiEllipsesGradient: TMenuItem;
    MiRectanglesAnimation: TMenuItem;
    MiStars: TMenuItem;
    MiStarsRandom: TMenuItem;
    MiStarAnimation: TMenuItem;
    N5: TMenuItem;
    MiDemo: TMenuItem;
    Agg2DControl: TAgg2DControl;
    MiPolygons: TMenuItem;
    MiRectanglesArbitrary: TMenuItem;
    MiPolygonsRandom: TMenuItem;
    MiPolygonsAnimation: TMenuItem;
    MiColors: TMenuItem;
    MiColorsRed: TMenuItem;
    MiColorsGreen: TMenuItem;
    MiColorsBlue: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure MiAlwaysClearClick(Sender: TObject);
    procedure MiArcsRandomClick(Sender: TObject);
    procedure MiCircleAnimationClick(Sender: TObject);
    procedure MiCurvesAnimationClick(Sender: TObject);
    procedure MiCurvesCircleClick(Sender: TObject);
    procedure MiCurvesRandomClick(Sender: TObject);
    procedure MiEllipsesCirclesClick(Sender: TObject);
    procedure MiEllipsesGradientClick(Sender: TObject);
    procedure MiEllipsesRandomClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiLinesAnimationClick(Sender: TObject);
    procedure MiLinesCircleClick(Sender: TObject);
    procedure MiLinesGradientClick(Sender: TObject);
    procedure MiLinesRandomClick(Sender: TObject);
    procedure MiRectanglesAnimationClick(Sender: TObject);
    procedure MiRectanglesRandomClick(Sender: TObject);
    procedure MiRotateClick(Sender: TObject);
    procedure MiSaveAsClick(Sender: TObject);
    procedure MiStarAnimationClick(Sender: TObject);
    procedure MiStarsRandomClick(Sender: TObject);
    procedure MiTextAnimationClick(Sender: TObject);
    procedure MiTextLoremIpsumClick(Sender: TObject);
    procedure MiTextRandomClick(Sender: TObject);
    procedure MiTrianglesAnimationClick(Sender: TObject);
    procedure MiTrianglesRandomClick(Sender: TObject);
    procedure TimerCircleAnimation(Sender: TObject);
    procedure TimerCurvesAnimation(Sender: TObject);
    procedure TimerDemoAnimation(Sender: TObject);
    procedure TimerLineAnimation(Sender: TObject);
    procedure TimerRectanglesAnimation(Sender: TObject);
    procedure TimerStarsAnimation(Sender: TObject);
    procedure TimerTextAnimation(Sender: TObject);
    procedure TimerTrianglesAnimation(Sender: TObject);
    procedure TimerPolygonsAnimation(Sender: TObject);
    procedure MiDemoClick(Sender: TObject);
    procedure MiRectanglesArbitraryClick(Sender: TObject);
    procedure MiPolygonsRandomClick(Sender: TObject);
    procedure MiPolygonsAnimationClick(Sender: TObject);
    procedure MiColorsRedClick(Sender: TObject);
    procedure MiColorsGreenClick(Sender: TObject);
    procedure MiColorsBlueClick(Sender: TObject);
  private
(*
    FAgg2D: TAgg2D;
    FBuffer: array [0..1] of TPixelMap;
*)
  protected
    procedure ClearCanvas;
    procedure DrawRandomEllipse;
    procedure DrawRandomCircle;
    procedure DrawRandomTriangle;
    procedure DrawRandomRectangle;
    procedure DrawRandomStar;
    procedure DrawRandomPolygon;
  public
  end;

var
  FmAgg2D: TFmAgg2D;

implementation

uses
  Math;

{$R *.dfm}

procedure TFmAgg2D.FormDestroy(Sender: TObject);
begin
  OnResize := nil;
end;

procedure TFmAgg2D.FormShow(Sender: TObject);
begin
  with Agg2DControl.Agg2D do
  begin
    ClearCanvas;
    FillColor := CRgba8Black;
    NoLine;
    FlipText := True;
    Font('Arial', 36, True, False, fcVector);
    Text(0, 36, 'Hello World!');
  end;

  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.FormResize(Sender: TObject);
begin
(*
  if (FBuffer[0].Width <> ClientWidth) and
    (FBuffer[0].Height <> ClientHeight) then
  begin
    FBuffer[0].SetSize(ClientWidth, ClientHeight);
    FAgg2D.Attach(FBuffer[0].Buffer, FBuffer[0].Width, FBuffer[0].Height,
      -FBuffer[0].Stride);
    FAgg2D.ClearAll($FF, $FF, $FF, $FF);

    if not MiAlwaysClear.Checked then
    begin
      RowPointer[0] := FBuffer[0].Buffer;
      RowPointer[1] := FBuffer[1].Buffer;
      MinWidth := Min(FBuffer[0].Width, FBuffer[1].Width);
      for Index := 0 to Min(FBuffer[0].Height, FBuffer[1].Height) - 1 do
      begin
        Move(RowPointer[1]^, RowPointer[0]^, MinWidth * SizeOf(Cardinal));
        Inc(RowPointer[1], FBuffer[1].Width);
        Inc(RowPointer[0], FBuffer[0].Width);
      end;
    end;

    FBuffer[1].SetSize(ClientWidth, ClientHeight);

    Agg2DControl.Invalidate;
  end;
*)
end;

procedure TFmAgg2D.FormClick(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TFmAgg2D.MiSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Agg2DControl.Buffer.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TFmAgg2D.MiExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFmAgg2D.ClearCanvas;
begin
  if MiAlwaysClear.Checked then
    Agg2DControl.Agg2D.ClearAll($FF, $FF, $FF, $FF);
end;

procedure TFmAgg2D.MiAlwaysClearClick(Sender: TObject);
begin
  MiAlwaysClear.Checked := not MiAlwaysClear.Checked;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmAgg2D.DrawRandomEllipse;
begin
  with Agg2DControl, Agg2D do
  begin
    LineColor := RandomRgba8;
    FillColor := RandomRgba8;
    LineWidth := 10 * Random;
    Ellipse(Random * Width, Random * Height, Random * Random * Width,
      Random * Random * Height);
  end;
end;

procedure TFmAgg2D.DrawRandomCircle;
begin
  with Agg2DControl, Agg2D do
  begin
    LineColor := RandomRgba8;
    FillColor := RandomRgba8;
    LineWidth := 10 * Random;
    Circle(Random * Width, Random * Height, 16 + 16 * Random);
  end;
end;

procedure TFmAgg2D.DrawRandomTriangle;
begin
  with Agg2DControl, Agg2D do
  begin
    LineColor := RandomRgba8;
    FillColor := RandomRgba8;
    LineWidth := 10 * Random;
    Triangle(Random * Width, Random * Height, Random * Width,
      Random * Height, Random * Width, Random * Height);
  end;
end;

procedure TFmAgg2D.DrawRandomRectangle;
begin
  with Agg2DControl, Agg2D do
  begin
    LineColor := RandomRgba8;
    FillColor := RandomRgba8;
    LineWidth := 10 * Random;
    RoundedRect(Random * Width, Random * Height, Random * Width,
      Random * Height, 32 * Random * Random);
  end;
end;

procedure TFmAgg2D.DrawRandomStar;
begin
  with Agg2DControl, Agg2D do
  begin
    LineColor := RandomRgba8;
    FillColor := RandomRgba8;
    LineWidth := 10 * Random;
    Star(Random * Width, Random * Height, 16 + 16 * Random, 16 + 16 * Random,
      2 * Pi * Random, 3 + Random(9));
  end;
end;

procedure TFmAgg2D.DrawRandomPolygon;
var
  PolygonPoints: array of TPointDouble;
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    LineColor := RandomRgba8;
    FillColor := RandomRgba8;
    LineWidth := 10 * Random;
    FillEvenOdd := Boolean(Random(2));
    SetLength(PolygonPoints, 3 + Random(9));
    for Index := 0 to Length(PolygonPoints) - 1 do
    begin
      PolygonPoints[Index].X := Random * Width;
      PolygonPoints[Index].Y := Random * Height;
    end;
    Polygon(@PolygonPoints[0], Length(PolygonPoints));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmAgg2D.MiDemoClick(Sender: TObject);
begin
  ClearCanvas;
  Timer.OnTimer := TimerDemoAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiLinesRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to $FF do
    begin
      LineColor := RandomRgba8;
      FillColor := RandomRgba8;
      LineWidth := 10 * Random;
      Line(Random * Width, Random * Height, Random * Width, Random * Height);
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiLinesCircleClick(Sender: TObject);
var
  Index: Integer;
  IntWidth: Integer;
  Temp: Double;
  Sn, Cn: Double;
begin
  with Agg2DControl.Agg2D do
  begin
    ClearCanvas;
    IntWidth := 1;
    LineWidth := IntWidth;
    for Index := 0 to (360 div IntWidth) - 1 do
    begin
      Temp := IntWidth * Index / 360;
      LineColor := HueSaturationLuminanceToRgb8(Temp, 1, 0.5);
      SinCos(2 * Pi * Temp, Sn, Cn);
      Line(0.5 * Width, 0.5 * Height, 0.5 * Width * (1 + Sn),
        0.5 * Height * (1 + Cn));
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiLinesGradientClick(Sender: TObject);
var
  Index: Integer;
  Bounds: TRectDouble;
  Colors: array [0..1] of TAggColorRgba8;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to $FF do
    begin
      LineWidth := 10 * Random;
      Colors[0] := RandomRgba8;
      Colors[1] := RandomRgba8;
      Bounds.X1 := Random * Width;
      Bounds.Y1 := Random * Height;
      Bounds.X2 := Random * Width;
      Bounds.Y2 := Random * Height;
      LineLinearGradient(Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2, Colors[0],
        Colors[1]);
      Line(Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2);
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiLinesAnimationClick(Sender: TObject);
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    ResetPath;
    MoveTo(Random * Width, Random * Height);
  end;

  Timer.OnTimer := TimerLineAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiCurvesRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to $FF do
    begin
      LineColor := RandomRgba8;
      FillColor := RandomRgba8;
      LineWidth := 10 * Random;
      Curve(Random * Width, Random * Height,
        Random * Width, Random * Height,
        Random * Width, Random * Height);
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiCurvesAnimationClick(Sender: TObject);
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    ResetPath;
    MoveTo(Random * Width, Random * Height);
  end;

  Timer.OnTimer := TimerCurvesAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiCurvesCircleClick(Sender: TObject);
var
  Index: Integer;
  IntWidth: Integer;
  Temp: Double;
  Sn, Cn: Double;
  Center: TPointDouble;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    IntWidth := 1;
    LineWidth := 1.5 * IntWidth;
    Center.X := 0.5 * Width;
    Center.Y := 0.5 * Height;
    for Index := 0 to (180 div IntWidth) - 1 do
    begin
      Temp := IntWidth * Index / 180;
      LineColor := HueSaturationLuminanceToRgb8(Temp, 1, 0.5);
      SinCos(2 * Pi * Temp, Sn, Cn);
      Curve(
        Center.X * (1 - 0.1 * Cn), Center.Y * (1 + 0.1 * Sn),
        Center.X * (1 + 0.5 * Sn), Center.Y * (1 + 0.5 * Cn),
        Center.X * (1 + 0.9 * Cn), Center.Y * (1 - 0.9 * Sn)); end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiArcsRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to $FF do
    begin
      LineColor := RandomRgba8;
      FillColor := RandomRgba8;
      LineWidth := 10 * Random;
      Arc(Random * Width, Random * Height, Random * Width, Random * Height,
        2 * Pi * Random, 2 * Pi * Random);
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiEllipsesRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to 99 do
      DrawRandomEllipse;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiEllipsesCirclesClick(Sender: TObject);
var
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to 99 do
      DrawRandomCircle;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiEllipsesGradientClick(Sender: TObject);
var
  Index: Integer;
  Radius: Double;
  Center: TPointDouble;
  Colors: array [0..1] of TAggColorRgba8;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    NoLine;
    for Index := 0 to 99 do
    begin
      Radius := 16 + 16 * Random;
      Colors[0] := RandomRgba8;
      Colors[1] := RandomRgba8;
      Center.X := Random * Width;
      Center.Y := Random * Height;
      FillRadialGradient(Center.X, Center.Y, Radius, Colors[0], Colors[1]);
      Circle(Center.X, Center.Y, Radius);
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiCircleAnimationClick(Sender: TObject);
begin
  ClearCanvas;
  Timer.OnTimer := TimerCircleAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiTrianglesRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  ClearCanvas;
  for Index := 0 to 99 do
    DrawRandomTriangle;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiColorsBlueClick(Sender: TObject);
var
  Index: Integer;
  Rgba8: TAggColorRgba8;
begin
  ClearCanvas;
  for Index := 0 to 99 do

  with Agg2DControl, Agg2D do
  begin
    Rgba8.B := Random($100);
    Rgba8.G := 0;
    Rgba8.R := 0;
    Rgba8.A := Random($100);
    LineColor := Rgba8;

    Rgba8.B := Random($100);
    Rgba8.G := 0;
    Rgba8.R := 0;
    Rgba8.A := Random($100);
    FillColor := Rgba8;

    LineWidth := 10 * Random;
    RoundedRect(Random * Width, Random * Height, Random * Width,
      Random * Height, 32 * Random * Random);
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiColorsGreenClick(Sender: TObject);
var
  Index: Integer;
  Rgba8: TAggColorRgba8;
begin
  ClearCanvas;
  for Index := 0 to 99 do

  with Agg2DControl, Agg2D do
  begin
    Rgba8.G := Random($100);
    Rgba8.R := 0;
    Rgba8.B := 0;
    Rgba8.A := Random($100);
    LineColor := Rgba8;

    Rgba8.G := Random($100);
    Rgba8.R := 0;
    Rgba8.B := 0;
    Rgba8.A := Random($100);
    FillColor := Rgba8;

    LineWidth := 10 * Random;
    RoundedRect(Random * Width, Random * Height, Random * Width,
      Random * Height, 32 * Random * Random);
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiColorsRedClick(Sender: TObject);
var
  Index: Integer;
  Rgba8: TAggColorRgba8;
begin
  ClearCanvas;
  for Index := 0 to 99 do

  with Agg2DControl, Agg2D do
  begin
    Rgba8.R := Random($100);
    Rgba8.G := 0;
    Rgba8.B := 0;
    Rgba8.A := Random($100);
    LineColor := Rgba8;

    Rgba8.R := Random($100);
    Rgba8.G := 0;
    Rgba8.B := 0;
    Rgba8.A := Random($100);
    FillColor := Rgba8;

    LineWidth := 10 * Random;
    RoundedRect(Random * Width, Random * Height, Random * Width,
      Random * Height, 32 * Random * Random);
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiTrianglesAnimationClick(Sender: TObject);
begin
  ClearCanvas;
  Timer.OnTimer := TimerTrianglesAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiRectanglesRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  ClearCanvas;
  for Index := 0 to $FF do
    DrawRandomRectangle;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiRectanglesArbitraryClick(Sender: TObject);
var
  Index: Integer;
  Angle: Double;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to $FF do
    begin
      FillColor := RandomRgba8;
      LineColor := RandomRgba8;
      LineWidth := 10 * Random;
      Angle := Random * 2 * Pi;
      Rotate(Angle);
      Rectangle(2 * Width * Random - 1, 2 * Height * Random - 1,
        Width * Random, Height * Random);
      Rotate(-Angle);
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiRectanglesAnimationClick(Sender: TObject);
begin
  ClearCanvas;
  Timer.OnTimer := TimerRectanglesAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiStarsRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  ClearCanvas;
  for Index := 0 to 99 do
    DrawRandomStar;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiStarAnimationClick(Sender: TObject);
begin
  ClearCanvas;
  Timer.OnTimer := TimerStarsAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiPolygonsAnimationClick(Sender: TObject);
begin
  ClearCanvas;
  Timer.OnTimer := TimerPolygonsAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiPolygonsRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  ClearCanvas;
  for Index := 0 to 99 do
    DrawRandomPolygon;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiTextRandomClick(Sender: TObject);
var
  Index: Integer;
begin
  with Agg2DControl, Agg2D do
  begin
    ClearCanvas;
    for Index := 0 to $3FF do
    begin
      FillColor := RandomRgba8;
      NoLine;
      Font('Arial', 12 + Random(36), Boolean(Random(2)), False, fcVector,
        2 * Pi * Random);
      Text(Random * Width, Random * Height, AnsiChar(33 + Random(60)));
    end;
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiTextLoremIpsumClick(Sender: TObject);
begin
  with Agg2DControl.Agg2D do
  begin
    ClearAll($FF, $FF, $FF, $FF);

    FillColor := CRgba8Black;
    NoLine;
    FlipText := True;

    Font('Arial', 16, False, False, fcRaster);
    Text(0, 16, WrapText('Lorem ipsum dolor sit amet, consetetur sadipscing '
      + 'elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore '
      + 'magna aliquyam erat, sed diam voluptua. At vero eos et accusam et '
      + 'justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea '
      + 'takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor '
      + 'sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor '
      + 'invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. '
      + 'At vero eos et accusam et justo duo dolores et ea rebum. Stet clita '
      + 'kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
      + 'amet.', #13#10, ['.',' ',#9,'-'], 80));
  end;

  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.MiTextAnimationClick(Sender: TObject);
begin
  with Agg2DControl.Agg2D do
  begin
    ClearCanvas;
    if MiAlwaysClear.Checked then
      ClearAll($FF, $FF, $FF, $FF);
    NoLine;
  end;

  Timer.OnTimer := TimerTextAnimation;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TFmAgg2D.MiRotateClick(Sender: TObject);
begin
  Agg2DControl.Agg2D.Rotate(2 * Pi * Random);
  Agg2DControl.Invalidate;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmAgg2D.TimerLineAnimation(Sender: TObject);
var
  X, Y : Double;
begin
  with Agg2DControl, Agg2D do
   begin
     LineColor := RandomRgba8;
     X := Random * Width;
     Y := Random * Height;
     LineWidth := 1 + 7 * Random;
     LineTo(X, Y);
     DrawPath(dpfStrokeOnly);
     ResetPath;
     MoveTo(X, Y);
   end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerCurvesAnimation(Sender: TObject);
var
  X, Y : Double;
begin
  with Agg2DControl, Agg2D do
   begin
     LineColor := RandomRgba8;
     X := Random * Width;
     Y := Random * Height;
     LineWidth := 1 + 7 * Random;
     QuadricCurveTo(Random * Width, Random * Height, X, Y);
     DrawPath(dpfStrokeOnly);
     ResetPath;
     MoveTo(X, Y);
   end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerCircleAnimation(Sender: TObject);
begin
  DrawRandomCircle;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerTrianglesAnimation(Sender: TObject);
begin
  DrawRandomTriangle;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerRectanglesAnimation(Sender: TObject);
begin
  DrawRandomRectangle;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerStarsAnimation(Sender: TObject);
begin
  DrawRandomStar;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerPolygonsAnimation(Sender: TObject);
begin
  DrawRandomPolygon;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerTextAnimation(Sender: TObject);
begin
  with Agg2DControl, Agg2D do
  begin
    FillColor := RandomRgba8;
    Font('Arial', 12 + Random(36), Boolean(Random(2)), False, fcVector,
      2 * Pi * Random);
    Text(Random * Width, Random * Height, AnsiChar(33 + Random(60)));
  end;
  Agg2DControl.Invalidate;
end;

procedure TFmAgg2D.TimerDemoAnimation(Sender: TObject);
var
  Colors: array [0..1] of TAggColorRgba8;
  Bounds: TRectDouble;
begin
  with Agg2DControl, Agg2D do
  begin
    Colors[0] := RandomRgba8;
    Colors[1] := RandomRgba8;

    Bounds.X1 := Random * Width;
    Bounds.Y1 := Random * Height;
    Bounds.X2 := Random * Width;
    Bounds.Y2 := Random * Height;

    LineWidth := 10 * Random;

    case Random(4) of
      0:
        FillLinearGradient(Random * Width, Random * Height, Random * Width,
          Random * Height, Colors[0], Colors[1]);
      1:
        FillRadialGradient(Bounds.CenterX, Bounds.CenterY, 0.5 *
          (Bounds.CenterX + Bounds.CenterY), Colors[0], Colors[1]);
      2:
        FillColor := Colors[0];
      3:
        NoFill;
    end;
    case Random(4) of
      0:
        LineLinearGradient(Random * Width, Random * Height, Random * Width,
          Random * Height, Colors[0], Colors[1]);
      1:
        LineRadialGradient(Bounds.CenterX, Bounds.CenterY, 0.5 *
          (Bounds.CenterX + Bounds.CenterY), Colors[0], Colors[1]);
      2:
        LineColor := Colors[0];
      3:
        NoLine;
    end;

    case Random(9) of
      0:
        Line(Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2);
      1:
        Triangle(Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2, Random * Width,
          Random * Height);
      2:
        Rectangle(Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2);
      3:
        RoundedRect(Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2, 16 +
          16 * Random);
      4:
        Circle(Bounds.CenterX, Bounds.CenterY, 0.5 * ((Bounds.CenterX -
          Bounds.X1) + (Bounds.CenterY - Bounds.Y1)));
      5:
        Ellipse(Bounds.CenterX, Bounds.CenterY, Bounds.CenterX - Bounds.X1,
          Bounds.CenterY - Bounds.Y1);
      6:
        Star(Bounds.CenterX, Bounds.CenterY, Bounds.CenterX - Bounds.X1,
          Bounds.CenterY - Bounds.Y1, 2 * Pi * Random, 3 + Random(9));
      7..8:
        begin
          Font('Arial', 12 + Random(60), Boolean(Random(2)), False, fcVector,
            2 * Pi * Random);
          Text(Bounds.CenterX, Bounds.CenterY, AnsiChar(33 + Random(60)));
        end;
    end;

  end;
  Agg2DControl.Invalidate;
end;

end.

