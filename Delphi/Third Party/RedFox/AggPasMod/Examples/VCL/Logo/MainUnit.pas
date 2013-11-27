unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, AggBasics, AggColor, Agg2D, Agg2DControl;

type
  TFmAgg2D = class(TForm)
    Agg2DControl: TAgg2DControl;
    procedure Agg2DControlPaint(Sender: TObject);
    procedure Agg2DControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Agg2DControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FAngle, FAngleOffset: Double;
    FScale, FScaleOffset: Double;
  protected
  public
  end;

var
  FmAgg2D: TFmAgg2D;

implementation

uses
  Math;

{$R *.dfm}

procedure TFmAgg2D.FormCreate(Sender: TObject);
begin
  FScale := 1;
  FAngle := 0;
end;

procedure TFmAgg2D.Agg2DControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Center, Pnt: TPointDouble;
begin
  Center := PointDouble(0.5 * Agg2DControl.Width, 0.5 * Agg2DControl.Height);
  Pnt := PointDouble(X - Center.X, Y - Center.Y);
  FAngleOffset := ArcTan2(Pnt.Y, Pnt.X) - FAngle;
  FScaleOffset := Hypot(Pnt.X / Center.X, Pnt.Y / Center.Y) / FScale;
end;

procedure TFmAgg2D.Agg2DControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Center, Pnt: TPointDouble;
begin
  if ssLeft in Shift then
  begin
    Center := PointDouble(0.5 * Agg2DControl.Width, 0.5 * Agg2DControl.Height);
    Pnt := PointDouble(X - Center.X, Y - Center.Y);
    FScale := Hypot(Pnt.X / Center.X, Pnt.Y / Center.Y) / FScaleOffset;
    FAngle := ArcTan2(Pnt.Y, Pnt.X) - FAngleOffset;
    Agg2DControl.Invalidate;
  end;
end;

procedure TFmAgg2D.Agg2DControlPaint(Sender: TObject);
var
  AggColor: TAggColorRgba8;
begin
  with Agg2DControl, Agg2D do
  begin
    // setup Agg2D control
    ResetTransformations;
    Viewport(0, 0, 174, 174, 0, 0, ClientWidth, ClientHeight);
    ClearAll(CRgba8White);

    Translate(-0.5 * ClientWidth, -0.5 * ClientHeight);
    Rotate(FAngle);
    Scale(FScale, FScale);
    Translate(0.5 * ClientWidth, 0.5 * ClientHeight);

    // Circle
    NoLine;
    AggColor.ABGR := $FF221BE2;
    FillColor := AggColor;
    Circle(87, 87, 87);

    // Half Circle
    AggColor.ABGR := $FF526EEB;
    FillColor := AggColor;
    ResetPath;
    MoveTo(17.34375, 89.375);
    ArcTo(5.3368431, 5.3368431, 0, False, False, 12.03125, 94.71875);
    CubicCurveTo(12.03125, 132.82644, 45.917638, 163.28125, 87, 163.28125);
    CubicCurveTo(128.08238, 163.28125, 161.96875, 132.82644, 161.96875,
      94.71875);
    ArcTo(5.3368431, 5.3368431, 0, False, False, 156.65625, 89.375);
    LineTo(146.4375, 89.375);
    LineTo(136.25, 89.375);
    LineTo(38.21875, 89.375);
    LineTo(27.78125, 89.375);
    LineTo(17.34375, 89.375);
    DrawPath(dpfFillOnly);

    // Spike 1
    FillColor := CRgba8White;
    ResetPath;
    MoveTo(116.21875, 15.65625);
    LineTo(91.858265, 50.912099);
    LineTo(107.46875, 57.03125);
    LineTo(143.59375, 26.21875);

    // Spike 2
    MoveTo(83.8125, 17.25);
    LineTo(72.640625, 53.09375);
    LineTo(89.78125, 50.4375);
    LineTo(109.60613, 17.253509);

    // Spike 3
    MoveTo(76.9375, 21.6875);
    LineTo(54.8125, 30.28125);
    LineTo(57.617188, 62.710937);
    LineTo(71.273438, 53.492187);

    // Spike 4
    MoveTo(50.5, 35.78125);
    LineTo(34.75, 50.6875);
    LineTo(48.705941, 77.449603);
    LineTo(56.539063, 63.875);

    // Spike 5
    MoveTo(33.094512, 56.30183);
    LineTo(27.28125, 74.21875);
    LineTo(46.356044, 90.295439);
    LineTo(48.831075, 79.326634);

    // Spike 6
    MoveTo(28.28125, 78.40625);
    LineTo(27.71875, 94.1875);
    LineTo(48.409391, 102.61742);
    LineTo(46.3125, 91.65625);

    // Spike 7
    MoveTo(28.25, 96.625);
    LineTo(32.71875, 110.96875);
    LineTo(51.892818, 111.03275);
    LineTo(49.243337, 103.56573);

    // Spike 8
    MoveTo(52.316561, 111.44508);
    LineTo(34.3125, 113.1562);
    LineTo(42.024318,123.57415);
    LineTo(56.931488,117.22633);
    DrawPath(dpfFillOnly);

    // Background Circle
    AggColor.ABGR := $FF526EEB;
    FillColor := AggColor;
    ResetPath;
    MoveTo(127.89327, 90.999997);
    ArcTo(40.893275, 40.893275, 0, False, True, 46.106725, 90.999997);
    ArcTo(40.893275, 40.893275, 0, True, True, 127.89327, 90.999997);
    DrawPath(dpfFillOnly);

    // Helmet Circle
    FillColor := CRgba8White;
    ResetPath;
    MoveTo(87.75, 55.4375);
    ArcTo(35.571, 35.571007, 0, False, False, 65.125, 119.03125);
    LineTo(67.59375, 121.25);
    LineTo(86.5, 118.75);
    LineTo(80.90625, 103.59375);
    LineTo(115.5, 69.71875);
    ArcTo(35.571, 35.571007, 0, False, False, 87.75, 55.4375);
    DrawPath(dpfFillOnly);

    // Helmet Part 0
    AggColor.ABGR := $FFA1B5F3;
    FillColor := AggColor;
    ResetPath;
    MoveTo(124.78125, 58.4375);
    LineTo(72.25, 94.875);
    LineTo(75.84375, 107.625);
    CubicCurveTo(94.606902, 103.41847, 132.125, 94.96875, 132.125, 94.96875);
    CubicCurveTo(129.67728, 82.79162, 124.78125, 58.4375, 124.78125, 58.4375);
    DrawPath(dpfFillOnly);

    // Helmet Part 1
    FillColor := CRgba8White;
    ResetPath;
    MoveTo(121.375, 66.875);
    LineTo(126.28125, 91.15625);
    LineTo(79.375, 101.71875);
    LineTo(78.03125, 96.9375);
    LineTo(121.375, 66.875);
    DrawPath(dpfFillOnly);

    // Helmet Part 2
    ResetPath;
    MoveTo(87.28125, 107.3125);
    LineTo(88.03125, 108.96875);
    CubicCurveTo(88.03125, 108.96875, 91.693696, 117.3024, 95.90625,
      126.46875);
    CubicCurveTo(100.1188, 135.6351, 104.82694, 145.59083, 106.96875,
      149.03125);
    CubicCurveTo(107.17503, 149.36261, 107.59478, 149.50994, 107.9375,
      149.5);
    CubicCurveTo(108.28022, 149.49, 108.62919, 149.38065, 109,
      149.21875);
    CubicCurveTo(109.74162, 148.89494, 110.5715, 148.32996, 111.4375,
      147.625);
    CubicCurveTo(113.16949, 146.21508, 114.95287, 144.27985, 115.5625,
      142.5);
    CubicCurveTo(116.13222, 140.83669, 115.539, 138.40147, 114.4375,
      135.65625);
    CubicCurveTo(113.33596, 132.91103, 111.6635, 129.87631, 109.78125,
      127.21875);
    CubicCurveTo(106.85741, 123.09056, 101.61579, 118.4075, 97.09375,
      114.71875);
    CubicCurveTo(92.57171, 111.03, 88.75, 108.34375, 88.75, 108.34375);
    LineTo(87.28125, 107.3125);
    DrawPath(dpfFillOnly);

    // Helmet Part 3
    AggColor.ABGR := $FF526EEB;
    FillColor := AggColor;
    ResetPath;
    MoveTo(79.679222, 60.619469);
    CubicCurveTo(72.896972, 62.051929, 66.561832, 65.787739, 61.960472,
      71.681969);
    CubicCurveTo(52.761752, 83.465299, 53.568662, 99.889949, 63.147972,
      110.68197);
    LineTo(69.241722, 109.30697);
    CubicCurveTo(58.486892, 98.216539, 55.890172, 82.365299, 66.241722,
      70.463219);
    CubicCurveTo(70.026722, 66.111279, 74.778642, 63.085619, 79.897972,
      61.431969);
    LineTo(79.679222, 60.619469);
    DrawPath(dpfFillOnly);

    // Helmet Part 4
    AggColor.ABGR := $FF241dE4;
    FillColor := AggColor;
    ResetPath;
    MoveTo(85.853952,116.52758);
    CubicCurveTo(86.786482, 116.7176, 92.818232, 131.30436, 92.818232,
      131.30436);
    CubicCurveTo(92.818232, 131.30436, 88.928002, 137.98183, 88.621802,
      137.86686);
    CubicCurveTo(75.194472, 132.82537, 67.369672, 132.59698, 52.996802,
      137.19722);
    CubicCurveTo(50.899162, 137.8686, 59.435642, 121.75976, 61.345022,
      118.98293);
    CubicCurveTo(64.539192, 114.33764, 79.075592, 115.14633, 85.853952,
      116.52758);
    DrawPath(dpfFillOnly);

    // Helmet Part 5
    AggColor.ABGR := $FF5271EB;
    FillColor := AggColor;
    ResetPath;
    MoveTo(72.429222, 117.24447);
    CubicCurveTo(69.191292, 117.38102, 66.238762, 118.01608, 65.179222,
      119.55697);
    CubicCurveTo(64.501612, 120.54242, 62.588842, 124.03634, 61.147972,
      127.02572);
    CubicCurveTo(63.045912, 124.35898, 65.323382, 121.63536, 66.554222,
      121.02572);
    CubicCurveTo(68.149632, 120.23551, 68.786982, 118.46824, 82.210472,
      119.74447);
    CubicCurveTo(82.320322, 119.75487, 82.463222, 119.81967, 82.585472,
      119.90072);
    CubicCurveTo(82.051152, 118.80698, 81.600772, 117.96692, 81.429222,
      117.93197);
    CubicCurveTo(79.180772, 117.4738, 75.667152, 117.10792, 72.429222,
      117.24447);
    DrawPath(dpfFillOnly);
  end;
end;

end.

