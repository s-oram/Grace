unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  FMX.Canvas.AggPas,
  FMX.Layouts;

type
  TFmDelphiLogo = class(TForm)
    PathCircle: TPath;
    PathHalfCircle: TPath;
    PathBackgroundCircle: TPath;
    PathHelmetCircle: TPath;
    PathHelmet1: TPath;
    PathHelmet2: TPath;
    PathHelmet3: TPath;
    PathHelmet4: TPath;
    PathHelmet5: TPath;
    PathHelmet6: TPath;
    LayoutHelmet: TLayout;
    PathSpikes: TPath;
    LayoutDelphiLogo: TLayout;
    LayoutFrame: TLayout;
    procedure LayoutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure LayoutScalePaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure LayoutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
  private
    FAngle, FAngleOffset: Double;
    FScale, FScaleOffset: Double;
  end;

var
  FmDelphiLogo: TFmDelphiLogo;

implementation

uses
  Math;

{$R *.fmx}

procedure TFmDelphiLogo.FormCreate(Sender: TObject);
begin
  FScale := 1;
  FAngle := 0;
end;

procedure TFmDelphiLogo.LayoutMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Center, Pnt: TPointF;
begin
  with LayoutDelphiLogo do
  begin
    Center := PointF(0.5 * ClientWidth, 0.5 * ClientHeight);
    Pnt := PointF(X - Center.X, Y - Center.Y);
    FScaleOffset := Hypot(Pnt.X / Center.X, Pnt.Y / Center.Y) / FScale;
    FAngleOffset := ArcTan2(Pnt.Y, Pnt.X) - FAngle;
  end;
end;

procedure TFmDelphiLogo.LayoutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  Center, Pnt: TPointF;
begin
  if ssLeft in Shift then
    with LayoutDelphiLogo do
    begin
      Center := PointF(0.5 * ClientWidth, 0.5 * ClientHeight);
      Pnt := PointF(X - Center.X, Y - Center.Y);
      FScale := Hypot(Pnt.X / Center.X, Pnt.Y / Center.Y) / FScaleOffset;
      FAngle := ArcTan2(Pnt.Y, Pnt.X) - FAngleOffset;

      BeginUpdate;
      RotationAngle := RadToDeg(FAngle);
      Scale.X := FScale;
      Scale.Y := FScale;
      Position.X := Center.X - FScale * Center.X;
      Position.Y := Center.Y - FScale * Center.Y;
      EndUpdate;
    end;
end;

procedure TFmDelphiLogo.LayoutScalePaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  Polygon: TPolygon;
begin
(*
//  Canvas.ClearRect(RectF(12, 12, 59, 59), TAlphaColorRec.Aqua);

  Fill.Kind := TBrushKind.bkGradient;
  Canvas.FillEllipse(RectF(12, 12, 59, 99), 1);

  SetLength(Polygon, 5);
  Polygon[0].X := 50;
  Polygon[0].Y := 50;
  Polygon[1].X := 100;
  Polygon[1].Y := 50;
  Polygon[2].X := 100;
  Polygon[2].Y := 100;
  Polygon[3].X := 50;
  Polygon[3].Y := 100;
  Polygon[4].X := 150;
  Polygon[4].Y := 75;
  Canvas.DrawPolygon(Polygon, 1);
//  Canvas.Clear(TAlphaColorRec.Aqua);
*)
end;

end.

