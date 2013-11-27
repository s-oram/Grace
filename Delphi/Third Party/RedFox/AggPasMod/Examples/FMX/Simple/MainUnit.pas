unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  FMX.Canvas.AggPas,
//  FMX.Canvas.D2D,
//  FMX.Canvas.GDIP,
//  FMX.Canvas.VPR,
  FMX.Layouts, FMX.Colors, FMX.Effects, FMX.ExtCtrls;

type
  TFmDelphiLogo = class(TForm)
    Rectangle1: TRectangle;
    Ellipse1: TEllipse;
    Text1: TText;
    CalloutRectangle1: TCalloutRectangle;
    Image1: TImage;
    Text3: TText;
    Line1: TLine;
    Line2: TLine;
    Line3: TLine;
    Text4: TText;
    Text2: TText;
    Text5: TText;
    Line4: TLine;
    Text6: TText;
    Text7: TText;
    Text8: TText;
    Line5: TLine;
    RoundRect: TRoundRect;
    Smiley: TPie;
    DialRotation: TArcDial;
    Text9: TText;
    Text10: TText;
    DialGradient: TArcDial;
    StyleBook1: TStyleBook;
    TtStartY: TText;
    TtStartX: TText;
    GradRect: TRectangle;
    TtStopX: TText;
    TtStopY: TText;
    GradLine: TLine;
    TtAngle: TText;
    AlphaTrackBar1: TAlphaTrackBar;
    Rectangle2: TRectangle;
    GlowEffect1: TGlowEffect;
    InnerGlowEffect1: TInnerGlowEffect;
    Button5: TButton;
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure DialRotationChange(Sender: TObject);
    procedure DialGradientChange(Sender: TObject);
    procedure Rectangle2Click(Sender: TObject);
  private
    FLastMousePos: TPointF;
  end;

var
  FmDelphiLogo: TFmDelphiLogo;

implementation

{$R *.fmx}

procedure TFmDelphiLogo.DialRotationChange(Sender: TObject);
begin
  RoundRect.RotationAngle := 360 - DialRotation.Value;
  Smiley.RotationAngle := 360 - DialRotation.Value;
end;

procedure TFmDelphiLogo.DialGradientChange(Sender: TObject);
var
  ARect: TRectF;
  CRect: TRectF;
  Temp: Double;
  Pos: Single;
begin
  Pos := 2 * Pi * -DialGradient.Value / 360;
  with RoundRect.Fill.Gradient do
  begin
    StartPosition.X := 0.5 * (1 + EnsureRange(Sqrt(2) * Cos(Pos), -1, 1));
    StartPosition.Y := 0.5 * (1 + EnsureRange(Sqrt(2) * Sin(Pos), -1, 1));
    StopPosition.X := 0.5 * (1 + EnsureRange(Sqrt(2) * -Cos(Pos), -1, 1));
    StopPosition.Y := 0.5 * (1 + EnsureRange(Sqrt(2) * -Sin(Pos), -1, 1));

    TtStartX.Text := FloatToStrF(StartPosition.X, ffGeneral, 3, 3);
    TtStartY.Text := FloatToStrF(StartPosition.Y, ffGeneral, 3, 3);
    TtStopX.Text := FloatToStrF(StopPosition.X, ffGeneral, 3, 3);
    TtStopY.Text := FloatToStrF(StopPosition.Y, ffGeneral, 3, 3);

(*
    GradLine.Position.X := {GradRect.Width *} StartPosition.X;
    GradLine.Position.Y := {GradRect.Height *} (1 - StartPosition.Y);
    GradLine.Width := {GradRect.Width *} StopPosition.X;
    GradLine.Height := {GradRect.Height *} (1 - StopPosition.Y);
*)
    TtAngle.Text := FloatToStrF(ArcTan2(StopPosition.Point.Y -
      StartPosition.Point.Y, StopPosition.Point.X -
      StartPosition.Point.X) * 180 / Pi, ffGeneral, 4, 4);
  end;
  GradRect.InvalidateRect(GradRect.ClipRect);
end;

procedure TFmDelphiLogo.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  Polygon: TPolygon;
begin
//  Canvas.ClearRect(RectF(12, 12, 59, 59), TAlphaColorRec.Aqua);
(*
  Fill.Kind := TBrushKind.bkGradient;
  Canvas.FillEllipse(RectF(12, 12, 59, 99), 1);
*)

  Canvas.Stroke.Color := TAlphaColorRec.Azure;
  Canvas.DrawLine(PointF(20, 160), PointF(160, 40), 1);

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

  with RoundRect.Fill.Gradient do
  begin
    Canvas.Stroke.Color := TAlphaColorRec.Black;
    Canvas.DrawLine(PointF(GradRect.Position.X + GradRect.Width * StartPosition.X,
      GradRect.Position.Y + GradRect.Height * (1 - StartPosition.Y)),
      PointF(GradRect.Position.X + GradRect.Width * StopPosition.X,
        GradRect.Position.Y + GradRect.Height * (1 - StopPosition.Y)), 1);
  end;


//  Canvas.FillPolygon(Polygon, 1);
//  Canvas.Clear(TAlphaColorRec.Aqua);
end;

procedure TFmDelphiLogo.Rectangle2Click(Sender: TObject);
begin
  InnerGlowEffect1.Enabled := not InnerGlowEffect1.Enabled;
  GlowEffect1.Enabled := not GlowEffect1.Enabled;
end;

end.

