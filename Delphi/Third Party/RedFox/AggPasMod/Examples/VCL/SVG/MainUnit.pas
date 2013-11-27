unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AggControlVCL;

type
  TFmSvgViewer = class(TForm)
    AggSVG: TAggSVG;
    OpenDialog: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure AggSVGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AggSVGMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FAngleOffset: Double;
    FScaleOffset: Double;
  end;

var
  FmSvgViewer: TFmSvgViewer;

implementation

uses
  Math, AggBasics;

{$R *.dfm}

procedure TFmSvgViewer.AggSVGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Center, Pnt: TPointDouble;
begin
  Center := PointDouble(0.5 * AggSVG.Width, 0.5 * AggSVG.Height);
  Pnt := PointDouble(X - Center.X, Y - Center.Y);
  FAngleOffset := ArcTan2(Pnt.Y, Pnt.X) - AggSVG.Angle;
  FScaleOffset := Hypot(Pnt.X / Center.X, Pnt.Y / Center.Y) / AggSVG.Scale;
end;

procedure TFmSvgViewer.AggSVGMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Center, Pnt: TPointDouble;
begin
  if ssLeft in Shift then
  begin
    Center := PointDouble(0.5 * AggSVG.Width, 0.5 * AggSVG.Height);
    Pnt := PointDouble(X - Center.X, Y - Center.Y);
    AggSVG.Scale := Hypot(Pnt.X / Center.X, Pnt.Y / Center.Y) / FScaleOffset;
    AggSVG.Angle := ArcTan2(Pnt.Y, Pnt.X) - FAngleOffset;
    AggSVG.Invalidate;
  end;
end;

procedure TFmSvgViewer.FormShow(Sender: TObject);
var
  FileName: TFileName;
begin
  FileName := ExpandFileName(ParamStr(1));
  if not FileExists(FileName) then
    if OpenDialog.Execute then
      FileName := OpenDialog.FileName;

  if FileExists(FileName) then
  begin
    AggSVG.LoadFromFile(FileName);
    ClientWidth := Round(Max(AggSVG.Bounds.X1, AggSVG.Bounds.X2));
    ClientHeight := Round(Max(AggSVG.Bounds.Y1, AggSVG.Bounds.Y2));
  end;
end;

end.

