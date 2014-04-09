unit VamMiniLevelMeter;

interface

uses
  VamGuiControlInterfaces,
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamMiniLevelMeter = class(TVamWinControl)
  private
    fLevelMonitor: ILevelMonitor;

  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LevelMonitor : ILevelMonitor read fLevelMonitor write fLevelMonitor;
  end;

implementation

uses
  Agg2D,
  RedFoxImageBuffer;



{ TVamMiniLevelMeter }

constructor TVamMiniLevelMeter.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TVamMiniLevelMeter.Destroy;
begin

  inherited;
end;

procedure TVamMiniLevelMeter.Paint;
type
  TDrawMeterMethod = reference to procedure(const x1, y1, Width, Height : integer; const DbLevel : single);
var
  dbA, dbB : single;
  x1, y1, x2, y2 : single;
  DrawMeter : TDrawMeterMethod;
begin
  inherited;

  if assigned(fLevelMonitor) then
  begin
    fLevelMonitor.GetDbLevel(dbA, dbB);
  end else
  begin
    dbA := -120;
    dbB := -120;
  end;

  DrawMeter := procedure(const x1, y1, Width, Height : integer; const DbLevel : single)
  const
    PeakIndicatorHeight = 4;
    MinDb = -72;
  var
    dx1, dx2, dy1, dy2 : single;
    c1, c2 : TAggColorRgba8;
    MeterThrowDist : integer;
    LevelFactor : single;
  begin
    BackBuffer.BufferInterface.FillColor := GetRedFoxColor('$FF333333');
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.Rectangle(x1, y1, x1 + Width, y1 + Height);

    dx1 := x1;
    dx2 := x1 + Width;

    // draw peak indicator if required
    if DbLevel >= 0 then
    begin
      BackBuffer.BufferInterface.FillColor := GetRedFoxColor(clRed);
      dy1 := y1;
      dy2 := y1 + PeakIndicatorHeight;

      BackBuffer.BufferInterface.Rectangle(dx1, dy1, dx2, dy2);
    end;
    //===================================

    c1 := GetRedFoxColor(clYellow);
    c2 := GetRedFoxColor('$FF33ff33');

    if DbLevel > 0 then
    begin
      LevelFactor := 1;
    end else
    if DbLevel < MinDb then
    begin
      LevelFactor := 0;
    end else
    begin
      LevelFactor := (dbLevel - MinDb)/72;
      LevelFactor := LevelFactor * LevelFactor;
    end;

    MeterThrowDist := (Height-PeakIndicatorHeight);

    dy2 := y1 + Height;
    dy1 := dy2 - MeterThrowDist;
    BackBuffer.BufferInterface.FillLinearGradient(dx1, dy1, dx2, dy2, c1, c2);

    dy2 := y1 + Height;
    dy1 := dy2 - round(MeterThrowDist * LevelFactor);
    BackBuffer.BufferInterface.Rectangle(dx1, dy1, dx2, dy2);
  end;


  DrawMeter(0,0, 3, Height, dbA);
  DrawMeter(5,0, 3, Height, dbB);
end;

end.
