unit VamArrows;

interface

uses
  Classes, RedFox, RedFoxColor, RedFoxGraphicControl, VamGraphicControl;

type
  TVamArrows = class(TVamGraphicControl)
  private
    procedure SetColor(const Value: TRedFoxColorString);
  protected
    fColor : TRedFoxColorString;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;



    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Color : TRedFoxColorString read fColor write SetColor;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  AggBasics;

{ TVamArrows }

constructor TVamArrows.Create(AOwner: TComponent);
begin
  inherited;

  Color := '$FF000000';
end;

destructor TVamArrows.Destroy;
begin

  inherited;
end;

procedure TVamArrows.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth > 0) and (AHeight > 0) then
  begin
    Invalidate;
  end;

end;

procedure TVamArrows.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor then
  begin
    fColor := Value;
    Invalidate;
  end;
end;


procedure TVamArrows.Paint;
var
  Points : array[0..2] of TPointDouble;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(Color);


  Points[0].X := Width * 0.5;
  Points[0].Y := Height * (1/16);

  Points[1].X := Width;
  Points[1].Y := Height * (7/16);

  Points[2].X := 0;
  Points[2].Y := Height * (7/16);

  BackBuffer.BufferInterface.Polygon(@Points[0], 3);



  Points[0].X := Width * 0.5;
  Points[0].Y := Height * (15/16);

  Points[1].X := Width;
  Points[1].Y := Height * (9/16);

  Points[2].X := 0;
  Points[2].Y := Height * (9/16);

  BackBuffer.BufferInterface.Polygon(@Points[0], 3);


end;



end.
