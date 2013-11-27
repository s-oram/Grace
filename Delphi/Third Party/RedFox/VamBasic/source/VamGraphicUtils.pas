unit VamGraphicUtils;

interface

type
  TXYBoundsF = record
    x1 : single;
    y1 : single;
    x2 : single;
    y2 : single;

    function Width  : single;
    function Height : single;
  end;


// Translates a XY cooridinate position from one sized plane to another.
procedure TranslateCoordinate(const SourceBounds, DestBounds : TXYBoundsF; const SourceX, SourceY : single; out DestX, DestY:single);





implementation

procedure TranslateCoordinate(const SourceBounds, DestBounds : TXYBoundsF; const SourceX, SourceY : single; out DestX, DestY:single);
var
  fx, fy : single;

begin
  fx := (SourceX - SourceBounds.x1) / (SourceBounds.x2 - SourceBounds.x1);
  fy := (SourceY - SourceBounds.y1) / (SourceBounds.y2 - SourceBounds.y1);

  DestX := (DestBounds.x2 - DestBounds.x1) * fx + DestBounds.x1;
  DestY := (DestBounds.y2 - DestBounds.y1) * fy + DestBounds.y1;
end;

{ TXYBoundsF }

function TXYBoundsF.Height: single;
begin
  if y2 > y1
    then result := y2 - y1
    else result := y1 - y2;
end;

function TXYBoundsF.Width: single;
begin
  if x2 > x1
    then result := x2 - x1
    else result := x1 - x2;
end;

end.
