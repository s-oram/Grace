unit AudioPlugin.Functions;

interface

uses
  Types;

function CalcNewEditorSize(const InitialRatio : double; const CurrentWidth, CurrentHeight, OffsetX, OffsetY : integer):TSize;

implementation

function CalcNewEditorSize(const InitialRatio : double; const CurrentWidth, CurrentHeight, OffsetX, OffsetY : integer):TSize;
var
  w1, h1 : integer;
  w2, h2 : integer;
begin
  w1 := CurrentWidth + OffsetX;
  h1 := round(w1 / InitialRatio);

  h2 := CurrentHeight + OffsetY;
  w2 := round(h2 * InitialRatio);

  if abs(w1 - CurrentWidth) < abs(w2 - CurrentWidth) then
  begin
    result.cx := w1;
    result.cy := h1;
  end else
  begin
    result.cx := w2;
    result.cy := h2;
  end;

end;

end.
