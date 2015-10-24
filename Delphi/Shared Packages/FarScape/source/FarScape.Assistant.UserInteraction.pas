unit FarScape.Assistant.UserInteraction;

interface

uses
  FarScape.Scene;

type
  UserInteractionAssistant = record
  public
    // GetInteractionTargetAt() looks for controls that are visible and pass the hit test.
    class function GetInteractionTargetAt(const Scene : TScene; const X, Y : integer) : TSceneElement; static;
  end;

implementation

uses
  Types,
  FarScape.CustomControl;

{ UserInteractionAssistant }

class function UserInteractionAssistant.GetInteractionTargetAt(const Scene: TScene; const X, Y: integer): TSceneElement;
var
  c1: Integer;
  c : TFarScapeControl;
  cx, cy : integer;
begin
  for c1 := Scene.ElementCount-1 downto 0 do
  begin
    if (Scene.Element[c1].IsShowing) and ( Scene.Element[c1].AbsoluteBoundsRect.Contains(Point(X,Y)) ) then
    begin
      c := Scene.Element[c1].Control;

      if (c.HitTest = htAlways) then exit(Scene.Element[c1]);

      if (c.HitTest = htPartial) then
      begin
        cx := X - c.Left;
        cy := Y - c.Top;
        if c.IsHit(cx, cy) then exit(Scene.Element[c1]);
      end;
    end;
  end;

  // If we make it this far, no matching scene element has been found.
  result := nil;
end;

end.
