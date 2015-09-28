unit FarScape.Assistant.UserInteraction;

interface

uses
  FarScape.Scene;

type
  UserInteractionAssistant = record
  public
    class function GetInteractionTargetAt(const Scene : TScene; const X, Y : integer) : TSceneElement; static;
  end;

implementation

uses
  Types;

{ UserInteractionAssistant }

class function UserInteractionAssistant.GetInteractionTargetAt(const Scene: TScene; const X, Y: integer): TSceneElement;
var
  c1: Integer;
begin
  for c1 := Scene.ElementCount-1 downto 0 do
  begin
    if (Scene.Element[c1].IsShowing) and ( Scene.Element[c1].AbsoluteBoundsRect.Contains(Point(X,Y)) ) then exit(Scene.Element[c1]);
  end;

  // If we make it this far, no matching scene element has been found.
  result := nil;
end;

end.
