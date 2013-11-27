unit eeGuiHelpers;

interface

uses
  Menus;

type
  TMenuHelper = class(TMenu)
  private
  public

    function FindItemByName(Name : string):TMenuItem;
  end;


function MenuHelper(aMenu : TMenu):TMenuHelper;

implementation

function MenuHelper(aMenu : TMenu):TMenuHelper;
begin
  result := TMenuHelper(aMenu);
end;

{ TMenuHelper }

function TMenuHelper.FindItemByName(Name: string): TMenuItem;
var
  c1: Integer;
begin
  for c1 := 0 to Items.Count-1 do
  begin
    if Items[c1].Name = Name then
    begin
      exit(Items[c1]);
    end;
  end;


  // no matching item has been found, return nil.
  result := nil;
end;

end.
