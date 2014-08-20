unit Menu.GroupVisibility;

interface

uses
  Menu.CustomPopupMenu;

type
  TGroupVisibilityMenu = class(TCustomPopupMenu)
  private
    procedure EventHandle_GroupVisibilityItemSelected(Sender : TObject);
  public
    procedure Popup(const x, y : integer);
  end;

implementation

uses
  uConstants,
  uLucidityEnums,
  SysUtils,
  Menus;

{ TGroupVisibilityMenu }

procedure TGroupVisibilityMenu.EventHandle_GroupVisibilityItemSelected(Sender: TObject);
var
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;
  case Tag of
  1: Plugin.Globals.GuiState.SampleMapGroupVisibility := TGroupVisibility.AllGroups;
  2: Plugin.Globals.GuiState.SampleMapGroupVisibility := TGroupVisibility.SelectedGroup;
  else
    raise Exception.Create('Type not handled.');
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GroupVisibilityChanged);
end;

procedure TGroupVisibilityMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'All Groups Visible';
  mi.OnClick := EventHandle_GroupVisibilityItemSelected;
  mi.Tag := 1;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Current Group Visible';
  mi.OnClick := EventHandle_GroupVisibilityItemSelected;
  mi.Tag := 2;
  Menu.Items.Add(mi);

  Menu.Popup(x, y);
end;

end.
