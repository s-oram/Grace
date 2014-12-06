unit Menu.AutoSelectMenu;

interface

uses
  Classes,
  Menu.CustomPopupMenu;

type
  TAutoSelectMenu = class(TCustomPopupMenu)
  private
    procedure HandleEvent_AutoOnOff(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  uConstants,
  SysUtils,
  Vcl.Menus;

{ TAutoSelectMenu }

constructor TAutoSelectMenu.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TAutoSelectMenu.Destroy;
begin

  inherited;
end;

procedure TAutoSelectMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Auto Select On';
  mi.OnClick := HandleEvent_AutoOnOff;
  mi.Tag := 1;
  if Plugin.Globals.GuiState.IsAutoSelectActive
    then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Auto Select Off';
  mi.OnClick := HandleEvent_AutoOnOff;
  mi.Tag := 2;
  if not Plugin.Globals.GuiState.IsAutoSelectActive
    then mi.Checked := true;
  Menu.Items.Add(mi);

  Menu.Popup(x, y);
end;

procedure TAutoSelectMenu.HandleEvent_AutoOnOff(Sender: TObject);
var
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;
  case Tag of
    1: Plugin.Globals.GuiState.IsAutoSelectActive := true;
    2: Plugin.Globals.GuiState.IsAutoSelectActive := false;
  else
    raise Exception.Create('Unexpected tag value.');
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Cmd_RefreshParDisplay);

  // IMPORTANT - check auto free status.
  if (AutoFreeMenu) then self.Free;
end;



end.
