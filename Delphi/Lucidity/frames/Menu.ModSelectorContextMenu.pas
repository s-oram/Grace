unit Menu.ModSelectorContextMenu;

interface

uses
  eeEnumMenu, uLucidityEnums,
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;

type
  TModSelectorContextMenu = class(TCustomPopupMenu)
  private
  protected
    Menu : TPopUpMenu;
    ModSourceMenu : TEnumMenu<TModSource>;
    ModViaMenu    : TEnumMenu<TModSource>;
    ModSlotIndex  : integer;

    procedure Handle_ModSourceSelected(Sender : TObject; aSource : TModSource);
    procedure Handle_ModViaSelected(Sender : TObject; aSource : TModSource);

    procedure Handle_ToggleModulationMute(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const aModSlotIndex : integer; const x, y : integer);
  end;

implementation

uses
  uConstants;

{ TModSelectorContextMenu }

constructor TModSelectorContextMenu.Create;
var
  mi : TMenuItem;
begin
  ModSourceMenu := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModSourceMenu.Items.Caption := 'Mod Source';
  ModSourceMenu.OnItemSelected := Handle_ModSourceSelected;

  ModViaMenu    := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModViaMenu.Items.Caption := 'Mod Via';
  ModViaMenu.OnItemSelected := Handle_ModViaSelected;


  Menu := TPopUpMenu.Create(nil);
  Menu.Items.Add(ModSourceMenu.Items);
  Menu.Items.Add(ModViaMenu.Items);


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Mute Modulation';
  mi.OnClick := self.Handle_ToggleModulationMute;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Un-mute Modulation';
  mi.OnClick := self.Handle_ToggleModulationMute;
  Menu.Items.Add(mi);

end;

destructor TModSelectorContextMenu.Destroy;
begin
  ModSourceMenu.Free;
  ModViaMenu.Free;
  Menu.Free;
  inherited;
end;

procedure TModSelectorContextMenu.Popup(const aModSlotIndex : integer; const x, y: integer);
var
  IsMute : boolean;
  s : string;
  mi : TMenuItem;
  c1: Integer;
  ModSource : TModSource;
  ModVia    : TModSource;
begin
  ModSlotIndex := aModSlotIndex;


  ModSource := Plugin.ActiveKeyGroup.GetModConnections.GetModSource(ModSlotIndex);
  ModVia    := Plugin.ActiveKeyGroup.GetModConnections.GetModVia(ModSlotIndex);

  for c1 := 0 to ModSourceMenu.Items.Count-1
    do ModSourceMenu.Items[c1].Checked := false;

  for c1 := 0 to ModViaMenu.Items.Count-1
    do ModViaMenu.Items[c1].Checked := false;

  mi :=  ModSourceMenu.FindMenuItemByEnum(ModSource);
  if assigned(mi) then
  begin
    mi.Checked := true;
  end;

  mi :=  ModViaMenu.FindMenuItemByEnum(ModVia);
  if assigned(mi) then
  begin
    mi.Checked := true;
  end;




  IsMute := Plugin.ActiveKeyGroup.GetModConnections.GetModMute(ModSlotIndex);
  if IsMute then
  begin
    mi := Menu.Items.Find('Mute Modulation');
    if assigned(mi)
      then mi.Visible := false;

    mi := Menu.Items.Find('Un-mute Modulation');
    if assigned(mi)
      then mi.Visible := true;
  end else
  begin
    mi := Menu.Items.Find('Mute Modulation');
    if assigned(mi)
      then mi.Visible := true;

    mi := Menu.Items.Find('Un-mute Modulation');
    if assigned(mi)
      then mi.Visible := false;
  end;






  Menu.Popup(x, y);


end;

procedure TModSelectorContextMenu.Handle_ModSourceSelected(Sender: TObject; aSource: TModSource);
begin
  Plugin.ActiveKeyGroup.GetModConnections.SetModSource(ModSlotIndex, aSource);
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;

procedure TModSelectorContextMenu.Handle_ModViaSelected(Sender: TObject; aSource: TModSource);
begin
  Plugin.ActiveKeyGroup.GetModConnections.SetModVia(ModSlotIndex, aSource);
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;

procedure TModSelectorContextMenu.Handle_ToggleModulationMute(Sender: TObject);
var
  IsMute : boolean;
begin
  IsMute := Plugin.ActiveKeyGroup.GetModConnections.GetModMute(ModSlotIndex);
  IsMute := not IsMute;
  Plugin.ActiveKeyGroup.GetModConnections.SetModMute(ModSlotIndex, IsMute);
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;

end.
