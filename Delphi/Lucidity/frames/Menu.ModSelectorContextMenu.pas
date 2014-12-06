unit Menu.ModSelectorContextMenu;

interface

uses
  Classes,
  eeEnumMenu, Lucidity.Enums,
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;

type
  TModSourceMenu = TEnumMenu<TModSource>;

  TModSelectorContextMenu = class(TCustomPopupMenu)
  private
  protected
    ModSourceMenu : TModSourceMenu;
    ModViaMenu    : TModSourceMenu;
    ModSlotIndex  : integer;

    procedure Handle_ModSourcePolaritySelected(Sender : TObject);
    procedure Handle_ModSourceSelected(Sender : TObject; aSource : TModSource);
    procedure Handle_ModViaSelected(Sender : TObject; aSource : TModSource);

    procedure Handle_ToggleModulationMute(Sender : TObject);

    procedure Init;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(const aModSlotIndex : integer; const x, y : integer);
  end;

implementation

uses
  SysUtils,
  Lucidity.Interfaces,
  uConstants;

{ TModSelectorContextMenu }

constructor TModSelectorContextMenu.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TModSelectorContextMenu.Destroy;
begin
  if assigned(ModSourceMenu) then FreeAndNil(ModSourceMenu);
  if assigned(ModViaMenu)    then FreeAndNil(ModViaMenu);
  inherited;
end;

procedure TModSelectorContextMenu.Init;
var
  mi : TMenuItem;
begin
  if assigned(ModSourceMenu) then FreeAndNil(ModSourceMenu);
  if assigned(ModViaMenu)    then FreeAndNil(ModViaMenu);

  ModSourceMenu := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModSourceMenu.Items.Caption := 'Mod Source';
  ModSourceMenu.OnItemSelected := Handle_ModSourceSelected;

  ModViaMenu    := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModViaMenu.Items.Caption := 'Mod Via';
  ModViaMenu.OnItemSelected := Handle_ModViaSelected;

  Menu.Items.Clear;
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


procedure TModSelectorContextMenu.Popup(const aModSlotIndex : integer; const x, y: integer);
var
  IsMute : boolean;
  mi : TMenuItem;
  c1: Integer;
  ModSource : TModSource;
  ModVia    : TModSource;

  kg : IKeyGroup;
begin
  kg := Plugin.ActiveKeyGroup;
  if not assigned(kg) then exit;


  Init; // important! initialise the menu

  ModSlotIndex := aModSlotIndex;

  ModSource := kg.GetModConnections.GetModSource(ModSlotIndex);
  ModVia    := kg.GetModConnections.GetModVia(ModSlotIndex);

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


  //======== Add extra Mod Source Menu Items =============
  mi := TMenuItem.Create(nil);
  mi.Caption := '-';
  ModSourceMenu.Items.Add(mi);

  mi := TMenuItem.Create(nil);
  mi.Caption := 'Unipolar';
  mi.Tag := 1;
  mi.OnClick := Handle_ModSourcePolaritySelected;
  if kg.GetModConnections.GetModSourcePolarity(ModSlotIndex) = TModSourcePolarity.Unipolar then mi.Checked := true;
  ModSourceMenu.Items.Add(mi);

  mi := TMenuItem.Create(nil);
  mi.Caption := 'Bipolar';
  mi.OnClick := Handle_ModSourcePolaritySelected;
  mi.Tag := 2;
  if kg.GetModConnections.GetModSourcePolarity(ModSlotIndex) = TModSourcePolarity.Bipolar then mi.Checked := true;
  ModSourceMenu.Items.Add(mi);
  //======================================================



  IsMute := kg.GetModConnections.GetModMute(ModSlotIndex);
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

procedure TModSelectorContextMenu.Handle_ModSourcePolaritySelected(Sender: TObject);
var
  kg : IKeyGroup;
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;

  kg := Plugin.ActiveKeyGroup;
  if assigned(kg) then
  begin
    case Tag of
      1: kg.GetModConnections.SetModSourcePolarity(ModSlotIndex, TModSourcePolarity.Unipolar);
      2: kg.GetModConnections.SetModSourcePolarity(ModSlotIndex, TModSourcePolarity.Bipolar);
    else
      raise Exception.Create('Unexpected tag value.');
    end;

    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;

procedure TModSelectorContextMenu.Handle_ModSourceSelected(Sender: TObject; aSource: TModSource);
var
  kg : IKeyGroup;
begin
  kg := Plugin.ActiveKeyGroup;
  if assigned(kg) then
  begin
    kg.GetModConnections.SetModSource(ModSlotIndex, aSource);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;

procedure TModSelectorContextMenu.Handle_ModViaSelected(Sender: TObject; aSource: TModSource);
var
  kg : IKeyGroup;
begin
  kg := Plugin.ActiveKeyGroup;
  if assigned(kg) then
  begin
    kg.GetModConnections.SetModVia(ModSlotIndex, aSource);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;

procedure TModSelectorContextMenu.Handle_ToggleModulationMute(Sender: TObject);
var
  IsMute : boolean;
  kg : IKeyGroup;
begin
  kg := Plugin.ActiveKeyGroup;
  if assigned(kg) then
  begin
    IsMute := kg.GetModConnections.GetModMute(ModSlotIndex);
    IsMute := not IsMute;
    kg.GetModConnections.SetModMute(ModSlotIndex, IsMute);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;


end.
