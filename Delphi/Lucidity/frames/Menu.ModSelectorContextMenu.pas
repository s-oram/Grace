unit Menu.ModSelectorContextMenu;

interface

uses
  eeEnumMenu, uLucidityEnums,
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;

type
  TModSourceMenu = TEnumMenu<TModSource>;

  TModSelectorContextMenu = class(TCustomPopupMenu)
  private
  protected
    Menu : TPopUpMenu;
    ModSourceMenu : TModSourceMenu;
    ModViaMenu    : TModSourceMenu;
    ModSlotIndex  : integer;

    procedure Handle_ModSourceSelected(Sender : TObject; aSource : TModSource);
    procedure Handle_ModViaSelected(Sender : TObject; aSource : TModSource);

    procedure Handle_ToggleModulationMute(Sender : TObject);

    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const aModSlotIndex : integer; const x, y : integer);
  end;

implementation

uses
  SysUtils,
  Lucidity.Interfaces,
  uConstants;


procedure SortModMenu(const aMenu : TModSourceMenu);
  procedure MoveItemToMenu(const DestMenu : TMenuItem; const SourceMenu : TModSourceMenu; const Item : TModSource);
  var
    mi : TMenuItem;
  begin
    mi := SourceMenu.FindMenuItemByEnum(Item);
    SourceMenu.Items.Remove(mi);
    DestMenu.Add(mi);
    // Check the parent menu so the user can find the current checked item even
    // when buried in a child menu.
    if mi.Checked
      then DestMenu.Checked := true;
  end;
var
  UnipolarMenu : TMenuItem;
  BipolarMenu  : TMenuItem;
  SpacerMI : TMenuItem;
  mi : TMenuItem;
begin
  UnipolarMenu := TMenuItem.Create(aMenu.Menu);
  BipolarMenu  := TMenuItem.Create(aMenu.Menu);

  UnipolarMenu.Caption := 'Unipolar';
  BipolarMenu.Caption := 'Bipolar';

  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Midi_Note_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Midi_Velocity_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Midi_PitchBend_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Midi_ModWheel_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Midi_Toggle_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.AmpEnv_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.FilterEnv_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Lfo1_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.Lfo2_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.StepSeq1_Unipolar);
  MoveItemToMenu(UnipolarMenu, aMenu, TModSource.StepSeq2_Unipolar);

  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Midi_Note_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Midi_Velocity_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Midi_PitchBend_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Midi_ModWheel_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Midi_Toggle_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.AmpEnv_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.FilterEnv_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Lfo1_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.Lfo2_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.StepSeq1_Bipolar);
  MoveItemToMenu(BipolarMenu, aMenu, TModSource.StepSeq2_Bipolar);

  aMenu.Items.Add(UnipolarMenu);
  aMenu.Items.Add(BipolarMenu);

  SpacerMI := TMenuItem.Create(aMenu.Menu);
  SpacerMI.Caption := '-';
  aMenu.Items.Add(SpacerMI);

  mi := aMenu.Items[0];
  aMenu.Items.Remove(mi);
  aMenu.Items.Add(mi);


end;

{ TModSelectorContextMenu }

constructor TModSelectorContextMenu.Create;
begin
end;

destructor TModSelectorContextMenu.Destroy;
begin
  ModSourceMenu.Free;
  ModViaMenu.Free;
  Menu.Free;
  inherited;
end;

procedure TModSelectorContextMenu.Init;
var
  mi : TMenuItem;
begin
  if assigned(ModSourceMenu) then FreeAndNil(ModSourceMenu);
  if assigned(ModViaMenu)    then FreeAndNil(ModViaMenu);
  if assigned(Menu)          then FreeAndNil(Menu);

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


  SortModMenu(ModSourceMenu);
  SortModMenu(ModViaMenu);

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

procedure TModSelectorContextMenu.Handle_ModSourceSelected(Sender: TObject; aSource: TModSource);
var
  kg : IKeyGroup;
begin
  kg := Plugin.ActiveKeyGroup;
  if assigned(kg) then
  begin
    kg.GetModConnections.SetModSource(ModSlotIndex, aSource);
    Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
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
    Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
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
    Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
  end;
end;


end.
