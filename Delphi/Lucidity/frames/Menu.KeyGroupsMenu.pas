unit Menu.KeyGroupsMenu;

interface

uses
  Menu.CustomPopupMenu,
  eePlugin, Vcl.Menus;

type
  TGroupsMenu = class(TCustomPopupMenu)
  private
  protected
    CurrentGroup : string;
    procedure CreateNewKeyGroup(Sender : TObject);
    procedure DeleteKeyGroup(Sender : TObject);
    procedure MergeAllKeyGroups(Sender : TObject);
    procedure FocusKeyGroup(Sender : TObject);
    procedure CopyKeyGroupParameters(Sender : TObject);
    procedure PasteKeyGroupParameters(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  Lucidity.SampleMap, SysUtils,
  eePluginEx,
  Lucidity.KeyGroupManager,
  uConstants, Lucidity.Interfaces,
  uAutoFree;

{ TGroupsMenu }

constructor TGroupsMenu.Create;
begin
  inherited;

end;

destructor TGroupsMenu.Destroy;
begin

  inherited;
end;

procedure TGroupsMenu.Popup(const x, y: integer);
var
  KG : IKeyGroup;
  KeyGroupName : string;
  mi : TMenuItem;
  KeyGroupInfo : IKeyGroupsInfo;
  c1: Integer;
  RegionList : TRegionInterfaceList;
begin
  if not assigned(Plugin) then exit;

  RegionList := TRegionInterfaceList.Create;
  AutoFree(@RegionList);


  kg := Plugin.FocusedKeyGroup;

  if assigned(kg)
    then CurrentGroup := kg.GetName
    else CurrentGroup := '';

  KeyGroupInfo := Plugin.KeyGroups.GetInfo;

  Menu.Items.Clear;

  for c1 := 0 to KeyGroupInfo.GetKeyGroupCount-1 do
  begin
    KeyGroupName := KeyGroupInfo.GetKeyGroup(c1).GetName;

    RegionList.Clear;
    Plugin.SampleMap.FindRegionsByKeyGroup(KeyGroupName, RegionList);

    mi := TMenuItem.Create(Menu);

    if RegionList.Count = 1
      then mi.Caption := KeyGroupName + '   (1 sample)'
      else mi.Caption := KeyGroupName + '   (' + IntToStr(RegionList.Count) + ' samples)';

    mi.Hint    := KeyGroupName;
    mi.OnClick := FocusKeyGroup;

    if KeyGroupName = CurrentGroup
      then mi.Checked := true
      else mi.Checked := false;

    Menu.Items.Add(mi);
  end;

  //====== spacer ==================
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //================================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'New Key Group';
  mi.OnClick := CreateNewKeyGroup;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Delete Key Group';
  mi.OnClick := DeleteKeyGroup;
  if CurrentGroup = '' then mi.Enabled := false;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Merge All Key Groups';
  mi.OnClick := MergeAllKeyGroups;
  if CurrentGroup = '' then mi.Enabled := false;
  Menu.Items.Add(mi);

  //====== spacer ==================
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //================================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Copy Key Group Settings';
  mi.OnClick := CopyKeyGroupParameters;
  if CurrentGroup = '' then mi.Enabled := false;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Paste Key Group Settings';
  mi.OnClick := PasteKeyGroupParameters;
  if CurrentGroup = '' then mi.Enabled := false;
  Menu.Items.Add(mi);



  Menu.Popup(X, Y);
end;

procedure TGroupsMenu.CreateNewKeyGroup(Sender: TObject);
var
  kg : IKeyGroup;
begin
  kg := Plugin.KeyGroups.NewKeyGroup;
  Plugin.FocusKeyGroup(kg.GetName);

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModAmountChanged);
end;

procedure TGroupsMenu.DeleteKeyGroup(Sender: TObject);
begin
  Plugin.DeleteKeyGroup(CurrentGroup);

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
  try
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModAmountChanged);
  finally
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
  end;
end;

procedure TGroupsMenu.FocusKeyGroup(Sender: TObject);
var
  Text : string;
begin
  Text := (Sender as TMenuItem).Hint;
  Plugin.FocusKeyGroup(Text);


  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
  try
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModAmountChanged);
  finally
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
  end;
end;

procedure TGroupsMenu.MergeAllKeyGroups(Sender: TObject);
begin
  Plugin.MergeAllKeyGroups;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
  try
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModAmountChanged);
  finally
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
  end;
end;

procedure TGroupsMenu.CopyKeyGroupParameters(Sender: TObject);
begin
  Plugin.CopyKeyGroupParameters;
end;

procedure TGroupsMenu.PasteKeyGroupParameters(Sender: TObject);
begin
  Plugin.PasteKeyGroupParameters;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
  try
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModAmountChanged);
  finally
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
  end;
end;







end.
