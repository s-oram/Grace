unit Menu.SampleMapContextMenu;

interface

uses
  Lucidity.Interfaces,
  eePlugin, Vcl.Menus, Lucidity.SampleMap;

type
  TSampleMapContextMenu = class
  private
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
    RegionContext : IRegion;


    procedure MenuItemClicked(Sender : TObject);

    procedure EventHandle_ShowInWindowsExplorer(Sender : TObject);
    procedure EventHandle_DuplicateRegions(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer; aRegion : IRegion);

  end;

implementation

uses
  SysUtils,
  eeWinEx,
  VamLib.Utils,
  Lucidity.GuiUtils,
  Dialogs, //delete this
  Lucidity.KeyGroupManager,
  uConstants;

{ TSampleMapContextMenu }

constructor TSampleMapContextMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TSampleMapContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TSampleMapContextMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

procedure TSampleMapContextMenu.Popup(const x, y: integer; aRegion : IRegion);
var
  c1 : integer;
  mi : TMenuItem;
  KeyGroupMenuItem : TMenuItem;
  KeyGroupInfo : IKeyGroupsInfo;
  KeyGroupName : string;
begin
  RegionContext := aRegion;


  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 1;
  mi.Caption := 'Delete Regions...';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Duplicate Regions...';
  mi.OnClick := self.EventHandle_DuplicateRegions;
  Menu.Items.Add(mi);


  //============================================================================
  //=== Move to Key Group =====
  //============================================================================

  KeyGroupMenuItem := TMenuItem.Create(Menu);
  KeyGroupMenuItem.Tag     := 1;
  KeyGroupMenuItem.Caption := 'Move To Key Group';
  Menu.Items.Add(KeyGroupMenuItem);

  KeyGroupInfo := Plugin.KeyGroups.GetInfo;
  for c1 := 0 to KeyGroupInfo.GetKeyGroupCount-1 do
  begin
    KeyGroupName := KeyGroupInfo.GetKeyGroup(c1).GetName;

    mi := TMenuItem.Create(Menu);
    mi.Caption := KeyGroupName;
    mi.Hint    := KeyGroupName;
    mi.OnClick := MenuItemClicked;
    mi.Tag     := 2;

    //if KeyGroupName = KeyGroupInfo.GetFocusedGroup.GetName
    //  then mi.Checked := true
    //  else mi.Checked := false;

    KeyGroupMenuItem.Add(mi);
  end;

  //============================================================================



  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Show in Windows Exporer...';
  mi.OnClick := EventHandle_ShowInWindowsExplorer;
  Menu.Items.Add(mi);





  Menu.Popup(x, y);
end;

procedure TSampleMapContextMenu.MenuItemClicked(Sender: TObject);
var
  Tag : integer;
  KeyGroupName : string;
begin
  if not assigned(Plugin) then raise Exception.Create('Plugin not assigned!!');

  Tag := (Sender as TMenuItem).Tag;

  case Tag of
    1: Command.DeleteRegionsSelectedInSampleMap(Plugin);

    2:
    begin
      KeyGroupName := (Sender as TMenuItem).Hint;
      Plugin.MoveSelectedRegionsToKeyGroup(KeyGroupName);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleRegionChanged);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.CheckForSampleFocusChange);
    end;
  else
    raise Exception.Create('Unexpected tag value.');
  end;
end;



procedure TSampleMapContextMenu.EventHandle_DuplicateRegions(Sender: TObject);
var
  x : integer;
  Text : string;
begin
  if not assigned(Plugin) then raise Exception.Create('Plugin not assigned!!');
  Plugin.DuplicateSelectedRegions;

  x := Plugin.SampleMap.SelectedRegionCount;
  case x of
  0: Text := 'No regions duplicated.';
  1: Text := '1 region duplicated.';
  else
    Text := IntToStr(x) + ' regions duplicated';
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Msg_XRegionsDuplicated, @Text, nil);
end;

procedure TSampleMapContextMenu.EventHandle_ShowInWindowsExplorer(Sender: TObject);
var
  fn : string;
begin
  if not assigned(Plugin) then exit;

  if assigned(RegionContext) then
  begin
    fn := RegionContext.GetProperties^.SampleFileName;
    if FileExists(Fn) then
    begin
      OpenFolderAndSelectFile(Fn);
    end;
  end;
end;




end.
