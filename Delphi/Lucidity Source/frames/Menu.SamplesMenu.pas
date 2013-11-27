unit Menu.SamplesMenu;

interface

uses
  Menu.CustomPopupMenu,
  eePlugin, Vcl.Menus, Windows, Messages;

type
  TSamplesMenu = class(TCustomPopupMenu)
  private
    // Receive windows message in a custom non-windowed class.
    // http://delphi.about.com/od/windowsshellapi/a/receive-windows-messages-in-custom-delphi-class-nonwindowed-control.htm
    // http://www.delphidabbler.com/articles?article=1

    // Select menu item without closing menu.
    // http://stackoverflow.com/questions/5983217/how-to-select-a-menu-item-without-closing-the-menu
    // http://stackoverflow.com/questions/14182307/checking-unchecking-tmenuitem-without-closing-the-tpopupmenu

    FGetPopupWindowHandle: Boolean;
    FPopupWindowHandle: HWND;
    OrgPopupWindowProc, HookedPopupWindowProc: Pointer;
    FSelectedItemID: UINT;

    fMsgHandlerHWND : HWND;
    procedure WndMethod(var Msg: TMessage); virtual;
    procedure PopupWindowProc(var Msg: TMessage);

    procedure MenuSelectPos(aMenu: TMenu; ItemPos: UINT; out CanClose: Boolean);
    procedure MenuSelectID(aMenu: TMenu; ItemID: UINT; out CanClose: Boolean);
  protected
    Menu : TPopUpMenu;
    AppHandle : HWND;
    x : integer;

    procedure SampleClicked(Sender : TObject);
  public
    constructor Create(aAppHandle : HWND);
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  SysUtils, Classes,
  uKeyGroupManager, uSampleMap,
  uConstants, uLucidityKeyGroupInterface,
  uAutoFree;

{ TSamplesMenu }


constructor TSamplesMenu.Create(aAppHandle : HWND);
begin
  AppHandle := aAppHandle;

  fMsgHandlerHWND := AllocateHWnd(WndMethod);

  Menu := TPopupMenu.Create(nil);
end;

destructor TSamplesMenu.Destroy;
begin
  DeallocateHWnd(fMsgHandlerHWND);
  Menu.Free;
  inherited;
end;

procedure TSamplesMenu.Popup(const x, y: integer);
const
  //tpmFlags = TPM_LEFTALIGN or TPM_RETURNCMD or TPM_NONOTIFY;
  tpmFlags = 0;
var
  mi : TMenuItem;
  SampleMapInfo : ISampleMapInfo;
  KeyGroupInfo : IKeyGroupsInfo;
  c1: Integer;

  fn : string;
  aRegion : IRegion;
  RegionList : TRegionInterfaceList;

  KeyGroupName : string;
  c2: Integer;
begin
  if not assigned(Plugin) then exit;

  RegionList := TRegionInterfaceList.Create;
  AutoFree(@RegionList);

  SampleMapInfo := Plugin.SampleMap.GetInfo;
  KeyGroupInfo := Plugin.KeyGroups.GetInfo;

  Menu.Items.Clear;


  for c1 := 0 to KeyGroupInfo.GetKeyGroupCount-1 do
  begin
    KeyGroupName := KeyGroupInfo.GetKeyGroup(c1).GetName;

    RegionList.Clear;
    Plugin.SampleMap.FindRegionsByKeyGroup(KeyGroupName, RegionList);

    if RegionList.Count > 0 then
    begin
      mi := TMenuItem.Create(Menu);
      mi.Caption := '---- ' + KeyGroupName + ' ----';
      mi.Enabled := false;
      Menu.Items.Add(mi);

      for c2 := 0 to RegionList.Count-1 do
      begin
        aRegion := RegionList.Items[c2];

        fn := aRegion.GetProperties^.SampleFileName;
        fn := ExtractFileName(fn);

        mi := TMenuItem.Create(Menu);
        mi.Caption := fn;
        mi.Hint    := GuidToString(aRegion.GetProperties^.UniqueID);
        mi.Checked := aRegion.GetProperties^.IsFocused;
        mi.OnClick := SampleClicked;
        Menu.Items.Add(mi);
      end;

      mi := TMenuItem.Create(Menu);
      mi.Caption := '-';
      Menu.Items.Add(mi);
    end;
  end;

  Menu.Popup(X, Y);

  //TrackPopupMenu(Menu.Handle, tpmFlags, X, Y, 0, fMsgHandlerHWND, nil);
end;

procedure TSamplesMenu.SampleClicked(Sender: TObject);
var
  guid : TGuid;
  mi : TMenuItem;
begin
  assert(Sender is TMenuItem);

  mi := (Sender as TMenuItem);

  mi.Checked := not mi.Checked;

  guid := StringToGuid((Sender as TMenuItem).Hint);

  Plugin.FocusRegion(guid);

  //if FPopupWindowHandle <> 0
  //  then InvalidateRect(FPopupWindowHandle, nil, False);
end;

procedure TSamplesMenu.WndMethod(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_INITMENUPOPUP:
    begin
      if TWMInitMenuPopup(Msg).MenuPopup = Menu.Handle
        then FGetPopupWindowHandle := True;
    end;

    WM_ENTERIDLE:
    begin
      if FGetPopupWindowHandle then
      begin
        FGetPopupWindowHandle := False;
        FPopupWindowHandle := TWmEnterIdle(Msg).IdleWnd;
        HookedPopupWindowProc := classes.MakeObjectInstance(PopupWindowProc);
        OrgPopupWindowProc := Pointer(GetWindowLong(FPopupWindowHandle, GWL_WNDPROC));
        SetWindowLong(FPopupWindowHandle, GWL_WNDPROC, Longint(HookedPopupWindowProc));
      end;
    end;

    WM_MENUSELECT:
    begin
       if TWmMenuSelect(Msg).Menu = Menu.Handle then
       begin
         FSelectedItemID := TWmMenuSelect(Msg).IDItem;
       end;
    end;

  end;
  //InvalidateRect(FPopupWindowHandle, nil, False);

  Msg.Result := DefWindowProc(fMsgHandlerHWND, Msg.Msg, Msg.wParam, Msg.lParam);

end;

const
  MN_BUTTONDOWN = $01ED;

procedure TSamplesMenu.PopupWindowProc(var Msg: TMessage);
var
  NormalItem: Boolean;
begin
  case Msg.Msg of
    MN_BUTTONDOWN:
      begin
        MenuSelectPos(Menu, UINT(Msg.WParamLo), NormalItem);
        if not NormalItem then
          Exit;
      end;
    WM_KEYDOWN:
      if Msg.WParam = VK_RETURN then begin
        MenuSelectID(Menu, FSelectedItemID, NormalItem);
        if not NormalItem then
          Exit;
      end;
    WM_DESTROY:
      begin
        SetWindowLong(FPopupWindowHandle, GWL_WNDPROC, Longint(OrgPopupWindowProc));
        classes.FreeObjectInstance(HookedPopupWindowProc);
      end;
  end;

  Msg.Result := CallWindowProc(OrgPopupWindowProc, FPopupWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);

end;

procedure TSamplesMenu.MenuSelectID(aMenu: TMenu; ItemID: UINT; out CanClose: Boolean);
var
  Item: TMenuItem;
begin
  CanClose := false;
  Item := aMenu.FindItem(ItemID, fkCommand);
  if Assigned(Item) then
  begin
    // aMenu Item is clicked
    Item.Click;
    // Panel1.Caption := Item.Name;
    //CanClose := Item = Item1Normal1;
  end;
end;

procedure TSamplesMenu.MenuSelectPos(aMenu: TMenu; ItemPos: UINT; out CanClose: Boolean);
begin
  MenuSelectID(aMenu, GetMenuItemID(aMenu.Handle, ItemPos), CanClose);
end;





end.
