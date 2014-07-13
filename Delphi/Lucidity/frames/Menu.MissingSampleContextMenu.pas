unit Menu.MissingSampleContextMenu;

interface

uses
  Vcl.Menus,
  Menu.CustomPopupMenu;

type
  TMissingSampleContextMenu = class(TCustomPopupMenu)
  private
    Menu : TPopUpMenu;
  protected
    procedure HandleEvent_LocateMissingSample(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

  end;

implementation

uses
  Dialogs,
  VamLib.Utils,
  uConstants,
  Lucidity.Types,
  Lucidity.Interfaces,
  uGuiUtils;

{ TMissingSampleContextMenu }

constructor TMissingSampleContextMenu.Create;
begin
  Menu := TPopUpMenu.Create(nil);
end;

destructor TMissingSampleContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TMissingSampleContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Locate Missing Sample...';
  mi.OnClick := HandleEvent_LocateMissingSample;
  Menu.Items.Add(mi);

  Menu.Popup(x, y);
end;

procedure TMissingSampleContextMenu.HandleEvent_LocateMissingSample(Sender: TObject);
var
  FileOpenDialog : TFileOpenDialog;
  rx : IRegion;
begin
  rx := Plugin.FocusedRegion;
  if not assigned(rx) then exit;
  if rx.GetProperties^.SampleDataLoaded = true then exit;
  if rx.GetProperties^.SampleErrorType <> TSampleError.FileNotFound then exit;


  FileOpenDialog := TFileOpenDialog.Create(nil);
  AutoFree(@FileOpenDialog);

  SetupFileOpenDialog(FileOpenDialog, TDialogTarget.dtAudioFile);

  if FileOpenDialog.Execute then
  begin
    if rx.ReplaceSample(FileOpenDialog.FileName) then
    begin
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleRegionChanged);
    end;
  end;
end;



end.
