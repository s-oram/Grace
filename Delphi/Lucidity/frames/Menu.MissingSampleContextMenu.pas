unit Menu.MissingSampleContextMenu;

interface

uses
  Vcl.Menus,
  Menu.CustomPopupMenu;

type
  TMissingSampleContextMenu = class(TCustomPopupMenu)
  private

  protected
    procedure HandleEvent_LocateMissingSample(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

  end;

implementation

uses
  SysUtils,
  XPLAT.Dialogs,
  Dialogs, //delete this
  VamLib.Utils,
  uConstants,
  Lucidity.Types,
  Lucidity.Interfaces,
  Lucidity.GuiUtils;

{ TMissingSampleContextMenu }

constructor TMissingSampleContextMenu.Create;
begin
  inherited;
end;

destructor TMissingSampleContextMenu.Destroy;
begin

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
  FileOpenDialog : TxpFileOpenDialog;
  rx : IRegion;
  fn : string;
  newRG : IRegion;
begin
  rx := Plugin.FocusedRegion;

  if not assigned(rx) then exit;
  if rx.GetProperties^.SampleDataLoaded = true then exit;
  if rx.GetProperties^.SampleErrorType <> TSampleError.FileNotFound then exit;

  FileOpenDialog := TxpFileOpenDialog.Create(nil);
  AutoFree(@FileOpenDialog);

  SetupFileOpenDialog(Plugin, FileOpenDialog, TDialogTarget.dtAudioFile);

  fn := rx.GetProperties^.SampleFileName;
  fn := ExtractFileName(fn);
  FileOpenDialog.FileName := fn;


  if FileOpenDialog.Execute then
  begin
    newRG := Plugin.ReplaceSample(rx, FileOpenDialog.FileName);
    if assigned(newRG) then
    begin
      Plugin.FocusRegion(newRG.GetProperties^.UniqueID);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleRegionChanged);
    end;
  end;
end;



end.
