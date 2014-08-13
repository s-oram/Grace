unit Menu.MainMenu;

interface

{$INCLUDE Defines.inc}


uses
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;


type
  TMainMenu = class(TCustomPopupMenu)
  private
  protected
    Menu : TPopUpMenu;
    procedure MenuItemClicked(Sender : TObject);
    procedure OpenKeyFile(Sender : TObject);
    procedure ShowAboutBox(Sender : TObject);

    procedure EventHandle_OpenManual(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ImportSFZ(Sender : TObject);
    procedure EventHandle_SaveProgramAsDefault(Sender : TObject);
    procedure EventHandle_SaveMidiMapAsDefault(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  XPLAT.Dialogs,
  Windows,
  ShellApi,
  eePluginDataDir,
  uLucidityEnums,
  uAboutDialog,
  SysUtils,
  uAutoFree,
  uGuiUtils,
  Dialogs,
  uConstants;

{ TMainMenu }

constructor TMainMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TMainMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TMainMenu.MenuItemClicked(Sender: TObject);
var
  Tag : integer;
  SaveDialog : TxpFileSaveDialog;
  OpenDialog : TxpFileOpenDialog;
begin
  assert(Sender is TMenuItem);

  Tag := (Sender as TMenuItem).Tag;

  if Tag = 1 then
  begin
    Plugin.InitializeState;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
  end;

  if Tag = 2 then
  begin
    SaveDialog := TxpFileSaveDialog.Create(nil);
    AutoFree(@SaveDialog);

    SetupFileSaveDialog_Program(SaveDialog);

    SaveDialog.FileName := Plugin.PresetName + kLucidityProgramFileExtension;

    if SaveDialog.Execute then
    begin
      Plugin.SaveProgramToFile(SaveDialog.FileName);
    end;
  end;

  if Tag = 3 then
  begin
    OpenDialog := TxpFileOpenDialog.Create(nil);
    AutoFree(@OpenDialog);

    SetupFileOpenDialog_Program(OpenDialog);

    OpenDialog.FileName := ''; //TODO:

    if OpenDialog.Execute then
    begin
      Plugin.ImportProgram(OpenDialog.FileName);
    end;
  end;


end;

procedure TMainMenu.OpenKeyFile(Sender: TObject);
var
  FileOpenDialog : TxpFileOpenDialog;
begin
  FileOpenDialog := TxpFileOpenDialog.Create(nil);
  AutoFree(@FileOpenDialog);

  FileOpenDialog.FileName := kKeyFileName;
  FileOpenDialog.Title := 'Open Registration Key File...';
  FileOpenDialog.Filter := 'Key Data File|*.key';

  if FileOpenDialog.Execute then
  begin
    Plugin.Globals.LoadRegistrationKeyFile(FileOpenDialog.FileName);

    if Plugin.Globals.KeyData.IsKeyChecksumValid then
    begin
      ShowMessage('Lucidity is now registered. Thank you for your support!');
    end else
    begin
      ShowMessage('ERROR: Unable to register. Please contact support.');
    end;

  end;


end;

procedure TMainMenu.Popup(const x, y: integer);
var
  mi     : TMenuItem;
  miRefA : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 1;
  mi.Caption := 'New Program';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 2;
  mi.Caption := 'Save Program As...';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save Program As Default';
  mi.OnClick := EventHandle_SaveProgramAsDefault;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 3;
  mi.Caption := 'Load Program...';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);


  // Import program menu
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Import Program';
  Menu.Items.Add(mi);
  miRefA := mi;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Import SFZ...';
  mi.OnClick := EventHandle_ImportSFZ;
  miRefA.Add(mi);




  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Map...';
  mi.OnClick := EventHandle_EditSampleMap;
  Menu.Items.Add(mi);


  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save MIDI Map As Default';
  mi.OnClick := EventHandle_SaveMidiMapAsDefault;
  Menu.Items.Add(mi);

  if Plugin.Globals.KeyData.IsKeyChecksumValid = false then
  begin
    // insert the register option...
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Open Registration Key File...';
    mi.OnClick := OpenKeyFile;
    Menu.Items.Add(mi);
  end;


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Open Manual...';
  mi.OnClick := EventHandle_OpenManual;
  Menu.Items.Add(mi);


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'About...';
  mi.OnClick := ShowAboutBox;
  Menu.Items.Add(mi);

  //NOTE: Not sure if I want to inlude a 'Registered To...' item. hmmm.
  {
  if Plugin.Globals.KeyData.IsKeyChecksumValid = true then
  begin
    // insert the register option...
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Registered to ' + Plugin.Globals.KeyData.UserName;
    mi.Enabled := false;
    Menu.Items.Add(mi);
  end;
  }

  Menu.Popup(x, y);
end;

procedure TMainMenu.ShowAboutBox(Sender: TObject);
var
  AboutDialog : IAboutDialog;
  CloseCallback : TProc;
  s : string;
begin
  AboutDialog := TAboutDialog.Create;
  AboutDialog.Setup(DialogDisplay.GetDisplayArea);

  if Plugin.Globals.KeyData.IsKeyChecksumValid = true
    then s := 'Registered to ' + Plugin.Globals.KeyData.UserName
    else s := 'UNREGISTERED';
  AboutDialog.AboutText.Add(s);

  CloseCallback := procedure
  begin
    AboutDialog := nil;
  end;

  DialogDisplay.Show(true, CloseCallback);
end;

procedure TMainMenu.EventHandle_EditSampleMap(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleMapVisibility(Plugin);
end;

procedure TMainMenu.EventHandle_ImportSFZ(Sender: TObject);
var
  OpenDialog : TxpFileOpenDialog;
begin
  OpenDialog := TxpFileOpenDialog.Create(nil);
  AutoFree(@OpenDialog);

  SetupFileOpenDialog(OpenDialog, TDialogTarget.dtSfzProgram);

  OpenDialog.FileName := ''; //TODO:

  if OpenDialog.Execute then
  begin
    Plugin.ImportProgram(OpenDialog.FileName, TProgramFormat.Sfz);
  end;
end;

procedure TMainMenu.EventHandle_OpenManual(Sender: TObject);
var
  fn : string;
begin
  if PluginDataDir.Exists then
  begin
    fn := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Manual') + 'Lucidity Manual.html';
    if FileExists(fn) then
    begin
      ShellExecute(0, 'OPEN', PChar(fn), '', '', SW_SHOWNORMAL);
    end else
    begin
      ShowMessage('Manual not found. Please contact support.');
    end;
  end else
  begin
    ShowMessage('Data directory not found. Please re-install Lucidity.');
  end;
end;

procedure TMainMenu.EventHandle_SaveMidiMapAsDefault(Sender: TObject);
begin

end;

procedure TMainMenu.EventHandle_SaveProgramAsDefault(Sender: TObject);
begin
  Plugin.SaveProgramAsDefault;
end;

end.
