unit Menu.MainMenu;

interface

{$INCLUDE Defines.inc}


uses
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;


type
  TMainMenu = class(TCustomPopupMenu)
  private
  protected
    procedure MenuItemClicked(Sender : TObject);
    procedure OpenKeyFile(Sender : TObject);
    procedure ShowAboutBox(Sender : TObject);

    procedure EventHandle_OpenManual(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ImportSFZ(Sender : TObject);
    procedure EventHandle_SaveProgramAsDefault(Sender : TObject);
    procedure EventHandle_SaveProgram(Sender : TObject);
    procedure EventHandle_SaveProgramAs(Sender : TObject);

    procedure EventHandle_SaveMidiMapAs(Sender : TObject);
    procedure EventHandle_SaveMidiMapAsDefault(Sender : TObject);
    procedure EventHandle_LoadMidiMap(Sender : TObject);
    procedure EventHandle_LoadDefaultMidiMap(Sender : TObject);
    procedure EventHandle_ClearMidiMap(Sender : TObject);

    procedure EventHandle_OpenDataFoler(Sender : TObject);
    procedure EventHandle_AutoSelectChange(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  Windows,
  ShellApi,
  VamLib.Utils,
  VamLib.WinUtils,
  eeVstExtra,
  InWindowDialog,
  XPLAT.Dialogs,
  eeWinEx,
  eePluginDataDir,
  uLucidityEnums,
  uAboutDialog,
  SysUtils,
  uAutoFree,
  Lucidity.GuiUtils,
  Dialogs, //delete this
  uConstants;

{ TMainMenu }

constructor TMainMenu.Create;
begin
  inherited;
end;

destructor TMainMenu.Destroy;
begin

  inherited;
end;

procedure TMainMenu.MenuItemClicked(Sender: TObject);
var
  Tag : integer;
  OpenDialog : TxpFileOpenDialog;
begin
  assert(Sender is TMenuItem);

  Tag := (Sender as TMenuItem).Tag;

  if Tag = 1 then
  begin
    Plugin.InitializeState;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
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

  FileOpenDialog.FileName := '';
  FileOpenDialog.Title := 'Open Registration Key File...';
  FileOpenDialog.Filter := 'Key Data File|*.dat';

  if FileOpenDialog.Execute then
  begin
    Command.RegisterPlugin(Plugin, FileOpenDialog.FileName);
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
  mi.Caption := 'Save Program';
  mi.OnClick := EventHandle_SaveProgram;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save Program As...';
  mi.OnClick := EventHandle_SaveProgramAs;
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
  mi.Caption := 'Save MIDI Map As...';
  mi.OnClick := EventHandle_SaveMidiMapAs;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save MIDI Map As Default';
  mi.OnClick := EventHandle_SaveMidiMapAsDefault;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Load MIDI Map...';
  mi.OnClick := EventHandle_LoadMidiMap;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Load Default MIDI Map';
  mi.OnClick := EventHandle_LoadDefaultMidiMap;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Clear MIDI Map';
  mi.OnClick := EventHandle_ClearMidiMap;
  Menu.Items.Add(mi);

  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Auto Select On';
  mi.OnClick := EventHandle_AutoSelectChange;
  mi.Tag     := 1;
  if Plugin.Globals.GuiState.IsAutoSelectActive
    then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Auto Select Off';
  mi.OnClick := EventHandle_AutoSelectChange;
  mi.Tag     := 2;
  if not Plugin.Globals.GuiState.IsAutoSelectActive
    then mi.Checked := true;
  Menu.Items.Add(mi);

  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  if Plugin.Globals.CopyProtection.IsRegistered = false then
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
  mi.Caption := 'Open Data Folder...';
  mi.OnClick := EventHandle_OpenDataFoler;
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
begin
  //TODO:MED The about box is a small message box. It would be better to have a nice big window with full credits etc.

  s := 'Lucidity Vst Sampler By One Small Clue' + EndOfLine;

  s := s + EndOfLine;

  {$IFDEF ReleaseBuild}
  s := s + 'Version ' + GetBuildInfoAsString + ' Release Build' + EndOfLine;
  {$ELSE}
  s := s + 'Version ' + GetBuildInfoAsString + ' Debug Build' + EndOfLine;
  {$ENDIF}

  if Plugin.Globals.CopyProtection.IsRegistered
    then s := s + 'Registered to ' + Plugin.Globals.CopyProtection.KeyData.UserName + ' <' + Plugin.Globals.CopyProtection.KeyData.UserEmail + '>' + EndOfLine
    else s := s + 'UNREGISTERED' + EndOfLine;

  s := s + EndOfLine;

  s := s + 'VST PlugIn Technology by Steinberg';

  s := s + EndOfLine;

  InWindow_ShowMessage(Plugin.Globals.TopLevelForm, s);
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

  SetupFileOpenDialog(Plugin, OpenDialog, TDialogTarget.dtSfzProgram);

  OpenDialog.FileName := ''; //TODO:

  if OpenDialog.Execute then
  begin
    Plugin.ImportProgram(OpenDialog.FileName, TProgramFormat.Sfz);
  end;
end;

procedure TMainMenu.EventHandle_OpenManual(Sender: TObject);
var
  fn : string;
  seResult : integer;
  ErrMsg : string;
begin
  if PluginDataDir.Exists then
  begin
    fn := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Manual') + 'Lucidity Manual.html';
    if FileExists(fn) then
    begin
      seResult := ShellExecute(0, 'OPEN', PChar(fn), '', '', SW_SHOWNORMAL);
      if seResult <= 32 then
      begin
        ErrMsg := ShellExecuteErrorCodeToString(seResult);
        InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Lucidity Error: ' + ErrMsg);
      end;
    end else
    begin
      InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Manual not found. Please contact support.');
    end;
  end else
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Data directory not found. Please re-install Lucidity.');
  end;
end;

procedure TMainMenu.EventHandle_OpenDataFoler(Sender: TObject);
var
  ErrMsg : string;
begin
  if (PluginDataDir^.Exists) then
  begin
    if not ShowDirectoryInWindowsExplorer(PluginDataDir^.Path, ErrMsg) then
    begin
      InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Lucidity Error: ' + ErrMsg);
    end;
  end else
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Data Directory not found. Please reinstall Lucidity.');
  end;
end;

procedure TMainMenu.EventHandle_AutoSelectChange(Sender: TObject);
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
end;

procedure TMainMenu.EventHandle_ClearMidiMap(Sender: TObject);
begin
  Plugin.ClearMidiMap;
end;

procedure TMainMenu.EventHandle_SaveMidiMapAsDefault(Sender: TObject);
begin
  Plugin.SaveMIDIMapAsDefault;
end;

procedure TMainMenu.EventHandle_SaveMidiMapAs(Sender: TObject);
var
  SaveDialog : TxpFileSaveDialog;
begin
  SaveDialog := TxpFileSaveDialog.Create(nil);
  AutoFree(@SaveDialog);

  SetupFileSaveDialog(Plugin, SaveDialog, TDialogTarget.dtMidiMap);

  if SaveDialog.Execute then
  begin
    Plugin.SaveMidiMap(SaveDialog.FileName);
  end;
end;

procedure TMainMenu.EventHandle_LoadDefaultMidiMap(Sender: TObject);
begin
  Plugin.LoadDefaultMIDIMap;
end;

procedure TMainMenu.EventHandle_LoadMidiMap(Sender: TObject);
var
  OpenDialog : TxpFileOpenDialog;
begin
  OpenDialog := TxpFileOpenDialog.Create(nil);
  AutoFree(@OpenDialog);

  SetupFileOpenDialog(Plugin, OpenDialog, TDialogTarget.dtMidiMap);

  OpenDialog.FileName := ''; //TODO:

  if OpenDialog.Execute then
  begin
    Plugin.LoadMidiMap(OpenDialog.FileName);
  end;
end;

procedure TMainMenu.EventHandle_SaveProgram(Sender: TObject);
begin
  if Plugin.SaveCurrentProgram = true then
  begin
    // TODO:MED show a fading message here.
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Program Saved');
  end else
  begin
    EventHandle_SaveProgramAs(self);
  end;
  
end;

procedure TMainMenu.EventHandle_SaveProgramAs(Sender: TObject);
var
  SaveDialog : TxpFileSaveDialog;
begin
  SaveDialog := TxpFileSaveDialog.Create(nil);
  AutoFree(@SaveDialog);

  SetupFileSaveDialog_Program(Plugin, SaveDialog);

  if SaveDialog.Execute then
  begin
    Plugin.SaveProgramToFile(SaveDialog.FileName);
  end;
end;

procedure TMainMenu.EventHandle_SaveProgramAsDefault(Sender: TObject);
begin
  Plugin.SaveProgramAsDefault;
end;







end.
