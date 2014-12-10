unit Menu.MainMenu;

interface

{$INCLUDE Defines.inc}


uses
  Classes,
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;


type
  TMainMenu = class(TCustomPopupMenu)
  private
  protected
    procedure MenuItemClicked(Sender : TObject);
    procedure OpenKeyFile(Sender : TObject);
    procedure ShowAboutBox(Sender : TObject);
    procedure ShowLastLoadedProgramFile(Sender : TObject);

    procedure EventHandle_OpenManual(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ImportSFZ(Sender : TObject);
    procedure EventHandle_SaveProgramAsDefault(Sender : TObject);
    procedure EventHandle_SaveProgram(Sender : TObject);
    procedure EventHandle_SaveProgramAs(Sender : TObject);
    procedure EventHandle_SaveProgramAsWithSamples(Sender : TObject);

    procedure EventHandle_SaveMidiMapAs(Sender : TObject);
    procedure EventHandle_SaveMidiMapAsDefault(Sender : TObject);
    procedure EventHandle_LoadMidiMap(Sender : TObject);
    procedure EventHandle_LoadDefaultMidiMap(Sender : TObject);
    procedure EventHandle_ClearMidiMap(Sender : TObject);

    procedure EventHandle_OpenDataFoler(Sender : TObject);
    procedure EventHandle_AutoSelectChange(Sender : TObject);
    procedure EventHandle_FindMissingSamples(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  Controls,
  Windows,
  ShellApi,
  VamLib.Utils,
  VamLib.WinUtils,
  eeVstExtra,
  InWindowDialog,
  XPLAT.Dialogs,
  eeWinEx,
  eePluginDataDir,
  Lucidity.Enums,
  SysUtils,
  uAutoFree,
  Lucidity.Utils,
  Lucidity.GuiUtils,
  Dialogs, //delete this
  uConstants;

{ TMainMenu }

constructor TMainMenu.Create(AOwner: TComponent);
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
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.NewProgramLoaded);
  end;

  if Tag = 3 then
  begin
    assert(FOwner <> nil);
    OpenDialog := TxpFileOpenDialog.Create(FOwner);
    AutoFree(@OpenDialog);

    SetupFileOpenDialog_Program(OpenDialog);

    OpenDialog.FileName := ''; //TODO:

    if OpenDialog.Execute(GetComponentHandle(FOwner)) then
    begin
      Plugin.ImportProgram(OpenDialog.FileName);
    end;
  end;
end;

procedure TMainMenu.OpenKeyFile(Sender: TObject);
var
  FileOpenDialog : TxpFileOpenDialog;
begin
  assert(FOwner <> nil);
  FileOpenDialog := TxpFileOpenDialog.Create(FOwner);
  AutoFree(@FileOpenDialog);

  FileOpenDialog.FileName := '';
  FileOpenDialog.Title := 'Open Registration Key File...';
  FileOpenDialog.Filter := 'Key Data File|*.dat';

  if FileOpenDialog.Execute(GetComponentHandle(FOwner)) then
  begin
    if Command.RegisterPlugin(Plugin, FileOpenDialog.FileName)
      then Plugin.LoadLastProgram;
  end;
end;

procedure TMainMenu.Popup(const x, y: integer);
var
  mi     : TMenuItem;
  //miRefA : TMenuItem;
  MissingSampleCount : integer;
  MissingSampleText  : string;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 1;
  mi.Caption := 'New Program';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);

  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 3;
  mi.Caption := 'Load Program...';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);

  {
  // Import program menu
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Import Program';
  Menu.Items.Add(mi);
  miRefA := mi;
  }

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Import SFZ...';
  mi.OnClick := EventHandle_ImportSFZ;
  Menu.Items.Add(mi);
  //miRefA.Add(mi);


    //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save Program';
  mi.OnClick := EventHandle_SaveProgram;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save Program As...';
  mi.OnClick := EventHandle_SaveProgramAs;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save Program + Samples As...';
  mi.OnClick := EventHandle_SaveProgramAsWithSamples;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save Current Program As Default';
  mi.OnClick := EventHandle_SaveProgramAsDefault;
  Menu.Items.Add(mi);

  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Clear MIDI Map';
  mi.OnClick := EventHandle_ClearMidiMap;
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
  mi.Caption := 'Save MIDI Map As...';
  mi.OnClick := EventHandle_SaveMidiMapAs;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Save MIDI Map As Default';
  mi.OnClick := EventHandle_SaveMidiMapAsDefault;
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

  //==== insert a spacer =====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=========================

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Show Last Loaded Program File...';
  mi.OnClick := ShowLastLoadedProgramFile;
  mi.Visible := false; //TODO:MED this would be a handy feature for a debugging build.
  Menu.Items.Add(mi);

  // TODO:HIGH this command should be disabled if no samples are missing.
  MissingSampleCount := Command.GetNumberOfMissingSamples(Plugin);
  if MissingSampleCount = 0
    then MissingSampleText := ''
    else MissingSampleText := '(' + IntToStr(MissingSampleCount) + ' missing)';

  mi := TMenuItem.Create(Menu);
  if MissingSampleCount > 0
    then mi.Enabled := true
    else mi.Enabled := false;
  mi.Caption := 'Locate Missing Samples... ' + MissingSampleText;
  mi.OnClick := EventHandle_FindMissingSamples;

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
  s : string;
begin
  //TODO:MED The about box is a small message box. It would be better to have a nice big window with full credits etc.

  s := kProductName + ' Vst Sampler By One Small Clue' + EndOfLine;
  s := s + GetPluginBuildInfo;
  s := s + EndOfLine;

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
  assert(FOwner <> nil);
  OpenDialog := TxpFileOpenDialog.Create(FOwner);
  AutoFree(@OpenDialog);

  SetupFileOpenDialog(Plugin, OpenDialog, TDialogTarget.dtSfzProgram);

  OpenDialog.FileName := ''; //TODO:

  if OpenDialog.Execute(GetComponentHandle(FOwner)) then
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
    fn := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Manual') + 'Grace Manual.html';
    if FileExists(fn) then
    begin
      seResult := ShellExecute(0, 'OPEN', PChar(fn), '', '', SW_SHOWNORMAL);
      if seResult <= 32 then
      begin
        ErrMsg := ShellExecuteErrorCodeToString(seResult);
        InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Error: ' + ErrMsg);
      end;
    end else
    begin
      InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Manual not found. Please contact support.');
    end;
  end else
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Data directory not found. Please re-install.');
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
      InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Error: ' + ErrMsg);
    end;
  end else
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Data Directory not found. Please reinstall.');
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
  assert(FOwner <> nil);
  SaveDialog := TxpFileSaveDialog.Create(FOwner);
  AutoFree(@SaveDialog);

  SetupFileSaveDialog(Plugin, SaveDialog, TDialogTarget.dtMidiMap);

  if SaveDialog.Execute(GetComponentHandle(FOwner)) then
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
  assert(FOwner <> nil);
  OpenDialog := TxpFileOpenDialog.Create(FOwner);
  AutoFree(@OpenDialog);

  SetupFileOpenDialog(Plugin, OpenDialog, TDialogTarget.dtMidiMap);

  OpenDialog.FileName := ''; //TODO:

  if OpenDialog.Execute(GetComponentHandle(FOwner)) then
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
  assert(FOwner <> nil);
  SaveDialog := TxpFileSaveDialog.Create(FOwner);
  AutoFree(@SaveDialog);

  SetupFileSaveDialog_Program(Plugin, SaveDialog);

  if SaveDialog.Execute(GetComponentHandle(FOwner)) then
  begin
    Plugin.SaveProgramToFileWithoutSamples(SaveDialog.FileName);
  end;
end;

procedure TMainMenu.EventHandle_SaveProgramAsDefault(Sender: TObject);
begin
  Plugin.SaveCurrentProgramAsDefault;
end;

procedure TMainMenu.EventHandle_SaveProgramAsWithSamples(Sender: TObject);
var
  SaveDialog : TxpFileSaveDialog;
begin
  assert(FOwner <> nil);
  SaveDialog := TxpFileSaveDialog.Create(FOwner);
  AutoFree(@SaveDialog);

  SetupFileSaveDialog_Program(Plugin, SaveDialog);

  if SaveDialog.Execute(GetComponentHandle(FOwner)) then
  begin
    Plugin.SaveProgramToFileWithSamples(SaveDialog.FileName);
  end;
end;

procedure TMainMenu.ShowLastLoadedProgramFile(Sender: TObject);
var
  fn : string;
  //ErrMsg : string;
begin
  fn := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Error Reports') + RandomString(8) + '.lpg';
  if Plugin.SaveLastPresetToFile(fn) then
  begin
    if FileExists(fn) then
    begin
      //if not ExecuteFile(fn, ErrMsg) then InWindow_ShowMessage(Plugin.Globals.TopLevelForm, ErrMsg);
      ShowFileInWindowsExplorer(fn);
    end else
    begin
      InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Prest Data file hasn''t been saved. Something went wrong somewhere.');
    end;

  end else
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'There was no preset data or something went wrong.');
  end;
end;

procedure TMainMenu.EventHandle_FindMissingSamples(Sender: TObject);
begin
  Command.FindMissingSamples(Plugin);
end;





end.
