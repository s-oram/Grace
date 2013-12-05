unit Menu.MainMenu;

interface

{$WARN SYMBOL_PLATFORM OFF}

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

    procedure EventHandle_EditSamplePoints(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ImportSFZ(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);
  end;

implementation

uses
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
  SaveDialog : TFileSaveDialog;
  OpenDialog : TFileOpenDialog;
begin
  assert(Sender is TMenuItem);

  Tag := (Sender as TMenuItem).Tag;

  if Tag = 1 then
  begin
    Plugin.InitializeState;
    Plugin.Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
  end;


  if Tag = 2 then
  begin
    SaveDialog := TFileSaveDialog.Create(nil);
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
    OpenDialog := TFileOpenDialog.Create(nil);
    AutoFree(@OpenDialog);

    SetupFileOpenDialog_Program(OpenDialog);

    OpenDialog.FileName := ''; //TODO:

    if OpenDialog.Execute then
    begin
      Plugin.LoadProgramFromFile(OpenDialog.FileName);
    end;
  end;


end;

procedure TMainMenu.OpenKeyFile(Sender: TObject);
var
  FileOpenDialog : TFileOpenDialog;
begin
  FileOpenDialog := TFileOpenDialog.Create(nil);
  AutoFree(@FileOpenDialog);

  FileOpenDialog.FileName := kKeyFileName;
  FileOpenDialog.Title := 'Open Registration Key File...';

  with FileOpenDialog.FileTypes.Add do
  begin
    DisplayName := 'Key Data File';
    FileMask    := '*.dat';
  end;

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
  mi.Caption := 'Save Program...';
  mi.OnClick := MenuItemClicked;
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
  mi.Caption := 'Import SFZ';
  mi.OnClick := EventHandle_ImportSFZ;
  miRefA.Add(mi);




  // insert a spacer...
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Points...';
  mi.OnClick := EventHandle_EditSamplePoints;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Map...';
  mi.OnClick := EventHandle_EditSampleMap;
  Menu.Items.Add(mi);




  // insert a spacer...
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'About...';
  mi.OnClick := ShowAboutBox;
  Menu.Items.Add(mi);






  if Plugin.Globals.KeyData.IsKeyChecksumValid = false then
  begin
    // insert a spacer...
    mi := TMenuItem.Create(Menu);
    mi.Caption := '-';
    Menu.Items.Add(mi);

    // insert the register option...
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Open Registration Key File...';
    mi.OnClick := OpenKeyFile;
    Menu.Items.Add(mi);
  end;



  Menu.Popup(x, y);
end;

procedure TMainMenu.ShowAboutBox(Sender: TObject);
var
  AboutDialog : IAboutDialog;
  CloseCallback : TProc;
begin
  AboutDialog := TAboutDialog.Create;
  AboutDialog.Setup(DialogDisplay.GetDisplayArea);

  CloseCallback := procedure
  begin
    AboutDialog := nil;
  end;

  DialogDisplay.Show(true, CloseCallback);
end;

procedure TMainMenu.EventHandle_EditSampleMap(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  if Plugin.GuiState.IsSampleMapVisible
    then Plugin.Globals.SendWindowsMessage(UM_HIDE_SAMPLE_MAP_EDIT)
    else Plugin.Globals.SendWindowsMessage(UM_SHOW_SAMPLE_MAP_EDIT);
end;

procedure TMainMenu.EventHandle_EditSamplePoints(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Plugin.Globals.SendWindowsMessage(UM_SHOW_LOOP_EDIT_FRAME);
end;



procedure TMainMenu.EventHandle_ImportSFZ(Sender: TObject);
var
  OpenDialog : TFileOpenDialog;
begin
  OpenDialog := TFileOpenDialog.Create(nil);
  AutoFree(@OpenDialog);

  SetupFileOpenDialog(OpenDialog, TDialogTarget.dtSfzProgram);

  OpenDialog.FileName := ''; //TODO:

  if OpenDialog.Execute then
  begin
    Plugin.ImportProgram(OpenDialog.FileName, TProgramFormat.Sfz);
  end;
end;

end.
