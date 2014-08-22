unit Menu.FileTreeMenu;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  eeWinEx,
  VamTreeView, eeFileBrowserAddon,
  eePlugin, Vcl.Menus;


type
  // MainContextMenu shows when the tree view background is context clicked.
  TFileTreeViewMainContextMenu = class
  private
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
    procedure MenuItemClicked(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer);
  end;

  // NodeContextMenu shows when a tree view node is context clicked.
  TFileTreeViewNodeContextMenu = class
  private
    fFocusedNode: TVamTreeViewNode;
    fNodeFileName: string;
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
    procedure MenuItemClicked(Sender : TObject);

    procedure EventHandler_OpenWithUnknownApp(Sender : TObject);
    procedure EventHandler_OpenWith(Sender : TObject);
    procedure EventHandler_RenameRootNode(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    property FocusedNode : TVamTreeViewNode read fFocusedNode write fFocusedNode;
    property NodeFileName : string read fNodeFileName write fNodeFileName;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer);
  end;

implementation

uses
  XPLAT.Dialogs,
  uLucidityExtra,
  SysUtils,
  uAutoFree,
  Dialogs,
  uConstants;



{ TFileTreeViewContextMenu }

constructor TFileTreeViewMainContextMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TFileTreeViewMainContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TFileTreeViewMainContextMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

procedure TFileTreeViewMainContextMenu.MenuItemClicked(Sender: TObject);
var
  Tag : integer;
  OD : TxpBrowserSelectDialog;
  DirName, DirPath : string;
begin
  assert(Sender is TMenuItem);

  Tag := (Sender as TMenuItem).Tag;

  if Tag = 1 then
  begin
    OD := TxpBrowserSelectDialog.Create(nil);
    AutoFree(@OD);
    if OD.Execute then
    begin
      DirPath := OD.FileName;
      DirName := ExtractFileName(OD.FileName);

      Plugin.SampleDirectories.AddSampleDirectory(DirName, DirPath);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
    end;
  end;
end;

procedure TFileTreeViewMainContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 1;
  mi.Caption := 'Add Directory...';
  mi.OnClick := MenuItemClicked;
  Menu.Items.Add(mi);

  Menu.Popup(x, y);
end;

{ TFileTreeViewNodeContextMenu }

constructor TFileTreeViewNodeContextMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TFileTreeViewNodeContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TFileTreeViewNodeContextMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

procedure TFileTreeViewNodeContextMenu.MenuItemClicked(Sender: TObject);
var
  Tag : integer;
  NodeData  : PNodeData;
  OD : TxpBrowserSelectDialog;
  DirName, DirPath : string;
begin
  assert(Sender is TMenuItem);

  Tag := (Sender as TMenuItem).Tag;

  //remove sample directory.
  if Tag = 1 then
  begin
    Plugin.SampleDirectories.RemoveSampleDirectory(FocusedNode.NodeIndex);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
  end;

  //show in windows explorer
  if Tag = 2 then
  begin
    NodeData := FocusedNode.Data;
    OpenFolderAndSelectFile(NodeData^.FileName);
  end;

  //move up
  if Tag = 3 then
  begin
    Plugin.SampleDirectories.MoveDirectoryUp(FocusedNode.NodeIndex);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
  end;

  //move down
  if Tag = 4 then
  begin
    Plugin.SampleDirectories.MoveDirectoryDown(FocusedNode.NodeIndex);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
  end;

  if Tag = 5 then
  begin
    OD := TxpBrowserSelectDialog.Create(nil);
    AutoFree(@OD);

    // NOTE: Using file open dialog to select folders.
    // - http://stackoverflow.com/a/7422764/395461
    if OD.Execute then
    begin
      DirPath := OD.FileName;
      DirName := ExtractFileName(OD.FileName);

      Plugin.SampleDirectories.AddSampleDirectory(DirName, DirPath);

      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
    end;
  end;

end;

procedure TFileTreeViewNodeContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
  c1: Integer;
begin
  Menu.Items.Clear;

  if (assigned(FocusedNode)) and (FocusedNode.IsRootNode) then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Tag     := 5;
    mi.Caption := 'Add Directory...';
    mi.OnClick := MenuItemClicked;
    Menu.Items.Add(mi);
  end;

  if (assigned(FocusedNode)) and (FocusedNode.IsRootNode) then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Tag     := 1;
    mi.Caption := 'Remove Directory';
    mi.OnClick := MenuItemClicked;
    Menu.Items.Add(mi);
  end;

  if (assigned(FocusedNode)) and (FocusedNode.IsRootNode) then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Tag     := 1;
    mi.Caption := 'Rename...';
    mi.OnClick := EventHandler_RenameRootNode;
    Menu.Items.Add(mi);
  end;

  if (assigned(FocusedNode)) and (FocusedNode.IsRootNode) then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Tag     := 3;
    mi.Caption := 'Move Up';
    mi.OnClick := MenuItemClicked;
    Menu.Items.Add(mi);
  end;

  if (assigned(FocusedNode)) and (FocusedNode.IsRootNode) then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Tag     := 4;
    mi.Caption := 'Move Down';
    mi.OnClick := MenuItemClicked;
    Menu.Items.Add(mi);
  end;

  if (assigned(FocusedNode)) and (NodeFileName <> '') and (FileExists(NodeFilename)) and (IsSupportedAudioFormat(NodeFilename)) then
  begin
    // Add menu commands for existing sound editors.
    for c1 := 0 to Plugin.Globals.Options.SoundEditors.Count-1 do
    begin
      mi := TMenuItem.Create(Menu);
      mi.Caption := 'Open With ' + Plugin.Globals.Options.SoundEditors[c1].ApplicationName;
      mi.Tag     := c1;
      mi.OnClick := EventHandler_OpenWith;
      Menu.Items.Add(mi);
    end;

    // Add the Open With X command.
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Open With...';
    mi.OnClick := EventHandler_OpenWithUnknownApp;
    Menu.Items.Add(mi);
  end;

  if (assigned(FocusedNode)) then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Tag     := 2;
    mi.Caption := 'Show in Windows Exporer...';
    mi.OnClick := MenuItemClicked;
    Menu.Items.Add(mi);
  end;





  Menu.Popup(x, y);
end;


procedure TFileTreeViewNodeContextMenu.EventHandler_OpenWith(Sender: TObject);
var
  Tag : integer;
  SoundEditorApp : string;
begin
  Tag := (Sender as TMenuItem).Tag;

  SoundEditorApp := Plugin.Globals.Options.SoundEditors[Tag].ApplicationExe;

  if FileExists(SoundEditorApp) then
  begin
    ShellOpenFileWith(NodeFileName, SoundEditorApp);
  end else
  begin
    ShowMessage('"' + SoundEditorApp + '" is not found.');
  end;
end;

procedure TFileTreeViewNodeContextMenu.EventHandler_OpenWithUnknownApp(Sender: TObject);
var
  OD : TxpFileOpenDialog;
  SoundEditorApp : string;
begin
  OD := TxpFileOpenDialog.Create(nil);
  AutoFree(@OD);

  OD.Filter := 'Executable|*.exe';

  if od.Execute then
  begin
    SoundEditorApp := od.FileName;
    Plugin.Globals.Options.AddNewSoundEditor(SoundEditorApp);
    ShellOpenFileWith(NodeFileName, SoundEditorApp);
  end;
end;



procedure TFileTreeViewNodeContextMenu.EventHandler_RenameRootNode(Sender: TObject);
var
  InputBox : TxpInputBox;
begin
  if not (assigned(FocusedNode)) then exit;

  InputBox := TxpInputBox.Create(nil);
  AutoFree(@InputBox);

  InputBox.Caption := 'Rename Directory';
  InputBox.Prompt  := 'Rename Directory';
  InputBox.InitialValue := FocusedNode.Caption;

  if (InputBox.Execute) and (InputBox.ResultText <> '') then
  begin
    Plugin.SampleDirectories.RenameSampleDirectory(FocusedNode.NodeIndex, InputBox.ResultText);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
  end;
end;

end.
