unit uFileBrowserFrame;

interface

uses
  VamLib.UniqueID,
  VamLib.Debouncer,
  VamLib.ZeroObject,
  uLucidityEnums,
  Lucidity.Interfaces,
  Menu.FileTreeMenu,
  eeFileBrowserAddon, uConstants, eePlugin, eeGuiStandardv2,
  VamVisibleControl,
  Winapi.Windows, Winapi.Messages, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamCustomTreeView, VamTreeView,
  VamScrollBox, RedFoxGraphicControl, VamGraphicControl, VamLabel, VamDiv,
  VamKnob, VamButton;

type
  TFileBrowserFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    ScrollBox: TVamScrollBox;
    FileTreeView: TVamTreeView;
    LowerPanel: TVamDiv;
    TextInfoContainer: TVamDiv;
    PreviewInfo3: TVamLabel;
    PreviewInfo1: TVamLabel;
    PreviewInfo2: TVamLabel;
    PreviewControlDiv: TVamDiv;
    PreviewVolumeKnob: TVamKnob;
    PreviewControlLabel: TVamLabel;
    PreviewOnOffButton: TVamButton;
    InsidePanel: TVamPanel;
    procedure FileTreeViewScrollXChange(Sender: TObject);
    procedure FileTreeViewScrollYChange(Sender: TObject);
    procedure ScrollBoxScroll(Sender: TObject; Kind: TScrollEventKind; ScrollPos: Single);
    procedure FileTreeViewNodeRightClicked(Sender: TObject; Node: TVamTreeViewNode);
    procedure FileTreeViewTreeRightClicked(Sender: TObject);
    procedure PreviewOnOffButtonChanged(Sender: TObject);
    procedure PreviewControlLabelClick(Sender: TObject);
    procedure FileTreeViewOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data: IVamDragData);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    IsManualScroll : boolean;
    FileBrowserAddon : TFileBrowserAddon;

    PreviewDebounceID : TUniqueID;

    MainContextMenu : TFileTreeViewMainContextMenu;
    NodeContextMenu : TFileTreeViewNodeContextMenu;

    PreviewInfo_ShowVolume : boolean;

    procedure EventHandle_NodeFocusChanged(Sender : TObject);
    procedure EventHandle_FilterNodes(Sender : TObject; const RootDir : string; var FolderNodes, FileNodes : TStringList);
    procedure EventHandle_GetNodeBitmap(Sender : TObject; const NodeFileName : string; var Bitmap : TBitmap);

    property Plugin      : TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;

    procedure Command_ReplaceLoad;
    procedure RefreshFileBrowser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);

    procedure KeyCommand(Command:TKeyCommand);

    procedure PreviewInfoChanged;
    procedure SampleDirectoriesChanged;
  end;

implementation

uses
  FileCtrl, SysUtils,
  VamLayoutWizard,
  uGuiUtils,
  Lucidity.PluginParameters,
  eeFunctions, uLucidityExtra,
  Lucidity.SampleMap, RedFoxColor;

{$R *.dfm}

constructor TFileBrowserFrame.Create(AOwner: TComponent);
begin
  inherited;

  PreviewDebounceID.Init;

  FileBrowserAddon := TFileBrowserAddon.Create(FileTreeView);
  FileBrowserAddOn.OnNodeFocusChanged := EventHandle_NodeFocusChanged;
  FileBrowserAddOn.OnFilterNodes      := EventHandle_FilterNodes;
  FileBrowserAddOn.OnGetNodeBitmap    := EventHandle_GetNodeBitmap;

  MainContextMenu := TFileTreeViewMainContextMenu.Create;
  NodeContextMenu := TFileTreeViewNodeContextMenu.Create;

  PreviewInfo_ShowVolume := false;
end;

destructor TFileBrowserFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  FileBrowserAddon.Free;
  MainContextMenu.Free;
  NodeContextMenu.Free;
  inherited;
end;

procedure TFileBrowserFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  CurrentFocus_ParName : string;
  b : boolean;
begin
  if MsgID = TLucidMsgID.SampleDirectoriesChanged then SampleDirectoriesChanged;
  if MsgID = TLucidMsgID.ProgramSavedToDisk       then RefreshFileBrowser;
  if MsgID = TLucidMsgID.PreviewInfoChanged       then PreviewInfoChanged;


  if (MsgID = TLucidMsgID.OnActiveParameterChanged) then
  begin
    CurrentFocus_ParName := string(Data^);
    if CurrentFocus_ParName = PluginParToName(TPluginParameter.PreviewVolume)
      then b := true
      else b := false;

    if PreviewInfo_ShowVolume <> b then
    begin
      PreviewInfo_ShowVolume := b;
      PreviewInfoChanged;
    end;
  end;

  if (PreviewInfo_ShowVolume) and (MsgID = TLucidMsgID.Command_UpdateScope) then
  begin
    // This is called here to update the preview volume text.
    PreviewInfoChanged;
  end;



end;




procedure TFileBrowserFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');
  assert(assigned(aPlugin));
  assert(assigned(aGuiStandard));

  fPlugin      := aPlugin;
  fGuiStandard := aGuiStandard;

  //==== Some basic GUI setup ====
  ScrollBox.Align := alClient;

  TextInfoContainer.Align := alClient;

  LowerPanel.Height := 60 + LowerPanel.Padding.Top + LowerPanel.Padding.Bottom;
  //LowerPanel.Height := 94 + LowerPanel.Padding.Top + LowerPanel.Padding.Bottom;

  PreviewControlDiv.Width := 80;

  PreviewVolumeKnob.Top := 0;
  PreviewVolumeKnob.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight);
  PreviewVolumeKnob.Layout.SnapToParentEdge(TControlFeature.RightEdge).Move(-7,4);




  //==== Parameters ====
  GuiStandard_RegisterControl(aGuiStandard, PreviewVolumeKnob,         TPluginParameter.PreviewVolume);



  // TODO: NOTE: Setting the color here doesn't work correctly in Win64.
  //ScrollBox.Color_Border     := GetRedFoxColor(kPanelVeryDark);
  //ScrollBox.Color_Background := GetRedFoxColor(kPanelDark);
  //ScrollBox.Color_Foreground := GetRedFoxColor(kPanelLight);


  MainContextMenu.Initialize(aPlugin);
  NodeContextMenu.Initialize(aPlugin);
  //========================================


  PreviewOnOffButton.Text := '';
  PreviewOnOffButton.Layout.SetSize(18,18);
  PreviewOnOffButton.Color_Border := kColor_ToggleButtonBorder;
  PreviewOnOffButton.ColorOnA     := kColor_ToggleButtonOn;
  PreviewOnOffButton.ColorOnB     := kColor_ToggleButtonOnMouseOver;
  PreviewOnOffButton.ColorOffA    := kColor_ToggleButtonOff;
  PreviewOnOffButton.ColorOffB    := kColor_ToggleButtonOffMouseOver;
  PreviewOnOffButton.ImageOn      := Plugin.Globals.SkinImageLoader.GetImage('Preview_Icon');
  PreviewOnOffButton.ImageOff     := Plugin.Globals.SkinImageLoader.GetImage('Preview_Icon');

  PreviewOnOffButton.Layout.Anchor(PreviewVolumeKnob).SnapToEdge(TControlFeature.LeftEdge).Move(0,0);

  //======================================
  RefreshFileBrowser; //Update some GUI controls...
  PreviewInfoChanged; //Update some GUI controls...
end;

procedure TFileBrowserFrame.SampleDirectoriesChanged;
var
  c1 : integer;
  Name, Path : string;
begin
  if not assigned(Plugin) then exit;

  Plugin.SampleDirectories.RefreshDirectoryList;

  FileBrowserAddon.ClearRootNodes;

  for c1 := 0 to Plugin.SampleDirectories.Count-1 do
  begin
    Name := Plugin.SampleDirectories[c1].Name;
    Path := Plugin.SampleDirectories[c1].Path;

    if (Name <> '') and (DirectoryExists(Path)) then
    begin
      FileBrowserAddon.AddRootNode(Path, Name);
    end;
  end;

  FileBrowserAddon.UpdateRootNodes;
end;




procedure TFileBrowserFrame.FileTreeViewScrollXChange(Sender: TObject);
var
  range : single;
begin
  if IsManualScroll then exit;

  ScrollBox.ScrollXPos := FileTreeView.ScrollPosX;
  ScrollBox.ScrollYPos := (1 - FileTreeView.ScrollPosY);

  ScrollBox.ScrollXIndexSize := FileTreeView.ScrollRangeX;

  range := FileTreeView.ScrollRangeY;
  if Range < 0.1 then Range := 0.1;
  ScrollBox.ScrollYIndexSize := range;
end;

procedure TFileBrowserFrame.FileTreeViewScrollYChange(Sender: TObject);
var
  range : single;
begin
  if IsManualScroll then exit;

  ScrollBox.ScrollXPos := FileTreeView.ScrollPosX;
  ScrollBox.ScrollYPos := (1 - FileTreeView.ScrollPosY);

  ScrollBox.ScrollXIndexSize := FileTreeView.ScrollRangeX;

  range := FileTreeView.ScrollRangeY;
  if range < 0.1 then range := 0.1;
  ScrollBox.ScrollYIndexSize := range;
end;

procedure TFileBrowserFrame.ScrollBoxScroll(Sender: TObject; Kind: TScrollEventKind; ScrollPos: Single);
begin
  IsManualScroll := true;

  try
    FileTreeView.ScrollPosX := ScrollBox.ScrollXPos;
    FileTreeView.ScrollPosY := (1-ScrollBox.ScrollYPos);
    FileTreeView.Invalidate;
  finally
    IsManualScroll := false;
  end;
end;

procedure TFileBrowserFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMothership;
end;

procedure TFileBrowserFrame.KeyCommand(Command: TKeyCommand);
begin
  if not assigned(Plugin) then exit;

  case Command of
    TKeyCommand.ContextUp:    FileBrowserAddOn.Command_BrowserUp;
    TKeyCommand.ContextDown:  FileBrowserAddOn.Command_BrowserDown;
    TKeyCommand.ContextLeft:  FileBrowserAddOn.Command_BrowserLeft;
    TKeyCommand.ContextRight: FileBrowserAddOn.Command_BrowserRight;
    TKeyCommand.PageUp:       FileBrowserAddOn.Command_PageUp;
    TKeyCommand.PageDown:     FileBrowserAddOn.Command_PageDown;
    TKeyCommand.SelectUp:     FileBrowserAddOn.Command_SelectUp;
    TKeyCommand.SelectDown:   FileBrowserAddOn.Command_SelectDown;
    TKeyCommand.ReplaceLoad:  Command_ReplaceLoad;
  else
    raise Exception.Create('Unexpected Command Value');
  end;
end;






procedure TFileBrowserFrame.PreviewInfoChanged;
var
  Text : string;
begin
  if not assigned(Plugin) then exit;

  if (Plugin.PreviewInfo^.IsSupported) and (Plugin.PreviewInfo^.IsValid) then
  begin
    PreviewInfo1.Text := Plugin.PreviewInfo^.FileName;

    if Plugin.PreviewInfo^.ChannelCount = '1'
      then PreviewInfo2.Text := Plugin.PreviewInfo^.SampleTime + '  MONO'
      else PreviewInfo2.Text := Plugin.PreviewInfo^.SampleTime + '  STEREO';

    PreviewInfo3.Text := Plugin.PreviewInfo^.SampleRate + ' hz' + '  ' + Plugin.PreviewInfo^.BitDepth + ' bit';
  end else
  if (Plugin.PreviewInfo^.IsSupported) and (Plugin.PreviewInfo^.IsValid = false) then
  begin
    PreviewInfo1.Text := Plugin.PreviewInfo^.FileName;
    PreviewInfo2.Text := 'Invalid Sample';
    PreviewInfo3.Text := '';
  end else
  begin
    PreviewInfo1.Text := '';
    PreviewInfo2.Text := '';
    PreviewInfo3.Text := '';
  end;


  if PreviewInfo_ShowVolume = false then
  begin
    if Plugin.IsPreviewEnabled
      then PreviewControlLabel.Text := 'PREVIEW ON'
      else PreviewControlLabel.Text := 'PREVIEW OFF';
  end else
  begin
    Text := 'VOLUME ' + IntToStr(round(Plugin.PreviewVolume * 100)) + '%';
    PreviewControlLabel.Text := Text;
  end;


  if PreviewOnOffButton.IsOn <> Plugin.IsPreviewEnabled
    then PreviewOnOffButton.IsOn := Plugin.IsPreviewEnabled;



end;



procedure TFileBrowserFrame.EventHandle_NodeFocusChanged(Sender: TObject);
var
  NodeData : PNodeData;
  Proc : TProc;
begin
  if not assigned(Plugin) then exit;

  Plugin.StopPreview;

  Proc := procedure
  begin
    NodeData := FileBrowserAddOn.GetFocusedNodeData;
    if assigned(NodeData) then
    begin
      if FileExists(NodeData.FileName)
        then Plugin.TriggerPreview(NodeData.FileName)
        else Plugin.ClearPreviewInfo;
    end;

  end;

  Debounce(PreviewDebounceID, 150, TDebounceEdge.deTrailing, Proc);
end;

procedure TFileBrowserFrame.EventHandle_FilterNodes(Sender: TObject; const RootDir: string; var FolderNodes, FileNodes: TStringList);
var
  c1 : integer;
  ext : string;
  DataFolderName : string;
  Index : integer;
  fn : string;
begin
  // Remove any program sample data folders from the folder listing.
  for c1 := 0 to FileNodes.Count-1 do
  begin
    ext := ExtractFileExt(FileNodes[c1]);
    if SameText(ext, '.lpg') then
    begin
      DataFolderName := RemoveFileExt(FileNodes[c1]) + ' Samples';
      Index := FolderNodes.IndexOf(DataFoldername);
      if Index <> -1 then FolderNodes.Delete(Index);
    end;
  end;


  // remove un-supported audio format files.
  for c1 := FileNodes.Count-1  downto 0 do
  begin
    fn := IncludeTrailingPathDelimiter(RootDir) + FileNodes[c1];
    if (IsSupportedAudioFormat(fn) = false) and (IsSupportedProgramFormat(fn) = false) then
    begin
      FileNodes.Delete(c1);
    end;
  end;

end;



procedure TFileBrowserFrame.Command_ReplaceLoad;
var
  NodeData : PNodeData;
  CurRegion : IRegion;
  NewRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  Plugin.StopPreview;

  NodeData := FileBrowserAddOn.GetFocusedNodeData;
  if (assigned(NodeData)) and (FileExists(NodeData.FileName)) then
  begin
    if IsSupportedAudioFormat(NodeData.FileName) then
    begin
      CurRegion := Plugin.FocusedRegion;

      if not assigned(CurRegion) then
      begin
        NewRegion := Plugin.SampleMap.LoadSample(NodeData.FileName, Plugin.FocusedKeyGroup);
      end else
      begin
        NewRegion := Plugin.SampleMap.ReplaceSample(NodeData.FileName, CurRegion);
      end;

      if assigned(NewRegion) then
      begin
        Plugin.FocusRegion(NewRegion.GetProperties^.UniqueID);
      end;
    end;


    if IsSupportedProgramFormat(NodeData.FileName) then
    begin
      Plugin.LoadProgramFromFile(NodeData.FileName);
    end;


  end;
end;

procedure TFileBrowserFrame.FileTreeViewNodeRightClicked(Sender: TObject; Node: TVamTreeViewNode);
var
  NodeData : PNodeData;
begin
  //called when a node is right clicked.
  NodeContextMenu.FocusedNode := Node;

  NodeData := FileBrowserAddOn.GetFocusedNodeData;
  if assigned(NodeData)
    then NodeContextMenu.NodeFileName := NodeData^.FileName
    else NodeContextMenu.NodeFileName := '';

  NodeContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFileBrowserFrame.FileTreeViewTreeRightClicked(Sender: TObject);
begin
  //called when the treeview is right clicked with no node under the cursor.
  MainContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;


procedure TFileBrowserFrame.EventHandle_GetNodeBitmap(Sender: TObject; const NodeFileName: string; var Bitmap: TBitmap);
var
  ext : string;
begin
  if not assigned(Plugin) then exit;

  ext := ExtractFileExt(NodeFileName);
  if SameText(ext, '') then
  begin
    Bitmap := Plugin.Globals.SkinImageLoader.GetImage('Browser_FolderIcon');
    exit; //=====================================================================>>exit>>========>>
  end;


  if IsSupportedAudioFormat(NodeFileName) then
  begin
    Bitmap := Plugin.Globals.SkinImageLoader.GetImage('Browser_AudioIcon');
    exit; //=====================================================================>>exit>>========>>
  end;


  if IsSupportedProgramFormat(NodeFileName) then
  begin
    Bitmap := Plugin.Globals.SkinImageLoader.GetImage('Browser_ProgramIcon');
    exit; //=====================================================================>>exit>>========>>
  end;








  //Plugin.Globals.
end;

procedure TFileBrowserFrame.PreviewOnOffButtonChanged(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  Plugin.IsPreviewEnabled := (Sender as TVamButton).IsOn;
  PreviewInfoChanged;
end;

procedure TFileBrowserFrame.PreviewControlLabelClick(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  // Toggle the preview enabled state.
  Plugin.IsPreviewEnabled := not Plugin.IsPreviewEnabled;
  PreviewInfoChanged;
end;

procedure TFileBrowserFrame.RefreshFileBrowser;
begin
  //TODO:
  // Here we need to refresh the file browser.
  // this is desired because it would be good if the browser refreshed after saving
  // a new program to disk. Currently it doesn't show up until the patch browser node
  // is closed and re-opended.

  SampleDirectoriesChanged;
end;

procedure TFileBrowserFrame.FileTreeViewOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data: IVamDragData);
var
  Files : TStringList;
  c1: Integer;
  fn : string;
  DirPath, DirName : string;
  Ext : string;
begin
  //=== Check drag and dropped files for directories. Add any directories to the browser =====
  Files := Data.GetFiles;
  if assigned(Files) then
  begin
    for c1 := 0 to Files.Count-1 do
    begin
      fn := Files[c1];
      Ext := ExtractFileExt(fn);
      if Ext = '' then
      begin
        fn := ExcludeTrailingPathDelimiter(fn);
        DirName := ExtractFileName(fn);
        DirPath := ExtractFileDir(fn);
        DirPath := IncludeTrailingPathDelimiter(DirPath) + DirName;
        Plugin.SampleDirectories.AddSampleDirectory(DirName, DirPath);
      end;
    end;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleDirectoriesChanged);
  end;
end;







end.
