unit Menu.SampleContextMenu;

interface

uses
  Lucidity.Interfaces,
  eePlugin, Vcl.Menus;


type
  TSampleContextMenu = class
  private
    fLoopPointsVisible: boolean;
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
    MouseDownSamplePos : integer;

    // TODO:HIGH CurrentRegion needs to be set to nil when the region is closed.
    CurrentRegion : IRegion;

    procedure EventHandler_RenameSampleFile(Sender : TObject);
    procedure EventHandler_ReloadSampleFile(Sender : TObject);
    procedure EventHandler_OpenWithUnknownApp(Sender : TObject);
    procedure EventHandler_OpenWith(Sender : TObject);
    procedure EventHandle_NormaliseSample(Sender : TObject);
    procedure EventHandle_ZoomSample(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ShowInWindowsExplorer(Sender : TObject);
    procedure EventHandle_ModulationCommand(Sender : TObject);
    procedure EventHandle_ClearAllModulationForAllSamplePoints(Sender : TObject);
    procedure EventHandle_ClearCurrentModulationForAllSamplePoints(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer; const aMouseDownSamplePos : integer; const aCurrentRegion : IRegion);

    property LoopPointsVisible : boolean read fLoopPointsVisible write fLoopPointsVisible;

  end;

implementation

uses
  Dialogs, //TODO:HIGH delete this.
  XPLAT.Dialogs,
  InWindowDialog,
  VamLib.Utils,
  uLucidityEnums,
  Lucidity.PluginParameters,
  Lucidity.Types,
  uGuiUtils,
  SysUtils,
  eeWinEx,
  Lucidity.SampleMap,
  uConstants;

const
  SampleStartTag = 1;
  SampleEndTag   = 2;
  LoopStartTag   = 3;
  LoopEndTag     = 4;

  Caption_MoveHere               = 'Move Here';
  Caption_ClearAllModulation     = 'Clear All Modulation';
  Caption_ClearCurrentModulation = 'Clear Current Modulation';

{ TSampleDisplayMenu }

constructor TSampleContextMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TSampleContextMenu.Destroy;
begin
  Menu.Free;
  CurrentRegion := nil;
  inherited;
end;

procedure TSampleContextMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

procedure TSampleContextMenu.Popup(const x, y: integer; const aMouseDownSamplePos : integer; const aCurrentRegion : IRegion);
var
  c1 : integer;
  mi : TMenuItem;
  Tag : integer;
  IsSampleStartModulated : boolean;
  IsSampleEndModulated   : boolean;
  IsLoopStartModulated   : boolean;
  IsLoopEndModulated     : boolean;
  IsAnythingModulated    : boolean;
begin
  CurrentRegion := aCurrentRegion;

  Menu.Items.Clear;

  MouseDownSamplePos := aMouseDownSamplePos;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom In';
  mi.Tag := 1;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom Out';
  mi.Tag := 2;
  if Plugin.Globals.GuiState.SampleDisplayZoom > 0
    then mi.Enabled := true
    else mi.Enabled := false;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom Out Full';
  mi.Tag := 3;
  if Plugin.Globals.GuiState.SampleDisplayZoom > 0
    then mi.Enabled := true
    else mi.Enabled := false;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Sample Start';
  mi.Tag := 4;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Sample End';
  mi.Tag := 5;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Loop Start';
  mi.Tag := 6;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Loop End';
  mi.Tag := 7;
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);


  //=== spacer ====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=====================


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Normalise Sample';
  mi.OnClick := EventHandle_NormaliseSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Reload Sample';
  mi.OnClick := EventHandler_ReloadSampleFile;
  Menu.Items.Add(mi);

  // Add menu commands for existing sound editors.
  for c1 := 0 to Plugin.Globals.Options.SoundEditors.Count-1 do
  begin
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Open With ' + Plugin.Globals.Options.SoundEditors[c1].ApplicationName + '...';
    mi.Tag     := c1;
    mi.OnClick := EventHandler_OpenWith;
    Menu.Items.Add(mi);
  end;

  // Add the Open With X command.
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Open With...';
  mi.OnClick := EventHandler_OpenWithUnknownApp;
  Menu.Items.Add(mi);



  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Show in Windows Exporer...';
  mi.OnClick := EventHandle_ShowInWindowsExplorer;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Rename Sample File...';
  mi.OnClick := EventHandler_RenameSampleFile;
  Menu.Items.Add(mi);





  {
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Map...';
  mi.OnClick := EventHandle_EditSampleMap;
  Menu.Items.Add(mi);
  }





  //=== spacer ====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=====================


  {
  Tag := SampleStartTag;
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Sample Start';
  Menu.Items.Add(mi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_MoveHere;
    ChildMi.Hint    := Caption_MoveHere;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearCurrentModulation;
    ChildMi.Hint    := Caption_ClearCurrentModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearAllModulation;
    ChildMi.Hint    := Caption_ClearAllModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);


  Tag := SampleEndTag;
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Sample End';
  Menu.Items.Add(mi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_MoveHere;
    ChildMi.Hint    := Caption_MoveHere;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearCurrentModulation;
    ChildMi.Hint    := Caption_ClearCurrentModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearAllModulation;
    ChildMi.Hint    := Caption_ClearAllModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);


  if LoopPointsVisible then
  begin

    Tag := LoopStartTag;
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Loop Start';
    Menu.Items.Add(mi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_MoveHere;
      ChildMi.Hint    := Caption_MoveHere;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearCurrentModulation;
      ChildMi.Hint    := Caption_ClearCurrentModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearAllModulation;
      ChildMi.Hint    := Caption_ClearAllModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);


    Tag := LoopEndTag;
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Loop End';
    Menu.Items.Add(mi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_MoveHere;
      ChildMi.Hint    := Caption_MoveHere;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearCurrentModulation;
      ChildMi.Hint    := Caption_ClearCurrentModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearAllModulation;
      ChildMi.Hint    := Caption_ClearAllModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);
  end;
  }

  //=== spacer ====
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=====================

  {
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Clear Current Modulation';
  mi.OnClick := EventHandle_ClearCurrentModulationForAllSamplePoints;
  Menu.Items.Add(mi);
  }




  IsSampleStartModulated := Command.IsParameterModulated(Plugin, 'SampleStart');
  IsSampleEndModulated   := Command.IsParameterModulated(Plugin, 'SampleEnd');
  IsLoopStartModulated   := Command.IsParameterModulated(Plugin, 'LoopStart');
  IsLoopEndModulated     := Command.IsParameterModulated(Plugin, 'LoopEnd');

  if IsSampleStartModulated or IsSampleEndModulated or IsLoopStartModulated or IsLoopEndModulated
    then IsAnythingModulated := true
    else IsAnythingModulated := false;


  Tag := SampleStartTag;
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Clear All Sample Start Modulation';
  mi.Hint    := Caption_ClearAllModulation;
  mi.Tag     := Tag;
  mi.OnClick := EventHandle_ModulationCommand;
  mi.Enabled := IsSampleStartModulated;
  Menu.Items.Add(mi);

  Tag := SampleEndTag;
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Clear All Sample End Modulation';
  mi.Hint    := Caption_ClearAllModulation;
  mi.Tag     := Tag;
  mi.OnClick := EventHandle_ModulationCommand;
  mi.Enabled := IsSampleEndModulated;
  Menu.Items.Add(mi);

  if LoopPointsVisible then
  begin
    Tag := LoopStartTag;
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Clear All Loop Start Modulation';
    mi.Hint    := Caption_ClearAllModulation;
    mi.Tag     := Tag;
    mi.OnClick := EventHandle_ModulationCommand;
    mi.Enabled := IsLoopStartModulated;
    Menu.Items.Add(mi);

    Tag := LoopEndTag;
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Clear All Loop End Modulation';
    mi.Hint    := Caption_ClearAllModulation;
    mi.Tag     := Tag;
    mi.OnClick := EventHandle_ModulationCommand;
    mi.Enabled := IsLoopEndModulated;
    Menu.Items.Add(mi);
  end;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Clear All Modulation';
  mi.OnClick := EventHandle_ClearAllModulationForAllSamplePoints;
  mi.Enabled := IsAnythingModulated;
  Menu.Items.Add(mi);


  Menu.Popup(x, y);
end;

procedure TSampleContextMenu.EventHandle_EditSampleMap(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleMapVisibility(Plugin);
end;

procedure TSampleContextMenu.EventHandle_ZoomSample(Sender: TObject);
var
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;



  case Tag of
    1:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomIn, @MouseDownSamplePos, nil);
    2:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomOut, @MouseDownSamplePos, nil);
    3:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomOutFull, @MouseDownSamplePos, nil);
    4:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomToSampleStart, @MouseDownSamplePos, nil);
    5:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomToSampleEnd, @MouseDownSamplePos, nil);
    6:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomToLoopStart, @MouseDownSamplePos, nil);
    7:Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_Sample_ZoomToLoopEnd, @MouseDownSamplePos, nil);
  else
    raise Exception.Create('Index not handled.');
  end;


end;

procedure TSampleContextMenu.EventHandle_NormaliseSample(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.NormaliseSamples(Plugin);
end;

procedure TSampleContextMenu.EventHandle_ShowInWindowsExplorer(Sender: TObject);
var
  Region : IRegion;
  fn : string;
begin
  if not assigned(Plugin) then exit;

  Region := CurrentRegion;

  if assigned(Region) then
  begin
    fn := Region.GetProperties^.SampleFileName;
    if FileExists(Fn) then
    begin
      OpenFolderAndSelectFile(Fn);
    end;

  end;
end;

procedure TSampleContextMenu.EventHandle_ModulationCommand(Sender: TObject);
var
  Tag : integer;
  ModParIndex : integer;
  Caption : string;
  SampleMarker : TSampleMarker;
begin
  Tag := (Sender as TMenuItem).Tag;

  case Tag of
    SampleStartTag : ModParIndex := GetModParIndex(TPluginParameter.SampleStart);
    SampleEndTag   : ModParIndex := GetModParIndex(TPluginParameter.SampleEnd);
    LoopStartTag   : ModParIndex := GetModParIndex(TPluginParameter.LoopStart);
    LoopEndTag     : ModParIndex := GetModParIndex(TPluginParameter.LoopEnd);
  else
    raise Exception.Create('Type not handled.');
  end;

  case Tag of
    SampleStartTag : SampleMarker := TSampleMarker.smSampleStartMarker;
    SampleEndTag   : SampleMarker := TSampleMarker.smSampleEndMarker;
    LoopStartTag   : SampleMarker := TSampleMarker.smLoopStartMarker;
    LoopEndTag     : SampleMarker := TSampleMarker.smLoopEndMarker;
  else
    raise Exception.Create('Type not handled.');
  end;

  Caption := (Sender as TMenuItem).Hint;

  if Caption = Caption_MoveHere then
  begin
    Command.MoveSampleMarker(Plugin, SampleMarker, MouseDownSamplePos);
  end;

  if Caption = Caption_ClearCurrentModulation then
  begin
    Command.ClearCurrentModulationForParameter_OLD(Plugin, ModParIndex);
  end;

  if Caption = Caption_ClearAllModulation then
  begin
    Command.ClearAllModulationForParameter_OLD(Plugin, ModParIndex);
  end;



end;


procedure TSampleContextMenu.EventHandler_OpenWith(Sender: TObject);
var
  fn : string;
  Tag : integer;
  SoundEditorApp : string;
begin
  fn := CurrentRegion.GetProperties^.SampleFileName;
  if FileExists(fn) = false then
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, '"' + fn + '" is not found.');
    exit;
  end;

  Tag := (Sender as TMenuItem).Tag;

  SoundEditorApp := Plugin.Globals.Options.SoundEditors[Tag].ApplicationExe;

  if FileExists(SoundEditorApp) then
  begin
    ShellOpenFileWith(fn, SoundEditorApp);
  end else
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, '"' + SoundEditorApp + '" is not found.');
  end;
end;

procedure TSampleContextMenu.EventHandler_OpenWithUnknownApp(Sender: TObject);
var
  fn : string;
  OD : TxpFileOpenDialog;
  SoundEditorApp : string;
begin
  fn := CurrentRegion.GetProperties^.SampleFileName;
  if FileExists(fn) = false then
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, '"' + fn + '" is not found.');
    exit;
  end;

  OD := TxpFileOpenDialog.Create(nil);
  AutoFree(@OD);

  OD.Filter := 'Executable|*.exe';

  if od.Execute then
  begin
    SoundEditorApp := od.FileName;
    Plugin.Globals.Options.AddNewSoundEditor(SoundEditorApp);
    ShellOpenFileWith(fn, SoundEditorApp);
  end;
end;

procedure TSampleContextMenu.EventHandle_ClearAllModulationForAllSamplePoints(Sender: TObject);
var
  ModParIndex : integer;
begin
  ModParIndex := GetModParIndex(TPluginParameter.SampleStart);
  Command.ClearAllModulationForParameter_OLD(Plugin, ModParIndex);

  ModParIndex := GetModParIndex(TPluginParameter.SampleEnd);
  Command.ClearAllModulationForParameter_OLD(Plugin, ModParIndex);

  ModParIndex := GetModParIndex(TPluginParameter.LoopStart);
  Command.ClearAllModulationForParameter_OLD(Plugin, ModParIndex);

  ModParIndex := GetModParIndex(TPluginParameter.LoopEnd);
  Command.ClearAllModulationForParameter_OLD(Plugin, ModParIndex);
end;

procedure TSampleContextMenu.EventHandle_ClearCurrentModulationForAllSamplePoints(Sender: TObject);
var
  ModParIndex : integer;
begin
  ModParIndex := GetModParIndex(TPluginParameter.SampleStart);
  Command.ClearCurrentModulationForParameter_OLD(Plugin, ModParIndex);

  ModParIndex := GetModParIndex(TPluginParameter.SampleEnd);
  Command.ClearCurrentModulationForParameter_OLD(Plugin, ModParIndex);

  ModParIndex := GetModParIndex(TPluginParameter.LoopStart);
  Command.ClearCurrentModulationForParameter_OLD(Plugin, ModParIndex);

  ModParIndex := GetModParIndex(TPluginParameter.LoopEnd);
  Command.ClearCurrentModulationForParameter_OLD(Plugin, ModParIndex);
end;

procedure TSampleContextMenu.EventHandler_ReloadSampleFile(Sender: TObject);
begin
  if not assigned(CurrentRegion) then exit;
  Plugin.ReloadRegion(CurrentRegion);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
end;

procedure TSampleContextMenu.EventHandler_RenameSampleFile(Sender: TObject);
var
  InputBox : TxpInputBox;
  fn : string;
  Dir : string;
  extA : string;
  ExtB : string;

  OldFileName : string;
  NewFileName : string;
begin
  if not assigned(CurrentRegion) then exit;

  InputBox := TxpInputBox.Create(nil);
  AutoFree(@InputBox);

  OldFileName := CurrentRegion.GetProperties^.SampleFileName;

  fn := ExtractFileName(CurrentRegion.GetProperties^.SampleFileName);
  extA := ExtractFileExt(CurrentRegion.GetProperties^.SampleFileName);
  Dir  := ExtractFileDir(CurrentRegion.GetProperties^.SampleFileName);

  InputBox.Caption := 'Rename File';
  InputBox.Prompt  := 'Rename File';
  InputBox.InitialValue := fn;

  if (InputBox.Execute) and (InputBox.ResultText <> '') then
  begin
    fn := IncludeTrailingPathDelimiter(Dir) + InputBox.ResultText;
    extB := ExtractFileExt(fn);
    if SameText(extA, extB) = false then
    begin
      fn := fn + extA;
    end;
    NewFileName := fn;

    if FileExists(NewFileName) then
    begin
      // TODO:MED it would be nice to send an error message to
      // the GUI instead of using a modal dialog box. Sometimes
      // these modal dialog boxes make the plugin look like it has
      // hung.
      InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Error: "' + NewFileName + '" already exists."');
      exit;
    end;



    Plugin.RenameInUseSample(NewFileName, OldFileName);
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
end;







end.
