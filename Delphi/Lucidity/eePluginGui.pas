unit eePluginGui;

interface

{$INCLUDE Defines.inc}


uses
  VamLib.ZeroObject,
  uSequencerFrame,
  OtlComm, uModSystem2Frame,
  uAboutFrame, Lucidity.SampleMap, uDialogDisplayArea, uInfoBarFrame,
  eeEnumHelper, uLucidityEnums,
  DAEffect, DAEffectX, eePluginKeyHook,
  uGuiState, eePluginHotkeys,
  uConstants, uGuiFeedbackData,
  uSampleMapFrame, uFileBrowserFrame, uSampleDisplayFrame, uVoiceControlFrame,
  uMenuBarFrame,
  uModControlFrame, uMiniSampleDisplayFrame,
  eeFileBrowserAddon, eeRedFoxDropFileTarget,
  eePlugin, eeGuiStandard,
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RedFoxContainer, RedFoxWinControl, VamWinControl, VamPanel, VamDiv,
  VamScrollBox, VamCustomTreeView, VamTreeView, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamTabs, VamTabPanel, Vcl.ExtCtrls, VamScrollBar,
  VamMemo;



type
  TPluginGui = class(TForm, IZeroObject)
    RedFoxContainer: TRedFoxContainer;
    MainWorkArea: TVamDiv;
    MainTop: TVamDiv;
    MainMenuBar: TVamDiv;
    SampleMapDiv: TVamDiv;
    MainPanel: TVamDiv;
    SideWorkArea: TVamDiv;
    TitlePanel: TVamPanel;
    VamLabel1: TVamLabel;
    VoiceControlDiv: TVamDiv;
    TabPanel: TVamTabPanel;
    SidePanel: TVamDiv;
    InfoBarDiv: TVamDiv;
    SpacerPanel1: TVamPanel;
    ModSystem2Div: TVamDiv;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LowerTabsChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fPlugin: TeePlugin;
    fLowerTabState: TLowerTabOptions;
    fCurrentGuiState: TGuiState;
    fPluginHotkeys: TPluginHotkeys;
    fPluginKeyHook: TPluginKeyHook;
    procedure SetLowerTabState(const Value: TLowerTabOptions);
  private
    FMotherShip : IMothership;
    function GetMotherShipReference:IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    Manually:boolean;
    GuiStandard : TGuiStandard;
    DialogDisplayArea : TDialogDisplayArea;
    VstWindow : Hwnd;
    DropFileTarget : TRedFoxDropFileTarget;

    MenuBarFrame           : TMenuBarFrame;
    SampleDisplayFrame     : TSampleDisplayFrame;
    MiniSampleDisplayFrame : TMiniSampleDisplayFrame;
    SampleMapFrame         : TSampleMapFrame;
    FileBrowserFrame       : TFileBrowserFrame;
    VoiceControlFrame      : TVoiceControlFrame;
    ModControlFrame        : TModControlFrame;
    InfoBarFrame           : TInfoBarFrame;
    AboutFrame             : TAboutFrame;
    ModSystem2Frame        : TModSystem2Frame;
    SequencerFrame         : TSequencerFrame;

    FeedbackData : TGuiFeedBackData;

    OverlayContainer : TVamPanel;

    procedure ShowSampleMapEdit;
    procedure HideSampleMapEdit;
    procedure ShowAboutDialog;
    procedure ShowLoopEditFrame;
    procedure CloseCurrentDialog;

    property LowerTabState : TLowerTabOptions read fLowerTabState write SetLowerTabState;
    property CurrentGuiState : TGuiState read fCurrentGuiState write fCurrentGuiState;

    procedure HotkeyEvent(Sender : TObject; const CommandID : string);

    procedure OverlayContainerClicked(Sender : TObject);

    function DoGetDialogArea(Sender : TObject):TWinControl;
    procedure DoShowDialogArea(Sender : TObject);
    procedure DoHideDialogArea(Sender : TObject);


    procedure EventHandle_ControlMouseDown(Sender: TObject; const Target:TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Block : boolean);
  public
    procedure PostCreate(const aVstWindow : HWnd);
    procedure BeforeClose;
    procedure UpdateGui(Sender:TObject);

    property Plugin:TeePlugin read fPlugin write fPlugin;
    property PluginHotkeys : TPluginHotkeys read fPluginHotkeys write fPluginHotkeys;
    property PluginKeyHook : TPluginKeyHook read fPluginKeyHook write fPluginKeyHook;

    procedure UpdateLayout;

  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eePluginDataDir,
  uLoopEditDialog,
  uLucidityKeyGroup,
  GuidEx,
  RedFoxColor,
  eeGuiSetup,
  uGuiUtils,
  uLucidityData,
  SysUtils,
  VamQuery,
  VamKnob;




{$R *.dfm}


{ TPluginGui }

procedure TPluginGui.FormCreate(Sender: TObject);
const
  kScrollPanelWidth = 635;
begin
  DialogDisplayArea := TDialogDisplayArea.Create;
  DialogDisplayArea.OnShowDialogArea := DoShowDialogArea;
  DialogDisplayArea.OnHideDialogArea := DoHideDialogArea;
  DialogDisplayArea.OnGetDisplayArea := DoGetDialogArea;


  OverlayContainer := TVamPanel.Create(self);
  OverlayContainer.HitTest := true;
  OverlayContainer.Parent := RedFoxContainer;
  OverlayContainer.Visible := false;
  OverlayContainer.Left := 0;
  OverlayContainer.Top  := 0;
  OverlayContainer.Width :=  self.Width;
  OverlayContainer.Height := self.Height;
  OverlayContainer.Color := '$cc000000';
  OverlayContainer.OnClick := OverlayContainerClicked;

  PluginHotkeys := TPluginHotkeys.Create;
  PluginHotkeys.OnCommandKeyDown := self.HotkeyEvent;

  FeedbackData := TGuiFeedBackData.Create;

  CurrentGuiState := TGuiState.Create;


  //====== Initialize all frames ==============
  MenuBarFrame := TMenuBarFrame.Create(self.Owner);
  MenuBarFrame.BackgroundPanel.Parent := MainMenuBar;
  MenuBarFrame.BackgroundPanel.Align  := TAlign.alClient;
  MenuBarFrame.BackgroundPanel.Visible  := true;

  SampleMapFrame := TSampleMapFrame.Create(self.Owner);
  SampleMapFrame.BackgroundPanel.Parent := SampleMapDiv;
  SampleMapFrame.BackgroundPanel.Align  := TAlign.alClient;
  SampleMapFrame.BackgroundPanel.Visible  := true;

  SampleDisplayFrame := TSampleDisplayFrame.Create(self.Owner);
  SampleDisplayFrame.BackgroundPanel.Parent := MainTop;
  SampleDisplayFrame.BackgroundPanel.Align  := alClient;
  SampleDisplayFrame.BackgroundPanel.Visible  := false;

  MiniSampleDisplayFrame := TMiniSampleDisplayFrame.Create(self.Owner);
  MiniSampleDisplayFrame.BackgroundPanel.Parent := MainTop;
  MiniSampleDisplayFrame.BackgroundPanel.Align := alClient;
  MiniSampleDisplayFrame.BackgroundPanel.Padding.SetBounds(0,0,0,0);
  MiniSampleDisplayFrame.BackgroundPanel.Margins.SetBounds(0,0,0,0);
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius1 := 3;
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius2 := 3;
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius3 := 3;
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius4 := 3;

  VoiceControlFrame := TVoiceControlFrame.Create(self.Owner);
  VoiceControlFrame.BackgroundPanel.Parent := VoiceControlDiv;
  VoiceControlFrame.BackgroundPanel.Align := alClient;
  VoiceControlFrame.BackgroundPanel.Padding.SetBounds(0,0,0,0);
  VoiceControlFrame.BackgroundPanel.Margins.SetBounds(0,0,0,0);
  VoiceControlFrame.BackgroundPanel.CornerRadius1 := 3;
  VoiceControlFrame.BackgroundPanel.CornerRadius2 := 3;
  VoiceControlFrame.BackgroundPanel.CornerRadius3 := 3;
  VoiceControlFrame.BackgroundPanel.CornerRadius4 := 3;

  ModControlFrame := TModControlFrame.Create(self.Owner);
  ModControlFrame.BackgroundPanel.Parent := TabPanel;
  ModControlFrame.BackgroundPanel.Align  := alClient;
  ModControlFrame.BackgroundPanel.Padding.SetBounds(0,0,0,0);
  ModControlFrame.BackgroundPanel.Margins.SetBounds(0,0,0,0);
  ModControlFrame.BackgroundPanel.CornerRadius1 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius2 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius3 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius4 := 3;
  ModControlFrame.BackgroundPanel.Visible := false;



  InfoBarFrame := TInfoBarFrame.Create(self.Owner);
  InfoBarFrame.BackgroundPanel.Parent := InfoBarDiv;
  InfoBarFrame.BackgroundPanel.Align  := alClient;
  InfoBarFrame.BackgroundPanel.Visible := true;

  FileBrowserFrame := TFileBrowserFrame.Create(self.Owner);
  FileBrowserFrame.BackgroundPanel.Parent := SidePanel;
  FileBrowserFrame.BackgroundPanel.Align  := alClient;
  FileBrowserFrame.BackgroundPanel.Visible := true;

  AboutFrame := TAboutFrame.Create(self.Owner);
  AboutFrame.BackgroundPanel.Align := alNone;
  AboutFrame.BackgroundPanel.Parent := OverlayContainer;
  AboutFrame.BackgroundPanel.Visible := false;


  ModSystem2Frame := TModSystem2Frame.Create(self.Owner);
  ModSystem2Frame.BackgroundPanel.Align := alClient;
  ModSystem2Frame.BackgroundPanel.Visible := true;
  ModSystem2Frame.BackgroundPanel.Parent := ModSystem2Div;

  SequencerFrame := TSequencerFrame.Create(self.Owner);
  SequencerFrame.BackgroundPanel.Visible := true;
  SequencerFrame.BackgroundPanel.Parent := TabPanel;

  //======= Build the GUI =======

  DropFileTarget := TRedFoxDropFileTarget.Create(RedFoxContainer);
  DropFileTarget.RegisterTarget(SampleMapFrame.SampleMap);
  DropFileTarget.RegisterTarget(SampleDisplayFrame.SampleOverlay);
  DropFileTarget.RegisterTarget(MiniSampleDisplayFrame.SampleOverlay);


  //===================================



  // Finally...
  UpdateLayout;
end;

procedure TPluginGui.FormDestroy(Sender: TObject);
begin
  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(self);

  if assigned(GuiStandard)
    then GuiStandard.Free;

  DialogDisplayArea.Free;
  CurrentGuiState.Free;

  FreeAndNil(MenuBarFrame);
  FreeAndNil(SampleMapFrame);
  FreeAndNil(SampleDisplayFrame);
  FreeAndNil(MiniSampleDisplayFrame);
  FreeAndNil(FileBrowserFrame);
  FreeAndNil(VoiceControlFrame);
  FreeAndNil(ModControlFrame);
  FreeAndNil(InfoBarFrame);
  FreeAndNil(ModSystem2Frame);
  FreeAndNil(SequencerFrame);

  DropFileTarget.Free;
  FeedBackData.Free;
  PluginHotkeys.Free;
  if assigned(fPluginKeyHook) then FreeAndNil(fPluginKeyHook);
  OverlayContainer.Free;
end;



procedure TPluginGui.PostCreate(const aVstWindow : HWnd);
var
  fn : string;
  msg : TMessage;
  VQ : IVamQuery;
  c : TControl;
  bm1 : TBitmap;
  bm2 : TBitmap;
  bm3 : TBitmap;
  aRegionID : TGUID;
begin
  assert(assigned(Plugin));

  VstWindow := aVstWindow;

  GuiStandard := TGuiStandard.Create(Plugin, Plugin.Globals);
  GuiStandard.OnControlMouseDown := self.EventHandle_ControlMouseDown;

  Plugin.Globals.MotherShip.RegisterZeroObject(self);




  //fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'User';
  fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Factory';
  fn := IncludeTrailingPathDelimiter(fn) + 'Config';
  fn := IncludeTrailingPathDelimiter(fn) + 'KeyHook.xml';

  {$IFDEF Logging}
  if FileExists(fn)
    then LogMain.LogText('KeyHook.xml Found', fn)
    else LogMain.LogText('KeyHook.xml NOT Found!', fn);
  {$ENDIF}


  //called by the plugin after plugin reference has been assigned.
  if FileExists(fn) then
  begin
    PluginKeyHook := TPluginKeyHook.Create(Plugin.Globals.HostProperties^.HostName, Plugin.Globals.HostProperties^.HostVersion, aVstWindow, fn);

    PluginKeyHook.OnKeyDown := PluginHotKeys.KeyDown;
    PluginKeyHook.OnKeyUp   := PluginHotKeys.KeyUp;
  end;



  //fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'User';
  fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Factory';
  fn := IncludeTrailingPathDelimiter(fn) + 'Config';
  fn := IncludeTrailingPathDelimiter(fn) + 'KeyCommands.xml';

  {$IFDEF Logging}
  if FileExists(fn)
    then LogMain.LogText('KeyCommands.xml Found', fn)
    else LogMain.LogText('KeyCommands.xml NOT Found!', fn);
  {$ENDIF}


  if FileExists(fn) then
  begin
    PluginHotkeys.LoadFromXML(fn);
  end;



  Plugin.Globals.AddWindowsMessageListener(self.Handle);



  //====== Register frames as zero objects =====================================
  Plugin.Globals.MotherShip.RegisterZeroObject(FileBrowserFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(MenuBarFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(MiniSampleDisplayFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(SampleMapFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(VoiceControlFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(ModControlFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(SequencerFrame);
  Plugin.Globals.MotherShip.RegisterZeroObject(ModSystem2Frame);


  try
    // Initalize all the frame controls...
    MiniSampleDisplayFrame.InitializeFrame(Plugin, GuiStandard);
    FileBrowserFrame.InitializeFrame(Plugin, GuiStandard);
    MenuBarFrame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    SampleMapFrame.InitializeFrame(Plugin, GuiStandard);
    SampleDisplayFrame.InitializeFrame(Plugin, GuiStandard);
    ModControlFrame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    ModSystem2Frame.InitializeFrame(Plugin, GuiStandard);
    SequencerFrame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    VoiceControlFrame.InitializeFrame(Plugin, GuiStandard);
    InfoBarFrame.InitializeFrame(Plugin, GuiStandard, RedFoxContainer);
  except
  end;




  //Update the GUI to match the previous GUI state.
  LowerTabState := Plugin.GuiState.LowerTabState;
  UpdateLayout;



  //===== set up the skin graphics =======
  {
  if Plugin.Globals.SkinImageLoader.Exists('Knob_UniPolar') then
  begin
    bm1 := Plugin.Globals.SkinImageLoader.GetImage('Knob_UniPolar');
    bm2 := Plugin.Globals.SkinImageLoader.GetImage('Knob_PlaceHolder');
    VQ := VamQueryRequest(RedFoxContainer, 'UniPolarKnob');
    for c in VQ.List do
    begin
      (c as TVamKnob).ImageStrip := bm1;
      (c as TVamKnob).ImageStripGlyphCount := 65;
      (c as TVamKnob).DisabledImage := bm2;
    end;
  end;

  if Plugin.Globals.SkinImageLoader.Exists('Knob_BiPolar') then
  begin
    bm1 := Plugin.Globals.SkinImageLoader.GetImage('Knob_BiPolar');
    bm2 := Plugin.Globals.SkinImageLoader.GetImage('Knob_PlaceHolder');
    VQ := VamQueryRequest(RedFoxContainer, 'BiPolarKnob');
    for c in VQ.List do
    begin
      (c as TVamKnob).ImageStrip := bm1;
      (c as TVamKnob).ImageStripGlyphCount := 65;
      (c as TVamKnob).DisabledImage := bm2;
    end;
  end;

  if Plugin.Globals.SkinImageLoader.Exists('ModMatrix_Knob') then
  begin
    bm1 := Plugin.Globals.SkinImageLoader.GetImage('ModMatrix_Knob');
    VQ := VamQueryRequest(RedFoxContainer, 'ModMatrixKnob');
    for c in VQ.List do
    begin
      (c as TVamKnob).ImageStrip := bm1;
      (c as TVamKnob).ImageStripGlyphCount := 65;
    end;
  end;
  }

    if Plugin.Globals.SkinImageLoader.Exists('Knob_Lower')
      then bm1 := Plugin.Globals.SkinImageLoader.GetImage('Knob_Lower');

    if Plugin.Globals.SkinImageLoader.Exists('Knob_Upper')
      then bm2 := Plugin.Globals.SkinImageLoader.GetImage('Knob_Upper');

    if Plugin.Globals.SkinImageLoader.Exists('Knob_PlaceHolder')
      then bm3 := Plugin.Globals.SkinImageLoader.GetImage('Knob_PlaceHolder');

    VQ := VamQueryRequest(RedFoxContainer, 'UniPolarKnob');
    for c in VQ.List do
    begin
      (c as TVamKnob).Image_KnobLower := bm1;
      (c as TVamKnob).Image_KnobUpper := bm2;
      (c as TVamKnob).DisabledImage   := bm3;
      (c as TVamKnob).ModLineDist  := 15;
      (c as TVamKnob).ModLineWidth := 4.5;
      (c as TVamKnob).IsBipolarKnob := false;
      (c as TVamKnob).ModLineColor := kModLineColorA;
      (c as TVamKnob).ModLineOffColor := kModLineColorOff;
      (c as TVamKnob).IndicatorSize := 2.4;
      (c as TVamKnob).IndicatorDist := 6.5;
    end;

    VQ := VamQueryRequest(RedFoxContainer, 'BiPolarKnob');
    for c in VQ.List do
    begin
      (c as TVamKnob).Image_KnobLower := bm1;
      (c as TVamKnob).Image_KnobUpper := bm2;
      (c as TVamKnob).DisabledImage   := bm3;
      (c as TVamKnob).ModLineDist  := 15;
      (c as TVamKnob).ModLineWidth := 4.5;
      (c as TVamKnob).IsBipolarKnob := true;
      (c as TVamKnob).ModLineColor := kModLineColorA;
      (c as TVamKnob).ModLineOffColor := kModLineColorOff;
      (c as TVamKnob).IndicatorSize := 2.4;
      (c as TVamKnob).IndicatorDist := 6.5;
    end;


    if Plugin.Globals.SkinImageLoader.Exists('Small_Knob')
      then bm2 := Plugin.Globals.SkinImageLoader.GetImage('Small_Knob');

    VQ := VamQueryRequest(RedFoxContainer, 'SmallBipolarKnob');
    for c in VQ.List do
    begin
      //(c as TVamKnob).Image_KnobLower := bm1;
      (c as TVamKnob).Image_KnobUpper := bm2;
      //(c as TVamKnob).DisabledImage   := bm3;

      (c as TVamKnob).ModLineDist  := 12;
      (c as TVamKnob).ModLineWidth := 3.5;
      (c as TVamKnob).IndicatorDist := 5;
      (c as TVamKnob).IndicatorSize := 2;

      (c as TVamKnob).IsBipolarKnob := true;
      (c as TVamKnob).ModLineColor := kModLineColorA;
      (c as TVamKnob).ModLineOffColor := kModLineColorOff;
    end;

    VQ := VamQueryRequest(RedFoxContainer, 'SmallUnipolarKnob');
    for c in VQ.List do
    begin
      //(c as TVamKnob).Image_KnobLower := bm1;
      (c as TVamKnob).Image_KnobUpper := bm2;
      //(c as TVamKnob).DisabledImage   := bm3;

      (c as TVamKnob).ModLineDist  := 12;
      (c as TVamKnob).ModLineWidth := 3.5;
      (c as TVamKnob).IndicatorDist := 5;
      (c as TVamKnob).IndicatorSize := 2;

      (c as TVamKnob).IsBipolarKnob := false;
      (c as TVamKnob).ModLineColor := kModLineColorA;
      (c as TVamKnob).ModLineOffColor := kModLineColorOff;
    end;






  //==============



  //==============
  if (Plugin.FocusedRegion = nil) and (Plugin.SampleMap.RegionCount > 0) then
  begin
    aRegionID := Plugin.SampleMap.Regions[0].GetProperties^.UniqueID;
    Plugin.FocusRegion(aRegionID);
    MiniSampleDisplayFrame.UpdateSampleDisplay;
  end;

end;

procedure TPluginGui.BeforeClose;
begin
  if assigned(Plugin) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(self.Handle);
  end;
end;


procedure TPluginGui.UpdateGui(Sender: TObject);
var
  rd:TRegionDisplayResult;
begin
  //Update the gui elements here.
  Manually := true;

  if assigned(PluginKeyHook) then
  begin
    PluginKeyHook.RefreshKeyHookTarget;
  end;

  if CurrentGuiState.IsSampleMapVisible <> Plugin.GuiState.IsSampleMapVisible then UpdateLayout;

  rd := FindRegionToDisplay(Plugin);
  FeedbackData.FocusedRegion := rd.Region;

  Plugin.GetGuiFeedBack(FeedbackData);

  MenuBarFrame.UpdateGui(Sender, @FeedbackData);
  SampleDisplayFrame.UpdateGui(Sender, @FeedbackData);
  MiniSampleDisplayFrame.UpdateGui(Sender, @FeedbackData);
  ModControlFrame.UpdateGui(Sender, @FeedbackData);
  SequencerFrame.UpdateGui(Sender, @FeedbackData);
  VoiceControlFrame.UpdateGui(Sender, @FeedbackData);

  GuiStandard.UpdateControls;

  Manually := false;
end;

procedure TPluginGui.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  if MsgID = TLucidMsgId.SampleFocusChanged then
  begin
    SampleMapFrame.UpdateSampleRegions;
    SampleDisplayFrame.UpdateSampleDisplay;
    MiniSampleDisplayFrame.UpdateSampleDisplay;
    MenuBarFrame.SampleFocusChanged;
  end;

  if MsgID = TLucidMsgId.SampleRegionChanged then
  begin
    SampleMapFrame.UpdateSampleRegions;
    SampleMapFrame.UpdateRootNoteKeys;
    SampleMapFrame.UpdateRegionInfoDisplay;
    SampleDisplayFrame.UpdateSampleDisplay;
    MiniSampleDisplayFrame.UpdateSampleDisplay;
    MenuBarFrame.SampleFocusChanged;
  end;

  if MsgID = TLucidMsgId.MidiKeyChanged then
  begin
    SampleMapFrame.MidiKeyChanged;
  end;

  if MsgID = TLucidMsgId.MouseOverSampleRegionChanged then
  begin
    MenuBarFrame.SampleFocusChanged;
    MiniSampleDisplayFrame.UpdateSampleDisplay;
  end;

  if MsgID = TLucidMsgID.PreviewInfoChanged then
  begin
    FileBrowserFrame.PreviewInfoChanged;
  end;

  if MsgID = TLucidMsgID.SampleMarkersChanged then MiniSampleDisplayFrame.GuiEvent_SampleMakersChanged;
  if MsgID = TLucidMsgID.SampleOscTypeChanged then VoiceControlFrame.PlaybackTypeChanged;


  if MsgID = TLucidMsgID.Command_ShowSampleMapEdit then ShowSampleMapEdit;
  if MsgID = TLucidMsgID.Command_HideSampleMapEdit then HideSampleMapEdit;

  if MsgID = TLucidMsgID.Command_ShowAboutDialog then ShowAboutDialog;
  if MsgID = TLucidMsgID.Command_ShowLoopEditFrame then ShowLoopEditFrame;

  if MsgID = TLucidMsgID.Command_CloseCurrentDialog then CloseCurrentDialog;









end;


procedure TPluginGui.SetLowerTabState(const Value: TLowerTabOptions);
begin
  fLowerTabState := Value;

  case Value of
    TLowerTabOptions.TabSeq1: TabPanel.TabIndex := 0;
    TLowerTabOptions.TabSeq2: TabPanel.TabIndex := 1;
    TLowerTabOptions.TabMain: TabPanel.TabIndex := 2;
  else
    raise Exception.Create('Unexpect tab value.');
  end;

  case Value of
    TLowerTabOptions.TabSeq1:
    begin
      SequencerFrame.SequencerIndex := 0;
      SequencerFrame.BackgroundPanel.Visible  := true;
      ModControlFrame.BackgroundPanel.Visible := false;
    end;

    TLowerTabOptions.TabSeq2:
    begin
      SequencerFrame.SequencerIndex := 1;
      SequencerFrame.BackgroundPanel.Visible  := true;
      ModControlFrame.BackgroundPanel.Visible := false;
    end;

    TLowerTabOptions.TabMain:
    begin
      SequencerFrame.BackgroundPanel.Visible  := false;
      ModControlFrame.BackgroundPanel.Visible := true;

    end;
  else
    raise Exception.Create('Unexpect tab value.');
  end;

end;

procedure TPluginGui.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMothership := aMotherShip;
end;

procedure TPluginGui.LowerTabsChanged(Sender: TObject);
var
  TabSelected : TLowerTabOptions;
begin
  if not assigned(Plugin) then exit;

  case TabPanel.TabIndex of
    -1: TabSelected := TLowerTabOptions.TabMain;
    0: TabSelected := TLowerTabOptions.TabSeq1;
    1: TabSelected := TLowerTabOptions.TabSeq2;
    2: TabSelected := TLowerTabOptions.TabMain;
  else
    raise Exception.Create('Error Message');
  end;

  Plugin.GuiState.LowerTabState := TabSelected;
  self.LowerTabState := TabSelected;

end;






procedure TPluginGui.HotkeyEvent(Sender: TObject; const CommandID: string);
var
  KeyCommand : TKeyCommand;
begin
  KeyCommand := TKeyCommandHelper.ToEnum(CommandID);

  case KeyCommand of
    TKeyCommand.ContextUp,
    TKeyCommand.ContextDown,
    TKeyCommand.ContextLeft,
    TKeyCommand.ContextRight,
    TKeyCommand.PageUp,
    TKeyCommand.PageDown,
    TKeyCommand.SelectUp,
    TKeyCommand.SelectDown,
    TKeyCommand.ReplaceLoad:
    begin
      FileBrowserFrame.KeyCommand(KeyCommand);
    end;

  else
    raise Exception.Create('Error: Key command not handled.');
  end;

end;

procedure TPluginGui.FormResize(Sender: TObject);
begin
  if assigned(OverlayContainer) then
  begin
    OverlayContainer.Left := 0;
    OverlayContainer.Top  := 0;
    OverlayContainer.Width :=  self.Width;
    OverlayContainer.Height := self.Height;
  end;
end;

function TPluginGui.GetMotherShipReference: IMotherShip;
begin
  result := FMotherShip;
end;

procedure TPluginGui.ShowSampleMapEdit;
begin
  if not assigned(Plugin) then exit;

  Plugin.GuiState.IsSampleMapVisible := true;

  // NOTE: Logically it feels like SampleMapFrame.UpdateRootNoteKeys
  // should be called after UpdateLayout. But there is a noticable
  // lag between the layout becoming updated and the root note keys
  // being updated. Therefore I've decided to update the root keys
  // before changing the layout.
  // Maybe in future this could be avoid by using some sort of
  // nested BeginUpdate/EndUpdate setup. IE. A control doesn't
  // update if any of it's parent controls are in an update stage.
  SampleMapFrame.UpdateRootNoteKeys;

  UpdateLayout;
end;

procedure TPluginGui.HideSampleMapEdit;
var
  kg : IKeyGroup;
  aRegion : IRegion;
begin
  if not assigned(Plugin) then exit;
  Plugin.GuiState.IsSampleMapVisible := false;
  UpdateLayout;

  if (Plugin.FocusedRegion = nil) and (Plugin.SampleMap.RegionCount > 0) then
  begin
    kg := Plugin.FocusedKeyGroup;
    if assigned(kg) then
    begin
      aRegion := Plugin.SampleMap.FindRegionByKeyGroup(kg.GetName);
      if assigned(aRegion) then
      begin
        Plugin.FocusRegion(aRegion.GetProperties^.UniqueID);

        // trigger extra updates to avoid the message queue latency.
        MenuBarFrame.SampleFocusChanged;
        MiniSampleDisplayFrame.UpdateSampleDisplay;
      end;
    end;
  end;
end;

procedure TPluginGui.ShowLoopEditFrame;
var
  LoopEditDialog : ILoopEditDialog;
  CloseCallback : TProc;
begin
  LoopEditDialog := TLoopEditDialog.Create;
  LoopEditDialog.Setup(DialogDisplayArea.GetDisplayArea, Plugin, GuiStandard);

  CloseCallback := procedure
  begin
    LoopEditDialog := nil;
  end;

  DialogDisplayArea.Show(true, CloseCallback);
end;

procedure TPluginGui.ShowAboutDialog;
var
  aW, aH, aT, aL : integer;
begin
  OverlayContainer.Visible := true;
  AboutFrame.BackgroundPanel.Visible := true;

  aW := round(OverlayContainer.Width * (2/3));
  aH := round(OverlayContainer.Height * (2/3));
  aL := round((OverlayContainer.Width  - aW)  * (1/2));
  aT := round((OverlayContainer.Height - aH) * (1/3));

  AboutFrame.BackgroundPanel.Resize(aL, aT, aW, aH);
  AboutFrame.BackgroundPanel.Invalidate;
  OverlayContainer.Invalidate;
end;



procedure TPluginGui.CloseCurrentDialog;
begin
  if assigned(OverlayContainer) then
  begin
    OverlayContainer.Visible    := false;
    AboutFrame.BackgroundPanel.Visible    := false;
  end;
end;

procedure TPluginGui.UpdateLayout;
  function StackDiv(const aDiv : TVamDiv; const Offset, Margin: integer):integer;
  begin
    aDiv.Top := Offset + Margin;
    result := aDiv.Top + aDiv.Height;
  end;
var
  IsSampleMapVisible : boolean;
  WorkAreaWidth : integer;
  Erector : TControlErector;
begin
  if not assigned(Plugin) then exit;

  //============================================================================
  //         Colors
  //============================================================================

  TitlePanel.Color := kPanelLight;
  MenuBarFrame.BackgroundPanel.Color := kPanelLight;
  FileBrowserFrame.BackgroundPanel.Color := kPanelLight;


  SampleMapFrame.BackgroundPanel.Color         := kPanelLight;
  MiniSampleDisplayFrame.BackgroundPanel.Color := kPanelLight;
  ModControlFrame.BackgroundPanel.Color        := kPanelLight;
  SequencerFrame.BackgroundPanel.Color         := kPanelLight;
  VoiceControlFrame.BackgroundPanel.Color      := kPanelLight;
  ModSystem2Frame.BackgroundPanel.Color        := kPanelLight;
  InfoBarFrame.BackgroundPanel.Color           := kPanelDark;



  FileBrowserFrame.InsidePanel.Color := kColor_LcdDark1;
  MiniSampleDisplayFrame.InsidePanel.Color := kColor_LcdDark1;
  SampleMapFrame.InsidePanel.Color := kColor_LcdDark1;


  //FileBrowserFrame.FileTreeView.Font.Name := 'Tondo';
  FileBrowserFrame.FileTreeView.Font.Name := 'Tahoma';
  FileBrowserFrame.FileTreeView.Font.Size := 9;
  FileBrowserFrame.FileTreeView.Font.Color := GetRedFoxColor(kColor_LcdDark5);


  TabPanel.Color_Background := kPanelDark;
  TabPanel.Color_TabOff     := kPanelDark;
  TabPanel.Color_TabOn      := kPanelLight;


  //============== Static GUI Setup ================================
  SidePanel.Align := alClient;


  //============================================================================
  // Reset margins and paddings for all the container panels.
  // NOTE: ClearPadding() also sets the CornerRadius values.
  //============================================================================
  ClearPadding(self.RedFoxContainer);
    ClearPadding(self.MainPanel);
      ClearPadding(self.MainWorkArea);
        ClearPadding(self.MainMenuBar);
        ClearPadding(self.SampleMapDiv);
        ClearPadding(self.MainTop);
        ClearPadding(self.VoiceControlDiv);
        ClearPadding(self.TabPanel);
        ClearPadding(self.InfoBarDiv);
      ClearPadding(self.SideWorkArea);
        ClearPadding(self.SidePanel);
        ClearPadding(self.TitlePanel);

  ClearPadding(MenuBarFrame.BackgroundPanel);
  ClearPadding(SampleDisplayFrame.BackgroundPanel);
  ClearPadding(MiniSampleDisplayFrame.BackgroundPanel);
  ClearPadding(SampleMapFrame.BackgroundPanel);
  ClearPadding(FileBrowserFrame.BackgroundPanel);
  ClearPadding(AboutFrame.BackgroundPanel);
  ClearPadding(VoiceControlFrame.BackgroundPanel);
  ClearPadding(ModControlFrame.BackgroundPanel);
  ClearPadding(SequencerFrame.BackgroundPanel);
  ClearPadding(ModSystem2Frame.BackgroundPanel);
  ClearPadding(InfoBarFrame.BackgroundPanel);





  //============================================================================
  // Set padding and margins so there is a two pixel space between
  // all panel components.
  //============================================================================

  //== Outside Containers ==
  MainPanel.Padding.SetBounds(3,3,3,3);
  MainWorkArea.Padding.SetBounds(1,0,1,0);
  SideWorkArea.Padding.SetBounds(1,0,1,0);

  //== Side Work Area Panels ==
  TitlePanel.Margins.SetBounds(0,1,0,1);
  SidePanel.Margins.SetBounds(0,1,0,1);




  //== Main Work Area Panels ==

  //============================================================================
  // Set internal padding of a few panels to ensure consistenency.
  //============================================================================

  FileBrowserFrame.BackgroundPanel.Padding.SetBounds(4,4,4,4);
  SampleMapFrame.BackgroundPanel.Padding.SetBounds(4,4,4,4);
  MiniSampleDisplayFrame.BackgroundPanel.Padding.SetBounds(4,4,4,4);
  MiniSampleDisplayFrame.InsidePanel.Padding.SetBounds(4,4,4,4);
  AboutFrame.BackgroundPanel.Padding.SetBounds(16,16,16,16);
  VoiceControlFrame.BackgroundPanel.Padding.SetBounds(16,8,16,8);
  ModControlFrame.BackgroundPanel.Padding.SetBounds(16,8,16,8);
  ModSystem2Frame.BackgroundPanel.Padding.SetBounds(16,8,16,8);


  //InfoBarDiv.Padding.SetBounds(0,2,0,0);

  //============================================================================
  //   A little more tweaking...
  //===========================================================================

  ModControlFrame.BackgroundPanel.CornerRadius1 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius2 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius3 := 0;
  ModControlFrame.BackgroundPanel.CornerRadius4 := 0;

  SequencerFrame.BackgroundPanel.CornerRadius1 := 3;
  SequencerFrame.BackgroundPanel.CornerRadius2 := 3;
  SequencerFrame.BackgroundPanel.CornerRadius3 := 0;
  SequencerFrame.BackgroundPanel.CornerRadius4 := 0;

  TabPanel.CornerRadius1 := 0;
  TabPanel.CornerRadius2 := 0;
  TabPanel.CornerRadius3 := 3;
  TabPanel.CornerRadius4 := 3;

  SpacerPanel1.CornerRadius1 := 3;
  SpacerPanel1.CornerRadius2 := 3;
  SpacerPanel1.CornerRadius3 := 3;
  SpacerPanel1.CornerRadius4 := 3;




  //============== Set main panels to correct dimensions ================================



  //============================================================================
  //============== Dynamic GUI Setup ================================
  //============================================================================



  //== Setup ==
  CurrentGuiState.IsSampleMapVisible := Plugin.GuiState.IsSampleMapVisible;
  IsSampleMapVisible := CurrentGuiState.IsSampleMapVisible;
  //===================


  MainWorkArea.Width := 600 + MainWorkArea.Padding.Left + MainWorkArea.Padding.right;
  WorkAreaWidth := MainWorkArea.Width - MainWorkArea.Padding.Left - MainWorkArea.Padding.Right;

  MainMenuBar.Left     := MainWorkArea.Padding.Left;
  SampleMapDiv.Left    := MainWorkArea.Padding.Left;
  MainTop.Left         := MainWorkArea.Padding.Left;
  VoiceControlDiv.Left := MainWorkArea.Padding.Left;

  TabPanel.Left        := MainWorkArea.Padding.Left;
  ModSystem2Div.Left   := MainWorkArea.Padding.Left;
  InfoBarDiv.Left      := MainWorkArea.Padding.Left;
  SpacerPanel1.Left    := MainWorkArea.Padding.Left;


  MainMenuBar.Width     := WorkAreaWidth;
  SampleMapDiv.Width    := WorkAreaWidth;
  MainTop.Width         := WorkAreaWidth;
  VoiceControlDiv.Width := WorkAreaWidth;
  TabPanel.Width        := WorkAreaWidth;
  ModSystem2Div.Width   := WorkAreaWidth;
  InfoBarDiv.Width      := WorkAreaWidth;
  SpacerPanel1.Width    := WorkAreaWidth;

  TitlePanel.Height      := 28;
  MainMenuBar.Height     := 28;
  MainTop.Height         := 208;
  VoiceControlDiv.Height := 80;
  TabPanel.Height        := 252;
  ModSystem2Div.Height   := 70;
  InfoBarDiv.Height      := 30;
  SpacerPanel1.Height    := 36;

  //SampleMapDiv.Height    := 258;
  SampleMapDiv.Height    := 406;

  if IsSampleMapVisible = true then
  begin
    SampleMapDiv.Visible    := true;
    VoiceControlDiv.Visible := false;
    TabPanel.Visible        := false;
    ModSystem2Div.Visible   := false;
    SpacerPanel1.Visible    := false;


    MainMenuBar.Top := 1;
    MainMenuBar.Left := 1;
    Erector.Init(MainMenuBar, MainTop).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(MainTop, SampleMapDiv).SnapToEdge(cfBottomEdge).Move(0,2);
    //Erector.Init(SampleMapDiv, VoiceControlDiv).SnapToEdge(cfBottomEdge).Move(0,2);
    //Erector.Init(VoiceControlDiv, MainMid).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(SampleMapDiv, InfoBarDiv).SnapToEdge(cfBottomEdge).Move(0,2);

  end else
  begin
    SampleMapDiv.Visible    := false;
    VoiceControlDiv.Visible := true;
    TabPanel.Visible        := true;
    ModSystem2Div.Visible   := true;
    SpacerPanel1.Visible    := false;

    MainMenuBar.Top := 1;
    MainMenuBar.Left := 1;
    Erector.Init(MainMenuBar, MainTop).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(MainTop, VoiceControlDiv).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(VoiceControlDiv, TabPanel).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(TabPanel, ModSystem2Div).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(ModSystem2Div, SpacerPanel1).SnapToEdge(cfBottomEdge).Move(0,2);
    Erector.Init(ModSystem2Div, InfoBarDiv).SnapToEdge(cfBottomEdge).Move(0,2);
  end;



  // TODO: InfoBarDiv and SpacePanel1 should both be deleted.
  InfoBarDiv.Visible   := false;
  SpacerPanel1.Visible := false;

  MainWorkArea.Repaint;
end;



procedure TPluginGui.DoShowDialogArea(Sender: TObject);
begin
  OverlayContainer.Visible := true;
end;

procedure TPluginGui.DoHideDialogArea(Sender: TObject);
begin
  OverlayContainer.Visible := false;
end;

function TPluginGui.DoGetDialogArea(Sender: TObject): TWinControl;
begin
  result := OverlayContainer;
end;

procedure TPluginGui.OverlayContainerClicked(Sender: TObject);
begin
  //CloseCurrentDialog(msg);

  if DialogDisplayArea.AllowClose then
  begin
    DialogDisplayArea.Hide;
  end;
end;

procedure TPluginGui.EventHandle_ControlMouseDown(Sender: TObject; const Target: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Block: boolean);
begin
  Plugin.GuiState.FocusedControl := Target;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FocusedControlChanged);
end;





end.
