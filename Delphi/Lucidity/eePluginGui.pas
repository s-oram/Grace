unit eePluginGui;

interface

{$INCLUDE Defines.inc}


uses
  eeGuiStandardv2,
  LucidityGui.KnobHandler,
  LucidityGui.MenuButtonHandler,
  Lucidity.Interfaces,
  VamLib.ZeroObject,
  uXYPadsFrame,
  uSequencerFrame,
  OtlComm, uModSystem2Frame,
  uAboutFrame, Lucidity.SampleMap, uDialogDisplayArea,
  eeEnumHelper, uLucidityEnums,
  DAEffect, DAEffectX, eePluginKeyHook,
  uGuiState, eePluginHotkeys,
  uConstants, uGuiFeedbackData,
  uSampleMapFrame, uFileBrowserFrame, uVoiceControlFrame,
  uMenuBarFrame,
  uZoomSampleDisplayFrame,
  uModControlFrame, uMiniSampleDisplayFrame,
  eeFileBrowserAddon, eeRedFoxDropFileTarget,
  eePlugin,
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
    SideWorkArea: TVamDiv;
    TitlePanel: TVamPanel;
    VamLabel1: TVamLabel;
    VoiceControlDiv: TVamDiv;
    TabPanel: TVamTabPanel;
    SidePanel: TVamDiv;
    ModSystem2Div: TVamDiv;
    MainPanel: TVamPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LowerTabsChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fPlugin: TeePlugin;
    fLowerTabState: TLowerTabOptions; //TODO:MED I think this can be deleted.
    fCurrentGuiState: TGuiState;
    fPluginHotkeys: TPluginHotkeys;
    fPluginKeyHook: TPluginKeyHook;
    procedure SetLowerTabState(const Value: TLowerTabOptions);
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    Manually:boolean;
    GuiStandard : TGuiStandard;
    DialogDisplayArea : TDialogDisplayArea;
    VstWindow : Hwnd;
    DropFileTarget : TRedFoxDropFileTarget;

    MenuBarFrame           : TMenuBarFrame;
    MiniSampleDisplayFrame : TMiniSampleDisplayFrame;
    SampleMapFrame         : TSampleMapFrame;
    FileBrowserFrame       : TFileBrowserFrame;
    VoiceControlFrame      : TVoiceControlFrame;
    ModControlFrame        : TModControlFrame;
    AboutFrame             : TAboutFrame;
    ModSystem2Frame        : TModSystem2Frame;
    SequencerFrame         : TSequencerFrame;
    VoiceSetupFrame        : TXYPadsFrame;

    FeedbackData : TGuiFeedBackData;

    OverlayContainer : TVamPanel;

    procedure ShowSampleMapEdit;
    procedure HideSampleMapEdit;
    procedure ShowAboutDialog;
    procedure CloseCurrentDialog;

    property LowerTabState : TLowerTabOptions read fLowerTabState write SetLowerTabState;
    property CurrentGuiState : TGuiState read fCurrentGuiState write fCurrentGuiState; // TODO:MED: I'm not sure if the GUI needs a copy of the Current GUI state object anymore.

    procedure HotkeyEvent(Sender : TObject; const CommandID : string);

    procedure OverlayContainerClicked(Sender : TObject);

    function DoGetDialogArea(Sender : TObject):TWinControl;
    procedure DoShowDialogArea(Sender : TObject);
    procedure DoHideDialogArea(Sender : TObject);

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
  VamLayoutWizard,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eePluginDataDir,
  Lucidity.KeyGroup,
  GuidEx,
  eeGuiSetup,
  RedFoxColor,
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
  MainPanel.AlignWithMargins := false;

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

  VoiceSetupFrame := TXYPadsFrame.Create(self.Owner);
  VoiceSetupFrame.BackgroundPanel.Visible := true;
  VoiceSetupFrame.BackgroundPanel.Parent := TabPanel;

  //======= Build the GUI =======
  DropFileTarget := TRedFoxDropFileTarget.Create(RedFoxContainer);
  DropFileTarget.RegisterTarget(SampleMapFrame.SampleMap);
  DropFileTarget.RegisterTarget(MiniSampleDisplayFrame.SampleOverlay);
  //===================================


  // Finally...
  UpdateLayout;
end;

procedure TPluginGui.FormDestroy(Sender: TObject);
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  if assigned(GuiStandard)
    then GuiStandard.Free;

  DialogDisplayArea.Free;
  CurrentGuiState.Free;

  FreeAndNil(MenuBarFrame);
  FreeAndNil(SampleMapFrame);
  FreeAndNil(MiniSampleDisplayFrame);
  FreeAndNil(FileBrowserFrame);
  FreeAndNil(VoiceControlFrame);
  FreeAndNil(ModControlFrame);
  FreeAndNil(ModSystem2Frame);
  FreeAndNil(SequencerFrame);
  FreeAndNil(AboutFrame);
  FreeAndNil(VoiceSetupFrame);

  DropFileTarget.Free;
  FeedBackData.Free;
  PluginHotkeys.Free;
  if assigned(fPluginKeyHook) then FreeAndNil(fPluginKeyHook);
  OverlayContainer.Free;
end;



procedure TPluginGui.PostCreate(const aVstWindow : HWnd);
var
  fn : string;
  VQ : IVamQuery;
  c : TControl;
  bm1 : TBitmap;
  bm2 : TBitmap;
  bm3 : TBitmap;
  KnobHandler : TKnobHandler;
  MenuHandler : TMenuButtonHandler;
begin
  assert(assigned(Plugin));

  VstWindow := aVstWindow;

  GuiStandard := TGuiStandard.Create;

  KnobHandler := TKnobHandler.Create(Plugin);
  GuiStandard.RegisterHandler('KnobHandler', KnobHandler);
  Plugin.Globals.MotherShip.RegisterZeroObject(KnobHandler, TZeroObjectRank.VCL);

  MenuHandler := TMenuButtonHandler.Create(Plugin);
  GuiStandard.RegisterHandler('MenuButtonHandler', MenuHandler);
  Plugin.Globals.MotherShip.RegisterZeroObject(MenuHandler, TZeroObjectRank.VCL);


  Plugin.Globals.MotherShip.RegisterZeroObject(self, TZeroObjectRank.VCL);

  //==== Load the key hook config ==============================================
  // TODO:HIGH need to check for duplicate file in the Config User Override directory.
  fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Config Default';
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



  //==== Load the key commands config ==========================================
  // TODO:HIGH need to check for duplicate file in the Config User Override directory.
  fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Config Default';
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



  // TODO:MED Delete this AddWindowsMessageLister(). It's been superceded by the ZeroObject stuff.
  Plugin.Globals.AddWindowsMessageListener(self.Handle);


  //====== Register frames as zero objects =====================================
  Plugin.Globals.MotherShip.RegisterZeroObject(FileBrowserFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(MenuBarFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(MiniSampleDisplayFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(SampleMapFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(VoiceControlFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(ModControlFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(SequencerFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(VoiceSetupFrame, TZeroObjectRank.VCL);
  Plugin.Globals.MotherShip.RegisterZeroObject(ModSystem2Frame, TZeroObjectRank.VCL);


  try
    // Initalize all the frame controls...
    MiniSampleDisplayFrame.InitializeFrame(Plugin, GuiStandard);
    FileBrowserFrame.InitializeFrame(Plugin, GuiStandard);
    MenuBarFrame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    SampleMapFrame.InitializeFrame(Plugin, GuiStandard);
    ModControlFrame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    ModSystem2Frame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    SequencerFrame.InitializeFrame(Plugin, GuiStandard, DialogDisplayArea);
    VoiceSetupFrame.InitializeFrame(Plugin, GuiStandard);
    VoiceControlFrame.InitializeFrame(Plugin, GuiStandard);
  except
  end;




  //Update the GUI to match the previous GUI state.
  LowerTabState := Plugin.Globals.GuiState.LowerTabState;
  UpdateLayout;



  //===== set up the skin graphics =======
  if Plugin.Globals.SkinImageLoader.Exists('Knob_Lower')
    then bm1 := Plugin.Globals.SkinImageLoader.GetImage('Knob_Lower')
    else bm1 := nil;

  if Plugin.Globals.SkinImageLoader.Exists('Knob_Upper')
    then bm2 := Plugin.Globals.SkinImageLoader.GetImage('Knob_Upper')
    else bm2 := nil;

  if Plugin.Globals.SkinImageLoader.Exists('Knob_PlaceHolder')
    then bm3 := Plugin.Globals.SkinImageLoader.GetImage('Knob_PlaceHolder')
    else bm3 := nil;

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

  if Plugin.Globals.SkinImageLoader.Exists('Knob_PlaceHolder')
    then bm3 := Plugin.Globals.SkinImageLoader.GetImage('Knob_PlaceHolder');

  VQ := VamQueryRequest(RedFoxContainer, 'SmallBipolarKnob');
  for c in VQ.List do
  begin
    //(c as TVamKnob).Image_KnobLower := bm1;
    (c as TVamKnob).Image_KnobUpper := bm2;
    (c as TVamKnob).DisabledImage   := bm3;

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
    (c as TVamKnob).DisabledImage   := bm3;

    (c as TVamKnob).ModLineDist  := 12;
    (c as TVamKnob).ModLineWidth := 3.5;
    (c as TVamKnob).IndicatorDist := 5;
    (c as TVamKnob).IndicatorSize := 2;

    (c as TVamKnob).IsBipolarKnob := false;
    (c as TVamKnob).ModLineColor := kModLineColorA;
    (c as TVamKnob).ModLineOffColor := kModLineColorOff;
  end;
  //==============

  TabPanel.Invalidate;
  //==============

  Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnPostCreateFinished);
  Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.Command_UpdateGUI);
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

  rd := FindRegionToDisplay(Plugin);
  FeedbackData.FocusedRegion := rd.Region;

  Plugin.GetGuiFeedBack(FeedbackData);

  MenuBarFrame.UpdateGui(Sender, @FeedbackData);
  MiniSampleDisplayFrame.UpdateGui(Sender, @FeedbackData);
  ModControlFrame.UpdateGui(Sender, @FeedbackData);
  SequencerFrame.UpdateGui(Sender, @FeedbackData);
  VoiceSetupFrame.UpdateGui(Sender, @FeedbackData);
  VoiceControlFrame.UpdateGui(Sender, @FeedbackData);

  GuiStandard.UpdateControls;

  Manually := false;
end;

procedure TPluginGui.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  if MsgID = TLucidMsgId.SampleFocusChanged then
  begin
    SampleMapFrame.UpdateSampleRegions;
    MiniSampleDisplayFrame.UpdateSampleDisplay;
  end;

  if MsgID = TLucidMsgId.SampleRegionChanged then
  begin
    SampleMapFrame.UpdateSampleRegions;
    SampleMapFrame.UpdateRootNoteKeys;
    SampleMapFrame.UpdateRegionInfoDisplay;
    MiniSampleDisplayFrame.UpdateSampleDisplay;
  end;

  if MsgID = TLucidMsgId.MidiKeyChanged then
  begin
    SampleMapFrame.MidiKeyChanged;
  end;

  if MsgID = TLucidMsgId.MouseOverSampleRegionChanged then
  begin
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
  if MsgID = TLucidMsgID.Command_CloseCurrentDialog then CloseCurrentDialog;

  if MsgID = TLucidMsgID.GUILayoutChanged then UpdateLayout;

  if MsgID = TLucidMsgID.Command_BeginGuiUpdate
    then MainPanel.BeginUpdate;

  if MsgID = TLucidMsgID.Command_EndGuiUpdate
    then MainPanel.EndUpdate;


end;


procedure TPluginGui.SetLowerTabState(const Value: TLowerTabOptions);
begin
  fLowerTabState := Value;

  case Value of
    TLowerTabOptions.TabMain:  TabPanel.TabIndex := 0;
    TLowerTabOptions.TabSeq1:  TabPanel.TabIndex := 1;
    TLowerTabOptions.TabSeq2:  TabPanel.TabIndex := 2;
    TLowerTabOptions.TabSetup: TabPanel.TabIndex := 3;
  else
    raise Exception.Create('Unexpect tab value.');
  end;

  case Value of
    TLowerTabOptions.TabMain:
    begin
      SequencerFrame.BackgroundPanel.Visible  := false;
      ModControlFrame.BackgroundPanel.Visible := true;
      VoiceSetupFrame.BackgroundPanel.Visible := false;
    end;

    TLowerTabOptions.TabSeq1:
    begin
      SequencerFrame.SequencerIndex := 0;
      SequencerFrame.BackgroundPanel.Visible  := true;
      ModControlFrame.BackgroundPanel.Visible := false;
      VoiceSetupFrame.BackgroundPanel.Visible := false;
    end;

    TLowerTabOptions.TabSeq2:
    begin
      SequencerFrame.SequencerIndex := 1;
      SequencerFrame.BackgroundPanel.Visible  := true;
      ModControlFrame.BackgroundPanel.Visible := false;
      VoiceSetupFrame.BackgroundPanel.Visible := false;
    end;

    TLowerTabOptions.TabSetup:
    begin
      SequencerFrame.BackgroundPanel.Visible  := false;
      ModControlFrame.BackgroundPanel.Visible := false;
      VoiceSetupFrame.BackgroundPanel.Visible := true;
    end
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
    0: TabSelected := TLowerTabOptions.TabMain;
    1: TabSelected := TLowerTabOptions.TabSeq1;
    2: TabSelected := TLowerTabOptions.TabSeq2;
    3: TabSelected := TLowerTabOptions.TabSetup;
  else
    raise Exception.Create('Error Message');
  end;

  Plugin.Globals.GuiState.LowerTabState := TabSelected;
  self.LowerTabState := TabSelected;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.RefreshRequest_StepSeqDisplay);
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

procedure TPluginGui.ShowSampleMapEdit;
begin
  if not assigned(Plugin) then exit;


  Plugin.Globals.GuiState.MainGuiLayout := TMainGuiLayout.MapEdit;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GUILayoutChanged);

  // NOTE: Logically it feels like SampleMapFrame.UpdateRootNoteKeys
  // should be called after UpdateLayout. But there is a noticable
  // lag between the layout becoming updated and the root note keys
  // being updated. Therefore I've decided to update the root keys
  // before changing the layout.
  // Maybe in future this could be avoid by using some sort of
  // nested BeginUpdate/EndUpdate setup. IE. A control doesn't
  // update if any of it's parent controls are in an update stage.
  SampleMapFrame.UpdateRootNoteKeys;
end;

procedure TPluginGui.HideSampleMapEdit;
var
  kg : IKeyGroup;
  aRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  Plugin.Globals.GuiState.MainGuiLayout := TMainGuiLayout.Default;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GUILayoutChanged);

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
        MiniSampleDisplayFrame.UpdateSampleDisplay;
      end;
    end;
  end;
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
  VoiceSetupFrame.BackgroundPanel.Color        := kPanelLight;
  VoiceControlFrame.BackgroundPanel.Color      := kPanelLight;
  ModSystem2Frame.BackgroundPanel.Color        := kPanelLight;


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
      ClearPadding(self.SideWorkArea);
        ClearPadding(self.SidePanel);
        ClearPadding(self.TitlePanel);

  ClearPadding(MenuBarFrame.BackgroundPanel);
  ClearPadding(MiniSampleDisplayFrame.BackgroundPanel);
  ClearPadding(SampleMapFrame.BackgroundPanel);
  ClearPadding(FileBrowserFrame.BackgroundPanel);
  ClearPadding(AboutFrame.BackgroundPanel);
  ClearPadding(VoiceControlFrame.BackgroundPanel);
  ClearPadding(ModControlFrame.BackgroundPanel);
  ClearPadding(SequencerFrame.BackgroundPanel);
  ClearPadding(VoiceSetupFrame.BackgroundPanel);
  ClearPadding(ModSystem2Frame.BackgroundPanel);


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

  VoiceSetupFrame.BackgroundPanel.CornerRadius1 := 3;
  VoiceSetupFrame.BackgroundPanel.CornerRadius2 := 3;
  VoiceSetupFrame.BackgroundPanel.CornerRadius3 := 0;
  VoiceSetupFrame.BackgroundPanel.CornerRadius4 := 0;

  TabPanel.CornerRadius1 := 0;
  TabPanel.CornerRadius2 := 0;
  TabPanel.CornerRadius3 := 3;
  TabPanel.CornerRadius4 := 3;

  //============== Set main panels to correct dimensions ================================



  //============================================================================
  //============== Dynamic GUI Setup ================================
  //============================================================================
  MainWorkArea.Width := 600 + MainWorkArea.Padding.Left + MainWorkArea.Padding.right;
  WorkAreaWidth := MainWorkArea.Width - MainWorkArea.Padding.Left - MainWorkArea.Padding.Right;

  MainMenuBar.Left     := MainWorkArea.Padding.Left;
  SampleMapDiv.Left    := MainWorkArea.Padding.Left;
  MainTop.Left         := MainWorkArea.Padding.Left;
  VoiceControlDiv.Left := MainWorkArea.Padding.Left;

  TabPanel.Left        := MainWorkArea.Padding.Left;
  ModSystem2Div.Left   := MainWorkArea.Padding.Left;


  MainMenuBar.Width     := WorkAreaWidth;
  SampleMapDiv.Width    := WorkAreaWidth;
  MainTop.Width         := WorkAreaWidth;
  VoiceControlDiv.Width := WorkAreaWidth;
  TabPanel.Width        := WorkAreaWidth;
  ModSystem2Div.Width   := WorkAreaWidth;

  TitlePanel.Height      := 28;
  MainMenuBar.Height     := 28;
  MainTop.Height         := 208;
  VoiceControlDiv.Height := 80;
  TabPanel.Height        := 252;
  ModSystem2Div.Height   := 70;



  //SampleMapDiv.Height    := 258;
  SampleMapDiv.Height    := 406;



  case Plugin.Globals.GuiState.MainGuiLayout of
    TMainGuiLayout.Default:
    begin
      MiniSampleDisplayFrame.UsageContext := TUsageContext.General;
      MainTop.Height         := 208;

      MainTop.Visible := true;

      SampleMapDiv.Visible    := false;
      VoiceControlDiv.Visible := true;
      TabPanel.Visible        := true;
      ModSystem2Div.Visible   := true;

      MainMenuBar.Top := 1;
      MainMenuBar.Left := 1;


      MainTop.Layout.Anchor(MainMenuBar).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
      VoiceControlDiv.Layout.Anchor(MainTop).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
      TabPanel.Layout.Anchor(VoiceControlDiv).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
      ModSystem2Div.Layout.Anchor(TabPanel).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
    end;

    TMainGuiLayout.SampleZoom:
    begin
      MiniSampleDisplayFrame.UsageContext := TUsageContext.SampleZoom;
      MainTop.Height := 208 + 80 + 2 + 70 + 2;


      MainTop.Visible := true;
      SampleMapDiv.Visible    := false;
      VoiceControlDiv.Visible := false;
      TabPanel.Visible        := true;
      ModSystem2Div.Visible := false;

      MainMenuBar.Top := 1;
      MainMenuBar.Left := 1;

      MainTop.Layout.Anchor(MainMenuBar).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
      TabPanel.Layout.Anchor(MainTop).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
      ModSystem2Div.Layout.Anchor(TabPanel).SnapToEdge(VamLayoutWizard.TControlFeature.BottomEdge).Move(0,2);
    end;

    TMainGuiLayout.MapEdit:
    begin
      //==========================================================================
      MiniSampleDisplayFrame.UsageContext := TUsageContext.General;
      MainTop.Height         := 208;

      SampleMapDiv.Visible    := true;
      VoiceControlDiv.Visible := false;
      TabPanel.Visible        := false;
      ModSystem2Div.Visible   := false;


      MainMenuBar.Top := 1;
      MainMenuBar.Left := 1;
      Erector.Init(MainMenuBar, MainTop).SnapToEdge(cfBottomEdge).Move(0,2);
      Erector.Init(MainTop, SampleMapDiv).SnapToEdge(cfBottomEdge).Move(0,2);
      //==========================================================================
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

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
  if DialogDisplayArea.AllowClose then
  begin
    DialogDisplayArea.Hide;
  end;
end;






end.
