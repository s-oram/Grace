unit eePluginGui;

interface

{$INCLUDE Defines.inc}


uses
  Lucidity.GuiStandard,
  LucidityGui.KnobHandler,
  LucidityGui.MenuButtonHandler,
  Lucidity.Interfaces,
  VamLib.ZeroObject,
  uXYPadsFrame,
  uSequencerFrame,
  uModSystemFrame,
  Lucidity.SampleMap,
  eeEnumHelper, Lucidity.Enums,
  VamVst2.DAEffect, VamVst2.DAEffectX,
  eePluginKeyHook,
  Lucidity.GuiState,
  eePluginHotkeys,
  uConstants, uGuiFeedbackData,
  uSampleMapFrame, uFileBrowserFrame, uVoiceControlFrame,
  uMenuBarFrame,
  uModControlFrame, uMiniSampleDisplayFrame,
  eeFileBrowserAddon, eeRedFoxDropFileTarget,
  eePlugin,
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  RedFoxContainer, RedFoxWinControl, VamWinControl, VamPanel, VamDiv,
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
    MainTitleLabel: TVamLabel;
    VoiceControlDiv: TVamDiv;
    TabPanel: TVamTabPanel;
    SidePanel: TVamDiv;
    ModSystem2Div: TVamDiv;
    MainPanel: TVamPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LowerTabsChanged(Sender: TObject);
  private
    fPlugin: TeePlugin;
    fLowerTabState: TLowerTabOptions; //TODO:MED I think this can be deleted.
    fCurrentGuiState: TGuiState;
    procedure SetLowerTabState(const Value: TLowerTabOptions);
  private
    FMotherShip : IMothership;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface);
    procedure AddMotherShipReference(aMotherShip : IMothership);
    procedure RemoveMotherShipReference(aMotherShip : IMothership);
  protected
    Manually:boolean;
    GuiStandard : TGuiStandard;
    DropFileTarget : TRedFoxDropFileTarget;

    MenuBarFrame           : TMenuBarFrame;
    MiniSampleDisplayFrame : TMiniSampleDisplayFrame;
    SampleMapFrame         : TSampleMapFrame;
    FileBrowserFrame       : TFileBrowserFrame;
    VoiceControlFrame      : TVoiceControlFrame;
    ModControlFrame        : TModControlFrame;
    ModSystem2Frame        : TModSystemFrame;
    SequencerFrame         : TSequencerFrame;
    VoiceSetupFrame        : TXYPadsFrame;

    FeedbackData : TGuiFeedBackData;

    procedure WndProc(var Message: TMessage) ; override;

    procedure ShowSampleMapEdit;
    procedure HideSampleMapEdit;

    property LowerTabState : TLowerTabOptions read fLowerTabState write SetLowerTabState;
    property CurrentGuiState : TGuiState read fCurrentGuiState write fCurrentGuiState; // TODO:MED: I'm not sure if the GUI needs a copy of the Current GUI state object anymore.


  public
    procedure PostCreate(const aVstWindow : HWnd);
    procedure BeforeClose;
    procedure UpdateGui(Sender:TObject);

    property Plugin:TeePlugin read fPlugin write fPlugin;

    procedure UpdateLayout;

  end;

implementation

uses
  {$IFDEF Logging}VamLib.Logging,{$ENDIF}
  InWindowDialog,
  VamLayoutWizard,
  eePluginDataDir,
  Lucidity.KeyGroup,
  eeGuiSetup,
  RedFoxColor,
  Lucidity.GuiUtils,
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

  FeedbackData := TGuiFeedBackData.Create;

  CurrentGuiState := TGuiState.Create;

  //====== Initialize all frames ==============
  MenuBarFrame := TMenuBarFrame.Create(self);
  MenuBarFrame.BackgroundPanel.Parent := MainMenuBar;
  MenuBarFrame.BackgroundPanel.Align  := TAlign.alClient;
  MenuBarFrame.BackgroundPanel.Visible  := true;

  SampleMapFrame := TSampleMapFrame.Create(self);
  SampleMapFrame.BackgroundPanel.Parent := SampleMapDiv;
  SampleMapFrame.BackgroundPanel.Align  := TAlign.alClient;
  SampleMapFrame.BackgroundPanel.Visible  := true;

  MiniSampleDisplayFrame := TMiniSampleDisplayFrame.Create(self);
  MiniSampleDisplayFrame.BackgroundPanel.Parent := MainTop;
  MiniSampleDisplayFrame.BackgroundPanel.Align := alClient;
  MiniSampleDisplayFrame.BackgroundPanel.Padding.SetBounds(0,0,0,0);
  MiniSampleDisplayFrame.BackgroundPanel.Margins.SetBounds(0,0,0,0);
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius1 := 3;
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius2 := 3;
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius3 := 3;
  MiniSampleDisplayFrame.BackgroundPanel.CornerRadius4 := 3;

  VoiceControlFrame := TVoiceControlFrame.Create(self);
  VoiceControlFrame.BackgroundPanel.Parent := VoiceControlDiv;
  VoiceControlFrame.BackgroundPanel.Align := alClient;
  VoiceControlFrame.BackgroundPanel.Padding.SetBounds(0,0,0,0);
  VoiceControlFrame.BackgroundPanel.Margins.SetBounds(0,0,0,0);
  VoiceControlFrame.BackgroundPanel.CornerRadius1 := 3;
  VoiceControlFrame.BackgroundPanel.CornerRadius2 := 3;
  VoiceControlFrame.BackgroundPanel.CornerRadius3 := 3;
  VoiceControlFrame.BackgroundPanel.CornerRadius4 := 3;

  ModControlFrame := TModControlFrame.Create(self);
  ModControlFrame.BackgroundPanel.Parent := TabPanel;
  ModControlFrame.BackgroundPanel.Align  := alClient;
  ModControlFrame.BackgroundPanel.Padding.SetBounds(0,0,0,0);
  ModControlFrame.BackgroundPanel.Margins.SetBounds(0,0,0,0);
  ModControlFrame.BackgroundPanel.CornerRadius1 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius2 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius3 := 3;
  ModControlFrame.BackgroundPanel.CornerRadius4 := 3;
  ModControlFrame.BackgroundPanel.Visible := false;

  FileBrowserFrame := TFileBrowserFrame.Create(self);
  FileBrowserFrame.BackgroundPanel.Parent := SidePanel;
  FileBrowserFrame.BackgroundPanel.Align  := alClient;
  FileBrowserFrame.BackgroundPanel.Visible := true;

  ModSystem2Frame := TModSystemFrame.Create(self);
  ModSystem2Frame.BackgroundPanel.Align := alClient;
  ModSystem2Frame.BackgroundPanel.Visible := true;
  ModSystem2Frame.BackgroundPanel.Parent := ModSystem2Div;

  SequencerFrame := TSequencerFrame.Create(self);
  SequencerFrame.BackgroundPanel.Visible := true;
  SequencerFrame.BackgroundPanel.Parent := TabPanel;

  VoiceSetupFrame := TXYPadsFrame.Create(self);
  VoiceSetupFrame.BackgroundPanel.Visible := true;
  VoiceSetupFrame.BackgroundPanel.Parent := TabPanel;

  //======= Build the GUI =======
  DropFileTarget := TRedFoxDropFileTarget.Create(RedFoxContainer);
  DropFileTarget.RegisterTarget(SampleMapFrame.SampleMap); //TODO:MED SampleMapFrame is used and should be deleted.
  DropFileTarget.RegisterTarget(MiniSampleDisplayFrame.SampleOverlay);
  DropFileTarget.RegisterTarget(FileBrowserFrame.FileTreeView);
  //===================================


  // Finally...
  UpdateLayout;

  //=== update the font for all controls ===
  MainTitleLabel.Font.Name := 'Liberation Sans';

  self.Font.Name := 'Liberation Sans';
  self.Font.Size := 10;
  self.Font.Style := [];

  ApplyFontValues(MainTitleLabel);

  ApplyToAllControls(self, procedure(const c:TControl)
  begin
    ApplyFontValues(c);
  end);

  // Set form scaling to false to ignore Windows DPI changes.
  // http://stackoverflow.com/a/8298713/395461
  self.PixelsPerInch := 96;
  self.Scaled := false;

  MainTitleLabel.Font.Size := 19;
  MainTitleLabel.Font.Style := [fsBold];
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

  CurrentGuiState.Free;

  FreeAndNil(MenuBarFrame);
  FreeAndNil(SampleMapFrame);
  FreeAndNil(MiniSampleDisplayFrame);
  FreeAndNil(FileBrowserFrame);
  FreeAndNil(VoiceControlFrame);
  FreeAndNil(ModControlFrame);
  FreeAndNil(ModSystem2Frame);
  FreeAndNil(SequencerFrame);
  FreeAndNil(VoiceSetupFrame);

  DropFileTarget.Free;
  FeedBackData.Free;
end;

procedure TPluginGui.PostCreate(const aVstWindow : HWnd);
var
  VQ : IVamQuery;
  c : TControl;
  bm1 : TBitmap;
  bm2 : TBitmap;
  bm3 : TBitmap;
begin
  assert(assigned(Plugin));

  GuiStandard := TGuiStandard.Create(self, Plugin);

  // register self to mother ship.
  Plugin.Globals.MotherShip.RegisterZeroObject(self, TZeroObjectRank.VCL);

  {
  KnobHandler := TKnobHandler.Create(Plugin);
  GuiStandard.RegisterHandler('KnobHandler', KnobHandler);
  Plugin.Globals.MotherShip.RegisterZeroObject(KnobHandler, TZeroObjectRank.VCL);

  MenuHandler := TMenuButtonHandler.Create(Plugin);
  GuiStandard.RegisterHandler('MenuButtonHandler', MenuHandler);
  Plugin.Globals.MotherShip.RegisterZeroObject(MenuHandler, TZeroObjectRank.VCL);
  }

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

  // Initalize all the frame controls...
  MiniSampleDisplayFrame.InitializeFrame(Plugin, GuiStandard);
  FileBrowserFrame.InitializeFrame(Plugin, GuiStandard);
  MenuBarFrame.InitializeFrame(Plugin, GuiStandard);
  SampleMapFrame.InitializeFrame(Plugin, GuiStandard);
  ModControlFrame.InitializeFrame(Plugin, GuiStandard);
  ModSystem2Frame.InitializeFrame(Plugin, GuiStandard);
  SequencerFrame.InitializeFrame(Plugin, GuiStandard);
  VoiceSetupFrame.InitializeFrame(Plugin, GuiStandard);
  VoiceControlFrame.InitializeFrame(Plugin, GuiStandard);

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

  Plugin.Globals.GuiState.HotKeyContext := THotKeyContext.None;

  //self.TabPanel.Font.PixelsPerInch := 96;

  //self.TabPanel.Font.PixelsPerInch := 200;
  self.TabPanel.Font.Size := 8;
  self.Scaled := false;
end;

procedure TPluginGui.BeforeClose;
begin
end;

procedure TPluginGui.AddMotherShipReference(aMotherShip: IMothership);
begin
  if assigned(FMotherShip) then raise Exception.Create('MotherShip reference already set. Cannot assign again.');
  FMotherShip := aMotherShip;
end;

procedure TPluginGui.RemoveMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := nil;
end;






procedure TPluginGui.UpdateGui(Sender: TObject);
var
  rd:TRegionDisplayResult;
begin
  //Update the gui elements here.
  Manually := true;

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

procedure TPluginGui.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB:IInterface);
var
  MsgText : string;
begin
  if (MsgID = TLucidMsgID.OnPostCreateFinished) and (not PluginDataDir^.Exists) then
  begin
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, 'Data directory not found. Please re-install.');
  end;

  if MsgID = TLucidMsgId.MidiKeyChanged then
  begin
    SampleMapFrame.MidiKeyChanged;
  end;

  if MsgID = TLucidMsgID.PreviewInfoChanged then
  begin
    FileBrowserFrame.PreviewInfoChanged;
  end;

  if MsgID = TLucidMsgID.SampleMarkersChanged then MiniSampleDisplayFrame.GuiEvent_SampleMakersChanged;
  if MsgID = TLucidMsgID.SampleOscTypeChanged then VoiceControlFrame.PlaybackTypeChanged;

  if MsgID = TLucidMsgID.Command_ShowSampleMapEdit then ShowSampleMapEdit;
  if MsgID = TLucidMsgID.Command_HideSampleMapEdit then HideSampleMapEdit;

  if MsgID = TLucidMsgID.GUILayoutChanged then UpdateLayout;

  if MsgID = TLucidMsgID.Command_BeginGuiUpdate then MainPanel.BeginUpdate;
  if MsgID = TLucidMsgID.Command_EndGuiUpdate   then MainPanel.EndUpdate;

  if (MsgID = TLucidMsgID.OnPostCreateFinished) then
  begin
    //Do noting... might do something in here later.
  end;

  if (MsgID = TLucidMsgID.Command_ShowMessage) then
  begin
    MsgText := (DataB as ICustomLucidityMessage).GetMsg();
    InWindow_ShowMessage(Plugin.Globals.TopLevelForm, MsgText );
  end;
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

        //TODO:MED this would more appropiiately be a Commmand_UpdateSampleDisplayFocus
        // to say the GUI needs to reconsider what region should be in focus.
        Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateSampleDisplay);
      end;
    end;
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
  ClearPadding(VoiceControlFrame.BackgroundPanel);
  ClearPadding(ModControlFrame.BackgroundPanel);
  ClearPadding(SequencerFrame.BackgroundPanel);
  ClearPadding(VoiceSetupFrame.BackgroundPanel);
  ClearPadding(ModSystem2Frame.BackgroundPanel);

  MainPanel.CornerRadius1 := 0;
  MainPanel.CornerRadius2 := 0;
  MainPanel.CornerRadius3 := 0;
  MainPanel.CornerRadius4 := 0;

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

  //============================================================================
  // Set internal padding of a few panels to ensure consistenency.
  //============================================================================

  FileBrowserFrame.BackgroundPanel.Padding.SetBounds(4,4,4,4);
  SampleMapFrame.BackgroundPanel.Padding.SetBounds(4,4,4,4);
  MiniSampleDisplayFrame.BackgroundPanel.Padding.SetBounds(4,4,4,4);
  MiniSampleDisplayFrame.InsidePanel.Padding.SetBounds(4,4,4,2);
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
  SampleMapDiv.Height    := 406;

  case Plugin.Globals.GuiState.MainGuiLayout of
    TMainGuiLayout.Default:
    begin
      MainTop.Height  := 226;
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
      MainTop.Height := 226;

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

    TMainGuiLayout.MapEdit:
    begin
      //==========================================================================
      MainTop.Height := 226;

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



procedure TPluginGui.WndProc(var Message: TMessage);
begin

  inherited;
end;




end.
