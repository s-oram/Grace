                                                                                             unit uVoiceControlFrame;

interface

uses
  VamLib.ZeroObject,
  eeGuiStandardv2,
  uGuiFeedbackData, eePlugin, Menus, uConstants,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamKnob, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamDiv, VamTextBox, VamImage, VamMiniLevelMeter,
  Vcl.ExtCtrls;

type
  TVoiceControlFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    VoiceControlsContainer: TVamDiv;
    VoiceControlsLabel: TVamLabel;
    GrainStretchControls: TVamDiv;
    GrainStretchLabel: TVamLabel;
    GrainSizeLabel: TVamLabel;
    GrainRateLabel: TVamLabel;
    GrainPosLabel: TVamLabel;
    GrainLoopLabel: TVamLabel;
    GrainLoopTextBox: TVamTextBox;
    GrainLengthKnob: TVamKnob;
    GrainRateKnob: TVamKnob;
    GrainPosKnob: TVamKnob;
    OscillatorControls: TVamDiv;
    OscillatorLabel: TVamLabel;
    VamLabel1: TVamLabel;
    VamLabel2: TVamLabel;
    OscShapeKnob: TVamKnob;
    OscPulseWidthKnob: TVamKnob;
    OneShotSampleControls: TVamDiv;
    SampleOneShotLabel: TVamLabel;
    PlaybackTypeLabel: TVamLabel;
    SamplePlaybackTypeTextbox: TVamTextBox;
    ResetLabel: TVamLabel;
    ResetTextBox: TVamTextBox;
    VoicePitch1Knob: TVamKnob;
    VoicePitch1Label: TVamLabel;
    VoicePitch2Label: TVamLabel;
    VoicePitch2Knob: TVamKnob;
    SamplerLoopBoundsTextBox: TVamTextBox;
    SamplerLoopBoundsLabel: TVamLabel;
    VoiceModeTextBox: TVamTextBox;
    VoiceModeLabel: TVamLabel;
    GlideKnob: TVamKnob;
    GlideLabel: TVamLabel;
    SamplerLoopModeLabel: TVamLabel;
    SamplerLoopModeTextBox: TVamTextBox;
    PitchTrackTextBox: TVamTextBox;
    PitchTrackLabel: TVamLabel;
    MainOutputKnob: TVamKnob;
    MainOutputLabel: TVamLabel;
    MainPanKnob: TVamKnob;
    MainPanLabel: TVamLabel;
    VoiceLevelMeter: TVamMiniLevelMeter;
    Timer1: TTimer;
    procedure VoiceControlsContainerResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    procedure ShowPlayTypeMenuCallBack(aMenu : TMenu);
    procedure ShowSamplResetMenuCallBack(aMenu : TMenu);
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    procedure UpdateControlVisibility;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;


    procedure PlaybackTypeChanged;
  end;

implementation

{$R *.dfm}

uses
  Lucidity.PluginParameters,
  VamGuiControlInterfaces,
  eeVstParameter,
  eeGuiHelpers,
  Lucidity.SampleMap,
  VamLayoutWizard,
  Lucidity.Interfaces,
  RedFoxColor,
  uGuiUtils,
  uLucidityEnums,
  Lucidity.KeyGroup;

{ TVoiceControlFrame }

constructor TVoiceControlFrame.Create(AOwner: TComponent);
begin
  inherited;
  VoicePitch1Knob.VisibleSteps := 24; //24 steps reflects tuning by +/-12 semitones.


  //misc.
  GlideKnob.MinModDepth := 0;
  GlideKnob.MaxModDepth := 0;
end;


destructor TVoiceControlFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  inherited;
end;

procedure TVoiceControlFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  NameA, NameB : string;
  PMenu : ^TMenu;
begin
  if MsgID = TLucidMsgID.SampleOscTypeChanged            then UpdateControlVisibility;
  if MsgID = TLucidMsgID.Command_UpdateControlVisibility then UpdateControlVisibility;

  if MsgID = TLucidMsgID.OnShowMenu then
  begin
    NameA := PMsgData_ShowMenuEvent(Data)^.MenuName;
    PMenu := PMsgData_ShowMenuEvent(Data)^.Menu;


    NameB := PluginParToName(TPluginParameter.SamplePlaybackType);
    if NameA = NameB
      then ShowPlayTypeMenuCallBack(PMenu^);


    NameB := PluginParToName(TPluginParameter.SampleResetClockSource);
    if NameA = NameB
      then ShowSamplResetMenuCallBack(PMenu^);
  end;




  if MsgID = TLucidMsgID.Command_DisposeKeyGroup then
  begin
    VoiceLevelMeter.LevelMonitor := nil;
  end;

end;



procedure TVoiceControlFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;


  //==== Assign standard control handling ====
  GuiStandard_RegisterControl(aGuiStandard, MainOutputKnob,         TPluginParameter.OutputGain);
  GuiStandard_RegisterControl(aGuiStandard, MainPanKnob,            TPluginParameter.OutputPan);
  GuiStandard_RegisterControl(aGuiStandard, VoicePitch1Knob,        TPluginParameter.VoicePitchOne);
  GuiStandard_RegisterControl(aGuiStandard, VoicePitch2Knob,        TPluginParameter.VoicePitchTwo);
  GuiStandard_RegisterControl(aGuiStandard, GlideKnob,              TPluginParameter.VoiceGlide);
  //GuiStandard_RegisterControl(aGuiStandard, GrainLengthKnob,        TPluginParameter.GrainLength);
  //GuiStandard_RegisterControl(aGuiStandard, GrainRateKnob,          TPluginParameter.GrainRate);
  //GuiStandard_RegisterControl(aGuiStandard, GrainPosKnob,           TPluginParameter.GrainPosition);
  //GuiStandard_RegisterControl(aGuiStandard, OscShapeKnob,           TPluginParameter.OscShape);
  //GuiStandard_RegisterControl(aGuiStandard, OscPulseWidthKnob,      TPluginParameter.OscPulseWidth);

  GuiStandard_RegisterMenuButton(aGuiStandard, VoiceModeTextBox,            TPluginParameter.VoiceMode);
  GuiStandard_RegisterMenuButton(aGuiStandard, SamplePlaybackTypeTextBox,   TPluginParameter.SamplePlaybackType);     // NOTE: Using ShowPlayTypeMenuCallBack().
  GuiStandard_RegisterMenuButton(aGuiStandard, PitchTrackTextBox,           TPluginParameter.PitchTracking);
  GuiStandard_RegisterMenuButton(aGuiStandard, ResetTextBox,                TPluginParameter.SampleResetClockSource); // NOTE: Using ShowSamplResetMenuCallBack().
  GuiStandard_RegisterMenuButton(aGuiStandard, SamplerLoopModeTextBox,      TPluginParameter.SamplerLoopMode);
  GuiStandard_RegisterMenuButton(aGuiStandard, SamplerLoopBoundsTextBox,    TPluginParameter.SamplerLoopBounds);
  //GuiStandard_RegisterControl(aGuiStandard, GrainLoopTextBox,            TPluginParameter.GrainLoop);


  //============================================================================
  //                         GUI Stylings
  //============================================================================
  OscillatorControls.Visible := false;


  //== Hide osc container labels ==
  GrainStretchLabel.Visible  := false;
  OscillatorLabel.Visible    := false;
  SampleOneShotLabel.Visible := false;

  GrainStretchControls.Align   := alTop;
  OscillatorControls.Align     := alTop;
  OneShotSampleControls.Align  := alTop;

  GrainStretchControls.AlignWithMargins := true;
  OscillatorControls.AlignWithMargins := true;
  OneShotSampleControls.AlignWithMargins := true;

  GrainStretchControls.Margins.SetBounds(0,16,0,0);
  OscillatorControls.Margins.SetBounds(0,16,0,0);
  OneShotSampleControls.Margins.SetBounds(0,16,0,0);


  OscillatorControls.Visible     := false;
  GrainStretchControls.Visible   := false;
  OneShotSampleControls.Visible  := false;



  //=== skinning setup ====
  PitchTrackTextBox.Font.Color         := GetRedFoxColor(kColor_LcdDark5);
  SamplePlaybackTypeTextBox.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  ResetTextBox.Font.Color              := GetRedFoxColor(kColor_LcdDark5);
  VoiceModeTextBox.Font.Color          := GetRedFoxColor(kColor_LcdDark5);
  GrainLoopTextBox.Font.Color          := GetRedFoxColor(kColor_LcdDark5);
  SamplerLoopModeTextBox.Font.Color    := GetRedFoxColor(kColor_LcdDark5);
  SamplerLoopBoundsTextBox.Font.Color  := GetRedFoxColor(kColor_LcdDark5);

  PitchTrackTextBox.Color              := kColor_LcdDark1;
  SamplePlaybackTypeTextBox.Color      := kColor_LcdDark1;
  ResetTextBox.Color                   := kColor_LcdDark1;
  VoiceModeTextBox.Color               := kColor_LcdDark1;
  GrainLoopTextBox.Color               := kColor_LcdDark1;
  SamplerLoopModeTextBox.Color         := kColor_LcdDark1;
  SamplerLoopBoundsTextBox.Color       := kColor_LcdDark1;

  PitchTrackTextBox.ColorMouseOver  := kColor_ButtonMouseOver;
  SamplePlaybackTypeTextBox.ColorMouseOver  := kColor_ButtonMouseOver;
  ResetTextBox.ColorMouseOver               := kColor_ButtonMouseOver;
  VoiceModeTextBox.ColorMouseOver           := kColor_ButtonMouseOver;
  GrainLoopTextBox.ColorMouseOver           := kColor_ButtonMouseOver;
  SamplerLoopModeTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  SamplerLoopBoundsTextBox.ColorMouseOver   := kColor_ButtonMouseOver;


  //=====    Main Voice Controls    =======================

  VoiceControlsContainer.Align := alClient;
  VoiceControlsContainer.AlignWithMargins := true;
  VoiceControlsContainer.Margins.SetBounds(0,0,0,0);

  VoiceLevelMeter.Layout.SetPos(0,18);
  VoiceLevelMeter.Layout.SetSize(8,32+12);
  VoiceLevelMeter.BringToFront;


  //** Gain, Pan, Tune, Fine **
  MainOutputKnob.Layout.SetSize(32,32);
  MainPanKnob.Layout.SetSize(32,32);
  VoicePitch1Knob.Layout.SetSize(32,32);
  VoicePitch2Knob.Layout.SetSize(32,32);

  MainOutputKnob.Layout.SetPos(12,18);
  MainPanKnob.Layout.Anchor(MainOutputKnob).SnapToEdge(TControlFeature.RightEdge);
  VoicePitch1Knob.Layout.Anchor(MainPanKnob).SnapToEdge(TControlFeature.RightEdge).Move(8, 0);
  VoicePitch2Knob.Layout.Anchor(VoicePitch1Knob).SnapToEdge(TControlFeature.RightEdge);

  MainOutputLabel.Layout.Anchor(MainOutputKnob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  MainPanLabel.Layout.Anchor(MainPanKnob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  VoicePitch1Label.Layout.Anchor(VoicePitch1Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  VoicePitch2Label.Layout.Anchor(VoicePitch2Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);


  // ** Sample Playback, Loop, Bounds, Reset **
  //SamplePlaybackTypeTextBox.Layout.SetSize(32 * 3, 16);
  //SamplePlaybackTypeTextBox.Layout.Anchor(VoicePitch2Knob).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge).Move(8,0);
  //PlaybackTypeLabel.Layout.Anchor(SamplePlaybackTypeTextBox).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  PitchTrackTextBox.Layout.SetSize(32 * 2, 16);
  PitchTrackTextBox.Layout.Anchor(VoicePitch2Knob).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge).Move(8,0);
  PitchTrackLabel.Layout.Anchor(PitchTrackTextBox).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  SamplerLoopModeTextBox.Layout.SetSize(32 * 2, 16);
  SamplerLoopModeTextBox.Layout.Anchor(PitchTrackTextBox).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  SamplerLoopModeLabel.Layout.SetSize(32 * 2, 16).Anchor(SamplerLoopModeTextBox).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  SamplerLoopBoundsTextBox.Layout.SetSize(32 * 2, 16);
  SamplerLoopBoundsTextBox.Layout.Anchor(SamplerLoopModeTextBox).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  SamplerLoopBoundsLabel.Layout.SetSize(32 * 2, 16).Anchor(SamplerLoopBoundsTextBox).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  ResetTextBox.Layout.SetSize(64, 16);
  ResetTextBox.Layout.Anchor(SamplerLoopBoundsTextBox).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge).Move(8,0);
  ResetLabel.Layout.Anchor(ResetTextBox).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);



  //== finally, call the message handlers to ensure everything is up to date ===
  UpdateControlVisibility;

  Timer1.Enabled := true;
  Timer1.Interval := 40;
end;

procedure TVoiceControlFrame.VoiceControlsContainerResize(Sender: TObject);
begin
  // ** Voice mode, Glide **
  GlideKnob.Layout.SetSize(32,32);
  GlideKnob.Layout.Anchor(VoicePitch2Knob).AlignEdge(TControlFeature.BottomEdge);
  GlideKnob.Left := GlideKnob.Parent.Width - GlideKnob.Width;
  GlideLabel.Layout.Anchor(GlideKnob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  VoiceModeTextBox.Layout.SetSize(64, 16);
  VoiceModeTextBox.Layout.Anchor(GlideKnob).SnapToEdge(TControlFeature.LeftEdge).AlignEdge(TControlFeature.BottomEdge).Move(-8,0);
  VoiceModeLabel.Layout.Anchor(VoiceModeTextBox).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
end;





procedure TVoiceControlFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TVoiceControlFrame.ShowPlayTypeMenuCallBack(aMenu: TMenu);
var
  mi : TMenuItem;
begin
  mi := MenuHelper(aMenu).FindItemByName('miNoteSampler');
  if assigned(mi)
    then mi.Caption := mi.Caption + ' (Pitch Tracks Keyboard)';

  mi := MenuHelper(aMenu).FindItemByName('miLoopSampler');
  if assigned(mi)
    then mi.Caption := mi.Caption + ' (Loop Syncs To Tempo)';

  mi := MenuHelper(aMenu).FindItemByName('miOneShotSampler');
  if assigned(mi)
    then mi.Caption := mi.Caption + ' (No Pitch Tracking)';

  mi := MenuHelper(aMenu).FindItemByName('miGrainStretch');
  if assigned(mi)
    then mi.Visible := false;

  mi := MenuHelper(aMenu).FindItemByName('miWaveOsc');
  if assigned(mi)
    then mi.Visible := false;

end;

procedure TVoiceControlFrame.ShowSamplResetMenuCallBack(aMenu: TMenu);
var
  mi : TMenuItem;
  c1: Integer;
begin
  for c1 := 0 to aMenu.Items.Count-1 do
  begin
    mi := aMenu.Items[c1];
    if mi.Name = 'miSampleLoop'
      then mi.Visible := false;

  end;

end;

procedure TVoiceControlFrame.Timer1Timer(Sender: TObject);
var
  kg : IKeyGroup;
  LM : ILevelMonitor;
begin
  // NOTE: TODO: there is something funky going on.
  // The LFO 2 - Par 2 knob always causes a crash.
  // Something here becomes un-defined.
  kg := Plugin.ActiveKeyGroup;
  if (assigned(kg)) and (Supports(kg, ILevelMonitor, LM)) then
  begin
    VoiceLevelMeter.LevelMonitor := LM;
    VoiceLevelMeter.Invalidate;
  end;
end;

procedure TVoiceControlFrame.UpdateControlVisibility;
var
  vm : TVoiceMode;
  PlaybackType :  TSamplePlaybackType;
begin
  if not assigned(Plugin) then exit;

  PlaybackType := Command.GetParValue<TSamplePlaybackType>(Plugin, TPluginParameter.SamplePlaybackType);

  case PlaybackType of
    TSamplePlaybackType.NoteSampler:
    begin
      VoicePitch1Knob.Enabled := true;
      VoicePitch2Knob.Enabled := true;

      VoicePitch1Label.Visible := true;
      VoicePitch2Label.Visible := true;
    end;

    TSamplePlaybackType.LoopSampler:
    begin
      VoicePitch1Knob.Enabled := false;
      VoicePitch2Knob.Enabled := false;

      VoicePitch1Label.Visible := false;
      VoicePitch2Label.Visible := false;
    end;

    TSamplePlaybackType.OneShotSampler:
    begin
      VoicePitch1Knob.Enabled := true;
      VoicePitch2Knob.Enabled := true;

      VoicePitch1Label.Visible := true;
      VoicePitch2Label.Visible := true;
    end;
    //TSamplePlaybackType.GrainStretch
    //TSamplePlaybackType.WaveOsc;:
  else
    raise Exception.Create('Type not handled.');
  end;


  vm :=  Command.GetParValue<TVoiceMode>(Plugin, TPluginParameter.VoiceMode);
  case vm of
    TVoiceMode.Poly:
    begin
      GlideKnob.IsKnobEnabled := false;
      GlideLabel.Visible := false;
    end;

    TVoiceMode.Mono:
    begin
      GlideKnob.IsKnobEnabled := true;
      GlideLabel.Visible := true;
    end;

    TVoiceMode.Legato:
    begin
      GlideKnob.IsKnobEnabled := true;
      GlideLabel.Visible := true;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;



end;

procedure TVoiceControlFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
end;

procedure TVoiceControlFrame.PlaybackTypeChanged;
var
  SampleOscType : TSamplePlaybackType;
begin
  if not assigned(Plugin) then exit;

  OneShotSampleControls.Visible := false;
  GrainStretchControls.Visible  := false;

  SampleOscType := Command.GetParValue<TSamplePlaybackType>(Plugin, TPluginParameter.SamplePlaybackType);

  case SampleOscType of
    TSamplePlaybackType.NoteSampler:    OneShotSampleControls.Visible := true;
    TSamplePlaybackType.LoopSampler:    OneShotSampleControls.Visible := true;
    TSamplePlaybackType.OneShotSampler: OneShotSampleControls.Visible := true;
    TSamplePlaybackType.GrainStretch:   GrainStretchControls.Visible  := true;
    TSamplePlaybackType.WaveOsc: ; //TODO
  else
    raise Exception.Create('SampleOscType not handled.');
  end;

end;


end.
