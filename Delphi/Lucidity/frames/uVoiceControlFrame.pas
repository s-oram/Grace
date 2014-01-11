unit uVoiceControlFrame;

interface

uses
  uGuiFeedbackData, eePlugin, eeGuiStandard, Menus, uConstants,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamKnob, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamDiv, VamTextBox, VamImage;

type
  TVoiceControlFrame = class(TFrame)
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
    MainOutputKnob: TVamKnob;
    MainOutputLabel: TVamLabel;
    MainPanLabel: TVamLabel;
    MainPanKnob: TVamKnob;
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
    procedure VoiceControlsContainerResize(Sender: TObject);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);

    procedure ShowPlayTypeMenuCallBack(aMenu : TMenu);
    procedure ShowSamplResetMenuCallBack(aMenu : TMenu);
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
  eeVstParameter,
  eeGuiStandard_MenuController,
  eeGuiHelpers,
  Lucidity.SampleMap,
  VamLayoutWizard,
  uLucidityKeyGroupInterface,
  RedFoxColor,
  uGuiUtils,
  uLucidityEnums,
  uLucidityKeyGroup;

{ TVoiceControlFrame }

constructor TVoiceControlFrame.Create(AOwner: TComponent);
begin
  inherited;
  VoicePitch1Knob.VisibleSteps := 24; //24 steps reflects tuning by +/-12 semitones.

  // NOTE: AFAIK TFrame should be receive a windows handle at some stage.
  // for whatever reason this TFrame instance wasn't receiving a handle and
  // i couldn't figure out why. This is a work-around so that the frame
  // can receive messages posted by the EasyEffect Globals class.
  MsgHandle := AllocateHWND(MessageHandler);
end;


destructor TVoiceControlFrame.Destroy;
begin
  if (MsgHandle <> 0) and (assigned(Plugin)) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(MsgHandle);
  end;
  DeallocateHWnd(MsgHandle);

  inherited;
end;

procedure TVoiceControlFrame.MessageHandler(var Message: TMessage);
begin
  if Message.Msg = UM_Update_Control_Visibility then UpdateControlVisibility;
  if Message.Msg = UM_SAMPLE_OSC_TYPE_CHANGED   then UpdateControlVisibility;
end;

procedure TVoiceControlFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
var
  ParIndex : integer;
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  if MsgHandle <> 0 then
  begin
    Plugin.Globals.AddWindowsMessageListener(MsgHandle);
  end;

  UpdateControlVisibility;


  VoicePitch1Knob.Tag     := fPlugin.Globals.VstParameters.FindParameterIndexByName('VoicePitchOne');
  VoicePitch2Knob.Tag     := fPlugin.Globals.VstParameters.FindParameterIndexByName('VoicePitchTwo');
  GlideKnob.Tag           := fPlugin.Globals.VstParameters.FindParameterIndexByName('VoiceGlide');

  GrainLengthKnob.Tag     := fPlugin.Globals.VstParameters.FindParameterIndexByName('GrainLength');
  GrainRateKnob.Tag       := fPlugin.Globals.VstParameters.FindParameterIndexByName('GrainRate');
  GrainPosKnob.Tag        := fPlugin.Globals.VstParameters.FindParameterIndexByName('GrainPosition');

  OscShapeKnob.Tag      := fPlugin.Globals.VstParameters.FindParameterIndexByName('OscShape');
  OscPulseWidthKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('OscPulseWidth');

  MainOutputKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('OutputGain');
  MainPanKnob.Tag    := fPlugin.Globals.VstParameters.FindParameterIndexByName('OutputPan');


  //== General Voice Controls ==
  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('VoiceMode');
  fGuiStandard.RegisterControlAsMenuControl(VoiceModeTextBox, ParIndex, TVoiceModeHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('SamplePlaybackType');
  fGuiStandard.RegisterControlAsMenuControl(SamplePlaybackTypeTextBox, ParIndex, TSamplePlaybackTypeHelper, TShowMenu.ContextClickOnly, ShowPlayTypeMenuCallBack);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PitchTracking');
  fGuiStandard.RegisterControlAsMenuControl(PitchTrackTextBox, ParIndex, TPitchTrackingHelper, TShowMenu.ContextClickOnly, ShowPlayTypeMenuCallBack);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('SampleResetClockSource');
  fGuiStandard.RegisterControlAsMenuControl(ResetTextBox, ParIndex, TClockSourceHelper, TShowMenu.ContextClickOnly, ShowSamplResetMenuCallBack);

  fGuiStandard.RegisterControlForAutoUpdate(VoicePitch1Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(VoicePitch2Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(GlideKnob, true);

  fGuiStandard.RegisterControlForAutoUpdate(MainOutputKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(MainPanKnob, true);

  //== One Shot Sample Controls ==
  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('SamplerLoopMode');
  fGuiStandard.RegisterControlAsMenuControl(SamplerLoopModeTextBox, ParIndex, TSamplerLoopModeHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('SamplerLoopBounds');
  fGuiStandard.RegisterControlAsMenuControl(SamplerLoopBoundsTextBox, ParIndex, TSamplerLoopBoundsHelper, TShowMenu.ContextClickOnly);



  //== Grain Stretch Osc Controls ==
  fGuiStandard.RegisterControlForAutoUpdate(GrainLengthKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(GrainRateKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(GrainPosKnob, true);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('GrainLoop');
  fGuiStandard.RegisterControlAsMenuControl(GrainLoopTextBox, ParIndex, TGrainStretchLoopModeHelper, TShowMenu.ContextClickOnly);


  //== Synth Osc Controls ==
  fGuiStandard.RegisterControlForAutoUpdate(OscShapeKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(OscPulseWidthKnob, true);


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

  PitchTrackTextBox.Color      := kColor_LcdDark1;
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


  //** Gain, Pan, Tune, Fine **

  MainOutputKnob.Layout.SetSize(32,32);
  MainPanKnob.Layout.SetSize(32,32);
  VoicePitch1Knob.Layout.SetSize(32,32);
  VoicePitch2Knob.Layout.SetSize(32,32);

  MainOutputKnob.Layout.SetPos(0,18);
  MainPanKnob.Layout.SetPos(32,18);
  VoicePitch1Knob.Layout.Anchor(MainPanKnob).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
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

  //================================
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

procedure TVoiceControlFrame.UpdateControlVisibility;
var
  Par : TVstParameter;
  vm : TVoiceMode;
begin
  if not assigned(Plugin) then exit;

  Par := Plugin.Globals.VstParameters.FindParameter('SamplePlaybackType');
  case TSamplePlaybackTypeHelper.ToEnum(Par.ValueVST) of
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




  Par := Plugin.Globals.VstParameters.FindParameter('VoiceMode');
  vm := TVoiceModeHelper.ToEnum(Par.ValueVST);
  case vm of
    TVoiceMode.Poly:
    begin
      GlideKnob.Enabled := false;
      GlideLabel.Visible := false;
    end;

    TVoiceMode.Mono:
    begin
      GlideKnob.Enabled := true;
      GlideLabel.Visible := true;
    end;

    TVoiceMode.Legato:
    begin
      GlideKnob.Enabled := true;
      GlideLabel.Visible := true;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;



end;

procedure TVoiceControlFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
var
  TargetTextBox : TVamTextBox;
begin
  //== General Voice Controls ==
  TargetTextBox := VoiceModeTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TVoiceModeHelper, Plugin.Globals);

  TargetTextBox := PitchTrackTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TPitchTrackingHelper, Plugin.Globals);

  TargetTextBox := SamplePlaybackTypeTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TSamplePlaybackTypeHelper, Plugin.Globals);

  TargetTextBox := ResetTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TClockSourceHelper, Plugin.Globals);

  //== Sample One Shot Controls ==
  TargetTextBox := SamplerLoopModeTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TSamplerLoopModeHelper, Plugin.Globals);

  TargetTextBox := SamplerLoopBoundsTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TSamplerLoopBoundsHelper, Plugin.Globals);

  //== Grain Stretch Osc Controls ==
  TargetTextBox := GrainLoopTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TGrainStretchLoopModeHelper, Plugin.Globals);

end;

procedure TVoiceControlFrame.PlaybackTypeChanged;
var
  SampleOscType : TSamplePlaybackType;
  ParValue : single;
begin
  if not assigned(Plugin) then exit;

  ParValue := Plugin.Globals.VstParameters.FindParameter('SamplePlaybackType').ValueVST;
  SampleOscType := TSamplePlaybackTypeHelper.ToEnum(ParValue);

  OneShotSampleControls.Visible := false;
  GrainStretchControls.Visible  := false;

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
