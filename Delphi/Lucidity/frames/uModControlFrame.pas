unit uModControlFrame;

interface

uses
  uDialogDisplayArea,
  uGuiFeedbackData, uLucidityKeyGroupInterface, VamStatusLed,
  eePlugin, eeGuiStandard, eeGuiStandard_MenuBuilder,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamKnob, VamModularJack, VamDiv, Vcl.Menus,
  VamVectorSequence, VamTextBox, Menu.StepSequenceMenu, VamImage;

type
  TAltFilterText = record
    LastControl : TControl;
    ShowAltText1 : boolean;
    ShowAltText2 : boolean;
    AltText1 : string;
    AltText2 : string;
  end;


  TModControlFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    Row1: TVamDiv;
    Row3: TVamDiv;
    Row2: TVamDiv;
    Filter1Container: TVamDiv;
    VamLabel11: TVamLabel;
    Filter1P3Label: TVamLabel;
    Filter1P2Label: TVamLabel;
    Filter1P1Label: TVamLabel;
    Filter1Par1Knob: TVamKnob;
    Filter1Par2Knob: TVamKnob;
    Filter1Par3Knob: TVamKnob;
    AmpEnvContainer: TVamDiv;
    VamLabel5: TVamLabel;
    AmpEnvReleaseLabel: TVamLabel;
    AmpEnvSustainLabel: TVamLabel;
    AmpEnvDecayLabel: TVamLabel;
    AmpEnvAttackLabel: TVamLabel;
    AmpEnvHoldLabel: TVamLabel;
    AmpEnvAttackKnob: TVamKnob;
    AmpEnvDecayKnob: TVamKnob;
    AmpEnvSustainKnob: TVamKnob;
    AmpEnvReleaseKnob: TVamKnob;
    AmpEnvHoldKnob: TVamKnob;
    FilterTwoContainer: TVamDiv;
    VamLabel16: TVamLabel;
    Filter2P1Label: TVamLabel;
    Filter2P2Label: TVamLabel;
    Filter2P3Label: TVamLabel;
    Filter2Par1Knob: TVamKnob;
    Filter2Par2Knob: TVamKnob;
    Filter2Par3Knob: TVamKnob;
    FilterEnvContainer: TVamDiv;
    VamLabel8: TVamLabel;
    FilterEnvReleaseLabel: TVamLabel;
    FilterEnvSustainLabel: TVamLabel;
    FilterEnvDecayLabel: TVamLabel;
    FilterEnvAttackLabel: TVamLabel;
    FilterEnvHoldLabel: TVamLabel;
    FilterEnvAttackKnob: TVamKnob;
    FilterEnvDecayKnob: TVamKnob;
    FilterEnvSustainKnob: TVamKnob;
    FilterEnvReleaseKnob: TVamKnob;
    FilterEnvHoldKnob: TVamKnob;
    StepSeq1Container: TVamDiv;
    VamLabel34: TVamLabel;
    VamDiv2: TVamDiv;
    StepSeq1: TVamVectorSequence;
    VamDiv3: TVamDiv;
    VamDiv4: TVamDiv;
    VamLabel30: TVamLabel;
    VamLabel31: TVamLabel;
    VamLabel32: TVamLabel;
    StepSeq2Container: TVamDiv;
    VamLabel28: TVamLabel;
    VamDiv6: TVamDiv;
    StepSeq2: TVamVectorSequence;
    VamDiv7: TVamDiv;
    VamLabel33: TVamLabel;
    VamLabel36: TVamLabel;
    VamLabel39: TVamLabel;
    VamDiv8: TVamDiv;
    Filter1TypeTextBox: TVamTextBox;
    Seq1ClockTextBox: TVamTextBox;
    Seq1DirectionTextBox: TVamTextBox;
    Seq1StepsTextBox: TVamTextBox;
    Seq2ClockTextBox: TVamTextBox;
    Seq2DirectionTextBox: TVamTextBox;
    Seq2StepsTextBox: TVamTextBox;
    Filter2TypeTextBox: TVamTextBox;
    LfoAContainer: TVamDiv;
    VamLabel26: TVamLabel;
    LfoShapeTextBox1: TVamTextBox;
    LfoSpeedKnob1: TVamKnob;
    ModEnvAContainer: TVamDiv;
    ModEnvBContainer: TVamDiv;
    VamLabel25: TVamLabel;
    ModEnvAAttackKnob: TVamKnob;
    ModEnvAModeBox: TVamTextBox;
    ModEnvADecayKnob: TVamKnob;
    VamLabel12: TVamLabel;
    ModEnvBAttackKnob: TVamKnob;
    ModEnvBModeBox: TVamTextBox;
    ModEnvBDecayKnob: TVamKnob;
    ModEnvAAttackLabel: TVamLabel;
    ModEnvADecayLabel: TVamLabel;
    ModEnvBAttackLabel: TVamLabel;
    ModEnvBDecayLabel: TVamLabel;
    LfoBContainer: TVamDiv;
    VamLabel13: TVamLabel;
    LfoSpeedKnob2: TVamKnob;
    LfoShapeTextBox2: TVamTextBox;
    LfoDepthKnob1: TVamKnob;
    LfoDepthKnob2: TVamKnob;
    Lfo1DepthLabel: TVamLabel;
    Lfo1RateLabel: TVamLabel;
    Lfo2DepthLabel: TVamLabel;
    Lfo2RateLabel: TVamLabel;
    AmpVelocityButton: TVamTextBox;
    FilterVelocityButton: TVamTextBox;
    Filter1Par4Knob: TVamKnob;
    Filter1P4Label: TVamLabel;
    Filter2Par4Knob: TVamKnob;
    Filter2P4Label: TVamLabel;
    procedure StepSeq1Changed(Sender: TObject);
    procedure FilterKnobMouseEnter(Sender: TObject);
    procedure FilterKnobMouseLeave(Sender: TObject);
    procedure StepSeqShowContextMenu(Sender: TObject; X, Y: Integer);
  private
    fPlugin: TeePlugin;
    fGuiStandard: TGuiStandard;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);
    procedure UpdateControlVisibility;
  protected
    AltFilterText : TAltFilterText;
    FilterParameterInfo : TFilterParameterInfo;

    ParIndexFilter1Type : integer;
    ParIndexFilter2Type : integer;

    StepSequenceMenu : TStepSequenceMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;

    procedure FilterChanged;
  end;

implementation

uses
  VamLayoutWizard, eeVstParameter,
  RedFoxColor, uLucidityEnums,
  uConstants, uGuiUtils,
  uLucidityKeyGroup;

{$R *.dfm}

{ TModControlFrame }

constructor TModControlFrame.Create(AOwner: TComponent);
begin
  inherited;

  MsgHandle := AllocateHWND(MessageHandler);

  AltFilterText.ShowAltText1 := false;
  AltFilterText.ShowAltText2 := false;

  StepSequenceMenu := TStepSequenceMenu.Create;
end;

destructor TModControlFrame.Destroy;
begin
  if (MsgHandle <> 0) and (assigned(Plugin)) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(MsgHandle);
  end;
  DeallocateHWnd(MsgHandle);

  StepSequenceMenu.Free;
  inherited;
end;

procedure TModControlFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
var
  ParIndex : integer;
  FilterKnobWidth : integer;
  kw, kh : integer;
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  if MsgHandle <> 0 then Plugin.Globals.AddWindowsMessageListener(MsgHandle);
  UpdateControlVisibility;

  StepSequenceMenu.Initialize(aPlugin, aDialogDisplayArea);

  AmpEnvAttackKnob.Tag  := fPlugin.Globals.VstParameters.FindParameterIndexByName('AmpAttack');
  AmpEnvHoldKnob.Tag    := fPlugin.Globals.VstParameters.FindParameterIndexByName('AmpHold');
  AmpEnvDecayKnob.Tag   := fPlugin.Globals.VstParameters.FindParameterIndexByName('AmpDecay');
  AmpEnvSustainKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('AmpSustain');
  AmpEnvReleaseKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('AmpRelease');

  FilterEnvAttackKnob.Tag  := fPlugin.Globals.VstParameters.FindParameterIndexByName('FilterAttack');
  FilterEnvHoldKnob.Tag    := fPlugin.Globals.VstParameters.FindParameterIndexByName('FilterHold');
  FilterEnvDecayKnob.Tag   := fPlugin.Globals.VstParameters.FindParameterIndexByName('FilterDecay');
  FilterEnvSustainKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('FilterSustain');
  FilterEnvReleaseKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('FilterRelease');

  Filter1Par1Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter1Par1');
  Filter1Par2Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter1Par2');
  Filter1Par3Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter1Par3');
  Filter1Par4Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter1Par4');

  Filter2Par1Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter2Par1');
  Filter2Par2Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter2Par2');
  Filter2Par3Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter2Par3');
  Filter2Par4Knob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter2Par4');

  ModEnvAAttackKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('ModEnvAAttack');
  ModEnvADecayKnob.Tag  := fPlugin.Globals.VstParameters.FindParameterIndexByName('ModEnvADecay');

  ModEnvBAttackKnob.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('ModEnvBAttack');
  ModEnvBDecayKnob.Tag  := fPlugin.Globals.VstParameters.FindParameterIndexByName('ModEnvBDecay');

  ModEnvAModeBox.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('ModEnvAMode');
  ModEnvBModeBox.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('ModEnvBMode');

  ParIndexFilter1Type := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter1Type');
  ParIndexFilter2Type := fPlugin.Globals.VstParameters.FindParameterIndexByName('Filter2Type');

  LfoShapeTextBox1.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('LfoShape1');
  LfoShapeTextBox2.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('LfoShape2');

  LfoSpeedKnob1.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('LfoRate1');
  LfoSpeedKnob2.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('LfoRate2');

  LfoDepthKnob1.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('LfoAPar2');
  LfoDepthKnob2.Tag := fPlugin.Globals.VstParameters.FindParameterIndexByName('LfoBPar2');



  fGuiStandard.RegisterControlForAutoUpdate(AmpEnvAttackKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(AmpEnvHoldKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(AmpEnvDecayKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(AmpEnvSustainKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(AmpEnvReleaseKnob, true);

  fGuiStandard.RegisterControlForAutoUpdate(FilterEnvAttackKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(FilterEnvHoldKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(FilterEnvDecayKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(FilterEnvSustainKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(FilterEnvReleaseKnob, true);

  fGuiStandard.RegisterControlForAutoUpdate(Filter1Par1Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(Filter1Par2Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(Filter1Par3Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(Filter1Par4Knob, true);

  fGuiStandard.RegisterControlForAutoUpdate(Filter2Par1Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(Filter2Par2Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(Filter2Par3Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(Filter2Par4Knob, true);

  fGuiStandard.RegisterControlForAutoUpdate(LfoSpeedKnob1, true);
  fGuiStandard.RegisterControlForAutoUpdate(LfoSpeedKnob2, true);

  fGuiStandard.RegisterControlForAutoUpdate(LfoDepthKnob1, true);
  fGuiStandard.RegisterControlForAutoUpdate(LfoDepthKnob2, true);

  fGuiStandard.RegisterControlForAutoUpdate(ModEnvAAttackKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(ModEnvADecayKnob, true);

  fGuiStandard.RegisterControlForAutoUpdate(ModEnvBAttackKnob, true);
  fGuiStandard.RegisterControlForAutoUpdate(ModEnvBDecayKnob, true);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('AmpVelocity');
  fGuiStandard.RegisterControlAsMenuControl(AmpVelocityButton, ParIndex, TEnvVelocityDepthHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('FilterVelocity');
  fGuiStandard.RegisterControlAsMenuControl(FilterVelocityButton, ParIndex, TEnvVelocityDepthHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('ModEnvAMode');
  fGuiStandard.RegisterControlAsMenuControl(ModEnvAModeBox, ParIndex, TModEnvModeHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('ModEnvBMode');
  fGuiStandard.RegisterControlAsMenuControl(ModEnvBModeBox, ParIndex, TModEnvModeHelper, TShowMenu.ContextClickOnly);

  //=== LFO Shape Menu controls ==
  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('LfoShape1');
  fGuiStandard.RegisterControlAsMenuControl(LfoShapeTextBox1, ParIndex, TLfoShapeHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('LfoShape2');
  fGuiStandard.RegisterControlAsMenuControl(LfoShapeTextBox2, ParIndex, TLfoShapeHelper, TShowMenu.ContextClickOnly);


  //=== Filter type Menu controls ==
  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('Filter1Type');
  fGuiStandard.RegisterControlAsMenuControl(Filter1TypeTextBox, ParIndex, TFilterTypeHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('Filter2Type');
  fGuiStandard.RegisterControlAsMenuControl(Filter2TypeTextBox, ParIndex, TFilterTypeHelper, TShowMenu.ContextClickOnly);

  //=== Step seq one ====
  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('Seq1Clock');
  fGuiStandard.RegisterControlAsMenuControl(Seq1ClockTextBox, ParIndex, TSequencerClockHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('Seq1Direction');
  fGuiStandard.RegisterControlAsMenuControl(Seq1DirectionTextBox, ParIndex, TStepSequencerDirectionHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('StepSeq1Length');
  fGuiStandard.RegisterControlAsMenuControl(Seq1StepsTextBox, ParIndex, TStepSequencerLengthHelper, TShowMenu.ContextClickOnly);

  //=== Step seq two ====
  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('Seq2Clock');
  fGuiStandard.RegisterControlAsMenuControl(Seq2ClockTextBox, ParIndex, TSequencerClockHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('Seq2Direction');
  fGuiStandard.RegisterControlAsMenuControl(Seq2DirectionTextBox, ParIndex, TStepSequencerDirectionHelper, TShowMenu.ContextClickOnly);

  ParIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('StepSeq2Length');
  fGuiStandard.RegisterControlAsMenuControl(Seq2StepsTextBox, ParIndex, TStepSequencerLengthHelper, TShowMenu.ContextClickOnly);








  //============================================================================
  //                         GUI Stylings
  //============================================================================

  FilterKnobWidth := 40;


  Row1.Height := 82;
  Row1.Align := alTop;
  Row1.Margins.Bottom := 16;

    Filter1Container.Align := alLeft;
    Filter1Container.Width := (4 * FilterKnobWidth) - 8;
    Filter1Container.Margins.Right := 42;

      Filter1TypeTextBox.Layout.SetPos(0,64).AdjustBounds(-4,0,-4,0);

    AmpEnvContainer.Align := alLeft;
    AmpEnvContainer.Width := 5 * 32;

    LfoAContainer.Width := 2 * 40;
    LfoAContainer.AlignWithMargins := true;
    LfoAContainer.Margins.SetBounds(0,0,16,0);

      LfoSpeedKnob1.Layout.SetPos(0,18);
      LfoDepthKnob1.Layout.SetPos(40,18);

      Lfo1RateLabel.Layout.Anchor(LfoSpeedKnob1).SnapToEdge(TControlFeature.BottomEdge);
      Lfo1DepthLabel.Layout.Anchor(LfoDepthKnob1).SnapToEdge(TControlFeature.BottomEdge);

      LfoShapeTextBox1.Layout.Anchor(Lfo1RateLabel).SnapToEdge(TControlFeature.BottomEdge).Move(5,0);
      LfoShapeTextBox1.Width := 70;

    LfoBContainer.Width := 2 * 40;
    LfoBContainer.AlignWithMargins := true;
    LfoBContainer.Margins.SetBounds(0,0,0,0);

      LfoSpeedKnob2.Layout.SetPos(0,18);
      LfoDepthKnob2.Layout.SetPos(40,18);

      Lfo2RateLabel.Layout.Anchor(LfoSpeedKnob2).SnapToEdge(TControlFeature.BottomEdge);
      Lfo2DepthLabel.Layout.Anchor(LfoDepthKnob2).SnapToEdge(TControlFeature.BottomEdge);

      LfoShapeTextBox2.Layout.Anchor(Lfo2RateLabel).SnapToEdge(TControlFeature.BottomEdge).Move(5,0);
      LfoShapeTextBox2.Width := 70;

    LfoBContainer.Align := alRight;
    LfoAContainer.Align := alRight;




  Row2.Height := 82;
  Row2.Align := alTop;
  Row2.Margins.Bottom := 16;

    FilterTwoContainer.Align := alLeft;
    FilterTwoContainer.Width :=  (4 * FilterKnobWidth) - 8;
    FilterTwoContainer.Margins.Right := 42;

      Filter2TypeTextBox.Layout.SetPos(0,64).AdjustBounds(-4,0,-4,0);

    FilterEnvContainer.Align := alLeft;
    FilterEnvContainer.Width := 5 * 32;

    ModEnvAContainer.Width := 2 * 40;
    ModEnvAContainer.AlignWithMargins := true;
    ModEnvAContainer.Margins.SetBounds(0,0,16,0);

      ModEnvAAttackKnob.Layout.SetPos(0,18);
      ModEnvADecayKnob.Layout.SetPos(40,18);

      ModEnvAAttackLabel.Layout.Anchor(ModEnvAAttackKnob).SnapToEdge(TControlFeature.BottomEdge).Move(0,-2);
      ModEnvADecayLabel.Layout.Anchor(ModEnvADecayKnob).SnapToEdge(TControlFeature.BottomEdge).Move(0,-2);

      ModEnvAModeBox.Layout.Anchor(ModEnvAAttackLabel).SnapToEdge(TControlFeature.BottomEdge).Move(5,0);
      ModEnvAModeBox.Width := 70;



    ModEnvBContainer.Width := 2 * 40;
    ModEnvBContainer.AlignWithMargins := true;
    ModEnvBContainer.Margins.SetBounds(0,0,0,0);


    ModEnvBContainer.Align := alRight;
    ModEnvAContainer.Align := alRight;

      ModEnvBAttackKnob.Layout.SetPos(0,18);
      ModEnvBDecayKnob.Layout.SetPos(40,18);

      ModEnvBAttackLabel.Layout.Anchor(ModEnvBAttackKnob).SnapToEdge(TControlFeature.BottomEdge).Move(0,-2);
      ModEnvBDecayLabel.Layout.Anchor(ModEnvBDecayKnob).SnapToEdge(TControlFeature.BottomEdge).Move(0,-2);

      ModEnvBModeBox.Layout.Anchor(ModEnvBAttackLabel).SnapToEdge(TControlFeature.BottomEdge).Move(5,0);
      ModEnvBModeBox.Width := 70;



  Row3.Height := 78;
  Row3.Align := alTop;
  Row3.Margins.Bottom := 16;

    StepSeq1Container.Align := alLeft;
    StepSeq1Container.Margins.Right := 16;

    StepSeq2Container.Align := alLeft;
    StepSeq2Container.Margins.Right := 16;




  //==================================================

  //==== Filter 1 controls ====
  Filter1Par1Knob.Layout.SetSize(FilterKnobWidth,32).SetPos(-4, 16);
  Filter1Par2Knob.Layout.SetSize(FilterKnobWidth,32).Anchor(Filter1Par1Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter1Par3Knob.Layout.SetSize(FilterKnobWidth,32).Anchor(Filter1Par2Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter1Par4Knob.Layout.SetSize(FilterKnobWidth,32).Anchor(Filter1Par3Knob).SnapToEdge(TControlFeature.RightEdge);

  Filter1P1Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter1Par1Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter1P2Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter1Par2Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter1P3Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter1Par3Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter1P4Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter1Par4Knob).SnapToEdge(TControlFeature.BottomEdge);

  //==== Filter 2 controls ====
  Filter2Par1Knob.Layout.SetSize(FilterKnobWidth,32).SetPos(-4, 16);
  Filter2Par2Knob.Layout.SetSize(FilterKnobWidth,32).Anchor(Filter2Par1Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter2Par3Knob.Layout.SetSize(FilterKnobWidth,32).Anchor(Filter2Par2Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter2Par4Knob.Layout.SetSize(FilterKnobWidth,32).Anchor(Filter2Par3Knob).SnapToEdge(TControlFeature.RightEdge);

  Filter2P1Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter2Par1Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter2P2Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter2Par2Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter2P3Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter2Par3Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter2P4Label.Layout.SetSize(FilterKnobWidth,16).Anchor(Filter2Par4Knob).SnapToEdge(TControlFeature.BottomEdge);



  //=== ADSR 1 ====
  kw := 32;
  kh := 32;
  AmpEnvAttackKnob.Layout.SetSize(kw, kh).SetPos(0,18);
  AmpEnvHoldKnob.Layout.SetSize(kw, kh).SetPos(1 * kw, 18);
  AmpEnvDecayKnob.Layout.SetSize(kw, kh).SetPos(2 * kw, 18);
  AmpEnvSustainKnob.Layout.SetSize(kw, kh).SetPos(3 * kw, 18);
  AmpEnvReleaseKnob.Layout.SetSize(kw, kh).SetPos(4 * kw, 18);

  AmpEnvAttackLabel.Layout.SetSize(kw, 16).Anchor(AmpEnvAttackKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvHoldLabel.Layout.SetSize(kw, 16).Anchor(AmpEnvHoldKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvDecayLabel.Layout.SetSize(kw, 16).Anchor(AmpEnvDecayKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvSustainLabel.Layout.SetSize(kw, 16).Anchor(AmpEnvSustainKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvReleaseLabel.Layout.SetSize(kw, 16).Anchor(AmpEnvReleaseKnob).SnapToEdge(TControlFeature.BottomEdge);

  AmpVelocityButton.Layout.Anchor(AmpEnvSustainLabel).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  AmpVelocityButton.Layout.SetSize(60, 16);
  //==================================================


  //=== ADSR 2 ====
  kw := 32;
  kh := 32;
  FilterEnvAttackKnob.Layout.SetSize(kw, kh).SetPos(0,18);
  FilterEnvHoldKnob.Layout.SetSize(kw, kh).SetPos(1 * kw, 18);
  FilterEnvDecayKnob.Layout.SetSize(kw, kh).SetPos(2 * kw, 18);
  FilterEnvSustainKnob.Layout.SetSize(kw, kh).SetPos(3 * kw, 18);
  FilterEnvReleaseKnob.Layout.SetSize(kw, kh).SetPos(4 * kw, 18);

  FilterEnvAttackLabel.Layout.SetSize(kw, 16).Anchor(FilterEnvAttackKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvHoldLabel.Layout.SetSize(kw, 16).Anchor(FilterEnvHoldKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvDecayLabel.Layout.SetSize(kw, 16).Anchor(FilterEnvDecayKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvSustainLabel.Layout.SetSize(kw, 16).Anchor(FilterEnvSustainKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvReleaseLabel.Layout.SetSize(kw, 16).Anchor(FilterEnvReleaseKnob).SnapToEdge(TControlFeature.BottomEdge);

  FilterVelocityButton.Layout.Anchor(FilterEnvSustainLabel).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  FilterVelocityButton.Layout.SetSize(60, 16);
  //==================================================



  //=== colors ===

  StepSeq1.Color_Background := kColor_LcdDark1;
  StepSeq1.Color_Border     := kColor_LcdDark1;
  StepSeq1.Color_Step       := kColor_LcdDark4;
  StepSeq1.Color_StepActive := kColor_LcdDark5;

  StepSeq2.Color_Background := kColor_LcdDark1;
  StepSeq2.Color_Border     := kColor_LcdDark1;
  StepSeq2.Color_Step       := kColor_LcdDark4;
  StepSeq2.Color_StepActive := kColor_LcdDark5;


  AmpVelocityButton.Font.Color    := GetRedFoxColor(kColor_LcdDark5);
  FilterVelocityButton.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  Filter1TypeTextBox.Font.Color   := GetRedFoxColor(kColor_LcdDark5);
  Filter2TypeTextBox.Font.Color   := GetRedFoxColor(kColor_LcdDark5);
  LfoShapeTextBox1.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  LfoShapeTextBox2.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq1ClockTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq1DirectionTextBox.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  Seq1StepsTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq2ClockTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq2DirectionTextBox.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  Seq2StepsTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  ModEnvAModeBox.Font.Color       := GetRedFoxColor(kColor_LcdDark5);
  ModEnvBModeBox.Font.Color       := GetRedFoxColor(kColor_LcdDark5);


  AmpVelocityButton.Color    := kColor_LcdDark1;
  FilterVelocityButton.Color := kColor_LcdDark1;
  Filter1TypeTextBox.Color   := kColor_LcdDark1;
  Filter2TypeTextBox.Color   := kColor_LcdDark1;
  LfoShapeTextBox1.Color     := kColor_LcdDark1;
  LfoShapeTextBox2.Color     := kColor_LcdDark1;
  Seq1ClockTextBox.Color     := kColor_LcdDark1;
  Seq1DirectionTextBox.Color := kColor_LcdDark1;
  Seq1StepsTextBox.Color     := kColor_LcdDark1;
  Seq2ClockTextBox.Color     := kColor_LcdDark1;
  Seq2DirectionTextBox.Color := kColor_LcdDark1;
  Seq2StepsTextBox.Color     := kColor_LcdDark1;
  ModEnvAModeBox.Color       := kColor_LcdDark1;
  ModEnvBModeBox.Color       := kColor_LcdDark1;

  AmpVelocityButton.ColorMouseOver    := kColor_ButtonMouseOver;
  FilterVelocityButton.ColorMouseOver := kColor_ButtonMouseOver;
  Filter1TypeTextBox.ColorMouseOver   := kColor_ButtonMouseOver;
  Filter2TypeTextBox.ColorMouseOver   := kColor_ButtonMouseOver;
  LfoShapeTextBox1.ColorMouseOver     := kColor_ButtonMouseOver;
  LfoShapeTextBox2.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq1ClockTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq1DirectionTextBox.ColorMouseOver := kColor_ButtonMouseOver;
  Seq1StepsTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq2ClockTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq2DirectionTextBox.ColorMouseOver := kColor_ButtonMouseOver;
  Seq2StepsTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  ModEnvAModeBox.ColorMouseOver       := kColor_ButtonMouseOver;
  ModEnvBModeBox.ColorMouseOver       := kColor_ButtonMouseOver;
end;

procedure TModControlFrame.MessageHandler(var Message: TMessage);
begin
  if Message.Msg = UM_Update_Control_Visibility then UpdateControlVisibility;
  if Message.Msg = UM_FILTER_CHANGED then UpdateControlVisibility;
end;




procedure TModControlFrame.UpdateControlVisibility;
var
  Par : TVstParameter;
  FT  : TFilterType;
  LfoShape : TLfoShape;

  Knobs : array[0..3] of TControl;
  Labels : array[0..3] of TControl;
begin
  Par := Plugin.Globals.VstParameters.Par('Filter1Type');
  FT := TFilterTypeHelper.ToEnum(Par.ValueVST);

  Knobs[0] := Filter1Par1Knob;
  Knobs[1] := Filter1Par2Knob;
  Knobs[2] := Filter1Par3Knob;
  Knobs[3] := Filter1Par4Knob;

  Labels[0] := Filter1P1Label;
  Labels[1] := Filter1P2Label;
  Labels[2] := Filter1P3Label;
  Labels[3] := Filter1P4Label;

  UpdateFilterControls(Knobs, Labels, FT);



  Par := Plugin.Globals.VstParameters.Par('Filter2Type');
  FT := TFilterTypeHelper.ToEnum(Par.ValueVST);

  Knobs[0] := Filter2Par1Knob;
  Knobs[1] := Filter2Par2Knob;
  Knobs[2] := Filter2Par3Knob;
  Knobs[3] := Filter2Par4Knob;

  Labels[0] := Filter2P1Label;
  Labels[1] := Filter2P2Label;
  Labels[2] := Filter2P3Label;
  Labels[3] := Filter2P4Label;

  UpdateFilterControls(Knobs, Labels, FT);




  Par := Plugin.Globals.VstParameters.Par('LfoShape1');
  LfoShape := TLfoShapeHelper.ToEnum(Par.ValueVST);
  case LfoShape of
    TLfoShape.SawUp,
    TLfoShape.SawDown,
    TLfoShape.Square,
    TLfoShape.Triangle,
    TLfoShape.Sine:
    begin
      Lfo1DepthLabel.Text    := 'PHASE';
      LfoDepthKnob1.Enabled := true;
      Lfo1DepthLabel.Visible := true;
    end;

    TLfoShape.Random:
    begin
      Lfo1DepthLabel.Text    := 'MOD';
      LfoDepthKnob1.Enabled  := true;
      Lfo1DepthLabel.Visible := true;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;



  Par := Plugin.Globals.VstParameters.Par('LfoShape2');
  LfoShape := TLfoShapeHelper.ToEnum(Par.ValueVST);
    case LfoShape of
    TLfoShape.SawUp,
    TLfoShape.SawDown,
    TLfoShape.Square,
    TLfoShape.Triangle,
    TLfoShape.Sine:
    begin
      Lfo2DepthLabel.Text    := 'PHASE';
      LfoDepthKnob2.Enabled := true;
      Lfo2DepthLabel.Visible := true;
    end;

    TLfoShape.Random:
    begin
      Lfo2DepthLabel.Text    := 'MOD';
      LfoDepthKnob2.Enabled  := true;
      Lfo2DepthLabel.Visible := true;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;



end;

procedure TModControlFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
var
  c1 : integer;
  x1 : single;
  a1 : integer;
  TargetTextBox : TVamTextBox;
  SG : IKeyGroup;
  CurEngine : TKeyGroup;
  SeqLength : TStepSequencerLength;
begin
  // NOTE: Don't enable the Update GUI time until the plugin variable as been assigned.
  assert(assigned(Plugin));


  //TODO: Rather then pulling the focused engine from the plugin, perhaps it
  // would be better to at the Focused engine check as part of the GUI FeedBack data.
  SG := Plugin.FocusedKeyGroup;

  //== Filter 1 Type ==
  if AltFilterText.ShowAltText1 then
  begin
    if Filter1TypeTextBox.Text <> AltFilterText.AltText1 then Filter1TypeTextBox.Text := AltFilterText.AltText1;
  end else
  begin
    TargetTextBox := Filter1TypeTextBox;
    UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TFilterTypeHelper, Plugin.Globals);
  end;

  //== Filter 2 Type ==
  if AltFilterText.ShowAltText2 then
  begin
    if Filter2TypeTextBox.Text <> AltFilterText.AltText2 then Filter2TypeTextBox.Text := AltFilterText.AltText2;
  end else
  begin
    TargetTextBox := Filter2TypeTextBox;
    UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TFilterTypeHelper, Plugin.Globals);
  end;


  //==== LFO shape parameters ===
  TargetTextBox := LfoShapeTextBox1;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TLfoShapeHelper, Plugin.Globals);

  TargetTextBox := LfoShapeTextBox2;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TLfoShapeHelper, Plugin.Globals);


  //==== Env Velocity Depth ===
  TargetTextBox := AmpVelocityButton;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TEnvVelocityDepthHelper, Plugin.Globals);

  TargetTextBox := FilterVelocityButton;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TEnvVelocityDepthHelper, Plugin.Globals);

  //==== Mod Env parameters ===
  TargetTextBox := ModEnvAModeBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TModEnvModeHelper, Plugin.Globals);

  TargetTextBox := ModEnvBModeBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TModEnvModeHelper, Plugin.Globals);


  //== Step Seq 1 ==
  if assigned(SG) then
  begin
    CurEngine := SG.GetObject as TKeyGroup;
    if StepSeq1.IsControlGrabbed = false then
    begin
      for c1 := 0 to kMaxStepSequencerLength-1 do
      begin
        x1 := CurEngine.VoiceParameters.Seq1StepValue[c1] * 2 - 1;
        if StepSeq1.SequenceValue[c1] <> x1 then StepSeq1.SequenceValue[c1] := x1;
      end;
    end;
  end;

  x1 := Plugin.Globals.VstParameters.Par('StepSeq1Length').ValueVST;
  SeqLength := TStepSequencerLengthHelper.ToEnum(x1);
  case SeqLength of
    TStepSequencerLength.Two:     a1 := 2;
    TStepSequencerLength.Three:   a1 := 3;
    TStepSequencerLength.Four:    a1 := 4;
    TStepSequencerLength.Five:    a1 := 5;
    TStepSequencerLength.Six:     a1 := 6;
    TStepSequencerLength.Seven:   a1 := 7;
    TStepSequencerLength.Eight:   a1 := 8;
    TStepSequencerLength.Twelve:  a1 := 12;
    TStepSequencerLength.Sixteen: a1 := 16;
  else
    raise Exception.Create('unexpected step count value.');
  end;
  if StepSeq1.SequenceLength <> a1 then StepSeq1.SequenceLength := a1;

  TargetTextBox := Seq1ClockTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TSequencerClockHelper, Plugin.Globals);

  TargetTextBox := Seq1DirectionTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TStepSequencerDirectionHelper, Plugin.Globals);

  TargetTextBox := Seq1StepsTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TStepSequencerLengthHelper, Plugin.Globals);



  //== Step Seq 2 ==
  if assigned(SG) then
  begin
    CurEngine := SG.GetObject as TKeyGroup;
    if StepSeq2.IsControlGrabbed = false then
    begin
      for c1 := 0 to kMaxStepSequencerLength-1 do
      begin
        x1 := CurEngine.VoiceParameters.Seq2StepValue[c1] * 2 - 1;
        if StepSeq2.SequenceValue[c1] <> x1 then StepSeq2.SequenceValue[c1] := x1;
      end;
    end;
  end;

  x1 := Plugin.Globals.VstParameters.Par('StepSeq2Length').ValueVST;
  SeqLength := TStepSequencerLengthHelper.ToEnum(x1);
  case SeqLength of
    TStepSequencerLength.Two:     a1 := 2;
    TStepSequencerLength.Three:   a1 := 3;
    TStepSequencerLength.Four:    a1 := 4;
    TStepSequencerLength.Five:    a1 := 5;
    TStepSequencerLength.Six:     a1 := 6;
    TStepSequencerLength.Seven:   a1 := 7;
    TStepSequencerLength.Eight:   a1 := 8;
    TStepSequencerLength.Twelve:  a1 := 12;
    TStepSequencerLength.Sixteen: a1 := 16;
  else
    raise Exception.Create('unexpected step count value.');
  end;
  if StepSeq2.SequenceLength <> a1 then StepSeq2.SequenceLength := a1;

  TargetTextBox := Seq2ClockTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TSequencerClockHelper, Plugin.Globals);

  TargetTextBox := Seq2DirectionTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TStepSequencerDirectionHelper, Plugin.Globals);

  TargetTextBox := Seq2StepsTextBox;
  UpdateTextBoxWithParValue(TargetTextBox, TargetTextBox.Tag, TStepSequencerLengthHelper, Plugin.Globals);



  if FeedBack^.IsVoiceActive then
  begin
    if StepSeq1.CurrentStep <> FeedBack^.StepSeq1CurStep then StepSeq1.CurrentStep := FeedBack^.StepSeq1CurStep;
    if StepSeq2.CurrentStep <> FeedBack^.StepSeq2CurStep then StepSeq2.CurrentStep := FeedBack^.StepSeq2CurStep;
  end else
  begin
    if StepSeq1.CurrentStep <> -1 then StepSeq1.CurrentStep := -1;
    if StepSeq2.CurrentStep <> -1 then StepSeq2.CurrentStep := -1;
  end;



end;

procedure TModControlFrame.StepSeq1Changed(Sender: TObject);
var
  Tag : integer;
  c1: integer;
  x1 : single;
  SG : IKeyGroup;
  CurEngine : TKeyGroup;
begin
  if not assigned(Plugin) then exit;

  SG := Plugin.FocusedKeyGroup;

  if not assigned(sg) then exit;

  CurEngine := SG.GetObject as TKeyGroup;

  Tag := (Sender as TVamVectorSequence).Tag;

  //== Step Seq 1 ==
  if Tag = 1 then
  begin
    for c1 := 0 to kMaxStepSequencerLength-1 do
    begin
      x1 := StepSeq1.SequenceValue[c1] * 0.5 + 0.5;
      if CurEngine.VoiceParameters.Seq1StepValue[c1] <> x1 then CurEngine.VoiceParameters.Seq1StepValue[c1] := x1;
    end;
  end;


  //== Step Seq 2 ==
  if Tag = 2 then
  begin
    for c1 := 0 to kMaxStepSequencerLength-1 do
    begin
      x1 := StepSeq2.SequenceValue[c1] * 0.5 + 0.5;
      if CurEngine.VoiceParameters.Seq2StepValue[c1] <> x1 then CurEngine.VoiceParameters.Seq2StepValue[c1] := x1;
    end;
  end;
end;


procedure TModControlFrame.FilterKnobMouseEnter(Sender: TObject);
begin
  AltFilterText.LastControl := Sender as TControl;

  AltFilterText.ShowAltText1 := false;
  AltFilterText.ShowAltText2 := false;

  if (Sender = Filter1Par1Knob) then
  begin
    AltFilterText.ShowAltText1 := true;
    AltFilterText.AltText1 := FilterParameterInfo.Filter1Par1FullName;
  end;

  if (Sender = Filter1Par2Knob) then
  begin
    AltFilterText.ShowAltText1 := true;
    AltFilterText.AltText1 := FilterParameterInfo.Filter1Par2FullName;
  end;

  if (Sender = Filter1Par3Knob) then
  begin
    AltFilterText.ShowAltText1 := true;
    AltFilterText.AltText1 := FilterParameterInfo.Filter1Par3FullName;
  end;

  if (Sender = Filter2Par1Knob) then
  begin
    AltFilterText.ShowAltText2 := true;
    AltFilterText.AltText2 := FilterParameterInfo.Filter2Par1FullName;
  end;

  if (Sender = Filter2Par2Knob) then
  begin
    AltFilterText.ShowAltText2 := true;
    AltFilterText.AltText2 := FilterParameterInfo.Filter2Par2FullName;
  end;

  if (Sender = Filter2Par3Knob) then
  begin
    AltFilterText.ShowAltText2 := true;
    AltFilterText.AltText2 := FilterParameterInfo.Filter2Par3FullName;
  end;

end;


procedure TModControlFrame.FilterKnobMouseLeave(Sender: TObject);
begin
  if (Sender as TControl) = AltFilterText.LastControl then
  begin
    AltFilterText.LastControl := nil;
    AltFilterText.ShowAltText1 := false;
    AltFilterText.ShowAltText2 := false;
  end;
end;

procedure TModControlFrame.FilterChanged;
begin
  //TODO:
  assert(assigned(Plugin));
  Plugin.GetFilterInfo(@FilterParameterInfo);

  {
  Filter1P1Label.Text := FilterParameterInfo.Filter1Par1ShortName;
  Filter1P2Label.Text := FilterParameterInfo.Filter1Par2ShortName;
  Filter1P3Label.Text := FilterParameterInfo.Filter1Par3ShortName;

  Filter2P1Label.Text := FilterParameterInfo.Filter2Par1ShortName;
  Filter2P2Label.Text := FilterParameterInfo.Filter2Par2ShortName;
  Filter2P3Label.Text := FilterParameterInfo.Filter2Par3ShortName;
  }
end;

procedure TModControlFrame.StepSeqShowContextMenu(Sender: TObject; X, Y: Integer);
var
  StepSeqIndex : integer;
begin
  StepSeqIndex := (Sender as TVamVectorSequence).Tag-1;
  StepSequenceMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, StepSeqIndex);
end;





end.
