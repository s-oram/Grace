unit uModControlFrame;

interface

{$INCLUDE Defines.inc}

uses
  Lucidity.PluginParameters,
  eeGuiStandardv2,
  VamLib.ZeroObject,
  VamLib.Collections.Lists,
  uDialogDisplayArea,
  uGuiFeedbackData, Lucidity.Interfaces, VamStatusLed,
  eePlugin,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamKnob, VamModularJack, VamDiv, Vcl.Menus,
  VamTextBox, Menu.StepSequenceMenu, VamImage, VamButton,
  LucidityGui.VectorSequence,
  LucidityGui.Scope, Vcl.ExtCtrls, Contnrs, VamSliderSwitch,
  VamCompoundNumericKnob;

type
  TAltFilterText = record
    LastControl : TControl;
    ShowAltText1 : boolean;
    ShowAltText2 : boolean;
    AltText1 : string;
    AltText2 : string;
  end;


  TModControlFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    StepSeq1Container: TVamDiv;
    VamLabel34: TVamLabel;
    VamDiv2: TVamDiv;
    VamDiv3: TVamDiv;
    Seq1ClockTextBox: TVamTextBox;
    Seq1DirectionTextBox: TVamTextBox;
    Seq1StepsTextBox: TVamTextBox;
    VamDiv4: TVamDiv;
    VamLabel30: TVamLabel;
    VamLabel31: TVamLabel;
    VamLabel32: TVamLabel;
    StepSeq2Container: TVamDiv;
    VamLabel28: TVamLabel;
    VamDiv6: TVamDiv;
    VamDiv7: TVamDiv;
    VamLabel33: TVamLabel;
    VamLabel36: TVamLabel;
    VamLabel39: TVamLabel;
    VamDiv8: TVamDiv;
    Seq2ClockTextBox: TVamTextBox;
    Seq2DirectionTextBox: TVamTextBox;
    Seq2StepsTextBox: TVamTextBox;
    FilterTwoContainer: TVamDiv;
    FilterTwoContainerLabel: TVamLabel;
    Filter2P1Label: TVamLabel;
    Filter2P2Label: TVamLabel;
    Filter2P3Label: TVamLabel;
    Filter2TypeTextBox: TVamTextBox;
    Filter2Par1Knob: TVamKnob;
    Filter2Par2Knob: TVamKnob;
    Filter2Par3Knob: TVamKnob;
    Filter2Par4Knob: TVamKnob;
    Filter2P4Label: TVamLabel;
    ModEnvContainer: TVamDiv;
    ModEnvContainerLabel: TVamLabel;
    ModEnvReleaseLabel: TVamLabel;
    ModEnvSustainLabel: TVamLabel;
    ModEnvDecayLabel: TVamLabel;
    ModEnvAttackLabel: TVamLabel;
    ModEnvHoldLabel: TVamLabel;
    ModEnvAttackKnob: TVamKnob;
    ModEnvDecayKnob: TVamKnob;
    ModEnvSustainKnob: TVamKnob;
    ModEnvReleaseKnob: TVamKnob;
    ModEnvHoldKnob: TVamKnob;
    ModEnvVelocityButton: TVamTextBox;
    LfoAContainer: TVamDiv;
    LfoAContainerLabel: TVamLabel;
    LfoALabel2: TVamLabel;
    LfoALabel1: TVamLabel;
    LfoAShapeSelector: TVamTextBox;
    LfoAKnob1: TVamKnob;
    LfoAKnob2: TVamKnob;
    AmpEnvContainer: TVamDiv;
    AmpEnvContainerLabel: TVamLabel;
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
    AmpVelocityButton: TVamTextBox;
    Filter1Container: TVamDiv;
    FilterOneContainerLabel: TVamLabel;
    Filter1P3Label: TVamLabel;
    Filter1P2Label: TVamLabel;
    Filter1P1Label: TVamLabel;
    Filter1TypeTextBox: TVamTextBox;
    Filter1Par1Knob: TVamKnob;
    Filter1Par2Knob: TVamKnob;
    Filter1Par3Knob: TVamKnob;
    Filter1Par4Knob: TVamKnob;
    Filter1P4Label: TVamLabel;
    FilterBlendContainer: TVamDiv;
    FilterBlendContainerLabel: TVamLabel;
    FilterBlendLabel: TVamLabel;
    FilterBlendKnob: TVamKnob;
    LfoAKnob3: TVamKnob;
    LfoALabel3: TVamLabel;
    Timer1: TTimer;
    Filter1KeyTrackKnob: TVamCompoundNumericKnob;
    Filter2KeyTrackKnob: TVamCompoundNumericKnob;
    LfoAFreqModeSelector: TVamTextBox;
    FilterRoutingButton: TVamTextBox;
    LfoBContainer: TVamDiv;
    LfoBContainerLabel: TVamLabel;
    LfoBLabel2: TVamLabel;
    LfoBLabel1: TVamLabel;
    LfoBShapeSelector: TVamTextBox;
    LfoBKnob1: TVamKnob;
    LfoBKnob2: TVamKnob;
    LfoBKnob3: TVamKnob;
    LfoBLabel3: TVamLabel;
    LfoBFreqModeSelector: TVamTextBox;
    AmpEnvSnapButton: TVamTextBox;
    ModEnvSnapButton: TVamTextBox;
    procedure FilterKnobMouseEnter(Sender: TObject);
    procedure FilterKnobMouseLeave(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FilterOneContainerLabelClick(Sender: TObject);
  private
    fPlugin: TeePlugin;
    KnobList : TObjectList; //TODO:MED is this knob list being used?
    procedure UpdateControlVisibility;
    procedure UpdateLfo; //called when the mod slot changes...
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);
  protected
    AltFilterText : TAltFilterText;
    FilterParameterInfo : TFilterParameterInfo;

    ParIndexFilter1Type : integer;
    ParIndexFilter2Type : integer;

    StepSequenceMenu : TStepSequenceMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:eeGuiStandardv2.TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  {$IFDEF Logging}VamLib.LoggingProxy,{$ENDIF}
  TestDialog,
  Math,
  eeTypes,
  VamQuery,
  RedFox,
  MadExcept,
  VamLayoutWizard,
  RedFoxColor, uLucidityEnums,
  uConstants, uGuiUtils,
  GuiDrawingRoutines,
  InWindowDialog,
  Lucidity.KeyGroup,
  LucidityModConnections;

{$R *.dfm}

{ TModControlFrame }

constructor TModControlFrame.Create(AOwner: TComponent);
var
  c1: Integer;
begin
  inherited;

  Timer1.Enabled := false;

  AltFilterText.ShowAltText1 := false;
  AltFilterText.ShowAltText2 := false;

  StepSequenceMenu := TStepSequenceMenu.Create;

  KnobList := TObjectList.Create;

  KnobList.Add(AmpEnvAttackKnob);
  KnobList.Add(AmpEnvHoldKnob);
  KnobList.Add(AmpEnvDecayKnob);
  KnobList.Add(AmpEnvSustainKnob);
  KnobList.Add(AmpEnvReleaseKnob);
  KnobList.Add(ModEnvAttackKnob);
  KnobList.Add(ModEnvHoldKnob);
  KnobList.Add(ModEnvDecayKnob);
  KnobList.Add(ModEnvSustainKnob);
  KnobList.Add(ModEnvReleaseKnob);
  KnobList.Add(Filter1Par1Knob);
  KnobList.Add(Filter1Par2Knob);
  KnobList.Add(Filter1Par3Knob);
  KnobList.Add(Filter1Par4Knob);
  KnobList.Add(Filter2Par1Knob);
  KnobList.Add(Filter2Par2Knob);
  KnobList.Add(Filter2Par3Knob);
  KnobList.Add(Filter2Par4Knob);
  KnobList.Add(LfoAKnob1);
  KnobList.Add(LfoAKnob2);
  KnobList.Add(LfoAKnob3);
  KnobList.Add(LfoBKnob1);
  KnobList.Add(LfoBKnob2);
  KnobList.Add(LfoBKnob3);
  KnobList.Add(FilterBlendKnob);

  for c1 := 0 to KnobList.Count-1 do
  begin
    (KnobList[c1] as TVamKnob).ModEditRadius := 0.40;
  end;



  LfoAKnob1.ParameterName := PluginParToName(TPluginParameter.Lfo1Par1);
  LfoAKnob2.ParameterName := PluginParToName(TPluginParameter.Lfo1Par2);
  LfoAKnob3.ParameterName := PluginParToName(TPluginParameter.Lfo1Par3);

  LfoAShapeSelector.ParameterName    := PluginParToName(TPluginParameter.Lfo1Shape);
  LfoAFreqModeSelector.ParameterName := PluginParToName(TPluginParameter.Lfo1FreqMode);

  LfoBKnob1.ParameterName := PluginParToName(TPluginParameter.Lfo2Par1);
  LfoBKnob2.ParameterName := PluginParToName(TPluginParameter.Lfo2Par2);
  LfoBKnob3.ParameterName := PluginParToName(TPluginParameter.Lfo2Par3);

  LfoBShapeSelector.ParameterName    := PluginParToName(TPluginParameter.Lfo2Shape);
  LfoBFreqModeSelector.ParameterName := PluginParToName(TPluginParameter.Lfo2FreqMode);


end;

destructor TModControlFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  StepSequenceMenu.Free;
  KnobList.Free;
  inherited;
end;

procedure TModControlFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:eeGuiStandardv2.TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
const
  kContainerWidth  = 600;
  kContainerHeight = 322;
var
  FilterKnobWidth : integer;
  FilterKnobHeight : integer;
  kw, kh : integer;
  KnobTop : integer;
  RowHeight : integer;
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;

  StepSequenceMenu.Initialize(aPlugin, aDialogDisplayArea);

  GuiStandard_RegisterControl(aGuiStandard, AmpEnvAttackKnob,                TPluginParameter.AmpAttack);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvHoldKnob,                  TPluginParameter.AmpHold);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvDecayKnob,                 TPluginParameter.AmpDecay);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvSustainKnob,               TPluginParameter.AmpSustain);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvReleaseKnob,               TPluginParameter.AmpRelease);

  GuiStandard_RegisterControl(aGuiStandard, ModEnvAttackKnob,                TPluginParameter.ModAttack);
  GuiStandard_RegisterControl(aGuiStandard, ModEnvHoldKnob,                  TPluginParameter.ModHold);
  GuiStandard_RegisterControl(aGuiStandard, ModEnvDecayKnob,                 TPluginParameter.ModDecay);
  GuiStandard_RegisterControl(aGuiStandard, ModEnvSustainKnob,               TPluginParameter.ModSustain);
  GuiStandard_RegisterControl(aGuiStandard, ModEnvReleaseKnob,               TPluginParameter.ModRelease);

  GuiStandard_RegisterControl(aGuiStandard, FilterBlendKnob,                 TPluginParameter.FilterOutputBlend);
  GuiStandard_RegisterControl(aGuiStandard, Filter1Par1Knob,                 TPluginParameter.Filter1Par1);
  GuiStandard_RegisterControl(aGuiStandard, Filter1Par2Knob,                 TPluginParameter.Filter1Par2);
  GuiStandard_RegisterControl(aGuiStandard, Filter1Par3Knob,                 TPluginParameter.Filter1Par3);
  GuiStandard_RegisterControl(aGuiStandard, Filter1Par4Knob,                 TPluginParameter.Filter1Par4);
  GuiStandard_RegisterControl(aGuiStandard, Filter2Par1Knob,                 TPluginParameter.Filter2Par1);
  GuiStandard_RegisterControl(aGuiStandard, Filter2Par2Knob,                 TPluginParameter.Filter2Par2);
  GuiStandard_RegisterControl(aGuiStandard, Filter2Par3Knob,                 TPluginParameter.Filter2Par3);
  GuiStandard_RegisterControl(aGuiStandard, Filter2Par4Knob,                 TPluginParameter.Filter2Par4);
  GuiStandard_RegisterControl(aGuiStandard, Filter1KeyTrackKnob,             TPluginParameter.Filter1KeyFollow);
  GuiStandard_RegisterControl(aGuiStandard, Filter2KeyTrackKnob,             TPluginParameter.Filter2KeyFollow);
  GuiStandard_RegisterControl(aGuiStandard, LfoAKnob1,                       TPluginParameter.Lfo1Par1);
  GuiStandard_RegisterControl(aGuiStandard, LfoAKnob2,                       TPluginParameter.Lfo1Par2);
  GuiStandard_RegisterControl(aGuiStandard, LfoAKnob3,                       TPluginParameter.Lfo1Par3);
  GuiStandard_RegisterControl(aGuiStandard, LfoBKnob1,                       TPluginParameter.Lfo2Par1);
  GuiStandard_RegisterControl(aGuiStandard, LfoBKnob2,                       TPluginParameter.Lfo2Par2);
  GuiStandard_RegisterControl(aGuiStandard, LfoBKnob3,                       TPluginParameter.Lfo2Par3);

  GuiStandard_RegisterMenuButton(aGuiStandard, FilterRoutingButton,    TPluginParameter.FilterRouting);
  GuiStandard_RegisterMenuButton(aGuiStandard, Filter1TypeTextBox,     TPluginParameter.Filter1Type);
  GuiStandard_RegisterMenuButton(aGuiStandard, Filter2TypeTextBox,     TPluginParameter.Filter2Type);
  GuiStandard_RegisterMenuButton(aGuiStandard, AmpVelocityButton,      TPluginParameter.AmpVelocity);
  GuiStandard_RegisterMenuButton(aGuiStandard, ModEnvVelocityButton,   TPluginParameter.ModVelocity);
  GuiStandard_RegisterMenuButton(aGuiStandard, AmpEnvSnapButton,       TPluginParameter.AmpEnvSnap);
  GuiStandard_RegisterMenuButton(aGuiStandard, ModEnvSnapButton,       TPluginParameter.ModEnvSnap);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq1ClockTextBox,       TPluginParameter.Seq1Clock);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq1DirectionTextBox,   TPluginParameter.Seq1Direction);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq1StepsTextBox,       TPluginParameter.Seq1Length);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq2ClockTextBox,       TPluginParameter.Seq2Clock);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq2DirectionTextBox,   TPluginParameter.Seq2Direction);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq2StepsTextBox,       TPluginParameter.Seq2Length);

  GuiStandard_RegisterMenuButton(aGuiStandard, LfoAShapeSelector,       TPluginParameter.Lfo1Shape);
  GuiStandard_RegisterMenuButton(aGuiStandard, LfoAFreqModeSelector,    TPluginParameter.Lfo1FreqMode);
  GuiStandard_RegisterMenuButton(aGuiStandard, LfoBShapeSelector,       TPluginParameter.Lfo2Shape);
  GuiStandard_RegisterMenuButton(aGuiStandard, LfoBFreqModeSelector,    TPluginParameter.Lfo2FreqMode);

  //============================================================================
  //                         GUI Stylings
  //============================================================================

  FilterKnobWidth  := TGuiConst.KnobWidth;
  FilterKnobHeight := TGuiConst.KnobHeight;
  RowHeight := TGuiConst.KnobHeight + TGuiConst.SectionLabelHeight + TGuiConst.KnobLabelHeight + TGuiConst.SelectorButtonHeight;

  //======= row 1 =======
  Filter1Container.Width  := (4 * FilterKnobWidth);
  Filter1Container.Height := RowHeight;
  Filter1Container.Layout.SetPos(16,8);
  //Filter1Container.Layout.SetPos(kContainerWidth - 16, 8, TAlignPoint.TopRight);
      Filter1TypeTextBox.Layout.SetSize(FilterKnobWidth * 2, TGuiConst.SelectorButtonHeight).snapToParentEdge(TControlFeature.BottomEdge);

      Filter1KeyTrackKnob.Layout.SetSize(FilterKnobWidth * 2, TGuiConst.SelectorButtonHeight);
      Filter1KeyTrackKnob.Layout.Anchor(Filter1TypeTextBox).SnapToEdge(TControlFeature.RightEdge);

      Filter1TypeTextBox.Layout.AdjustBounds(-4,0,-4,0);
      Filter1KeyTrackKnob.Layout.AdjustBounds(-4,0,-4,0);

  LfoAContainer.Width := (FilterKnobWidth * 3);
  LfoAContainer.Height := RowHeight;
  LfoAContainer.Layout.SetPos(kContainerWidth - 16, 8, TAlignPoint.TopRight);


  AmpEnvContainer.Width := (5 * FilterKnobWidth);
  AmpEnvContainer.Height := RowHeight;
  AmpEnvContainer.Layout.Anchor(LfoAContainer).SnapToEdge(TControlFeature.LeftEdge).Move(-16,0);


  //======= row 2 =======
  //Row2.Height := 82;
  //Row2.Align := alTop;
  //Row2.Margins.Bottom := 16;

  FilterTwoContainer.Width  := (4 * FilterKnobWidth);
  FilterTwoContainer.Height := RowHeight;
  FilterTwoContainer.Layout.Anchor(Filter1Container).SnapToEdge(TControlFeature.BottomEdge).Move(0,16);
      Filter2TypeTextBox.Layout.SetSize(FilterKnobWidth * 2, TGuiConst.SelectorButtonHeight).snapToParentEdge(TControlFeature.BottomEdge);

      Filter2KeyTrackKnob.Layout.SetSize(FilterKnobWidth * 2, TGuiConst.SelectorButtonHeight);
      Filter2KeyTrackKnob.Layout.Anchor(Filter2TypeTextBox).SnapToEdge(TControlFeature.RightEdge);

      Filter2TypeTextBox.Layout.AdjustBounds(-4,0,-4,0);
      Filter2KeyTrackKnob.Layout.AdjustBounds(-4,0,-4,0);

  ModEnvContainer.Width := (5 * FilterKnobWidth);
  ModEnvContainer.Height := RowHeight;
  ModEnvContainer.Layout.Anchor(AmpEnvContainer).SnapToEdge(TControlFeature.BottomEdge).Move(0,16);

  FilterBlendContainer.Width := (FilterKnobWidth);
  FilterBlendContainer.Height := RowHeight;
  FilterBlendContainer.Layout.Anchor(Filter1Container).SnapToEdge(TControlFeature.RightEdge).Move(16,0);


  LfoBContainer.Width := (FilterKnobWidth * 3);
  LfoBContainer.Height := RowHeight;
  LfoBContainer.Layout.Anchor(LfoAContainer).SnapToEdge(TControlFeature.BottomEdge).Move(0,16);




  //======= row 3 =======
  //Row3.Height := 78;
  //Row3.Align := alTop;
  //Row3.Margins.Bottom := 16;


  StepSeq1Container.Width  := (600 - (16 * 3)) div 2;
  StepSeq1Container.Height := RowHeight;
  StepSeq1Container.Left   := 16;
  StepSeq1Container.Top    := RowHeight * 2 + 32 + 8;

  StepSeq2Container.Width  := (600 - (16 * 3)) div 2;
  StepSeq2Container.Height := RowHeight;
  StepSeq2Container.Top    := RowHeight * 2 + 40;
  StepSeq2Container.Left   := 600 - StepSeq2Container.Width - 16;

  //==================================================

  // Make all containers disapper.
  //Filter1Container.Visible := false;
  //AmpEnvContainer.Visible := false;

  //LfoBContainer.Visible := false;
  //LfoAContainer.Visible := false;

  //FilterTwoContainer.Visible := false;
  //FilterEnvContainer.Visible := false;
  //ModEnvBContainer.Visible := false;
  //ModEnvAContainer.Visible := false;

  //StepSeq1Container.Visible := false;
  //StepSeq2Container.Visible := false;

  //==================================================

  //==== Filter 1 controls ====
  Filter1Par1Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).SetPos(0, TGuiConst.SectionLabelHeight);
  Filter1Par2Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).Anchor(Filter1Par1Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter1Par3Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).Anchor(Filter1Par2Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter1Par4Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).Anchor(Filter1Par3Knob).SnapToEdge(TControlFeature.RightEdge);

  Filter1P1Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter1Par1Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter1P2Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter1Par2Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter1P3Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter1Par3Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter1P4Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter1Par4Knob).SnapToEdge(TControlFeature.BottomEdge);

  //==== Filter 2 controls ====
  Filter2Par1Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).SetPos(0, TGuiConst.SectionLabelHeight);
  Filter2Par2Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).Anchor(Filter2Par1Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter2Par3Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).Anchor(Filter2Par2Knob).SnapToEdge(TControlFeature.RightEdge);
  Filter2Par4Knob.Layout.SetSize(FilterKnobWidth,FilterKnobHeight).Anchor(Filter2Par3Knob).SnapToEdge(TControlFeature.RightEdge);

  Filter2P1Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter2Par1Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter2P2Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter2Par2Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter2P3Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter2Par3Knob).SnapToEdge(TControlFeature.BottomEdge);
  Filter2P4Label.Layout.SetSize(FilterKnobWidth,TGuiConst.KnobLabelHeight).Anchor(Filter2Par4Knob).SnapToEdge(TControlFeature.BottomEdge);



  //=== ADSR 1 ====
  kw := TGuiConst.KnobWidth;
  kh := TGuiConst.KnobHeight;
  KnobTop := TGuiConst.SectionLabelHeight;

  AmpEnvAttackKnob.Layout.SetSize(kw, kh).SetPos(0, KnobTop);
  AmpEnvHoldKnob.Layout.SetSize(kw, kh).SetPos(1 * kw, KnobTop);
  AmpEnvDecayKnob.Layout.SetSize(kw, kh).SetPos(2 * kw, KnobTop);
  AmpEnvSustainKnob.Layout.SetSize(kw, kh).SetPos(3 * kw, KnobTop);
  AmpEnvReleaseKnob.Layout.SetSize(kw, kh).SetPos(4 * kw, KnobTop);

  AmpEnvAttackLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(AmpEnvAttackKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvHoldLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(AmpEnvHoldKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvDecayLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(AmpEnvDecayKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvSustainLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(AmpEnvSustainKnob).SnapToEdge(TControlFeature.BottomEdge);
  AmpEnvReleaseLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(AmpEnvReleaseKnob).SnapToEdge(TControlFeature.BottomEdge);

  AmpEnvSnapButton.Layout.SetSize(58, TGuiConst.SelectorButtonHeight);
  AmpVelocityButton.Layout.SetSize(58, TGuiConst.SelectorButtonHeight);

  AmpVelocityButton.Layout.AlignWithinParent(TRedFoxAlign.AlignFar, TRedFoxAlign.AlignFar);
  AmpEnvSnapButton.Layout.Anchor(AmpVelocityButton).SnapToEdge(TControlFeature.LeftEdge).Move(-4, 0);

  //==================================================


  //=== ADSR 2 ====
  kw := TGuiConst.KnobWidth;
  kh := TGuiConst.KnobHeight;
  KnobTop := TGuiConst.SectionLabelHeight;

  ModEnvAttackKnob.Layout.SetSize(kw, kh).SetPos(0,KnobTop);
  ModEnvHoldKnob.Layout.SetSize(kw, kh).SetPos(1 * kw, KnobTop);
  ModEnvDecayKnob.Layout.SetSize(kw, kh).SetPos(2 * kw, KnobTop);
  ModEnvSustainKnob.Layout.SetSize(kw, kh).SetPos(3 * kw, KnobTop);
  ModEnvReleaseKnob.Layout.SetSize(kw, kh).SetPos(4 * kw, KnobTop);

  ModEnvAttackLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(ModEnvAttackKnob).SnapToEdge(TControlFeature.BottomEdge);
  ModEnvHoldLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(ModEnvHoldKnob).SnapToEdge(TControlFeature.BottomEdge);
  ModEnvDecayLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(ModEnvDecayKnob).SnapToEdge(TControlFeature.BottomEdge);
  ModEnvSustainLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(ModEnvSustainKnob).SnapToEdge(TControlFeature.BottomEdge);
  ModEnvReleaseLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(ModEnvReleaseKnob).SnapToEdge(TControlFeature.BottomEdge);

  ModEnvSnapButton.Layout.SetSize(58, TGuiConst.SelectorButtonHeight);
  ModEnvVelocityButton.Layout.SetSize(58, TGuiConst.SelectorButtonHeight);

  ModEnvVelocityButton.Layout.AlignWithinParent(TRedFoxAlign.AlignFar, TRedFoxAlign.AlignFar);
  ModEnvSnapButton.Layout.Anchor(ModEnvVelocityButton).SnapToEdge(TControlFeature.LeftEdge).Move(-4, 0);
  //==================================================





  //==== LFO 1 ========================================
  LfoAContainerLabel.Align  := alTop;
  LfoAContainerLabel.Height := TGuiConst.SectionLabelHeight;
  LfoAContainerLabel.TextAlign := TRedFoxAlign.AlignNear;
  LfoAContainerLabel.Layout.SetPos(12, 0);

  LfoAKnob1.Layout.SetSize(kw, kh).SetPos(0,TGuiConst.SectionLabelHeight);
  LfoAKnob2.Layout.SetSize(kw, kh).Anchor(LfoAKnob1).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge);
  LfoAKnob3.Layout.SetSize(kw, kh).Anchor(LfoAKnob2).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge);

  LfoALabel1.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoAKnob1).SnapToEdge(TControlFeature.BottomEdge);
  LfoALabel2.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoAKnob2).SnapToEdge(TControlFeature.BottomEdge);
  LfoALabel3.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoAKnob3).SnapToEdge(TControlFeature.BottomEdge);

  // NOTE: The LFO Shape selector is wider than the others, but might be reduced if I add "Global/Reset/Free" lfo mode selector button.
  //LfoAShapeSelector.Layout.SetSize(40, TGuiConst.SelectorButtonHeight).SnapToParentEdge(TControlFeature.BottomEdge);
  LfoAShapeSelector.Layout.SetSize(LfoAContainer.Width div 2, TGuiConst.SelectorButtonHeight).SnapToParentEdge(TControlFeature.BottomEdge);

  LfoAFreqModeSelector.Layout.SetSize(LfoAContainer.Width div 2, TGuiConst.SelectorButtonHeight);
  LfoAFreqModeSelector.Layout.Anchor(LfoAShapeSelector).SnapToEdge(TControlFeature.RightEdge);

  LfoAShapeSelector.Layout.AdjustBounds(0,0,-2,0);
  LfoAFreqModeSelector.Layout.AdjustBounds(-2,0,-2,0);
  //==================================================


  //==== LFO B ========================================
  LfoBContainerLabel.Align  := alTop;
  LfoBContainerLabel.Height := TGuiConst.SectionLabelHeight;
  LfoBContainerLabel.TextAlign := TRedFoxAlign.AlignNear;
  LfoBContainerLabel.Layout.SetPos(12, 0);

  LfoBKnob1.Layout.SetSize(kw, kh).SetPos(0,TGuiConst.SectionLabelHeight);
  LfoBKnob2.Layout.SetSize(kw, kh).Anchor(LfoBKnob1).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge);
  LfoBKnob3.Layout.SetSize(kw, kh).Anchor(LfoBKnob2).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge);

  LfoBLabel1.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoBKnob1).SnapToEdge(TControlFeature.BottomEdge);
  LfoBLabel2.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoBKnob2).SnapToEdge(TControlFeature.BottomEdge);
  LfoBLabel3.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoBKnob3).SnapToEdge(TControlFeature.BottomEdge);

  // NOTE: The LFO Shape selector is wider than the others, but might be reduced if I add "Global/Reset/Free" lfo mode selector button.
  //LfoBShapeSelector.Layout.SetSize(40, TGuiConst.SelectorButtonHeight).SnapToParentEdge(TControlFeature.BottomEdge);
  LfoBShapeSelector.Layout.SetSize(LfoAContainer.Width div 2, TGuiConst.SelectorButtonHeight).SnapToParentEdge(TControlFeature.BottomEdge);

  LfoBFreqModeSelector.Layout.SetSize(LfoAContainer.Width div 2, TGuiConst.SelectorButtonHeight);
  LfoBFreqModeSelector.Layout.Anchor(LfoBShapeSelector).SnapToEdge(TControlFeature.RightEdge);

  LfoBShapeSelector.Layout.AdjustBounds(0,0,-2,0);
  LfoBFreqModeSelector.Layout.AdjustBounds(-2,0,-2,0);
  //==================================================



  //=== Filter Blend ====
  FilterBlendKnob.Layout.SetSize(kw, kh).SetPos(0,TGuiConst.SectionLabelHeight);
  FilterBlendLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight);
  FilterBlendLabel.Layout.Anchor(FilterBlendKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterRoutingButton.Layout.SetSize(FilterKnobWidth, TGuiConst.SelectorButtonHeight).Anchor(FilterBlendLabel).SnapToEdge(TControlFeature.BottomEdge);
  //==================================================



  //=== colors ===
  GuiSetup.StyleButton_SelectorButton(AmpVelocityButton);
  GuiSetup.StyleButton_SelectorButton(AmpEnvSnapButton);
  GuiSetup.StyleButton_SelectorButton(ModEnvVelocityButton);
  GuiSetup.StyleButton_SelectorButton(ModEnvSnapButton);
  GuiSetup.StyleButton_SelectorButton(FilterRoutingButton);
  GuiSetup.StyleButton_SelectorButton(Filter1TypeTextBox);
  GuiSetup.StyleButton_SelectorButton(Filter2TypeTextBox);
  GuiSetup.StyleButton_SelectorButton(LfoAShapeSelector);
  GuiSetup.StyleButton_SelectorButton(LfoAFreqModeSelector);
  GuiSetup.StyleButton_SelectorButton(LfoBShapeSelector);
  GuiSetup.StyleButton_SelectorButton(LfoBFreqModeSelector);
  GuiSetup.StyleButton_SelectorButton(Seq1ClockTextBox);
  GuiSetup.StyleButton_SelectorButton(Seq1DirectionTextBox);
  GuiSetup.StyleButton_SelectorButton(Seq1StepsTextBox);
  GuiSetup.StyleButton_SelectorButton(Seq2ClockTextBox);
  GuiSetup.StyleButton_SelectorButton(Seq2DirectionTextBox);
  GuiSetup.StyleButton_SelectorButton(Seq2StepsTextBox);

  Filter1KeyTrackKnob.Padding.Left  := 4;
  Filter1KeyTrackKnob.Padding.Right := 4;
  Filter1KeyTrackKnob.Color_Background := kColor_LcdDark1;
  Filter1KeyTrackKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  Filter1KeyTrackKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);
  Filter1KeyTrackKnob.Color_Arrows1  := kArrowColor1;
  Filter1KeyTrackKnob.Color_Arrows2  := kArrowColor2;

  Filter2KeyTrackKnob.Padding.Left  := 4;
  Filter2KeyTrackKnob.Padding.Right := 4;
  Filter2KeyTrackKnob.Color_Background := kColor_LcdDark1;
  Filter2KeyTrackKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  Filter2KeyTrackKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);
  Filter2KeyTrackKnob.Color_Arrows1  := kArrowColor1;
  Filter2KeyTrackKnob.Color_Arrows2  := kArrowColor2;


  //TODO:MED Delete this timer.
  Timer1.Enabled := true;
  Timer1.Interval := 25;


  //== finally, call the message handlers to ensure everything is up to date ===
  UpdateControlVisibility;
  UpdateLfo;
end;

procedure TModControlFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IInterface);
begin
  if MsgID = TLucidMsgID.OnPostCreateFinished            then UpdateControlVisibility;
  if MsgID = TLucidMsgID.NewPatchLoaded                  then UpdateControlVisibility;
  if MsgID = TLucidMsgID.SampleFocusChanged              then UpdateControlVisibility;
  if MsgID = TLucidMsgID.FilterChanged                   then UpdateControlVisibility;
  if MsgID = TLucidMsgID.Command_UpdateControlVisibility then UpdateControlVisibility;
  if MsgID = TLucidMsgID.LfoChanged                      then UpdateLfo;
end;

procedure TModControlFrame.UpdateControlVisibility;
var
  Knobs : array[0..3] of TControl;
  Labels : array[0..3] of TControl;
  ParName  : string;
  ParID    : TPluginParameterID;
  ParValue : single;
  FT  : TFilterType;
begin
  // TODO:MED this needs to be cleaned up.
  ParName := PluginParToName(TPluginParameter.Filter1Type);
  ParID := PluginParToID(TPluginParameter.Filter1Type);
  ParValue := Plugin.GetPluginParameter(ParID);
  FT := TFilterTypeHelper.ToEnum(ParValue);

  Knobs[0] := Filter1Par1Knob;
  Knobs[1] := Filter1Par2Knob;
  Knobs[2] := Filter1Par3Knob;
  Knobs[3] := Filter1Par4Knob;

  Labels[0] := Filter1P1Label;
  Labels[1] := Filter1P2Label;
  Labels[2] := Filter1P3Label;
  Labels[3] := Filter1P4Label;

  UpdateFilterControls(Knobs, Labels, FT);



  ParName := PluginParToName(TPluginParameter.Filter2Type);
  ParID   := PluginParToID(TPluginParameter.Filter2Type);
  ParValue := Plugin.GetPluginParameter(ParID);
  FT := TFilterTypeHelper.ToEnum(ParValue);

  Knobs[0] := Filter2Par1Knob;
  Knobs[1] := Filter2Par2Knob;
  Knobs[2] := Filter2Par3Knob;
  Knobs[3] := Filter2Par4Knob;

  Labels[0] := Filter2P1Label;
  Labels[1] := Filter2P2Label;
  Labels[2] := Filter2P3Label;
  Labels[3] := Filter2P4Label;

  UpdateFilterControls(Knobs, Labels, FT);
end;

procedure TModControlFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
  //FilterOneContainerLabel.Text := FloatToStr(Floor(Plugin.Globals.ppqPos));
end;

procedure TModControlFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TModControlFrame.FilterKnobMouseEnter(Sender: TObject);
begin
  // Is this code needed?
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




procedure TModControlFrame.Timer1Timer(Sender: TObject);
begin
  //TODO:MED delete this timer.
end;

procedure TModControlFrame.UpdateLfo;
var
  CurrentLfoShape : TLfoShape;
begin
  // TODO:LOW: there's duplicated code here. It might be better to refactor that away.


  //==== LFO 1 ===================
  CurrentLfoShape := Command.GetParValue<TLfoShape>(Plugin, TPluginParameter.Lfo1Shape);


  case CurrentLfoShape of
    TLfoShape.SawUp,
    TLfoShape.SawDown,
    TLfoShape.Square,
    TLfoShape.Triangle,
    TLfoShape.Sine:
    begin
      LfoALabel1.Text := 'RATE';
      LfoALabel2.Text := 'PH';
      LfoALabel3.Text := 'SYM';
    end;

    TLfoShape.RandomStepped,
    TLfoShape.RandomSmooth:
    begin
      LfoALabel1.Text := 'RATE';
      LfoALabel2.Text := '%';
      LfoALabel3.Text := 'FLUX';
    end;

    TLfoShape.Cycle,
    TLfoShape.AttackRelease,
    TLfoShape.AttackDecay:
    begin
      LfoALabel1.Text := 'RATE';
      LfoALabel2.Text := 'BIAS';
      LfoALabel3.Text := 'BEND';
    end;
  else
    raise Exception.Create('Type not handled.');
  end;



  //==== LFO 2 ===================
  CurrentLfoShape := Command.GetParValue<TLfoShape>(Plugin, TPluginParameter.Lfo2Shape);


  case CurrentLfoShape of
    TLfoShape.SawUp,
    TLfoShape.SawDown,
    TLfoShape.Square,
    TLfoShape.Triangle,
    TLfoShape.Sine:
    begin
      LfoBLabel1.Text := 'RATE';
      LfoBLabel2.Text := 'PH';
      LfoBLabel3.Text := 'SYM';
    end;

    TLfoShape.RandomStepped,
    TLfoShape.RandomSmooth:
    begin
      LfoBLabel1.Text := 'RATE';
      LfoBLabel2.Text := '%';
      LfoBLabel3.Text := 'FLUX';
    end;

    TLfoShape.Cycle,
    TLfoShape.AttackRelease,
    TLfoShape.AttackDecay:
    begin
      LfoBLabel1.Text := 'RATE';
      LfoBLabel2.Text := 'BIAS';
      LfoBLabel3.Text := 'BEND';
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

procedure TModControlFrame.FilterOneContainerLabelClick(Sender: TObject);
var
  TestDialog : TTestDialog;
begin
  TestDialog := TTestDialog.Create;
  TestDialog.ShowInWindow_WithAutoFree(TForm(Plugin.Globals.TopGuiWindow), false, false);
end;

end.

