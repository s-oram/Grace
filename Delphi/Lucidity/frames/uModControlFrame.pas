unit uModControlFrame;

interface

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
    VamLabel16: TVamLabel;
    Filter2P1Label: TVamLabel;
    Filter2P2Label: TVamLabel;
    Filter2P3Label: TVamLabel;
    Filter2TypeTextBox: TVamTextBox;
    Filter2Par1Knob: TVamKnob;
    Filter2Par2Knob: TVamKnob;
    Filter2Par3Knob: TVamKnob;
    Filter2Par4Knob: TVamKnob;
    Filter2P4Label: TVamLabel;
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
    FilterVelocityButton: TVamTextBox;
    LfoAContainer: TVamDiv;
    LfoContainerLabel: TVamLabel;
    LfoLabel2: TVamLabel;
    LfoLabel1: TVamLabel;
    LfoShapeSelector: TVamTextBox;
    LfoKnob1: TVamKnob;
    LfoKnob2: TVamKnob;
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
    AmpVelocityButton: TVamTextBox;
    Filter1Container: TVamDiv;
    VamLabel11: TVamLabel;
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
    VamLabel13: TVamLabel;
    FilterBlendLabel: TVamLabel;
    FilterBlendKnob: TVamKnob;
    LfoKnob3: TVamKnob;
    LfoLabel3: TVamLabel;
    LfoSelectButton1: TVamButton;
    LfoSelectButton2: TVamButton;
    Timer1: TTimer;
    LfoSelector: TVamSliderSwitch;
    Filter1KeyTrackKnob: TVamCompoundNumericKnob;
    Filter2KeyTrackKnob: TVamCompoundNumericKnob;
    LfoFreqModeSelector: TVamTextBox;
    FilterRoutingButton: TVamTextBox;
    LfoRangeSelector: TVamTextBox;
    procedure FilterKnobMouseEnter(Sender: TObject);
    procedure FilterKnobMouseLeave(Sender: TObject);
    procedure LfoSelectButton1Changed(Sender: TObject);
    procedure LfoSelectButton2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure LfoSelectorChanged(Sender: TObject);
    procedure LfoSelectorMouseEnter(Sender: TObject);
    procedure LfoSelectorMouseLeave(Sender: TObject);
  private
    fPlugin: TeePlugin;
    KnobList : TObjectList;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);
    procedure UpdateControlVisibility;
    procedure UpdateLfo; //called when the mod slot changes...

  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    AltFilterText : TAltFilterText;
    FilterParameterInfo : TFilterParameterInfo;

    ParIndexFilter1Type : integer;
    ParIndexFilter2Type : integer;

    StepSequenceMenu : TStepSequenceMenu;

    Scope : TLucidityScope;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:eeGuiStandardv2.TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;

    procedure FilterChanged;
  end;

implementation

uses
  VamQuery,
  RedFox,
  eeVstParameterEx,
  MadExcept,
  VamLayoutWizard, eeVstParameter,
  RedFoxColor, uLucidityEnums,
  uConstants, uGuiUtils,
  Lucidity.KeyGroup,
  LucidityModConnections;

{$R *.dfm}

{ TModControlFrame }

constructor TModControlFrame.Create(AOwner: TComponent);
begin
  inherited;

  Timer1.Enabled := false;

  MsgHandle := AllocateHWND(MessageHandler);

  AltFilterText.ShowAltText1 := false;
  AltFilterText.ShowAltText2 := false;

  StepSequenceMenu := TStepSequenceMenu.Create;

  KnobList := TObjectList.Create;

  KnobList.Add(AmpEnvAttackKnob);
  KnobList.Add(AmpEnvHoldKnob);
  KnobList.Add(AmpEnvDecayKnob);
  KnobList.Add(AmpEnvSustainKnob);
  KnobList.Add(AmpEnvReleaseKnob);
  KnobList.Add(FilterEnvAttackKnob);
  KnobList.Add(FilterEnvHoldKnob);
  KnobList.Add(FilterEnvDecayKnob);
  KnobList.Add(FilterEnvSustainKnob);
  KnobList.Add(FilterEnvReleaseKnob);
  KnobList.Add(Filter1Par1Knob);
  KnobList.Add(Filter1Par2Knob);
  KnobList.Add(Filter1Par3Knob);
  KnobList.Add(Filter1Par4Knob);
  KnobList.Add(Filter2Par1Knob);
  KnobList.Add(Filter2Par2Knob);
  KnobList.Add(Filter2Par3Knob);
  KnobList.Add(Filter2Par4Knob);
  KnobList.Add(LfoKnob1);
  KnobList.Add(LfoKnob2);
  KnobList.Add(LfoKnob3);
  KnobList.Add(FilterBlendKnob);


  Scope := TLucidityScope.Create(AOwner);
  Scope.Visible := true;
  Scope.Parent := BackgroundPanel;
  Scope.Name := 'Scope';

  Scope.Font.Name := 'Tahoma';
  Scope.Font.Style := [];


end;

destructor TModControlFrame.Destroy;
begin
  if (MsgHandle <> 0) and (assigned(Plugin)) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(MsgHandle);
  end;
  DeallocateHWnd(MsgHandle);

  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(Pointer(IZeroObject(Self)));

  StepSequenceMenu.Free;
  KnobList.Free;
  inherited;
end;

procedure TModControlFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:eeGuiStandardv2.TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
const
  kContainerWidth  = 600;
  kContainerHeight = 322;
var
  ParIndex : integer;
  FilterKnobWidth : integer;
  FilterKnobHeight : integer;
  kw, kh : integer;
  KnobTop : integer;
  RowHeight : integer;
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;

  if MsgHandle <> 0 then Plugin.Globals.AddWindowsMessageListener(MsgHandle);

  //TODO: This should be tied to the active voice group, or the active voice.
  // not the global scope.
  Scope.SignalRecorder := Plugin.SignalRecorder;
  Scope.FreqAnalyzer   := Plugin.FreqAnalyzer;

  StepSequenceMenu.Initialize(aPlugin, aDialogDisplayArea);

  GuiStandard_RegisterControl(aGuiStandard, AmpEnvAttackKnob,                TPluginParameter.AmpAttack);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvHoldKnob,                  TPluginParameter.AmpHold);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvDecayKnob,                 TPluginParameter.AmpDecay);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvSustainKnob,               TPluginParameter.AmpSustain);
  GuiStandard_RegisterControl(aGuiStandard, AmpEnvReleaseKnob,               TPluginParameter.AmpRelease);

  GuiStandard_RegisterControl(aGuiStandard, FilterEnvAttackKnob,             TPluginParameter.FilterAttack);
  GuiStandard_RegisterControl(aGuiStandard, FilterEnvHoldKnob,               TPluginParameter.FilterHold);
  GuiStandard_RegisterControl(aGuiStandard, FilterEnvDecayKnob,              TPluginParameter.FilterDecay);
  GuiStandard_RegisterControl(aGuiStandard, FilterEnvSustainKnob,            TPluginParameter.FilterSustain);
  GuiStandard_RegisterControl(aGuiStandard, FilterEnvReleaseKnob,            TPluginParameter.FilterRelease);

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
  GuiStandard_RegisterControl(aGuiStandard, LfoKnob1,                        TPluginParameter.Lfo1Par1);
  GuiStandard_RegisterControl(aGuiStandard, LfoKnob2,                        TPluginParameter.Lfo1Par2);
  GuiStandard_RegisterControl(aGuiStandard, LfoKnob3,                        TPluginParameter.Lfo1Par3);

  GuiStandard_RegisterMenuButton(aGuiStandard, FilterRoutingButton,    TPluginParameter.FilterRouting);
  GuiStandard_RegisterMenuButton(aGuiStandard, Filter1TypeTextBox,     TPluginParameter.Filter1Type);
  GuiStandard_RegisterMenuButton(aGuiStandard, Filter2TypeTextBox,     TPluginParameter.Filter2Type);
  GuiStandard_RegisterMenuButton(aGuiStandard, AmpVelocityButton,      TPluginParameter.AmpVelocity);
  GuiStandard_RegisterMenuButton(aGuiStandard, FilterVelocityButton,   TPluginParameter.FilterVelocity);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq1ClockTextBox,       TPluginParameter.Seq1Clock);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq1DirectionTextBox,   TPluginParameter.Seq1Direction);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq1StepsTextBox,       TPluginParameter.Seq1Length);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq2ClockTextBox,       TPluginParameter.Seq2Clock);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq2DirectionTextBox,   TPluginParameter.Seq2Direction);
  GuiStandard_RegisterMenuButton(aGuiStandard, Seq2StepsTextBox,       TPluginParameter.Seq2Length);

  GuiStandard_RegisterMenuButton(aGuiStandard, LfoShapeSelector,       TPluginParameter.Lfo1Shape);
  GuiStandard_RegisterMenuButton(aGuiStandard, LfoFreqModeSelector,    TPluginParameter.Lfo1FreqMode);
  GuiStandard_RegisterMenuButton(aGuiStandard, LfoRangeSelector,       TPluginParameter.Lfo1Range);




  //============================================================================
  AddDisplayClass(AmpEnvAttackKnob,       TScopeFocusID.AmpEnv);
  AddDisplayClass(AmpEnvHoldKnob,         TScopeFocusID.AmpEnv);
  AddDisplayClass(AmpEnvDecayKnob,        TScopeFocusID.AmpEnv);
  AddDisplayClass(AmpEnvSustainKnob,      TScopeFocusID.AmpEnv);
  AddDisplayClass(AmpEnvReleaseKnob,      TScopeFocusID.AmpEnv);

  AddDisplayClass(FilterEnvAttackKnob,    TScopeFocusID.ModEnv);
  AddDisplayClass(FilterEnvHoldKnob,      TScopeFocusID.ModEnv);
  AddDisplayClass(FilterEnvDecayKnob,     TScopeFocusID.ModEnv);
  AddDisplayClass(FilterEnvSustainKnob,   TScopeFocusID.ModEnv);
  AddDisplayClass(FilterEnvReleaseKnob,   TScopeFocusID.ModEnv);
  AddDisplayClass(Filter1Par1Knob,        TScopeFocusID.Filter1);
  AddDisplayClass(Filter1Par2Knob,        TScopeFocusID.Filter1);
  AddDisplayClass(Filter1Par3Knob,        TScopeFocusID.Filter1);
  AddDisplayClass(Filter1Par4Knob,        TScopeFocusID.Filter1);
  AddDisplayClass(Filter2Par1Knob,        TScopeFocusID.Filter2);
  AddDisplayClass(Filter2Par2Knob,        TScopeFocusID.Filter2);
  AddDisplayClass(Filter2Par3Knob,        TScopeFocusID.Filter2);
  AddDisplayClass(Filter2Par4Knob,        TScopeFocusID.Filter2);

  AddDisplayClass(FilterBlendKnob,        TScopeFocusID.FilterBlend);
  AddDisplayClass(FilterRoutingButton,    TScopeFocusID.FilterBlend);

  AddDisplayClass(Filter1TypeTextBox,     TScopeFocusID.Filter1);
  AddDisplayClass(Filter1KeyTrackKnob,    TScopeFocusID.Filter1);
  AddDisplayClass(Filter2TypeTextBox,     TScopeFocusID.Filter2);
  AddDisplayClass(Filter2KeyTrackKnob,    TScopeFocusID.Filter2);
  AddDisplayClass(AmpVelocityButton,      TScopeFocusID.AmpEnv);
  AddDisplayClass(FilterVelocityButton,   TScopeFocusID.ModEnv);


  AddDisplayClass(LfoSelectButton1, TScopeFocusID.Lfo1);
  AddDisplayClass(LfoSelectButton2, TScopeFocusID.Lfo2);
  AddDisplayClass(LfoSelector,      LfoControlDisplayClass);


  //AddDisplayClass(LfoShapeTextBox1,       TScopeFocusID);
  //AddDisplayClass(Seq1ClockTextBox,       TScopeFocusID);
  //AddDisplayClass(Seq1DirectionTextBox,   TScopeFocusID);
  //AddDisplayClass(Seq1StepsTextBox,       TScopeFocusID);
  //AddDisplayClass(Seq2ClockTextBox,       TScopeFocusID);
  //AddDisplayClass(Seq2DirectionTextBox,   TScopeFocusID);
  //AddDisplayClass(Seq2StepsTextBox,       TScopeFocusID);






  //============================================================================
  //                         GUI Stylings
  //============================================================================

  FilterKnobWidth  := TGuiConst.KnobWidth;
  FilterKnobHeight := TGuiConst.KnobHeight;
  RowHeight := TGuiConst.KnobHeight + TGuiConst.SectionLabelHeight + TGuiConst.KnobLabelHeight + TGuiConst.SelectorButtonHeight;

  //======= row 1 =======
  Filter1Container.Width  := (4 * FilterKnobWidth);
  Filter1Container.Height := RowHeight;
  Filter1Container.Layout.SetPos(kContainerWidth - 16, 8, TAlignPoint.TopRight);
      Filter1TypeTextBox.Layout.SetSize(FilterKnobWidth * 2, TGuiConst.SelectorButtonHeight).snapToParentEdge(TControlFeature.BottomEdge);

      Filter1KeyTrackKnob.Layout.SetSize(FilterKnobWidth * 2, TGuiConst.SelectorButtonHeight);
      Filter1KeyTrackKnob.Layout.Anchor(Filter1TypeTextBox).SnapToEdge(TControlFeature.RightEdge);

      Filter1TypeTextBox.Layout.AdjustBounds(-4,0,-4,0);
      Filter1KeyTrackKnob.Layout.AdjustBounds(-4,0,-4,0);


  AmpEnvContainer.Width := (5 * FilterKnobWidth);
  AmpEnvContainer.Height := RowHeight;
  AmpEnvContainer.Layout.SetPos(16, 8);


  //Scope.Layout.Anchor(AmpEnvContainer).MatchHeight.SnapToEdge(TControlFeature.RightEdge).Move(16, 0).AdjustBounds(0,-4,0,0);
  //Scope.Width := 177;
  //ShowMessage(IntToStr(Scope.Left));
  //ShowMessage(IntToStr(Scope.Top));
  //ShowMessage(IntToStr(Scope.Width));
  //ShowMessage(IntToStr(Scope.Height));

  Scope.Layout.SetPos(232,12).SetSize(177,90);


    //TODO:
    {
    ModEnvAContainer.Width := (2 * FilterKnobWidth);
    ModEnvAContainer.Height := RowHeight;
    ModEnvAContainer.Layout.Anchor(AmpEnvContainer).SnapToEdge(TControlFeature.RightEdge).Move(16,0);

    ModEnvBContainer.Width := (2 * FilterKnobWidth);
    ModEnvBContainer.Height := RowHeight;
    ModEnvBContainer.Layout.Anchor(ModEnvAContainer).SnapToEdge(TControlFeature.RightEdge).Move(16,0);
    }




    //LfoBContainer.Align := alRight;
    //LfoAContainer.Align := alRight;



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

  FilterEnvContainer.Width := (5 * FilterKnobWidth);
  FilterEnvContainer.Height := RowHeight;
  FilterEnvContainer.Layout.Anchor(AmpEnvContainer).SnapToEdge(TControlFeature.BottomEdge).Move(0,16);

  LfoAContainer.Width := (FilterKnobWidth + 32 + 32 + 8);
  LfoAContainer.Height := RowHeight;
  LfoAContainer.Layout.Anchor(FilterEnvContainer).SnapToEdge(TControlFeature.RightEdge).Move(16,0);


  FilterBlendContainer.Width := (FilterKnobWidth);
  FilterBlendContainer.Height := RowHeight;
  FilterBlendContainer.Layout.Anchor(FilterTwoContainer).SnapToEdge(TControlFeature.LeftEdge).Move(-16,0);






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

  AmpVelocityButton.Layout.Anchor(AmpEnvSustainLabel).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  AmpVelocityButton.Layout.SetSize(60, TGuiConst.SelectorButtonHeight);
  //==================================================


  //=== ADSR 2 ====
  kw := TGuiConst.KnobWidth;
  kh := TGuiConst.KnobHeight;
  KnobTop := TGuiConst.SectionLabelHeight;

  FilterEnvAttackKnob.Layout.SetSize(kw, kh).SetPos(0,KnobTop);
  FilterEnvHoldKnob.Layout.SetSize(kw, kh).SetPos(1 * kw, KnobTop);
  FilterEnvDecayKnob.Layout.SetSize(kw, kh).SetPos(2 * kw, KnobTop);
  FilterEnvSustainKnob.Layout.SetSize(kw, kh).SetPos(3 * kw, KnobTop);
  FilterEnvReleaseKnob.Layout.SetSize(kw, kh).SetPos(4 * kw, KnobTop);

  FilterEnvAttackLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(FilterEnvAttackKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvHoldLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(FilterEnvHoldKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvDecayLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(FilterEnvDecayKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvSustainLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(FilterEnvSustainKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterEnvReleaseLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(FilterEnvReleaseKnob).SnapToEdge(TControlFeature.BottomEdge);

  FilterVelocityButton.Layout.Anchor(FilterEnvSustainLabel).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  FilterVelocityButton.Layout.SetSize(60, TGuiConst.SelectorButtonHeight);
  //==================================================





  //==== LFO 1 ========================================
  LfoContainerLabel.Align  := alNone;
  LfoContainerLabel.Width  := kw * 2;
  LfoContainerLabel.Height := TGuiConst.SectionLabelHeight;
  LfoContainerLabel.TextAlign := TRedFoxAlign.AlignNear;
  LfoContainerLabel.Layout.SetPos(12, 0);

  LfoSelectButton2.Layout.SetSize(18,18);
  LfoSelectButton2.Layout.SnapToParentEdge(TControlFeature.RightEdge).Move(-6,0);
  LfoSelectButton2.Top := 0;

  LfoSelector.Layout.SetSize(30,18);
  LfoSelector.Layout.Anchor(LfoSelectButton2).SnapToEdge(TControlFeature.LeftEdge).Move(0,0);

  LfoSelectButton1.Layout.SetSize(18,18).Anchor(LfoSelector).SnapToEdge(TControlFeature.LeftEdge).Move(0,0);


  LfoKnob1.Layout.SetSize(kw, kh).SetPos(0,TGuiConst.SectionLabelHeight);
  LfoKnob2.Layout.SetSize(32, 32).Anchor(LfoKnob1).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge);
  LfoKnob3.Layout.SetSize(32, 32).Anchor(LfoKnob2).SnapToEdge(TControlFeature.RightEdge).AlignEdge(TControlFeature.BottomEdge);

  LfoLabel1.Layout.SetSize(kw, TGuiConst.KnobLabelHeight).Anchor(LfoKnob1).SnapToEdge(TControlFeature.BottomEdge);
  LfoLabel2.Layout.SetSize(32, TGuiConst.KnobLabelHeight).Anchor(LfoKnob2).SnapToEdge(TControlFeature.BottomEdge);
  LfoLabel3.Layout.SetSize(32, TGuiConst.KnobLabelHeight).Anchor(LfoKnob3).SnapToEdge(TControlFeature.BottomEdge);

  //LfoShapeTextBox1.Layout.SetSize(68, TGuiConst.SelectorButtonHeight).SnapToParentEdge(TControlFeature.BottomEdge);
  LfoShapeSelector.Layout.SetSize(40, TGuiConst.SelectorButtonHeight).SnapToParentEdge(TControlFeature.BottomEdge);

  LfoFreqModeSelector.Layout.SetSize(40, TGuiConst.SelectorButtonHeight);
  LfoFreqModeSelector.Layout.Anchor(LfoShapeSelector).SnapToEdge(TControlFeature.RightEdge);
  LfoRangeSelector.Layout.SetSize(24, TGuiConst.SelectorButtonHeight).Anchor(LfoFreqModeSelector).SnapToEdge(TControlFeature.RightEdge);

  LfoShapeSelector.Layout.AdjustBounds(0,0,-2,0);
  LfoFreqModeSelector.Layout.AdjustBounds(-2,0,-2,0);
  LfoRangeSelector.Layout.AdjustBounds(-2,0,0,0);
  //==================================================



  //=== Filter Blend ====
  FilterBlendKnob.Layout.SetSize(kw, kh).SetPos(0,TGuiConst.SectionLabelHeight);
  FilterBlendLabel.Layout.SetSize(kw, TGuiConst.KnobLabelHeight);
  FilterBlendLabel.Layout.Anchor(FilterBlendKnob).SnapToEdge(TControlFeature.BottomEdge);
  FilterRoutingButton.Layout.SetSize(FilterKnobWidth, TGuiConst.SelectorButtonHeight).Anchor(FilterBlendLabel).SnapToEdge(TControlFeature.BottomEdge);
  //==================================================





  //=== colors ===
  AmpVelocityButton.Font.Color    := GetRedFoxColor(kColor_LcdDark5);
  FilterVelocityButton.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  FilterRoutingButton.Font.Color  := GetRedFoxColor(kColor_LcdDark5);
  Filter1TypeTextBox.Font.Color   := GetRedFoxColor(kColor_LcdDark5);
  Filter2TypeTextBox.Font.Color   := GetRedFoxColor(kColor_LcdDark5);
  LfoShapeSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  LfoFreqModeSelector.Font.Color   := GetRedFoxColor(kColor_LcdDark5);
  LfoRangeSelector.Font.Color       := GetRedFoxColor(kColor_LcdDark5);
  Seq1ClockTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq1DirectionTextBox.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  Seq1StepsTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq2ClockTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Seq2DirectionTextBox.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  Seq2StepsTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  Scope.Font.Color                := GetRedFoxColor(kColor_LcdDark5);

  Filter1KeyTrackKnob.Padding.Left  := 4;
  Filter1KeyTrackKnob.Padding.Right := 4;
  Filter1KeyTrackKnob.Color_Background := kColor_LcdDark1;
  Filter1KeyTrackKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  Filter1KeyTrackKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);
  Filter1KeyTrackKnob.Color_Arrows1 := '$33FFFFFF';
  Filter1KeyTrackKnob.Color_Arrows2 := '$ccFFFFFF';

  Filter2KeyTrackKnob.Padding.Left  := 4;
  Filter2KeyTrackKnob.Padding.Right := 4;
  Filter2KeyTrackKnob.Color_Background := kColor_LcdDark1;
  Filter2KeyTrackKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  Filter2KeyTrackKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);
  Filter2KeyTrackKnob.Color_Arrows1 := '$33FFFFFF'; // TODO: This should be a constant.
  Filter2KeyTrackKnob.Color_Arrows2 := '$ccFFFFFF'; // TODO: This should be a constant.



  AmpVelocityButton.Color    := kColor_LcdDark1;
  FilterVelocityButton.Color := kColor_LcdDark1;
  FilterRoutingButton.Color  := kColor_LcdDark1;
  Filter1TypeTextBox.Color   := kColor_LcdDark1;
  Filter2TypeTextBox.Color   := kColor_LcdDark1;
  LfoShapeSelector.Color     := kColor_LcdDark1;
  LfoFreqModeSelector.Color  := kColor_LcdDark1;
  LfoRangeSelector.Color     := kColor_LcdDark1;
  Seq1ClockTextBox.Color     := kColor_LcdDark1;
  Seq1DirectionTextBox.Color := kColor_LcdDark1;
  Seq1StepsTextBox.Color     := kColor_LcdDark1;
  Seq2ClockTextBox.Color     := kColor_LcdDark1;
  Seq2DirectionTextBox.Color := kColor_LcdDark1;
  Seq2StepsTextBox.Color     := kColor_LcdDark1;

  AmpVelocityButton.ColorMouseOver    := kColor_ButtonMouseOver;
  FilterVelocityButton.ColorMouseOver := kColor_ButtonMouseOver;
  FilterRoutingButton.ColorMouseOver  := kColor_ButtonMouseOver;
  Filter1TypeTextBox.ColorMouseOver   := kColor_ButtonMouseOver;
  Filter2TypeTextBox.ColorMouseOver   := kColor_ButtonMouseOver;
  LfoShapeSelector.ColorMouseOver     := kColor_ButtonMouseOver;
  LfoFreqModeSelector.ColorMouseOver  := kColor_ButtonMouseOver;
  LfoRangeSelector.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq1ClockTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq1DirectionTextBox.ColorMouseOver := kColor_ButtonMouseOver;
  Seq1StepsTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq2ClockTextBox.ColorMouseOver     := kColor_ButtonMouseOver;
  Seq2DirectionTextBox.ColorMouseOver := kColor_ButtonMouseOver;
  Seq2StepsTextBox.ColorMouseOver     := kColor_ButtonMouseOver;

  Scope.ColorBackground := kColor_LcdDark1;
  Scope.ColorForeground := GetRedFoxColor(kColor_LcdDark5);



  LfoSelector.BackgroundImage := Plugin.Globals.SkinImageLoader.GetImage('Switch_Background');
  LfoSelector.IndexImage      := Plugin.Globals.SkinImageLoader.GetImage('Switch_Index');

  LfoSelectButton1.Color_Border := '$00000000';
  LfoSelectButton1.ColorOnA     := '$00000000';
  LfoSelectButton1.ColorOnB     := '$66000000';
  LfoSelectButton1.ColorOffA    := '$00000000';
  LfoSelectButton1.ColorOffB    := '$66000000';

  LfoSelectButton2.Color_Border := '$00000000';
  LfoSelectButton2.ColorOnA     := '$00000000';
  LfoSelectButton2.ColorOnB     := '$66000000';
  LfoSelectButton2.ColorOffA    := '$00000000';
  LfoSelectButton2.ColorOffB    := '$66000000';



  Timer1.Enabled := true;
  Timer1.Interval := 25;




  //== finally, call the message handlers to ensure everything is up to date ===
  UpdateControlVisibility;
  UpdateLfo;
end;



procedure TModControlFrame.MessageHandler(var Message: TMessage);
begin
  //TODO : delete
end;

procedure TModControlFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  if MsgID = TLucidMsgID.FilterChanged                   then UpdateControlVisibility;
  if MsgID = TLucidMsgID.Command_UpdateControlVisibility then UpdateControlVisibility;
  if MsgID = TLucidMsgID.LfoChanged                      then UpdateLfo;

end;

procedure TModControlFrame.UpdateControlVisibility;
var
  Par : TVstParameter;
  FT  : TFilterType;
  Knobs : array[0..3] of TControl;
  Labels : array[0..3] of TControl;
begin
  Par := Plugin.Globals.VstParameters.FindParameter(TParName.Filter1Type);
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



  Par := Plugin.Globals.VstParameters.FindParameter(TParName.Filter2Type);
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
end;

procedure TModControlFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

procedure TModControlFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
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



procedure TModControlFrame.Timer1Timer(Sender: TObject);
begin
  Scope.Invalidate;
end;

procedure TModControlFrame.UpdateLfo;
var
  CurrentLfoShape : TLfoShape;
begin
  assert((Plugin.Globals.SelectedLfo = 0) or (Plugin.Globals.SelectedLfo = 1));


  if Plugin.Globals.SelectedLfo = 0 then
  begin
    RemoveDisplayClass(LfoKnob1, TScopeFocusID.Lfo2);
    RemoveDisplayClass(LfoKnob2, TScopeFocusID.Lfo2);
    RemoveDisplayClass(LfoKnob3, TScopeFocusID.Lfo2);

    AddDisplayClass(LfoKnob1, TScopeFocusID.Lfo1);
    AddDisplayClass(LfoKnob2, TScopeFocusID.Lfo1);
    AddDisplayClass(LfoKnob3, TScopeFocusID.Lfo1);

    RemoveDisplayClass(LfoShapeSelector,    TScopeFocusID.Lfo2);
    RemoveDisplayClass(LfoFreqModeSelector, TScopeFocusID.Lfo2);
    RemoveDisplayClass(LfoRangeSelector,    TScopeFocusID.Lfo2);

    AddDisplayClass(LfoShapeSelector,    TScopeFocusID.Lfo1);
    AddDisplayClass(LfoFreqModeSelector, TScopeFocusID.Lfo1);
    AddDisplayClass(LfoRangeSelector,    TScopeFocusID.Lfo1);

    LfoSelectButton1.IsOn := true;
    LfoSelectButton2.IsOn := false;
    LfoSelector.SwitchPos := 0;

    LfoKnob1.ParameterName := PluginParToName(TPluginParameter.Lfo1Par1);
    LfoKnob2.ParameterName := PluginParToName(TPluginParameter.Lfo1Par2);
    LfoKnob3.ParameterName := PluginParToName(TPluginParameter.Lfo1Par3);

    LfoShapeSelector.ParameterName    := PluginParToName(TPluginParameter.Lfo1Shape);
    LfoFreqModeSelector.ParameterName := PluginParToName(TPluginParameter.Lfo1FreqMode);
    LfoRangeSelector.ParameterName    := PluginParToName(TPluginParameter.Lfo1Range);

    CurrentLfoShape := Plugin.Globals.VstParameters.FindParameter(TParName.Lfo1Shape).ValueAsEnum<TLfoShape>;
  end else
  //if Plugin.Globals.SelectedLfo = 1 then
  begin
    RemoveDisplayClass(LfoKnob1, TScopeFocusID.Lfo1);
    RemoveDisplayClass(LfoKnob2, TScopeFocusID.Lfo1);
    RemoveDisplayClass(LfoKnob3, TScopeFocusID.Lfo1);

    AddDisplayClass(LfoKnob1, TScopeFocusID.Lfo2);
    AddDisplayClass(LfoKnob2, TScopeFocusID.Lfo2);
    AddDisplayClass(LfoKnob3, TScopeFocusID.Lfo2);

    RemoveDisplayClass(LfoShapeSelector,    TScopeFocusID.Lfo1);
    RemoveDisplayClass(LfoFreqModeSelector, TScopeFocusID.Lfo1);
    RemoveDisplayClass(LfoRangeSelector,    TScopeFocusID.Lfo1);

    AddDisplayClass(LfoShapeSelector,    TScopeFocusID.Lfo2);
    AddDisplayClass(LfoFreqModeSelector, TScopeFocusID.Lfo2);
    AddDisplayClass(LfoRangeSelector,    TScopeFocusID.Lfo2);

    LfoSelectButton1.IsOn := false;
    LfoSelectButton2.IsOn := true;
    LfoSelector.SwitchPos := 1;

    LfoKnob1.ParameterName := PluginParToName(TPluginParameter.Lfo2Par1);
    LfoKnob2.ParameterName := PluginParToName(TPluginParameter.Lfo2Par2);
    LfoKnob3.ParameterName := PluginParToName(TPluginParameter.Lfo2Par3);

    LfoShapeSelector.ParameterName    := PluginParToName(TPluginParameter.Lfo2Shape);
    LfoFreqModeSelector.ParameterName := PluginParToName(TPluginParameter.Lfo2FreqMode);
    LfoRangeSelector.ParameterName    := PluginParToName(TPluginParameter.Lfo2Range);

    CurrentLfoShape := Plugin.Globals.VstParameters.FindParameter(TParName.Lfo2Shape).ValueAsEnum<TLfoShape>;
  end;



  case CurrentLfoShape of
    TLfoShape.SawUp,
    TLfoShape.SawDown,
    TLfoShape.Square,
    TLfoShape.Triangle,
    TLfoShape.Sine:
    begin
      LfoLabel1.Text := 'RATE';
      LfoLabel2.Text := 'PH';
      LfoLabel3.Text := 'SYM';
    end;

    TLfoShape.RandomStepped,
    TLfoShape.RandomSmooth:
    begin
      LfoLabel1.Text := 'RATE';
      LfoLabel2.Text := '%';
      LfoLabel3.Text := 'FLUX';
    end;

    TLfoShape.AttackDecay:
    begin
      LfoLabel1.Text := 'CURVE';
      LfoLabel2.Text := 'A';
      LfoLabel3.Text := 'D';
    end;

    TLfoShape.AttackRelease:
    begin
      LfoLabel1.Text := 'CURVE';
      LfoLabel2.Text := 'A';
      LfoLabel3.Text := 'R';
    end;

    TLfoShape.Cycle:
    begin
      LfoLabel1.Text := 'CURVE';
      LfoLabel2.Text := 'A';
      LfoLabel3.Text := 'D';
    end;
  else
    raise Exception.Create('Type not handled.');
  end;


end;

procedure TModControlFrame.LfoSelectButton1Changed(Sender: TObject);
var
  Index : integer;
begin
  Index := (Sender as TWinControl).Tag;
  Plugin.Globals.SelectedLfo := Index;
  UpdateLfo;

  //TODO: I'm not sure if this message here is entirely necessary.
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.ModSlotChanged);
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.Command_UpdateScope);
end;




procedure TModControlFrame.LfoSelectButton2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  Index := (Sender as TWinControl).Tag;
  Plugin.Globals.SelectedLfo := Index;
  UpdateLfo;

  //TODO: I'm not sure if this message here is entirely necessary.
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.ModSlotChanged);
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.Command_UpdateScope);
end;


procedure TModControlFrame.LfoSelectorChanged(Sender: TObject);
var
  Index : integer;
begin
  Index := (Sender as TVamSliderSwitch).SwitchPos;
  assert(Index >= 0);
  assert(Index <= 1);
  Plugin.Globals.SelectedLfo := Index;
  UpdateLfo;

  //TODO: I'm not sure if this message here is entirely necessary.
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.ModSlotChanged);
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.Command_UpdateScope);
end;

procedure TModControlFrame.LfoSelectorMouseEnter(Sender: TObject);
begin
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnLfoSelectorEnter);
end;

procedure TModControlFrame.LfoSelectorMouseLeave(Sender: TObject);
begin
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnLfoSelectorLeave);
end;

end.

