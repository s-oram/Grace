unit uSequencerFrame;

interface

uses
  VamLib.Vcl.ZeroFrame,
  VamLib.ZeroObject,
  uConstants,
  Menu.StepSequenceMenu,
  eePlugin, Lucidity.GuiStandard, uGuiFeedbackData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, LucidityGui.DropBoxSelector,
  VamLabel, VamDiv, LucidityGui.VectorSequence;

type
  TSequencerFrame = class(TZeroFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SeqBackPanel: TVamPanel;
    InfoDiv: TVamDiv;
    SequencerLabel: TVamLabel;
    StepCountSelector: TDropBoxSelector;
    ClockSelector: TDropBoxSelector;
    ModeSelector: TDropBoxSelector;
    StepSeqControl: TLucidityVectorSequence;
    procedure SeqBackPanelResize(Sender: TObject);
    procedure StepSeqControlShowContextMenu(Sender: TObject; X, Y: Integer);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    fSequencerIndex: integer;
    procedure SetSequencerIndex(const Value: integer);

  protected
    StepSequenceMenu : TStepSequenceMenu;
    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property SequencerIndex : integer read fSequencerIndex write SetSequencerIndex;
  end;

implementation

uses
  Lucidity.Interfaces,
  Lucidity.PluginParameters,
  Lucidity.GuiUtils,
  Lucidity.Enums,
  RedFoxColor;

{$R *.dfm}

{ TFrame1 }

constructor TSequencerFrame.Create(AOwner: TComponent);
begin
  inherited;

  StepSequenceMenu := TStepSequenceMenu.Create(AOwner);
end;

destructor TSequencerFrame.Destroy;
begin
  StepSequenceMenu.Free;
  inherited;
end;

procedure TSequencerFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  StepSequenceMenu.Initialize(aPlugin);

  StepSeqControl.Align := alClient;
  StepSeqControl.Color_Background := kColor_LcdDark1;
  StepSeqControl.Color_Border     := kColor_LcdDark1;
  StepSeqControl.Color_Step       := kColor_LcdDark4;
  StepSeqControl.Color_StepActive := kColor_LcdDark5;


  InfoDiv.Height := 20;
  SeqBackPanel.Color  := kColor_LcdDark1;

  ClockSelector.Color     := kColor_LcdDark1;
  ModeSelector.Color      := kColor_LcdDark1;
  StepCountSelector.Color := kColor_LcdDark1;

  ClockSelector.ColorMouseOver     := kColor_LcdDark2;
  ModeSelector.ColorMouseOver      := kColor_LcdDark2;
  StepCountSelector.ColorMouseOver := kColor_LcdDark2;

  ClockSelector.ColorTextA     := kColor_LcdDark4;
  ModeSelector.ColorTextA      := kColor_LcdDark4;
  StepCountSelector.ColorTextA := kColor_LcdDark4;

  ClockSelector.ColorTextB     := kColor_LcdDark5;
  ModeSelector.ColorTextB      := kColor_LcdDark5;
  StepCountSelector.ColorTextB := kColor_LcdDark5;

  SequencerLabel.Font.Color := GetTColor(kColor_LcdDark5);
  SequencerLabel.Font.Style := [];
end;

procedure TSequencerFrame.SeqBackPanelResize(Sender: TObject);
begin
  //
end;

procedure TSequencerFrame.SetSequencerIndex(const Value: integer);
begin
  if not assigned(Plugin)      then exit;
  if not assigned(GuiStandard) then exit;

  fSequencerIndex := Value;

  if fSequencerIndex = 0 then
  begin
    SequencerLabel.Text := 'Sequencer One';

    GuiStandard_RegisterMenuButton(GuiStandard, ClockSelector,       TPluginParameter.Seq1Clock);
    GuiStandard_RegisterMenuButton(GuiStandard, ModeSelector,        TPluginParameter.Seq1Direction);
    GuiStandard_RegisterMenuButton(GuiStandard, StepCountSelector,   TPluginParameter.Seq1Length);
  end else
  if fSequencerIndex = 1 then
  begin
    SequencerLabel.Text := 'Sequencer Two';

    GuiStandard_RegisterMenuButton(GuiStandard, ClockSelector,       TPluginParameter.Seq2Clock);
    GuiStandard_RegisterMenuButton(GuiStandard, ModeSelector,        TPluginParameter.Seq2Direction);
    GuiStandard_RegisterMenuButton(GuiStandard, StepCountSelector,   TPluginParameter.Seq2Length);
  end;
end;

procedure TSequencerFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
var
  CurrentStep : integer;
  SeqLength : TStepSequencerLength;
  x : integer;
begin
  if fSequencerIndex = 0
      then SeqLength := Command.GetParValue<TStepSequencerLength>(Plugin, TPluginParameter.Seq1Length)
      else SeqLength := Command.GetParValue<TStepSequencerLength>(Plugin, TPluginParameter.Seq2Length);

  x := StepSequencerLengthToInteger(SeqLength);

  if StepSeqControl.SequenceLength <> x
    then StepSeqControl.SequenceLength := x;

  if Feedback^.IsVoiceActive then
  begin
    if fSequencerIndex = 0
      then CurrentStep := Feedback^.StepSeq1CurStep
      else CurrentStep := Feedback^.StepSeq2CurStep;

    if StepSeqControl.CurrentStep <> CurrentStep then StepSeqControl.CurrentStep := CurrentStep;
  end else
  begin
    CurrentStep := -1;
    if StepSeqControl.CurrentStep <> CurrentStep then StepSeqControl.CurrentStep := CurrentStep;
  end;
end;

procedure TSequencerFrame.StepSeqControlShowContextMenu(Sender: TObject; X, Y: Integer);
begin
  StepSequenceMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, fSequencerIndex);
end;

procedure TSequencerFrame.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB:IInterface);
var
  kg : IKeyGroup;
begin
  inherited;

  // TODO:MED There is a bit of code repetition here. It could be consolidated.
  if MsgID = TLucidMsgID.RefreshRequest_StepSeqDisplay then
  begin
    kg := Plugin.FocusedKeyGroup;
    if assigned(kg)
      then StepSeqControl.SequenceData := kg.GetSequenceData(fSequencerIndex)
      else StepSeqControl.SequenceData := nil;
    StepSeqControl.Invalidate;
  end;

  if MsgID = TLucidMsgID.OnPostCreateFinished then
  begin
    kg := Plugin.FocusedKeyGroup;
    if assigned(kg)
      then StepSeqControl.SequenceData := kg.GetSequenceData(fSequencerIndex)
      else StepSeqControl.SequenceData := nil;
    StepSeqControl.Invalidate;
  end;

  if MsgID = TLucidMsgID.SampleFocusChanged then
  begin
    kg := Plugin.FocusedKeyGroup;
    if assigned(kg)
      then StepSeqControl.SequenceData := kg.GetSequenceData(fSequencerIndex)
      else StepSeqControl.SequenceData := nil;
    StepSeqControl.Invalidate;
  end;

  if MsgID = TLucidMsgID.NewProgramLoaded then
  begin
    kg := Plugin.FocusedKeyGroup;
    if assigned(kg)
      then StepSeqControl.SequenceData := kg.GetSequenceData(fSequencerIndex)
      else StepSeqControl.SequenceData := nil;
    StepSeqControl.Invalidate;
  end;

  if MsgID = TLucidMsgID.Command_DisposeKeyGroup then
  begin
    StepSeqControl.SequenceData := nil;
  end;
end;




end.
