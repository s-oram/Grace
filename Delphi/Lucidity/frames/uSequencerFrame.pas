unit uSequencerFrame;

interface

uses
  VamLib.ZeroObject,
  uConstants,
  Menu.StepSequenceMenu,
  eePlugin, eeGuiStandard, uDialogDisplayArea, uGuiFeedbackData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, LucidityGui.DropBoxSelector,
  VamLabel, VamDiv, LucidityGui.VectorSequence;

type
  TSequencerFrame = class(TFrame, IZeroObject)
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
    FMotherShip : IMotherShip;
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    fSequencerIndex: integer;
    procedure SetSequencerIndex(const Value: integer);
    function GetMotherShipReference:IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    StepSequenceMenu : TStepSequenceMenu;
    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property SequencerIndex : integer read fSequencerIndex write SetSequencerIndex;
  end;

implementation

uses
  uLucidityEnums,
  RedFoxColor;

{$R *.dfm}

{ TFrame1 }

constructor TSequencerFrame.Create(AOwner: TComponent);
begin
  inherited;

  StepSequenceMenu := TStepSequenceMenu.Create;
end;

destructor TSequencerFrame.Destroy;
begin
  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(self);

  StepSequenceMenu.Free;
  inherited;
end;

procedure TSequencerFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

function TSequencerFrame.GetMotherShipReference: IMotherShip;
begin
  result := FMotherShip;
end;

procedure TSequencerFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard; aDialogDisplayArea: TDialogDisplayArea);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  StepSequenceMenu.Initialize(aPlugin, aDialogDisplayArea);

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

    GuiStandard.RedFoxMenuHandler.RegisterControl(ClockSelector,     Plugin.Globals.VstParameters.FindParameter(TParName.Seq1Clock),      TSequencerClockHelper);
    GuiStandard.RedFoxMenuHandler.RegisterControl(ModeSelector,      Plugin.Globals.VstParameters.FindParameter(TParName.Seq1Direction),  TStepSequencerDirectionHelper);
    GuiStandard.RedFoxMenuHandler.RegisterControl(StepCountSelector, Plugin.Globals.VstParameters.FindParameter(TParName.Seq1Length),     TStepSequencerLengthHelper);
  end else
  if fSequencerIndex = 1 then
  begin
    SequencerLabel.Text := 'Sequencer Two';

    GuiStandard.RedFoxMenuHandler.RegisterControl(ClockSelector,     Plugin.Globals.VstParameters.FindParameter(TParName.Seq2Clock),      TSequencerClockHelper);
    GuiStandard.RedFoxMenuHandler.RegisterControl(ModeSelector,      Plugin.Globals.VstParameters.FindParameter(TParName.Seq2Direction),  TStepSequencerDirectionHelper);
    GuiStandard.RedFoxMenuHandler.RegisterControl(StepCountSelector, Plugin.Globals.VstParameters.FindParameter(TParName.Seq2Length),     TStepSequencerLengthHelper);
  end;

  StepSeqControl.SequenceData := Plugin.ActiveKeyGroup.GetVectorSequenceData(fSequencerIndex);
end;

procedure TSequencerFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
  {
  if FeedBack^.IsVoiceActive then
  begin
    if StepSeq1.CurrentStep <> FeedBack^.StepSeq1CurStep then StepSeq1.CurrentStep := FeedBack^.StepSeq1CurStep;
    if StepSeq2.CurrentStep <> FeedBack^.StepSeq2CurStep then StepSeq2.CurrentStep := FeedBack^.StepSeq2CurStep;
  end else
  begin
    if StepSeq1.CurrentStep <> -1 then StepSeq1.CurrentStep := -1;
    if StepSeq2.CurrentStep <> -1 then StepSeq2.CurrentStep := -1;
  end;
  }
end;

procedure TSequencerFrame.StepSeqControlShowContextMenu(Sender: TObject; X, Y: Integer);
begin
  StepSequenceMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, fSequencerIndex);
end;

procedure TSequencerFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  if MsgID = TLucidMsgID.RefreshRequest_StepSeqDisplay then
  begin
    StepSeqControl.Invalidate;
  end;

end;




end.
