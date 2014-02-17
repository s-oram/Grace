unit uSequencerFrame;

interface

uses
  uConstants,
  eePlugin, eeGuiStandard, uDialogDisplayArea, uGuiFeedbackData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, LucidityGui.DropBoxSelector,
  VamLabel, VamDiv, VamVectorSequence;

type
  TSequencerFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SeqBackPanel: TVamPanel;
    InfoDiv: TVamDiv;
    SequencerLabel: TVamLabel;
    StepCountSelector: TDropBoxSelector;
    ClockSelector: TDropBoxSelector;
    ModeSelector: TDropBoxSelector;
    SeqStepControl: TVamVectorSequence;
    procedure SeqBackPanelResize(Sender: TObject);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    fSequencerIndex: integer;
    procedure SetSequencerIndex(const Value: integer);
  protected
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

end;

destructor TSequencerFrame.Destroy;
begin

  inherited;
end;

procedure TSequencerFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard; aDialogDisplayArea: TDialogDisplayArea);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;


  SeqStepControl.Align := alClient;
  SeqStepControl.Color_Background := kColor_LcdDark1;
  SeqStepControl.Color_Border     := kColor_LcdDark1;
  SeqStepControl.Color_Step       := kColor_LcdDark4;
  SeqStepControl.Color_StepActive := kColor_LcdDark5;


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




end;

procedure TSequencerFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

end.
