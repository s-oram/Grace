unit uSequencerFrame;

interface

uses
  uConstants,
  eePlugin, eeGuiStandard, uDialogDisplayArea, uGuiFeedbackData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer;

type
  TSequencerFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SeqBackPanel: TVamPanel;
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
  protected
    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);


  end;

implementation

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


  SeqBackPanel.Color := kColor_LcdDark1;
end;

procedure TSequencerFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

end.
