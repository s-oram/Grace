unit uInfoBarFrame;

interface

uses
  eePlugin, eeGuiStandard,
  LucidityGUI.InfoBarController,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamTextBox;

type
  TInfoBarFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    InfoTextBox: TVamTextBox;
  private
    fInfoBarController: TInfoBarController;

    procedure EventHandle_InfoBarTextChanged(Sender : TObject; Text:string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; GuiContainer:TControl);

    property InfoBarController : TInfoBarController read fInfoBarController;
  end;

implementation

uses
  uConstants, RedFoxColor;

{$R *.dfm}

{ TInfoBarFrame }

constructor TInfoBarFrame.Create(AOwner: TComponent);
begin
  inherited;
  fInfoBarController := TInfoBarController.Create;
  fInfoBarController.OnTextChanged := EventHandle_InfoBarTextChanged;
end;

destructor TInfoBarFrame.Destroy;
begin
  fInfoBarController.Free;
  inherited;
end;

procedure TInfoBarFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard; GuiContainer: TControl);
begin
  BackgroundPanel.Color := kPanelDark;
  BackgroundPanel.Invalidate;

  InfoTextBox.Align := alClient;
  InfoTextBox.AlignWithMargins := true;
  InfoTextBox.Text := '';
  InfoTextBox.TextPadding.SetBounds(4,0,4,0);

  InfoTextBox.Color          := kColor_LcdDark1;
  InfoTextBox.ColorMouseOver := kColor_LcdDark1;
  InfoTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);

  InfoBarController.Initalize(aPlugin, GuiContainer);
end;

procedure TInfoBarFrame.EventHandle_InfoBarTextChanged(Sender: TObject; Text: string);
begin
  InfoTextBox.Text := Text;
end;



end.
