unit uInfoBarFrame;

interface

uses
  eePlugin, eeGuiStandardv2,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamTextBox;

type
  TInfoBarFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    InfoTextBox: TVamTextBox;
  private
    Plugin : TeePlugin;

    procedure Handle_InfoMessageChanged(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; GuiContainer:TControl);

  end;

implementation

uses
  uConstants, RedFoxColor;

{$R *.dfm}

{ TInfoBarFrame }

constructor TInfoBarFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TInfoBarFrame.Destroy;
begin

  inherited;
end;

procedure TInfoBarFrame.Handle_InfoMessageChanged(Sender: TObject);
begin
  InfoTextBox.Text := Plugin.Globals.InfoBarReceiver.MessageText;
end;

procedure TInfoBarFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard; GuiContainer: TControl);
begin
  Plugin := aPlugin;

  BackgroundPanel.Color := kPanelDark;
  BackgroundPanel.Invalidate;

  InfoTextBox.Align := alClient;
  InfoTextBox.AlignWithMargins := true;
  InfoTextBox.Text := '';
  InfoTextBox.TextPadding.SetBounds(4,0,4,0);

  InfoTextBox.Color          := kColor_LcdDark1;
  InfoTextBox.ColorMouseOver := kColor_LcdDark1;
  InfoTextBox.Font.Color     := GetRedFoxColor(kColor_LcdDark5);

  Plugin.Globals.InfoBarReceiver.OnMessageChanged := Handle_InfoMessageChanged;
end;





end.
