unit uXYPadsFrame;

interface

uses
  eeGuiStandard, eePlugin, uGuiFeedbackData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VamXYPad,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxContainer,
  RedFoxGraphicControl, VamGraphicControl, VamLabel, VamDiv;

type
  TXYPadsFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    VamLabel1: TVamLabel;
    VamDiv1: TVamDiv;
    CpuText: TVamLabel;
    VoiceCountText: TVamLabel;
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    procedure SetGuiStandard(const Value: TGuiStandard);
    procedure SetPlugin(const Value: TeePlugin);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin write SetPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard write SetGuiStandard;
  end;

implementation

uses
  eeGuiSetup;

{$R *.dfm}

constructor TXYPadsFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TXYPadsFrame.Destroy;
begin

  inherited;
end;

procedure TXYPadsFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
begin
  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;
end;

procedure TXYPadsFrame.SetPlugin(const Value: TeePlugin);
begin
  fPlugin := Value;


  if fPlugin <> nil then
  begin

  end;
end;

procedure TXYPadsFrame.SetGuiStandard(const Value: TGuiStandard);
begin
  assert(assigned(Plugin));

  fGuiStandard := Value;

  if Value <> nil then
  begin
    
  end;


end;

procedure TXYPadsFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
var
  s : string;
begin
  s := 'CPU: ' + FloatToStr(Plugin.Globals.CpuUsage^.ProcessReplacingLoad) + '%';
  if CpuText.Text <> s then CpuText.Text := s;

  s := 'Voices: ' + IntToStr(Feedback^.ActiveVoiceCount);
  if VoiceCountText.Text <> s then VoiceCountText.Text := s;
end;

end.
