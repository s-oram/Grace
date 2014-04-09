unit uRegionInfoFrame;

interface

uses
  LucidityInterfaces,
  Lucidity.SampleMap,
  eePlugin, uGuiFeedbackData, eeGuiStandard,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, Vcl.StdCtrls;

type
  TRegionInfoFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    procedure SetGuiStandard(const Value: TGuiStandard);
    procedure SetPlugin(const Value: TeePlugin);
  protected
    Memo1 : TMemo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SampleFocusChanged;

    procedure InitializeFrame;
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin write SetPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard write SetGuiStandard;
  end;

implementation

uses
  Lucidity.Types;

{$R *.dfm}

{ TRegionInfoFrame }

constructor TRegionInfoFrame.Create(AOwner: TComponent);
begin
  inherited;

  Memo1 := TMemo.Create(AOwner);
  Memo1.Parent := Panel;
  Memo1.Align  := TAlign.alClient;
  Memo1.ReadOnly := true;
  Memo1.Invalidate;
  Memo1.Visible := true;
end;

destructor TRegionInfoFrame.Destroy;
begin

  inherited;
end;

procedure TRegionInfoFrame.InitializeFrame;
begin

end;

procedure TRegionInfoFrame.SetGuiStandard(const Value: TGuiStandard);
begin
  fGuiStandard := Value;
end;

procedure TRegionInfoFrame.SetPlugin(const Value: TeePlugin);
begin
  fPlugin := Value;
end;

procedure TRegionInfoFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

procedure TRegionInfoFrame.SampleFocusChanged;
var
  region : IRegion;
  props : PRegionProperties;
  s : string;
begin
  if not assigned(Plugin) then exit;

  region := Plugin.FocusedRegion;

  Memo1.Clear;

  if assigned(Region) then
  begin
    props := Region.GetProperties;

    s:= ExtractFileName(props^.SampleFileName);
    Memo1.Lines.Add(s);

    s := 'Note: ' + IntToStr(props^.LowNote) + ' - ' + IntToStr(props^.HighNote);
    Memo1.Lines.Add(s);

    s := 'Vel: ' + IntToStr(props^.LowVelocity) + ' - ' + IntToStr(props^.HighVelocity);
    Memo1.Lines.Add(s);
  end else
  begin

  end;

  Memo1.Invalidate;

end;



end.
