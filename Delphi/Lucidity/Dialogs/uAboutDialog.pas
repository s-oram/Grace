unit uAboutDialog;

interface

uses
  eePlugin,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamPanel, RedFoxContainer, VamLabel, VamDiv, VamMultiLineTextBox, VamMemo;

type
  TAboutDialogForm = class(TForm)
    RedFoxContainer: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    VamDiv1: TVamDiv;
    VamLabel1: TVamLabel;
    TextBox: TVamMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IAboutDialog = interface
    ['{3B1C6F01-C788-4394-9AE3-C8FEBFA12711}']
    procedure Setup(ParentContainer : TWinControl);
    function AboutText: TStrings;
  end;

  TAboutDialog = class(TInterfacedObject, IAboutDialog)
  private
    Form : TAboutDialogForm;

    procedure LoadAboutText(const Text : TStrings);

    function AboutText: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Setup(ParentContainer : TWinControl);



  end;



implementation

{$R *.dfm}

uses
  uConstants, eeVstExtra;


{ TAboutDialog }

constructor TAboutDialog.Create;
begin

end;

destructor TAboutDialog.Destroy;
begin
  if assigned(Form)
    then Form.Free;

  inherited;
end;

procedure TAboutDialog.LoadAboutText(const Text: TStrings);
begin
  Text.Add('Lucidity Vst Sampler By One Small Clue');
  Text.Add('Version ' + GetBuildInfoAsString);
end;

procedure TAboutDialog.Setup(ParentContainer: TWinControl);
var
  aW, aH, aT, aL : integer;
begin
  Form := TAboutDialogForm.CreateParented(ParentContainer.Handle);

  Form.BackgroundPanel.Align := alNone;
  Form.BackgroundPanel.Visible := false;
  Form.BackgroundPanel.Parent := ParentContainer;

  aW := round(ParentContainer.Width * (2/3));
  aH := round(ParentContainer.Height * (2/3));
  aL := round((ParentContainer.Width  - aW)  * (1/2));
  aT := round((ParentContainer.Height - aH) * (1/3));

  Form.BackgroundPanel.Width := aW;
  Form.BackgroundPanel.Height := aH;
  Form.BackgroundPanel.Left := aL;
  Form.BackgroundPanel.Top  := aT;

  Form.BackgroundPanel.Color := kPanelLight;
  Form.BackgroundPanel.CornerRadius1 := 3;
  Form.BackgroundPanel.CornerRadius2 := 3;
  Form.BackgroundPanel.CornerRadius3 := 3;
  Form.BackgroundPanel.CornerRadius4 := 3;



  Form.TextBox.Text.Clear;
  LoadAboutText(Form.TextBox.Text);



  Form.BackgroundPanel.Visible := true;
end;

function TAboutDialog.AboutText: TStrings;
begin
  result := Form.TextBox.Text;
end;




end.
