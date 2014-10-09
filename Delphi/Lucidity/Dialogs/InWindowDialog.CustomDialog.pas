unit InWindowDialog.CustomDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TDialogResult = reference to procedure(Text : string);

  TCustomDialog = class(TPluginDialog)
  private
    fButtons : array of string;
    fText: string;
    fColorBorder: TColor;
    fDialogResultHandler: TDialogResult;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;

    procedure EventHandle_DialogResult(Sender : TObject; Text : string);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddButtons(const Buttons : array of string);

    property Text : string read fText write fText;

    property ColorBorder : TColor read fColorBorder write fColorBorder;

    property DialogResultHandler : TDialogResult read fDialogResultHandler write fDialogResultHandler;
  end;

implementation

uses
  InWindowDialog.CustomDialog.Form;

{ TTestDialog }

constructor TCustomDialog.Create;
begin
  inherited;
  SetLength(fButtons, 0);
  Text := '';
  ColorBorder := clBlack;
end;

destructor TCustomDialog.Destroy;
begin
  SetLength(fButtons, 0);
  inherited;
end;

procedure TCustomDialog.AddButtons(const Buttons: array of string);
var
  c1: Integer;
begin
  SetLength(fButtons, Length(Buttons));
  for c1 := 0 to Length(Buttons)-1 do
  begin
    fButtons[c1] := Buttons[c1];
  end;
end;

function TCustomDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TCustomDialogForm;
begin
  aForm := TCustomDialogForm.Create(AOwner);
  aForm.DialogText := Text;
  aForm.AddButtons(fButtons);
  aForm.OnDialogResult := EventHandle_DialogResult;
  result := aForm;
end;

procedure TCustomDialog.EventHandle_DialogResult(Sender: TObject; Text: string);
begin
  if assigned(fDialogResultHandler) then fDialogResultHandler(Text);

end;







end.
