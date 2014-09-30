unit InWindowDialog.InputDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TDialogResult = reference to procedure(Text : string);

  TInputDialog = class(TPluginDialog)
  private
    fInputLabel: string;
    fDefaultValue: string;
    fInputText: string;
    fDialogResultHandler: TDialogResult;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;

    procedure EventHandle_DialogResult(Sender : TObject; Text : string);
  public
    constructor Create; override;
    destructor Destroy; override;

    property InputText    : string read fInputText    write fInputText;
    property InputLabel   : string read fInputLabel   write fInputLabel;
    property DefaultValue : string read fDefaultValue write fDefaultValue;

    property DialogResultHandler : TDialogResult read fDialogResultHandler write fDialogResultHandler;
  end;

implementation

uses
  InWindowDialog.InputDialog.Form;

{ TInputDialog }

constructor TInputDialog.Create;
begin
  inherited;

end;

destructor TInputDialog.Destroy;
begin

  inherited;
end;


procedure TInputDialog.EventHandle_DialogResult(Sender: TObject; Text : string);
begin
  if assigned(fDialogResultHandler) then fDialogResultHandler(Text);
end;

function TInputDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TInputDialogForm;
begin
  aForm := TInputDialogForm.Create(AOwner);

  aForm.InputText    := self.InputText;
  aForm.InputLabel   := self.InputLabel;
  aForm.DefaultValue := self.DefaultValue;

  aForm.OnDialogResult := EventHandle_DialogResult;

  result := aForm;
end;



end.
