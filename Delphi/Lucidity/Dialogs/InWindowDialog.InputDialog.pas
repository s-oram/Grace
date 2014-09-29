unit InWindowDialog.InputDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TInputDialog = class(TPluginDialog)
  private
    fInputLabel: string;
    fDefaultValue: string;
    fInputText: string;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;

    procedure EventHandle_OkButtonClicked(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    property InputText    : string read fInputText    write fInputText;
    property InputLabel   : string read fInputLabel   write fInputLabel;
    property DefaultValue : string read fDefaultValue write fDefaultValue;

  end;

implementation

uses
  InWindowDialog.InputBoxDialog.Form;

{ TInputBox }

constructor TInputDialog.Create;
begin
  inherited;

end;

destructor TInputDialog.Destroy;
begin

  inherited;
end;


procedure TInputDialog.EventHandle_OkButtonClicked(Sender: TObject);
begin

end;

function TInputDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TInputDialogForm;
begin
  aForm := TInputDialogForm.Create(AOwner);

  aForm.InputText := self.InputText;
  aForm.InputLabel := self.InputLabel;
  aForm.DefaultValue := self.DefaultValue;

  result := aForm;
end;



end.
