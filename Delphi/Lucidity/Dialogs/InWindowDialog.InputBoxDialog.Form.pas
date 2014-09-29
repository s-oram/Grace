unit InWindowDialog.InputBoxDialog.Form;

interface

uses
  InWindowDialog.Prototypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TInputDialogForm = class(TPluginDialogForm)
  private
    fOnOkayButton: TNotifyEvent;
    fInputLabel: string;
    fDefaultValue: string;
    fInputText: string;
    procedure SetDefaultValue(const Value: string);
    procedure SetInputLabel(const Value: string);
    procedure SetInputText(const Value: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property InputText    : string read fInputText    write SetInputText;
    property InputLabel   : string read fInputLabel   write SetInputLabel;
    property DefaultValue : string read fDefaultValue write SetDefaultValue;

    property OnOkButton : TNotifyEvent read fOnOkayButton write fOnOkayButton;
  end;



implementation

{$R *.dfm}

{ TInputBoxDialogForm }

constructor TInputDialogForm.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TInputDialogForm.Destroy;
begin

  inherited;
end;

procedure TInputDialogForm.SetDefaultValue(const Value: string);
begin
  fDefaultValue := Value;
end;

procedure TInputDialogForm.SetInputLabel(const Value: string);
begin
  fInputLabel := Value;
end;

procedure TInputDialogForm.SetInputText(const Value: string);
begin
  fInputText := Value;
end;

end.
