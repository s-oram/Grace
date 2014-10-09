unit InWindowDialog.CustomDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TCustomDialog = class(TPluginDialog)
  private
    fText: string;
    fColorBorder: TColor;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;

    procedure EventHandle_OkButtonClicked(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    property Text : string read fText write fText;
    property ColorBorder : TColor read fColorBorder write fColorBorder;
  end;

implementation

uses
  InWindowDialog.CustomDialog.Form;

{ TTestDialog }

constructor TCustomDialog.Create;
begin
  inherited;
  Text := '';
  ColorBorder := clBlack;
end;

destructor TCustomDialog.Destroy;
begin

  inherited;
end;

procedure TCustomDialog.EventHandle_OkButtonClicked(Sender: TObject);
begin

end;

function TCustomDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TCustomDialogForm;
begin
  aForm := TCustomDialogForm.Create(AOwner);
  aForm.DialogText := Text;

  result := aForm;
end;



end.
