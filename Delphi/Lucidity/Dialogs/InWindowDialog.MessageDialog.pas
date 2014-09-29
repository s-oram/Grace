unit InWindowDialog.MessageDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TMessageDialog = class(TPluginDialog)
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
  InWindowDialog.MessageDialog.Form;

{ TTestDialog }

constructor TMessageDialog.Create;
begin
  inherited;
  Text := '';
  ColorBorder := clBlack;
end;

destructor TMessageDialog.Destroy;
begin

  inherited;
end;

procedure TMessageDialog.EventHandle_OkButtonClicked(Sender: TObject);
begin

end;

function TMessageDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TMessageDialogForm;
begin
  aForm := TMessageDialogForm.Create(AOwner);
  aForm.DialogText := Text;

  result := aForm;
end;



end.
