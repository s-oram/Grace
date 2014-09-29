unit InWindowsDialog.MessageDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TMessageDialog = class(TPluginDialog)
  private
    fText: string;
    fColorBorder: TColor;
    fColorText: TColor;
    fColorBackground: TColor;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;

    procedure EventHandle_OkButtonClicked(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    property Text : string read fText write fText;
    property ColorBackground : TColor read fColorBackground write fColorBackground;
    property ColorBorder     : TColor read fColorBorder     write fColorBorder;
    property ColorText       : TColor read fColorText       write fColorText;
  end;

implementation

uses
  InWindowsDialog.MessageDialog.Form;

{ TTestDialog }

constructor TMessageDialog.Create;
begin
  inherited;
  Text := '';

  ColorBackground := clGray;
  ColorBorder     := clBlack;
  ColorText       := clBlack;
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
