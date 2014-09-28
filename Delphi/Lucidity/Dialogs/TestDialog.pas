unit TestDialog;

interface

uses
  InWindowDialog, InWindowDialog.Prototypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TTestDialog = class(TluginDialog)
  private
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TTestDialogForm = class(TPluginDialogForm)
    Panel1: TPanel;
    CancelButton: TButton;
    OkButton: TButton;
    procedure OkButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

uses
  VamLib.LoggingProxy;

{$R *.dfm}

{ TTestDialog }

constructor TTestDialog.Create;
begin
  inherited;

end;

destructor TTestDialog.Destroy;
begin

  inherited;
end;

function TTestDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TTestDialogForm;
begin
  aForm := TTestDialogForm.Create(AOwner);

  // TODO: populate form data here.

  //=====================================

  result := aForm;
end;



{ TTestDialogForm }

constructor TTestDialogForm.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TTestDialogForm.Destroy;
begin
  Log.LogMessage('TestDialogForm.Destroy');
  inherited;
end;

procedure TTestDialogForm.OkButtonClick(Sender: TObject);
begin
  CloseDialog;
end;




end.
