unit InWindowDialog.SampleFinderDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TSampleFinderDialog = class(TPluginDialog)
  private
    fText: string;
    fColorBorder: TColor;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  InWindowDialog.SampleFinderDialog.Form;

{ TSampleFinderDialog }

constructor TSampleFinderDialog.Create;
begin
  inherited;

end;

destructor TSampleFinderDialog.Destroy;
begin

  inherited;
end;

function TSampleFinderDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TSampleFinderDialogForm;
begin
  aForm := TSampleFinderDialogForm.Create(AOwner);
  //aForm.DialogText := Text;

  result := aForm;
end;

end.
