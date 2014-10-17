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
    fMissingFiles : TStringList;
    fSearchPaths : TStringList;
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddMissingFiles(const FullPathFilenames : TStringList);
    procedure AddSearchPaths(const SearchPaths : TStringList);
  end;

implementation

uses
  InWindowDialog.SampleFinderDialog.Form;

{ TSampleFinderDialog }

constructor TSampleFinderDialog.Create;
begin
  inherited;
  fMissingFiles := TStringList.Create;
  fSearchPaths := TStringList.Create;
end;

destructor TSampleFinderDialog.Destroy;
begin
  fMissingFiles.Free;
  fSearchPaths.Free;
  inherited;
end;

procedure TSampleFinderDialog.AddMissingFiles(const FullPathFilenames: TStringList);
begin
  fMissingFiles.AddStrings(FullPathFilenames);
end;

procedure TSampleFinderDialog.AddSearchPaths(const SearchPaths: TStringList);
begin
  fSearchPaths.AddStrings(SearchPaths);
end;

function TSampleFinderDialog.CreateDialogForm(AOwner: TComponent): TPluginDialogForm;
var
  aForm : TSampleFinderDialogForm;
begin
  aForm := TSampleFinderDialogForm.Create(AOwner, fMissingFiles, fSearchPaths);
  //aForm.DialogText := Text;

  result := aForm;
end;

end.
