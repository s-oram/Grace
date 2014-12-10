unit InWindowDialog.SampleFinderDialog;

interface

uses
  Classes, Graphics,
  InWindowDialog.Prototypes;

type
  TFileFoundCallback = reference to procedure(const MissingIndex : integer; const OldFileName, NewFileName : string; var Accept : boolean; var AcceptMessage : string);

  TSampleFinderDialog = class(TPluginDialog)
  private
    fFileFoundCallback: TFileFoundCallback;
  protected
    fMissingFiles : TStringList;
    fSearchPaths : TStringList;
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; override;

    procedure EventHandle_FileFound(Sender : TObject; const MissingIndex : integer; const OldFileName, NewFileName : string; var Accept : boolean; var AcceptMessage : string);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddMissingFiles(const FullPathFilenames : TStringList);
    procedure AddSearchPaths(const SearchPaths : TStringList);

    property FileFoundCallback : TFileFoundCallback read fFileFoundCallback write fFileFoundCallback;
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
  aForm.OnFileFound := self.EventHandle_FileFound;
  result := aForm;
end;

procedure TSampleFinderDialog.EventHandle_FileFound(Sender: TObject; const MissingIndex: integer; const OldFileName, NewFileName: string; var Accept: boolean; var AcceptMessage: string);
begin
  if assigned(FileFoundCallback) then FileFoundCallback(MissingIndex, OldFileName, NewFileName, Accept, AcceptMessage);
end;



end.
