unit InWindowDialog.SampleFinderDialog.Brain;

{
  The sample finder brain wraps up all the searching functionality.
}

interface

uses
  Classes;

type
  TFileFoundEvent = procedure(Sender : TObject; const MissingIndex : integer; const OldFileName, NewFileName : string) of object;

  TSampleFinderBrain = class
  private
    fOnUpdateMainView: TNotifyEvent;
    fOnSearchFinished: TNotifyEvent;
    fOnFileFound: TFileFoundEvent;
    function GetCurrentMissingFileFullPath: string;
    function GetCurrentMissingFileName: string;
    function GetCurrentMissingFileCount: integer;
  protected
    PreviousFindLocations : TStringList;

    fMissingFiles : TStringList;
    fSearchPaths  : TStringList;

    MissingIndex : integer;

    procedure IncrementMissingIndex;

    procedure UpdateMainView;
    procedure SearchFinished;
    procedure FileFound(const MissingIndex : integer; const OldFileName, NewFileName : string);
  public
    constructor Create(var MissingFiles, SearchPaths : TStringList);
    destructor Destroy; override;

    procedure Skip;
    procedure LocateFile;

    property CurrentMissingFileCount    : integer read GetCurrentMissingFileCount;
    property CurrentMissingFileName     : string read GetCurrentMissingFileName;
    property CurrentMissingFileFullPath : string read GetCurrentMissingFileFullPath;

    property OnUpdateMainView : TNotifyEvent read fOnUpdateMainView write fOnUpdateMainView;
    property OnSearchFinished : TNotifyEvent read fOnSearchFinished write fOnSearchFinished;
    property OnFileFound      : TFileFoundEvent read fOnFileFound write fOnFileFound;
  end;

implementation

uses
  Dialogs,
  XPlat.Dialogs,
  SysUtils;

{ TSampleFinderBrain }

constructor TSampleFinderBrain.Create(var MissingFiles, SearchPaths: TStringList);
begin
  fMissingFiles := MissingFiles;
  fSearchPaths  := SearchPaths;

  MissingIndex := 0;

  PreviousFindLocations := TStringList.Create;
end;

destructor TSampleFinderBrain.Destroy;
begin
  PreviousFindLocations.Free;
  inherited;
end;

procedure TSampleFinderBrain.FileFound(const MissingIndex: integer; const OldFileName, NewFileName: string);
var
  Dir : string;
begin
  Dir := ExtractFilePath(NewFileName);
  if PreviousFindLocations.IndexOf(Dir) = -1 then
  begin
    PreviousFindLocations.Add(Dir);
  end;
  if assigned(OnFileFound) then OnFileFound(self, MissingIndex, OldFileName, NewFileName);
end;

function TSampleFinderBrain.GetCurrentMissingFileCount: integer;
begin
  result := fMissingFiles.Count - MissingIndex;
end;

function TSampleFinderBrain.GetCurrentMissingFileFullPath: string;
begin
  if MissingIndex < fMissingFiles.Count
    then result := fMissingFiles[MissingIndex]
    else result := '';
end;

function TSampleFinderBrain.GetCurrentMissingFileName: string;
begin
  if MissingIndex < fMissingFiles.Count
    then result := ExtractFileName(fMissingFiles[MissingIndex])
    else result := '';
end;

procedure TSampleFinderBrain.SearchFinished;
begin
  if assigned(OnSearchFinished) then OnSearchFinished(Self);

end;

procedure TSampleFinderBrain.IncrementMissingIndex;
begin
  if MissingIndex < fMissingFiles.Count-1 then
  begin
    inc(MissingIndex);
    UpdateMainView;
  end else
  begin
    SearchFinished;
  end;
end;

procedure TSampleFinderBrain.Skip;
begin
  IncrementMissingIndex;
end;

procedure TSampleFinderBrain.UpdateMainView;
begin
  if assigned(OnUpdateMainView) then OnUpdateMainView(self);
end;

procedure TSampleFinderBrain.LocateFile;
var
  OpenDialog : TxpFileOpenDialog;
  fn : string;
  Dir : string;
begin
  fn  := ExtractFileName(CurrentMissingFileFullPath);
  Dir := ExtractFilePath(CurrentMissingFileFullPath);

  OpenDialog := TxpFileOpenDialog.Create(nil);
  try
    OpenDialog.Title := 'Open File';
    if DirectoryExists(Dir) then OpenDialog.InitialDir := Dir;
    OpenDialog.FileName := fn;

    if OpenDialog.Execute then
    begin
      FileFound(MissingIndex, CurrentMissingFileFullPath, OpenDialog.FileName);
      IncrementMissingIndex;
    end;
  finally
    OpenDialog.Free;
  end;
end;



end.
