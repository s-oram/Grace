unit InWindowDialog.SampleFinderDialog.Brain;

{
  The sample finder brain wraps up all the searching functionality.
}

interface

uses
  ASyncCalls,
  Classes;

type
  TAutoFileSearchToken = class;

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
    CallRef : IAsyncCall;
    SearchToken : TAutoFileSearchToken;

    PreviousFindLocations : TStringList;

    fMissingFiles : TStringList;
    fSearchPaths  : TStringList;

    MissingIndex : integer;


    procedure SearchForMissingFileIn(const MissingFileName, SearchPath : string);
    procedure IncrementMissingIndex;

    procedure UpdateMainView;
    procedure SearchFinished;
    procedure FileFound(const MissingIndex : integer; const OldFileName, NewFileName : string);
  public
    constructor Create(var MissingFiles, SearchPaths : TStringList);
    destructor Destroy; override;

    procedure Skip;
    procedure LocateFile;
    procedure SearchIn;

    property CurrentMissingFileCount    : integer read GetCurrentMissingFileCount;
    property CurrentMissingFileName     : string read GetCurrentMissingFileName;
    property CurrentMissingFileFullPath : string read GetCurrentMissingFileFullPath;

    property OnUpdateMainView : TNotifyEvent read fOnUpdateMainView write fOnUpdateMainView;
    property OnSearchFinished : TNotifyEvent read fOnSearchFinished write fOnSearchFinished;
    property OnFileFound      : TFileFoundEvent read fOnFileFound write fOnFileFound;
  end;



  //=====================================================================================
  //================== Below here is for private internal usage =========================
  //=====================================================================================
  TAutoFileSearchToken = class(TObject)
  private
  protected
  public
    TargetFileName : string;
    SearchPath     : string;
    CancelCurrentSearch : boolean;
  end;


  procedure AutoFileSearch(Token : TAutoFileSearchToken); cdecl;

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

  SearchToken := TAutoFileSearchToken.Create;
end;

destructor TSampleFinderBrain.Destroy;
begin
  {
  if assigned(CallRef) then
  begin

    CallRef.CancelInvocation;
    CallRef.Sync;
  end;
  }
  SearchToken.CancelCurrentSearch := true;
  CallRef := nil;

  PreviousFindLocations.Free;
  SearchToken.Free;

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

  if DirectoryExists(Dir) = false then
  begin
    // TODO:MED jump up the file path until
    // a directory is found that exists.

    // TODO:MED even better would be to search for a
    // roughly equal file path. Perhaps this method could be useful.
    //
    // How do you implement Levenshtein distance in Delphi?
    // http://stackoverflow.com/q/54797/395461
  end;


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

procedure TSampleFinderBrain.SearchIn;
var
  fn : string;
  Dir : string;
  DirSelectDialog : TxpDirectorySelectDialog;
begin
  fn  := ExtractFileName(CurrentMissingFileFullPath);
  Dir := ExtractFilePath(CurrentMissingFileFullPath);

  DirSelectDialog := TxpDirectorySelectDialog.Create(nil);
  try
    if DirSelectDialog.Execute then
    begin
      SearchForMissingFileIn(fn, DirSelectDialog.FileName);
    end;
  finally
    DirSelectDialog.Free;
  end;
end;


procedure TSampleFinderBrain.SearchForMissingFileIn(const MissingFileName, SearchPath: string);
begin
  if assigned(CallRef) then
  begin
    SearchToken.CancelCurrentSearch := true;
    CallRef.CancelInvocation;
    CallRef.Sync;
  end;

  SearchToken.TargetFileName := MissingFileName;
  SearchToken.SearchPath     := SearchPath;

  CallRef := AsyncCall(@AutoFileSearch, [SearchToken]);
end;


//================================================================================================
//================================================================================================
//================================================================================================


procedure AutoFileSearch(Token : TAutoFileSearchToken); cdecl;
begin
  Token.CancelCurrentSearch := false;
end;




end.
