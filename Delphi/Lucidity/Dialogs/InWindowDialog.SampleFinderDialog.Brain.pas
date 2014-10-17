unit InWindowDialog.SampleFinderDialog.Brain;

{
  The sample finder brain wraps up all the searching functionality.
}

interface

uses
  ASyncCalls,
  Classes;

type
  TFileSearchToken = class;

  TFileFoundEvent = procedure(Sender : TObject; const MissingIndex : integer; const OldFileName, NewFileName : string) of object;
  TStringEvent = procedure(Sender : TObject; Str:string) of object;

  TSampleFinderBrain = class
  private
    fOnUpdateMainView: TNotifyEvent;
    fOnFinished: TNotifyEvent;
    fOnFileFound: TFileFoundEvent;
    fOnSearchPathChanged: TStringEvent;
    function GetCurrentMissingFileFullPath: string;
    function GetCurrentMissingFileName: string;
    function GetCurrentMissingFileCount: integer;
  protected
    CallRef : IAsyncCall;
    SearchToken : TFileSearchToken;

    PreviousFindLocations : TStringList;

    fMissingFiles : TStringList;
    fSearchPaths  : TStringList;

    MissingIndex : integer;

    procedure SearchForMissingFileIn(const MissingFileName, SearchPath : string);
    procedure IncrementMissingIndex;

    procedure UpdateMainView;
    procedure Finished; //Call to signal that all missing files have been processed.
    procedure FileFound(const NewFileName : string);
    procedure SearchPathChanged(NewPath : string);

  public
    constructor Create(var MissingFiles, SearchPaths : TStringList);
    destructor Destroy; override;

    procedure Skip;
    procedure LocateFile;
    procedure SearchIn;

    property CurrentMissingFileCount    : integer read GetCurrentMissingFileCount;
    property CurrentMissingFileName     : string read GetCurrentMissingFileName;
    property CurrentMissingFileFullPath : string read GetCurrentMissingFileFullPath;

    property OnSearchPathChanged : TStringEvent read fOnSearchPathChanged write fOnSearchPathChanged;
    property OnUpdateMainView    : TNotifyEvent read fOnUpdateMainView write fOnUpdateMainView;
    property OnFinished          : TNotifyEvent read fOnFinished write fOnFinished;
    property OnFileFound         : TFileFoundEvent read fOnFileFound write fOnFileFound;
  end;

  //=====================================================================================
  //================== Below here is for private internal usage =========================
  //=====================================================================================
  TFileSearchToken = class(TObject)
  private
  protected
  public
    Brain          : TSampleFinderBrain;
    TargetFileName : string;
    SearchPath     : string;
    CancelCurrentSearch : boolean;
  end;

  procedure SingleFileSearch(Token : TFileSearchToken); cdecl;

implementation

uses
  VamLib.FindFiles,
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

  SearchToken := TFileSearchToken.Create;
  SearchToken.Brain := self;
end;

destructor TSampleFinderBrain.Destroy;
begin
  SearchToken.CancelCurrentSearch := true;
  CallRef := nil;

  PreviousFindLocations.Free;
  SearchToken.Free;

  inherited;
end;

procedure TSampleFinderBrain.FileFound(const NewFileName: string);
var
  Dir : string;
  OldFileName : string;
begin
  OldFileName := CurrentMissingFileFullPath;
  Dir := ExtractFilePath(NewFileName);
  if PreviousFindLocations.IndexOf(Dir) = -1 then
  begin
    PreviousFindLocations.Add(Dir);
  end;
  if assigned(OnFileFound) then OnFileFound(self, MissingIndex, OldFileName, NewFileName);
  IncrementMissingIndex;
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

procedure TSampleFinderBrain.Finished;
begin
  if assigned(OnFinished) then OnFinished(Self);
end;

procedure TSampleFinderBrain.IncrementMissingIndex;
  function QuickSearchForCurrentFileInPreviousLocations:boolean;
  var
    Dir : string;
    fn : string;
    c1 : integer;
    FullPathResult : string;
  begin
    fn := ExtractFileName(CurrentMissingFileName);
    for c1 := 0 to PreviousFindLocations.Count-1 do
    begin
      Dir := PreviousFindLocations[c1];
      if SearchForFile(Dir, fn, false, FullPathResult, nil, nil) then
      begin
        FileFound(FullPathResult);
        exit(true); //====================>>exit>>===============>>
      end;
    end;
    //if we make it this far, no file found...
    result := false;
  end;
begin
  inc(MissingIndex);

  if MissingIndex < fMissingFiles.Count then
  begin
    if QuickSearchForCurrentFileInPreviousLocations = false
      then UpdateMainView;
  end else
  begin
    Finished;
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
  SearchToken.CancelCurrentSearch := true;

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
    if OpenDialog.Execute then FileFound(OpenDialog.FileName);
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


procedure TSampleFinderBrain.SearchPathChanged(NewPath: string);
begin
  if assigned(OnSearchPathChanged) then OnSearchPathChanged(self, NewPath);
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

  CallRef := AsyncCall(@SingleFileSearch, [SearchToken]);
end;


//================================================================================================
//================================================================================================
//================================================================================================

procedure SingleFileSearch(Token : TFileSearchToken); cdecl;
var
  FullPathResult : string;
  SearchPathChangedCallback : TStringProcReference;
  CancelSearchCallback : TBooleanFuncReference;
  spcAdapter : TProc;
begin
  Token.CancelCurrentSearch := false;

  SearchPathChangedCallback := procedure(NewPath : string)
  begin
    EnterMainThread;
    try
      Token.Brain.SearchPathChanged(NewPath);
    finally
      LeaveMainThread;
    end;
  end;

  CancelSearchCallback := function : boolean
  begin
    result := Token.CancelCurrentSearch;
  end;

  if SearchForFile(Token.SearchPath, Token.TargetFileName, true, FullPathResult, SearchPathChangedCallback, CancelSearchCallback) then
  begin
    EnterMainThread;
    try
      Token.Brain.FileFound(FullPathResult);
    finally
      LeaveMainThread;
    end;
  end else
  begin
    ShowMessage('file not found');
  end;
end;




end.
