unit InWindowDialog.SampleFinderDialog.Brain;

{
  The sample finder brain wraps up all the searching functionality.
}

interface

uses
  VamLib.Threads,
  Classes;

type
  TFileSearchMotile = class;

  TFileFoundEvent = procedure(Sender : TObject; const MissingIndex : integer; const OldFileName, NewFileName : string; var Accept : boolean) of object;
  TStringEvent = procedure(Sender : TObject; Str:string) of object;


  TSampleFinderBrain = class
  private
    fOnUpdateMainView: TNotifyEvent;
    fOnFinished: TNotifyEvent;
    fOnFileFound: TFileFoundEvent;
    fOnSearchPathChanged: TStringEvent;
    fOnSearchFinished_FileNotFound: TNotifyEvent;
    function GetCurrentMissingFileFullPath: string;
    function GetCurrentMissingFileName: string;
    function GetCurrentMissingFileCount: integer;
  protected
    FOwner : TComponent;
    FileSearchMotile : TFileSearchMotile;

    PreviousFindLocations : TStringList;

    fMissingFiles : TStringList;
    fSearchPaths  : TStringList;

    MissingIndex : integer;

    procedure SearchForMissingFileIn(const MissingFileName, SearchPath : string);
    procedure IncrementMissingIndex;

    procedure UpdateMainView;
    procedure TriggerEvent_Finished; //Call to signal that all missing files have been processed.
    procedure TriggerEvent_FileFound(const NewFileName : string);
    procedure TriggerEvent_SearchPathChanged(NewPath : string);
    procedure TriggerEvent_SearchFinished_FileNotFound;
  public
    constructor Create(AOwner : TComponent; var MissingFiles, SearchPaths : TStringList);
    destructor Destroy; override;

    procedure Skip;
    procedure LocateFile;
    procedure SearchIn;

    property CurrentMissingFileCount    : integer read GetCurrentMissingFileCount;
    property CurrentMissingFileName     : string read GetCurrentMissingFileName;
    property CurrentMissingFileFullPath : string read GetCurrentMissingFileFullPath;

    property OnSearchFinished_FileNotFound : TNotifyEvent read fOnSearchFinished_FileNotFound write fOnSearchFinished_FileNotFound;
    property OnSearchPathChanged : TStringEvent read fOnSearchPathChanged write fOnSearchPathChanged;
    property OnUpdateMainView    : TNotifyEvent read fOnUpdateMainView write fOnUpdateMainView;
    property OnFinished          : TNotifyEvent read fOnFinished write fOnFinished;
    property OnFileFound         : TFileFoundEvent read fOnFileFound write fOnFileFound;
  end;

  //=====================================================================================
  //================== Below here is for private internal usage =========================
  //=====================================================================================
  TFileSearchMotile = class(TCustomMotile)
  private
  protected
    procedure Task; override;
    procedure TaskFinished; override;
  public
    Brain          : TSampleFinderBrain;
    TargetFileName : string;
    SearchPath     : string;
  end;



implementation

uses
  SysUtils,
  Dialogs,
  VamLib.FindFiles,
  XPlat.Dialogs,
  Lucidity.Utils;


{ TSampleFinderBrain }

constructor TSampleFinderBrain.Create(AOwner : TComponent; var MissingFiles, SearchPaths: TStringList);
begin
  FOwner := AOwner;
  fMissingFiles := MissingFiles;
  fSearchPaths  := SearchPaths;

  MissingIndex := 0;

  PreviousFindLocations := TStringList.Create;

  FileSearchMotile := TFileSearchMotile.Create;
  FileSearchMotile.Brain := self;
end;

destructor TSampleFinderBrain.Destroy;
begin
  FileSearchMotile.Free;
  PreviousFindLocations.Free;
  inherited;
end;

procedure TSampleFinderBrain.TriggerEvent_FileFound(const NewFileName: string);
var
  Dir : string;
  OldFileName : string;
  Accept : boolean;
begin
  OldFileName := CurrentMissingFileFullPath;
  Dir := ExtractFilePath(NewFileName);
  if PreviousFindLocations.IndexOf(Dir) = -1 then
  begin
    PreviousFindLocations.Add(Dir);
  end;

  Accept := true;
  if assigned(OnFileFound) then OnFileFound(self, MissingIndex, OldFileName, NewFileName, Accept);

  if Accept
    then IncrementMissingIndex;
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

procedure TSampleFinderBrain.TriggerEvent_Finished;
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
        TriggerEvent_FileFound(FullPathResult);
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
    TriggerEvent_Finished;
  end;
end;

procedure TSampleFinderBrain.Skip;
begin
  FileSearchMotile.Cancel;
  IncrementMissingIndex;
end;

procedure TSampleFinderBrain.TriggerEvent_SearchFinished_FileNotFound;
begin
  if assigned(OnSearchFinished_FileNotFound) then OnSearchFinished_FileNotFound(self);
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
  FileSearchMotile.Cancel;

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


  OpenDialog := TxpFileOpenDialog.Create(FOwner);
  try
    OpenDialog.Title := 'Open File';
    if DirectoryExists(Dir) then OpenDialog.InitialDir := Dir;
    OpenDialog.FileName := fn;
    if OpenDialog.Execute(GetComponentHandle(FOwner)) then TriggerEvent_FileFound(OpenDialog.FileName);
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
  FileSearchMotile.Cancel;

  fn  := ExtractFileName(CurrentMissingFileFullPath);
  Dir := ExtractFilePath(CurrentMissingFileFullPath);

  DirSelectDialog := TxpDirectorySelectDialog.Create(FOwner);
  try
    if DirSelectDialog.Execute(GetComponentHandle(FOwner)) then
    begin
      SearchForMissingFileIn(fn, DirSelectDialog.FileName);
    end;
  finally
    DirSelectDialog.Free;
  end;
end;


procedure TSampleFinderBrain.TriggerEvent_SearchPathChanged(NewPath: string);
begin
  if assigned(OnSearchPathChanged) then OnSearchPathChanged(self, NewPath);
end;

procedure TSampleFinderBrain.SearchForMissingFileIn(const MissingFileName, SearchPath: string);
begin
  FileSearchMotile.Stop;

  FileSearchMotile.TargetFileName      := MissingFileName;
  FileSearchMotile.SearchPath          := SearchPath;

  FileSearchMotile.Run;
end;


//================================================================================================
//================================================================================================
//================================================================================================

{ TFileSearchMotile }

procedure TFileSearchMotile.Task;
var
  FullPathResult : string;
  SearchPathChangedCallback : TStringProcReference;
  CancelSearchCallback : TBooleanFuncReference;
  spcAdapter : TProc;
begin
  SearchPathChangedCallback := procedure(NewPath : string)
  begin
    self.Queue(procedure
    begin
      Self.Brain.TriggerEvent_SearchPathChanged(NewPath);
    end);
  end;

  CancelSearchCallback := function : boolean
  begin
    result := Self.IsCanceled;
  end;

  if SearchForFile(Self.SearchPath, Self.TargetFileName, true, FullPathResult, SearchPathChangedCallback, CancelSearchCallback) then
  begin
    self.Synchronize(procedure
    begin
      self.Brain.TriggerEvent_FileFound(FullPathResult);
    end);
  end else
  begin
    self.Synchronize(procedure
    begin
      Self.Brain.TriggerEvent_SearchFinished_FileNotFound;
    end);
  end;
end;


procedure TFileSearchMotile.TaskFinished;
begin
  inherited;

end;

end.
