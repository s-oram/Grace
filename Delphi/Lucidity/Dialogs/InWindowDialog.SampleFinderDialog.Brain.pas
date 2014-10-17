unit InWindowDialog.SampleFinderDialog.Brain;

{
  The sample finder brain wraps up all the searching functionality.
}

interface

uses
  Classes;

type
  TSampleFinderBrain = class
  private
    fOnUpdateMainView: TNotifyEvent;
    fOnSearchFinished: TNotifyEvent;
    function GetCurrentMissingFileFullPath: string;
    function GetCurrentMissingFileName: string;
    function GetCurrentMissingFileCount: integer;
  protected
    fMissingFiles : TStringList;
    fSearchPaths  : TStringList;

    MissingIndex : integer;

    procedure UpdateMainView;
    procedure SearchFinished;
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
  end;

implementation

uses
  SysUtils;

{ TSampleFinderBrain }

constructor TSampleFinderBrain.Create(var MissingFiles, SearchPaths: TStringList);
begin
  fMissingFiles := MissingFiles;
  fSearchPaths  := SearchPaths;

  MissingIndex := 0;
end;

destructor TSampleFinderBrain.Destroy;
begin

  inherited;
end;

function TSampleFinderBrain.GetCurrentMissingFileCount: integer;
begin
  result := fMissingFiles.Count - MissingIndex;
end;

function TSampleFinderBrain.GetCurrentMissingFileFullPath: string;
begin
  if MissingIndex < fMissingFiles.Count
    then result := ExtractFilePath(fMissingFiles[MissingIndex])
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

procedure TSampleFinderBrain.Skip;
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

procedure TSampleFinderBrain.UpdateMainView;
begin
  if assigned(OnUpdateMainView) then OnUpdateMainView(self);
end;

procedure TSampleFinderBrain.LocateFile;
var
  fn : string;
begin
  fn := CurrentMissingFileFullPath;
end;



end.
