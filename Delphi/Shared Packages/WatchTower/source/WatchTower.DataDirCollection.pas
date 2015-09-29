unit WatchTower.DataDirCollection;

interface

uses
  Classes;

type
  PDataDirInfo = ^TDataDirInfo;
  TDataDirInfo = record
    DataDir : string;
  end;

  PDataDirCollection = ^TDataDirCollection;
  TDataDirCollection = class
  private
    function GetCount: integer;
  protected
    fList : TList;
    procedure Clear;
    function IndexOf(const aDataDir : string):integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddDataDir(const aDataDir : string);
    function GetDataDir(Index : integer):string;

    property Count : integer read GetCount;

    property DataDir[Index : integer]:string read GetDataDir;
  end;

implementation

uses
  SysUtils;

{ TCaptain }

constructor TDataDirCollection.Create;
begin
  fList := TList.Create;
end;

destructor TDataDirCollection.Destroy;
begin
  Clear; //IMPORTANT: Call clear before freeing the tests list.
  fList.Free;
  inherited;
end;

procedure TDataDirCollection.Clear;
var
  c1 : integer;
  Info : PDataDirInfo;
begin
  for c1 := fList.Count-1 downto 0 do
  begin
    Info := fList[c1];
    Dispose(Info);
    fList.Delete(c1);
  end;
end;

function TDataDirCollection.GetCount: integer;
begin
  result := fList.Count;
end;

function TDataDirCollection.IndexOf(const aDataDir: string): integer;
var
  c1: Integer;
begin
  for c1 := 0 to GetCount-1 do
  begin
    if DataDir[c1] = aDataDir then exit(c1);
  end;
  // if we make it this far, no match has been found.
  result := -1;
end;

procedure TDataDirCollection.AddDataDir(const aDataDir: string);
var
  Info : PDataDirInfo;
begin
  if IndexOf(aDataDir) = -1 then
  begin
    New(Info);
    Info^.DataDir := aDataDir;
    fList.Add(Info);
  end;
end;

function TDataDirCollection.GetDataDir(Index: integer): string;
var
  Info : PDataDirInfo;
begin
  Info := fList[Index];
  result := Info^.DataDir;
end;



end.
