unit WatchTower.TestCollection;

interface

uses
  WatchTower,
  Classes;

type
  PTestInfo = ^TTestInfo;
  TTestInfo = record
    TestClass   : TWatchTowerTestClass;
  end;

  PTestCollection = ^TTestCollection;
  TTestCollection = class
  private
    function GetCount: integer;
  protected
    fTestList : TList;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTest(const TestClass : TWatchTowerTestClass);
    function GetTest(Index : integer):TWatchTowerTestClass;

    property Count : integer read GetCount;
  end;

implementation

uses
  SysUtils;

{ TCaptain }

constructor TTestCollection.Create;
begin
  fTestList := TList.Create;
end;

destructor TTestCollection.Destroy;
begin
  Clear; //IMPORTANT: Call clear before freeing the tests list.
  fTestList.Free;
  inherited;
end;

procedure TTestCollection.Clear;
var
  c1 : integer;
  Info : PTestInfo;
begin
  for c1 := fTestList.Count-1 downto 0 do
  begin
    Info := fTestList[c1];
    Dispose(Info);
    fTestList.Delete(c1);
  end;
end;

function TTestCollection.GetCount: integer;
begin
  result := fTestList.Count;
end;

function TTestCollection.GetTest(Index: integer): TWatchTowerTestClass;
var
  TestInfo : PTestInfo;
begin
  TestInfo := fTestList[Index];
  result := TestInfo^.TestClass;
end;

procedure TTestCollection.AddTest(const TestClass : TWatchTowerTestClass);
var
  TestInfo : PTestInfo;
begin
  New(TestInfo);
  TestInfo.TestClass   := TestClass;
  fTestList.Add(TestInfo);
end;


end.
