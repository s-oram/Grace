unit WatchTower.TestCollection;

interface

uses
  WatchTower,
  Classes;

type
  PTestCase = ^TTestCase;
  TTestCase = record
    PackageName : string;
    GroupName   : string;
    TestName    : string;
    TestMethod  : TWatchTowerTestMethod;
  end;

  PTestCollection = ^TTestCollection;
  TTestCollection = class
  private
    function GetCount: integer;
  protected
    fTests : TList;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTest(const PackageName, GroupName, TestName : string; const TestMethod : TWatchTowerTestMethod);
    procedure SelectTests(var Dest: TList; const PackageName, GroupName, TestName : string);

    property Count : integer read GetCount;
  end;

implementation

uses
  SysUtils;

{ TCaptain }

constructor TTestCollection.Create;
begin
  fTests := TList.Create;
end;

destructor TTestCollection.Destroy;
begin
  Clear; //IMPORTANT: Call clear before freeing the tests list.
  fTests.Free;
  inherited;
end;

procedure TTestCollection.Clear;
var
  c1 : integer;
  aTest : PTestCase;
begin
  for c1 := fTests.Count-1 downto 0 do
  begin
    aTest := fTests[c1];
    Dispose(aTest);
    fTests.Delete(c1);
  end;
end;

function TTestCollection.GetCount: integer;
begin
  result := fTests.Count;
end;

procedure TTestCollection.AddTest(const PackageName, GroupName, TestName: string; const TestMethod: TWatchTowerTestMethod);
var
  aTest : PTestCase;
begin
  assert(PackageName <> '');
  assert(GroupName <> '');
  assert(TestName <> '');
  assert(assigned(TestMethod));

  New(aTest);
  aTest.PackageName := PackageName;
  aTest.GroupName := GroupName;
  aTest.TestName := TestName;
  aTest.TestMethod := TestMethod;

  fTests.Add(aTest);
end;

procedure TTestCollection.SelectTests(var Dest: TList; const PackageName, GroupName, TestName: string);
var
  c1: Integer;
  aTest : PTestCase;
  MatchPackage, MatchGroup, MatchTestName : boolean;
begin
  assert(assigned(Dest));
  for c1 := 0 to fTests.Count-1 do
  begin
    aTest := fTests[c1];

    if (PackageName = '*') or (PackageName = aTest.PackageName)
      then MatchPackage := true
      else MatchPackage := false;

    if (GroupName = '*') or (GroupName = aTest.GroupName)
      then MatchGroup := true
      else MatchGroup := false;

    if (TestName = '*') or (TestName = aTest.TestName)
      then MatchTestName := true
      else MatchTestName := false;

    if (MatchPackage) and (MatchGroup) and (MatchTestName)
      then Dest.Add(aTest);
  end;
end;





end.
