unit WatchTower.Global;

interface

uses
  System.SyncObjs,
  WatchTower,
  WatchTower.TestCollection;

procedure RegisterTest(const TestClass : TWatchTowerTestClass);
procedure RunTests(const LogCallback : TWriteToLogMethod);

function GlobalTestCollection:TTestCollection;

implementation

uses
  WatchTower.TestRunner;

var
  __GlobalTestCollection : TObject;

function GlobalTestCollection:TTestCollection;
var
  NewCollection : TObject;
begin
  // Create a singleton object using the pattern suggested by Ian Boyd.
  // http://stackoverflow.com/a/24657705/395461
  if not assigned(__GlobalTestCollection) then
  begin
    //The object doesn't exist yet. Create one.
    NewCollection := TTestCollection.Create;

    //It's possible another thread also created one.
    //Only one of us will be able to set the AObject singleton variable

    if TInterlocked.CompareExchange(__GlobalTestCollection, NewCollection, nil) <> nil then
    begin
      //The other beat us. Destroy our newly created object and use theirs.
      NewCollection.Free;
    end;
  end;
  result := __GlobalTestCollection as TTestCollection;
end;

procedure RegisterTest(const TestClass : TWatchTowerTestClass);
begin
  GlobalTestCollection.AddTest(TestClass);
end;

procedure RunTests(const LogCallback : TWriteToLogMethod);
var
  Runner : TTestRunner;
  c1: Integer;
begin
  Runner := TTestRunner.Create;
  try
    for c1 := 0 to GlobalTestCollection.Count-1 do
    begin
      Runner.AddTest(GlobalTestCollection.GetTest(c1));
    end;
    Runner.RunTests(LogCallback);
  finally
    Runner.Free;
  end;
end;


initialization

finalization
  if assigned(__GlobalTestCollection) then __GlobalTestCollection.Free;

end.
