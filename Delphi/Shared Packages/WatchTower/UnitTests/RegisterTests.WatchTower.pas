unit RegisterTests.WatchTower;

interface

implementation

uses
  WatchTower.Global,
  Test.WatchTower.DataDirCollection;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TDataDirCollectionTest);
end;

initialization
  RegisterTests();

end.
