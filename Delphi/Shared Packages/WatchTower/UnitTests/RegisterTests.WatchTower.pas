unit RegisterTests.WatchTower;

interface

implementation

uses
  WatchTower.Global,
  Test.WatchTower.DataDirCollection;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TDataDirCollection_BasicTest);
end;

initialization
  RegisterTests();

end.
