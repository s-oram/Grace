unit RegisterTests.Lucidity;

interface



implementation

uses
  WatchTower.Global,
  Test.Lucidity.ProgramFileUtils;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TLucidity_ProgramFileUtils);
end;

initialization
  RegisterTests();

end.
