unit RegisterTests.Lucidity;

interface



implementation

uses
  WatchTower.Global,
  Test.Lucidity.ProgramFileUtils,
  Test.LucidityBug.SetGuiParameter;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TLucidity_ProgramFileUtils);
  WatchTower.Global.RegisterTest(TLucidityBug_SetGUIPar);
end;

initialization
  RegisterTests();

end.
