unit RegisterTests.Lucidity;

interface



implementation

uses
  WatchTower.Global,
  Test.Lucidity.ProgramFileUtils,
  Test.LucidityBug.SetGuiParameter,
  Test.Lucidity.SfzOpcodeConversion;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TLucidity_ProgramFileUtils);
  WatchTower.Global.RegisterTest(TLucidityBug_SetGUIPar);
  WatchTower.Global.RegisterTest(TSFZOpcodeConversion);
end;

initialization
  RegisterTests();

end.
