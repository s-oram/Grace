unit RegisterTests.PluginLib;

interface

implementation

uses
  WatchTower.Global,
  Test.AudioPlugin,
  Test.AudioPlugin.ProcessController;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TAudioPluginTest);
  WatchTower.Global.RegisterTest(TProcessControllerTest);
end;

initialization
  RegisterTests();


end.
