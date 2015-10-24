unit RegisterTests.PluginLib;

interface

implementation

uses
  WatchTower.Global,
  Test.Helm.AnyValue,
  Test.AudioPlugin,
  Test.VamVst2.VstEventBuffer,
  Test.AudioPlugin.ProcessController,
  Test.AudioPlugin.Functions;

procedure RegisterTests;
begin
  // Audio Plugin Tests
  WatchTower.Global.RegisterTest(TAudioPluginTest);
  WatchTower.Global.RegisterTest(TAudioPluginFunctions);
  WatchTower.Global.RegisterTest(TVstEventBufferTest);
  WatchTower.Global.RegisterTest(TProcessControllerTest);



  // Helm Tests.
  WatchTower.Global.RegisterTest(TAnyValueTests);

end;

initialization
  RegisterTests();


end.
