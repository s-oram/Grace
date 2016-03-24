unit RegisterTests.PluginLib;

interface

implementation

uses
  WatchTower.Global,
  Test.Helm.AnyValue,
  Test.Helm.Message,
  Test.Helm.Dispatcher,
  Test.AudioPlugin,
  Test.VamVst2.VstEventBuffer,
  Test.AudioPlugin.ProcessController,
  Test.AudioPlugin.Functions,
  Test.VamVst2.MidiEventOutputBuffer,
  Test.PlugLib.AirControl,
  Test.PlugLib.AirControl.TaskQueue;

procedure RegisterTests;
begin
  // Audio Plugin Tests
  WatchTower.Global.RegisterTest(TAudioPluginTest);
  WatchTower.Global.RegisterTest(TAudioPluginFunctions);
  WatchTower.Global.RegisterTest(TVstEventBufferTest);
  WatchTower.Global.RegisterTest(TProcessControllerTest);
  WatchTower.Global.RegisterTest(TMidiEventOutputBufferTest);


  // Helm Tests.
  WatchTower.Global.RegisterTest(TAnyValueTests);
  WatchTower.Global.RegisterTest(THelmMessageTests);
  WatchTower.Global.RegisterTest(TEventDispatcherTests);

  // AirControl Tests
  WatchTower.Global.RegisterTest(TAirControlTest);
  WatchTower.Global.RegisterTest(TTaskQueueTest);


end;

initialization
  RegisterTests();


end.
