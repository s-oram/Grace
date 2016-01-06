unit RegisterTests.FarScape;

interface

implementation

uses
  WatchTower.Global,
  Test.FarScape.Parentage,
  Test.FarScape.Positioning,
  Test.FarScape.Scene,
  Test.FarScape.SupportFunctions,
  Test.FarScape.RootControl,
  Test.FarScape.ControlHelper,
  Test.FarScape.ControlAlignment,
  Test.FarScape.Assistant.ControlNamePath,
  Test.FarScape.Events,
  Test.FarScape.Color;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TControlParentTest);
  WatchTower.Global.RegisterTest(TPositioningTest);
  WatchTower.Global.RegisterTest(TFarScapeSceneTest);
  WatchTower.Global.RegisterTest(TRootControlTest);
  WatchTower.Global.RegisterTest(TControlHelperTest);
  WatchTower.Global.RegisterTest(TControlAlignmentTest);
  WatchTower.Global.RegisterTest(TControlNamePathTest);
  WatchTower.Global.RegisterTest(TFarScapeEventsTest);
  WatchTower.Global.RegisterTest(TSupportFunctionsTests);
  WatchTower.Global.RegisterTest(TFarScapeControlEvents);
  WatchTower.Global.RegisterTest(TFarScapeColorTest);



end;

initialization
  RegisterTests();

end.
