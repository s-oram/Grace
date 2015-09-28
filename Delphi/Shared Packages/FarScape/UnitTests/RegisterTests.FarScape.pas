unit RegisterTests.FarScape;

interface

implementation

uses
  WatchTower.Global,
  Test.FarScape.Parentage,
  Test.FarScape.Positioning,
  Test.FarScape.Scene,
  Test.FarScape.RootControl,
  Test.FarScape.ControlHelper,
  Test.FarScape.ControlAlignment;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TControlParentTest);
  WatchTower.Global.RegisterTest(TPositioningTest);
  WatchTower.Global.RegisterTest(TFarScapeSceneTest);
  WatchTower.Global.RegisterTest(TRootControlTest);
  WatchTower.Global.RegisterTest(TControlHelperTest);
  WatchTower.Global.RegisterTest(TControlAlignmentTest);
end;

initialization
  RegisterTests();

end.
