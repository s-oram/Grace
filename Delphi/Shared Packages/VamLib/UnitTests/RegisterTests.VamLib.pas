unit RegisterTests.VamLib;

interface

implementation

uses
  WatchTower.Global,
  Test.VamLib.Win.Links,
  Test.VamLib.FlexValue,
  Test.Decal;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TVamLib_Win_Links_Test);
  WatchTower.Global.RegisterTest(TFlexValueTest);
  WatchTower.Global.RegisterTest(TFlexContainerTest);
  WatchTower.Global.RegisterTest(TFlexListTest);
end;


initialization
  RegisterTests();



end.
