unit RegisterTests.VamLib;

interface

implementation

uses
  WatchTower.Global,
  Test.VamLib.ArrayUtils,
  Test.VamLib.MultiCastEvents,
  Test.VamLib.Utils,
  Test.VamLib.Win.Links,
  Test.VamLib.FlexValue,
  Test.VamLib.Collection.DoubleLinkedList,
  Test.VamLib.Collection.List,
  Test.Decal;

procedure RegisterTests;
begin
  // TODO:MED This test requires a test data directory.
  // but I need to think about how test data directories are
  // registered.
  //WatchTower.Global.RegisterTest(TVamLib_Win_Links_Test);

  WatchTower.Global.RegisterTest(TFlexValueTest);
  WatchTower.Global.RegisterTest(TFlexContainerTest);
  WatchTower.Global.RegisterTest(TFlexListTest);
  WatchTower.Global.RegisterTest(TVamLibUtilsTest);
  WatchTower.Global.RegisterTest(TMultiCastEventTest);
  WatchTower.Global.RegisterTest(TArrayUtilTest);

  // == Collection ==
  WatchTower.Global.RegisterTest(TDoubleLinkedListTest);
  WatchTower.Global.RegisterTest(TRecListTest);
end;

initialization
  RegisterTests();

end.
