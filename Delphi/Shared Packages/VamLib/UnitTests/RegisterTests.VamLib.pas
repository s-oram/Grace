unit RegisterTests.VamLib;

interface

implementation

uses
  WatchTower.Global,
  Test.VamLib.Win.Links;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TVamLib_Win_Links_Test);
end;


initialization
  RegisterTests();



end.
