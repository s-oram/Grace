unit Test.WatchTower;

interface

implementation

// TODO: Write some more tests testing WatchTower!

uses
  WatchTower,
  WatchTower.Utils;

procedure Test_GetTestID(const ReportError : TReportErrorMethod);
const
  PackageName : string = 'Package';
  GroupName : string = 'Group';
  TestName : string = 'Test';
var
  s : string;
  TestID : string;
begin
  TestID := GetTestID(PackageName, GroupName, TestName);
  s := PackageName + '::' + GroupName + '::' + TestName;
  if s <> TestID then ReportError('TestID function returns incorrect result.');
end;

initialization
  WatchTower.RegisterTest('WatchTower', 'Test.WatchTower', 'Test_GetTestID', Test_GetTestID);

finalization

end.
