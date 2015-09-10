unit WatchTower;

interface

type
  TWriteToLogMethod = reference to procedure(Msg : string);
  TReportErrorMethod = reference to procedure(const ErrorMsg : string);
  TWatchTowerTestMethod = reference to procedure(const ReportError : TReportErrorMethod);

procedure RegisterTest(const PackageName, GroupName, TestName : string; const TestMethod : TWatchTowerTestMethod);

procedure RunTests(const PackageName, GroupName, TestName : string; const WriteToLog : TWriteToLogMethod; out TestCount, ErrorCount : integer);

implementation

uses
  SysUtils,
  Classes,
  WatchTower.Utils,
  WatchTower.TestRunner,
  WatchTower.TestCollection;

var
  Global_TestCollection : TTestCollection;

function TestCollection : TTestCollection;
begin
  if (Global_TestCollection = nil) then CreateSingletonObject(Pointer(Global_TestCollection), TTestCollection.Create);
  result := Global_TestCollection;
end;

procedure RegisterTest(const PackageName, GroupName, TestName : string; const TestMethod : TWatchTowerTestMethod);
begin
  TestCollection.AddTest(PackageName, GroupName, TestName, TestMethod);
end;

procedure RunTests(const PackageName, GroupName, TestName : string; const WriteToLog : TWriteToLogMethod; out TestCount, ErrorCount : integer);
var
  TestID : string;
  SelectedTests : TList;
begin
  WriteToLog('Running WatchTower tests matching ' + GetTestID(PackageName, GroupName, TestName));
  WriteToLog(IntToStr(TestCollection.Count) + ' tests registered.');

  SelectedTests := TList.Create;
  try
    TestCollection.SelectTests(SelectedTests, PackageName, GroupName, TestName);
    WriteToLog(IntToStr(SelectedTests.Count) + ' tests selected.');
    if SelectedTests.Count > 0 then WatchTower.TestRunner.RunTests(SelectedTests, WriteToLog, TestCount, ErrorCount);
  finally
    SelectedTests.Free;
  end;

  WriteToLog(GetTestingFinishedMessage(TestCount, ErrorCount));
end;

initialization

finalization
  if assigned(Global_TestCollection) then Global_TestCollection.Free;


end.
