unit WatchTower.TestRunner;

interface

uses
  Classes,
  WatchTower,
  WatchTower.TestCollection;


procedure RunTests(const SelectedTests : TList; const WriteToLog : TWriteToLogMethod; out TestCount, ErrorCount : integer);

implementation

uses
  SysUtils,
  WatchTower.Utils;



procedure RunTests(const SelectedTests : TList; const WriteToLog : TWriteToLogMethod; out TestCount, ErrorCount : integer);
var
  TestIndex, NumberOfErrors : integer;
  CurrentTest : PTestCase;
  TestID : string;
  ReportError : TReportErrorMethod;
  TestResult : string;
begin
  TestIndex := 0;
  NumberOfErrors := 0;

  ReportError := procedure(const ErrorMsg : string)
  begin
    inc(NumberOfErrors);
    WriteToLog('Error: ' + ErrorMsg);
  end;

  while TestIndex <= SelectedTests.Count-1 do
  begin
    CurrentTest := SelectedTests[TestIndex];
    inc(TestIndex);

    TestID := GetTestID(CurrentTest.PackageName, CurrentTest.GroupName, CurrentTest.TestName);
    WriteToLog(TestID);

    try
      CurrentTest.TestMethod(ReportError);
    except
      on e : exception do
      begin
        inc(NumberOfErrors);
        WriteToLog('Exception Error: ' + e.ClassName + ' ' + e.Message);
      end;
    end;
  end;

  TestCount  := TestIndex;
  ErrorCount := NumberOfErrors;
end;

end.
