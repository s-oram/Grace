unit WatchTower.TestRunner;

interface

uses
  WatchTower,
  WatchTower.TestCollection,
  WatchTower.DataDirCollection;

type
  TTestRunner = class
  private
    TestCollection : TTestCollection;
    DataDirCollection : TDataDirCollection;
    fTestDataDirectory : string;
  public
    constructor Create(const aTestDataDirectory : string);
    destructor Destroy; override;

    procedure AddDataDir(const aDataDir : string);

    procedure AddTest(const aTestClass : TWatchTowerTestClass);
    procedure RunTests(const LogCallback : TWriteToLogMethod);
  end;

implementation

uses
  WatchTower.Confirm,
  SysUtils,
  Rtti;

type
  TTestInfo = record
    TestClassName : string;
    TestMethodName : string;
  end;

procedure PerformTest(const aTestClass : TWatchTowerTestClass; const TestReporter : TWatchTowerTestCallbacks; const MethodName : string; const aTestDataDirectory:string);
var
  c : TRttiContext;
  t : TRttiType;
  m : TRttiMethod;
  ActiveTest : TWatchTowerTest;
begin
  c := TRttiContext.Create;
  try
    t := c.GetType(aTestClass);

    ActiveTest := aTestClass.Create(TestReporter, aTestDataDirectory);
    try
      try
        m := t.GetMethod('Setup');
        if assigned(m) then m.Invoke(ActiveTest, []);
      except
        on E: Exception do TestReporter.ReportErrorCallback('Exception during test setup! ' + E.Message + ' (' + E.ClassName + ')');
      end;

      try
        m := t.GetMethod(MethodName);
        m.Invoke(ActiveTest, []);
      except
        on E: Exception do TestReporter.ReportErrorCallback('Exception: ' + E.Message + ' (' + E.ClassName + ')');
      end;

      try
        m := t.GetMethod('TearDown');
        if assigned(m) then m.Invoke(ActiveTest, []);
      except
        on E: Exception do TestReporter.ReportErrorCallback('Exception during test teardown! ' + E.Message + ' (' + E.ClassName + ')');
      end;
    finally
      ActiveTest.Free;
    end;
  finally
    c.Free;
  end;
end;


{ TTestRunner }

constructor TTestRunner.Create(const aTestDataDirectory : string);
begin
  TestCollection := TTestCollection.Create;
  DataDirCollection := TDataDirCollection.Create;

  fTestDataDirectory := aTestDataDirectory;
end;

destructor TTestRunner.Destroy;
begin
  TestCollection.Free;
  DataDirCollection.Free;
  inherited;
end;

procedure TTestRunner.AddDataDir(const aDataDir: string);
begin
  DataDirCollection.AddDataDir(aDataDir);
end;

procedure TTestRunner.AddTest(const aTestClass: TWatchTowerTestClass);
begin
  TestCollection.AddTest(aTestClass);
end;

procedure TTestRunner.RunTests(const LogCallback: TWriteToLogMethod);
var
  c1: Integer;
  TestClass : TWatchTowerTestClass;
  TestCallbacks : TWatchTowerTestCallbacks;
  TestCount : integer;
  ErrorCount : integer;
  c : TRttiContext;
  t : TRttiType;
  a : TCustomAttribute;
  m : TRttiMethod;
  ActiveTestInfo : TTestInfo;
begin
  WatchTower.Confirm.Confirm.TestFailure := WatchTower.EWatchTowerTestFail;


  ActiveTestInfo.TestClassName := '';
  ActiveTestInfo.TestMethodName := '';

  LogCallback('## WatchTower ##');

  LogCallback(' ');

  TestCount := 0;
  ErrorCount := 0;

  TestCallbacks.LogMessageCallback := LogCallback;

  TestCallbacks.ReportErrorCallback := procedure(const ErrorMsg : string)
  begin
    inc(ErrorCount);
    LogCallback('  ERROR: ' + ErrorMsg);
  end;

  c := TRttiContext.Create;
  try
    for c1 := 0 to TestCollection.Count-1 do
    begin
      TestClass := TestCollection.GetTest(c1);

      ActiveTestInfo.TestClassName := TestClass.ClassName;

      t := c.GetType(TestClass);

      for m in t.GetMethods do
      begin
        for a in m.GetAttributes do
        begin
          if a.ClassType = TestAttribute then
          begin
            inc(TestCount);
            ActiveTestInfo.TestMethodName := m.Name;
            LogCallback('(' + a.UnitName + '.pas) ' + ActiveTestInfo.TestClassName + '.' + ActiveTestInfo.TestMethodName);
            PerformTest(TestClass, TestCallbacks, ActiveTestInfo.TestMethodName, fTestDataDirectory);
            LogCallback(' ');
          end;
        end;
      end;
    end;
  finally
    c.Free;
  end;

  LogCallback(' ');

  LogCallback('## WatchTower Testing Finished ##');

  LogCallback('  Test Count: ' + IntToStr(TestCount));
  LogCallback('  Error Count: ' + IntToStr(ErrorCount));
end;




end.
