unit WatchTower;

interface

// NOTE: Testing terminology.
// https://www.finalbuilder.com/resources/blogs/postid/697/introducing-dunitx

// Using RTTI in D2010
// http://robstechcorner.blogspot.com.au/2009/09/using-attributes-and-tcustomattribute.html

uses
  SysUtils;

type
  TestAttribute = class(TCustomAttribute)
  end;

  TWriteToLogMethod  = reference to procedure(Msg : string);
  TReportErrorMethod = reference to procedure(const ErrorMsg : string);

  TWatchTowerTestCallbacks = record
    ReportErrorCallback : TReportErrorMethod;
    LogMessageCallback : TWriteToLogMethod;
  end;

  TWatchTowerTest = class
  private
    fTestReporter : TWatchTowerTestCallbacks;
  public
    constructor Create(const TestReporter : TWatchTowerTestCallbacks); virtual;
    procedure Setup; virtual;
    procedure TearDown; virtual;

    // Call ReportError() from within test methods to report errors.
    procedure ReportError(const ErrorMsg : string);

    // Write a message to the attached logger output.
    procedure LogMessage(const Msg : string);
  end;

  TWatchTowerTestClass = class of TWatchTowerTest;


  EWatchTowerTestFail = class(Exception);

implementation


{ TWatchTowerTest }

constructor TWatchTowerTest.Create(const TestReporter : TWatchTowerTestCallbacks);
begin
  fTestReporter := TestReporter;
end;

procedure TWatchTowerTest.Setup;
begin
  // Setup() will be called before each test is run. Override and perform
  // required test preparation.
end;

procedure TWatchTowerTest.TearDown;
begin
  // TearDown() will be called after each test is finished. Override to perform
  // clean up tasks.
end;

procedure TWatchTowerTest.LogMessage(const Msg: string);
begin
  if assigned(fTestReporter.LogMessageCallback) then fTestReporter.LogMessageCallback(Msg);
end;

procedure TWatchTowerTest.ReportError(const ErrorMsg: string);
begin
  if assigned(fTestReporter.ReportErrorCallback) then fTestReporter.ReportErrorCallback(ErrorMsg);
end;



end.
