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
  TFindTestDataDir   = reference to function(const TestUnitName : string):string;

  TWatchTowerTestCallbacks = record
    ReportErrorCallback     : TReportErrorMethod;
    LogMessageCallback      : TWriteToLogMethod;
    FindTestDataDirCallback : TFindTestDataDir;
  end;

  TWatchTowerTest = class
  private
    fTestCallbacks : TWatchTowerTestCallbacks;
    fTestDataDirectory : string;
  protected
    property TestDataDirectory : string read fTestDataDirectory;
  public
    constructor Create(const aTestCallbacks : TWatchTowerTestCallbacks; const aTestDataDirectory : string); virtual;

    procedure Setup; virtual;
    procedure TearDown; virtual;

    // Call ReportError() from within test methods to report errors.
    procedure ReportError(const ErrorMsg : string);

    // Write a message to the attached logger output.
    procedure LogMessage(const Msg : string);

    function FindTestDataDir(const TestUnitName : string):string;
  end;

  TWatchTowerTestClass = class of TWatchTowerTest;


  EWatchTowerTestFail = class(Exception);

implementation


{ TWatchTowerTest }

constructor TWatchTowerTest.Create(const aTestCallbacks : TWatchTowerTestCallbacks; const aTestDataDirectory : string);
begin
  fTestCallbacks := aTestCallbacks;
  fTestDataDirectory := aTestDataDirectory;
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

function TWatchTowerTest.FindTestDataDir(const TestUnitName: string): string;
begin
  if assigned(fTestCallbacks.FindTestDataDirCallback)
    then result := fTestCallbacks.FindTestDataDirCallback(TestUnitName)
    else result := '';
end;

procedure TWatchTowerTest.LogMessage(const Msg: string);
begin
  if assigned(fTestCallbacks.LogMessageCallback) then fTestCallbacks.LogMessageCallback(Msg);
end;

procedure TWatchTowerTest.ReportError(const ErrorMsg: string);
begin
  if assigned(fTestCallbacks.ReportErrorCallback) then fTestCallbacks.ReportErrorCallback(ErrorMsg);
end;



end.
