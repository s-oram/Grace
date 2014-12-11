unit VamLib.Logging;

interface

{$SCOPEDENUMS ON}

uses
  VamLib.Logging.AbstractLog;

type
  TLogSession = (Main, Controls, Lib, Debug, Timing);

  // Log is a global singleton class. All client code can use the "Log" to write out
  // to the global application log.
  //
  // Application developers will need to inject a "Log Object" that handles the
  // actual logging and maps the IAbstractLog methods onto those provided by
  // their logging framework of choice.
  //
  // VamLib is a low-level library. Using dependency injection avoids requiring
  // VamLib to depend on a specific logging frame work. The application developer
  // can choose a logging frame and configuration that is suitable for the application
  // being developed.
  //
  // Including "log" as a low level service in VamLib allows code using VamLib to write out to
  // the global application log, without requiring knowledge of the global application
  // log implementation. Very handy when debugging other packages/libraries that
  // are used by the application!
  Log = class
  private
    class var FMain     : IAbstractLog;
    class var FControls : IAbstractLog;
    class var FLib      : IAbstractLog;
    class var FDebug    : IAbstractLog;
    class var FTiming   : IAbstractLog;
  public
    class constructor Create;
    class destructor Destroy;

    class function Main     : IAbstractLog;
    class function Controls : IAbstractLog;
    class function Lib      : IAbstractLog;
    class function Debug    : IAbstractLog;
    class function Timing   : IAbstractLog;

    class procedure Inject(const Session : TLogSession; const LogObject : IAbstractLog);
  end;

implementation

uses
 SysUtils;

{ Log }

class constructor Log.Create;
begin
  FMain     := TDoNothingLog.Create;
  FControls := TDoNothingLog.Create;
  FLib      := TDoNothingLog.Create;
  FDebug    := TDoNothingLog.Create;
  FTiming   := TDoNothingLog.Create;
end;

class destructor Log.Destroy;
begin
  FMain     := nil;
  FControls := nil;
  FLib      := nil;
  FDebug    := nil;
  FTiming   := nil;
end;

class function Log.Controls: IAbstractLog;
begin
  result := FControls;
end;

class function Log.Debug: IAbstractLog;
begin
  result := FDebug;
end;

class function Log.Lib: IAbstractLog;
begin
  result := FLib;
end;

class function Log.Main: IAbstractLog;
begin
  result := FMain;
end;

class function Log.Timing: IAbstractLog;
begin
  result := FTiming;
end;

class procedure Log.Inject(const Session: TLogSession; const LogObject: IAbstractLog);
begin
  case Session of
    TLogSession.Main:     FMain     := LogObject;
    TLogSession.Controls: FControls := LogObject;
    TLogSession.Lib:      FLib      := LogObject;
    TLogSession.Debug:    FDebug    := LogObject;
    TLogSession.Timing:   FTiming   := LogObject;
  else
    raise Exception.Create('Log type not handled.');
  end;
end;



end.
