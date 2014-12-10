unit VamLib.SmartInspect;

interface

uses
  SysUtils,
  Windows,
  SmartInspect,
  VamLib.SmartInspectEx;

var
  SI: TSmartInspect;

  Log : record
    Main     : TSiSession;
    Controls : TSiSession;
    Lib      : TSiSession;
    Debug    : TSiSession;
    Timing   : TSiSession;
  end;

  LogMain : TSiSession;


type
  PSmartInspectConfig = ^TSmartInspectConfig;
  TSmartInspectConfig = record
    LoggingEnabled : boolean;
    LogToConsole   : boolean;
    LogToFile      : boolean;
    LogFileName    : string;
  end;


procedure SetupLogging(Config : PSmartInspectConfig);


function TrackMethodTime(const AMethodName: UnicodeString):ISiMethodTimer; overload;
function TrackMethodTime(const AMethodName: UnicodeString; const AMinTime : cardinal):ISiMethodTimer; overload;


implementation

var
  HasLoggingBeenSetup : boolean;

procedure SetupLogging(Config : PSmartInspectConfig);
var
  Connections : string;
begin
  if HasLoggingBeenSetup then raise Exception.Create('Logging has already been initialised.');

  SI := TSmartInspect.Create(ExtractFileName(ParamStr(0)));

  Log.Main     := Si.AddSession('Main', True);
  Log.Controls := Si.AddSession('Controls', True);
  Log.Lib      := Si.AddSession('Lib', True);
  Log.Debug    := Si.AddSession('Debug', True);
  Log.Timing   := Si.AddSession('Timing', True);

  LogMain := Log.Main;

  SI.Enabled := Config^.LoggingEnabled;

  if (Config^.LogToFile) and (Config^.LogToConsole) then
  begin
    Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
    Connections := Connections + ',' + format('file(filename="%s", append=false, rotate=daily, maxparts=5)', [Config^.LogFileName]);
    SI.Connections := Connections;
  end else
  if  (Config^.LogToConsole) then
  begin
    Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
    SI.Connections := Connections;
  end else
  if (Config^.LogToFile) then
  begin
    Connections := format('file(filename="%s", append=false, rotate=daily, maxparts=5)', [Config^.LogFileName]);
    SI.Connections := Connections;
  end else
  begin
    SI.Enabled := false;
  end;

  Log.Main.LogMessage('Logging Initialised');

  HasLoggingBeenSetup := true;
end;

function TrackMethodTime(const AMethodName: UnicodeString):ISiMethodTimer;
begin
  if assigned(Log.Timing)
    then result := TSiMethodTimer.Create(TSiLevel.lvDebug, Log.Timing, AMethodName, 50)
    else result := nil;
end;

function TrackMethodTime(const AMethodName: UnicodeString; const AMinTime : cardinal):ISiMethodTimer; overload;
begin
  if assigned(Log.Timing)
    then result := TSiMethodTimer.Create(TSiLevel.lvDebug, Log.Timing, AMethodName, AMinTime)
    else result := nil;
end;

initialization
  HasLoggingBeenSetup := false;

finalization
  if (HasLoggingBeenSetup) then
  begin
    Log.Main.LogMessage('Logging Finished');
  end;

  LogMain      := nil;
  Log.Main     := nil;
  Log.Controls := nil;
  Log.Lib      := nil;
  Log.Debug    := nil;
  Log.Timing   := nil;
  if assigned(SI)
    then FreeAndNil(Si);


end.