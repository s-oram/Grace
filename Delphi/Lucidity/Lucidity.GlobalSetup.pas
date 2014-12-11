unit Lucidity.GlobalSetup;

interface

{$INCLUDE Defines.inc}

implementation

uses
  SysUtils,
  eePluginDataDir,
  SmartInspect,
  VamLib.Logging,
  Vam.SmartInspectExt;


procedure SetupLogging;
var
  LoggingConfig : TLogServicetConfig;
  LogService : TLogServiceResult;
begin
  {$IF Defined(Logging)}

    {$IF Defined(LogToFile)}
    LoggingConfig.LogToFile := true;
    {$ELSE}
    LoggingConfig.LogToFile := false;
    {$IFEND}

    {$IF Defined(LogToConsole)}
    LoggingConfig.LogToConsole := true;
    {$ELSE}
    LoggingConfig.LogToConsole := false;
    {$IFEND}

    if PluginDataDir^.Exists
      then LoggingConfig.LogFileName := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Error Reports') + 'Grace Log.sil'
      else LoggingConfig.LogFileName := '';

    LogService := GetLogService(@LoggingConfig);

    Log.Inject(TLogSession.Main,     LogService.LogMain);
    Log.Inject(TLogSession.Controls, LogService.LogControls);
    Log.Inject(TLogSession.Lib,      LogService.LogLib);
    Log.Inject(TLogSession.Debug,    LogService.LogDebug);
    Log.Inject(TLogSession.Timing,   LogService.LogTiming);
  {$IFEND}
end;

initialization
  SetupLogging;

finalization

end.
