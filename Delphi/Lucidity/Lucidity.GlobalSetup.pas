unit Lucidity.GlobalSetup;

interface

{$INCLUDE Defines.inc}

implementation

uses
  SysUtils,
  eePluginDataDir,
  VamLib.SmartInspect;

procedure SetupLogging;
var
  LogFileName : string;
  LoggingConfig : TSmartInspectConfig;
begin
  {$IF Defined(Logging)}
    LoggingConfig.LoggingEnabled := true;
  {$ELSE}
    LoggingConfig.LoggingEnabled := false;
  {$IFEND}

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
    then LogFileName := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Error Reports') + 'Grace Log.sil'
    else LogFileName := '';



  {$IF Defined(Logging)}
  VamLib.SmartInspect.SetupLogging(@LoggingConfig);
  {$IFEND}




end;

initialization
  SetupLogging;

finalization

end.
