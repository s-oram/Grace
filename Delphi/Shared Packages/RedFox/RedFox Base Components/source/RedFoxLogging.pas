unit RedFoxLogging;






{$IFDEF DesignTimeLogging}
interface
uses
  SmartInspect;
var
  Si: TSmartInspect;
  RedFoxLog : TSiSession;
implementation
uses
  SysUtils;
var
  Connections : string;
initialization
  Si := TSmartInspect.Create(ExtractFileName(ParamStr(0)));
  RedFoxLog := Si.AddSession('RedFox', True);
  Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
  si.Connections := Connections;
  Si.Enabled := true;
  RedFoxLog.LogMessage('Logging Initialized.');
finalization
  RedFoxLog := nil;
  FreeAndNil(Si);
{$ELSE}
interface
implementation
{$ENDIF}

end.
