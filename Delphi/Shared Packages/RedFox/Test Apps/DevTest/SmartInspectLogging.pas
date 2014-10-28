unit SmartInspectLogging;

interface

uses
  SmartInspect;

var
  Si: TSmartInspect;
  LogMain : TSiSession;

implementation

uses
  SysUtils;

var
  Connections : string;

initialization
  Si := TSmartInspect.Create(ExtractFileName(ParamStr(0)));

  LogMain := Si.AddSession('Main', True);

  Connections := 'pipe(reconnect=true, reconnect.interval=1s)';

  si.Connections := Connections;
  Si.Enabled := true;

  LogMain.LogMessage('Logging Initialized.');

finalization

  LogMain := nil;
  FreeAndNil(Si);

end.
