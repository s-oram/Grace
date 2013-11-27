unit SiAuto;

interface

{$INCLUDE Defines.inc}

uses
  SysUtils,
  SmartInspect;

var
  Si: TSmartInspect;
  SiMain: TSiSession;

implementation

const
  SiConnections = 'pipe(reconnect=true, reconnect.interval=1s)';
  SiSession = 'Main';

initialization
  Si := TSmartInspect.Create(ExtractFileName(ParamStr(0)));
  Si.Connections := SiConnections;
  SiMain := Si.AddSession(SiSession, True);


  {$IFDEF Logging}
    Si.enabled := true;
  {$ELSE}
    Si.enabled := false;
  {$ENDIF}

  SiMain.LogMessage('** Logging Started **');

finalization
  SiMain.LogMessage('** Logging Finished **');

  SiMain := nil;
  FreeAndNil(Si);
end.

