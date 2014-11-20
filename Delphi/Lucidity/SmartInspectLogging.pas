unit SmartInspectLogging;

interface

{$INCLUDE Defines.inc}

uses
  Windows,
  VamLib.LoggingProxy,
  SmartInspect;

var
  Si: TSmartInspect;
  LogMain : TSiSession;
  VamLibLog : TSiSession;
  LogSpecial : TSiSession;
  TimingLog  : TSiSession;

type
  TSmartInspectProxy = class(TInterfacedObject, ILoggingProxy)
  private
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    procedure TrackMethod(const aMethodName : string);
  end;

  ISiMethodTimer = interface
  end;

  TSiMethodTimer = class(TInterfacedObject, ISiMethodTimer)
  private
    FLevel: TSiLevel;
    FMethodName: UnicodeString;
    FSession: TSiSession;
    StartTicks : DWord;
    EndTicks   : DWord;
  public
    constructor Create(const ALevel: TSiLevel; const ASession: TSiSession; const AMethodName: UnicodeString);
    destructor Destroy; override;
  end;


function TrackMethodTime(const AMethodName: UnicodeString):ISiMethodTimer;

implementation

uses
  VamLib.Utils,
  Lucidity.Utils,
  eePluginDataDir,
  SysUtils;

var
  Connections : string;

{ TSmartInspectProxy }

procedure TSmartInspectProxy.LogError(const aTitle: string);
begin
  VamLibLog.LogError(aTitle);
end;

procedure TSmartInspectProxy.LogMessage(const aTitle: string);
begin
  VamLibLog.LogMessage(aTitle);
end;

procedure TSmartInspectProxy.TrackMethod(const aMethodName: string);
begin
  VamLibLog.TrackMethod(aMethodName);
end;

{$IF Defined(LogToFile)}
var
  LogFileName : string;
{$IFEND}

{ TSiMethodTimer }

constructor TSiMethodTimer.Create(const ALevel: TSiLevel; const ASession: TSiSession; const AMethodName: UnicodeString);
begin
  FSession := ASession;
  FMethodName := AMethodName;
  FLevel := ALevel;
  StartTicks := GetTickCount;
end;

destructor TSiMethodTimer.Destroy;
begin
  EndTicks := GetTickCount;

  if (EndTicks - StartTicks) > 0
    then FSession.LogMessage('Method Time: ' + FMethodName + kChar.Space + IntToStr(EndTicks-StartTicks) + 'ms');

  inherited;
end;



function TrackMethodTime(const AMethodName: UnicodeString):ISiMethodTimer;
begin
  if assigned(TimingLog)
    then result := TSiMethodTimer.Create(TSiLevel.lvDebug, TimingLog, AMethodName)
    else result := nil;
end;


initialization
  Si := TSmartInspect.Create(ExtractFileName(ParamStr(0)));

  LogMain   := Si.AddSession('Main', True);
  VamLibLog := Si.AddSession('VamLib', True);
  LogSpecial := si.AddSession('LogSpecial', true);
  TimingLog  := si.AddSession('TimingLog', true);
  LogSpecial.Active := false;


  {$IF Defined(LogToFile)}
  if PluginDataDir^.Exists then
  begin
    LogFileName := IncludeTrailingPathDelimiter(PluginDataDir.Path) + IncludeTrailingPathDelimiter('Error Reports') + 'Grace Log.sil';
  end;
  {$IFEND}

  {$IF Defined(LogToFile) and Defined(LogToConsole)}
    Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
    Connections := Connections + ',' + format('file(filename="%s", append=false, rotate=daily, maxparts=5)', [LogFileName]);
  {$ELSEIF Defined(LogToFile)}
    Connections := format('file(filename="%s", append=false, rotate=daily, maxparts=5)', [LogFileName]);
  {$ELSEIF Defined(LogToConsole)}
    Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
  {$IFEND}


  si.Connections := Connections;
  Si.Enabled := true;

  LogMain.LogMessage('Logging Initialized.');
  VamLibLog.LogMessage('VamLibLog Created');

  LogMain.LogText('Grace Build Info', 'Grace ' + GetPluginBuildInfo);

  //LogMain.LogMessage();

finalization
  // NOTE: HACK: Clearing the proxy here feels hackish.. Dunno. Maybe it needs to be setup in another unit.
  VamLib.LoggingProxy.Log.ClearProxy;

  VamLibLog := nil;
  LogMain := nil;
  LogSpecial := nil;
  FreeAndNil(Si);

end.
