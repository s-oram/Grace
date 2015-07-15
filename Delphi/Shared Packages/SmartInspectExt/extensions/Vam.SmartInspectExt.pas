unit Vam.SmartInspectExt;

interface

uses
  Windows,
  SmartInspect,
  VamLib.Logging.AbstractLog;

type
  IAutoFreeObjectWrapper = interface
    ['{31A791D3-A8CC-43F5-83A5-46CF870D9170}']
    function GetObject : TObject;
  end;

  TAutoFreeObjectWrapper = class(TInterfacedObject, IAutoFreeObjectWrapper)
  private
    FObject : TObject;
  public
    constructor Create(const aObj : TObject);
    destructor Destroy; override;
    function GetObject : TObject;
  end;

  TTrackMethodResult = class(TInterfacedObject, ITrackMethod)
  private
    FSession : TSiSession;
    FMethodName : string;
  public
    constructor Create(const ASession: TSiSession; const AMethodName: string);
    destructor Destroy; override;
  end;

  TTrackMethodTimeResult = class(TInterfacedObject, ITrackMethodTime)
  private
    FSession : TSiSession;
    FMethodName : string;
    FMinTime    : cardinal;
    StartTicks  : DWord;
    EndTicks    : DWord;
  public
    constructor Create(const ASession: TSiSession; const AMethodName: string; const AMinTime : cardinal);
    destructor Destroy; override;
  end;

  TLogSessionWrapper = class(TInterfacedObject, IAbstractLog)
  private
    FSmartInspectObjectWrapper : IAutoFreeObjectWrapper;
    FSession : TSiSession;
  public
    constructor Create(const aSmartInspectObjectWrapper : IAutoFreeObjectWrapper; const aSession : TSiSession);
    destructor Destroy; override;

    procedure LogTime(const aTitle : string);
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    procedure LogText(const aTitle, aText: string);

    procedure EnterMethod(const aMethodName : string);
    procedure LeaveMethod(const aMethodName : string);

    function TrackMethod(const aMethodName : string):ITrackMethod;
    function TrackMethodTime(const aMethodName : string):ITrackMethodTime; overload;
    function TrackMethodTime(const aMethodName : string; const aMinTime : cardinal):ITrackMethodTime; overload;
  end;


  TLogServiceResult = record
    LogMain     : IAbstractLog;
    LogControls : IAbstractLog;
    LogLib      : IAbstractLog;
    LogDebug    : IAbstractLog;
    LogTiming   : IAbstractLog;
  end;


type
  PLogServiceConfig = ^TLogServicetConfig;
  TLogServicetConfig = record
    LogFileName    : string;
    LogToFile      : boolean;
    LogToConsole   : boolean;
  end;

function GetLogService(Config : PLogServiceConfig):TLogServiceResult;

implementation

uses
  SysUtils,
  VamLib.Utils;

{ TSmartInspectLogService }

constructor TLogSessionWrapper.Create(const aSmartInspectObjectWrapper: IAutoFreeObjectWrapper; const aSession: TSiSession);
begin
  FSmartInspectObjectWrapper := aSmartInspectObjectWrapper;
  FSession := aSession;
end;

destructor TLogSessionWrapper.Destroy;
begin
  FSmartInspectObjectWrapper := nil;
  FSession := nil;
  inherited;
end;

procedure TLogSessionWrapper.EnterMethod(const aMethodName: string);
begin
  FSession.EnterMethod(aMethodName);
end;

procedure TLogSessionWrapper.LeaveMethod(const aMethodName: string);
begin
  FSession.LeaveMethod(aMethodName);
end;

procedure TLogSessionWrapper.LogError(const aTitle: string);
begin
  FSession.LogError(ATitle);
end;

procedure TLogSessionWrapper.LogMessage(const aTitle: string);
begin
  FSession.LogMessage(ATitle);
end;

procedure TLogSessionWrapper.LogText(const aTitle, aText: string);
begin
  FSession.LogText(aTitle, aText);
end;

procedure TLogSessionWrapper.LogTime(const aTitle: string);
var
  Hour, Min, Sec, MSec : word;
  s : string;
begin
  DecodeTime(now, Hour, Min, Sec, MSec);
  s := IntToStr(Hour) + ':' + IntToStrB(Min, 2) + '.' + IntToStrB(Sec, 2);
  FSession.LogMessage(ATitle + ' ' + s);
end;

function TLogSessionWrapper.TrackMethod(const aMethodName: string): ITrackMethod;
begin
  result := TTrackMethodResult.Create(FSession, AMethodName);
end;

function TLogSessionWrapper.TrackMethodTime(const aMethodName: string; const AMinTime: cardinal): ITrackMethodTime;
begin
  result := TTrackMethodTimeResult.Create(FSession, AMethodName, AMinTime);
end;

function TLogSessionWrapper.TrackMethodTime(const aMethodName: string): ITrackMethodTime;
begin
  result := TTrackMethodTimeResult.Create(FSession, AMethodName, 0);
end;

{ TAutoFreeObjectWrapper }

constructor TAutoFreeObjectWrapper.Create(const aObj: TObject);
begin
  FObject := aObj;
end;

destructor TAutoFreeObjectWrapper.Destroy;
begin
  if assigned(FObject) then FObject.Free;
  inherited;
end;

function TAutoFreeObjectWrapper.GetObject: TObject;
begin
  result := FObject;
end;

{ TTrackMethodResult }

constructor TTrackMethodResult.Create(const ASession: TSiSession; const AMethodName: string);
begin
  FSession := ASession;
  FMethodName := AMethodName;
  FSession.EnterMethod(FMethodName);
end;

destructor TTrackMethodResult.Destroy;
begin
  FSession.LeaveMethod(FMethodName);
  inherited;
end;

{ TTrackMethodTimeResult }

constructor TTrackMethodTimeResult.Create(const ASession: TSiSession; const AMethodName: string; const AMinTime: cardinal);
begin
  FMinTime := AMinTime;
  FSession := ASession;
  FMethodName := AMethodName;
  StartTicks := GetTickCount;
end;

destructor TTrackMethodTimeResult.Destroy;
begin
  EndTicks := GetTickCount;

  if (EndTicks - StartTicks) >= FMinTime
    then FSession.LogMessage('Method Time: ' + FMethodName + kChar.Space + IntToStr(EndTicks-StartTicks) + 'ms');

  inherited;
end;

function GetLogService(Config : PLogServiceConfig):TLogServiceResult;
var
  Connections : string;
  SI: TSmartInspect;
  LogMain     : TSiSession;
  LogControls : TSiSession;
  LogLib      : TSiSession;
  LogDebug    : TSiSession;
  LogTiming   : TSiSession;

  SIAutoFreeObject : IAutoFreeObjectWrapper;
begin
  //==== Setup all the smart inspect logging objects ====
  SI := TSmartInspect.Create(ExtractFileName(ParamStr(0)));

  LogMain     := Si.AddSession('Main', True);
  LogControls := Si.AddSession('Controls', True);
  LogLib      := Si.AddSession('Lib', True);
  LogDebug    := Si.AddSession('Debug', True);
  LogTiming   := Si.AddSession('Timing', True);

  if (Config^.LogToFile) and (Config^.LogToConsole) then
  begin
    Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
    Connections := Connections + ',' + format('file(filename="%s", append=false, rotate=daily, maxparts=5)', [Config^.LogFileName]);
    SI.Connections := Connections;
    SI.Enabled := true;
  end else
  if  (Config^.LogToConsole) then
  begin
    Connections := 'pipe(reconnect=true, reconnect.interval=1s)';
    SI.Connections := Connections;
    SI.Enabled := true;
  end else
  if (Config^.LogToFile) then
  begin
    Connections := format('file(filename="%s", append=false, rotate=daily, maxparts=5)', [Config^.LogFileName]);
    SI.Connections := Connections;
    SI.Enabled := true;
  end else
  begin
    SI.Enabled := false;
  end;


  //==== Wrap smart inspect logging objects into objects appropiate for the VamLib.Logging service point. ====
  SIAutoFreeObject := TAutoFreeObjectWrapper.Create(SI);

  result.LogMain     := TLogSessionWrapper.Create(SIAutoFreeObject, LogMain);
  result.LogControls := TLogSessionWrapper.Create(SIAutoFreeObject, LogControls);
  result.LogLib      := TLogSessionWrapper.Create(SIAutoFreeObject, LogLib);
  result.LogDebug    := TLogSessionWrapper.Create(SIAutoFreeObject, LogDebug);
  result.LogTiming   := TLogSessionWrapper.Create(SIAutoFreeObject, LogTiming);

  // NOTE: The role of the TAutoFreeObjectWrapper code might not be obvious at first.
  // VamLib.Logger requires interface references to the log sessions.
  // The log sessions require the shared TSmartInspect object reference.
  // The difficulty comes when it's time to free everything. The wrapped log sessions are easy.
  // Interface reference counting means they will automatically be free'd when the interfaces
  // go out of scope. The TSmartInspect object instance is not reference counted so
  // will not automatically be destroyed. Instead we wrap the TSmartInspect reference in
  // an interface that will free the object when it goes out of scope. References to the wrapper
  // interface are held in each session interface.
  // Maybe there is an easier way...
end;



end.
