unit Vam.SmartInspect;

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

  TSmartInspectLogService = class(TInterfacedObject, IAbstractLog)
  private
    FSmartInspectObjectWrapper : IAutoFreeObjectWrapper;
    FSession : TSiSession;
  public
    constructor Create(const aSmartInspectObjectWrapper : IAutoFreeObjectWrapper; const aSession : TSiSession);
    destructor Destroy; override;

    procedure LogTime(const aTitle : string);
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    function TrackMethod(const aMethodName : string):ITrackMethod;
    function TrackMethodTime(const aMethodName : string):ITrackMethodTime; overload;
    function TrackMethodTime(const aMethodName : string; const aMinTime : cardinal):ITrackMethodTime; overload;
  end;

implementation

uses
  SysUtils,
  VamLib.Utils;

{ TSmartInspectLogService }

constructor TSmartInspectLogService.Create(const aSmartInspectObjectWrapper: IAutoFreeObjectWrapper; const aSession: TSiSession);
begin
  FSmartInspectObjectWrapper := aSmartInspectObjectWrapper;
  FSession := aSession;
end;

destructor TSmartInspectLogService.Destroy;
begin
  FSmartInspectObjectWrapper := nil;
  FSession := nil;
  inherited;
end;

procedure TSmartInspectLogService.LogError(const aTitle: string);
begin
  FSession.LogError(ATitle);
end;

procedure TSmartInspectLogService.LogMessage(const aTitle: string);
begin
  FSession.LogMessage(ATitle);
end;

procedure TSmartInspectLogService.LogTime(const aTitle: string);
var
  Hour, Min, Sec, MSec : word;
  s : string;
begin
  DecodeTime(now, Hour, Min, Sec, MSec);
  s := IntToStr(Hour) + ':' + IntToStrB(Min, 2) + '.' + IntToStrB(Sec, 2);
  FSession.LogMessage(ATitle + ' ' + s);
end;

function TSmartInspectLogService.TrackMethod(const aMethodName: string): ITrackMethod;
begin
  result := TTrackMethodResult.Create(FSession, AMethodName);
end;

function TSmartInspectLogService.TrackMethodTime(const aMethodName: string; const AMinTime: cardinal): ITrackMethodTime;
begin
  result := TTrackMethodTimeResult.Create(FSession, AMethodName, AMinTime);
end;

function TSmartInspectLogService.TrackMethodTime(const aMethodName: string): ITrackMethodTime;
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

end.
