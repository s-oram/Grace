unit Vam.SmartInspect;

interface

uses
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
    function TrackMethodTime(const aMethodName : string):ITrackMethodTime;
  end;

implementation

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
begin

end;

function TSmartInspectLogService.TrackMethod(const aMethodName: string): ITrackMethod;
begin
  // TODO:HIGH
  result := nil;
end;

function TSmartInspectLogService.TrackMethodTime(const aMethodName: string): ITrackMethodTime;
begin
  // TODO:HIGH
  result := nil;
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

end.
