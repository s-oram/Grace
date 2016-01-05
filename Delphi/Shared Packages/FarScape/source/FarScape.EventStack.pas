unit FarScape.EventStack;

interface

uses
  Classes,
  Contnrs,
  FarScape.Event;

type
  GlobalEventStack = record
  public
    class function NewEvent:TEventData; static;
    class procedure ReleaseEvent(ev : TEventData); static;
  end;

implementation

uses
  System.SyncObjs,
  SysUtils;

type
  TEventStack = class
  private
  protected
    FLock : TObject;
    FList : TObjectList;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;

    function Pop:TEventData;
    procedure Push(const aMsg : TEventData);
  end;


{ TFarScapeMessageStack }

constructor TEventStack.Create;
begin
  FLock := TObject.Create;
  FList := TObjectList.Create;
  FList.OwnsObjects := true;
end;

destructor TEventStack.Destroy;
begin
  // This slightly unusual destruction pattern is copied from TThreadList.Destroy;

  Lock; //make sure nobody else is using the list.
  try
    Flist.Free;
    inherited;
  finally
    UnLock;
    FLock.Free;
  end;
end;

procedure TEventStack.Lock;
begin
  TMonitor.Enter(FLock);
end;

procedure TEventStack.Unlock;
begin
  TMonitor.Exit(FLock);
end;

function TEventStack.Pop: TEventData;
begin
  Lock;
  try
    if FList.Count > 0
      then result := FList.Extract(FList.Last) as TEventData
      else result := nil;
  finally
    Unlock;
  end;
end;

procedure TEventStack.Push(const aMsg: TEventData);
begin
  Lock;
  try
    FList.Add(aMsg);
  finally
    Unlock;
  end;
end;



var
  _MessageStack : TObject;

{ GlobalMessageStack }

class function GlobalEventStack.NewEvent: TEventData;
var
  newObject : TObject;
begin
  // Singleton object creation pattern by Ian Boyd.
  // http://stackoverflow.com/a/24657705/395461
  if _MessageStack = nil then
  begin
    newObject := TEventStack.Create;
    if TInterlocked.CompareExchange(_MessageStack, newObject, nil) <> nil then
    begin
      newObject.Free;
    end;
  end;
  result := (_MessageStack as TEventStack).Pop;
end;

class procedure GlobalEventStack.ReleaseEvent(ev: TEventData);
begin
  if assigned(_MessageStack)
    then (_MessageStack as TEventStack).Push(ev)
    else ev.Free;
end;

initialization

finalization
  if assigned(_MessageStack) then FreeAndNil(_MessageStack);


end.
