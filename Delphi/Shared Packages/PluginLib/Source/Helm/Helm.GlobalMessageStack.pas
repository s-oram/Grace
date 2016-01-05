unit Helm.GlobalMessageStack;

interface

uses
  Classes,
  Contnrs,
  Helm.Message;

type
  GlobalMessageStack = record
  public
    class function NewMessage:THelmMessage; static;
    class procedure ReleaseMessage(aMsg : THelmMessage); static;
  end;

implementation

uses
  System.SyncObjs,
  SysUtils;

type
  THelmMessageStack = class
  private
  protected
    FLock : TObject;
    FList : TObjectList;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;

    function Pop:THelmMessage;
    procedure Push(const aMsg : THelmMessage);
  end;


{ THelmMessageStack }

constructor THelmMessageStack.Create;
begin
  FLock := TObject.Create;
  FList := TObjectList.Create;
  FList.OwnsObjects := true;
end;

destructor THelmMessageStack.Destroy;
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

procedure THelmMessageStack.Lock;
begin
  TMonitor.Enter(FLock);
end;

procedure THelmMessageStack.Unlock;
begin
  TMonitor.Exit(FLock);
end;

function THelmMessageStack.Pop: THelmMessage;
begin
  Lock;
  try
    if FList.Count > 0
      then result := FList.Extract(FList.Last) as THelmMessage
      else result := nil;
  finally
    Unlock;
  end;
end;

procedure THelmMessageStack.Push(const aMsg: THelmMessage);
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

class function GlobalMessageStack.NewMessage: THelmMessage;
var
  newObject : TObject;
begin
  // Singleton object creation pattern by Ian Boyd.
  // http://stackoverflow.com/a/24657705/395461
  if _MessageStack = nil then
  begin
    newObject := THelmMessageStack.Create;
    if TInterlocked.CompareExchange(_MessageStack, newObject, nil) <> nil then
    begin
      newObject.Free;
    end;
  end;
  result := (_MessageStack as THelmMessageStack).Pop;
end;

class procedure GlobalMessageStack.ReleaseMessage(aMsg: THelmMessage);
begin
  if assigned(_MessageStack)
    then (_MessageStack as THelmMessageStack).Push(aMsg)
    else aMsg.Free;
end;

initialization

finalization
  if assigned(_MessageStack) then FreeAndNil(_MessageStack);



end.
