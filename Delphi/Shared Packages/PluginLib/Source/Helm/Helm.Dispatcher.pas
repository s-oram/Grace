unit Helm.Dispatcher;

interface

uses
  Classes,
  Helm.Sync,
  Helm.Message;

type
  THelmMessageHandler = procedure(const Msg : THelmMessage) of object;

  TListenerDuration = (
    ldOnce, // respond to the first event only.
    ldAll   // respond to all events.
  );

  PListenerInfo = ^TListenerInfo;
  TListenerInfo = record
    EventTypes : array of TAppMessageClass;
    Handler    : THelmMessageHandler;
    Duration   : TListenerDuration;
  end;

  THelmDispatcher = class
  private
    Lock : TLock;
    List : TList;
  protected
    function IsEventForListener(const Event : THelmMessage; const Listener : PListenerInfo):boolean;
    procedure DeleteListenerInfo(const Index : integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(const EventTypes : array of TAppMessageClass; const Handler : THelmMessageHandler; const Duration : TListenerDuration = ldAll);
    procedure RemoveListener(const Handler : THelmMessageHandler);

    // Sends an event to all subscribed listeners.
    procedure Broadcast(const EventMsg : THelmMessage; const ReleaseEvent : boolean = true);
  end;

implementation

{ THelmStream }

constructor THelmDispatcher.Create;
begin
  Lock := TLock.Create;
  List := TList.Create;
end;

destructor THelmDispatcher.Destroy;
var
  c1: Integer;
begin
  Lock.Enter;
  try
    for c1 := List.Count-1 downto 0 do
    begin
      if assigned(List[c1])
        then DeleteListenerInfo(c1);
    end;
    List.Free;
  finally
    Lock.Leave;
  end;
  Lock.Free;
  inherited;
end;

procedure THelmDispatcher.DeleteListenerInfo(const Index: integer);
var
  Info : PListenerInfo;
begin
  Info := List[Index];
  Dispose(Info);
  List[Index] := nil;
end;

procedure THelmDispatcher.AddListener(const EventTypes: array of TAppMessageClass; const Handler: THelmMessageHandler; const Duration: TListenerDuration);
var
  Info : PListenerInfo;
  c1: Integer;
  EventTypeCount : integer;
begin
  Lock.Enter;
  try
    New(Info);

    Info^.Handler := Handler;
    Info^.Duration := Duration;

    // Copy watched event types to the info pointer.
    EventTypeCount := Length(EventTypes);
    SetLength(Info^.EventTypes, EventTypeCount);
    for c1 := 0 to EventTypeCount-1 do Info^.EventTypes[c1] := EventTypes[c1];

    // Look for any empty spaces in the list when storing the Info pointer.
    // This is preferable to continually growing the list.
    for c1 := List.Count-1 downto 0 do
    begin
      if List[c1] = nil then
      begin
        List[c1] := Info;
        exit; //===== exit =====>>
      end;
    end;

    // If we make it this far, no empty space has been found. Add the info pointer
    // to the end of the list.
    List.Add(Info);
  finally
    Lock.Leave;
  end;
end;

procedure THelmDispatcher.RemoveListener(const Handler: THelmMessageHandler);
var
  Info : PListenerInfo;
  c1: Integer;
begin
  Lock.Enter;
  try
    for c1 := 0 to List.Count-1 do
    begin
      Info := List[c1];
      if (assigned(Info)) and (addr(Info.Handler) = addr(Handler))
        then  DeleteListenerInfo(c1);
    end;
  finally
    Lock.Leave;
  end;
end;

procedure THelmDispatcher.Broadcast(const EventMsg: THelmMessage; const ReleaseEvent : boolean);
var
  Info : PListenerInfo;
  c1: Integer;
begin
  Lock.Enter;
  try
    for c1 := 0 to List.Count-1 do
    begin
      Info := List[c1];
      if (assigned(Info)) and (IsEventForListener(EventMsg, Info)) then
      begin
        Info.Handler(EventMsg);
        if Info.Duration = TListenerDuration.ldOnce then DeleteListenerInfo(c1);
      end;
    end;

    if ReleaseEvent then EventMsg.Release;
  finally
    Lock.Leave;
  end;
end;

function THelmDispatcher.IsEventForListener(const Event: THelmMessage; const Listener: PListenerInfo): boolean;
var
  evCount : integer;
  c1: Integer;
begin
  if Length(Listener.EventTypes) = 0 then exit(true);

  evCount := Length(Listener.EventTypes);
  for c1 := 0 to evCount-1 do
  begin
    if Listener.EventTypes[c1] = Event.MsgClass
      then exit(true);
  end;

  // if we make it this far, the event type hasn't been matched.
  result := false;
end;




end.
