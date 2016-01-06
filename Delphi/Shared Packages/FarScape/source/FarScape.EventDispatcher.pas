unit FarScape.EventDispatcher;

interface

uses
  Classes,
  FarScape.Event;

type
  TEventHandler = procedure(const ev : TEventData) of object;

  TListenerDuration = (
    ldOnce, // respond to the first event only.
    ldAll   // respond to all events.
  );

  PListenerInfo = ^TListenerInfo;
  TListenerInfo = record
    EventTypes : array of TEventClass;
    Handler    : TEventHandler;
    Duration   : TListenerDuration;
  end;

  TEventDispatcher = class
  private
    List : TList;
  protected
    function IsEventForListener(const Event : TEventData; const Listener : PListenerInfo):boolean;
    procedure DeleteListenerInfo(const Index : integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(const EventTypes : array of TEventClass; const Handler : TEventHandler; const Duration : TListenerDuration);
    procedure RemoveListener(const Handler : TEventHandler);
    procedure RemoveAllListeners;

    // Sends an event to all subscribed listeners.
    procedure Broadcast(const EventMsg : TEventData; const ReleaseEvent : boolean = true);
  end;

implementation

{ TFarScapeStream }

constructor TEventDispatcher.Create;
begin
  List := TList.Create;
end;

destructor TEventDispatcher.Destroy;
begin
  RemoveAllListeners;
  List.Free;
  inherited;
end;

procedure TEventDispatcher.DeleteListenerInfo(const Index: integer);
var
  Info : PListenerInfo;
begin
  Info := List[Index];
  assert(assigned(Info));
  Dispose(Info);
  List[Index] := nil;
end;

procedure TEventDispatcher.AddListener(const EventTypes: array of TEventClass; const Handler: TEventHandler; const Duration: TListenerDuration);
var
  Info : PListenerInfo;
  c1: Integer;
  EventTypeCount : integer;
begin
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
end;

procedure TEventDispatcher.RemoveAllListeners;
var
  c1: Integer;
begin
  for c1 := List.Count-1 downto 0 do
  begin
    if assigned(List[c1])
      then DeleteListenerInfo(c1);
  end;
end;

procedure TEventDispatcher.RemoveListener(const Handler: TEventHandler);
var
  Info : PListenerInfo;
  c1: Integer;
begin
  for c1 := 0 to List.Count-1 do
  begin
    Info := List[c1];
    if (assigned(Info)) and (addr(Info.Handler) = addr(Handler))
      then  DeleteListenerInfo(c1);
  end;
end;

procedure TEventDispatcher.Broadcast(const EventMsg: TEventData; const ReleaseEvent : boolean);
var
  Info : PListenerInfo;
  c1: Integer;
begin
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
end;

function TEventDispatcher.IsEventForListener(const Event: TEventData; const Listener: PListenerInfo): boolean;
var
  evCount : integer;
  c1: Integer;
begin
  if Length(Listener.EventTypes) = 0 then exit(true);

  evCount := Length(Listener.EventTypes);
  for c1 := 0 to evCount-1 do
  begin
    if Listener.EventTypes[c1] = Event.EventClass
      then exit(true);
  end;

  // if we make it this far, the event type hasn't been matched.
  result := false;
end;


end.
