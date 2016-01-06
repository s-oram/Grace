unit VamLib.MultiCastEvents;

interface

uses
  Classes,
  Vcl.Controls;

type
  PMethod = ^TMethod;

  TCustomMultiCastEvent = class
  private
    FWatcherCount: integer;
  protected
    EventHandlers : array of PMethod;
    function NewEventHandler : PMethod;
    procedure DisposeEventHandler(const Index : integer);
    function FindEventHandler(const eh : TMethod):integer;
  public
    constructor Create;
    destructor Destroy; override;

    property WatcherCount : integer read FWatcherCount;
  end;

  TMultiCastNotifyEvent = class(TCustomMultiCastEvent)
  private
  public
    procedure AddWatcher(const aEventHandler:TNotifyEvent);
    procedure RemoveWatcher(const aEventHandler:TNotifyEvent);
    procedure TriggerAll(const Sender : TObject);
  end;





implementation

{ TCustomMultiCastEvent }

constructor TCustomMultiCastEvent.Create;
begin
  FWatcherCount := 0;
end;

destructor TCustomMultiCastEvent.Destroy;
var
  ehCount : integer;
  c1: Integer;
begin
  ehCount := Length(EventHandlers);
  for c1 := ehCount-1 downto 0 do DisposeEventHandler(c1);
  inherited;
end;

function TCustomMultiCastEvent.NewEventHandler: PMethod;
var
  ehCount : integer;
  c1: Integer;
  Index : integer;
begin
  // Note: Always increment the watcher count when returning a new event handler storage location.
  Inc(FWatcherCount);

  Index := -1;

  ehCount := Length(EventHandlers);
  for c1 := 0 to ehCount-1 do
  begin
    if not assigned(EventHandlers[c1]) then
    begin
      Index := c1;
      break;
    end;
  end;

  if Index = -1 then
  begin
    SetLength(EventHandlers, ehCount+1);
    Index := ehCount;
  end;

  New(EventHandlers[Index]);
  result := EventHandlers[Index];
end;

procedure TCustomMultiCastEvent.DisposeEventHandler(const Index: integer);
begin
  assert( Index < Length(EventHandlers) );

  if assigned(EventHandlers[Index]) then
  begin
    // Only remove the watcher count if the the handler pointer is not nil.
    Dec(FWatcherCount);
    Dispose(EventHandlers[Index]);
    EventHandlers[Index] := nil; //IMPORTANT: nil pointer after disposing it.
  end;
end;


function TCustomMultiCastEvent.FindEventHandler(const eh: TMethod): integer;
var
  ehCount : integer;
  c1: Integer;
begin
  ehCount := Length(EventHandlers);
  for c1 := 0 to ehCount-1 do
  begin
    if (assigned(EventHandlers[c1])) and (eh.Code = EventHandlers[c1]^.Code) and (eh.Data = EventHandlers[c1]^.Data)
      then exit(c1); //================= exit ===========================>>
  end;

  // if we make it this far...
  result := -1;
end;

{ TMultiCastNotifyEvent }

procedure TMultiCastNotifyEvent.AddWatcher(const aEventHandler: TNotifyEvent);
var
  Index : integer;
  eh : PMethod;
begin
  Index := FindEventHandler(TMethod(aEventHandler));
  if Index = -1 then
  begin
    eh := self.NewEventHandler;
    eh^.Code := TMethod(aEventHandler).Code;
    eh^.Data := TMethod(aEventHandler).Data;
  end;
end;

procedure TMultiCastNotifyEvent.RemoveWatcher(const aEventHandler: TNotifyEvent);
var
  Index : integer;
begin
  Index := FindEventHandler(TMethod(aEventHandler));
  if Index <> -1
    then self.DisposeEventHandler(Index);
end;

procedure TMultiCastNotifyEvent.TriggerAll(const Sender: TObject);
var
  ehCount : integer;
  c1: Integer;
  m : TNotifyEvent;
begin
  ehCount := Length(EventHandlers);
  for c1 := 0 to ehCount-1 do
  begin
    if assigned(EventHandlers[c1]) then
    begin
      m := TNotifyEvent(EventHandlers[c1]^);
      m(Sender);
    end;
  end;
end;

end.
