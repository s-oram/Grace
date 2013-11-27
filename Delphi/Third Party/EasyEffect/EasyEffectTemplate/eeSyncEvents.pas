unit eeSyncEvents;

interface

uses
  SysUtils, Generics.Collections;

type

  //===================================================================
  //  Private Internal Use Only : Use only within this unit.
  //===================================================================
  PSyncEvent = ^TSyncEvent;
  TSyncEvent = record
    Sender   : TObject;
    Delta    : integer;
    CallBack : TProc;
  end;

  TSyncEventList = TList<PSyncEvent>;
  //===================================================================
  //===================================================================


  // IMPORTANT NOTE: Only use the TSyncEventController's public methods. Do not use
  // TSyncEventList or TSyncEvent in any code outside this unit.
  // I think the sync event list would be better implemented with a
  // linked list and a object pool to prevent memory allocations/deallocations
  // during the process thread would be better. If the SyncEvent system
  // ends up getting heavily used in a project I might come back and improve
  // the implementation. But as a proof-of-concept and light production
  // usage, the current implementation should be fine.

  TSyncEventController = class
  private
  protected
    SyncEventList : TSyncEventList;
    procedure RemoveEventFromList(EventIndex:integer); //inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSyncEvent(Delta : integer; CallBack:TProc; Sender: TObject);

    //SamplesToNextSyncEvent() returns -1 if there are no events.
    function SamplesToNextSyncEvent:integer; //inline;

    // Calls any events with a Delta 0 value.
    procedure CallNextSyncEvent; //inline;

    procedure DecrementDeltas(SampleFrames:integer); //inline;

    procedure Clear;

    procedure DeleteAllEventsFrom(Sender:TObject);
  end;

implementation

{ TSyncEventList }

constructor TSyncEventController.Create;
begin
  SyncEventList := TSyncEventList.Create;
end;

destructor TSyncEventController.Destroy;
begin
  SyncEventList.Free;
  inherited;
end;

procedure TSyncEventController.RemoveEventFromList(EventIndex: integer);
var
  se:PSyncEvent;
begin
  se := SyncEventList[EventIndex];
  SyncEventList.Delete(EventIndex);
  Dispose(se);
end;

function TSyncEventController.SamplesToNextSyncEvent: integer;
begin
  if SyncEventList.Count = 0 then
  begin
    result := -1;
  end else
  begin
    result := SyncEventList.First^.Delta;
    assert(result >= 0);
  end;
end;

procedure TSyncEventController.AddSyncEvent(Delta: integer; CallBack: TProc; Sender: TObject);
var
  se:PSyncEvent;
  c1: Integer;
begin
  new(se);
  se^.Sender   := Sender;
  se^.Delta    := Delta;
  se^.CallBack := CallBack;

  if SyncEventList.Count = 0 then
  begin
    SyncEventList.Add(se);
  end else
  if SyncEventList.Last^.Delta <= se^.Delta then
  begin
    SyncEventList.Add(se);
  end else
  begin
    for c1 := 0 to SyncEventList.Count-1 do
    begin
      if SyncEventList[c1]^.Delta > se^.Delta then
      begin
        SyncEventList.Insert(c1, se);
        break;
      end;
    end;
  end;
end;

procedure TSyncEventController.CallNextSyncEvent;
var
  CallBack:TProc;
begin
  while (SyncEventList.Count > 0) and (SyncEventList.First^.Delta = 0) do
  begin
    //Call the event callback.
    CallBack := SyncEventList.First^.CallBack;
    CallBack;
    RemoveEventFromList(0);
  end;
end;

procedure TSyncEventController.Clear;
var
  c1: Integer;
begin
  for c1 := SyncEventList.Count-1 downto 0 do
  begin
    RemoveEventFromList(c1);
  end;
end;

procedure TSyncEventController.DecrementDeltas(SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to SyncEventList.Count-1 do
  begin
    dec(SyncEventList[c1]^.Delta, SampleFrames);

    assert(SyncEventList[c1]^.Delta >= 0);
  end;
end;





procedure TSyncEventController.DeleteAllEventsFrom(Sender: TObject);
var
  c1 : integer;
begin
  for c1 := SyncEventList.Count-1 downto 0 do
  begin
    //Access violation here for some reason...
    if SyncEventList[c1]^.Sender = Sender then RemoveEventFromList(c1);
  end;
end;

end.
