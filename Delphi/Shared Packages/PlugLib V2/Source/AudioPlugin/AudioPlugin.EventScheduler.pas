unit AudioPlugin.EventScheduler;

interface

// TODO:LOW this unit isn't being used anywhere. Consider deleting it.

uses
  Classes,
  VamLib.Collection.DoubleLinkedList,
  VamLib.Collection.List,
  VamLib.UniqueID,
  PlugLib.Types;

type
  PEventData = ^TEventData;
  TEventData = record
    ID          : TUniqueID;
    DeltaFrames : integer;
    Task        : TThreadProcedure;
  end;

  TEventScheduler = class
  private
    ItemsInUse     : TRecList;
    ItemsInReserve : TRecList;
    CS : TFixedCriticalSection;
    FMaxCapacity: integer;
    function GetCapacity: integer;
  public
    constructor Create(const Capacity, MaxCapacity : integer);
    destructor Destroy; override;

    function InsertEvent(const DeltaFrames : integer; const Task : TThreadProcedure):TUniqueID;
    procedure CancelEvent(const ID : TUniqueID);
    function NextEventDelta : integer;

    // Pop() will pop the first item in the list with a DeltaFrames value
    // smaller than the CurDeltaFrames argument.
    function Pop(const CurDeltaFrames : integer; out Task : TThreadProcedure):boolean;

    property Capacity    : integer read GetCapacity;
    property MaxCapacity : integer read FMaxCapacity;

    procedure ReduceDeltaFrames(const SampleFrames : integer);
  end;

implementation

{ TEventScheduler }

constructor TEventScheduler.Create(const Capacity, MaxCapacity: integer);
var
  c1 : integer;
  ev : PEventData;
begin
  cs := TFixedCriticalSection.Create;

  FMaxCapacity := MaxCapacity;

  ItemsInUse     := TRecList.Create(Capacity, 1, MaxCapacity);
  ItemsInReserve := TRecList.Create(Capacity, 1, MaxCapacity);

  // Create the initial capacity.
  for c1 := 0 to Capacity-1 do
  begin
    New(ev);
    ItemsInReserve.Push(ev);
  end;
end;

destructor TEventScheduler.Destroy;
var
  ev : PEventData;
begin
  while true do
  begin
     ev := ItemsInReserve.Pop;
     if assigned(ev)
       then Dispose(ev)
       else break;
  end;

  while true do
  begin
     ev := ItemsInUse.Pop;
     if assigned(ev)
       then Dispose(ev)
       else break;
  end;

  cs.Free;
  inherited;
end;

function TEventScheduler.GetCapacity: integer;
begin
  result := ItemsInUse.Count + ItemsInReserve.Count;
end;

function TEventScheduler.InsertEvent(const DeltaFrames: integer; const Task: TThreadProcedure): TUniqueID;
var
  ev : PEventData;
begin
  assert(assigned(Task));
  assert(DeltaFrames >= 0);

  cs.Enter;
  try
    ev := ItemsInReserve.Pop;
    if not assigned(ev) then
    begin
      ItemsInUse.Compress;
      if (ItemsInReserve.Count + ItemsInUse.Count) >= MaxCapacity
        then raise EPlugLibException.Create('TTaskQueue at max capacity.')
        else New(ev);
    end;

    ev^.ID.Init;
    ev^.DeltaFrames := DeltaFrames;
    ev^.Task := Task;
    ItemsInUse.Push(ev);

    result := ev^.ID;
  finally
    cs.Leave;
  end;
end;


procedure TEventScheduler.CancelEvent(const ID: TUniqueID);
var
  c1: Integer;
  ev : PEventData;
begin
  cs.Enter;
  try
    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      ev := ItemsInUse[c1];
      if (assigned(ev)) and (ev^.ID = ev^.ID) then
      begin
        ItemsInUse.Remove(ev);
        ev^.ID.Clear;
        ev^.DeltaFrames := 0;
        ev^.Task := nil;
        ItemsInReserve.Push(ev);
      end;
    end;
  finally
    cs.Leave;
  end;

end;

function TEventScheduler.NextEventDelta: integer;
var
  c1: Integer;
  ev : PEventData;
  MinDelta : integer;
begin
  result := -1;

  cs.Enter;
  try
    if ItemsInUse.Count = 0 then
    begin
      exit(-1); //============>> exit >>=========>>
    end;

    MinDelta := High(integer);

    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      ev := ItemsInUse[c1];
      if (assigned(ev)) then
      begin
        if (MinDelta > ev^.DeltaFrames)
          then MinDelta := ev^.DeltaFrames;
      end;
    end;
  finally
    cs.Leave;
  end;

end;

function TEventScheduler.Pop(const CurDeltaFrames : integer; out Task: TThreadProcedure): boolean;
var
  c1: Integer;
  ev : PEventData;
begin
  result := false;

  cs.Enter;
  try
    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      ev := ItemsInUse[c1];
      if (assigned(ev)) then
      begin
        if ev^.DeltaFrames <= CurDeltaFrames then
        begin
          Task := ev^.Task;

          ItemsInUse.Remove(ev);
          ev^.ID.Clear;
          ev^.DeltaFrames := 0;
          ev^.Task := nil;
          ItemsInReserve.Push(ev);

          exit; //==============>> exit >>=================>>
        end;
      end;
    end;
  finally
    cs.Leave;
  end;
end;


procedure TEventScheduler.ReduceDeltaFrames(const SampleFrames: integer);
var
  c1: Integer;
  ev : PEventData;
begin
  cs.Enter;
  try
    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      ev := ItemsInUse[c1];
      if (assigned(ev)) then
      begin
        if ev^.DeltaFrames - SampleFrames >= 0
          then dec(ev^.DeltaFrames, SampleFrames)
          else ev^.DeltaFrames := 0;
      end;
    end;

    if ItemsInUse.Count >= (Capacity div 2)
      then ItemsInUse.Compress;

  finally
    cs.Leave;
  end;
end;

end.
