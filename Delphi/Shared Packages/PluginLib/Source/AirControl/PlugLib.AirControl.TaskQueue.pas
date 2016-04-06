unit PlugLib.AirControl.TaskQueue;

interface

uses
  Classes,
  VamLib.Collection.DoubleLinkedList,
  VamLib.Collection.List,
  VamLib.UniqueID,
  VamLib.Types;

type
  PTaskData = ^TTaskData;
  TTaskData = record
    InUse : boolean;
    ID    : TUniqueID;
    Task  : TThreadProcedure;
  end;

  PTaskQueue = ^TTaskQueue;
  TTaskQueue = class
  private
    ItemsInUse     : TRecDoubleLinkedList;
    ItemsInReserve : TRecList;
    CS : TFixedCriticalSection;
    FGrowBy: integer;
    FMaxCapacity: integer;
    function GetCount: integer;
    function GetCapacity: integer;
  public
    constructor Create(const Capacity, GrowBy, MaxCapacity : integer);
    destructor Destroy; override;

    function Push(const Task : TThreadProcedure):TUniqueID; //add task to end of queue.
    function Pop(out Task:TThreadProcedure):boolean; // Pop oldest task on queue.

    function IsEmpty : boolean;

    property Capacity    : integer read GetCapacity;
    property GrowBy      : integer read FGrowBy write FGrowBy;
    property MaxCapacity : integer read FMaxCapacity;
    property Count       : integer read GetCount;
  end;

implementation

uses
  VamLib.ArrayUtils;

{ TTaskQueue }

constructor TTaskQueue.Create(const Capacity, GrowBy, MaxCapacity : integer);
var
  c1: Integer;
  TaskData : PTaskData;
begin
  cs := TFixedCriticalSection.Create;

  ItemsInUse     := TRecDoubleLinkedList.Create(Capacity, GrowBy, MaxCapacity);
  ItemsInReserve := TRecList.Create(Capacity, GrowBy, MaxCapacity);

  FGrowBy := GrowBy;
  FMaxCapacity := MaxCapacity;

  // Create the initial capacity.
  for c1 := 0 to Capacity-1 do
  begin
    New(TaskData);
    ItemsInReserve.Push(TaskData);
  end;
end;

destructor TTaskQueue.Destroy;
var
  TaskData : PTaskData;
begin
  while true do
  begin
     TaskData := ItemsInReserve.Pop;
     if assigned(TaskData)
       then Dispose(TaskData)
       else break;
  end;

  while true do
  begin
     TaskData := ItemsInUse.PopLast;
     if assigned(TaskData)
       then Dispose(TaskData)
       else break;
  end;


  cs.Free;
  inherited;
end;

function TTaskQueue.GetCapacity: integer;
begin
  result := ItemsInUse.Count + ItemsInReserve.Count;
end;

function TTaskQueue.GetCount: integer;
begin
  result := ItemsInUse.Count;
end;

function TTaskQueue.IsEmpty: boolean;
begin
  if ItemsInUse.Count = 0
    then result := true
    else result := false;
end;

function TTaskQueue.Push(const Task : TThreadProcedure): TUniqueID;
var
  TaskData : PTaskData;
begin
  cs.Enter;
  try
    TaskData := ItemsInReserve.Pop;
    if not assigned(TaskData) then
    begin
      ItemsInReserve.Compress;
      if (ItemsInReserve.Count + ItemsInUse.Count) >= MaxCapacity
        then raise EVamLibException.Create('TTaskQueue at max capacity.')
        else New(TaskData);
    end;

    TaskData^.InUse := true;
    TaskData^.ID.Init;
    TaskData^.Task := Task;

    ItemsInUse.AppendItem(TaskData);

    result := TaskData^.ID;
  finally
    cs.Leave;
  end;
end;


function TTaskQueue.Pop(out Task:TThreadProcedure):boolean;
var
  TD : PTaskData;
begin
  cs.Enter;
  try
    TD := ItemsInUse.PopFirst;
    if assigned(TD) then
    begin
      if not TD^.InUse then raise EVamLibException.Create('The data record isn''t in use. This is very unexpected and is an error!');
      TD^.InUse := false;
      TD^.ID.Clear;

      Task := TD^.Task;
      result := true;

      // Return the task to not in use list.
      TD^.InUse := false;
      TD^.ID.Clear;
      TD^.Task := nil;
      ItemsInReserve.Push(TD);
    end else
    begin
      Task := nil;
      result := false;
    end;
  finally
    cs.Leave;
  end;
end;




end.
