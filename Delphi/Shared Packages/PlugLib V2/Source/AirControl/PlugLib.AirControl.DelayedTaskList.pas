unit PlugLib.AirControl.DelayedTaskList;

interface

uses
  Classes,
  VamLib.Types,
  VamLib.UniqueID,
  VamLib.Collection.List;

type
  PDelayedTaskData = ^TDelayedTaskData;
  TDelayedTaskData = record
    ID : TUniqueID;
    Task, OnAudioSync, OnGuiSync : TThreadProcedure;
    StartTimeRef : cardinal;
    ExecuteDelay : cardinal;
    procedure Clear;
  end;

  PDelayedTaskList = ^TDelayedTaskList;
  TDelayedTaskList = class
  private
    CS : TFixedCriticalSection;
    ItemsInUse     : TRecList;
    ItemsInReserve : TRecList;
    FMaxCapacity: integer;
    function GetCount: integer;
  protected
  public
    constructor Create(const Capacity, GrowBy, MaxCapacity : integer);
    destructor Destroy; override;

    property MaxCapacity : integer read FMaxCapacity;

    function Push(const Task, OnAudioSync, OnGuiSync : TThreadProcedure; const ExecuteDelay : cardinal):TUniqueID;
    function Pop(out Task, OnAudioSync, OnGuiSync :TThreadProcedure):boolean; // Pop oldest task on queue, only if the execution delay has passed.

    procedure CancelTask(const TaskID : TUniqueID);

    property Count : integer read GetCount;

  end;

implementation

uses
  WinApi.Windows;

{ TDelayedTaskList }

constructor TDelayedTaskList.Create(const Capacity, GrowBy, MaxCapacity: integer);
var
  c1: Integer;
  TaskData : PDelayedTaskData;
begin
  FMaxCapacity := MaxCapacity;

  ItemsInUse     := TRecList.Create(Capacity, GrowBy, MaxCapacity);
  ItemsInReserve := TRecList.Create(Capacity, GrowBy, MaxCapacity);

  for c1 := 0 to Capacity-1 do
  begin
    New(TaskData);
    ItemsInReserve.Push(TaskData);
  end;

  CS := TFixedCriticalSection.Create;
end;

destructor TDelayedTaskList.Destroy;
var
  c1: Integer;
  TaskData : PDelayedTaskData;
begin
  CS.Enter;
  try
    // ensure all task data items are free.
    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      TaskData := ItemsInUse[c1];
      if assigned(TaskData) then
      begin
        ItemsInUse.Remove(TaskData);
        Dispose(TaskData);
      end;
    end;

    for c1 := 0 to ItemsInReserve.Count-1 do
    begin
      TaskData := ItemsInReserve[c1];
      if assigned(TaskData) then
      begin
        ItemsInReserve.Remove(TaskData);
        Dispose(TaskData);
      end;
    end;
  finally
    CS.Leave;
    CS.Free;
  end;


  inherited;
end;


// NewDelayedTask()
// - ExecuteDelay is milliseconds.
function TDelayedTaskList.Push(const Task, OnAudioSync, OnGuiSync: TThreadProcedure; const ExecuteDelay: cardinal): TUniqueID;
var
  TaskData : PDelayedTaskData;
begin
  CS.Enter;
  try
    assert(ExecuteDelay > 0);

    TaskData := ItemsInReserve.Pop;
    if not assigned(TaskData) then
    begin
      ItemsInReserve.Compress;
      ItemsInUse.Compress;

      if (ItemsInReserve.Count + ItemsInUse.Count) >= MaxCapacity
        then raise EVamLibException.Create('TDelayedTaskList at max capacity.')
        else New(TaskData);
    end;

    TaskData^.Task := Task;
    TaskData^.ID.Init;
    TaskData^.OnAudioSync := OnAudioSync;
    TaskData^.OnGuiSync   := OnGuiSync;
    TaskData^.StartTimeRef := GetTickCount();
    TaskData^.ExecuteDelay := ExecuteDelay;

    result := TaskData^.ID;

    ItemsInUse.Push(TaskData);
  finally
    CS.Leave;
  end;
end;

function TDelayedTaskList.Pop(out Task, OnAudioSync, OnGuiSync: TThreadProcedure): boolean;
var
  currentTC : cardinal;
  c1: Integer;
  TaskData : PDelayedTaskData;
begin
  cs.Enter;
  try
    currentTC := GetTickCount();

    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      TaskData := ItemsInUse[c1];
      // IMPORTANT:  Always use Current – Start (comparison) Interval when checking elapsed time with GetTickCount()
      //   - CapnBry Development Blog - Proper Timing with GetTickCount()
      //     http://capnbry.net/blog/?p=44
      if (assigned(TaskData)) and ((CurrentTC - TaskData^.StartTimeRef) > TaskData^.ExecuteDelay) then
      begin
        Task        := TaskData^.Task;
        OnAudioSync := TaskData^.OnAudioSync;
        OnGuiSync   := TaskData^.OnGuiSync;

        TaskData^.Task := nil;
        TaskData^.OnAudioSync := nil;
        TaskData^.OnGuiSync := nil;
        TaskData^.ID.Clear;
        TaskData^.StartTimeRef := 0;
        TaskData^.ExecuteDelay := 0;

        ItemsInUse.Remove(TaskData);
        ItemsInReserve.Push(TaskData);

        exit(true);
      end;
    end;


    // if we make it this far, no item has been found.
    result := false;
  finally
    cs.Leave;
  end;
end;

procedure TDelayedTaskList.CancelTask(const TaskID: TUniqueID);
var
  c1 : integer;
  TaskData : PDelayedTaskData;
begin
  assert(TaskID.IsSet);

  cs.Enter;
  try
    for c1 := 0 to ItemsInUse.Count-1 do
    begin
      TaskData := ItemsInUse[c1];
      if (assigned(TaskData)) and (TaskData^.ID = TaskID) then
      begin
        TaskData^.Clear;
        ItemsInUse.Remove(TaskData);
        ItemsInReserve.Push(TaskData);
      end;
    end;
  finally
    cs.Leave;
  end;
end;

function TDelayedTaskList.GetCount: integer;
begin
  cs.Enter;
  try
    result := ItemsInUse.Count;
  finally
    cs.Leave;
  end;
end;

{ TDelayedTaskData }

procedure TDelayedTaskData.Clear;
begin
  self.ID.Clear;
  self.Task         := nil;
  self.OnAudioSync  := nil;
  self.OnGuiSync    := nil;
  self.StartTimeRef := 0;
  self.ExecuteDelay := 0;
end;

end.
