unit PlugLib.AirControl.TaskSyncQueue;


interface

uses
  Classes,
  VamLib.Collection.DoubleLinkedList,
  VamLib.UniqueID,
  VamLib.Types;

type
  PTaskSyncData = ^TTaskSyncData;
  TTaskSyncData = record
    InUse : boolean;
    ID    : TUniqueID;
    Task  : TThreadProcedure;       // Task!
    OnAudioSync : TThreadProcedure; // method is executed in the audio thread.
    OnGuiSync   : TThreadProcedure; // method is executed in the GUI thread.
  end;

  PTaskSyncQueue = ^TTaskSyncQueue;
  TTaskSyncQueue = class
  private
    List : TRecDoubleLinkedList;
    CS : TFixedCriticalSection;
    FTaskData : array of TTaskSyncData;
    FCapacity: integer;
    FGrowBy: integer;
    FMaxCapacity: integer;
    procedure SetCapacity(const Value: integer);
    function GetCount: integer;
  public
    constructor Create(const Capacity, GrowBy, MaxCapacity : integer);
    destructor Destroy; override;

    function Push(const Task, OnAudioSync, OnGuiSync : TThreadProcedure):TUniqueID; //add task to end of queue.
    function Pop(out Task, OnAudioSync, OnGuiSync :TThreadProcedure):boolean; // Pop oldest task on queue.

    function IsEmpty : boolean;

    property Capacity    : integer read FCapacity;
    property GrowBy      : integer read FGrowBy write FGrowBy;
    property MaxCapacity : integer read FMaxCapacity;
    property Count       : integer read GetCount;
  end;

implementation

uses
  VamLib.ArrayUtils;

{ TTaskQueue }

constructor TTaskSyncQueue.Create(const Capacity, GrowBy, MaxCapacity : integer);
begin
  cs := TFixedCriticalSection.Create;
  List := TRecDoubleLinkedList.Create(Capacity, GrowBy, MaxCapacity);
  FGrowBy := GrowBy;
  FMaxCapacity := MaxCapacity;
  SetCapacity(Capacity);
end;

destructor TTaskSyncQueue.Destroy;
begin
  SetLength(FTaskData, 0);
  cs.Free;
  inherited;
end;

function TTaskSyncQueue.GetCount: integer;
begin
  result := List.Count;
end;

procedure TTaskSyncQueue.SetCapacity(const Value: integer);
begin
  FCapacity := Value;
  SetLength(FTaskData, Value);
end;

function TTaskSyncQueue.IsEmpty: boolean;
begin
  if List.Count = 0
    then result := true
    else result := false;
end;

function TTaskSyncQueue.Push(const Task, OnAudioSync, OnGuiSync  : TThreadProcedure): TUniqueID;
var
  WriteIndex : integer;
  c1: Integer;
begin
  cs.Enter;
  try
    //=== Grow the list if needed ===
    if (Count >= Capacity) then
    begin
      if Capacity + GrowBy > MaxCapacity
        then EVamLibException.Create('Cannot grow list. Max capacity reached.');
      SetCapacity(Capacity+GrowBy);
    end;

    WriteIndex := -1;
    for c1 := 0 to Capacity-1 do
    begin
      if FTaskData[c1].InUse = false then
      begin
        WriteIndex := c1;
        break;
      end;
    end;
    if WriteIndex = -1 then raise EVamLibException.Create('Unexpected error. Cannot find data location.');

    FTaskData[c1].InUse := true;
    FTaskData[c1].ID.Init;
    FTaskData[c1].Task := Task;
    FTaskData[c1].OnAudioSync := OnAudioSync;
    FTaskData[c1].OnGuiSync   := OnGuiSync;
    result := FTaskData[c1].ID;

    List.AppendItem(@FTaskData[c1]);
  finally
    cs.Leave;
  end;
end;


function TTaskSyncQueue.Pop(out Task, OnAudioSync, OnGuiSync :TThreadProcedure):boolean;
var
  TD : PTaskSyncData;
begin
  cs.Enter;
  try
    TD := List.PopFirst;
    if assigned(TD) then
    begin
      if not TD^.InUse then raise EVamLibException.Create('The data record isn''t in use. This is very unexpected and is an error!');
      TD^.InUse := false;
      TD^.ID.Clear;

      Task          := TD^.Task;
      OnAudioSync := TD^.OnAudioSync;
      OnGuiSync   := TD^.OnGuiSync;

      result := true;
    end else
    begin
      Task := nil;
      OnAudioSync := nil;
      OnGuiSync   := nil;
      result := false;
    end;
  finally
    cs.Leave;
  end;
end;

end.
