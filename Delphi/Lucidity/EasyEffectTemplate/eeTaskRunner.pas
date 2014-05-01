unit eeTaskRunner;

interface

uses
  SysUtils,
  OtlCommon,
  OtlContainers;

type
  TTaskRunner = class
  private
    TaskQueue : TOmniQueue;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunTasks;
    procedure Clear;
    procedure AddTask(aTask : TProc);
  end;

implementation

type
  TTaskData = record
    TaskMethod : TProc;
  end;

{ TTaskRunner }

constructor TTaskRunner.Create;
begin
  TaskQueue := TOmniQueue.Create;
end;

destructor TTaskRunner.Destroy;
begin
  Clear;

  TaskQueue.Free;
  inherited;
end;

procedure TTaskRunner.AddTask(aTask: TProc);
var
  TaskData : TTaskData;
  QueueValue : TOmniValue;
begin
  TaskData.TaskMethod := aTask;
  QueueValue := TOmniValue.FromRecord<TTaskData>(TaskData);
  TaskQueue.Enqueue(QueueValue);
end;

procedure TTaskRunner.Clear;
var
  TaskData : TTaskData;
  QueueValue : TOmniValue;
begin
  while TaskQueue.TryDequeue(QueueValue) do
  begin
    TaskData := QueueValue.ToRecord<TTaskData>;
    TaskData.TaskMethod := nil;
  end;
end;

procedure TTaskRunner.RunTasks;
var
  TaskData : TTaskData;
  QueueValue : TOmniValue;
begin
  while TaskQueue.TryDequeue(QueueValue) do
  begin
    TaskData := QueueValue.ToRecord<TTaskData>;
    TaskData.TaskMethod();
    TaskData.TaskMethod := nil;
  end;
end;

end.
