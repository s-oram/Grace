{
  The TBufferedWorker thread is a worker thread that can be given tasks at will.
  If TBufferedWorker is already busy, the new task will be placed into a queue
  to be completed after the current task is finished.

}

unit eeThread.BufferedWorker;

interface

uses
  OtlSync, OtlTask, OtlTaskControl, Classes;

type
  TProcedureOfObject = procedure of object;

  TBufferedWorker = class
  private
    procedure Worker(const Task:IOmniTask);
    procedure TerminatedHandler;
  protected
    CurrentTask     : TProcedureOfObject;
    CurrentOnFinish : TProcedureOfObject;
    NextTask        : TProcedureOfObject;
    NextOnFinish    : TProcedureOfObject;
    IsWorkingLock : TOmniCS;
    IsWorking   : boolean;
    TaskControl : IOmniTaskControl;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTask(Task:TProcedureOfObject; OnFinish:TProcedureOfObject);


  end;

implementation

{ TBufferedWorker }

constructor TBufferedWorker.Create;
begin
  TaskControl := nil;
  CurrentTask := nil;
  IsWorking := false;
end;

destructor TBufferedWorker.Destroy;
begin
  if assigned(TaskControl) then
  begin
    TaskControl.Terminate;
    TaskControl := nil;
  end;

  inherited;
end;

procedure TBufferedWorker.AddTask(Task:TProcedureOfObject; OnFinish:TProcedureOfObject);
begin
  IsWorkingLock.Acquire;
  try
    if IsWorking = false then
    begin
      IsWorking := true;
      CurrentTask     := Task;
      CurrentOnFinish := OnFinish;
      TaskControl := CreateTask(Worker, 'Worker').OnTerminated(TerminatedHandler).Run;
    end else
    begin
      NextTask     := Task;
      NextOnFinish := OnFinish;
    end;
  finally
    IsWorkingLock.Release;
  end;
end;

procedure TBufferedWorker.Worker(const Task: IOmniTask);
begin
  if assigned(CurrentTask)
    then CurrentTask;
  CurrentTask := nil;
end;

procedure TBufferedWorker.TerminatedHandler;
begin
  if Assigned(CurrentOnFinish) then CurrentOnFinish;
  TaskControl := nil;

  IsWorkingLock.Acquire;
  try
    if assigned(NextTask) then
    begin
      CurrentTask     := NextTask;
      CurrentOnFinish := NextOnFinish;
      NextTask        := nil;
      NextOnFinish    := nil;
      TaskControl     := CreateTask(Worker, 'Worker').OnTerminated(TerminatedHandler).Run;
    end else
    begin
      IsWorking := false;
    end;
  finally
    IsWorkingLock.Release;
  end;
end;


end.
