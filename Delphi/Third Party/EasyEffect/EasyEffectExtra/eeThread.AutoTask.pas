{
  AutoTask provides a very simple method to execute a procedure in another thread.
}


unit eeThread.AutoTask;

interface

type
  TProcedureOfObject = procedure of object;

procedure AutoTask(Task, OnFinish : TProcedureOfObject);

implementation

uses
  OtlTask, OtlTaskControl;


type
  TAutoTaskClass = class
  private
    fTask : TProcedureOfObject;
    fOnFinish : TProcedureOfObject;
    procedure Worker(const Task:IOmniTask);
    procedure TerminatedHandler;
  protected
  public
    constructor Create(Task, OnFinish : TProcedureOfObject);
    destructor Destroy; override;
  end;

{ TOtlWrapper }

constructor TAutoTaskClass.Create(Task, OnFinish : TProcedureOfObject);
begin
  fTask := Task;
  fOnFinish := OnFinish;
  CreateTask(Worker, 'Worker').Unobserved.OnTerminated(TerminatedHandler).Run;
end;

procedure TAutoTaskClass.Worker(const Task: IOmniTask);
begin
  if assigned(fTask) then fTask;
end;

destructor TAutoTaskClass.Destroy;
begin
  inherited;
end;

procedure TAutoTaskClass.TerminatedHandler;
begin
  if assigned(fOnFinish) then fOnFinish;
  Self.Free;
end;


//==============================================================================
procedure AutoTask(Task, OnFinish : TProcedureOfObject);
begin
  TAutoTaskClass.Create(Task, OnFinish);
end;


end.
