unit MyWorker;

interface

uses
  VamLib.Types,
  Classes,
  SysUtils;

type
  TWorkerThread = class;

  TWorker = class
  private
    Thread : TWorkerThread;
    procedure EventHandle_ThreadTerminated(Sender: TObject);
  protected
    IsCanceled : boolean;

    procedure Queue(AMethod: TThreadMethod); overload;
    procedure Queue(AThreadProc: TThreadProcedure); overload;
    procedure Synchronize(AMethod: TThreadMethod); overload;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload;

    procedure Task; virtual;
    procedure TaskFinished; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
    procedure Cancel;
    procedure Stop;
  end;

  TWorkerThread = class(TThread)
  private
    fTask: TProc;
  protected
    procedure Execute; override;
  public
    property Task : TProc read fTask write fTask;
  end;

implementation

{ TWorker }

constructor TWorker.Create;
begin

end;

destructor TWorker.Destroy;
begin
  Stop;
  inherited;
end;

procedure TWorker.EventHandle_ThreadTerminated(Sender: TObject);
begin
  TaskFinished;
end;

procedure TWorker.Queue(AMethod: TThreadMethod);
begin
  Thread.Queue(AMethod);
end;

procedure TWorker.Queue(AThreadProc: TThreadProcedure);
begin
  Thread.Queue(AThreadProc);
end;

procedure TWorker.Run;
begin
  if (assigned(Thread)) then
  begin
    if not Thread.Finished then
    begin
      Thread.Terminate;
      Thread.WaitFor;
    end;
    FreeAndNil(Thread);
  end;

  IsCanceled := false;

  if not (assigned(Thread)) then
  begin
    Thread := TWorkerThread.Create(true);
    Thread.FreeOnTerminate := false;
    Thread.OnTerminate := self.EventHandle_ThreadTerminated;
  end;

  Thread.Task := Task;
  Thread.Start;
end;

procedure TWorker.Cancel;
begin
  IsCanceled := true;

  if (assigned(Thread)) then
  begin
    if not Thread.Finished then
    begin
      Thread.Terminate;
    end;
  end;
end;

procedure TWorker.Stop;
begin
  IsCanceled := true;

  if (assigned(Thread)) then
  begin
    if not Thread.Finished then
    begin
      Thread.Terminate;
      Thread.WaitFor;
    end;
    FreeAndNil(Thread);
  end;
end;



procedure TWorker.Synchronize(AMethod: TThreadMethod);
begin
  Thread.Synchronize(AMethod);
end;

procedure TWorker.Synchronize(AThreadProc: TThreadProcedure);
begin
  Thread.Synchronize(AThreadProc);
end;

procedure TWorker.Task;
begin
  // override this method to add custom task code here.
end;



procedure TWorker.TaskFinished;
begin
  // do something when the task ends.
end;



{ TWorkerThread }

procedure TWorkerThread.Execute;
begin
  if not Terminated then
  begin
    if assigned(fTask) then fTask();
  end;
  ReturnValue := 1;
end;

end.
