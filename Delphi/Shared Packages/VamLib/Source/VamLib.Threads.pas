unit VamLib.Threads;

interface

// DLL, Form and Thread (all in one) problem in delphi
// http://stackoverflow.com/q/5792573/395461

// CheckSynchronize().
// http://www.delphifaq.com/faq/delphi/vcl/f270.shtml

uses
  Classes,
  SysUtils;


procedure RunTask(aTask, FinishedCallback : TThreadProcedure);

// Action's are executed in the main thead (usually GUI afaik).
// TODO:HIGH there probably needs to be a way to ensure the delayed action always works.
// I think there is a real chance of AV errors happening if the action is slow and
// the GUI is closed.
procedure DelayedAction(const Delay : integer; Action : TThreadProcedure);


type
  //===== forward declarations ========
  TCustomMotile = class;
  TMotileThread = class;
  //===================================

  // TCustomMotile is a base class for creating "threaded tasks". The custom motile
  // descendent will represent a specific task that will be run in a seperate thread.
  //
  // Compared to TThread, TCustomMotile has a more specific intended role. TCustomMotile
  // represents a task. TThread encapsulates a "thread".
  //
  // TCustomMotile contains methods for synchronsing with the main application thread.
  TCustomMotile = class
  private
    Thread : TMotileThread;
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

    procedure Run;    //start the task. Not blocking.
    procedure Cancel; //cancel. Not blocking.
    procedure Stop;   //cancel and wait. Blocking.
    procedure Sync;   //wait until finished. Blocking.
  end;


  //==== TMotileThread is private and for internal usage only. ========
  TMotileThread = class(TThread)
  private
    fTask: TProc;
  protected
    procedure Execute; override;
  public
    property Task : TProc read fTask write fTask;
  end;

  //===== forward declarations ========
  TAsyncTask = class;
  TAsyncTaskThread = class;
  IAwait = interface;
  //===================================

  //==== private and for internal usage only. ========
  IAwait = interface
    procedure Run;
    procedure Await(proc: TThreadProcedure);
  end; { IOmniAwait }

  //==== private and for internal usage only. ========
  TAsyncTask = class(TInterfacedObject, IAwait)
  private
  protected
     AsyncThread : TAsyncTaskThread;
  public
    constructor Create(aTask: TThreadProcedure);
    destructor Destroy; override;

    procedure Await(proc: TThreadProcedure);
    procedure Run;
  end;

  //==== private and for internal usage only. ========
  TAsyncTaskThread = class(TThread)
  private
  protected
    FTask     : TThreadProcedure;
    FSyncTask : TThreadProcedure;
    procedure Execute; override;
  public
    destructor Destroy; override;
  end;

function Async(Task: TThreadProcedure): IAwait;

implementation

uses
  ExtCtrls;

procedure RunTask(aTask, FinishedCallback : TThreadProcedure);
begin
  // NOTE: FinishedCallBack will always be executed in the same thread as the calling code. This makes
  // it useful for updating GUI components as it will be thread safe.

  Async(aTask).Await(FinishedCallback);
end;


procedure DelayedAction(const Delay : integer; Action : TThreadProcedure);
var
  Delegate : TThreadProcedure;
begin
  Delegate := procedure
  begin
    Sleep(Delay);
  end;

  Async(Delegate).Await(Action);
end;





{ TWorker }

constructor TCustomMotile.Create;
begin

end;

destructor TCustomMotile.Destroy;
begin
  Stop;
  inherited;
end;

procedure TCustomMotile.EventHandle_ThreadTerminated(Sender: TObject);
begin
  TaskFinished;
end;

procedure TCustomMotile.Queue(AMethod: TThreadMethod);
begin
  Thread.Queue(AMethod);
end;

procedure TCustomMotile.Queue(AThreadProc: TThreadProcedure);
begin
  Thread.Queue(AThreadProc);
end;

procedure TCustomMotile.Run;
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
    Thread := TMotileThread.Create(true);
    Thread.FreeOnTerminate := false;
    Thread.OnTerminate := self.EventHandle_ThreadTerminated;
  end;

  Thread.Task := Task;
  Thread.Start;
end;

procedure TCustomMotile.Cancel;
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

procedure TCustomMotile.Stop;
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



procedure TCustomMotile.Synchronize(AMethod: TThreadMethod);
begin
  Thread.Synchronize(AMethod);
end;

procedure TCustomMotile.Sync;
begin
  //TODO:HIGH
  assert(false, 'todo');
end;

procedure TCustomMotile.Synchronize(AThreadProc: TThreadProcedure);
begin
  Thread.Synchronize(AThreadProc);
end;

procedure TCustomMotile.Task;
begin
  // override this method to add custom task code here.
end;



procedure TCustomMotile.TaskFinished;
begin
  // do something when the task ends.
end;



{ TWorkerThread }

procedure TMotileThread.Execute;
begin
  if not Terminated then
  begin
    if assigned(fTask) then fTask();
  end;
  ReturnValue := 1;
end;

{ TAsyncTask }

constructor TAsyncTask.Create(aTask: TThreadProcedure);
begin
  AsyncThread := TAsyncTaskThread.Create(true);
  AsyncThread.FreeOnTerminate := true;
  AsyncThread.FTask := aTask;
end;

destructor TAsyncTask.Destroy;
begin
  if assigned(AsyncThread)
    then AsyncThread.Free;

  inherited;
end;

procedure TAsyncTask.Await(proc: TThreadProcedure);
begin
  AsyncThread.FSyncTask := proc;
  AsyncThread.Start;
  AsyncThread := nil;
end;

procedure TAsyncTask.Run;
begin
  AsyncThread.Start;
  AsyncThread := nil;
end;

{ TAsyncTaskThread }

destructor TAsyncTaskThread.Destroy;
begin
  FTask := nil;
  FSyncTask := nil;
  inherited;
end;

procedure TAsyncTaskThread.Execute;
begin
  inherited;
  if assigned(FTask)
    then FTask();
  if assigned(FSyncTask)
    then Synchronize(FSyncTask);
end;


function Async(Task: TThreadProcedure): IAwait;
begin
  result := TAsyncTask.Create(Task);
end;



end.
