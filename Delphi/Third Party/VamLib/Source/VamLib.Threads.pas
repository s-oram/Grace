unit VamLib.Threads;

interface

// DLL, Form and Thread (all in one) problem in delphi
// http://stackoverflow.com/q/5792573/395461

// CheckSynchronize().
// http://www.delphifaq.com/faq/delphi/vcl/f270.shtml





uses
  Classes,
  SysUtils,
  OtlParallel,
  OtlTaskControl;

// I wrote the RunTask() procedures before finding the OTL Async() procedure which
// does the same thing (but probably better). I've rewritten the RunTask() methods
// as wrappers around the Async() procedure.
procedure RunTask(aTask : TProc); overload;
procedure RunTask(aTask, FinishedCallback : TProc); overload;
procedure RunTask(aTask : TProc; aTaskStartDelay : integer; FinishedCallback : TProc); overload;

// Action's are executed in the main thead (usually GUI afaik).
procedure DelayedAction(const Delay : integer; Action : TProc);




type
  // TODO:MED I wonder if it would be possible to have TCustomMotile descend from
  // TThread instead of encapsulating the TThread behaviour.
  //

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

implementation

uses
  ExtCtrls,
  OtlTask;

procedure RunTask(aTask : TProc);
begin
  Async(aTask);
end;

procedure RunTask(aTask, FinishedCallback : TProc);
begin
  // NOTE: FinishedCallBack will always be executed in the same thread as the calling code. This makes
  // it useful for updating GUI components as it will be thread safe.

  Async(aTask).Await(FinishedCallback);
end;

procedure RunTask(aTask : TProc; aTaskStartDelay : integer; FinishedCallback : TProc); overload;
var
  Delegate : TProc;
begin
  // NOTE: FinishedCallBack will always be executed in the same thread as the calling code. This makes
  // it useful for updating GUI components as it will be thread safe.

  Delegate := procedure
  begin
    if aTaskStartDelay > 0
      then Sleep(aTaskStartDelay);
    aTask;
  end;

  if assigned(FinishedCallback)
    then Async(Delegate).Await(FinishedCallback)
    else Async(Delegate);
end;



procedure DelayedAction(const Delay : integer; Action : TProc);
var
  Delegate : TProc;
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

end.
