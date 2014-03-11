unit VamLib.Threads;

interface

uses
  SysUtils,
  OtlParallel,
  OtlTaskControl;


// I wrote the RunTask() procedures before finding the OTL Async() procedure which
// does the same thing (but probably better). I've rewritten the RunTask() methods
// as wrappers around the Async() procedure.
procedure RunTask(aTask : TProc); overload;
procedure RunTask(aTask, FinishedCallback : TProc); overload;
procedure RunTask(aTask : TProc; aTaskStartDelay : integer; FinishedCallback : TProc); overload;

procedure DelayedGuiAction(const Delay : integer; Action : TProc);

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



procedure DelayedGuiAction(const Delay : integer; Action : TProc);
var
  Delegate : TProc;
begin
  Delegate := procedure
  begin
    Sleep(Delay);
  end;

  Async(Delegate).Await(Action);
end;

end.
