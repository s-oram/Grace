unit VamLib.Threads;

interface

uses
  SysUtils;

procedure RunTask(aTask : TProc); overload;

//FinishedCallBack will always be executed in the same thread as the calling code. This makes
// it useful for updating GUI components as it will be thread safe.
procedure RunTask(aTask, FinishedCallback : TProc); overload;

procedure RunTask(aTask : TProc; aTaskStartDelay : integer; FinishedCallback : TProc); overload;

implementation

uses
  OtlTask, OtlTaskControl;

procedure RunTask(aTask : TProc);
var
  Delgate : TOmniTaskDelegate;
begin
  Delgate := procedure(const task: IOmniTask)
  begin
    aTask;
  end;
  TOmniTaskControl.Create(Delgate, '').Unobserved.Run;
end;

procedure RunTask(aTask, FinishedCallback : TProc);
var
  TaskControl : TOmniTaskControl;
  Delgate : TOmniTaskDelegate;
  OnFinishCB : TOmniOnTerminatedFunctionSimple;
begin
  Delgate := procedure(const task: IOmniTask)
  begin
    aTask;
  end;

  TaskControl := TOmniTaskControl.Create(Delgate, '');

  if assigned(FinishedCallback) then
  begin
    OnFinishCB := procedure
    begin
      FinishedCallback;
    end;

    TaskControl.OnTerminated(OnFinishCB);
  end;

  TaskControl.Unobserved.Run;
end;


procedure RunTask(aTask : TProc; aTaskStartDelay : integer; FinishedCallback : TProc); overload;
var
  TaskControl : TOmniTaskControl;
  Delgate : TOmniTaskDelegate;
  OnFinishCB : TOmniOnTerminatedFunctionSimple;
begin
  Delgate := procedure(const task: IOmniTask)
  begin
    if aTaskStartDelay > 0
      then Sleep(aTaskStartDelay);
    aTask;
  end;

  TaskControl := TOmniTaskControl.Create(Delgate, '');

  if assigned(FinishedCallback) then
  begin
    OnFinishCB := procedure
    begin
      FinishedCallback;
    end;

    TaskControl.OnTerminated(OnFinishCB);
  end;

  TaskControl.Unobserved.Run;
end;

end.
