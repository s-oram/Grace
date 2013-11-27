unit eeThreadv2.Thread;

interface

uses
  SysUtils, WinApi.Windows, System.Classes,
  eeThreadv2.Sync;

type
  TProcedureOfObject = procedure of object;
  TeeTaskProcedure = reference to procedure(Data:Pointer; DoQuitTask:PBoolean);

  TeeThread = class
  private
    FHandle: THandle platform;
    FThreadID: TThreadID;
    fIsTerminated: boolean;
    fThreadSleepTimeOut: integer;
    FFatalException: TObject;
    fLock: TeeCriticalSection;
  protected
    fTerminate   : boolean;
    fDoQuitTask  : boolean;
    NextTaskProc : TeeTaskProcedure;
    NextTaskData : Pointer;
    class procedure Sleep(TimeOut:integer);
    property Lock : TeeCriticalSection read fLock write fLock;
    procedure Terminate;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(aTask:TeeTaskProcedure; aTaskData:Pointer; InterruptCurrentTask:boolean = false);
    procedure ExecuteAutonomously(aTask:TProcedureOfObject;  InterruptCurrentTask:boolean = false);
    procedure QuitTask;

    property IsTerminated   : boolean read fIsTerminated;
    property FatalException : TObject read FFatalException;
  end;




implementation

uses
  System.RTLConsts;

function ThreadFunc(Thread: TeeThread): Integer;
var
  CurTaskProc : TeeTaskProcedure;
  CurTaskData : Pointer;
begin
  Result := 0;

  while Thread.fTerminate = false do
  begin
    //== Get the next task ==
    try
      Thread.Lock.Enter;
      CurTaskProc := Thread.NextTaskProc;
      CurTaskData := Thread.NextTaskData;
      Thread.NextTaskProc := nil;
      Thread.NextTaskData := nil;
      Thread.fDoQuitTask  := false;
    finally
      Thread.Lock.Leave;
    end;

    //== Perform the next task ==
    try
      if assigned(CurTaskProc) then CurTaskProc(CurTaskData, @Thread.fDoQuitTask);
    except
      Thread.FFatalException := AcquireExceptionObject;
      Break;
    end;

    //== check if the thread needs to terminate, else go to sleep ==
    if Thread.fTerminate = false
      then Sleep(10)
      else Break;
  end;

  //=== Finish Up =====
  Thread.fIsTerminated := true;
end;

{ TThread }

constructor TeeThread.Create;
begin
  Lock := TeeCriticalSection.Create;

  fDoQuitTask := false;

  NextTaskProc := nil;
  NextTaskData := nil;

  fTerminate    := false;
  fIsTerminated := false;

  FHandle := BeginThread(nil, 0, @ThreadFunc, Pointer(Self), CREATE_SUSPENDED, FThreadID);
  if FHandle = 0 then
    raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(GetLastError)]);

  ResumeThread(fHandle);
end;

destructor TeeThread.Destroy;
begin
  fDoQuitTask := true;
  Terminate;
  while IsTerminated = false do
  begin
    Sleep(10);
  end;
  Lock.Free;
  inherited;
end;

procedure TeeThread.Execute(aTask:TeeTaskProcedure; aTaskData:Pointer; InterruptCurrentTask:boolean = false);
begin
  Lock.Enter;
  try
    if InterruptCurrentTask then fDoQuitTask := true;
    NextTaskProc := aTask;
    NextTaskData := aTaskData;
  finally
    Lock.Leave;
  end;
end;

procedure TeeThread.ExecuteAutonomously(aTask:TProcedureOfObject; InterruptCurrentTask:boolean = false);
var
  AutonomousTaskHandler : TeeTaskProcedure;
begin
  AutonomousTaskHandler := procedure(Data: Pointer; DoQuitTask: PBoolean)
                           begin
                             aTask;
                           end;
  Lock.Enter;
  try
    if InterruptCurrentTask then fDoQuitTask := true;
    NextTaskProc := AutonomousTaskHandler;
    NextTaskData := nil;
  finally
    Lock.Leave;
  end;
end;



procedure TeeThread.QuitTask;
begin
  Lock.Enter;
  try
    fDoQuitTask  := true;
    NextTaskProc := nil;
    NextTaskData := nil;
  finally
    Lock.Leave;
  end;
end;

procedure TeeThread.Terminate;
begin
  fTerminate := true;
end;

class procedure TeeThread.Sleep(TimeOut: integer);
begin
  Winapi.Windows.Sleep(Timeout);
end;






end.
