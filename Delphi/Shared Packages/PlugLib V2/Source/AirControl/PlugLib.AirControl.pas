unit PlugLib.AirControl;

interface

uses
  Classes,
  SyncObjs,
  PlugLib.Types,
  VamLib.UniqueID,
  AudioPlugin.Types,
  PlugLib.AirControl.DelayedTaskList,
  PlugLib.AirControl.TaskQueue,
  PlugLib.AirControl.TaskSyncQueue;

type
  TAirControl = class;
  TBackgroundWorker = class;

  // == TAirControl ==
  // Plugins are multi-threaded. Typically a plugin will be called from an Audio and
  // a GUI thread. All real-time audio processing is completed in the Audio thread,
  // as well as other non-GUI tasks such as state loading/saving etc.
  // Plugins also need to run time and compute intensive tasks in background threads
  // to avoid blocking the GUI and Audio threads.
  // AirControl is designed to help plugins manage their multi-threaded environment.
  // It provides a method to pass tasks off to a background worker thread. It
  // also provides methods to execute code in the GUI and Audio threads. Which
  // helps with synchronisation.
  //
  // IMPORTANT: Background tasks will always be executed. Outstanding Audio and GUI sync
  // tasks will be dropped if the plugin or GUI closes. Delayed tasks will also be
  // dropped.

  TAirControl = class
  strict private
    CS : TFixedCriticalSection;
    BgEventToken : TEvent;
    BackgroundWorker : TBackgroundWorker;
    DelayedTaskQueue : TDelayedTaskList;
    TaskSyncQueue    : TTaskSyncQueue;
    AudioSyncQueue   : TTaskQueue;
    GuiSyncQueue     : TTaskQueue;

    FCapacity: integer;
    FMaxCapacity: integer;
    FGrowBy: integer;
    FIsTerminated: boolean;

    FGlobalExceptionHander : TExceptionHandlerFunc;
  private
  protected
    property IsTerminated : boolean read FIsTerminated;
    property GlobalExceptionHander : TExceptionHandlerFunc read FGlobalExceptionHander;
  public
    constructor Create(const Capacity, GrowBy, MaxCapacity : integer; const GlobalExceptionHander : TExceptionHandlerFunc);
    destructor Destroy; override;

    procedure TerminateProcessingAndPrepareForShutDown;

    procedure ClearTaskQueue;
    procedure ClearGuiSyncQueue;
    procedure ClearAudioSyncQueue;

    function AddDelayedTask(const Task, OnAudioSync, OnGuiSync : TThreadProcedure; const ExecuteDelay : cardinal):TUniqueID;
    procedure CancelTask(const TaskID : TUniqueID); // NOTE: will not cancel task if it's already started. //Cancels a delayed task.

    procedure AddTask(const Task : TThreadProcedure); overload;
    function AddTask(const Task, OnAudioSync, OnGuiSync : TThreadProcedure):TUniqueID; overload;

    procedure GuiSync(const proc : TThreadProcedure);
    procedure AudioSync(const proc : TThreadProcedure);

    property Capacity    : integer read FCapacity;
    property GrowBy      : integer read FGrowBy;
    property MaxCapacity : integer read FMaxCapacity;

    procedure ProcessAudioSync; //The ProcessAudioSync() method needs to be reguarlly called from the audio thread.
    procedure ProcessGuiSync;   //The ProcessGuiSync() method needs to be reguarlly called from the GUI thread.
  end;

  TBackgroundWorker = class(TThread)
  strict private
    FAudioSyncQueue : PTaskQueue;
    FGuiSyncQueue   : PTaskQueue;
    FTaskSyncQueue  : PTaskSyncQueue;
    FDelayedTaskList: PDelayedTaskList;
    FSharedEvent    : TEvent;
    FAirControl     : TAirControl;
    procedure ProcessTasks;
  private
  protected
    procedure Execute; override;

    property AirControl            : TAirControl      read FAirControl           write FAirControl;
    property SharedEventToken      : TEvent           read FSharedEvent          write FSharedEvent;
    property TaskSyncQueue         : PTaskSyncQueue   read FTaskSyncQueue        write FTaskSyncQueue;
    property AudioSyncQueue        : PTaskQueue       read FAudioSyncQueue       write FAudioSyncQueue;
    property GuiSyncQueue          : PTaskQueue       read FGuiSyncQueue         write FGuiSyncQueue;
    property DelayedTaskList       : PDelayedTaskList read FDelayedTaskList      write FDelayedTaskList;
  public
  end;

implementation

uses
  SysUtils,
  WinApi.Windows;

{ TAirControl }

constructor TAirControl.Create(const Capacity, GrowBy, MaxCapacity : integer; const GlobalExceptionHander : TExceptionHandlerFunc);
begin
  FGlobalExceptionHander := GlobalExceptionHander;
  FIsTerminated := false;

  FCapacity := Capacity;
  FGrowBy := GrowBy;
  FMaxCapacity := MaxCapacity;

  DelayedTaskQueue := TDelayedTaskList.Create(Capacity, GrowBy, MaxCapacity);
  TaskSyncQueue    := TTaskSyncQueue.Create(Capacity, GrowBy, MaxCapacity);
  AudioSyncQueue   := TTaskQueue.Create(Capacity, GrowBy, MaxCapacity);
  GuiSyncQueue     := TTaskQueue.Create(Capacity, GrowBy, MaxCapacity);

  CS := TFixedCriticalSection.Create;

  BgEventToken := TEvent.Create(nil, true, false, '', false);
  BgEventToken.ResetEvent;

  // == Ordering is important! ==
  // 1) Create the background worker, and configure shared variables.
  BackgroundWorker := TBackgroundWorker.Create(true);
  BackgroundWorker.FreeOnTerminate  := false;
  BackgroundWorker.AirControl       := self;
  BackgroundWorker.TaskSyncQueue    := @self.TaskSyncQueue;
  BackgroundWorker.AudioSyncQueue   := @self.AudioSyncQueue;
  BackgroundWorker.GuiSyncQueue     := @self.GuiSyncQueue;
  BackgroundWorker.DelayedTaskList  := @self.DelayedTaskQueue;
  BackgroundWorker.SharedEventToken := self.BgEventToken;

  // 2) Start the worker...
  BackgroundWorker.Start;
end;

destructor TAirControl.Destroy;
begin
  if assigned(BackgroundWorker)
    then TerminateProcessingAndPrepareForShutDown;

  cs.Enter;
  try
    // Normally FreeAndNil() isn't used in a destructor. But this class will
    // be getting used in a multi-threaded context. Locking and freeing to nil
    // might help avoid a bug in the long run. (Although hopefully the design
    // is a bit better than that in the first place!)
    FreeAndNil(DelayedTaskQueue);
    FreeAndNil(TaskSyncQueue);
    FreeAndNil(AudioSyncQueue);
    FreeAndNil(GuiSyncQueue);

    FreeAndNil(BgEventToken);
  finally
    cs.Leave;
  end;
  cs.Free;

  inherited;
end;

procedure TAirControl.TerminateProcessingAndPrepareForShutDown;
begin
  cs.Enter;
  try
    FIsTerminated := true;

    // shut down the background worker.
    BackgroundWorker.Terminate;
    BgEventToken.SetEvent;
    BackgroundWorker.WaitFor;
    FreeAndNil(BackgroundWorker);
    //=======================
  finally
    cs.Leave;
  end;

  // clear all outstanding background tasks.
  ClearTaskQueue;
  ClearGuiSyncQueue;
  ClearAudioSyncQueue;
end;


procedure TAirControl.AudioSync(const proc: TThreadProcedure);
begin
  cs.Enter;
  try
    if IsTerminated then raise EPlugLibException.Create('AirControl processing has already been terminated.');
    AudioSyncQueue.Push(proc);
  finally
    cs.Leave;
  end;
end;

procedure TAirControl.GuiSync(const proc: TThreadProcedure);
begin
  cs.Enter;
  try
    if IsTerminated then raise EPlugLibException.Create('AirControl processing has already been terminated.');
    GuiSyncQueue.Push(proc);
  finally
    cs.Leave;
  end;
end;

function TAirControl.AddDelayedTask(const Task, OnAudioSync, OnGuiSync: TThreadProcedure; const ExecuteDelay: cardinal): TUniqueID;
begin
  cs.Enter;
  try
    if IsTerminated then raise EPlugLibException.Create('AirControl processing has already been terminated.');
    result := DelayedTaskQueue.Push(Task, OnAudioSync, OnGuiSync, ExecuteDelay);
  finally
    cs.Leave;
  end;
  BgEventToken.SetEvent; // Set the event so the background thread will start processing.
end;

function TAirControl.AddTask(const Task, OnAudioSync, OnGuiSync: TThreadProcedure): TUniqueID;
begin
  cs.Enter;
  try
    if IsTerminated then raise EPlugLibException.Create('AirControl processing has already been terminated.');
    result := TaskSyncQueue.Push(Task, OnAudioSync, OnGuiSync);
  finally
    cs.Leave;
  end;
  BgEventToken.SetEvent; // Set the event so the background thread will start processing.
end;

procedure TAirControl.AddTask(const Task: TThreadProcedure);
begin
  AddTask(Task, nil, nil);
end;

procedure TAirControl.CancelTask(const TaskID: TUniqueID);
begin
  cs.Enter;
  try
    DelayedTaskQueue.CancelTask(TaskID);
  finally
    cs.Leave;
  end;
end;

procedure TAirControl.ClearAudioSyncQueue;
var
  Task : TThreadProcedure;
begin
  cs.Enter;
  try
    while AudioSyncQueue.Pop(Task) do
    begin
      // do nothing.
    end;
  finally
    cs.Leave;
  end;
end;

procedure TAirControl.ClearGuiSyncQueue;
var
  Task : TThreadProcedure;
begin
  cs.Enter;
  try
    while GuiSyncQueue.Pop(Task) do
    begin
      // do nothing.
    end;
  finally
    cs.Leave;
  end;
end;

procedure TAirControl.ClearTaskQueue;
var
  Task, OnAudioSync, OnGuiSync : TThreadProcedure;
begin
  cs.Enter;
  try
    while TaskSyncQueue.Pop(Task, OnAudioSync, OnGuiSync) do
    begin
      // do nothing.
    end;
  finally
    cs.Leave;
  end;
end;

procedure TAirControl.ProcessAudioSync;
var
  Task : TThreadProcedure;
begin
  while AudioSyncQueue.Pop(Task) do
  begin
    Task();
  end;
end;

procedure TAirControl.ProcessGuiSync;
var
  Task : TThreadProcedure;
begin
  while GuiSyncQueue.Pop(Task) do
  begin
    Task();
  end;
end;

{ TBackgroundThread }

procedure TBackgroundWorker.Execute;
var
  WaitResult : TWaitResult;
  FExceptAddr : pointer;
  FException  : Exception;
begin
  assert(assigned(SharedEventToken));
  assert(assigned(AirControl));

  WaitResult := TWaitResult.wrSignaled;

  while not Terminated do
  begin
    try
      if Terminated then exit;

      case WaitResult of
        wrSignaled, wrTimeout:
        begin
          ProcessTasks;
        end;
        wrAbandoned,
        wrError,
        wrIOCompletion:
        begin
          // NOTE: I'm not really sure what should happen here. I don't think any of these
          // WaitResults should be returned in normal operation. For now just do nothing.
          // But consider throwing an exception here if debugging the background worker and
          // it's quitting mysteriously.
        end
      else
        raise EPlugLibException.Create('Unexpected wait result.');
      end;

      if DelayedTaskList^.Count > 0
        then WaitResult := SharedEventToken.WaitFor(100)   //Wait for 100 milliseconds.
        else WaitResult := SharedEventToken.WaitFor(2000); //wait for 2 seconds.
    except
      on E: Exception do
      begin
        // NOTE: It's vastly preferable for the exception to be handled by the global exception handler.
        // If the exception isn't handled, try to raise it in the audio thread. I think that should
        // give the best chance of the expection being seen and fixed.
        if (assigned(AirControl.GlobalExceptionHander)) and (AirControl.GlobalExceptionHander() = true) then
        begin
          // do nothing. Exception has been handled.
        end else
        begin
          // Implementation based on answer at StackOverflow.
          // http://stackoverflow.com/a/5443127/395461

          FExceptAddr := ExceptAddr;
          FException  := AcquireExceptionObject;

          // Raise the exception in the main audio thread so that it get's seen.
          AirControl.AudioSync(
          procedure
          begin
            raise FException at FExceptAddr;
          end
          );
        end;
      end;
    end;
  end;
end;

procedure TBackgroundWorker.ProcessTasks;
var
  Task, OnAudioSync, OnGuiSync : TThreadProcedure;
begin
  while DelayedTaskList^.Pop(Task, OnAudioSync, OnGuiSync) do
  begin
    // IMPORTANT: Ordering is important here.

    // 1) Complete the task...
    if assigned(Task)
      then Task();

    // 2) Then push the OnSync tasks to their respective queues.
    if assigned(OnAudioSync)
      then AudioSyncQueue^.Push(OnAudioSync);

    if assigned(OnGuiSync)
      then GuiSyncQueue^.Push(OnGuiSync);
  end;


  while TaskSyncQueue^.Pop(Task, OnAudioSync, OnGuiSync) do
  begin
    // IMPORTANT: Ordering is important here.

    // 1) Complete the task...
    if assigned(Task)
      then Task();

    // 2) Then push the OnSync tasks to their respective queues.
    if assigned(OnAudioSync)
      then AudioSyncQueue^.Push(OnAudioSync);

    if assigned(OnGuiSync)
      then GuiSyncQueue^.Push(OnGuiSync);
  end;
end;

end.
