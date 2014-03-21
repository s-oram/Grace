unit VamLib.HighSpeedTimer;

interface

uses
  Classes,
  SysUtils,
  OtlCommon,
  OtlComm,
  OtlTaskControl,
  OtlTask,
  OtlParallel;

type
  THighSpeedTimer = class
  private
    fOnTimer: TNotifyEvent;
    fInterval: integer;
    fEnabled: boolean;

    IsTaskActive : boolean;
    TaskControl : IOmniTaskControl;
    Monitor : IOmniTaskControlMonitor;
    fUseMainThreadForTimerEvent: boolean;

    procedure SetEnabled(const Value: boolean);
    procedure SetInterval(const Value: integer);

    procedure TaskLoop(const task: IOmniTask);
    procedure TaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure HandleTaskTerminated(const task: IOmniTaskControl);

  public
    constructor Create;
    destructor Destroy; override;

    property Enabled  : boolean read fEnabled  write SetEnabled;
    property Interval : integer read fInterval write SetInterval;
    property UseMainThreadForTimerEvent : boolean read fUseMainThreadForTimerEvent write fUseMainThreadForTimerEvent;

    property OnTimer : TNotifyEvent read fOnTimer write fOnTimer;
  end;

implementation

const
  MsgDoTimerEvent = 1;

{ THighSpeedTimer }

constructor THighSpeedTimer.Create;
begin
  IsTaskActive := false;
  UseMainThreadForTimerEvent := true;
end;

destructor THighSpeedTimer.Destroy;
begin
  OnTimer := nil;

  if (IsTaskActive) then
  begin
    fEnabled := false;
    TaskControl.Terminate;
  end;

  TaskControl := nil;

  inherited;
end;

procedure THighSpeedTimer.SetInterval(const Value: integer);
begin
  fInterval := Value;
end;

procedure THighSpeedTimer.SetEnabled(const Value: boolean);
begin
  fEnabled := Value;

  if (fEnabled) and (not IsTaskActive) then
  begin
    // TODO: OmniThreadLibrary has a SetTimer() method. It might
    // be preferable to use that.
    IsTaskActive := true;
    TaskControl := CreateTask(TaskLoop);
    TaskControl.OnMessage(TaskMessage);
    TaskControl.OnTerminated(HandleTaskTerminated);
    TaskControl.Schedule;
  end;
end;

procedure THighSpeedTimer.TaskLoop(const task: IOmniTask);
begin
  while (fEnabled) and (not Task.Terminated) do
  begin
    Sleep(Interval);

    if fEnabled then
    begin
      if UseMainThreadForTimerEvent
        then Task.Comm.Send(MsgDoTimerEvent)
        else if assigned(OnTimer) then OnTimer(self);
    end;
  end;
end;

procedure THighSpeedTimer.HandleTaskTerminated(const task: IOmniTaskControl);
begin
  if Enabled then
  begin
    TaskControl := CreateTask(TaskLoop);
    TaskControl.OnMessage(TaskMessage);
    TaskControl.OnTerminated(HandleTaskTerminated);
    TaskControl.Run
  end else
  begin
    TaskControl := nil;
    IsTaskActive := false;
  end;
end;

procedure THighSpeedTimer.TaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  if Msg.MsgID = MsgDoTimerEvent then
  begin
    if assigned(OnTimer)
      then OnTimer(self);
  end;

end;

end.

