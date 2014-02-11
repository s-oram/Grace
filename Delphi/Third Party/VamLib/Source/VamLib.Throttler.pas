unit VamLib.Throttler;

interface

uses
  ExtCtrls,
  SysUtils,
  VamLib.Collections.Lists;


//==============================================================================
//        high level global functions.
//==============================================================================
procedure InitGlobalThrottler;
procedure FreeGlobalThrottler;

function GetThrottleHandle : cardinal;

procedure Throttle(const Handle : cardinal; Time: integer; Task : TProc);


//==============================================================================
//        Low level type declarations
//==============================================================================
type
  PThrottleInfo = ^TThrottleInfo;
  TThrottleInfo = record
    Handle     : cardinal;
    Task       : TProc;
    HoldTime   : cardinal; //The desired throttled time delay.
    TimeCalled : TDateTime;
    IsExpired  : boolean;
  end;

  TTaskList = class(TSimpleRecordList<TThrottleInfo>)
  public
    function FindOrCreate(ID : cardinal):PThrottleInfo;
    function Find(ID : cardinal):PThrottleInfo;
  end;

  TThrottler = class
  private
    TaskList : TTaskList;
    Timer : TTimer;
    procedure HandleTimerEvent(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Throttle(const Handle : cardinal; Time: integer; Task : TProc);
  end;


implementation

uses
  DateUtils;

var
  GlobalThrottleHandleCount : cardinal;
  GlobalThrottler : TThrottler;

function GetThrottleHandle : cardinal;
begin
  //TODO: Ideally there should be some sort of mutex here to
  // prevent multiple threads from calling
  // GetThrottleHandle() at the same time.
  result := GlobalThrottleHandleCount;
  inc(GlobalThrottleHandleCount);
end;

procedure InitGlobalThrottler;
begin
  if not assigned(GlobalThrottler)
    then GlobalThrottler := TThrottler.Create;
end;

procedure FreeGlobalThrottler;
begin
  FreeAndNil(GlobalThrottler);
end;

procedure Throttle(const Handle : cardinal; Time: integer; Task : TProc);
begin
  assert(assigned(GlobalThrottler));
  GlobalThrottler.Throttle(Handle, Time, Task);
end;

{ TThrottler }

constructor TThrottler.Create;
begin
  TaskList := TTaskList.Create;
  Timer := TTimer.Create(nil);
  Timer.Interval := 1;
  Timer.OnTimer := HandleTimerEvent;
end;

destructor TThrottler.Destroy;
begin
  TaskList.Free;
  Timer.Free;
  inherited;
end;

procedure TThrottler.HandleTimerEvent(Sender: TObject);
var
  c1: Integer;
  TaskInfo : PThrottleInfo;
  ms : Int64;
begin
  for c1 := TaskList.Count-1 downto 0 do
  begin
    TaskInfo := @TaskList.Raw[c1];

    if TaskInfo^.IsExpired then
    begin
      TaskList.Delete(c1);
    end else
    begin
      ms := MilliSecondsBetween(Now, TaskInfo^.TimeCalled);
      if (ms >= TaskInfo^.HoldTime)  then
      begin
        TaskInfo^.Task();
        TaskInfo^.TimeCalled := Now;
        TaskInfo^.IsExpired := true;
      end;
    end;
  end;

  if TaskList.Count = 0
    then Timer.Enabled := false;

end;

procedure TThrottler.Throttle(const Handle: cardinal; Time: integer; Task: TProc);
var
  TaskInfo : PThrottleInfo;
begin
  TaskInfo := TaskList.Find(Handle);
  if not assigned(TaskInfo) then
  begin
    Task();

    TaskInfo := TaskList.FindOrCreate(Handle);
    TaskInfo.IsExpired  := true;
    TaskInfo.Task       := Task;
    TaskInfo.HoldTime   := Time;
    TaskInfo.TimeCalled := Now;
    Timer.Enabled       := true;
  end else
  begin
    TaskInfo.IsExpired := false;
  end;
end;

{ TTaskList }

function TTaskList.Find(ID: cardinal): PThrottleInfo;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if self[c1].Handle = ID
      then exit(@self.Raw[c1]);
  end;

  result := nil;
end;

function TTaskList.FindOrCreate(ID: cardinal): PThrottleInfo;
var
  c1: Integer;
  ptr : PThrottleInfo;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if self[c1].Handle = ID
      then exit(@self.Raw[c1]);
  end;

  ptr := New;
  ptr^.Handle := ID;
  result := ptr;
end;

initialization
  GlobalThrottleHandleCount := 0;
finalization
  if assigned(GlobalThrottler)
    then GlobalThrottler.Free;
end.
