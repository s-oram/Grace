unit VamLib.Throttler;

interface

uses
  ExtCtrls,
  SysUtils,
  VamLib.HighSpeedTimer,
  VamLib.UniqueID,
  VamLib.Collections.Lists;


//==============================================================================
//        high level global functions.
//==============================================================================
procedure Throttle(const Handle : TUniqueID; Time: integer; Task : TProc);

// TODO:HIGH need a clear throttle method.


//==============================================================================
//        Low level type declarations
//==============================================================================
type
  PThrottleInfo = ^TThrottleInfo;
  TThrottleInfo = record
    Handle     : TUniqueID;
    Task       : TProc;
    HoldTime   : cardinal; //The desired throttled time delay.
    TimeCalled : TDateTime;
    IsExpired  : boolean;
  end;

  TInfoList = class(TSimpleRecordList<TThrottleInfo>)
  public
    function FindOrCreate(ID : TUniqueID):PThrottleInfo;
    function Find(ID : TUniqueID):PThrottleInfo;
  end;

  TThrottleController = class
  private
    TaskList : TInfoList;
    //TODO: Use high speed timer here.
    Timer : THighSpeedTimer;
    procedure HandleTimerEvent(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Throttle(const Handle : TUniqueID; Time: integer; Task : TProc);
  end;


implementation

uses
  VamLib.LoggingProxy,
  DateUtils;

var
  GlobalThrottler  : TThrottleController;

procedure Throttle(const Handle : TUniqueID; Time: integer; Task : TProc);
begin
  if not assigned(GlobalThrottler) then GlobalThrottler := TThrottleController.Create;
  GlobalThrottler.Throttle(Handle, Time, Task);
end;

{ TThrottler }

constructor TThrottleController.Create;
begin
  TaskList := TInfoList.Create;
  Timer := THighSpeedTimer.Create;
  Timer.UseMainThreadForTimerEvent := false;
  Timer.Interval := 5;
  Timer.OnTimer := HandleTimerEvent;
  Timer.Enabled       := true;
end;

destructor TThrottleController.Destroy;
begin
  Timer.Free;
  TaskList.Free;
  inherited;
end;

procedure TThrottleController.HandleTimerEvent(Sender: TObject);
var
  c1: Integer;
  TaskInfo : PThrottleInfo;
  ms : Int64;
begin
  Log.LogMessage('Throttle Timer Event');

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

  //if TaskList.Count = 0
  //  then Timer.Enabled := false;

end;

procedure TThrottleController.Throttle(const Handle: TUniqueID; Time: integer; Task: TProc);
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
  end else
  begin
    TaskInfo.IsExpired := false;
    //Timer.Enabled := true;
  end;
end;

{ TTaskList }

function TInfoList.Find(ID: TUniqueID): PThrottleInfo;
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

function TInfoList.FindOrCreate(ID: TUniqueID): PThrottleInfo;
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

finalization
  if assigned(GlobalThrottler)
    then GlobalThrottler.Free;
end.
