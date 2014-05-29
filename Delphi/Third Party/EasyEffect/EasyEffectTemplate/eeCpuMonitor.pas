unit eeCpuMonitor;

interface

{$INCLUDE Defines.inc}

type
  TCpuMonitor = class
  private type
    PTimerData = ^TTimerData;
    TTimerData = record
      StartTime : Int64;
      ProcessTime : double; // time in milliseconds.
      ProcessLoad : double; // 0..100% of max load.
      SampleFrames, SampleRate : double;
    end;
  private
    function GetProcessReplacingLoad: double;
    function GetProcessReplacingTime: double;
  protected
    ProcessReplacingData : TTimerData;

    procedure CalculateCpuTimeAndLoad(const TimerData : PTimerData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartVstEventTimer;
    procedure StopVstEventTimer;

    procedure StartProcessReplacingTimer(aSampleFrames, aSampleRate : double);
    procedure StopProcessingReplacingTimer;

    property ProcessReplacingTime : double read GetProcessReplacingTime; // milliseconds
    property ProcessReplacingLoad : double read GetProcessReplacingLoad; // 0..100 (percentage of max load)
  end;

implementation

uses
  SysUtils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  Windows;

{ TCpuMonitor }

constructor TCpuMonitor.Create;
begin
  ProcessReplacingData.ProcessTime := 0;
end;

destructor TCpuMonitor.Destroy;
begin

  inherited;
end;

function TCpuMonitor.GetProcessReplacingLoad: double;
begin
  result := ProcessReplacingData.ProcessLoad;
end;

function TCpuMonitor.GetProcessReplacingTime: double;
begin
  result := ProcessReplacingData.ProcessTime;
end;

procedure TCpuMonitor.CalculateCpuTimeAndLoad(const TimerData: PTimerData);
var
  pc:Int64;
  freq:Int64;
  xLoad : single;
begin
  QueryPerformanceCounter(pc);
  QueryPerformanceFrequency(freq);
  TimerData^.ProcessTime := (pc - TimerData^.StartTime) * 1000 / freq;

  // Approximately calculate CPU load as a ratio of maximum execution time before audio is slower then real-time.
  xLoad := TimerData^.ProcessTime / (TimerData^.SampleFrames / TimerData^.SampleRate * 1000);
  // Convert to percentage.
  xLoad := xLoad * 100;
  TimerData^.ProcessLoad := xLoad;
end;

procedure TCpuMonitor.StartProcessReplacingTimer(aSampleFrames, aSampleRate : double);
var
  freq:Int64;
begin
  ProcessReplacingData.SampleFrames := aSampleFrames;
  ProcessReplacingData.SampleRate   := aSampleRate;
  QueryPerformanceCounter(ProcessReplacingData.StartTime);
end;

procedure TCpuMonitor.StopProcessingReplacingTimer;
var
  TimerData : PTimerData;
  {$IFDEF Logging}s : string;{$ENDIF}
begin
  TimerData := @ProcessReplacingData;

  CalculateCpuTimeAndLoad(TimerData);

  {$IFDEF Logging}
  if TimerData.ProcessLoad >= 100 then
  begin
    s := IntToStr(round(TimerData.ProcessLoad));
    LogMain.LogError('Processing Replacing Load is ' + s + '%');
  end;
  {$ENDIF}
end;

procedure TCpuMonitor.StartVstEventTimer;
begin

end;

procedure TCpuMonitor.StopVstEventTimer;
begin

end;

end.
