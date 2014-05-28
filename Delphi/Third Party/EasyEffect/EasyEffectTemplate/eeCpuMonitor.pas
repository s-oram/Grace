unit eeCpuMonitor;

interface

{$INCLUDE Defines.inc}

type
  TCpuMonitor = class
  private type
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

procedure TCpuMonitor.StartProcessReplacingTimer(aSampleFrames, aSampleRate : double);
var
  freq:Int64;
begin
  ProcessReplacingData.SampleFrames := aSampleFrames;
  ProcessReplacingData.SampleRate   := aSampleRate;
  QueryPerformanceCounter(ProcessReplacingData.StartTime);
  //QueryPerformanceFrequency(freq);
end;

procedure TCpuMonitor.StopProcessingReplacingTimer;
var
  pc:Int64;
  freq:Int64;
  xLoad : single;
  {$IFDEF Logging}s : string;{$ENDIF}
begin
  QueryPerformanceCounter(pc);
  QueryPerformanceFrequency(freq);
  ProcessReplacingData.ProcessTime := (pc - ProcessReplacingData.StartTime) * 1000 / freq;

  // Approximately calculate CPU load as a ratio of maximum execution time before audio is slower then real-time.
  xLoad := ProcessReplacingData.ProcessTime / (ProcessReplacingData.SampleFrames / ProcessReplacingData.SampleRate * 1000);
  // Convert to percentage.
  xLoad := xLoad * 100;
  ProcessReplacingData.ProcessLoad := xLoad;

  {$IFDEF Logging}
  if xLoad >= 50 then
  begin
    s := IntToStr(round(xLoad));
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
