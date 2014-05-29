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
    LogCount : integer;
    ProcessReplacingData : TTimerData;
    AudioProcess2Data : TTimerData;
    VstEventData : TTimerData;

    procedure CalculateCpuTimeAndLoad(const TimerData : PTimerData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartVstEventTimer;
    procedure StopVstEventTimer;

    procedure StartProcessReplacingTimer(aSampleFrames, aSampleRate : double);
    procedure StopProcessingReplacingTimer;

    procedure StartAudioProcessTimer2;
    procedure StopAudioProcessTimer2;

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
  AudioProcess2Data.ProcessTime := 0;

  VstEventData.ProcessTime := 0;
  // Sampleframes and samplerate values aren't set by the plugin when
  // processing VST Events. However we'll use these values as a standin
  // when calculating the load, because the loading is what's important. We
  // don't want to overload the host process method.
  VstEventData.SampleFrames := 64;
  VstEventData.SampleRate   := 44100;

  LogCount := 0;
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
begin
  AudioProcess2Data.SampleFrames := aSampleFrames;
  AudioProcess2Data.SampleRate   := aSampleRate;

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
  if ProcessReplacingData.ProcessLoad >= 100 then
  begin
    s := IntToStr(round(ProcessReplacingData.ProcessLoad));
    LogMain.LogError('Processing Replacing Load is ' + s + '%');
  end;
  {$ENDIF}
end;

procedure TCpuMonitor.StartVstEventTimer;
begin
  QueryPerformanceCounter(VstEventData.StartTime);
end;

procedure TCpuMonitor.StopVstEventTimer;
var
  TimerData : PTimerData;
  {$IFDEF Logging}s : string;{$ENDIF}
begin
  TimerData := @VstEventData;

  CalculateCpuTimeAndLoad(TimerData);

  {$IFDEF Logging}
  if VstEventData.ProcessLoad >= 100 then
  begin
    s := IntToStr(round(VstEventData.ProcessLoad));
    LogMain.LogError('VST Event Load is ' + s + '%');
  end;
  {$ENDIF}
end;

procedure TCpuMonitor.StartAudioProcessTimer2;
begin
  QueryPerformanceCounter(AudioProcess2Data.StartTime);
end;

procedure TCpuMonitor.StopAudioProcessTimer2;
var
  TimerData : PTimerData;
  {$IFDEF Logging}s : string;{$ENDIF}
begin
  TimerData := @AudioProcess2Data;

  CalculateCpuTimeAndLoad(TimerData);

  {$IFDEF Logging}
  inc(LogCount);
  if (LogCount > 100) or (AudioProcess2Data.ProcessLoad >= 100)  then
  begin
    s := IntToStr(round(AudioProcess2Data.ProcessLoad));
    LogMain.LogError('Audio Process 2 Load is ' + s + '%');
    LogCount := 0;
  end;
  {$ENDIF}
end;




end.
