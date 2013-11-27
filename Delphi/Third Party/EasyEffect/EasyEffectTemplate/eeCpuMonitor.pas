unit eeCpuMonitor;

interface

{$INCLUDE Defines.inc}

type
  TCpuMonitor = class
  private
    fProcessReplacingTime: double;
    fProcessReplacingLoad: double;
  protected
    fSampleFrames, fSampleRate : double;
    prStartTime : Int64; //Process Replacing Start Time
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartProcessReplacingTimer(aSampleFrames, aSampleRate : double);
    function StopProcessingReplacingTimer:boolean;

    property ProcessReplacingTime : double read fProcessReplacingTime; // milliseconds
    property ProcessReplacingLoad : double read fProcessReplacingLoad; // 0..100 (percentage of max load)
  end;

implementation

uses
  SysUtils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  Windows;

{ TCpuMonitor }

constructor TCpuMonitor.Create;
begin
  fProcessReplacingTime := 0;
end;

destructor TCpuMonitor.Destroy;
begin

  inherited;
end;

procedure TCpuMonitor.StartProcessReplacingTimer(aSampleFrames, aSampleRate : double);
var
  freq:Int64;
begin
  fSampleFrames := aSampleFrames;
  fSampleRate   := aSampleRate;
  QueryPerformanceCounter(prStartTime);
  QueryPerformanceFrequency(freq);
end;

function TCpuMonitor.StopProcessingReplacingTimer:boolean;
var
  pc:Int64;
  freq:Int64;
  s:string;
begin
  QueryPerformanceCounter(pc);
  QueryPerformanceFrequency(freq);
  fProcessReplacingTime := (pc - prStartTime) * 1000 / freq;

  // Approximately calculate CPU load as a ratio of maximum execution time before audio is slower then real-time.
  fProcessReplacingLoad := fProcessReplacingTime / (fSampleFrames / fSampleRate * 1000);
  // Convert to percentage.
  fProcessReplacingLoad := fProcessReplacingLoad * 100;

  if fProcessReplacingLoad >= 50
    then result := true
    else result := false;


  {$IFDEF Logging}
  if fProcessReplacingLoad >= 50 then
  begin
    s := IntToStr(round(fProcessReplacingLoad));
    LogMain.LogError('Processing Replacing Load is ' + s + '%');
    LogMain.LogMessage('- Sampleframes = ' + IntToStr(round(fSampleFrames)));
  end;
  {$ENDIF}
end;

end.
