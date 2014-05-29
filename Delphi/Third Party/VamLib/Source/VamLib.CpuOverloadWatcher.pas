unit VamLib.CpuOverloadWatcher;

interface

uses
  VamLib.LoggingProxy;

type
  TCpuOverloadWatcher = class
  private
    MaxTime   : cardinal; // time in milliseconds,
    StartTime : Int64;
    ProcessTime : double; // time in milliseconds.
    ProcessLoad : double; // 0..100% of max load.
    fWatchName : string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(const SampleFrames, SampleRate: double; const WatchName : string);
    procedure Stop;
  end;


implementation

uses
  SysUtils,
  Windows;

{ TCpuOverloadWatcher }

constructor TCpuOverloadWatcher.Create;
begin

end;

destructor TCpuOverloadWatcher.Destroy;
begin

  inherited;
end;

procedure TCpuOverloadWatcher.Start(const SampleFrames, SampleRate: double; const WatchName : string);
begin
  fWatchName := WatchName;
  MaxTime := round(SampleFrames / SampleRate * 1000);
  QueryPerformanceCounter(StartTime);
end;

procedure TCpuOverloadWatcher.Stop;
var
  pc:Int64;
  freq:Int64;
  xLoad : string;
begin
  QueryPerformanceCounter(pc);
  QueryPerformanceFrequency(freq);
  ProcessTime := (pc - StartTime) * 1000 / freq;
  ProcessLoad := ProcessTime / MaxTime * 100;


  //Check for overloads..
  if ProcessTime > MaxTime then
  begin
    xLoad := IntToStr(round(ProcessTime / MaxTime * 100)) + '%';
    VamLib.LoggingProxy.Log.LogError('CPU Overload (' + fWatchName + ') Load = ' + xLoad);
  end;


end;

end.
