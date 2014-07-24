unit VamLib.CpuOverloadWatcher;

interface

uses
  VamLib.LoggingProxy;

type
  TCpuOverloadWatcher = class
  private
    MaxTime   : Int64; // time in milliseconds,
    StartTime : Int64;
    ProcessTime : double; // time in milliseconds.
    ProcessLoad : double; // 0..100% of max load.
    fWatchName : string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(const SampleFrames, SampleRate: double; const WatchName : string); overload;
    procedure Start(const MaxProcessTime : Int64;  const WatchName : string); overload;

    procedure Stop;
  end;


procedure CpuOverloadWatch_Start(const WatchName : string; const MaxProcessTime : single = 1);
procedure CpuOverloadWatch_Stop(const WatchName : string);


implementation

uses
  SysUtils,
  Windows,
  Contnrs;

{ TCpuOverloadWatcher }

constructor TCpuOverloadWatcher.Create;
begin
  // TODO:MED It would be handy to check for the minimum process time slice here.
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


procedure TCpuOverloadWatcher.Start(const MaxProcessTime: Int64; const WatchName: string);
// MaxProcessTime is in Milliseconds.
begin
  fWatchName := WatchName;
  MaxTime := MaxProcessTime;
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




//==============================================================================================
//==============================================================================================




type
  TCpuWatchData = class
  private
  public
    WatchName : string;
    MaxTime   : single; // time in milliseconds,
    StartTime : Int64;
    EndTime   : Int64;
  end;

var
  GlobalWatchList : TObjectList;

function CpuOverloadWatch_FindOrCreateWatch(const WatchName : string):TCpuWatchData;
var
  c1: Integer;
  wd : TCpuWatchData;
begin
  if not assigned(GlobalWatchList) then
  begin
    GlobalWatchList := TObjectList.Create;
    GlobalWatchList.OwnsObjects := true;
  end;

  wd := nil;

  for c1 := 0 to GlobalWatchList.Count-1 do
  begin
    if (GlobalWatchList.Items[c1] as TCpuWatchData).WatchName = WatchName then
    begin
      wd := GlobalWatchList.Items[c1] as TCpuWatchData;
      exit(wd); //=========== take a short cut! ===============>>
    end;
  end;

  if not assigned(wd) then
  begin
    wd := TCpuWatchData.Create;
    wd.WatchName := WatchName;
    GlobalWatchList.Add(wd);
  end;

  result := wd;
end;

procedure CpuOverloadWatch_Start(const WatchName : string; const MaxProcessTime : single);
var
  wd : TCpuWatchData;
begin
  // TODO:HIGH add conditional defines so all this is optional.
  wd := CpuOverloadWatch_FindOrCreateWatch(WatchName);
  wd.MaxTime := MaxProcessTime;
  QueryPerformanceCounter(wd.StartTime);
end;

procedure CpuOverloadWatch_Stop(const WatchName : string);
var
  wd : TCpuWatchData;
  freq:Int64;
  ProcessTime:single;
  xLoad : string;
begin
  // TODO:HIGH add conditional defines so all this is optional.

  wd := CpuOverloadWatch_FindOrCreateWatch(WatchName);
  QueryPerformanceCounter(wd.EndTime);
  QueryPerformanceFrequency(freq);

  ProcessTime := (wd.EndTime - wd.StartTime) * 1000 / freq;
  if ProcessTime > wd.MaxTime then
  begin
    xLoad := IntToStr(round(ProcessTime / wd.MaxTime * 100)) + '%';
    VamLib.LoggingProxy.Log.LogError('CPU Overload (' + WatchName + ') Load = ' + xLoad);
  end;
end;


//==============================================================================================
//==============================================================================================

initialization
  GlobalWatchList := TObjectList.Create;
  GlobalWatchList.OwnsObjects := true;


finalization
  if assigned(GlobalWatchList)
    then FreeAndNil(GlobalWatchList);

end.
