unit VamLib.OneShotTimer;

// One Shot Timer is copied from
// http://stackoverflow.com/a/10469878/395461

interface


uses
  SysUtils,
  Windows, Generics.Collections;

type
  TOnTimerProc = TProc;

  ///  If ATimerID matches an existing time, the timer will be reset and replaced
  ///  with the new timer.
  ///
  ///  ATimeOut is in milliseconds
  function SetTimeout(AProc: TOnTimerProc; ATimeout: Cardinal):cardinal; overload;
  function SetTimeout(AProc: TOnTimerProc; ATimerID, ATimeout: Cardinal):cardinal; overload;
  procedure ClearTimeout(const TimerID : cardinal);

implementation

var
  TimerList: TDictionary<UINT_PTR, TOnTimerProc>;

procedure TimerProc(hwnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
var
  Proc: TOnTimerProc;
begin
  KillTimer(0, idEvent);

  if TimerList.TryGetValue(idEvent, Proc) then
  begin
    TimerList.Remove(idEvent);
    Proc();
  end;
end;

function SetTimeout(AProc: TOnTimerProc; ATimeout: Cardinal):cardinal;
var
  TimerID : Cardinal;
begin
  TimerID := SetTimer(0, 0, ATimeout, @TimerProc);
  if TimerID = 0 then raise Exception.Create('Unable to set timer.');
  TimerList.AddOrSetValue(TimerID, AProc);
  result := TimerID;
end;

function SetTimeout(AProc: TOnTimerProc; ATimerID, ATimeout: Cardinal):cardinal;
var
  TimerID : Cardinal;
begin
  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms644906%28v=vs.85%29.aspx
  TimerID := SetTimer(0, ATimerID, ATimeout, @TimerProc);
  if TimerID = 0 then raise Exception.Create('Unable to set timer.');
  TimerList.Add(TimerID, AProc);
  result := TimerID;
end;

procedure ClearTimeout(const TimerID : cardinal);
begin
  KillTimer(0, TimerID);
  TimerList.Remove(TimerID);
end;

initialization
  TimerList := TDictionary<UINT_PTR, TOnTimerProc>.Create;
finalization
  TimerList.Free;

end.
