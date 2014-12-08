unit VamLib.GuiUtils;

interface

uses
  Controls,
  SysUtils;



// NOTE: Here is a good tutorial explaining the different timer options in Windows.
// http://www.codeproject.com/Articles/1236/Timers-Tutorial#WaitableTimers
// This tutorial will be useful if I have to re-implement the debounce and throttle
// methods again if something doesn't work properly.

procedure Wait(const MilliSeconds : Longint);
function FindFocusedControl(aControl : TWinControl):TWinControl;

implementation

uses
  DateUtils,
  Vcl.Forms,
  WinApi.Windows,
  VamLib.Threads;


procedure Wait(const MilliSeconds : Longint);
// Wait might be a useful alternative to sleep. It can be used in GUI code
// but will not block the GUI or prevent messages from being processed.
// (It does this by calling Application.ProcessMessages(). I don't
// know if that is good behaviour however.
// - Processing will resume in the same thread after Wait() returns.
// This probably isn't useful in non-GUI code.
const
  _SECOND = 10000000;
  unitsPerMilliSecond = 10*1000;
var
 Busy : LongInt;
 TimerHandle : LongInt;
 DueTime : LARGE_INTEGER;
begin
  // Waitable Timers in Delphi.
  // http://delphi32.blogspot.com.au/2006/03/using-waitable-timer-in-delphi.html
  // http://www.adp-gmbh.ch/win/misc/timer.html

  TimerHandle := CreateWaitableTimer(nil, True, nil);
  if TimerHandle = 0 then Exit;
  DueTime.QuadPart := -(unitsPerMilliSecond * MilliSeconds);
  SetWaitableTimer(TimerHandle, TLargeInteger(DueTime), 0, nil, nil, False);

  repeat
    Busy := MsgWaitForMultipleObjects(1, TimerHandle, False, INFINITE, QS_ALLINPUT);
    Application.ProcessMessages;
  until Busy = WAIT_OBJECT_0;

  // TODO:LOW I wonder if there is a way to interrupt the timer to force it to return early.

  // Close the handles when you are done with them.
  CloseHandle(TimerHandle);
end;

function FindFocusedControl(aControl : TWinControl):TWinControl;
var
  c1 : integer;
  c : TControl;
  wc : TWinControl;
begin
  if aControl.Focused then
  begin
    exit(aControl); //============== exit with focused control =====>>
  end else
  begin
    for c1 := 0 to aControl.ControlCount-1 do
    begin
      c := aControl.Controls[c1];
      if (c is TWinControl) then
      begin
        wc := FindFocusedControl(c as TWinControl);
        if assigned(wc)
          then exit(wc); //============== exit with focused control =====>>
      end;
    end;
  end;

  // if we make it this far, no control has focus.
  result := nil;

end;


end.
