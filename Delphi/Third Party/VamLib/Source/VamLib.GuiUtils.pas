unit VamLib.GuiUtils;

interface

uses
  SysUtils;

procedure Wait(const MilliSeconds : Longint);

// TODO:HIGH write a GuiDebouce and/or GuiThrottle methods based
// on code in wait().

//function GuiDebounce(const MilliSeconds : integer; var ReferenceTime : Int64):boolean;

{
  TODO:HIGH i think it should be possible to debounce and throttle functions using
  the wait method above, and using a technique similar to how Javascript debounce
  functions work, (in the sense they wrap the function to be debounced and return
  another function.

  The Debounce() method would return a TDebouncedMethod which would be an anonymous
  method. It means application developers will need to do some setup. But they
  have to do that anyway when using VamLib.Debouncer.

  VamLib.Debouncer is somewhat complicated and involves running a timer. This code
  might be less complicated and resource intensive.

  On the downside these debounce methods could only be used in GUI code because
  it requires the use of wait.. which uses  Application.ProcessMessages to avoid
  blocking the main thread.

}


type
  TDebounceToken = record
  private
    LastCallTime : TDateTime;
    CallRef : TProc;
    IsTrailingCallRequired : Boolean;
    IsActive : boolean;
  public
  end;

  TDebounceEdge = (deLeading, deTrailing, deBoth);

procedure Debounce(var DebounceToken : TDebounceToken; const Edge : TDebounceEdge; const MilliSeconds : integer; Proc : TProc);






implementation

uses
  DateUtils,
  Vcl.Forms,
  WinApi.Windows;


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

procedure Debounce(var DebounceToken : TDebounceToken; const Edge : TDebounceEdge; const MilliSeconds : integer; Proc : TProc);
var
  IsWaitFinished : boolean;
begin
  if (DebounceToken.IsActive = false) then
  begin
    if (Edge <> TDebounceEdge.deTrailing)
      then Proc();

    DebounceToken.IsTrailingCallRequired := true;
    DebounceToken.IsActive := true;
    DebounceToken.LastCallTime := Now;

    IsWaitFinished := false;

    repeat
      Wait(MilliSeconds);

      if not WithinPastMilliSeconds(Now, DebounceToken.LastCallTime, MilliSeconds) then
      begin
        if DebounceToken.IsTrailingCallRequired then
        begin
          if (Edge <> TDebounceEdge.deLeading)
            then Proc();

          DebounceToken.IsTrailingCallRequired := false;
          DebounceToken.LastCallTime := Now;
        end else
        begin
          IsWaitFinished := true;
        end;
      end;
    until
      IsWaitFinished;

    DebounceToken.IsActive := false;
  end else
  begin
    DebounceToken.LastCallTime := Now;
    DebounceToken.IsTrailingCallRequired := true;
  end;
end;



end.
