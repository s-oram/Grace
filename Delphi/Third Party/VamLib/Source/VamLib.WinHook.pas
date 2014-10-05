unit VamLib.WinHook;

interface

uses
  Windows;

type
  // TWindowsEventHook is a wrapper around SetWinEventHook(). Documentation is
  // here:
  //    MSDN SetWinEventHook function
  //    http://msdn.microsoft.com/en-us/library/windows/desktop/dd373640%28v=vs.85%29.aspx
  //    MSDN Event constants
  //    http://msdn.microsoft.com/en-us/library/windows/desktop/dd318066%28v=vs.85%29.aspx

  TWinEvent = procedure(Sender : TObject; Event, hwnd, idObject, idChild, EventThread, EventTime : cardinal) of object;

  TWindowsEventHook = class
  private
    fOnWinEvent: TWinEvent;

  protected
    CallbackMethodPointer : pointer;
    HookHandle : NativeUInt;
  public
    //  MSDN Event constants:
    //  - http://msdn.microsoft.com/en-us/library/windows/desktop/dd318066%28v=vs.85%29.aspx
    //  idProcess, idThread. Use 0 for all processes and all threads.
    constructor Create(const EventMin, EventMax : cardinal; const idProcess : cardinal = 0; const idThread : cardinal = 0);

    destructor Destroy; override;

    property OnWinEvent : TWinEvent read fOnWinEvent write fOnWinEvent;
  published
    // Published but only intended for internal usage.
    procedure WinHookCallback(hWinEventHook : NativeUInt; dwEvent:dword; handle : hwnd; idObject, idChild : Long; dwEventThread, dwmsEventTime : dword); stdcall;
  end;


implementation

uses
  VamLib.Utils;



{ TWindowsEventHook }

constructor TWindowsEventHook.Create(const EventMin, EventMax : cardinal; const idProcess : cardinal; const idThread : cardinal);
var
  MyAddress : pointer;
begin
  // EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_FOREGROUND

  MyAddress := self.MethodAddress('WinHookCallback');

  if MyAddress <> nil then
  begin
    CallbackMethodPointer := MethodToProcedure(self, MyAddress, 7);
    HookHandle := SetWinEventHook(EventMin, EventMax, 0, CallbackMethodPointer, idProcess, idThread, 0);
  end;

end;

destructor TWindowsEventHook.Destroy;
begin
  UnhookWinEvent(HookHandle);
  VirtualFree(CallbackMethodPointer, 0, MEM_RELEASE);

  inherited;
end;

procedure TWindowsEventHook.WinHookCallback(hWinEventHook: NativeUInt;
  dwEvent: dword; handle: hwnd; idObject, idChild: Long; dwEventThread,
  dwmsEventTime: dword);
begin
  // Documentation on callback function.
  // MSDN WinEventProc callback function
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd373885%28v=vs.85%29.aspx
  if assigned(fOnWinEvent)
    then fOnWinEvent(self, dwEvent, handle, idObject, idChild, dwEventThread, dwmsEventTime);
end;

end.
