{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksTimers;

{$I ksTools.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms;

type
  TksTimer = class(TComponent)
  private
    FDueTime: Cardinal;
    FPeriod: Cardinal;
    FTimerHandle: THandle;
    FWindowHandle: HWND;
    FThreadCount: Cardinal;
    FCount: Cardinal;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure WndProc(var Msg: TMessage);
    procedure SetDueTime(const Value: Cardinal);
    procedure SetPeriod(const Value: Cardinal);
  protected
    procedure Timer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Count: Cardinal read FCount;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property DueTime: Cardinal read FDueTime write SetDueTime default 1000;
    property Period: Cardinal read FPeriod write SetPeriod default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

type
  TWaitOrTimerCallback = procedure(lpParameter: Pointer; TimerOrWaitFired: Boolean); stdcall;

function CreateTimerQueueTimer(
  var Timer: THandle;
  TimerQueue: THandle;
  Callback: TWaitOrTimerCallback;
  Parameter: Pointer;
  DueTime: LongWord;
  Period: LongWord;
  Flags: LongWord
  ): BOOL; stdcall;

function DeleteTimerQueueTimer(
  TimerQueue: THandle;
  Timer: THandle;
  CompletionEvent: THandle
  ): BOOL; stdcall;

implementation

function CreateTimerQueueTimer; external kernel32 name 'CreateTimerQueueTimer';
function DeleteTimerQueueTimer; external kernel32 name 'DeleteTimerQueueTimer';

procedure TimerCallback(Timer: TksTimer; TimerOrWaitFired: Boolean); stdcall;
begin
  Inc(Timer.FThreadCount);
  PostMessage(Timer.FWindowHandle, WM_APP + 1, 0, Timer.FThreadCount);
end;

{ TksTimer }

constructor TksTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDueTime:= 1000;
  FPeriod:= 1000;
  FWindowHandle:= Classes.AllocateHWnd(WndProc);
end;

destructor TksTimer.Destroy;
begin
  SetEnabled(False);
  Classes.DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

procedure TksTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_APP + 1 then begin
      FCount:= lParam;
      try
        Timer;
      except
        Application.HandleException(Self);
      end
    end
    else
      Result:= DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TksTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    if Value then begin
      FThreadCount:= 0;
      FCount:= 0;
      FEnabled:= CreateTimerQueueTimer(FTimerHandle, 0, @TimerCallback, Self,
        FDueTime, FPeriod, 0);
    end
    else begin
      DeleteTimerQueueTimer(0, FTimerHandle, 0);
      FEnabled:= False;
    end;
  end;
end;

procedure TksTimer.SetDueTime(const Value: Cardinal);
begin
  if not FEnabled then
    FDueTime:= Value;
end;

procedure TksTimer.SetPeriod(const Value: Cardinal);
begin
  if not FEnabled then
    FPeriod:= Value;
end;

procedure TksTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

end.
