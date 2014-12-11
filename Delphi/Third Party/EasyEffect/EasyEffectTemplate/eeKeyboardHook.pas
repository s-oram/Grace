unit eeKeyboardHook;

interface

{
  This class creates a global key hook. Only one hook is created and is shared between application
  instances.

  By Shannon Oram

  Updated: 12th December 2009

}

{$INCLUDE Defines.inc}

uses
  {$IFDEF VER230}
    Vcl.Forms,
  {$ELSE}
    Forms,
  {$ENDIF}
  Windows;

type
  TProcessKey = function (VirtualKeyCode:Word):boolean of object;

  TKeyboardHook = class
  public
    constructor Create(aHandle:HWND; aKeyDown, aKeyUp:TProcessKey);
    destructor Destroy; override;

    procedure UpdateValues(aHandle:HWND; aKeyDown, aKeyUp:TProcessKey);
  end;

implementation

uses
  {$IFDEF Logging}VamLib.Logging,{$ENDIF}
  {$IFDEF VER230}
    Vcl.Controls,
  {$ELSE}
    Controls,
  {$ENDIF}
  uGetWindowsInfo, uGeneralFunctions;


var
  HookCount          : integer;
  KeyboardHookHandle : HHook;
  GlobalEditorWindow : hwnd;
  GlobalKeyDown  : TProcessKey;
  GlobalKeyUp    : TProcessKey;


function ShouldMessageBeProcessed:boolean;
var
  h:hwnd;
begin
  if (GlobalEditorWindow = 0) then
  begin
    result := false;
    exit; //==========================>> exit >>=========>>
  end;


  // NOTE: Previously the keyboard hook routine would check to see if the current foreground
  // window was the hook target window or a child of it. Allowing "child" windows to be considered
  // the same as the hook target was causing problems in Reaper (and possibly other hosts).
  // (Problems usually arose when trying to save presets.
  //
  // To avoid this problem I've changed the code so that only the current foreground window is
  // checked to see if it is the target. Child windows are assumed removed from Poise and not
  // valid targets for the hotkey hook. (I'm a little hesitent about this change because there are
  // so many VST hosts and they might not all work similarly.

  // ==== OLD CODE Check if window is child of target ====
  {
  h := GetForegroundWindow;
  while (h <> 0) and (h <> GlobalEditorWindow) do h := GetParent(h);
  if h = GlobalEditorWindow
    then result := true
    else result := false;
  }


  // ==== NEW CODE: *Only* check if target is in focus ====
  h := GetForegroundWindow;
  if (h = 0) then exit(false); //No focused foreground window.
  if (h <> GlobalEditorWindow) then exit(false); //Foreground window is not our plugin window.

  // if we make it this far, return true.
  result := true;
end;




function KeyboardHookProc(Code: Integer; WordParam: Word; LongParam: LongInt): LongInt; stdcall;
var
  IsKeyUp:boolean;
  MessageProcessed:boolean;
begin
  MessageProcessed := false;

  if (Code < 0) or (Code = HC_NOREMOVE) then
  begin
    result := CallNextHookEx(KeyboardHookHandle,Code,WordParam,LongParam);
    exit; //===============================================================>
  end;

  if Code = HC_ACTION then
  try
    if not assigned(GlobalKeyUp) then
    begin
      result := CallNextHookEx(KeyboardHookHandle,Code,WordParam,LongParam);
      exit; //===============================================================>
    end;

    if not assigned(GlobalKeyDown) then
    begin
      result := CallNextHookEx(KeyboardHookHandle,Code,WordParam,LongParam);
      exit; //===============================================================>
    end;

    if ShouldMessageBeProcessed = false then
    begin
      result := CallNextHookEx(KeyboardHookHandle,Code,WordParam,LongParam);
      exit; //===============================================================>
    end;

    IsKeyUp := ((LongParam and (1 shl 31)) <> 0);

    if IsKeyUp
      then MessageProcessed := GlobalKeyUp(WordParam)
      else MessageProcessed := GlobalKeyDown(WordParam);

  finally
    //Don't need to do anything here.
  end;

  if MessageProcessed
    then result := -1
    else result := CallNextHookEx(KeyboardHookHandle,Code,WordParam,LongParam);

end;



{ TKeyboardHook }

constructor TKeyboardHook.Create(aHandle:HWND; aKeyDown, aKeyUp: TProcessKey);
begin
  GlobalKeyDown      := aKeyDown;
  GlobalKeyUp        := aKeyUp;
  GlobalEditorWindow := aHandle;

  if HookCount = 0 then
  begin
    KeyboardHookHandle := SetWindowsHookEx(WH_KEYBOARD, @KeyboardHookProc, 0, GetCurrentThreadId());
  end;
  inc(HookCount);
end;


destructor TKeyboardHook.Destroy;
begin
  dec(HookCount);
  if HookCount = 0 then
  begin
    UnHookWindowsHookEx(KeyboardHookHandle);
  end;

  GlobalEditorWindow := 0;
  GlobalKeyUp        := nil;
  GlobalKeyDown      := nil;


  inherited;
end;


procedure TKeyboardHook.UpdateValues(aHandle: HWND; aKeyDown, aKeyUp: TProcessKey);
begin
  GlobalKeyDown      := aKeyDown;
  GlobalKeyUp        := aKeyUp;
  GlobalEditorWindow := aHandle;
end;


initialization
  HookCount := 0;


end.
